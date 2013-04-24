;;; auto-complete-clang-objc.el --- Auto Completion source for clang for GNU Emacs

;; Copyright (C) 2010 Brian Jiang
;; Copyright (C) 2013 Rafal Kowalski

;; Author: Brian Jiang <brianjcj@gmail.com>
;; Author: Rafal Kowalski <rafal.kowalski@mac.com>
;; Keywords: completion, convenience
;; Version: 0.2

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:
;;
;; Auto Completion source for clang. Most of codes are taken from
;; company-clang.el and modified and enhanced for Auto Completion.

;;; Code:

(require 'auto-complete)
(require 'popup)

(defcustom ac-clang-executable
  (executable-find "clang")
  "*Location of clang executable"
  :group 'auto-complete
  :type 'file)

(defcustom ac-clang-auto-save nil
  "*Determines whether to save the buffer when retrieving completions.
Old version of clang can only complete correctly when the buffer has been saved.
Now clang can parse the codes from standard input so that we can turn this option
to Off. If you are still using the old clang, turn it on!"
  :group 'auto-complete
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)))

(defcustom ac-clang-lang-option-function nil
  "*function to return the lang type for option -x."
  :group 'auto-complete
  :type 'function)

;;; Extra compilation flags to pass to clang.
(defcustom ac-clang-flags nil
  "Extra flags to pass to the Clang executable.
This variable will typically contain include paths, e.g., ( \"-I~/MyProject\", \"-I.\" )."
  :group 'auto-complete
  :type '(repeat (string :tag "Argument" "")))

;;; The prefix header to use with Clang code completion.
(defvar ac-clang-prefix-header nil)

;;; Set the Clang prefix header
(defun ac-clang-set-prefix-header (ph)
  (interactive
   (let ((def (car (directory-files "." t "\\([^.]h\\|[^h]\\).pch\\'" t))))
     (list
      (read-file-name (concat "Clang prefix header(current: " ac-clang-prefix-header ") : ")
                      (when def (file-name-directory def))
                      def nil (when def (file-name-nondirectory def))))))
  (cond ((string-match "^[\s\t]*$" ph)
         (setq ac-clang-prefix-header nil))
        (t
         (setq ac-clang-prefix-header ph))))

;;; Set a new cflags for clang
(defun ac-clang-set-cflags ()
  "set new cflags for clang from input string"
  (interactive)
  (setq ac-clang-flags (split-string (read-string "New cflags: "))))

;;; Set new cflags from shell command output
(defun ac-clang-set-cflags-from-shell-command ()
  "set new cflags for ac-clang from shell command output"
  (interactive)
  (setq ac-clang-flags
        (split-string
         (shell-command-to-string
          (read-shell-command "Shell command: " nil nil
                              (and buffer-file-name
                                   (file-relative-name buffer-file-name)))))))

(defconst ac-clang-completion-pattern
  "^COMPLETION: *\\(Pattern :\\)* *\\(([^)]*)\\)*\\(%s[^[:space:]]*\\)\\(?: : \\)*\\(.*$\\)")

(defconst ac-clang-error-buffer-name "*clang error*")

(defun ac-clang-parse-output (prefix)
  (goto-char (point-min))
  (let ((pattern (format ac-clang-completion-pattern
                         (regexp-quote prefix)))
        lines case match detailed_info
        (prev-match ""))
    (while (re-search-forward pattern nil t)
      (setq case (match-string-no-properties 1))
      (setq match (match-string-no-properties 3))
      (setq detailed_info
            (if (string= "Pattern :" case)
                (concat (match-string-no-properties 2)
                        (match-string-no-properties 3)
                        (match-string-no-properties 4)
                        (match-string-no-properties 5))
              (concat
               (match-string-no-properties 4)
               (match-string-no-properties 5))))
      (if (string= match prev-match)
          (when detailed_info
            (setq match (propertize match
                                    'ac-clang-help
                                    (concat
                                     (get-text-property 0 'ac-clang-help (car lines))
                                     "\n"
                                     detailed_info)))
            (setq match detailed_info)
            (setf (car lines) match))
        (setq prev-match match)
        (when detailed_info
          (setq match (propertize match 'ac-clang-help detailed_info)))
        (push match lines)))
    lines))


(defun ac-clang-handle-error (res args)
  (goto-char (point-min))
  (let* ((buf (get-buffer-create ac-clang-error-buffer-name))
         (cmd (concat ac-clang-executable " " (mapconcat 'identity args " ")))
         (pattern (format ac-clang-completion-pattern ""))
         (err (if (re-search-forward pattern nil t)
                  (buffer-substring-no-properties (point-min)
                                                  (1- (match-beginning 0)))
                ;; Warn the user more agressively if no match was found.
                (message "clang failed with error %d:\n%s" res cmd)
                (buffer-string))))

    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (current-time-string)
                (format "\nclang failed with error %d:\n" res)
                cmd "\n\n")
        (insert err)
        (setq buffer-read-only t)
        (goto-char (point-min))))))

(defun ac-clang-call-process (prefix &rest args)
  (let ((buf (get-buffer-create "*clang-output*"))
        res)
    (with-current-buffer buf (erase-buffer))
    (setq res (if ac-clang-auto-save
                  (apply 'call-process ac-clang-executable nil buf nil args)
                (apply 'call-process-region (point-min) (point-max)
                       ac-clang-executable nil buf nil args)))
    (with-current-buffer buf
      (unless (eq 0 res)
        (ac-clang-handle-error res args))
      ;; Still try to get any useful input.
      (ac-clang-parse-output prefix))))


(defsubst ac-clang-build-location (pos)
  (save-excursion
    (goto-char pos)
    (format "%s:%d:%d" (if ac-clang-auto-save buffer-file-name "-") (line-number-at-pos)
            (1+ (- (point) (line-beginning-position))))))

(defsubst ac-clang-lang-option ()
  (or (and ac-clang-lang-option-function
           (funcall ac-clang-lang-option-function))
      (cond ((eq major-mode 'c++-mode)
             "c++")
            ((eq major-mode 'c-mode)
             "c")
            ((eq major-mode 'objc-mode)
             (cond ((string= "m" (file-name-extension (buffer-file-name)))
                    "objective-c")
                   (t
                    "objective-c")))
            (t
             "objective-c"))))

(defsubst ac-clang-build-complete-args (pos)
  (append '("-cc1" "-fsyntax-only")
          (unless ac-clang-auto-save
            (list "-x" (ac-clang-lang-option)))
          ac-clang-flags
          (when (stringp ac-clang-prefix-header)
            (list "-include-pch" (expand-file-name ac-clang-prefix-header)))
          '("-code-completion-at")
          (list (ac-clang-build-location pos))
          (list (if ac-clang-auto-save buffer-file-name "-"))))


(defsubst ac-clang-clean-document (s)
  (when s
    (setq s (replace-regexp-in-string "<#\\|#>" "" s))
    (setq s (replace-regexp-in-string "\\[#\\(.*\\)#\\]" "(\\1)" s)))
  s)

(defun ac-clang-document (item)
  (if (stringp item)
      (let (s)
        (setq s (get-text-property 0 'ac-clang-help item))
        (ac-clang-clean-document s)))
  ;; (popup-item-property item 'ac-clang-help)
  )


(defface ac-clang-candidate-face
  '((t (:background "lightgray" :foreground "navy")))
  "Face for clang candidate"
  :group 'auto-complete)

(defface ac-clang-selection-face
  '((t (:background "navy" :foreground "white")))
  "Face for the clang selected candidate."
  :group 'auto-complete)

;; (defsubst ac-in-string/comment ()
(defun ac-in-string/comment ()
  "Return non-nil if point is in a literal (a comment or string)."
  (nth 8 (syntax-ppss)))

(defun ac-clang-candidate ()
  (unless (ac-in-string/comment)
    (and ac-clang-auto-save
         (buffer-modified-p)
         (basic-save-buffer))
    (save-restriction
      (widen)
      (apply 'ac-clang-call-process
             ac-prefix
             (ac-clang-build-complete-args (- (point) (length ac-prefix)))))))


(defvar ac-template-start-point nil)

(defvar ac-template-candidates (list "ok" "no" "yes:)"))

(defvar begining-of-candidate nil)

(defun ac-clang-action ()
  (interactive)
  (let ((raw-help (get-text-property 0 'ac-clang-help (cdr ac-last-completion)))
        (help (ac-clang-clean-document
               (get-text-property 0 'ac-clang-help (cdr ac-last-completion))))
        ss)
    (setq ss (split-string raw-help "\n"))
    (dolist (s ss)
;;; Example of an clang completion line might be:
;;; [#id#]stringWithContentsOfFile:<#(NSString *)#> encoding:<#(NSStringEncoding)#> error:<#(NSError **)#>
      (setq s (replace-regexp-in-string "\\[#.*?#\\]" "" s))
      (unless (string= s "")
        (setq s (replace-regexp-in-string "<#\\([^#>]*\\)#>" "${\\1}" s))
        (cond ((featurep 'yasnippet)
               (yas/expand-snippet s begining-of-candidate (point))))))))

(defun ac-clang-prefix ()
  (let ((pos
         (or (ac-prefix-symbol)
             (let ((c (char-before)))
               (when (or (eq ?\. c)
                         ;; ->
                         (and (eq ?> c)
                              (eq ?- (char-before (1- (point)))))
                         ;; ::
                         (and (eq ?: c)
                              (eq ?: (char-before (1- (point))))))
                 (point))))))
    (setq begining-of-candidate pos)
    pos))

(ac-define-source clang
  '((candidates . ac-clang-candidate)
    (candidate-face . ac-clang-candidate-face)
    (selection-face . ac-clang-selection-face)
    (prefix . ac-clang-prefix)
    (requires . 0)
    (document . ac-clang-document)
    (action . ac-clang-action)
    (cache)
    (symbol . "c")))

(provide 'auto-complete-clang-objc)
