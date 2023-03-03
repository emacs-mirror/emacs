;;; sieve-mode.el --- Sieve code editing commands for Emacs  -*- lexical-binding: t -*-

;; Copyright (C) 2001-2023 Free Software Foundation, Inc.

;; Author: Simon Josefsson <simon@josefsson.org>

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file contain editing mode functions and font-lock support for
;; editing Sieve scripts.  It sets up C-mode with support for
;; sieve-style #-comments and a lightly hacked syntax table.  It was
;; strongly influenced by awk-mode.el.
;;
;; References:
;;
;; RFC 3028,
;; "Sieve: A Mail Filtering Language",
;; by Tim Showalter.
;;
;; Release history:
;;
;; 2001-03-02 version 1.0 posted to gnu.emacs.sources
;;            version 1.1 change file extension into ".siv" (official one)
;;                        added keymap and menubar to hook into sieve-manage
;; 2001-10-31 version 1.2 committed to Oort Gnus

;;; Code:

(autoload 'sieve-manage "sieve")
(autoload 'sieve-upload "sieve")

(defgroup sieve nil
  "Sieve."
  :group 'languages)

(defcustom sieve-mode-hook nil
  "Hook run in sieve mode buffers."
  :type 'hook)

;; Font-lock

(defface sieve-control-commands
  '((((type tty) (class color)) (:foreground "blue" :weight light))
    (((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
    (((class color) (background light)) (:foreground "Orchid"))
    (((class color) (background dark)) (:foreground "LightSteelBlue"))
    (t (:bold t)))
  "Face used for Sieve Control Commands.")

(defface sieve-action-commands
  '((((type tty) (class color)) (:foreground "blue" :weight bold))
    (((class color) (background light)) (:foreground "Blue"))
    (((class color) (background dark)) (:foreground "LightSkyBlue"))
    (t (:inverse-video t :bold t)))
  "Face used for Sieve Action Commands.")

(defface sieve-test-commands
  '((((type tty) (class color)) (:foreground "magenta"))
    (((class grayscale) (background light))
     (:foreground "LightGray" :bold t :underline t))
    (((class grayscale) (background dark))
     (:foreground "Gray50" :bold t :underline t))
    (((class color) (background light)) (:foreground "CadetBlue"))
    (((class color) (background dark)) (:foreground "Aquamarine"))
    (t (:bold t :underline t)))
  "Face used for Sieve Test Commands.")

(defface sieve-tagged-arguments
  '((((type tty) (class color)) (:foreground "cyan" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
    (((class color) (background light)) (:foreground "Purple"))
    (((class color) (background dark)) (:foreground "Cyan"))
    (t (:bold t)))
  "Face used for Sieve Tagged Arguments.")


(defconst sieve-font-lock-keywords
  (eval-when-compile
    `(
      ;; control commands
      (,(regexp-opt '("require" "if" "else" "elsif" "stop") 'words)
       . 'sieve-control-commands)
      ;; action commands
      (,(regexp-opt '("fileinto" "redirect" "reject" "keep" "discard") 'words)
       . 'sieve-action-commands)
      ;; test commands
      (,(regexp-opt '("address" "allof" "anyof" "exists" "false"
		      "true" "header" "not" "size" "envelope"
                      "body")
                    'words)
       . 'sieve-test-commands)
      ("\\Sw+:\\sw+" . 'sieve-tagged-arguments))))

;; Syntax table

(defvar sieve-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\\ "\\" st)
    (modify-syntax-entry ?\n ">   " st)
    (modify-syntax-entry ?\f ">   " st)
    (modify-syntax-entry ?\# "<   " st)
    (modify-syntax-entry ?/ ". 14" st)
    (modify-syntax-entry ?* ". 23b" st)
    (modify-syntax-entry ?+ "." st)
    (modify-syntax-entry ?- "." st)
    (modify-syntax-entry ?= "." st)
    (modify-syntax-entry ?% "." st)
    (modify-syntax-entry ?< "." st)
    (modify-syntax-entry ?> "." st)
    (modify-syntax-entry ?& "." st)
    (modify-syntax-entry ?| "." st)
    (modify-syntax-entry ?_ "_" st)
    (modify-syntax-entry ?\' "\"" st)
    (modify-syntax-entry ?\{ "(}" st)
    (modify-syntax-entry ?\} "){" st)
    (modify-syntax-entry ?\" "\"" st)
    st)
  "Syntax table in use in `sieve-mode' buffers.")


;; Key map definition

(defvar-keymap sieve-mode-map
  :doc "Keymap used in sieve mode."
  "C-c C-l" #'sieve-upload
  "C-c C-c" #'sieve-upload-and-kill
  "C-c RET" #'sieve-manage)

;; Menu

(easy-menu-define sieve-mode-menu sieve-mode-map
  "Sieve Menu."
  '("Sieve"
    ["Upload script" sieve-upload t]
    ["Manage scripts on server" sieve-manage t]))

;; Code for Sieve editing mode.


(defun sieve-syntax-propertize (beg end)
  (goto-char beg)
  (sieve-syntax-propertize-text end)
  (funcall
   (syntax-propertize-rules
    ;; FIXME: When there's a "text:" with a # comment, the \n plays dual role:
    ;; it closes the comment and starts the string.  This is problematic for us
    ;; since syntax-table entries can either close a comment or
    ;; delimit a string, but not both.
    ("\\_<text:[ \t]*\\(?:#.*\\(.\\)\\)?\\(\n\\)"
     (1 ">")
     (2 (prog1 (unless (save-excursion
                         (nth 8 (syntax-ppss (match-beginning 0))))
                 (string-to-syntax "|"))
          (sieve-syntax-propertize-text end)))))
   beg end))

(defun sieve-syntax-propertize-text (end)
  (let ((ppss (syntax-ppss)))
    (when (and (eq t (nth 3 ppss))
               (re-search-forward "^\\.\\(\n\\)" end 'move))
      (put-text-property (match-beginning 1) (match-end 1)
                         'syntax-table (string-to-syntax "|")))))

;;;###autoload
(define-derived-mode sieve-mode prog-mode "Sieve"
  "Major mode for editing Sieve code.
Turning on Sieve mode runs `sieve-mode-hook'."
  (setq-local paragraph-start (concat "$\\|" page-delimiter))
  (setq-local paragraph-separate paragraph-start)
  (setq-local comment-start "#")
  (setq-local comment-end "")
  ;; (setq-local comment-start-skip "\\(^\\|\\s-\\);?#+ *")
  (setq-local comment-start-skip "#+ *")
  (setq-local syntax-propertize-function #'sieve-syntax-propertize)
  (setq-local font-lock-defaults
              '(sieve-font-lock-keywords nil nil ((?_ . "w"))))
  (setq-local indent-line-function #'sieve-mode-indent-function)
  (easy-menu-add-item nil nil sieve-mode-menu))

(defun sieve-mode-indent-function ()
  (save-excursion
    (beginning-of-line)
    (let ((depth (car (syntax-ppss))))
      (when (looking-at "[ \t]*}")
        (setq depth (1- depth)))
      (indent-line-to (* 2 depth))))
  ;; Skip to the end of the indentation if at the beginning of the
  ;; line.
  (when (save-excursion
          (skip-chars-backward " \t")
          (bolp))
    (skip-chars-forward " \t")))

(provide 'sieve-mode)

;;; sieve-mode.el ends here
