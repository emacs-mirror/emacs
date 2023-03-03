;;; em-prompt.el --- command prompts  -*- lexical-binding:t -*-

;; Copyright (C) 1999-2023 Free Software Foundation, Inc.

;; Author: John Wiegley <johnw@gnu.org>

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

;; Most of the prompt navigation commands of `comint-mode' are
;; supported, such as C-c C-n, C-c C-p, etc.

;;; Code:

(require 'esh-mode)
(require 'text-property-search)

;;;###autoload
(progn
(defgroup eshell-prompt nil
  "This module provides command prompts, and navigation between them,
as is common with most shells."
  :tag "Command prompts"
  :group 'eshell-module))

;;; User Variables:

(defcustom eshell-prompt-load-hook nil
  "A list of functions to call when loading `eshell-prompt'."
  :version "24.1"			; removed eshell-prompt-initialize
  :type 'hook
  :group 'eshell-prompt)

(autoload 'eshell/pwd "em-dirs")

(defcustom eshell-prompt-function
  (lambda ()
    (concat (abbreviate-file-name (eshell/pwd))
            (if (= (file-user-uid) 0) " # " " $ ")))
  "A function that returns the Eshell prompt string.
Make sure to update `eshell-prompt-regexp' so that it will match your
prompt."
  :type 'function
  :group 'eshell-prompt)

(defcustom eshell-prompt-regexp "^[^#$\n]* [#$] "
  "A regexp which fully matches your Eshell prompt.
This is useful for navigating by paragraph using \
\\[forward-paragraph] and \\[backward-paragraph].

If this variable is changed, all Eshell buffers must be exited
and re-entered for it to take effect."
  :type 'regexp
  :group 'eshell-prompt)

(defcustom eshell-highlight-prompt t
  "If non-nil, Eshell should highlight the prompt."
  :type 'boolean
  :group 'eshell-prompt)

(defface eshell-prompt
  '((default :weight bold)
    (((class color) (background light)) :foreground "Red")
    (((class color) (background dark))  :foreground "Pink"))
  "The face used to highlight prompt strings.
For highlighting other kinds of strings -- similar to shell mode's
behavior -- simply use an output filer which changes text properties."
  :group 'eshell-prompt)

(defcustom eshell-before-prompt-hook nil
  "A list of functions to call before outputting the prompt."
  :type 'hook
  :options '(eshell-begin-on-new-line)
  :group 'eshell-prompt)

(defcustom eshell-after-prompt-hook nil
  "A list of functions to call after outputting the prompt.
Note that if `eshell-scroll-show-maximum-output' is non-nil, then
setting `eshell-show-maximum-output' here won't do much.  It depends
on whether the user wants the resizing to happen while output is
arriving, or after."
  :type 'hook
  :options '(eshell-show-maximum-output)
  :group 'eshell-prompt)

(defvar-keymap eshell-prompt-mode-map
  "C-c C-n" #'eshell-next-prompt
  "C-c C-p" #'eshell-previous-prompt)

(defvar-keymap eshell-prompt-repeat-map
  :doc "Keymap to repeat eshell-prompt key sequences.  Used in `repeat-mode'."
  :repeat t
  "C-n" #'eshell-next-prompt
  "C-p" #'eshell-previous-prompt)

;;; Functions:

(define-minor-mode eshell-prompt-mode
  "Minor mode for eshell-prompt module.

\\{eshell-prompt-mode-map}"
  :keymap eshell-prompt-mode-map)

(defun eshell-prompt-initialize ()  ;Called from `eshell-mode' via intern-soft!
  "Initialize the prompting code."
  (unless eshell-non-interactive-p
    (add-hook 'eshell-post-command-hook 'eshell-emit-prompt nil t)

    (make-local-variable 'eshell-prompt-regexp)
    (if eshell-prompt-regexp
        (setq-local paragraph-start eshell-prompt-regexp))

    (eshell-prompt-mode)))

(defun eshell-emit-prompt ()
  "Emit a prompt if eshell is being used interactively."
  (when (boundp 'ansi-color-context-region)
    (setq ansi-color-context-region nil))
  (run-hooks 'eshell-before-prompt-hook)
  (if (not eshell-prompt-function)
      (set-marker eshell-last-output-end (point))
    (let ((prompt (funcall eshell-prompt-function)))
      (add-text-properties
       0 (length prompt)
       (if eshell-highlight-prompt
           '( read-only t
              field prompt
              font-lock-face eshell-prompt
              front-sticky (read-only field font-lock-face)
              rear-nonsticky (read-only field font-lock-face))
         '( field prompt
            front-sticky (field)
            rear-nonsticky (field)))
       prompt)
      (eshell-interactive-filter nil prompt)))
  (run-hooks 'eshell-after-prompt-hook))

(defun eshell-forward-matching-input (regexp arg)
  "Search forward through buffer for command input that matches REGEXP.
With prefix argument N, search for Nth next match.  If N is
negative, find the Nth previous match."
  (interactive (eshell-regexp-arg "Forward input matching (regexp): "))
  (let ((direction (if (> arg 0) 1 -1))
        (count (abs arg)))
    (unless (catch 'found
              (while (> count 0)
                (eshell-next-prompt direction)
                (when (and (string-match regexp (field-string))
                           (= (setq count (1- count)) 0))
                  (throw 'found t))))
      (message "Not found")
      (ding))))

(defun eshell-backward-matching-input (regexp arg)
  "Search backward through buffer for command input that matches REGEXP.
With prefix argument N, search for Nth previous match.  If N is
negative, find the Nth next match."
  (interactive (eshell-regexp-arg "Backward input matching (regexp): "))
  (eshell-forward-matching-input regexp (- arg)))

(defun eshell-next-prompt (n)
  "Move to end of Nth next prompt in the buffer."
  (interactive "p")
  (if (natnump n)
      (while (and (> n 0)
                  (text-property-search-forward 'field 'prompt t))
        (setq n (1- n)))
    (let (match this-match)
      (forward-line 0)           ; Don't count prompt on current line.
      (while (and (< n 0)
                  (setq this-match (text-property-search-backward
                                    'field 'prompt t)))
        (setq match this-match
              n (1+ n)))
      (when match
        (goto-char (prop-match-end match))))))

(defun eshell-previous-prompt (n)
  "Move to end of Nth previous prompt in the buffer."
  (interactive "p")
  (eshell-next-prompt (- n)))

(defun eshell-skip-prompt ()
  "Skip past the text matching regexp `eshell-prompt-regexp'.
If this takes us past the end of the current line, don't skip at all."
  (declare (obsolete nil "30.1"))
  (let ((eol (line-end-position)))
    (if (and (looking-at eshell-prompt-regexp)
	     (<= (match-end 0) eol))
	(goto-char (match-end 0)))))

(defun eshell-bol-ignoring-prompt (arg)
  "Move point to the beginning of the current line, past the prompt (if any).
With argument ARG not nil or 1, move forward ARG - 1 lines
first (see `move-beginning-of-line' for more information)."
  (interactive "^p")
  (let ((inhibit-field-text-motion t))
    (move-beginning-of-line arg)))

(provide 'em-prompt)

;; Local Variables:
;; generated-autoload-file: "esh-groups.el"
;; End:

;;; em-prompt.el ends here
