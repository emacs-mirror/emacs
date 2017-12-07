;;; subword.el --- Handling capitalized subwords in a nomenclature -*- lexical-binding: t -*-

;; Copyright (C) 2004-2017 Free Software Foundation, Inc.

;; Author: Masatake YAMATO

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

;; This package provides the `subword' minor mode, which merges the
;; old remap-based subword.el (derived from cc-mode code) and
;; cap-words.el, which takes advantage of core Emacs
;; word-motion-customization functionality.

;; In spite of GNU Coding Standards, it is popular to name a symbol by
;; mixing uppercase and lowercase letters, e.g. "GtkWidget",
;; "EmacsFrameClass", "NSGraphicsContext", etc.  Here we call these
;; mixed case symbols `nomenclatures'.  Also, each capitalized (or
;; completely uppercase) part of a nomenclature is called a `subword'.
;; Here are some examples:

;;  Nomenclature           Subwords
;;  ===========================================================
;;  GtkWindow          =>  "Gtk" and "Window"
;;  EmacsFrameClass    =>  "Emacs", "Frame" and "Class"
;;  NSGraphicsContext  =>  "NS", "Graphics" and "Context"

;; The subword oriented commands defined in this package recognize
;; subwords in a nomenclature to move between them and to edit them as
;; words.  You also get a mode to treat symbols as words instead,
;; called `superword-mode' (the opposite of `subword-mode').

;; To make the mode turn on automatically, put the following code in
;; your .emacs:
;;
;; (add-hook 'c-mode-common-hook 'subword-mode)
;;

;; To make the mode turn `superword-mode' on automatically for
;; only some modes, put the following code in your .emacs:
;;
;; (add-hook 'c-mode-common-hook 'superword-mode)
;;

;; Acknowledgment:
;; The regular expressions to detect subwords are mostly based on
;; the old `c-forward-into-nomenclature' originally contributed by
;; Terry_Glanfield dot Southern at rxuk dot xerox dot com.

;; TODO: ispell-word.

;;; Code:

(defvar subword-forward-function 'subword-forward-internal
  "Function to call for forward subword movement.")

(defvar subword-backward-function 'subword-backward-internal
  "Function to call for backward subword movement.")

(defvar subword-forward-regexp
  "\\W*\\(\\([[:upper:]]*\\(\\W\\)?\\)[[:lower:][:digit:]]*\\)"
  "Regexp used by `subword-forward-internal'.")

(defvar subword-backward-regexp
  "\\(\\(\\W\\|[[:lower:][:digit:]]\\)\\([[:upper:]]+\\W*\\)\\|\\W\\w+\\)"
  "Regexp used by `subword-backward-internal'.")

(defvar subword-mode-map
  ;; We originally remapped motion keys here, but now use Emacs core
  ;; hooks.  Leave this keymap around so that user additions to it
  ;; keep working.
  (make-sparse-keymap)
  "Keymap used in `subword-mode' minor mode.")

;;;###autoload
(define-obsolete-function-alias
  'capitalized-words-mode 'subword-mode "25.1")

;;;###autoload
(define-minor-mode subword-mode
  "Toggle subword movement and editing (Subword mode).
With a prefix argument ARG, enable Subword mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

Subword mode is a buffer-local minor mode.  Enabling it changes
the definition of a word so that word-based commands stop inside
symbols with mixed uppercase and lowercase letters,
e.g. \"GtkWidget\", \"EmacsFrameClass\", \"NSGraphicsContext\".

Here we call these mixed case symbols `nomenclatures'.  Each
capitalized (or completely uppercase) part of a nomenclature is
called a `subword'.  Here are some examples:

  Nomenclature           Subwords
  ===========================================================
  GtkWindow          =>  \"Gtk\" and \"Window\"
  EmacsFrameClass    =>  \"Emacs\", \"Frame\" and \"Class\"
  NSGraphicsContext  =>  \"NS\", \"Graphics\" and \"Context\"

This mode changes the definition of a word so that word commands
treat nomenclature boundaries as word boundaries.

\\{subword-mode-map}"
    :lighter " ,"
    (when subword-mode (superword-mode -1))
    (subword-setup-buffer))

(define-obsolete-function-alias 'c-subword-mode 'subword-mode "23.2")

;;;###autoload
(define-global-minor-mode global-subword-mode subword-mode
  (lambda () (subword-mode 1))
  :group 'convenience)

;; N.B. These commands aren't used unless explicitly invoked; they're
;; here for compatibility.  Today, subword-mode leaves motion commands
;; alone and uses `find-word-boundary-function-table' to change how
;; `forward-word' and other low-level commands detect word boundaries.
;; This way, all word-related activities, not just the images we
;; imagine here, get subword treatment.

(defun subword-forward (&optional arg)
  "Do the same as `forward-word' but on subwords.
See the command `subword-mode' for a description of subwords.
Optional argument ARG is the same as for `forward-word'."
  (interactive "^p")
  (unless arg (setq arg 1))
  (cond
   ((< 0 arg)
    (dotimes (_i arg (point))
      (funcall subword-forward-function)))
   ((> 0 arg)
    (dotimes (_i (- arg) (point))
      (funcall subword-backward-function)))
   (t
    (point))))

(put 'subword-forward 'CUA 'move)

(defun subword-backward (&optional arg)
  "Do the same as `backward-word' but on subwords.
See the command `subword-mode' for a description of subwords.
Optional argument ARG is the same as for `backward-word'."
  (interactive "^p")
  (subword-forward (- (or arg 1))))

(defun subword-right (&optional arg)
  "Do the same as `right-word' but on subwords."
  (interactive "^p")
  (if (eq (current-bidi-paragraph-direction) 'left-to-right)
      (subword-forward arg)
    (subword-backward arg)))

(defun subword-left (&optional arg)
  "Do the same as `left-word' but on subwords."
  (interactive "^p")
  (if (eq (current-bidi-paragraph-direction) 'left-to-right)
      (subword-backward arg)
    (subword-forward arg)))

(defun subword-mark (arg)
  "Do the same as `mark-word' but on subwords.
See the command `subword-mode' for a description of subwords.
Optional argument ARG is the same as for `mark-word'."
  ;; This code is almost copied from `mark-word' in GNU Emacs.
  (interactive "p")
  (cond ((and (eq last-command this-command) (mark t))
	 (set-mark
	  (save-excursion
	    (goto-char (mark))
	    (subword-forward arg)
	    (point))))
	(t
	 (push-mark
	  (save-excursion
	    (subword-forward arg)
	    (point))
	  nil t))))

(put 'subword-backward 'CUA 'move)

(defun subword-kill (arg)
  "Do the same as `kill-word' but on subwords.
See the command `subword-mode' for a description of subwords.
Optional argument ARG is the same as for `kill-word'."
  (interactive "p")
  (kill-region (point) (subword-forward arg)))

(defun subword-backward-kill (arg)
  "Do the same as `backward-kill-word' but on subwords.
See the command `subword-mode' for a description of subwords.
Optional argument ARG is the same as for `backward-kill-word'."
  (interactive "p")
  (subword-kill (- arg)))

(defun subword-transpose (arg)
  "Do the same as `transpose-words' but on subwords.
See the command `subword-mode' for a description of subwords.
Optional argument ARG is the same as for `transpose-words'."
  (interactive "*p")
  (transpose-subr 'subword-forward arg))

(defun subword-downcase (arg)
  "Do the same as `downcase-word' but on subwords.
See the command `subword-mode' for a description of subwords.
Optional argument ARG is the same as for `downcase-word'."
  (interactive "p")
  (let ((start (point)))
    (downcase-region (point) (subword-forward arg))
    (when (< arg 0)
      (goto-char start))))

(defun subword-upcase (arg)
  "Do the same as `upcase-word' but on subwords.
See the command `subword-mode' for a description of subwords.
Optional argument ARG is the same as for `upcase-word'."
  (interactive "p")
  (let ((start (point)))
    (upcase-region (point) (subword-forward arg))
    (when (< arg 0)
      (goto-char start))))

(defun subword-capitalize (arg)
  "Do the same as `capitalize-word' but on subwords.
See the command `subword-mode' for a description of subwords.
Optional argument ARG is the same as for `capitalize-word'."
  (interactive "p")
  (condition-case nil
      (let ((count (abs arg))
            (start (point))
            (advance (>= arg 0)))

        (dotimes (_i count)
          (if advance
              (progn
                (re-search-forward "[[:alpha:]]")
                (goto-char (match-beginning 0)))
            (subword-backward))
          (let* ((p (point))
                 (pp (1+ p))
                 (np (subword-forward)))
            (upcase-region p pp)
            (downcase-region pp np)
            (goto-char (if advance np p))))
        (unless advance
          (goto-char start)))
    (search-failed nil)))



(defvar superword-mode-map subword-mode-map
  "Keymap used in `superword-mode' minor mode.")

;;;###autoload
(define-minor-mode superword-mode
  "Toggle superword movement and editing (Superword mode).
With a prefix argument ARG, enable Superword mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

Superword mode is a buffer-local minor mode.  Enabling it changes
the definition of words such that symbols characters are treated
as parts of words: e.g., in `superword-mode',
\"this_is_a_symbol\" counts as one word.

\\{superword-mode-map}"
    :lighter " ²"
    (when superword-mode (subword-mode -1))
    (subword-setup-buffer))

;;;###autoload
(define-global-minor-mode global-superword-mode superword-mode
  (lambda () (superword-mode 1))
  :group 'convenience)


;;
;; Internal functions
;;
(defun subword-forward-internal ()
  (if superword-mode
      (forward-symbol 1)
    (if (and
         (save-excursion
           (let ((case-fold-search nil))
             (re-search-forward subword-forward-regexp nil t)))
         (> (match-end 0) (point)))
        (goto-char
         (cond
          ((and (< 1 (- (match-end 2) (match-beginning 2)))
                ;; If we have an all-caps word with no following lower-case or
                ;; non-word letter, don't leave the last char (bug#13758).
                (not (and (null (match-beginning 3))
                          (eq (match-end 2) (match-end 1)))))
           (1- (match-end 2)))
          (t
           (match-end 0))))
      (forward-word 1))))

(defun subword-backward-internal ()
  (if superword-mode
      (forward-symbol -1)
    (if (save-excursion
          (let ((case-fold-search nil))
            (re-search-backward subword-backward-regexp nil t)))
        (goto-char
         (cond
          ((and (match-end 3)
                (< 1 (- (match-end 3) (match-beginning 3)))
                (not (eq (point) (match-end 3))))
           (1- (match-end 3)))
          (t
           (1+ (match-beginning 0)))))
      (backward-word 1))))

(defconst subword-find-word-boundary-function-table
  (let ((tab (make-char-table nil)))
    (set-char-table-range tab t #'subword-find-word-boundary)
    tab)
  "Assigned to `find-word-boundary-function-table' in
`subword-mode' and `superword-mode'; defers to
`subword-find-word-boundary'.")

(defconst subword-empty-char-table
  (make-char-table nil)
  "Assigned to `find-word-boundary-function-table' while we're
searching subwords in order to avoid unwanted reentrancy.")

(defun subword-setup-buffer ()
  (set (make-local-variable 'find-word-boundary-function-table)
       (if (or subword-mode superword-mode)
           subword-find-word-boundary-function-table
         subword-empty-char-table)))

(defun subword-find-word-boundary (pos limit)
  "Catch-all handler in `subword-find-word-boundary-function-table'."
  (let ((find-word-boundary-function-table subword-empty-char-table))
    (save-match-data
      (save-excursion
        (save-restriction
          (if (< pos limit)
              (progn
                (goto-char pos)
                (narrow-to-region (point-min) limit)
                (funcall subword-forward-function))
            (goto-char (1+ pos))
            (narrow-to-region limit (point-max))
            (funcall subword-backward-function))
          (point))))))



(provide 'subword)
(provide 'superword)
(provide 'cap-words) ; Obsolete alias

;;; subword.el ends here
