;;; prog-mode.el --- Generic major mode for programming  -*- lexical-binding: t -*-

;; Copyright (C) 2013 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: internal
;; Package: emacs

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This major mode is mostly intended as a parent of other programming
;; modes.  All major modes for programming languages should derive from this
;; mode so that users can put generic customization on prog-mode-hook.

;;; Code:

(eval-when-compile (require 'cl-lib))

(defgroup prog-mode nil
  "Generic programming mode, from which others derive."
  :group 'languages)

(defvar prog-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?\C-\M-q] 'prog-indent-sexp)
    map)
  "Keymap used for programming modes.")

(defun prog-indent-sexp (&optional defun)
  "Indent the expression after point.
When interactively called with prefix, indent the enclosing defun
instead."
  (interactive "P")
  (save-excursion
    (when defun
      (end-of-line)
      (beginning-of-defun))
    (let ((start (point))
	  (end (progn (forward-sexp 1) (point))))
      (indent-region start end nil))))

(defvar prog-prettify-symbols-alist nil)

(defcustom prog-prettify-symbols nil
  "Whether symbols should be prettified.
When set to an alist in the form `((STRING . CHARACTER)...)' it
will augment the mode's native prettify alist."
  :type '(choice
          (const :tag "No thanks" nil)
          (const :tag "Mode defaults" t)
          (alist :tag "Mode defaults augmented with your own list"
                 :key-type string :value-type character))
  :version "24.4")

(defun prog--prettify-font-lock-compose-symbol (alist)
  "Compose a sequence of ascii chars into a symbol.
Regexp match data 0 points to the chars."
  ;; Check that the chars should really be composed into a symbol.
  (let* ((start (match-beginning 0))
	 (end (match-end 0))
	 (syntaxes (if (eq (char-syntax (char-after start)) ?w)
		       '(?w) '(?. ?\\))))
    (if (or (memq (char-syntax (or (char-before start) ?\ )) syntaxes)
	    (memq (char-syntax (or (char-after end) ?\ )) syntaxes)
            (nth 8 (syntax-ppss)))
	;; No composition for you.  Let's actually remove any composition
	;; we may have added earlier and which is now incorrect.
	(remove-text-properties start end '(composition))
      ;; That's a symbol alright, so add the composition.
      (compose-region start end (cdr (assoc (match-string 0) alist)))))
  ;; Return nil because we're not adding any face property.
  nil)

(defun prog-prettify-font-lock-symbols-keywords ()
  (when prog-prettify-symbols
    (let ((alist (append prog-prettify-symbols-alist
                         (if (listp prog-prettify-symbols)
                             prog-prettify-symbols
                           nil))))
      `((,(regexp-opt (mapcar 'car alist) t)
         (0 (prog--prettify-font-lock-compose-symbol ',alist)))))))

(defun prog-prettify-install (alist)
"Install prog-mode support to prettify symbols according to ALIST.

ALIST is in the format `((STRING . CHARACTER)...)' like
`prog-prettify-symbols'.

Internally, `font-lock-add-keywords' is called."
  (setq-local prog-prettify-symbols-alist alist)
  (let ((keywords (prog-prettify-font-lock-symbols-keywords)))
    (if keywords (font-lock-add-keywords nil keywords))))

;;;###autoload
(define-derived-mode prog-mode fundamental-mode "Prog"
  "Major mode for editing programming language source code."
  (set (make-local-variable 'require-final-newline) mode-require-final-newline)
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  ;; Any programming language is always written left to right.
  (setq bidi-paragraph-direction 'left-to-right))

(provide 'prog-mode)

;;; prog-mode.el ends here
