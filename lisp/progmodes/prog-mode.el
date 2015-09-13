;;; prog-mode.el --- Generic major mode for programming  -*- lexical-binding: t -*-

;; Copyright (C) 2013-2015 Free Software Foundation, Inc.

;; Maintainer: emacs-devel@gnu.org
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

(defcustom prog-mode-hook nil
  "Normal hook run when entering programming modes."
  :type 'hook
  :options '(flyspell-prog-mode abbrev-mode flymake-mode linum-mode
                                prettify-symbols-mode)
  :group 'prog-mode)

(defvar prog-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?\C-\M-q] 'prog-indent-sexp)
    map)
  "Keymap used for programming modes.")

(defvar prog-indentation-context nil
  "Non-nil while indenting embedded code chunks.
There are languages where part of the code is actually written in
a sub language, e.g., a Yacc/Bison or ANTLR grammar also consists
of plain C code.  This variable enables the major mode of the
main language to use the indentation engine of the sub mode for
lines in code chunks written in the sub language.

When a major mode of such a main language decides to delegate the
indentation of a line/region to the indentation engine of the sub
mode, it is supposed to bind this variable to non-nil around the call.

The non-nil value looks as follows
   \(FIRST-COLUMN (START . END) PREVIOUS-CHUNKS)

FIRST-COLUMN is the column the indentation engine of the sub mode
should usually choose for top-level language constructs inside
the code chunk (instead of 0).

START to END is the region of the code chunk.  See function
`prog-widen' for additional info.

PREVIOUS-CHUNKS, if non-nil, provides the indentation engine of
the sub mode with the virtual context of the code chunk.  Valid
values are:

 - A string containing code which the indentation engine can
   consider as standing in front of the code chunk.  To cache the
   string's calculated syntactic information for repeated calls
   with the same string, it is valid and expected for the inner
   mode to add text-properties to the string.

   A typical use case is for grammars with code chunks which are
   to be indented like function bodies - the string would contain
   a corresponding function header.

 - A function called with the start position of the current
   chunk.  It will return either the region of the previous chunk
   as \(PREV-START . PREV-END) or nil if there is no further
   previous chunk.

   A typical use case are literate programming sources - the
   function would successively return the code chunks of the
   previous macro definitions for the same name.")

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

(defun prog-first-column ()
  "Return the indentation column normally used for top-level constructs."
  (or (car prog-indentation-context) 0))

(defun prog-widen ()
  "Remove restrictions (narrowing) from current code chunk or buffer.
This function can be used instead of `widen' in any function used
by the indentation engine to make it respect the value
`prog-indentation-context'.

This function (like `widen') is useful inside a
`save-restriction' to make the indentation correctly work when
narrowing is in effect."
  (let ((chunk (cadr prog-indentation-context)))
    (if chunk
        ;; no widen necessary here, as narrow-to-region changes (not
        ;; just narrows) existing restrictions
        (narrow-to-region (car chunk) (or (cdr chunk) (point-max)))
      (widen))))


(defvar-local prettify-symbols-alist nil
  "Alist of symbol prettifications.
Each element looks like (SYMBOL . CHARACTER), where the symbol
matching SYMBOL (a string, not a regexp) will be shown as
CHARACTER instead.")

(defun prettify-symbols-default-compose-p (start end _match)
  "Return true iff the symbol MATCH should be composed.
The symbol starts at position START and ends at position END.
This is default `prettify-symbols-compose-predicate' which is
suitable for most programming languages such as C or Lisp."
  ;; Check that the chars should really be composed into a symbol.
  (let* ((syntaxes-beg (if (memq (char-syntax (char-after start)) '(?w ?_))
                           '(?w ?_) '(?. ?\\)))
         (syntaxes-end (if (memq (char-syntax (char-before end)) '(?w ?_))
                           '(?w ?_) '(?. ?\\))))
    (not (or (memq (char-syntax (or (char-before start) ?\s)) syntaxes-beg)
             (memq (char-syntax (or (char-after end) ?\s)) syntaxes-end)
             (nth 8 (syntax-ppss))))))

(defvar-local prettify-symbols-compose-predicate
  #'prettify-symbols-default-compose-p
  "A predicate deciding if the currently matched symbol is to be composed.
The matched symbol is the car of one entry in `prettify-symbols-alist'.
The predicate receives the match's start and end position as well
as the match-string as arguments.")

(defun prettify-symbols--compose-symbol (alist)
  "Compose a sequence of characters into a symbol.
Regexp match data 0 points to the chars."
  ;; Check that the chars should really be composed into a symbol.
  (let ((start (match-beginning 0))
        (end (match-end 0))
        (match (match-string 0)))
    (if (funcall prettify-symbols-compose-predicate start end match)
        ;; That's a symbol alright, so add the composition.
        (compose-region start end (cdr (assoc match alist)))
      ;; No composition for you.  Let's actually remove any
      ;; composition we may have added earlier and which is now
      ;; incorrect.
      (remove-text-properties start end '(composition))))
  ;; Return nil because we're not adding any face property.
  nil)

(defun prettify-symbols--make-keywords ()
  (if prettify-symbols-alist
      `((,(regexp-opt (mapcar 'car prettify-symbols-alist) t)
         (0 (prettify-symbols--compose-symbol ',prettify-symbols-alist))))
    nil))

(defvar-local prettify-symbols--keywords nil)

;;;###autoload
(define-minor-mode prettify-symbols-mode
  "Toggle Prettify Symbols mode.
With a prefix argument ARG, enable Prettify Symbols mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

When Prettify Symbols mode and font-locking are enabled, symbols are
prettified (displayed as composed characters) according to the rules
in `prettify-symbols-alist' (which see), which are locally defined
by major modes supporting prettifying.  To add further customizations
for a given major mode, you can modify `prettify-symbols-alist' thus:

  (add-hook \\='emacs-lisp-mode-hook
            (lambda ()
              (push \\='(\"<=\" . ?≤) prettify-symbols-alist)))

You can enable this mode locally in desired buffers, or use
`global-prettify-symbols-mode' to enable it for all modes that
support it."
  :init-value nil
  (if prettify-symbols-mode
      ;; Turn on
      (when (setq prettify-symbols--keywords (prettify-symbols--make-keywords))
        (font-lock-add-keywords nil prettify-symbols--keywords)
        (setq-local font-lock-extra-managed-props
                    (cons 'composition font-lock-extra-managed-props))
        (font-lock-flush))
    ;; Turn off
    (when prettify-symbols--keywords
      (font-lock-remove-keywords nil prettify-symbols--keywords)
      (setq prettify-symbols--keywords nil))
    (when (memq 'composition font-lock-extra-managed-props)
      (setq font-lock-extra-managed-props (delq 'composition
                                                font-lock-extra-managed-props))
      (with-silent-modifications
        (remove-text-properties (point-min) (point-max) '(composition nil))))))

(defun turn-on-prettify-symbols-mode ()
  (when (and (not prettify-symbols-mode)
             (local-variable-p 'prettify-symbols-alist))
    (prettify-symbols-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-prettify-symbols-mode
  prettify-symbols-mode turn-on-prettify-symbols-mode)

;;;###autoload
(define-derived-mode prog-mode fundamental-mode "Prog"
  "Major mode for editing programming language source code."
  (setq-local require-final-newline mode-require-final-newline)
  (setq-local parse-sexp-ignore-comments t)
  ;; Any programming language is always written left to right.
  (setq bidi-paragraph-direction 'left-to-right))

(provide 'prog-mode)

;;; prog-mode.el ends here
