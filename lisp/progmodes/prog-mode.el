;;; prog-mode.el --- Generic major mode for programming  -*- lexical-binding: t -*-

;; Copyright (C) 2013-2020 Free Software Foundation, Inc.

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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This major mode is mostly intended as a parent of other programming
;; modes.  All major modes for programming languages should derive from this
;; mode so that users can put generic customization on prog-mode-hook.

;;; Code:

(eval-when-compile (require 'cl-lib)
                   (require 'subr-x))

(defgroup prog-mode nil
  "Generic programming mode, from which others derive."
  :group 'languages)

(defcustom prog-mode-hook nil
  "Normal hook run when entering programming modes."
  :type 'hook
  :options '(flyspell-prog-mode abbrev-mode flymake-mode
                                display-line-numbers-mode
                                prettify-symbols-mode)
  :group 'prog-mode)

(defvar prog-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?\C-\M-q] 'prog-indent-sexp)
    map)
  "Keymap used for programming modes.")

(defvar prog-indentation-context nil
  "When non-nil, provides context for indenting embedded code chunks.

There are languages where part of the code is actually written in
a sub language, e.g., a Yacc/Bison or ANTLR grammar can also include
JS or Python code.  This variable enables the primary mode of the
main language to use the indentation engine of the sub-mode for
lines in code chunks written in the sub-mode's language.

When a major mode of such a main language decides to delegate the
indentation of a line/region to the indentation engine of the sub
mode, it should bind this variable to non-nil around the call.

The non-nil value should be a list of the form:

   (FIRST-COLUMN . REST)

FIRST-COLUMN is the column the indentation engine of the sub-mode
should use for top-level language constructs inside the code
chunk (instead of 0).

REST is currently unused.")

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

(defvar-local prettify-symbols-alist nil
  "Alist of symbol string prettifications.
Each element can look like (STRING . CHARACTER), where the
STRING (a string, not a regexp) will be shown as CHARACTER
instead.

For example: \"->\" to the Unicode RIGHT ARROW →
 (\"->\" . ?→)

Elements can also look like (IDENTIFIER REGEXP CHARACTER) which
will behave like the simpler (SYMBOL-STRING . CHARACTER) form
except it will match regular expressions. The REGEXP can have
capturing groups, in which case the first such group will be
prettified. If there are no capturing groups, the whole REGEXP is
prettified.

The IDENTIFIER can be any symbol and should be unique to every
package that augments `prettify-symbols-alist' (in order to
remove prettifications easily with
`prettify-symbols-remove-prettifications').

For example: \"abc[123]\" matching \"abc1\", \"abc2\", or
\"abc3\" could be mapped to the Unicode WORLD MAP. Note again the
IDENTIFIER is an arbitrary Lisp symbol.
 (my-worldmap \"abc[123]\" ?\U0001f5fa)

CHARACTER can be a character, or it can be a list or vector, in
which case it will be used to compose the new symbol as per the
third argument of `compose-region'.")

(defun prettify-symbols-default-compose-p (start end _match)
  "Return true iff the symbol MATCH should be composed.
The symbol starts at position START and ends at position END.
This is the default for `prettify-symbols-compose-predicate'
which is suitable for most programming languages such as C or Lisp."
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
  "A predicate for deciding if the currently matched symbol is to be composed.
The matched symbol is the car of one entry in `prettify-symbols-alist'.
The predicate receives the match's start and end positions as well
as the match-string as arguments.")

;; (prettify-symbols-default-compose-replacer '(("xyz" 231) (prettify-regexp "aaa\\(bbb\\)" 169)) 568 574 "aaabbb")
(defun prettify-symbols-default-compose-replacer (alist start end match &optional identifier)
  "Return the compose-region prettification for MATCH from ALIST.
START and END are passed back and may be modified (narrowed)."
  (let ((quick-assoc (cdr (assoc match alist))))
    (if quick-assoc
        ;; Return the quick lookup if we can, else...
        (list start end quick-assoc)
      (cl-loop for ps in alist
               ;; Did we get a valid regexp entry, and does it match
               ;; the identifier (if packaged in the call) or the regexp?
               if (and (symbolp (car-safe ps))
                       ;; We must match the identifier symbol if we got it.
                       (if identifier
                           (eq identifier (car ps))
                         t) ; But if there's no identifier, pass safely.

                       ;; ...We need to always do a string-match for the bounds.
                       (string-match (nth 1 ps) match))
               ;; Now return the actual prettification start and end.
               return (list (+ start (or
                                      (match-beginning 1)
                                      (match-beginning 0)))
                            (+ start (or
                                      (match-end 1)
                                      (match-end 0)))
                            (nth 2 ps))))))

(defvar-local prettify-symbols-compose-replacer
  #'prettify-symbols-default-compose-replacer
  "A function to generate the replacement for a matched string.
The function receives the current prettify-symbols-alist, the
match's start and end positions, and the match-string as
arguments.

For regexp matches, the function will also receive the symbol
that identifies the match, as per `prettify-symbols-alist'.")

(defun prettify-symbols--compose-symbol (alist &optional identifier)
  "Compose a sequence of characters into a symbol.
Regexp match data 0 specifies the characters to be composed."
  ;; Check that the chars should really be composed into a symbol.
  (let ((start (match-beginning 0))
        (end (match-end 0))
        (match (match-string 0)))
    (if (and (not (equal prettify-symbols--current-symbol-bounds (list start end)))
             (funcall prettify-symbols-compose-predicate start end match))
        ;; That's a symbol alright, so add the composition.
        (with-silent-modifications
          (let* ((replacement (funcall prettify-symbols-compose-replacer
                                       alist start end match identifier))
                 (start (nth 0 replacement))
                 (end (nth 1 replacement)))
            (apply #'compose-region replacement)
            (add-text-properties
             start end
             `(prettify-symbols-start ,start prettify-symbols-end ,end))))
      ;; No composition for you.  Let's actually remove any
      ;; composition we may have added earlier and which is now
      ;; incorrect.
      (remove-list-of-text-properties start end
                                      '(composition
                                        prettify-symbols-start
                                        prettify-symbols-end))))
  ;; Return nil because we're not adding any face property.
  nil)

(defun prettify-symbols--make-fixed-matcher (alist)
  "Make the fixed string matcher portion of the font-lock keywords from ALIST."
  (regexp-opt (cl-loop for s in (mapcar 'car alist)
                       if (stringp s)
                       collect s)
              t))

(defun prettify-symbols--make-regexp-keywords (alist)
  "Make the regexp string matcher portion of the font-lock keywords from ALIST."
  ;; Collect the symbols to generate matchers keyed on them.
  (cl-loop for ps in alist
           if (symbolp (car-safe ps))
           collect `(
                     ,(nth 1 ps)          ; the regexp
                     ;; the symbol composer called with the identifier
                     (0 (prettify-symbols--compose-symbol
                         ',prettify-symbols-alist
                         ',(car ps))))))

(defun prettify-symbols--make-keywords ()
  (if prettify-symbols-alist
      `((,(prettify-symbols--make-fixed-matcher prettify-symbols-alist)
         (0 (prettify-symbols--compose-symbol ',prettify-symbols-alist)))
        ,@(prettify-symbols--make-regexp-keywords prettify-symbols-alist))
    nil))

(defvar-local prettify-symbols--keywords nil)

(defvar-local prettify-symbols--current-symbol-bounds nil)

(defcustom prettify-symbols-unprettify-at-point nil
  "If non-nil, show the non-prettified version of a symbol when point is on it.
If set to the symbol `right-edge', also unprettify if point
is immediately after the symbol.  The prettification will be
reapplied as soon as point moves away from the symbol.  If
set to nil, the prettification persists even when point is
on the symbol."
  :version "25.1"
  :type '(choice (const :tag "Never unprettify" nil)
                 (const :tag "Unprettify when point is inside" t)
                 (const :tag "Unprettify when point is inside or at right edge" right-edge))
  :group 'prog-mode)

(defun prettify-symbols--post-command-hook ()
  (cl-labels ((get-prop-as-list
               (prop)
               (remove nil
                       (list (get-text-property (point) prop)
                             (when (and (eq prettify-symbols-unprettify-at-point 'right-edge)
                                        (not (bobp)))
                               (get-text-property (1- (point)) prop))))))
    ;; Re-apply prettification to the previous symbol.
    (when (and prettify-symbols--current-symbol-bounds
	       (or (< (point) (car prettify-symbols--current-symbol-bounds))
		   (> (point) (cadr prettify-symbols--current-symbol-bounds))
		   (and (not (eq prettify-symbols-unprettify-at-point 'right-edge))
			(= (point) (cadr prettify-symbols--current-symbol-bounds)))))
      ;; Adjust the bounds in case either end is invalid.
      (setf (car prettify-symbols--current-symbol-bounds)
            (max (car prettify-symbols--current-symbol-bounds) (point-min))
            (cadr prettify-symbols--current-symbol-bounds)
            (min (cadr prettify-symbols--current-symbol-bounds) (point-max)))
      (apply #'font-lock-flush prettify-symbols--current-symbol-bounds)
      (setq prettify-symbols--current-symbol-bounds nil))
    ;; Unprettify the current symbol.
    (when-let* ((c (get-prop-as-list 'composition))
	        (s (get-prop-as-list 'prettify-symbols-start))
	        (e (get-prop-as-list 'prettify-symbols-end))
	        (s (apply #'min s))
	        (e (apply #'max e)))
      (with-silent-modifications
	(setq prettify-symbols--current-symbol-bounds (list s e))
        (remove-text-properties s e '(composition nil))))))

(defun prettify-symbols-add-prettification-entry (entry)
  "Add ENTRY to `prettify-symbols-alist' for the current buffer.
ENTRY is formatted as per `prettify-symbols-alist' (which see).
Duplicates according to `equal' will not be added."
  (setq-local prettify-symbols-alist (cl-adjoin entry
                                                prettify-symbols-alist
                                                :test #'equal)))

(defun prettify-symbols-add-prettification-rx (identifier regexp replacement)
  "Convenience wrapper of `prettify-symbols-add-prettification-entry' to prettify REGEXP with REPLACEMENT."
  (prettify-symbols-add-prettification-entry
   (list identifier regexp replacement)))

(defun prettify-symbols-add-prettification-string (fixed-string replacement)
  "Convenience wrapper of `prettify-symbols-add-prettification-entry' to prettify FIXED-STRING with REPLACEMENT."
  (prettify-symbols-add-prettification-entry
   (cons fixed-string replacement)))

(defun prettify-symbols-remove-prettification (entry)
  "Remove ENTRY to `prettify-symbols-alist' for the current buffer.
ENTRY is found with an `equal' test."
  (setq-local prettify-symbols-alist (cl-remove entry
                                                prettify-symbols-alist
                                                :test #'equal)))

(defun prettify-symbols-remove-prettifications (identifier)
  "Remove all IDENTIFIER entries from `prettify-symbols-alist' for the current buffer.
IDENTIFIER is as per `prettify-symbols-alist' (which see)."
  (setq-local prettify-symbols-alist (cl-remove identifier
                                                prettify-symbols-alist
                                                :test #'car)))

;;;###autoload
(define-minor-mode prettify-symbols-mode
  "Toggle Prettify Symbols mode.

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
  (when prettify-symbols--keywords
    (font-lock-remove-keywords nil prettify-symbols--keywords)
    (setq prettify-symbols--keywords nil))
  (if prettify-symbols-mode
      ;; Turn on
      (when (setq prettify-symbols--keywords (prettify-symbols--make-keywords))
        (font-lock-add-keywords nil prettify-symbols--keywords)
        (setq-local font-lock-extra-managed-props
                    (append font-lock-extra-managed-props
                            '(composition
                              prettify-symbols-start
                              prettify-symbols-end)))
        (when prettify-symbols-unprettify-at-point
          (add-hook 'post-command-hook
                    #'prettify-symbols--post-command-hook nil t))
        (font-lock-flush))
    ;; Turn off
    (remove-hook 'post-command-hook #'prettify-symbols--post-command-hook t)
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
