;;; prog-mode.el --- Generic major mode for programming  -*- lexical-binding: t -*-

;; Copyright (C) 2013-2022 Free Software Foundation, Inc.

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
                                prettify-symbols-mode))

(defun prog-context-menu (menu click)
  "Populate MENU with xref commands at CLICK."
  (require 'xref)
  (define-key-after menu [prog-separator] menu-bar-separator
    'middle-separator)

  (unless (xref-marker-stack-empty-p)
    (define-key-after menu [xref-pop]
      '(menu-item "Go Back" xref-pop-marker-stack
                  :help "Back to the position of the last search")
      'prog-separator))

  (let ((identifier (save-excursion
                      (mouse-set-point click)
                      (xref-backend-identifier-at-point
                       (xref-find-backend)))))
    (when identifier
      (define-key-after menu [xref-find-ref]
        `(menu-item "Find References" xref-find-references-at-mouse
                    :help ,(format "Find references to `%s'" identifier))
        'prog-separator)
      (define-key-after menu [xref-find-def]
        `(menu-item "Find Definition" xref-find-definitions-at-mouse
                    :help ,(format "Find definition of `%s'" identifier))
        'prog-separator)))

  (when (thing-at-mouse click 'symbol)
    (define-key-after menu [select-region mark-symbol]
      `(menu-item "Symbol"
                  ,(lambda (e) (interactive "e") (mark-thing-at-mouse e 'symbol))
                  :help "Mark the symbol at click for a subsequent cut/copy")
      'mark-whole-buffer))
  (define-key-after menu [select-region mark-list]
    `(menu-item "List"
                ,(lambda (e) (interactive "e") (mark-thing-at-mouse e 'list))
                :help "Mark the list at click for a subsequent cut/copy")
    'mark-whole-buffer)
  (define-key-after menu [select-region mark-defun]
    `(menu-item "Defun"
                ,(lambda (e) (interactive "e") (mark-thing-at-mouse e 'defun))
                :help "Mark the defun at click for a subsequent cut/copy")
    'mark-whole-buffer)

  ;; Include text-mode select menu only in strings and comments.
  (when (nth 8 (save-excursion
                 (with-current-buffer (window-buffer (posn-window (event-end click)))
                   (syntax-ppss (posn-point (event-end click))))))
    (text-mode-context-menu menu click))

  menu)

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
  "Alist of symbol prettifications.
Each element looks like (SYMBOL . CHARACTER), where the symbol
matching SYMBOL (a string, not a regexp) will be shown as
CHARACTER instead.

CHARACTER can be a character, or it can be a list or vector, in
which case it will be used to compose the new symbol as per the
third argument of `compose-region'.")

(defun prettify-symbols-default-compose-p (start end _match)
  "Return non-nil iff the symbol MATCH should be composed.
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
as the `match-string' as arguments.")

(defun prettify-symbols--compose-symbol (alist)
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
          (compose-region start end (cdr (assoc match alist)))
          (add-text-properties
           start end
           `(prettify-symbols-start ,start prettify-symbols-end ,end)))
      ;; No composition for you.  Let's actually remove any
      ;; composition we may have added earlier and which is now
      ;; incorrect.
      (remove-list-of-text-properties start end
                                      '(composition
                                        prettify-symbols-start
                                        prettify-symbols-end))))
  ;; Return nil because we're not adding any face property.
  nil)

(defun prettify-symbols--make-keywords ()
  (if prettify-symbols-alist
      `((,(regexp-opt (mapcar 'car prettify-symbols-alist) t)
         (0 (prettify-symbols--compose-symbol ',prettify-symbols-alist))))
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
                 (const :tag "Unprettify when point is inside or at right edge" right-edge)))

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
  (add-hook 'context-menu-functions 'prog-context-menu 10 t)
  ;; Any programming language is always written left to right.
  (setq bidi-paragraph-direction 'left-to-right))

(provide 'prog-mode)

;;; prog-mode.el ends here
