;;; tildify.el --- adding hard spaces into texts -*- lexical-binding: t -*-

;; Copyright (C) 1997-2022 Free Software Foundation, Inc.

;; Author:     Milan Zamazal <pdm@zamazal.org>
;;             Michal Nazarewicz <mina86@mina86.com>
;; Version:    4.6.1
;; Keywords:   text, TeX, SGML, wp

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

;; This package can be typically used for adding forgotten tildes in TeX
;; sources or adding `&nbsp;' sequences in SGML (e.g. HTML) texts.
;;
;; For example, the Czech orthography requires avoiding one letter
;; prepositions at line endings.  So they should be connected with the
;; following words by a tilde.  Some users forget to do this all the
;; time.  The purpose of this program is to check the text and suggest
;; adding of missing tildes on some places.  It works in a similar
;; manner to `query-replace-regexp'.
;;
;; The functionality of this program is actually performing query
;; replace on certain regions, but for historical reasons explained
;; above it is called `tildify'.
;;
;; The default variable settings are suited for Czech, so do not try to
;; understand them if you are not familiar with Czech grammar and spelling.
;;
;; The algorithm was inspired by Petr Olšák's program `vlna'.  Abilities of
;; `tildify.el' are a little limited; if you have improvement suggestions, let
;; me know.

;;; Code:


;;; *** User configuration variables ***


(defgroup tildify nil
  "Add hard spaces or other text fragments to text buffers."
  :version "21.1"
  :group 'text)

(defcustom tildify-pattern
  "\\(?:[,:;(][ \t]*[a]\\|\\<[AIKOSUVZikosuvz]\\)\\([ \t]+\\|[ \t]*\n[ \t]*\\)\\(?:\\w\\|[([{\\]\\|<[a-zA-Z]\\)"
  "A pattern specifying where to insert hard spaces.

`tildify-buffer' function will replace first capturing group of the regexp with
a hard space (as defined by `tildify-space-string' variable).  (Hint: \\(…\\)
non-capturing groups can be used for grouping prior to the part of the regexp
matching the white space).  The pattern is matched case-sensitive regardless of
the value of `case-fold-search' setting."
  :version "25.1"
  :type 'regexp
  :safe #'stringp)

(defcustom tildify-pattern-alist ()
  "Alist specifying where to insert hard spaces.

Each alist item is of the form (MAJOR-MODE REGEXP NUMBER) or
\(MAJOR-MODE . SYMBOL).

MAJOR-MODE defines major mode, for which the item applies.  It can be either:
- a symbol equal to the major mode of the buffer to be fixed
- t for default item, this applies to all major modes not defined in another
  alist item

REGEXP is a regular expression matching the part of a text, where a hard space
is missing.  The regexp is always case sensitive, regardless of the current
`case-fold-search' setting.

NUMBER defines the number of the REGEXP subexpression which should be replaced
by the hard space character.

The form (MAJOR-MODE . SYMBOL) defines alias item for MAJOR-MODE.  For this
mode, the item for the mode SYMBOL is looked up in the alist instead."
  :type '(repeat (cons :tag "Entry for major mode"
                       (choice (const  :tag "Default" t)
                               (symbol :tag "Major mode"))
                       (choice (list   :tag "Regexp"
                                       regexp
                                       (integer :tag "Group "))
                               (symbol :tag "Like other")))))
(make-obsolete-variable 'tildify-pattern-alist 'tildify-pattern "25.1")

(defcustom tildify-space-string "\u00A0"
  "Representation of a hard (a.k.a. no-break) space in current major mode.

Used by `tildify-buffer' in places where space is required but line
cannot be broken.  For example \"~\" for TeX or \"&#160;\" for SGML,
HTML and XML modes.  A no-break space Unicode character (\"\\u00A0\")
might be used for other modes if compatible encoding is used.

If nil, current major mode has no way to represent a hard space."
  :version "25.1"
  :type '(choice (const :tag "Space character (no hard-space representation)"
                        " ")
                 (const :tag "No-break space (U+00A0)" "\u00A0")
                 (string :tag "Custom string"))
  :safe #'string-or-null-p)

(defcustom tildify-string-alist ()
  "Alist specifying what is a hard space in the current major mode.

Each alist item is of the form (MAJOR-MODE . STRING) or
\(MAJOR-MODE . SYMBOL).

MAJOR-MODE defines major mode, for which the item applies.  It can be either:
- a symbol equal to the major mode of the buffer to be fixed
- t for default item, this applies to all major modes not defined in another
  alist item

STRING defines the hard space, which is inserted at places defined by
`tildify-pattern'.  For example it can be \"~\" for TeX or \"&nbsp;\" for SGML.

The form (MAJOR-MODE . SYMBOL) defines alias item for MAJOR-MODE.  For this
mode, the item for the mode SYMBOL is looked up in the alist instead."
  :type '(repeat (cons :tag "Entry for major mode"
                       (choice (const  :tag "Default" t)
                               (symbol :tag "Major mode"))
                       (choice (const  :tag "No-break space (U+00A0)" "\u00A0")
                               (string :tag "String    ")
                               (symbol :tag "Like other")))))
(make-obsolete-variable 'tildify-string-alist
                        'tildify-space-string "25.1")

(defcustom tildify-foreach-region-function
  'tildify--deprecated-ignore-evironments
  "A function calling a callback on portions of the buffer to tildify.

The function is called from `tildify-buffer' function with three arguments: FUNC
BEG END.  FUNC is a callback accepting two arguments -- REG-BEG REG-END --
specifying a portion of buffer to operate on.

The BEG and END arguments may be used to limit portion of the buffer being
scanned, but the `tildify-foreach-region-function' is not required to make use
of them.  IT must, however, terminate as soon as FUNC returns nil.

For example, if `tildify-buffer' function should operate on the whole buffer,
a simple pass through function could be used:
    (setq-local tildify-foreach-region-function
                (lambda (cb beg end) (funcall cb beg end)))
or better still:
    (setq-local tildify-foreach-region-function \\='funcall)
See `tildify-foreach-ignore-environments' function for other ways to use the
variable."
  :version "25.1"
  :type 'function)

(defcustom tildify-ignored-environments-alist ()
  "Alist specifying ignored structured text environments.
Parts of text defined in this alist are skipped without performing hard space
insertion on them.  These setting allow skipping text parts like verbatim or
math environments in TeX or preformatted text in SGML.

Each list element is of the form
  (MAJOR-MODE (BEG-REGEX . END-REGEX) (BEG-REGEX . END-REGEX) ... )

MAJOR-MODE defines major mode, for which the item applies.  It can be either:
- a symbol equal to the major mode of the buffer to be fixed
- t for default item, this applies to all major modes not defined in another
  alist item

See `tildify-foreach-ignore-environments' function for description of BEG-REGEX
and END-REGEX."
  :type '(repeat
          (cons :tag "Entry for major mode"
                (choice (const  :tag "Default" t)
                        (symbol :tag "Major mode"))
                (choice
                 (const  :tag "None")
                 (repeat
                  :tag "Environments"
                  (cons :tag "Regexp pair"
                        (regexp :tag "Open ")
                        (choice :tag "Close"
                                (regexp :tag "Regexp")
                                (list :tag "Regexp and groups (concatenated)"
                                      (choice (regexp  :tag "Regexp")
                                              (integer :tag "Group "))))))
                 (symbol :tag "Like other")))))
(make-obsolete-variable 'tildify-ignored-environments-alist
                        'tildify-foreach-region-function "25.1")


;;; *** Interactive functions ***

;;;###autoload
(defun tildify-region (beg end &optional dont-ask)
  "Add hard spaces in the region between BEG and END.
See variables `tildify-pattern', `tildify-space-string', and
`tildify-ignored-environments-alist' for information about configuration
parameters.
This function performs no refilling of the changed text.
If DONT-ASK is set, or called interactively with prefix argument, user
won't be prompted for confirmation of each substitution."
  (interactive "*rP")
  (let (case-fold-search (count 0) (ask (not dont-ask)))
    (tildify--foreach-region
      (lambda (beg end)
        (let ((aux (tildify-tildify beg end ask)))
          (setq count (+ count (car aux)))
          (if (not (eq (cdr aux) 'force))
              (cdr aux)
            (setq ask nil)
            t)))
      beg end)
    (message "%d spaces replaced." count)))

;;;###autoload
(defun tildify-buffer (&optional dont-ask)
  "Add hard spaces in the current buffer.
See variables `tildify-pattern', `tildify-space-string', and
`tildify-ignored-environments-alist' for information about configuration
parameters.
This function performs no refilling of the changed text.
If DONT-ASK is set, or called interactively with prefix argument, user
won't be prompted for confirmation of each substitution."
  (interactive  "*P")
  (tildify-region (point-min) (point-max) dont-ask))


;;; *** Auxiliary functions ***

(defun tildify--pick-alist-entry (mode-alist &optional mode)
  "Return alist item for the MODE-ALIST in the current major MODE."
  (let ((alist (cdr (or (assoc (or mode major-mode) mode-alist)
			(assoc t mode-alist)))))
    (if (and alist
	     (symbolp alist))
	(tildify--pick-alist-entry mode-alist alist)
      alist)))
(make-obsolete 'tildify--pick-alist-entry
               "it should not be used in new code." "25.1")

(defun tildify--deprecated-ignore-evironments (callback beg end)
  "Call CALLBACK on regions between BEG and END.

Call CALLBACK on each region outside of environment to ignore.  Stop scanning
the region as soon as CALLBACK returns nil.  Environments to ignore are
defined by deprecated `tildify-ignored-environments-alist'.   CALLBACK may be
called on portions of the buffer outside of [BEG END)."
  (let ((pairs (with-suppressed-warnings ((obsolete tildify--pick-alist-entry))
                 (tildify--pick-alist-entry
                  tildify-ignored-environments-alist))))
    (if pairs
        (tildify-foreach-ignore-environments pairs callback beg end)
      (funcall callback beg end))))
(make-obsolete 'tildify--deprecated-ignore-evironments
               "it should not be used in new code." "25.1")

(defun tildify-foreach-ignore-environments (pairs callback _beg end)
  "Outside of environments defined by PAIRS call CALLBACK.

PAIRS is a list of (BEG-REGEX . END-REGEX) cons.  BEG-REGEX is a regexp matching
beginning of a text part to be skipped.  END-REGEX defines end of the
corresponding text part and can be either:
- a regexp matching the end of the skipped text part
- a list of regexps and numbers, which will compose the ending regexp by
  concatenating themselves, while replacing the numbers with corresponding
  subexpressions of BEG-REGEX (this is used to solve cases like
  \\\\verb<character> in TeX).

CALLBACK is a function accepting two arguments -- REG-BEG and REG-END -- that
will be called for portions of the buffer outside of the environments defined by
PAIRS regexps.

The function will return as soon as CALLBACK returns nil or point goes past END.
CALLBACK may be called on portions of the buffer outside of [BEG END); in fact
BEG argument is ignored.

This function is meant to be used to set `tildify-foreach-region-function'
variable.  For example, for an XML file one might use:
  (setq-local tildify-foreach-region-function
    (apply-partially \\='tildify-foreach-ignore-environments
                     \\='((\"<! *--\" . \"-- *>\") (\"<\" . \">\"))))"
  (let ((beg-re (concat "\\(?:" (mapconcat #'car pairs "\\)\\|\\(?:") "\\)"))
        p end-re)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (while (and (< (setq p (point)) end)
                    (if (setq end-re (tildify--find-env beg-re pairs))
                        (and (funcall callback p (match-beginning 0))
                             (< (point) end)
                             (re-search-forward end-re nil t))
                      (funcall callback p end)
                      nil)))))))

(defun tildify--foreach-region (callback beg end)
  "Call CALLBACK on portions of the buffer between BEG and END.

Which portions to call CALLBACK on is determined by
`tildify-foreach-region-function' variable.  This function merely makes sure
CALLBACK is not called with portions of the buffer outside of [BEG END)."
  (let ((func (lambda (reg-beg reg-end)
                (setq reg-beg (max reg-beg beg) reg-end (min reg-end end))
                (and (or (>= reg-beg reg-end)
                         (funcall callback reg-beg reg-end))
                     (< reg-end end)))))
    (funcall tildify-foreach-region-function func beg end)))

(defun tildify--find-env (regexp pairs)
  "Find environment using REGEXP.
Return regexp for the end of the environment found in PAIRS or nil if
no environment was found."
  ;; Find environment
  (when (re-search-forward regexp nil t)
    (save-match-data
      (let ((match (match-string 0)))
        (while (not (eq (string-match (caar pairs) match) 0))
          (setq pairs (cdr pairs)))
        (let ((expression (cdar pairs)))
          (if (stringp expression)
              expression
            (mapconcat
             (lambda (expr)
               (if (stringp expr)
                   expr
                 (regexp-quote (match-string expr match))))
             expression
             "")))))))

(defun tildify-tildify (beg end ask)
  "Add tilde characters in the region between BEG and END.
This function does not do any further checking except of for comments and
macros.

If ASK is nil, perform replace without asking user for confirmation.

Returns (count . response) cons where count is number of string
replacements done and response is one of symbols: t (all right), nil
\(quit), force (replace without further questions)."
  (save-excursion
    (goto-char beg)
    (let ((regexp tildify-pattern)
          (match-number 1)
          (tilde (or (with-suppressed-warnings ((obsolete
                                                 tildify--pick-alist-entry))
                       (tildify--pick-alist-entry tildify-string-alist))
                     tildify-space-string))
          (end-marker (copy-marker end))
          answer
          bad-answer
          replace
          quit
          (message-log-max nil)
          (count 0))
      ;; For the time being, tildify-pattern-alist overwrites tildify-pattern
      (let ((alist (with-suppressed-warnings ((obsolete
                                               tildify--pick-alist-entry))
                     (tildify--pick-alist-entry tildify-pattern-alist))))
        (when alist
          (setq regexp (car alist) match-number (cadr alist))))
      (while (and (not quit)
		  (re-search-forward regexp (marker-position end-marker) t))
	(when (or (not ask)
		  (progn
		    (goto-char (match-beginning match-number))
		    (setq bad-answer t)
		    (while bad-answer
		      (setq bad-answer nil)
		      (message "Replace? (yn!q) ")
		      (setq answer (read-event)))
		    (cond
		     ((or (eq answer ?y) (eq answer ? ) (eq answer 'space))
		      (setq replace t))
		     ((eq answer ?n)
		      (setq replace nil))
		     ((eq answer ?!)
		      (setq replace t
			    ask nil))
		     ((eq answer ?q)
		      (setq replace nil
			    quit t))
		     (t
		      (message "Press y, n, !, or q.")
		      (setq bad-answer t)))
		    replace))
	  (replace-match tilde t t nil match-number)
	  (setq count (1+ count))))
      ;; Return value
      (cons count (cond (quit nil)
                        ((not ask) 'force)
                        (t t))))))


;;; *** Tildify Mode ***

(defcustom tildify-space-pattern "[,:;(][ \t]*[a]\\|\\<[AIKOSUVWZikosuvwz]"
  "Pattern specifying whether to insert a hard space at point.

If the pattern matches `looking-back', a hard space needs to be inserted instead
of a space at point.  The regexp is always case sensitive, regardless of the
current `case-fold-search' setting."
  :version "25.1"
  :type 'regexp)

(defcustom tildify-space-predicates '(tildify-space-region-predicate)
  "A list of predicate functions for `tildify-space' function."
  :version "25.1"
  :type '(repeat function))

(defcustom tildify-double-space-undos t
  "Weather `tildify-space' should undo hard space when space is typed again."
  :version "25.1"
  :type 'boolean)

;;;###autoload
(defun tildify-space ()
  "Convert space before point into a hard space if the context is right.

If
 * character before point is a space character,
 * character before that has \"w\" character syntax (i.e. it's a word
   constituent),
 * `tildify-space-pattern' matches when `looking-back' (no more than 10
   characters) from before the space character, and
 * all predicates in `tildify-space-predicates' return non-nil,
replace the space character with value of `tildify-space-string' and
return t.

Otherwise, if
 * `tildify-double-space-undos' variable is non-nil,
 * character before point is a space character, and
 * text before that is a hard space as defined by
   `tildify-space-string' variable,
remove the hard space and leave only the space character.

This function is meant to be used as a `post-self-insert-hook'."
  (interactive)
  (let* ((p (point)) (p-1 (1- p)) (n (- p (point-min)))
         (l (length tildify-space-string)) (l+1 (1+ l))
         case-fold-search)
    (when (and (> n 2) (eq (preceding-char) ?\s))
      (cond
       ((and (eq (char-syntax (char-before p-1)) ?w)
             (save-excursion
               (goto-char p-1)
               (looking-back tildify-space-pattern (max (point-min) (- p 10))))
             (run-hook-with-args-until-failure 'tildify-space-predicates))
        (delete-char -1)
        (insert tildify-space-string)
        t)
       ((and tildify-double-space-undos
             (> n l+1)
             (string-equal tildify-space-string
                           (buffer-substring (- p l+1) p-1)))
        (goto-char p-1)
        (delete-char (- l))
        (goto-char (1+ (point)))
        nil)))))

(defun tildify-space-region-predicate ()
  "Check whether character before point should be tildified.
Based on `tildify-foreach-region-function', check whether character before,
which is assumed to be a space character, should be replaced with a hard space."
  (catch 'found
    (tildify--foreach-region (lambda (_b _e) (throw 'found t)) (1- (point)) (point))))

;;;###autoload
(define-minor-mode tildify-mode
  "Adds electric behavior to space character.

When space is inserted into a buffer in a position where hard space is required
instead (determined by `tildify-space-pattern' and `tildify-space-predicates'),
that space character is replaced by a hard space specified by
`tildify-space-string'.  Converting of the space is done by `tildify-space'.

When `tildify-mode' is enabled, if `tildify-string-alist' specifies a hard space
representation for current major mode, the `tildify-space-string' buffer-local
variable will be set to the representation."
  :lighter " ~"
  (when tildify-mode
    (let ((space (with-suppressed-warnings ((obsolete
                                             tildify--pick-alist-entry))
                   (tildify--pick-alist-entry tildify-string-alist))))
      (if (not (string-equal " " (or space tildify-space-string)))
          (when space
            (setq tildify-space-string space))
        (message (eval-when-compile
                   (concat "Hard space is a single space character, tildify-"
                           "mode won't have any effect, disabling.")))
        (setq tildify-mode nil))))
  (if tildify-mode
      (add-hook 'post-self-insert-hook #'tildify-space nil t)
    (remove-hook 'post-self-insert-hook #'tildify-space t)))


(provide 'tildify)

;;; tildify.el ends here
