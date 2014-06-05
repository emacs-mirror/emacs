;;; tildify.el --- adding hard spaces into texts

;; Copyright (C) 1997-2014 Free Software Foundation, Inc.

;; Author:     Milan Zamazal <pdm@zamazal.org>
;;             Michal Nazarewicz <mina86@mina86.com>
;; Version:    4.5.3
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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

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
  :group 'wp)

(defcustom tildify-pattern-alist
  '((t "\\([,:;(][ \t]*[a]\\|\\<[AIKOSUVZikosuvz]\\)\\([ \t]+\\|[ \t]*\n[ \t]*\\)\\(\\w\\|[([{\\]\\|<[a-zA-Z]\\)" 2))
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
  :group 'tildify
  :type '(repeat (cons :tag "Entry for major mode"
                       (choice (const  :tag "Default" t)
                               (symbol :tag "Major mode"))
                       (choice (list   :tag "Regexp"
                                       regexp
                                       (integer :tag "Group "))
                               (symbol :tag "Like other")))))

(defcustom tildify-string-alist
  '((latex-mode . "~")
    (tex-mode . latex-mode)
    (plain-tex-mode . latex-mode)
    (sgml-mode . "&nbsp;")
    (html-mode . sgml-mode)
    (xml-mode . "&#160;") ; XML does not define &nbsp; use numeric reference
    (nxml-mode . xml-mode)
    (t . " "))
  "Alist specifying what is a hard space in the current major mode.

Each alist item is of the form (MAJOR-MODE . STRING) or
\(MAJOR-MODE . SYMBOL).

MAJOR-MODE defines major mode, for which the item applies.  It can be either:
- a symbol equal to the major mode of the buffer to be fixed
- t for default item, this applies to all major modes not defined in another
  alist item

STRING defines the hard space, which is inserted at places defined by
`tildify-pattern-alist'.  For example it can be \"~\" for TeX or \"&nbsp;\"
for SGML.

The form (MAJOR-MODE . SYMBOL) defines alias item for MAJOR-MODE.  For this
mode, the item for the mode SYMBOL is looked up in the alist instead."
  :group 'tildify
  :type '(repeat (cons :tag "Entry for major mode"
                       (choice (const  :tag "Default" t)
                               (symbol :tag "Major mode"))
                       (choice (const  :tag "No-break space (U+00A0)" "\u00A0")
                               (string :tag "String    ")
                               (symbol :tag "Like other")))))

(defcustom tildify-ignored-environments-alist
  `((latex-mode
     ("\\\\\\\\" . "")		; do not remove this
     (,(eval-when-compile (concat
                           "\\\\begin{\\("
                           (regexp-opt '("verbatim" "math" "displaymath"
                                         "equation" "eqnarray" "eqnarray*"))
                           "\\)}"))
      . ("\\\\end{" 1 "}"))
     ("\\\\verb\\*?\\(.\\)" . (1))
     ("\\$\\$?" . (0))
     ("\\\\(" . "\\\\)")
     ("\\\\[[]" . "\\\\[]]")
     ("\\\\[a-zA-Z]+\\( +\\|{}\\)[a-zA-Z]*" . "")
     ("%" . "$"))
    (plain-tex-mode . latex-mode)
    (html-mode
     (,(eval-when-compile (concat
                           "<\\("
                           (regexp-opt '("pre" "dfn" "code" "samp" "kbd" "var"
                                         "PRE" "DFN" "CODE" "SAMP" "KBD" "VAR"))
                           "\\)\\>[^>]*>"))
      . ("</" 1 ">"))
     ("<! *--" . "-- *>")
     ("<" . ">"))
    (sgml-mode . html-mode)
    (xml-mode
     ("<! *--" . "-- *>")
     ("<" . ">"))
    (nxml-mode . xml-mode))
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

BEG-REGEX is a regexp matching beginning of a text part to be skipped.
END-REGEX defines end of the corresponding text part and can be either:
- a regexp matching the end of the skipped text part
- a list of regexps and numbers, which will compose the ending regexp by
  concatenating themselves, while replacing the numbers with corresponding
  subexpressions of BEG-REGEX (this is used to solve cases like
  \\\\verb<character> in TeX)."
  :group 'tildify
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


;;; *** Interactive functions ***

;;;###autoload
(defun tildify-region (beg end &optional dont-ask)
  "Add hard spaces in the region between BEG and END.
See variables `tildify-pattern-alist', `tildify-string-alist', and
`tildify-ignored-environments-alist' for information about configuration
parameters.
This function performs no refilling of the changed text.
If DONT-ASK is set, or called interactively with prefix argument, user
won't be prompted for confirmation of each substitution."
  (interactive "*rP")
  (let (case-fold-search (count 0) (ask (not dont-ask)))
    (tildify-foreach-region-outside-env beg end
      (lambda (beg end)
        (let ((aux (tildify-tildify beg end ask)))
          (setq count (+ count (car aux)))
          (if (not (eq (cdr aux) 'force))
              (cdr aux)
            (setq ask nil)
            t))))
    (message "%d spaces replaced." count)))

;;;###autoload
(defun tildify-buffer (&optional dont-ask)
  "Add hard spaces in the current buffer.
See variables `tildify-pattern-alist', `tildify-string-alist', and
`tildify-ignored-environments-alist' for information about configuration
parameters.
This function performs no refilling of the changed text.
If DONT-ASK is set, or called interactively with prefix argument, user
won't be prompted for confirmation of each substitution."
  (interactive  "*P")
  (tildify-region (point-min) (point-max) dont-ask))


;;; *** Auxiliary functions ***

(defun tildify-mode-alist (mode-alist &optional mode)
  "Return alist item for the MODE-ALIST in the current major MODE."
  (let ((alist (cdr (or (assoc (or mode major-mode) mode-alist)
			(assoc t mode-alist)))))
    (if (and alist
	     (symbolp alist))
	(tildify-mode-alist mode-alist alist)
      alist)))

(defun tildify-foreach-region-outside-env (beg end callback)
  "Scan region from BEG to END calling CALLBACK on portions out of environments.
Call CALLBACK on each region outside of environment to ignore.
CALLBACK will only be called for regions which have intersection
with [BEG END].  It must be a function that takes two point
arguments specifying the region to operate on.  Stop scanning the
region as soon as CALLBACK returns nil.  Environments to ignore
are determined from `tildify-ignored-environments-alist'."
  (declare (indent 2))
  (let ((pairs (tildify-mode-alist tildify-ignored-environments-alist)))
    (if (not pairs)
        (funcall callback beg end)
      (let ((func (lambda (b e)
                    (let ((b (max b beg)) (e (min e end)))
                    (if (< b e) (funcall callback b e) t))))
            (beg-re (concat "\\(?:"
                            (mapconcat 'car pairs "\\)\\|\\(?:")
                            "\\)"))
            p end-re)
        (save-excursion
          (save-restriction
            (widen)
            (goto-char (point-min))
            (while (and (< (setq p (point)) end)
                        (if (not (setq end-re
                                       (tildify-find-env beg-re pairs)))
                            (progn (funcall func p end) nil)
                          (funcall func p (match-beginning 0))
                          (when (< (point) end)
                            (setq p (point))
                            (re-search-forward end-re nil t)))))))))))

(defun tildify-find-env (regexp pairs)
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
(quit), force (replace without further questions)."
  (save-excursion
    (goto-char beg)
    (let* ((alist (tildify-mode-alist tildify-pattern-alist))
	   (regexp (car alist))
	   (match-number (cadr alist))
	   (tilde (tildify-mode-alist tildify-string-alist))
	   (end-marker (copy-marker end))
	   answer
	   bad-answer
	   replace
	   quit
	   (message-log-max nil)
	   (count 0))
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


;;; *** Announce ***

(provide 'tildify)


;; Local variables:
;; coding: utf-8
;; End:

;;; tildify.el ends here
