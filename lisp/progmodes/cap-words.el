;;; cap-words.el --- minor mode for motion in CapitalizedWordIdentifiers

;; Copyright (C) 2002  Free Software Foundation, Inc.

;; Author: Dave Love <fx@gnu.org>
;; Keywords: languages

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Provides Capitalized Words minor mode for word movement in
;; identifiers CapitalizedLikeThis.

;; Note that the same effect could be obtained by frobbing the
;; category of upper case characters to produce word boundaries, but
;; the necessary processing isn't done for ASCII characters.

;; Fixme: This doesn't work properly for mouse double clicks.

;;; Code:

(defun capitalized-next-word-boundary (pos limit)
  "Function for use in `next-word-boundary-function-table'.
Looks for word boundaries before capitals."
  (save-excursion
    (goto-char pos)
    (let (case-fold-search)
      (if (<= pos limit)
	  ;; Fixme: Are these regexps the best?
	  (or (and (re-search-forward "\\=.\\w*[[:upper:]]"
				      limit t)
		   (progn (backward-char)
			  t))
	      (re-search-forward "\\>" limit t))
	(or (re-search-backward "[[:upper:]]\\w*\\=" limit t)
	    (re-search-backward "\\<" limit t))))
    (point)))

(defconst capitalized-next-word-boundary-function-table
  (let ((tab (make-char-table nil)))
    (set-char-table-range tab t #'capitalized-next-word-boundary)
    tab)
  "Assigned to `next-word-boundary-function-table' in Capitalized Words mode.")

;;;###autoload
(define-minor-mode capitalized-words-mode
  "Toggle Capitalized- Words mode.

In this minor mode, a word boundary occurs immediately before an
uppercase letter in a symbol.  This is in addition to all the normal
boundaries given by the syntax and category tables.  There is no
restriction to ASCII.

E.g. the beginning of words in the following identifier are as marked:

  capitalizedWorDD
  ^          ^  ^^

Note that these word boundaries only apply for word motion and
marking commands such as \\[forward-word].  This mode does not affect word
boundaries in found by regexp matching (`\\>', `\\w' &c).

This style of identifiers is common in environments like Java ones,
where underscores aren't trendy enough.  Capitalization rules are
sometimes part of the language, e.g. Haskell, which may thus encourage
such a style.  It is appropriate to add `capitalized-words-mode' to
the mode hook for programming langauge modes in which you encounter
variables like this, e.g. `java-mode-hook'.  It's unlikely to cause
trouble if such identifiers aren't used.

See also `glasses-mode' and `studlify-word'.
Obsoletes `c-forward-into-nomenclature'."
  nil " Caps" nil :group 'programming
  (set (make-local-variable 'next-word-boundary-function-table)
       capitalized-next-word-boundary-function-table))

(provide 'cap-words)

;;; arch-tag: 46513b64-fe5a-4c0b-902c-ed235c22975f
;;; cap-words.el ends here
