;;; regexp-opt.el --- generate efficient regexps to match strings.

;; Copyright (C) 1994, 1995, 1996, 1997 Free Software Foundation, Inc.

;; Author: Simon Marshall <simon@gnu.ai.mit.edu>
;; Keywords: strings, regexps
;; Version: 1.04.01

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; The "opt" in "regexp-opt" stands for "optim\\(al\\|i\\(se\\|ze\\)\\)".
;;
;; This package generates a regexp from a given list of strings (that matches
;; one of those strings) that is equivalent to but more efficient than:
;;
;; (mapconcat 'identity (mapcar 'regexp-quote strings) "\\|")
;;
;; For example:
;;
;; (let ((strings '("cond" "if" "when" "unless" "while"
;; 		    "let" "let*" "progn" "prog1" "prog2"
;; 		    "save-restriction" "save-excursion" "save-window-excursion"
;; 		    "save-current-buffer" "save-match-data"
;; 		    "catch" "throw" "unwind-protect" "condition-case")))
;;   (concat "(" (regexp-opt strings t) "\\>"))
;;  => "(\\(c\\(atch\\|ond\\(ition-case\\)?\\)\\|if\\|let\\*?\\|prog[12n]\\|save-\\(current-buffer\\|excursion\\|match-data\\|restriction\\|window-excursion\\)\\|throw\\|un\\(less\\|wind-protect\\)\\|wh\\(en\\|ile\\)\\)\\>"
;;
;; Searching using the above example `regexp-opt' regexp is significantly
;; faster than searching using the equivalent `mapconcat' regexp, taking
;; approximately two-thirds of the time.
;;
;; Since this package was written to produce efficient regexps, not regexps
;; efficiently, it is probably not a good idea to in-line too many calls in
;; your code, unless you use the following trick with `eval-when-compile':
;;
;; (defvar definition-regexp
;;   (eval-when-compile
;;     (concat "^("
;;             (regexp-opt '("defun" "defsubst" "defmacro" "defalias"
;;                           "defvar" "defconst") t)
;;             "\\>")))
;;
;; The `byte-compile' code will be as if you had defined the variable thus:
;;
;; (defvar definition-regexp
;;   "^(\\(def\\(alias\\|const\\|macro\\|subst\\|un\\|var\\)\\)\\>")
;;
;; Originally written for font-lock.el, from an idea from Stig's hl319.el.
;; Please don't tell me that it doesn't produce optimal regexps; I know that
;; already.  For example, the above explanation for the meaning of "opt" would
;; be more efficient as "optim\\(al\\|i[sz]e\\)", but this requires complex
;; forward looking.  But (ideas or) code to improve things (are) is welcome.

;;; Code:

;;;###autoload
(defun regexp-opt (strings &optional paren)
  "Return a regexp to match a string in STRINGS.
If optional PAREN non-nil, ensure that the returned regexp is enclosed by at
least one regexp grouping construct.
Each string in STRINGS should be unique and should not contain any regexps.
The returned regexp is typically more efficient than the equivalent regexp:

 (mapconcat 'identity (mapcar 'regexp-quote STRINGS) \"\\\\|\")

but typically contains regexp grouping constructs.  Use `regexp-opt-depth' to
count them."
  (save-match-data
    ;; Recurse on the sorted list.
    (let ((max-lisp-eval-depth (* 1024 1024))
	  (completion-ignore-case nil))
      (regexp-opt-group (sort (copy-sequence strings) 'string-lessp) paren))))

;;;###autoload
(defun regexp-opt-depth (regexp)
  "Return the depth of REGEXP.
This means the number of regexp grouping constructs (parenthesised expressions)
in REGEXP."
  (save-match-data
    ;; Hack to signal an error if REGEXP does not have balanced parentheses.
    (string-match regexp "")
    ;; Count the number of open parentheses in REGEXP.
    (let ((count 0) start)
      (while (string-match "\\\\(" regexp start)
	(setq count (1+ count) start (match-end 0)))
      count)))

;;; Workhorse functions.

(eval-when-compile
  (require 'cl))

(unless (fboundp 'make-bool-vector)
  (defalias 'make-bool-vector 'make-vector))

(defun regexp-opt-group (strings &optional paren lax)
  ;;
  ;; Return a regexp to match a string in STRINGS.
  ;; If PAREN non-nil, output regexp parentheses around returned regexp.
  ;; If LAX non-nil, don't output parentheses if it doesn't require them.
  ;; Merges keywords to avoid backtracking in Emacs' regexp matcher.
  ;;
  ;; The basic idea is to find the shortest common prefix, remove it and
  ;; recurse.  If there is no prefix, we divide the list into two so that (at
  ;; least) one half will have at least a one-character common prefix.
  ;;
  ;; Also we delay the addition of grouping parenthesis as long as possible
  ;; until we're sure we need them, and try to remove one-character sequences
  ;; so we can use character sets rather than grouping parenthesis.
  ;;
  (let* ((open-group (if paren "\\(" ""))
	 (close-group (if paren "\\)" ""))
	 (open-charset (if lax "" open-group))
	 (close-charset (if lax "" close-group)))
    (cond
     ;;
     ;; If there is only one string, just return it.
     ((= (length strings) 1)
      (if (= (length (car strings)) 1)
	  (concat open-charset (regexp-quote (car strings)) close-charset)
	(concat open-group (regexp-quote (car strings)) close-group)))
     ;;
     ;; If there is an empty string, remove it and recurse on the rest.
     ((= (length (car strings)) 0)
      (concat open-charset
	      (regexp-opt-group (cdr strings) t t) "?"
	      close-charset))
     ;;
     ;; If all are one-character strings, just return a character set.
     ((= (length strings) (apply '+ (mapcar 'length strings)))
      (concat open-charset
	      (regexp-opt-charset strings)
	      close-charset))
     ;;
     ;; We have a list of different length strings.
     (t
      (let ((prefix (try-completion "" (mapcar 'list strings)))
	    (letters (let ((completion-regexp-list '("^.$")))
		       (all-completions "" (mapcar 'list strings)))))
	(cond
	 ;;
	 ;; If there is a common prefix, remove it and recurse on the suffixes.
	 ((> (length prefix) 0)
	  (let* ((length (length prefix))
		 (suffixes (mapcar (lambda (s) (substring s length)) strings)))
	    (concat open-group
		    (regexp-quote prefix) (regexp-opt-group suffixes t t)
		    close-group)))
	 ;;
	 ;; If there are several one-character strings, remove them and recurse
	 ;; on the rest.
	 ((> (length letters) 1)
	  (let ((rest (let ((completion-regexp-list '("^..+$")))
			(all-completions "" (mapcar 'list strings)))))
	    (concat open-group
		    (regexp-opt-charset letters) "\\|" (regexp-opt-group rest)
		    close-group)))
	 ;;
	 ;; Otherwise, divide the list into those that start with a particular
	 ;; letter and those that do not, and recurse on them.
	 (t
	  (let* ((char (substring (car strings) 0 1))
		 (half1 (all-completions char (mapcar 'list strings)))
		 (half2 (nthcdr (length half1) strings)))
	    (concat open-group
		    (regexp-opt-group half1) "\\|" (regexp-opt-group half2)
		    close-group)))))))))

(defun regexp-opt-charset (chars)
  ;;
  ;; Return a regexp to match a character in CHARS.
  ;;
  ;; The basic idea is to find character ranges.  Also we take care in the
  ;; position of character set meta characters in the character set regexp.
  ;;
  (let* ((charwidth 256)				; Yeah, right.
	 (charmap (make-bool-vector charwidth nil))
	 (charset "")
	 (bracket "") (dash "") (caret ""))
    ;;
    ;; Make a character map but extract character set meta characters.
    (let (char)
      (while chars
	(setq char (string-to-char (pop chars)))
	(cond ((eq char ?\])
	       (setq bracket "]"))
	      ((eq char ?^)
	       (setq caret "^"))
	      ((eq char ?-)
	       (setq dash "-"))
	      (t
	       (aset charmap char t)))))
    ;;
    ;; Make a character set from the map using ranges where applicable.
    (let ((elt 0) start)
      (while (< elt charwidth)
	(when (aref charmap elt)
	  (setq start (1+ elt))
	  (while (and (< start charwidth) (aref charmap start))
	    (incf start))
	  (if (< (- start elt) 4)
	      (setq charset (format "%s%c" charset elt))
	    (setq charset (format "%s%c-%c" charset elt (1- start))
		  elt start)))
	(incf elt)))
    ;;
    ;; Make sure a caret is not first and a dash is first or last.
    (if (and (string-equal charset "") (string-equal bracket ""))
	(concat "[" dash caret "]")
      (concat "[" bracket charset caret dash "]"))))

(provide 'regexp-opt)

;;; regexp-opt.el ends here
