;;; rst.el --- Mode for viewing and editing reStructuredText-documents.

;; Copyright (C) 2003-2016 Free Software Foundation, Inc.

;; Maintainer: Stefan Merten <stefan at merten-home dot de>
;; Author: Stefan Merten <stefan at merten-home dot de>,
;;         Martin Blais <blais@furius.ca>,
;;         David Goodger <goodger@python.org>,
;;         Wei-Wei Guo <wwguocn@gmail.com>

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

;; This package provides major mode rst-mode, which supports documents marked
;; up using the reStructuredText format.  Support includes font locking as well
;; as a lot of convenience functions for editing.  It does this by defining a
;; Emacs major mode: rst-mode (ReST).  This mode is derived from text-mode.
;; This package also contains:
;;
;; - Functions to automatically adjust and cycle the section underline
;;   adornments;
;; - A mode that displays the table of contents and allows you to jump anywhere
;;   from it;
;; - Functions to insert and automatically update a TOC in your source
;;   document;
;; - Function to insert list, processing item bullets and enumerations
;;   automatically;
;; - Font-lock highlighting of most reStructuredText structures;
;; - Indentation and filling according to reStructuredText syntax;
;; - Cursor movement according to reStructuredText syntax;
;; - Some other convenience functions.
;;
;; See the accompanying document in the docutils documentation about
;; the contents of this package and how to use it.
;;
;; For more information about reStructuredText, see
;; http://docutils.sourceforge.net/rst.html
;;
;; For full details on how to use the contents of this file, see
;; http://docutils.sourceforge.net/docs/user/emacs.html
;;
;; There are a number of convenient key bindings provided by rst-mode.  For the
;; bindings, try C-c C-h when in rst-mode.  There are also many variables that
;; can be customized, look for defcustom in this file or look for the "rst"
;; customization group contained in the "wp" group.
;;
;; If you use the table-of-contents feature, you may want to add a hook to
;; update the TOC automatically every time you adjust a section title::
;;
;;   (add-hook 'rst-adjust-hook 'rst-toc-update)
;;
;; Syntax highlighting: font-lock is enabled by default.  If you want to turn
;; off syntax highlighting to rst-mode, you can use the following::
;;
;;   (setq font-lock-global-modes '(not rst-mode ...))
;;

;;; DOWNLOAD

;; The latest release of this file lies in the docutils source code repository:
;;   http://docutils.svn.sourceforge.net/svnroot/docutils/trunk/docutils/tools/editors/emacs/rst.el

;;; INSTALLATION

;; Add the following lines to your init file:
;;
;;   (require 'rst)
;;
;; If you are using `.txt' as a standard extension for reST files as
;; http://docutils.sourceforge.net/FAQ.html#what-s-the-standard-filename-extension-for-a-restructuredtext-file
;; suggests you may use one of the `Local Variables in Files' mechanism Emacs
;; provides to set the major mode automatically.  For instance you may use::
;;
;;    .. -*- mode: rst -*-
;;
;; in the very first line of your file.  The following code is useful if you
;; want automatically enter rst-mode from any file with compatible extensions:
;;
;; (setq auto-mode-alist
;;       (append '(("\\.txt\\'" . rst-mode)
;;                 ("\\.rst\\'" . rst-mode)
;;                 ("\\.rest\\'" . rst-mode)) auto-mode-alist))
;;

;;; Code:

;; FIXME: Check through major mode conventions again.

;; FIXME: Add proper ";;;###autoload" comments.

;; FIXME: When 24.1 is common place remove use of `lexical-let' and put "-*-
;;        lexical-binding: t -*-" in the first line.

;; FIXME: Embed complicated `defconst's in `eval-when-compile'.

;; FIXME: Use `testcover'. Mark up a function with sufficient test coverage by
;;        a comment tagged with `testcover' after the `defun'.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Support for `testcover'

(when (and (boundp 'testcover-1value-functions)
	   (boundp 'testcover-compose-functions))
  ;; Below `lambda' is used in a loop with varying parameters and is thus not
  ;; 1valued.
  (setq testcover-1value-functions
	(delq 'lambda testcover-1value-functions))
  (add-to-list 'testcover-compose-functions 'lambda))

(defun rst-testcover-defcustom ()
  "Remove all customized variables from `testcover-module-constants'.
This seems to be a bug in `testcover': `defcustom' variables are
considered constants.  Revert it with this function after each `defcustom'."
  (when (boundp 'testcover-module-constants)
    (setq testcover-module-constants
	  (delq nil
		(mapcar
		 (lambda (sym)
		   (if (not (plist-member (symbol-plist sym) 'standard-value))
		       sym))
		 testcover-module-constants)))))

(defun rst-testcover-add-compose (fun)
  "Add FUN to `testcover-compose-functions'."
  (when (boundp 'testcover-compose-functions)
    (add-to-list 'testcover-compose-functions fun)))

(defun rst-testcover-add-1value (fun)
  "Add FUN to `testcover-1value-functions'."
  (when (boundp 'testcover-1value-functions)
    (add-to-list 'testcover-1value-functions fun)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Common Lisp stuff

;; Only use of macros is allowed - may be replaced by `cl-lib' some time.
(eval-when-compile
  (require 'cl))

;; Redefine some functions from `cl.el' in a proper namespace until they may be
;; used from there.

(defun rst-signum (x)
  ;; testcover: ok.
  "Return 1 if X is positive, -1 if negative, 0 if zero."
  (cond
   ((> x 0) 1)
   ((< x 0) -1)
   (t 0)))

(defun rst-some (seq &optional pred)
  ;; testcover: ok.
  "Return non-nil if any element of SEQ yields non-nil when PRED is applied.
Apply PRED to each element of list SEQ until the first non-nil
result is yielded and return this result.  PRED defaults to
`identity'."
  (unless pred
    (setq pred 'identity))
  (catch 'rst-some
    (dolist (elem seq)
      (let ((r (funcall pred elem)))
	(when r
	  (throw 'rst-some r))))))

(defun rst-position-if (pred seq)
  ;; testcover: ok.
  "Return position of first element satisfying PRED in list SEQ or nil."
  (catch 'rst-position-if
    (let ((i 0))
      (dolist (elem seq)
	(when (funcall pred elem)
	  (throw 'rst-position-if i))
	(incf i)))))

(defun rst-position (elem seq)
  ;; testcover: ok.
  "Return position of ELEM in list SEQ or nil.
Comparison done with `equal'."
  ;; Create a closure containing `elem' so the `lambda' always sees our
  ;; parameter instead of an `elem' which may be in dynamic scope at the time
  ;; of execution of the `lambda'.
  (lexical-let ((elem elem))
    (rst-position-if (function (lambda (e)
				 (equal elem e)))
		     seq)))

(defun rst-member-if (pred seq)
  ;; testcover: ok.
  "Return sublist of SEQ starting with the element whose car satisfies PRED."
  (let (found)
    (while (and (not found) seq)
      (if (funcall pred (car seq))
	  (setq found seq)
	(setq seq (cdr seq))))
    found))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Versions

(defun rst-extract-version (delim-re head-re re tail-re var &optional default)
  ;; testcover: ok.
  "Extract the version from a variable according to the given regexes.
Return the version after regex DELIM-RE and HEAD-RE matching RE
and before TAIL-RE and DELIM-RE in VAR or DEFAULT for no match."
  (if (string-match
       (concat delim-re head-re "\\(" re "\\)" tail-re delim-re)
       var)
      (match-string 1 var)
    default))

;; Use CVSHeader to really get information from CVS and not other version
;; control systems.
(defconst rst-cvs-header
  "$CVSHeader: sm/rst_el/rst.el,v 1.600 2016/07/31 11:13:44 stefan Exp $")
(defconst rst-cvs-rev
  (rst-extract-version "\\$" "CVSHeader: \\S + " "[0-9]+\\(?:\\.[0-9]+\\)+"
		       " .*" rst-cvs-header "0.0")
  "The CVS revision of this file.  CVS revision is the development revision.")
(defconst rst-cvs-timestamp
  (rst-extract-version "\\$" "CVSHeader: \\S + \\S + "
		       "[0-9]+-[0-9]+-[0-9]+ [0-9]+:[0-9]+:[0-9]+" " .*"
		       rst-cvs-header "1970-01-01 00:00:00")
  "The CVS time stamp of this file.")

;; Use LastChanged... to really get information from SVN.
(defconst rst-svn-rev
  (rst-extract-version "\\$" "LastChangedRevision: " "[0-9]+" " "
		       "$LastChangedRevision: 7963 $")
  "The SVN revision of this file.
SVN revision is the upstream (docutils) revision.")
(defconst rst-svn-timestamp
  (rst-extract-version "\\$" "LastChangedDate: " ".+?+" " "
		       "$LastChangedDate: 2016-07-31 13:13:21 +0200 (Sun, 31 Jul 2016) $")
  "The SVN time stamp of this file.")

;; Maintained by the release process.
(defconst rst-official-version
  (rst-extract-version "%" "OfficialVersion: " "[0-9]+\\(?:\\.[0-9]+\\)+" " "
		       "%OfficialVersion: 1.5.0 %")
  "Official version of the package.")
(defconst rst-official-cvs-rev
  (rst-extract-version "[%$]" "Revision: " "[0-9]+\\(?:\\.[0-9]+\\)+" " "
		       "%Revision: 1.600 %")
  "CVS revision of this file in the official version.")

(defconst rst-version
  (if (equal rst-official-cvs-rev rst-cvs-rev)
      rst-official-version
    (format "%s (development %s [%s])" rst-official-version
	    rst-cvs-rev rst-cvs-timestamp))
  "The version string.
Starts with the current official version.  For developer versions
in parentheses follows the development revision and the time stamp.")

(defconst rst-package-emacs-version-alist
  '(("1.0.0" . "24.3")
    ("1.1.0" . "24.3")
    ("1.2.0" . "24.3")
    ("1.2.1" . "24.3")
    ("1.3.0" . "24.3")
    ("1.3.1" . "24.3")
    ("1.4.0" . "24.3")
    ("1.4.1" . "24.5")
    ("1.4.2" . "24.5")
    ("1.5.0" . "26.1")
    ))

(unless (assoc rst-official-version rst-package-emacs-version-alist)
  (error "Version %s not listed in `rst-package-emacs-version-alist'"
	 rst-version))

(add-to-list 'customize-package-emacs-version-alist
	     (cons 'ReST rst-package-emacs-version-alist))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize customization

(defgroup rst nil "Support for reStructuredText documents."
  :group 'text
  :version "23.1"
  :link '(url-link "http://docutils.sourceforge.net/rst.html"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Facilities for regular expressions used everywhere

;; The trailing numbers in the names give the number of referenceable regex
;; groups contained in the regex.

;; Used to be customizable but really is not customizable but fixed by the reST
;; syntax.
(defconst rst-bullets
  ;; Sorted so they can form a character class when concatenated.
  '(?- ?* ?+ ?• ?‣ ?⁃)
  "List of all possible bullet characters for bulleted lists.")

(defconst rst-uri-schemes
  '("acap" "cid" "data" "dav" "fax" "file" "ftp" "gopher" "http" "https" "imap"
    "ldap" "mailto" "mid" "modem" "news" "nfs" "nntp" "pop" "prospero" "rtsp"
    "service" "sip" "tel" "telnet" "tip" "urn" "vemmi" "wais")
  "Supported URI schemes.")

(defconst rst-adornment-chars
  ;; Sorted so they can form a character class when concatenated.
  '(?\]
    ?! ?\" ?# ?$ ?% ?& ?' ?\( ?\) ?* ?+ ?, ?. ?/ ?: ?\; ?< ?= ?> ?? ?@ ?\[ ?\\
    ?^ ?_ ?` ?{ ?| ?} ?~
    ?-)
  "Characters which may be used in adornments for sections and transitions.")

(defconst rst-max-inline-length
  1000
  "Maximum length of inline markup to recognize.")

(defconst rst-re-alist-def
  ;; `*-beg' matches * at the beginning of a line.
  ;; `*-end' matches * at the end of a line.
  ;; `*-prt' matches a part of *.
  ;; `*-tag' matches *.
  ;; `*-sta' matches the start of * which may be followed by respective content.
  ;; `*-pfx' matches the delimiter left of *.
  ;; `*-sfx' matches the delimiter right of *.
  ;; `*-hlp' helper for *.
  ;;
  ;; A trailing number says how many referenceable groups are contained.
  `(

    ;; Horizontal white space (`hws')
    (hws-prt "[\t ]")
    (hws-tag hws-prt "*") ; Optional sequence of horizontal white space.
    (hws-sta hws-prt "+") ; Mandatory sequence of horizontal white space.

    ;; Lines (`lin')
    (lin-beg "^" hws-tag) ; Beginning of a possibly indented line.
    (lin-end hws-tag "$") ; End of a line with optional trailing white space.
    (linemp-tag "^" hws-tag "$") ; Empty line with optional white space.

    ;; Various tags and parts
    (ell-tag "\\.\\.\\.") ; Ellipsis
    (bul-tag ,(concat "[" rst-bullets "]")) ; A bullet.
    (ltr-tag "[a-zA-Z]") ; A letter enumerator tag.
    (num-prt "[0-9]") ; A number enumerator part.
    (num-tag num-prt "+") ; A number enumerator tag.
    (rom-prt "[IVXLCDMivxlcdm]") ; A roman enumerator part.
    (rom-tag rom-prt "+") ; A roman enumerator tag.
    (aut-tag "#") ; An automatic enumerator tag.
    (dcl-tag "::") ; Double colon.

    ;; Block lead in (`bli')
    (bli-sfx (:alt hws-sta "$")) ; Suffix of a block lead-in with *optional*
				 ; immediate content.

    ;; Various starts
    (bul-sta bul-tag bli-sfx) ; Start of a bulleted item.

    ;; Explicit markup tag (`exm')
    (exm-tag "\\.\\.")
    (exm-sta exm-tag hws-sta)
    (exm-beg lin-beg exm-sta)

    ;; Counters in enumerations (`cnt')
    (cntany-tag (:alt ltr-tag num-tag rom-tag aut-tag)) ; An arbitrary counter.
    (cntexp-tag (:alt ltr-tag num-tag rom-tag)) ; An arbitrary explicit counter.

    ;; Enumerator (`enm')
    (enmany-tag (:alt
		 (:seq cntany-tag "\\.")
		 (:seq "(?" cntany-tag ")"))) ; An arbitrary enumerator.
    (enmexp-tag (:alt
		 (:seq cntexp-tag "\\.")
		 (:seq "(?" cntexp-tag ")"))) ; An arbitrary explicit
					      ; enumerator.
    (enmaut-tag (:alt
		 (:seq aut-tag "\\.")
		 (:seq "(?" aut-tag ")"))) ; An automatic enumerator.
    (enmany-sta enmany-tag bli-sfx) ; An arbitrary enumerator start.
    (enmexp-sta enmexp-tag bli-sfx) ; An arbitrary explicit enumerator start.
    (enmexp-beg lin-beg enmexp-sta) ; An arbitrary explicit enumerator start
				    ; at the beginning of a line.

    ;; Items may be enumerated or bulleted (`itm')
    (itmany-tag (:alt enmany-tag bul-tag)) ; An arbitrary item tag.
    (itmany-sta-1 (:grp itmany-tag) bli-sfx) ; An arbitrary item start, group
					     ; is the item tag.
    (itmany-beg-1 lin-beg itmany-sta-1) ; An arbitrary item start at the
				        ; beginning of a line, group is the
				        ; item tag.

    ;; Inline markup (`ilm')
    (ilm-pfx (:alt "^" hws-prt "[-'\"([{<‘“«’/:]"))
    (ilm-sfx (:alt "$" hws-prt "[]-'\")}>’”»/:.,;!?\\]"))

    ;; Inline markup content (`ilc')
    (ilcsgl-tag "\\S ") ; A single non-white character.
    (ilcast-prt (:alt "[^*\\]" "\\\\.")) ; Part of non-asterisk content.
    (ilcbkq-prt (:alt "[^`\\]" "\\\\.")) ; Part of non-backquote content.
    (ilcbkqdef-prt (:alt "[^`\\\n]" "\\\\.")) ; Part of non-backquote
					      ; definition.
    (ilcbar-prt (:alt "[^|\\]" "\\\\.")) ; Part of non-vertical-bar content.
    (ilcbardef-prt (:alt "[^|\\\n]" "\\\\.")) ; Part of non-vertical-bar
					      ; definition.
    (ilcast-sfx "[^\t *\\]") ; Suffix of non-asterisk content.
    (ilcbkq-sfx "[^\t `\\]") ; Suffix of non-backquote content.
    (ilcbar-sfx "[^\t |\\]") ; Suffix of non-vertical-bar content.
    (ilcrep-hlp ,(format "\\{0,%d\\}" rst-max-inline-length)) ; Repeat count.
    (ilcast-tag (:alt ilcsgl-tag
		      (:seq ilcsgl-tag
			    ilcast-prt ilcrep-hlp
			    ilcast-sfx))) ; Non-asterisk content.
    (ilcbkq-tag (:alt ilcsgl-tag
		      (:seq ilcsgl-tag
			    ilcbkq-prt ilcrep-hlp
			    ilcbkq-sfx))) ; Non-backquote content.
    (ilcbkqdef-tag (:alt ilcsgl-tag
			 (:seq ilcsgl-tag
			       ilcbkqdef-prt ilcrep-hlp
			       ilcbkq-sfx))) ; Non-backquote definition.
    (ilcbar-tag (:alt ilcsgl-tag
		      (:seq ilcsgl-tag
			    ilcbar-prt ilcrep-hlp
			    ilcbar-sfx))) ; Non-vertical-bar content.
    (ilcbardef-tag (:alt ilcsgl-tag
			 (:seq ilcsgl-tag
			       ilcbardef-prt ilcrep-hlp
			       ilcbar-sfx))) ; Non-vertical-bar definition.

    ;; Fields (`fld')
    (fldnam-prt (:alt "[^:\n]" "\\\\:")) ; Part of a field name.
    (fldnam-tag fldnam-prt "+") ; A field name.
    (fld-tag ":" fldnam-tag ":") ; A field marker.

    ;; Options (`opt')
    (optsta-tag (:alt "[-+/]" "--")) ; Start of an option.
    (optnam-tag "\\sw" (:alt "-" "\\sw") "*") ; Name of an option.
    (optarg-tag (:shy "[ =]\\S +")) ; Option argument.
    (optsep-tag (:shy "," hws-prt)) ; Separator between options.
    (opt-tag (:shy optsta-tag optnam-tag optarg-tag "?")) ; A complete option.

    ;; Footnotes and citations (`fnc')
    (fncnam-prt "[^]\n]") ; Part of a footnote or citation name.
    (fncnam-tag fncnam-prt "+") ; A footnote or citation name.
    (fnc-tag "\\[" fncnam-tag "]") ; A complete footnote or citation tag.
    (fncdef-tag-2 (:grp exm-sta)
		  (:grp fnc-tag)) ; A complete footnote or citation definition
				  ; tag.  First group is the explicit markup
				  ; start, second group is the footnote /
				  ; citation tag.
    (fnc-sta-2 fncdef-tag-2 bli-sfx) ; Start of a footnote or citation
				     ; definition.  First group is the explicit
				     ; markup start, second group is the
				     ; footnote / citation tag.

    ;; Substitutions (`sub')
    (sub-tag "|" ilcbar-tag "|") ; A complete substitution tag.
    (subdef-tag "|" ilcbardef-tag "|") ; A complete substitution definition
				       ; tag.

    ;; Symbol (`sym')
    (sym-prt "[-+.:_]") ; Non-word part of a symbol.
    (sym-tag (:shy "\\sw+" (:shy sym-prt "\\sw+") "*"))

    ;; URIs (`uri')
    (uri-tag (:alt ,@rst-uri-schemes))

    ;; Adornment (`ado')
    (ado-prt "[" ,(concat rst-adornment-chars) "]")
    (adorep3-hlp "\\{3,\\}") ; There must be at least 3 characters because
			     ; otherwise explicit markup start would be
			     ; recognized.
    (adorep2-hlp "\\{2,\\}") ; As `adorep3-hlp' but when the first of three
			     ; characters is matched differently.
    (ado-tag-1-1 (:grp ado-prt)
		 "\\1" adorep2-hlp) ; A complete adornment, group is the first
				    ; adornment character and MUST be the FIRST
				    ; group in the whole expression.
    (ado-tag-1-2 (:grp ado-prt)
		 "\\2" adorep2-hlp) ; A complete adornment, group is the first
				    ; adornment character and MUST be the
				    ; SECOND group in the whole expression.
    (ado-beg-2-1 "^" (:grp ado-tag-1-2)
		 lin-end) ; A complete adornment line; first group is the whole
			  ; adornment and MUST be the FIRST group in the whole
			  ; expression; second group is the first adornment
			  ; character.

    ;; Titles (`ttl')
    (ttl-tag "\\S *\\w.*\\S ") ; A title text.
    (ttl-beg-1 lin-beg (:grp ttl-tag)) ; A title text at the beginning of a
				       ; line.  First group is the complete,
				       ; trimmed title text.

    ;; Directives and substitution definitions (`dir')
    (dir-tag-3 (:grp exm-sta)
	       (:grp (:shy subdef-tag hws-sta) "?")
	       (:grp sym-tag dcl-tag)) ; A directive or substitution definition
				       ; tag.  First group is explicit markup
				       ; start, second group is a possibly
				       ; empty substitution tag, third group is
				       ; the directive tag including the double
				       ; colon.
    (dir-sta-3 dir-tag-3 bli-sfx) ; Start of a directive or substitution
				  ; definition.  Groups are as in dir-tag-3.

    ;; Literal block (`lit')
    (lit-sta-2 (:grp (:alt "[^.\n]" "\\.[^.\n]") ".*") "?"
	       (:grp dcl-tag) "$") ; Start of a literal block.  First group is
				   ; any text before the double colon tag which
				   ; may not exist, second group is the double
				   ; colon tag.

    ;; Comments (`cmt')
    (cmt-sta-1 (:grp exm-sta) "[^[|_\n]"
	       (:alt "[^:\n]" (:seq ":" (:alt "[^:\n]" "$")))
	       "*$") ; Start of a comment block; first group is explicit markup
		     ; start.

    ;; Paragraphs (`par')
    (par-tag- (:alt itmany-tag fld-tag opt-tag fncdef-tag-2 dir-tag-3 exm-tag)
	      ) ; Tag at the beginning of a paragraph; there may be groups in
		; certain cases.
    )
  "Definition alist of relevant regexes.
Each entry consists of the symbol naming the regex and an
argument list for `rst-re'.")

(defvar rst-re-alist) ; Forward declare to use it in `rst-re'.

;; FIXME: Use `sregex' or `rx' instead of re-inventing the wheel.
(rst-testcover-add-compose 'rst-re)
(defun rst-re (&rest args)
  ;; testcover: ok.
  "Interpret ARGS as regular expressions and return a regex string.
Each element of ARGS may be one of the following:

A string which is inserted unchanged.

A character which is resolved to a quoted regex.

A symbol which is resolved to a string using `rst-re-alist-def'.

A list with a keyword in the car.  Each element of the cdr of such
a list is recursively interpreted as ARGS.  The results of this
interpretation are concatenated according to the keyword.

For the keyword `:seq' the results are simply concatenated.

For the keyword `:shy' the results are concatenated and
surrounded by a shy-group (\"\\(?:...\\)\").

For the keyword `:alt' the results form an alternative (\"\\|\")
which is shy-grouped (\"\\(?:...\\)\").

For the keyword `:grp' the results are concatenated and form a
referenceable group (\"\\(...\\)\").

After interpretation of ARGS the results are concatenated as for
`:seq'."
  (apply 'concat
	 (mapcar
	  (lambda (re)
	    (cond
	     ((stringp re)
	      re)
	     ((symbolp re)
	      (cadr (assoc re rst-re-alist)))
	     ((characterp re)
	      (regexp-quote (char-to-string re)))
	     ((listp re)
	      (let ((nested
		     (mapcar (lambda (elt)
			       (rst-re elt))
			     (cdr re))))
		(cond
		 ((eq (car re) :seq)
		  (mapconcat 'identity nested ""))
		 ((eq (car re) :shy)
		  (concat "\\(?:" (mapconcat 'identity nested "") "\\)"))
		 ((eq (car re) :grp)
		  (concat "\\(" (mapconcat 'identity nested "") "\\)"))
		 ((eq (car re) :alt)
		  (concat "\\(?:" (mapconcat 'identity nested "\\|") "\\)"))
		 (t
		  (error "Unknown list car: %s" (car re))))))
	     (t
	      (error "Unknown object type for building regex: %s" re))))
	  args)))

;; FIXME: Remove circular dependency between `rst-re' and `rst-re-alist'.
(with-no-warnings ; Silence byte-compiler about this construction.
  (defconst rst-re-alist
    ;; Shadow global value we are just defining so we can construct it step by
    ;; step.
    (let (rst-re-alist)
      (dolist (re rst-re-alist-def rst-re-alist)
	(setq rst-re-alist
	      (nconc rst-re-alist
		     (list (list (car re) (apply 'rst-re (cdr re))))))))
    "Alist mapping symbols from `rst-re-alist-def' to regex strings."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Concepts

;; Each of the following classes represents an own concept. The suffix of the
;; class name is used in the code to represent entities of the respective
;; class.
;;
;; In addition a reStructuredText section header in the buffer is called
;; "section".
;;
;; For lists a "s" is added to the name of the concepts.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Class rst-Ado

(defstruct
  (rst-Ado
   (:constructor nil) ;; Prevent creating unchecked values.
   ;; Construct a transition.
   (:constructor
    rst-Ado-new-transition
    (&aux
     (char nil)
     (-style 'transition)))
   ;; Construct a simple section header.
   (:constructor
    rst-Ado-new-simple
    (char-arg
     &aux
     (char (rst-Ado--validate-char char-arg))
     (-style 'simple)))
   ;; Construct a over-and-under section header.
   (:constructor
    rst-Ado-new-over-and-under
    (char-arg
     &aux
     (char (rst-Ado--validate-char char-arg))
     (-style 'over-and-under)))
   ;; Construct from adornment with inverted style.
   (:constructor
    rst-Ado-new-invert
    (ado-arg
     &aux
     (char (rst-Ado-char ado-arg))
     (-style (let ((sty (rst-Ado--style ado-arg)))
	      (cond
	       ((eq sty 'simple)
		'over-and-under)
	       ((eq sty 'over-and-under)
		'simple)
	       (sty)))))))
  "Representation of a reStructuredText adornment.
Adornments are either section markers where they markup the
section header or transitions.

This type is immutable."
  ;; The character used for the adornment.
  (char nil :read-only t)
  ;; The style of the adornment. This is a private attribute.
  (-style nil :read-only t))

;; Private class methods

(defun rst-Ado--validate-char (char)
  ;; testcover: ok.
  "Validate CHAR to be a valid adornment character.
Return CHAR if so or signal an error otherwise."
  (cond
   ((not (characterp char))
    (signal 'wrong-type-argument (list 'characterp char)))
   ((memq char rst-adornment-chars)
    char)
   (t
    (signal 'args-out-of-range
	    (list (format
		   "Character must be a valid adornment character, not '%s'"
		   char))))))

;; Public methods

(defun rst-Ado-is-transition (self)
  ;; testcover: ok.
  "Return non-nil if SELF is a transition adornment."
  (unless (rst-Ado-p self)
    (signal 'wrong-type-argument
	    (list 'rst-Ado-p self)))
  (eq (rst-Ado--style self) 'transition))

(defun rst-Ado-is-section (self)
  ;; testcover: ok.
  "Return non-nil if SELF is a section adornment."
  (unless (rst-Ado-p self)
    (signal 'wrong-type-argument
	    (list 'rst-Ado-p self)))
  (not (rst-Ado-is-transition self)))

(defun rst-Ado-is-simple (self)
  ;; testcover: ok.
  "Return non-nil if SELF is a simple section adornment."
  (unless (rst-Ado-p self)
    (signal 'wrong-type-argument
	    (list 'rst-Ado-p self)))
  (eq (rst-Ado--style self) 'simple))

(defun rst-Ado-is-over-and-under (self)
  ;; testcover: ok.
  "Return non-nil if SELF is a over-and-under section adornment."
  (unless (rst-Ado-p self)
    (signal 'wrong-type-argument
	    (list 'rst-Ado-p self)))
  (eq (rst-Ado--style self) 'over-and-under))

(defun rst-Ado-equal (self other)
  ;; testcover: ok.
  "Return non-nil when SELF and OTHER are equal."
  (cond
   ((not (rst-Ado-p self))
    (signal 'wrong-type-argument
	    (list 'rst-Ado-p self)))
   ((not (rst-Ado-p other))
    (signal 'wrong-type-argument
	    (list 'rst-Ado-p other)))
   ((not (eq (rst-Ado--style self) (rst-Ado--style other)))
    nil)
   ((rst-Ado-is-transition self))
   ((equal (rst-Ado-char self) (rst-Ado-char other)))))

(defun rst-Ado-position (self ados)
  ;; testcover: ok.
  "Return position of of SELF in ADOS or nil."
  (unless (rst-Ado-p self)
    (signal 'wrong-type-argument
	    (list 'rst-Ado-p self)))
  (lexical-let ((ado self)) ;; Create closure.
    (rst-position-if (function (lambda (e)
				 (rst-Ado-equal ado e)))
		     ados)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Class rst-Hdr

(defstruct
  (rst-Hdr
   (:constructor nil) ;; Prevent creating unchecked values.
   ;; Construct while all parameters must be valid.
   (:constructor
    rst-Hdr-new
    (ado-arg
     indent-arg
     &aux
     (ado (rst-Hdr--validate-ado ado-arg))
     (indent (rst-Hdr--validate-indent indent-arg ado nil))))
   ;; Construct while all parameters but `indent' must be valid.
   (:constructor
    rst-Hdr-new-lax
    (ado-arg
     indent-arg
     &aux
     (ado (rst-Hdr--validate-ado ado-arg))
     (indent (rst-Hdr--validate-indent indent-arg ado t))))
   ;; Construct a header with same characteristics but opposite style as `ado'.
   (:constructor
    rst-Hdr-new-invert
    (ado-arg
     indent-arg
     &aux
     (ado (rst-Hdr--validate-ado (rst-Ado-new-invert ado-arg)))
     (indent (rst-Hdr--validate-indent indent-arg ado t))))
   (:copier rst-Hdr-copy)) ;; Not really needed for an immutable type.
  "Representation of reStructuredText section header characteristics.

This type is immutable."
  ;; The adornment of the header.
  (ado nil :read-only t)
  ;; The indentation of a title text or nil if not given.
  (indent nil :read-only t))

;; Private class methods

(defun rst-Hdr--validate-indent (indent ado lax)
  ;; testcover: ok.
  "Validate INDENT to be a valid indentation for ADO.
Return INDENT if so or signal an error otherwise.  If LAX don't
signal an error and return a valid indent."
  (cond
   ((not (integerp indent))
    (signal 'wrong-type-argument
	    (list 'integerp 'null indent)))
   ((zerop indent)
    indent)
   ((rst-Ado-is-simple ado)
    (if lax
	0
      (signal 'args-out-of-range
	      '("Indentation must be 0 for style simple"))))
   ((< indent 0)
    (if lax
	0
      (signal 'args-out-of-range
	      '("Indentation must not be negative"))))
   (indent))) ;; Implicitly over-and-under.

(defun rst-Hdr--validate-ado (ado)
  ;; testcover: ok.
  "Validate ADO to be a valid adornment.
Return ADO if so or signal an error otherwise."
  (cond
   ((not (rst-Ado-p ado))
    (signal 'wrong-type-argument
	    (list 'rst-Ado-p ado)))
   ((rst-Ado-is-transition ado)
    (signal 'args-out-of-range
	    '("Adornment for header must not be transition.")))
   (t
    ado)))

;; Public class methods

(defun rst-Hdr-preferred-adornments ()
  ;; testcover: ok.
  "Return preferred adornments as list of `rst-Hdr'."
  (mapcar (lambda (el)
	    (rst-Hdr-new-lax
	     (if (eq (cadr el) 'over-and-under)
		 (rst-Ado-new-over-and-under (car el))
	       (rst-Ado-new-simple (car el)))
	     (caddr el)))
	  rst-preferred-adornments))

;; Public methods

(defun rst-Hdr-member-ado (self hdrs)
  ;; testcover: ok.
  "Return sublist of HDRS whose car's adornment equals that of SELF or nil."
  (unless (rst-Hdr-p self)
    (signal 'wrong-type-argument
	    (list 'rst-Hdr-p self)))
  (let ((pos (rst-Ado-position (rst-Hdr-ado self) (rst-Hdr-ado-map hdrs))))
    (and pos (nthcdr pos hdrs))))

(defun rst-Hdr-ado-map (selves)
  ;; testcover: ok.
  "Return `rst-Ado' list extracted from elements of SELVES."
  (mapcar 'rst-Hdr-ado selves))

(defun rst-Hdr-get-char (self)
  ;; testcover: ok.
  "Return character of the adornment of SELF."
  (unless (rst-Hdr-p self)
    (signal 'wrong-type-argument
	    (list 'rst-Hdr-p self)))
  (rst-Ado-char (rst-Hdr-ado self)))

(defun rst-Hdr-is-over-and-under (self)
  ;; testcover: ok.
  "Return non-nil if SELF is a over-and-under section header."
  (unless (rst-Hdr-p self)
    (signal 'wrong-type-argument
	    (list 'rst-Hdr-p self)))
  (rst-Ado-is-over-and-under (rst-Hdr-ado self)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Class rst-Ttl

(defstruct
  (rst-Ttl
   (:constructor nil) ;; Prevent creating unchecked values.
   ;; Construct with valid parameters for all attributes.
   (:constructor
    rst-Ttl-new
    (ado-arg
     match-arg
     indent-arg
     text-arg
     &optional
     hdr-arg
     level-arg
     &aux
     (ado (rst-Ttl--validate-ado ado-arg))
     (match (rst-Ttl--validate-match match-arg ado))
     (indent (rst-Ttl--validate-indent indent-arg ado))
     (text (rst-Ttl--validate-text text-arg ado))
     (hdr (and hdr-arg (rst-Ttl--validate-hdr hdr-arg ado indent)))
     (level (and level-arg (rst-Ttl--validate-level level-arg)))))
   (:copier rst-Ttl-copy))
  "Representation of a reStructuredText section header as found in the buffer.
This type gathers information about an adorned part in the
buffer. Thus only the basic attributes are immutable. Although
the remaining attributes are `setf'-able the respective setters
should be used."
  ;; The adornment characteristics or nil for a title candidate.
  (ado nil :read-only t)
  ;; The match-data for `ado' as returned by `match-data'. Match group 0
  ;; matches the whole construct.  Match group 1 matches the overline adornment
  ;; if present.  Match group 2 matches the section title text or the
  ;; transition.  Match group 3 matches the underline adornment.
  (match nil :read-only t)
  ;; An indentation found for the title line or nil for a transition.
  (indent nil :read-only t)
  ;; The text of the title or nil for a transition.
  (text nil :read-only t)
  ;; The header characteristics if it is a valid section header.
  (hdr nil)
  ;; The hierarchical level of the section header starting with 0.
  (level nil))

;; Private class methods

(defun rst-Ttl--validate-ado (ado)
  ;; testcover: ok.
  "Return valid ADO or signal error."
  (unless (or (null ado) (rst-Ado-p ado))
    (signal 'wrong-type-argument
	    (list 'null 'rst-Ado-p ado)))
  ado)

(defun rst-Ttl--validate-match (match ado)
  ;; testcover: ok.
  "Return valid MATCH matching ADO or signal error."
  (unless (listp match)
    (signal 'wrong-type-argument
	    (list 'listp match)))
  (unless (equal (length match) 8)
    (signal 'args-out-of-range
	    '("Match data must consist of exactly 8 buffer positions.")))
  (mapcar (lambda (pos)
	    (unless (or (null pos) (integer-or-marker-p pos))
	      (signal 'wrong-type-argument
		      (list 'integer-or-marker-p 'null pos))))
	  match)
  (unless (and (integer-or-marker-p (nth 0 match))
	       (integer-or-marker-p (nth 1 match)))
    (signal 'args-out-of-range
	    '("First two elements of match data must be buffer positions.")))
  (cond
   ((null ado)
    (unless (and (null (nth 2 match))
		 (null (nth 3 match))
		 (integer-or-marker-p (nth 4 match))
		 (integer-or-marker-p (nth 5 match))
		 (null (nth 6 match))
		 (null (nth 7 match)))
      (signal 'args-out-of-range
	      '("For a title candidate exactly the third match pair must be set."))))
   ((rst-Ado-is-transition ado)
    (unless (and (null (nth 2 match))
		 (null (nth 3 match))
		 (integer-or-marker-p (nth 4 match))
		 (integer-or-marker-p (nth 5 match))
		 (null (nth 6 match))
		 (null (nth 7 match)))
      (signal 'args-out-of-range
	      '("For a transition exactly the third match pair must be set."))))
   ((rst-Ado-is-simple ado)
    (unless (and (null (nth 2 match))
		 (null (nth 3 match))
		 (integer-or-marker-p (nth 4 match))
		 (integer-or-marker-p (nth 5 match))
		 (integer-or-marker-p (nth 6 match))
		 (integer-or-marker-p (nth 7 match)))
    (signal 'args-out-of-range
	    '("For a simple section adornment exactly the third and fourth match pair must be set."))))
   (t ;; over-and-under
    (unless (and (integer-or-marker-p (nth 2 match))
		 (integer-or-marker-p (nth 3 match))
		 (integer-or-marker-p (nth 4 match))
		 (integer-or-marker-p (nth 5 match))
		 (or (null (nth 6 match)) (integer-or-marker-p (nth 6 match)))
		 (or (null (nth 7 match)) (integer-or-marker-p (nth 7 match))))
    (signal 'args-out-of-range
	    '("For a over-and-under section adornment all match pairs must be set.")))))
  match)

(defun rst-Ttl--validate-indent (indent ado)
  ;; testcover: ok.
  "Return valid INDENT for ADO or signal error."
  (if (and ado (rst-Ado-is-transition ado))
      (unless (null indent)
	(signal 'args-out-of-range
		'("Indent for a transition must be nil.")))
    (unless (integerp indent)
      (signal 'wrong-type-argument
	      (list 'integerp indent)))
    (unless (>= indent 0)
      (signal 'args-out-of-range
	      '("Indent for a section header must be non-negative."))))
  indent)

(defun rst-Ttl--validate-hdr (hdr ado indent)
  ;; testcover: ok.
  "Return valid HDR in relation to ADO and INDENT or signal error."
  (unless (rst-Hdr-p hdr)
    (signal 'wrong-type-argument
	    (list 'rst-Hdr-p hdr)))
  (unless (rst-Ado-equal (rst-Hdr-ado hdr) ado)
    (signal 'args-out-of-range
	    '("Basic adornment and adornment in header must match.")))
  (unless (equal (rst-Hdr-indent hdr) indent)
    (signal 'args-out-of-range
	    '("Basic indent and indent in header must match.")))
  hdr)

(defun rst-Ttl--validate-text (text ado)
  ;; testcover: ok.
  "Return valid TEXT for ADO or signal error."
  (if (and ado (rst-Ado-is-transition ado))
      (unless (null text)
	(signal 'args-out-of-range
		'("Transitions may not have title text.")))
    (unless (stringp text)
      (signal 'wrong-type-argument
	      (list 'stringp text))))
  text)

(defun rst-Ttl--validate-level (level)
  ;; testcover: ok.
  "Return valid LEVEL or signal error."
  (unless (integerp level)
    (signal 'wrong-type-argument
	    (list 'integerp level)))
  (unless (>= level 0)
    (signal 'args-out-of-range
	    '("Level must be non-negative.")))
  level)

;; Public methods

(defun rst-Ttl-evaluate-hdr (self)
  ;; testcover: ok.
  "Check for `ado' and `indent' in SELF forming a valid `rst-Hdr'.
Set and return it or nil if no valid `rst-Hdr' can be formed."
  (setf (rst-Ttl-hdr self)
	(condition-case nil
	    (rst-Hdr-new (rst-Ttl-ado self) (rst-Ttl-indent self))
	  (error nil))))

(defun rst-Ttl-set-level (self level)
  ;; testcover: ok.
  "In SELF set and return LEVEL or nil if invalid."
  (setf (rst-Ttl-level self)
	(rst-Ttl--validate-level level)))

(defun rst-Ttl-get-title-beginning (self)
  ;; testcover: ok.
  "Return position of beginning of title text of SELF.
This position should always be at the start of a line."
  (nth 4 (rst-Ttl-match self)))

(defun rst-Ttl-get-beginning (self)
  ;; testcover: ok.
  "Return position of beginning of whole SELF."
  (nth 0 (rst-Ttl-match self)))

(defun rst-Ttl-get-end (self)
  ;; testcover: ok.
  "Return position of end of whole SELF."
  (nth 1 (rst-Ttl-match self)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Class rst-Stn

(defstruct
  (rst-Stn
   (:constructor nil) ;; Prevent creating unchecked values.
   ;; Construct while all parameters must be valid.
   (:constructor
    rst-Stn-new
    (ttl-arg
     level-arg
     children-arg
     &aux
     (ttl (rst-Stn--validate-ttl ttl-arg))
     (level (rst-Stn--validate-level level-arg ttl))
     (children (rst-Stn--validate-children children-arg ttl)))))
  "Representation of a section tree node.

This type is immutable."
  ;; The title of the node or nil for a missing node.
  (ttl nil :read-only t)
  ;; The level of the node in the tree. Negative for the (virtual) top level
  ;; node.
  (level nil :read-only t)
  ;; The list of children of the node.
  (children nil :read-only t))

;; Private class methods

(defun rst-Stn--validate-ttl (ttl)
  ;; testcover: ok.
  "Return valid TTL or signal error."
  (unless (or (null ttl) (rst-Ttl-p ttl))
    (signal 'wrong-type-argument
	    (list 'null 'rst-Ttl-p ttl)))
  ttl)

(defun rst-Stn--validate-level (level ttl)
  ;; testcover: ok.
  "Return valid LEVEL for TTL or signal error."
  (unless (integerp level)
    (signal 'wrong-type-argument
	    (list 'integerp level)))
  (when ttl
    (unless (or (not (rst-Ttl-level ttl))
		(equal (rst-Ttl-level ttl) level))
      (signal 'args-out-of-range
	      '("A title must have correct level or none at all.")))
    (when (< level 0)
      ;; testcover: Never reached because a title may not have a negative level
      (signal 'args-out-of-range
	      '("Top level node must not have a title."))))
  level)

(defun rst-Stn--validate-children (children ttl)
  ;; testcover: ok.
  "Return valid CHILDREN for TTL or signal error."
  (unless (listp children)
    (signal 'wrong-type-argument
	    (list 'listp children)))
  (mapcar (lambda (child)
	    (unless (rst-Stn-p child)
	      (signal 'wrong-type-argument
		      (list 'rst-Stn-p child))))
	  children)
  (unless (or ttl children)
    (signal 'args-out-of-range
	    '("A missing node must have children.")))
  children)

;; Public methods

(defun rst-Stn-get-title-beginning (self)
  ;; testcover: ok.
  "Return the beginning of the title of SELF.
Handles missing node properly."
  (unless (rst-Stn-p self)
    (signal 'wrong-type-argument
	    (list 'rst-Stn-p self)))
  (let ((ttl (rst-Stn-ttl self)))
    (if ttl
	(rst-Ttl-get-title-beginning ttl)
      (rst-Stn-get-title-beginning (car (rst-Stn-children self))))))

(defun rst-Stn-get-text (self &optional default)
  ;; testcover: ok.
  "Return title text of SELF or DEFAULT if SELF is a missing node.
For a missing node and no DEFAULT given return a standard title text."
  (unless (rst-Stn-p self)
    (signal 'wrong-type-argument
	    (list 'rst-Stn-p self)))
  (let ((ttl (rst-Stn-ttl self)))
    (cond
     (ttl
      (rst-Ttl-text ttl))
     (default)
     ("[missing node]"))))

(defun rst-Stn-is-top (self)
  ;; testcover: ok.
  "Return non-nil if SELF is a top level node."
  (unless (rst-Stn-p self)
    (signal 'wrong-type-argument
	    (list 'rst-Stn-p self)))
  (< (rst-Stn-level self) 0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode definition

(defun rst-define-key (keymap key def &rest deprecated)
  ;; testcover: ok.
  "Bind like `define-key' but add deprecated key definitions.
KEYMAP, KEY, and DEF are as in `define-key'.  DEPRECATED key
definitions should be in vector notation.  These are defined
as well but give an additional message."
  (define-key keymap key def)
  (when deprecated
    (let* ((command-name (symbol-name def))
           (forwarder-function-name
            (if (string-match "^rst-\\(.*\\)$" command-name)
                (concat "rst-deprecated-"
                        (match-string 1 command-name))
              (error "Not an RST command: %s" command-name)))
           (forwarder-function (intern forwarder-function-name)))
      (unless (fboundp forwarder-function)
        (defalias forwarder-function
          (lexical-let ((key key) (def def))
            (lambda ()
              (interactive)
              (call-interactively def)
              (message "[Deprecated use of key %s; use key %s instead]"
          (key-description (this-command-keys))
          (key-description key))))
          (format "Deprecated binding for %s, use \\[%s] instead."
                  def def)))
      (dolist (dep-key deprecated)
        (define-key keymap dep-key forwarder-function)))))

 ;; Key bindings.
(defvar rst-mode-map
  (let ((map (make-sparse-keymap)))

    ;; \C-c is the general keymap.
    (rst-define-key map [?\C-c ?\C-h] 'describe-prefix-bindings)

    ;;
    ;; Section Adornments
    ;;
    ;; The adjustment function that adorns or rotates a section title.
    (rst-define-key map [?\C-c ?\C-=] 'rst-adjust [?\C-c ?\C-a t])
    (rst-define-key map [?\C-=] 'rst-adjust) ; Does not work on macOS and
					     ; on consoles.

    ;; \C-c \C-a is the keymap for adornments.
    (rst-define-key map [?\C-c ?\C-a ?\C-h] 'describe-prefix-bindings)
    ;; Another binding which works with all types of input.
    (rst-define-key map [?\C-c ?\C-a ?\C-a] 'rst-adjust)
    ;; Display the hierarchy of adornments implied by the current document
    ;; contents.
    (rst-define-key map [?\C-c ?\C-a ?\C-d] 'rst-display-hdr-hierarchy)
    ;; Homogenize the adornments in the document.
    (rst-define-key map [?\C-c ?\C-a ?\C-s] 'rst-straighten-sections
		    [?\C-c ?\C-s])

    ;;
    ;; Section Movement and Selection
    ;;
    ;; Mark the subsection where the cursor is.
    (rst-define-key map [?\C-\M-h] 'rst-mark-section
		    ;; Same as mark-defun sgml-mark-current-element.
		    [?\C-c ?\C-m])
    ;; Move backward/forward between section titles.
    ;; FIXME: Also bind similar to outline mode.
    (rst-define-key map [?\C-\M-a] 'rst-backward-section
		    ;; Same as beginning-of-defun.
		    [?\C-c ?\C-n])
    (rst-define-key map [?\C-\M-e] 'rst-forward-section
		    ;; Same as end-of-defun.
		    [?\C-c ?\C-p])

    ;;
    ;; Operating on regions
    ;;
    ;; \C-c \C-r is the keymap for regions.
    (rst-define-key map [?\C-c ?\C-r ?\C-h] 'describe-prefix-bindings)
    ;; Makes region a line-block.
    (rst-define-key map [?\C-c ?\C-r ?\C-l] 'rst-line-block-region
		    [?\C-c ?\C-d])
    ;; Shift region left or right according to tabs.
    (rst-define-key map [?\C-c ?\C-r tab] 'rst-shift-region
		    [?\C-c ?\C-r t] [?\C-c ?\C-l t])

    ;;
    ;; Operating on lists
    ;;
    ;; \C-c \C-l is the keymap for lists.
    (rst-define-key map [?\C-c ?\C-l ?\C-h] 'describe-prefix-bindings)
    ;; Makes paragraphs in region as a bullet list.
    (rst-define-key map [?\C-c ?\C-l ?\C-b] 'rst-bullet-list-region
		    [?\C-c ?\C-b])
    ;; Makes paragraphs in region as a enumeration.
    (rst-define-key map [?\C-c ?\C-l ?\C-e] 'rst-enumerate-region
		    [?\C-c ?\C-e])
    ;; Converts bullets to an enumeration.
    (rst-define-key map [?\C-c ?\C-l ?\C-c] 'rst-convert-bullets-to-enumeration
		    [?\C-c ?\C-v])
    ;; Make sure that all the bullets in the region are consistent.
    (rst-define-key map [?\C-c ?\C-l ?\C-s] 'rst-straighten-bullets-region
		    [?\C-c ?\C-w])
    ;; Insert a list item.
    (rst-define-key map [?\C-c ?\C-l ?\C-i] 'rst-insert-list)

    ;;
    ;; Table-of-Contents Features
    ;;
    ;; \C-c \C-t is the keymap for table of contents.
    (rst-define-key map [?\C-c ?\C-t ?\C-h] 'describe-prefix-bindings)
    ;; Enter a TOC buffer to view and move to a specific section.
    (rst-define-key map [?\C-c ?\C-t ?\C-t] 'rst-toc)
    ;; Insert a TOC here.
    (rst-define-key map [?\C-c ?\C-t ?\C-i] 'rst-toc-insert
		    [?\C-c ?\C-i])
    ;; Update the document's TOC (without changing the cursor position).
    (rst-define-key map [?\C-c ?\C-t ?\C-u] 'rst-toc-update
		    [?\C-c ?\C-u])
    ;; Go to the section under the cursor (cursor must be in TOC).
    (rst-define-key map [?\C-c ?\C-t ?\C-j] 'rst-goto-section
		    [?\C-c ?\C-f])

    ;;
    ;; Converting Documents from Emacs
    ;;
    ;; \C-c \C-c is the keymap for compilation.
    (rst-define-key map [?\C-c ?\C-c ?\C-h] 'describe-prefix-bindings)
    ;; Run one of two pre-configured toolset commands on the document.
    (rst-define-key map [?\C-c ?\C-c ?\C-c] 'rst-compile
		    [?\C-c ?1])
    (rst-define-key map [?\C-c ?\C-c ?\C-a] 'rst-compile-alt-toolset
		    [?\C-c ?2])
    ;; Convert the active region to pseudo-xml using the docutils tools.
    (rst-define-key map [?\C-c ?\C-c ?\C-x] 'rst-compile-pseudo-region
		    [?\C-c ?3])
    ;; Convert the current document to PDF and launch a viewer on the results.
    (rst-define-key map [?\C-c ?\C-c ?\C-p] 'rst-compile-pdf-preview
		    [?\C-c ?4])
    ;; Convert the current document to S5 slides and view in a web browser.
    (rst-define-key map [?\C-c ?\C-c ?\C-s] 'rst-compile-slides-preview
		    [?\C-c ?5])

    map)
  "Keymap for reStructuredText mode commands.
This inherits from Text mode.")


;; Abbrevs.
(define-abbrev-table 'rst-mode-abbrev-table
  (mapcar (lambda (x) (append x '(nil 0 system)))
          '(("contents" ".. contents::\n..\n   ")
            ("con" ".. contents::\n..\n   ")
            ("cont" "[...]")
            ("skip" "\n\n[...]\n\n  ")
            ("seq" "\n\n[...]\n\n  ")
            ;; FIXME: Add footnotes, links, and more.
            ))
  "Abbrev table used while in `rst-mode'.")


;; Syntax table.
(defvar rst-mode-syntax-table
  (let ((st (copy-syntax-table text-mode-syntax-table)))
    (modify-syntax-entry ?$ "." st)
    (modify-syntax-entry ?% "." st)
    (modify-syntax-entry ?& "." st)
    (modify-syntax-entry ?' "." st)
    (modify-syntax-entry ?* "." st)
    (modify-syntax-entry ?+ "." st)
    (modify-syntax-entry ?- "." st)
    (modify-syntax-entry ?/ "." st)
    (modify-syntax-entry ?< "." st)
    (modify-syntax-entry ?= "." st)
    (modify-syntax-entry ?> "." st)
    (modify-syntax-entry ?\\ "\\" st)
    (modify-syntax-entry ?_ "." st)
    (modify-syntax-entry ?| "." st)
    (modify-syntax-entry ?« "." st)
    (modify-syntax-entry ?» "." st)
    (modify-syntax-entry ?‘ "." st)
    (modify-syntax-entry ?’ "." st)
    (modify-syntax-entry ?“ "." st)
    (modify-syntax-entry ?” "." st)
    st)
  "Syntax table used while in `rst-mode'.")

(defcustom rst-mode-hook nil
  "Hook run when `rst-mode' is turned on.
The hook for `text-mode' is run before this one."
  :group 'rst
  :type '(hook))
(rst-testcover-defcustom)

;; Pull in variable definitions silencing byte-compiler.
(require 'newcomment)

(defvar electric-pair-pairs)

;; Use rst-mode for *.rst and *.rest files.  Many ReStructured-Text files
;; use *.txt, but this is too generic to be set as a default.
;;;###autoload (add-to-list 'auto-mode-alist (purecopy '("\\.re?st\\'" . rst-mode)))
;;;###autoload
(define-derived-mode rst-mode text-mode "ReST"
  "Major mode for editing reStructuredText documents.
\\<rst-mode-map>

Turning on `rst-mode' calls the normal hooks `text-mode-hook'
and `rst-mode-hook'.  This mode also supports font-lock
highlighting.

\\{rst-mode-map}"
  :abbrev-table rst-mode-abbrev-table
  :syntax-table rst-mode-syntax-table
  :group 'rst

  ;; Paragraph recognition.
  (setq-local paragraph-separate
	      (rst-re '(:alt
			"\f"
			lin-end)))
  (setq-local paragraph-start
	      (rst-re '(:alt
			"\f"
			lin-end
			(:seq hws-tag par-tag- bli-sfx))))

  ;; Indenting and filling.
  (setq-local indent-line-function 'rst-indent-line)
  (setq-local adaptive-fill-mode t)
  (setq-local adaptive-fill-regexp (rst-re 'hws-tag 'par-tag- "?" 'hws-tag))
  (setq-local adaptive-fill-function 'rst-adaptive-fill)
  (setq-local fill-paragraph-handle-comment nil)

  ;; Comments.
  (setq-local comment-start ".. ")
  (setq-local comment-start-skip (rst-re 'lin-beg 'exm-tag 'bli-sfx))
  (setq-local comment-continue "   ")
  (setq-local comment-multi-line t)
  (setq-local comment-use-syntax nil)
  ;; reStructuredText has not really a comment ender but nil is not really a
  ;; permissible value.
  (setq-local comment-end "")
  (setq-local comment-end-skip nil)

  ;; Commenting in reStructuredText is very special so use our own set of
  ;; functions.
  (setq-local comment-line-break-function 'rst-comment-line-break)
  (setq-local comment-indent-function 'rst-comment-indent)
  (setq-local comment-insert-comment-function 'rst-comment-insert-comment)
  (setq-local comment-region-function 'rst-comment-region)
  (setq-local uncomment-region-function 'rst-uncomment-region)

  (setq-local electric-pair-pairs '((?\" . ?\") (?\* . ?\*) (?\` . ?\`)))

  ;; Imenu and which function.
  ;; FIXME: Check documentation of `which-function' for alternative ways to
  ;;        determine the current function name.
  (setq-local imenu-create-index-function 'rst-imenu-create-index)

  ;; Font lock.
  (setq-local font-lock-defaults
	      '(rst-font-lock-keywords
		t nil nil nil
		(font-lock-multiline . t)
		(font-lock-mark-block-function . mark-paragraph)))
  (add-hook 'font-lock-extend-region-functions 'rst-font-lock-extend-region t)

  ;; Text after a changed line may need new fontification.
  (setq-local jit-lock-contextually t)

  ;; Indentation is not deterministic.
  (setq-local electric-indent-inhibit t))

;;;###autoload
(define-minor-mode rst-minor-mode
  "Toggle ReST minor mode.
With a prefix argument ARG, enable ReST minor mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

When ReST minor mode is enabled, the ReST mode keybindings
are installed on top of the major mode bindings.  Use this
for modes derived from Text mode, like Mail mode."
 ;; The initial value.
 nil
 ;; The indicator for the mode line.
 " ReST"
 ;; The minor mode bindings.
 rst-mode-map
 :group 'rst)

;; FIXME: can I somehow install these too?
;;        :abbrev-table rst-mode-abbrev-table
;;        :syntax-table rst-mode-syntax-table


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section adornment adjustment

;; The following functions implement a smart automatic title sectioning feature.
;; The idea is that with the cursor sitting on a section title, we try to get as
;; much information from context and try to do the best thing automatically.
;; This function can be invoked many times and/or with prefix argument to rotate
;; between the various sectioning adornments.
;;
;; Some notes:
;;
;; - The underlining character that is used depends on context. The file is
;;   scanned to find other sections and an appropriate character is selected.
;;   If the function is invoked on a section that is complete, the character is
;;   rotated among the existing section adornments.
;;
;;   Note that when rotating the characters, if we come to the end of the
;;   hierarchy of adornments, the variable `rst-preferred-adornments' is
;;   consulted to propose a new underline adornment, and if continued, we cycle
;;   the adornments all over again.  Set this variable to nil if you want to
;;   limit the underlining character propositions to the existing adornments in
;;   the file.
;;
;; - An underline/overline that is not extended to the column at which it should
;;   be hanging is dubbed INCOMPLETE.  For example::
;;
;;      |Some Title
;;      |-------
;;
;; Examples of default invocation:
;;
;;   |Some Title       --->    |Some Title
;;   |                         |----------
;;
;;   |Some Title       --->    |Some Title
;;   |-----                    |----------
;;
;;   |                         |------------
;;   | Some Title      --->    | Some Title
;;   |                         |------------
;;
;; In over-and-under style, when alternating the style, a variable is
;; available to select how much default indent to use (it can be zero).  Note
;; that if the current section adornment already has an indent, we don't
;; adjust it to the default, we rather use the current indent that is already
;; there for adjustment (unless we cycle, in which case we use the indent
;; that has been found previously).

(defgroup rst-adjust nil
  "Settings for adjustment and cycling of section title adornments."
  :group 'rst
  :version "21.1")

(define-obsolete-variable-alias
  'rst-preferred-decorations 'rst-preferred-adornments "rst 1.0.0")
;; FIXME: Default must match suggestion in
;;        http://sphinx-doc.org/rest.html#sections for Python documentation.
(defcustom rst-preferred-adornments '((?= over-and-under 1)
				      (?= simple 0)
				      (?- simple 0)
				      (?~ simple 0)
				      (?+ simple 0)
				      (?` simple 0)
				      (?# simple 0)
				      (?@ simple 0))
  "Preferred hierarchy of section title adornments.
A list consisting of lists of the form (CHARACTER STYLE INDENT).
CHARACTER is the character used.  STYLE is one of the symbols
`over-and-under' or `simple'.  INDENT is an integer giving the
wanted indentation for STYLE `over-and-under'.

This sequence is consulted to offer a new adornment suggestion
when we rotate the underlines at the end of the existing
hierarchy of characters, or when there is no existing section
title in the file.

Set this to an empty list to use only the adornment found in the
file."
  :group 'rst-adjust
  :type `(repeat
	  (group :tag "Adornment specification"
		 (choice :tag "Adornment character"
			 ,@(mapcar (lambda (char)
				     (list 'const
					   :tag (char-to-string char) char))
				   rst-adornment-chars))
		 (radio :tag "Adornment type"
			(const :tag "Overline and underline" over-and-under)
			(const :tag "Underline only" simple))
		 (integer :tag "Indentation for overline and underline type"
			  :value 0))))
(rst-testcover-defcustom)

;; FIXME: Rename this to `rst-over-and-under-default-indent' and set default to
;;        0 because the effect of 1 is probably surprising in the few cases
;;        where this is used.
;; FIXME: A matching adornment style can be looked for in
;;        `rst-preferred-adornments' and its indentation used before using this
;;        variable.
(defcustom rst-default-indent 1
  "Number of characters to indent the section title.
This is only used while toggling adornment styles when switching
from a simple adornment style to a over-and-under adornment
style.  In addition this is used in cases where the adornments
found in the buffer are to be used but the indentation for
over-and-under adornments is inconsistent across the buffer."
  :group 'rst-adjust
  :type '(integer))
(rst-testcover-defcustom)

(defun rst-new-preferred-hdr (seen prev)
  ;; testcover: ok.
  "Return a new, preferred `rst-Hdr' different from all in SEEN.
PREV is the previous `rst-Hdr' in the buffer.  If given the
search starts after this entry.  Return nil if no new preferred
`rst-Hdr' can be found."
  ;; All preferred adornments are candidates.
  (let ((candidates
	 (append
	  (if prev
	      ;; Start searching after the level of the previous adornment.
	      (cdr (rst-Hdr-member-ado prev (rst-Hdr-preferred-adornments))))
	  (rst-Hdr-preferred-adornments))))
    (car
     (rst-member-if (lambda (cand)
		      (not (rst-Hdr-member-ado cand seen)))
		    candidates))))

(defun rst-delete-entire-line ()
  "Delete the entire current line without using the `kill-ring'."
  (delete-region (line-beginning-position)
                 (line-beginning-position 2)))

(defun rst-update-section (hdr)
  "Unconditionally update the style of the section header at point to HDR.
If there are existing overline and/or underline from the
existing adornment, they are removed before adding the
requested adornment."
  (end-of-line)
  (let ((indent (or (rst-Hdr-indent hdr) 0))
	(marker (point-marker))
	len)

    ;; Fixup whitespace at the beginning and end of the line.
    (beginning-of-line)
    (delete-horizontal-space)
    (insert (make-string indent ? ))

    (end-of-line)
    (delete-horizontal-space)

    ;; Set the current column, we're at the end of the title line.
    (setq len (+ (current-column) indent))

    ;; Remove previous line if it is an adornment.
    (save-excursion
      (forward-line -1) ;; FIXME testcover: Doesn't work when in first line of
                        ;;                  buffer.
      (if (and (looking-at (rst-re 'ado-beg-2-1))
	       ;; Avoid removing the underline of a title right above us.
	       (save-excursion (forward-line -1)
			       (not (looking-at (rst-re 'ttl-beg-1)))))
	  (rst-delete-entire-line)))

    ;; Remove following line if it is an adornment.
    (save-excursion
      (forward-line +1) ;; FIXME testcover: Doesn't work when in last line
                        ;;                  of buffer.
      (if (looking-at (rst-re 'ado-beg-2-1))
	  (rst-delete-entire-line))
      ;; Add a newline if we're at the end of the buffer unless it is the final
      ;; empty line, for the subsequent inserting of the underline.
      (if (and (= (point) (buffer-end 1)) (not (bolp)))
	  (newline 1)))

    ;; Insert overline.
    (when (rst-Hdr-is-over-and-under hdr)
      (save-excursion
	(beginning-of-line)
	(open-line 1)
	(insert (make-string len (rst-Hdr-get-char hdr)))))

    ;; Insert underline.
    (1value ;; Line has been inserted above.
     (forward-line +1))
    (open-line 1)
    (insert (make-string len (rst-Hdr-get-char hdr)))

    (1value ;; Line has been inserted above.
     (forward-line +1))
    (goto-char marker)))

(defun rst-classify-adornment (adornment end)
  "Classify adornment string for section titles and transitions.
ADORNMENT is the complete adornment string as found in the buffer
with optional trailing whitespace.  END is the point after the
last character of ADORNMENT.  Return a `rst-Ttl' or nil if no
syntactically valid adornment is found."
  (save-excursion
    (save-match-data
      (when (string-match (rst-re 'ado-beg-2-1) adornment)
	(goto-char end)
	(let* ((ado-ch (string-to-char (match-string 2 adornment)))
	       (ado-re (rst-re ado-ch 'adorep3-hlp))
	       (end-pnt (point))
	       (beg-pnt (progn
			  (1value ;; No lines may be left to move.
			   (forward-line 0))
			  (point)))
	       (nxt-emp ; Next line nonexistent or empty
		(save-excursion
		  (or (not (zerop (forward-line 1)))
		      ;; FIXME testcover: Add test classifying at the end of
		      ;;                  buffer.
		      (looking-at (rst-re 'lin-end)))))
	       (prv-emp ; Previous line nonexistent or empty
		(save-excursion
		  (or (not (zerop (forward-line -1)))
		      (looking-at (rst-re 'lin-end)))))
	       txt-blw
	       (ttl-blw ; Title found below starting here.
		(save-excursion
		  (and
		   (zerop (forward-line 1)) ;; FIXME testcover: Add test
					    ;; classifying at the end of
					    ;; buffer.
		   (looking-at (rst-re 'ttl-beg-1))
		   (setq txt-blw (match-string-no-properties 1))
		   (point))))
	       txt-abv
	       (ttl-abv ; Title found above starting here.
		(save-excursion
		  (and
		   (zerop (forward-line -1))
		   (looking-at (rst-re 'ttl-beg-1))
		   (setq txt-abv (match-string-no-properties 1))
		   (point))))
	       (und-fnd ; Matching underline found starting here.
		(save-excursion
		  (and ttl-blw
		   (zerop (forward-line 2)) ;; FIXME testcover: Add test
					    ;; classifying at the end of
					    ;; buffer.
		   (looking-at (rst-re ado-re 'lin-end))
		   (point))))
	       (ovr-fnd ; Matching overline found starting here.
		(save-excursion
		  (and ttl-abv
		   (zerop (forward-line -2))
		   (looking-at (rst-re ado-re 'lin-end))
		   (point))))
	       ado ind txt beg-ovr end-ovr beg-txt end-txt beg-und end-und)
	  (cond
	   ((and nxt-emp prv-emp)
	    ;; A transition.
	    (setq ado (rst-Ado-new-transition)
		  beg-txt beg-pnt
		  end-txt end-pnt))
	   ((or und-fnd ovr-fnd)
	    ;; An overline with an underline.
	    (setq ado (rst-Ado-new-over-and-under ado-ch))
	    (let (;; Prefer overline match over underline match.
		  (und-pnt (if ovr-fnd beg-pnt und-fnd))
		  (ovr-pnt (if ovr-fnd ovr-fnd beg-pnt))
		  (txt-pnt (if ovr-fnd ttl-abv ttl-blw)))
	      (goto-char ovr-pnt)
	      (setq beg-ovr (point)
		    end-ovr (line-end-position))
	      (goto-char txt-pnt)
	      (setq beg-txt (point)
		    end-txt (line-end-position)
		    ind (current-indentation)
		    txt (if ovr-fnd txt-abv txt-blw))
	      (goto-char und-pnt)
	      (setq beg-und (point)
		    end-und (line-end-position))))
	   (ttl-abv
	    ;; An underline.
	    (setq ado (rst-Ado-new-simple ado-ch)
		  beg-und beg-pnt
		  end-und end-pnt)
	    (goto-char ttl-abv)
	    (setq beg-txt (point)
		  end-txt (line-end-position)
		  ind (current-indentation)
		  txt txt-abv))
	   (t
	    ;; Invalid adornment.
	    (setq ado nil)))
	  (if ado
	      (rst-Ttl-new ado
			   (list
			    (or beg-ovr beg-txt)
			    (or end-und end-txt)
			    beg-ovr end-ovr beg-txt end-txt beg-und end-und)
			   ind txt)))))))

(defun rst-ttl-at-point ()
  "Find a section title line around point and return its characteristics.
If the point is on an adornment line find the respective title
line.  If the point is on an empty line check previous or next
line whether it is a suitable title line and use it if so.  If
point is on a suitable title line use it.  Return a `rst-Ttl' for
a section header or nil if no title line is found."
  (save-excursion
    (1value ;; No lines may be left to move.
     (forward-line 0))
    (let ((orig-pnt (point))
	  (orig-end (line-end-position)))
      (cond
       ((looking-at (rst-re 'ado-beg-2-1))
	;; Adornment found - consider it.
	(let ((char (string-to-char (match-string-no-properties 2)))
	      (r (rst-classify-adornment (match-string-no-properties 0)
					 (match-end 0))))
	  (cond
	   ((not r)
	    ;; Invalid adornment - check whether this is an overline with
	    ;; missing underline.
	    (if (and
		 (zerop (forward-line 1))
		 (looking-at (rst-re 'ttl-beg-1)))
		(rst-Ttl-new (rst-Ado-new-over-and-under char)
			     (list orig-pnt (line-end-position)
				   orig-pnt orig-end
				   (point) (line-end-position)
				   nil nil)
			     (current-indentation)
			     (match-string-no-properties 1))))
	   ((rst-Ado-is-transition (rst-Ttl-ado r))
	    nil)
	   ;; Return any other classification as is.
	   (r))))
       ((looking-at (rst-re 'lin-end))
	;; Empty line found - check surrounding lines for a title.
	(or
	 (save-excursion
	   (if (and (zerop (forward-line -1))
		    (looking-at (rst-re 'ttl-beg-1)))
	       (rst-Ttl-new nil
			    (list (point) (line-end-position)
				  nil nil
				  (point) (line-end-position)
				  nil nil)
			    (current-indentation)
			    (match-string-no-properties 1))))
	 (save-excursion
	   (if (and (zerop (forward-line 1))
		    (looking-at (rst-re 'ttl-beg-1)))
	       (rst-Ttl-new nil
			    (list (point) (line-end-position)
				  nil nil
				  (point) (line-end-position)
				  nil nil)
			    (current-indentation)
			    (match-string-no-properties 1))))))
       ((looking-at (rst-re 'ttl-beg-1))
	;; Title line found - check for a following underline.
	(let ((txt (match-string-no-properties 1)))
	  (or (rst-classify-adornment
	       (buffer-substring-no-properties
		(line-beginning-position 2) (line-end-position 2))
	       (line-end-position 2))
	      ;; No valid adornment found.
	      (rst-Ttl-new nil
			   (list (point) (line-end-position)
				 nil nil
				 (point) (line-end-position)
				 nil nil)
			   (current-indentation)
			   txt))))))))

;; The following function and variables are used to maintain information about
;; current section adornment in a buffer local cache. Thus they can be used for
;; font-locking and manipulation commands.

(defvar rst-all-ttls-cache nil
  "All section adornments in the buffer as found by `rst-all-ttls'.
Set to t when no section adornments were found.")
(make-variable-buffer-local 'rst-all-ttls-cache)

;; FIXME: If this variable is set to a different value font-locking of section
;;        headers is wrong.
(defvar rst-hdr-hierarchy-cache nil
  "Section hierarchy in the buffer as determined by `rst-hdr-hierarchy'.
Set to t when no section adornments were found.
Value depends on `rst-all-ttls-cache'.")
(make-variable-buffer-local 'rst-hdr-hierarchy-cache)

(rst-testcover-add-1value 'rst-reset-section-caches)
(defun rst-reset-section-caches ()
  "Reset all section cache variables.
Should be called by interactive functions which deal with sections."
  (setq rst-all-ttls-cache nil
	rst-hdr-hierarchy-cache nil))

(defun rst-all-ttls ()
  "Return all the section adornments in the current buffer.
Return a list of `rst-Ttl' with ascending line number.

Uses and sets `rst-all-ttls-cache'."
  (unless rst-all-ttls-cache
    (let (positions)
      ;; Iterate over all the section titles/adornments in the file.
      (save-excursion
	(save-match-data
	  (goto-char (point-min))
	  (while (re-search-forward (rst-re 'ado-beg-2-1) nil t)
	    (let ((ttl (rst-classify-adornment
			(match-string-no-properties 0) (point))))
	      (when (and ttl (rst-Ado-is-section (rst-Ttl-ado ttl)))
		(when (rst-Ttl-evaluate-hdr ttl)
		  (push ttl positions))
		(goto-char (rst-Ttl-get-end ttl)))))
	  (setq positions (nreverse positions))
	  (setq rst-all-ttls-cache (or positions t))))))
  (if (eq rst-all-ttls-cache t)
      nil
    (mapcar 'rst-Ttl-copy rst-all-ttls-cache)))

(defun rst-infer-hdr-hierarchy (hdrs)
  "Build a hierarchy from HDRS.
HDRS reflects the order in which the headers appear in the
buffer.  Return a `rst-Hdr' list representing the hierarchy of
headers in the buffer.  Indentation is unified."
  (let (ado2indents)
    (dolist (hdr hdrs)
      (let* ((ado (rst-Hdr-ado hdr))
	     (indent (rst-Hdr-indent hdr))
	     (found (assoc ado ado2indents)))
        (if found
	    (unless (member indent (cdr found))
	      ;; Append newly found indent.
	      (setcdr found (append (cdr found) (list indent))))
	  (push (list ado indent) ado2indents))))
    (mapcar (lambda (ado_indents)
	      (let ((ado (car ado_indents))
		    (indents (cdr ado_indents)))
		(rst-Hdr-new
		 ado
		 (if (> (length indents) 1)
		     ;; Indentations used inconsistently - use default.
		     rst-default-indent
		   ;; Only one indentation used - use this.
		   (car indents)))))
	    (nreverse ado2indents))))

(defun rst-hdr-hierarchy (&optional ignore-current)
  "Return the hierarchy of section titles in the file as a `rst-Hdr' list.
Each returned element may be used directly to create a section
adornment on that level.  If IGNORE-CURRENT a title found on the
current line is not taken into account when building the
hierarchy unless it appears again elsewhere.  This catches cases
where the current title is edited and may not be final regarding
its level.

Uses and sets `rst-hdr-hierarchy-cache' unless IGNORE-CURRENT is
given."
  (let* ((all-ttls (rst-all-ttls))
	 (ignore-position (if ignore-current
			      (line-beginning-position)))
	 (ignore-ttl
	  (if ignore-position
	      (car (member-if
		    (lambda (ttl)
		      (equal ignore-position (rst-Ttl-get-title-beginning ttl)))
		    all-ttls))))
	 (really-ignore
	  (if ignore-ttl
	      (<= (count-if
		   (lambda (ttl)
		     (rst-Ado-equal (rst-Ttl-ado ignore-ttl) (rst-Ttl-ado ttl)))
		   all-ttls)
		  1)))
	 (real-ttls (delq (if really-ignore ignore-ttl) all-ttls)))
    (mapcar ;; Protect cache.
     'rst-Hdr-copy
     (if (and (not ignore-current) rst-hdr-hierarchy-cache)
	 (if (eq rst-hdr-hierarchy-cache t)
	     nil
	   rst-hdr-hierarchy-cache)
       (let ((r (rst-infer-hdr-hierarchy (mapcar 'rst-Ttl-hdr real-ttls))))
	 (setq rst-hdr-hierarchy-cache
	       (if ignore-current
		   ;; Clear cache reflecting that a possible update is not
		   ;; reflected.
		   nil
		 (or r t)))
	 r)))))

(defun rst-all-ttls-with-level ()
  "Return the section adornments with levels set according to hierarchy.
Return a list of `rst-Ttl' with ascending line number."
    (let ((hier (rst-Hdr-ado-map (rst-hdr-hierarchy))))
      (mapcar
       (lambda (ttl)
	 (rst-Ttl-set-level ttl (rst-Ado-position (rst-Ttl-ado ttl) hier))
	 ttl)
       (rst-all-ttls))))

(defun rst-get-previous-hdr ()
  "Return the `rst-Hdr' before point or nil if none."
  (let ((ttls (rst-all-ttls))
	(curpos (line-beginning-position))
	prev)

    ;; Search for the adornments around the current line.
    (while (and ttls (< (rst-Ttl-get-title-beginning (car ttls)) curpos))
      (setq prev (car ttls)
            ttls (cdr ttls)))
    (and prev (rst-Ttl-hdr prev))))

(defun rst-adornment-complete-p (ado indent)
  "Return true if the adornment ADO around point is complete using INDENT.
The adornment is complete if it is a completely correct
reStructuredText adornment for the title line at point.  This
includes indentation and correct length of adornment lines."
  ;; Note: we assume that the detection of the overline as being the underline
  ;; of a preceding title has already been detected, and has been eliminated
  ;; from the adornment that is given to us.
  (let ((exps (rst-re "^" (rst-Ado-char ado)
		      (format "\\{%d\\}"
			      (+ (save-excursion
				   ;; Determine last column of title.
				   (end-of-line)
				   (current-column))
				 indent)) "$")))
    (and
     (save-excursion (forward-line +1)
		     (looking-at exps))
     (or (rst-Ado-is-simple ado)
	 (save-excursion (forward-line -1)
			 (looking-at exps))))))

(defun rst-next-hdr (hdr hier prev down)
  ;; testcover: ok.
  "Return the next best `rst-Hdr' upward from HDR.
Consider existing hierarchy HIER and preferred headers.  PREV may
be a previous `rst-Hdr' which may be taken into account.  If DOWN
return the next best `rst-Hdr' downward instead. Return nil in
HIER is nil."
  (let* ((normalized-hier (if down
			      hier
			    (reverse hier)))
	 (fnd (rst-Hdr-member-ado hdr normalized-hier))
	 (prev-fnd (and prev (rst-Hdr-member-ado prev normalized-hier))))
    (or
     ;; Next entry in existing hierarchy if it exists.
     (cadr fnd)
     (if fnd
	 ;; If current header is found try introducing a new one from preferred
	 ;; hierarchy.
	 (rst-new-preferred-hdr hier prev)
       ;; If not found try using previous header.
       (if down
	   (cadr prev-fnd)
	 (car prev-fnd)))
     ;; All failed - rotate by using first from normalized existing hierarchy.
     (car normalized-hier))))

;; FIXME: A line "``/`` full" is not accepted as a section title.
(defun rst-adjust (pfxarg)
  "Auto-adjust the adornment around point.
Adjust/rotate the section adornment for the section title around
point or promote/demote the adornments inside the region,
depending on whether the region is active.  This function is meant
to be invoked possibly multiple times, and can vary its behavior
with a positive PFXARG (toggle style), or with a negative
PFXARG (alternate behavior).

This function is a bit of a swiss knife.  It is meant to adjust
the adornments of a section title in reStructuredText.  It tries
to deal with all the possible cases gracefully and to do \"the
right thing\" in all cases.

See the documentations of `rst-adjust-section' and
`rst-promote-region' for full details.

The method can take either (but not both) of

a. a (non-negative) prefix argument, which means to toggle the
   adornment style.  Invoke with a prefix argument for example;

b. a negative numerical argument, which generally inverts the
   direction of search in the file or hierarchy.  Invoke with C--
   prefix for example."
  (interactive "P")

  (let* (;; Save our original position on the current line.
	 (origpt (point-marker))

         (reverse-direction (and pfxarg (< (prefix-numeric-value pfxarg) 0)))
         (toggle-style (and pfxarg (not reverse-direction))))

    (if (use-region-p)
        ;; Adjust adornments within region.
        (rst-promote-region (and pfxarg t))
      ;; Adjust adornment around point.
      (let ((msg (rst-adjust-section toggle-style reverse-direction)))
	(when msg
	  (apply 'message msg))))

    ;; Run the hooks to run after adjusting.
    (run-hooks 'rst-adjust-hook)

    (rst-reset-section-caches)

    ;; Make sure to reset the cursor position properly after we're done.
    (goto-char origpt)))

(defcustom rst-adjust-hook nil
  "Hooks to be run after running `rst-adjust'."
  :group 'rst-adjust
  :type '(hook)
  :package-version '(rst . "1.1.0"))
(rst-testcover-defcustom)

(defcustom rst-new-adornment-down nil
  "Controls level of new adornment for section headers."
  :group 'rst-adjust
  :type '(choice
	  (const :tag "Same level as previous one" nil)
	  (const :tag "One level down relative to the previous one" t))
  :package-version '(rst . "1.1.0"))
(rst-testcover-defcustom)

(defun rst-adjust-adornment (pfxarg)
  "Call `rst-adjust-section' interactively.
Keep this for compatibility for older bindings (are there any?).
Argument PFXARG has the same meaning as for `rst-adjust'."
  (interactive "P")

  (let* ((reverse-direction (and pfxarg (< (prefix-numeric-value pfxarg) 0)))
         (toggle-style (and pfxarg (not reverse-direction))))
    (rst-adjust-section toggle-style reverse-direction)))

(defun rst-adjust-section (toggle-style reverse)
"Adjust/rotate the section adornment for the section title around point.
The action this function takes depends on context around the
point, and it is meant to be invoked possibly more than once to
rotate among the various possibilities.  Basically, this function
deals with:

- adding an adornment if the title does not have one;

- adjusting the length of the underline characters to fit a
  modified title;

- rotating the adornment in the set of already existing
  sectioning adornments used in the file;

- switching between simple and over-and-under styles by giving
  TOGGLE-STYLE.

Return nil if the function did something.  If the function were
not able to do something return an argument list for `message' to
inform the user about what failed.

The following is a detailed description but you should normally
not have to read it.

Before applying the adornment change, the cursor is placed on the
closest line that could contain a section title if such is found
around the cursor.  Then the following cases are distinguished.

* Case 1: No Adornment

  If the current line has no adornment around it,

  - search for a previous adornment, and apply this adornment (unless
    `rst-new-adornment-down') or one level lower (otherwise) to the current
    line.  If there is no defined level below this previous adornment, we
    suggest the most appropriate of the `rst-preferred-adornments'.

    If REVERSE is true, we simply use the previous adornment found
    directly.

  - if there is no adornment found in the given direction, we use the first of
    `rst-preferred-adornments'.

  TOGGLE-STYLE forces a toggle of the prescribed adornment style.

* Case 2: Incomplete Adornment

  If the current line does have an existing adornment, but the adornment is
  incomplete, that is, the underline/overline does not extend to exactly the
  end of the title line (it is either too short or too long), we simply extend
  the length of the underlines/overlines to fit exactly the section title.

  If TOGGLE-STYLE we toggle the style of the adornment as well.

  REVERSE has no effect in this case.

* Case 3: Complete Existing Adornment

  If the adornment is complete (i.e. the underline (overline) length is already
  adjusted to the end of the title line), we rotate the current title's
  adornment according to the adornment hierarchy found in the buffer.  This is
  meant to be used potentially multiple times, until the desired adornment is
  found around the title.

  If we hit the boundary of the hierarchy, exactly one choice from the list of
  preferred adornments is suggested/chosen, the first of those adornment that
  has not been seen in the buffer yet, and the next invocation rolls over to
  the other end of the hierarchy (i.e. it cycles).

  If REVERSE is we go up in the hierarchy.  Otherwise we go down.

  However, if TOGGLE-STYLE, we do not rotate the adornment, but instead simply
  toggle the style of the current adornment."
  (rst-reset-section-caches)
  (let ((ttl (rst-ttl-at-point))
	(orig-pnt (point))
	msg)
    (if (not ttl)
	(setq msg '("No section header or candidate at point"))
      (goto-char (rst-Ttl-get-title-beginning ttl))
      (let ((moved (- (line-number-at-pos) (line-number-at-pos orig-pnt)))
	    (found (rst-Ttl-ado ttl))
	    (indent (rst-Ttl-indent ttl))
	    (prev (rst-get-previous-hdr))
	    new)
	(when (and found (not (rst-Ado-p found)))
	  ;; Normalize found adornment - overline with no underline counts as
	  ;; overline.
	  (setq found (rst-Ado-new-over-and-under found)))
	(setq new
	      (cond
	       ((not found)
		;; Case 1: No adornment at all.
		(let ((hier (rst-hdr-hierarchy)))
		  (if prev
		      ;; Previous header exists - use it.
		      (cond
		       ;; Customization and parameters require that the
		       ;; previous level is used - use it as is.
		       ((or (and rst-new-adornment-down reverse)
			    (and (not rst-new-adornment-down) (not reverse)))
			prev)
		       ;; Advance one level down.
		       ((rst-next-hdr prev hier prev t))
		       (t
			(setq msg '("Neither hierarchy nor preferences can suggest a deeper header"))
			nil))
		    ;; First header in the buffer - use the first adornment
		    ;; from preferences or hierarchy.
		    (let ((p (car (rst-Hdr-preferred-adornments)))
			  (h (car hier)))
		      (cond
		       ((if reverse
			    ;; Prefer hierarchy for downwards
			    (or h p)
			  ;; Prefer preferences for upwards
			  (or p h)))
		       (t
			(setq msg '("No preferences to suggest a top level from"))
			nil))))))
	       ((not (rst-adornment-complete-p found indent))
		;; Case 2: Incomplete adornment.
		;; Use lax since indentation might not match suggestion.
		(rst-Hdr-new-lax found indent))
	       ;; Case 3: Complete adornment exists from here on.
	       (toggle-style
		;; Simply switch the style of the current adornment.
		(setq toggle-style nil) ;; Remember toggling has been done.
		(rst-Hdr-new-invert found rst-default-indent))
	       (t
		;; Rotate, ignoring a sole adornment around the current line.
		(let ((hier (rst-hdr-hierarchy t)))
		  (cond
		   ;; Next header can be determined from hierarchy or
		   ;; preferences.
		   ((rst-next-hdr
		     ;; Use lax since indentation might not match suggestion.
		     (rst-Hdr-new-lax found indent) hier prev reverse))
		   ;; No next header found.
		   (t
		    (setq msg '("No preferences or hierarchy to suggest another level from"))
		    nil))))))
	(if (not new)
	    (goto-char orig-pnt)
	  (when toggle-style
	    (setq new (rst-Hdr-new-invert (rst-Hdr-ado new) indent)))
	  ;; Override indent with present indent if there is some.
	  (when (> indent 0)
	    ;; Use lax since existing indent may not be valid for new style.
	    (setq new (rst-Hdr-new-lax (rst-Hdr-ado new) indent)))
	  (rst-update-section new)
	  ;; Correct the position of the cursor to more accurately reflect where
	  ;; it was located when the function was invoked.
	  (unless (zerop moved)
	    (forward-line (- moved))
	    (end-of-line)))))
    msg))

;; Maintain an alias for compatibility.
(defalias 'rst-adjust-section-title 'rst-adjust)

(defun rst-promote-region (demote)
  "Promote the section titles within the region.
With argument DEMOTE or a prefix argument, demote the section
titles instead.  The algorithm used at the boundaries of the
hierarchy is similar to that used by `rst-adjust-section'."
  (interactive "P")
  (rst-reset-section-caches)
  (let ((ttls (rst-all-ttls))
	(hier (rst-hdr-hierarchy))
	(region-beg (save-excursion
		      (goto-char (region-beginning))
		      (line-beginning-position)))
	(region-end (save-excursion
		      (goto-char (region-end))
		      (line-beginning-position)))
	marker-list)

    ;; Skip the markers that come before the region beginning.
    (while (and ttls (< (rst-Ttl-get-title-beginning (car ttls)) region-beg))
      (setq ttls (cdr ttls)))

    ;; Create a list of markers for all the adornments which are found within
    ;; the region.
    (save-excursion
      (while (and ttls (< (rst-Ttl-get-title-beginning (car ttls)) region-end))
	(push (cons (copy-marker (rst-Ttl-get-title-beginning (car ttls)))
		    (rst-Ttl-hdr (car ttls))) marker-list)
	(setq ttls (cdr ttls)))

      ;; Apply modifications.
      (dolist (p marker-list)
	;; Go to the adornment to promote.
	(goto-char (car p))
	;; `rst-next-hdr' cannot return nil because we apply to a section
	;; header so there is some hierarchy.
	(rst-update-section (rst-next-hdr (cdr p) hier nil demote))

	;; Clear marker to avoid slowing down the editing after we're done.
	(set-marker (car p) nil))
      (setq deactivate-mark nil))))

(defun rst-display-hdr-hierarchy ()
  "Display the current file's section title adornments hierarchy.
Hierarchy is displayed in a temporary buffer."
  (interactive)
  (rst-reset-section-caches)
  (let ((hdrs (rst-hdr-hierarchy))
	(level 1))
    (with-output-to-temp-buffer "*rest section hierarchy*"
      (with-current-buffer standard-output
	(dolist (hdr hdrs)
	  (insert (format "\nSection Level %d" level))
	  (rst-update-section hdr)
	  (goto-char (point-max))
	  (insert "\n")
	  (incf level))))))

;; Maintain an alias for backward compatibility.
(defalias 'rst-display-adornments-hierarchy 'rst-display-hdr-hierarchy)

;; FIXME: Should accept an argument giving the hierarchy level to start with
;;        instead of the top of the hierarchy.
(defun rst-straighten-sections ()
  "Redo the adornments of all section titles in the current buffer.
This is done using the preferred set of adornments.  This can be
used, for example, when using somebody else's copy of a document,
in order to adapt it to our preferred style."
  (interactive)
  (rst-reset-section-caches)
  (save-excursion
    (dolist (ttl-marker (mapcar
			 (lambda (ttl)
			   (cons ttl (copy-marker
				      (rst-Ttl-get-title-beginning ttl))))
			 (rst-all-ttls-with-level)))
      ;; Go to the appropriate position.
      (goto-char (cdr ttl-marker))
      (rst-update-section (nth (rst-Ttl-level (car ttl-marker))
			       (rst-Hdr-preferred-adornments)))
      ;; Reset the marker to avoid slowing down editing.
      (set-marker (cdr ttl-marker) nil))))

;; Maintain an alias for compatibility.
(defalias 'rst-straighten-adornments 'rst-straighten-sections)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Insert list items

; Borrowed from a2r.el (version 1.3), by Lawrence Mitchell <wence@gmx.li>.
; I needed to make some tiny changes to the functions, so I put it here.
; -- Wei-Wei Guo

(defconst rst-arabic-to-roman
  '((1000 .   "M") (900  .  "CM") (500  .   "D") (400  .  "CD")
    (100  .   "C") (90   .  "XC") (50   .   "L") (40   .  "XL")
    (10   .   "X") (9    .  "IX") (5    .   "V") (4    .  "IV")
    (1    .   "I"))
  "List of maps between Arabic numbers and their Roman numeral equivalents.")

(defun rst-arabic-to-roman (num &optional arg)
  "Convert Arabic number NUM to its Roman numeral representation.

Obviously, NUM must be greater than zero.  Don't blame me, blame the
Romans, I mean \"what have the Romans ever _done_ for /us/?\" (with
apologies to Monty Python).
If optional ARG is non-nil, insert in current buffer."
  (let ((map rst-arabic-to-roman)
        res)
    (while (and map (> num 0))
      (if (or (= num (caar map))
              (> num (caar map)))
          (setq res (concat res (cdar map))
                num (- num (caar map)))
        (setq map (cdr map))))
    (if arg (insert (or res "")) res)))

(defun rst-roman-to-arabic (string &optional arg)
  "Convert STRING of Roman numerals to an Arabic number.

If STRING contains a letter which isn't a valid Roman numeral,
the rest of the string from that point onwards is ignored.

Hence:
MMD == 2500
and
MMDFLXXVI == 2500.
If optional ARG is non-nil, insert in current buffer."
  (let ((res 0)
        (map rst-arabic-to-roman))
    (while map
      (if (string-match (concat "^" (cdar map)) string)
          (setq res (+ res (caar map))
                string (replace-match "" nil t string))
        (setq map (cdr map))))
    (if arg (insert res) res)))

;; End of borrow.

(defun rst-find-pfx-in-region (beg end pfx-re)
  "Find all the positions of prefixes in region between BEG and END.
This is used to find bullets and enumerated list items.  PFX-RE is
a regular expression for matching the lines after indentation
with items.  Returns a list of cons cells consisting of the point
and the column of the point."
  (let ((pfx ()))
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
	(back-to-indentation)
	(when (and
	       (looking-at pfx-re) ; pfx found and...
	       (let ((pfx-col (current-column)))
		 (save-excursion
		   (forward-line -1) ; ...previous line is...
		   (back-to-indentation)
		   (or (looking-at (rst-re 'lin-end)) ; ...empty,
		       (> (current-column) pfx-col) ; ...deeper level, or
		       (and (= (current-column) pfx-col)
			    (looking-at pfx-re)))))) ; ...pfx at same level.
	  (push (cons (point) (current-column))
                pfx))
	(forward-line 1)))
    (nreverse pfx)))

(defun rst-insert-list-pos (newitem)
  "Arrange relative position of a newly inserted list item of style NEWITEM.

Adding a new list might consider three situations:

 (a) Current line is a blank line.
 (b) Previous line is a blank line.
 (c) Following line is a blank line.

When (a) and (b), just add the new list at current line.

when (a) and not (b), a blank line is added before adding the new list.

When not (a), first forward point to the end of the line, and add two
blank lines, then add the new list.

Other situations are just ignored and left to users themselves."
  (if (save-excursion
        (beginning-of-line)
        (looking-at (rst-re 'lin-end)))
      (if (save-excursion
            (forward-line -1)
            (looking-at (rst-re 'lin-end)))
          (insert newitem " ")
        (insert "\n" newitem " "))
    (end-of-line)
    (insert "\n\n" newitem " ")))

;; FIXME: Isn't this a `defconst'?
(defvar rst-initial-enums
  (let (vals)
    (dolist (fmt '("%s." "(%s)" "%s)"))
      (dolist (c '("1" "a" "A" "I" "i"))
        (push (format fmt c) vals)))
    (cons "#." (nreverse vals)))
  "List of initial enumerations.")

;; FIXME: Isn't this a `defconst'?
(defvar rst-initial-items
  (append (mapcar 'char-to-string rst-bullets) rst-initial-enums)
  "List of initial items.  It's a collection of bullets and enumerations.")

(defun rst-insert-list-new-item ()
  "Insert a new list item.

User is asked to select the item style first, for example (a), i), +.
Use TAB for completion and choices.

If user selects bullets or #, it's just added with position arranged by
`rst-insert-list-pos'.

If user selects enumerations, a further prompt is given.  User need to
input a starting item, for example 'e' for 'A)' style.  The position is
also arranged by `rst-insert-list-pos'."
  (interactive)
  ;; FIXME: Make this comply to `interactive' standards.
  (let* ((itemstyle (completing-read
		     "Select preferred item style [#.]: "
		     rst-initial-items nil t nil nil "#."))
	 (cnt (if (string-match (rst-re 'cntexp-tag) itemstyle)
		  (match-string 0 itemstyle)))
	 (no
	  (save-match-data
	    ;; FIXME: Make this comply to `interactive' standards.
	    (cond
	     ((equal cnt "a")
	      (let ((itemno (read-string "Give starting value [a]: "
					 nil nil "a")))
		(downcase (substring itemno 0 1))))
	     ((equal cnt "A")
	      (let ((itemno (read-string "Give starting value [A]: "
					 nil nil "A")))
		(upcase (substring itemno 0 1))))
	     ((equal cnt "I")
	      (let ((itemno (read-number "Give starting value [1]: " 1)))
		(rst-arabic-to-roman itemno)))
	     ((equal cnt "i")
	      (let ((itemno (read-number "Give starting value [1]: " 1)))
		(downcase (rst-arabic-to-roman itemno))))
	     ((equal cnt "1")
	      (let ((itemno (read-number "Give starting value [1]: " 1)))
		(number-to-string itemno)))))))
    (if no
	(setq itemstyle (replace-match no t t itemstyle)))
    (rst-insert-list-pos itemstyle)))

(defcustom rst-preferred-bullets
  '(?* ?- ?+)
  "List of favorite bullets."
  :group 'rst
  :type `(repeat
	  (choice ,@(mapcar (lambda (char)
			      (list 'const
				    :tag (char-to-string char) char))
			    rst-bullets)))
  :package-version '(rst . "1.1.0"))
(rst-testcover-defcustom)

(defun rst-insert-list-continue (curitem prefer-roman)
  "Insert a list item with list start CURITEM including its indentation level.
If PREFER-ROMAN roman numbering is preferred over using letters."
  (end-of-line)
  (insert
   "\n" ; FIXME: Separating lines must be possible.
   (cond
    ((string-match (rst-re '(:alt enmaut-tag
				  bul-tag)) curitem)
     curitem)
    ((string-match (rst-re 'num-tag) curitem)
     (replace-match (number-to-string
		     (1+ (string-to-number (match-string 0 curitem))))
		    nil nil curitem))
    ((and (string-match (rst-re 'rom-tag) curitem)
	  (save-match-data
	    (if (string-match (rst-re 'ltr-tag) curitem) ; Also a letter tag.
		(save-excursion
		  ;; FIXME: Assumes one line list items without separating
		  ;;        empty lines.
		  (if (and (zerop (forward-line -1))
			   (looking-at (rst-re 'enmexp-beg)))
		      (string-match
		       (rst-re 'rom-tag)
		       (match-string 0)) ; Previous was a roman tag.
		    prefer-roman)) ; Don't know - use flag.
	      t))) ; Not a letter tag.
     (replace-match
      (let* ((old (match-string 0 curitem))
	     (new (save-match-data
		    (rst-arabic-to-roman
		     (1+ (rst-roman-to-arabic
			  (upcase old)))))))
	(if (equal old (upcase old))
	    (upcase new)
	  (downcase new)))
      t nil curitem))
    ((string-match (rst-re 'ltr-tag) curitem)
     (replace-match (char-to-string
		     (1+ (string-to-char (match-string 0 curitem))))
		    nil nil curitem)))))

;; FIXME: At least the continuation may be folded into
;;        `newline-and-indent`. However, this may not be wanted by everyone so
;;        it should be possible to switch this off.
(defun rst-insert-list (&optional prefer-roman)
  "Insert a list item at the current point.

The command can insert a new list or a continuing list.  When it is called at a
non-list line, it will promote to insert new list.  When it is called at a list
line, it will insert a list with the same list style.

1. When inserting a new list:

User is asked to select the item style first, for example (a), i), +.  Use TAB
for completion and choices.

 (a) If user selects bullets or #, it's just added.
 (b) If user selects enumerations, a further prompt is given.  User needs to
     input a starting item, for example `e' for `A)' style.

The position of the new list is arranged according to whether or not the
current line and the previous line are blank lines.

2. When continuing a list, one thing needs to be noticed:

List style alphabetical list, such as `a.', and roman numerical list, such as
`i.', have some overlapping items, for example `v.' The function can deal with
the problem elegantly in most situations.  But when those overlapped list are
preceded by a blank line, it is hard to determine which type to use
automatically.  The function uses alphabetical list by default.  If you want
roman numerical list, just use a prefix to set PREFER-ROMAN."
  (interactive "P")
  (beginning-of-line)
  (if (looking-at (rst-re 'itmany-beg-1))
      (rst-insert-list-continue (match-string 0) prefer-roman)
    (rst-insert-list-new-item)))

(defun rst-straighten-bullets-region (beg end)
  "Make all the bulleted list items in the region consistent.
The region is specified between BEG and END.  You can use this
after you have merged multiple bulleted lists to make them use
the same/correct/consistent bullet characters.

See variable `rst-preferred-bullets' for the list of bullets to
adjust.  If bullets are found on levels beyond the
`rst-preferred-bullets' list, they are not modified."
  (interactive "r")

  (let ((bullets (rst-find-pfx-in-region beg end (rst-re 'bul-sta)))
	(levtable (make-hash-table :size 4)))

    ;; Create a map of levels to list of positions.
    (dolist (x bullets)
      (let ((key (cdr x)))
	(puthash key
		  (append (gethash key levtable (list))
			  (list (car x)))
		  levtable)))

    ;; Sort this map and create a new map of prefix char and list of positions.
    (let ((poslist ()))                 ; List of (indent . positions).
      (maphash (lambda (x y) (push (cons x y) poslist)) levtable)

      (let ((bullets rst-preferred-bullets))
        (dolist (x (sort poslist 'car-less-than-car))
          (when bullets
            ;; Apply the characters.
            (dolist (pos (cdr x))
              (goto-char pos)
              (delete-char 1)
              (insert (string (car bullets))))
            (setq bullets (cdr bullets))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Table of contents

(defun rst-all-stn ()
  "Return the hierarchical tree of section titles as a top level `rst-Stn'.
Return nil for no section titles."
  ;; FIXME: The top level node may contain the document title instead of nil.
  (cdr (rst-remaining-stn (rst-all-ttls-with-level) -1)))

(defun rst-remaining-stn (remaining lev)
  "Process the first entry of REMAINING expected to be on level LEV.
REMAINING is the remaining list of `rst-Ttl' entries.
Return (UNPROCESSED . NODE) for the first entry of REMAINING.
UNPROCESSED is the list of still unprocessed entries.  NODE is a
`rst-Stn' or nil if REMAINING is empty."
  (let ((ttl (car remaining))
	(unprocessed remaining)
        fnd children)
    ;; If the current adornment matches expected level.
    (when (and ttl (= (rst-Ttl-level ttl) lev))
      ;; Consume the current entry and create the current node with it.
      (setq unprocessed (cdr remaining))
      (setq fnd ttl))
    ;; Build the child nodes as long as they have deeper level.
    (while (and unprocessed (> (rst-Ttl-level (car unprocessed)) lev))
      (let* ((rem-child (rst-remaining-stn unprocessed (1+ lev)))
	     (child (cdr rem-child)))
	(when child
	  (push child children))
	(setq unprocessed (car rem-child))))
    (setq children (reverse children))
    (cons unprocessed
	  (if (or fnd children)
	      (rst-Stn-new fnd lev children)))))

(defun rst-stn-containing-point (stn &optional point)
  "Return `rst-Stn' in STN before POINT or nil if in no section.
POINT defaults to the current point.  STN may be nil for no
section headers at all."
  (when stn
    (setq point (or point (point)))
    (when (>= point (rst-Stn-get-title-beginning stn))
      ;; Point may be in this section or a child.
      (let ((children (rst-Stn-children stn))
	    found)
	(while (and children
		    (>= point (rst-Stn-get-title-beginning (car children))))
	  ;; Point may be in this child.
	  (setq found (car children)
		children (cdr children)))
	(if found
	    (rst-stn-containing-point found point)
	  stn)))))

(defgroup rst-toc nil
  "Settings for reStructuredText table of contents."
  :group 'rst
  :version "21.1")

(defcustom rst-toc-indent 2
  "Indentation for table-of-contents display.
Also used for formatting insertion, when numbering is disabled."
  :type 'integer
  :group 'rst-toc)
(rst-testcover-defcustom)

(defcustom rst-toc-insert-style 'fixed
  "Insertion style for table-of-contents.
Set this to one of the following values to determine numbering and
indentation style:
- `plain': no numbering (fixed indentation)
- `fixed': numbering, but fixed indentation
- `aligned': numbering, titles aligned under each other
- `listed': numbering, with dashes like list items (EXPERIMENTAL)"
  :type '(choice (const plain)
                 (const fixed)
                 (const aligned)
                 (const listed))
  :group 'rst-toc)
(rst-testcover-defcustom)

(defcustom rst-toc-insert-number-separator "  "
  "Separator that goes between the TOC number and the title."
  :type 'string
  :group 'rst-toc)
(rst-testcover-defcustom)

;; FIXME: What does this mean?
;; This is used to avoid having to change the user's mode.
(defvar rst-toc-insert-click-keymap
  (let ((map (make-sparse-keymap)))
       (define-key map [mouse-1] 'rst-toc-mode-mouse-goto)
       map)
  "(Internal) What happens when you click on propertized text in the TOC.")

(defcustom rst-toc-insert-max-level nil
  "If non-nil, maximum depth of the inserted TOC."
  :type '(choice (const nil) integer)
  :group 'rst-toc)
(rst-testcover-defcustom)

(defun rst-toc-insert (&optional pfxarg)
  "Insert a text rendering of the table of contents of the current section.
By default the top level is ignored if there is only one, because
we assume that the document will have a single title.

If a numeric prefix argument PFXARG is given, insert the TOC up
to the specified level.

The TOC is inserted indented at the current column."
  (interactive "P")
  (rst-reset-section-caches)
  (let (;; Check maximum level override.
	(rst-toc-insert-max-level
	 (if (and (integerp pfxarg) (> (prefix-numeric-value pfxarg) 0))
	     (prefix-numeric-value pfxarg) rst-toc-insert-max-level))
	(pt-stn (rst-stn-containing-point (rst-all-stn)))
	;; Figure out initial indent.
	(initial-indent (make-string (current-column) ? ))
	(init-point (point)))
    (when (and pt-stn (rst-Stn-children pt-stn))
      (rst-toc-insert-node pt-stn 0 initial-indent "")
      ;; FIXME: Really having the last newline would be better.
      ;; Delete the last newline added.
      (delete-char -1))))

(defun rst-toc-insert-node (stn level indent pfx)
  "Insert STN in table-of-contents.
LEVEL is the depth level of the sections in the tree currently
rendered.  INDENT is the indentation string.  PFX is the prefix
numbering, that includes the alignment necessary for all the
children of level to align."
  ;; Note: we do child numbering from the parent, so we start number the
  ;; children one level before we print them.
  (when (> level 0)
    (unless (> (current-column) 0)
      ;; No indent yet - insert it.
      (insert indent))
    (let ((beg (point)))
      (unless (equal rst-toc-insert-style 'plain)
	(insert pfx rst-toc-insert-number-separator))
      (insert (rst-Stn-get-text stn))
      ;; Add properties to the text, even though in normal text mode it
      ;; won't be doing anything for now.  Not sure that I want to change
      ;; mode stuff.  At least the highlighting gives the idea that this
      ;; is generated automatically.
      (put-text-property beg (point) 'mouse-face 'highlight)
      (put-text-property
       beg (point) 'rst-toc-target
       (set-marker (make-marker) (rst-Stn-get-title-beginning stn)))
      (put-text-property beg (point) 'keymap rst-toc-insert-click-keymap))
    (insert "\n")
    ;; Prepare indent for children.
    (setq indent
	  (cond
	   ((eq rst-toc-insert-style 'plain)
	    (concat indent (make-string rst-toc-indent ? )))
	   ((eq rst-toc-insert-style 'fixed)
	    (concat indent (make-string rst-toc-indent ? )))
	   ((eq rst-toc-insert-style 'aligned)
	    (concat indent (make-string (+ (length pfx) 2) ? )))
	   ((eq rst-toc-insert-style 'listed)
	    (concat (substring indent 0 -3)
		    (concat (make-string (+ (length pfx) 2) ? ) " - "))))))
  (when (or (eq rst-toc-insert-max-level nil)
	    (< level rst-toc-insert-max-level))
    (let ((count 1)
	  fmt)
      ;; Add a separating dot if there is already a prefix.
      (when (> (length pfx) 0)
	(string-match (rst-re "[ \t\n]*\\'") pfx)
	(setq pfx (concat (replace-match "" t t pfx) ".")))
      ;; Calculate the amount of space that the prefix will require
      ;; for the numbers.
      (when (rst-Stn-children stn)
	(setq fmt
	      (format "%%-%dd"
		      (1+ (floor (log (length (rst-Stn-children stn))
				      10))))))
      (dolist (child (rst-Stn-children stn))
	(rst-toc-insert-node child (1+ level) indent
			     (concat pfx (format fmt count)))
	(incf count)))))

(defun rst-toc-update ()
  "Automatically find the contents section of a document and update.
Updates the inserted TOC if present.  You can use this in your
file-write hook to always make it up-to-date automatically."
  (interactive)
  (save-excursion
    ;; Find and delete an existing comment after the first contents directive.
    ;; Delete that region.
    (goto-char (point-min))
    ;; We look for the following and the following only (in other words, if your
    ;; syntax differs, this won't work.).
    ;;
    ;;   .. contents:: [...anything here...]
    ;;      [:field: value]...
    ;;   ..
    ;;      XXXXXXXX
    ;;      XXXXXXXX
    ;;      [more lines]
    (let ((beg (re-search-forward
		(rst-re "^" 'exm-sta "contents" 'dcl-tag ".*\n"
			"\\(?:" 'hws-sta 'fld-tag ".*\n\\)*" 'exm-tag) nil t))
	  last-real)
      (when beg
	;; Look for the first line that starts at the first column.
	(forward-line 1)
	(while (and
		(< (point) (point-max))
		(or (if (looking-at
			 (rst-re 'hws-sta "\\S ")) ; indented content.
			(setq last-real (point)))
		    (looking-at (rst-re 'lin-end)))) ; empty line.
	  (forward-line 1))
	(if last-real
	    (progn
	      (goto-char last-real)
	      (end-of-line)
	      (delete-region beg (point)))
	  (goto-char beg))
	(insert "\n    ")
	(rst-toc-insert))))
  ;; Note: always return nil, because this may be used as a hook.
  nil)

;; FIXME: Updating the toc on saving would be nice. However, this doesn't work
;;        correctly:
;;
;;	  (add-hook 'write-contents-hooks 'rst-toc-update-fun)
;;	  (defun rst-toc-update-fun ()
;;	    ;; Disable undo for the write file hook.
;;	    (let ((buffer-undo-list t)) (rst-toc-update) ))

(defalias 'rst-toc-insert-update 'rst-toc-update) ; backwards compat.

(defun rst-toc-node (stn buf target)
  "Insert STN in the table-of-contents of buffer BUF.
If TARGET is given and this call renders a `rst-Stn' at the same
location return position of beginning of line.  Otherwise return
nil."
  (let ((beg (point))
	fnd)
    (if (or (not stn) (rst-Stn-is-top stn))
	(progn
	  (insert (format "Table of Contents:\n"))
	  (put-text-property beg (point)
			     'face (list '(background-color . "gray"))))
      (when (and target
		 (equal (rst-Stn-get-title-beginning stn)
			(rst-Stn-get-title-beginning target)))
	(setq fnd beg))
      (insert (make-string (* rst-toc-indent (rst-Stn-level stn)) ? ))
      (insert (rst-Stn-get-text stn))
      ;; Highlight lines.
      (put-text-property beg (point) 'mouse-face 'highlight)
      (insert "\n")
      ;; Add link on lines.
      (put-text-property
       beg (point) 'rst-toc-target
       (set-marker (make-marker) (rst-Stn-get-title-beginning stn) buf)))
    (when stn
      (dolist (child (rst-Stn-children stn))
	(setq fnd (or (rst-toc-node child buf target) fnd))))
    fnd))

(defvar rst-toc-buffer-name "*Table of Contents*"
  "Name of the Table of Contents buffer.")

(defvar rst-toc-return-wincfg nil
  "Window configuration to which to return when leaving the TOC.")

(defun rst-toc ()
  "Display a table-of-contents.
Finds all the section titles and their adornments in the
file, and displays a hierarchically-organized list of the
titles, which is essentially a table-of-contents of the
document.

The Emacs buffer can be navigated, and selecting a section
brings the cursor in that section."
  (interactive)
  (rst-reset-section-caches)
  (let* ((wincfg (list (current-window-configuration) (point-marker)))
         (sectree (rst-all-stn))
 	 (target-node (rst-stn-containing-point sectree))
	 (target-buf (current-buffer))
         (buf (get-buffer-create rst-toc-buffer-name))
	 target-pos)
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (rst-toc-mode)
        (delete-region (point-min) (point-max))
        (setq target-pos (rst-toc-node sectree target-buf target-node))))
    (display-buffer buf)
    (pop-to-buffer buf)
    (setq-local rst-toc-return-wincfg wincfg)
    (goto-char (or target-pos (point-min)))))

(defun rst-toc-mode-find-section ()
  "Get the section from text property at point."
  (let ((pos (get-text-property (point) 'rst-toc-target)))
    (unless pos
      (error "No section on this line"))
    (unless (buffer-live-p (marker-buffer pos))
      (error "Buffer for this section was killed"))
    pos))

;; FIXME: Cursor before or behind the list must be handled properly; before the
;;        list should jump to the top and behind the list to the last normal
;;        paragraph.
(defun rst-goto-section (&optional kill)
  "Go to the section the current line describes.
If KILL a TOC buffer is destroyed."
  (interactive)
  (let ((pos (rst-toc-mode-find-section)))
    (when kill
      ;; FIXME: This should rather go to `rst-toc-mode-goto-section'.
      (set-window-configuration (car rst-toc-return-wincfg))
      (kill-buffer (get-buffer rst-toc-buffer-name)))
    (pop-to-buffer (marker-buffer pos))
    (goto-char pos)
    ;; FIXME: make the recentering conditional on scroll.
    (recenter 5)))

(defun rst-toc-mode-goto-section ()
  "Go to the section the current line describes and kill the TOC buffer."
  (interactive)
  (rst-goto-section t))

(defun rst-toc-mode-mouse-goto (event)
  "In `rst-toc' mode, go to the occurrence whose line you click on.
EVENT is the input event."
  (interactive "e")
  (let ((pos
	 (with-current-buffer (window-buffer (posn-window (event-end event)))
	   (save-excursion
	     (goto-char (posn-point (event-end event)))
             (rst-toc-mode-find-section)))))
    (pop-to-buffer (marker-buffer pos))
    (goto-char pos)
    (recenter 5)))

(defun rst-toc-mode-mouse-goto-kill (event)
  "Same as `rst-toc-mode-mouse-goto', but kill TOC buffer as well.
EVENT is the input event."
  (interactive "e")
  (call-interactively 'rst-toc-mode-mouse-goto event)
  (kill-buffer (get-buffer rst-toc-buffer-name)))

(defun rst-toc-quit-window ()
  "Leave the current TOC buffer."
  (interactive)
  (let ((retbuf rst-toc-return-wincfg))
    (set-window-configuration (car retbuf))
    (goto-char (cadr retbuf))))

(defvar rst-toc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'rst-toc-mode-mouse-goto-kill)
    ;; FIXME: This very useful function must be on some key.
    (define-key map [mouse-2] 'rst-toc-mode-mouse-goto)
    (define-key map "\C-m" 'rst-toc-mode-goto-section)
    (define-key map "f" 'rst-toc-mode-goto-section)
    (define-key map "q" 'rst-toc-quit-window)
    ;; FIXME: Killing should clean up like `rst-toc-quit-window' does.
    (define-key map "z" 'kill-this-buffer)
    map)
  "Keymap for `rst-toc-mode'.")

(put 'rst-toc-mode 'mode-class 'special)

;; Could inherit from the new `special-mode'.
(define-derived-mode rst-toc-mode nil "ReST-TOC"
  "Major mode for output from \\[rst-toc], the table-of-contents for the document.

\\{rst-toc-mode-map}"
  (setq buffer-read-only t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section movement

(defun rst-forward-section (&optional offset)
  "Skip to the next reStructuredText section title.
OFFSET specifies how many titles to skip.  Use a negative OFFSET
to move backwards in the file (default is to use 1)."
  (interactive)
  (rst-reset-section-caches)
  (let* ((offset (or offset 1))
         (ttls (rst-all-ttls))
         (curpos (line-beginning-position))
         (cur ttls)
         (idx 0)
	 ttl)

    ;; Find the index of the "next" adornment with respect to the current line.
    (while (and cur (< (rst-Ttl-get-title-beginning (car cur)) curpos))
      (setq cur (cdr cur))
      (incf idx))
    ;; `cur' is the `rst-Ttl' on or following the current line.

    (if (and (> offset 0) cur
	     (equal (rst-Ttl-get-title-beginning (car cur)) curpos))
        (incf idx))

    ;; Find the final index.
    (setq idx (+ idx (if (> offset 0) (- offset 1) offset)))
    (setq ttl (nth idx ttls))
    (goto-char (cond
		((and ttl (>= idx 0))
		 (rst-Ttl-get-title-beginning ttl))
		((> offset 0)
		 (point-max))
		((point-min))))))

(defun rst-backward-section ()
  "Like `rst-forward-section', except move back one title."
  (interactive)
  (rst-forward-section -1))

;; FIXME: What is `allow-extend' for?
(defun rst-mark-section (&optional count allow-extend)
  "Select COUNT sections around point.
Mark following sections for positive COUNT or preceding sections
for negative COUNT."
  ;; Cloned from mark-paragraph.
  (interactive "p\np")
  (unless count (setq count 1))
  (when (zerop count)
    (error "Cannot mark zero sections"))
  (cond ((and allow-extend
	      (or (and (eq last-command this-command) (mark t))
		  (use-region-p)))
	 (set-mark
	  (save-excursion
	    (goto-char (mark))
	    (rst-forward-section count)
	    (point))))
	(t
	 (rst-forward-section count)
	 (push-mark nil t t)
	 (rst-forward-section (- count)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indentation

(defun rst-find-leftmost-column (beg end)
  "Return the leftmost column spanned by region BEG to END.
The line containing the start of the region is always considered
spanned.  If the region ends at the beginning of a line this line
is not considered spanned, otherwise it is spanned."
  (let (mincol)
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (back-to-indentation)
        (unless (looking-at (rst-re 'lin-end))
	  (setq mincol (if mincol
			   (min mincol (current-column))
			 (current-column))))
        (forward-line 1)))
    mincol))

;; FIXME: At the moment only block comments with leading empty comment line are
;;        supported.  Comment lines with leading comment markup should be also
;;        supported.  May be a customizable option could control which style to
;;        prefer.

(defgroup rst-indent nil "Settings for indentation in reStructuredText.

In reStructuredText indentation points are usually determined by
preceding lines.  Sometimes the syntax allows arbitrary indentation
points such as where to start the first line following a directive.
These indentation widths can be customized here."
  :group 'rst
  :package-version '(rst . "1.1.0"))

(define-obsolete-variable-alias
  'rst-shift-basic-offset 'rst-indent-width "rst 1.0.0")
(defcustom rst-indent-width 2
  "Indentation when there is no more indentation point given."
  :group 'rst-indent
  :type '(integer))
(rst-testcover-defcustom)

(defcustom rst-indent-field 3
  "Indentation for first line after a field or 0 to always indent for content."
  :group 'rst-indent
  :package-version '(rst . "1.1.0")
  :type '(integer))
(rst-testcover-defcustom)

(defcustom rst-indent-literal-normal 3
  "Default indentation for literal block after a markup on an own line."
  :group 'rst-indent
  :package-version '(rst . "1.1.0")
  :type '(integer))
(rst-testcover-defcustom)

(defcustom rst-indent-literal-minimized 2
  "Default indentation for literal block after a minimized markup."
  :group 'rst-indent
  :package-version '(rst . "1.1.0")
  :type '(integer))
(rst-testcover-defcustom)

(defcustom rst-indent-comment 3
  "Default indentation for first line of a comment."
  :group 'rst-indent
  :package-version '(rst . "1.1.0")
  :type '(integer))
(rst-testcover-defcustom)

;; FIXME: Must consider other tabs:
;;        * Line blocks
;;        * Definition lists
;;        * Option lists
(defun rst-line-tabs ()
  "Return tabs of the current line or nil for no tab.
The list is sorted so the tab where writing continues most likely
is the first one.  Each tab is of the form (COLUMN . INNER).
COLUMN is the column of the tab.  INNER is non-nil if this is an
inner tab.  I.e. a tab which does come from the basic indentation
and not from inner alignment points."
  (save-excursion
    (forward-line 0)
    (save-match-data
      (unless (looking-at (rst-re 'lin-end))
	(back-to-indentation)
	;; Current indentation is always the least likely tab.
	(let ((tabs (list (list (point) 0 nil)))) ; (POINT OFFSET INNER)
	  ;; Push inner tabs more likely to continue writing.
	  (cond
	   ;; Item.
	   ((looking-at (rst-re '(:grp itmany-tag hws-sta) '(:grp "\\S ") "?"))
	    (when (match-string 2)
	      (push (list (match-beginning 2) 0 t) tabs)))
	   ;; Field.
	   ((looking-at (rst-re '(:grp fld-tag) '(:grp hws-tag)
				'(:grp "\\S ") "?"))
	    (unless (zerop rst-indent-field)
	      (push (list (match-beginning 1) rst-indent-field t) tabs))
	    (if (match-string 3)
		(push (list (match-beginning 3) 0 t) tabs)
	      (if (zerop rst-indent-field)
		  (push (list (match-end 2)
			      (if (string= (match-string 2) "") 1 0)
			      t) tabs))))
	   ;; Directive.
	   ((looking-at (rst-re 'dir-sta-3 '(:grp "\\S ") "?"))
	    (push (list (match-end 1) 0 t) tabs)
	    (unless (string= (match-string 2) "")
	      (push (list (match-end 2) 0 t) tabs))
	    (when (match-string 4)
	      (push (list (match-beginning 4) 0 t) tabs)))
	   ;; Footnote or citation definition.
	   ((looking-at (rst-re 'fnc-sta-2 '(:grp "\\S ") "?"))
	    (push (list (match-end 1) 0 t) tabs)
	    (when (match-string 3)
	      (push (list (match-beginning 3) 0 t) tabs)))
	   ;; Comment.
	   ((looking-at (rst-re 'cmt-sta-1))
	    (push (list (point) rst-indent-comment t) tabs)))
	  ;; Start of literal block.
	  (when (looking-at (rst-re 'lit-sta-2))
	    (let ((tab0 (first tabs)))
	      (push (list (first tab0)
			  (+ (second tab0)
			     (if (match-string 1)
				 rst-indent-literal-minimized
			       rst-indent-literal-normal))
			  t) tabs)))
	  (mapcar (lambda (tab)
		    (goto-char (first tab))
		    (cons (+ (current-column) (second tab)) (third tab)))
		  tabs))))))

(defun rst-compute-tabs (pt)
  "Build the list of possible tabs for all lines above.
Search backwards from point PT to build the list of possible tabs.
Return a list of tabs sorted by likeliness to continue writing
like `rst-line-tabs'.  Nearer lines have generally a higher
likeliness than farther lines.  Return nil if no tab is found in
the text above."
  (save-excursion
    (goto-char pt)
    (let (leftmost ; Leftmost column found so far.
	  innermost ; Leftmost column for inner tab.
	  tablist)
      (while (and (zerop (forward-line -1))
		  (or (not leftmost)
		      (> leftmost 0)))
	(let* ((tabs (rst-line-tabs))
	       (leftcol (if tabs (apply 'min (mapcar 'car tabs)))))
	  (when tabs
	    ;; Consider only lines indented less or same if not INNERMOST.
	    (when (or (not leftmost)
		      (< leftcol leftmost)
		      (and (not innermost) (= leftcol leftmost)))
	      (dolist (tab tabs)
		(let ((inner (cdr tab))
		      (newcol (car tab)))
		  (when (and
			 (or
			  (and (not inner)
			       (or (not leftmost)
				   (< newcol leftmost)))
			  (and inner
			       (or (not innermost)
				   (< newcol innermost))))
			 (not (memq newcol tablist)))
		    (push newcol tablist))))
	      (setq innermost (if (rst-some (mapcar 'cdr tabs)) ; Has inner.
				  leftcol
				innermost))
	      (setq leftmost leftcol)))))
      (nreverse tablist))))

(defun rst-indent-line (&optional dflt)
  "Indent current line to next best reStructuredText tab.
The next best tab is taken from the tab list returned by
`rst-compute-tabs' which is used in a cyclic manner.  If the
current indentation does not end on a tab use the first one.  If
the current indentation is on a tab use the next tab.  This allows
a repeated use of \\[indent-for-tab-command] to cycle through all
possible tabs.  If no indentation is possible return `noindent' or
use DFLT.  Return the indentation indented to.  When point is in
indentation it ends up at its end.  Otherwise the point is kept
relative to the content."
  (let* ((pt (point-marker))
	 (cur (current-indentation))
	 (clm (current-column))
	 (tabs (rst-compute-tabs (point)))
	 (fnd (rst-position cur tabs))
	 ind)
    (if (and (not tabs) (not dflt))
	'noindent
      (if (not tabs)
	  (setq ind dflt)
	(if (not fnd)
	    (setq fnd 0)
	  (setq fnd (1+ fnd))
	  (if (>= fnd (length tabs))
	      (setq fnd 0)))
	(setq ind (nth fnd tabs)))
      (indent-line-to ind)
      (if (> clm cur)
	  (goto-char pt))
      (set-marker pt nil)
      ind)))

(defun rst-shift-region (beg end cnt)
  "Shift region BEG to END by CNT tabs.
Shift by one tab to the right (CNT > 0) or left (CNT < 0) or
remove all indentation (CNT = 0).  A tab is taken from the text
above.  If no suitable tab is found `rst-indent-width' is used."
  (interactive "r\np")
  (let ((tabs (sort (rst-compute-tabs beg) (lambda (x y) (<= x y))))
	(leftmostcol (rst-find-leftmost-column beg end)))
    (when (or (> leftmostcol 0) (> cnt 0))
      ;; Apply the indent.
      (indent-rigidly
       beg end
       (if (zerop cnt)
	   (- leftmostcol)
	 ;; Find the next tab after the leftmost column.
	 (let* ((cmp (if (> cnt 0) '> '<))
		(tabs (if (> cnt 0) tabs (reverse tabs)))
		(len (length tabs))
		(dir (rst-signum cnt)) ; Direction to take.
		(abs (abs cnt)) ; Absolute number of steps to take.
		;; Get the position of the first tab beyond leftmostcol.
		(fnd (lexical-let ((cmp cmp)
				   (leftmostcol leftmostcol)) ;; Create closure.
		       (rst-position-if (lambda (elt)
					  (funcall cmp elt leftmostcol))
					tabs)))
		;; Virtual position of tab.
		(pos (+ (or fnd len) (1- abs)))
		(tab (if (< pos len)
			 ;; Tab exists - use it.
			 (nth pos tabs)
		       ;; Column needs to be computed.
		       (let ((col (+ (or (car (last tabs)) leftmostcol)
				     ;; Base on last known column.
				     (* (- pos (1- len)) ; Distance left.
					dir ; Direction to take.
					rst-indent-width))))
			 (if (< col 0) 0 col)))))
	   (- tab leftmostcol)))))))

;; FIXME: A paragraph with an (incorrectly) indented second line is not filled
;;        correctly::
;;
;;          Some start
;;            continued wrong
(defun rst-adaptive-fill ()
  "Return fill prefix found at point.
Value for `adaptive-fill-function'."
  (let ((fnd (if (looking-at adaptive-fill-regexp)
		 (match-string-no-properties 0))))
    (if (save-match-data
	  (not (string-match comment-start-skip fnd)))
	;; An non-comment prefix is fine.
	fnd
      ;; Matches a comment - return whitespace instead.
      (make-string (-
		    (save-excursion
		      (goto-char (match-end 0))
		      (current-column))
		    (save-excursion
		      (goto-char (match-beginning 0))
		      (current-column))) ? ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Comments

(defun rst-comment-line-break (&optional soft)
  "Break line and indent, continuing reStructuredText comment if within one.
Value for `comment-line-break-function'.  If SOFT use soft
newlines as mandated by `comment-line-break-function'."
  (if soft
      (insert-and-inherit ?\n)
    (newline 1))
  (save-excursion
    (forward-char -1)
    (delete-horizontal-space))
  (delete-horizontal-space)
  (let ((tabs (rst-compute-tabs (point))))
    (when tabs
      (indent-line-to (car tabs)))))

(defun rst-comment-indent ()
  "Return indentation for current comment line."
  (car (rst-compute-tabs (point))))

(defun rst-comment-insert-comment ()
  "Insert a comment in the current line."
  (rst-indent-line 0)
  (insert comment-start))

(defun rst-comment-region (beg end &optional arg)
  "Comment or uncomment the current region.
Region is from BEG to END.  Uncomment if ARG."
  (save-excursion
    (if (consp arg)
	(rst-uncomment-region beg end arg)
      (goto-char beg)
      (let ((ind (current-indentation))
	    bol)
	(forward-line 0)
	(setq bol (point))
	(indent-rigidly bol end rst-indent-comment)
	(goto-char bol)
	(open-line 1)
	(indent-line-to ind)
	(insert (comment-string-strip comment-start t t))))))

(defun rst-uncomment-region (beg end &optional _arg)
  "Uncomment the current region.
Region is from BEG to END.  _ARG is ignored"
  (save-excursion
    (let (bol eol)
      (goto-char beg)
      (forward-line 0)
      (setq bol (point))
      (forward-line 1)
      (setq eol (point))
      (indent-rigidly eol end (- rst-indent-comment))
      (delete-region bol eol))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Apply to indented block

;; FIXME: These next functions should become part of a larger effort to redo
;;        the bullets in bulleted lists.  The enumerate would just be one of
;;        the possible outputs.
;;
;; FIXME: We need to do the enumeration removal as well.

(defun rst-apply-indented-blocks (beg end ind fun)
  "Apply FUN to all lines from BEG to END in blocks indented to IND.
The first indented block starts with the first non-empty line
containing or after BEG and indented to IND.  After the first
line the indented block may contain more lines with same
indentation (the paragraph) followed by empty lines and lines
more indented (the sub-blocks).  A following line indented to IND
starts the next indented block.  A line with less indentation
than IND terminates the current indented block.  Such lines and
all following lines not indented to IND are skipped.  FUN is
applied to unskipped lines like this

  (FUN COUNT FIRSTP SUBP EMPTYP RELIND LASTRET)

COUNT is 0 before the first indented block and increments for
every indented block found.

FIRSTP is t when this is the first line of the paragraph.

SUBP is t when this line is part of a sub-block.

EMPTYP is t when this line is empty.

RELIND is nil for an empty line, 0 for a line indented to IND,
and the number of columns more indented otherwise.

LASTRET is the return value of FUN returned by the last
invocation for the same indented block or nil for the first
invocation.

When FUN is called point is immediately behind indentation of
that line.  FUN may change everything as long as a marker at END
is handled correctly by the change.

Return the return value of the last invocation of FUN or nil if
FUN was never called."
  (let (lastret
	subp
	skipping
	nextm
	(count 0) ; Before first indented block
	(endm (copy-marker end t)))
    (save-excursion
      (goto-char beg)
      (while (< (point) endm)
	(save-excursion
	  (setq nextm (save-excursion
			(forward-line 1)
			(copy-marker (point) t)))
	  (back-to-indentation)
	  (let (firstp
		emptyp
		(relind (- (current-column) ind)))
	    (cond
	     ((looking-at (rst-re 'lin-end))
	      (setq emptyp t)
	      (setq relind nil)
	      ;; Breaks indented block if one is started
	      (setq subp (not (zerop count))))
	     ((< relind 0) ; Less indented
	      (setq skipping t))
	     ((zerop relind) ; In indented block
	      (when (or subp skipping (zerop count))
		(setq firstp t)
		(incf count))
	      (setq subp nil)
	      (setq skipping nil))
	     (t ; More indented
	      (setq subp t)))
	    (unless skipping
	      (setq lastret
		    (funcall fun count firstp subp emptyp relind lastret)))))
	(goto-char nextm))
      lastret)))

(defun rst-enumerate-region (beg end all)
  "Add enumeration to all the leftmost paragraphs in the given region.
The region is specified between BEG and END.  With ALL,
do all lines instead of just paragraphs."
  (interactive "r\nP")
  (let ((enum 0))
    (rst-apply-indented-blocks
     beg end (rst-find-leftmost-column beg end)
     (lambda (count firstp subp emptyp relind lastret)
       (cond
	(emptyp)
	((zerop count))
	(subp
	 (insert lastret))
	((or firstp all)
	 (let ((ins (format "%d. " (incf enum))))
	   (setq lastret (make-string (length ins) ?\ ))
	   (insert ins)))
	(t
	 (insert lastret)))
       lastret))))

;; FIXME: Does not deal with deeper indentation - although
;;        `rst-apply-indented-blocks' could.
(defun rst-bullet-list-region (beg end all)
  "Add bullets to all the leftmost paragraphs in the given region.
The region is specified between BEG and END.  With ALL,
do all lines instead of just paragraphs."
  (interactive "r\nP")
  (unless rst-preferred-bullets
    (error "No preferred bullets defined"))
  (let ((bul (format "%c " (car rst-preferred-bullets)))
	(cont "  "))
    (rst-apply-indented-blocks
     beg end (rst-find-leftmost-column beg end)
     (lambda (count firstp subp emptyp relind lastret)
       (cond
	(emptyp)
	((zerop count))
	(subp
	 (insert cont))
	((or firstp all)
	 (insert bul))
	(t
	 (insert cont)))
       nil))))

;; FIXME: Does not deal with a varying number of digits appropriately.
;; FIXME: Does not deal with multiple levels independently.
;; FIXME: Does not indent a multiline item correctly.
(defun rst-convert-bullets-to-enumeration (beg end)
  "Convert the bulleted and enumerated items in the region to enumerated lists.
Renumber as necessary.  Region is from BEG to END."
  (interactive "r")
  (let* (;; Find items and convert the positions to markers.
	 (items (mapcar
		 (lambda (x)
		   (cons (copy-marker (car x))
			 (cdr x)))
		 (rst-find-pfx-in-region beg end (rst-re 'itmany-sta-1))))
	 (count 1))
    (save-excursion
      (dolist (x items)
	(goto-char (car x))
	(looking-at (rst-re 'itmany-beg-1))
	(replace-match (format "%d." count) nil nil nil 1)
	(incf count)))))

(defun rst-line-block-region (beg end &optional with-empty)
  "Add line block prefixes for a region.
Region is from BEG to END.  With WITH-EMPTY prefix empty lines too."
  (interactive "r\nP")
  (let ((ind (rst-find-leftmost-column beg end)))
    (rst-apply-indented-blocks
     beg end ind
     (lambda (count firstp subp emptyp relind lastret)
       (when (or with-empty (not emptyp))
	 (move-to-column ind t)
	 (insert "| "))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Font lock

(require 'font-lock)

;; FIXME: The obsolete variables need to disappear.

;; The following versions have been done inside Emacs and should not be
;; replaced by `:package-version' attributes until a change.

(defgroup rst-faces nil "Faces used in Rst Mode."
  :group 'rst
  :group 'faces
  :version "21.1")

(defface rst-block '((t :inherit font-lock-keyword-face))
  "Face used for all syntax marking up a special block."
  :version "24.1"
  :group 'rst-faces)

(defcustom rst-block-face 'rst-block
  "All syntax marking up a special block."
  :version "24.1"
  :group 'rst-faces
  :type '(face))
(rst-testcover-defcustom)
(make-obsolete-variable 'rst-block-face
                        "customize the face `rst-block' instead."
                        "24.1")

(defface rst-external '((t :inherit font-lock-type-face))
  "Face used for field names and interpreted text."
  :version "24.1"
  :group 'rst-faces)

(defcustom rst-external-face 'rst-external
  "Field names and interpreted text."
  :version "24.1"
  :group 'rst-faces
  :type '(face))
(rst-testcover-defcustom)
(make-obsolete-variable 'rst-external-face
                        "customize the face `rst-external' instead."
                        "24.1")

(defface rst-definition '((t :inherit font-lock-function-name-face))
  "Face used for all other defining constructs."
  :version "24.1"
  :group 'rst-faces)

(defcustom rst-definition-face 'rst-definition
  "All other defining constructs."
  :version "24.1"
  :group 'rst-faces
  :type '(face))
(rst-testcover-defcustom)
(make-obsolete-variable 'rst-definition-face
                        "customize the face `rst-definition' instead."
                        "24.1")

;; XEmacs compatibility (?).
(defface rst-directive (if (boundp 'font-lock-builtin-face)
                           '((t :inherit font-lock-builtin-face))
                         '((t :inherit font-lock-preprocessor-face)))
  "Face used for directives and roles."
  :version "24.1"
  :group 'rst-faces)

(defcustom rst-directive-face 'rst-directive
  "Directives and roles."
  :group 'rst-faces
  :type '(face))
(rst-testcover-defcustom)
(make-obsolete-variable 'rst-directive-face
                        "customize the face `rst-directive' instead."
                        "24.1")

(defface rst-comment '((t :inherit font-lock-comment-face))
  "Face used for comments."
  :version "24.1"
  :group 'rst-faces)

(defcustom rst-comment-face 'rst-comment
  "Comments."
  :version "24.1"
  :group 'rst-faces
  :type '(face))
(rst-testcover-defcustom)
(make-obsolete-variable 'rst-comment-face
                        "customize the face `rst-comment' instead."
                        "24.1")

(defface rst-emphasis1 '((t :inherit italic))
  "Face used for simple emphasis."
  :version "24.1"
  :group 'rst-faces)

(defcustom rst-emphasis1-face 'rst-emphasis1
  "Simple emphasis."
  :version "24.1"
  :group 'rst-faces
  :type '(face))
(rst-testcover-defcustom)
(make-obsolete-variable 'rst-emphasis1-face
                        "customize the face `rst-emphasis1' instead."
                        "24.1")

(defface rst-emphasis2 '((t :inherit bold))
  "Face used for double emphasis."
  :version "24.1"
  :group 'rst-faces)

(defcustom rst-emphasis2-face 'rst-emphasis2
  "Double emphasis."
  :group 'rst-faces
  :type '(face))
(rst-testcover-defcustom)
(make-obsolete-variable 'rst-emphasis2-face
                        "customize the face `rst-emphasis2' instead."
                        "24.1")

(defface rst-literal '((t :inherit font-lock-string-face))
  "Face used for literal text."
  :version "24.1"
  :group 'rst-faces)

(defcustom rst-literal-face 'rst-literal
  "Literal text."
  :version "24.1"
  :group 'rst-faces
  :type '(face))
(rst-testcover-defcustom)
(make-obsolete-variable 'rst-literal-face
                        "customize the face `rst-literal' instead."
                        "24.1")

(defface rst-reference '((t :inherit font-lock-variable-name-face))
  "Face used for references to a definition."
  :version "24.1"
  :group 'rst-faces)

(defcustom rst-reference-face 'rst-reference
  "References to a definition."
  :version "24.1"
  :group 'rst-faces
  :type '(face))
(rst-testcover-defcustom)
(make-obsolete-variable 'rst-reference-face
                        "customize the face `rst-reference' instead."
                        "24.1")

(defface rst-transition '((t :inherit font-lock-keyword-face))
  "Face used for a transition."
  :package-version '(rst . "1.3.0")
  :group 'rst-faces)

(defface rst-adornment '((t :inherit font-lock-keyword-face))
  "Face used for the adornment of a section header."
  :package-version '(rst . "1.3.0")
  :group 'rst-faces)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(dolist (var '(rst-level-face-max rst-level-face-base-color
				  rst-level-face-base-light
				  rst-level-face-format-light
				  rst-level-face-step-light
				  rst-level-1-face
				  rst-level-2-face
				  rst-level-3-face
				  rst-level-4-face
				  rst-level-5-face
				  rst-level-6-face))
  (make-obsolete-variable var "customize the faces `rst-level-*' instead."
			  "24.3"))

;; Define faces for the first 6 levels. More levels are possible, however.
(defface rst-level-1 '((((background light)) (:background "grey85"))
		       (((background dark)) (:background "grey15")))
  "Default face for section title text at level 1."
  :package-version '(rst . "1.4.0"))

(defface rst-level-2 '((((background light)) (:background "grey78"))
		       (((background dark)) (:background "grey22")))
  "Default face for section title text at level 2."
  :package-version '(rst . "1.4.0"))

(defface rst-level-3 '((((background light)) (:background "grey71"))
		       (((background dark)) (:background "grey29")))
  "Default face for section title text at level 3."
  :package-version '(rst . "1.4.0"))

(defface rst-level-4 '((((background light)) (:background "grey64"))
		       (((background dark)) (:background "grey36")))
  "Default face for section title text at level 4."
  :package-version '(rst . "1.4.0"))

(defface rst-level-5 '((((background light)) (:background "grey57"))
		       (((background dark)) (:background "grey43")))
  "Default face for section title text at level 5."
  :package-version '(rst . "1.4.0"))

(defface rst-level-6 '((((background light)) (:background "grey50"))
		       (((background dark)) (:background "grey50")))
  "Default face for section title text at level 6."
  :package-version '(rst . "1.4.0"))

(defcustom rst-adornment-faces-alist
  '((t . rst-transition)
    (nil . rst-adornment)
    (1 . rst-level-1)
    (2 . rst-level-2)
    (3 . rst-level-3)
    (4 . rst-level-4)
    (5 . rst-level-5)
    (6 . rst-level-6))
    "Faces for the various adornment types.
Key is a number (for the section title text of that level
starting with 1), t (for transitions) or nil (for section title
adornment).  If you need levels beyond 6 you have to define faces
of your own."
  :group 'rst-faces
  :type '(alist
	  :key-type
	  (choice
	   (integer :tag "Section level")
	   (const :tag "transitions" t)
	   (const :tag "section title adornment" nil))
	  :value-type (face)))
(rst-testcover-defcustom)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar rst-font-lock-keywords
  ;; The reST-links in the comments below all relate to sections in
  ;; http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html.
  `(;; FIXME: Block markup is not recognized in blocks after explicit markup
    ;;        start.

    ;; Simple `Body Elements`_
    ;; `Bullet Lists`_
    ;; FIXME: A bullet directly after a field name is not recognized.
    (,(rst-re 'lin-beg '(:grp bul-sta))
     1 rst-block-face)
    ;; `Enumerated Lists`_
    (,(rst-re 'lin-beg '(:grp enmany-sta))
     1 rst-block-face)
    ;; `Definition Lists`_
    ;; FIXME: missing.
    ;; `Field Lists`_
    (,(rst-re 'lin-beg '(:grp fld-tag) 'bli-sfx)
     1 rst-external-face)
    ;; `Option Lists`_
    (,(rst-re 'lin-beg '(:grp opt-tag (:shy optsep-tag opt-tag) "*")
	      '(:alt "$" (:seq hws-prt "\\{2\\}")))
     1 rst-block-face)
    ;; `Line Blocks`_
    ;; Only for lines containing no more bar - to distinguish from tables.
    (,(rst-re 'lin-beg '(:grp "|" bli-sfx) "[^|\n]*$")
     1 rst-block-face)

    ;; `Tables`_
    ;; FIXME: missing

    ;; All the `Explicit Markup Blocks`_
    ;; `Footnotes`_ / `Citations`_
    (,(rst-re 'lin-beg 'fnc-sta-2)
     (1 rst-definition-face)
     (2 rst-definition-face))
    ;; `Directives`_ / `Substitution Definitions`_
    (,(rst-re 'lin-beg 'dir-sta-3)
     (1 rst-directive-face)
     (2 rst-definition-face)
     (3 rst-directive-face))
    ;; `Hyperlink Targets`_
    (,(rst-re 'lin-beg
	      '(:grp exm-sta "_" (:alt
				  (:seq "`" ilcbkqdef-tag "`")
				  (:seq (:alt "[^:\\\n]" "\\\\.") "+")) ":")
	      'bli-sfx)
     1 rst-definition-face)
    (,(rst-re 'lin-beg '(:grp "__") 'bli-sfx)
     1 rst-definition-face)

    ;; All `Inline Markup`_
    ;; Most of them may be multiline though this is uninteresting.

    ;; FIXME: Condition 5 preventing fontification of e.g. "*" not implemented
    ;;        `Strong Emphasis`_.
    (,(rst-re 'ilm-pfx '(:grp "\\*\\*" ilcast-tag "\\*\\*") 'ilm-sfx)
     1 rst-emphasis2-face)
    ;; `Emphasis`_
    (,(rst-re 'ilm-pfx '(:grp "\\*" ilcast-tag "\\*") 'ilm-sfx)
     1 rst-emphasis1-face)
    ;; `Inline Literals`_
    (,(rst-re 'ilm-pfx '(:grp "``" ilcbkq-tag "``") 'ilm-sfx)
     1 rst-literal-face)
    ;; `Inline Internal Targets`_
    (,(rst-re 'ilm-pfx '(:grp "_`" ilcbkq-tag "`") 'ilm-sfx)
     1 rst-definition-face)
    ;; `Hyperlink References`_
    ;; FIXME: `Embedded URIs and Aliases`_ not considered.
    ;; FIXME: Directly adjacent marked up words are not fontified correctly
    ;;        unless they are not separated by two spaces: foo_ bar_.
    (,(rst-re 'ilm-pfx '(:grp (:alt (:seq "`" ilcbkq-tag "`")
				    (:seq "\\sw" (:alt "\\sw" "-") "+\\sw"))
			      "__?") 'ilm-sfx)
     1 rst-reference-face)
    ;; `Interpreted Text`_
    (,(rst-re 'ilm-pfx '(:grp (:shy ":" sym-tag ":") "?")
	      '(:grp "`" ilcbkq-tag "`")
	      '(:grp (:shy ":" sym-tag ":") "?") 'ilm-sfx)
     (1 rst-directive-face)
     (2 rst-external-face)
     (3 rst-directive-face))
    ;; `Footnote References`_ / `Citation References`_
    (,(rst-re 'ilm-pfx '(:grp fnc-tag "_") 'ilm-sfx)
     1 rst-reference-face)
    ;; `Substitution References`_
    ;; FIXME: References substitutions like |this|_ or |this|__ are not
    ;;        fontified correctly.
    (,(rst-re 'ilm-pfx '(:grp sub-tag) 'ilm-sfx)
     1 rst-reference-face)
    ;; `Standalone Hyperlinks`_
    ;; FIXME: This takes it easy by using a whitespace as delimiter.
    (,(rst-re 'ilm-pfx '(:grp uri-tag ":\\S +") 'ilm-sfx)
     1 rst-definition-face)
    (,(rst-re 'ilm-pfx '(:grp sym-tag "@" sym-tag ) 'ilm-sfx)
     1 rst-definition-face)

    ;; Do all block fontification as late as possible so 'append works.

    ;; Sections_ / Transitions_
    ;; For sections this is multiline.
    (,(rst-re 'ado-beg-2-1)
     (rst-font-lock-handle-adornment-matcher
      (rst-font-lock-handle-adornment-pre-match-form
       (match-string-no-properties 1) (match-end 1))
      nil
      (1 (cdr (assoc nil rst-adornment-faces-alist)) append t)
      (2 (cdr (assoc rst-font-lock-adornment-level
		     rst-adornment-faces-alist)) append t)
      (3 (cdr (assoc nil rst-adornment-faces-alist)) append t)))

    ;; FIXME: FACESPEC could be used instead of ordinary faces to set
    ;;        properties on comments and literal blocks so they are *not*
    ;;        inline fontified.  See (elisp)Search-based Fontification.

    ;; FIXME: And / or use `syntax-propertize' functions as in `octave-mod.el'
    ;;        and other V24 modes.  May make `font-lock-extend-region'
    ;;        superfluous.

    ;; `Comments`_
    ;; This is multiline.
    (,(rst-re 'lin-beg 'cmt-sta-1)
     (1 rst-comment-face)
     (rst-font-lock-find-unindented-line-match
      (rst-font-lock-find-unindented-line-limit (match-end 1))
      nil
      (0 rst-comment-face append)))
    (,(rst-re 'lin-beg '(:grp exm-tag) '(:grp hws-tag) "$")
     (1 rst-comment-face)
     (2 rst-comment-face)
     (rst-font-lock-find-unindented-line-match
      (rst-font-lock-find-unindented-line-limit 'next)
      nil
      (0 rst-comment-face append)))

    ;; FIXME: This is not rendered as comment::
    ;;        .. .. list-table::
    ;;              :stub-columns: 1
    ;;              :header-rows: 1

    ;; FIXME: This is rendered wrong::
    ;;
    ;; 	 xxx yyy::
    ;;
    ;; 	 			----|> KKKKK <|----
    ;; 	 		       /	     	    \
    ;; 	    -|> AAAAAAAAAAPPPPPP <|-   	       	 -|> AAAAAAAAAABBBBBBB <|-
    ;; 	    |			   |	     	 |     	       	       	 |
    ;; 	    |			   |		 |			 |
    ;; 	    PPPPPP     PPPPPPDDDDDDD             BBBBBBB     PPPPPPBBBBBBB
    ;;
    ;; Indentation needs to be taken from the line with the ``::`` and not from
    ;; the first content line.

    ;; `Indented Literal Blocks`_
    ;; This is multiline.
    (,(rst-re 'lin-beg 'lit-sta-2)
     (2 rst-block-face)
     (rst-font-lock-find-unindented-line-match
      (rst-font-lock-find-unindented-line-limit t)
      nil
      (0 rst-literal-face append)))

    ;; FIXME: `Quoted Literal Blocks`_ missing.
    ;; This is multiline.

    ;; `Doctest Blocks`_
    ;; FIXME: This is wrong according to the specification:
    ;;
    ;;   Doctest blocks are text blocks which begin with ">>> ", the Python
    ;;   interactive interpreter main prompt, and end with a blank line.
    ;;   Doctest blocks are treated as a special case of literal blocks,
    ;;   without requiring the literal block syntax. If both are present, the
    ;;   literal block syntax takes priority over Doctest block syntax:
    ;;
    ;;   This is an ordinary paragraph.
    ;;
    ;;   >>> print 'this is a Doctest block'
    ;;   this is a Doctest block
    ;;
    ;;   The following is a literal block::
    ;;
    ;;       >>> This is not recognized as a doctest block by
    ;;       reStructuredText.  It *will* be recognized by the doctest
    ;;       module, though!
    ;;
    ;;   Indentation is not required for doctest blocks.
    (,(rst-re 'lin-beg '(:grp (:alt ">>>" ell-tag)) '(:grp ".+"))
     (1 rst-block-face)
     (2 rst-literal-face)))
  "Keywords to highlight in rst mode.")

(defvar font-lock-beg)
(defvar font-lock-end)

(defun rst-font-lock-extend-region ()
  "Extend the font-lock region if it might be in a multi-line construct.
Return non-nil if so.  Font-lock region is from `font-lock-beg'
to `font-lock-end'."
  (let ((r (rst-font-lock-extend-region-internal font-lock-beg font-lock-end)))
    (when r
      (setq font-lock-beg (car r))
      (setq font-lock-end (cdr r))
      t)))

(defun rst-font-lock-extend-region-internal (beg end)
  "Check the region BEG / END for being in the middle of a multi-line construct.
Return nil if not or a cons with new values for BEG / END"
  (let ((nbeg (rst-font-lock-extend-region-extend beg -1))
	(nend (rst-font-lock-extend-region-extend end 1)))
    (if (or nbeg nend)
	(cons (or nbeg beg) (or nend end)))))

(defun rst-forward-line (&optional n)
  "Like `forward-line' but always end up in column 0 and return accordingly.
Move N lines forward just as `forward-line'."
  (let ((moved (forward-line n)))
    (if (bolp)
	moved
      (forward-line 0)
      (- moved (rst-signum n)))))

;; FIXME: If a single line is made a section header by `rst-adjust' the header
;;        is not always fontified immediately.
(defun rst-font-lock-extend-region-extend (pt dir)
  "Extend the region starting at point PT and extending in direction DIR.
Return extended point or nil if not moved."
  ;; There are many potential multiline constructs but there are two groups
  ;; which are really relevant. The first group consists of
  ;;
  ;; * comment lines without leading explicit markup tag and
  ;;
  ;; * literal blocks following "::"
  ;;
  ;; which are both indented. Thus indentation is the first thing recognized
  ;; here. The second criteria is an explicit markup tag which may be a comment
  ;; or a double colon at the end of a line.
  ;;
  ;; The second group consists of the adornment cases.
  (if (not (get-text-property pt 'font-lock-multiline))
      ;; Move only if we don't start inside a multiline construct already.
      (save-excursion
	(let (;; Non-empty non-indented line, explicit markup tag or literal
	      ;; block tag.
	      (stop-re (rst-re '(:alt "[^ \t\n]"
				      (:seq hws-tag exm-tag)
				      (:seq ".*" dcl-tag lin-end)))))
	  ;; The comments below are for dir == -1 / dir == 1.
	  (goto-char pt)
	  (forward-line 0)
	  (setq pt (point))
	  (while (and (not (looking-at stop-re))
		      (zerop (rst-forward-line dir)))) ; try previous / next
						       ; line if it exists.
	  (if (looking-at (rst-re 'ado-beg-2-1)) ; may be an underline /
						 ; overline.
	      (if (zerop (rst-forward-line dir))
		  (if (looking-at (rst-re 'ttl-beg-1)) ; title found, i.e.
						       ; underline / overline
						       ; found.
		      (if (zerop (rst-forward-line dir))
			  (if (not
			       (looking-at (rst-re 'ado-beg-2-1))) ; no
								   ; overline /
								   ; underline.
			      (rst-forward-line (- dir)))) ; step back to title
							   ; / adornment.
		    (if (< dir 0) ; keep downward adornment.
			(rst-forward-line (- dir))))) ; step back to adornment.
	    (if (looking-at (rst-re 'ttl-beg-1)) ; may be a title.
		(if (zerop (rst-forward-line dir))
		    (if (not
			 (looking-at (rst-re 'ado-beg-2-1))) ; no overline /
							     ; underline.
			(rst-forward-line (- dir)))))) ; step back to line.
	  (if (not (= (point) pt))
	      (point))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indented blocks

(defun rst-forward-indented-block (&optional column limit)
  "Move forward across one indented block.
Find the next non-empty line which is not indented at least to COLUMN (defaults
to the column of the point).  Moves point to first character of this line or the
first empty line immediately before it and returns that position.  If there is
no such line before LIMIT (defaults to the end of the buffer) returns nil and
point is not moved."
  (interactive)
  (let ((clm (or column (current-column)))
	(start (point))
	fnd beg cand)
    (if (not limit)
	(setq limit (point-max)))
    (save-match-data
      (while (and (not fnd) (< (point) limit))
	(forward-line 1)
	(when (< (point) limit)
	  (setq beg (point))
	  (if (looking-at (rst-re 'lin-end))
	      (setq cand (or cand beg)) ; An empty line is a candidate.
	    (move-to-column clm)
	    ;; FIXME: No indentation [(zerop clm)] must be handled in some
	    ;;        useful way - though it is not clear what this should mean
	    ;;        at all.
	    (if (string-match
		 (rst-re 'linemp-tag)
		 (buffer-substring-no-properties beg (point)))
		(setq cand nil) ; An indented line resets a candidate.
	      (setq fnd (or cand beg)))))))
    (goto-char (or fnd start))
    fnd))

(defvar rst-font-lock-find-unindented-line-begin nil
  "Beginning of the match if `rst-font-lock-find-unindented-line-end'.")

(defvar rst-font-lock-find-unindented-line-end nil
  "End of the match as determined by `rst-font-lock-find-unindented-line-limit'.
Also used as a trigger for `rst-font-lock-find-unindented-line-match'.")

(defun rst-font-lock-find-unindented-line-limit (ind-pnt)
  "Find the next unindented line relative to indentation at IND-PNT.
Return this point, the end of the buffer or nil if nothing found.
If IND-PNT is `next' take the indentation from the next line if
this is not empty and indented more than the current one.  If
IND-PNT is non-nil but not a number take the indentation from the
next non-empty line if this is indented more than the current one."
  (setq rst-font-lock-find-unindented-line-begin ind-pnt)
  (setq rst-font-lock-find-unindented-line-end
	(save-excursion
	  (when (not (numberp ind-pnt))
	    ;; Find indentation point in next line if any.
	    (setq ind-pnt
		  ;; FIXME: Should be refactored to two different functions
		  ;;        giving their result to this function, may be
		  ;;        integrated in caller.
		  (save-match-data
		    (let ((cur-ind (current-indentation)))
		      (if (eq ind-pnt 'next)
			  (when (and (zerop (forward-line 1))
				     (< (point) (point-max)))
			    ;; Not at EOF.
			    (setq rst-font-lock-find-unindented-line-begin
				  (point))
			    (when (and (not (looking-at (rst-re 'lin-end)))
				       (> (current-indentation) cur-ind))
			        ;; Use end of indentation if non-empty line.
				(looking-at (rst-re 'hws-tag))
				(match-end 0)))
			;; Skip until non-empty line or EOF.
			(while (and (zerop (forward-line 1))
				    (< (point) (point-max))
				    (looking-at (rst-re 'lin-end))))
			(when (< (point) (point-max))
			  ;; Not at EOF.
			  (setq rst-font-lock-find-unindented-line-begin
				(point))
			  (when (> (current-indentation) cur-ind)
			    ;; Indentation bigger than line of departure.
			    (looking-at (rst-re 'hws-tag))
			    (match-end 0))))))))
	  (when ind-pnt
	    (goto-char ind-pnt)
	    (or (rst-forward-indented-block nil (point-max))
		(point-max))))))

(defun rst-font-lock-find-unindented-line-match (_limit)
  "Set the match found earlier if match were found.
Match has been found by `rst-font-lock-find-unindented-line-limit'
the first time called or no match is found.  Return non-nil if
match was found.  _LIMIT is not used but mandated by the caller."
  (when rst-font-lock-find-unindented-line-end
    (set-match-data
     (list rst-font-lock-find-unindented-line-begin
	   rst-font-lock-find-unindented-line-end))
    (put-text-property rst-font-lock-find-unindented-line-begin
		       rst-font-lock-find-unindented-line-end
		       'font-lock-multiline t)
    ;; Make sure this is called only once.
    (setq rst-font-lock-find-unindented-line-end nil)
    t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Adornments

(defvar rst-font-lock-adornment-level nil
  "Storage for `rst-font-lock-handle-adornment-matcher'.
Either section level of the current adornment or t for a transition.")

(defun rst-adornment-level (ado)
  "Return section level for ADO or t for a transition.
If ADO is found in the hierarchy return its level.  Otherwise
return a level one beyond the existing hierarchy."
  (if (rst-Ado-is-transition ado)
      t
    (let ((hier (rst-Hdr-ado-map (rst-hdr-hierarchy))))
      (1+ (or (rst-Ado-position ado hier)
	      (length hier))))))

(defvar rst-font-lock-adornment-match nil
  "Storage for match for current adornment.
Set by `rst-font-lock-handle-adornment-pre-match-form'.  Also used
as a trigger for `rst-font-lock-handle-adornment-matcher'.")

(defun rst-font-lock-handle-adornment-pre-match-form (ado ado-end)
  "Determine limit for adornments.
Determine all things necessary for font-locking section titles
and transitions and put the result to `rst-font-lock-adornment-match'
and `rst-font-lock-adornment-level'.  ADO is the complete adornment
matched.  ADO-END is the point where ADO ends.  Return the point
where the whole adorned construct ends.

Called as a PRE-MATCH-FORM in the sense of `font-lock-keywords'."
  (let ((ttl (rst-classify-adornment ado ado-end)))
    (if (not ttl)
	(setq rst-font-lock-adornment-level nil
	      rst-font-lock-adornment-match nil)
      (setq rst-font-lock-adornment-level
	    (rst-adornment-level (rst-Ttl-ado ttl)))
      (setq rst-font-lock-adornment-match (rst-Ttl-match ttl))
      (goto-char (rst-Ttl-get-beginning ttl))
      (rst-Ttl-get-end ttl))))

(defun rst-font-lock-handle-adornment-matcher (_limit)
  "Set the match found earlier if match were found.
Match has been found by
`rst-font-lock-handle-adornment-pre-match-form' the first time
called or no match is found.  Return non-nil if match was found.

Called as a MATCHER in the sense of `font-lock-keywords'.
_LIMIT is not used but mandated by the caller."
  (let ((match rst-font-lock-adornment-match))
    ;; May run only once - enforce this.
    (setq rst-font-lock-adornment-match nil)
    (when match
      (set-match-data match)
      (goto-char (match-end 0))
      (put-text-property (match-beginning 0) (match-end 0)
			 'font-lock-multiline t)
      t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compilation

(defgroup rst-compile nil
  "Settings for support of conversion of reStructuredText
document with \\[rst-compile]."
  :group 'rst
  :version "21.1")

(defcustom rst-compile-toolsets
  `((html ,(if (executable-find "rst2html.py") "rst2html.py" "rst2html")
          ".html" nil)
    (latex ,(if (executable-find "rst2latex.py") "rst2latex.py" "rst2latex")
           ".tex" nil)
    (newlatex ,(if (executable-find "rst2newlatex.py") "rst2newlatex.py"
                 "rst2newlatex")
              ".tex" nil)
    (pseudoxml ,(if (executable-find "rst2pseudoxml.py") "rst2pseudoxml.py"
                  "rst2pseudoxml")
               ".xml" nil)
    (xml ,(if (executable-find "rst2xml.py") "rst2xml.py" "rst2xml")
         ".xml" nil)
    (pdf ,(if (executable-find "rst2pdf.py") "rst2pdf.py" "rst2pdf")
         ".pdf" nil)
    (s5 ,(if (executable-find "rst2s5.py") "rst2s5.py" "rst2s5")
        ".html" nil))
  ;; FIXME: Add at least those converters officially supported like `rst2odt'
  ;;        and `rst2man'.
  ;; FIXME: To make this really useful there should be a generic command the
  ;;        user gives one of the symbols and this way select the conversion to
  ;;        run. This should replace the toolset stuff somehow.
  ;; FIXME: Allow a template for the conversion command so `rst2pdf ... -o ...'
  ;;        can be supported.
  "Table describing the command to use for each tool-set.
An association list of the tool-set to a list of the (command to use,
extension of produced filename, options to the tool (nil or a
string)) to be used for converting the document."
  ;; FIXME: These are not options but symbols which may be referenced by
  ;;        `rst-compile-*-toolset` below. The `:validate' keyword of
  ;;        `defcustom' may help to define this properly in newer Emacs
  ;;        versions (> 23.1).
  :type '(alist :options (html latex newlatex pseudoxml xml pdf s5)
                :key-type symbol
                :value-type (list :tag "Specification"
                             (file :tag "Command")
                             (string :tag "File extension")
                             (choice :tag "Command options"
                                     (const :tag "No options" nil)
                                     (string :tag "Options"))))
  :group 'rst-compile
  :package-version "1.2.0")
(rst-testcover-defcustom)

;; FIXME: Must be defcustom.
(defvar rst-compile-primary-toolset 'html
  "The default tool-set for `rst-compile'.")

;; FIXME: Must be defcustom.
(defvar rst-compile-secondary-toolset 'latex
  "The default tool-set for `rst-compile' with a prefix argument.")

(defun rst-compile-find-conf ()
  "Look for the configuration file in the parents of the current path."
  (interactive)
  (let ((file-name "docutils.conf")
        (buffer-file (buffer-file-name)))
    ;; Move up in the dir hierarchy till we find a change log file.
    (let* ((dir (file-name-directory buffer-file))
	   (prevdir nil))
      (while (and (or (not (string= dir prevdir))
		      (setq dir nil)
		      nil)
                  (not (file-exists-p (concat dir file-name))))
        ;; Move up to the parent dir and try again.
	(setq prevdir dir)
        (setq dir (expand-file-name (file-name-directory
                                     (directory-file-name
				      (file-name-directory dir))))))
      (or (and dir (concat dir file-name)) nil))))

(require 'compile)

(defun rst-compile (&optional use-alt)
  "Compile command to convert reST document into some output file.
Attempts to find configuration file, if it can, overrides the
options.  There are two commands to choose from; with USE-ALT,
select the alternative tool-set."
  (interactive "P")
  ;; Note: maybe we want to check if there is a Makefile too and not do anything
  ;; if that is the case.  I dunno.
  (let* ((toolset (cdr (assq (if use-alt
				 rst-compile-secondary-toolset
			       rst-compile-primary-toolset)
			rst-compile-toolsets)))
         (command (car toolset))
         (extension (cadr toolset))
         (options (caddr toolset))
         (conffile (rst-compile-find-conf))
         (bufname (file-name-nondirectory buffer-file-name))
         (outname (file-name-sans-extension bufname)))

    ;; Set compile-command before invocation of compile.
    (setq-local
     compile-command
     (mapconcat 'identity
		(list command
		      (or options "")
		      (if conffile
			  (concat "--config=" (shell-quote-argument conffile))
			"")
		      (shell-quote-argument bufname)
		      (shell-quote-argument (concat outname extension)))
		" "))

    ;; Invoke the compile command.
    (if (or compilation-read-command use-alt)
        (call-interactively 'compile)
      (compile compile-command))))

(defun rst-compile-alt-toolset ()
  "Compile command with the alternative tool-set."
  (interactive)
  (rst-compile t))

(defun rst-compile-pseudo-region ()
  "Show pseudo-XML rendering.
Rendering is done of the current active region, or of the entire
buffer, if the region is not selected."
  ;; FIXME: The region should be given interactively.
  (interactive)
  (with-output-to-temp-buffer "*pseudoxml*"
    (shell-command-on-region
     (if mark-active (region-beginning) (point-min))
     (if mark-active (region-end) (point-max))
     (cadr (assq 'pseudoxml rst-compile-toolsets))
     standard-output)))

;; FIXME: Should be integrated in `rst-compile-toolsets'.
(defvar rst-pdf-program "xpdf"
  "Program used to preview PDF files.")

(defun rst-compile-pdf-preview ()
  "Convert the document to a PDF file and launch a preview program."
  (interactive)
  (let* ((tmp-filename (make-temp-file "rst_el" nil ".pdf"))
	 (command (format "%s %s %s && %s %s ; rm %s"
			  (cadr (assq 'pdf rst-compile-toolsets))
			  buffer-file-name tmp-filename
			  rst-pdf-program tmp-filename tmp-filename)))
    (start-process-shell-command "rst-pdf-preview" nil command)
    ;; Note: you could also use (compile command) to view the compilation
    ;; output.
    ))

;; FIXME: Should be integrated in `rst-compile-toolsets' defaulting to
;;        something like `browse-url'.
(defvar rst-slides-program "firefox"
  "Program used to preview S5 slides.")

(defun rst-compile-slides-preview ()
  "Convert the document to an S5 slide presentation and launch a preview program."
  (interactive)
  (let* ((tmp-filename (make-temp-file "rst_el" nil ".html"))
	 (command (format "%s %s %s && %s %s ; rm %s"
			  (cadr (assq 's5 rst-compile-toolsets))
			  buffer-file-name tmp-filename
			  rst-slides-program tmp-filename tmp-filename)))
    (start-process-shell-command "rst-slides-preview" nil command)
    ;; Note: you could also use (compile command) to view the compilation
    ;; output.
    ))

;; FIXME: Add `rst-compile-html-preview'.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Imenu support

;; FIXME: Consider a key binding. A key binding needs to definitely switch on
;;        `which-func-mode' - i.e. `which-func-modes' must be set properly.

;; Based on ideas from Masatake YAMATO <yamato@redhat.com>.

(defun rst-imenu-convert-cell (stn)
  "Convert a STN to an Imenu index node and return it."
  (let ((ttl (rst-Stn-ttl stn))
	(children (rst-Stn-children stn))
	(pos (rst-Stn-get-title-beginning stn))
	(txt (rst-Stn-get-text stn ""))
	(pfx " ")
	(sfx "")
	name)
    (when ttl
      (let ((hdr (rst-Ttl-hdr ttl)))
	(setq pfx (char-to-string (rst-Hdr-get-char hdr)))
	(when (rst-Hdr-is-over-and-under hdr)
	  (setq sfx pfx))))
    ;; FIXME: Overline adornment characters need to be in front so they
    ;;        become visible even for long title lines. May be an additional
    ;;        level number is also useful.
    (setq name (format "%s%s%s" pfx txt sfx))
    (cons name ;; The name of the entry.
	  (if children
	      (cons ;; The entry has a submenu.
	       (cons name pos) ;; The entry itself.
	       (mapcar 'rst-imenu-convert-cell children)) ;; The children.
	    pos)))) ;; The position of a plain entry.

;; FIXME: Document title and subtitle need to be handled properly. They should
;;        get an own "Document" top level entry.
(defun rst-imenu-create-index ()
  "Create index for Imenu.
Return as described for `imenu--index-alist'."
  (rst-reset-section-caches)
  (let ((root (rst-all-stn)))
    (when root
      (mapcar 'rst-imenu-convert-cell (rst-Stn-children root)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convenience functions

;; FIXME: Unbound command - should be bound or removed.
(defun rst-replace-lines (fromchar tochar)
  "Replace flush-left lines of FROMCHAR with equal-length lines of TOCHAR."
  (interactive "\
cSearch for flush-left lines of char:
cand replace with char: ")
  (save-excursion
    (let ((searchre (rst-re "^" fromchar "+\\( *\\)$"))
          (found 0))
      (while (search-forward-regexp searchre nil t)
        (setq found (1+ found))
        (goto-char (match-beginning 1))
        (let ((width (current-column)))
          (rst-delete-entire-line)
          (insert-char tochar width)))
      (message "%d lines replaced." found))))

;; FIXME: Unbound command - should be bound or removed.
(defun rst-join-paragraph ()
  "Join lines in current paragraph into one line, removing end-of-lines."
  (interactive)
  (let ((fill-column 65000)) ; Some big number.
    (call-interactively 'fill-paragraph)))

;; FIXME: Unbound command - should be bound or removed.
(defun rst-force-fill-paragraph ()
  "Fill paragraph at point, first joining the paragraph's lines into one.
This is useful for filling list item paragraphs."
  (interactive)
  (rst-join-paragraph)
  (fill-paragraph nil))


;; FIXME: Unbound command - should be bound or removed.
;; Generic character repeater function.
;; For sections, better to use the specialized function above, but this can
;; be useful for creating separators.
(defun rst-repeat-last-character (use-next)
  "Fill the current line using the last character on the current line.
Fill up to the length of the preceding line or up to `fill-column' if preceding
line is empty.

If USE-NEXT, use the next line rather than the preceding line.

If the current line is longer than the desired length, shave the characters off
the current line to fit the desired length.

As an added convenience, if the command is repeated immediately, the alternative
column is used (fill-column vs. end of previous/next line)."
  (interactive "P")
  (let* ((curcol (current-column))
         (curline (+ (count-lines (point-min) (point))
                     (if (zerop curcol) 1 0)))
         (lbp (line-beginning-position 0))
         (prevcol (if (and (= curline 1) (not use-next))
                      fill-column
                    (save-excursion
                      (forward-line (if use-next 1 -1))
                      (end-of-line)
                      (skip-chars-backward " \t" lbp)
                      (let ((cc (current-column)))
                        (if (zerop cc) fill-column cc)))))
         (rightmost-column
          (cond ((equal last-command 'rst-repeat-last-character)
                 (if (= curcol fill-column) prevcol fill-column))
                (t (save-excursion
                     (if (zerop prevcol) fill-column prevcol))))))
    (end-of-line)
    (if (> (current-column) rightmost-column)
        ;; Shave characters off the end.
        (delete-region (- (point)
                          (- (current-column) rightmost-column))
                       (point))
      ;; Fill with last characters.
      (insert-char (preceding-char)
                   (- rightmost-column (current-column))))))



;; LocalWords:  docutils http sourceforge rst html wp svn svnroot txt reST regex
;; LocalWords:  regexes alist seq alt grp keymap abbrev overline overlines toc
;; LocalWords:  XML PNT propertized init referenceable

(provide 'rst)

;; Local Variables:
;; sentence-end-double-space: t
;; End:

;;; rst.el ends here
