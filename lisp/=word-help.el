;;; word-help.el --- keyword help for any language doc'd in TeXinfo.

;; Copyright (c) 1996 Free Software Foundation, Inc.

;; Maintainer: Jens T. Berger Thielemann, <jensthi@ifi.uio.no>
;; Keywords: help, keyword, languages, completion

;; This file is part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package provides a rather general interface for doing keyword
;; help in most languages.  In short, it'll determine which TeXinfo
;; file which is relevant for the current mode; cache the index and
;; use regexps to give you help on the keyword you're looking at.

;; Installation
;; ************

;; For the default setup to work for all supported modes, make sure
;; the Texinfo files from the following packages are installed:

;; Texinfo file   | Available in archive or URL | Notes
;; autoconf.info  | autoconf-2.10.tar.gz        | -
;; bison.info     | bison-1.25.tar.gz           | -
;; libc.info      | glibc-1.09.1.tar.gz         | -
;; elisp.info     | elisp-manual-19-2.4.tar.gz  | -
;; latex.info     | ftp://ftp.dante.de/pub/tex/info/latex2e-help-texinfo/latex2e.texi
;; groff.info     | groff-1.10.tar.gz           | -
;; m4.info        | m4-1.4.tar.gz               | -
;; make.info      | make-3.75.tar.gz            | -
;; perl.info      | http://www.perl.com/CPAN/doc/manual/info/
;; simula.info    | Mail bjort@ifi.uio.no       | Written in Norwegian
;; texinfo.info   | texinfo-3.9.tar.gz          | -

;; BTW: We refer to Texinfo files by just their last component, not
;; with an absolute file name.  You must thus set up
;; `Info-directory-list' and `Info-default-directory-list' so that
;; these can automatically be located.

;; Usage
;; *****
;;
;; Place the cursor over the function/variable/type/whatever you want
;; help on.  Type "C-h C-i".  `word-help' will then make a suggestion
;; to an index topic; press return to accept this.  If not, you may use
;; tab-completion to find the topic you're interested in.

;; `word-help' is also able to do symbol completion via the
;; `word-help-complete' function. Bind this function to C-TAB by
;; adding the following line to your .emacs file:
;;
;;   (global-set-key [?\M-\t] 'word-help-complete)
;;
;; Note that some modes automatically override this key; you may
;; therefore wish to either put the above statement in a hook or
;; associate the function with an other key.

;; Usually, `word-help' is able to determine the relevant Texinfo
;; file from looking at the buffer's `mode-name'; if not, you can use
;; the interactive function `set-help-file' to set this.

;; Customizing
;; ***********
;;
;; User interface
;; --------------
;;
;; Two variables control the behaviour of the user-interface of
;; `word-help': `word-help-split-window' and
;; `word-help-magic-index'.  Do C-h v to get more information on
;; these.

;; Adding more Texinfo files
;; -------------------------
;;
;; Associations between mode-names and Texinfo files can be done
;; through the `word-help-mode-alist' variable, which defines an
;; `alist' making `set-help-file' able to initialize the necessary
;; variable.

;; NOTE: If you have to customize the regexps, it is *CRUCIAL* that
;; none of your regexps match the empty string! Not adhering to this
;; restriction will make `word-help' enter an infinite loop.

;; Contacting the author
;; *********************
;;
;; If you wish to contact me for any reason, please feel free to write
;; to:

;; Jens Berger
;; Spektrumveien 4
;; N-0666 Oslo
;; Norway
;;
;; E-mail: <jensthi@ifi.uio.no>

;; Have fun.

;;
;;; Code:
;;

(require 'info)

;;;--------------------
;;;    USER OPTIONS
;;;--------------------

(defvar word-help-split-window t
  "*Non-nil means that the info buffer will pop up in a separate window.
If nil, we will just switch to it.")

(defvar word-help-magic-index t
  "*Non-nil means that the keyword will be searched for in the requested node.
This is done by determining whether the line the point is positioned
on after using `Info-goto-node', actually contains the keyword.  If
not, we will search for the first occurence of the keyword.  This may
help when the info file isn't correctly indexed.")

;;; ---- end of user configurable variables

;;;-------------------------
;;;   ADVANCED USER OPTIONS
;;;-------------------------

(defvar word-help-mode-alist
  '(
    ("autoconf"
     (("autoconf" "Macro Index") ("m4" "Macro index"))
     (("AC_\\([A-Za-z0-9_]+\\)" 1)
      ("[a-z]+"))
     nil
     nil
     (("AC_\\([A-Za-z0-9_]+\\)" 1 nil (("^[A-Z_]+$")))
      ("[a-z_][a-z_]*" 0 nil (("^[a-z_]+$")))))

    ("Bison"
     (("bison" "Index")
      ("libc" "Type Index" "Function Index" "Variable Index"))
     (("%[A-Za-z]*")
      ("[A-Za-z_][A-Za-z0-9_]*"))
     nil
     nil
     (("%[A-Za-z]*" nil nil (("^%")))
      ("[A-Za-z_][A-Za-z0-9_]*" nil nil (("[A-Za-z_][A-Za-z0-9_]*")))))

    ("YACC" . "Bison")

    ("C" (("libc" "Type Index" "Function Index" "Variable Index")))
    ("C++" . "C")

    ("Emacs-Lisp"
     (("elisp" "Index"))
     (("[^][ ()\n\t.\"'#]+"))
     nil
     nil
     lisp-complete-symbol)

    ("LaTeX"
     (("latex" "Command Index"))
     (("\\\\\\(begin\\|end\\){\\([^}\n]+\\)}" 2 0)
      ("\\\\[A-Za-z]+")
      ("\\\\[^A-Za-z]")
      ("[A-Za-z]+"))
     nil
     nil
     (("\\\\begin{\\([A-Za-z]*\\)" 1 "}" (("^[A-Za-z]+$")))
      ("\\\\end{\\([A-Za-z]*\\)" 1 "}" (("^[A-Za-z]+$")))
      ("\\\\renewcommand{\\(\\\\?[A-Za-z]*\\)" 1 "}" (("^\\\\[A-Za-z]+")))
      ("\\\\renewcommand\\(\\\\?[A-Za-z]*\\)" 1 "" (("^\\\\[A-Za-z]+")))
      ("\\\\renewenvironment{?\\([A-Za-z]*\\)" 1  "}"(("^[A-Za-z]+$")))
      ("\\\\[A-Za-z]*" 0 "" (("^\\\\[A-Za-z]+")))))

    ("latex" . "LaTeX")

    ("Nroff"
     (("groff" "Macro Index" "Register Index" "Request Index"))
     (("\\.[^A-Za-z]")
      ("\\.[A-Za-z]+")
      ("\\.\\([A-Za-z]+\\)" 1))
     nil
     nil
     (("\\.[A-Za-z]*" nil nil (("^\\.[A-Za-z]+$")))
      ("\\.\\([A-Za-z]*\\)" 1 nil (("^[A-Za-z]+$")))))

    ("Groff" . "Nroff")

   ("m4"
     (("m4" "Macro index"))
     (("\\([mM]4_\\)?\\([A-Za-z_][A-Za-z_0-9]*\\)" 2))
     nil
     nil
     (("[mM]4_\\([A-Za-z_]?[A-Za-z_0-9]*\\)" 1)
      ("[A-Za-z_][A-Za-z_0-9]*")))

    ("Makefile"
     (("make" "Name Index"))
     (("\\.[A-Za-z]+") ;; .SUFFIXES
      ("\\$[^()]")  ;; $@
      ("\\$([^A-Za-z].)") ;; $(<@)
      ("\\$[\(\{]\\([a-zA-Z+]\\)" 1) ;; $(wildcard)
      ("[A-Za-z]+")) ;; foreach
     nil
     nil
     (("\\.[A-Za-z]*" nil ":" (("^\\.[A-Za-z]+$")))
      ("\\$(\\([A-Z]*\\)" 1 ")" (("^[A-Z]")))
      ("[a-z]+" nil nil (("^[a-z]+$")))))

    ("Perl"
     (("perl" "Variable Index" "Function Index"))
     (("\\$[^A-Za-z^]") ;; $@
      ("\\$\\^[A-Za-z]?") ;; $^D
      ("\\$[A-Za-z][A-Za-z_0-9]+") ;; $foobar
      ("[A-Za-z_][A-Za-z_0-9]+")) ;; dbmopen
     nil
     nil
     (("\\$[A-Za-z]*" nil nil (("^\\$[A-Za-z]+$"))) ;; $variable
      ("[A-Za-z_][A-Za-z_0-9]*" nil nil
       (("^[A-Za-z_][A-Za-z_0-9]*$"))))) ;; function

    ("Simula" (("simula" "Index")) nil t)
    ("Ifi Simula" . "Simula")
    ("SIMULA" . "Simula")

    ("Texinfo"
     (("texinfo" "Command and Variable Index"))
     (("@\\([A-Za-z]+\\)" 1))
     nil
     nil
     (("@\\([A-Za-z]*\\)" 1)))

    )
  "Assoc list between `mode-name' and Texinfo files.
The variable should be initialized with a list of elements with the
following form:

\(mode-name (word-help-info-files) (word-help-keyword-regexps)
	   word-help-ignore-case word-help-index-mapper
           word-help-complete-list)

where `word-help-info-files', `word-help-keyword-regexps' and so
forth of course are the values which should be put in these variables
for this mode.  Note that `mode-name' doesn't have to be a legal
mode-name; the user may use the call `set-help-file', where
`mode-name' will be used in the `completing-read'.

Example entry (for C):

\(\"C\" ((\"libc\" \"Type Index\" \"Function Index\" \"Variable Index\"))
       ((\"[A-Za-z_][A-Za-z0-9]+\")))

The two first variables must be initialized; the two remaining will
get default values if you omit them or set them to nil.  The default
values are:

word-help-keyword-regexps: (\"[A-Za-z_][A-Za-z0-9]+\")
word-help-ignore-case:     nil

More settings may be defined in the future.

You may also define aliases, if there are several relevant mode-names
to a single entry.  These should be of the form:

\(MODE-NAME-ALIAS . MODE-NAME-REAL)

For C++, you would use the alias

\(\"C++\" . \"C\")

to make C++ mode use the same help files as C files do.  Please note
that you can shoot yourself in the foot with this possibility, by
defining recursive aliases.")

;;; --- end of advanced user options

(defvar word-help-ignore-case nil
  "Non-nil means that case is ignored when doing lookup.")
(make-variable-buffer-local 'word-help-ignore-case)

(defvar word-help-info-files nil
  "List of info files with respective nodes, for the current mode.

This should be a list of the following form:

\((INFO-FILE-1 NODE-NAME-1 NODE-NAME-2 ...)
 (INFO-FILE-1 NODE-NAME-1 NODE-NAME-2 ...)
      :           :           :
 (INFO-FILE-1 NODE-NAME-1 NODE-NAME-2 ...))

An example entry for e.g. C would be:

\((\"/local/share/gnu/info/libc\" \"Function Index\" \"Type Index\"
  \"Variable Index\"))

The files and nodes will be searched/cached in the order specified.
This variable is usually set by the `word-help-switch-help-file'
function, which utilizes  the `word-help-mode-alist'.")
(make-variable-buffer-local 'word-help-info-files)

(defvar word-help-keyword-regexps nil
  "Regexps for finding keywords in the current mode.

This is constructed as a list of the following form:

\((REGEXP SUBMATCH-LOOKUP SUBMATCH-CURSOR)
 (REGEXP SUBMATCH-LOOKUP SUBMATCH-CURSOR)
       :          :          :
 (REGEXP SUBMATCH-LOOKUP SUBMATCH-CURSOR))

The regexps will be searched in order for a match which the cursor is
within.

submatch-lookup is the submatch number which will be looked for in the
index.  May be omitted; defaults to 0 (e.g. the entire pattern).  This is
useful in for instance configure lookup; each command is there prefixed
with 'AC_', which must be ignored when doing a lookup.  Example regexp
entry for this:

\(\"AC_\\\\([A-Za-z0-9]+\\\\)\" 1)

submatch-cursor is the part of the match which the cursor must be within.
May be omitted; defaults to 0 (e.g. the entire pattern).")
(make-variable-buffer-local 'word-help-keyword-regexps)
(set-default 'word-help-keyword-regexps '(("[A-Za-z_][A-Za-z_0-9]*")))

(defvar word-help-index-mapper nil
  "Regexps to use for massaging index-entries into keywords.
This variable should contain a list of regexps with sub-expressions,
where we will only look for the sub-expression in the user text.

The regexp list should be formatted as:

  ((REGEXP SUBEXP) (REGEXP SUBEXP) ... )

If the index entry does not match any of the regexps, it will be ignored.

Example:

Perl has index entries of the following form:

* abs VALUE:                    perlfunc.
* accept NEWSOCKET,GENERICSOCKET: perlfunc.
* alarm SECONDS:                perlfunc.
* atan2 Y,X:                    perlfunc.
* bind SOCKET,NAME:             perlfunc.
         :             :           :

We will thus try to extract the first word in the index entry -
\"abs\" from \"abs VALUE\", etc.  This is done by the following entry:

\((\"^\\\\([^ \\t\\n]+\\\\)\" 1))

This value is btw. the default one, and works with most Texinfo files")
(make-variable-buffer-local 'word-help-index-mapper)
(set-default 'word-help-index-mapper '(("^\\([^ \t\n]+\\)" 1)))


(defvar word-help-complete-list nil
  "Regexps or function to use for completion of symbols.
The list should have the following format:

  ((REGEXP SUBMATCH TEXT-APPEND (RE-FILTER-1 REG-FILTER-2 ...)
           :               :             :               :      :
   (REGEXP SUBMATCH TEXT-APPEND (RE-FILTER-1 REG-FILTER-2 ...))

The two first entries are similar to `word-help-keyword-regexps',
REGEXP is a regular expression which should match any relevant
expression, and where SUBMATCH should be used for look up. By
specifying non-nil REGEXP-FILTERs, we'll only include entries in the
index which matches the regexp specified.

If the contents of this variable is a symbol of a function, this
function will be called instead. This is useful for modes providing
a more intelligent function (like `lisp-complete-symbol' in Emacs Lisp mode).

If you would like to use another function instead, you may.

Non-nil TEXT-APPEND means that this text will be inserted after the
completion, if we manage to do make a completion.")
(make-variable-buffer-local 'word-help-complete-list)
(set-default 'word-help-complete-list '(("[A-Za-z_][A-Za-z_0-9]*")))

;;; Work variables


(defvar word-help-main-index nil
  "List of all index entries.

See `word-help-process-indexes' for structure formatting.

Minor note: This variable is a list if it is initialized, t if
initializing failed and nil if uninitialized.")
(make-variable-buffer-local 'word-help-main-index)

(defvar word-help-complete-index nil
  "List of regexps for completion, with matching index entries.
Value is nil if uninitialized, t if initialized but not accessible,
a list if we're feeling ok.")
(make-variable-buffer-local 'word-help-complete-index)

(defvar word-help-main-obarray nil
  "Global work variable for `word-help' system.
Do Not mess with this!")

(defvar word-help-history nil
  "History for `word-help' minibuffer queries.")
(make-local-variable 'word-help-history)

(defvar word-help-current-help-file nil
  "Current help file active for this mode.")

(defvar word-help-index-alist nil
  "An assoc list mapping help files to info indexes.
This means that `word-help-mode-index' can be init'ed faster.")

(defvar word-help-help-mode nil
  "Which mode the help system is bound to for the current mode.")
(make-variable-buffer-local 'word-help-help-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; User Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Debugging

;;;###autoload
(defun reset-word-help ()
  "Clear all cached indexes in the `word-help' system.
You should only need this when installing new info files, and/or
adding more Texinfo files to the `word-help' system."
  (interactive)
  (setq word-help-index-alist nil
	word-help-main-index nil
	word-help-info-files nil
	word-help-complete-index nil))


;;; Changing help file

;;;###autoload
(defun set-help-file ()
  "Change which set of Texinfo files used for word-help.

`word-help' maintains a list over which Texinfo files which are
relevant for each programming language (`word-help-mode-alist').  It
usually selects the correct one, based upon the value of `mode-name'.
If this guess is incorrect, you may also use this function manually to
instruct future `word-help' calls which Texinfo files to use."
  (interactive)
  (let (helpfile helpguess (completion-ignore-case t))
;; Try to make a guess
    (setq helpguess (cond
		     (word-help-current-help-file)
		     ((word-help-guess-help-file))))
;; Ask the user
    (setq helpfile (completing-read
		    (if helpguess
			(format "Select help mode (default %s): " helpguess)
		      "Select help mode: ")
		    word-help-mode-alist
		    nil t nil nil))
    (if (equal "" helpfile)
	(setq helpfile helpguess))
    (if helpfile
	(word-help-switch-help-file helpfile))))

;;; Main user interface

;;;###autoload
(defun word-help ()
  "Find documentation on the keyword under the cursor.
The determination of which language the keyword belongs to, is based upon
The relevant info file is selected by matching `mode-name' (the major
mode) against the assoc list `word-help-mode-alist'.

If this is not possible, `set-help-file' will be invoked for selecting
the relevant info file.  `set-help-file' may also be invoked
interactively by the user.

If the keyword you are looking at is not available in any index, no
default suggestion will be presented. "
  (interactive)
  (let (myguess guess index-info
		(completion-ignore-case word-help-ignore-case))
;; Set necessary variables for later lookup
    (word-help-find-help-file)
;; Have we previously cached datas?
    (word-help-process-indexes)
    (if
	(atom word-help-main-index)
	(message "No help file available for this mode.")
;; First make a guess at what the user is looking for
        (setq myguess (word-help-guess
		       (point)
		       (cond
			((not (atom word-help-main-index))
			 (car word-help-main-index)))
		       word-help-keyword-regexps))
;; Ask the user himself
	(setq guess (completing-read
					; Format string
		     (if myguess
			 (format "Look up keyword (default %s): " myguess)
  		       "Look up keyword: ")
					; Collection
		     (car word-help-main-index)
		     nil t nil 'word-help-history))
	(if (equal guess "")
	    (setq guess myguess))
;; If we've got anything meaningful to lookup, do so
	(if (not guess)
	    (message "Help aborted.")
	    (setq index-info (word-help-find-index-node
			      guess
			      word-help-main-index))
	    (if (not index-info)
		(message "Oops, I could not find \"%s\" anyway! Bug?" guess)
	      (word-help-goto-index-node (nconc index-info (list guess))))))))

;;;###autoload
(defun word-help-complete ()
  "Perform completion on the symbol preceding the point.
The determination of which language the keyword belongs to, is based upon
The relevant info file is selected by matching `mode-name' (the major
mode) against the assoc list `word-help-mode-alist'.

If this is not possible, `set-help-file' will be invoked for selecting
the relevant info file.  `set-help-file' may also be invoked
interactively by the user.

The keywords are extracted from the index of the info file defined for
this mode, by using the `word-help-complete-list' variable."
  (interactive)
  (word-help-make-complete)
  (cond
   ((not word-help-complete-index)
    (message "No completion available for this mode."))
   ((symbolp word-help-complete-index)
    (call-interactively word-help-complete-index))
   ((listp word-help-complete-index)
    (let ((all-match (word-help-guess-all (point)
					  word-help-complete-index t))
	  (completion-ignore-case word-help-ignore-case)
	  (c-list word-help-complete-index)
	  c-entry word-match completion completed)
;; Loop over and try to find a match
      (while (and all-match (not completed))
	(setq word-match (car all-match)
	      c-entry (car c-list)
	      c-list (cdr c-list)
	      all-match (cdr all-match))
;; Check whether the current pattern matched
	(if word-match
	    (let ((close (nth 3 c-entry))
		  (words (nth 4 c-entry)))
;; Find the maximum completion for this word
;		(print word-match)
;		(print c-entry)
;		(print close)
	      (setq completion (try-completion word-match words))
;; Was the match exact
	      (cond ((eq completion t)
		     (and close
			  (not (looking-at (regexp-quote close)))
			  (insert close))
		     (setq completed t))
;; Silently ignore non-matches
		    ((not completion))
;; May we complete more unambiguously
		    ((not (string-equal completion word-match))
		     (delete-region (- (point) (length word-match))
				    (point))
		     (insert completion)
		     (if (eq t (try-completion completion words))
			 (progn
			   (and close
				(not (looking-at (regexp-quote close)))
				(insert close))))
		     (setq completed t))
		    (t
		     (message "Making completion list...")
		     (let ((list (all-completions word-match words nil)))
		       (setq completed list)
		       (with-output-to-temp-buffer "*Completions*"
			 (display-completion-list list)))
		     (message "Making completion list...done"))))))
      (if (not completed) (message "No match."))))
   (t (message "No completion available for this mode."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; Index mapping ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun word-help-map-index-entries (str re-list)
  "Transform an Info index entry into a programming keyword.
Uses this by mapping the entries through `word-help-index-mapper'."
  (let ((regexp (car (car re-list)))
	(subexp (car (cdr (car re-list))))
	(next (cdr re-list)))
    (cond
     ((string-match regexp str)
      (substring str (match-beginning subexp) (match-end subexp)))
     (next
      (word-help-map-index-entries str next)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;; Switch mode files ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Mode lookup

(defun word-help-guess-help-file ()
  "Guesses a relevant help file based on mode name.
Returns nil if no guess could be made.  Uses `word-help-mode-alist'."
  (let (guess)
    (cond
     ((setq guess (assoc mode-name word-help-mode-alist))
      (car guess)))))


(defun word-help-switch-help-file (helpfile)
  "Changes the help-file to the mode name given.
Uses `word-help-mode-alist'."
  (if helpfile
      (let (helpdesc)
	(if (not (setq helpdesc (assoc helpfile word-help-mode-alist)))
	    (message "No help defined for \"%s\"." helpfile)
	    (if (stringp (cdr helpdesc))
		(word-help-switch-help-file (cdr helpdesc))
  	        (word-help-make-default-map
		 helpdesc
		 (list 'word-help-help-mode
		       'word-help-info-files
		       'word-help-keyword-regexps
		       'word-help-ignore-case
		       'word-help-index-mapper
		       'word-help-complete-list))))
	    (setq word-help-main-index nil
		  word-help-complete-index nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;; Index collection ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun word-help-extract-index (file-name index-list index-map ignore-case)
  "Extract index from filename and the first node name in index list.
`file-name' is the name of the info file, while `index-list' is a list
of node-names to search."
  (let (cmd1 cmdlow nodename ob-array next (case-fold-search word-help-ignore-case))
    (setq nodename (car index-list))
    (setq ob-array (make-vector 211 0))
    (message "Processing \"%s\" in %s..." nodename file-name)
    (save-window-excursion
      (Info-goto-node (concat "(" file-name ")" nodename))
      (end-of-buffer)
      (while (re-search-backward "\\* \\([^\n:]+\\):" nil t)
	(setq cmd1 (buffer-substring (match-beginning 1) (match-end 1)))
	(setq cmdlow (if ignore-case (downcase cmd1) cmd1))
	(if index-map
	    (setq cmdlow (word-help-map-index-entries cmdlow
			      index-map)))
;; We have to do this workaround to support case-insensitive matching
	(cond
	 (cmdlow
	  (put (intern cmdlow ob-array) 'word-help-real-name cmd1)
	  (intern cmdlow word-help-main-obarray)))))
    (setq next (cond
		((cdr index-list)
		 (word-help-extract-index file-name (cdr index-list)
					     index-map ignore-case))))
    (nconc (list (list nodename ob-array)) next)))


(defun word-help-collect-indexes (info-file)
  "Process all the indexes in an info file.

Uses `word-help-extract-index' on each node, and returns an entry
suitable for merging into `word-help-process-indexes'.  `info-file'
is an entry of the form

\(FILE-NAME INDEX-NAME-1 INDEX-NAME-2 ...)"
  (let ((file  (car info-file))
	(nodes (cdr info-file)))
    (nconc (list file) (word-help-extract-index file nodes
						   word-help-index-mapper
                                                   word-help-ignore-case))))

(defun word-help-process-indexes ()
  "Process all the entries in the global variable `word-help-info-files'.
Returns a list formatted as follows:

\(all-entries-ob
 (file-name-1 (node-name-1 this-node-entries-ob)
	      (node-name-2 this-node-entries-ob)
	           :          :         :
	      (node-name-n this-node-entries-ob))
 (file-name-2 (node-name-1 this-node-entries-ob)
	      (node-name-2 this-node-entries-ob)
	           :          :         :
	      (node-name-n this-node-entries-ob))
    :    :    :    :    :    :    :    :    :
 (file-name-n (node-name-1 this-node-entries-ob)
	      (node-name-2 this-node-entries-ob)
	           :          :         :
	      (node-name-n this-node-entries-ob)))

The symbols in the obarrays may contain the additional property
`word-help-real-name', which tells the *real* node to go to.

Note that we use `word-help-index-alist' to speed up the process.  Note
that `word-help-switch-help-file' must have been called before this function.

This structure is then later searched by `word-help-find-index-node'."
  (let (index-words old-index)
    (if (not word-help-main-index)
	 (cond
	  ((setq old-index
		 (assoc word-help-help-mode word-help-index-alist))
	   (setq word-help-main-index (nth 1 old-index)))
	  (word-help-info-files
	   (setq word-help-main-obarray (make-vector 307 0)
		 index-words (mapcar 'word-help-collect-indexes
				     word-help-info-files)
		 word-help-main-index
		 (append (list word-help-main-obarray) index-words))
	   (setq word-help-index-alist (cons (list word-help-help-mode
						       word-help-main-index)
						 word-help-index-alist)))
	  (t (setq word-help-main-index t))))))

(defun word-help-find-help-file ()
  "Tries to find and set a relevant help file for the current mode."
  (let (helpguess)
    (if (not word-help-info-files)
	(if (setq helpguess (word-help-guess-help-file))
	    (word-help-switch-help-file helpguess)
	  (set-help-file)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; Keyword guess ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun word-help-guess-all (cur-point re-list
			    &optional copy-to-point)
  "Guesses *all* keywords the user possibly may be looking at.
Returns a list of all possible keywords. "
  (let ((regexp (car (car re-list)))
	(submatch  (cond ((nth 1 (car re-list))) (0)))
	(cursmatch (cond ((nth 2 (car re-list))) (0)))
	(guess nil)
	(next-guess nil)
	(case-fold-search word-help-ignore-case)
	(end-point nil))
    (save-excursion
      (end-of-line)
      (setq end-point (point))
      ;; Start at the beginning
      (beginning-of-line)
      (while (and (not guess) (re-search-forward regexp end-point t))
	;; Look whether the cursor is within the match
	(if (and (<= (match-beginning cursmatch) cur-point)
		 (>= (match-end cursmatch) cur-point))
	    (if (or (not copy-to-point) (<= cur-point (match-end submatch)))
		(setq guess (buffer-substring (match-beginning submatch)
					      (if copy-to-point
						  cur-point
						(match-end submatch)))))))
      ;; If we found anything, return it and call ourselves again
      (if (cdr re-list)
	  (setq next-guess (word-help-guess-all cur-point (cdr re-list)
						copy-to-point))))
    (cons guess next-guess)))

(defun word-help-guess-match (all-match cmd-array)
  (let ((sym (car all-match)))
    (cond
     ((and sym (intern-soft (if word-help-ignore-case
				(downcase sym)
			      sym) cmd-array)
      sym))
     ((cdr all-match)
      (word-help-guess-match (cdr all-match) cmd-array)))))


(defun word-help-guess (cur-point cmd-array re-list)
  "Guesses what keyword the user is looking at, and returns that.
CUR-POINT should be the current value of `point', CMD-ARRAY an obarray
of all the keywords which are defined for the current mode, and
RE-LIST a list of regexps use for the hunt.  See also
`word-help-keyword-regexps'."
  (let ((all-matches (word-help-guess-all cur-point re-list)))
;    (print all-matches)
    (word-help-guess-match all-matches cmd-array)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;	Show node for keyword ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Find an index entry

(defun word-help-find-index-node (node index-reg)
  "Finds the node named `node' in the index-register `index-reg'.
`index-reg' has the format as returned (and documented) by the
`word-help-process-indexes' call.  In most cases, this will be equal to
`word-help-main-index'.

Returns a list with format
  (file-name index-node-name index-entry)
which contains the file and index where the entry can be found.
Returns nil if the entry can't be found."
  (let (file-info node-name)
    (setq node-name (cond (word-help-ignore-case (downcase node)) (node)))
    (if (intern-soft node-name (car index-reg))
      (setq file-info (word-help-index-search-file node-name
						      (cdr index-reg))))
    file-info))

(defun word-help-index-search-file (entry file-data)
  "Searches a cached file for the index-entry `entry'."
  (let (this-file next-files file-name node node-infos)
    (setq this-file (car file-data)
	  next-files (cdr file-data)
	  file-name (car this-file)
	  node-infos (cdr this-file)
	  node (word-help-index-search-nodes entry node-infos))
    (cond
     (node
      (cons file-name node))
     (next-files (word-help-index-search-file entry next-files)))))

(defun word-help-index-search-nodes (entry node-info)
  "Searches a cached list of nodes for the entry `entry'."
  (let (this-node next-nodes node-name node-ob node-sym)
    (setq this-node (car node-info)
	  next-nodes (cdr node-info)
	  node-name (car this-node)
	  node-ob (car (cdr this-node))
	  node-sym (intern-soft entry node-ob))
    (cond
     (node-sym
      (list node-name (get node-sym 'word-help-real-name)))
     (next-nodes (word-help-index-search-nodes entry next-nodes)))))

;;; Switch to a node in an index

(defun word-help-goto-index-node (index-info)
  "Jumps to an index node.
`index-info' should be a list with the following format:

\(FILE-NAME INDEX-NODE-NAME INDEX-ENTRY KEYWORD)"

  (let* ((file-name (car index-info))
	 (node-name (nth 1 index-info))
	 (entry-name (nth 2 index-info))
	 (kw-name (nth 3 index-info))
	 (buffer (current-buffer)))
    (if word-help-split-window
	(pop-to-buffer nil))
    (Info-goto-node (concat "(" file-name ")" node-name))
    (Info-menu entry-name)
;; Do magic keyword search
    (if word-help-magic-index
	(let (end-point regs this-re found entry-re)
	  (setq entry-re (regexp-quote kw-name)
		regs (list (concat
			    (if (string-match "^[A-Za-z]" entry-name)
				"\\<" "")
			    entry-re
			    (if (string-match "[A-Za-z]$" entry-name)
				"\\>" ""))
			   (concat "[`\"\(]" entry-re)
			   (concat "^" entry-re
				   (if (string-match "[A-Za-z]$" entry-name)
				       "\\>" ""))))
	  (end-of-line)
	  (setq end-point (point))
	  (beginning-of-line)
	  (if (not (re-search-forward (car regs) end-point t))
	      (while (and (not found) (car regs))
		(setq this-re (car regs)
		      regs (cdr regs)
		      found (re-search-forward this-re nil t))))
	(recenter 0)))
    (if word-help-split-window
	(pop-to-buffer buffer))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Completion ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun word-help-extract-matches (from-ob dest-ob re-list)
  "Takes atoms from from-ob, and puts them in dest-ob if they match re-list."
  (let ((regexp (car (car re-list))))
    (mapatoms (lambda (x)
		(if (or (not regexp) (string-match regexp (symbol-name x)))
		    (intern (symbol-name x) dest-ob)))
	      from-ob)
    (if (cdr re-list)
	(word-help-extract-matches from-ob dest-ob (cdr re-list))))
  dest-ob)

(defun word-help-make-complete ()
  "Generates the `word-help-complete-index'."
  (if word-help-complete-index
      nil
    (word-help-find-help-file)
    (cond
     ((symbolp word-help-complete-list)
      (setq word-help-complete-index word-help-complete-list))
     (t
      (word-help-process-indexes)
      (if (not (atom word-help-main-index))
	  (let ((from-ob (car word-help-main-index)))
	    (message "Processing keywords...")
	    (setq word-help-complete-index
		  (mapcar
		   (lambda (cmpl)
		     (let
			 ((regexp (car cmpl))
			  (subm (cond ((nth 1 cmpl)) (0)))
			  (app (cond ((nth 2 cmpl)) ("")))
			  (re-list (cond ((nth 3 cmpl)) ('((".")))))
			  (obarr (make-vector 47 0)))
		       (list regexp subm subm app
			     (word-help-extract-matches from-ob obarr
							re-list))))
		   word-help-complete-list))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Misc. ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Default mapping

(defun word-help-make-default-map (list vars)
  "Makes a default mapping for `vars', which must be listed in order.
vars is a list of quoted symbols.  If the nth entry in the list is
non-nil, the nth variable will be given this value.  If nil, the var
will be given the global default value."
  (set (car vars) (cond ((car list)) ((default-value (car vars)))))
  (if (cdr vars)
      (word-help-make-default-map (cdr list) (cdr vars))))

(provide 'word-help)

;;; word-help.el ends here
