;; reftex.el --- Minor mode for doing \label, \ref and \cite in LaTeX
;; Copyright (c) 1997, 1998 Free Software Foundation, Inc.

;; Author:     Carsten Dominik <dominik@strw.LeidenUniv.nl>
;; Keywords:   tex

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

;;---------------------------------------------------------------------------
;;
;;; Commentary:
;;
;; RefTeX is a minor mode with distinct support for \ref, \label and
;; \cite commands in (multi-file) LaTeX documents.
;; Labels are created semi-automatically.  Definition context of labels is
;; provided when creating a reference.  Citations are simplified with
;; efficient database lookup.  A table of contents buffer provides easy
;; access to any part of a document.
;;
;;
;; To turn RefTeX Minor Mode on and off in a particular buffer, use
;; `M-x reftex-mode'.
;;
;; To turn on RefTeX Minor Mode for all LaTeX files, add one of the
;; following lines to your .emacs file:
;;
;;   (add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
;;   (add-hook 'latex-mode-hook 'turn-on-reftex)   ; with Emacs latex mode
;;
;;
;; DOCUMENTATION
;; -------------
;;
;; There is an extensive texinfo document describing RefTeX in detail.
;; When you are getting reftex.el with the Emacs distribution, the
;; info files should already be installed.  To view this
;; documentation, use `M-x reftex-info RET'.
;;
;; The documentation is also available at
;;
;;     http://www.strw.leidenuniv.nl/~dominik/Tools/
;;
;;---------------------------------------------------------------------------
;; 
;; RefTeX in a Nutshell
;; ====================
;; 
;;   1. Labels and References
;;      RefTeX distinguishes labels for different environments.  It always
;;      knows if a certain label references a figure, table etc..  You can
;;      configure RefTeX to recognize any additional labeled environments
;;      you have defined yourself (variable REFTEX-LABEL-ALIST).
;;         - Creating Labels
;;           Type `C-c (' (`reftex-label') to insert a label at point.
;;           RefTeX will either
;;              - derive a label from context (default for section labels)
;; 
;;              - insert a simple label consisting of a prefix and a number
;;                (default for equations, enumerate items, and footnotes)
;;                or
;; 
;;              - prompt for a label string (figures and tables).
;; 
;;           Which labels are created how is configurable (variable
;;           REFTEX-INSERT-LABEL-FLAGS).
;; 
;;         - Referencing Labels
;;           Referencing labels is a snap and I promise you'll love it.
;;           In order to make a reference, type `C-c )'
;;           (`reftex-reference').  This shows an outline of the document
;;           with all labels of a certain type (figure, equation,...) and
;;           context of the label definition.  Selecting one of the labels
;;           inserts a `\ref' macro into the original buffer.  Online help
;;           during the selection is available with `?'.
;; 
;;   2. Citations
;;      After typing `C-c [' (`reftex-citation'), RefTeX will let you
;;      specify a regular expression to search in current BibTeX database
;;      files (as specified in the `\bibliography' command) and pull out a
;;      formatted list of matches for you to choose from.  The list is
;;      *formatted* and sorted, thus much easier to read than the raw
;;      database entries.  The text inserted into the buffer is by default
;;      just `\cite{KEY}', but can also contain author names and the year
;;      in a configurable way (variable REFTEX-CITE-FORMAT).
;; 
;;   3. Viewing Cross References
;;      With point on or anywhere before a `\ref' or `\cite' macro, press
;;      `C-c &' (`reftex-view-crossref').  This will display the
;;      corresponding label definition or BibTeX database entry in another
;;      window.
;; 
;;   4. Table of Contents
;;      Typing `C-c =' (`reftex-toc') will show a table of contents of the
;;      document.  From that buffer, you can jump quickly to every part of
;;      your document.
;; 
;;   5. Multifile Documents
;;      Multifile Domuments are fully supported. RefTeX will provide cross
;;      referencing information from all files which are part of the
;;      document. You may also use it to reference labels in external
;;      documents (in cooperation with the LaTeX package `xr').
;; 
;;   6. Document Parsing
;;      RefTeX needs to parse the document in order to find labels and
;;      other information.  It will do it automatically once, when you
;;      start working with a document.  Re-parsing should not be necessary
;;      too often since RefTeX updates its lists internally when you make
;;      a new label with `reftex-label'.  To enforce reparsing, call any
;;      of the commands described above with a raw `C-u' prefix, or press
;;      the `r' key in the label selection buffer or the table of contents
;;      buffer.
;;
;;---------------------------------------------------------------------------
;;
;; AUTHOR
;; ======
;;
;; Carsten Dominik <dominik@strw.LeidenUniv.nl>
;;
;;         with contributions from Stephen Eglen
;;
;; The newest version of RefTeX can be found at
;;
;;    http://www.strw.leidenuniv.nl/~dominik/Tools/
;;    ftp://strw.leidenuniv.nl/pub/dominik/
;;
;; The version at this site is compatible with Emacs 19 - the version
;; distributed with Emacs 20 is not.
;;
;; THANKS TO:
;; ---------
;; Thanks to the people on the Net who have used RefTeX and helped
;; developing it with their reports.  In particular thanks to
;;
;;    F. Burstall, Alastair Burt, Soren Dayton, Stephen Eglen,
;;    Karl Eichwalder, Peter Galbraith, Dieter Kraft, Adrian Lanz,
;;    Rory Molinari, Laurent Mugnier, Sudeep Kumar Palat, Daniel Polani,
;;    Robin Socha, Richard Stanton, Allan Strand, Jan Vroonhof,
;;    Christoph Wedler.
;;
;; The view crossref feature was inspired by the similar function in
;; Peter Galbraith's bib-cite.el.
;;
;; Finally thanks to Uwe Bolick who first got me (some years ago) into
;; supporting LaTeX labels and references with an Editor (which was
;; MicroEmacs at the time).
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

;;; Code:

(eval-when-compile (require 'cl))

;; Stuff that needs to be there when we use defcustom
;; --------------------------------------------------

(require 'custom)

(defvar reftex-tables-dirty t
  "Flag showing if tables need to be re-computed.")

(eval-and-compile
  (defun reftex-set-dirty (symbol value)
    (setq reftex-tables-dirty t)
    (set symbol value)))

(eval-and-compile
  (defmacro reftex-fp (n)
    (if (fboundp 'forward-point) 
	(list 'forward-point n)
      (list '+ '(point) n))))

;;; Begin of Configuration Section ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Define the two constants which are needed during compilation

(eval-and-compile
(defconst reftex-label-alist-builtin
  '(
    ;; Some aliases, mostly for backward compatibility
    (Sideways    "Alias for -->rotating" (rotating))
    (AMSTeX      "amsmath with eqref macro"
                 ((nil ?e nil "~\\eqref{%s}")
                  amsmath))

    ;; Individual package defaults
    (amsmath "AMS-LaTeX math environments"
     (("align"        ?e nil nil eqnarray-like)
      ("gather"       ?e nil nil eqnarray-like)
      ("multline"     ?e nil nil t)
      ("flalign"      ?e nil nil eqnarray-like)
      ("alignat"      ?e nil nil alignat-like)
      ("xalignat"     ?e nil nil alignat-like)
      ("xxalignat"    ?e nil nil alignat-like)
      ("subequations" ?e nil nil t)))

    (endnotes "The \\endnote macro"
     (("\\endnote[]{}" ?n nil nil 2 (regexp "Endnotes?"))))

    (fancybox "The Beqnarray environment"
     (("Beqnarray" ?e nil nil eqnarray-like)))

    (floatfig "The floatingfigure environment"
     (("floatingfigure" ?f nil nil caption)))

    (longtable   "The longtable environment"
     (("longtable"  ?t nil nil caption)))

    (picinpar    "The figwindow and tabwindow environments"
     (("figwindow" ?f nil nil 1)
      ("tabwindow" ?f nil nil 1)))

    (rotating    "Sidewaysfigure and table"
     (("sidewaysfigure" ?f nil nil caption)
      ("sidewaystable"  ?t nil nil caption)))

    (sidecap      "CSfigure and SCtable"
     (("SCfigure"       ?f nil nil caption)
      ("SCtable"        ?t nil nil caption)))

    (subfigure   "Subfigure environments/macro"
     (("subfigure"   ?f nil nil caption)
      ("subfigure*"  ?f nil nil caption)
      ("\\subfigure[]{}" ?f nil nil 1)))

    (supertab    "Supertabular environment"
     (("supertabular" ?t nil nil "\\tablecaption{")))

    (wrapfig     "The wrapfigure environment"
     (("wrapfigure" ?f nil nil caption)))

    ;; The LaTeX core stuff
    (LaTeX       "LaTeX default environments"
     (("section"   ?s "sec:" "~\\ref{%s}" (nil . t)
       (regexp "Parts?" "Chapters?" "Chap\\." "Sections?" "Sect?\\."
	       "Paragraphs?" "Par\\."
	       "\\\\S" "\247" "Teile?" "Kapitel" "Kap\\." "Abschnitte?"
	       "Appendi\\(x\\|ces\\)" "App\\."  "Anh\"?ange?" "Anh\\."))

      ("enumerate" ?i "item:" "~\\ref{%s}" item
       (regexp "Items?" "Punkte?"))
      
      ("equation"  ?e "eq:" "~(\\ref{%s})" t
       (regexp "Equations?" "Eqs?\\." "Eqn\\." "Gleichung\\(en\\)?"  "Gl\\."))
      ("eqnarray"  ?e "eq:" nil eqnarray-like)
      
      ("figure"    ?f "fig:" "~\\ref{%s}" caption
       (regexp "Figure[sn]?" "Figs?\\." "Abbildung\\(en\\)?" "Abb\\."))
      ("figure*"   ?f nil nil caption)
      
      ("table"     ?t "tab:" "~\\ref{%s}" caption
       (regexp "Tables?" "Tab\\." "Tabellen?"))
      ("table*"    ?t nil nil caption)
      
      ("\\footnote[]{}" ?n "note:" "~\\ref{%s}" 2
       (regexp "Footnotes?" "Notes?"))
      
      ("any"       ?\  " "   "~\\ref{%s}" nil)))

    )
  "The default label environment descriptions.
Lower-case symbols correspond to a style file of the same name in the LaTeX
distribution.  Mixed-case symbols are convenience aliases.")

(defconst reftex-cite-format-builtin
  '(
    (default "Default macro \\cite{%l}"
      "\\cite{%l}")
    (natbib "The Natbib package"
     ((?\C-m . "\\cite{%l}")
      (?t    . "\\citet{%l}")
      (?T    . "\\citet*{%l}")
      (?p    . "\\citep{%l}")
      (?P    . "\\citep*{%l}")
      (?e    . "\\citep[e.g.][]{%l}")
      (?a    . "\\citeauthor{%l}")
      (?y    . "\\citeyear{%l}")))
    (harvard "The Harvard package"
     ((?\C-m . "\\cite{%l}")
      (?p    . "\\cite{%l}")
      (?t    . "\\citeasnoun{%l}")
      (?n    . "\\citeasnoun{%l}")
      (?s    . "\\possessivecite{%l}")
      (?e    . "\\citeaffixed{%l}{?}")
      (?y    . "\\citeyear{%l}")
      (?a    . "\\citename{%l}")))
    (chicago "The Chicago package"
     ((?\C-m . "\\cite{%l}")
      (?t    . "\\citeN{%l}")
      (?T    . "\\shortciteN{%l}")
      (?p    . "\\cite{%l}")
      (?P    . "\\shortcite{%l}")
      (?a    . "\\citeA{%l}")
      (?A    . "\\shortciteA{%l}")
      (?y    . "\\citeyear{%l}")))
    (astron "The Astron package"
     ((?\C-m . "\\cite{%l}")
      (?p    . "\\cite{%l}" )
      (?t    . "%2a (\\cite{%l})")))
    (author-year "Do-it-yourself Author-year"
     ((?\C-m . "\\cite{%l}")
      (?t    . "%2a (%y)\\nocite{%l}")
      (?p    . "(%2a %y\\nocite{%l})")))
    (locally     "Full info in parenthesis"
     "(%2a %y, %j %v, %P, %e: %b, %u, %s %<)")
    ;; undocumented feature: `%<' kills white space and punctuation locally.
    )
  "Builtin versions of for the citation format.
The following conventions are valid for all alist entries:
`?\C-m' should always point to a straight \\cite{%l} macro.
`?t'    should point to a textual citation (citation as a noun).
`?p'    should point to a parenthetical citation.")
)

;; Configuration Variables and User Options for RefTeX ------------------

(defgroup reftex nil
  "LaTeX label and citation support."
  :tag "RefTeX"
  :link '(url-link :tag "Home Page" 
		   "http://strw.leidenuniv.nl/~dominik/Tools/")
  :link '(emacs-commentary-link :tag "Commentary in reftex.el" "reftex.el")
  :prefix "reftex-"
  :group 'tex)

(defun reftex-customize ()
  "Call the customize function with reftex as argument."
  (interactive)
  ;; Depending on the customize version we can call different functions.
  (cond
   ((fboundp 'customize-browse)
    (customize-browse 'reftex))
   ((fboundp 'customize-group)
    (customize-group 'reftex))
   ((fboundp 'customize)
    (customize 'reftex))
   (t (error "Custom.el not available"))))

(defun reftex-create-customize-menu ()
  "Create a full customization menu for RefTeX."
  (interactive)
  (if (fboundp 'customize-menu-create)
      (progn
	(easy-menu-change 
	 '("Ref") "Customize"
	 `(["Browse RefTeX group" reftex-customize t]
	   "---"
	   ,(customize-menu-create 'reftex)
	   ["Set" Custom-set t]
	   ["Save" Custom-save t]
	   ["Reset to Current" Custom-reset-current t]
	   ["Reset to Saved" Custom-reset-saved t]
	   ["Reset to Standard Settings" Custom-reset-standard t]))
	(message "\"Ref\"-menu now contains full customization menu"))
    (error "Cannot expand menu (outdated version of cus-edit.el)")))

(defun reftex-show-commentary ()
  "Use the finder to view the file documentation from `reftex.el'."
  (interactive)
  (require 'finder)
  (finder-commentary "reftex.el"))

(defun reftex-info ()
  "Read documentation for RefTeX in the info system."
  (interactive)
  (require 'info)
  (Info-goto-node "(reftex)"))

;; Support for \label and \ref --------------------------------------

(defgroup reftex-label-support nil
  "Support for creation, insertion and referencing of labels in LaTeX."
  :group 'reftex)

(defgroup reftex-defining-label-environments nil
  "Definition of environments and macros to do with label."
  :group 'reftex-label-support)

;; Make a constant for the customization stuff
(eval-and-compile
  (defconst reftex-tmp
    '((const  :tag "Default position" t)
      (const  :tag "After label"      nil)
      (number :tag "Macro arg nr" 1)
      (regexp :tag "Regexp" "")
      (const  :tag "Caption in float" caption)
      (const  :tag "Item in list" item)
      (const  :tag "Eqnarray-like" eqnarray-like)
      (const  :tag "Alignat-like" alignat-like)
      (symbol :tag "Function" my-func))))

(defcustom reftex-default-label-alist-entries
  '(amsmath endnotes fancybox floatfig longtable picinpar
	    rotating sidecap subfigure supertab wrapfig LaTeX)
  "Default label alist specifications.  LaTeX should be the last entry.
This list describes the default label environments RefTeX should always use.
It is probably a mistake to remove the LaTeX symbol from this list.

The options include:
LaTeX      The standard LaTeX environments.
Sideways   The sidewaysfigure and sidewaystable environments.
AMSTeX     The math environments in the AMS-LaTeX amsmath package.

For the full list of options, try

M-x customize-variable RET reftex-default-label-alist-entries RET."
  :group 'reftex-defining-label-environments
  :set   'reftex-set-dirty
  :type `(set
	  :indent 4
          :inline t
          :greedy t
          ,@(mapcar
             (function
              (lambda (x)
                (list 'const ':tag (concat (symbol-name (nth 0 x))
                                           ": " (nth 1 x))
                      (nth 0 x))))
             reftex-label-alist-builtin)))

(defcustom reftex-label-alist nil
  "Alist with information on environments for \\label-\\ref use.

This docstring is easier to understand after reading the configuration
examples in `reftex.el'.  Looking at the builtin defaults in the constant
`reftex-label-alist-builtin' may also be instructive.

Set this variable to define additions and changes to the default.  The only
things you MUST NOT change is that `?s' is the type indicator for section
labels, and SPC for the `any' label type.  These are hard-coded at other
places in the code.

Each list entry describes either an environment carrying a counter for use
with \\label and \\ref, or a LaTeX macro defining a label as (or inside)
one of its arguments.  The elements of each list entry are:

0.  Name of the environment (like \"table\") or macro (like \"\\\\myfig\").
    For macros, indicate the macro arguments for best results, as in
    \"\\\\myfig[]{}{}{*}{}\".  Use square brackets for optional arguments,
    a star to mark the label argument, if any.  The macro does not have to
    have a label argument - you could also use \\label{..} inside one of
    its arguments.
    Special names: `section' for section labels, `any' to define a group
    which contains all labels.
    This may also be nil if the entry is only meant to change some settings
    associated with the type indicator character (see below).

1.  Type indicator character, like `?t', must be a printable ASCII character.
    The type indicator is a single character which defines a label type.
    Any label inside the environment or macro is assumed to belong to this
    type.  The same character may occur several times in this list, to cover
    cases in which different environments carry the same label type (like
    `equation' and `eqnarray').

2.  Label prefix string, like \"tab:\".
    The prefix is a short string used as the start of a label.  It may be the
    empty string.  The prefix may contain the following `%' escapes:
       %f   Current file name with directory and extension stripped.
       %F   Current file name relative to directory of master file.
       %u   User login name, on systems which support this.

    Example: In a file `intro.tex', \"eq:%f:\" will become \"eq:intro:\").

3.  Format string for reference insert in buffer.  `%s' will be replaced by
    the label.
    When the format starts with `~', the `~' will only be inserted if
    there is not already a whitespace before point.

4.  Indication on how to find the short context.
    - If nil, use the text following the \\label{...} macro.
    - If t, use
       - the section heading for section labels.
       - text following the \\begin{...} statement of environments.
         (not a good choice for environments like eqnarray or enumerate,
         where one has several labels in a single environment).
       - text after the macro name (starting with the first arg) for macros.
    - If an integer, use the nth argument of the macro.  As a special case,
      1000 means to get text after the last macro argument.
    - If a string, use as regexp to search *backward* from the label.  Context
      is then the text following the end of the match.  E.g. putting this to
      \"\\\\\\\\caption[[{]\" will use the caption in a figure or table
      environment.
      \"\\\\\\\\begin{eqnarray}\\\\|\\\\\\\\\\\\\\\\\" works for eqnarrays.
    - If any of `caption', `item', `eqnarray-like', `alignat-like', this
      symbol will internally be translated into an appropriate regexp
      (see also the variable `reftex-default-context-regexps').
    - If a function, call this function with the name of the environment/macro
      as argument.  On call, point will be just after the \\label macro.  The
      function is expected to return a suitable context string.  It should
      throw an exception (error) when failing to find context.
      As an example, here is a function returning the 10 chars following
      the label macro as context:

        (defun my-context-function (env-or-mac)
          (if (> (point-max) (+ 10 (point)))
              (buffer-substring (point) (+ 10 (point)))
            (error \"Buffer too small\")))

    Label context is used in two ways by RefTeX: For display in the label
    menu, and to derive a label string.  If you want to use a different
    method for each of these, specify them as a dotted pair.
    E.g. `(nil . t)' uses the text after the label (nil) for display, and
    text from the default position (t) to derive a label string.  This is
    actually used for section labels.

    Setting the variable `reftex-use-text-after-label-as-context' to t
    overrides the setting here.

5.  List of magic words which identify a reference to be of this type.
    If the word before point is equal to one of these words when calling
    `reftex-reference', the label list offered will be automatically
    restricted to labels of the correct type.
    If the first element of this wordlist is the symbol `regexp', the
    strings are interpreted as regular expressions.  RefTeX will add
    a \"\\\\W\" to the beginning and other stuff to the end of the regexp.

If the type indicator characters of two or more entries are the same, RefTeX
will use
 - the first non-nil format and prefix
 - the magic words of all involved entries.

Any list entry may also be a symbol.  If that has an association in
`reftex-label-alist-builtin', the cddr of that association is spliced into the
list.  However, builtin defaults should normally be set with the variable
`reftex-default-label-alist-entries."
  :group 'reftex-defining-label-environments
  :set 'reftex-set-dirty
  :type
  `(repeat
    (choice
     :value ("" ?a nil nil nil nil)
     (list :tag "Detailed label alist entry"
           :value ("" ?a nil nil nil nil)
           (choice    :tag "Environment or \\macro "
                      (const  :tag "Ignore, just use typekey" nil)
                      (string ""))
           (character :tag "Typekey character     " ?a)
           (choice    :tag "Label prefix string   "
                      (const  :tag "Default" nil)
                      (string :tag "String" "lab:"))
           (choice    :tag "Label reference format"
                      (const  :tag "Default" nil)
                      (string :tag "String" "~\\ref{%s}"))
           (choice :tag "Context"
                   (choice
                    :tag "1 method"
                    ,@reftex-tmp)
                   (cons :tag "Split methods"
                         (choice
                          :tag "  Display context     "
                          ,@reftex-tmp)
                         (choice
                          :tag "  Derive label context"
                          ,@reftex-tmp)))
	   (repeat :tag "List of Magic Words" (string)))
     (choice
      :tag "Package"
      :value AMSTeX
      ,@(mapcar
         (function
          (lambda (x)
            (list 'const ':tag (concat (symbol-name (nth 0 x)))
                  (nth 0 x))))
         reftex-label-alist-builtin)))))

;; LaTeX section commands and level numbers
(defcustom reftex-section-levels
  '(
    ("part"            . 0)
    ("chapter"         . 1)
    ("section"         . 2)
    ("subsection"      . 3)
    ("subsubsection"   . 4)
    ("paragraph"       . 5)
    ("subparagraph"    . 6)
    ("subsubparagraph" . 7)
    )
  "Commands and levels used for defining sections in the document.
The car of each cons cell is the name of the section macro.  The cdr is a
number indicating its level."
  :group 'reftex-defining-label-environments
  :set 'reftex-set-dirty
  :type '(repeat
          (cons (string :tag "sectioning macro" "")
                (number :tag "level           " 0))))

(defcustom reftex-default-context-regexps
  '((caption       . "\\\\\\(rot\\)?caption\\*?[[{]")
    (item          . "\\\\item\\(\\[[^]]*\\]\\)?")
    (eqnarray-like . "\\\\begin{%s}\\|\\\\\\\\")
    (alignat-like  . "\\\\begin{%s}{[0-9]*}\\|\\\\\\\\"))
"Alist with default regular expressions for finding context.
The form (format regexp (regexp-quote environment)) is used to calculate
the final regular expression - so %s will be replaced with the environment
or macro."
  :group 'reftex-defining-label-environments
  :type '(repeat (cons (symbol) (regexp))))
  
(defcustom reftex-use-text-after-label-as-context nil
  "*t means, grab context from directly after the \\label{..} macro.
This is the fastest method for obtaining context of the label definition, but
requires discipline when placing labels.  Setting this variable to t takes
precedence over the individual settings in `reftex-label-alist'.
This variable may be set to t, nil, or a string of label type letters
indicating the label types for which it should be true."
  :group 'reftex-defining-label-environments
  :set 'reftex-set-dirty
  :type '(choice
          (const :tag "on" t) (const :tag "off" nil)
          (string :tag "Selected label types")))

;; Label insertion

(defgroup reftex-making-and-inserting-labels nil
  "Options on how to create new labels."
  :group 'reftex-label-support)

(defcustom reftex-insert-label-flags '("s" "sft")
  "Flags governing label insertion.  First flag DERIVE, second flag PROMPT.

If DERIVE is t, RefTeX will try to derive a sensible label from context.
A section label for example will be derived from the section heading.
The conversion of the context to a legal label is governed by the
specifications given in `reftex-derive-label-parameters'.
If RefTeX fails to derive a label, it will prompt the user.
If DERIVE is nil, the label generated will consist of the prefix and a
unique number, like `eq:23'.

If PROMPT is t, the user will be prompted for a label string.  The prompt will
already contain the prefix, and (if DERIVE is t) a default label derived from
context.  When PROMPT is nil, the default label will be inserted without
query.

So the combination of DERIVE and PROMPT controls label insertion.  Here is a
table describing all four possibilities:

DERIVE   PROMPT      ACTION
-------------------------------------------------------------------------
 nil     nil     Insert simple label, like eq:22 or sec:13.  No query.
 nil     t       Prompt for label.
 t       nil     Derive a label from context and insert without query.
 t       t       Derive a label from context and prompt for confirmation.

Each flag may be set to t, nil, or a string of label type letters
indicating the label types for which it should be true.  The strings work
like character classes.
Thus, the combination may be set differently for each label type.  The
default settings \"s\" and \"sft\" mean: Derive section labels from headings
(with confirmation).  Prompt for figure and table labels.  Use simple labels
without confirmation for everything else.
The available label types are: s (section), f (figure), t (table), i (item),
e (equation), n (footnote), plus any definitions in `reftex-label-alist'."
  :group 'reftex-making-and-inserting-labels
  :type  '(list (choice :tag "Derive label from context"
                         (const  :tag "always" t)
                         (const  :tag "never" nil)
                         (string :tag "selected label types" ""))
                (choice :tag "Prompt for label string  "
                        :entry-format "  %b %v"
                        (const  :tag "always" t)
                        (const  :tag "never" nil)
                        (string :tag "selected label types" ""))))

(defcustom reftex-derive-label-parameters '(3 20 t 1 "-"
         ("the" "on" "in" "off" "a" "for" "by" "of" "and" "is"))
  "Parameters for converting a string into a label.
NWORDS      Number of words to use.
MAXCHAR     Maximum number of characters in a label string.
ILLEGAL     nil: Throw away any words containing characters illegal in labels.
            t:   Throw away only the illegal characters, not the whole word.
ABBREV      nil: Never abbreviate words.
            t:   Always abbreviate words (see `reftex-abbrev-parameters').
            not t and not nil: Abbreviate words if necessary to shorten
                               label string below MAXCHAR.
SEPARATOR   String separating different words in the label.
IGNOREWORDS List of words which should not be part of labels."
  :group 'reftex-making-and-inserting-labels
  :type  '(list (integer :tag "Number of words            "  3)
                (integer :tag "Maximum label length       " 20)
                (choice  :tag "Illegal characters in words"
                         (const :tag "throw away entire word" nil)
                         (const :tag "throw away single chars" t))
                (choice  :tag "Abbreviate words           "
                         (const :tag "never" nil)
                         (const :tag "always" t)
                         (const :tag "when label is too long" 1))
                (string  :tag "Separator between words    " "-")
                (repeat  :tag "Ignore words"
                         :entry-format "           %i %d %v"
                         (string :tag ""))))

(defcustom reftex-label-illegal-re "[\000-\040\177-\377\\\\#$%&~^_{}]"
  "Regexp matching characters not legal in labels.
For historic reasons, this character class comes *with* the [] brackets."
  :group 'reftex-making-and-inserting-labels
  :type '(regexp :tag "Character class"))

(defcustom reftex-abbrev-parameters '(4 2 "^saeiou" "aeiou")
  "Parameters for abbreviation of words.
MIN-CHARS    Minimum number of characters remaining after abbreviation.
MIN-KILL     Minimum number of characters to remove when abbreviating words.
BEFORE       Character class before abbrev point in word.
AFTER        Character class after  abbrev point in word."
  :group 'reftex-making-and-inserting-labels
  :type '(list
          (integer :tag "Minimum chars per word" 4)
          (integer :tag "Shorten by at least   " 2)
          (string  :tag "cut before char class " "^saeiou")
          (string  :tag "cut after  char class " "aeiou")))

(defcustom reftex-format-label-function nil
  "Function which produces the string to insert as a label definition.
Normally should be nil, unless you want to do something fancy.
The function will be called with two arguments, the LABEL and the DEFAULT
FORMAT, which usually is `\label{%s}'.  The function should return the
string to insert into the buffer."
  :group 'reftex-making-and-inserting-labels
  :version "20.3"
  :type 'function)

;; Label referencing

(defgroup reftex-referencing-labels nil
  "Options on how to reference labels."
  :group 'reftex-label-support)

(eval-and-compile
  (defconst reftex-tmp
    '((const :tag "on" t)
      (const :tag "off" nil)
      (string :tag "Selected label types"))))

(defcustom reftex-label-menu-flags '(t t nil nil nil nil t nil)
  "List of flags governing the label menu makeup.
The flags are:

TABLE-OF-CONTENTS  Show the labels embedded in a table of context.
SECTION-NUMBERS    Include section numbers (like 4.1.3) in table of contents.
COUNTERS           Show counters.  This just numbers the labels in the menu.
NO-CONTEXT         Non-nil means do NOT show the short context.
FOLLOW             Follow full context in other window.
SHOW-COMMENTED     Show labels from regions which are commented out.
MATCH-IN-TOC       Obsolete flag.
SHOW FILES         Show begin and end of included files.

Each of these flags can be set to t or nil, or to a string of type letters
indicating the label types for which it should be true.  These strings work
like character classes in regular expressions.  Thus, setting one of the
flags to \"sf\" makes the flag true for section and figure labels, nil
for everything else.  Setting it to \"^sf\" makes it the other way round.
The available label types are: s (section), f (figure), t (table), i (item),
e (equation), n (footnote), plus any definitions in `reftex-label-alist'.

Most options can also be switched from the label menu itself - so if you
decide here to not have a table of contents in the label menu, you can still
get one interactively during selection from the label menu."
  :group 'reftex-referencing-labels
  :type
  `(list
    (choice :tag "Embed in table of contents      " ,@reftex-tmp)
    (choice :tag "Show section numbers            " ,@reftex-tmp)
    (choice :tag "Show individual counters        " ,@reftex-tmp)
    (choice :tag "Hide short context              " ,@reftex-tmp)
    (choice :tag "Follow context in other window  " ,@reftex-tmp)
    (choice :tag "Show commented labels           " ,@reftex-tmp)
    (choice :tag "Obsolete flag,  Don't use.      " ,@reftex-tmp)
    (choice :tag "Show begin/end of included files" ,@reftex-tmp)))

(defcustom reftex-vref-is-default nil
  "*Non-nil means, the varioref macro \\vref is used as default.
In the selection buffer, the `v' key toggles the reference macro between 
`\\ref' and `\\vref'.  The value of this variable determines the default
which is active when entering the selection process.
Instead of nil or t, this may also be a string of type letters indicating
the label types for which it should be true."
  :group  'reftex-referencing-labels
  :type `(choice :tag "\\vref is default macro" ,@reftex-tmp))

(defcustom reftex-level-indent 2
  "*Number of spaces to be used for indentation per section level."
  :group 'reftex-referencing-labels
  :type '(integer))

(defcustom reftex-guess-label-type t
  "*Non-nil means, `reftex-reference' will try to guess the label type.
To do that, RefTeX will look at the word before the cursor and compare it with
the words given in `reftex-label-alist'.  When it finds a match, RefTeX will
immediately offer the correct label menu - otherwise it will prompt you for
a label type.  If you set this variable to nil, RefTeX will always prompt."
  :group 'reftex-referencing-labels
  :type '(boolean))

(defcustom reftex-format-ref-function nil
  "Function which produces the string to insert as a reference.
Normally should be nil, because the format to insert a reference can 
already be specified in `reftex-label-alist'.
The function will be called with two arguments, the LABEL and the DEFAULT
FORMAT, which normally is `~\ref{%s}'.  The function should return the
string to insert into the buffer."
  :group 'reftex-referencing-labels
  :version "20.3"
  :type 'function)

;; BibteX citation configuration ----------------------------------------

(defgroup reftex-citation-support nil
  "Support for referencing bibliographic data with BibTeX."
  :group 'reftex)

(defcustom reftex-bibpath-environment-variables '("BIBINPUTS" "TEXBIB")
  "*List of env vars which might contain the path to BibTeX database files."
  :group 'reftex-citation-support
  :set 'reftex-set-dirty
  :type '(repeat (string :tag "Environment variable")))

(defcustom reftex-bibfile-ignore-list nil
  "*List of files in \\bibliography{..} RefTeX should *not* parse.
The file names have to be in the exact same form as in the bibliography
macro - i.e. without the `.bib' extension.
Intended for files which contain only `@string' macro definitions and the
like, which are ignored by RefTeX anyway."
  :group 'reftex-citation-support
  :version "20.3"
  :set 'reftex-set-dirty
  :type '(repeat (string :tag "File name")))

(defcustom reftex-default-bibliography nil
  "*List of BibTeX database file which should be used if none are specified.
When `reftex-citation' is called from a document which has neither a
`\bibliography{..}' statement nor a `thebibliography' environment,
RefTeX will scan these files instead.  Intended for using
`reftex-citation' in non-LaTeX files."
  :group 'reftex-citation-support
  :version "20.3"
  :type '(repeat (file)))

(defcustom reftex-sort-bibtex-matches 'reverse-year
  "*Sorting of the entries found in BibTeX databases by reftex-citation.
Possible values:
nil            Do not sort entries.
'author        Sort entries by author name.
'year          Sort entries by increasing year.
'reverse-year  Sort entries by decreasing year."
  :group 'reftex-citation-support
  :type '(choice (const :tag "not" nil)
                 (const :tag "by author" author)
                 (const :tag "by year"   year)
                 (const :tag "by year, reversed" reverse-year)))

(defcustom reftex-cite-format 'default
  "*The format of citations to be inserted into the buffer.
It can be a string or an alist.  In the simplest case this is just
the string \"\\cite{%l}\", which is also the default.  See the
definition of `reftex-cite-format-builtin' for more complex examples.

If `reftex-cite-format' is a string, it will be used as the format.
In the format, the following percent escapes will be expanded.

%l   The BibTeX label of the citation.
%a   List of author names, see also `reftex-cite-punctuation.
%2a  Like %a, but abbreviate more than 2 authors like Jones et al.
%A   First author name only.
%e   Works like %a, but on list of editor names. (%2e and %E work a well)

It is also possible to access all other BibTeX database fields:
%b booktitle     %c chapter        %d edition    %h howpublished
%i institution   %j journal        %k key        %m month
%n number        %o organization   %p pages      %P first page
%r address       %s school         %u publisher  %t title
%v volume        %y year

Usually, only %l is needed.  Try, however, (setq reftex-comment-citations t).

Beware that all this only works when using BibTeX database files.  When
citations are made from the \\bibitems in an explicit thebibliography
environment, only %l is available.

If `reftex-cite-format' is an alist of characters and strings, the user
will be prompted for a character to select one of the possible format
strings.
  In order to configure this variable, you can either set
`reftex-cite-format' directly yourself or set it to the SYMBOL of one of
the predefined styles (see `reftex-cite-format-builtin').  E.g.:
(setq reftex-cite-format 'harvard)"
  :group 'reftex-citation-support
  :type
  `(choice
    :format "%{%t%}: \n%[Value Menu%] %v"
    (radio :tag "Symbolic Builtins"
           :indent 4
           :value default
           ,@(mapcar
              (function
               (lambda (x)
                 (list 'const ':tag (concat (symbol-name (nth 0 x))
                                            ": " (nth 1 x))
                       (nth 0 x))))
              reftex-cite-format-builtin))
    (string :tag "format string" "\\cite{%l}")
    (repeat :tag "key-ed format strings"
            :value ((?\r . "\\cite{%l}")
                    (?t  . "\\cite{%l}") (?p . "\\cite{%l}"))
            (cons (character :tag "Key character" ?\r)
                  (string    :tag "Format string" "")))))

(defcustom reftex-comment-citations nil
  "*Non-nil means add a comment for each citation describing the full entry.
The comment is formatted according to `reftex-cite-comment-format'."
  :group 'reftex-citation-support
  :type '(boolean))

(defcustom reftex-cite-comment-format
  "%% %2a %y, %j %v, %P, %e: %b, %u, %s %<\n"
  "Citation format used for commented citations.  Must NOT contain %l."
  :group 'reftex-citation-support
  :type '(string))

(defcustom reftex-cite-punctuation '(", " " \\& " " {\\it et al.}")
  "Punctuation for formatting of name lists in citations.
This is a list of 3 strings.
1. normal names separator, like \", \"     in Jones, Brown and Miller
2. final names separator,  like \" and \"  in Jones, Brown and Miller
3. The \"et al\" string,   like \" {\\it et al.}\" in Jones {\\it et al.}"
  :group 'reftex-citation-support
  :type '(list
          (string :tag "Separator for names            ")
          (string :tag "Separator for last name in list")
          (string :tag "string used as et al.          ")))

(defcustom reftex-format-cite-function nil
  "Function which produces the string to insert as a citation.
Normally should be nil, because the format to insert a reference can 
already be specified in `reftex-cite-format'.
The function will be called with two arguments, the CITATION KEY and the
DEFAULT FORMAT, which is taken from `reftex-cite-format'.  The function
should return the string to insert into the buffer."
  :group 'reftex-citation-support
  :version "20.3"
  :type 'function)

;; Table of contents configuration --------------------------------------

(defgroup reftex-table-of-contents-browser nil
  "A multifile table of contents browser."
  :group 'reftex)

(defcustom reftex-toc-follow-mode nil
  "*Non-nil means, point in *toc* buffer will cause other window to follow.
The other window will show the corresponding part of the document.
This flag can be toggled from within the *toc* buffer with the `f' key."
  :group 'reftex-table-of-contents-browser
  :type '(boolean))

;; Tuning the parser ----------------------------------------------------

(defgroup reftex-optimizations-for-large-documents nil
  "Configuration of parser speed and memory usage."
  :group 'reftex)

(defcustom reftex-keep-temporary-buffers 1
  "*Non-nil means, keep buffers created for parsing and lookup.
RefTeX sometimes needs to visit files related to the current document.
We distinguish files visited for
PARSING: Parts of a multifile document loaded when (re)-parsing the document.
LOOKUP:  BibTeX database files and TeX files loaded to find a reference,
         to display label context, etc.
The created buffers can be kept for later use, or be thrown away immediately
after use, depending on the value of this variable:

nil  Throw away as much as possible.
t    Keep everything.
1    Throw away buffers created for parsing, but keep the ones created
     for lookup.

If a buffer is to be kept, the file is visited normally (which is potentially
slow but will happen only once).
If a buffer is to be thrown away, the initialization of the buffer depends
upon the variable `reftex-initialize-temporary-buffers'."
  :group 'reftex-miscellaneous-configurations
  :version "20.3"
  :type '(choice
          (const :tag "Throw away everything" nil)
          (const :tag "Keep everything" t)
          (const :tag "Keep lookup buffers only" 1)))

(defcustom reftex-initialize-temporary-buffers nil
  "*Non-nil means do initializations even when visiting file temporarily.
When nil, RefTeX may turn off find-file hooks and other stuff to briefly
visit a file.
When t, the full default initializations are done (find-file-hook etc.).
Instead of t or nil, this variable may also be a list of hook functions to
do a minimal initialization."
  :group 'reftex-miscellaneous-configurations
  :version "20.3"
  :type '(choice
          (const :tag "Read files literally" nil)
          (const :tag "Fully initialize buffers" t)
          (repeat :tag "Hook functions" :value (nil)
           (function-item))))

(defcustom reftex-no-include-regexps '("\\.pstex_t\\'") ; default for x-symbol
  "*List of regular expressions to exclude certain input files from parsing.
If the name of a file included via \\include or \\input is matched by any
of the regular expressions in this list, that file is not parsed by RefTeX."
  :group 'reftex-optimizations-for-large-documents
  :version "20.3"
  :type '(repeat (regexp)))

(defcustom reftex-enable-partial-scans nil
  "*Non-nil means, re-parse only 1 file when asked to re-parse.
Re-parsing is normally requested with a `C-u' prefix to many RefTeX commands,
or with the `r' key in menus.  When this option is t in a multifile document,
we will only parse the current buffer, or the file associated with the label
or section heading near point in a menu.  Requesting re-parsing of an entire
multifile document then requires a `C-u C-u' prefix or the capital `R' key
in menus."
  :group 'reftex-optimizations-for-large-documents
  :version "20.3"
  :type 'boolean)

(defcustom reftex-allow-automatic-rescan t
  "*Non-nil means, RefTeX may rescan the document when this seems necessary.
Currently this applies only to rescanning after label insertion, when
the new label cannot be inserted correctly into the internal label
list."
  :group 'reftex-optimizations-for-large-documents
  :version "20.3"
  :type 'boolean)

(defcustom reftex-save-parse-info nil
  "*Non-nil means, save information gathered with parsing in a file.
The file MASTER.rel in the same directory as MASTER.tex is used to save the
information.  When this variable is t, 
- accessing the parsing information for the first time in an editing session
  will read that file (if available) instead of parsing the document.
- each time (part of) the document is rescanned, a new version of the file
  is written."
  :group 'reftex-optimizations-for-large-documents
  :version "20.3"
  :type 'boolean)

(defcustom reftex-use-multiple-selection-buffers nil
  "*Non-nil means use a separate selection buffer for each label type.
These buffers are kept from one selection to the next and need not to be
created for each use - so the menu generally comes up faster.  The
selection buffers will be erased (and therefore updated) automatically
when new labels in its category are added.  See the variable
`reftex-auto-update-selection-buffers'."
  :group 'reftex-optimizations-for-large-documents
  :version "20.3"
  :type 'boolean)

(defcustom reftex-auto-update-selection-buffers t
  "*Non-nil means, selection buffers will be updated automatically.
When a new label is defined with `reftex-label', all selection buffers
associated with that label category are emptied, in order to force an
update upon next use.  When nil, the buffers are left alone and have to be
updated by hand, with the `g' key from the label selection process.
The value of this variable will only have any effect when
`reftex-use-multiple-selection-buffers' is non-nil."
  :group 'reftex-optimizations-for-large-documents
  :version "20.3"
  :type 'boolean)

;; Fontification and Faces ----------------------------------------------

(defgroup reftex-fontification-configurations nil
  "Options concerning the faces used in RefTeX."
  :group 'reftex)

(defcustom reftex-use-fonts t
  "*Non-nil means, use fonts in *toc* and selection buffers.
Font-lock must be loaded as well to actually get fontified display."
  :group 'reftex-fontification-configurations
  :type '(boolean))

(defcustom reftex-refontify-context 1
  "*Non-nil means, re-fontify the context in the label menu with font-lock.
This slightly slows down the creation of the label menu.  It is only necessary
when you definitely want the context fontified.

This option may have 3 different values:
nil  Never refontify.
t    Always refontify.
1    Refontify when absolutely necessary, e.g. when with the x-symbol package.
The option is ignored when `reftex-use-fonts' is nil."
  :group 'reftex-fontification-configurations
  :version "20.3"
  :type '(choice
          (const :tag "Never" nil)
          (const :tag "Always" t)
          (const :tag "When necessary" 1)))

(defcustom reftex-highlight-selection 'cursor
  "*Non-nil mean, highlight selected text in selection and *toc* buffers.
Normally, the text near the cursor is the selected text, and it is
highlighted.  This is the entry most keys in the selction and *toc*
buffers act on.  However, if you mainly use the mouse to select an
item, you may find it nice to have mouse-triggered highlighting
instead or as well. The variable may have one of these values:

   nil      No highlighting.
   cursor   Highlighting is cursor driven.
   mouse    Highlighting is mouse driven.
   both     Both cursor and mouse trigger highlighting."
  :group 'reftex-fontification-configurations
  :version "20.3"
  :type '(choice
	  (const :tag "Never" nil)
	  (const :tag "Cursor driven" cursor)
	  (const :tag "Mouse driven" mouse)
	  (const :tag "Mouse and Cursor driven." both)))

(defcustom reftex-cursor-selected-face 'highlight
  "Face name to highlight cursor selected item in toc and selection buffers.
See also the variable `reftex-highlight-selection'."
  :group 'reftex-fontification-configurations
  :version "20.3"
  :type 'symbol)
(defcustom reftex-mouse-selected-face 'secondary-selection
  "Face name to highlight mouse selected item in toc and selection buffers.
See also the variable `reftex-highlight-selection'."
  :group 'reftex-fontification-configurations
  :version "20.3"
  :type 'symbol)
(defcustom reftex-file-boundary-face 'font-lock-comment-face
  "Face name for file boundaries in selection buffer."
  :group 'reftex-fontification-configurations
  :version "20.3"
  :type 'symbol)
(defcustom reftex-label-face 'font-lock-constant-face
  "Face name for labels in selection buffer."
  :group 'reftex-fontification-configurations
  :version "20.3"
  :type 'symbol)
(defcustom reftex-section-heading-face 'font-lock-function-name-face
  "Face name for section headings in toc and selection buffers."
  :group 'reftex-fontification-configurations
  :version "20.3"
  :type 'symbol)
(defcustom reftex-toc-header-face 'font-lock-comment-face
  "Face name for the header of a toc buffer."
  :group 'reftex-fontification-configurations
  :version "20.3"
  :type 'symbol)
(defcustom reftex-bib-author-face 'font-lock-keyword-face
  "Face name for author names in bib selection buffer."
  :group 'reftex-fontification-configurations
  :version "20.3"
  :type 'symbol)
(defcustom reftex-bib-year-face 'font-lock-comment-face
  "Face name for year in bib selection buffer."
  :group 'reftex-fontification-configurations
  :version "20.3"
  :type 'symbol)
(defcustom reftex-bib-title-face 'font-lock-function-name-face
  "Face name for article title in bib selection buffer."
  :group 'reftex-fontification-configurations
  :version "20.3"
  :type 'symbol)
(defcustom reftex-bib-extra-face 'font-lock-comment-face
  "Face name for bibliographic information in bib selection buffer."
  :group 'reftex-fontification-configurations
  :version "20.3"
  :type 'symbol)

;; Miscellaneous configurations -----------------------------------------

(defgroup reftex-miscellaneous-configurations nil
  "Collection of further configurations."
  :group 'reftex)

(defcustom reftex-extra-bindings nil
  "Non-nil means, make additional key bindings on startup.
These extra bindings are located in the users `C-c letter' map."
  :group 'reftex-miscellaneous-configurations
  :type '(boolean))

(defcustom reftex-plug-into-AUCTeX nil
  "*Plug-in flags for AUCTeX interface.
This variable is a list of 4 boolean flags.  When a flag is non-nil, it
means:

  Flag 1:  use `reftex-label'     as `LaTeX-label-function'.
  Flag 2:  use `reftex-arg-label' as `TeX-arg-label'
  Flag 3:  use `reftex-arg-ref'   as `TeX-arg-ref'
  Flag 4:  use `reftex-arg-cite'  as `TeX-arg-cite'

You may also set the variable itself to t or nil in order to turn all
plug-ins on or off, respectively.
\\<LaTeX-mode-map>`LaTeX-label-function' is the function used for label insertion when you
enter a new environment in AUCTeX with \\[LaTeX-environment].
The `TeX-arg-label' etc. functions are for entering macro arguments during
macro insertion with \\[TeX-insert-macro].
See the AUCTeX documentation for more information.
RefTeX uses `fset' to take over the function calls.  Changing the variable
may require a restart of Emacs in order to become effective."
  :group 'reftex-miscellaneous-configurations
  :type '(choice (const :tag "No plug-ins" nil)
                 (const :tag "All possible plug-ins" t)
                 (list
                  :tag "Individual choice"
                  :value (nil nil nil nil)
                  (boolean :tag "Use reftex-label     as LaTeX-label-function")
                  (boolean :tag "Use reftex-arg-label as TeX-arg-label       ")
                  (boolean :tag "Use reftex-arg-ref   as TeX-arg-ref         ")
                  (boolean :tag "Use reftex-arg-cite  as TeX-arg-cite        ")
                  )))

(defcustom reftex-auto-show-entry 'copy
  "*Non-nil means, do something when context in other window is hidden.
Some modes like `outline-mode' or `folding-mode' hide parts of buffers.
When RefTeX is asked to show context for a label definition, and the context
is invisible, it can unhide that section permanently (value t), or copy the
context to a temporary buffer (value 'copy)."
  :group 'reftex-miscellaneous-configurations
  :type '(radio :value copy
                :indent 4
          (const :tag "Do nothing" nil)
          (const :tag "Unhide section permanently" t)
          (const :tag "Copy context to show" copy)))

(defcustom reftex-revisit-to-follow t
  "*Non-nil means, follow-mode will revisit files if necessary.
When nil, follow-mode will be suspended for stuff in unvisited files."
  :group 'reftex-miscellaneous-configurations
  :version "20.3"
  :type '(boolean))

(defcustom reftex-load-hook nil
  "Hook which is being run when loading reftex.el."
  :group 'reftex-miscellaneous-configurations
  :type 'hook)

(defcustom reftex-mode-hook nil
  "Hook which is being run when turning on RefTeX mode."
  :group 'reftex-miscellaneous-configurations
  :type 'hook)

;;; End of Configuration Section ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;===========================================================================
;;;
;;; Define the formal stuff for a minor mode named RefTeX.
;;;

;; This file corresponds to RefTeX version 3.22

(defvar reftex-mode nil
  "Determines if RefTeX minor mode is active.")
(make-variable-buffer-local 'reftex-mode)

(defvar reftex-mode-map (make-sparse-keymap)
  "Keymap for RefTeX minor mode.")

(defvar reftex-mode-menu nil)

;;;###autoload
(defun turn-on-reftex ()
  "Turn on RefTeX minor mode."
  (reftex-mode t))

;;;###autoload
(defun reftex-mode (&optional arg)
  "Minor mode with distinct support for \\label, \\ref and \\cite in LaTeX.

Labels can be created with `\\[reftex-label]' and referenced with `\\[reftex-reference]'.
When referencing, you get a menu with all labels of a given type and
context of the label definition.  The selected label is inserted as a
\\ref macro.

Citations can be made with `\\[reftex-citation]' which will use a regular expression
to pull out a *formatted* list of articles from your BibTeX
database.  The selected citation is inserted as a \\cite macro.

A Table of Contents of the entire (multifile) document with browsing
capabilities is available with `\\[reftex-toc]'.

Most command have help available on the fly.  This help is accessed by
pressing `?' to any prompt mentioning this feature.

Extensive documentation about RefTeX is in the file header of `reftex.el'.
You can view this information with `\\[reftex-show-commentary]'.

\\{reftex-mode-map}
Under X, these and other functions will also be available as `Ref' menu
on the menu bar.

------------------------------------------------------------------------------"

  (interactive "P")
  (setq reftex-mode (not (or (and (null arg) reftex-mode)
                             (<= (prefix-numeric-value arg) 0))))

  ; Add or remove the menu, and run the hook
  (if reftex-mode
      (progn
        (easy-menu-add reftex-mode-menu)
        (reftex-plug-into-AUCTeX)
        (run-hooks 'reftex-mode-hook))
    (easy-menu-remove reftex-mode-menu)))

(or (assoc 'reftex-mode minor-mode-alist)
    (push '(reftex-mode " Ref") minor-mode-alist))

(or (assoc 'reftex-mode minor-mode-map-alist)
    (push (cons 'reftex-mode reftex-mode-map) minor-mode-map-alist))

;;; ===========================================================================
;;;
;;; Silence warnings about variables in other packages.
(defvar TeX-master)
(defvar LaTeX-label-function)
(defvar tex-main-file)
(defvar outline-minor-mode)
(defvar font-lock-fontify-region-function)
(defvar font-lock-syntactic-keywords)

;;; ===========================================================================
;;;
;;; Interfaces for other packages
;;; -----------------------------
;;;
;;; AUCTeX
;;; ------

(defun reftex-arg-label (optional &optional prompt definition)
  "Use `reftex-label' to create label.  Insert it with `TeX-argument-insert'.
This function is intended for AUCTeX macro support."
  (let ((label (reftex-label nil t)))
    (if (and definition (not (string-equal "" label)))
        (LaTeX-add-labels label))
    (TeX-argument-insert label optional optional)))

(defun reftex-arg-ref (optional &optional prompt definition)
  "Use `reftex-reference' to select label.  Insert with `TeX-argument-insert'.
This function is intended for AUCTeX macro support."
  (let ((label (reftex-reference nil t)))
    (if (and definition (not (string-equal "" label)))
        (LaTeX-add-labels label))
    (TeX-argument-insert label optional optional)))

(defun reftex-arg-cite (optional &optional prompt definition)
  "Use reftex-citation to select a key.  Insert with `TeX-argument-insert'.
This function is intended for AUCTeX macro support."
  (let ((key (reftex-citation t)))
    (TeX-argument-insert (or key "") optional optional)))

(defun reftex-plug-into-AUCTeX ()
  ;; Replace AUCTeX functions with RefTeX functions.
  ;; Which functions are replaced is controlled by the variable
  ;; `reftex-plug-into-AUCTeX'.
  (let ((flags
         (cond ((eq reftex-plug-into-AUCTeX t) '(t t t t))
               ((eq reftex-plug-into-AUCTeX nil) '(nil nil nil nil))
               (t reftex-plug-into-AUCTeX))))

    (and (nth 0 flags)
         (boundp 'LaTeX-label-function)
         (setq LaTeX-label-function 'reftex-label))

    (and (nth 1 flags)
         (fboundp 'TeX-arg-label)
         (fset 'TeX-arg-label 'reftex-arg-label))

    (and (nth 2 flags)
         (fboundp 'TeX-arg-ref)
         (fset 'TeX-arg-ref 'reftex-arg-ref))

    (and (nth 3 flags)
         (fboundp 'TeX-arg-cite)
         (fset 'TeX-arg-cite 'reftex-arg-cite))))


(defvar reftex-label-alist-external-add-ons nil
  "List of label alist entries added with reftex-add-to-label-alist.")

(defun reftex-add-to-label-alist (entry-list)
  "Add label environment descriptions to `reftex-label-alist-external-add-ons'.
The format of ENTRY-LIST is exactly like `reftex-label-alist'.  See there
for details.
This function makes it possible to support RefTeX from AUCTeX style files.
The entries in ENTRY-LIST will be processed after the user settings in
`reftex-label-alist', and before the defaults (specified in
`reftex-default-label-alist-entries').  Any changes made to
`reftex-label-alist-external-add-ons' will raise a flag to the effect that a
mode reset is done on the next occasion."
  (let (entry)
    (while entry-list
      (setq entry (car entry-list)
            entry-list (cdr entry-list))
      (unless (member entry reftex-label-alist-external-add-ons)
        (setq reftex-tables-dirty t)
        (push entry reftex-label-alist-external-add-ons)))))

;;; ===========================================================================
;;;
;;; Multifile support
;;;
;;; Technical notes: Multifile works as follows: We keep just one list
;;; of labels for each master file - this can save a lot of memory.
;;; `reftex-master-index-list' is an alist which connects the true file name
;;; of each master file with the symbols holding the information on that
;;; document.  Each buffer has local variables which point to these symbols.

;; List of variables which handle the multifile stuff.
;; This list is used to tie, untie, and reset these symbols.
(defconst reftex-multifile-symbols
  '(reftex-docstruct-symbol))

;; Alist connecting master file names with the corresponding lisp symbols.
(defvar reftex-master-index-list nil)

;; Last index used for a master file.
(defvar reftex-multifile-index 0)

;; Variable holding the symbol with the label list of the document.
(defvar reftex-docstruct-symbol nil)
(make-variable-buffer-local 'reftex-docstruct-symbol)

(defun reftex-next-multifile-index ()
  ;; Return the next free index for multifile symbols.
  (incf reftex-multifile-index))

(defun reftex-tie-multifile-symbols ()
  ;; Tie the buffer-local symbols to globals connected with the master file.
  ;; If the symbols for the current master file do not exist, they are created.

  (let* ((master (file-truename (reftex-TeX-master-file)))
         (index (assoc master reftex-master-index-list))
         (symlist reftex-multifile-symbols)
         (symbol nil)
         (symname nil)
         (newflag nil))
    ;; Find the correct index.
    (if index
        ;; symbols do exist
        (setq index (cdr index))
      ;; Get a new index and add info to the alist.
      (setq index (reftex-next-multifile-index)
            newflag t)
      (push (cons master index) reftex-master-index-list))

    ;; Get/create symbols and tie them.
    (while symlist
      (setq symbol (car symlist)
            symlist (cdr symlist)
            symname (symbol-name symbol))
      (set symbol (intern (concat symname "-" (int-to-string index))))
      (put (symbol-value symbol) ':master-index index)
      ;; Initialize if new symbols.
      (if newflag (set (symbol-value symbol) nil)))

    ;; Return t if the symbols did already exist, nil when we've made them.
    (not newflag)))

(defun reftex-untie-multifile-symbols ()
  ;; Remove ties from multifile symbols, so that next use makes new ones.
  (let ((symlist reftex-multifile-symbols)
        (symbol nil))
    (while symlist
      (setq symbol  (car symlist)
            symlist (cdr symlist))
      (set symbol nil))))

(defun reftex-TeX-master-file ()
  ;; Return the name of the master file associated with the current buffer.
  ;; When AUCTeX is loaded, we will use it's more sophisticated method.
  ;; We also support the default TeX and LaTeX modes by checking for a
  ;; variable tex-main-file.
  (let
      ((master
        (cond
         ((fboundp 'TeX-master-file) ; AUCTeX is loaded.  Use its mechanism.
          (TeX-master-file t))
         ((boundp 'TeX-master)       ; The variable is defined - lets use it.
          (cond
           ((eq TeX-master t)
            (buffer-file-name))
           ((eq TeX-master 'shared)
            (setq TeX-master (read-file-name "Master file: "
                                             nil nil t nil)))
           (TeX-master)
           (t
            (setq TeX-master (read-file-name "Master file: "
                                             nil nil t nil)))))
         ((boundp 'tex-main-file)
          ;; This is the variable from the default TeX modes.
          (cond
           ((stringp tex-main-file)
            ;; ok, this must be it
            tex-main-file)
           (t
            ;; In this case, the buffer is its own master.
            (buffer-file-name))))
         (t
          ;; Know nothing about master file.  Assume this is a master file.
          (buffer-file-name)))))
    (cond
     ((null master)
      (error "Need a filename for this buffer.  Please save it first."))
     ((or (file-exists-p master)
          (reftex-get-buffer-visiting master))
      ;; We either see the file, or have a buffer on it.  OK.
      )
     ((or (file-exists-p (concat master ".tex"))
          (reftex-get-buffer-visiting (concat master ".tex")))
      ;; Ahh, an extra .tex was missing...
      (setq master (concat master ".tex")))
     (t
      ;; Something is wrong here.  Throw an exception.
      (buffer-file-name)))
;      (error "No such master file %s" master)))
    (expand-file-name master)))

(defun reftex-parse-one ()
  "Re-parse this file."
  (interactive)
  (let ((reftex-enable-partial-scans t))
    (reftex-access-scan-info '(4))))

(defun reftex-parse-all ()
  "Re-parse entire document."
  (interactive)
  (reftex-access-scan-info '(16)))

(defun reftex-all-document-files (&optional relative)
  ;; Return a list of all files belonging to the current document.
  ;; When RELATIVE is non-nil, give file names relative to directory
  ;; of master file.
  (let* ((all (symbol-value reftex-docstruct-symbol))
         (master-dir (file-name-directory (reftex-TeX-master-file)))
         (re (concat "\\`" (regexp-quote master-dir)))
        file-list tmp file)
    (while (setq tmp (assoc 'bof all))
      (setq file (nth 1 tmp)
            all (cdr (memq tmp all)))
      (and relative
           (string-match re file)
           (setq file (substring file (match-end 0))))
      (push file file-list))
    (nreverse file-list)))

(defun reftex-create-tags-file ()
  "Create TAGS file by running `etags' on the current document.
The TAGS file is also immediately visited with `visit-tags-table'."
  (interactive)
  (reftex-access-scan-info current-prefix-arg)
  (let* ((master (reftex-TeX-master-file))
         (files  (reftex-all-document-files))
         (cmd    (format "etags %s" (mapconcat 'identity files " "))))
    (save-excursion
      (set-buffer (reftex-get-buffer-visiting master))
      (message "Running etags to create TAGS file...")
      (shell-command cmd)
      (visit-tags-table "TAGS"))))

;; History of grep commands.
(defvar reftex-grep-history nil)
(defvar reftex-grep-command "grep -n "
  "Last grep command used in \\[reftex-grep-document]; default for next grep.")

(defun reftex-grep-document (grep-cmd)
  "Run grep query through all files related to this document.
With prefix arg, force to rescan document.
This works also without an active TAGS table."

  (interactive
   (list (read-from-minibuffer "Run grep on document (like this): "
                               reftex-grep-command nil nil
                               'reftex-grep-history)))
  (reftex-access-scan-info current-prefix-arg)
  (let* ((files  (reftex-all-document-files t))
         (cmd    (format
                  "%s %s" grep-cmd
                  (mapconcat 'identity files " "))))
    (grep cmd)))

(defun reftex-search-document (&optional regexp)
  "Regexp search through all files of the current TeX document.
Starts always in the master file.  Stops when a match is found.
To continue searching for next match, use command \\[tags-loop-continue].
This works also without an active TAGS table."
  (interactive)
  (let ((default (reftex-this-word)))
    (unless regexp
      (setq regexp (read-string (format "Search regexp in document [%s]: "
                                        default))))
    (if (string= regexp "") (setq regexp (regexp-quote default)))

    (reftex-access-scan-info current-prefix-arg)
    (tags-search regexp (list 'reftex-all-document-files))))

(defun reftex-query-replace-document (&optional from to delimited)
  "Run a query-replace-regexp of FROM with TO over the entire TeX document.
Third arg DELIMITED (prefix arg) means replace only word-delimited matches.
If you exit (\\[keyboard-quit] or ESC), you can resume the query replace
with the command \\[tags-loop-continue].
This works also without an active TAGS table."
  (interactive)
  (let ((default (reftex-this-word)))
    (unless from
      (setq from (read-string (format "Replace regexp in document [%s]: "
                                      default)))
      (if (string= from "") (setq from (regexp-quote default))))
    (unless to
      (setq to (read-string (format "Replace regexp %s with: " from))))
    (reftex-access-scan-info current-prefix-arg)
    (tags-query-replace from to (or delimited current-prefix-arg)
                        (list 'reftex-all-document-files))))

(defun reftex-change-label (&optional from to)
  "Query replace FROM with TO in all \\label and \\ref commands.
Works on the entire multifile document.
If you exit (\\[keyboard-quit] or ESC), you can resume the query replace
with the command \\[tags-loop-continue].
This works also without an active TAGS table."
  (interactive)
  (let ((default (reftex-this-word "-a-zA-Z0-9_*.:")))
    (unless from
      (setq from (read-string (format "Replace label globally [%s]: "
                                      default))))
    (if (string= from "") (setq from default))
    (unless to
      (setq to (read-string (format "Replace label %s with: "
                                    from))))
    (reftex-query-replace-document
     (concat "\\\\\\(label\\|[a-z]*ref\\){" (regexp-quote from) "}")
     (format "\\\\\\1{%s}" to))))

(defun reftex-save-all-document-buffers ()
  "Save all documents associated with the current document.
The function is useful after a global action like replacing or renumbering
labels."
  (interactive)
  (let ((files (reftex-all-document-files))
	file buffer)
    (save-excursion
      (while (setq file (pop files))
	(setq buffer (reftex-get-buffer-visiting file))
	(when buffer
	  (set-buffer buffer)
	  (save-buffer))))))

;;; ===========================================================================
;;;
;;; Functions to create and reference automatic labels.

;; The following constants are derived from `reftex-label-alist'.

;; Prompt used for label type queries directed to the user.
(defconst reftex-type-query-prompt nil)

;; Help string for label type queries.
(defconst reftex-type-query-help nil)

;; Alist relating label type to reference format.
(defconst reftex-typekey-to-format-alist nil)

;; Alist relating label type to label affix.
(defconst reftex-typekey-to-prefix-alist nil)

;; Alist relating environments or macros to label type and context regexp.
(defconst reftex-env-or-mac-alist nil)

;; List of macros carrying a label.
(defconst reftex-label-mac-list nil)

;; List of environments carrying a label.
(defconst reftex-label-env-list nil)

;; List of all typekey letters in use.
(defconst reftex-typekey-list nil)

;; Alist relating magic words to a label type.
(defconst reftex-words-to-typekey-alist nil)

;; The last list-of-labels entry used in a reference.
(defvar reftex-last-used-reference (list nil nil nil nil))

;; The regular expression used to abbreviate words.
(defconst reftex-abbrev-regexp
  (concat
   "\\`\\("
   (make-string (nth 0 reftex-abbrev-parameters) ?.)
   "[" (nth 2 reftex-abbrev-parameters) "]*"
   "\\)"
   "[" (nth 3 reftex-abbrev-parameters) "]"
   (make-string (1- (nth 1 reftex-abbrev-parameters)) ?.)))

;; Global variables used for communication between functions.
(defvar reftex-default-context-position nil)
(defvar reftex-location-start nil)
(defvar reftex-call-back-to-this-buffer nil)
(defvar reftex-select-return-marker (make-marker))
(defvar reftex-active-toc nil)
(defvar reftex-tex-path nil)
(defvar reftex-bib-path nil)
(defvar reftex-last-follow-point nil)
(defvar reftex-prefix)

;; List of buffers created temporarily for lookup, which should be killed.
(defvar reftex-buffers-to-kill nil)

;; Regexp to find section statements.  Computed from reftex-section-levels.
(defvar reftex-section-regexp nil)
(defvar reftex-section-or-include-regexp nil)
(defvar reftex-everything-regexp nil)
(defvar reftex-find-label-regexp-format nil)
(defvar reftex-find-label-regexp-format2 nil)

;; The parser functions ----------------------------------

(defvar reftex-memory nil
  "Memorizes old variable values to indicate changes in these variables.")

(defun reftex-access-scan-info (&optional rescan file)
  ;; Access the scanning info.  When the multifile symbols are not yet tied,
  ;; tie them.  When they are empty or RESCAN is non-nil, scan the document.
  ;; When FILE is non-nil, parse only from that file.

  ;; Reset the mode if we had changes to important variables.
  (when (or reftex-tables-dirty
	    (not (eq reftex-label-alist (nth 0 reftex-memory)))
	    (not (eq reftex-label-alist-external-add-ons
		     (nth 1 reftex-memory)))
	    (not (eq reftex-default-label-alist-entries
		     (nth 2 reftex-memory))))
    (reftex-reset-mode))

  (if (eq reftex-docstruct-symbol nil)
      ;; Symbols are not yet tied: Tie them.
      (reftex-tie-multifile-symbols))
  
  (when (or (null (symbol-value reftex-docstruct-symbol))
	    (member rescan '(t 1 (4) (16))))
    ;; The docstruct will change: Remove selection buffers.
    (save-excursion
      (reftex-erase-all-selection-buffers)))

  (if (and (null (symbol-value reftex-docstruct-symbol))
           reftex-save-parse-info)
      ;; Try to read the stuff from a file
      (reftex-access-parse-file 'read))

  (cond
   ((not (symbol-value reftex-docstruct-symbol))
    (reftex-do-parse 1 file))
   ((member rescan '(t 1 (4) (16)))
    (reftex-do-parse rescan file))))

(defun reftex-do-parse (rescan &optional file)
  ;; Do a rescan.  When allowed, do only a partial scan from FILE.

  ;; Normalize the rescan argument
  (setq rescan (cond ((eq rescan t) t)
                     ((eq rescan 1) 1)
                     ((equal rescan '(4)) t)
                     ((equal rescan '(16)) 1)
                     (t 1)))

  ;; Partial scans only when allowed
  (unless reftex-enable-partial-scans
    (setq rescan 1))

  ;; Do the scanning.

  (let* ((old-list (symbol-value reftex-docstruct-symbol))
         (master (reftex-TeX-master-file))
	 (master-dir (file-name-as-directory (file-name-directory master)))
         (file (or file (buffer-file-name)))
         from-file appendix docstruct tmp)

    ;; Make sure replacement is really an option here
    (when (and (eq rescan t)
               (not (and (member (list 'bof file) old-list)
                         (member (list 'eof file) old-list))))
      (message "Scanning whole document (no file section %s)" file)
      (setq rescan 1))
    (when (string= file master)
      (message "Scanning whole document (%s is master)" file)
      (setq rescan 1))

    ;; From which file do we start?
    (setq from-file
          (cond ((eq rescan t) (or file master))
                ((eq rescan 1) master)
                (t (error "horrible!!"))))

    ;; Find active toc entry and initialize section-numbers
    (setq reftex-active-toc (reftex-last-assoc-before-elt
			     'toc (list 'bof from-file) old-list)
	  appendix (reftex-last-assoc-before-elt
		    'appendix (list 'bof from-file) old-list))

    (reftex-init-section-numbers reftex-active-toc appendix)

    (if (eq rescan 1)
        (message "Scanning entire document...")
      (message "Scanning document from %s..." from-file))

    (save-window-excursion
      (save-excursion
        (unwind-protect
            (setq docstruct
                  (reftex-parse-from-file
                   from-file docstruct master-dir))
          (reftex-kill-temporary-buffers))))

    (message "Scanning document... done")

    ;; Turn the list around.
    (setq docstruct (nreverse docstruct))

    ;; Set or insert
    (setq docstruct (reftex-replace-label-list-segment
                     old-list docstruct (eq rescan 1)))

    ;; Add all missing information
    (unless (assq 'label-numbers docstruct)
      (push (cons 'label-numbers nil) docstruct))
    (unless (assq 'master-dir docstruct)
      (push (cons 'master-dir master-dir) docstruct))
    (let* ((bof1 (memq (assq 'bof docstruct) docstruct))
           (bof2 (assq 'bof (cdr bof1)))
           (is-multi (not (not (and bof1 bof2))))
           (entry (or (assq 'is-multi docstruct)
                      (car (push (list 'is-multi is-multi) docstruct)))))
      (setcdr entry (cons is-multi nil)))
    (unless (assq 'xr docstruct)
      (let* ((allxr (reftex-all-assq 'xr-doc docstruct))
	     (alist (mapcar
		     (function
		      (lambda (x) 
			(if (setq tmp (reftex-find-tex-file (nth 2 x)
							    master-dir))
			    (cons (nth 1 x) tmp)
			  (message "Can't find external document %s"
				   (nth 2 x))
			  nil)))
		    allxr))
	     (alist (delete nil alist))
	     (allprefix (delete nil (mapcar 'car alist)))
	     (regexp (if allprefix
			 (concat "\\`\\(" 
				 (mapconcat 'identity allprefix "\\|")
				 "\\)")
		       "\\\\\\\\\\\\")))   ; this will never match
	(push (list 'xr alist regexp) docstruct)))

    (set reftex-docstruct-symbol docstruct)

    ;; Save the parsing information into a file?
    (if reftex-save-parse-info
        (reftex-access-parse-file 'write))))

(defun reftex-is-multi ()
  ;; Tell if this is a multifile document.  When not sure, say yes.
  (let ((entry (assq 'is-multi (symbol-value reftex-docstruct-symbol))))
    (if entry
        (nth 1 entry)
      t)))

(defun reftex-parse-from-file (file docstruct master-dir)
  ;; Scan the buffer for labels and save them in a list.
  (let ((regexp reftex-everything-regexp)
        (bound 0)
        file-found tmp include-file
        (level 1)
        (highest-level 100)
        toc-entry next-buf)

    (catch 'exit
      (setq file-found (reftex-find-tex-file file master-dir))
      (unless file-found
        (push (list 'file-error file) docstruct)
        (throw 'exit nil))

      (save-excursion

        (message "Scanning file %s" file)
        (set-buffer
         (setq next-buf
               (reftex-get-file-buffer-force
                file-found
                (not (eq t reftex-keep-temporary-buffers)))))

        ;; Begin of file mark
        (setq file (buffer-file-name))
        (push (list 'bof file) docstruct)

        (save-excursion
          (save-restriction
            (widen)
            (goto-char 1)

            (while (re-search-forward regexp nil t)

              (cond

               ((match-end 1)
               ;; It is a label
                (push (reftex-label-info (reftex-match-string 1) file bound)
                      docstruct))

               ((match-end 3)
                ;; It is a section
                (setq bound (point))

                ;; Insert in List
                (setq toc-entry (reftex-section-info file))
                (setq level (nth 5 toc-entry))
                (setq highest-level (min highest-level level))
                (if (= level highest-level)
                    (message
                     "Scanning %s %s ..."
                     (car (nth level reftex-section-levels))
                     (nth 6 toc-entry)))

                (push toc-entry docstruct)
                (setq reftex-active-toc toc-entry))

               ((match-end 7)
                ;; It's an include or input
		(setq include-file (reftex-match-string 7))
		(if (delq nil (mapcar 
			       (lambda (x) (string-match x include-file))
			       reftex-no-include-regexps))
		    (progn
		      ;; Ignore the file
		      (push (list 'bof include-file) docstruct)
		      (push (list 'eof include-file) docstruct))
		  ;; Parse it
		  (setq docstruct
			(reftex-parse-from-file
			 include-file
			 docstruct master-dir))))

	       ((match-end 9)
		;; Appendix starts here
		(reftex-init-section-numbers nil t)
		(push (cons 'appendix t) docstruct))

               ((match-end 10)
                ;; A macro with label
                (save-excursion
                  (let* ((mac (reftex-match-string 10))
			 (label (progn (goto-char (match-end 10))
				       (save-match-data
					 (reftex-no-props
					  (reftex-nth-arg-wrapper
					   mac)))))
                         (entry (progn (goto-char (match-end 0))
                                       (reftex-label-info
					label file bound mac))))
                    (push entry docstruct))))
               (t (error "This should not happen (reftex-parse-from-file)")))
              )


            ;; Find bibliography statement
            (when (setq tmp (reftex-locate-bibliography-files master-dir))
              (push (cons 'bib tmp) docstruct))

	    (goto-char 1)
	    (when (re-search-forward 
		   "\\(\\`\\|[\n\r]\\)[ \t]*\\\\begin{thebibliography}" nil t)
	      (push (cons 'thebib file) docstruct))
		   
	    ;; Find external document specifications
	    (goto-char 1)
	    (while (re-search-forward "[\n\r][ \t]*\\\\externaldocument\\(\\[\\([^]]*\\)\\]\\)?{\\([^}]+\\)}" nil t)
	      (push (list 'xr-doc (reftex-match-string 2)
			  (reftex-match-string 3))
		    docstruct))

            ;; End of file mark
            (push (list 'eof file) docstruct))))

      ;; Kill the scanned buffer
      (reftex-kill-temporary-buffers next-buf))

    ;; Return the list
    docstruct))

(defun reftex-locate-bibliography-files (master-dir)
  ;; Scan buffer for bibliography macro and return file list.
  (let (file-list)
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward
           "\\(\\`\\|[\n\r]\\)[ \t]*\\\\bibliography{[ \t]*\\([^}]+\\)" nil t)
          (setq file-list
                (mapcar (function (lambda (x) (concat x ".bib")))
                        (reftex-delete-list
                         reftex-bibfile-ignore-list
                         (split-string
                          (reftex-match-string 2)
                          "[ \t\n\r]*,[ \t\n\r]*")))))
      (delete nil
	      (mapcar 
	       (function 
		(lambda (file)
		  (reftex-find-bib-file file master-dir)))
	       file-list)))))
  
(defun reftex-last-assoc-before-elt (key elt list)
  ;; Find the last association of KEY in LIST before or at ELT
  ;; ELT is found in LIST with equal, not eq.
  ;; Returns nil when either KEY or elt are not found in LIST.
  ;; On success, returns the association.
  (let* ((elt (car (member elt list))) ass last-ass)

    (while (and (setq ass (assoc key list))
                (setq list (memq ass list))
                (memq elt list))
      (setq last-ass ass
            list (cdr list)))
    last-ass))

(defun reftex-replace-label-list-segment (old insert &optional entirely)
  ;; Replace the segment in OLD which corresponds to INSERT.
  ;; Works with side effects, directly changes old.
  ;; If entirely is t, just return INSERT.
  ;; This function also makes sure the old toc markers do not point anywhere.

  (cond
   (entirely
    (reftex-silence-toc-markers old (length old))
    insert)
   (t (let* ((new old)
             (file (nth 1 (car insert)))
             (eof-list (member (list 'eof file) old))
             (bof-list (member (list 'bof file) old))
             n)
        (if (not (and bof-list eof-list))
            (error "Cannot splice")
          ;; Splice
          (reftex-silence-toc-markers bof-list (- (length bof-list)
                                                  (length eof-list)))
          (setq n (- (length old) (length bof-list)))
          (setcdr (nthcdr n new) (cdr insert))
          (setcdr (nthcdr (1- (length new)) new) (cdr eof-list)))
        new))))

(defun reftex-silence-toc-markers (list n)
  ;; Set all markers in list to nil
  (while (and list (> (decf n) -1))
    (and (eq (car (car list)) 'toc)
         (markerp (nth 4 (car list)))
         (set-marker (nth 4 (car list)) nil))
    (pop list)))

(defun reftex-access-parse-file (action)
  (let* ((list (symbol-value reftex-docstruct-symbol))
         (master (reftex-TeX-master-file))
	 (enable-local-variables nil)
         (file (if (string-match "\\.[a-zA-Z]+\\'" master)
                   (concat (substring master 0 (match-beginning 0)) ".rel")
                 (concat master ".rel"))))
    (cond
     ((eq action 'readable)
      (file-readable-p file))
     ((eq action 'restore)
      (if (eq reftex-docstruct-symbol nil)
          ;; Symbols are not yet tied: Tie them.
          (reftex-tie-multifile-symbols))
      (if (file-exists-p file)
          ;; load the file and return t for success
          (progn (load-file file) t)
        ;; throw an exception
        (error "No restore file %s" file)))
     ((eq action 'read)
      (if (file-exists-p file)
          ;; load the file and return t for success
          (progn (load-file file) t)
        ;; return nil for failure, but no exception
        nil))
     (t
      (save-excursion
        (if (file-writable-p file)
            (progn
              (message "Writing parse file %s" (abbreviate-file-name file))
              (find-file file)
              (erase-buffer)
              (insert (format ";; RefTeX parse info file\n"))
              (insert (format ";; File: %s\n" master))
              (insert (format ";; Date: %s\n"
                              (format-time-string "%D %T"
                                                  (current-time))))
              (insert (format ";; User: %s (%s)\n\n"
                              (user-login-name) (user-full-name)))
              (insert "(set reftex-docstruct-symbol '(\n\n")
              (let ((standard-output (current-buffer)))
                (mapcar
                 (function
                  (lambda (x)
                    (cond ((eq (car x) 'toc)
                           ;; A toc entry. Do not save the marker.
                           ;; Save the markers  position at position 8
                           (print (list 'toc "toc" (nth 2 x) (nth 3 x)
                                        nil (nth 5 x) (nth 6 x) (nth 7 x)
                                        (or (and (markerp (nth 4 x))
                                                 (marker-position (nth 4 x)))
                                            (nth 8 x)))))
                          (t (print x)))))
                 list))
              (insert "))")
              (save-buffer 0)
              (kill-buffer (current-buffer)))
          (error "Cannot write to file %s" file)))
      t))))

;; Creating labels --------------

(defun reftex-label (&optional environment no-insert)
  "Insert a unique label.  Return the label.
If ENVIRONMENT is given, don't bother to find out yourself.
If NO-INSERT is non-nil, do not insert label into buffer.
With prefix arg, force to rescan document first.
When you are prompted to enter or confirm a label, and you reply with
just the prefix or an empty string, no label at all will be inserted.
A new label is also recorded into the label list.
This function is controlled by the settings of reftex-insert-label-flags."

  (interactive)

  ;; Ensure access to scanning info and rescan buffer if prefix are is '(4).
  (reftex-access-scan-info current-prefix-arg)

  ;; Find out what kind of environment this is and abort if necessary.
  (if (or (not environment)
          (not (assoc environment reftex-env-or-mac-alist)))
      (setq environment (reftex-label-location)))
  (unless environment
    (error "Can't figure out what kind of label should be inserted"))

  ;; Ok, go ahead.
  (catch 'exit
    (let* ((entry (assoc environment reftex-env-or-mac-alist))
	   (typekey (nth 1 entry))
	   (format (nth 3 entry))
	   label prefix valid default force-prompt rescan-is-useful)
      (when (and (eq (string-to-char environment) ?\\)
		 (nth 5 entry)
		 (memq (preceding-char) '(?\[ ?\{)))
	(setq format "%s"))

      (setq prefix (or (cdr (assoc typekey reftex-typekey-to-prefix-alist))
		       (concat typekey "-")))
      ;; Replace any escapes in the prefix
      (setq prefix (reftex-replace-prefix-escapes prefix))

      ;; Make a default label.
      (cond

       ((reftex-typekey-check typekey (nth 0 reftex-insert-label-flags))
	;; Derive a label from context.
	(setq reftex-active-toc (reftex-last-assoc-before-elt
				 'toc (car (reftex-where-am-I))
				 (symbol-value reftex-docstruct-symbol)))
	(setq default (reftex-no-props
		       (nth 2 (reftex-label-info " " nil nil t))))
	;; Catch the cases where the is actually no context available.
	(if (or (string-match "NO MATCH FOR CONTEXT REGEXP" default)
		(string-match "ILLEGAL VALUE OF PARSE" default)
		(string-match "SECTION HEADING NOT FOUND" default)
		(string-match "HOOK ERROR" default)
		(string-match "^[ \t]*$" default))
	    (setq default prefix
		  force-prompt t)	; need to prompt
	  (setq default (concat prefix (reftex-string-to-label default)))

	  ;; Make it unique.
	  (setq default (reftex-uniquify-label default nil "-"))))

       ((reftex-typekey-check typekey (nth 1 reftex-insert-label-flags))
	;; Minimal default: the user will be prompted.
	(setq default prefix))

       (t
	;; Make an automatic label.
	(setq default (reftex-uniquify-label prefix t))))

      ;; Should we ask the user?
      (if (or (reftex-typekey-check typekey
				    (nth 1 reftex-insert-label-flags)) ; prompt
	      force-prompt)

	  (while (not valid)
	    ;; iterate until we get a legal label

	    (setq label (read-string
			 (if (string= format "%s") "Naked Label: " "Label: ")
			 default))

	    ;; Lets make sure that this is a legal label
	    (cond

	     ((string-match (concat "\\`\\(" (regexp-quote prefix)
				    "\\)?[ \t]*\\'")
			    label)
	      ;; No label at all, please
	      (message "No label inserted.")
	      (throw 'exit nil))

	     ;; Test if label contains strange characters
	     ((string-match reftex-label-illegal-re label)
	      (message "Label \"%s\" contains illegal characters" label)
	      (ding)
	      (sit-for 2))

	     ;; Look it up in the label list
	     ((setq entry (assoc label
				 (symbol-value reftex-docstruct-symbol)))
	      (ding)
	      (if (y-or-n-p 
		   (format "Label '%s' exists. Use anyway? " label))
		  (setq valid t)))

	     ;; Label is ok
	     (t
	      (setq valid t))))
	(setq label default))

      ;; Insert the label into the label list
      (let* ((here-I-am-info (reftex-where-am-I))
	     (here-I-am (car here-I-am-info))
	     (note (if (cdr here-I-am-info)
		       ""
		     "POSITION UNCERTAIN.  RESCAN TO FIX."))
	     (file (buffer-file-name))
	     (text nil)
	     (tail (memq here-I-am (symbol-value reftex-docstruct-symbol))))

	(or (cdr here-I-am-info) (setq rescan-is-useful t))

	(if tail
	    (setcdr tail (cons (list label typekey text file note)
			       (cdr tail)))))

      ;; Insert the label into the buffer
      (unless no-insert
	(insert
	 (if reftex-format-label-function
	     (funcall reftex-format-label-function label format)
	   (format format label))))

      ;; Delete the corresponding selection buffers to force update on next use.
      (when reftex-auto-update-selection-buffers
	(reftex-erase-buffer (reftex-make-selection-buffer-name typekey))
	(reftex-erase-buffer (reftex-make-selection-buffer-name " ")))

      (when (and rescan-is-useful reftex-allow-automatic-rescan)
	(reftex-parse-one))

      ;; return value of the function is the label
      label)))

(defun reftex-string-to-label (string)
  ;; Convert a string (a sentence) to a label.
  ;;
  ;; Uses reftex-derive-label-parameters and reftex-abbrev-parameters
  ;;

  (let* ((words0 (split-string string "[- \t\n\r]+"))
         (ignore-words (nth 5 reftex-derive-label-parameters))
         words word)

    ;; remove words from the ignore list or with funny characters
    (while (setq word (pop words0))
      (cond
       ((member (downcase word) ignore-words))
       ((string-match reftex-label-illegal-re word)
        (when (nth 2 reftex-derive-label-parameters)
          (while (string-match reftex-label-illegal-re word)
            (setq word (replace-match "" nil nil word)))
          (push word words)))
       (t
        (push word words))))
    (setq words (nreverse words))

    ;; restrict number of words
    (if (> (length words) (nth 0 reftex-derive-label-parameters))
        (setcdr (nthcdr (1- (nth 0 reftex-derive-label-parameters)) words) nil))

    ;; First, try to use all words
    (setq string (mapconcat 'identity words
                            (nth 4 reftex-derive-label-parameters)))

    ;; Abbreviate words if enforced by user settings or string length
    (if (or (eq t (nth 3 reftex-derive-label-parameters))
            (and (nth 3 reftex-derive-label-parameters)
                 (> (length string) (nth 1 reftex-derive-label-parameters))))
        (setq words
              (mapcar
	       (function
		(lambda (w) (if (string-match reftex-abbrev-regexp w)
				(match-string 1 w)
			      w)))
               words)
              string (mapconcat 'identity words
                                (nth 4 reftex-derive-label-parameters))))

    ;; Shorten if still to long
    (setq string
          (if (> (length string) (nth 1 reftex-derive-label-parameters))
              (substring string 0 (nth 1 reftex-derive-label-parameters))
            string))

    ;; Delete the final punctuation, if any
    (if (string-match "[^a-zA-Z0-9]+\\'" string)
        (setq string (replace-match "" nil nil string)))
    string))

(defun reftex-replace-prefix-escapes (prefix)
  ;; Replace %escapes in a label prefix
  (save-match-data
    (let (letter (num 0) replace)
      (while (string-match "\\%\\([a-zA-Z]\\)" prefix num)
        (setq letter (match-string 1 prefix))
        (setq replace
              (cond
               ((equal letter "f")
                (file-name-sans-extension
                 (file-name-nondirectory (buffer-file-name))))
               ((equal letter "F")
                (let ((masterdir (file-name-directory (reftex-TeX-master-file)))
                      (file (file-name-sans-extension (buffer-file-name))))
                  (if (string-match (concat "\\`" (regexp-quote masterdir))
                                    file)
                      (substring file (length masterdir))
                    file)))
               ((equal letter "u")
                (or (user-login-name) ""))
               (t "")))
        (setq num (1- (+ (match-beginning 1) (length replace)))
              prefix (replace-match replace nil nil prefix)))
      prefix)))

(defun reftex-label-location (&optional bound)
  ;; Return the environment or macro which determines the label type at point.
  ;; If optional BOUND is an integer, limit backward searches to that point.

  (let* ((loc1 (reftex-what-macro reftex-label-mac-list bound))
         (loc2 (reftex-what-environment reftex-label-env-list bound))
         (p1 (or (cdr loc1) 0))
         (p2 (or (cdr loc2) 0)))

    (setq reftex-location-start (max p1 p2))
    (if (>= p1 p2)
        (progn
          (setq reftex-default-context-position (+ p1 (length (car loc1))))
          (or (car loc1) "section"))
      (setq reftex-default-context-position (+ p2 8 (length (car loc2))))
      (or (car loc2) "section"))))

(defun reftex-uniquify-label (label &optional force separator)
  ;; Make label unique by appending a number.
  ;; Optional FORCE means, force appending a number, even if label is unique.
  ;; Optional SEPARATOR is a string to stick between label and number.

  ;; Ensure access to scanning info
  (reftex-access-scan-info)

  (cond
   ((and (not force)
         (not (assoc label (symbol-value reftex-docstruct-symbol))))
    label)
   (t
    (let* ((label-numbers (assq 'label-numbers
                                (symbol-value reftex-docstruct-symbol)))
           (label-numbers-alist (cdr label-numbers))
           (cell (or (assoc label label-numbers-alist)
                     (car (setcdr label-numbers
                                  (cons (cons label 0)
                                        label-numbers-alist)))))
           (num (1+ (cdr cell)))
           (sep (or separator "")))
      (while (assoc (concat label sep (int-to-string num))
                    (symbol-value reftex-docstruct-symbol))
        (incf num))
      (setcdr cell num)
      (concat label sep (int-to-string num))))))

;; Help string for the reference label menu
(defconst reftex-select-label-prompt
  "Select: [n]ext [p]revious [r]escan [ ]context e[x]tern [q]uit RET [?]HELP+more")

(defconst reftex-select-label-help
  " n / p      Go to next/previous label (Cursor motion works as well)
 C-c C-n/p  Go to next/previous section heading.
 b / l      Jump back to previous selection / Reuse last referenced label
 C-s / C-r  Search forward/backward.  Use repeated C-s/C-r as in isearch.
 g / s      Update menu      / Switch label type
 r / R      Reparse document / Reparse entire document
 x          Switch to label menu of external document (with LaTeX package `xr')
 t i c # %  Toggle: [i]ncl. file borders, [t]able of contents,  [c]ontext
                    [#] label counters,   [%] labels in comments
 SPC / f    Show full context in other window / Toggle follow mode
 v   / .    Toggle \\ref <-> \\vref / Show insertion point in other window
 TAB        Enter a label with completion
 q / RET    Quit without referencing / Accept current label (also on mouse-2)")

(defvar reftex-select-label-map nil
  "Keymap used for *RefTeX Select* buffer, when selecting a label.
This keymap can be used to configure the label selection process which is
started with the command \\[reftex-reference].")

(defun reftex-select-label-mode ()
  "Major mode for selecting a label in a LaTeX document.
This buffer was created with RefTeX.
It only has a meaningful keymap when you are in the middle of a 
selection process.
To select a label, move the cursor to it and press RET.
Press `?' for a summary of important key bindings.

During a selection process, these are the local bindings.

\\{reftex-select-label-map}"

  (interactive)
  (kill-all-local-variables)
  (make-local-hook 'pre-command-hook)
  (make-local-hook 'post-command-hook)
  (setq major-mode 'reftex-select-label-mode
	mode-name "RefTeX Select Label")
  ;; We do not set a local map - reftex-select-item does this.
  (run-hooks 'reftex-select-label-mode-hook))

(defun reftex-reference (&optional type no-insert cut)
  "Make a LaTeX reference.  Look only for labels of a certain TYPE.
With prefix arg, force to rescan buffer for labels.  This should only be
necessary if you have recently entered labels yourself without using
reftex-label.  Rescanning of the buffer can also be requested from the
label selection menu.
The function returns the selected label or nil.
If NO-INSERT is non-nil, do not insert \\ref command, just return label.
When called with 2 C-u prefix args, disable magic word recognition."

  (interactive)

  ;; check for active recursive edits
  (reftex-check-recursive-edit)

  ;; Ensure access to scanning info and rescan buffer if prefix are is '(4)
  (reftex-access-scan-info current-prefix-arg)

  (unless type
    ;; guess type from context
    (if (and reftex-guess-label-type
             (setq type (reftex-guess-label-type)))
	(setq cut (cdr type)
	      type (car type))
      (setq type (reftex-query-label-type))))

  (let* ((varioref (if (reftex-typekey-check 
			type reftex-vref-is-default)
		       "\\vref" "\\ref"))
	 (form "\\ref{%s}")
	 label pair)

    ;; Have the user select a label
    (set-marker reftex-select-return-marker (point))
    (setq pair (save-excursion
		 (reftex-offer-label-menu type)))
    (set-marker reftex-select-return-marker nil)
    (setq label (car pair)
	  type  (cdr pair)
	  form (or (cdr (assoc type reftex-typekey-to-format-alist))
		   form))

    (if (and label
             (not no-insert))
        (progn
	  (if cut (backward-delete-char cut))

	  ;; remove ~ if we do already have a space
	  (when (and (= ?~ (string-to-char form))
		     (= ?\  (preceding-char)))
	    (setq form (substring form 1)))
	  ;; do we need to switch from \ref to \vref?
	  (when (string= varioref "\\vref")
	    (while (string-match "\\\\ref{" form)
	      (setq form (replace-match "\\vref{" t t form))))
          ;; ok, insert the reference
	  (insert
	   (if reftex-format-ref-function
	       (funcall reftex-format-ref-function label form)
	     (format form label label)))
          (message ""))
      (message "Quit"))
    ;; return the label
    label))

(defun reftex-guess-label-type ()
  (let ((words reftex-words-to-typekey-alist)
	(case-fold-search t)
	(bound (max (point-min) (- (point) 35)))
	matched	cell)
    (save-excursion
      (while (and (setq cell (pop words))
		  (not (setq matched 
			     (re-search-backward (car cell) bound t))))))
    (if matched
	(cons (cdr cell) (- (match-end 0) (match-end 1)))
      nil)))

(defun reftex-offer-label-menu (typekey)
  ;; Offer a menu with the appropriate labels.  Return (label . typekey).
  (let* ((buf (current-buffer))
	 (xr-data (assq 'xr (symbol-value reftex-docstruct-symbol)))
	 (xr-alist (cons (cons "" (buffer-file-name)) (nth 1 xr-data)))
	 (xr-index 0)
         (here-I-am (car (reftex-where-am-I)))
	 (here-I-am1 here-I-am)
         (toc (reftex-typekey-check typekey reftex-label-menu-flags 0))
         (files (reftex-typekey-check typekey reftex-label-menu-flags 7))
         (context (not (reftex-typekey-check
                        typekey reftex-label-menu-flags 3)))
         (counter (reftex-typekey-check
                   typekey reftex-label-menu-flags 2))
         (follow  (reftex-typekey-check
                   typekey reftex-label-menu-flags 4))
         (commented (nth 5 reftex-label-menu-flags))
	 (prefix "")
	 selection-buffers
         offset rtn key data last-data entry)

    (setq entry (cons nil nil))

    ;; The following unwind-protect kills temporary buffers after use
    (unwind-protect
        (catch 'exit
          (while t
            (save-window-excursion
	      (delete-other-windows)
	      (setq reftex-call-back-to-this-buffer buf)
	      (let ((default-major-mode 'reftex-select-label-mode))
		(if reftex-use-multiple-selection-buffers
		    (switch-to-buffer-other-window
		     (save-excursion
		       (set-buffer buf)
		       (reftex-make-selection-buffer-name typekey)))
		  (switch-to-buffer-other-window "*RefTeX Select*")
		  (reftex-erase-buffer)))
	      (unless (eq major-mode 'reftex-select-label-mode)
		(reftex-select-label-mode))
	      (add-to-list 'selection-buffers (current-buffer))
              (setq truncate-lines t)
	      (setq mode-line-format
		    (list "----  " 'mode-line-buffer-identification
			  "   " 'varioref
			  "   " (abbreviate-file-name
				 (buffer-file-name buf))
			  " -%-"))
	      (cond
	       ((= 0 (buffer-size))
		(let ((buffer-read-only nil))
		  (setq offset (reftex-make-and-insert-label-list
				typekey buf toc files context counter commented
				(or here-I-am offset) prefix))))
	       (here-I-am
		(setq offset (reftex-get-offset buf here-I-am typekey)))
	       (t (setq offset t)))
	      (setq buffer-read-only t)
	      (setq offset (or offset t))

              (setq here-I-am nil) ; turn off determination of offset
              (setq rtn
                    (reftex-select-item
                     reftex-select-label-prompt
                     reftex-select-label-help
		     reftex-select-label-map
                     offset
                     'reftex-select-label-callback follow))
              (setq key       (car rtn)
                    data      (nth 1 rtn)
                    last-data (nth 2 rtn)
		    offset    t)
              (unless key (throw 'exit nil))
              (cond
	       ((eq key ?g)
		;; update buffer
		(reftex-erase-buffer))
               ((or (eq key ?r)
                    (eq key ?R))
                ;; rescan buffer
		(reftex-erase-buffer)
                (reftex-parse-document buf last-data key))
               ((eq key ?c)
                ;; toggle context mode
		(reftex-erase-buffer)
                (setq context (not context)))
               ((eq key ?s)
                ;; switch type
		(setq here-I-am here-I-am1)
                (setq typekey (reftex-query-label-type)))
               ((eq key ?t)
                ;; toggle table of contents display
		(reftex-erase-buffer)
                (setq toc (not toc)))
               ((eq key ?i)
                ;; toggle display of included file borders
		(reftex-erase-buffer)
                (setq files (not files)))
               ((eq key ?#)
                ;; toggle counter display
		(reftex-erase-buffer)
                (setq counter (not counter)))
               ((eq key ?%)
                ;; toggle display of commented labels
		(reftex-erase-buffer)
                (setq commented (not commented)))
               ((eq key ?l)
                ;; reuse the last referenced label again
                (setq entry reftex-last-used-reference)
                (throw 'exit t))
	       ((eq key ?x)
		;; select an external document
		(setq xr-index (reftex-select-external-document
				xr-alist xr-index))
		(setq buf (or (reftex-get-file-buffer-force
			       (cdr (nth xr-index xr-alist)))
			      (error "Cannot switch document"))
		      prefix (or (car (nth xr-index xr-alist)) ""))
		(set-buffer buf)
		(reftex-access-scan-info))
	       ((stringp key)
		(setq entry 
		      (or (assoc key (symbol-value reftex-docstruct-symbol))
			  (list key typekey)))
		(throw 'exit t))
               (t
                (set-buffer buf)
                (if data
                    (progn
                      (setq entry data)
                      (setq reftex-last-used-reference entry))
                  (setq entry nil))
                (throw 'exit t))))))
      (mapcar (function (lambda (buf) 
			  (and (buffer-live-p buf)
			       (bury-buffer buf))))					   
	      selection-buffers)
      (reftex-kill-buffer "*RefTeX Context Copy*")
      (reftex-kill-temporary-buffers))
    (cons (if (nth 0 entry) (concat prefix (nth 0 entry)) nil)
	  (nth 1 entry))))

(defun reftex-select-external-document (xr-alist xr-index)
  ;; Return index of an external document.
  (let* ((len (length xr-alist)) (highest (1- (+ ?0 len)))
	 (prompt (format "[%c-%c] Select      SPC/DEL: Scroll      TAB: Read prefix with completion" ?0 highest))
	 key prefix)
    (cond
     ((= len 1)
      (message "No external documents available")
      (ding) 0)
     ((= len 2)
      (- 1 xr-index))
     (t
      (save-excursion
	(save-window-excursion
	  (let* ((length (apply 'max (mapcar 
				      (lambda(x) (length (car x))) xr-alist)))
		 (fmt (format " [%%c]  %%-%ds  %%s\n" length))
		 (n (1- ?0)))
	    (with-output-to-temp-buffer "*RefTeX Select*"
	      (princ "SELECT EXTERNAL DOCUMENT\n------------------------\n")
	      (princ
	       (mapconcat
		(function
		 (lambda (x) 
		   (format fmt (incf n) (or (car x) "")
			   (abbreviate-file-name (cdr x)))))
		xr-alist ""))))
	  (reftex-enlarge-to-fit "*RefTeX Select*")
	  (select-window (get-buffer-window "*RefTeX Select*"))
	  (setq truncate-lines t)
	  (catch 'exit
	    (while t
	      (princ prompt)
	      (setq key (read-char-exclusive))
	      (cond
	       ((= key ?\C-?) (condition-case nil (scroll-down) (error nil)))
	       ((= key ?\ )   (condition-case nil (scroll-up) (error nil)))
	       ((and (>= key ?0) (<= key highest))
		(throw 'exit (- key ?0)))
	       ((= key ?\C-i)
		(setq prefix (completing-read "Prefix: " xr-alist nil t))
		(throw 'exit (- len (length (memq (assoc prefix xr-alist)
						  xr-alist)))))
	       (t (error "Illegal document selection [%c]" key)))))))))))

(defun reftex-make-selection-buffer-name (type &optional index)
  ;; Make unique name for a selection buffer.
  (format "*RefTeX[%s][%d]*" 
	  type (or index (get reftex-docstruct-symbol ':master-index) 0)))

(defun reftex-get-offset (buf here-am-I typekey)
  ;; Find the correct offset data, like make-and-insert would, but faster.
  (save-excursion
    (set-buffer buf)
    (reftex-access-scan-info)
    (let* ((rest (memq here-am-I (symbol-value reftex-docstruct-symbol)))
	   entry)
      (while (and (setq entry (pop rest))
		  (not (and (stringp (car entry))
			    (equal typekey (nth 1 entry))))))
      entry)))
			      
(defun reftex-make-and-insert-label-list
  (typekey0 buf toc files context counter show-commented here-I-am xr-prefix)
  ;; Insert a menu of all labels in buffer BUF into current buffer.
  ;; Return the data property of the entry corresponding to HERE-I-AM.
  (let* ((font (reftex-use-fonts))
         (cnt 0)
         (index -1)
         (toc-indent " ")
         (label-indent
          (concat "> "
                  (if toc (make-string (* 7 reftex-level-indent) ?\ ) "")))
         (context-indent
          (concat ".   "
                  (if toc (make-string (* 7 reftex-level-indent) ?\ ) "")))
	 (mouse-face
	  (if (memq reftex-highlight-selection '(mouse both))
	      reftex-mouse-selected-face
	    nil))
         all cell text label typekey note comment master-dir-re
         offset from to docstruct-symbol)

    (message "Creating Selection Buffer...")

    ;; Pop to buffer buf to get the correct buffer-local variables
    (save-excursion
      (set-buffer buf)

      ;; Ensure access to scanning info
      (reftex-access-scan-info)

      (setq docstruct-symbol reftex-docstruct-symbol
	    all (symbol-value reftex-docstruct-symbol)
            reftex-active-toc nil
            master-dir-re
            (concat "\\`" (regexp-quote
                           (file-name-directory (reftex-TeX-master-file))))))

    (set (make-local-variable 'reftex-docstruct-symbol) docstruct-symbol)
    (set (make-local-variable 'reftex-prefix)
	 (cdr (assoc typekey0 reftex-typekey-to-prefix-alist)))
    (if (equal reftex-prefix " ") (setq reftex-prefix nil))

    ;; Walk the docstruct and insert the appropriate stuff
    (while (setq cell (pop all))

      (incf index)
      (setq from (point))

      (if (eq cell here-I-am) (setq offset 'attention))

      (cond

       ((memq (car cell) '(bib thebib label-numbers appendix
			       master-dir is-multi xr xr-doc)))
       ;; These are currently ignored

       ((memq (car cell) '(bof eof file-error))
        ;; Beginning or end of a file
        (when files
          (insert
           " File " (if (string-match master-dir-re (nth 1 cell))
                   (substring (nth 1 cell) (match-end 0))
                 (nth 1 cell))
           (cond ((eq (car cell) 'bof) " starts here\n")
                 ((eq (car cell) 'eof) " ends here\n")
                 ((eq (car cell) 'file-error) " was not found\n")))
          (when font
            (put-text-property from (point)
                               'face reftex-file-boundary-face))))

       ((eq (car cell) 'toc)
        ;; a table of contents entry
        (when toc
          (setq reftex-active-toc cell)
          (insert (concat toc-indent (nth 2 cell) "\n"))))

       ((stringp (car cell))
        ;; a label
        (when (null (nth 2 cell))
          ;; No context yet.  Quick update.
	  (setcdr cell (cdr (reftex-label-info-update cell))))

        (setq label   (car cell)
              typekey (nth 1 cell)
              text    (nth 2 cell)
              note    (nth 4 cell)
              comment (get-text-property 0 'in-comment text))

        (when (and (or (string= typekey typekey0) (string= typekey0 " "))
                   (or show-commented (null comment)))

          ;; Yes we want this one
          (incf cnt)
	  (if (eq offset 'attention) (setq offset cell))

	  (setq label (concat xr-prefix label))
          (when comment (setq label (concat "% " label)))
          (insert label-indent label)
          (when font
            (put-text-property
             (- (point) (length label)) (point)
             'face (if comment
                       'font-lock-comment-face
                     reftex-label-face)))

          (insert (if counter (format " (%d) " cnt) "")
                  (if comment " LABEL IS COMMENTED OUT " "")
                  (if note (concat "  " note) "")
                  "\n")
          (setq to (point))

          (when context
            (insert context-indent text "\n")
            (setq to (point)))
          (put-text-property from to ':data cell)
	  (when mouse-face
	    (put-text-property from (1- to)
			       'mouse-face mouse-face))	  
          (goto-char to)))))

    (when (reftex-refontify)
      (reftex-fontify-select-label-buffer))
    offset))

(defun reftex-parse-document (&optional buffer data key)
  "Rescan the document."
  (interactive)
  (save-window-excursion
    (save-excursion
      (if buffer
          (if (not (bufferp buffer))
              (error "No such buffer %s" (buffer-name buffer))
            (set-buffer buffer)))
      (let ((arg (if (eq key ?R) '(16) '(4)))
            (file (nth 3 data)))
        (reftex-access-scan-info arg file)))))

(defun reftex-query-label-type ()
  ;; Ask for label type
  (message reftex-type-query-prompt)
  (let ((key (read-char)))
    (when (eq key ?\?)
      (save-window-excursion
        (with-output-to-temp-buffer "*RefTeX Help*"
          (princ reftex-type-query-help))
	(reftex-enlarge-to-fit "*RefTeX Help*")
        (setq key (read-char))
        (kill-buffer "*RefTeX Help*")))
    (unless (member (char-to-string key) reftex-typekey-list)
      (error "No such label type: %s" (char-to-string key)))
    (char-to-string key)))

;; Variable holding the vector with section numbers
(defvar reftex-section-numbers [0 0 0 0 0 0 0 0])

(defun reftex-section-info (file)
  ;; Return a section entry for the current match.
  ;; Carefull: This function expects the match-data to be still in place!
  (let* ((marker (set-marker (make-marker) (1- (match-beginning 3))))
         (macro (reftex-match-string 3))
         (star (= ?* (char-after (match-end 3))))
         (level (cdr (assoc macro reftex-section-levels)))
         (section-number (reftex-section-number level star))
         (text1 (save-match-data (save-excursion (reftex-context-substring))))
         (literal (buffer-substring-no-properties
                   (1- (match-beginning 3))
                   (min (point-max) (+ (match-end 0) (length text1) 1))))
	 ;; Literal can be too short since text1 too short. No big problem. 
         (text (reftex-nicify-text text1)))

    ;; Add section number and indentation
    (setq text
          (concat
           (make-string (* reftex-level-indent level) ?\ )
           (if (nth 1 reftex-label-menu-flags) ; section number flag
               (concat section-number " "))
           text))
    ;; Fontify
    (if (reftex-use-fonts)
        (put-text-property 0 (length text)
                           'face reftex-section-heading-face text))
    (list 'toc "toc" text file marker level section-number
          literal (marker-position marker))))

(defun reftex-label-info-update (cell)
  ;; Update information about just one label in a different file.
  ;; CELL contains the old info list
  (let* ((label   (nth 0 cell))
         (typekey (nth 1 cell))
         ;; (text    (nth 2 cell))
         (file    (nth 3 cell))
         (note    (nth 4 cell))
         (buf (reftex-get-file-buffer-force
               file (not (eq t reftex-keep-temporary-buffers)))))
    (if (not buf)
        (list label typekey "" file "LOST LABEL.  RESCAN TO FIX.")
      (save-excursion
        (set-buffer buf)
        (save-restriction
          (widen)
          (goto-char 1)

          (if (or (re-search-forward
		   (format reftex-find-label-regexp-format
			   (regexp-quote label)) nil t)
		  (re-search-forward
		   (format reftex-find-label-regexp-format2
			   (regexp-quote label)) nil t))

              (progn
                (backward-char 1)
                (append (reftex-label-info label file) (list note)))
            (list label typekey "" file "LOST LABEL.  RESCAN TO FIX.")))))))

(defun reftex-label-info (label &optional file bound derive env-or-mac)
  ;; Return info list on LABEL at point.
  (let* ((env-or-mac (or env-or-mac (reftex-label-location bound)))
         (typekey (nth 1 (assoc env-or-mac reftex-env-or-mac-alist)))
         (file (or file (buffer-file-name)))
         (parse (if (reftex-typekey-check
                     typekey reftex-use-text-after-label-as-context)
                    nil
                  (nth 2 (assoc env-or-mac reftex-env-or-mac-alist))))
         (text (reftex-short-context env-or-mac parse reftex-location-start
                                     derive)))
    (if (reftex-in-comment)
        (put-text-property 0 1 'in-comment t text))
    (list label typekey text file)))

(defun reftex-in-comment ()
  (save-excursion
    (skip-chars-backward "^%\n\r")
    (eq (preceding-char) ?%)))

(defun reftex-short-context (env parse &optional bound derive)
  ;; Get about one line of useful context for the label definition at point.

  (if (consp parse)
      (setq parse (if derive (cdr parse) (car parse))))

  (reftex-nicify-text

   (cond

    ((null parse)
     (save-excursion
       (reftex-context-substring)))

    ((eq parse t)
     (if (string= env "section")
         ;; special treatment for section labels
         (save-excursion
           (if (and (re-search-backward reftex-section-or-include-regexp
                                        (point-min) t)
                    (match-end 2))
               (progn
                 (goto-char (match-end 0))
                 (reftex-context-substring))
             (if reftex-active-toc
                 (progn
                   (string-match "{\\([^}]*\\)" (nth 7 reftex-active-toc))
                   (match-string 1 (nth 7 reftex-active-toc)))
               "SECTION HEADING NOT FOUND")))
       (save-excursion
	 (goto-char reftex-default-context-position)
	 (unless (eq (string-to-char env) ?\\)
	   (reftex-move-over-touching-args))
         (reftex-context-substring))))

    ((stringp parse)
     (save-excursion
       (if (re-search-backward parse bound t)
           (progn
             (goto-char (match-end 0))
             (reftex-context-substring))
         "NO MATCH FOR CONTEXT REGEXP")))

    ((integerp parse)
     (or (save-excursion
           (goto-char reftex-default-context-position)
           (reftex-nth-arg
            parse
            (nth 6 (assoc env reftex-env-or-mac-alist))))
         ""))

    ((fboundp parse)
     ;; A hook function.  Call it.
     (save-excursion
       (condition-case error-var
           (funcall parse env)
         (error (format "HOOK ERROR: %s" (cdr error-var))))))
    (t
     "ILLEGAL VALUE OF PARSE"))))

(defun reftex-where-am-I ()
  ;; Return the docstruct entry above point.  Actually returns a cons
  ;; cell in which the cdr is a flag indicating if the information is
  ;; exact (t) or approximate (nil).

  (let ((docstruct (symbol-value reftex-docstruct-symbol))
        (cnt 0) rtn
        found)
    (save-excursion
      (while (not rtn)
        (incf cnt)
        (setq found (re-search-backward reftex-everything-regexp nil t))
        (setq rtn
              (cond
               ((not found)
                ;; no match
                (or
                 (car (member (list 'bof (buffer-file-name)) docstruct))
                 (not (setq cnt 2))
                 (assq 'bof docstruct)  ;; for safety reasons
                 'corrupted))
               ((match-end 1)
                ;; Label
                (assoc (reftex-match-string 1)
                       (symbol-value reftex-docstruct-symbol)))
               ((match-end 3)
                ;; Section
                (goto-char (1- (match-beginning 3)))
                (let* ((list (member (list 'bof (buffer-file-name))
                                     docstruct))
                       (endelt (car (member (list 'eof (buffer-file-name))
                                            list)))
                       rtn1)
                  (while (and list (not (eq endelt (car list))))
                    (if (and (eq (car (car list)) 'toc)
                             (string= (buffer-file-name)
                                      (nth 3 (car list))))
                        (cond
                         ((equal (point)
                                 (or (and (markerp (nth 4 (car list)))
                                          (marker-position (nth 4 (car list))))
                                     (nth 8 (car list))))
                          ;; Fits with marker position or recorded position
                          (setq rtn1 (car list) list nil))
                         ((looking-at (reftex-make-regexp-allow-for-ctrl-m
                                       (nth 7 (car list))))
                          ;; Same title
                          (setq rtn1 (car list) list nil cnt 2))))
                    (pop list))
                  rtn1))
               ((match-end 7)
                ;; Input or include...
                (car
                 (member (list 'eof (reftex-find-tex-file
                                     (reftex-match-string 7)
				      (cdr (assq 'master-dir docstruct))))
                         docstruct)))
	       ((match-end 9)
		(assq 'appendix (symbol-value reftex-docstruct-symbol)))
               ((match-end 10)
                (save-excursion
                  (goto-char (match-end 10))
                  (assoc (reftex-no-props
                          (reftex-nth-arg-wrapper
                           (reftex-match-string 10)))
                         (symbol-value reftex-docstruct-symbol))))
               (t
                (error "This should not happen (reftex-where-am-I)"))))))
    (cons rtn (eq cnt 1))))

(defun reftex-parse-args (macro)
  ;; Return a list of macro name, nargs, arg-nr which is label and a list of
  ;; optional argument indices.
  (if (string-match "[[{]\\*?[]}]" macro)
      (progn
        (let ((must-match (substring macro 0 (match-beginning 0)))
              (args (substring macro (match-beginning 0)))
              opt-list nlabel (cnt 0))
          (while (string-match "\\`[[{]\\(\\*\\)?[]}]" args)
            (incf cnt)
            (when (eq ?\[ (string-to-char args))
              (push cnt opt-list))
            (when (and (match-end 1)
		       (not nlabel))
	      (setq nlabel cnt))
            (setq args (substring args (match-end 0))))
          (list must-match cnt nlabel opt-list)))
    nil))

(defsubst reftex-move-to-next-arg (&optional ignore)
  ;; Assuming that we are at the end of a macro name or a macro argument,
  ;; move forward to the opening parenthesis of the next argument.
  ;; This function understands the splitting of macros over several lines
  ;; in TeX.
  (cond
   ;; Just to be quick:
   ((memq (following-char) '(?\[ ?\{)))
   ;; Do a search
   ((looking-at "[ \t]*[\n\r]?\\([ \t]*%[^\n\r]*[\n\r]\\)*[ \t]*[[{]")
    (goto-char (1- (match-end 0)))
    t)
   (t nil)))

(defsubst reftex-move-to-previous-arg (&optional bound)
  ;; Assuming that we are in front of a macro argument,
  ;; move backward to the closing parenthesis of the previous argument.
  ;; This function understands the splitting of macros over several lines
  ;; in TeX.
  (cond
   ;; Just to be quick:
   ((memq (preceding-char) '(?\] ?\})))
   ;; Do a search
   ((re-search-backward
     "[]}][ \t]*[\n\r]?\\([ \t]*%[^\n\r]*[\n\r]\\)*[ \t]*\\=" bound t)
    (goto-char (1+ (match-beginning 0)))
    t)
   (t nil)))

(defun reftex-nth-arg-wrapper (key)
  (let ((entry (assoc key reftex-env-or-mac-alist)))
    (reftex-nth-arg (nth 5 entry) (nth 6 entry))))

(defun reftex-nth-arg (n &optional opt-args)
  ;; Return the nth following {} or [] parentheses content.
  ;; OPT-ARGS is a list of argument numbers which are optional.

  ;; If we are sitting at a macro start, skip to end of macro name.
  (and (eq (following-char) ?\\) (skip-chars-forward "a-zA-Z*\\\\"))

  (if (= n 1000)
      ;; Special case:  Skip all touching arguments
      (progn
	(reftex-move-over-touching-args)
	(reftex-context-substring))

    ;; Do the real thing.
    (let ((cnt 1))
      
      (when (reftex-move-to-next-arg)
	
	(while (< cnt n)
	  (while (and (member cnt opt-args)
		      (eq (following-char) ?\{))
	    (incf cnt))
	  (when (< cnt n)
	    (unless (and (condition-case nil
			     (or (forward-list 1) t)
			   (error nil))
			 (reftex-move-to-next-arg)
			 (incf cnt))
	      (setq cnt 1000))))

	(while (and (memq cnt opt-args)
		    (eq (following-char) ?\{))
	  (incf cnt)))
      (if (and (= n cnt)
	       (> (skip-chars-forward "{\\[") 0))
	  (reftex-context-substring)
	nil))))

(defun reftex-move-over-touching-args ()
  (condition-case nil
      (while (memq (following-char) '(?\[ ?\{))
	(forward-list 1))
    (error nil)))  

(defun reftex-context-substring ()
  ;; Return up to 100 chars from point
  ;; When point is just after a { or [, limit string to matching parenthesis
  (cond
   ((or (= (preceding-char) ?\{)
        (= (preceding-char) ?\[))
    ;; Inside a list - get only the list.
    (buffer-substring-no-properties
     (point)
     (min (reftex-fp 150)
          (point-max)
          (condition-case nil
              (progn
                (up-list 1)
                (1- (point)))
            (error (point-max))))))
   (t
    ;; no list - just grab 100 characters
    (buffer-substring-no-properties (point) (min (reftex-fp 150) (point-max))))))

(defun reftex-init-section-numbers (&optional toc-entry appendix)
  ;; Initialize the section numbers with zeros or with what is found
  ;; in the toc entry.
  (let* ((level  (or (nth 5 toc-entry) -1))
         (numbers (nreverse (split-string (or (nth 6 toc-entry) "") "\\.")))
         (depth (1- (length reftex-section-numbers)))
         (i depth) number-string)
    (while (>= i 0)
      (if (> i level)
          (aset reftex-section-numbers i 0)
	(setq number-string (or (car numbers) "0"))
	(if (string-match "\\`[A-Z]\\'" number-string)
	    (aset reftex-section-numbers i
		  (- (string-to-char number-string) ?A -1))
	    (aset reftex-section-numbers i (string-to-int number-string)))
        (pop numbers))
      (decf i)))
  (put 'reftex-section-numbers 'appendix appendix))

(defun reftex-section-number (&optional level star)
  ;; Return a string with the current section number.
  ;; When LEVEL is non-nil, increase section numbers on that level.
  (let* ((depth (1- (length reftex-section-numbers))) idx n (string "")
	 (appendix (get 'reftex-section-numbers 'appendix)))
    (when level
      (when (and (> level -1) (not star))
        (aset reftex-section-numbers 
	      level (1+ (aref reftex-section-numbers level))))
      (setq idx (1+ level))
      (while (<= idx depth)
        (aset reftex-section-numbers idx 0)
        (incf idx)))
    (setq idx 0)
    (while (<= idx depth)
      (setq n (aref reftex-section-numbers idx))
      (setq string (concat string (if (not (string= string "")) "." "")
                           (int-to-string n)))
      (incf idx))
    (save-match-data
      (if (string-match "\\`\\([@0]\\.\\)+" string)
          (setq string (replace-match "" nil nil string)))
      (if (string-match "\\(\\.0\\)+\\'" string)
          (setq string (replace-match "" nil nil string)))
      (if (and appendix
	       (string-match "\\`[0-9]+" string))
	  (setq string (concat (char-to-string (1- (+ ?A (string-to-int (match-string 0 string))))) (substring string (match-end 0))))))
	       
    (if star
        (concat (make-string (1- (length string)) ?\ ) "*")
      string)))

(defun reftex-select-label-callback (data forward no-revisit)
  ;; Callback function called from the label selection in order to
  ;; show context in another window
  (let* ((this-window (selected-window))
         label file buffer re found)
    ;; pop to original buffer in order to get correct variables
    (catch 'exit
      (setq label (nth 0 data)
	    file  (nth 3 data))

      ;; goto the file in another window
      (setq buffer 
	    (if no-revisit
		(get-file-buffer file)
	      (reftex-get-file-buffer-force
	       file (not reftex-keep-temporary-buffers))))
      (if buffer
          ;; good - the file is available
          (switch-to-buffer-other-window buffer)
        ;; we have got a problem here.  The file does not exist.
        ;; Let' get out of here..
        ;; (ding)
        (throw 'exit nil))

      ;; search for that label
      (setq re (format reftex-find-label-regexp-format (regexp-quote label)))
      (setq found
	    (if forward
		(re-search-forward re nil t)
	      (re-search-backward re nil t)))
      (unless found
        (goto-char (point-min))
	(unless (re-search-forward re nil t)
	  ;; Ooops.  Must be in a macro with distributed args.
	  (re-search-forward (format reftex-find-label-regexp-format2
				     (regexp-quote label)) nil t)))
      (when (match-end 3)
        (reftex-highlight 0 (match-beginning 3) (match-end 3))
        (reftex-show-entry (- (point) (match-beginning 3))
                           (- (point) (match-end 3)))
        (recenter (/ (window-height) 2)))
      (select-window this-window))))

(defun reftex-pop-to-label (label file-list &optional mark-to-kill highlight)
  ;; Find LABEL in any file in FILE-LIST in another window.
  ;; If mark-to-kill is non-nil, mark new buffer for killing.
  ;; If HIGHLIGHT is non-nil, highlight the label definition.
  (let* ((re1 (format reftex-find-label-regexp-format (regexp-quote label)))
	 (re2 (format reftex-find-label-regexp-format2 (regexp-quote label)))
	 (re-list (list re1 re2)) re
	 (file-list-1 file-list)
         file buf)
    (catch 'exit
      (while (setq re (pop re-list))
	(setq file-list file-list-1)
	(while (setq file (pop file-list))
	  (unless (setq buf (reftex-get-file-buffer-force file mark-to-kill))
	    (error "No such file %s" file))
	  (set-buffer buf)
	  (widen)
	  (goto-char (point-min))
	  (when (re-search-forward re nil t)
	    (switch-to-buffer-other-window buf)
	    (goto-char (match-beginning 0))
	    (recenter (/ (window-height) 2))
	    (if highlight
		(reftex-highlight 0 (match-beginning 3) (match-end 3)))
	    (throw 'exit (selected-window)))))
      (error "Label %s not found" label))))

(defun reftex-find-duplicate-labels ()
  "Produce a list of all duplicate labels in the document."

  (interactive)

  ;; Rescan the document to make sure
  (reftex-access-scan-info t)

  (let ((master (reftex-TeX-master-file))
	(cnt 0)
        (dlist
         (mapcar
	  (function
	   (lambda (x)
	     (let (x1)
	       (cond
		((memq (car x)
		       '(toc bof eof bib thebib label-numbers xr xr-doc
			     master-dir file-error appendix is-multi))
		 nil)
		(t
		 (setq x1 (reftex-all-assoc-string
			   (car x) (symbol-value reftex-docstruct-symbol)))
		 (if (< 1 (length x1))
		     (append (list (car x))
			     (mapcar (function
				      (lambda(x)
					(abbreviate-file-name (nth 3 x))))
				     x1))
		   (list nil)))))))
          (reftex-uniquify (symbol-value reftex-docstruct-symbol)))))

    (setq dlist (reftex-uniquify dlist))
    (if (null dlist) (error "No duplicate labels in document"))
    (switch-to-buffer-other-window "*Duplicate Labels*")
    (make-local-variable 'TeX-master)
    (setq TeX-master master)
    (erase-buffer)
    (insert "                MULTIPLE LABELS IN CURRENT DOCUMENT:\n")
    (insert 
     " Move point to label and type `r' to run a query-replace on the label\n"
     " and its references.  Type `q' to exit this buffer.\n\n")
    (insert " LABEL               FILE\n")
    (insert " -------------------------------------------------------------\n")
    (use-local-map (make-sparse-keymap))
    (local-set-key [?q] (function
			 (lambda () (interactive)
			   (kill-buffer (current-buffer)) (delete-window))))
    (local-set-key [?r] 'reftex-change-label)
    (while dlist
      (when (and (car (car dlist))
                 (cdr (car dlist)))
	(incf cnt)
        (insert (mapconcat 'identity (car dlist) "\n    ") "\n"))
      (pop dlist))
    (goto-char (point-min))
    (when (= cnt 0)
      (kill-buffer (current-buffer))
      (delete-window)
      (message "Document does not contain duplicate labels."))))

(defun reftex-all-assq (key list)
  ;; Return a list of all associations of KEY in LIST.  Comparison with string=
  (let (rtn)
    (while (setq list (memq (assq key list) list))
      (push (car list) rtn)
      (pop list))
    (nreverse rtn)))

(defun reftex-all-assoc-string (key list)
  ;; Return a list of all associations of KEY in LIST.  Comparison with string=
  (let (rtn)
    (while list
      (if (string= (car (car list)) key)
          (push (car list) rtn))
      (pop list))
    (nreverse rtn)))

(defun reftex-kill-temporary-buffers (&optional buffer)
  ;; Kill all buffers in the list reftex-kill-temporary-buffers.
  (cond
   (buffer
    (when (member buffer reftex-buffers-to-kill)
      (kill-buffer buffer)
      (setq reftex-buffers-to-kill
            (delete buffer reftex-buffers-to-kill))))
   (t
    (while (setq buffer (pop reftex-buffers-to-kill))
      (when (bufferp buffer)
        (and (buffer-modified-p buffer)
             (y-or-n-p (format "Save file %s? "
                               (buffer-file-name buffer)))
             (save-excursion
               (set-buffer buffer)
               (save-buffer)))
        (kill-buffer buffer))
      (pop reftex-buffers-to-kill)))))

(defun reftex-show-entry (beg-hlt end-hlt)
  ;; Show entry if point is hidden by outline mode
  (let* ((pos (point))
         (n (/ (window-height) 2))
         (beg (save-excursion
               (re-search-backward "[\n\r]" nil 1 n) (point)))
         (end (save-excursion
                (re-search-forward  "[\n\r]" nil 1 n) (point))))
    (if (and reftex-auto-show-entry
             (string-match
              "\r" (buffer-substring beg end)))
        (cond
         ((eq t reftex-auto-show-entry)
          (subst-char-in-region
           (save-excursion (search-backward "\n" nil t) (point))
           (save-excursion (search-forward  "\n" nil t) (point))
           ?\r ?\n t))
         ((eq reftex-auto-show-entry 'copy)
          (let ((string (buffer-substring beg end)))
            (switch-to-buffer "*RefTeX Context Copy*")
            (setq buffer-read-only nil)
            (erase-buffer)
            (insert string)
            (subst-char-in-region (point-min) (point-max) ?\r ?\n t)
            (goto-char (- pos beg))
            (reftex-highlight 0 (1+ (- (point) beg-hlt))
                              (1+ (- (point) end-hlt)))
            (when (reftex-refontify)
              (set (make-local-variable 'font-lock-defaults) nil)
	      (let ((major-mode 'latex-mode))
		(font-lock-mode 1)))
            (setq buffer-read-only t)))
         ))))

(defun reftex-nicify-text (text)
  ;; Make TEXT nice for inclusion as context into label menu
  ;; remove line breaks and extra white space
  (while (string-match "[\n\r\t]\\|[ \t][ \t]+" text)
    (setq text (replace-match " " nil t text)))
  ;; cut before the next `\end{' or `\item' or `\\'
  (if (string-match "\\(\\\\end{\\|\\\\item\\|\\\\\\\\\\).*" text)
      (setq text (replace-match "" nil t text)))
  ;; kill the embedded label
  (if (string-match "\\\\label{[^}]*}" text)
      (setq text (replace-match "" nil t text)))
  ;; remove leading garbage
  (if (string-match "\\`[ }]+" text)
      (setq text (replace-match "" nil t text)))
  ;; limit length
  (cond
   ((> (length text) 100) (substring text 0 100))
   ((= (length text) 0) (make-string 1 ?\ ))
   (t text)))

(defun reftex-typekey-check (typekey conf-variable &optional n)
  ;; Check if CONF-VARIABLE is true or contains TYPEKEY
  (and n (setq conf-variable (nth n conf-variable)))
  (or (eq conf-variable t)
      (and (stringp conf-variable)
           (string-match (concat "[" conf-variable "]") typekey))))

;;; ===========================================================================
;;;
;;; Table of contents

;; We keep at most one *toc* buffer - it is easy to make them

(defvar reftex-toc-map (make-sparse-keymap)
  "Keymap used for *toc* buffer.")

(defun reftex-toc-mode ()
  "Major mode for managing Table of Contents for LaTeX files.
This buffer was created with RefTeX.
Press `?' for a summary of important key bindings.

Here are all local bindings.

\\{reftex-toc-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'reftex-toc-mode
	mode-name "RefTeX Table of Contents")
  (use-local-map reftex-toc-map)
  (set (make-local-variable 'revert-buffer-function) 'reftex-toc-revert)
  (setq truncate-lines t)
  (make-local-hook 'post-command-hook)
  (make-local-hook 'pre-command-hook)
  (make-local-variable 'reftex-last-follow-point)
  (add-hook 'post-command-hook 'reftex-toc-post-command-hook nil t)
  (add-hook 'pre-command-hook  'reftex-toc-pre-command-hook nil t)
  (run-hooks 'reftex-toc-mode-hook))

(defvar reftex-last-toc-master nil
  "Stores the name of the tex file that `reftex-toc' was last run on.")

(defvar reftex-last-toc-file nil
  "Stores the file name from which `reftex-toc' was called.  For redo command.")

(defvar reftex-last-window-height nil)

(defvar reftex-toc-return-marker (make-marker)
  "Marker which makes it possible to return from toc to old position.")

(defconst reftex-toc-help
"                      AVAILABLE KEYS IN TOC BUFFER
                      ============================
n / p   next-line / previous-line
SPC     Show the corresponding section of the LaTeX document.
TAB     Goto the section.
RET     Goto the section and hide the *toc* buffer (also on mouse-2).
q / Q   Hide/Kill *toc* buffer, return to position of last reftex-toc command.
f / g   Toggle follow mode on and off / Refresh *toc* buffer.
r / R   Reparse the LaTeX document / Reparse entire LaTeX document.
x       Switch to TOC of external document (with LaTeX package `xr').")

(defun reftex-toc ()
  "Show the table of contents for the current document.
When called with a raw C-u prefix, rescan the document first."

  (interactive)

  (if (or (not (string= reftex-last-toc-master (reftex-TeX-master-file)))
          current-prefix-arg)
      (reftex-erase-buffer "*toc*"))

  (setq reftex-last-toc-file   (buffer-file-name))
  (setq reftex-last-toc-master (reftex-TeX-master-file))

  (set-marker reftex-toc-return-marker (point))

  ;; If follow mode is active, arrange to delay it one command
  (if reftex-toc-follow-mode
      (setq reftex-toc-follow-mode 1))

  ;; Ensure access to scanning info and rescan buffer if prefix are is '(4)
  (reftex-access-scan-info current-prefix-arg)

  (let* ((all   (symbol-value reftex-docstruct-symbol))
	 (xr-data (assq 'xr all))
	 (xr-alist (cons (cons "" (buffer-file-name)) (nth 1 xr-data)))
         (where (reftex-nearest-section))
	 (mouse-face
	  (if (memq reftex-highlight-selection '(mouse both))
	      reftex-mouse-selected-face
	    nil))
	 toc1 cell startpos)

    (if (get-buffer-window "*toc*")
        (select-window (get-buffer-window "*toc*"))
      (when (< (window-height) (* 2 window-min-height))
	(delete-other-windows))
      (setq reftex-last-window-height (window-height))  ; remember
      (split-window)
      (let ((default-major-mode 'reftex-toc-mode))
	(switch-to-buffer "*toc*")))

    (or (eq major-mode 'reftex-toc-mode) (reftex-toc-mode))

    (cond
     ;; buffer is empty - fill it with the table of contents
     ((= (buffer-size) 0)
      (message "Building *toc* buffer...")

      (setq buffer-read-only nil)
      (insert (format
"TABLE-OF-CONTENTS on %s
SPC=view TAB=goto RET=goto+hide [q]uit [r]escan [f]ollow-mode e[x]tern [?]Help
-------------------------------------------------------------------------------
" (abbreviate-file-name reftex-last-toc-master)))
      (setq startpos (point))

      (if (reftex-use-fonts)
          (put-text-property 1 (point) 'face reftex-toc-header-face))
      (put-text-property 1 (point) 'intangible t)
      (put-text-property 1 2 'xr-alist xr-alist)

      (while all
        (setq cell (car all)
              all  (cdr all))
        (when (eq (car cell) 'toc)
          (setq toc1 (concat (nth 2 cell) "\n"))
          (put-text-property 0 (length toc1) 'toc cell toc1)
	  (when mouse-face
	    (put-text-property 0 (1- (length toc1))
			       'mouse-face mouse-face toc1))
          (insert toc1)))

      (backward-delete-char 1)

      (message "Building *toc* buffer...done.")
      (setq buffer-read-only t))
     (t
      (goto-line 3)
      (beginning-of-line)
      (setq startpos (point))))

    ;; Find the correct section
    (goto-char (point-max))
    (beginning-of-line)
    (while (and (> (point) startpos)
                (not (eq (get-text-property (point) 'toc) where)))
      (beginning-of-line 0))
    (setq reftex-last-follow-point (point))))

(defun reftex-nearest-section ()
  ;; Return (file . find) of nearest section command
  (let* ((here-I-am (car (reftex-where-am-I))))
    (reftex-last-assoc-before-elt
     'toc here-I-am (symbol-value reftex-docstruct-symbol))))

(defun reftex-toc-pre-command-hook ()
  ;; used as pre command hook in *toc* buffer
  (reftex-unhighlight 0)
  (reftex-unhighlight 1))

(defun reftex-toc-post-command-hook ()
  ;; used in the post-command-hook for the *toc* buffer
  (and (> (point) 1)
       (memq reftex-highlight-selection '(cursor both))
       (save-excursion
         (reftex-highlight 1
                          (progn (beginning-of-line) (point))
                          (progn (end-of-line) (point)))))
  (cond
   ((integerp reftex-toc-follow-mode)
    ;; remove delayed action
    (setq reftex-toc-follow-mode t))
   ((and reftex-toc-follow-mode
	 (not (equal reftex-last-follow-point (point))))
    ;; show context in other window
    (setq reftex-last-follow-point (point))
    (condition-case nil
        (reftex-toc-visit-line nil (not reftex-revisit-to-follow))
      (error t)))))

(defun reftex-re-enlarge ()
  (enlarge-window
   (max 0 (- (or reftex-last-window-height (window-height))
             (window-height)))))

(defun reftex-toc-show-help ()
  (interactive)
  (with-output-to-temp-buffer "*RefTeX Help*"
    (princ reftex-toc-help))
  ;; If follow mode is active, arrange to delay it one command
  (if reftex-toc-follow-mode
      (setq reftex-toc-follow-mode 1)))

(defun reftex-toc-toggle-follow ()
  "Toggle toc-follow mode.
(It is not really a mode, just a flag)."
  (interactive)
  (setq reftex-toc-follow-mode (not reftex-toc-follow-mode)))
(defun reftex-toc-view-line ()
  "View document location in other window."
  (interactive)
  (reftex-toc-visit-line))
(defun reftex-toc-mouse-view-line (ev)
  "View document location in other window."
  (interactive "e")
  (mouse-set-point ev)
  (reftex-toc-visit-line))
(defun reftex-toc-goto-line-and-hide ()
  "Go to document location in other window.  Hide the *toc* window."
  (interactive)
  (reftex-toc-visit-line 'hide))
(defun reftex-toc-goto-line ()
  "Go to document location in other window.  Hide the *toc* window."
  (interactive)
  (reftex-toc-visit-line t))
(defun reftex-toc-mouse-goto-line-and-hide (ev)
  "Go to document location in other window.  Hide the *toc* window."
  (interactive "e")
  (mouse-set-point ev)
  (reftex-toc-visit-line 'hide))
(defun reftex-toc-quit ()
  "Hide the *toc* window and do not move point."
  (interactive)
  (or (one-window-p) (delete-window))
  (switch-to-buffer (marker-buffer reftex-toc-return-marker))
  (reftex-re-enlarge)
  (goto-char (or (marker-position reftex-toc-return-marker) (point))))
(defun reftex-toc-quit-and-kill ()
  "Kill the *toc* buffer."
  (interactive)
  (kill-buffer "*toc*")
  (or (one-window-p) (delete-window))
  (switch-to-buffer (marker-buffer reftex-toc-return-marker))
  (reftex-re-enlarge)
  (goto-char (marker-position reftex-toc-return-marker)))
(defun reftex-toc-rescan (&rest ignore)
  "Regenerate the *toc* buffer by reparsing file of section at point."
  (interactive)
  (if reftex-enable-partial-scans
      (let ((file (nth 3 (get-text-property (point) 'toc))))
        (if (not file)
            (error "Don't know which file to rescan.  Try `R'.")
          (switch-to-buffer-other-window
           (reftex-get-file-buffer-force file))
          (setq current-prefix-arg '(4))
          (reftex-toc)))
    (reftex-toc-Rescan))
  (reftex-kill-temporary-buffers))
(defun reftex-toc-Rescan (&rest ignore)
  "Regenerate the *toc* buffer by reparsing the entire document."
  (interactive)
  (switch-to-buffer-other-window
   (reftex-get-file-buffer-force reftex-last-toc-file))
  (setq current-prefix-arg '(16))
  (reftex-toc))
(defun reftex-toc-revert (&rest ignore)
  "Regenerate the *toc* from the internal lists."
  (interactive)
  (switch-to-buffer-other-window
   (reftex-get-file-buffer-force reftex-last-toc-file))
  (reftex-erase-buffer "*toc*")
  (setq current-prefix-arg nil)
  (reftex-toc))
(defun reftex-toc-external (&rest ignore)
  "Switch to table of contents of an external document."
  (interactive)
  (let* ((old-buf (current-buffer))
	 (xr-alist (get-text-property 1 'xr-alist))
	 (xr-index (reftex-select-external-document
		   xr-alist 0)))
    (switch-to-buffer-other-window (or (reftex-get-file-buffer-force
					(cdr (nth xr-index xr-alist)))
				       (error "Cannot switch document")))
    (reftex-toc)
    (if (equal old-buf (current-buffer))
	(message "")
      (message "Switched document"))))

(defun reftex-toc-visit-line (&optional final no-revisit)
  ;; Visit the tex file corresponding to the toc entry on the current line.
  ;; If FINAL is t, stay there
  ;; If FINAL is 'hide, hide the *toc* window.
  ;; Otherwise, move cursor back into *toc* window.
  ;; This function is pretty clever about finding back a section heading,
  ;; even if the buffer is not live, or things like outline, x-symbol etc.
  ;; have been active.

  (let* ((toc (get-text-property (point) 'toc))
         (file (nth 3 toc))
         (marker (nth 4 toc))
         (level (nth 5 toc))
         (literal (nth 7 toc))
         (emergency-point (nth 8 toc))
         (toc-window (selected-window))
         show-window show-buffer match)

    (unless toc (error "Don't know which toc line to visit"))

    (setq match
          (cond
           ((and (markerp marker) (marker-buffer marker))
            ;; Buffer is still live and we have the marker.  Should be easy.
            (switch-to-buffer-other-window (marker-buffer marker))
            (goto-char (marker-position marker))
            (or (looking-at (regexp-quote literal))
                (looking-at (reftex-make-regexp-allow-for-ctrl-m literal))
                (looking-at (reftex-make-desperate-section-regexp literal))
                (looking-at (concat "\\\\"
                                    (regexp-quote
                                     (car (rassq level reftex-section-levels)))
                                    "[[{]"))))
           ((or (not no-revisit)
		(get-file-buffer file))
            ;; Marker is lost.  Use the backup method.
            (switch-to-buffer-other-window
             (reftex-get-file-buffer-force file nil))
            (goto-char (or emergency-point (point-min)))
            (or (looking-at (regexp-quote literal))
                (let ((pos (point)))
                  (re-search-backward "\\`\\|[\r\n][ \t]*[\r\n]" nil t)
                  (or (reftex-nearest-match (regexp-quote literal) pos)
                      (reftex-nearest-match
                       (reftex-make-regexp-allow-for-ctrl-m literal) pos)
                      (reftex-nearest-match
                       (reftex-make-desperate-section-regexp literal) pos)))))
           ))

    (setq show-window (selected-window)
          show-buffer (current-buffer))

    (unless match
      (select-window toc-window)
      (error "Cannot find line"))

    (goto-char (match-beginning 0))
    (recenter 1)
    (reftex-highlight 0 (match-beginning 0) (match-end 0) (current-buffer))

    (select-window toc-window)

    ;; use the `final' parameter to decide what to do next
    (cond
     ((eq final t)
      (reftex-unhighlight 0)
      (select-window show-window))
     ((eq final 'hide)
      (reftex-unhighlight 0)
      (or (one-window-p) (delete-window))
      (switch-to-buffer show-buffer)
      (reftex-re-enlarge))
     (t nil))))

;;; ===========================================================================
;;;
;;; BibTeX citations.

;; Variables and constants

;; Define variable to silence compiler warnings
(defvar reftex-cite-format-builtin)

;; The history list of regular expressions used for citations
(defvar reftex-cite-regexp-hist nil)

;; Prompt and help string for citation selection
(defconst reftex-citation-prompt
  "Select: [n]ext [p]revious [r]estrict [ ]full_entry [q]uit RET [?]Help+more")

(defconst reftex-citation-help
  " n / p      Go to next/previous entry (Cursor motion works as well).
 C-s / C-r  Search forward/backward.  Use repeated C-s/C-r as in isearch.
 g   / r    Start over with new regexp / Refine with additional regexp.
 SPC        Show full database entry in other window.
 f          Toggle follow mode: Other window will follow with full db entry.
 .          Show insertion point.
 q          Quit without inserting \\cite macro into buffer.
 TAB        Enter citation key with completion.
 RET        Accept current entry (also on mouse-2)
 a          Accept all entries.")

(defvar reftex-select-bib-map nil
  "Keymap used for *RefTeX Select* buffer, when selecting a BibTeX entry.
This keymap can be used to configure the BibTeX selection process which is
started with the command \\[reftex-citation].")

(defun reftex-select-bib-mode ()
  "Major mode for selecting a citation key in a LaTeX document.
This buffer was created with RefTeX.
It only has a meaningful keymap when you are in the middle of a 
selection process.
In order to select a citation, move the cursor to it and press RET.
Press `?' for a summary of important key bindings.

During a selection process, these are the local bindings.

\\{reftex-select-label-map}"
  (interactive)
  (kill-all-local-variables)
  (make-local-hook 'pre-command-hook)
  (make-local-hook 'post-command-hook)
  (setq major-mode 'reftex-select-bib-mode
	mode-name "RefTeX Select Bib")
  ;; We do not set a local map - reftex-select-item does this.
  (run-hooks 'reftex-select-bib-mode-hook))

;; Find bibtex files

(defun reftex-get-bibfile-list ()
  ;; Return list of bibfiles for current document.
  ;; When using the chapterbib or bibunits package you should either
  ;; use the same database files everywhere, or separate parts using
  ;; different databases into different files (included into the mater file).
  ;; Then this function will return the applicable database files.

  ;; Ensure access to scanning info
  (reftex-access-scan-info)
  (or
   ;; Try inside this file (and its includes)
   (cdr (reftex-last-assoc-before-elt
         'bib (list 'eof (buffer-file-name))
         (member (list 'bof (buffer-file-name))
                 (symbol-value reftex-docstruct-symbol))))
   ;; Try after the beginning of this file
   (cdr (assq 'bib (member (list 'bof (buffer-file-name))
                           (symbol-value reftex-docstruct-symbol))))
   ;; Anywhere in the entire document
   (cdr (assq 'bib (symbol-value reftex-docstruct-symbol)))
   (error "\\bibliography statement missing or .bib files not found.")))

(defun reftex-find-tex-file (file master-dir &optional die)
  ;; Find FILE in MASTER-DIR or on reftex-tex-path.
  ;; FILE may be given without the .tex extension.
  (cond 
   ((file-name-absolute-p file)
    (if (file-exists-p file) file nil))
   (t
    (reftex-access-search-path "tex")
    (let* ((path (cons master-dir reftex-tex-path))
	   file1)
      (setq file1
	    (or (reftex-find-file-on-path (concat file ".tex") path)
		(reftex-find-file-on-path file path)))
      (unless file1
	(reftex-access-search-path "tex" t file)
	(setq path (cons master-dir reftex-tex-path))
	(setq file1
	      (or (reftex-find-file-on-path (concat file ".tex") path)
		  (reftex-find-file-on-path file path))))
      (cond (file1 file1)
	    (die (error "No such file: %s" file) nil)
	    (t (message "No such file: %s (ignored)" file) nil))))))

(defun reftex-find-bib-file (file master-dir &optional die)
  ;; Find FILE in MASTER-DIR or on reftex-bib-path
  (reftex-access-search-path "bib")
  (let ((file1 (reftex-find-file-on-path 
		file (cons master-dir reftex-bib-path))))
    (unless file1
      (reftex-access-search-path "bib" t file)
      (setq file1 (reftex-find-file-on-path 
		file (cons master-dir reftex-bib-path))))
    (cond (file1 file1)
	  (die (error "No such file: %s" file) nil)
	  (t (message "No such file: %s (ignored)" file) nil))))

;; Find a certain reference in any of the BibTeX files.

(defun reftex-pop-to-bibtex-entry (key file-list
                                      &optional mark-to-kill highlight item)
  ;; Find BibTeX KEY in any file in FILE-LIST in another window.
  ;; If MARK-TO-KILL is non-nil, mark new buffer to kill.
  ;; If HIGHLIGHT is non-nil, highlight the match.
  ;; If ITEM in non-nil, search for bibitem instead of database entry.

  (let* ((re
	  (if item 
	      (concat "\\\\bibitem\\(\\[[^]]*\\]\\)?{" (regexp-quote key) "}")
	    (concat "@[a-zA-Z]+[ \t\n\r]*[{(][ \t\n\r]*" (regexp-quote key))))
         (window-conf (current-window-configuration))
         file buf)

    (catch 'exit
      (switch-to-buffer-other-window (current-buffer))
      (while file-list
        (setq file (car file-list)
              file-list (cdr file-list))
        (unless (setq buf (reftex-get-file-buffer-force file mark-to-kill))
          (error "No such file %s" file))
        (switch-to-buffer buf)
        (widen)
        (goto-char (point-min))
        (when (re-search-forward re nil t)
          (goto-char (match-beginning 0))
          (recenter 0)
          (if highlight
              (reftex-highlight 0 (match-beginning 0) (match-end 0)))
          (throw 'exit (selected-window))))
      (set-window-configuration window-conf)
      (if item
	  (message "No \\bibitem with citation key %s" key)
	(message "No BibTeX entry with citation key %s" key)))))

;; Parse bibtex buffers

(defun reftex-extract-bib-entries (buffers &optional get-word)
  ;; Extract bib entries which match regexps from BUFFERS.
  ;; BUFFERS is a list of buffers or file names.
  ;; Return list with entries."
  (let* (re-list first-re rest-re
                 (buffer-list (if (listp buffers) buffers (list buffers)))
                 found-list entry buffer1 buffer alist
                 key-point start-point end-point)

    (setq re-list (split-string 
		   (read-string "RegExp [ && RegExp...]: "
				nil 'reftex-cite-regexp-hist)
		   "[ \t]*&&[ \t]*"))

    (setq first-re (car re-list)    ; We'll use the first re to find things,
          rest-re  (cdr re-list))   ; the others to narrow down.
    (if (string-match "\\`[ \t]*\\'" first-re)
        (error "Empty regular expression"))

    (save-excursion
      (save-window-excursion

        ;; Walk through all bibtex files
        (while buffer-list
          (setq buffer (car buffer-list)
                buffer-list (cdr buffer-list))
          (if (and (bufferp buffer)
                   (buffer-live-p buffer))
              (setq buffer1 buffer)
            (setq buffer1 (reftex-get-file-buffer-force
                           buffer (not reftex-keep-temporary-buffers))))
          (if (not buffer1)
              (error "Cannot find BibTeX file %s" buffer)
            (message "Scanning bibliography database %s" buffer1))

          (set-buffer buffer1)
          (save-excursion
            (goto-char (point-min))
            (while (re-search-forward first-re nil t)
              (catch 'search-again
                (setq key-point (point))
                (unless (re-search-backward
                         "\\(\\`\\|[\n\r]\\)[ \t]*@\\([a-zA-Z]+\\)[ \t\n\r]*[{(]" nil t)
                  (throw 'search-again nil))
                (setq start-point (point))
                (goto-char (match-end 0))
                (condition-case nil
                    (up-list 1)
                  (error (goto-char key-point)
                          (throw 'search-again nil)))
                (setq end-point (point))

                ;; Ignore @string, @comment and @c entries or things
                ;; outside entries
                (when (or (string= (downcase (match-string 2)) "string")
                          (string= (downcase (match-string 2)) "comment")
                          (string= (downcase (match-string 2)) "c")
                          (< (point) key-point)) ; this means match not in {}
                  (goto-char key-point)
                  (throw 'search-again nil))

                ;; Well, we have got a match
                (setq entry (concat
                             (buffer-substring start-point (point)) "\n"))

                ;; Check if other regexp match as well
                (setq re-list rest-re)
                (while re-list
                  (unless (string-match (car re-list) entry)
                    ;; nope - move on
                    (throw 'search-again nil))
                  (pop re-list))

                (setq alist (reftex-parse-bibtex-entry
                             nil start-point end-point))
                (push (cons "&entry" entry) alist)

                ;; check for crossref entries
                (if (assoc "crossref" alist)
                    (setq alist
                          (append
                           alist (reftex-get-crossref-alist alist))))

                ;; format the entry
                (push (cons "&formatted" (reftex-format-bib-entry alist))
                      alist)

                ;; add it to the list
                (push alist found-list))))
          (reftex-kill-temporary-buffers))))
    (setq found-list (nreverse found-list))

    ;; Sorting
    (cond
     ((eq 'author reftex-sort-bibtex-matches)
      (sort found-list 'reftex-bib-sort-author))
     ((eq 'year   reftex-sort-bibtex-matches)
      (sort found-list 'reftex-bib-sort-year))
     ((eq 'reverse-year reftex-sort-bibtex-matches)
      (sort found-list 'reftex-bib-sort-year-reverse))
     (t found-list))))

(defun reftex-bib-sort-author (e1 e2)
  (let ((al1 (reftex-get-bib-names "author" e1))
        (al2 (reftex-get-bib-names "author" e2)))
    (while (and al1 al2 (string= (car al1) (car al2)))
      (pop al1)
      (pop al2))
    (if (and (stringp (car al1))
             (stringp (car al2)))
        (string< (car al1) (car al2))
      (not (stringp (car al1))))))

(defun reftex-bib-sort-year (e1 e2)
  (< (string-to-int (cdr (assoc "year" e1)))
     (string-to-int (cdr (assoc "year" e2)))))

(defun reftex-bib-sort-year-reverse (e1 e2)
  (> (string-to-int (or (cdr (assoc "year" e1)) "0"))
     (string-to-int (or (cdr (assoc "year" e2)) "0"))))

(defun reftex-get-crossref-alist (entry)
  ;; return the alist from a crossref entry
  (let ((crkey (cdr (assoc "crossref" entry)))
        start)
    (save-excursion
      (save-restriction
        (widen)
        (if (re-search-forward
             (concat "@\\w+[{(][ \t\n\r]*" (regexp-quote crkey)
                     "[ \t\n\r]*,") nil t)
            (progn
              (setq start (match-beginning 0))
              (condition-case nil
                  (up-list 1)
                (error nil))
              (reftex-parse-bibtex-entry nil start (point)))
          nil)))))

;; Parse the thebibliography environment
(defun reftex-extract-bib-entries-from-thebibliography (file)
  ;; Extract bib-entries from the \begin{thebibliography} environment.
  ;; Parsing is not as good as for the BibTeX database stuff.
  ;; The environment should be located in file FILE.

  (let* (start end buf entries re re-list)
    (unless file
      (error "Need file name to find thebibliography environment"))
    (setq buf (reftex-get-file-buffer-force 
	       file (not reftex-keep-temporary-buffers)))
    (unless buf
      (error "No such file %s" file))
    (message "Scanning thebibliography environment in %s" file)

    (save-excursion
      (set-buffer buf)
      (save-restriction
	(widen)
	(goto-char (point-min))
	(if (re-search-forward 
	     "\\(\\`\\|[\n\r]\\)[ \t]*\\\\begin{thebibliography}" nil t)
	    (progn
	      (beginning-of-line 2)
	      (setq start (point))))
	(if (re-search-forward 
	     "\\(\\`\\|[\n\r]\\)[ \t]*\\\\end{thebibliography}" nil t)
	    (progn
	      (beginning-of-line 1)
	      (setq end (point))))
	(when (and start (point))
	  (setq entries 
		(mapcar 'reftex-parse-bibitem
		   (delete ""
		      (split-string 
		       (buffer-substring-no-properties start end)
		       "[ \t\n\r]*\\\\bibitem\\(\\[[^]]*]\\)*")))))))
    (unless entries
      (error "No bibitems found"))

    (setq re-list (split-string 
		   (read-string "RegExp [ && RegExp...]: "
				nil 'reftex-cite-regexp-hist)
		   "[ \t]*&&[ \t]*"))
    (if (string-match "\\`[ \t]*\\'" (car re-list))
        (error "Empty regular expression"))

    (while (and (setq re (pop re-list)) entries)
      (setq entries 
	    (delete nil (mapcar
			 (function
			  (lambda (x)
			    (if (string-match re (cdr (assoc "&entry" x)))
				x nil)))
			 entries))))

    (setq entries 
	  (mapcar 
	   (function
	    (lambda (x)
	      (cons (cons "&formatted" (reftex-format-bibitem x)) x)))
	   entries))

    entries))

;; Parse and format individual entries

(defun reftex-get-bib-names (field entry)
  ;; Return a list with the author or editor names in ENTRY
  (let ((names (reftex-get-bib-field field entry)))
    (if (equal "" names)
        (setq names (reftex-get-bib-field "editor" entry)))
    (while (string-match "\\band\\b[ \t]*" names)
      (setq names (replace-match "\n" nil t names)))
    (while (string-match "[\\.a-zA-Z\\-]+\\.[ \t]*\\|,.*\\|[{}]+" names)
      (setq names (replace-match "" nil t names)))
    (while (string-match "^[ \t]+\\|[ \t]+$" names)
      (setq names (replace-match "" nil t names)))
    (while (string-match "[ \t][ \t]+" names)
      (setq names (replace-match " " nil t names)))
    (split-string names "\n")))

(defun reftex-parse-bibtex-entry (entry &optional from to)
  (let (alist key start field)
    (save-excursion
      (save-restriction
        (if entry
            (progn
              (switch-to-buffer "*RefTeX-scratch*")
              (fundamental-mode)
              (erase-buffer)
              (insert entry))
          (widen)
          (narrow-to-region from to))
        (goto-char (point-min))

        (if (re-search-forward
             "@\\(\\w+\\)[ \t\n\r]*[{(][ \t\n\r]*\\([^ \t\n\r,]+\\)" nil t)
            (setq alist
                  (list
                   (cons "&type" (downcase (reftex-match-string 1)))
                   (cons "&key"  (reftex-match-string 2)))))
        (while (re-search-forward "\\(\\w+\\)[ \t\n\r]*=[ \t\n\r]*" nil t)
          (setq key (downcase (reftex-match-string 1)))
          (cond
           ((= (following-char) ?{)
            (forward-char 1)
            (setq start (point))
            (condition-case nil
                (up-list 1)
              (error nil)))
           ((= (following-char) ?\")
            (forward-char 1)
            (setq start (point))
            (while (and (search-forward "\"" nil t)
                        (= ?\\ (char-after (- (point) 2))))))
           (t
            (setq start (point))
            (re-search-forward "[ \t\n\r,}]" nil 1)))
          (setq field (buffer-substring-no-properties start (1- (point))))
          ;; remove extra whitespace
          (while (string-match "[\n\t\r]\\|[ \t][ \t]+" field)
            (setq field (replace-match " " nil t field)))
          ;; remove leading garbage
          (if (string-match "^[ \t{]+" field)
              (setq field (replace-match "" nil t field)))
          ;; remove trailing garbage
          (if (string-match "[ \t}]+$" field)
              (setq field (replace-match "" nil t field)))
          (push (cons key field) alist))))
    alist))

(defun reftex-get-bib-field (fieldname entry)
  ;; Extract the field FIELDNAME from an ENTRY
  (or (cdr (assoc fieldname entry))
      ""))

(defun reftex-format-bib-entry (entry)
  ;; Format a BibTeX ENTRY so that it is nice to look at
  (let*
      ((auth-list (reftex-get-bib-names "author" entry))
       (authors (mapconcat 'identity auth-list ", "))
       (year      (reftex-get-bib-field "year" entry))
       (title     (reftex-get-bib-field "title" entry))
       (type      (reftex-get-bib-field "&type" entry))
       (key       (reftex-get-bib-field "&key"  entry))
       (extra
        (cond
         ((equal type "article")
          (concat (reftex-get-bib-field "journal" entry) " "
                  (reftex-get-bib-field "volume" entry) ", "
                  (reftex-get-bib-field "pages" entry)))
         ((equal type "book")
          (concat "book (" (reftex-get-bib-field "publisher" entry) ")"))
         ((equal type "phdthesis")
          (concat "PhD: " (reftex-get-bib-field "school" entry)))
         ((equal type "mastersthesis")
          (concat "Master: " (reftex-get-bib-field "school" entry)))
         ((equal type "inbook")
          (concat "Chap: " (reftex-get-bib-field "chapter" entry)
                  ", pp. " (reftex-get-bib-field "pages"   entry)))
         ((or (equal type "conference")
              (equal type "incollection")
              (equal type "inproceedings"))
          (concat "in: " (reftex-get-bib-field "booktitle" entry)))
         (t ""))))
    (setq authors (reftex-truncate authors 30 t t))
    (when (reftex-use-fonts)
      (put-text-property 0 (length authors) 'face reftex-bib-author-face
                         authors)
      (put-text-property 0 (length year)    'face reftex-bib-year-face
                         year)
      (put-text-property 0 (length title)   'face reftex-bib-title-face
                         title)
      (put-text-property 0 (length extra)   'face reftex-bib-extra-face
                         extra))
    (concat key "\n     " authors " " year " " extra "\n     " title "\n\n")))

(defun reftex-parse-bibitem (item)
  ;; Parse a \bibitem entry
  (let ((key "") (text ""))
    (when (string-match "\\`{\\([^}]+\\)}\\([\001-\255]*\\)" item)
      (setq key (match-string 1 item)
	    text (match-string 2 item)))
    ;; Clean up the text a little bit
    (while (string-match "[\n\r\t]\\|[ \t][ \t]+" text)
      (setq text (replace-match " " nil t text)))
    (if (string-match "\\`[ \t]+" text)
	(setq text (replace-match "" nil t text)))
    (list
     (cons "&key" key)
     (cons "&text" text)
     (cons "&entry" (concat key " " text)))))

(defun reftex-format-bibitem (item)
  ;; Format a \bibitem entry so that it is (relatively) nice to look at.
  (let ((text (reftex-get-bib-field "&text" item))
	(key  (reftex-get-bib-field "&key" item))
	(lines nil))

    ;; Wrap the text into several lines.
    (while (and (> (length text) 70)
		(string-match " " (substring text 60)))
	(push (substring text 0 (+ 60 (match-beginning 0))) lines)
	(setq text (substring text (+ 61 (match-beginning 0)))))
    (push text lines)
    (setq text (mapconcat 'identity (nreverse lines) "\n     "))

    (when (reftex-use-fonts)
      (put-text-property 0 (length text) 'face reftex-bib-author-face text))
    (concat key "\n     " text "\n\n")))

;; Make a citation

(defun reftex-citation (&optional no-insert)
  "Make a citation using BibTeX database files.
After asking for a Regular Expression, it scans the buffers with
bibtex entries (taken from the \\bibliography command) and offers the
matching entries for selection.  The selected entry is formated according
to `reftex-cite-format' and inserted into the buffer.
If NO-INSERT is non-nil, nothing is inserted, only the selected key returned.
The regular expression uses an expanded syntax: && is interpreted as `and'.
Thus, `aaaa&&bbb' matches entries which contain both `aaaa' and `bbb'.
When this function is called with point inside the braces of a \\cite
command, it will add another key, ignoring the value of `reftex-cite-format'.
When called with a numeric prefix, that many citations will be made and all
put into the same \\cite command.
When called with just C-u as prefix, enforces rescan of buffer for
bibliography statement (e.g. if it was changed)."

  (interactive)

  ;; check for recursive edit
  (reftex-check-recursive-edit)

  ;; if there is just 1 C-u prefix arg, force to rescan buffer
  (reftex-access-scan-info current-prefix-arg)

  ;; Call reftex-do-citation, but protected
  (unwind-protect
      (reftex-do-citation current-prefix-arg no-insert)
    (reftex-kill-temporary-buffers)))

(defun reftex-do-citation (&optional arg no-insert)
  ;; This really does the work of reftex-citation.

  ;; Check if there is already a cite command at point and change cite format
  ;; in order to only add another reference in the same cite command.
  (let (key format (macro (car (car (reftex-what-macro t)))))
    (if (and (stringp macro)
             (string-match "\\`\\\\cite\\|cite\\'" macro))
        (progn
          (cond
           ((or (not arg)
                (not (listp arg)))
            (setq format
                  (concat
                   (if (not (or (= (preceding-char) ?{)
                                (= (preceding-char) ?,)))
                       ","
                     "")
                   "%l"
                   (if (not (or (= (following-char) ?})
                                (= (following-char) ?,)))
                       ","
                     ""))))
           (t
            (setq format "%l"))))
      ;; else: figure out the correct format
      (setq format
            (cond
             ((stringp reftex-cite-format) reftex-cite-format)
             ((and (symbolp reftex-cite-format)
                   (assq reftex-cite-format reftex-cite-format-builtin))
              (nth 2 (assq reftex-cite-format reftex-cite-format-builtin)))
             (t reftex-cite-format)))
      (if (listp format)
          (save-window-excursion
            (with-output-to-temp-buffer "*RefTeX Select*"
              (princ "SELECT A CITATION FORMAT\n\n")
              (princ
               (mapconcat
                (function (lambda (x)
                            (format "[%c] %s  %s" (car x)
                                    (if (> (car x) 31) " " "")
                                    (cdr x))))
                format "\n")))
	    (reftex-enlarge-to-fit "*RefTeX Select*")
            (setq key (read-char))
            (if (assq key format)
                (setq format (cdr (assq key format)))
              (error "No citation format associated with key `%c'" key)))))

    (let* (entry data rtn ins-string re-list re found-list found-list-r)

      ;; Scan bibtex files
      (setq found-list
	    (cond
	     ((assq 'bib (symbol-value reftex-docstruct-symbol))
	      ;; using BibTeX database files.
	      (reftex-extract-bib-entries (reftex-get-bibfile-list)))
	     ((assq 'thebib (symbol-value reftex-docstruct-symbol))
	      ;; using thebibliography environment.
	      (reftex-extract-bib-entries-from-thebibliography
	       (cdr (assq 'thebib (symbol-value reftex-docstruct-symbol)))))
	     (reftex-default-bibliography
	      (message "Using default bibliography")
	      (reftex-extract-bib-entries reftex-default-bibliography))
	     (t (error "Document does not have a bibliography"))))

      (unless found-list
        (error "Sorry, no matches found"))

      ;; remember where we came from
      (setq reftex-call-back-to-this-buffer (current-buffer))
      (set-marker reftex-select-return-marker (point))

      ;; offer selection
      (save-window-excursion
	(delete-other-windows)
	(let ((default-major-mode 'reftex-select-bib-mode))
	  (reftex-kill-buffer "*RefTeX Select*")
	  (switch-to-buffer-other-window "*RefTeX Select*")
	  (unless (eq major-mode 'reftex-select-bib-mode)
	    (reftex-select-bib-mode))
	  (let ((buffer-read-only nil))
	    (erase-buffer)
	    (reftex-insert-bib-matches found-list)))
	(setq buffer-read-only t)
        (if (= 0 (buffer-size))
            (error "Sorry, no matches found"))
        (setq truncate-lines t)
        (goto-char 1)
        (if (catch 'exit
              (while t
                (setq rtn
                      (reftex-select-item
                       reftex-citation-prompt
                       reftex-citation-help
		       reftex-select-bib-map
                       nil
                       'reftex-bibtex-selection-callback nil))
                (setq key (car rtn)
                      data (nth 1 rtn))
                (unless key (throw 'exit nil))
                (cond
                 ((eq key ?g)
                  (setq found-list
                        (save-excursion
                          (set-buffer reftex-call-back-to-this-buffer)
                          (reftex-extract-bib-entries
                           (reftex-get-bibfile-list))))
		  (let ((buffer-read-only nil))
		    (erase-buffer)
		    (reftex-insert-bib-matches found-list))
                  (if (= 0 (buffer-size))
                      (error "Sorry, no matches found"))
                  (goto-char 1))

                 ((eq key ?r)
                  ;; restrict with new regular expression
                  (setq re-list
                        (split-string (read-string
				       "RegExp [ && RegExp...]: "
				       nil 'reftex-cite-regexp-hist)
				       "[ \t]*&&[ \t]*"))
                  (while re-list
                    (setq re (car re-list)
                          re-list (cdr re-list))
                    (setq found-list-r
                          (delete ""
                                  (mapcar
				   (function
				    (lambda (x)
				      (if (string-match
					   re (cdr (assoc "&entry" x)))
					  x
					"")))
                                   found-list))))
                  (if found-list-r
                      (setq found-list found-list-r)
                    (ding))
		  (let ((buffer-read-only nil))
		    (erase-buffer)
		    (reftex-insert-bib-matches found-list))
                  (goto-char 1))
                 ((eq key ?a)
                  (setq entry 'all)
                  (throw 'exit t))
                 ((or (eq key ?\C-m)
                      (eq key 'return))
                  (if data
                      (setq entry data)
                    (setq entry nil))
                  (throw 'exit t))
		 ((stringp key)
		  (setq entry (list (cons "&key" key)))
                  (throw 'exit t))
                 (t
                  (ding)))))
            (progn
              ;; format the entry
              (if (eq entry 'all)
                  (setq ins-string
                        (mapconcat
			 (function
			  (lambda (entry)
			    (if reftex-format-cite-function
				(funcall reftex-format-cite-function
					 (reftex-get-bib-field "&key" entry)
					 format)
			      (reftex-format-citation entry format))))
                         found-list "\n"))
                (setq ins-string 
		      (if reftex-format-cite-function
			  (funcall reftex-format-cite-function
				   (reftex-get-bib-field "&key" entry)
				   format)
			(reftex-format-citation entry format)))))
          (setq ins-string "")
          (message "Quit")))
      (set-marker reftex-select-return-marker (point))
      (kill-buffer "*RefTeX Select*")

      (unless no-insert
        (insert ins-string)
        (when (string-match "\\?" ins-string)
          (search-backward "?")
          (delete-char 1)))
      (message "")

      ;; Check if the prefix arg was numeric, and call recursively
      (when (and (integerp arg)
                 (> arg 1)
                 (re-search-backward
                  "\\\\\\([a-zA-Z]*cite\\|cite[a-zA-Z]*\\)\\**\\(\\[[^]]*\\]\\)*{\\([^}]*\\)" nil t))
        (goto-char (match-end 0))
        (decf arg)
        (reftex-do-citation arg))

      ;; Return the citation key
      (or (eq entry 'all)
          (reftex-get-bib-field "&key" entry)))))

(defun reftex-insert-bib-matches (list)
  ;; Insert the bib matches and number them correctly
  (let ((mouse-face
	 (if (memq reftex-highlight-selection '(mouse both))
	     reftex-mouse-selected-face
	   nil))
	tmp len)
    (mapcar 
     (function
      (lambda (x)
	(setq tmp (cdr (assoc "&formatted" x))
	      len (length tmp))
	(put-text-property 0 len ':data x tmp)
	(put-text-property 0 (1- len) 'mouse-face mouse-face tmp)
	(insert tmp)))
     list)))

(defun reftex-format-names (namelist n)
  (let (last (len (length namelist)))
    (cond
     ((= 1 len) (car namelist))
     ((> len n) (concat (car namelist) (nth 2 reftex-cite-punctuation)))
     (t
      (setq n (min len n)
            last (nth (1- n) namelist))
      (setcdr (nthcdr (- n 2) namelist) nil)
      (concat
       (mapconcat 'identity namelist (nth 0 reftex-cite-punctuation))
       (nth 1 reftex-cite-punctuation)
       last)))))

(defun reftex-format-citation (entry format)
  ;; Format a citation from the info in the BibTeX ENTRY

  (unless (stringp format) (setq format "\\cite{%l}"))

  (if (and reftex-comment-citations
           (string-match "%l" reftex-cite-comment-format))
      (error "reftex-cite-comment-format contains illegal %%l"))

  (while (string-match
          "\\(\\`\\|[^%]\\)\\(\\(%\\([0-9]*\\)\\([a-zA-Z]\\)\\)[.,;: ]*\\)"
          format)
    (let ((n (string-to-int (match-string 4 format)))
          (l (string-to-char (match-string 5 format)))
          rpl b e)
      (save-match-data
        (setq rpl
              (cond
               ((= l ?l) (concat
                          (reftex-get-bib-field "&key" entry)
                          (if reftex-comment-citations
                              reftex-cite-comment-format
                            "")))
               ((= l ?a) (reftex-format-names
                          (reftex-get-bib-names "author" entry)
                          (or n 2)))
               ((= l ?A) (car (reftex-get-bib-names "author" entry)))
               ((= l ?b) (reftex-get-bib-field "booktitle" entry))
               ((= l ?c) (reftex-get-bib-field "chapter" entry))
               ((= l ?d) (reftex-get-bib-field "edition" entry))
               ((= l ?e) (reftex-format-names
                          (reftex-get-bib-names "editor" entry)
                          (or n 2)))
               ((= l ?E) (car (reftex-get-bib-names "editor" entry)))
               ((= l ?h) (reftex-get-bib-field "howpublished" entry))
               ((= l ?i) (reftex-get-bib-field "institution" entry))
               ((= l ?j) (reftex-get-bib-field "journal" entry))
               ((= l ?k) (reftex-get-bib-field "key" entry))
               ((= l ?m) (reftex-get-bib-field "month" entry))
               ((= l ?n) (reftex-get-bib-field "number" entry))
               ((= l ?o) (reftex-get-bib-field "organization" entry))
               ((= l ?p) (reftex-get-bib-field "pages" entry))
               ((= l ?P) (car (split-string
                               (reftex-get-bib-field "pages" entry)
                               "[- .]+")))
               ((= l ?s) (reftex-get-bib-field "school" entry))
               ((= l ?u) (reftex-get-bib-field "publisher" entry))
               ((= l ?r) (reftex-get-bib-field "address" entry))
               ((= l ?t) (reftex-get-bib-field "title" entry))
               ((= l ?v) (reftex-get-bib-field "volume" entry))
               ((= l ?y) (reftex-get-bib-field "year" entry)))))

      (if (string= rpl "")
          (setq b (match-beginning 2) e (match-end 2))
        (setq b (match-beginning 3) e (match-end 3)))
      (setq format (concat (substring format 0 b) rpl (substring format e)))))
  (while (string-match "%%" format)
    (setq format (replace-match "%" t t format)))
  (while (string-match "[ ,.;:]*%<" format)
    (setq format (replace-match "" t t format)))
  format)

(defun reftex-bibtex-selection-callback (data ignore no-revisit)
  ;; Callback function to be called from the BibTeX selection, in
  ;; order to display context.  This function is relatively slow and not
  ;; recommended for follow mode.  It works OK for individual lookups.
  (let ((win (selected-window))
        (key (reftex-get-bib-field "&key" data))
        bibfile-list item tmp)

    (catch 'exit
      (save-excursion
	(set-buffer reftex-call-back-to-this-buffer)
	(cond
	 ((assq 'bib (symbol-value reftex-docstruct-symbol))
	  (setq bibfile-list (reftex-get-bibfile-list)))
	 ((setq tmp (assq 'thebib (symbol-value reftex-docstruct-symbol)))
	  (setq bibfile-list (list (cdr tmp))
		item t))
	 (reftex-default-bibliography
	  (setq bibfile-list reftex-default-bibliography))
	 (t (ding) (throw 'exit))))

      (when no-revisit
	(setq bibfile-list 
	      (delq nil (mapcar (lambda (x)
				  (if (get-file-buffer x) x nil))
				bibfile-list))))

      (reftex-pop-to-bibtex-entry 
       key bibfile-list (not reftex-keep-temporary-buffers) t item))

    (select-window win)))

;;; ===========================================================================
;;;
;;; Here is the routine used for selection

;; Marker for return point from recursive edit
(defvar reftex-recursive-edit-marker (make-marker))

(defvar reftex-last-data nil)
(defvar reftex-last-line nil)

(defun reftex-check-recursive-edit ()
  ;; Check if we are already in a recursive edit.  Abort with helpful
  ;; message if so.
  (if (marker-position reftex-recursive-edit-marker)
      (error
       (substitute-command-keys
        "In unfinished selection process. Finish, or abort with \\[abort-recursive-edit]."))))

(defun reftex-select-item (prompt help-string keymap
				  &optional offset
				  call-back cb-flag)
;; Select an item, using PROMPT. The function returns a key indicating
;; an exit status, along with a data structure indicating which item was
;; selected.
;; HELP-STRING contains help.  KEYMAP is a keymap with the available
;; selection commands.
;; OFFSET can be a label list item which will be selected at start.
;; When it is t, point will start out at the beginning of the buffer.
;; Any other value will cause restart where last selection left off.
;; When CALL-BACK is given, it is a function which is called with the index
;; of the element.
;; CB-FLAG is the initial value of that flag.

  (let* (ev data last-data callback-fwd (selection-buffer (current-buffer)))

    (setq ev
          (catch 'myexit
            (save-window-excursion
              (setq truncate-lines t)

	      ;; Find a good starting point
	      (cond
	       (offset
		(goto-char
		 (or (and (listp offset)
			  (text-property-any (point-min) (point-max)
					     ':data offset))
		     (and (local-variable-p 'reftex-last-data (current-buffer))
			  (boundp 'reftex-last-data)
			  (listp reftex-last-data)
			  (text-property-any (point-min) (point-max)
					     ':data reftex-last-data))
		     (and (local-variable-p 'reftex-last-line (current-buffer))
			  (boundp 'reftex-last-line)
			  (integerp reftex-last-line)
			  (progn (goto-line reftex-last-line) (point)))
		     (point-min))))
	       (t (goto-char (point-min))))
              (beginning-of-line 1)
	      (set (make-local-variable 'reftex-last-follow-point) (point))

      (unwind-protect
	  (progn
	    (use-local-map keymap)
	    (add-hook 'pre-command-hook 'reftex-select-pre-command-hook nil t)
	    (add-hook 'post-command-hook 'reftex-select-post-command-hook nil t)
	    (princ prompt)
	    (set-marker reftex-recursive-edit-marker (point))
	    (run-hooks 'post-command-hook)  ;; because XEmacs does not do it
	    (recursive-edit))

	(set-marker reftex-recursive-edit-marker nil)
	(save-excursion
	  (set-buffer selection-buffer)
	  (use-local-map nil)
	  (remove-hook 'pre-command-hook 'reftex-select-pre-command-hook t)
	  (remove-hook 'post-command-hook 
		       'reftex-select-post-command-hook t))))))

    (set (make-local-variable 'reftex-last-line)
	 (+ (count-lines (point-min) (point)) (if (bolp) 1 0)))
    (set (make-local-variable 'reftex-last-data) last-data)
    (reftex-kill-buffer "*RefTeX Help*")
    (message "")
    (list ev data last-data)))

;; The following variables are all bound dynamically in `reftex-select-item'.
;; The defvars are here only to silence the byte compiler.

(defvar found-list)
(defvar cb-flag)
(defvar data)
(defvar prompt)
(defvar last-data)
(defvar call-back)
(defvar help-string)
(defvar callback-fwd)
(defvar varioref)

;; The selection commands

(defun reftex-select-pre-command-hook ()
  (reftex-unhighlight 1)
  (reftex-unhighlight 0))

(defun reftex-select-post-command-hook ()
  (let (b e)
    (setq data (get-text-property (point) ':data))
    (setq last-data (or data last-data))
  
    (when (and data cb-flag
	       (not (equal reftex-last-follow-point (point))))
      (setq reftex-last-follow-point (point))
      (funcall call-back data callback-fwd 
	       (not reftex-revisit-to-follow)))
    (if data
	(setq b (or (previous-single-property-change
		     (1+ (point)) ':data)
		    (point-min))
	      e (or (next-single-property-change
		     (point) ':data)
		    (point-max)))
      (setq b (point) e (point)))
    (and (memq reftex-highlight-selection '(cursor both))
	 (reftex-highlight 1 b e))
    (if (or (not (pos-visible-in-window-p b))
	    (not (pos-visible-in-window-p e)))
	(recenter (/ (window-height) 2)))
    (when (and (fboundp 'current-message)
	       (not (current-message)))
      (princ prompt))))

(defun reftex-select-next (&optional arg)
  (interactive "p")
  (setq callback-fwd t)
  (or (eobp) (forward-char 1))
  (re-search-forward "^[^. \t\n\r]" nil t arg)
  (beginning-of-line 1))
(defun reftex-select-previous (&optional arg)
  (interactive "p")
  (setq callback-fwd nil)
  (re-search-backward "^[^. \t\n\r]" nil t arg))
(defun reftex-select-next-heading (&optional arg)
  (interactive "p")
  (end-of-line)
  (re-search-forward "^ " nil t arg)
  (beginning-of-line))
(defun reftex-select-previous-heading (&optional arg)
  (interactive "p")
  (re-search-backward "^ " nil t arg))
(defun reftex-select-quit ()
  (interactive)
  (throw 'myexit nil))
(defun reftex-select-keyboard-quit ()
  (interactive)
  (throw 'exit t))
(defun reftex-select-jump-to-previous ()
  (interactive)
  (let (pos)
    (cond
     ((and (local-variable-p 'reftex-last-data (current-buffer))
	   reftex-last-data
	   (setq pos (text-property-any (point-min) (point-max)
					':data reftex-last-data)))
      (goto-char pos))
     ((and (local-variable-p 'reftex-last-line (current-buffer))
	   (integerp reftex-last-line))
      (goto-line reftex-last-line))
     (t (ding)))))
(defun reftex-select-toggle-follow ()
  (interactive)
  (setq cb-flag (not cb-flag)))
(defun reftex-select-toggle-varioref ()
  (interactive)
  (if (string= varioref "\\ref")
      (setq varioref "\\vref")
    (setq varioref "\\ref"))
  (force-mode-line-update))
(defun reftex-select-show-insertion-point ()
  (interactive)
  (let ((this-window (selected-window)))
    (unwind-protect
	(progn
	  (switch-to-buffer-other-window
	   (marker-buffer reftex-select-return-marker))
	  (goto-char (marker-position reftex-select-return-marker))
	  (recenter (/ (window-height) 2)))
      (select-window this-window))))
(defun reftex-select-callback ()
  (interactive)
  (if data (funcall call-back data callback-fwd nil) (ding)))
(defun reftex-select-accept ()
  (interactive)
  (throw 'myexit 'return))
(defun reftex-select-mouse-accept (ev)
  (interactive "e")
  (mouse-set-point ev)
  (setq data (get-text-property (point) ':data))
  (setq last-data (or data last-data))
  (throw 'myexit 'return))
(defun reftex-select-read-label ()
  (interactive)
  (let ((label (completing-read 
		"Label: " (symbol-value reftex-docstruct-symbol)
		nil nil reftex-prefix)))
    (unless (or (equal label "") (equal label reftex-prefix))
      (throw 'myexit label))))
(defun reftex-select-read-cite ()
  (interactive)
  (let* ((list (mapcar (lambda (x) 
			 (cons (reftex-get-bib-field "&key" x) 1))
		       found-list))
	 (key (completing-read "Citation key: " list)))
    (unless (equal key "")
      (throw 'myexit key))))
(defun reftex-select-help ()
  (interactive)
  (with-output-to-temp-buffer "*RefTeX Help*"
    (princ help-string))
  (reftex-enlarge-to-fit "*RefTeX Help*" t))

;;; ===========================================================================
;;;
;;; View cross references

(defun reftex-view-crossref (&optional arg)
  "View cross reference of \\ref or \\cite macro at point.
If the macro at point is a \\ref, show the corresponding label definition.
If it is a \\cite, show the BibTeX database entry or the \\bibitem.
If there is no such macro at point, search forward to find one.
When you call this function several times in direct succession, point will
move to view subsequent cross references further down in the buffer.
To cope with the plethora of variations in packages, this function
assumes any macro either starting with or ending in `ref' or `cite' to
contain cross references.
When the LaTeX package `xr' is being used, this command will also view
crossreferences in external documents.  However, this works correctly only
when the \\externaldocument macros are used with the optional label prefix
argument.
With one or two C-u prefixes, enforce rescanning of the document.
With argument t or 1, select the window showing the cross reference."

  (interactive "P")

  ;; See where we are.
  (let* ((re "\\\\\\([a-z]*\\(cite\\|ref\\)\\|\\(cite\\|ref\\)[a-z]*\\)\\**\\(\\[[^{}]*\\]\\)?{")
         (macro (car (car (reftex-what-macro t))))
         (this-word (reftex-this-word "*a-zA-Z\\\\"))
         (my-window (selected-window))
         pop-window cmd args point)

    (if (and macro
             (string-match "\\`\\\\cite\\|\\`\\\\ref\\|cite\\'\\|ref\\'"
                           macro))
        (and (setq macro (match-string 0 macro))
             (string-match "\\`\\\\" macro)
             (setq macro (substring macro 1)))
      (setq macro nil))

    (if (and macro 
	     (eq last-command this-command)
	     (eq last-command 'reftex-view-crossref))
        (if (string= macro "cite")
            (progn
              (skip-chars-forward "^},%")
              (while (and (eq (following-char) ?%)
                          (or (beginning-of-line 2) t)
                          (skip-chars-forward " \t\n\r")))
              (skip-chars-forward ",")
              (if (eq (following-char) ?})
                  (setq macro nil)))
          (setq macro nil)))

    (if (and (not macro)
             (or (not (string-match "\\`\\\\" this-word))
		 (eq (following-char) ?\\)
                 (search-backward "\\" nil t)
                 t))
	(if (interactive-p)
	    ;; Only move far if this function was called directly
	    (and (re-search-forward re nil t)
		 (setq macro (or (match-string 2) (match-string 3))))
	  ;; The macro needs to be at point
	  (and (looking-at re)
	       (setq macro (or (match-string 2) (match-string 3)))
	       (goto-char (match-end 0)))))
	     

    (unless macro
      (error "No cross reference to display"))

    ;; Ensure access to scanning info
    (reftex-access-scan-info current-prefix-arg)

    (cond
     ((string= macro "cite")
      (cond
       ((assq 'bib (symbol-value reftex-docstruct-symbol))
	(setq cmd 'reftex-pop-to-bibtex-entry
	      args (list
		    (reftex-this-word "^{},%\n\r")
		    (reftex-get-bibfile-list) nil t)))
       ((assq 'thebib (symbol-value reftex-docstruct-symbol))
	(setq cmd 'reftex-pop-to-bibtex-entry
	      args (list 
		    (reftex-this-word "^{},%\n\r")
		    (list (cdr (assq 'thebib 
				     (symbol-value reftex-docstruct-symbol))))
		    nil t t)))
       (t (error "Cannot display crossref\n"))))
     ((string= macro "ref")
      (let* ((label (reftex-this-word "^{}%\n\r"))
	     (xr-data (assoc 'xr (symbol-value reftex-docstruct-symbol)))
	     (xr-re (nth 2 xr-data))
             (entry (assoc label (symbol-value reftex-docstruct-symbol))))

	(if (and (not entry) (string-match xr-re label))
	    ;; Label is defined in external document
	    (save-excursion
	      (save-match-data
		(set-buffer 
		 (or (reftex-get-file-buffer-force
		      (cdr (assoc (match-string 1 label) (nth 1 xr-data))))
		     (error "Problem with external label %s" label))))
	      (setq label (substring label (match-end 1)))
	      (reftex-access-scan-info)
	      (setq entry 
		    (assoc label (symbol-value reftex-docstruct-symbol)))))
	(if entry
	    (setq cmd 'reftex-pop-to-label
                  args (list label (list (nth 3 entry)) nil t))		
          (error "Label %s not known - reparse document might help" label))))
     (t (error "This should not happen (reftex-view-crossref)")))
    (setq point (point))
    (apply cmd args)
    (setq pop-window (selected-window))
    (add-hook 'pre-command-hook 'reftex-highlight-shall-die)
    (select-window my-window)
    (goto-char point)
    (when (or (equal arg t) (equal arg 1))
      (select-window pop-window))))

(defun reftex-mouse-view-crossref (ev)
  "View cross reference of \\ref or \\cite macro where you click.
If the macro at point is a \\ref, show the corresponding label definition.
If it is a \\cite, show the BibTeX database entry.
If there is no such macro at point, search forward to find one.
With argument, actually select the window showing the cross reference."
  (interactive "e")
  (mouse-set-point ev)
  (reftex-view-crossref current-prefix-arg))

;;; ===========================================================================
;;;
;;; Functions that check out the surroundings

(defun reftex-what-macro (which &optional bound)
  ;; Find out if point is within the arguments of any TeX-macro.
  ;; The return value is either ("\\macro" . (point)) or a list of them.

  ;; If WHICH is nil, immediately return nil.
  ;; If WHICH is t, return list of all macros enclosing point.
  ;; If WHICH is a list of macros, look only for those macros and return the
  ;;    name of the first macro in this list found to enclose point.
  ;; If the optional BOUND is an integer, bound backwards directed
  ;;    searches to this point.  If it is nil, limit to nearest \section -
  ;;    like statement.

  ;; This function is pretty stable, but can be fooled if the text contains
  ;; things like \macro{aa}{bb} where \macro is defined to take only one
  ;; argument.  As RefTeX cannot know this, the string "bb" would still be
  ;; considered an argument of macro \macro.

  (unless reftex-section-regexp (reftex-compute-ref-cite-tables))
  (catch 'exit
    (if (null which) (throw 'exit nil))
    (let ((bound (or bound (save-excursion (re-search-backward
                                            reftex-section-regexp nil 1)
                                           (point))))
          pos cmd-list cmd cnt cnt-opt entry)
      (save-restriction
        (save-excursion
          (narrow-to-region (max 1 bound) (point-max))
          ;; move back out of the current parenthesis
          (while (condition-case nil
                     (progn (up-list -1) t)
                   (error nil))
            (setq cnt 1 cnt-opt 0)
            ;; move back over any touching sexps
            (while (and (reftex-move-to-previous-arg bound)
			(condition-case nil
			    (progn (backward-sexp) t)
			  (error nil)))
	      (if (eq (following-char) ?\[) (incf cnt-opt))
              (incf cnt))
            (setq pos (point))
            (when (and (or (= (following-char) ?\[)
                           (= (following-char) ?\{))
                       (re-search-backward "\\\\[*a-zA-Z]+\\=" nil t))
              (setq cmd (reftex-match-string 0))
	      (when (looking-at "\\\\begin{[^}]*}")
		(setq cmd (reftex-match-string 0)
		      cnt (1- cnt)))
	      ;; This does ignore optional arguments.  Very hard to fix.
	      (when (setq entry (assoc cmd reftex-env-or-mac-alist))
		(if (> cnt (or (nth 4 entry) 100))
		    (setq cmd nil)))
              (cond
	       ((null cmd))
	       ((eq t which)
		(push (cons cmd (point)) cmd-list))
	       ((member cmd which)
		(throw 'exit (cons cmd (point))))))
            (goto-char pos)))
        (nreverse cmd-list)))))

(defun reftex-what-environment (which &optional bound)
  ;; Find out if point is inside a LaTeX environment.
  ;; The return value is (e.g.) either ("equation" . (point)) or a list of
  ;; them.

  ;; If WHICH is nil, immediately return nil.
  ;; If WHICH is t, return list of all environments enclosing point.
  ;; If WHICH is a list of environments, look only for those environments and
  ;;   return the name of the first environment in this list found to enclose
  ;;   point.

  ;; If the optional BOUND is an integer, bound backwards directed searches to
  ;; this point.  If it is nil, limit to nearest \section - like statement.

  (catch 'exit
    (save-excursion
      (if (null which) (throw 'exit nil))
      (let ((bound (or bound (save-excursion (re-search-backward
                                              reftex-section-regexp nil 1)
                                             (point))))
            env-list end-list env)
        (while (re-search-backward "\\\\\\(begin\\|end\\){\\([^}]+\\)}"
                                   bound t)
          (setq env (buffer-substring-no-properties
                     (match-beginning 2) (match-end 2)))
          (cond
           ((string= (match-string 1) "end")
            (add-to-list 'end-list env))
           ((member env end-list)
            (setq end-list (delete env end-list)))
           ((eq t which)
            (push (cons env (point)) env-list))
           ((member env which)
            (throw 'exit (cons env (point))))))
        (nreverse env-list)))))

;; ============================================================================
;;
;; Some generally useful functions

(defun reftex-no-props (string)
  ;; Return STRING with all text properties removed
  (and (stringp string)
       (set-text-properties 0 (length string) nil string))
  string)

(defun reftex-match-string (n)
  ;; Match string without properties
  (when (match-beginning n)
    (buffer-substring-no-properties (match-beginning n) (match-end n))))

(defun reftex-kill-buffer (buffer)
  ;; Kill buffer if it exists.
  (and (setq buffer (get-buffer buffer))
       (kill-buffer buffer)))

(defun reftex-erase-buffer (&optional buffer)
  ;; Erase BUFFER if it exists.  BUFFER defaults to current buffer.
  ;; This even erases read-only buffers.
  (cond
   ((null buffer)
    ;; erase current buffer
    (let ((buffer-read-only nil)) (erase-buffer)))
   ((setq buffer (get-buffer buffer))
    ;; buffer exists
    (save-excursion
      (set-buffer buffer)
      (let ((buffer-read-only nil)) (erase-buffer))))))

(defun reftex-this-word (&optional class)
  ;; Grab the word around point.
  (setq class (or class "-a-zA-Z0-9:_/.*;|"))
  (save-excursion
    (buffer-substring-no-properties
     (progn (skip-chars-backward class) (point))
     (progn (skip-chars-forward  class) (point)))))

(defvar enable-multibyte-characters)
(defun reftex-truncate (string ncols &optional ellipses padding)
  ;; Truncate a string to NCHAR characters.  
  ;; Works fast with ASCII and correctly with Mule characters.
  ;; When ELLIPSES is non-nil, put three dots at the end of the string.
  (setq string
	(cond
	 ((and (boundp 'enable-multibyte-characters)
	       enable-multibyte-characters)
	  (if (<= (string-width string) ncols)
	      string
	    (if ellipses
		(concat (truncate-string-to-width string (- ncols 3)) "...")
	      (truncate-string-to-width string ncols))))
	 (t
	  (if (<= (length string) ncols)
	      string
	    (if ellipses
		(concat (substring string 0 (- ncols 3)) "...")
	      (substring string 0 ncols))))))
  (if padding
      (format (format "%%-%ds" ncols) string)
    string))

(defun reftex-nearest-match (regexp &optional pos)
  ;; Find the nearest match of REGEXP.  Set the match data.
  ;; If POS is given, calculate distances relative to it.
  ;; Return nil if there is no match.
  (let ((start (point)) (pos (or pos (point))) match1 match2 match)
    (goto-char start)
    (when (re-search-backward regexp nil t)
      (setq match1 (match-data)))
    (goto-char start)
    (when (re-search-forward regexp nil t)
      (setq match2 (match-data)))
    (goto-char start)
    (setq match
          (cond
           ((not match1) match2)
           ((not match2) match1)
           ((< (abs (- pos (car match1))) (abs (- pos (car match2)))) match1)
           (t match2)))
    (if match (progn (set-match-data match) t) nil)))

(defun reftex-auto-mode-alist ()
  ;; Return an `auto-mode-alist' with only the .gz (etc) thingies.
  ;; Stolen from gnus nnheader.
  (let ((alist auto-mode-alist)
        out)
    (while alist
      (when (listp (cdr (car alist)))
        (push (car alist) out))
      (pop alist))
    (nreverse out)))

(defun reftex-enlarge-to-fit (buf2 &optional keep-current)
  ;; Enlarge other window displaying buffer without killing current window
  ;; If KEEP-CURRENT in non-nil, current buffer must remain visible.
  (let* ((win1 (selected-window))
	 (buf1 (current-buffer))
	 (win2 (get-buffer-window buf2)))
    (when win2
      (select-window win2)
      (unless (and (pos-visible-in-window-p 1)
		   (pos-visible-in-window-p (point-max)))
	(enlarge-window (1+ (- (count-lines 1 (point-max))
			       (window-height))))))
    (cond
     ((window-live-p win1) (select-window win1))
     (keep-current
      ;; we must have the old buffer!
      (switch-to-buffer-other-window buf1)
      (shrink-window (- (window-height) window-min-height))))))

(defun reftex-access-search-path (which &optional recurse file)
  ;; Access path from environment variables. WHICH is either "tex" or "bib".
  ;; When RECURSE is t, expand recursive paths, ending in double slash.
  ;; FILE is just for the message.
  (let* ((pathvar (intern (concat "reftex-" which "-path")))
	 (status (get pathvar 'status)))
    (cond
     ((eq status 'recursed))
     ((and status (null recurse)))
     ((null status)
      (let ((env-vars (if (equal which "tex") (list "TEXINPUTS")
			reftex-bibpath-environment-variables)))
	(set pathvar (reftex-parse-colon-path
		       (mapconcat (function (lambda(x) (or (getenv x) "")))
				  env-vars path-separator))))
	(put pathvar 'status 'split))
     ((and (eq 'split status) recurse)
      (message "Expanding search path to find %s file: %s ..." which file)
      (set pathvar (reftex-expand-path (symbol-value pathvar)))
      (put pathvar 'status 'recursed)))))

(defun reftex-find-file-on-path (file path)
  ;; Find FILE along the directory list PATH.
  (catch 'exit
    (when (file-name-absolute-p file)
      (if (file-exists-p file)
	  (throw 'exit file)
	(throw 'exit nil)))
    (let* ((thepath path) file1 dir 
	   (doubleslash (concat "/" "/")))
      (while (setq dir (pop thepath))
	(when (string= (substring dir -2) doubleslash)
	  (setq dir (substring dir 0 -1)))
	(setq file1 (expand-file-name file (expand-file-name dir)))
	(if (file-exists-p file1)
	    (throw 'exit file1)))
      ;; No such file
      nil)))

(defun reftex-parse-colon-path (path)
  ;; Like parse-colon-parse, but // or /~ have no effects.
  (mapcar 'file-name-as-directory
	  (delete "" (split-string path (concat path-separator "+")))))

(defun reftex-expand-path (path)
  ;; Expand parts of path ending in a double slash
  (let (path1 dir dirs (doubleslash (concat "/" "/")))
    (while (setq dir (pop path))
      (if (string= (substring dir -2) doubleslash)
	  (progn
	    (setq dir (substring dir 0 -1))
	    (setq dirs (reftex-recursive-directory-list dir))
	    (setq path1 (append dirs path1)))
	(push dir path1)))
    (nreverse path1)))

(defun reftex-recursive-directory-list (dir)
  (let ((path (list dir)) dirs path1)
    (while (setq dir (pop path))
      (setq dirs
	  (delete nil 
	    (mapcar 
	     (function 
	      (lambda (x)
	        (if (and (file-directory-p x)
		         (not (string-match "/\\.+\\'" x)))
		    (file-name-as-directory x)
		  nil)))
		    (directory-files dir t))))
      (setq path (append dirs path))
      (push dir path1))
    path1))

(defun reftex-make-regexp-allow-for-ctrl-m (string)
  ;; convert STRING into a regexp, allowing ^M for \n and vice versa
  (let ((start -2))
    (setq string (regexp-quote string))
    (while (setq start (string-match "[\n\r]" string (+ 3 start)))
      (setq string (replace-match "[\n\r]" nil t string)))
    string))

(defun reftex-make-desperate-section-regexp (old)
  ;; Return a regexp which will still match a section statement even if
  ;; x-symbol or isotex or the like have been at work in the mean time.
  (let* ((n (1+ (string-match "[[{]" old)))
         (new (regexp-quote (substring old 0 (1+ (string-match "[[{]" old)))))
         (old (substring old n)))
    (while (string-match
            "\\([\r\n]\\)\\|\\(\\`\\|[ \t\n\r]\\)\\([a-zA-Z0-9]+\\)\\([ \t\n\r]\\|}\\'\\)"
            old)
      (if (match-beginning 1)
          (setq new (concat new "[^\n\r]*[\n\r]"))
        (setq new (concat new "[^\n\r]*" (match-string 3 old))))
      (setq old (substring old (match-end 0))))
    new))

(defun reftex-delete-list (elt-list list)
  ;; like delete, but with a list of things to delete
  ;; (original code from Rory Molinari)
  (while elt-list
    (setq list (delete (car elt-list) list)
          elt-list (cdr elt-list)))
  list)

(defun reftex-get-buffer-visiting (file)
  ;; return a buffer visiting FILE
  (cond
   ((boundp 'find-file-compare-truenames) ; XEmacs
    (let ((find-file-compare-truenames t))
      (get-file-buffer file)))
   ((fboundp 'find-buffer-visiting)       ; Emacs
    (find-buffer-visiting file))
   (t (error "This should not happen (reftex-get-buffer-visiting)"))))

(defun reftex-get-file-buffer-force (file &optional mark-to-kill)
  ;; Return a buffer visiting file.  Make one, if necessary.
  ;; If neither such a buffer nor the file exist, return nil.
  ;; If MARK-TO-KILL is t and there is no live buffer, load the file with
  ;; initializations according to `reftex-initialize-temporary-buffers',
  ;; and mark the buffer to be killed after use.

  (let ((buf (reftex-get-buffer-visiting file)))

    (cond (buf
           ;; We have it already as a buffer - just return it
           buf)

          ((file-readable-p file)
           ;; At least there is such a file and we can read it.

           (if (or (not mark-to-kill)
                   (eq t reftex-initialize-temporary-buffers))

               ;; Visit the file with full magic
               (setq buf (find-file-noselect file))

             ;; Else: Visit the file just briefly, without or
             ;;       with limited Magic

             ;; The magic goes away
             (let ((format-alist nil)
                   (auto-mode-alist (reftex-auto-mode-alist))
                   (default-major-mode 'fundamental-mode)
		   (enable-local-variables nil)
                   (after-insert-file-functions nil))
               (setq buf (find-file-noselect file)))

             ;; Is there a hook to run?
             (when (listp reftex-initialize-temporary-buffers)
               (save-excursion
                 (set-buffer buf)
                 (run-hooks 'reftex-initialize-temporary-buffers))))

	   ;; Lets see if we got a license to kill :-|
	   (and mark-to-kill
		(add-to-list 'reftex-buffers-to-kill buf))

           ;; Return the new buffer
           buf)

          ;; If no such file exists, return nil
          (t nil))))

(defun reftex-splice-symbols-into-list (list alist)
  ;; Splice the association in ALIST of any symbols in LIST into the list.
  ;; Return new list.
  (let (rtn tmp)
    (while list
      (while (and (not (null (car list)))  ;; keep list elements nil
                  (symbolp (car list)))
        (setq tmp (car list))
        (cond
         ((assoc tmp alist)
          (setq list (append (nth 2 (assoc tmp alist)) (cdr list))))
         (t
          (error "Cannot treat symbol %s in reftex-label-alist"
                 (symbol-name tmp)))))
      (push (pop list) rtn))
    (nreverse rtn)))

(defun reftex-uniquify (alist &optional keep-list)
  ;; Return a list of all elements in ALIST, but each car only once.
  ;; Elements of KEEP-LIST are not removed even if duplicate.
  (let (new elm)
    (while alist
      (setq elm (pop alist))
      (if (or (member (car elm) keep-list)
              (not (assoc (car elm) new)))
          (push elm new)))
    (nreverse new)))

(defun reftex-use-fonts ()
  ;; Return t if we can and want to use fonts.
  (and window-system
       reftex-use-fonts
       (featurep 'font-lock)))

(defun reftex-refontify ()
  ;; Return t if we need to refontify context
  (and (reftex-use-fonts)
       (or (eq t reftex-refontify-context)
           (and (eq 1 reftex-refontify-context)
                (or (featurep 'x-symbol))))))

(defun reftex-fontify-select-label-buffer ()
  ;; Fontify the `*RefTeX Select*' buffer
  (cond
   ((fboundp 'font-lock-default-fontify-region)
    ;; Good: we have the indirection functions, and can support lazy-lock etc.
    (set (make-local-variable 'font-lock-fontify-region-function)
	 'reftex-select-font-lock-fontify-region)
    (let ((major-mode 'latex-mode))
      (font-lock-mode 1)))
   ((fboundp 'font-lock-set-defaults-1)
    ;; Looks like the XEmacs font-lock stuff.
    ;; FIXME: this is still kind of a hack, but might go away some day.
    (set (make-local-variable 'font-lock-keywords) nil)
    (let ((major-mode 'latex-mode)
	  (font-lock-defaults-computed nil))
      (font-lock-set-defaults-1)
      (reftex-select-font-lock-fontify-region (point-min) (point-max))))
   (t
    ;; Oops? 
    (message "Sorry: cannot refontify RefTeX Select buffer."))))

(defun reftex-select-font-lock-fontify-region (beg end &optional loudly)
  ;; Fontify a region, but only lines starting with a dot.
  (let ((func (if (fboundp 'font-lock-default-fontify-region)
		  'font-lock-default-fontify-region
		'font-lock-fontify-region))
	beg1 end1)
    (goto-char beg)
    (while (re-search-forward "^\\." end t)
      (setq beg1 (point) end1 (progn (skip-chars-forward "^\n") (point)))
      (funcall func beg1 end1 nil)
      (goto-char end1))))

;; Highlighting uses overlays.  If this is for XEmacs, we need to load
;; the overlay library, available in version 19.15
(and (not (fboundp 'make-overlay))
     (condition-case nil
         (require 'overlay)
       (error
        (error "RefTeX needs overlay emulation (available in XEmacs 19.15)"))))

;; We keep a vector with several different overlays to do our highlighting.
(defvar reftex-highlight-overlays [nil nil])

;; Initialize the overlays
(aset reftex-highlight-overlays 0 (make-overlay 1 1))
(overlay-put (aref reftex-highlight-overlays 0) 
	     'face 'highlight)
(aset reftex-highlight-overlays 1 (make-overlay 1 1))
(overlay-put (aref reftex-highlight-overlays 1)
	     'face reftex-cursor-selected-face)

;; Two functions for activating and deactivation highlight overlays
(defun reftex-highlight (index begin end &optional buffer)
  "Highlight a region with overlay INDEX."
  (move-overlay (aref reftex-highlight-overlays index)
                begin end (or buffer (current-buffer))))
(defun reftex-unhighlight (index)
  "Detach overlay INDEX."
  (delete-overlay (aref reftex-highlight-overlays index)))

(defun reftex-highlight-shall-die ()
  ;; Function used in pre-command-hook to remove highlights.
  (remove-hook 'pre-command-hook 'reftex-highlight-shall-die)
  (reftex-unhighlight 0))

;;; ---------------------------------------------------------------------------
;;;
;;; Functions to compile the tables, reset the mode etc.

(defun reftex-reset-mode ()
  "Reset RefTeX Mode.  Required to implement changes to some list variables.
This function will compile the information in `reftex-label-alist' and similar
variables.  It is called when RefTeX is first used, and after changes to
these variables."
  (interactive)

  ;; Record that we have done this
  (setq reftex-tables-dirty nil)
  (setq reftex-memory 
	(list reftex-label-alist reftex-label-alist-external-add-ons
	      reftex-default-label-alist-entries))

  ;; Reset the file search path variables
  (put 'reftex-tex-path 'status nil)
  (put 'reftex-bib-path 'status nil)

  ;; Kill temporary buffers associated with RefTeX - just in case they
  ;; were not cleaned up properly
  (save-excursion
    (let ((buffer-list '("*RefTeX Help*" "*RefTeX Select*"
			 "*Duplicate Labels*" "*toc*" "*RefTeX-scratch*"))
	  buf)
      (while (setq buf (pop buffer-list))
	(if (get-buffer buf)
	    (kill-buffer buf))))
    (reftex-erase-all-selection-buffers))

  ;; Make sure the current document will be rescanned soon.
  (reftex-reset-scanning-information)

  ;; Plug functions into AUCTeX if the user option says so.
  (reftex-plug-into-AUCTeX)

  (message "updating internal tables...")
  (reftex-compute-ref-cite-tables)
  (message "updating internal tables... done"))

(defun reftex-reset-scanning-information ()
  "Reset the symbols containing information from buffer scanning.
This enforces rescanning the buffer on next use."
  (if (string= reftex-last-toc-master (reftex-TeX-master-file))
      (reftex-erase-buffer "*toc*"))
  (let ((symlist reftex-multifile-symbols)
        symbol)
    (while symlist
      (setq symbol (car symlist)
            symlist (cdr symlist))
      (if (and (symbolp (symbol-value symbol))
               (not (null (symbol-value symbol))))
          (set (symbol-value symbol) nil)))))

(defun reftex-erase-all-selection-buffers ()
  ;; Remove all selection buffers associated with current document.
  (mapcar (function 
	   (lambda (type)
	     (reftex-erase-buffer (reftex-make-selection-buffer-name type))))
	  reftex-typekey-list))

(defun reftex-compute-ref-cite-tables ()
  ;; Update ref and cite tables

  ;; Update AUCTeX style information
  (when (and (featurep 'tex-site) (fboundp 'TeX-update-style))
    (condition-case nil (TeX-update-style) (error nil)))

  ;; Compile information in reftex-label-alist
  (let ((tmp (reftex-uniquify (reftex-splice-symbols-into-list
                              (append
                               reftex-label-alist
                               reftex-label-alist-external-add-ons
                               reftex-default-label-alist-entries)
                              reftex-label-alist-builtin)
                             '(nil)))
        entry env-or-mac typekeychar typekey prefix context word
        fmt reffmt labelfmt wordlist qh-list macros-with-labels
        nargs nlabel opt-args cell sum i words-are-re)

    (setq reftex-words-to-typekey-alist nil
          reftex-typekey-list nil
          reftex-typekey-to-format-alist nil
          reftex-typekey-to-prefix-alist nil
          reftex-env-or-mac-alist nil
          reftex-label-env-list nil
          reftex-label-mac-list nil)
    (while tmp
      (catch 'next-entry
        (setq entry (car tmp)
              env-or-mac (car entry)
              entry (cdr entry)
              tmp (cdr tmp))
        (if (null env-or-mac)
            (setq env-or-mac ""))
        (if (stringp (car entry))
            ;; This is before version 2.00 - convert entry to new format
            ;; This is just to keep old users happy
            (setq entry (cons (string-to-char (car entry))
                              (cons (concat (car entry) ":")
                                    (cdr entry)))))
        (setq typekeychar (nth 0 entry)
              typekey (char-to-string typekeychar)
              prefix (nth 1 entry)
              fmt (nth 2 entry)
              context (nth 3 entry)
              wordlist (nth 4 entry))
        (if (stringp wordlist)
            ;; This is before version 2.04 - convert to new format
            (setq wordlist (nthcdr 4 entry)))

        (if (and (stringp fmt)
                 (string-match "@" fmt))
            ;; special syntax for specifying a label format
            (setq fmt (split-string fmt "@+"))
          (setq fmt (list "\\label{%s}" fmt)))
        (setq labelfmt (car fmt)
              reffmt (nth 1 fmt))
        (if typekey
            (add-to-list 'reftex-typekey-list typekey))
        (if (and typekey prefix
                 (not (assoc typekey reftex-typekey-to-prefix-alist)))
            (add-to-list 'reftex-typekey-to-prefix-alist
                         (cons typekey prefix)))
        (cond
         ((string-match "\\`\\\\" env-or-mac)
          ;; It's a macro
          (let ((result (reftex-parse-args env-or-mac)))
            (setq env-or-mac (or (first result) env-or-mac)
		  nargs (second result)
                  nlabel (third result)
                  opt-args (fourth result))
            (if nlabel (add-to-list 'macros-with-labels env-or-mac)))
          (add-to-list 'reftex-label-mac-list env-or-mac))
         (t
          (setq nargs nil nlabel nil opt-args nil)
          (cond ((string= env-or-mac "any"))
                ((string= env-or-mac ""))
                ((string= env-or-mac "section"))
                (t
                 (add-to-list 'reftex-label-env-list env-or-mac)))))
	;; Translate some special context cases
	(when (assq context reftex-default-context-regexps)
	  (setq context 
		(format 
		 (cdr (assq context reftex-default-context-regexps))
		 (regexp-quote env-or-mac))))
        (and reffmt
             (not (assoc typekey reftex-typekey-to-format-alist))
             (push (cons typekey reffmt) reftex-typekey-to-format-alist))
        (and (not (string= env-or-mac "any"))
             (not (string= env-or-mac ""))
             (not (assoc env-or-mac reftex-env-or-mac-alist))
             (push (list env-or-mac typekey context labelfmt
			 nargs nlabel opt-args)
                   reftex-env-or-mac-alist))
	(if (eq (car wordlist) 'regexp)
	    (setq wordlist (cdr wordlist)
		  words-are-re t)
	  (setq words-are-re nil))
        (while (and (setq word (pop wordlist))
                    (stringp word))
          (setq word (identity (if words-are-re word (regexp-quote word))))
          (or (assoc word reftex-words-to-typekey-alist)
              (push (cons word typekey) reftex-words-to-typekey-alist)))
        (cond
         ((string= "" env-or-mac) nil)
         ((setq cell (assoc typekey qh-list))
          (push env-or-mac (cdr cell)))
         (t
          (push (list typekey env-or-mac) qh-list)))))

    (setq qh-list 
	  (sort qh-list (function 
			 (lambda (x1 x2) (string< (car x1) (car x2))))))
    (setq reftex-typekey-to-prefix-alist
          (nreverse reftex-typekey-to-prefix-alist))
    (setq reftex-type-query-prompt
          (concat "Label type: "
                  (mapconcat (function (lambda(x) (format "[%s]" (car x))))
                             qh-list " ")
                  " (?=Help)"))
    (setq reftex-type-query-help
          (concat "SELECT A LABEL TYPE:\n--------------------\n"
                  (mapconcat
		   (function
		    (lambda(x)
		     (setq sum 0)
		     (format " [%s]   %s"
			     (car x)
			     (mapconcat
			      (lambda(x)
				(setq sum (+ sum (length x)))
				(if (< sum 60)
				    x
				  (setq sum 0)
				  (concat "\n       " x)))
			      (cdr x) " "))))
                   qh-list "\n")))

    ;; Convert magic words to regular expressions
    (setq reftex-words-to-typekey-alist
	  (mapcar 
	   (lambda (x)
	     (setq word (car x)
		   typekey (cdr x)
		   fmt (cdr (assoc typekey reftex-typekey-to-format-alist)))
	     (setq word (concat "\\W\\(" word "[ \t\n\r]*\\)\\("))
	     (setq i 0)
	     (while (and (< i 10)
			 (< i (length fmt))
			 (not (member (aref fmt i) '(?%))))
	       (setq word (concat word "\\|" (regexp-quote
					      (substring fmt 0 (1+ i)))))
	       (incf i))
	     (cons (concat word "\\)\\=") typekey))
	   (nreverse reftex-words-to-typekey-alist)))

    ;; Calculate the regular expressions
    (let ((label-re "\\\\label{\\([^}]*\\)}")
          (include-re "\\(\\`\\|[\n\r]\\)[ \t]*\\\\\\(include\\|input\\)[{ \t]+\\([^} \t\n\r]+\\)")
          (section-re
           (concat "\\(\\`\\|[\n\r]\\)[ \t]*\\\\\\("
                  (mapconcat 'car reftex-section-levels "\\|")
                  "\\)\\*?\\(\\[[^]]*\\]\\)?{"))
	  (appendix-re "\\(\\`\\|[\n\r]\\)[ \t]*\\(\\\\appendix\\)")
          (macro-re
           (if macros-with-labels
               (concat "\\("
                       (mapconcat 'regexp-quote macros-with-labels "\\|")
                       "\\)[[{]")
             ""))
          (find-label-re-format
           (concat "\\("
		   (mapconcat 'regexp-quote (append '("\\label")
						    macros-with-labels) "\\|")
                   "\\)\\([[{][^]}]*[]}]\\)*[[{]\\(%s\\)[]}]")))
      (setq reftex-section-regexp section-re
            reftex-section-or-include-regexp
            (concat section-re "\\|" include-re)
            reftex-everything-regexp
            (concat label-re "\\|" section-re "\\|" include-re
		    "\\|" appendix-re
                    (if macros-with-labels "\\|" "") macro-re)
            reftex-find-label-regexp-format find-label-re-format
	    reftex-find-label-regexp-format2 
	    "\\([]} \t\n\r]\\)\\([[{]\\)\\(%s\\)[]}]"))))

;;; Keybindings --------------------------------------------------------------

(define-key reftex-mode-map  "\C-c="     'reftex-toc)
(define-key reftex-mode-map  "\C-c("     'reftex-label)
(define-key reftex-mode-map  "\C-c)"     'reftex-reference)
(define-key reftex-mode-map  "\C-c["     'reftex-citation)
(define-key reftex-mode-map  "\C-c&"     'reftex-view-crossref)

;; Bind `reftex-mouse-view-crossref' only when the key is still free
(if (string-match "XEmacs" emacs-version)
    (unless (key-binding [(shift button2)])
      (define-key reftex-mode-map [(shift button2)] 
	'reftex-mouse-view-crossref))
  (unless (key-binding [(shift mouse-2)])
    (define-key reftex-mode-map [(shift mouse-2)] 
      'reftex-mouse-view-crossref)))

;; If the user requests so, she can have a few more bindings:
(cond
 (reftex-extra-bindings
  (define-key reftex-mode-map  "\C-ct"     'reftex-toc)
  (define-key reftex-mode-map  "\C-cl"     'reftex-label)
  (define-key reftex-mode-map  "\C-cr"     'reftex-reference)
  (define-key reftex-mode-map  "\C-cc"     'reftex-citation)
  (define-key reftex-mode-map  "\C-cv"     'reftex-view-crossref)
  (define-key reftex-mode-map  "\C-cg"     'reftex-grep-document)
  (define-key reftex-mode-map  "\C-cs"     'reftex-search-document)))

;; Selection Keymaps
(let ((map (make-sparse-keymap)))
  ; First, the common stuff
  (substitute-key-definition
   'next-line 'reftex-select-next		       map global-map)
  (substitute-key-definition
   'previous-line 'reftex-select-previous	       map global-map)
  (substitute-key-definition
   'keyboard-quit 'reftex-select-keyboard-quit         map global-map)
  (substitute-key-definition
   'newline 'reftex-select-accept		       map global-map)
  (define-key map " "        'reftex-select-callback)
  (define-key map "n"        'reftex-select-next)
  (define-key map [(down)]   'reftex-select-next)
  (define-key map "p"        'reftex-select-previous)
  (define-key map [(up)]     'reftex-select-previous)
  (define-key map "f"        'reftex-select-toggle-follow)
  (define-key map "\C-m"     'reftex-select-accept)
  (define-key map [(return)] 'reftex-select-accept) 
  (define-key map "q"        'reftex-select-quit)
  (define-key map "."        'reftex-select-show-insertion-point)
  (define-key map "?"        'reftex-select-help)
  (if (string-match "XEmacs" emacs-version)
      (define-key map [(button2)] 'reftex-select-mouse-accept) ; XEmacs
    (define-key map [(mouse-2)] 'reftex-select-mouse-accept))  ; Emacs

  (loop for key across "0123456789" do
	(define-key map (vector (list key)) 'digit-argument))
  (define-key map "-" 'negative-argument)

  (setq reftex-select-label-map map)
  (setq reftex-select-bib-map (copy-keymap map))

  ;; Now the rest

  (loop for key across "cgilrRstx#%" do
	(define-key reftex-select-label-map (vector (list key))
	  (list 'lambda '() '(interactive) (list 'throw '(quote myexit) key))))
  (define-key reftex-select-label-map "b"     'reftex-select-jump-to-previous)
  (define-key reftex-select-label-map "v"     'reftex-select-toggle-varioref)
  (define-key reftex-select-label-map [(tab)] 'reftex-select-read-label)
  (define-key reftex-select-label-map "\C-i"  'reftex-select-read-label)
  (define-key reftex-select-label-map "\C-c\C-n" 'reftex-select-next-heading)
  (define-key reftex-select-label-map "\C-c\C-p" 'reftex-select-previous-heading)

  (loop for key across "grRa" do
	(define-key reftex-select-bib-map (vector (list key))
	  (list 'lambda '() '(interactive) (list 'throw '(quote myexit) key))))
  (define-key reftex-select-bib-map "\C-i" 'reftex-select-read-cite)
  (define-key reftex-select-bib-map [(tab)] 'reftex-select-read-cite))

;; Table of Contents map
(let ((map reftex-toc-map))
  (if (string-match "XEmacs" emacs-version)
      (define-key map  [(button2)] 'reftex-toc-mouse-goto-line-and-hide);XEmacs
    (define-key map  [(mouse-2)] 'reftex-toc-mouse-goto-line-and-hide)) ;Emacs
  (define-key map  "n"         'next-line)
  (define-key map  "p"         'previous-line)
  (define-key map  "?"         'reftex-toc-show-help)
  (define-key map  " "         'reftex-toc-view-line)
  (define-key map  "\C-m"      'reftex-toc-goto-line-and-hide)
  (define-key map  "\C-i"      'reftex-toc-goto-line)
  (define-key map  "r"         'reftex-toc-rescan)
  (define-key map  "R"         'reftex-toc-Rescan)
  (define-key map  "g"         'revert-buffer)
  (define-key map  "q"         'reftex-toc-quit)
  (define-key map  "Q"         'reftex-toc-quit-and-kill)
  (define-key map  "f"         'reftex-toc-toggle-follow)
  (define-key map  "x"         'reftex-toc-external)
  (loop for key across "0123456789" do
	(define-key map (vector (list key)) 'digit-argument))
  (define-key map "-" 'negative-argument))
 
;;; Menus --------------------------------------------------------------------

;; Define a menu for the menu bar if Emacs is running under X

(require 'easymenu)

(easy-menu-define
 reftex-mode-menu reftex-mode-map
 "Menu used in RefTeX mode"
 `("Ref"
   ["Table of Contents"       reftex-toc t]
   "----"
   ["\\label"                 reftex-label t]
   ["\\ref"                   reftex-reference t]
   ["\\cite"                  reftex-citation t]
   ["View Crossref"           reftex-view-crossref t]
   "----"
   ("Parse Document"
    ["Only this File"         reftex-parse-one t]
    ["Entire Document"        reftex-parse-all (reftex-is-multi)]
    ["Save to File"           (reftex-access-parse-file 'write)
     (> (length (symbol-value reftex-docstruct-symbol)) 0)]
    ["Restore from File"      (reftex-access-parse-file 'restore) t]
    "----"
    ["Enable Partial Scans" 
     (setq reftex-enable-partial-scans (not reftex-enable-partial-scans))
     :style toggle :selected reftex-enable-partial-scans]
    ["Auto-Save Parse Info"
     (setq reftex-save-parse-info (not reftex-save-parse-info))
     :style toggle :selected reftex-save-parse-info]
    "---"
    ["Reset RefTeX Mode"       reftex-reset-mode t])
   ("Global Actions"
    ["Search Whole Document"  reftex-search-document t]
    ["Replace in Document"    reftex-query-replace-document t]
    ["Grep on Document"       reftex-grep-document t]
    "----"
    ["Create TAGS File"       reftex-create-tags-file t]
    "----"
    ["Find Duplicate Labels"  reftex-find-duplicate-labels t]
    ["Change Label and Refs"  reftex-change-label t]
    ;;;["Renumber Simple Labels" reftex-renumber-simple-labels t]
    "---"
    ["Save document"          reftex-save-all-document-buffers t])
   "---"
   ("Reference Options"
    "Varioref Setting"
    ["Use `\\vref' by Default" 
     (setq reftex-vref-is-default (not reftex-vref-is-default))
     :style toggle :selected reftex-vref-is-default]
    "---"
    "Selection Buffers"
    ["Use Multiple Buffers"
     (setq reftex-use-multiple-selection-buffers
	   (not reftex-use-multiple-selection-buffers))
     :style toggle :selected reftex-use-multiple-selection-buffers]
    ["Auto Update Buffers"
     (setq reftex-auto-update-selection-buffers
	   (not reftex-auto-update-selection-buffers))
     :style toggle :selected reftex-auto-update-selection-buffers])
   ("Citation Options"
    "Citation Style"
    ,@(mapcar
       (function
	(lambda (x)
	  (vector
	   (capitalize (symbol-name (car x)))
	   (list 'setq 'reftex-cite-format (list 'quote (car x)))
	   ':style 'radio ':selected
	   (list 'eq 'reftex-cite-format (list 'quote (car x))))))
	reftex-cite-format-builtin)
    "----"
    "Bibinfo in Comments"
    ["Attach Comments"
     (setq reftex-comment-citations (not reftex-comment-citations))
     :style toggle :selected reftex-comment-citations]
    "---"
    "Sort Database Matches"
    ["by Author" (setq reftex-sort-bibtex-matches 'author)
     :style radio :selected (eq reftex-sort-bibtex-matches 'author)]
    ["by Year" (setq reftex-sort-bibtex-matches 'year)
     :style radio :selected (eq reftex-sort-bibtex-matches 'year)]
    ["by Year, reversed" (setq reftex-sort-bibtex-matches 'reverse-year)
     :style radio :selected (eq reftex-sort-bibtex-matches 'reverse-year)]
    ["Not" (setq reftex-sort-bibtex-matches nil)
     :style radio :selected (eq reftex-sort-bibtex-matches nil)])
   ("Customize"
    ["Browse RefTeX group" reftex-customize t]
    "---"
    ["Build Full Customize Menu" reftex-create-customize-menu 
     (fboundp 'customize-menu-create)])
   "----"
   ("Documentation"
    ["Info" reftex-info t]
    ["Commentary" reftex-show-commentary t])))

;;; Run Hook ------------------------------------------------------------------

(run-hooks 'reftex-load-hook)

;;; That's it! ----------------------------------------------------------------

(setq reftex-tables-dirty t)  ; in case this file is evaluated by hand
(provide 'reftex) 

;;;============================================================================

;;; reftex.el ends here
