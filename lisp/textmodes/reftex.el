;; reftex.el --- Minor mode for doing \label, \ref and \cite in LaTeX

;; Copyright (c) 1997 Free Software Foundation, Inc.

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
;; efficient database lookup.
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
;; For key bindings, see further down in this documentation.
;;
;;---------------------------------------------------------------------------
;;
;; OVERVIEW
;; 
;; 1. USING \label AND \ref.  Labels and references are one of the
;;    strong points of LaTeX.  But, in documents with hundreds of
;;    equations, figures, tables etc. it becomes quickly impossible to
;;    find good label names and to actually remember them.  Then, also
;;    completion of labels in not enough.  One actually needs to see the
;;    context of the label definition to find the right one.
;; 
;;    - RefTeX distinguishes labels for different environments.  It
;;      always knows if a certain label references a figure, table
;;      etc..  You can configure RefTeX to recognize any additional
;;      labeled environments you might have defined yourself.
;;
;;    - RefTeX defines automatically unique labels.  Type `C-c ('
;;      (reftex-label) to insert a label at point.  RefTeX will either
;;        - derive a label from context (default for section labels)
;;        - insert a simple label consisting of a prefix and a number
;;          (default for equations and enumerate items) or
;;        - prompt for a label string (figures and tables).
;;      Which labels are created how can be controlled with the variable
;;      `reftex-insert-label-flags'.
;;
;;    - Referencing labels is a snap and I promise you'll love it.
;;      In order to make a reference, type `C-c )' (`reftex-reference').
;;      This shows an outline of the document with all labels of a
;;      certain type (figure, equation,...) and context of the label
;;      definition.  Selecting one of the labels inserts a \ref macro
;;      into the original buffer.  Online help during the selection is
;;      available with `?'.
;; 
;; 2. CITATIONS.  After typing `C-c [' (`reftex-citation'), RefTeX will
;;    let you specify a regexp to search in current BibTeX database files
;;    (as specified in the \bibliography command) and pull out a formatted
;;    list of matches for you to choose from.  The list is *formatted* and
;;    thus much easier to read than the raw database entries.  It can also
;;    be sorted.  The text inserted into the buffer is by default just
;;    `\cite{KEY}', but can also contain author names and the year in a
;;    configurable way.  See documentation of the variable
;;    `reftex-cite-format'.
;; 
;; 3. TABLE OF CONTENTS.  Typing `C-c =' (`reftex-toc') will show a table
;;    of contents of the document.  From that buffer, you can jump quickly
;;    to every part of your document.  This is similar to imenu, only it
;;    works for entire multifile documents and uses the keyboard rather
;;    than the mouse.  The initial version of this function was contributed
;;    by Stephen Eglen.
;;
;; 4. MULTIFILE DOCUMENTS are fully supported by RefTeX.  Such documents
;;    consist of a master file and many other files being included via
;;    \input or \include.  (La)TeX source files included from another file
;;    should specify the name of the top level master file with a `file
;;    variable' (see the Emacs documentation on file variables for details:
;;    [Emacs/Customization/Variables/File Variables]).  With AUCTeX, this
;;    variable is called `TeX-master'.  The Emacs (La)TeX modes use
;;    `tex-main-file' instead.  RefTeX will check for both.  Check the
;;    documentation of your (La)TeX modes on how to specify the master
;;    file.
;;    RefTeX knows about all files related to a document via \input and
;;    \include.  It provides functions to run regular expression searches
;;    and replaces over the entire document and to create a TAGS file.
;;
;; 5. DOCUMENT PARSING.  RefTeX needs to parse the document in order to find
;;    labels and other information.  It will do it automatically once, when
;;    you start working with a document.  If you need to enforce reparsing
;;    later, call any of the functions `reftex-citation', `reftex-label',
;;    `reftex-reference', `reftex-toc' with a raw C-u prefix.
;;
;;-------------------------------------------------------------------------
;; 
;; CONFIGURATION
;; 
;; RefTeX contains configurable options which change the way it works.
;;
;; Most importantly, RefTeX needs to be configured if you use labels to
;; mark non-standard environments.  RefTeX always understands LaTeX section
;; commands and the following environments: figure, figure*,
;; sidewaysfigure, table, table*, sidewaystable, equation, eqnarray,
;; enumerate.  For everythings else, it needs to be configured.
;;
;; A good way to configure RefTeX is with the custom.el package by Per
;; Abrahamsen, shipped with Emacs 20 and XEmacs 19.15.  To do this, just
;; say `M-x reftex-customize'.
;;
;; Here is a complete list of the RefTeX configuration variables with
;; their default settings.  You could copy part of this list to your
;; .emacs file and change whatever is necessary.  Each variable has an
;; extensive documentation string.  Look it up for more information!
;;
;;   ;; Configuration Variables and User Options for RefTeX ------------------
;;   ;; Support for \label and \ref ------------------------------------------
;;        (setq reftex-label-alist nil)
;;        (setq reftex-default-label-alist-entries '(Sideways LaTeX))
;;        (setq reftex-use-text-after-label-as-context nil)
;;   ;; Label insertion
;;        (setq reftex-insert-label-flags '("s" "sft"))
;;        (setq reftex-derive-label-parameters '(3 20 t 1 "-"
;;                 ("the" "on" "in" "off" "a" "for" "by" "of" "and" "is")))
;;        (setq reftex-label-illegal-re "[\000-\040\177-\377\\\\#$%&~^_{}]")
;;        (setq reftex-abbrev-parameters '(4 2 "^saeiou" "aeiou"))
;;   ;; Label referencing
;;        (setq reftex-label-menu-flags '(t t nil nil nil nil t))
;;        (setq reftex-guess-label-type t)
;;   ;; BibteX citation configuration ----------------------------------------
;;        (setq reftex-bibpath-environment-variables '("BIBINPUTS" "TEXBIB"))
;;        (setq reftex-bibfile-ignore-list nil)
;;        (setq reftex-sort-bibtex-matches 'reverse-year)
;;        (setq reftex-cite-format 'default)
;;        (setq reftex-comment-citations nil)
;;        (setq reftex-cite-comment-format
;;          "%% %2a %y, %j %v, %P, %e: %b, %u, %s %<\n")
;;        (setq reftex-cite-punctuation '(", " " \\& " " {\\it et al.}"))
;;   ;; Table of contents configuration --------------------------------------
;;        (setq reftex-toc-follow-mode nil)
;;   ;; Miscellaneous configurations -----------------------------------------
;;        (setq reftex-extra-bindings nil)
;;        (setq reftex-plug-into-AUCTeX nil)
;;        (setq reftex-use-fonts t)
;;        (setq reftex-keep-temporary-buffers t)
;;        (setq reftex-auto-show-entry t)
;;
;; CONFIGURATION EXAMPLES:
;; =======================
;;
;;  Suppose you are working with AMS-LaTeX amsmath package (with its math
;;  environments like `align', `multline' etc.).  Here is how you would
;;  configure RefTeX to recognize these environments:
;;
;;  (setq reftex-label-alist '(AMSTeX))
;;
;;  This is very easy since RefTeX has builtin support for AMS-LaTeX.
;;  Suppose, however, you are also
;;
;;  -  using "\newtheorem" in LaTeX in order to define two new environments
;;     "Theorem" and "Axiom" like this:
;;
;;       \newtheorem{axiom}{Axiom}
;;       \newtheorem{theorem}{Theorem}
;;
;;  -  making your figures not directly with the figure environment, but with
;;     a macro like
;;
;;         \newcommand{\myfig}[4][tbp]{
;;           \begin{figure}[#1]
;;           \epsimp[#4]{#2}
;;           \caption{#3}
;;           \end{figure}}
;;
;;     which would be called like
;;
;;         \myfig{filename}{\label{fig:13} caption text}{1}
;;
;; Here is how to tell RefTeX to also recognize Theorem and Axiom as
;; labeled environments, and that any labels defined inside the \myfig
;; macro are figure labels:
;;
;;    (setq reftex-label-alist 
;;       '(AMSTeX
;;	   ("axiom"   ?a "ax:"  "~\\ref{%s}" nil ("Axiom"   "Ax."))
;;	   ("theorem" ?h "thr:" "~\\ref{%s}" t   ("Theorem" "Theor." "Th."))
;;	   ("\\myfig" ?f "fig:" nil          2)))
;;
;; The type indicator characters ?a and ?h are used for prompts when
;; RefTeX queries for a label type.  Note that "h" was chosen for "theorem"
;; since "t" is already taken by "table".  Note that also "s", "f", "e", "n"
;; are taken by the standard environments.
;; The automatic labels for Axioms and Theorems will look like "ax:23" or
;; "thr:24".
;; The "\ref{%s}" is a format string indicating how to insert references to
;; these labels.  The nil format in the \myfig entry means to use the same
;; format as other figure labels. 
;; The next item indicates how to grab context of the label definition. 
;; - t means to get it from a default location (from the beginning of a \macro
;;   or after the \begin statement).  t is *not* a good choice for eqnarray
;;   and similar environments.
;; - nil means to use the text right after the label definition.
;; - 2 means grab the 2nd macro argument: \myfig{xxx}{THIS IS CONTEXT}{1}
;; - For more complex ways of getting context, see the docstring of
;;   `reftex-label-alist'.
;; The strings at the end of each entry are used to guess the correct label
;; type from the word before point when creating a reference.  E.g. if you
;; write: "As we have shown in Theorem" and then press `C-)', RefTeX will
;; know that you are looking for a Theorem label and restrict the labels in 
;; the menu to only these labels without even asking.
;; See also the documentation string of the variable `reftex-label-alist'.
;;
;; Depending on how you would like the label insertion and selection for the
;; new environments to work, you might want to add the letters "a" and "h"
;; to some of the flags in the following variables:
;;
;;    reftex-insert-label-flags
;;    reftex-label-menu-flags
;;
;; The individual flags in these variables can be set to t or nil to enable or
;; disable the feature for all label types.  They may also contain a string of
;; label type letters in order to turn on the feature for those types only.
;;
;; -----
;; If you are writing in a language different from English you might want to
;; add magic words for that language.  Here is a German example:
;;
;;   (setq reftex-label-alist 
;;      '((nil ?s nil nil nil ("Kapitel" "Kap." "Abschnitt" "Teil"))
;;        (nil ?e nil nil nil ("Gleichung" "Gl."))
;;        (nil ?t nil nil nil ("Tabelle"))
;;        (nil ?f nil nil nil ("Figur" "Abbildung" "Abb."))
;;        (nil ?n nil nil nil ("Punkt"))))
;;
;; Using nil as first item in each entry makes sure that this entry does
;; not replace the original entry for that label type, but just adds magic
;; words.
;;
;; -----
;;
;; By default, citations are inserted simply as \cite{KEY}.  You can have
;; more complex citation commands with many available packages, most
;; notably the harvard and natbib packages.  RefTeX can be configured to
;; support these and other styles by setting the variable reftex-cite-format. 
;; E.g., for the natbib package you would use
;;  
;;   (setq reftex-cite-format 'natbib)
;;
;; This can also be done as a file variable.  For the full list of builtin
;; options, try `M-x customize-variable RET reftex-cite-format RET'.
;;
;; HOOKS
;; -----
;; - Loading reftex.el runs the hook `reftex-load-hook'.
;; - Turning on reftex-mode runs `reftex-mode-hook'.
;; - Creating the master buffer runs `reftex-make-master-buffer-hook'
;;   on the master buffer just before parsing starts.
;;
;;-------------------------------------------------------------------------
;;
;; KEY BINDINGS
;;
;; All important functions of RefTeX can be reached from its menu which
;; is installed in the menu bar as "Ref" menu.  Only the more frequently used
;; functions have key bindings.
;;
;; Here is the default set of keybindings from RefTeX.
;;
;;   C-c =     reftex-toc
;;   C-c (     reftex-label
;;   C-c )     reftex-reference
;;   C-c [     reftex-citation
;;   C-c &     reftex-view-crossref
;;
;; I've used these bindings in order to avoid interfering with AUCTeX's
;; settings.  Personally, I also bind some functions in the C-c LETTER
;; map for easier access:
;;
;;   C-c t     reftex-toc
;;   C-c l     reftex-label
;;   C-c r     reftex-reference
;;   C-c c     reftex-citation
;;   C-c v     reftex-view-crossref
;;   C-c s     reftex-search-document
;;   C-c g     reftex-grep-document
;;
;; If you want to copy those as well, set in your .emacs file:
;; 
;; (setq reftex-extra-bindings t)
;;
;; It is possible to bind the function for viewing cross references to a
;; mouse event.  Something like the following in .emacs will do the trick:
;;
;; (add-hook 'reftex-load-hook 
;;    '(lambda ()
;;       (define-key reftex-mode-map [(alt mouse-1)]
;;         'reftex-mouse-view-crossref)))
;; 
;;-------------------------------------------------------------------------
;;
;; RELATED PACKAGES
;;
;; AUCTeX
;; ------
;; If you are writing TeX or LaTeX documents with Emacs, you should have
;; a look at AUCTeX, the definitive package to work with TeX and LaTeX.
;; Information on AUCTeX can be found here:
;;
;;   http://www.sunsite.auc.dk/auctex/
;;
;; Instead of using the RefTeX functions described above directly, you
;; can also use them indirectly through AUCTeX (>9.7p).  RefTeX provides
;; several interface functions which can be used as replacement for
;; corresponding AUCTeX functions dealing with labels and citations.
;; In this way you can work normally with AUCTeX and use RefTeX
;; internals to create and complete labels and citation keys.
;;
;; `reftex-label' can be used as the `LaTeX-label-function' which does
;; label insertion when new environments are created with C-c C-e.
;;
;; `reftex-arg-label', `reftex-arg-ref' and `reftex-arg-cite' can replace
;; the corresponding `TeX-arg-...' functions.  E.g. when you insert a
;; label macro with `C-c RET label RET', RefTeX will be transparently used
;; to create the label.
;;
;; In order to plug all 4 functions into AUCTeX, use in .emacs:
;;
;;    (setq reftex-plug-into-AUCTeX t)
;;
;; You may also choose to plug in only some of these functions.  See
;; the doc string of `reftex-plug-into-AUCTeX'.
;;
;; AUCTeX can support RefTeX via style files.  A style file may contain
;; calls to `reftex-add-to-label-alist' which defines additions to
;; `reftex-label-alist'.  The argument taken by this function must have
;; the same format as `reftex-label-alist'.  The `amsmath.el' style file
;; of AUCTeX (>9.7p) for example contains the following:
;;
;;    (TeX-add-style-hook "amsmath"
;;     (function
;;      (lambda ()
;;        (if (featurep 'reftex)
;;              (reftex-add-to-label-alist '(AMSTeX))))))
;;
;; while a package `myprop' defining a proposition environment with
;; \newtheorem might use
;;
;;    (TeX-add-style-hook "myprop"
;;     (function
;;      (lambda ()
;;        (if (featurep 'reftex)
;;            (reftex-add-to-label-alist
;;             '(("proposition" ?p "prop:"  "~\\ref{%s}" t 
;;                              ("Proposition" "Prop."))))))))
;;
;; Bib-cite.el
;; -----------
;; Once you have written a document with labels, refs and citations,
;; it can be nice to read such a file like a hypertext document.
;; RefTeX has some support for that (`reftex-view-crossref',
;; `reftex-search-document').  A more elegant interface with mouse
;; support and links into Hyperbole is provided (among other things)
;; by Peter S. Galbraith's `bib-cite.el'.  There is some overlap in the
;; functionalities of Bib-cite and RefTeX.  Bib-cite.el comes bundled
;; with AUCTeX.  You can also get the latest version from
;;
;;  ftp://ftp.phys.ocean.dal.ca/users/rhogee/elisp/bib-cite.el
;;
;;-------------------------------------------------------------------------
;;
;; PERFORMANCE ISSUES
;;
;; 1. RefTeX will load other parts of a multifile document as well as BibTeX
;;    database files for lookup purposes.  These buffers are kept, so that
;;    subsequent lookup in the same files is fast.  For large documents and
;;    large BibTeX databases, this can use up a lot of memory.  If you have
;;    more time than memory, try the following option, which will remove
;;    buffers created for lookup after use.
;;
;;       (setq reftex-keep-temporary-buffers nil)
;;
;; 2. Parsing the document for labels and their context can be slow.
;;    Therefore, RefTeX does it just once automatically.  Further parsing
;;    happens only on user request
;;     - with a raw C-u prefix arg to any of the functions `reftex-label',
;;       `reftex-reference', `reftex-citation', `reftex-toc'.
;;     - with the `r' key from the label selection menu or the *toc* buffer.
;;
;;    *** If you use `reftex-label' to create labels, the list will be
;;    *** updated internally, so that no extra parsing is required.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; KNOWN BUGS AND WORK-AROUNDS
;;
;; o If you change `reftex-label-alist' in an editing session, you need to
;;   reset reftex with `M-x reftex-reset-mode' in order to make these
;;   changes effective.  Changes introduced with the function
;;   `reftex-add-to-label-alist' as well as changes applied from the
;;   customization buffer automatically trigger a reset.
;;
;; o At times the short context shown by RefTeX may not be what you want.
;;   In particular, eqnarray environments can be difficult to parse.
;;   RefTeX's default behavior for eqnarrays is to scan backwards to either
;;   a double backslash or the beginning of the environment.  If this gives
;;   unsatisfactory results, make it a habit to place the label *before*
;;   each equation
;;  
;;     \begin{eqnarray}
;;       \label{eq:1}
;;       E = \gamma m c^2 \\
;;       \label{eq:2}
;;       \gamma = \sqrt{1-v^2/c^2}
;;     \end{eqnarray}
;;  
;;   and turn off parsing for context in equation and eqnarray environments
;;   with
;;  
;;      (setq reftex-use-text-after-label-as-context "e").
;;
;; o RefTeX keeps only one global copy of the configuration variables.
;;   Also, any additions from style files go into a global variable.
;;   Practically, this should not be a problem.  Theoretically, it could
;;   give conflicts if two documents used environments with identical
;;   names, but different associated label types.
;;  
;; o Input, include, bibliography and section statements have to be first
;;   on a line (except for white space) in order to be seen by RefTeX.
;;  
;; o When the document is scanned, RefTeX creates a large "master" buffer
;;   containing the entire document instead of scanning the individual
;;   files one by one.  I hope to change this eventually.
;;
;; o Creating the master buffer bypasses `find-file-hook' as well as
;;   TeX/LaTeX mode hooks for efficiency reasons.  If any of the relevant
;;   hooks contains functions filtering (La)TeX files (e.g. isotex), these
;;   functions should go into `reftex-make-master-buffer-hook' as well.
;;
;; o If you have two identical section headings in the same file,
;;   `reftex-toc' will only let you jump to the first one because it searches
;;   for the section heading from the beginning of the file.  You can work
;;   around this by changing one of the section titles in a way LaTeX does
;;   not see, e.g. with extra white space.  RefTeX will distinguish
;;   \section{Introduction} from \section{ Introduction}.
;;
;; o RefTeX needs a \label macro to identify a label during parsing.  Some
;;   packages, however, define environments and macros which require a naked
;;   label as argument.  This is currently not possible with RefTeX.  It
;;   will be in version 3.00.
;;
;; o RefTeX sees also labels in regions commented out and will refuse to
;;   make duplicates of such a label.  This is considered to be a feature.
;;  
;; o When RefTeX tries to show a window full of context from inside a
;;   section hidden with `outline-minor-mode', it will unhide that section.
;;   This change will not be reversed automatically.
;;
;;---------------------------------------------------------------------------
;;
;; TO DO
;;
;; Rewriting the parser so that it can parse the file without making a
;; master buffer, save parsing information and reparse only parts of
;; the document.  This will make RefTeX better suited for very large
;; projects.
;;
;; Deal with naked labels.
;;
;;---------------------------------------------------------------------------
;;
;; AUTHOR
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
;; THANKS TO:
;; ---------
;; At least the following people have invested time to test and bug-fix
;; reftex.el.  Some have send patches for fixes or new features.
;;
;; Stephen Eglen   <stephene@cogs.susx.ac.uk>
;; F.E.Burstall    <F.E.Burstall@maths.bath.ac.uk>
;; Karl Eichwalder <ke@ke.Central.DE>
;; Laurent Mugnier <mugnier@onera.fr>
;; Rory Molinari   <molinari@yunt.math.lsa.umich.edu>
;; Soren Dayton    <csdayton@cs.uchicago.edu>
;; Daniel Polani   <polani@Informatik.Uni-Mainz.DE>
;; Allan Strand    <astrand@trillium.NMSU.Edu>
;; Adrian Lanz     <lanz@waho.ethz.ch>
;; Jan Vroonhof    <vroonhof@math.ethz.ch>
;;
;; The view crossref feature was inspired by the similar function in 
;; Peter S. Galbraith's bib-cite.el.
;; 
;; Finally thanks to Uwe Bolick <bolick@physik.tu-berlin.de> who first
;; got me (some years ago) into supporting LaTeX labels and references
;; with an Editor (which was MicroEmacs at the time).
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

;;; Code:

;; Stuff that needs to be there when we use defcustom
;; --------------------------------------------------

(require 'custom)

(defvar reftex-tables-dirty t
  "Flag showing if tables need to be re-computed.")

(eval-and-compile
  (defun reftex-set-dirty (symbol value)
    (setq reftex-tables-dirty t)
    (set symbol value)))

;;; Begin of Configuration Section ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Configuration Variables and User Options for RefTeX ------------------

(defgroup reftex nil
  "LaTeX label and citation support."
  :tag "RefTeX"
  :link '(url-link :tag "Home Page" "http://strw.leidenuniv.nl/~dominik/Tools/")
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
   (t
    (customize 'reftex))))

(defun reftex-show-commentary ()
  "Use the finder to view the file documentation from `reftex.el'."
  (interactive)
  (require 'finder)
  (finder-commentary "reftex.el"))

;; Support for \label and \ref --------------------------------------

(defgroup reftex-label-support nil
  "Support for creation, insertion and referencing of labels in LaTeX."
  :group 'reftex)

(defgroup reftex-defining-label-environments nil
  "Definition of environments and macros to do with label."
  :group 'reftex-label-support)

(defcustom reftex-label-alist nil
  "Alist with information on environments for \\label-\\ref use.
See the definition of `reftex-label-alist-builtin' for examples.  This variable
should define additions and changes to the default.  The only things you MUST
NOT change is that `?s' is the type indicator for section labels and SPACE is
for the `any' label type.  These are hard-coded at other places in the code.

Changes to this variable after RefTeX has been loaded become only
effective when RefTeX is reset with \\[reftex-reset-mode].

Each list entry is a list describing an environment or macro carrying a
label.  The elements of each list entry are:

0.  Name of the environment (like \"table\") or macro (like \"\\\\myfig\").
    Special names: `section' for section labels, `any' to define a group
    which contains all labels.
    This may also be nil if this entry is only meant to change some settings
    associated with the type indicator character (see below).

1.  Type indicator character, like `?t'.
    The type indicator is a single character used in prompts for
    label types.  It must be a printable character.  The same character
    may occur several times in this list, to cover cases in which different
    environments carry the same label type (like equation and eqnarray).

2.  Label prefix string, like \"tab:\".
    The prefix is a short string used as the start of a label.  It may be the
    empty string.  If `nil', a prefix will be constructed from the typekey.
    The prefix may contain the following `%' escapes:
       %f   Current file name with directory and extension stripped.
       %F   Current file name relative to directory of master file.
       %u   User login name, on systems which support this.

    Example: In a file `intro.tex', \"eq:%f:\" will become \"eq:intro:\").

3.  Format string for reference insert in buffer.  `%s' will be replaced by
    the label.
    When the format starts with `~', whitespace before point will be removed
    so that the reference cannot be separated from the word before it.

4.  Indication on how to find the short context.
    - If nil, use the text following the \\label{...} macro.
    - If t, use
       - text following the \\begin{...} statement of environments
         (not a good choice in in eqnarray or enumerate environments!)
       - the section heading for section labels.
       - the begin of the macro for macros.
    - If an integer, use the nth argument of the macro.  If there are less
      than n arguments (parenthesis pairs), use text after the last one.
    - If a string, use as regexp to search *backward* from the label.  Context
      is then the text following the end of the match.  E.g. putting this to
      \"\\\\\\\\caption{\" will use the beginning of the caption in a figure
      or table environment. 
      \"\\\\\\\\begin{eqnarray}\\\\|\\\\\\\\\\\\\\\\\" works for eqnarrays.
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

If the type indicator characters of two or more entries are the same, RefTeX
will use
 - the first non-nil format and prefix
 - the magic words of all involved entries.

Any list entry may also be a symbol.  If that has an association in
`reftex-label-alist-builtin', the cdr of that association is spliced into the
list.  See the AMSTeX configuration example in the comment section of
`reftex.el'."
  :group 'reftex-defining-label-environments
  :set 'reftex-set-dirty
  :type
  '(repeat
    :inline t
    (radio
     :value ("" ?a nil nil t nil)
     (choice 
      :tag "Builtin"
      :value AMSTeX
      ;; Update here with: update-builtin-label-1
      (const :tag "Sideways : Alias for -->rotating" Sideways)
      (const :tag "AMSTeX   : Alias for -->amsmath" AMSTeX)
      (const :tag "amsmath  : AMS math environments" amsmath)
      (const :tag "longtable: The longtable environment" longtable)
      (const :tag "rotating : Sidewaysfigure and table" rotating)
      (const :tag "subfigure: Subfigure environments/macro" subfigure)
      (const :tag "LaTeX    : LaTeX default environments" LaTeX))
     (list  :tag "Detailed custom entry"
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
		     (const  :tag "Default position" t)
		     (const  :tag "After label"      nil)
		     (number :tag "Macro arg nr" 1)
		     (regexp :tag "Regexp" "")
		     (symbol :tag "Function" my-func))
		    (cons :tag "2 different methods"
		     (choice    
		      :tag "  Display context     "
		      (const  :tag "Default position" t)
		      (const  :tag "After label"      nil)
		      (number :tag "Macro arg nr" 1)
		      (regexp :tag "Regexp" "")
		      (symbol :tag "Function" my-func))
		     (choice    
		      :tag "  Derive label context"
		      (const  :tag "Default position" t)
		      (const  :tag "After label"      nil)
		      (number :tag "Macro arg nr" 1)
		      (regexp :tag "Regexp" "")
		      (symbol :tag "Function" my-func))))
	    (repeat    :tag "List of Magic Words" (string))))))

(defcustom reftex-default-label-alist-entries '(Sideways LaTeX)
  "Default label alist specifications.  LaTeX should be the last entry.
This list describes the default label environments RefTeX should always use
in addition to the specifications in reftex-label-alist.  It is probably a
mistake to remove the LaTeX symbol from this list.

The options include:
LaTeX     The standard LaTeX environments
Sideways  The sidewaysfigure and sidewaystable environments
AMSTeX    The math environments in the AMS_LaTeX amsmath package

For the full list of options, try

M-x customize-variable RET reftex-default-label-alist-entries RET."
  :group 'reftex-defining-label-environments
  :set   'reftex-set-dirty
  :type '(set
	  :indent 4
	  :inline t
	  :greedy t
	  ;; Update here with: update-builtin-label-2
	  (const :tag "Sideways : Alias for -->rotating" Sideways)
	  (const :tag "AMSTeX   : Alias for -->amsmath" AMSTeX)
	  (const :tag "amsmath  : AMS math environments" amsmath)
	  (const :tag "longtable: The longtable environment" longtable)
	  (const :tag "rotating : Sidewaysfigure and table" rotating)
	  (const :tag "subfigure: Subfigure environments/macro" subfigure)
	  (const :tag "LaTeX    : LaTeX default environments" LaTeX))
  )

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
indicating the label types for which it should be true.
Thus, the combination may be set differently for each label type.  The
default settings \"s\" and \"sft\" mean: Derive section labels from headings
(with confirmation).  Prompt for figure and table labels.  Use simple labels
without confirmation for everything else."
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

(defcustom reftex-derive-label-parameters '(3 20 t 1 "-"             ; continue
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

;; Label referencing

(defgroup reftex-referencing-labels nil
  "Options on how to reference labels."
  :group 'reftex-label-support)

(defcustom reftex-label-menu-flags '(t t nil nil nil nil t)
  "*List of flags governing the label menu makeup.
The flags are:

TABLE-OF-CONTENTS  Show the labels embedded in a table of context.
SECTION-NUMBERS    Include section numbers (like 4.1.3) in table of contents.
COUNTERS           Show counters.  This just numbers the labels in the menu.
NO-CONTEXT         Non-nil means do NOT show the short context.
FOLLOW             Follow full context in other window.
SHOW-COMMENTED     Show labels from regions which are commented out.
MATCH-IN-TOC       Searches in label menu will also match in toc lines.

Each of these flags can be set to t or nil, or to a string of type letters
indicating the label types for which it should be true.  These strings work
like character classes in regular expressions.  Thus, setting one of the
flags to \"sf\" makes the flag true for section and figure labels, nil
for everything else.  Setting it to \"^ft\" makes it the other way round.

Most options can also be switched from the label menu itself - so if you
decide here to not have a table of contents in the label menu, you can still
get one interactively during selection from the label menu."
  :group 'reftex-referencing-labels
  :type '(list
	  (choice :tag "Embed in table of contents    "
		  (const :tag "on" t) (const :tag "off" nil)
		  (string :tag "Selected label types"))
	  (choice :tag "Show section numbers          "
		  (const :tag "on" t) (const :tag "off" nil))
	  (choice :tag "Show individual counters      " 
		  (const :tag "on" t) (const :tag "off" nil)
		  (string :tag "Selected label types"))
	  (choice :tag "Hide short context            "
		  (const :tag "on" t) (const :tag "off" nil)
		  (string :tag "Selected label types"))
	  (choice :tag "Follow context in other window"
		  (const :tag "on" t) (const :tag "off" nil)
		  (string :tag "Selected label types"))
	  (choice :tag "Show commented labels         "
		  (const :tag "on" t) (const :tag "off" nil)
		  (string :tag "Selected label types"))
	  (choice :tag "Searches match in toc lines   "
		  (const :tag "on" t) (const :tag "off" nil)
		  (string :tag "Selected label types"))))
	  

(defcustom reftex-guess-label-type t
  "*Non-nil means, `reftex-reference' will try to guess the label type.
To do that, RefTeX will look at the word before the cursor and compare it with
the words given in `reftex-label-alist'.  When it finds a match, RefTeX will
immediately offer the correct label menu - otherwise it will prompt you for
a label type.  If you set this variable to nil, RefTeX will always prompt."
  :group 'reftex-referencing-labels
  :type '(boolean))

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
  "List of files in \\bibliography{..} RefTeX should *not* parse.
The file names have to be in the exact same form as in the bibliography
macro - i.e. without the `.bib' extension.
Intended for files which contain only `@string' macro definitions and the
like, which are ignored by RefTeX anyway."
  :group 'reftex-citation-support
  :set 'reftex-set-dirty
  :type '(repeat (string :tag "File name")))

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
  "The format of citations to be inserted into the buffer.
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

If `reftex-cite-format' is an alist of characters and strings, the user
will be prompted for a character to select one of the possible format
strings.
  In order to configure this variable, you can either set
`reftex-cite-format' directly yourself or set it to the SYMBOL of one of
the predefined styles (see `reftex-cite-format-builtin').  E.g.:
(setq reftex-cite-format 'harvard)"
   :group 'reftex-citation-support
   :type
'(choice :format "%{%t%}: \n%[Value Menu%] %v"
  (radio :tag "Symbolic Builtins"
          :indent 4
 	  :value default
	  ;; Update here with: update-builtin-cite
	  (const :tag "default:        Default macro \\cite{%l}" default)
          (const :tag "harvard:        P. William's and T. Schnier's Harvard package" harvard)
          (const :tag "natbib:         Patrick W. Daly's Natbib package" natbib)
          (const :tag "astron:         S. Hogeveen's Astron package" astron)
          (const :tag "author-year:    Do-it-yourself Author year citation" author-year)
          (const :tag "locally:        Put full info in parenthesis" locally))
  (string :tag "format string" "\\cite{%l}")
  (repeat :tag "key-ed format strings"
	  :value ((?\r . "\\cite{%l}") 
                  (?t  . "\\cite{%l}") (?p . "\\cite{%l}"))
	  (cons (character :tag "Key character" ?\r)
                (string    :tag "Format string" "")))))

(defcustom reftex-comment-citations nil
  "Non-nil means add a comment for each citation describing the full entry.
The comment is formatted according to `reftex-cite-comment-format'."
  :group 'reftex-citation-support
  :type '(boolean))

(defcustom reftex-cite-comment-format  ; continue
  "%% %2a %y, %j %v, %P, %e: %b, %u, %s %<\n"
  "Citation format used for commented citations.  Must NOT contain %l."
  :group 'reftex-citation-support
  :type '(string))

(defcustom reftex-cite-punctuation '(", " " \\& " " {\\it et al.}")
  "Punctuation for formatting of name lists in citations.
This is a list of 3 strings.
1. normal names separator, like \", \"     in Jones, Brown and Miller
2. final names separator,  like \" and \"  in Jones, Brown and Miller
3. The \"et al\" string,     like \" {...}\" in Jones {\\it et al.}"
  :group 'reftex-citation-support 
  :type '(list
	  (string :tag "Separator for names            ")
	  (string :tag "Separator for last name in list")
	  (string :tag "string used as et al.          ")))

;; Table of contents configuration --------------------------------------

(defgroup reftex-table-of-contents-browser nil
  "A multifile table of contents browser."
  :group 'reftex)

(defcustom reftex-toc-follow-mode nil
  "Non-nil means, point in *toc* buffer will cause other window to follow.
The other window will show the corresponding part of the document.
This flag can be toggled from within the *toc* buffer with the `f' key."
  :group 'reftex-table-of-contents-browser
  :type '(boolean))

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
  "Plug-in flags for AUCTeX interface.
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

(defcustom reftex-use-fonts t
  "*Non-nil means, use fonts in label menu and on-the-fly help.
Font-lock must be loaded as well to actually get fontified display."
  :group 'reftex-miscellaneous-configurations
  :type '(boolean))

(defcustom reftex-keep-temporary-buffers t
  "*Non-nil means, keep any TeX and BibTeX files loaded for lookup.
Nil means, kill it immediately after use unless it was already an existing
buffer before the lookup happened.  It is faster to keep the buffers, but can
use a lot of memory, depending on the size of your database and document."
  :group 'reftex-miscellaneous-configurations
  :type '(boolean))

(defcustom reftex-auto-show-entry t
  "*Non-nil means, showing context in another window may unhide a section.
This is important when using outline-minor-mode.  If the context to be shown
is in a hidden section, RefTeX will issue a \"show-entry\" command in order
to show it.  This is not reversed when the label is selected - so the section
remains shown after command completion."
  :group 'reftex-miscellaneous-configurations
  :type '(boolean))

;;; End of Configuration Section ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;===========================================================================
;;;
;;; Define the formal stuff for a minor mode named RefTeX.
;;;

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

Extensive documentation about reftex is in the file header of `reftex.el'.

\\{reftex-mode-map}
Under X, these functions will also be available in a menu on the menu bar.

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
    (setq minor-mode-alist
          (cons '(reftex-mode " Ref") minor-mode-alist)))

(or (assoc 'reftex-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
          (cons (cons 'reftex-mode reftex-mode-map)
                minor-mode-map-alist)))










;;; ===========================================================================
;;;
;;; Silence warnings about variables in other packages.
(defvar TeX-master)
(defvar LaTeX-label-function)
(defvar tex-main-file)
(defvar outline-minor-mode)

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
  (let ((key (reftex-citation nil t)))
    (TeX-argument-insert (or key "") optional optional)))

(defun reftex-plug-into-AUCTeX ()
  ;; Replace AucTeX functions with RefTeX functions.
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

;;;###autoload
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
      (if (not (member entry reftex-label-alist-external-add-ons))
          (setq reftex-tables-dirty t
                reftex-label-alist-external-add-ons
		(cons entry reftex-label-alist-external-add-ons))))))

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
  '(reftex-label-numbers-symbol reftex-list-of-labels-symbol
                               reftex-bibfile-list-symbol))

;; Alist connecting master file names with the corresponding lisp symbols.
(defvar reftex-master-index-list nil)

;; Last index used for a master file.
(defvar reftex-multifile-index 0)

;; Alist connecting a master file with all included files.
(defvar reftex-master-include-list nil)

;; Variable holding the symbol with current value of label postfix.
(defvar reftex-label-numbers-symbol nil )
(make-variable-buffer-local 'reftex-label-numbers-symbol)

;; Variable holding the symbol with the label list of the document.
;; Each element of the label list is again a list with the following elements:
;; 0: One character label type indicator.
;; 1: Short context to put into label menu.
;; 2: The label.
;; 3: The name of the file where the label is defined.
(defvar reftex-list-of-labels-symbol nil)
(make-variable-buffer-local 'reftex-list-of-labels-symbol)

;; Variable holding the symbol with a list of library files for this document.
(defvar reftex-bibfile-list-symbol nil)
(make-variable-buffer-local 'reftex-bibfile-list-symbol)

(defun reftex-next-multifile-index ()
  ;; Return the next free index for multifile symbols.
  (setq reftex-multifile-index (1+ reftex-multifile-index)))

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
            reftex-master-index-list (cons
                                     (cons master index)
                                     reftex-master-index-list)
            newflag t))

    ;; Get/create symbols and tie them.
    (while symlist
      (setq symbol (car symlist)
            symlist (cdr symlist)
            symname (symbol-name symbol))
      (set symbol (intern (concat symname "-" (int-to-string index))))
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
      (error "No such master file %s" master)))
    (expand-file-name master)))

(defun reftex-make-master-buffer (master-file)
;; Make a master buffer which contains the MASTER-FILE and all includes.
;; This is to prepare a buffer containing the entire document in correct
;; sequence for parsing.
;; After making the buffer, it runs reftex-make-master-buffer-hook.
;; The function returns the number of input/include files not found.

  (let ((not-found 0) file file1 
	file-list dir-list tmp
	(this-syntax-table (syntax-table))
	(format-alist nil)
        (default-major-mode 'fundamental-mode)
        (after-insert-file-functions nil))

    ;; Set up the buffer with correct syntax table for parsing a tex buffer.
    ;; We do not use the mode because that may involve heavy stuff
    ;; like font-lock etc.
    (switch-to-buffer "*RefTeX Master*")
    (erase-buffer)
    (set-syntax-table this-syntax-table)

    ;; First insert the master file.
    (if (not (file-exists-p master-file))
        (error "No such master file: %s" master-file))
    (reftex-insert-buffer-or-file master-file)
    (subst-char-in-region (point-min) (point-max) ?\r ?\n t)
    (setq file-list (cons master-file file-list))
    (goto-char 1)

    ;; Remember from which file these lines came.
    (put-text-property (point-min) (point-max) 'file
                       (expand-file-name master-file))

    ;; Make the default directory that of the master file.
    ;; All input and include stuff works relative to that directory.
    (cd (file-name-directory (expand-file-name master-file)))

    ;; Create the list of directories from the TEXINPUTS environment variable
    (setq dir-list (cons default-directory 
			 (reftex-split (concat path-separator "+")
				       (or (getenv "TEXINPUTS") ""))))

    ;; Now find recursively all include/input statements and expand them.
    (while 
	(re-search-forward
	 "^[ \t]*\\\\\\(include\\|input\\)[{ \t]+\\([^} \t\n\r]+\\)[} \t\n\r]"
	 nil t)
      (setq file (reftex-no-props (match-string 2)))
      (if (not (and (> (length file) 4)
		    (string= (substring file -4) ".tex")))
	  (setq file (concat file ".tex")))
      (setq file1 file)
      (if (setq file (car (reftex-find-files-on-path (list file) dir-list)))
          (progn
            (replace-match
             (format "\n%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% START OF %s FILE: %s\n%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   END OF %s FILE: %s\n"
                     (match-string 1) file
                     (match-string 1) file))
            (beginning-of-line 0)
            (narrow-to-region (point) (point))
            ;; Insert the file.
            (reftex-insert-buffer-or-file file)
	    (subst-char-in-region (point-min) (point-max) ?\r ?\n t)
            (setq file-list (cons (expand-file-name file) file-list))
            ;; Remember from which file these lines came.
            (put-text-property (point-min) (point-max)
                               'file (expand-file-name file))
	    (goto-char (point-min))
            (widen))
        (message "Input/include file %s not found.  Ignored.  Continuing..."
                 file1)
        (setq not-found (1+ not-found))))
    (setq file-list (nreverse file-list))
    (while (setq tmp (assoc (car file-list) reftex-master-include-list))
      (setq reftex-master-include-list (delq tmp reftex-master-include-list)))
    (setq reftex-master-include-list
	  (cons file-list reftex-master-include-list))
    
    ;; Run the hook
    (run-hooks 'reftex-make-master-buffer-hook)

    not-found))

(defun reftex-insert-buffer-or-file (file)
  "If there is a buffer associated with FILE, insert it - otherwise the FILE."
  (let ((buffer (reftex-get-buffer-visiting file)))
    (if buffer
        (let (beg end beg1 end1)
          (save-excursion
            ;; Make sure we get the whole buffer.
            (set-buffer buffer)
            (setq beg (point-min) end (point-max))
            (widen)
            (setq beg1 (point-min) end1 (point-max)))
          (insert-buffer-substring buffer beg1 end1)
          (save-excursion
            (set-buffer buffer)
            (narrow-to-region beg end)))
      (insert-file-contents file))))


(defun reftex-parse-document (&optional buffer)
  "Rescan the document."
  (interactive)
  (save-window-excursion
    (save-excursion
      (if buffer
          (if (not (bufferp buffer))
              (error "No such buffer %s" (buffer-name buffer))
            (set-buffer buffer)))
      (reftex-access-scan-info t))))

(defun reftex-access-scan-info (&optional rescan)
  ;; Access the scanning info.  When the multifile symbols are not yet tied,
  ;; tie them.  When they are have to be created, do a buffer scan to
  ;; fill them.

  ;; If RESCAN is non-nil, enforce document scanning

  (catch 'exit
    (let ((rescan (or (equal rescan t) (equal rescan '(4)))))

      ;; Reset the mode if we had changes from style hooks.
      (and reftex-tables-dirty
           (reftex-reset-mode))
          
      (if (eq reftex-list-of-labels-symbol nil)
          ;; Symbols are not yet tied: Tie them and see if they are set.
	  (reftex-tie-multifile-symbols))

      (if (and (symbol-value reftex-list-of-labels-symbol)
               (not rescan))
          ;; Lists do already exist and we don't need to rescan.
          ;; Return from here.
          (throw 'exit t))

      ;; We need to rescan
      ;; =================
      
      (unwind-protect
          (save-window-excursion
            (save-excursion
              
              ;; Do the scanning.
              
              (let ((label-list-symbol    reftex-list-of-labels-symbol)
		    (label-numbers-symbol reftex-label-numbers-symbol)
                    (bibfile-list-symbol  reftex-bibfile-list-symbol))

                (message "Creating master buffer...")
                (reftex-make-master-buffer (reftex-TeX-master-file))
              
                (message "Scanning document...")

                (reftex-scan-buffer-for-labels
		 label-numbers-symbol label-list-symbol)

                (reftex-scan-buffer-for-bibliography-statement 
                 bibfile-list-symbol)
              
                (message "Scanning document... done"))))
        
        (if (get-buffer "*RefTeX Master*")
	    (kill-buffer "*RefTeX Master*"))))))

(defun reftex-create-tags-file ()
  "Create TAGS file by running `etags' on the current document.
The TAGS file is also immediately visited with `visit-tags-table'."
  (interactive)
  (reftex-access-scan-info current-prefix-arg)
  (let* ((master (reftex-TeX-master-file))
	 (files  (assoc master reftex-master-include-list))
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
  (let* ((master (reftex-TeX-master-file))
	 (default-directory (file-name-directory master))
	 (re (format "\\`%s\\(.*\\)" (regexp-quote 
			      (expand-file-name default-directory))))
         (files  (assoc master reftex-master-include-list))
         (cmd    (format 
		  "%s %s" grep-cmd
		  (mapconcat (function (lambda (x)
					 (if (string-match re x)
					     (match-string 1 x)
					   x)))
			     files " "))))
    (grep cmd)))

(defun reftex-search-document (&optional regexp)
  "Regexp search through all files of the current TeX document.
Starts always in the master file.  Stops when a match is found.
To continue searching for next match, use command \\[tags-loop-continue].
This works also without an active TAGS table."
  (interactive)
  (let ((default (reftex-this-word)))
    (if (not regexp)
	(setq regexp (read-string (format "Search regexp in document [%s]: "
					  default))))
    (if (string= regexp "") (setq regexp (regexp-quote default)))

    (reftex-access-scan-info current-prefix-arg)
    (tags-search regexp (list 'assoc (reftex-TeX-master-file)
			      'reftex-master-include-list))))

(defun reftex-query-replace-document (&optional from to delimited)
  "Run a query-replace-regexp of FROM with TO over the entire TeX document.
Third arg DELIMITED (prefix arg) means replace only word-delimited matches.
If you exit (\\[keyboard-quit] or ESC), you can resume the query replace
with the command \\[tags-loop-continue].
This works also without an active TAGS table."
  (interactive)
  (let ((default (reftex-this-word)))
    (if (not from)
	(progn
	  (setq from (read-string (format "Replace regexp in document [%s]: "
					  default)))
	  (if (string= from "") (setq from (regexp-quote default)))))
    (if (not to)
	(setq to (read-string (format "Replace regexp %s with: " from))))
    (reftex-access-scan-info current-prefix-arg)
    (tags-query-replace from to (or delimited current-prefix-arg)
			(list 'assoc (reftex-TeX-master-file)
			      'reftex-master-include-list))))

(defun reftex-change-label (&optional from to)
  "Query replace FROM with TO in all \\label and \\ref commands.
Works on the entire multifile document.
If you exit (\\[keyboard-quit] or ESC), you can resume the query replace
with the command \\[tags-loop-continue].
This works also without an active TAGS table."
  (interactive)
  (let ((default (reftex-this-word "-a-zA-Z0-9_*.:")))
    (if (not from)
	(setq from (read-string (format "Replace label globally [%s]: "
					default))))
    (if (string= from "") (setq from default))
    (if (not to)
	(setq to (read-string (format "Replace label %s with: "
				      from))))
    (reftex-query-replace-document
     (concat "\\\\\\(label\\|[a-z]*ref\\){" (regexp-quote from) "}")
     (format "\\\\\\1{%s}" to))))

(defun reftex-this-word (&optional class)
;; Grab the word around point.
  (setq class (or class "-a-zA-Z0-9:_/.*;|"))
  (save-excursion
    (buffer-substring-no-properties
     (progn (skip-chars-backward class) (point))
     (progn (skip-chars-forward  class) (point)))))

;;; ===========================================================================
;;;
;;; Functions to create and reference automatic labels.

;; The following constants are derived from `reftex-label-alist'.

;; Prompt used for label type querys directed to the user.
(defconst reftex-type-query-prompt nil)

;; Help string for label type querys.
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
   "^\\("
   (make-string (nth 0 reftex-abbrev-parameters) ?.)
   "[" (nth 2 reftex-abbrev-parameters) "]*"
   "\\)"
   "[" (nth 3 reftex-abbrev-parameters) "]"
   (make-string (1- (nth 1 reftex-abbrev-parameters)) ?.)))

;; Global variables used for communication between functions.
(defvar reftex-default-context-position nil)
(defvar reftex-location-start nil)
(defvar reftex-call-back-to-this-buffer nil)

;; List of buffers created temporarily for lookup, which should be killed.
(defvar reftex-buffers-to-kill nil)

;; Regexp to find section statements.  Computed from reftex-section-levels.
(defvar reftex-section-regexp nil)

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

(defun reftex-label (&optional environment no-insert)
  "Insert a unique label.  Return the label.
If ENVIRONMENT is given, don't bother to find out yourself.
If NO-INSERT is non-nil, do not insert label into buffer.
With prefix arg, force to rescan document first.
The label is also inserted into the label list.
This function is controlled by the settings of reftex-insert-label-flags."

  (interactive)
  ;; Ensure access to scanning info and rescan buffer if prefix are is '(4).
  (reftex-access-scan-info current-prefix-arg)

  ;; Find out what kind of environment this is and abort if necessary.
  (if (or (not environment)
          (not (assoc environment reftex-env-or-mac-alist)))
      (setq environment (reftex-label-location)))
  (if (not environment)
      (error "Can't figure out what kind of label should be inserted"))

  ;; Ok, go ahead.
  (let (label typekey prefix
	      entry format cell lab valid default force-prompt)
    (setq typekey (nth 1 (assoc environment reftex-env-or-mac-alist))
	  format (nth 3 (assoc environment reftex-env-or-mac-alist)))
    (setq prefix (or (cdr (assoc typekey reftex-typekey-to-prefix-alist))
                     (concat typekey "-")))
    ;; Replace any escapes in the prefix
    (setq prefix (reftex-replace-prefix-escapes prefix))

    ;; Make a default label.
    (cond

     ((reftex-typekey-check typekey (nth 0 reftex-insert-label-flags))
      ;; Derive a label from context.
      (setq default (nth 2 (reftex-label-info " " nil nil t)))
      ;; Catch the cases where the is actually no context available.
      (if (or (string-match "NO MATCH FOR CONTEXT REGEXP" default)
              (string-match "ILLEGAL VALUE OF PARSE" default)
              (string-match "SECTION HEADING NOT FOUND" default)
              (string-match "HOOK ERROR" default)
              (string-match "^[ \t]*$" default))
          (setq default prefix
                force-prompt t)                       ; need to prompt
        (setq default (concat prefix (reftex-string-to-label default)))

        ;; Make it unique.
	(setq default (reftex-uniquify-label default nil "-"))))

     ((reftex-typekey-check typekey (nth 1 reftex-insert-label-flags)) ; prompt
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

          (setq label (read-string "Label: " default))

          ;; Lets make sure that this is a legal label
          (cond

           ;; Test if label contains strange characters
           ((string-match reftex-label-illegal-re label)
            (message "Label \"%s\" contains illegal characters" label)
            (ding)
            (sit-for 2))

           ;; Look it up in the label list
           ((setq entry (assoc label
                               (symbol-value reftex-list-of-labels-symbol)))
            (message "Label \"%s\" exists in file %s" label (nth 3 entry))
            (ding)
            (sit-for 2))

           ;; Label is ok
           (t
            (setq valid t))))
      (setq label default))

    ;; Insert the label
    (if (not no-insert)
        (insert (format format label)))

    ;; Insert the label into the label list
    (if (symbol-value reftex-list-of-labels-symbol)
        (let ((cnt 0)
              (pos (point))
              (all (symbol-value reftex-list-of-labels-symbol))
              (look-for nil)
              (note nil)
              (text nil)
              (file (buffer-file-name)))

          ;; find the previous label in order to know where to insert new label
          ;; into label list
          (save-excursion
            (if (re-search-backward "\\\\[a-z]*label{\\([^}]+\\)}" nil 1 2)
                (setq look-for (reftex-no-props (match-string 1))))
            (if (or (re-search-forward
                     "\\\\\\(include\\|input\\){[^}\n]+}" pos t)
                    (re-search-forward reftex-section-regexp pos t)
                    (null look-for))
                (setq note "POSITION UNCERTAIN.  RESCAN TO FIX.")))
          (if (not look-for)
              (set reftex-list-of-labels-symbol
                   (cons (list label typekey text file note)
                         (symbol-value reftex-list-of-labels-symbol)))
            (while all
              (setq cell (car all)
                    all (cdr all)
                    cnt (1+ cnt)
                    lab (nth 0 cell))
              (if (string= lab look-for)
                  (progn
                    (setcdr
                     (nthcdr (1- cnt)
                             (symbol-value reftex-list-of-labels-symbol))
                     (cons (list label typekey text file note)
                           (nthcdr
                            cnt (symbol-value reftex-list-of-labels-symbol))))
                    ;; to end the loop, set all to nil
                    (setq all nil)))))))
    ;; return value of the function is the label
    label))

(defun reftex-string-to-label (string)
  ;; Convert a string (a sentence) to a label.
  ;;
  ;; Uses reftex-derive-label-parameters and reftex-abbrev-parameters
  ;;

  (let* ((words0 (reftex-split "[- \t\n\r]+"
                              (reftex-no-props string)))
         (ignore-words (nth 5 reftex-derive-label-parameters))
         words word)

    ;; remove words from the ignore list or with funny characters
    (while words0
      (setq word (car words0) words0 (cdr words0))
      (cond
       ((member (downcase word) ignore-words))
       ((string-match reftex-label-illegal-re word)
        (if (nth 2 reftex-derive-label-parameters)
            (progn
              (while (string-match reftex-label-illegal-re word)
                (setq word (replace-match "" nil nil word)))
              (setq words (cons word words)))))
       (t
        (setq words (cons word words)))))
    (setq words (nreverse words))

    ;; restrict number of words
    (if (> (length words) (nth 0 reftex-derive-label-parameters))
        (setcdr (nthcdr (1- (nth 0 reftex-derive-label-parameters)) words) nil))

    ;; First, try to use all words
    (setq string (mapconcat '(lambda(w) w) words
                            (nth 4 reftex-derive-label-parameters)))

    ;; Abbreviate words if enforced by user settings or string length
    (if (or (eq t (nth 3 reftex-derive-label-parameters))
            (and (nth 3 reftex-derive-label-parameters)
                 (> (length string) (nth 1 reftex-derive-label-parameters))))
        (setq words
              (mapcar
               '(lambda (w) (if (string-match reftex-abbrev-regexp w)
                               (match-string 1 w)
                             w))
               words)
              string (mapconcat '(lambda(w) w) words
                                (nth 4 reftex-derive-label-parameters))))

    ;; Shorten if still to long
    (setq string
          (if (> (length string) (nth 1 reftex-derive-label-parameters))
              (substring string 0 (nth 1 reftex-derive-label-parameters))
            string))

    ;; Delete the final punctuation, if any
    (if (string-match "[^a-zA-Z0-9]+$" string)
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
    (if (> p1 p2)
        (progn
          (setq reftex-default-context-position p1)
          (car loc1))
      (setq reftex-default-context-position
            (+ p2 8 (length (car loc2))))
      (or (car loc2) "section"))))


(defun reftex-uniquify-label (label &optional force separator)
  ;; Make label unique by appending a number.
  ;; Optional FORCE means, force appending a number, even if label is unique.
  ;; Optional SEPARATOR is a string to stick between label and number.

  ;; Ensure access to scanning info
  (reftex-access-scan-info)
  
  (cond
   ((and (not force)
	 (not (assoc label (symbol-value reftex-list-of-labels-symbol))))
    label)
   (t
    (let* ((cell (or (assoc label (symbol-value reftex-label-numbers-symbol))
		     (car
		      (set reftex-label-numbers-symbol
			   (cons (cons label 0)
				 (symbol-value reftex-label-numbers-symbol))))))
	   (num (1+ (cdr cell)))
	   (sep (or separator "")))
      (while (assoc (concat label sep (int-to-string num))
		    (symbol-value reftex-list-of-labels-symbol))
	(setq num (1+ num)))
      (setcdr cell num)
      (concat label sep (int-to-string num))))))

;; Help string for the reference label menu
(defconst reftex-select-label-prompt
  "Select: [n]ext [p]revious [r]escan [ ]context [q]uit RET [?]HELP+more")

(defconst reftex-select-label-help
  "              AVAILABLE KEYS IN REFERENCE LABEL MENU
              ======================================
 n / p      Go to next/previous label (Cursor motion works as well)
 C-s / C-r  Search forward/backward.  Use repeated C-s/C-r as in isearch.
 r / s      Rescan document for labels  / Switch label type
 t / #      Toggle table of contents    / Toggle counter mode
 c          Toggle display of short context
 SPC        Show full context for current label in other window
 f          Toggle follow mode: other window will follow context
 l / q      Reuse last referenced label / Quit without accepting label
 M-r        Recursive Edit into other window
 RET        Accept current label")

(defun reftex-reference (&optional type no-insert)
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

  (if (not type)
      ;; guess type from context
      (if (and reftex-guess-label-type
               (not (= 16 (prefix-numeric-value current-prefix-arg)))
               (setq type (assoc (downcase (reftex-word-before-point))
                                 reftex-words-to-typekey-alist)))
          (setq type (cdr type))
        (setq type (reftex-query-label-type))))

  (let (label pair
              (form (or (cdr (assoc type reftex-typekey-to-format-alist))
                        "\\ref{%s}")))

    ;; Have the user select a label
    (setq pair (reftex-offer-label-menu type))
    (setq label (car pair))

    (if (and label
             (not no-insert))
        (progn
          ;; do we need to remove spaces?
          (if (string= "~" (substring form 0 1))
              (while (or (= (preceding-char) ?\ )
                         (= (preceding-char) ?\C-i))
                (backward-delete-char 1)))
          ;; ok, insert the reference
          (insert (format form label label))
          (message ""))
      (message "Quit"))
    ;; return the label
    label))

(defun reftex-goto-label (&optional arg)
  "Go to a LaTeX label.  With prefix ARG, go to label in another window."
  (interactive "P")
  (let (type label file pair)
    (if (not type)
        (setq type (reftex-query-label-type)))

    (setq pair (reftex-offer-label-menu type)
          label (car pair)
          file (cdr pair))
    (if (and label file (file-exists-p file))
        (progn
          (if arg
              (find-file-other-window file)
            (find-file file))
          (goto-char (point-min))
          (if (not (re-search-forward 
		    (concat "\\\\[a-z]*label{" (regexp-quote label) "}")
		    nil t))
              (error "No such label found: %s" label)
	    (reftex-highlight 0 (match-beginning 0) (match-end 0))
	    (add-hook 'pre-command-hook 'reftex-highlight-shall-die)))
      (message "Quit")
      nil)))

;; Internal list with index numbers of labels in the selection menu
(defvar reftex-label-index-list nil)

(defun reftex-offer-label-menu (typekey)
  ;; Offer a menu with the appropriate labels.  Return (label . file).
  (let* ((buf (current-buffer))
         (near-label (reftex-find-nearby-label))
         (toc (reftex-typekey-check typekey reftex-label-menu-flags 0))
         (context (not (reftex-typekey-check
                        typekey reftex-label-menu-flags 3)))
         (counter (reftex-typekey-check
                   typekey reftex-label-menu-flags 2))
         (follow  (reftex-typekey-check
                   typekey reftex-label-menu-flags 4))
	 (match-everywhere (reftex-typekey-check 
			    typekey reftex-label-menu-flags 6))
         offset rtn key cnt last-cnt entry)

    (setq reftex-call-back-to-this-buffer buf)
    (setq entry (cons nil nil))

    (unwind-protect
        (catch 'exit
          (while t
            (save-window-excursion
              (switch-to-buffer-other-window "*RefTeX Select*")
              (erase-buffer)
              (setq truncate-lines t)
              (setq reftex-label-index-list (reftex-make-and-insert-label-list
                                            typekey buf toc context counter
                                            near-label))
              (setq near-label "_ ") ; turn off search for near label
              (setq offset (or (car reftex-label-index-list) offset))
              ;; use only when searched
              (setq reftex-label-index-list (cdr reftex-label-index-list))
              ;; only this is the true list
              (if (not reftex-label-index-list)
                  (error "No labels of type \"%s\"" typekey))
              (setq rtn
                    (reftex-select-item
		     reftex-select-label-prompt
                     "^>"
                     2
                     reftex-select-label-help
                     '(?r ?g ?c ?t ?s ?# ?l)
                     offset
                     'reftex-select-label-callback follow
		     match-everywhere))
              (setq key      (car rtn)
                    cnt      (nth 1 rtn)
                    last-cnt (nth 2 rtn)
                    offset   (1+ (or cnt last-cnt)))
              (if (not key) (throw 'exit nil))
              (cond
               ((or (eq key ?r)
		    (eq key ?g))
                ;; rescan buffer
                (reftex-parse-document buf))
               ((eq key ?c)
                ;; toggle context mode
                (setq context (not context)))
               ((eq key ?s)
                ;; switch type
                (setq typekey (reftex-query-label-type)))
               ((eq key ?t)
                ;; toggle tabel of contents display
                (setq toc (not toc)))
               ((eq key ?#)
                ;; toggle counter display
                (setq counter (not counter)))
               ((eq key ?l)
                ;; reuse the last referenced label again
                (setq entry reftex-last-used-reference)
                (throw 'exit t))
               (t
                (set-buffer buf)
		(if cnt
		    (progn
		      (setq entry (nth (nth cnt reftex-label-index-list)
			(symbol-value reftex-list-of-labels-symbol)))
		      (setq reftex-last-used-reference entry))
		  (setq entry nil))
                (throw 'exit t))))))
      (kill-buffer "*RefTeX Select*")
      (reftex-kill-temporary-buffers))
    (cons (reftex-no-props (nth 0 entry)) (nth 3 entry))))

;; Indentation for table of context lines in the menu
(defconst reftex-toc-indent " ")
;; Indentation for the lines containing the label
(defconst reftex-label-indent "> ")
;; Indentation for context lines
(defconst reftex-context-indent ".   ")
;; Indentation per section level
(defvar reftex-level-indent 2
  "*Number of spaces to be used for indentation per section level.
With more indentation, the label menu looks nicer, but shows less context.
Changing this is only fully operational after the next buffer scan.")

(defun reftex-make-and-insert-label-list (typekey0 buf toc context
                                                 counter near-label)
  ;; Insert a menu of all labels in buffer BUF into current buffer.
  ;; Return the list of labels, with the index of NEAR-LABEL as extra car.
  (let (ins-list index-list offset)
    (save-excursion
      (set-buffer buf)
      (let* ((all nil)
             (font (reftex-use-fonts))
             (cnt 0)
             ;; (file nil)
             (index -1)
             (toc-indent reftex-toc-indent)
             (label-indent
              (concat reftex-label-indent
                      (if toc (make-string (* 7 reftex-level-indent) ?\ ) "")))
             (context-indent
              (concat reftex-context-indent
                      (if toc (make-string (* 7 reftex-level-indent) ?\ ) "")))
             cell text label typekey note comment)

        ; Ensure access to scanning info
        (reftex-access-scan-info)

        (setq all (symbol-value reftex-list-of-labels-symbol))

        (while all

          (setq index (1+ index)
                cell (car all)
                all (cdr all))

          (if (null (nth 2 cell))
              ;; No context yet.  Quick update
              (progn
                (setq cell (reftex-label-info-update cell))
                (setcar (nthcdr index
                                (symbol-value reftex-list-of-labels-symbol))
                        cell)))

          ;; in the following setq we *copy* the label, since we will change
          ;; its properties, and we cannot have any properties in the list
          ;; (because of assoc searches)
          (setq label   (if (stringp (car cell))
			    (copy-sequence (car cell))
			  (car cell))
                typekey (nth 1 cell)
                text    (nth 2 cell)
                ;; file    (nth 3 cell)
                note    (nth 4 cell)
                comment (get-text-property 0 'in-comment text))

          (if (string= label near-label)
              (setq offset (1+ cnt)))

	  (cond
	   ((and toc (string= typekey "toc"))
	    (setq ins-list
		  (cons (concat toc-indent text "\n")
			ins-list)))
	   ((string= typekey "toc"))
	   ((and (or (string= typekey typekey0) (string= typekey0 " "))
		 (or (nth 5 reftex-label-menu-flags) ; show-commented?
		     (null comment)))
	    (setq cnt (1+ cnt))
	    (if comment (setq label (concat "% " label)))
	    (if font
		(put-text-property
		 0 (length label)
		 'face
		 (if comment
		     'font-lock-comment-face
		   'font-lock-reference-face)
		 label))
	    (setq index-list (cons index index-list))
	    (setq ins-list
		  (cons (concat
			 label-indent
			 label
			 (if counter (format " (%d) " cnt))
			 (if comment " LABEL IS COMMENTED OUT ")
			 (if note (concat "  " note) "")
			 "\n"
			 (if context (concat context-indent text "\n")))
			ins-list))
	    (put-text-property 0 (length (car ins-list)) 'cnt (1- cnt)
			       (car ins-list))
	    ))
	  )))

    (apply 'insert (nreverse ins-list))
    (cons offset (nreverse index-list))))

(defun reftex-query-label-type ()
  ;; Ask for label type
  (message reftex-type-query-prompt)
  (let ((key (read-char)))
    (if (eq key ?\?)
        (progn
          (save-window-excursion
            (with-output-to-temp-buffer "*RefTeX Help*"
              (princ reftex-type-query-help))
            (setq key (read-char))
            (kill-buffer "*RefTeX Help*"))))
    (if (not (member (char-to-string key) reftex-typekey-list))
        (error "No such label type: %s" (char-to-string key)))
    (char-to-string key)))

(defun reftex-find-nearby-label ()
  ;; Find a nearby label.
  (save-excursion
    (if (or (re-search-backward "\\\\[a-z]*label{\\([^}]+\\)}" nil t)
            (re-search-forward  "\\\\[a-z]*label{\\([^}]+\\)}" nil t))
        (reftex-no-props (match-string 1))
      nil)))

;; Variable holding the vector with section numbers
(defvar reftex-section-numbers [0 0 0 0 0 0 0 0])

(defun reftex-scan-buffer-for-labels (label-numbers-symbol label-list-symbol)
  ;; Scan the buffer for labels and save them in a list.
  (save-excursion
    (let ((regexp (concat "\\\\[a-z]*label{\\([^}]*\\)}" "\\|"
                          reftex-section-regexp))
          (font (reftex-use-fonts))
          (bound 0)
          (highest-level 100)
          file (level 1) star text text1 label section-number macro find)
      (set label-list-symbol nil)
      (goto-char 0)

      ;; reset label numbers
      (set label-numbers-symbol nil)

      ;; reset section numbers
      (reftex-init-section-numbers reftex-section-numbers)

      (while (re-search-forward regexp nil t)
        (setq file (get-text-property (match-beginning 0) 'file))
        (if (match-string 1)
            ;; It is a label
            (progn
              (setq label (reftex-no-props (match-string 1)))
              (set label-list-symbol
                   (cons (reftex-label-info label file bound)
                         (symbol-value label-list-symbol))))

          ;; It is a section
          (setq bound (point))
	  (setq star (= ?* (char-after (match-end 2))))
          (setq find (buffer-substring-no-properties
                      (1- (match-beginning 2)) (match-end 0)))
          (setq macro (reftex-no-props (match-string 2)))
          (setq level (cdr (assoc macro reftex-section-levels)))

          (setq section-number (reftex-section-number
                                reftex-section-numbers level star))
          (setq highest-level (min highest-level level))
          (if (= level highest-level)
              (message 
               "Scanning %s %s ..."
               (car (nth level reftex-section-levels))
               section-number))

          ;; get the title
          (save-match-data
	    (save-excursion
	      (setq text1 (reftex-context-substring))
	      (setq text (reftex-nicify-text text1))))

          (setq find 
		(reftex-allow-for-ctrl-m 
		 (concat find text1 
			 (char-to-string 
			  (char-after (+ (match-end 0) (length text1)))))))

          ;; add section number and indentation
          (setq text
                (concat
                 (make-string (* reftex-level-indent level) ?\ )
                 (if (nth 1 reftex-label-menu-flags) ; section number flag
                     (concat section-number " "))
                 text))
          ;; fontify
          (if font (put-text-property 0 (length text)
                                      'face 'font-lock-comment-face text))

          ;; insert in list
          (set label-list-symbol
               (cons (list 'toc "toc" text file find level section-number)
                     (symbol-value label-list-symbol)))))
      (set label-list-symbol
           (nreverse (symbol-value label-list-symbol))))))

(defun reftex-label-info-update (cell)
  ;; Update information about just one label in a different file.
  ;; CELL contains the old info list
  (let* ((label   (nth 0 cell))
         (typekey (nth 1 cell))
         ;; (text    (nth 2 cell))
         (file    (nth 3 cell))
         (note    (nth 4 cell))
         (buf (reftex-get-file-buffer-force
               file (not reftex-keep-temporary-buffers))))
    (if (not buf)
        (list label typekey "" file "LOST LABEL.  RESCAN TO FIX.")
      (save-excursion
        (set-buffer buf)
        (save-restriction
          (widen)
          (goto-char 1)

          (if (re-search-forward (concat "\\\\[a-z]*label{"
					 (regexp-quote label) "}")
                                 nil t)
              (append (reftex-label-info label file) (list note))
            (list label typekey "" file "LOST LABEL.  RESCAN TO FIX.")))))))

(defun reftex-label-info (label &optional file bound derive)
  ;; Return info list on LABEL at point.
  (let* ((env-or-mac (reftex-label-location bound))
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
    (= (preceding-char) ?%)))

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
           (if (re-search-backward reftex-section-regexp (point-min) t)
               (progn
                 (goto-char (match-end 0))
                 (reftex-context-substring))
             "SECTION HEADING NOT FOUND"))
       (save-excursion
         (goto-char reftex-default-context-position)
         (reftex-context-substring))))

    ((stringp parse)
     (save-excursion
       (if (re-search-backward parse bound t)
           (progn
             (goto-char (match-end 0))
             (reftex-context-substring))
         "NO MATCH FOR CONTEXT REGEXP")))

    ((integerp parse)
     (save-excursion
       (goto-char reftex-default-context-position)
       (reftex-nth-parens-substring parse)))

    ((fboundp parse)
     ;; A hook function.  Call it.
     (save-excursion
       (condition-case error-var
           (funcall parse env)
         (error (format "HOOK ERROR: %s" (cdr error-var))))))
    (t
     "ILLEGAL VALUE OF PARSE"))))

(defun reftex-nth-parens-substring (n)
  ;; Return the nth following {} or [] parentheses content.
  ;; If we are sitting at a macro start, skip to ent of macro name.
  (and (eq (following-char) ?\\) (skip-chars-forward "a-zA-Z*\\\\"))
  
  ;; Skip parenthesis pairs.
  (while (and (> (setq n (1- n)) 0)
	      (or (eq (following-char) ?{) (eq (following-char) ?\[))
	      (condition-case nil (or (forward-list 1) t) (error nil))))
  (skip-chars-forward "{\\[")
  (reftex-context-substring))

(defun reftex-context-substring ()
  ;; Return up to 100 chars from point
  ;; When point is just after a { or [, limit string to matching parenthesis
  (cond
   ((or (= (preceding-char) ?\{)
        (= (preceding-char) ?\[))
    ;; Inside a list - get only the list.
    (buffer-substring
     (point)
     (min (+ 100 (point))
	  (point-max)
	  (condition-case nil
	      (progn 
		(up-list 1)
		(1- (point)))
	    (error (point-max))))))
   (t
    ;; no list - just grab 100 characters
    (buffer-substring (point) (min (+ 100 (point)) (point-max))))))

(defun reftex-init-section-numbers (section-numbers &optional toc-entry)
  ;; Initialize the section numbers with zeros or with what is found
  ;; in the toc entry.
  (let* ((level  (or (nth 5 toc-entry) -1))
	 (numbers (nreverse (reftex-split "\\." (or (nth 6 toc-entry) ""))))
	 (depth (1- (length section-numbers)))
	 (i depth))
    (while (>= i 0)
      (if (> i level)
	  (aset section-numbers i 0)
	(aset section-numbers i (string-to-int (or (car numbers) "0")))
	(setq numbers (cdr numbers)))
      (setq i (1- i)))))

(defun reftex-section-number (section-numbers &optional level star)
  ;; Return a string with the current section number.
  ;; When LEVEL is non-nil, increase section numbers on that level.
  (let* ((depth (1- (length section-numbers))) idx n (string ""))
    (if level
        (progn
          (if (and (> level -1) (not star))
              (aset section-numbers level (1+ (aref section-numbers level))))
          (setq idx (1+ level))
          (while (<= idx depth)
            (aset section-numbers idx 0)
            (setq idx (1+ idx)))))
    (setq idx 0)
    (while (<= idx depth)
      (setq n (aref section-numbers idx))
      (setq string (concat string (if (not (string= string "")) "." "")
                           (int-to-string n)))
      (setq idx (1+ idx)))
    (save-match-data
      (if (string-match "\\`\\(0\\.\\)+" string)
          (setq string (replace-match "" nil nil string)))
      (if (string-match "\\(\\.0\\)+\\'" string)
          (setq string (replace-match "" nil nil string))))
    (if star 
	(concat (make-string (1- (length string)) ?\ ) "*")
      string)))

;; A variable to remember the index of the last label context shown
(defvar reftex-last-cnt 0)

(defun reftex-select-label-callback (cnt)
  ;; Callback function called from the label selection in order to
  ;; show context in another window
  (let* ((this-window (selected-window))
         index entry label file buffer)
    ;; pop to original buffer in order to get correct variables
    (catch 'exit
      (save-excursion
        (set-buffer reftex-call-back-to-this-buffer)
        (setq index (nth (or cnt 1) reftex-label-index-list)
              entry (nth index (symbol-value reftex-list-of-labels-symbol))
              label (nth 0 entry)
              file  (nth 3 entry)))

      ;; goto the file in another window
      (setq buffer (reftex-get-file-buffer-force
                    file (not reftex-keep-temporary-buffers)))
      (if buffer
          ;; good - the file is available
          (switch-to-buffer-other-window buffer)
        ;; we have got a problem here.  The file does not exist.
        ;; Let' get out of here..
        (ding)
        (throw 'exit nil))


      ;; search for that label
      (if (not (and (integerp cnt)
                    (integerp reftex-last-cnt)
                    (if (> cnt reftex-last-cnt)
                        (re-search-forward 
			 (concat "\\\\[a-z]*label{" (regexp-quote label)
				 "}") nil t)
                      (re-search-backward 
		       (concat "\\\\[a-z]*label{" (regexp-quote label)
			       "}") nil t))))
          (progn
            (goto-char 1)
            (re-search-forward (concat "\\\\[a-z]*label{"
				       (regexp-quote label) "}") nil t)))
      (reftex-highlight 0 (match-beginning 0) (match-end 0))
      (reftex-show-entry)
      (recenter (/ (window-height) 2))
      (select-window this-window))))

(defun reftex-pop-to-label (label file-list &optional mark-to-kill highlight)
  ;; Find LABEL in any file in FILE-LIST in another window.
  ;; If mark-to-kill is non-nil, mark new buffer for killing.
  ;; If HIGHLIGHT is non-nil, highlight the label definition.
  (let* ((re (concat "\\\\[a-z]*label{" (regexp-quote label) "}"))
         file buf)
    (catch 'exit
      (while file-list
        (setq file (car file-list)
              file-list (cdr file-list))
        (if (not (setq buf (reftex-get-file-buffer-force file mark-to-kill)))
            (error "No such file %s" file))
        (set-buffer buf)
        (widen)
        (goto-char (point-min))
        (if (re-search-forward re nil t)
            (progn
              (switch-to-buffer-other-window buf)
              (goto-char (match-beginning 0))
              (recenter (/ (window-height) 2))
	      (if highlight
		  (reftex-highlight 0 (match-beginning 0) (match-end 0)))
              (throw 'exit (selected-window)))))
      (error "Label %s not found" label))))

(defun reftex-find-duplicate-labels ()
  "Produce a list of all duplicate labels in the document."

  (interactive)

  ;; Rescan the document to make sure
  (reftex-access-scan-info t)

  (let ((master (reftex-TeX-master-file))
	(dlist
         (mapcar
          '(lambda(x)
             (let (x1)
               (cond
		((memq (car x) '(toc bof eof)) nil)
                (t
                 (setq x1 (reftex-all-assoc-string
                           (car x) (symbol-value reftex-list-of-labels-symbol)))
                 (if (< 1 (length x1))
                     (append (list (reftex-no-props (car x)))
                             (mapcar '(lambda(x)
                                        (abbreviate-file-name (nth 3 x))) x1))
                   (list nil))))))
          (reftex-uniquify (symbol-value reftex-list-of-labels-symbol)))))
    (setq dlist (reftex-uniquify dlist))
    (if (null dlist) (error "No duplicate labels in document"))
    (switch-to-buffer-other-window "*Duplicate Labels*")
    (make-local-variable 'TeX-master)
    (setq TeX-master master)
    (erase-buffer)
    (insert "                MULTIPLE LABELS IN CURRENT DOCUMENT:\n")
    (insert " Move point to label and type `M-x reftex-change-label'\n"
	    " This will run a query-replace on the label and its references\n")
    (insert " LABEL               FILE\n")
    (insert " -------------------------------------------------------------\n")
    (while dlist
      (if (and (car (car dlist))
               (cdr (car dlist)))
          (progn
            (insert (mapconcat '(lambda(x) x) (car dlist) "\n    ") "\n")))
      (setq dlist (cdr dlist)))
    (goto-char (point-min))))

(defun reftex-all-assoc-string (key list)
  ;; Return a list of all associations of KEY in LIST.  Comparison with string=
  (let (rtn)
    (while list
      (if (string= (car (car list)) key)
          (setq rtn (cons (car list) rtn)))
      (setq list (cdr list)))
    (nreverse rtn)))

(defun reftex-kill-temporary-buffers ()
  ;; Kill all buffers in the list reftex-kill-temporary-buffers.
  (while reftex-buffers-to-kill
    (if (bufferp (car reftex-buffers-to-kill))
        (progn
          (and (buffer-modified-p (car reftex-buffers-to-kill))
               (y-or-n-p (format "Save file %s? "
                                 (buffer-file-name
                                  (car reftex-buffers-to-kill))))
               (save-excursion
                 (set-buffer (car reftex-buffers-to-kill))
                 (save-buffer)))
          (kill-buffer (car reftex-buffers-to-kill))))
    (setq reftex-buffers-to-kill (cdr reftex-buffers-to-kill))))

(defun reftex-show-entry ()
  ;; Show entry if point is hidden by outline mode
  (let ((pos (point)))
    (if (and reftex-auto-show-entry
             (boundp 'outline-minor-mode)
             outline-minor-mode
             (looking-at "[^\n\r]*\r"))
        (progn
          (outline-back-to-heading)
          (show-entry)
          (goto-char pos)))))


(defun reftex-nicify-text (text)
  ;; Make TEXT nice for inclusion into label menu
  (while (string-match "[\n\r\t]\\|[ \t][ \t]+" text) ; remove extra whitespace
    (setq text (replace-match " " nil t text)))
  (if (string-match "\\\\end{.*" text)                ; nothing beyond \end{
      (setq text (replace-match "" nil t text)))
  (if (string-match "\\\\[a-z]*label{[^}]*}" text)    ; kill the label
      (setq text (replace-match "" nil t text)))
  (if (string-match "^ +" text)                       ; leading whitespace
      (setq text (replace-match "" nil t text)))
  (cond
   ((> (length text) 100)                             ; not to long
    (setq text (substring text 0 100)))
   ((= (length text) 0)                               ; not empty
    (setq text " ")))
  text)

(defun reftex-typekey-check (typekey conf-variable &optional n)
  ;; Check if CONF-VARIABLE is true or contains TYPEKEY
  (and n (setq conf-variable (nth n conf-variable)))
  (or (eq conf-variable t)
      (and (stringp conf-variable)
           (string-match (concat "[" conf-variable "]") typekey))))

;;; ===========================================================================
;;;
;;; Table of contents (contributed from Stephen Eglen, changed by C. Dominik)

;; We keep at most one *toc* buffer - it is easy to make them

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
SPC     Show the corresponding section of the LaTeX document.
TAB     Goto the section.
RET     Goto the section and hide the *toc* buffer (also on mouse-2).
q / Q   Hide/Kill *toc* buffer, return to position of last reftex-toc command.
f       Toggle follow mode on and off.
r / g   Reparse the LaTeX document.")

(defun reftex-toc ()
  "Show the table of contents for the current document.
When called with a raw C-u prefix, rescan the document first."

  (interactive)

  (if (or (not (string= reftex-last-toc-master (reftex-TeX-master-file)))
	  current-prefix-arg)
      (reftex-empty-toc-buffer))

  (setq reftex-last-toc-file   (buffer-file-name))
  (setq reftex-last-toc-master (reftex-TeX-master-file))

  (set-marker reftex-toc-return-marker (point))

  ;; If follow mode is active, arrange to delay it one command
  (if reftex-toc-follow-mode
      (setq reftex-toc-follow-mode 1))

  ;; Ensure access to scanning info and rescan buffer if prefix are is '(4)
  (reftex-access-scan-info current-prefix-arg)

  (let* ((all   (symbol-value reftex-list-of-labels-symbol))
         (where (reftex-nearest-section))
         toc toc1 cell label file find startpos)

    (if (get-buffer-window "*toc*")
        (select-window (get-buffer-window "*toc*"))
      (setq reftex-last-window-height (window-height))  ; remember
      (split-window-vertically)
      (switch-to-buffer (get-buffer-create "*toc*")))

    (cond
     ;; buffer is empty - fill it with the table of contents
     ((= (buffer-size) 0)

      (local-set-key  "?"         'reftex-toc-show-help)
      (local-set-key  " "         'reftex-toc-view-line)
      (local-set-key  "\C-m"      'reftex-toc-goto-line-and-hide)
      (local-set-key  "\C-i"      'reftex-toc-goto-line)
      (local-set-key  "r"         'reftex-toc-redo)
      (local-set-key  "g"         'revert-buffer)
      (local-set-key  "q"         'reftex-toc-quit)
      (local-set-key  "Q"         'reftex-toc-quit-and-kill)
      (local-set-key  "f"         'reftex-toc-toggle-follow)
      (local-set-key  [(mouse-2)] 'reftex-toc-mouse-goto-line-and-hide); Emacs
      (local-set-key  [(button2)] 'reftex-toc-mouse-goto-line-and-hide); XEmacs
      (make-local-variable 'revert-buffer-function)
      (setq revert-buffer-function 'reftex-toc-redo)
      (setq truncate-lines t)
      (make-local-hook 'post-command-hook)
      (make-local-hook 'pre-command-hook)
      (setq post-command-hook '(reftex-toc-post-command-hook))
      (setq pre-command-hook  '(reftex-toc-pre-command-hook))

      (insert (format
"TABLE-OF-CONTENTS on %s
 SPC=view  TAB=goto  RET=goto+hide  [q]uit  [r]escan  [f]ollow-mode  [?]Help
-------------------------------------------------------------------------------
" (abbreviate-file-name reftex-last-toc-master)))
      (setq startpos (point))

      (if (reftex-use-fonts)
          (put-text-property 1 (point) 'face 'font-lock-keyword-face))
      (put-text-property 1 (point) 'intangible t)

      (while all
        (setq cell (car all)
              all  (cdr all))
        (setq label  (nth 0 cell)
              toc    (nth 2 cell)
              file   (nth 3 cell)
              find   (nth 4 cell))
        (if (eq label 'toc)
            (progn
	      (setq toc1 (concat toc "\n"))
              (put-text-property 0 (length toc1)
                                 'file file toc1)
              (put-text-property 0 (length toc1)
                                 'find find toc1)
              (insert toc1)
              )))

      (backward-delete-char 1)

      (setq buffer-read-only t))
     (t
      (goto-line 3)
      (beginning-of-line)
      (setq startpos (point))))

    ;; Find the correct section
    (goto-char (point-max))
    (beginning-of-line)
    (while (and (> (point) startpos)
                (or (not (string= (get-text-property (point) 'file)
                                  (car where)))
                    (not (string= (get-text-property (point) 'find)
                                  (cdr where)))))
      (beginning-of-line 0))))

(defun reftex-nearest-section ()
  ;; Return (file . find) of nearest section command
  (let (cell label rest pos text1 text2)
    (save-excursion
      (cond
      ;; Try to find a section heading
       ((or (re-search-backward reftex-section-regexp nil t)
            (re-search-forward  reftex-section-regexp nil t))
        (goto-char (match-end 0))
	(setq pos (point)
	      text1 (buffer-substring-no-properties
		     (1- (match-beginning 1)) (match-end 0))
	      text2 (reftex-context-substring))
        (cons (buffer-file-name)
	      (reftex-allow-for-ctrl-m
	       (concat text1 text2 
		       (char-to-string
			(char-after (+ pos (length text2))))))))
       ;; Try to find a label
       ((and (or (re-search-backward "\\\\[a-z]*label{\\([^}]+\\)}" nil t)
                 (re-search-forward  "\\\\[a-z]*label{\\([^}]+\\)}" nil t))
             (setq label (reftex-no-props (match-string 1)))
             (setq cell (assoc label (symbol-value 
                                      reftex-list-of-labels-symbol)))
             (setq rest (memq cell (symbol-value reftex-list-of-labels-symbol)))
             (setq cell (car (memq (assoc nil rest) rest)))
             (null (car cell)))
        (cons (nth 3 cell) (nth 4 cell)))
       (t nil)))))

(defun reftex-toc-pre-command-hook ()
  ;; used as pre command hook in *toc* buffer
  (reftex-unhighlight 0)
  (reftex-unhighlight 1))

(defun reftex-toc-post-command-hook ()
  ;; used in the post-command-hook for the *toc* buffer
  (and (> (point) 1)
       (save-excursion
         (reftex-highlight 1 
			  (progn (beginning-of-line) (point))
			  (progn (end-of-line) (point)))))
  (cond
   ((integerp reftex-toc-follow-mode)
    ;; remove delayed action
    (setq reftex-toc-follow-mode t))
   (reftex-toc-follow-mode
    ;; show context in other window
    (condition-case nil
        (reftex-toc-visit-line)
      (error (ding) t)))))

(defun reftex-empty-toc-buffer ()
  (if (get-buffer "*toc*")
      (save-excursion 
	(set-buffer "*toc*")
	(setq buffer-read-only nil)
	(erase-buffer))))

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
  (goto-char (marker-position reftex-toc-return-marker)))
(defun reftex-toc-quit-and-kill ()
  "Kill the *toc* buffer."
  (interactive)
  (kill-buffer "*toc*")
  (or (one-window-p) (delete-window))
  (switch-to-buffer (marker-buffer reftex-toc-return-marker))
  (reftex-re-enlarge)
  (goto-char (marker-position reftex-toc-return-marker)))
(defun reftex-toc-redo (&rest ignore)
  "Regenerate the *toc* buffer.  Call only from within the *toc* buffer"
  (interactive)
  (switch-to-buffer-other-window
   (reftex-get-file-buffer-force reftex-last-toc-file))
  (setq current-prefix-arg '(4))
  (reftex-toc))

(defun reftex-toc-visit-line (&optional final)
  ;; Visit the tex file corresponding to the toc entry on the current line.
  ;; If FINAL is t, stay there
  ;; If FINAL is 'hide, hide the *toc* window.
  ;; Otherwise, move cursor back into *toc* window

  (let (file find beg end (toc-window (selected-window)) 
	     show-window show-buffer)
    (save-excursion
      (beginning-of-line)
      (setq beg (point))
      (end-of-line)
      (setq end (point)))

    ;; get the file and the search string
    (setq file (get-text-property (point) 'file))
    (setq find (get-text-property (point) 'find))
    (if (or (not file) (not find))
        (error "Cannot visit line"))

    (switch-to-buffer-other-window (reftex-get-file-buffer-force file))
    (setq show-window (selected-window))
    (setq show-buffer (current-buffer))
    (goto-char (point-min))

    (if (not (re-search-forward find nil t))
	(progn
	  (select-window toc-window)
	  (error "Cannot visit line")))

    (setq beg (match-beginning 0)
          end (match-end 0))

    (goto-char beg)
    (recenter 1)
    (reftex-highlight 0 beg end (current-buffer))

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
(defvar reftex-found-list)
(defvar reftex-cite-format-builtin)

;; The history list of regular expressions used for citations
(defvar reftex-cite-regexp-hist nil)

;; Prompt and help string for citation selection
(defconst reftex-citation-prompt
  "Select: [n]ext [p]revious [r]estrict [ ]full_entry [q]uit RET [?]Help+more")

(defconst reftex-citation-help
  "AVAILABLE KEYS IN MAKE CITATION MENU
---------------------------------------
 n / p      Go to next/previous entry (Cursor motion works as well).
 C-s / C-r  Search forward/backward.  Use repeated C-s/C-r as in isearch.
 g   / r    Start over with new regexp / Restrict with additional regexp.
 SPC        Show full database entry in other window.
 f          Toggle follow mode: Other window will follow with full db entry.
 q          Quit without inserting \\cite macro into buffer.
 M-r        Recursive edit into other window.
 RET  / a   Accept current entry / Accept all entries.")

;; Find bibtex files

(defun reftex-get-bibfile-list ()
  ;; Return list of bibfiles for current document

  ;; Ensure access to scanning info
  (reftex-access-scan-info)

  (or (symbol-value reftex-bibfile-list-symbol)
      (error "No BibTeX files to parse. Add \\bibliography statment to document and reparse.")))

(defun reftex-scan-buffer-for-bibliography-statement (bib-list-symbol)
  ;; Scan buffer for bibliography macro and store file list in bib-list-symbol.
  (let (file-list dir-list)
    (setq dir-list
          (reftex-split
           (concat path-separator "+")
           (mapconcat '(lambda(x)
                         (if (getenv x) (getenv x) ""))
                      reftex-bibpath-environment-variables
                      path-separator)))
    (goto-char (point-min))
    (if (re-search-forward "^[ \t]*\\\\bibliography{[ \t]*\\([^}]+\\)" nil t)
        (progn
          (setq dir-list
                (cons (file-name-directory
                       (get-text-property (match-beginning 0) 'file))
                      dir-list))
          (setq file-list
                (mapcar '(lambda (x) (concat x ".bib"))
                        (reftex-delete-list
                         reftex-bibfile-ignore-list
                         (reftex-split
                          "[ \t\n]*,[ \t\n]*"
                          (reftex-no-props (match-string 1)))))))
      (message "No \\bibliography command in document."))
    (set bib-list-symbol
         (if file-list
             (reftex-find-files-on-path file-list dir-list 
                                       "While parsing \\bibliography:")
           nil))))

(defun reftex-find-files-on-path (file-list path-list &optional error-string)
  ;; Search for all files in FILE-LIST on PATH-LIST.  Return absolute names.
  ;; A missing file throws an exception with the error message ERROR-STRING.
  ;; When error-string is nil, no exception is thrown.

  (let (found-list found file)
    (while file-list
      (setq file (car file-list)
            file-list (cdr file-list)
            found nil)
      (if (file-name-absolute-p file)
          (setq found (expand-file-name file))
        (let ((dirs path-list))
          (while (and dirs (not found))
            (if (and (not (string= (car dirs) ""))
                     (file-exists-p (expand-file-name file (car dirs))))
                (setq found (expand-file-name file (car dirs))))
            (setq dirs (cdr dirs)))))
      (if (and found
               (file-exists-p found))
          (add-to-list 'found-list (expand-file-name found))
	(and error-string
	     (error "%s No such file %s." error-string file))))
    (nreverse found-list)))

;; Find a certain reference in any of the BibTeX files.

(defun reftex-pop-to-bibtex-entry (key file-list
				      &optional mark-to-kill highlight)
  ;; Find BibTeX KEY in any file in FILE-LIST in another window.
  ;; If mark-to-kill is non-nil, mark new buffer to kill."

  (let* ((re (concat "@[a-zA-Z]+[ \t\n\r]*[{(][ \t\n\r]*" (regexp-quote key) "[ \t\n\r,]"))
         (window-conf (current-window-configuration))
         file buf)
    (catch 'exit
      (switch-to-buffer-other-window (current-buffer))
      (while file-list
        (setq file (car file-list)
              file-list (cdr file-list))
        (if (not (setq buf (reftex-get-file-buffer-force file mark-to-kill)))
            (error "No such file %s" file))
        (switch-to-buffer buf)
        (widen)
        (goto-char 0)
        (if (re-search-forward re nil t)
            (progn
              (goto-char (match-beginning 0))
              (recenter 0)
	      (if highlight
		  (reftex-highlight 0 (match-beginning 0) (match-end 0)))
              (throw 'exit (selected-window)))))
      (set-window-configuration window-conf)
      (beep)
      (message "No BibTeX entry with citation key %s" key))))

;; Parse bibtex buffers

(defun reftex-extract-bib-entries (buffers &optional get-word)
  ;; Extract bib entries which match regexps from BUFFERS.
  ;; BUFFERS is a list of buffers or file names.
  ;; Return list with entries."
  (let* (re-list first-re rest-re
                 ;; avoid fontification of lookup buffers
                 (lazy-lock-minimum-size 1)
                 (buffer-list (if (listp buffers) buffers (list buffers)))
                 found-list entry buffer1 buffer alist
                 key-point start-point end-point)

    (setq re-list (reftex-split "[ \t]*&&[ \t]*"
                               (read-string "RegExp [ && RegExp...]: "
                                              nil 'reftex-cite-regexp-hist)))

    (setq first-re (car re-list)    ; We'll use the first re to find things,
          rest-re  (cdr re-list))   ; the other to narrow down.
    (if (string-match "\\`[ \t]*\\'" first-re)
        (error "Empty regular expression"))

    (save-excursion
      (save-window-excursion

        ;; walk through all bibtex files
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
		(if (not (re-search-backward 
			  "^[ \t]*@\\([a-zA-Z]+\\)[ \t\n\r]*[{(]" nil t))
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
		(if (or (string= (downcase (match-string 1)) "string")
			(string= (downcase (match-string 1)) "comment")
			(string= (downcase (match-string 1)) "c")
			(< (point) key-point)) ; this means match not in {}
		    (progn
		      (goto-char key-point)
		      (throw 'search-again nil)))
		
		;; Well, we have got a match
		(setq entry (concat
			     (buffer-substring start-point (point)) "\n"))
		
		;; Check if other regexp match as well
		(setq re-list rest-re)
		(while re-list
		  (if (not (string-match (car re-list) entry))
		      ;; nope - move on
		      (throw 'search-again nil))
		  (setq re-list (cdr re-list)))
		
		(setq alist (reftex-parse-bibtex-entry
			     nil start-point end-point))
		(setq alist (cons (cons "&entry" entry) alist))
		
		;; check for crossref entries
		(if (assoc "crossref" alist)
		    (setq alist
			  (append
			   alist (reftex-get-crossref-alist alist))))
		
		;; format the entry
		(setq alist
		      (cons
		       (cons "&formatted" (reftex-format-bib-entry alist))
		       alist))
		
		;; add it to the list
		(setq found-list (cons alist found-list)))))
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
      (setq al1 (cdr al1)
	    al2 (cdr al2)))
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
             (concat "@\\w+[{(][ \t\n\r]*" (regexp-quote crkey) "[ \t\n\r]*,") nil t)
            (progn
              (setq start (match-beginning 0))
              (condition-case nil
                  (up-list 1)
                (error nil))
              (reftex-parse-bibtex-entry nil start (point)))
          nil)))))

;; Parse and format individual entries

(defun reftex-get-bib-names (field entry)
  ;; Return a list with the author or editor anmes in ENTRY
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
    (reftex-split "\n" names)))

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
                   (cons "&type" (downcase (reftex-no-props (match-string 1))))
                   (cons "&key"  (reftex-no-props (match-string 2))))))
        (while (re-search-forward "\\(\\w+\\)[ \t\n\r]*=[ \t\n\r]*" nil t)
          (setq key (reftex-no-props (downcase (match-string 1))))
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
          ;; remove extra whitesp
          (while (string-match "[\n\t\r]\\|[ \t][ \t]+" field)
            (setq field (replace-match " " nil t field)))
          ;; remove leading garbage
          (if (string-match "^[ \t{]+" field)
              (setq field (replace-match "" nil t field)))
          ;; remove trailing garbage
          (if (string-match "[ \t}]+$" field)
              (setq field (replace-match "" nil t field)))
          (setq alist (cons (cons key field) alist)))
      alist))))

(defun reftex-get-bib-field (fieldname entry)
  ;; Extract the field FIELDNAME from an ENTRY
  (or (cdr (assoc fieldname entry))
      ""))

(defun reftex-format-bib-entry (entry)
  ;; Format a BibTeX ENTRY so that it is nice to look at
  (let*
      ((rtn nil)
       (auth-list (reftex-get-bib-names "author" entry))
       (authors (mapconcat '(lambda (x) x) auth-list ", "))
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
    (setq authors
          (if (> (length authors) 30)
              (concat (substring authors 0 27) "...")
            (format "%-30s" authors))
          title
          (if (> (length title) 70)
              (concat (substring title   0 67) "...")
            (format "%-70s" title))
          extra
          (if (> (length extra) 40)
              (concat (substring extra   0 37) "...")
            (format "%-40s" extra)))
    (if (reftex-use-fonts)
        (progn
          (put-text-property 0 (length authors) 'face 'font-lock-keyword-face
                             authors)
          (put-text-property 0 (length title)   'face 'font-lock-comment-face
                             title)
          (put-text-property 0 (length extra)   'face 'font-lock-reference-face
                             extra)))
    (setq rtn (concat key "\n     " authors " " year " " extra
                      "\n     " title "\n\n"))
    rtn))

;; Make a citation

(defun reftex-citation (&optional arg no-insert)
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

  (interactive "P")

  ;; check for recursive edit
  (reftex-check-recursive-edit)

  ;; if there is just 1 C-u prefix arg, force to rescan buffer
  (if (and current-prefix-arg
           (listp current-prefix-arg)
           (= 4 (prefix-numeric-value arg)))
      (reftex-reset-scanning-information))

  ;; check if there is already a cite command at point and change cite format
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
	    (setq key (read-char))
	    (if (assq key format)
		(setq format (cdr (assq key format)))
	      (error "No citation format associated with key `%c'" key)))))

    (let* (entry cnt rtn ins-string re-list re
		 ;; scan bibtex files
		 (lazy-lock-minimum-size 1)
		 (reftex-found-list (reftex-extract-bib-entries
				    (reftex-get-bibfile-list)))
		 (found-list-r nil))
      (if (not reftex-found-list)
	  (error "Sorry, no matches found"))

      ;; remember where we came from
      (setq reftex-call-back-to-this-buffer (current-buffer))

      ;; offer selection
      (save-window-excursion
	(switch-to-buffer-other-window "*RefTeX Select*")
	(erase-buffer)
	(reftex-insert-bib-matches reftex-found-list)
	(if (= 0 (buffer-size))
	    (error "Sorry, no matches found"))
	(setq truncate-lines t)
	(goto-char 1)
	(if (catch 'exit
	      (while t
		(setq rtn
		      (reftex-select-item
		       reftex-citation-prompt
		       "^[^ \t\n]"
		       4
		       reftex-citation-help
		       '(?r ?a ?g ?\C-m)
		       nil
		       'reftex-bibtex-selection-callback nil))
		(setq key (car rtn)
		      cnt (nth 1 rtn))
		(if (not key) (throw 'exit nil))
		(cond
		 ((eq key ?g)
		  (setq reftex-found-list
			(save-excursion 
			  (set-buffer reftex-call-back-to-this-buffer)
			  (reftex-extract-bib-entries
			   (reftex-get-bibfile-list))))
		  (erase-buffer)
		  (reftex-insert-bib-matches reftex-found-list)
		  (if (= 0 (buffer-size))
		      (error "Sorry, no matches found"))
		  (goto-char 1))
		  
		 ((eq key ?r)
		  ;; restrict with new regular expression
		  (setq re-list
			(reftex-split "[ \t]*&&[ \t]*"
				     (read-string 
				      "RegExp [ && RegExp...]: "
				      nil 'reftex-cite-regexp-hist)))
		  (while re-list
		    (setq re (car re-list)
			  re-list (cdr re-list))
		    (setq found-list-r
			  (delete ""
				  (mapcar
				   '(lambda (x)
				      (if (string-match 
					   re (cdr (assoc "&entry" x)))
					  x
					""))
				   reftex-found-list))))
		  (if found-list-r
		      (setq reftex-found-list found-list-r)
		    (ding))
		  (erase-buffer)
		  (reftex-insert-bib-matches reftex-found-list)
		  (goto-char 1))
		 ((eq key ?a)
		  (setq entry 'all)
		  (throw 'exit t))
		 ((or (eq key ?\C-m)
		      (eq key 'return))
		  (if cnt
		      (setq entry (nth cnt reftex-found-list))
		    (setq entry nil))
		  (throw 'exit t))
		 (t
		  (ding)))))
	    (progn
	      ;; format the entry
	      (if (eq entry 'all)
		  (setq ins-string 
			(mapconcat
			 '(lambda (entry)
			    (reftex-format-citation entry format))
			 reftex-found-list "\n"))
		(setq ins-string (reftex-format-citation entry format))))
	  (setq ins-string "")
	  (message "Quit")))
      (kill-buffer "*RefTeX Select*")

      (if (not no-insert)
	  (progn
	    (insert ins-string)
	    (if (string-match "\\?" ins-string)
		(progn
		  (search-backward "?")
		  (delete-char 1)))))
      (message "")
      
      ;; Check if the prefix arg was numeric, and call recursively
      (if (and (integerp arg)
	       (> arg 1)
	       (re-search-backward
		"\\\\\\([a-zA-Z]*cite\\|cite[a-zA-Z]*\\)\\**\\(\\[[^]]*\\]\\)*{\\([^}]*\\)" nil t))   ;;; FIXME this can break with ? in format
	  (progn
	    (goto-char (match-end 0))
	    (setq arg (1- arg))
	    (reftex-citation arg))
	(reftex-kill-temporary-buffers))
      ;; Return the citation key
      (or (eq entry 'all)
	  (reftex-get-bib-field "&key" entry)))))
  
(defun reftex-insert-bib-matches (list)
  ;; Insert the bib matches and number them correctly
  (let ((cnt -1) tmp)
    (mapcar '(lambda (x)
	       (setq tmp (cdr (assoc "&formatted" x))
		     cnt (1+ cnt))
	       (put-text-property 0 (length tmp) 'cnt cnt tmp)
	       (insert tmp))
	    list)))

(defun reftex-format-names (namelist n)
  (interactive)
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

  (if (not (stringp format)) (setq format "\\cite{%l}"))

  (if (and reftex-comment-citations
	   (string-match "%l" reftex-cite-comment-format))
      (error "reftex-cite-comment-format contains illeagal %%l"))

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
	       ((= l ?P) (car (reftex-split 
			       "[- .]+"
			       (reftex-get-bib-field "pages" entry))))
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

;; this is slow and not recommended for follow mode
(defun reftex-bibtex-selection-callback (cnt)
  ;; Callback function to be called from the BibTeX selection, in
  ;; order to display context.  This function is relatively slow and not
  ;; recommended for follow mode, just for individual lookups.
  (let ((win (selected-window))
        (key (reftex-get-bib-field "&key" (nth cnt reftex-found-list)))
        (bibfile-list (save-excursion
                        (set-buffer reftex-call-back-to-this-buffer)
                        (reftex-get-bibfile-list))))
    (reftex-pop-to-bibtex-entry key bibfile-list
                               (not reftex-keep-temporary-buffers) t)
    (select-window win)))

;;; ===========================================================================
;;;
;;; Here is the routine used for selection

;; Marker for return point from recursive edit
(defvar reftex-recursive-edit-marker (make-marker))

(defun reftex-check-recursive-edit ()
  ;; Check if we are already in a recursive edit.  Abort with helpful
  ;; message if so.
  (if (marker-position reftex-recursive-edit-marker)
      (error
       (substitute-command-keys
        "In unfinished recursive edit. Finish (\\[exit-recursive-edit]) or abort (\\[abort-recursive-edit])."))))

(defun reftex-select-item (prompt next-re size help-string
                                 event-list &optional offset
                                 call-back cb-flag match-everywhere)
;; Select an item.  Show PROMPT to user, find next item with NEXT-RE
;; regular expression, return on any of the events listed in
;; EVENT-LIST.  The function returns the event along with an integer
;; indicating which item was selected.  When OFFSET is specified,
;; starts at that item in the list.  When CALL-BACK is given, it is a
;; function which is called with the index of the element.

  (let* (key key-sq b e ev cnt last-cnt cmd skip-callback
	     (search-str "") tmp search-start matched forward mini-map last-key
             (offset1 (or offset 1)))

    ;; Set up a minibuffer keymap for the search stuff
    (setq mini-map (copy-keymap minibuffer-local-map))
    (define-key mini-map "\C-s" 
      '(lambda () (interactive) (setq forward t) (exit-minibuffer)))
    (define-key mini-map "\C-r" 
      '(lambda () (interactive) (setq forward nil) (exit-minibuffer)))
    (define-key mini-map "\C-m" 'exit-minibuffer)

    (setq ev
          (catch 'exit
            (save-window-excursion
              (if (= 0 (buffer-size))
                  (throw 'exit nil))
              (setq truncate-lines t)
              (goto-char 1)
              (if (not (re-search-forward next-re nil t offset1))
                  (progn              ; in case the offset is illegal
                    (setq offset1 1)
                    (if (not (re-search-forward next-re nil t offset1))
                        (throw 'exit nil))))
              (beginning-of-line 1)
              (while t
		(setq last-cnt (or cnt last-cnt))
		(setq cnt (get-text-property (point) 'cnt))
                (if (and cnt cb-flag call-back (not skip-callback))
                    (funcall call-back cnt))
		(setq skip-callback nil)
		(if cnt
		    (setq b (or (previous-single-property-change
				 (1+ (point)) 'cnt)
				(point-min))
			  e (or (next-single-property-change
				 (point) 'cnt)
				(point-max)))
		  (setq b (point) e (point)))
		(reftex-highlight 1 b e)
                (if (or (not (pos-visible-in-window-p b))
                        (not (pos-visible-in-window-p e)))
                    (recenter (/ (window-height) 2)))
                (setq key-sq (read-key-sequence prompt))
		(setq last-key key)
                (setq key (car
                           (cond
                            ((fboundp 'listify-key-sequence) ; Emacs
                             (listify-key-sequence key-sq))
                            ((fboundp 'event-to-character)   ; XEmacs
                             (mapcar 'event-to-character key-sq))
                            (t (error "Please report this problem to dominik@strw.leidenuniv.nl")))))

                (setq cmd (key-binding key-sq))

		(reftex-unhighlight 2)
                (reftex-unhighlight 0)

                (cond

		 ;; Single line motions
                 ((or (eq key ?n)
                      (eq key ?\C-i)
                      (eq cmd 'next-line))
		  (forward-char 1)
                  (re-search-forward next-re nil t 1)
                  (beginning-of-line 1))
                 ((or (eq key ?p)
                      (eq cmd 'previous-line))
                  (re-search-backward next-re nil t))

		 ;; Page motions
                 ((eq cmd 'scroll-up)
                  (while (and (pos-visible-in-window-p)
                              (re-search-forward next-re nil t)))
                  (beginning-of-line 1)
                  (recenter 1))
                 ((eq cmd 'scroll-down)
                  (while (and (pos-visible-in-window-p)
                              (re-search-backward next-re nil t)))
                  (recenter (- (window-height) size 2)))

		 ;; Begin and end of buffer
		 ((eq cmd 'beginning-of-buffer)
		  (goto-char (point-min))
		  (re-search-forward next-re nil t)
		  (beginning-of-line 1))
		 ((eq cmd 'end-of-buffer)
		  (goto-char (point-max))
		  (re-search-backward next-re nil t))

		 ;; Exit
                 ((eq key ?q)
                  (throw 'exit nil))
                 ((eq key ?\C-g)
		  (if (or (eq last-key ?\C-s) (eq last-key ?\C-r))
		      (ding)
		    (bury-buffer)
		    (error "Abort")))
                 ((or (eq key ?\C-m)
                      (eq key 'return)
                      (eq cmd 'newline))
                  (throw 'exit 'return))
                 ((memq key event-list)
                  (throw 'exit key))

		 ;; Callback
                 ((or (eq key ?C)    ; backward compatibility
                      (eq key ?f))
                  (setq cb-flag (not cb-flag)))
                 ((eq key ?\ )
		  (if cnt (funcall call-back cnt) (ding)))

		 ;; Help
                 ((eq key ?\?)
		  (with-output-to-temp-buffer "*RefTeX Help*"
		    (princ help-string))
		  (setq skip-callback t))

		 ;; Searching
		 ((or (setq forward (eq key ?\C-s)) (eq key ?\C-r))
		  (if (or (and (not (eq last-key ?\C-s))
			       (not (eq last-key ?\C-r)))
			  (string= search-str ""))
		      (setq tmp                       ; get a new string
			    (read-from-minibuffer
			     (if (string= search-str "")
				 "Search: "
			       (format "Search [%s]:" search-str))
			     nil mini-map)
			    search-str (if (string= tmp "")
					   search-str tmp)))
		  (setq search-start (point))
		  (and (not (string= search-str ""))
		       (progn
			 (while 
			     (and (setq matched
					(if forward
					    (search-forward search-str nil 1)
					  (search-backward search-str nil 1)))
				  (or (>= (save-excursion 
					    (goto-char (match-beginning 0))
					    (current-column))
					  (window-width))
				      (not (or (get-text-property (point) 'cnt)
					       match-everywhere)))))
			 (if matched 
			     (reftex-highlight 2 (match-beginning 0)
					       (match-end 0))
			   (ding)
			   (goto-char search-start)))))

		 ;; Recursive edit
                 ((eq key ?\M-r)
                  (set-marker reftex-recursive-edit-marker (point))
                  (unwind-protect
                      (progn
                        (save-window-excursion
                          (save-excursion
                            (other-window 1)
                            (message
                             (substitute-command-keys
                              "Recursive edit.  Return to selection with \\[exit-recursive-edit]"))
                            (recursive-edit)))
                        (if (not (equal (marker-buffer
                                         reftex-recursive-edit-marker)
                                        (current-buffer)))
                            (error
                             "Cannot continue RefTeX from this buffer."))
                        (goto-char reftex-recursive-edit-marker))
                    (set-marker reftex-recursive-edit-marker nil)))

                 (t
                  (ding)))))))
    (and (get-buffer "*RefTeX Help*") (kill-buffer "*RefTeX Help*"))
    (message "")
    (list ev cnt last-cnt)))

;;; ===========================================================================
;;;
;;; View cross references

(defun reftex-view-crossref (&optional arg)
  "View cross reference of \\ref or \\cite macro at point.
If the macro at point is a \\ref, show the corresponding label definition.
If it is a \\cite, show the BibTeX database entry.
If there is no such macro at point, search forward to find one.
When you call this function several times in direct successtion, point will
move to view subsequent cross references further down in the buffer.
To cope with the plethora of variations in packages, this function
assumes any macro either starting with ending in `ref' or `cite' to contain
cross references.
With argument, actually select the window showing the cross reference."

  (interactive "P")

  ;; See where we are.
  (let* ((re "\\\\\\([a-z]*\\(cite\\|ref\\)\\|\\(cite\\|ref\\)[a-z]*\\)\\**\\(\\[[^{}]*\\]\\)?{")
	 (macro (car (car (reftex-what-macro t))))
	 (this-word (reftex-this-word "*a-zA-Z\\\\"))
	 (my-window (get-buffer-window (current-buffer)))
	 pop-window cmd args label point)

    (if (and macro 
	     (string-match "\\`\\\\cite\\|\\`\\\\ref\\|cite\\'\\|ref\\'"
			   macro))
	(and (setq macro (match-string 0 macro))
	     (string-match "\\`\\\\" macro)
	     (setq macro (substring macro 1)))
      (setq macro nil))
    
    (if (and macro (eq last-command this-command))
	(if (string= macro "cite")
	    (progn
	      (skip-chars-forward "^},%")
	      (while (and (eq (following-char) ?%)
			  (or (beginning-of-line 2) t)
			  (skip-chars-forward " \t\n")))
	      (skip-chars-forward ",")
	      (if (eq (following-char) ?})
		  (setq macro nil)))
	  (setq macro nil)))

    (if (and (not macro) 
	     (eq this-command 'reftex-view-crossref)
	     (or (not (string-match "\\`\\\\" this-word))
		 (search-backward "\\" nil t)
		 t)
	     (re-search-forward re nil t))
	(setq macro (or (match-string 2) (match-string 3))))

    (if (not macro)
	(error "No cross reference to display"))

    ;; Ensure access to scanning info
    (reftex-access-scan-info)

    (cond 
     ((string= macro "cite")
      (setq cmd 'reftex-pop-to-bibtex-entry
	    args (list
		  (reftex-no-props (reftex-this-word "^{},%\n"))
		  (reftex-get-bibfile-list) nil t)))
     ((string= macro "ref")
      (let* ((label (reftex-no-props (reftex-this-word "^{}%\n")))
	     (entry (assoc label (symbol-value reftex-list-of-labels-symbol))))
	(if entry
	    (setq cmd 'reftex-pop-to-label
		  args (list label (list (nth 3 entry)) nil t))
	  (error "Label %s not known - reparse document might help" label))))
     (t (error "This should not happen")))
    (setq point (point))
    (apply cmd args)
    (setq pop-window (selected-window))
    (add-hook 'pre-command-hook 'reftex-highlight-shall-die)
    (select-window my-window)
    (goto-char point)
    (and arg (select-window pop-window))))

(defun reftex-mouse-view-crossref (ev)
  "View cross reference of \\ref or \\cite macro where you click.
If the macro at point is a \\ref, show the corresponding label definition.
If it is a \\cite, show the BibTeX database entry.
If there is no such macro at point, search forward to find one.
With argument, actually select the window showing the cross reference."
  (interactive "e")
  (mouse-set-point ev)
  (setq last-command 'self-insert-command)   ;; make sure we do not move!
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

  (catch 'exit
    (if (null which) (throw 'exit nil))
    (let ((bound (or bound (save-excursion (re-search-backward
                                            reftex-section-regexp nil 1)
                                           (point))))
          pos cmd-list cmd)
      (save-restriction
        (save-excursion
          (narrow-to-region (max 1 bound) (point-max))
          ;; move back out of the current parenthesis
          (while (condition-case nil
                     (progn (up-list -1) t)
                   (error nil))
            ;; move back over any touching sexps
            (while (or (= (preceding-char) ?\])
                       (= (preceding-char) ?\}))
              (backward-sexp))
            (setq pos (point))
            (if (and (or (= (following-char) ?\[)
                         (= (following-char) ?\{))
                     (and (re-search-backward "\\(\\\\[*a-zA-Z]+\\)" nil t)
                          (= (match-end 0) pos)))
                (progn
                  (setq cmd (buffer-substring-no-properties
                             (match-beginning 0) (match-end 0)))
                  (if (eq t which)
                      (setq cmd-list (cons (cons cmd (point)) cmd-list))
                    (if (member cmd which)
                        (throw 'exit (cons cmd (point)))))))
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
            (setq env-list (cons (cons env (point)) env-list)))
           ((member env which)
            (throw 'exit (cons env (point))))))
        (nreverse env-list)))))

(defun reftex-word-before-point ()
  ;; Return the word before point.  Word means here:
  ;; Consists of [a-zA-Z0-9.:] and ends at point or whitespace.
  (let ((pos (point)))
    (save-excursion
      (re-search-backward "[^ \t\n\r]" (point-min) 1)
      (setq pos (1+ (point)))
      (if (re-search-backward "[^a-zA-Z0-9\\\.:]"  (point-min) 1)
          (forward-char 1))
      (buffer-substring-no-properties (point) pos))))

;; ============================================================================
;;
;; Some generally useful functions

(defun reftex-no-props (string)
  ;; Return STRING with all text properties removed
  (and (stringp string)
       (set-text-properties 0 (length string) nil string))
  string)

(defun reftex-split (regexp string)
  ;; Split like perl
  (let ((start 0) list)
    (while (string-match regexp string start)
      (setq list (cons (substring string start (match-beginning 0)) list))
      (setq start (match-end 0)))
    (setq list (nreverse (cons (substring string start) list)))))

(defun reftex-allow-for-ctrl-m (string)
  ;; convert STRING into a regexp, allowing ^M for \n
  (let ((start -2))
    (setq string (regexp-quote string))
    (while (setq start (string-match "[\n\r]" string (+ 3 start)))
      (setq string (replace-match "[\n\r]" nil t string)))
    string))

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
   ((fboundp 'find-buffer-visiting)       ; Emacs
    (find-buffer-visiting file))
   ((boundp 'find-file-compare-truenames) ; XEmacs
    (let ((find-file-compare-truenames t))
      (get-file-buffer file)))
   (t (error "Please report this problem to dominik@strw.leidenuniv.nl"))))

(defun reftex-get-file-buffer-force (file &optional mark-to-kill)
  ;; Return a buffer visiting file.  Make one, if necessary.
  ;; If neither such a buffer no the file exist, return nil.
  ;; If MARK-TO-KILL in non-nil, put any new buffers into the kill list."

  (let ((buf (reftex-get-buffer-visiting file)))
    (cond
     (buf buf)
     ((file-exists-p file)
      (setq buf (find-file-noselect file))
      (if mark-to-kill
          (add-to-list 'reftex-buffers-to-kill buf))
      buf)
     (t nil))))

(defun reftex-splice-symbols-into-list (list alist)
  ;; Splice the association in ALIST of any symbols in LIST into the list.
  ;; Return new list.
  (let (rtn tmp)
    (while list
      (while (and (not (null (car list)))
		  (symbolp (car list)))
        (setq tmp (car list))
        (cond
         ((assoc tmp alist)
          (setq list (append (nth 2 (assoc tmp alist)) (cdr list))))
         (t
          (error "Cannot treat symbol %s in reftex-label-alist"
                 (symbol-name tmp)))))
      (setq rtn (cons (car list) rtn)
            list (cdr list)))
    (nreverse rtn)))

(defun reftex-uniquify (alist &optional keep-list)
  ;; Return a list of all elements in ALIST, but each car only once.
  ;; Elements of KEEP-LIST are not removed even if duplicate.
  (let (new elm)
    (while alist
      (setq elm (car alist)
            alist (cdr alist))
      (if (or (member (car elm) keep-list)
	      (not (assoc (car elm) new)))
          (setq new (cons elm new))))
    (setq new (nreverse new))
    new))

(defun reftex-use-fonts ()
  ;; Return t if we can and want to use fonts.
  (and window-system
       reftex-use-fonts
       (boundp 'font-lock-keyword-face)))

;; Highlighting uses overlays.  If this is for XEmacs, we need to load
;; the overlay library, available in version 19.15
(and (not (fboundp 'make-overlay))
     (condition-case nil
         (require 'overlay)
       (error 
        (error "RefTeX needs overlay emulation (available in XEmacs 19.15)"))))

;; We keep a vector with several different overlays to do our highlighting.
(defvar reftex-highlight-overlays [nil nil nil])

;; Initialize the overlays
(aset reftex-highlight-overlays 0 (make-overlay 1 1))
(overlay-put (aref reftex-highlight-overlays 0) 'face 'highlight)
(aset reftex-highlight-overlays 1 (make-overlay 1 1))
(overlay-put (aref reftex-highlight-overlays 1) 'face 'highlight)
(aset reftex-highlight-overlays 2 (make-overlay 1 1))
(overlay-put (aref reftex-highlight-overlays 2) 'face 
	     (if (string-match "XEmacs" emacs-version) 'zmacs-region 'region))

;; Two functions for activating and deactivation highlight overlays
(defun reftex-highlight (index begin end &optional buffer)
  "Highlight a region with overlay INDEX."
  (move-overlay (aref reftex-highlight-overlays index)
                begin end (or buffer (current-buffer))))
(defun reftex-unhighlight (index)
  "Detatch overlay INDEX."
  (delete-overlay (aref reftex-highlight-overlays index)))

(defun reftex-highlight-shall-die ()
  ;; Function used in pre-command-hook to remove highlights.
  (remove-hook 'pre-command-hook 'reftex-highlight-shall-die)
  (reftex-unhighlight 0))

;;; ---------------------------------------------------------------------------
;;;
;;; Cursor position after insertion of forms

(defun reftex-position-cursor ()
  ;; Search back to question mark, delete it, leave point there
  (if (search-backward "\?" (- (point) 100) t)
      (delete-char 1)))

(defun reftex-item ()
  "Insert an \\item and provide a label if the environments supports that."
  (interactive)
  (let ((env (car
              (reftex-what-environment '("itemize" "enumerate" "eqnarray")))))

    (if (and env (not (bolp))) (newline))

    (cond

     ((string= env "eqnarray")
      (if (not (bolp))
	  (newline))
      (reftex-label env)
      (insert "\n & & ")
      (beginning-of-line 1))

     ((string= env "itemize")
      (newline)
      (insert "\\item "))

     ((string= env "enumerate")
      (newline)
      (insert "\\item")
      (reftex-label env)
      (insert " "))
     (t
      (error "\\item command does not make sense here...")))))

;;; ---------------------------------------------------------------------------
;;; ---------------------------------------------------------------------------
;;; ---------------------------------------------------------------------------
;;;
;;; Data Section: Definition of large constants

(defconst reftex-label-alist-builtin
  '(
    ;; Some aliases, mostly for backward compatibility
    (Sideways    "Alias for -->rotating" (rotating))
    (AMSTeX      "Alias for -->amsmath" (amsmath))

    ;; Here come the individual packages of the LaTeX distribution
    (amsmath     "AMS math environments"
     (("align"    ?e "eq:" "~\\eqref{%s}" "\\\\begin{align}\\|\\\\\\\\")
      ("gather"   ?e "eq:" nil            "\\\\begin{gather}\\|\\\\\\\\")
      ("multline" ?e "eq:" nil            t)
      ("flalign"  ?e "eq:" nil            "\\\\begin{flalign}\\|\\\\\\\\")
      ("alignat"  ?e "eq:" nil     "\\\\begin{alignat}{[0-9]*}\\|\\\\\\\\")
      ("xalignat" ?e "eq:" nil     "\\\\begin{xalignat}{[0-9]*}\\|\\\\\\\\")
      ("xxalignat" ?e "eq:" nil    "\\\\begin{xxalignat}{[0-9]*}\\|\\\\\\\\")))

    (longtable   "The longtable environment"
     (("longtable"      ?t nil nil "\\\\caption\\(\\[[^]]*\\]\\)?{")))

    (rotating    "Sidewaysfigure and table"
     (("sidewaysfigure" ?f nil nil "\\\\caption\\(\\[[^]]*\\]\\)?{")
      ("sidewaystable"  ?t nil nil "\\\\caption\\(\\[[^]]*\\]\\)?{")))

    (subfigure   "Subfigure environments/macro"
     (("subfigure"   ?f nil nil "\\\\caption\\(\\[[^]]*\\]\\)?{")
      ("subfigure*"  ?f nil nil "\\\\caption\\(\\[[^]]*\\]\\)?{")
      ("\\subfigure" ?f nil nil "\\\\subfigure[[{]")))

    (LaTeX       "LaTeX default environments"
     (("section"   ?s "sec:" "~\\ref{%s}" (nil . t)
       ("Part" "Chapter" "Chap." "Section" "Sec." "Sect." "Paragraph" "Par."
	"\\S" "Teil" "Kapitel" "Kap." "Abschnitt" ))
     
      ("enumerate" ?n "item:" "~\\ref{%s}" "\\\\item\\(\\[[^]]*\\]\\)?"
       ("Item" "Punkt"))
     
      ("equation"  ?e "eq:" "~(\\ref{%s})" t
       ("Equation" "Eq." "Eqn." "Gleichung"  "Gl."))
      ("eqnarray"  ?e "eq:" nil "\\\\begin{eqnarray}\\|\\\\\\\\")
      
      ("figure"    ?f "fig:" "~\\ref{%s}" "\\\\caption\\(\\[[^]]*\\]\\)?{"
       ("Figure" "Fig." "Abbildung" "Abb."))
      ("figure*"   ?f nil nil "\\\\caption\\(\\[[^]]*\\]\\)?{")
      
      ("table"     ?t "tab:" "~\\ref{%s}" "\\\\caption\\(\\[[^]]*\\]\\)?{"
       ("Table" "Tab." "Tabelle"))
      ("table*"    ?t nil nil "\\\\caption\\(\\[[^]]*\\]\\)?{")
      
      ("any"       ?\  " "   "\\ref{%s}" nil)))
    
    )
  "The default label environment descriptions.
Lower-case symbols correspond to a style file of the same name in the LaTeX
distribution.  Mixed-case symbols are convenience aliases.")

(defconst reftex-cite-format-builtin
  '(
    (default "Default macro \\cite{%l}" 
      "\\cite{%l}")
    (harvard "P. William's and T. Schnier's Harvard package"
      ((?\C-m . "\\cite{%l}")
       (?p    . "\\cite{%l}")
       (?t    . "\\citeasnoun{%l}")
       (?n    . "\\citeasnoun{%l}")
       (?s    . "\\possessivecite{%l}")
       (?e    . "\\citeaffixed{%l}{?}")
       (?y    . "\\citeyear{%l}")
       (?a    . "\\citename{%l}")))
    (natbib "Patrick W. Daly's Natbib package"
      ((?\C-m . "\\cite{%l}")
       (?t    . "\\citet{%l}")
       (?T    . "\\citet*{%l}")
       (?p    . "\\citep{%l}")
       (?P    . "\\citep*{%l}")
       (?e    . "\\citep[e.g.][]{%l}")
       (?a    . "\\citeauthor{%l}")
       (?y    . "\\citeyear{%l}")))
    (astron "S. Hogeveen's Astron package"
      ((?\C-m . "\\cite{%l}")
       (?p    . "\\cite{%l}" )
       (?t    . "%2a (\\cite{%l})")))
    (author-year "Do-it-yourself Author year citation"
      ((?\C-m . "\\cite{%l}")
       (?t    . "%2a (%y)\\nocite{%l}")
       (?p    . "(%2a %y\\nocite{%l})")))
    (locally     "Put full info in parenthesis"
      "(%2a %y, %j %v, %P, %e: %b, %u, %s %<)") 
    ;; undocumented feature: `%<' kills white space and punctuation locally.
    )
  "Builtin versions of for the citation format.
The following conventions are valid for all alist entries:
`?\C-m' should always point to a straight \\cite{%l} macro.
`?t'    should point to a textual citation (citation as a noun).
`?p'    should point to a parenthetical citation.")

;;; ---------------------------------------------------------------------------
;;;
;;; Functions to compile the tables, reset the mode etc.

(defun reftex-reset-mode ()
  "Reset RefTeX Mode.  Required to implement changes to some list variables.
This function will compile the information in `reftex-label-alist' and similar
variables.  It is called when RefTeX is first used, and after changes to
these variables via `reftex-add-to-label-alist'."
  (interactive)

  ;; Record that we have done this
  (setq reftex-tables-dirty nil)

  ;; Kill temporary buffers associated with RefTeX - just in case they
  ;; were not cleaned up properly
  (let ((buffer-list '("*RefTeX Master*" "*RefTeX Help*" "*RefTeX Select*"
		      "*Duplicate Labels*" "*toc*" "*RefTeX-scratch*")))
    (while buffer-list
      (if (get-buffer (car buffer-list))
	  (kill-buffer (car buffer-list)))
      (setq buffer-list (cdr buffer-list))))

  (reftex-reset-scanning-information)

  ;; Plug functions into AUCTeX if the user option says so
  (reftex-plug-into-AUCTeX)

  ;; To update buffer-local variables
  (hack-local-variables)
  (message "updating internal tables...")
  (reftex-compute-ref-cite-tables)
  (message "updating internal tables... done"))

(defun reftex-reset-scanning-information ()
  "Reset the symbols containing information from buffer scanning.
This enforces rescanning the buffer on next use."
  (if (string= reftex-last-toc-master (reftex-TeX-master-file))
      (reftex-empty-toc-buffer))
  (let ((symlist reftex-multifile-symbols)
        symbol)
    (while symlist
      (setq symbol (car symlist)
            symlist (cdr symlist))
      (if (and (symbolp (symbol-value symbol))
               (not (null (symbol-value symbol))))
          (set (symbol-value symbol) nil)))))

(defun reftex-compute-ref-cite-tables ()
  ;; Update ref and cite tables

  (interactive)

  ;; Compile information in reftex-label-alist
  (let ((tmp (reftex-uniquify (reftex-splice-symbols-into-list
			      (append
			       reftex-label-alist
			       reftex-label-alist-external-add-ons
			       reftex-default-label-alist-entries)
			      reftex-label-alist-builtin)
			     '(nil)))
        entry env-or-mac typekeychar typekey prefix context
        fmt reffmt labelfmt wordlist qh-list)

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
	    (setq fmt (reftex-split "@+" fmt))
	  (setq fmt (list "\\label{%s}" fmt)))
	(setq labelfmt (car fmt)
	      reffmt (nth 1 fmt))
	(if typekey
	    (add-to-list 'reftex-typekey-list typekey))
	(if (and typekey prefix 
		 (not (assoc typekey reftex-typekey-to-prefix-alist)))
	    (add-to-list 'reftex-typekey-to-prefix-alist (cons typekey prefix)))
	(cond
	 ((string-match "\\`\\\\" env-or-mac)
	  ;; It's a macro
	  (add-to-list 'reftex-label-mac-list env-or-mac))
	 (t
	  (cond ((string= env-or-mac "any"))
		((string= env-or-mac ""))
		((string= env-or-mac "section"))
		(t
		 (add-to-list 'reftex-label-env-list env-or-mac)
		 ;; if context is t, translate to skip many parens pairs
		 (if (eq t context) (setq context 100))))))
	(and reffmt
	     (not (assoc typekey reftex-typekey-to-format-alist))
	     (setq reftex-typekey-to-format-alist
		   (cons (cons typekey reffmt)
			 reftex-typekey-to-format-alist)))
	(and (not (string= env-or-mac "any"))
	     (not (string= env-or-mac ""))
	     (not (assoc env-or-mac reftex-env-or-mac-alist))
	     (setq reftex-env-or-mac-alist
		   (cons (list env-or-mac typekey context labelfmt)
			 reftex-env-or-mac-alist)))
	(while (and wordlist (stringp (car wordlist)))
	  (or (assoc (car wordlist) reftex-words-to-typekey-alist)
	      (setq reftex-words-to-typekey-alist
		    (cons (cons (downcase (car wordlist)) typekey)
			  reftex-words-to-typekey-alist)))
	  (setq wordlist (cdr wordlist)))
	(cond
	 ((string= "" env-or-mac) nil)
	 ((assoc typekey qh-list)
	  (setcdr (assoc typekey qh-list)
		  (concat (cdr (assoc typekey qh-list)) " " env-or-mac)))
	 (t
	  (setq qh-list (cons (cons typekey env-or-mac) qh-list))))))

    (setq qh-list (nreverse qh-list))
    (setq reftex-typekey-to-prefix-alist
          (nreverse reftex-typekey-to-prefix-alist))
    (setq reftex-type-query-prompt
          (concat "Label type: "
                  (mapconcat '(lambda(x)
                                (format "[%s]" (car x)))
                             qh-list " ")
                  " (?=Help)"))
    (setq reftex-type-query-help
          (concat "SELECT A LABEL TYPE:\n--------------------\n"
                  (mapconcat '(lambda(x)
                                (format " [%s]   %s"
                                        (car x) (cdr x)))
                             qh-list "\n")))

    ;; Calculate the section regexp
    (setq reftex-section-regexp 
	  (concat "^[      ]*\\\\\\("
		  (mapconcat 'car reftex-section-levels "\\|")
		  "\\)\\*?\\(\\[[^]]*\\]\\)?{"))
    ))

;;; Keybindings --------------------------------------------------------------

(define-key reftex-mode-map  "\C-c-"     'reftex-item)
(define-key reftex-mode-map  "\C-c="     'reftex-toc)
(define-key reftex-mode-map  "\C-c("     'reftex-label)
(define-key reftex-mode-map  "\C-c)"     'reftex-reference)
(define-key reftex-mode-map  "\C-c["     'reftex-citation)
(define-key reftex-mode-map  "\C-c&"     'reftex-view-crossref)

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

;;; Menus --------------------------------------------------------------------

;; Define a menu for the menu bar if Emacs is running under X

(require 'easymenu)

(easy-menu-define 
 reftex-mode-menu reftex-mode-map
 "Menu used in RefTeX mode"
 '("Ref"
   ["Table of Contents"       reftex-toc t]
   "----"
   ["\\label"                 reftex-label t]
   ["\\ref"                   reftex-reference t]
   ["\\cite"                  reftex-citation t]
   ["View crossref"           reftex-view-crossref t]
   "----"
   ("Search and Replace"
    ["Search whole document"  reftex-search-document t]
    ["Replace in document"    reftex-query-replace-document t]
    ["Grep on document"       reftex-grep-document t]
    "----"
    ["Find duplicate labels"  reftex-find-duplicate-labels t]
    ["Change label and refs"  reftex-change-label t]
    "----"
    ["Create TAGS file"       reftex-create-tags-file t])
   "----"
   ["Parse document"          reftex-parse-document t]
   ["Reset RefTeX Mode"       reftex-reset-mode t]
   ["Show documentation"      reftex-show-commentary t]
   ["Customize RefTeX"        reftex-customize t]))

;;; Run Hook ------------------------------------------------------------------

(run-hooks 'reftex-load-hook)

;;; That's it! ----------------------------------------------------------------

; Make sure tabels are compiled
(message "updating internal tables...")
(reftex-compute-ref-cite-tables)
(message "updating internal tables...done")
(setq reftex-tables-dirty nil)

(provide 'reftex)

;;;============================================================================

;;; reftex.el ends here
