;;; org-exp.el --- ASCII, HTML, XOXO and iCalendar export for Org-mode

;; Copyright (C) 2004, 2005, 2006, 2007, 2008 Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten at orgmode dot org>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;; Version: 6.09a
;;
;; This file is part of GNU Emacs.
;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

(require 'org)
(require 'org-agenda)
(eval-and-compile
  (require 'cl))

(declare-function org-export-latex-preprocess "org-export-latex" ())
(declare-function org-agenda-skip "org-agenda" ())
(declare-function org-infojs-options-inbuffer-template "org-jsinfo" ())
(declare-function htmlize-region "ext:htmlize" (beg end))
(defvar htmlize-buffer-places)  ; from htmlize.el

(defgroup org-export nil
  "Options for exporting org-listings."
  :tag "Org Export"
  :group 'org)

(defgroup org-export-general nil
  "General options for exporting Org-mode files."
  :tag "Org Export General"
  :group 'org-export)

;; FIXME
(defvar org-export-publishing-directory nil)

(defcustom org-export-run-in-background nil
  "Non-nil means export and publishing commands will run in background.
This works by starting up a separate Emacs process visiting the same file
and doing the export from there.
Not all export commands are affected by this - only the ones which
actually write to a file, and that do not depend on the buffer state.

If this option is nil, you can still get background export by calling
`org-export' with a double prefix arg: `C-u C-u C-c C-e'.

If this option is t, the double prefix can be used to exceptionally
force an export command into the current process."
  :group 'org-export-general
  :type 'boolean)


(defcustom org-export-select-tags '("export")
  "Tags that select a tree for export.
If any such tag is found in a buffer, all trees that do not carry one
of these tags will be deleted before export.
Inside trees that are selected like this, you can still deselect a
subtree by tagging it with one of the `org-export-excude-tags'."
  :group 'org-export-general
  :type '(repeat (string :tag "Tag")))

(defcustom org-export-exclude-tags '("noexport")
  "Tags that exclude a tree from export.
All trees carrying any of these tags will be excluded from export.
This is without contition, so even subtrees inside that carry one of the
`org-export-select-tags' will be removed."
  :group 'org-export-general
  :type '(repeat (string :tag "Tag")))

(defcustom org-export-with-special-strings t
  "Non-nil means, interpret \"\-\", \"--\" and \"---\" for export.
When this option is turned on, these strings will be exported as:

  Org   HTML       LaTeX
 -----+----------+--------
  \\-    &shy;      \\-
  --    &ndash;    --
  ---   &mdash;    ---
  ...   &hellip;   \ldots

This option can also be set with the +OPTIONS line, e.g. \"-:nil\"."
  :group 'org-export-translation
  :type 'boolean)

(defcustom org-export-language-setup
  '(("en"  "Author"          "Date"  "Table of Contents")
    ("cs"  "Autor"           "Datum" "Obsah")
    ("da"  "Ophavsmand"      "Dato"  "Indhold")
    ("de"  "Autor"           "Datum" "Inhaltsverzeichnis")
    ("es"  "Autor"           "Fecha" "\xcdndice")
    ("fr"  "Auteur"          "Date"  "Table des mati\xe8res")
    ("it"  "Autore"          "Data"  "Indice")
    ("nl"  "Auteur"          "Datum" "Inhoudsopgave")
    ("no"  "Forfatter"       "Dato"  "Innhold")
    ("nb"  "Forfatter"       "Dato"  "Innhold")  ;; nb = Norsk (bokm.l)
    ("nn"  "Forfattar"       "Dato"  "Innhald")  ;; nn = Norsk (nynorsk)
    ("sv"  "F\xf6rfattarens" "Datum" "Inneh\xe5ll"))
  "Terms used in export text, translated to different languages.
Use the variable `org-export-default-language' to set the language,
or use the +OPTION lines for a per-file setting."
  :group 'org-export-general
  :type '(repeat
	  (list
	   (string :tag "HTML language tag")
	   (string :tag "Author")
	   (string :tag "Date")
	   (string :tag "Table of Contents"))))

(defcustom org-export-default-language "en"
  "The default language of HTML export, as a string.
This should have an association in `org-export-language-setup'."
  :group 'org-export-general
  :type 'string)

(defcustom org-export-skip-text-before-1st-heading nil
  "Non-nil means, skip all text before the first headline when exporting.
When nil, that text is exported as well."
  :group 'org-export-general
  :type 'boolean)

(defcustom org-export-headline-levels 3
  "The last level which is still exported as a headline.
Inferior levels will produce itemize lists when exported.
Note that a numeric prefix argument to an exporter function overrides
this setting.

This option can also be set with the +OPTIONS line, e.g. \"H:2\"."
  :group 'org-export-general
  :type 'number)

(defcustom org-export-with-section-numbers t
  "Non-nil means, add section numbers to headlines when exporting.

This option can also be set with the +OPTIONS line, e.g. \"num:t\"."
  :group 'org-export-general
  :type 'boolean)

(defcustom org-export-section-number-format '((("1" ".")) . "")
  "Format of section numbers for export.
The variable has two components.
1. A list of lists, each indicating a counter type and a separator.
   The counter type can be any of \"1\", \"A\", \"a\", \"I\", or \"a\".
   It causes causes numeric, alphabetic, or roman counters, respectively.
   The separator is only used if another counter for a subsection is being
   added.
   If there are more numbered section levels than entries in this lists,
   then the last entry will be reused.
2. A terminator string that will be added after the entire
   section number."
  :group 'org-export-general
  :type '(cons
	  (repeat
	   (list
	    (string :tag "Counter Type")
	    (string :tag "Separator   ")))
	  (string :tag "Terminator")))

(defcustom org-export-with-toc t
  "Non-nil means, create a table of contents in exported files.
The TOC contains headlines with levels up to`org-export-headline-levels'.
When an integer, include levels up to N in the toc, this may then be
different from `org-export-headline-levels', but it will not be allowed
to be larger than the number of headline levels.
When nil, no table of contents is made.

Headlines which contain any TODO items will be marked with \"(*)\" in
ASCII export, and with red color in HTML output, if the option
`org-export-mark-todo-in-toc' is set.

In HTML output, the TOC will be clickable.

This option can also be set with the +OPTIONS line, e.g. \"toc:nil\"
or \"toc:3\"."
  :group 'org-export-general
  :type '(choice
	  (const :tag "No Table of Contents" nil)
	  (const :tag "Full Table of Contents" t)
	  (integer :tag "TOC to level")))

(defcustom org-export-mark-todo-in-toc nil
  "Non-nil means, mark TOC lines that contain any open TODO items."
  :group 'org-export-general
  :type 'boolean)

(defcustom org-export-preserve-breaks nil
  "Non-nil means, preserve all line breaks when exporting.
Normally, in HTML output paragraphs will be reformatted.  In ASCII
export, line breaks will always be preserved, regardless of this variable.

This option can also be set with the +OPTIONS line, e.g. \"\\n:t\"."
  :group 'org-export-general
  :type 'boolean)

(defcustom org-export-with-archived-trees 'headline
  "Whether subtrees with the ARCHIVE tag should be exported.
This can have three different values
nil       Do not export, pretend this tree is not present
t         Do export the entire tree
headline  Only export the headline, but skip the tree below it."
  :group 'org-export-general
  :group 'org-archive
  :type '(choice
	  (const :tag "not at all" nil)
	  (const :tag "headline only" 'headline)
	  (const :tag "entirely" t)))

(defcustom org-export-author-info t
  "Non-nil means, insert author name and email into the exported file.

This option can also be set with the +OPTIONS line,
e.g. \"author-info:nil\"."
  :group 'org-export-general
  :type 'boolean)

(defcustom org-export-creator-info t
  "Non-nil means, the postamle should contain a creator sentence.
This sentence is \"HTML generated by org-mode XX in emacs XXX\"."
  :group 'org-export-general
  :type 'boolean)

(defcustom org-export-time-stamp-file t
  "Non-nil means, insert a time stamp into the exported file.
The time stamp shows when the file was created.

This option can also be set with the +OPTIONS line,
e.g. \"timestamp:nil\"."
  :group 'org-export-general
  :type 'boolean)

(defcustom org-export-with-timestamps t
  "If nil, do not export time stamps and associated keywords."
  :group 'org-export-general
  :type 'boolean)

(defcustom org-export-remove-timestamps-from-toc t
  "If nil, remove timestamps from the table of contents entries."
  :group 'org-export-general
  :type 'boolean)

(defcustom org-export-with-tags 'not-in-toc
  "If nil, do not export tags, just remove them from headlines.
If this is the symbol `not-in-toc', tags will be removed from table of
contents entries, but still be shown in the headlines of the document.

This option can also be set with the +OPTIONS line, e.g. \"tags:nil\"."
  :group 'org-export-general
  :type '(choice
	  (const :tag "Off" nil)
	  (const :tag "Not in TOC" not-in-toc)
	  (const :tag "On" t)))

(defcustom org-export-with-drawers nil
  "Non-nil means, export with drawers like the property drawer.
When t, all drawers are exported.  This may also be a list of
drawer names to export."
  :group 'org-export-general
  :type '(choice
	  (const :tag "All drawers" t)
	  (const :tag "None" nil)
	  (repeat :tag "Selected drawers"
		  (string :tag "Drawer name"))))

(defvar org-export-preprocess-hook nil
  "Hook for preprocessing an export buffer.
Pretty much the first thing when exporting is running this hook.")

(defgroup org-export-translation nil
  "Options for translating special ascii sequences for the export backends."
  :tag "Org Export Translation"
  :group 'org-export)

(defcustom org-export-with-emphasize t
  "Non-nil means, interpret *word*, /word/, and _word_ as emphasized text.
If the export target supports emphasizing text, the word will be
typeset in bold, italic, or underlined, respectively.  Works only for
single words, but you can say: I *really* *mean* *this*.
Not all export backends support this.

This option can also be set with the +OPTIONS line, e.g. \"*:nil\"."
  :group 'org-export-translation
  :type 'boolean)

(defcustom org-export-with-footnotes t
  "If nil, export [1] as a footnote marker.
Lines starting with [1] will be formatted as footnotes.

This option can also be set with the +OPTIONS line, e.g. \"f:nil\"."
  :group 'org-export-translation
  :type 'boolean)

(defcustom org-export-with-sub-superscripts t
  "Non-nil means, interpret \"_\" and \"^\" for export.
When this option is turned on, you can use TeX-like syntax for sub- and
superscripts.  Several characters after \"_\" or \"^\" will be
considered as a single item - so grouping with {} is normally not
needed.  For example, the following things will be parsed as single
sub- or superscripts.

 10^24   or   10^tau     several digits will be considered 1 item.
 10^-12  or   10^-tau    a leading sign with digits or a word
 x^2-y^3                 will be read as x^2 - y^3, because items are
			 terminated by almost any nonword/nondigit char.
 x_{i^2} or   x^(2-i)    braces or parenthesis do grouping.

Still, ambiguity is possible - so when in doubt use {} to enclose the
sub/superscript.  If you set this variable to the symbol `{}',
the braces are *required* in order to trigger interpretations as
sub/superscript.  This can be helpful in documents that need \"_\"
frequently in plain text.

Not all export backends support this, but HTML does.

This option can also be set with the +OPTIONS line, e.g. \"^:nil\"."
  :group 'org-export-translation
  :type '(choice
	  (const :tag "Always interpret" t)
	  (const :tag "Only with braces" {})
	  (const :tag "Never interpret" nil)))

(defcustom org-export-with-special-strings t
  "Non-nil means, interpret \"\-\", \"--\" and \"---\" for export.
When this option is turned on, these strings will be exported as:

\\-  : &shy;
--  : &ndash;
--- :  &mdash;

Not all export backends support this, but HTML does.

This option can also be set with the +OPTIONS line, e.g. \"-:nil\"."
  :group 'org-export-translation
  :type 'boolean)

(defcustom org-export-with-TeX-macros t
  "Non-nil means, interpret simple TeX-like macros when exporting.
For example, HTML export converts \\alpha to &alpha; and \\AA to &Aring;.
No only real TeX macros will work here, but the standard HTML entities
for math can be used as macro names as well.  For a list of supported
names in HTML export, see the constant `org-html-entities'.
Not all export backends support this.

This option can also be set with the +OPTIONS line, e.g. \"TeX:nil\"."
  :group 'org-export-translation
  :group 'org-export-latex
  :type 'boolean)

(defcustom org-export-with-LaTeX-fragments nil
  "Non-nil means, convert LaTeX fragments to images when exporting to HTML.
When set, the exporter will find LaTeX environments if the \\begin line is
the first non-white thing on a line.  It will also find the math delimiters
like $a=b$ and \\( a=b \\) for inline math,  $$a=b$$ and \\[ a=b \\] for
display math.

This option can also be set with the +OPTIONS line, e.g. \"LaTeX:t\"."
  :group 'org-export-translation
  :group 'org-export-latex
  :type 'boolean)

(defcustom org-export-with-fixed-width t
  "Non-nil means, lines starting with \":\" will be in fixed width font.
This can be used to have pre-formatted text, fragments of code etc.  For
example:
  : ;; Some Lisp examples
  : (while (defc cnt)
  :   (ding))
will be looking just like this in also HTML.  See also the QUOTE keyword.
Not all export backends support this.

This option can also be set with the +OPTIONS line, e.g. \"::nil\"."
  :group 'org-export-translation
  :type 'boolean)

(defcustom org-match-sexp-depth 3
  "Number of stacked braces for sub/superscript matching.
This has to be set before loading org.el to be effective."
  :group 'org-export-translation
  :type 'integer)

(defgroup org-export-tables nil
  "Options for exporting tables in Org-mode."
  :tag "Org Export Tables"
  :group 'org-export)

(defcustom org-export-with-tables t
  "If non-nil, lines starting with \"|\" define a table.
For example:

  | Name        | Address  | Birthday  |
  |-------------+----------+-----------|
  | Arthur Dent | England  | 29.2.2100 |

Not all export backends support this.

This option can also be set with the +OPTIONS line, e.g. \"|:nil\"."
  :group 'org-export-tables
  :type 'boolean)

(defcustom org-export-highlight-first-table-line t
  "Non-nil means, highlight the first table line.
In HTML export, this means use <th> instead of <td>.
In tables created with table.el, this applies to the first table line.
In Org-mode tables, all lines before the first horizontal separator
line will be formatted with <th> tags."
  :group 'org-export-tables
  :type 'boolean)

(defcustom org-export-table-remove-special-lines t
  "Remove special lines and marking characters in calculating tables.
This removes the special marking character column from tables that are set
up for spreadsheet calculations.  It also removes the entire lines
marked with `!', `_', or `^'.  The lines with `$' are kept, because
the values of constants may be useful to have."
  :group 'org-export-tables
  :type 'boolean)

(defcustom org-export-prefer-native-exporter-for-tables nil
  "Non-nil means, always export tables created with table.el natively.
Natively means, use the HTML code generator in table.el.
When nil, Org-mode's own HTML generator is used when possible (i.e. if
the table does not use row- or column-spanning).  This has the
advantage, that the automatic HTML conversions for math symbols and
sub/superscripts can be applied.  Org-mode's HTML generator is also
much faster."
  :group 'org-export-tables
  :type 'boolean)

(defgroup org-export-ascii nil
  "Options specific for ASCII export of Org-mode files."
  :tag "Org Export ASCII"
  :group 'org-export)

(defcustom org-export-ascii-underline '(?\$ ?\# ?^ ?\~ ?\= ?\-)
  "Characters for underlining headings in ASCII export.
In the given sequence, these characters will be used for level 1, 2, ..."
  :group 'org-export-ascii
  :type '(repeat character))

(defcustom org-export-ascii-bullets '(?* ?+ ?-)
  "Bullet characters for headlines converted to lists in ASCII export.
The first character is used for the first lest level generated in this
way, and so on.  If there are more levels than characters given here,
the list will be repeated.
Note that plain lists will keep the same bullets as the have in the
Org-mode file."
  :group 'org-export-ascii
  :type '(repeat character))

(defgroup org-export-xml nil
  "Options specific for XML export of Org-mode files."
  :tag "Org Export XML"
  :group 'org-export)

(defgroup org-export-html nil
  "Options specific for HTML export of Org-mode files."
  :tag "Org Export HTML"
  :group 'org-export)

(defcustom org-export-html-coding-system nil
  "Coding system for HTML export, defaults to buffer-file-coding-system."
  :group 'org-export-html
  :type 'coding-system)

(defcustom org-export-html-extension "html"
  "The extension for exported HTML files."
  :group 'org-export-html
  :type 'string)

(defcustom org-export-html-link-up ""
  "Where should the \"UP\" link of exported HTML pages lead?"
  :group 'org-export-html
  :type '(string :tag "File or URL"))

(defcustom org-export-html-link-home ""
  "Where should the \"HOME\" link of exported HTML pages lead?"
  :group 'org-export-html
  :type '(string :tag "File or URL"))

(defconst org-export-html-style-default
"<style type=\"text/css\">
  html { font-family: Times, serif; font-size: 12pt; }
  .title  { text-align: center; }
  .todo   { color: red; }
  .done   { color: green; }
  .tag    { background-color:lightblue; font-weight:normal }
  .target { }
  .timestamp { color: grey }
  .timestamp-kwd { color: CadetBlue }
  p.verse { margin-left: 3% }
  pre {
	border: 1pt solid #AEBDCC;
	background-color: #F3F5F7;
	padding: 5pt;
	font-family: courier, monospace;
        font-size: 90%;
        overflow:auto;
  }
  table { border-collapse: collapse; }
  td, th { vertical-align: top; }
  dt { font-weight: bold; }

  .org-info-js_info-navigation { border-style:none; }
  #org-info-js_console-label { font-size:10px; font-weight:bold;
                               white-space:nowrap; }
  .org-info-js_search-highlight {background-color:#ffff00; color:#000000;
                                 font-weight:bold; }

</style>"
  "The default style specification for exported HTML files.
Please use the variables `org-export-html-style' and
`org-export-html-style-extra' to add to this style.")

(defcustom org-export-html-style ""
  "Org-wide style definitions for exported HTML files.

This variable needs to contain the full HTML structure to provide a style,
including the surrounding HTML tags.  If you set the value of this variable,
you should consider to include definitions for the following classes:
 title, todo, done, timestamp, timestamp-kwd, tag, target.

For example, a valid value would be:

   <style type=\"text/css\">
       p { font-weight: normal; color: gray; }
       h1 { color: black; }
      .title { text-align: center; }
      .todo, .timestamp-kwd { color: red; }
      .done { color: green; }
   </style>

If you'd like to refer to en external style file, use something like

   <link rel=\"stylesheet\" type=\"text/css\" href=\"mystyles.css\">

As the value of this option simply gets inserted into the HTML <head> header,
you can \"misuse\" it to add arbitrary text to the header.
See also the variable `org-export-html-style-extra'."
  :group 'org-export-html
  :type 'string)
;;;###autoload
(put 'org-export-html-style 'safe-local-variable 'stringp)

(defcustom org-export-html-style-extra ""
  "Additional style information for HTML export.
The value of this variable is inserted into the HTML buffer right after
the value of `org-export-html-style'.  Use this variable for per-file
settings of style information, and do not forget to surround the style
settings with <style>...</style> tags."
  :group 'org-export-html
  :type 'string)
;;;###autoload
(put 'org-export-html-style-extra 'safe-local-variable 'stringp)


(defcustom org-export-html-title-format "<h1 class=\"title\">%s</h1>\n"
  "Format for typesetting the document title in HTML export."
  :group 'org-export-html
  :type 'string)

(defcustom org-export-html-toplevel-hlevel 2
  "The <H> level for level 1 headings in HTML export."
  :group 'org-export-html
  :type 'string)

(defcustom org-export-html-link-org-files-as-html t
  "Non-nil means, make file links to `file.org' point to `file.html'.
When org-mode is exporting an org-mode file to HTML, links to
non-html files are directly put into a href tag in HTML.
However, links to other Org-mode files (recognized by the
extension `.org.) should become links to the corresponding html
file, assuming that the linked org-mode file will also be
converted to HTML.
When nil, the links still point to the plain `.org' file."
  :group 'org-export-html
  :type 'boolean)

(defcustom org-export-html-inline-images 'maybe
  "Non-nil means, inline images into exported HTML pages.
This is done using an <img> tag.  When nil, an anchor with href is used to
link to the image.  If this option is `maybe', then images in links with
an empty description will be inlined, while images with a description will
be linked only."
  :group 'org-export-html
  :type '(choice (const :tag "Never" nil)
		 (const :tag "Always" t)
		 (const :tag "When there is no description" maybe)))

;; FIXME: rename
(defcustom org-export-html-expand t
  "Non-nil means, for HTML export, treat @<...> as HTML tag.
When nil, these tags will be exported as plain text and therefore
not be interpreted by a browser.

This option can also be set with the +OPTIONS line, e.g. \"@:nil\"."
  :group 'org-export-html
  :type 'boolean)

(defcustom org-export-html-table-tag
  "<table border=\"2\" cellspacing=\"0\" cellpadding=\"6\" rules=\"groups\" frame=\"hsides\">"
  "The HTML tag that is used to start a table.
This must be a <table> tag, but you may change the options like
borders and spacing."
  :group 'org-export-html
  :type 'string)

(defcustom org-export-table-header-tags '("<th>" . "</th>")
  "The opening tag for table header fields.
This is customizable so that alignment options can be specified."
  :group 'org-export-tables
  :type '(cons (string :tag "Opening tag") (string :tag "Closing tag")))

(defcustom org-export-table-data-tags '("<td>" . "</td>")
  "The opening tag for table data fields.
This is customizable so that alignment options can be specified."
  :group 'org-export-tables
  :type '(cons (string :tag "Opening tag") (string :tag "Closing tag")))

(defcustom org-export-html-with-timestamp nil
  "If non-nil, write `org-export-html-html-helper-timestamp'
into the exported HTML text.  Otherwise, the buffer will just be saved
to a file."
  :group 'org-export-html
  :type 'boolean)

(defcustom org-export-html-html-helper-timestamp
  "<br/><br/><hr><p><!-- hhmts start --> <!-- hhmts end --></p>\n"
  "The HTML tag used as timestamp delimiter for HTML-helper-mode."
  :group 'org-export-html
  :type 'string)

(defgroup org-export-htmlize nil
  "Options for processing examples with htmlize.el."
  :tag "Org Export Htmlize"
  :group 'org-export-html)

(defcustom org-export-htmlize-output-type 'inline-css
  "Output type to be used by htmlize when formatting code snippets.
Normally this is `inline-css', but if you have defined to appropriate
classes in your css style file, setting this to `css' means that the
fontification will use the class names.
See also the function `org-export-htmlize-generate-css'."
  :group 'org-export-htmlize
  :type '(choice (const css) (const inline-css)))

(defcustom org-export-htmlize-css-font-prefix "org-"
  "The prefix for CSS class names for htmlize font specifications."
  :group 'org-export-htmlize
  :type 'string)

(defgroup org-export-icalendar nil
  "Options specific for iCalendar export of Org-mode files."
  :tag "Org Export iCalendar"
  :group 'org-export)

(defcustom org-combined-agenda-icalendar-file "~/org.ics"
  "The file name for the iCalendar file covering all agenda files.
This file is created with the command \\[org-export-icalendar-all-agenda-files].
The file name should be absolute, the file will be overwritten without warning."
  :group 'org-export-icalendar
  :type 'file)

(defcustom org-icalendar-combined-name "OrgMode"
  "Calendar name for the combined iCalendar representing all agenda files."
  :group 'org-export-icalendar
  :type 'string)

(defcustom org-icalendar-use-deadline '(event-if-not-todo todo-due)
  "Contexts where iCalendar export should use a deadline time stamp.
This is a list with several symbols in it.  Valid symbol are:

event-if-todo       Deadlines in TODO entries become calendar events.
event-if-not-todo   Deadlines in non-TODO entries become calendar events.
todo-due            Use deadlines in TODO entries as due-dates"
  :group 'org-export-icalendar
  :type '(set :greedy t
	      (const :tag "Deadlines in non-TODO entries become events"
		     event-if-not-todo)
	      (const :tag "Deadline in TODO entries become events"
		     event-if-todo)
	      (const :tag "Deadlines in TODO entries become due-dates"
		     todo-due)))

(defcustom org-icalendar-use-scheduled '(todo-start)
  "Contexts where iCalendar export should use a scheduling time stamp.
This is a list with several symbols in it.  Valid symbol are:

event-if-todo       Scheduling time stamps in TODO entries become an event.
event-if-not-todo   Scheduling time stamps in non-TODO entries become an event.
todo-start          Scheduling time stamps in TODO entries become start date.
                    Some calendar applications show TODO entries only after
                    that date."
  :group 'org-export-icalendar
  :type '(set :greedy t
	      (const :tag
		     "SCHEDULED timestamps in non-TODO entries become events"
		     event-if-not-todo)
	      (const :tag "SCHEDULED timestamps in TODO entries become events"
		     event-if-todo)
	      (const :tag "SCHEDULED in TODO entries become start date"
		     todo-start)))

(defcustom org-icalendar-categories '(local-tags category)
  "Items that should be entered into the categories field.
This is a list of symbols, the following are valid:

category    The Org-mode category of the current file or tree
todo-state  The todo state, if any
local-tags  The tags, defined in the current line
all-tags    All tags, including inherited ones."
  :group 'org-export-icalendar
  :type '(repeat
	  (choice
	   (const :tag "The file or tree category" category)
	   (const :tag "The TODO state" todo-state)
	   (const :tag "Tags defined in current line" local-tags)
	   (const :tag "All tags, including inherited ones" all-tags))))

(defcustom org-icalendar-include-todo nil
  "Non-nil means, export to iCalendar files should also cover TODO items."
  :group 'org-export-icalendar
  :type '(choice
	  (const :tag "None" nil)
	  (const :tag "Unfinished" t)
	  (const :tag "All" all)))

(defcustom org-icalendar-include-sexps t
  "Non-nil means, export to iCalendar files should also cover sexp entries.
These are entries like in the diary, but directly in an Org-mode file."
  :group 'org-export-icalendar
  :type 'boolean)

(defcustom org-icalendar-include-body 100
  "Amount of text below headline to be included in iCalendar export.
This is a number of characters that should maximally be included.
Properties, scheduling and clocking lines will always be removed.
The text will be inserted into the DESCRIPTION field."
  :group 'org-export-icalendar
  :type '(choice
	  (const :tag "Nothing" nil)
	  (const :tag "Everything" t)
	  (integer :tag "Max characters")))

(defcustom org-icalendar-store-UID nil
  "Non-nil means, store any created UIDs in properties.
The iCalendar standard requires that all entries have a unique identifyer.
Org will create these identifiers as needed.  When this variable is non-nil,
the created UIDs will be stored in the ID property of the entry.  Then the
next time this entry is exported, it will be exported with the same UID,
superceeding the previous form of it.  This is essential for
synchronization services.
This variable is not turned on by default because we want to avoid creating
a property drawer in every entry if people are only playing with this feature,
or if they are only using it locally."
  :group 'org-export-icalendar
  :type 'boolean)

;;;; Exporting

;;; Variables, constants, and parameter plists

(defconst org-level-max 20)

(defvar org-export-html-preamble nil
  "Preamble, to be inserted just before <body>.  Set by publishing functions.")
(defvar org-export-html-postamble nil
  "Preamble, to be inserted just after </body>.  Set by publishing functions.")
(defvar org-export-html-auto-preamble t
  "Should default preamble be inserted?  Set by publishing functions.")
(defvar org-export-html-auto-postamble t
  "Should default postamble be inserted?  Set by publishing functions.")
(defvar org-current-export-file nil) ; dynamically scoped parameter
(defvar org-current-export-dir nil) ; dynamically scoped parameter

(defconst org-export-plist-vars
  '((:link-up              . org-export-html-link-up)
    (:link-home            . org-export-html-link-home)
    (:language             . org-export-default-language)
    (:customtime           . org-display-custom-times)
    (:headline-levels      . org-export-headline-levels)
    (:section-numbers      . org-export-with-section-numbers)
    (:section-number-format . org-export-section-number-format)
    (:table-of-contents    . org-export-with-toc)
    (:preserve-breaks      . org-export-preserve-breaks)
    (:archived-trees       . org-export-with-archived-trees)
    (:emphasize            . org-export-with-emphasize)
    (:sub-superscript      . org-export-with-sub-superscripts)
    (:special-strings      . org-export-with-special-strings)
    (:footnotes            . org-export-with-footnotes)
    (:drawers              . org-export-with-drawers)
    (:tags                 . org-export-with-tags)
    (:TeX-macros           . org-export-with-TeX-macros)
    (:LaTeX-fragments      . org-export-with-LaTeX-fragments)
    (:skip-before-1st-heading . org-export-skip-text-before-1st-heading)
    (:fixed-width          . org-export-with-fixed-width)
    (:timestamps           . org-export-with-timestamps)
    (:author-info          . org-export-author-info)
    (:creator-info         . org-export-creator-info)
    (:time-stamp-file      . org-export-time-stamp-file)
    (:tables               . org-export-with-tables)
    (:table-auto-headline  . org-export-highlight-first-table-line)
    (:style                . org-export-html-style)
    (:style-extra          . org-export-html-style-extra)
    (:agenda-style         . org-agenda-export-html-style)
    (:convert-org-links    . org-export-html-link-org-files-as-html)
    (:inline-images        . org-export-html-inline-images)
    (:html-extension       . org-export-html-extension)
    (:html-table-tag       . org-export-html-table-tag)
    (:expand-quoted-html   . org-export-html-expand)
    (:timestamp            . org-export-html-with-timestamp)
    (:publishing-directory . org-export-publishing-directory)
    (:preamble             . org-export-html-preamble)
    (:postamble            . org-export-html-postamble)
    (:auto-preamble        . org-export-html-auto-preamble)
    (:auto-postamble       . org-export-html-auto-postamble)
    (:author               . user-full-name)
    (:email                . user-mail-address)
    (:select-tags          . org-export-select-tags)
    (:exclude-tags         . org-export-exclude-tags)))

(defun org-default-export-plist ()
  "Return the property list with default settings for the export variables."
  (let ((l org-export-plist-vars) rtn e)
    (while (setq e (pop l))
      (setq rtn (cons (car e) (cons (symbol-value (cdr e)) rtn))))
    rtn))

(defvar org-export-inbuffer-options-extra nil
  "List of additional in-buffer options that should be detected.
Just before export, the buffer is scanned for options like #+TITLE, #+EMAIL,
etc.  Extensions can add to this list to get their options detected, and they
can then add a function to `org-export-options-filters' to process these
options.
Each element in this list must be a list, with the in-buffer keyword as car,
and a property (a symbol) as the next element.  All occurences of the
keyword will be found, the values concatenated with a space character
in between, and the result stored in the export options property list.")

(defvar org-export-options-filters nil
  "Functions to be called to finalize the export/publishing options.
All these options are stored in a property list, and each of the functions
in this hook gets a chance to modify this property list.  Each function
must accept the property list as an argument, and must return the (possibly
modified) list.")

(defun org-infile-export-plist ()
  "Return the property list with file-local settings for export."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((re (org-make-options-regexp
		 (append
		  '("TITLE" "AUTHOR" "DATE" "EMAIL" "TEXT" "OPTIONS" "LANGUAGE"
		    "LINK_UP" "LINK_HOME" "SETUPFILE" "STYLE" "LATEX_HEADER"
		    "EXPORT_SELECT_TAGS" "EXPORT_EXCLUDE_TAGS")
		  (mapcar 'car org-export-inbuffer-options-extra))))
	    p key val text options js-up js-main js-css js-opt a pr style
	    latex-header
	    ext-setup-or-nil setup-contents (start 0))
	(while (or (and ext-setup-or-nil
			(string-match re ext-setup-or-nil start)
			(setq start (match-end 0)))
		   (and (setq ext-setup-or-nil nil start 0)
			(re-search-forward re nil t)))
	  (setq key (upcase (org-match-string-no-properties 1 ext-setup-or-nil))
		val (org-match-string-no-properties 2 ext-setup-or-nil))
	  (cond
	   ((setq a (assoc key org-export-inbuffer-options-extra))
	    (setq pr (nth 1 a))
	    (setq p (plist-put p pr (concat (plist-get p pr) " " val))))
	   ((string-equal key "TITLE") (setq p (plist-put p :title val)))
	   ((string-equal key "AUTHOR")(setq p (plist-put p :author val)))
	   ((string-equal key "EMAIL") (setq p (plist-put p :email val)))
	   ((string-equal key "DATE") (setq p (plist-put p :date val)))
	   ((string-equal key "LANGUAGE") (setq p (plist-put p :language val)))
	   ((string-equal key "STYLE")
	    (setq style (concat style "\n" val)))
	   ((string-equal key "LATEX_HEADER")
	    (setq latex-header (concat latex-header "\n" val)))
	   ((string-equal key "TEXT")
	    (setq text (if text (concat text "\n" val) val)))
	   ((string-equal key "OPTIONS")
	    (setq options (concat val " " options)))
	   ((string-equal key "LINK_UP")
	    (setq p (plist-put p :link-up val)))
	   ((string-equal key "LINK_HOME")
	    (setq p (plist-put p :link-home val)))
	   ((string-equal key "EXPORT_SELECT_TAGS")
	    (setq p (plist-put p :select-tags (org-split-string val))))
	   ((string-equal key "EXPORT_EXCLUDE_TAGS")
	    (setq p (plist-put p :exclude-tags (org-split-string val))))
	   ((equal key "SETUPFILE")
	    (setq setup-contents (org-file-contents
				  (expand-file-name
				   (org-remove-double-quotes
				    (org-trim val)))
				  'noerror))
	    (if (not ext-setup-or-nil)
		(setq ext-setup-or-nil setup-contents start 0)
	      (setq ext-setup-or-nil
		    (concat (substring ext-setup-or-nil 0 start)
			    "\n" setup-contents "\n"
			    (substring ext-setup-or-nil start)))))))
	(setq p (plist-put p :text text))
	(when style (setq p (plist-put p :style-extra style)))
	(when latex-header
	  (setq p (plist-put p :latex-header-extra (substring latex-header 1))))
	(when options
	  (setq p (org-export-add-options-to-plist p options)))
	p))))

(defun org-export-add-options-to-plist (p options)
  "Parse an OPTONS line and set values in the property list P."
  (let (o)
    (when options
      (let ((op '(("H"     . :headline-levels)
		  ("num"   . :section-numbers)
		  ("toc"   . :table-of-contents)
		  ("\\n"   . :preserve-breaks)
		  ("@"     . :expand-quoted-html)
		  (":"     . :fixed-width)
		  ("|"     . :tables)
		  ("^"     . :sub-superscript)
		  ("-"     . :special-strings)
		  ("f"     . :footnotes)
		  ("d"     . :drawers)
		  ("tags"  . :tags)
		  ("*"     . :emphasize)
		  ("TeX"   . :TeX-macros)
		  ("LaTeX" . :LaTeX-fragments)
		  ("skip"  . :skip-before-1st-heading)
		  ("author" . :author-info)
		  ("creator" . :creator-info)
		  ("timestamp" . :time-stamp-file)))
	    o)
	(while (setq o (pop op))
	  (if (string-match (concat (regexp-quote (car o))
				    ":\\([^ \t\n\r;,.]*\\)")
			    options)
	      (setq p (plist-put p (cdr o)
				 (car (read-from-string
				       (match-string 1 options))))))))))
  p)
  
(defun org-export-add-subtree-options (p pos)
  "Add options in subtree at position POS to property list P."
  (save-excursion
    (goto-char pos)
    (when (org-at-heading-p)
      (let (a)
	;; This is actually read in `org-export-get-title-from-subtree'
	;; (when (setq a (org-entry-get pos "EXPORT_TITLE"))
	;;   (setq p (plist-put p :title a)))
	(when (setq a (org-entry-get pos "EXPORT_TEXT"))
	  (setq p (plist-put p :text a)))
	(when (setq a (org-entry-get pos "EXPORT_OPTIONS"))
	  (setq p (org-export-add-options-to-plist p a)))))
    p))

(defun org-export-directory (type plist)
  (let* ((val (plist-get plist :publishing-directory))
	 (dir (if (listp val)
		  (or (cdr (assoc type val)) ".")
		val)))
    dir))

(defun org-export-process-option-filters (plist)
  (let ((functions org-export-options-filters) f)
    (while (setq f (pop functions))
      (setq plist (funcall f plist))))
  plist)

;;;###autoload
(defun org-export (&optional arg)
  "Export dispatcher for Org-mode.
When `org-export-run-in-background' is non-nil, try to run the command
in the background.  This will be done only for commands that write
to a file.  For details see the docstring of `org-export-run-in-background'.

The prefix argument ARG will be passed to the exporter.  However, if
ARG is a double universal prefix `C-u C-u', that means to inverse the
value of `org-export-run-in-background'."
  (interactive "P")
  (let* ((bg (org-xor (equal arg '(16)) org-export-run-in-background))
	 (help "[t]   insert the export option template
\[v]   limit export to visible part of outline tree

\[a] export as ASCII

\[h] export as HTML
\[H] export as HTML to temporary buffer
\[R] export region as HTML
\[b] export as HTML and browse immediately
\[x] export as XOXO

\[l] export as LaTeX
\[L] export as LaTeX to temporary buffer

\[i] export current file as iCalendar file
\[I] export all agenda files as iCalendar files
\[c] export agenda files into combined iCalendar file

\[F] publish current file
\[P] publish current project
\[X] publish... (project will be prompted for)
\[A] publish all projects")
	 (cmds
	  '((?t org-insert-export-options-template nil)
	    (?v org-export-visible nil)
	    (?a org-export-as-ascii t)
	    (?h org-export-as-html t)
	    (?b org-export-as-html-and-open t)
	    (?H org-export-as-html-to-buffer nil)
	    (?R org-export-region-as-html nil)
	    (?x org-export-as-xoxo t)
	    (?l org-export-as-latex t)
	    (?L org-export-as-latex-to-buffer nil)
	    (?i org-export-icalendar-this-file t)
	    (?I org-export-icalendar-all-agenda-files t)
	    (?c org-export-icalendar-combine-agenda-files t)
	    (?F org-publish-current-file t)
	    (?P org-publish-current-project t)
	    (?X org-publish t)
	    (?A org-publish-all t)))
	 r1 r2 ass)
    (save-window-excursion
      (delete-other-windows)
      (with-output-to-temp-buffer "*Org Export/Publishing Help*"
	(princ help))
      (message "Select command: ")
      (setq r1 (read-char-exclusive)))
    (setq r2 (if (< r1 27) (+ r1 96) r1))
    (unless (setq ass (assq r2 cmds))
      (error "No command associated with key %c" r1))
    (if (and bg (nth 2 ass))
	;; execute in background
	(let ((p (start-process
		  (concat "Exporting " (file-name-nondirectory (buffer-file-name)))
		  "*Org Processes*"
		  (expand-file-name invocation-name invocation-directory)
		  "-batch"
		  "-l" user-init-file
		  "--eval" "(require 'org-exp)"
		  "--eval" "(setq org-wait .2)"
		  (buffer-file-name)
		  "-f" (symbol-name (nth 1 ass)))))
	  (set-process-sentinel p 'org-export-process-sentinel)
	  (message "Background process \"%s\": started" p))
      ;; background processing not requested, or not possible
      (call-interactively (nth 1 ass)))))

(defun org-export-process-sentinel (process status)
  (if (string-match "\n+\\'" status)
      (setq status (substring status 0 -1)))
  (message "Background process \"%s\": %s" process status))

(defconst org-html-entities
  '(("nbsp")
    ("iexcl")
    ("cent")
    ("pound")
    ("curren")
    ("yen")
    ("brvbar")
    ("vert" . "&#124;")
    ("sect")
    ("uml")
    ("copy")
    ("ordf")
    ("laquo")
    ("not")
    ("shy")
    ("reg")
    ("macr")
    ("deg")
    ("plusmn")
    ("sup2")
    ("sup3")
    ("acute")
    ("micro")
    ("para")
    ("middot")
    ("odot"."o")
    ("star"."*")
    ("cedil")
    ("sup1")
    ("ordm")
    ("raquo")
    ("frac14")
    ("frac12")
    ("frac34")
    ("iquest")
    ("Agrave")
    ("Aacute")
    ("Acirc")
    ("Atilde")
    ("Auml")
    ("Aring") ("AA"."&Aring;")
    ("AElig")
    ("Ccedil")
    ("Egrave")
    ("Eacute")
    ("Ecirc")
    ("Euml")
    ("Igrave")
    ("Iacute")
    ("Icirc")
    ("Iuml")
    ("ETH")
    ("Ntilde")
    ("Ograve")
    ("Oacute")
    ("Ocirc")
    ("Otilde")
    ("Ouml")
    ("times")
    ("Oslash")
    ("Ugrave")
    ("Uacute")
    ("Ucirc")
    ("Uuml")
    ("Yacute")
    ("THORN")
    ("szlig")
    ("agrave")
    ("aacute")
    ("acirc")
    ("atilde")
    ("auml")
    ("aring")
    ("aelig")
    ("ccedil")
    ("egrave")
    ("eacute")
    ("ecirc")
    ("euml")
    ("igrave")
    ("iacute")
    ("icirc")
    ("iuml")
    ("eth")
    ("ntilde")
    ("ograve")
    ("oacute")
    ("ocirc")
    ("otilde")
    ("ouml")
    ("divide")
    ("oslash")
    ("ugrave")
    ("uacute")
    ("ucirc")
    ("uuml")
    ("yacute")
    ("thorn")
    ("yuml")
    ("fnof")
    ("Alpha")
    ("Beta")
    ("Gamma")
    ("Delta")
    ("Epsilon")
    ("Zeta")
    ("Eta")
    ("Theta")
    ("Iota")
    ("Kappa")
    ("Lambda")
    ("Mu")
    ("Nu")
    ("Xi")
    ("Omicron")
    ("Pi")
    ("Rho")
    ("Sigma")
    ("Tau")
    ("Upsilon")
    ("Phi")
    ("Chi")
    ("Psi")
    ("Omega")
    ("alpha")
    ("beta")
    ("gamma")
    ("delta")
    ("epsilon")
    ("varepsilon"."&epsilon;")
    ("zeta")
    ("eta")
    ("theta")
    ("iota")
    ("kappa")
    ("lambda")
    ("mu")
    ("nu")
    ("xi")
    ("omicron")
    ("pi")
    ("rho")
    ("sigmaf") ("varsigma"."&sigmaf;")
    ("sigma")
    ("tau")
    ("upsilon")
    ("phi")
    ("chi")
    ("psi")
    ("omega")
    ("thetasym") ("vartheta"."&thetasym;")
    ("upsih")
    ("piv")
    ("bull") ("bullet"."&bull;")
    ("hellip") ("dots"."&hellip;")
    ("prime")
    ("Prime")
    ("oline")
    ("frasl")
    ("weierp")
    ("image")
    ("real")
    ("trade")
    ("alefsym")
    ("larr") ("leftarrow"."&larr;") ("gets"."&larr;")
    ("uarr") ("uparrow"."&uarr;")
    ("rarr") ("to"."&rarr;") ("rightarrow"."&rarr;")
    ("darr")("downarrow"."&darr;")
    ("harr") ("leftrightarrow"."&harr;")
    ("crarr") ("hookleftarrow"."&crarr;") ; has round hook, not quite CR
    ("lArr") ("Leftarrow"."&lArr;")
    ("uArr") ("Uparrow"."&uArr;")
    ("rArr") ("Rightarrow"."&rArr;")
    ("dArr") ("Downarrow"."&dArr;")
    ("hArr") ("Leftrightarrow"."&hArr;")
    ("forall")
    ("part") ("partial"."&part;")
    ("exist") ("exists"."&exist;")
    ("empty") ("emptyset"."&empty;")
    ("nabla")
    ("isin") ("in"."&isin;")
    ("notin")
    ("ni")
    ("prod")
    ("sum")
    ("minus")
    ("lowast") ("ast"."&lowast;")
    ("radic")
    ("prop") ("proptp"."&prop;")
    ("infin") ("infty"."&infin;")
    ("ang") ("angle"."&ang;")
    ("and") ("wedge"."&and;")
    ("or") ("vee"."&or;")
    ("cap")
    ("cup")
    ("int")
    ("there4")
    ("sim")
    ("cong") ("simeq"."&cong;")
    ("asymp")("approx"."&asymp;")
    ("ne") ("neq"."&ne;")
    ("equiv")
    ("le")
    ("ge")
    ("sub") ("subset"."&sub;")
    ("sup") ("supset"."&sup;")
    ("nsub")
    ("sube")
    ("supe")
    ("oplus")
    ("otimes")
    ("perp")
    ("sdot") ("cdot"."&sdot;")
    ("lceil")
    ("rceil")
    ("lfloor")
    ("rfloor")
    ("lang")
    ("rang")
    ("loz") ("Diamond"."&loz;")
    ("spades") ("spadesuit"."&spades;")
    ("clubs") ("clubsuit"."&clubs;")
    ("hearts") ("diamondsuit"."&hearts;")
    ("diams") ("diamondsuit"."&diams;")
    ("smile"."&#9786;") ("blacksmile"."&#9787;") ("sad"."&#9785;")
    ("quot")
    ("amp")
    ("lt")
    ("gt")
    ("OElig")
    ("oelig")
    ("Scaron")
    ("scaron")
    ("Yuml")
    ("circ")
    ("tilde")
    ("ensp")
    ("emsp")
    ("thinsp")
    ("zwnj")
    ("zwj")
    ("lrm")
    ("rlm")
    ("ndash")
    ("mdash")
    ("lsquo")
    ("rsquo")
    ("sbquo")
    ("ldquo")
    ("rdquo")
    ("bdquo")
    ("dagger")
    ("Dagger")
    ("permil")
    ("lsaquo")
    ("rsaquo")
    ("euro")

    ("arccos"."arccos")
    ("arcsin"."arcsin")
    ("arctan"."arctan")
    ("arg"."arg")
    ("cos"."cos")
    ("cosh"."cosh")
    ("cot"."cot")
    ("coth"."coth")
    ("csc"."csc")
    ("deg"."deg")
    ("det"."det")
    ("dim"."dim")
    ("exp"."exp")
    ("gcd"."gcd")
    ("hom"."hom")
    ("inf"."inf")
    ("ker"."ker")
    ("lg"."lg")
    ("lim"."lim")
    ("liminf"."liminf")
    ("limsup"."limsup")
    ("ln"."ln")
    ("log"."log")
    ("max"."max")
    ("min"."min")
    ("Pr"."Pr")
    ("sec"."sec")
    ("sin"."sin")
    ("sinh"."sinh")
    ("sup"."sup")
    ("tan"."tan")
    ("tanh"."tanh")
    )
  "Entities for TeX->HTML translation.
Entries can be like (\"ent\"), in which case \"\\ent\" will be translated to
\"&ent;\".  An entry can also be a dotted pair like (\"ent\".\"&other;\").
In that case, \"\\ent\" will be translated to \"&other;\".
The list contains HTML entities for Latin-1, Greek and other symbols.
It is supplemented by a number of commonly used TeX macros with appropriate
translations.  There is currently no way for users to extend this.")

;;; General functions for all backends

(defun org-export-preprocess-string (string &rest parameters)
  "Cleanup STRING so that that the true exported has a more consistent source.
This function takes STRING, which should be a buffer-string of an org-file
to export.  It then creates a temporary buffer where it does its job.
The result is then again returned as a string, and the exporter works
on this string to produce the exported version."
  (interactive)
  (let* ((htmlp (plist-get parameters :for-html))
	 (asciip (plist-get parameters :for-ascii))
	 (latexp (plist-get parameters :for-LaTeX))
	 (archived-trees (plist-get parameters :archived-trees))
	 (inhibit-read-only t)
	 (drawers org-drawers)
	 (outline-regexp "\\*+ ")
	 target-alist rtn)

    (with-current-buffer (get-buffer-create " org-mode-tmp")
      (erase-buffer)
      (insert string)
      (setq case-fold-search t)
      ;; Call the hook
      (run-hooks 'org-export-preprocess-hook)

      ;; Remove license-to-kill stuff
      ;; The caller markes some stuff fo killing, stuff that has been
      ;; used to create the page title, for example.
      (org-export-kill-licensed-text)
      
      (let ((org-inhibit-startup t)) (org-mode))
      (setq case-fold-search t)
      (untabify (point-min) (point-max))
      
      ;; Handle include files
      (org-export-handle-include-files)
      
      ;; Get rid of excluded trees
      (org-export-handle-export-tags (plist-get parameters :select-tags)
				     (plist-get parameters :exclude-tags))

      ;; Handle source code snippets
      (org-export-replace-src-segments)
      
      ;; Get rid of drawers
      (org-export-remove-or-extract-drawers drawers
					    (plist-get parameters :drawers))
      
      ;; Get the correct stuff before the first headline
      (when (plist-get parameters :skip-before-1st-heading)
	(goto-char (point-min))
	(when (re-search-forward "^\\*+[ \t]" nil t)
	  (delete-region (point-min) (match-beginning 0))
	  (goto-char (point-min))
	  (insert "\n")))
      (when (plist-get parameters :add-text)
	(goto-char (point-min))
	(insert (plist-get parameters :add-text) "\n"))
      
      ;; Get rid of archived trees
      (org-export-remove-archived-trees archived-trees)
      
      ;; Find all headings and compute the targets for them
      (setq target-alist (org-export-define-heading-targets target-alist))

      ;; Find targets in comments and move them out of comments,
      ;; but mark them as targets that should be invisible
      (setq target-alist (org-export-handle-invisible-targets target-alist))

      ;; Protect examples
      (org-export-protect-examples (if asciip 'indent nil))

      ;; Protect backend specific stuff, throw away the others.
      (org-export-select-backend-specific-text
       (cond (htmlp 'html) (latexp 'latex) (asciip 'ascii)))

      ;; Protect quoted subtrees
      (org-export-protect-quoted-subtrees)

      ;; Protect verbatim elements
      (org-export-protect-verbatim)

      ;; Blockquotes and verse
      (org-export-mark-blockquote-and-verse)

      ;; Remove comment environment and comment subtrees
      (org-export-remove-comment-blocks-and-subtrees)


      ;; Find matches for radio targets and turn them into internal links
      (org-export-mark-radio-links)

      ;; Find all links that contain a newline and put them into a single line
      (org-export-concatenate-multiline-links)

      ;; Normalize links: Convert angle and plain links into bracket links
      ;; and expand link abbreviations
      (org-export-normalize-links)

      ;; Find all internal links.  If they have a fuzzy match (i.e. not
      ;; a *dedicated* target match, let the link  point to the
      ;; corresponding section.
      (org-export-target-internal-links target-alist)

      ;; Find multiline emphasis and put them into single line
      (when (plist-get parameters :emph-multiline)
	(org-export-concatenate-multiline-emphasis))

      ;; Remove special table lines
      (when org-export-table-remove-special-lines
	(org-export-remove-special-table-lines))

      ;; Specific LaTeX stuff
      (when latexp
	(require 'org-export-latex nil)
	(org-export-latex-preprocess))

      ;; Specific ASCII stuff
      (when asciip
	(org-export-ascii-preprocess))

      ;; Specific HTML stuff
      (when htmlp
	(org-export-html-preprocess parameters))

      ;; Remove or replace comments
      (org-export-handle-comments (plist-get parameters :comments))


      (setq rtn (buffer-string)))
    (kill-buffer " org-mode-tmp")
    rtn))

(defun org-export-kill-licensed-text ()
  "Remove all text that is marked with a :org-license-to-kill property."
  (let (p q)
    (while (setq p (text-property-any (point-min) (point-max)
				      :org-license-to-kill t))
      (delete-region
       p (or (next-single-property-change p :org-license-to-kill)
	     (point-max))))))

(defun org-export-define-heading-targets (target-alist)
  "Find all headings and define the targets for them.
The new targets are added to TARGET-ALIST, which is also returned."
  (goto-char (point-min))
  (org-init-section-numbers)
  (let ((re (concat "^" org-outline-regexp))
	level target)
    (while (re-search-forward re nil t)
      (setq level (org-reduced-level
		   (save-excursion (goto-char (point-at-bol))
				   (org-outline-level))))
      (setq target (org-solidify-link-text
		    (format "sec-%s" (org-section-number level))))
      (push (cons target target) target-alist)
      (add-text-properties
       (point-at-bol) (point-at-eol)
       (list 'target target))))
  target-alist)

(defun org-export-handle-invisible-targets (target-alist)
  "Find targets in comments and move them out of comments.
Mark them as invisible targets."
  (let (target tmp)
    (goto-char (point-min))
    (while (re-search-forward "^#.*?\\(<<<?\\([^>\r\n]+\\)>>>?\\).*" nil t)
      ;; Check if the line before or after is a headline with a target
      (if (setq target (or (get-text-property (point-at-bol 0) 'target)
			   (get-text-property (point-at-bol 2) 'target)))
	  (progn
	    ;; use the existing target in a neighboring line
	    (setq tmp (match-string 2))
	    (replace-match "")
	    (and (looking-at "\n") (delete-char 1))
	    (push (cons (org-solidify-link-text tmp) target)
		  target-alist))
	;; Make an invisible target
	(replace-match "\\1(INVISIBLE)"))))
  target-alist)

(defun org-export-target-internal-links (target-alist)
  "Find all internal links and assign target to them.
If a link has a fuzzy match (i.e. not a *dedicated* target match),
let the link  point to the corresponding section."
  (goto-char (point-min))
  (while (re-search-forward org-bracket-link-regexp nil t)
    (org-if-unprotected
     (let* ((md (match-data))
	    (desc (match-end 2))
	    (link (org-link-unescape (match-string 1)))
	    (slink (org-solidify-link-text link))
	    found props pos
	    (target
	     (cond
	      ((cdr (assoc slink target-alist)))
	      ((string-match org-link-types-re link) nil)
	      ((or (file-name-absolute-p link)
		   (string-match "^\\." link))
	       nil)
	      (t
	       (save-excursion
		 (setq found (condition-case nil (org-link-search link)
			       (error nil)))
		 (when (and found
			    (or (org-on-heading-p)
				(not (eq found 'dedicated))))
		   (or (get-text-property (point) 'target)
		       (get-text-property
			(max (point-min)
			     (1- (previous-single-property-change
				  (point) 'target)))
			'target))))))))
       (when target
	 (set-match-data md)
	 (goto-char (match-beginning 1))
	 (setq props (text-properties-at (point)))
	 (delete-region (match-beginning 1) (match-end 1))
	 (setq pos (point))
	 (insert target)
	 (unless desc (insert "][" link))
	 (add-text-properties pos (point) props))))))

(defun org-export-remove-or-extract-drawers (all-drawers exp-drawers)
  "Remove drawers, or extract the content.
ALL-DRAWERS is a list of all drawer names valid in the current buffer.
EXP-DRAWERS can be t to keep all drawer contents, or a list of drawers
whose content to keep."
  (unless (eq t exp-drawers)
    (goto-char (point-min))
    (let ((re (concat "^[ \t]*:\\("
		      (mapconcat
		       'identity
		       (org-delete-all exp-drawers
				       (copy-sequence all-drawers))
		       "\\|")
		      "\\):[ \t]*\n\\([^@]*?\n\\)?[ \t]*:END:[ \t]*\n")))
      (while (re-search-forward re nil t)
	(replace-match "")))))

(defun org-export-handle-export-tags (select-tags exclude-tags)
  "Modify the buffer, honoring SELECT-TAGS and EXCLUDE-TAGS.
Both arguments are lists of tags.
If any of SELECT-TAGS is found, all trees not marked by a SELECT-TAG
will be removed.
After that, all subtrees that are marked by EXCLUDE-TAGS will be
removed as well."
  (remove-text-properties (point-min) (point-max) '(:org-delete t))
  (let* ((re-sel (concat ":\\(" (mapconcat 'regexp-quote
					   select-tags "\\|")
			 "\\):"))
	 (re-excl (concat ":\\(" (mapconcat 'regexp-quote
					   exclude-tags "\\|")
			"\\):"))
	 beg end cont)
    (goto-char (point-min))
    (when (and select-tags
	       (re-search-forward
		(concat "^\\*+[ \t].*" re-sel "[^ \t\n]*[ \t]*$") nil t))
      ;; At least one tree is marked for export, this means
      ;; all the unmarked stuff needs to go.
      ;; Dig out the trees that should be exported
      (goto-char (point-min))
      (outline-next-heading)
      (setq beg (point))
      (put-text-property beg (point-max) :org-delete t)
      (while (re-search-forward re-sel nil t)
	(when (org-on-heading-p)
	  (org-back-to-heading)
	  (remove-text-properties
	   (max (1- (point)) (point-min))
	   (setq cont (save-excursion (org-end-of-subtree t t)))
	   '(:org-delete t))
	  (while (and (org-up-heading-safe)
		      (get-text-property (point) :org-delete))
	    (remove-text-properties (max (1- (point)) (point-min))
				    (point-at-eol) '(:org-delete t)))
	  (goto-char cont))))
    ;; Remove the trees explicitly marked for noexport
    (when exclude-tags
      (goto-char (point-min))
      (while (re-search-forward re-excl nil t)
	(when (org-at-heading-p)
	  (org-back-to-heading t)
	  (setq beg (point))
	  (org-end-of-subtree t)
	  (delete-region beg (point)))))
    ;; Remove everything that is now still marked for deletion
    (goto-char (point-min))
    (while (setq beg (text-property-any (point-min) (point-max) :org-delete t))
      (setq end (or (next-single-property-change beg :org-delete)
		    (point-max)))
      (delete-region beg end))))

(defun org-export-remove-archived-trees (export-archived-trees)
  "Remove archived trees.
When EXPORT-ARCHIVED-TREES is `headline;, only the headline will be exported.
When it is t, the entire archived tree will be exported.
When it is nil the entire tree including the headline will be removed
from the buffer."
  (let ((re-archive (concat ":" org-archive-tag ":"))
	a b)
    (when (not (eq export-archived-trees t))
      (goto-char (point-min))
      (while (re-search-forward re-archive nil t)
	(if (not (org-on-heading-p t))
	    (org-end-of-subtree t)
	  (beginning-of-line 1)
	  (setq a (if export-archived-trees
		      (1+ (point-at-eol)) (point))
		b (org-end-of-subtree t))
	  (if (> b a) (delete-region a b)))))))

(defun org-export-protect-quoted-subtrees ()
  "Mark quoted subtrees with the protection property."
  (let ((re-quote (concat "^\\*+[ \t]+" org-quote-string "\\>")))
    (goto-char (point-min))
    (while (re-search-forward re-quote nil t)
      (goto-char (match-beginning 0))
      (end-of-line 1)
      (add-text-properties (point) (org-end-of-subtree t)
			   '(org-protected t)))))

(defun org-export-protect-verbatim ()
  "Mark verbatim snippets with the protection property."
  (goto-char (point-min))
  (while (re-search-forward org-verbatim-re nil t)
    (add-text-properties (match-beginning 4) (match-end 4)
			 '(org-protected t))
    (goto-char (1+ (match-end 4)))))

(defun org-export-protect-examples (&optional indent)
  "Protect code that should be exported as monospaced examples."
  (goto-char (point-min))
  (while (re-search-forward "^#\\+BEGIN_EXAMPLE[ \t]*\n" nil t)
    (goto-char (match-end 0))
    (while (and (not (looking-at "#\\+END_EXAMPLE")) (not (eobp)))
      (insert (if indent ":  " ":"))
      (beginning-of-line 2)))
  (goto-char (point-min))
  (while (re-search-forward "^[ \t]*:.*\\(\n[ \t]*:.*\\)*" nil t)
    (add-text-properties (match-beginning 0) (match-end 0)
			 '(org-protected t))))

(defun org-export-select-backend-specific-text (backend)
  (let ((formatters
	 '((html "HTML" "BEGIN_HTML" "END_HTML")
	   (ascii "ASCII" "BEGIN_ASCII" "END_ASCII")
	   (latex "LaTeX" "BEGIN_LaTeX" "END_LaTeX")))
	fmt)

    (while formatters
      (setq fmt (pop formatters))
      (when (eq (car fmt) backend)
	;; This is selected code, put it into the file for real
	(goto-char (point-min))
	(while (re-search-forward (concat "^#\\+" (cadr fmt)
					  ":[ \t]*\\(.*\\)") nil t)
	  (replace-match "\\1" t)
	  (add-text-properties
	   (point-at-bol) (min (1+ (point-at-eol)) (point-max))
	   '(org-protected t))))
      (goto-char (point-min))
      (while (re-search-forward
	      (concat "^#\\+"
		      (caddr fmt) "\\>.*\\(\\(\n.*\\)*?\n\\)#\\+"
		      (cadddr fmt) "\\>.*\n?") nil t)
	(if (eq (car fmt) backend)
	    ;; yes, keep this
	    (add-text-properties (match-beginning 1) (1+ (match-end 1))
				 '(org-protected t))
	  ;; No, this is for a different backend, kill it
	  (delete-region (match-beginning 0) (match-end 0)))))))

(defun org-export-mark-blockquote-and-verse ()
  "Mark block quote and verse environments with special cookies.
These special cookies will later be interpreted by the backend."
  ;; Blockquotes
  (goto-char (point-min))
  (while (re-search-forward "^#\\+\\(begin\\|end\\)_\\(block\\)?quote\\>.*"
			    nil t)
    (replace-match (if (equal (downcase (match-string 1)) "end")
		       "ORG-BLOCKQUOTE-END" "ORG-BLOCKQUOTE-START")
		   t t))
  ;; Verse
  (goto-char (point-min))
  (while (re-search-forward "^#\\+\\(begin\\|end\\)_verse\\>.*" nil t)
    (replace-match (if (equal (downcase (match-string 1)) "end")
		       "ORG-VERSE-END" "ORG-VERSE-START")
		   t t)))

(defun org-export-remove-comment-blocks-and-subtrees ()
  "Remove the comment environment, and also commented subtrees."
  (let ((re-commented (concat "^\\*+[ \t]+" org-comment-string "\\>"))
        (case-fold-search nil))
    ;; Remove comment environment
    (goto-char (point-min))
    (while (re-search-forward
	    "^#\\+BEGIN_COMMENT[ \t]*\n[^\000]*?^#\\+END_COMMENT\\>.*" nil t)
      (replace-match "" t t))
    ;; Remove subtrees that are commented
    (goto-char (point-min))
    (while (re-search-forward re-commented nil t)
      (goto-char (match-beginning 0))
      (delete-region (point) (org-end-of-subtree t)))))

(defun org-export-handle-comments (commentsp)
  "Remove comments, or convert to backend-specific format.
COMMENTSP can be a format string for publishing comments.
When it is nil, all comments will be removed."
  (let ((re "^#\\(.*\n?\\)")
	pos)
    (goto-char (point-min))
    (while (or (looking-at re)
	       (re-search-forward re nil t))
      (setq pos (match-beginning 0))
      (if commentsp
	  (progn (add-text-properties
		  (match-beginning 0) (match-end 0) '(org-protected t))
		 (replace-match (format commentsp (match-string 1)) t t))
	(goto-char (1+ pos))
	(org-if-unprotected
	 (replace-match "")
	 (goto-char (max (point-min) (1- pos))))))))

(defun org-export-mark-radio-links ()
  "Find all matches for radio targets and turn them into internal links."
  (let ((re-radio (and org-target-link-regexp
		       (concat "\\([^<]\\)\\(" org-target-link-regexp "\\)"))))
    (goto-char (point-min))
    (when re-radio
      (while (re-search-forward re-radio nil t)
	(org-if-unprotected
	 (replace-match "\\1[[\\2]]"))))))

(defun org-export-remove-special-table-lines ()
  "Remove tables lines that are used for internal purposes."
  (goto-char (point-min))
  (while (re-search-forward "^[ \t]*|" nil t)
    (beginning-of-line 1)
    (if (or (looking-at "[ \t]*| *[!_^] *|")
	    (and (looking-at ".*?| *<[0-9]+> *|")
		 (not (looking-at ".*?| *[^ <|]"))))
	(delete-region (max (point-min) (1- (point-at-bol)))
		       (point-at-eol))
      (end-of-line 1))))

(defun org-export-normalize-links ()
  "Convert all links to bracket links, and expand link abbreviations."
  (let ((re-plain-link (concat "\\([^[<]\\)" org-plain-link-re))
	(re-angle-link (concat "\\([^[]\\)" org-angle-link-re)))
    (goto-char (point-min))
    (while (re-search-forward re-plain-link nil t)
      (goto-char (1- (match-end 0)))
      (org-if-unprotected
       (let* ((s (concat (match-string 1) "[[" (match-string 2)
			 ":" (match-string 3) "]]")))
	 ;; added 'org-link face to links
	 (put-text-property 0 (length s) 'face 'org-link s)
	 (replace-match s t t))))
    (goto-char (point-min))
    (while (re-search-forward re-angle-link nil t)
      (goto-char (1- (match-end 0)))
      (org-if-unprotected
       (let* ((s (concat (match-string 1) "[[" (match-string 2)
			 ":" (match-string 3) "]]")))
	 (put-text-property 0 (length s) 'face 'org-link s)
	 (replace-match s t t))))
    (goto-char (point-min))
    (while (re-search-forward org-bracket-link-regexp nil t)
      (goto-char (1- (match-end 0)))
      (org-if-unprotected
       (let* ((xx (save-match-data
		    (org-link-expand-abbrev (match-string 1))))
	      (s (concat
		  "[[" xx "]"
		  (if (match-end 3)
		      (match-string 2)
		    (concat "[" xx "]"))
		  "]")))
	 (put-text-property 0 (length s) 'face 'org-link s)
	 (replace-match s t t))))))
  
(defun org-export-concatenate-multiline-links ()
  "Find multi-line links and put it all into a single line.
This is to make sure that the line-processing export backends
can work correctly."
  (goto-char (point-min))
  (while (re-search-forward "\\(\\(\\[\\|\\]\\)\\[[^]]*?\\)[ \t]*\n[ \t]*\\([^]]*\\]\\(\\[\\|\\]\\)\\)" nil t)
    (org-if-unprotected
     (replace-match "\\1 \\3")
     (goto-char (match-beginning 0)))))

(defun org-export-concatenate-multiline-emphasis ()
  "Find multi-line emphasis and put it all into a single line.
This is to make sure that the line-processing export backends
can work correctly."
  (goto-char (point-min))
  (while (re-search-forward org-emph-re nil t)
    (if (not (= (char-after (match-beginning 3))
		(char-after (match-beginning 4))))
	(org-if-unprotected
	 (subst-char-in-region (match-beginning 0) (match-end 0)
			       ?\n ?\  t)
	 (goto-char (1- (match-end 0))))
      (goto-char (1+ (match-beginning 0))))))

(defun org-export-grab-title-from-buffer ()
  "Get a title for the current document, from looking at the buffer."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (let ((end (if (looking-at org-outline-regexp)
		     (point)
		   (save-excursion (outline-next-heading) (point)))))
	(when (re-search-forward "^[ \t]*[^|# \t\r\n].*\n" end t)
	  ;; Mark the line so that it will not be exported as normal text.
	  (org-unmodified
	   (add-text-properties (match-beginning 0) (match-end 0)
				(list :org-license-to-kill t)))
	  ;; Return the title string
	  (org-trim (match-string 0)))))))

(defun org-export-get-title-from-subtree ()
  "Return subtree title and exclude it from export."
  (let (title (m (mark)) (rbeg (region-beginning)) (rend (region-end)))
    (save-excursion
      (goto-char rbeg)
      (when (and (org-at-heading-p)
		 (>= (org-end-of-subtree t t) rend))
	;; This is a subtree, we take the title from the first heading
	(goto-char rbeg)
	(looking-at org-todo-line-regexp)
	(setq title (match-string 3))
	(org-unmodified
	 (add-text-properties (point) (1+ (point-at-eol))
			      (list :org-license-to-kill t)))
	(setq title (or (org-entry-get nil "EXPORT_TITLE") title))))
    title))

(defun org-solidify-link-text (s &optional alist)
  "Take link text and make a safe target out of it."
  (save-match-data
    (let* ((rtn
	    (mapconcat
	     'identity
	     (org-split-string s "[ \t\r\n]+") "=="))
	   (a (assoc rtn alist)))
      (or (cdr a) rtn))))

(defun org-get-min-level (lines)
  "Get the minimum level in LINES."
  (let ((re "^\\(\\*+\\) ") l min)
    (catch 'exit
      (while (setq l (pop lines))
	(if (string-match re l)
	    (throw 'exit (org-tr-level (length (match-string 1 l))))))
      1)))

;; Variable holding the vector with section numbers
(defvar org-section-numbers (make-vector org-level-max 0))

(defun org-init-section-numbers ()
  "Initialize the vector for the section numbers."
  (let* ((level  -1)
	 (numbers (nreverse (org-split-string "" "\\.")))
	 (depth (1- (length org-section-numbers)))
	 (i depth) number-string)
    (while (>= i 0)
      (if (> i level)
	  (aset org-section-numbers i 0)
	(setq number-string (or (car numbers) "0"))
	(if (string-match "\\`[A-Z]\\'" number-string)
	    (aset org-section-numbers i
		  (- (string-to-char number-string) ?A -1))
	  (aset org-section-numbers i (string-to-number number-string)))
	(pop numbers))
      (setq i (1- i)))))

(defun org-section-number (&optional level)
  "Return a string with the current section number.
When LEVEL is non-nil, increase section numbers on that level."
  (let* ((depth (1- (length org-section-numbers)))
	 (string "")
	 (fmts (car org-export-section-number-format))
	 (term (cdr org-export-section-number-format))
	 (sep "")
	 ctype fmt idx n)
    (when level
      (when (> level -1)
	(aset org-section-numbers
	      level (1+ (aref org-section-numbers level))))
      (setq idx (1+ level))
      (while (<= idx depth)
	(if (not (= idx 1))
	    (aset org-section-numbers idx 0))
	(setq idx (1+ idx))))
    (setq idx 0)
    (while (<= idx depth)
      (when (> (aref org-section-numbers idx) 0)
	(setq fmt (or (pop fmts) fmt)
	      ctype (car fmt)
	      n (aref org-section-numbers idx)
	      string (if (> n 0)
			 (concat string sep (org-number-to-counter n ctype))
		       (concat string ".0"))
	      sep (nth 1 fmt)))
      (setq idx (1+ idx)))
    (save-match-data
      (if (string-match "\\`\\([@0]\\.\\)+" string)
	  (setq string (replace-match "" t nil string)))
      (if (string-match "\\(\\.0\\)+\\'" string)
	  (setq string (replace-match "" t nil string))))
    (concat string term)))

(defun org-number-to-counter (n type)
  "Concert number N to a string counter, according to TYPE.
TYPE must be a string, any of:
 1  number
 A  A,B,....
 a  a,b,....
 I  uppper case roman numeral
 i  lower case roman numeral"
  (cond
   ((equal type "1") (number-to-string n))
   ((equal type "A") (char-to-string (+ ?A n -1)))
   ((equal type "a") (char-to-string (+ ?a n -1)))
   ((equal type "I") (org-number-to-roman n))
   ((equal type "i") (downcase (org-number-to-roman n)))
   (t (error "Invalid counter type `%s'" type))))

(defun org-number-to-roman (n)
  "Convert integer N into a roman numeral."
  (let ((roman '((1000 . "M") (900 . "CM") (500 . "D") (400 . "CD")
		 ( 100 . "C") ( 90 . "XC") ( 50 . "L") ( 40 . "XL")
		 (  10 . "X") (  9 . "IX") (  5 . "V") (  4 . "IV")
		 (   1 . "I")))
	(res ""))
    (if (<= n 0)
	(number-to-string n)
      (while roman
	(if (>= n (caar roman))
	    (setq n (- n (caar roman))
		  res (concat res (cdar roman)))
	  (pop roman)))
      res)))

(org-number-to-roman 1961)
   

;;; Include files

(defun org-export-handle-include-files ()
  "Include the contents of include files, with proper formatting."
  (let ((case-fold-search t)
	params file markup lang start end prefix prefix1)
    (goto-char (point-min))
    (while (re-search-forward "^#\\+INCLUDE:?[ \t]+\\(.*\\)" nil t)
      (setq params (read (concat "(" (match-string 1) ")"))
	    prefix (org-get-and-remove-property 'params :prefix)
	    prefix1 (org-get-and-remove-property 'params :prefix1)
	    file (org-symname-or-string (pop params))
	    markup (org-symname-or-string (pop params))
	    lang (org-symname-or-string (pop params)))
      (delete-region (match-beginning 0) (match-end 0))
      (if (or (not file)
	      (not (file-exists-p file))
	      (not (file-readable-p file)))
	  (insert (format "CANNOT INCLUDE FILE %s" file))
	(when markup
	  (if (equal (downcase markup) "src")
	      (setq start (format "#+begin_src %s\n" (or lang "fundamental"))
		    end "#+end_src")
	    (setq start (format "#+begin_%s\n" markup)
		  end  (format "#+end_%s" markup))))
	(insert (or start ""))
	(insert (org-get-file-contents (expand-file-name file) prefix prefix1))
	(or (bolp) (newline))
	(insert (or end ""))))))

(defun org-get-file-contents (file &optional prefix prefix1)
  "Get the contents of FILE and return them as a string.
If PREFIX is a string, prepend it to each line.  If PREFIX1
is a string, prepend it to the first line instead of PREFIX."
  (with-temp-buffer
    (insert-file-contents file)
    (when (or prefix prefix1)
      (goto-char (point-min))
      (while (not (eobp))
	(insert (or prefix1 prefix))
	(setq prefix1 nil)
	(beginning-of-line 2)))
    (buffer-string)))

(defun org-get-and-remove-property (listvar prop)
  "Check if the value of LISTVAR contains PROP as a property.
If yes, return the value of that property (i.e. the element following
in the list) and remove property and value from the list in LISTVAR."
  (let ((list (symbol-value listvar)) m v)
    (when (setq m (member prop list))
      (setq v (nth 1 m))
      (if (equal (car list) prop)
	  (set listvar (cddr list))
	(setcdr (nthcdr (- (length list) (length m) 1) list)
		(cddr m))
	(set listvar list)))
    v))

(defun org-symname-or-string (s)
  (if (symbolp s)
      (if s (symbol-name s) s)
    s))

;;; Fontification of code
;; Currently only for the HTML backend, but who knows....
(defun org-export-replace-src-segments ()
  "Replace source code segments with special code for export."
  (let ((case-fold-search t)
	lang code trans)
    (goto-char (point-min))
    (while (re-search-forward
	    "^#\\+BEGIN_SRC:?[ \t]+\\([^ \t\n]+\\)[ \t]*\n\\([^\000]+?\n\\)#\\+END_SRC.*"
	    nil t)
      (setq lang (match-string 1) code (match-string 2)
	    trans (org-export-format-source-code lang code))
      (replace-match trans t t))))

(defvar htmlp)  ;; dynamically scoped from org-exp.el

(defun org-export-format-source-code (lang code)
  "Format CODE from language LANG and return it formatted for export.
Currently, this only does something for HTML export, for all other
backends, it converts the segment into an EXAMPLE segment."
  (save-match-data
    (cond
     (htmlp
      ;; We are exporting to HTML
      (condition-case nil (require 'htmlize) (nil t))
      (if (not (fboundp 'htmlize-region-for-paste))
	  (progn
	    ;; we do not have htmlize.el, or an old version of it
	    (message
	     "htmlize.el 1.34 or later is needed for source code formatting")
	    (concat "#+BEGIN_EXAMPLE\n" code
		    (if (string-match "\n\\'" code) "" "\n")
		    "#+END_EXAMPLE\n"))
	;; ok, we are good to go
	(let* ((mode (and lang (intern (concat lang "-mode"))))
	       (org-inhibit-startup t)
	       (org-startup-folded nil)
	       (htmltext
		(with-temp-buffer
		  (insert code)
		  ;; Free up the protected stuff
		  (goto-char (point-min))
		  (while (re-search-forward "^," nil t)
		    (replace-match "")
		    (end-of-line 1))
		  (if (functionp mode)
		      (funcall mode)
		    (fundamental-mode))
		  (font-lock-fontify-buffer)
		  (org-export-htmlize-region-for-paste
		   (point-min) (point-max)))))
	  (if (string-match "<pre\\([^>]*\\)>\n?" htmltext)
	      (setq htmltext (replace-match
			      (format "<pre class=\"src src-%s\">" lang)
			      t t htmltext)))
	  (concat "#+BEGIN_HTML\n" htmltext "\n#+END_HTML\n"))))
     (t
      ;; This is not HTML, so just make it an example.
      (when (equal lang "org")
	(while (string-match "^," code)
	  (setq code (replace-match "" t t code))))
      (concat "#+BEGIN_EXAMPLE\n" code
	      (if (string-match "\n\\'" code) "" "\n")
	      "#+END_EXAMPLE\n")))))

;;; ASCII export

(defvar org-last-level nil) ; dynamically scoped variable
(defvar org-min-level nil) ; dynamically scoped variable
(defvar org-levels-open nil) ; dynamically scoped parameter
(defvar org-ascii-current-indentation nil) ; For communication

;;;###autoload
(defun org-export-as-ascii (arg)
  "Export the outline as a pretty ASCII file.
If there is an active region, export only the region.
The prefix ARG specifies how many levels of the outline should become
underlined headlines.  The default is 3."
  (interactive "P")
  (setq-default org-todo-line-regexp org-todo-line-regexp)
  (let* ((opt-plist (org-combine-plists (org-default-export-plist)
					(org-infile-export-plist)))
	 (region-p (org-region-active-p))
	 (rbeg (and region-p (region-beginning)))
	 (rend (and region-p (region-end)))
	 (subtree-p
	  (when region-p
	    (save-excursion
	      (goto-char rbeg)
	      (and (org-at-heading-p)
		   (>= (org-end-of-subtree t t) rend)))))
	 (opt-plist (if subtree-p 
			(org-export-add-subtree-options opt-plist rbeg)
		      opt-plist))
	 (custom-times org-display-custom-times)
	 (org-ascii-current-indentation '(0 . 0))
	 (level 0) line txt
	 (umax nil)
	 (umax-toc nil)
	 (case-fold-search nil)
         (filename (concat (file-name-as-directory
			    (org-export-directory :ascii opt-plist))
			   (file-name-sans-extension
			    (or (and subtree-p
				     (org-entry-get (region-beginning)
						    "EXPORT_FILE_NAME" t))
				(file-name-nondirectory buffer-file-name)))
			   ".txt"))
	 (filename (if (equal (file-truename filename)
			      (file-truename buffer-file-name))
		       (concat filename ".txt")
		     filename))
	 (buffer (find-file-noselect filename))
	 (org-levels-open (make-vector org-level-max nil))
	 (odd org-odd-levels-only)
	 (date  (plist-get opt-plist :date))
	 (author      (plist-get opt-plist :author))
	 (title       (or (and subtree-p (org-export-get-title-from-subtree))
			  (plist-get opt-plist :title)
			  (and (not
				(plist-get opt-plist :skip-before-1st-heading))
			       (org-export-grab-title-from-buffer))
			  (file-name-sans-extension
			   (file-name-nondirectory buffer-file-name))))
	 (email       (plist-get opt-plist :email))
	 (language    (plist-get opt-plist :language))
	 (quote-re0   (concat "^[ \t]*" org-quote-string "\\>"))
;	 (quote-re    (concat "^\\(\\*+\\)\\([ \t]*" org-quote-string "\\>\\)"))
	 (todo nil)
	 (lang-words nil)
	 (region
	  (buffer-substring
	   (if (org-region-active-p) (region-beginning) (point-min))
	   (if (org-region-active-p) (region-end) (point-max))))
	 (lines (org-split-string
		 (org-export-preprocess-string
		  region
		  :for-ascii t
		  :skip-before-1st-heading
		  (plist-get opt-plist :skip-before-1st-heading)
		  :drawers (plist-get opt-plist :drawers)
		  :verbatim-multiline t
		  :select-tags (plist-get opt-plist :select-tags)
		  :exclude-tags (plist-get opt-plist :exclude-tags)
		  :archived-trees
		  (plist-get opt-plist :archived-trees)
		  :add-text (plist-get opt-plist :text))
		 "\n"))
	 thetoc have-headings first-heading-pos
	 table-open table-buffer)

    (let ((inhibit-read-only t))
      (org-unmodified
       (remove-text-properties (point-min) (point-max)
			       '(:org-license-to-kill t))))

    (setq org-min-level (org-get-min-level lines))
    (setq org-last-level org-min-level)
    (org-init-section-numbers)

    (find-file-noselect filename)

    (setq lang-words (or (assoc language org-export-language-setup)
			 (assoc "en" org-export-language-setup)))
    (switch-to-buffer-other-window buffer)
    (erase-buffer)
    (fundamental-mode)
    ;; create local variables for all options, to make sure all called
    ;; functions get the correct information
    (mapc (lambda (x)
	    (set (make-local-variable (cdr x))
		 (plist-get opt-plist (car x))))
	  org-export-plist-vars)
    (org-set-local 'org-odd-levels-only odd)
    (setq umax (if arg (prefix-numeric-value arg)
		 org-export-headline-levels))
    (setq umax-toc (if (integerp org-export-with-toc)
		       (min org-export-with-toc umax)
		     umax))

    ;; File header
    (if title (org-insert-centered title ?=))
    (insert "\n")
    (if (and (or author email)
	     org-export-author-info)
	(insert (concat (nth 1 lang-words) ": " (or author "")
			(if email (concat " <" email ">") "")
			"\n")))

    (cond
     ((and date (string-match "%" date))
      (setq date (format-time-string date)))
     (date)
     (t (setq date (format-time-string "%Y-%m-%d %T %Z"))))

    (if (and date org-export-time-stamp-file)
	(insert (concat (nth 2 lang-words) ": " date"\n")))

    (insert "\n\n")

    (if org-export-with-toc
	(progn
	  (push (concat (nth 3 lang-words) "\n") thetoc)
	  (push (concat (make-string (string-width (nth 3 lang-words)) ?=)
			"\n") thetoc)
	  (mapc '(lambda (line)
		   (if (string-match org-todo-line-regexp
				     line)
		       ;; This is a headline
		       (progn
			 (setq have-headings t)
			 (setq level (- (match-end 1) (match-beginning 1))
			       level (org-tr-level level)
			       txt (match-string 3 line)
			       todo
			       (or (and org-export-mark-todo-in-toc
					(match-beginning 2)
					(not (member (match-string 2 line)
						     org-done-keywords)))
					; TODO, not DONE
				   (and org-export-mark-todo-in-toc
					(= level umax-toc)
					(org-search-todo-below
					 line lines level))))
			 (setq txt (org-html-expand-for-ascii txt))

			 (while (string-match org-bracket-link-regexp txt)
			   (setq txt
				 (replace-match
				  (match-string (if (match-end 2) 3 1) txt)
				  t t txt)))

			 (if (and (memq org-export-with-tags '(not-in-toc nil))
				  (string-match
				   (org-re "[ \t]+:[[:alnum:]_@:]+:[ \t]*$")
				   txt))
			     (setq txt (replace-match "" t t txt)))
			 (if (string-match quote-re0 txt)
			     (setq txt (replace-match "" t t txt)))

			 (if org-export-with-section-numbers
			     (setq txt (concat (org-section-number level)
					       " " txt)))
			 (if (<= level umax-toc)
			     (progn
			       (push
				(concat
				 (make-string
				  (* (max 0 (- level org-min-level)) 4) ?\ )
				 (format (if todo "%s (*)\n" "%s\n") txt))
				thetoc)
			       (setq org-last-level level))
			   ))))
		lines)
	  (setq thetoc (if have-headings (nreverse thetoc) nil))))

    (org-init-section-numbers)
    (while (setq line (pop lines))
      ;; Remove the quoted HTML tags.
      (setq line (org-html-expand-for-ascii line))
      ;; Remove targets
      (while (string-match "<<<?[^<>]*>>>?[ \t]*\n?" line)
	(setq line (replace-match "" t t line)))
      ;; Replace internal links
      (while (string-match org-bracket-link-regexp line)
	(setq line (replace-match
		    (if (match-end 3) "[\\3]" "[\\1]")
		    t nil line)))
      (when custom-times
	(setq line (org-translate-time line)))
      (cond
       ((string-match "^\\(\\*+\\)[ \t]+\\(.*\\)" line)
	;; a Headline
	(setq first-heading-pos (or first-heading-pos (point)))
	(setq level (org-tr-level (- (match-end 1) (match-beginning 1)))
	      txt (match-string 2 line))
	(org-ascii-level-start level txt umax lines))

       ((and org-export-with-tables
	     (string-match "^\\([ \t]*\\)\\(|\\|\\+-+\\+\\)" line))
	(if (not table-open)
	    ;; New table starts
	    (setq table-open t table-buffer nil))
	;; Accumulate lines
	(setq table-buffer (cons line table-buffer))
	(when (or (not lines)
		  (not (string-match "^\\([ \t]*\\)\\(|\\|\\+-+\\+\\)"
				     (car lines))))
	  (setq table-open nil
		table-buffer (nreverse table-buffer))
	  (insert (mapconcat
		   (lambda (x)
		     (org-fix-indentation x org-ascii-current-indentation))
		   (org-format-table-ascii table-buffer)
		   "\n") "\n")))
       (t
	(setq line (org-fix-indentation line org-ascii-current-indentation))
	(if (and org-export-with-fixed-width
		 (string-match "^\\([ \t]*\\)\\(:\\)" line))
	    (setq line (replace-match "\\1" nil nil line)))
	(insert line "\n"))))

    (normal-mode)

    ;; insert the table of contents
    (when thetoc
      (goto-char (point-min))
      (if (re-search-forward "^[ \t]*\\[TABLE-OF-CONTENTS\\][ \t]*$" nil t)
	  (progn
	    (goto-char (match-beginning 0))
	    (replace-match ""))
	(goto-char first-heading-pos))
      (mapc 'insert thetoc)
      (or (looking-at "[ \t]*\n[ \t]*\n")
	  (insert "\n\n")))

    ;; Convert whitespace place holders
    (goto-char (point-min))
    (let (beg end)
      (while (setq beg (next-single-property-change (point) 'org-whitespace))
	(setq end (next-single-property-change beg 'org-whitespace))
	(goto-char beg)
	(delete-region beg end)
	(insert (make-string (- end beg) ?\ ))))

    (save-buffer)
    ;; remove display and invisible chars
    (let (beg end)
      (goto-char (point-min))
      (while (setq beg (next-single-property-change (point) 'display))
	(setq end (next-single-property-change beg 'display))
	(delete-region beg end)
	(goto-char beg)
	(insert "=>"))
      (goto-char (point-min))
      (while (setq beg (next-single-property-change (point) 'org-cwidth))
	(setq end (next-single-property-change beg 'org-cwidth))
	(delete-region beg end)
	(goto-char beg)))
    (goto-char (point-min))))

(defun org-export-ascii-preprocess ()
  "Do extra work for ASCII export"
  (goto-char (point-min))
  (while (re-search-forward org-verbatim-re nil t)
    (goto-char (match-end 2))
    (backward-delete-char 1) (insert "'")
    (goto-char (match-beginning 2))
    (delete-char 1) (insert "`")
    (goto-char (match-end 2))))

(defun org-search-todo-below (line lines level)
  "Search the subtree below LINE for any TODO entries."
  (let ((rest (cdr (memq line lines)))
	(re org-todo-line-regexp)
	line lv todo)
    (catch 'exit
      (while (setq line (pop rest))
	(if (string-match re line)
	    (progn
	      (setq lv (- (match-end 1) (match-beginning 1))
		    todo (and (match-beginning 2)
			      (not (member (match-string 2 line)
					  org-done-keywords))))
					; TODO, not DONE
	      (if (<= lv level) (throw 'exit nil))
	      (if todo (throw 'exit t))))))))

(defun org-html-expand-for-ascii (line)
  "Handle quoted HTML for ASCII export."
  (if org-export-html-expand
      (while (string-match "@<[^<>\n]*>" line)
	;; We just remove the tags for now.
	(setq line (replace-match "" nil nil line))))
  line)

(defun org-insert-centered (s &optional underline)
  "Insert the string S centered and underline it with character UNDERLINE."
  (let ((ind (max (/ (- fill-column (string-width s)) 2) 0)))
    (insert (make-string ind ?\ ) s "\n")
    (if underline
	(insert (make-string ind ?\ )
		(make-string (string-width s) underline)
		"\n"))))

(defun org-ascii-level-start (level title umax &optional lines)
  "Insert a new level in ASCII export."
  (let (char (n (- level umax 1)) (ind 0))
    (if (> level umax)
	(progn
	  (insert (make-string (* 2 n) ?\ )
		  (char-to-string (nth (% n (length org-export-ascii-bullets))
				       org-export-ascii-bullets))
		  " " title "\n")
	  ;; find the indentation of the next non-empty line
	  (catch 'stop
	    (while lines
	      (if (string-match "^\\* " (car lines)) (throw 'stop nil))
	      (if (string-match "^\\([ \t]*\\)\\S-" (car lines))
		  (throw 'stop (setq ind (org-get-indentation (car lines)))))
	      (pop lines)))
	  (setq org-ascii-current-indentation (cons (* 2 (1+ n)) ind)))
      (if (or (not (equal (char-before) ?\n))
	      (not (equal (char-before (1- (point))) ?\n)))
	  (insert "\n"))
      (setq char (nth (- umax level) (reverse org-export-ascii-underline)))
      (unless org-export-with-tags
	(if (string-match (org-re "[ \t]+\\(:[[:alnum:]_@:]+:\\)[ \t]*$") title)
	    (setq title (replace-match "" t t title))))
      (if org-export-with-section-numbers
	  (setq title (concat (org-section-number level) " " title)))
      (insert title "\n" (make-string (string-width title) char) "\n")
      (setq org-ascii-current-indentation '(0 . 0)))))

;;;###autoload
(defun org-export-visible (type arg)
  "Create a copy of the visible part of the current buffer, and export it.
The copy is created in a temporary buffer and removed after use.
TYPE is the final key (as a string) that also select the export command in
the `C-c C-e' export dispatcher.
As a special case, if the you type SPC at the prompt, the temporary
org-mode file will not be removed but presented to you so that you can
continue to use it.  The prefix arg ARG is passed through to the exporting
command."
  (interactive
   (list (progn
	   (message "Export visible: [a]SCII  [h]tml  [b]rowse HTML [H/R]uffer with HTML  [x]OXO  [ ]keep buffer")
	   (read-char-exclusive))
	 current-prefix-arg))
  (if (not (member type '(?a ?\C-a ?b ?\C-b ?h ?x ?\ )))
      (error "Invalid export key"))
  (let* ((binding (cdr (assoc type
			      '((?a . org-export-as-ascii)
				(?\C-a . org-export-as-ascii)
				(?b . org-export-as-html-and-open)
				(?\C-b . org-export-as-html-and-open)
				(?h . org-export-as-html)
				(?H . org-export-as-html-to-buffer)
				(?R . org-export-region-as-html)
				(?x . org-export-as-xoxo)))))
	 (keepp (equal type ?\ ))
	 (file buffer-file-name)
	 (buffer (get-buffer-create "*Org Export Visible*"))
	 s e)
    ;; Need to hack the drawers here.
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-drawer-regexp nil t)
	(goto-char (match-beginning 1))
	(or (org-invisible-p) (org-flag-drawer nil))))
    (with-current-buffer buffer (erase-buffer))
    (save-excursion
      (setq s (goto-char (point-min)))
      (while (not (= (point) (point-max)))
	(goto-char (org-find-invisible))
	(append-to-buffer buffer s (point))
	(setq s (goto-char (org-find-visible))))
      (org-cycle-hide-drawers 'all)
      (goto-char (point-min))
      (unless keepp
	;; Copy all comment lines to the end, to make sure #+ settings are
	;; still available for the second export step.  Kind of a hack, but
	;; does do the trick.
	(if (looking-at "#[^\r\n]*")
	    (append-to-buffer buffer (match-beginning 0) (1+ (match-end 0))))
	(while (re-search-forward "[\n\r]#[^\n\r]*" nil t)
	  (append-to-buffer buffer (1+ (match-beginning 0))
			    (min (point-max) (1+ (match-end 0))))))
      (set-buffer buffer)
      (let ((buffer-file-name file)
	    (org-inhibit-startup t))
	(org-mode)
	(show-all)
	(unless keepp (funcall binding arg))))
    (if (not keepp)
	(kill-buffer buffer)
      (switch-to-buffer-other-window buffer)
      (goto-char (point-min)))))

(defun org-find-visible ()
  (let ((s (point)))
    (while (and (not (= (point-max) (setq s (next-overlay-change s))))
		(get-char-property s 'invisible)))
    s))
(defun org-find-invisible ()
  (let ((s (point)))
    (while (and (not (= (point-max) (setq s (next-overlay-change s))))
		(not (get-char-property s 'invisible))))
    s))

;;; HTML export

(defvar org-archive-location)  ;; gets loades with the org-archive require.
(defun org-get-current-options ()
  "Return a string with current options as keyword options.
Does include HTML export options as well as TODO and CATEGORY stuff."
  (require 'org-archive)
  (format
   "#+TITLE:     %s
#+AUTHOR:    %s
#+EMAIL:     %s
#+DATE:      %s
#+LANGUAGE:  %s
#+OPTIONS:   H:%d num:%s toc:%s \\n:%s @:%s ::%s |:%s ^:%s -:%s f:%s *:%s TeX:%s LaTeX:%s skip:%s d:%s tags:%s
%s
#+EXPORT_SELECT_TAGS: %s
#+EXPORT_EXCUDE_TAGS: %s
#+LINK_UP:   %s
#+LINK_HOME: %s
#+CATEGORY:  %s
#+SEQ_TODO:  %s
#+TYP_TODO:  %s
#+PRIORITIES: %c %c %c
#+DRAWERS:   %s
#+STARTUP:   %s %s %s %s %s
#+TAGS:      %s
#+FILETAGS:  %s
#+ARCHIVE:   %s
#+LINK:      %s
"
   (buffer-name) (user-full-name) user-mail-address
   (format-time-string (substring (car org-time-stamp-formats) 1 -1))
   org-export-default-language
   org-export-headline-levels
   org-export-with-section-numbers
   org-export-with-toc
   org-export-preserve-breaks
   org-export-html-expand
   org-export-with-fixed-width
   org-export-with-tables
   org-export-with-sub-superscripts
   org-export-with-special-strings
   org-export-with-footnotes
   org-export-with-emphasize
   org-export-with-TeX-macros
   org-export-with-LaTeX-fragments
   org-export-skip-text-before-1st-heading
   org-export-with-drawers
   org-export-with-tags
   (if (featurep 'org-jsinfo) (org-infojs-options-inbuffer-template) "")
   (mapconcat 'identity org-export-select-tags " ")
   (mapconcat 'identity org-export-exclude-tags " ")
   org-export-html-link-up
   org-export-html-link-home
   (file-name-nondirectory buffer-file-name)
   "TODO FEEDBACK VERIFY DONE"
   "Me Jason Marie DONE"
   org-highest-priority org-lowest-priority org-default-priority
   (mapconcat 'identity org-drawers " ")
   (cdr (assoc org-startup-folded
	       '((nil . "showall") (t . "overview") (content . "content"))))
   (if org-odd-levels-only "odd" "oddeven")
   (if org-hide-leading-stars "hidestars" "showstars")
   (if org-startup-align-all-tables "align" "noalign")
   (cond ((eq org-log-done t) "logdone")
	 ((equal org-log-done 'note) "lognotedone")
	 ((not org-log-done) "nologdone"))
   (or (mapconcat (lambda (x)
		    (cond
		     ((equal '(:startgroup) x) "{")
		     ((equal '(:endgroup) x) "}")
		     ((cdr x) (format "%s(%c)" (car x) (cdr x)))
		     (t (car x))))
		  (or org-tag-alist (org-get-buffer-tags)) " ") "")
   (mapconcat 'identity org-file-tags " ")
   org-archive-location
   "org file:~/org/%s.org"
   ))

(defun org-export-html-preprocess (parameters)
  ;; Convert LaTeX fragments to images
  (when (plist-get parameters :LaTeX-fragments)
    (org-format-latex
     (concat "ltxpng/" (file-name-sans-extension
			(file-name-nondirectory
			 org-current-export-file)))
     org-current-export-dir nil "Creating LaTeX image %s"))
  (message "Exporting..."))

;;;###autoload
(defun org-insert-export-options-template ()
  "Insert into the buffer a template with information for exporting."
  (interactive)
  (if (not (bolp)) (newline))
  (let ((s (org-get-current-options)))
    (and (string-match "#\\+CATEGORY" s)
	 (setq s (substring s 0 (match-beginning 0))))
    (insert s)))

;;;###autoload
(defun org-export-as-html-and-open (arg)
  "Export the outline as HTML and immediately open it with a browser.
If there is an active region, export only the region.
The prefix ARG specifies how many levels of the outline should become
headlines.  The default is 3.  Lower levels will become bulleted lists."
  (interactive "P")
  (org-export-as-html arg 'hidden)
  (org-open-file buffer-file-name))

;;;###autoload
(defun org-export-as-html-batch ()
  "Call `org-export-as-html', may be used in batch processing as
emacs 	--batch
	--load=$HOME/lib/emacs/org.el
	--eval \"(setq org-export-headline-levels 2)\"
	--visit=MyFile --funcall org-export-as-html-batch"
  (org-export-as-html org-export-headline-levels 'hidden))

;;;###autoload
(defun org-export-as-html-to-buffer (arg)
  "Call `org-exort-as-html` with output to a temporary buffer.
No file is created.  The prefix ARG is passed through to `org-export-as-html'."
  (interactive "P")
  (org-export-as-html arg nil nil "*Org HTML Export*")
  (switch-to-buffer-other-window "*Org HTML Export*"))

;;;###autoload
(defun org-replace-region-by-html (beg end)
  "Assume the current region has org-mode syntax, and convert it to HTML.
This can be used in any buffer.  For example, you could write an
itemized list in org-mode syntax in an HTML buffer and then use this
command to convert it."
  (interactive "r")
  (let (reg html buf pop-up-frames)
    (save-window-excursion
      (if (org-mode-p)
	  (setq html (org-export-region-as-html
		      beg end t 'string))
	(setq reg (buffer-substring beg end)
	      buf (get-buffer-create "*Org tmp*"))
	(with-current-buffer buf
	  (erase-buffer)
	  (insert reg)
	  (org-mode)
	  (setq html (org-export-region-as-html
		      (point-min) (point-max) t 'string)))
	(kill-buffer buf)))
    (delete-region beg end)
    (insert html)))

;;;###autoload
(defun org-export-region-as-html (beg end &optional body-only buffer)
  "Convert region from BEG to END in org-mode buffer to HTML.
If prefix arg BODY-ONLY is set, omit file header, footer, and table of
contents, and only produce the region of converted text, useful for
cut-and-paste operations.
If BUFFER is a buffer or a string, use/create that buffer as a target
of the converted HTML.  If BUFFER is the symbol `string', return the
produced HTML as a string and leave not buffer behind.  For example,
a Lisp program could call this function in the following way:

  (setq html (org-export-region-as-html beg end t 'string))

When called interactively, the output buffer is selected, and shown
in a window.  A non-interactive call will only return the buffer."
  (interactive "r\nP")
  (when (interactive-p)
    (setq buffer "*Org HTML Export*"))
  (let ((transient-mark-mode t) (zmacs-regions t)
	ext-plist rtn)
    (setq ext-plist (plist-put ext-plist :ignore-subree-p t))
    (goto-char end)
    (set-mark (point)) ;; to activate the region
    (goto-char beg)
    (setq rtn (org-export-as-html
	       nil nil ext-plist
	       buffer body-only))
    (if (fboundp 'deactivate-mark) (deactivate-mark))
    (if (and (interactive-p) (bufferp rtn))
	(switch-to-buffer-other-window rtn)
      rtn)))

(defvar html-table-tag nil) ; dynamically scoped into this.
;;;###autoload
(defun org-export-as-html (arg &optional hidden ext-plist
			       to-buffer body-only pub-dir)
  "Export the outline as a pretty HTML file.
If there is an active region, export only the region.  The prefix
ARG specifies how many levels of the outline should become
headlines.  The default is 3.  Lower levels will become bulleted
lists.  When HIDDEN is non-nil, don't display the HTML buffer.
EXT-PLIST is a property list with external parameters overriding
org-mode's default settings, but still inferior to file-local
settings.  When TO-BUFFER is non-nil, create a buffer with that
name and export to that buffer.  If TO-BUFFER is the symbol
`string', don't leave any buffer behind but just return the
resulting HTML as a string.  When BODY-ONLY is set, don't produce
the file header and footer, simply return the content of
<body>...</body>, without even the body tags themselves.  When
PUB-DIR is set, use this as the publishing directory."
  (interactive "P")

  ;; Make sure we have a file name when we need it.
  (when (and (not (or to-buffer body-only))
	     (not buffer-file-name))
    (if (buffer-base-buffer)
	(org-set-local 'buffer-file-name
		       (with-current-buffer (buffer-base-buffer)
			 buffer-file-name))
      (error "Need a file name to be able to export.")))

  (message "Exporting...")
  (setq-default org-todo-line-regexp org-todo-line-regexp)
  (setq-default org-deadline-line-regexp org-deadline-line-regexp)
  (setq-default org-done-keywords org-done-keywords)
  (setq-default org-maybe-keyword-time-regexp org-maybe-keyword-time-regexp)
  (let* ((opt-plist
	  (org-export-process-option-filters
	   (org-combine-plists (org-default-export-plist)
			       ext-plist
			       (org-infile-export-plist))))

	 (style (concat org-export-html-style-default
			(plist-get opt-plist :style)
			(plist-get opt-plist :style-extra)))
	 (html-extension (plist-get opt-plist :html-extension))
	 (link-validate (plist-get opt-plist :link-validation-function))
	 valid thetoc have-headings first-heading-pos
	 (odd org-odd-levels-only)
	 (region-p (org-region-active-p))
	 (rbeg (and region-p (region-beginning)))
	 (rend (and region-p (region-end)))
	 (subtree-p
	  (if (plist-get opt-plist :ignore-subree-p)
	      nil
	    (when region-p
	      (save-excursion
		(goto-char rbeg)
		(and (org-at-heading-p)
		     (>= (org-end-of-subtree t t) rend))))))
	 (opt-plist (if subtree-p 
			(org-export-add-subtree-options opt-plist rbeg)
		      opt-plist))
	 ;; The following two are dynamically scoped into other
	 ;; routines below.
	 (org-current-export-dir
	  (or pub-dir (org-export-directory :html opt-plist)))
	 (org-current-export-file buffer-file-name)
         (level 0) (line "") (origline "") txt todo
         (umax nil)
         (umax-toc nil)
         (filename (if to-buffer nil
		     (expand-file-name
		      (concat
		       (file-name-sans-extension
			(or (and subtree-p
				 (org-entry-get (region-beginning)
						"EXPORT_FILE_NAME" t))
			    (file-name-nondirectory buffer-file-name)))
		       "." html-extension)
		      (file-name-as-directory
		       (or pub-dir (org-export-directory :html opt-plist))))))
	 (current-dir (if buffer-file-name
			  (file-name-directory buffer-file-name)
			default-directory))
	 (buffer (if to-buffer
		     (cond
		      ((eq to-buffer 'string) (get-buffer-create "*Org HTML Export*"))
		      (t (get-buffer-create to-buffer)))
		   (find-file-noselect filename)))
         (org-levels-open (make-vector org-level-max nil))
	 (date (plist-get opt-plist :date))
         (author      (plist-get opt-plist :author))
	 (title       (or (and subtree-p (org-export-get-title-from-subtree))
			  (plist-get opt-plist :title)
			  (and (not
				(plist-get opt-plist :skip-before-1st-heading))
			       (org-export-grab-title-from-buffer))
			  (and buffer-file-name
			       (file-name-sans-extension
				(file-name-nondirectory buffer-file-name)))
			  "UNTITLED"))
	 (html-table-tag (plist-get opt-plist :html-table-tag))
	 (quote-re0   (concat "^[ \t]*" org-quote-string "\\>"))
	 (quote-re    (concat "^\\(\\*+\\)\\([ \t]+" org-quote-string "\\>\\)"))
	 (inquote     nil)
	 (infixed     nil)
	 (inverse     nil)
	 (in-local-list nil)
	 (local-list-type nil)
	 (local-list-indent nil)
	 (llt org-plain-list-ordered-item-terminator)
	 (email       (plist-get opt-plist :email))
         (language    (plist-get opt-plist :language))
	 (lang-words  nil)
	 (head-count  0) cnt
	 (start       0)
	 (coding-system (and (boundp 'buffer-file-coding-system)
			     buffer-file-coding-system))
	 (coding-system-for-write (or org-export-html-coding-system
				      coding-system))
	 (save-buffer-coding-system (or org-export-html-coding-system
					coding-system))
	 (charset (and coding-system-for-write
		       (fboundp 'coding-system-get)
		       (coding-system-get coding-system-for-write
					  'mime-charset)))
         (region
          (buffer-substring
           (if region-p (region-beginning) (point-min))
           (if region-p (region-end) (point-max))))
         (lines
          (org-split-string
	   (org-export-preprocess-string
	    region
	    :emph-multiline t
	    :for-html t
	    :skip-before-1st-heading
	    (plist-get opt-plist :skip-before-1st-heading)
	    :drawers (plist-get opt-plist :drawers)
	    :archived-trees
	    (plist-get opt-plist :archived-trees)
	    :select-tags (plist-get opt-plist :select-tags)
	    :exclude-tags (plist-get opt-plist :exclude-tags)
	    :add-text
	    (plist-get opt-plist :text)
	    :LaTeX-fragments
	    (plist-get opt-plist :LaTeX-fragments))
	   "[\r\n]"))
	 table-open type
	 table-buffer table-orig-buffer
	 ind item-type starter didclose
	 rpl path attr desc descp desc1 desc2 link
	 snumber fnc item-tag
	 )

    (let ((inhibit-read-only t))
      (org-unmodified
       (remove-text-properties (point-min) (point-max)
			       '(:org-license-to-kill t))))

    (message "Exporting...")

    (setq org-min-level (org-get-min-level lines))
    (setq org-last-level org-min-level)
    (org-init-section-numbers)

    (cond
     ((and date (string-match "%" date))
      (setq date (format-time-string date)))
     (date)
     (t (setq date (format-time-string "%Y-%m-%d %T %Z"))))

    ;; Get the language-dependent settings
    (setq lang-words (or (assoc language org-export-language-setup)
                         (assoc "en" org-export-language-setup)))

    ;; Switch to the output buffer
    (set-buffer buffer)
    (let ((inhibit-read-only t)) (erase-buffer))
    (fundamental-mode)

    (and (fboundp 'set-buffer-file-coding-system)
	 (set-buffer-file-coding-system coding-system-for-write))

    (let ((case-fold-search nil)
	  (org-odd-levels-only odd))
      ;; create local variables for all options, to make sure all called
      ;; functions get the correct information
      (mapc (lambda (x)
	      (set (make-local-variable (cdr x))
		   (plist-get opt-plist (car x))))
	    org-export-plist-vars)
      (setq umax (if arg (prefix-numeric-value arg)
                   org-export-headline-levels))
      (setq umax-toc (if (integerp org-export-with-toc)
			 (min org-export-with-toc umax)
		       umax))
      (unless body-only
	;; File header
	(insert (format
		 "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
               \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\"
lang=\"%s\" xml:lang=\"%s\">
<head>
<title>%s</title>
<meta http-equiv=\"Content-Type\" content=\"text/html;charset=%s\"/>
<meta name=\"generator\" content=\"Org-mode\"/>
<meta name=\"generated\" content=\"%s\"/>
<meta name=\"author\" content=\"%s\"/>
%s
</head><body>
"
		 language language (org-html-expand title)
		 (or charset "iso-8859-1") date author style))

	(insert (or (plist-get opt-plist :preamble) ""))

	(when (plist-get opt-plist :auto-preamble)
	  (if title (insert (format org-export-html-title-format
				    (org-html-expand title))))))

      (if (and org-export-with-toc (not body-only))
	  (progn
	    (push (format "<h%d>%s</h%d>\n"
			  org-export-html-toplevel-hlevel
			  (nth 3 lang-words)
			  org-export-html-toplevel-hlevel)
		  thetoc)
	    (push "<div id=\"text-table-of-contents\">\n" thetoc)
	    (push "<ul>\n<li>" thetoc)
	    (setq lines
		  (mapcar '(lambda (line)
		    (if (string-match org-todo-line-regexp line)
			;; This is a headline
			(progn
			  (setq have-headings t)
			  (setq level (- (match-end 1) (match-beginning 1))
				level (org-tr-level level)
				txt (save-match-data
				      (org-html-expand
				       (org-export-cleanup-toc-line
					(match-string 3 line))))
				todo
				(or (and org-export-mark-todo-in-toc
					 (match-beginning 2)
					 (not (member (match-string 2 line)
						      org-done-keywords)))
					; TODO, not DONE
				    (and org-export-mark-todo-in-toc
					 (= level umax-toc)
					 (org-search-todo-below
					  line lines level))))
			  (if (string-match
			       (org-re "[ \t]+:\\([[:alnum:]_@:]+\\):[ \t]*$") txt)
			      (setq txt (replace-match  "&nbsp;&nbsp;&nbsp;<span class=\"tag\"> \\1</span>" t nil txt)))
			  (if (string-match quote-re0 txt)
			      (setq txt (replace-match "" t t txt)))
			  (setq snumber (org-section-number level))
			  (if org-export-with-section-numbers
			      (setq txt (concat snumber " " txt)))
			  (if (<= level (max umax umax-toc))
			      (setq head-count (+ head-count 1)))
			  (if (<= level umax-toc)
			      (progn
				(if (> level org-last-level)
				    (progn
				      (setq cnt (- level org-last-level))
				      (while (>= (setq cnt (1- cnt)) 0)
					(push "\n<ul>\n<li>" thetoc))
				      (push "\n" thetoc)))
				(if (< level org-last-level)
				    (progn
				      (setq cnt (- org-last-level level))
				      (while (>= (setq cnt (1- cnt)) 0)
					(push "</li>\n</ul>" thetoc))
				      (push "\n" thetoc)))
				;; Check for targets
				(while (string-match org-any-target-regexp line)
				  (setq line (replace-match
					      (concat "@<span class=\"target\">" (match-string 1 line) "@</span> ")
					      t t line)))
				(while (string-match "&lt;\\(&lt;\\)+\\|&gt;\\(&gt;\\)+" txt)
				  (setq txt (replace-match "" t t txt)))
				(push
				 (format
				  (if todo
				      "</li>\n<li><a href=\"#sec-%s\"><span class=\"todo\">%s</span></a>"
				    "</li>\n<li><a href=\"#sec-%s\">%s</a>")
				  snumber txt) thetoc)

				(setq org-last-level level))
			    )))
		    line)
			  lines))
	    (while (> org-last-level (1- org-min-level))
	      (setq org-last-level (1- org-last-level))
	      (push "</li>\n</ul>\n" thetoc))
	    (push "</div>\n" thetoc)
	    (setq thetoc (if have-headings (nreverse thetoc) nil))))

      (setq head-count 0)
      (org-init-section-numbers)

      (org-open-par)

      (while (setq line (pop lines) origline line)
	(catch 'nextline

	  ;; end of quote section?
 	  (when (and inquote (string-match "^\\*+ " line))
	    (insert "</pre>\n")
	    (setq inquote nil))
	  ;; inside a quote section?
	  (when inquote
	    (insert (org-html-protect line) "\n")
	    (throw 'nextline nil))

	  ;; Fixed-width, verbatim lines (examples)
	  (when (and org-export-with-fixed-width
		     (string-match "^[ \t]*:\\(.*\\)" line))
	    (when (not infixed)
	      (setq infixed t)
	      (insert "<pre class=\"example\">\n"))
	    (insert (org-html-protect (match-string 1 line)) "\n")
	    (when (or (not lines)
		      (not (string-match "^[ \t]*\\(:.*\\)"
					 (car lines))))
	      (setq infixed nil)
	      (insert "</pre>\n"))
	    (throw 'nextline nil))

	  ;; Protected HTML
	  (when (get-text-property 0 'org-protected line)
	    (let (par)
	      (when (re-search-backward
		     "\\(<p>\\)\\([ \t\r\n]*\\)\\=" (- (point) 100) t)
		(setq par (match-string 1))
		(replace-match "\\2\n"))
	      (insert line "\n")
	      (while (and lines
			  (not (string-match "^[ \t]*:" (car lines)))
			  (or (= (length (car lines)) 0)
			      (get-text-property 0 'org-protected (car lines))))
		(insert (pop lines) "\n"))
	      (and par (insert "<p>\n")))
	    (throw 'nextline nil))

	  ;; Horizontal line
	  (when (string-match "^[ \t]*-\\{5,\\}[ \t]*$" line)
	    (insert "\n<hr/>\n")
	    (throw 'nextline nil))

	  ;; Blockquotes and verse
	  (when (equal "ORG-BLOCKQUOTE-START" line)
	    (insert "<blockquote>\n<p>\n")
	    (throw 'nextline nil))
	  (when (equal "ORG-BLOCKQUOTE-END" line)
	    (insert "</p>\n</blockquote>\n")
	    (throw 'nextline nil))
	  (when (equal "ORG-VERSE-START" line)
	    (insert "\n<p class=\"verse\">\n")
	    (setq inverse t)
	    (throw 'nextline nil))
	  (when (equal "ORG-VERSE-END" line)
	    (insert "</p>\n")
	    (setq inverse nil)
	    (throw 'nextline nil))
	  (when inverse
	    (let ((i (org-get-string-indentation line)))
	      (if (> i 0)
		  (setq line (concat (mapconcat 'identity
						(make-list (* 2 i) "\\nbsp") "")
				     " " (org-trim line))))
	      (setq line (concat line " \\\\"))))

	  ;; make targets to anchors
	  (while (string-match "<<<?\\([^<>]*\\)>>>?\\((INVISIBLE)\\)?[ \t]*\n?" line)
	    (cond
	     ((match-end 2)
	      (setq line (replace-match
			  (concat "@<a name=\""
				  (org-solidify-link-text (match-string 1 line))
				  "\">\\nbsp@</a>")
			  t t line)))
	     ((and org-export-with-toc (equal (string-to-char line) ?*))
	      ;; FIXME: NOT DEPENDENT on TOC?????????????????????
	      (setq line (replace-match
			  (concat "@<span class=\"target\">" (match-string 1 line) "@</span> ")
;			  (concat "@<i>" (match-string 1 line) "@</i> ")
			  t t line)))
	     (t
	      (setq line (replace-match
			  (concat "@<a name=\""
				  (org-solidify-link-text (match-string 1 line))
				  "\" class=\"target\">" (match-string 1 line) "@</a> ")
			  t t line)))))

	  (setq line (org-html-handle-time-stamps line))

	  ;; replace "&" by "&amp;", "<" and ">" by "&lt;" and "&gt;"
	  ;; handle @<..> HTML tags (replace "@&gt;..&lt;" by "<..>")
	  ;; Also handle sub_superscripts and checkboxes
	  (or (string-match org-table-hline-regexp line)
	      (setq line (org-html-expand line)))

	  ;; Format the links
	  (setq start 0)
	  (while (string-match org-bracket-link-analytic-regexp line start)
	    (setq start (match-beginning 0))
	    (setq path (save-match-data (org-link-unescape
					 (match-string 3 line))))
	    (setq type (cond
			((match-end 2) (match-string 2 line))
			((save-match-data
			   (or (file-name-absolute-p path)
			       (string-match "^\\.\\.?/" path)))
			 "file")
			(t "internal")))
	    (setq path (org-extract-attributes path))
	    (setq attr (org-attributes-to-string
			(get-text-property 0 'org-attributes path)))
	    (setq desc1 (if (match-end 5) (match-string 5 line))
		  desc2 (if (match-end 2) (concat type ":" path) path)
		  descp (and desc1 (not (equal desc1 desc2)))
		  desc (or desc1 desc2))
	    ;; Make an image out of the description if that is so wanted
	    (when (and descp (org-file-image-p desc))
	      (save-match-data
		(if (string-match "^file:" desc)
		    (setq desc (substring desc (match-end 0)))))
	      (setq desc (concat "<img src=\"" desc "\"/>")))
	    ;; FIXME: do we need to unescape here somewhere?
	    (cond
	     ((equal type "internal")
	      (setq rpl
		    (concat
		     "<a href=\"#"
		     (org-solidify-link-text
		      (save-match-data (org-link-unescape path)) nil)
		     "\"" attr ">" desc "</a>")))
	     ((member type '("http" "https"))
	      ;; standard URL, just check if we need to inline an image
	      (if (and (or (eq t org-export-html-inline-images)
			   (and org-export-html-inline-images (not descp)))
		       (org-file-image-p path))
		  (setq rpl (concat "<img src=\"" type ":" path "\"" attr "/>"))
		(setq link (concat type ":" path))
		(setq rpl (concat "<a href=\"" link "\"" attr ">"
				  desc "</a>"))))
	     ((member type '("ftp" "mailto" "news"))
	      ;; standard URL
	      (setq link (concat type ":" path))
	      (setq rpl (concat "<a href=\"" link "\"" attr ">" desc "</a>")))

	     ((functionp (setq fnc (nth 2 (assoc type org-link-protocols))))
	      ;; The link protocol has a function for format the link
	      (setq rpl
		    (save-match-data
		      (funcall fnc (org-link-unescape path) desc1 'html))))

	     ((string= type "file")
	      ;; FILE link
	      (let* ((filename path)
		     (abs-p (file-name-absolute-p filename))
		     thefile file-is-image-p search)
		(save-match-data
		  (if (string-match "::\\(.*\\)" filename)
		      (setq search (match-string 1 filename)
			    filename (replace-match "" t nil filename)))
		  (setq valid
			(if (functionp link-validate)
			    (funcall link-validate filename current-dir)
			  t))
		  (setq file-is-image-p (org-file-image-p filename))
		  (setq thefile (if abs-p (expand-file-name filename) filename))
		  (when (and org-export-html-link-org-files-as-html
			     (string-match "\\.org$" thefile))
		    (setq thefile (concat (substring thefile 0
						     (match-beginning 0))
					  "." html-extension))
		    (if (and search
			     ;; make sure this is can be used as target search
			     (not (string-match "^[0-9]*$" search))
			     (not (string-match "^\\*" search))
			     (not (string-match "^/.*/$" search)))
			(setq thefile (concat thefile "#"
					      (org-solidify-link-text
					       (org-link-unescape search)))))
		    (when (string-match "^file:" desc)
		      (setq desc (replace-match "" t t desc))
		      (if (string-match "\\.org$" desc)
			  (setq desc (replace-match "" t t desc))))))
		(setq rpl (if (and file-is-image-p
				   (or (eq t org-export-html-inline-images)
				       (and org-export-html-inline-images
					    (not descp))))
			      (concat "<img src=\"" thefile "\"" attr "/>")
			    (concat "<a href=\"" thefile "\"" attr ">"
				    desc "</a>")))
		(if (not valid) (setq rpl desc))))

	     (t
	      ;; just publish the path, as default
	      (setq rpl (concat "<i>&lt;" type ":"
				(save-match-data (org-link-unescape path))
				"&gt;</i>"))))
	    (setq line (replace-match rpl t t line)
		  start (+ start (length rpl))))

	  ;; TODO items
	  (if (and (string-match org-todo-line-regexp line)
		   (match-beginning 2))

              (setq line
                    (concat (substring line 0 (match-beginning 2))
                            "<span class=\""
                            (if (member (match-string 2 line)
                                        org-done-keywords)
                                "done" "todo")
                            "\">" (match-string 2 line)
                            "</span>" (substring line (match-end 2)))))

	  ;; Does this contain a reference to a footnote?
	  (when org-export-with-footnotes
	    (setq start 0)
	    (while (string-match "\\([^* \t].*?\\)\\[\\([0-9]+\\)\\]" line start)
	      (if (get-text-property (match-beginning 2) 'org-protected line)
		  (setq start (match-end 2))
		(let ((n (match-string 2 line)))
		  (setq line
			(replace-match
			 (format
			  "%s<sup><a class=\"footref\" name=\"fnr.%s\" href=\"#fn.%s\">%s</a></sup>"
			  (match-string 1 line) n n n)
			 t t line))))))

	  (cond
	   ((string-match "^\\(\\*+\\)[ \t]+\\(.*\\)" line)
	    ;; This is a headline
	    (setq level (org-tr-level (- (match-end 1) (match-beginning 1)))
		  txt (match-string 2 line))
	    (if (string-match quote-re0 txt)
		(setq txt (replace-match "" t t txt)))
	    (if (<= level (max umax umax-toc))
		(setq head-count (+ head-count 1)))
	    (when in-local-list
	      ;; Close any local lists before inserting a new header line
	      (while local-list-type
		(org-close-li (car local-list-type))
		(insert (format "</%sl>\n" (car local-list-type)))
		(pop local-list-type))
	      (setq local-list-indent nil
		    in-local-list nil))
	    (setq first-heading-pos (or first-heading-pos (point)))
	    (org-html-level-start level txt umax
				  (and org-export-with-toc (<= level umax))
				  head-count)
	    ;; QUOTES
	    (when (string-match quote-re line)
	      (insert "<pre>")
	      (setq inquote t)))

	   ((and org-export-with-tables
		 (string-match "^\\([ \t]*\\)\\(|\\|\\+-+\\+\\)" line))
	    (if (not table-open)
		;; New table starts
		(setq table-open t table-buffer nil table-orig-buffer nil))
	    ;; Accumulate lines
	    (setq table-buffer (cons line table-buffer)
		  table-orig-buffer (cons origline table-orig-buffer))
	    (when (or (not lines)
		      (not (string-match "^\\([ \t]*\\)\\(|\\|\\+-+\\+\\)"
					 (car lines))))
	      (setq table-open nil
		    table-buffer (nreverse table-buffer)
		    table-orig-buffer (nreverse table-orig-buffer))
	      (org-close-par-maybe)
	      (insert (org-format-table-html table-buffer table-orig-buffer))))
	   (t
	    ;; Normal lines
	    (when (string-match
		   (cond
		    ((eq llt t) "^\\([ \t]*\\)\\(\\([-+*] \\)\\|\\([0-9]+[.)]\\) \\)?\\( *[^ \t\n\r]\\|[ \t]*$\\)")
		    ((= llt ?.) "^\\([ \t]*\\)\\(\\([-+*] \\)\\|\\([0-9]+\\.\\) \\)?\\( *[^ \t\n\r]\\|[ \t]*$\\)")
		    ((= llt ?\)) "^\\([ \t]*\\)\\(\\([-+*] \\)\\|\\([0-9]+)\\) \\)?\\( *[^ \t\n\r]\\|[ \t]*$\\)")
		    (t (error "Invalid value of `org-plain-list-ordered-item-terminator'")))
		   line)
	      (setq ind (org-get-string-indentation line)
		    item-type (if (match-beginning 4) "o" "u")
		    starter (if (match-beginning 2)
				(substring (match-string 2 line) 0 -1))
		    line (substring line (match-beginning 5))
		    item-tag nil)
	      (if (and starter (string-match "\\(.*?\\) ::[ \t]*" line))
		  (setq item-type "d"
			item-tag (match-string 1 line)
			line (substring line (match-end 0))))
	      (when (and (not (equal item-type "d"))
			 (not (string-match "[^ \t]" line)))
		;; empty line.  Pretend indentation is large.
		(setq ind (if org-empty-line-terminates-plain-lists
			      0
			    (1+ (or (car local-list-indent) 1)))))
	      (setq didclose nil)
	      (while (and in-local-list
			  (or (and (= ind (car local-list-indent))
				   (not starter))
			      (< ind (car local-list-indent))))
		(setq didclose t)
		(org-close-li (car local-list-type))
		(insert (format "</%sl>\n" (car local-list-type)))
		(pop local-list-type) (pop local-list-indent)
		(setq in-local-list local-list-indent))
	      (cond
	       ((and starter
		     (or (not in-local-list)
			 (> ind (car local-list-indent))))
		;; Start new (level of) list
		(org-close-par-maybe)
		(insert (cond
			 ((equal item-type "u") "<ul>\n<li>\n")
			 ((equal item-type "o") "<ol>\n<li>\n")
			 ((equal item-type "d") 
			  (format "<dl>\n<dt>%s</dt><dd>\n" item-tag))))
		(push item-type local-list-type)
		(push ind local-list-indent)
		(setq in-local-list t))
	       (starter
		;; continue current list
		(org-close-li (car local-list-type))
		(insert (cond
			 ((equal (car local-list-type) "d")
			  (format "<dt>%s</dt><dd>\n" (or item-tag "???")))
			 (t "<li>\n"))))
	       (didclose
		;; we did close a list, normal text follows: need <p>
		(org-open-par)))
	      (if (string-match "^[ \t]*\\[\\([X ]\\)\\]" line)
		  (setq line
			(replace-match
			 (if (equal (match-string 1 line) "X")
			     "<b>[X]</b>"
			   "<b>[<span style=\"visibility:hidden;\">X</span>]</b>")
			   t t line))))

	    ;; Empty lines start a new paragraph.  If hand-formatted lists
	    ;; are not fully interpreted, lines starting with "-", "+", "*"
	    ;; also start a new paragraph.
	    (if (string-match "^ [-+*]-\\|^[ \t]*$" line) (org-open-par))

	    ;; Is this the start of a footnote?
	    (when org-export-with-footnotes
	      (when (string-match "^[ \t]*\\[\\([0-9]+\\)\\]" line)
		(org-close-par-maybe)
		(let ((n (match-string 1 line)))
		  (setq line (replace-match
			      (format "<p class=\"footnote\"><sup><a class=\"footnum\" name=\"fn.%s\" href=\"#fnr.%s\">%s</a></sup>" n n n) t t line))
		  (setq line (concat line "</p>")))))

	    ;; Check if the line break needs to be conserved
	    (cond
	     ((string-match "\\\\\\\\[ \t]*$" line)
	      (setq line (replace-match "<br/>" t t line)))
	     (org-export-preserve-breaks
	      (setq line (concat line "<br/>"))))

	    (insert line "\n")))))

      ;; Properly close all local lists and other lists
      (when inquote (insert "</pre>\n"))
      (when in-local-list
	;; Close any local lists before inserting a new header line
	(while local-list-type
	  (org-close-li (car local-list-type))
	  (insert (format "</%sl>\n" (car local-list-type)))
	  (pop local-list-type))
	(setq local-list-indent nil
	      in-local-list nil))
      (org-html-level-start 1 nil umax
			    (and org-export-with-toc (<= level umax))
			    head-count)
      ;; the </div> to close the last text-... div.
      (insert "</div>\n")

      (unless body-only
	(when (plist-get opt-plist :auto-postamble)
	  (insert "<div id=\"postamble\">")
	  (when (and org-export-author-info author)
	    (insert "<p class=\"author\"> "
		    (nth 1 lang-words) ": " author "\n")
	    (when email
	      (if (listp (split-string email ",+ *"))
		  (mapc (lambda(e)
			  (insert "<a href=\"mailto:" e "\">&lt;"
				  e "&gt;</a>\n"))
			(split-string email ",+ *"))
		(insert "<a href=\"mailto:" email "\">&lt;"
			email "&gt;</a>\n")))
	    (insert "</p>\n"))
	  (when (and date org-export-time-stamp-file)
	    (insert "<p class=\"date\"> "
		    (nth 2 lang-words) ": "
		    date "</p>\n"))
	  (when org-export-creator-info
	    (insert (format "<p>HTML generated by org-mode %s in emacs %s</p>\n"
			    org-version emacs-major-version)))
	  (insert "</div>"))

	(if org-export-html-with-timestamp
	    (insert org-export-html-html-helper-timestamp))
	(insert (or (plist-get opt-plist :postamble) ""))
	(insert "</body>\n</html>\n"))

      (unless (plist-get opt-plist :buffer-will-be-killed)
	(normal-mode)
	(if (eq major-mode default-major-mode) (html-mode)))

      ;; insert the table of contents
      (goto-char (point-min))
      (when thetoc
	(if (or (re-search-forward
		 "<p>\\s-*\\[TABLE-OF-CONTENTS\\]\\s-*</p>" nil t)
		(re-search-forward
		 "\\[TABLE-OF-CONTENTS\\]" nil t))
	    (progn
	      (goto-char (match-beginning 0))
	      (replace-match ""))
	  (goto-char first-heading-pos)
	  (when (looking-at "\\s-*</p>")
	    (goto-char (match-end 0))
	    (insert "\n")))
	(insert "<div id=\"table-of-contents\">\n")
	(mapc 'insert thetoc)
	(insert "</div>\n"))
      ;; remove empty paragraphs and lists
      (goto-char (point-min))
      (while (re-search-forward "<p>[ \r\n\t]*</p>" nil t)
	(replace-match ""))
      (goto-char (point-min))
      (while (re-search-forward "<li>[ \r\n\t]*</li>\n?" nil t)
	(replace-match ""))
      (goto-char (point-min))
      (while (re-search-forward "</ul>\\s-*<ul>\n?" nil t)
	(replace-match ""))
      ;; Convert whitespace place holders
      (goto-char (point-min))
      (let (beg end n)
	(while (setq beg (next-single-property-change (point) 'org-whitespace))
	  (setq n (get-text-property beg 'org-whitespace)
		end (next-single-property-change beg 'org-whitespace))
	  (goto-char beg)
	  (delete-region beg end)
	  (insert (format "<span style=\"visibility:hidden;\">%s</span>"
			  (make-string n ?x)))))
      (or to-buffer (save-buffer))
      (goto-char (point-min))
      (message "Exporting... done")
      (if (eq to-buffer 'string)
	  (prog1 (buffer-substring (point-min) (point-max))
	    (kill-buffer (current-buffer)))
	(current-buffer)))))


(defvar org-table-colgroup-info nil)
(defun org-format-table-ascii (lines)
  "Format a table for ascii export."
  (if (stringp lines)
      (setq lines (org-split-string lines "\n")))
  (if (not (string-match "^[ \t]*|" (car lines)))
      ;; Table made by table.el - test for spanning
      lines

    ;; A normal org table
    ;; Get rid of hlines at beginning and end
    (if (string-match "^[ \t]*|-" (car lines)) (setq lines (cdr lines)))
    (setq lines (nreverse lines))
    (if (string-match "^[ \t]*|-" (car lines)) (setq lines (cdr lines)))
    (setq lines (nreverse lines))
    (when org-export-table-remove-special-lines
      ;; Check if the table has a marking column.  If yes remove the
      ;; column and the special lines
      (setq lines (org-table-clean-before-export lines)))
    ;; Get rid of the vertical lines except for grouping
    (let ((vl (org-colgroup-info-to-vline-list org-table-colgroup-info))
	  rtn line vl1 start)
      (while (setq line (pop lines))
	(if (string-match org-table-hline-regexp line)
	    (and (string-match "|\\(.*\\)|" line)
		 (setq line (replace-match " \\1" t nil line)))
	  (setq start 0 vl1 vl)
	  (while (string-match "|" line start)
	    (setq start (match-end 0))
	    (or (pop vl1) (setq line (replace-match " " t t line)))))
	(push line rtn))
      (nreverse rtn))))

(defun org-colgroup-info-to-vline-list (info)
  (let (vl new last)
    (while info
      (setq last new new (pop info))
      (if (or (memq last '(:end :startend))
	      (memq new  '(:start :startend)))
	  (push t vl)
	(push nil vl)))
    (setq vl (nreverse vl))
    (and vl (setcar vl nil))
    vl))

(defvar org-table-number-regexp) ; defined in org-table.el
(defun org-format-table-html (lines olines)
  "Find out which HTML converter to use and return the HTML code."
  (if (stringp lines)
      (setq lines (org-split-string lines "\n")))
  (if (string-match "^[ \t]*|" (car lines))
      ;; A normal org table
      (org-format-org-table-html lines)
    ;; Table made by table.el - test for spanning
    (let* ((hlines (delq nil (mapcar
			      (lambda (x)
				(if (string-match "^[ \t]*\\+-" x) x
				  nil))
			      lines)))
	   (first (car hlines))
	   (ll (and (string-match "\\S-+" first)
		    (match-string 0 first)))
	   (re (concat "^[ \t]*" (regexp-quote ll)))
	   (spanning (delq nil (mapcar (lambda (x) (not (string-match re x)))
				       hlines))))
      (if (and (not spanning)
	       (not org-export-prefer-native-exporter-for-tables))
	  ;; We can use my own converter with HTML conversions
	  (org-format-table-table-html lines)
	;; Need to use the code generator in table.el, with the original text.
	(org-format-table-table-html-using-table-generate-source olines)))))

(defvar org-table-number-fraction) ; defined in org-table.el
(defun org-format-org-table-html (lines &optional splice)
  "Format a table into HTML."
  (require 'org-table)
  ;; Get rid of hlines at beginning and end
  (if (string-match "^[ \t]*|-" (car lines)) (setq lines (cdr lines)))
  (setq lines (nreverse lines))
  (if (string-match "^[ \t]*|-" (car lines)) (setq lines (cdr lines)))
  (setq lines (nreverse lines))
  (when org-export-table-remove-special-lines
    ;; Check if the table has a marking column.  If yes remove the
    ;; column and the special lines
    (setq lines (org-table-clean-before-export lines)))

  (let ((head (and org-export-highlight-first-table-line
		   (delq nil (mapcar
			      (lambda (x) (string-match "^[ \t]*|-" x))
			      (cdr lines)))))
	(nlines 0) fnum i
	tbopen line fields html gr colgropen)
    (if splice (setq head nil))
    (unless splice (push (if head "<thead>" "<tbody>") html))
    (setq tbopen t)
    (while (setq line (pop lines))
      (catch 'next-line
	(if (string-match "^[ \t]*|-" line)
	    (progn
	      (unless splice
		(push (if head "</thead>" "</tbody>") html)
		(if lines (push "<tbody>" html) (setq tbopen nil)))
	      (setq head nil)   ;; head ends here, first time around
	      ;; ignore this line
	      (throw 'next-line t)))
	;; Break the line into fields
	(setq fields (org-split-string line "[ \t]*|[ \t]*"))
	(unless fnum (setq fnum (make-vector (length fields) 0)))
	(setq nlines (1+ nlines) i -1)
	(push (concat "<tr>"
		      (mapconcat
		       (lambda (x)
			 (setq i (1+ i))
			 (if (and (< i nlines)
				  (string-match org-table-number-regexp x))
			     (incf (aref fnum i)))
			 (if head
			     (concat (car org-export-table-header-tags) x
				     (cdr org-export-table-header-tags))
			   (concat (car org-export-table-data-tags) x
				   (cdr org-export-table-data-tags))))
		       fields "")
		      "</tr>")
	      html)))
    (unless splice (if tbopen (push "</tbody>" html)))
    (unless splice (push "</table>\n" html))
    (setq html (nreverse html))
    (unless splice
      ;; Put in col tags with the alignment (unfortuntely often ignored...)
      (push (mapconcat
	     (lambda (x)
	       (setq gr (pop org-table-colgroup-info))
	       (format "%s<col align=\"%s\"></col>%s"
		       (if (memq gr '(:start :startend))
			   (prog1
			       (if colgropen "</colgroup>\n<colgroup>" "<colgroup>")
			     (setq colgropen t))
			 "")
		       (if (> (/ (float x) nlines) org-table-number-fraction)
			   "right" "left")
		       (if (memq gr '(:end :startend))
			   (progn (setq colgropen nil) "</colgroup>")
			 "")))
	     fnum "")
	    html)
      (if colgropen (setq html (cons (car html) (cons "</colgroup>" (cdr html)))))
      (push html-table-tag html))
    (concat (mapconcat 'identity html "\n") "\n")))

(defun org-table-clean-before-export (lines)
  "Check if the table has a marking column.
If yes remove the column and the special lines."
  (setq org-table-colgroup-info nil)
  (if (memq nil
	    (mapcar
	     (lambda (x) (or (string-match "^[ \t]*|-" x)
			     (string-match "^[ \t]*| *\\([#!$*_^ /]\\) *|" x)))
	     lines))
      (progn
	(setq org-table-clean-did-remove-column nil)
	(delq nil
	      (mapcar
	       (lambda (x)
		 (cond
		  ((string-match  "^[ \t]*| */ *|" x)
		   (setq org-table-colgroup-info
			 (mapcar (lambda (x)
				   (cond ((member x '("<" "&lt;")) :start)
					 ((member x '(">" "&gt;")) :end)
					 ((member x '("<>" "&lt;&gt;")) :startend)
					 (t nil)))
				 (org-split-string x "[ \t]*|[ \t]*")))
		   nil)
		  (t x)))
	       lines)))
    (setq org-table-clean-did-remove-column t)
    (delq nil
	  (mapcar
	   (lambda (x)
	     (cond
	      ((string-match  "^[ \t]*| */ *|" x)
	       (setq org-table-colgroup-info
		     (mapcar (lambda (x)
			       (cond ((member x '("<" "&lt;")) :start)
				     ((member x '(">" "&gt;")) :end)
				     ((member x '("<>" "&lt;&gt;")) :startend)
				     (t nil)))
			     (cdr (org-split-string x "[ \t]*|[ \t]*"))))
	       nil)
	      ((string-match "^[ \t]*| *[!_^/] *|" x)
	       nil) ; ignore this line
	      ((or (string-match "^\\([ \t]*\\)|-+\\+" x)
		   (string-match "^\\([ \t]*\\)|[^|]*|" x))
	       ;; remove the first column
	       (replace-match "\\1|" t nil x))))
	   lines))))

(defun org-format-table-table-html (lines)
  "Format a table generated by table.el into HTML.
This conversion does *not* use `table-generate-source' from table.el.
This has the advantage that Org-mode's HTML conversions can be used.
But it has the disadvantage, that no cell- or row-spanning is allowed."
  (let (line field-buffer
	     (head org-export-highlight-first-table-line)
	     fields html empty)
    (setq html (concat html-table-tag "\n"))
    (while (setq line (pop lines))
      (setq empty "&nbsp;")
      (catch 'next-line
	(if (string-match "^[ \t]*\\+-" line)
	    (progn
	      (if field-buffer
		  (progn
		    (setq
		     html
		     (concat
		      html
		      "<tr>"
		      (mapconcat
		       (lambda (x)
			 (if (equal x "") (setq x empty))
			 (if head
			     (concat (car org-export-table-header-tags) x
				     (cdr org-export-table-header-tags))
			   (concat (car org-export-table-data-tags) x
				   (cdr org-export-table-data-tags))))
		       field-buffer "\n")
		      "</tr>\n"))
		    (setq head nil)
		    (setq field-buffer nil)))
	      ;; Ignore this line
	      (throw 'next-line t)))
	;; Break the line into fields and store the fields
	(setq fields (org-split-string line "[ \t]*|[ \t]*"))
	(if field-buffer
	    (setq field-buffer (mapcar
				(lambda (x)
				  (concat x "<br/>" (pop fields)))
				field-buffer))
	  (setq field-buffer fields))))
    (setq html (concat html "</table>\n"))
    html))

(defun org-format-table-table-html-using-table-generate-source (lines)
  "Format a table into html, using `table-generate-source' from table.el.
This has the advantage that cell- or row-spanning is allowed.
But it has the disadvantage, that Org-mode's HTML conversions cannot be used."
  (require 'table)
  (with-current-buffer (get-buffer-create " org-tmp1 ")
    (erase-buffer)
    (insert (mapconcat 'identity lines "\n"))
    (goto-char (point-min))
    (if (not (re-search-forward "|[^+]" nil t))
	(error "Error processing table"))
    (table-recognize-table)
    (with-current-buffer (get-buffer-create " org-tmp2 ") (erase-buffer))
    (table-generate-source 'html " org-tmp2 ")
    (set-buffer " org-tmp2 ")
    (buffer-substring (point-min) (point-max))))

(defun org-export-splice-style (style extra)
  "Splice EXTRA into STYLE, just before \"</style>\"."
  (if (and (stringp extra)
	   (string-match "\\S-" extra)
	   (string-match "</style>" style))
      (concat (substring style 0 (match-beginning 0))
	      "\n" extra "\n"
	      (substring style (match-beginning 0)))
    style))

(defun org-html-handle-time-stamps (s)
  "Format time stamps in string S, or remove them."
  (catch 'exit
    (let (r b)
      (while (string-match org-maybe-keyword-time-regexp s)
	(if (and (match-end 1) (equal (match-string 1 s) org-clock-string))
	    ;; never export CLOCK
	    (throw 'exit ""))
	(or b (setq b (substring s 0 (match-beginning 0))))
	(if (not org-export-with-timestamps)
	    (setq r (concat r (substring s 0 (match-beginning 0)))
		  s (substring s (match-end 0)))
	  (setq r (concat
		   r (substring s 0 (match-beginning 0))
		   (if (match-end 1)
		       (format "@<span class=\"timestamp-kwd\">%s @</span>"
			       (match-string 1 s)))
		   (format " @<span class=\"timestamp\">%s@</span>"
			   (substring
			    (org-translate-time (match-string 3 s)) 1 -1)))
		s (substring s (match-end 0)))))
      ;; Line break if line started and ended with time stamp stuff
      (if (not r)
	  s
	(setq r (concat r s))
	(unless (string-match "\\S-" (concat b s))
	  (setq r (concat r "@<br/>")))
	r))))

(defun org-export-htmlize-region-for-paste (beg end)
  "Convert the region to HTML, using htmlize.el.
This is much like `htmlize-region-for-paste', only that it uses
the settings define in the org-... variables."
  (let* ((htmlize-output-type org-export-htmlize-output-type)
	 (htmlize-css-name-prefix org-export-htmlize-css-font-prefix)
	 (htmlbuf (htmlize-region beg end)))
    (unwind-protect
	(with-current-buffer htmlbuf
	  (buffer-substring (plist-get htmlize-buffer-places 'content-start)
			    (plist-get htmlize-buffer-places 'content-end)))
      (kill-buffer htmlbuf))))

;;;###autoload
(defun org-export-htmlize-generate-css ()
  "Create the CSS for all font definitions in the current Emacs session.
Use this to create face definitions in your CSS style file that can then
be used by code snippets transformed by htmlize.
This command just produces a buffer that contains class definitions for all
faces used in the current Emacs session.  You can copy and paste the ones you
need into your CSS file.

If you then set `org-export-htmlize-output-type' to `css', calls to
the function `org-export-htmlize-region-for-paste' will produce code
that uses these same face definitions."
  (interactive)
  (require 'htmlize)
  (and (get-buffer "*html*") (kill-buffer "*html*"))
  (with-temp-buffer
    (let ((fl (face-list))
	  (htmlize-css-name-prefix "org-")
	  (htmlize-output-type 'css)
	  f i)
      (while (setq f (pop fl)
		   i (and f (face-attribute f :inherit)))
	(when (and (symbolp f) (or (not i) (not (listp i))))
	  (insert (org-add-props (copy-sequence "1") nil 'face f))))
      (htmlize-region (point-min) (point-max))))
  (switch-to-buffer "*html*")
  (goto-char (point-min))
  (if (re-search-forward "<style" nil t)
      (delete-region (point-min) (match-beginning 0)))
  (if (re-search-forward "</style>" nil t)
      (delete-region (1+ (match-end 0)) (point-max)))
  (beginning-of-line 1)
  (if (looking-at " +") (replace-match ""))
  (goto-char (point-min)))

(defun org-html-protect (s)
  ;; convert & to &amp;, < to &lt; and > to &gt;
  (let ((start 0))
    (while (string-match "&" s start)
      (setq s (replace-match "&amp;" t t s)
	    start (1+ (match-beginning 0))))
    (while (string-match "<" s)
      (setq s (replace-match "&lt;" t t s)))
    (while (string-match ">" s)
      (setq s (replace-match "&gt;" t t s))))
  s)

(defun org-export-cleanup-toc-line (s)
  "Remove tags and time staps from lines going into the toc."
  (when (memq org-export-with-tags '(not-in-toc nil))
    (if (string-match (org-re " +:[[:alnum:]_@:]+: *$") s)
	(setq s (replace-match "" t t s))))
  (when org-export-remove-timestamps-from-toc
    (while (string-match org-maybe-keyword-time-regexp s)
      (setq s (replace-match "" t t s))))
  (while (string-match org-bracket-link-regexp s)
    (setq s (replace-match (match-string (if (match-end 3) 3 1) s)
			   t t s)))
  s)

(defun org-html-expand (string)
  "Prepare STRING for HTML export.  Applies all active conversions.
If there are links in the string, don't modify these."
  (let* ((re (concat org-bracket-link-regexp "\\|"
		     (org-re "[ \t]+\\(:[[:alnum:]_@:]+:\\)[ \t]*$")))
	 m s l res)
    (while (setq m (string-match re string))
      (setq s (substring string 0 m)
	    l (match-string 0 string)
	    string (substring string (match-end 0)))
      (push (org-html-do-expand s) res)
      (push l res))
    (push (org-html-do-expand string) res)
    (apply 'concat (nreverse res))))

(defun org-html-do-expand (s)
  "Apply all active conversions to translate special ASCII to HTML."
  (setq s (org-html-protect s))
  (if org-export-html-expand
      (let ((start 0))
	(while (string-match "@&lt;\\([^&]*\\)&gt;" s)
	  (setq s (replace-match "<\\1>" t nil s)))))
  (if org-export-with-emphasize
      (setq s (org-export-html-convert-emphasize s)))
  (if org-export-with-special-strings
      (setq s (org-export-html-convert-special-strings s)))
  (if org-export-with-sub-superscripts
      (setq s (org-export-html-convert-sub-super s)))
  (if org-export-with-TeX-macros
      (let ((start 0) wd ass)
	(while (setq start (string-match "\\\\\\([a-zA-Z]+\\)\\({}\\)?"
					 s start))
	  (if (get-text-property (match-beginning 0) 'org-protected s)
	      (setq start (match-end 0))
	    (setq wd (match-string 1 s))
	    (if (setq ass (assoc wd org-html-entities))
		(setq s (replace-match (or (cdr ass)
					   (concat "&" (car ass) ";"))
				       t t s))
	      (setq start (+ start (length wd))))))))
  s)

(defun org-create-multibrace-regexp (left right n)
  "Create a regular expression which will match a balanced sexp.
Opening delimiter is LEFT, and closing delimiter is RIGHT, both given
as single character strings.
The regexp returned will match the entire expression including the
delimiters.  It will also define a single group which contains the
match except for the outermost delimiters.  The maximum depth of
stacked delimiters is N.  Escaping delimiters is not possible."
  (let* ((nothing (concat "[^" "\\" left "\\" right "]*?"))
	 (or "\\|")
	 (re nothing)
	 (next (concat "\\(?:" nothing left nothing right "\\)+" nothing)))
    (while (> n 1)
      (setq n (1- n)
	    re (concat re or next)
	    next (concat "\\(?:" nothing left next right "\\)+" nothing)))
    (concat left "\\(" re "\\)" right)))

(defvar org-match-substring-regexp
  (concat
   "\\([^\\]\\)\\([_^]\\)\\("
   "\\(" (org-create-multibrace-regexp "{" "}" org-match-sexp-depth) "\\)"
   "\\|"
   "\\(" (org-create-multibrace-regexp "(" ")" org-match-sexp-depth) "\\)"
   "\\|"
   "\\(\\(?:\\*\\|[-+]?[^-+*!@#$%^_ \t\r\n,:\"?<>~;./{}=()]+\\)\\)\\)")
  "The regular expression matching a sub- or superscript.")

(defvar org-match-substring-with-braces-regexp
  (concat
   "\\([^\\]\\)\\([_^]\\)\\("
   "\\(" (org-create-multibrace-regexp "{" "}" org-match-sexp-depth) "\\)"
   "\\)")
  "The regular expression matching a sub- or superscript, forcing braces.")

(defconst org-export-html-special-string-regexps
  '(("\\\\-" . "&shy;")
    ("---\\([^-]\\)" . "&mdash;\\1")
    ("--\\([^-]\\)" . "&ndash;\\1")
    ("\\.\\.\\." . "&hellip;"))
  "Regular expressions for special string conversion.")

(defun org-export-html-convert-special-strings (string)
  "Convert special characters in STRING to HTML."
  (let ((all org-export-html-special-string-regexps)
	e a re rpl start)
    (while (setq a (pop all))
      (setq re (car a) rpl (cdr a) start 0)
      (while (string-match re string start)
	(if (get-text-property (match-beginning 0) 'org-protected string)
	    (setq start (match-end 0))
	  (setq string (replace-match rpl t nil string)))))
    string))

(defun org-export-html-convert-sub-super (string)
  "Convert sub- and superscripts in STRING to HTML."
  (let (key c (s 0) (requireb (eq org-export-with-sub-superscripts '{})))
    (while (string-match org-match-substring-regexp string s)
      (cond
       ((and requireb (match-end 8)) (setq s (match-end 2)))
       ((get-text-property  (match-beginning 2) 'org-protected string)
	(setq s (match-end 2)))
       (t
	(setq s (match-end 1)
	      key (if (string= (match-string 2 string) "_") "sub" "sup")
	      c (or (match-string 8 string)
		    (match-string 6 string)
		    (match-string 5 string))
	      string (replace-match
		      (concat (match-string 1 string)
			      "<" key ">" c "</" key ">")
		      t t string)))))
    (while (string-match "\\\\\\([_^]\\)" string)
      (setq string (replace-match (match-string 1 string) t t string)))
    string))

(defun org-export-html-convert-emphasize (string)
  "Apply emphasis."
  (let ((s 0) rpl)
    (while (string-match org-emph-re string s)
      (if (not (equal
		(substring string (match-beginning 3) (1+ (match-beginning 3)))
		(substring string (match-beginning 4) (1+ (match-beginning 4)))))
	  (setq s (match-beginning 0)
		rpl
		(concat
		 (match-string 1 string)
		 (nth 2 (assoc (match-string 3 string) org-emphasis-alist))
		 (match-string 4 string)
		 (nth 3 (assoc (match-string 3 string)
			       org-emphasis-alist))
		 (match-string 5 string))
		string (replace-match rpl t t string)
		s (+ s (- (length rpl) 2)))
	(setq s (1+ s))))
    string))

(defvar org-par-open nil)
(defun org-open-par ()
  "Insert <p>, but first close previous paragraph if any."
  (org-close-par-maybe)
  (insert "\n<p>")
  (setq org-par-open t))
(defun org-close-par-maybe ()
  "Close paragraph if there is one open."
  (when org-par-open
    (insert "</p>")
    (setq org-par-open nil)))
(defun org-close-li (&optional type)
  "Close <li> if necessary."
  (org-close-par-maybe)
  (insert (if (equal type "d") "</dd>\n" "</li>\n")))

(defvar body-only) ; dynamically scoped into this.
(defun org-html-level-start (level title umax with-toc head-count)
  "Insert a new level in HTML export.
When TITLE is nil, just close all open levels."
  (org-close-par-maybe)
  (let ((target (and title (org-get-text-property-any 0 'target title)))
	(l org-level-max)
	snumber)
    (while (>= l level)
      (if (aref org-levels-open (1- l))
	  (progn
	    (org-html-level-close l umax)
	    (aset org-levels-open (1- l) nil)))
      (setq l (1- l)))
    (when title
      ;; If title is nil, this means this function is called to close
      ;; all levels, so the rest is done only if title is given
	(when (string-match (org-re "\\(:[[:alnum:]_@:]+:\\)[ \t]*$") title)
	  (setq title (replace-match
		       (if org-export-with-tags
			   (save-match-data
			     (concat
			      "&nbsp;&nbsp;&nbsp;<span class=\"tag\">"
			      (mapconcat 'identity (org-split-string
						    (match-string 1 title) ":")
					 "&nbsp;")
			      "</span>"))
			 "")
		       t t title)))
      (if (> level umax)
	  (progn
	    (if (aref org-levels-open (1- level))
		(progn
		  (org-close-li)
		  (if target
		      (insert (format "<li id=\"%s\">" target) title "<br/>\n")
		    (insert "<li>" title "<br/>\n")))
	      (aset org-levels-open (1- level) t)
	      (org-close-par-maybe)
	      (if target
		  (insert (format "<ul>\n<li id=\"%s\">" target)
			  title "<br/>\n")
		(insert "<ul>\n<li>" title "<br/>\n"))))
	(aset org-levels-open (1- level) t)
	(setq snumber (org-section-number level))
	(if (and org-export-with-section-numbers (not body-only))
	    (setq title (concat snumber " " title)))
	(setq level (+ level org-export-html-toplevel-hlevel -1))
	(unless (= head-count 1) (insert "\n</div>\n"))
	(insert (format "\n<div id=\"outline-container-%s\" class=\"outline-%d\">\n<h%d id=\"sec-%s\">%s</h%d>\n<div id=\"text-%s\">\n"
			snumber level level snumber title level snumber))
	(org-open-par)))))

(defun org-get-text-property-any (pos prop &optional object)
  (or (get-text-property pos prop object)
      (and (setq pos (next-single-property-change pos prop object))
	   (get-text-property pos prop object))))

(defun org-html-level-close (level max-outline-level)
  "Terminate one level in HTML export."
  (if (<= level max-outline-level)
      (insert "</div>\n")
    (org-close-li)
    (insert "</ul>\n")))

;;; iCalendar export

;;;###autoload
(defun org-export-icalendar-this-file ()
  "Export current file as an iCalendar file.
The iCalendar file will be located in the same directory as the Org-mode
file, but with extension `.ics'."
  (interactive)
  (org-export-icalendar nil buffer-file-name))

;;;###autoload
(defun org-export-icalendar-all-agenda-files ()
  "Export all files in `org-agenda-files' to iCalendar .ics files.
Each iCalendar file will be located in the same directory as the Org-mode
file, but with extension `.ics'."
  (interactive)
  (apply 'org-export-icalendar nil (org-agenda-files t)))

;;;###autoload
(defun org-export-icalendar-combine-agenda-files ()
  "Export all files in `org-agenda-files' to a single combined iCalendar file.
The file is stored under the name `org-combined-agenda-icalendar-file'."
  (interactive)
  (apply 'org-export-icalendar t (org-agenda-files t)))

(defun org-export-icalendar (combine &rest files)
  "Create iCalendar files for all elements of FILES.
If COMBINE is non-nil, combine all calendar entries into a single large
file and store it under the name `org-combined-agenda-icalendar-file'."
  (save-excursion
    (org-prepare-agenda-buffers files)
    (let* ((dir (org-export-directory
		 :ical (list :publishing-directory
			     org-export-publishing-directory)))
	   file ical-file ical-buffer category started org-agenda-new-buffers)
      (and (get-buffer "*ical-tmp*") (kill-buffer "*ical-tmp*"))
      (when combine
	(setq ical-file
	      (if (file-name-absolute-p org-combined-agenda-icalendar-file)
		  org-combined-agenda-icalendar-file
		(expand-file-name org-combined-agenda-icalendar-file dir))
	      ical-buffer (org-get-agenda-file-buffer ical-file))
	(set-buffer ical-buffer) (erase-buffer))
      (while (setq file (pop files))
	(catch 'nextfile
	  (org-check-agenda-file file)
	  (set-buffer (org-get-agenda-file-buffer file))
	  (unless combine
	    (setq ical-file (concat (file-name-as-directory dir)
				    (file-name-sans-extension
				     (file-name-nondirectory buffer-file-name))
				    ".ics"))
	    (setq ical-buffer (org-get-agenda-file-buffer ical-file))
	    (with-current-buffer ical-buffer (erase-buffer)))
	  (setq category (or org-category
			     (file-name-sans-extension
			      (file-name-nondirectory buffer-file-name))))
	  (if (symbolp category) (setq category (symbol-name category)))
	  (let ((standard-output ical-buffer))
	    (if combine
		(and (not started) (setq started t)
		     (org-start-icalendar-file org-icalendar-combined-name))
	      (org-start-icalendar-file category))
	    (org-print-icalendar-entries combine)
	    (when (or (and combine (not files)) (not combine))
	      (org-finish-icalendar-file)
	      (set-buffer ical-buffer)
	      (run-hooks 'org-before-save-iCalendar-file-hook)
	      (save-buffer)
	      (run-hooks 'org-after-save-iCalendar-file-hook)
	      (and (boundp 'org-wait) (numberp org-wait) (sit-for org-wait))
	      ))))
      (org-release-buffers org-agenda-new-buffers))))

(defvar org-before-save-iCalendar-file-hook nil
  "Hook run before  an iCalendar file has been saved.
This can be used to modify the result of the export.")

(defvar org-after-save-iCalendar-file-hook nil
  "Hook run after an iCalendar file has been saved.
The iCalendar buffer is still current when this hook is run.
A good way to use this is to tell a desktop calenndar application to re-read
the iCalendar file.")

(defvar org-agenda-default-appointment-duration) ; defined in org-agenda.el
(defun org-print-icalendar-entries (&optional combine)
  "Print iCalendar entries for the current Org-mode file to `standard-output'.
When COMBINE is non nil, add the category to each line."
  (require 'org-agenda)
  (let ((re1 (concat org-ts-regexp "\\|<%%([^>\n]+>"))
	(re2 (concat "--?-?\\(" org-ts-regexp "\\)"))
	(dts (org-ical-ts-to-string
	      (format-time-string (cdr org-time-stamp-formats) (current-time))
	      "DTSTART"))
	hd ts ts2 state status (inc t) pos b sexp rrule
	scheduledp deadlinep todo prefix due start
	tmp pri categories entry location summary desc uid
	(sexp-buffer (get-buffer-create "*ical-tmp*")))
    (org-refresh-category-properties)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward re1 nil t)
	(catch :skip
	  (org-agenda-skip)
	  (when (boundp 'org-icalendar-verify-function)
	    (unless (funcall org-icalendar-verify-function)
	      (outline-next-heading)
	      (backward-char 1)
	      (throw :skip nil)))
	  (setq pos (match-beginning 0)
		ts (match-string 0)
		inc t
		hd (condition-case nil
		       (org-icalendar-cleanup-string
			(org-get-heading))
		     (error (throw :skip nil)))
		summary (org-icalendar-cleanup-string
			 (org-entry-get nil "SUMMARY"))
		desc (org-icalendar-cleanup-string
		      (or (org-entry-get nil "DESCRIPTION")
			  (and org-icalendar-include-body (org-get-entry)))
		      t org-icalendar-include-body)
		location (org-icalendar-cleanup-string
			  (org-entry-get nil "LOCATION"))
		uid (if org-icalendar-store-UID
			(org-id-get-create)
		      (or (org-id-get) (org-id-new)))
		categories (org-export-get-categories)
		deadlinep nil scheduledp nil)
	  (if (looking-at re2)
	      (progn
		(goto-char (match-end 0))
		(setq ts2 (match-string 1)
		      inc (not (string-match "[0-9]\\{1,2\\}:[0-9][0-9]" ts2))))
	    (setq tmp (buffer-substring (max (point-min)
					     (- pos org-ds-keyword-length))
					pos)
		  ts2 (if (string-match "[0-9]\\{1,2\\}:[0-9][0-9]-\\([0-9]\\{1,2\\}:[0-9][0-9]\\)" ts)
			  (progn
			    (setq inc nil)
			    (replace-match "\\1" t nil ts))
			ts)
		  deadlinep (string-match org-deadline-regexp tmp)
		  scheduledp (string-match org-scheduled-regexp tmp)
		  todo (org-get-todo-state)
		  ;; donep (org-entry-is-done-p)
		  ))
	  (when (and
		 deadlinep
		 (if todo
		     (not (memq 'event-if-todo org-icalendar-use-deadline))
		   (not (memq 'event-if-not-todo org-icalendar-use-deadline))))
	    (throw :skip t))
	  (when (and
		 scheduledp
		 (if todo
		     (not (memq 'event-if-todo org-icalendar-use-scheduled))
		   (not (memq 'event-if-not-todo org-icalendar-use-scheduled))))
	    (throw :skip t))
	  (setq prefix (if deadlinep "DL-" (if scheduledp "SC-" "TS-")))
	  (if (or (string-match org-tr-regexp hd)
		  (string-match org-ts-regexp hd))
	      (setq hd (replace-match "" t t hd)))
	  (if (string-match "\\+\\([0-9]+\\)\\([dwmy]\\)>" ts)
	      (setq rrule
		    (concat "\nRRULE:FREQ="
			    (cdr (assoc
				  (match-string 2 ts)
				  '(("d" . "DAILY")("w" . "WEEKLY")
				    ("m" . "MONTHLY")("y" . "YEARLY"))))
			    ";INTERVAL=" (match-string 1 ts)))
	    (setq rrule ""))
	  (setq summary (or summary hd))
	  (if (string-match org-bracket-link-regexp summary)
	      (setq summary
		    (replace-match (if (match-end 3)
				       (match-string 3 summary)
				     (match-string 1 summary))
				   t t summary)))
	  (if deadlinep (setq summary (concat "DL: " summary)))
	  (if scheduledp (setq summary (concat "S: " summary)))
	  (if (string-match "\\`<%%" ts)
	      (with-current-buffer sexp-buffer
		(insert (substring ts 1 -1) " " summary "\n"))
	    (princ (format "BEGIN:VEVENT
UID: %s
%s
%s%s
SUMMARY:%s%s%s
CATEGORIES:%s
END:VEVENT\n"
			   (concat prefix uid)
			   (org-ical-ts-to-string ts "DTSTART")
			   (org-ical-ts-to-string ts2 "DTEND" inc)
			   rrule summary
			   (if (and desc (string-match "\\S-" desc))
			       (concat "\nDESCRIPTION: " desc) "")
			   (if (and location (string-match "\\S-" location))
			       (concat "\nLOCATION: " location) "")
			   categories)))))
      (when (and org-icalendar-include-sexps
		 (condition-case nil (require 'icalendar) (error nil))
		 (fboundp 'icalendar-export-region))
	;; Get all the literal sexps
	(goto-char (point-min))
	(while (re-search-forward "^&?%%(" nil t)
	  (catch :skip
	    (org-agenda-skip)
	    (setq b (match-beginning 0))
	    (goto-char (1- (match-end 0)))
	    (forward-sexp 1)
	    (end-of-line 1)
	    (setq sexp (buffer-substring b (point)))
	    (with-current-buffer sexp-buffer
	      (insert sexp "\n"))))
	(princ (org-diary-to-ical-string sexp-buffer))
	(kill-buffer sexp-buffer))
      
      (when org-icalendar-include-todo
	(setq prefix "TODO-")
	(goto-char (point-min))
	(while (re-search-forward org-todo-line-regexp nil t)
	  (catch :skip
	    (org-agenda-skip)
	    (when (boundp 'org-icalendar-verify-function)
	      (unless (funcall org-icalendar-verify-function)
		(outline-next-heading)
		(backward-char 1)
		(throw :skip nil)))
	    (setq state (match-string 2))
	    (setq status (if (member state org-done-keywords)
			     "COMPLETED" "NEEDS-ACTION"))
	    (when (and state
		       (or (not (member state org-done-keywords))
			   (eq org-icalendar-include-todo 'all))
		       (not (member org-archive-tag (org-get-tags-at)))
		       )
	      (setq hd (match-string 3)
		    summary (org-icalendar-cleanup-string
			     (org-entry-get nil "SUMMARY"))
		    desc (org-icalendar-cleanup-string
			  (or (org-entry-get nil "DESCRIPTION")
			      (and org-icalendar-include-body (org-get-entry)))
			  t org-icalendar-include-body)
		    location (org-icalendar-cleanup-string
			      (org-entry-get nil "LOCATION"))
		    due (and (member 'todo-due org-icalendar-use-deadline)
			     (org-entry-get nil "DEADLINE"))
		    start (and (member 'todo-start org-icalendar-use-scheduled)
			     (org-entry-get nil "SCHEDULED"))
		    categories (org-export-get-categories)
		    uid (if org-icalendar-store-UID
			    (org-id-get-create)
			  (or (org-id-get) (org-id-new))))
	      (and due (setq due (org-ical-ts-to-string due "DUE")))
	      (and start (setq start (org-ical-ts-to-string start "DTSTART")))

	      (if (string-match org-bracket-link-regexp hd)
		  (setq hd (replace-match (if (match-end 3) (match-string 3 hd)
					    (match-string 1 hd))
					  t t hd)))
	      (if (string-match org-priority-regexp hd)
		  (setq pri (string-to-char (match-string 2 hd))
			hd (concat (substring hd 0 (match-beginning 1))
				   (substring hd (match-end 1))))
		(setq pri org-default-priority))
	      (setq pri (floor (1+ (* 8. (/ (float (- org-lowest-priority pri))
					    (- org-lowest-priority org-highest-priority))))))

	      (princ (format "BEGIN:VTODO
UID: %s
%s
SUMMARY:%s%s%s%s
CATEGORIES:%s
SEQUENCE:1
PRIORITY:%d
STATUS:%s
END:VTODO\n"
			     (concat prefix uid)
			     (or start dts)
			     (or summary hd)
			     (if (and location (string-match "\\S-" location))
				 (concat "\nLOCATION: " location) "")
			     (if (and desc (string-match "\\S-" desc))
				 (concat "\nDESCRIPTION: " desc) "")
			     (if due (concat "\n" due) "")
			     categories
			     pri status)))))))))

(defun org-export-get-categories ()
  "Get categories according to `org-icalendar-categories'."
  (let ((cs org-icalendar-categories) c rtn tmp)
    (while (setq c (pop cs))
      (cond
       ((eq c 'category) (push (org-get-category) rtn))
       ((eq c 'todo-state)
	(setq tmp (org-get-todo-state))
	(and tmp (push tmp rtn)))
       ((eq c 'local-tags)
	(setq rtn (append (nreverse (org-get-local-tags-at (point))) rtn)))
       ((eq c 'all-tags)
	(setq rtn (append (nreverse (org-get-tags-at (point))) rtn)))))
    (mapconcat 'identity (nreverse rtn) ",")))

(defun org-icalendar-cleanup-string (s &optional is-body maxlength)
  "Take out stuff and quote what needs to be quoted.
When IS-BODY is non-nil, assume that this is the body of an item, clean up
whitespace, newlines, drawers, and timestamps, and cut it down to MAXLENGTH
characters."
  (if (not s)
      nil
    (when is-body
      (let ((re (concat "\\(" org-drawer-regexp "\\)[^\000]*?:END:.*\n?"))
	    (re2 (concat "^[ \t]*" org-keyword-time-regexp ".*\n?")))
	(while (string-match re s) (setq s (replace-match "" t t s)))
	(while (string-match re2 s) (setq s (replace-match "" t t s)))))
    (let ((start 0))
      (while (string-match "\\([,;]\\)" s start)
	(setq start (+ (match-beginning 0) 2)
	      s (replace-match "\\\\\\1" nil nil s))))
    (when is-body
      (while (string-match "[ \t]*\n[ \t]*" s)
	(setq s (replace-match "\\n" t t s))))
    (setq s (org-trim s))
    (if is-body
	(if maxlength
	    (if (and (numberp maxlength)
		     (> (length s) maxlength))
		(setq s (substring s 0 maxlength)))))
    s))

(defun org-get-entry ()
  "Clean-up description string."
  (save-excursion
    (org-back-to-heading t)
    (buffer-substring (point-at-bol 2) (org-end-of-subtree t))))

(defun org-start-icalendar-file (name)
  "Start an iCalendar file by inserting the header."
  (let ((user user-full-name)
	(name (or name "unknown"))
	(timezone (cadr (current-time-zone))))
    (princ
     (format "BEGIN:VCALENDAR
VERSION:2.0
X-WR-CALNAME:%s
PRODID:-//%s//Emacs with Org-mode//EN
X-WR-TIMEZONE:%s
CALSCALE:GREGORIAN\n" name user timezone))))

(defun org-finish-icalendar-file ()
  "Finish an iCalendar file by inserting the END statement."
  (princ "END:VCALENDAR\n"))

(defun org-ical-ts-to-string (s keyword &optional inc)
  "Take a time string S and convert it to iCalendar format.
KEYWORD is added in front, to make a complete line like DTSTART....
When INC is non-nil, increase the hour by two (if time string contains
a time), or the day by one (if it does not contain a time)."
  (let ((t1 (org-parse-time-string s 'nodefault))
	t2 fmt have-time time)
    (if (and (car t1) (nth 1 t1) (nth 2 t1))
	(setq t2 t1 have-time t)
      (setq t2 (org-parse-time-string s)))
    (let ((s (car t2))   (mi (nth 1 t2)) (h (nth 2 t2))
	  (d (nth 3 t2)) (m  (nth 4 t2)) (y (nth 5 t2)))
      (when inc
	(if have-time
	    (if org-agenda-default-appointment-duration
		(setq mi (+ org-agenda-default-appointment-duration mi))
	      (setq h (+ 2 h)))
	  (setq d (1+ d))))
      (setq time (encode-time s mi h d m y)))
    (setq fmt (if have-time ":%Y%m%dT%H%M%S" ";VALUE=DATE:%Y%m%d"))
    (concat keyword (format-time-string fmt time))))

;;; XOXO export

(defun org-export-as-xoxo-insert-into (buffer &rest output)
  (with-current-buffer buffer
    (apply 'insert output)))
(put 'org-export-as-xoxo-insert-into 'lisp-indent-function 1)

;;;###autoload
(defun org-export-as-xoxo (&optional buffer)
  "Export the org buffer as XOXO.
The XOXO buffer is named *xoxo-<source buffer name>*"
  (interactive (list (current-buffer)))
  ;; A quickie abstraction

  ;; Output everything as XOXO
  (with-current-buffer (get-buffer buffer)
    (let* ((pos (point))
	   (opt-plist (org-combine-plists (org-default-export-plist)
					(org-infile-export-plist)))
	   (filename (concat (file-name-as-directory
			      (org-export-directory :xoxo opt-plist))
			     (file-name-sans-extension
			      (file-name-nondirectory buffer-file-name))
			     ".html"))
	   (out (find-file-noselect filename))
	   (last-level 1)
	   (hanging-li nil))
      (goto-char (point-min))  ;; CD:  beginning-of-buffer is not allowed.
      ;; Check the output buffer is empty.
      (with-current-buffer out (erase-buffer))
      ;; Kick off the output
      (org-export-as-xoxo-insert-into out "<ol class='xoxo'>\n")
      (while (re-search-forward "^\\(\\*+\\)[ \t]+\\(.+\\)" (point-max) 't)
        (let* ((hd (match-string-no-properties 1))
               (level (length hd))
               (text (concat
                      (match-string-no-properties 2)
                      (save-excursion
                        (goto-char (match-end 0))
                        (let ((str ""))
                          (catch 'loop
                            (while 't
                              (forward-line)
                              (if (looking-at "^[ \t]\\(.*\\)")
                                  (setq str (concat str (match-string-no-properties 1)))
                                (throw 'loop str)))))))))

          ;; Handle level rendering
          (cond
           ((> level last-level)
            (org-export-as-xoxo-insert-into out "\n<ol>\n"))

           ((< level last-level)
            (dotimes (- (- last-level level) 1)
              (if hanging-li
                  (org-export-as-xoxo-insert-into out "</li>\n"))
              (org-export-as-xoxo-insert-into out "</ol>\n"))
            (when hanging-li
              (org-export-as-xoxo-insert-into out "</li>\n")
              (setq hanging-li nil)))

           ((equal level last-level)
            (if hanging-li
                (org-export-as-xoxo-insert-into out "</li>\n")))
           )

          (setq last-level level)

          ;; And output the new li
          (setq hanging-li 't)
          (if (equal ?+ (elt text 0))
              (org-export-as-xoxo-insert-into out "<li class='" (substring text 1) "'>")
            (org-export-as-xoxo-insert-into out "<li>" text))))

      ;; Finally finish off the ol
      (dotimes (- last-level 1)
        (if hanging-li
            (org-export-as-xoxo-insert-into out "</li>\n"))
        (org-export-as-xoxo-insert-into out "</ol>\n"))

      (goto-char pos)
      ;; Finish the buffer off and clean it up.
      (switch-to-buffer-other-window out)
      (indent-region (point-min) (point-max) nil)
      (save-buffer)
      (goto-char (point-min))
      )))

(provide 'org-exp)

;; arch-tag: 65985fe9-095c-49c7-a7b6-cb4ee15c0a95

;;; org-exp.el ends here
