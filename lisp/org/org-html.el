;;; org-html.el --- HTML export for Org-mode

;; Copyright (C) 2004, 2005, 2006, 2007, 2008, 2009, 2010
;;   Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten at orgmode dot org>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;; Version: 6.35i
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

(require 'org-exp)
(eval-when-compile (require 'cl))

(declare-function org-id-find-id-file "org-id" (id))
(declare-function htmlize-region "ext:htmlize" (beg end))

(defgroup org-export-html nil
  "Options specific for HTML export of Org-mode files."
  :tag "Org Export HTML"
  :group 'org-export)

(defcustom org-export-html-footnotes-section "<div id=\"footnotes\">
<h2 class=\"footnotes\">%s: </h2>
<div id=\"text-footnotes\">
%s
</div>
</div>"
  "Format for the footnotes section.
Should contain a two instances of %s.  The first will be replaced with the
language-specific word for \"Footnotes\", the second one will be replaced
by the footnotes themselves."
  :group 'org-export-html
  :type 'string)

(defcustom org-export-html-footnote-format "<sup>%s</sup>"
  "The format for the footnote reference.
%s will be replaced by the footnote reference itself."
  :group 'org-export-html
  :type 'string)

(defcustom org-export-html-coding-system nil
  "Coding system for HTML export, defaults to buffer-file-coding-system."
  :group 'org-export-html
  :type 'coding-system)

(defcustom org-export-html-extension "html"
  "The extension for exported HTML files."
  :group 'org-export-html
  :type 'string)

(defcustom org-export-html-xml-declaration
  '(("html" . "<?xml version=\"1.0\" encoding=\"%s\"?>")
    ("php" . "<?php echo \"<?xml version=\\\"1.0\\\" encoding=\\\"%s\\\" ?>\"; ?>"))
  "The extension for exported HTML files.
%s will be replaced with the charset of the exported file.
This may be a string, or an alist with export extensions
and corresponding declarations."
  :group 'org-export-html
  :type '(choice
	  (string :tag "Single declaration")
	  (repeat :tag "Dependent on extension"
		  (cons (string :tag "Extension")
			(string :tag "Declaration")))))

(defcustom org-export-html-style-include-scripts t
  "Non-nil means include the javascript snippets in exported HTML files.
The actual script is defined in `org-export-html-scripts' and should
not be modified."
  :group 'org-export-html
  :type 'boolean)

(defconst org-export-html-scripts
"<script type=\"text/javascript\">
<!--/*--><![CDATA[/*><!--*/
 function CodeHighlightOn(elem, id)
 {
   var target = document.getElementById(id);
   if(null != target) {
     elem.cacheClassElem = elem.className;
     elem.cacheClassTarget = target.className;
     target.className = \"code-highlighted\";
     elem.className   = \"code-highlighted\";
   }
 }
 function CodeHighlightOff(elem, id)
 {
   var target = document.getElementById(id);
   if(elem.cacheClassElem)
     elem.className = elem.cacheClassElem;
   if(elem.cacheClassTarget)
     target.className = elem.cacheClassTarget;
 }
/*]]>*///-->
</script>"
"Basic javascript that is needed by HTML files produced by Org-mode.")

(defconst org-export-html-style-default
"<style type=\"text/css\">
 <!--/*--><![CDATA[/*><!--*/
  html { font-family: Times, serif; font-size: 12pt; }
  .title  { text-align: center; }
  .todo   { color: red; }
  .done   { color: green; }
  .tag    { background-color: #add8e6; font-weight:normal }
  .target { }
  .timestamp { color: #bebebe; }
  .timestamp-kwd { color: #5f9ea0; }
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
  div.figure { padding: 0.5em; }
  div.figure p { text-align: center; }
  textarea { overflow-x: auto; }
  .linenr { font-size:smaller }
  .code-highlighted {background-color:#ffff00;}
  .org-info-js_info-navigation { border-style:none; }
  #org-info-js_console-label { font-size:10px; font-weight:bold;
                               white-space:nowrap; }
  .org-info-js_search-highlight {background-color:#ffff00; color:#000000;
                                 font-weight:bold; }
  /*]]>*/-->
</style>"
  "The default style specification for exported HTML files.
Please use the variables `org-export-html-style' and
`org-export-html-style-extra' to add to this style.  If you wish to not
have the default style included, customize the variable
`org-export-html-style-include-default'.")

(defcustom org-export-html-style-include-default t
  "Non-nil means include the default style in exported HTML files.
The actual style is defined in `org-export-html-style-default' and should
not be modified.  Use the variables `org-export-html-style' to add
your own style information."
  :group 'org-export-html
  :type 'boolean)
;;;###autoload
(put 'org-export-html-style-include-default 'safe-local-variable 'booleanp)

(defcustom org-export-html-style ""
  "Org-wide style definitions for exported HTML files.

This variable needs to contain the full HTML structure to provide a style,
including the surrounding HTML tags.  If you set the value of this variable,
you should consider to include definitions for the following classes:
 title, todo, done, timestamp, timestamp-kwd, tag, target.

For example, a valid value would be:

   <style type=\"text/css\">
    <![CDATA[
       p { font-weight: normal; color: gray; }
       h1 { color: black; }
      .title { text-align: center; }
      .todo, .timestamp-kwd { color: red; }
      .done { color: green; }
    ]]>
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

(defcustom org-export-html-tag-class-prefix ""
  "Prefix to clas names for TODO keywords.
Each tag gets a class given by the tag itself, with this prefix.
The default prefix is empty because it is nice to just use the keyword
as a class name.  But if you get into conflicts with other, existing
CSS classes, then this prefic can be very useful."
  :group 'org-export-html
  :type 'string)

(defcustom org-export-html-todo-kwd-class-prefix ""
  "Prefix to clas names for TODO keywords.
Each TODO keyword gets a class given by the keyword itself, with this prefix.
The default prefix is empty because it is nice to just use the keyword
as a class name.  But if you get into conflicts with other, existing
CSS classes, then this prefic can be very useful."
  :group 'org-export-html
  :type 'string)

(defcustom org-export-html-title-format "<h1 class=\"title\">%s</h1>\n"
  "Format for typesetting the document title in HTML export."
  :group 'org-export-html
  :type 'string)

(defcustom org-export-html-home/up-format
  "<div id=\"org-div-home-and-up\" style=\"text-align:right;font-size:70%%;white-space:nowrap;\">
 <a accesskey=\"h\" href=\"%s\"> UP </a>
 |
 <a accesskey=\"H\" href=\"%s\"> HOME </a>
</div>"
  "Snippet used to insert the HOME and UP links.  This is a format,
the first %s will receive the UP link, the second the HOME link.
If both `org-export-html-link-up' and `org-export-html-link-home' are
empty, the entire snippet will be ignored."
  :group 'org-export-html
  :type 'string)

(defcustom org-export-html-toplevel-hlevel 2
  "The <H> level for level 1 headings in HTML export.
This is also important for the classes that will be wrapped around headlines
and outline structure.  If this variable is 1, the top-level headlines will
be <h1>, and the corresponding classes will be outline-1, section-number-1,
and outline-text-1.  If this is 2, all of these will get a 2 instead.
The default for this variable is 2, because we use <h1> for formatting the
document title."
  :group 'org-export-html
  :type 'string)

(defcustom org-export-html-link-org-files-as-html t
  "Non-nil means make file links to `file.org' point to `file.html'.
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
  "Non-nil means inline images into exported HTML pages.
This is done using an <img> tag.  When nil, an anchor with href is used to
link to the image.  If this option is `maybe', then images in links with
an empty description will be inlined, while images with a description will
be linked only."
  :group 'org-export-html
  :type '(choice (const :tag "Never" nil)
		 (const :tag "Always" t)
		 (const :tag "When there is no description" maybe)))

(defcustom org-export-html-inline-image-extensions
  '("png" "jpeg" "jpg" "gif")
  "Extensions of image files that can be inlined into HTML."
  :group 'org-export-html
  :type '(repeat (string :tag "Extension")))

(defcustom org-export-html-table-tag
  "<table border=\"2\" cellspacing=\"0\" cellpadding=\"6\" rules=\"groups\" frame=\"hsides\">"
  "The HTML tag that is used to start a table.
This must be a <table> tag, but you may change the options like
borders and spacing."
  :group 'org-export-html
  :type 'string)

(defcustom org-export-table-header-tags '("<th scope=\"%s\">" . "</th>")
  "The opening tag for table header fields.
This is customizable so that alignment options can be specified.
%s will be filled with the scope of the field, either row or col.
See also the variable `org-export-html-table-use-header-tags-for-first-column'."
  :group 'org-export-tables
  :type '(cons (string :tag "Opening tag") (string :tag "Closing tag")))

(defcustom org-export-table-data-tags '("<td>" . "</td>")
  "The opening tag for table data fields.
This is customizable so that alignment options can be specified."
  :group 'org-export-tables
  :type '(cons (string :tag "Opening tag") (string :tag "Closing tag")))

(defcustom org-export-table-row-tags '("<tr>" . "</tr>")
  "The opening tag for table data fields.
This is customizable so that alignment options can be specified.
Instead of strings, these can be Lisp forms that will be evaluated
for each row in order to construct the table row tags.  During evaluation,
the variable `head' will be true when this is a header line, nil when this
is a body line.  And the variable `nline' will contain the line number,
starting from 1 in the first header line.  For example

  (setq org-export-table-row-tags
        (cons '(if head
                   \"<tr>\"
                 (if (= (mod nline 2) 1)
                     \"<tr class=\\\"tr-odd\\\">\"
                   \"<tr class=\\\"tr-even\\\">\"))
              \"</tr>\"))

will give even lines the class \"tr-even\" and odd lines the class \"tr-odd\"."
  :group 'org-export-tables
  :type '(cons
	  (choice :tag "Opening tag"
		  (string :tag "Specify")
		  (sexp))
	  (choice :tag "Closing tag"
		  (string :tag "Specify")
		  (sexp))))



(defcustom org-export-html-table-use-header-tags-for-first-column nil
  "Non-nil means format column one in tables with header tags.
When nil, also column one will use data tags."
  :group 'org-export-tables
  :type 'boolean)

(defcustom org-export-html-validation-link nil
  "Non-nil means add validationlink to postamble of HTML exported files."
  :group 'org-export-html
  :type '(choice
	  (const :tag "Nothing" nil)
	  (const :tag "XHTML 1.0" "<p class=\"xhtml-validation\"><a href=\"http://validator.w3.org/check?uri=referer\">Validate XHTML 1.0</a></p>")
	  (string :tag "Specify full HTML")))


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
We use as default  `inline-css', in order to make the resulting
HTML self-containing.
However, this will fail when using Emacs in batch mode for export, because
then no rich font definitions are in place.  It will also not be good if
people with different Emacs setup contribute HTML files to a website,
because the fonts will represent the individual setups.  In these cases,
it is much better to let Org/Htmlize assign classes only, and to use
a style file to define the look of these classes.
To get a start for your css file, start Emacs session and make sure that
all the faces you are interested in are defined, for example by loading files
in all modes you want.  Then, use the command
\\[org-export-htmlize-generate-css] to extract class definitions."
  :group 'org-export-htmlize
  :type '(choice (const css) (const inline-css)))

(defcustom org-export-htmlize-css-font-prefix "org-"
  "The prefix for CSS class names for htmlize font specifications."
  :group 'org-export-htmlize
  :type 'string)

(defcustom org-export-htmlized-org-css-url nil
  "URL pointing to a CSS file defining text colors for htmlized Emacs buffers.
Normally when creating an htmlized version of an Org buffer, htmlize will
create CSS to define the font colors.  However, this does not work when
converting in batch mode, and it also can look bad if different people
with different fontification setup work on the same website.
When this variable is non-nil, creating an htmlized version of an Org buffer
using `org-export-as-org' will remove the internal CSS section and replace it
with a link to this URL."
  :group 'org-export-htmlize
  :type '(choice
	  (const :tag "Keep internal css" nil)
	  (string :tag "URL or local href")))

;;; Variables, constants, and parameter plists

(defvar org-export-html-preamble nil
  "Preamble, to be inserted just before <body>.  Set by publishing functions.
This may also be a function, building and inserting the preamble.")
(defvar org-export-html-postamble nil
  "Preamble, to be inserted just after </body>.  Set by publishing functions.
This may also be a function, building and inserting the postamble.")
(defvar org-export-html-auto-preamble t
  "Should default preamble be inserted?  Set by publishing functions.")
(defvar org-export-html-auto-postamble t
  "Should default postamble be inserted?  Set by publishing functions.")

;;; Hooks

(defvar org-export-html-after-blockquotes-hook nil
  "Hook run during HTML export, after blockquote, verse, center are done.")

(defvar org-export-html-final-hook nil
  "Hook run at the end of HTML export, in the new buffer.")

;;; HTML export

(defun org-export-html-preprocess (parameters)
  ;; Convert LaTeX fragments to images
  (when (and org-current-export-file
	     (plist-get parameters :LaTeX-fragments))
    (org-format-latex
     (concat "ltxpng/" (file-name-sans-extension
			(file-name-nondirectory
			 org-current-export-file)))
     org-current-export-dir nil "Creating LaTeX image %s"))
  (goto-char (point-min))
  (let (label l1)
    (while (re-search-forward "\\\\ref{\\([^{}\n]+\\)}" nil t)
      (org-if-unprotected-at (match-beginning 1)
	(setq label (match-string 1))
	(save-match-data
	  (if (string-match "\\`[a-z]\\{1,10\\}:\\(.+\\)" label)
	      (setq l1 (substring label (match-beginning 1)))
	    (setq l1 label)))
	(replace-match (format "[[#%s][%s]]" label l1) t t)))))

;;;###autoload
(defun org-export-as-html-and-open (arg)
  "Export the outline as HTML and immediately open it with a browser.
If there is an active region, export only the region.
The prefix ARG specifies how many levels of the outline should become
headlines.  The default is 3.  Lower levels will become bulleted lists."
  (interactive "P")
  (org-export-as-html arg 'hidden)
  (org-open-file buffer-file-name)
  (when org-export-kill-product-buffer-when-displayed
    (kill-buffer)))

;;;###autoload
(defun org-export-as-html-batch ()
  "Call `org-export-as-html', may be used in batch processing as
emacs   --batch
        --load=$HOME/lib/emacs/org.el
        --eval \"(setq org-export-headline-levels 2)\"
        --visit=MyFile --funcall org-export-as-html-batch"
  (org-export-as-html org-export-headline-levels 'hidden))

;;;###autoload
(defun org-export-as-html-to-buffer (arg)
  "Call `org-export-as-html` with output to a temporary buffer.
No file is created.  The prefix ARG is passed through to `org-export-as-html'."
  (interactive "P")
  (org-export-as-html arg nil nil "*Org HTML Export*")
  (when org-export-show-temporary-export-buffer
    (switch-to-buffer-other-window "*Org HTML Export*")))

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
    (setq ext-plist (plist-put ext-plist :ignore-subtree-p t))
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
(defvar org-par-open nil)
;;;###autoload
(defun org-export-as-html (arg &optional hidden ext-plist
			       to-buffer body-only pub-dir)
  "Export the outline as a pretty HTML file.
If there is an active region, export only the region.  The prefix
ARG specifies how many levels of the outline should become
headlines.  The default is 3.  Lower levels will become bulleted
lists.  HIDDEN is obsolete and does nothing.
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
  (run-hooks 'org-export-first-hook)

  ;; Make sure we have a file name when we need it.
  (when (and (not (or to-buffer body-only))
	     (not buffer-file-name))
    (if (buffer-base-buffer)
	(org-set-local 'buffer-file-name
		       (with-current-buffer (buffer-base-buffer)
			 buffer-file-name))
      (error "Need a file name to be able to export")))

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
	 (body-only (or body-only (plist-get opt-plist :body-only)))
	 (style (concat (if (plist-get opt-plist :style-include-default)
			    org-export-html-style-default)
			(plist-get opt-plist :style)
			(plist-get opt-plist :style-extra)
			"\n"
			(if (plist-get opt-plist :style-include-scripts)
			    org-export-html-scripts)))
	 (html-extension (plist-get opt-plist :html-extension))
	 (link-validate (plist-get opt-plist :link-validation-function))
	 valid thetoc have-headings first-heading-pos
	 (odd org-odd-levels-only)
	 (region-p (org-region-active-p))
	 (rbeg (and region-p (region-beginning)))
	 (rend (and region-p (region-end)))
	 (subtree-p
	  (if (plist-get opt-plist :ignore-subtree-p)
	      nil
	    (when region-p
	      (save-excursion
		(goto-char rbeg)
		(and (org-at-heading-p)
		     (>= (org-end-of-subtree t t) rend))))))
	 (level-offset (if subtree-p
			   (save-excursion
			     (goto-char rbeg)
			     (+ (funcall outline-level)
				(if org-odd-levels-only 1 0)))
			 0))
	 (opt-plist (setq org-export-opt-plist
			  (if subtree-p
			      (org-export-add-subtree-options opt-plist rbeg)
			    opt-plist)))
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
			  (and (not body-only)
			       (not
				(plist-get opt-plist :skip-before-1st-heading))
			       (org-export-grab-title-from-buffer))
			  (and buffer-file-name
			       (file-name-sans-extension
				(file-name-nondirectory buffer-file-name)))
			  "UNTITLED"))
	 (link-up (and (plist-get opt-plist :link-up)
		       (string-match "\\S-" (plist-get opt-plist :link-up))
		       (plist-get opt-plist :link-up)))
	 (link-home (and (plist-get opt-plist :link-home)
			(string-match "\\S-" (plist-get opt-plist :link-home))
			(plist-get opt-plist :link-home)))
	 (dummy (setq opt-plist (plist-put opt-plist :title title)))
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
	 (keywords    (plist-get opt-plist :keywords))
	 (description (plist-get opt-plist :description))
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
	    :todo-keywords (plist-get opt-plist :todo-keywords)
	    :tags (plist-get opt-plist :tags)
	    :priority (plist-get opt-plist :priority)
	    :footnotes (plist-get opt-plist :footnotes)
	    :timestamps (plist-get opt-plist :timestamps)
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
	 footnotes footref-seen
	 id-file href
	 )

    (let ((inhibit-read-only t))
      (org-unmodified
       (remove-text-properties (point-min) (point-max)
			       '(:org-license-to-kill t))))

    (message "Exporting...")

    (setq org-min-level (org-get-min-level lines level-offset))
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
    (org-install-letbind)

    (and (fboundp 'set-buffer-file-coding-system)
	 (set-buffer-file-coding-system coding-system-for-write))

    (let ((case-fold-search nil)
	  (org-odd-levels-only odd))
      ;; create local variables for all options, to make sure all called
      ;; functions get the correct information
      (mapc (lambda (x)
	      (set (make-local-variable (nth 2 x))
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
		 "%s
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
               \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\"
lang=\"%s\" xml:lang=\"%s\">
<head>
<title>%s</title>
<meta http-equiv=\"Content-Type\" content=\"text/html;charset=%s\"/>
<meta name=\"generator\" content=\"Org-mode\"/>
<meta name=\"generated\" content=\"%s\"/>
<meta name=\"author\" content=\"%s\"/>
<meta name=\"description\" content=\"%s\"/>
<meta name=\"keywords\" content=\"%s\"/>
%s
</head>
<body>
<div id=\"content\">
%s
"
		 (format
		  (or (and (stringp org-export-html-xml-declaration)
			   org-export-html-xml-declaration)
		      (cdr (assoc html-extension org-export-html-xml-declaration))
		      (cdr (assoc "html" org-export-html-xml-declaration))

		      "")
		  (or charset "iso-8859-1"))
		 language language
		 (org-html-expand title)
		 (or charset "iso-8859-1")
		 date author description keywords
		 style
		 (if (or link-up link-home)
		     (concat
		      (format org-export-html-home/up-format
			      (or link-up link-home)
			      (or link-home link-up))
		      "\n")
		   "")))

        (org-export-html-insert-plist-item opt-plist :preamble opt-plist)

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
		    (if (and (string-match org-todo-line-regexp line)
			     (not (get-text-property 0 'org-protected line)))
			;; This is a headline
			(progn
			  (setq have-headings t)
			  (setq level (- (match-end 1) (match-beginning 1)
					 level-offset)
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
				(setq href (format "sec-%s" snumber))
				(setq href (or (cdr (assoc href org-export-preferred-target-alist)) href))
				(push
				 (format
				  (if todo
				      "</li>\n<li><a href=\"#%s\"><span class=\"todo\">%s</span></a>"
				    "</li>\n<li><a href=\"#%s\">%s</a>")
				  href txt) thetoc)

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
	    (org-open-par)
	    (setq inquote nil))
	  ;; inside a quote section?
	  (when inquote
	    (insert (org-html-protect line) "\n")
	    (throw 'nextline nil))

	  ;; Fixed-width, verbatim lines (examples)
	  (when (and org-export-with-fixed-width
		     (string-match "^[ \t]*:\\(\\([ \t]\\|$\\)\\(.*\\)\\)" line))
	    (when (not infixed)
	      (setq infixed t)
	      (org-close-par-maybe)

	      (insert "<pre class=\"example\">\n"))
	    (insert (org-html-protect (match-string 3 line)) "\n")
	    (when (or (not lines)
		      (not (string-match "^[ \t]*:\\(\\([ \t]\\|$\\)\\(.*\\)\\)"
					 (car lines))))
	      (setq infixed nil)
	      (insert "</pre>\n")
	      (org-open-par))
	    (throw 'nextline nil))

	  (org-export-html-close-lists-maybe line)

	  ;; Protected HTML
	  (when (get-text-property 0 'org-protected line)
	    (let (par (ind (get-text-property 0 'original-indentation line)))
	      (when (re-search-backward
		     "\\(<p>\\)\\([ \t\r\n]*\\)\\=" (- (point) 100) t)
		(setq par (match-string 1))
		(replace-match "\\2\n"))
	      (insert line "\n")
	      (while (and lines
			  (or (= (length (car lines)) 0)
			      (not ind)
			      (equal ind (get-text-property 0 'original-indentation (car lines))))
			  (or (= (length (car lines)) 0)
			      (get-text-property 0 'org-protected (car lines))))
		(insert (pop lines) "\n"))
	      (and par (insert "<p>\n")))
	    (throw 'nextline nil))

	  ;; Blockquotes, verse, and center
	  (when (equal "ORG-BLOCKQUOTE-START" line)
	    (org-close-par-maybe)
	    (insert "<blockquote>\n")
	    (org-open-par)
	    (throw 'nextline nil))
	  (when (equal "ORG-BLOCKQUOTE-END" line)
	    (org-close-par-maybe)
	    (insert "\n</blockquote>\n")
	    (org-open-par)
	    (throw 'nextline nil))
	  (when (equal "ORG-VERSE-START" line)
	    (org-close-par-maybe)
	    (insert "\n<p class=\"verse\">\n")
	    (setq inverse t)
	    (throw 'nextline nil))
	  (when (equal "ORG-VERSE-END" line)
	    (insert "</p>\n")
	    (org-open-par)
	    (setq inverse nil)
	    (throw 'nextline nil))
	  (when (equal "ORG-CENTER-START" line)
	    (org-close-par-maybe)
	    (insert "\n<div style=\"text-align: center\">")
	    (org-open-par)
	    (throw 'nextline nil))
	  (when (equal "ORG-CENTER-END" line)
	    (org-close-par-maybe)
	    (insert "\n</div>")
	    (org-open-par)
	    (throw 'nextline nil))
	  (run-hooks 'org-export-html-after-blockquotes-hook)
	  (when inverse
	    (let ((i (org-get-string-indentation line)))
	      (if (> i 0)
		  (setq line (concat (mapconcat 'identity
						(make-list (* 2 i) "\\nbsp") "")
				     " " (org-trim line))))
	      (unless (string-match "\\\\\\\\[ \t]*$" line)
		(setq line (concat line "\\\\")))))

	  ;; make targets to anchors
	  (setq start 0)
	  (while (string-match
		  "<<<?\\([^<>]*\\)>>>?\\((INVISIBLE)\\)?[ \t]*\n?" line start)
	    (cond
	     ((get-text-property (match-beginning 1) 'org-protected line)
	      (setq start (match-end 1)))
	     ((match-end 2)
	      (setq line (replace-match
			  (format
			   "@<a name=\"%s\" id=\"%s\">@</a>"
			   (org-solidify-link-text (match-string 1 line))
			   (org-solidify-link-text (match-string 1 line)))
			  t t line)))
	     ((and org-export-with-toc (equal (string-to-char line) ?*))
	      ;; FIXME: NOT DEPENDENT on TOC?????????????????????
	      (setq line (replace-match
			  (concat "@<span class=\"target\">"
				  (match-string 1 line) "@</span> ")
			  ;; (concat "@<i>" (match-string 1 line) "@</i> ")
			  t t line)))
	     (t
	      (setq line (replace-match
			  (concat "@<a name=\""
				  (org-solidify-link-text (match-string 1 line))
				  "\" class=\"target\">" (match-string 1 line)
				  "@</a> ")
			  t t line)))))

	  (setq line (org-html-handle-time-stamps line))

	  ;; replace "&" by "&amp;", "<" and ">" by "&lt;" and "&gt;"
	  ;; handle @<..> HTML tags (replace "@&gt;..&lt;" by "<..>")
	  ;; Also handle sub_superscripts and checkboxes
	  (or (string-match org-table-hline-regexp line)
	      (setq line (org-html-expand line)))

	  ;; Format the links
	  (setq start 0)
	  (while (string-match org-bracket-link-analytic-regexp++ line start)
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
	    (setq path (org-extract-attributes (org-link-unescape path)))
	    (setq attr (get-text-property 0 'org-attributes path))
	    (setq desc1 (if (match-end 5) (match-string 5 line))
		  desc2 (if (match-end 2) (concat type ":" path) path)
		  descp (and desc1 (not (equal desc1 desc2)))
		  desc (or desc1 desc2))
	    ;; Make an image out of the description if that is so wanted
	    (when (and descp (org-file-image-p
			      desc org-export-html-inline-image-extensions))
	      (save-match-data
		(if (string-match "^file:" desc)
		    (setq desc (substring desc (match-end 0)))))
	      (setq desc (org-add-props
			     (concat "<img src=\"" desc "\"/>")
			     '(org-protected t))))
	    ;; FIXME: do we need to unescape here somewhere?
	    (cond
	     ((equal type "internal")
	      (setq rpl
		    (concat
		     "<a href=\""
		     (if (= (string-to-char path) ?#) "" "#")
		     (org-solidify-link-text
		      (save-match-data (org-link-unescape path)) nil)
		     "\"" attr ">"
		     (org-export-html-format-desc desc)
		     "</a>")))
	     ((and (equal type "id")
		   (setq id-file (org-id-find-id-file path)))
	      ;; This is an id: link to another file (if it was the same file,
	      ;; it would have become an internal link...)
	      (save-match-data
		(setq id-file (file-relative-name
			       id-file (file-name-directory org-current-export-file)))
		(setq id-file (concat (file-name-sans-extension id-file)
				      "." html-extension))
		(setq rpl (concat "<a href=\"" id-file "#"
				  (if (org-uuidgen-p path) "ID-")
				  path "\""
				  attr ">"
				  (org-export-html-format-desc desc)
				  "</a>"))))
	     ((member type '("http" "https"))
	      ;; standard URL, just check if we need to inline an image
	      (if (and (or (eq t org-export-html-inline-images)
			   (and org-export-html-inline-images (not descp)))
		       (org-file-image-p
			path org-export-html-inline-image-extensions))
		  (setq rpl (org-export-html-format-image
			     (concat type ":" path) org-par-open))
		(setq link (concat type ":" path))
		(setq rpl (concat "<a href=\""
				  (org-export-html-format-href link)
				  "\"" attr ">"
				  (org-export-html-format-desc desc)
				  "</a>"))))
	     ((member type '("ftp" "mailto" "news"))
	      ;; standard URL
	      (setq link (concat type ":" path))
	      (setq rpl (concat "<a href=\""
				(org-export-html-format-href link)
				"\"" attr ">"
				(org-export-html-format-desc desc)
				"</a>")))

	     ((string= type "coderef")
	      (setq rpl (format "<a href=\"#coderef-%s\" class=\"coderef\" onmouseover=\"CodeHighlightOn(this, 'coderef-%s');\" onmouseout=\"CodeHighlightOff(this, 'coderef-%s');\">%s</a>"
				path path path
				(format (org-export-get-coderef-format path (and descp desc))
					(cdr (assoc path org-export-code-refs))))))

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
		  (setq file-is-image-p
			(org-file-image-p
			 filename org-export-html-inline-image-extensions))
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
			(setq thefile
			      (concat thefile
				      (if (= (string-to-char search) ?#) "" "#")
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
			      (progn
				(message "image %s %s" thefile org-par-open)
				(org-export-html-format-image thefile org-par-open))
			    (concat "<a href=\"" thefile "\"" attr ">"
				    (org-export-html-format-desc desc)
				    "</a>")))
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
			    " " (match-string 2 line)
			    "\"> " (org-export-html-get-todo-kwd-class-name
				    (match-string 2 line))
			    "</span>" (substring line (match-end 2)))))

	  ;; Does this contain a reference to a footnote?
	  (when org-export-with-footnotes
	    (setq start 0)
	    (while (string-match "\\([^* \t].*?\\)\\[\\([0-9]+\\)\\]" line start)
	      (if (get-text-property (match-beginning 2) 'org-protected line)
		  (setq start (match-end 2))
		(let ((n (match-string 2 line)) extra a)
		  (if (setq a (assoc n footref-seen))
		      (progn
			(setcdr a (1+ (cdr a)))
			(setq extra (format ".%d" (cdr a))))
		    (setq extra "")
		    (push (cons n 1) footref-seen))
		  (setq line
			(replace-match
			 (format
			  (concat "%s"
			 	  (format org-export-html-footnote-format
			 		  "<a class=\"footref\" name=\"fnr.%s%s\" href=\"#fn.%s\">%s</a>"))
			  (or (match-string 1 line) "") n extra n n)
			 t t line))))))

	  (cond
	   ((string-match "^\\(\\*+\\)[ \t]+\\(.*\\)" line)
	    ;; This is a headline
	    (setq level (org-tr-level (- (match-end 1) (match-beginning 1)
					 level-offset))
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
	      (org-close-par-maybe)
	      (insert "<pre>")
	      (setq inquote t)))

	   ((string-match "^[ \t]*- __+[ \t]*$" line)
	    ;; Explicit list closure
	    (when local-list-type
	      (let ((ind (org-get-indentation line)))
		(while (and local-list-indent
			    (<= ind (car local-list-indent)))
		  (org-close-li (car local-list-type))
		  (insert (format "</%sl>\n" (car local-list-type)))
		  (pop local-list-type)
		  (pop local-list-indent))
		(or local-list-indent (setq in-local-list nil))))
	    (throw 'nextline nil))

	   ((and org-export-with-tables
		 (string-match "^\\([ \t]*\\)\\(|\\|\\+-+\\+\\)" line))
	    (when (not table-open)
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
	      (setq ind (or (get-text-property 0 'original-indentation line)
			    (org-get-string-indentation line))
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

	    ;; Horizontal line
	    (when (string-match "^[ \t]*-\\{5,\\}[ \t]*$" line)
	      (if org-par-open
		  (insert "\n</p>\n<hr/>\n<p>\n")
		(insert "\n<hr/>\n"))
	      (throw 'nextline nil))

	    ;; Empty lines start a new paragraph.  If hand-formatted lists
	    ;; are not fully interpreted, lines starting with "-", "+", "*"
	    ;; also start a new paragraph.
	    (if (string-match "^ [-+*]-\\|^[ \t]*$" line) (org-open-par))

	    ;; Is this the start of a footnote?
	    (when org-export-with-footnotes
	      (when (and (boundp 'footnote-section-tag-regexp)
			 (string-match (concat "^" footnote-section-tag-regexp)
				       line))
		;; ignore this line
		(throw 'nextline nil))
	      (when (string-match "^[ \t]*\\[\\([0-9]+\\)\\]" line)
		(org-close-par-maybe)
		(let ((n (match-string 1 line)))
		  (setq org-par-open t
			line (replace-match
			      (format
			       (concat "<p class=\"footnote\">"
				       (format org-export-html-footnote-format
					       "<a class=\"footnum\" name=\"fn.%s\" href=\"#fnr.%s\">%s</a>"))
			       n n n) t t line)))))
	    ;; Check if the line break needs to be conserved
	    (cond
	     ((string-match "\\\\\\\\[ \t]*$" line)
	      (setq line (replace-match "<br/>" t t line)))
	     (org-export-preserve-breaks
	      (setq line (concat line "<br/>"))))

	    ;; Check if a paragraph should be started
	    (let ((start 0))
	      (while (and org-par-open
			  (string-match "\\\\par\\>" line start))
		;; Leave a space in the </p> so that the footnote matcher
		;; does not see this.
		(if (not (get-text-property (match-beginning 0)
					    'org-protected line))
		    (setq line (replace-match "</p ><p >" t t line)))
		(setq start (match-end 0))))

	    (insert line "\n")))))

      ;; Properly close all local lists and other lists
      (when inquote
	(insert "</pre>\n")
	(org-open-par))
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
      (when (and (> umax 0) first-heading-pos) (insert "</div>\n"))

      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward "<p class=\"footnote\">[^\000]*?\\(</p>\\|\\'\\)" nil t)
	  (push (match-string 0) footnotes)
	  (replace-match "" t t)))
      (when footnotes
	(insert (format org-export-html-footnotes-section
			(nth 4 lang-words)
			(mapconcat 'identity (nreverse footnotes) "\n"))
		"\n"))
      (let ((bib (org-export-html-get-bibliography)))
	(when bib
	  (insert "\n" bib "\n")))
      (unless body-only
	(when (plist-get opt-plist :auto-postamble)
	  (insert "<div id=\"postamble\">\n")
	  (when (and org-export-author-info author)
	    (insert "<p class=\"author\"> "
		    (nth 1 lang-words) ": " author "\n")
	    (when (and org-export-email-info email (string-match "\\S-" email))
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
	    (insert (format "<p class=\"creator\">HTML generated by org-mode %s in emacs %s</p>\n"
			    org-version emacs-major-version)))
	  (when org-export-html-validation-link
	    (insert org-export-html-validation-link "\n"))
	  (insert "</div>"))

	(if org-export-html-with-timestamp
	    (insert org-export-html-html-helper-timestamp))
        (org-export-html-insert-plist-item opt-plist :postamble opt-plist)
	(insert "\n</div>\n</body>\n</html>\n"))

      (unless (plist-get opt-plist :buffer-will-be-killed)
	(normal-mode)
	(if (eq major-mode (default-value 'major-mode))
	    (html-mode)))

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
      ;; Remove empty lines at the beginning of the file.
      (goto-char (point-min))
      (when (looking-at "\\s-+\n") (replace-match ""))
      ;; Remove display properties
      (remove-text-properties (point-min) (point-max) '(display t))
      ;; Run the hook
      (run-hooks 'org-export-html-final-hook)
      (or to-buffer (save-buffer))
      (goto-char (point-min))
      (or (org-export-push-to-kill-ring "HTML")
	  (message "Exporting... done"))
      (if (eq to-buffer 'string)
	  (prog1 (buffer-substring (point-min) (point-max))
	    (kill-buffer (current-buffer)))
	(current-buffer)))))

(defun org-export-html-insert-plist-item (plist key &rest args)
  (let ((item (plist-get plist key)))
    (cond ((functionp item)
           (apply item args))
          (item
           (insert item)))))

(defun org-export-html-format-href (s)
  "Make sure the S is valid as a href reference in an XHTML document."
  (save-match-data
    (let ((start 0))
      (while (string-match "&" s start)
	(setq start (+ (match-beginning 0) 3)
	      s (replace-match "&amp;" t t s)))))
  s)

(defun org-export-html-format-desc (s)
  "Make sure the S is valid as a description in a link."
  (if (and s (not (get-text-property 1 'org-protected s)))
      (save-match-data
	(org-html-do-expand s))
    s))

(defun org-export-html-format-image (src par-open)
  "Create image tag with source and attributes."
  (save-match-data
    (if (string-match "^ltxpng/" src)
	(format "<img src=\"%s\" alt=\"%s\"/>"
                src (org-find-text-property-in-string 'org-latex-src src))
      (let* ((caption (org-find-text-property-in-string 'org-caption src))
	     (attr (org-find-text-property-in-string 'org-attributes src))
	     (label (org-find-text-property-in-string 'org-label src)))
	(setq caption (and caption (org-html-do-expand caption)))
	(concat
	(if caption
	    (format "%s<div %sclass=\"figure\">
<p>"
		    (if org-par-open "</p>\n" "")
		    (if label (format "id=\"%s\" " label) "")))
	(format "<img src=\"%s\"%s />"
		src
		(if (string-match "\\<alt=" (or attr ""))
		    (concat " " attr )
		  (concat " " attr " alt=\"" src "\"")))
	(if caption
	    (format "</p>%s
</div>%s"
		(concat "\n<p>" caption "</p>")
		(if org-par-open "\n<p>" ""))))))))

(defun org-export-html-get-bibliography ()
  "Find bibliography, cut it out and return it."
  (catch 'exit
    (let (beg end (cnt 1) bib)
      (save-excursion
	(goto-char (point-min))
	(when (re-search-forward "^[ \t]*<div \\(id\\|class\\)=\"bibliography\"" nil t)
	  (setq beg (match-beginning 0))
	  (while (re-search-forward "</?div\\>" nil t)
	    (setq cnt (+ cnt (if (string= (match-string 0) "<div") +1 -1)))
	    (when (= cnt 0)
	      (and (looking-at ">") (forward-char 1))
	      (setq bib (buffer-substring beg (point)))
	      (delete-region beg (point))
	    (throw 'exit bib))))
	nil))))

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

  (let* ((caption (org-find-text-property-in-string 'org-caption (car lines)))
	 (label (org-find-text-property-in-string 'org-label (car lines)))
	 (attributes (org-find-text-property-in-string 'org-attributes
						       (car lines)))
	 (html-table-tag (org-export-splice-attributes
			  html-table-tag attributes))
	 (head (and org-export-highlight-first-table-line
		    (delq nil (mapcar
			       (lambda (x) (string-match "^[ \t]*|-" x))
			       (cdr lines)))))

	 (nline 0) fnum i
	 tbopen line fields html gr colgropen rowstart rowend)
    (setq caption (and caption (org-html-do-expand caption)))
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
	(setq nline (1+ nline) i -1
	      rowstart (eval (car org-export-table-row-tags))
	      rowend (eval (cdr org-export-table-row-tags)))
	(push (concat rowstart
		      (mapconcat
		       (lambda (x)
			 (setq i (1+ i))
			 (if (and (< i nline)
				  (string-match org-table-number-regexp x))
			     (incf (aref fnum i)))
			 (cond
			  (head
			   (concat
			    (format (car org-export-table-header-tags) "col")
			    x
			    (cdr org-export-table-header-tags)))
			  ((and (= i 0) org-export-html-table-use-header-tags-for-first-column)
			   (concat
			    (format (car org-export-table-header-tags) "row")
			    x
			    (cdr org-export-table-header-tags)))
			  (t
			   (concat (car org-export-table-data-tags) x
				   (cdr org-export-table-data-tags)))))
		       fields "")
		      rowend)
	      html)))
    (unless splice (if tbopen (push "</tbody>" html)))
    (unless splice (push "</table>\n" html))
    (setq html (nreverse html))
    (unless splice
      ;; Put in col tags with the alignment (unfortunately often ignored...)
      (unless (car org-table-colgroup-info)
	(setq org-table-colgroup-info
	      (cons :start (cdr org-table-colgroup-info))))
      (push (mapconcat
	     (lambda (x)
	       (setq gr (pop org-table-colgroup-info))
	       (format "%s<col align=\"%s\" />%s"
		       (if (memq gr '(:start :startend))
			   (prog1
			       (if colgropen "</colgroup>\n<colgroup>" "<colgroup>")
			     (setq colgropen t))
			 "")
		       (if (> (/ (float x) nline) org-table-number-fraction)
			   "right" "left")
		       (if (memq gr '(:end :startend))
			   (progn (setq colgropen nil) "</colgroup>")
			 "")))
	     fnum "")
	    html)
      (if colgropen (setq html (cons (car html) (cons "</colgroup>" (cdr html)))))
      ;; Since the output of HTML table formatter can also be used in
      ;; DocBook document, we want to always include the caption to make
      ;; DocBook XML file valid.
      (push (format "<caption>%s</caption>" (or caption "")) html)
      (when label (push (format "<a name=\"%s\" id=\"%s\"></a>" label label)
			html))
      (push html-table-tag html))
    (concat (mapconcat 'identity html "\n") "\n")))

(defun org-export-splice-attributes (tag attributes)
  "Read attributes in string ATTRIBUTES, add and replace in HTML tag TAG."
  (if (not attributes)
      tag
    (let (oldatt newatt)
      (setq oldatt (org-extract-attributes-from-string tag)
	    tag (pop oldatt)
	    newatt (cdr (org-extract-attributes-from-string attributes)))
      (while newatt
	(setq oldatt (plist-put oldatt (pop newatt) (pop newatt))))
      (if (string-match ">" tag)
	  (setq tag
		(replace-match (concat (org-attributes-to-string oldatt) ">")
			       t t tag)))
      tag)))

(defun org-format-table-table-html (lines)
  "Format a table generated by table.el into HTML.
This conversion does *not* use `table-generate-source' from table.el.
This has the advantage that Org-mode's HTML conversions can be used.
But it has the disadvantage, that no cell- or row-spanning is allowed."
  (let (line field-buffer
	     (head org-export-highlight-first-table-line)
	     fields html empty i)
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
			     (concat
			      (format (car org-export-table-header-tags) "col")
			      x
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
	(or b (setq b (substring s 0 (match-beginning 0))))
	(setq r (concat
		 r (substring s 0 (match-beginning 0))
		 " @<span class=\"timestamp-wrapper\">"
		 (if (match-end 1)
		     (format "@<span class=\"timestamp-kwd\">%s @</span>"
			     (match-string 1 s)))
		 (format " @<span class=\"timestamp\">%s@</span>"
			 (substring
			  (org-translate-time (match-string 3 s)) 1 -1))
		 "@</span>")
	      s (substring s (match-end 0))))
      ;; Line break if line started and ended with time stamp stuff
      (if (not r)
	  s
	(setq r (concat r s))
	(unless (string-match "\\S-" (concat b s))
	  (setq r (concat r "@<br/>")))
	r))))

(defvar htmlize-buffer-places)  ; from htmlize.el
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
      (setq s (replace-match "&gt;" t t s)))
;    (while (string-match "\"" s)
;      (setq s (replace-match "&quot;" t t s)))
    )
  s)

(defun org-html-expand (string)
  "Prepare STRING for HTML export.  Applies all active conversions.
If there are links in the string, don't modify these."
  (let* ((re (concat org-bracket-link-regexp "\\|"
		     (org-re "[ \t]+\\(:[[:alnum:]_@:]+:\\)[ \t]*$")))
	 m s l res)
    (if (string-match "^[ \t]*\\+-[-+]*\\+[ \t]*$" string)
	string
      (while (setq m (string-match re string))
	(setq s (substring string 0 m)
	      l (match-string 0 string)
	      string (substring string (match-end 0)))
	(push (org-html-do-expand s) res)
	(push l res))
      (push (org-html-do-expand string) res)
      (apply 'concat (nreverse res)))))

(defun org-html-do-expand (s)
  "Apply all active conversions to translate special ASCII to HTML."
  (setq s (org-html-protect s))
  (if org-export-html-expand
      (while (string-match "@&lt;\\([^&]*\\)&gt;" s)
	(setq s (replace-match "<\\1>" t nil s))))
  (if org-export-with-emphasize
      (setq s (org-export-html-convert-emphasize s)))
  (if org-export-with-special-strings
      (setq s (org-export-html-convert-special-strings s)))
  (if org-export-with-sub-superscripts
      (setq s (org-export-html-convert-sub-super s)))
  (if org-export-with-TeX-macros
      (let ((start 0) wd rep)
	(while (setq start (string-match "\\\\\\([a-zA-Z]+[0-9]*\\)\\({}\\)?"
					 s start))
	  (if (get-text-property (match-beginning 0) 'org-protected s)
	      (setq start (match-end 0))
	    (setq wd (match-string 1 s))
	    (if (setq rep (org-entity-get-representation wd 'html))
		(setq s (replace-match rep t t s))
	      (setq start (+ start (length wd))))))))
  s)

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

(defvar in-local-list)
(defvar local-list-indent)
(defvar local-list-type)
(defun org-export-html-close-lists-maybe (line)
  (let ((ind (or (get-text-property 0 'original-indentation line)))
;		 (and (string-match "\\S-" line)
;		      (org-get-indentation line))))
	didclose)
    (when ind
      (while (and in-local-list
		  (<= ind (car local-list-indent)))
	(setq didclose t)
	(org-close-li (car local-list-type))
	(insert (format "</%sl>\n" (car local-list-type)))
	(pop local-list-type) (pop local-list-indent)
	(setq in-local-list local-list-indent))
      (and didclose (org-open-par)))))

(defvar body-only) ; dynamically scoped into this.
(defun org-html-level-start (level title umax with-toc head-count)
  "Insert a new level in HTML export.
When TITLE is nil, just close all open levels."
  (org-close-par-maybe)
  (let* ((target (and title (org-get-text-property-any 0 'target title)))
	 (extra-targets (and target
			     (assoc target org-export-target-aliases)))
	 (extra-class (and title (org-get-text-property-any 0 'html-container-class title)))
	 (preferred (and target
			 (cdr (assoc target org-export-preferred-target-alist))))
	 (remove (or preferred target))
	 (l org-level-max)
	 snumber href suffix)
    (setq extra-targets (remove remove extra-targets))
    (setq extra-targets
	  (mapconcat (lambda (x)
		       (if (org-uuidgen-p x) (setq x (concat "ID-" x)))
		       (format "<a name=\"%s\" id=\"%s\"></a>"
			       x x))
		     extra-targets
		     ""))
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
			      (mapconcat
			       (lambda (x)
				 (format "<span class=\"%s\">%s</span>"
					 (org-export-html-get-tag-class-name x)
					 x))
			       (org-split-string (match-string 1 title) ":")
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
		      (insert (format "<li id=\"%s\">" target) extra-targets title "<br/>\n")
		    (insert "<li>" title "<br/>\n")))
	      (aset org-levels-open (1- level) t)
	      (org-close-par-maybe)
	      (if target
		  (insert (format "<ul>\n<li id=\"%s\">" target)
			  extra-targets title "<br/>\n")
		(insert "<ul>\n<li>" title "<br/>\n"))))
	(aset org-levels-open (1- level) t)
	(setq snumber (org-section-number level))
	(setq level (+ level org-export-html-toplevel-hlevel -1))
	(if (and org-export-with-section-numbers (not body-only))
	    (setq title (concat
			 (format "<span class=\"section-number-%d\">%s</span>"
				 level snumber)
			 " " title)))
	(unless (= head-count 1) (insert "\n</div>\n"))
	(setq href (cdr (assoc (concat "sec-" snumber) org-export-preferred-target-alist)))
	(setq suffix (or href snumber))
	(setq href (or href (concat "sec-" snumber)))
	(insert (format "\n<div id=\"outline-container-%s\" class=\"outline-%d%s\">\n<h%d id=\"%s\">%s%s</h%d>\n<div class=\"outline-text-%d\" id=\"text-%s\">\n"
			suffix level (if extra-class (concat " " extra-class) "")
			level href
			extra-targets
			title level level suffix))
	(org-open-par)))))

(defun org-export-html-get-tag-class-name (tag)
  "Turn tag into a valid class name.
Replaces invalid characters with \"_\" and then prepends a prefix."
  (save-match-data
    (while (string-match "[^a-zA-Z0-9_]" tag)
      (setq tag (replace-match "_" t t tag))))
  (concat org-export-html-tag-class-prefix tag))

(defun org-export-html-get-todo-kwd-class-name (kwd)
  "Turn todo keyword into a valid class name.
Replaces invalid characters with \"_\" and then prepends a prefix."
  (save-match-data
    (while (string-match "[^a-zA-Z0-9_]" kwd)
      (setq kwd (replace-match "_" t t kwd))))
  (concat org-export-html-todo-kwd-class-prefix kwd))

(defun org-html-level-close (level max-outline-level)
  "Terminate one level in HTML export."
  (if (<= level max-outline-level)
      (insert "</div>\n")
    (org-close-li)
    (insert "</ul>\n")))

(provide 'org-html)

;; arch-tag: 8109d84d-eb8f-460b-b1a8-f45f3a6c7ea1
;;; org-html.el ends here
