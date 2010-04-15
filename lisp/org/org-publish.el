;;; org-publish.el --- publish related org-mode files as a website
;; Copyright (C) 2006, 2007, 2008, 2009, 2010
;;          Free Software Foundation, Inc.

;; Author: David O'Toole <dto@gnu.org>
;; Maintainer: Carsten Dominik <carsten DOT dominik AT gmail DOT com>
;; Keywords: hypermedia, outlines, wp
;; Version: 6.35i

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

;;; Commentary:

;; This program allow configurable publishing of related sets of
;; Org-mode files as a complete website.
;;
;; org-publish.el can do the following:
;;
;; + Publish all one's org-files to HTML or PDF
;; + Upload HTML, images, attachments and other files to a web server
;; + Exclude selected private pages from publishing
;; + Publish a clickable sitemap of pages
;; + Manage local timestamps for publishing only changed files
;; + Accept plugin functions to extend range of publishable content
;;
;; Documentation for publishing is in the manual.

;;; Code:


(defun org-publish-sanitize-plist (plist)
  (mapcar (lambda (x)
	    (or (cdr (assq x '((:index-filename . :sitemap-filename)
			       (:index-title . :sitemap-title)
			       (:index-function . :sitemap-function)
			       (:index-style . :sitemap-style)
			       (:auto-index . :auto-sitemap))))
		x))
	  plist))

(eval-when-compile
  (require 'cl))
(require 'org)
(require 'org-exp)

(eval-and-compile
  (unless (fboundp 'declare-function)
    (defmacro declare-function (fn file &optional arglist fileonly))))

(defgroup org-publish nil
  "Options for publishing a set of Org-mode and related files."
  :tag "Org Publishing"
  :group 'org)

(defcustom org-publish-project-alist nil
  "Association list to control publishing behavior.
Each element of the alist is a publishing 'project.'  The CAR of
each element is a string, uniquely identifying the project.  The
CDR of each element is in one of the following forms:

  (:property value :property value ... )

OR,

  (:components (\"project-1\" \"project-2\" ...))

When the CDR of an element of org-publish-project-alist is in
this second form, the elements of the list after :components are
taken to be components of the project, which group together files
requiring different publishing options.  When you publish such a
project with \\[org-publish], the components all publish.

When a property is given a value in org-publish-project-alist, its
setting overrides the value of the corresponding user variable
\(if any) during publishing.  However, options set within a file
override everything.

Most properties are optional, but some should always be set:

  :base-directory        Directory containing publishing source files
  :base-extension        Extension (without the dot!) of source files.
                         This can be a regular expression.
  :publishing-directory  Directory (possibly remote) where output
                         files will be published

The :exclude property may be used to prevent certain files from
being published.  Its value may be a string or regexp matching
file names you don't want to be published.

The :include property may be used to include extra files.  Its
value may be a list of filenames to include. The filenames are
considered relative to the base directory.

When both :include and :exclude properties are given values, the
exclusion step happens first.

One special property controls which back-end function to use for
publishing files in the project.  This can be used to extend the
set of file types publishable by org-publish, as well as the set
of output formats.

  :publishing-function     Function to publish file.  The default is
                           `org-publish-org-to-html', but other
                           values are possible.  May also be a
                           list of functions, in which case
                           each function in the list is invoked
                           in turn.

Another property allows you to insert code that prepares a
project for publishing.  For example, you could call GNU Make on a
certain makefile, to ensure published files are built up to date.

  :preparation-function   Function to be called before publishing
                          this project.  This may also be a list
                          of functions.
  :completion-function    Function to be called after publishing
                          this project.  This may also be a list
                          of functions.

Some properties control details of the Org publishing process,
and are equivalent to the corresponding user variables listed in
the right column.  See the documentation for those variables to
learn more about their use and default values.

  :language              `org-export-default-language'
  :headline-levels       `org-export-headline-levels'
  :section-numbers       `org-export-with-section-numbers'
  :table-of-contents     `org-export-with-toc'
  :emphasize             `org-export-with-emphasize'
  :sub-superscript       `org-export-with-sub-superscripts'
  :TeX-macros            `org-export-with-TeX-macros'
  :fixed-width           `org-export-with-fixed-width'
  :tables                `org-export-with-tables'
  :table-auto-headline   `org-export-highlight-first-table-line'
  :style                 `org-export-html-style'
  :convert-org-links     `org-export-html-link-org-files-as-html'
  :inline-images         `org-export-html-inline-images'
  :expand-quoted-html    `org-export-html-expand'
  :timestamp             `org-export-html-with-timestamp'
  :publishing-directory  `org-export-publishing-directory'
  :preamble              `org-export-html-preamble'
  :postamble             `org-export-html-postamble'
  :auto-preamble         `org-export-html-auto-preamble'
  :auto-postamble        `org-export-html-auto-postamble'
  :author                `user-full-name'
  :email                 `user-mail-address'

The following properties may be used to control publishing of a
sitemap of files or summary page for a given project.

  :auto-sitemap           Whether to publish a sitemap during
                         `org-publish-current-project' or `org-publish-all'.
  :sitemap-filename      Filename for output of sitemap.  Defaults
                         to 'sitemap.org' (which becomes 'sitemap.html').
  :sitemap-title         Title of sitemap page.  Defaults to name of file.
  :sitemap-function      Plugin function to use for generation of sitemap.
                         Defaults to `org-publish-org-sitemap', which
                         generates a plain list of links to all files
                         in the project.
  :sitemap-style         Can be `list' (sitemap is just an itemized list
                         of the titles of the files involved) or
                         `tree' (the directory structure of the source
                         files is reflected in the sitemap).  Defaults to
                         `tree'."
  :group 'org-publish
  :type 'alist)

(defcustom org-publish-use-timestamps-flag t
  "When non-nil, use timestamp checking to publish only changed files.
When nil, do no timestamp checking and always publish all files."
  :group 'org-publish
  :type 'boolean)

(defcustom org-publish-timestamp-directory (convert-standard-filename
					    "~/.org-timestamps/")
  "Name of directory in which to store publishing timestamps."
  :group 'org-publish
  :type 'directory)

(defcustom org-publish-list-skipped-files t
  "Non-nil means show message about files *not* published."
  :group 'org-publish
  :type 'boolean)

(defcustom org-publish-before-export-hook nil
  "Hook run before export on the Org file.
The hook may modify the file in arbitrary ways before publishing happens.
The original version of the buffer will be restored after publishing."
  :group 'org-publish
  :type 'hook)

(defcustom org-publish-after-export-hook nil
  "Hook run after export on the exported buffer.
Any changes made by this hook will be saved."
  :group 'org-publish
  :type 'hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Timestamp-related functions

(defun org-publish-timestamp-filename (filename &optional pub-dir pub-func)
  "Return path to timestamp file for filename FILENAME."
  (setq filename (concat filename "::" (or pub-dir "") "::"
			 (format "%s" (or pub-func ""))))
  (concat (file-name-as-directory org-publish-timestamp-directory)
	  "X" (if (fboundp 'sha1) (sha1 filename) (md5 filename))))

(defun org-publish-needed-p (filename &optional pub-dir pub-func true-pub-dir)
  "Return `t' if FILENAME should be published in PUB-DIR using PUB-FUNC.
TRUE-PUB-DIR is there the file will truely end up.  Currently we are not using
this - maybe it can eventually be used to check if the file is present at
the target location, and how old it is.  Right ow we cannot do this, because
we do not know under what file name the file will be stored - the publishing
function can still decide about that independently."
  (let ((rtn
	 (if org-publish-use-timestamps-flag
	     (if (file-exists-p org-publish-timestamp-directory)
		 ;; first handle possible wrong timestamp directory
		 (if (not (file-directory-p org-publish-timestamp-directory))
		     (error "Org publish timestamp: %s is not a directory"
			    org-publish-timestamp-directory)
		   ;; there is a timestamp, check if FILENAME is newer
		   (file-newer-than-file-p
		    filename (org-publish-timestamp-filename
			      filename pub-dir pub-func)))
	       (make-directory org-publish-timestamp-directory)
	       t)
	   ;; don't use timestamps, always return t
	   t)))
    (if rtn
	(message "Publishing file %s using `%s'" filename pub-func)
      (when org-publish-list-skipped-files
	(message   "Skipping unmodified file %s" filename)))
    rtn))

(defun org-publish-update-timestamp (filename &optional pub-dir pub-func)
  "Update publishing timestamp for file FILENAME.
If there is no timestamp, create one."
  (let ((timestamp-file (org-publish-timestamp-filename
			 filename pub-dir pub-func))
	newly-created-timestamp)
    (if (not (file-exists-p timestamp-file))
	;; create timestamp file if needed
	(with-temp-buffer
	  (make-directory (file-name-directory timestamp-file) t)
	  (write-file timestamp-file)
	  (setq newly-created-timestamp t)))
    ;; Emacs 21 doesn't have `set-file-times'
    (if (and (fboundp 'set-file-times)
	     (not newly-created-timestamp))
	(set-file-times timestamp-file)
      (call-process "touch" nil 0 nil (expand-file-name timestamp-file)))))

(defun org-publish-remove-all-timestamps ()
  "Remove all files in the timstamp directory."
  (let ((dir org-publish-timestamp-directory)
	files)
    (when (and (file-exists-p dir)
	       (file-directory-p dir))
      (mapc 'delete-file (directory-files dir 'full "[^.]\\'")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mapping files to project names

(defvar org-publish-files-alist nil
  "Alist of files and their parent projects.
Each element of this alist is of the form:

  (file-name . project-name)")

(defvar org-publish-initial-buffer nil
  "The buffer `org-publish' has been called from.")
(defvar org-publish-temp-files nil
  "Temporary list of files to be published.")

(defun org-publish-initialize-files-alist (&optional refresh)
  "Set `org-publish-files-alist' if it is not set.
Also set it if the optional argument REFRESH is non-nil."
  (interactive "P")
  (when (or refresh (not org-publish-files-alist))
    (setq org-publish-files-alist
	  (org-publish-get-files org-publish-project-alist))))

(defun org-publish-validate-link (link &optional directory)
  "Check if LINK points to a file in the current project."
  (assoc (expand-file-name link directory) org-publish-files-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Compatibility aliases

;; Delete-dups is not in Emacs <22
(if (fboundp 'delete-dups)
    (defalias 'org-publish-delete-dups 'delete-dups)
  (defun org-publish-delete-dups (list)
    "Destructively remove `equal' duplicates from LIST.
Store the result in LIST and return it.  LIST must be a proper list.
Of several `equal' occurrences of an element in LIST, the first
one is kept.

This is a compatibility function for Emacsen without `delete-dups'."
    ;; Code from `subr.el' in Emacs 22:
    (let ((tail list))
      (while tail
	(setcdr tail (delete (car tail) (cdr tail)))
	(setq tail (cdr tail))))
    list))

(declare-function org-publish-delete-dups "org-publish" (list))
(declare-function find-lisp-find-files "find-lisp" (directory regexp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Getting project information out of org-publish-project-alist

(defun org-publish-get-files (projects-alist &optional no-exclusion)
  "Return the list of all publishable files for PROJECTS-ALIST.
If NO-EXCLUSION is non-nil, don't exclude files."
  (let (all-files)
    ;; add all projects
    (mapc
     (lambda(p)
       (let* ((exclude (plist-get (cdr p) :exclude))
	      (files (and p (org-publish-get-base-files p exclude))))
	 ;; add all files from this project
	 (mapc (lambda(f)
		 (add-to-list 'all-files
			      (cons (expand-file-name f) (car p))))
	       files)))
     (org-publish-expand-projects projects-alist))
    all-files))

(defun org-publish-expand-projects (projects-alist)
  "Expand projects in PROJECTS-ALIST.
This splices all the components into the list."
  (let ((rest projects-alist) rtn p components)
    (while (setq p (pop rest))
      (if (setq components (plist-get (cdr p) :components))
	  (setq rest (append
		      (mapcar (lambda (x) (assoc x org-publish-project-alist))
			      components)
		      rest))
	(push p rtn)))
    (nreverse (org-publish-delete-dups (delq nil rtn)))))

(defun org-publish-get-base-files-1 (base-dir &optional recurse match skip-file skip-dir)
  "Set `org-publish-temp-files' with files from BASE-DIR directory.
If RECURSE is non-nil, check BASE-DIR recursively.  If MATCH is
non-nil, restrict this list to the files matching the regexp
MATCH.  If SKIP-FILE is non-nil, skip file matching the regexp
SKIP-FILE.  If SKIP-DIR is non-nil, don't check directories
matching the regexp SKIP-DIR when recursing through BASE-DIR."
  (mapc (lambda (f)
	  (let ((fd-p (file-directory-p f))
		(fnd (file-name-nondirectory f)))
	    (if (and fd-p recurse
		     (not (string-match "^\\.+$" fnd))
		     (if skip-dir (not (string-match skip-dir fnd)) t))
		(org-publish-get-base-files-1 f recurse match skip-file skip-dir)
	      (unless (or fd-p ;; this is a directory
			  (and skip-file (string-match skip-file fnd))
			  (not (file-exists-p (file-truename f)))
			  (not (string-match match fnd)))
		(pushnew f org-publish-temp-files)))))
	(directory-files base-dir t (unless recurse match))))

(defun org-publish-get-base-files (project &optional exclude-regexp)
  "Return a list of all files in PROJECT.
If EXCLUDE-REGEXP is set, this will be used to filter out
matching filenames."
  (let* ((project-plist (cdr project))
	 (base-dir (file-name-as-directory
		    (plist-get project-plist :base-directory)))
	 (include-list (plist-get project-plist :include))
	 (recurse (plist-get project-plist :recursive))
	 (extension (or (plist-get project-plist :base-extension) "org"))
	 (match (if (eq extension 'any)
                    "^[^\\.]"
		  (concat "^[^\\.].*\\.\\(" extension "\\)$"))))
    (setq org-publish-temp-files nil)
    (org-publish-get-base-files-1 base-dir recurse match
				  ;; FIXME distinguish exclude regexp
				  ;; for skip-file and skip-dir?
				  exclude-regexp exclude-regexp)
    (mapc (lambda (f)
	    (pushnew
	     (expand-file-name (concat base-dir f))
	     org-publish-temp-files))
	  include-list)
    org-publish-temp-files))

(defun org-publish-get-project-from-filename (filename &optional up)
  "Return the project FILENAME belongs."
  (let* ((project-name (cdr (assoc (expand-file-name filename)
				   org-publish-files-alist))))
    (when up
      (dolist (prj org-publish-project-alist)
	(if (member project-name (plist-get (cdr prj) :components))
	    (setq project-name (car prj)))))
    (assoc project-name org-publish-project-alist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Pluggable publishing back-end functions

(defun org-publish-org-to (format plist filename pub-dir)
  "Publish an org file to FORMAT.
PLIST is the property list for the given project.
FILENAME is the filename of the org file to be published.
PUB-DIR is the publishing directory."
  (require 'org)
  (unless (file-exists-p pub-dir)
    (make-directory pub-dir t))
  (let ((visiting (find-buffer-visiting filename)))
    (save-excursion
      (switch-to-buffer (or visiting (find-file filename)))
      (let* ((plist (cons :buffer-will-be-killed (cons t plist)))
	     (init-buf (current-buffer))
	     (init-point (point))
	     (init-buf-string (buffer-string))
	     export-buf-or-file)
	;; run hooks before exporting
	(run-hooks 'org-publish-before-export-hook)
	;; export the possibly modified buffer
	(setq export-buf-or-file
	      (funcall (intern (concat "org-export-as-" format))
		       (plist-get plist :headline-levels)
		       nil plist nil nil pub-dir))
	(when (and (bufferp export-buf-or-file)
		   (buffer-live-p export-buf-or-file))
	  (set-buffer export-buf-or-file)
	  ;; run hooks after export and save export
	  (progn (run-hooks 'org-publish-after-export-hook)
		 (if (buffer-modified-p) (save-buffer)))
	  (kill-buffer export-buf-or-file))
	;; maybe restore buffer's content
	(set-buffer init-buf)
	(when (buffer-modified-p init-buf)
	  (erase-buffer)
	  (insert init-buf-string)
	  (save-buffer)
	  (goto-char init-point))
	(unless visiting
	  (kill-buffer init-buf))))))

(defmacro org-publish-with-aux-preprocess-maybe (&rest body)
  "Execute BODY with a modified hook to preprocess for index."
  `(let ((org-export-preprocess-after-headline-targets-hook
	 (if (plist-get project-plist :makeindex)
	     (cons 'org-publish-aux-preprocess
		   org-export-preprocess-after-headline-targets-hook)
	   org-export-preprocess-after-headline-targets-hook)))
     ,@body))

(defvar project-plist)
(defun org-publish-org-to-latex (plist filename pub-dir)
  "Publish an org file to LaTeX.
See `org-publish-org-to' to the list of arguments."
  (org-publish-with-aux-preprocess-maybe
   (org-publish-org-to "latex" plist filename pub-dir)))

(defun org-publish-org-to-pdf (plist filename pub-dir)
  "Publish an org file to PDF (via LaTeX).
See `org-publish-org-to' to the list of arguments."
  (org-publish-with-aux-preprocess-maybe
   (org-publish-org-to "pdf" plist filename pub-dir)))

(defun org-publish-org-to-html (plist filename pub-dir)
  "Publish an org file to HTML.
See `org-publish-org-to' to the list of arguments."
  (org-publish-with-aux-preprocess-maybe
    (org-publish-org-to "html" plist filename pub-dir)))

(defun org-publish-org-to-org (plist filename pub-dir)
  "Publish an org file to HTML.
See `org-publish-org-to' to the list of arguments."
  (org-publish-org-to "org" plist filename pub-dir))

(defun org-publish-attachment (plist filename pub-dir)
  "Publish a file with no transformation of any kind.
See `org-publish-org-to' to the list of arguments."
  ;; make sure eshell/cp code is loaded
  (unless (file-directory-p pub-dir)
    (make-directory pub-dir t))
  (or (equal (expand-file-name (file-name-directory filename))
	     (file-name-as-directory (expand-file-name pub-dir)))
      (copy-file filename
		 (expand-file-name (file-name-nondirectory filename) pub-dir)
		 t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Publishing files, sets of files, and indices

(defun org-publish-file (filename &optional project)
  "Publish file FILENAME from PROJECT."
  (let* ((project
	  (or project
	      (or (org-publish-get-project-from-filename filename)
		  (if (y-or-n-p
		       (format "%s is not in a project.  Re-read the list of projects files? "
			       (abbreviate-file-name filename)))
		      ;; If requested, re-initialize the list of projects files
		      (progn (org-publish-initialize-files-alist t)
			     (or (org-publish-get-project-from-filename filename)
				 (error "File %s not part of any known project"
					(abbreviate-file-name filename))))
		    (error "Can't publish file outside of a project")))))
	 (project-plist (cdr project))
	 (ftname (file-truename filename))
	 (publishing-function
	  (or (plist-get project-plist :publishing-function)
	      'org-publish-org-to-html))
	 (base-dir (file-name-as-directory
		    (file-truename (plist-get project-plist :base-directory))))
	 (pub-dir (file-name-as-directory
		   (file-truename (plist-get project-plist :publishing-directory))))
	 tmp-pub-dir)
    (setq tmp-pub-dir
	  (file-name-directory
	   (concat pub-dir
		   (and (string-match (regexp-quote base-dir) ftname)
			(substring ftname (match-end 0))))))
    (if (listp publishing-function)
	;; allow chain of publishing functions
	(mapc (lambda (f)
		(when (org-publish-needed-p filename pub-dir f tmp-pub-dir)
		  (funcall f project-plist filename tmp-pub-dir)
		  (org-publish-update-timestamp filename pub-dir f)))
	      publishing-function)
      (when (org-publish-needed-p filename pub-dir publishing-function
				  tmp-pub-dir)
	(funcall publishing-function project-plist filename tmp-pub-dir)
	(org-publish-update-timestamp
	 filename pub-dir publishing-function)))))

(defun org-publish-projects (projects)
  "Publish all files belonging to the PROJECTS alist.
If :auto-sitemap is set, publish the sitemap too.
If :makeindex is set, also produce a file theindex.org."
  (mapc
   (lambda (project)
     (let*
	 ((project-plist (cdr project))
	  (exclude-regexp (plist-get project-plist :exclude))
	  (sitemap-p (plist-get project-plist :auto-sitemap))
	  (sitemap-filename (or (plist-get project-plist :sitemap-filename)
				"sitemap.org"))
	  (sitemap-function (or (plist-get project-plist :sitemap-function)
				'org-publish-org-sitemap))
	  (preparation-function (plist-get project-plist :preparation-function))
	  (completion-function (plist-get project-plist :completion-function))
	  (files (org-publish-get-base-files project exclude-regexp)) file)
       (when preparation-function (run-hooks 'preparation-function))
       (if sitemap-p (funcall sitemap-function project sitemap-filename))
       (while (setq file (pop files))
	 (org-publish-file file project))
       (when (plist-get project-plist :makeindex)
	 (org-publish-index-generate-theindex.inc
	  (plist-get project-plist :base-directory))
	 (org-publish-file (expand-file-name
			    "theindex.org"
			    (plist-get project-plist :base-directory))
			   project))
       (when completion-function (run-hooks 'completion-function))))
   (org-publish-expand-projects projects)))

(defun org-publish-org-sitemap (project &optional sitemap-filename)
  "Create an sitemap of pages in set defined by PROJECT.
Optionally set the filename of the sitemap with SITEMAP-FILENAME.
Default for SITEMAP-FILENAME is 'sitemap.org'."
  (let* ((project-plist (cdr project))
	 (dir (file-name-as-directory
	       (plist-get project-plist :base-directory)))
	 (localdir (file-name-directory dir))
	 (indent-str (make-string 2 ?\ ))
	 (exclude-regexp (plist-get project-plist :exclude))
	 (files (nreverse (org-publish-get-base-files project exclude-regexp)))
	 (sitemap-filename (concat dir (or sitemap-filename "sitemap.org")))
	 (sitemap-title (or (plist-get project-plist :sitemap-title)
			  (concat "Sitemap for project " (car project))))
	 (sitemap-style (or (plist-get project-plist :sitemap-style)
			  'tree))
	 (visiting (find-buffer-visiting sitemap-filename))
	 (ifn (file-name-nondirectory sitemap-filename))
	 file sitemap-buffer)
    (with-current-buffer (setq sitemap-buffer
			       (or visiting (find-file sitemap-filename)))
      (erase-buffer)
      (insert (concat "#+TITLE: " sitemap-title "\n\n"))
      (while (setq file (pop files))
	(let ((fn (file-name-nondirectory file))
	      (link (file-relative-name file dir))
	      (oldlocal localdir))
	  ;; sitemap shouldn't list itself
	  (unless (equal (file-truename sitemap-filename)
			 (file-truename file))
	    (if (eq sitemap-style 'list)
		(message "Generating list-style sitemap for %s" sitemap-title)
	      (message "Generating tree-style sitemap for %s" sitemap-title)
	      (setq localdir (concat (file-name-as-directory dir)
				     (file-name-directory link)))
	      (unless (string= localdir oldlocal)
		(if (string= localdir dir)
		    (setq indent-str (make-string 2 ?\ ))
		  (let ((subdirs
			 (split-string
			  (directory-file-name
			   (file-name-directory
			    (file-relative-name localdir dir))) "/"))
			(subdir "")
			(old-subdirs (split-string
				      (file-relative-name oldlocal dir) "/")))
		    (setq indent-str (make-string 2 ?\ ))
		    (while (string= (car old-subdirs) (car subdirs))
		      (setq indent-str (concat indent-str (make-string 2 ?\ )))
		      (pop old-subdirs)
		      (pop subdirs))
		    (dolist (d subdirs)
		      (setq subdir (concat subdir d "/"))
		      (insert (concat indent-str " + " d "\n"))
		      (setq indent-str (make-string
					(+ (length indent-str) 2) ?\ )))))))
	    ;; This is common to 'flat and 'tree
	    (insert (concat indent-str " + [[file:" link "]["
			    (org-publish-find-title file)
			    "]]\n")))))
      (save-buffer))
    (or visiting (kill-buffer sitemap-buffer))))

(defun org-publish-find-title (file)
  "Find the title of file in project."
  (let* ((visiting (find-buffer-visiting file))
	 (buffer (or visiting (find-file-noselect file)))
	 title)
    (with-current-buffer buffer
      (let* ((opt-plist (org-combine-plists (org-default-export-plist)
					    (org-infile-export-plist))))
	(setq title
	      (or (plist-get opt-plist :title)
		  (and (not
			(plist-get opt-plist :skip-before-1st-heading))
		       (org-export-grab-title-from-buffer))
		  (file-name-nondirectory (file-name-sans-extension file))))))
    (unless visiting
      (kill-buffer buffer))
    title))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interactive publishing functions

;;;###autoload
(defalias 'org-publish-project 'org-publish)

;;;###autoload
(defun org-publish (project &optional force)
  "Publish PROJECT."
  (interactive
   (list
    (assoc (org-icompleting-read
	    "Publish project: "
	    org-publish-project-alist nil t)
	   org-publish-project-alist)
    current-prefix-arg))
  (setq org-publish-initial-buffer (current-buffer))
  (save-window-excursion
    (let* ((org-publish-use-timestamps-flag
	    (if force nil org-publish-use-timestamps-flag)))
      (org-publish-projects (list project)))))

;;;###autoload
(defun org-publish-all (&optional force)
  "Publish all projects.
With prefix argument, remove all files in the timestamp
directory and force publishing all files."
  (interactive "P")
  (when force
    (org-publish-remove-all-timestamps))
  (org-publish-initialize-files-alist force)
  (save-window-excursion
    (let ((org-publish-use-timestamps-flag
	   (if force nil org-publish-use-timestamps-flag)))
      (org-publish-projects org-publish-project-alist))))


;;;###autoload
(defun org-publish-current-file (&optional force)
  "Publish the current file.
With prefix argument, force publish the file."
  (interactive "P")
  (org-publish-initialize-files-alist force)
  (save-window-excursion
    (let ((org-publish-use-timestamps-flag
	   (if force nil org-publish-use-timestamps-flag)))
      (org-publish-file (buffer-file-name)))))

;;;###autoload
(defun org-publish-current-project (&optional force)
  "Publish the project associated with the current file.
With a prefix argument, force publishing of all files in
the project."
  (interactive "P")
  (org-publish-initialize-files-alist force)
  (save-window-excursion
    (let ((project (org-publish-get-project-from-filename (buffer-file-name) 'up))
	  (org-publish-use-timestamps-flag
	   (if force nil org-publish-use-timestamps-flag)))
      (if (not project)
	  (error "File %s is not part of any known project" (buffer-file-name)))
      (org-publish project))))


;;; Index generation

(defvar backend) ; dynamically scoped
(defun org-publish-aux-preprocess ()
  "Find index entries and write them to an .orgx file."
  (let ((case-fold-search t)
	entry index target)
    (goto-char (point-min))
    (while
	(and
	 (re-search-forward "^[ \t]*#\\+index:[ \t]*\\(.*?\\)[ \t]*$" nil t)
	 (> (match-end 1) (match-beginning 1)))
      (setq entry (match-string 1))
      (when (eq backend 'latex)
	(replace-match (format "\\index{%s}" entry) t t))
      (save-excursion
	(org-back-to-heading t)
	(setq target (get-text-property (point) 'target))
	(setq target (or (cdr (assoc target org-export-preferred-target-alist))
			 (cdr (assoc target org-export-id-target-alist))
			 target))
	(push (cons entry target) index)))
    (with-temp-file
	(concat (file-name-sans-extension org-current-export-file) ".orgx")
      (dolist (entry (nreverse index))
	(insert (format "INDEX: (%s) %s\n" (cdr entry) (car entry)))))))

(defun org-publish-index-generate-theindex.inc (directory)
  "Generate the index from all .orgx files in the current directory and below."
  (require 'find-lisp)
  (let* ((fulldir (file-name-as-directory
		   (expand-file-name directory)))
	 (full-files (find-lisp-find-files directory "\\.orgx\\'"))
	 (re (concat "\\`" fulldir))
	 (files (mapcar (lambda (f) (if (string-match re f)
					(substring f (match-end 0))
				      f))
			full-files))
	 (default-directory directory)
	 index origfile buf target entry ibuffer
	 main last-main letter last-letter file sub link)
    ;; `files' contains the list of relative file names
    (dolist (file files)
      (setq origfile (substring file 0 -1))
      (setq buf (find-file-noselect file))
      (with-current-buffer buf
	(goto-char (point-min))
	(while (re-search-forward "^INDEX: (\\(.*?\\)) \\(.*\\)" nil t)
	  (setq target (match-string 1)
		entry (match-string 2))
	  (push (list entry origfile target) index)))
      (kill-buffer buf))
    (setq index (sort index (lambda (a b) (string< (downcase (car a))
						   (downcase (car b))))))
    (setq ibuffer (find-file-noselect (expand-file-name "theindex.inc" directory)))
    (with-current-buffer ibuffer
      (erase-buffer)
      (insert "* Index\n")
      (setq last-letter nil)
      (dolist (idx index)
	(setq entry (car idx) file (nth 1 idx) target (nth 2 idx))
	(setq letter (upcase (substring entry 0 1)))
	(when (not (equal letter last-letter))
	  (insert "** " letter "\n")
	  (setq last-letter letter))
	(if (string-match "!" entry)
	    (setq main (substring entry 0 (match-beginning 0))
		  sub (substring entry (match-end 0)))
	  (setq main nil sub nil last-main nil))
	(when (and main (not (equal main last-main)))
	  (insert "   - " main "\n")
	  (setq last-main main))
	(setq link (concat "[[file:" file "::#" target "]"
			   "[" (or sub entry) "]]"))
	(if (and main sub)
	    (insert "     - " link "\n")
	  (insert "   - " link "\n")))
      (save-buffer))
    (kill-buffer ibuffer)

    (let ((index-file (expand-file-name "theindex.org" directory)))
      (unless (file-exists-p index-file)
	(setq ibuffer (find-file-noselect index-file))
	(with-current-buffer ibuffer
	  (erase-buffer)
	  (insert "\n\n#+include: \"theindex.inc\"\n\n")
	  (save-buffer))
	(kill-buffer ibuffer)))))

(provide 'org-publish)


;; arch-tag: 72807f3c-8af0-4a6b-8dca-c3376eb25adb

;;; org-publish.el ends here
