;;; org-publish.el --- publish related org-mode files as a website
;; Copyright (C) 2006, 2007, 2008  Free Software Foundation, Inc.

;; Author: David O'Toole <dto@gnu.org>
;; Maintainer: Bastien Guerry <bzg AT altern DOT org>
;; Keywords: hypermedia, outlines, wp
;; Version: 6.02b

;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Requires at least version 4.27 of org.el

;; This program allow configurable publishing of related sets of
;; Org-mode files as a complete website.
;;
;; org-publish.el can do the following:
;;
;; + Publish all one's org-files to HTML or LaTeX
;; + Upload HTML, images, attachments and other files to a web server
;; + Exclude selected private pages from publishing
;; + Publish a clickable index of pages
;; + Manage local timestamps for publishing only changed files
;; + Accept plugin functions to extend range of publishable content
;;
;; Special thanks to the org-mode maintainer Carsten Dominik for his
;; ideas, enthusiasm, and cooperation.

;;; Installation:

;; Put org-publish.el in your load path, byte-compile it, and then add
;; the following lines to your emacs initialization file:

;; (autoload 'org-publish "org-publish" nil t)
;; (autoload 'org-publish "org-publish-all" nil t)
;; (autoload 'org-publish "org-publish-current-file" nil t)
;; (autoload 'org-publish "org-publish-current-project" nil t)

;; NOTE: When org-publish.el is included with org.el, those forms are
;; already in the file org-install.el, and hence don't need to be put
;; in your emacs initialization file in this case.

;;; Usage:
;;
;; The program's main configuration variable is
;; `org-publish-project-alist'. See below for example configurations
;; with commentary.

;; The main interactive functions are:
;;
;; M-x org-publish
;; M-x org-publish-all
;; M-x org-publish-current-file
;; M-x org-publish-current-project

;;;; Simple example configuration:

;; (setq org-publish-project-alist
;;       (list
;;        '("org" . (:base-directory "~/org/"
;; 		     :base-extension "org"
;; 		     :publishing-directory "~/public_html"
;;                   :with-section-numbers nil
;; 		     :table-of-contents nil
;;                   :recursive t
;; 		     :style "<link rel=stylesheet href=\"../other/mystyle.css\" type=\"text/css\">")))

;;;; More complex example configuration:

;; Imagine your *.org files are kept in ~/org, your images in
;; ~/images, and stylesheets in ~/other. Now imagine you want to
;; publish the files through an ssh connection to a remote host, via
;; Tramp-mode. To maintain relative links from *.org files to /images
;; and /other, we should replicate the same directory structure in
;; your web server account's designated html root (in this case,
;; assumed to be ~/html)

;; Once you've done created the proper directories, you can adapt the
;; following example configuration to your specific paths, run M-x
;; org-publish-all, and it should publish the files to the correct
;; directories on the web server, transforming the *.org files into
;; HTML, and leaving other files alone.

;; (setq org-publish-project-alist
;;       (list
;;        '("orgfiles" :base-directory "~/org/"
;; 		       :base-extension "org"
;; 		       :publishing-directory "/ssh:user@host:~/html/notebook/"
;; 		       :publishing-function org-publish-org-to-html
;; 		       :exclude "PrivatePage.org"   ;; regexp
;; 		       :headline-levels 3
;;                     :with-section-numbers nil
;; 		       :table-of-contents nil
;; 		       :style "<link rel=stylesheet href=\"../other/mystyle.css\" type=\"text/css\">"
;; 		       :auto-preamble t
;; 		       :auto-postamble nil)
;;         ("images" :base-directory "~/images/"
;; 	             :base-extension "jpg\\|gif\\|png"
;; 		     :publishing-directory "/ssh:user@host:~/html/images/"
;; 		     :publishing-function org-publish-attachment)
;;         ("other"  :base-directory "~/other/"
;; 	   	     :base-extension "css"
;; 		     :publishing-directory "/ssh:user@host:~/html/other/"
;; 		     :publishing-function org-publish-attachment)
;;         ("website" :components ("orgfiles" "images" "other"))))

;; For more information, see the documentation for the variable
;; `org-publish-project-alist'.

;; Of course, you don't have to publish to remote directories from
;; within emacs. You can always just publish to local folders, and
;; then use the synchronization/upload tool of your choice.

;;; List of user-visible changes since version 1.27

;; 1.78: Allow list-valued :publishing-function
;; 1.77: Added :preparation-function, this allows you to use GNU Make etc.
;; 1.65: Remove old "composite projects". They're redundant.
;; 1.64: Allow meta-projects with :components
;; 1.57: Timestamps flag is now called "org-publish-use-timestamps-flag"
;; 1.52: Properly set default for :index-filename
;; 1.48: Composite projects allowed.
;;       :include keyword allowed.
;; 1.43: Index no longer includes itself in the index.
;; 1.42: Fix "function definition is void" error
;;       when :publishing-function not set in org-publish-current-file.
;; 1.41: Fixed bug where index isn't published on first try.
;; 1.37: Added interactive function "org-publish". Prompts for particular
;;       project name to publish.
;; 1.34: Added force-publish option to all interactive functions.
;; 1.32: Fixed "index.org has changed on disk" error during index publishing.
;; 1.30: Fixed startup error caused by (require 'em-unix)

;;; Code:

(eval-when-compile
  (require 'cl))

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
each element is a string, uniquely identifying the project. The
CDR of each element is in one of the following forms:

  (:property value :property value ... )

OR,

  (:components (\"project-1\" \"project-2\" ...))

When the CDR of an element of org-publish-project-alist is in
this second form, the elements of the list after :components are
taken to be components of the project, which group together files
requiring different publishing options. When you publish such a
project with M-x org-publish, the components all publish.

When a property is given a value in org-publish-project-alist, its
setting overrides the value of the corresponding user variable
 (if any) during publishing. However, options set within a file
override everything.

Most properties are optional, but some should always be set:

  :base-directory        Directory containing publishing source files
  :base-extension        Extension (without the dot!) of source files.
                         This can be a regular expression.
  :publishing-directory  Directory (possibly remote) where output
                         files will be published

The :exclude property may be used to prevent certain files from
being published. Its value may be a string or regexp matching
file names you don't want to be published.

The :include property may be used to include extra files. Its
value may be a list of filenames to include. The filenames are
considered relative to the publishing directory.

When both :include and :exclude properties are given values, the
exclusion step happens first.

One special property controls which back-end function to use for
publishing files in the project. This can be used to extend the
set of file types publishable by org-publish, as well as the set
of output formats.

  :publishing-function     Function to publish file. The default is
                           org-publish-org-to-html, but other
                           values are possible. May also be a
                           list of functions, in which case
                           each function in the list is invoked
                           in turn.

Another property allows you to insert code that prepares a
project for publishing. For example, you could call GNU Make on a
certain makefile, to ensure published files are built up to date.

  :preparation-function   Function to be called before publishing
                          this project.

Some properties control details of the Org publishing process,
and are equivalent to the corresponding user variables listed in
the right column. See the documentation for those variables to
learn more about their use and default values.

  :language              org-export-default-language
  :headline-levels       org-export-headline-levels
  :section-numbers       org-export-with-section-numbers
  :table-of-contents     org-export-with-toc
  :emphasize             org-export-with-emphasize
  :sub-superscript       org-export-with-sub-superscripts
  :TeX-macros            org-export-with-TeX-macros
  :fixed-width           org-export-with-fixed-width
  :tables                org-export-with-tables
  :table-auto-headline   org-export-highlight-first-table-line
  :style                 org-export-html-style
  :convert-org-links     org-export-html-link-org-files-as-html
  :inline-images         org-export-html-inline-images
  :expand-quoted-html    org-export-html-expand
  :timestamp             org-export-html-with-timestamp
  :publishing-directory  org-export-publishing-directory
  :preamble              org-export-html-preamble
  :postamble             org-export-html-postamble
  :auto-preamble         org-export-html-auto-preamble
  :auto-postamble        org-export-html-auto-postamble
  :author                user-full-name
  :email                 user-mail-address

The following properties may be used to control publishing of an
index of files or summary page for a given project.

  :auto-index            Whether to publish an index during
                         org-publish-current-project or org-publish-all.
  :index-filename        Filename for output of index. Defaults
                         to 'index.org' (which becomes 'index.html')
  :index-title           Title of index page. Defaults to name of file.
  :index-function        Plugin function to use for generation of index.
                         Defaults to 'org-publish-org-index', which
                         generates a plain list of links to all files
                         in the project."
  :group 'org-publish
  :type 'alist)

(defcustom org-publish-use-timestamps-flag t
  "When non-nil, use timestamp checking to publish only changed files.
When nil, do no timestamp checking and always publish all
files."
  :group 'org-publish
  :type 'boolean)

(defcustom org-publish-timestamp-directory "~/.org-timestamps/"
  "Name of directory in which to store publishing timestamps."
  :group 'org-publish
  :type 'directory)

(defcustom org-publish-before-export-hook nil
  "Hook run before export on the Org file.
If the functions in this hook modify the original Org buffer, the
modified buffer will be used for export, but the buffer will be
restored and saved back to its initial state after export."
  :group 'org-publish
  :type 'hook)

(defcustom org-publish-after-export-hook nil
  "Hook run after export on the exported buffer.
If functions in this hook modify the buffer, it will be saved."
  :group 'org-publish
  :type 'hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Timestamp-related functions

(defun org-publish-timestamp-filename (filename)
  "Return path to timestamp file for filename FILENAME."
  (while (string-match
	  (if (eq system-type 'windows-nt) "~\\|/\\|:" "~\\|/") filename)
    (setq filename (replace-match "_" nil t filename)))
  (concat org-publish-timestamp-directory filename ".timestamp"))

(defun org-publish-needed-p (filename)
  "Return `t' if FILENAME should be published."
  (if org-publish-use-timestamps-flag
      (if (file-exists-p org-publish-timestamp-directory)
	  ;; first handle possible wrong timestamp directory
	  (if (not (file-directory-p org-publish-timestamp-directory))
	      (error "Org publish timestamp: %s is not a directory"
		     org-publish-timestamp-directory)
	    ;; there is a timestamp, check if FILENAME is newer
	    (file-newer-than-file-p
	     filename (org-publish-timestamp-filename filename))))
    ;; don't use timestamps, always return t
    t))

(defun org-publish-update-timestamp (filename)
  "Update publishing timestamp for file FILENAME.
If there is no timestamp, create one."
  (let ((timestamp-file (org-publish-timestamp-filename filename))
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
      (call-process "touch" nil 0 nil timestamp-file))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mapping files to project names

(defvar org-publish-files-alist nil
  "Alist of files and their parent project.
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
  "Expand projects contained in PROJECTS-ALIST."
  (let (without-component with-component)
    (mapc (lambda(p)
	    (add-to-list
	     (if (plist-get (cdr p) :components)
		 'with-component 'without-component) p))
	  projects-alist)
    (org-publish-delete-dups
     (append without-component
	     (car (mapcar (lambda(p) (org-publish-expand-components p))
			  with-component))))))

(defun org-publish-expand-components (project)
  "Expand PROJECT into an alist of its components."
  (let* ((components (plist-get (cdr project) :components)))
    (org-publish-delete-dups
     (delq nil (mapcar (lambda(c) (assoc c org-publish-project-alist))
		       components)))))

(defun org-publish-get-base-files-1 (base-dir &optional recurse match skip-file skip-dir)
  "Set `org-publish-temp-files' with files from BASE-DIR directory.
If RECURSE is non-nil, check BASE-DIR recursively.  If MATCH is
non-nil, restrict this list to the files matching the regexp
MATCH.  If SKIP-FILE is non-nil, skip file matching the regexp
SKIP-FILE.  If SKIP-DIR is non-nil, don't check directories
matching the regexp SKIP-DIR when recursiing through BASE-DIR."
  (mapc (lambda (f)
	  (let ((fd-p (car (file-attributes f)))
		(fnd (file-name-nondirectory f)))
	    (if (and fd-p recurse
		     (not (string-match "^\\.+$" fnd))
		     (if skip-dir (not (string-match skip-dir fnd)) t))
		(org-publish-get-base-files-1 f recurse match skip-file skip-dir)
	      (unless (or fd-p ;; this is a directory
			  (and skip-file (string-match skip-file fnd))
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
 	 (match (concat "^[^\\.].*\\.\\(" extension "\\)$")))
    (setq org-publish-temp-files nil)
    (org-publish-get-base-files-1 base-dir recurse match
				  ;; FIXME distinguish exclude regexp
				  ;; for skip-file and skip-dir?
				  exclude-regexp exclude-regexp)
    org-publish-temp-files))

(defun org-publish-get-project-from-filename (filename)
  "Return the project FILENAME belongs."
  (let* ((project-name (cdr (assoc (expand-file-name filename)
				   org-publish-files-alist))))
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
  (find-file filename)
  (let ((init-buf (current-buffer))
	(init-point (point))
	(init-buf-string (buffer-string)) export-buf)
    ;; run hooks before exporting
    (run-hooks 'org-publish-before-export-hook)
    ;; export the possibly modified buffer
    (setq export-buf
	  (funcall (intern (concat "org-export-as-" format))
		   (plist-get plist :headline-levels)
		   nil plist nil nil pub-dir))
    (set-buffer export-buf)
    ;; run hooks after export and save export
    (and (run-hooks 'org-publish-after-export-hook)
	 (if (buffer-modified-p) (save-buffer)))
    (kill-buffer export-buf)
    ;; maybe restore buffer's content
    (set-buffer init-buf)
    (when (buffer-modified-p init-buf)
      (erase-buffer)
      (insert init-buf-string)
      (save-buffer)
      (goto-char init-point))
    (unless (eq init-buf org-publish-initial-buffer)
      (kill-buffer init-buf))))

(defun org-publish-org-to-latex (plist filename pub-dir)
  "Publish an org file to LaTeX.
See `org-publish-org-to' to the list of arguments."
  (org-publish-org-to "latex" plist filename pub-dir))

(defun org-publish-org-to-html (plist filename pub-dir)
  "Publish an org file to HTML.
See `org-publish-org-to' to the list of arguments."
  (org-publish-org-to "html" plist filename pub-dir))

(defun org-publish-attachment (plist filename pub-dir)
  "Publish a file with no transformation of any kind.
See `org-publish-org-to' to the list of arguments."
  ;; make sure eshell/cp code is loaded
  (eval-and-compile
    (require 'eshell)
    (require 'esh-maint)
    (require 'em-unix))
  (eshell/cp filename pub-dir))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Publishing files, sets of files, and indices

(defun org-publish-file (filename &optional project)
  "Publish file FILENAME from PROJECT."
  (when (org-publish-needed-p filename)
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
		     (and (string-match (regexp-quote base-dir) filename)
			  (substring filename (match-end 0))))))
      (if (listp publishing-function)
	  ;; allow chain of publishing functions
	  (mapc (lambda (f)
		  (funcall f project-plist filename tmp-pub-dir))
		publishing-function)
	(funcall publishing-function project-plist filename tmp-pub-dir)))
    (org-publish-update-timestamp filename)))

(defun org-publish-projects (projects)
  "Publish all files belonging to the PROJECTS alist.
If :auto-index is set, publish the index too."
  (mapc
   (lambda (project)
     (let* ((project-plist (cdr project))
	    (exclude-regexp (plist-get project-plist :exclude))
	    (index-p (plist-get project-plist :auto-index))
	    (index-filename (or (plist-get project-plist :index-filename)
				"index.org"))
	    (index-function (or (plist-get project-plist :index-function)
				'org-publish-org-index))
	    (preparation-function (plist-get project-plist :preparation-function))
	    (files (org-publish-get-base-files project exclude-regexp)) file)
       (when preparation-function (funcall preparation-function))
       (if index-p (funcall index-function project index-filename))
       (while (setq file (pop files))
	 (org-publish-file file project))))
   (org-publish-expand-projects projects)))

(defun org-publish-org-index (project &optional index-filename)
  "Create an index of pages in set defined by PROJECT.
Optionally set the filename of the index with INDEX-FILENAME.
Default for INDEX-FILENAME is 'index.org'."
  (let* ((project-plist (cdr project))
	 (dir (file-name-as-directory
	       (plist-get project-plist :base-directory)))
	 (exclude-regexp (plist-get project-plist :exclude))
	 (files (org-publish-get-base-files project exclude-regexp))
	 (index-filename (concat dir (or index-filename "index.org")))
	 (index-buffer (find-buffer-visiting index-filename))
	 (ifn (file-name-nondirectory index-filename))
	 file)
    ;; if buffer is already open, kill it to prevent error message
    (if index-buffer
	(kill-buffer index-buffer))
    (with-temp-buffer
      (while (setq file (pop files))
	(let ((fn (file-name-nondirectory file)))
	  ;; index shouldn't index itself
	  (unless (string= fn ifn)
	    (insert (concat " + [[file:" fn "]["
			    (file-name-sans-extension fn)
			    "]]\n")))))
      (write-file index-filename)
      (kill-buffer (current-buffer)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interactive publishing functions

(defalias 'org-publish-project 'org-publish)

;;;###autoload
(defun org-publish (project &optional force)
  "Publish PROJECT."
  (interactive "P")
  (setq org-publish-initial-buffer (current-buffer))
  (save-window-excursion
    (let* ((force current-prefix-arg)
	   (org-publish-use-timestamps-flag
	    (if force nil org-publish-use-timestamps-flag)))
      (org-publish-projects
       (list (or project
		 (assoc (completing-read
			 "Publish project: "
			 org-publish-project-alist nil t)
			org-publish-project-alist)))))))

;;;###autoload
(defun org-publish-all (&optional force)
  "Publish all projects.
With prefix argument, force publish all files."
  (interactive "P")
  (org-publish-initialize-files-alist)
  (save-window-excursion
    (let ((org-publish-use-timestamps-flag
	   (if force nil org-publish-use-timestamps-flag)))
      (org-publish-projects org-publish-project-alist))))

;;;###autoload
(defun org-publish-current-file (&optional force)
  "Publish the current file.
With prefix argument, force publish the file."
  (interactive "P")
  (org-publish-initialize-files-alist)
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
  (org-publish-initialize-files-alist)
  (save-window-excursion
    (let ((project (org-publish-get-project-from-filename (buffer-file-name)))
	  (org-publish-use-timestamps-flag
	   (if force nil org-publish-use-timestamps-flag)))
      (if (not project)
	  (error "File %s is not part of any known project" (buffer-file-name)))
      (org-publish project))))

(provide 'org-publish)


;; arch-tag: 72807f3c-8af0-4a6b-8dca-c3376eb25adb
;;; org-publish.el ends here
