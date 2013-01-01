;;; admin.el --- utilities for Emacs administration

;; Copyright (C) 2001-2013 Free Software Foundation, Inc.

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

;; add-release-logs	Add ``Version X released'' change log entries.
;; set-version		Change Emacs version number in source tree.
;; set-copyright        Change emacs short copyright string (eg as
;;                      printed by --version) in source tree.

;;; Code:

(defvar add-log-time-format)		; in add-log

(defun add-release-logs (root version)
  "Add \"Version VERSION released.\" change log entries in ROOT.
Root must be the root of an Emacs source tree."
  (interactive "DEmacs root directory: \nNVersion number: ")
  (setq root (expand-file-name root))
  (unless (file-exists-p (expand-file-name "src/emacs.c" root))
    (error "%s doesn't seem to be the root of an Emacs source tree" root))
  (require 'add-log)
  (let* ((logs (process-lines "find" root "-name" "ChangeLog"))
	 (entry (format "%s  %s  <%s>\n\n\t* Version %s released.\n\n"
			(funcall add-log-time-format)
			(or add-log-full-name (user-full-name))
			(or add-log-mailing-address user-mail-address)
			version)))
    (dolist (log logs)
      (unless (string-match "/gnus/" log)
	(find-file log)
	(goto-char (point-min))
	(insert entry)))))

(defun set-version-in-file (root file version rx)
  (find-file (expand-file-name file root))
  (goto-char (point-min))
  (unless (re-search-forward rx nil t)
    (error "Version not found in %s" file))
  (replace-match (format "%s" version) nil nil nil 1))

(defun set-version (root version)
  "Set Emacs version to VERSION in relevant files under ROOT.
Root must be the root of an Emacs source tree."
  (interactive "DEmacs root directory: \nsVersion number: ")
  (unless (file-exists-p (expand-file-name "src/emacs.c" root))
    (error "%s doesn't seem to be the root of an Emacs source tree" root))
  (set-version-in-file root "README" version
		       (rx (and "version" (1+ space)
				(submatch (1+ (in "0-9."))))))
  (set-version-in-file root "configure.ac" version
		       (rx (and "AC_INIT" (1+ (not (in ?,)))
                                ?, (0+ space)
                                (submatch (1+ (in "0-9."))))))
  (set-version-in-file root "doc/emacs/emacsver.texi" version
		       (rx (and "EMACSVER" (1+ space)
				(submatch (1+ (in "0-9."))))))
  (set-version-in-file root "doc/man/emacs.1" version
		       (rx (and ".TH EMACS" (1+ not-newline)
                                "GNU Emacs" (1+ space)
				(submatch (1+ (in "0-9."))))))
  (set-version-in-file root "nt/config.nt" version
		       (rx (and bol "#" (0+ blank) "define" (1+ blank)
				"VERSION" (1+ blank) "\""
				(submatch (1+ (in "0-9."))))))
  (set-version-in-file root "msdos/sed2v2.inp" version
		       (rx (and bol "/^#undef " (1+ not-newline)
				"define VERSION" (1+ space) "\""
				(submatch (1+ (in "0-9."))))))
  (set-version-in-file root "nt/makefile.w32-in" version
		       (rx (and "VERSION" (0+ space) "=" (0+ space)
				(submatch (1+ (in "0-9."))))))
  ;; nt/emacs.rc also contains the version number, but in an awkward
  ;; format. It must contain four components, separated by commas, and
  ;; in two places those commas are followed by space, in two other
  ;; places they are not.
  (let* ((version-components (append (split-string version "\\.")
				    '("0" "0")))
	 (comma-version
	  (concat (car version-components) ","
		  (cadr version-components) ","
		  (cadr (cdr version-components)) ","
		  (cadr (cdr (cdr version-components)))))
	 (comma-space-version
	  (concat (car version-components) ", "
		  (cadr version-components) ", "
		  (cadr (cdr version-components)) ", "
		  (cadr (cdr (cdr version-components))))))
    (set-version-in-file root "nt/emacs.rc" comma-version
			 (rx (and "FILEVERSION" (1+ space)
				  (submatch (1+ (in "0-9,"))))))
    (set-version-in-file root "nt/emacs.rc" comma-version
			 (rx (and "PRODUCTVERSION" (1+ space)
				  (submatch (1+ (in "0-9,"))))))
    (set-version-in-file root "nt/emacs.rc" comma-space-version
			 (rx (and "\"FileVersion\"" (0+ space) ?, (0+ space)
				  ?\" (submatch (1+ (in "0-9, "))) "\\0\"")))
    (set-version-in-file root "nt/emacs.rc" comma-space-version
			 (rx (and "\"ProductVersion\"" (0+ space) ?,
				  (0+ space) ?\" (submatch (1+ (in "0-9, ")))
				  "\\0\"")))
    ;; Likewise for emacsclient.rc
    (set-version-in-file root "nt/emacsclient.rc" comma-version
			 (rx (and "FILEVERSION" (1+ space)
				  (submatch (1+ (in "0-9,"))))))
    (set-version-in-file root "nt/emacsclient.rc" comma-version
			 (rx (and "PRODUCTVERSION" (1+ space)
				  (submatch (1+ (in "0-9,"))))))
    (set-version-in-file root "nt/emacsclient.rc" comma-space-version
			 (rx (and "\"FileVersion\"" (0+ space) ?, (0+ space)
				  ?\" (submatch (1+ (in "0-9, "))) "\\0\"")))
    (set-version-in-file root "nt/emacsclient.rc" comma-space-version
			 (rx (and "\"ProductVersion\"" (0+ space) ?,
				  (0+ space) ?\" (submatch (1+ (in "0-9, ")))
				  "\\0\"")))
    ;; Major version only.
    (when (string-match "\\([0-9]\\{2,\\}\\)" version)
      (setq version (match-string 1 version))
      (set-version-in-file root "src/msdos.c" version
			   (rx (and "Vwindow_system_version" (1+ not-newline)
				    ?\( (submatch (1+ (in "0-9"))) ?\))))
      (set-version-in-file root "etc/refcards/ru-refcard.tex" version
			   "\\\\newcommand{\\\\versionemacs}\\[0\\]\
{\\([0-9]\\{2,\\}\\)}.+%.+version of Emacs")
      (set-version-in-file root "etc/refcards/emacsver.tex" version
			   "\\\\def\\\\versionemacs\
{\\([0-9]\\{2,\\}\\)}.+%.+version of Emacs"))))


;; Note this makes some assumptions about form of short copyright.
(defun set-copyright (root copyright)
  "Set Emacs short copyright to COPYRIGHT in relevant files under ROOT.
Root must be the root of an Emacs source tree."
  (interactive (list
                (read-directory-name "Emacs root directory: " nil nil t)
                (read-string
                 "Short copyright string: "
                 (format "Copyright (C) %s Free Software Foundation, Inc."
                         (format-time-string "%Y")))))
  (unless (file-exists-p (expand-file-name "src/emacs.c" root))
    (error "%s doesn't seem to be the root of an Emacs source tree" root))
  (set-version-in-file root "configure.ac" copyright
		       (rx (and bol "copyright" (0+ (not (in ?\")))
        			?\" (submatch (1+ (not (in ?\")))) ?\")))
  (set-version-in-file root "nt/config.nt" copyright
		       (rx (and bol "#" (0+ blank) "define" (1+ blank)
				"COPYRIGHT" (1+ blank)
				?\" (submatch (1+ (not (in ?\")))) ?\")))
  (set-version-in-file root "lib-src/rcs2log" copyright
        	       (rx (and "Copyright" (0+ space) ?= (0+ space)
        			?\' (submatch (1+ nonl)))))
  (when (string-match "\\([0-9]\\{4\\}\\)" copyright)
    (setq copyright (match-string 1 copyright))
    (set-version-in-file root "etc/refcards/ru-refcard.tex" copyright
			 "\\\\newcommand{\\\\cyear}\\[0\\]\
{\\([0-9]\\{4\\}\\)}.+%.+copyright year")
    (set-version-in-file root "etc/refcards/emacsver.tex" copyright
			 "\\\\def\\\\year\
{\\([0-9]\\{4\\}\\)}.+%.+copyright year")))

;;; Various bits of magic for generating the web manuals

(defun make-manuals (root)
  "Generate the web manuals for the Emacs webpage."
  (interactive "DEmacs root directory: ")
  (let* ((dest (expand-file-name "manual" root))
	 (html-node-dir (expand-file-name "html_node" dest))
	 (html-mono-dir (expand-file-name "html_mono" dest))
	 (txt-dir (expand-file-name "text" dest))
	 (dvi-dir (expand-file-name "dvi" dest))
	 (ps-dir (expand-file-name "ps" dest)))
    (when (file-directory-p dest)
      (if (y-or-n-p (format "Directory %s exists, delete it first?" dest))
	  (delete-directory dest t)
	(error "Aborted")))
    (make-directory dest)
    (make-directory html-node-dir)
    (make-directory html-mono-dir)
    (make-directory txt-dir)
    (make-directory dvi-dir)
    (make-directory ps-dir)
    ;; Emacs manual
    (let ((texi (expand-file-name "doc/emacs/emacs.texi" root)))
      (manual-html-node texi (expand-file-name "emacs" html-node-dir))
      (manual-html-mono texi (expand-file-name "emacs.html" html-mono-dir))
      (manual-txt texi (expand-file-name "emacs.txt" txt-dir))
      (manual-pdf texi (expand-file-name "emacs.pdf" dest))
      (manual-dvi texi (expand-file-name "emacs.dvi" dvi-dir)
		  (expand-file-name "emacs.ps" ps-dir)))
    ;; Lisp manual
    (let ((texi (expand-file-name "doc/lispref/elisp.texi" root)))
      (manual-html-node texi (expand-file-name "elisp" html-node-dir))
      (manual-html-mono texi (expand-file-name "elisp.html" html-mono-dir))
      (manual-txt texi (expand-file-name "elisp.txt" txt-dir))
      (manual-pdf texi (expand-file-name "elisp.pdf" dest))
      (manual-dvi texi (expand-file-name "elisp.dvi" dvi-dir)
		  (expand-file-name "elisp.ps" ps-dir)))
    ;; Misc manuals
    (let ((manuals '("ada-mode" "auth" "autotype" "calc" "cc-mode"
		     "cl" "dbus" "dired-x" "ebrowse" "ede" "ediff"
		     "edt" "eieio" "emacs-mime" "epa" "erc" "ert"
		     "eshell" "eudc" "faq" "flymake" "forms"
		     "gnus" "emacs-gnutls" "idlwave" "info"
		     "mairix-el" "message" "mh-e" "newsticker"
		     "nxml-mode" "org" "pcl-cvs" "pgg" "rcirc"
		     "remember" "reftex" "sasl" "sc" "semantic"
		     "ses" "sieve" "smtpmail" "speedbar" "tramp"
		     "url" "vip" "viper" "widget" "woman")))
      (dolist (manual manuals)
	(manual-misc-html manual root html-node-dir html-mono-dir)))
    (message "Manuals created in %s" dest)))

(defconst manual-doctype-string
  "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"
\"http://www.w3.org/TR/html4/loose.dtd\">\n\n")

(defconst manual-meta-string
  "<meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\">
<link rev=\"made\" href=\"mailto:webmasters@gnu.org\">
<link rel=\"icon\" type=\"image/png\" href=\"/graphics/gnu-head-mini.png\">
<meta name=\"ICBM\" content=\"42.256233,-71.006581\">
<meta name=\"DC.title\" content=\"gnu.org\">\n\n")

(defconst manual-style-string "<style type=\"text/css\">
@import url('/style.css');\n</style>\n")

(defun manual-misc-html (name root html-node-dir html-mono-dir)
  (let ((texi (expand-file-name (format "doc/misc/%s.texi" name) root)))
    (manual-html-node texi (expand-file-name name html-node-dir))
    (manual-html-mono texi (expand-file-name (concat name ".html")
					     html-mono-dir))))

(defun manual-html-mono (texi-file dest)
  "Run Makeinfo on TEXI-FILE, emitting mono HTML output to DEST.
This function also edits the HTML files so that they validate as
HTML 4.01 Transitional, and pulls in the gnu.org stylesheet using
the @import directive."
  (call-process "makeinfo" nil nil nil
		"--html" "--no-split" texi-file "-o" dest)
  (with-temp-buffer
    (insert-file-contents dest)
    (setq buffer-file-name dest)
    (manual-html-fix-headers)
    (manual-html-fix-index-1)
    (manual-html-fix-index-2 t)
    (manual-html-fix-node-div)
    (goto-char (point-max))
    (re-search-backward "</body>[\n \t]*</html>")
    (insert "</div>\n\n")
    (save-buffer)))

(defun manual-html-node (texi-file dir)
  "Run Makeinfo on TEXI-FILE, emitting per-node HTML output to DIR.
This function also edits the HTML files so that they validate as
HTML 4.01 Transitional, and pulls in the gnu.org stylesheet using
the @import directive."
  (unless (file-exists-p texi-file)
    (error "Manual file %s not found" texi-file))
  (call-process "makeinfo" nil nil nil
		"--html" texi-file "-o" dir)
  ;; Loop through the node files, fixing them up.
  (dolist (f (directory-files dir nil "\\.html\\'"))
    (let (opoint)
      (with-temp-buffer
	(insert-file-contents (expand-file-name f dir))
	(setq buffer-file-name (expand-file-name f dir))
	(if (looking-at "<meta http-equiv")
	    ;; Ignore those HTML files that are just redirects.
	    (set-buffer-modified-p nil)
	  (manual-html-fix-headers)
	  (if (equal f "index.html")
	      (let (copyright-text)
		(manual-html-fix-index-1)
		;; Move copyright notice to the end.
		(when (re-search-forward "[ \t]*<p>Copyright &copy;" nil t)
		  (setq opoint (match-beginning 0))
		  (re-search-forward "</blockquote>")
		  (setq copyright-text (buffer-substring opoint (point)))
		  (delete-region opoint (point)))
		(manual-html-fix-index-2)
		(if copyright-text
		    (insert copyright-text))
		(insert "\n</div>\n"))
	    ;; For normal nodes, give the header div a blue bg.
	    (manual-html-fix-node-div))
	  (save-buffer))))))

(defun manual-txt (texi-file dest)
  "Run Makeinfo on TEXI-FILE, emitting plaintext output to DEST."
  (call-process "makeinfo" nil nil nil
		"--plaintext" "--no-split" texi-file "-o" dest)
  (shell-command (concat "gzip -c " dest " > " (concat dest ".gz"))))

(defun manual-pdf (texi-file dest)
  "Run texi2pdf on TEXI-FILE, emitting plaintext output to DEST."
  (call-process "texi2pdf" nil nil nil texi-file "-o" dest))

(defun manual-dvi (texi-file dest ps-dest)
  "Run texi2dvi on TEXI-FILE, emitting dvi output to DEST.
Also generate PostScript output in PS-DEST."
  (call-process "texi2dvi" nil nil nil texi-file "-o" dest)
  (call-process "dvips" nil nil nil dest "-o" ps-dest)
  (call-process "gzip" nil nil nil dest)
  (call-process "gzip" nil nil nil ps-dest))

(defun manual-html-fix-headers ()
  "Fix up HTML headers for the Emacs manual in the current buffer."
  (let (opoint)
    (insert manual-doctype-string)
    (search-forward "<head>\n")
    (insert manual-meta-string)
    (search-forward "<meta")
    (setq opoint (match-beginning 0))
    (re-search-forward "<!--")
    (goto-char (match-beginning 0))
    (delete-region opoint (point))
    (insert manual-style-string)
    (search-forward "<meta http-equiv=\"Content-Style")
    (setq opoint (match-beginning 0))
    (search-forward "</head>")
    (delete-region opoint (match-beginning 0))))

(defun manual-html-fix-node-div ()
  "Fix up HTML \"node\" divs in the current buffer."
  (let (opoint div-end)
    (while (search-forward "<div class=\"node\">" nil t)
      (replace-match
       "<div class=\"node\" style=\"background-color:#DDDDFF\">"
       t t)
      (setq opoint (point))
      (re-search-forward "</div>")
      (setq div-end (match-beginning 0))
      (goto-char opoint)
      (if (search-forward "<hr>" div-end 'move)
	  (replace-match "" t t)))))

(defun manual-html-fix-index-1 ()
  (let (opoint)
    (re-search-forward "<body>\n")
    (setq opoint (match-end 0))
    (search-forward "<h2 class=\"")
    (goto-char (match-beginning 0))
    (delete-region opoint (point))
    (insert "<div id=\"content\" class=\"inner\">\n\n")))

(defun manual-html-fix-index-2 (&optional table-workaround)
  "Replace the index list in the current buffer with a HTML table."
  (let (done open-td tag desc)
    ;; Convert the list that Makeinfo made into a table.
    (or (search-forward "<ul class=\"menu\">" nil t)
	(search-forward "<ul>"))
    (replace-match "<table style=\"float:left\" width=\"100%\">")
    (forward-line 1)
    (while (not done)
      (cond
       ((or (looking-at "<li>\\(<a.+</a>\\):[ \t]+\\(.*\\)$")
	    (looking-at "<li>\\(<a.+</a>\\)$"))
	(setq tag (match-string 1))
	(setq desc (match-string 2))
	(replace-match "" t t)
	(when open-td
	  (save-excursion
	    (forward-char -1)
	    (skip-chars-backward " ")
	    (delete-region (point) (line-end-position))
	    (insert "</td>\n  </tr>")))
	(insert "  <tr>\n    ")
	(if table-workaround
	    ;; This works around a Firefox bug in the mono file.
	    (insert "<td bgcolor=\"white\">")
	  (insert "<td>"))
	(insert tag "</td>\n    <td>" (or desc ""))
	(setq open-td t))
       ((eq (char-after) ?\n)
	(delete-char 1)
	;; Negate the following `forward-line'.
	(forward-line -1))
       ((looking-at "<!-- ")
	(search-forward "-->"))
       ((looking-at "<p>[- ]*The Detailed Node Listing[- \n]*")
	(replace-match "  </td></tr></table>\n
<h3>Detailed Node Listing</h3>\n\n" t t)
	(search-forward "<p>")
	(search-forward "<p>" nil t)
	(goto-char (match-beginning 0))
	(skip-chars-backward "\n ")
	(setq open-td nil)
	(insert "</p>\n\n<table  style=\"float:left\" width=\"100%\">"))
       ((looking-at "</li></ul>")
	(replace-match "" t t))
       ((looking-at "<p>")
	(replace-match "" t t)
	(when open-td
	  (insert "  </td></tr>")
	  (setq open-td nil))
	(insert "  <tr>
    <th colspan=\"2\" align=\"left\" style=\"text-align:left\">")
	(if (re-search-forward "</p>[ \t\n]*<ul class=\"menu\">" nil t)
	    (replace-match "  </th></tr>")))
       ((looking-at "[ \t]*</ul>[ \t]*$")
	(replace-match
	 (if open-td
	     "  </td></tr>\n</table>"
	   "</table>") t t)
	(setq done t))
       (t
	(if (eobp)
	    (error "Parse error in %s" f)) ; f is bound in manual-html-node
	(unless open-td
	  (setq done t))))
      (forward-line 1))))


;; Stuff to check new defcustoms got :version tags.
;; Adapted from check-declare.el.

(defun cusver-find-files (root &optional old)
  "Find .el files beneath directory ROOT that contain defcustoms.
If optional OLD is non-nil, also include defvars."
  (process-lines find-program root
		 "-name" "*.el"
		 "-exec" grep-program
		 "-l" "-E" (format "^[ \\t]*\\(def%s"
				   (if old "(custom|var)"
				     "custom"
				     ))
		 "{}" "+"))

(defvar cusver-new-version (format "%s.%s" emacs-major-version
				   (1+ emacs-minor-version))
  "Version number that new defcustoms should have.")

(defun cusver-scan (file &optional old)
  "Scan FILE for `defcustom' calls.
Return a list with elements of the form (VAR . VER),
This means that FILE contains a defcustom for variable VAR, with
a :version tag having value VER (may be nil).
If optional argument OLD is non-nil, also scan for defvars."
  (let ((m (format "Scanning %s..." file))
	(re (format "^[ \t]*\\((def%s\\)[ \t\n]"
		    (if old "\\(custom\\|var\\)" "\\(custom\\|group\\)")))
        alist var ver form glist grp)
    (message "%s" m)
    (with-temp-buffer
      (insert-file-contents file)
      ;; FIXME we could theoretically be inside a string.
      (while (re-search-forward re nil t)
        (goto-char (match-beginning 1))
        (if (and (setq form (ignore-errors (read (current-buffer))))
		 (setq var (car-safe (cdr-safe form)))
		 ;; Exclude macros, eg (defcustom ,varname ...).
		 (symbolp var))
	    (progn
	      (setq ver (car (cdr-safe (memq :version form))))
	      (if (equal "group" (match-string 2))
		  ;; Group :version could be old.
		  (if (equal ver cusver-new-version)
		      (setq glist (cons (cons var ver) glist)))
		;; If it specifies a group and the whole group has a
		;; version. use that.
		(unless ver
		  (setq grp (car (cdr-safe (memq :group form))))
		  (and grp
		       (setq grp (car (cdr-safe grp))) ; (quote foo) -> foo
		       (setq ver (assq grp glist))))
		(setq alist (cons (cons var ver) alist))))
          (if form (message "Malformed defcustom: `%s'" form)))))
    (message "%sdone" m)
    alist))

(defun cusver-scan-cus-start (file)
  "Scan cus-start.el and return an alist with elements (VAR . VER)."
  (if (file-readable-p file)
      (with-temp-buffer
	(insert-file-contents file)
	(when (search-forward "(let ((all '(" nil t)
	  (backward-char 1)
	  (let (var ver alist)
	    (dolist (elem (ignore-errors (read (current-buffer))))
	      (when (symbolp (setq var (car-safe elem)))
		(or (stringp (setq ver (nth 3 elem)))
		    (setq ver nil))
		(setq alist (cons (cons var ver) alist))))
	    alist)))))

(define-button-type 'cusver-xref 'action #'cusver-goto-xref)

(defun cusver-goto-xref (button)
  "Jump to a lisp file for the BUTTON at point."
  (let ((file (button-get button 'file))
	(var (button-get button 'var)))
    (if (not (file-readable-p file))
	(message "Cannot read `%s'" file)
      (with-current-buffer (find-file-noselect file)
	(goto-char (point-min))
	(or (re-search-forward (format "^[ \t]*(defcustom[ \t]*%s" var) nil t)
	    (message "Unable to locate defcustom"))
	(pop-to-buffer (current-buffer))))))

;; You should probably at least do a grep over the old directory
;; to check the results of this look sensible.
;; TODO Check cus-start if something moved from C to Lisp.
;; TODO Handle renamed things with aliases to the old names.
(defun cusver-check (newdir olddir version)
  "Check that defcustoms have :version tags where needed.
NEWDIR is the current lisp/ directory, OLDDIR is that from the previous
release.  A defcustom that is only in NEWDIR should have a :version
tag.  We exclude cases where a defvar exists in OLDDIR, since
just converting a defvar to a defcustom does not require a :version bump.

Note that a :version tag should also be added if the value of a defcustom
changes (in a non-trivial way).  This function does not check for that."
  (interactive (list (read-directory-name "New Lisp directory: ")
		     (read-directory-name "Old Lisp directory: ")
		     (number-to-string
		      (read-number "New version number: "
				   (string-to-number cusver-new-version)))))
  (or (file-directory-p (setq newdir (expand-file-name newdir)))
      (error "Directory `%s' not found" newdir))
  (or (file-directory-p (setq olddir (expand-file-name olddir)))
      (error "Directory `%s' not found" olddir))
  (setq cusver-new-version version)
  (let* ((newfiles (progn (message "Finding new files with defcustoms...")
			  (cusver-find-files newdir)))
	 (oldfiles (progn (message "Finding old files with defcustoms...")
			  (cusver-find-files olddir t)))
	 (newcus (progn (message "Reading new defcustoms...")
			(mapcar
			 (lambda (file)
			   (cons file (cusver-scan file))) newfiles)))
	 oldcus result thisfile file)
    (message "Reading old defcustoms...")
    (dolist (file oldfiles)
      (setq oldcus (append oldcus (cusver-scan file t))))
    (setq oldcus (append oldcus (cusver-scan-cus-start
				 (expand-file-name "cus-start.el" olddir))))
    ;; newcus has elements (FILE (VAR VER) ... ).
    ;; oldcus just (VAR . VER).
    (message "Checking for version tags...")
    (dolist (new newcus)
      (setq file (car new)
	    thisfile
	    (let (missing var)
	      (dolist (cons (cdr new))
		(or (cdr cons)
		    (assq (setq var (car cons)) oldcus)
		    (push var missing)))
	      (if missing
		  (cons file missing))))
      (if thisfile
	  (setq result (cons thisfile result))))
    (message "Checking for version tags... done")
    (if (not result)
	(message "No missing :version tags")
      (pop-to-buffer "*cusver*")
      (erase-buffer)
      (insert "These defcustoms might be missing :version tags:\n\n")
      (dolist (elem result)
	(let* ((str (file-relative-name (car elem) newdir))
	       (strlen (length str)))
	  (dolist (var (cdr elem))
	    (insert (format "%s: %s\n" str var))
	    (make-text-button (+ (line-beginning-position 0) strlen 2)
			      (line-end-position 0)
			      'file (car elem)
			      'var var
			      'help-echo "Mouse-2: visit this definition"
			      :type 'cusver-xref)))))))

(provide 'admin)

;;; admin.el ends here
