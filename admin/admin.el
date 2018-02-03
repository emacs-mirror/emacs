;;; admin.el --- utilities for Emacs administration

;; Copyright (C) 2001-2018 Free Software Foundation, Inc.

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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; add-release-logs	Add ``Version X released'' change log entries.
;; set-version		Change Emacs version number in source tree.
;; set-copyright        Change Emacs short copyright string (eg as
;;                      printed by --version) in source tree.

;;; Code:

(defvar add-log-time-format)		; in add-log

(defun add-release-logs (root version &optional date)
  "Add \"Version VERSION released.\" change log entries in ROOT.
Also update the etc/HISTORY file.
Root must be the root of an Emacs source tree.
Optional argument DATE is the release date, default today."
  (interactive (list (read-directory-name "Emacs root directory: ")
		     (read-string "Version number: "
				  (format "%s.%s" emacs-major-version
					  emacs-minor-version))
		     (read-string "Release date: "
				  (progn (require 'add-log)
                                         (funcall add-log-time-format nil t)))))
  (setq root (expand-file-name root))
  (unless (file-exists-p (expand-file-name "src/emacs.c" root))
    (user-error "%s doesn't seem to be the root of an Emacs source tree" root))
  (let ((clog (expand-file-name "ChangeLog" root)))
    (if (file-exists-p clog)
        ;; Basic check that a ChangeLog that exists is not your personal one.
        ;; TODO Perhaps we should move any existing file and unconditionally
        ;; call make ChangeLog?  Or make ChangeLog CHANGELOG=temp and compare
        ;; with the existing?
        (with-temp-buffer
          (insert-file-contents clog)
          (or (re-search-forward "^[ \t]*Copyright.*Free Software" nil t)
              (user-error "ChangeLog looks like a personal one - remove it?")))
      (or
       (zerop (call-process "make" nil nil nil "-C" root "ChangeLog"))
       (error "Problem generating ChangeLog"))))
  (require 'add-log)
  (or date (setq date (funcall add-log-time-format nil t)))
  (let* ((logs (process-lines "find" root "-name" "ChangeLog"))
	 (entry (format "%s  %s  <%s>\n\n\t* Version %s released.\n\n"
			date
			(or add-log-full-name (user-full-name))
			(or add-log-mailing-address user-mail-address)
			version)))
    (dolist (log logs)
      (find-file log)
      (goto-char (point-min))
      (insert entry)))
  (let ((histfile (expand-file-name "etc/HISTORY" root)))
    (unless (file-exists-p histfile)
      (error "%s not present" histfile))
    (find-file histfile)
    (goto-char (point-max))
    (search-backward "")
    (insert (format "GNU Emacs %s (%s) emacs-%s\n\n" version date version))))

(defun set-version-in-file (root file version rx)
  "Subroutine of `set-version' and `set-copyright'."
  (find-file (expand-file-name file root))
  (goto-char (point-min))
  (setq version (format "%s" version))
  (unless (re-search-forward rx nil :noerror)
    (user-error "Version not found in %s" file))
  (if (not (equal version (match-string 1)))
      (replace-match version nil nil nil 1)
    (kill-buffer)
    (message "No need to update `%s'" file)))

(defun set-version (root version)
  "Set Emacs version to VERSION in relevant files under ROOT.
Root must be the root of an Emacs source tree."
  (interactive (list
		(read-directory-name "Emacs root directory: " source-directory)
		(read-string "Version number: " emacs-version)))
  (unless (file-exists-p (expand-file-name "src/emacs.c" root))
    (user-error "%s doesn't seem to be the root of an Emacs source tree" root))
  (message "Setting version numbers...")
  ;; There's also a "version 3" (standing for GPLv3) at the end of
  ;; `README', but since `set-version-in-file' only replaces the first
  ;; occurrence, it won't be replaced.
  (set-version-in-file root "README" version
		       (rx (and "version" (1+ space)
				(submatch (1+ (in "0-9."))))))
  (set-version-in-file root "configure.ac" version
		       (rx (and "AC_INIT" (1+ (not (in ?,)))
                                ?, (0+ space)
                                (submatch (1+ (in "0-9."))))))
  (set-version-in-file root "nt/README.W32" version
		       (rx (and "version" (1+ space)
				(submatch (1+ (in "0-9."))))))
  ;; TODO: msdos could easily extract the version number from
  ;; configure.ac with sed, rather than duplicating the information.
  (set-version-in-file root "msdos/sed2v2.inp" version
		       (rx (and bol "/^#undef " (1+ not-newline)
				"define PACKAGE_VERSION" (1+ space) "\""
				(submatch (1+ (in "0-9."))))))
  ;; Major version only.
  (when (string-match "\\([0-9]\\{2,\\}\\)" version)
    (let ((newmajor (match-string 1 version)))
      (set-version-in-file root "src/msdos.c" newmajor
                           (rx (and "Vwindow_system_version" (1+ not-newline)
                                    ?\( (submatch (1+ (in "0-9"))) ?\))))
      (set-version-in-file root "etc/refcards/ru-refcard.tex" newmajor
                           "\\\\newcommand{\\\\versionemacs}\\[0\\]\
{\\([0-9]\\{2,\\}\\)}.+%.+version of Emacs")))
  (let* ((oldversion
          (with-temp-buffer
            (insert-file-contents (expand-file-name "README" root))
            (if (re-search-forward "version \\([0-9.]*\\)" nil t)
                (version-to-list (match-string 1)))))
         (oldmajor (if oldversion (car oldversion)))
         (newversion (version-to-list version))
         (newmajor (car newversion))
         (newshort (format "%s.%s" newmajor
                           (+ (cadr newversion)
                              (if (eq 2 (length newversion)) 0 1))))
         (majorbump (and oldversion (not (equal oldmajor newmajor))))
         (minorbump (and oldversion (not majorbump)
                         (not (equal (cadr oldversion) (cadr newversion)))))
         (newsfile (expand-file-name "etc/NEWS" root))
         (oldnewsfile (expand-file-name (format "etc/NEWS.%s" oldmajor) root)))
    (unless (> (length newversion) 2)   ; pretest or release candidate?
      (with-temp-buffer
        (insert-file-contents newsfile)
        (if (re-search-forward "^\\(+++ *\\|--- *\\)$" nil t)
            (display-warning 'admin
                             "NEWS file still contains temporary markup.
Documentation changes might not have been completed!"))))
    (when (and majorbump
               (not (file-exists-p oldnewsfile)))
      (rename-file newsfile oldnewsfile)
      (find-file oldnewsfile)           ; to prompt you to commit it
      (copy-file oldnewsfile newsfile)
      (with-temp-buffer
        (insert-file-contents newsfile)
        (re-search-forward "is about changes in Emacs version \\([0-9]+\\)")
        (replace-match (number-to-string newmajor) nil nil nil 1)
        (re-search-forward "^See files \\(NEWS\\)")
        (unless (save-match-data
                  (when (looking-at "\\(\\..*\\), \\(\\.\\.\\.\\|…\\)")
                    (replace-match
                     (format ".%s, NEWS.%s" oldmajor (1- oldmajor))
                     nil nil nil 1)
                    t))
          (replace-match (format "NEWS.%s, NEWS" oldmajor) nil nil nil 1)
          (let ((start (line-beginning-position)))
            (search-forward "in older Emacs versions")
            (or (equal start (line-beginning-position))
                (fill-region start (line-beginning-position 2)))))
        (re-search-forward "^$")
        (forward-line -1)
        (let ((start (point)))
          (goto-char (point-max))
          (re-search-backward "^$" nil nil 2)
          (delete-region start (line-beginning-position 0)))
        (write-region nil nil newsfile)))
    (when (or majorbump minorbump)
      (find-file newsfile)
      (goto-char (point-min))
      (if (re-search-forward (format "^\\* .*in Emacs %s" newshort) nil t)
          (progn
            (kill-buffer)
            (message "No need to update etc/NEWS"))
        (goto-char (point-min))
        (re-search-forward "^$")
        (forward-line -1)
        (dolist (s '("Installation Changes" "Startup Changes" "Changes"
                     "Editing Changes"
                     "Changes in Specialized Modes and Packages"
                          "New Modes and Packages"
                          "Incompatible Lisp Changes"
                          "Lisp Changes"))
          (insert (format "\n\n* %s in Emacs %s\n" s newshort)))
        (insert (format "\n\n* Changes in Emacs %s on \
Non-Free Operating Systems\n" newshort)))
      ;; Because we skip "bump version" commits when merging between branches.
      ;; Probably doesn't matter in practice, because NEWS changes
      ;; will only happen on master anyway.
      (message "Commit any NEWS changes separately")))
  (message "Setting version numbers...done"))

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
    (user-error "%s doesn't seem to be the root of an Emacs source tree" root))
  (message "Setting copyrights...")
  (set-version-in-file root "configure.ac" copyright
		       (rx (and bol "copyright" (0+ (not (in ?\")))
        			?\" (submatch (1+ (not (in ?\")))) ?\")))
  (set-version-in-file root "msdos/sed2v2.inp" copyright
		       (rx (and bol "/^#undef " (1+ not-newline)
				"define COPYRIGHT" (1+ space)
				?\" (submatch (1+ (not (in ?\")))) ?\")))
  (set-version-in-file root "lib-src/rcs2log" copyright
        	       (rx (and "Copyright" (0+ space) ?= (0+ space)
        			?\' (submatch (1+ nonl)))))
  (when (string-match "\\([0-9]\\{4\\}\\)" copyright)
    (setq copyright (match-string 1 copyright))
    (set-version-in-file root "etc/refcards/ru-refcard.tex" copyright
			 "\\\\newcommand{\\\\cyear}\\[0\\]\
{\\([0-9]\\{4\\}\\)}.+%.+copyright year")
    (set-version-in-file root "etc/refcards/emacsver.tex.in" copyright
			 "\\\\def\\\\year\
{\\([0-9]\\{4\\}\\)}.+%.+copyright year"))
  (message "Setting copyrights...done"))

;;; Various bits of magic for generating the web manuals

(defun manual-misc-manuals (root)
  "Return doc/misc manuals as list of strings.
ROOT should be the root of an Emacs source tree."
  ;; Similar to `make -C doc/misc echo-info', but works if unconfigured,
  ;; and for INFO_TARGETS rather than INFO_INSTALL.
  (with-temp-buffer
    (insert-file-contents (expand-file-name "doc/misc/Makefile.in" root))
    ;; Should really use expanded value of INFO_TARGETS.
    (search-forward "INFO_COMMON = ")
    (let ((start (point)))
      (end-of-line)
      (while (and (looking-back "\\\\")
		  (zerop (forward-line 1)))
	(end-of-line))
      (append (split-string (replace-regexp-in-string
			     "\\(\\\\\\|\\.info\\)" ""
			     (buffer-substring start (point))))
	      '("efaq-w32")))))

;; TODO report the progress
(defun make-manuals (root &optional type)
  "Generate the web manuals for the Emacs webpage.
ROOT should be the root of an Emacs source tree.
Interactively with a prefix argument, prompt for TYPE.
Optional argument TYPE is type of output (nil means all)."
  (interactive (let ((root (read-directory-name "Emacs root directory: "
						source-directory nil t)))
		 (list root
		       (if current-prefix-arg
			   (completing-read
			    "Type: "
			    (append
			     '("misc" "pdf" "ps")
			     (let (res)
			       (dolist (i '("emacs" "elisp" "eintr") res)
				 (dolist (j '("" "-mono" "-node" "-ps" "-pdf"))
				   (push (concat i j) res))))
			     (manual-misc-manuals root)))))))
  (let* ((dest (expand-file-name "manual" root))
	 (html-node-dir (expand-file-name "html_node" dest))
	 (html-mono-dir (expand-file-name "html_mono" dest))
	 (ps-dir (expand-file-name "ps" dest))
	 (pdf-dir (expand-file-name "pdf" dest))
	 (emacs (expand-file-name "doc/emacs/emacs.texi" root))
	 (emacs-xtra (expand-file-name "doc/emacs/emacs-xtra.texi" root))
	 (elisp (expand-file-name "doc/lispref/elisp.texi" root))
	 (eintr (expand-file-name "doc/lispintro/emacs-lisp-intro.texi" root))
	 (misc (manual-misc-manuals root)))
    ;; TODO this makes it non-continuable.
    ;; Instead, delete the individual dest directory each time.
    (when (file-directory-p dest)
      (if (y-or-n-p (format "Directory %s exists, delete it first? " dest))
	  (delete-directory dest t)
	(user-error "Aborted")))
    (if (member type '(nil "emacs" "emacs-node"))
	(manual-html-node emacs (expand-file-name "emacs" html-node-dir)))
    (if (member type '(nil "emacs" "emacs-mono"))
	(manual-html-mono emacs (expand-file-name "emacs.html" html-mono-dir)))
    (when (member type '(nil "emacs" "emacs-pdf" "pdf"))
      (manual-pdf emacs (expand-file-name "emacs.pdf" pdf-dir))
      ;; emacs-xtra exists only in pdf/ps format.
      ;; In other formats it is included in the Emacs manual.
      (manual-pdf emacs-xtra (expand-file-name "emacs-xtra.pdf" pdf-dir)))
    (when (member type '(nil "emacs" "emacs-ps" "ps"))
      (manual-ps emacs (expand-file-name "emacs.ps" ps-dir))
      (manual-ps emacs-xtra (expand-file-name "emacs-xtra.ps" ps-dir)))
    (if (member type '(nil "elisp" "elisp-node"))
	(manual-html-node elisp (expand-file-name "elisp" html-node-dir)))
    (if (member type '(nil "elisp" "elisp-mono"))
	(manual-html-mono elisp (expand-file-name "elisp.html" html-mono-dir)))
    (if (member type '(nil "elisp" "elisp-pdf" "pdf"))
	(manual-pdf elisp (expand-file-name "elisp.pdf" pdf-dir)))
    (if (member type '(nil "elisp" "elisp-ps" "ps"))
	(manual-ps elisp (expand-file-name "elisp.ps" ps-dir)))
    (if (member type '(nil "eintr" "eintr-node"))
	(manual-html-node eintr (expand-file-name "eintr" html-node-dir)))
    (if (member type '(nil "eintr" "eintr-node"))
	(manual-html-mono eintr (expand-file-name "eintr.html" html-mono-dir)))
    (if (member type '(nil "eintr" "eintr-pdf" "pdf"))
	(manual-pdf eintr (expand-file-name "eintr.pdf" pdf-dir)))
    (if (member type '(nil "eintr" "eintr-ps" "ps"))
	(manual-ps eintr (expand-file-name "eintr.ps" ps-dir)))
    ;; Misc manuals
    (dolist (manual misc)
      (if (member type `(nil ,manual "misc"))
	  (manual-misc-html manual root html-node-dir html-mono-dir)))
    (message "Manuals created in %s" dest)))

(defconst manual-doctype-string
  "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"
\"http://www.w3.org/TR/html4/loose.dtd\">\n\n")

(defconst manual-meta-string
  "<meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\">
<link rev=\"made\" href=\"mailto:bug-gnu-emacs@gnu.org\">
<link rel=\"icon\" type=\"image/png\" href=\"/graphics/gnu-head-mini.png\">
<meta name=\"ICBM\" content=\"42.256233,-71.006581\">
<meta name=\"DC.title\" content=\"gnu.org\">\n\n")

(defconst manual-style-string "<style type=\"text/css\">
@import url('/software/emacs/manual.css');\n</style>\n")

(defun manual-misc-html (name root html-node-dir html-mono-dir)
  ;; Hack to deal with the cases where .texi creates a different .info.
  ;; Blech.  TODO Why not just rename the .texi (or .info) files?
  (let* ((texiname (cond ((equal name "ccmode") "cc-mode")
			 (t name)))
	 (texi (expand-file-name (format "doc/misc/%s.texi" texiname) root)))
    (manual-html-node texi (expand-file-name name html-node-dir))
    (manual-html-mono texi (expand-file-name (concat name ".html")
					     html-mono-dir))))

(defun manual-html-mono (texi-file dest)
  "Run Makeinfo on TEXI-FILE, emitting mono HTML output to DEST.
This function also edits the HTML files so that they validate as
HTML 4.01 Transitional, and pulls in the gnu.org stylesheet using
the @import directive."
  (make-directory (or (file-name-directory dest) ".") t)
  (call-process "makeinfo" nil nil nil
		"-D" "WWW_GNU_ORG"
		"-I" (expand-file-name "../emacs"
				       (file-name-directory texi-file))
		"-I" (expand-file-name "../misc"
				       (file-name-directory texi-file))
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
    ;; Close the div id="content" that fix-index-1 added.
    (insert "</div>\n\n")
    (save-buffer)))

(defun manual-html-node (texi-file dir)
  "Run Makeinfo on TEXI-FILE, emitting per-node HTML output to DIR.
This function also edits the HTML files so that they validate as
HTML 4.01 Transitional, and pulls in the gnu.org stylesheet using
the @import directive."
  (unless (file-exists-p texi-file)
    (user-error "Manual file %s not found" texi-file))
  (make-directory dir t)
  (call-process "makeinfo" nil nil nil
		"-D" "WWW_GNU_ORG"
		"-I" (expand-file-name "../emacs"
				       (file-name-directory texi-file))
		"-I" (expand-file-name "../misc"
				       (file-name-directory texi-file))
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
		;; Close the div id="content" that fix-index-1 added.
		(insert "\n</div>\n"))
	    ;; For normal nodes, give the header div a blue bg.
	    (manual-html-fix-node-div t))
	  (save-buffer))))))

(defun manual-pdf (texi-file dest)
  "Run texi2pdf on TEXI-FILE, emitting PDF output to DEST."
  (make-directory (or (file-name-directory dest) ".") t)
  (let ((default-directory (file-name-directory texi-file)))
    (call-process "texi2pdf" nil nil nil
		  "-I" "../emacs" "-I" "../misc"
		  texi-file "-o" dest)))

(defun manual-ps (texi-file dest)
  "Generate a PostScript version of TEXI-FILE as DEST."
  (make-directory (or (file-name-directory dest) ".") t)
  (let ((dvi-dest (concat (file-name-sans-extension dest) ".dvi"))
	(default-directory (file-name-directory texi-file)))
    ;; FIXME: Use `texi2dvi --ps'?  --xfq
    (call-process "texi2dvi" nil nil nil
		  "-I" "../emacs" "-I" "../misc"
		  texi-file "-o" dvi-dest)
    (call-process "dvips" nil nil nil dvi-dest "-o" dest)
    (delete-file dvi-dest)
    (call-process "gzip" nil nil nil dest)))

(defun manual-html-fix-headers ()
  "Fix up HTML headers for the Emacs manual in the current buffer."
  (let ((texi5 (search-forward "<!DOCTYPE" nil t))
	opoint)
    ;; Texinfo 5 supplies a DOCTYPE.
    (or texi5
	(insert manual-doctype-string))
    (search-forward "<head>\n")
    (insert manual-meta-string)
    (search-forward "<meta")
    (setq opoint (match-beginning 0))
    (unless texi5
      (search-forward "<!--")
      (goto-char (match-beginning 0))
      (delete-region opoint (point))
      (search-forward "<meta http-equiv=\"Content-Style")
      (setq opoint (match-beginning 0)))
    (search-forward "</head>")
    (goto-char (match-beginning 0))
    (delete-region opoint (point))
    (insert manual-style-string)
    ;; Remove Texinfo 5 hard-coding bgcolor, text, link, vlink, alink.
    (when (re-search-forward "<body lang=\"[^\"]+\"" nil t)
      (setq opoint (point))
      (search-forward ">")
      (if (> (point) (1+ opoint))
	  (delete-region opoint (1- (point))))
      (search-backward "</head"))))

;; Texinfo 5 changed these from class = "node" to "header", yay.
(defun manual-html-fix-node-div (&optional split)
  "Fix up HTML \"node\" divs in the current buffer."
  (let (opoint div-end type)
    (while (re-search-forward "<div class=\"\\(node\\|header\\)\"\\(>\\)" nil t)
      (setq type (match-string 1))
      ;; NB it is this that makes the bg of non-header cells in the
      ;; index tables be blue.  Is that intended?
      ;; Also, if you don't remove the <hr>, the color of the first
      ;; row in the table will be wrong.
      ;; This all seems rather odd to me...
      (replace-match " style=\"background-color:#DDDDFF\">" t t nil 2)
      (setq opoint (point))
      (when (or split (equal type "node"))
	;; In Texinfo 4, the <hr> (and anchor) comes after the <div>.
	(re-search-forward "</div>")
	(setq div-end (if (equal type "node")
			  (match-beginning 0)
			(line-end-position 2)))
	(goto-char opoint)
	(if (search-forward "<hr>" div-end 'move)
		(replace-match "" t t)
	  (if split (forward-line -1))))
      ;; In Texinfo 5, the <hr> (and anchor) comes before the <div> (?).
      ;; Except in split output, where it comes on the line after
      ;; the <div>.  But only sometimes.  I have no clue what the
      ;; logic of where it goes is.
      (when (equal type "header")
	(goto-char opoint)
	(when (re-search-backward "^<hr>$" (line-beginning-position -3) t)
	  (replace-match "")
	  (goto-char opoint))))))


(defun manual-html-fix-index-1 ()
  "Remove the h1 header, and the short and long contents lists.
Also start a \"content\" div."
  (let (opoint)
    (re-search-forward "<body.*>\n")
    (setq opoint (match-end 0))
    ;; FIXME?  Fragile if a Texinfo 5 document does not use @top.
    (or (re-search-forward "<h1 class=\"top\"" nil t) ; Texinfo 5
	(search-forward "<h2 class=\""))
    (goto-char (match-beginning 0))
    (delete-region opoint (point))
    ;; NB caller must close this div.
    (insert "<div id=\"content\" class=\"inner\">\n\n")))

(defun manual-html-fix-index-2 (&optional table-workaround)
  "Replace the index list in the current buffer with a HTML table.
Leave point after the table."
  (if (re-search-forward "<table class=\"menu\"\\(.*\\)>" nil t)
      ;; Texinfo 5 already uses a table.  Tweak it a bit.
      (let (opoint done)
	(replace-match " style=\"float:left\" width=\"100%\"" nil t nil 1)
	(forward-line 1)
	(while (not done)
	  (cond ((re-search-forward "<tr><td.*&bull; \\(<a.*</a>\\)\
:</td><td>&nbsp;&nbsp;</td><td[^>]*>\\(.*\\)" (line-end-position) t)
		 (replace-match (format "<tr><td%s>\\1</td>\n<td>\\2"
					(if table-workaround
					    " bgcolor=\"white\"" "")))
		 (search-forward "</td></tr>")
		 (forward-line 1))
		((looking-at "<tr><th.*<pre class=\"menu-comment\">\n")
		 (replace-match "<tr><th colspan=\"2\" align=\"left\" \
style=\"text-align:left\">")
		 (search-forward "</pre></th></tr>")
		 (replace-match "</th></tr>\n"))
		;; Not all manuals have the detailed menu.
		;; If it is there, split it into a separate table.
		((re-search-forward "<tr>.*The Detailed Node Listing *"
				    (line-end-position) t)
		 (setq opoint (match-beginning 0))
		 (while (and (looking-at " *&mdash;")
			     (zerop (forward-line 1))))
		 (delete-region opoint (point))
		 (insert "</table>\n\n\
<h2>Detailed Node Listing</h2>\n\n<p>")
		 ;; FIXME Fragile!
		 ;; The Emacs and Elisp manual have some text at the
		 ;; start of the detailed menu that is not part of the menu.
		 ;; Other manuals do not.
		 (if (re-search-forward "in one step:" (line-end-position 3) t)
		     (forward-line 1))
		 (insert "</p>\n")
		 (search-forward "</pre></th></tr>")
		 (delete-region (match-beginning 0) (match-end 0))
		 (forward-line -1)
		 (or (looking-at "^$") (error "Parse error 1"))
		 (forward-line -1)
		 (if (looking-at "^$") (error "Parse error 2"))
		 (forward-line -1)
		 (or (looking-at "^$") (error "Parse error 3"))
		 (forward-line 1)
		 (insert "<table class=\"menu\" style=\"float:left\" width=\"100%\">\n\
<tr><th colspan=\"2\" align=\"left\" style=\"text-align:left\">\n")
		 (forward-line 1)
		 (insert "</th></tr>")
		 (forward-line 1))
		((looking-at ".*</table")
		 (forward-line 1)
		 (setq done t)))))
    (let (done open-td tag desc)
      ;; Convert the list that Makeinfo made into a table.
      (or (search-forward "<ul class=\"menu\">" nil t)
	  ;; FIXME?  The following search seems dangerously lax.
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
	  ;; FIXME Fragile!
	  ;; The Emacs and Elisp manual have some text at the
	  ;; start of the detailed menu that is not part of the menu.
	  ;; Other manuals do not.
	  (if (looking-at "Here are some other nodes")
	      (search-forward "<p>"))
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
	      (error "Parse error in %s"
		     (file-name-nondirectory buffer-file-name)))
	  (unless open-td
	    (setq done t))))
	(forward-line 1)))))


(defconst make-manuals-dist-output-variables
  `(("@\\(top_\\)?srcdir@" . ".")	; top_srcdir is wrong, but not used
    ("^\\(\\(?:texinfo\\|buildinfo\\|emacs\\)dir *=\\).*" . "\\1 .")
    ("^\\(clean:.*\\)" . "\\1 infoclean")
    ("@MAKEINFO@" . "makeinfo")
    ("@MKDIR_P@" . "mkdir -p")
    ("@INFO_EXT@" . ".info")
    ("@INFO_OPTS@" . "")
    ("@SHELL@" . "/bin/bash")
    ("@prefix@" . "/usr/local")
    ("@datarootdir@" . "${prefix}/share")
    ("@datadir@" . "${datarootdir}")
    ("@PACKAGE_TARNAME@" . "emacs")
    ("@docdir@" . "${datarootdir}/doc/${PACKAGE_TARNAME}")
    ("@\\(dvi\\|html\\|pdf\\|ps\\)dir@" . "${docdir}")
    ("@GZIP_PROG@" . "gzip")
    ("@INSTALL@" . "install -c")
    ("@INSTALL_DATA@" . "${INSTALL} -m 644")
    ("@configure_input@" . "")
    ("@AM_DEFAULT_VERBOSITY@" . "0")
    ("@AM_V@" . "${V}")
    ("@AM_DEFAULT_V@" . "${AM_DEFAULT_VERBOSITY}"))
  "Alist of (REGEXP . REPLACEMENT) pairs for `make-manuals-dist'.")

(defun make-manuals-dist--1 (root type)
  "Subroutine of `make-manuals-dist'."
  (let* ((dest (expand-file-name "manual" root))
	 (default-directory (progn (make-directory dest t)
				   (file-name-as-directory dest)))
	 (version (with-temp-buffer
		    (insert-file-contents "../doc/emacs/emacsver.texi")
		    (re-search-forward "@set EMACSVER \\([0-9.]+\\)")
		    (match-string 1)))
	 (stem (format "emacs-%s-%s" (if (equal type "emacs") "manual" type)
		       version))
	 (tarfile (format "%s.tar" stem)))
    (message "Doing %s..." type)
    (if (file-directory-p stem)
	(delete-directory stem t))
    (make-directory stem)
    (copy-file "../doc/misc/texinfo.tex" stem)
    (unless (equal type "emacs")
      (copy-file "../doc/emacs/emacsver.texi" stem)
      (copy-file "../doc/emacs/docstyle.texi" stem))
    (dolist (file (directory-files (format "../doc/%s" type) t))
      (if (or (string-match-p "\\(\\.texi\\'\\|/README\\'\\)" file)
	      (and (equal type "lispintro")
		   (string-match-p "\\.\\(eps\\|pdf\\)\\'" file)))
	  (copy-file file stem)))
    (with-temp-buffer
      (let ((outvars make-manuals-dist-output-variables))
	(push `("@version@" . ,version) outvars)
	(insert-file-contents (format "../doc/%s/Makefile.in" type))
	(dolist (cons outvars)
	  (while (re-search-forward (car cons) nil t)
	    (replace-match (cdr cons) t))
	  (goto-char (point-min))))
      (let (ats)
	(while (re-search-forward "@[a-zA-Z_]+@" nil t)
	  (setq ats t)
	  (message "Unexpanded: %s" (match-string 0)))
	(if ats (error "Unexpanded configure variables in Makefile?")))
      (write-region nil nil (expand-file-name (format "%s/Makefile" stem))
		    nil 'silent))
    (call-process "tar" nil nil nil "-cf" tarfile stem)
    (delete-directory stem t)
    (message "...created %s" tarfile)))

;; Does anyone actually use these tarfiles?
(defun make-manuals-dist (root &optional type)
  "Make the standalone manual source tarfiles for the Emacs webpage.
ROOT should be the root of an Emacs source tree.
Interactively with a prefix argument, prompt for TYPE.
Optional argument TYPE is type of output (nil means all)."
  (interactive (let ((root (read-directory-name "Emacs root directory: "
						source-directory nil t)))
		 (list root
		       (if current-prefix-arg
			   (completing-read
			    "Type: "
			    '("emacs" "lispref" "lispintro" "misc"))))))
  (unless (file-exists-p (expand-file-name "src/emacs.c" root))
    (user-error "%s doesn't seem to be the root of an Emacs source tree" root))
  (dolist (m '("emacs" "lispref" "lispintro" "misc"))
    (if (member type (list nil m))
	(make-manuals-dist--1 root m))))


;; Stuff to check new `defcustom's got :version tags.
;; Adapted from check-declare.el.

(defun cusver-find-files (root &optional old)
  "Find .el files beneath directory ROOT that contain `defcustom's.
If optional OLD is non-nil, also include `defvar's."
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
  "Version number that new `defcustom's should have.")

(defun cusver-scan (file &optional old)
  "Scan FILE for `defcustom' calls.
Return a list with elements of the form (VAR . VER),
This means that FILE contains a defcustom for variable VAR, with
a :version tag having value VER (may be nil).
If optional argument OLD is non-nil, also scan for `defvar's."
  (let ((m (format "Scanning %s..." file))
	(re (format "^[ \t]*\\((def%s\\)[ \t\n]"
		    (if old "\\(custom\\|var\\)" "\\(custom\\|group\\)")))
        alist var ver form glist grp)
    (message "%s" m)
    (with-temp-buffer
      (insert-file-contents file)
      ;; FIXME we could theoretically be inside a string.
      (while (re-search-forward re nil :noerror)
        (goto-char (match-beginning 1))
        (if (and (setq form (ignore-errors (read (current-buffer))))
		 (setq var (car-safe (cdr-safe form)))
		 ;; Exclude macros, eg (defcustom ,varname ...).
		 (symbolp var))
	    (progn
	      ;; FIXME It should be cus-test-apropos that does this.
	      (and (not old)
		   (equal "custom" (match-string 2))
		   (not (memq :type form))
		   (display-warning
                    'custom (format-message "Missing type in: `%s'" form)))
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
          (if form (format-message "Malformed defcustom: `%s'" form)))))
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
  "Jump to a Lisp file for the BUTTON at point."
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
  "Check that `defcustom's have :version tags where needed.
NEWDIR is the current lisp/ directory, OLDDIR is that from the
previous release, VERSION is the new version number.  A
`defcustom' that is only in NEWDIR should have a :version tag.
We exclude cases where a `defvar' exists in OLDDIR, since just
converting a `defvar' to a `defcustom' does not require
a :version bump.

Note that a :version tag should also be added if the value of a defcustom
changes (in a non-trivial way).  This function does not check for that."
  (interactive (list (read-directory-name "New Lisp directory: " nil nil t)
		     (read-directory-name "Old Lisp directory: " nil nil t)
		     (number-to-string
		      (read-number "New version number: "
				   (string-to-number cusver-new-version)))))
  (or (file-directory-p (setq newdir (expand-file-name newdir)))
      (user-error "Directory `%s' not found" newdir))
  (or (file-directory-p (setq olddir (expand-file-name olddir)))
      (user-error "Directory `%s' not found" olddir))
  (setq cusver-new-version version)
  (let* ((newfiles (progn (message "Finding new files with `defcustom's...")
			  (cusver-find-files newdir)))
	 (oldfiles (progn (message "Finding old files with `defcustom's...")
			  (cusver-find-files olddir t)))
	 (newcus (progn (message "Reading new `defcustom's...")
			(mapcar
			 (lambda (file)
			   (cons file (cusver-scan file))) newfiles)))
	 oldcus result thisfile file)
    (message "Reading old `defcustom's...")
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
      (insert (substitute-command-keys
               "These `defcustom's might be missing :version tags:\n\n"))
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

;; Local Variables:
;; coding: utf-8
;; End:
