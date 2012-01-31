;;; admin.el --- utilities for Emacs administration

;; Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009,
;;   2010, 2011, 2012  Free Software Foundation, Inc.

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
  (set-version-in-file root "lisp/version.el" version
		       (rx (and "emacs-version" (0+ space)
				?\" (submatch (1+ (not (in ?\")))) ?\")))
  (set-version-in-file root "README" version
		       (rx (and "version" (1+ space)
				(submatch (1+ (in "0-9."))))))
  (set-version-in-file root "configure.in" version
		       (rx (and "AC_INIT" (1+ (not (in ?,)))
                                ?, (0+ space)
                                (submatch (1+ (in "0-9."))))))
  (set-version-in-file root "doc/emacs/emacs.texi" version
		       (rx (and "EMACSVER" (1+ space)
				(submatch (1+ (in "0-9."))))))
  (set-version-in-file root "doc/lispref/elisp.texi" version
		       (rx (and "EMACSVER" (1+ space)
				(submatch (1+ (in "0-9."))))))
  (set-version-in-file root "doc/lispref/vol1.texi" version
		       (rx (and "EMACSVER" (1+ space)
				(submatch (1+ (in "0-9."))))))
  (set-version-in-file root "doc/lispref/vol2.texi" version
		       (rx (and "EMACSVER" (1+ space)
				(submatch (1+ (in "0-9."))))))
  (set-version-in-file root "doc/lispref/book-spine.texinfo" version
		       (rx (and "Emacs Version" (1+ space)
				(submatch (1+ (in "0-9."))))))
  (set-version-in-file root "doc/man/emacs.1" version
		       (rx (and ".TH EMACS" (1+ not-newline)
                                "GNU Emacs" (1+ space)
				(submatch (1+ (in "0-9."))))))
  (set-version-in-file root "doc/misc/faq.texi" version
		       (rx (and "VER" (1+ space)
				(submatch (1+ (in "0-9."))))))
  (set-version-in-file root "lib-src/makefile.w32-in" version
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
				  "\\0\""))))
  ;; nextstep.
  (set-version-in-file
   root "nextstep/Cocoa/Emacs.base/Contents/Info.plist"
   version (rx (and "CFBundleGetInfoString" (1+ anything) "Emacs" (1+ space)
                    (submatch (1+ (in "0-9."))))))
  (set-version-in-file
   root "nextstep/Cocoa/Emacs.base/Contents/Info.plist"
   version (rx (and "CFBundleShortVersionString" (1+ not-newline) ?\n
                    (0+ not-newline) "<string>" (0+ space)
                    (submatch (1+ (in "0-9."))))))
  (set-version-in-file
   root "nextstep/Cocoa/Emacs.base/Contents/Resources/English.lproj/InfoPlist.strings"
   version (rx (and "CFBundleShortVersionString" (0+ space) ?= (0+ space)
                    ?\" (0+ space) "Version" (1+ space)
                    (submatch (1+ (in "0-9."))))))
  (set-version-in-file
   root "nextstep/Cocoa/Emacs.base/Contents/Resources/English.lproj/InfoPlist.strings"
   version (rx (and "CFBundleGetInfoString" (0+ space) ?= (0+ space)
                    ?\" (0+ space) "Emacs version" (1+ space)
                    (submatch (1+ (in "0-9."))))))
  (set-version-in-file
   root "nextstep/GNUstep/Emacs.base/Resources/Info-gnustep.plist"
   version (rx (and "ApplicationRelease" (0+ space) ?= (0+ space)
                    ?\" (0+ space) (submatch (1+ (in "0-9."))))))
  (set-version-in-file
   root "nextstep/GNUstep/Emacs.base/Resources/Info-gnustep.plist"
   version (rx (and "FullVersionID" (0+ space) ?= (0+ space)
                    ?\" (0+ space) "Emacs" (1+ space)
                    (submatch (1+ (in "0-9."))))))
  (set-version-in-file
   root "nextstep/GNUstep/Emacs.base/Resources/Emacs.desktop"
   version (rx (and "Version=" (submatch (1+ (in "0-9.")))))))

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
  (set-version-in-file root "lisp/version.el" copyright
        	       (rx (and "emacs-copyright" (0+ space)
        			?\" (submatch (1+ (not (in ?\")))) ?\")))
  (set-version-in-file root "lib-src/ebrowse.c" copyright
                       (rx (and "emacs_copyright" (0+ (not (in ?\")))
        			?\" (submatch (1+ (not (in ?\")))) ?\")))
  (set-version-in-file root "lib-src/etags.c" copyright
                       (rx (and "emacs_copyright" (0+ (not (in ?\")))
        			?\" (submatch (1+ (not (in ?\")))) ?\")))
  (set-version-in-file root "lib-src/rcs2log" copyright
        	       (rx (and "Copyright" (0+ space) ?= (0+ space)
        			?\' (submatch (1+ nonl)))))
  ;; This one is a nuisance, as it needs to be split over two lines.
  (string-match "\\(.*[0-9]\\{4\\} *\\)\\(.*\\)" copyright)
  ;; nextstep.
  (set-version-in-file
   root "nextstep/Cocoa/Emacs.base/Contents/Info.plist"
   copyright (rx (and "CFBundleGetInfoString" (1+ anything) "Emacs" (1+ space)
                    (1+ (in "0-9.")) (1+ space)
                    (submatch (1+ (not (in ?\<)))))))
  (set-version-in-file
   root "nextstep/Cocoa/Emacs.base/Contents/Resources/English.lproj/InfoPlist.strings"
   copyright (rx (and "NSHumanReadableCopyright" (0+ space) ?\= (0+ space)
                    ?\" (submatch (1+ (not (in ?\")))))))
  (set-version-in-file
   root "nextstep/GNUstep/Emacs.base/Resources/Info-gnustep.plist"
   copyright (rx (and "Copyright" (0+ space) ?\= (0+ space)
                      ?\" (submatch (1+ (not (in ?\")))))))
  (when (string-match "\\([0-9]\\{4\\}\\)" copyright)
    (setq copyright (match-string 1 copyright))
    (dolist (file (directory-files (expand-file-name "etc/refcards" root)
                                   t "\\.tex\\'"))
      (unless (string-match "gnus-refcard\\.tex" file)
        (set-version-in-file
         root file copyright
         (concat (if (string-match "ru-refcard\\.tex" file)
                     "\\\\newcommand{\\\\cyear}\\[0\\]{"
                   "\\\\def\\\\year{")
                 "\\([0-9]\\{4\\}\\)}.+%.+copyright year"))))))

(provide 'admin)

;;; admin.el ends here
