;;; gnus-setup.el --- Initialization & Setup for Gnus 5
;; Copyright (C) 1995, 96 Free Software Foundation, Inc.

;; Author: Steven L. Baur <steve@miranova.com>
;; Keywords: news

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

;;; Commentary:
;; My head is starting to spin with all the different mail/news packages.
;; Stop The Madness!

;; Given that Emacs Lisp byte codes may be diverging, it is probably best
;; not to byte compile this, and just arrange to have the .el loaded out
;; of .emacs.

;;; Code:

(defvar running-xemacs (string-match "XEmacs\\|Lucid" emacs-version))

(defvar gnus-emacs-lisp-directory (if running-xemacs
				      "/usr/local/lib/xemacs/"
				    "/usr/local/share/emacs/")
  "Directory where Emacs site lisp is located.")

(defvar gnus-gnus-lisp-directory (concat gnus-emacs-lisp-directory
					 "gnus-5.0.15/lisp/")
  "Directory where Gnus Emacs lisp is found.")

(defvar gnus-sgnus-lisp-directory (concat gnus-emacs-lisp-directory
					  "sgnus/lisp/")
  "Directory where September Gnus Emacs lisp is found.")

(defvar gnus-tm-lisp-directory (concat gnus-emacs-lisp-directory
				       "site-lisp/")
  "Directory where TM Emacs lisp is found.")

(defvar gnus-mailcrypt-lisp-directory (concat gnus-emacs-lisp-directory
					      "site-lisp/mailcrypt-3.4/")
  "Directory where Mailcrypt Emacs Lisp is found.")

(defvar gnus-bbdb-lisp-directory (concat gnus-emacs-lisp-directory
					 "site-lisp/bbdb-1.50/")
  "Directory where Big Brother Database is found.")

(defvar gnus-use-tm t
  "Set this if you want MIME support for Gnus")
(defvar gnus-use-mhe nil
  "Set this if you want to use MH-E for mail reading")
(defvar gnus-use-rmail nil
  "Set this if you want to use RMAIL for mail reading")
(defvar gnus-use-sendmail t
  "Set this if you want to use SENDMAIL for mail reading")
(defvar gnus-use-vm nil
  "Set this if you want to use the VM package for mail reading")
(defvar gnus-use-sc t
  "Set this if you want to use Supercite")
(defvar gnus-use-mailcrypt t
  "Set this if you want to use Mailcrypt for dealing with PGP messages")
(defvar gnus-use-bbdb nil
  "Set this if you want to use the Big Brother DataBase")
(defvar gnus-use-september nil
  "Set this if you are using the experimental September Gnus")

(let ((gnus-directory (if gnus-use-september
			  gnus-sgnus-lisp-directory
			gnus-gnus-lisp-directory)))
  (if (null (member gnus-directory load-path))
      (setq load-path (cons gnus-directory load-path))))

;;; Tools for MIME by
;;; UMEDA Masanobu <umerin@mse.kyutech.ac.jp>
;;; MORIOKA Tomohiko <morioka@jaist.ac.jp>

(if gnus-use-tm
    (progn
      (if (null (member gnus-tm-lisp-directory load-path))
 	  (setq load-path (cons gnus-tm-lisp-directory load-path)))
       (load "mime-setup")))

;;; Mailcrypt by
;;; Jin Choi <jin@atype.com>
;;; Patrick LoPresti <patl@lcs.mit.edu>

(if gnus-use-mailcrypt
    (progn
      (if (null (member gnus-mailcrypt-lisp-directory load-path))
 	  (setq load-path (cons gnus-mailcrypt-lisp-directory load-path)))
      (autoload 'mc-install-write-mode "mailcrypt" nil t)
      (autoload 'mc-install-read-mode "mailcrypt" nil t)
      (add-hook 'message-mode-hook 'mc-install-write-mode)
      (add-hook 'gnus-summary-mode-hook 'mc-install-read-mode)
      (if gnus-use-mhe
	  (progn
	    (add-hook 'mh-folder-mode-hook 'mc-install-read-mode)
 	    (add-hook 'mh-letter-mode-hook 'mc-install-write-mode)))))

;;; BBDB by
;;; Jamie Zawinski <jwz@lucid.com>

(if gnus-use-bbdb
    (progn
      (if (null (member gnus-bbdb-lisp-directory load-path))
 	  (setq load-path (cons gnus-bbdb-lisp-directory load-path)))
      (autoload 'bbdb "bbdb-com"
	"Insidious Big Brother Database" t)
      (autoload 'bbdb-name "bbdb-com"
	"Insidious Big Brother Database" t)
      (autoload 'bbdb-company "bbdb-com"
	"Insidious Big Brother Database" t)
      (autoload 'bbdb-net "bbdb-com"
	"Insidious Big Brother Database" t)
      (autoload 'bbdb-notes "bbdb-com"
	"Insidious Big Brother Database" t)

      (if gnus-use-vm
	  (progn
	    (autoload 'bbdb-insinuate-vm "bbdb-vm"
	      "Hook BBDB into VM" t)))

      (if gnus-use-rmail
	  (progn
	    (autoload 'bbdb-insinuate-rmail "bbdb-rmail"
	      "Hook BBDB into RMAIL" t)
	    (add-hook 'rmail-mode-hook 'bbdb-insinuate-rmail)))

      (if gnus-use-mhe
	  (progn
	    (autoload 'bbdb-insinuate-mh "bbdb-mh"
	      "Hook BBDB into MH-E" t)
	    (add-hook 'mh-folder-mode-hook 'bbdb-insinuate-mh)))

      (autoload 'bbdb-insinuate-gnus "bbdb-gnus"
	"Hook BBDB into Gnus" t)
      (add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)

      (if gnus-use-sendmail
	  (progn
	    (autoload 'bbdb-insinuate-sendmail "bbdb"
	      "Insidious Big Brother Database" t)
	    (add-hook 'mail-setup-hook 'bbdb-insinuate-sendmail)
	    (add-hook 'message-setup-hook 'bbdb-insinuate-sendmail)))))

(if gnus-use-sc
    (progn
      (add-hook 'mail-citation-hook 'sc-cite-original)
      (setq message-cite-function 'sc-cite-original)
      (autoload 'sc-cite-original "supercite")))

;;;### (autoloads (gnus-batch-score gnus-fetch-group gnus gnus-slave gnus-no-server gnus-update-format) "gnus" "lisp/gnus.el" (12473 2137))
;;; Generated autoloads from lisp/gnus.el

(autoload 'gnus-update-format "gnus" "\
Update the format specification near point." t nil)

(autoload 'gnus-slave-no-server "gnus" "\
Read network news as a slave without connecting to local server." t nil)

(autoload 'gnus-no-server "gnus" "\
Read network news.
If ARG is a positive number, Gnus will use that as the
startup level.  If ARG is nil, Gnus will be started at level 2. 
If ARG is non-nil and not a positive number, Gnus will
prompt the user for the name of an NNTP server to use.
As opposed to `gnus', this command will not connect to the local server." t nil)

(autoload 'gnus-slave "gnus" "\
Read news as a slave." t nil)

(autoload 'gnus "gnus" "\
Read network news.
If ARG is non-nil and a positive number, Gnus will use that as the
startup level.  If ARG is non-nil and not a positive number, Gnus will
prompt the user for the name of an NNTP server to use." t nil)

(autoload 'gnus-fetch-group "gnus" "\
Start Gnus if necessary and enter GROUP.
Returns whether the fetching was successful or not." t nil)

(defalias 'gnus-batch-kill 'gnus-batch-score)

(autoload 'gnus-batch-score "gnus" "\
Run batched scoring.
Usage: emacs -batch -l gnus -f gnus-batch-score <newsgroups> ...
Newsgroups is a list of strings in Bnews format.  If you want to score
the comp hierarchy, you'd say \"comp.all\".  If you would not like to
score the alt hierarchy, you'd say \"!alt.all\"." t nil)

;;;***

(provide 'gnus-setup)

(run-hooks 'gnus-setup-load-hook)

;;; gnus-setup.el ends here
