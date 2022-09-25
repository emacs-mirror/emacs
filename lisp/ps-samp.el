;;; ps-samp.el --- ps-print sample setup code  -*- lexical-binding: t -*-

;; Copyright (C) 2007-2022 Free Software Foundation, Inc.

;; Author: Jim Thompson (was <thompson@wg2.waii.com>)
;;	Jacques Duthen (was <duthen@cegelec-red.fr>)
;;	Vinicius Jose Latorre <viniciusjl.gnu@gmail.com>
;;	Kenichi Handa <handa@gnu.org> (multi-byte characters)
;; Maintainer: Vinicius Jose Latorre <viniciusjl.gnu@gmail.com>
;; Keywords: wp, print, PostScript
;; URL: https://www.emacswiki.org/cgi-bin/wiki/ViniciusJoseLatorre
;; Package: ps-print

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

;; Some example hacks for ps-print.el.
;; This stuff is for anybody that's brave enough to look this far,
;; and able to figure out how to use it.  It isn't really part of
;; ps-print, but I'll leave it here in hopes it might be useful:

;; WARNING!!! The following code is *sample* code only.
;; Don't use it unless you understand what it does!

;;; Code:

(require 'ps-print)



;; A hook to bind to `rmail-mode-hook' to locally bind prsc and set
;; `ps-left-header' specially for mail messages.
(defun ps-rmail-mode-hook ()
  (local-set-key [print] 'ps-rmail-print-message-from-summary)
  (setq-local ps-header-lines 3)
  ;; The left header will display the message's subject, its
  ;; author, and the name of the folder it was in.
  (setq-local ps-left-header
	      '(ps-article-subject ps-article-author buffer-name)))

;; Like `ps-gnus-print-article-from-summary', but for rmail.
(defun ps-rmail-print-message-from-summary ()
  (interactive)
  (ps-print-message-from-summary 'rmail-summary-buffer "RMAIL"))

;; Used in `ps-rmail-print-message-from-summary',
;; `ps-gnus-print-article-from-summary' and `ps-vm-print-message-from-summary'.
(defun ps-print-message-from-summary (summary-buffer summary-default)
  (let ((ps-buf (or (and (boundp summary-buffer)
			 (symbol-value summary-buffer))
		    summary-default)))
    (and (get-buffer ps-buf)
	 (with-current-buffer ps-buf
	   (ps-spool-buffer-with-faces)))))

;; Look in an article or mail message for the Subject: line.
(defun ps-article-subject ()
  (save-excursion
    (save-restriction
      (narrow-to-region (point-min) (progn (rfc822-goto-eoh) (point)))
      (concat "Subject: " (or (mail-fetch-field "Subject") "???")))))

;; Look in an article or mail message for the From: line.  Sorta-kinda
;; understands RFC 822 (or later) addresses and can pull the real name
;; out where it's provided.
(defun ps-article-author ()
  (save-excursion
    (save-restriction
      (narrow-to-region (point-min) (progn (rfc822-goto-eoh) (point)))
      (let ((fromstring (mail-fetch-field "From")))
	(cond
	 ;; Try first to match addresses that look like
	 ;; thompson@wg2.waii.com (Jim Thompson)
	 ((and fromstring (string-match ".*[ \t]+(\\(.*\\))" fromstring))
	  (match-string 1 fromstring))
	 ;; Next try to match addresses that look like
	 ;; Jim Thompson <thompson@wg2.waii.com> or
	 ;; "Jim Thompson" <thompson@wg2.waii.com>
	 ((and fromstring
	       (string-match "\\(\"?\\)\\(.*\\)\\1[ \t]+<.*>" fromstring))
	  (match-string 2 fromstring))
	 ;; Couldn't find a real name -- show the address instead.
	 (fromstring)
	 (t "From ???"))))))

;; A hook to bind to `gnus-article-prepare-hook'.  This will set
;; `ps-left-header' specially for gnus articles.  Unfortunately,
;; `gnus-article-mode-hook' is called only once, the first time the *Article*
;; buffer enters that mode, so it would only work for the first time
;; we ran gnus.  The second time, this hook wouldn't get set up.  The
;; only alternative is `gnus-article-prepare-hook'.
(defun ps-gnus-article-prepare-hook ()
  (setq-local ps-header-lines 3)
  ;; The left headers will display the article's subject, its
  ;; author, and the newsgroup it was in.
  (setq-local ps-left-header
	      '(ps-article-subject ps-article-author gnus-newsgroup-name)))

;; A hook to bind to `vm-mode-hook' to locally bind prsc and set
;; `ps-left-header' specially for mail messages.
(defun ps-vm-mode-hook ()
  (local-set-key [print] 'ps-vm-print-message-from-summary)
  (setq-local ps-header-lines 3)
  ;; The left headers will display the message's subject, its
  ;; author, and the name of the folder it was in.
  (setq-local ps-left-header
	'(ps-article-subject ps-article-author buffer-name)))

;; Every now and then I forget to switch from the *Summary* buffer to
;; the *Article* before hitting prsc, and a nicely formatted list of
;; article subjects shows up at the printer.  This function, bound to
;; prsc for the gnus *Summary* buffer means I don't have to switch
;; buffers first.
(defun ps-gnus-print-article-from-summary ()
  (interactive)
  (ps-print-message-from-summary 'gnus-article-buffer "*Article*"))

;; Like `ps-gnus-print-article-from-summary', but for vm.
(defun ps-vm-print-message-from-summary ()
  (interactive)
  (ps-print-message-from-summary 'vm-mail-buffer ""))

;; A hook to bind to `gnus-summary-setup-buffer' to locally bind prsc.
(defun ps-gnus-summary-setup ()
  (local-set-key [print] 'ps-gnus-print-article-from-summary))

(defun ps-info-file ()
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "File:[ \t]+\\([^, \t\n]*\\)" nil t)
	(match-string 1)
      "File ???")))

(defun ps-info-node ()
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "Node:[ \t]+\\([^,\t\n]*\\)" nil t)
	(match-string 1)
      "Node ???")))

(defun ps-info-mode-hook ()
  ;; The left headers will display the node name and file name.
  (setq-local ps-left-header '(ps-info-node ps-info-file)))

;; WARNING! The following function is a *sample* only, and is *not* meant
;; to be used as a whole unless you understand what the effects will be!
(defun ps-samp-ps-setup ()
  (global-set-key [print] 'ps-spool-buffer-with-faces)
  (global-set-key [S-print] 'ps-spool-region-with-faces)
  (global-set-key [C-print] 'ps-despool)
  (add-hook 'gnus-article-prepare-hook 'ps-gnus-article-prepare-hook)
  (add-hook 'gnus-summary-mode-hook 'ps-gnus-summary-setup)
  (add-hook 'vm-mode-hook 'ps-vm-mode-hook)
  (add-hook 'vm-mode-hooks 'ps-vm-mode-hook)
  (add-hook 'Info-mode-hook 'ps-info-mode-hook)
  (setq ps-spool-duplex t
	ps-print-color-p nil
	ps-lpr-command "lpr"
	ps-lpr-switches '("-Jjct,duplex_long")
	ps-paper-type        'a4
	ps-landscape-mode    t
	ps-number-of-columns 2
	ps-left-margin   (/ (* 72  1.0) 2.54) ;  1.0 cm
	ps-right-margin  (/ (* 72  1.0) 2.54) ;  1.0 cm
	ps-inter-column  (/ (* 72  1.0) 2.54) ;  1.0 cm
	ps-bottom-margin (/ (* 72  1.5) 2.54) ;  1.5 cm
	ps-top-margin    (/ (* 72  1.5) 2.54) ;  1.5 cm
	ps-header-offset (/ (* 72  1.0) 2.54) ;  1.0 cm
	ps-header-line-pad    .15
	ps-print-header       t
	ps-print-header-frame t
	ps-header-lines       2
	ps-show-n-of-n        t
	ps-spool-duplex       nil
	ps-font-family             'Courier
	ps-font-size               5.5
	ps-header-font-family      'Helvetica
	ps-header-font-size        6
	ps-header-title-font-size  8))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; If zeroconf is enabled, all CUPS printers can be detected.  The
;; "PostScript printer" menu will be modified dynamically, as printers
;; are added or removed.

;; Preconditions:
;;
;; * Emacs has D-Bus support enabled.  That is, D-Bus is installed on
;;   the system, and Emacs has been configured and built with the
;;   --with-dbus option.
;;
;; * The zeroconf daemon avahi-daemon is running.
;;
;; * CUPS has enabled the option "Share published printers connected
;;   to this system" (see <http://localhost:631/admin>).


(require 'printing)
(require 'zeroconf)

;; Add a PostScript printer to the "PostScript printer" menu.
(defun ps-add-printer (service)
  (let ((name (zeroconf-service-name service))
	(text (zeroconf-service-txt service))
	(addr (zeroconf-service-address service))
	(port (zeroconf-service-port service))
	is-ps cups-queue)
    ;; `text' is an array of key=value strings like ("Duplex=T" "Copies=T").
    (dolist (string text)
      (let ((split (split-string string "=" t)))
	;; If it is a PostScript printer, there must be a string like
	;; "pdl=application/postscript,application/vnd.hp-PCL,...".
	(when (and (string-equal "pdl" (car split))
		   (string-match "application/postscript" (cadr split)))
	  (setq is-ps t))
	;; A CUPS printer queue is coded as "rp=printers/<name>".
	(when (and (string-equal "rp" (car split))
		   (string-match "printers/\\(.+\\)" (cadr split)))
	  (setq cups-queue (match-string 1 (cadr split))))))
    ;; Add the printer.
    (when is-ps
      (if cups-queue
	  (add-to-list
	   'pr-ps-printer-alist (list (intern name) "lpr" nil "-P" cups-queue))
	;; No CUPS printer, but a network printer.
	(add-to-list
	 'pr-ps-printer-alist (list (intern name) "cupsdoprint"
				    '("-P" "default")
				    "-H" (format "%s:%s" addr port))))
      (pr-update-menus t))))

;; Remove a printer from the "PostScript printer" menu.
(defun ps-remove-printer (service)
  (setq pr-ps-printer-alist
	(delete (assoc (intern (zeroconf-service-name service))
		       pr-ps-printer-alist)
		pr-ps-printer-alist))
  (pr-update-menus t))

;; Activate the functions in zeroconf.
(defun ps-make-dynamic-printer-menu ()
  (when (featurep 'dbusbind)
    (zeroconf-init)
    (zeroconf-service-add-hook "_ipp._tcp" :new 'ps-add-printer)
    (zeroconf-service-add-hook "_ipp._tcp" :removed 'ps-remove-printer)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'ps-samp)

;;; ps-samp.el ends here
