;;; setaddr.el --- determine whether sendmail is configured on this machine

;; Copyright (C) 1997 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: mail

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

;; If neither sendmail nor Emacs knows what host address to use
;; for this machine, ask for it, and save it in site-start.el
;; so we won't have to ask again.

;; This uses a heuristic about the output from sendmail
;; which may or may not really work.  We will have to find
;; out by experiment.

;;; Code:

(or mail-host-address
    (let (sendmail-configured)
      (with-temp-buffer " mail-host-address"
	(call-process sendmail-program nil t nil "-bv" "root")
	(goto-char (point-min))
	(setq sendmail-configured (looking-at "root@")))
      (or sendmail-configured
	  (let (buffer)
		(setq mail-host-address
		      (read-string "Specify your host's fully qualified domain name: ")))
	  ;; Create an init file, and if we just read mail-host-address,
	  ;; make the init file set it.
	  (unwind-protect
	      (save-excursion
		(set-buffer (find-file-noselect "site-start.el"))
		(setq buffer (current-buffer))
		;; Get rid of the line that ran this file.
		(if (search-forward "(load \"setaddr\")\n")
		    (progn
		      (beginning-of-line)
		      (delete-region (point)
				     (progn (end-of-line)
					    (point)))))
		;; Add the results
		(goto-char (point-max))
		(insert "\n(setq mail-host-address "
			(prin1-to-string mail-host-address)
			")\n")
		(condition-case nil
		    (save-buffer)
		  (file-error nil)))
	    (if buffer
		(kill-buffer buffer))))))

;;; setaddr.el ends here
