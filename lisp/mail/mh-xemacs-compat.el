;;; mh-xemacs-compat.el --- GNU Emacs Functions needed by XEmacs

;; Copyright (C) 2001, 2002 Free Software Foundation, Inc.

;; Author: FSF
;; Maintainer: Bill Wohler <wohler@newt.com>
;; Keywords: mail
;; See: mh-e.el

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

;;; Change Log:

;; $Id: mh-xemacs-compat.el,v 1.7 2002/04/07 19:20:55 wohler Exp $

;;; Code:

;;; Simple compatibility:

(unless (fboundp 'match-string-no-properties)
  (defalias 'match-string-no-properties 'match-string))

;;; Functions from simple.el of Emacs-21.1
;;; simple.el --- basic editing commands for Emacs

;; Copyright (C) 1985, 86, 87, 93, 94, 95, 96, 97, 98, 99, 2000, 2001
;;        Free Software Foundation, Inc.

(defun rfc822-goto-eoh ()
  ;; Go to header delimiter line in a mail message, following RFC822 rules
  (goto-char (point-min))
  (while (looking-at "^[^: \n]+:\\|^[ \t]")
    (forward-line 1))
  (point))

;;; Functions from sendmail.el of Emacs-21.1
;;; sendmail.el --- mail sending commands for Emacs.

;; Copyright (C) 1985, 86, 92, 93, 94, 95, 96, 98, 2000, 2001
;;   Free Software Foundation, Inc.

(defun mail-header-end ()
  "Return the buffer location of the end of headers, as a number."
  (save-restriction
    (widen)
    (save-excursion
      (rfc822-goto-eoh)
      (point))))

(defun mail-mode-fill-paragraph (arg)
  ;; Do something special only if within the headers.
  (if (< (point) (mail-header-end))
      (let (beg end fieldname)
	(when (prog1 (re-search-backward "^[-a-zA-Z]+:" nil 'yes)
		(setq beg (point)))
	(setq fieldname
		(downcase (buffer-substring beg (1- (match-end 0))))))
	(forward-line 1)
	;; Find continuation lines and get rid of their continuation markers.
	(while (looking-at "[ \t]")
	  (delete-horizontal-space)
	  (forward-line 1))
	(setq end (point-marker))
	(goto-char beg)
	;; If this field contains addresses,
	;; make sure we can fill after each address.
	(if (member fieldname
		    '("to" "cc" "bcc" "from" "reply-to"
		      "resent-to" "resent-cc" "resent-bcc"
		      "resent-from" "resent-reply-to"))
	    (while (search-forward "," end t)
	      (or (looking-at "[ \t]")
		  (insert " "))))
	(fill-region-as-paragraph beg end)
	;; Mark all lines except the first as continuations.
	(goto-char beg)
	(forward-line 1)
	(while (< (point) end)
	  (insert "  ")
	  (forward-line 1))
	(move-marker end nil)
	t)))

(provide 'mh-xemacs-compat)

;;; mh-xemacs-compat.el ends here
