;;; compface.el --- functions for converting X-Face headers

;; Copyright (C) 2002, 2003, 2004, 2005, 2006, 2007, 2008 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: news

;; This file is part of GNU Emacs.

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

;;; Code:

;;;###
(defun uncompface (face)
  "Convert FACE to pbm.
Requires the external programs `uncompface', and `icontopbm'.  On a
GNU/Linux system these might be in packages with names like `compface'
or `faces-xface' and `netpbm' or `libgr-progs', for instance."
  (with-temp-buffer
    (unless (featurep 'xemacs) (set-buffer-multibyte nil))
    (insert face)
    (let ((coding-system-for-read 'raw-text)
	  ;; At least "icontopbm" doesn't work with Windows because
	  ;; the line-break code is converted into CRLF by default.
	  (coding-system-for-write 'binary))
      (and (eq 0 (apply 'call-process-region (point-min) (point-max)
			"uncompface"
			'delete '(t nil) nil))
	   (progn
	     (goto-char (point-min))
	     (insert "/* Width=48, Height=48 */\n")
	     ;; I just can't get "icontopbm" to work correctly on its
	     ;; own in XEmacs.  And Emacs doesn't understand un-raw pbm
	     ;; files.
	     (if (not (featurep 'xemacs))
		 (eq 0 (call-process-region (point-min) (point-max)
					    "icontopbm"
					    'delete '(t nil)))
	       (shell-command-on-region (point-min) (point-max)
					"icontopbm | pnmnoraw"
					(current-buffer) t)
	       t))
	   (buffer-string)))))

(provide 'compface)

;; arch-tag: f9c78e84-98c0-4142-9682-8ba4cf4c3441
;;; compface.el ends here
