;;; inc-vers.el --- load this to increment the recorded Emacs version number.

;; Copyright (C) 1985, 1986 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: internal

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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Code:

(insert-file-contents "../lisp/version.el")

(re-search-forward "emacs-version \"[^\"]*[0-9]+\"")
(forward-char -1)
(save-excursion
  (save-restriction
    (narrow-to-region (point)
		      (progn (skip-chars-backward "0-9") (point)))
    (goto-char (point-min))
    (let ((version (read (current-buffer))))
      (delete-region (point-min) (point-max))
      (prin1 (1+ version) (current-buffer)))))
(skip-chars-backward "^\"")
(message "New Emacs version will be %s"
	 (buffer-substring (point)
			   (progn (skip-chars-forward "^\"") (point))))


(if (and (file-accessible-directory-p "../lisp/")
	 (null (file-writable-p "../lisp/version.el")))
    (delete-file "../lisp/version.el"))
(write-region (point-min) (point-max) "../lisp/version.el" nil 'nomsg)
(erase-buffer)
(set-buffer-modified-p nil)

(kill-emacs)

;;; inc-vers.el ends here
