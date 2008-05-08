;;; epa-hook.el --- preloaded code to enable epa-file.el
;; Copyright (C) 2006, 2007, 2008 Free Software Foundation, Inc.

;; Author: Daiki Ueno <ueno@unixuser.org>
;; Keywords: PGP, GnuPG

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

;;; Code:

(defgroup epa-file nil
  "The EasyPG Assistant hooks for transparent file encryption"
  :version "23.1"
  :group 'epa)

(defun epa-file--file-name-regexp-set (variable value)
  (set-default variable value)
  (if (fboundp 'epa-file-name-regexp-update)
      (epa-file-name-regexp-update)))

(defcustom epa-file-name-regexp "\\.gpg\\(~\\|\\.~[0-9]+~\\)?\\'"
  "Regexp which matches filenames to be encrypted with GnuPG.

If you set this outside Custom while epa-file is already enabled, you
have to call `epa-file-name-regexp-update' after setting it to
properly update file-name-handler-alist.  Setting this through Custom
does that automatically."
  :type 'regexp
  :group 'epa-file
  :set 'epa-file--file-name-regexp-set)

(defcustom epa-file-inhibit-auto-save t
  "If non-nil, disable auto-saving when opening an encrypted file."
  :type 'boolean
  :group 'epa-file)

(defvar epa-file-encrypt-to nil
  "*Recipient(s) used for encrypting files.
May either be a string or a list of strings.")

(put 'epa-file-encrypt-to 'safe-local-variable
     (lambda (val)
       (or (stringp val)
	   (and (listp val)
		(catch 'safe
		  (mapc (lambda (elt)
			  (unless (stringp elt)
			    (throw 'safe nil)))
			val)
		  t)))))

(put 'epa-file-encrypt-to 'permanent-local t)

(defvar epa-file-handler
  (cons epa-file-name-regexp 'epa-file-handler))

(defvar epa-file-auto-mode-alist-entry
  (list epa-file-name-regexp nil 'epa-file))

(defun epa-file-name-regexp-update ()
  (interactive)
  (unless (equal (car epa-file-handler) epa-file-name-regexp)
    (setcar epa-file-handler epa-file-name-regexp)))

(defun epa-file-find-file-hook ()
  (if (and buffer-file-name
	   (string-match epa-file-name-regexp buffer-file-name)
	   epa-file-inhibit-auto-save)
      (auto-save-mode 0))
  (set-buffer-modified-p nil))

(define-minor-mode auto-encryption-mode
  "Toggle automatic file encryption and decryption.
With prefix argument ARG, turn auto encryption on if positive, else off.
Return the new status of auto encryption (non-nil means on)."
  :global t :init-value t :group 'epa-file :version "23.1"
  (setq file-name-handler-alist
	(delq epa-file-handler file-name-handler-alist))
  (remove-hook 'find-file-hooks 'epa-file-find-file-hook)
  (setq auto-mode-alist (delq epa-file-auto-mode-alist-entry
			      auto-mode-alist))
  (when auto-encryption-mode
    (setq file-name-handler-alist
	  (cons epa-file-handler file-name-handler-alist))
    (add-hook 'find-file-hook 'epa-file-find-file-hook)
    (setq auto-mode-alist (cons epa-file-auto-mode-alist-entry
				auto-mode-alist))))

(put 'epa-file-handler 'safe-magic t)
(put 'epa-file-handler 'operations '(write-region insert-file-contents))

(provide 'epa-hook)

;; arch-tag: f75c8a50-d32e-4eb3-9ec6-9e940c1fc8b5
;;; epa-hook.el ends here
