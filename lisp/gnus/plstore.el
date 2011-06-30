;;; plstore.el --- searchable, partially encrypted, persistent plist store -*- lexical-binding: t -*-
;; Copyright (C) 2011 Free Software Foundation, Inc.

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

;;; Commentary

;; Creating:
;;
;; (setq store (plstore-open (expand-file-name "~/.emacs.d/auth.plist")))
;; (plstore-put store "foo" '(:host "foo.example.org" :port 80) nil)
;; (plstore-save store)
;; ;; :user property is secret
;; (plstore-put store "bar" '(:host "bar.example.org") '(:user "test"))
;; (plstore-put store "baz" '(:host "baz.example.org") '(:user "test"))
;; (plstore-save store) ;<= will ask passphrase via GPG
;; (plstore-close store)
;;
;; Searching:
;;
;; (setq store (plstore-open (expand-file-name "~/.emacs.d/auth.plist")))
;; (plstore-find store '(:host ("foo.example.org")))
;; (plstore-find store '(:host ("bar.example.org"))) ;<= will ask passphrase via GPG
;; (plstore-close store)
;;

;;; Code:

(require 'epg)

(defvar plstore-cache-passphrase-for-symmetric-encryption nil)
(defvar plstore-passphrase-alist nil)

(defun plstore-passphrase-callback-function (_context _key-id plstore)
  (if plstore-cache-passphrase-for-symmetric-encryption
      (let* ((file (file-truename (plstore--get-buffer plstore)))
	     (entry (assoc file plstore-passphrase-alist))
	     passphrase)
	(or (copy-sequence (cdr entry))
	    (progn
	      (unless entry
		(setq entry (list file)
		      plstore-passphrase-alist
		      (cons entry
			    plstore-passphrase-alist)))
	      (setq passphrase
		    (read-passwd (format "Passphrase for PLSTORE %s: "
					 (plstore--get-buffer plstore))))
	      (setcdr entry (copy-sequence passphrase))
	      passphrase)))
    (read-passwd (format "Passphrase for PLSTORE %s: "
			 (plstore--get-buffer plstore)))))

(defun plstore-progress-callback-function (_context _what _char current total
						    handback)
  (if (= current total)
      (message "%s...done" handback)
    (message "%s...%d%%" handback
	     (if (> total 0) (floor (* (/ current (float total)) 100)) 0))))

(defun plstore--get-buffer (this)
  (aref this 0))

(defun plstore--get-alist (this)
  (aref this 1))

(defun plstore--get-encrypted-data (this)
  (aref this 2))

(defun plstore--get-secret-alist (this)
  (aref this 3))

(defun plstore--get-merged-alist (this)
  (aref this 4))

(defun plstore--set-file (this file)
  (aset this 0 file))

(defun plstore--set-alist (this plist)
  (aset this 1 plist))

(defun plstore--set-encrypted-data (this encrypted-data)
  (aset this 2 encrypted-data))

(defun plstore--set-secret-alist (this secret-alist)
  (aset this 3 secret-alist))

(defun plstore--set-merged-alist (this merged-alist)
  (aset this 4 merged-alist))

(defun plstore-get-file (this)
  (buffer-file-name (plstore--get-buffer this)))

;;;###autoload
(defun plstore-open (file)
  "Create a plstore instance associated with FILE."
  (let ((store (vector
		(find-file-noselect file)
		nil		     ;plist (plist)
		nil		     ;encrypted data (string)
		nil		     ;secret plist (plist)
		nil		     ;merged plist (plist)
		)))
    (with-current-buffer (plstore--get-buffer store)
      (goto-char (point-min))
      (when (looking-at ";;; public entries\n")
	(forward-line)
	(plstore--set-alist store (read (point-marker)))
	(forward-sexp)
	(forward-char)
	(when (looking-at ";;; secret entries\n")
	  (forward-line)
	  (plstore--set-encrypted-data store (read (point-marker))))
	(plstore--merge-secret store)))
    store))

(defun plstore-close (plstore)
  "Destroy a plstore instance PLSTORE."
  (kill-buffer (plstore--get-buffer plstore)))

(defun plstore--merge-secret (plstore)
  (let ((alist (plstore--get-secret-alist plstore))
	modified-alist
	modified-plist
	modified-entry
	entry
	plist
	placeholder)
    (plstore--set-merged-alist
     plstore
     (copy-tree (plstore--get-alist plstore)))
    (setq modified-alist (plstore--get-merged-alist plstore))
    (while alist
      (setq entry (car alist)
	    alist (cdr alist)
	    plist (cdr entry)
	    modified-entry (assoc (car entry) modified-alist)
	    modified-plist (cdr modified-entry))
      (while plist
	(setq placeholder
	      (plist-member
	       modified-plist
	       (intern (concat ":secret-"
			       (substring (symbol-name (car plist)) 1)))))
	(if placeholder
	    (setcar placeholder (car plist)))
	(setq modified-plist
	      (plist-put modified-plist (car plist) (car (cdr plist))))
	(setq plist (nthcdr 2 plist)))
      (setcdr modified-entry modified-plist))))

(defun plstore--decrypt (plstore)
  (if (plstore--get-encrypted-data plstore)
      (let ((context (epg-make-context 'OpenPGP))
	    plain)
	(epg-context-set-passphrase-callback
	 context
	 (cons #'plstore-passphrase-callback-function
	       plstore))
	(epg-context-set-progress-callback
	 context
	 (cons #'plstore-progress-callback-function
	       (format "Decrypting %s" (plstore-get-file plstore))))
	(setq plain
	      (epg-decrypt-string context
				  (plstore--get-encrypted-data plstore)))
	(plstore--set-secret-alist plstore (car (read-from-string plain)))
	(plstore--merge-secret plstore)
	(plstore--set-encrypted-data plstore nil))))

(defun plstore--match (entry keys skip-if-secret-found)
  (let ((result t) key-name key-value prop-value secret-name)
    (while keys
      (setq key-name (car keys)
	    key-value (car (cdr keys))
	    prop-value (plist-get (cdr entry) key-name))
	(unless (member prop-value key-value)
	  (if skip-if-secret-found
	      (progn
		(setq secret-name
		      (intern (concat ":secret-"
				      (substring (symbol-name key-name) 1))))
		(if (plist-member (cdr entry) secret-name)
		    (setq result 'secret)
		  (setq result nil
			keys nil)))
	    (setq result nil
		  keys nil)))
	(setq keys (nthcdr 2 keys)))
    result))

(defun plstore-find (plstore keys)
  "Perform search on PLSTORE with KEYS.
KEYS is a plist."
  (let (entries alist entry match decrypt plist)
    ;; First, go through the merged plist alist and collect entries
    ;; matched with keys.
    (setq alist (plstore--get-merged-alist plstore))
    (while alist
      (setq entry (car alist)
	    alist (cdr alist)
	    match (plstore--match entry keys t))
      (if (eq match 'secret)
	  (setq decrypt t)
	(when match
	  (setq plist (cdr entry))
	  (while plist
	    (if (string-match "\\`:secret-" (symbol-name (car plist)))
		(setq decrypt t
		      plist nil))
	    (setq plist (nthcdr 2 plist)))
	  (setq entries (cons entry entries)))))
    ;; Second, decrypt the encrypted plist and try again.
    (when decrypt
      (setq entries nil)
      (plstore--decrypt plstore)
      (setq alist (plstore--get-merged-alist plstore))
      (while alist
	(setq entry (car alist)
	      alist (cdr alist)
	      match (plstore--match entry keys nil))
	(if match
	    (setq entries (cons entry entries)))))
    (nreverse entries)))

(defun plstore-get (plstore name)
  "Get an entry with NAME in PLSTORE."
  (let ((entry (assoc name (plstore--get-merged-alist plstore)))
	plist)
    (setq plist (cdr entry))
    (while plist
      (if (string-match "\\`:secret-" (symbol-name (car plist)))
	  (progn
	    (plstore--decrypt plstore)
	    (setq entry (assoc name (plstore--get-merged-alist plstore))
		  plist nil))
	(setq plist (nthcdr 2 plist))))
    entry))

(defun plstore-put (plstore name keys secret-keys)
  "Put an entry with NAME in PLSTORE.
KEYS is a plist containing non-secret data.
SECRET-KEYS is a plist containing secret data."
  (let (entry
	plist
	secret-plist
	symbol)
    (if secret-keys
	(plstore--decrypt plstore))
    (while secret-keys
      (setq symbol
	    (intern (concat ":secret-"
			    (substring (symbol-name (car secret-keys)) 1))))
      (setq plist (plist-put plist symbol t)
	    secret-plist (plist-put secret-plist
				    (car secret-keys) (car (cdr secret-keys)))
	    secret-keys (nthcdr 2 secret-keys)))
    (while keys
      (setq symbol
	    (intern (concat ":secret-"
			    (substring (symbol-name (car keys)) 1))))
      (setq plist (plist-put plist (car keys) (car (cdr keys)))
	    keys (nthcdr 2 keys)))
    (setq entry (assoc name (plstore--get-alist plstore)))
    (if entry
	(setcdr entry plist)
      (plstore--set-alist
       plstore
       (cons (cons name plist) (plstore--get-alist plstore))))
    (when secret-plist
      (setq entry (assoc name (plstore--get-secret-alist plstore)))
      (if entry
	  (setcdr entry secret-plist)
	(plstore--set-secret-alist
	 plstore
	 (cons (cons name secret-plist) (plstore--get-secret-alist plstore)))))
    (plstore--merge-secret plstore)))

(defvar pp-escape-newlines)
(defun plstore-save (plstore)
  "Save the contents of PLSTORE associated with a FILE."
  (with-current-buffer (plstore--get-buffer plstore)
    (erase-buffer)
    (insert ";;; public entries\n" (pp-to-string (plstore--get-alist plstore)))
    (if (plstore--get-secret-alist plstore)
	(let ((context (epg-make-context 'OpenPGP))
	      (pp-escape-newlines nil)
	      cipher)
	  (epg-context-set-armor context t)
	  (epg-context-set-passphrase-callback
	   context
	   (cons #'plstore-passphrase-callback-function
		 plstore))
	  (setq cipher (epg-encrypt-string context
					   (pp-to-string
					    (plstore--get-secret-alist plstore))
					   nil))
	  (insert ";;; secret entries\n" (pp-to-string cipher))))
    (save-buffer)))

(provide 'plstore)

;;; plstore.el ends here
