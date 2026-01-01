;;; plstore.el --- secure plist store -*- lexical-binding: t -*-

;; Copyright (C) 2011-2026 Free Software Foundation, Inc.

;; Author: Daiki Ueno <ueno@gnu.org>
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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Plist based data store providing search and partial encryption.
;;
;; By default, this library uses symmetric encryption, which means
;; that you have to enter the password protecting your store more
;; often than you probably expect to.  To use public key encryption
;; with this library, create a GnuPG key and customize user option
;; `plstore-encrypt-to' to use it.  You can then configure the GnuPG
;; agent to adjust caching and expiration of the passphrase for your
;; store.
;;
;; You can read more on these topics in the EasyPG Assistant user's
;; manual (Info node `(epa)'), of which much information also applies
;; to this library.  Most notably:
;;
;; - Info node `(epa)GnuPG version compatibility'
;; - Info node `(epa)GnuPG Pinentry'
;; - Info node `(epa)Caching Passphrases'
;;
;; Use only keyword symbols (starting with a colon) as property names
;; in any plist stored with this library.  While this library does not
;; actively enforce the use of keyword symbols, it silently assumes
;; that the first character of all property names can be discarded
;; without sacrificing uniqueness of names (FIXME).  Likewise, this
;; library does not enforce that the plists provided as input are
;; actually valid and can behave in undefined ways if they are not
;; (FIXME).
;;
;; Creating:
;;
;; ;; Open a new store associated with ~/.emacs.d/auth.plist.
;; (setq store (plstore-open (expand-file-name "~/.emacs.d/auth.plist")))
;; ;; Both `:host' and `:port' are public property.
;; (plstore-put store "foo" '(:host "foo.example.org" :port 80) nil)
;; ;; No encryption will be needed.
;; (plstore-save store)
;;
;; ;; `:user' is marked as secret.
;; (plstore-put store "bar" '(:host "bar.example.org") '(:user "test"))
;; ;; `:password' is marked as secret.
;; (plstore-put store "baz" '(:host "baz.example.org") '(:password "test"))
;; ;; Those secret properties are encrypted together.
;; (plstore-save store)
;;
;; ;; Kill the buffer visiting ~/.emacs.d/auth.plist.
;; (plstore-close store)
;;
;; Avoid marking one property both as public *and* secret, as the
;; behavior of this library with respect to such duplicate properties
;; is not defined (FIXME).
;;
;; Searching:
;;
;; (setq store (plstore-open (expand-file-name "~/.emacs.d/auth.plist")))
;;
;; ;; As the entry "foo" associated with "foo.example.org" has no
;; ;; secret properties, no need for decryption.
;; (plstore-find store '(:host ("foo.example.org")))
;;
;; ;; As the entry "bar" associated with "bar.example.org" has a
;; ;; secret property `:user', Emacs tries to decrypt the secret (and
;; ;; thus you will need to input passphrase).
;; (plstore-find store '(:host ("bar.example.org")))
;;
;; ;; While the entry "baz" associated with "baz.example.org" has also
;; ;; a secret property `:password', it is encrypted together with
;; ;; `:user' of "bar", so no need to decrypt the secret.
;; (plstore-find store '(:host ("baz.example.org")))
;;
;; (plstore-close store)
;;
;; Editing:
;;
;; This file also provides `plstore-mode', a major mode for editing
;; the plstore format file.  Visit a non-existing file and put the
;; following line:
;;
;; (("foo" :host "foo.example.org" :secret-user "user"))
;;
;; where the prefixing `:secret-' means the property (without
;; `:secret-' prefix) is marked as secret.  Thus, when you save the
;; buffer, the `:secret-user' property is encrypted as `:user'.  Do
;; not use a property consisting solely of the prefix, as the behavior
;; of this library with respect to such properties is not defined
;; (FIXME).
;;
;; You can toggle the view between encrypted form and the decrypted
;; form with C-c C-c.
;;
;; If you have opened a plstore with `plstore-open' you should not
;; edit its underlying buffer in `plstore-mode' or in any other way at
;; the same time, since your manual changes will be overwritten when
;; `plstore-save' is called on that plstore.
;;
;; Internals:
;;
;; This is information on the internal data structure and functions of
;; this library.  None of it should be necessary to actually use it.
;; For easier reading, we usually do not distinguish in this internal
;; documentation between a Lisp object and its printed representation.
;;
;; A plstore corresponds to an alist mapping strings to property
;; lists.  Internally, that alist is organized as two alists, one
;; mapping to the non-secret properties and placeholders for the
;; secret properties (called "template alist" with identifier ALIST)
;; and one mapping to the secret properties ("secret alist",
;; SECRET-ALIST).  The secret alist is read from and written to file
;; as pgp-encrypted printed representation of the alist ("encrypted
;; data", ENCRYPTED-DATA).
;;
;; During the lifetime of a plstore, a third type of alist may pop up,
;; which maps to the merged non-secret properties and plain-text
;; secret properties ("merged alist", MERGED-ALIST).
;;
;; After executing the "foo", "bar", "baz" example from above the
;; alists described above look like the following:
;;
;;   Template Alist:
;;
;;     (("foo" :host "foo.example.org" :port 80)
;;      ("bar" :secret-user t :host "bar.example.org")
;;      ("baz" :secret-password t :host "baz.example.org"))
;;
;;   Secret Alist:
;;
;;     (("bar" :user "test")
;;      ("baz" :password "test"))
;;
;;   Merged Alist:
;;
;;     (("foo" :host "foo.example.org" :port 80)
;;      ("bar" :user "test" :host "bar.example.org")
;;      ("baz" :password "test" :host "baz.example.org"))
;;
;; Finally, a plstore requires a buffer ("plstore buffer", BUFFER) for
;; conversion between its Lisp objects and its file representation.
;; It is important to note that this buffer is *not* continuously
;; synchronized as the plstore changes.  During the lifetime of a
;; plstore, its buffer is read from in function `plstore-open' and
;; (destructively) written to in `plstore-save', but not touched
;; otherwise.  We call the file visited by the plstore buffer the
;; associated file of the plstore.
;;
;; With the identifiers defined above a plstore is a vector with the
;; following elements and accessor functions:
;;
;;   [
;;     BUFFER           ; plstore--get/set-buffer
;;     ALIST            ; plstore--get/set-alist
;;     ENCRYPTED-DATA   ; plstore--get/set-encrypted-data
;;     SECRET-ALIST     ; plstore--get/set-secret-alist
;;     MERGED-ALIST     ; plstore--get/set-merged-alist
;;   ]
;;
;; When a plstore is created through `plstore-open', its ALIST and
;; ENCRYPTED-DATA are initialized from the contents of BUFFER without
;; any decryption taking place, and MERGED-ALIST is initialized as a
;; copy of ALIST.  (Which means that at that stage the merged alist
;; still contains the secret property placeholders!)
;;
;; During on-demand decryption of a plstore through function
;; `plstore--decrypt', SECRET-ALIST is populated from ENCRYPTED-DATA,
;; which is in turn replaced by value nil.  (Which further serves as
;; an indicator that the plstore has been decrypted already.)  In
;; addition, MERGED-ALIST is recomputed by function
;; `plstore--merge-secret' to replace the secret property placeholders
;; by their plain-text secret property equivalents.
;;
;; The file representation of a plstore consists of two Lisp forms plus
;; markers to introduce them:
;;
;;   ;;; public entries
;;   ALIST
;;   ;;; secret entries
;;   ENCRYPTED-DATA
;;
;; Both of these are optional, but the first section must be present
;; if the second one is.  If both sections are missing, the plstore is
;; empty.  If the second section is missing, it contains only
;; non-secret data.  If present, the printed representation of the
;; encrypted data includes the delimiting double quotes.
;;
;; The plstore API (`plstore-open', `plstore-put', etc.) and the
;; plstore mode implemented by `plstore-mode' are orthogonal to each
;; other and should not be mixed up.  In particular, encoding and
;; decoding a plstore mode buffer with `plstore-mode-toggle-display'
;; is not related in any way to the state of the plstore buffer.

;;; Code:

(require 'epg)

(defgroup plstore nil
  "Searchable, partially encrypted, persistent plist store."
  :version "24.1"
  :group 'files)

(defcustom plstore-select-keys 'silent
  "Control whether or not to pop up the key selection dialog.

If t, always asks user to select recipients.
If nil, query user only when a file's default recipients are not
known (i.e. `plstore-encrypt-to' is not locally set in the buffer
visiting a plstore file).
If neither t nor nil, doesn't ask user."
  :type '(choice (const :tag "Ask always" t)
		 (const :tag "Ask when recipients are not set" nil)
		 (const :tag "Don't ask" silent))
  :group 'plstore)

(defcustom plstore-encrypt-to nil
  "Recipient(s) used for encrypting secret entries.
May either be a string or a list of strings.  If it is nil,
symmetric encryption will be used."
  :type '(choice (const nil) (repeat :tag "Recipient(s)" string))
  :local 'permanent-only
  :group 'plstore)

;;;###autoload
(put 'plstore-encrypt-to 'safe-local-variable
     (lambda (val)
       (or (stringp val)
	   (and (listp val)
		(catch 'safe
		  (mapc (lambda (elt)
			  (unless (stringp elt)
			    (throw 'safe nil)))
			val)
		  t)))))


(defvar plstore-encoded nil
  "Non-nil if the current buffer shows the decoded alist.") ; [sic!]

(put 'plstore-encoded 'permanent-local t)

;;; EasyPG callback functions.

(defvar plstore-cache-passphrase-for-symmetric-encryption nil)
(defvar plstore-passphrase-alist nil)

(defun plstore-passphrase-callback-function (_context _key-id plstore)
  (if plstore-cache-passphrase-for-symmetric-encryption
      (let* ((file (file-truename (plstore-get-file plstore)))
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
		    (read-passwd (format "Passphrase for plstore %s: "
					 (plstore--get-buffer plstore))))
	      (setcdr entry (copy-sequence passphrase))
	      passphrase)))
    (read-passwd (format "Passphrase for plstore %s: "
			 (plstore--get-buffer plstore)))))

(defun plstore-progress-callback-function (_context _what _char current total
						    handback)
  (if (= current total)
      (message "%s...done" handback)
    (message "%s...%d%%" handback
	     (if (> total 0) (floor (* (/ current (float total)) 100)) 0))))

;;; Core functions.

(defun plstore--get-buffer (arg)
  (aref arg 0))

(defun plstore--get-alist (arg)
  (aref arg 1))

(defun plstore--get-encrypted-data (arg)
  (aref arg 2))

(defun plstore--get-secret-alist (arg)
  (aref arg 3))

(defun plstore--get-merged-alist (arg)
  (aref arg 4))

(defun plstore--set-buffer (arg buffer)
  (aset arg 0 buffer))

(defun plstore--set-alist (arg plist)
  (aset arg 1 plist))

(defun plstore--set-encrypted-data (arg encrypted-data)
  (aset arg 2 encrypted-data))

(defun plstore--set-secret-alist (arg secret-alist)
  (aset arg 3 secret-alist))

(defun plstore--set-merged-alist (arg merged-alist)
  (aset arg 4 merged-alist))

(defun plstore-get-file (arg)
  (buffer-file-name (plstore--get-buffer arg)))

(defun plstore--make (&optional buffer alist encrypted-data secret-alist
				merged-alist)
  (vector buffer alist encrypted-data secret-alist merged-alist))

(defun plstore--init-from-buffer (plstore)
  "Parse current buffer and initialize PLSTORE from it."
  (goto-char (point-min))
  (when (looking-at ";;; public entries")
    (forward-line)
    (plstore--set-alist plstore (read (point-marker)))
    (forward-sexp)
    (forward-char)
    (when (looking-at ";;; secret entries")
      (forward-line)
      (plstore--set-encrypted-data plstore (read (point-marker))))
    (plstore--merge-secret plstore)))

;;;###autoload
(defun plstore-open (file)
  "Create a plstore instance associated with FILE."
  (let* ((filename (file-truename file))
	 (buffer (or (find-buffer-visiting filename)
		     (generate-new-buffer (format " plstore %s" filename))))
	 (store (plstore--make buffer)))
    (with-current-buffer buffer
      (erase-buffer)
      (condition-case nil
          (let ((coding-system-for-read 'raw-text))
            (insert-file-contents file))
	(error))
      (setq buffer-file-name (file-truename file))
      (set-buffer-modified-p nil)
      (plstore--init-from-buffer store)
      store)))

(defun plstore-revert (plstore)
  "Replace current data in PLSTORE from its associated file."
  (with-current-buffer (plstore--get-buffer plstore)
    (revert-buffer t t)
    (plstore--init-from-buffer plstore)))

(defun plstore-close (plstore)
  "Destroy plstore instance PLSTORE."
  (kill-buffer (plstore--get-buffer plstore)))

(defun plstore--merge-secret (plstore)
  "Determine the merged alist of PLSTORE.
Create the merged alist as a copy of the template alist with all
placeholder properties that have corresponding properties in the
secret alist replaced by their plain-text secret properties."
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
        ;; Search for a placeholder property in the merged alist
        ;; corresponding to the current secret property.
	(setq placeholder
	      (plist-member
	       modified-plist
	       (intern (concat ":secret-"
			       (substring (symbol-name (car plist)) 1)))))
        ;; Replace its name with the real, secret property name.
	(if placeholder
	    (setcar placeholder (car plist)))
        ;; Update its value to the plain-text secret property value.
	(setq modified-plist
	      (plist-put modified-plist (car plist) (car (cdr plist))))
	(setq plist (nthcdr 2 plist)))
      (setcdr modified-entry modified-plist))))

(defun plstore--decrypt (plstore)
  "Decrypt the encrypted data of PLSTORE.
Update its internal alists and other data structures
accordingly."
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
	(condition-case error
	    (setq plain
		  (epg-decrypt-string context
				      (plstore--get-encrypted-data plstore)))
	  (error
	   (let ((entry (assoc (plstore-get-file plstore)
			       plstore-passphrase-alist)))
	     (if entry
		 (setcdr entry nil)))
	   (signal (car error) (cdr error))))
	(plstore--set-secret-alist plstore (car (read-from-string plain)))
	(plstore--merge-secret plstore)
	(plstore--set-encrypted-data plstore nil))))

(defun plstore--match (entry keys skip-if-secret-found)
  "Return whether plist KEYS matches ENTRY.
ENTRY should be a key of the merged alist of a PLSTORE.  This
function returns nil if KEYS do not match ENTRY, t if they match,
and symbol `secret' if the secret alist needs to be consulted to
perform a match."
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
  "Return all PLSTORE entries matching plist KEYS."
  (let (entries alist entry match decrypt plist)
    ;; First, go through the merged alist and collect entries matched
    ;; by the keys.
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
    ;; Second, decrypt the plstore and try again.
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
  "Return the entry named NAME in PLSTORE.
Return nil if there is none."
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
  "Put an entry named NAME in PLSTORE.
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

(defun plstore-delete (plstore name)
  "Delete the first entry named NAME from PLSTORE."
  (let ((entry (assoc name (plstore--get-alist plstore))))
    (if entry
	(plstore--set-alist
	 plstore
	 (delq entry (plstore--get-alist plstore))))
    (setq entry (assoc name (plstore--get-secret-alist plstore)))
    (if entry
	(plstore--set-secret-alist
	 plstore
	 (delq entry (plstore--get-secret-alist plstore))))
    (setq entry (assoc name (plstore--get-merged-alist plstore)))
    (if entry
	(plstore--set-merged-alist
	 plstore
	 (delq entry (plstore--get-merged-alist plstore))))))

(defvar pp-escape-newlines)
(defun plstore--insert-buffer (plstore)
  "Insert the file representation of PLSTORE at point."
  (insert ";;; public entries -*- mode: plstore -*- \n"
	  (pp-to-string (plstore--get-alist plstore)))
  (let ((pp-escape-newlines nil)
        (cipher nil))
    (cond
     ;; Reuse the encrypted data as cipher text if this store has not
     ;; been decrypted yet.
     ((plstore--get-encrypted-data plstore)
      (setq cipher (plstore--get-encrypted-data plstore)))
     ;; Encrypt the secret alist to generate the cipher text.
     ((plstore--get-secret-alist plstore)
      (let ((context (epg-make-context 'OpenPGP))
	    (recipients
	     (cond
	      ((listp plstore-encrypt-to) plstore-encrypt-to)
	      ((stringp plstore-encrypt-to) (list plstore-encrypt-to)))))
	(setf (epg-context-armor context) t)
	(epg-context-set-passphrase-callback
	 context
	 (cons #'plstore-passphrase-callback-function
	       plstore))
	(setq cipher (epg-encrypt-string
		      context
		      (pp-to-string
		       (plstore--get-secret-alist plstore))
		      (if (or (eq plstore-select-keys t)
			      (and (null plstore-select-keys)
				   (not (local-variable-p 'plstore-encrypt-to
							  (current-buffer)))))
			  (epa-select-keys
			   context
			   "Select recipients for encryption.
If no one is selected, symmetric encryption will be performed.  "
			   recipients)
			(if plstore-encrypt-to
			    (epg-list-keys context recipients))))))))
    (when cipher
      (goto-char (point-max))
      (insert ";;; secret entries\n" (pp-to-string cipher)))))

(defun plstore-save (plstore)
  "Save PLSTORE to its associated file.
Save with symmetric encryption or public key encryption depending
on `plstore-encrypt-to'.  If `plstore-encrypt-to' is non-nil but
none of the recipients from `plstore-encrypt-to' matches any
GnuPG key, silently save with symmetric encryption." ; (FIXME)
  (with-current-buffer (plstore--get-buffer plstore)
    (erase-buffer)
    (plstore--insert-buffer plstore)
    (save-buffer)))

;;; plstore mode.

;; The functions related to plstore mode unfortunately introduce yet
;; another alist format ("decoded alist").  After executing the "foo",
;; "bar", "baz" example from above the decoded alist of the plstore
;; would look like the following:
;;
;;   (("foo" :host "foo.example.org" :port 80)
;;    ("bar" :secret-user "test" :host "bar.example.org")
;;    ("baz" :secret-password "test" :host "baz.example.org"))
;;
;; Even more unfortunately, variable and function names of the
;; following are a bit mixed up IMHO: With the current names, the
;; result of function `plstore--encode' is used to create what is
;; presented as "decoded form of a plstore" to the user.  And variable
;; `plstore-encoded' is non-nil if a buffer shows the decoded form.

(defun plstore--encode (plstore)
  "Return the printed representation of the decoded alist of PLSTORE."
  (plstore--decrypt plstore)
  (let ((merged-alist (plstore--get-merged-alist plstore)))
    (concat "("
	    (mapconcat
	     (lambda (entry)
	       (setq entry (copy-sequence entry))
	       (let ((merged-plist (cdr (assoc (car entry) merged-alist)))
		     (plist (cdr entry)))
		 (while plist
		   (if (string-match "\\`:secret-" (symbol-name (car plist)))
		       (setcar (cdr plist)
			       (plist-get
				merged-plist
				(intern (concat ":"
						(substring (symbol-name
							    (car plist))
							   (match-end 0)))))))
		   (setq plist (nthcdr 2 plist)))
		 (prin1-to-string entry)))
	     (plstore--get-alist plstore)
	     "\n")
	    ")")))

(defun plstore--decode (string)
  "Create a plstore instance from STRING.
STRING should be the printed representation of a decoded alist of
some plstore."
  (let* ((alist (car (read-from-string string)))
	 (pointer alist)
	 secret-alist
	 plist
	 entry)
    (while pointer
      (unless (stringp (car (car pointer)))
	(error "Invalid plstore format %s" string))
      (setq plist (cdr (car pointer)))
      (while plist
	(when (string-match "\\`:secret-" (symbol-name (car plist)))
	  (setq entry (assoc (car (car pointer)) secret-alist))
	  (unless entry
	    (setq entry (list (car (car pointer)))
		  secret-alist (cons entry secret-alist)))
	  (setcdr entry (plist-put (cdr entry)
				   (intern (concat ":"
						(substring (symbol-name
							    (car plist))
							   (match-end 0))))
				   (car (cdr plist))))
	  (setcar (cdr plist) t))
	(setq plist (nthcdr 2 plist)))
      (setq pointer (cdr pointer)))
    (plstore--make nil alist nil secret-alist)))

(defun plstore--write-contents-functions ()
  "Convert the decoded form of a plstore in the current buffer.
Convert it to the regular file representation of a plstore if
needed.  This function is used on hook `write-contents-functions'
in plstore mode buffers."
  (when plstore-encoded
    (let ((store (plstore--decode (buffer-string)))
	  (file (buffer-file-name)))
      (unwind-protect
	  (progn
	    (set-visited-file-name nil)
	    (with-temp-buffer
	      (plstore--insert-buffer store)
	      (write-region (buffer-string) nil file)))
	(set-visited-file-name file)
	(set-buffer-modified-p nil))
      t)))

(defun plstore-mode-original ()
  "Show the original form of this buffer."
  (interactive)
  (when plstore-encoded
    (if (and (buffer-modified-p)
	     (y-or-n-p "Save buffer before reading the original form? "))
	(save-buffer))
    (erase-buffer)
    (insert-file-contents-literally (buffer-file-name))
    (set-buffer-modified-p nil)
    (setq plstore-encoded nil)))

(defun plstore-mode-decoded ()
  "Show the decoded form of this buffer."
  (interactive)
  (unless plstore-encoded
    (if (and (buffer-modified-p)
	     (y-or-n-p "Save buffer before decoding? "))
	(save-buffer))
    (let ((store (plstore--make (current-buffer))))
      (plstore--init-from-buffer store)
      (erase-buffer)
      (insert
       (substitute-command-keys "\
;;; You are looking at the decoded form of the plstore file.\n\
;;; To see the original form content, do \\[plstore-mode-toggle-display]\n\n"))
      (insert (plstore--encode store))
      (set-buffer-modified-p nil)
      (setq plstore-encoded t))))

(defun plstore-mode-toggle-display ()
  "Toggle the display mode of PLSTORE between the original and decoded forms."
  (interactive)
  (if plstore-encoded
      (plstore-mode-original)
    (plstore-mode-decoded)))

;;;###autoload
(define-derived-mode plstore-mode emacs-lisp-mode "PLSTORE"
  "Major mode for editing plstore files."
  (make-local-variable 'plstore-encoded)
  (add-hook 'write-contents-functions #'plstore--write-contents-functions)
  (define-key plstore-mode-map "\C-c\C-c" #'plstore-mode-toggle-display)
  ;; to create a new file with plstore-mode, mark it as already decoded
  (if (called-interactively-p 'any)
      (setq plstore-encoded t)
    (plstore-mode-decoded)))

(provide 'plstore)

;;; plstore.el ends here
