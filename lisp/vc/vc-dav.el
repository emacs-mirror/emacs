;;; vc-dav.el --- vc.el support for WebDAV  -*- lexical-binding: t; -*-

;; Copyright (C) 2001, 2004-2022 Free Software Foundation, Inc.

;; Author: Bill Perry <wmperry@gnu.org>
;; Keywords: url, vc
;; Package: vc

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

;;; Todo:
;;
;; - Some methods need to be updated to match the current vc.el.
;;     - rename "version" -> "revision"
;;     - some methods need to take a fileset as a parameter instead of a
;;       single file.

;;; Code:

(require 'url)
(require 'url-dav)

;;; Required functions for a vc backend
(defun vc-dav-registered (url)
  "Return t if URL is registered with a DAV aware server."
  (url-dav-vc-registered url))

(defun vc-dav-state (url)
  "Return the current version control state of URL.
For a list of possible values, see `vc-state'."
  ;; Things we can support for WebDAV
  ;;
  ;; up-to-date - use lockdiscovery
  ;; edited     - check for an active lock by us
  ;; USER       - use lockdiscovery + owner
  ;;
  ;; These don't make sense for WebDAV
  ;; needs-patch
  ;; needs-merge
  ;; unlocked-changes
  (let ((locks (url-dav-active-locks url)))
    (cond
     ((null locks) 'up-to-date)
     ((assoc url locks)
      ;; SOMEBODY has a lock... let's find out who.
      (setq locks (cdr (assoc url locks)))
      (if (rassoc url-dav-lock-identifier locks)
	  ;; _WE_ have a lock
	  'edited
	(cdr (car locks)))))))

(defun vc-dav-checkout-model (_url)
  "Indicate whether URL needs to be \"checked out\" before it can be edited.
See `vc-checkout-model' for a list of possible values."
  ;; The only thing we can support with webdav is 'locking
  'locking)

;; This should figure out the version # of the file somehow.  What is
;; the most appropriate property in WebDAV to look at for this?
(defun vc-dav-workfile-version (_url)
  "Return the current workfile version of URL."
  "Unknown")

(defun vc-dav-register (_url &optional _comment)
  "Register URL in the DAV backend."
  ;; Do we need to do anything here?  FIXME?
  )

(defun vc-dav-checkin (_url _comment &optional _rev)
  "Commit changes in URL to WebDAV. COMMENT is used as a check-in comment."
  ;; This should PUT the resource and release any locks that we hold.
  )

(defun vc-dav-checkout (_url &optional _rev _destfile)
  "Check out revision REV of URL into the working area.

If EDITABLE is non-nil URL should be writable by the user and if
locking is used for URL, a lock should also be set.

If REV is non-nil, that is the revision to check out.  If REV is the
empty string, that means to check ou tht ehead of the trunk.

If optional arg DESTFILE is given, it is an alternate filename to
write the contents to."
  ;; This should LOCK the resource.
  )

(defun vc-dav-revert (_url &optional _contents-done)
  "Revert URL back to the current workfile version.

If optional arg CONTENTS-DONE is non-nil, then the contents of FILE
have already been reverted from a version backup, and this function
only needs to update the status of URL within the backend."
  ;; Should do a GET if !contents_done
  ;; Should UNLOCK the file.
  )

(defun vc-dav-print-log (_url)
  "Insert the revision log of URL into the *vc* buffer."
  )

(defun vc-dav-diff (_url &optional _rev1 _rev2 _buffer _async)
  "Insert the diff for URL into the *vc-diff* buffer.
If REV1 and REV2 are non-nil report differences from REV1 to REV2.
If REV1 is nil, use the current workfile version as the older version.
If REV2 is nil, use the current workfile contents as the nwer version.

It should return a status of either 0 (no differences found), or
1 (either non-empty diff or the diff is run asynchronously)."
  ;; We should do this asynchronously...
  ;; How would we do it at all, that is the question!
  )



;;; Optional functions

;; This should use url-dav-get-properties with a depth of `1' to get
;; all the properties.
(defun vc-dav-dir-state (_url)
  "Find the version control state of all files in DIR in a fast way."
  )

(defun vc-dav-responsible-p (_url)
  "Return non-nil if DAV considers itself `responsible' for URL."
  ;; Check for DAV support on the web server.
  t)

;;; Unimplemented functions
;;
;; vc-dav-mode-line-string(url)
;;    Return a dav-specific mode line string for URL. Are there any
;;    specific states that we want exposed?
;;
;; vc-dir support
;;
;; vc-dav-receive-file(url rev)
;;    Let this backend `receive' a file that is already registered
;;    under another backend.  The default just calls `register', which
;;    should be sufficient for WebDAV.
;;
;; vc-dav-unregister(url)
;;    Unregister URL.  Not possible with WebDAV, other than by
;;    deleting the resource.

(provide 'vc-dav)

;;; vc-dav.el ends here
