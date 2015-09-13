;;; filenotify.el --- watch files for changes on disk  -*- lexical-binding:t -*-

;; Copyright (C) 2013-2015 Free Software Foundation, Inc.

;; Author: Michael Albinus <michael.albinus@gmx.de>

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

;; This package is an abstraction layer from the different low-level
;; file notification packages `gfilenotify', `inotify' and
;; `w32notify'.

;;; Code:

(defconst file-notify--library
  (cond
   ((featurep 'gfilenotify) 'gfilenotify)
   ((featurep 'inotify) 'inotify)
   ((featurep 'w32notify) 'w32notify))
  "Non-nil when Emacs has been compiled with file notification support.
The value is the name of the low-level file notification package
to be used for local file systems.  Remote file notifications
could use another implementation.")

(defvar file-notify-descriptors (make-hash-table :test 'equal)
  "Hash table for registered file notification descriptors.
A key in this hash table is the descriptor as returned from
`gfilenotify', `inotify', `w32notify' or a file name handler.
The value in the hash table is a list

  \(DIR (FILE . CALLBACK) (FILE . CALLBACK) ...)

Several values for a given DIR happen only for `inotify', when
different files from the same directory are watched.")

;; This function is used by `gfilenotify', `inotify' and `w32notify' events.
;;;###autoload
(defun file-notify-handle-event (event)
  "Handle file system monitoring event.
If EVENT is a filewatch event, call its callback.  It has the format

  \(file-notify (DESCRIPTOR ACTIONS FILE [FILE1-OR-COOKIE]) CALLBACK)

Otherwise, signal a `file-notify-error'."
  (interactive "e")
  (if (and (eq (car event) 'file-notify)
	   (>= (length event) 3))
      (funcall (nth 2 event) (nth 1 event))
    (signal 'file-notify-error
	    (cons "Not a valid file-notify event" event))))

;; Needed for `inotify' and `w32notify'.  In the latter case, COOKIE is nil.
(defvar file-notify--pending-event nil
  "A pending file notification events for a future `renamed' action.
It is a form ((DESCRIPTOR ACTION FILE [FILE1-OR-COOKIE]) CALLBACK).")

(defun file-notify--event-file-name (event)
  "Return file name of file notification event, or nil."
  (expand-file-name
   (or  (and (stringp (nth 2 event)) (nth 2 event)) "")
   (car (gethash (car event) file-notify-descriptors))))

;; Only `gfilenotify' could return two file names.
(defun file-notify--event-file1-name (event)
  "Return second file name of file notification event, or nil.
This is available in case a file has been moved."
  (and (stringp (nth 3 event))
       (expand-file-name
	(nth 3 event) (car (gethash (car event) file-notify-descriptors)))))

;; Cookies are offered by `inotify' only.
(defun file-notify--event-cookie (event)
  "Return cookie of file notification event, or nil.
This is available in case a file has been moved."
  (nth 3 event))

;; `inotify' returns the same descriptor when the file (directory)
;; uses the same inode.  We want to distinguish, and apply a virtual
;; descriptor which make the difference.
(defun file-notify--descriptor (descriptor)
  "Return the descriptor to be used in `file-notify-*-watch'.
For `gfilenotify' and `w32notify' it is the same descriptor as
used in the low-level file notification package."
  (if (and (natnump descriptor) (eq file-notify--library 'inotify))
      (cons descriptor
            (car (cadr (gethash descriptor file-notify-descriptors))))
    descriptor))

;; The callback function used to map between specific flags of the
;; respective file notifications, and the ones we return.
(defun file-notify-callback (event)
  "Handle an EVENT returned from file notification.
EVENT is the cadr of the event in `file-notify-handle-event'
\(DESCRIPTOR ACTIONS FILE [FILE1-OR-COOKIE])."
  (let* ((desc (car event))
	 (registered (gethash desc file-notify-descriptors))
	 (actions (nth 1 event))
	 (file (file-notify--event-file-name event))
	 file1 callback pending-event)

    ;; Make actions a list.
    (unless (consp actions) (setq actions (cons actions nil)))

    ;; Loop over registered entries.  In fact, more than one entry
    ;; happens only for `inotify'.
    (dolist (entry (cdr registered))

      ;; Check, that event is meant for us.
      (unless (setq callback (cdr entry))
	(setq actions nil))

      ;; Loop over actions.  In fact, more than one action happens only
      ;; for `inotify'.
      (dolist (action actions)

	;; Send pending event, if it doesn't match.
	(when (and file-notify--pending-event
		   ;; The cookie doesn't match.
		   (not (eq (file-notify--event-cookie
                             (car file-notify--pending-event))
			    (file-notify--event-cookie event)))
		   (or
		    ;; inotify.
		    (and (eq (nth 1 (car file-notify--pending-event))
                             'moved-from)
			 (not (eq action 'moved-to)))
		    ;; w32notify.
		    (and (eq (nth 1 (car file-notify--pending-event))
                             'renamed-from)
			 (not (eq action 'renamed-to)))))
          (setq pending-event file-notify--pending-event
                file-notify--pending-event nil)
          (setcar (cdar pending-event) 'deleted))

	;; Map action.  We ignore all events which cannot be mapped.
	(setq action
	      (cond
	       ;; gfilenotify.
	       ((memq action '(attribute-changed changed created deleted))
		action)
	       ((eq action 'moved)
		(setq file1 (file-notify--event-file1-name event))
		'renamed)

	       ;; inotify, w32notify.
	       ((eq action 'attrib) 'attribute-changed)
	       ((memq action '(create added)) 'created)
	       ((memq action '(modify modified)) 'changed)
	       ((memq action '(delete 'delete-self move-self removed)) 'deleted)
	       ;; Make the event pending.
	       ((memq action '(moved-from renamed-from))
		(setq file-notify--pending-event
                      `((,desc ,action ,file ,(file-notify--event-cookie event))
                        ,callback))
		nil)
	       ;; Look for pending event.
	       ((memq action '(moved-to renamed-to))
		(if (null file-notify--pending-event)
		    'created
		  (setq file1 file
			file (file-notify--event-file-name
                              (car file-notify--pending-event)))
                  ;; If the source is handled by another watch, we
                  ;; must fire the rename event there as well.
                  (when (not (equal (file-notify--descriptor desc)
                                    (file-notify--descriptor
                                     (caar file-notify--pending-event))))
                    (setq pending-event
                          `((,(caar file-notify--pending-event)
                             renamed ,file ,file1)
                            ,(cadr file-notify--pending-event))))
                  (setq file-notify--pending-event nil)
                  'renamed))))

        ;; Apply pending callback.
        (when pending-event
          (setcar
           (car pending-event) (file-notify--descriptor (caar pending-event)))
          (funcall (cadr pending-event) (car pending-event))
          (setq pending-event nil))

	;; Apply callback.
	(when (and action
		   (or
		    ;; If there is no relative file name for that watch,
		    ;; we watch the whole directory.
		    (null (nth 0 entry))
		    ;; File matches.
		    (string-equal
		     (nth 0 entry) (file-name-nondirectory file))
		    ;; File1 matches.
		    (and (stringp file1)
			 (string-equal
			  (nth 0 entry) (file-name-nondirectory file1)))))
	  (if file1
	      (funcall
	       callback
	       `(,(file-notify--descriptor desc) ,action ,file ,file1))
	    (funcall
	     callback
	     `(,(file-notify--descriptor desc) ,action ,file))))))))

;; `gfilenotify' and `w32notify' return a unique descriptor for every
;; `file-notify-add-watch', while `inotify' returns a unique
;; descriptor per inode only.
(defun file-notify-add-watch (file flags callback)
  "Add a watch for filesystem events pertaining to FILE.
This arranges for filesystem events pertaining to FILE to be reported
to Emacs.  Use `file-notify-rm-watch' to cancel the watch.

The returned value is a descriptor for the added watch.  If the
file cannot be watched for some reason, this function signals a
`file-notify-error' error.

FLAGS is a list of conditions to set what will be watched for.  It can
include the following symbols:

  `change'           -- watch for file changes
  `attribute-change' -- watch for file attributes changes, like
                        permissions or modification time

If FILE is a directory, `change' watches for file creation or
deletion in that directory.  This does not work recursively.

When any event happens, Emacs will call the CALLBACK function passing
it a single argument EVENT, which is of the form

  (DESCRIPTOR ACTION FILE [FILE1])

DESCRIPTOR is the same object as the one returned by this function.
ACTION is the description of the event.  It could be any one of the
following:

  `created'           -- FILE was created
  `deleted'           -- FILE was deleted
  `changed'           -- FILE has changed
  `renamed'           -- FILE has been renamed to FILE1
  `attribute-changed' -- a FILE attribute was changed

FILE is the name of the file whose event is being reported."
  ;; Check arguments.
  (unless (stringp file)
    (signal 'wrong-type-argument (list file)))
  (setq file (expand-file-name file))
  (unless (and (consp flags)
	       (null (delq 'change (delq 'attribute-change (copy-tree flags)))))
    (signal 'wrong-type-argument (list flags)))
  (unless (functionp callback)
    (signal 'wrong-type-argument (list callback)))

  (let* ((handler (find-file-name-handler file 'file-notify-add-watch))
	 (dir (directory-file-name
	       (if (file-directory-p file)
		   file
		 (file-name-directory file))))
	desc func l-flags registered)

    (if handler
	;; A file name handler could exist even if there is no local
	;; file notification support.
	(setq desc (funcall
		    handler 'file-notify-add-watch dir flags callback))

      ;; Check, whether Emacs has been compiled with file
      ;; notification support.
      (unless file-notify--library
	(signal 'file-notify-error
		'("No file notification package available")))

      ;; Determine low-level function to be called.
      (setq func
	    (cond
	     ((eq file-notify--library 'gfilenotify) 'gfile-add-watch)
	     ((eq file-notify--library 'inotify) 'inotify-add-watch)
	     ((eq file-notify--library 'w32notify) 'w32notify-add-watch)))

      ;; Determine respective flags.
      (if (eq file-notify--library 'gfilenotify)
	  (setq l-flags '(watch-mounts send-moved))
	(when (memq 'change flags)
	  (setq
	   l-flags
	   (cond
	    ((eq file-notify--library 'inotify) '(create modify move delete))
	    ((eq file-notify--library 'w32notify)
	     '(file-name directory-name size last-write-time)))))
	(when (memq 'attribute-change flags)
	  (push (cond
                 ((eq file-notify--library 'inotify) 'attrib)
                 ((eq file-notify--library 'w32notify) 'attributes))
                l-flags)))

      ;; Call low-level function.
      (setq desc (funcall func dir l-flags 'file-notify-callback)))

    ;; Modify `file-notify-descriptors'.
    (setq registered (gethash desc file-notify-descriptors))
    (puthash
     desc
     `(,dir
       (,(unless (file-directory-p file) (file-name-nondirectory file))
	. ,callback)
       . ,(cdr registered))
     file-notify-descriptors)

    ;; Return descriptor.
    (file-notify--descriptor desc)))

(defun file-notify-rm-watch (descriptor)
  "Remove an existing watch specified by its DESCRIPTOR.
DESCRIPTOR should be an object returned by `file-notify-add-watch'."
  (let* ((desc (if (consp descriptor) (car descriptor) descriptor))
	 (file (if (consp descriptor) (cdr descriptor)))
	 (dir (car (gethash desc file-notify-descriptors)))
	 handler registered)

    (when (stringp dir)
      (setq handler (find-file-name-handler dir 'file-notify-rm-watch))

      ;; Modify `file-notify-descriptors'.
      (if (not file)
	  (remhash desc file-notify-descriptors)

	(setq registered (gethash desc file-notify-descriptors))
	(setcdr registered
		(delete (assoc file (cdr registered)) (cdr registered)))
	(if (null (cdr registered))
	    (remhash desc file-notify-descriptors)
	  (puthash desc registered file-notify-descriptors)))

      ;; Call low-level function.
      (when (null (cdr registered))
	(if handler
	    ;; A file name handler could exist even if there is no local
	    ;; file notification support.
	    (funcall handler 'file-notify-rm-watch desc)

	  (funcall
	   (cond
	    ((eq file-notify--library 'gfilenotify) 'gfile-rm-watch)
	    ((eq file-notify--library 'inotify) 'inotify-rm-watch)
	    ((eq file-notify--library 'w32notify) 'w32notify-rm-watch))
	   desc))))))

;; The end:
(provide 'filenotify)

;;; filenotify.el ends here
