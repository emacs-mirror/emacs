;;; filenotify.el --- watch files for changes on disk  -*- lexical-binding:t -*-

;; Copyright (C) 2013-2017 Free Software Foundation, Inc.

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
;; file notification packages `inotify', `kqueue', `gfilenotify' and
;; `w32notify'.

;;; Code:

(require 'cl-lib)

(defconst file-notify--library
  (cond
   ((featurep 'inotify) 'inotify)
   ((featurep 'kqueue) 'kqueue)
   ((featurep 'gfilenotify) 'gfilenotify)
   ((featurep 'w32notify) 'w32notify))
  "Non-nil when Emacs has been compiled with file notification support.
The value is the name of the low-level file notification package
to be used for local file systems.  Remote file notifications
could use another implementation.")

(defvar file-notify-descriptors (make-hash-table :test 'equal)
  "Hash table for registered file notification descriptors.
A key in this hash table is the descriptor as returned from
`inotify', `kqueue', `gfilenotify', `w32notify' or a file name
handler.  The value in the hash table is a list

  (DIR (FILE . CALLBACK) (FILE . CALLBACK) ...)

Several values for a given DIR happen only for `inotify', when
different files from the same directory are watched.")

(defun file-notify--rm-descriptor (descriptor)
  "Remove DESCRIPTOR from `file-notify-descriptors'.
DESCRIPTOR should be an object returned by `file-notify-add-watch'.
If it is registered in `file-notify-descriptors', a stopped event is sent."
  (let* ((desc (if (consp descriptor) (car descriptor) descriptor))
         (registered (gethash desc file-notify-descriptors))
	 (file (if (consp descriptor) (cdr descriptor) (cl-caadr registered)))
	 (dir (car registered)))

    (when (consp registered)
      ;; Send `stopped' event.
      (funcall
       (cdr (assoc file (cdr registered)))
       `(,descriptor stopped ,(if file (expand-file-name file dir) dir)))

      ;; Modify `file-notify-descriptors'.
      (if (not file)
	  (remhash desc file-notify-descriptors)
	(setcdr registered
		(delete (assoc file (cdr registered)) (cdr registered)))
	(if (null (cdr registered))
	    (remhash desc file-notify-descriptors)
	  (puthash desc registered file-notify-descriptors))))))

;; This function is used by `inotify', `kqueue', `gfilenotify' and
;; `w32notify' events.
;;;###autoload
(defun file-notify-handle-event (event)
  "Handle file system monitoring event.
If EVENT is a filewatch event, call its callback.  It has the format

  (file-notify (DESCRIPTOR ACTIONS FILE [FILE1-OR-COOKIE]) CALLBACK)

Otherwise, signal a `file-notify-error'."
  (interactive "e")
  ;;(message "file-notify-handle-event %S" event)
  (if (and (eq (car event) 'file-notify)
	   (>= (length event) 3))
      (funcall (nth 2 event) (nth 1 event))
    (signal 'file-notify-error
	    (cons "Not a valid file-notify event" event))))

;; Needed for `inotify' and `w32notify'.  In the latter case, COOKIE is nil.
(defvar file-notify--pending-event nil
  "A pending file notification events for a future `renamed' action.
It is a form ((DESCRIPTOR ACTION FILE [FILE1-OR-COOKIE]) CALLBACK).")

(defun file-notify--event-watched-file (event)
  "Return file or directory being watched.
Could be different from the directory watched by the backend library."
  (let* ((desc (if (consp (car event)) (caar event) (car event)))
         (registered (gethash desc file-notify-descriptors))
	 (file (if (consp (car event)) (cdar event) (cl-caadr registered)))
	 (dir (car registered)))
    (if file (expand-file-name file dir) dir)))

(defun file-notify--event-file-name (event)
  "Return file name of file notification event, or nil."
  (directory-file-name
   (expand-file-name
    (or  (and (stringp (nth 2 event)) (nth 2 event)) "")
    (car (gethash (car event) file-notify-descriptors)))))

;; Only `gfilenotify' could return two file names.
(defun file-notify--event-file1-name (event)
  "Return second file name of file notification event, or nil.
This is available in case a file has been moved."
  (and (stringp (nth 3 event))
       (directory-file-name
        (expand-file-name
         (nth 3 event) (car (gethash (car event) file-notify-descriptors))))))

;; Cookies are offered by `inotify' only.
(defun file-notify--event-cookie (event)
  "Return cookie of file notification event, or nil.
This is available in case a file has been moved."
  (nth 3 event))

;; `inotify' returns the same descriptor when the file (directory)
;; uses the same inode.  We want to distinguish, and apply a virtual
;; descriptor which make the difference.
(defun file-notify--descriptor (desc file)
  "Return the descriptor to be used in `file-notify-*-watch'.
For `gfilenotify' and `w32notify' it is the same descriptor as
used in the low-level file notification package."
  (if (and (natnump desc) (eq file-notify--library 'inotify))
      (cons desc
            (and (stringp file)
                 (car (assoc
                       (file-name-nondirectory file)
                       (gethash desc file-notify-descriptors)))))
    desc))

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
	 file1 callback pending-event stopped)

    ;; Make actions a list.
    (unless (consp actions) (setq actions (cons actions nil)))

    ;; Loop over registered entries.  In fact, more than one entry
    ;; happens only for `inotify'.
    (dolist (entry (cdr registered))

      ;; Check, that event is meant for us.
      (unless (setq callback (cdr entry))
	(setq actions nil))

      ;; Loop over actions.  In fact, more than one action happens only
      ;; for `inotify' and `kqueue'.
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
	       ((memq action
                      '(attribute-changed changed created deleted renamed))
		action)
	       ((memq action '(moved rename))
		;; The kqueue rename event does not return file1 in
		;; case a file monitor is established.
		(if (setq file1 (file-notify--event-file1-name event))
		    'renamed 'deleted))
	       ((eq action 'ignored)
                (setq stopped t actions nil))
	       ((memq action '(attrib link)) 'attribute-changed)
	       ((memq action '(create added)) 'created)
	       ((memq action '(modify modified write)) 'changed)
	       ((memq action '(delete delete-self move-self removed)) 'deleted)
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
                  (when (not (equal (file-notify--descriptor desc file1)
                                    (file-notify--descriptor
                                     (caar file-notify--pending-event)
                                     (file-notify--event-file-name
                                      file-notify--pending-event))))
                    (setq pending-event
                          `((,(caar file-notify--pending-event)
                             renamed ,file ,file1)
                            ,(cadr file-notify--pending-event))))
                  (setq file-notify--pending-event nil)
                  'renamed))))

        ;; Apply pending callback.
        (when pending-event
          (setcar
           (car pending-event)
           (file-notify--descriptor
            (caar pending-event)
            (file-notify--event-file-name file-notify--pending-event)))
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
		    ;; Directory matches.
		    (string-equal
		     (file-name-nondirectory file)
		     (file-name-nondirectory (car registered)))
		    ;; File1 matches.
		    (and (stringp file1)
			 (string-equal
			  (nth 0 entry) (file-name-nondirectory file1)))))
          ;;(message
           ;;"file-notify-callback %S %S %S %S %S"
           ;;(file-notify--descriptor desc (car entry))
           ;;action file file1 registered)
	  (if file1
	      (funcall
	       callback
	       `(,(file-notify--descriptor desc (car entry))
                 ,action ,file ,file1))
	    (funcall
	     callback
	     `(,(file-notify--descriptor desc (car entry)) ,action ,file))))

        ;; Send `stopped' event.
        (when (or stopped
                  (and (memq action '(deleted renamed))
                       ;; Not, when a file is backed up.
                       (not (and (stringp file1) (backup-file-name-p file1)))
                       ;; Watched file or directory is concerned.
                       (string-equal
                        file (file-notify--event-watched-file event))))
          (file-notify-rm-watch (file-notify--descriptor desc (car entry))))))))

;; `kqueue', `gfilenotify' and `w32notify' return a unique descriptor
;; for every `file-notify-add-watch', while `inotify' returns a unique
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
  `stopped'           -- watching FILE has been stopped

FILE is the name of the file whose event is being reported."
  ;; Check arguments.
  (unless (stringp file)
    (signal 'wrong-type-argument `(,file)))
  (setq file (expand-file-name file))
  (unless (and (consp flags)
	       (null (delq 'change (delq 'attribute-change (copy-tree flags)))))
    (signal 'wrong-type-argument `(,flags)))
  (unless (functionp callback)
    (signal 'wrong-type-argument `(,callback)))

  (let* ((handler (find-file-name-handler file 'file-notify-add-watch))
	 (dir (directory-file-name
	       (if (file-directory-p file)
		   file
		 (file-name-directory file))))
	desc func l-flags registered entry)

    (unless (file-directory-p dir)
      (signal 'file-notify-error `("Directory does not exist" ,dir)))

    (if handler
	;; A file name handler could exist even if there is no local
	;; file notification support.
	(setq desc (funcall
		    handler 'file-notify-add-watch
                    ;; kqueue does not report file changes in
                    ;; directory monitor.  So we must watch the file
                    ;; itself.
                    (if (eq file-notify--library 'kqueue) file dir)
                    flags callback))

      ;; Check, whether Emacs has been compiled with file notification
      ;; support.
      (unless file-notify--library
	(signal 'file-notify-error
		'("No file notification package available")))

      ;; Determine low-level function to be called.
      (setq func
	    (cond
	     ((eq file-notify--library 'inotify) 'inotify-add-watch)
	     ((eq file-notify--library 'kqueue) 'kqueue-add-watch)
	     ((eq file-notify--library 'gfilenotify) 'gfile-add-watch)
	     ((eq file-notify--library 'w32notify) 'w32notify-add-watch)))

      ;; Determine respective flags.
      (if (eq file-notify--library 'gfilenotify)
	  (setq l-flags (append '(watch-mounts send-moved) flags))
	(when (memq 'change flags)
	  (setq
	   l-flags
	   (cond
	    ((eq file-notify--library 'inotify)
	     '(create delete delete-self modify move-self move))
	    ((eq file-notify--library 'kqueue)
	     '(create delete write extend rename))
	    ((eq file-notify--library 'w32notify)
	     '(file-name directory-name size last-write-time)))))
	(when (memq 'attribute-change flags)
	  (push (cond
                 ((eq file-notify--library 'inotify) 'attrib)
                 ((eq file-notify--library 'kqueue) 'attrib)
                 ((eq file-notify--library 'w32notify) 'attributes))
                l-flags)))

      ;; Call low-level function.
      (setq desc (funcall
                  func (if (eq file-notify--library 'kqueue) file dir)
                  l-flags 'file-notify-callback)))

    ;; Modify `file-notify-descriptors'.
    (setq file (unless (file-directory-p file) (file-name-nondirectory file))
	  desc (if (consp desc) (car desc) desc)
	  registered (gethash desc file-notify-descriptors)
	  entry `(,file . ,callback))
    (unless (member entry (cdr registered))
      (puthash desc `(,dir ,entry . ,(cdr registered)) file-notify-descriptors))

    ;; Return descriptor.
    (file-notify--descriptor desc file)))

(defun file-notify-rm-watch (descriptor)
  "Remove an existing watch specified by its DESCRIPTOR.
DESCRIPTOR should be an object returned by `file-notify-add-watch'."
  (let* ((desc (if (consp descriptor) (car descriptor) descriptor))
	 (file (if (consp descriptor) (cdr descriptor)))
         (registered (gethash desc file-notify-descriptors))
	 (dir (car registered))
	 (handler (and (stringp dir)
                       (find-file-name-handler dir 'file-notify-rm-watch))))

    (when (stringp dir)
      ;; Call low-level function.
      (when (or (not file)
                (and (= (length (cdr registered)) 1)
                     (assoc file (cdr registered))))
        (condition-case nil
            (if handler
                ;; A file name handler could exist even if there is no local
                ;; file notification support.
                (funcall handler 'file-notify-rm-watch descriptor)

              (funcall
               (cond
                ((eq file-notify--library 'inotify) 'inotify-rm-watch)
                ((eq file-notify--library 'kqueue) 'kqueue-rm-watch)
                ((eq file-notify--library 'gfilenotify) 'gfile-rm-watch)
                ((eq file-notify--library 'w32notify) 'w32notify-rm-watch))
               desc))
          (file-notify-error nil)))

      ;; Modify `file-notify-descriptors'.
      (file-notify--rm-descriptor descriptor))))

(defun file-notify-valid-p (descriptor)
  "Check a watch specified by its DESCRIPTOR.
DESCRIPTOR should be an object returned by `file-notify-add-watch'."
  (let* ((desc (if (consp descriptor) (car descriptor) descriptor))
	 (file (if (consp descriptor) (cdr descriptor)))
         (registered (gethash desc file-notify-descriptors))
	 (dir (car registered))
	 handler)

    (when (stringp dir)
      (setq handler (find-file-name-handler dir 'file-notify-valid-p))

      (and (or ;; It is a directory.
               (not file)
               ;; The file is registered.
               (assoc file (cdr registered)))
           (if handler
               ;; A file name handler could exist even if there is no
               ;; local file notification support.
               (funcall handler 'file-notify-valid-p descriptor)
             (funcall
              (cond
               ((eq file-notify--library 'inotify) 'inotify-valid-p)
               ((eq file-notify--library 'kqueue) 'kqueue-valid-p)
               ((eq file-notify--library 'gfilenotify) 'gfile-valid-p)
               ((eq file-notify--library 'w32notify) 'w32notify-valid-p))
              desc))
           t))))

;; The end:
(provide 'filenotify)

;;; filenotify.el ends here
