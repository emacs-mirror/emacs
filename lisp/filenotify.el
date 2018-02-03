;;; filenotify.el --- watch files for changes on disk  -*- lexical-binding:t -*-

;; Copyright (C) 2013-2018 Free Software Foundation, Inc.

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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary

;; This package is an abstraction layer from the different low-level
;; file notification packages `inotify', `kqueue', `gfilenotify' and
;; `w32notify'.

;;; Code:

(require 'cl-lib)
(eval-when-compile (require 'subr-x))

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

(cl-defstruct (file-notify--watch
               (:constructor nil)
               (:constructor
                file-notify--watch-make (directory filename callback)))
  ;; Watched directory
  directory
  ;; Watched relative filename, nil if watching the directory.
  filename
  ;; Function to propagate events to
  callback)

(defun file-notify--watch-absolute-filename (watch)
  "Return the absolute filename observed by WATCH."
  (if (file-notify--watch-filename watch)
      (expand-file-name
       (file-notify--watch-filename watch)
       (file-notify--watch-directory watch))
    (file-notify--watch-directory watch)))

(defvar file-notify-descriptors (make-hash-table :test 'equal)
  "Hash table for registered file notification descriptors.
A key in this hash table is the descriptor as returned from
`inotify', `kqueue', `gfilenotify', `w32notify' or a file name
handler.  The value in the hash table is `file-notify--watch'
struct.")

(defun file-notify--rm-descriptor (descriptor)
  "Remove DESCRIPTOR from `file-notify-descriptors'.
DESCRIPTOR should be an object returned by `file-notify-add-watch'.
If it is registered in `file-notify-descriptors', a stopped event is sent."
  (when-let* ((watch (gethash descriptor file-notify-descriptors)))
    ;; Send `stopped' event.
    (unwind-protect
        (funcall
         (file-notify--watch-callback watch)
         `(,descriptor stopped ,(file-notify--watch-absolute-filename watch)))
      (remhash descriptor file-notify-descriptors))))

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
  (if (and (consp event)
           (eq (car event) 'file-notify)
	   (>= (length event) 3))
      (funcall (nth 2 event) (nth 1 event))
    (signal 'file-notify-error
	    (cons "Not a valid file-notify event" event))))

;; Needed for `inotify' and `w32notify'.  In the latter case, COOKIE is nil.
(defvar file-notify--pending-event nil
  "A pending file notification event for a future `renamed' action.
It is a form ((DESCRIPTOR ACTION FILE [FILE1-OR-COOKIE]) CALLBACK).")

(defun file-notify--event-watched-file (event)
  "Return file or directory being watched.
Could be different from the directory watched by the backend library."
  (when-let* ((watch (gethash (car event) file-notify-descriptors)))
    (file-notify--watch-absolute-filename watch)))

(defun file-notify--event-file-name (event)
  "Return file name of file notification event, or nil."
  (when-let* ((watch (gethash (car event) file-notify-descriptors)))
    (directory-file-name
     (expand-file-name
      (or  (and (stringp (nth 2 event)) (nth 2 event)) "")
      (file-notify--watch-directory watch)))))

;; Only `gfilenotify' could return two file names.
(defun file-notify--event-file1-name (event)
  "Return second file name of file notification event, or nil.
This is available in case a file has been moved."
  (when-let* ((watch (gethash (car event) file-notify-descriptors)))
    (and (stringp (nth 3 event))
         (directory-file-name
          (expand-file-name
           (nth 3 event) (file-notify--watch-directory watch))))))

;; Cookies are offered by `inotify' only.
(defun file-notify--event-cookie (event)
  "Return cookie of file notification event, or nil.
This is available in case a file has been moved."
  (nth 3 event))

;; The callback function used to map between specific flags of the
;; respective file notifications, and the ones we return.
(defun file-notify-callback (event)
  "Handle an EVENT returned from file notification.
EVENT is the cadr of the event in `file-notify-handle-event'
\(DESCRIPTOR ACTIONS FILE [FILE1-OR-COOKIE])."
  (let* ((desc (car event))
	 (watch (gethash desc file-notify-descriptors))
	 (actions (nth 1 event))
	 (file (file-notify--event-file-name event))
	 file1 pending-event stopped)

    ;; Make actions a list.
    (unless (consp actions) (setq actions (cons actions nil)))

    (when watch
      ;; Loop over actions.  In fact, more than one action happens only
      ;; for `inotify' and `kqueue'.
      (while actions
        (let ((action (pop actions)))
          ;; Send pending event, if it doesn't match.
          (when (and file-notify--pending-event
                     ;; The cookie doesn't match.
                     (not (equal (file-notify--event-cookie
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
                 ((memq action '(delete delete-self move-self removed))
		  'deleted)
                 ;; Make the event pending.
                 ((memq action '(moved-from renamed-from))
                  (setq file-notify--pending-event
                        `((,desc ,action ,file
                           ,(file-notify--event-cookie event))
                          ,(file-notify--watch-callback watch)))
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
                    (unless (equal desc (caar file-notify--pending-event))
                      (setq pending-event
                            `((,(caar file-notify--pending-event)
                               renamed ,file ,file1)
                              ,(cadr file-notify--pending-event))))
                    (setq file-notify--pending-event nil)
                    'renamed))))

          ;; Apply pending callback.
          (when pending-event
            (funcall (cadr pending-event) (car pending-event))
            (setq pending-event nil))

          ;; Apply callback.
          (when (and action
                     (or
                      ;; If there is no relative file name for that
                      ;; watch, we watch the whole directory.
                      (null (file-notify--watch-filename watch))
                      ;; File matches.
                      (string-equal
                       (file-notify--watch-filename watch)
                       (file-name-nondirectory file))
                      ;; Directory matches.
                      (string-equal
                       (file-name-nondirectory file)
                       (file-name-nondirectory
                        (file-notify--watch-directory watch)))
                      ;; File1 matches.
                      (and (stringp file1)
                           (string-equal
                            (file-notify--watch-filename watch)
                            (file-name-nondirectory file1)))))
            ;;(message
            ;;"file-notify-callback %S %S %S %S %S"
            ;;desc action file file1 watch)
            (if file1
                (funcall (file-notify--watch-callback watch)
                         `(,desc ,action ,file ,file1))
              (funcall (file-notify--watch-callback watch)
                       `(,desc  ,action ,file))))

          ;; Send `stopped' event.
          (when (or stopped
                    (and (memq action '(deleted renamed))
                         ;; Not, when a file is backed up.
                         (not (and (stringp file1) (backup-file-name-p file1)))
                         ;; Watched file or directory is concerned.
                         (string-equal
                          file (file-notify--event-watched-file event))))
            (file-notify-rm-watch desc)))))))

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

  (let ((handler (find-file-name-handler file 'file-notify-add-watch))
	(dir (directory-file-name
	      (if (file-directory-p file)
		  file
		(file-name-directory file))))
        desc func l-flags)

    (unless (file-directory-p dir)
      (signal 'file-notify-error `("Directory does not exist" ,dir)))

    (if handler
	;; A file name handler could exist even if there is no local
	;; file notification support.
	(setq desc (funcall handler 'file-notify-add-watch dir flags callback))

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
                  ;; kqueue does not report file changes in directory
                  ;; monitor.  So we must watch the file itself.
                  func (if (eq file-notify--library 'kqueue) file dir)
                  l-flags 'file-notify-callback)))

    ;; We do not want to enter quoted file names into the hash.
    (setq file (file-name-unquote file)
          dir  (file-name-unquote dir))

    ;; Modify `file-notify-descriptors'.
    (let ((watch (file-notify--watch-make
                  dir
                  (unless (file-directory-p file) (file-name-nondirectory file))
                  callback)))
      (puthash desc watch file-notify-descriptors))
    ;; Return descriptor.
    desc))

(defun file-notify-rm-watch (descriptor)
  "Remove an existing watch specified by its DESCRIPTOR.
DESCRIPTOR should be an object returned by `file-notify-add-watch'."
  (when-let* ((watch (gethash descriptor file-notify-descriptors)))
    (let ((handler (find-file-name-handler
                    (file-notify--watch-directory watch)
                    'file-notify-rm-watch)))
      (condition-case nil
          (if handler
              ;; A file name handler could exist even if there is no
              ;; local file notification support.
              (funcall handler 'file-notify-rm-watch descriptor)

            (funcall
             (cond
              ((eq file-notify--library 'inotify) 'inotify-rm-watch)
              ((eq file-notify--library 'kqueue) 'kqueue-rm-watch)
              ((eq file-notify--library 'gfilenotify) 'gfile-rm-watch)
              ((eq file-notify--library 'w32notify) 'w32notify-rm-watch))
             descriptor))
        (file-notify-error nil)))
    ;; Modify `file-notify-descriptors'.
    (file-notify--rm-descriptor descriptor)))

(defun file-notify-valid-p (descriptor)
  "Check a watch specified by its DESCRIPTOR.
DESCRIPTOR should be an object returned by `file-notify-add-watch'."
  (when-let* ((watch (gethash descriptor file-notify-descriptors)))
    (let ((handler (find-file-name-handler
                    (file-notify--watch-directory watch)
                    'file-notify-valid-p)))
      (and (if handler
               ;; A file name handler could exist even if there is no
               ;; local file notification support.
               (funcall handler 'file-notify-valid-p descriptor)
             (funcall
              (cond
               ((eq file-notify--library 'inotify) 'inotify-valid-p)
               ((eq file-notify--library 'kqueue) 'kqueue-valid-p)
               ((eq file-notify--library 'gfilenotify) 'gfile-valid-p)
               ((eq file-notify--library 'w32notify) 'w32notify-valid-p))
              descriptor))
           t))))


;; TODO:
;; * Watching a /dir/file may receive events for dir.
;;   (This may be the desired behaviour.)
;; * Watching a file in a already watched directory
;;   If the file is created and *then* a watch is added to that file, the
;;   watch might receive events which occurred prior to it being created,
;;   due to the way events are propagated during idle time.  Note: This
;;   may be perfectly acceptable.

;; The end:
(provide 'filenotify)

;;; filenotify.el ends here
