;;; filenotify.el --- watch files for changes on disk  -*- lexical-binding:t -*-

;; Copyright (C) 2013-2020 Free Software Foundation, Inc.

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

(defvar file-notify-debug nil
  "Use for debug messages.")

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
  "The internal struct for bookkeeping watched files or directories.
Used in `file-notify-descriptors'."
  ;; Watched directory.
  directory
  ;; Watched relative filename, nil if watching the directory.
  filename
  ;; Function to propagate events to, or nil if watch is being removed.
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
handler.  The value in the hash table is a `file-notify--watch'
struct.")

(defun file-notify--rm-descriptor (descriptor)
  "Remove DESCRIPTOR from `file-notify-descriptors'.
DESCRIPTOR should be an object returned by `file-notify-add-watch'.
If it is registered in `file-notify-descriptors', a `stopped' event is sent."
  (when-let* ((watch (gethash descriptor file-notify-descriptors)))
    (let ((callback (file-notify--watch-callback watch)))
      ;; Make sure this is the last time the callback is invoked.
      (setf (file-notify--watch-callback watch) nil)
      ;; Send `stopped' event.
      (unwind-protect
          (funcall
           callback
           `(,descriptor stopped ,(file-notify--watch-absolute-filename watch)))
        (remhash descriptor file-notify-descriptors)))))

(cl-defstruct (file-notify (:type list) :named)
  "A file system monitoring event, coming from the backends."
  -event -callback)

;; This function is used by `inotify', `kqueue', `gfilenotify',
;; `w32notify' and remote file system handlers.  Usually, we call the
;; argument `event' for such handlers.  But in the following, `event'
;; means a part of the argument only, so we call the argument `object'.
;;;###autoload
(defun file-notify-handle-event (object)
  "Handle a file system monitoring event, coming from backends.
If OBJECT is a filewatch event, call its callback.
Otherwise, signal a `file-notify-error'."
  (interactive "e")
  (when file-notify-debug
    (message "file-notify-handle-event %S" object))
  (if (file-notify-p object)
      (funcall (file-notify--callback object) (file-notify--event object))
    (signal 'file-notify-error
	    (cons "Not a valid file-notify-event" object))))

(cl-defstruct (file-notify--rename
               (:constructor nil)
               (:constructor
                file-notify--rename-make (watch desc from-file cookie)))
  watch desc from-file cookie)

(defvar file-notify--pending-rename nil
  "A pending rename event awaiting the destination file name.
It is nil or a `file-notify--rename' defstruct where the cookie can be nil.")

(defun file-notify--expand-file-name (watch file)
  "Full file name of FILE reported for WATCH."
  (directory-file-name
   (expand-file-name file (file-notify--watch-directory watch))))

(cl-defun file-notify--callback-inotify ((desc actions file
                                          &optional file1-or-cookie))
  "Notification callback for inotify."
  (file-notify--handle-event
   desc
   (delq nil (mapcar (lambda (action)
                       (cond
                        ((eq action 'create) 'created)
                        ((eq action 'modify) 'changed)
                        ((eq action 'attrib) 'attribute-changed)
                        ((memq action '(delete delete-self move-self)) 'deleted)
                        ((eq action 'moved-from) 'renamed-from)
                        ((eq action 'moved-to) 'renamed-to)
                        ((eq action 'ignored) 'stopped)))
                     actions))
   file file1-or-cookie))

(cl-defun file-notify--callback-kqueue ((desc actions file
                                         &optional file1-or-cookie))
  "Notification callback for kqueue."
  (file-notify--handle-event
   desc
   (delq nil (mapcar (lambda (action)
                       (cond
                        ((eq action 'create) 'created)
                        ((eq action 'write) 'changed)
                        ((memq action '(attrib link)) 'attribute-changed)
                        ((eq action 'delete) 'deleted)
                        ((eq action 'rename) 'renamed)))
                     actions))
   file file1-or-cookie))

(cl-defun file-notify--callback-w32notify ((desc actions file
                                            &optional file1-or-cookie))
  "Notification callback for w32notify."
  (let ((action (pcase actions
                 ('added 'created)
                 ('modified 'changed)
                 ('removed 'deleted)
                 ('renamed-from 'renamed-from)
                 ('renamed-to 'renamed-to))))
    (when action
      (file-notify--handle-event desc (list action) file file1-or-cookie))))

(cl-defun file-notify--callback-gfilenotify ((desc actions file
                                              &optional file1-or-cookie))
  "Notification callback for gfilenotify."
  (file-notify--handle-event
   desc
   (delq nil (mapcar (lambda (action)
                       (cond
                        ((memq action
                               '(created changed attribute-changed deleted))
                         action)
                        ((eq action 'moved) 'renamed)))
                     (if (consp actions) actions (list actions))))
   file file1-or-cookie))

(cl-defun file-notify-callback ((desc actions file &optional file1-or-cookie))
  "Notification callback for file name handlers."
  (file-notify--handle-event
   desc
   ;; File name handlers use gfilenotify or inotify actions.
   (delq nil (mapcar
              (lambda (action)
                (cond
                 ;; gfilenotify actions:
                 ((memq action '(created changed attribute-changed deleted))
                  action)
                 ((eq action 'moved) 'renamed)
                 ;; inotify actions:
                 ((eq action 'create) 'created)
                 ((eq action 'modify) 'changed)
                 ((eq action 'attrib) 'attribute-changed)
                 ((memq action '(delete delete-self move-self)) 'deleted)
                 ((eq action 'moved-from) 'renamed-from)
                 ((eq action 'moved-to) 'renamed-to)
                 ((eq action 'ignored) 'stopped)))
              (if (consp actions) actions (list actions))))
   file file1-or-cookie))

(defun file-notify--call-handler (watch desc action file file1)
  "Call the handler of WATCH with the arguments DESC, ACTION, FILE and FILE1."
  (when (or
         ;; If there is no relative file name for that
         ;; watch, we watch the whole directory.
         (null (file-notify--watch-filename watch))
         ;; File matches.
         (string-equal
          (file-notify--watch-filename watch)
          (file-name-nondirectory file))

         ;; Directory matches.
         ;;  FIXME: What purpose would this condition serve?
         ;;  Doesn't it just slip through events for files
         ;;  having the same name as the last component of the
         ;;  directory of the file that we are really watching?
         ;;(string-equal
         ;; (file-name-nondirectory file)
         ;; (file-name-nondirectory (file-notify--watch-directory watch)))

         ;; File1 matches.
         (and (stringp file1)
              (string-equal (file-notify--watch-filename watch)
                            (file-name-nondirectory file1))))
    (when file-notify-debug
      (message
       "file-notify-callback %S %S %S %S %S %S %S"
       desc action file file1 watch
       (file-notify--watch-absolute-filename watch)
       (file-notify--watch-directory watch)))
    (funcall (file-notify--watch-callback watch)
             (if file1
                 (list desc action file file1)
               (list desc action file)))))

(defun file-notify--handle-event (desc actions file file1-or-cookie)
  "Handle an event returned from file notification.
DESC is the back-end descriptor.  ACTIONS is a list of:
 `created'
 `changed'
 `attribute-changed'
 `deleted'
 `renamed'           -- FILE is old name, FILE1-OR-COOKIE is new name or nil
 `renamed-from'      -- FILE is old name, FILE1-OR-COOKIE is cookie or nil
 `renamed-to'        -- FILE is new name, FILE1-OR-COOKIE is cookie or nil
 `stopped'           -- no more events after this should be sent"
  (let* ((watch (gethash desc file-notify-descriptors))
         (file (and watch (file-notify--expand-file-name watch file))))
    (when watch
      (while actions
        (let ((action (pop actions)))
          ;; We only handle {renamed,moved}-{from,to} pairs when these
          ;; arrive in order without anything else in-between.
          ;; If there is a pending rename that does not match this event,
          ;; then send the former as a deletion (since we don't know the
          ;; rename destination).
          (when file-notify--pending-rename
            (unless (and (equal (file-notify--rename-cookie
                                 file-notify--pending-rename)
                                file1-or-cookie)
                         (eq action 'renamed-to))
              (let ((callback (file-notify--watch-callback
                               (file-notify--rename-watch
                                file-notify--pending-rename))))
                (when callback
                  (funcall callback (list (file-notify--rename-desc
                                           file-notify--pending-rename)
                                          'deleted
                                          (file-notify--rename-from-file
                                           file-notify--pending-rename))))
                (setq file-notify--pending-rename nil))))

          (let ((file1 nil))
            (cond
             ((eq action 'renamed)
              ;; A `renamed' event may not have a destination name;
              ;; if none, treat it as a deletion.
              (if file1-or-cookie
                  (setq file1
                        (file-notify--expand-file-name watch file1-or-cookie))
                (setq action 'deleted)))
             ((eq action 'stopped)
              (file-notify-rm-watch desc)
              (setq actions nil
                    action nil))
             ;; Make the event pending.
             ((eq action 'renamed-from)
              (setq file-notify--pending-rename
                    (file-notify--rename-make watch desc file file1-or-cookie)
                    action nil))
             ;; Look for pending event.
             ((eq action 'renamed-to)
              (if file-notify--pending-rename
                  (let ((callback (file-notify--watch-callback
                                   (file-notify--rename-watch
                                    file-notify--pending-rename)))
                        (pending-desc (file-notify--rename-desc
                                       file-notify--pending-rename))
                        (from-file (file-notify--rename-from-file
                                    file-notify--pending-rename)))
                    (setq file1 file
                          file from-file)
                    ;; If the source is handled by another watch, we
                    ;; must fire the rename event there as well.
                    (when (and (not (equal desc pending-desc))
                               callback)
                      (funcall callback
                               (list pending-desc 'renamed file file1)))
                    (setq file-notify--pending-rename nil
                          action 'renamed))
                (setq action 'created))))

            (when action
              (file-notify--call-handler watch desc action file file1))

            ;; Send `stopped' event.
            (when (and (memq action '(deleted renamed))
                       ;; Not when a file is backed up.
                       (not (and (stringp file1) (backup-file-name-p file1)))
                       ;; Watched file or directory is concerned.
                       (string-equal
                        file (file-notify--watch-absolute-filename watch)))
              (file-notify-rm-watch desc))))))))

(declare-function inotify-add-watch "inotify.c" (file flags callback))
(declare-function kqueue-add-watch "kqueue.c" (file flags callback))
(declare-function w32notify-add-watch "w32notify.c" (file flags callback))
(declare-function gfile-add-watch "gfilenotify.c" (file flags callback))

(defun file-notify--add-watch-inotify (_file dir flags)
  "Add a watch for FILE in DIR with FLAGS, using inotify."
  (inotify-add-watch dir
                     (append
                      (and (memq 'change flags)
                           '(create delete delete-self modify move-self move))
                      (and (memq 'attribute-change flags)
                           '(attrib)))
                     #'file-notify--callback-inotify))

(defun file-notify--add-watch-kqueue (file _dir flags)
  "Add a watch for FILE in DIR with FLAGS, using kqueue."
  ;; kqueue does not report changes to file contents when watching
  ;; directories, so we watch each file directly.
  (kqueue-add-watch file
                    (append
                     (and (memq 'change flags)
	                  '(create delete write extend rename))
                     (and (memq 'attribute-change flags)
                          '(attrib)))
                    #'file-notify--callback-kqueue))

(defun file-notify--add-watch-w32notify (_file dir flags)
  "Add a watch for FILE in DIR with FLAGS, using w32notify."
  (w32notify-add-watch dir
                       (append
                        (and (memq 'change flags)
                             '(file-name directory-name size last-write-time))
                        (and (memq 'attribute-change flags)
                             '(attributes)))
                       #'file-notify--callback-w32notify))

(defun file-notify--add-watch-gfilenotify (_file dir flags)
  "Add a watch for FILE in DIR with FLAGS, using gfilenotify."
  (gfile-add-watch dir
                   (append '(watch-mounts send-moved) flags)
                   #'file-notify--callback-gfilenotify))

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
		(file-name-directory file)))))

    (unless (file-directory-p dir)
      (signal 'file-notify-error `("Directory does not exist" ,dir)))

    (let ((desc
           (if handler
               (funcall handler 'file-notify-add-watch dir flags callback)
             (funcall
              (pcase file-notify--library
                ('inotify     #'file-notify--add-watch-inotify)
                ('kqueue      #'file-notify--add-watch-kqueue)
                ('w32notify   #'file-notify--add-watch-w32notify)
                ('gfilenotify #'file-notify--add-watch-gfilenotify)
                (_ (signal 'file-notify-error
		           '("No file notification package available"))))
              file dir flags))))

      ;; Modify `file-notify-descriptors'.
      (let ((watch (file-notify--watch-make
                    ;; We do not want to enter quoted file names into the hash.
                    (file-name-unquote dir)
                    (unless (file-directory-p file)
                      (file-name-nondirectory file))
                    callback)))
        (puthash desc watch file-notify-descriptors))
      ;; Return descriptor.
      desc)))

(defun file-notify-rm-watch (descriptor)
  "Remove an existing watch specified by its DESCRIPTOR.
DESCRIPTOR should be an object returned by `file-notify-add-watch'."
  (when-let* ((watch (gethash descriptor file-notify-descriptors)))
    ;; If we are called from a `stopped' event, do nothing.
    (when (file-notify--watch-callback watch)
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
      ;; Modify `file-notify-descriptors' and send a `stopped' event.
      (file-notify--rm-descriptor descriptor))))

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

;; * Watching a file in an already watched directory.
;;   If the file is created and *then* a watch is added to that file, the
;;   watch might receive events which occurred prior to it being created,
;;   due to the way events are propagated during idle time.  Note: This
;;   may be perfectly acceptable.

;; The end:
(provide 'filenotify)

;;; filenotify.el ends here
