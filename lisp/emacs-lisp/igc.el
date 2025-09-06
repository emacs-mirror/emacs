;;; igc.el --- Support functions for IGC  -*- lexical-binding: t -*-

;; Copyright (C) 2024-2025 Free Software Foundation, Inc.

;; Keywords: GC

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

;;; Code:

(require 'cl-lib)
(require 'sqlite)

(declare-function igc-info "igc.c")
(declare-function igc--collect "igc.c")
(declare-function igc--roots "igc.c")

(defun igc--diff (i1 i2)
  (cl-loop for (t1 n1 s1) in i1
	   for (_t2 n2 s2) in i2
	   when (and n1 n2 (/= n1 n2))
	   collect (list t1 (- n1 n2) (and s1 (- s1 s2)))))

(defvar igc--a nil
  "IGC snapshot A.  Used for debugging.")
(defvar igc--b nil
  "IGC snapshot B.  Used for debugging.")
(defvar igc--display-mode 'a
  "IGC stats current snapshot, \='a or \='b.")

(defun igc-snapshot ()
  "Take snapshot A or B depending on igc--display-mode."
  (interactive)
  (if (eq igc--display-mode 'a)
      (setq igc--a (igc-info))
    (setq igc--b (igc-info)))
  (igc-stats))

(defun igc--info-to-display ()
  (cl-ecase igc--display-mode
    (diff (igc--diff igc--b igc--a))
    (a igc--a)
    (b igc--b)))

(defun igc-display-diff ()
  "Display the difference between IGC snapshots A and B."
  (interactive)
  (setq igc--display-mode 'diff)
  (igc-stats))

(defun igc-display-a ()
  "Display IGC stats snapshot A.  See igc-stats-mode."
  (interactive)
  (setq igc--display-mode 'a)
  (igc-stats))

(defun igc-display-b ()
  "Display IGC stats snapshot B.  See igc-stats-mode."
  (interactive)
  (setq igc--display-mode 'b)
  (igc-stats))

;;;###autoload
(defun igc-collect ()
  "Perform a full GC."
  (interactive)
  (let ((garbage-collection-messages t))
    (igc--collect)))

(defun igc-clear ()
  "Reset snapshots A and B."
  (interactive)
  (setq igc--a nil igc--b nil)
  (igc-stats))

(define-derived-mode igc-stats-mode special-mode "Statistics"
  "Display memory statistics from `igc-info', about incremental GC.
You can display two snapshots A and B containing the info from `igc-info'
at different times.  These can be displayed either as-is, or by showing the
difference between them.  Type \\`a' to switch to snapshot A
and \\`b' to switch to snapshot B.  To take a snapshot, type \\`s'.
To reset the current snapshot, type \\`s'.
To display the difference between A and B, type \\`d'.
To perform a full GC, type \\`c'.
Type \\`g' to refresh the the difference view.
Type \\`?' to see the mode's help.

The IGC memory statistics is a list of elements describing the various
statistics of the incremental GC.  The elements are of the
form (NAME NOBJECTS NBYTES LARGEST), where:
- NAME is a string describing the kind of objects this entry represents,
- NOBJECTS is the number of objects of this type,
- NBYTES is the number of bytes used by objects of this type,
- LARGEST is the largest object of this type.

In addition, there are several pseudo-objects which provide overall
IGC statistics:
 - committed       -- the amount of committed memory in bytes
 - commit-limit    -- max amount of memory the arena is allowed to commit
 - spare-committed -- memory which remains committed and which the
     arena is managing as free memory
 - reserved        -- total address space reserved by the arena
 - spare           -- spare commit limit fraction
 - pause-time      -- max amount of time GC operations may pause Emacs."
  (keymap-local-set "a" #'igc-display-a)
  (keymap-local-set "b" #'igc-display-b)
  (keymap-local-set "c" #'igc-collect)
  (keymap-local-set "d" #'igc-display-diff)
  (keymap-local-set "s" #'igc-snapshot)
  (keymap-local-set "x" #'igc-clear)
  (display-line-numbers-mode -1)
  (setq header-line-format
	'((:eval (format " %-35s %10s %15s %10s %13s"
			 (concat "Display "
				 (symbol-name igc--display-mode))
			 "Objects"
			 "Bytes"
			 "Avg"
                         "Largest"))))
  (setq-local revert-buffer-function
	      (lambda (&rest _)
		(setq igc--display-mode 'diff)
		(igc-snapshot)
		(igc-stats))))

;;;###autoload
(defun igc-stats ()
  "Display memory statistics from `igc-info'.
You can display two snapshots A and B containing the info from `igc-info'
at different times.  These can be displayed either as-is, or by showing the
difference between them.  \\<igc-stats-mode-map>Type \\[igc-display-a] to switch to snapshot A and \\[igc-display-b] to switch to
snapshot B.  To take a snapshot, type \\[igc-snapshot].
To reset the current snapshot, type \\[igc-clear].
To display the difference between A and B, type \\[igc-display-diff].
Type \\`g' to refresh the the difference view.
Type \\`?' to see the mode's help."
  (interactive)
  (with-current-buffer (get-buffer-create "*igc*")
    (igc-stats-mode)
    (setq buffer-read-only t buffer-file-name nil)
    (let ((old-line (line-number-at-pos))
          (info (igc--info-to-display))
	  (inhibit-read-only t)
	  (inhibit-modification-hooks t)
	  (standard-output (current-buffer)))
      (erase-buffer)
      (delete-all-overlays)
      (when info
	(cl-loop for (title n bytes largest) in info do
                 (insert (format "%-35s %10s %15s %10s %13s\n"
                                 title n bytes
                                 (and bytes n
                                      (if (zerop n)
                                          0
                                        (abs (/ bytes n))))
                                 largest)))
	(sort-lines nil (point-min) (point-max)))
      (goto-char (point-min))
      (forward-line (1- old-line))))
  (display-buffer "*igc*")
  (message "Type `?' in the *igc* buffer for help"))

(defun igc--roots-diff (i1 i2)
  (cl-loop for (t1 n1 s1) in i1
	   for (_t2 n2 s2) in i2
	   unless (= n1 n2)
	   collect (list t1 (- n1 n2) (and s1 (- s1 s2)))))

(defvar igc--roots-a nil
  "IGC roots snapshot A.  Used for debugging statistics.")
(defvar igc--roots-b nil
  "IGC roots snapshot B.  Used for debugging statistics.")
(defvar igc--roots-display-mode 'a)

(defun igc-roots-display-diff ()
"Display the difference between IGC roots snapshots A and B."
  (interactive)
  (setq igc--roots-display-mode 'diff)
  (igc-roots-stats))

(defun igc-roots-display-a ()
  "Display IGC roots for snapshot A."
  (interactive)
  (setq igc--roots-display-mode 'a)
  (igc-roots-stats))

(defun igc-roots-display-b ()
  "Display IGC roots for snapshot B."
  (interactive)
  (setq igc--roots-display-mode 'b)
  (igc-roots-stats))

(defun igc--roots-info ()
  (let ((h (make-hash-table :test 'equal)))
    (cl-loop for (label type start end) in (igc--roots)
             for (found _ n size) = (gethash label h) do
             (if found
                 (puthash label
                          (list label type (1+ n)
                                (and size
                                     (+ size (- end start))) h)
                          h)
               (puthash label
                        (list label type 1 (and end (- end start)))
                        h)))
    (cl-loop for i being the hash-values of h collect i)))

(defun igc--roots-snapshot ()
  "Display roots snapshots a or b."
  (interactive)
  (if (eq igc--roots-display-mode 'a)
      (setq igc--roots-a (igc--roots-info))
    (setq igc--roots-b (igc--roots-info)))
  (igc-roots-stats))

(defun igc--roots-info-to-display ()
  (cl-ecase igc--roots-display-mode
    (diff (igc--roots-diff igc--b igc--a))
    (a igc--roots-a)
    (b igc--roots-b)))

(defun igc-roots-clear ()
  "GC, then set snapshot B to current `igc-info'."
  (interactive)
  (setq igc--roots-a nil igc--roots-b nil)
  (igc-roots-stats))

(define-derived-mode igc-roots-mode special-mode "Roots"
    "Display root statistics from `igc--roots'.
You can display two snapshots A and B containing the info from `igc--roots'
at different times.  These can be displayed either as-is, or by showing the
difference between them.  \\<igc-roots-mode-map>Type \\[igc-roots-display-a] to switch to snapshot A and \\[igc-roots-display-b] to switch to
snapshot B.  To take a snapshot, type \\[igc--roots-snapshot].
To reset the current snapshot, type \\[igc-roots-clear].
To display the difference between A and B, type \\[igc-roots-display-diff].
Type \\`g' to refresh the the difference view.
Type \\`?' to see the mode's help.

The list of IGC roots is a list of elements, one each for every
root.  Each element has the form (LABEL TYPE START END), where
- LABEL is the label of the root
- TYPE is either `ambig' or `exact'
- START is the start address
- END is either the end address or nil."
  (keymap-local-set "a" #'igc-roots-display-a)
  (keymap-local-set "b" #'igc-roots-display-b)
  (keymap-local-set "c" #'igc-collect)
  (keymap-local-set "d" #'igc-roots-display-diff)
  (keymap-local-set "s" #'igc--roots-snapshot)
  (keymap-local-set "x" #'igc-roots-clear)
  (display-line-numbers-mode -1)
  (setq header-line-format
	'((:eval (format " %-25s %10s %15s %15s"
			 (concat "Display "
				 (symbol-name igc--roots-display-mode))
			 "Type" "N" "Bytes"))))
  (setq-local revert-buffer-function
	      (lambda (&rest _)
		(setq igc--roots-display-mode 'diff)
		(igc--roots-snapshot)
		(igc-roots-stats))))

;;;###autoload
(defun igc-roots-stats ()
  "Display root statistics from `igc--roots'.
You can display two snapshots A and B containing the info from `igc--roots'
at different times.  These can be displayed either as-is, or by showing the
difference between them.  \\<igc-roots-mode-map>Type \\[igc-roots-display-a] to switch to snapshot A and \\[igc-roots-display-b] to switch to
snapshot B.  To take a snapshot, type \\[igc--roots-snapshot].
To reset the current snapshot, type \\[igc-roots-clear].
To display the difference between A and B, type \\[igc-roots-display-diff].
Type \\`g' to refresh the the difference view.
Type \\`?' to see the mode's help."
  (interactive)
  (with-current-buffer (get-buffer-create "*igc roots*")
    (igc-roots-mode)
    (setq buffer-read-only t buffer-file-name nil)
    (let ((info (igc--roots-info-to-display))
	  (inhibit-read-only t)
	  (inhibit-modification-hooks t)
	  (standard-output (current-buffer)))
      (erase-buffer)
      (delete-all-overlays)
      (when info
	(cl-loop for (label type n size) in info
		 do (insert (format "%-25s %10s %15s %15s\n"
                                    label type n size)))
	(sort-lines nil (point-min) (point-max)))
      (goto-char (point-min))))
  (display-buffer "*igc roots*")
  (message "Type `?' in the *igc roots* buffer for help"))

(defvar igc--collect-timer nil)
(defvar igc--collect-file nil)
(defvar igc--sqlite nil)

;;;###autoload
(defun igc-stop-collecting-stats ()
  "Stop collecting IGC stats."
  (interactive)
  (when igc--collect-timer
    (cancel-timer igc--collect-timer)
    (setq igc--collect-timer nil)
    (when (sqlitep igc--sqlite)
      (sqlite-close igc--sqlite)
      (setq igc--sqlite nil))))

(defvar igc-stats-time-format "%T.%3N"
  "Format for exporting time stamps in IGC stats, e.g. to csv or sqlite.
Used in calls to `format-time-string'.")

(defun igc--collect-stats-csv ()
  (let ((buffer (get-file-buffer igc--collect-file)))
    (when buffer
      (with-current-buffer buffer
        (goto-char (point-max))
        (when (= (point-min) (point-max))
          (insert (format "\"%s\",\"%s\",\"%s\",\"%s\"\n"
                          "Time" "Type" "N" "Bytes")))
        (cl-loop with time = (format-time-string igc-stats-time-format)
                 for (title n bytes) in (igc-info) do
                 (insert (format "\"%s\",\"%s\",\"%s\",\"%s\"\n"
                                 time title n bytes))))
      (save-buffer))))

(defun igc--collect-stats-sqlite ()
  (let ((values (cl-loop with time = (format-time-string igc-stats-time-format)
                         for (title n bytes) in (igc-info)
                         collect (format "(\"%s\",\"%s\",\"%s\",\"%s\")"
                                         time title n bytes))))
    (with-sqlite-transaction igc--sqlite
      (sqlite-execute igc--sqlite
                      (format "INSERT INTO igc VALUES %s"
                              (mapconcat #'identity values ","))))))

(defun igc--open-sqlite (db)
  (let ((con (or (sqlite-open db)
                 (error "Error opening sqlite database %s" db))))
    (with-sqlite-transaction con
      (sqlite-execute con (concat "CREATE TABLE IF NOT EXISTS igc "
                                  "(time, type, n, bytes)"))
      ;; Which indices we need depends, and has to be seen with time.
      (sqlite-execute con (concat "CREATE INDEX IF NOT EXISTS igc_time_index "
                                  "ON igc (time)"))
      (sqlite-execute con (concat "CREATE INDEX IF NOT EXISTS igc_type_index "
                                  "ON igc (type)")))
    con))

;;;###autoload
(defun igc-start-collecting-stats (type file secs)
  "Start collecting IGC statistics."
  (interactive
   (let* ((completion-ignore-case t)
          (type (completing-read "Output to: " '("SQLite DB" "CSV") nil t))
          (file (if (string-equal-ignore-case type "CSV")
                    (read-file-name "CSV file: ")
                  (unless (sqlite-available-p)
                    (error "Your Emacs is not built with SQLite support"))
                  (read-file-name "Sqlite database file: ")))
          (secs (read-number "Interval seconds: " 10)))
     (list (if (string-equal-ignore-case type "CSV") 'csv 'sqlite)
           file secs)))
  (unless (> secs 0)
    (error "Invalid interval seconds"))
  (igc-stop-collecting-stats)
  (cond ((eq type 'csv)
         (setq igc--collect-file file)
         (find-file-noselect file)
         (setq igc--collect-timer
               (run-at-time nil secs #'igc--collect-stats-csv)))
        (t
         (setq igc--sqlite (igc--open-sqlite file))
         (setq igc--collect-timer
               (run-at-time nil secs #'igc--collect-stats-sqlite)))))


;;; Opportunistic GC

(defvar igc--idle-timer nil
  "Idle timer to trigger oppurtunistic GC.")

(defvar igc--idle-delay 2.0
  "Time, in seconds, to wait for `igc--idle-timer'.")

(defvar igc--step-interval 0.01
  "Time, in seconds, MPS is allowed to use for one step.")

(defun igc-start-idle-timer ()
  "Start a timer to do GC work while Emacs is idle."
  (when igc--idle-timer
    (cancel-timer igc--idle-timer))
  (setq igc--idle-timer
        (run-with-idle-timer igc--idle-delay t #'igc--on-idle)))

(defun igc--predict-idle-time ()
  (* igc--idle-delay 0.66))

(defun igc--on-idle ()
  (let* ((available-time (igc--predict-idle-time))
         (interval igc--step-interval)
         (multiplier (floor (/ available-time interval))))
    (named-let step ((n multiplier))
      (let* ((work-to-do (igc--arena-step interval n)))
        (when (and work-to-do
                   (> n 0)
                   (not (accept-process-output nil 0)))
          (step (1- n)))))))

(provide 'igc)

;;; igc.el ends here.
