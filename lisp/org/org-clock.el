;;; org-clock.el --- The time clocking code for Org-mode

;; Copyright (C) 2004, 2005, 2006, 2007, 2008 Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten at orgmode dot org>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;; Version: 6.02b
;;
;; This file is part of GNU Emacs.
;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file contains the time clocking code for Org-mode

(require 'org)
(eval-when-compile
  (require 'cl)
  (require 'calendar))

(declare-function calendar-absolute-from-iso    "cal-iso"    (&optional date))

(defgroup org-clock nil
  "Options concerning clocking working time in Org-mode."
  :tag "Org Clock"
  :group 'org-progress)

(defcustom org-clock-into-drawer 2
  "Should clocking info be wrapped into a drawer?
When t, clocking info will always be inserted into a :CLOCK: drawer.
If necessary, the drawer will be created.
When nil, the drawer will not be created, but used when present.
When an integer and the number of clocking entries in an item
reaches or exceeds this number, a drawer will be created."
  :group 'org-todo
  :group 'org-clock
  :type '(choice
	  (const :tag "Always" t)
	  (const :tag "Only when drawer exists" nil)
	  (integer :tag "When at least N clock entries")))

(defcustom org-clock-out-when-done t
  "When t, the clock will be stopped when the relevant entry is marked DONE.
Nil means, clock will keep running until stopped explicitly with
`C-c C-x C-o', or until the clock is started in a different item."
  :group 'org-clock
  :type 'boolean)

(defcustom org-clock-out-remove-zero-time-clocks nil
  "Non-nil means, remove the clock line when the resulting time is zero."
  :group 'org-clock
  :type 'boolean)

(defcustom org-clock-in-switch-to-state nil
  "Set task to a special todo state while clocking it.
The value should be the state to which the entry should be switched."
  :group 'org-clock
  :group 'org-todo
  :type '(choice
	  (const :tag "Don't force a state" nil)
	  (string :tag "State")))

(defcustom org-clock-history-length 5
  "Number of clock tasks to remember in history."
  :group 'org-clock
  :type 'integer)

(defcustom org-clock-heading-function nil
  "When non-nil, should be a function to create `org-clock-heading'.
This is the string shown in the mode line when a clock is running.
The function is called with point at the beginning of the headline."
  :group 'org-clock
  :type 'function)


;;; The clock for measuring work time.

(defvar org-mode-line-string "")
(put 'org-mode-line-string 'risky-local-variable t)

(defvar org-mode-line-timer nil)
(defvar org-clock-heading "")
(defvar org-clock-start-time "")

(defvar org-clock-history nil
  "Marker pointing to the previous task teking clock time.
This is used to find back to the previous task after interrupting work.
When clocking into a task and the clock is currently running, this marker
is moved to the position of the currently running task and continues
to point there even after the task is clocked out.")

(defvar org-clock-default-task (make-marker)
  "Marker pointing to the default task that should clock time.
The clock can be made to switch to this task after clocking out
of a different task.")

(defvar org-clock-interrupted-task (make-marker)
  "Marker pointing to the default task that should clock time.
The clock can be made to switch to this task after clocking out
of a different task.")

(defun org-clock-history-push (&optional pos buffer)
  "Push a marker to the clock history."
  (let ((m (move-marker (make-marker) (or pos (point)) buffer)) n l)
    (while (setq n (member m org-clock-history))
      (move-marker (car n) nil))
    (setq org-clock-history
	  (delq nil
		(mapcar (lambda (x) (if (marker-buffer x) x nil))
			org-clock-history)))
    (when (>= (setq l (length org-clock-history)) org-clock-history-length)
      (setq org-clock-history
	    (nreverse
	     (nthcdr (- l org-clock-history-length -1)
		     (nreverse org-clock-history)))))
    (push m org-clock-history)))

(defun org-clock-select-task (&optional prompt)
  "Select a task that recently was associated with clocking."
  (interactive)
  (let (sel-list rpl file task (i 0) s)
    (save-window-excursion 
      (org-switch-to-buffer-other-window
       (get-buffer-create "*Clock Task Select*"))
      (erase-buffer)
      (when (marker-buffer org-clock-default-task)
	(insert (org-add-props "Default Task\n" nil 'face 'bold))
	(setq s (org-clock-insert-selection-line ?d org-clock-default-task))
	(push s sel-list))
      (when (marker-buffer org-clock-interrupted-task)
	(insert (org-add-props "The task interrupted by starting the last one\n" nil 'face 'bold))
	(setq s (org-clock-insert-selection-line ?i org-clock-interrupted-task))
	(push s sel-list))
      (when (marker-buffer org-clock-marker)
	(insert (org-add-props "Current Clocking Task\n" nil 'face 'bold))
	(setq s (org-clock-insert-selection-line ?c org-clock-marker))
	(push s sel-list))
      (insert (org-add-props "Recent Tasks\n" nil 'face 'bold))
      (mapc
       (lambda (m)
	 (when (marker-buffer m)
	   (setq i (1+ i)
		 s (org-clock-insert-selection-line
		    (string-to-char (number-to-string i)) m))
	   (push s sel-list)))
       org-clock-history)
      (shrink-window-if-larger-than-buffer)
      (message (or prompt "Select task for clocking:"))
      (setq rpl (read-char-exclusive))
      (cond
       ((eq rpl ?q) nil)
       ((eq rpl ?x) nil)
       ((assoc rpl sel-list) (cdr (assoc rpl sel-list)))
       (t (error "Invalid task choice %c" rpl))))))

(defun org-clock-insert-selection-line (i marker)
  (when (marker-buffer marker)
    (let (file cat task)
      (with-current-buffer (marker-buffer marker)
	(save-excursion
	  (goto-char marker)
	  (setq file (buffer-file-name (marker-buffer marker))
		cat (or (org-get-category)
			(progn (org-refresh-category-properties)
			       (org-get-category)))
		task (org-get-heading 'notags))))
      (when (and cat task)
	(insert (format "[%c] %-15s %s\n" i cat task))
	(cons i marker)))))
  
(defun org-update-mode-line ()
  (let* ((delta (- (time-to-seconds (current-time))
                   (time-to-seconds org-clock-start-time)))
	 (h (floor delta 3600))
	 (m (floor (- delta (* 3600 h)) 60)))
    (setq org-mode-line-string
	  (propertize (format "-[%d:%02d (%s)]" h m org-clock-heading)
		      'help-echo "Org-mode clock is running"))
    (force-mode-line-update)))

(defvar org-clock-mode-line-entry nil
  "Information for the modeline about the running clock.")

(defun org-clock-in (&optional select)
  "Start the clock on the current item.
If necessary, clock-out of the currently active clock.
With prefix arg SELECT, offer a list of recently clocked ta sks to
clock into.  When SELECT is `C-u C-u', clock into the current task and mark
is as the default task, a special task that will always be offered in
the clocking selection, associated with the letter `d'."
  (interactive "P")
  (let ((interrupting (marker-buffer org-clock-marker))
	ts selected-task)
    (when (equal select '(4))
      (setq selected-task (org-clock-select-task "Clock-in on task: "))
      (if selected-task
	  (setq selected-task (copy-marker selected-task))
	(error "Abort")))
    ;; Are we interrupting the clocking of a differnt task?
    (if interrupting
	(progn
	  (move-marker org-clock-interrupted-task
		       (marker-position org-clock-marker)
		       (marker-buffer org-clock-marker))
	  (org-clock-out t)))
    
    (when (equal select '(16))
      (save-excursion
	(org-back-to-heading t)
	(move-marker org-clock-default-task (point))))
    
    (save-excursion
      (org-back-to-heading t)
      (when (and selected-task (marker-buffer selected-task))
	(set-buffer (marker-buffer selected-task))
	(goto-char selected-task)
	(move-marker selected-task nil))
      (or interrupting (move-marker org-clock-interrupted-task nil))
      (org-clock-history-push)
      (when (and org-clock-in-switch-to-state
		 (not (looking-at (concat outline-regexp "[ \t]*"
					  org-clock-in-switch-to-state
					  "\\>"))))
	(org-todo org-clock-in-switch-to-state))
      (if (and org-clock-heading-function
	       (functionp org-clock-heading-function))
	  (setq org-clock-heading (funcall org-clock-heading-function))
	(if (looking-at org-complex-heading-regexp)
	    (setq org-clock-heading (match-string 4))
	  (setq org-clock-heading "???")))
      (setq org-clock-heading (propertize org-clock-heading 'face nil))
      (org-clock-find-position)
      
      (insert "\n") (backward-char 1)
      (indent-relative)
      (insert org-clock-string " ")
      (setq org-clock-start-time (current-time))
      (setq ts (org-insert-time-stamp (current-time) 'with-hm 'inactive))
      (move-marker org-clock-marker (point) (buffer-base-buffer))
      (or global-mode-string (setq global-mode-string '("")))
      (or (memq 'org-mode-line-string global-mode-string)
	  (setq global-mode-string
		(append global-mode-string '(org-mode-line-string))))
      (org-update-mode-line)
      (setq org-mode-line-timer (run-with-timer 60 60 'org-update-mode-line))
      (message "Clock started at %s" ts))))

(defun org-clock-find-position ()
  "Find the location where the next clock line should be inserted."
  (org-back-to-heading t)
  (catch 'exit
    (let ((beg (save-excursion
		 (beginning-of-line 2)
		 (or (bolp) (newline))
		 (point)))
	  (end (progn (outline-next-heading) (point)))
	  (re (concat "^[ \t]*" org-clock-string))
	  (cnt 0)
	  first last)
      (goto-char beg)
      (when (eobp) (newline) (setq end (max (point) end)))
      (when (re-search-forward "^[ \t]*:CLOCK:" end t)
	;; we seem to have a CLOCK drawer, so go there.
	(beginning-of-line 2)
	(throw 'exit t))
      ;; Lets count the CLOCK lines
      (goto-char beg)
      (while (re-search-forward re end t)
	(setq first (or first (match-beginning 0))
	      last (match-beginning 0)
	      cnt (1+ cnt)))
      (when (and (integerp org-clock-into-drawer)
		 (>= (1+ cnt) org-clock-into-drawer))
	;; Wrap current entries into a new drawer
	(goto-char last)
	(beginning-of-line 2)
	(if (org-at-item-p) (org-end-of-item))
	(insert ":END:\n")
	(beginning-of-line 0)
	(org-indent-line-function)
	(goto-char first)
	(insert ":CLOCK:\n")
	(beginning-of-line 0)
	(org-indent-line-function)
	(org-flag-drawer t)
	(beginning-of-line 2)
	(throw 'exit nil))

      (goto-char beg)
      (while (and (looking-at (concat "[ \t]*" org-keyword-time-regexp))
		  (not (equal (match-string 1) org-clock-string)))
	;; Planning info, skip to after it
	(beginning-of-line 2)
	(or (bolp) (newline)))
      (when (eq t org-clock-into-drawer)
	(insert ":CLOCK:\n:END:\n")
	(beginning-of-line -1)
	(org-indent-line-function)
	(org-flag-drawer t)
	(beginning-of-line 2)
	(org-indent-line-function)))))

(defun org-clock-out (&optional fail-quietly)
  "Stop the currently running clock.
If there is no running clock, throw an error, unless FAIL-QUIETLY is set."
  (interactive)
  (catch 'exit
  (if (not (marker-buffer org-clock-marker))
      (if fail-quietly (throw 'exit t) (error "No active clock")))
  (let (ts te s h m remove)
    (save-excursion
      (set-buffer (marker-buffer org-clock-marker))
      (save-restriction
	(widen)
	(goto-char org-clock-marker)
	(beginning-of-line 1)
	(if (and (looking-at (concat "[ \t]*" org-keyword-time-regexp))
		 (equal (match-string 1) org-clock-string))
	    (setq ts (match-string 2))
	  (if fail-quietly (throw 'exit nil) (error "Clock start time is gone")))
	(goto-char (match-end 0))
	(delete-region (point) (point-at-eol))
	(insert "--")
	(setq te (org-insert-time-stamp (current-time) 'with-hm 'inactive))
	(setq s (- (time-to-seconds (apply 'encode-time (org-parse-time-string te)))
		   (time-to-seconds (apply 'encode-time (org-parse-time-string ts))))
	      h (floor (/ s 3600))
	      s (- s (* 3600 h))
	      m (floor (/ s 60))
	      s (- s (* 60 s)))
	(insert " => " (format "%2d:%02d" h m))
	(when (setq remove (and org-clock-out-remove-zero-time-clocks
				(= (+ h m) 0)))
	  (beginning-of-line 1)
	  (delete-region (point) (point-at-eol))
	  (and (looking-at "\n") (> (point-max) (1+ (point)))
	       (delete-char 1)))
	(move-marker org-clock-marker nil)
	(when org-log-note-clock-out
	  (org-add-log-setup 'clock-out))
	(when org-mode-line-timer
	  (cancel-timer org-mode-line-timer)
	  (setq org-mode-line-timer nil))
	(setq global-mode-string
	      (delq 'org-mode-line-string global-mode-string))
	(force-mode-line-update)
	(message "Clock stopped at %s after HH:MM = %d:%02d%s" te h m
		 (if remove " => LINE REMOVED" "")))))))

(defun org-clock-cancel ()
  "Cancel the running clock be removing the start timestamp."
  (interactive)
  (if (not (marker-buffer org-clock-marker))
      (error "No active clock"))
  (save-excursion
    (set-buffer (marker-buffer org-clock-marker))
    (goto-char org-clock-marker)
    (delete-region (1- (point-at-bol)) (point-at-eol)))
  (setq global-mode-string
	(delq 'org-mode-line-string global-mode-string))
  (force-mode-line-update)
  (message "Clock canceled"))

(defun org-clock-goto (&optional select)
  "Go to the currently clocked-in entry.
With prefix arg SELECT, offer recently clocked tasks."
  (interactive "P")
  (let ((m (if select
	       (org-clock-select-task "Select task to go to: ")
	     org-clock-marker)))
    (if (not (marker-buffer m))
	(if select
	    (error "No task selected")
	  (error "No active clock")))
    (switch-to-buffer (marker-buffer m))
    (goto-char m)
    (org-show-entry)
    (org-back-to-heading)
    (org-cycle-hide-drawers 'children)
    (recenter)))

(defvar org-clock-file-total-minutes nil
  "Holds the file total time in minutes, after a call to `org-clock-sum'.")
  (make-variable-buffer-local 'org-clock-file-total-minutes)

(defun org-clock-sum (&optional tstart tend)
  "Sum the times for each subtree.
Puts the resulting times in minutes as a text property on each headline."
  (interactive)
  (let* ((bmp (buffer-modified-p))
	 (re (concat "^\\(\\*+\\)[ \t]\\|^[ \t]*"
		     org-clock-string
		     "[ \t]*\\(?:\\(\\[.*?\\]\\)-+\\(\\[.*?\\]\\)\\|=>[ \t]+\\([0-9]+\\):\\([0-9]+\\)\\)"))
	 (lmax 30)
	 (ltimes (make-vector lmax 0))
	 (t1 0)
	 (level 0)
	 ts te dt
	 time)
    (remove-text-properties (point-min) (point-max) '(:org-clock-minutes t))
    (save-excursion
      (goto-char (point-max))
      (while (re-search-backward re nil t)
	(cond
	 ((match-end 2)
	  ;; Two time stamps
	  (setq ts (match-string 2)
		te (match-string 3)
		ts (time-to-seconds
		    (apply 'encode-time (org-parse-time-string ts)))
		te (time-to-seconds
		    (apply 'encode-time (org-parse-time-string te)))
		ts (if tstart (max ts tstart) ts)
		te (if tend (min te tend) te)
		dt (- te ts)
		t1 (if (> dt 0) (+ t1 (floor (/ dt 60))) t1)))
	 ((match-end 4)
	  ;; A naket time
	  (setq t1 (+ t1 (string-to-number (match-string 5))
		      (* 60 (string-to-number (match-string 4))))))
	 (t ;; A headline
	  (setq level (- (match-end 1) (match-beginning 1)))
	  (when (or (> t1 0) (> (aref ltimes level) 0))
	    (loop for l from 0 to level do
		  (aset ltimes l (+ (aref ltimes l) t1)))
	    (setq t1 0 time (aref ltimes level))
	    (loop for l from level to (1- lmax) do
		  (aset ltimes l 0))
	    (goto-char (match-beginning 0))
	    (put-text-property (point) (point-at-eol) :org-clock-minutes time)))))
      (setq org-clock-file-total-minutes (aref ltimes 0)))
    (set-buffer-modified-p bmp)))

(defun org-clock-display (&optional total-only)
  "Show subtree times in the entire buffer.
If TOTAL-ONLY is non-nil, only show the total time for the entire file
in the echo area."
  (interactive)
  (org-remove-clock-overlays)
  (let (time h m p)
    (org-clock-sum)
    (unless total-only
      (save-excursion
	(goto-char (point-min))
	(while (or (and (equal (setq p (point)) (point-min))
			(get-text-property p :org-clock-minutes))
		   (setq p (next-single-property-change
			    (point) :org-clock-minutes)))
	  (goto-char p)
	  (when (setq time (get-text-property p :org-clock-minutes))
	    (org-put-clock-overlay time (funcall outline-level))))
	(setq h (/ org-clock-file-total-minutes 60)
	      m (- org-clock-file-total-minutes (* 60 h)))
	;; Arrange to remove the overlays upon next change.
	(when org-remove-highlights-with-change
	  (org-add-hook 'before-change-functions 'org-remove-clock-overlays
			nil 'local))))
    (message "Total file time: %d:%02d (%d hours and %d minutes)" h m h m)))

(defvar org-clock-overlays nil)
(make-variable-buffer-local 'org-clock-overlays)

(defun org-put-clock-overlay (time &optional level)
  "Put an overlays on the current line, displaying TIME.
If LEVEL is given, prefix time with a corresponding number of stars.
This creates a new overlay and stores it in `org-clock-overlays', so that it
will be easy to remove."
  (let* ((c 60) (h (floor (/ time 60))) (m (- time (* 60 h)))
	 (l (if level (org-get-valid-level level 0) 0))
	 (off 0)
	 ov tx)
    (org-move-to-column c)
    (unless (eolp) (skip-chars-backward "^ \t"))
    (skip-chars-backward " \t")
    (setq ov (org-make-overlay (1- (point)) (point-at-eol))
	  tx (concat (buffer-substring (1- (point)) (point))
		     (make-string (+ off (max 0 (- c (current-column)))) ?.)
		     (org-add-props (format "%s %2d:%02d%s"
					    (make-string l ?*) h m
					    (make-string (- 16 l) ?\ ))
			 '(face secondary-selection))
		     ""))
    (if (not (featurep 'xemacs))
	(org-overlay-put ov 'display tx)
      (org-overlay-put ov 'invisible t)
      (org-overlay-put ov 'end-glyph (make-glyph tx)))
    (push ov org-clock-overlays)))

(defun org-remove-clock-overlays (&optional beg end noremove)
  "Remove the occur highlights from the buffer.
BEG and END are ignored.  If NOREMOVE is nil, remove this function
from the `before-change-functions' in the current buffer."
  (interactive)
  (unless org-inhibit-highlight-removal
    (mapc 'org-delete-overlay org-clock-overlays)
    (setq org-clock-overlays nil)
    (unless noremove
      (remove-hook 'before-change-functions
		   'org-remove-clock-overlays 'local))))

(defvar state) ;; dynamically scoped into this function
(defun org-clock-out-if-current ()
  "Clock out if the current entry contains the running clock.
This is used to stop the clock after a TODO entry is marked DONE,
and is only done if the variable `org-clock-out-when-done' is not nil."
  (when (and org-clock-out-when-done
	     (member state org-done-keywords)
	     (equal (marker-buffer org-clock-marker) (current-buffer))
	     (< (point) org-clock-marker)
	     (> (save-excursion (outline-next-heading) (point))
		org-clock-marker))
    ;; Clock out, but don't accept a logging message for this.
    (let ((org-log-note-clock-out nil))
      (org-clock-out))))

(add-hook 'org-after-todo-state-change-hook
	  'org-clock-out-if-current)

;;;###autoload
(defun org-get-clocktable (&rest props)
  "Get a formatted clocktable with parameters according to PROPS.
The table is created in a temporary buffer, fully formatted and
fontified, and then returned."
  ;; Set the defaults
  (setq props (plist-put props :name "clocktable"))
  (unless (plist-member props :maxlevel)
    (setq props (plist-put props :maxlevel 2)))
  (unless (plist-member props :scope)
    (setq props (plist-put props :scope 'agenda)))
  (with-temp-buffer
    (org-mode)
    (org-create-dblock props)
    (org-update-dblock)
    (font-lock-fontify-buffer)
    (forward-line 2)
    (buffer-substring (point) (progn
				(re-search-forward "^#\\+END" nil t)
				(point-at-bol)))))

(defun org-clock-report (&optional arg)
  "Create a table containing a report about clocked time.
If the cursor is inside an existing clocktable block, then the table
will be updated.  If not, a new clocktable will be inserted.
When called with a prefix argument, move to the first clock table in the
buffer and update it."
  (interactive "P")
  (org-remove-clock-overlays)
  (when arg
    (org-find-dblock "clocktable")
    (org-show-entry))
  (if (org-in-clocktable-p)
      (goto-char (org-in-clocktable-p))
    (org-create-dblock (list :name "clocktable"
			     :maxlevel 2 :scope 'file)))
  (org-update-dblock))

(defun org-in-clocktable-p ()
  "Check if the cursor is in a clocktable."
  (let ((pos (point)) start)
    (save-excursion
      (end-of-line 1)
      (and (re-search-backward "^#\\+BEGIN:[ \t]+clocktable" nil t)
	   (setq start (match-beginning 0))
	   (re-search-forward "^#\\+END:.*" nil t)
	   (>= (match-end 0) pos)
	   start))))

(defun org-clock-special-range (key &optional time as-strings)
  "Return two times bordering a special time range.
Key is a symbol specifying the range and can be one of `today', `yesterday',
`thisweek', `lastweek', `thismonth', `lastmonth', `thisyear', `lastyear'.
A week starts Monday 0:00 and ends Sunday 24:00.
The range is determined relative to TIME.  TIME defaults to the current time.
The return value is a cons cell with two internal times like the ones
returned by `current time' or `encode-time'. if AS-STRINGS is non-nil,
the returned times will be formatted strings."
  (if (integerp key) (setq key (intern (number-to-string key))))
  (let* ((tm (decode-time (or time (current-time))))
	 (s 0) (m (nth 1 tm)) (h (nth 2 tm))
	 (d (nth 3 tm)) (month (nth 4 tm)) (y (nth 5 tm))
	 (dow (nth 6 tm))
	 (skey (symbol-name key))
	 (shift 0)
	 s1 m1 h1 d1 month1 y1 diff ts te fm txt w date)
    (cond
     ((string-match "^[0-9]+$" skey)
      (setq y (string-to-number skey) m 1 d 1 key 'year))
     ((string-match "^\\([0-9]+\\)-\\([0-9]\\{1,2\\}\\)$" skey)
      (setq y (string-to-number (match-string 1 skey))
	    month (string-to-number (match-string 2 skey))
	    d 1 key 'month))
     ((string-match "^\\([0-9]+\\)-[wW]\\([0-9]\\{1,2\\}\\)$" skey)
      (require 'cal-iso)
      (setq y (string-to-number (match-string 1 skey))
	    w (string-to-number (match-string 2 skey)))
      (setq date (calendar-gregorian-from-absolute
		  (calendar-absolute-from-iso (list w 1 y))))
      (setq d (nth 1 date) month (car date) y (nth 2 date)
	    key 'week))
     ((string-match "^\\([0-9]+\\)-\\([0-9]\\{1,2\\}\\)-\\([0-9]\\{1,2\\}\\)$" skey)
      (setq y (string-to-number (match-string 1 skey))
	    month (string-to-number (match-string 2 skey))
	    d (string-to-number (match-string 3 skey))
	    key 'day))
     ((string-match "\\([-+][0-9]+\\)$" skey)
      (setq shift (string-to-number (match-string 1 skey))
	    key (intern (substring skey 0 (match-beginning 1))))))
    (unless shift
      (cond ((eq key 'yesterday) (setq key 'today shift -1))
	    ((eq key 'lastweek)  (setq key 'week  shift -1))
	    ((eq key 'lastmonth) (setq key 'month shift -1))
	    ((eq key 'lastyear)  (setq key 'year  shift -1))))
    (cond
     ((memq key '(day today))
      (setq d (+ d shift) h 0 m 0 h1 24 m1 0))
     ((memq key '(week thisweek))
      (setq diff (+ (* -7 shift) (if (= dow 0) 6 (1- dow)))
	    m 0 h 0 d (- d diff) d1 (+ 7 d)))
     ((memq key '(month thismonth))
      (setq d 1 h 0 m 0 d1 1 month (+ month shift) month1 (1+ month) h1 0 m1 0))
     ((memq key '(year thisyear))
      (setq m 0 h 0 d 1 month 1 y (+ y shift) y1 (1+ y)))
     (t (error "No such time block %s" key)))
    (setq ts (encode-time s m h d month y)
	  te (encode-time (or s1 s) (or m1 m) (or h1 h)
			  (or d1 d) (or month1 month) (or y1 y)))
    (setq fm (cdr org-time-stamp-formats))
    (cond
     ((memq key '(day today))
      (setq txt (format-time-string "%A, %B %d, %Y" ts)))
     ((memq key '(week thisweek))
      (setq txt (format-time-string "week %G-W%V" ts)))
     ((memq key '(month thismonth))
      (setq txt (format-time-string "%B %Y" ts)))
     ((memq key '(year thisyear))
      (setq txt (format-time-string "the year %Y" ts))))
    (if as-strings
	(list (format-time-string fm ts) (format-time-string fm te) txt)
      (list ts te txt))))

(defun org-clocktable-shift (dir n)
  "Try to shift the :block date of the clocktable at point.
Point must be in the #+BEGIN: line of a clocktable, or this function
will throw an error.
DIR is a direction, a symbol `left', `right', `up', or `down'.
Both `left' and `down' shift the block toward the past, `up' and `right'
push it toward the future.
N is the number of shift steps to take.  The size of the step depends on
the currently selected interval size."
  (setq n (prefix-numeric-value n))
  (and (memq dir '(left down)) (setq n (- n)))
  (save-excursion
    (goto-char (point-at-bol))
    (if (not (looking-at "#\\+BEGIN: clocktable\\>.*?:block[ \t]+\\(\\S-+\\)"))
	(error "Line needs a :block definition before this command works")
      (let* ((b (match-beginning 1)) (e (match-end 1))
	     (s (match-string 1))
	     block shift ins y mw d date wp m)
	(cond
	 ((string-match "^\\(today\\|thisweek\\|thismonth\\|thisyear\\)\\([-+][0-9]+\\)?$" s)
	  (setq block (match-string 1 s)
		shift (if (match-end 2)
			  (string-to-number (match-string 2 s))
			0))
	  (setq shift (+ shift n))
	  (setq ins (if (= shift 0) block (format "%s%+d" block shift))))
	 ((string-match "\\([0-9]+\\)\\(-\\([wW]?\\)\\([0-9]\\{1,2\\}\\)\\(-\\([0-9]\\{1,2\\}\\)\\)?\\)?" s)
	  ;;               1        1  2   3       3  4                4  5   6                6  5   2
	  (setq y (string-to-number (match-string 1 s))
                wp (and (match-end 3) (match-string 3 s))
		mw (and (match-end 4) (string-to-number (match-string 4 s)))
		d (and (match-end 6) (string-to-number (match-string 6 s))))
	  (cond
	   (d (setq ins (format-time-string
			 "%Y-%m-%d"
			 (encode-time 0 0 0 (+ d n) m y))))
	   ((and wp mw (> (length wp) 0))
	    (require 'cal-iso)
	    (setq date (calendar-gregorian-from-absolute (calendar-absolute-from-iso (list (+ mw n) 1 y))))
	    (setq ins (format-time-string
		       "%G-W%V"
		       (encode-time 0 0 0 (nth 1 date) (car date) (nth 2 date)))))
	   (mw
	    (setq ins (format-time-string
		       "%Y-%m"
		       (encode-time 0 0 0 1 (+ mw n) y))))
	   (y
	    (setq ins (number-to-string (+ y n))))))
	 (t (error "Cannot shift clocktable block")))
	(when ins
	  (goto-char b)
	  (insert ins)
	  (delete-region (point) (+ (point) (- e b)))
	  (beginning-of-line 1)
	  (org-update-dblock)
	  t)))))

(defun org-dblock-write:clocktable (params)
  "Write the standard clocktable."
  (catch 'exit
    (let* ((hlchars '((1 . "*") (2 . "/")))
	   (ins (make-marker))
	   (total-time nil)
	   (scope (plist-get params :scope))
	   (tostring (plist-get  params :tostring))
	   (multifile (plist-get  params :multifile))
	   (header (plist-get  params :header))
	   (maxlevel (or (plist-get params :maxlevel) 3))
	   (step (plist-get params :step))
	   (emph (plist-get params :emphasize))
	   (ts (plist-get params :tstart))
	   (te (plist-get params :tend))
	   (block (plist-get params :block))
	   (link (plist-get params :link))
	   ipos time p level hlc hdl
	   cc beg end pos tbl tbl1 range-text rm-file-column scope-is-list)
      (setq org-clock-file-total-minutes nil)
      (when step
	(unless (or block (and ts te))
	  (error "Clocktable `:step' can only be used with `:block' or `:tstart,:end'"))
	(org-clocktable-steps params)
	(throw 'exit nil))
      (when block
	(setq cc (org-clock-special-range block nil t)
	      ts (car cc) te (nth 1 cc) range-text (nth 2 cc)))
      (when (integerp ts) (setq ts (calendar-gregorian-from-absolute ts)))
      (when (integerp te) (setq te (calendar-gregorian-from-absolute te)))
      (when (and ts (listp ts))
	(setq ts (format "%4d-%02d-%02d" (nth 2 ts) (car ts) (nth 1 ts))))
      (when (and te (listp te))
	(setq te (format "%4d-%02d-%02d" (nth 2 te) (car te) (nth 1 te))))
      ;; Now the times are strings we can parse.
      (if ts (setq ts (time-to-seconds
		       (apply 'encode-time (org-parse-time-string ts)))))
      (if te (setq te (time-to-seconds
		       (apply 'encode-time (org-parse-time-string te)))))
      (move-marker ins (point))
      (setq ipos (point))

      ;; Get the right scope
      (setq pos (point))
      (cond
       ((and scope (listp scope) (symbolp (car scope)))
	(setq scope (eval scope)))
       ((eq scope 'agenda)
	(setq scope (org-agenda-files t)))
       ((eq scope 'agenda-with-archives)
	(setq scope (org-agenda-files t))
	(setq scope (org-add-archive-files scope)))
       ((eq scope 'file-with-archives)
	(setq scope (org-add-archive-files (list (buffer-file-name)))
	      rm-file-column t)))
      (setq scope-is-list (and scope (listp scope)))
      (save-restriction
	(cond
	 ((not scope))
	 ((eq scope 'file) (widen))
	 ((eq scope 'subtree) (org-narrow-to-subtree))
	 ((eq scope 'tree)
	  (while (org-up-heading-safe))
	  (org-narrow-to-subtree))
	 ((and (symbolp scope) (string-match "^tree\\([0-9]+\\)$"
					     (symbol-name scope)))
	  (setq level (string-to-number (match-string 1 (symbol-name scope))))
	  (catch 'exit
	    (while (org-up-heading-safe)
	      (looking-at outline-regexp)
	      (if (<= (org-reduced-level (funcall outline-level)) level)
		  (throw 'exit nil))))
	  (org-narrow-to-subtree))
	 (scope-is-list
	  (let* ((files scope)
		 (scope 'agenda)
		 (p1 (copy-sequence params))
		 file)
	    (setq p1 (plist-put p1 :tostring t))
	    (setq p1 (plist-put p1 :multifile t))
	    (setq p1 (plist-put p1 :scope 'file))
	    (org-prepare-agenda-buffers files)
	    (while (setq file (pop files))
	      (with-current-buffer (find-buffer-visiting file)
		(setq tbl1 (org-dblock-write:clocktable p1))
		(when tbl1
		  (push (org-clocktable-add-file
			 file
			 (concat "| |*File time*|*"
				 (org-minutes-to-hh:mm-string
				  org-clock-file-total-minutes)
				 "*|\n"
				 tbl1)) tbl)
		  (setq total-time (+ (or total-time 0)
				      org-clock-file-total-minutes))))))))
	(goto-char pos)

	(unless scope-is-list
	  (org-clock-sum ts te)
	  (goto-char (point-min))
	  (while (setq p (next-single-property-change (point) :org-clock-minutes))
	    (goto-char p)
	    (when (setq time (get-text-property p :org-clock-minutes))
	      (save-excursion
		(beginning-of-line 1)
		(when (and (looking-at (org-re "\\(\\*+\\)[ \t]+\\(.*?\\)\\([ \t]+:[[:alnum:]_@:]+:\\)?[ \t]*$"))
			   (setq level (org-reduced-level
					(- (match-end 1) (match-beginning 1))))
			   (<= level maxlevel))
		  (setq hlc (if emph (or (cdr (assoc level hlchars)) "") "")
			hdl (if (not link)
				(match-string 2)
			      (org-make-link-string
			       (format "file:%s::%s"
				       (buffer-file-name)
				       (save-match-data
					 (org-make-org-heading-search-string
					  (match-string 2))))
			       (match-string 2))))
		  (if (and (not multifile) (= level 1)) (push "|-" tbl))
		  (push (concat
			 "| " (int-to-string level) "|" hlc hdl hlc " |"
			 (make-string (1- level) ?|)
			 hlc (org-minutes-to-hh:mm-string time) hlc
			 " |") tbl))))))
	(setq tbl (nreverse tbl))
	(if tostring
	    (if tbl (mapconcat 'identity tbl "\n") nil)
	  (goto-char ins)
	  (insert-before-markers
	   (or header
	       (concat
		"Clock summary at ["
		(substring
		 (format-time-string (cdr org-time-stamp-formats))
		 1 -1)
		"]"
		(if block (concat ", for " range-text ".") "")
		"\n\n"))
	   (if scope-is-list "|File" "")
	   "|L|Headline|Time|\n")
	  (setq total-time (or total-time org-clock-file-total-minutes))
	  (insert-before-markers
	   "|-\n|"
	   (if scope-is-list "|" "")
	   "|"
	   "*Total time*| *"
	   (org-minutes-to-hh:mm-string (or total-time 0))
	   "*|\n|-\n")
	  (setq tbl (delq nil tbl))
	  (if (and (stringp (car tbl)) (> (length (car tbl)) 1)
		   (equal (substring (car tbl) 0 2) "|-"))
	      (pop tbl))
	  (insert-before-markers (mapconcat
				  'identity (delq nil tbl)
				  (if scope-is-list "\n|-\n" "\n")))
	  (backward-delete-char 1)
	  (goto-char ipos)
	  (skip-chars-forward "^|")
	  (org-table-align)
	  (when rm-file-column
	    (forward-char 1)
	    (org-table-delete-column)))))))

(defun org-clocktable-steps (params)
  (let* ((p1 (copy-sequence params))
	 (ts (plist-get p1 :tstart))
	 (te (plist-get p1 :tend))
	 (step0 (plist-get p1 :step))
	 (step (cdr (assoc step0 '((day . 86400) (week . 604800)))))
	 (block (plist-get p1 :block))
	 cc range-text)
    (when block
      (setq cc (org-clock-special-range block nil t)
	    ts (car cc) te (nth 1 cc) range-text (nth 2 cc)))
    (if ts (setq ts (time-to-seconds
		     (apply 'encode-time (org-parse-time-string ts)))))
    (if te (setq te (time-to-seconds
		     (apply 'encode-time (org-parse-time-string te)))))
    (setq p1 (plist-put p1 :header ""))
    (setq p1 (plist-put p1 :step nil))
    (setq p1 (plist-put p1 :block nil))
    (while (< ts te)
      (or (bolp) (insert "\n"))
      (setq p1 (plist-put p1 :tstart (format-time-string
				      (car org-time-stamp-formats)
				      (seconds-to-time ts))))
      (setq p1 (plist-put p1 :tend (format-time-string
				    (car org-time-stamp-formats)
				    (seconds-to-time (setq ts (+ ts step))))))
      (insert "\n" (if (eq step0 'day) "Daily report: " "Weekly report starting on: ")
	      (plist-get p1 :tstart) "\n")
      (org-dblock-write:clocktable p1)
      (re-search-forward "#\\+END:")
      (end-of-line 0))))


(defun org-clocktable-add-file (file table)
  (if table
      (let ((lines (org-split-string table "\n"))
	    (ff (file-name-nondirectory file)))
	(mapconcat 'identity
		   (mapcar (lambda (x)
			     (if (string-match org-table-dataline-regexp x)
				 (concat "|" ff x)
			       x))
			   lines)
		   "\n"))))

(provide 'org-clock)

;;; org-clock.el ends here


;; arch-tag: 7b42c5d4-9b36-48be-97c0-66a869daed4c
