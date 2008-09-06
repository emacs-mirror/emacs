;;; proced.el --- operate on system processes like dired

;; Copyright (C) 2008 Free Software Foundation, Inc.

;; Author: Roland Winkler <Roland.Winkler@physik.uni-erlangen.de>
;; Keywords: Processes, Unix

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

;;; Commentary:

;; Proced makes an Emacs buffer containing a listing of the current
;; system processes.  You can use the normal Emacs commands to move around
;; in this buffer, and special Proced commands to operate on the processes
;; listed.
;;
;; To do:
;; - use defcustom where appropriate
;; - interactive temporary customizability of `proced-grammar-alist'
;; - allow "sudo kill PID", "renice PID"

;;; Code:

(require 'time-date)                 ; for `with-decoded-time-value'

(defgroup proced nil
  "Proced mode."
  :group 'processes
  :group 'unix
  :prefix "proced-")

(defcustom proced-signal-function 'signal-process
  "Name of signal function.
It can be an elisp function (usually `signal-process') or a string specifying
the external command (usually \"kill\")."
  :group 'proced
  :type '(choice (function :tag "function")
                 (string :tag "command")))

(defcustom proced-signal-list
  '( ;; signals supported on all POSIX compliant systems
    ("HUP   (1.  Hangup)")
    ("INT   (2.  Terminal interrupt)")
    ("QUIT  (3.  Terminal quit)")
    ("ABRT  (6.  Process abort)")
    ("KILL  (9.  Kill - cannot be caught or ignored)")
    ("ALRM  (14. Alarm Clock)")
    ("TERM  (15. Termination)")
    ;; POSIX 1003.1-2001
    ;; Which systems do not support these signals so that we can
    ;; exclude them from `proced-signal-list'?
    ("CONT (Continue executing)")
    ("STOP (Stop executing / pause - cannot be caught or ignored)")
    ("TSTP (Terminal stop / pause)"))
  "List of signals, used for minibuffer completion."
  :group 'proced
  :type '(repeat (string :tag "signal")))

;; For which attributes can we use a fixed width of the output field?
;; A fixed width speeds up formatting, yet it can make
;; `proced-grammar-alist' system-dependent.
;; (If proced runs like top(1) we want it to be fast.)
;;
;; If it is impossible / unlikely that an attribute has the same value
;; for two processes, then sorting can be based on one ordinary (fast)
;; predicate like `<'.  Otherwise, a list of proced predicates can be used
;; to refine the sort.
;;
;; It would be neat if one could temporarily override the following
;; predefined rules.
(defvar proced-grammar-alist
  '( ;; attributes defined in `system-process-attributes'
    (euid    "EUID"    "%d" right proced-< nil (euid pid) (nil t nil))
    (user    "USER"    "%s" left proced-string-lessp nil (user pid) (nil t nil))
    (egid    "EGID"    "%d" right proced-< nil (egid euid pid) (nil t nil))
    (group   "GROUP"   "%s" left proced-string-lessp nil (group user pid) (nil t nil))
    (comm    "COMMAND" "%s" left proced-string-lessp nil (comm pid) (nil t nil))
    (state   "STAT"    "%s" left proced-string-lessp nil (state pid) (nil t nil))
    (ppid    "PPID"    "%d" right proced-< nil (ppid pid) (nil t nil))
    (pgrp    "PGRP"    "%d" right proced-< nil (pgrp euid pid) (nil t nil))
    (sess    "SESS"    "%d" right proced-< nil (sess pid) (nil t nil))
    (ttname  "TTY"     proced-format-ttname left proced-string-lessp nil (ttname pid) (nil t nil))
    (tpgid   "TPGID"   "%d" right proced-< nil (tpgid pid) (nil t nil))
    (minflt  "MINFLT"  "%d" right proced-< nil (minflt pid) (nil t t))
    (majflt  "MAJFLT"  "%d" right proced-< nil (majflt pid) (nil t t))
    (cminflt "CMINFLT" "%d" right proced-< nil (cminflt pid) (nil t t))
    (cmajflt "CMAJFLT" "%d" right proced-< nil (cmajflt pid) (nil t t))
    (utime   "UTIME"   proced-format-time right proced-time-lessp t (utime pid) (nil t t))
    (stime   "STIME"   proced-format-time right proced-time-lessp t (stime pid) (nil t t))
    (cutime  "CUTIME"  proced-format-time right proced-time-lessp t (cutime pid) (nil t t))
    (cstime  "CSTIME"  proced-format-time right proced-time-lessp t (cstime pid) (nil t t))
    (pri     "PR"      "%d" right proced-< t (pri pid) (nil t t))
    (nice    "NI"      "%3d" 3 proced-< t (nice pid) (t t nil))
    (thcount "THCOUNT" "%d" right proced-< t (thcount pid) (nil t t))
    (start   "START"   proced-format-start 6 proced-time-lessp nil (start pid) (t t nil))
    (vsize   "VSIZE"   "%d" right proced-< t (vsize pid) (nil t t))
    (rss     "RSS"     "%d" right proced-< t (rss pid) (nil t t))
    (etime   "ETIME"   proced-format-time right proced-time-lessp t (etime pid) (nil t t))
    (pcpu    "%CPU"    "%.1f" right proced-< t (pcpu pid) (nil t t))
    (pmem    "%MEM"    "%.1f" right proced-< t (pmem pid) (nil t t))
    (args    "ARGS"    "%s"   left proced-string-lessp nil (args pid) (nil t nil))
    ;;
    ;; attributes defined by proced (see `proced-process-attributes')
    (pid     "PID"     "%d" right proced-< nil (pid) (t t nil))
    ;; time: sum of utime and stime
    (time    "TIME"   proced-format-time right proced-time-lessp t (time pid) (nil t t))
    ;; ctime: sum of cutime and cstime
    (ctime   "CTIME"  proced-format-time right proced-time-lessp t (ctime pid) (nil t t)))
  "Alist of rules for handling Proced attributes.

Each element has the form

  (KEY NAME FORMAT JUSTIFY PREDICATE REVERSE SORT-SCHEME FILTER-SCHEME).

KEY is the car of a process attribute.

NAME appears in the header line.

FORMAT specifies the format for displaying the attribute values.
It is either a string passed to `format' or a function called with one
argument, the value of the attribute.

If JUSTIFY is an integer, its modulus gives the width of the attribute
vales formatted with FORMAT.  If JUSTIFY is positive, NAME appears
right-justified, otherwise it appears left-justified.  If JUSTIFY is 'left
or 'right, the field width is calculated from all field values in the listing.
If JUSTIFY is 'left, the field values are formatted left-justified and
right-justified otherwise.

PREDICATE is the predicate for sorting and filtering the process listing
based on attribute KEY.  PREDICATE takes two arguments P1 and P2,
the corresponding attribute values of two processes.  PREDICATE should
return 'equal if P1 has same rank like P2.  Any other non-nil value says
that P1 is \"less than\" P2, or nil if not.

REVERSE is non-nil if the sort order is opposite to the order defined
by PREDICATE.

SORT-SCHEME is a list (KEY1 KEY2 ...) defing a hierarchy of rules
for sorting the process listing.  KEY1, KEY2, ... are KEYs appearing as cars
of `proced-grammar-alist'.  First the PREDICATE of KEY1 is evaluated.
If it yields non-equal, it defines the sorting order for the corresponding
processes.  If it evaluates to 'equal the PREDICATE of KEY2 is evaluated, etc.

FILTER-SCHEME is a list (LESS-B EQUAL-B LARGER-B) used by the command
`proced-filter-attribute' for filtering KEY (see there).  This command
compares the value of attribute KEY of every process with the value
of attribute KEY of the process at the position of point using PREDICATE.
If PREDICATE yields non-nil, the process is accepted if LESS-B is non-nil.
If PREDICATE yields 'equal, the process is accepted if EQUAL-B is non-nil.
If PREDICATE yields nil, the process is accepted if LARGER-B is non-nil.")

(defvar proced-custom-attributes nil
  "List of functions defining custom attributes.
This variable extends the functionality of `proced-process-attributes'.
Each function is called with one argument, the list of attributes
of a system process.  It returns a cons cell of the form (KEY . VALUE)
like `system-process-attributes'.")

;; Formatting and sorting rules are defined "per attribute".  If formatting
;; and / or sorting should use more than one attribute, it appears more
;; transparent to define a new derived attribute, so that formatting and
;; sorting can use them consistently.  (Are there exceptions to this rule?
;; Would it be advantageous to have yet more general methods available?)
;; Sorting can also be based on attributes that are invisible in the listing.

(defvar proced-format-alist
  '((short user pid pcpu pmem start time args)
    (medium user pid pcpu pmem vsize rss ttname state start time args)
    (long user euid group pid pri nice pcpu pmem vsize rss ttname state
          start time args)
    (verbose user euid group egid pid ppid pgrp sess comm pri nice pcpu pmem
             state thcount vsize rss ttname tpgid minflt majflt cminflt cmajflt
             start time utime stime ctime cutime cstime etime args))
  "Alist of formats of listing.
The car of each element is a symbol, the name of the format.
The cdr is a list of keys appearing in `proced-grammar-alist'.")

(defvar proced-format 'short
  "Current format of Proced listing.
It can be the car of an element of `proced-format-alist'.
It can also be a list of keys appearing in `proced-grammar-alist'.")
(make-variable-buffer-local 'proced-format)

;; FIXME: is there a better name for filter `user' that does not coincide
;; with an attribute key?
(defvar proced-filter-alist
  `((user (user . ,(concat "\\`" (user-real-login-name) "\\'")))
    (user-running (user . ,(concat "\\`" (user-real-login-name) "\\'"))
                  (state . "\\`[Rr]\\'"))
    (all)
    (all-running (state . "\\`[Rr]\\'"))
    (emacs (fun-all . (lambda (list)
                        (proced-filter-children list ,(emacs-pid))))))
  "Alist of process filters.
The car of each element is a symbol, the name of the filter.
The cdr is a list of elementary filters that are applied to every process.
A process is displayed if it passes all elementary filters of a selected
filter.

An elementary filter can be one of the following:
\(KEY . REGEXP)   If value of attribute KEY matches REGEXP,
                 accept this process.
\(KEY . FUN)      Apply function FUN to attribute KEY.  Accept this process,
                 if FUN returns non-nil.
\(function . FUN) For each process, apply function FUN to list of attributes
                 of each.  Accept the process if FUN returns non-nil.
\(fun-all . FUN)  Apply function FUN to entire process list.
                 FUN must return the filtered list.")

(defvar proced-filter 'user
  "Current filter of proced listing.
It can be the car of an element of `proced-filter-alist'.
It can also be a list of elementary filters as in the cdrs of the elements
of `proced-filter-alist'.")
(make-variable-buffer-local 'proced-filter)

(defvar proced-sort 'pcpu
  "Current sorting scheme for proced listing.
It must be the KEY of an element of `proced-grammar-alist'.
It can also be a list of KEYs as in the SORT-SCHEMEs of the elements
of `proced-grammar-alist'.")
(make-variable-buffer-local 'proced-format)

(defcustom proced-goal-attribute 'args
  "If non-nil, key of the attribute that defines the `goal-column'."
  :group 'proced
  :type '(choice (const :tag "none" nil)
                 (symbol :tag "key")))

(defcustom proced-timer-interval 5
  "Time interval in seconds for updating Proced buffers."
  :group 'proced
  :type 'integer)

(defcustom proced-timer-flag nil
  "Non-nil for regular update of a Proced buffer.
Can be changed interactively via `proced-toggle-timer-flag'."
  :group 'proced
  :type 'boolean)
(make-variable-buffer-local 'proced-timer-flag)

;; Internal variables

(defvar proced-process-alist nil
  "Alist of PIDs displayed by Proced.")
(make-variable-buffer-local 'proced-process-alist)

(defvar proced-sort-internal nil
  "Sorting scheme for listing (internal format).")

(defvar proced-marker-char ?*		; the answer is 42
  "In proced, the current mark character.")

;; face and font-lock code taken from dired
(defgroup proced-faces nil
  "Faces used by Proced."
  :group 'proced
  :group 'faces)

(defface proced-mark
  '((t (:inherit font-lock-constant-face)))
  "Face used for proced marks."
  :group 'proced-faces)
(defvar proced-mark-face 'proced-mark
  "Face name used for proced marks.")

(defface proced-marked
  '((t (:inherit font-lock-warning-face)))
  "Face used for marked processes."
  :group 'proced-faces)
(defvar proced-marked-face 'proced-marked
  "Face name used for marked processes.")

(defvar proced-re-mark "^[^ \n]"
  "Regexp matching a marked line.
Important: the match ends just after the marker.")

(defvar proced-header-line nil
  "Headers in Proced buffer as a string.")
(make-variable-buffer-local 'proced-header-line)

(defvar proced-log-buffer "*Proced log*"
  "Name of Proced Log buffer.")

(defvar proced-process-tree nil
  "Process tree of listing (internal variable).")

(defvar proced-timer nil
  "Stores if Proced timer is already installed.")

(defconst proced-help-string
  "(n)ext, (p)revious, (m)ark, (u)nmark, (k)ill, (q)uit (type ? for more help)"
  "Help string for proced.")

(defconst proced-header-help-echo
  "mouse-2: sort by attribute %s%s"
  "Help string shown when mouse is over a sortable header.")

(defconst proced-field-help-echo
  "mouse-2, RET: filter by attribute %s %s"
  "Help string shown when mouse is over a filterable field.")

(defvar proced-font-lock-keywords
  (list
   ;;
   ;; Proced marks.
   (list proced-re-mark '(0 proced-mark-face))
   ;;
   ;; Marked files.
   (list (concat "^[" (char-to-string proced-marker-char) "]")
         '(".+" (proced-move-to-goal-column) nil (0 proced-marked-face)))))

(defvar proced-mode-map
  (let ((km (make-sparse-keymap)))
    ;; moving
    (define-key km " " 'proced-next-line)
    (define-key km "n" 'next-line)
    (define-key km "p" 'previous-line)
    (define-key km "\C-n" 'next-line)
    (define-key km "\C-p" 'previous-line)
    (define-key km "\C-?" 'previous-line)
    (define-key km [down] 'next-line)
    (define-key km [up] 'previous-line)
    ;; marking
    (define-key km "d" 'proced-mark) ; Dired compatibility ("delete")
    (define-key km "m" 'proced-mark)
    (define-key km "u" 'proced-unmark)
    (define-key km "\177" 'proced-unmark-backward)
    (define-key km "M" 'proced-mark-all)
    (define-key km "U" 'proced-unmark-all)
    (define-key km "t" 'proced-toggle-marks)
    (define-key km "C" 'proced-mark-children)
    (define-key km "P" 'proced-mark-parents)
    ;; filtering
    (define-key km "f"  'proced-filter-interactive)
    (define-key km [mouse-2] 'proced-filter-attribute)
    (define-key km "\C-m" 'proced-filter-attribute)
    ;; sorting
    (define-key km "sc" 'proced-sort-pcpu)
    (define-key km "sm" 'proced-sort-pmem)
    (define-key km "sp" 'proced-sort-pid)
    (define-key km "ss" 'proced-sort-start)
    (define-key km "sS" 'proced-sort-interactive)
    (define-key km "st" 'proced-sort-time)
    (define-key km "su" 'proced-sort-user)
    (define-key km [header-line mouse-2] 'proced-sort-header)
    ;; formatting
    (define-key km "F"  'proced-format-interactive)
    ;; operate
    (define-key km "o" 'proced-omit-processes)
    (define-key km "x" 'proced-send-signal) ; Dired compatibility
    (define-key km "k" 'proced-send-signal) ; kill processes
    ;; misc
    (define-key km "g" 'revert-buffer)  ; Dired compatibility
    (define-key km "h" 'describe-mode)
    (define-key km "?" 'proced-help)
    (define-key km "q" 'quit-window)
    (define-key km [remap undo] 'proced-undo)
    (define-key km [remap advertised-undo] 'proced-undo)
    km)
  "Keymap for proced commands.")

(easy-menu-define
  proced-menu proced-mode-map "Proced Menu"
  `("Proced"
    ["Mark" proced-mark
     :help "Mark Current Process"]
    ["Unmark" proced-unmark
     :help "Unmark Current Process"]
    ["Mark All" proced-mark-all
     :help "Mark All Processes"]
    ["Unmark All" proced-unmark-all
     :help "Unmark All Process"]
    ["Toggle Marks" proced-toggle-marks
     :help "Marked Processes Become Unmarked, and Vice Versa"]
    ["Mark Children" proced-mark-children
     :help "Mark Current Process and its Children"]
    ["Mark Parents" proced-mark-parents
     :help "Mark Current Process and its Parents"]
    "--"
    ("Filters"
     :help "Select Filter for Process Listing"
     ,@(mapcar (lambda (el)
                 (let ((filter (car el)))
                   `[,(symbol-name filter)
                     (proced-filter-interactive ',filter)
                     :style radio
                     :selected (eq proced-filter ',filter)]))
               proced-filter-alist))
    ("Sorting"
     :help "Select Sorting Scheme"
     ["Sort..." proced-sort-interactive
      :help "Sort Process List"]
     "--"
     ["Sort by %CPU" proced-sort-pcpu]
     ["Sort by %MEM" proced-sort-pmem]
     ["Sort by PID" proced-sort-pid]
     ["Sort by START" proced-sort-start]
     ["Sort by TIME" proced-sort-time]
     ["Sort by USER" proced-sort-user])
    ("Formats"
     :help "Select Format for Process Listing"
     ,@(mapcar (lambda (el)
                 (let ((format (car el)))
                   `[,(symbol-name format)
                     (proced-format-interactive ',format)
                     :style radio
                     :selected (eq proced-format ',format)]))
               proced-format-alist))
    "--"
    ["Omit Marked Processes" proced-omit-processes
     :help "Omit Marked Processes in Process Listing."]
    "--"
    ["Revert" revert-buffer
     :help "Revert Process Listing"]
    ["Regular Update" proced-toggle-timer-flag
     :style radio
     :selected (eval proced-timer-flag)
     :help "Regular Update of Proced buffer"]
    ["Send signal" proced-send-signal
     :help "Send Signal to Marked Processes"]))

;; helper functions
(defun proced-marker-regexp ()
  "Return regexp matching `proced-marker-char'."
  ;; `proced-marker-char' must appear in column zero
  (concat "^" (regexp-quote (char-to-string proced-marker-char))))

(defun proced-success-message (action count)
  "Display success message for ACTION performed for COUNT processes."
  (message "%s %s process%s" action count (if (= 1 count) "" "es")))

;; Unlike dired, we do not define our own commands for vertical motion.
;; If `goal-column' is set, `next-line' and `previous-line' are fancy
;; commands to satisfy our modest needs.  If `proced-goal-attribute'
;; and/or `goal-column' are not set, `next-line' and `previous-line'
;; are really what we need to preserve the column of point.
;; We use `proced-move-to-goal-column' for "non-interactive" cases only
;; to get a well-defined position of point.

(defun proced-move-to-goal-column ()
  "Move to `goal-column' if non-nil."
  (beginning-of-line)
  (unless (eobp)
    (if goal-column
        (forward-char goal-column)
      (forward-char 2))))

(defun proced-header-line ()
  "Return header line for Proced buffer."
  (list (propertize " " 'display '(space :align-to 0))
        (replace-regexp-in-string ;; preserve text properties
         "\\(%\\)" "\\1\\1" (substring proced-header-line (window-hscroll)))))

(defun proced-pid-at-point ()
  "Return pid of system process at point.
Return nil if point is not on a process line."
  (save-excursion
    (beginning-of-line)
    (if (looking-at "^. .")
        (get-text-property (match-end 0) 'proced-pid))))

;; proced mode

(define-derived-mode proced-mode nil "Proced"
  "Mode for displaying UNIX system processes and sending signals to them.
Type \\<proced-mode-map>\\[proced-mark] to mark a process for later commands.
Type \\[proced-send-signal] to send signals to marked processes.

\\{proced-mode-map}"
  (abbrev-mode 0)
  (auto-fill-mode 0)
  (setq buffer-read-only t
        truncate-lines t
        header-line-format '(:eval (proced-header-line)))
  (add-hook 'post-command-hook 'force-mode-line-update nil t)
  (set (make-local-variable 'revert-buffer-function) 'proced-revert)
  (set (make-local-variable 'font-lock-defaults)
       '(proced-font-lock-keywords t nil nil beginning-of-line))
  (if (and (not proced-timer) proced-timer-interval)
      (setq proced-timer
            (run-at-time t proced-timer-interval 'proced-timer))))

;; Proced mode is suitable only for specially formatted data.
(put 'proced-mode 'mode-class 'special)

;;;###autoload
(defun proced (&optional arg)
  "Mode for displaying UNIX system processes and sending signals to them.
Type \\<proced-mode-map>\\[proced-mark] to mark a process for later commands.
Type \\[proced-send-signal] to send signals to marked processes.

If invoked with optional ARG the window displaying the process
information will be displayed but not selected.

\\{proced-mode-map}"
  (interactive "P")
  (let ((buffer (get-buffer-create "*Proced*")) new)
    (set-buffer buffer)
    (setq new (zerop (buffer-size)))
    (if new (proced-mode))
    (if (or new arg)
        (proced-update t))
    (if arg
	(display-buffer buffer)
      (pop-to-buffer buffer)
      (message
       (substitute-command-keys
        "Type \\<proced-mode-map>\\[quit-window] to quit, \\[proced-help] for help")))))

(defun proced-timer ()
  "Update Proced buffers regularly using `run-at-time'."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (if (and (eq major-mode 'proced-mode)
               proced-timer-flag)
          (proced-update t t)))))

(defun proced-toggle-timer-flag (arg)
  "Change whether this Proced buffer is updated regularly.
With prefix ARG, update this buffer regularly if ARG is positive,
otherwise do not update.  Sets the variable `proced-timer-flag'.
The time interval for updates is specified via `proced-timer-interval'."
  (interactive (list (or current-prefix-arg 'toggle)))
  (setq proced-timer-flag
        (cond ((eq arg 'toggle) (not proced-timer-flag))
              (arg (> (prefix-numeric-value arg) 0))
              (t (not proced-timer-flag))))
  (message "`proced-timer-flag' set to %s" proced-timer-flag))

(defun proced-mark (&optional count)
  "Mark the current (or next COUNT) processes."
  (interactive "p")
  (proced-do-mark t count))

(defun proced-unmark (&optional count)
  "Unmark the current (or next COUNT) processes."
  (interactive "p")
  (proced-do-mark nil count))

(defun proced-unmark-backward (&optional count)
  "Unmark the previous (or COUNT previous) processes."
  ;; Analogous to `dired-unmark-backward',
  ;; but `ibuffer-unmark-backward' behaves different.
  (interactive "p")
  (proced-do-mark nil (- (or count 1))))

(defun proced-do-mark (mark &optional count)
  "Mark the current (or next COUNT) processes using MARK."
  (or count (setq count 1))
  (let ((backward (< count 0))
	buffer-read-only)
    (setq count (1+ (if (<= 0 count) count
                      (min (1- (line-number-at-pos)) (abs count)))))
    (beginning-of-line)
    (while (not (or (zerop (setq count (1- count))) (eobp)))
      (proced-insert-mark mark backward))
    (proced-move-to-goal-column)))

(defun proced-mark-all ()
  "Mark all processes.
If `transient-mark-mode' is turned on and the region is active,
mark the region."
  (interactive)
  (proced-do-mark-all t))

(defun proced-unmark-all ()
  "Unmark all processes.
If `transient-mark-mode' is turned on and the region is active,
unmark the region."
  (interactive)
  (proced-do-mark-all nil))

(defun proced-do-mark-all (mark)
  "Mark all processes using MARK.
If `transient-mark-mode' is turned on and the region is active,
mark the region."
  (let ((count 0) end buffer-read-only)
    (save-excursion
      (if (use-region-p)
          ;; Operate even on those lines that are only partially a part
          ;; of region.  This appears most consistent with
          ;; `proced-move-to-goal-column'.
          (progn (setq end (save-excursion
                             (goto-char (region-end))
                             (unless (looking-at "^") (forward-line))
                             (point)))
                 (goto-char (region-beginning))
                 (unless (looking-at "^") (beginning-of-line)))
        (goto-char (point-min))
        (setq end (point-max)))
      (while (< (point) end)
        (setq count (1+ count))
        (proced-insert-mark mark))
      (proced-success-message "Marked" count))))

(defun proced-toggle-marks ()
  "Toggle marks: marked processes become unmarked, and vice versa."
  (interactive)
  (let ((mark-re (proced-marker-regexp))
        buffer-read-only)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (cond ((looking-at mark-re)
               (proced-insert-mark nil))
              ((looking-at " ")
               (proced-insert-mark t))
              (t
               (forward-line 1)))))))

(defun proced-insert-mark (mark &optional backward)
  "If MARK is non-nil, insert `proced-marker-char'.
If BACKWARD is non-nil, move one line backwards before inserting the mark.
Otherwise move one line forward after inserting the mark."
  (if backward (forward-line -1))
  (insert (if mark proced-marker-char ?\s))
  (delete-char 1)
  (unless backward (forward-line)))

(defun proced-mark-children (ppid &optional omit-ppid)
  "Mark child processes of process PPID.
Also mark process PPID unless prefix OMIT-PPID is non-nil."
  (interactive (list (proced-pid-at-point) current-prefix-arg))
  (proced-mark-process-alist
   (proced-filter-children proced-process-alist ppid omit-ppid)))

(defun proced-mark-parents (cpid &optional omit-cpid)
  "Mark parent processes of process CPID.
Also mark CPID unless prefix OMIT-CPID is non-nil."
  (interactive (list (proced-pid-at-point) current-prefix-arg))
  (proced-mark-process-alist
   (proced-filter-parents proced-process-alist cpid omit-cpid)))

(defun proced-mark-process-alist (process-alist &optional quiet)
  (let ((count 0))
    (if process-alist
        (let (buffer-read-only)
          (save-excursion
            (goto-char (point-min))
            (while (not (eobp))
              (when (assq (proced-pid-at-point) process-alist)
                (insert proced-marker-char)
                (delete-char 1)
                (setq count (1+ count)))
              (forward-line)))))
    (unless quiet
      (proced-success-message "Marked" count))))

;; Mostly analog of `dired-do-kill-lines'.
;; However, for negative args the target lines of `dired-do-kill-lines'
;; include the current line, whereas `dired-mark' for negative args operates
;; on the preceding lines.  Here we are consistent with `dired-mark'.
(defun proced-omit-processes (&optional arg quiet)
  "Omit marked processes.
With prefix ARG, omit that many lines starting with the current line.
\(A negative argument omits backward.)
If `transient-mark-mode' is turned on and the region is active,
omit the processes in region.
If QUIET is non-nil suppress status message.
Returns count of omitted lines."
  (interactive "P")
  (let ((mark-re (proced-marker-regexp))
        (count 0)
        buffer-read-only)
    (cond ((use-region-p) ;; Omit active region
           (let ((lines (count-lines (region-beginning) (region-end))))
             (save-excursion
               (goto-char (region-beginning))
               (while (< count lines)
                 (proced-omit-process)
                 (setq count (1+ count))))))
          ((not arg) ;; Omit marked lines
           (save-excursion
             (goto-char (point-min))
             (while (and (not (eobp))
                         (re-search-forward mark-re nil t))
               (proced-omit-process)
               (setq count (1+ count)))))
          ((< 0 arg) ;; Omit forward
           (while (and (not (eobp)) (< count arg))
             (proced-omit-process)
             (setq count (1+ count))))
          ((< arg 0) ;; Omit backward
           (while (and (not (bobp)) (< count (- arg)))
             (forward-line -1)
             (proced-omit-process)
             (setq count (1+ count)))))
    (unless (zerop count) (proced-move-to-goal-column))
    (unless quiet (proced-success-message "Omitted" count))
    count))

(defun proced-omit-process ()
  "Omit process from listing point is on.
Update `proced-process-alist' accordingly."
  (setq proced-process-alist
        (assq-delete-all (proced-pid-at-point) proced-process-alist))
  (delete-region (line-beginning-position)
                 (save-excursion (forward-line) (point))))

;;; Filtering

(defun proced-filter (process-alist filter-list)
  "Apply FILTER-LIST to PROCESS-ALIST."
  (if (symbolp filter-list)
      (setq filter-list (cdr (assq filter-list proced-filter-alist))))
  (dolist (filter filter-list)
    (let (new-alist)
      (cond ( ;; apply function to entire process list
             (eq (car filter) 'fun-all)
             (setq new-alist (funcall (cdr filter) process-alist)))
            ( ;; apply predicate to each list of attributes
             (eq (car filter) 'function)
             (dolist (process process-alist)
               (if (funcall (car filter) (cdr process))
                   (push process new-alist))))
            (t ;; apply predicate to specified attribute
             (let ((fun (if (stringp (cdr filter))
                            `(lambda (val)
                               (string-match ,(cdr filter) val))
                          (cdr filter)))
                   value)
               (dolist (process process-alist)
                 (setq value (cdr (assq (car filter) (cdr process))))
                 (if (and value (funcall fun value))
                     (push process new-alist))))))
      (setq process-alist new-alist)))
  process-alist)

(defun proced-filter-interactive (scheme &optional revert)
  "Filter Proced buffer using SCHEME.
When called interactively, an empty string means nil, i.e., no filtering.
With prefix REVERT non-nil revert listing."
  (interactive
   (let ((scheme (completing-read "Filter: "
                                  proced-filter-alist nil t)))
     (list (if (string= "" scheme) nil (intern scheme))
           current-prefix-arg)))
  (setq proced-filter scheme)
  (proced-update revert))

(defun proced-process-tree (process-alist)
  "Return process tree for PROCESS-ALIST.
The process tree is an alist with elements (PPID PID1 PID2 ...).
PPID is a parent PID.  PID1, PID2, ... are the child processes of PPID.
The list of children does not include grandchildren."
  (let (children-list ppid cpids)
    (dolist (process process-alist children-list)
      (setq ppid (cdr (assq 'ppid (cdr process))))
      (if ppid
          (setq children-list
                (if (setq cpids (assq ppid children-list))
                    (cons (cons ppid (cons (car process) (cdr cpids)))
                          (assq-delete-all ppid children-list))
                  (cons (list ppid (car process))
                        children-list)))))))

(defun proced-filter-children (process-alist ppid &optional omit-ppid)
  "For PROCESS-ALIST return list of child processes of PPID.
This list includes PPID unless OMIT-PPID is non-nil."
  (let ((proced-process-tree (proced-process-tree process-alist))
        new-alist)
    (dolist (pid (proced-children-pids ppid))
      (push (assq pid process-alist) new-alist))
    (if omit-ppid
        (assq-delete-all ppid new-alist)
      new-alist)))

;; helper function
(defun proced-children-pids (ppid)
  "Return list of children PIDs of PPID (including PPID)."
  (let ((cpids (cdr (assq ppid proced-process-tree))))
    (if cpids
        (cons ppid (apply 'append (mapcar 'proced-children-pids cpids)))
      (list ppid))))

(defun proced-filter-parents (process-alist pid &optional omit-pid)
  "For PROCESS-ALIST return list of parent processes of PID.
This list includes CPID unless OMIT-CPID is non-nil."
  (let ((parent-list (unless omit-pid (list (assq pid process-alist)))))
    (while (setq pid (cdr (assq 'ppid (cdr (assq pid process-alist)))))
      (push (assq pid process-alist) parent-list))
    parent-list))

(defun proced-filter-attribute (&optional event)
  "Filter Proced listing based on the attribute at point.
Optional EVENT is the location of the Proced field."
  (interactive (list last-input-event))
  (if event (posn-set-point (event-end event)))
  (let ((key (get-text-property (point) 'proced-key))
        (pid (get-text-property (point) 'proced-pid)))
    (if (and key pid)
        (let* ((grammar (assq key proced-grammar-alist))
               (predicate (nth 4 grammar))
               (filter (nth 7 grammar))
               (ref (cdr (assq key (cdr (assq pid proced-process-alist)))))
               val new-alist)
          (when ref
            (dolist (process proced-process-alist)
              (setq val (funcall predicate (cdr (assq key (cdr process))) ref))
              (if (cond ((not val) (nth 2 filter))
                        ((eq val 'equal) (nth 1 filter))
                        (val (car filter)))
                  (push process new-alist)))
            (setq proced-process-alist new-alist)
            (proced-update)))
      (message "No filter defined here."))))

;; Proced predicates for sorting and filtering are based on a three-valued
;; logic:
;; Predicates takes two arguments P1 and P2, the corresponding attribute
;; values of two processes.  Predicate should return 'equal if P1 has
;; same rank like P2.  Any other non-nil value says that P1 is "less than" P2,
;; or nil if not.

(defun proced-< (num1 num2)
  "Return t if NUM1 less than NUM2.
Return `equal' if NUM1 equals NUM2.  Return nil if NUM1 greater than NUM2."
  (if (= num1 num2)
      'equal
    (< num1 num2)))

(defun proced-string-lessp (s1 s2)
  "Return t if string S1 is less than S2 in lexicographic order.
Return `equal' if S1 and S2 have identical contents.
Return nil otherwise."
  (if (string= s1 s2)
      'equal
    (string-lessp s1 s2)))

(defun proced-time-lessp (t1 t2)
  "Return t if time value T1 is less than time value T2.
Return `equal' if T1 equals T2.  Return nil otherwise."
  (with-decoded-time-value ((high1 low1 micro1 t1)
			    (high2 low2 micro2 t2))
    (cond ((< high1 high2))
          ((< high2 high1) nil)
          ((< low1 low2))
          ((< low2 low1) nil)
          ((< micro1 micro2))
          ((< micro2 micro1) nil)
          (t 'equal))))

;;; Sorting

(defsubst proced-xor (b1 b2)
  "Return the logical exclusive or of args B1 and B2."
  (and (or b1 b2)
       (not (and b1 b2))))

(defun proced-sort-p (p1 p2)
  "Predicate for sorting processes P1 and P2."
  (if (not (cdr proced-sort-internal))
      ;; only one predicate: fast scheme
      (let* ((sorter (car proced-sort-internal))
             (k1 (cdr (assq (car sorter) (cdr p1))))
             (k2 (cdr (assq (car sorter) (cdr p2)))))
        ;; if the attributes are undefined, we should really abort sorting
        (if (and k1 k2)
            (proced-xor (funcall (nth 1 sorter) k1 k2)
                        (nth 2 sorter))))
    (let ((sort-list proced-sort-internal) sorter predicate k1 k2)
      (catch 'done
        (while (setq sorter (pop sort-list))
          (setq k1 (cdr (assq (car sorter) (cdr p1)))
                k2 (cdr (assq (car sorter) (cdr p2)))
                predicate
                (if (and k1 k2)
                    (funcall (nth 1 sorter) k1 k2)))
          (if (not (eq predicate 'equal))
              (throw 'done (proced-xor predicate (nth 2 sorter)))))
        (eq t predicate)))))

(defun proced-sort (process-alist sorter)
  "Sort PROCESS-ALIST using scheme SORTER.
Return sorted process list."
  ;; translate SORTER into a list of lists (KEY PREDICATE REVERSE)
  (setq proced-sort-internal
        (mapcar (lambda (arg)
                  (let ((grammar (assq arg proced-grammar-alist)))
                    (list arg (nth 4 grammar) (nth 5 grammar))))
                (cond ((listp sorter) sorter)
                      ((and (symbolp sorter)
                            (nth 6 (assq sorter proced-grammar-alist))))
                      ((symbolp sorter) (list sorter))
                      (t (error "Sorter undefined %s" sorter)))))
  (if proced-sort-internal
      (sort process-alist 'proced-sort-p)
    process-alist))

(defun proced-sort-interactive (scheme &optional revert)
  "Sort Proced buffer using SCHEME.
When called interactively, an empty string means nil, i.e., no sorting.
With prefix REVERT non-nil revert listing."
  (interactive
   (let ((scheme (completing-read "Sorting type: "
                                  proced-grammar-alist nil t)))
     (list (if (string= "" scheme) nil (intern scheme))
           current-prefix-arg)))
  (setq proced-sort scheme)
  (proced-update revert))

(defun proced-sort-pcpu (&optional revert)
  "Sort Proced buffer by percentage CPU time (%CPU)."
  (interactive "P")
  (proced-sort-interactive 'pcpu revert))

(defun proced-sort-pmem (&optional revert)
  "Sort Proced buffer by percentage memory usage (%MEM)."
  (interactive "P")
  (proced-sort-interactive 'pmem))

(defun proced-sort-pid (&optional revert)
  "Sort Proced buffer by PID."
  (interactive "P")
  (proced-sort-interactive 'pid revert))

(defun proced-sort-start (&optional revert)
  "Sort Proced buffer by time the command started (START)."
  (interactive "P")
  (proced-sort-interactive 'start revert))

(defun proced-sort-time (&optional revert)
  "Sort Proced buffer by CPU time (TIME)."
  (interactive "P")
  (proced-sort-interactive 'time revert))

(defun proced-sort-user (&optional revert)
  "Sort Proced buffer by USER."
  (interactive "P")
  (proced-sort-interactive 'user revert))

(defun proced-sort-header (event &optional revert)
  "Sort Proced listing based on an attribute.
EVENT is a mouse event with starting position in the header line.
It is converted in the corresponding attribute key."
  (interactive "e\nP")
  (let ((start (event-start event))
        col key)
    (save-selected-window
      (select-window (posn-window start))
      (setq col (+ (1- (car (posn-col-row start)))
                   (window-hscroll)))
      (when (and (<= 0 col) (< col (length proced-header-line)))
        (setq key (get-text-property col 'proced-key proced-header-line))
        (if key
            (proced-sort-interactive key revert)
          (message "No sorter defined here."))))))

;;; Formating

(defun proced-format-time (time)
  "Format time intervall TIME."
  (let* ((ftime (float-time time))
         (days (truncate ftime 86400))
         (ftime (mod ftime 86400))
         (hours (truncate ftime 3600))
         (ftime (mod ftime 3600))
         (minutes (truncate ftime 60))
         (seconds (mod ftime 60)))
    (cond ((< 0 days)
           (format "%d-%02d:%02d:%02d" days hours minutes seconds))
          ((< 0 hours)
           (format "%02d:%02d:%02d" hours minutes seconds))
          (t
           (format "%02d:%02d" minutes seconds)))))

(defun proced-format-start (start)
  "Format time START.
The return string is always 6 characters wide."
  (let ((d-start (decode-time start))
        (d-current (decode-time)))
    (cond ( ;; process started in previous years
           (< (nth 5 d-start) (nth 5 d-current))
           (format-time-string "  %Y" start))
          ;; process started today
          ((and (= (nth 3 d-start) (nth 3 d-current))
                (= (nth 4 d-start) (nth 4 d-current)))
           (format-time-string " %H:%M" start))
          (t ;; process started this year
           (format-time-string "%b %e" start)))))

(defun proced-format-ttname (ttname)
  "Format attribute TTNAME, omitting prefix \"/dev/\"."
  ;; Does this work for all systems?
  (format "%s" (substring ttname
                          (if (string-match "\\`/dev/" ttname)
                              (match-end 0) 0))))

(defun proced-format (process-alist format)
  "Display PROCESS-ALIST using FORMAT."
  (if (symbolp format)
      (setq format (cdr (assq format proced-format-alist))))
  (insert (make-string (length process-alist) ?\n))
  (let ((whitespace " ") header-list grammar)
    ;; Loop over all attributes
    (while (setq grammar (pop format))
      (if (symbolp grammar)
          (setq grammar (assq grammar proced-grammar-alist)))
      (let* ((key (car grammar))
             (fun (if (stringp (nth 2 grammar))
                      `(lambda (arg) (format ,(nth 2 grammar) arg))
                    (nth 2 grammar)))
             (whitespace (if format whitespace ""))
             ;; Text properties:
             ;; We use the text property `proced-key' to store in each
             ;; field the corresponding key.
             ;; Of course, the sort predicate appearing in help-echo
             ;; is only part of the story.  But it gives the main idea.
             (hprops `(proced-key ,key mouse-face highlight
                                  help-echo ,(format proced-header-help-echo
                                                     (if (nth 5 grammar) "-" "+")
                                                     (nth 1 grammar))))
             (fprops `(proced-key ,key mouse-face highlight
                                  help-echo ,(format proced-field-help-echo
                                                     (nth 1 grammar)
                                                     (mapconcat (lambda (s)
                                                                  (if s "+" "-"))
                                                                (nth 7 grammar) ""))))
             value)

        (goto-char (point-min))
        (cond ( ;; fixed width of output field
               (numberp (nth 3 grammar))
               (dolist (process process-alist)
                 (end-of-line)
                 (setq value (cdr (assq key (cdr process))))
                 (insert (if value
                             (apply 'propertize (funcall fun value) fprops)
                           (make-string (abs (nth 3 grammar)) ?\s))
                         whitespace)
                 (forward-line))
               (push (format (concat "%" (number-to-string (nth 3 grammar)) "s")
                             (apply 'propertize (nth 1 grammar) hprops))
                     header-list))

              ( ;; last field left-justified
               (and (not format) (eq 'left (nth 3 grammar)))
               (dolist (process process-alist)
                 (end-of-line)
                 (setq value (cdr (assq key (cdr process))))
                 (if value (insert (apply 'propertize (funcall fun value) fprops)))
                 (forward-line))
               (push (apply 'propertize (nth 1 grammar) hprops) header-list))

              (t ;; calculated field width
               (let ((width (length (nth 1 grammar)))
                     field-list value)
                 (dolist (process process-alist)
                   (setq value (cdr (assq key (cdr process))))
                   (if value
                       (setq value (apply 'propertize (funcall fun value) fprops)
                             width (max width (length value))
                             field-list (cons value field-list))
                     (push "" field-list)))
                 (let ((afmt (concat "%" (if (eq 'left (nth 3 grammar)) "-" "")
                                     (number-to-string width) "s")))
                   (push (format afmt (apply 'propertize (nth 1 grammar) hprops))
                         header-list)
                   (dolist (value (nreverse field-list))
                     (end-of-line)
                     (insert (format afmt value) whitespace)
                     (forward-line))))))))

    ;; final cleanup
    (goto-char (point-min))
    (dolist (process process-alist)
      ;; We use the text property `proced-pid' to store in each line
      ;; the corresponding pid
      (put-text-property (point) (line-end-position) 'proced-pid (car process))
      (forward-line))
    ;; Set header line
    (setq proced-header-line
          (mapconcat 'identity (nreverse header-list) whitespace))
    (if (string-match "[ \t]+$" proced-header-line)
        (setq proced-header-line (substring proced-header-line 0
                                            (match-beginning 0))))
    ;; (delete-trailing-whitespace)
    (goto-char (point-min))
    (while (re-search-forward "[ \t\r]+$" nil t)
      (delete-region (match-beginning 0) (match-end 0)))))

(defun proced-format-interactive (scheme &optional revert)
  "Format Proced buffer using SCHEME.
When called interactively, an empty string means nil, i.e., no formatting.
With prefix REVERT non-nil revert listing."
  (interactive
   (let ((scheme (completing-read "Format: "
                                  proced-format-alist nil t)))
     (list (if (string= "" scheme) nil (intern scheme))
           current-prefix-arg)))
  (setq proced-format scheme)
  (proced-update revert))

;; generate listing

(defun proced-process-attributes ()
  "Return alist of attributes for each system process.
This alist can be customized via `proced-custom-attributes'."
  (mapcar (lambda (pid)
            (let* ((attributes (system-process-attributes pid))
                   (utime (cdr (assq 'utime attributes)))
                   (stime (cdr (assq 'stime attributes)))
                   (cutime (cdr (assq 'cutime attributes)))
                   (cstime (cdr (assq 'cstime attributes))))
              (setq attributes
                    (append (list (cons 'pid pid))
                            (if (and utime stime)
                                (list (cons 'time (time-add utime stime))))
                            (if (and cutime cstime)
                                (list (cons 'ctime (time-add cutime cstime))))
                            attributes))
              (dolist (fun proced-custom-attributes)
                (push (funcall fun attributes) attributes))
              (cons pid attributes)))
          (list-system-processes)))

(defun proced-update (&optional revert quiet)
  "Update the `proced' process information.  Preserves point and marks.
With prefix REVERT non-nil, revert listing.
Suppress status information if QUIET is nil."
  ;; This is the main function that generates and updates the process listing.
  (interactive "P")
  (setq revert (or revert (not proced-process-alist)))
  (or quiet (message (if revert "Updating process information..."
                       "Updating process display...")))
  ;; If point is on a field, we try to return point to that field.
  ;; Otherwise we try to return to the same column
  (let ((old-pos (let ((key (get-text-property (point) 'proced-key)))
                   (list (proced-pid-at-point) key
                         (if key
                             (if (get-text-property (1- (point)) 'proced-key)
                                 (- (point) (previous-single-property-change
                                             (point) 'proced-key))
                               0)
                           (current-column)))))
        buffer-read-only mp-list)
    ;; remember marked processes (whatever the mark was)
    (goto-char (point-min))
    (while (re-search-forward "^\\(\\S-\\)" nil t)
      (push (cons (save-match-data (proced-pid-at-point))
                  (match-string-no-properties 1)) mp-list))
    (when revert
      ;; all attributes of all processes
      (setq proced-process-alist (proced-process-attributes))
      ;; do not keep undo information
      (if (consp buffer-undo-list)
	  (setq buffer-undo-list nil)))
    ;; filtering and sorting
    (setq proced-process-alist
          (proced-sort (proced-filter proced-process-alist
                                      proced-filter) proced-sort))
    ;; generate listing
    (erase-buffer)
    (proced-format proced-process-alist proced-format)
    (goto-char (point-min))
    (while (not (eobp))
      (insert "  ")
      (forward-line))
    (setq proced-header-line (concat "  " proced-header-line))
    (if revert (set-buffer-modified-p nil))
    ;; set `goal-column'
    (let ((grammar (assq proced-goal-attribute proced-grammar-alist)))
      (setq goal-column ;; set to nil if no match
            (if (and grammar
                     (not (zerop (buffer-size)))
                     (string-match (regexp-quote (nth 1 grammar))
                                   proced-header-line))
                (if (nth 3 grammar)
                    (match-beginning 0)
                  (match-end 0)))))
    ;; restore process marks and buffer position (if possible)
    (goto-char (point-min))
    (if (or mp-list old-pos)
        (let (pid mark new-pos)
          (while (not (eobp))
            (setq pid (proced-pid-at-point))
            (when (setq mark (assq pid mp-list))
              (insert (cdr mark))
              (delete-char 1)
              (beginning-of-line))
            (when (eq (car old-pos) pid)
              (if (nth 1 old-pos)
                  (let ((limit (line-end-position)) pos)
                    (while (and (not new-pos)
                                (setq pos (next-property-change (point) nil limit)))
                      (goto-char pos)
                      (when (eq (nth 1 old-pos)
                                (get-text-property (point) 'proced-key))
                        (forward-char (min (nth 2 old-pos)
                                           (- (next-property-change (point))
                                              (point))))
                        (setq new-pos (point))))
                    (unless new-pos
                      (setq new-pos (if goal-column
                                        (+ (line-beginning-position) goal-column)
                                      (line-beginning-position)))))
                (setq new-pos (min (+ (line-beginning-position) (nth 2 old-pos))
                                   (line-end-position)))))
            (forward-line))
          (if new-pos
              (goto-char new-pos)
            (proced-move-to-goal-column)))
      (proced-move-to-goal-column))
    ;; update modeline
    ;; Does the long mode-name clutter the modeline?
    (setq mode-name
          (concat "Proced"
                  (if proced-filter
                      (concat ": " (symbol-name proced-filter))
                    "")
                  (if proced-sort
                      (let* ((key (if (listp proced-sort) (car proced-sort)
                                    proced-sort))
                             (grammar (assq key proced-grammar-alist)))
                        (concat " by " (if (nth 5 grammar) "-" "+")
                                (nth 1 grammar)))
                    "")))
    (force-mode-line-update)
    ;; done
    (or quiet (input-pending-p)
        (message (if revert "Updating process information...done."
                   "Updating process display...done.")))))

(defun proced-revert (&rest args)
  "Analog of `revert-buffer'."
  (proced-update t))

;; I do not want to reinvent the wheel.  Should we rename `dired-pop-to-buffer'
;; and move it to window.el so that proced and ibuffer can easily use it, too?
;; What about functions like `appt-disp-window' that use
;; `shrink-window-if-larger-than-buffer'?
(autoload 'dired-pop-to-buffer "dired")

(defun proced-send-signal (&optional signal)
  "Send a SIGNAL to the marked processes.
If no process is marked, operate on current process.
SIGNAL may be a string (HUP, INT, TERM, etc.) or a number.
If SIGNAL is nil display marked processes and query interactively for SIGNAL."
  (interactive)
  (let ((regexp (proced-marker-regexp))
        process-alist)
    ;; collect marked processes
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (push (cons (proced-pid-at-point)
                    ;; How much info should we collect here?
                    (substring (match-string-no-properties 0) 2))
              process-alist)))
    (setq process-alist
          (if process-alist
              (nreverse process-alist)
            ;; take current process
            (list (cons (proced-pid-at-point)
                        (buffer-substring-no-properties
                         (+ 2 (line-beginning-position))
                         (line-end-position))))))
    (unless signal
      ;; Display marked processes (code taken from `dired-mark-pop-up').
      (let ((bufname  " *Marked Processes*")
            (header-line (substring-no-properties proced-header-line)))
        (with-current-buffer (get-buffer-create bufname)
          (setq truncate-lines t
                proced-header-line header-line ; inherit header line
                header-line-format '(:eval (proced-header-line)))
          (add-hook 'post-command-hook 'force-mode-line-update nil t)
          (erase-buffer)
          (dolist (process process-alist)
            (insert "  " (cdr process) "\n"))
          (save-window-excursion
            (dired-pop-to-buffer bufname) ; all we need
            (let* ((completion-ignore-case t)
                   (pnum (if (= 1 (length process-alist))
                             "1 process"
                           (format "%d processes" (length process-alist))))
                   ;; The following is an ugly hack.  Is there a better way
                   ;; to help people like me to remember the signals and
                   ;; their meanings?
                   (tmp (completing-read (concat "Send signal [" pnum
                                                 "] (default TERM): ")
                                         proced-signal-list
                                         nil nil nil nil "TERM")))
              (setq signal (if (string-match "^\\(\\S-+\\)\\s-" tmp)
                               (match-string 1 tmp) tmp))))))
      ;; send signal
      (let ((count 0)
            failures)
        ;; Why not always use `signal-process'?  See
        ;; http://lists.gnu.org/archive/html/emacs-devel/2008-03/msg02955.html
        (if (functionp proced-signal-function)
            ;; use built-in `signal-process'
            (let ((signal (if (stringp signal)
                              (if (string-match "\\`[0-9]+\\'" signal)
                                  (string-to-number signal)
                                (make-symbol signal))
                            signal)))   ; number
              (dolist (process process-alist)
                (condition-case err
                    (if (zerop (funcall
                                proced-signal-function (car process) signal))
                        (setq count (1+ count))
                      (proced-log "%s\n" (cdr process))
                      (push (cdr process) failures))
                  (error ;; catch errors from failed signals
                   (proced-log "%s\n" err)
                   (proced-log "%s\n" (cdr process))
                   (push (cdr process) failures)))))
          ;; use external system call
          (let ((signal (concat "-" (if (numberp signal)
                                        (number-to-string signal) signal))))
            (dolist (process process-alist)
              (with-temp-buffer
                (condition-case err
                    (if (zerop (call-process
                                proced-signal-function nil t nil
                                signal (number-to-string (car process))))
                        (setq count (1+ count))
                      (proced-log (current-buffer))
                      (proced-log "%s\n" (cdr process))
                      (push (cdr process) failures))
                  (error ;; catch errors from failed signals
                   (proced-log (current-buffer))
                   (proced-log "%s\n" (cdr process))
                   (push (cdr process) failures)))))))
        (if failures
            ;; Proced error message are not always very precise.
            ;; Can we issue a useful one-line summary in the
            ;; message area (using FAILURES) if only one signal failed?
            (proced-log-summary
             signal
             (format "%d of %d signal%s failed"
                     (length failures) (length process-alist)
                     (if (= 1 (length process-alist)) "" "s")))
          (proced-success-message "Sent signal to" count)))
      ;; final clean-up
      (run-hooks 'proced-after-send-signal-hook))))

;; similar to `dired-why'
(defun proced-why ()
  "Pop up a buffer with error log output from Proced.
A group of errors from a single command ends with a formfeed.
Thus, use \\[backward-page] to find the beginning of a group of errors."
  (interactive)
  (if (get-buffer proced-log-buffer)
      (save-selected-window
        ;; move `proced-log-buffer' to the front of the buffer list
        (select-window (display-buffer (get-buffer proced-log-buffer)))
        (setq truncate-lines t)
        (set-buffer-modified-p nil)
        (setq buffer-read-only t)
        (goto-char (point-max))
        (forward-line -1)
        (backward-page 1)
        (recenter 0))))

;; similar to `dired-log'
(defun proced-log (log &rest args)
  "Log a message or the contents of a buffer.
If LOG is a string and there are more args, it is formatted with
those ARGS.  Usually the LOG string ends with a \\n.
End each bunch of errors with (proced-log t signal):
this inserts the current time, buffer and signal at the start of the page,
and \f (formfeed) at the end."
  (let ((obuf (current-buffer)))
    (with-current-buffer (get-buffer-create proced-log-buffer)
      (goto-char (point-max))
      (let (buffer-read-only)
	(cond ((stringp log)
	       (insert (if args
			   (apply 'format log args)
			 log)))
	      ((bufferp log)
	       (insert-buffer-substring log))
	      ((eq t log)
	       (backward-page 1)
	       (unless (bolp)
		 (insert "\n"))
	       (insert (current-time-string)
		       "\tBuffer `" (buffer-name obuf) "', "
                       (format "signal `%s'\n" (car args)))
	       (goto-char (point-max))
	       (insert "\f\n")))))))

;; similar to `dired-log-summary'
(defun proced-log-summary (signal string)
  "State a summary of SIGNAL's failures, in echo area and log buffer.
STRING is an overall summary of the failures."
  (message "Signal %s: %s--type ? for details" signal string)
  ;; Log a summary describing a bunch of errors.
  (proced-log (concat "\n" string "\n"))
  (proced-log t signal))

(defun proced-help ()
  "Provide help for the `proced' user."
  (interactive)
  (proced-why)
  (if (eq last-command 'proced-help)
      (describe-mode)
    (message proced-help-string)))

(defun proced-undo ()
  "Undo in a proced buffer.
This doesn't recover killed processes, it just undoes changes in the proced
buffer.  You can use it to recover marks."
  (interactive)
  (let (buffer-read-only)
    (undo))
  (message "Change in Proced buffer undone.
Killed processes cannot be recovered by Emacs."))

(provide 'proced)

;; arch-tag: a6e312ad-9032-45aa-972d-31a8cfc545af
;;; proced.el ends here
