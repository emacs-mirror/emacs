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

;; Proced makes an Emacs buffer containing a listing of the current system
;; processes (using ps(1)).  You can use the normal Emacs commands
;; to move around in this buffer, and special Proced commands to operate
;; on the processes listed.
;;
;; To do:
;; - decompose ps(1) output into columns (for `proced-header-alist')
;;   How can we identify columns that may contain whitespace
;;   and that can be either right or left justified?
;;   Use a "grammar table"?
;; - sort the "cooked" values used in the output format fields
;;   if ps(1) doesn't support the requested sorting scheme
;; - filter by user name or other criteria
;; - automatic update of process list

;;; Code:

(defgroup proced nil
  "Proced mode."
  :group 'processes
  :group 'unix
  :prefix "proced-")

;; FIXME: a better approach instead of PID-COLUMN would be based
;; on `proced-header-alist' once we have a reliable scheme to set this variable
(defcustom proced-command-alist
  (cond ((memq system-type '(berkeley-unix))
         '(("user" ("ps" "-uxgww") 2)
           ("user-running" ("ps" "-uxrgww") 2)
           ("all" ("ps" "-auxgww") 2)
           ("all-running" ("ps" "-auxrgww") 2)))
        ((memq system-type '(gnu gnu/linux)) ; BSD syntax
         `(("user" ("ps" "uxwww") 2)
           ("user-running" ("ps" "uxrwww") 2)
           ("all" ("ps" "auxwww") 2)
           ("all-running" ("ps" "auxrwww") 2)
           ("emacs" ("ps" "--pid" ,(number-to-string (emacs-pid))
                     "--ppid" ,(number-to-string (emacs-pid))
                      "uwww") 2)))
        ((memq system-type '(darwin))
         `(("user" ("ps" "-u" ,(number-to-string (user-uid))) 2)
           ("all" ("ps" "-Au") 2)))
        (t ; standard UNIX syntax; doesn't allow to list running processes only
         `(("user" ("ps" "-fu" ,(number-to-string (user-uid))) 2)
           ("all" ("ps" "-ef") 2))))
  "Alist of commands to get list of processes.
Each element has the form (NAME COMMAND PID-COLUMN).
NAME is a shorthand name to select the type of listing.
COMMAND is a list (COMMAND-NAME ARG1 ARG2 ...),
where COMMAND-NAME is the command to generate the listing (usually \"ps\").
ARG1, ARG2, ... are arguments passed to COMMAND-NAME to generate
a particular listing.  These arguments differ under various operating systems.
PID-COLUMN is the column number (starting from 1) of the process ID."
  :group 'proced
  :type '(repeat (group (string :tag "name")
                        (cons (string :tag "command")
                              (repeat (string :tag "option")))
                        (integer :tag "PID column"))))

(defcustom proced-command (if (zerop (user-real-uid)) "all" "user")
  "Name of process listing.
Must be the car of an element of `proced-command-alist'."
  :group 'proced
  :type '(string :tag "name"))
(make-variable-buffer-local 'proced-command)

;; Should we incorporate in NAME that sorting can be done in ascending
;; or descending order?  Then we couldn't associate NAME anymore with one
;; of the headers in the output of ps(1).
;; FIXME: A sorting scheme without options or with an option being a symbol
;; should be implemented in elisp
(defcustom proced-sorting-schemes-alist
  (cond ((memq system-type '(gnu gnu/linux)) ; GNU long options
         '(("%CPU" "--sort" "-pcpu") ; descending order
           ("%MEM" "--sort" "-pmem") ; descending order
           ("COMMAND" "--sort" "args")
           ("PID" "--sort" "pid")
           ("PGID,PID" "--sort" "pgid,pid")
           ("PPID,PID" "--sort" "ppid,pid")
           ("RSS" "--sort" "rss,pid") ; equal RSS's are rare
           ("STAT,PID" "--sort" "stat,pid")
           ("START" "--sort" "start_time")
           ("TIME" "--sort" "cputime")
           ("TTY,PID" "--sort" "tty,pid")
           ("UID,PID" "--sort" "uid,pid")
           ("USER,PID" "--sort" "user,pid")
           ("VSZ,PID" "--sort" "vsz,pid"))))
  "Alist of sorting schemes.
Each element is a list (NAME OPTION1 OPTION2 ...).
NAME denotes the sorting scheme.  It is the name of a header or a
comma-separated sequence of headers in the output of ps(1).
OPTION1, OPTION2, ... are options defining the sorting scheme."
  :group 'proced
  :type '(repeat (cons (string :tag "name")
                       (repeat (string :tag "option")))))

(defcustom proced-sorting-scheme nil
  "Proced sorting type.
Must be the car of an element of `proced-sorting-schemes-alist' or nil."
  :group 'proced
  :type `(choice ,@(append '((const nil)) ; sorting type may be nil
                           (mapcar (lambda (item)
                                     (list 'const (car item)))
                                   proced-sorting-schemes-alist))))
(make-variable-buffer-local 'proced-sorting-scheme)

(defcustom proced-goal-header-re "\\b\\(CMD\\|COMMAND\\)\\b"
  "If non-nil, regexp that defines the `proced-goal-column'."
  :group 'proced
  :type '(choice (const :tag "none" nil)
                 (regexp :tag "regexp")))

(defcustom proced-signal-function 'signal-process
  "Name of signal function.
It can be an elisp function (usually `signal-process') or a string specifying
the external command (usually \"kill\")."
  :group 'proced
  :type '(choice (function :tag "function")
                 (string :tag "command")))

(defcustom proced-signal-list
  '(("HUP   (1.  Hangup)")
    ("INT   (2.  Terminal interrupt)")
    ("QUIT  (3.  Terminal quit)")
    ("ABRT  (6.  Process abort)")
    ("KILL  (9.  Kill -- cannot be caught or ignored)")
    ("ALRM  (14. Alarm Clock)")
    ("TERM  (15. Termination)"))
  "List of signals, used for minibuffer completion."
  :group 'proced
  :type '(repeat (string :tag "signal")))

;; Internal variables
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

(defvar proced-goal-column nil
  "Proced goal column.  Initialized based on `proced-goal-header-re'.")
(make-variable-buffer-local 'proced-goal-column)

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
    (define-key km "n" 'proced-next-line)
    (define-key km "p" 'proced-previous-line)
    (define-key km "\C-n" 'proced-next-line)
    (define-key km "\C-p" 'proced-previous-line)
    (define-key km "\C-?" 'proced-previous-line)
    (define-key km [down] 'proced-next-line)
    (define-key km [up] 'proced-previous-line)
    ;; marking
    (define-key km "d" 'proced-mark) ; Dired compatibility
    (define-key km "m" 'proced-mark)
    (define-key km "u" 'proced-unmark)
    (define-key km "\177" 'proced-unmark-backward)
    (define-key km "M" 'proced-mark-all)
    (define-key km "U" 'proced-unmark-all)
    (define-key km "t" 'proced-toggle-marks)
    ;; sorting
    (define-key km "sc" 'proced-sort-pcpu)
    (define-key km "sm" 'proced-sort-pmem)
    (define-key km "sp" 'proced-sort-pid)
    (define-key km "ss" 'proced-sort-start)
    (define-key km "sS" 'proced-sort)
    (define-key km "st" 'proced-sort-time)
    ;; operate
    (define-key km "h" 'proced-hide-processes)
    (define-key km "x" 'proced-send-signal) ; Dired compatibility
    (define-key km "k" 'proced-send-signal) ; kill processes
    ;; misc
    (define-key km "l" 'proced-listing-type)
    (define-key km "g" 'revert-buffer) ; Dired compatibility
    (define-key km "h" 'describe-mode)
    (define-key km "?" 'proced-help)
    (define-key km "q" 'quit-window)
    (define-key km [remap undo] 'proced-undo)
    (define-key km [remap advertised-undo] 'proced-undo)
    km)
  "Keymap for proced commands")

(easy-menu-define
  proced-menu proced-mode-map "Proced Menu"
  '("Proced"
    ["Mark" proced-mark t]
    ["Unmark" proced-unmark t]
    ["Mark All" proced-mark-all t]
    ["Unmark All" proced-unmark-all t]
    ["Toggle Marks" proced-unmark-all t]
    "--"
    ["Sort" proced-sort t]
    ["Sort by %CPU" proced-sort-pcpu (proced-sorting-scheme-p "%CPU")]
    ["Sort by %MEM" proced-sort-pmem (proced-sorting-scheme-p "%MEM")]
    ["Sort by PID" proced-sort-pid (proced-sorting-scheme-p "PID")]
    ["Sort by START" proced-sort-start (proced-sorting-scheme-p "START")]
    ["Sort by TIME" proced-sort-time (proced-sorting-scheme-p "TIME")]
    "--"
    ["Hide Marked Processes" proced-hide-processes t]
    "--"
    ["Revert" revert-buffer t]
    ["Send signal" proced-send-signal t]
    ["Change listing" proced-listing-type t]))

(defconst proced-help-string
  "(n)ext, (p)revious, (m)ark, (u)nmark, (k)ill, (q)uit  (type ? for more help)"
  "Help string for proced.")

(defvar proced-header-alist nil
  "Alist of headers in Proced buffer.
Each element is of the form (NAME START END JUSTIFY).
NAME is name of header in the output of ps(1).
START and END are column numbers starting from 0.
END is t if there is no end column for that field.
JUSTIFY is 'left or 'right for left or right-justified output of ps(1).")
(make-variable-buffer-local 'proced-header-alist)

(defvar proced-sorting-schemes-re nil
  "Regexp to match valid sorting schemes.")
(make-variable-buffer-local 'proced-sorting-schemes-re)

;; helper functions
(defun proced-marker-regexp ()
  "Return regexp matching `proced-marker-char'."
  ;; `proced-marker-char' must appear in column zero
  (concat "^" (regexp-quote (char-to-string proced-marker-char))))

(defun proced-success-message (action count)
  "Display success message for ACTION performed for COUNT processes."
  (message "%s %s process%s" action count (if (= 1 count) "" "es")))

(defun proced-move-to-goal-column ()
  "Move to `proced-goal-column' if non-nil."
  (beginning-of-line)
  (if proced-goal-column
      (forward-char proced-goal-column)
    (forward-char 2)))

;; FIXME: a better approach would be based on `proced-header-alist'
;; once we have a reliable scheme to set this variable
(defsubst proced-skip-regexp ()
  "Regexp to skip in process listing to find PID column."
  (apply 'concat (make-list (1- (nth 2 (assoc proced-command
                                              proced-command-alist)))
                            "\\s-+\\S-+")))

(define-derived-mode proced-mode nil "Proced"
  "Mode for displaying UNIX system processes and sending signals to them.
Type \\[proced-mark-process] to mark a process for later commands.
Type \\[proced-send-signal] to send signals to marked processes.

\\{proced-mode-map}"
  (abbrev-mode 0)
  (auto-fill-mode 0)
  (setq buffer-read-only t
        truncate-lines t)
  (set (make-local-variable 'revert-buffer-function) 'proced-revert)
  (set (make-local-variable 'font-lock-defaults)
       '(proced-font-lock-keywords t nil nil beginning-of-line)))

;; Proced mode is suitable only for specially formatted data.
(put 'proced-mode 'mode-class 'special)

;;;###autoload
(defun proced (&optional arg)
  "Mode for displaying UNIX system processes and sending signals to them.
Type \\[proced-mark-process] to mark a process for later commands.
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
        (proced-update))

    (if arg
	(display-buffer buffer)
      (pop-to-buffer buffer)
      (message (substitute-command-keys
                "type \\[quit-window] to quit, \\[proced-help] for help")))))

(defun proced-next-line (arg)
  "Move down lines then position at `proced-goal-column'.
Optional prefix ARG says how many lines to move; default is one line."
  (interactive "p")
  (next-line arg)
  (proced-move-to-goal-column))

(defun proced-previous-line (arg)
  "Move up lines then position at `proced-goal-column'.
Optional prefix ARG says how many lines to move; default is one line."
  (interactive "p")
  (previous-line arg)
  (proced-move-to-goal-column))

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
  "Mark the current (or next ARG) processes using MARK."
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
  "Mark all processes."
  (interactive)
  (proced-do-mark-all t))

(defun proced-unmark-all ()
  "Unmark all processes."
  (interactive)
  (proced-do-mark-all nil))

(defun proced-do-mark-all (mark)
  "Mark all processes using MARK."
  (let (buffer-read-only)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (proced-insert-mark mark)))))

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

;; Mostly analog of `dired-do-kill-lines'.
;; However, for negative args the target lines of `dired-do-kill-lines'
;; include the current line, whereas `dired-mark' for negative args operates
;; on the preceding lines. Here we are consistent with `dired-mark'.
(defun proced-hide-processes (&optional arg quiet)
  "Hide marked processes.
With prefix ARG, hide that many lines starting with the current line.
\(A negative argument hides backward.)
If QUIET is non-nil suppress status message.
Returns count of hidden lines."
  (interactive "P")
  (let ((mark-re (proced-marker-regexp))
        (count 0)
        buffer-read-only)
    (save-excursion
      (if arg
          ;; Hide ARG lines starting with the current line.
          (delete-region (line-beginning-position)
                         (save-excursion
                           (if (<= 0 arg)
                               (setq count (- arg (forward-line arg)))
                             (setq count (min (1- (line-number-at-pos))
                                              (abs arg)))
                             (forward-line (- count)))
                           (point)))
        ;; Hide marked lines
        (while (and (not (eobp))
                    (re-search-forward mark-re nil t))
          (delete-region (match-beginning 0)
                         (save-excursion (forward-line) (point)))
          (setq count (1+ count)))))
    (unless (zerop count) (proced-move-to-goal-column))
    (unless quiet (proced-success-message "Hid" count))
    count))

(defun proced-listing-type (command)
  "Select `proced' listing type COMMAND from `proced-command-alist'."
  (interactive
   (list (completing-read "Listing type: " proced-command-alist nil t)))
  (setq proced-command command)
  (proced-update))

;; adopted from `ruler-mode-space'
(defsubst proced-header-space (width)
  "Return a single space string of WIDTH times the normal character width."
  (propertize " " 'display (list 'space :width width)))

(defun proced-update (&optional quiet)
  "Update the `proced' process information.  Preserves point and marks."
  ;; This is the main function that generates and updates the process listing.
  (interactive)
  (or quiet (message "Updating process information..."))
  (let* ((command (cadr (assoc proced-command proced-command-alist)))
         (regexp (concat (proced-skip-regexp) "\\s-+\\([0-9]+\\>\\)"))
         (old-pos (if (save-excursion
                        (beginning-of-line)
                        (looking-at (concat "^[* ]" regexp)))
                      (cons (match-string-no-properties 1)
                            (current-column))))
         buffer-read-only mp-list)
    (goto-char (point-min))
    ;; remember marked processes (whatever the mark was)
    (while (re-search-forward (concat "^\\(\\S-\\)" regexp) nil t)
      (push (cons (match-string-no-properties 2)
                  (match-string-no-properties 1)) mp-list))
    ;; generate new listing
    (erase-buffer)
    (apply 'call-process (car command) nil t nil
           (append (cdr command) (cdr (assoc proced-sorting-scheme
                                             proced-sorting-schemes-alist))))
    (goto-char (point-min))
    (while (not (eobp))
      (insert "  ")
      (forward-line))
    ;; (delete-trailing-whitespace)
    (goto-char (point-min))
    (while (re-search-forward "[ \t\r]+$" nil t)
      (delete-region (match-beginning 0) (match-end 0)))
    (goto-char (point-min))
    (let ((lep (line-end-position)))
      ;; header line: code inspired by `ruler-mode-ruler'
      (setq header-line-format
            (list "" (if (eq 'left (car (window-current-scroll-bars)))
                         (proced-header-space 'scroll-bar))
                  (proced-header-space 'left-fringe)
                  (proced-header-space 'left-margin)
                  (replace-regexp-in-string
                   "%" "%%" (buffer-substring-no-properties (point) lep))))
      (setq proced-header-alist nil)
      ;; FIXME: handle left/right justification properly
      (while (re-search-forward "\\([^ \t\n]+\\)[ \t]*\\($\\)?" lep t)
        (push (list (match-string-no-properties 1)
                    ;; take the column number starting from zero
                    (1- (match-beginning 0)) (or (not (not (match-beginning 2)))
                                                 (1- (match-end 0)))
                    'left)
              proced-header-alist)))
    (let ((temp (regexp-opt (mapcar 'car proced-header-alist) t)))
      (setq proced-sorting-schemes-re
            (concat "\\`" temp "\\(," temp "\\)*\\'")))
    ;; remove header line from ps(1) output
    (goto-char (point-min))
    (delete-region (point)
                   (save-excursion (forward-line) (point)))
    (set-buffer-modified-p nil)
    ;; set `proced-goal-column'
    (if proced-goal-header-re
        (let ((hlist proced-header-alist) header)
          (while (setq header (pop hlist))
            (if (string-match proced-goal-header-re (car header))
                (setq proced-goal-column
                      (if (eq 'left (nth 3 header))
                          (nth 1 header) (nth 2 header))
                      hlist nil)))))
    ;; restore process marks
    (if mp-list
        (save-excursion
          (goto-char (point-min))
          (let (mark)
            (while (re-search-forward (concat "^" regexp) nil t)
              (if (setq mark (assoc (match-string-no-properties 1) mp-list))
                  (save-excursion
                    (beginning-of-line)
                    (insert (cdr mark))
                    (delete-char 1)))))))
    ;; restore buffer position (if possible)
    (goto-char (point-min))
    (if (and old-pos
             (re-search-forward
              (concat "^[* ]" (proced-skip-regexp) "\\s-+" (car old-pos) "\\>")
              nil t))
        (progn
          (beginning-of-line)
          (forward-char (cdr old-pos)))
      (proced-move-to-goal-column))
    ;; update modeline
    ;; Does the long mode-name clutter the modeline?
    (setq mode-name (concat "Proced: " proced-command
                            (if proced-sorting-scheme
                                (concat " by " proced-sorting-scheme)
                              "")))
    (force-mode-line-update)
    ;; done
    (or quiet (input-pending-p)
        (message "Updating process information...done."))))

(defun proced-revert (&rest args)
  "Analog of `revert-buffer'."
  (proced-update))

;; I do not want to reinvent the wheel.  Should we rename `dired-pop-to-buffer'
;; and move it to window.el so that proced and ibuffer can easily use it, too?
;; What about functions like `appt-disp-window' that use
;; `shrink-window-if-larger-than-buffer'?
(autoload 'dired-pop-to-buffer "dired")

(defun proced-send-signal (&optional signal)
  "Send a SIGNAL to the marked processes.
SIGNAL may be a string (HUP, INT, TERM, etc.) or a number.
If SIGNAL is nil display marked processes and query interactively for SIGNAL."
  (interactive)
  (let ((regexp (concat (proced-marker-regexp)
                        (proced-skip-regexp) "\\s-+\\([0-9]+\\>\\).*$"))
        process-list)
    ;; collect marked processes
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (push (cons (match-string-no-properties 1)
                    ;; How much info should we collect here?  Would it be
                    ;; better to collect only the PID (to avoid ambiguities)
                    ;; and the command name?
                    (substring (match-string-no-properties 0) 2))
              process-list)))
    (setq process-list (nreverse process-list))
    (if (not process-list)
        (message "No processes marked")
      (unless signal
        ;; Display marked processes (code taken from `dired-mark-pop-up').
        (let ((bufname  " *Marked Processes*")
              (header header-line-format)) ; reuse
          (with-current-buffer (get-buffer-create bufname)
            (setq truncate-lines t
                  header-line-format header)
            (erase-buffer)
            (dolist (process process-list)
              (insert "  " (cdr process) "\n"))
            (save-window-excursion
              (dired-pop-to-buffer bufname) ; all we need
              (let* ((completion-ignore-case t)
                     (pnum (if (= 1 (length process-list))
                               "1 process"
                             (format "%d processes" (length process-list))))
                     ;; The following is an ugly hack. Is there a better way
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
              err-list)
          (if (functionp proced-signal-function)
              ;; use built-in `signal-process'
              (let ((signal (if (stringp signal)
                                (if (string-match "\\`[0-9]+\\'" signal)
                                    (string-to-number signal)
                                  (make-symbol signal))
                              signal))) ; number
                (dolist (process process-list)
                  (if (zerop (funcall
                              proced-signal-function
                              (string-to-number (car process)) signal))
                      (setq count (1+ count))
                    (push (cdr process) err-list))))
            ;; use external system call
            (let ((signal (concat "-" (if (numberp signal)
                                          (number-to-string signal) signal))))
              (dolist (process process-list)
                (if (zerop (call-process
                            proced-signal-function nil 0 nil
                            signal (car process)))
                    (setq count (1+ count))
                  (push (cdr process) err-list)))))
          (if err-list
              ;; FIXME: that's not enough to display the errors.
              (message "%s: %s" signal err-list)
            (proced-success-message "Sent signal to" count)))
        ;; final clean-up
        (run-hooks 'proced-after-send-signal-hook)))))

(defun proced-help ()
  "Provide help for the `proced' user."
  (interactive)
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

;;; Sorting
(defun proced-sort (scheme)
  "Sort Proced buffer using SCHEME.
When called interactively, an empty string means nil, i.e., no sorting."
  (interactive
   (list (let* ((completion-ignore-case t)
                ;; restrict completion list to applicable sorting schemes
                (completion-list
                 (apply 'append
                        (mapcar (lambda (x)
                                  (if (string-match proced-sorting-schemes-re
                                                    (car x))
                                      (list (car x))))
                                proced-sorting-schemes-alist)))
                (scheme (completing-read "Sorting type: "
                                         completion-list nil t)))
           (if (string= "" scheme) nil scheme))))
  (if (proced-sorting-scheme-p scheme)
      (progn
        (setq proced-sorting-scheme scheme)
        (proced-update))
    (error "Proced sorting scheme %s not applicable" scheme)))

(defun proced-sorting-scheme-p (scheme)
  "Return non-nil if SCHEME is an applicable sorting scheme.
SCHEME must be a string or nil."
  (or (not scheme)
      (and (string-match proced-sorting-schemes-re scheme)
           (assoc scheme proced-sorting-schemes-alist))))

(defun proced-sort-pcpu ()
  "Sort Proced buffer by percentage CPU time (%CPU)."
  (interactive)
  (proced-sort "%CPU"))

(defun proced-sort-pmem ()
  "Sort Proced buffer by percentage memory usage (%MEM)."
  (interactive)
  (proced-sort "%MEM"))

(defun proced-sort-pid ()
  "Sort Proced buffer by PID."
  (interactive)
  (proced-sort "PID"))

(defun proced-sort-start ()
  "Sort Proced buffer by time the command started (START)."
  (interactive)
  (proced-sort "START"))

(defun proced-sort-time ()
  "Sort Proced buffer by cumulative CPU time (TIME)."
  (interactive)
  (proced-sort "TIME"))

(provide 'proced)

;; arch-tag: a6e312ad-9032-45aa-972d-31a8cfc545af
;;; proced.el ends here.
