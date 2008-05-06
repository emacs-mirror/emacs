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

(defcustom proced-procname-column-regexp "\\b\\(CMD\\|COMMAND\\)\\b"
  "If non-nil, regexp that defines the `proced-procname-column'."
  :group 'proced
  :type '(choice (const :tag "none" nil)
                 (regexp :tag "regexp")))

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

;; Should we incorporate in NAME if sorting is done in descending order?
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
NAME denotes the sorting scheme and OPTION1, OPTION2, ... are options
defining the sorting scheme."
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

(defcustom proced-command (if (zerop (user-real-uid)) "all" "user")
  "Name of process listing.
Must be the car of an element of `proced-command-alist'."
  :group 'proced
  :type '(string :tag "name"))
(make-variable-buffer-local 'proced-command)

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

(defvar proced-marker-char ?*		; the answer is 42
  "In proced, the current mark character.")

;; face and font-lock code taken from dired
(defgroup proced-faces nil
  "Faces used by Proced."
  :group 'proced
  :group 'faces)

(defface proced-header
  '((t (:inherit font-lock-type-face)))
  "Face used for proced headers."
  :group 'proced-faces)
(defvar proced-header-face 'proced-header
  "Face name used for proced headers.")

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

(defvar proced-header-regexp "\\`.*$"
  "Regexp matching a header line.")

(defvar proced-procname-column nil
  "Proced command column.
Initialized based on `proced-procname-column-regexp'.")
(make-variable-buffer-local 'proced-procname-column)

(defvar proced-font-lock-keywords
  (list
   ;;
   ;; Process listing headers.
   (list proced-header-regexp '(0 proced-header-face))
   ;;
   ;; Proced marks.
   (list proced-re-mark '(0 proced-mark-face))
   ;;
   ;; Marked files.
   (list (concat "^[" (char-to-string proced-marker-char) "]")
         '(".+" (proced-move-to-procname) nil (0 proced-marked-face)))))

(defvar proced-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km " " 'next-line)
    (define-key km "n" 'next-line)
    (define-key km "p" 'previous-line)
    (define-key km "\C-?" 'previous-line)
    (define-key km "h" 'describe-mode)
    (define-key km "?" 'proced-help)
    (define-key km "d" 'proced-mark) ; Dired compatibility
    (define-key km "m" 'proced-mark)
    (define-key km "M" 'proced-mark-all)
    (define-key km "u" 'proced-unmark)
    (define-key km "\177" 'proced-unmark-backward)
    (define-key km "U" 'proced-unmark-all)
    (define-key km "t" 'proced-toggle-marks)
    (define-key km "h" 'proced-hide-processes)
    (define-key km "x" 'proced-send-signal) ; Dired compatibility
    (define-key km "k" 'proced-send-signal) ; kill processes
    (define-key km "l" 'proced-listing-type)
    (define-key km "g" 'revert-buffer) ; Dired compatibility
    (define-key km "q" 'quit-window)
    (define-key km "sc" 'proced-sort-pcpu)
    (define-key km "sm" 'proced-sort-pmem)
    (define-key km "sp" 'proced-sort-pid)
    (define-key km "ss" 'proced-sort-start)
    (define-key km "sS" 'proced-sort)
    (define-key km "st" 'proced-sort-time)
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

(defun proced-marker-regexp ()
  "Return regexp matching `proced-marker-char'."
  (concat "^" (regexp-quote (char-to-string proced-marker-char))))

(defun proced-success-message (action count)
  "Display success message for ACTION performed for COUNT processes."
  (message "%s %s process%s" action count (if (= 1 count) "" "es")))

(defun proced-move-to-procname ()
  "Move to the beginning of the process name on the current line.
Return the position of the beginning of the process name, or nil if none found."
  (beginning-of-line)
  (if proced-procname-column
      (forward-char proced-procname-column)
    (forward-char 2)))

(defsubst proced-skip-regexp ()
  "Regexp to skip in process listing."
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
  (let ((buffer (get-buffer-create "*Process Info*")) new)
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
        (line (line-number-at-pos))
	buffer-read-only)
    ;; do nothing in the first line
    (unless (= line 1)
      (setq count (1+ (if (<= 0 count) count
                        (min (- line 2) (abs count)))))
      (beginning-of-line)
      (while (not (or (zerop (setq count (1- count))) (eobp)))
        (proced-insert-mark mark backward))
      (proced-move-to-procname))))

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
      (goto-line 2)
      (while (not (eobp))
        (proced-insert-mark mark)))))

(defun proced-toggle-marks ()
  "Toggle marks: marked processes become unmarked, and vice versa."
  (interactive)
  (let ((mark-re (proced-marker-regexp))
        buffer-read-only)
    (save-excursion
      (goto-line 2)
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
          (let ((line (line-number-at-pos)))
            ;; do nothing in the first line
            (unless (= line 1)
              (delete-region (line-beginning-position)
                             (save-excursion
                               (if (<= 0 arg)
                                   (setq count (- arg (forward-line arg)))
                                 (setq count (min (- line 2) (abs arg)))
                                 (forward-line (- count)))
                               (point)))))
        ;; Hide marked lines
        (goto-line 2)
        (while (and (not (eobp))
                    (re-search-forward mark-re nil t))
          (delete-region (match-beginning 0)
                         (save-excursion (forward-line) (point)))
          (setq count (1+ count)))))
    (unless (zerop count) (proced-move-to-procname))
    (unless quiet
      (proced-success-message "Hid" count))
    count))

(defun proced-listing-type (command)
  "Select `proced' listing type COMMAND from `proced-command-alist'."
  (interactive
   (list (completing-read "Listing type: " proced-command-alist nil t)))
  (setq proced-command command)
  (proced-update))

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
         buffer-read-only plist)
    (goto-char (point-min))
    ;; remember marked processes (whatever the mark was)
    (while (re-search-forward (concat "^\\(\\S-\\)" regexp) nil t)
      (push (cons (match-string-no-properties 2)
                  (match-string-no-properties 1)) plist))
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
    (set-buffer-modified-p nil)
    ;; set `proced-procname-column'
    (goto-char (point-min))
    (and proced-procname-column-regexp
         (re-search-forward proced-procname-column-regexp nil t)
         (setq proced-procname-column (1- (match-beginning 0))))
    ;; restore process marks
    (if plist
        (save-excursion
          (goto-line 2)
          (let (mark)
            (while (re-search-forward (concat "^" regexp) nil t)
              (if (setq mark (assoc (match-string-no-properties 1) plist))
                  (save-excursion
                    (beginning-of-line)
                    (insert (cdr mark))
                    (delete-char 1)))))))
    ;; restore buffer position (if possible)
    (goto-line 2)
    (if (and old-pos
             (re-search-forward
              (concat "^[* ]" (proced-skip-regexp) "\\s-+" (car old-pos) "\\>")
              nil t))
        (progn
          (beginning-of-line)
          (forward-char (cdr old-pos)))
      (proced-move-to-procname))
    ;; update modeline
    (setq mode-name (if proced-sorting-scheme
                        (concat "Proced by " proced-sorting-scheme)
                      "Proced"))
    (force-mode-line-update)
    ;; done
    (or quiet (input-pending-p)
        (message "Updating process information...done."))))

(defun proced-revert (&rest args)
  "Analog of `revert-buffer'."
  (proced-update))

;; I do not want to reinvent the wheel.  Should we rename `dired-pop-to-buffer'
;; and move it to simple.el so that proced and ibuffer can easily use it, too?
(autoload 'dired-pop-to-buffer "dired")

(defun proced-send-signal (&optional signal)
  "Send a SIGNAL to the marked processes.
SIGNAL may be a string (HUP, INT, TERM, etc.) or a number.
If SIGNAL is nil display marked processes and query interactively for SIGNAL."
  (interactive)
  (let ((regexp (concat (proced-marker-regexp)
                        (proced-skip-regexp) "\\s-+\\([0-9]+\\>\\).*$"))
        plist)
    ;; collect marked processes
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (push (cons (match-string-no-properties 1)
                    ;; How much info should we collect here?  Would it be
                    ;; better to collect only the PID (to avoid ambiguities)
                    ;; and the command name?
                    (substring (match-string-no-properties 0) 2))
              plist)))
    (setq plist (nreverse plist))
    (if (not plist)
        (message "No processes marked")
      (unless signal
        ;; Display marked processes (code taken from `dired-mark-pop-up').
        (let ((bufname  " *Marked Processes*")
              (header (save-excursion
                        (goto-char (+ 2 (point-min)))
                        (buffer-substring-no-properties
                         (point) (line-end-position)))))
          (with-current-buffer (get-buffer-create bufname)
            (setq truncate-lines t)
            (erase-buffer)
            (insert header "\n")
            (dolist (proc plist)
              (insert (cdr proc) "\n"))
            (save-window-excursion
              (dired-pop-to-buffer bufname) ; all we need
              (let* ((completion-ignore-case t)
                     (pnum (if (= 1 (length plist))
                               "1 process"
                             (format "%d processes" (length plist))))
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
                (dolist (process plist)
                  (if (zerop (funcall
                              proced-signal-function
                              (string-to-number (car process)) signal))
                      (setq count (1+ count))
                    (push (cdr process) err-list))))
            ;; use external system call
            (let ((signal (concat "-" (if (numberp signal)
                                          (number-to-string signal) signal))))
              (dolist (process plist)
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
  (message "Change in proced buffer undone.
Killed processes cannot be recovered by Emacs."))

;;; Sorting
(defun proced-sort (scheme)
  "Sort Proced buffer using SCHEME.
When called interactively, an empty string means nil, i.e., no sorting."
  (interactive
   (list (let* ((completion-ignore-case t)
                (scheme (completing-read "Sorting type: "
                                         proced-sorting-schemes-alist nil t)))
           (if (string= "" scheme) nil scheme))))
  (if (proced-sorting-scheme-p scheme)
      (progn
        (setq proced-sorting-scheme scheme)
        (proced-update))
    (error "Proced sorting scheme %s undefined" scheme)))

(defun proced-sorting-scheme-p (scheme)
  "Return non-nil if SCHEME is an applicable sorting scheme.
SCHEME must be a string or nil."
  (or (not scheme)
      (assoc scheme proced-sorting-schemes-alist)))

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
