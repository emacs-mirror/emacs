;;; proced.el --- operate on processes like dired

;; Copyright (C) 2008 Free Software Foundation, Inc.

;; Author: Roland Winkler <Roland.Winkler@physik.uni-erlangen.de>
;; Keywords: Processes, Unix

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Proced makes an Emacs buffer containing a listing of the current processes
;; (using ps(1)).  You can use the normal Emacs commands to move around in
;; this buffer, and special Proced commands to operate on the processes listed.
;;
;; To autoload, use
;;     (autoload 'proced "proced" nil t)
;; in your .emacs file.
;;
;; Is there a need for additional features like:
;; - automatic update of process list
;; - sort by CPU time or other criteria
;; - filter by user name or other criteria

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
  (cond ((memq system-type '(berkeley-unix netbsd))
         '(("user" ("ps" "-uxgww") 2)
           ("user-running" ("ps" "-uxrgww") 2)
           ("all" ("ps" "-auxgww") 2)
           ("all-running" ("ps" "-auxrgww") 2)))
        ((memq system-type '(linux lignux gnu/linux))
         `(("user" ("ps" "uxwww") 2)
           ("user-running" ("ps" "uxrwww") 2)
           ("all" ("ps" "auxwww") 2)
           ("all-running" ("ps" "auxrwww") 2)
           ("emacs" ("ps" "--pid" ,(number-to-string (emacs-pid))
                     "--ppid" ,(number-to-string (emacs-pid))
                      "uwww") 2)))
        (t ; standard syntax doesn't allow us to list running processes only
         `(("user" ("ps" "-fu" ,(number-to-string (user-uid))) 2)
           ("all" ("ps" "-ef") 2))))
  "Alist of commands to get list of processes.
Each element has the form (NAME COMMAND PID-COLUMN SORT-COLUMN).
NAME is a shorthand name to select the type of listing.
COMMAND is a list (COMMAND-NAME ARG1 ARG2 ...),
where COMMAND-NAME is the command to generate the listing (usually \"ps\").
ARG1, ARG2, ... are arguments passed to COMMAND-NAME to generate
a particular listing.  These arguments differ under various operating systems.
PID-COLUMN is the column number (starting from 1) of the process ID.
SORT-COLUMN is the column number used for sorting the process listing
\(must be a numeric field).  If nil, the process listing is not sorted."
  :group 'proced
  :type '(repeat (group (string :tag "name")
                        (cons (string :tag "command")
                              (repeat (string :tag "option")))
                        (integer :tag "PID column")
                        (option (integer :tag "sort column")))))

(defcustom proced-command (if (zerop (user-real-uid)) "all" "user")
  "Name of process listing.
Must be the car of an element of `proced-command-alist'."
  :group 'proced
  :type '(string :tag "name"))

(defcustom proced-kill-program "kill"
  "Name of kill command (usually `kill')."
  :group 'proced
  :type '(string :tag "command"))

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
    (define-key km "g" 'revert-buffer) ; Dired compatibility
    (define-key km "q" 'quit-window)
    (define-key km "u" 'proced-unmark)
    (define-key km "U" 'proced-unmark-all)
    (define-key km "x" 'proced-send-signal) ; Dired compatibility
    (define-key km "k" 'proced-send-signal) ; kill processes
    (define-key km "l" 'proced-listing-type)
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
    "--"
    ["Revert" revert-buffer t]
    ["Send signal" proced-send-signal t]
    ["Change listing" proced-listing-type t]))

(defconst proced-help-string
  "(n)ext, (p)revious, (m)ark, (u)nmark, (k)ill, (q)uit  (type ? for more help)"
  "Help string for proced.")

(defun proced-mode (&optional arg)
  "Mode for displaying UNIX processes and sending signals to them.
Type \\[proced-mark-process] to mark a process for later commands.
Type \\[proced-send-signal] to send signals to marked processes.

If invoked with optional ARG the window displaying the process
information will be displayed but not selected.

\\{proced-mode-map}"
  (interactive "P")
  (let ((proced-buffer (get-buffer-create "*Process Info*")) new)
    (set-buffer proced-buffer)
    (setq new (zerop (buffer-size)))
    (when new
      (kill-all-local-variables)
      (use-local-map proced-mode-map)
      (abbrev-mode 0)
      (auto-fill-mode 0)
      (setq buffer-read-only t
            truncate-lines t
            major-mode 'proced-mode
            mode-name "Proced")
      (set (make-local-variable 'revert-buffer-function) 'proced-revert)
      (set (make-local-variable 'font-lock-defaults)
           '(proced-font-lock-keywords t nil nil beginning-of-line)))

    (if (or new arg)
        (proced-update))

    (if arg
	(display-buffer proced-buffer)
      (pop-to-buffer proced-buffer)
      (message (substitute-command-keys
                "type \\[quit-window] to quit, \\[proced-help] for help")))
    (if new (run-mode-hooks 'proced-mode-hook))))

;; Proced mode is suitable only for specially formatted data.
(put 'proced-mode 'mode-class 'special)

(fset 'proced 'proced-mode)

(defun proced-move-to-procname ()
  "Move to the beginning of the process name on the current line.
Return the position of the beginning of the process name, or nil if none found."
  (beginning-of-line)
  (if proced-procname-column
      (forward-char proced-procname-column)
    (forward-char 2)))

(defun proced-mark (&optional count)
  "Mark the current (or next COUNT) processes."
  (interactive "p")
  (proced-do-mark t count))

(defun proced-unmark (&optional count)
  "Unmark the current (or next COUNT) processes."
  (interactive "p")
  (proced-do-mark nil count))

(defun proced-do-mark (mark &optional count)
  "Mark the current (or next ARG) processes using MARK."
  (or count (setq count 1))
  (let ((n (if (<= 0 count) 1 -1))
        (line (line-number-at-pos))
	buffer-read-only)
    ;; do nothing in the first line
    (unless (= line 1)
      (setq count (1+ (cond ((<= 0 count) count)
                            ((< (abs count) line) (abs count))
                            (t (1- line)))))
      (beginning-of-line)
      (while (not (or (zerop (setq count (1- count))) (eobp)))
        (proced-insert-mark mark n))
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
  (save-excursion
    (let (buffer-read-only)
      (goto-line 2)
      (while (not (eobp))
        (proced-insert-mark mark 1)))))

(defun proced-insert-mark (mark n)
  "If MARK is non-nil, insert `proced-marker-char', move N lines."
  ;; Do we need other marks besides `proced-marker-char'?
  (insert (if mark proced-marker-char ?\s))
  (delete-char 1)
  (forward-line n))

(defun proced-listing-type (command)
  "Select `proced' listing type COMMAND from `proced-command-alist'."
  (interactive
   (list (completing-read "Listing type: " proced-command-alist nil t)))
  (setq proced-command command)
  (proced-update))

(defsubst proced-skip-regexp ()
  "Regexp to skip in process listing."
  (apply 'concat (make-list (1- (nth 2 (assoc proced-command
                                              proced-command-alist)))
                            "\\s-+\\S-+")))

(defun proced-update (&optional quiet)
  "Update the `proced' process information.  Preserves point and marks."
  (interactive)
  (or quiet (message "Updating process information..."))
  (let* ((command (cdr (assoc proced-command proced-command-alist)))
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
    (apply 'call-process (caar command) nil t nil (cdar command))
    (goto-char (point-min))
    (while (not (eobp))
      (insert "  ")
      (forward-line))
    ;; (delete-trailing-whitespace)
    (goto-char (point-min))
    (while (re-search-forward "[ \t\r]+$" nil t)
      (delete-region (match-beginning 0) (match-end 0)))
    ;; set `proced-procname-column'
    (goto-char (point-min))
    (and proced-procname-column-regexp
         (re-search-forward proced-procname-column-regexp nil t)
         (setq proced-procname-column (1- (match-beginning 0))))
    ;; sort fields
    (goto-line 2)
    (if (nth 2 command)
        (sort-numeric-fields (nth 2 command) (point) (point-max)))
    (set-buffer-modified-p nil)
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
    (or quiet (input-pending-p)
        (message "Updating process information...done."))))

(defun proced-revert (&rest args)
  "Analog of `revert-buffer'."
  (proced-update))

;; I do not want to reinvent the wheel
(autoload 'dired-pop-to-buffer "dired")

(defun proced-send-signal (&optional signal)
  "Send a SIGNAL to the marked processes.
SIGNAL may be a string (HUP, INT, TERM, etc.) or a number.
If SIGNAL is nil display marked processes and query interactively for SIGNAL."
  (interactive)
  (let ((regexp (concat "^\\*" (proced-skip-regexp) "\\s-+\\([0-9]+\\>\\).*$"))
        plist)
    ;; collect marked processes
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (push (cons (match-string-no-properties 1)
                    (substring (match-string-no-properties 0) 2))
              plist)))
    (if (not plist)
        (message "No processes marked")
      (unless signal
        ;; Display marked processes (code taken from `dired-mark-pop-up').
        ;; We include all process information to distinguish multiple
        ;; instances of the same program.
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
                     ;; The following is an ugly hack. Is there a better way
                     ;; to help people like me to remember the signals and
                     ;; their meanings?
                     (tmp (completing-read "Signal (default TERM): "
                                           proced-signal-list
                                           nil nil nil nil "TERM")))
                (setq signal (if (string-match "^\\(\\S-+\\)\\s-" tmp)
                                 (match-string 1 tmp) tmp))))))
        ;; send signal
        (apply 'call-process proced-kill-program nil 0 nil
               (concat "-" (if (numberp signal)
                               (number-to-string signal) signal))
               (mapcar 'car plist))
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

(provide 'proced)

;; arch-tag: a6e312ad-9032-45aa-972d-31a8cfc545af
;;; proced.el ends here.
