(in-package :lem/legit)

#|
Done:

- start a rebase process from the commit at point,
  - abort the ongoing rebase:
   - with C-c C-k in the interactive buffer
   - or "r a" (M-x rebase-abort), which works when the rebase was started by another process.
  - continue the ongoing rebase:
   - with C-c C-c in the interactive buffer
   - or "r c" (M-x rebase-continue)
  - skip it with "r s"
- open a rebase buffer and press p, f… to pick, fixup… the commit at point.
- validate it, stop it.
- show in legit-status when a rebase is in process.

Nice to have:

- in the rebase buffer, show the commit diff on the right window, just like legit-status.
- create a major mode for git rebase files (we currently rely on yaml mode).
- prompt for confirmation in rebase-abort-yes-or-no

TODOs:

- when (e)dit or (r)eword are used, we need to handle another operation.
- show rebase conflicts: when a rebase is interrupted with conflicts we don't see them in legit-status.

and

- Windows support for the rebase script (trap a signal) (see porcelain).

|#

;; xxx: define a major mode for the git rebase file format,
;; where we would highlight special words and commits.
(define-major-mode legit-rebase-mode lem-yaml-mode:yaml-mode
    (:name "legit-rebase-mode"
     :syntax-table lem-yaml-mode::*yaml-syntax-table*
     :keymap *legit-rebase-mode-keymap*)
  (setf (variable-value 'enable-syntax-highlight) t))

(define-file-associations legit-rebase-mode
  ((:file-namestring "git-rebase-todo")
   (:file-namestring "git-rebase-todo.backup")))


;; Use commits with a keypress:
(define-key *legit-rebase-mode-keymap* "p" 'rebase-mark-line-pick)
(define-key *legit-rebase-mode-keymap* "r" 'rebase-mark-line-reword)
(define-key *legit-rebase-mode-keymap* "e" 'rebase-mark-line-edit)
(define-key *legit-rebase-mode-keymap* "s" 'rebase-mark-line-squash)
(define-key *legit-rebase-mode-keymap* "f" 'rebase-mark-line-fixup)
(define-key *legit-rebase-mode-keymap* "x" 'rebase-mark-line-exec)
(define-key *legit-rebase-mode-keymap* "b" 'rebase-mark-line-break)
(define-key *legit-rebase-mode-keymap* "d" 'rebase-mark-line-drop)
(define-key *legit-rebase-mode-keymap* "l" 'rebase-mark-line-label)
(define-key *legit-rebase-mode-keymap* "t" 'rebase-mark-line-reset)
(define-key *legit-rebase-mode-keymap* "m" 'rebase-mark-line-merge)

;; Validate, abort.
(define-key *legit-rebase-mode-keymap* "C-c C-c" 'rebase-continue)
(define-key *legit-rebase-mode-keymap* "C-Return" 'rebase-continue)
(define-key *legit-rebase-mode-keymap* "M-q" 'rebase-abort)
(define-key *legit-rebase-mode-keymap* "C-c C-k" 'rebase-abort)
;; xxx: with validation.
(define-key *legit-rebase-mode-keymap* "Escape" 'rebase-abort-yes-or-no)
(define-key *legit-rebase-mode-keymap* "q" 'rebase-abort-yes-or-no)

;; Navigation.
(define-key *legit-rebase-mode-keymap* "n" 'next-line)
(define-key *legit-rebase-mode-keymap* "C-n" 'next-line)
(define-key *legit-rebase-mode-keymap* "C-p" 'previous-line)

;; Help.
(define-key *legit-rebase-mode-keymap* "?" 'rebase-help)
(define-key *legit-rebase-mode-keymap* "C-x ?" 'rebase-help)

(define-command rebase-abort () ()
  (with-current-project (vcs)
    (run-function (lambda () (lem/porcelain:rebase-abort vcs)))
    (when (get-buffer "git-rebase-todo")
      (kill-buffer "git-rebase-todo"))
    (message "rebase aborted.")))

(define-command rebase-continue () ()
  (with-current-project (vcs)
    (run-function (lambda () (lem/porcelain:rebase-continue vcs)))
    (when (get-buffer "git-rebase-todo")
      (kill-buffer "git-rebase-todo"))))

(define-command rebase-skip () ()
  (with-current-project (vcs)
    (run-function (lambda () (lem/porcelain:rebase-skip vcs)))
    (when (get-buffer "git-rebase-todo")
      (kill-buffer "git-rebase-todo"))))

(defun %rebase-change-command (command)
  "Insert this command (string, such as \"fixup\") at the beginning of this line."
  (save-excursion
    (move-to-beginning-of-line)
    (delete-word 1)
    (insert-string (current-point) command)
    (save-buffer (current-buffer)))
  (move-to-next-virtual-line (current-point)))

(define-command rebase-mark-line-pick () ()
  (%rebase-change-command "pick"))

(define-command rebase-mark-line-reword () ()
  (%rebase-change-command "reword"))

(define-command rebase-mark-line-edit () ()
  (%rebase-change-command "edit"))

(define-command rebase-mark-line-squash () ()
  (%rebase-change-command "squash"))

(define-command rebase-mark-line-fixup () ()
  (%rebase-change-command "fixup"))

(define-command rebase-mark-line-exec () ()
  (%rebase-change-command "exec"))

(define-command rebase-mark-line-break () ()
  (%rebase-change-command "break"))

(define-command rebase-mark-line-drop () ()
  (%rebase-change-command "drop"))

(define-command rebase-mark-line-label () ()
  (%rebase-change-command "label"))

(define-command rebase-mark-line-reset () ()
  (%rebase-change-command "reset"))

(define-command rebase-mark-line-merge () ()
  (%rebase-change-command "merge"))

(define-command rebase-help () ()
  "Show the important keybindings."
  (with-pop-up-typeout-window (s (make-buffer "*legit-help*") :erase t)
    (format s "Git interactive rebase.~&")
    (format s "~%")
    (format s "Commands:~&")
    (format s "(p)ick commit, (f)ixup... WARN: other commands like reword are not implemented.")
    (format s "~%")
    (format s "Validate: C-Return, C-c C-c~&")
    (format s "Abort: C-c C-k~&")
    (format s "Stop and quit: Escape, M-q.~&")
    (format s "Navigate: C-n and C-p.~&")
    (format s "~%")
    (format s "Show this help: C-x ? or ?, M-x legit-rebase-help")
    ))
