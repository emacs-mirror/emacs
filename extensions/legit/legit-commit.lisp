(in-package :lem/legit)

#|
Done:

- "c" opens a commit message window on the right side, we can type a long message
  - this command is defined in legit.lisp.
- lines starting by a # are ignored.

TODOs:

- add C-c C-s to sign the message
- and other shortcuts and features

Future:

- save previous messages, add C-p C-n commands to show them
- find related Github issue when writing "fixes #"

|#

;; major-mode for the commit buffer.
(define-major-mode legit-commit-mode ()
    (:name "legit-commit-mode"
     :keymap *legit-commit-mode-keymap*)
  ;; no syntax highlight in fact.
  (setf (variable-value 'enable-syntax-highlight) t))

(define-file-associations legit-commit-mode
  ((:file-namestring "COMMIT_EDITMSG")))

;; User parameters.
(defparameter *prompt-to-abort-commit* t
  "If non t, abort the current commit message without asking for confirmation.")

;; Keys:
;; validate, abort.
(define-key *legit-commit-mode-keymap* "C-c C-c" 'commit-continue)
(define-key *legit-commit-mode-keymap* "C-Return" 'commit-continue)
(define-key *legit-commit-mode-keymap* "M-q" 'commit-abort)
(define-key *legit-commit-mode-keymap* "C-c C-k" 'commit-abort)

;; Nice to have: find and display the previous commit messages.
;; (define-key *legit-commit-mode-keymap* "C-n" 'next-commit)
;; (define-key *legit-commit-mode-keymap* "C-p" 'previous-commit)

;; Help.
(define-key *legit-commit-mode-keymap* "C-x ?" 'commit-help)

(defun clean-commit-message (text)
  "Remove lines starting with a #."
  ;; We should collect meaningful data too, like a signature.
  (loop for line in (str:lines text)
        unless (str:starts-with-p "#" line)
          collect line into result
        finally (return (str:unlines result))))

;; void message:
#+(or)
(assert (str:blankp (clean-commit-message
"# Please enter the commit message for your changes. Lines starting
# with '#' will be ignored, and an empty message aborts the commit.
")))

;; a message on the first line:
#+(or)
(assert (equal "test message" (clean-commit-message
"test message
# Please enter the commit message for your changes. Lines starting
# with '#' will be ignored, and an empty message aborts the commit.
")))

;; a few lines:
#+(or)
(assert (equal "one

two
" (clean-commit-message
"one

two

# Please enter the commit message for your changes. Lines starting
# with '#' will be ignored, and an empty message aborts the commit.
")))

(define-command commit-continue () ()
  "If the commit message is non-empty, commit, kill the commit buffer and come back to the legit status window.

  Lines starting with '#' are ignored."
  ;; The commit buffer can be:
  ;; - our *legit-commit* buffer
  ;; - the .git/COMMIT_EDITMSG file
  (let* ((message (buffer-text (current-buffer)))
         (cleaned-message (clean-commit-message message)))
    (cond
      ((str:blankp cleaned-message)
       (message "No commit message, do nothing."))
      (t
       (with-current-project (vcs)
         (run-function (lambda ()
                         (lem/porcelain::commit vcs cleaned-message))
                       :message "commited")
         (kill-buffer (current-buffer))
         ;; come back on the status on  the left:
         (lem-core/commands/window:previous-window)
         ;; and refresh.
         (legit-status))))))

(define-command commit-abort () ()
  (when (or (not *prompt-to-abort-commit*)
            (prompt-for-y-or-n-p "Abort commit?"))
    (lem-core/commands/window:previous-window)
    (kill-buffer "*legit-commit*")))

(define-command commit-help () ()
  "Show the important keybindings."
  (with-pop-up-typeout-window (s (make-buffer "*legit-help*") :erase t)
    (format s "Legit commit.~&")
    (format s "~%")
    (format s "Commands:~&")
    (format s "Validate: C-Return, C-c C-c~&")
    (format s "Stop and quit: Escape, M-q.~&")
    (format s "~%")
    (format s "Show this help: C-x ? or ?, M-x legit-commit-help")
    ))
