
(in-package :lem/legit)

#|
Legit: an interactive interface to Git, with preliminary support for other version-control systems (Fossil, Mercurial).

We display version control data of the current project in an interactive two-panes window.

This file in particular defines the right window of the legit interface and the user-level commands.

It gets VCS data by calling lem/porcelain and asking functions on peek-legit.lisp to display data on the left window.


Done:

- status window: current branch, latest commits, unstaged changes, staged changes, untracked files.
- navigation commands
- view changes diff
- stage, unstage files
- inside the diff, stage, unstage hunks
- discard an unstaged file
- commit, redact a commit text in its dedicated buffer
- push, pull the remote branch
- branch checkout, branch create&checkout
- view commit at point
- rebase interactively (see legit-rebase)
- basic Fossil support (current branch, add change, commit)
- basic Mercurial support
- show the commits log, with pagination
- view stashes, stash push, pop and drop stash at point

Ongoing:

- interactive rebase (POC working for Unix). See more in legit-rebase.lisp

### See also

- https://github.com/fiddlerwoaroof/cl-git native CL, no wrapper around libgit.
- https://github.com/russell/cl-git/ wrapper around libgit2.
- http://shinmera.github.io/legit/ rename that lib and see how useful its caching can be.

|#

(defvar *ignore-all-space* nil "If non t, show all spaces in a diff. Spaces are ignored by default.

Currently Git-only. Concretely, this calls Git with the -w option.")

(defvar *show-stashes* t "List stashes on the Legit status buffer.")


;; Supercharge patch-mode with our keys.
(define-major-mode legit-diff-mode lem-patch-mode:patch-mode
    (:name "legit-diff"
     :syntax-table lem-patch-mode::*patch-syntax-table*
     :keymap *legit-diff-mode-keymap*)
  (setf (variable-value 'enable-syntax-highlight) t))

(define-minor-mode legit-commits-log-mode
    (:name "Log"
     :keymap *legit-commits-log-keymap*)
  (setf (not-switchable-buffer-p (current-buffer)) t))

;; git commands.
;; Some are defined on peek-legit too.
(define-key *global-keymap* "C-x g" 'legit-status)
(define-key *legit-diff-mode-keymap* "s" 'legit-stage-hunk)
(define-key *legit-diff-mode-keymap* "u" 'legit-unstage-hunk)
(define-key *legit-diff-mode-keymap* "n" 'legit-goto-next-hunk)
(define-key *legit-diff-mode-keymap* "p" 'legit-goto-previous-hunk)

(define-key *legit-diff-mode-keymap* "c" 'legit-commit)
(define-key *peek-legit-keymap* "c" 'legit-commit)

(define-key *peek-legit-keymap* "b b" 'legit-branch-checkout)
(define-key *legit-diff-mode-keymap* "b b" 'legit-branch-checkout)
(define-key *peek-legit-keymap* "b c" 'legit-branch-create)
(define-key *legit-diff-mode-keymap* "b c" 'legit-branch-create)
;; push
(define-key *legit-diff-mode-keymap* "P p" 'legit-push)
(define-key *peek-legit-keymap* "P p" 'legit-push)
;; pull
(define-key *peek-legit-keymap* "F p" 'legit-pull)
(define-key *legit-diff-mode-keymap* "F p" 'legit-pull)

;; commits log
(define-key *peek-legit-keymap* "l l" 'legit-commits-log)
(define-key *peek-legit-keymap* "l F" 'legit-commits-log-last-page)
(define-key *legit-diff-mode-keymap* "l l" 'legit-commits-log)

;; only in commits log view
(define-key *legit-commits-log-keymap* "n" 'peek-legit-next)
(define-key *legit-commits-log-keymap* "p" 'peek-legit-previous)

(define-key *legit-commits-log-keymap* "f" 'legit-commits-log-next-page)
(define-key *legit-commits-log-keymap* "b" 'legit-commits-log-previous-page)
(define-key *legit-commits-log-keymap* "F" 'legit-commits-log-last-page)
(define-key *legit-commits-log-keymap* "B" 'legit-commits-log-first-page)
(define-key *legit-commits-log-keymap* "q" 'legit-status)  ;; could we save and re-display
                                                           ;; the status buffer?
(define-key *legit-commits-log-keymap* "Q" 'legit-quit)
(define-key *legit-commits-log-keymap* "?" 'legit-logs-help)

;; rebase
;;; interactive
(define-key *peek-legit-keymap* "r i" 'legit-rebase-interactive)
(define-key *peek-legit-keymap* "r a" 'rebase-abort)
(define-key *peek-legit-keymap* "r c" 'rebase-continue)
(define-key *peek-legit-keymap* "r s" 'rebase-skip)

;; Stashes
(define-key *peek-legit-keymap* "z z" 'legit-stash-push)
(define-key *peek-legit-keymap* "z p" 'legit-stash-pop)

;; redraw everything:
(define-key *peek-legit-keymap* "g" 'legit-status)

;; navigation
(define-key *legit-diff-mode-keymap* "C-n" 'next-line)
(define-key *legit-diff-mode-keymap* "C-p" 'previous-line)
(define-key *peek-legit-keymap* "M-n" 'legit-next-header)
(define-key *peek-legit-keymap* "M-p" 'legit-previous-header)
(define-key *legit-diff-mode-keymap* "Tab" 'next-window)
(define-key *legit-diff-mode-keymap* "Return" 'legit-jump-to-hunk)

;; help
(define-key *peek-legit-keymap* "?" 'legit-help)
(define-key *peek-legit-keymap* "C-x ?" 'legit-help)
;; quit
(define-key *legit-diff-mode-keymap* "q" 'legit-quit)
(define-key *peek-legit-keymap* "q" 'legit-quit)
(define-key *legit-diff-mode-keymap* "M-q" 'legit-quit)
(define-key *peek-legit-keymap* "M-q" 'legit-quit)
(define-key *legit-diff-mode-keymap* "Escape" 'legit-quit)
(define-key *peek-legit-keymap* "Escape" 'legit-quit)
(define-key *legit-diff-mode-keymap* "C-c C-k" 'legit-quit)
(define-key *peek-legit-keymap* "C-c C-k" 'legit-quit)


(defmethod execute :after ((mode legit-commits-log-mode) command argument)
  "After moving around the commit lines with n and p, show the commit diff on the right window."
  (when (eq (current-window) *peek-window*)
    (show-matched-line)))

(defun pop-up-message (message)
  (with-pop-up-typeout-window (s (make-buffer "*legit status*") :erase t)
    (format s "~a" message)))

(defun last-character (s)
  (subseq s (- (length s) 2) (- (length s) 1)))

;;; Git commands
;;; that operate on files.
;;;
;;; Global commands like commit need not be defined here.

;; diff
(defun show-diff (diff)
  ;; Show a diff in the *legit-diff* buffer.
  ;; Share usage between showing file diff ("move" function)
  ;; and showing a commit.
  (let ((buffer (make-buffer "*legit-diff*")))
    (setf (buffer-directory buffer)
          (uiop:getcwd))
    (setf (buffer-read-only-p buffer) nil)
    (erase-buffer buffer)
    (move-to-line (buffer-point buffer) 1)
    (change-buffer-mode buffer 'legit-diff-mode)
    (insert-string (buffer-point buffer) diff)
    (setf (buffer-read-only-p buffer) t)
    (move-to-line (buffer-point buffer) 1)))

(defun make-stash-show-function (stash)
  (lambda ()
    (with-current-project (vcs)
      (cond
        ((and (numberp stash)
              (not (minusp stash)))
         (show-diff (lem/porcelain:stash-show vcs :position stash)))
        (t
         (show-diff (format nil "=== this stash reference is not valid: ~s" stash)))))))

(defun make-stash-pop-function (stash)
  (lambda ()
    (with-current-project (vcs)
      (cond
        ((and (numberp stash)
              (not (minusp stash)))
         (lem/porcelain:stash-pop vcs :position stash)
         )
        (t
         (message (format nil "=== this stash reference is not valid: ~s" stash)))))))

(defun make-stash-drop-function (stash)
  (lambda ()
    (with-current-project (vcs)
      (cond
        ((and (numberp stash)
              (not (minusp stash)))
         (when (prompt-for-y-or-n-p "Drop stash? "))
             (lem/porcelain:stash-drop vcs :position stash))
        (t
         (message (format nil "=== this stash reference is not valid: ~s" stash)))))))

(defun make-diff-function (file &key cached type)
  (lambda ()
    (with-current-project (vcs)
      (cond
        ((eq type :deleted)
         (show-diff (format nil "File ~A has been deleted." file)))
        ((and (not cached) (not type))
         (cond
           ((uiop:file-exists-p file)
            (handler-case
                (show-diff (format nil "=== Untracked file: ~A ===~%~%~A"
                                   file
                                   (uiop:read-file-string file)))
              (error ()
                (show-diff (format nil "=== Untracked file: ~A ===~%~%Unable to read file contents. It may be a binary file."
                                   file)))))
           ((uiop:directory-exists-p file)
            (let* ((absolute-file (uiop:ensure-absolute-pathname file (uiop:getcwd)))
                   (contents (append (uiop:directory-files absolute-file)
                                     (uiop:subdirectories absolute-file)))
                   (relative-paths (mapcar (lambda (path)
                                             (format nil "~A~A"
                                                     file
                                                     (enough-namestring path absolute-file)))
                                           contents)))
              (show-diff (format nil "=== Untracked directory: ~A ===~%~%~{~A~%~}"
                                 absolute-file
                                 relative-paths))))
           (t
            (show-diff (format nil "~A does not exist." file)))))
        (t
         (show-diff (lem/porcelain:file-diff vcs file :cached cached)))))))

(defun make-visit-file-function (file)
  ;; note: the lambda inside the loop is not enough, it captures the last loop value.
  (lambda ()
    file))

;; show commit.
(defun make-show-commit-function (ref)
  (lambda ()
    (with-current-project (vcs)
      (show-diff (lem/porcelain:show-commit-diff vcs ref :ignore-all-space *ignore-all-space*)))))

;; stage
(defun make-stage-function (file)
  (lambda ()
    (with-current-project (vcs)
      (lem/porcelain:stage vcs file)
      t)))

;; unstage
(defun make-unstage-function (file &key already-unstaged)
  (lambda ()
    (with-current-project (vcs)
      (if already-unstaged
          (message "Already unstaged")
          (lem/porcelain:unstage vcs file)))))

;; discard an unstaged change.
(defun make-discard-file-function (file &key is-staged)
  "Discard changes to an unstaged file.

  If is-staged is not nil, then message the user that this file must be unstaged."
  (lambda ()
    (cond
      (is-staged
       (message "Unstage the file first"))
      (t
       (with-current-project (vcs)
         (when (prompt-for-y-or-n-p  (format nil "Discard unstaged changes in ~a?" file))
           (lem/porcelain:discard-file vcs file)))))))


;;;
;;; Git commands
;;; that operate on diff hunks.
;;;

(defun %hunk-start-point (start?)
  "Find the start of the current hunk.
  It is the beginning of the previous \"@@ \" line.
  Return a point, or nil."
  (line-start start?)
  (if (str:starts-with-p "@@ " (line-string start?))
      start?
      (save-excursion
       (let ((point (search-backward-regexp start? "^\@\@")))
         (when point
           point)))))

(defun %hunk-end-point (end?)
  "Find the end of the current hunk.
  It is the last point of the line preceding the following \"@@ \" line,
  or the end of buffer."
  (line-start end?)
  (if (str:starts-with-p "@@ " (line-string end?))
      ;; start searching from next line.
      (setf end?
            (move-to-next-virtual-line end?)))
  (move-point
   end?
   (or
    (and
     (search-forward-regexp end? "^\@\@")
     (line-offset end? -1)
     (line-end end?)
     end?)
    (move-to-end-of-buffer))))

(defun %current-hunk ()
  "Get the current diff hunk at point.
  Return: string.

  To find a hunk, the point has to be inside one,
  i.e, after a line that starts with \"@@\"."
  ;; We are inside a diff (patch) buffer.
  ;; Get the headers and hunk at point to create a patch,
  ;; and we apply the patch.
  ;;
  ;; Steps to stage hunks are:
  ;; - create a patch file
  ;;   - ensure it respects git's format (ends with a space, the first line character is meaningful)
  ;; - apply it to the index
  ;; and that's it.
  ;;
  ;; Idea:
  ;; To get the list of hunk lines, simply check what lines start with "@@ "
  ;; save the lines index, and move the point to the closest line.
  ;; We would NOT need to tediously move points to find lines.

  (save-excursion
   (with-point ((keypresspoint (copy-point (current-point))))
     ;; The first 4 lines are the patch header.
     (let* ((diff-text (buffer-text (point-buffer keypresspoint)))
            (diff-lines (str:lines diff-text))
            (header (str:unlines (subseq diff-lines 0 4)))
            hunk
            patch)
       ;; Get hunk at point.
       (with-point ((start (copy-point keypresspoint) ) ;; @@
                    (start? (copy-point keypresspoint))
                    (end (copy-point keypresspoint))
                    (end? (copy-point keypresspoint)))
         (setf start (%hunk-start-point start?))
         (unless start
           (message "No hunk at point.")
           (return-from %current-hunk))
         (setf end (%hunk-end-point end?))

         (setf hunk (points-to-string start end))
         (setf patch (str:concat header
                                 (string #\newline)
                                 hunk))

         (when (not (equal " " (last-character patch)))
           ;; important for git patch.
           (setf patch (str:join "" (list patch (string #\newline) " "))))

         ;; Delete current hunk in diff buffer, place cursor on next one.
         (when (and start end)
           (setf (buffer-read-only-p (current-buffer)) nil)
           (delete-character start (count-characters start end))
           ;; delete a remaining newline character and we are on the next hunk line.
           (delete-character start 1)
           (setf (buffer-read-only-p (current-buffer)) t))

         patch)))))

(defun run-function (fn &key message)
  "Run this function and show `message` and standard output
  to the user on success as a tooltip message,
  or show the external command's error output on a popup window.

  The function FN returns up to three values:

   - standard output (string)
   - error output (string)
   - exit code (integer)

  Use with-current-project in the caller too.
  Typically used to run an external process in the context of a diff buffer command."
  (multiple-value-bind (output error-output exit-code)
      (funcall fn)
    (cond
      ((zerop exit-code)
       (let ((msg (str:join #\newline (remove-if #'null (list message output)))))
         (when (str:non-blank-string-p msg)
           (message msg))))
      (t
       (when error-output
         (pop-up-message error-output))))))

(define-command legit-stage-hunk () ()
  (with-current-project (vcs)
    (run-function (lambda ()
                    (lem/porcelain:apply-patch vcs (%current-hunk)))
                  :message "Staged hunk")))

(define-command legit-unstage-hunk () ()
  (with-current-project (vcs)
    (run-function (lambda ()
                    (lem/porcelain:apply-patch vcs (%current-hunk) :reverse t))
                  :message "Unstaged hunk")))

(define-command legit-goto-next-hunk () ()
  "Move point to the next hunk line, if any."
  (let* ((point (copy-point (current-point)))
         (end? (copy-point (current-point)))
         (end (move-to-next-virtual-line
               (move-point (current-point) (%hunk-end-point point)))))
    (if (equal end (buffer-end end?))
        point
        end)))

(define-command legit-goto-previous-hunk () ()
  "Move point to the previous hunk line, if any."
  (let* ((point (copy-point (current-point)))
         (start? (move-to-previous-virtual-line
                  (copy-point (current-point))))
         (start (when start?
                  (%hunk-start-point start?))))
    (when start
      (move-point (current-point) start)
      (if (equal start (buffer-start start?))
          point
          start))))

(define-command legit-jump-to-hunk () ()
  "Jump to the corresponding line in the source file for the current hunk."
  (let ((first-line (with-point ((p (current-point)))
                      (buffer-start p)
                      (line-string p))))
    (when (str:starts-with-p "diff" first-line)
      (let* ((hunk-text (%current-hunk))
             (lines (str:lines hunk-text))
             (file-line (find-if (lambda (line) (str:starts-with? "+++ b/" line)) lines))
             (hunk-header (find-if (lambda (line) (str:starts-with? "@@ " line)) lines)))
        (if (and file-line hunk-header)
            (let* ((relative-file (if (str:starts-with-p "diff --git" first-line)
                                      (subseq file-line 6)  ; Remove "+++ b/" prefix for Git
                                      (cl-ppcre:register-groups-bind (file-path)
                                          ("\\+\\+\\+ b/([^\\t]+)" file-line)
                                        file-path)))  ; For Mercurial (also has datetime)
                   (start-line (cl-ppcre:register-groups-bind (nil line)
                                   ("@@ -(\\d+),\\d+ \\+(\\d+)" hunk-header)
                                 line))
                   (target-line (when start-line
                                  (+
                                   (parse-integer start-line :junk-allowed t)
                                   lem/porcelain:*diff-context-lines*))))
              (if (and relative-file target-line)
                  (with-current-project (vcs)
                    (declare (ignore vcs))
                    (let ((absolute-file (merge-pathnames relative-file (uiop:getcwd))))
                      (%legit-quit)
                      (find-file (namestring absolute-file))
                      (goto-line target-line)))
                  (message "Could not determine file or line number")))
            (message "Could not parse hunk information"))))))

(defparameter *commit-buffer-message*
  "~%~%# Please enter the commit message for your changes.~%~
  # Lines starting with '#' will be discarded, and an empty message does nothing.~%~
  # Validate with C-c C-c, quit with M-q or C-c C-k")

(define-command legit-commit () ()
  "Write a commit message in its dedicated buffer.

  In this buffer, use C-c to validate, M-q or C-c C-k to quit."

  ;; The git command accepts a commit message as argument (-m),
  ;; but also a simple "git commit" starts an editing process, with a pre-formatted
  ;; help text. As with the interactive rebase process, we would need to:
  ;; - start the commit process with a dummy editor,
  ;; - on validation kill the dummy editor and let the git process continue (at this moment git itself decides to validate or to ignore the message).
  ;; This is used by Magit, and we do this for the interactive rebase, but:
  ;; - our dummy editor script doesn't support windows (still as of <2023-09-22 Fri>).
  ;;
  ;; So we go with a simpler, cross-platform and pure Lem/Lisp workflow:
  ;; - create a Lem buffer, add some help text
  ;; - on validation, check ourselves that the message isn't void, extract other information (signature…) and run the commit, with the -m argument.

  (let ((buffer (make-buffer "*legit-commit*")))
    (setf (buffer-directory buffer) (buffer-directory))
    (setf (buffer-read-only-p buffer) nil)
    (erase-buffer buffer)
    (move-to-line (buffer-point buffer) 1)
    (insert-string (buffer-point buffer)
                   (format nil *commit-buffer-message*))
    (change-buffer-mode buffer 'legit-commit-mode)
    (move-to-line (buffer-point buffer) 1)

    ;; The Legit command, like grep, creates its own window.
    ;; Where is it best to show the commit buffer?
    ;; 1) quit the legit view altogether, open the commit buffer in full height:
    ;; (lem/legit::legit-quit)
    ;; 2) open the commit buffer on the left instead of legit status (and nice to have: show the full changes on the right)
    ;; (setf (not-switchable-buffer-p (current-buffer)) nil)
    ;; 3) open the commit buffer on the right, don't touch the ongoing legit status.
    (next-window)
    (switch-to-buffer buffer)))


(define-command legit-status () ()
  "Show changes, untracked files, stashes and latest commits in an interactive window."
  (with-current-project (vcs)
    (multiple-value-bind (untracked-files unstaged-files staged-files)
        (lem/porcelain:components vcs)

      ;; big try! It works \o/
      (with-collecting-sources (collector :read-only nil
                                          :minor-mode 'peek-legit-mode)
        ;; (if we don't specify the minor-mode, the macro arguments's default value will not be found)
        ;;
        ;; Header: current branch.
        (collector-insert
         (format nil "Branch: ~a" (lem/porcelain:current-branch vcs))
         :header t)
        (collector-insert "")

        ;; Is a git rebase in progress?
        (let ((rebase-status (lem/porcelain::rebase-in-progress-p vcs)))
          (when (getf rebase-status :status)
            (collector-insert
             (format nil "!rebase in progress: ~a onto ~a"
                     (getf rebase-status :head-short-name)
                     (getf rebase-status :onto-short-commit)))
            (collector-insert "")))

        ;; Untracked files.
        (collector-insert (format nil "Untracked files (~a):" (length untracked-files)) :header t)
        (if untracked-files
            (loop :for file :in untracked-files
                  :do (with-appending-source
                          (point :move-function (make-diff-function file)
                                 :visit-file-function (make-visit-file-function file)
                                 :stage-function (make-stage-function file)
                                 :unstage-function (lambda () (message "File is not tracked, can't be unstaged.")))
                        (insert-string point file :attribute 'filename-attribute :read-only t)))
            (collector-insert "<none>"))


        ;; Stashes.
        (collector-insert "")
        (let ((stashes (lem/porcelain:stash-list vcs)))
          (collector-insert (format nil "Stashes (~a)" (length stashes)) :header t)
          (when *show-stashes*
            (loop :for line :in stashes
                  :for position := 0 :then (incf position)
                  :do (with-appending-source
                          (point :move-function (make-stash-show-function position)
                                 :visit-file-function (lambda ()
                                                        (message "Apply this stash with (s)")
                                                        ;; Have a side effect,
                                                        ;; don't try to open a file.
                                                        (values))
                                 :stage-function (make-stash-pop-function position)
                                 :discard-file-function (make-stash-drop-function position))
                        (insert-string point line
                                       :attribute 'filename-attribute :read-only t)))))

        ;; Unstaged changes
        (collector-insert "")
        (collector-insert (format nil "Unstaged changes (~a):" (length unstaged-files)) :header t)
        (if unstaged-files
            (loop for file-info in unstaged-files
                  for file = (getf file-info :file)
                  for type = (getf file-info :type)
                  do (with-appending-source
                         (point :move-function (make-diff-function file :type type)
                                :visit-file-function (make-visit-file-function file)
                                :stage-function (make-stage-function file)
                                :unstage-function (make-unstage-function file :already-unstaged t)
                                :discard-file-function (make-discard-file-function file))
                       (insert-string point
                                      (format nil "~10a ~a"
                                              (case type
                                                (:modified "modified")
                                                (:deleted "deleted")
                                                (t ""))
                                              file)
                                      :attribute 'filename-attribute
                                      :read-only t)))
            (collector-insert "<none>"))

        ;; Staged changes
        (collector-insert "")
        (collector-insert (format nil "Staged changes (~a):" (length staged-files)) :header t)
        (if staged-files
            (loop for file-info in staged-files
                  for file = (getf file-info :file)
                  for type = (getf file-info :type)
                  do (with-appending-source
                         (point :move-function (make-diff-function file :cached t :type type)
                                :visit-file-function (make-visit-file-function file)
                                :stage-function (make-stage-function file)
                                :unstage-function (make-unstage-function file)
                                :discard-file-function (make-discard-file-function file :is-staged t))
                       (insert-string point
                                      (format nil "~10a ~a"
                                              (case type
                                                (:modified "modified")
                                                (:added "created")
                                                (:deleted "deleted")
                                                (t ""))
                                              file)
                                      :attribute 'filename-attribute
                                      :read-only t)))
            (collector-insert "<none>"))

        ;; Latest commits.
        (collector-insert "")
        (collector-insert "Latest commits:" :header t)
        (let ((latest-commits (lem/porcelain:latest-commits vcs)))
          (if latest-commits
              (loop for commit in latest-commits
                    for line = nil
                    for hash = nil
                    for message = nil
                    if (consp commit)
                      do (setf line (getf commit :line))
                         (setf hash (getf commit :hash))
                         (setf message (getf commit :message))
                    else
                      do (setf line commit)

                    do (with-appending-source
                           (point :move-function (make-show-commit-function hash)
                                  :visit-file-function (lambda ())
                                  :stage-function (lambda () )
                                  :unstage-function (lambda () ))
                         (with-point ((start point))
                           (when hash
                             (insert-string point hash :attribute 'filename-attribute :read-only t))
                           (if message
                               (insert-string point message)
                               (insert-string point line))

                           ;; Save the hash on this line for later use.
                           (when hash
                             (put-text-property start point :commit-hash hash)))))
              (collector-insert "<none>")))

        (add-hook (variable-value 'after-change-functions :buffer (collector-buffer collector))
                  'change-grep-buffer)))))

(define-command legit () ()
  "Show changes, untracked files and latest commits in an interactive window.

  Calls M-x legit-status."
  (legit-status))

(defun prompt-for-branch (vcs &key prompt initial-value)
  ;; only call from a command.
  (let* ((current-branch (or initial-value (lem/porcelain:current-branch vcs)))
         (candidates (lem/porcelain:branches vcs)))
    (if candidates
        (prompt-for-string (or prompt "Branch: ")
                           :initial-value current-branch
                           :history-symbol '*legit-branches-history*
                           :completion-function (lambda (x) (completion-strings x candidates))
                           :test-function (lambda (name) (member name candidates :test #'string=)))
        (message "No branches"))))

(define-command legit-branch-checkout () ()
  "Choose a branch to checkout."
  (with-current-project (vcs)
    (let ((branch (prompt-for-branch vcs))
          (current-branch (lem/porcelain:current-branch vcs)))
      (when (equal branch current-branch)
        (show-message (format nil "Already on ~a" branch) :timeout 3)
        (return-from legit-branch-checkout))
      (when branch
        (run-function (lambda ()
                        (lem/porcelain:checkout vcs branch))
                      :message (format nil "Checked out ~a" branch))
        (legit-status)))))

(define-command legit-branch-create () ()
  "Create and checkout a new branch."
  (with-current-project (vcs)
    (let ((new (prompt-for-string "New branch name: "
                                  :history-symbol '*new-branch-name-history*))
          (base (prompt-for-branch vcs :prompt "Base branch: " :initial-value "")))
      (when (and new base)
        (run-function (lambda ()
                        (lem/porcelain:checkout-create vcs new base))
                      :message (format nil "Created ~a" new))
        (legit-status)))))

(define-command legit-pull () ()
  "Pull changes, update HEAD."
  (with-current-project (vcs)
    (run-function (lambda () (lem/porcelain:pull vcs)))))

(define-command legit-push () ()
  "Push changes to the current remote."
  (with-current-project (vcs)
    (run-function (lambda ()
                    (lem/porcelain:push-default vcs))
                  :message "Done")))

(define-command legit-rebase-interactive () ()
  "Rebase interactively, from the commit the point is on.

  Austostash pending changes, to enable the rebase and find the changes back afterwards."
  (with-current-project (vcs)

    ;; Find the commit hash the point is on: mandatory.
    (let ((commit-hash (text-property-at (current-point) :commit-hash)))

      (unless commit-hash
        (message "Not on a commit line?")
        (return-from legit-rebase-interactive))

      (run-function (lambda ()
                      (lem/porcelain::rebase-interactively vcs :from commit-hash)))

      (let ((buffer (find-file-buffer ".git/rebase-merge/git-rebase-todo")))
        (when buffer
          (%legit-quit)
          (switch-to-buffer buffer)
          (change-buffer-mode buffer 'legit-rebase-mode))))))

(define-command legit-next-header () ()
  "Move point to the next header of this VCS window."
  (peek-legit-next-header))

(define-command legit-previous-header () ()
  "Move point to the previous header of this VCS window."
  (peek-legit-previous-header))

(define-command legit-commits-log () ()
  "List commits on a new buffer."
  (with-current-project (vcs)
    (display-commits-log vcs 0)))

(defun display-commits-log (vcs offset)
  "Display the commit lines on a dedicated legit buffer."
  (let* ((commits (lem/porcelain:commits-log vcs :offset offset :limit lem/porcelain:*commits-log-page-size*)))
    (with-collecting-sources (collector :buffer :commits-log
                                                       :minor-mode 'legit-commits-log-mode
                                                       :read-only nil)
      (collector-insert
       (format nil "Commits (~A):" offset)
       :header t)
      (if commits
          (progn
            (loop for commit in commits
                  for line = nil
                  for hash = nil
                  for message = nil
                  if (consp commit)
                  do (setf line (getf commit :line)
                           hash (getf commit :hash)
                           message (getf commit :message))
                  else
                  do (setf line commit)
                  do (with-appending-source
                         (point :move-function (make-show-commit-function hash)
                                :visit-file-function (lambda ())
                                :stage-function (lambda () )
                                :unstage-function (lambda () ))
                       (with-point ((start point))
                         (when hash
                           (insert-string point hash :attribute 'filename-attribute :read-only t))
                         (if message
                             (insert-string point message)
                             (insert-string point line))
                         (when hash
                           (put-text-property start point :commit-hash hash)))))
            (setf (buffer-value (collector-buffer collector) 'commits-offset) offset))
          (collector-insert "<no commits>")))))

(define-command legit-commits-log-next-page () ()
  "Show the next page of the commits log."
  (with-current-project (vcs)
    (let* ((buffer (current-buffer))
           (current-offset (or (buffer-value buffer 'commits-offset) 0))
           (new-offset (+ current-offset lem/porcelain:*commits-log-page-size*))
           (commits (lem/porcelain:commits-log vcs
                                               :offset new-offset
                                               :limit lem/porcelain:*commits-log-page-size*)))
      (if commits
          (display-commits-log vcs new-offset)
          (message "No more commits to display.")))))

(define-command legit-commits-log-previous-page () ()
  "Show the previous page of the commits log."
  (with-current-project (vcs)
    (let* ((buffer (current-buffer))
           (current-offset (or (buffer-value buffer 'commits-offset) 0))
           (new-offset (max 0 (- current-offset lem/porcelain:*commits-log-page-size*))))
      (display-commits-log vcs new-offset))))

(define-command legit-commits-log-first-page () ()
  "Go to the first page of the commit log."
  (with-current-project (vcs)
    (display-commits-log vcs 0)))

(define-command legit-commits-log-last-page () ()
  "Go to the last page of the commit log."
  (with-current-project (vcs)
    (let* ((commits-per-page lem/porcelain:*commits-log-page-size*)
           (last-page-offset (* (floor (/ (1- (lem/porcelain:commit-count vcs)) commits-per-page))
                                commits-per-page)))
      (display-commits-log vcs last-page-offset))))

(define-command legit-stash-push () ()
  "Ask for a message and stash the current changes."
  (with-current-project (vcs)
    (let ((message (prompt-for-string "Stash message: ")))
      (lem/porcelain::stash-push vcs :message message)
      (legit-status))))

(define-command legit-stash-pop () ()
  "Pop the latest staged changes"
  (with-current-project (vcs)
    (let ((confirm (prompt-for-y-or-n-p "Pop the latest stash to the current branch? ")))
      (when confirm
        (lem/porcelain::stash-pop vcs)
        (legit-status)))))

(define-command legit-quit () ()
  "Quit"
  (%legit-quit)
  (ignore-errors
   (delete-buffer (get-buffer "*legit-diff*"))
   (delete-buffer (get-buffer "*legit-help*"))))

(define-command legit-help () ()
  "Show the important keybindings."
  (with-pop-up-typeout-window (s (make-buffer "*legit-help*") :erase t)
    (format s "Lem's interface to git. M-x legit-status (C-x g)~&")
    (format s "~%")
    (format s "Commands:~&")
    (format s "(s)tage and (u)nstage a file. Inside a diff, (s)tage or (u)nstage a hunk.~&")
    (format s "  pop the stash at point.~&")
    (format s "(k) discard changes, drop the stash at point.~&")
    (format s "(c)ommit~&")
    (format s "(b)ranches-> checkout another (b)ranch.~&")
    (format s "          -> (c)reate.~&")
    (format s "(l)og-> (l) commits log~&")
    (format s "     -> (F) first page of the commits history.~&")
    (format s "     Navigate commit pages with (b) and (f).~&")
    (format s "(F)etch, pull-> (p) from remote branch~&")
    (format s "(P)push      -> (p) to remote branch~&")
    (format s "(r)ebase     -> (i)nteractively from commit at point, (a)bort~&")
    (format s "(z) stashes  -> (z) stash changes (p)op latest stash~&")
    (format s "             -> also use (s) and (k) on a stash.~&")
    (format s "(g) -> refresh~&")
    (format s "~%")
    (format s "Navigate: n and p, C-n and C-p, M-n and M-p.~&")
    (format s "Change windows: Tab, C-x o, M-o~&")
    (format s "Quit: Escape, q, C-x 0.~&")
    (format s "~%")
    (format s "Show this help: C-x ? or ?, M-x legit-help~&")
    (format s "~%")
    (format s "You can customize:~&")
    (format s "~%")
    (format s "lem/legit:*show-stashes* : set to nil to not see the list of stashes in the status buffer~&")
    (format s "lem/porcelain:*nb-latest-commits* which defaults to 10~&")
    (format s "(and more)~&")
    ))

(define-command legit-logs-help () ()
  "Help for the commits log view."
  (with-pop-up-typeout-window (s (make-buffer "*legit-help*") :erase t)
    (format s "In this commits log buffer, use:~&")
    (format s "~%")
    (format s "(f) forward page~&")
    (format s "(b) backward one page~&")
    (format s "(F) go to the last page of the commits history~&")
    (format s "(B) come back to the first page of the commits history~&")
    (format s "(q) come back to the legit status~&")
    (format s "(Q) quit legit~&")
    (format s "~%")
    (format s "You can customize:~&")
    (format s "~%")
    (format s "lem/porcelain:*commits-log-page-size* which defaults to 200~&")
    ))
