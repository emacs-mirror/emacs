(uiop:define-package :lem/porcelain/git
  (:use :cl
    ;; let's import all the classes and methods defined in the main porcelain:
    :lem/porcelain)
  (:import-from :trivial-types
                :proper-list)
  (:export :git-project-p)
  (:documentation "Implements the porcelain interface for git-based repos."))

(in-package :lem/porcelain/git)

(declaim (type (proper-list string) *git-base-arglist*))
(defvar *git-base-arglist* (list "git")
  "The git program, to be appended command-line options.")

(defvar *branch-sort-by* "-creatordate"
  "When listing branches, sort by this field name.
  Prefix with \"-\" to sort in descending order.

  Defaults to \"-creatordate\", to list the latest used branches first.")

(defvar *file-diff-args* (list "diff" "--no-color")
  "Arguments to display the file diffs.
  Will be surrounded by the git binary and the file path.

  For staged files, --cached is added by the command.")

(defvar *verbose* nil)

;; VCS implementation for git
(defclass vcs-git (vcs-project)
  ((rebase-pid :initform nil
               :accessor rebase-pid
               :type (or null string)
               :documentation "When a rebase is started, save its PID."))
  (:default-initargs
   :name "git"))


(defvar *projects-mapping* (make-hash-table :test #'equal)
  "Map a VCS project root to its VCS object.

  We need a single VCS object to retain data, such as the ongoing rebase PID.")

(defun get-or-create-project ()
  "If a vcs-git object exists for the current project root (as of
  the CWD, set by `with-current-project', return it. Otherwise, create a
  new `vcs-git' object instance.

  This is important to save the current rebase PID."
  (let* ((root (uiop:getcwd)) ;; CWD set by with-current-project.
         (existing (gethash root *projects-mapping*)))
    (or existing
        (setf (gethash root *projects-mapping*)
              (make-instance 'vcs-git)))))

(defun git-project-p ()
  "When we find a .git/ directory in the current directory (which should be the project root. Use `lem/porcelain:with-current-project`),
  return a VCS object representing the repo: otherwise, return nil"
  (when (uiop:directory-exists-p ".git")
    (get-or-create-project)))

(defun run-git (arglist)
  "Run the git command with these options (list).
  Return standard and error output as strings.

  Don't signal an error if the process doesn't exit successfully
  (but return the error message, so it can be displayed by the caller).
  However, if uiop:run-program fails to run the command, the interactive debugger
  is invoked (and shown correctly in Lem)."
  (uiop:run-program (concatenate 'list
                                 *git-base-arglist*
                                 (uiop:ensure-list arglist))
                    :output :string
                    :error-output :string
                    :ignore-error-status t))

(defun porcelain ()
  "Return the git status: for each file in the project, the `git status --porcelain` command
allows to learn about the file state: modified, deleted, ignored… "
  (run-git (list "status" "--porcelain=v1")))

(defmethod components ((vcs vcs-git))
  "Return 3 values:
  - untracked files
  - modified and unstaged files
  - modified and staged files

   Git manual:

   In the short-format, the status of each path is shown as one of these forms
           XY PATH
           XY ORIG_PATH -> PATH
   For paths with merge conflicts, X and Y show the modification states of each side of the
       merge. For paths that do not have merge conflicts, X shows the status of the index, and Y
       shows the status of the work tree. For untracked paths, XY are ??. Other status codes can
       be interpreted as follows:
       •   ' ' = unmodified
       •   M = modified
       •   A = added
       •   D = deleted
       •   R = renamed
       •   C = copied
       •   U = updated but unmerged"
  (loop for line in (str:lines (porcelain))
        for file = (subseq line 3)
        for status = (subseq line 0 2)
        unless (str:blankp line)
        if (equal (elt status 0) #\M)
        collect (list :file file :type :modified) into modified-staged-files
        if (equal (elt status 0) #\A)
        collect (list :file file :type :added) into modified-staged-files
        if (equal (elt status 0) #\D)
        collect (list :file file :type :deleted) into modified-staged-files
        if (equal (elt status 1) #\M)
        collect (list :file file :type :modified) into modified-unstaged-files
        if (equal (elt status 1) #\D)
        collect (list :file file :type :deleted) into modified-unstaged-files
        if (str:starts-with-p "??" status)
        collect file into untracked-files
        finally (return (values untracked-files
                                modified-unstaged-files
                                modified-staged-files))))

(defmethod checkout ((vcs vcs-git) branch)
  (run-git (list "checkout" branch)))

(defmethod file-diff ((vcs vcs-git) file &key cached)
  ;; --cached is a synonym for --staged.
  ;; So it is set only for staged files. From git-components: the 3rd value, modified and staged files.
  (run-git
   (concatenate 'list
                *file-diff-args*
                (list (format nil "-U~D" lem/porcelain:*diff-context-lines*))
                (if cached '("--cached"))
                (list file))))

(defmethod show-commit-diff ((vcs vcs-git) ref &key ignore-all-space)
  (let ((options '()))
    (when ignore-all-space
      (cl:push "-w" options))
    (run-git `("show" ,@options ,ref))))

(defmethod commit ((vcs vcs-git) message)
  (run-git (list "commit" "-m" message)))

(defun list-branches (&key (sort-by *branch-sort-by*))
  "Return: list of branches, raw output.
  Each element starts with two meaningful characters, such as \"* \" for the current branch."
  (str:lines
   (run-git (list "branch"
                  "--list"
                  "--no-color"
                  (format nil "--sort=~a" sort-by)))))

(defmethod branches ((vcs vcs-git) &key (sort-by *branch-sort-by*))
  (loop for branch in (list-branches :sort-by sort-by)
        collect (subseq branch 2 (length branch))))

(defmethod checkout-create ((vcs vcs-git) new start)
  (run-git (list "checkout" "-b" new start)))

(defmethod pull ((vcs vcs-git))
  ;; xxx: recurse submodules, etc.
  (run-git (list "pull")))

(defmethod push-default ((vcs vcs-git))
  (run-git (list "push")))

(defmethod current-branch ((vcs vcs-git))
  (let ((branches (list-branches :sort-by "-creatordate")))
    (loop for branch in branches
          if (str:starts-with-p "*" branch)
          return (subseq branch 2))))

(defmethod rebase-in-progress-p ((vcs vcs-git))
  (when (uiop:directory-exists-p ".git/rebase-merge/")
    (let ((head (str:trim (str:from-file ".git/rebase-merge/head-name")))
          (onto (str:trim (str:from-file ".git/rebase-merge/onto"))))
      (list :status t
            :head-name head
            :head-short-name (or (third (str:split "/" head))
                                 head)
            :onto onto
            :onto-short-commit (str:shorten 8 onto :ellipsis "")))))

(defun %git-list-latest-commits (&optional (n lem/porcelain:*nb-latest-commits*))
  (when (plusp n)
    (str:lines
     (run-git (list "log" "--pretty=oneline" "-n" (princ-to-string n))))))

(defmethod latest-commits ((vcs vcs-git) &key (n lem/porcelain:*commits-log-page-size*) (hash-length 8) (offset 0))
  (let* ((n-arg (when n (list "-n" (princ-to-string n))))
         (lines (str:lines
                 (run-git (append (list "log"
                                        "--pretty=oneline"
                                        "--skip" (princ-to-string offset))
                                  n-arg)))))
    (loop for line in lines
          for space-position = (position #\space line)
          for small-hash = (subseq line 0 hash-length)
          for message = (subseq line space-position)
          collect (list :line (concatenate 'string small-hash message)
                        :message message
                        :hash small-hash))))

(defmethod commits-log ((vcs vcs-git) &key (offset 0) limit (hash-length 8))
  (latest-commits vcs :n limit
                  :hash-length hash-length
                  :offset offset))

(defmethod commit-count ((vcs vcs-git))
  (parse-integer
   (str:trim (run-git '("rev-list" "--count" "HEAD")))))

(defmethod stage ((vcs vcs-git) file)
  (run-git (list "add" file)))

(defmethod unstage ((vcs vcs-git) file)
  "Unstage changes to a file."
  (run-git (list "reset" "HEAD" "--" file)))

(defmethod discard-file ((vcs vcs-git) file)
  "Discard all the changes to this file.

  This currently means: checkout this file."
  (run-git (list "checkout" file)))

(defun %maybe-lem-home ()
  "Return Lem's home directory by calling lem:lem-home only if the :lem package exists,
  otherwise return ~/.lem/.
  We don't want a hard-coded dependency on Lem in the porcelain package, to ease testing."
  (if (find-package :lem)
      (uiop:symbol-call :lem :lem-home)
      (merge-pathnames ".lem/" (user-homedir-pathname))))

(defmethod apply-patch ((vcs vcs-git) diff &key reverse)
  "Apply a patch file.
  This is used to stage hunks of files."
  (when *verbose*
    (log:info diff)
    (with-open-file (f (merge-pathnames "lem-hunk-latest.patch" (%maybe-lem-home))
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
      (write-sequence diff f)))

  ;; Write diff on file.
  ;; It should also be possible to give it on stdin ("git apply - ")
  ;; (uiop:with-temporary-file (:stream f) ;; issues with this.
  (with-open-file (f ".lem-hunk.patch"
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (write-sequence diff f)
    (finish-output f))

  (let ((base (list "apply"
                    "--ignore-space-change" ;; in context only.
                    "-C0" ;; easier to apply patch without context.
                    "--index"
                    "--cached"))
        (maybe (if reverse
                   (list "--reverse")
                   (list)))
        (arglist (list ".lem-hunk.patch")))
    (run-git (concatenate 'list base maybe arglist))))

(defun root-commit-p (hash)
  "Find this repository's very first commit on the current branch,
  return T if this commit hash is the root.

  hash: (string) can be a short commit hash or an entire one.

  This check is required when doing a git interactive rebase."
  ;; the git command
  ;; git rebase --interactive a1b2c3^
  ;; fails if a1b2c3 is the root commit.
  ;; We must use --root instead.
  (let ((root (run-git (list "rev-list"
                             "--max-parents=0"
                             "HEAD"))))
    ;; We use small hashes, so don't use equal.
    (str:starts-with-p hash root)))


;;
;; Git interactive rebase.
;;
;; -i --autostash
;; If rebase from first commit: use --root
;; otherwise use <commit-hash>^
;; exple: git rebase --autostash -i 317315966^
;; This creates a file in .git/rebase-merge/git-rebase-merge-todo
;; which we edit with Lem, and we validate the rebase process.
;;

(defvar *rebase-script-path*)

;; Save our script as a string at compile time.
(defparameter *rebase-script-content*
  #+(or lem-ncurses lem-sdl2)
  (str:from-file
   (asdf:system-relative-pathname (asdf:find-system "lem-legit")
                                  "scripts/dumbrebaseeditor.sh"))
  #-(or lem-ncurses lem-sdl2)
  ""
  "Our dumb editor shell script, saved as a string at compile time.
  We then save it to the user's ~/.lem/legit/rebaseetidor.sh at first use.")

(defun rebase-script-path ()
  (if (boundp '*rebase-script-path*)
      *rebase-script-path*
      (let* ((legit-path (merge-pathnames "legit/" (%maybe-lem-home)))
             (script-path (namestring (uiop:merge-pathnames* "dumbrebaseeditor.sh" legit-path))))
        (ensure-directories-exist legit-path)
        (unless (uiop:file-exists-p script-path)
          (str:to-file script-path *rebase-script-content*))
        ;; Ensure the file is executable.
        #+unix
        (uiop:run-program (list "chmod" "+x" script-path)
                          :output :string
                          :error-output :string
                          :ignore-error-status t)
        #-unix
        (porcelain-error "lem/legit: our rebase script is only for Unix platforms currently. We need to run a shell script and trap a signal.")
        (setf *rebase-script-path* script-path))))

(defmethod rebase-interactively ((vcs vcs-git) &key from)
  "Start a rebase session.

  Then edit the git rebase file and validate the rebase with `rebase-continue`
  or stop it with `rebase-abort`.

  from: commit hash (string) to start the rebase from.

  Return three values suitable for `legit:run-function`: output string, error output string, exit code (integer)."
  ;; For testing, go to a test project (,cd on Slime), and edit this project's
  ;; .git/rebase-merge/git-rebase-merge-todo
  ;; Beware of C-c ~ lol^^
  (when (uiop:directory-exists-p ".git/rebase-merge/")
    (porcelain-error "It seems that there is already a rebase-merge directory,
and I wonder if you are in the middle of another rebase.
If that is the case, please try
   git rebase (--continue | --abort | --skip)
If that is not the case, please
   rm -fr \".git/rebase-merge\"
and run me again.
I am stopping in case you still have something valuable there."))

  (unless from
    (return-from rebase-interactively
      (values "Git rebase is missing the commit to rebase from. We are too shy to rebase everything from the root commit yet. Aborting"
              nil
              1)))

  (let ((editor (uiop:getenv "EDITOR")))
    (setf (uiop:getenv "EDITOR") (rebase-script-path))
    (unwind-protect
         ;; xxx: get the error output, if any, to get explanations of failure.
         (let ((process (uiop:launch-program (list
                                              "git"
                                              "rebase"
                                              "--autostash"
                                              "-i"
                                              ;; Give the right commit to rebase from.
                                              ;; When rebasing from the root commit,
                                              ;; something special?
                                              (if (root-commit-p from)
                                                  "--root"
                                                  (format nil "~a^" from)))
                                             :output :stream
                                             :error-output :stream
                                             :ignore-error-status t)))
           (if (uiop:process-alive-p process)
               (let* ((output (read-line (uiop:process-info-output process)))
                      (pidtxt (str:trim (second (str:split ":" output)))))
                 (setf (rebase-pid vcs) pidtxt)
                 (format t "The git interactive rebase is started on pid ~a. Edit the rebase file and validate." pidtxt)
                 (values (format nil "rebase started")
                         nil
                         0))
               (porcelain-error "git rebase process didn't start properly. Aborting.")))
      (setf (uiop:getenv "EDITOR") editor))))

(defun %rebase-signal (vcs &key (sig "-SIGTERM"))
  (declare (type vcs-git vcs))
  "Send a kill signal to our rebase script: with -SIGTERM, git picks up our changes and continues the rebase process. This is called by a rebase continue command.
  With -SIGKILL the process is stopped. This is called by rebase abort."
  (multiple-value-bind (output error-output exit-code)
      (uiop:run-program (list "kill" sig (rebase-pid vcs))
                        :output :string
                        :error-output :string
                        :ignore-error-status t)
    (declare (ignorable output))
    (setf (rebase-pid vcs) nil)
    (values (format nil "rebase finished.")
            error-output
            exit-code)))

(defmethod rebase-continue ((vcs vcs-git))
  (cond
    ;; First, if we are running our rebase script, send a "continue" signal
    ;; so that git continues the rebase.
    ;; This is used by C-c C-c in the interactive rebase buffer.
    ((rebase-pid vcs)
     (%rebase-signal vcs))
    ;; Check if a rebase was started by someone else and continue it.
    ;; This is called from legit interace: "r" "c".
    ((uiop:directory-exists-p ".git/rebase-merge/")
     (run-git (list "rebase" "--continue")))
    (t
     (porcelain-error  "No git rebase in process?"))))
(defmethod rebase-abort ((vcs vcs-git))
  (cond
    ;; First, if we are running our rebase script, kill it.
    ;; This makes git abort the rebase too.
    ;; This is used by C-c C-k in the interactive rebase buffer.
    ((rebase-pid vcs)
     (%rebase-signal vcs :sig "-SIGKILL"))
    ;; Check if a rebase was started by someone else and abort it.
    ;; This is called from legit interface: "r" "a".
    ((uiop:directory-exists-p ".git/rebase-merge/")
     (run-git (list "rebase" "--abort")))
    (t
     (porcelain-error  "No git rebase in process? PID not found."))))

(defmethod rebase-skip ((vcs vcs-git))
  (cond
    ((rebase-pid vcs)
     (porcelain-error "The rebase process you started from Lem is still running. Please continue or abort it (or kill job ~a)" (rebase-pid vcs)))
    ;; Check if a rebase was started by someone else and abort it.
    ;; This is called from legit interface: "r" "s".
    ((uiop:directory-exists-p ".git/rebase-merge/")
     (run-git (list "rebase" "--skip")))
    (t
     (porcelain-error  "No git rebase in process? PID not found."))))

(defmethod stash-push ((vcs vcs-git) &key message)
  "Stash the current changes. Ask for a stash message."
  (if message
      (run-git (list "stash" "push" "-m" message))
      (run-git (list "stash" "push"))))

(defmethod stash-pop ((vcs vcs-git) &key (position 0))
  "Pop the latest stashed changes."
  (when (not (and (numberp position)
                  (not (minusp position))))
    (porcelain-error "Bad stash index: ~a. We expect a non-negative number." position))
  (run-git (list "stash"
                 "pop"
                 (format nil "stash@{~a}" position) ;; position alone works too.
                 )))

(defmethod stash-drop ((vcs vcs-git) &key position)
  "drop this stash."
  (when (not (and (numberp position)
                  (not (minusp position))))
    (porcelain-error "Bad stash index: ~a. We expect a non-negative number." position))
  (run-git (list "stash"
                 "drop"
                 (format nil "stash@{~a}" position))))

(defmethod stash-list ((vcs vcs-git))
  ;; each line is like
  ;; stash@{7}: On main: notes: legit vim interference
  ;; just display them.
  (str:lines
   (run-git (list "stash" "list"))))

(defmethod stash-show ((vcs vcs-git) &key position)
  (if (and (numberp position)
           (not (minusp position)))
      (run-git (list "stash"
                     "show"
                     "-p" ;; view as patch = view diff
                     (princ-to-string position)))
      (format nil "Invalid stash reference: ~s. We expect a positive number." position)))
