(uiop:define-package :lem/porcelain/hg
    (:use :cl
     :lem/porcelain)
  (:shadow :push)
  (:import-from :trivial-types
                :proper-list)
  (:export :hg-project-p)
  (:documentation "Implements the porcelain interface for hg-based repos."))

(in-package :lem/porcelain/hg)

(declaim (type (proper-list string) *hg-base-arglist*))
(defvar *hg-base-arglist* (list "hg")
  "The mercurial program (hg), to be appended command-line options.")

(defvar *projects-mapping* (make-hash-table :test #'equal)
  "Map a VCS project root to its VCS object.

  We need a single VCS object to retain data.

  Note: no special data is saved for Mercurial projects yet.")

(defun get-or-create-project ()
  "If a VCS object exists for the current project root (as of
  the CWD, set by `with-current-project', return it. Otherwise, create a
  new `vcs-hg' object instance."
  (let* ((root (uiop:getcwd)) ;; CWD set by with-current-project.
         (existing (gethash root *projects-mapping*)))
    (or existing
        (setf (gethash root *projects-mapping*)
              (make-instance 'vcs-hg)))))

;; VCS implementation for hg
(defclass vcs-hg (vcs-project)
  ()
  (:default-initargs
   :name "mercurial"))

(defun hg-project-p ()
  "Return t if we find a .hg/ directory in the current directory (which should be the project root. Use `lem/porcelain:with-current-project`)."
  (when (uiop:directory-exists-p ".hg")
    (get-or-create-project)))

(defun run-hg (arglist)
  "Run the mercurial command with these options (list).
  Return standard and error output as strings.

  For error handling strategy, see `run-git`."
  (uiop:run-program (concatenate 'list
                                 *hg-base-arglist*
                                 (uiop:ensure-list arglist)
                                 '("--pager" "never")   ;; no interactive pager.
                                 )
                    :output :string
                    :error-output :string
                    :ignore-error-status t))

(defun porcelain ()
  "Return changes."
  (run-hg "status"))

(defmethod components ((vcs vcs-hg))
  "Return 3 values:
  - untracked files
  - modified and unstaged files
  - modified and staged files."
  ;; Mercurial manual:
  ;; The codes used to show the status of files are:
  ;;    M = modified
  ;;    A = added
  ;;    R = removed => difference with Git
  ;;    C = clean   => difference
  ;;    ! = missing (deleted by non-hg command, but still tracked)
  ;;    ? = not tracked
  ;;    I = ignored (=> not shown without -A)
  ;;      = origin of the previous file (with --copies)
  (loop for line in (str:lines (porcelain))
        for file = (subseq line 2)  ;; a slight difference than with git.
        unless (str:blankp line)
        ;; Modified
        if (equal (elt line 0) #\M)
        collect (list :file file :type :modified) into modified-staged-files
        ;; Added
        if (equal (elt line 0) #\A)
        collect (list :file file :type :added) into modified-staged-files
        ;; Removed
        if (equal (elt line 0) #\R)
        collect (list :file file :type :deleted) into modified-staged-files
        ;; Modified (unstaged)
        if (or (equal (elt line 1) #\M) (equal (elt line 1) #\X))
        collect (list :file file :type :modified) into modified-unstaged-files
        ;; Untracked
        if (str:starts-with-p "?" line)
        collect file into untracked-files
        ;; Missing (deleted)
        if (str:starts-with-p "!" line)
        collect (list :file file :type :deleted) into modified-unstaged-files
        finally (return (values untracked-files
                                modified-unstaged-files
                                modified-staged-files))))

(defmethod file-diff ((vcs vcs-hg) file &key cached)
  "Show the diff of staged files (and only them)."
  (when cached
    (run-hg (list "diff"
                  (format nil "-U~D" lem/porcelain:*diff-context-lines*)
                  file))
    ;; files not staged can't be diffed.
    ;; We could read and display their full content anyways?
    ))

(defmethod show-commit-diff ((vcs vcs-hg) ref &key ignore-all-space)
  (declare (ignore ignore-all-space))
  (run-hg (list "log" "-r" ref "-p")))


(defmethod commit ((vcs vcs-hg) message)
  (run-hg (list "commit" "-m" message)))

(defmethod current-branch ((vcs vcs-hg))
  "Return the current branch name."
  (str:trim
   (run-hg "branch")))

(defmethod branches ((vcs vcs-hg) &key sort-by)
  (declare (ignore sort-by))
  (porcelain-error "not implemented"))

(defmethod latest-commits ((vcs vcs-hg) &key (n lem/porcelain:*nb-latest-commits*) (hash-length 8) (offset 0))
  (declare (ignorable n hash-length offset))
  (let ((out (run-hg "log")))
    ;; Split by paragraph.
    #| $ hg log
         changeset:   1:c20c766359d3
         user:        user@machine
         date:        Mon Oct 02 23:01:32 2023 +0200
         summary:     second line

         changeset:   0:b27dda897ba8
         user:        user@machine
         date:        Mon Oct 02 22:51:57 2023 +0200
         summary:     test

         |#
    (loop for paragraph in (ppcre:split "\\n\\n" out)
          collect
             (loop for line in (str:lines (str:trim paragraph))
                   with entry = (list :line ""
                                      :message ""
                                      :hash "")
                   for (key %val) = (str:split ":" line :limit 2)
                   for val = (str:trim %val)
                   for changeset = ""
                   for tag = ""
                   for user = ""
                   for date = ""
                   for summary = ""
                   do
                      (cond
                        ;; we can use str:string-case with a recent enough quicklisp dist > July 2023 (PR 103)
                        ((equal key "changeset")
                         (setf changeset val)
                         (setf (getf entry :changeset) val)
                         (setf (getf entry :hash) val))
                        ((equal key "tag")
                         (setf tag val)
                         (setf (getf entry :tag) val))
                        ((equal key "user")
                         (setf user val)
                         (setf (getf entry :user) val))
                        ((equal key "date")
                         (setf date val)
                         (setf (getf entry :date) val))
                        ((equal key "summary")
                         (setf summary (str:trim val))
                         (setf (getf entry :summary) val)
                         (setf (getf entry :message)
                               (str:concat " " val))
                         (setf (getf entry :line)
                               (str:concat changeset " " summary))))
                   finally (return entry)))))

(defmethod commits-log ((vcs vcs-hg) &key (offset 0) limit (hash-length 8))
  (declare (ignorable hash-length))
  (let* ((commits (latest-commits vcs))
         (total-commits (length commits))
         (end (when limit
                (min total-commits (+ offset limit)))))
    (if (>= offset total-commits)
        nil
        (subseq commits offset end))))

(defmethod commit-count ((vcs vcs-hg))
  (parse-integer
   (str:trim (run-hg '("id" "--num" "--rev" "tip")))))

(defmethod stage ((vcs vcs-hg) file)
  (run-hg (list "add" file)))

(defmethod apply-patch ((vcs vcs-hg) diff &key reverse)
  (declare (ignorable diff reverse))
  (porcelain-error "applying patch not yet implemented for Mercurial"))

(defmethod rebase-interactively ((vcs vcs-hg) &key from)
  (declare (ignore from))
  (porcelain-error "Interactive rebase not implemented for Mercurial"))
