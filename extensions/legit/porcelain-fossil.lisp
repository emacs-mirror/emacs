(uiop:define-package :lem/porcelain/fossil
    (:use :cl
    ;; we import all the classes and methods defined in the main porcelain:
     :lem/porcelain)
  (:shadow :push)
  (:import-from :trivial-types
                :proper-list)
  (:export :fossil-project-p)
  (:documentation "Implements the porcelain interface for fossil-based repos."))

(in-package :lem/porcelain/fossil)

(declaim (type (proper-list string) *fossil-base-args*))
(defvar *fossil-base-args* (list "fossil")
  "The fossil program, to be appended command-line options.")

;; VCS implementation for fossil
(defclass vcs-fossil (vcs-project)
  ()
  (:default-initargs
   :name "fossil"))

(defvar *projects-mapping* (make-hash-table :test #'equal)
  "Map a VCS project root to its VCS object.

  We need a single VCS object to retain data.

  Note: no special data is saved for Fossil projects yet.")

(defun get-or-create-project ()
  "If a vcs-fossil object exists for the current project root (as of
  the CWD, set by `with-current-project', return it. Otherwise, create a
  new `vcs-fossil' object instance."
  (let* ((root (uiop:getcwd)) ;; CWD set by with-current-project.
         (existing (gethash root *projects-mapping*)))
    (or existing
        (setf (gethash root *projects-mapping*)
              (make-instance 'vcs-fossil)))))

(defun fossil-project-p ()
  "Return t if we either find a .fslckout file in the current directory (should be the project root) or a file with a .fossil extension."
  (cond
    ((uiop:file-exists-p ".fslckout")
     (make-instance 'vcs-fossil))
    (t
     ;; Maybe do we need "fossil open" here.
     (loop for file in (uiop:directory-files "./")
           if (equal "fossil" (pathname-type file))
           return (get-or-create-project)))))

(defun run-fossil (arglist)
  "Run the fossil command with these options.

   arglist: a list of CLI options and arguments to append to the base fossil program.
   See `*fossil-base-args*`."
  (uiop:run-program (concatenate 'list
                                 *fossil-base-args*
                                 (uiop:ensure-list arglist))
                    :output :string
                    :error-output :string
                    :ignore-error-status t))

(defun porcelain ()
  "Get changes."
  (multiple-value-bind (out error code)
      (run-fossil "changes")
    (cond
      ((not (zerop code))
       (porcelain-error (str:join #\newline (list out error))))
      (t
       (values out error)))))

(defmethod components ((vcs vcs-fossil))
  "Return values:
  - untracked files
  - list of ADDED files
  - modified files"
  (loop for line in (str:lines (porcelain))
        for parts = (str:words line)
        for status = (first parts)
        for file = (second parts)
        if (equal "ADDED" status)
        collect (list :file file :type :added) into modified-staged-files
        if (equal "EDITED" status)
        collect (list :file file :type :modified) into modified-staged-files
        if (equal "DELETED" status)
        collect (list :file file :type :deleted) into modified-staged-files
        if (equal "UNKNOWN" status)
        collect file into untracked-files
        finally (return (values untracked-files
                                nil
                                modified-staged-files))))

(defmethod file-diff ((vcs vcs-fossil) file &key cached)
  (declare (ignorable cached))
  (run-fossil (list "diff" file)))


(defmethod show-commit-diff ((vcs vcs-fossil) ref &key ignore-all-space)
  (declare (ignore vcs)
           (ignore ref)
           (ignore ignore-all-space))
  nil)


(defmethod commit ((vcs vcs-fossil) message)
  (run-fossil (list "commit" "-m" message)))

(defmethod branches ((vcs vcs-fossil) &key sort-by)
  (declare (ignore sort-by))
  (porcelain-error "not implemented"))

(defmethod current-branch ((vcs vcs-fossil))
  ;; strip out "* " from "* trunk"
  (str:trim
   (subseq (run-fossil "branch") 2)))

(defun %not-fossil-commit-line (line)
  (str:starts-with-p "+++ no more data" line))

(defun fossil-commit-count ()
  ;; Not really tested in Lem.
  (length
   ;; Does the timeline command always end with "+++ no more data (1) +++" ?
   (remove-if #'%not-fossil-commit-line
              (str:lines
               (run-fossil (list "timeline" "--oneline"))))))

(defmethod commit-count ((vcs vcs-fossil))
  (fossil-commit-count))

(defmethod apply-patch ((vcs vcs-fossil) diff &key reverse)
  "Needs fossil at least > 2.10. Version 2.22 works."
  (declare (ignorable diff reverse))
  ;; mmh… it wants a binary patch.
  (values nil "fossil patch is not supported." 1))

(defmethod latest-commits ((vcs vcs-fossil) &key (n lem/porcelain:*nb-latest-commits*) (hash-length 8) (offset 0))
  (declare (ignorable n hash-length offset))
  ;; return bare result.
  (str:lines (run-fossil "timeline")))

(defmethod commits-log ((vcs vcs-fossil) &key (offset 0) limit (hash-length 8))
  (declare (ignorable hash-length))
  (let* ((commits (latest-commits vcs))
         (total-commits (length commits))
         (end (when limit (min total-commits (+ offset limit)))))
    (if (>= offset total-commits)
        nil
        (subseq commits offset end))))

(defmethod stage ((vcs vcs-fossil) file)
  (run-fossil (list "add" file)))

(defmethod rebase-interactively ((vcs vcs-fossil) &key from)
  (declare (ignore from))
  (porcelain-error "No interactive rebase for Fossil."))
