(uiop:define-package :lem/directory-mode/file
  (:use :cl
        :lem)
  (:export
   :delete-file*
   :rename-file*
   :copy-files
   :rename-files
   :symbolic-link-p
   :pathname-directory-last-name))
(in-package :lem/directory-mode/file)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-ext:lock-package :lem/directory-mode/file))

(defvar *rename-p* nil)

(defun delete-file* (file)
  #+windows
  (if (uiop:directory-pathname-p file)
      (sb-ext:delete-directory file :recursive t)
      (delete-file file))
  #-windows
  (if (and (not (string= (namestring file)
                         (namestring (uiop:resolve-symlinks file)))))
      (and (prompt-for-y-or-n-p
            (format nil "It appears that ~a is a symlink, delete it?" file))
           (run-command `("unlink" ,(string-right-trim
                                     (string
                                      (uiop:directory-separator-for-host))
                                     (namestring file)))))
      (run-command `("rm" "-fr" ,file))))

(defun subdirectory-p (to-pathname from-pathname)
  (let ((to-dir (pathname-directory to-pathname))
        (from-dir (pathname-directory from-pathname)))
    (assert (eq :absolute (car to-dir)))
    (assert (eq :absolute (car from-dir)))
    (and (<= (length from-dir)
             (length to-dir))
         (loop
           :for from-elt :in (cdr from-dir)
           :for to-elt :in (cdr to-dir)
           :when (not (equal from-elt to-elt))
           :do (return nil)
           :finally (return t)))))

(defun copy-directory (src dst)
  (setf dst (uiop:ensure-directory-pathname dst))
  (let ((dst (if (probe-file dst)
                 (merge-pathnames (pathname-directory-last-name src)
                                  dst)
                 dst)))
    (when (subdirectory-p dst src)
      (editor-error "Cannot copy `~A' into its subdirectory `~A'" src dst))
    ;(format t "mkdir ~A~%" dst)
    (let ((dst (ensure-directories-exist dst)))
      (dolist (file (list-directory src))
        (copy-file file dst)))
    (when *rename-p* (uiop:delete-empty-directory src))))

(defun copy-file (src dst)
  (if (uiop:directory-pathname-p src)
      (copy-directory src dst)
      (let ((probe-dst (probe-file dst)))
        (cond ((and probe-dst (uiop:directory-pathname-p probe-dst))
               (copy-file src (merge-pathnames (file-namestring src) probe-dst)))
              (t
               ;(format t "copy ~A -> ~A~%" src dst)
               (if *rename-p*
                   (rename-file src dst)
                   (uiop:copy-file src dst)))))))

(defun copy-file* (src dst)
  #+windows
  (copy-file src dst)
  #-windows
  (run-command `("cp" "-R" ,src ,dst)))

(defun rename-file* (src dst)
  #+windows
  (let ((*rename-p* t))
    (copy-file src dst))
  #-windows
  (run-command `("mv" ,src ,dst)))

(defun copy-or-rename-file (src dst)
  #+windows
  (copy-file src dst)
  #-windows
  (if *rename-p*
      (run-command `("mv" ,src ,dst))
      (run-command `("cp" "-R" ,src ,dst))))

(defun check-copy-files (src-files dst)
  (let ((n (length src-files)))
    (cond ((or (and (< 1 n) (uiop:file-exists-p dst))
               (and (= 1 n) (uiop:directory-pathname-p (first src-files))
                    (uiop:file-exists-p dst)))
           (editor-error "Target must be a directory"))
          ((and (< 1 n) (not (uiop:directory-exists-p dst)))
           (editor-error "No such file or directory")))))

(defun copy-files (src-files dst-file)
  (check-copy-files src-files dst-file)
  (dolist (file src-files)
    (copy-or-rename-file file dst-file)))

(defun rename-files (src-files dst-file)
  (let ((*rename-p* t))
    (dolist (file src-files)
      (copy-or-rename-file file dst-file))))

(defun symbolic-link-p (pathname)
  (not (uiop:pathname-equal pathname (probe-file pathname))))

(defun pathname-directory-last-name (pathname)
  (enough-namestring pathname (uiop:pathname-parent-directory-pathname pathname)))

;;; internal functions
(defun run-command (command)
  (when (consp command)
    (setf command (mapcar #'princ-to-string command)))
  (let ((error-string
          (with-output-to-string (error-output)
            (uiop:run-program command
                              :ignore-error-status t
                              :error-output error-output))))
    (when (string/= error-string "")
      (lem:editor-error "~A" error-string))))
