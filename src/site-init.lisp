(in-package :lem-core)

(defvar *site-init-name* "lem-site-init")
(defvar *site-init-comment ";; don't edit !!!")
(defvar *inits-directory-name* "lisp")

(defun site-init-path ()
  (let ((path (merge-pathnames (format nil "~A.asd"
                                       *site-init-name*)
                               (lem-home))))
    (with-open-file (out (ensure-directories-exist path)
                         :direction :output
                         :if-exists nil)
      (format out "~A~%~(~S~)~%"
              *site-init-comment
              `(asdf:defsystem ,*site-init-name*)))
    path))

(defun raw-init-files ()
  (directory (merge-pathnames "inits/*.lisp" (lem-home))))

(defun site-init-list-inits ()
  (loop for i in (sort (mapcar #'pathname-name (raw-init-files)) #'string<)
        collect (list :file (format nil "inits/~A" i))))

(defun site-init ()
  (with-open-file (i (site-init-path))
    (let ((*package* (find-package :lem-user)))
      (read i))))

(defun (setf site-init) (exp)
  (with-open-file (o (site-init-path) :direction :output :if-exists :supersede)
    (let ((*package* (find-package :lem-user)))
      (format o "~A~%~(~S~)" *site-init-comment exp))))

(defun update-site-init-inits ()
  (let* ((site-init (site-init))
         (file (getf (cddr site-init) :components))
         (dir (site-init-list-inits)))
    (unless (and (= (length file)
                    (length dir))
                 (loop :for i :in file
                       :for j :in dir
                       :always (equal (second i) (second j))))
      (setf (getf (cddr site-init) :components) dir
            (site-init) site-init))))

(defun load-site-init (&key force)
  (let* ((asdf:*central-registry*
           (union (remove-duplicates
                   (mapcar #'pathname
                           (mapcar #'directory-namestring
                                   (directory
                                    (merge-pathnames
                                     "**/*.asd"
                                     (pathname (str:concat
                                                (directory-namestring (lem-home))
                                                *inits-directory-name*
                                                (string  (uiop:directory-separator-for-host)))))))))
                  asdf:*central-registry*
                  :test #'equal))
         (system-name *site-init-name*)
         (key (intern (string-upcase system-name) :keyword)))
    (unless (and (find key *features*)
                 (not force))
      (pushnew key *features*)
      (update-site-init-inits)
      (asdf:load-asd (site-init-path))
      (let ((*package* (find-package :lem-user)))
        (maybe-load-systems system-name :silent t)))))

(define-command site-init-add-dependency (symbols)
    ((prompt-for-library "library: " :history-symbol 'load-library))
  "Input system name and test it's loadable."
  (loop :with site-init := (site-init)
        :with depends-on := (getf (cddr site-init) :depends-on)
        :for s :in (uiop:split-string symbols)
        :for key := (read-from-string (format nil ":lem-~A" s))
        :do (unless (find key depends-on)
              (maybe-load-systems key :silent t)
              (push key depends-on))
        :finally (setf (getf (cddr site-init) :depends-on) depends-on)
                 (setf (site-init) site-init)
                 (message "~A" depends-on)))

(define-command site-init-remove-dependency (symbols) ((:string "Package:"))
  "Remove system name from site-init depends-on"
  ;;TBD prepare prompt-site-init-depends-on like function
  (loop :with site-init := (site-init)
        :with depends-on := (getf (cddr site-init) :depends-on)
        :for s :in (uiop:split-string symbols)
        :for key := (read-from-string (format nil ":lem-~A" s))
        :do (when (find key depends-on)
              (setf depends-on (remove key depends-on)))
        :finally (setf (getf (cddr site-init) :depends-on) depends-on)
                 (setf (site-init) site-init)
                 (message "~A" depends-on)))

;; TBD prepare some commands to edit asd file.
;; M-x site-init-edit-dependency / prepare buffer one system in a line which are editable. test loadable when save and update asd.
;; M-x site-init-add / prepare a file which load.
;; M-x site-init-edit / open dired like interface "~/.lem/inits/"
