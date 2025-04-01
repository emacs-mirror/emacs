(defpackage :lem-documentation-mode/utils
  (:use :cl)
  (:export :collect-global-command-packages
           :collect-commands-in-package))
(in-package :lem-documentation-mode/utils)

(defun extract-defpackage-name (form)
  (assert (and (consp form)
               (member (first form) '(defpackage uiop:define-package))))
  (second form))

(defun collect-global-command-packages ()
  (loop :for component :in (asdf:component-children (asdf:find-component :lem "commands"))
        :collect (find-package
                  (extract-defpackage-name
                   (uiop:read-file-form
                    (asdf:component-pathname component))))))

#+sbcl
(defun sort-by-file-location (commands)
  (sort commands
        #'<
        :key (lambda (command)
               (sb-c:definition-source-location-toplevel-form-number
		(lem-core::command-source-location command)))))

(defun collect-commands-in-package (package)
  (let ((commands '()))
    (do-external-symbols (sym package)
      (let ((command (lem:get-command sym)))
        (when command
          (push command commands))))
    #+sbcl
    (sort-by-file-location commands)
    #-sbcl
    commands))
