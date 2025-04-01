
(defpackage :lem/legit
  (:use :cl
        :lem)
  (:export :legit-status
           :*prompt-for-commit-abort-p*
           :*ignore-all-space*
           :*show-stashes*
           :*vcs-existence-order*
	   :*peek-legit-keymap*
           :peek-legit-discard-file
           :peek-legit-previous
           :peek-legit-next)
  (:documentation "Display version control data of the current project in an interactive two-panes window.

  This package in particular defines the right window of the legit interface and the user-level commands.

  Gets VCS data by calling lem/porcelain and asking lem/peek-legit to display data on the left window."))

(in-package :lem/legit)

(defvar *default-vcs-existence-order*
  (list :git :fossil :hg))

(defparameter *vcs-existence-order* *default-vcs-existence-order*
  "The order in which to detect the VCS of the current project.

  List of keywords. Choices are: :git, :fossil and :hg.")

(defvar *vcs-keyword-function-mapping*
  (list :git #'lem/porcelain/git:git-project-p
        :fossil #'lem/porcelain/fossil:fossil-project-p
        :hg #'lem/porcelain/hg:hg-project-p
        :mercurial #'lem/porcelain/hg:hg-project-p)
  "A keyword to function mapping to help users configure their *vcs-existence-order*.")

(defun vcs-project-p (&optional (vcs-order *vcs-existence-order*))
  "When this project is under a known version control system, returns a VCS object for the project.
   Otherwise, returns nil."
  ;; This doesn't return the 2 values :(
  ;; (or (fossil-project-p)
  ;;     (git-project-p))
  (loop for choice in vcs-order
        for fn = (if (keywordp choice)
                     (getf *vcs-keyword-function-mapping* choice)
                     choice)
        for vcs = (funcall fn)
        if vcs
          return vcs))

(defun call-with-porcelain-error (function)
  (handler-bind ((lem/porcelain:porcelain-error
                   (lambda (c)
                     (lem:editor-error (slot-value c 'lem/porcelain:message)))))
    (funcall function)))

(defmacro with-porcelain-error (&body body)
  "Handle porcelain errors and turn them into a lem:editor-error."
  ;; Doing this helps avoiding tight coupling between the porcelain package and Lem.
  `(call-with-porcelain-error (lambda () ,@body)))

(defun call-with-current-project (function)
  (with-porcelain-error ()
    (let ((root (lem-core/commands/project:find-root (lem:buffer-directory))))
      (uiop:with-current-directory (root)
        (let ((vcs (vcs-project-p *vcs-existence-order*)))
          (if vcs
              (funcall function vcs)
              (lem:message "Not inside a version-controlled project?")))))))

(defmacro with-current-project ((vcs-bind) &body body)
  "Execute body with the current working directory changed to the project's root,
  find and `vcs-bind` as the VCS

  If no Git directory (or other supported VCS system) are found, message the user."
  `(call-with-current-project (lambda (,vcs-bind) ,@body)))
