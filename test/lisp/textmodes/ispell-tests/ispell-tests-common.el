;;; common.el ---  -*- lexical-binding: t; -*-

;; (defvar tests-ispell-data-directory
;;   (expand-file-name "test/lisp/textmodes/ispell-resources/" source-directory))
(require 'ert)
(require 'ert-x)

(defvar tests-ispell-data-directory
  (let ((ert-resource-directory-trim-right-regexp "-tests/.*-tests-common\\.el"))
    (ert-resource-directory)))

;;(message "lwf:tests-ispell-data-directory=%s" tests-ispell-data-directory)

(defvar fake-aspell-path
  (expand-file-name "fake-aspell-new.bash" tests-ispell-data-directory))


(let* ((backend-binaries (list "ispell" "aspell"  "hunspell"  "enchant-2" fake-aspell-path)
                         )
       (filter-binaries (seq-filter
                           (lambda (b)
                             (and
                              (executable-find b)
                              ;; (equal 0
                              ;;        (with-temp-buffer
                              ;;          (call-process b nil t "-v")))
                              ))
                           backend-binaries)))

  (defun ispell-tests--some-backend-available-p ()
    (not
     (null filter-binaries)))

  (defun ispell-tests--some-backend ()
    (let ((retval (car filter-binaries)))
      (message "available backend is:%s" retval)
      retval)))

(eval-when-compile
  (require 'cl-macs))
(cl-defmacro letopt (bindings &body body)
  (declare (indent 1))
  (let* ((binding-var (lambda (binding) (car binding)))
	 (binding-val (lambda (binding) (cadr binding)))
	 (make-setopt (lambda (a b)
			(list 'setopt a b)))
	 (vars (seq-map binding-var bindings))
	 (values (seq-map binding-val bindings))
	 (temp-vars (seq-map #'gensym vars))
	 (savebindings (seq-mapn #'list temp-vars vars))
	 (tempbindings (seq-mapn make-setopt vars values))
	 (restorebindings (seq-mapn make-setopt vars temp-vars)))
    `(let ,savebindings
       (unwind-protect (progn ,@tempbindings
			      ,@body)
	 ,@(reverse restorebindings)))))

(cl-defmacro with-ispell-global-dictionary (bindings &body body)
  "This macro should not really be needed, but `ispell.el'.
Sets up dictionaries in a stupid way."
  (declare (indent 1))
  (let* ((dictionary-val (car bindings))
	 (temp-var (gensym 'old-dictionary)))
    `(let ((,temp-var (symbol-value 'ispell-dictionary)))
       (unwind-protect (progn (ispell-change-dictionary ,dictionary-val t)
			      ,@body)
	 (progn
           (unwind-protect
               (ispell-change-dictionary ,temp-var t)
             (message "restoring original dictionary failed")))))))

(provide 'ispell-tests-common)
