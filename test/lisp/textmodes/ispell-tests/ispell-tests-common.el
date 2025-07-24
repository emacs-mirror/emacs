;;; common.el ---  -*- lexical-binding: t; -*-

(defvar tests-ispell-data-directory
  (expand-file-name "test/lisp/textmodes/ispell-resources/" source-directory))

(let* ((backend-binaries (list "ispell" "aspell" "hunspell" "enchant-2"))
       (filter-binaries (lambda ()
                          (seq-filter
                           #'executable-find
                           backend-binaries))))

  (defun ispell-tests--some-backend-available-p ()
    (not
     (null (funcall filter-binaries))))

  (defun ispell-tests--some-backend ()
    (car (funcall filter-binaries))))

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

(provide 'ispell-tests-common)
