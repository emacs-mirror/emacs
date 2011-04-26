(defun ede-locate-test-locate (file)
  "Test EDE Locate on FILE using LOCATE type.
The search is done with the current EDE root."
  (interactive "sFile: ")
  (let ((loc (ede-locate-locate
	      "test"
	      :root (ede-project-root-directory
		     (ede-toplevel)))))
    (data-debug-new-buffer "*EDE Locate ADEBUG*")
    (ede-locate-file-in-project loc file)
    (data-debug-insert-object-slots loc "]"))
  )

(defun ede-locate-test-global (file)
  "Test EDE Locate on FILE using GNU Global type.
The search is done with the current EDE root."
  (interactive "sFile: ")
  (let ((loc (ede-locate-global
	      "test"
	      :root (ede-project-root-directory
		     (ede-toplevel)))))
    (data-debug-new-buffer "*EDE Locate ADEBUG*")
    (ede-locate-file-in-project loc file)
    (data-debug-insert-object-slots loc "]"))
  )

(defun ede-locate-test-idutils (file)
  "Test EDE Locate on FILE using ID Utils type.
The search is done with the current EDE root."
  (interactive "sFile: ")
  (let ((loc (ede-locate-idutils
	      "test"
	      :root (ede-project-root-directory
		     (ede-toplevel)))))
    (data-debug-new-buffer "*EDE Locate ADEBUG*")
    (ede-locate-file-in-project loc file)
    (data-debug-insert-object-slots loc "]"))
  )

(defun ede-locate-test-cscope (file)
  "Test EDE Locate on FILE using CScope type.
The search is done with the current EDE root."
  (interactive "sFile: ")
  (let ((loc (ede-locate-cscope
	      "test"
	      :root (ede-project-root-directory
		     (ede-toplevel)))))
    (data-debug-new-buffer "*EDE Locate ADEBUG*")
    (ede-locate-file-in-project loc file)
    (data-debug-insert-object-slots loc "]"))
  )
