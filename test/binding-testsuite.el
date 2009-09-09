(defvar binding-test-buffer-A (get-buffer-create "A"))
(defvar binding-test-buffer-B (get-buffer-create "B"))

(defvar binding-test-always-local 'always)
(make-variable-buffer-local 'binding-test-always-local)

(defvar binding-test-some-local 'some)
(with-current-buffer binding-test-buffer-A
  (set (make-local-variable 'binding-test-some-local) 'local))

(defvar fails 0)
(defvar total 0)

(defun check (v name)
  (if v
      (message "PASS: %s" name)
    (message "FAIL: %s" name)
    (setq fails (1+ fails)))
  (setq total (1+ total)))

(defun binding-test-manual ()
  "A test case from the elisp manual."
  (set-buffer binding-test-buffer-A)
  (let ((binding-test-some-local 'something-else))
    (check (eq binding-test-some-local 'something-else)
	   "let binding failed")
    (set-buffer binding-test-buffer-B)
    (check (eq binding-test-some-local 'some)
	   "set-buffer failed to rebind"))
  (check (eq binding-test-some-local 'some)
	 "let erroneously rebound")
  (set-buffer binding-test-buffer-A)
  (check (eq binding-test-some-local 'local)
	 "let failed to unbind"))

(defun binding-test-setq-default ()
  "Test that a setq-default has no effect when there is a local binding."
  (set-buffer binding-test-buffer-B)
  ;; This variable is not local in this buffer.
  (let ((binding-test-some-local 'something-else))
    (setq-default binding-test-some-local 'new-default))
  (check (eq binding-test-some-local 'some)
	 "set-buffer failed to rebind"))

(defun binding-test-makunbound ()
  "Tests of makunbound, from the manual."
  (set-buffer binding-test-buffer-B)
  (check (boundp 'binding-test-some-local)
	 "verify variable is bound")
  (let ((binding-test-some-local 'outer))
    (let ((binding-test-some-local 'inner))
      (makunbound 'binding-test-some-local)
      (check (not (boundp 'binding-test-some-local))
	     "verify variable is unbound"))
    (check (and (boundp 'binding-test-some-local)
		(eq binding-test-some-local 'outer))
	   "verify variable is bound again")))

(defun binding-test-defvar-bool ()
  "Test DEFVAR_BOOL"
  (let ((display-hourglass 5))
    (check (eq display-hourglass t)
	   "DEFVAR_BOOL value rewriting")))

(defun binding-test-defvar-int ()
  "Test DEFVAR_INT"
  (check (condition-case nil
	     (progn (setq window-min-height 5.0)
		    nil)
	   (wrong-type-argument t))
	 "DEFVAR_INT with wrong type"))

(defun binding-test-set-constant ()
  "Test setting a constant"
  (check (condition-case nil
	     (progn (setq t 'bob)
		    nil)
	   (setting-constant t))
	 "Setting t")
  (check (condition-case nil
	     (progn (setq nil 'bob)
		    nil)
	   (setting-constant t))
	 "Setting nil")
  (check (condition-case nil
	     (progn (setq :keyword 'bob)
		    nil)
	   (setting-constant t))
	 "Setting keyword")
  (check (condition-case nil
	     (progn (setq :keyword :keyword)
		    t)
	   (setting-constant nil))
	 "Setting keyword to itself"))

;; kill-local-variable
;; defconst; can modify
;; defvar and defconst modify the local binding [ doesn't matter for us ]
;; various kinds of special internal forwarding objects
;;   a couple examples in manual, not enough
;; frame-local vars
;; variable aliases

(defun binding-test-all ()
  (save-excursion (binding-test-manual))
  (save-excursion (binding-test-setq-default))
  (save-excursion (binding-test-makunbound))
  (binding-test-defvar-bool)
  (binding-test-defvar-int)
  (binding-test-set-constant)

  (if (eq fails 0)
      (message "All %d binding tests passed" total)
    (message "%d/%d binding tests failed" fails total)))
