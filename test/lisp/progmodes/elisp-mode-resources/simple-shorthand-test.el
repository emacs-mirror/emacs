;;; simple-shorthand-test.el ---   -*- lexical-binding: t; -*-

(defun f-test ()
  (let ((read-symbol-shorthands '(("foo-" . "bar-"))))
    (with-temp-buffer
      (insert "(foo-bar)")
      (goto-char (point-min))
      (read (current-buffer)))))

(defun f-test2 ()
  (let ((read-symbol-shorthands '(("foo-" . "bar-"))))
    (read-from-string "(foo-bar)")))


(defun f-test3 ()
  (let ((read-symbol-shorthands '(("foo-" . "bar-"))))
    (intern "foo-bar")))

(defvar f-test-complete-me 42)

(elisp--foo-test3)

(defun #_f-test4--- () 84)

(defmacro f-define-test-5 ())

;; should be font locked with both shorthand
;; highlighting _and_ macro highlighting.
(f-define-test-5)

(when nil
  (f-test3)
  (f-test2)
  (f-test)
  (#_f-test4---))


;; Local Variables:
;; read-symbol-shorthands: (("f-" . "elisp--foo-"))
;; End:
