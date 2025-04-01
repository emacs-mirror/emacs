(defpackage :lem/tests/file
  (:use :cl :rove))
(in-package :lem/tests/file)

(deftest get-file-mode
  (ok (equal 'lem-posix-shell-mode:posix-shell-mode
             (lem:get-file-mode ".bashrc")))
  (ok (equal 'lem-posix-shell-mode:posix-shell-mode
             (lem:get-file-mode "foo.bashrc")))
  (ok (equal 'lem-lisp-mode:lisp-mode
             (lem:get-file-mode "foo.lisp")))
  (ok (null (lem:get-file-mode "foo.lisp.bak"))))
