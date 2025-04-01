(defpackage :lem-tests/lisp-mode/package-inferred-system
  (:use :cl :rove)
  (:import-from :lem-lisp-mode))
(in-package :lem-tests/lisp-mode/package-inferred-system)

(deftest infer-package-name-1
  (ok (equal "project-root/foo/bar"
             (lem-lisp-mode/package-inferred-system::infer-package-name-1
              (lem-lisp-mode/package-inferred-system::make-project-root
               :name "project-root"
               :asd-file #P"/common-lisp/project-root/project-root.asd")
              #P"/common-lisp/project-root/foo/bar.lisp")))
  (ok (equal "project-root-tests/a"
             (lem-lisp-mode/package-inferred-system::infer-package-name-1
              (lem-lisp-mode/package-inferred-system::make-project-root
               :name "project-root-tests"
               :asd-file #P"/common-lisp/project-root/project-root-tests.asd"
               :pathname "tests")
              #P"/common-lisp/project-root/tests/a.lisp")))
  (ok (equal "project-root/test/a/b"
             (lem-lisp-mode/package-inferred-system::infer-package-name-1
              (lem-lisp-mode/package-inferred-system::make-project-root
               :name "project-root/test"
               :asd-file #P"/common-lisp/project-root/project-root.asd"
               :pathname "test")
              #P"/common-lisp/project-root/test/a/b.lisp")))
  (ok (equal "project-root/test/a/b"
             (lem-lisp-mode/package-inferred-system::infer-package-name-1
              (lem-lisp-mode/package-inferred-system::make-project-root
               :name "project-root"
               :asd-file #P"/common-lisp/project-root/project-root.asd")
              #P"/common-lisp/project-root/test/a/b.lisp")))
  (ok (equal "project-root/main"
             (lem-lisp-mode/package-inferred-system::infer-package-name-1
              (lem-lisp-mode/package-inferred-system::make-project-root
               :name "project-root"
               :asd-file #P"/common-lisp/project-root/project-root.asd")
              #p"/common-lisp/project-root/main.lisp")))
  (ok (equal "lem-project-root/foo/bar"
             (lem-lisp-mode/package-inferred-system::infer-package-name-1
              (lem-lisp-mode/package-inferred-system::make-project-root
               :name "lem-project-root"
               :asd-file #P"/common-lisp/project-root/project-root.asd")
              #P"/common-lisp/project-root/foo/bar.lisp"))))
