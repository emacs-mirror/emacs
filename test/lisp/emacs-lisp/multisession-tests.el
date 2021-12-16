;;; multisession-tests.el --- Tests for multisession.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'multisession)
(require 'ert)
(require 'ert-x)
(require 'cl-lib)

(ert-deftest multi-test-sqlite-simple ()
  (skip-unless (sqlite-available-p))
  (ert-with-temp-file dir
    :directory t
    (let ((user-init-file "/tmp/foo.el")
          (multisession-storage 'sqlite)
          (multisession-directory dir))
      (unwind-protect
          (progn
            (define-multisession-variable foo 0
              ""
              :synchronized t)
            (should (= (multisession-value foo) 0))
            (cl-incf (multisession-value foo))
            (should (= (multisession-value foo) 1))
            (call-process
             (concat invocation-directory invocation-name)
             nil t nil
             "-Q" "-batch"
             "--eval" (prin1-to-string
                       `(progn
                          (require 'multisession)
                          (let ((multisession-directory ,dir)
                                (multisession-storage 'sqlite)
                                (user-init-file "/tmp/foo.el"))
                            (define-multisession-variable foo 0
                              ""
                              :synchronized t)
                            (cl-incf (multisession-value foo))))))
            (should (= (multisession-value foo) 2)))
        (sqlite-close multisession--db)
        (setq multisession--db nil)))))

(ert-deftest multi-test-sqlite-busy ()
  (skip-unless (and t (sqlite-available-p)))
  (ert-with-temp-file dir
    :directory t
    (let ((user-init-file "/tmp/foo.el")
          (multisession-directory dir)
          (multisession-storage 'sqlite)
          proc)
      (unwind-protect
          (progn
            (define-multisession-variable bar 0
              ""
              :synchronized t)
            (should (= (multisession-value bar) 0))
            (cl-incf (multisession-value bar))
            (should (= (multisession-value bar) 1))
            (setq proc
                  (start-process
                   "other-emacs"
                   nil
                   (concat invocation-directory invocation-name)
                   "-Q" "-batch"
                   "--eval" (prin1-to-string
                             `(progn
                                (require 'multisession)
                                (let ((multisession-directory ,dir)
                                      (multisession-storage 'sqlite)
                                      (user-init-file "/tmp/bar.el"))
                                  (define-multisession-variable bar 0
                                    "" :synchronized t)
                                  (dotimes (i 100)
                                    (cl-incf (multisession-value bar))))))))
            (while (process-live-p proc)
              (ignore-error 'sqlite-locked-error
                (message "bar %s" (multisession-value bar))
                ;;(cl-incf (multisession-value bar))
                )
              (sleep-for 0.1))
            (message "bar ends up as %s" (multisession-value bar))
            (should (< (multisession-value bar) 1003)))
        (sqlite-close multisession--db)
        (setq multisession--db nil)))))

(ert-deftest multi-test-files-simple ()
  (ert-with-temp-file dir
    :directory t
    (let ((user-init-file "/tmp/sfoo.el")
          (multisession-storage 'files)
          (multisession-directory dir))
      (define-multisession-variable sfoo 0
        ""
        :synchronized t)
      (should (= (multisession-value sfoo) 0))
      (cl-incf (multisession-value sfoo))
      (should (= (multisession-value sfoo) 1))
      (call-process
       (concat invocation-directory invocation-name)
       nil t nil
       "-Q" "-batch"
       "--eval" (prin1-to-string
                 `(progn
                    (require 'multisession)
                    (let ((multisession-directory ,dir)
                          (multisession-storage 'files)
                          (user-init-file "/tmp/sfoo.el"))
                      (define-multisession-variable sfoo 0
                        ""
                        :synchronized t)
                      (cl-incf (multisession-value sfoo))))))
      (should (= (multisession-value sfoo) 2)))))

(ert-deftest multi-test-files-busy ()
  (skip-unless (and t (sqlite-available-p)))
  (ert-with-temp-file dir
    :directory t
    (let ((user-init-file "/tmp/foo.el")
          (multisession-storage 'files)
          (multisession-directory dir)
          proc)
      (define-multisession-variable sbar 0
        ""
        :synchronized t)
      (should (= (multisession-value sbar) 0))
      (cl-incf (multisession-value sbar))
      (should (= (multisession-value sbar) 1))
      (setq proc
            (start-process
             "other-emacs"
             nil
             (concat invocation-directory invocation-name)
             "-Q" "-batch"
             "--eval" (prin1-to-string
                       `(progn
                          (require 'multisession)
                          (let ((multisession-directory ,dir)
                                (multisession-storage 'files)
                                (user-init-file "/tmp/sbar.el"))
                            (define-multisession-variable sbar 0
                              "" :synchronized t)
                            (dotimes (i 1000)
                              (cl-incf (multisession-value sbar))))))))
      (while (process-live-p proc)
        (message "sbar %s" (multisession-value sbar))
        ;;(cl-incf (multisession-value sbar))
        (sleep-for 0.1))
      (message "sbar ends up as %s" (multisession-value sbar))
      (should (< (multisession-value sbar) 2000)))))

(ert-deftest multi-test-files-some-values ()
  (ert-with-temp-file dir
    :directory t
    (let ((user-init-file "/tmp/sfoo.el")
          (multisession-storage 'files)
          (multisession-directory dir))
      (define-multisession-variable foo1 nil)
      (should (eq (multisession-value foo1) nil))
      (setf (multisession-value foo1) nil)
      (should (eq (multisession-value foo1) nil))
      (setf (multisession-value foo1) t)
      (should (eq (multisession-value foo1) t))

      (define-multisession-variable foo2 t)
      (setf (multisession-value foo2) nil)
      (should (eq (multisession-value foo2) nil))
      (setf (multisession-value foo2) t)
      (should (eq (multisession-value foo2) t))

      (define-multisession-variable foo3 t)
      (should-error (setf (multisession-value foo3) (make-marker)))

      (let ((string (with-temp-buffer
                      (set-buffer-multibyte nil)
                      (insert 0 1 2)
                      (buffer-string))))
        (should-not (multibyte-string-p string))
        (define-multisession-variable foo4 nil)
        (setf (multisession-value foo4) string)
        (should (equal (multisession-value foo4) string))))))

;;; multisession-tests.el ends here
