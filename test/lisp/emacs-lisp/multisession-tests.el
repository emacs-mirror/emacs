;;; multisession-tests.el --- Tests for multisession.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2024 Free Software Foundation, Inc.

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

(declare-function sqlite-close "sqlite.c")

(ert-deftest multi-test-sqlite-simple ()
  (skip-unless (sqlite-available-p))
  (ert-with-temp-file dir
    :directory t
    (let ((user-init-file "/tmp/foo.el")
          (multisession-storage 'sqlite)
          (multisession-directory dir))
      (unwind-protect
          (progn
            (define-multisession-variable multisession--foo 0
              ""
              :synchronized t)
            (should (= (multisession-value multisession--foo) 0))
            (cl-incf (multisession-value multisession--foo))
            (should (= (multisession-value multisession--foo) 1))
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
                            (define-multisession-variable multisession--foo 0
                              ""
                              :synchronized t)
                            (cl-incf (multisession-value multisession--foo))))))
            (should (= (multisession-value multisession--foo) 2)))
        (sqlite-close multisession--db)
        (setq multisession--db nil)))))

(ert-deftest multi-test-sqlite-busy ()
  (skip-unless (sqlite-available-p))
  (ert-with-temp-file dir
    :directory t
    (let ((user-init-file "/tmp/foo.el")
          (multisession-directory dir)
          (multisession-storage 'sqlite)
          proc)
      (unwind-protect
          (progn
            (define-multisession-variable multisession--bar 0
              ""
              :synchronized t)
            (should (= (multisession-value multisession--bar) 0))
            (cl-incf (multisession-value multisession--bar))
            (should (= (multisession-value multisession--bar) 1))
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
                                  (define-multisession-variable multisession--bar 0
                                    "" :synchronized t)
                                  (dotimes (i 100)
                                    (cl-incf (multisession-value multisession--bar))))))))
            (while (process-live-p proc)
              (ignore-error 'sqlite-locked-error
                (message "multisession--bar %s" (multisession-value multisession--bar))
                ;;(cl-incf (multisession-value multisession--bar))
                )
              (sleep-for 0.1))
            (message "multisession--bar ends up as %s" (multisession-value multisession--bar))
            (should (< (multisession-value multisession--bar) 1003)))
        (sqlite-close multisession--db)
        (setq multisession--db nil)))))

(ert-deftest multi-test-files-simple ()
  (ert-with-temp-file dir
    :directory t
    (let ((user-init-file "/tmp/sfoo.el")
          (multisession-storage 'files)
          (multisession-directory dir))
      (define-multisession-variable multisession--sfoo 0
        ""
        :synchronized t)
      (should (= (multisession-value multisession--sfoo) 0))
      (cl-incf (multisession-value multisession--sfoo))
      (should (= (multisession-value multisession--sfoo) 1))
      ;; On Windows and Haiku, we don't have sub-second resolution, so
      ;; let some time pass to make the "later" logic work.
      (when (memq system-type '(windows-nt haiku))
        (sleep-for 0.6))
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
                      (define-multisession-variable multisession--sfoo 0
                        ""
                        :synchronized t)
                      (cl-incf (multisession-value multisession--sfoo))))))
      (should (= (multisession-value multisession--sfoo) 2)))))

(ert-deftest multi-test-files-busy ()
  (skip-unless (sqlite-available-p))
  (ert-with-temp-file dir
    :directory t
    (let ((user-init-file "/tmp/foo.el")
          (multisession-storage 'files)
          (multisession-directory dir)
          proc)
      (define-multisession-variable multisession--sbar 0
        ""
        :synchronized t)
      (should (= (multisession-value multisession--sbar) 0))
      (cl-incf (multisession-value multisession--sbar))
      (should (= (multisession-value multisession--sbar) 1))
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
                            (define-multisession-variable multisession--sbar 0
                              "" :synchronized t)
                            (dotimes (i 100)
                              (cl-incf (multisession-value multisession--sbar))))))))
      (while (process-live-p proc)
        (message "multisession--sbar %s" (multisession-value multisession--sbar))
        ;;(cl-incf (multisession-value multisession--sbar))
        (sleep-for 0.1))
      (message "multisession--sbar ends up as %s" (multisession-value multisession--sbar))
      (should (< (multisession-value multisession--sbar) 200)))))

(ert-deftest multi-test-files-some-values ()
  (ert-with-temp-file dir
    :directory t
    (let ((user-init-file "/tmp/sfoo.el")
          (multisession-storage 'files)
          (multisession-directory dir))
      (define-multisession-variable multisession--foo1 nil)
      (should (eq (multisession-value multisession--foo1) nil))
      (setf (multisession-value multisession--foo1) nil)
      (should (eq (multisession-value multisession--foo1) nil))
      (setf (multisession-value multisession--foo1) t)
      (should (eq (multisession-value multisession--foo1) t))

      (define-multisession-variable multisession--foo2 t)
      (setf (multisession-value multisession--foo2) nil)
      (should (eq (multisession-value multisession--foo2) nil))
      (setf (multisession-value multisession--foo2) t)
      (should (eq (multisession-value multisession--foo2) t))

      (define-multisession-variable multisession--foo3 t)
      (should-error (setf (multisession-value multisession--foo3) (make-marker)))

      (let ((string (with-temp-buffer
                      (set-buffer-multibyte nil)
                      (insert 0 1 2)
                      (buffer-string))))
        (should-not (multibyte-string-p string))
        (define-multisession-variable multisession--foo4 nil)
        (setf (multisession-value multisession--foo4) string)
        (should (equal (multisession-value multisession--foo4) string))))))

;;; multisession-tests.el ends here
