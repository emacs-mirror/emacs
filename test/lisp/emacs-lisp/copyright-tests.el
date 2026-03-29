;;; copyright-tests.el --- tests for copyright.el  -*- lexical-binding: t -*-

;; Copyright (C) 2020-2026 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'copyright)

(defmacro with-copyright-test (orig result)
  `(cl-letf (((symbol-function 'format-time-string) (lambda (&rest _) "2019")))
     (let ((copyright-query nil)
           (copyright-current-year 2019))
       (with-temp-buffer
         (insert ,orig)
         (copyright-update)
         (should (equal (buffer-string) ,result))))))

(defvar copyright-tests--data
  '((";; Copyright (C) 2017 Free Software Foundation, Inc."
     . ";; Copyright (C) 2017, 2019 Free Software Foundation, Inc.")
    (";; Copyright (C) 2017-2018 Free Software Foundation, Inc."
     . ";; Copyright (C) 2017-2019 Free Software Foundation, Inc.")
    (";; Copyright (C) 2017–2018 Free Software Foundation, Inc."
     . ";; Copyright (C) 2017–2019 Free Software Foundation, Inc.")
    (";; Copyright (C) 2005-2006, 2015, 2017-2018 Free Software Foundation, Inc."
     . ";; Copyright (C) 2005-2006, 2015, 2017-2019 Free Software Foundation, Inc.")
    (";; Copyright (C) 2005–2006, 2015, 2017–2018 Free Software Foundation, Inc."
     . ";; Copyright (C) 2005–2006, 2015, 2017–2019 Free Software Foundation, Inc.")
    (";; copyright '18 FSF"
     . ";; copyright '18, '19 FSF")))

(ert-deftest test-copyright-update ()
  (dolist (test copyright-tests--data)
    (with-copyright-test (car test) (cdr test))))

(ert-deftest test-end-chop ()
  (should
   (equal
    (with-temp-buffer
      (let ((copyright-query nil))
        (insert (make-string (- copyright-limit 14) ?x) "\n"
                "\nCopyright 2006, 2007, 2008 Foo Bar\n\n")
        (copyright-update)
        (buffer-substring (- (point-max) 42) (point-max))))
    (format "Copyright 2006, 2007, 2008, %s Foo Bar\n\n"
            (format-time-string "%Y")))))

(ert-deftest test-correct-notice ()
  (should (equal
           (with-temp-buffer
             (dotimes (_ 2)
               (insert "Copyright 2021 FSF\n"))
             (let ((copyright-at-end-flag t)
                   (copyright-query nil))
               (copyright-update))
             (buffer-string))
           (format "Copyright 2021 FSF\nCopyright 2021, %s FSF\n"
                   (format-time-string "%Y")))))

(defmacro with-copyright-fix-years-test (orig result)
  `(let ((copyright-year-ranges t))
     (with-temp-buffer
       (insert ,orig)
       (copyright-fix-years)
       (should (equal (buffer-string) ,result)))))

(defvar copyright-fix-years-tests--data
  '((";; Copyright (C) 2008, 2010, 2012"
     . ";; Copyright (C) 2008, 2010, 2012")
    (";; Copyright (C) 2008, 2009, 2010, 2013, 2014, 2015, 2016, 2018"
     . ";; Copyright (C) 2008-2010, 2013-2016, 2018")
    (";; Copyright (C) 2008-2010, 2011, 2015, 2016, 2017"
     . ";; Copyright (C) 2008-2010, 2011, 2015-2017")))

(ert-deftest text-copyright-fix-years ()
  "Test basics of \\[copyright-fix-years]."
  (dolist (test copyright-fix-years-tests--data)
    (with-copyright-fix-years-test (car test) (cdr test))))

(provide 'copyright-tests)
;;; copyright-tests.el ends here
