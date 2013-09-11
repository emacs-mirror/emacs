;;; data-tests.el --- tests for src/data.c

;; Copyright (C) 2013 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see `http://www.gnu.org/licenses/'.

;;; Commentary:

;;; Code:

(ert-deftest data-tests-= ()
  (should-error (=))
  (should (= 1))
  (should (= 2 2))
  (should (= 9 9 9 9 9 9 9 9 9))
  (should-not (apply #'= '(3 8 3)))
  (should-error (= 9 9 'foo))
  ;; Short circuits before getting to bad arg
  (should-not (= 9 8 'foo)))

(ert-deftest data-tests-< ()
  (should-error (<))
  (should (< 1))
  (should (< 2 3))
  (should (< -6 -1 0 2 3 4 8 9 999))
  (should-not (apply #'< '(3 8 3)))
  (should-error (< 9 10 'foo))
  ;; Short circuits before getting to bad arg
  (should-not (< 9 8 'foo)))

(ert-deftest data-tests-> ()
  (should-error (>))
  (should (> 1))
  (should (> 3 2))
  (should (> 6 1 0 -2 -3 -4 -8 -9 -999))
  (should-not (apply #'> '(3 8 3)))
  (should-error (> 9 8 'foo))
  ;; Short circuits before getting to bad arg
  (should-not (> 8 9 'foo)))

(ert-deftest data-tests-<= ()
  (should-error (<=))
  (should (<= 1))
  (should (<= 2 3))
  (should (<= -6 -1 -1 0 0 0 2 3 4 8 999))
  (should-not (apply #'<= '(3 8 3 3)))
  (should-error (<= 9 10 'foo))
  ;; Short circuits before getting to bad arg
  (should-not (<= 9 8 'foo)))

(ert-deftest data-tests->= ()
  (should-error (>=))
  (should (>= 1))
  (should (>= 3 2))
  (should (>= 666 1 0 0 -2 -3 -3 -3 -4 -8 -8 -9 -999))
  (should-not (apply #'>= '(3 8 3)))
  (should-error (>= 9 8 'foo))
  ;; Short circuits before getting to bad arg
  (should-not (>= 8 9 'foo)))

;;; data-tests.el ends here

