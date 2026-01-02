;;; time-tests.el --- Tests for time.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2026 Free Software Foundation, Inc.

;; Author: Stefan Kangas <stefankangas@gmail.com>

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

(require 'ert)
(require 'ert-x)
(require 'time)

(ert-deftest time-tests-display-time-mail-check-directory ()
  (let ((display-time-mail-directory (ert-resource-directory)))
    (should (display-time-mail-check-directory))))

(ert-deftest time-tests-display-time-update--load ()
  (let ((display-time-load-average 1)
        (display-time-load-average-threshold 0))
    (display-time-next-load-average)
    (should (string-match (rx string-start " "
                              (+ (| digit "."))
                              string-end)
                          (display-time-update--load))))
  (let (display-time-load-average)
    (should (equal (display-time-update--load) ""))))

(ert-deftest time-tests-display-time-update ()
  (let ((display-time-load-average 1)
        (display-time-load-average-threshold 0)
        display-time-string)
    (display-time-update)
    (should (string-match (rx string-start
                              (? digit) digit ":" digit digit
                              (? (| "AM" "PM"))
                              " " (+ (| digit "."))
                              (? " Mail")
                              " "
                              string-end)
                          display-time-string))))

(ert-deftest time-tests-display-time-file-nonempty-p ()
  (should (display-time-file-nonempty-p (ert-resource-file "non-empty")))
  (should-not (display-time-file-nonempty-p "/non/existent")))

(ert-deftest time-tests-world-clock ()
  (save-window-excursion
    (world-clock)
    (should (equal (buffer-name) world-clock-buffer-name))
    (should (string-match "New York" (buffer-string)))))

(ert-deftest time-tests-world-clock/revert-buffer-works ()
  (save-window-excursion
    (world-clock)
    (revert-buffer)
    (should (string-match "New York" (buffer-string)))))

(ert-deftest time-tests-emacs-uptime ()
  (should (string-match "^[0-9.]+ seconds?$" (emacs-uptime "%S"))))

(ert-deftest time-tests-emacs-init-time ()
  (should (string-match "^[0-9.]+ seconds?$" (emacs-init-time))))

(provide 'time-tests)
;;; time-tests.el ends here
