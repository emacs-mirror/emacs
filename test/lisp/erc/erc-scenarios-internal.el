;;; erc-scenarios-internal.el --- Proxy file for erc-d tests -*- lexical-binding: t -*-

;; Copyright (C) 2022-2026 Free Software Foundation, Inc.

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

;;; Code:

(require 'ert-x)
(eval-and-compile
  (when (and (getenv "EMACS_TEST_DIRECTORY")
             (getenv "EMACS_TEST_JUNIT_REPORT"))
    (setq ert-load-file-name (or (macroexp-file-name) buffer-file-name)))
  (let ((load-path `(,(expand-file-name "erc-d" (ert-resource-directory))
                     ,(ert-resource-directory)
                     ,@load-path)))
    ;; Run all tests in ./resources/erc-d/erc-d-tests.el.
    (load "erc-d-tests" nil 'silent)
    (require 'erc-tests-common)))

;; Run all tests tagged `:erc--graphical' in an "interactive"
;; subprocess.  Time out after 90 seconds.
(ert-deftest erc-scenarios-internal--run-graphical-all ()
  :tags '(:expensive-test :unstable)
  (unless (and (getenv "ERC_TESTS_GRAPHICAL_ALL")
               (not (getenv "ERC_TESTS_GRAPHICAL"))
               (not (getenv "CI")))
    (ert-skip "Environmental conditions unmet"))

  (let* ((default-directory (expand-file-name "../" (ert-resource-directory)))
         (libs (directory-files default-directory 'full (rx ".el" eot)))
         (process-environment (cons "ERC_TESTS_GRAPHICAL=1"
                                    process-environment))
         (program '(progn (ert (quote (tag :erc--graphical)))
                          (with-current-buffer ert--output-buffer-name
                            (kill-emacs (ert--stats-failed-unexpected
                                         ert--results-stats)))))
         (proc (erc-tests-common-create-subprocess program
                                                   '( "-L" "." "-l" "ert")
                                                   libs)))

    (erc-d-t-wait-for 90 "interactive tests to complete"
      (not (process-live-p proc)))

    (should (zerop (process-exit-status proc)))))

;;; erc-scenarios-internal.el ends here
