;;; emacsclient-tests.el --- Test emacsclient

;; Copyright (C) 2016-2020 Free Software Foundation, Inc.

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

;;

;;; Code:

(require 'ert)

(defconst emacsclient-test-emacs
  (if installation-directory
      (expand-file-name "lib-src/emacsclient" installation-directory)
    "emacsclient")
  "The emacsclient binary to test.")

(defmacro emacsclient-test-call-emacsclient (editor)
  "Run emacsclient with ALTERNATE_EDITOR set to EDITOR."
  `(let* ((process-environment
           (cons (concat "ALTERNATE_EDITOR=" ,editor) process-environment))
          (stat (call-process emacsclient-test-emacs nil nil nil
                              "--server-file"
                              (expand-file-name "non-existent-file"
                                                invocation-directory)
                              "foo")))
     ;; Skip if emacsclient was compiled with -pg (bug#28319).
     ;; Use ert--skip-unless rather than skip-unless to silence compiler.
     (ert--skip-unless (not (and (stringp stat)
                                 (string-match-p "Profiling" stat))))
     (should (eq 0 stat))))

(ert-deftest emacsclient-test-alternate-editor-allows-arguments ()
  (emacsclient-test-call-emacsclient
   (concat (expand-file-name invocation-name invocation-directory) " --batch")))

(ert-deftest emacsclient-test-alternate-editor-allows-quotes ()
  (emacsclient-test-call-emacsclient
   (concat "\"" (expand-file-name invocation-name invocation-directory)
           "\"" " --batch")))

(provide 'emacsclient-tests)
;;; emacsclient-tests.el ends here
