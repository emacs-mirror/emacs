;;; spelling-tests.el --- Test suite for so-long.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Free Software Foundation, Inc.

;; Author: Phil Sainty <psainty@orcon.net.nz>
;; Keywords: convenience

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

(require 'ert)
(require 'ispell)
(require 'cl-lib)

;; This test is tagged as :unstable on the basis that there may be
;; inconsistencies between spell-checking facilities on different
;; systems, which may cause the test to be unreliable in practice.
;; As such the Emacs test Makefile will skip it by default, but you
;; can run it manually with:
;;
;; make lisp/so-long-tests/spelling-tests SELECTOR=t

;; Only define the test if spell-checking is possible.
(when (and ispell-program-name
           (executable-find ispell-program-name)
           (condition-case ()
               (progn (ispell-check-version) t)
             (error nil))
           (member "british" (ispell-valid-dictionary-list)))
  (ert-deftest so-long-spelling ()
    "Check the spelling in the source code."
    :tags '(:unstable) ;; It works for me, but I'm not sure about others.
    ;; There could be different "british" dictionaries yielding different
    ;; results, for instance.
    ;;
    ;; The Emacs test Makefile's use of HOME=/nonexistent triggers an error
    ;; when starting the inferior ispell process, so we set HOME to a valid
    ;; (but empty) temporary directory for this test.
    (let* ((tmpdir (make-temp-file "so-long." :dir ".ispell"))
           (process-environment (cons (format "HOME=%s" tmpdir)
                                      process-environment))
           (find-spelling-mistake
            (unwind-protect
                (cl-letf (((symbol-function 'ispell-command-loop)
                           (lambda (_miss _guess word _start _end)
                             (message "Unrecognized word: %s." word)
                             (throw 'mistake t))))
                  (catch 'mistake
                    (find-library "so-long")
                    (ispell-buffer)
                    nil))
              (delete-directory tmpdir))))
      (should (not find-spelling-mistake)))))

;;; spelling-tests.el ends here
