;;; eshell-tests-unload.el --- test unloading Eshell  -*- lexical-binding:t -*-

;; Copyright (C) 2023-2026 Free Software Foundation, Inc.

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

;; Tests for unloading Eshell.

;;; Code:

(require 'ert)
(require 'ert-x)

;; In order to test unloading Eshell, don't require any of its files
;; at the top level.  This means we need to explicitly declare some of
;; the variables and functions we'll use.
(defvar eshell-directory-name)
(defvar eshell-history-file-name)
(defvar eshell-last-dir-ring-file-name)
(defvar eshell-modules-list)
(defvar eshell-module-loading-messages)

(declare-function eshell-module--feature-name "esh-module"
                  (module &optional kind))
(declare-function eshell-subgroups "esh-util" (groupsym))

(defvar max-unload-time 5
  "The maximum amount of time to wait to unload Eshell modules, in seconds.
See `unload-eshell'.")

(defun load-eshell ()
  "Load Eshell by calling the `eshell' function and immediately closing it."
  (save-current-buffer
    (ert-with-temp-directory eshell-directory-name
      (let* (;; We want no history file, so prevent Eshell from falling
             ;; back on $HISTFILE.
             (process-environment (cons "HISTFILE" process-environment))
             (eshell-history-file-name nil)
             (eshell-last-dir-ring-file-name nil)
             (eshell-module-loading-messages nil)
             (eshell-buffer (eshell t)))
        (let (kill-buffer-query-functions)
          (kill-buffer eshell-buffer))))))

(defun unload-eshell ()
  "Unload Eshell, waiting until the core modules are unloaded as well."
  (let ((debug-on-error t)
        (inhibit-message t))
    (unload-feature 'eshell)
    ;; We unload core modules are unloaded from a timer, since they
    ;; need to wait until after `eshell' itself is unloaded.  Wait for
    ;; this to finish.
    (let ((start (current-time)))
      (while (featurep 'esh-arg)
        (when (> (float-time (time-since start))
                 max-unload-time)
          (error "timed out waiting to unload Eshell modules"))
        (sit-for 0.1)))))

;;; Tests:

(ert-deftest eshell-test-unload/default ()
  "Test unloading Eshell with the default list of extension modules."
  (load-eshell)
  (unload-eshell))

(ert-deftest eshell-test-unload/no-modules ()
  "Test unloading Eshell with no extension modules."
  (require 'esh-module)
  (let (eshell-modules-list)
    (load-eshell))
  (dolist (module (eshell-subgroups 'eshell-module))
    (should-not (featurep (intern (eshell-module--feature-name module)))))
  (unload-eshell))

(ert-deftest eshell-test-unload/all-modules ()
  "Test unloading Eshell with every extension module."
  (require 'esh-module)
  (let ((eshell-modules-list (eshell-subgroups 'eshell-module)))
    (load-eshell))
  (dolist (module (eshell-subgroups 'eshell-module))
    (should (featurep (intern (eshell-module--feature-name module)))))
  (unload-eshell))

(provide 'eshell-tests-unload)
;;; eshell-tests-unload.el ends here
