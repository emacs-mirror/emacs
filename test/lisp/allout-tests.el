;;; allout-tests.el --- Tests for allout.el  -*- lexical-binding: t -*-

;; Copyright (C) 2020-2024 Free Software Foundation, Inc.

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
(require 'allout)

(require 'cl-lib)

(defun allout-tests-obliterate-variable (name)
  "Completely unbind variable with NAME."
  (if (local-variable-p name (current-buffer)) (kill-local-variable name))
  (while (boundp name) (makunbound name)))

(defvar allout-tests-globally-unbound nil
  "Fodder for allout resumptions tests -- defvar just for byte compiler.")
(defvar allout-tests-globally-true nil
  "Fodder for allout resumptions tests -- defvar just for byte compiler.")
(defvar allout-tests-locally-true nil
  "Fodder for allout resumptions tests -- defvar just for byte compiler.")

;; For each resumption case, we also test that the right local/global
;; scopes are affected during resumption effects.

(ert-deftest allout-test-resumption-unbound-return-to-unbound  ()
  "Previously unbound variables return to the unbound state."
  (with-temp-buffer
    (allout-tests-obliterate-variable 'allout-tests-globally-unbound)
    (allout-add-resumptions '(allout-tests-globally-unbound t))
    (should (not (default-boundp 'allout-tests-globally-unbound)))
    (should (local-variable-p 'allout-tests-globally-unbound (current-buffer)))
    (should (boundp 'allout-tests-globally-unbound))
    (should (equal allout-tests-globally-unbound t))
    (allout-do-resumptions)
    (should (not (local-variable-p 'allout-tests-globally-unbound
                                      (current-buffer))))
    (should (not (boundp 'allout-tests-globally-unbound)))))

(ert-deftest allout-test-resumption-variable-resumed  ()
  "Ensure that variable with prior global value is resumed."
  (with-temp-buffer
    (allout-tests-obliterate-variable 'allout-tests-globally-true)
    (setq allout-tests-globally-true t)
    (allout-add-resumptions '(allout-tests-globally-true nil))
    (should (equal (default-value 'allout-tests-globally-true) t))
    (should (local-variable-p 'allout-tests-globally-true (current-buffer)))
    (should (equal allout-tests-globally-true nil))
    (allout-do-resumptions)
    (should (not (local-variable-p 'allout-tests-globally-true
                                   (current-buffer))))
    (should (boundp 'allout-tests-globally-true))
    (should (equal allout-tests-globally-true t))))

(ert-deftest allout-test-resumption-prior-value-resumed ()
  "Ensure that prior local value is resumed."
  (with-temp-buffer
    (allout-tests-obliterate-variable 'allout-tests-locally-true)
    (setq-local allout-tests-locally-true t)
    (cl-assert (not (default-boundp 'allout-tests-locally-true))
               nil (concat "Test setup mistake -- variable supposed to"
                           " not have global binding, but it does."))
    (cl-assert (local-variable-p 'allout-tests-locally-true (current-buffer))
               nil (concat "Test setup mistake -- variable supposed to have"
                           " local binding, but it lacks one."))
    (allout-add-resumptions '(allout-tests-locally-true nil))
    (should (not (default-boundp 'allout-tests-locally-true)))
    (should (local-variable-p 'allout-tests-locally-true (current-buffer)))
    (should (equal allout-tests-locally-true nil))
    (allout-do-resumptions)
    (should (boundp 'allout-tests-locally-true))
    (should (local-variable-p 'allout-tests-locally-true (current-buffer)))
    (should (equal allout-tests-locally-true t))
    (should (not (default-boundp 'allout-tests-locally-true)))))

(ert-deftest allout-test-resumption-multiple-holds ()
  "Ensure that last of multiple resumptions holds, for various scopes."
  (with-temp-buffer
    (allout-tests-obliterate-variable 'allout-tests-globally-unbound)
    (allout-tests-obliterate-variable 'allout-tests-globally-true)
    (setq allout-tests-globally-true t)
    (allout-tests-obliterate-variable 'allout-tests-locally-true)
    (setq-local allout-tests-locally-true t)
    (allout-add-resumptions '(allout-tests-globally-unbound t)
                            '(allout-tests-globally-true nil)
                            '(allout-tests-locally-true nil))
    (allout-add-resumptions '(allout-tests-globally-unbound 2)
                            '(allout-tests-globally-true 3)
                            '(allout-tests-locally-true 4))
    ;; reestablish many of the basic conditions are maintained after re-add:
    (should (not (default-boundp 'allout-tests-globally-unbound)))
    (should (local-variable-p 'allout-tests-globally-unbound (current-buffer)))
    (should (equal allout-tests-globally-unbound 2))
    (should (default-boundp 'allout-tests-globally-true))
    (should (local-variable-p 'allout-tests-globally-true (current-buffer)))
    (should (equal allout-tests-globally-true 3))
    (should (not (default-boundp 'allout-tests-locally-true)))
    (should (local-variable-p 'allout-tests-locally-true (current-buffer)))
    (should (equal allout-tests-locally-true 4))
    (allout-do-resumptions)
    (should (not (local-variable-p 'allout-tests-globally-unbound
                                   (current-buffer))))
    (should (not (boundp 'allout-tests-globally-unbound)))
    (should (not (local-variable-p 'allout-tests-globally-true
                                   (current-buffer))))
    (should (boundp 'allout-tests-globally-true))
    (should (equal allout-tests-globally-true t))
    (should (boundp 'allout-tests-locally-true))
    (should (local-variable-p 'allout-tests-locally-true (current-buffer)))
    (should (equal allout-tests-locally-true t))
    (should (not (default-boundp 'allout-tests-locally-true)))))

(ert-deftest allout-test-resumption-unbinding ()
  "Ensure that deliberately unbinding registered variables doesn't foul things."
  (with-temp-buffer
    (allout-tests-obliterate-variable 'allout-tests-globally-unbound)
    (allout-tests-obliterate-variable 'allout-tests-globally-true)
    (setq allout-tests-globally-true t)
    (allout-tests-obliterate-variable 'allout-tests-locally-true)
    (setq-local allout-tests-locally-true t)
    (allout-add-resumptions '(allout-tests-globally-unbound t)
                            '(allout-tests-globally-true nil)
                            '(allout-tests-locally-true nil))
    (allout-tests-obliterate-variable 'allout-tests-globally-unbound)
    (allout-tests-obliterate-variable 'allout-tests-globally-true)
    (allout-tests-obliterate-variable 'allout-tests-locally-true)
    (allout-do-resumptions)))

(provide 'allout-tests)
;;; allout-tests.el ends here
