;;; cus-edit-tests.el --- Tests for cus-edit.el  -*- lexical-binding: t -*-

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
(require 'ert-x)
(eval-when-compile (require 'cl-lib))
(require 'cus-edit)

(defmacro with-cus-edit-test (buffer &rest body)
  (declare (indent 1))
  `(save-window-excursion
     (unwind-protect
         (progn ,@body)
       (when-let ((buf (get-buffer ,buffer)))
         (kill-buffer buf)))))


;;;; showing/hiding obsolete options

(defgroup cus-edit-tests nil "Test."
  :group 'test-group)

(defcustom cus-edit-tests--obsolete-option-tag nil
  "This should never be removed; it is obsolete for testing purposes."
  :type 'boolean
  :version "917.10") ; a super high version number
(make-obsolete-variable 'cus-edit-tests--obsolete-option-tag nil "X.X-test")
(defconst cus-edit-tests--obsolete-option-tag
  (custom-unlispify-tag-name 'cus-edit-tests--obsolete-option-tag))

(ert-deftest cus-edit-tests-customize-apropos/hide-obsolete ()
  (with-cus-edit-test "*Customize Apropos*"
    (customize-apropos "cus-edit-tests")
    (should-not (search-forward cus-edit-tests--obsolete-option-tag nil t))))

(ert-deftest cus-edit-tests-customize-changed/hide-obsolete ()
  (with-cus-edit-test "*Customize Changed Options*"
    (customize-changed "917.2") ;; Some future version.
    (should-not (search-forward cus-edit-tests--obsolete-option-tag nil t))))

(ert-deftest cus-edit-tests-customize-group/hide-obsolete ()
  "Check that obsolete variables do not show up."
  (with-cus-edit-test "*Customize Group: Cus Edit Tests*"
    (customize-group 'cus-edit-tests)
    (should-not (search-forward cus-edit-tests--obsolete-option-tag nil t))))

(ert-deftest cus-edit-tests-customize-option/show-obsolete ()
  (with-cus-edit-test "*Customize Option: Cus Edit Tests Obsolete Option Tag*"
    (customize-option 'cus-edit-tests--obsolete-option-tag)
    (goto-char (point-min))
    (should (search-forward cus-edit-tests--obsolete-option-tag nil t))))

(ert-deftest cus-edit-tests-customize-saved/show-obsolete ()
  (with-cus-edit-test "*Customize Saved*"
    (cl-letf (((get 'cus-edit-tests--obsolete-option-tag 'saved-value) '(t)))
      (customize-saved)
      (should (search-forward cus-edit-tests--obsolete-option-tag nil t)))))

(defcustom cus-edit-test-foo1 0
  ""
  :type 'number)

(ert-deftest test-setopt ()
  (should (= (setopt cus-edit-test-foo1 1) 1))
  (should (= cus-edit-test-foo1 1))
  (let* ((text-quoting-style 'grave)
         (warn-txt
          (with-current-buffer (get-buffer-create "*Warnings*")
            (let ((inhibit-read-only t))
              (erase-buffer))
            (setopt cus-edit-test-foo1 :foo)
            (buffer-substring-no-properties (point-min) (point-max)))))
    (should (string-search "Value `:foo' does not match type number"
                           warn-txt))))
(provide 'cus-edit-tests)
;;; cus-edit-tests.el ends here
