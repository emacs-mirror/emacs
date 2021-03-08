;;; custom-tests.el --- tests for custom.el  -*- lexical-binding: t -*-

;; Copyright (C) 2018-2021 Free Software Foundation, Inc.

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'ert)
(require 'ert-x)

(require 'wid-edit)
(require 'cus-edit)

(defmacro custom-tests--with-temp-dir (&rest body)
  "Eval BODY with `temporary-file-directory' bound to a fresh directory.
Ensure the directory is recursively deleted after the fact."
  (declare (debug t) (indent 0))
  (let ((dir (make-symbol "dir")))
    `(let ((,dir (file-name-as-directory (make-temp-file "custom-tests-" t))))
       (unwind-protect
           (let ((temporary-file-directory ,dir))
             ,@body)
         (delete-directory ,dir t)))))

(ert-deftest custom-theme--load-path ()
  "Test `custom-theme--load-path' behavior."
  (custom-tests--with-temp-dir
    ;; Path is empty.
    (let ((custom-theme-load-path ()))
      (should (null (custom-theme--load-path))))

    ;; Path comprises non-existent file.
    (let* ((name (make-temp-name temporary-file-directory))
           (custom-theme-load-path (list name)))
      (should (not (file-exists-p name)))
      (should (null (custom-theme--load-path))))

    ;; Path comprises existing file.
    (let* ((file (make-temp-file "file"))
           (custom-theme-load-path (list file)))
      (should (file-exists-p file))
      (should (not (file-directory-p file)))
      (should (null (custom-theme--load-path))))

    ;; Path comprises existing directory.
    (let* ((dir (make-temp-file "dir" t))
           (custom-theme-load-path (list dir)))
      (should (file-directory-p dir))
      (should (equal (custom-theme--load-path) custom-theme-load-path)))

    ;; Expand `custom-theme-directory' path element.
    (let ((custom-theme-load-path '(custom-theme-directory)))
      (let ((custom-theme-directory (make-temp-name temporary-file-directory)))
        (should (not (file-exists-p custom-theme-directory)))
        (should (null (custom-theme--load-path))))
      (let ((custom-theme-directory (make-temp-file "file")))
        (should (file-exists-p custom-theme-directory))
        (should (not (file-directory-p custom-theme-directory)))
        (should (null (custom-theme--load-path))))
      (let ((custom-theme-directory (make-temp-file "dir" t)))
        (should (file-directory-p custom-theme-directory))
        (should (equal (custom-theme--load-path)
                       (list custom-theme-directory)))))

    ;; Expand t path element.
    (let ((custom-theme-load-path '(t)))
      (let ((data-directory (make-temp-name temporary-file-directory)))
        (should (not (file-exists-p data-directory)))
        (should (null (custom-theme--load-path))))
      (let ((data-directory temporary-file-directory)
            (themedir (expand-file-name "themes" temporary-file-directory)))
        (should (not (file-exists-p themedir)))
        (should (null (custom-theme--load-path)))
        (with-temp-file themedir)
        (should (file-exists-p themedir))
        (should (not (file-directory-p themedir)))
        (should (null (custom-theme--load-path)))
        (delete-file themedir)
        (make-directory themedir)
        (should (file-directory-p themedir))
        (should (equal (custom-theme--load-path) (list themedir)))))))

(ert-deftest custom-tests-require-theme ()
  "Test `require-theme'."
  (custom-tests--with-temp-dir
    (let* ((default-directory temporary-file-directory)
           (custom-theme-load-path (list default-directory))
           (load-path ()))
      ;; Generate some `.el' and `.elc' files.
      (with-temp-file "custom-tests--a.el"
        (insert "(provide 'custom-tests--a)"))
      (make-empty-file "custom-tests--b.el")
      (with-temp-file "custom-tests--b.elc"
        (byte-compile-insert-header nil (current-buffer))
        (insert "(provide 'custom-tests--b)"))
      (make-empty-file "custom-tests--c.el")
      (with-temp-file "custom-tests--d.elc"
        (byte-compile-insert-header nil (current-buffer)))
      ;; Load them.
      (dolist (feature '(a b c d e))
        (should-not (featurep (intern (format "custom-tests--%s" feature)))))
      (should (eq (require-theme 'custom-tests--a) 'custom-tests--a))
      (delete-file "custom-tests--a.el")
      (dolist (feature '(custom-tests--a custom-tests--b))
        (should (eq (require-theme feature) feature))
        (should (featurep feature)))
      (dolist (feature '(custom-tests--c custom-tests--d))
        (dolist (noerror '(nil t))
          (let ((err (should-error (require-theme feature noerror))))
            (should (string-search "failed to provide feature" (cadr err))))))
      (should-error (require-theme 'custom-tests--e) :type 'file-missing)
      (should-not (require-theme 'custom-tests--e t))
      (dolist (feature '(custom-tests--c custom-tests--d custom-tests--e))
        (should-not (featurep feature))))))

(defcustom custom--test-user-option 'foo
  "User option for test."
  :group 'emacs
  :type 'symbol)

(defvar custom--test-variable 'foo
  "Variable for test.")

;; This is demonstrating bug#34027.
(ert-deftest custom--test-theme-variables ()
  "Test variables setting with enabling / disabling a custom theme."
  ;; We load custom-resources/custom--test-theme.el.
  (let ((custom-theme-load-path `(,(ert-resource-directory))))
    (load-theme 'custom--test 'no-confirm 'no-enable)
    ;; The variables have still their initial values.
    (should (equal custom--test-user-option 'foo))
    (should (equal custom--test-variable 'foo))

    (custom-set-variables
     '(custom--test-user-option 'baz)
     '(custom--test-variable 'baz))
    ;; The initial values have been changed.
    (should (equal custom--test-user-option 'baz))
    (should (equal custom--test-variable 'baz))

    ;; Enable and then disable.
    (enable-theme 'custom--test)
    (disable-theme 'custom--test)
    ;; The variables should have the changed values, by reverting.
    (should (equal custom--test-user-option 'baz))
    (should (equal custom--test-variable 'baz))))

;; This tests Bug#5358.
(ert-deftest custom-test-show-comment-preserves-changes ()
  "Test that adding a comment doesn't discard modifications in progress."
  (customize-option 'custom--test-user-option)
  (let* ((field (seq-find (lambda (widget)
                            (eq custom--test-user-option (widget-value widget)))
                          widget-field-list))
         (parent (widget-get field :parent))
	 (origvalue (widget-value field)))
    ;; Move to the end of the text of the widget, and modify it.  This
    ;; modification should be preserved after showing the comment field.
    (goto-char (widget-field-text-end field))
    (insert "bar")
    (custom-comment-show parent)
    ;; From now on, must use `widget-at' to get the value of the widget.
    (should-not (eq origvalue (widget-value (widget-at))))
    (should (eq (widget-get parent :custom-state) 'modified))
    (should (eq (widget-value (widget-at))
                (widget-apply field
                              :value-to-external
                              (concat
                               (widget-apply field :value-to-internal origvalue)
                               "bar"))))))

(ert-deftest custom-test-enable-theme-keeps-settings ()
  "Test that enabling a theme doesn't change its settings."
  (let* ((custom-theme-load-path `(,(ert-resource-directory)))
         settings)
    (load-theme 'custom--test 'no-confirm 'no-enable)
    (setq settings (get 'custom--test 'theme-settings))
    (enable-theme 'custom--test)
    (should (equal settings (get 'custom--test 'theme-settings)))))

(defcustom custom--test-local-option 'initial
  "Buffer-local user option for testing."
  :group 'emacs
  :type '(choice (const initial) (const changed))
  :local t)

(defcustom custom--test-permanent-option 'initial
  "Permanently local user option for testing."
  :group 'emacs
  :type '(choice (const initial) (const changed))
  :local 'permanent)

(ert-deftest custom-test-local-option ()
  "Test :local user options."
  ;; Initial default values.
  (should (eq custom--test-local-option 'initial))
  (should (eq custom--test-permanent-option 'initial))
  (should (eq (default-value 'custom--test-local-option) 'initial))
  (should (eq (default-value 'custom--test-permanent-option) 'initial))
  (let ((obuf (current-buffer)))
    (with-temp-buffer
      ;; Changed buffer-local values.
      (setq custom--test-local-option 'changed)
      (setq custom--test-permanent-option 'changed)
      (should (eq custom--test-local-option 'changed))
      (should (eq custom--test-permanent-option 'changed))
      (should (eq (default-value 'custom--test-local-option) 'initial))
      (should (eq (default-value 'custom--test-permanent-option) 'initial))
      (with-current-buffer obuf
        (should (eq custom--test-local-option 'initial))
        (should (eq custom--test-permanent-option 'initial)))
      ;; Permanent variable remains unchanged.
      (kill-all-local-variables)
      (should (eq custom--test-local-option 'initial))
      (should (eq custom--test-permanent-option 'changed))
      (should (eq (default-value 'custom--test-local-option) 'initial))
      (should (eq (default-value 'custom--test-permanent-option) 'initial)))))

;;; custom-tests.el ends here
