;;; custom-tests.el --- tests for custom.el  -*- lexical-binding: t -*-

;; Copyright (C) 2018-2024 Free Software Foundation, Inc.

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
(require 'bytecomp)

(ert-deftest custom-theme--load-path ()
  "Test `custom-theme--load-path' behavior."
  (ert-with-temp-directory temporary-file-directory
    ;; Path is empty.
    (let ((custom-theme-load-path ()))
      (should (null (custom-theme--load-path))))

    ;; Path comprises non-existent file.
    (let* ((name (make-temp-name temporary-file-directory))
           (custom-theme-load-path (list name)))
      (should (not (file-exists-p name)))
      (should (null (custom-theme--load-path))))

    ;; Path comprises existing file.
    (ert-with-temp-file file
      (let* ((custom-theme-load-path (list file)))
        (should (file-exists-p file))
        (should (not (file-directory-p file)))
        (should (null (custom-theme--load-path)))))

    ;; Path comprises existing directory.
    (ert-with-temp-directory dir
      (let* ((custom-theme-load-path (list dir)))
        (should (file-directory-p dir))
        (should (equal (custom-theme--load-path) custom-theme-load-path))))

    ;; Expand `custom-theme-directory' path element.
    (let ((custom-theme-load-path '(custom-theme-directory)))
      (let ((custom-theme-directory (make-temp-name temporary-file-directory)))
        (should (not (file-exists-p custom-theme-directory)))
        (should (null (custom-theme--load-path))))
      (ert-with-temp-file custom-theme-directory
        (should (file-exists-p custom-theme-directory))
        (should (not (file-directory-p custom-theme-directory)))
        (should (null (custom-theme--load-path))))
      (ert-with-temp-directory custom-theme-directory
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
  (require 'warnings)
  (ert-with-temp-directory temporary-file-directory
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

;; The following three tests demonstrate Bug#21355.
;; In this one, we set an user option for the current session and then
;; we enable a theme that doesn't have a setting for it, ending up with
;; a non-nil saved-value property.  Since the `caar' of the theme-value
;; property is user (i.e., the user theme setting is active), we might
;; save the setting to the custom-file, even though it was meant for the
;; current session only.  So there should be a nil saved-value property
;; for this test to pass.
(ert-deftest custom-test-no-saved-value-after-enabling-theme ()
  "Test that we don't record a saved-value property when we shouldn't."
  (let ((custom-theme-load-path `(,(ert-resource-directory))))
    (customize-option 'mark-ring-max)
    (let* ((field (seq-find (lambda (widget)
                              (eq mark-ring-max (widget-value widget)))
                            widget-field-list))
           (parent (widget-get field :parent)))
      ;; Move to the editable widget, modify the value and save it.
      (goto-char (widget-field-text-end field))
      (insert "0")
      (widget-apply parent :custom-set)
      ;; Just setting for the current session should not store a saved-value
      ;; property.
      (should-not (get 'mark-ring-max 'saved-value))
      ;; Now enable and disable the test theme.
      (load-theme 'custom--test 'no-confirm)
      (disable-theme 'custom--test)
      ;; Since the user customized the option, this is OK.
      (should (eq (caar (get 'mark-ring-max 'theme-value)) 'user))
      ;; The saved-value property should still be nil.
      (should-not (get 'mark-ring-max 'saved-value)))))

;; In this second test, we load a theme that has a setting for the user option
;; above.  We must check that we don't end up with a non-nil saved-value
;; property and a user setting active in the theme-value property, which
;; means we might inadvertently save the session setting in the custom-file.
(defcustom custom--test-bug-21355-before 'foo
  "User option for `custom-test-no-saved-value-after-enabling-theme-2'."
  :type 'symbol :group 'emacs)

(ert-deftest custom-test-no-saved-value-after-enabling-theme-2 ()
  "Test that we don't record a saved-value property when we shouldn't."
  (let ((custom-theme-load-path `(,(ert-resource-directory))))
    (customize-option 'custom--test-bug-21355-before)
    (let* ((field (seq-find
                   (lambda (widget)
                     (eq custom--test-bug-21355-before (widget-value widget)))
                   widget-field-list))
           (parent (widget-get field :parent)))
      ;; Move to the editable widget, modify the value and save it.
      (goto-char (widget-field-text-end field))
      (insert "bar")
      (widget-apply parent :custom-set)
      ;; Just setting for the current session should not store a saved-value
      ;; property.
      (should-not (get 'custom--test-bug-21355-before 'saved-value))
      ;; Now load our test theme, which has a setting for
      ;; `custom--test-bug-21355-before'.
      (load-theme 'custom--test 'no-confirm 'no-enable)
      (enable-theme 'custom--test)
      ;; Since the user customized the option, this is OK.
      (should (eq (caar (get 'custom--test-bug-21355-before 'theme-value))
                  'user))
      ;; But the saved-value property has to be nil, since the user didn't mark
      ;; this variable to save for future sessions.
      (should-not (get 'custom--test-bug-21355-before 'saved-value)))))

(defvar custom--test-bug-21355-after)

;; In this test, we check that stashing a theme value for a not yet defined
;; option works, but that later on if the user customizes the option for the
;; current session, we might save the theme setting in the custom file.
(ert-deftest custom-test-no-saved-value-after-customizing-option ()
  "Test for a nil saved-value after setting an option for the current session."
  (let ((custom-theme-load-path `(,(ert-resource-directory))))
    ;; Check that we correctly stashed the value.
    (load-theme 'custom--test 'no-confirm 'no-enable)
    (enable-theme 'custom--test)
    (should (and (not (boundp 'custom--test-bug-21355-after))
                 (eq (eval
                      (car (get 'custom--test-bug-21355-after 'saved-value)))
                     'after)))
    ;; Now Emacs finds the defcustom.
    (defcustom custom--test-bug-21355-after 'initially "..."
      :type 'symbol :group 'emacs)
    ;; And we used the stashed value correctly.
    (should (and (boundp 'custom--test-bug-21355-after)
                 (eq custom--test-bug-21355-after 'after)))
    ;; Now customize it.
    (customize-option 'custom--test-bug-21355-after)
    (let* ((field (seq-find (lambda (widget)
                              (eq custom--test-bug-21355-after
                                  (widget-value widget)))
                            widget-field-list))
           (parent (widget-get field :parent)))
      ;; Move to the editable widget, modify the value and save it.
      (goto-char (widget-field-text-end field))
      (insert "bar")
      (widget-apply parent :custom-set)
      ;; The user customized the variable, so this is OK.
      (should (eq (caar (get 'custom--test-bug-21355-after 'theme-value))
                  'user))
      ;; But it was only for the current session, so this should not happen.
      (should-not (get 'custom--test-bug-21355-after 'saved-value)))))

;;; custom-tests.el ends here
