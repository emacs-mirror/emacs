;;; emerge-tests.el --- Tests of shadowfile  -*- lexical-binding:t -*-

(require 'tramp)
(require 'ert-x)
(require 'emerge)

(setq auth-source-save-behavior nil
      password-cache-expiry nil
      ;; When the remote user id is 0, Tramp refuses unsafe temporary files.
      tramp-allow-unsafe-temporary-files
      (or tramp-allow-unsafe-temporary-files noninteractive)
      tramp-cache-read-persistent-data t ;; For auth-sources.
      tramp-persistency-file-name nil
      tramp-verbose 0
      ;; On macOS, `temporary-file-directory' is a symlinked directory.
      temporary-file-directory (file-truename temporary-file-directory)
      ert-remote-temporary-file-directory
      (ignore-errors (file-truename ert-remote-temporary-file-directory)))

(ert-deftest emerge-test-files ()
  "Check emerge for two files."
  (skip-when (memq system-type '(windows-nt ms-dos)))
  (skip-unless (file-remote-p ert-remote-temporary-file-directory))

  ;; Remote file.
  (ert-with-temp-file
      file1 :prefix (expand-file-name "emerge-tests" ert-remote-temporary-file-directory) :text "foo"
  ;; Local file.
  (ert-with-temp-file
      file2 :prefix (expand-file-name "emerge-tests" temporary-file-directory) :text "foo"
  ;; Output.
  (ert-with-temp-file
      file3 :prefix (expand-file-name "emerge-tests" temporary-file-directory)

      ;; Run emerge.
      (should (emerge-files nil file1 file2 file3))
      (cl-letf (((symbol-function #'y-or-n-p) #'always))
        (emerge-quit nil))

      ;; Check result.
      (with-temp-buffer
        (insert-file-contents file3)
        (should (string-equal "foo" (buffer-string))))))))

(ert-deftest emerge-test-files-with-ancestor ()
  "Check emerge for three files."
  (skip-when (memq system-type '(windows-nt ms-dos)))
  (skip-unless (file-remote-p ert-remote-temporary-file-directory))

  ;; Remote file.
  (ert-with-temp-file
      file1 :prefix (expand-file-name "emerge-tests" ert-remote-temporary-file-directory) :text "foo"
  ;; Local file.
  (ert-with-temp-file
      file2 :prefix (expand-file-name "emerge-tests" temporary-file-directory) :text "foo"
  ;; Remote file.
  (ert-with-temp-file
      file3 :prefix (expand-file-name "emerge-tests" ert-remote-temporary-file-directory) :text "foo"
  ;; Output.
  (ert-with-temp-file
      file4 :prefix (expand-file-name "emerge-tests" temporary-file-directory)

      ;; Run emerge.
      (should (emerge-files-with-ancestor nil file1 file2 file3 file4))
      (cl-letf (((symbol-function #'y-or-n-p) #'always))
        (emerge-quit nil))

      ;; Check result.
      (with-temp-buffer
        (insert-file-contents file4)
        (should (string-equal "foo" (buffer-string)))))))))

(defun emerge-test-all (&optional interactive)
  "Run all tests for `emerge-*'."
  (interactive "p")
  (if interactive
      (ert-run-tests-interactively "^emerge-")
    (ert-run-tests-batch "^emerge-")))

(provide 'emerge-tests)
;;; emerge-tests.el ends here
