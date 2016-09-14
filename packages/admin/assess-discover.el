;; This is a hacked version, needs to be merged in with main (probably
;; this version is correct!)

(defun assess-discover-tests (directory)
  "Discover tests in directory.

Tests must conform to one (and only one!) of several naming
schemes.

 - End with -test.el
 - End with -tests.el
 - Start with test-
 - Any .el file in a directory called test
 - Any .el file in a directory called tests

Each of these is tried until one matches. So, a top-level file
called \"blah-test.el\" will prevent discovery of files in a
tests directory."
  (or
   ;; files with
   (directory-files directory nil ".*-test.el$")
   (directory-files directory nil ".*-tests.el$")
   (directory-files directory nil "test-.*.el$")
   (let ((dir-test
          (concat directory "/test/")))
     (when (file-exists-p dir-test)
       (mapcar
        (lambda (file)
          (concat dir-test file))
        (directory-files dir-test nil ".*.el"))))
   (let ((dir-tests
          (concat directory "/tests/")))
     (when (file-exists-p dir-tests)
       (mapcar
        (lambda (file)
          (concat dir-tests file))
        (directory-files dir-tests nil ".*.el"))))))

(defun assess-discover--load-all-tests (directory)
  (let ((loads
         (assess-discover-tests directory)))
    (mapc
     'load
     loads)))

(defun assess-discover-load-tests ()
  (interactive)
  (assess-discover--load-all-tests default-directory))

;;;###autoload
(defun assess-discover-run-batch (&optional selector)
  (assess-discover--load-all-tests default-directory)
  (ert-run-tests-batch selector))

;;;###autoload
(defun assess-discover-run-and-exit-batch (&optional selector)
  (assess-discover-run-and-exit-batch-dir default-directory))

(defun assess-discover-run-and-exit-batch-dir (directory &optional selector)
  (assess-discover--load-all-tests
   (expand-file-name directory))
  (ert-run-tests-batch-and-exit selector))
