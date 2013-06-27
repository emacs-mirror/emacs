;;; package-test.el --- Tests for the Emacs package system

;; Copyright (C) 2013 Free Software Foundation, Inc.

;; Author: Daniel Hackney <dan@haxney.org>
;; Version: 1.0

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; You may want to run this from a separate Emacs instance from your
;; main one, because a bug in the code below could mess with your
;; installed packages.

;; Run this in a clean Emacs session using:
;;
;;     $ emacs -Q --batch -L . -l package-x-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'package-x)
(require 'ert)
(require 'cl-lib)

;; package-test is not normally in `load-path', so temporarily set
;; `load-path' to contain the current directory.
(let ((load-path (append (list (file-name-directory (or load-file-name
                                                        buffer-file-name)))
                         load-path)))
  (require 'package-test))

(defvar package-x-test--single-archive-entry-1-3
  (cons 'simple-single
        (package-make-ac-desc '(1 3) nil
                              "A single-file package with no dependencies"
                              'single))
  "Expected contents of the archive entry from the \"simple-single\" package.")

(defvar package-x-test--single-archive-entry-1-4
  (cons 'simple-single
        (package-make-ac-desc '(1 4) nil
                              "A single-file package with no dependencies"
                              'single))
  "Expected contents of the archive entry from the updated \"simple-single\" package.")

(ert-deftest package-x-test-upload-buffer ()
  "Test creating an \"archive-contents\" file"
  (with-package-test (:basedir "data/package"
                               :file "simple-single-1.3.el"
                               :upload-base t)
    (package-upload-buffer)
    (should (file-exists-p (expand-file-name "archive-contents"
                                             package-archive-upload-base)))
    (should (file-exists-p (expand-file-name "simple-single-1.3.el"
                                             package-archive-upload-base)))
    (should (file-exists-p (expand-file-name "simple-single-readme.txt"
                                             package-archive-upload-base)))

    (let (archive-contents)
      (with-temp-buffer
        (insert-file-contents
         (expand-file-name "archive-contents"
                           package-archive-upload-base))
        (setq archive-contents
              (package-read-from-string
               (buffer-substring (point-min) (point-max)))))
      (should (equal archive-contents
                     (list 1 package-x-test--single-archive-entry-1-3))))))

(ert-deftest package-x-test-upload-new-version ()
  "Test uploading a new version of a package"
  (with-package-test (:basedir "data/package"
                               :file "simple-single-1.3.el"
                               :upload-base t)
    (package-upload-buffer)
    (with-temp-buffer
      (insert-file-contents "newer-versions/simple-single-1.4.el")
      (package-upload-buffer))

    (let (archive-contents)
      (with-temp-buffer
        (insert-file-contents
         (expand-file-name "archive-contents"
                           package-archive-upload-base))
        (setq archive-contents
              (package-read-from-string
               (buffer-substring (point-min) (point-max)))))
      (should (equal archive-contents
                     (list 1 package-x-test--single-archive-entry-1-4))))))

(provide 'package-x-test)

;;; package-x-test.el ends here
