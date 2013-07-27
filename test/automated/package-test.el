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
;;     $ emacs -Q --batch -L . -l package-test.el -l ert -f ert-run-tests-batch-and-exit

;;; Code:

(require 'package)
(require 'ert)
(require 'cl-lib)

(defvar package-test-user-dir nil
  "Directory to use for installing packages during testing.")

(defvar package-test-file-dir (file-name-directory (or load-file-name
                                                       buffer-file-name))
  "Directory of the actual \"package-test.el\" file.")

(defvar simple-single-desc
  (package-desc-create :name 'simple-single
                       :version '(1 3)
                       :summary "A single-file package with no dependencies"
                       :kind 'single)
  "Expected `package-desc' parsed from simple-single-1.3.el.")

(defvar simple-single-desc-1-4
  (package-desc-create :name 'simple-single
                       :version '(1 4)
                       :summary "A single-file package with no dependencies"
                       :kind 'single)
  "Expected `package-desc' parsed from simple-single-1.4.el.")

(defvar simple-depend-desc
  (package-desc-create :name 'simple-depend
                       :version '(1 0)
                       :summary "A single-file package with a dependency."
                       :kind 'single
                       :reqs '((simple-single (1 3))))
  "Expected `package-desc' parsed from simple-depend-1.0.el.")

(defvar multi-file-desc
  (package-desc-create :name 'multi-file
                       :version '(0 2 3)
                       :summary "Example of a multi-file tar package"
                       :kind 'tar)
  "Expected `package-desc' from \"multi-file-0.2.3.tar\".")

(defvar new-pkg-desc
  (package-desc-create :name 'new-pkg
                       :version '(1 0)
                       :kind 'single)
  "Expected `package-desc' parsed from new-pkg-1.0.el.")

(defvar package-test-data-dir (expand-file-name "data/package" package-test-file-dir)
  "Base directory of package test files.")

(defvar package-test-fake-contents-file
  (expand-file-name "archive-contents" package-test-data-dir)
  "Path to a static copy of \"archive-contents\".")

(defvar package-test-built-file-suffixes '(".tar" "/dir" "/*.info")
  "Remove these files when cleaning up a built package.")

(cl-defmacro with-package-test ((&optional &key file
                                           basedir
                                           install
                                           update-news
                                           upload-base)
                                &rest body)
  "Set up temporary locations and variables for testing."
  (declare (indent 1))
  `(let* ((package-test-user-dir (make-temp-file "pkg-test-user-dir-" t))
          (package-user-dir package-test-user-dir)
          (package-archives `(("gnu" . ,package-test-data-dir)))
          (old-yes-no-defn (symbol-function 'yes-or-no-p))
          (old-pwd default-directory)
          package--initialized
          package-alist
          ,@(if update-news
                '(package-update-news-on-upload t)
              (list (cl-gensym)))
          ,@(if upload-base
                '((package-test-archive-upload-base (make-temp-file "pkg-archive-base-" t))
                  (package-archive-upload-base package-test-archive-upload-base))
              (list (cl-gensym)))) ;; Dummy value so `let' doesn't try to bind `nil'
     (unwind-protect
         (progn
           ,(if basedir `(cd ,basedir))
           (setf (symbol-function 'yes-or-no-p) #'(lambda (&rest r) t))
           (unless (file-directory-p package-user-dir)
             (mkdir package-user-dir))
           ,@(when install
               `((package-initialize)
                 (package-refresh-contents)
                 (mapc 'package-install ,install)))
           (with-temp-buffer
             ,(if file
                  `(insert-file-contents ,file))
             ,@body))

       (when (file-directory-p package-test-user-dir)
         (delete-directory package-test-user-dir t))

       (when (and (boundp 'package-test-archive-upload-base)
                  (file-directory-p package-test-archive-upload-base))
         (delete-directory package-test-archive-upload-base t))
       (setf (symbol-function 'yes-or-no-p) old-yes-no-defn)
       (cd old-pwd))))

(defmacro with-fake-help-buffer (&rest body)
  "Execute BODY in a temp buffer which is treated as the \"*Help*\" buffer."
  `(with-temp-buffer
    (help-mode)
    ;; Trick `help-buffer' into using the temp buffer.
    (let ((help-xref-following t))
      ,@body)))

(autoload 'makeinfo-buffer "makeinfo")
(defvar compilation-in-progress)

(defun package-test-install-texinfo (file)
  "Install from texinfo FILE.

FILE should be a .texinfo file relative to the current
`default-directory'"
  (require 'info)
  (let* ((full-file (expand-file-name file))
         (info-file (replace-regexp-in-string "\\.texi\\'" ".info" full-file))
         (old-info-defn (symbol-function 'Info-revert-find-node)))
    (require 'info)
    (setf (symbol-function 'Info-revert-find-node) #'ignore)
    (with-current-buffer (find-file-literally full-file)
      (unwind-protect
          (progn
            (makeinfo-buffer)
            ;; Give `makeinfo-buffer' a chance to finish
            (while compilation-in-progress
              (sit-for 0.1))
            (call-process "ginstall-info" nil nil nil
                          (format "--info-dir=%s" default-directory)
                          (format "%s" info-file)))
        (kill-buffer)
        (setf (symbol-function 'Info-revert-find-node) old-info-defn)))))

(defun package-test-strip-version (dir)
  (replace-regexp-in-string "-pkg\\.el\\'" "" (package--description-file dir)))

(defun package-test-suffix-matches (base suffix-list)
  "Return file names matching BASE concatenated with each item in SUFFIX-LIST"
  (cl-mapcan
   '(lambda (item) (file-expand-wildcards (concat base item)))
   suffix-list))

(defun package-test-cleanup-built-files (dir)
  "Remove files which were the result of creating a tar archive.

DIR is the base name of the package directory, without the trailing slash"
  (let* ((pkg-dirname (file-name-nondirectory dir)))
    (dolist (file (package-test-suffix-matches dir package-test-built-file-suffixes))
      (delete-file file))))

(defvar tar-parse-info)
(declare-function tar-header-name "tar-mode" (cl-x) t) ; defstruct

(defun package-test-search-tar-file (filename)
  "Search the current buffer's `tar-parse-info' variable for FILENAME.

Must called from within a `tar-mode' buffer."
  (cl-dolist (header tar-parse-info)
    (let ((tar-name (tar-header-name header)))
      (when (string= tar-name filename)
        (cl-return t)))))

(defun package-test-desc-version-string (desc)
  "Return the package version as a string."
  (package-version-join (package-desc-version desc)))

(ert-deftest package-test-desc-from-buffer ()
  "Parse an elisp buffer to get a `package-desc' object."
  (with-package-test (:basedir "data/package" :file "simple-single-1.3.el")
    (should (equal (package-buffer-info) simple-single-desc)))
  (with-package-test (:basedir "data/package" :file "simple-depend-1.0.el")
    (should (equal (package-buffer-info) simple-depend-desc)))
  (with-package-test (:basedir "data/package"
                               :file "multi-file-0.2.3.tar")
    (tar-mode)
    (should (equal (package-tar-file-info) multi-file-desc))))

(ert-deftest package-test-install-single ()
  "Install a single file without using an archive."
  (with-package-test (:basedir "data/package" :file "simple-single-1.3.el")
    (should (package-install-from-buffer))
    (package-initialize)
    (should (package-installed-p 'simple-single))
    (let* ((simple-pkg-dir (file-name-as-directory
                            (expand-file-name
                             "simple-single-1.3"
                             package-test-user-dir)))
           (autoloads-file (expand-file-name "simple-single-autoloads.el"
                                             simple-pkg-dir)))
      (should (file-directory-p simple-pkg-dir))
      (with-temp-buffer
        (insert-file-contents (expand-file-name "simple-single-pkg.el"
                                                simple-pkg-dir))
        (should (string= (buffer-string)
                         (concat "(define-package \"simple-single\" \"1.3\" "
                                 "\"A single-file package "
                                 "with no dependencies\" 'nil)\n"))))
      (should (file-exists-p autoloads-file))
      (should-not (get-file-buffer autoloads-file)))))

(ert-deftest package-test-install-dependency ()
  "Install a package which includes a dependency."
  (with-package-test ()
    (package-initialize)
    (package-refresh-contents)
    (package-install 'simple-depend)
    (should (package-installed-p 'simple-single))
    (should (package-installed-p 'simple-depend))))

(ert-deftest package-test-refresh-contents ()
  "Parse an \"archive-contents\" file."
  (with-package-test ()
    (package-initialize)
    (package-refresh-contents)
    (should (eq 3 (length package-archive-contents)))))

(ert-deftest package-test-install-single-from-archive ()
  "Install a single package from a package archive."
  (with-package-test ()
    (package-initialize)
    (package-refresh-contents)
    (package-install 'simple-single)))

(ert-deftest package-test-install-multifile ()
  "Check properties of the installed multi-file package."
  (with-package-test (:basedir "data/package" :install '(multi-file))
    (let ((autoload-file
           (expand-file-name "multi-file-autoloads.el"
                             (expand-file-name
                              "multi-file-0.2.3"
                              package-test-user-dir)))
          (installed-files '("dir" "multi-file.info" "multi-file-sub.elc"
                             "multi-file-autoloads.el" "multi-file.elc"))
          (autoload-forms '("^(defvar multi-file-custom-var"
                            "^(custom-autoload 'multi-file-custom-var"
                            "^(autoload 'multi-file-mode"))
          (pkg-dir (file-name-as-directory
                    (expand-file-name
                     "multi-file-0.2.3"
                     package-test-user-dir))))
      (package-refresh-contents)
      (should (package-installed-p 'multi-file))
      (with-temp-buffer
        (insert-file-contents-literally autoload-file)
        (dolist (fn installed-files)
          (should (file-exists-p (expand-file-name fn pkg-dir))))
        (dolist (re autoload-forms)
          (goto-char (point-min))
          (should (re-search-forward re nil t)))))))

(ert-deftest package-test-update-listing ()
  "Ensure installed package status is updated."
  (with-package-test ()
    (let ((buf (package-list-packages)))
      (search-forward-regexp "^ +simple-single")
      (package-menu-mark-install)
      (package-menu-execute)
      (should (package-installed-p 'simple-single))
      (switch-to-buffer "*Packages*")
      (goto-char (point-min))
      (should (re-search-forward "^\\s-+simple-single\\s-+1.3\\s-+installed" nil t))
      (goto-char (point-min))
      (should-not (re-search-forward "^\\s-+simple-single\\s-+1.3\\s-+\\(available\\|new\\)" nil t))
      (kill-buffer buf))))

(ert-deftest package-test-update-archives ()
  "Test updating package archives."
  (with-package-test ()
    (let ((buf (package-list-packages)))
      (package-menu-refresh)
      (search-forward-regexp "^ +simple-single")
      (package-menu-mark-install)
      (package-menu-execute)
      (should (package-installed-p 'simple-single))
      (let ((package-test-data-dir
             (expand-file-name "data/package/newer-versions" package-test-file-dir)))
        (setq package-archives `(("gnu" . ,package-test-data-dir)))
        (package-menu-refresh)

        ;; New version should be available and old version should be installed
        (goto-char (point-min))
        (should (re-search-forward "^\\s-+simple-single\\s-+1.4\\s-+new" nil t))
        (should (re-search-forward "^\\s-+simple-single\\s-+1.3\\s-+installed" nil t))

        (goto-char (point-min))
        (should (re-search-forward "^\\s-+new-pkg\\s-+1.0\\s-+\\(available\\|new\\)" nil t))

        (package-menu-mark-upgrades)
        (package-menu-execute)
        (package-menu-refresh)
        (should (package-installed-p 'simple-single '(1 4)))))))

(ert-deftest package-test-describe-package ()
  "Test displaying help for a package."

  (require 'finder-inf)
  ;; Built-in
  (with-fake-help-buffer
   (describe-package '5x5)
   (goto-char (point-min))
   (should (search-forward "5x5 is a built-in package." nil t))
   (should (search-forward "Status: Built-in." nil t))
   (should (search-forward "Summary: simple little puzzle game" nil t))
   (should (search-forward "The aim of 5x5" nil t)))

  ;; Installed
  (with-package-test ()
    (package-initialize)
    (package-refresh-contents)
    (package-install 'simple-single)
    (with-fake-help-buffer
     (describe-package 'simple-single)
     (goto-char (point-min))
     (should (search-forward "simple-single is an installed package." nil t))
     (should (search-forward
              (format "Status: Installed in `%s/'."
                      (expand-file-name "simple-single-1.3" package-user-dir))
              nil t))
     (should (search-forward "Version: 1.3" nil t))
     (should (search-forward "Summary: A single-file package with no dependencies"
                             nil t))
     ;; No description, though. Because at this point we don't know
     ;; what archive the package originated from, and we don't have
     ;; its readme file saved.
     )))

(ert-deftest package-test-describe-not-installed-package ()
  "Test displaying of the readme for not-installed package."

  (with-package-test ()
    (package-initialize)
    (package-refresh-contents)
    (with-fake-help-buffer
     (describe-package 'simple-single)
     (goto-char (point-min))
     (should (search-forward "This package provides a minor mode to frobnicate"
                             nil t)))))

(ert-deftest package-test-describe-non-installed-package ()
  "Test displaying of the readme for non-installed package."

  (with-package-test ()
    (package-initialize)
    (package-refresh-contents)
    (with-fake-help-buffer
     (describe-package 'simple-single)
     (goto-char (point-min))
     (should (search-forward "This package provides a minor mode to frobnicate"
                             nil t)))))

(ert-deftest package-test-describe-non-installed-multi-file-package ()
  "Test displaying of the readme for non-installed multi-file package."

  (with-package-test ()
    (package-initialize)
    (package-refresh-contents)
    (with-fake-help-buffer
     (describe-package 'multi-file)
     (goto-char (point-min))
     (should (search-forward "This is a bare-bones readme file for the multi-file"
                             nil t)))))

(provide 'package-test)

;;; package-test.el ends here
