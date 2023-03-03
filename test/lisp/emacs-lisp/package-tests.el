;;; package-tests.el --- Tests for the Emacs package system  -*- lexical-binding:t -*-

;; Copyright (C) 2013-2023 Free Software Foundation, Inc.

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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; You may want to run this from a separate Emacs instance from your
;; main one, because a bug in the code below could mess with your
;; installed packages.

;; Run this in a clean Emacs session using:
;;
;;     $ emacs -Q --batch -L . -l package-tests.el -l ert -f ert-run-tests-batch-and-exit
;;
;; From the top level directory of the Emacs development repository,
;; you can use this instead:
;;
;;     $ make -C test package-tests

;;; Code:

(require 'package)
(require 'ert)
(require 'ert-x)
(require 'cl-lib)

(setq package-menu-async nil)

(defvar package-test-user-dir nil
  "Directory to use for installing packages during testing.")

(defvar package-test-file-dir (file-name-directory (or load-file-name
                                                       buffer-file-name))
  "Directory of the actual \"package-test.el\" file.")

(defvar simple-single-desc
  (package-desc-create :name 'simple-single
                       :version '(1 3)
                       :summary "A single-file package with no dependencies"
                       :kind 'single
                       :extras '((:authors ("J. R. Hacker" . "jrh@example.com"))
                                 (:maintainer "J. R. Hacker" . "jrh@example.com")
                                 (:url . "http://doodles.au")))
  "Expected `package-desc' parsed from simple-single-1.3.el.")

(defvar simple-depend-desc
  (package-desc-create :name 'simple-depend
                       :version '(1 0)
                       :summary "A single-file package with a dependency."
                       :kind 'single
                       :reqs '((simple-single (1 3)))
                       :extras '((:authors ("J. R. Hacker" . "jrh@example.com"))
                                 (:maintainer "J. R. Hacker" . "jrh@example.com")))
  "Expected `package-desc' parsed from simple-depend-1.0.el.")

(defvar multi-file-desc
  (package-desc-create :name 'multi-file
                       :version '(0 2 3)
                       :summary "Example of a multi-file tar package"
                       :kind 'tar
                       :extras '((:url . "http://puddles.li")))
  "Expected `package-desc' from \"multi-file-0.2.3.tar\".")

(defvar new-pkg-desc
  (package-desc-create :name 'new-pkg
                       :version '(1 0)
                       :kind 'single)
  "Expected `package-desc' parsed from new-pkg-1.0.el.")

(defvar simple-depend-desc-1
  (package-desc-create :name 'simple-depend-1
                       :version '(1 0)
                       :summary "A single-file package with a dependency."
                       :kind 'single
                       :reqs '((simple-depend (1 0))
                               (multi-file (0 1))))
  "`package-desc' used for testing dependencies.")

(defvar simple-depend-desc-2
  (package-desc-create :name 'simple-depend-2
                       :version '(1 0)
                       :summary "A single-file package with a dependency."
                       :kind 'single
                       :reqs '((simple-depend-1 (1 0))
                               (multi-file (0 1))))
  "`package-desc' used for testing dependencies.")

(defvar package-test-data-dir (ert-resource-directory)
  "Base directory of package test files.")

(cl-defmacro with-package-test ((&optional &key file
                                           basedir
                                           install
                                           location
                                           update-news
                                           upload-base)
                                &rest body)
  "Set up temporary locations and variables for testing."
  (declare (indent 1) (debug (([&rest form]) body)))
  `(ert-with-temp-directory package-test-user-dir
     (let* ((process-environment (cons (format "HOME=%s" package-test-user-dir)
                                       process-environment))
            (package-user-dir package-test-user-dir)
            (package-gnupghome-dir (expand-file-name "gnupg" package-user-dir))
            (package-archives `(("gnu" . ,(or ,location package-test-data-dir))))
            (default-directory package-test-file-dir)
            abbreviated-home-dir
            package--initialized
            package-alist
            ,@(if update-news
                  '(package-update-news-on-upload t)
                (list (cl-gensym)))
            ,@(if upload-base
                  '((package-test-archive-upload-base (make-temp-file "pkg-archive-base-" t))
                    (package-archive-upload-base package-test-archive-upload-base))
                (list (cl-gensym)))) ;; Dummy value so `let' doesn't try to bind nil
       (let ((buf (get-buffer "*Packages*")))
         (when (buffer-live-p buf)
           (kill-buffer buf)))
       (unwind-protect
           (progn
             ,(if basedir `(cd ,basedir))
             (unless (file-directory-p package-user-dir)
               (mkdir package-user-dir))
             (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                       ((symbol-function 'y-or-n-p)    (lambda (&rest _) t)))
               ,@(when install
                   `((package-initialize)
                     (package-refresh-contents)
                     (mapc 'package-install ,install)))
               (with-temp-buffer
                 ,(if file
                      `(insert-file-contents ,file))
                 ,@body)))

         (when ,upload-base
           (dolist (f '("archive-contents"
                        "simple-single-1.3.el"
                        "simple-single-1.4.el"
                        "simple-single-readme.txt"))
             (ignore-errors
               (delete-file
                (expand-file-name f package-test-archive-upload-base))))
           (delete-directory package-test-archive-upload-base))

         (when (and (boundp 'package-test-archive-upload-base)
                    (file-directory-p package-test-archive-upload-base))
           (delete-directory package-test-archive-upload-base t))))))

(defmacro with-fake-help-buffer (&rest body)
  "Execute BODY in a temp buffer which is treated as the \"*Help*\" buffer."
  (declare (debug body))
  `(with-temp-buffer
    (help-mode)
    ;; Trick `help-buffer' into using the temp buffer.
    (let ((help-xref-following t))
      ,@body)))

(defun package-test-strip-version (dir)
  (replace-regexp-in-string "-pkg\\.el\\'" "" (package--description-file dir)))

(defun package-test-suffix-matches (base suffix-list)
  "Return file names matching BASE concatenated with each item in SUFFIX-LIST."
  (mapcan (lambda (item) (file-expand-wildcards (concat base item)))
          suffix-list))

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

(defun package-test--compatible-p (pkg-desc pkg-sample &optional kind)
  (and (cl-every (lambda (f)
                   (equal (funcall f pkg-desc)
                          (funcall f pkg-sample)))
                 (cons (if kind #'package-desc-kind #'ignore)
                       '(package-desc-name
                         package-desc-version
                         package-desc-summary
                         package-desc-reqs
                         package-desc-archive
                         package-desc-dir
                         package-desc-signed)))
       ;; The `extras' field should contain at least the specified elements.
       (let ((extras (package-desc-extras pkg-desc))
             (extras-sample (package-desc-extras pkg-sample)))
         (cl-every (lambda (sample-elem)
                     (member sample-elem extras))
                   extras-sample))))

(ert-deftest package-test-desc-from-buffer ()
  "Parse an elisp buffer to get a `package-desc' object."
  (with-package-test (:basedir (ert-resource-directory) :file "simple-single-1.3.el")
    (should (package-test--compatible-p
             (package-buffer-info) simple-single-desc 'kind)))
  (with-package-test (:basedir (ert-resource-directory) :file "simple-depend-1.0.el")
    (should (package-test--compatible-p
             (package-buffer-info) simple-depend-desc 'kind)))
  (with-package-test (:basedir (ert-resource-directory)
                               :file "multi-file-0.2.3.tar")
    (tar-mode)
    (should (equal (package-tar-file-info) multi-file-desc))))

(ert-deftest package-test-install-single ()
  "Install a single file without using an archive."
  (with-package-test (:basedir (ert-resource-directory) :file "simple-single-1.3.el")
    (should (package-install-from-buffer))
    (package-initialize)
    (should (package-installed-p 'simple-single))
    ;; Check if we properly report an "already installed".
    (package-install 'simple-single)
    (with-current-buffer "*Messages*"
      (should (string-match "^[`‘']simple-single[’'] is already installed\n?\\'"
                            (buffer-string))))
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
        (goto-char (point-min))
        (let ((sexp (read (current-buffer))))
          (should (eq (car-safe sexp) 'define-package))
          (should (package-test--compatible-p
                   (apply #'package-desc-from-define (cdr sexp))
                   simple-single-desc))))
      (should (file-exists-p autoloads-file))
      (should-not (get-file-buffer autoloads-file)))))

(ert-deftest package-test-install-file ()
  "Install files with `package-install-file'."
  (with-package-test (:basedir (ert-resource-directory))
    (package-initialize)
    (let* ((pkg-el "simple-single-1.3.el")
           (source-file (expand-file-name pkg-el (ert-resource-directory))))
      (should-not (package-installed-p 'simple-single))
      (package-install-file source-file)
      (should (package-installed-p 'simple-single))
      (package-delete (cadr (assq 'simple-single package-alist)))
      (should-not (package-installed-p 'simple-single)))

    (let* ((pkg-el "multi-file-0.2.3.tar")
           (source-file (expand-file-name pkg-el (ert-resource-directory))))
      (should-not (package-installed-p 'multie-file))
      (package-install-file source-file)
      (should (package-installed-p 'multi-file))
      (package-delete (cadr (assq 'multi-file package-alist))))))

(ert-deftest package-test-bug58367 ()
  "Check variations in tarball formats."
  (with-package-test (:basedir (ert-resource-directory))
    (package-initialize)

    ;; A package whose first entry is the main dir but without trailing /.
    (let* ((pkg-el "ustar-withsub-0.1.tar")
           (source-file (expand-file-name pkg-el (ert-resource-directory))))
      (should-not (package-installed-p 'ustar-withsub))
      (package-install-file source-file)
      (should (package-installed-p 'ustar-withsub))
      (package-delete (cadr (assq 'ustar-withsub package-alist))))

    ;; A package whose first entry is a file in a subdir.
    (let* ((pkg-el "v7-withsub-0.1.tar")
           (source-file (expand-file-name pkg-el (ert-resource-directory))))
      (should-not (package-installed-p 'v7-withsub))
      (package-install-file source-file)
      (should (package-installed-p 'v7-withsub))
      (package-delete (cadr (assq 'v7-withsub package-alist))))
    ))

(ert-deftest package-test-install-file-EOLs ()
  "Install same file multiple time with `package-install-file'
but with a different end of line convention (bug#48137)."
  (with-package-test (:basedir (ert-resource-directory))
    (package-initialize)
    (let* ((pkg-el "simple-single-1.3.el")
           (source-file (expand-file-name pkg-el (ert-resource-directory))))

      (with-temp-buffer
        (insert-file-contents source-file)

        (let (hashes)
          (dolist (coding '(unix dos mac) hashes)
            (let* ((eol-file (expand-file-name pkg-el package-test-user-dir)))
              ;; save package with this EOL convention.
              (set-buffer-file-coding-system coding)
              (write-region (point-min) (point-max) eol-file)

              (should-not (package-installed-p 'simple-single))
              (package-install-file eol-file)
              (should (package-installed-p 'simple-single))

              ;; check the package file has been installed unmodified.
              (let ((eol-hash (with-temp-buffer
                                (insert-file-contents-literally eol-file)
                                (buffer-hash))))
                ;; also perform an additional check that the package
                ;; file created with this EOL convention is different
                ;; than all the others created so far.
                (should-not (member eol-hash hashes))
                (setq hashes (cons eol-hash hashes))

                (let* ((descr (cadr (assq 'simple-single package-alist)))
                       (pkg-dir (package-desc-dir descr))
                       (dest-file (expand-file-name "simple-single.el" pkg-dir ))
                       (dest-hash (with-temp-buffer
                                    (insert-file-contents-literally dest-file)
                                    (buffer-hash))))

                  (should (string= dest-hash eol-hash))))

              (package-delete (cadr (assq 'simple-single package-alist)))
              (should-not (package-installed-p 'simple-single))
              (delete-file eol-file)
              (should-not (file-exists-p eol-file))
              )))))))

(ert-deftest package-test-install-dependency ()
  "Install a package which includes a dependency."
  (with-package-test ()
    (package-initialize)
    (package-refresh-contents)
    (package-install 'simple-depend)
    (should (package-installed-p 'simple-single))
    (should (package-installed-p 'simple-depend))))

(declare-function macro-problem-func "macro-problem" ())
(declare-function macro-problem-10-and-90 "macro-problem" ())
(declare-function macro-builtin-func "macro-builtin" ())
(declare-function macro-builtin-10-and-90 "macro-builtin" ())

(ert-deftest package-test-macro-compilation ()
  "\"Activation has to be done before compilation, so that if we're
   upgrading and macros have changed we load the new definitions
   before compiling.\" -- package.el"
  (with-package-test (:basedir (ert-resource-directory))
    (package-install-file (expand-file-name "macro-problem-package-1.0/"))
    (require 'macro-problem)
    ;; `macro-problem-func' uses a macro from `macro-aux'.
    (should (equal (macro-problem-func) '(progn a b)))
    (package-install-file (expand-file-name "macro-problem-package-2.0/"))
    ;; After upgrading, `macro-problem-func' depends on a new version
    ;; of the macro from `macro-aux'.
    (should (equal (macro-problem-func) '(1 b)))
    ;; `macro-problem-10-and-90' depends on an entirely new macro from `macro-aux'.
    (should (equal (macro-problem-10-and-90) '(10 90)))))

(ert-deftest package-test-macro-compilation-gz ()
  "Built-in's can be superseded as well."
  (with-package-test (:basedir (ert-resource-directory))
    (let ((dir (expand-file-name "macro-builtin-package-1.0")))
      (unwind-protect
          (let ((load-path load-path))
            (add-to-list 'load-path (directory-file-name dir))
            (byte-recompile-directory dir 0 t)
            (mapc (lambda (f) (call-process "gzip" nil nil nil f))
                  (directory-files-recursively dir "\\`[^\\.].*\\.el\\'"))
            (require 'macro-builtin)
            (should (member (expand-file-name "macro-builtin-aux.elc" dir)
                            (mapcar #'car load-history)))
            ;; `macro-builtin-func' uses a macro from `macro-aux'.
            (should (equal (macro-builtin-func) '(progn a b)))
            (package-install-file (expand-file-name "macro-builtin-package-2.0/"))
            ;; After upgrading, `macro-builtin-func' depends on a new version
            ;; of the macro from `macro-builtin-aux'.
            (should (equal (macro-builtin-func) '(1 b)))
            ;; `macro-builtin-10-and-90' depends on an entirely new macro from `macro-aux'.
            (should (equal (macro-builtin-10-and-90) '(10 90))))
        (mapc #'delete-file
              (directory-files-recursively dir "\\`[^\\.].*\\.elc\\'"))
        (mapc (lambda (f) (call-process "gunzip" nil nil nil f))
              (directory-files-recursively dir "\\`[^\\.].*\\.el\\.gz\\'"))))))

(ert-deftest package-test-install-two-dependencies ()
  "Install a package which includes a dependency."
  (with-package-test ()
    (package-initialize)
    (package-refresh-contents)
    (package-install 'simple-two-depend)
    (should (package-installed-p 'simple-single))
    (should (package-installed-p 'simple-depend))
    (should (package-installed-p 'simple-two-depend))))

(ert-deftest package-test-refresh-contents ()
  "Parse an \"archive-contents\" file."
  (with-package-test ()
    (package-initialize)
    (package-refresh-contents)
    (should (eq 4 (length package-archive-contents)))))

(ert-deftest package-test-install-single-from-archive ()
  "Install a single package from a package archive."
  (with-package-test ()
    (package-initialize)
    (package-refresh-contents)
    (package-install 'simple-single)))

(ert-deftest package-test-install-prioritized ()
  "Install a lower version from a higher-prioritized archive."
  (with-package-test ()
    (let* ((newer-version (ert-resource-file "newer-versions"))
           (package-archives `(("older" . ,package-test-data-dir)
                               ("newer" . ,newer-version)))
           (package-archive-priorities '(("older" . 100))))

      (package-initialize)
      (package-refresh-contents)
      (package-install 'simple-single)

      (let ((installed (cadr (assq 'simple-single package-alist))))
        (should (version-list-= '(1 3)
                                (package-desc-version installed)))))))

(ert-deftest package-test-install-multifile ()
  "Check properties of the installed multi-file package."
  (with-package-test (:basedir (ert-resource-directory) :install '(multi-file))
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


;;; Package Menu tests

(defmacro with-package-menu-test (&rest body)
  "Set up Package Menu (\"*Packages*\") buffer for testing."
  (declare (indent 0) (debug (([&rest form]) body)))
  `(with-package-test ()
     (let ((buf (package-list-packages)))
       (unwind-protect
           (progn ,@body)
         (kill-buffer buf)))))

(ert-deftest package-test-update-listing ()
  "Ensure installed package status is updated."
  (with-package-menu-test
    (search-forward-regexp "^ +simple-single")
    (package-menu-mark-install)
    (package-menu-execute)
    (run-hooks 'post-command-hook)
    (should (package-installed-p 'simple-single))
    (switch-to-buffer "*Packages*")
    (goto-char (point-min))
    (should (re-search-forward "^\\s-+simple-single\\s-+1.3\\s-+installed" nil t))
    (goto-char (point-min))
    (should-not (re-search-forward "^\\s-+simple-single\\s-+1.3\\s-+\\(available\\|new\\)" nil t))))

(ert-deftest package-test-list-filter-by-archive ()
  "Ensure package list is filtered correctly by archive version."
  (with-package-menu-test
    ;; TODO: Add another package archive to test filtering, because
    ;;       the testing environment currently only has one.
    (package-menu-filter-by-archive "gnu")
    (goto-char (point-min))
    (should (looking-at "^\\s-+multi-file"))
    (should (= (count-lines (point-min) (point-max)) 4))
    (should-error (package-menu-filter-by-archive "non-existent archive"))))

(ert-deftest package-test-list-filter-by-keyword ()
  "Ensure package list is filtered correctly by package keyword."
  (with-package-menu-test
    (package-menu-filter-by-keyword "frobnicate")
    (goto-char (point-min))
    (should (re-search-forward "^\\s-+simple-single" nil t))
    (should (= (count-lines (point-min) (point-max)) 1))
    (should-error (package-menu-filter-by-keyword "non-existent-keyword"))))

(ert-deftest package-test-list-filter-by-name ()
  "Ensure package list is filtered correctly by package name."
  (with-package-menu-test ()
    (package-menu-filter-by-name "ansi-color")
    (goto-char (point-min))
    (should (re-search-forward "^\\s-+ansi-color" nil t))
    (should (= (count-lines (point-min) (point-max)) 1))))

(ert-deftest package-test-list-filter-by-status ()
  "Ensure package list is filtered correctly by package status."
  (with-package-menu-test
    (package-menu-filter-by-status "available")
    (goto-char (point-min))
    (should (re-search-forward "^\\s-+multi-file" nil t))
    (should (= (count-lines (point-min) (point-max)) 4))
    ;; No installed packages in default environment.
    (should-error (package-menu-filter-by-status "installed"))))

(ert-deftest package-test-list-filter-marked ()
  "Ensure package list is filtered correctly by non-empty mark."
  (with-package-test ()
    (package-list-packages)
    (revert-buffer)
    (search-forward-regexp "^ +simple-single")
    (package-menu-mark-install)
    (package-menu-filter-marked)
    (goto-char (point-min))
    (should (re-search-forward "^I +simple-single" nil t))
    (should (= (count-lines (point-min) (point-max)) 1))
    (package-menu-mark-unmark)
    ;; No marked packages in default environment.
    (should-error (package-menu-filter-marked))))

(ert-deftest package-test-list-filter-by-version ()
  (with-package-menu-test
    (should-error (package-menu-filter-by-version "1.1" 'unknown-symbol)))  )

(defun package-test-filter-by-version (version predicate name)
  (with-package-menu-test
    (package-menu-filter-by-version version predicate)
    (goto-char (point-min))
    ;; We just check that the given package is included in the
    ;; listing.  One could be more ambitious.
    (should (re-search-forward name))))

(ert-deftest package-test-list-filter-by-version-= ()
  "Ensure package list is filtered correctly by package version (=)."
  (package-test-filter-by-version "1.1" '= "^\\s-+simple-two-depend"))

(ert-deftest package-test-list-filter-by-version-< ()
  "Ensure package list is filtered correctly by package version (<)."
  (package-test-filter-by-version "1.2" '< "^\\s-+simple-two-depend"))

(ert-deftest package-test-list-filter-by-version-> ()
  "Ensure package list is filtered correctly by package version (>)."
  (package-test-filter-by-version "1.0" '> "^\\s-+simple-two-depend"))

(ert-deftest package-test-list-clear-filter ()
  "Ensure package list filter is cleared correctly."
  (with-package-menu-test
    (let ((num-packages (count-lines (point-min) (point-max))))
      (package-menu-filter-by-name "ansi-color")
      (should (= (count-lines (point-min) (point-max)) 1))
      (package-menu-clear-filter)
      (should (= (count-lines (point-min) (point-max)) num-packages)))))

(ert-deftest package-test-update-archives ()
  "Test updating package archives."
  (with-package-test ()
    (let ((_buf (package-list-packages)))
      (revert-buffer)
      (search-forward-regexp "^ +simple-single")
      (package-menu-mark-install)
      (package-menu-execute)
      (should (package-installed-p 'simple-single))
      (let ((package-test-data-dir (ert-resource-file "newer-versions")))
        (setq package-archives `(("gnu" . ,package-test-data-dir)))
        (revert-buffer)

        ;; New version should be available and old version should be installed
        (goto-char (point-min))
        (should (re-search-forward "^\\s-+simple-single\\s-+1.4\\s-+available" nil t))
        (should (re-search-forward "^\\s-+simple-single\\s-+1.3\\s-+installed" nil t))

        (goto-char (point-min))
        (should (re-search-forward "^\\s-+new-pkg\\s-+1.0\\s-+\\(available\\|new\\)" nil t))

        (package-menu-mark-upgrades)
        (package-menu-execute)
        (revert-buffer)
        (should (package-installed-p 'simple-single '(1 4)))))))

(ert-deftest package-test-update-archives-async ()
  "Test updating package archives asynchronously."
  :tags '(:expensive-test)
  (skip-unless (executable-find "python2"))
  (let* ((package-menu-async t)
         (default-directory package-test-data-dir)
         (process (start-process
                   "package-server" "package-server-buffer"
                   (executable-find "python2")
                   "package-test-server.py"))
         (addr nil))
    (unwind-protect
        (progn
          (with-current-buffer "package-server-buffer"
            (should
             (with-timeout (10 nil)
               (while (not addr)
                 (accept-process-output nil 1)
                 (goto-char (point-min))
                 (when (re-search-forward "Server started, \\(.*\\)\n" nil t)
                   (setq addr (match-string 1))))
               addr)))
          (with-package-test (:basedir (ert-resource-directory) :location addr)
            (list-packages)
            (should package--downloads-in-progress)
            (should mode-line-process)
            (should-not
             (with-timeout (10 'timeout)
               (while package--downloads-in-progress
                 (accept-process-output nil 1))
               nil))
            ;; If the server process died, there's some non-Emacs problem.
            ;; Eg maybe the port was already in use.
            (skip-unless (process-live-p process))
            (goto-char (point-min))
            (should
             (search-forward-regexp "^ +simple-single" nil t))))
      (if (process-live-p process) (kill-process process)))))

(ert-deftest package-test-update-archives/ignore-nil-entry ()
  "Ignore any packages that are nil.  Test for Bug#28502."
  (with-package-test ()
    (let* ((with-nil-entry (ert-resource-file "with-nil-entry"))
           (package-archives `(("with-nil-entry" . ,with-nil-entry))))
      (package-initialize)
      (package-refresh-contents)
      (should (equal (length package-archive-contents) 2)))))

(ert-deftest package-test-package-installed-p ()
  "Test package-installed-p before and after package initialization."
  (with-package-test ()
    ;; Verify that `package-installed-p' evaluates true for a built-in
    ;; package, in this case `project', before package initialization.
    (should (not package--initialized))
    (should (package-installed-p 'project nil))
    (should (not (package-installed-p 'imaginary-package nil)))

    ;; The results don't change after package initialization.
    (package-initialize)
    (should package--initialized)
    (should (package-installed-p 'project nil))
    (should (not (package-installed-p 'imaginary-package nil)))))

(ert-deftest package-test-describe-package ()
  "Test displaying help for a package."

  (require 'finder-inf)
  ;; Built-in
  (with-fake-help-buffer
   (describe-package '5x5)
   (goto-char (point-min))
   (should (search-forward "5x5 is built-in." nil t))
   ;; Don't assume the descriptions are in any particular order.
   (save-excursion (should (search-forward "Status: Built-in." nil t)))
   (save-excursion (should (search-forward "Summary: simple little puzzle game" nil t)))
   (should (search-forward "The aim of 5x5" nil t)))

  ;; Installed
  (with-package-test ()
    (package-initialize)
    (package-refresh-contents)
    (package-install 'simple-single)
    (with-fake-help-buffer
     (describe-package 'simple-single)
     (goto-char (point-min))
     (should (search-forward "Package simple-single is installed." nil t))
     (save-excursion (should (re-search-forward "Status: Installed in ['`‘]simple-single-1.3/['’] (unsigned)." nil t)))
     (save-excursion (should (search-forward "Version: 1.3" nil t)))
     (save-excursion (should (search-forward "Summary: A single-file package with no dependencies" nil t)))
     (save-excursion (should (search-forward "Website: http://doodles.au" nil t)))
     (save-excursion (should (re-search-forward "Keywords: \\[?frobnicate\\]?" nil t)))
     (save-excursion (should (search-forward "This package provides a minor mode to frobnicate"
                                             nil t)))
     )))

(ert-deftest package-test-describe-installed-multi-file-package ()
  "Test displaying of the readme for installed multi-file package."

  (with-package-test ()
    (package-initialize)
    (package-refresh-contents)
    (package-install 'multi-file)
    (with-fake-help-buffer
     (describe-package 'multi-file)
     (goto-char (point-min))
     (should (search-forward "Website: http://puddles.li" nil t))
     (should (search-forward "This is a bare-bones readme file for the multi-file"
                             nil t)))))

(ert-deftest package-test-describe-non-installed-package ()
  "Test displaying of the readme for non-installed package."

  (with-package-test ()
    (package-initialize)
    (package-refresh-contents)
    (with-fake-help-buffer
     (describe-package 'simple-single)
     (goto-char (point-min))
     (should (search-forward "Website: http://doodles.au" nil t))
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
     (should (search-forward "Website: http://puddles.li" nil t))
     (should (search-forward "This is a bare-bones readme file for the multi-file"
                             nil t)))))

(defvar epg-config--program-alist) ; Silence byte-compiler.
(ert-deftest package-test-signed ()
  "Test verifying package signature."
  (skip-unless (ert-with-temp-directory homedir
                 (let ((process-environment
                        (cons (concat "HOME=" homedir)
                              process-environment)))
                   (require 'epg-config)
                   (defvar epg-config--program-alist)
                   (epg-find-configuration
                    'OpenPGP nil
                    ;; By default we require gpg2 2.1+ due to some
                    ;; practical problems with pinentry.  But this
                    ;; test works fine with 2.0 as well.
                    (let ((prog-alist (copy-tree epg-config--program-alist)))
                      (setf (alist-get "gpg2"
                                       (alist-get 'OpenPGP prog-alist)
                                       nil nil #'equal)
                            "2.0")
                      prog-alist)))))
  (let* ((keyring (expand-file-name "key.pub" package-test-data-dir))
         (package-test-data-dir (ert-resource-file "signed")))
    (with-package-test ()
      (package-initialize)
      (package-import-keyring keyring)
      (package-refresh-contents)
      (let ((package-check-signature 'allow-unsigned))
        (should (progn (package-install 'signed-good) 'noerror))
        (should-error (package-install 'signed-bad)))
      (package-delete (car (alist-get 'signed-good package-alist)))
      (let ((package-check-signature t))
        (should (progn (package-install 'signed-good) 'noerror))
        (should-error (package-install 'signed-bad)))
      (package-delete (car (alist-get 'signed-good package-alist)))
      (let ((package-check-signature nil))
        (should (progn (package-install 'signed-good) 'noerror))
        (should (progn (package-install 'signed-bad) 'noerror)))
      ;; Check if the installed package status is updated.
      (let ((_buf (package-list-packages)))
	(revert-buffer)
	(should (re-search-forward
		 "^\\s-+signed-good\\s-+\\(\\S-+\\)\\s-+\\(\\S-+\\)\\s-"
		 nil t))
	(should (string-equal (match-string-no-properties 1) "1.0"))
	(should (string-equal (match-string-no-properties 2) "installed")))
      ;; Check if the package description is updated.
      (with-fake-help-buffer
       (describe-package 'signed-good)
       (goto-char (point-min))
       (should (re-search-forward "Package signed-good is \\(\\S-+\\)\\." nil t))
       (should (string-equal (match-string-no-properties 1) "installed"))
       (should (re-search-forward
		"Status: Installed in ['`‘]signed-good-1.0/['’]."
		nil t))))))



;;; Tests for package-x features.

(require 'package-x)

(defvar package-x-test--single-archive-entry-1-3
  (cons 'simple-single
        (package-make-ac-desc '(1 3) nil
                              "A single-file package with no dependencies"
                              'single
                              '((:authors ("J. R. Hacker" . "jrh@example.com"))
                                (:maintainer "J. R. Hacker" . "jrh@example.com")
                                (:url . "http://doodles.au"))))
  "Expected contents of the archive entry from the \"simple-single\" package.")

(defvar package-x-test--single-archive-entry-1-4
  (cons 'simple-single
        (package-make-ac-desc '(1 4) nil
                              "A single-file package with no dependencies"
                              'single
                              '((:authors ("J. R. Hacker" . "jrh@example.com"))
                                (:maintainer "J. R. Hacker" . "jrh@example.com"))))
  "Expected contents of the archive entry from the updated \"simple-single\" package.")

(ert-deftest package-x-test-upload-buffer ()
  "Test creating an \"archive-contents\" file"
  (with-package-test (:basedir (ert-resource-directory)
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
      (should (equal 1 (car archive-contents)))
      (should (equal 2 (length archive-contents)))
      (let ((pac (cadr archive-contents))
            (pac-sample package-x-test--single-archive-entry-1-3))
        (should (equal (pop pac) (pop pac-sample)))
        (dotimes (i 4)
          (should (equal (aref pac i) (aref pac-sample i))))
        ;; The `extras' field should contain at least the specified elements.
        (should (cl-every (lambda (sample-elem)
                            (member sample-elem (aref pac 4)))
                          (aref pac-sample 4)))))))

(ert-deftest package-x-test-upload-new-version ()
  "Test uploading a new version of a package"
  (with-package-test (:basedir (ert-resource-directory)
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
      (should (equal 1 (car archive-contents)))
      (should (equal 2 (length archive-contents)))
      (let ((pac (cadr archive-contents))
            (pac-sample package-x-test--single-archive-entry-1-4))
        (should (equal (pop pac) (pop pac-sample)))
        (dotimes (i 4)
          (should (equal (aref pac i) (aref pac-sample i))))
        ;; The `extras' field should contain at least the specified elements.
        (should (cl-every (lambda (sample-elem)
                            (member sample-elem (aref pac 4)))
                          (aref pac-sample 4)))))))

(ert-deftest package-test-get-deps ()
  "Test `package--get-deps' with complex structures."
  (let ((package-alist
         (mapcar (lambda (p) (list (package-desc-name p) p))
           (list simple-single-desc
                 simple-depend-desc
                 multi-file-desc
                 new-pkg-desc
                 simple-depend-desc-1
                 simple-depend-desc-2)))
        (pkg-cmp #'string-lessp))
    (should
     (equal (sort (package--get-deps '(simple-depend)) pkg-cmp)
            (sort (list 'simple-depend 'simple-single) pkg-cmp)))
    (should
     (equal (sort (package--get-deps '(simple-depend-2)) pkg-cmp)
            (sort (list 'simple-depend-2 'simple-depend-1 'multi-file
                        'simple-depend 'simple-single)
                  pkg-cmp)))))

(ert-deftest package-test-sort-by-dependence ()
  "Test `package--sort-by-dependence' with complex structures."
  (let ((package-alist
         (mapcar (lambda (p) (list (package-desc-name p) p))
           (list simple-single-desc
                 simple-depend-desc
                 multi-file-desc
                 new-pkg-desc
                 simple-depend-desc-1
                 simple-depend-desc-2)))
        (delete-list
         (list simple-single-desc
               simple-depend-desc
               multi-file-desc
               new-pkg-desc
               simple-depend-desc-1
               simple-depend-desc-2)))
    (should
     (equal (package--sort-by-dependence delete-list)

            (list simple-depend-desc-2 simple-depend-desc-1 new-pkg-desc
                  multi-file-desc simple-depend-desc simple-single-desc)))
    (should
     (equal (package--sort-by-dependence (reverse delete-list))
            (list new-pkg-desc simple-depend-desc-2 simple-depend-desc-1
                  multi-file-desc simple-depend-desc simple-single-desc)))))

(provide 'package-test)

;;; package-tests.el ends here
