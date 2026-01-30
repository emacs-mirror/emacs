;;; package-vc-tests.el --- Tests for package-vc -*- lexical-binding:t -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;; Author: Przemsyław Kryger <pkryger@gmail.com>
;; Maintainer: Philip Kaludercic <philipk@posteo.net>
;; Keywords: package

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

;; These tests focus on verifying post conditions for `package-vc'
;; operations on packages.  These tests install and load test packages
;; with a sample test implementation, resulting in modification of
;; numerous global variables, for example `load-history', `load-path',
;; `features', etc.  When run with `ert' it may contaminate current
;; Emacs session.  For this reason, tests execute their bodies in
;; `with-package-vc-tests-installed' (which see), that takes care of
;; cleaning up the environment.

;;; Code:

(require 'package-vc)
(require 'package)
(require 'vc-git)
(require 'vc)
(require 'cl-lib)
(require 'info)
(require 'ert-x)
(require 'ert)

;; Silence byte-compiler
(defvar message-auto-save-directory)

(defvar package-vc-tests-preserve-artifacts nil
  "When non-nil preserve temporary files and buffers produced by tests.
Each test produces a new temporary directory for each package under
test.  This leads to creation of [length of `package-vc-tests-packages']
times [number of tests executed] temporary directories for each tests
run.  Tests create temporary directories with `make-temp-file', which
see.

In addition some tests may produce temporary buffers, for example when
building a documentation.

When this variable is nil then delete temporary directories and kill
temporary buffers as soon as they are no longer needed.  When this
variable is a symbol, then preserve temporary directories and buffers
for the package that matches the symbol.  When this variable is a list
of symbols, then preserve temporary directories and buffers for each
package that matches a symbol in the list.  When this variable is t then
preserve all temporary directories.")

(defvar package-vc-tests-repos (make-hash-table))

(defvar package-vc-tests-dir)
(defvar package-vc-tests-packages)
(defvar package-vc-tests-repository)

(eval-and-compile
  (defun package-vc-tests-packages (&optional full)
    "Return a list of package definitions to test.
When variable `package-vc-tests-packages' is bound then return its
value.  If `package-vc-tests-dir' is bound or FULL is non nil then each
entry is in a form of (PKG CHECKOUT-DIR LISP-DIR INSTALL-FUN), where PKG
is a package name (a symbol), CHECKOUT-DIR either is nil when
`package-vc-tests-dir' is not bound or is an expected checkout
directory, LISP-DIR is a directory with package's sources (relative to
CHECKOUT-DIR), and INSTALL-FUN is a function that checkouts and install
the package.  Otherwise each entry is in a form of PKG."
    (if (boundp 'package-vc-tests-packages)
        package-vc-tests-packages
      (cl-macrolet ((test-package-def
                      (pkg checkout-dir-exp lisp-dir install-fun)
                      `(if (or (boundp 'package-vc-tests-dir) full)
                           (list
                            ',pkg
                            (expand-file-name (symbol-name ',pkg)
                                              ,checkout-dir-exp)
                            ,lisp-dir
                            #',install-fun)
                         ',pkg)))
        (let* ((tests-dir (bound-and-true-p package-vc-tests-dir))
               (user-dir (and tests-dir package-user-dir)))
          (list
           ;; checkout and install with `package-vc-install' (on ELPA)
           (test-package-def
            test-package-one user-dir nil
            package-vc-tests-install-from-elpa)
           ;; checkout and install with `package-vc-install' (not on
           ;; ELPA)
           (test-package-def
            test-package-two user-dir nil
            package-vc-tests-install-from-spec)
           ;; checkout with `package-vc-checktout' and install with
           ;; `package-vc-install-from-checkout' (on ELPA)
           (test-package-def
            test-package-three tests-dir nil
            package-vc-tests-checkout-from-elpa-install-from-checkout)
           ;; checkout with git and install with
           ;; `package-vc-install-from-checkout'
           (test-package-def
            test-package-four tests-dir nil
            package-vc-tests-checkout-with-git-install-from-checkout)
           ;; sources in "lisp" sub directory, checkout and install with
           ;; `package-vc-install' (not on ELPA)
           (test-package-def
            test-package-five user-dir "lisp"
            package-vc-tests-install-from-spec)
           ;; sources in "lisp" sub directory, checkout with git and
           ;; install with `package-vc-install-from-checkout'
           (test-package-def
            test-package-six tests-dir "lisp"
            package-vc-tests-checkout-with-git-install-from-checkout)
           ;; sources in "src" sub directory, checkout and install with
           ;; `package-vc-install' (on ELPA)
           (test-package-def
            test-package-seven user-dir "src"
            package-vc-tests-install-from-elpa)
           ;; sources in "src" sub directory, checkout with
           ;; `package-vc-checktout' and install with
           ;; `package-vc-install-from-checkout' (on ELPA)
           (test-package-def
            test-package-eight tests-dir nil
            package-vc-tests-checkout-from-elpa-install-from-checkout)
           ;; sources in "custom-dir" sub directory, checkout and
           ;; install with `package-vc-install' (on ELPA)
           (test-package-def
            test-package-nine user-dir "custom-dir"
            package-vc-tests-install-from-elpa)))))))

;; TODO: add test for deleting packages, with asserting
;; `package-vc-selected-packages'

;; TODO: clarify `package-vc-install-all' behavior with regards to
;; packages installed with `package-vc' but not stored in
;; `package-vc-selected-packages' i.e., packages from ELPAs

(defun package-vc-tests-add (suffix in-file &optional lisp-dir)
  "Create a new file from IN-FILE template updating SUFFIX in it.
When LISP-DIR is non-nil place the NAME file under LISP-DIR."
  (let* ((resource-dir (ert-resource-directory))
         (suffix (if (stringp suffix) suffix (format "%s" suffix)))
         (file (let ((file (replace-regexp-in-string
                            (rx (or "SUFFIX"
                                    (: "-v" digit (* "." (1+ digit)))
                                    (: ".in" string-end)) )
                            (lambda (mat)
                              (if (string= mat "SUFFIX") suffix ""))
                            in-file
                            :fixedcase)))
                 (file-name-concat lisp-dir file))))
    (unless (zerop (call-process
                    "sed" (expand-file-name in-file resource-dir)
                    `(:file ,file) nil
                    (format "s/SUFFIX/%s/g" suffix)))
      (error "Failed to invoke sed on %s" in-file))
    (vc-git-command nil 0 nil "add" ".")))

(defun package-vc-tests-create-repository (suffix repos-dir &optional lisp-dir)
  "Create a test package repository with SUFFIX in REPOS-DIR.
If LISP-DIR is non-nil place sources of the package in LISP-DIR."
  (let* ((name (format "test-package-%s" suffix))
         (repo-dir (expand-file-name name repos-dir)))
    (make-directory (expand-file-name (or lisp-dir ".") repo-dir) t)
    (let ((default-directory repo-dir)
          (process-environment
           (append (list
                    (format "EMAIL=%s@example.com" name)
                    (format "GIT_AUTHOR_NAME=%s" name)
                    (format "GIT_COMMITTER_NAME=%s" name))
                   process-environment)))
      (vc-git-command nil 0 nil "init")
      (vc-git-command nil 0 nil "checkout" "-b" "master")
      (package-vc-tests-add
       suffix "test-package-SUFFIX-lib-v0.1.el.in" lisp-dir)
      (package-vc-tests-add
       suffix "test-package-SUFFIX-v0.1.el.in" lisp-dir)
      (package-vc-tests-add
       suffix "test-package-SUFFIX.texi.in" lisp-dir)
      (package-vc-tests-add
       suffix "test-package-SUFFIX-inc.texi.in" lisp-dir)
      ;; Place Makefile in root of the repository
      (package-vc-tests-add
       suffix "Makefile.in" nil)
      (vc-git-command nil 0 nil "commit" "-m" "First commit")
      (package-vc-tests-add
       suffix "test-package-SUFFIX-lib-v0.2.el.in" lisp-dir)
      (package-vc-tests-add
       suffix "test-package-SUFFIX-v0.2.el.in" lisp-dir)
      (vc-git-command nil 0 nil "commit" "-m" "Second commit")
      (list repo-dir (vc-git-working-revision nil)))))

(defun package-vc-tests-package-desc (pkg &optional installed)
  "Return descriptor of PKG.
When INSTALLED is non-nil the descriptor comes from `package-alist'.
Otherwise the descriptor comes from `package-archive-contents'.  This
is to mimic `package-vc--read-package-desc'."
  (cadr (assq pkg (if installed package-alist package-archive-contents))))

(defun package-vc-tests-package-spec (pkg)
  "Return pkg-spec for PKG from `package-vc-selected-packages'."
  (cdr (assoc pkg package-vc-selected-packages #'string=)))

(defun package-vc-tests-package-lisp-dir (pkg)
  "Return a Lisp directory of PKG."
  (and-let* ((checkout-dir (car (alist-get pkg package-vc-tests-packages))))
    (if-let* ((lisp-dir (cadr (alist-get pkg package-vc-tests-packages))))
        (expand-file-name lisp-dir checkout-dir)
      checkout-dir)))

(defun package-vc-tests-package-main-file (pkg)
  "Return a main file of PKG."
  (file-name-concat (package-vc-tests-package-lisp-dir pkg)
                    (format "%s.el" pkg)))

;; When `package-vc-upgrade', `package-vc-rebuild', or other a
;; package-vc function re-compiles a package's source the package also
;; reloaded [1] to ensure that the most recent version of compiled code
;; is available to Emacs.  Some tests add markers in `load-history'
;; before executing such functions.  And then follow up tests use these
;; markers to assert that expected package files are in correct places
;; in the `load-history'.
;;
;; [1] Only when a file has been previously loaded.

(defun package-vc-tests-load-history-marker (name)
  "Return a `load-history' marker with NAME."
  (file-truename
   (expand-file-name (symbol-name name) package-vc-tests-dir)))

(defun package-vc-tests-load-history-pattern (pkg type)
  "Return a regexp pattern for PKG's file of TYPE."
  (pcase type
    (:autoloads
     (rx (literal (file-truename
                   (file-name-concat
                    package-user-dir
                    (symbol-name pkg)
                    (format "%s-autoloads.el" pkg))))
         eos))
    (:main
     (rx (literal (file-truename
                   (package-vc-tests-package-main-file pkg)))
         eos))
    (:main-compiled
     (rx (literal (file-truename
                   (package-vc-tests-package-main-file pkg)))
         "c" eos))
    (:marker
     (regexp-quote (file-truename
                    (package-vc-tests-load-history-marker pkg))))))

(defun package-vc-tests-load-history-interesting-entries ()
  "Return interesting entries in `load-history'.
An entry in `load-history' is interesting when it starts with
`package-vc-tests-dir'."
  (let ((interesting-entry
         (rx bos (literal (file-truename package-vc-tests-dir)))))
    (mapcan
     (lambda (ent)
       (and (consp ent)
            (stringp (car ent))
            (let ((file-name (file-truename (car ent))))
              (and (string-match interesting-entry file-name)
                   (list file-name)))))
     load-history)))

(defun package-vc-tests-load-history-position (pkg type)
  "Return a PKG's file of TYPE position in `load-history'.
If TYPE is `:autoloads' return a position of a PKG autoloads file.
Otherwise, if TYPE is `:main' return a position of PKG main file (not
compiled).  Otherwise, if TYPE is `:main-compiled' return a position of
PKG compiled main file.  Otherwise, if TYPE is `:marker' return a
position of a marker PKG."
  (let ((pkg-file (package-vc-tests-load-history-pattern pkg type)))
    (cl-position-if
     (lambda (file) (string-match pkg-file file))
     (package-vc-tests-load-history-interesting-entries))))

(defun package-vc-tests-explain-load-history-position (pkg type)
  "Explain why `package-vc-tests-load-history' failed for PKG of TYPE."
  (let ((pattern
         (concat "..."
                 (substring
                  (package-vc-tests-load-history-pattern pkg type)
                  (length (regexp-quote
                           (file-truename package-vc-tests-dir))))))
        (reason
         (if-let* ((pos (package-vc-tests-load-history-position
                         pkg type)))
             `(found in load-history at pos ,pos)
           '(not found in load-history)))
        (entries
         (cl-loop
          with len = (length (file-truename package-vc-tests-dir))
          for hist in (package-vc-tests-load-history-interesting-entries)
          collect (concat "..." (substring hist len)))))
    (append (list 'pattern pattern) reason (list entries))))

(put #'package-vc-tests-load-history-position
     'ert-explainer
     #'package-vc-tests-explain-load-history-position)

(defun package-vc-tests-log-buffer-name (pkg type)
  "Return name for action TYPE log buffer for PKG .
See `package-vc--build-documentation' and `package-vc--make' for format
names."
  (format " *package-vc %s: %s*" type pkg))

(defun package-vc-tests-log-buffer-exists (pkg type)
  "Return non-nil when log buffer for action TYPE exists for PKG."
  (when-let* ((name (package-vc-tests-log-buffer-name pkg type)))
    (get-buffer name)))

(defun package-vc-tests-explain-log-buffer (pkg type)
  "Explain why `package-vc-tests-log-buffer-exists' failed for TYPE action for PKG."
  (if-let* ((name (package-vc-tests-log-buffer-name pkg type))
            (buffer (get-buffer name))
            (sep (make-string 80 ?-)))
      (progn
        (message "package-vc-tests: Contents of log-buffer %s\n%s\n%s\n%s"
                 name
                 sep
                 (with-current-buffer buffer
                   (buffer-string))
                 sep)
        `(log-buffer ,name exists))
    `(log-buffer ,name does not exist)))

(put #'package-vc-tests-log-buffer-exists
     'ert-explainer
     #'package-vc-tests-explain-log-buffer)

(defun package-vc-tests-elc-files (pkg)
  "Return elc files for PKG."
  (when-let* ((dir (package-vc-tests-package-lisp-dir pkg)))
    (directory-files dir nil (rx ".elc" string-end))))

(defun package-vc-tests-assert-elc (pkg)
  "Assert that PKG has correct .elc files in."
  (let* ((dir (package-vc-tests-package-lisp-dir pkg))
         (elc-files (should (package-vc-tests-elc-files pkg)))
         (autoloads-rx (rx (literal (format "%s-autoloads.elc" pkg))
                           string-end)))
    (should-not (cl-find-if (lambda (elc)
                              (string-match autoloads-rx elc))
                            elc-files))
    (dolist (elc-file elc-files)
      (delete-file (expand-file-name elc-file dir)))))

(defun package-vc-tests-assert-package-alist (pkg version)
  "Assert that PKG entry in `package-alist' have correct VERSION and dir."
  (let ((pkg-desc (should (cadr (assq pkg package-alist)))))
    (should (equal (file-name-as-directory
                    (expand-file-name (format "%s" pkg)
                                      package-user-dir))
                   (file-name-as-directory
                    (package-desc-dir pkg-desc))))
    (should (equal (list pkg version)
                   (list pkg (package-desc-version pkg-desc))))))

(defun package-vc-tests-reset-head^ (pkg)
  "Reset to HEAD^ checkout for PKG."
  (let ((default-directory (cadr (assoc pkg package-vc-tests-packages))))
    (vc-git-command nil 0 nil "reset" "--hard" "HEAD^")))

(defun package-vc-tests-package-head (pkg)
  "Return HEAD revisions of a PKG."
  (let ((default-directory (cadr (assoc pkg package-vc-tests-packages))))
    (vc-git-working-revision nil)))

(defun package-vc-tests-make-spec (pkg)
  "Return a pkg-spec for PKG."
  (let ((lisp-dir
         (cadr (alist-get pkg package-vc-tests-packages))))
    (append
     (list pkg
           :url (car package-vc-tests-repository)
           :doc (let ((doc-file (format "%s.texi" pkg)))
                  (if lisp-dir
                      (file-name-concat lisp-dir doc-file)
                    doc-file))
           :make (format "build-%s" pkg)
           :shell-command (format "touch %s.cmd-build" pkg))
     (and lisp-dir
          (not (member lisp-dir '("lisp" "src")))
          (list :lisp-dir lisp-dir)))))

(defun package-vc-tests-make-temp-dir (prefix)
  "Create temp directory with PREFIX."
  (expand-file-name
   (make-temp-file prefix t (format-time-string "-%Y%m%d.%H%M%S.%3N"))))

(defun package-vc-with-tests-environment (pkg function)
  "Call FUNCTION with no arguments within a test environment set up for PKG."
  ;; Create a test package sources repository, based on skeleton files
  ;; in directory package-vc-resources.  Before executing body make sure
  ;; that:
  ;;
  (let* ((package-vc-tests-dir
          (package-vc-tests-make-temp-dir "package-vc-tests-"))
         ;; - packages are installed into test directory
         (package-user-dir (expand-file-name "elpa"
                                             package-vc-tests-dir))
         ;; - keyring is saved in test directory
         (package-gnupghome-dir (expand-file-name "gnupg"
                                                  package-user-dir))
         ;; - `package' has been initialized, and there are no
         ;;   `package-archives' defined
         (package-archives (unless package--initialized
                             (let (package-archives)
                               (package-initialize)
                               (package-vc--archives-initialize))
                             nil))
         ;; - define test packages, their checkout locations, lisp
         ;;   directories, and install functions
         (package-vc-tests-packages (package-vc-tests-packages))
         ;; - create a test package bundle
         (package-vc-tests-repository
          (or
           (gethash pkg package-vc-tests-repos)
           (let* ((pkg-name (symbol-name pkg))
                  (suffix (and (string-match
                                (rx ?- (group (1+ (not ?-))) eos)
                                pkg-name)
                               (match-string 1 pkg-name)))
                  (repos-dir
                   (or (gethash 'repos-dir package-vc-tests-repos)
                       (puthash 'repos-dir
                                (package-vc-tests-make-temp-dir
                                 "package-vc-tests-repos-")
                                package-vc-tests-repos))))
             (puthash pkg
                      (package-vc-tests-create-repository
                       suffix
                       repos-dir
                       (cadr (alist-get pkg package-vc-tests-packages)))
                      package-vc-tests-repos))))
         ;; - find all packages that are present in a test ELPA
         (package-vc-tests-elpa-packages
          (cl-loop
           for (name _ _ fn) in package-vc-tests-packages
           when (memq
                 fn
                 '(package-vc-tests-install-from-elpa
                   package-vc-tests-checkout-from-elpa-install-from-checkout))
           collect name))
         ;; - make test packages recognizable by `package' and
         ;;   `package-vc' internals:
         (package-archive-contents
          (mapcar
           (lambda (pkg)
             (list pkg
                   (package-desc-create
                    :name pkg
                    :version '(0 2)
                    :reqs '((emacs (30.1)))
                    :kind 'tar
                    :archive "test-elpa"
                    :extras
                    (list
                     '(:maintainer
                       ("Test Maintainer"
                        . "test-maintainer@test-domain.org"))
                     (cons :url  (car package-vc-tests-repository))
                     (cons :commit (cadr package-vc-tests-repository))
                     (cons :revdesc (substring
                                     (cadr package-vc-tests-repository)
                                     0 12))))))
           package-vc-tests-elpa-packages))
         ;; Branch needs to be specified in a pkg-spec, as cloning from
         ;; a bundle won't checkout a default branch.
         (package-vc--archive-spec-alists
          (list
           (cons 'test-elpa
                 (mapcar #'package-vc-tests-make-spec
                         package-vc-tests-elpa-packages))))
         (package-vc--archive-data-alist
          '((test-elpa :version 1 :default-vc Git)))
         ;; - `vc-guess-backend-url' is recognizing bundles as `Git'
         ;;   repositories:
         (vc-clone-heuristic-alist
          `((,(rx "test-package-" (1+ digit) ".bundle" eos)
             . Git)
            ,@vc-clone-heuristic-alist))
         ;; - ensure that `package-alist' and
         ;;   `package-vc-selected-packages' are empty
         (package-alist '())
         (package-vc-selected-packages '())
         ;; - don't save any customization
         (user-init-file nil)
         (custom-file nil)
         ;; - don't register projects
         (package-vc-register-as-project nil)
         ;; - allow build commands
         (package-vc-allow-build-commands t))
    (funcall function)))

(defun package-vc-tests-preserve-pkg-artifacts-p (pkg)
  "Return non nil if files and buffers for PKG should be preserved."
  (or (memq package-vc-tests-preserve-artifacts `(t ,pkg))
      (and (listp package-vc-tests-preserve-artifacts)
           (memq pkg package-vc-tests-preserve-artifacts))))

(defun package-vc-tests-environment-tear-down (pkg)
  "Tear down test environment for PKG.
Unbind package defined symbols, and remove package defined features and
entries from `load-path',`load-history', and `Info-directory-list'.
Delete temporary directories and buffers produced by tests, except for
when PKG matches `package-vc-tests-preserve-artifacts'."
  (let ((pattern (rx string-start (literal package-vc-tests-dir))))
    (dolist (entry load-history)
      (when-let* ((file (car-safe entry))
                  ((stringp file))
                  ((string-match pattern file)))
        (dolist (elt (cdr entry))
          (pcase elt
            (`(defun . ,fun)
             (fmakunbound fun))
            (`(provide . ,feat)
             (setq features (cl-remove feat features)))
            ((and (pred symbolp)
                  (pred boundp))
             (makunbound elt))))))
    (setq load-path (cl-remove-if
                     (lambda (path)
                       (and (stringp path)
                            (string-match pattern path)))
                     load-path)
          load-history (cl-remove-if
                        (lambda (entry)
                          (and-let* ((path (car-safe entry))
                                     (_ (stringp path)))
                            (string-match pattern path)))
                        load-history)
          Info-directory-list (cl-remove-if
                               (lambda (dir)
                                 (and (stringp dir)
                                      (string-match pattern dir)))
                               Info-directory-list)))
  (let ((buffers
         (delq nil
               (mapcar (lambda (type)
                         (get-buffer
                          (package-vc-tests-log-buffer-name pkg
                                                            type)))
                       '(doc make)))))
    (if (package-vc-tests-preserve-pkg-artifacts-p pkg)
        (let ((buffers
               (if buffers
                   (format " and %s: %s"
                           (if (cdr buffers) "buffers" "buffer")
                           (mapconcat
                            (lambda (buffer)
                              (with-current-buffer buffer
                                (let* ((old-name (buffer-name))
                                       (new-name (make-temp-name
                                                  (string-trim old-name))))
                                  (rename-buffer new-name)
                                  (format "`%s' -> `%s'"
                                          old-name new-name))))
                            buffers
                            ", "))
                 ""))
              (repo-dir (car (gethash pkg package-vc-tests-repos))))
          (message
           "package-vc-tests: preserving temporary %s: %s%s%s"
           (if repo-dir "directories" "directory")
           package-vc-tests-dir
           (if repo-dir (format " and %s" repo-dir) "")
           buffers))
      (delete-directory package-vc-tests-dir t)
      (dolist (buffer buffers)
        (kill-buffer buffer)))))

;; Tests create a repository for a package only once per a tests run.
;; The repository location is cached in `package-vc-tests-repos'.  To
;; support development, clear the cache on start of each tests run, such
;; that the package repository contains files from the source code.
;; When tests run completes delete repositories accounting for
;; `package-vc-tests-preserve-artifacts', which see.

(defun package-vc-tests-add-ert-run-tests-listener (args)
  "Add `package-vc-tests' repositories cleanup to listener in ARGS."
  (if-let* ((listener (cadr args))
            ((functionp listener)))
      (cl-list*
       (car args)
       (lambda (event-type &rest event-args)
         (cl-case event-type
           (run-started
            (clrhash package-vc-tests-repos))
           (run-ended
            (when-let* ((repos-dir (gethash 'repos-dir
                                            package-vc-tests-repos))
                        ((file-directory-p repos-dir)))
              (if package-vc-tests-preserve-artifacts
                  (progn
                    (dolist (pkg (package-vc-tests-packages))
                      (unless
                          (package-vc-tests-preserve-pkg-artifacts-p pkg)
                        (when-let* ((repo-dir
                                     (car (gethash pkg package-vc-tests-repos)))
                                    ((file-directory-p repo-dir)))
                          (delete-directory repo-dir t))))
                    (when (directory-empty-p repos-dir)
                      (delete-directory repos-dir)))
                (delete-directory repos-dir t)))))
         (apply listener (cons event-type event-args)))
       (drop 2 args))
    args))

(advice-add #'ert-run-tests
            :filter-args #'package-vc-tests-add-ert-run-tests-listener)

(defun package-vc-tests-with-installed (pkg function)
  "Call FUNCTION with PKG installed in a test environment.
FUNCTION should have no arguments."
  (package-vc-with-tests-environment
   pkg (lambda ()
         (unwind-protect
             (progn
               (funcall (or (caddr (alist-get pkg package-vc-tests-packages))
                            (lambda (name)
                              (ert-fail
                               (format
                                "Cannot find %s in package-vc-tests-packages"
                                name))))
                        pkg)
               (funcall function))
           (package-vc-tests-environment-tear-down pkg)))))

(defun package-vc-tests-install-from-elpa (pkg)
  "Install PKG with `package-vc-install'."
  (push (list (package-vc-tests-load-history-marker 'install-begin))
        load-history)
  (should (eq t (package-vc-install pkg)))
  (push (list (package-vc-tests-load-history-marker 'install-end))
        load-history)
  (should-not (package-vc-tests-package-spec pkg)))

(defun package-vc-tests-install-from-spec (pkg)
  "Install PKG with `package-vc-install' (not on ELPA)."
  (push (list (package-vc-tests-load-history-marker 'install-begin))
        load-history)
  (should (eq t (package-vc-install (package-vc-tests-make-spec pkg))))
  (push (list (package-vc-tests-load-history-marker 'install-end))
        load-history)
  (should (equal (car package-vc-tests-repository)
                 (plist-get (package-vc-tests-package-spec pkg)
                            :url))))

(defun package-vc-tests-checkout-from-elpa-install-from-checkout (pkg)
  "Install PKG with `package-vc-install-from-checkout'.
Make checkout with `package-vc-checkout'."
  (let ((checkout-dir (car (alist-get pkg package-vc-tests-packages))))
    (let* ((uniquify-buffer-name-style nil)
           (buffer (package-vc-checkout (package-vc-tests-package-desc
                                        pkg)
                                        checkout-dir)))
      (should (bufferp buffer))
      (should (string-prefix-p (symbol-name pkg) (buffer-name buffer))))
    (push (list (package-vc-tests-load-history-marker 'install-begin))
          load-history)
    (should (eq t
                (with-suppressed-warnings ((obsolete package-vc-install-from-checkout))
                  (package-vc-install-from-checkout checkout-dir))))
    (push (list (package-vc-tests-load-history-marker 'install-end))
          load-history)
    (let ((extras (package-desc-extras (package-vc-tests-package-desc pkg t))))
      (should (equal checkout-dir (alist-get :vc-dir extras))))))

(defun package-vc-tests-checkout-with-git-install-from-checkout (pkg)
  "Install PKG with `package-vc-install-from-checkout'.
Make checkout with git(1)."
  (let ((checkout-dir (car (alist-get pkg package-vc-tests-packages))))
    (vc-git-clone  (car package-vc-tests-repository)
                   checkout-dir
                   "master")
    (push (list (package-vc-tests-load-history-marker 'install-begin))
          load-history)
    (should (eq t
                (with-suppressed-warnings ((obsolete package-vc-install-from-checkout))
                  (package-vc-install-from-checkout checkout-dir (symbol-name pkg)))))
    (push (list (package-vc-tests-load-history-marker 'install-end))
          load-history)
    (let ((extras (package-desc-extras (package-vc-tests-package-desc pkg t))))
      (should (equal checkout-dir (alist-get :vc-dir extras))))))

;; Some of VC commands used by package-vc execute VC operations
;; asynchronously.  When such an operation executes as a part of test
;; body, the test needs to wait for the operation to finish before
;; asserting post conditions.  The maximum wait time should be at least
;; a single order of magnitude higher than what the operation usually
;; takes.  This decreases probability of false positives (for example
;; when execution takes place on a busy machine). On the other hand the
;; value cannot be too large to ensure reasonable execution time in case
;; of a legitimate failure.

(defmacro package-vc-tests-package-vc-async-wait (seconds count flags &rest body)
  "Wait up to SECONDS for COUNT async vc commands with FLAGS called by BODY.
Return nil on timeout or the value of last form in BODY."
  (declare (indent 3))
  (let ((count-sym (make-symbol "count"))
        (post-vc-command-sym (make-symbol "post-vc-command")))
    `(let* ((,count-sym ,count)
            (,post-vc-command-sym
             (lambda (command _ command-flags)
               ;; A crude filter for vc commands
               (when (and (equal command vc-git-program)
                          (cl-every (lambda (flag)
                                      (member flag command-flags))
                                    ,flags))
                 (decf ,count-sym)))))
       (add-hook 'vc-post-command-functions ,post-vc-command-sym 100)
       (unwind-protect
           (with-timeout (,seconds nil)
             (prog1 (progn ,@body)
               (while (/= ,count-sym 0)
                 (accept-process-output nil 0.01))))
         (remove-hook 'vc-post-command-functions ,post-vc-command-sym)))))

(defmacro package-vc-test-deftest (name args &rest body)
  "For each package under test define a test with NAME.
Use function `package-vc-tests-packages' to obtain packages under test.
Execute BODY as a test body with a package under test installed.  Bind
car of ARGS (a symbol) to name of the package.  When plist cdr ARGS
contains key `:tags' use its value as tests tags."
  (declare (debug (&define [&name "test@" symbolp]
			   sexp
			   def-body))
           (indent 2))
  (when (length< args 1)
    (error "`package-vc' tests have to take at least one argument"))
  (unless (symbolp (car-safe args))
    (error "`package-vc' tests first argument has to be a symbol"))
  (let ((file (or (macroexp-file-name) buffer-file-name))
        (tests '()) (fn (gensym))
        (pkg-arg (car args))
        (skip-forms (take-while (lambda (form)
                                  (memq (car-safe form) '(skip-when
                                                          skip-unless)))
                                body))
        (tags (plist-get (cdr-safe args) :tags)))
    (setq body (nthcdr (length skip-forms) body))
    (dolist (pkg (package-vc-tests-packages))
      (let ((name (intern (format "package-vc-tests-%s/%s" name pkg))))
        (push
         `(ert-set-test ',name
                        (make-ert-test
                         :name ',name
                         :tags (cons 'package-vc ',tags)
                         :file-name ,file
                         :body
                         (lambda ()
                           (funcall ,fn ',pkg)
                           nil)))
         tests)))
    `(cl-macrolet ((skip-when (form) `(ert--skip-when ,form))
                   (skip-unless (form) `(ert--skip-unless ,form)))
       (let ((,fn (lambda (,pkg-arg)
                    ,@skip-forms
                    (package-vc-tests-with-installed ,pkg-arg
                                                     (lambda () ,@body)))))
         ,@tests))))

(package-vc-test-deftest install-post-conditions (pkg)
  (let ((install-begin
         (should (package-vc-tests-load-history-position
                  'install-begin :marker)))
        (install-end
         (should (package-vc-tests-load-history-position
                  'install-end :marker)))
        (autoloads-pos
         (should (package-vc-tests-load-history-position
                  pkg :autoloads))))
    (should (< install-end autoloads-pos install-begin))
    (should-not (package-vc-tests-load-history-position
                 pkg :main))
    (should-not (package-vc-tests-load-history-position
                 pkg :main-compiled)))
  (should (equal (package-vc--main-file
                  (package-vc-tests-package-desc pkg t))
                 (package-vc-tests-package-main-file pkg)))
  (should (equal (package-vc-commit
                  (package-vc-tests-package-desc pkg t))
                 (cadr package-vc-tests-repository)))
  (package-vc-tests-assert-elc pkg)
  (package-vc-tests-assert-package-alist pkg '(0 2)))

(package-vc-test-deftest require (pkg)
  (should (fboundp (intern (format "%s-func" pkg))))
  (should (autoloadp
           (symbol-function (intern (format "%s-func" pkg)))))
  (should (require pkg))
  (should (fboundp (intern (format "%s-func" pkg))))
  (should-not (autoloadp
               (symbol-function (intern (format "%s-func" pkg)))))
  (should-not (fboundp (intern (format "%s-old-func" pkg))))
  (should-not (package-vc-tests-load-history-position
               pkg :main))
  (let ((install-end
         (should (package-vc-tests-load-history-position
                  'install-end :marker)))
        (main-compiled-pos
         (should (package-vc-tests-load-history-position
                  pkg :main-compiled))))
    (should (< main-compiled-pos install-end))))

(package-vc-test-deftest upgrade (pkg :tags (:expensive-test))
  (let ((head (package-vc-tests-package-head pkg)))
    (package-vc-tests-reset-head^ pkg)
    (push (list (package-vc-tests-load-history-marker
                 'upgrade-begin))
          load-history)
    (should
     (package-vc-tests-package-vc-async-wait 5 1 '("pull")
       (package-vc-upgrade (package-vc-tests-package-desc pkg t))
       t))
    (push (list (package-vc-tests-load-history-marker
                 'upgrade-end))
          load-history)
    (should-not (package-vc-tests-load-history-position
                 pkg :main))
    (should-not (package-vc-tests-load-history-position
                 pkg :main-compiled))
    (let ((upgrade-begin
           (should (package-vc-tests-load-history-position
                    'upgrade-begin :marker)))
          (upgrade-end
           (should (package-vc-tests-load-history-position
                    'upgrade-end :marker)))
          (autoloads-pos
           (should (package-vc-tests-load-history-position
                    pkg :autoloads))))
      (should (< upgrade-end autoloads-pos upgrade-begin)))
    (let ((func (intern (format "%s-func" pkg))))
      (should (fboundp func))
      (should (autoloadp
               (symbol-function func)))
      (should (equal "New macro test"
                     (funcall func "test"))))
    (should-not (fboundp (intern (format "%s-old-func" pkg))))
    (should (equal head
                   (package-vc-tests-package-head pkg))))
  (package-vc-tests-assert-elc pkg)
  (package-vc-tests-assert-package-alist pkg '(0 2)))

(package-vc-test-deftest upgrade-after-require (pkg)
  (should (require pkg))
  (let ((head (package-vc-tests-package-head pkg)))
    (package-vc-tests-reset-head^ pkg)
    (push (list (package-vc-tests-load-history-marker
                 'upgrade-begin))
          load-history)
    (should
     (package-vc-tests-package-vc-async-wait 5 1 '("pull")
       (package-vc-upgrade (package-vc-tests-package-desc pkg t))
       t))
    (push (list (package-vc-tests-load-history-marker
                 'upgrade-end))
          load-history)
    (let ((upgrade-begin
           (should (package-vc-tests-load-history-position
                    'upgrade-begin :marker)))
          (upgrade-end
           (should (package-vc-tests-load-history-position
                    'upgrade-end :marker)))
          (autoloads-pos
           (should (package-vc-tests-load-history-position
                    pkg :autoloads)))
          (main-pos
           (should (package-vc-tests-load-history-position
                    pkg :main)))
          (main-compiled-pos
           (should (package-vc-tests-load-history-position
                    pkg :main-compiled))))
      (should (< upgrade-end autoloads-pos upgrade-begin))
      (should (< upgrade-end main-pos upgrade-begin))
      (should (< upgrade-end main-compiled-pos upgrade-begin)))
    (let ((func (intern (format "%s-func" pkg))))
      (should (fboundp func))
      (should-not (autoloadp
                   (symbol-function func)))
      (should (equal "New macro test"
                     (funcall func "test"))))
    (should-not (fboundp (intern (format "%s-old-func" pkg))))
    (should (equal head
                   (package-vc-tests-package-head pkg))))
  (package-vc-tests-assert-elc pkg)
  (package-vc-tests-assert-package-alist pkg '(0 2)))

(package-vc-test-deftest upgrade-all (pkg :tags (:expensive-test))
  (let ((head (package-vc-tests-package-head pkg)))
    (package-vc-tests-reset-head^ pkg)
    (push (list (package-vc-tests-load-history-marker
                 'upgrade-all-begin))
          load-history)
    (should
     (package-vc-tests-package-vc-async-wait 5 1 '("pull")
       (package-vc-upgrade-all)
       t))
    (push (list (package-vc-tests-load-history-marker
                 'upgrade-all-end))
          load-history)
    (should-not (package-vc-tests-load-history-position
                 pkg :main))
    (should-not (package-vc-tests-load-history-position
                 pkg :main-compiled))
    (let ((upgrade-begin
           (should (package-vc-tests-load-history-position
                    'upgrade-all-begin :marker)))
          (upgrade-end
           (should (package-vc-tests-load-history-position
                    'upgrade-all-end :marker)))
          (autoloads-pos
           (should (package-vc-tests-load-history-position
                    pkg :autoloads))))
      (should (< upgrade-end autoloads-pos upgrade-begin)))
    (let ((func (intern (format "%s-func" pkg))))
      (should (fboundp func))
      (should (autoloadp
               (symbol-function func)))
      (should (equal "New macro test"
                     (funcall func "test"))))
    (should-not (fboundp (intern (format "%s-old-func" pkg))))
    (should (equal head
                   (package-vc-tests-package-head pkg))))
  (package-vc-tests-assert-elc pkg)
  (package-vc-tests-assert-package-alist pkg '(0 2)))

(package-vc-test-deftest upgrade-all-after-require (pkg :tags (:expensive-test))
  (should (require pkg))
  (let ((head (package-vc-tests-package-head pkg)))
    (package-vc-tests-reset-head^ pkg)
    (push (list (package-vc-tests-load-history-marker
                 'upgrade-all-begin))
          load-history)
    (should
     (package-vc-tests-package-vc-async-wait 5 1 '("pull")
       (package-vc-upgrade-all)
       t))
    (push (list (package-vc-tests-load-history-marker
                 'upgrade-all-end))
          load-history)
    (let ((upgrade-begin
           (should (package-vc-tests-load-history-position
                    'upgrade-all-begin :marker)))
          (upgrade-end
           (should (package-vc-tests-load-history-position
                    'upgrade-all-end :marker)))
          (autoloads-pos
           (should (package-vc-tests-load-history-position
                    pkg :autoloads)))
          (main-pos
           (should (package-vc-tests-load-history-position
                    pkg :main)))
          (main-compiled-pos
           (should (package-vc-tests-load-history-position
                    pkg :main-compiled))))
      (should (< upgrade-end autoloads-pos upgrade-begin))
      (should (< upgrade-end main-pos upgrade-begin))
      (should (< upgrade-end main-compiled-pos upgrade-begin)))
    (let ((func (intern (format "%s-func" pkg))))
      (should (fboundp func))
      (should-not (autoloadp
                   (symbol-function func)))
      (should (equal "New macro test"
                     (funcall func "test"))))
    (should-not (fboundp (intern (format "%s-old-func" pkg))))
    (should (equal head
                   (package-vc-tests-package-head pkg))))
  (package-vc-tests-assert-elc pkg)
  (package-vc-tests-assert-package-alist pkg '(0 2)))

(package-vc-test-deftest rebuild (pkg :tags (:expensive-test))
  (package-vc-tests-reset-head^ pkg)
  (let ((head (package-vc-tests-package-head pkg)))
    (package-vc-rebuild
     (package-vc-tests-package-desc pkg t))
    (let ((old-func (intern (format "%s-old-func" pkg))))
      (should (fboundp old-func))
      (should (autoloadp
               (symbol-function old-func))))
    (let ((func (intern (format "%s-func" pkg))))
      (should (fboundp func))
      (should (autoloadp
               (symbol-function func)))
      (should (equal "Old macro test"
                     (funcall func "test"))))
    (should (equal head
                   (package-vc-tests-package-head pkg))))
  (package-vc-tests-assert-elc pkg)
  (package-vc-tests-assert-package-alist pkg '(0 1)))

(package-vc-test-deftest rebuild-after-require (pkg)
  (should (require pkg))
  (package-vc-tests-reset-head^ pkg)
  (let ((head (package-vc-tests-package-head pkg)))
    (package-vc-rebuild
     (package-vc-tests-package-desc pkg t))
    (let ((old-func (intern (format "%s-old-func" pkg))))
      (should (fboundp old-func))
      (should-not (autoloadp
                   (symbol-function old-func))))
    (let ((func (intern (format "%s-func" pkg))))
      (should (fboundp func))
      (should-not (autoloadp
                   (symbol-function func)))
      (should (equal "Old macro test"
                     (funcall func "test"))))
    (should (equal head
                   (package-vc-tests-package-head pkg))))
  (package-vc-tests-assert-elc pkg)
  (package-vc-tests-assert-package-alist pkg '(0 1)))

(package-vc-test-deftest prepare-patch (pkg :tags (:expensive-test))
  ;; Ensure `vc-prepare-patch' respects subject from function argument
  (let ((message-auto-save-directory package-vc-tests-dir)
        (vc-prepare-patches-separately nil))
    (package-vc-prepare-patch (package-vc-tests-package-desc pkg t)
                              "test-subject"
                              (cdr package-vc-tests-repository))
    (let ((message-buffer
           (should (get-buffer "*unsent mail to Test Maintainer*"))))
      (should (bufferp message-buffer))
      (switch-to-buffer message-buffer)
      (goto-char (point-min))
      (should
       (string-match
        (rx
         "To: Test Maintainer <test-maintainer@test-domain.org>")
        (buffer-substring (point) (pos-eol))))
      (forward-line)
      (should
       (string-match
        (rx "Subject: test-subject")
        (buffer-substring (point) (pos-eol))))
      (let ((kill-buffer-query-functions nil))
        (with-current-buffer message-buffer
          ;; we mark the buffer as unmodified so that `kill-buffer'
          ;; doesn't complain (interrupting automatic testsx)
          (set-buffer-modified-p nil))
        (kill-buffer message-buffer)))))

(package-vc-test-deftest log-incoming (pkg :tags (:expensive-test))
  (package-vc-tests-reset-head^ pkg)
  (should
   (package-vc-tests-package-vc-async-wait
       5 1 '("log" "--decorate")
     (package-vc-root-log-incoming (package-vc-tests-package-desc pkg t))
     t))
  (let ((incoming-buffer (get-buffer "*vc-incoming*"))
        (pattern (rx (literal
                      (substring
                       (cadr package-vc-tests-repository)
                       0 7))
                     (one-or-more anychar)
                     "Second commit"
                     line-end)))
    (should (bufferp incoming-buffer))
    (switch-to-buffer incoming-buffer)
    (goto-char (point-min))
    (should
     (string-match
      pattern
      (buffer-substring (point) (pos-eol))))
    (let (kill-buffer-query-functions)
      (kill-buffer incoming-buffer))))

(package-vc-test-deftest pkg-spec-make-shell-command (pkg)
  ;; Only `package-vc-install' runs make and shell command
  (skip-unless (memq (caddr (alist-get pkg (package-vc-tests-packages t)))
                     '(package-vc-tests-install-from-elpa
                       package-vc-tests-install-from-spec)))
  (let* ((desc (package-vc-tests-package-desc pkg t))
         (checkout-dir (package-vc--checkout-dir desc)))
    (should (file-exists-p
             (expand-file-name
              (format "%s.make-build" pkg)
              checkout-dir)))
    (should (file-exists-p
             (expand-file-name
              (format "%s.cmd-build" pkg)
              checkout-dir)))))

(package-vc-test-deftest pkg-spec-info-manual (pkg :tags (:expensive-test))
  ;; Only `package-vc-install' builds info manuals, but only when
  ;; executable install-info is available.
  (skip-unless (and (executable-find "install-info")
                    (memq (caddr (alist-get pkg (package-vc-tests-packages t)))
                          '(package-vc-tests-install-from-elpa
                            package-vc-tests-install-from-spec))))
  (should-not (package-vc-tests-log-buffer-exists 'doc pkg))
  (should (cl-member-if
           (lambda (dir)
             (and (stringp dir)
                  (string-prefix-p package-vc-tests-dir dir)))
           Info-directory-list))
  (let ((info-file
         (expand-file-name (format "%s.info" pkg)
                           (car (alist-get
                                 pkg package-vc-tests-packages)))))
    (should (file-exists-p info-file))
    (ert-with-test-buffer
        (:name (format "*package-vc-tests: %s.info*" pkg))
      (insert-file-contents info-file)
      (goto-char (point-min))
      (should (re-search-forward
               (format "First chapter for %s" pkg)))
      (should (re-search-forward
               (format "Second chapter for %s" pkg))))))

(provide 'package-vc-tests)
;;; package-vc-tests.el ends here
