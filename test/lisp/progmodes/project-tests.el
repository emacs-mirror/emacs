;;; project-tests.el --- tests for project.el -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Free Software Foundation, Inc.

;; Keywords:

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

;; Unit tests for progmodes/project.el.

;;; Code:

(require 'project)

(require 'cl-lib)
(require 'ert)
(require 'ert-x) ; ert-with-temp-directory
(require 'grep)
(require 'xref)

(declare-function vc-git--program-version "vc-git")

(ert-deftest project/quoted-directory ()
  "Check that `project-files' and `project-find-regexp' deal with
quoted directory names (Bug#47799)."
  (skip-unless (executable-find find-program))
  (skip-unless (executable-find "xargs"))
  (skip-unless (executable-find "grep"))
  (ert-with-temp-directory directory
    (let ((default-directory directory)
          (project-current-directory-override t)
          (project-find-functions nil)
          (project-list-file
           (expand-file-name "projects" directory))
          (project (cons 'transient (file-name-quote directory)))
          (file (expand-file-name "file" directory)))
      (add-hook 'project-find-functions (lambda (_dir) project))
      (should (eq (project-current) project))
      (write-region "contents" nil file nil nil nil 'excl)
      (should (equal (project-files project)
                     (list (file-name-quote file))))
      (let* ((references nil)
             (xref-search-program 'grep)
             (xref-show-xrefs-function
              (lambda (fetcher _display)
                (push (funcall fetcher) references))))
        (project-find-regexp "tent")
        (pcase references
          (`((,item))
           ;; FIXME: Shouldn't `xref-match-item' be a subclass of
           ;; `xref-item'?
           (should (cl-typep item '(or xref-item xref-match-item)))
           (should (file-equal-p
                    (xref-location-group (xref-item-location item))
                    file)))
          (otherwise
           (ert-fail (format-message "Unexpected references: %S"
                                     otherwise))))))))

(cl-defstruct project-tests--trivial root ignores)

(cl-defmethod project-root ((project project-tests--trivial))
  (project-tests--trivial-root project))

(cl-defmethod project-ignores ((project project-tests--trivial) _dir)
  (project-tests--trivial-ignores project))

(ert-deftest project-ignores ()
  "Check that `project-files' correctly ignores the files
returned by `project-ignores' if the root directory is a
directory name (Bug#48471)."
  (skip-unless (executable-find find-program))
  (ert-with-temp-directory dir
    (make-empty-file (expand-file-name "some-file" dir))
    (make-empty-file (expand-file-name "ignored-file" dir))
    (let* ((project (make-project-tests--trivial
                     :root (file-name-as-directory dir)
                     :ignores '("./ignored-file")))
           (files (project-files project))
           (relative-files
            (cl-loop for file in files
                     collect (file-relative-name file dir))))
      (should (equal relative-files '("some-file"))))))

(ert-deftest project-ignores-bug-50240 ()
  "Check that `project-files' does not ignore all files.
When `project-ignores' includes a name matching project dir."
  (skip-unless (executable-find find-program))
  (ert-with-temp-directory dir
    (make-empty-file (expand-file-name "some-file" dir))
    (let* ((project (make-project-tests--trivial
                     :root (file-name-as-directory dir)
                     :ignores (list (file-name-nondirectory
                                     (directory-file-name dir)))))
           (files (project-files project)))
      (should (equal files
                     (list
                      (expand-file-name "some-file" dir)))))))

(defvar project-tests--this-file (or (bound-and-true-p byte-compile-current-file)
                                     (and load-in-progress load-file-name)
                                     buffer-file-name))

(ert-deftest project-vc-recognizes-git ()
  "Check that Git repository is detected."
  (skip-unless (eq (vc-responsible-backend default-directory) 'Git))
  (let* ((vc-handled-backends '(Git))
         (dir (file-name-directory project-tests--this-file))
         (_ (vc-file-clearprops dir))
         (project-vc-extra-root-markers nil)
         (project (project-current nil dir)))
    (should-not (null project))
    (should (string-match-p
             "\\`test/lisp/progmodes/project-tests\\.elc?"
             (file-relative-name
              project-tests--this-file
              (project-root project))))))

(ert-deftest project-vc-extra-root-markers-supports-wildcards ()
  "Check that one can add wildcard entries."
  (skip-unless (eq (vc-responsible-backend default-directory) 'Git))
  (let* ((dir (file-name-directory project-tests--this-file))
         (_ (vc-file-clearprops dir))
         (project-vc-extra-root-markers '("files-x-tests.*"))
         (project (project-current nil dir)))
    (should-not (null project))
    (should (nth 1 project))
    (should (string-match-p "/test/lisp/\\'" (project-root project)))
    ;; bug#73801
    (should (equal
             project
             (project-current nil (project-root project))))))

(ert-deftest project-vc-supports-project-in-different-dir ()
  "Check that it picks up dir-locals settings from somewhere else."
  (skip-unless (eq (vc-responsible-backend default-directory) 'Git))
  (let* ((dir (ert-resource-directory))
         (_ (vc-file-clearprops dir))
         (project-vc-extra-root-markers '(".dir-locals.el"))
         (project (project-current nil dir)))
    (should-not (null project))
    (should (string-match-p "/test/lisp/progmodes/project-resources/\\'" (project-root project)))
    (should (member "etc" (project-ignores project dir)))
    (should (equal `(,@(when (version<= "2.13" (vc-git--program-version))
                         (list ".dir-locals.el"))
                     "foo")
                   (mapcar #'file-name-nondirectory (project-files project))))))

(ert-deftest project-vc-supports-files-in-subdirectory ()
  "Check that it lists only files from a repo's subdirectory."
  (skip-unless (eq (vc-responsible-backend default-directory) 'Git))
  (let* ((dir (ert-resource-directory))
         (_ (vc-file-clearprops dir))
         (project-vc-extra-root-markers '("project-tests.el"))
         (project (project-current nil dir)))
    (should-not (null project))
    (should (string-match-p "/test/lisp/progmodes/\\'" (project-root project)))
    (should (equal `(,@(when (version<= "2.13" (vc-git--program-version))
                         (list ".dir-locals.el"))
                     "foo")
                   (mapcar #'file-name-nondirectory
                           (project-files project
                                          (list dir)))))))

(ert-deftest project-vc-ignores-in-external-directory ()
  "Check that it applies project-vc-ignores when DIR is external to root."
  (skip-unless (eq (vc-responsible-backend default-directory) 'Git))
  (let* ((dir (ert-resource-directory))
         (_ (vc-file-clearprops dir))
         ;; Do not detect VC backend.
         (project-vc-backend-markers-alist nil)
         (project-vc-extra-root-markers '("configure.ac"))
         (project (project-current nil (expand-file-name "../autoconf-resources/" dir))))
    (should-not (null project))
    (should (string-match-p "/test/lisp/progmodes/autoconf-resources/\\'" (project-root project)))
    (should (equal `(,@(when (version<= "2.13" (vc-git--program-version))
                         (list ".dir-locals.el"))
                     "foo")
                   (mapcar #'file-name-nondirectory
                           (project-files project
                                          (list dir)))))))

(ert-deftest project-vc-nonexistent-directory-no-error ()
  "Check that is doesn't error out when the current dir does not exist."
  (skip-unless (eq (vc-responsible-backend default-directory) 'Git))
  (let* ((dir (expand-file-name "foo-456/bar/" (ert-resource-directory)))
         (_ (vc-file-clearprops dir))
         (project-vc-extra-root-markers '(".dir-locals.el"))
         (project (project-current nil dir)))
    (should-not (null project))
    (should (string-match-p "/test/lisp/progmodes/project-resources/\\'" (project-root project)))))

(ert-deftest project-find-regexp ()
  "Check the happy path."
  (skip-unless (executable-find find-program))
  (skip-unless (executable-find "xargs"))
  (skip-unless (executable-find "grep"))
  (let* ((directory (ert-resource-directory))
         (project-find-functions nil)
         (project-list-file (expand-file-name "emacs-projects" temporary-file-directory))
         (project (cons 'transient directory)))
    (add-hook 'project-find-functions (lambda (_dir) project))
    (should (eq (project-current) project))
    (let* ((matches nil)
           (xref-search-program 'grep)
           (xref-show-xrefs-function
            (lambda (fetcher _display)
              (setq matches (funcall fetcher)))))
      (project-find-regexp "etc")
      (should (equal (mapcar (lambda (item)
                               (file-name-base
                                (xref-location-group (xref-item-location item))))
                             matches)
                     '(".dir-locals" "etc")))
      (should (equal (sort (mapcar #'xref-item-summary matches) #'string<)
                     '("((nil . ((project-vc-ignores . (\"etc\")))))" "etc"))))))

(ert-deftest project-find-regexp-with-prefix ()
  "Check the happy path."
  (skip-unless (executable-find find-program))
  (skip-unless (executable-find "xargs"))
  (skip-unless (executable-find "grep"))
  (let* ((directory (ert-resource-directory))
         (project-find-functions nil)
         (project-list-file (expand-file-name "emacs-projects" temporary-file-directory))
         (project (cons 'transient (expand-file-name "../elisp-mode-resources/" directory))))
    (add-hook 'project-find-functions (lambda (_dir) project))
    (should (eq (project-current) project))
    (let* ((matches nil)
           (xref-search-program 'grep)
           (xref-show-xrefs-function
            (lambda (fetcher _display)
              (setq matches (funcall fetcher))))
           (current-prefix-arg t))
      (cl-letf (((symbol-function 'read-directory-name)
                 (lambda (_prompt _default _dirname _mm) directory))
                ((symbol-function 'grep-read-files) (lambda (_re) "*")))
        (project-find-regexp "etc"))
      (should (equal (mapcar (lambda (item)
                               (file-name-base
                                (xref-location-group (xref-item-location item))))
                             matches)
                     '(".dir-locals" "etc")))
      (should (equal (sort (mapcar #'xref-item-summary matches) #'string<)
                     '("((nil . ((project-vc-ignores . (\"etc\")))))" "etc"))))))

;;; project-tests.el ends here
