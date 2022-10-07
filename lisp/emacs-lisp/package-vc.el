;;; package-vc.el --- Manage packages from VC checkouts     -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

;; Author: Philip Kaludercic <philipk@posteo.net>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; While packages managed by package.el use tarballs for distributing
;; the source code, this extension allows for packages to be fetched
;; and updated directly from a version control system.

;;; TODO:

;; - Allow for automatic updating                               TODO
;;   * Detect merge conflicts                                   TODO
;;   * Check if there are upstream changes                      TODO
;; - Allow finding revisions that bump the version tag          TODO
;;   * Allow for `package-vc-fetch' to use the version
;;     of the package if already installed.
;; - Allow for ELPA specifications to be respected without      TODO
;;   endangering the user with arbitrary code execution
;; - Allow sending patches to package maintainers               TODO
;;   * Add `vc-send-patch' to vc.el                             TODO

;;; Code:

(eval-when-compile (require 'rx))
(require 'package)
(require 'lisp-mnt)
(require 'vc)
(require 'seq)

(defgroup package-vc nil
  "Manage packages from VC checkouts."
  :group 'package
  :version "29.1")

(defcustom package-vc-probable-repository-regexp
  (rx bos "http" (? "s") "://"
      (or (: (? "www.") "github.com"
             "/" (+ (or alnum "-" "." "_"))
             "/" (+ (or alnum "-" "." "_")))
          (: "codeberg.org"
             "/" (+ (or alnum "-" "." "_"))
             "/" (+ (or alnum "-" "." "_")))
          (: (? "www.") "gitlab" (+ "." (+ alnum))
             "/" (+ (or alnum "-" "." "_"))
             "/" (+ (or alnum "-" "." "_")))
          (: "git.sr.ht"
             "/~" (+ (or alnum "-" "." "_"))
             "/" (+ (or alnum "-" "." "_"))))
      (or (? "/") ".git") eos)
  "Regular expression matching URLs that are repositories."
  :version "29.1"
  :type 'regex)

(defun package-vc-commit (pkg)
  "Extract the commit of a development package PKG."
  (cl-assert (package-vc-p pkg))
  ;; FIXME: vc should be extended to allow querying the commit of a
  ;; directory (as is possible when dealing with git repositores).
  ;; This should be a fallback option.
  (cl-loop with dir = (package-desc-dir pkg)
           for file in (directory-files dir t "\\.el\\'" t)
           when (vc-working-revision file) return it
           finally return "unknown"))

(defun package-vc-version (pkg)
  "Extract the commit of a development package PKG."
  (cl-assert (package-vc-p pkg))
  (cl-loop with dir = (package-desc-dir pkg) ;FIXME: dir is nil
           for file in (sort (directory-files dir t "\\.el\\'")
                             (lambda (s1 s2)
                               (< (length s1) (length s2))))
           when (with-temp-buffer
                  (insert-file-contents file)
                  (package-strip-rcs-id
                   (or (lm-header "package-version")
                       (lm-header "version"))))
           return it
           finally return "0"))

(defun package-vc-generate-description-file (pkg-desc pkg-file)
  "Generate a package description file for PKG-DESC.
The output is written out into PKG-FILE."
  (let* ((name (package-desc-name pkg-desc)))
    (let ((print-level nil)
          (print-quoted t)
          (print-length nil))
      (write-region
       (concat
        ";;; Generated package description from "
        (replace-regexp-in-string
         "-pkg\\.el\\'" ".el"
         (file-name-nondirectory pkg-file))
        "  -*- no-byte-compile: t -*-\n"
        (prin1-to-string
         (nconc
          (list 'define-package
                (symbol-name name)
                (cons 'vc (package-vc-version pkg-desc))
                (package-desc-summary pkg-desc)
                (let ((requires (package-desc-reqs pkg-desc)))
                  (list 'quote
                        ;; Turn version lists into string form.
                        (mapcar
                         (lambda (elt)
                           (list (car elt)
                                 (package-version-join (cadr elt))))
                         requires))))
          (package--alist-to-plist-args
           (package-desc-extras pkg-desc))))
        "\n")
       nil pkg-file nil 'silent))))

(defun package-vc-unpack (pkg-desc)
  "Install the package described by PKG-DESC."
  (let* ((name (package-desc-name pkg-desc))
         (dirname (package-desc-full-name pkg-desc))
         (pkg-dir (expand-file-name dirname package-user-dir)))
    (setf (package-desc-dir pkg-desc) pkg-dir)
    (when (file-exists-p pkg-dir)
      (if (yes-or-no-p "Overwrite previous checkout?")
          (package--delete-directory pkg-dir)
        (error "There already exists a checkout for %s" name)))
    (pcase-let* ((attr (package-desc-extras pkg-desc))
                 (`(,backend ,repo ,dir ,branch)
                  (or (alist-get :upstream attr)
                      (error "Source package has no repository"))))
      (make-directory (file-name-directory pkg-dir) t)
      (unless (setf (car (alist-get :upstream attr))
                    (vc-clone backend repo pkg-dir))
        (error "Failed to clone %s from %s" name repo))
      (when-let ((rev (or (alist-get :rev attr) branch)))
        (vc-retrieve-tag pkg-dir rev))
      (when dir (setq pkg-dir (file-name-concat pkg-dir dir)))

      ;; In case the package was installed directly from source, the
      ;; dependency list wasn't know beforehand, and they might have
      ;; to be installed explicitly.
      (let (deps)
        (dolist (file (directory-files pkg-dir t "\\.el\\'" t))
          (with-temp-buffer
            (insert-file-contents file)
            (when-let* ((require-lines (lm-header-multiline "package-requires")))
              (thread-last
                (mapconcat #'identity require-lines " ")
                package-read-from-string
                package--prepare-dependencies
                (nconc deps)
                (setq deps)))))
        (dolist (dep deps)
          (cl-callf version-to-list (cadr dep)))
        (package-download-transaction
         (package-compute-transaction nil (delete-dups deps)))))

    (let ((default-directory (file-name-as-directory pkg-dir))
          (name (package-desc-name pkg-desc))
          (pkg-file (expand-file-name (package--description-file pkg-dir) pkg-dir)))
      ;; Generate autoloads
      (package-generate-autoloads name pkg-dir)
      (vc-ignore (concat "/" (file-relative-name
                              (expand-file-name (format "%s-autoloads.el" name))
                              default-directory)))

      ;; Generate package file
      (package-vc-generate-description-file pkg-desc pkg-file)
      (vc-ignore (concat "/" (file-relative-name pkg-file default-directory)))

      ;; Detect a manual
      (when (executable-find "install-info")
        ;; Only proceed if we can find an unambiguous TeXinfo file
        (let ((texi-files (directory-files pkg-dir t "\\.texi\\'"))
              (dir-file (expand-file-name "dir" pkg-dir)))
          (when (length= texi-files 1)
            (call-process "install-info" nil nil nil
                          (concat "--dir=" dir-file)
                          (car texi-files)))
          (vc-ignore "/dir"))))

    ;; Update package-alist.
    (let ((new-desc (package-load-descriptor pkg-dir)))
      ;; Activation has to be done before compilation, so that if we're
      ;; upgrading and macros have changed we load the new definitions
      ;; before compiling.
      (when (package-activate-1 new-desc :reload :deps)
        ;; FIXME: Compilation should be done as a separate, optional, step.
        ;; E.g. for multi-package installs, we should first install all packages
        ;; and then compile them.
        (package--compile new-desc)
        (when package-native-compile
          (package--native-compile-async new-desc))
        ;; After compilation, load again any files loaded by
        ;; `activate-1', so that we use the byte-compiled definitions.
        (package--reload-previously-loaded new-desc)))

    ;; Mark package as selected
    (package--save-selected-packages
     (cons name package-selected-packages))))

(defun package-vc-sourced-packages-list ()
  "Generate a list of packages with VC data."
  (seq-filter
   (lambda (pkg)
     (let ((extras (package-desc-extras (cadr pkg))))
       (or (alist-get :vc extras)
           ;; If we have no explicit VC data, we can try a kind of
           ;; heuristic and use the URL header, that might already be
           ;; pointing towards a repository, and use that as a backup
           (and-let* ((url (alist-get :url extras))
                      ((string-match-p package-vc-probable-repository-regexp
                                       url)))
             ;; XXX: Currently `package-vc-probable-repository-regexp'
             ;; only contains Git repositories, so we can infer the
             ;; repository type.  This might work for now, but is not a
             ;; particularly resilient approach.
             (setf (alist-get :vc (package-desc-extras (cadr pkg)))
                   (list 'Git url))
             t))))
   package-archive-contents))

(defun package-vc-update (pkg-desc)
  "Attempt to update the packager PKG-DESC."
  (let ((default-directory (package-desc-dir pkg-desc)))
    (with-demoted-errors "Error during package update: %S"
      (vc-pull))))

;;;###autoload
(defun package-vc-fetch (name-or-url &optional name rev)
  "Fetch the source of NAME-OR-URL.
If NAME-OR-URL is a URL, then the package will be downloaded from
the repository indicated by the URL.  The function will try to
guess the name of the package using `file-name-base'.  This can
be overridden by manually passing the optional NAME.  Otherwise
NAME-OR-URL is taken to be a package name, and the package
metadata will be consulted for the URL.  An explicit revision can
be requested using REV."
  (interactive
   (progn
     ;; Initialize the package system to get the list of package
     ;; symbols for completion.
     (package--archives-initialize)
     (let* ((packages (package-vc-sourced-packages-list))
            (input (completing-read
                    "Fetch package source (name or URL): " packages))
            (name (file-name-base input)))
       (list input (intern (string-remove-prefix "emacs-" name))))))
  (package--archives-initialize)
  (package-vc-unpack
   (cond
    ((and (stringp name-or-url)
          (url-type (url-generic-parse-url name-or-url)))
     (package-desc-create
      :name (or name (intern (file-name-base name-or-url)))
      :kind 'vc
      :extras `((:upstream . ,(list nil name-or-url nil nil))
                (:rev . ,rev))))
    ((when-let* ((desc (cadr (assoc name-or-url package-archive-contents
                                    #'string=)))
                 (upstream (or (alist-get :vc (package-desc-extras desc))
                               (user-error "Package has no VC data"))))
       (package-desc-create
        :name (if (stringp name-or-url)
                  (intern name-or-url)
                name-or-url)
        :kind 'vc
        :extras `((:upstream . ,upstream)
                  (:rev . ,rev)))))
    ((user-error "Unknown package to fetch: %s" name-or-url)))))

;;;###autoload
(defalias 'package-checkout #'package-vc-fetch)

(defun package-vc-read-pkg (prompt)
  "Query for a source package description with PROMPT."
  (completing-read
   prompt
   package-alist
   (lambda (pkg) (package-vc-p (cadr pkg)))
   t))

(defun package-vc-prepare-patch (pkg subject revisions)
  "Send a patch to the maintainer of a package PKG.
SUBJECT and REVISIONS are used passed on to `vc-prepare-patch'.
PKG must be a package description."
  (interactive
   (list (package-vc-read-pkg "Package to prepare a patch for: ")
         (and (not vc-prepare-patches-separately)
              (read-string "Subject: " "[PATCH] " nil nil t))
         (or (log-view-get-marked)
             (vc-read-multiple-revisions "Revisions: "))))
  (vc-prepare-patch (package-maintainers pkg t)
                    subject revisions))

(provide 'package-vc)
;;; package-vc.el ends here
