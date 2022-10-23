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

;; - Allow for automatic updating
;;   * Detect merge conflicts
;;   * Check if there are upstream changes
;; - Allow finding revisions that bump the version tag
;;   * Allow for `package-vc-install' to use the version
;;     of the package if already installed.
;; - Allow for ELPA specifications to be respected without
;;   endangering the user with arbitrary code execution
;; - Allow maintaining patches that are ported back onto regular
;;   packages and maintained between versions.
;; - Allow locking the specific revisions of sourced packages
;;  (comparable to `package-selected-packages') so that specific
;;  revisions can be re-installed.

;;; Code:

(eval-when-compile (require 'rx))
(eval-when-compile (require 'inline))
(eval-when-compile (require 'map))
(require 'package)
(require 'lisp-mnt)
(require 'vc)
(require 'seq)
(require 'xdg)

(defgroup package-vc nil
  "Manage packages from VC checkouts."
  :group 'package
  :version "29.1")

(defcustom package-vc-heusitic-alist
  `((,(rx bos "http" (? "s") "://"
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
                 "/" (+ (or alnum "-" "." "_")))
              (: "git." (or "savannah" "sv") "." (? "non") "gnu.org/"
                 (or "r" "git") "/"
                 (+ (or alnum "-" "." "_")) (? "/")))
          (or (? "/") ".git") eos)
     . Git)
    (,(rx bos "http" (? "s") "://"
          (or (: "hg.sr.ht"
                 "/~" (+ (or alnum "-" "." "_"))
                 "/" (+ (or alnum "-" "." "_")))
              (: "hg." (or "savannah" "sv") "." (? "non") "gnu.org/hgweb/"
                 (+ (or alnum "-" "." "_")) (? "/")))
          eos)
     . Hg)
    (,(rx bos "http" (? "s") "://"
          (or (: "bzr." (or "savannah" "sv") "." (? "non") "gnu.org/r/"
                 (+ (or alnum "-" "." "_")) (? "/")))
          eos)
     . Bzr))
  "Heuristic mapping URL regular expressions to VC backends."
  :type `(alist :key-type (regexp :tag "Regular expression matching URLs")
                :value-type (choice :tag "VC Backend"
                                    ,@(mapcar (lambda (b) `(const ,b))
                                              vc-handled-backends)))
  :version "29.1")

(defcustom package-vc-repository-store
  (expand-file-name "emacs/vc-packages" (xdg-data-home))
  "Directory used by `package-vc-unpack' to store repositories."
  :type 'directory
  :version "29.1")

(defcustom package-vc-default-backend 'Git
  "Default VC backend used when cloning a package repository.
If no repository type was specified or could be guessed by
`package-vc-heusitic-alist', the VC backend denoted by this
symbol is used.  The value must be a member of
`vc-handled-backends' that implements the `clone' function."
  :type `(choice ,@(mapcar (lambda (b) (list 'const b))
                           vc-handled-backends))
  :version "29.1")

(defvar package-vc-archive-spec-alist nil
  "List of package specifications for each archive.
The list maps package names as string to plist.  Valid keys
include

        `:url' (string)

The URL of the repository used to fetch the package source.

        `:branch' (string)

If given, the branch to check out after cloning the directory.

        `:lisp-dir' (string)

The repository-relative directory to use for loading the Lisp
sources.  If not given, the value defaults to the root directory
of the repository.

        `:main-file' (string)

The main file of the project, relevant to gather package
metadata.  If not given, the assumed default is the package named
with \".el\" concatenated to the end.

All other values are ignored.")

(defun pacakge-vc-desc->spec (pkg-desc &optional name)
  "Retrieve the package specification for PKG-DESC.
The optional argument NAME can be used to override the default
name for PKG-DESC."
  (alist-get
   (or name (package-desc-name pkg-desc))
   (alist-get (intern (package-desc-archive pkg-desc))
              package-vc-archive-spec-alist)
   nil nil #'string=))

(define-inline package-vc-query-spec (pkg-desc prop)
  "Query the property PROP for the package specification for PKG-DESC.
If no package specification can be determined, the function will
return nil."
  (inline-letevals (pkg-desc prop)
    (inline-quote (plist-get (pacakge-vc-desc->spec ,pkg-desc) ,prop))))

(defun package-vc--read-archive-data (archive)
  "Update `package-vc-archive-spec-alist' with the contents of ARCHIVE.
This function is meant to be used as a hook for
`package--read-archive-hook'."
  (let* ((contents-file (expand-file-name
                         (format "archives/%s/elpa-packages.eld" archive)
                         package-user-dir)))
    (when (file-exists-p contents-file)
      (with-temp-buffer
        (let ((coding-system-for-read 'utf-8))
          (insert-file-contents contents-file))
        (setf (alist-get (intern archive) package-vc-archive-spec-alist)
              (read (current-buffer)))))))

(defun package-vc--download-and-read-archives (&optional async)
  "Download specifications of all `package-archives' and read them.
Populate `package-vc-archive-spec-alist' with the result.

If optional argument ASYNC is non-nil, perform the downloads
asynchronously."
  (dolist (archive package-archives)
    (condition-case-unless-debug nil
        (package--download-one-archive archive "elpa-packages.eld" async)
      (error (message "Failed to download `%s' archive." (car archive))))))

(add-hook 'package-read-archive-hook     #'package-vc--read-archive-data 20)
(add-hook 'package-refresh-contents-hook #'package-vc--download-and-read-archives 20)

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
  (let ((name (package-desc-name pkg-desc)))
    ;; Infer the subject if missing.
    (unless (package-desc-summary pkg-desc)
      (setf (package-desc-summary pkg-desc)
            (or (package-desc-summary pkg-desc)
                (and-let* ((pkg (cadr (assq name package-archive-contents))))
                  (package-desc-summary pkg))
                (and-let* ((pkg-spec (pacakge-vc-desc->spec pkg-desc))
                           (main-file (plist-get pkg-spec :main-file)))
                  (lm-summary main-file))
                (and-let* ((main-file (expand-file-name
                                       (format "%s.el" name)
                                       (package-desc-dir pkg-desc)))
                           ((file-exists-p main-file)))
                  (lm-summary main-file))
                package--default-summary)))
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

(declare-function org-export-to-file "ox" (backend file))

(defun package-vc-build-documentation (pkg-desc file)
  "Build documentation FILE for PKG-DESC."
  (let ((pkg-dir (package-desc-dir pkg-desc)))
    (when (string-match-p "\\.org\\'" file)
      (require 'ox)
      (require 'ox-texinfo)
      (with-temp-buffer
        (insert-file-contents file)
        (setq file (make-temp-file "ox-texinfo-"))
        (org-export-to-file 'texinfo file)))
    (call-process "install-info" nil nil nil
                  file pkg-dir)))

(defun package-vc-unpack-1 (pkg-desc pkg-dir)
  "Install PKG-DESC that is already located in PKG-DIR."
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
     (package-compute-transaction nil (delete-dups deps))))

  (let ((default-directory (file-name-as-directory pkg-dir))
        (name (package-desc-name pkg-desc))
        (pkg-file (expand-file-name (package--description-file pkg-dir) pkg-dir)))
    ;; Generate autoloads
    (package-generate-autoloads name pkg-dir)

    ;; Generate package file
    (package-vc-generate-description-file pkg-desc pkg-file)

    ;; Detect a manual
    (when-let ((pkg-spec (pacakge-vc-desc->spec pkg-desc))
               ((executable-find "install-info")))
      (dolist (doc-file (ensure-list (plist-get pkg-spec :doc)))
        (package-vc-build-documentation pkg-desc doc-file))))

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
   (cons (package-desc-name pkg-desc)
         package-selected-packages)))

(defun package-vc-unpack (pkg-desc pkg-spec &optional rev)
  "Install the package described by PKG-DESC.
PKG-SPEC is a package specification is a property list describing
how to fetch and build the package PKG-DESC.  See
`package-vc-archive-spec-alist' for details.  The optional argument
REV specifies a specific revision to checkout.  This overrides
the `:brach' attribute in PKG-SPEC."
  (let* ((name (package-desc-name pkg-desc))
         (dirname (package-desc-full-name pkg-desc))
         (pkg-dir (expand-file-name dirname package-user-dir)))
    (setf (package-desc-dir pkg-desc) pkg-dir)
    (when (file-exists-p pkg-dir)
      (if (yes-or-no-p "Overwrite previous checkout?")
          (package--delete-directory pkg-dir pkg-desc)
        (error "There already exists a checkout for %s" name)))
    (pcase-let* (((map :url :branch :lisp-dir) pkg-spec)
                 (repo-dir
                  (if (null lisp-dir)
                      pkg-dir
                    (unless (file-exists-p package-vc-repository-store)
                      (make-directory package-vc-repository-store t))
                    (file-name-concat
                     package-vc-repository-store
                     ;; FIXME: We aren't sure this directory
                     ;; will be unique, but we can try other
                     ;; names to avoid an unnecessary error.
                     (file-name-base url)))))

      ;; Clone the repository into `repo-dir' if necessary
      (unless (file-exists-p repo-dir)
        (make-directory (file-name-directory repo-dir) t)
        (let ((backend (or (and url (alist-get url package-vc-heusitic-alist
                                               nil nil #'string-match-p))
                           package-vc-default-backend)))
          (unless (vc-clone url backend repo-dir (or rev branch))
            (error "Failed to clone %s from %s" name url))))

      (unless (eq pkg-dir repo-dir)
        ;; Link from the right position in `repo-dir' to the package
        ;; directory in the ELPA store.
        (make-symbolic-link (file-name-concat repo-dir lisp-dir) pkg-dir)))
    (package-vc-unpack-1 pkg-desc pkg-dir)))

(defun package-vc-sourced-packages-list ()
  "Generate a list of packages with VC data."
  (seq-filter
   (lambda (pkg)
     (or (pacakge-vc-desc->spec (cadr pkg))
         ;; If we have no explicit VC data, we can try a kind of
         ;; heuristic and use the URL header, that might already be
         ;; pointing towards a repository, and use that as a backup
         (and-let* ((extras (package-desc-extras (cadr pkg)))
                    (url (alist-get :url extras))
                    (backend (alist-get url package-vc-heusitic-alist
                                        nil nil #'string-match-p))))))
   package-archive-contents))

(defun package-vc-update (pkg-desc)
  "Attempt to update the packager PKG-DESC."
  (let* ((default-directory (package-desc-dir pkg-desc))
         (ret (with-demoted-errors "Error during package update: %S"
                (vc-pull)))
         (buf (cond
               ((processp ret) (process-buffer ret))
               ((bufferp ret) ret))))
    (if buf
        (with-current-buffer buf
          (vc-run-delayed
            (package-vc-unpack-1 pkg-desc default-directory)))
      (package-vc-unpack-1 pkg-desc default-directory))))

;;;###autoload
(defun package-vc-install (name-or-url &optional name rev)
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
  (cond
   ((and-let* ((stringp name-or-url)
               (backend (alist-get name-or-url
                                   package-vc-heusitic-alist
                                   nil nil #'string-match-p)))
      (package-vc-unpack
       (package-desc-create
        :name (or name (intern (file-name-base name-or-url)))
        :kind 'vc)
       (list :vc-backend backend :url name-or-url)
       rev)))
   ((and-let* ((desc (assoc name-or-url package-archive-contents #'string=)))
      (package-vc-unpack
       (let ((copy (copy-package-desc (cadr desc))))
         (setf (package-desc-kind copy) 'vc)
         copy)
       (or (pacakge-vc-desc->spec (cadr desc))
           (user-error "Package has no VC data"))
       rev)))
   ((user-error "Unknown package to fetch: %s" name-or-url))))

(defun package-vc-link-directory (dir name)
  "Install the package NAME in DIR by linking it into the ELPA directory.
If invoked interactively with a prefix argument, the user will be
prompted for the package NAME.  Otherwise it will be inferred
from the base name of DIR."
  (interactive (let ((dir (read-directory-name "Directory: ")))
                 (list dir
                       (if current-prefix-arg
                           (read-string "Package name: ")
                         (file-name-base (directory-file-name dir))))))
  (unless (vc-responsible-backend dir)
    (user-error "Directory %S is not under version control" dir))
  (package--archives-initialize)
  (let* ((name (or name (file-name-base (directory-file-name dir))))
         (pkg-dir (expand-file-name name package-user-dir)))
    (make-symbolic-link dir pkg-dir)
    (package-vc-unpack-1 (package-desc-create
                          :name (intern name)
                          :kind 'vc)
                         pkg-dir)))

(defun package-vc-refresh (pkg-desc)
  "Refresh the installation for PKG-DESC."
  (interactive (package-vc-read-pkg "Refresh package: "))
  (package-vc-unpack-1 pkg-desc (package-desc-dir pkg-desc)))

(defun package-vc-read-pkg (prompt)
  "Query for a source package description with PROMPT."
  (cadr (assoc (completing-read
                prompt
                package-alist
                (lambda (pkg) (package-vc-p (cadr pkg)))
                t)
               package-alist
               #'string=)))

;;;###autoload
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
