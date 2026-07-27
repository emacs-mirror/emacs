;;; vc-test-helper.el --- VC test suite helpers  -*- lexical-binding:t -*-

;; Copyright (C) 2014-2026 Free Software Foundation, Inc.

;; Author: Michael Albinus <michael.albinus@gmx.de>
;;         Sean Whitton <spwhitton@spwhitton.name>

;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(defun vc-test--create-repo-function (backend)
  "Run the `vc-create-repo' backend function.
For backends which don't support it, it is emulated."
  (cond
   ((eq backend 'CVS)
    (let ((tmp-dir
	   (expand-file-name
	    (make-temp-name "vc-test") temporary-file-directory)))
      (make-directory (expand-file-name "module" tmp-dir) 'parents)
      (make-directory (expand-file-name "CVSROOT" tmp-dir) 'parents)
      (if (not (fboundp 'w32-application-type))
          (shell-command-to-string (format "cvs -Q -d:local:%s co module"
                                           tmp-dir))
        (let ((cvs-prog (executable-find "cvs"))
              (tdir tmp-dir))
          ;; If CVS executable is an MSYS program, reformat the file
          ;; name of TMP-DIR to have the /d/foo/bar form supported by
          ;; MSYS programs.  (FIXME: What about Cygwin cvs.exe?)
          (if (eq (w32-application-type cvs-prog) 'msys)
              (setq tdir
                    (concat "/" (substring tmp-dir 0 1) (substring tmp-dir 2))))
          (shell-command-to-string (format "cvs -Q -d:local:%s co module"
                                           tdir))))
      (rename-file "module/CVS" default-directory)
      (delete-directory "module" 'recursive)
      ;; We must cleanup the "remote" CVS repo as well.
      (add-hook 'vc-test--cleanup-hook
		(lambda () (delete-directory tmp-dir 'recursive)))))

   ((eq backend 'Arch)
    (let ((archive-name (format "%s--%s" user-mail-address (random))))
      (when (string-match
	     "no arch user id set" (shell-command-to-string "tla my-id"))
	(shell-command-to-string
	 (format "tla my-id \"<%s>\"" user-mail-address)))
      (shell-command-to-string
       (format "tla make-archive %s %s" archive-name default-directory))
      (shell-command-to-string
       (format "tla my-default-archive %s" archive-name))))

   ((eq backend 'Mtn)
    (let ((archive-name "foo.mtn"))
      (shell-command-to-string
       (format
	"mtn db init --db=%s"
	(expand-file-name archive-name default-directory)))
      (shell-command-to-string
       (format "mtn --db=%s --branch=foo setup ." archive-name))))

   (t (vc-create-repo backend))))

(defvar vc-hg-global-switches)

(defmacro vc-test--with-author-identity (backend &rest body)
  (declare (indent 1) (debug t))
  `(let ((process-environment process-environment)
         (vc-hg-global-switches (bound-and-true-p vc-hg-global-switches)))
     ;; git tries various approaches to guess a user name and email,
     ;; which can fail depending on how the system is configured.
     ;; Eg if the user account has no GECOS, git commit can fail with
     ;; status 128 "fatal: empty ident name".
     (when (memq ,backend '(Bzr Git))
       (push "EMAIL=joh.doe@example.com" process-environment))
     (when (eq ,backend 'Git)
       (setq process-environment (append '("GIT_AUTHOR_NAME=A"
                                           "GIT_COMMITTER_NAME=C")
                                         process-environment)))

     ;; Mercurial fails to autodetect an identity on MS-Windows.
     (when (eq ,backend 'Hg)
       (push "--config=ui.username=john@doe.ee" vc-hg-global-switches))
     ,@body))

(provide 'vc-tests-helpers)
