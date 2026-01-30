;;; vc-tests.el --- Tests of different backends of vc.el  -*- lexical-binding:t -*-

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

;; For every supported VC on the machine, different test cases are
;; generated automatically.

;; Functions to be tested (see Commentary of vc.el).  Mandatory
;; functions are marked with `*', optional functions are marked with `-':

;; BACKEND PROPERTIES
;;
;; * revision-granularity                                       DONE

;; STATE-QUERYING FUNCTIONS
;;
;; * registered (file)                                          DONE
;; * state (file)                                               DONE
;; - dir-status (dir update-function)
;; - dir-status-files (dir files default-state update-function)
;; - dir-extra-headers (dir)
;; - dir-printer (fileinfo)
;; - status-fileinfo-extra (file)
;; * working-revision (file)                                    DONE
;; - latest-on-branch-p (file)
;; * checkout-model (files)                                     DONE
;; - mode-line-string (file)
;; - other-working-trees ()                                     DONE

;; STATE-CHANGING FUNCTIONS
;;
;; * create-repo (backend)                                      DONE
;; * register (files &optional comment)                         DONE
;; - responsible-p (file)
;; - receive-file (file rev)
;; - unregister (file)                                          DONE
;; * checkin (files comment)                                    DONE
;; * find-revision (file rev buffer)
;; * checkout (file &optional rev)
;; * revert (file &optional contents-done)
;; - rollback (files)
;; - merge-file (file rev1 rev2)
;; - merge-branch ()
;; - merge-news (file)
;; - pull (prompt)
;; - steal-lock (file &optional revision)
;; - modify-change-comment (files rev comment)
;; - mark-resolved (files)
;; - find-admin-dir (file)
;; - add-working-tree (directory)                               DONE
;; - delete-working-tree (directory)                            DONE
;; - move-working-tree (from to)                                DONE

;; HISTORY FUNCTIONS
;;
;; * print-log (files buffer &optional shortlog start-revision limit)
;; - log-outgoing (backend upstream-location)
;; - log-incoming (backend upstream-location)
;; - log-view-mode ()
;; - show-log-entry (revision)
;; - comment-history (file)
;; - update-changelog (files)
;; * diff (files &optional async rev1 rev2 buffer)              DONE
;; - revision-completion-table (files)
;; - annotate-command (file buf &optional rev)
;; - annotate-time ()
;; - annotate-current-time ()
;; - annotate-extract-revision-at-line ()
;; - region-history (FILE BUFFER LFROM LTO)
;; - region-history-mode ()

;; TAG SYSTEM
;;
;; - create-tag (dir name branchp)
;; - retrieve-tag (dir name update)

;; MISCELLANEOUS
;;
;; - make-version-backups-p (file)
;; - root (file)
;; - ignore (file &optional directory)
;; - ignore-completion-table
;; - previous-revision (file rev)
;; - next-revision (file rev)
;; - log-edit-mode ()
;; - check-headers ()
;; - delete-file (file)
;; - rename-file (old new)                                      DONE
;; - find-file-hook ()
;; - extra-menu ()
;; - extra-dir-menu ()
;; - conflicted-files (dir)

;;; Code:

(require 'ert)
(require 'ert-x)
(require 'vc)
(require 'log-edit)
(require 'project)
(require 'cl-lib)

(declare-function w32-application-type "w32proc.c")

;; The working horses.

(defvar vc-test--cleanup-hook nil
  "Functions for cleanup at the end of an ert test.
Don't set it globally, the functions should be let-bound.")

(defun vc-test--revision-granularity-function (backend)
  "Run the `revision-granularity' backend function."
  (vc-call-backend backend 'revision-granularity))

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

(defmacro vc--fix-home-for-bzr (tempdir)
  ;; See the comment in `vc-bzr-test-bug9726'.
  `(when (eq backend 'Bzr)
     (push (format "BZR_HOME=%s" ,tempdir) process-environment)
     (push (format "HOME=%s" ,tempdir) process-environment)))

(defun vc-test--create-repo (backend)
  "Create a test repository in `default-directory', a temporary directory."
  (ert-with-temp-directory tempdir
    (let ((vc-handled-backends `(,backend))
          (default-directory
           (file-name-as-directory
            (expand-file-name
             (make-temp-name "vc-test") temporary-file-directory)))
          (process-environment process-environment)
          vc-test--cleanup-hook)
      (vc--fix-home-for-bzr tempdir)
      (unwind-protect
          (progn
            ;; Cleanup.
            (add-hook
             'vc-test--cleanup-hook
             (let ((dir default-directory))
               (lambda () (delete-directory dir 'recursive))))

            ;; Check the revision granularity.
            (should (memq (vc-test--revision-granularity-function backend)
                          '(file repository)))

            ;; Create empty repository.
            (make-directory default-directory)
            (should (file-directory-p default-directory))
            (vc-test--create-repo-function backend)
            (should (eq (vc-responsible-backend default-directory) backend)))

        ;; Save exit.
        (ignore-errors
          (run-hooks 'vc-test--cleanup-hook))))))

;; FIXME: Why isn't there `vc-unregister'?
(defun vc-test--unregister-function (backend file)
  "Run the `vc-unregister' backend function.
For backends which don't support it, `vc-not-supported' is signaled."
  ;; CVS, SVN, SCCS, SRC and Mtn are not supported, and will signal
  ;; `vc-not-supported'.
  (prog1
      (vc-call-backend backend 'unregister file)
    (vc-file-clearprops file)))

(defmacro vc-test--run-maybe-unsupported-function (func &rest args)
  "Run FUNC with ARGS as arguments.
Catch the `vc-not-supported' error."
  `(condition-case err
       (funcall ,func ,@args)
     (vc-not-supported 'vc-not-supported)
     (t (signal (car err) (cdr err)))))

(defun vc-test--register (backend)
  "Register and unregister a file.
This checks also `vc-backend' and `vc-responsible-backend'."
  (ert-with-temp-directory tempdir
    (let ((vc-handled-backends `(,backend))
          (default-directory
           (file-name-as-directory
            (expand-file-name
             (make-temp-name "vc-test") temporary-file-directory)))
          (process-environment process-environment)
          vc-test--cleanup-hook)
      (vc--fix-home-for-bzr tempdir)
      (unwind-protect
          (progn
            ;; Cleanup.
            (add-hook
             'vc-test--cleanup-hook
             (let ((dir default-directory))
               (lambda () (delete-directory dir 'recursive))))

            ;; Create empty repository.
            (make-directory default-directory)
            (vc-test--create-repo-function backend)
            ;; For file oriented backends CVS, RCS and SVN the backend is
            ;; returned, and the directory is registered already.
            (should (if (vc-backend default-directory)
                        (vc-registered default-directory)
                      (not (vc-registered default-directory))))
            (should (eq (vc-responsible-backend default-directory) backend))

            (let ((tmp-name1 (expand-file-name "foo" default-directory))
                  (tmp-name2 "bla"))
              ;; Register files.  Check for it.
              (write-region "foo" nil tmp-name1 nil 'nomessage)
              (should (file-exists-p tmp-name1))
              (should-not (vc-backend tmp-name1))
              (should (eq (vc-responsible-backend tmp-name1) backend))
              (should-not (vc-registered tmp-name1))

              (write-region "bla" nil tmp-name2 nil 'nomessage)
              (should (file-exists-p tmp-name2))
              (should-not (vc-backend tmp-name2))
              (should (eq (vc-responsible-backend tmp-name2) backend))
              (should-not (vc-registered tmp-name2))

              (vc-register (list backend (list tmp-name1 tmp-name2)))
              (should (file-exists-p tmp-name1))
              (should (eq (vc-backend tmp-name1) backend))
              (should (eq (vc-responsible-backend tmp-name1) backend))
              (should (vc-registered tmp-name1))

              (should (file-exists-p tmp-name2))
              (should (eq (vc-backend tmp-name2) backend))
              (should (eq (vc-responsible-backend tmp-name2) backend))
              (should (vc-registered tmp-name2))

              ;; `vc-backend' accepts also a list of files,
              ;; `vc-responsible-backend' doesn't.
              (should (vc-backend (list tmp-name1 tmp-name2)))

              ;; Unregister the files.
              (unless (eq (vc-test--run-maybe-unsupported-function
                           'vc-test--unregister-function backend tmp-name1)
                          'vc-not-supported)
                (should-not (vc-backend tmp-name1))
                (should-not (vc-registered tmp-name1)))
              (unless (eq (vc-test--run-maybe-unsupported-function
                           'vc-test--unregister-function backend tmp-name2)
                          'vc-not-supported)
                (should-not (vc-backend tmp-name2))
                (should-not (vc-registered tmp-name2)))

              ;; The files should still exist.
              (should (file-exists-p tmp-name1))
              (should (file-exists-p tmp-name2))))

        ;; Save exit.
        (ignore-errors
          (run-hooks 'vc-test--cleanup-hook))))))

(defun vc-test--state (backend)
  "Check the different states of a file."
  (ert-with-temp-directory tempdir
    (let ((vc-handled-backends `(,backend))
          (default-directory
           (file-name-as-directory
            (expand-file-name
             (make-temp-name "vc-test") temporary-file-directory)))
          (process-environment process-environment)
          vc-test--cleanup-hook)
      (vc--fix-home-for-bzr tempdir)
      (unwind-protect
          (progn
            ;; Cleanup.
            (add-hook
             'vc-test--cleanup-hook
             (let ((dir default-directory))
               (lambda () (delete-directory dir 'recursive))))

            ;; Create empty repository.
            (make-directory default-directory)
            (vc-test--create-repo-function backend)

            (let ((tmp-name (expand-file-name "foo" default-directory)))
              ;; Check state of a nonexistent file.

              (message "vc-state2 %s" (vc-state tmp-name))
              (should (null (vc-state tmp-name)))

              ;; Write a new file.  Check state.
              (write-region "foo" nil tmp-name nil 'nomessage)

              ;; nil: Mtn
              ;; unregistered: Bzr CVS Git Hg SVN RCS
              (message "vc-state3 %s %s" backend (vc-state tmp-name backend))
              (should (memq (vc-state tmp-name backend) '(nil unregistered)))

              ;; Register a file.  Check state.
              (vc-register
               (list backend (list (file-name-nondirectory tmp-name))))

              ;; FIXME: nil is definitely wrong.
              ;; nil: SRC
              ;; added: Bzr CVS Git Hg Mtn SVN
              ;; up-to-date: RCS SCCS
              (message "vc-state4 %s" (vc-state tmp-name))
              (should (memq (vc-state tmp-name) '(nil added up-to-date)))

              ;; Unregister the file.  Check state.
              (if (eq (vc-test--run-maybe-unsupported-function
                       'vc-test--unregister-function backend tmp-name)
                      'vc-not-supported)
                  (message "vc-state5 unsupported")
                ;; unregistered: Bzr Git RCS Hg
                ;; unsupported: CVS SCCS SRC SVN
                (message "vc-state5 %s %s" backend (vc-state tmp-name backend))
                (should (memq (vc-state tmp-name backend)
                              '(nil unregistered))))))

        ;; Save exit.
        (ignore-errors
          (run-hooks 'vc-test--cleanup-hook))))))

(defun vc-test--working-revision (backend)
  "Check the working revision of a repository."
  (ert-with-temp-directory tempdir
    (let ((vc-handled-backends `(,backend))
          (default-directory
           (file-name-as-directory
            (expand-file-name
             (make-temp-name "vc-test") temporary-file-directory)))
          (process-environment process-environment)
          vc-test--cleanup-hook)
      (vc--fix-home-for-bzr tempdir)
      (unwind-protect
          (progn
            ;; Cleanup.
            (add-hook
             'vc-test--cleanup-hook
             (let ((dir default-directory))
               (lambda () (delete-directory dir 'recursive))))

            ;; Create empty repository.  Check working revision of
            ;; repository, should be nil.
            (make-directory default-directory)
            (vc-test--create-repo-function backend)

            ;; FIXME: Is the value for SVN correct?
            ;; nil: Bzr CVS Git Hg Mtn RCS SCCS SRC
            ;; "0": SVN
            (message
             "vc-working-revision1 %s" (vc-working-revision default-directory))
            (should (member (vc-working-revision default-directory) '(nil "0")))

            (let ((tmp-name (expand-file-name "foo" default-directory)))
              ;; Check initial working revision, should be nil until
              ;; it's registered.

              (message "vc-working-revision2 %s" (vc-working-revision tmp-name))
              (should-not (vc-working-revision tmp-name))

              ;; Write a new file.  Check working revision.
              (write-region "foo" nil tmp-name nil 'nomessage)

              (message "vc-working-revision3 %s" (vc-working-revision tmp-name))
              (should-not (vc-working-revision tmp-name))

              ;; Register a file.  Check working revision.
              (vc-register
               (list backend (list (file-name-nondirectory tmp-name))))

              ;; XXX: nil is fine, at least in Git's case, because
              ;; `vc-register' only makes the file `added' in this case.
              ;; nil: Git Mtn
              ;; "0": Bzr CVS Hg SRC SVN
              ;; "1.1": RCS SCCS
              ;; "-1": Hg versions before 5 (probably)
              (message "vc-working-revision4 %s" (vc-working-revision tmp-name))
              (should (member (vc-working-revision tmp-name) '(nil "0" "1.1" "-1")))

              ;; TODO: Call `vc-checkin', and check the resulting
              ;; working revision.  None of the return values should be
              ;; nil then.

              ;; Unregister the file.  Check working revision.
              (if (eq (vc-test--run-maybe-unsupported-function
                       'vc-test--unregister-function backend tmp-name)
                      'vc-not-supported)
                  (message "vc-working-revision5 unsupported")
                ;; nil: Bzr Git Hg RCS
                ;; unsupported: CVS Mtn SCCS SRC SVN
                (message "vc-working-revision5 %s" (vc-working-revision tmp-name))
                (should-not (vc-working-revision tmp-name)))))

        ;; Save exit.
        (ignore-errors
          (run-hooks 'vc-test--cleanup-hook))))))

(defun vc-test--checkout-model (backend)
  "Check the checkout model of a repository."
  (ert-with-temp-directory tempdir
    (let ((vc-handled-backends `(,backend))
          (default-directory
           (file-name-as-directory
            (expand-file-name
             (make-temp-name "vc-test") temporary-file-directory)))
          (process-environment process-environment)
          vc-test--cleanup-hook)
      (vc--fix-home-for-bzr tempdir)
      (unwind-protect
          (progn
            ;; Cleanup.
            (add-hook
             'vc-test--cleanup-hook
             (let ((dir default-directory))
               (lambda () (delete-directory dir 'recursive))))

            ;; Create empty repository.  Check repository checkout model.
            (make-directory default-directory)
            (vc-test--create-repo-function backend)

            ;; Surprisingly, none of the backends returns 'announce.
            ;; implicit: Bzr CVS Git Hg Mtn SRC SVN
            ;; locking: RCS SCCS
            (message
             "vc-checkout-model1 %s"
             (vc-checkout-model backend default-directory))
            (should (memq (vc-checkout-model backend default-directory)
                          '(announce implicit locking)))

            (let ((tmp-name (expand-file-name "foo" default-directory)))
              ;; Check checkout model of a nonexistent file.

              ;; implicit: Bzr CVS Git Hg Mtn SRC SVN
              ;; locking: RCS SCCS
              (message
               "vc-checkout-model2 %s" (vc-checkout-model backend tmp-name))
              (should (memq (vc-checkout-model backend tmp-name)
                            '(announce implicit locking)))

              ;; Write a new file.  Check checkout model.
              (write-region "foo" nil tmp-name nil 'nomessage)

              ;; implicit: Bzr CVS Git Hg Mtn SRC SVN
              ;; locking: RCS SCCS
              (message
               "vc-checkout-model3 %s" (vc-checkout-model backend tmp-name))
              (should (memq (vc-checkout-model backend tmp-name)
                            '(announce implicit locking)))

              ;; Register a file.  Check checkout model.
              (vc-register
               (list backend (list (file-name-nondirectory tmp-name))))

              ;; implicit: Bzr CVS Git Hg Mtn SRC SVN
              ;; locking: RCS SCCS
              (message
               "vc-checkout-model4 %s" (vc-checkout-model backend tmp-name))
              (should (memq (vc-checkout-model backend tmp-name)
                            '(announce implicit locking)))

              ;; Unregister the file.  Check checkout model.
              (if (eq (vc-test--run-maybe-unsupported-function
                       'vc-test--unregister-function backend tmp-name)
                      'vc-not-supported)
                  (message "vc-checkout-model5 unsupported")
                ;; implicit: Bzr Git Hg
                ;; locking: RCS
                ;; unsupported: CVS Mtn SCCS SRC SVN
                (message
                 "vc-checkout-model5 %s" (vc-checkout-model backend tmp-name))
                (should (memq (vc-checkout-model backend tmp-name)
                              '(announce implicit locking))))))

        ;; Save exit.
        (ignore-errors
          (run-hooks 'vc-test--cleanup-hook))))))

(defun vc-test--rename-file (backend)
  "Check the rename-file action."
  (ert-with-temp-directory tempdir
    (let ((vc-handled-backends `(,backend))
          (default-directory
           (file-name-as-directory
            (expand-file-name
             (make-temp-name "vc-test") temporary-file-directory)))
          (process-environment process-environment)
          vc-test--cleanup-hook)
      (vc--fix-home-for-bzr tempdir)
      (unwind-protect
          (progn
            ;; Cleanup.
            (add-hook
             'vc-test--cleanup-hook
             (let ((dir default-directory))
               (lambda () (delete-directory dir 'recursive))))

            ;; Create empty repository.
            (make-directory default-directory)
            (vc-test--create-repo-function backend)

            (let ((tmp-name (expand-file-name "foo" default-directory))
                  (new-name (expand-file-name "bar" default-directory)))
              ;; Write a new file.
              (write-region "foo" nil tmp-name nil 'nomessage)

              ;; Register it.  Renaming can fail otherwise.
              (vc-register
               (list backend (list (file-name-nondirectory tmp-name))))

              ;; Test that `vc-rename-file' isn't affected by
              ;; `default-directory' except for the meaning of OLD and
              ;; NEW if they are relative file names.
              (let ((tmp-name (file-relative-name tmp-name
                                                  temporary-file-directory))
                    (new-name (file-relative-name new-name
                                                  temporary-file-directory))
                    (default-directory temporary-file-directory))
                (vc-rename-file tmp-name new-name))

              (should (not (file-exists-p tmp-name)))
              (should (file-exists-p new-name))

              (should (equal (vc-state new-name)
                             (if (memq backend '(RCS SCCS))
                                 'up-to-date
                               'added)))))

        ;; Save exit.
        (ignore-errors
          (run-hooks 'vc-test--cleanup-hook))))))

(defvar vc-hg-global-switches)

(defmacro vc-test--with-author-identity (backend &rest body)
  (declare (indent 1) (debug t))
  `(let ((process-environment process-environment)
         (vc-hg-global-switches vc-hg-global-switches))
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

(declare-function log-edit-done "vc/log-edit")

(defun vc-test--version-diff (backend)
  "Check the diff version of a repository."
  (ert-with-temp-directory tempdir
    (let ((vc-handled-backends `(,backend))
          (default-directory
           (file-name-as-directory
            (file-truename
             (expand-file-name
              (make-temp-name "vc-test") temporary-file-directory))))
          vc-test--cleanup-hook)
      (vc--fix-home-for-bzr tempdir)
      (vc-test--with-author-identity backend

        (unwind-protect
            (progn
              ;; Cleanup.
              (add-hook
               'vc-test--cleanup-hook
               (let ((dir default-directory))
                 (lambda () (delete-directory dir 'recursive))))

              ;; Create empty repository.  Check repository checkout model.
              (make-directory default-directory)
              (vc-test--create-repo-function backend)

              (let* ((tmp-name (expand-file-name "foo" default-directory))
                     (files (list (file-name-nondirectory tmp-name))))
                ;; Write and register a new file.
                (write-region "originaltext" nil tmp-name nil 'nomessage)
                (vc-register (list backend files))

                (let ((buff (find-file tmp-name)))
                  (with-current-buffer buff
                    (progn
                      ;; Optionally checkout file.
                      (when (memq backend '(RCS CVS SCCS))
                        (vc-checkout tmp-name))

                      ;; Checkin file.
                      (vc-checkin files backend)
                      (insert "Testing vc-version-diff")
                      (let (vc-async-checkin)
                        (log-edit-done)))))

                ;; Modify file content.
                (when (memq backend '(RCS CVS SCCS))
                  (vc-checkout tmp-name))
                (write-region "updatedtext" nil tmp-name nil 'nomessage)

                ;; Check version diff.
                (vc-version-diff files nil nil)
                (if (eq backend 'Bzr)
                    (sleep-for 1))
                (should (bufferp (get-buffer "*vc-diff*")))

                (with-current-buffer "*vc-diff*"
                  (progn
                    (let ((rawtext (buffer-substring-no-properties (point-min)
                                                                   (point-max))))
                      (should (string-search "-originaltext" rawtext))
                      (should (string-search "+updatedtext" rawtext)))))))

          ;; Save exit.
          (ignore-errors
            (run-hooks 'vc-test--cleanup-hook)))))))

(declare-function vc-git--program-version "vc-git")

(defun vc-test--other-working-trees (backend)
  "Test other working trees actions."
  (ert-with-temp-directory _tempdir
    (let ((vc-handled-backends `(,backend))
          (default-directory
           (file-name-as-directory
            (expand-file-name
             (make-temp-name "vc-test") temporary-file-directory)))
          vc-test--cleanup-hook)
      (vc-test--with-author-identity backend
        (unwind-protect
            (progn
              ;; Cleanup.
              (add-hook
               'vc-test--cleanup-hook
               (let ((dir default-directory))
                 (lambda ()
                   (delete-directory dir 'recursive)
                   (dolist (name '("first" "second" "first"))
                     (project-forget-project
                      (expand-file-name name default-directory))))))

              (let* ((first (file-truename
                             (file-name-as-directory
                              (expand-file-name "first" default-directory))))
                     (second (file-truename
                              (file-name-as-directory
                               (expand-file-name "second" default-directory))))
                     (third (file-truename
                             (file-name-as-directory
                              (expand-file-name "third" default-directory))))
                     (tmp-name (expand-file-name "foo" first))
                     (project-list-file
                      (expand-file-name "projects.eld" default-directory)))

                ;; Set up the first working tree.
                (make-directory first t)
                (let ((default-directory first))
                  (vc-test--create-repo-function backend)
                  (write-region "foo" nil tmp-name nil 'nomessage)
                  (vc-register `(,backend (,(file-name-nondirectory tmp-name)))))
                (with-current-buffer (find-file-noselect tmp-name)
                  (vc-checkin (list (file-name-nondirectory tmp-name)) backend)
                  (insert "Testing other working trees")
                  (let (vc-async-checkin)
                    (log-edit-done))

                  ;; Set up the second working tree.
                  ;; For the backends which do additional prompting (as
                  ;; specified in the API for this backend function) we
                  ;; need to stub that out.
                  (cl-ecase backend
                    (Git (cl-letf (((symbol-function 'completing-read)
                                    (lambda (&rest _ignore) "")))
                           (vc-add-working-tree backend second)))
                    (Hg (vc-add-working-tree backend second))))

                ;; Test `known-other-working-trees'.
                (with-current-buffer (find-file-noselect tmp-name)
                  (should
                   (equal (list second)
                          (vc-call-backend backend 'known-other-working-trees)))
                  (let ((default-directory second))
                    (should
                     (equal (list first)
                            (vc-call-backend backend 'known-other-working-trees))))

                  ;; Test `move-working-tree'.
                  (vc-move-working-tree backend second third)
                  (should
                   (equal (list third)
                          (vc-call-backend backend 'known-other-working-trees)))
                  (should-not (file-directory-p second))
                  (should (file-directory-p third))
                  ;; Moving the first working tree is only supported
                  ;; for some backends.
                  (cond ((and (eq backend 'Git)
                              (version<= "2.29" (vc-git--program-version)))
                         (let ((default-directory third))
                           (vc-move-working-tree backend first second))
                         (let ((default-directory third))
                           (should
                            (equal (list second)
                                   (vc-call-backend backend
                                                    'known-other-working-trees))))
                         (should-not (file-directory-p first))
                         (should (file-directory-p second))
                         (vc-move-working-tree backend second first))
                        ((eq backend 'Hg)
                         (let ((default-directory third))
                           (should-error (vc-move-working-tree backend
                                                               first second)))))
                  (vc-move-working-tree backend third second)

                  ;; Test `delete-working-tree'.
                  (let ((default-directory first))
                    (vc-delete-working-tree backend second)
                    (should-not (file-directory-p second))))))

          ;; Save exit.
          (ignore-errors
            (run-hooks 'vc-test--cleanup-hook)))))))

(declare-function vc-hg-command "vc-hg")
(declare-function vc-git--out-str "vc-git")

(defmacro vc-test--with-temp-change (buf &rest body)
  (declare (indent 1) (debug (symbolp body)))
  (cl-with-gensyms (handle)
    `(let ((,handle (prepare-change-group ,buf)))
       (unwind-protect
           (with-current-buffer ,buf
             (activate-change-group ,handle)
             (insert "bar\n")
             (write-region nil nil buffer-file-name nil t)
             ,@body)
         (cancel-change-group ,handle)
         (with-current-buffer ,buf
           (write-region nil nil buffer-file-name nil t))))))

(defun vc-test--checkin-patch (backend)
  "Test preparing and checking in patches."
  (ert-with-temp-directory _tempdir
    (let ((vc-handled-backends `(,backend))
          (default-directory
           (file-name-as-directory
            (expand-file-name
             (make-temp-name "vc-test") temporary-file-directory)))
          (file "foo")
          (author "VC user <vc@example.org>")
          (date "Fri, 19 Sep 2025 15:00:00 +0100")
          (desc1 "Make a modification")
          (desc2 "Make a modification redux")
          vc-test--cleanup-hook buf)
      (vc-test--with-author-identity backend
        (unwind-protect
            (cl-flet
                ((get-patch-string ()
                   "Get patch corresponding to most recent commit to FILE."
                   (let* ((rev (vc-symbolic-working-revision file backend))
                          (patch (vc-call-backend backend 'prepare-patch rev)))
                     (with-current-buffer (plist-get patch :buffer)
                       (buffer-substring-no-properties (point-min)
                                                       (point-max)))))
                 (revert (msg)
                   "Make a commit reverting the most recent change to FILE."
                   (with-current-buffer buf
                     (vc-checkin (list file) backend)
                     (insert msg)
                     (let (vc-async-checkin)
                       (log-edit-done))))
                 (check (author date desc)
                   "Assert that most recent commit has AUTHOR, DATE and DESC."
                   (should
                    (equal
                     (string-trim-right
                      (cl-case backend
                        (Git
                         (vc-git--out-str "log" "-n1"
                                          "--pretty=%an <%ae>%n%aD%n%B"))
                        (Hg
                         (with-output-to-string
                           (vc-hg-command standard-output 0 nil "log" "--limit=1"
                                          "--template"
                                          "{author}\n{date|rfc822date}\n{desc}")))))
                     (format "%s\n%s\n%s" author date desc)))))
              ;; (1) Cleanup.
              (add-hook 'vc-test--cleanup-hook
                        (let ((dir default-directory))
                          (lambda ()
                            (delete-directory dir 'recursive))))

              ;; (2) Basic setup.
              (make-directory default-directory)
              (vc-test--create-repo-function backend)
              (write-region "foo\n" nil file nil 'nomessage)
              (vc-register `(,backend (,file)))
              (setq buf (find-file-noselect file))
              (with-current-buffer buf
                (vc-checkin (list file) backend)
                (insert "Initial commit")
                (let (vc-async-checkin)
                  (log-edit-done)))

              ;; (3) Prepare a commit with a known Author & Date.
              (vc-test--with-temp-change buf
                (vc-root-diff nil)
                (vc-next-action nil)
                (insert desc1)
                (goto-char (point-min))
                (insert (format "Author: %s\n" author))
                (insert (format "Date: %s\n" date))
                (let (vc-async-checkin)
                  (log-edit-done)))

              ;; (4) Revert it, then test applying it with
              ;; checkin-patch, passing nil as COMMENT.  Should take the
              ;; author, date and comment from PATCH-STRING.
              (let ((patch-string (get-patch-string)))
                (revert "Revert modification, first time")
                (vc-test--with-temp-change buf
                  (vc-call-backend backend 'checkin-patch patch-string nil)))
              (check author date desc1)

              ;; (5) Revert it again and try applying it with
              ;; checkin-patch again, but passing non-nil COMMENT.
              ;; Should take the author, date but not the comment from
              ;; PATCH-STRING.
              (let ((patch-string (get-patch-string)))
                (revert "Revert modification, second time")
                (vc-test--with-temp-change buf
                  (vc-call-backend backend 'checkin-patch patch-string desc2)))
              (check author date desc2))

          ;; Save exit.
          (ignore-errors
            (run-hooks 'vc-test--cleanup-hook)))))))

(defun vc-test--apply-to-other-working-tree (backend)
  "Test `vc--apply-to-other-working-tree'."
  (ert-with-temp-directory _tempdir
    (let ((vc-handled-backends `(,backend))
          (default-directory
           (file-name-as-directory
            (expand-file-name
             (make-temp-name "vc-test") temporary-file-directory)))
          vc-test--cleanup-hook)
      (vc-test--with-author-identity backend
        (unwind-protect
            (let ((first (file-truename
                          (file-name-as-directory
                           (expand-file-name "first" default-directory))))
                  (second (file-truename
                           (file-name-as-directory
                            (expand-file-name "second" default-directory)))))
              ;; Cleanup.
              (add-hook 'vc-test--cleanup-hook
                        (let ((dir default-directory))
                          (lambda ()
                            (delete-directory dir 'recursive))))

              ;; Set up the two working trees.
              (make-directory first 'parents)
              (let ((default-directory first)
                    (names '("foo" "bar" "baz")))
                (vc-test--create-repo-function backend)
                (dolist (str names)
                  (write-region (concat str "\n") nil str nil 'nomessage)
                  (vc-register `(,backend (,str))))
                (vc-checkin names backend "Test files"))
              ;; For the purposes of this test just copying the tree is
              ;; enough.  FIRST and SECOND don't have to actually share
              ;; a backing revisions store.
              (copy-directory first (directory-file-name second))

              ;; Make modifications that we will try to move.
              (let ((default-directory first))
                (write-region "qux\n" nil "qux" nil 'nomessage)
                (vc-register `(,backend ("qux")))
                (write-region "quux\n" nil "quux" nil 'nomessage)
                (cl-letf (((symbol-function 'y-or-n-p) #'always))
                  (vc-delete-file "bar"))
                (delete-file "baz")
                (write-region "foobar\n" nil "foo" nil 'nomessage)
                (should (eq (vc-state "foo"  backend) 'edited))
                (should (eq (vc-state "baz"  backend) 'missing))
                (should (eq (vc-state "bar"  backend) 'removed))
                (should (eq (vc-state "qux"  backend) 'added))
                (should (eq (vc-state "quux" backend) 'unregistered)))

              (cl-flet ((go ()
                          (let ((default-directory first)
                                (vc-no-confirm-moving-changes t))
                            (vc--apply-to-other-working-tree
                             second second `(,backend
                                             ("foo" "bar" "baz" "qux" "quux"))
                             nil t))))
                (let ((default-directory second))
                  ;; Set up a series of incompatibilities, one-by-one, and
                  ;; try to move.  In each case the problem should block the
                  ;; move from proceeding.

                  ;; User refuses to sync destination fileset.
                  (with-current-buffer (find-file-noselect "bar")
                    (set-buffer-modified-p t)
                    (cl-letf (((symbol-function 'y-or-n-p) #'ignore))
                      (should-error (go)))
                    (set-buffer-modified-p nil))

                  ;; New file to be copied already exists.
                  (with-temp-file "qux")
                  (should-error (go))
                  (delete-file "qux")

                  ;; File to be deleted has changes.
                  (write-region "foobar\n" nil "bar" nil 'nomessage)
                  (should-error (go))
                  (vc-revert-file "bar")

                  ;; Finally, a move that should succeed.  Check that
                  ;; everything we expected to happen did happen.
                  (go)
                  (with-current-buffer (find-file-noselect "foo")
                    (should (equal (buffer-string) "foobar\n")))
                  (should-not (file-exists-p "bar"))
                  (should-not (file-exists-p "baz"))
                  (should (file-exists-p "qux"))
                  (should (file-exists-p "quux"))
                  (let ((default-directory first))
                    (with-current-buffer (find-file-noselect "foo")
                      (should (equal (buffer-string) "foo\n")))
                    (should (file-exists-p "bar"))
                    (should (file-exists-p "baz"))
                    (should-not (file-exists-p "qux"))
                    (should-not (file-exists-p "quux"))))))

          ;; Save exit.
          (ignore-errors
            (run-hooks 'vc-test--cleanup-hook)))))))

;; Create the test cases.

(defun vc-test--rcs-enabled ()
  (executable-find "rcs"))

(defun vc-test--cvs-enabled ()
  (executable-find "cvs"))

(defvar vc-svn-program)
(defun vc-test--svn-enabled ()
  (and (executable-find "svnadmin")
       (executable-find vc-svn-program)))

(defun vc-test--sccs-enabled ()
  (executable-find "sccs"))

(defvar vc-src-program)
(defun vc-test--src-enabled ()
  (executable-find vc-src-program))

(defvar vc-bzr-program)
(defun vc-test--bzr-enabled ()
  (executable-find vc-bzr-program))

(defvar vc-git-program)
(defun vc-test--git-enabled ()
  (executable-find vc-git-program))

(defvar vc-hg-program)
(defun vc-test--hg-enabled ()
  (executable-find vc-hg-program))

(defvar vc-mtn-program)
(defun vc-test--mtn-enabled ()
  (executable-find vc-mtn-program))

;; Obsoleted.
(defvar vc-arch-program)
(defun vc-test--arch-enabled ()
  (executable-find vc-arch-program))

;; Create the test cases.
(dolist (backend vc-handled-backends)
  (let ((backend-string (downcase (symbol-name backend))))
    (require (intern (format "vc-%s" backend-string)))
    (eval
     ;; Check, whether the backend is supported.
     `(when (funcall ',(intern (format "vc-test--%s-enabled" backend-string)))

	(ert-deftest
	    ,(intern (format "vc-test-%s00-create-repo" backend-string)) ()
	  ,(format "Check `vc-create-repo' for the %s backend."
		   backend-string)
	  (vc-test--create-repo ',backend))

	(ert-deftest
	    ,(intern (format "vc-test-%s01-register" backend-string)) ()
	  ,(format
	    "Check `vc-register' and `vc-registered' for the %s backend."
	    backend-string)
	  (skip-unless
	   (ert-test-passed-p
	    (ert-test-most-recent-result
	     (ert-get-test
	      ',(intern
		 (format "vc-test-%s00-create-repo" backend-string))))))
	  (vc-test--register ',backend))

	(ert-deftest
	    ,(intern (format "vc-test-%s02-state" backend-string)) ()
	  ,(format "Check `vc-state' for the %s backend." backend-string)
	  (skip-unless
	   (ert-test-passed-p
	    (ert-test-most-recent-result
	     (ert-get-test
	      ',(intern
		 (format "vc-test-%s01-register" backend-string))))))
	  (vc-test--state ',backend))

	(ert-deftest
	    ,(intern (format "vc-test-%s03-working-revision" backend-string)) ()
	  ,(format "Check `vc-working-revision' for the %s backend."
		   backend-string)
	  (skip-unless
	   (ert-test-passed-p
	    (ert-test-most-recent-result
	     (ert-get-test
	      ',(intern
		 (format "vc-test-%s01-register" backend-string))))))
	  (vc-test--working-revision ',backend))

	(ert-deftest
	    ,(intern (format "vc-test-%s04-checkout-model" backend-string)) ()
	  ,(format "Check `vc-checkout-model' for the %s backend."
		   backend-string)
	  (skip-unless
	   (ert-test-passed-p
	    (ert-test-most-recent-result
	     (ert-get-test
	      ',(intern
		 (format "vc-test-%s01-register" backend-string))))))
	  (vc-test--checkout-model ',backend))

        (ert-deftest
            ,(intern (format "vc-test-%s05-rename-file" backend-string)) ()
          ,(format "Check `vc-rename-file' for the %s backend."
                   backend-string)
          (skip-unless
           (ert-test-passed-p
            (ert-test-most-recent-result
             (ert-get-test
              ',(intern
                 (format "vc-test-%s01-register" backend-string))))))
          ;; CVS calls vc-delete-file, which insists on prompting
          ;; "Really want to delete ...?", and `vc-mtn.el' does not implement
          ;; `delete-file' at all.
          (skip-when (memq ',backend '(CVS Mtn)))
          (vc-test--rename-file ',backend))

        (ert-deftest
            ,(intern (format "vc-test-%s06-version-diff" backend-string)) ()
          ,(format "Check `vc-version-diff' for the %s backend."
                   backend-string)
          (skip-unless
           (ert-test-passed-p
            (ert-test-most-recent-result
             (ert-get-test
              ',(intern
                 (format "vc-test-%s01-register" backend-string))))))
          ;; `vc-mtn.el' gives me:
          ;; "Failed (status 1): mtn commit -m Testing vc-version-diff\n\n foo"
          (skip-when (memq ',backend '(Mtn)))
          ;; `vc-hg.el' gives me, only on MS-Windows and only in batch mode:
          ;; "Failed (status 255): hg --config ui.report_untrusted=0 commit -m Testing vc-version-diff\n\n foo"
          (skip-when (and (memq ',backend '(Hg))
                          (eq system-type 'windows-nt)
                          noninteractive))
          (vc-test--version-diff ',backend))

        (ert-deftest
            ,(intern (format "vc-test-%s07-other-working-trees" backend-string)) ()
          ,(format "Check other working trees functions for the %s backend."
                   backend-string)
          (skip-unless
	   (ert-test-passed-p
	    (ert-test-most-recent-result
	     (ert-get-test
	      ',(intern
	         (format "vc-test-%s01-register" backend-string))))))
          (skip-unless (memq ',backend '(Git Hg)))
          (skip-when
           (and (eq ',backend 'Hg)
                (cl-search "failed to import extension"
                           (car
                            (process-lines-ignore-status
                             "hg" "--config=extensions.share=" "share")))))
          (skip-when
           (and (eq ',backend 'Git)
                (version< (vc-git--program-version) "2.17")))
          (let ((vc-hg-global-switches (cons "--config=extensions.share="
                                             vc-hg-global-switches)))
            (vc-test--other-working-trees ',backend)))

        (ert-deftest
            ,(intern (format "vc-test-%s08-apply-to-other-working-tree" backend-string)) ()
          ,(format "Test `vc--apply-to-other-working-tree' with the %s backend."
                   backend-string)
          (skip-when
	   (ert-test-skipped-p
	    (ert-test-most-recent-result
	     (ert-get-test
	      ',(intern
	         (format "vc-test-%s07-other-working-trees" backend-string))))))
          (vc-test--apply-to-other-working-tree ',backend))

        (ert-deftest
            ,(intern (format "vc-test-%s09-checkin-patch" backend-string)) ()
          ,(format "Check preparing and checking in patches with the %s backend."
                   backend-string)
          (skip-unless
	   (ert-test-passed-p
	    (ert-test-most-recent-result
	     (ert-get-test
	      ',(intern
	         (format "vc-test-%s01-register" backend-string))))))
          (skip-unless (memq ',backend '(Git Hg)))
          (vc-test--checkin-patch ',backend))))))

(provide 'vc-tests)
;;; vc-tests.el ends here
