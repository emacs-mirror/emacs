;;; vc-tests.el --- Tests of different backends of vc.el

;; Copyright (C) 2014-2020 Free Software Foundation, Inc.

;; Author: Michael Albinus <michael.albinus@gmx.de>

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see `https://www.gnu.org/licenses/'.

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

;; STATE-CHANGING FUNCTIONS
;;
;; * create-repo (backend)                                      DONE
;; * register (files &optional comment)                         DONE
;; - responsible-p (file)
;; - receive-file (file rev)
;; - unregister (file)                                          DONE
;; * checkin (files comment)
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

;; HISTORY FUNCTIONS
;;
;; * print-log (files buffer &optional shortlog start-revision limit)
;; * log-outgoing (backend remote-location)
;; * log-incoming (backend remote-location)
;; - log-view-mode ()
;; - show-log-entry (revision)
;; - comment-history (file)
;; - update-changelog (files)
;; * diff (files &optional async rev1 rev2 buffer)
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
;; - rename-file (old new)
;; - find-file-hook ()
;; - extra-menu ()
;; - extra-dir-menu ()
;; - conflicted-files (dir)

;;; Code:

(require 'ert)
(require 'vc)

(declare-function w32-application-type "w32proc.c")

;; The working horses.

(defvar vc-test--cleanup-hook nil
  "Functions for cleanup at the end of an ert test.
Don't set it globally, the functions should be let-bound.")

(defun vc-test--revision-granularity-function (backend)
  "Run the `vc-revision-granularity' backend function."
  (vc-call-backend backend 'revision-granularity))

(defun vc-test--create-repo-function (backend)
  "Run the `vc-create-repo' backend function.
For backends which dont support it, it is emulated."

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
		`(lambda () (delete-directory ,tmp-dir 'recursive)))))

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

(defun vc-test--create-repo (backend)
  "Create a test repository in `default-directory', a temporary directory."

  (let ((vc-handled-backends `(,backend))
	(default-directory
	  (file-name-as-directory
	   (expand-file-name
	    (make-temp-name "vc-test") temporary-file-directory)))
	(process-environment process-environment)
	tempdir
	vc-test--cleanup-hook)
    (when (eq backend 'Bzr)
      (setq tempdir (make-temp-file "vc-test--create-repo" t)
	    process-environment (cons (format "BZR_HOME=%s" tempdir)
				      process-environment)))

    (unwind-protect
	(progn
	  ;; Cleanup.
	  (add-hook
	   'vc-test--cleanup-hook
	   `(lambda () (delete-directory ,default-directory 'recursive)))

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
        (if tempdir (delete-directory tempdir t))
        (run-hooks 'vc-test--cleanup-hook)))))

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
  `(let (err)
    (condition-case err
        (funcall ,func ,@args)
      (vc-not-supported 'vc-not-supported)
      (t (signal (car err) (cdr err))))))

(defun vc-test--register (backend)
  "Register and unregister a file.
This checks also `vc-backend' and `vc-responsible-backend'."

  (let ((vc-handled-backends `(,backend))
	(default-directory
	  (file-name-as-directory
	   (expand-file-name
	    (make-temp-name "vc-test") temporary-file-directory)))
	(process-environment process-environment)
	tempdir
	vc-test--cleanup-hook)
    (when (eq backend 'Bzr)
      (setq tempdir (make-temp-file "vc-test--register" t)
	    process-environment (cons (format "BZR_HOME=%s" tempdir)
				      process-environment)))
    (unwind-protect
	(progn
	  ;; Cleanup.
	  (add-hook
	   'vc-test--cleanup-hook
	   `(lambda () (delete-directory ,default-directory 'recursive)))

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
        (if tempdir (delete-directory tempdir t))
        (run-hooks 'vc-test--cleanup-hook)))))

(defun vc-test--state (backend)
  "Check the different states of a file."

  (let ((vc-handled-backends `(,backend))
	(default-directory
	  (file-name-as-directory
	   (expand-file-name
	    (make-temp-name "vc-test") temporary-file-directory)))
	(process-environment process-environment)
	tempdir
	vc-test--cleanup-hook)
    (when (eq backend 'Bzr)
      (setq tempdir (make-temp-file "vc-test--state" t)
	    process-environment (cons (format "BZR_HOME=%s" tempdir)
				      process-environment)))
    (unwind-protect
	(progn
	  ;; Cleanup.
	  (add-hook
	   'vc-test--cleanup-hook
	   `(lambda () (delete-directory ,default-directory 'recursive)))

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
              ;; unsupported: CVS Mtn SCCS SRC SVN
              (message "vc-state5 %s %s" backend (vc-state tmp-name backend))
              (should (memq (vc-state tmp-name backend)
                            '(nil unregistered))))))

      ;; Save exit.
      (ignore-errors
	(if tempdir (delete-directory tempdir t))
	(run-hooks 'vc-test--cleanup-hook)))))

(defun vc-test--working-revision (backend)
  "Check the working revision of a repository."

  (let ((vc-handled-backends `(,backend))
	(default-directory
	  (file-name-as-directory
	   (expand-file-name
	    (make-temp-name "vc-test") temporary-file-directory)))
	(process-environment process-environment)
	tempdir
	vc-test--cleanup-hook)
    (when (eq backend 'Bzr)
      (setq tempdir (make-temp-file "vc-test--working-revision" t)
	    process-environment (cons (format "BZR_HOME=%s" tempdir)
				      process-environment)))

    (unwind-protect
	(progn
	  ;; Cleanup.
	  (add-hook
	   'vc-test--cleanup-hook
	   `(lambda () (delete-directory ,default-directory 'recursive)))

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
            (message "vc-working-revision4 %s" (vc-working-revision tmp-name))
            (should (member (vc-working-revision tmp-name) '(nil "0" "1.1")))

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
        (if tempdir (delete-directory tempdir t))
        (run-hooks 'vc-test--cleanup-hook)))))

(defun vc-test--checkout-model (backend)
  "Check the checkout model of a repository."

  (let ((vc-handled-backends `(,backend))
	(default-directory
	  (file-name-as-directory
	   (expand-file-name
	    (make-temp-name "vc-test") temporary-file-directory)))
	(process-environment process-environment)
	tempdir
	vc-test--cleanup-hook)
    (when (eq backend 'Bzr)
      (setq tempdir (make-temp-file "vc-test--checkout-model" t)
	    process-environment (cons (format "BZR_HOME=%s" tempdir)
				      process-environment)))

    (unwind-protect
	(progn
	  ;; Cleanup.
	  (add-hook
	   'vc-test--cleanup-hook
	   `(lambda () (delete-directory ,default-directory 'recursive)))

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
        (if tempdir (delete-directory tempdir t))
        (run-hooks 'vc-test--cleanup-hook)))))

;; Create the test cases.

(defun vc-test--rcs-enabled ()
  (executable-find "rcs"))

(defun vc-test--cvs-enabled ()
  (executable-find "cvs"))

(defvar vc-svn-program)
(defun vc-test--svn-enabled ()
  (executable-find vc-svn-program))

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
	  (vc-test--checkout-model ',backend))))))

(provide 'vc-tests)
;;; vc-tests.el ends here
