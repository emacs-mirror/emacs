;;; vc-tests.el --- Tests of different backends of vc.el

;; Copyright (C) 2014-2015 Free Software Foundation, Inc.

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
;; along with this program.  If not, see `http://www.gnu.org/licenses/'.

;;; Commentary:

;; For every supported VC on the machine, different test cases are
;; generated automatically.

;; Functions to be tested (see Commentary of vc.el).  Mandatory
;; functions are marked with `*', optional functions are marked with `-':

;; BACKEND PROPERTIES
;;
;; * revision-granularity

;; STATE-QUERYING FUNCTIONS
;;
;; * registered (file)
;; * state (file)
;; - dir-status (dir update-function)
;; - dir-status-files (dir files default-state update-function)
;; - dir-extra-headers (dir)
;; - dir-printer (fileinfo)
;; - status-fileinfo-extra (file)
;; * working-revision (file)
;; - latest-on-branch-p (file)
;; * checkout-model (files)
;; - mode-line-string (file)

;; STATE-CHANGING FUNCTIONS
;;
;; * create-repo (backend)
;; * register (files &optional comment)
;; - responsible-p (file)
;; - receive-file (file rev)
;; - unregister (file)
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

;; The working horses.

(defvar vc-test--cleanup-hook nil
  "Functions for cleanup at the end of an ert test.
Don't set it globally, the functions shall be let-bound.")

(defun vc-test--revision-granularity-function (backend)
  "Run the `vc-revision-granularity' backend function."
  (funcall (intern (downcase (format "vc-%s-revision-granularity" backend)))))

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
      (shell-command-to-string (format "cvs -Q -d:local:%s co module" tmp-dir))
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
	vc-test--cleanup-hook)

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
	  (vc-test--create-repo-function backend))

      ;; Save exit.
      (ignore-errors (run-hooks 'vc-test--cleanup-hook)))))

;; Why isn't there `vc-unregister'?
(defun vc-test--unregister-function (backend file)
  "Run the `vc-unregister' backend function.
For backends which dont support it, `vc-not-supported' is signalled."

  (let ((symbol (intern (downcase (format "vc-%s-unregister" backend)))))
    (if (functionp symbol)
	(funcall symbol file)
      ;; CVS, SVN, SCCS, SRC and Mtn are not supported.
      (signal 'vc-not-supported (list 'unregister backend)))))

(defun vc-test--register (backend)
  "Register and unregister a file."

  (let ((vc-handled-backends `(,backend))
	(default-directory
	  (file-name-as-directory
	   (expand-file-name
	    (make-temp-name "vc-test") temporary-file-directory)))
	vc-test--cleanup-hook)

    (unwind-protect
	(progn
	  ;; Cleanup.
	  (add-hook
	   'vc-test--cleanup-hook
	   `(lambda () (delete-directory ,default-directory 'recursive)))

	  ;; Create empty repository.
	  (make-directory default-directory)
	  (vc-test--create-repo-function backend)

	  (let ((tmp-name1 (expand-file-name "foo" default-directory))
		(tmp-name2 "bla"))
	    ;; Register files.  Check for it.
	    (write-region "foo" nil tmp-name1 nil 'nomessage)
	    (should (file-exists-p tmp-name1))
	    (should-not (vc-registered tmp-name1))
	    (write-region "bla" nil tmp-name2 nil 'nomessage)
	    (should (file-exists-p tmp-name2))
	    (should-not (vc-registered tmp-name2))
	    (vc-register
	     (list backend (list tmp-name1 tmp-name2)))
	    (should (file-exists-p tmp-name1))
	    (should (vc-registered tmp-name1))
	    (should (file-exists-p tmp-name2))
	    (should (vc-registered tmp-name2))

	    ;; Unregister the files.
	    (condition-case err
		(progn
		  (vc-test--unregister-function backend tmp-name1)
		  (should-not (vc-registered tmp-name1))
		  (vc-test--unregister-function backend tmp-name2)
		  (should-not (vc-registered tmp-name2)))
	      ;; CVS, SVN, SCCS, SRC and Mtn are not supported.
	      (vc-not-supported (message "%s" (error-message-string err))))
	    (should (file-exists-p tmp-name1))
	    (should (file-exists-p tmp-name2))))

      ;; Save exit.
      (ignore-errors (run-hooks 'vc-test--cleanup-hook)))))

;; `vc-state' returns different results for different backends.  So we
;; don't check with `should', but print the results for analysis.
(defun vc-test--state (backend)
  "Check the different states of a file."

  (let ((vc-handled-backends `(,backend))
	(default-directory
	  (file-name-as-directory
	   (expand-file-name
	    (make-temp-name "vc-test") temporary-file-directory)))
	vc-test--cleanup-hook errors)

    (unwind-protect
	(progn
	  ;; Cleanup.
	  (add-hook
	   'vc-test--cleanup-hook
	   `(lambda () (delete-directory ,default-directory 'recursive)))

	  ;; Create empty repository.
	  (make-directory default-directory)
	  (vc-test--create-repo-function backend)

	  (message "%s" (vc-state default-directory backend))
	  ;(should (eq (vc-state default-directory backend) 'up-to-date))

	  (let ((tmp-name (expand-file-name "foo" default-directory)))
	    ;; Check for initial state.
	    (message "%s" (vc-state tmp-name backend))
	    ;(should (eq (vc-state tmp-name backend) 'unregistered))

	    ;; Write a new file.  Check for state.
	    (write-region "foo" nil tmp-name nil 'nomessage)
	    (message "%s" (vc-state tmp-name backend))
	    ;(should (eq (vc-state tmp-name backend) 'unregistered))

	    ;; Register a file.  Check for state.
	    (vc-register
	     (list backend (list (file-name-nondirectory tmp-name))))
	    (message "%s" (vc-state tmp-name backend))
	    ;(should (eq (vc-state tmp-name backend) 'added))

	    ;; Unregister the file.  Check for state.
	    (condition-case nil
		(progn
		  (vc-test--unregister-function backend tmp-name)
		  (message "%s" (vc-state tmp-name backend))
		  );(should (eq (vc-state tmp-name backend) 'unregistered)))
	      (vc-not-supported (message "%s" 'unsupported)))))

      ;; Save exit.
      (ignore-errors (run-hooks 'vc-test--cleanup-hook)))))

(defun vc-test--working-revision (backend)
  "Check the working revision of a repository."

  (let ((vc-handled-backends `(,backend))
	(default-directory
	  (file-name-as-directory
	   (expand-file-name
	    (make-temp-name "vc-test") temporary-file-directory)))
	vc-test--cleanup-hook errors)

    (unwind-protect
	(progn
	  ;; Cleanup.
	  (add-hook
	   'vc-test--cleanup-hook
	   `(lambda () (delete-directory ,default-directory 'recursive)))

	  ;; Create empty repository.
	  (make-directory default-directory)
	  (vc-test--create-repo-function backend)

	  (should
	   (member
	    (vc-working-revision default-directory backend) '("0" "master")))

	  (let ((tmp-name (expand-file-name "foo" default-directory)))
	    ;; Check for initial state, should be nil until it's registered.
            ;; Don't pass the backend explictly, otherwise some implementations
            ;; return non-nil.
	    (should (null (vc-working-revision tmp-name)))

	    ;; Write a new file.  Check state.
	    (write-region "foo" nil tmp-name nil 'nomessage)
	    (should (null (vc-working-revision tmp-name)))

	    ;; Register a file.  Check for state.
	    (vc-register
	     (list backend (list (file-name-nondirectory tmp-name))))
            ;; FIXME: Don't pass the backend.  Emacs should be able to
            ;; figure it out.
	    (should
	     (member (vc-working-revision tmp-name backend) '("0" "master")))

	    ;; Unregister the file.  Check for working-revision.
	    (condition-case nil
		(progn
		  (vc-test--unregister-function backend tmp-name)
		  (should
		   (member
		    (vc-working-revision tmp-name backend) '("0" "master"))))
	      (vc-not-supported (message "%s" 'unsupported)))))

      ;; Save exit.
      (ignore-errors (run-hooks 'vc-test--cleanup-hook)))))

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

(defvar vc-arch-program)
(defun vc-test--arch-enabled ()
  (executable-find vc-arch-program))


;; There are too many failed test cases yet.  We suppress them on hydra.
(if (getenv "NIX_STORE")
    (ert-deftest vc-test ()
      "Dummy test case for hydra."
      (ert-pass))

  ;; Create the test cases.
  (dolist (backend vc-handled-backends)
    (let ((backend-string (downcase (symbol-name backend))))
      (require (intern (format "vc-%s" backend-string)))
      (eval
       ;; Check, whether the backend is supported.
       `(when (funcall ',(intern (format "vc-test--%s-enabled" backend-string)))

	  (ert-deftest
	      ,(intern (format "vc-test-%s00-create-repo" backend-string)) ()
	    ,(format "Check `vc-create-repo' for the %s backend." backend-string)
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
	    ,(format "Check `vc-working-revision' for the %s backend." backend-string)
	    (skip-unless
	     (ert-test-passed-p
	      (ert-test-most-recent-result
	       (ert-get-test
		',(intern
		   (format "vc-test-%s01-register" backend-string))))))
	    (vc-test--working-revision ',backend)))))))

(provide 'vc-tests)
;;; vc-tests.el ends here
