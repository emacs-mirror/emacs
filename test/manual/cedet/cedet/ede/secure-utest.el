;;; secure-utest.el --- Test the security features of EDE.
;;
;; Copyright (C) 2014 Eric Ludlam
;;
;; Author: Eric Ludlam <zappo@ballista>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see http://www.gnu.org/licenses/.

;;; Commentary:
;;
;; EDE has several security features that prevents it from
;; accidentally loading malicious code.

;;; Code:

(defclass ede-security-entry ()
  ((file :initarg :file
	 :documentation
	 "The file to load in and test.")

   (classp :initarg :classp
	   :documentation
	   "Class test predicate.  It is passed the root project found.
Use this to see if a project was loaded or not.  Unsecure projects should
not be loaded.")

   (hazzard :initarg :hazzard
	    :documentation
	    "Non-nil if this entry is considered HAZARDOUS and shouldn't be
loaded the first time.")

   (has-config :initarg :has-config
	       :documentation
	       "Non-nil if this entry has a config file.  These projects
are considered SAFE, but will skip loading thier config file until explicitly
permitted.")
   )
  "A testing entry for the security unit tests.")

(defvar ede-security-project-entries
  (list
   (ede-security-entry "proj" :file "src/proj/TEST"
		       :classp 'ede-proj-project-p
		       :hazzard t
		       :has-config nil)
   (ede-security-entry "generic make" :file "src/generic/gen_make/sub/test.cpp"
		     :classp 'ede-generic-makefile-project-p
		     :hazzard nil
		     :has-config t)
   (ede-security-entry "generic scons" :file "src/generic/gen_scons/sub/test.cpp"
		     :classp 'ede-generic-scons-project-p
		     :hazzard nil
		     :has-config t)
   (ede-security-entry "generic cmake" :file "src/generic/gen_cmake/sub/test.cpp"
		     :classp 'ede-generic-cmake-project-p
		     :hazzard nil
		     :has-config t)
   (ede-security-entry "arduino" :file "src/arduino/Blink/Blink.ino"
		     :classp 'ede-arduino-project-p
		     :hazzard nil
		     :has-config t)
   )
  "List of project test entries to try.")

(defun ede-security-question-yes (&rest R)
  "Return that we want to add the project."
  t)

(defun ede-security-question-no (&rest R)
  "Return that we DO NOT want to add the project."
  nil)

(defun ede-security-question-err (&rest R)
  "Throw an error if the user is being pestered at the wrong time."
  (error "Query posed at the wrong time!"))

(defun ede-security-utest ()
  "Execute security unit tests."
  (interactive)
  (save-excursion

    ;; Protect from previous tests.  Flush all project caches, and all known projects.
    (ede-flush-directory-hash)
    (ede-flush-project-hash)
    (setq ede-projects nil) ;; Whack all known projects.

    ;; Enable the generic EDE project types so we can test them.
    (ede-enable-generic-projects)

    ;; Start Logging
    (cedet-utest-log-setup "EDE SECURITY")

    (set-buffer (semantic-find-file-noselect
		 (expand-file-name "cedet/ede/detect.el"
				   cedet-utest-root)))

    (let ((ede-project-directories nil) ;; Force us to ADD projects.
	  (errlog nil)
	  )

      (dolist (fle ede-security-project-entries)

	(ede-security-test-one-entry fle)

	)

      ;; Close out the test suite.
      (cedet-utest-log-shutdown
       "EDE SECURITY"
       (when errlog
	 (format "%s Failures found." (length errlog)))))

    ))

(defun ede-security-test-one-entry (entry)
  "Test a project ENTRY.  Use QUERYFCN as the replacement user query fcn."
  (let ((ede-check-project-query-fcn 'ede-security-question-err)
	)

    ;; Make sure we have the files we think we have.
    (when (not (file-exists-p (oref fle :file)))
      (error "Cannot find unit test; file not found: %s" (oref fle :file)))

    ;; Notes:
    (message "  Security Test for: %S" (oref fle :file))

    ;; Do the load
    (let ((fb (find-buffer-visiting (oref fle :file)))
	  (b (semantic-find-file-noselect (oref fle :file))))

      (save-excursion
	(set-buffer b)

	(when (oref fle :hazzard)
	  ;; Projects that might load dangerous code should be protected.
	  ;; This section makes sure it doesn't load.

	  ;; Run the EDE detection code.  Firing up the mode isn't really needed.
	  ;; Don't protect this as with the detect-utest.el stuff.  That should
	  ;; have vetted these projects.  Now we are only testing if they detected.
	  (ede-initialize-state-current-buffer)
	  (when (not (eq b (current-buffer)))
	    (error "Buffer changed during init!"))

	  (when ede-object-root-project
	    (error "Unsafe project was loaded without asking!"))

	  ;; Now do the same thing again, but this time by using the
	  ;; security fcn direcly, which is similar to forcing EDE to
	  ;; load the project by using the `ede' function.  Say NO when
	  ;; it wants to ask the security question.
	  (setq ede-check-project-query-fcn 'ede-security-question-no)
	  (if (ede-check-project-directory default-directory)
	      (error "Unsafe project would have loaded even though we said no!"))

	  (when (member (directory-file-name default-directory) ede-project-directories)
	    (error "We asked to not load this project, but it was added to the project directories."))

	  ;; Try again, this time really try to load the project, and also
	  ;; say YES when it asks the question.
	  (setq ede-check-project-query-fcn 'ede-security-question-yes)
	  (ede default-directory)

	  (when (not ede-object-root-project)
	    (error "Unsafe project was NOT loaded even though we said yes!"))

	  (unless (member (directory-file-name default-directory) ede-project-directories)
	    (error "We asked to make it safe, but it wasn't added to the safe dirs list."))
	  )

	(when (not (oref fle :hazzard))
	  ;; Code that is not hazzardous should get created automatically
	  ;; during buffer initialization.

	  ;; Run the EDE detection code.  Firing up the mode isn't really needed.
	  ;; This should load one of these non-hazzard modes.
	  (ede-initialize-state-current-buffer)
	  (when (not (eq b (current-buffer)))
	    (error "Buffer changed during init!"))

	  (unless ede-object-root-project
	    (error "Safe project was not loaded!"))
	  )

	(when (oref fle :has-config)
	  ;; In both cases (hazzard and non-hazzard projects) the project should
	  ;; be loaded now.  Next, we need to force the configuration to get
	  ;; loaded from disk

	  (let ((config (ede-config-get-configuration ede-object-root-project)))

	    ;; Make sure there is a config
	    (when (not config)
	      (error "No configuration for project %S"
		     (eieio-object-name ede-object-root-project)))

	    ;; Make sure the config was automatically ignored.
	    (unless (and (oref config ignored-file)
			 (eq (oref config ignored-file) 'auto))
	      (error "Configuration was not auto-ignored. [%S]"
		     (oref config ignored-file)))

	    ;; Force loading a project, but say no.
	    (setq ede-check-project-query-fcn 'ede-security-question-no)
	    (project-rescan ede-object-root-project)
	    (setq config (ede-config-get-configuration ede-object-root-project))

	    ;; Make sure there is a config
	    (when (not config)
	      (error "No configuration (part 2) for project %S"
		     (eieio-object-name ede-object-root-project)))

	    ;; Make sure the config was manually ignored via our NO fcn.
	    (unless (and (oref config ignored-file)
			 (eq (oref config ignored-file) 'manual))
	      (if (eq (oref config ignored-file) 'auto)
		  (error "Configuration was not manually ignored.
Make sure project-rescan has (call-next-method)")
		(error "Configuration was not manually-ignored. [%S]"
		       (oref config ignored-file))))


	    ;; Now agree to load the config.
	    (setq ede-check-project-query-fcn 'ede-security-question-yes)
	    (project-rescan ede-object-root-project)
	    (setq config (ede-config-get-configuration ede-object-root-project))

	    ;; Make sure there is a config
	    (when (not config)
	      (error "No configuration (part 3) for project %S"
		     (eieio-object-name ede-object-root-project)))

	    ;; Make sure the config was manually ignored via our NO fcn.
	    (when (oref config ignored-file)
	      (error "Configuration was ignored instead of loaded."))

	    (unless (and (oref config c-preprocessor-table)
			 (string= "TEST" (car (car (oref config c-preprocessor-table)))))
	      (error "Config claimed to be loaded, but stored setting was ignored."))

	    ))

	)
      ;; If it wasn't already in memory, whack it.
      (when (and b (not fb))
	(kill-buffer b))
      ))
  )


(provide 'cedet/ede/secure-utest)

;;; secure-utest.el ends here
