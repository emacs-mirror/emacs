;;; ede/detect.el --- Tests for detecting different kinds of projects.
;;
;; Copyright (C) 2014, 2015 Eric M. Ludlam
;;
;; Author: Eric M. Ludlam <eric@siege-engine.com>
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

;;; Code:

(require 'ede/linux)
(require 'eieio)

(defclass ede-detect-entry ()
  ((file :initarg :file
	 :documentation
	 "The file to load in and test.")

   (feature :initarg :feature
	    :documentation
	    "The feature that gets loaded in association with this project.
If left empty, no feature based testing occurs.")

   ;; These represent different ways to test the discovered project.
   (classp :initarg :classp
	   :documentation
	   "Class test predicate.  It is passed the root project found.")
   (optp :initarg :optp
	 :initform nil
	 :documentation
	 "Optional 2nd predicate for testing.
It is passed the root project found.")
   (dorescan :initarg :dorescan
	     :initform t
	     :documentation
	     "Non-nil if this test should even try to do the rescan test.")
   (rescan :initarg :rescan
	   :initform nil
	   :documentation
	   "Non-nil if rescan is expected to do a full replace.")

   ;; When running the test, store the result of each test in these
   ;; state variables.
   (init-state :initform nil)
   (init-opt-data)
   (classp-state :initform nil)
   (optp-state :initform nil)
   (doubledetect-state :initform nil)
   (rescan-state :initform nil)
   (rescan-opt-data)
   )
  "A single testing entry.")

(defmethod ede-detect-do-test ((entry ede-detect-entry) proj)
  "Run tests on ENTRY to validate found PROJ is correct."
  (let ((cp (oref entry :classp))
	(op (oref entry :optp)))
    ;; Cache the results.
    (oset entry classp-state (funcall cp proj))
    (oset entry optp-state (or (not op) (funcall op proj)))
    ;; Return the results.
    (and (oref entry classp-state) (oref entry optp-state))))

(defmethod ede-detect-show-state (header)
  "Display a header for the state of entries."
  (cedet-utest-log "\n%40s Found Class Opt 2ndFind Rescan Overall State"
		   "Relative File Name")
  (cedet-utest-log "%40s ----- ----- --- ------- ------ -------------"
		   "------------------")
  )

(defmethod ede-detect-show-state ((entry ede-detect-entry))
  "Display the state of this entry."
  (let* ((init (oref entry init-state))
	 (found (if (eq init t) " t " "nil"))
	 (class (if (oref entry classp-state) " t " "nil"))
	 (opt   (if (slot-boundp entry :optp)
		    (if (oref entry optp-state) " t " "nil")
		  " - "))
	 (ddet (oref entry doubledetect-state))
	 (ddets (if (stringp ddet) "nil" " t "))
	 ;; Rescan may never be reached.
	 (rescan (oref entry rescan-state))
	 (rescan-dat (when (slot-boundp entry 'rescan-opt-data)
		       (oref entry rescan-opt-data)))
	 (rescans (if (stringp ddet) " x "
		    (if (oref entry :dorescan)
			(if (eq rescan t) " t " "nil")
		      " - ")))
	 (inits (if found
		    ;; Checkd double detect
		    (if (eq ddet t)
			;; Check rescan messages.
			(if (or (eq rescan t) (not (oref entry :dorescan)))
			    "Success!"
			  ;; Else, check rescan should be a text message.
			  (stringp rescan)
			  (if (slot-boundp entry 'rescan-opt-data)
			      (format "%s: %S" rescan rescan-dat)
			    rescan))
		      ;; Else, and error from the double detect.
		      ddet)
		  ;; Else, check init text message
		  (if (slot-boundp entry init-opt-data)
		      (format "%s: %S" init (oref entry init-opt-data))
		    init)))
	 )
    (cedet-utest-log "%40s %s   %s   %s  %s     %s    %s"
			   (oref entry file)
			   found class opt ddets rescans inits)
    (when (stringp init)
      ;; A string is an error with some details.
      (cedet-utest-log "%40s %s" "--init msg-- "init))
    ))

(defmethod ede-detect-show-progress ((entry ede-detect-entry))
  "Show progress while executing tests."
  (cedet-utest-log "Testing %s ..." (eieio-object-name entry)))

(defvar ede-detect-utest-project-entries
  (list
   (ede-detect-entry "proj" :file "src/proj/sub/TEST"
		     :classp 'ede-proj-project-p
		     :optp 'ede-detect-utest-subproj-p
		     :rescan t)
   (ede-detect-entry "proj sub" :file "src/proj/Project.ede"
		     :classp 'ede-proj-project-p
		     :rescan t)
   (ede-detect-entry "project-am sub" :file "src/automake/sub/Makefile.am"
		     :classp 'project-am-makefile-p
		     :optp 'ede-detect-utest-subautomake-p)
   (ede-detect-entry "project-am" :file "src/automake/Makefile.am"
		     :classp 'project-am-makefile-p)
   (ede-detect-entry "android sub" :file "src/android/AndroidManifest.xml"
		     :classp 'ede-android-project-p)
   (ede-detect-entry "emacs" :file "src/emacs/Makefile"
		     :classp 'ede-emacs-project-p)
   (ede-detect-entry "emacs sub" :file "src/emacs/src/emacs.c"
		     :classp 'ede-emacs-project-p)
   (ede-detect-entry "linux driver" :file "src/linux/drivers/block/ub.c"
		     :classp 'ede-linux-project-p)
   (ede-detect-entry "linux" :file "src/linux/Makefile"
		     :classp 'ede-linux-project-p)
   (ede-detect-entry "linux sub" :file "src/linux/scripts/ver_linux"
		     :classp 'ede-linux-project-p)
   ;; Generic project types just key of Makefile, SCons, etc.
   (ede-detect-entry "generic make" :file "src/generic/gen_make/sub/test.cpp"
		     :classp 'ede-generic-makefile-project-p
		     :optp 'ede-detect-utest-generic-p)
   (ede-detect-entry "generic scons" :file "src/generic/gen_scons/sub/test.cpp"
		     :classp 'ede-generic-scons-project-p
		     :optp 'ede-detect-utest-generic-p)
   (ede-detect-entry "generic cmake" :file "src/generic/gen_cmake/sub/test.cpp"
		     :classp 'ede-generic-cmake-project-p
		     :optp'ede-detect-utest-generic-p)
   ;; Generic Version Control only case.
   (ede-detect-entry "generic vc" :file "src/generic/gen_vc/sub/test.cpp"
		     :classp 'ede-generic-vc-project-p
		     :optp 'ede-detect-utest-generic-vc-p)
   ;; these ROOT projects are created by hand in a .emacs file.
   ;; These need to be defined in here to get this test to work.
   (ede-detect-entry "cpp-root sub" :file "src/cpproot/src/main.cpp"
		     :classp 'ede-cpp-root-project-p
		     :dorescan nil)
   (ede-detect-entry "cpp-root" :file "src/cpproot/README"
		     :classp 'ede-cpp-root-project-p
		     :dorescan nil)
   )
  "List of testing entries that do not use `diretest' feature.")

(defvar ede-detect-utest-linux-extra-project-entries
  (list
   (ede-detect-entry "linux driver" :file "src/linux/drivers/block/ub.c"
		     :classp 'ede-linux-project-p)
   (ede-detect-entry "linux" :file "src/linux/Makefile"
		     :classp 'ede-linux-project-p)
   (ede-detect-entry "linux sub" :file "src/linux/scripts/ver_linux"
		     :classp 'ede-linux-project-p)
   )
  "List of testing entries that do not use `diretest' feature.")

(defvar ede-detect-utest-project-dirmatch-entries
  (list
   (ede-detect-entry "dirmatch sub"
		     :file "src/dirmatch/MyDirmatch/sub/dmlib.cpp"
		     :feature 'cedet/ede/detect-dirtest
		     :classp 'ede-detect-test-dirmatch-project-p)
   (ede-detect-entry "dirmatch"
		     :file "src/dirmatch/MyDirmatch/MyDirmatch.cpp"
		     :classp 'ede-detect-test-dirmatch-project-p)
   (ede-detect-entry "arduino sub"
		     :file "src/arduino/Blink/sub/lib.cpp"
		     :feature 'ede/arduino
		     :classp 'ede-arduino-project-p)
   (ede-detect-entry "arduino"
		     :file "src/arduino/Blink/Blink.ino"
		     :classp 'ede-arduino-project-p)
   )
  "List of project test entries for dirmatch projects.")

(defun ede-detect-utest-basedir ()
  "Get the basedir of the detection unit tests."
  (save-current-buffer
    (set-buffer (semantic-find-file-noselect
		 (expand-file-name "cedet/ede/detect.el"
				   cedet-utest-root)))
    (expand-file-name "src" default-directory)))

(defun ede-detect-utest-configdir ()
  "Get the basedir of the detection unit tests."
  (save-current-buffer
    (set-buffer (semantic-find-file-noselect
		 (expand-file-name "cedet/ede/detect.el"
				   cedet-utest-root)))
    (expand-file-name "config" default-directory)))

(ede-cpp-root-project "UTESTCPP"
		      :name "cpp root test"
		      :file (expand-file-name "cpproot/README"
					      (ede-detect-utest-basedir)))

;;;###autoload
(defun ede-detect-utest (&optional FLAG)
  "Test out the detection scheme for EDE.
Optional FLAG is for re-running a subset of tests with an alternate config."
  (interactive)
  (unless FLAG (setq FLAG 'none))
  (save-excursion
    ;; Make sure the dirtest project is set-up, but without
    ;; loading in the project type.
    (ede-detect-utest-init-dirmatch)

    ;; Enable the generic EDE project types so we can test them.
    (ede-enable-generic-projects)

    ;; Create a fake VC style project that we can detect.
    (ede-generic-new-autoloader "generic-VC" "FAKE VC"
				"VC" 'ede-generic-vc-project)

    ;; Initial State Dump
    ;;(message "Dump Autoload State.")
    ;;(dolist (pc ede-project-class-files)
    ;;  (message (eieio-object-name pc) ))

    ;; Start Logging
    (cond
     ((eq FLAG 'none)
      (cedet-utest-log-setup "EDE DETECT"))
     ((eq FLAG 'linux)
      (cedet-utest-log-setup "EDE LINUX EXTRA DETECT"))
     )

    (let ((errlog nil))

      ;; Test all the primary project types.
      (cond
       ((eq FLAG 'none)
	(ede-detect-utest-loop ede-detect-utest-project-entries FLAG))
       ((eq FLAG 'linux)
	(ede-detect-utest-loop ede-detect-utest-linux-extra-project-entries FLAG))
       )

      (when (member FLAG '(none))
	;; Make sure we didn't accidentally pull in the project using
	;; the dirtest project type.
	(ede-detect-utest-validate-loadstate nil)

	;; Now make sure that DIRTEST is testing properly.
	(ede-detect-utest-loop ede-detect-utest-project-dirmatch-entries FLAG)

	;; Make sure we did load dirtest - though that should be obvious if pre
	;; line worked.
	(ede-detect-utest-validate-loadstate t)
	)

      ;; Now lets retry the basics with INODE support turned off -- assuming
      ;; that our test platform supports it in the first place.
      (unless ede--disable-inode

	(setq ede--disable-inode t)
	(unwind-protect
	    (progn

	      (cedet-utest-log "\n-- Retry All Tests w/ INODE optimizations disabled.")
	      ;; ReTry all the primary project types.
              (cond
              ((eq FLAG 'none)
               (ede-detect-utest-loop ede-detect-utest-project-entries FLAG))
              ((eq FLAG 'linux)
               (ede-detect-utest-loop ede-detect-utest-linux-extra-project-entries FLAG))
	      )

	      ;; Now retry that DIRTEST is testing properly.
	      (when (member FLAG '(none))
                (ede-detect-utest-loop ede-detect-utest-project-dirmatch-entries FLAG))

	      )
	  (setq ede--disable-inode nil)))

      ;; Close out the test suite.
      (let ((msg (cond ((eq FLAG 'none)
                        "")
                       ((eq FLAG 'linux)
                        "LINUX "))))
      (cedet-utest-log-shutdown
       (concat "EDE " msg "DETECT")
       (when errlog
         (format "%s Failures found." (length errlog))))
      ))))

(defun ede-detect-linux-utest ()
  "Extra tests similar to ede-detect-utest, but by specifying a linux build dir."
  (ede-detect-utest 'linux))

(defun ede-detect-utest-loop (test-entries FLAG)
  "Test the primary EDE project types."
  (save-excursion
    (let ((project-linux-build-directory-default 'same)
	  (project-linux-architecture-default
           (cond
            ((eq FLAG 'none)
             'same)
            ((eq FLAG 'linux)
             "/tmp/")))
	  (ede-project-directories t) ; safe to load Project.ede
	  (basedir nil)
	  (baselen nil)
	  )
      (set-buffer (semantic-find-file-noselect
		   (expand-file-name "cedet/ede/detect.el"
				     cedet-utest-root)))
      (setq basedir default-directory
	    baselen (length basedir))

      (dolist (fle test-entries)

	;; Make sure we have the files we think we have.
	(when (not (file-exists-p (oref fle :file)))
	  (error "Cannot find unit test; file not found: %s" (oref fle :file)))

	;; Show we are starting a entry
	;;(ede-detect-show-progress fle)

	;; Do the detection
	(let ((fb (find-buffer-visiting (oref fle :file)))
	      (b (semantic-find-file-noselect (oref fle :file))))

	  (save-excursion
	    (set-buffer b)

	    ;; Run the EDE detection code.  Firing up the mode isn't really needed.
	    (condition-case err
		(progn
		  (ede-initialize-state-current-buffer)
		  (when (not (eq b (current-buffer)))
		    (error "Buffer changed during init!"))
		  )
	      (error
	       ;;(cedet-utest-log "\n!! In %s: load threw error %S\n"
	       ;;			(substring default-directory baselen)
	       ;;			err)
	       (oset fle init-state "Error thrown during buffer init.")
	       (oset fle init-opt-data err)
	       (push fle errlog)
	       ))

	    (let* ((proj ede-object-root-project))

	      (if (not proj)
		  (progn

		    ;; Use the detector to to provide better debugging info.
		    (let ((projdetect (ede-detect-directory-for-project default-directory)))

		      (if (not projdetect)
			  (progn
			    ;; Detected nothing
			    ;;(cedet-utest-log  "\n!! In %s: Detected nothing, wanted %S\n"
			    ;;			    (substring default-directory baselen)
			    ;;			    (eieio-object-name fle))
			    (oset fle init-state "No Project Detected.")
			    (push fle errlog))

			;; Else, some other error.
			;;(cedet-utest-log "\n!! In %s: Detected %S, failed to load project type %s\n"
			;;			 (substring default-directory baselen)
			;;			 (eieio-object-name (cdr projdetect))
			;;			 (eieio-object-name fle))
			(oset fle init-state
			      (format "Failed to load project of correct type. Found %S"
				      (oref (cdr projdetect) class-sym)))
			(oset fle init-opt-data (eieio-object-name (cdr projdetect)))
			(push fle errlog))))

		;; We successfully created a project.
		(oset fle init-state t)

		;; Test the result.
		(if (ede-detect-do-test fle proj)

		    (progn
		      ;; Once we have succeeded in detecting this once, what happens
		      ;; if the buffer is re-initialized - as per ede-reset-all-buffers?
		      ;; Run the EDE detection code.  Firing up the mode isn't really needed.
		      (condition-case err
			  (progn
			    (ede-initialize-state-current-buffer)
			    (when (not (eq b (current-buffer)))
			      (error "Buffer changed during init!"))
			    )
			(error
			 ;;(cedet-utest-log "\n!! In %s: load threw error %S\n"
			 ;;			(substring default-directory baselen)
			 ;;			err)
			 (oset fle init-state "Error thrown during buffer reset.")
			 (oset fle init-opt-data err)
			 (push fle errlog)
			 ))

		      (let ((proj2 ede-object-root-project))

			(if (not proj2)
			    (oset fle doubledetect-state "Failed to load project.")

			  (if (not (eq proj proj2))
			      (progn
				(oset fle doubledetect-state "Double Scan created new project!")
				)
			    (oset fle doubledetect-state t)

			    ;; Project Rescan Tests: Only if project detection worked.
			    (ede-detect-utest-rescan proj fle)
			    )))
		      )

		  ;; Else test failed.
		  (oset fle init-state "Failed Testing predicate")
		  (oset fle init-opt-data (eieio-object-name proj))
		  ;;(cedet-utest-log  "\n!! In %s: Found %S, wanted %S\n" (substring default-directory baselen) (eieio-object-name proj) (eieio-object-name fle))

		  (push fle errlog))

		)))

	  ;; If it wasn't already in memory, whack it.
	  (when (and b (not fb))
	    (kill-buffer b))
	  ))

      ;; Dump out a table of our results.
      (ede-detect-show-state 'header)
      (dolist (fle test-entries)
	(ede-detect-show-state fle))
      (cedet-utest-log ".")

      ;; Do a final sanity check.
      (ede-global-list-sanity-check)

      ))

  )

(defun ede-detect-utest-rescan (proj fle)
  "Test the found project PROJ to make sure 'rescan' works.
Things to look for:
 * No errors.
 * If original PROJ and resultant PROJ are different, make sure both
   aren't in the master list.
 * Make sure they are roughly the same."
  (let ((origproj proj)
	(midproj ede-object-root-project)
	(newproj nil))

    (when (not (eq origproj midproj))
      (error "Strange problem initiating rescan test."))

    (when (oref fle dorescan)

      ;; Run the classic EDE rescan function.
      (condition-case err
	  (ede-rescan-toplevel)
	(error
	 ;;(cedet-utest-log "\n!! In %s: rescan threw error %S\n"
	 ;;		  (substring default-directory baselen)
	 ;;		  err)
	 (oset fle rescan-state "Rescan threw an error")
	 (oset fle rescan-opt-data err)
	 (push fle errlog)))

      ;; Get the resultant new root project.
      (setq newproj ede-object-root-project)

      ;; Some projects do a full recreate.
      (when (not (eq newproj proj))

	;; This says the test didn't want this project to be replaced...
	(when (not (oref fle rescan))
	  (oset fle rescan-state "Rescan resulted in replaced project: Not expected.")
	  (push fle errlog))

	(when (not (string= (oref proj :directory) (oref newproj :directory)))
	  ;;(cedet-utest-log "\n!! In %s: Rescanned project directory missmatch: Was %s, now %s\n"
	  ;;			 (substring default-directory baselen)
	  ;;			 (oref proj :directory) (oref newproj :directory))
	  (oset fle rescan-state "Rescan project directory missmatch")
	  (oset fle rescan-opt-data (cons (oref proj :directory) (oref newproj :directory)))
	  (push fle errlog))

	(when (memq proj ede-projects)
	  ;;(cedet-utest-log "\n!! In %s: Rescanned project left old project in master project list!\n"
	  ;;			 (substring default-directory baselen))
	  (oset fle rescan-state "Rescan left old project in master project list.")
	  (push fle errlog))

	)

      (when (not (oref fle rescan-state))
	(oset fle rescan-state t))

      ;;(cedet-utest-log "** In %s: rescan ... Done" (substring default-directory baselen))
      nil)))


(defun ede-detect-utest-validate-loadstate (loadedp)
  "Detect all entries in `ede-detect-utest-project-dirmatch-entries' for load state.
If LOADEDP is t, make sure they were all loaded.
If LOADEDP is nil, make sure non were loaded."
  (dolist (fle ede-detect-utest-project-dirmatch-entries)

    (let ((feature (when (slot-boundp fle 'feature) (oref fle feature))))

      ;; Items without features are not asking to be tested.
      (when feature

	(if loadedp

	    ;; Note Loaded Test
	    (if (featurep feature)
		(cedet-utest-log "** Successfully loaded %s project."
				       feature)
	      (cedet-utest-log "!! Project type %s was not loaded as expected."
				     feature)
	      (push fle errlog))

	  ;; ELSE make sure it wasn't loaded
	  (if (featurep feature)
	      (progn
		(cedet-utest-log "!! Project type %s loaded unexpectedly."
				       feature)
		(push fle errlog))
	    (cedet-utest-log "** Successfully did not load %s project."
				   feature))
	  )))))

;;; Custom Predicates
;;
;; These predicates are for projects where we want to make a few extra tests
;; on some of the projects.
;;
(defun ede-detect-utest-subproj-p (project)
  "Special predicate for testing the ede-proj-project type."
  (and (ede-proj-project-p project)
       (string= (file-name-nondirectory (directory-file-name (oref project directory))) "proj")
       (not (eq project (ede-current-project)))
       ))

(defun ede-detect-utest-subautomake-p (project)
  "Special predicate for testing the ede-proj-project type."
  (and (project-am-makefile-p project)
       (string= (file-name-nondirectory (directory-file-name (oref project directory))) "automake")
       (not (eq project (ede-current-project)))
       ))

(defun ede-detect-utest-generic-p (project)
  "Special predicate for testing that a generic project was loaded."
  (and (ede-generic-project-child-p project)
       ;; This part also validates that generic projects can load in their
       ;; configuration, and that we get the correct value from that configuration.
       (let ((config (oref project config)))
	 (and config
	      (oref config c-preprocessor-table)
	      (string= "TEST" (car (car (oref config c-preprocessor-table))))
	      ))
       ))

(defun ede-detect-utest-generic-vc-p (project)
  "Special predicate for testing a generic VC project was loaded."
  (and (ede-detect-utest-generic-p project)
       (ede-generic-vc-project-p project)))

;;; TEST PROJECT
;;
;; This project exists to test dirmatch.

(defvar ede-detect-utest-dirmatch-fname
  (expand-file-name (concat (make-temp-name "utest-dirmatch-") ".txt")
		    temporary-file-directory)
  "A config file to use with DIRTEST.")

(defvar ede-detect-utest-arduino-fname
  (expand-file-name (concat (make-temp-name "utest-arduino-") ".txt")
		    temporary-file-directory)
  "A config file to use with detection of arduino.")

(defvar ede-detect-utest-arduino-install
  (expand-file-name "arduino" (ede-detect-utest-configdir))
  "A config file to use with detection of arduino.")

(defun ede-detect-utest-init-dirmatch ()
  "Init the config file for for dirtesting."

  ;; Setup the DIRMATCH project type.
  (let ((mypath (expand-file-name "dirmatch" (ede-detect-utest-basedir))))
    ;;(message "Dirmatch Location: %s" mypath)
    (save-excursion
      (set-buffer (semantic-find-file-noselect ede-detect-utest-dirmatch-fname))
      (erase-buffer)
      (insert "path=" mypath "\n")
      (save-buffer 0)
      ))

  ;; Override some bits of the ARDUINO project type.
  (setq ede-arduino-preferences-file ede-detect-utest-arduino-fname
	ede-arduino-appdir ede-detect-utest-arduino-install)


  (let ((mypath (expand-file-name "arduino" (ede-detect-utest-basedir))))
    ;;(message "Dirmatch Location: %s" mypath)
    (save-excursion
      (set-buffer (semantic-find-file-noselect ede-detect-utest-arduino-fname))
      (erase-buffer)
      (insert "sketchbook.path=" mypath "\n"
	      "serial.port=tty00\n"
	      "board=ede_utest\n")
      (save-buffer 0)
      ) )
  ;; Now we need to augment the existing autoloader for arduino.
  (let* ((arduinoauto (object-assoc 'ede/arduino :file
				    ede-project-class-files))
	 (adm (oref arduinoauto proj-root-dirmatch)))
    ;; Splice the new tmp pref file into the system.
    (oset adm :fromconfig ede-arduino-preferences-file)
    )

  )

(ede-add-project-autoload
 (ede-project-autoload "dirmatchtest"
		       :name "DIRMATCH TEST"
		       :file 'cedet/ede/detect-dirtest
		       :root-only nil
		       :proj-root-dirmatch
		       (ede-project-autoload-dirmatch
			"dirmatch test"
			:fromconfig ede-detect-utest-dirmatch-fname
			:configregex "^path=\\([^\n]+\\)$"
			:configregexidx 1)
		       :proj-file nil
		       :load-type 'ede-dirmatch-load
		       :class-sym 'ede-test-dirmatch-project
		       :safe-p t)
 )

;; (message "AUTOLOADS: %S" ede-project-class-files)

(provide 'cedet/ede/detect-utest)

;;; detect.el ends here
