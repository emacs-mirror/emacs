;;; ede/detect.el --- Tests for detecting different kinds of projects.
;;
;; Copyright (C) 2014 Eric M. Ludlam
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

(defvar ede-detect-utest-project-list
  '(
    ( "src/proj/sub/TEST" . ede-detect-utest-subproj-p )
    ( "src/proj/Project.ede" . ede-proj-project-p )
    ( "src/automake/sub/Makefile.am" . ede-detect-utest-subautomake-p)
    ( "src/automake/Makefile.am" . project-am-makefile-p )
    ( "src/android/AndroidManifest.xml" . ede-android-project-p )
    ( "src/android/src/test.java" . ede-android-project-p )
    ( "src/emacs/Makefile" . ede-emacs-project-p )
    ( "src/emacs/src/emacs.c" . ede-emacs-project-p )
    ;( "src/ant/build.xml" . ede-ant-project-p )
    ( "src/linux/Makefile" . ede-linux-project-p )
    ( "src/linux/scripts/ver_linux" . ede-linux-project-p )
    ;; these ROOT projects are created by hand in a .emacs file.
    ;; These need to be defined in here to get this test to work.
    ( "src/cpproot/src/main.cpp" . ede-cpp-root-project-p )
    ( "src/cpproot/README" . ede-cpp-root-project-p )
    ( "src/javaroot/com/test/Foo.Java" . ede-java-root-project-p )
    ( "src/javaroot/README" . ede-java-root-project-p )
     )
  "List of sources to load in ndetectable projects.
Each entry is a cons cell:
  ( SRCFILE . PROJECT-TYPE )")

(defun ede-detect-utest-basedir ()
  "Get the basedir of the detection unit tests."
  (save-current-buffer
    (set-buffer (semantic-find-file-noselect
		 (expand-file-name "cedet/ede/detect.el"
				   cedet-utest-root)))
    (expand-file-name "src" default-directory)))

(ede-cpp-root-project "UTESTCPP"
		      :file (expand-file-name "cpproot/README"
					      (ede-detect-utest-basedir)))

(ede-java-root-project "UTESTJAVA"
		       :file (expand-file-name "javaroot/README"
					       (ede-detect-utest-basedir)))

;;;###autoload
(defun ede-detect-utest ()
  "Test out the detection scheme for EDE."
  (interactive)
  (save-excursion

    (let ((errlog nil)
	  (project-linux-build-directory-default 'same)
	  (project-linux-architecture-default "glnx")
	  (ede-project-directories t) ; safe to load Project.ede
	  (basedir nil)
	  (baselen nil)
	  )
      (cedet-utest-log-setup "EDE DETECT")

      (set-buffer (semantic-find-file-noselect
		   (expand-file-name "cedet/ede/detect.el"
				     cedet-utest-root)))
      (setq basedir default-directory
	    baselen (length basedir))

      (dolist (fl ede-detect-utest-project-list)

	;; Make sure we have the files we think we have.
	(when (not (file-exists-p (car fl)))
	  (error "Cannot find unit test file: %s" (car fl)))

	;; Do the detection
	(let ((fb (find-buffer-visiting (car fl)))
	      (b (semantic-find-file-noselect (car fl))))

	  (save-excursion
	    (set-buffer b)

	    ;; Run the EDE detection code.  Firing up the mode isn't really needed.
	    (condition-case err
		(ede-initialize-state-current-buffer)
	      (error
	       (semantic-ia-utest-log "\n!! In %s: load threw error %S\n"
				      (substring default-directory baselen)
				      err)
	       (push fl errlog)
	       ))

	    (let* ((proj ede-object-root-project))

	      (if (not proj)
		  (progn

		    ;; Use the detector to to provide better debugging info.
		    (let ((projdetect (ede-detect-directory-for-project default-directory)))

		      (if (not projdetect)
			  (progn
			    ;; Detected nothing
			    (semantic-ia-utest-log  "\n!! In %s: Detected nothing, wanted %S\n"
						    (substring default-directory baselen)
						    (cdr fl))
			    (push fl errlog))

			;; Else, some other error.
			(semantic-ia-utest-log "\n!! In %s: Detected %S, failed to load project type %s\n"
						 (substring default-directory baselen)
						 (eieio-object-name (cdr projdetect))
						 (cdr fl))
			(push fl errlog))))

		;; Test the result.
		(if (funcall (cdr fl) proj)

		    (semantic-ia-utest-log "** In %s: Found %s ... Done"
					   (substring default-directory baselen)
					   (cdr fl))

		  (semantic-ia-utest-log  "\n!! In %s: Found %S, wanted %S\n"
					  (substring default-directory baselen)
					  (eieio-object-name proj)
					  (cdr fl))

		  (push fl errlog))
		)))

	  ;; If it wasn't already in memory, whack it.
	  (when (and b (not fb))
	    (kill-buffer b))
	  ))

	(cedet-utest-log-shutdown
       "EDE DETECT"
       (when errlog
	 (format "%s Failures found." (length errlog))))

      (when errlog
	(error "Failures found looking for project in %s" (car (car errlog))))
      ))

  )

(defun ede-detect-utest-subproj-p (project)
  "Special predicate for testing the ede-proj-project type."
  (and (ede-proj-project-p project)
       (string= (file-name-nondirectory (directory-file-name (oref project directory))) "proj")
       (not (eq project (ede-current-project)))
       ))

(defun ede-detect-utest-subautomake-p (project)
  "Special predicate for testing the ede-proj-project type."
  (and (project-am-makefile project)
       (string= (file-name-nondirectory (directory-file-name (oref project directory))) "automake")
       (not (eq project (ede-current-project)))
       ))

(provide 'cedet/ede/detect-utest)

;;; detect.el ends here
