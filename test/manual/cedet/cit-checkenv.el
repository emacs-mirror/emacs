;;; cit-checkenv.el --- Check this computer's environment for needed tools.
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
;; Lots of build tools are needed to run the CEDET Integration tests.
;; This command will summarize the requirements.

;;; Code:

(require 'eieio)

(require 'cedet-global)
(require 'cedet-idutils)
(require 'cedet-cscope)
(require 'cedet-java)

(defclass cit-checkenv-test (eieio-named)
  ((testgroup :initarg :testgroup
	      :initform nil)
   (test :initarg :test
	 :documentation
	 "A function, or a string representing a program to run.")
   (result :initarg :result
	   :documentation
	   "The expected result.
If :test is a function then this is the expected output, compare w/ eq.
If :test is a string, then this is a version string to look for.")
   (required :initarg :required
	     :initform t)
   )
  "A class representing a thing to check.")

(defmethod cit-checkenv-dotest ((test cit-checkenv-test) testgroup)
  "Execute the TEST."
  (when (let ((tg (oref test testgroup)))
	  (or (not testgroup) (not tg)
	    (if (stringp tg)
		(string= testgroup tg)
	      (member testgroup tg))))

    (let ((result nil))
      (cond ((functionp (oref test test))
	     ;; Run SYMBOL
	     (let ((found (funcall (oref test test)))
		   (expect (oref test result)))
	       (if (eq found expect)
		   (cit-checkenv-ok test)
		 (cit-checkenv-error test "Test failed: Found %s, expected %s"
				     found expect))))

	    ((and (stringp (oref test test)) (not (slot-boundp test 'result)))
	     ;; Just check it is on the exec path.
	     (if (locate-file (oref test test) exec-path)
		 (cit-checkenv-ok test)
	       (cit-checkenv-error test "%s not found on exec-path."
				   (oref test test)))
	     )
	    (t
	     (error "Unknown thing to check: %S" (oref test test))))
      )))

(defmethod cit-checkenv-ok ((this cit-checkenv-test))
  "Note that test THIS is OK."
  (message "OK: %S" (oref this :object-name)))

(defmethod cit-checkenv-error ((this cit-checkenv-test) formatmsg &rest args)
  "Issue an error (ie - test failed) for test THIS."
  (let* ((req (oref this required))
	 (msg (concat
	       (if req "Error" "Warning")
	       (format " (%s): " (oref this :object-name))
	       (apply 'format formatmsg args))))
    (if req
	(error msg)
      (message msg))))

(defvar cit-checkenv-checks
  (list
   (cit-checkenv-test "global"
		      :testgroup '("Make" "Automake" "globalref")
		      :test (lambda () (cedet-gnu-global-version-check t))
		      :result t)
   (cit-checkenv-test "idutils"
		      :testgroup '("Make" "Automake")
		      :test (lambda () (cedet-idutils-version-check t))
		      :result t
		      :required nil)
   (cit-checkenv-test "cscope"
		      :testgroup '("Make" "Automake")
		      :test (lambda () (cedet-cscope-version-check t))
		      :result t)
   ;; Make tooling
   (cit-checkenv-test "make" :testgroup '("Make" "Automake" "Arduino") :test "make")
   ;; Automake tooling.
   (cit-checkenv-test "automake" :testgroup "Automake" :test "automake")
   (cit-checkenv-test "libtool" :testgroup "Automake" :test "libtool")
   ;; C and CPP stuff
   (cit-checkenv-test "cpp" :testgroup '("Make" "Automake" "cpp") :test "cpp")
   ;; Texinfo
   (cit-checkenv-test "texinfo" :testgroup '("Make" "Automake") :test "makeinfo")
   ;; Java stuff
   (cit-checkenv-test "java"
		      :testgroup "Java"
		      :test (lambda () (cedet-java-version-check t))
		      :result t)
   (cit-checkenv-test "jar" :testgroup "Java" :test "jar")
   (cit-checkenv-test "javap" :testgroup "Java" :test "javap")
   ;; arduino
   (cit-checkenv-test "arduino" :testgroup "Arduino"
		      :test (lambda ()
			      (require 'ede/arduino)
			      (stringp (ede-arduino-find-install)))
		      :result t)
   (cit-checkenv-test "arduino-mk" :testgroup "Arduino"
		      :test (lambda ()
			      (require 'ede/arduino)
			      (stringp (ede-arduino-Arduino.mk)))
		      :result t)
   ;; android
   (cit-checkenv-test "android-sdk" :testgroup "Android"
		      :test (lambda () (cedet-android-adb-version-check t))
		      :result t)
   (cit-checkenv-test "ant" :testgroup "Android" :test "ant") ;; may be moving off this soon.
    )
  "List of checks to perform to see if this system can run the tests.
Each entry is a `cit-checkenv-test' object.  See the doc for that
class for details.")

(defun cit-checkenv (testgroup)
  "Check this computer for the needed tools."
  (interactive "sTest Group: ")
  (message "\n** Environment testing:
   Checking your environment for the tools needed to test CEDET.\n")
  (mapc (lambda (check) (cit-checkenv-dotest check testgroup)) cit-checkenv-checks)

  (message "** Environment Tests complete\n")
  )


(provide 'cit-checkenv)

;;; cit-checkenv.el ends here
