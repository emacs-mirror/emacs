;;; cedet-utests.el --- Run all unit tests in the CEDET suite.

;; Copyright (C) 2008-2017 Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <eric@siege-engine.com>

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Remembering to run all the unit tests available in CEDET one at a
;; time is a bit time consuming.  This links all the tests together
;; into one command.

(require 'cedet)
(require 'cedet-uutil)
(require 'inversion-utest)
(require 'pulse-utest)
(require 'cedet-files-utests)
(require 'cedet-compat)
(require 'cedet/ede/detect-utest)
(require 'cedet/ede/secure-utest)
(require 'cedet/semantic/lex-utest)
(require 'cedet/semantic/lex-spp-utest)
(require 'cedet/semantic/utest-parse)
(require 'cedet/semantic/utest-c)
(require 'cedet/semantic/ia-utest)
(require 'cedet/semantic/utest-fw)
(require 'cedet/semantic/gcc-utest)
(require 'cedet/semantic/fmt-utest)
(require 'semantic/wisent/calc)
(require 'cedet/srecode/test)
(require 'cedet/srecode/fields-utest)
(require 'cedet/srecode/test-getset)
(require 'cedet/cogre/utest)
(require 'cedet/cogre/periodic-utest)
(require 'cedet/cogre/convert-utest)
(require 'chart)

;;; Code:
(defvar cedet-utest-test-alist
  '(
    ("cedet versions" . cedet-version)
    ;;
    ;; COMMON
    ;;

    ;; Test inversion
    ("inversion" . inversion-unit-test)

    ;; EZ Image dumping.
    ("ezimage associations" . ezimage-image-association-dump)
    ("ezimage images" . ezimage-image-dump)

    ;; Pulse
    ("pulse interactive test" . (lambda () (pulse-test t)))

    ;; Files
    ("cedet file conversion" . cedet-files-utest)

    ;;
    ;; EIEIO
    ;;
    ("eieio" . cedet-utest-eieio-classloader)
    ("eieio: browser" . eieio-browse)
    ("eieio: custom" . (lambda ()
			 (require 'eieio-custom)
			 (customize-variable 'eieio-widget-test)))
    ("eieio: chart" . (lambda ()
			(if (cedet-utest-noninteractive)
			    (message " ** Skipping test in noninteractive mode.")
			  (chart-test-it-all))))
    ;;
    ;; EDE
    ;;
    ("ede: project detection tests" . ede-detect-utest) ;; NOTE: must be before other EDE tests.
    ("ede: security tests" . ede-security-utest)

    ;;
    ;; SEMANTIC
    ;;
    ("semantic: lex spp table write" . semantic-lex-spp-write-utest)
    ("semantic: multi-lang parsing" . semantic-utest-main)
    ("semantic: C preprocessor" . semantic-utest-c)
    ("semantic: analyzer tests" . semantic-ia-utest)
    ("semanticdb: data cache" . semantic-test-data-cache)
    ("semantic: throw-on-input" .
     (lambda ()
       (if (cedet-utest-noninteractive)
	   (message " ** Skipping test in noninteractive mode.")
	 (semantic-test-throw-on-input))))

    ("semantic: gcc: output parse test" . semantic-gcc-test-output-parser)
    ;;
    ;; SRECODE
    ;;
    ("srecode: show maps" . srecode-utest-map-reset)
    ("srecode: templates" . srecode-utest-template-output)
    ("srecode: fields" . srecode-field-utest)
    ("srecode: project" . srecode-utest-project)
    ("srecode: getset" . srecode-utest-getset-output)
   )
  "Alist of all the tests in CEDET we should run.")

;;;###autoload
(defun cedet-utest (&optional exit-on-error)
  "Run the CEDET unit tests.
EXIT-ON-ERROR causes the test suite to exit on an error, instead
of just logging the error."
  (interactive)
  (if (or (not (featurep 'semantic/db-mode))
	  (not (semanticdb-minor-mode-p)))
      (error "CEDET Tests require: M-x semantic-mode"))
  (cedet-utest-log-setup "ALL TESTS")
  (let ((tl cedet-utest-test-alist)
	(notes nil)
	(err nil)
	(start (current-time))
	(end nil)
	(cedet-running-master-tests t)
	)
    (dolist (T tl)
      (cedet-utest-add-log-item-start (car T))
      (setq notes nil err nil)
      (condition-case Cerr
	  (progn
	    (funcall (cdr T))
	    )
	(error
	 (setq err (format "ERROR: %S" Cerr))
	 ;;(message "Error caught: %s" Cerr)
	 ))

      ;; Cleanup stray input and events that are in the way.
      ;; Not doing this causes sit-for to not refresh the screen.
      ;; Doing this causes the user to need to press keys more frequently.
      (when (and (interactive-p) (input-pending-p))
	(if (fboundp 'read-event)
	    (read-event)
	  (read-char)))

      (cedet-utest-add-log-item-done notes err)
      (when (and exit-on-error err)
	(message "to debug this test point, execute:")
	(message "%S" (cdr T))
	(message "\n ** Exiting Test Suite. ** \n")
	(throw 'cedet-utest-exit-on-error t)
	)
      )
    (setq end (current-time))
    ;; Run consistency checks.
    (cedet-utest-add-log-item-start "Sanity Checks.")
    (ede-global-list-sanity-check)
    (cedet-utest-add-log-item-done nil nil)
    ;; We passed!
    (cedet-utest-log-shutdown-msg "ALL TESTS" start end)
    nil))

;;;###autoload
(defun cedet-utest-batch ()
  "Run the CEDET unit test in BATCH mode."
  (unless (cedet-utest-noninteractive)
    (error "`cedet-utest-batch' is to be used only with -batch"))
  (condition-case err
      (when (catch 'cedet-utest-exit-on-error
	      ;; Get basic semantic features up.
              ;; OLD (semantic-load-enable-minimum-features)
              ;; NEW
              (semantic-mode 1)
              ;(global-semanticdb-minor-mode 1)
	      ;; Disables all caches related to semantic DB so all
	      ;; tests run as if we have bootstrapped CEDET for the
	      ;; first time.
	      (setq-default semanticdb-new-database-class 'semanticdb-project-database)
	      (message "Disabling existing Semantic Database Caches.")

	      ;; Disabling the srecoder map, we won't load a pre-existing one
	      ;; and will be forced to bootstrap a new one.
	      (setq srecode-map-save-file nil)

              ;; Disable saving EDE's cache file.
              (setq ede-project-placeholder-cache-file nil)

	      ;; Run the tests
	      (cedet-utest t)
	      )
	(kill-emacs 1))
    (error
     (error "Error in unit test harness:\n  %S" err))
    )
  )

;;; HELPER FUNCTIONS FOR SOME TESTS
(defun cedet-utest-eieio-classloader ()
  "Try out the EIEIO tests, which just requires loading the test file."
  (let ((lib (locate-library "eieio/eieio-tests.el" t)))
    (unless lib
      (error "Could not locate 'eieio/eieio-tests.el'"))
    (message "EIEIO Base tests loading from: %S" lib)
    (load-file lib)
    ))

(provide 'cedet-utests)

;;; cedet-utests.el ends here
