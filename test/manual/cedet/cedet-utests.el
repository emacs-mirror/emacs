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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Remembering to run all the unit tests available in CEDET one at a
;; time is a bit time consuming.  This links all the tests together
;; into one command.

(require 'cedet)
;;; Code:
(defvar cedet-utest-test-alist
  '(
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
    ("eieio" . (lambda () (let ((lib (locate-library "eieio-tests.el"
						     t)))
			    (load-file lib))))
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

    ;; @todo - Currently handled in the integration tests.  Need
    ;;         some simpler unit tests here.

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
    ("srecode: fields" . srecode-field-utest)
    ("srecode: templates" . srecode-utest-template-output)
    ("srecode: show maps" . srecode-get-maps)
    ("srecode: getset" . srecode-utest-getset-output)
   )
  "Alist of all the tests in CEDET we should run.")

(defvar cedet-running-master-tests nil
  "Non-nil when CEDET-utest is running all the tests.")

(defun cedet-utest (&optional exit-on-error)
  "Run the CEDET unit tests.
EXIT-ON-ERROR causes the test suite to exit on an error, instead
of just logging the error."
  (interactive)
  (if (or (not (featurep 'semanticdb-mode))
	  (not (semanticdb-minor-mode-p)))
      (error "CEDET Tests require: M-x semantic-load-enable-minimum-features"))
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
    (cedet-utest-log-shutdown-msg "ALL TESTS" start end)
    nil))

(defun cedet-utest-noninteractive ()
  "Return non-nil if running non-interactively."
  (if (featurep 'xemacs)
      (noninteractive)
    noninteractive))

;;;###autoload
(defun cedet-utest-batch ()
  "Run the CEDET unit test in BATCH mode."
  (unless (cedet-utest-noninteractive)
    (error "`cedet-utest-batch' is to be used only with -batch"))
  (condition-case err
      (when (catch 'cedet-utest-exit-on-error
	      ;; Get basic semantic features up.
	      (semantic-load-enable-minimum-features)
	      ;; Disables all caches related to semantic DB so all
	      ;; tests run as if we have bootstrapped CEDET for the
	      ;; first time.
	      (setq-default semanticdb-new-database-class 'semanticdb-project-database)
	      (message "Disabling existing Semantic Database Caches.")

	      ;; Disabling the srecoder map, we won't load a pre-existing one
	      ;; and will be forced to bootstrap a new one.
	      (setq srecode-map-save-file nil)

	      ;; Run the tests
	      (cedet-utest t)
	      )
	(kill-emacs 1))
    (error
     (error "Error in unit test harness:\n  %S" err))
    )
  )

;;; Logging utility.
;;
(defvar cedet-utest-frame nil
  "Frame used during cedet unit test logging.")
(defvar cedet-utest-buffer nil
  "Frame used during cedet unit test logging.")
(defvar cedet-utest-frame-parameters
  '((name . "CEDET-UTEST")
    (width . 80)
    (height . 25)
    (minibuffer . t))
  "Frame parameters used for the cedet utest log frame.")

(defvar cedet-utest-last-log-item nil
  "Remember the last item we were logging for.")

(defvar cedet-utest-log-timer nil
  "During a test, track the start time.")

(defun cedet-utest-log-setup (&optional title)
  "Setup a frame and buffer for unit testing.
Optional argument TITLE is the title of this testing session."
  (setq cedet-utest-log-timer (current-time))
  (if (cedet-utest-noninteractive)
      (message "\n>> Setting up %s tests to run @ %s\n"
	       (or title "")
	       (current-time-string))

    ;; Interactive mode needs a frame and buffer.
    (when (or (not cedet-utest-frame) (not (frame-live-p cedet-utest-frame)))
      (setq cedet-utest-frame (make-frame cedet-utest-frame-parameters)))
    (when (or (not cedet-utest-buffer) (not (buffer-live-p cedet-utest-buffer)))
      (setq cedet-utest-buffer (get-buffer-create "*CEDET utest log*")))
    (save-excursion
      (set-buffer cedet-utest-buffer)
      (setq cedet-utest-last-log-item nil)
      (when (not cedet-running-master-tests)
	(erase-buffer))
      (insert "\n\nSetting up "
	      (or title "")
	      " tests to run @ " (current-time-string) "\n\n"))
    (let ((oframe (selected-frame)))
      (unwind-protect
	  (progn
	    (select-frame cedet-utest-frame)
	    (switch-to-buffer cedet-utest-buffer t))
	(select-frame oframe)))
    ))

(defun cedet-utest-elapsed-time (start end)
  "Copied from elp.el.  Was elp-elapsed-time.
Argument START and END bound the time being calculated."
  (+ (* (- (car end) (car start)) 65536.0)
     (- (car (cdr end)) (car (cdr start)))
     (/ (- (car (cdr (cdr end))) (car (cdr (cdr start)))) 1000000.0)))

(defun cedet-utest-log-shutdown (title &optional errorcondition)
  "Shut-down a larger test suite.
TITLE is the section that is done.
ERRORCONDITION is some error that may have occurred during testing."
  (let ((endtime (current-time))
	)
    (cedet-utest-log-shutdown-msg title cedet-utest-log-timer endtime)
    (setq cedet-utest-log-timer nil)
    ))

(defun cedet-utest-log-shutdown-msg (title startime endtime)
  "Show a shutdown message with TITLE, STARTIME, and ENDTIME."
  (if (cedet-utest-noninteractive)
      (progn
	(message "\n>> Test Suite %s ended at @ %s"
		 title
		 (format-time-string "%c" endtime))
	(message "     Elapsed Time %.2f Seconds\n"
		 (cedet-utest-elapsed-time startime endtime)))

    (save-excursion
      (set-buffer cedet-utest-buffer)
      (goto-char (point-max))
      (insert "\n>> Test Suite " title " ended at @ "
	      (format-time-string "%c" endtime) "\n"
	      "     Elapsed Time "
	      (number-to-string
	       (cedet-utest-elapsed-time startime endtime))
	      " Seconds\n * "))
    ))

(defun cedet-utest-show-log-end ()
  "Show the end of the current unit test log."
  (unless (cedet-utest-noninteractive)
    (let* ((cb (current-buffer))
	   (cf (selected-frame))
	   (bw (or (get-buffer-window cedet-utest-buffer t)
		   (get-buffer-window (switch-to-buffer cedet-utest-buffer) t)))
	   (lf (window-frame bw))
	   )
      (select-frame lf)
      (select-window bw)
      (goto-char (point-max))
      (select-frame cf)
      (set-buffer cb)
      )))

(defun cedet-utest-post-command-hook ()
  "Hook run after the current log command was run."
    (if (cedet-utest-noninteractive)
	(message "")
      (save-excursion
	(set-buffer cedet-utest-buffer)
	(goto-char (point-max))
	(insert "\n\n")))
    (setq cedet-utest-last-log-item nil)
    (remove-hook 'post-command-hook 'cedet-utest-post-command-hook)
    )

(defun cedet-utest-add-log-item-start (item)
  "Add ITEM into the log as being started."
  (unless (equal item cedet-utest-last-log-item)
    (setq cedet-utest-last-log-item item)
    ;; This next line makes sure we clear out status during logging.
    (add-hook 'post-command-hook 'cedet-utest-post-command-hook)

    (if (cedet-utest-noninteractive)
	(message " - Running %s ..." item)
      (save-excursion
	(set-buffer cedet-utest-buffer)
	(goto-char (point-max))
	(when (not (bolp)) (insert "\n"))
	(insert "Running " item " ... ")
	(sit-for 0)
	))
    (cedet-utest-show-log-end)
    ))

(defun cedet-utest-add-log-item-done (&optional notes err precr)
  "Add into the log that the last item is done.
Apply NOTES to the doneness of the log.
Apply ERR if there was an error in previous item.
Optional argument PRECR indicates to prefix the done msg w/ a newline."
  (if (cedet-utest-noninteractive)
      ;; Non-interactive-mode - show a message.
      (if notes
	  (message "   * %s {%s}" (or err "done") notes)
	(message "   * %s" (or err "done")))
    ;; Interactive-mode - insert into the buffer.
    (save-excursion
      (set-buffer cedet-utest-buffer)
      (goto-char (point-max))
      (when precr (insert "\n"))
      (if err
	  (insert err)
	(insert "done")
	(when notes (insert " (" notes ")")))
      (insert "\n")
      (setq cedet-utest-last-log-item nil)
      (sit-for 0)
      )))

;;; INDIVIDUAL TEST API
;;
;; Use these APIs to start and log information.
;;
;; The other fcns will be used to log across all the tests at once.
(defun cedet-utest-log-start (testname)
  "Setup the log for the test TESTNAME."
  ;; Make sure we have a log buffer.
  (save-window-excursion
    (when (or (not cedet-utest-buffer)
	      (not (buffer-live-p cedet-utest-buffer))
	      (not (get-buffer-window cedet-utest-buffer t))
	      )
      (cedet-utest-log-setup))
    ;; Add our startup message.
    (cedet-utest-add-log-item-start testname)
    ))

(defun cedet-utest-log(format &rest args)
  "Log the text string FORMAT.
The rest of the ARGS are used to fill in FORMAT with `format'."
  (if (cedet-utest-noninteractive)
      (apply 'message format args)
    (save-excursion
      (set-buffer cedet-utest-buffer)
      (goto-char (point-max))
      (when (not (bolp)) (insert "\n"))
      (insert (apply 'format format args))
      (insert "\n")
      (sit-for 0)
      ))
  (cedet-utest-show-log-end)
  )

;;; Inversion tests

(defun inversion-unit-test ()
  "Test inversion to make sure it can identify different version strings."
  (interactive)
  (let ((c1 (inversion-package-version 'inversion))
	(c1i (inversion-package-incompatibility-version 'inversion))
	(c2 (inversion-decode-version  "1.3alpha2"))
	(c3 (inversion-decode-version  "1.3beta4"))
	(c4 (inversion-decode-version  "1.3 beta5"))
	(c5 (inversion-decode-version  "1.3.4"))
	(c6 (inversion-decode-version  "2.3alpha"))
	(c7 (inversion-decode-version  "1.3"))
	(c8 (inversion-decode-version  "1.3pre1"))
	(c9 (inversion-decode-version  "2.4 (patch 2)"))
	(c10 (inversion-decode-version "2.4 (patch 3)"))
	(c11 (inversion-decode-version "2.4.2.1"))
	(c12 (inversion-decode-version "2.4.2.2"))
	)
    (if (not (and
	      (inversion-= c1 c1)
	      (inversion-< c1i c1)
	      (inversion-< c2 c3)
	      (inversion-< c3 c4)
	      (inversion-< c4 c5)
	      (inversion-< c5 c6)
	      (inversion-< c2 c4)
	      (inversion-< c2 c5)
	      (inversion-< c2 c6)
	      (inversion-< c3 c5)
	      (inversion-< c3 c6)
	      (inversion-< c7 c6)
	      (inversion-< c4 c7)
	      (inversion-< c2 c7)
	      (inversion-< c8 c6)
	      (inversion-< c8 c7)
	      (inversion-< c4 c8)
	      (inversion-< c2 c8)
	      (inversion-< c9 c10)
	      (inversion-< c10 c11)
	      (inversion-< c11 c12)
	      ;; Negatives
	      (not (inversion-< c3 c2))
	      (not (inversion-< c4 c3))
	      (not (inversion-< c5 c4))
	      (not (inversion-< c6 c5))
	      (not (inversion-< c7 c2))
	      (not (inversion-< c7 c8))
	      (not (inversion-< c12 c11))
	      ;; Test the tester on inversion
	      (not (inversion-test 'inversion inversion-version))
	      ;; Test that we throw an error
	      (inversion-test 'inversion "0.0.0")
	      (inversion-test 'inversion "1000.0")
	      ))
	(error "Inversion tests failed")
      (message "Inversion tests passed."))))

;;; cedet-files unit test

(defvar cedet-files-utest-list
  '(
    ( "/home/me/src/myproj/src/foo.c" . "!home!me!src!myproj!src!foo.c" )
    ( "c:/work/myproj/foo.el" . "!drive_c!work!myproj!foo.el" )
    ( "//windows/proj/foo.java" . "!!windows!proj!foo.java" )
    ( "/home/me/proj!bang/foo.c" . "!home!me!proj!!bang!foo.c" )
    )
  "List of different file names to test.
Each entry is a cons cell of ( FNAME . CONVERTED )
where FNAME is some file name, and CONVERTED is what it should be
converted into.")

(defun cedet-files-utest ()
  "Test out some file name conversions."
  (interactive)
  (let ((idx 0))
    (dolist (FT cedet-files-utest-list)

      (setq idx (+ idx 1))

      (let ((dir->file (cedet-directory-name-to-file-name (car FT) t))
	    (file->dir (cedet-file-name-to-directory-name (cdr FT) t))
	    )

	(unless (string= (cdr FT) dir->file)
	  (error "Failed: %d.  Found: %S Wanted: %S"
		 idx dir->file (cdr FT))
	  )

	(unless (string= file->dir (car FT))
	  (error "Failed: %d.  Found: %S Wanted: %S"
		 idx file->dir (car FT)))))))

;;; pulse test

(defun pulse-test (&optional no-error)
  "Test the lightening function for pulsing a line.
When optional NO-ERROR don't throw an error if we can't run tests."
  (interactive)
  (if (or (not pulse-flag) (not (pulse-available-p)))
      (if no-error
	  nil
	(error (concat "Pulse test only works on versions of Emacs"
		       " that support pulsing")))
    ;; Run the tests
    (when (interactive-p)
      (message "<Press a key> Pulse one line.")
      (read-char))
    (pulse-momentary-highlight-one-line (point))
    (when (interactive-p)
      (message "<Press a key> Pulse a region.")
      (read-char))
    (pulse-momentary-highlight-region (point)
				      (save-excursion
					(condition-case nil
					    (forward-char 30)
					  (error nil))
					(point)))
    (when (interactive-p)
      (message "<Press a key> Pulse line a specific color.")
      (read-char))
    (pulse-momentary-highlight-one-line (point) 'modeline)
    (when (interactive-p)
      (message "<Press a key> Pulse a pre-existing overlay.")
      (read-char))
    (let* ((start (point-at-bol))
	   (end (save-excursion
		  (end-of-line)
		  (when (not (eobp))
		    (forward-char 1))
		  (point)))
	   (o (make-overlay start end))
	   )
      (pulse-momentary-highlight-overlay o)
      (if (overlay-buffer o)
	  (delete-overlay o)
	(error "Non-temporary overlay was deleted!"))
      )
    (when (interactive-p)
      (message "Done!"))))

(provide 'cedet-utests)

;;; cedet-utests.el ends here
