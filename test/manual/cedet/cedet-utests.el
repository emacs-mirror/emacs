;;; cedet-utests.el --- Run all unit tests in the CEDET suite.  -*- lexical-binding: t; -*-

;; Copyright (C) 2008-2026 Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <zappo@gnu.org>

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

(defvar cedet-utest-directory
  (let* ((C (file-name-directory (locate-library "cedet")))
         (D (expand-file-name "../../test/manual/cedet/" C)))
    D)
  "Location of test files for this test suite.")

(defvar cedet-utest-libs '("ede-tests"
                           "semantic-tests"
                           )
  "List of test srcs that need to be loaded.")

;;; Code:
(defvar cedet-utest-test-alist
  '(
    ;;
    ;; COMMON
    ;;

    ;; Test inversion
    ;; ("inversion" . inversion-unit-test) ; moved to automated suite

    ;; EZ Image dumping.
    ("ezimage associations" . ezimage-image-association-dump)
    ("ezimage images" . (lambda ()
                          (ezimage-image-dump)
                          (kill-buffer "*Ezimage Images*")))

    ;; Pulse
    ("pulse interactive test" . (lambda () (pulse-test t)))

    ;; Files
    ;; ("cedet file conversion" . cedet-files-utest) ; moved to automated suite

    ;;
    ;; EIEIO
    ;;

    ("eieio: browser" . (lambda ()
                          (eieio-browse)
                          (kill-buffer "*EIEIO OBJECT BROWSE*")))
    ("eieio: custom" . (lambda ()
			 (require 'eieio-custom)
			 (customize-variable 'eieio-widget-test)
                         (kill-buffer "*Customize Option: Eieio Widget Test*")
                         ))
    ("eieio: chart" . (lambda ()
                        (require 'chart)
			(if noninteractive
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
    ;;("semantic: multi-lang parsing" . semantic-utest-main)
    ;;("semantic: C preprocessor" . semantic-utest-c) - Now in automated suite
    ;;("semantic: analyzer tests" . semantic-ia-utest)
    ("semanticdb: data cache" . semantic-test-data-cache)
    ("semantic: throw-on-input" .
     (lambda ()
       (if noninteractive
	   (message " ** Skipping test in noninteractive mode.")
	 (semantic-test-throw-on-input))))

    ;;("semantic: gcc: output parse test" . semantic-gcc-test-output-parser)  ; moved to automated suite

    ;;
    ;; SRECODE
    ;;

    ;; TODO - fix the fields test
    ;;("srecode: fields" . srecode-field-utest)  ; moved to automated suite
    ;;("srecode: templates" . srecode-utest-template-output)
    ("srecode: show maps" . srecode-get-maps)
    ;;("srecode: getset" . srecode-utest-getset-output)
   )
  "Alist of all the tests in CEDET we should run.")

(defvar cedet-running-master-tests nil
  "Non-nil when CEDET-utest is running all the tests.")

(defun cedet-utest (&optional exit-on-error)
  "Run the CEDET unit tests.
EXIT-ON-ERROR causes the test suite to exit on an error, instead
of just logging the error."
  (interactive)
  (unless (and (fboundp 'semanticdb-minor-mode-p)
               (semanticdb-minor-mode-p))
    (error "CEDET Tests require semantic-mode to be enabled"))
  (dolist (L cedet-utest-libs)
    (load-file (expand-file-name (concat L ".el") cedet-utest-directory)))
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
      (when (and (called-interactively-p 'interactive) (input-pending-p))
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
  (declare (obsolete nil "27.1"))
  noninteractive)

(defvar srecode-map-save-file)

;;;###autoload
(defun cedet-utest-batch ()
  "Run the CEDET unit test in BATCH mode."
  (unless noninteractive
    (error "`cedet-utest-batch' is to be used only with -batch"))
  (condition-case err
      (when (catch 'cedet-utest-exit-on-error
	      ;; Get basic semantic features up.
	      ;; FIXME: I can't see any such function in our code!
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
  (if noninteractive
      (message "\n>> Setting up %s tests to run @ %s\n"
	       (or title "")
	       (current-time-string))

    ;; Interactive mode needs a frame and buffer.
    (when (or (not cedet-utest-frame) (not (frame-live-p cedet-utest-frame)))
      (setq cedet-utest-frame (make-frame cedet-utest-frame-parameters)))
    (when (or (not cedet-utest-buffer) (not (buffer-live-p cedet-utest-buffer)))
      (setq cedet-utest-buffer (get-buffer-create "*CEDET utest log*")))
    (with-current-buffer cedet-utest-buffer
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
  (float-time (time-subtract start end)))

(defun cedet-utest-log-shutdown (title &optional _errorcondition)
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
  (if noninteractive
      (progn
	(message "\n>> Test Suite %s ended at @ %s"
		 title
		 (format-time-string "%c" endtime))
	(message "     Elapsed Time %.2f Seconds\n"
		 (cedet-utest-elapsed-time startime endtime)))

    (with-current-buffer cedet-utest-buffer
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
  (unless noninteractive
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
    (if noninteractive
	(message "")
      (with-current-buffer cedet-utest-buffer
	(goto-char (point-max))
	(insert "\n\n")))
    (setq cedet-utest-last-log-item nil)
    (remove-hook 'post-command-hook #'cedet-utest-post-command-hook)
    )

(defun cedet-utest-add-log-item-start (item)
  "Add ITEM into the log as being started."
  (unless (equal item cedet-utest-last-log-item)
    (setq cedet-utest-last-log-item item)
    ;; This next line makes sure we clear out status during logging.
    (add-hook 'post-command-hook #'cedet-utest-post-command-hook)

    (if noninteractive
	(message " - Running %s ..." item)
      (with-current-buffer cedet-utest-buffer
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
Optional argument PRECR indicates to prefix the done message with
a newline."
  (if noninteractive
      ;; Non-interactive-mode - show a message.
      (if notes
	  (message "   * %s {%s}" (or err "done") notes)
	(message "   * %s" (or err "done")))
    ;; Interactive-mode - insert into the buffer.
    (with-current-buffer cedet-utest-buffer
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

(defun cedet-utest-log (format &rest args)
  "Log the text string FORMAT.
The rest of the ARGS are used to fill in FORMAT with `format'."
  (if noninteractive
      (apply #'message format args)
    (with-current-buffer cedet-utest-buffer
      (goto-char (point-max))
      (when (not (bolp)) (insert "\n"))
      (insert (apply #'format format args))
      (insert "\n")
      (sit-for 0)
      ))
  (cedet-utest-show-log-end)
  )

;;; pulse test

(defun pulse-test (&optional no-error)
  "Test the lightening function for pulsing a line.
When optional NO-ERROR don't throw an error if we can't run tests."
  (interactive)
  (if (not (and (bound-and-true-p pulse-flag)
                (fboundp 'pulse-available-p)
                (pulse-available-p)))
      (if no-error
	  nil
	(error (concat "Pulse test only works on versions of Emacs"
		       " that support pulsing")))
    (declare-function pulse-momentary-highlight-overlay
                      "pulse.el" (o &optional face))
    ;; Run the tests
    (when (called-interactively-p 'interactive)
      (message "<Press a key> Pulse one line.")
      (read-char))
    (pulse-momentary-highlight-one-line (point))
    (when (called-interactively-p 'interactive)
      (message "<Press a key> Pulse a region.")
      (read-char))
    (pulse-momentary-highlight-region (point)
				      (save-excursion
					(condition-case nil
					    (forward-char 30)
					  (error nil))
					(point)))
    (when (called-interactively-p 'interactive)
      (message "<Press a key> Pulse line a specific color.")
      (read-char))
    (pulse-momentary-highlight-one-line (point) 'mode-line)
    (when (called-interactively-p 'interactive)
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
    (when (called-interactively-p 'interactive)
      (message "Done!"))))

(provide 'cedet-utests)

;;; cedet-utests.el ends here
