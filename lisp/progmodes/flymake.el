;;; flymake.el --- A universal on-the-fly syntax checker  -*- lexical-binding: t; -*-

;; Copyright (C) 2003-2017 Free Software Foundation, Inc.

;; Author:  Pavel Kobyakov <pk_at_work@yahoo.com>
;; Maintainer: Leo Liu <sdl.web@gmail.com>
;; Version: 0.3
;; Keywords: c languages tools

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
;; Flymake is a minor Emacs mode performing on-the-fly syntax checks.
;;
;; This file contains the UI for displaying and interacting with the
;; results of such checks, as well as entry points for backends to
;; hook on to. Backends are sources of diagnostic info.
;;
;;; Code:

(eval-when-compile (require 'cl-lib))

(defgroup flymake nil
  "Universal on-the-fly syntax checker."
  :version "23.1"
  :link '(custom-manual "(flymake) Top")
  :group 'tools)

(defcustom flymake-error-bitmap '(exclamation-mark error)
  "Bitmap (a symbol) used in the fringe for indicating errors.
The value may also be a list of two elements where the second
element specifies the face for the bitmap.  For possible bitmap
symbols, see `fringe-bitmaps'.  See also `flymake-warning-bitmap'.

The option `flymake-fringe-indicator-position' controls how and where
this is used."
  :group 'flymake
  :version "24.3"
  :type '(choice (symbol :tag "Bitmap")
                 (list :tag "Bitmap and face"
                       (symbol :tag "Bitmap")
                       (face :tag "Face"))))

(defcustom flymake-warning-bitmap 'question-mark
  "Bitmap (a symbol) used in the fringe for indicating warnings.
The value may also be a list of two elements where the second
element specifies the face for the bitmap.  For possible bitmap
symbols, see `fringe-bitmaps'.  See also `flymake-error-bitmap'.

The option `flymake-fringe-indicator-position' controls how and where
this is used."
  :group 'flymake
  :version "24.3"
  :type '(choice (symbol :tag "Bitmap")
                 (list :tag "Bitmap and face"
                       (symbol :tag "Bitmap")
                       (face :tag "Face"))))

(defcustom flymake-fringe-indicator-position 'left-fringe
  "The position to put flymake fringe indicator.
The value can be nil (do not use indicators), `left-fringe' or `right-fringe'.
See `flymake-error-bitmap' and `flymake-warning-bitmap'."
  :group 'flymake
  :version "24.3"
  :type '(choice (const left-fringe)
		 (const right-fringe)
		 (const :tag "No fringe indicators" nil)))

(defcustom flymake-start-syntax-check-on-newline t
  "Start syntax check if newline char was added/removed from the buffer."
  :group 'flymake
  :type 'boolean)

(defcustom flymake-no-changes-timeout 0.5
  "Time to wait after last change before starting compilation."
  :group 'flymake
  :type 'number)

(defcustom flymake-gui-warnings-enabled t
  "Enables/disables GUI warnings."
  :group 'flymake
  :type 'boolean)
(make-obsolete-variable 'flymake-gui-warnings-enabled
			"it no longer has any effect." "26.1")

(defcustom flymake-start-syntax-check-on-find-file t
  "Start syntax check on find file."
  :group 'flymake
  :type 'boolean)

(defcustom flymake-log-level -1
  "Logging level, only messages with level lower or equal will be logged.
-1 = NONE, 0 = ERROR, 1 = WARNING, 2 = INFO, 3 = DEBUG"
  :group 'flymake
  :type 'integer)

(defvar-local flymake-timer nil
  "Timer for starting syntax check.")

(defvar-local flymake-last-change-time nil
  "Time of last buffer change.")

(defvar-local flymake-check-start-time nil
  "Time at which syntax check was started.")

(defvar-local flymake-check-was-interrupted nil
  "Non-nil if syntax check was killed by `flymake-compile'.")

(defvar-local flymake-err-info nil
  "Sorted list of line numbers and lists of err info in the form (file, err-text).")

(defvar-local flymake-new-err-info nil
  "Same as `flymake-err-info', effective when a syntax check is in progress.")

(defun flymake-log (level text &rest args)
  "Log a message at level LEVEL.
If LEVEL is higher than `flymake-log-level', the message is
ignored.  Otherwise, it is printed using `message'.
TEXT is a format control string, and the remaining arguments ARGS
are the string substitutions (see the function `format')."
  (if (<= level flymake-log-level)
      (let* ((msg (apply #'format-message text args)))
	(message "%s" msg))))

(defun flymake-ins-after (list pos val)
  "Insert VAL into LIST after position POS.
POS counts from zero."
  (let ((tmp (copy-sequence list)))
    (setcdr (nthcdr pos tmp) (cons val (nthcdr (1+ pos) tmp)))
    tmp))

(defun flymake-set-at (list pos val)
  "Set VAL at position POS in LIST.
POS counts from zero."
  (let ((tmp (copy-sequence list)))
    (setcar (nthcdr pos tmp) val)
    tmp))

(defun flymake-er-make-er (line-no line-err-info-list)
  (list line-no line-err-info-list))

(defun flymake-er-get-line (err-info)
  (nth 0 err-info))

(defun flymake-er-get-line-err-info-list (err-info)
  (nth 1 err-info))

(cl-defstruct (flymake-ler
               (:constructor nil)
               (:constructor flymake-ler-make-ler (file line type text &optional full-file)))
  file line type text full-file)

(defun flymake-ler-set-file (line-err-info file)
  (flymake-ler-make-ler file
			(flymake-ler-line line-err-info)
			(flymake-ler-type line-err-info)
			(flymake-ler-text line-err-info)
			(flymake-ler-full-file line-err-info)))

(defun flymake-ler-set-full-file (line-err-info full-file)
  (flymake-ler-make-ler (flymake-ler-file line-err-info)
			(flymake-ler-line line-err-info)
			(flymake-ler-type line-err-info)
			(flymake-ler-text line-err-info)
			full-file))

(defun flymake-ler-set-line (line-err-info line)
  (flymake-ler-make-ler (flymake-ler-file line-err-info)
			line
			(flymake-ler-type line-err-info)
			(flymake-ler-text line-err-info)
			(flymake-ler-full-file line-err-info)))

(defun flymake-get-line-err-count (line-err-info-list type)
  "Return number of errors of specified TYPE.
Value of TYPE is either \"e\" or \"w\"."
  (let* ((idx        0)
	 (count      (length line-err-info-list))
	 (err-count  0))

    (while (< idx count)
      (when (equal type (flymake-ler-type (nth idx line-err-info-list)))
	(setq err-count (1+ err-count)))
      (setq idx (1+ idx)))
    err-count))

(defun flymake-get-err-count (err-info-list type)
  "Return number of errors of specified TYPE for ERR-INFO-LIST."
  (let* ((idx        0)
	 (count      (length err-info-list))
	 (err-count  0))
    (while (< idx count)
      (setq err-count (+ err-count (flymake-get-line-err-count (nth 1 (nth idx err-info-list)) type)))
      (setq idx (1+ idx)))
    err-count))

(defun flymake-highlight-err-lines (err-info-list)
  "Highlight error lines in BUFFER using info from ERR-INFO-LIST."
  (save-excursion
    (dolist (err err-info-list)
      (flymake-highlight-line (car err) (nth 1 err)))))

(defun flymake-overlay-p (ov)
  "Determine whether overlay OV was created by flymake."
  (and (overlayp ov) (overlay-get ov 'flymake-overlay)))

(defun flymake-make-overlay (beg end tooltip-text face bitmap)
  "Allocate a flymake overlay in range BEG and END."
  (when (not (flymake-region-has-flymake-overlays beg end))
    (let ((ov (make-overlay beg end nil t))
	  (fringe (and flymake-fringe-indicator-position
		       (propertize "!" 'display
				   (cons flymake-fringe-indicator-position
					 (if (listp bitmap)
					     bitmap
					   (list bitmap)))))))
      (overlay-put ov 'face           face)
      (overlay-put ov 'help-echo      tooltip-text)
      (overlay-put ov 'flymake-overlay  t)
      (overlay-put ov 'priority 100)
      (overlay-put ov 'evaporate t)
      (overlay-put ov 'before-string fringe)
      ;;+(flymake-log 3 "created overlay %s" ov)
      ov)
    (flymake-log 3 "created an overlay at (%d-%d)" beg end)))

(defun flymake-delete-own-overlays ()
  "Delete all flymake overlays in BUFFER."
  (dolist (ol (overlays-in (point-min) (point-max)))
    (when (flymake-overlay-p ol)
      (delete-overlay ol)
      ;;+(flymake-log 3 "deleted overlay %s" ol)
      )))

(defun flymake-region-has-flymake-overlays (beg end)
  "Check if region specified by BEG and END has overlay.
Return t if it has at least one flymake overlay, nil if no overlay."
  (let ((ov                  (overlays-in beg end))
	(has-flymake-overlays  nil))
    (while (consp ov)
      (when (flymake-overlay-p (car ov))
	(setq has-flymake-overlays t))
      (setq ov (cdr ov)))
    has-flymake-overlays))

(defface flymake-errline
  '((((supports :underline (:style wave)))
     :underline (:style wave :color "Red1"))
    (t
     :inherit error))
  "Face used for marking error lines."
  :version "24.4"
  :group 'flymake)

(defface flymake-warnline
  '((((supports :underline (:style wave)))
     :underline (:style wave :color "DarkOrange"))
    (t
     :inherit warning))
  "Face used for marking warning lines."
  :version "24.4"
  :group 'flymake)

(defun flymake-highlight-line (line-no line-err-info-list)
  "Highlight line LINE-NO in current buffer.
Perhaps use text from LINE-ERR-INFO-LIST to enhance highlighting."
  (goto-char (point-min))
  (forward-line (1- line-no))
  (pcase-let* ((beg (progn (back-to-indentation) (point)))
               (end (progn
                      (end-of-line)
                      (skip-chars-backward " \t\f\t\n" beg)
                      (if (eq (point) beg)
                          (line-beginning-position 2)
                        (point))))
               (tooltip-text (mapconcat #'flymake-ler-text line-err-info-list "\n"))
               (`(,face ,bitmap)
                (if (> (flymake-get-line-err-count line-err-info-list "e") 0)
                    (list 'flymake-errline flymake-error-bitmap)
                  (list 'flymake-warnline flymake-warning-bitmap))))
    (flymake-make-overlay beg end tooltip-text face bitmap)))

(defun flymake-find-err-info (err-info-list line-no)
  "Find (line-err-info-list pos) for specified LINE-NO."
  (if err-info-list
      (let* ((line-err-info-list  nil)
	     (pos       0)
	     (count     (length err-info-list)))

	(while (and (< pos count) (< (car (nth pos err-info-list)) line-no))
	  (setq pos (1+ pos)))
	(when (and (< pos count) (equal (car (nth pos err-info-list)) line-no))
	  (setq line-err-info-list (flymake-er-get-line-err-info-list (nth pos err-info-list))))
	(list line-err-info-list pos))
    '(nil 0)))

(defun flymake-line-err-info-is-less-or-equal (line-one line-two)
  (or (string< (flymake-ler-type line-one) (flymake-ler-type line-two))
      (and (string= (flymake-ler-type line-one) (flymake-ler-type line-two))
	   (not (flymake-ler-file line-one)) (flymake-ler-file line-two))
      (and (string= (flymake-ler-type line-one) (flymake-ler-type line-two))
	   (or (and      (flymake-ler-file line-one)       (flymake-ler-file line-two))
	       (and (not (flymake-ler-file line-one)) (not (flymake-ler-file line-two)))))))

(defun flymake-add-line-err-info (line-err-info-list line-err-info)
  "Update LINE-ERR-INFO-LIST with the error LINE-ERR-INFO.
For the format of LINE-ERR-INFO, see `flymake-ler-make-ler'.
The new element is inserted in the proper position, according to
the predicate `flymake-line-err-info-is-less-or-equal'.
The updated value of LINE-ERR-INFO-LIST is returned."
  (if (not line-err-info-list)
      (list line-err-info)
    (let* ((count  (length line-err-info-list))
	   (idx    0))
      (while (and (< idx count) (flymake-line-err-info-is-less-or-equal (nth idx line-err-info-list) line-err-info))
	(setq idx (1+ idx)))
      (cond ((equal 0     idx)    (setq line-err-info-list (cons line-err-info line-err-info-list)))
	    (t                    (setq line-err-info-list (flymake-ins-after line-err-info-list (1- idx) line-err-info))))
      line-err-info-list)))

(defun flymake-add-err-info (err-info-list line-err-info)
  "Update ERR-INFO-LIST with the error LINE-ERR-INFO, preserving sort order.
Returns the updated value of ERR-INFO-LIST.
For the format of ERR-INFO-LIST, see `flymake-err-info'.
For the format of LINE-ERR-INFO, see `flymake-ler-make-ler'."
  (let* ((line-no             (if (flymake-ler-file line-err-info) 1 (flymake-ler-line line-err-info)))
	 (info-and-pos        (flymake-find-err-info err-info-list line-no))
	 (exists              (car info-and-pos))
	 (pos                 (nth 1 info-and-pos))
	 (line-err-info-list  nil)
	 (err-info            nil))

    (if exists
	(setq line-err-info-list (flymake-er-get-line-err-info-list (car (nthcdr pos err-info-list)))))
    (setq line-err-info-list (flymake-add-line-err-info line-err-info-list line-err-info))

    (setq err-info (flymake-er-make-er line-no line-err-info-list))
    (cond (exists             (setq err-info-list (flymake-set-at err-info-list pos err-info)))
	  ((equal 0 pos)      (setq err-info-list (cons err-info err-info-list)))
	  (t                  (setq err-info-list (flymake-ins-after err-info-list (1- pos) err-info))))
    err-info-list))

(defvar-local flymake-is-running nil
  "If t, flymake syntax check process is running for the current buffer.")

(defun flymake-on-timer-event (buffer)
  "Start a syntax check for buffer BUFFER if necessary."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (not flymake-is-running)
		 flymake-last-change-time
		 (> (- (float-time) flymake-last-change-time)
                    flymake-no-changes-timeout))

	(setq flymake-last-change-time nil)
	(flymake-log 3 "starting syntax check as more than 1 second passed since last change")
	(flymake-start-syntax-check)))))

(define-obsolete-function-alias 'flymake-display-err-menu-for-current-line
  'flymake-popup-current-error-menu "24.4")

(defun flymake-popup-current-error-menu (&optional event)
  "Pop up a menu with errors/warnings for current line."
  (interactive (list last-nonmenu-event))
  (let* ((line-no (line-number-at-pos))
         (errors (or (car (flymake-find-err-info flymake-err-info line-no))
                     (user-error "No errors for current line")))
         (menu (mapcar (lambda (x)
                         (if (flymake-ler-file x)
                             (cons (format "%s - %s(%d)"
                                           (flymake-ler-text x)
                                           (flymake-ler-file x)
                                           (flymake-ler-line x))
                                   x)
                           (list (flymake-ler-text x))))
                       errors))
         (event (if (mouse-event-p event)
                    event
                  (list 'mouse-1 (posn-at-point))))
         (title (format "Line %d: %d error(s), %d warning(s)"
                        line-no
                        (flymake-get-line-err-count errors "e")
                        (flymake-get-line-err-count errors "w")))
         (choice (x-popup-menu event (list title (cons "" menu)))))
    (flymake-log 3 "choice=%s" choice)
    (when choice
      (flymake-goto-file-and-line (flymake-ler-full-file choice)
                                  (flymake-ler-line choice)))))

(defun flymake-goto-file-and-line (file line)
  "Try to get buffer for FILE and goto line LINE in it."
  (if (not (file-exists-p file))
      (flymake-log 1 "File %s does not exist" file)
    (find-file file)
    (goto-char (point-min))
    (forward-line (1- line))))

;; flymake minor mode declarations
(defvar-local flymake-mode-line nil)
(defvar-local flymake-mode-line-e-w nil)
(defvar-local flymake-mode-line-status nil)

(defun flymake-report-status (e-w &optional status)
  "Show status in mode line."
  (when e-w
    (setq flymake-mode-line-e-w e-w))
  (when status
    (setq flymake-mode-line-status status))
  (let* ((mode-line " Flymake"))
    (when (> (length flymake-mode-line-e-w) 0)
      (setq mode-line (concat mode-line ":" flymake-mode-line-e-w)))
    (setq mode-line (concat mode-line flymake-mode-line-status))
    (setq flymake-mode-line mode-line)
    (force-mode-line-update)))

;; Nothing in flymake uses this at all any more, so this is just for
;; third-party compatibility.
(define-obsolete-function-alias 'flymake-display-warning 'message-box "26.1")

(defun flymake-report-fatal-status (status warning)
  "Display a warning and switch flymake mode off."
  ;; This first message was always shown by default, and flymake-log
  ;; does nothing by default, hence the use of message.
  ;; Another option is display-warning.
  (if (< flymake-log-level 0)
      (message "Flymake: %s. Flymake will be switched OFF" warning))
  (flymake-mode 0)
  (flymake-log 0 "switched OFF Flymake mode for buffer %s due to fatal status %s, warning %s"
               (buffer-name) status warning))

(defun flymake-fix-line-numbers (err-info-list min-line max-line)
  "Replace line numbers with fixed value.
If line-numbers is less than MIN-LINE, set line numbers to MIN-LINE.
If line numbers is greater than MAX-LINE, set line numbers to MAX-LINE.
The reason for this fix is because some compilers might report
line number outside the file being compiled."
  (let* ((count     (length err-info-list))
	 (err-info  nil)
	 (line      0))
    (while (> count 0)
      (setq err-info (nth (1- count) err-info-list))
      (setq line (flymake-er-get-line err-info))
      (when (or (< line min-line) (> line max-line))
	(setq line (if (< line min-line) min-line max-line))
	(setq err-info-list (flymake-set-at err-info-list (1- count)
					    (flymake-er-make-er line
								(flymake-er-get-line-err-info-list err-info)))))
      (setq count (1- count))))
  err-info-list)

;;;###autoload
(define-minor-mode flymake-mode nil
  :group 'flymake :lighter flymake-mode-line
  (cond

   ;; Turning the mode ON.
   (flymake-mode
    (cond
     ((not buffer-file-name)
      (message "Flymake unable to run without a buffer file name"))
     ((not (flymake-can-syntax-check-file buffer-file-name))
      (flymake-log 2 "flymake cannot check syntax in buffer %s" (buffer-name)))
     (t
      (add-hook 'after-change-functions 'flymake-after-change-function nil t)
      (add-hook 'after-save-hook 'flymake-after-save-hook nil t)
      (add-hook 'kill-buffer-hook 'flymake-kill-buffer-hook nil t)
      ;;+(add-hook 'find-file-hook 'flymake-find-file-hook)

      (flymake-report-status "" "")

      (setq flymake-timer
            (run-at-time nil 1 'flymake-on-timer-event (current-buffer)))

      (when (and flymake-start-syntax-check-on-find-file
                 ;; Since we write temp files in current dir, there's no point
                 ;; trying if the directory is read-only (bug#8954).
                 (file-writable-p (file-name-directory buffer-file-name)))
        (with-demoted-errors
            (flymake-start-syntax-check))))))

   ;; Turning the mode OFF.
   (t
    (remove-hook 'after-change-functions 'flymake-after-change-function t)
    (remove-hook 'after-save-hook 'flymake-after-save-hook t)
    (remove-hook 'kill-buffer-hook 'flymake-kill-buffer-hook t)
    ;;+(remove-hook 'find-file-hook (function flymake-find-file-hook) t)

    (flymake-delete-own-overlays)

    (when flymake-timer
      (cancel-timer flymake-timer)
      (setq flymake-timer nil))

    (setq flymake-is-running nil))))

;;;###autoload
(defun flymake-mode-on ()
  "Turn flymake mode on."
  (flymake-mode 1)
  (flymake-log 1 "flymake mode turned ON for buffer %s" (buffer-name)))

;;;###autoload
(defun flymake-mode-off ()
  "Turn flymake mode off."
  (flymake-mode 0)
  (flymake-log 1 "flymake mode turned OFF for buffer %s" (buffer-name)))

(defun flymake-after-change-function (start stop _len)
  "Start syntax check for current buffer if it isn't already running."
  ;;+(flymake-log 0 "setting change time to %s" (float-time))
  (let((new-text (buffer-substring start stop)))
    (when (and flymake-start-syntax-check-on-newline (equal new-text "\n"))
      (flymake-log 3 "starting syntax check as new-line has been seen")
      (flymake-start-syntax-check))
    (setq flymake-last-change-time (float-time))))

(defun flymake-after-save-hook ()
  (if (local-variable-p 'flymake-mode (current-buffer))	; (???) other way to determine whether flymake is active in buffer being saved?
      (progn
	(flymake-log 3 "starting syntax check as buffer was saved")
	(flymake-start-syntax-check)))) ; no more mode 3. cannot start check if mode 3 (to temp copies) is active - (???)

(defun flymake-kill-buffer-hook ()
  (when flymake-timer
    (cancel-timer flymake-timer)
    (setq flymake-timer nil)))

;;;###autoload
(defun flymake-find-file-hook ()
  ;;+(when flymake-start-syntax-check-on-find-file
  ;;+    (flymake-log 3 "starting syntax check on file open")
  ;;+    (flymake-start-syntax-check)
  ;;+)
  (when (and (not (local-variable-p 'flymake-mode (current-buffer)))
	     (flymake-can-syntax-check-file buffer-file-name))
    (flymake-mode)
    (flymake-log 3 "automatically turned ON flymake mode")))

(defun flymake-get-first-err-line-no (err-info-list)
  "Return first line with error."
  (when err-info-list
    (flymake-er-get-line (car err-info-list))))

(defun flymake-get-last-err-line-no (err-info-list)
  "Return last line with error."
  (when err-info-list
    (flymake-er-get-line (nth (1- (length err-info-list)) err-info-list))))

(defun flymake-get-next-err-line-no (err-info-list line-no)
  "Return next line with error."
  (when err-info-list
    (let* ((count  (length err-info-list))
	   (idx    0))
      (while (and (< idx count) (>= line-no (flymake-er-get-line (nth idx err-info-list))))
	(setq idx (1+ idx)))
      (if (< idx count)
	  (flymake-er-get-line (nth idx err-info-list))))))

(defun flymake-get-prev-err-line-no (err-info-list line-no)
  "Return previous line with error."
  (when err-info-list
    (let* ((count (length err-info-list)))
      (while (and (> count 0) (<= line-no (flymake-er-get-line (nth (1- count) err-info-list))))
	(setq count (1- count)))
      (if (> count 0)
	  (flymake-er-get-line (nth (1- count) err-info-list))))))

(defun flymake-skip-whitespace ()
  "Move forward until non-whitespace is reached."
  (while (looking-at "[ \t]")
    (forward-char)))

(defun flymake-goto-line (line-no)
  "Go to line LINE-NO, then skip whitespace."
  (goto-char (point-min))
  (forward-line (1- line-no))
  (flymake-skip-whitespace))

(defun flymake-goto-next-error ()
  "Go to next error in err ring."
  (interactive)
  (let ((line-no (flymake-get-next-err-line-no flymake-err-info (line-number-at-pos))))
    (when (not line-no)
      (setq line-no (flymake-get-first-err-line-no flymake-err-info))
      (flymake-log 1 "passed end of file"))
    (if line-no
	(flymake-goto-line line-no)
      (flymake-log 1 "no errors in current buffer"))))

(defun flymake-goto-prev-error ()
  "Go to previous error in err ring."
  (interactive)
  (let ((line-no (flymake-get-prev-err-line-no flymake-err-info (line-number-at-pos))))
    (when (not line-no)
      (setq line-no (flymake-get-last-err-line-no flymake-err-info))
      (flymake-log 1 "passed beginning of file"))
    (if line-no
	(flymake-goto-line line-no)
      (flymake-log 1 "no errors in current buffer"))))

(defun flymake-patch-err-text (string)
  (if (string-match "^[\n\t :0-9]*\\(.*\\)$" string)
      (match-string 1 string)
    string))

(provide 'flymake)

(declare-function flymake-start-syntax-check "flymake-proc")
(declare-function flymake-can-syntax-check-file "flymake-proc")

(require 'flymake-proc)
;;; flymake.el ends here
