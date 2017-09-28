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

(require 'cl-lib)
(require 'thingatpt) ; end-of-thing
(require 'warnings) ; warning-numeric-level, display-warning
(require 'compile) ; for some faces
(eval-when-compile (require 'subr-x)) ; when-let*, if-let*, hash-table-keys

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
  "Obsolete and ignored variable."
  :type 'integer)
(make-obsolete-variable 'flymake-log-level
			"it is superseded by `warning-minimum-log-level.'"
                        "26.1")

(defcustom flymake-wrap-around t
  "If non-nil, moving to errors wraps around buffer boundaries."
  :group 'flymake :type 'boolean)

(defvar-local flymake-timer nil
  "Timer for starting syntax check.")

(defvar-local flymake-last-change-time nil
  "Time of last buffer change.")

(defvar-local flymake-check-start-time nil
  "Time at which syntax check was started.")

(defun flymake--log-1 (level sublog msg &rest args)
  "Do actual work for `flymake-log'."
  (let (;; never popup the log buffer
        (warning-minimum-level :emergency)
        (warning-type-format
         (format " [%s %s]"
                 (or sublog 'flymake)
                 (current-buffer))))
    (display-warning (list 'flymake sublog)
                     (apply #'format-message msg args)
                     (if (numberp level)
                         (or (nth level
                                  '(:emergency :error :warning :debug :debug) )
                             :error)
                       level)
                     "*Flymake log*")))

(defmacro flymake-log (level msg &rest args)
  "Log, at level LEVEL, the message MSG formatted with ARGS.
LEVEL is passed to `display-warning', which is used to display
the warning.  If this form is included in a byte-compiled file,
the generated warning contains an indication of the file that
generated it."
  (let* ((compile-file (and (boundp 'byte-compile-current-file)
                            (symbol-value 'byte-compile-current-file)))
         (sublog (if (and
                      compile-file
                      (not load-file-name))
                     (intern
                      (file-name-nondirectory
                       (file-name-sans-extension compile-file))))))
    `(flymake--log-1 ,level ',sublog ,msg ,@args)))

(defun flymake-error (text &rest args)
  "Format TEXT with ARGS and signal an error for flymake."
  (let ((msg (apply #'format-message text args)))
    (flymake-log :error msg)
    (error (concat "[Flymake] " msg))))

(cl-defstruct (flymake--diag
               (:constructor flymake--diag-make))
  buffer beg end type text backend)

(defun flymake-make-diagnostic (buffer
                                beg
                                end
                                type
                                text)
  "Mark BUFFER's region from BEG to END with a flymake diagnostic.
TYPE is a key to `flymake-diagnostic-types-alist' and TEXT is a
description of the problem detected in this region."
  (flymake--diag-make :buffer buffer :beg beg :end end :type type :text text))

(defun flymake-ler-make-ler (file line type text &optional full-file)
  (let* ((file (or full-file file))
         (buf (find-buffer-visiting file)))
    (unless buf (flymake-error "No buffer visiting %s" file))
    (pcase-let* ((`(,beg . ,end)
                  (with-current-buffer buf
                    (flymake-diag-region line nil))))
      (flymake-make-diagnostic buf beg end type text))))

(make-obsolete 'flymake-ler-make-ler 'flymake-make-diagnostic "26.1")

(cl-defun flymake--overlays (&key beg end filter compare key)
  "Get flymake-related overlays.
If BEG is non-nil and END is nil, consider only `overlays-at'
BEG. Otherwise consider `overlays-in' the region comprised by BEG
and END, defaulting to the whole buffer.  Remove all that do not
verify FILTER, sort them by COMPARE (using KEY)."
  (cl-remove-if-not
   (lambda (ov)
     (and (overlay-get ov 'flymake-overlay)
          (or (not filter)
              (cond ((functionp filter) (funcall filter ov))
                    ((symbolp filter) (overlay-get ov filter))))))
   (save-restriction
     (widen)
     (let ((ovs (if (and beg (null end))
                    (overlays-at beg t)
                  (overlays-in (or beg (point-min))
                               (or end (point-max))))))
       (if compare
           (cl-sort ovs compare :key (or key
                                         #'identity))
         ovs)))))

(defun flymake-delete-own-overlays (&optional filter)
  "Delete all flymake overlays in BUFFER."
  (mapc #'delete-overlay (flymake--overlays :filter filter)))

(defface flymake-error
  '((((supports :underline (:style wave)))
     :underline (:style wave :color "Red1"))
    (t
     :inherit error))
  "Face used for marking error regions."
  :version "24.4"
  :group 'flymake)

(defface flymake-warning
  '((((supports :underline (:style wave)))
     :underline (:style wave :color "deep sky blue"))
    (t
     :inherit warning))
  "Face used for marking warning regions."
  :version "24.4"
  :group 'flymake)

(defface flymake-note
  '((((supports :underline (:style wave)))
     :underline (:style wave :color "yellow green"))
    (t
     :inherit warning))
  "Face used for marking note regions."
  :version "26.1"
  :group 'flymake)

(define-obsolete-face-alias 'flymake-warnline 'flymake-warning "26.1")
(define-obsolete-face-alias 'flymake-errline 'flymake-error "26.1")

(defun flymake-diag-region (line &optional col)
  "Compute region (BEG . END) corresponding to LINE and COL.
If COL is nil, return a region just for LINE.
Return nil if the region is invalid."
  (condition-case-unless-debug _err
      (let ((line (min (max line 1)
                       (line-number-at-pos (point-max) 'absolute))))
        (save-excursion
          (goto-char (point-min))
          (forward-line (1- line))
          (cl-flet ((fallback-bol
                     () (progn (back-to-indentation) (point)))
                    (fallback-eol
                     (beg)
                     (progn
                       (end-of-line)
                       (skip-chars-backward " \t\f\t\n" beg)
                       (if (eq (point) beg)
                           (line-beginning-position 2)
                         (point)))))
            (if (and col (cl-plusp col))
                (let* ((beg (progn (forward-char (1- col))
                                   (point)))
                       (sexp-end (ignore-errors (end-of-thing 'sexp)))
                       (end (or (and sexp-end
                                     (not (= sexp-end beg))
                                     sexp-end)
                                (ignore-errors (goto-char (1+ beg)))))
                       (safe-end (or end
                                     (fallback-eol beg))))
                  (cons (if end beg (fallback-bol))
                        safe-end))
              (let* ((beg (fallback-bol))
                     (end (fallback-eol beg)))
                (cons beg end))))))
    (error (flymake-error "Invalid region line=%s col=%s" line col))))

(defvar flymake-diagnostic-functions nil
  "List of flymake backends i.e. sources of flymake diagnostics.

This variable holds an arbitrary number of \"backends\" or
\"checkers\" providing the flymake UI's \"frontend\" with
information about where and how to annotate problems diagnosed in
a buffer.

Backends are lisp functions sharing a common calling
convention. Whenever flymake decides it is time to re-check the
buffer, each backend is called with a single argument, a
REPORT-FN callback, detailed below.  Backend functions are first
expected to quickly and inexpensively announce the feasibility of
checking the buffer (i.e. they aren't expected to immediately
start checking the buffer):

* If the backend function returns nil, flymake forgets about this
  backend for the current check, but will call it again the next
  time;

* If the backend function returns non-nil, flymake expects this backend to
  check the buffer and call its REPORT-FN callback function. If
  the computation involved is inexpensive, the backend function
  may do so synchronously before returning. If it is not, it may
  do so after retuning, using idle timers, asynchronous
  processes or other asynchronous mechanisms.

* If the backend function signals an error, it is disabled, i.e. flymake
  will not attempt it again for this buffer until `flymake-mode'
  is turned off and on again.

When calling REPORT-FN, the first argument passed to it decides
how to proceed. Recognized values are:

* A (possibly empty) list of objects created with
  `flymake-make-diagnostic', causing flymake to annotate the
  buffer with this information and consider the backend has
  having finished its check normally.

* The symbol `:progress', signalling that the backend is still
  working and will call REPORT-FN again in the future.

* The symbol `:panic', signalling that the backend has
  encountered an exceptional situation and should be disabled.

In the latter cases, it is also possible to provide REPORT-FN
with a string as the keyword argument `:explanation'. The string
should give human-readable details of the situation.")

(defvar flymake-diagnostic-types-alist
  `((:error
     . ((flymake-category . flymake-error)))
    (:warning
     . ((flymake-category . flymake-warning)))
    (:note
     . ((flymake-category . flymake-note))))
  "Alist ((KEY . PROPS)*) of properties of flymake error types.
KEY can be anything passed as `:type' to `flymake-diag-make'.

PROPS is an alist of properties that are applied, in order, to
the diagnostics of each type.  The recognized properties are:

* Every property pertaining to overlays, except `category' and
  `evaporate' (see Info Node `(elisp)Overlay Properties'), used
  affect the appearance of Flymake annotations.

* `bitmap', an image displayed in the fringe according to
  `flymake-fringe-indicator-position'.  The value actually
  follows the syntax of `flymake-error-bitmap' (which see).  It
  is overriden by any `before-string' overlay property.

* `severity', a non-negative integer specifying the diagnostic's
  severity.  The higher, the more serious.  If the overlay
  priority `priority' is not specified, `severity' is used to set
  it and help sort overlapping overlays.

* `flymake-category', a symbol whose property list is considered
  as a default for missing values of any other properties.  This
  is useful to backend authors when creating new diagnostic types
  that differ from an existing type by only a few properties.")

(put 'flymake-error 'face 'flymake-error)
(put 'flymake-error 'bitmap flymake-error-bitmap)
(put 'flymake-error 'severity (warning-numeric-level :error))
(put 'flymake-error 'mode-line-face 'compilation-error)

(put 'flymake-warning 'face 'flymake-warning)
(put 'flymake-warning 'bitmap flymake-warning-bitmap)
(put 'flymake-warning 'severity (warning-numeric-level :warning))
(put 'flymake-warning 'mode-line-face 'compilation-warning)

(put 'flymake-note 'face 'flymake-note)
(put 'flymake-note 'bitmap nil)
(put 'flymake-note 'severity (warning-numeric-level :debug))
(put 'flymake-note 'mode-line-face 'compilation-info)

(defun flymake--lookup-type-property (type prop &optional default)
  "Look up PROP for TYPE in `flymake-diagnostic-types-alist'.
If TYPE doesn't declare PROP in either
`flymake-diagnostic-types-alist' or in the symbol of its
associated `flymake-category' return DEFAULT."
  (let ((alist-probe (assoc type flymake-diagnostic-types-alist)))
    (cond (alist-probe
           (let* ((alist (cdr alist-probe))
                  (prop-probe (assoc prop alist)))
             (if prop-probe
                 (cdr prop-probe)
               (if-let* ((cat (assoc-default 'flymake-category alist))
                         (plist (and (symbolp cat)
                                     (symbol-plist cat)))
                         (cat-probe (plist-member plist prop)))
                   (cadr cat-probe)
                 default))))
          (t
           default))))

(defun flymake--fringe-overlay-spec (bitmap)
  (and flymake-fringe-indicator-position
       bitmap
       (propertize "!" 'display
                   (cons flymake-fringe-indicator-position
                         (if (listp bitmap)
                             bitmap
                           (list bitmap))))))

(defun flymake--highlight-line (diagnostic)
  "Highlight buffer with info in DIAGNOSTIC."
  (when-let* ((ov (make-overlay
                   (flymake--diag-beg diagnostic)
                   (flymake--diag-end diagnostic))))
    ;; First set `category' in the overlay, then copy over every other
    ;; property.
    ;;
    (let ((alist (assoc-default (flymake--diag-type diagnostic)
                                flymake-diagnostic-types-alist)))
      (overlay-put ov 'category (assoc-default 'flymake-category alist))
      (cl-loop for (k . v) in alist
               unless (eq k 'category)
               do (overlay-put ov k v)))
    ;; Now ensure some essential defaults are set
    ;;
    (cl-flet ((default-maybe
                (prop value)
                (unless (or (plist-member (overlay-properties ov) prop)
                            (let ((cat (overlay-get ov
                                                    'flymake-category)))
                              (and cat
                                   (plist-member (symbol-plist cat) prop))))
                  (overlay-put ov prop value))))
      (default-maybe 'bitmap flymake-error-bitmap)
      (default-maybe 'before-string
        (flymake--fringe-overlay-spec
         (overlay-get ov 'bitmap)))
      (default-maybe 'help-echo
        (lambda (_window _ov pos)
          (mapconcat
           (lambda (ov)
             (let ((diag (overlay-get ov 'flymake--diagnostic)))
               (flymake--diag-text diag)))
           (flymake--overlays :beg pos)
           "\n")))
      (default-maybe 'severity (warning-numeric-level :error))
      (default-maybe 'priority (+ 100 (overlay-get ov 'severity))))
    ;; Some properties can't be overriden
    ;;
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'flymake-overlay t)
    (overlay-put ov 'flymake--diagnostic diagnostic)))

(defun flymake-on-timer-event (buffer)
  "Start a syntax check for buffer BUFFER if necessary."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and flymake-mode
		 flymake-last-change-time
		 (> (- (float-time) flymake-last-change-time)
                    flymake-no-changes-timeout))

	(setq flymake-last-change-time nil)
	(flymake-log :debug "starting syntax check after no changes for some time")
	(flymake-start)))))

(define-obsolete-function-alias 'flymake-display-err-menu-for-current-line
  'flymake-popup-current-error-menu "24.4")

(defun flymake-popup-current-error-menu (&optional event)
  "Pop up a menu with errors/warnings for current line."
  (interactive (list last-nonmenu-event))
  (let* ((diag-overlays (or
                         (flymake--overlays :filter 'flymake--diagnostic
                                            :beg (line-beginning-position)
                                            :end (line-end-position))
                         (user-error "No flymake problem for current line")))
         (menu (mapcar (lambda (ov)
                         (let ((diag (overlay-get ov 'flymake--diagnostic)))
                           (cons (flymake--diag-text diag)
                                 ov)))
                       diag-overlays))
         (event (if (mouse-event-p event)
                    event
                  (list 'mouse-1 (posn-at-point))))
         (diagnostics (mapcar (lambda (ov) (overlay-get ov 'flymake--diagnostic))
                              diag-overlays))
         (title (format "Line %d: %d diagnostics(s)"
                        (line-number-at-pos)
                        (length diagnostics)))
         (choice (x-popup-menu event (list title (cons "" menu)))))
    (flymake-log :debug "choice=%s" choice)
    ;; FIXME: What is the point of going to the problem locus if we're
    ;; certainly already there?
    ;;
    (when choice (goto-char (overlay-start choice)))))


;; Nothing in flymake uses this at all any more, so this is just for
;; third-party compatibility.
(define-obsolete-function-alias 'flymake-display-warning 'message-box "26.1")

(defvar-local flymake--running-backends nil
  "List of currently active flymake backends.
An active backend is a member of `flymake-diagnostic-functions'
that has been invoked but hasn't reported any final status yet.")

(defvar-local flymake--disabled-backends nil
  "List of currently disabled flymake backends.
A backend is disabled if it reported `:panic'.")

(defvar-local flymake--diagnostics-table nil
  "Hash table of all diagnostics indexed by backend.")

(defun flymake-is-running ()
  "Tell if flymake has running backends in this buffer"
  flymake--running-backends)

(defun flymake--disable-backend (backend action &optional explanation)
  (cl-pushnew backend flymake--disabled-backends)
  (flymake-log :warning "Disabled the backend %s due to reports of %s (%s)"
               backend action explanation))

(cl-defun flymake--handle-report (backend action &key explanation force)
  "Handle reports from flymake backend identified by BACKEND.

BACKEND, ACTION and EXPLANATION conform to the calling convention
described in `flymake-diagnostic-functions' (which see). Optional
FORCE says to handle a report even if it was not expected."
  (cond
   ((and (not (memq backend flymake--running-backends))
         (not force))
    (flymake-error "Ignoring unexpected report from backend %s" backend))
   ((eq action :progress)
    (flymake-log 3 "Backend %s reports progress: %s" backend explanation))
   ((eq :panic action)
    (flymake--disable-backend backend action explanation))
   ((listp action)
    (let ((diagnostics action))
      (save-restriction
        (widen)
        (flymake-delete-own-overlays
         (lambda (ov)
           (eq backend
               (flymake--diag-backend
                (overlay-get ov 'flymake--diagnostic)))))
        (puthash backend diagnostics flymake--diagnostics-table)
        (mapc (lambda (diag)
                (flymake--highlight-line diag)
                (setf (flymake--diag-backend diag) backend))
              diagnostics)
        (when flymake-check-start-time
          (flymake-log 2 "backend %s reported %d diagnostics in %.2f second(s)"
                       backend
                       (length diagnostics)
                       (- (float-time) flymake-check-start-time))))))
   (t
    (flymake--disable-backend "?"
                              :strange
                              (format "unknown action %s (%s)"
                                      action explanation))))
  (unless (eq action :progress)
    (flymake--stop-backend backend)))

(defun flymake-make-report-fn (backend)
  "Make a suitable anonymous report function for BACKEND.
BACKEND is used to help flymake distinguish diagnostic
sources."
  (lambda (&rest args)
    (apply #'flymake--handle-report backend args)))

(defun flymake--stop-backend (backend)
  "Stop the backend BACKEND."
  (setq flymake--running-backends (delq backend flymake--running-backends)))

(defun flymake--run-backend (backend)
  "Run the backend BACKEND."
  (push backend flymake--running-backends)
  (remhash backend flymake--diagnostics-table)
  ;; FIXME: Should use `condition-case-unless-debug'
  ;; here, but that won't let me catch errors during
  ;; testing where `debug-on-error' is always t
  (condition-case err
      (unless (funcall backend
                       (flymake-make-report-fn backend))
        (flymake--stop-backend backend))
    (error
     (flymake--disable-backend backend :error
                               err)
     (flymake--stop-backend backend))))

(defun flymake-start (&optional deferred interactive)
  "Start a syntax check.
Start it immediately, or after current command if DEFERRED is
non-nil.  With optional INTERACTIVE or interactively, clear any
stale information about running and automatically disabled
backends."
  (interactive (list nil t))
  (cl-labels
      ((start
        ()
        (remove-hook 'post-command-hook #'start 'local)
        (setq flymake-check-start-time (float-time))
        (when interactive
          (setq flymake--diagnostics-table (make-hash-table)
                flymake--running-backends nil
                flymake--disabled-backends nil))
        (dolist (backend flymake-diagnostic-functions)
          (cond ((memq backend flymake--running-backends)
                 (flymake-log :debug "Backend %s still running, not restarting"
                              backend))
                ((memq backend flymake--disabled-backends)
                 (flymake-log :debug "Backend %s is disabled, not starting"
                              backend))
                (t
                 (flymake--run-backend backend))))))
    (if (and deferred
             this-command)
        (add-hook 'post-command-hook #'start 'append 'local)
      (start))))

;;;###autoload
(define-minor-mode flymake-mode nil
  :group 'flymake :lighter flymake--mode-line-format
  (setq flymake--running-backends nil
        flymake--disabled-backends nil)
  (cond
   ;; Turning the mode ON.
   (flymake-mode
    (cond
     ((not flymake-diagnostic-functions)
      (flymake-error "No backends to check buffer %s" (buffer-name)))
     (t
      (add-hook 'after-change-functions 'flymake-after-change-function nil t)
      (add-hook 'after-save-hook 'flymake-after-save-hook nil t)
      (add-hook 'kill-buffer-hook 'flymake-kill-buffer-hook nil t)

      (setq flymake-timer
            (run-at-time nil 1 'flymake-on-timer-event (current-buffer)))
      (setq flymake--diagnostics-table (make-hash-table))

      (when flymake-start-syntax-check-on-find-file
        (flymake-start)))))

   ;; Turning the mode OFF.
   (t
    (remove-hook 'after-change-functions 'flymake-after-change-function t)
    (remove-hook 'after-save-hook 'flymake-after-save-hook t)
    (remove-hook 'kill-buffer-hook 'flymake-kill-buffer-hook t)
    ;;+(remove-hook 'find-file-hook (function flymake-find-file-hook) t)

    (flymake-delete-own-overlays)

    (when flymake-timer
      (cancel-timer flymake-timer)
      (setq flymake-timer nil)))))

;;;###autoload
(defun flymake-mode-on ()
  "Turn flymake mode on."
  (flymake-mode 1))

;;;###autoload
(defun flymake-mode-off ()
  "Turn flymake mode off."
  (flymake-mode 0))

(defun flymake-after-change-function (start stop _len)
  "Start syntax check for current buffer if it isn't already running."
  (let((new-text (buffer-substring start stop)))
    (when (and flymake-start-syntax-check-on-newline (equal new-text "\n"))
      (flymake-log :debug "starting syntax check as new-line has been seen")
      (flymake-start 'deferred))
    (setq flymake-last-change-time (float-time))))

(defun flymake-after-save-hook ()
  (when flymake-mode
    (flymake-log :debug "starting syntax check as buffer was saved")
    (flymake-start)))

(defun flymake-kill-buffer-hook ()
  (when flymake-timer
    (cancel-timer flymake-timer)
    (setq flymake-timer nil)))

;;;###autoload
(defun flymake-find-file-hook ()
  (unless (or flymake-mode
              (null flymake-diagnostic-functions))
    (flymake-mode)
    (flymake-log :warning "Turned on in `flymake-find-file-hook'")))

(defun flymake-goto-next-error (&optional n filter interactive)
  "Go to Nth next flymake error in buffer matching FILTER.
FILTER is a list of diagnostic types found in
`flymake-diagnostic-types-alist', or nil, if no filter is to be
applied.

Interactively, always goes to the next error.  Also
interactively, FILTER is determined by the prefix arg.  With no
prefix arg, don't use a filter, otherwise only consider
diagnostics of type `:error' and `:warning'."
  (interactive (list 1
                     (if current-prefix-arg
                         '(:error :warning))
                     t))
  (let* ((n (or n 1))
         (ovs (flymake--overlays :filter
                                 (lambda (ov)
                                   (let ((diag (overlay-get
                                                ov
                                                'flymake--diagnostic)))
                                     (and diag
                                          (or (not filter)
                                              (memq (flymake--diag-type diag)
                                                    filter)))))
                                 :compare (if (cl-plusp n) #'< #'>)
                                 :key #'overlay-start))
         (tail (cl-member-if (lambda (ov)
                               (if (cl-plusp n)
                                   (> (overlay-start ov)
                                      (point))
                                 (< (overlay-start ov)
                                    (point))))
                             ovs))
         (chain (if flymake-wrap-around
                    (if tail
                        (progn (setcdr (last tail) ovs) tail)
                      (and ovs (setcdr (last ovs) ovs)))
                  tail))
         (target (nth (1- n) chain)))
    (cond (target
           (goto-char (overlay-start target))
           (when interactive
             (message
              (funcall (overlay-get target 'help-echo)
                       nil nil (point)))))
          (interactive
           (user-error "No more flymake errors%s"
                       (if filter
                           (format " of types %s" filter)
                         ""))))))

(defun flymake-goto-prev-error (&optional n filter interactive)
  "Go to Nth previous flymake error in buffer matching FILTER.
FILTER is a list of diagnostic types found in
`flymake-diagnostic-types-alist', or nil, if no filter is to be
applied.

Interactively, always goes to the previous error.  Also
interactively, FILTER is determined by the prefix arg.  With no
prefix arg, don't use a filter, otherwise only consider
diagnostics of type `:error' and `:warning'."
  (interactive (list 1 (if current-prefix-arg
                           '(:error :warning))
                     t))
  (flymake-goto-next-error (- (or n 1)) filter interactive))


;;; Mode-line fanciness
;;;
(defvar flymake--mode-line-format `(:eval (flymake--mode-line-format)))

(put 'flymake--mode-line-format 'risky-local-variable t)

(defun flymake--mode-line-format ()
  "Produce a pretty minor mode indicator."
  (let ((running flymake--running-backends)
        (reported (hash-table-keys flymake--diagnostics-table)))
    `((:propertize " Flymake"
                   mouse-face mode-line-highlight
                   ,@(when (not reported)
                       `(face compilation-mode-line-fail))
                   help-echo
                   ,(concat (format "%s registered backends\n"
                                    (length flymake-diagnostic-functions))
                            (format "%s running\n"
                                    (length running))
                            (format "%s disabled\n"
                                    (length flymake--disabled-backends))
                            "mouse-1: go to log buffer ")
                   keymap
                   ,(let ((map (make-sparse-keymap)))
                      (define-key map [mode-line mouse-1]
                        (lambda (_event)
                          (interactive "e")
                          (switch-to-buffer "*Flymake log*")))
                      map))
      ,@(when running
          `(":" (:propertize "Run"
                             face compilation-mode-line-run
                             help-echo
                             ,(format "%s running backends"
                                      (length running)))))
      ,@(when reported
          (let ((by-type (make-hash-table)))
            (maphash (lambda (_backend diags)
                       (mapc (lambda (diag)
                               (push diag
                                     (gethash (flymake--diag-type diag)
                                              by-type)))
                             diags))
                     flymake--diagnostics-table)
            (cl-loop
             for (type . severity)
             in (cl-sort (mapcar (lambda (type)
                                   (cons type (flymake--lookup-type-property
                                               type
                                               'severity
                                               (warning-numeric-level :error))))
                                 (cl-union (hash-table-keys by-type)
                                           '(:error :warning)))
                         #'>
                         :key #'cdr)
             for diags = (gethash type by-type)
             for face = (flymake--lookup-type-property type
                                                       'mode-line-face
                                                       'compilation-error)
             when (or diags
                      (>= severity (warning-numeric-level :warning)))
             collect `(:propertize
                       ,(format "%d" (length diags))
                       face ,face
                       mouse-face mode-line-highlight
                       keymap
                       ,(let ((map (make-sparse-keymap))
                              (type type))
                          (define-key map [mode-line mouse-4]
                            (lambda (_event)
                              (interactive "e")
                              (flymake-goto-prev-error 1 (list type) t)))
                          (define-key map [mode-line mouse-5]
                            (lambda (_event)
                              (interactive "e")
                              (flymake-goto-next-error 1 (list type) t)))
                          map)
                       help-echo
                       ,(concat (format "%s diagnostics of type %s\n"
                                        (propertize (format "%d"
                                                            (length diags))
                                                    'face face)
                                        (propertize (format "%s" type)
                                                    'face face))
                                "mouse-4/mouse-5: previous/next of this type\n"))
             into forms
             finally return
             `((:propertize "[")
               ,@(cl-loop for (a . rest) on forms by #'cdr
                          collect a when rest collect
                          '(:propertize " "))
               (:propertize "]"))))))))




(provide 'flymake)

(require 'flymake-proc)
(require 'flymake-elisp)
;;; flymake.el ends here
