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
(require 'subr-x) ; when-let*, if-let*, hash-table-keys, hash-table-values

(defgroup flymake nil
  "Universal on-the-fly syntax checker."
  :version "23.1"
  :link '(custom-manual "(flymake) Top")
  :group 'tools)

(defcustom flymake-error-bitmap '(flymake-double-exclamation-mark
                                  compilation-error)
  "Bitmap (a symbol) used in the fringe for indicating errors.
The value may also be a list of two elements where the second
element specifies the face for the bitmap.  For possible bitmap
symbols, see `fringe-bitmaps'.  See also `flymake-warning-bitmap'.

The option `flymake-fringe-indicator-position' controls how and where
this is used."
  :version "24.3"
  :type '(choice (symbol :tag "Bitmap")
                 (list :tag "Bitmap and face"
                       (symbol :tag "Bitmap")
                       (face :tag "Face"))))

(defcustom flymake-warning-bitmap '(exclamation-mark compilation-warning)
  "Bitmap (a symbol) used in the fringe for indicating warnings.
The value may also be a list of two elements where the second
element specifies the face for the bitmap.  For possible bitmap
symbols, see `fringe-bitmaps'.  See also `flymake-error-bitmap'.

The option `flymake-fringe-indicator-position' controls how and where
this is used."
  :version "24.3"
  :type '(choice (symbol :tag "Bitmap")
                 (list :tag "Bitmap and face"
                       (symbol :tag "Bitmap")
                       (face :tag "Face"))))

(defcustom flymake-note-bitmap '(exclamation-mark compilation-info)
  "Bitmap (a symbol) used in the fringe for indicating info notes.
The value may also be a list of two elements where the second
element specifies the face for the bitmap.  For possible bitmap
symbols, see `fringe-bitmaps'.  See also `flymake-error-bitmap'.

The option `flymake-fringe-indicator-position' controls how and where
this is used."
  :version "26.1"
  :type '(choice (symbol :tag "Bitmap")
                 (list :tag "Bitmap and face"
                       (symbol :tag "Bitmap")
                       (face :tag "Face"))))

(defcustom flymake-fringe-indicator-position 'left-fringe
  "The position to put Flymake fringe indicator.
The value can be nil (do not use indicators), `left-fringe' or `right-fringe'.
See `flymake-error-bitmap' and `flymake-warning-bitmap'."
  :version "24.3"
  :type '(choice (const left-fringe)
		 (const right-fringe)
		 (const :tag "No fringe indicators" nil)))

(defcustom flymake-start-syntax-check-on-newline t
  "Start syntax check if newline char was added/removed from the buffer."
  :type 'boolean)

(defcustom flymake-no-changes-timeout 0.5
  "Time to wait after last change before automatically checking buffer.
If nil, never start checking buffer automatically like this."
  :type 'number)

(defcustom flymake-gui-warnings-enabled t
  "Enables/disables GUI warnings."
  :type 'boolean)
(make-obsolete-variable 'flymake-gui-warnings-enabled
			"it no longer has any effect." "26.1")

(defcustom flymake-start-syntax-check-on-find-file t
  "Start syntax check on find file."
  :type 'boolean)

(defcustom flymake-log-level -1
  "Obsolete and ignored variable."
  :type 'integer)
(make-obsolete-variable 'flymake-log-level
			"it is superseded by `warning-minimum-log-level.'"
                        "26.1")

(defcustom flymake-wrap-around t
  "If non-nil, moving to errors wraps around buffer boundaries."
  :type 'boolean)

(define-fringe-bitmap 'flymake-double-exclamation-mark
  (vector #b00000000
          #b00000000
          #b00000000
          #b00000000
          #b01100110
          #b01100110
          #b01100110
          #b01100110
          #b01100110
          #b01100110
          #b01100110
          #b01100110
          #b00000000
          #b01100110
          #b00000000
          #b00000000
          #b00000000))

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
  "Make a Flymake diagnostic for BUFFER's region from BEG to END.
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
verify FILTER, a function, and sort them by COMPARE (using KEY)."
  (save-restriction
    (widen)
    (let ((ovs (cl-remove-if-not
                (lambda (ov)
                  (and (overlay-get ov 'flymake)
                       (or (not filter)
                           (funcall filter ov))))
                (if (and beg (null end))
                    (overlays-at beg t)
                  (overlays-in (or beg (point-min))
                               (or end (point-max)))))))
      (if compare
          (cl-sort ovs compare :key (or key
                                        #'identity))
        ovs))))

(defun flymake-delete-own-overlays (&optional filter)
  "Delete all Flymake overlays in BUFFER."
  (mapc #'delete-overlay (flymake--overlays :filter filter)))

(defface flymake-error
  '((((supports :underline (:style wave)))
     :underline (:style wave :color "Red1"))
    (t
     :inherit error))
  "Face used for marking error regions."
  :version "24.4")

(defface flymake-warning
  '((((supports :underline (:style wave)))
     :underline (:style wave :color "deep sky blue"))
    (t
     :inherit warning))
  "Face used for marking warning regions."
  :version "24.4")

(defface flymake-note
  '((((supports :underline (:style wave)))
     :underline (:style wave :color "yellow green"))
    (t
     :inherit warning))
  "Face used for marking note regions."
  :version "26.1")

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
  "Special hook of Flymake backends to check a buffer.

The functions in this hook diagnose problems in a buffer’s
contents and provide the Flymake user interface with information
about where and how to annotate problems diagnosed in a buffer.

Whenever Flymake or the user decides to re-check the buffer, each
function is called with a common calling convention, a single
REPORT-FN argument and a list of keword value pairs, detailed
below.  Backend functions are expected to initiate the buffer
check, but aren't required to complete it check before exiting:
if the computation involved is expensive, especially for large
buffers, that task can be scheduled for the future using
asynchronous processes or other asynchronous mechanisms.

In any case, backend functions are expected to return quickly or
signal an error, in which case the backend is disabled.  Flymake
will not try disabled backends again for any future checks of
this buffer.  Certain commands, like turning `flymake-mode' off
and on again, reset the list of disabled backends.

If the function returns, Flymake considers the backend to be
\"running\". If it has not done so already, the backend is
expected to call the function REPORT-FN with a single argument
ACTION followed by an optional list of keyword-value pairs
their values (:KEY1 VALUE1 :KEY2 VALUE2...).

The possible values for ACTION are.

* A (possibly empty) list of diagnostic objects created with
  `flymake-make-diagnostic', causing Flymake to annotate the
  buffer with this information.

  A backend may call REPORT-FN repeatedly in this manner, but
  only until Flymake considers that the most recently requested
  buffer check is now obsolete because, say, buffer contents have
  changed in the meantime. The backend is only given notice of
  this via a renewed call to the backend function. Thus, to
  prevent making obsolete reports and wasting resources, backend
  functions should first cancel any ongoing processing from
  previous calls.

* The symbol `:panic', signalling that the backend has
  encountered an exceptional situation and should be disabled.

The recognized optional keyword arguments are:

* ‘:explanation’: value should give user-readable details of
  the situation encountered, if any.

* ‘:force’: value should be a boolean suggesting that the Flymake
  considers the report even if was somehow unexpected.")

(defvar flymake-diagnostic-types-alist
  `((:error
     . ((flymake-category . flymake-error)))
    (:warning
     . ((flymake-category . flymake-warning)))
    (:note
     . ((flymake-category . flymake-note))))
  "Alist ((KEY . PROPS)*) of properties of Flymake error types.
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
(put 'flymake-error 'bitmap 'flymake-error-bitmap)
(put 'flymake-error 'severity (warning-numeric-level :error))
(put 'flymake-error 'mode-line-face 'compilation-error)

(put 'flymake-warning 'face 'flymake-warning)
(put 'flymake-warning 'bitmap 'flymake-warning-bitmap)
(put 'flymake-warning 'severity (warning-numeric-level :warning))
(put 'flymake-warning 'mode-line-face 'compilation-warning)

(put 'flymake-note 'face 'flymake-note)
(put 'flymake-note 'bitmap 'flymake-note-bitmap)
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

(defun flymake--fringe-overlay-spec (bitmap &optional recursed)
  (if (and (symbolp bitmap)
           (boundp bitmap)
           (not recursed))
      (flymake--fringe-overlay-spec
       (symbol-value bitmap) t)
    (and flymake-fringe-indicator-position
         bitmap
         (propertize "!" 'display
                     (cons flymake-fringe-indicator-position
                           (if (listp bitmap)
                               bitmap
                             (list bitmap)))))))

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
      (default-maybe 'bitmap 'flymake-error-bitmap)
      (default-maybe 'face 'flymake-error)
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
    (overlay-put ov 'flymake t)
    (overlay-put ov 'flymake--diagnostic diagnostic)))

(defun flymake-on-timer-event (buffer)
  "Start a syntax check for buffer BUFFER if necessary."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and flymake-mode
		 flymake-last-change-time
                 flymake-no-changes-timeout
		 (> (- (float-time) flymake-last-change-time)
                    flymake-no-changes-timeout))

	(setq flymake-last-change-time nil)
	(flymake-log :debug "starting syntax check after no changes for some time")
	(flymake-start)))))

;; Nothing in Flymake uses this at all any more, so this is just for
;; third-party compatibility.
(define-obsolete-function-alias 'flymake-display-warning 'message-box "26.1")

(defvar-local flymake--backend-state nil
  "Buffer-local hash table of a Flymake backend's state.
The keys to this hash table are functions as found in
`flymake-diagnostic-functions'. The values are plists where the
following keys are possible:

`:running', a symbol to keep track of a backend's replies via its
REPORT-FN argument. A backend is running if this key is
present. If the key is absent if the backend isn't expecting any
replies from the backend.

`:diags', a (possibly empty) list of diagnostic objects created
with `flymake-make-diagnostic'. This key is absent if the
backend hasn't reported anything yet.

`:disabled', a string with the explanation for a previous
exceptional situation reported by the backend. If this key is
present the backend is disabled.")

(defmacro flymake--saving-backend-state (backend state-var &rest body)
  "Bind BACKEND's STATE-VAR to its state, run BODY, then save it."
  (declare (indent 2) (debug (sexp sexp &rest form)))
  (let ((b (make-symbol "b")))
    `(let* ((,b ,backend)
            (,state-var (gethash ,b flymake--backend-state)))
       (unwind-protect
           (progn ,@body)
         (puthash ,b ,state-var flymake--backend-state)))))

(defun flymake-is-running ()
  "Tell if Flymake has running backends in this buffer"
  (flymake-running-backends))

(cl-defun flymake--handle-report (backend token action
                                          &key explanation force)
  "Handle reports from BACKEND identified by TOKEN.

BACKEND, ACTION and EXPLANATION, and FORCE conform to the calling
convention described in `flymake-diagnostic-functions' (which
see). Optional FORCE says to handle a report even if TOKEN was
not expected."
  (let ((state (gethash backend flymake--backend-state)))
    (let (expected-token
          new-diags)
      (cond
       ((null state)
        (flymake-error
         "Unexpected report from unknown backend %s" backend))
       ((cl-getf state :disabled)
        (flymake-error
         "Unexpected report from disabled backend %s" backend))
       ((progn
          (setq expected-token (cl-getf state :running))
          (null expected-token))
        ;; should never happen
        (flymake-error "Unexpected report from stopped backend %s" backend))
       ((and (not (eq expected-token token))
             (not force))
        (flymake-error "Obsolete report from backend %s with explanation %s"
                       backend explanation))
       ((eq :panic action)
        (flymake--disable-backend backend explanation))
       ((not (listp action))
        (flymake--disable-backend backend
                                  (format "Unknown action %S" action))
        (flymake-error "Expected report, but got unknown key %s" action))
       (t
        (setq new-diags action)
        (save-restriction
          (widen)
          (unless (cl-getf state :diags)
            ;; only delete overlays if this is the first batch of
            ;; diagnostics we are receiving.
            (flymake-delete-own-overlays
             (lambda (ov)
               (eq backend
                   (flymake--diag-backend
                    (overlay-get ov 'flymake--diagnostic))))))
          (mapc (lambda (diag)
                  (flymake--highlight-line diag)
                  (setf (flymake--diag-backend diag) backend))
                new-diags)
          (flymake--saving-backend-state backend state
            (setf (cl-getf state :diags)
                  (append new-diags (cl-getf state :diags))))
          (when flymake-check-start-time
            (flymake-log :debug "backend %s reported %d diagnostics in %.2f second(s)"
                         backend
                         (length new-diags)
                         (- (float-time) flymake-check-start-time)))))))))

(defun flymake-make-report-fn (backend &optional token)
  "Make a suitable anonymous report function for BACKEND.
BACKEND is used to help Flymake distinguish different diagnostic
sources.  If provided, TOKEN helps Flymake distinguish between
different runs of the same backend."
  (let ((buffer (current-buffer)))
    (lambda (&rest args)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (apply #'flymake--handle-report backend token args))))))

(defun flymake--collect (fn)
  (let (retval)
    (maphash (lambda (backend state)
               (when (funcall fn state) (push backend retval)))
             flymake--backend-state)
    retval))

(defun flymake-running-backends ()
  "Compute running Flymake backends in current buffer."
  (flymake--collect (lambda (state) (cl-getf state :running))))

(defun flymake-disabled-backends ()
  "Compute disabled Flymake backends in current buffer."
  (flymake--collect (lambda (state) (cl-getf state :disabled))))

(defun flymake-reporting-backends ()
  "Compute reporting Flymake backends in current buffer."
  (flymake--collect (lambda (state) (or (plist-member state :diags)
                                        (plist-member state :disabled)))))

(defun flymake--disable-backend (backend &optional explanation)
  "Disable BACKEND because EXPLANATION.
If is is running also stop it."
  (flymake-log :warning "Disabling backend %s because %s" backend explanation)
  (flymake--saving-backend-state backend state
    (setf (cl-getf state :disabled) explanation)
    (cl-remf state :running)))

(defun flymake--run-backend (backend)
  "Run the backend BACKEND, reenabling if necessary."
  (flymake-log :debug "Running backend %s" backend)
  (let ((run-token (cl-gensym "backend-token")))
    (flymake--saving-backend-state backend state
      (setf (cl-getf state :running) run-token)
      (cl-remf state :disabled)
      (cl-remf state :diags))
    ;; FIXME: Should use `condition-case-unless-debug' here, for don't
    ;; for two reasons: (1) that won't let me catch errors from inside
    ;; `ert-deftest' where `debug-on-error' appears to be always
    ;; t. (2) In cases where the user is debugging elisp somewhere
    ;; else, and using flymake, the presence of a frequently
    ;; misbehaving backend in the global hook (most likely the legacy
    ;; backend) will trigger an annoying backtrace.
    ;;
    (condition-case err
        (funcall backend
                 (flymake-make-report-fn backend run-token))
      (error
       (flymake--disable-backend backend err)))))

(defun flymake-start (&optional deferred force)
  "Start a syntax check.
Start it immediately, or after current command if DEFERRED is
non-nil.  With optional FORCE run even disabled backends.

Interactively, with a prefix arg, FORCE is t."
  (interactive (list nil current-prefix-arg))
  (cl-labels
      ((start
        ()
        (remove-hook 'post-command-hook #'start 'local)
        (setq flymake-check-start-time (float-time))
        (run-hook-wrapped
         'flymake-diagnostic-functions
         (lambda (backend)
           (cond
            ((and (not force)
                  (plist-member (gethash backend flymake--backend-state) :disabled))
             (flymake-log :debug "Backend %s is disabled, not starting"
                          backend))
            (t
             (flymake--run-backend backend)))
           nil))))
    (if (and deferred
             this-command)
        (add-hook 'post-command-hook #'start 'append 'local)
      (start))))

(defvar flymake-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `flymake-mode'.")

;;;###autoload
(define-minor-mode flymake-mode nil
  :group 'flymake :lighter flymake--mode-line-format :keymap flymake-mode-map
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
      (setq flymake--backend-state (make-hash-table))

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
  "Turn Flymake mode on."
  (flymake-mode 1))

;;;###autoload
(defun flymake-mode-off ()
  "Turn Flymake mode off."
  (flymake-mode 0))

(make-obsolete 'flymake-mode-on 'flymake-mode "26.1")
(make-obsolete 'flymake-mode-off 'flymake-mode "26.1")

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
  "Go to Nth next Flymake error in buffer matching FILTER.

Interactively, always move to the next error.  Interactively, and
with a prefix arg, skip any diagnostics with a severity less than
‘:warning’.

If ‘flymake-wrap-around’ is non-nil, resumes search from top
at end of buffer.

FILTER is a list of diagnostic types found in
`flymake-diagnostic-types-alist', or nil, if no filter is to be
applied."
  ;; TODO: let filter be a number, a severity below which diags are
  ;; skipped.
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
           (user-error "No more Flymake errors%s"
                       (if filter
                           (format " of types %s" filter)
                         ""))))))

(defun flymake-goto-prev-error (&optional n filter interactive)
  "Go to Nth previous Flymake error in buffer matching FILTER.

Interactively, always move to the previous error.  Interactively,
and with a prefix arg, skip any diagnostics with a severity less
than ‘:warning’.

If ‘flymake-wrap-around’ is non-nil, resumes search from top
at end of buffer.

FILTER is a list of diagnostic types found in
`flymake-diagnostic-types-alist', or nil, if no filter is to be
applied."
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
  (let ((known (hash-table-keys flymake--backend-state))
        (running (flymake-running-backends))
        (disabled (flymake-disabled-backends))
        (reported (flymake-reporting-backends))
        (diags-by-type (make-hash-table)))
    (maphash (lambda (_b state)
               (mapc (lambda (diag)
                       (push diag
                             (gethash (flymake--diag-type diag)
                                      diags-by-type)))
                     (cl-getf state :diags)))
             flymake--backend-state)
    `((:propertize " Flymake"
                   mouse-face mode-line-highlight
                   help-echo
                   ,(concat (format "%s known backends\n" (length known))
                            (format "%s running\n" (length running))
                            (format "%s disabled\n" (length disabled))
                            "mouse-1: go to log buffer ")
                   keymap
                   ,(let ((map (make-sparse-keymap)))
                      (define-key map [mode-line mouse-1]
                        (lambda (_event)
                          (interactive "e")
                          (switch-to-buffer "*Flymake log*")))
                      map))
      ,@(if (null reported)
            (pcase-let ((`(,ind ,face ,explain)
                         (cond ((null known)
                                `("?" mode-line "No known backends"))
                               (running
                                `("Wait" compilation-mode-line-run
                                  ,(format "Waiting for %s running backends"
                                           (length running))))
                               (disabled
                                `("!" compilation-mode-line-run
                                  "All backends disabled")))))
              `(":"
                (:propertize ,ind
                             face ,face
                             help-echo ,explain)))
          (cl-loop
           for (type . severity)
           in (cl-sort (mapcar (lambda (type)
                                 (cons type (flymake--lookup-type-property
                                             type
                                             'severity
                                             (warning-numeric-level :error))))
                               (cl-union (hash-table-keys diags-by-type)
                                         '(:error :warning)))
                       #'>
                       :key #'cdr)
           for diags = (gethash type diags-by-type)
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
             (:propertize "]")))))))




(provide 'flymake)

(require 'flymake-proc)
(require 'flymake-elisp)
;;; flymake.el ends here
