;;; flymake.el --- A universal on-the-fly syntax checker  -*- lexical-binding: t; -*-

;; Copyright (C) 2003-2022 Free Software Foundation, Inc.

;; Author: Pavel Kobyakov <pk_at_work@yahoo.com>
;; Maintainer: João Távora <joaotavora@gmail.com>
;; Version: 1.2.2
;; Keywords: c languages tools
;; Package-Requires: ((emacs "26.1") (eldoc "1.1.0") (project "0.7.1"))

;; This is a GNU ELPA :core package.  Avoid functionality that is not
;; compatible with the version of Emacs recorded above.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Flymake is a minor Emacs mode performing on-the-fly syntax checks.
;;
;; Flymake collects diagnostic information for multiple sources,
;; called backends, and visually annotates the relevant portions in
;; the buffer.
;;
;; This file contains the UI for displaying and interacting with the
;; results produced by these backends, as well as entry points for
;; backends to hook on to.
;;
;; The main interactive entry point is the `flymake-mode' minor mode,
;; which periodically and automatically initiates checks as the user
;; is editing the buffer.  The variables `flymake-no-changes-timeout',
;; `flymake-start-on-flymake-mode' give finer control over the events
;; triggering a check, as does the interactive command  `flymake-start',
;; which immediately starts a check.
;;
;; Shortly after each check, a summary of collected diagnostics should
;; appear in the mode-line.  If it doesn't, there might not be a
;; suitable Flymake backend for the current buffer's major mode, in
;; which case Flymake will indicate this in the mode-line.  The
;; indicator will be `!' (exclamation mark), if all the configured
;; backends errored (or decided to disable themselves) and `?'
;; (question mark) if no backends were even configured.
;;
;; For programmers interested in writing a new Flymake backend, the
;; docstring of `flymake-diagnostic-functions', the Flymake manual,
;; and the code of existing backends are probably a good starting
;; point.
;;
;; The user wishing to customize the appearance of error types should
;; set properties on the symbols associated with each diagnostic type.
;; The standard diagnostic symbols are `:error', `:warning' and
;; `:note' (though a specific backend may define and use more).  The
;; following properties can be set:
;;
;; * `flymake-bitmap', an image displayed in the fringe according to
;; `flymake-fringe-indicator-position'.  The value actually follows
;; the syntax of `flymake-error-bitmap' (which see).  It is overridden
;; by any `before-string' overlay property.
;;
;; * `flymake-severity', a non-negative integer specifying the
;; diagnostic's severity.  The higher, the more serious.  If the
;; overlay property `priority' is not specified, `severity' is used to
;; set it and help sort overlapping overlays.
;;
;; * `flymake-overlay-control', an alist ((OVPROP . VALUE) ...) of
;; further properties used to affect the appearance of Flymake
;; annotations.  With the exception of `category' and `evaporate',
;; these properties are applied directly to the created overlay.  See
;; Info Node `(elisp)Overlay Properties'.
;;
;; * `flymake-category', a symbol whose property list is considered a
;; default for missing values of any other properties.  This is useful
;; to backend authors when creating new diagnostic types that differ
;; from an existing type by only a few properties.  The category
;; symbols `flymake-error', `flymake-warning' and `flymake-note' make
;; good candidates for values of this property.
;;
;; For instance, to omit the fringe bitmap displayed for the standard
;; `:note' type, set its `flymake-bitmap' property to nil:
;;
;;   (put :note 'flymake-bitmap nil)
;;
;; To change the face for `:note' type, add a `face' entry to its
;; `flymake-overlay-control' property.
;;
;;   (push '(face . highlight) (get :note 'flymake-overlay-control))
;;
;; If you push another alist entry in front, it overrides the previous
;; one.  So this effectively removes the face from `:note'
;; diagnostics.
;;
;;   (push '(face . nil) (get :note 'flymake-overlay-control))
;;
;; To erase customizations and go back to the original look for
;; `:note' types:
;;
;;   (cl-remf (symbol-plist :note) 'flymake-overlay-control)
;;   (cl-remf (symbol-plist :note) 'flymake-bitmap)
;;
;;; Code:

(require 'cl-lib)
(require 'thingatpt) ; end-of-thing
(require 'warnings) ; warning-numeric-level, display-warning
(require 'compile) ; for some faces
;; We need the next require to avoid compiler warnings and run-time
;; errors about mouse-wheel-up/down-event in builds --without-x, where
;; mwheel is not preloaded.
(require 'mwheel)
;; when-let*, if-let*, hash-table-keys, hash-table-values:
(eval-when-compile (require 'subr-x))
(require 'project)

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

(make-obsolete-variable 'flymake-start-syntax-check-on-newline
		        "can check on newline in post-self-insert-hook"
                        "27.1")

(defcustom flymake-no-changes-timeout 0.5
  "Time to wait after last change before automatically checking buffer.
If nil, never start checking buffer automatically like this."
  :type '(choice (number :tag "Timeout in seconds")
                 (const :tag "No check on timeout" nil)))

(defcustom flymake-gui-warnings-enabled t
  "Enables/disables GUI warnings."
  :type 'boolean)
(make-obsolete-variable 'flymake-gui-warnings-enabled
			"it no longer has any effect." "26.1")

(define-obsolete-variable-alias 'flymake-start-syntax-check-on-find-file
  'flymake-start-on-flymake-mode "26.1")

(defcustom flymake-start-on-flymake-mode t
  "If non-nil, start syntax check when `flymake-mode' is enabled.
Specifically, start it when the buffer is actually displayed."
  :version "26.1"
  :type 'boolean)

(defcustom flymake-start-on-save-buffer t
  "If non-nil, start syntax check when a buffer is saved.
Specifically, start it when the saved buffer is actually displayed."
  :version "27.1"
  :type 'boolean)

(defcustom flymake-log-level -1
  "Obsolete and ignored variable."
  :type 'integer)
(make-obsolete-variable 'flymake-log-level
			"it is superseded by `warning-minimum-log-level.'"
                        "26.1")

(defcustom flymake-wrap-around t
  "If non-nil, moving to errors wraps around buffer boundaries."
  :version "26.1"
  :type 'boolean)

(defcustom flymake-suppress-zero-counters :warning
  "Control appearance of zero-valued diagnostic counters in mode line.

If set to t, suppress all zero counters.  If set to a severity
symbol like `:warning' (the default) suppress zero counters less
severe than that severity, according to `warning-numeric-level'.
If set to nil, don't suppress any zero counters."
  :type 'symbol)

(when (fboundp 'define-fringe-bitmap)
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
            #b00000000)))

(defvar-local flymake-timer nil
  "Timer for starting syntax check.")

(defvar-local flymake-check-start-time nil
  "Time at which syntax check was started.")

(defun flymake--log-1 (level sublog msg &rest args)
  "Do actual work for `flymake-log'."
  (let (;; never popup the log buffer
        (warning-minimum-level :emergency)
        (warning-type-format
         (format " [%s %s]"
                 (or sublog 'flymake)
                 ;; Handle file names with "%" correctly.  (Bug#51549)
                 (replace-regexp-in-string "%" "%%"
                                           (buffer-name (current-buffer))))))
    (display-warning (list 'flymake sublog)
                     (apply #'format-message msg args)
                     (if (numberp level)
                         (or (nth level
                                  '(:emergency :error :warning :debug :debug) )
                             :error)
                       level)
                     "*Flymake log*")))

(defun flymake-switch-to-log-buffer ()
  "Go to the *Flymake log* buffer."
  (interactive)
  (switch-to-buffer "*Flymake log*"))

;;;###autoload
(defmacro flymake-log (level msg &rest args)
  "Log, at level LEVEL, the message MSG formatted with ARGS.
LEVEL is passed to `display-warning', which is used to display
the warning.  If this form is included in a file,
the generated warning contains an indication of the file that
generated it."
  (let* ((file (if (fboundp 'macroexp-file-name)
                   (macroexp-file-name)
                 (and (not load-file-name)
                      (bound-and-true-p byte-compile-current-file))))
         (sublog (if (stringp file)
                     (intern
                      (file-name-nondirectory
                       (file-name-sans-extension file))))))
    `(flymake--log-1 ,level ',sublog ,msg ,@args)))

(defun flymake-error (text &rest args)
  "Format TEXT with ARGS and signal an error for Flymake."
  (let ((msg (apply #'format-message text args)))
    (flymake-log :error msg)
    (error (concat "[Flymake] " msg))))

(cl-defstruct (flymake--diag
               (:constructor flymake--diag-make))
  locus beg end type text backend data overlay-properties overlay
  ;; FIXME: See usage of these two in `flymake--highlight-line'.
  ;; Ideally they wouldn't be needed.
  orig-beg orig-end)

;;;###autoload
(defun flymake-make-diagnostic (locus
                                beg
                                end
                                type
                                text
                                &optional data
                                overlay-properties)
  "Make a Flymake diagnostic for LOCUS's region from BEG to END.
LOCUS is a buffer object or a string designating a file name.

TYPE is a diagnostic symbol and TEXT is string describing the
problem detected in this region.  DATA is any object that the
caller wishes to attach to the created diagnostic for later
retrieval with `flymake-diagnostic-data'.

If LOCUS is a buffer BEG and END should be buffer positions
inside it.  If LOCUS designates a file, BEG and END should be a
cons (LINE . COL) indicating a file position.  In this second
case, END may be omitted in which case the region is computed
using `flymake-diag-region' if the diagnostic is appended to an
actual buffer.

OVERLAY-PROPERTIES is an alist of properties attached to the
created diagnostic, overriding the default properties and any
properties listed in the `flymake-overlay-control' property of
the diagnostic's type symbol."
  (when (stringp locus)
    (setq locus (expand-file-name locus)))
  (flymake--diag-make :locus locus :beg beg :end end
                      :type type :text text :data data
                      :overlay-properties overlay-properties
                      :orig-beg beg
                      :orig-end end))

;;;###autoload
(defun flymake-diagnostics (&optional beg end)
  "Get Flymake diagnostics in region determined by BEG and END.

If neither BEG or END is supplied, use whole accessible buffer,
otherwise if BEG is non-nil and END is nil, consider only
diagnostics at BEG."
  (mapcar (lambda (ov) (overlay-get ov 'flymake-diagnostic))
          (flymake--overlays :beg beg :end end)))

(defmacro flymake--diag-accessor (public internal thing)
  "Make PUBLIC an alias for INTERNAL, add doc using THING."
  `(defsubst ,public (diag)
     ,(format "Get Flymake diagnostic DIAG's %s." (symbol-name thing))
     (,internal diag)))

(flymake--diag-accessor flymake-diagnostic-text flymake--diag-text text)
(flymake--diag-accessor flymake-diagnostic-type flymake--diag-type type)
(flymake--diag-accessor flymake-diagnostic-backend flymake--diag-backend backend)
(flymake--diag-accessor flymake-diagnostic-data flymake--diag-data data)
(flymake--diag-accessor flymake-diagnostic-beg flymake--diag-beg beg)
(flymake--diag-accessor flymake-diagnostic-end flymake--diag-end end)
(flymake--diag-accessor flymake-diagnostic-buffer flymake--diag-locus locus)

(cl-defun flymake--overlays (&key beg end filter compare key)
  "Get flymake-related overlays.
If BEG is non-nil and END is nil, consider only `overlays-at'
BEG.  Otherwise consider `overlays-in' the region comprised by BEG
and END, defaulting to the whole buffer.  Remove all that do not
verify FILTER, a function, and sort them by COMPARE (using KEY)."
  (save-restriction
    (widen)
    (let ((ovs (cl-remove-if-not
                (lambda (ov)
                  (and (overlay-get ov 'flymake-diagnostic)
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

;;;###autoload
(defun flymake-diag-region (buffer line &optional col)
  "Compute BUFFER's region (BEG . END) corresponding to LINE and COL.
If COL is nil, return a region just for LINE.  Return nil if the
region is invalid.  This function saves match data."
  (condition-case-unless-debug _err
      (with-current-buffer buffer
        (let ((line (min (max line 1)
                         (line-number-at-pos (point-max) 'absolute))))
          (save-excursion
            (save-match-data
              (goto-char (point-min))
              (forward-line (1- line))
              (cl-flet ((fallback-bol
                         ()
                         (back-to-indentation)
                         (if (eobp)
                             (line-beginning-position 0)
                           (point)))
                        (fallback-eol
                         (beg)
                         (progn
                           (end-of-line)
                           (skip-chars-backward " \t\f\n" beg)
                           (if (eq (point) beg)
                               (line-beginning-position 2)
                             (point)))))
                (if (and col (cl-plusp col))
                    (let* ((beg (progn (forward-char (1- col))
                                       (point)))
                           (sexp-end (or (ignore-errors (end-of-thing 'sexp))
                                         (ignore-errors (end-of-thing 'symbol))))
                           (end (or (and sexp-end
                                         (not (= sexp-end beg))
                                         sexp-end)
                                    (and (< (goto-char (1+ beg)) (point-max))
                                         (point)))))
                      (if end
                          (cons beg end)
                        (cons (setq beg (fallback-bol))
                              (fallback-eol beg))))
                  (let* ((beg (fallback-bol))
                         (end (fallback-eol beg)))
                    (cons beg end))))))))
    (error (flymake-log :warning "Invalid region line=%s col=%s" line col)
           nil)))

(defvar flymake-diagnostic-functions nil
  "Special hook of Flymake backends that check a buffer.

The functions in this hook diagnose problems in a buffer's
contents and provide information to the Flymake user interface
about where and how to annotate problems diagnosed in a buffer.

Each backend function must be prepared to accept an arbitrary
number of arguments:

* the first argument is always REPORT-FN, a callback function
  detailed below;

* the remaining arguments are keyword-value pairs in the
  form (:KEY VALUE :KEY2 VALUE2...).

Currently, Flymake may provide these keyword-value pairs:

* `:recent-changes', a list of recent changes since the last time
  the backend function was called for the buffer.  An empty list
  indicates that no changes have been recorded.  If it is the
  first time that this backend function is called for this
  activation of `flymake-mode', then this argument isn't provided
  at all (i.e. it's not merely nil).

  Each element is in the form (BEG END TEXT) where BEG and END
  are buffer positions, and TEXT is a string containing the text
  contained between those positions (if any) after the change was
  performed.

* `:changes-start' and `:changes-end', the minimum and maximum
  buffer positions touched by the recent changes.  These are only
  provided if `:recent-changes' is also provided.

Whenever Flymake or the user decides to re-check the buffer,
backend functions are called as detailed above and are expected
to initiate this check, but aren't required to complete it before
exiting: if the computation involved is expensive, especially for
large buffers, that task can be scheduled for the future using
asynchronous processes or other asynchronous mechanisms.

In any case, backend functions are expected to return quickly or
signal an error, in which case the backend is disabled.  Flymake
will not try disabled backends again for any future checks of
this buffer.  To reset the list of disabled backends, turn
`flymake-mode' off and on again, or interactively call
`flymake-start' with a prefix argument.

If the function returns, Flymake considers the backend to be
\"running\".  If it has not done so already, the backend is
expected to call the function REPORT-FN with a single argument
REPORT-ACTION also followed by an optional list of keyword-value
pairs in the form (:REPORT-KEY VALUE :REPORT-KEY2 VALUE2...).

Currently accepted values for REPORT-ACTION are:

* A (possibly empty) list of diagnostic objects created with
  `flymake-make-diagnostic', causing Flymake to delete all
  previous diagnostic annotations in the buffer and create new
  ones from this list.

  A backend may call REPORT-FN repeatedly in this manner, but
  only until Flymake considers that the most recently requested
  buffer check is now obsolete because, say, buffer contents have
  changed in the meantime.  The backend is only given notice of
  this via a renewed call to the backend function.  Thus, to
  prevent making obsolete reports and wasting resources, backend
  functions should first cancel any ongoing processing from
  previous calls.

* The symbol `:panic', signaling that the backend has encountered
  an exceptional situation and should be disabled.

Currently accepted REPORT-KEY arguments are:

* `:explanation' value should give user-readable details of
  the situation encountered, if any.

* `:force': value should be a boolean suggesting that Flymake
  consider the report even if it was somehow unexpected.

* `:region': a cons (BEG . END) of buffer positions indicating
  that the report applies to that region only.  Specifically,
  this means that Flymake will only delete diagnostic annotations
  of past reports if they intersect the region by at least one
  character.")

(put 'flymake-diagnostic-functions 'safe-local-variable #'null)

(put :error 'flymake-category 'flymake-error)
(put :warning 'flymake-category 'flymake-warning)
(put :note 'flymake-category 'flymake-note)

(defvar flymake-diagnostic-types-alist '())
(make-obsolete-variable
 'flymake-diagnostic-types-alist
 "Set properties on the diagnostic symbols instead.  See Info
Node `(Flymake)Flymake error types'"
 "27.1")

(put 'flymake-error 'face 'flymake-error)
(put 'flymake-error 'flymake-bitmap 'flymake-error-bitmap)
(put 'flymake-error 'severity (warning-numeric-level :error))
(put 'flymake-error 'mode-line-face 'compilation-error)
(put 'flymake-error 'flymake-type-name "error")

(put 'flymake-warning 'face 'flymake-warning)
(put 'flymake-warning 'flymake-bitmap 'flymake-warning-bitmap)
(put 'flymake-warning 'severity (warning-numeric-level :warning))
(put 'flymake-warning 'mode-line-face 'compilation-warning)
(put 'flymake-warning 'flymake-type-name "warning")

(put 'flymake-note 'face 'flymake-note)
(put 'flymake-note 'flymake-bitmap 'flymake-note-bitmap)
(put 'flymake-note 'severity (warning-numeric-level :debug))
(put 'flymake-note 'mode-line-face 'compilation-info)
(put 'flymake-note 'flymake-type-name "note")

(defun flymake--lookup-type-property (type prop &optional default)
  "Look up PROP for diagnostic TYPE.
If TYPE doesn't declare PROP in its plist or in the symbol of its
associated `flymake-category' return DEFAULT."
  ;; This function also consults `flymake-diagnostic-types-alist' for
  ;; backward compatibility.
  ;;
  (if (plist-member (symbol-plist type) prop)
      ;; allow nil values to survive
      (get type prop)
    (let (alist)
      (or
       (alist-get
        prop (setq
              alist
              (alist-get type flymake-diagnostic-types-alist)))
       (when-let* ((cat (or
                         (get type 'flymake-category)
                         (alist-get 'flymake-category alist)))
                   (plist (and (symbolp cat)
                               (symbol-plist cat)))
                   (cat-probe (plist-member plist prop)))
         (cadr cat-probe))
       default))))

(defun flymake--severity (type)
  "Get the severity for diagnostic TYPE."
  (flymake--lookup-type-property type 'severity
                                 (warning-numeric-level :error)))

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

(defun flymake--equal-diagnostic-p (a b)
  "Tell if A and B are equivalent `flymake--diag' objects."
  (or (eq a b)
      (cl-loop for comp in '(flymake--diag-end
                             flymake--diag-beg
                             flymake-diagnostic-type
                             flymake-diagnostic-backend
                             flymake-diagnostic-text)
               always (equal (funcall comp a) (funcall comp b)))))

(cl-defun flymake--highlight-line (diagnostic &optional foreign)
  "Attempt to overlay DIAGNOSTIC in current buffer.

FOREIGN says if DIAGNOSTIC is \"foreign\" to the current buffer,
i.e. managed by another buffer where `flymake-mode' is also
active.

This function mayskip overlay creation if a diagnostic which is
the same as DIAGNOSTIC is already highlighted
(in the sense of `flymake--equal-diagnostic-p').  In that case
the action to take depends on FOREIGN.  If nil the existing
overlay is deleted, else no overlay is created.

Return nil or the overlay created."
  (let* ((type (or (flymake-diagnostic-type diagnostic)
                   :error))
         (beg (flymake--diag-beg diagnostic))
         (end (flymake--diag-end diagnostic))
         (convert (lambda (cell)
                    (flymake-diag-region (current-buffer)
                                         (car cell)
                                         (cdr cell))))
         ov)
    ;; Convert (LINE . COL) forms of `flymake--diag-beg' and
    ;; `flymake--diag-end'.  Record the converted positions.
    ;;
    (cond ((and (consp beg) (not (null end)))
           (setq beg (car (funcall convert beg)))
           (when (consp end)
             (setq end (car (funcall convert end)))))
          ((consp beg)
           (cl-destructuring-bind (a . b) (funcall convert beg)
             (setq beg a end b))))
    (setf (flymake--diag-beg diagnostic) beg
          (flymake--diag-end diagnostic) end)
    ;; Try to fix the remedy the situation if there is the same
    ;; diagnostic is already registered in the same place, which only
    ;; happens for clashes between domestic and foreign diagnostics
    (cl-loop for e in (flymake-diagnostics beg end)
             when (flymake--equal-diagnostic-p e diagnostic)
             ;; FIXME.  This is an imperfect heuristic.  Ideally, we'd
             ;; want to delete no overlays and keep annotating the
             ;; superseded foreign in an overlay but hide it from most
             ;; `flymake-diagnostics' calls.  If the target buffer is
             ;; killed we can keep the "latent" state of the foreign
             ;; diagnostic (with filename and updated line/col info).
             ;; If it is revisited the foreign diagnostic can be
             ;; revived again.
             do (if foreign
                    (cl-return-from flymake--highlight-line nil)
                  (setf (flymake--diag-beg e)
                        (flymake--diag-orig-beg e)
                        (flymake--diag-end e)
                        (flymake--diag-orig-end e))
                  (delete-overlay (flymake--diag-overlay e))))
    (setq ov (make-overlay end beg))
    (setf (flymake--diag-beg diagnostic) (overlay-start ov)
          (flymake--diag-end diagnostic) (overlay-end ov))
    ;; First set `category' in the overlay
    ;;
    (overlay-put ov 'category
                 (flymake--lookup-type-property type 'flymake-category))
    ;; Now "paint" the overlay with all the other non-category
    ;; properties.
    (cl-loop
     for (ov-prop . value) in
     (append (reverse
              (flymake--diag-overlay-properties diagnostic))
             (reverse ; ensure earlier props override later ones
              (flymake--lookup-type-property type 'flymake-overlay-control))
             (alist-get type flymake-diagnostic-types-alist))
     do (overlay-put ov ov-prop value))
    ;; Now ensure some essential defaults are set
    ;;
    (cl-flet ((default-maybe
                (prop value)
                (unless (plist-member (overlay-properties ov) prop)
                  (overlay-put ov prop (flymake--lookup-type-property
                                        type prop value)))))
      (default-maybe 'face 'flymake-error)
      (default-maybe 'before-string
        (flymake--fringe-overlay-spec
         (flymake--lookup-type-property
          type
          'flymake-bitmap
          (alist-get 'bitmap (alist-get type ; backward compat
                                        flymake-diagnostic-types-alist)))))
      (default-maybe 'help-echo
        (lambda (window _ov pos)
          (with-selected-window window
            (mapconcat
             #'flymake-diagnostic-text
             (flymake-diagnostics pos)
             "\n"))))
      (default-maybe 'severity (warning-numeric-level :error))
      ;; Use (PRIMARY . SECONDARY) priority, to avoid clashing with
      ;; `region' face, for example (bug#34022).
      (default-maybe 'priority (cons nil (+ 40 (overlay-get ov 'severity)))))
    ;; Some properties can't be overridden.
    ;;
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'flymake-diagnostic diagnostic)
    (setf (flymake--diag-overlay diagnostic) ov)
    ov))

;; Nothing in Flymake uses this at all any more, so this is just for
;; third-party compatibility.
(define-obsolete-function-alias 'flymake-display-warning 'message-box "26.1")

(defvar-local flymake--state nil
  "State of a buffer's multiple Flymake backends.
The keys to this hash table are functions as found in
`flymake-diagnostic-functions'.  The values are structures
of the type `flymake--state', with these slots:

`running', a symbol to keep track of a backend's replies via its
REPORT-FN argument.  A backend is running if this key is
present.  If nil, Flymake isn't expecting any replies from the
backend.

`diags', a (possibly empty) list of recent diagnostic objects
created by the backend with `flymake-make-diagnostic'.

`reported-p', a boolean indicating if the backend has replied
since it last was contacted.

`disabled', a string with the explanation for a previous
exceptional situation reported by the backend, nil if the
backend is operating normally.

`foreign-diags', a hash table of buffers/files to
collections of diagnostics outside the buffer where this
`flymake--state' pertains.")

(cl-defstruct (flymake--state
               (:constructor flymake--make-backend-state))
  running reported-p disabled diags (foreign-diags
                                     (make-hash-table)))

(defmacro flymake--with-backend-state (backend state-var &rest body)
  "Bind BACKEND's STATE-VAR to its state, run BODY."
  (declare (indent 2) (debug (sexp sexp &rest form)))
  (let ((b (make-symbol "b")))
    `(let* ((,b ,backend)
            (,state-var
             (or (gethash ,b flymake--state)
                 (puthash ,b (flymake--make-backend-state)
                          flymake--state))))
       ,@body)))

(defun flymake-is-running ()
  "Tell if Flymake has running backends in this buffer."
  (flymake-running-backends))

;; FIXME: clone of `isearch-intersects-p'! Make this an util.
(defun flymake--intersects-p (start0 end0 start1 end1)
  "Return t if regions START0..END0 and START1..END1 intersect."
  (or (and (>= start0 start1) (<  start0 end1))
      (and (>  end0 start1)   (<= end0 end1))
      (and (>= start1 start0) (<  start1 end0))
      (and (>  end1 start0)   (<= end1 end0))))

(cl-defun flymake--handle-report
    (backend token report-action
             &key explanation force region
             &allow-other-keys)
  "Handle reports from BACKEND identified by TOKEN.
BACKEND, REPORT-ACTION and EXPLANATION, and FORCE conform to the
calling convention described in
`flymake-diagnostic-functions' (which see).  Optional FORCE says
to handle a report even if TOKEN was not expected.  REGION is
a (BEG . END) pair of buffer positions indicating that this
report applies to that region."
  (let ((state (or (gethash backend flymake--state)
                   (error "Can't find state for %s in `flymake--state'"
                          backend)))
        expected-token)
    (cond
     ((null state)
      (flymake-error
       "Unexpected report from unknown backend %s" backend))
     ((flymake--state-disabled state)
      (flymake-error
       "Unexpected report from disabled backend %s" backend))
     ((progn
        (setq expected-token (flymake--state-running state))
        (null expected-token))
      ;; should never happen
      (flymake-error "Unexpected report from stopped backend %s" backend))
     ((not (or (eq expected-token token)
               force))
      (flymake-error "Obsolete report from backend %s with explanation %s"
                     backend explanation))
     ((eq :panic report-action)
      (flymake--disable-backend backend explanation))
     ((not (listp report-action))
      (flymake--disable-backend backend
                                (format "Unknown action %S" report-action))
      (flymake-error "Expected report, but got unknown key %s" report-action))
     (t
      (flymake--publish-diagnostics report-action
                                    :backend backend
                                    :state state
                                    :region region)
      (when flymake-check-start-time
        (flymake-log :debug "backend %s reported %d diagnostics in %.2f second(s)"
                     backend
                     (length report-action)
                     (float-time
                      (time-since flymake-check-start-time))))))
    (setf (flymake--state-reported-p state) t)
    (flymake--update-diagnostics-listings (current-buffer))))

(defun flymake--clear-foreign-diags (state)
  (maphash (lambda (_buffer diags)
             (cl-loop for d in diags
                      when (flymake--diag-overlay d)
                      do (delete-overlay it)))
           (flymake--state-foreign-diags state))
  (clrhash (flymake--state-foreign-diags state)))

(defvar-local flymake-mode nil)

(cl-defun flymake--publish-diagnostics (diags &key backend state region)
  "Helper for `flymake--handle-report'.
Publish DIAGS, which contain diagnostics for the current buffer
and other buffers."
  (dolist (d diags) (setf (flymake--diag-backend d) backend))
  (save-restriction
    (widen)
    ;; First, clean up.  Remove diagnostics from bookkeeping lists and
    ;; their overlays from buffers.
    ;;
    (cond
     (;; If there is a `region' arg, only affect the diagnostics whose
      ;; overlays are in a certain region.  Discard "foreign"
      ;; diagnostics.
      region
      (cl-loop for diag in (flymake--state-diags state)
               for ov = (flymake--diag-overlay diag)
               if (or (not (overlay-buffer ov))
                      (flymake--intersects-p
                       (overlay-start ov) (overlay-end ov)
                       (car region) (cdr region)))
               do (delete-overlay ov)
               else collect diag into surviving
               finally (setf (flymake--state-diags state)
                             surviving)))
     (;; Else, if this is the first report, zero all lists and delete
      ;; all associated overlays.
      (not (flymake--state-reported-p state))
      (cl-loop for diag in (flymake--state-diags state)
               for ov = (flymake--diag-overlay diag)
               when ov do (delete-overlay ov))
      (setf (flymake--state-diags state) nil)
      ;; Also clear all overlays for `foreign-diags' in all other
      ;; buffers.
      (flymake--clear-foreign-diags state))
     (;; If this is not the first report, do no cleanup.
       t))

    ;; Now place new overlays for all diagnostics: "domestic"
    ;; diagnostics are for the current buffer; "foreign" may be for a
    ;; some other live buffer or for a file name that hasn't a buffer
    ;; yet.  If a foreign diagnostic is for a buffer, convert to a
    ;; file name, protecting it against that buffer's killing.
    ;;
    (cl-loop
     for d in diags
     for locus = (flymake--diag-locus d)
     do (cond ((eq locus (current-buffer))
               (push d (flymake--state-diags state))
               (flymake--highlight-line d))
              (t
               (when (or (buffer-live-p locus)
                         (setq locus (find-buffer-visiting locus)))
                 (with-current-buffer locus
                   (when flymake-mode (flymake--highlight-line d 'foreign))
                   ;; Ensure locus of a foreign diag is always a file-name
                   ;; string, even if created from a buffer.
                   (setf (flymake--diag-locus d) (buffer-file-name))))
               (cl-assert (stringp (flymake--diag-locus d)))
               (push d (gethash (flymake--diag-locus d)
                                (flymake--state-foreign-diags state))))))))

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

(defun flymake--collect (fn &optional message-prefix)
  "Collect Flymake backends matching FN.
If MESSAGE-PREFIX, echo a message using that prefix."
  (unless flymake--state
    (user-error "Flymake is not initialized"))
  (let (retval)
    (maphash (lambda (backend state)
               (when (funcall fn state) (push backend retval)))
             flymake--state)
    (when message-prefix
      (message "%s%s"
               message-prefix
               (mapconcat (lambda (s) (format "%s" s))
                          retval ", ")))
    retval))

(defun flymake-running-backends ()
  "Compute running Flymake backends in current buffer."
  (interactive)
  (flymake--collect #'flymake--state-running
                    (and (called-interactively-p 'interactive)
                         "Running backends: ")))

(defun flymake-disabled-backends ()
  "Compute disabled Flymake backends in current buffer."
  (interactive)
  (flymake--collect #'flymake--state-disabled
                    (and (called-interactively-p 'interactive)
                         "Disabled backends: ")))

(defun flymake-reporting-backends ()
  "Compute reporting Flymake backends in current buffer."
  (interactive)
  (flymake--collect #'flymake--state-reported-p
                    (and (called-interactively-p 'interactive)
                         "Reporting backends: ")))

(defun flymake--disable-backend (backend &optional explanation)
  "Disable BACKEND because EXPLANATION.
If it is running also stop it."
  (flymake-log :warning "Disabling backend %s because %s" backend explanation)
  (flymake--with-backend-state backend state
    (setf (flymake--state-running state) nil
          (flymake--state-disabled state) explanation
          (flymake--state-reported-p state) t)))

(defun flymake--run-backend (backend &optional args)
  "Run the backend BACKEND, re-enabling if necessary.
ARGS is a keyword-value plist passed to the backend along
with a report function."
  (flymake-log :debug "Running backend %s" backend)
  (let ((run-token (cl-gensym "backend-token")))
    (flymake--with-backend-state backend state
      (setf (flymake--state-running state) run-token
            (flymake--state-disabled state) nil
            (flymake--state-reported-p state) nil))
    ;; FIXME: Should use `condition-case-unless-debug' here, but don't
    ;; for two reasons: (1) that won't let me catch errors from inside
    ;; `ert-deftest' where `debug-on-error' appears to be always
    ;; t. (2) In cases where the user is debugging elisp somewhere
    ;; else, and using flymake, the presence of a frequently
    ;; misbehaving backend in the global hook (most likely the legacy
    ;; backend) will trigger an annoying backtrace.
    ;;
    (condition-case err
        (apply backend (flymake-make-report-fn backend run-token)
               args)
      (error
       (flymake--disable-backend backend err)))))

(defvar-local flymake--recent-changes nil
  "Recent changes collected by `flymake-after-change-function'.")
(defvar flymake-mode)

(defun flymake-start (&optional deferred force)
  "Start a syntax check for the current buffer.
DEFERRED is a list of symbols designating conditions to wait for
before actually starting the check.  If it is nil (the list is
empty), start it immediately, else defer the check to when those
conditions are met.  Currently recognized conditions are
`post-command', for waiting until the current command is over,
`on-display', for waiting until the buffer is actually displayed
in a window.  If DEFERRED is t, wait for all known conditions.

With optional FORCE run even disabled backends.

Interactively, with a prefix arg, FORCE is t."
  (interactive (list nil current-prefix-arg))
  (let ((deferred (if (eq t deferred)
                      '(post-command on-display)
                    deferred))
        (buffer (current-buffer)))
    (cl-labels
        ((start-post-command
          ()
          (remove-hook 'post-command-hook #'start-post-command
                       nil)
          ;; The buffer may have disappeared already, e.g. because of
          ;; code like `(with-temp-buffer (python-mode) ...)'.
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (flymake-start (remove 'post-command deferred) force))))
         (start-on-display
          ()
          (remove-hook 'window-configuration-change-hook #'start-on-display
                       'local)
          (flymake-start (remove 'on-display deferred) force)))
      (cond ((and (memq 'post-command deferred)
                  this-command)
             (add-hook 'post-command-hook
                       #'start-post-command
                       'append nil))
            ((and (memq 'on-display deferred)
                  (not (get-buffer-window (current-buffer))))
             (add-hook 'window-configuration-change-hook
                       #'start-on-display
                       'append 'local))
            (flymake-mode
             (setq flymake-check-start-time (float-time))
             (let ((backend-args
                    (and
                     flymake--recent-changes
                     (list :recent-changes
                           flymake--recent-changes
                           :changes-start
                           (cl-reduce
                            #'min (mapcar #'car flymake--recent-changes))
                           :changes-end
                           (cl-reduce
                            #'max (mapcar #'cadr flymake--recent-changes))))))
               (setq flymake--recent-changes nil)
               (run-hook-wrapped
                'flymake-diagnostic-functions
                (lambda (backend)
                  (cond
                   ((and (not force)
                         (flymake--with-backend-state backend state
                           (flymake--state-disabled state)))
                    (flymake-log :debug "Backend %s is disabled, not starting"
                                 backend))
                   (t
                    (flymake--run-backend backend backend-args)))
                  nil))))))))

(defvar flymake-mode-map
  (let ((map (make-sparse-keymap))) map)
  "Keymap for `flymake-mode'.")

;;;###autoload
(define-minor-mode flymake-mode
  "Toggle Flymake mode on or off.

Flymake is an Emacs minor mode for on-the-fly syntax checking.
Flymake collects diagnostic information from multiple sources,
called backends, and visually annotates the buffer with the
results.

Flymake performs these checks while the user is editing.
The customization variables `flymake-start-on-flymake-mode',
`flymake-no-changes-timeout' determine the exact circumstances
whereupon Flymake decides to initiate a check of the buffer.

The commands `flymake-goto-next-error' and
`flymake-goto-prev-error' can be used to navigate among Flymake
diagnostics annotated in the buffer.

The visual appearance of each type of diagnostic can be changed
by setting properties `flymake-overlay-control', `flymake-bitmap'
and `flymake-severity' on the symbols of diagnostic types (like
`:error', `:warning' and `:note').

Activation or deactivation of backends used by Flymake in each
buffer happens via the special hook
`flymake-diagnostic-functions'.

Some backends may take longer than others to respond or complete,
and some may decide to disable themselves if they are not
suitable for the current buffer.  The commands
`flymake-running-backends', `flymake-disabled-backends' and
`flymake-reporting-backends' summarize the situation, as does the
special *Flymake log* buffer."  :group 'flymake :lighter
  flymake-mode-line-format :keymap flymake-mode-map
  (cond
   ;; Turning the mode ON.
   (flymake-mode
    (add-hook 'after-change-functions 'flymake-after-change-function nil t)
    (add-hook 'after-save-hook 'flymake-after-save-hook nil t)
    (add-hook 'kill-buffer-hook 'flymake-kill-buffer-hook nil t)
    (add-hook 'eldoc-documentation-functions 'flymake-eldoc-function t t)

    ;; If Flymake happened to be already already ON, we must cleanup
    ;; existing diagnostic overlays, lest we forget them by blindly
    ;; reinitializing `flymake--state' in the next line.
    ;; See https://github.com/joaotavora/eglot/issues/223.
    (mapc #'delete-overlay (flymake--overlays))
    (setq flymake--state (make-hash-table))
    (setq flymake--recent-changes nil)

    (when flymake-start-on-flymake-mode (flymake-start t))

    ;; Other diagnostic sources may already target this buffer's file
    ;; before we turned on: these sources may be of two types...
    (let ((source (current-buffer))
          (bfn buffer-file-name))
      ;; 1. For `flymake-list-only-diagnostics': here, we do nothing.
      ;; FIXME: We could remove the corresponding entry from that
      ;; variable, as we assume that new diagnostics will come in soon
      ;; via the brand new `flymake-mode' setup.  For simplicity's
      ;; sake, we have opted to leave the backend for now.
      nil
      ;; 2. other buffers where a backend has created "foreign"
      ;; diagnostics and pointed them here.  We must highlight them in
      ;; this buffer, i.e. create overlays for them.  Those other
      ;; buffers and backends are still responsible for them, i.e. the
      ;; current buffer does not "own" these foreign diags.
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (and flymake-mode flymake--state)
            (maphash (lambda (_backend state)
                       (maphash (lambda (file diags)
                                  (when (or (eq file source)
                                            (string= bfn (expand-file-name file)))
                                    (with-current-buffer source
                                      (mapc (lambda (diag)
                                              (flymake--highlight-line diag
                                                                       'foreign))
                                            diags))))
                                (flymake--state-foreign-diags state)))
                     flymake--state))))))

   ;; Turning the mode OFF.
   (t
    (remove-hook 'after-change-functions 'flymake-after-change-function t)
    (remove-hook 'after-save-hook 'flymake-after-save-hook t)
    (remove-hook 'kill-buffer-hook 'flymake-kill-buffer-hook t)
    ;;+(remove-hook 'find-file-hook (function flymake-find-file-hook) t)
    (remove-hook 'eldoc-documentation-functions 'flymake-eldoc-function t)

    (when flymake-timer
      (cancel-timer flymake-timer)
      (setq flymake-timer nil))
    (mapc #'delete-overlay (flymake--overlays))
    (when flymake--state
      (maphash (lambda (_backend state)
                 (flymake--clear-foreign-diags state))
               flymake--state)))
   ;; turning Flymake on or off has consequences for listings
   (flymake--update-diagnostics-listings (current-buffer))))

(defun flymake--schedule-timer-maybe ()
  "(Re)schedule an idle timer for checking the buffer.
Do it only if `flymake-no-changes-timeout' is non-nil."
  (when flymake-timer (cancel-timer flymake-timer))
  (when flymake-no-changes-timeout
    (setq
     flymake-timer
     (run-with-idle-timer
      ;; This can use time-convert instead of seconds-to-time,
      ;; once we can assume Emacs 27 or later.
      (seconds-to-time flymake-no-changes-timeout)
      nil
      (lambda (buffer)
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (when (and flymake-mode
                       flymake-no-changes-timeout)
	      (flymake-log
               :debug "starting syntax check after idle for %s seconds"
               flymake-no-changes-timeout)
	      (flymake-start t))
            (setq flymake-timer nil))))
      (current-buffer)))))

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
  "Start syntax check for current buffer if it isn't already running.
START and STOP and LEN are as in `after-change-functions'."
  (let((new-text (buffer-substring start stop)))
    (push (list start stop new-text) flymake--recent-changes)
    (flymake--schedule-timer-maybe)))

(defun flymake-after-save-hook ()
  (when flymake-start-on-save-buffer
    (flymake-log :debug "starting syntax check as buffer was saved")
    (flymake-start t)))

(defun flymake-kill-buffer-hook ()
  ;; Explicitly set flymake off, because that does a lot of useful
  ;; cleanup.
  (flymake-mode -1))

(defun flymake-find-file-hook ()
  (unless (or flymake-mode
              (null flymake-diagnostic-functions))
    (flymake-mode)
    (flymake-log :warning "Turned on in `flymake-find-file-hook'")))

(defun flymake-eldoc-function (report-doc &rest _)
  "Document diagnostics at point.
Intended for `eldoc-documentation-functions' (which see)."
  (let ((diags (flymake-diagnostics (point))))
    (when diags
      (funcall report-doc
               (mapconcat #'flymake-diagnostic-text diags "\n")))))

(defun flymake-goto-next-error (&optional n filter interactive)
  "Go to Nth next Flymake diagnostic that matches FILTER.
Interactively, always move to the next diagnostic.  With a prefix
arg, skip any diagnostics with a severity less than `:warning'.

If `flymake-wrap-around' is non-nil and no more next diagnostics,
resumes search from top.

FILTER is a list of diagnostic types.  Only diagnostics with
matching severities matching are considered.  If nil (the
default) no filter is applied."
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
                                                'flymake-diagnostic)))
                                     (and diag
                                          (or
                                           (not filter)
                                           (cl-find
                                            (flymake--severity
                                             (flymake-diagnostic-type diag))
                                            filter :key #'flymake--severity)))))
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
              "%s"
              (funcall (overlay-get target 'help-echo)
                       (selected-window) target (point)))))
          (interactive
           (user-error "No more Flymake diagnostics%s"
                       (if filter
                           (format " of %s severity"
                                   (mapconcat #'symbol-name filter ", ")) ""))))))

(defun flymake-goto-prev-error (&optional n filter interactive)
  "Go to Nth previous Flymake diagnostic that matches FILTER.
Interactively, always move to the previous diagnostic.  With a
prefix arg, skip any diagnostics with a severity less than
`:warning'.

If `flymake-wrap-around' is non-nil and no more previous
diagnostics, resumes search from bottom.

FILTER is a list of diagnostic types.  Only diagnostics with
matching severities matching are considered.  If nil (the
default) no filter is applied."
  (interactive (list 1 (if current-prefix-arg
                           '(:error :warning))
                     t))
  (flymake-goto-next-error (- (or n 1)) filter interactive))


;;; Mode-line and menu
;;;
(easy-menu-define flymake-menu flymake-mode-map "Flymake menu."
  '("Flymake"
    [ "Go to next problem"      flymake-goto-next-error t ]
    [ "Go to previous problem"  flymake-goto-prev-error t ]
    [ "Check now"               flymake-start t ]
    [ "List all problems"       flymake-show-buffer-diagnostics t ]
    "--"
    [ "Go to log buffer"        flymake-switch-to-log-buffer t ]
    [ "Turn off Flymake"        flymake-mode t ]))

(defcustom flymake-mode-line-format
  '(" " flymake-mode-line-title flymake-mode-line-exception
    flymake-mode-line-counters)
  "Mode line construct for customizing Flymake information."
  :type '(repeat (choice string symbol)))

(defcustom flymake-mode-line-counter-format
  '("["
    flymake-mode-line-error-counter
    flymake-mode-line-warning-counter
    flymake-mode-line-note-counter "]")
  "Mode-line construct for formatting Flymake diagnostic counters.
This is a suitable place for placing the `flymake-error-counter',
`flymake-warning-counter' and `flymake-note-counter' constructs.
Separating each of these with space is not necessary."
  :type '(repeat (choice string symbol)))

(defvar flymake-mode-line-title '(:eval (flymake--mode-line-title))
  "Mode-line construct to show Flymake's mode name and menu.")

(defvar flymake-mode-line-exception '(:eval (flymake--mode-line-exception))
  "Mode-line construct to report on exceptional Flymake status.")

(defvar flymake-mode-line-counters '(:eval (flymake--mode-line-counters))
  "Mode-line construct for counting Flymake diagnostics.
The counters are only placed if some Flymake backend initialized
correctly.")

(defvar flymake-mode-line-error-counter
  `(:eval (flymake--mode-line-counter :error t)))
(defvar flymake-mode-line-warning-counter
  `(:eval (flymake--mode-line-counter :warning)))
(defvar flymake-mode-line-note-counter
  `(:eval (flymake--mode-line-counter :note)))

(put 'flymake-mode-line-format 'risky-local-variable t)
(put 'flymake-mode-line-title 'risky-local-variable t)
(put 'flymake-mode-line-exception 'risky-local-variable t)
(put 'flymake-mode-line-counters 'risky-local-variable t)
(put 'flymake-mode-line-error-counter 'risky-local-variable t)
(put 'flymake-mode-line-warning-counter 'risky-local-variable t)
(put 'flymake-mode-line-note-counter 'risky-local-variable t)

(defun flymake--mode-line-title ()
  `(:propertize
    "Flymake"
    mouse-face mode-line-highlight
    help-echo
    ,(lambda (&rest _)
       (concat
        (format "%s known backends\n" (hash-table-count flymake--state))
        (format "%s running\n" (length (flymake-running-backends)))
        (format "%s disabled\n" (length (flymake-disabled-backends)))
        "mouse-1: Display minor mode menu\n"
        "mouse-2: Show help for minor mode"))
    keymap
    ,(let ((map (make-sparse-keymap)))
       (define-key map [mode-line down-mouse-1]
         flymake-menu)
       (define-key map [mode-line down-mouse-3]
         flymake-menu)
       (define-key map [mode-line mouse-2]
         (lambda ()
           (interactive)
           (describe-function 'flymake-mode)))
       map)))

(defun flymake--mode-line-exception ()
  "Helper for `flymake-mode-line-exception'."
  (pcase-let* ((running) (reported)
               (`(,ind ,face ,explain)
                (cond ((zerop (hash-table-count flymake--state))
                       '("?" nil "No known backends"))
                      ((cl-set-difference
                        (setq running (flymake-running-backends))
                        (setq reported (flymake-reporting-backends)))
                       `("Wait" compilation-mode-line-run
                         ,(format "Waiting for %s running backend(s)"
                                  (length (cl-set-difference running reported)))))
                      ((and (flymake-disabled-backends) (null running))
                       '("!" compilation-mode-line-run
                         "All backends disabled"))
                      (t
                       '(nil nil nil)))))
    (when ind
      `(":"
        (:propertize ,ind face ,face
                     help-echo ,explain
                     keymap ,(let ((map (make-sparse-keymap)))
                               (define-key map [mode-line mouse-1]
                                 'flymake-switch-to-log-buffer)
                               map))))))

(defun flymake--mode-line-counters ()
  (when (flymake-running-backends) flymake-mode-line-counter-format))

(defun flymake--mode-line-counter (type &optional no-space)
  "Compute number of diagnostics in buffer with TYPE's severity.
TYPE is usually keyword `:error', `:warning' or `:note'."
  (let ((count 0)
        (face (flymake--lookup-type-property type
                                             'mode-line-face
                                             'compilation-error)))
    (dolist (d (flymake-diagnostics))
      (when (= (flymake--severity type)
               (flymake--severity (flymake-diagnostic-type d)))
        (cl-incf count)))
    (when (or (cl-plusp count)
              (cond ((eq flymake-suppress-zero-counters t)
                     nil)
                    (flymake-suppress-zero-counters
                     (>= (flymake--severity type)
                         (warning-numeric-level
                          flymake-suppress-zero-counters)))
                    (t t)))
      `(,(if no-space "" '(:propertize " "))
        (:propertize
         ,(format "%d" count)
         face ,face
         mouse-face mode-line-highlight
         keymap
         ,(let ((map (make-sparse-keymap)))
            (define-key map (vector 'mode-line
                                    mouse-wheel-down-event)
              (lambda (event)
                (interactive "e")
                (with-selected-window (posn-window (event-start event))
                  (flymake-goto-prev-error 1 (list type) t))))
            (define-key map (vector 'mode-line
                                    mouse-wheel-up-event)
              (lambda (event)
                (interactive "e")
                (with-selected-window (posn-window (event-start event))
                  (flymake-goto-next-error 1 (list type) t))))
            map))))))

;;; Per-buffer diagnostic listing

(defvar-local flymake--diagnostics-buffer-source nil)

(defvar flymake-diagnostics-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'flymake-goto-diagnostic)
    (define-key map (kbd "SPC") 'flymake-show-diagnostic)
    map))

(defun flymake-show-diagnostic (pos &optional other-window)
  "Show location of diagnostic at POS."
  (interactive (list (point) t))
  (let* ((id (or (tabulated-list-get-id pos)
                 (user-error "Nothing at point")))
         (diag (plist-get id :diagnostic))
         (locus (flymake--diag-locus diag))
         (beg (flymake--diag-beg diag))
         (end (flymake--diag-end diag))
         (visit (lambda (b e)
                  (goto-char b)
                  (pulse-momentary-highlight-region (point)
                                                    (or e (line-end-position))
                                                    'highlight))))
    (with-current-buffer (cond ((bufferp locus) locus)
                               (t (find-file-noselect locus)))
      (with-selected-window
          (display-buffer (current-buffer) other-window)
        (cond (;; an annotated diagnostic (most common case), or a
               ;; non-annotated buffer diag
               (number-or-marker-p beg)
               (funcall visit beg end))
              (;; a non-annotated file diag (TODO: could use `end'
               ;; here, too)
               (pcase-let ((`(,bbeg . ,bend)
                            (flymake-diag-region (current-buffer)
                                                 (car beg)
                                                 (cdr beg))))
                 (funcall visit bbeg bend)))))
      (current-buffer))))

(defun flymake-goto-diagnostic (pos)
  "Show location of diagnostic at POS.
POS can be a buffer position or a button"
  (interactive "d")
  (pop-to-buffer
   (flymake-show-diagnostic (if (button-type pos) (button-start pos) pos))))

(defun flymake--tabulated-entries-1 (diags project-root)
  "Helper for `flymake--diagnostic-buffer-entries'.
PROJECT-ROOT indicates that each entry should be preceded by the
filename of the diagnostic relative to that directory."
  (cl-loop
   for diag in diags
   for locus = (flymake-diagnostic-buffer diag)
   for file = (if (bufferp locus)
                  (buffer-file-name locus)
                locus)
   for overlay = (flymake--diag-overlay diag)
   for (line . col) =
   (cond (;; has live overlay, use overlay for position
          (and overlay (overlay-buffer overlay))
          (with-current-buffer (overlay-buffer overlay)
            (save-excursion
              (goto-char (overlay-start overlay))
              (cons (line-number-at-pos)
                    (- (point)
                       (line-beginning-position))))))
         (;; diagnostic not annotated, maybe foreign, check for cons
          (consp (flymake--diag-beg diag))
          (flymake--diag-beg diag))
         (;; may still be a valid foreign diagnostic
          (consp (flymake--diag-orig-beg diag))
          (flymake--diag-orig-beg diag))
         (;; somehow dead annotated diagnostic, ignore/give up
          t nil))
   for type = (flymake-diagnostic-type diag)
   for backend = (flymake-diagnostic-backend diag)
   for bname = (or (ignore-errors (symbol-name backend))
                   "(anonymous function)")
   for data-vec = `[,(format "%s" line)
                    ,(format "%s" col)
                    ,(propertize (format "%s"
                                         (flymake--lookup-type-property
                                          type 'flymake-type-name type))
                                 'face (flymake--lookup-type-property
                                        type 'mode-line-face 'flymake-error))
                    ,(propertize
                      (if bname
                          (replace-regexp-in-string "\\(.\\)[^-]+\\(-\\|$\\)"
                                                    "\\1\\2" bname)
                        "(anon)")
                      'help-echo (format "From `%s' backend" backend))
                    (,(replace-regexp-in-string "\n.*" ""
                                                (flymake-diagnostic-text diag))
                     mouse-face highlight
                     help-echo "mouse-2: visit this diagnostic"
                     face nil
                     action flymake-goto-diagnostic
                     mouse-action flymake-goto-diagnostic)]
   when (and line col) collect
   (list (list :diagnostic diag
               :line line
               :severity (flymake--lookup-type-property
                          type
                          'severity (warning-numeric-level :error)))
         (if project-root
             (vconcat `[(,(file-name-nondirectory file)
                         help-echo ,(file-relative-name file project-root)
                         face nil
                         mouse-face highlight
                         action flymake-goto-diagnostic
                         mouse-action flymake-goto-diagnostic )]
                      data-vec)
           data-vec))))

(defun flymake--diagnostics-buffer-entries ()
  "Get tabulated list entries for current tabulated list buffer.
Expects `flymake--diagnostics-buffer-entries' to be bound to a
buffer."
  ;; Do nothing if 'flymake--diagnostics-buffer-source' has not yet
  ;; been set to a valid buffer.  This could happen when this function
  ;; is called too early.  For example 'global-display-line-numbers-mode'
  ;; calls us from its mode hook, when the diagnostic buffer has just
  ;; been created by 'flymake-show-buffer-diagnostics', but is not yet
  ;; set up properly (Bug#40529).
  (when (bufferp flymake--diagnostics-buffer-source)
    (with-current-buffer flymake--diagnostics-buffer-source
      (when flymake-mode
        (flymake--tabulated-entries-1 (flymake-diagnostics) nil)))))

(defvar flymake--diagnostics-base-tabulated-list-format
  `[("Line" 5 ,(lambda (l1 l2)
                 (< (plist-get (car l1) :line)
                    (plist-get (car l2) :line)))
     :right-align t)
    ("Col" 3 nil :right-align t)
    ("Type" 8 ,(lambda (l1 l2)
                 (< (plist-get (car l1) :severity)
                    (plist-get (car l2) :severity))))
    ("Backend" 8 t)
    ("Message" 0 t)])

(define-derived-mode flymake-diagnostics-buffer-mode tabulated-list-mode
  "Flymake diagnostics"
  "A mode for listing Flymake diagnostics."
  (setq tabulated-list-format flymake--diagnostics-base-tabulated-list-format)
  (setq tabulated-list-entries
        'flymake--diagnostics-buffer-entries)
  (tabulated-list-init-header))

(defun flymake--diagnostics-buffer-name ()
  (format "*Flymake diagnostics for `%s'*" (current-buffer)))

(define-obsolete-function-alias 'flymake-show-diagnostics-buffer
  'flymake-show-buffer-diagnostics "1.2.1")

(defun flymake-show-buffer-diagnostics ()
  "Show a list of Flymake diagnostics for current buffer."
  (interactive)
  (let* ((name (flymake--diagnostics-buffer-name))
         (source (current-buffer))
         (target (or (get-buffer name)
                     (with-current-buffer (get-buffer-create name)
                       (flymake-diagnostics-buffer-mode)
                       (current-buffer)))))
    (with-current-buffer target
      (setq flymake--diagnostics-buffer-source source)
      (display-buffer (current-buffer))
      (revert-buffer))))


;;; Per-project diagnostic listing
;;;

(defvar flymake-list-only-diagnostics nil
  "Diagnostics list meant for listing, not highlighting.
This variable holds an alist ((FILE-NAME . DIAGS) ...) where
FILE-NAME is a string holding an absolute file name and DIAGS is
a list of diagnostic objects created with
`flymake-make-diagnostic'.  These diagnostics are never annotated
as overlays in actual buffers: they merely serve as temporary
stand-ins for more accurate diagnostics that are produced once
the file they refer to is visited and `flymake-mode' is turned on
in the resulting buffer.

Flymake backends that somehow gain sporadic information about
diagnostics in neighbouring files may freely modify this variable
by adding or removing entries to for those files.  If the
information about those neighbouring files is acquired repeatedly
and reliably, it may be more sensible to report them as
\"foreign\" diagnostics instead.

Commands such as `flymake-show-project-diagnostics' will include
some of this variable's contents the diagnostic listings.")

(defvar-local flymake--project-diagnostic-list-project nil)

(define-derived-mode flymake-project-diagnostics-mode tabulated-list-mode
  "Flymake diagnostics"
  "A mode for listing Flymake diagnostics."
  (setq tabulated-list-format
        (vconcat [("File" 25 t)]
                 flymake--diagnostics-base-tabulated-list-format))
  (setq tabulated-list-entries
        'flymake--project-diagnostics-entries)
  (tabulated-list-init-header))

(cl-defun flymake--project-diagnostics (&optional (project (project-current)))
  "Get all known relevant diagnostics for PROJECT."
  (let* ((root (project-root project))
         (visited-buffers (cl-remove-if-not #'buffer-file-name (project-buffers project)))
         buffer-annotated-diags
         relevant-foreign-diags
         list-only-diags
         annotated-diag-files)
    (setq buffer-annotated-diags
          (cl-loop for buf in visited-buffers
                   for diags = (with-current-buffer buf
                                 (flymake-diagnostics))
                   when diags do
                   (push (buffer-file-name buf) annotated-diag-files)
                   append (cl-sort diags #'< :key #'flymake-diagnostic-beg)))
    (cl-loop
     for buf in visited-buffers
     do (with-current-buffer buf
          (when (and flymake-mode flymake--state)
            (maphash
             (lambda (_backend state)
               (maphash
                (lambda (foreign-file diags)
                  (setq foreign-file (expand-file-name foreign-file))
                  ;; FIXME: This is not right if more than one visited
                  ;; source targets the same foreign file.  Don't
                  ;; think we can get away without some kind of
                  ;; `cl-remove-duplicates' here that utilizes
                  ;; `flymake--equal-diagnostic-p'.
                  (unless (member foreign-file annotated-diag-files)
                    (push foreign-file annotated-diag-files)
                    (setq relevant-foreign-diags
                          (append relevant-foreign-diags
                                  diags))))
                (flymake--state-foreign-diags state)))
             flymake--state))))
    (setq list-only-diags
          (cl-loop for (file-name . diags) in flymake-list-only-diagnostics
                   if (and (string-prefix-p (expand-file-name root) file-name)
                           (not (member file-name annotated-diag-files)))
                   append diags))
    (append buffer-annotated-diags relevant-foreign-diags list-only-diags)))

(defun flymake--project-diagnostics-entries ()
  (let ((p (project-current)))
    (flymake--tabulated-entries-1 (flymake--project-diagnostics p)
                                  (project-root p))))

(defun flymake--project-diagnostics-buffer (root)
  (get-buffer-create (format "*Flymake diagnostics for `%s'*" root)))

(defun flymake-show-project-diagnostics ()
  "Show a list of Flymake diagnostics for the current project."
  (interactive)
  (let* ((prj (project-current))
         (root (project-root prj))
         (buffer (flymake--project-diagnostics-buffer root)))
    (with-current-buffer buffer
      (flymake-project-diagnostics-mode)
      (setq-local flymake--project-diagnostic-list-project prj)
      (display-buffer (current-buffer))
      (revert-buffer))))

(defun flymake--update-diagnostics-listings (buffer)
  "Update diagnostics listings somehow relevant to BUFFER."
  (dolist (probe (buffer-list))
    (with-current-buffer probe
      (when (or (and (eq major-mode 'flymake-project-diagnostics-mode)
                     flymake--project-diagnostic-list-project
                     (buffer-file-name buffer)
                     (memq buffer
                      (project-buffers flymake--project-diagnostic-list-project)))
                (and (eq major-mode 'flymake-diagnostics-buffer-mode)
                     (eq flymake--diagnostics-buffer-source buffer)))
        (revert-buffer)))))

(provide 'flymake)

(require 'flymake-proc)

;;; flymake.el ends here
