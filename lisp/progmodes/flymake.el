;;; flymake.el --- A universal on-the-fly syntax checker  -*- lexical-binding: t; -*-

;; Copyright (C) 2003-2026 Free Software Foundation, Inc.

;; Author: Pavel Kobyakov <pk_at_work@yahoo.com>
;; Maintainer: Spencer Baugh <sbaugh@janestreet.com>
;; Version: 1.4.3
;; Keywords: c languages tools
;; Package-Requires: ((emacs "26.1") (eldoc "1.14.0") (project "0.11.1"))

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
;; docstring of `flymake-diagnostic-functions', the Flymake manual, and the
;; code of existing backends are probably good starting points.
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
;; We need the next `require' to avoid compiler warnings and run-time
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

(add-to-list 'customize-package-emacs-version-alist
             '(Flymake ("1.3.4" . "30.1")
                       ("1.3.5" . "30.1")
                       ("1.3.6" . "30.1")))

(defcustom flymake-error-bitmap '(flymake-double-exclamation-mark
                                  flymake-error-fringe)
  "Bitmap (a symbol) used in the fringe for indicating errors.
The value may also be a list of two elements where the second
element specifies the face for the bitmap.  For possible bitmap
symbols, see `fringe-bitmaps'.  See also `flymake-warning-bitmap'.

The option `flymake-fringe-indicator-position' controls how and where
this is used."
  :version "31.1"
  :type '(choice (symbol :tag "Bitmap")
                 (list :tag "Bitmap and face"
                       (symbol :tag "Bitmap")
                       (face :tag "Face"))))

(defcustom flymake-warning-bitmap '(exclamation-mark flymake-warning-fringe)
  "Bitmap (a symbol) used in the fringe for indicating warnings.
The value may also be a list of two elements where the second
element specifies the face for the bitmap.  For possible bitmap
symbols, see `fringe-bitmaps'.  See also `flymake-error-bitmap'.

The option `flymake-fringe-indicator-position' controls how and where
this is used."
  :version "31.1"
  :type '(choice (symbol :tag "Bitmap")
                 (list :tag "Bitmap and face"
                       (symbol :tag "Bitmap")
                       (face :tag "Face"))))

(defcustom flymake-note-bitmap '(exclamation-mark flymake-note-fringe)
  "Bitmap (a symbol) used in the fringe for indicating info notes.
The value may also be a list of two elements where the second
element specifies the face for the bitmap.  For possible bitmap
symbols, see `fringe-bitmaps'.  See also `flymake-error-bitmap'.

The option `flymake-fringe-indicator-position' controls how and where
this is used."
  :version "31.1"
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

(defcustom flymake-indicator-type 'auto
  "Indicate which indicator type to use for display errors.

The value can be nil (don't indicate errors but just highlight them),
the symbol `fringes' (use fringes), the symbol `margins' (use margins),
or the symbol `auto' to automatically guess.

Difference between fringes and margin is that fringes support displaying
bitmaps on graphical displays and margins display text in a blank area
from current buffer that works in both graphical and text displays.
When margins are selected, Flymake may need to resize them for each
buffer.  See `flymake-autoresize-margins'.

See Info node `Fringes' and Info node `(elisp)Display Margins'."
  :version "31.1"
  :type '(choice (const :tag "Use Fringes" fringes)
                 (const :tag "Use Margins" margins)
                 (const :tag "Use fringes if possible, otherwise margins" auto)
                 (const :tag "No indicators" nil)))

(defcustom flymake-margin-indicators-string
  '((error "‼" compilation-error)
    (warning "!" compilation-warning)
    (note "!" compilation-info))
  "Strings used for margins indicators.
The value of each list may be a list of 3 elements where specifies the
error type, the string to use and its face,
or a list of 2 elements specifying only the error type and
the corresponding string.

The option `flymake-margin-indicator-position' controls how and where
this is used.

Note that the default value \"DOUBLE EXCLAMATION MARK\" for the
indicator of the \\+`error' type will be silently replaced by the
ASCII equivalent if that character is not displayable by the terminal."
  :version "30.1"
  :type '(repeat :tag "Error types lists"
                 (list :tag "String and face for error types"
                       (symbol :tag "Error type")
                       (string :tag "String")
                       (face :tag "Face"))))

(defcustom flymake-autoresize-margins t
  "If non-nil, automatically resize margin-width.

Only relevant if `flymake-indicator-type' is set to `margins' or `auto'."
  :version "30.1"
  :type 'boolean)

(defcustom flymake-margin-indicator-position 'left-margin
  "The position to put Flymake margin indicator.
The value can be nil (do not use indicators), `left-margin' or `right-margin'.
See `flymake-margin-indicators-string'."
  :version "30.1"
  :type '(choice (const left-margin)
                 (const right-margin)
                 (const :tag "No margin indicators" nil)))

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
    (flymake-log :error "%s" msg)
    (error (concat "[Flymake] " msg))))

(cl-defstruct (flymake--diag
               (:constructor flymake--diag-make))
  locus beg end type origin code message backend data overlay-properties overlay
  ;; FIXME: See usage of these two in `flymake--highlight-line'.
  ;; Ideally they wouldn't be needed.
  orig-beg orig-end)

;;;###autoload
(defun flymake-make-diagnostic (locus
                                beg
                                end
                                type
                                info
                                &optional data
                                overlay-properties)
  "Make a Flymake diagnostic for LOCUS's region from BEG to END.
LOCUS is a buffer object or a string designating a file name.

TYPE is a diagnostic symbol (see Info Node `(Flymake)Flymake error
types')

INFO is a description of the problem detected.  It may be a string, or
list (ORIGIN CODE MESSAGE) appropriately categorizing and describing the
diagnostic.  ORIGIN may be a string or nil.  CODE maybe be a string, a
number or nil.  MESSAGE must be a string.

DATA is any object that the caller wishes to attach to the created
diagnostic for later retrieval with `flymake-diagnostic-data'.

If LOCUS is a buffer, BEG and END should be buffer positions inside it.
If LOCUS designates a file, BEG and END should be a cons (LINE . COL)
indicating a file position.  In this second case, END may be omitted in
which case the region is computed using `flymake-diag-region' if the
diagnostic is appended to an actual buffer.

OVERLAY-PROPERTIES is an alist of properties attached to the created
diagnostic, overriding the default properties and any properties listed
in the `flymake-overlay-control' property of the diagnostic's type
symbol."
  (when (stringp locus)
    (setq locus (expand-file-name locus)))
  (cond ((stringp info)
         (setq info (list nil nil info)))
        ((numberp (cadr info))
         (setf (cadr info) (number-to-string (cadr info)))))
  (flymake--diag-make :locus locus :beg beg :end end
                      :type type :origin (car info) :code (cadr info)
                      :message (caddr info) :data data
                      :overlay-properties overlay-properties
                      :orig-beg beg
                      :orig-end end))

;;;###autoload
(defun flymake-diagnostics (&optional beg end)
  "Get Flymake diagnostics in region determined by BEG and END.

If neither BEG or END is supplied, use whole accessible buffer,
otherwise if BEG is non-nil and END is nil, consider only
diagnostics at BEG."
  (save-restriction
    (widen)
    (cl-loop for o in
             (cond (end (overlays-in beg end))
                   (beg (overlays-at beg))
                   (t (overlays-in (point-min) (point-max))))
             when (overlay-get o 'flymake-diagnostic) collect it)))

(defmacro flymake--diag-accessor (public internal thing)
  "Make PUBLIC an alias for INTERNAL, add doc using THING."
  `(defsubst ,public (diag)
     ,(format "Get Flymake diagnostic DIAG's %s." (symbol-name thing))
     (,internal diag)))

(flymake--diag-accessor flymake-diagnostic-type flymake--diag-type type)
(flymake--diag-accessor flymake-diagnostic-backend flymake--diag-backend backend)
(flymake--diag-accessor flymake-diagnostic-origin flymake--diag-origin backend)
(flymake--diag-accessor flymake-diagnostic-code flymake--diag-code backend)
(flymake--diag-accessor flymake-diagnostic-message flymake--diag-message backend)
(flymake--diag-accessor flymake-diagnostic-data flymake--diag-data data)
(flymake--diag-accessor flymake-diagnostic-beg flymake--diag-beg beg)
(flymake--diag-accessor flymake-diagnostic-end flymake--diag-end end)
(flymake--diag-accessor flymake-diagnostic-buffer flymake--diag-locus locus)

(defcustom flymake-diagnostic-format-alist
  '((:help-echo . (origin code oneliner))
    (:eol . (oneliner))
    (:eldoc . (origin code message))
    (:eldoc-echo . (origin code oneliner))
    (t . (origin code oneliner)))
  "How to format diagnostics for different output destinations.
Value is an alist where each element looks like (DESTINATION . PARTS).
DESTINATION is a symbol designating an outlet.  One of:

- `:help-echo', for the native Flymake echoing of diagnostics in the
   echo area as used my `flymake-goto-next-error' and `flymake-goto-prev-error';
- `:eol', for use with `flymake-show-diagnostics-at-end-of-line';
- `:eldoc', for use with Flymake's ElDoc backend;
- `:eldoc-echo', for use with Flymake's ElDoc backend, but for ElDoc's own
   confined outlets;
- t for all other destinations.

PARTS says which parts of the diagnostic to include.  It is a list of
symbols where the following values are meaningful:

- `origin': include diagnostic origin if it exists;
- `code': include diagnostics code if it exists;
- `message': include the full diagnostic's message text;
- `oneliner': include truncated diagnostic text;"
  :package-version '(Flymake . "1.4.0")
  :type '(alist :key-type (choice (const :help-echo)
                                  (const :eol)
                                  (const :eldoc)
                                  (const :eldoc-echo)
                                  (const t))
                :value-type (set (const origin)
                                 (const code)
                                 (const message)
                                 (const oneliner))))

(cl-defun flymake-diagnostic-text (diag
                                   &optional (parts '(origin code message)))
  "Describe diagnostic DIAG's as a string.
PARTS says which parts of the diagnostic to include.  It is a list of
symbols as described in `flymake-diagnostic-format-alist' (which see).
PARTS defaults to `(origin code message)'."
  (let* ((w parts)
         (a (and (memq 'origin w) (flymake--diag-origin diag)))
         (b (and (memq 'code w) (flymake--diag-code diag)))
         (c (cond ((memq 'message w) (flymake--diag-message diag))
                  ((memq 'oneliner w)
                   (let* ((msg (flymake--diag-message diag)))
                     (substring msg 0 (cl-loop for i from 0 for a across msg
                                               when (eq a ?\n) return i)))))))
    (concat a
            (when (and a b) " ")
            (when b (concat "[" b "]"))
            (when (and c (or a b)) ": ")
            c)))

(defun flymake--format-diagnostic (diag destination face-prop)
  (let ((txt (flymake-diagnostic-text
              diag (alist-get destination flymake-diagnostic-format-alist
                              (alist-get t flymake-diagnostic-format-alist
                                         '(origin code message))))))
    (if face-prop
        (propertize txt 'face
                    (flymake--lookup-type-property
                     (flymake-diagnostic-type diag) face-prop
                     'flymake-error))
      txt)))

(defun flymake-diagnostic-oneliner (diag &optional nopaintp)
  "Get truncated one-line text string for diagnostic DIAG.
This is useful for displaying the DIAG's text to the user in
confined spaces, such as the echo are.  Unless NOPAINTP is t,
propertize returned text with the `echo-face' property of DIAG's
type."
  (let* ((txt (flymake-diagnostic-text diag '(origin code oneliner))))
    (if nopaintp txt
      (propertize txt 'face
                  (flymake--lookup-type-property
                   (flymake-diagnostic-type diag) 'echo-face 'flymake-error)))))
(make-obsolete 'flymake-diagnostic-oneliner
               "use `flymake-diagnostic-text' instead."
               "Flymake package version 1.4.0")

(cl-defun flymake--really-all-overlays ()
  "Get flymake-related overlays.
If BEG is non-nil and END is nil, consider only `overlays-at'
BEG.  Otherwise consider `overlays-in' the region comprised by BEG
and END, defaulting to the whole buffer.  Remove all that do not
verify FILTER, a function, and sort them by COMPARE (using KEY)."
  (save-restriction
    (widen)
    (cl-remove-if-not (lambda (o) (overlay-get o 'flymake-overlay))
                      (overlays-in (point-min) (point-max)))))

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

(defface flymake-error-echo
  '((t :inherit compilation-error))
  "Face used for showing summarized descriptions of errors."
  :package-version '(Flymake . "1.3.4"))

(defface flymake-warning-echo
  '((t :inherit compilation-warning))
  "Face used for showing summarized descriptions of warnings."
  :package-version '(Flymake . "1.3.4"))

(defface flymake-note-echo
  '((t :inherit compilation-info))
  "Face used for showing summarized descriptions of notes."
  :package-version '(Flymake . "1.3.4"))

(defface flymake-end-of-line-diagnostics-face
  '((t :height 0.85))
  "Face used for end-of-line diagnostics.
See variable `flymake-show-diagnostics-at-end-of-line'."
  :package-version '(Flymake . "1.3.5"))

(defface flymake-error-echo-at-eol
  '((t :inherit (flymake-end-of-line-diagnostics-face compilation-error)))
  "Face like `flymake-error-echo', but for end-of-line overlays."
  :package-version '(Flymake . "1.3.5"))

(defface flymake-warning-echo-at-eol
  '((t :inherit (flymake-end-of-line-diagnostics-face compilation-warning)))
  "Face like `flymake-warning-echo', but for end-of-line overlays."
  :package-version '(Flymake . "1.3.5"))

(defface flymake-note-echo-at-eol
  '((t :inherit (flymake-end-of-line-diagnostics-face compilation-info)))
  "Face like `flymake-note-echo', but for end-of-line overlays."
  :package-version '(Flymake . "1.3.5"))

(defface flymake-eol-information-face
  '((t :inherit (flymake-end-of-line-diagnostics-face)
       :box nil
       :slant italic))
  "Face used for information about end-of-line diagnostics."
  :package-version '(Flymake . "1.3.6"))

(defface flymake-error-fringe '((t :inherit compilation-error))
  "Face used by default in the `flymake-error-bitmap'."
  :version "31.1"
  :package-version '(Flymake . "1.4.4"))

(defface flymake-warning-fringe '((t :inherit compilation-warning))
  "Face used by default in the `flymake-warning-bitmap'."
  :version "31.1"
  :package-version '(Flymake . "1.4.4"))

(defface flymake-note-fringe '((t :inherit compilation-info))
  "Face used by default in the `flymake-note-bitmap'."
  :version "31.1"
  :package-version '(Flymake . "1.4.4"))

(defcustom flymake-show-diagnostics-at-end-of-line nil
  "If non-nil, add diagnostic summary messages at end-of-line.
The value `short' means that only the most severe diagnostic
shall be shown.
The value `fancy' means to layout diagnostic summary information
below the affected line with Unicode graphics.
Any other non-nil value means show all diagnostic summaries at
end-of-line."
  :type '(choice (const :tag "Display most severe diagnostic" short)
                 (const :tag "Display all diagnostics" t)
                 (const :tag "Display all diagnostics using Unicode" fancy)
                 (const :tag "Don't display diagnostics at end-of-line" nil))
  :package-version '(Flymake . "1.3.6"))

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

The functions in this hook diagnose problems in a buffer's contents and
provide information to the Flymake user interface about where and how to
annotate problems diagnosed in a buffer.

Each backend function must be prepared to accept an arbitrary number of
arguments:

* the first argument is always REPORT-FN, a callback function
  detailed below;

* the remaining arguments are keyword-value pairs in the
  form (:KEY VALUE :KEY2 VALUE2...).

Currently, Flymake may provide these keyword-value pairs:

* `:recent-changes', a list of recent changes since the last time the
  backend function was called for the buffer.  An empty list indicates
  that no changes have been recorded.  If it is the first time that this
  backend function is called for this activation of `flymake-mode', then
  this argument isn't provided at all (i.e. it's not merely nil).

  Each element is in the form (BEG END TEXT) where BEG and END are
  buffer positions, and TEXT is a string containing the text contained
  between those positions (if any) after the change was performed.

* `:changes-start' and `:changes-end', the minimum and maximum buffer
  positions touched by the recent changes.  These are only provided if
  `:recent-changes' is also provided.

Whenever Flymake or the user decides to re-check the buffer, backend
functions are called as detailed above and are expected to initiate this
check, but aren't required to complete it before exiting: if the
computation involved is expensive, especially for large buffers, that
task can be scheduled for the future using asynchronous processes or
other asynchronous mechanisms.

In any case, backend functions are expected to return quickly or signal
an error, in which case the backend is disabled.  Flymake will not try
disabled backends again for any future checks of this buffer.  To reset
the list of disabled backends, turn `flymake-mode' off and on again, or
interactively call `flymake-start' with a prefix argument.

If the function returns, Flymake considers the backend to be
\"running\".  If it has not done so already, the backend is expected to
call the function REPORT-FN with a single argument REPORT-ACTION also
followed by an optional list of keyword-value pairs in the
form (:REPORT-KEY VALUE :REPORT-KEY2 VALUE2...).

Currently accepted values for REPORT-ACTION are:

* A (possibly empty) list of diagnostic objects created with
  `flymake-make-diagnostic', causing Flymake to delete all previous
  diagnostic annotations in the buffer and create new ones from this
  list.

  A backend may call REPORT-FN repeatedly in this manner, but only until
  Flymake considers that the most recently requested buffer check is now
  obsolete because, say, buffer contents have changed in the meantime.
  The backend is only given notice of this via a renewed call to the
  backend function.  Thus, to prevent making obsolete reports and
  wasting resources, backend functions should first cancel any ongoing
  processing from previous calls.

* The symbol `:panic', signaling that the backend has encountered an
  exceptional situation and should be disabled.

Currently accepted REPORT-KEY arguments are:

* `:explanation' value should give user-readable details of the
  situation encountered, if any.

* `:force': value should be a boolean suggesting that Flymake consider
  the report even if it was somehow unexpected.

* `:region': a cons (BEG . END) of buffer positions specifying that
  Flymake should only delete diagnostic annotations of past reports if
  they intersect the region by at least one character.  The list of
  diagnostics objects in the report need not be contained in the region.
  This makes it allows backends to choose between accumulating or
  completely replacing diagnostics across different invocations of
  REPORT-FN, by specifying a either 0-length region or the full buffer.")

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
(put 'flymake-error 'flymake-margin-string (alist-get 'error flymake-margin-indicators-string))
(put 'flymake-error 'severity (warning-numeric-level :error))
(put 'flymake-error 'mode-line-face 'flymake-error-echo)
(put 'flymake-error 'echo-face 'flymake-error-echo)
(put 'flymake-error 'eol-face 'flymake-error-echo-at-eol)
(put 'flymake-error 'flymake-type-name "error")

(put 'flymake-warning 'face 'flymake-warning)
(put 'flymake-warning 'flymake-bitmap 'flymake-warning-bitmap)
(put 'flymake-warning 'flymake-margin-string (alist-get 'warning flymake-margin-indicators-string))
(put 'flymake-warning 'severity (warning-numeric-level :warning))
(put 'flymake-warning 'mode-line-face 'flymake-warning-echo)
(put 'flymake-warning 'echo-face 'flymake-warning-echo)
(put 'flymake-warning 'eol-face 'flymake-warning-echo-at-eol)
(put 'flymake-warning 'flymake-type-name "warning")

(put 'flymake-note 'face 'flymake-note)
(put 'flymake-note 'flymake-bitmap 'flymake-note-bitmap)
(put 'flymake-note 'flymake-margin-string (alist-get 'note flymake-margin-indicators-string))
(put 'flymake-note 'severity (warning-numeric-level :debug))
(put 'flymake-note 'mode-line-face 'flymake-note-echo)
(put 'flymake-note 'echo-face 'flymake-note-echo)
(put 'flymake-note 'eol-face 'flymake-note-echo-at-eol)
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

(defun flymake--equal-diagnostic-p (a b)
  "Tell if A and B are equivalent `flymake--diag' objects."
  (or (eq a b)
      (cl-loop for comp in '(flymake--diag-end
                             flymake--diag-beg
                             flymake-diagnostic-type
                             flymake-diagnostic-backend
                             flymake-diagnostic-origin
                             flymake-diagnostic-code
                             flymake-diagnostic-message)
               always (equal (funcall comp a) (funcall comp b)))))

(defun flymake--delete-overlay (ov)
  "Like `delete-overlay', delete OV, but do some more stuff."
  (let ((eolov (overlay-get ov 'flymake--eol-ov)))
    (when eolov
      (let ((src-ovs (delq ov (overlay-get eolov 'flymake-eol-source-overlays))))
        (overlay-put eolov 'flymake-eol-source-overlays src-ovs)))
    (delete-overlay ov)))

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
    ;; Try to remedy the situation if the same diagnostic is already
    ;; registered in the same place.  This happens for clashes between
    ;; domestic and foreign diagnostics
    (cl-loop for e in (flymake-diagnostics beg end)
             for eov = (flymake--diag-overlay e)
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
                  (flymake--delete-overlay eov)))
    (setq ov (make-overlay beg end))
    (setf (flymake--diag-overlay diagnostic) ov)
    (when (= (overlay-start ov) (overlay-end ov))
      ;; Some backends report diagnostics with invalid bounds.  Don't
      ;; bother.
      (delete-overlay ov)
      (cl-return-from flymake--highlight-line nil))
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
      (default-maybe
       'before-string
       (propertize
        "!" 'display
        (if (eq flymake-indicator-type 'auto)
            `((when (flymake--suitably-fringed-p) .
                    ,(flymake--bs-display type 'fringes))
              (when (not (flymake--suitably-fringed-p)) .
                    ,(flymake--bs-display type 'margins)))
          (flymake--bs-display type flymake-indicator-type))))
      (default-maybe 'help-echo
        (lambda (window _ov pos)
          (with-selected-window window
            (mapconcat
             (lambda (d)
               (flymake--format-diagnostic d :help-echo 'echo-face))
             (flymake-diagnostics pos)
             "\n"))))
      (default-maybe 'severity (warning-numeric-level :error))
      ;; Use (PRIMARY . SECONDARY) priority, to avoid clashing with
      ;; `region' face, for example (bug#34022).
      (default-maybe 'priority (cons nil (+ 40 (overlay-get ov 'severity)))))
    ;; Some properties can't be overridden.
    ;;
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'flymake-overlay t)
    (overlay-put ov 'modification-hooks
                 `(,(lambda (ov after &rest _)
                      (when-let* ((eolov
                                   (and (null after) (overlay-get ov 'flymake--eol-ov))))
                        (delete-overlay eolov)))))
    (overlay-put ov 'flymake-diagnostic diagnostic)
    ;; Handle `flymake-show-diagnostics-at-end-of-line'
    ;;
    (when flymake-show-diagnostics-at-end-of-line
      (save-excursion
        (goto-char (overlay-start ov))
        (let* ((start (line-end-position))
               (end (min (1+ start) (point-max)))
               (eolov (car
                       (cl-remove-if-not
                        (lambda (o) (overlay-get o 'flymake-eol-source-overlays))
                        (overlays-in start end)))))
          ;; FIXME: 1. no checking if there are unexpectedly more than
          ;; one eolov at point.
          (if eolov
              (push ov (overlay-get eolov 'flymake-eol-source-overlays))
            (setq eolov (make-overlay start end nil t nil))
            (overlay-put eolov 'flymake-overlay t)
            (overlay-put eolov 'flymake--eol-overlay t)
            (overlay-put eolov 'flymake-eol-source-overlays (list ov))
            (overlay-put eolov 'evaporate (not (= start end)))) ; FIXME: fishy
          (overlay-put ov 'flymake--eol-ov eolov))))
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
    ;; All of the above might have touched the eol overlays, so issue
    ;; a call to update them.  But check running and reporting
    ;; backends first to flickering when multiple backends touch the
    ;; same eol overlays.
    (when (and flymake-show-diagnostics-at-end-of-line
               (not (cl-set-difference (flymake-running-backends)
                                       (flymake-reporting-backends))))
      (flymake--update-eol-overlays))
    (flymake--update-diagnostics-listings (current-buffer))))

(defun flymake--clear-foreign-diags (state)
  (maphash (lambda (_buffer diags)
             (cl-loop for d in diags
                      when (flymake--diag-overlay d)
                      do (flymake--delete-overlay it)))
           (flymake--state-foreign-diags state))
  (clrhash (flymake--state-foreign-diags state)))

(defun flymake--clear-state (state)
  (cl-loop for diag in (flymake--state-diags state)
           for ov = (flymake--diag-overlay diag)
           when ov do (flymake--delete-overlay ov))
  (setf (flymake--state-diags state) nil)
  (flymake--clear-foreign-diags state))

(defvar-local flymake-mode nil)

(defvar-local flymake--mode-line-counter-cache nil
  "A cache used in `flymake-mode-line-counters'.")

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
      ;; overlays are in a certain region.  Ignore "foreign"
      ;; diagnostics.
      region
      (cl-loop for diag in (flymake--state-diags state)
               for ov = (flymake--diag-overlay diag)
               if (or (not (overlay-buffer ov))
                      (flymake--intersects-p
                       (overlay-start ov) (overlay-end ov)
                       (car region) (cdr region)))
               do (flymake--delete-overlay ov)
               else collect diag into surviving
               finally (setf (flymake--state-diags state)
                             surviving)))
     (;; Else, if this is the first report, fully clear this state.
      (not (flymake--state-reported-p state))
      (flymake--clear-state state))
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
                                (flymake--state-foreign-diags state))))))
    ;; Finally, flush some caches
    (setq flymake--mode-line-counter-cache nil)))

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
  (flymake-log :warning "Disabling backend %s because %S" backend explanation)
  (flymake--with-backend-state backend state
    (setf (flymake--state-running state) nil
          (flymake--state-disabled state) explanation
          (flymake--state-reported-p state) t)))

(defun flymake--run-backend (backend &optional args)
  "Run the backend BACKEND, re-enabling if necessary.
ARGS is a keyword-value plist passed to the backend along
with a report function."
  (flymake-log :debug "Running backend %s" backend)
  (let ((run-token (gensym "backend-token")))
    (flymake--with-backend-state backend state
      (setf (flymake--state-running state) run-token
            (flymake--state-disabled state) nil
            (flymake--state-reported-p state) nil))
    (condition-case-unless-debug err
        (apply backend (flymake-make-report-fn backend run-token)
               args)
      (error
       (flymake--disable-backend backend err)))))

(defvar-local flymake--recent-changes nil
  "Recent changes collected by `flymake-after-change-function'.")
(defvar flymake-mode)

(defun flymake--import-foreign-diagnostics ()
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
    ;; 2. other buffers where a backend has created "foreign
    ;; diagnostics" and pointed them here.  We must highlight them in
    ;; this buffer, i.e. create overlays for them.  Those other
    ;; buffers and backends are still responsible for them, i.e. the
    ;; current buffer does not "own" these foreign diags.
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when flymake-mode
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
        ((visible-buffer-window ()
           (and (or (not (daemonp))
                    (not (eq (selected-frame) terminal-frame)))
                (get-buffer-window (current-buffer))))
         (start-post-command ()
          (remove-hook 'post-command-hook #'start-post-command
                       nil)
          ;; The buffer may have disappeared already, e.g. because of
          ;; code like `(with-temp-buffer (python-mode) ...)'.
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (flymake-start (remove 'post-command deferred) force))))
         (start-on-display ()
          (remove-hook 'window-configuration-change-hook #'start-on-display
                       'local)
          ;; Double check that buffer is actually visible (bug#77313)
          (if (visible-buffer-window)
              (setq deferred (remove 'on-display deferred)))
          (flymake-start deferred force)))
      (cond ((and (memq 'post-command deferred)
                  this-command)
             (add-hook 'post-command-hook
                       #'start-post-command
                       'append nil))
            ((and (memq 'on-display deferred)
                  (not (visible-buffer-window)))
             (add-hook 'window-configuration-change-hook
                       #'start-on-display
                       'append 'local))
            (flymake-mode
             ;; The buffer about to be annotated is visible.  Check
             ;; necessary conditions to auto-set margins here (bug#77313)
             (when-let* ((w (and (eq flymake-indicator-type 'auto)
                                 flymake-autoresize-margins
                                 (visible-buffer-window))))
               (unless (flymake--suitably-fringed-p w)
                 (flymake--resize-margins)))
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
               ;; Delete all overlays that didn't come from one of the
               ;; current diagnostic functions.
               ;; Sometimes diagnostic functions are removed from
               ;; `flymake-diagnostic-functions' (e.g. by eglot).  This
               ;; leaves overlays in the buffer which otherwise won't be
               ;; cleaned up until `flymake-mode' is restarted.
               ;; See bug#78862
               (maphash (lambda (backend state)
                          (unless (memq backend flymake-diagnostic-functions)
                            (flymake--clear-state state)))
                        flymake--state)
               (run-hook-wrapped
                'flymake-diagnostic-functions
                (lambda (backend)
                  (flymake--with-backend-state backend state
                    (setf (flymake--state-reported-p state) nil))))
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
                  nil)))
             (flymake--import-foreign-diagnostics))))))

(defvar flymake-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map
                `[,flymake-fringe-indicator-position mouse-1]
                #'flymake-show-buffer-diagnostics)
    map)
  "Keymap for `flymake-mode'")

(defvar-local flymake-current-diagnostic-line 0
  "The line of the most recently focused diagnostic in a diagnostics buffer.")

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

By default, `flymake-mode' doesn't override the \\[next-error] command, but
if you're using Flymake a lot (and don't use the regular compilation
mechanisms that often), it can be useful to put something like
the following in your init file:

  (setq next-error-function \\='flymake-goto-next-error)

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

    ;; Maybe auto-resize margins
    (when (and (eq flymake-indicator-type 'margins) flymake-autoresize-margins)
      (flymake--resize-margins))

    ;; We can't just `clrhash' `flymake--state': there may be in
    ;; in-transit requests from other backends if `flymake-mode' was
    ;; already active.  I.e. `flymake-mode' function should be as
    ;; idempotent as possible.  See bug#69809.
    (unless flymake--state (setq flymake--state (make-hash-table)))
    (setq flymake--recent-changes nil)
    (when flymake-start-on-flymake-mode (flymake-start t)))

   ;; Turning the mode OFF.
   (t
    (remove-hook 'after-change-functions 'flymake-after-change-function t)
    (remove-hook 'after-save-hook 'flymake-after-save-hook t)
    (remove-hook 'kill-buffer-hook 'flymake-kill-buffer-hook t)
    ;;+(remove-hook 'find-file-hook (function flymake-find-file-hook) t)
    (remove-hook 'eldoc-documentation-functions 'flymake-eldoc-function t)

    ;; return any resized margin to original size
    (flymake--restore-margins)

    (when flymake-timer
      (cancel-timer flymake-timer)
      (setq flymake-timer nil))
    (mapc #'flymake--delete-overlay (flymake--really-all-overlays))
    (when flymake--state
      (maphash (lambda (_backend state)
                 (flymake--clear-foreign-diags state))
               flymake--state))))
   ;; turning Flymake on or off has consequences for listings
   (flymake--update-diagnostics-listings (current-buffer)))

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

(defun flymake-after-change-function (start stop pre-change-len)
  "Start syntax check for current buffer if it isn't already running.
START and STOP and LEN are as in `after-change-functions'."
  (let((new-text (buffer-substring start stop)))
    (push (list start stop new-text) flymake--recent-changes)
    (flymake--schedule-timer-maybe))
  ;; Some special handling to prevent eol overlays from temporarily
  ;; moving to wrong line
  (when (and flymake-show-diagnostics-at-end-of-line
             (zerop pre-change-len))
    (save-excursion
      (goto-char start)
      (when-let* ((probe (search-forward "\n" stop t))
                  (eolovs (cl-remove-if-not
                           (lambda (o)
                             (let ((lbound
                                    (cl-loop for s in (overlay-get o 'flymake-eol-source-overlays)
                                             minimizing (overlay-start s))))
                               (and lbound (< lbound (1- probe)))))
                           (overlays-at (line-end-position)))))
        (goto-char start)
        (let ((newend (line-end-position)))
          (dolist (ov eolovs) (move-overlay ov newend (1+ newend))))))))

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
  (when-let* ((diags (flymake-diagnostics (point))))
    (funcall report-doc
             (mapconcat (lambda (d)
                          (flymake--format-diagnostic d :eldoc 'echo-face))
                        diags "\n")
             :echo (mapconcat
                    (lambda (d)
                      (flymake--format-diagnostic d :eldoc-echo 'echo-face))
                    diags "\n"))))

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
         (ovs (cl-loop
               for o in (overlays-in (point-min) (point-max))
               for diag = (overlay-get o 'flymake-diagnostic)
               when (and diag (or (not filter) (cl-find
                                                (flymake--severity
                                                 (flymake-diagnostic-type diag))
                                                filter :key #'flymake--severity)))
               collect o into retval
               finally (cl-return
                        (cl-sort retval (if (cl-plusp n) #'< #'>)
                                 :key #'overlay-start))))
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
This is a suitable place for placing the `flymake-mode-line-error-counter',
`flymake-mode-line-warning-counter' and `flymake-mode-line-note-counter'
constructs.
Separating each of these with space is not necessary."
  :type '(repeat (choice string symbol)))

(defcustom flymake-mode-line-lighter "Flymake"
  "The string to use in the Flymake mode line."
  :type 'string
  :version "29.1")

(defvar flymake-mode-line-title '(:eval (flymake--mode-line-title))
  "Mode-line construct to show Flymake's mode name and menu.")

(defvar flymake-mode-line-exception '(:eval (flymake--mode-line-exception))
  "Mode-line construct to report on exceptional Flymake status.")

(defvar flymake-mode-line-counters '(:eval (flymake--mode-line-counters))
  "Mode-line construct for counting Flymake diagnostics.
The counters are only placed if some Flymake backend initialized
correctly.")

(defvar flymake-mode-line-error-counter
  `(:eval (flymake--mode-line-counter :error)))
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
    ,flymake-mode-line-lighter
    mouse-face mode-line-highlight
    help-echo
    ,(lambda (w &rest _)
       (with-current-buffer (window-buffer w)
         ;; Mouse can activate tool-tip without window being active.
         ;; `flymake--state' is buffer local and is null when line
         ;; lighter appears in *Help* `describe-mode'.
         (concat
          (unless (null flymake--state)
            (concat
             (format "%s known backends\n"  (hash-table-count flymake--state))
             (format "%s running\n" (length (flymake-running-backends)))
             (format "%s disabled\n" (length (flymake-disabled-backends)))))
          "mouse-1: Display minor mode menu\n"
          "mouse-2: Show help for minor mode")))
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

(defun flymake--mode-line-counter-scroll-prev (event)
  (interactive "e")
  (let* ((event-start (event-start event))
         (posn-string (posn-string event-start))
         (type (get-text-property
                (cdr posn-string) 'flymake--diagnostic-type (car posn-string))))
    (with-selected-window (posn-window event-start)
      (flymake-goto-prev-error 1 (list type) t))))

(defun flymake--mode-line-counter-scroll-next (event)
  (interactive "e")
  (let* ((event-start (event-start event))
         (posn-string (posn-string event-start))
         (type (get-text-property
                (cdr posn-string) 'flymake--diagnostic-type (car posn-string))))
    (with-selected-window (posn-window event-start)
      (flymake-goto-next-error 1 (list type) t))))

(defvar flymake--mode-line-counter-map
  (let ((map (make-sparse-keymap)))
    ;; BEWARE: `mouse-wheel-UP-event' corresponds to `wheel-DOWN' events
    ;; and vice versa!!
    (with-no-warnings
      (define-key map (vector 'mode-line mouse-wheel-down-event)
                  #'flymake--mode-line-counter-scroll-prev)
      (define-key map [mode-line wheel-down]
                  #'flymake--mode-line-counter-scroll-next)
      (define-key map (vector 'mode-line mouse-wheel-up-event)
                  #'flymake--mode-line-counter-scroll-next)
      (define-key map [mode-line wheel-up]
                  #'flymake--mode-line-counter-scroll-prev))
    (define-key map [mode-line mouse-1]
                  #'flymake-show-buffer-diagnostics)
    map))

(defun flymake--mode-line-counter-1 (type)
  "Helper for `flymake--mode-line-counter'."
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
      `(,(if (eq type :error) "" '(:propertize " "))
        (:propertize
         ,(format "%d" count)
         face ,face
         mouse-face mode-line-highlight
         help-echo ,(format "Number of %s; scroll mouse to view."
                            (cond
                             ((eq type :error) "errors")
                             ((eq type :warning) "warnings")
                             ((eq type :note) "notes")
                             (t (format "%s diagnostics" type))))
         flymake--diagnostic-type ,type
         keymap ,flymake--mode-line-counter-map)))))

(defun flymake--mode-line-counter (type)
  "Compute number of diagnostics in buffer with TYPE's severity.
TYPE is usually keyword `:error', `:warning' or `:note'."
  (let ((probe (alist-get type flymake--mode-line-counter-cache 'none)))
    (if (eq probe 'none)
        (setf (alist-get type flymake--mode-line-counter-cache)
            (flymake--mode-line-counter-1 type))
      probe)))


;;; Per-buffer diagnostic listings
;;;
(defvar-local flymake--diagnostics-buffer-source nil)

(defvar flymake-diagnostics-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'flymake-goto-diagnostic)
    (define-key map (kbd "SPC") #'flymake-show-diagnostic)
    (define-key map (kbd "C-o") #'flymake-show-diagnostic)
    (define-key map (kbd "C-m") #'flymake-goto-diagnostic)
    (define-key map (kbd "n") #'next-error-this-buffer-no-select)
    (define-key map (kbd "p") #'previous-error-this-buffer-no-select)
    map))

(defun flymake-show-diagnostic (pos &optional other-window)
  "From Flymake diagnostics buffer, show source of diagnostic at POS."
  (interactive (list (point) t))
  (let* ((diagnostics-buffer (current-buffer))
         (id (or (tabulated-list-get-id pos)
                 (user-error "Nothing at point")))
         (diag (plist-get id :diagnostic))
         (locus (flymake--diag-locus diag))
         (beg (flymake--diag-beg diag))
         (end (flymake--diag-end diag))
         (visit (lambda (b e)
                  (goto-char b)
                  (pulse-momentary-highlight-region
                   b (or e (line-end-position))))))
    (setq flymake-current-diagnostic-line (line-number-at-pos pos))
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
      ;; Emacs < 27
      (setq next-error-last-buffer diagnostics-buffer)
      (current-buffer))))

(defun flymake-goto-diagnostic (pos)
  "From Flymake diagnostics buffer, goto source of diagnostic at POS.
POS can be a buffer position or a button"
  (interactive "d")
  (pop-to-buffer
   (flymake-show-diagnostic (if (button-type pos) (button-start pos) pos))))

(defvar flymake--tabulated-list-format-base
  `[("File" 15)
    ("Line" 4 ,(lambda (l1 l2)
                 (< (plist-get (car l1) :line)
                    (plist-get (car l2) :line)))
     :right-align t)
    ("Col" 3 nil :right-align t)
    ("Type" 4 ,(lambda (l1 l2)
                 (< (plist-get (car l1) :severity)
                    (plist-get (car l2) :severity))))
    ("Origin" 6 t)
    ("Code" 4 t)
    ("Message" 0 t)])

(defun flymake--tabulated-setup-1 (diags project-root)
  "Helper for `flymake--tabulated-setup'.
Sets `tabulated-list-format' and `tabulated-list-entries', dynamically
resizing columns and omitting redundant columns."
  (cl-loop
   with fields = (copy-tree flymake--tabulated-list-format-base t)
   initially (cl-loop for y across fields do (setf (cadr y) nil))
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
   for data-line = `[,(and project-root
                           `(,(file-name-nondirectory file)
                             help-echo ,(file-relative-name file project-root)
                             face nil
                             mouse-face highlight
                             action flymake-goto-diagnostic
                             mouse-action flymake-goto-diagnostic ))
                     ,(format "%s" line)
                     ,(format "%s" col)
                     ,(propertize (format "%s"
                                          (flymake--lookup-type-property
                                           type 'flymake-type-name type))
                                  'face (flymake--lookup-type-property
                                         type 'mode-line-face 'flymake-error))
                     ,(flymake-diagnostic-origin diag)
                     ,(flymake-diagnostic-code diag)
                     (,(flymake-diagnostic-text diag '(oneliner))
                      mouse-face highlight
                      help-echo "mouse-2: visit this diagnostic"
                      face nil
                      action flymake-goto-diagnostic
                      mouse-action flymake-goto-diagnostic)]
   for meta = (and line col
                   (list :diagnostic diag
                         :line line
                         :severity (flymake--lookup-type-property
                                    type
                                    'severity (warning-numeric-level :error))))
   when meta
   do (cl-loop for x across data-line
               for y across fields
               for z across flymake--tabulated-list-format-base
               for xlen = (cond ((stringp x) (length x))
                                (t (length (car x))))
               when (cl-plusp xlen)
               do (setf (cadr y)
                        (max xlen
                             (or (cadr y) (cadr z)))))
   collect (list meta data-line) into data
   finally
   ;; `data' and `fields' now hold more or less suitable values for
   ;; `tabulated-list-entries' and `tabulated-list-format' respectively,
   ;; but we need to trim them, first removing the columns of data where
   ;; the corresponding field is known to be nil for every line, and
   ;; then removing the field description itself.
   (cl-loop
    for entry in data
    do (setf (cadr entry) (cl-loop for x across (cadr entry)
                                   for y across fields
                                   when (cadr y)
                                   vconcat (vector (or x "-")))))
   (setq tabulated-list-entries data
         tabulated-list-format
         (cl-loop for y across fields
                  when (cadr y) vconcat (vector y)))))

(defun flymake--tabulated-setup (use-project)
  "Helper for `flymake-diagnostics-buffer-mode'.
And also `flymake-project-diagnostics-mode'."
  (setq-local next-error-function #'flymake--diagnostics-next-error)
  (let ((saved-r-b-f revert-buffer-function)
        (refresh
         (lambda ()
           (cond
            (use-project
             (let ((p (project-current)))
               (flymake--tabulated-setup-1
                (flymake--project-diagnostics p)
                (project-root p))))
            (t
             ;; Do nothing if 'flymake--diagnostics-buffer-source' has
             ;; not yet been set to a valid buffer.  This could happen
             ;; when this function is called too early.  For example
             ;; 'global-display-line-numbers-mode' calls us from its
             ;; mode hook, when the diagnostic buffer has just been
             ;; created by 'flymake-show-buffer-diagnostics', but is not
             ;; yet set up properly (Bug#40529).
             (flymake--tabulated-setup-1
              (and (bufferp flymake--diagnostics-buffer-source)
                   (with-current-buffer flymake--diagnostics-buffer-source
                     (and flymake-mode
                          (flymake-diagnostics))))
               nil)))
           (tabulated-list-init-header))))
    (setq revert-buffer-function
          (lambda (&rest args)
            (funcall refresh)
            (apply saved-r-b-f args)))))

(defun flymake--diagnostics-next-error (n &optional reset)
  "`next-error-function' for flymake diagnostics buffers.
N is an integer representing how many errors to move.
If RESET is non-nil, return to the beginning of the errors before
moving."
  (let ((line (if reset 1 flymake-current-diagnostic-line))
        (total-lines (count-lines (point-min) (point-max))))
    (goto-char (point-min))
    (unless (zerop total-lines)
      (let ((target-line (+ line n)))
        (setq target-line (max 1 target-line))
        (setq target-line (min target-line total-lines))
        (forward-line (1- target-line))))
    (when-let* ((win (get-buffer-window nil t)))
      (set-window-point win (point)))
    (flymake-goto-diagnostic (point))))

(define-derived-mode flymake-diagnostics-buffer-mode tabulated-list-mode
  "Flymake diagnostics"
  "A mode for listing Flymake diagnostics."
  :interactive nil
  (flymake--tabulated-setup nil))

(defun flymake--diagnostics-buffer-name ()
  (format "*Flymake diagnostics for `%s'*" (current-buffer)))

(define-obsolete-function-alias 'flymake-show-diagnostics-buffer
  'flymake-show-buffer-diagnostics "1.2.1")

(defun flymake--fit-diagnostics-window (window)
  (fit-window-to-buffer window 15 8))

(defun flymake-show-buffer-diagnostics (&optional diagnostic)
  "Show listing of Flymake diagnostics for current buffer.
With optional DIAGNOSTIC, find and highlight this diagnostic in the
listing.

Interactively, grab DIAGNOSTIC from context.  For mouse events in
margins and fringes, use the first diagnostic in the corresponding line,
else look in the click position.  For non-mouse events, look for
diagnostics at point.

This function doesn't move point"
  (interactive
   (if (mouse-event-p last-command-event)
       (with-selected-window (posn-window (event-end last-command-event))
         (with-current-buffer (window-buffer)
           (let* ((event-point (posn-point (event-end last-command-event)))
                  (diags
                   (or
                    (flymake-diagnostics event-point)
                    (let (event-lbp event-lep)
                      (save-excursion
                        (goto-char event-point)
                        (setq event-lbp (line-beginning-position)
                              event-lep (line-end-position)))
                      (flymake-diagnostics event-lbp event-lep))))
                  (diag (car diags)))
             (unless diag
               (error "No diagnostics here"))
             (list diag))))
     (flymake-diagnostics (point))))
  (unless flymake-mode
    (user-error "Flymake mode is not enabled in the current buffer"))
  (let* ((name (flymake--diagnostics-buffer-name))
         (source (current-buffer))
         (target (or (get-buffer name)
                     (with-current-buffer (get-buffer-create name)
                       (flymake-diagnostics-buffer-mode)
                       (current-buffer))))
         window)
    (with-current-buffer target
      (setq flymake--diagnostics-buffer-source source)
      (setq next-error-last-buffer (current-buffer))
      (revert-buffer)
      (setq window
            (display-buffer (current-buffer)
                            `((display-buffer-reuse-window
                               display-buffer-below-selected)
                              (window-height . flymake--fit-diagnostics-window))))
      (when (and window diagnostic)
        (with-selected-window window
          (cl-loop initially (goto-char (point-min))
                   until (eobp)
                   until (eq (plist-get (tabulated-list-get-id) :diagnostic)
                             diagnostic)
                   do (forward-line)
                   finally
                   (recenter)
                   (pulse-momentary-highlight-one-line
                    (point) 'highlight)))))))


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
diagnostics in neighboring files may freely modify this variable
by adding or removing entries to for those files.  If the
information about those neighboring files is acquired repeatedly
and reliably, it may be more sensible to report them as
\"foreign\" diagnostics instead.

Commands such as `flymake-show-project-diagnostics' will include
some of this variable's contents the diagnostic listings.")

(defvar-local flymake--project-diagnostic-list-project nil)

(defvar flymake-project-diagnostics-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map flymake-diagnostics-buffer-mode-map)
    map))

(define-derived-mode flymake-project-diagnostics-mode tabulated-list-mode
  "Flymake diagnostics"
  "A mode for listing Flymake diagnostics in a project."
  :interactive nil
  (flymake--tabulated-setup t))

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
          (when flymake-mode
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
      (setq next-error-last-buffer (current-buffer))
      (revert-buffer)
      (display-buffer (current-buffer)
                      `((display-buffer-reuse-window
                         display-buffer-at-bottom)
                        (window-height . flymake--fit-diagnostics-window))))))

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


;;; Eol overlay helpers
;;;
(defun flymake--update-eol-overlays ()
  "Update the `display' property of end-of-line overlays."
  (save-restriction
    (widen)
    (dolist (o (overlays-in (point-min) (point-max)))
      (when (overlay-get o 'flymake--eol-overlay)
        (if-let* ((src-ovs (overlay-get o 'flymake-eol-source-overlays)))
            (overlay-put o 'display (flymake--eol-overlay-summary src-ovs))
          (delete-overlay o))))))

(defun flymake--eol-overlay-summary (src-ovs)
  "Helper function for `flymake--update-eol-overlays'."
  (cl-flet ((summarize (d)
              (flymake--format-diagnostic d :eol 'eol-face)))
    (let* ((diags
            (cl-sort
             (mapcar (lambda (o) (overlay-get o 'flymake-diagnostic)) src-ovs)
             #'>
             :key (lambda (d) (flymake--severity (flymake-diagnostic-type d)))))
           (summary
            (concat
             "  "
             (cond ((eq flymake-show-diagnostics-at-end-of-line 'short)
                    (concat
                     (summarize (car diags))
                     (and (cdr diags)
                          (concat
                           " "
                           (propertize (format "and %s more"
                                               (1- (length diags)))
                                       'face 'flymake-eol-information-face)))))
                   ((eq flymake-show-diagnostics-at-end-of-line 'fancy)
                    (flymake--eol-draw-fancy diags #'summarize))
                   (t
                    (mapconcat #'summarize diags " ")))
             "\n")))
      (put-text-property 0 1 'cursor t summary)
      summary)))

(defun flymake--eol-draw-fancy-1 (text boxdraw-face line-beg-col
                                       height-to-clear
                                       text-beg-col
                                       text-end-col)
  (cl-flet ((move (cl)
              (let* ((lep (line-end-position))
                     (target (+ (point) cl))
                     (diff (- target lep)))
                (cond ((> diff 0)
                       (goto-char lep)
                       (insert (make-string diff ? )))
                      (t
                       (goto-char target)))))
            (onward ()
              (let ((rem (forward-line 1)))
                (unless (and (not (eobp)) (zerop rem))
                  (goto-char (point-max))
                  (insert "\n")))))
    (goto-char (point-min))
    (cl-loop
     with fork = (propertize "├" 'face boxdraw-face)
     with pipe = (propertize "│" 'face boxdraw-face)
     with inhibit-field-text-motion = t
     for i from 0
     repeat height-to-clear
     do (move line-beg-col)
     (let ((c (char-before)))
       (delete-char -1)
       (insert
        (propertize
         (cond ;; ((zerop i) "┬")
          ((memq c '(?└ ?├)) fork)
          (t         pipe))
         'face boxdraw-face)))
     (onward))
    (move line-beg-col)
    (delete-char -1)
    (insert (propertize "└" 'face boxdraw-face))
    (insert (propertize (make-string (- text-beg-col line-beg-col 1)
                                     ?─)
                        'face boxdraw-face))
    (insert " ")
    (let ((rect (with-temp-buffer
                  (insert text)
                  (let ((fill-column (- text-end-col text-beg-col)))
                    (fill-paragraph)
                    (forward-line 0)
                    (move fill-column)
                    (extract-rectangle (point-min) (point-max))))))
      (insert-rectangle rect)
      (+ height-to-clear (length rect)))))

(defun flymake--eol-draw-fancy (diags summarize-fn)
  (with-temp-buffer
    (cl-loop
     with sorted = (cl-sort diags #'> :key #'flymake-diagnostic-beg)
     for diag in sorted
     for text = (funcall summarize-fn diag)
     for line-beg-col =
     (with-current-buffer (flymake-diagnostic-buffer diag)
       (save-excursion
         (goto-char (flymake-diagnostic-beg diag))
         (1+ (current-column))))
     for height-to-clear = 0 then ret
     for i from 0
     for adjust = (* i 2)
     for face = `(:foreground
                  ,(face-attribute
                    (or (get-text-property 0 'face text)
                        'flymake-error)
                    :foreground nil t))
     for text-beg-col = (max (- (max 30 (+ line-beg-col 5)) adjust) (+ line-beg-col 1))
     for text-end-col = (max 100 (+ text-beg-col 40))
     for ret = (flymake--eol-draw-fancy-1
                text
                face
                line-beg-col
                height-to-clear
                text-beg-col
                text-end-col))
    (concat " \n" (buffer-string))))


;;; Margins and fringes

(defvar-local flymake--original-margin-width nil
  "Store original margin width.
Used by `flymake--resize-margins' for restoring original margin width
when flymake is turned off.")

(defun flymake--suitably-fringed-p (&optional window)
  "Tell if WINDOW is suitably fringed-up fro Flymake."
  (cl-case flymake-fringe-indicator-position
    (left-fringe (< 0 (nth 0 (window-fringes window))))
    (right-fringe (< 0 (nth 1 (window-fringes window))))))

(defun flymake--bs-display (type where)
  "Return a `display' spec for an overlay's `before-string'.
The overlay will represent a diagnostic of type TYPE.  WHERE is the
symbol `fringes' or the symbol `margins'."
  (let* ((indicator (flymake--lookup-type-property
                     type
                     (cl-case where
                       (fringes 'flymake-bitmap)
                       (margins 'flymake-margin-string))
                     (alist-get 'bitmap (alist-get type ; backward compat
                                                   flymake-diagnostic-types-alist))))
         (value (if (symbolp indicator)
                    (symbol-value indicator)
                  indicator))
         (valuelist (if (listp value)
                        value
                      (list value)))
         (indicator-car (car valuelist)))
    (cond ((and (symbolp indicator-car)
                flymake-fringe-indicator-position)
           (cons flymake-fringe-indicator-position valuelist))
          ((and (stringp indicator-car)
                flymake-margin-indicator-position)
           `((margin ,flymake-margin-indicator-position)
             ,(propertize
               indicator-car
               'face `(:inherit (,(cdr valuelist) default))
               'mouse-face 'highlight
               'help-echo "Open Flymake diagnostics"
               'keymap (let ((map (make-sparse-keymap)))
                         (define-key
                          map `[,flymake-margin-indicator-position mouse-1]
                          #'flymake-show-buffer-diagnostics)
                         map)))))))

(defun flymake--appropriate-margin ()
  (if (eq flymake-margin-indicator-position 'left-margin)
      'left-margin-width 'right-margin-width))

(defun flymake--restore-margins ()
  (when flymake--original-margin-width
    (set (flymake--appropriate-margin) flymake--original-margin-width))
  (flymake--apply-margins))

(defun flymake--suitable-margin-width ()
  (let* ((indicators
          (mapcar (lambda (sym)
                    (let ((ind (get sym 'flymake-margin-string)))
                      (when (and (equal (car ind) "‼")
                                 (not (char-displayable-p ?‼)))
                        (setq ind (cons "!!" (cdr ind)))
                        (put sym 'flymake-margin-string ind))
                      (car ind)))
                  '(flymake-error flymake-warning flymake-note))))
    (apply #'max (mapcar #'string-width indicators))))

(defun flymake--resize-margins ()
  (let* ((sym (flymake--appropriate-margin))
         (width (flymake--suitable-margin-width))
         (curr (symbol-value sym)))
    (unless (eq curr width)
      (setq flymake--original-margin-width (symbol-value sym))
      (set sym width)
      (flymake--apply-margins))))

(defun flymake--apply-margins ()
  ;; FIXME: this destroys any scroll an inactive window showing
  (mapc (lambda (w)
          (set-window-buffer w (window-buffer w)))
        (get-buffer-window-list nil nil 'visible)))

(provide 'flymake)

;;; flymake.el ends here
