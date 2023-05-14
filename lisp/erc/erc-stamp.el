;;; erc-stamp.el --- Timestamping for ERC messages  -*- lexical-binding:t -*-

;; Copyright (C) 2002-2004, 2006-2023 Free Software Foundation, Inc.

;; Author: Mario Lang <mlang@delysid.org>
;; Maintainer: Amin Bandali <bandali@gnu.org>, F. Jason Park <jp@neverwas.me>
;; Keywords: comm, timestamp
;; URL: https://www.emacswiki.org/emacs/ErcStamp

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

;; The code contained in this module is responsible for inserting
;; timestamps into ERC buffers.  In order to actually activate this,
;; you must call `erc-timestamp-mode'.

;; You can choose between two different ways of inserting timestamps.
;; Customize `erc-insert-timestamp-function' and
;; `erc-insert-away-timestamp-function'.

;;; Code:

(require 'erc)

(defgroup erc-stamp nil
  "For long conversation on IRC it is sometimes quite
useful to have individual messages timestamp.  This
group provides settings related to the format and display
of timestamp information in `erc-mode' buffer.

For timestamping to be activated, you just need to load `erc-stamp'
in your init file or interactively using `load-library'."
  :group 'erc)

(defcustom erc-timestamp-format "[%H:%M]"
  "If set to a string, messages will be timestamped.
This string is processed using `format-time-string'.
Good examples are \"%T\" and \"%H:%M\".

If nil, timestamping is turned off."
  :type '(choice (const nil)
		 (string)))

;; FIXME remove surrounding whitespace from default value and have
;; `erc-insert-timestamp-left-and-right' add it before insertion.

(defcustom erc-timestamp-format-left "\n[%a %b %e %Y]\n"
  "If set to a string, messages will be timestamped.
This string is processed using `format-time-string'.
Good examples are \"%T\" and \"%H:%M\".

This timestamp is used for timestamps on the left side of the
screen when `erc-insert-timestamp-function' is set to
`erc-insert-timestamp-left-and-right'.

If nil, timestamping is turned off."
  :type '(choice (const nil)
		 (string)))

(defcustom erc-timestamp-format-right nil
  "If set to a string, messages will be timestamped.
This string is processed using `format-time-string'.
Good examples are \"%T\" and \"%H:%M\".

This timestamp is used for timestamps on the right side of the
screen when `erc-insert-timestamp-function' is set to
`erc-insert-timestamp-left-and-right'.

Unlike `erc-timestamp-format' and `erc-timestamp-format-left', if
the value of this option is nil, it falls back to using the value
of `erc-timestamp-format'."
  :package-version '(ERC . "5.6") ; FIXME sync on release
  :type '(choice (const nil)
		 (string)))
(make-obsolete-variable 'erc-timestamp-format-right
                        'erc-timestamp-format "30.1")

(defcustom erc-insert-timestamp-function 'erc-insert-timestamp-left-and-right
  "Function to use to insert timestamps.

It takes a single argument STRING which is the final string
which all text-properties already appended.  This function only cares about
inserting this string at the right position.  Narrowing is in effect
while it is called, so (point-min) and (point-max) determine the region to
operate on.

You will probably want to set
`erc-insert-away-timestamp-function' to the same value."
  :type '(choice (const :tag "Both sides" erc-insert-timestamp-left-and-right)
		 (const :tag "Right" erc-insert-timestamp-right)
		 (const :tag "Left" erc-insert-timestamp-left)
		 function))

(defcustom erc-away-timestamp-format "<%H:%M>"
  "Timestamp format used when marked as being away.

If nil, timestamping is turned off when away unless `erc-timestamp-format'
is set.

If `erc-timestamp-format' is set, this will not be used."
  :type '(choice (const nil)
		 (string)))

(defcustom erc-insert-away-timestamp-function
  #'erc-insert-timestamp-left-and-right
  "Function to use to insert the away timestamp.

See `erc-insert-timestamp-function' for details."
  :type '(choice (const :tag "Both sides" erc-insert-timestamp-left-and-right)
		 (const :tag "Right" erc-insert-timestamp-right)
		 (const :tag "Left" erc-insert-timestamp-left)
		 function))

(defcustom erc-hide-timestamps nil
  "If non-nil, timestamps will be invisible.

This is useful for logging, because, although timestamps will be
hidden, they will still be present in the logs."
  :type 'boolean)

(defcustom erc-echo-timestamps nil
  "If non-nil, print timestamp in the minibuffer when point is moved.
Using this variable, you can turn off normal timestamping,
and simply move point to an irc message to see its timestamp
printed in the minibuffer."
  :type 'boolean)

(defcustom erc-echo-timestamp-format "Timestamped %A, %H:%M:%S"
  "Format string to be used when `erc-echo-timestamps' is non-nil.
This string specifies the format of the timestamp being echoed in
the minibuffer."
  :type 'string)

(defcustom erc-timestamp-intangible nil
  "Whether the timestamps should be intangible, i.e. prevent the point
from entering them and instead jump over them."
  :version "24.5"
  :type 'boolean)

(defface erc-timestamp-face '((t :weight bold :foreground "green"))
  "ERC timestamp face."
  :group 'erc-faces)

;; New libraries should only autoload the minor mode for a module's
;; preferred name (rather than its alias).

;;;###autoload(put 'timestamp 'erc--module 'stamp)
;;;###autoload(autoload 'erc-timestamp-mode "erc-stamp" nil t)
(define-erc-module stamp timestamp
  "This mode timestamps messages in the channel buffers."
  ((add-hook 'erc-mode-hook #'erc-munge-invisibility-spec)
   (add-hook 'erc-insert-modify-hook #'erc-add-timestamp t)
   (add-hook 'erc-send-modify-hook #'erc-add-timestamp t)
   (add-hook 'erc-mode-hook #'erc-stamp--recover-on-reconnect)
   (unless erc--updating-modules-p
     (erc-buffer-filter #'erc-munge-invisibility-spec)))
  ((remove-hook 'erc-mode-hook #'erc-munge-invisibility-spec)
   (remove-hook 'erc-insert-modify-hook #'erc-add-timestamp)
   (remove-hook 'erc-send-modify-hook #'erc-add-timestamp)
   (remove-hook 'erc-mode-hook #'erc-stamp--recover-on-reconnect)
   (erc-with-all-buffers-of-server nil nil
     (kill-local-variable 'erc-timestamp-last-inserted)
     (kill-local-variable 'erc-timestamp-last-inserted-left)
     (kill-local-variable 'erc-timestamp-last-inserted-right))))

(defun erc-stamp--recover-on-reconnect ()
  (when-let ((priors (or erc--server-reconnecting erc--target-priors)))
    (dolist (var '(erc-timestamp-last-inserted
                   erc-timestamp-last-inserted-left
                   erc-timestamp-last-inserted-right))
      (when-let (existing (alist-get var priors))
        (set var existing)))))

(defvar erc-stamp--current-time nil
  "The current time when calling `erc-insert-timestamp-function'.
Specifically, this is the same lisp time object used to create
the stamp passed to `erc-insert-timestamp-function'.")

(cl-defgeneric erc-stamp--current-time ()
  "Return a lisp time object to associate with an IRC message.
This becomes the message's `erc-timestamp' text property, which
may not be unique, `equal'-wise."
  (erc-current-time))

(cl-defmethod erc-stamp--current-time :around ()
  (or erc-stamp--current-time (cl-call-next-method)))

(defun erc-add-timestamp ()
  "Add timestamp and text-properties to message.

This function is meant to be called from `erc-insert-modify-hook'
or `erc-send-modify-hook'."
  (progn ; remove this `progn' on next major refactor
    (let* ((ct (erc-stamp--current-time))
           (invisible (get-text-property (point-min) 'invisible))
           (erc-stamp--current-time ct))
      (unless invisible
        (funcall erc-insert-timestamp-function
                 (erc-format-timestamp ct erc-timestamp-format)))
      ;; FIXME this will error when advice has been applied.
      (when (and (not invisible) (fboundp erc-insert-away-timestamp-function)
		 erc-away-timestamp-format
		 (erc-away-time)
		 (not erc-timestamp-format))
	(funcall erc-insert-away-timestamp-function
		 (erc-format-timestamp ct erc-away-timestamp-format)))
      (add-text-properties (point-min) (1- (point-max))
			   ;; It's important for the function to
			   ;; be different on different entries (bug#22700).
			   (list 'cursor-sensor-functions
                                 ;; Regions are no longer contiguous ^
                                 '(erc--echo-ts-csf) 'erc-timestamp ct)))))

(defvar-local erc-timestamp-last-window-width nil
  "The width of the last window that showed the current buffer.
his is used by `erc-insert-timestamp-right' when the current
buffer is not shown in any window.")

(defvar-local erc-timestamp-last-inserted nil
  "Last timestamp inserted into the buffer.")

(defvar-local erc-timestamp-last-inserted-left nil
  "Last timestamp inserted into the left side of the buffer.
This is used when `erc-insert-timestamp-function' is set to
`erc-timestamp-left-and-right'")

(defvar-local erc-timestamp-last-inserted-right nil
  "Last timestamp inserted into the right side of the buffer.
This is used when `erc-insert-timestamp-function' is set to
`erc-timestamp-left-and-right'")

(defcustom erc-timestamp-only-if-changed-flag t
  "Non-nil means insert timestamp only if its value changed since last insertion.
If `erc-insert-timestamp-function' is `erc-insert-timestamp-left', a
string of spaces which is the same size as the timestamp is added to
the beginning of the line in its place.  If you use
`erc-insert-timestamp-right', nothing gets inserted in place of the
timestamp."
  :type 'boolean)

(defcustom erc-timestamp-right-column nil
  "If non-nil, the column at which the timestamp is inserted,
if the timestamp is to be printed to the right.  If nil,
`erc-insert-timestamp-right' will use other means to determine
the correct column."
  :type '(choice
	  (integer :tag "Column number")
	  (const :tag "Unspecified" nil)))

(defcustom erc-timestamp-use-align-to (and (display-graphic-p) t)
  "If non-nil, use the :align-to display property to align the stamp.
This gives better results when variable-width characters (like
Asian language characters and math symbols) precede a timestamp.

This option only matters when `erc-insert-timestamp-function' is
set to `erc-insert-timestamp-right' or that option's default,
`erc-insert-timestamp-left-and-right'.  If the value is a
positive integer, alignment occurs that many columns from the
right edge.  If the value is `margin', the stamp appears in the
right margin when visible.

Enabling this option produces a side effect in that stamps aren't
indented in saved logs.  When its value is an integer, this
option adds a space after the end of a message if the stamp
doesn't already start with one.  And when its value is t, it adds
a single space, unconditionally.  And while this option never
adds a space when its value is `margin', ERC does offer a
workaround in `erc-stamp-prefix-log-filter', which strips
trailing stamps from messages and puts them before every line."
  :type '(choice boolean integer (const margin))
  :package-version '(ERC . "5.6")) ; FIXME sync on release

(defcustom erc-stamp-right-margin-width nil
  "Width in columns of the right margin.
When this option is nil, pretend its value is one column greater
than the `string-width' of the formatted `erc-timestamp-format'.
This option only matters when `erc-timestamp-use-align-to' is set
to `margin'."
  :package-version '(ERC . "5.6") ; FIXME sync on release
  :type '(choice (const nil) integer))

(defun erc-stamp--display-margin-force (orig &rest r)
  (let ((erc-timestamp-use-align-to 'margin))
    (apply orig r)))

(defun erc-stamp--adjust-right-margin (cols)
  "Adjust right margin by COLS.
When COLS is zero, reset width to `erc-stamp-right-margin-width'
or one col more than the `string-width' of
`erc-timestamp-format'."
  (let ((width
         (if (zerop cols)
             (or erc-stamp-right-margin-width
                 (1+ (string-width (or erc-timestamp-last-inserted-right
                                       (erc-format-timestamp
                                        (current-time)
                                        erc-timestamp-format)))))
           (+ right-margin-width cols))))
    (setq right-margin-width width)
    (when (eq (current-buffer) (window-buffer))
      (set-window-margins nil left-margin-width width))))

;;;###autoload
(defun erc-stamp-prefix-log-filter (text)
  "Prefix every message in the buffer with a stamp.
Remove trailing stamps as well.  For now, hard code the format to
\"ZNC\"-log style, which is [HH:MM:SS].  Expect to be used as a
`erc-log-filter-function' when `erc-timestamp-use-align-to' is
non-nil."
  (insert text)
  (goto-char (point-min))
  (while
      (progn
        (when-let* (((< (point) (pos-eol)))
                    (end (1- (pos-eol)))
                    ((eq 'erc-timestamp (field-at-pos end)))
                    (beg (field-beginning end))
                    ;; Skip a line that's just a timestamp.
                    ((> beg (point))))
          (delete-region beg (1+ end)))
        (when-let (time (get-text-property (point) 'erc-timestamp))
          (insert (format-time-string "[%H:%M:%S] " time)))
        (zerop (forward-line))))
  "")

(declare-function erc--remove-text-properties "erc" (string))

;; If people want to use this directly, we can convert it into
;; a local module.
(define-minor-mode erc-stamp--display-margin-mode
  "Internal minor mode for built-in modules integrating with `stamp'.
It binds `erc-timestamp-use-align-to' to `margin' around calls to
`erc-insert-timestamp-function' in the current buffer, and sets
the right window margin to `erc-stamp-right-margin-width'.  It
also arranges to remove most text properties when a user kills
message text so that stamps will be visible when yanked."
  :interactive nil
  (if erc-stamp--display-margin-mode
      (progn
        (setq fringes-outside-margins t)
        (when (eq (current-buffer) (window-buffer))
          (set-window-buffer (selected-window) (current-buffer)))
        (erc-stamp--adjust-right-margin 0)
        (add-function :filter-return (local 'filter-buffer-substring-function)
                      #'erc--remove-text-properties)
        (add-function :around (local 'erc-insert-timestamp-function)
                      #'erc-stamp--display-margin-force))
    (remove-function (local 'filter-buffer-substring-function)
                     #'erc--remove-text-properties)
    (remove-function (local 'erc-insert-timestamp-function)
                     #'erc-stamp--display-margin-force)
    (kill-local-variable 'right-margin-width)
    (kill-local-variable 'fringes-outside-margins)
    (when (eq (current-buffer) (window-buffer))
      (set-window-margins nil left-margin-width nil)
      (set-window-buffer (selected-window) (current-buffer)))))

(defun erc-insert-timestamp-left (string)
  "Insert timestamps at the beginning of the line."
  (goto-char (point-min))
  (let* ((ignore-p (and erc-timestamp-only-if-changed-flag
			(string-equal string erc-timestamp-last-inserted)))
	 (len (length string))
	 (s (if ignore-p (make-string len ? ) string)))
    (unless ignore-p (setq erc-timestamp-last-inserted string))
    (erc-put-text-property 0 len 'field 'erc-timestamp s)
    (erc-put-text-property 0 len 'invisible 'timestamp s)
    (insert s)))

(defun erc-insert-aligned (string pos)
  "Insert STRING at the POSth column.

If `erc-timestamp-use-align-to' is t, use the :align-to display
property to get to the POSth column."
  (declare (obsolete "inlined and removed from client code path" "30.1"))
  (if (not erc-timestamp-use-align-to)
      (indent-to pos)
    (insert " ")
    (put-text-property (1- (point)) (point) 'display
		       (list 'space ':align-to pos)))
  (insert string))

;; Silence byte-compiler
(defvar erc-fill-column)

(defvar erc-stamp--inherited-props '(line-prefix wrap-prefix))

(defun erc-insert-timestamp-right (string)
  "Insert timestamp on the right side of the screen.
STRING is the timestamp to insert.  This function is a possible
value for `erc-insert-timestamp-function'.

If `erc-timestamp-only-if-changed-flag' is nil, a timestamp is
always printed.  If this variable is non-nil, a timestamp is only
printed if it is different from the last.

If `erc-timestamp-right-column' is set, its value will be used as
the column at which the timestamp is to be printed.  If it is
nil, and `erc-fill-mode' is active, then the timestamp will be
printed just before `erc-fill-column'.  Otherwise, if the current
buffer is shown in a window, that window's width is used as the
right boundary.  In case multiple windows show the buffer, the
width of the most recently selected one is used.  If the buffer
is not shown, the timestamp will be printed just before the
window width of the last window that showed it.  If the buffer
was never shown, and `fill-column' is set, it will be printed
just before `fill-column'.  As a last resort, timestamp will be
printed just after each line's text (no alignment)."
  (unless (and erc-timestamp-only-if-changed-flag
	       (string-equal string erc-timestamp-last-inserted))
    (setq erc-timestamp-last-inserted string)
    (goto-char (point-max))
    (forward-char -1)                   ; before the last newline
    (let* ((str-width (string-width string))
           window                  ; used in computation of `pos' only
	   (pos (cond
		 (erc-timestamp-right-column erc-timestamp-right-column)
		 ((and (boundp 'erc-fill-mode)
		       erc-fill-mode
		       (boundp 'erc-fill-column)
		       erc-fill-column)
		  (1+ (- erc-fill-column str-width)))
                 ((setq window (get-buffer-window nil t))
                  (setq erc-timestamp-last-window-width
                        (window-width window))
                  (- erc-timestamp-last-window-width str-width))
                 (erc-timestamp-last-window-width
                  (- erc-timestamp-last-window-width str-width))
		 (fill-column
		  (1+ (- fill-column str-width)))
                 (t (current-column))))
	   (from (point))
	   (col (current-column)))
      ;; The following is a kludge used to calculate whether to move
      ;; to the next line before inserting a stamp.  It allows for
      ;; some margin of error if what is displayed on the line differs
      ;; from the number of characters on the line.
      (setq col (+ col (ceiling (/ (- col (- (point) (line-beginning-position))) 1.6))))
      ;; For compatibility reasons, the `erc-timestamp' field includes
      ;; intervening white space unless a hard break is warranted.
      (pcase erc-timestamp-use-align-to
        ((and 't (guard (< col pos)))
         (insert " ")
         (put-text-property from (point) 'display `(space :align-to ,pos)))
        ((pred integerp) ; (cl-type (integer 0 *))
         (insert " ")
         (when (eq ?\s (aref string 0))
           (setq string (substring string 1)))
         (let ((s (+ erc-timestamp-use-align-to (string-width string))))
           (put-text-property from (point) 'display
                              `(space :align-to (- right ,s)))))
        ('margin
         (put-text-property 0 (length string)
                            'display `((margin right-margin) ,string)
                            string))
        ((guard (>= col pos)) (newline) (indent-to pos) (setq from (point)))
        (_ (indent-to pos)))
      (insert string)
      (dolist (p erc-stamp--inherited-props)
        (when-let ((v (get-text-property (1- from) p)))
          (put-text-property from (point) p v)))
      (erc-put-text-property from (point) 'field 'erc-timestamp)
      (erc-put-text-property from (point) 'rear-nonsticky t)
      (when erc-timestamp-intangible
	(erc-put-text-property from (1+ (point)) 'cursor-intangible t)))))

(defvar erc-stamp--insert-date-function #'insert
  "Function to insert left \"left-right date\" stamp.
A local module might use this to modify text properties,
`insert-before-markers' or renarrow the region after insertion.")

(defun erc-insert-timestamp-left-and-right (string)
  "Insert a stamp on either side when it changes.
When the deprecated option `erc-timestamp-format-right' is nil,
use STRING, which originates from `erc-timestamp-format', for the
right-hand stamp.  Use `erc-timestamp-format-left' for the
left-hand stamp and expect it to change less frequently."
  (let* ((ct (or erc-stamp--current-time (erc-stamp--current-time)))
         (ts-left (erc-format-timestamp ct erc-timestamp-format-left))
         (ts-right (with-suppressed-warnings
                       ((obsolete erc-timestamp-format-right))
                     (if erc-timestamp-format-right
                         (erc-format-timestamp ct erc-timestamp-format-right)
                       string))))
    ;; insert left timestamp
    (unless (string-equal ts-left erc-timestamp-last-inserted-left)
      (goto-char (point-min))
      (erc-put-text-property 0 (length ts-left) 'field 'erc-timestamp ts-left)
      (funcall erc-stamp--insert-date-function ts-left)
      (setq erc-timestamp-last-inserted-left ts-left))
    ;; insert right timestamp
    (let ((erc-timestamp-only-if-changed-flag t)
	  (erc-timestamp-last-inserted erc-timestamp-last-inserted-right))
      (erc-insert-timestamp-right ts-right)
      (setq erc-timestamp-last-inserted-right ts-right))))

;; for testing: (setq erc-timestamp-only-if-changed-flag nil)
(defvar erc-stamp--tz nil)

(defun erc-format-timestamp (time format)
  "Return TIME formatted as string according to FORMAT.
Return the empty string if FORMAT is nil."
  (if format
      (let ((ts (format-time-string format time erc-stamp--tz)))
	(erc-put-text-property 0 (length ts)
			       'font-lock-face 'erc-timestamp-face ts)
	(erc-put-text-property 0 (length ts) 'invisible 'timestamp ts)
	(erc-put-text-property 0 (length ts)
			       'isearch-open-invisible 'timestamp ts)
	;; N.B. Later use categories instead of this harmless, but
	;; inelegant, hack. -- BPT
	(and erc-timestamp-intangible
	     (not erc-hide-timestamps)	; bug#11706
	     (erc-put-text-property 0 (length ts) 'cursor-intangible t ts))
	ts)
    ""))

;; This function is used to munge `buffer-invisibility-spec' to an
;; appropriate value. Currently, it only handles timestamps, thus its
;; location.  If you add other features which affect invisibility,
;; please modify this function and move it to a more appropriate
;; location.
(defun erc-munge-invisibility-spec ()
  (and erc-timestamp-intangible (not (bound-and-true-p cursor-intangible-mode))
       (cursor-intangible-mode 1))
  (and erc-echo-timestamps (not (bound-and-true-p cursor-sensor-mode))
       (cursor-sensor-mode 1))
  (if erc-hide-timestamps
      (add-to-invisibility-spec 'timestamp)
    (remove-from-invisibility-spec 'timestamp)))

(defun erc-hide-timestamps ()
  "Hide timestamp information from display."
  (interactive)
  (setq erc-hide-timestamps t)
  (erc-munge-invisibility-spec))

(defun erc-show-timestamps ()
  "Show timestamp information on display.
This function only works if `erc-timestamp-format' was previously
set, and timestamping is already active."
  (interactive)
  (setq erc-hide-timestamps nil)
  (erc-munge-invisibility-spec))

(defun erc-toggle-timestamps ()
  "Hide or show timestamps in ERC buffers.

Note that timestamps can only be shown for a message using this
function if `erc-timestamp-format' was set and timestamping was
enabled when the message was inserted."
  (interactive)
  (if erc-hide-timestamps
      (setq erc-hide-timestamps nil)
    (setq erc-hide-timestamps t))
  (mapc (lambda (buffer)
	  (with-current-buffer buffer
	    (erc-munge-invisibility-spec)))
	(erc-buffer-list)))

(defun erc-echo-timestamp (dir stamp)
  "Print timestamp text-property of an IRC message."
  ;; Could also pass an &optional `zone' arg to `format-time-string'.
  (interactive (list 'entered (get-text-property (point) 'erc-timestamp)))
  (when (eq 'entered dir)
    (when stamp
      (message "%s" (format-time-string erc-echo-timestamp-format
					stamp)))))

(defun erc--echo-ts-csf (_window _before dir)
  (erc-echo-timestamp dir (get-text-property (point) 'erc-timestamp)))

(provide 'erc-stamp)

;;; erc-stamp.el ends here
;;
;; Local Variables:
;; generated-autoload-file: "erc-loaddefs.el"
;; End:
