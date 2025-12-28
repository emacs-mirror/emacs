;;; icalendar-mode.el --- Major mode for iCalendar format  -*- lexical-binding: t; -*-
;;;

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Richard Lawrence <rwl@recursewithless.net>
;; Created: October 2024
;; Keywords: calendar
;; Human-Keywords: calendar, iCalendar

;; This file is part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file defines icalendar-mode, a major mode for iCalendar data.
;; Its main job is to provide syntax highlighting using the matching
;; functions created for iCalendar syntax in icalendar-parser.el, and to
;; perform line unfolding and folding via format conversion.

;; When activated, icalendar-mode unfolds content lines if necessary.
;; This is because the parsing functions, and thus syntax highlighting,
;; assume that content lines have already been unfolded.  When a buffer
;; is saved, icalendar-mode also automatically folds long content if
;; necessary, as required by RFC5545.


;;; Code:
(require 'icalendar-parser)
(require 'format)

;; Faces and font lock:
(defgroup ical:faces
  '((ical:property-name custom-face)
    (ical:property-value custom-face)
    (ical:parameter-name custom-face)
    (ical:parameter-value custom-face)
    (ical:component-name custom-face)
    (ical:keyword custom-face)
    (ical:binary-data custom-face)
    (ical:date-time-types custom-face)
    (ical:numeric-types custom-face)
    (ical:recurrence-rule custom-face)
    (ical:warning custom-face)
    (ical:ignored custom-face))
  "Faces for `icalendar-mode'."
  :version "31.1"
  :group 'icalendar
  :prefix 'icalendar)

(defface ical:property-name
  '((default . (:inherit font-lock-keyword-face)))
  "Face for iCalendar property names.")

(defface ical:property-value
  '((default . (:inherit default)))
  "Face for iCalendar property values.")

(defface ical:parameter-name
  '((default . (:inherit font-lock-property-name-face)))
  "Face for iCalendar parameter names.")

(defface ical:parameter-value
  '((default . (:inherit font-lock-property-use-face)))
  "Face for iCalendar parameter values.")

(defface ical:component-name
  '((default . (:inherit font-lock-constant-face)))
  "Face for iCalendar component names.")

(defface ical:keyword
  '((default . (:inherit font-lock-keyword-face)))
  "Face for other iCalendar keywords.")

(defface ical:binary-data
  '((default . (:inherit font-lock-comment-face)))
  "Face for iCalendar values that represent binary data.")

(defface ical:date-time-types
  '((default . (:inherit font-lock-type-face)))
  "Face for iCalendar values that represent time.
These include dates, date-times, durations, periods, and UTC offsets.")

(defface ical:numeric-types
  '((default . (:inherit ical:property-value-face)))
  "Face for iCalendar values that represent integers, floats, and geolocations.")

(defface ical:recurrence-rule
  '((default . (:inherit font-lock-type-face)))
  "Face for iCalendar recurrence rule values.")

(defface ical:uri
  '((default . (:inherit ical:property-value-face :underline t)))
  "Face for iCalendar values that are URIs (including URLs and mail addresses).")

(defface ical:warning
  '((default . (:inherit font-lock-warning-face)))
  "Face for iCalendar syntax errors.")

(defface ical:ignored
  '((default . (:inherit font-lock-comment-face)))
  "Face for iCalendar syntax which is parsed but ignored.")

;;; Font lock:
(defconst ical:params-font-lock-keywords
  '((ical:match-other-param
     (1 'font-lock-comment-face t t)
     (2 'font-lock-comment-face t t)
     (3 'ical:warning t t))
    (ical:match-value-param
     (1 'ical:parameter-name t t)
     (2 'ical:keyword t t)
     (3 'ical:warning t t))
    (ical:match-tzid-param
     (1 'ical:parameter-name t t)
     (2 'ical:parameter-value t t)
     (3 'ical:warning t t))
    (ical:match-sent-by-param
     (1 'ical:parameter-name t t)
     (2 'ical:uri t t)
     (3 'ical:warning t t))
    (ical:match-rsvp-param
     (1 'ical:parameter-name t t)
     (2 'ical:keyword t t)
     (3 'ical:warning t t))
    (ical:match-role-param
     (1 'ical:parameter-name t t)
     (2 'ical:keyword t t)
     (3 'ical:warning t t))
    (ical:match-reltype-param
     (1 'ical:parameter-name t t)
     (2 'ical:keyword t t)
     (3 'ical:warning t t))
    (ical:match-related-param
     (1 'ical:parameter-name t t)
     (2 'ical:keyword t t)
     (3 'ical:warning t t))
    (ical:match-range-param
     (1 'ical:parameter-name t t)
     (2 'ical:keyword t t)
     (3 'ical:warning t t))
    (ical:match-partstat-param
     (1 'ical:parameter-name t t)
     (2 'ical:keyword t t)
     (3 'ical:warning t t))
    (ical:match-member-param
     (1 'ical:parameter-name t t)
     (2 'ical:uri t t)
     (3 'ical:warning t t))
    (ical:match-language-param
     (1 'ical:parameter-name t t)
     (2 'ical:parameter-value t t)
     (3 'ical:warning t t))
    (ical:match-fbtype-param
     (1 'ical:parameter-name t t)
     (2 'ical:keyword t t)
     (3 'ical:warning t t))
    (ical:match-fmttype-param
     (1 'ical:parameter-name t t)
     (2 'ical:parameter-value t t)
     (3 'ical:warning t t))
    (ical:match-encoding-param
     (1 'ical:parameter-name t t)
     (2 'ical:keyword t t)
     (3 'ical:warning t t))
    (ical:match-dir-param
     (1 'ical:parameter-name t t)
     (2 'ical:uri t t)
     (3 'ical:warning t t))
    (ical:match-delegated-to-param
     (1 'ical:parameter-name t t)
     (2 'ical:uri t t)
     (3 'ical:warning t t))
    (ical:match-delegated-from-param
     (1 'ical:parameter-name t t)
     (2 'ical:uri t t)
     (3 'ical:warning t t))
    (ical:match-cutype-param
     (1 'ical:parameter-name t t)
     (2 'ical:keyword t t)
     (3 'ical:warning t t))
    (ical:match-cn-param
     (1 'ical:parameter-name t t)
     (2 'ical:parameter-value t t)
     (3 'ical:warning t t))
    (ical:match-altrep-param
     (1 'ical:parameter-name t t)
     (2 'ical:uri t t)
     (3 'ical:warning t t)))
  "Entries for iCalendar property parameters in `font-lock-keywords'.")

(defconst ical:properties-font-lock-keywords
  '((ical:match-request-status-property
     (1 'ical:property-name t t)
     (2 'ical:property-value t t)
     (3 'ical:warning t t))
    (ical:match-other-property
     (1 'ical:property-name t t)
     (2 'ical:property-value t t)
     (3 'ical:warning t t))
    (ical:match-sequence-property
     (1 'ical:property-name t t)
     (2 'ical:numeric-types t t)
     (3 'ical:warning t t))
    (ical:match-last-modified-property
     (1 'ical:property-name t t)
     (2 'ical:date-time-types t t)
     (3 'ical:warning t t))
    (ical:match-dtstamp-property
     (1 'ical:property-name t t)
     (2 'ical:date-time-types t t)
     (3 'ical:warning t t))
    (ical:match-created-property
     (1 'ical:property-name t t)
     (2 'ical:date-time-types t t)
     (3 'ical:warning t t))
    (ical:match-trigger-property
     (1 'ical:property-name t t)
     (2 'ical:date-time-types t t)
     (3 'ical:warning t t))
    (ical:match-repeat-property
     (1 'ical:property-name t t)
     (2 'ical:numeric-types t t)
     (3 'ical:warning t t))
    (ical:match-action-property
     (1 'ical:property-name t t)
     (2 'ical:keyword t t)
     (3 'ical:warning t t))
    (ical:match-rrule-property
     (1 'ical:property-name t t)
     (2 'ical:recurrence-rule t t)
     (3 'ical:warning t t))
    (ical:match-rdate-property
     (1 'ical:property-name t t)
     (2 'ical:date-time-types t t)
     (3 'ical:warning t t))
    (ical:match-exdate-property
     (1 'ical:property-name t t)
     (2 'ical:date-time-types t t)
     (3 'ical:warning t t))
    (ical:match-uid-property
     (1 'ical:property-name t t)
     (2 'ical:property-value t t)
     (3 'ical:warning t t))
    (ical:match-url-property
     (1 'ical:property-name t t)
     (2 'ical:uri t t)
     (3 'ical:warning t t))
    (ical:match-related-to-property
     (1 'ical:property-name t t)
     (2 'ical:property-value t t)
     (3 'ical:warning t t))
    (ical:match-recurrence-id-property
     (1 'ical:property-name t t)
     (2 'ical:date-time-types t t)
     (3 'ical:warning t t))
    (ical:match-organizer-property
     (1 'ical:property-name t t)
     (2 'ical:uri t t)
     (3 'ical:warning t t))
    (ical:match-contact-property
     (1 'ical:property-name t t)
     (2 'ical:property-value t t)
     (3 'ical:warning t t))
    (ical:match-attendee-property
     (1 'ical:property-name t t)
     (2 'ical:uri t t)
     (3 'ical:warning t t))
    (ical:match-tzurl-property
     (1 'ical:property-name t t)
     (2 'ical:uri t t)
     (3 'ical:warning t t))
    (ical:match-tzoffsetto-property
     (1 'ical:property-name t t)
     (2 'ical:date-time-types t t)
     (3 'ical:warning t t))
    (ical:match-tzoffsetfrom-property
     (1 'ical:property-name t t)
     (2 'ical:date-time-types t t)
     (3 'ical:warning t t))
    (ical:match-tzname-property
     (1 'ical:property-name t t)
     (2 'ical:property-value t t)
     (3 'ical:warning t t))
    (ical:match-tzid-property
     (1 'ical:property-name t t)
     (2 'ical:property-value t t)
     (3 'ical:warning t t))
    (ical:match-transp-property
     (1 'ical:property-name t t)
     (2 'ical:keyword t t)
     (3 'ical:warning t t))
    (ical:match-freebusy-property
     (1 'ical:property-name t t)
     (2 'ical:date-time-types t t)
     (3 'ical:warning t t))
    (ical:match-duration-property
     (1 'ical:property-name t t)
     (2 'ical:date-time-types t t)
     (3 'ical:warning t t))
    (ical:match-dtstart-property
     (1 'ical:property-name t t)
     (2 'ical:date-time-types t t)
     (3 'ical:warning t t))
    (ical:match-due-property
     (1 'ical:property-name t t)
     (2 'ical:date-time-types t t)
     (3 'ical:warning t t))
    (ical:match-dtend-property
     (1 'ical:property-name t t)
     (2 'ical:date-time-types t t)
     (3 'ical:warning t t))
    (ical:match-completed-property
     (1 'ical:property-name t t)
     (2 'ical:date-time-types t t)
     (3 'ical:warning t t))
    (ical:match-summary-property
     (1 'ical:property-name t t)
     (2 'ical:property-value t t)
     (3 'ical:warning t t))
    (ical:match-status-property
     (1 'ical:property-name t t)
     (2 'ical:keyword t t)
     (3 'ical:warning t t))
    (ical:match-resources-property
     (1 'ical:property-name t t)
     (2 'ical:property-value t t)
     (3 'ical:warning t t))
    (ical:match-priority-property
     (1 'ical:property-name t t)
     (2 'ical:numeric-types t t)
     (3 'ical:warning t t))
    (ical:match-percent-complete-property
     (1 'ical:property-name t t)
     (2 'ical:numeric-types t t)
     (3 'ical:warning t t))
    (ical:match-location-property
     (1 'ical:property-name t t)
     (2 'ical:property-value t t)
     (3 'ical:warning t t))
    (ical:match-geo-property
     (1 'ical:property-name t t)
     (2 'ical:numeric-types t t)
     (3 'ical:warning t t))
    (ical:match-description-property
     (1 'ical:property-name t t)
     (2 'ical:property-value t t)
     (3 'ical:warning t t))
    (ical:match-comment-property
     (1 'ical:property-name t t)
     (2 'ical:property-value t t)
     (3 'ical:warning t t))
    (ical:match-class-property
     (1 'ical:property-name t t)
     (2 'ical:keyword t t)
     (3 'ical:warning t t))
    (ical:match-categories-property
     (1 'ical:property-name t t)
     (2 'ical:property-value t t)
     (3 'ical:warning t t))
    (ical:match-attach-property
     (1 'ical:property-name t t)
     (2 'ical:property-value t t)
     (3 'ical:warning t t)
     (13 'ical:uri t t)
     (14 'ical:binary-data t t))
    (ical:match-version-property
     (1 'ical:property-name t t)
     (2 'ical:property-value t t)
     (3 'ical:warning t t))
    (ical:match-prodid-property
     (1 'ical:property-name t t)
     (2 'ical:property-value t t)
     (3 'ical:warning t t))
    (ical:match-method-property
     (1 'ical:property-name t t)
     (2 'ical:property-value t t)
     (3 'ical:warning t t))
    (ical:match-calscale-property
     (1 'ical:property-name t t)
     (2 'ical:keyword t t)
     (3 'ical:warning t t)))
  "Entries for iCalendar properties in `font-lock-keywords'.")

(defconst ical:ignored-properties-font-lock-keywords
  `((,(rx ical:other-property) (1 'ical:ignored keep t)
                               (2 'ical:ignored keep t)))
  "Entries for iCalendar ignored properties in `font-lock-keywords'.")

(defconst ical:components-font-lock-keywords
  '((ical:match-vcalendar-component
     (1 'ical:keyword t t)
     (2 'ical:component-name t t))
    (ical:match-other-component
     (1 'ical:keyword t t)
     (2 'ical:component-name t t))
    (ical:match-valarm-component
     (1 'ical:keyword t t)
     (2 'ical:component-name t t))
    (ical:match-daylight-component
     (1 'ical:keyword t t)
     (2 'ical:component-name t t))
    (ical:match-standard-component
     (1 'ical:keyword t t)
     (2 'ical:component-name t t))
    (ical:match-vtimezone-component
     (1 'ical:keyword t t)
     (2 'ical:component-name t t))
    (ical:match-vfreebusy-component
     (1 'ical:keyword t t)
     (2 'ical:component-name t t))
    (ical:match-vjournal-component
     (1 'ical:keyword t t)
     (2 'ical:component-name t t))
    (ical:match-vtodo-component
     (1 'ical:keyword t t)
     (2 'ical:component-name t t))
    (ical:match-vevent-component
     (1 'ical:keyword t t)
     (2 'ical:component-name t t)))
  "Entries for iCalendar components in `font-lock-keywords'.")

(defvar ical:font-lock-keywords
  (append ical:params-font-lock-keywords
          ical:properties-font-lock-keywords
          ical:components-font-lock-keywords
          ical:ignored-properties-font-lock-keywords)
  "Value of `font-lock-keywords' for `icalendar-mode'.")


;; The major mode:

;;; Mode hook
(defvar ical:mode-hook nil
  "Hook run when activating `icalendar-mode'.")

;;; Activating the mode for .ics files:
(add-to-list 'auto-mode-alist '("\\.ics\\'" . icalendar-mode))

;;; Syntax table
(defvar ical:mode-syntax-table
    (let ((st (make-syntax-table)))
      ;; Characters for which the standard syntax table suffices:
      ;; ; (punctuation): separates some property values, and property parameters
      ;; " (string): begins and ends string values
      ;; : (punctuation): separates property name (and parameters) from property
      ;;                  values
      ;; , (punctuation): separates values in a list
      ;; CR, LF (whitespace): content line endings
      ;; space (whitespace): when at the beginning of a line, continues the
      ;;                     previous line

      ;; Characters which need to be adjusted from the standard syntax table:
      ;; = is punctuation, not a symbol constituent:
      (modify-syntax-entry ?= ".   " st)
      ;; / is punctuation, not a symbol constituent:
      (modify-syntax-entry ?/ ".   " st)
      st)
    "Syntax table used in `icalendar-mode'.")

;;; Coding systems

;; Provide a hint to the decoding system that iCalendar files use DOS
;; line endings.  This appears to be the simplest way to ensure that
;; `find-file' will correctly decode an iCalendar file, since decoding
;; happens before icalendar-mode starts.
(add-to-list 'file-coding-system-alist '("\\.ics\\'" . undecided-dos))

;;; Format conversion

;; We use the format conversion infrastructure provided by format.el,
;; `insert-file-contents', and `write-region' to automatically perform
;; line unfolding when icalendar-mode starts in a buffer, and line
;; folding when it is saved to a file.  See Info node `(elisp)Format
;; Conversion' for more.

(defconst ical:format-definition
  '(text/calendar "iCalendar format"
                  nil ; no regexp - icalendar-mode runs decode instead
                  ical:unfold-region       ; decoding function
                  ical:folding-annotations ; encoding function
                  nil ; encoding function does not modify buffer
                  nil ; no need to activate a minor mode
                  t)  ; preserve the format when saving
  "Entry for iCalendar format in `format-alist'.")

(add-to-list 'format-alist ical:format-definition)

(defun ical:-format-decode-buffer ()
  "Call `format-decode-buffer' with the \\='text/calendar format.
This function is intended to be run from `icalendar-mode-hook'."
  (format-decode-buffer 'text/calendar))

(add-hook 'ical:mode-hook #'ical:-format-decode-buffer -90)

(defun ical:-disable-auto-fill ()
  "Disable `auto-fill-mode' in iCalendar buffers.
Auto-fill-mode interferes with line folding and syntax highlighting, so
it is off by default in iCalendar buffers.  This function is intended to
be run from `icalendar-mode-hook'."
  (when auto-fill-function
    (auto-fill-mode -1)))

(add-hook 'ical:mode-hook #'ical:-disable-auto-fill -91)

;;; Commands

(defun ical:switch-to-unfolded-buffer ()
  "Switch to a new buffer with content lines unfolded.
The new buffer will contain the same data as the current buffer, but
with content lines unfolded (before decoding, if possible).

`Folding' means inserting a line break and a single whitespace
character to continue lines longer than 75 octets; `unfolding'
means removing the extra whitespace inserted by folding.  The
iCalendar standard (RFC5545) requires folding lines when
serializing data to iCalendar format, and unfolding before
parsing it.  In `icalendar-mode', folded lines may not have proper
syntax highlighting; this command allows you to view iCalendar
data with proper syntax highlighting, as the parser sees it.

If the current buffer is visiting a file, this function will
offer to save the buffer first, and then reload the contents from
the file, performing unfolding with `icalendar-unfold-undecoded-region'
before decoding it.  This is the most reliable way to unfold lines.

If it is not visiting a file, it will unfold the new buffer
with `icalendar-unfold-region'.  This can in some cases have
undesirable effects (see its docstring), so the original contents
are preserved unchanged in the current buffer.

In both cases, after switching to the new buffer, this command
offers to kill the original buffer.

It is recommended to turn off `auto-fill-mode' when viewing an
unfolded buffer, so that filling does not interfere with syntax
highlighting.  This function offers to disable `auto-fill-mode' if
it is enabled in the new buffer; consider using
`visual-line-mode' instead."
  (interactive)
  (when (and buffer-file-name (buffer-modified-p))
    (when (y-or-n-p (format "Save before reloading from %s?"
                            (file-name-nondirectory buffer-file-name)))
      (save-buffer)))
  (let ((old-buffer (current-buffer))
        (mmode major-mode)
        (uf-buffer (if buffer-file-name
                       (ical:unfolded-buffer-from-file buffer-file-name)
                     (ical:unfolded-buffer-from-buffer (current-buffer)))))
    (switch-to-buffer uf-buffer)
    ;; restart original major mode, in case the new buffer is
    ;; still in fundamental-mode: TODO: is this necessary?
    (funcall mmode)
    (when (y-or-n-p (format "Unfolded buffer is shown.  Kill %s?"
                            (buffer-name old-buffer)))
      (kill-buffer old-buffer))
    (when (and auto-fill-function (y-or-n-p "Disable auto-fill-mode?"))
      (auto-fill-mode -1))))

;;; Mode definition
;;;###autoload
(define-derived-mode icalendar-mode text-mode "iCalendar"
  "Major mode for viewing and editing iCalendar (RFC5545) data.

This mode provides syntax highlighting for iCalendar components,
properties, values, and property parameters, and defines a format to
automatically handle folding and unfolding iCalendar content lines.

`Folding' means inserting whitespace characters to continue long
lines; `unfolding' means removing the extra whitespace inserted
by folding.  The iCalendar standard requires folding lines when
serializing data to iCalendar format, and unfolding before
parsing it.

Thus icalendar-mode's syntax highlighting is designed to work with
unfolded lines.  When `icalendar-mode' is activated in a buffer, it will
automatically unfold lines using a file format conversion, and
automatically fold lines when saving the buffer to a file; see Info
node `(elisp)Format Conversion' for more information.  It also disables
`auto-fill-mode' if it is active, since filling interferes with line
folding and syntax highlighting.  Consider using `visual-line-mode' in
`icalendar-mode' instead."
  :group 'icalendar
  :syntax-table ical:mode-syntax-table
  ;; TODO: Keymap?
  ;; TODO: buffer-local variables?
  ;; TODO: indent-line-function and indentation variables
  ;; TODO: mode-specific menu and context menus
  ;; TODO: eldoc integration
  ;; TODO: completion of keywords
  (progn
    (setq font-lock-defaults '(ical:font-lock-keywords nil t))))

(provide 'icalendar-mode)

;; Local Variables:
;; read-symbol-shorthands: (("ical:" . "icalendar-"))
;; End:
;;; icalendar-mode.el ends here
