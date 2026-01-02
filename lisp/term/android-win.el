;;; android-win.el --- terminal set up for Android  -*- lexical-binding:t -*-

;; Copyright (C) 2023-2026 Free Software Foundation, Inc.

;; Author: FSF
;; Keywords: terminals, i18n, android

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

;; This file contains the support for initializing the Lisp side of
;; Android windowing.

;;; Code:


(unless (featurep 'android)
  (error "%s: Loading android-win without having Android"
         invocation-name))

;; Documentation-purposes only: actually loaded in loadup.el.
(require 'frame)
(require 'mouse)
(require 'fontset)
(require 'dnd)
(require 'touch-screen)

(add-to-list 'display-format-alist '(".*" . android))

(declare-function android-get-connection "androidfns.c")
(declare-function x-handle-args "common-win" (args))

;; Window system initialization.  This is extremely simple because all
;; initialization is done in android_term_init.

(cl-defmethod window-system-initialization (&context (window-system android)
                                                     &optional _ignored)
  "Set up the window system.  WINDOW-SYSTEM must be ANDROID.
DISPLAY is ignored on Android."
  ;; Create the default fontset.
  (create-default-fontset)
  ;; Just make sure the window system was initialized at startup.
  (android-get-connection))

(cl-defmethod frame-creation-function (params &context (window-system android))
  (x-create-frame-with-faces params))

(cl-defmethod handle-args-function (args &context (window-system android))
  ;; Android has no command line to provide arguments on.
  ;; However, call x-handle-args to handle file name args.
  (x-handle-args args))


;;; Selection support.

(declare-function android-clipboard-exists-p "androidselect.c")
(declare-function android-get-clipboard "androidselect.c")
(declare-function android-set-clipboard "androidselect.c")
(declare-function android-clipboard-owner-p "androidselect.c")
(declare-function android-get-clipboard-targets "androidselect.c")
(declare-function android-get-clipboard-data "androidselect.c")

(defvar android-primary-selection nil
  "The last string placed in the primary selection.
nil if there was no such string.

Android is not equipped with a primary selection of its own, so
Emacs emulates one in Lisp.")

(defvar android-secondary-selection nil
  "The last string placed in the secondary selection.
nil if there was no such string.

Android is not equipped with a secondary selection of its own, so
Emacs emulates one in Lisp.")

(defun android-get-clipboard-1 (data-type)
  "Return data saved from the clipboard.
DATA-TYPE is a selection conversion target.

`STRING' means return the contents of the clipboard as a string,
while `TARGETS' means return the types of all data present within
the clipboard as a vector.

Interpret any other symbol as a MIME type for which any clipboard
data is returned"
  (or (and (eq data-type 'STRING)
           (android-get-clipboard))
      (and (eq data-type 'TARGETS)
           (android-clipboard-exists-p)
           (vconcat [TARGETS STRING]
                    (let ((i nil))
                      (dolist (type (android-get-clipboard-targets))
                        ;; Don't report plain text as a valid target
                        ;; since it is addressed by STRING.
                        (unless (equal type "text/plain")
                          (push (intern type) i)))
                      (nreverse i))))
      (and (symbolp data-type)
           (android-get-clipboard-data (symbol-name data-type)))))

(defun android-get-primary (data-type)
  "Return the last string placed in the primary selection, or nil.
Return nil if DATA-TYPE is anything other than STRING or TARGETS."
  (when android-primary-selection
    (or (and (eq data-type 'STRING)
             android-primary-selection)
        (and (eq data-type 'TARGETS)
             [TARGETS STRING]))))

(defun android-get-secondary (data-type)
  "Return the last string placed in the secondary selection, or nil.
Return nil if DATA-TYPE is anything other than STRING or TARGETS."
  (when android-secondary-selection
    (or (and (eq data-type 'STRING)
             android-secondary-selection)
        (and (eq data-type 'TARGETS)
             [TARGETS STRING]))))

(defun android-selection-bounds (value)
  "Return bounds of selection value VALUE.
The return value is a list (BEG END BUF) if VALUE is a cons of
two markers or an overlay.  Otherwise, it is nil."
  (cond ((bufferp value)
	 (with-current-buffer value
	   (when (mark t)
	     (list (mark t) (point) value))))
	((and (consp value)
	      (markerp (car value))
	      (markerp (cdr value)))
	 (when (and (marker-buffer (car value))
		    (buffer-name (marker-buffer (car value)))
		    (eq (marker-buffer (car value))
			(marker-buffer (cdr value))))
	   (list (marker-position (car value))
		 (marker-position (cdr value))
		 (marker-buffer (car value)))))
	((overlayp value)
	 (when (overlay-buffer value)
	   (list (overlay-start value)
		 (overlay-end value)
		 (overlay-buffer value))))))

(defun android-encode-select-string (value)
  "Turn VALUE into a string suitable for placing in the clipboard.
VALUE should be something suitable for passing to
`gui-set-selection'."
  (unless (stringp value)
    (when-let* ((bounds (android-selection-bounds value)))
      (setq value (ignore-errors
                    (with-current-buffer (nth 2 bounds)
                      (buffer-substring (nth 0 bounds)
                                        (nth 1 bounds)))))))
  value)

(cl-defmethod gui-backend-get-selection (type data-type
                                              &context (window-system android))
  (cond ((eq type 'CLIPBOARD)
         (android-get-clipboard-1 data-type))
        ((eq type 'PRIMARY)
         (android-get-primary data-type))
        ((eq type 'SECONDARY)
         (android-get-secondary data-type))))

(cl-defmethod gui-backend-selection-exists-p (selection
                                              &context (window-system android))
  (cond ((eq selection 'CLIPBOARD)
         (android-clipboard-exists-p))
        ((eq selection 'PRIMARY)
         (not (null android-primary-selection)))
        ((eq selection 'SECONDARY)
         (not (null android-secondary-selection)))))

(cl-defmethod gui-backend-selection-owner-p (selection
                                             &context (window-system android))
  (cond ((eq selection 'CLIPBOARD)
         (let ((ownership (android-clipboard-owner-p)))
           ;; If ownership is `lambda', then Emacs couldn't establish
           ;; whether or not it owns the clipboard.
           (and (not (eq ownership 'lambda)) ownership)))
        ((eq selection 'PRIMARY)
         ;; Emacs always owns its own primary selection as long as it
         ;; exists.
         (not (null android-primary-selection)))
        ((eq selection 'SECONDARY)
         ;; Emacs always owns its own secondary selection as long as
         ;; it exists.
         (not (null android-secondary-selection)))))

(cl-defmethod gui-backend-set-selection (type value
                                              &context (window-system android))
  ;; First, try to turn value into a string.
  ;; Don't set anything if that did not work.
  (when-let* ((string (android-encode-select-string value)))
    (cond ((eq type 'CLIPBOARD)
           (android-set-clipboard string))
          ((eq type 'PRIMARY)
           (setq android-primary-selection string))
          ((eq type 'SECONDARY)
           (setq android-secondary-selection string)))))

;;; Character composition display.

(defvar android-preedit-overlay nil
  "The overlay currently used to display preedit text from a compose sequence.")

;; With some input methods, text gets inserted before Emacs is told to
;; remove any preedit text that was displayed, which causes both the
;; preedit overlay and the text to be visible for a brief period of
;; time.  This pre-command-hook clears the overlay before any command
;; and should be set whenever a preedit overlay is visible.
(defun android-clear-preedit-text ()
  "Clear the pre-edit overlay and remove itself from `pre-command-hook'.
This function should be installed in `pre-command-hook' whenever
preedit text is displayed."
  (when android-preedit-overlay
    (delete-overlay android-preedit-overlay)
    (setq android-preedit-overlay nil))
  (remove-hook 'pre-command-hook #'android-clear-preedit-text))

(defun android-preedit-text (event)
  "Display preedit text from a compose sequence in EVENT.
EVENT is a preedit-text event."
  (interactive "e")
  (when android-preedit-overlay
    (delete-overlay android-preedit-overlay)
    (setq android-preedit-overlay nil)
    (remove-hook 'pre-command-hook #'android-clear-preedit-text))
  (when (nth 1 event)
    (let ((string (propertize (nth 1 event) 'face '(:underline t))))
      (setq android-preedit-overlay (make-overlay (point) (point)))
      (add-hook 'pre-command-hook #'android-clear-preedit-text)
      (overlay-put android-preedit-overlay 'window (selected-window))
      (overlay-put android-preedit-overlay 'before-string string))))

(define-key special-event-map [preedit-text] 'android-preedit-text)


;; Android cursor shapes, named according to the X scheme.
;; Many X cursors are missing.

(defconst x-pointer-arrow 1000)
(defconst x-pointer-left-ptr 1000)
(defconst x-pointer-left-side 1020)
(defconst x-pointer-sb-h-double-arrow 1014)
(defconst x-pointer-sb-v-double-arrow 1015)
(defconst x-pointer-watch 1004)
(defconst x-pointer-xterm 1008)
(defconst x-pointer-invisible 0)


;; Drag-and-drop.  There are two formats of drag and drop event under
;; Android.  The data field of the first is set to a cons of X and Y,
;; which represent a position within a frame that something is being
;; dragged over, whereas that of the second is a cons of either symbol
;; `uri' or `text' and a list of URIs or text to insert.
;;
;; If a content:// URI is encountered, then it in turn designates a
;; file within the special-purpose /content/by-authority directory,
;; which facilitates accessing such atypical files.

(declare-function url-type "url-parse")
(declare-function url-host "url-parse")
(declare-function url-filename "url-parse")

(defun android-handle-dnd-event (event)
  "Respond to a drag-and-drop event EVENT.
If it reflects the motion of an item above a frame, call
`dnd-handle-movement' to move the cursor or scroll the window
under the item pursuant to the pertinent user options.

If it holds dropped text, insert such text within window at the
location of the drop.

If it holds a list of URIs, or file names, then open each URI or
file name, converting content:// URIs into the special file
names which represent them."
  (interactive "e")
  (let ((message (caddr event))
        (posn (event-start event)))
    (cond ((fixnump (car message))
           (dnd-handle-movement posn))
          ((eq (car message) 'text)
           (let ((window (posn-window posn)))
             (with-selected-window window
               (unless mouse-yank-at-point
                 (goto-char (posn-point (event-start event))))
               (dnd-insert-text window 'copy (cdr message)))))
          ((eq (car message) 'uri)
           (let ((uri-list (split-string (cdr message)
                                         "[\0\r\n]" t))
                 (new-uri-list nil)
                 (dnd-unescape-file-uris t))
             (dolist (uri uri-list)
               ;; If the URI is a prepared file name, insert it directly.
               (if (string-match-p "^/content/by-authority\\(-named\\)?/" uri)
                   (setq uri (concat "file:" uri)
                         dnd-unescape-file-uris nil)
                 (ignore-errors
                   (let ((url (url-generic-parse-url uri)))
                     (when (equal (url-type url) "content")
                       ;; Replace URI with a matching /content file
                       ;; name.
                       (setq uri (format "file:/content/by-authority/%s%s"
                                         (url-host url)
                                         (url-filename url))
                             ;; And guarantee that this file URI is not
                             ;; subject to URI decoding, for it must be
                             ;; transformed back into a content URI.
                             dnd-unescape-file-uris nil)))))
               (push uri new-uri-list))
             (dnd-handle-multiple-urls (posn-window posn)
                                       new-uri-list
                                       'copy))))))

(define-key special-event-map [drag-n-drop] 'android-handle-dnd-event)


;; Bind keys sent by input methods to manipulate the state of the
;; selection to commands which set or deactivate the mark.

(defun android-deactivate-mark-command ()
  "Deactivate the mark in this buffer.
This command is generally invoked by input methods sending
the `stop-selecting-text' editing key."
  (interactive)
  (deactivate-mark))

(global-set-key [select-all] 'mark-whole-buffer)
(global-set-key [start-selecting-text] 'set-mark-command)
(global-set-key [stop-selecting-text] 'android-deactivate-mark-command)


;; Splash screen notice.  Users are frequently left scratching their
;; heads when they overlook the Android appendix in the Emacs manual
;; and discover that external storage is not accessible; worse yet,
;; Android 11 and later veil the settings panel controlling such
;; permissions behind layer upon layer of largely immaterial settings
;; panels, such that several modified copies of the Android Settings
;; app have omitted them altogether after their developers conducted
;; their own interface simplifications.  Display a button on the
;; splash screen that instructs users on granting these permissions
;; when they are denied.

(declare-function android-external-storage-available-p "androidfns.c")
(declare-function android-request-storage-access "androidfns.c")
(declare-function android-request-directory-access "androidfns.c")

(defun android-display-storage-permission-popup (&optional _ignored)
  "Display a dialog regarding storage permissions.
Display a buffer explaining the need for storage permissions and
offering to grant them."
  (interactive)
  (with-current-buffer (get-buffer-create "*Android Permissions*")
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert (propertize "Storage Access Permissions"
                        'face '(bold (:height 1.2))))
    (insert "

Before Emacs can access your device's external storage
directories, such as /sdcard and /storage/emulated/0, you must
grant it permission to do so.

Alternatively, you can request access to a particular directory
in external storage, whereafter it will be available under the
directory /content/storage.

")
    (insert-button "Grant storage permissions"
                   'action (lambda (_)
                             (android-request-storage-access)
                             (quit-window)))
    (newline)
    (newline)
    (insert-button "Request access to directory"
                   'action (lambda (_)
                             (android-request-directory-access)))
    (newline)
    (special-mode)
    (setq buffer-read-only t))
  (let ((window (display-buffer "*Android Permissions*")))
    (when (windowp window)
      (with-selected-window window
        ;; Fill the text to the width of this window in columns if it
        ;; does not exceed 72, that the text might not be wrapped or
        ;; truncated.
        (when (<= (window-width window) 72)
          (let ((fill-column (window-width window))
                (inhibit-read-only t))
            (fill-region (point-min) (point-max))))))))

(defun android-before-splash-screen (fancy-p)
  "Insert a brief notice on the absence of storage permissions.
If storage permissions are as yet denied to Emacs, insert a short
notice to that effect, followed by a button that enables the user
to grant such permissions.

FANCY-P non-nil means the notice will be displayed with faces, in
the style appropriate for its incorporation within the fancy splash
screen display; see `fancy-splash-insert'."
  (unless (android-external-storage-available-p)
    (if fancy-p
        (fancy-splash-insert
         :face '(variable-pitch
                 font-lock-function-call-face)
         "Permissions necessary to access external storage directories have"
         "\nbeen denied.  Click "
         :link '("here" android-display-storage-permission-popup)
         " to grant them.\n")
      (insert
       "Permissions necessary to access external storage directories"
       "\nhave been denied.  ")
      (insert-button "Click here to grant them.\n"
                     'action #'android-display-storage-permission-popup
                     'follow-link t)
      (newline))))


;;; Locale preferences.

(defvar android-os-language)

(defun android-locale-for-system-language ()
  "Return a locale representing the system language.
This locale reflects the system's language preferences in its
language name and country variant fields, and always specifies
the UTF-8 coding system."
  ;; android-os-language is a list comprising four elements LANGUAGE,
  ;; COUNTRY, SCRIPT, and VARIANT.
  ;;
  ;; LANGUAGE and COUNTRY are ISO language and country codes identical
  ;; to those stored within POSIX locales.
  ;;
  ;; SCRIPT is an ISO 15924 script tag, representing the script used
  ;; if available, or if required to disambiguate between distinct
  ;; writing systems for the same combination of language and country.
  ;;
  ;; VARIANT is an arbitrary string representing the variant of the
  ;; LANGUAGE or SCRIPT represented.
  ;;
  ;; Each of these fields might be empty, but the locale is invalid if
  ;; LANGUAGE is empty, which if true "en_US.UTF-8" is returned as a
  ;; placeholder.
  (let ((language (or (nth 0 android-os-language) ""))
        (country (or (nth 1 android-os-language) ""))
        (script (or (nth 2 android-os-language) ""))
        (variant (or (nth 3 android-os-language) ""))
        locale-base locale-modifier)
    (if (string-empty-p language)
        (setq locale-base "en_US.UTF-8")
      (if (string-empty-p country)
          (setq locale-base (concat language ".UTF-8"))
        (setq locale-base (concat language "_" country
                                  ".UTF-8"))))
    ;; No straightforward relation between Java script and variant
    ;; combinations exist: Java permits both a script and a variant to
    ;; be supplied at once, whereas POSIX's closest analog "modifiers"
    ;; permit only either an alternative script or a variant to be
    ;; supplied.
    ;;
    ;; Emacs disregards variants besides "EURO" and scripts besides
    ;; "Cyrl", for these two never coexist in existing locales, and
    ;; their POSIX equivalents are the sole modifiers recognized by
    ;; Emacs.
    (if (string-equal script "Cyrl")
        (setq locale-modifier "@cyrillic")
      (if (string-equal variant "EURO")
          (setq locale-modifier "@euro")
        (setq locale-modifier "")))
    ;; Return the concatenation of both these values.
    (concat locale-base locale-modifier)))


;; Miscellaneous functions.

(declare-function android-browse-url-internal "androidselect.c")

(defun android-browse-url (url &optional send)
  "Open URL in an external application.

URL should be a URL-encoded URL with a scheme specified unless
SEND is non-nil.  Signal an error upon failure.

If SEND is nil, start a program that is able to display the URL,
such as a web browser.  Otherwise, try to share URL using
programs such as email clients.

If URL is a file URI, convert it into a `content' address
accessible to other programs."
  (when-let* ((uri (url-generic-parse-url url))
              (filename (url-filename uri))
              ;; If `uri' is a file URI and the file resides in /content
              ;; or /assets, copy it to a temporary file before
              ;; providing it to other programs.
              (replacement-url (and (string-match-p
                                     "/\\(content\\|assets\\)[/$]"
                                     filename)
                                    (prog1 t
                                      (copy-file
                                       filename
                                       (setq filename
                                             (make-temp-file
                                              "local"
                                              nil
                                              (let ((extension
                                                     (file-name-extension
                                                      filename)))
                                                (if extension
                                                    (concat "."
                                                            extension)
                                                  nil))))
                                       t))
                                    (concat "file://" filename))))
    (setq url replacement-url))
  (android-browse-url-internal url send))


;; Coding systems used by androidvfs.c.

(define-ccl-program android-encode-jni
  '(2 ((loop
	(read r0)
	(if (r0 < #x1) ; 0x0 is encoded specially in JNI environments.
	    ((write #xc0)
	     (write #x80))
	  ((if (r0 < #x80) ; ASCII
	       ((write r0))
	     (if (r0 < #x800) ; \u0080 - \u07ff
		 ((write ((r0 >> 6) | #xC0))
		  (write ((r0 & #x3F) | #x80)))
	       ;; \u0800 - \uFFFF
	       (if (r0 < #x10000)
		   ((write ((r0 >> 12) | #xE0))
		    (write (((r0 >> 6) & #x3F) | #x80))
		    (write ((r0 & #x3F) | #x80)))
		 ;; Supplementary characters must be converted into
		 ;; surrogate pairs before encoding.
		 (;; High surrogate
		  (r1 = ((((r0 - #x10000) >> 10) & #x3ff) + #xD800))
		  ;; Low surrogate.
		  (r2 = (((r0 - #x10000) & #x3ff) + #xDC00))
		  ;; Write both surrogate characters.
		  (write ((r1 >> 12) | #xE0))
		  (write (((r1 >> 6) & #x3F) | #x80))
		  (write ((r1 & #x3F) | #x80))
		  (write ((r2 >> 12) | #xE0))
		  (write (((r2 >> 6) & #x3F) | #x80))
		  (write ((r2 & #x3F) | #x80))))))))
	(repeat))))
  "Encode characters from the input buffer for Java virtual machines.")

(define-ccl-program android-decode-jni
  '(1 ((loop
        ((read-if (r0 >= #x80) ; More than a one-byte sequence?
		  ((if (r0 < #xe0)
		       ;; Two-byte sequence; potentially a NULL
		       ;; character.
		       ((read r4)
			(r4 &= #x3f)
			(r0 = (((r0 & #x1f) << 6) | r4)))
		     (if (r0 < ?\xF0)
			 ;; Three-byte sequence, after which surrogate
			 ;; pairs should be processed.
			 ((read r4 r6)
			  (r4 = ((r4 & #x3f) << 6))
			  (r6 &= #x3f)
			  (r0 = ((((r0 & #xf) << 12) | r4) | r6)))
		       ;; Four-byte sequences are not valid under the
		       ;; JVM specification, but Android produces them
		       ;; when encoding Emoji characters for being
		       ;; supposedly less of a surprise to applications.
		       ;; This is obviously not true of programs written
		       ;; to the letter of the documentation, but 50
		       ;; million Frenchmen make a right (and this
		       ;; deviation from the norm is predictably absent
		       ;; from Android's documentation on the subject).
		       ((read r1 r4 r6)
			(r1 = ((r1 & #x3f) << 12))
			(r4 = ((r4 & #x3f) << 6))
			(r6 &= #x3F)
			(r0 = (((((r0 & #x07) << 18) | r1) | r4) | r6))))))))
	(if ((r0 & #xf800) == #xd800)
	    ;; High surrogate.
	    ((read-if (r2 >= #xe0)
		      ((r0 = ((r0 & #x3ff) << 10))
		       (read r4 r6)
		       (r4 = ((r4 & #x3f) << 6))
		       (r6 &= #x3f)
		       (r1 = ((((r2 & #xf) << 12) | r4) | r6))
		       (r0 = (((r1 & #x3ff) | r0) + #xffff))))))
	(write r0)
	(repeat))))
  "Decode JVM-encoded characters in the input buffer.")

(define-coding-system 'android-jni
  "CESU-8 based encoding for communication with the Android runtime."
  :mnemonic ?J
  :coding-type 'ccl
  :eol-type 'unix
  :ascii-compatible-p nil ; for \0 is encoded as a two-byte sequence.
  :default-char ?\0
  :charset-list '(unicode)
  :ccl-decoder 'android-decode-jni
  :ccl-encoder 'android-encode-jni)


;; Default key definitions.

;; Suppress KEYCODE_NOTIFICATION, which has been observed to be
;; spontaneously generated on certain tablets, so that the user may not
;; be disturbed by intrusive messages when it is registered.
(global-set-key [KEYCODE_NOTIFICATION] #'ignore)
(global-set-key [\83] #'ignore) ; KEYCODE_NOTIFICATION on pre-Honeycomb
                                ; releases.


(provide 'android-win)
;;; android-win.el ends here
