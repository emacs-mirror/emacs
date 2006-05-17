;;; mh-utils.el --- MH-E code needed for both sending and reading

;; Copyright (C) 1993, 1995, 1997,
;; 2000, 2001, 2002, 2003, 2004, 2005 Free Software Foundation, Inc.

;; Author: Bill Wohler <wohler@newt.com>
;; Maintainer: Bill Wohler <wohler@newt.com>
;; Keywords: mail
;; See: mh-e.el

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Internal support for MH-E package.

;;; Change Log:

;;; Code:

(defvar recursive-load-depth-limit)
(eval-and-compile
  (if (and (boundp 'recursive-load-depth-limit)
           (integerp recursive-load-depth-limit)
           (> 50 recursive-load-depth-limit))
      (setq recursive-load-depth-limit 50)))

(eval-when-compile (require 'mh-acros))
(mh-require-cl)
(require 'gnus-util)
(require 'font-lock)
(require 'mouse)
(load "tool-bar" t t)
(require 'mh-loaddefs)
(require 'mh-customize)
(require 'mh-inc)

(load "mm-decode" t t)                  ; Non-fatal dependency
(load "mm-view" t t)                    ; Non-fatal dependency
(load "vcard" t t)                      ; Non-fatal dependency
(load "hl-line" t t)                    ; Non-fatal dependency
(load "executable" t t)                 ; Non-fatal dependency on
                                        ; executable-find

;; Shush the byte-compiler
(defvar font-lock-auto-fontify)
(defvar font-lock-defaults)
(defvar mark-active)

;;; Autoloads
(autoload 'gnus-article-highlight-citation "gnus-cite")
(autoload 'message-fetch-field "message")
(autoload 'message-tokenize-header "message")
(require 'sendmail)
(unless (fboundp 'make-hash-table)
  (autoload 'make-hash-table "cl"))

;;; CL Replacements
(defun mh-search-from-end (char string)
  "Return the position of last occurrence of CHAR in STRING.
If CHAR is not present in STRING then return nil. The function is used in lieu
of `search' in the CL package."
  (loop for index from (1- (length string)) downto 0
        when (equal (aref string index) char) return index
        finally return nil))

;;; Additional header fields that might someday be added:
;;; "Sender: " "Reply-to: "


;;; Scan Line Formats

(defvar mh-scan-msg-number-regexp "^ *\\([0-9]+\\)"
  "This regexp is used to extract the message number from a scan line.
Note that the message number must be placed in a parenthesized expression as
in the default of \"^ *\\\\([0-9]+\\\\)\".")

(defvar mh-scan-msg-overflow-regexp "^[?0-9][0-9]"
  "This regexp matches scan lines in which the message number overflowed.")

(defvar mh-scan-msg-format-regexp "%\\([0-9]*\\)(msg)"
  "This regexp is used to find the message number width in a scan format.
Note that the message number must be placed in a parenthesized expression as
in the default of \"%\\\\([0-9]*\\\\)(msg)\".")

(defvar mh-scan-msg-format-string "%d"
  "This is a format string for width of the message number in a scan format.
Use `0%d' for zero-filled message numbers.")

(defvar mh-scan-msg-search-regexp "^[^0-9]*%d[^0-9]"
  "This format string regexp matches the scan line for a particular message.
Use `%d' to represent the location of the message number within the
expression as in the default of \"^[^0-9]*%d[^0-9]\".")

(defvar mh-cmd-note 4
  "This is the number of characters to skip over before inserting notation.
This variable should be set with the function `mh-set-cmd-note'. This variable
may be updated dynamically if `mh-adaptive-cmd-note-flag' is non-nil and
`mh-scan-format-file' is t.")
(make-variable-buffer-local 'mh-cmd-note)

(defvar mh-note-seq ?%
  "Messages in a user-defined sequence are marked by this character.
Messages in the `search' sequence are marked by this character as well.")



(defvar mh-show-buffer-mode-line-buffer-id "    {show-%s} %d"
  "Format string to produce `mode-line-buffer-identification' for show buffers.
First argument is folder name.  Second is message number.")



(defvar mh-mail-header-separator "--------"
  "*Line used by MH to separate headers from text in messages being composed.
This variable should not be used directly in programs. Programs should use
`mail-header-separator' instead. `mail-header-separator' is initialized to
`mh-mail-header-separator' in `mh-letter-mode'; in other contexts, you may
have to perform this initialization yourself.

Do not make this a regexp as it may be the argument to `insert' and it is
passed through `regexp-quote' before being used by functions like
`re-search-forward'.")

(defvar mh-signature-separator-regexp "^-- $"
  "Regexp used to find signature separator.
See `mh-signature-separator'.")

(defvar mh-signature-separator "-- \n"
  "Text of a signature separator.
A signature separator is used to separate the body of a message from the
signature. This can be used by user agents such as MH-E to render the
signature differently or to suppress the inclusion of the signature in a
reply.
Use `mh-signature-separator-regexp' when searching for a separator.")

(defun mh-signature-separator-p ()
  "Return non-nil if buffer includes \"^-- $\"."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward mh-signature-separator-regexp nil t)))

;; Variables for MIME display

;; Structure to keep track of MIME handles on a per buffer basis.
(mh-defstruct (mh-buffer-data (:conc-name mh-mime-)
                              (:constructor mh-make-buffer-data))
  (handles ())                          ; List of MIME handles
  (handles-cache (make-hash-table))     ; Cache to avoid multiple decodes of
                                        ; nested messages
  (parts-count 0)                       ; The button number is generated from
                                        ; this number
  (part-index-hash (make-hash-table)))  ; Avoid incrementing the part number
                                        ; for nested messages
;;; This has to be a macro, since we do: (setf (mh-buffer-data) ...)
(defmacro mh-buffer-data ()
  "Convenience macro to get the MIME data structures of the current buffer."
  `(gethash (current-buffer) mh-globals-hash))

(defvar mh-globals-hash (make-hash-table)
  "Keeps track of MIME data on a per buffer basis.")

(defvar mh-gnus-pgp-support-flag (not (not (locate-library "mml2015")))
  "Non-nil means installed Gnus has PGP support.")

(defvar mh-mm-inline-media-tests
  `(("image/jpeg"
     mm-inline-image
     (lambda (handle)
       (mm-valid-and-fit-image-p 'jpeg handle)))
    ("image/png"
     mm-inline-image
     (lambda (handle)
       (mm-valid-and-fit-image-p 'png handle)))
    ("image/gif"
     mm-inline-image
     (lambda (handle)
       (mm-valid-and-fit-image-p 'gif handle)))
    ("image/tiff"
     mm-inline-image
     (lambda (handle)
       (mm-valid-and-fit-image-p 'tiff handle)) )
    ("image/xbm"
     mm-inline-image
     (lambda (handle)
       (mm-valid-and-fit-image-p 'xbm handle)))
    ("image/x-xbitmap"
     mm-inline-image
     (lambda (handle)
       (mm-valid-and-fit-image-p 'xbm handle)))
    ("image/xpm"
     mm-inline-image
     (lambda (handle)
       (mm-valid-and-fit-image-p 'xpm handle)))
    ("image/x-pixmap"
     mm-inline-image
     (lambda (handle)
       (mm-valid-and-fit-image-p 'xpm handle)))
    ("image/bmp"
     mm-inline-image
     (lambda (handle)
       (mm-valid-and-fit-image-p 'bmp handle)))
    ("image/x-portable-bitmap"
     mm-inline-image
     (lambda (handle)
       (mm-valid-and-fit-image-p 'pbm handle)))
    ("text/plain" mm-inline-text identity)
    ("text/enriched" mm-inline-text identity)
    ("text/richtext" mm-inline-text identity)
    ("text/x-patch" mm-display-patch-inline
     (lambda (handle)
       (locate-library "diff-mode")))
    ("application/emacs-lisp" mm-display-elisp-inline identity)
    ("application/x-emacs-lisp" mm-display-elisp-inline identity)
    ("text/html"
     ,(if (fboundp 'mm-inline-text-html) 'mm-inline-text-html 'mm-inline-text)
     (lambda (handle)
       (or (and (boundp 'mm-inline-text-html-renderer)
                mm-inline-text-html-renderer)
           (and (boundp 'mm-text-html-renderer) mm-text-html-renderer))))
    ("text/x-vcard"
     mm-inline-text-vcard
     (lambda (handle)
       (or (featurep 'vcard)
           (locate-library "vcard"))))
    ("message/delivery-status" mm-inline-text identity)
    ("message/rfc822" mh-mm-inline-message identity)
    ;;("message/partial" mm-inline-partial identity)
    ;;("message/external-body" mm-inline-external-body identity)
    ("text/.*" mm-inline-text identity)
    ("audio/wav" mm-inline-audio
     (lambda (handle)
       (and (or (featurep 'nas-sound) (featurep 'native-sound))
            (device-sound-enabled-p))))
    ("audio/au"
     mm-inline-audio
     (lambda (handle)
       (and (or (featurep 'nas-sound) (featurep 'native-sound))
            (device-sound-enabled-p))))
    ("application/pgp-signature" ignore identity)
    ("application/x-pkcs7-signature" ignore identity)
    ("application/pkcs7-signature" ignore identity)
    ("application/x-pkcs7-mime" ignore identity)
    ("application/pkcs7-mime" ignore identity)
    ("multipart/alternative" ignore identity)
    ("multipart/mixed" ignore identity)
    ("multipart/related" ignore identity)
    ;; Disable audio and image
    ("audio/.*" ignore ignore)
    ("image/.*" ignore ignore)
    ;; Default to displaying as text
    (".*" mm-inline-text mm-readable-p))
  "Alist of media types/tests saying whether types can be displayed inline.")

;; Copy of `goto-address-mail-regexp'
(defvar mh-address-mail-regexp
  "[-a-zA-Z0-9._]+@\\([-a-zA-z0-9_]+\\.\\)+[a-zA-Z0-9]+"
  "A regular expression probably matching an e-mail address.")

;; From goto-addr.el, which we don't want to force-load on users.

(defun mh-goto-address-find-address-at-point ()
  "Find e-mail address around or before point.
Then search backwards to beginning of line for the start of an e-mail
address.  If no e-mail address found, return nil."
  (re-search-backward "[^-_A-z0-9.@]" (line-beginning-position) 'lim)
  (if (or (looking-at mh-address-mail-regexp)	; already at start
	  (and (re-search-forward mh-address-mail-regexp
				  (line-end-position) 'lim)
	       (goto-char (match-beginning 0))))
      (match-string-no-properties 0)))

(defun mh-mail-header-end ()
  "Substitute for `mail-header-end' that doesn't widen the buffer.
In MH-E we frequently need to find the end of headers in nested messages, where
the buffer has been narrowed. This function works in this situation."
  (save-excursion
    ;; XXX: The following replaces a call to rfc822-goto-eoh. Occasionally,
    ;; mail headers that MH-E has to read contains lines of the form:
    ;;    From xxx@yyy Mon May 10 11:48:07 2004
    ;; In this situation, rfc822-goto-eoh doesn't go to the end of the
    ;; header. The replacement allows From_ lines in the mail header.
    (goto-char (point-min))
    (loop for p = (re-search-forward
                   "^\\([:\n]\\|[^: \t\n]+[ \t\n]\\)" nil 'move)
          do (cond ((null p) (return))
                   (t (goto-char (match-beginning 0))
                      (unless (looking-at "From ") (return))
                      (goto-char p))))
    (point)))

(defun mh-in-header-p ()
  "Return non-nil if the point is in the header of a draft message."
  (< (point) (mh-mail-header-end)))

(defun mh-header-field-beginning ()
  "Move to the beginning of the current header field.
Handles RFC 822 continuation lines."
  (beginning-of-line)
  (while (looking-at "^[ \t]")
    (forward-line -1)))

(defun mh-header-field-end ()
  "Move to the end of the current header field.
Handles RFC 822 continuation lines."
  (forward-line 1)
  (while (looking-at "^[ \t]")
    (forward-line 1))
  (backward-char 1))                    ;to end of previous line

(defun mh-letter-header-font-lock (limit)
  "Return the entire mail header to font-lock.
Argument LIMIT limits search."
  (if (= (point) limit)
      nil
    (let* ((mail-header-end (save-match-data (mh-mail-header-end)))
           (lesser-limit (if (< mail-header-end limit) mail-header-end limit)))
      (when (mh-in-header-p)
        (set-match-data (list 1 lesser-limit))
        (goto-char lesser-limit)
        t))))

(defun mh-header-field-font-lock (field limit)
  "Return the value of a header field FIELD to font-lock.
Argument LIMIT limits search."
  (if (= (point) limit)
      nil
    (let* ((mail-header-end (mh-mail-header-end))
           (lesser-limit (if (< mail-header-end limit) mail-header-end limit))
           (case-fold-search t))
      (when (and (< (point) mail-header-end) ;Only within header
                 (re-search-forward (format "^%s" field) lesser-limit t))
        (let ((match-one-b (match-beginning 0))
              (match-one-e (match-end 0)))
          (mh-header-field-end)
          (if (> (point) limit)         ;Don't search for end beyond limit
              (goto-char limit))
          (set-match-data (list match-one-b match-one-e
                                (1+ match-one-e) (point)))
          t)))))

(defun mh-header-to-font-lock (limit)
  "Return the value of a header field To to font-lock.
Argument LIMIT limits search."
  (mh-header-field-font-lock "To:" limit))

(defun mh-header-cc-font-lock (limit)
  "Return the value of a header field cc to font-lock.
Argument LIMIT limits search."
  (mh-header-field-font-lock "cc:" limit))

(defun mh-header-subject-font-lock (limit)
  "Return the value of a header field Subject to font-lock.
Argument LIMIT limits search."
  (mh-header-field-font-lock "Subject:" limit))

(eval-and-compile
  ;; Otherwise byte-compilation fails on `mh-show-font-lock-keywords-with-cite'
  (defvar mh-show-font-lock-keywords
    '(("^\\(From:\\|Sender:\\)\\(.*\\)"  (1 'default) (2 mh-show-from-face))
      (mh-header-to-font-lock            (0 'default) (1 mh-show-to-face))
      (mh-header-cc-font-lock            (0 'default) (1 mh-show-cc-face))
      ("^\\(Reply-To:\\|Return-Path:\\)\\(.*\\)$"
       (1 'default) (2 mh-show-from-face))
      (mh-header-subject-font-lock       (0 'default) (1 mh-show-subject-face))
      ("^\\(Apparently-To:\\|Newsgroups:\\)\\(.*\\)"
       (1 'default) (2 mh-show-cc-face))
      ("^\\(In-reply-to\\|Date\\):\\(.*\\)$"
       (1 'default) (2 mh-show-date-face))
      (mh-letter-header-font-lock        (0 mh-show-header-face append t)))
    "Additional expressions to highlight in MH-show mode."))

(defvar mh-show-font-lock-keywords-with-cite
  (eval-when-compile
    (let* ((cite-chars "[>|}]")
           (cite-prefix "A-Za-z")
           (cite-suffix (concat cite-prefix "0-9_.@-`'\"")))
      (append
       mh-show-font-lock-keywords
       (list
        ;; Use MATCH-ANCHORED to effectively anchor the regexp left side.
        `(,cite-chars
          (,(concat "\\=[ \t]*"
                    "\\(\\([" cite-prefix "]+[" cite-suffix "]*\\)?"
                    "\\(" cite-chars "[ \t]*\\)\\)+"
                    "\\(.*\\)")
           (beginning-of-line) (end-of-line)
           (2 font-lock-constant-face nil t)
           (4 font-lock-comment-face nil t)))))))
  "Additional expressions to highlight in MH-show mode.")

(defvar mh-letter-font-lock-keywords
  `(,@mh-show-font-lock-keywords-with-cite
    (mh-font-lock-field-data (1 'mh-letter-header-field prepend t))))

(defun mh-show-font-lock-fontify-region (beg end loudly)
  "Limit font-lock in `mh-show-mode' to the header.
Used when `mh-highlight-citation-p' is set to gnus, leaving the body to be
dealt with by gnus highlighting. The region between BEG and END is
given over to be fontified and LOUDLY controls if a user sees a
message about the fontification operation."
  (let ((header-end (mh-mail-header-end)))
    (cond
     ((and (< beg header-end)(< end header-end))
      (font-lock-default-fontify-region beg end loudly))
     ((and (< beg header-end)(>= end header-end))
      (font-lock-default-fontify-region beg header-end loudly))
     (t
      nil))))

;; Needed to help shush the byte-compiler.
(if mh-xemacs-flag
    (progn
      (eval-and-compile
        (require 'gnus)
        (require 'gnus-art)
        (require 'gnus-cite))))

(defun mh-gnus-article-highlight-citation ()
  "Highlight cited text in current buffer using gnus."
  (interactive)
  ;; Requiring gnus-cite should have been sufficient. However for Emacs21.1,
  ;; recursive-load-depth-limit is only 10, so an error occurs. Also it may be
  ;; better to have an autoload at top-level (though that won't work because
  ;; of recursive-load-depth-limit). That gets rid of a compiler warning as
  ;; well.
  (unless mh-xemacs-flag
    (require 'gnus-art)
    (require 'gnus-cite))
  ;; Don't allow Gnus to create buttons while highlighting, maybe this is bad
  ;; style?
  (flet ((gnus-article-add-button (&rest args) nil))
    (let* ((modified (buffer-modified-p))
           (gnus-article-buffer (buffer-name))
           (gnus-cite-face-list `(,@(cdr gnus-cite-face-list)
                                    ,(car gnus-cite-face-list))))
      (gnus-article-highlight-citation t)
      (set-buffer-modified-p modified))))

;;; Internal bookkeeping variables:

;; Cached value of the `Path:' component in the user's MH profile.
;; User's mail folder directory.
(defvar mh-user-path nil)

;; An mh-draft-folder of nil means do not use a draft folder.
;; Cached value of the `Draft-Folder:' component in the user's MH profile.
;; Name of folder containing draft messages.
(defvar mh-draft-folder nil)

;; Cached value of the `Unseen-Sequence:' component in the user's MH profile.
;; Name of the Unseen sequence.
(defvar mh-unseen-seq nil)

;; Cached value of the `Previous-Sequence:' component in the user's MH
;; profile.
;; Name of the Previous sequence.
(defvar mh-previous-seq nil)

;; Cached value of the `Inbox:' component in the user's MH profile,
;; or "+inbox" if no such component.
;; Name of the Inbox folder.
(defvar mh-inbox nil)

;; The names of ephemeral buffers have a " *mh-" prefix (so that they are
;; hidden and can be programmatically removed in mh-quit), and the variable
;; names have the form mh-temp-.*-buffer.
(defconst mh-temp-buffer " *mh-temp*")  ;scratch
(defconst mh-temp-fetch-buffer " *mh-fetch*")  ;wget/curl/fetch output

;; The names of MH-E buffers that are not ephemeral and can be used by the
;; user (and deleted by the user when no longer needed) have a "*MH-E " prefix
;; (so they can be programmatically removed in mh-quit), and the variable
;; names have the form mh-.*-buffer.
(defconst mh-aliases-buffer "*MH-E Aliases*") ;alias lookups
(defconst mh-folders-buffer "*MH-E Folders*") ;folder list
(defconst mh-help-buffer "*MH-E Help*") ;quick help
(defconst mh-info-buffer "*MH-E Info*") ;version information buffer
(defconst mh-log-buffer "*MH-E Log*") ;output of MH commands and so on
(defconst mh-mail-delivery-buffer "*MH-E Mail Delivery*") ;mail delivery log
(defconst mh-recipients-buffer "*MH-E Recipients*") ;killed when draft sent
(defconst mh-sequences-buffer "*MH-E Sequences*") ;sequences list

;; Number of lines to keep in mh-log-buffer.
(defvar mh-log-buffer-lines 100)

;; Window configuration before MH-E command.
(defvar mh-previous-window-config nil)

;;Non-nil means next SPC or whatever goes to next undeleted message.
(defvar mh-page-to-next-msg-flag nil)

;;; Internal variables local to a folder.

;; Name of current folder, a string.
(defvar mh-current-folder nil)

;; Buffer that displays message for this folder.
(defvar mh-show-buffer nil)

;; Full path of directory for this folder.
(defvar mh-folder-filename nil)

;;Number of msgs in buffer.
(defvar mh-msg-count nil)

;; If non-nil, show the message in a separate window.
(defvar mh-showing-mode nil)

(defvar mh-show-mode-map (make-sparse-keymap)
  "Keymap used by the show buffer.")

(defvar mh-show-folder-buffer nil
  "Keeps track of folder whose message is being displayed.")

(defvar mh-logo-cache nil)

(defun mh-logo-display ()
  "Modify mode line to display MH-E logo."
  (mh-do-in-gnu-emacs
   (add-text-properties
    0 2
    `(display ,(or mh-logo-cache
                   (setq mh-logo-cache
                         (mh-funcall-if-exists
                          find-image '((:type xpm :ascent center
                                              :file "mh-logo.xpm"))))))
    (car mode-line-buffer-identification)))
  (mh-do-in-xemacs
   (setq modeline-buffer-identification
         (list
          (if mh-modeline-glyph
              (cons modeline-buffer-id-left-extent mh-modeline-glyph)
            (cons modeline-buffer-id-left-extent "XEmacs%N:"))
          (cons modeline-buffer-id-right-extent " %17b")))))

;;; This holds a documentation string used by describe-mode.
(defun mh-showing-mode (&optional arg)
  "Change whether messages should be displayed.
With arg, display messages iff ARG is positive."
  (setq mh-showing-mode
        (if (null arg)
            (not mh-showing-mode)
          (> (prefix-numeric-value arg) 0))))

;; The sequences of this folder.  An alist of (seq . msgs).
(defvar mh-seq-list nil)

;; List of displayed messages to be removed from the Unseen sequence.
(defvar mh-seen-list nil)

;; If non-nil, show buffer contains message with all headers.
;; If nil, show buffer contains message processed normally.
;; Showing message with headers or normally.
(defvar mh-showing-with-headers nil)

;;; MH-E macros

(defmacro with-mh-folder-updating (save-modification-flag &rest body)
  "Format is (with-mh-folder-updating (SAVE-MODIFICATION-FLAG) &body BODY).
Execute BODY, which can modify the folder buffer without having to
worry about file locking or the read-only flag, and return its result.
If SAVE-MODIFICATION-FLAG is non-nil, the buffer's modification
flag is unchanged, otherwise it is cleared."
  (setq save-modification-flag (car save-modification-flag)) ; CL style
  `(prog1
       (let ((mh-folder-updating-mod-flag (buffer-modified-p))
             (buffer-read-only nil)
             (buffer-file-name nil))    ;don't let the buffer get locked
         (prog1
             (progn
               ,@body)
           (mh-set-folder-modified-p mh-folder-updating-mod-flag)))
     ,@(if (not save-modification-flag)
           '((mh-set-folder-modified-p nil)))))

(put 'with-mh-folder-updating 'lisp-indent-hook 'defun)

(defmacro mh-in-show-buffer (show-buffer &rest body)
  "Format is (mh-in-show-buffer (SHOW-BUFFER) &body BODY).
Display buffer SHOW-BUFFER in other window and execute BODY in it.
Stronger than `save-excursion', weaker than `save-window-excursion'."
  (setq show-buffer (car show-buffer))  ; CL style
  `(let ((mh-in-show-buffer-saved-window (selected-window)))
     (switch-to-buffer-other-window ,show-buffer)
     (if mh-bury-show-buffer-flag (bury-buffer (current-buffer)))
     (unwind-protect
         (progn
           ,@body)
       (select-window mh-in-show-buffer-saved-window))))

(put 'mh-in-show-buffer 'lisp-indent-hook 'defun)

(defmacro mh-do-at-event-location (event &rest body)
  "Switch to the location of EVENT and execute BODY.
After BODY has been executed return to original window. The modification flag
of the buffer in the event window is preserved."
  (let ((event-window (make-symbol "event-window"))
        (event-position (make-symbol "event-position"))
        (original-window (make-symbol "original-window"))
        (original-position (make-symbol "original-position"))
        (modified-flag (make-symbol "modified-flag")))
    `(save-excursion
       (let* ((,event-window
               (or (mh-funcall-if-exists posn-window (event-start ,event))
                   (mh-funcall-if-exists event-window ,event)))
              (,event-position
               (or (mh-funcall-if-exists posn-point (event-start ,event))
                   (mh-funcall-if-exists event-closest-point ,event)))
              (,original-window (selected-window))
              (,original-position (progn
                                   (set-buffer (window-buffer ,event-window))
                                   (set-marker (make-marker) (point))))
              (,modified-flag (buffer-modified-p))
              (buffer-read-only nil))
         (unwind-protect (progn
                           (select-window ,event-window)
                           (goto-char ,event-position)
                           ,@body)
           (set-buffer-modified-p ,modified-flag)
           (goto-char ,original-position)
           (set-marker ,original-position nil)
           (select-window ,original-window))))))

(put 'mh-do-at-event-location 'lisp-indent-hook 'defun)

(defmacro mh-make-seq (name msgs)
  "Create sequence NAME with the given MSGS."
  (list 'cons name msgs))

(defmacro mh-seq-name (sequence)
  "Extract sequence name from the given SEQUENCE."
  (list 'car sequence))

(defmacro mh-seq-msgs (sequence)
  "Extract messages from the given SEQUENCE."
  (list 'cdr sequence))

(defun mh-recenter (arg)
  "Like recenter but with three improvements:
- At the end of the buffer it tries to show fewer empty lines.
- operates only if the current buffer is in the selected window.
  (Commands like `save-some-buffers' can make this false.)
- nil ARG means recenter as if prefix argument had been given."
  (cond ((not (eq (get-buffer-window (current-buffer)) (selected-window)))
         nil)
        ((= (point-max) (save-excursion
                          (forward-line (- (/ (window-height) 2) 2))
                          (point)))
         (let ((lines-from-end 2))
           (save-excursion
             (while (> (point-max) (progn (forward-line) (point)))
               (incf lines-from-end)))
           (recenter (- lines-from-end))))
        ;; '(4) is the same as C-u prefix argument.
        (t (recenter (or arg '(4))))))

(defun mh-start-of-uncleaned-message ()
  "Position uninteresting headers off the top of the window."
  (let ((case-fold-search t))
    (re-search-forward
     "^To:\\|^Cc:\\|^From:\\|^Subject:\\|^Date:" nil t)
    (beginning-of-line)
    (mh-recenter 0)))

(defun mh-invalidate-show-buffer ()
  "Invalidate the show buffer so we must update it to use it."
  (if (get-buffer mh-show-buffer)
      (save-excursion
        (set-buffer mh-show-buffer)
        (mh-unvisit-file))))

(defun mh-unvisit-file ()
  "Separate current buffer from the message file it was visiting."
  (or (not (buffer-modified-p))
      (null buffer-file-name)           ;we've been here before
      (yes-or-no-p (format "Message %s modified; flush changes? "
                           (file-name-nondirectory buffer-file-name)))
      (error "Flushing changes not confirmed"))
  (clear-visited-file-modtime)
  (unlock-buffer)
  (setq buffer-file-name nil))


(defun mh-get-msg-num (error-if-no-message)
  "Return the message number of the displayed message.
If the argument ERROR-IF-NO-MESSAGE is non-nil, then complain if the cursor is
not pointing to a message."
  (save-excursion
    (beginning-of-line)
    (cond ((looking-at mh-scan-msg-number-regexp)
           (string-to-number (buffer-substring (match-beginning 1)
                                               (match-end 1))))
          (error-if-no-message
           (error "Cursor not pointing to message"))
          (t nil))))

(defun mh-folder-name-p (name)
  "Return non-nil if NAME is the name of a folder.
A name (a string or symbol) can be a folder name if it begins with \"+\"."
  (if (symbolp name)
      (eq (aref (symbol-name name) 0) ?+)
    (and (> (length name) 0)
         (eq (aref name 0) ?+))))


(defun mh-expand-file-name (filename &optional default)
  "Expand FILENAME like `expand-file-name', but also handle MH folder names.
Any filename that starts with '+' is treated as a folder name.
See `expand-file-name' for description of DEFAULT."
  (if (mh-folder-name-p filename)
      (expand-file-name (substring filename 1) mh-user-path)
    (expand-file-name filename default)))


(defun mh-msg-filename (msg &optional folder)
  "Return the file name of MSG in FOLDER (default current folder)."
  (expand-file-name (int-to-string msg)
                    (if folder
                        (mh-expand-file-name folder)
                      mh-folder-filename)))

;;; Infrastructure to generate show-buffer functions from folder functions
;;; XEmacs does not have deactivate-mark? What is the equivalent of
;;; transient-mark-mode for XEmacs? Should we be restoring the mark in the
;;; folder buffer after the operation has been carried out.
(defmacro mh-defun-show-buffer (function original-function
                                         &optional dont-return)
  "Define FUNCTION to run ORIGINAL-FUNCTION in folder buffer.
If the buffer we start in is still visible and DONT-RETURN is nil then switch
to it after that."
  `(defun ,function ()
     ,(format "Calls %s from the message's folder.\n%s\nSee `%s' for more info.\n"
              original-function
              (if dont-return ""
                "When function completes, returns to the show buffer if it is
still visible.\n")
              original-function)
     (interactive)
     (when (buffer-live-p (get-buffer mh-show-folder-buffer))
       (let ((config (current-window-configuration))
             (folder-buffer mh-show-folder-buffer)
             (normal-exit nil)
             ,@(if dont-return () '((cur-buffer-name (buffer-name)))))
         (pop-to-buffer mh-show-folder-buffer nil)
         (unless (equal (buffer-name
                         (window-buffer (frame-first-window (selected-frame))))
                        folder-buffer)
           (delete-other-windows))
         (mh-goto-cur-msg t)
         (mh-funcall-if-exists deactivate-mark)
         (unwind-protect
             (prog1 (call-interactively (function ,original-function))
               (setq normal-exit t))
           (mh-funcall-if-exists deactivate-mark)
           (when (eq major-mode 'mh-folder-mode)
             (mh-funcall-if-exists hl-line-highlight))
           (cond ((not normal-exit)
                  (set-window-configuration config))
                 ,(if dont-return
                      `(t (setq mh-previous-window-config config))
                    `((and (get-buffer cur-buffer-name)
                           (window-live-p (get-buffer-window
                                           (get-buffer cur-buffer-name))))
                      (pop-to-buffer (get-buffer cur-buffer-name) nil)))))))))

;;; Generate interactive functions for the show buffer from the corresponding
;;; folder functions.
(mh-defun-show-buffer mh-show-previous-undeleted-msg
                      mh-previous-undeleted-msg)
(mh-defun-show-buffer mh-show-next-undeleted-msg
                      mh-next-undeleted-msg)
(mh-defun-show-buffer mh-show-quit mh-quit)
(mh-defun-show-buffer mh-show-delete-msg mh-delete-msg)
(mh-defun-show-buffer mh-show-refile-msg mh-refile-msg)
(mh-defun-show-buffer mh-show-undo mh-undo)
(mh-defun-show-buffer mh-show-execute-commands mh-execute-commands)
(mh-defun-show-buffer mh-show-reply mh-reply t)
(mh-defun-show-buffer mh-show-redistribute mh-redistribute)
(mh-defun-show-buffer mh-show-forward mh-forward t)
(mh-defun-show-buffer mh-show-header-display mh-header-display)
(mh-defun-show-buffer mh-show-refile-or-write-again
                      mh-refile-or-write-again)
(mh-defun-show-buffer mh-show-show mh-show)
(mh-defun-show-buffer mh-show-write-message-to-file
                      mh-write-msg-to-file)
(mh-defun-show-buffer mh-show-extract-rejected-mail
                      mh-extract-rejected-mail t)
(mh-defun-show-buffer mh-show-delete-msg-no-motion
                      mh-delete-msg-no-motion)
(mh-defun-show-buffer mh-show-first-msg mh-first-msg)
(mh-defun-show-buffer mh-show-last-msg mh-last-msg)
(mh-defun-show-buffer mh-show-copy-msg mh-copy-msg)
(mh-defun-show-buffer mh-show-edit-again mh-edit-again t)
(mh-defun-show-buffer mh-show-goto-msg mh-goto-msg)
(mh-defun-show-buffer mh-show-inc-folder mh-inc-folder)
(mh-defun-show-buffer mh-show-delete-subject-or-thread
                      mh-delete-subject-or-thread)
(mh-defun-show-buffer mh-show-delete-subject mh-delete-subject)
(mh-defun-show-buffer mh-show-print-msg mh-print-msg)
(mh-defun-show-buffer mh-show-send mh-send t)
(mh-defun-show-buffer mh-show-toggle-showing mh-toggle-showing t)
(mh-defun-show-buffer mh-show-pipe-msg mh-pipe-msg t)
(mh-defun-show-buffer mh-show-sort-folder mh-sort-folder)
(mh-defun-show-buffer mh-show-visit-folder mh-visit-folder t)
(mh-defun-show-buffer mh-show-rescan-folder mh-rescan-folder)
(mh-defun-show-buffer mh-show-pack-folder mh-pack-folder)
(mh-defun-show-buffer mh-show-kill-folder mh-kill-folder t)
(mh-defun-show-buffer mh-show-list-folders mh-list-folders t)
(mh-defun-show-buffer mh-show-search-folder mh-search-folder t)
(mh-defun-show-buffer mh-show-undo-folder mh-undo-folder)
(mh-defun-show-buffer mh-show-delete-msg-from-seq
                      mh-delete-msg-from-seq)
(mh-defun-show-buffer mh-show-delete-seq mh-delete-seq)
(mh-defun-show-buffer mh-show-list-sequences mh-list-sequences)
(mh-defun-show-buffer mh-show-narrow-to-seq mh-narrow-to-seq)
(mh-defun-show-buffer mh-show-put-msg-in-seq mh-put-msg-in-seq)
(mh-defun-show-buffer mh-show-msg-is-in-seq mh-msg-is-in-seq)
(mh-defun-show-buffer mh-show-widen mh-widen)
(mh-defun-show-buffer mh-show-narrow-to-subject mh-narrow-to-subject)
(mh-defun-show-buffer mh-show-narrow-to-from mh-narrow-to-from)
(mh-defun-show-buffer mh-show-narrow-to-cc mh-narrow-to-cc)
(mh-defun-show-buffer mh-show-narrow-to-range mh-narrow-to-range)
(mh-defun-show-buffer mh-show-narrow-to-to mh-narrow-to-to)
(mh-defun-show-buffer mh-show-store-msg mh-store-msg)
(mh-defun-show-buffer mh-show-page-digest mh-page-digest)
(mh-defun-show-buffer mh-show-page-digest-backwards
                      mh-page-digest-backwards)
(mh-defun-show-buffer mh-show-burst-digest mh-burst-digest)
(mh-defun-show-buffer mh-show-page-msg mh-page-msg)
(mh-defun-show-buffer mh-show-previous-page mh-previous-page)
(mh-defun-show-buffer mh-show-modify mh-modify t)
(mh-defun-show-buffer mh-show-next-button mh-next-button)
(mh-defun-show-buffer mh-show-prev-button mh-prev-button)
(mh-defun-show-buffer mh-show-toggle-mime-part mh-folder-toggle-mime-part)
(mh-defun-show-buffer mh-show-save-mime-part mh-folder-save-mime-part)
(mh-defun-show-buffer mh-show-inline-mime-part mh-folder-inline-mime-part)
(mh-defun-show-buffer mh-show-toggle-threads mh-toggle-threads)
(mh-defun-show-buffer mh-show-thread-delete mh-thread-delete)
(mh-defun-show-buffer mh-show-thread-refile mh-thread-refile)
(mh-defun-show-buffer mh-show-update-sequences mh-update-sequences)
(mh-defun-show-buffer mh-show-next-unread-msg mh-next-unread-msg)
(mh-defun-show-buffer mh-show-previous-unread-msg mh-previous-unread-msg)
(mh-defun-show-buffer mh-show-thread-ancestor mh-thread-ancestor)
(mh-defun-show-buffer mh-show-thread-next-sibling mh-thread-next-sibling)
(mh-defun-show-buffer mh-show-thread-previous-sibling
                      mh-thread-previous-sibling)
(mh-defun-show-buffer mh-show-index-visit-folder mh-index-visit-folder t)
(mh-defun-show-buffer mh-show-toggle-tick mh-toggle-tick)
(mh-defun-show-buffer mh-show-narrow-to-tick mh-narrow-to-tick)
(mh-defun-show-buffer mh-show-junk-blacklist mh-junk-blacklist)
(mh-defun-show-buffer mh-show-junk-whitelist mh-junk-whitelist)
(mh-defun-show-buffer mh-show-index-new-messages mh-index-new-messages)
(mh-defun-show-buffer mh-show-index-ticked-messages mh-index-ticked-messages)
(mh-defun-show-buffer mh-show-index-sequenced-messages
                      mh-index-sequenced-messages)
(mh-defun-show-buffer mh-show-catchup mh-catchup)
(mh-defun-show-buffer mh-show-ps-print-toggle-mime mh-ps-print-toggle-mime)
(mh-defun-show-buffer mh-show-ps-print-toggle-color mh-ps-print-toggle-color)
(mh-defun-show-buffer mh-show-ps-print-toggle-faces mh-ps-print-toggle-faces)
(mh-defun-show-buffer mh-show-ps-print-msg-file mh-ps-print-msg-file)
(mh-defun-show-buffer mh-show-ps-print-msg mh-ps-print-msg)
(mh-defun-show-buffer mh-show-ps-print-msg-show mh-ps-print-msg-show)
(mh-defun-show-buffer mh-show-toggle-mime-buttons mh-toggle-mime-buttons)
(mh-defun-show-buffer mh-show-display-with-external-viewer
                      mh-display-with-external-viewer)

;;; Populate mh-show-mode-map
(gnus-define-keys mh-show-mode-map
  " "    mh-show-page-msg
  "!"    mh-show-refile-or-write-again
  "'"    mh-show-toggle-tick
  ","    mh-show-header-display
  "."    mh-show-show
  ">"    mh-show-write-message-to-file
  "?"    mh-help
  "E"    mh-show-extract-rejected-mail
  "M"    mh-show-modify
  "\177" mh-show-previous-page
  "\C-d" mh-show-delete-msg-no-motion
  "\t"   mh-show-next-button
  [backtab] mh-show-prev-button
  "\M-\t" mh-show-prev-button
  "\ed"  mh-show-redistribute
  "^"    mh-show-refile-msg
  "c"    mh-show-copy-msg
  "d"    mh-show-delete-msg
  "e"    mh-show-edit-again
  "f"    mh-show-forward
  "g"    mh-show-goto-msg
  "i"    mh-show-inc-folder
  "k"    mh-show-delete-subject-or-thread
  "m"    mh-show-send
  "n"    mh-show-next-undeleted-msg
  "\M-n" mh-show-next-unread-msg
  "o"    mh-show-refile-msg
  "p"    mh-show-previous-undeleted-msg
  "\M-p" mh-show-previous-unread-msg
  "q"    mh-show-quit
  "r"    mh-show-reply
  "s"    mh-show-send
  "t"    mh-show-toggle-showing
  "u"    mh-show-undo
  "x"    mh-show-execute-commands
  "v"    mh-show-index-visit-folder
  "|"    mh-show-pipe-msg)

(gnus-define-keys (mh-show-folder-map "F" mh-show-mode-map)
  "?"    mh-prefix-help
  "'"    mh-index-ticked-messages
  "S"    mh-show-sort-folder
  "c"    mh-show-catchup
  "f"    mh-show-visit-folder
  "i"    mh-index-search
  "k"    mh-show-kill-folder
  "l"    mh-show-list-folders
  "n"    mh-index-new-messages
  "o"    mh-show-visit-folder
  "q"    mh-show-index-sequenced-messages
  "r"    mh-show-rescan-folder
  "s"    mh-show-search-folder
  "t"    mh-show-toggle-threads
  "u"    mh-show-undo-folder
  "v"    mh-show-visit-folder)

(gnus-define-keys (mh-show-sequence-map "S" mh-show-mode-map)
  "'"    mh-show-narrow-to-tick
  "?"    mh-prefix-help
  "d"    mh-show-delete-msg-from-seq
  "k"    mh-show-delete-seq
  "l"    mh-show-list-sequences
  "n"    mh-show-narrow-to-seq
  "p"    mh-show-put-msg-in-seq
  "s"    mh-show-msg-is-in-seq
  "w"    mh-show-widen)

(define-key mh-show-mode-map "I" mh-inc-spool-map)

(gnus-define-keys (mh-show-junk-map "J" mh-show-mode-map)
  "?"    mh-prefix-help
  "b"    mh-show-junk-blacklist
  "w"    mh-show-junk-whitelist)

(gnus-define-keys (mh-show-ps-print-map "P" mh-show-mode-map)
  "?"	mh-prefix-help
  "A"	mh-show-ps-print-toggle-mime
  "C"	mh-show-ps-print-toggle-color
  "F"	mh-show-ps-print-toggle-faces
  "M"	mh-show-ps-print-toggle-mime
  "f"	mh-show-ps-print-msg-file
  "l"   mh-show-print-msg
  "p"	mh-show-ps-print-msg
  "s"	mh-show-ps-print-msg-show)

(gnus-define-keys (mh-show-thread-map "T" mh-show-mode-map)
  "?"    mh-prefix-help
  "u"    mh-show-thread-ancestor
  "p"    mh-show-thread-previous-sibling
  "n"    mh-show-thread-next-sibling
  "t"    mh-show-toggle-threads
  "d"    mh-show-thread-delete
  "o"    mh-show-thread-refile)

(gnus-define-keys (mh-show-limit-map "/" mh-show-mode-map)
  "'"    mh-show-narrow-to-tick
  "?"    mh-prefix-help
  "c"    mh-show-narrow-to-cc
  "f"    mh-show-narrow-to-from
  "r"    mh-show-narrow-to-range
  "s"    mh-show-narrow-to-subject
  "t"    mh-show-narrow-to-to
  "w"    mh-show-widen)

(gnus-define-keys (mh-show-extract-map "X" mh-show-mode-map)
  "?"    mh-prefix-help
  "s"    mh-show-store-msg
  "u"    mh-show-store-msg)

;; Untested...
(gnus-define-keys (mh-show-digest-map "D" mh-show-mode-map)
  "?"    mh-prefix-help
  " "    mh-show-page-digest
  "\177" mh-show-page-digest-backwards
  "b"    mh-show-burst-digest)

(gnus-define-keys (mh-show-mime-map "K" mh-show-mode-map)
  "?"           mh-prefix-help
  "a"           mh-mime-save-parts
  "e"           mh-show-display-with-external-viewer
  "v"           mh-show-toggle-mime-part
  "o"           mh-show-save-mime-part
  "i"           mh-show-inline-mime-part
  "t"           mh-show-toggle-mime-buttons
  "\t"          mh-show-next-button
  [backtab]     mh-show-prev-button
  "\M-\t"       mh-show-prev-button)

(easy-menu-define
  mh-show-sequence-menu mh-show-mode-map "Menu for MH-E folder-sequence."
  '("Sequence"
    ["Add Message to Sequence..."       mh-show-put-msg-in-seq t]
    ["List Sequences for Message"       mh-show-msg-is-in-seq t]
    ["Delete Message from Sequence..."  mh-show-delete-msg-from-seq t]
    ["List Sequences in Folder..."      mh-show-list-sequences t]
    ["Delete Sequence..."               mh-show-delete-seq t]
    ["Narrow to Sequence..."            mh-show-narrow-to-seq t]
    ["Widen from Sequence"              mh-show-widen t]
    "--"
    ["Narrow to Subject Sequence"       mh-show-narrow-to-subject t]
    ["Narrow to Tick Sequence"          mh-show-narrow-to-tick
     (save-excursion
       (set-buffer mh-show-folder-buffer)
       (and mh-tick-seq (mh-seq-msgs (mh-find-seq mh-tick-seq))))]
    ["Delete Rest of Same Subject"      mh-show-delete-subject t]
    ["Toggle Tick Mark"                 mh-show-toggle-tick t]
    "--"
    ["Push State Out to MH"             mh-show-update-sequences t]))

(easy-menu-define
  mh-show-message-menu mh-show-mode-map "Menu for MH-E folder-message."
  '("Message"
    ["Show Message"                     mh-show-show t]
    ["Show Message with Header"         mh-show-header-display t]
    ["Next Message"                     mh-show-next-undeleted-msg t]
    ["Previous Message"                 mh-show-previous-undeleted-msg t]
    ["Go to First Message"              mh-show-first-msg t]
    ["Go to Last Message"               mh-show-last-msg t]
    ["Go to Message by Number..."       mh-show-goto-msg t]
    ["Modify Message"                   mh-show-modify t]
    ["Delete Message"                   mh-show-delete-msg t]
    ["Refile Message"                   mh-show-refile-msg t]
    ["Undo Delete/Refile"               mh-show-undo t]
    ["Process Delete/Refile"            mh-show-execute-commands t]
    "--"
    ["Compose a New Message"            mh-send t]
    ["Reply to Message..."              mh-show-reply t]
    ["Forward Message..."               mh-show-forward t]
    ["Redistribute Message..."          mh-show-redistribute t]
    ["Edit Message Again"               mh-show-edit-again t]
    ["Re-edit a Bounced Message"        mh-show-extract-rejected-mail t]
    "--"
    ["Copy Message to Folder..."        mh-show-copy-msg t]
    ["Print Message"                    mh-show-print-msg t]
    ["Write Message to File..."         mh-show-write-msg-to-file t]
    ["Pipe Message to Command..."       mh-show-pipe-msg t]
    ["Unpack Uuencoded Message..."      mh-show-store-msg t]
    ["Burst Digest Message"             mh-show-burst-digest t]))

(easy-menu-define
  mh-show-folder-menu mh-show-mode-map  "Menu for MH-E folder."
  '("Folder"
    ["Incorporate New Mail"             mh-show-inc-folder t]
    ["Toggle Show/Folder"               mh-show-toggle-showing t]
    ["Execute Delete/Refile"            mh-show-execute-commands t]
    ["Rescan Folder"                    mh-show-rescan-folder t]
    ["Thread Folder"                    mh-show-toggle-threads t]
    ["Pack Folder"                      mh-show-pack-folder t]
    ["Sort Folder"                      mh-show-sort-folder t]
    "--"
    ["List Folders"                     mh-show-list-folders t]
    ["Visit a Folder..."                mh-show-visit-folder t]
    ["View New Messages"                mh-show-index-new-messages t]
    ["Search a Folder..."               mh-show-search-folder t]
    ["Indexed Search..."                mh-index-search t]
    "--"
    ["Quit MH-E"                        mh-quit t]))


;;; Ensure new buffers won't get this mode if default-major-mode is nil.
(put 'mh-show-mode 'mode-class 'special)

;; Avoid compiler warnings in XEmacs and Emacs 20
(eval-when-compile
  (defvar tool-bar-mode)
  (defvar tool-bar-map))

(define-derived-mode mh-show-mode text-mode "MH-Show"
  "Major mode for showing messages in MH-E.\\<mh-show-mode-map>
The value of `mh-show-mode-hook' is a list of functions to
be called, with no arguments, upon entry to this mode.
See also `mh-folder-mode'.

\\{mh-show-mode-map}"
  (set (make-local-variable 'mail-header-separator) mh-mail-header-separator)
  (setq paragraph-start (default-value 'paragraph-start))
  (mh-show-unquote-From)
  (mh-show-xface)
  (mh-show-addr)
  (setq buffer-invisibility-spec '((vanish . t) t))
  (set (make-local-variable 'line-move-ignore-invisible) t)
  (make-local-variable 'font-lock-defaults)
  ;;(set (make-local-variable 'font-lock-support-mode) nil)
  (cond
   ((equal mh-highlight-citation-p 'font-lock)
    (setq font-lock-defaults '(mh-show-font-lock-keywords-with-cite t)))
   ((equal mh-highlight-citation-p 'gnus)
    (setq font-lock-defaults '((mh-show-font-lock-keywords)
                               t nil nil nil
                               (font-lock-fontify-region-function
                                . mh-show-font-lock-fontify-region)))
    (mh-gnus-article-highlight-citation))
   (t
    (setq font-lock-defaults '(mh-show-font-lock-keywords t))))
  (if (and mh-xemacs-flag
           font-lock-auto-fontify)
      (turn-on-font-lock))
  (set (make-local-variable 'tool-bar-map) mh-show-tool-bar-map)
  (mh-funcall-if-exists mh-toolbar-init :show)
  (when mh-decode-mime-flag
    (mh-make-local-hook 'kill-buffer-hook)
    (add-hook 'kill-buffer-hook 'mh-mime-cleanup nil t))
  (easy-menu-add mh-show-sequence-menu)
  (easy-menu-add mh-show-message-menu)
  (easy-menu-add mh-show-folder-menu)
  (make-local-variable 'mh-show-folder-buffer)
  (buffer-disable-undo)
  (setq buffer-read-only t)
  (use-local-map mh-show-mode-map))

(defun mh-show-addr ()
  "Use `goto-address'."
  (when mh-show-use-goto-addr-flag
    (if (not (featurep 'goto-addr))
        (load "goto-addr" t t))
    (if (fboundp 'goto-address)
        (goto-address))))



;; X-Face and Face display
(defvar mh-show-xface-function
  (cond ((and mh-xemacs-flag (locate-library "x-face") (not (featurep 'xface)))
         (load "x-face" t t)
         #'mh-face-display-function)
        ((>= emacs-major-version 21)
         #'mh-face-display-function)
        (t #'ignore))
  "Determine at run time what function should be called to display X-Face.")

(defvar mh-uncompface-executable
  (and (fboundp 'executable-find) (executable-find "uncompface")))

(defun mh-face-to-png (data)
  "Convert base64 encoded DATA to png image."
  (with-temp-buffer
    (insert data)
    (ignore-errors (base64-decode-region (point-min) (point-max)))
    (buffer-string)))

(defun mh-uncompface (data)
  "Run DATA through `uncompface' to generate bitmap."
  (with-temp-buffer
    (insert data)
    (when (and mh-uncompface-executable
               (equal (call-process-region (point-min) (point-max)
                                           mh-uncompface-executable t '(t nil))
                      0))
      (mh-icontopbm)
      (buffer-string))))

(defun mh-icontopbm ()
  "Elisp substitute for `icontopbm'."
  (goto-char (point-min))
  (let ((end (point-max)))
    (while (re-search-forward "0x\\(..\\)\\(..\\)," nil t)
      (save-excursion
        (goto-char (point-max))
        (insert (string-to-number (match-string 1) 16))
        (insert (string-to-number (match-string 2) 16))))
    (delete-region (point-min) end)
    (goto-char (point-min))
    (insert "P4\n48 48\n")))

(mh-do-in-xemacs (defvar default-enable-multibyte-characters))

(defun mh-face-display-function ()
  "Display a Face, X-Face, or X-Image-URL header field.
If more than one of these are present, then the first one found in this order
is used."
  (save-restriction
    (goto-char (point-min))
    (re-search-forward "\n\n" (point-max) t)
    (narrow-to-region (point-min) (point))
    (let* ((case-fold-search t)
           (default-enable-multibyte-characters nil)
           (face (message-fetch-field "face" t))
           (x-face (message-fetch-field "x-face" t))
           (url (message-fetch-field "x-image-url" t))
           raw type)
      (cond (face (setq raw (mh-face-to-png face)
                        type 'png))
            (x-face (setq raw (mh-uncompface x-face)
                          type 'pbm))
            (url (setq type 'url))
            (t (multiple-value-setq (type raw) (mh-picon-get-image))))
      (when type
        (goto-char (point-min))
        (when (re-search-forward "^from:" (point-max) t)
          ;; GNU Emacs
          (mh-do-in-gnu-emacs
            (if (eq type 'url)
                (mh-x-image-url-display url)
              (mh-funcall-if-exists
               insert-image (create-image
                             raw type t
                             :foreground (face-foreground 'mh-show-xface)
                             :background (face-background 'mh-show-xface))
                            " ")))
          ;; XEmacs
          (mh-do-in-xemacs
            (cond
             ((eq type 'url)
              (mh-x-image-url-display url))
             ((eq type 'png)
              (when (featurep 'png)
                (set-extent-begin-glyph
                 (make-extent (point) (point))
                 (make-glyph (vector 'png ':data (mh-face-to-png face))))))
             ;; Try internal xface support if available...
             ((and (eq type 'pbm) (featurep 'xface))
              (set-glyph-face
               (set-extent-begin-glyph
                (make-extent (point) (point))
                (make-glyph (vector 'xface ':data (concat "X-Face: " x-face))))
               'mh-show-xface))
             ;; Otherwise try external support with x-face...
             ((and (eq type 'pbm)
                   (fboundp 'x-face-xmas-wl-display-x-face)
                   (fboundp 'executable-find) (executable-find "uncompface"))
              (mh-funcall-if-exists x-face-xmas-wl-display-x-face))
             ;; Picon display
             ((and raw (member type '(xpm xbm gif)))
              (when (featurep type)
                (set-extent-begin-glyph
                 (make-extent (point) (point))
                 (make-glyph (vector type ':data raw))))))
            (when raw (insert " "))))))))

(defun mh-show-xface ()
  "Display X-Face."
  (when (and window-system mh-show-use-xface-flag
             (or mh-decode-mime-flag mhl-formfile
                 mh-clean-message-header-flag))
    (funcall mh-show-xface-function)))



;; Picon display

;;; XXX: This should be customizable. As a side-effect of setting this
;;;   variable, arrange to reset mh-picon-existing-directory-list to 'unset.
(defvar mh-picon-directory-list
  '("~/.picons" "~/.picons/users" "~/.picons/usenix" "~/.picons/news"
    "~/.picons/domains" "~/.picons/misc"
    "/usr/share/picons/" "/usr/share/picons/users" "/usr/share/picons/usenix"
    "/usr/share/picons/news" "/usr/share/picons/domains"
    "/usr/share/picons/misc")
  "List of directories where picons reside.
The directories are searched for in the order they appear in the list.")

(defvar mh-picon-existing-directory-list 'unset
  "List of directories to search in.")

(defvar mh-picon-cache (make-hash-table :test #'equal))

(defvar mh-picon-image-types
  (loop for type in '(xpm xbm gif)
        when (or (mh-do-in-gnu-emacs
                   (ignore-errors
                     (mh-funcall-if-exists image-type-available-p type)))
                 (mh-do-in-xemacs (featurep type)))
        collect type))

(defun mh-picon-set-directory-list ()
  "Update `mh-picon-existing-directory-list' if needed."
  (when (eq mh-picon-existing-directory-list 'unset)
    (setq mh-picon-existing-directory-list
          (loop for x in mh-picon-directory-list
                when (file-directory-p x) collect x))))

(defun* mh-picon-get-image ()
  "Find the best possible match and return contents."
  (mh-picon-set-directory-list)
  (save-restriction
    (let* ((from-field (ignore-errors (car (message-tokenize-header
                                            (mh-get-header-field "from:")))))
           (from (car (ignore-errors
                        (mh-funcall-if-exists ietf-drums-parse-address
                                              from-field))))
           (host (and from
                      (string-match "\\([^+]*\\)\\(+.*\\)?@\\(.*\\)" from)
                      (downcase (match-string 3 from))))
           (user (and host (downcase (match-string 1 from))))
           (canonical-address (format "%s@%s" user host))
           (cached-value (gethash canonical-address mh-picon-cache))
           (host-list (and host (delete "" (split-string host "\\."))))
           (match nil))
      (cond (cached-value (return-from mh-picon-get-image cached-value))
            ((not host-list) (return-from mh-picon-get-image nil)))
      (setq match
            (block 'loop
              ;; u@h search
              (loop for dir in mh-picon-existing-directory-list
                    do (loop for type in mh-picon-image-types
                             ;; [path]user@host
                             for file1 = (format "%s/%s.%s"
                                                 dir canonical-address type)
                             when (file-exists-p file1)
                             do (return-from 'loop file1)
                             ;; [path]user
                             for file2 = (format "%s/%s.%s" dir user type)
                             when (file-exists-p file2)
                             do (return-from 'loop file2)
                             ;; [path]host
                             for file3 = (format "%s/%s.%s" dir host type)
                             when (file-exists-p file3)
                             do (return-from 'loop file3)))
              ;; facedb search
              ;; Search order for user@foo.net:
              ;;   [path]net/foo/user
              ;;   [path]net/foo/user/face
              ;;   [path]net/user
              ;;   [path]net/user/face
              ;;   [path]net/foo/unknown
              ;;   [path]net/foo/unknown/face
              ;;   [path]net/unknown
              ;;   [path]net/unknown/face
              (loop for u in (list user "unknown")
                    do (loop for dir in mh-picon-existing-directory-list
                             do (loop for x on host-list by #'cdr
                                      for y = (mh-picon-generate-path x u dir)
                                      do (loop for type in mh-picon-image-types
                                               for z1 = (format "%s.%s" y type)
                                               when (file-exists-p z1)
                                               do (return-from 'loop z1)
                                               for z2 = (format "%s/face.%s"
                                                                y type)
                                               when (file-exists-p z2)
                                               do (return-from 'loop z2)))))))
      (setf (gethash canonical-address mh-picon-cache)
            (mh-picon-file-contents match)))))

(defun mh-picon-file-contents (file)
  "Return details about FILE.
A list of consisting of a symbol for the type of the file and the file
contents as a string is returned. If FILE is nil, then both elements of the
list are nil."
  (if (stringp file)
      (with-temp-buffer
        (let ((type (and (string-match ".*\\.\\(...\\)$" file)
                         (intern (match-string 1 file)))))
          (insert-file-contents-literally file)
          (values type (buffer-string))))
    (values nil nil)))

(defun mh-picon-generate-path (host-list user directory)
  "Generate the image file path.
HOST-LIST is the parsed host address of the email address, USER the username
and DIRECTORY is the directory relative to which the path is generated."
  (loop with acc = ""
        for elem in host-list
        do (setq acc (format "%s/%s" elem acc))
        finally return (format "%s/%s%s" directory acc user)))



;; X-Image-URL display

(defvar mh-x-image-cache-directory nil
  "Directory where X-Image-URL images are cached.")
(defvar mh-x-image-scaling-function
  (cond ((executable-find "convert")
         'mh-x-image-scale-with-convert)
        ((and (executable-find "anytopnm") (executable-find "pnmscale")
              (executable-find "pnmtopng"))
         'mh-x-image-scale-with-pnm)
        (t 'ignore))
  "Function to use to scale image to proper size.")
(defvar mh-wget-executable nil)
(defvar mh-wget-choice
  (or (and (setq mh-wget-executable (executable-find "wget")) 'wget)
      (and (setq mh-wget-executable (executable-find "fetch")) 'fetch)
      (and (setq mh-wget-executable (executable-find "curl")) 'curl)))
(defvar mh-wget-option
  (cdr (assoc mh-wget-choice '((curl . "-o") (fetch . "-o") (wget . "-O")))))
(defvar mh-x-image-temp-file nil)
(defvar mh-x-image-url nil)
(defvar mh-x-image-marker nil)
(defvar mh-x-image-url-cache-file nil)

;; Functions to scale image to proper size
(defun mh-x-image-scale-with-pnm (input output)
  "Scale image in INPUT file and write to OUTPUT file using pnm tools."
  (let ((res (shell-command-to-string
              (format "anytopnm < %s | pnmscale -xysize 96 48 | pnmtopng > %s"
                      input output))))
    (unless (equal res "")
      (delete-file output))))

(defun mh-x-image-scale-with-convert (input output)
  "Scale image in INPUT file and write to OUTPUT file using ImageMagick."
  (call-process "convert" nil nil nil "-geometry" "96x48" input output))

(defun mh-x-image-url-cache-canonicalize (url)
  "Canonicalize URL.
Replace the ?/ character with a ?! character and append .png."
   (format "%s/%s.png" mh-x-image-cache-directory
           (with-temp-buffer
             (insert url)
             (mh-replace-string "/" "!")
             (buffer-string))))

(defun mh-x-image-set-download-state (file data)
  "Setup a symbolic link from FILE to DATA."
  (if data
      (make-symbolic-link (symbol-name data) file t)
    (delete-file file)))

(defun mh-x-image-get-download-state (file)
  "Check the state of FILE by following any symbolic links."
  (unless (file-exists-p mh-x-image-cache-directory)
    (call-process "mkdir" nil nil nil mh-x-image-cache-directory))
  (cond ((file-symlink-p file)
         (intern (file-name-nondirectory (file-chase-links file))))
        ((not (file-exists-p file)) nil)
        (t 'ok)))

(defun mh-x-image-url-fetch-image (url cache-file marker sentinel)
  "Fetch and display the image specified by URL.
After the image is fetched, it is stored in CACHE-FILE. It will be displayed
in a buffer and position specified by MARKER. The actual display is carried
out by the SENTINEL function."
  (if mh-wget-executable
      (let ((buffer (get-buffer-create (generate-new-buffer-name
                                        mh-temp-fetch-buffer)))
            (filename (or (mh-funcall-if-exists make-temp-file "mhe-fetch")
                          (expand-file-name (make-temp-name "~/mhe-fetch")))))
        (save-excursion
          (set-buffer buffer)
          (set (make-local-variable 'mh-x-image-url-cache-file) cache-file)
          (set (make-local-variable 'mh-x-image-marker) marker)
          (set (make-local-variable 'mh-x-image-temp-file) filename))
        (set-process-sentinel
         (start-process "*mh-x-image-url-fetch*" buffer
                        mh-wget-executable mh-wget-option filename url)
         sentinel))
    ;; Temporary failure
    (mh-x-image-set-download-state cache-file 'try-again)))

(defun mh-x-image-display (image marker)
  "Display IMAGE at MARKER."
  (save-excursion
    (set-buffer (marker-buffer marker))
    (let ((buffer-read-only nil)
          (default-enable-multibyte-characters nil)
          (buffer-modified-flag (buffer-modified-p)))
      (unwind-protect
          (when (and (file-readable-p image) (not (file-symlink-p image))
                     (eq marker mh-x-image-marker))
            (goto-char marker)
            (mh-do-in-gnu-emacs
              (mh-funcall-if-exists insert-image (create-image image 'png)))
            (mh-do-in-xemacs
              (when (featurep 'png)
                (set-extent-begin-glyph
                 (make-extent (point) (point))
                 (make-glyph
                  (vector 'png ':data (with-temp-buffer
                                        (insert-file-contents-literally image)
                                        (buffer-string))))))))
        (set-buffer-modified-p buffer-modified-flag)))))

(defun mh-x-image-scale-and-display (process change)
  "When the wget PROCESS terminates scale and display image.
The argument CHANGE is ignored."
  (when (eq (process-status process) 'exit)
    (let (marker temp-file cache-filename wget-buffer)
      (save-excursion
        (set-buffer (setq wget-buffer (process-buffer process)))
        (setq marker mh-x-image-marker
              cache-filename mh-x-image-url-cache-file
              temp-file mh-x-image-temp-file))
      (cond
       ;; Check if we have `convert'
       ((eq mh-x-image-scaling-function 'ignore)
        (message "The `convert' program is needed to display X-Image-URL")
        (mh-x-image-set-download-state cache-filename 'try-again))
       ;; Scale fetched image
       ((and (funcall mh-x-image-scaling-function temp-file cache-filename)
             nil))
       ;; Attempt to display image if we have it
       ((file-exists-p cache-filename)
        (mh-x-image-display cache-filename marker))
       ;; We didn't find the image. Should we try to display it the next time?
       (t (mh-x-image-set-download-state cache-filename 'try-again)))
      (ignore-errors
        (set-marker marker nil)
        (delete-process process)
        (kill-buffer wget-buffer)
        (delete-file temp-file)))))

(defun mh-x-image-url-sane-p (url)
  "Check if URL is something sensible."
  (let ((len (length url)))
    (cond ((< len 5) nil)
          ((not (equal (substring url 0 5) "http:")) nil)
          ((> len 100) nil)
          (t t))))

(defun mh-x-image-url-display (url)
  "Display image from location URL.
If the URL isn't present in the cache then it is fetched with wget."
  (let* ((cache-filename (mh-x-image-url-cache-canonicalize url))
         (state (mh-x-image-get-download-state cache-filename))
         (marker (set-marker (make-marker) (point))))
    (set (make-local-variable 'mh-x-image-marker) marker)
    (cond ((not (mh-x-image-url-sane-p url)))
          ((eq state 'ok)
           (mh-x-image-display cache-filename marker))
          ((or (not mh-wget-executable)
               (eq mh-x-image-scaling-function 'ignore)))
          ((eq state 'never))
          ((not mh-fetch-x-image-url)
           (set-marker marker nil))
          ((eq state 'try-again)
           (mh-x-image-set-download-state cache-filename nil)
           (mh-x-image-url-fetch-image url cache-filename marker
                                       'mh-x-image-scale-and-display))
          ((and (eq mh-fetch-x-image-url 'ask)
                (not (y-or-n-p (format "Fetch %s? " url))))
           (mh-x-image-set-download-state cache-filename 'never))
          ((eq state nil)
           (mh-x-image-url-fetch-image url cache-filename marker
                                       'mh-x-image-scale-and-display)))))



(defun mh-maybe-show (&optional msg)
  "Display message at cursor, but only if in show mode.
If optional arg MSG is non-nil, display that message instead."
  (if mh-showing-mode (mh-show msg)))

(defun mh-show (&optional message redisplay-flag)
  "Show message at cursor.
If optional argument MESSAGE is non-nil, display that message instead.
Force a two-window display with the folder window on top (size given by the
variable `mh-summary-height') and the show buffer below it.
If the message is already visible, display the start of the message.

If REDISPLAY-FLAG is non-nil, the default when called interactively, the
message is redisplayed even if the show buffer was already displaying the
correct message.

Display of the message is controlled by setting the variables
`mh-clean-message-header-flag' and `mhl-formfile'.  The default behavior is
to scroll uninteresting headers off the top of the window.
Type \"\\[mh-header-display]\" to see the message with all its headers."
  (interactive (list nil t))
  (when (or redisplay-flag
            (and mh-showing-with-headers
                 (or mhl-formfile mh-clean-message-header-flag)))
    (mh-invalidate-show-buffer))
  (mh-show-msg message))

(defun mh-show-mouse (event)
  "Move point to mouse EVENT and show message."
  (interactive "e")
  (mouse-set-point event)
  (mh-show))

(defun mh-summary-height ()
  "Return ideal value for the variable `mh-summary-height'.
The current frame height is taken into consideration."
  (or (and (fboundp 'frame-height)
           (> (frame-height) 24)
           (min 10 (/ (frame-height) 6)))
      4))

(defun mh-show-msg (msg)
  "Show MSG.
The value of `mh-show-hook' is a list of functions to be called, with no
arguments, after the message has been displayed."
  (if (not msg)
      (setq msg (mh-get-msg-num t)))
  (mh-showing-mode t)
  (setq mh-page-to-next-msg-flag nil)
  (let ((folder mh-current-folder)
        (folders (list mh-current-folder))
        (clean-message-header mh-clean-message-header-flag)
        (show-window (get-buffer-window mh-show-buffer))
        (display-mime-buttons-flag mh-display-buttons-for-inline-parts-flag))
    (if (not (eq (next-window (minibuffer-window)) (selected-window)))
        (delete-other-windows))         ; force ourself to the top window
    (mh-in-show-buffer (mh-show-buffer)
      (setq mh-display-buttons-for-inline-parts-flag display-mime-buttons-flag)
      (if (and show-window
               (equal (mh-msg-filename msg folder) buffer-file-name))
          (progn                        ;just back up to start
            (goto-char (point-min))
            (if (not clean-message-header)
                (mh-start-of-uncleaned-message)))
        (mh-display-msg msg folder)))
    (if (not (= (1+ (window-height)) (frame-height))) ;not horizontally split
        (shrink-window (- (window-height) (or mh-summary-height
                                              (mh-summary-height)))))
    (mh-recenter nil)
    ;; The following line is a nop which forces update of the scan line so
    ;; that font-lock will update it (if needed)...
    (mh-notate nil nil mh-cmd-note)
    (if (not (memq msg mh-seen-list))
        (setq mh-seen-list (cons msg mh-seen-list)))
    (when mh-update-sequences-after-mh-show-flag
      (mh-update-sequences)
      (when mh-index-data
        (setq folders
              (append (mh-index-delete-from-sequence mh-unseen-seq (list msg))
                      folders)))
      (when (mh-speed-flists-active-p)
        (apply #'mh-speed-flists t folders)))
    (run-hooks 'mh-show-hook)))

(defun mh-modify (&optional message)
  "Edit message at cursor.
If optional argument MESSAGE is non-nil, edit that message instead.
Force a two-window display with the folder window on top (size given by the
value of the variable `mh-summary-height') and the message editing buffer below
it.

The message is displayed in raw form."
  (interactive)
  (let* ((message (or message (mh-get-msg-num t)))
         (msg-filename (mh-msg-filename message))
         edit-buffer)
    (when (not (file-exists-p msg-filename))
      (error "Message %d does not exist" message))

    ;; Invalidate the show buffer if it is showing the same message that is
    ;; to be edited.
    (when (and (buffer-live-p (get-buffer mh-show-buffer))
               (equal (save-excursion (set-buffer mh-show-buffer)
                                      buffer-file-name)
                      msg-filename))
      (mh-invalidate-show-buffer))

    ;; Edit message
    (find-file msg-filename)
    (setq edit-buffer (current-buffer))

    ;; Set buffer properties
    (mh-letter-mode)
    (use-local-map text-mode-map)

    ;; Just show the edit buffer...
    (delete-other-windows)
    (switch-to-buffer edit-buffer)))

(defun mh-show-unquote-From ()
  "Decode >From at beginning of lines for `mh-show-mode'."
  (save-excursion
    (let ((modified (buffer-modified-p))
          (case-fold-search nil)
          (buffer-read-only nil))
      (goto-char (mh-mail-header-end))
      (while (re-search-forward "^>From" nil t)
        (replace-match "From"))
      (set-buffer-modified-p modified))))

(defun mh-msg-folder (folder-name)
  "Return the name of the buffer for FOLDER-NAME."
  folder-name)

(defun mh-display-msg (msg-num folder-name)
  "Display MSG-NUM of FOLDER-NAME.
Sets the current buffer to the show buffer."
  (let ((folder (mh-msg-folder folder-name)))
    (set-buffer folder)
    ;; When Gnus uses external displayers it has to keep handles longer. So
    ;; we will delete these handles when mh-quit is called on the folder. It
    ;; would be nicer if there are weak pointers in emacs lisp, then we could
    ;; get the garbage collector to do this for us.
    (unless (mh-buffer-data)
      (setf (mh-buffer-data) (mh-make-buffer-data)))
    ;; Bind variables in folder buffer in case they are local
    (let ((formfile mhl-formfile)
          (clean-message-header mh-clean-message-header-flag)
          (invisible-headers mh-invisible-header-fields-compiled)
          (visible-headers nil)
          (msg-filename (mh-msg-filename msg-num folder-name))
          (show-buffer mh-show-buffer)
          (mm-inline-media-tests mh-mm-inline-media-tests))
      (if (not (file-exists-p msg-filename))
          (error "Message %d does not exist" msg-num))
      (if (and (> mh-show-maximum-size 0)
               (> (elt (file-attributes msg-filename) 7)
                  mh-show-maximum-size)
               (not (y-or-n-p
                     (format
                      "Message %d (%d bytes) exceeds %d bytes. Display it? "
                      msg-num (elt (file-attributes msg-filename) 7)
                      mh-show-maximum-size))))
          (error "Message %d not displayed" msg-num))
      (set-buffer show-buffer)
      (cond ((not (equal msg-filename buffer-file-name))
             (mh-unvisit-file)
             (setq buffer-read-only nil)
             (erase-buffer)
             ;; Changing contents, so this hook needs to be reinitialized.
             ;; pgp.el uses this.
             (if (boundp 'write-contents-hooks) ;Emacs 19
                 (kill-local-variable 'write-contents-hooks))
             (if formfile
                 (mh-exec-lib-cmd-output "mhl" "-nobell" "-noclear"
                                         (if (stringp formfile)
                                             (list "-form" formfile))
                                         msg-filename)
               (insert-file-contents-literally msg-filename))
             ;; Cleanup old mime handles
             (mh-mime-cleanup)
             ;; Use mm to display buffer
             (when (and mh-decode-mime-flag (not formfile))
               (mh-add-missing-mime-version-header)
               (setf (mh-buffer-data) (mh-make-buffer-data))
               (mh-mime-display))
             (mh-show-mode)
             ;; Header cleanup
             (goto-char (point-min))
             (cond (clean-message-header
                    (mh-clean-msg-header (point-min)
                                         invisible-headers
                                         visible-headers)
                    (goto-char (point-min)))
                   (t
                    (mh-start-of-uncleaned-message)))
             (mh-decode-message-header)
             ;; the parts of visiting we want to do (no locking)
             (or (eq buffer-undo-list t) ;don't save undo info for prev msgs
                 (setq buffer-undo-list nil))
             (set-buffer-auto-saved)
             ;; the parts of set-visited-file-name we want to do (no locking)
             (setq buffer-file-name msg-filename)
             (setq buffer-backed-up nil)
             (auto-save-mode 1)
             (set-mark nil)
             (unwind-protect
                 (when (and mh-decode-mime-flag (not formfile))
                   (setq buffer-read-only nil)
                   (mh-display-smileys)
                   (mh-display-emphasis))
               (setq buffer-read-only t))
             (set-buffer-modified-p nil)
             (setq mh-show-folder-buffer folder)
             (setq mode-line-buffer-identification
                   (list (format mh-show-buffer-mode-line-buffer-id
                                 folder-name msg-num)))
             (mh-logo-display)
             (set-buffer folder)
             (setq mh-showing-with-headers nil))))))

(defun mh-clean-msg-header (start invisible-headers visible-headers)
  "Flush extraneous lines in message header.
Header is cleaned from START to the end of the message header.
INVISIBLE-HEADERS contains a regular expression specifying lines to delete
from the header. VISIBLE-HEADERS contains a regular expression specifying the
lines to display. INVISIBLE-HEADERS is ignored if VISIBLE-HEADERS is non-nil.

Note that MH-E no longer supports the `mh-visible-headers' variable, so
this function could be trimmed of this feature too."
  (let ((case-fold-search t)
        (buffer-read-only nil)
        (after-change-functions nil))   ;Work around emacs-20 font-lock bug
                                        ;causing an endless loop.
    (save-restriction
      (goto-char start)
      (if (search-forward "\n\n" nil 'move)
          (backward-char 1))
      (narrow-to-region start (point))
      (goto-char (point-min))
      (if visible-headers
          (while (< (point) (point-max))
            (cond ((looking-at visible-headers)
                   (forward-line 1)
                   (while (looking-at "[ \t]") (forward-line 1)))
                  (t
                   (mh-delete-line 1)
                   (while (looking-at "[ \t]")
                     (mh-delete-line 1)))))
        (while (re-search-forward invisible-headers nil t)
          (beginning-of-line)
          (mh-delete-line 1)
          (while (looking-at "[ \t]")
            (mh-delete-line 1)))))
    (let ((mh-compose-skipped-header-fields ()))
      (mh-letter-hide-all-skipped-fields))
    (unlock-buffer)))

(defun mh-delete-line (lines)
  "Delete the next LINES lines."
  (delete-region (point) (progn (forward-line lines) (point))))

(defun mh-notate (msg notation offset)
  "Mark MSG with the character NOTATION at position OFFSET.
Null MSG means the message at cursor.
If NOTATION is nil then no change in the buffer occurs."
  (save-excursion
    (if (or (null msg)
            (mh-goto-msg msg t t))
        (with-mh-folder-updating (t)
          (beginning-of-line)
          (forward-char offset)
          (let* ((change-stack-flag (and (equal offset (1+ mh-cmd-note))
                                         (not (eq notation mh-note-seq))))
                 (msg (and change-stack-flag (or msg (mh-get-msg-num nil))))
                 (stack (and msg (gethash msg mh-sequence-notation-history)))
                 (notation (or notation (char-after))))
            (if stack
                ;; The presence of the stack tells us that we don't need to
                ;; notate the message, since the notation would be replaced
                ;; by a sequence notation. So we will just put the notation
                ;; at the bottom of the stack. If the sequence is deleted,
                ;; the correct notation will be shown.
                (setf (gethash msg mh-sequence-notation-history)
                      (reverse (cons notation (cdr (reverse stack)))))
              ;; Since we don't have any sequence notations in the way, just
              ;; notate the scan line.
              (delete-char 1)
              (insert notation))
            (when change-stack-flag
              (mh-thread-update-scan-line-map msg notation offset)))))))

(defun mh-goto-msg (number &optional no-error-if-no-message dont-show)
  "Position the cursor at message NUMBER.
Optional non-nil second argument NO-ERROR-IF-NO-MESSAGE means return nil
instead of signaling an error if message does not exist; in this case, the
cursor is positioned near where the message would have been.
Non-nil third argument DONT-SHOW means not to show the message."
  (interactive "NGo to message: ")
  (setq number (prefix-numeric-value number))
  (let ((point (point))
        (return-value t))
    (goto-char (point-min))
    (unless (re-search-forward (format "^[ ]*%s[^0-9]+" number) nil t)
      (goto-char point)
      (unless no-error-if-no-message
        (error "No message %d" number))
      (setq return-value nil))
    (beginning-of-line)
    (or dont-show (not return-value) (mh-maybe-show number))
    return-value))

(defun mh-get-profile-field (field)
  "Find and return the value of FIELD in the current buffer.
Returns nil if the field is not in the buffer."
  (let ((case-fold-search t))
    (goto-char (point-min))
    (cond ((not (re-search-forward (format "^%s" field) nil t)) nil)
          ((looking-at "[\t ]*$") nil)
          (t
           (re-search-forward "[\t ]*\\([^\t \n].*\\)$" nil t)
           (let ((start (match-beginning 1)))
             (end-of-line)
             (buffer-substring start (point)))))))

(defvar mh-find-path-run nil
  "Non-nil if `mh-find-path' has been run already.")

(defun mh-find-path ()
  "Set variables from user's MH profile.
Set `mh-user-path', `mh-draft-folder', `mh-unseen-seq', `mh-previous-seq',
`mh-inbox' from user's MH profile.
The value of `mh-find-path-hook' is a list of functions to be called, with no
arguments, after these variable have been set."
  (mh-variants)
  (unless mh-find-path-run
    (setq mh-find-path-run t)
    (save-excursion
      ;; Be sure profile is fully expanded before switching buffers
      (let ((profile (expand-file-name (or (getenv "MH") "~/.mh_profile"))))
        (set-buffer (get-buffer-create mh-temp-buffer))
        (setq buffer-offer-save nil)      ;for people who set default to t
        (erase-buffer)
        (condition-case err
            (insert-file-contents profile)
          (file-error
           (mh-install profile err)))
        (setq mh-user-path (mh-get-profile-field "Path:"))
        (if (not mh-user-path)
            (setq mh-user-path "Mail"))
        (setq mh-user-path
              (file-name-as-directory
               (expand-file-name mh-user-path (expand-file-name "~"))))
        (unless mh-x-image-cache-directory
          (setq mh-x-image-cache-directory
                (expand-file-name ".mhe-x-image-cache" mh-user-path)))
        (setq mh-draft-folder (mh-get-profile-field "Draft-Folder:"))
        (if mh-draft-folder
            (progn
              (if (not (mh-folder-name-p mh-draft-folder))
                  (setq mh-draft-folder (format "+%s" mh-draft-folder)))
              (if (not (file-exists-p (mh-expand-file-name mh-draft-folder)))
                  (error
                   "Draft folder \"%s\" not found.  Create it and try again"
                         (mh-expand-file-name mh-draft-folder)))))
        (setq mh-inbox (mh-get-profile-field "Inbox:"))
        (cond ((not mh-inbox)
               (setq mh-inbox "+inbox"))
              ((not (mh-folder-name-p mh-inbox))
               (setq mh-inbox (format "+%s" mh-inbox))))
        (setq mh-unseen-seq (mh-get-profile-field "Unseen-Sequence:"))
        (if mh-unseen-seq
            (setq mh-unseen-seq (intern mh-unseen-seq))
          (setq mh-unseen-seq 'unseen))   ;old MH default?
        (setq mh-previous-seq (mh-get-profile-field "Previous-Sequence:"))
        (if mh-previous-seq
            (setq mh-previous-seq (intern mh-previous-seq)))
        (run-hooks 'mh-find-path-hook)
        (mh-collect-folder-names)))))

(defun mh-file-command-p (file)
  "Return t if file FILE is the name of a executable regular file."
  (and (file-regular-p file) (file-executable-p file)))

(defvar mh-no-install nil)              ;do not run install-mh

(defun mh-install (profile error-val)
  "Initialize the MH environment.
This is called if we fail to read the PROFILE file. ERROR-VAL is the error
that made this call necessary."
  (if (or (getenv "MH")
          (file-exists-p profile)
          mh-no-install)
      (signal (car error-val)
              (list (format "Cannot read MH profile \"%s\"" profile)
                    (car (cdr (cdr error-val))))))
  ;; The "install-mh" command will output a short note which
  ;; mh-exec-cmd will display to the user.
  ;; The MH 5 version of install-mh might try prompt the user
  ;; for information, which would fail here.
  (mh-exec-cmd (expand-file-name "install-mh" mh-lib-progs) "-auto")
  ;; now try again to read the profile file
  (erase-buffer)
  (condition-case err
      (insert-file-contents profile)
    (file-error
     (signal (car err)                  ;re-signal with more specific msg
             (list (format "Cannot read MH profile \"%s\"" profile)
                   (car (cdr (cdr err))))))))

(defun mh-set-folder-modified-p (flag)
  "Mark current folder as modified or unmodified according to FLAG."
  (set-buffer-modified-p flag))

(defun mh-find-seq (name)
  "Return sequence NAME."
  (assoc name mh-seq-list))

(defun mh-seq-to-msgs (seq)
  "Return a list of the messages in SEQ."
  (mh-seq-msgs (mh-find-seq seq)))

(defun mh-update-scan-format (fmt width)
  "Return a scan format with the (msg) width in the FMT replaced with WIDTH.

The message number width portion of the format is discovered using
`mh-scan-msg-format-regexp'. Its replacement is controlled with
`mh-scan-msg-format-string'."
  (or (and
       (string-match mh-scan-msg-format-regexp fmt)
       (let ((begin (match-beginning 1))
             (end (match-end 1)))
         (concat (substring fmt 0 begin)
                 (format mh-scan-msg-format-string width)
                 (substring fmt end))))
      fmt))

(defun mh-message-number-width (folder)
  "Return the widest message number in this FOLDER."
  (or mh-progs (mh-find-path))
  (let ((tmp-buffer (get-buffer-create mh-temp-buffer))
        (width 0))
    (save-excursion
      (set-buffer tmp-buffer)
      (erase-buffer)
      (apply 'call-process
             (expand-file-name mh-scan-prog mh-progs) nil '(t nil) nil
             (list folder "last" "-format" "%(msg)"))
      (goto-char (point-min))
      (if (re-search-forward mh-scan-msg-number-regexp nil 0 1)
          (setq width (length (buffer-substring
                               (match-beginning 1) (match-end 1))))))
    width))

(defun mh-add-msgs-to-seq (msgs seq &optional internal-flag dont-annotate-flag)
  "Add MSGS to SEQ.
Remove duplicates and keep sequence sorted. If optional INTERNAL-FLAG is
non-nil, do not mark the message in the scan listing or inform MH of the
addition.

If DONT-ANNOTATE-FLAG is non-nil then the annotations in the folder buffer are
not updated."
  (let ((entry (mh-find-seq seq))
        (internal-seq-flag (mh-internal-seq seq)))
    (if (and msgs (atom msgs)) (setq msgs (list msgs)))
    (if (null entry)
        (setq mh-seq-list
              (cons (mh-make-seq seq (mh-canonicalize-sequence msgs))
                    mh-seq-list))
      (if msgs (setcdr entry (mh-canonicalize-sequence
                              (append msgs (mh-seq-msgs entry))))))
    (unless internal-flag
      (mh-add-to-sequence seq msgs)
      (when (not dont-annotate-flag)
        (mh-iterate-on-range msg msgs
          (unless (memq msg (cdr entry))
            (mh-add-sequence-notation msg internal-seq-flag)))))))

(defun mh-canonicalize-sequence (msgs)
  "Sort MSGS in decreasing order and remove duplicates."
  (let* ((sorted-msgs (sort (copy-sequence msgs) '>))
         (head sorted-msgs))
    (while (cdr head)
      (if (= (car head) (cadr head))
          (setcdr head (cddr head))
        (setq head (cdr head))))
    sorted-msgs))

(defvar mh-sub-folders-cache (make-hash-table :test #'equal))
(defvar mh-current-folder-name nil)
(defvar mh-flists-partial-line "")
(defvar mh-flists-process nil)

;; Initialize mh-sub-folders-cache...
(defun mh-collect-folder-names ()
  "Collect folder names by running `flists'."
  (unless mh-flists-process
    (setq mh-flists-process
          (mh-exec-cmd-daemon "folders" 'mh-collect-folder-names-filter
                              "-recurse" "-fast"))))

(defun mh-collect-folder-names-filter (process output)
  "Read folder names.
PROCESS is the flists process that was run to collect folder names and the
function is called when OUTPUT is available."
  (let ((position 0)
	(prevailing-match-data (match-data))
	line-end folder)
    (unwind-protect
	(while (setq line-end (string-match "\n" output position))
	  (setq folder (format "+%s%s"
                               mh-flists-partial-line
                               (substring output position line-end)))
	  (setq mh-flists-partial-line "")
          (unless (equal (aref folder 1) ?.)
            (mh-populate-sub-folders-cache folder))
	  (setq position (1+ line-end)))
      (set-match-data prevailing-match-data))
    (setq mh-flists-partial-line (substring output position))))

(defun mh-populate-sub-folders-cache (folder)
  "Tell `mh-sub-folders-cache' about FOLDER."
  (let* ((last-slash (mh-search-from-end ?/ folder))
         (child1 (substring folder (1+ (or last-slash 0))))
         (parent (and last-slash (substring folder 0 last-slash)))
         (parent-slash (and parent (mh-search-from-end ?/ parent)))
         (child2 (and parent (substring parent (1+ (or parent-slash 0)))))
         (grand-parent (and parent-slash (substring parent 0 parent-slash)))
         (cache-entry (gethash parent mh-sub-folders-cache)))
    (unless (loop for x in cache-entry when (equal (car x) child1) return t
                  finally return nil)
      (push (list child1) cache-entry)
      (setf (gethash parent mh-sub-folders-cache)
            (sort cache-entry (lambda (x y) (string< (car x) (car y)))))
      (when parent
        (loop for x in (gethash grand-parent mh-sub-folders-cache)
              when (equal (car x) child2)
              do (progn (setf (cdr x) t) (return)))))))

(defun mh-normalize-folder-name (folder &optional empty-string-okay
                                        dont-remove-trailing-slash)
  "Normalizes FOLDER name.
Makes sure that two '/' characters never occur next to each other. Also all
occurrences of \"..\" and \".\" are suitably processed. So \"+inbox/../news\"
will be normalized to \"+news\".

If optional argument EMPTY-STRING-OKAY is nil then a '+' is added at the
front if FOLDER lacks one. If non-nil and FOLDER is the empty string then
nothing is added.

If optional argument DONT-REMOVE-TRAILING-SLASH is non-nil then a trailing '/'
if present is retained (if present), otherwise it is removed."
  (when (stringp folder)
    ;; Replace two or more consecutive '/' characters with a single '/'
    (while (string-match "//" folder)
      (setq folder (replace-match "/" nil t folder)))
    (let* ((length (length folder))
           (trailing-slash-present (and (> length 0)
                                        (equal (aref folder (1- length)) ?/)))
           (leading-slash-present (and (> length 0)
                                       (equal (aref folder 0) ?/))))
      (when (and (> length 0) (equal (aref folder 0) ?@)
                 (stringp mh-current-folder-name))
        (setq folder (format "%s/%s/" mh-current-folder-name
                             (substring folder 1))))
      ;; XXX: Purge empty strings from the list that split-string returns. In
      ;;  XEmacs, (split-string "+foo/" "/") returns ("+foo" "") while in GNU
      ;;  Emacs it returns ("+foo"). In the code it is assumed that the
      ;; components list has no empty strings.
      (let ((components (delete "" (split-string folder "/")))
            (result ()))
        ;; Remove .. and . from the pathname.
        (dolist (component components)
          (cond ((and (equal component "..") result)
                 (pop result))
                ((equal component ".."))
                ((equal component "."))
                (t (push component result))))
        (setq folder "")
        (dolist (component result)
          (setq folder (concat component "/" folder)))
        ;; Remove trailing '/' if needed.
        (unless (and trailing-slash-present dont-remove-trailing-slash)
          (when (not (equal folder ""))
            (setq folder (substring folder 0 (1- (length folder))))))
        (when leading-slash-present
          (setq folder (concat "/" folder)))))
    (cond ((and empty-string-okay (equal folder "")))
          ((equal folder "") (setq folder "+"))
          ((not (equal (aref folder 0) ?+)) (setq folder (concat "+" folder)))))
  folder)

(defun mh-sub-folders (folder &optional add-trailing-slash-flag)
  "Find the subfolders of FOLDER.
The function avoids running folders unnecessarily by caching the results of
the actual folders call.

If optional argument ADD-TRAILING-SLASH-FLAG is non-nil then a slash is added
to each of the sub-folder names that may have nested folders within them."
  (let* ((folder (mh-normalize-folder-name folder))
         (match (gethash folder mh-sub-folders-cache 'no-result))
         (sub-folders (cond ((eq match 'no-result)
                             (setf (gethash folder mh-sub-folders-cache)
                                   (mh-sub-folders-actual folder)))
                            (t match))))
    (if add-trailing-slash-flag
        (mapcar #'(lambda (x)
                    (if (cdr x) (cons (concat (car x) "/") (cdr x)) x))
                sub-folders)
      sub-folders)))

(defun mh-sub-folders-actual (folder)
  "Execute the command folders to return the sub-folders of FOLDER.
Filters out the folder names that start with \".\" so that directories that
aren't usually mail folders are hidden."
  (let ((arg-list `(,(expand-file-name "folders" mh-progs)
                    nil (t nil) nil "-noheader" "-norecurse" "-nototal"
                    ,@(if (stringp folder) (list folder) ())))
        (results ())
        (current-folder (concat
                         (with-temp-buffer
                           (call-process (expand-file-name "folder" mh-progs)
                                         nil '(t nil) nil "-fast")
                           (buffer-substring (point-min) (1- (point-max))))
                         "+")))
    (with-temp-buffer
      (apply #'call-process arg-list)
      (goto-char (point-min))
      (while (not (and (eolp) (bolp)))
        (goto-char (line-end-position))
        (let ((start-pos (line-beginning-position))
              (has-pos (search-backward " has " (line-beginning-position) t)))
          (when (integerp has-pos)
            (while (equal (char-after has-pos) ? )
              (decf has-pos))
            (incf has-pos)
            (while (equal (char-after start-pos) ? )
              (incf start-pos))
            (let* ((name (buffer-substring start-pos has-pos))
                   (first-char (aref name 0))
                   (last-char (aref name (1- (length name)))))
              (unless (member first-char '(?. ?# ?,))
                (when (and (equal last-char ?+) (equal name current-folder))
                  (setq name (substring name 0 (1- (length name)))))
                (push
                 (cons name
                       (search-forward "(others)" (line-end-position) t))
                 results))))
          (forward-line 1))))
    (setq results (nreverse results))
    (when (stringp folder)
      (setq results (cdr results))
      (let ((folder-name-len (length (format "%s/" (substring folder 1)))))
        (setq results (mapcar (lambda (f)
                                (cons (substring (car f) folder-name-len)
                                      (cdr f)))
                              results))))
    results))

(defun mh-remove-from-sub-folders-cache (folder)
  "Remove FOLDER and its parent from `mh-sub-folders-cache'.
FOLDER should be unconditionally removed from the cache. Also the last ancestor
of FOLDER present in the cache must be removed as well.

To see why this is needed assume we have a folder +foo which has a single
sub-folder qux. Now we create the folder +foo/bar/baz. Here we will need to
invalidate the cached sub-folders of +foo, otherwise completion on +foo won't
tell us about the option +foo/bar!"
  (remhash folder mh-sub-folders-cache)
  (block ancestor-found
    (let ((parent folder)
          (one-ancestor-found nil)
          last-slash)
      (while (setq last-slash (mh-search-from-end ?/ parent))
        (setq parent (substring parent 0 last-slash))
        (unless (eq (gethash parent  mh-sub-folders-cache 'none) 'none)
          (remhash parent mh-sub-folders-cache)
          (if one-ancestor-found
              (return-from ancestor-found)
            (setq one-ancestor-found t))))
      (remhash nil mh-sub-folders-cache))))

(defvar mh-folder-hist nil)
(defvar mh-speed-folder-map)
(defvar mh-speed-flists-cache)

(defvar mh-allow-root-folder-flag nil
  "Non-nil means \"+\" is an acceptable folder name.
This variable is used to communicate with `mh-folder-completion-function'. That
function can have exactly three arguments so we bind this variable to t or nil.

This variable should never be set.")

(defvar mh-folder-completion-map (copy-keymap minibuffer-local-completion-map))
(define-key mh-folder-completion-map " " 'minibuffer-complete)

(defvar mh-speed-flists-inhibit-flag nil)

(defun mh-speed-flists-active-p ()
  "Check if speedbar is running with message counts enabled."
  (and (featurep 'mh-speed)
       (not mh-speed-flists-inhibit-flag)
       (> (hash-table-count mh-speed-flists-cache) 0)))

(defun mh-folder-completion-function (name predicate flag)
  "Programmable completion for folder names.
NAME is the partial folder name that has been input. PREDICATE if non-nil is a
function that is used to filter the possible choices and FLAG determines
whether the completion is over."
  (let* ((orig-name name)
         (name (mh-normalize-folder-name name nil t))
         (last-slash (mh-search-from-end ?/ name))
         (last-complete (if last-slash (substring name 0 last-slash) nil))
         (remainder (cond (last-complete (substring name (1+ last-slash)))
                          ((and (> (length name) 0) (equal (aref name 0) ?+))
                           (substring name 1))
                          (t ""))))
    (cond ((eq flag nil)
           (let ((try-res (try-completion
                           name
                           (mapcar (lambda (x)
                                     (cons (if (not last-complete)
                                               (concat "+" (car x))
                                             (concat last-complete "/" (car x)))
                                           (cdr x)))
                                   (mh-sub-folders last-complete t))
                           predicate)))
             (cond ((eq try-res nil) nil)
                   ((and (eq try-res t) (equal name orig-name)) t)
                   ((eq try-res t) name)
                   (t try-res))))
          ((eq flag t)
           (all-completions
            remainder (mh-sub-folders last-complete t) predicate))
          ((eq flag 'lambda)
           (let ((path (concat mh-user-path
                               (substring (mh-normalize-folder-name name) 1))))
             (cond (mh-allow-root-folder-flag (file-exists-p path))
                   ((equal path mh-user-path) nil)
                   (t (file-exists-p path))))))))

(defun mh-folder-completing-read (prompt default allow-root-folder-flag)
  "Read folder name with PROMPT and default result DEFAULT.
If ALLOW-ROOT-FOLDER-FLAG is non-nil then \"+\" is allowed to be a folder name
corresponding to `mh-user-path'."
  (mh-normalize-folder-name
   (let ((minibuffer-completing-file-name t)
         (completion-root-regexp "^[+/]")
         (minibuffer-local-completion-map mh-folder-completion-map)
         (mh-allow-root-folder-flag allow-root-folder-flag))
     (completing-read prompt 'mh-folder-completion-function nil nil nil
                      'mh-folder-hist default))
   t))

(defun mh-prompt-for-folder (prompt default can-create
                             &optional default-string allow-root-folder-flag)
  "Prompt for a folder name with PROMPT.
Returns the folder's name as a string. DEFAULT is used if the folder exists
and the user types return. If the CAN-CREATE flag is t, then a folder is
created if it doesn't already exist. If optional argument DEFAULT-STRING is
non-nil, use it in the prompt instead of DEFAULT. If ALLOW-ROOT-FOLDER-FLAG is
non-nil then the function will accept the folder +, which means all folders
when used in searching."
  (if (null default)
      (setq default ""))
  (let* ((default-string (cond (default-string (format " (default %s)" default-string))
                               ((equal "" default) "")
                               (t (format " (default %s)" default))))
         (prompt (format "%s folder%s: " prompt default-string))
         (mh-current-folder-name mh-current-folder)
         read-name folder-name)
    (while (and (setq read-name (mh-folder-completing-read
                                 prompt default allow-root-folder-flag))
                (equal read-name "")
                (equal default "")))
    (cond ((or (equal read-name "")
               (and (equal read-name "+") (not allow-root-folder-flag)))
           (setq read-name default))
          ((not (mh-folder-name-p read-name))
           (setq read-name (format "+%s" read-name))))
    (if (or (not read-name) (equal "" read-name))
        (error "No folder specified"))
    (setq folder-name read-name)
    (cond ((and (> (length folder-name) 0)
                (eq (aref folder-name (1- (length folder-name))) ?/))
           (setq folder-name (substring folder-name 0 -1))))
    (let* ((last-slash (mh-search-from-end ?/ folder-name))
           (parent (and last-slash (substring folder-name 0 last-slash)))
           (child (if last-slash
                      (substring folder-name (1+ last-slash))
                    (substring folder-name 1))))
      (unless (member child
                      (mapcar #'car (gethash parent mh-sub-folders-cache)))
        (mh-remove-from-sub-folders-cache folder-name)))
    (let ((new-file-flag
           (not (file-exists-p (mh-expand-file-name folder-name)))))
      (cond ((and new-file-flag
                  (y-or-n-p
                   (format "Folder %s does not exist.  Create it? "
                           folder-name)))
             (message "Creating %s" folder-name)
             (mh-exec-cmd-error nil "folder" folder-name)
             (mh-remove-from-sub-folders-cache folder-name)
             (when (boundp 'mh-speed-folder-map)
               (mh-speed-add-folder folder-name))
             (message "Creating %s...done" folder-name))
            (new-file-flag
             (error "Folder %s is not created" folder-name))
            ((not (file-directory-p (mh-expand-file-name folder-name)))
             (error "\"%s\" is not a directory"
                    (mh-expand-file-name folder-name)))))
    folder-name))

(defun mh-truncate-log-buffer ()
  "If `mh-log-buffer' is too big then truncate it.
If the number of lines in `mh-log-buffer' exceeds `mh-log-buffer-lines' then
keep only the last `mh-log-buffer-lines'. As a side effect the point is set to
the end of the log buffer.

The function returns the size of the final size of the log buffer."
  (with-current-buffer (get-buffer-create mh-log-buffer)
    (goto-char (point-max))
    (save-excursion
      (when (equal (forward-line (- mh-log-buffer-lines)) 0)
        (delete-region (point-min) (point))))
    (unless (or (bobp)
                (save-excursion
                  (and (equal (forward-line -1) 0) (equal (char-after) ?))))
      (insert "\n\n"))
    (buffer-size)))

;;; Issue commands to MH.

(defun mh-exec-cmd (command &rest args)
  "Execute mh-command COMMAND with ARGS.
The side effects are what is desired.
Any output is assumed to be an error and is shown to the user.
The output is not read or parsed by MH-E."
  (save-excursion
    (set-buffer (get-buffer-create mh-log-buffer))
    (let* ((initial-size (mh-truncate-log-buffer))
           (start (point))
           (args (mh-list-to-string args)))
      (apply 'call-process (expand-file-name command mh-progs) nil t nil args)
      (when (> (buffer-size) initial-size)
        (save-excursion
          (goto-char start)
          (insert "Errors when executing: " command)
          (loop for arg in args do (insert " " arg))
          (insert "\n"))
        (save-window-excursion
          (switch-to-buffer-other-window mh-log-buffer)
          (sit-for 5))))))

(defun mh-exec-cmd-error (env command &rest args)
  "In environment ENV, execute mh-command COMMAND with ARGS.
ENV is nil or a string of space-separated \"var=value\" elements.
Signals an error if process does not complete successfully."
  (save-excursion
    (set-buffer (get-buffer-create mh-temp-buffer))
    (erase-buffer)
    (let ((process-environment process-environment))
      ;; XXX: We should purge the list that split-string returns of empty
      ;;  strings. This can happen in XEmacs if leading or trailing spaces
      ;;  are present.
      (dolist (elem (if (stringp env) (split-string env " ") ()))
        (push elem process-environment))
      (mh-handle-process-error
       command (apply #'call-process (expand-file-name command mh-progs)
                      nil t nil (mh-list-to-string args))))))

(defun mh-exec-cmd-daemon (command filter &rest args)
  "Execute MH command COMMAND in the background.

If FILTER is non-nil then it is used to process the output otherwise the
default filter `mh-process-daemon' is used. See `set-process-filter' for more
details of FILTER.

ARGS are passed to COMMAND as command line arguments."
  (save-excursion
    (set-buffer (get-buffer-create mh-log-buffer))
    (mh-truncate-log-buffer))
  (let* ((process-connection-type nil)
         (process (apply 'start-process
                         command nil
                         (expand-file-name command mh-progs)
                         (mh-list-to-string args))))
    (set-process-filter process (or filter 'mh-process-daemon))
    process))

(defun mh-exec-cmd-env-daemon (env command filter &rest args)
  "In ennvironment ENV, execute mh-command COMMAND in the background.

ENV is nil or a string of space-separated \"var=value\" elements.
Signals an error if process does not complete successfully.

If FILTER is non-nil then it is used to process the output otherwise the
default filter `mh-process-daemon' is used. See `set-process-filter' for more
details of FILTER.

ARGS are passed to COMMAND as command line arguments."
  (let ((process-environment process-environment))
    (dolist (elem (if (stringp env) (split-string env " ") ()))
      (push elem process-environment))
    (apply #'mh-exec-cmd-daemon command filter args)))

(defun mh-process-daemon (process output)
  "PROCESS daemon that puts OUTPUT into a temporary buffer.
Any output from the process is displayed in an asynchronous pop-up window."
  (set-buffer (get-buffer-create mh-log-buffer))
  (insert-before-markers output)
  (display-buffer mh-log-buffer))

(defun mh-exec-cmd-quiet (raise-error command &rest args)
  "Signal RAISE-ERROR if COMMAND with ARGS fails.
Execute MH command COMMAND with ARGS.  ARGS is a list of strings.
Return at start of mh-temp buffer, where output can be parsed and used.
Returns value of `call-process', which is 0 for success, unless RAISE-ERROR is
non-nil, in which case an error is signaled if `call-process' returns non-0."
  (set-buffer (get-buffer-create mh-temp-buffer))
  (erase-buffer)
  (let ((value
         (apply 'call-process
                (expand-file-name command mh-progs) nil t nil
                args)))
    (goto-char (point-min))
    (if raise-error
        (mh-handle-process-error command value)
      value)))

(defun mh-profile-component (component)
  "Return COMPONENT value from mhparam, or nil if unset."
  (save-excursion
    (mh-exec-cmd-quiet nil "mhparam" "-components" component)
    (mh-get-profile-field (concat component ":"))))

(defun mh-exchange-point-and-mark-preserving-active-mark ()
  "Put the mark where point is now, and point where the mark is now.
This command works even when the mark is not active, and preserves whether the
mark is active or not."
  (interactive nil)
  (let ((is-active (and (boundp 'mark-active) mark-active)))
    (let ((omark (mark t)))
      (if (null omark)
          (error "No mark set in this buffer"))
      (set-mark (point))
      (goto-char omark)
      (if (boundp 'mark-active)
          (setq mark-active is-active))
      nil)))

(defun mh-exec-cmd-output (command display &rest args)
  "Execute MH command COMMAND with DISPLAY flag and ARGS.
Put the output into buffer after point.  Set mark after inserted text.
Output is expected to be shown to user, not parsed by MH-E."
  (push-mark (point) t)
  (apply 'call-process
         (expand-file-name command mh-progs) nil t display
         (mh-list-to-string args))

  ;; The following is used instead of 'exchange-point-and-mark because the
  ;; latter activates the current region (between point and mark), which
  ;; turns on highlighting.  So prior to this bug fix, doing "inc" would
  ;; highlight a region containing the new messages, which is undesirable.
  ;; The bug wasn't seen in emacs21 but still occurred in XEmacs21.4.
  (mh-exchange-point-and-mark-preserving-active-mark))

(defun mh-exec-lib-cmd-output (command &rest args)
  "Execute MH library command COMMAND with ARGS.
Put the output into buffer after point.  Set mark after inserted text."
  (apply 'mh-exec-cmd-output (expand-file-name command mh-lib-progs) nil args))

(defun mh-handle-process-error (command status)
  "Raise error if COMMAND returned non-zero STATUS, otherwise return STATUS."
  (if (equal status 0)
      status
    (goto-char (point-min))
    (insert (if (integerp status)
                (format "%s: exit code %d\n" command status)
              (format "%s: %s\n" command status)))
    (save-excursion
      (let ((error-message (buffer-substring (point-min) (point-max))))
        (set-buffer (get-buffer-create mh-log-buffer))
        (mh-truncate-log-buffer)
        (insert error-message)))
    (error "%s failed, check %s buffer for error message"
           command mh-log-buffer)))

(defun mh-list-to-string (l)
  "Flatten the list L and make every element of the new list into a string."
  (nreverse (mh-list-to-string-1 l)))

(defun mh-list-to-string-1 (l)
  "Flatten the list L and make every element of the new list into a string."
  (let ((new-list nil))
    (while l
      (cond ((null (car l)))
            ((symbolp (car l))
             (setq new-list (cons (symbol-name (car l)) new-list)))
            ((numberp (car l))
             (setq new-list (cons (int-to-string (car l)) new-list)))
            ((equal (car l) ""))
            ((stringp (car l)) (setq new-list (cons (car l) new-list)))
            ((listp (car l))
             (setq new-list (nconc (mh-list-to-string-1 (car l))
                                   new-list)))
            (t (error "Bad element in mh-list-to-string: %s" (car l))))
      (setq l (cdr l)))
    new-list))

(defun mh-replace-string (old new)
  "Replace all occurrences of OLD with NEW in the current buffer."
  (goto-char (point-min))
  (let ((case-fold-search t))
    (while (search-forward old nil t)
      (replace-match new t t))))

(defun mh-replace-in-string (regexp newtext string)
  "Replace REGEXP with NEWTEXT everywhere in STRING and return result.
NEWTEXT is taken literally---no \\DIGIT escapes will be recognized.

The function body was copied from `dired-replace-in-string' in dired.el.
Emacs21 has `replace-regexp-in-string' while XEmacs has `replace-in-string'.
Neither is present in Emacs20. The file gnus-util.el in Gnus 5.10.1 and above
has `gnus-replace-in-string'. We should use that when we decide to not support
older versions of Gnus."
  (let ((result "") (start 0) mb me)
    (while (string-match regexp string start)
      (setq mb (match-beginning 0)
            me (match-end 0)
            result (concat result (substring string start mb) newtext)
            start me))
    (concat result (substring string start))))

(provide 'mh-utils)

;;; Local Variables:
;;; indent-tabs-mode: nil
;;; sentence-end-double-space: nil
;;; End:

;;; arch-tag: 1af39fdf-f66f-4b06-9b48-18a7656c8e36
;;; mh-utils.el ends here
