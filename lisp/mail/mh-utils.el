;;; mh-utils.el --- mh-e code needed for both sending and reading

;; Copyright (C) 1993, 1995, 1997, 2000, 2001, 2002 Free Software Foundation, Inc.

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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Internal support for mh-e package.

;;; Change Log:

;; $Id: mh-utils.el,v 1.79 2002/04/07 19:20:56 wohler Exp $

;;; Code:

(load "executable" t t)                 ; Non-fatal dependency on
					; executable-find

;;; Autoload mh-seq

(autoload 'mh-add-to-sequence "mh-seq")
(autoload 'mh-notate-seq "mh-seq")
(autoload 'mh-read-seq-default "mh-seq")
(autoload 'mh-map-to-seq-msgs "mh-seq")

;;; Other Autoloads

(autoload 'mail-header-end "sendmail")

;;; Set for local environment:
;;; mh-progs and mh-lib used to be set in paths.el, which tried to
;;; figure out at build time which of several possible directories MH
;;; was installed into.  But if you installed MH after building Emacs,
;;; this would almost certainly be wrong, so now we do it at run time.

(defvar mh-progs nil
  "Directory containing MH commands, such as inc, repl, and rmm.")

(defvar mh-lib nil
  "Directory containing the MH library.
This directory contains, among other things,
the components file.")

(defvar mh-lib-progs nil
  "Directory containing MH helper programs.
This directory contains, among other things,
the mhl program.")

(defvar mh-nmh-p nil
  "Non-nil if nmh is installed on this system instead of MH.")

;;;###autoload
(put 'mh-progs 'risky-local-variable t)
;;;###autoload
(put 'mh-lib 'risky-local-variable t)
;;;###autoload
(put 'mh-lib-progs 'risky-local-variable t)
;;;###autoload
(put 'mh-nmh-p 'risky-local-variable t)

;;; User preferences:

(defgroup mh-buffer nil
  "Layout of MH-E buffers"
  :prefix "mh-"
  :group 'mh)


(defcustom mh-auto-folder-collect t
  "*Whether to start collecting MH folder names immediately in the background.
Non-nil means start a background process collecting the names of all
folders as soon as mh-e is loaded."
  :type 'boolean
  :group 'mh)

(defcustom mh-recursive-folders nil
  "*If non-nil, then commands which operate on folders do so recursively."
  :type 'boolean
  :group 'mh)

(defcustom mh-clean-message-header t
  "*Non-nil means clean headers of messages that are displayed or inserted.
The variables `mh-visible-headers' and `mh-invisible-headers' control what
is removed."
  :type 'boolean
  :group 'mh-buffer)

(defcustom mh-visible-headers nil
  "*If non-nil, contains a regexp specifying the headers to keep when cleaning.
Only used if `mh-clean-message-header' is non-nil.  Setting this variable
overrides `mh-invisible-headers'."
  :type '(choice (const nil) regexp)
  :group 'mh-buffer)

(defvar mh-invisible-headers
  (concat
   "^"
   (let ((max-specpdl-size 1000))	;workaround for insufficient default
     (regexp-opt
      '( ;; RFC 822
	"Received: " "Message-Id: " "Return-Path: "
	;; RFC 2045
	"Mime-Version" "Content-"
	;; sendmail
	"X-Authentication-Warning: " "X-MIME-Autoconverted: " "From "
	"Status: "
	;; X400
	"X400-" "P1-Message-Id: " "Original-Encoded-Information-Types: "
	"P1-Recipient: " "P1-Content-Type: " "Ua-Content-Id: "
	;; MH
	"Resent" "Prev-Resent" "Forwarded: " "Replied: " "Delivery-Date: "
	"In-Reply-To: " "Remailed-" "Via: " "Mail-from: "
	;; gnus
	"X-Gnus-Mail-Source: "
	;; MS Outlook
	"X-Priority: " "X-Msmail-" "X-MimeOLE: " "X-Apparently-From: "
	"Importance: " "Sensitivity: " "X-MS-TNEF-Correlator: "
	;; Juno
	"X-Juno-"
	;; Hotmail
	"X-OriginalArrivalTime: " "X-Originating-IP: "
	;; Netscape/Mozilla
	"X-Accept-Language: " "X-Mozilla-Status: "
	;; NTMail
	"X-Info: " "X-VSMLoop: "
	;; News
	"NNTP-" "X-News: "
	;; Mailman mailing list manager
	"List-" "X-Beenthere: " "X-Mailman-Version: "
	;; Egroups/yahoogroups mailing list manager
	"X-eGroups-" "X-Apparently-To: " "Mailing-List: " "Delivered-To: "
	;; SourceForge mailing list manager
	"X-Original-Date: "
	;; Unknown mailing list managers
	"X-Mailing-List: " "X-Loop: "
	"List-Subscribe: " "List-Unsubscribe: "
	"X-List-Subscribe: " "X-List-Unsubscribe: "
	"X-Listserver: " "List-" "X-List-Host: "
	;; Sieve filtering
	"X-Sieve: "
	;; Worldtalk gateways
	"X-Wss-Id: "
	;; User added
	"X-Face: " "X-Qotd-"
	;; Miscellaneous
	"X-Sender: " "X-Ack: " "Errors-To: " "Precedence: " "X-Message-Id"
	"X-From-Line" "X-Cron-Env: " "Delivery: " "X-Delivered"
	"X-Received: " "X-Vms-To: " "Xref: " "X-Request-" "X-UIDL: "
	"X-Orcl-Content-Type: " "X-Server-Uuid: " "X-Envelope-Sender: "
	"X-Envelope-To: " "Encoding: " "Old-Return-Path: " "Path: "
	"References: " "Lines: " "Autoforwarded: " "Bestservhost: "
	"X-pgp: " "X-Accept-Language: " "Priority: " "User-Agent: "
	"X-MIMETrack: " "X-Abuse-Info: " "X-Complaints-To: "
	"X-No-Archive: " "X-Original-Complaints-To: "
	"X-Original-Trace: " "X-Received-Date: " "X-Server-Date: "
	"X-Trace: " "X-UserInfo1: " "X-submission-address: ")
      t)))
   "*Regexp matching lines in a message header that are not to be shown.
If `mh-visible-headers' is non-nil, it is used instead to specify what
to keep.")

;;; Additional header fields that might someday be added:
;;; "Sender: " "Reply-to: "

(defcustom mh-bury-show-buffer t
  "*Non-nil means that the displayed show buffer for a folder is buried."
  :type 'boolean
  :group 'mh-buffer)

(defcustom mh-summary-height (or (and (fboundp 'frame-height)
                                      (> (frame-height) 24)
                                      (min 10 (/ (frame-height) 6)))
                                 4)
  "*Number of lines in MH-Folder window (including the mode line)."
  :type 'integer
  :group 'mh-buffer)

;; Use goto-addr if it was already loaded (which probably sets this
;; variable to t), or if this variable is otherwise set to t.
(defcustom mh-show-use-goto-addr (and (boundp 'goto-address-highlight-p)
                                      goto-address-highlight-p)
  "*Non-nil means URLs and e-mail addresses are highlighted using goto-addr while in mh-show-mode."
  :type 'boolean
  :group 'mh-buffer)

(defvar mh-scan-msg-number-regexp "^ *\\([0-9]+\\)"
  "Regexp to find the number of a message in a scan line.
The message's number must be surrounded with \\( \\)")

(defvar mh-scan-msg-search-regexp "^[^0-9]*%d[^0-9]"
  "Format string containing a regexp matching the scan listing for a message.
The desired message's number will be an argument to format.")

(defcustom mhl-formfile nil
  "*Name of format file to be used by mhl to show and print messages.
A value of T means use the default format file.
Nil means don't use mhl to format messages when showing; mhl is still used,
with the default format file, to format messages when printing them.
The format used should specify a non-zero value for overflowoffset so
the message continues to conform to RFC 822 and mh-e can parse the headers."
  :type '(choice (const nil) (const t) string)
  :group 'mh)
(put 'mhl-formfile 'info-file "mh-e")

(defvar mh-decode-quoted-printable-have-mimedecode
  (not (null (and (fboundp 'executable-find)(executable-find "mimedecode"))))
  "Whether the mimedecode command is installed on the system.
This sets the default value of variable `mh-decode-quoted-printable' to
determine whether quoted-printable MIME parts are decode when viewed in
`mh-show'.  The source code for mimedecode can be obtained from
http://www.freesoft.org/CIE/FAQ/mimedeco.c")

(defcustom mh-decode-quoted-printable
  mh-decode-quoted-printable-have-mimedecode
  "Whether to decode quoted-printable MIME parts in `mh-show'.
This can only be done if the 'mimedecode' command is available in the
executable path on the system (the mh-decode-quoted-printable-have-mimedecode
variable is set if the command was found).  That program is used as a helper
program to achieve this.  The source code for mimedecode can usually be
obtained from http://www.freesoft.org/CIE/FAQ/mimedeco.c"
  :type 'boolean
  :group 'mh-buffer)

(defcustom mh-update-sequences-after-mh-show t
  "Whether to call `mh-update-sequence' in `mh-show-mode'.
If set, `mh-update-sequence' is run every time a message is shown, telling
MH or nmh that this is your current message.  It's useful, for example, to
display MIME content using \"M-! mhshow RET\""
  :type 'boolean
  :group 'mh-buffer)

(defcustom mh-highlight-citation-p 'gnus
  "How to highlight citations in show buffers.
The gnus method uses a different color for each indentation."
  :type '(choice (const :tag "Use gnus" gnus)
                 (const :tag "Use font-lock" font-lock)
                 (const :tag "Don't fontify" nil))
  :group 'mh-buffer)

(defvar mh-default-folder-for-message-function nil
  "Function to select a default folder for refiling or Fcc.
If set to a function, that function is called with no arguments by
`\\[mh-refile-msg]' and `\\[mh-to-fcc]' to get a default when
prompting the user for a folder.  The function is called from within a
`save-excursion', with point at the start of the message.  It should
return the folder to offer as the refile or Fcc folder, as a string
with a leading `+' sign.  It can also return an empty string to use no
default, or NIL to calculate the default the usual way.
NOTE: This variable is not an ordinary hook;
It may not be a list of functions.")

(defvar mh-find-path-hook nil
  "Invoked by `mh-find-path' while reading the user's MH profile.")

(defvar mh-folder-list-change-hook nil
  "Invoked whenever the cached folder list `mh-folder-list' is changed.")

(defvar mh-show-buffer-mode-line-buffer-id "{show-%s} %d"
  "Format string to produce `mode-line-buffer-identification' for show buffers.
First argument is folder name.  Second is message number.")

(defvar mh-cmd-note 4
  "Offset to insert notation.")

(defvar mh-note-seq "%"
  "String whose first character is used to notate messages in a sequence.")

(defvar mh-mail-header-separator "--------"
  "*Line used by MH to separate headers from text in messages being composed.
This variable should not be used directly in programs. Programs should use
`mail-header-separator' instead. `mail-header-separator' is initialized to
`mh-mail-header-separator' in `mh-letter-mode'; in other contexts, you may
have to perform this initialization yourself.

Do not make this a regexp as it may be the argument to `insert' and it is
passed through `regexp-quote' before being used by functions like
`re-search-forward'.")

(defun mh-in-header-p ()
  ;; Return non-nil if the point is in the header of a draft message.
  (< (point) (mail-header-end)))

(defun mh-header-field-end ()
  ;; Move to the end of the current header field.
  ;; Handles RFC 822 continuation lines.
  (forward-line 1)
  (while (looking-at "^[ \t]")
    (forward-line 1))
  (backward-char 1))		;to end of previous line

(defun mh-letter-header-font-lock (limit)
  "Return the entire mail header to font-lock.
Argument LIMIT limits search."
  (if (= (point) limit)
      nil
    (let* ((mail-header-end (save-match-data (mail-header-end)))
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
    (let* ((mail-header-end (mail-header-end))
           (lesser-limit (if (< mail-header-end limit) mail-header-end limit))
           (case-fold-search t))
      (when (and (< (point) mail-header-end)   ;Only within header
                 (re-search-forward (format "^%s" field) lesser-limit t))
        (let ((match-one-b (match-beginning 0))
              (match-one-e (match-end 0)))
          (mh-header-field-end)
          (if (> (point) limit)           ;Don't search for end beyond limit
              (goto-char limit))
          (set-match-data (list match-one-b match-one-e
                                (1+ match-one-e) (point)))
          t)))))

(defun mh-header-to-font-lock (limit)
  (mh-header-field-font-lock "To:" limit))

(defun mh-header-cc-font-lock (limit)
  (mh-header-field-font-lock "cc:" limit))

(defun mh-header-subject-font-lock (limit)
  (mh-header-field-font-lock "Subject:" limit))

(defvar mh-show-to-face 'mh-show-to-face
  "Face for highlighting the To: header field.")
(if (boundp 'facemenu-unlisted-faces)
    (add-to-list 'facemenu-unlisted-faces "^mh-show"))
(defface mh-show-to-face
  '((((class grayscale) (background light))
     (:foreground "DimGray" :underline t))
    (((class grayscale) (background dark))
     (:foreground "LightGray" :underline t))
    (((class color) (background light)) (:foreground "SaddleBrown"))
    (((class color) (background dark))  (:foreground "burlywood"))
    (t (:underline t)))
  "Face for highlighting the To: header field."
  :group 'mh-buffer)

(defvar mh-show-from-face 'mh-show-from-face
  "Face for highlighting the From: header field.")
(defface mh-show-from-face
  '((((class color) (background light))
     (:foreground "red3"))
    (((class color) (background dark))
     (:foreground "cyan"))
    (t
     (:bold t)))
  "Face for highlighting the From: header field."
  :group 'mh-buffer)

(defvar mh-folder-subject-face 'mh-folder-subject-face
  "Face for highlighting subject text in MH-Folder buffers.")
(if (boundp 'facemenu-unlisted-faces)
    (add-to-list 'facemenu-unlisted-faces "^mh-folder"))
(defface mh-folder-subject-face
  '((((class color) (background light))
     (:foreground "blue4"))
    (((class color) (background dark))
     (:foreground "yellow"))
    (t
     (:bold t)))
  "Face for highlighting subject text in MH-Folder buffers."
  :group 'mh)
(defvar mh-show-subject-face 'mh-show-subject-face
  "Face for highlighting the Subject header field.")
(copy-face 'mh-folder-subject-face 'mh-show-subject-face)

(eval-after-load "font-lock"
  '(progn
     (defvar mh-show-cc-face 'mh-show-cc-face
       "Face for highlighting cc header fields.")
     (copy-face 'font-lock-variable-name-face 'mh-show-cc-face)
     (defvar mh-show-date-face 'mh-show-date-face
       "Face for highlighting the Date header field.")
     (copy-face 'font-lock-type-face 'mh-show-date-face)
     (defvar mh-show-header-face 'mh-show-header-face
       "Face used to deemphasize unspecified header fields.")
     (copy-face 'font-lock-string-face 'mh-show-header-face)
     
     (defvar mh-show-font-lock-keywords
       '(("^\\(From:\\|Sender:\\)\\(.*\\)"
          (1 'default) (2 mh-show-from-face))
         (mh-header-to-font-lock
          (0 'default) (1 mh-show-to-face))
         (mh-header-cc-font-lock
	  (0 'default) (1 mh-show-cc-face))
         ("^\\(Reply-To:\\|Return-Path:\\)\\(.*\\)$"
          (1 'default) (2 mh-show-from-face))
         (mh-header-subject-font-lock
          (0 'default) (1 mh-show-subject-face))
         ("^\\(Apparently-To:\\|Newsgroups:\\)\\(.*\\)"
          (1 'default) (2 mh-show-cc-face))
         ("^\\(In-reply-to\\|Date\\):\\(.*\\)$"
          (1 'default) (2 mh-show-date-face))
         (mh-letter-header-font-lock (0 mh-show-header-face append t)))
       "Additional expressions to highlight in MH-show mode.")
     
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
     ))

(defun mh-gnus-article-highlight-citation ()
  "Highlight cited text in current buffer using gnus."
  (interactive)
  (require 'gnus-cite)
  (let ((modified (buffer-modified-p))
        (gnus-article-buffer (buffer-name))
        (gnus-cite-face-list
         '(gnus-cite-face-2 gnus-cite-face-3 gnus-cite-face-4 gnus-cite-face-5
           gnus-cite-face-6 gnus-cite-face-7 gnus-cite-face-8 gnus-cite-face-9
           gnus-cite-face-10 gnus-cite-face-11 gnus-cite-face-1)))
    (gnus-article-highlight-citation t)
    (set-buffer-modified-p modified)))

;;; Internal bookkeeping variables:

;; The value of `mh-folder-list-change-hook' is called whenever
;; mh-folder-list variable is set.
;; List of folder names for completion.
(defvar mh-folder-list nil)

;; Cached value of the `Path:' component in the user's MH profile.
;; User's mail folder directory.
(defvar mh-user-path nil)

;; An mh-draft-folder of NIL means do not use a draft folder.
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

;; Name of mh-e scratch buffer.
(defconst mh-temp-buffer " *mh-temp*")

;; Name of the mh-e folder list buffer.
(defconst mh-temp-folders-buffer "*Folders*")

;; Name of the mh-e sequences list buffer.
(defconst mh-temp-sequences-buffer "*Sequences*")

;; Window configuration before mh-e command.
(defvar mh-previous-window-config nil)

;;Non-nil means next SPC or whatever goes to next undeleted message.
(defvar mh-page-to-next-msg-p nil)

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


;;; mh-e macros

(defmacro with-mh-folder-updating (save-modification-flag-p &rest body)
  ;; Format is (with-mh-folder-updating (SAVE-MODIFICATION-FLAG-P) &body BODY).
  ;; Execute BODY, which can modify the folder buffer without having to
  ;; worry about file locking or the read-only flag, and return its result.
  ;; If SAVE-MODIFICATION-FLAG-P is non-nil, the buffer's modification
  ;; flag is unchanged, otherwise it is cleared.
  (setq save-modification-flag-p (car save-modification-flag-p)) ; CL style
  `(prog1
       (let ((mh-folder-updating-mod-flag (buffer-modified-p))
	     (buffer-read-only nil)
	     (buffer-file-name nil))	;don't let the buffer get locked
	 (prog1
	     (progn
	       ,@body)
	   (mh-set-folder-modified-p mh-folder-updating-mod-flag)))
     ,@(if (not save-modification-flag-p)
	   '((mh-set-folder-modified-p nil)))))

(put 'with-mh-folder-updating 'lisp-indent-hook 1)

(defmacro mh-in-show-buffer (show-buffer &rest body)
  ;; Format is (mh-in-show-buffer (SHOW-BUFFER) &body BODY).
  ;; Display buffer SHOW-BUFFER in other window and execute BODY in it.
  ;; Stronger than save-excursion, weaker than save-window-excursion.
  (setq show-buffer (car show-buffer))	; CL style
  `(let ((mh-in-show-buffer-saved-window (selected-window)))
     (switch-to-buffer-other-window ,show-buffer)
     (if mh-bury-show-buffer (bury-buffer (current-buffer)))
     (unwind-protect
	 (progn
           ,@body)
       (select-window mh-in-show-buffer-saved-window))))

(put 'mh-in-show-buffer 'lisp-indent-hook 1)

(defmacro mh-make-seq (name msgs) (list 'cons name msgs))

(defmacro mh-seq-name (pair) (list 'car pair))

(defmacro mh-seq-msgs (pair) (list 'cdr pair))


;;; Ensure new buffers won't get this mode if default-major-mode is nil.
(put 'mh-show-mode 'mode-class 'special)

(define-derived-mode mh-show-mode text-mode "MH-Show"
  "Major mode for showing messages in mh-e.
The value of `mh-show-mode-hook' is called when a new message is displayed."
  (set (make-local-variable 'mail-header-separator) mh-mail-header-separator)
  (mh-show-unquote-From)
  (when mh-show-use-goto-addr
    (if (not (featurep 'goto-addr))
        (load "goto-addr" t t))
    (if (fboundp 'goto-address)
        (goto-address)))
  (make-local-variable 'font-lock-defaults)
  (set (make-local-variable 'font-lock-support-mode) nil)
  (cond
   ((equal mh-highlight-citation-p 'font-lock)
    (setq font-lock-defaults '(mh-show-font-lock-keywords-with-cite t)))
   ((equal mh-highlight-citation-p 'gnus)
    (setq font-lock-defaults '(mh-show-font-lock-keywords t))
    (mh-gnus-article-highlight-citation))
   (t
    (setq font-lock-defaults '(mh-show-font-lock-keywords t)))))

(defun mh-maybe-show (&optional msg)
  ;; If in showing mode, then display the message pointed to by the cursor.
  (if mh-showing-mode (mh-show msg)))

(defun mh-show (&optional message)
  "Show MESSAGE (default: message at cursor).
Force a two-window display with the folder window on top (size
`mh-summary-height') and the show buffer below it.
If the message is already visible, display the start of the message.

Display of the message is controlled by setting the variables
`mh-clean-message-header' and `mhl-formfile'.  The default behavior is
to scroll uninteresting headers off the top of the window.
Type \"\\[mh-header-display]\" to see the message with all its headers."
  (interactive)
  (and mh-showing-with-headers
       (or mhl-formfile mh-clean-message-header)
       (mh-invalidate-show-buffer))
  (mh-show-msg message))

(defun mh-show-mouse (EVENT)
  "Move point to mouse EVENT and show message."
  (interactive "e")
  (mouse-set-point EVENT)
  (mh-show))

(defun mh-show-msg (msg)
  (if (not msg)
      (setq msg (mh-get-msg-num t)))
  (mh-showing-mode t)
  (setq mh-page-to-next-msg-p nil)
  (let ((folder mh-current-folder)
	(clean-message-header mh-clean-message-header)
	(show-window (get-buffer-window mh-show-buffer)))
    (if (not (eq (next-window (minibuffer-window)) (selected-window)))
	(delete-other-windows))		; force ourself to the top window
    (mh-in-show-buffer (mh-show-buffer)
      (if (and show-window
	       (equal (mh-msg-filename msg folder) buffer-file-name))
	  (progn			;just back up to start
	    (goto-char (point-min))
	    (if (not clean-message-header)
		(mh-start-of-uncleaned-message)))
	(mh-display-msg msg folder))))
  (if (not (= (1+ (window-height)) (frame-height))) ;not horizontally split
      (shrink-window (- (window-height) mh-summary-height)))
  (mh-recenter nil)
  (if (not (memq msg mh-seen-list)) (setq mh-seen-list (cons msg mh-seen-list)))
  (when mh-update-sequences-after-mh-show
    (mh-update-sequences))
  (run-hooks 'mh-show-hook))


(defun mh-decode-quoted-printable ()
  ;; Run mimedecode commmand on current buffer, replacing it contents.
  (let ((case-fold-search t))
    (goto-char (point-min))
    (when (and (re-search-forward
                "^content-transfer-encoding:[ \t]*quoted-printable"
                nil t)
               (search-forward "\n\n" nil t))
      (message "Converting quoted-printable characters...")
      (let ((modified (buffer-modified-p))
            (command "mimedecode"))
        (shell-command-on-region (point-min) (point-max) command t t)
        (if (fboundp 'deactivate-mark)
            (deactivate-mark))
        (set-buffer-modified-p modified))
      (message "Converting quoted-printable characters... done."))))


(defun mh-show-unquote-From ()
  ;; Decode >From at beginning of lines for mh-show-mode
  (save-excursion
    (let ((modified (buffer-modified-p))
          (case-fold-search nil))
      (goto-char (mail-header-end))
      (while (re-search-forward "^>From" nil t)
        (replace-match "From"))
      (set-buffer-modified-p modified))))

(defun mh-display-msg (msg-num folder)
  ;; Display message NUMBER of FOLDER.
  ;; Sets the current buffer to the show buffer.
  (set-buffer folder)
  ;; Bind variables in folder buffer in case they are local
  (let ((formfile mhl-formfile)
	(clean-message-header mh-clean-message-header)
	(invisible-headers mh-invisible-headers)
	(visible-headers mh-visible-headers)
	(msg-filename (mh-msg-filename msg-num))
	(show-buffer mh-show-buffer))
    (if (not (file-exists-p msg-filename))
	(error "Message %d does not exist" msg-num))
    (set-buffer show-buffer)
    (cond ((not (equal msg-filename buffer-file-name))
	   (mh-unvisit-file)
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
	     (insert-file-contents msg-filename))
           (if mh-decode-quoted-printable
               (mh-decode-quoted-printable))
	   (goto-char (point-min))
	   (cond (clean-message-header
		  (mh-clean-msg-header (point-min)
				       invisible-headers
				       visible-headers)
		  (goto-char (point-min)))
		 (t
		  (mh-start-of-uncleaned-message)))
	   ;; the parts of visiting we want to do (no locking)
	   (or (eq buffer-undo-list t)	;don't save undo info for prev msgs
	       (setq buffer-undo-list nil))
	   (set-buffer-modified-p nil)
	   (set-buffer-auto-saved)
	   ;; the parts of set-visited-file-name we want to do (no locking)
	   (setq buffer-file-name msg-filename)
	   (setq buffer-backed-up nil)
	   (auto-save-mode 1)
	   (set-mark nil)
	   (mh-show-mode)
	   (setq mode-line-buffer-identification
		 (list (format mh-show-buffer-mode-line-buffer-id
			       folder msg-num)))
	   (set-buffer folder)
	   (setq mh-showing-with-headers nil)))))

(defun mh-start-of-uncleaned-message ()
  ;; position uninteresting headers off the top of the window
  (let ((case-fold-search t))
    (re-search-forward
     "^To:\\|^Cc:\\|^From:\\|^Subject:\\|^Date:" nil t)
    (beginning-of-line)
    (mh-recenter 0)))


(defun mh-invalidate-show-buffer ()
  ;; Invalidate the show buffer so we must update it to use it.
  (if (get-buffer mh-show-buffer)
      (save-excursion
	(set-buffer mh-show-buffer)
	(mh-unvisit-file))))


(defun mh-unvisit-file ()
  ;; Separate current buffer from the message file it was visiting.
  (or (not (buffer-modified-p))
      (null buffer-file-name)		;we've been here before
      (yes-or-no-p (format "Message %s modified; flush changes? "
			   (file-name-nondirectory buffer-file-name)))
      (error "Flushing changes not confirmed"))
  (clear-visited-file-modtime)
  (unlock-buffer)
  (setq buffer-file-name nil))

  
(defun mh-get-msg-num (error-if-no-message)
  ;; Return the message number of the displayed message.  If the argument
  ;; ERROR-IF-NO-MESSAGE is non-nil, then complain if the cursor is not
  ;; pointing to a message.
  (save-excursion
    (beginning-of-line)
    (cond ((looking-at mh-scan-msg-number-regexp)
	   (string-to-int (buffer-substring (match-beginning 1)
					    (match-end 1))))
	  (error-if-no-message
	   (error "Cursor not pointing to message"))
	  (t nil))))


(defun mh-msg-filename (msg &optional folder)
  ;; Return the file name of MESSAGE in FOLDER (default current folder).
  (expand-file-name (int-to-string msg)
		    (if folder
			(mh-expand-file-name folder)
			mh-folder-filename)))


(defun mh-clean-msg-header (start invisible-headers visible-headers)
  ;; Flush extraneous lines in a message header, from the given POINT to the
  ;; end of the message header.  If VISIBLE-HEADERS is non-nil, it contains a
  ;; regular expression specifying the lines to display, otherwise
  ;; INVISIBLE-HEADERS contains a regular expression specifying lines to
  ;; delete from the header.
  (let ((case-fold-search t)
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
	      (mh-delete-line 1))))
      (unlock-buffer))))


(defun mh-recenter (arg)
  ;; Like recenter but with two improvements:
  ;;  - only does anything if the current buffer is in the selected
  ;;    window.  (Commands like save-some-buffers can make this false.)
  ;;  - nil arg means recenter as with C-u prefix
  (if (eq (get-buffer-window (current-buffer))
	  (selected-window))
      ;; '(4) is the same as C-u prefix argument.
      (recenter (if arg arg '(4)))))


(defun mh-delete-line (lines)
  ;; Delete version of kill-line.
  (delete-region (point) (progn (forward-line lines) (point))))

(defun mh-notate (msg notation offset)
  ;; Marks MESSAGE with the character NOTATION at position OFFSET.
  ;; Null MESSAGE means the message that the cursor points to.
  (save-excursion
    (if (or (null msg)
	    (mh-goto-msg msg t t))
	(with-mh-folder-updating (t)
	  (beginning-of-line)
	  (forward-char offset)
	  (delete-char 1)
	  (insert notation)))))


(defun mh-find-msg-get-num (step)
  ;; Return the message number of the message on the current scan line
  ;; or one nearby.  Jumps over non-message lines, such as inc errors.
  ;; STEP tells whether to search forward or backward if we have to search.
  (or (mh-get-msg-num nil)
      (let ((msg-num nil)
	    (nreverses 0))
	(while (and (not msg-num)
		    (< nreverses 2))
	  (cond ((eobp)
		 (setq step -1)
		 (setq nreverses (1+ nreverses)))
		((bobp)
		 (setq step 1)
		 (setq nreverses (1+ nreverses))))
	  (forward-line step)
	  (setq msg-num (mh-get-msg-num nil)))
	msg-num)))

(defun mh-goto-msg (number &optional no-error-if-no-message dont-show)
  "Position the cursor at message NUMBER.
Optional non-nil second argument NO-ERROR-IF-NO-MESSAGE means return nil
instead of signaling an error if message does not exist; in this case, the
cursor is positioned near where the message would have been.
Non-nil third argument DONT-SHOW means not to show the message."
  (interactive "NGo to message: ")
  (setq number (prefix-numeric-value number)) ;Emacs 19
  ;; This basic routine tries to be as fast as possible,
  ;; using a binary search and minimal regexps.
  (let ((cur-msg (mh-find-msg-get-num -1))
	(jump-size mh-msg-count))
    (while (and (> jump-size 1)
		cur-msg
		(not (eq cur-msg number)))
      (cond ((< cur-msg number)
	     (setq jump-size (min (- number cur-msg)
				  (ash (1+ jump-size) -1)))
	     (forward-line jump-size)
	     (setq cur-msg (mh-find-msg-get-num 1)))
	    (t
	     (setq jump-size (min (- cur-msg number)
				  (ash (1+ jump-size) -1)))
	     (forward-line (- jump-size))
	     (setq cur-msg (mh-find-msg-get-num -1)))))
    (if (eq cur-msg number)
	(progn
	  (beginning-of-line)
	  (or dont-show
	      (mh-maybe-show number)
	      t))
      (if (not no-error-if-no-message)
	  (error "No message %d" number)))))


(defun mh-msg-search-pat (n)
  ;; Return a search pattern for message N in the scan listing.
  (format mh-scan-msg-search-regexp n))


(defun mh-get-profile-field (field)
  ;; Find and return the value of FIELD in the current buffer.
  ;; Returns NIL if the field is not in the buffer.
  (let ((case-fold-search t))
    (goto-char (point-min))
    (cond ((not (re-search-forward (format "^%s" field) nil t)) nil)
	  ((looking-at "[\t ]*$") nil)
	  (t
	   (re-search-forward "[\t ]*\\([^\t \n].*\\)$" nil t)
	   (let ((start (match-beginning 1)))
	     (end-of-line)
	     (buffer-substring start (point)))))))

(defvar mail-user-agent)
(defvar read-mail-command)

(defvar mh-find-path-run nil
  "Non-nil if `mh-find-path' has been run already.")

(defun mh-find-path ()
  ;; Set mh-progs, mh-lib, and mh-libs-progs
  ;; (This step is necessary if MH was installed after this Emacs was dumped.)
  ;; From profile file, set mh-user-path, mh-draft-folder,
  ;; mh-unseen-seq, mh-previous-seq, mh-inbox.
  (mh-find-progs)
  (unless mh-find-path-run
    (setq mh-find-path-run t)
    (setq read-mail-command 'mh-rmail)
    (setq mail-user-agent 'mh-e-user-agent))
  (save-excursion
    ;; Be sure profile is fully expanded before switching buffers
    (let ((profile (expand-file-name (or (getenv "MH") "~/.mh_profile"))))
      (set-buffer (get-buffer-create mh-temp-buffer))
      (setq buffer-offer-save nil)	;for people who set default to t
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
      (setq mh-draft-folder (mh-get-profile-field "Draft-Folder:"))
      (if mh-draft-folder
	  (progn
	    (if (not (mh-folder-name-p mh-draft-folder))
		(setq mh-draft-folder (format "+%s" mh-draft-folder)))
	    (if (not (file-exists-p (mh-expand-file-name mh-draft-folder)))
		(error "Draft folder \"%s\" not found.  Create it and try again"
		       (mh-expand-file-name mh-draft-folder)))))
      (setq mh-inbox (mh-get-profile-field "Inbox:"))
      (cond ((not mh-inbox)
	     (setq mh-inbox "+inbox"))
	    ((not (mh-folder-name-p mh-inbox))
	     (setq mh-inbox (format "+%s" mh-inbox))))
      (setq mh-unseen-seq (mh-get-profile-field "Unseen-Sequence:"))
      (if mh-unseen-seq
	  (setq mh-unseen-seq (intern mh-unseen-seq))
	(setq mh-unseen-seq 'unseen))	;old MH default?
      (setq mh-previous-seq (mh-get-profile-field "Previous-Sequence:"))
      (if mh-previous-seq
	  (setq mh-previous-seq (intern mh-previous-seq)))
      (run-hooks 'mh-find-path-hook)))
  (and mh-auto-folder-collect
       (let ((mh-no-install t))		;only get folders if MH installed
	 (condition-case err
	     (mh-make-folder-list-background)
	   (file-error)))))		;so don't complain if not installed

(defun mh-file-command-p (file)
  "Return t if file FILE is the name of a executable regular file."
  (and (file-regular-p file) (file-executable-p file)))

(defun mh-find-progs ()
  "Find the directories for the installed MH/nmh binaries and config files.
Set the `mh-progs' and `mh-lib', and `mh-lib-progs' variables to the
directory names and set `mh-nmh-p' if we detect nmh instead of MH."
  (let ((path (or (mh-path-search exec-path "mhparam")
		 (mh-path-search '("/usr/local/nmh/bin" ; nmh default
				  "/usr/local/bin/mh/"
				  "/usr/local/mh/"
				  "/usr/bin/mh/" ;Ultrix 4.2
				  "/usr/new/mh/" ;Ultrix <4.2
				  "/usr/contrib/mh/bin/" ;BSDI
				  "/usr/pkg/bin/"	; NetBSD
				  "/usr/local/bin/"
				  )
				 "mhparam"))))
    (if (not path)
      (error "Unable to find the `mhparam' command"))
    (save-excursion
      (let ((tmp-buffer (get-buffer-create mh-temp-buffer)))
	(set-buffer tmp-buffer)
	(unwind-protect
	    (progn
	      (call-process (expand-file-name "mhparam" path)
			    nil '(t nil) nil "libdir" "etcdir")
	      (goto-char (point-min))
	      (if (search-forward-regexp "^libdir:\\s-\\(\\S-+\\)\\s-*$" nil t)
		  (setq mh-lib-progs (match-string 1)
			mh-lib mh-lib-progs
			mh-progs path))
	      (goto-char (point-min))
	      (if (search-forward-regexp "^etcdir:\\s-\\(\\S-+\\)\\s-*$" nil t)
		  (setq mh-lib (match-string 1)
			mh-nmh-p t)))
	  (kill-buffer tmp-buffer))))
    (unless (and mh-progs mh-lib mh-lib-progs)
	(error "Unable to determine paths from `mhparam' command"))))

(defun mh-path-search (path file &optional func-p)
  ;; Search PATH, a list of directory names, for FILE.
  ;; Returns the element of PATH that contains FILE, or nil if not found.
  (while (and path
	      (not (funcall (or func-p 'mh-file-command-p)
			    (expand-file-name file (car path)))))
    (setq path (cdr path)))
  (car path))

(defvar mh-no-install nil)		;do not run install-mh

(defun mh-install (profile error-val)
  ;; Called to do error recovery if we fail to read the profile file.
  ;; If possible, initialize the MH environment.
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
     (signal (car err)			;re-signal with more specific msg
	     (list (format "Cannot read MH profile \"%s\"" profile)
		   (car (cdr (cdr err))))))))


(defun mh-set-folder-modified-p (flag)
  ;; Mark current folder as modified or unmodified according to FLAG.
  (set-buffer-modified-p flag))


(defun mh-find-seq (name) (assoc name mh-seq-list))

(defun mh-seq-to-msgs (seq)
  ;; Return a list of the messages in SEQUENCE.
  (mh-seq-msgs (mh-find-seq seq)))


(defun mh-add-msgs-to-seq (msgs seq &optional internal-flag)
  ;; Add MESSAGE(s) to the SEQUENCE.  If optional FLAG is non-nil, do not mark
  ;; the message in the scan listing or inform MH of the addition.
  (let ((entry (mh-find-seq seq)))
    (if (and msgs (atom msgs)) (setq msgs (list msgs)))
    (if (null entry)
	(setq mh-seq-list (cons (mh-make-seq seq msgs) mh-seq-list))
	(if msgs (setcdr entry (append msgs (mh-seq-msgs entry)))))
    (cond ((not internal-flag)
	   (mh-add-to-sequence seq msgs)
	   (mh-notate-seq seq mh-note-seq (1+ mh-cmd-note))))))

(defvar mh-folder-hist nil)

(defun mh-prompt-for-folder (prompt default can-create)
  ;; Prompt for a folder name with PROMPT.  Returns the folder's name as a
  ;; string.  DEFAULT is used if the folder exists and the user types return.
  ;; If the CAN-CREATE flag is t, then a non-existent folder is made.
  (if (null default)
      (setq default ""))
  (let* ((prompt (format "%s folder%s" prompt
			 (if (equal "" default)
			     "? "
			     (format " [%s]? " default))))
	 read-name folder-name)
    (if (null mh-folder-list)
	(mh-set-folder-list))
    (while (and (setq read-name (completing-read prompt mh-folder-list nil nil
						 "+" 'mh-folder-hist default))
		(equal read-name "")
		(equal default "")))
    (cond ((or (equal read-name "") (equal read-name "+"))
	   (setq read-name default))
	  ((not (mh-folder-name-p read-name))
	   (setq read-name (format "+%s" read-name))))
    (if (or (not read-name) (equal "" read-name))
        (error "No folder specified"))
    (setq folder-name read-name)
    (cond ((and (> (length folder-name) 0)
		(eq (aref folder-name (1- (length folder-name))) ?/))
	   (setq folder-name (substring folder-name 0 -1))))
    (let ((new-file-p (not (file-exists-p (mh-expand-file-name folder-name)))))
      (cond ((and new-file-p
		  (y-or-n-p
		   (format "Folder %s does not exist.  Create it? " folder-name)))
	     (message "Creating %s" folder-name)
	     (call-process "mkdir" nil nil nil (mh-expand-file-name folder-name))
	     (message "Creating %s...done" folder-name)
	     (setq mh-folder-list (cons (list read-name) mh-folder-list))
	     (run-hooks 'mh-folder-list-change-hook))
	    (new-file-p
	     (error "Folder %s is not created" folder-name))
	    ((not (file-directory-p (mh-expand-file-name folder-name)))
	     (error "\"%s\" is not a directory"
		    (mh-expand-file-name folder-name)))
	    ((and (null (assoc read-name mh-folder-list))
		  (null (assoc (concat read-name "/") mh-folder-list)))
	     (setq mh-folder-list (cons (list read-name) mh-folder-list))
	     (run-hooks 'mh-folder-list-change-hook))))
    folder-name))


(defvar mh-make-folder-list-process nil) ;The background process collecting the folder list.

(defvar mh-folder-list-temp nil)	;mh-folder-list as it is being built.

(defvar mh-folder-list-partial-line "")	;Start of last incomplete line from folder process.

(defun mh-set-folder-list ()
  ;; Sets mh-folder-list correctly.
  ;; A useful function for the command line or for when you need to
  ;; sync by hand.  Format is in a form suitable for completing read.
  (message "Collecting folder names...")
  (if (not mh-make-folder-list-process)
      (mh-make-folder-list-background))
  (while (eq (process-status mh-make-folder-list-process) 'run)
    (accept-process-output mh-make-folder-list-process))
  (setq mh-folder-list mh-folder-list-temp)
  (run-hooks 'mh-folder-list-change-hook)
  (setq mh-folder-list-temp nil)
  (delete-process mh-make-folder-list-process)
  (setq mh-make-folder-list-process nil)
  (message "Collecting folder names...done"))

(defun mh-make-folder-list-background ()
  ;; Start a background process to compute a list of the user's folders.
  ;; Call mh-set-folder-list to wait for the result.
  (cond
   ((not mh-make-folder-list-process)
    (unless mh-inbox
      (mh-find-path))
    (let ((process-connection-type nil))
      (setq mh-make-folder-list-process
	    (start-process "folders" nil (expand-file-name "folders" mh-progs)
			   "-fast"
			   (if mh-recursive-folders
			       "-recurse"
			     "-norecurse")))
      (set-process-filter mh-make-folder-list-process
			  'mh-make-folder-list-filter)
      (process-kill-without-query mh-make-folder-list-process)))))

(defun mh-make-folder-list-filter (process output)
  ;; parse output from "folders -fast"
  (let ((position 0)
	line-end
	new-folder
	(prevailing-match-data (match-data)))
    (unwind-protect
	;; make sure got complete line
	(while (setq line-end (string-match "\n" output position))
	  (setq new-folder (format "+%s%s"
				   mh-folder-list-partial-line
				   (substring output position line-end)))
	  (setq mh-folder-list-partial-line "")
	  ;; is new folder a subfolder of previous?
	  (if (and mh-folder-list-temp
		   (string-match
		    (regexp-quote
		     (concat (car (car mh-folder-list-temp)) "/"))
		    new-folder))
	      ;; append slash to parent folder for better completion
	      ;; (undone by mh-prompt-for-folder)
	      (setq mh-folder-list-temp
		    (cons
		     (list new-folder)
		     (cons
		      (list (concat (car (car mh-folder-list-temp)) "/"))
		      (cdr mh-folder-list-temp))))
	    (setq mh-folder-list-temp
		  (cons (list new-folder)
			mh-folder-list-temp)))
	  (setq position (1+ line-end)))
      (set-match-data prevailing-match-data))
    (setq mh-folder-list-partial-line (substring output position))))


(defun mh-folder-name-p (name)
  ;; Return non-NIL if NAME is possibly the name of a folder.
  ;; A name (a string or symbol) can be a folder name if it begins with "+".
  (if (symbolp name)
      (eq (aref (symbol-name name) 0) ?+)
    (and (> (length name) 0)
	 (eq (aref name 0) ?+))))


;;; Issue commands to MH.


(defun mh-exec-cmd (command &rest args)
  ;; Execute mh-command COMMAND with ARGS.
  ;; The side effects are what is desired.
  ;; Any output is assumed to be an error and is shown to the user.
  ;; The output is not read or parsed by mh-e.
  (save-excursion
    (set-buffer (get-buffer-create mh-temp-buffer))
    (erase-buffer)
    (apply 'call-process
	   (expand-file-name command mh-progs) nil t nil
	   (mh-list-to-string args))
    (if (> (buffer-size) 0)
	(save-window-excursion
	  (switch-to-buffer-other-window mh-temp-buffer)
	  (sit-for 5)))))


(defun mh-exec-cmd-error (env command &rest args)
  ;; In environment ENV, execute mh-command COMMAND with args ARGS.
  ;; ENV is nil or a string of space-separated "var=value" elements.
  ;; Signals an error if process does not complete successfully.
  (save-excursion
    (set-buffer (get-buffer-create mh-temp-buffer))
    (erase-buffer)
    (let ((status
	   (if env
	       ;; the shell hacks necessary here shows just how broken Unix is
	       (apply 'call-process "/bin/sh" nil t nil "-c"
		      (format "%s %s ${1+\"$@\"}"
			      env
			      (expand-file-name command mh-progs))
		      command
		      (mh-list-to-string args))
	       (apply 'call-process
		      (expand-file-name command mh-progs) nil t nil
		      (mh-list-to-string args)))))
      (mh-handle-process-error command status))))


(defun mh-exec-cmd-daemon (command &rest args)
  ;; Execute MH command COMMAND with ARGS in the background.
  ;; Any output from command is displayed in an asynchronous pop-up window.
  (save-excursion
    (set-buffer (get-buffer-create mh-temp-buffer))
    (erase-buffer))
  (let* ((process-connection-type nil)
	 (process (apply 'start-process
			 command nil
			 (expand-file-name command mh-progs)
			 (mh-list-to-string args))))
    (set-process-filter process 'mh-process-daemon)))

(defun mh-process-daemon (process output)
  ;; Process daemon that puts output into a temporary buffer.
  (set-buffer (get-buffer-create mh-temp-buffer))
  (insert-before-markers output)
  (display-buffer mh-temp-buffer))


(defun mh-exec-cmd-quiet (raise-error command &rest args)
  ;; Args are RAISE-ERROR, COMMANDS, ARGS....
  ;; Execute MH command COMMAND with ARGS.  ARGS is a list of strings.
  ;; Return at start of mh-temp buffer, where output can be parsed and used.
  ;; Returns value of call-process, which is 0 for success,
  ;; unless RAISE-ERROR is non-nil, in which case an error is signaled
  ;; if call-process returns non-0.
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


(defun mh-exec-cmd-output (command display &rest args)
  ;; Execute MH command COMMAND with DISPLAY flag and ARGS.
  ;; Put the output into buffer after point.  Set mark after inserted text.
  ;; Output is expected to be shown to user, not parsed by mh-e.
  (push-mark (point) t)
  (apply 'call-process
	 (expand-file-name command mh-progs) nil t display
	 (mh-list-to-string args))
  (exchange-point-and-mark))


(defun mh-exec-lib-cmd-output (command &rest args)
  ;; Execute MH library command COMMAND with ARGS.
  ;; Put the output into buffer after point.  Set mark after inserted text.
  (apply 'mh-exec-cmd-output (expand-file-name command mh-lib-progs) nil args))


(defun mh-handle-process-error (command status)
  ;; Raise error if COMMAND returned non-0 STATUS, otherwise return STATUS.
  ;; STATUS is return value from call-process.
  ;; Program output is in current buffer.
  ;; If output is too long to include in error message, display the buffer.
  (cond ((eq status 0)			;success
	 status)
	((stringp status)		;kill string
	 (error "%s: %s" command status))
	(t				;exit code
	 (cond
	  ((= (buffer-size) 0)		;program produced no error message
	   (error "%s: exit code %d" command status))
	  (t
	   ;; will error message fit on one line?
	   (goto-line 2)
	   (if (and (< (buffer-size) (frame-width))
		    (eobp))
	       (error "%s"
		      (buffer-substring 1 (progn (goto-char 1)
						 (end-of-line)
						 (point))))
	     (display-buffer (current-buffer))
	     (error "%s failed with status %d.  See error message in other window"
		    command status)))))))


(defun mh-expand-file-name (filename &optional default)
  ;; Just like `expand-file-name', but also handles MH folder names.
  ;; Assumes that any filename that starts with '+' is a folder name.
   (if (mh-folder-name-p filename)
       (expand-file-name (substring filename 1) mh-user-path)
     (expand-file-name filename default)))


(defun mh-list-to-string (l)
  ;; Flattens the list L and makes every element of the new list into a string.
  (nreverse (mh-list-to-string-1 l)))

(defun mh-list-to-string-1 (l)
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

(provide 'mh-utils)

;;; mh-utils.el ends here
