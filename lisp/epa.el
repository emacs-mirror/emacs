;;; epa.el --- the EasyPG Assistant -*- lexical-binding: t -*-

;; Copyright (C) 2006-2018 Free Software Foundation, Inc.

;; Author: Daiki Ueno <ueno@unixuser.org>
;; Keywords: PGP, GnuPG

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

;;; Code:

(require 'epg)
(require 'font-lock)
(require 'widget)
(eval-when-compile (require 'wid-edit))
(require 'derived)

(defgroup epa nil
  "The EasyPG Assistant"
  :version "23.1"
  :link '(custom-manual "(epa) Top")
  :group 'epg)

(defcustom epa-replace-original-text 'ask
  "Whether the original text shall be replaced by the decrypted.

If t, replace the original text without any confirmation.
If nil, don't replace the original text and show the result in a new buffer.
If neither t nor nil, ask user for confirmation."
  :version "26.1"
  :type '(choice (const :tag "Never" nil)
		 (const :tag "Ask the user" ask)
		 (const :tag "Always" t))
  :group 'epa)

(defcustom epa-popup-info-window t
  "If non-nil, display status information from epa commands in another window."
  :type 'boolean
  :group 'epa)

(defcustom epa-info-window-height 5
  "Number of lines used to display status information."
  :type 'integer
  :group 'epa)

;; In the doc string below, we say "symbol `error'" to avoid producing
;; a hyperlink for `error' the function.
(defcustom epa-pinentry-mode nil
  "The pinentry mode.

GnuPG 2.1 or later has an option to control the behavior of
Pinentry invocation.  The value should be the symbol `error',
`ask', `cancel', or `loopback'.  See the GnuPG manual for the
meanings.

In epa commands, a particularly useful mode is `loopback', which
redirects all Pinentry queries to the caller, so Emacs can query
passphrase through the minibuffer, instead of external Pinentry
program."
  :type '(choice (const nil)
		 (const ask)
		 (const cancel)
		 (const error)
		 (const loopback))
  :group 'epa
  :version "25.1")

(defgroup epa-faces nil
  "Faces for epa-mode."
  :version "23.1"
  :group 'epa)

(defcustom epa-mail-aliases nil
  "Alist of aliases of email addresses that stand for encryption keys.
Each element is a list of email addresses (ALIAS EXPANSIONS...).
When one of the recipients of a message being encrypted is ALIAS,
instead of encrypting it for ALIAS, encrypt it for EXPANSIONS...

If EXPANSIONS is empty, ignore ALIAS as regards encryption.
This is a handy way to avoid warnings about addresses that you don't
have any key for.

The command `epa-mail-encrypt' uses this."
  :type '(repeat (cons (string :tag "Alias") (repeat (string :tag "Expansion"))))
  :group 'epa
  :version "24.4")

(defface epa-validity-high
  '((default :weight bold)
    (((class color) (background dark)) :foreground "PaleTurquoise"))
  "Face for high validity EPA information."
  :group 'epa-faces)

(defface epa-validity-medium
  '((default :slant italic)
    (((class color) (background dark)) :foreground "PaleTurquoise"))
  "Face for medium validity EPA information."
  :group 'epa-faces)

(defface epa-validity-low
  '((t :slant italic))
  "Face used for displaying the low validity."
  :group 'epa-faces)

(defface epa-validity-disabled
  '((t :slant italic :inverse-video t))
  "Face used for displaying the disabled validity."
  :group 'epa-faces)

(defface epa-string
  '((((class color) (background dark))
     :foreground "lightyellow")
    (((class color) (background light))
     :foreground "blue4"))
  "Face used for displaying the string."
  :group 'epa-faces)

(defface epa-mark
  '((default :weight bold)
    (((class color) (background dark))  :foreground "orange")
    (((class color) (background light)) :foreground "red"))
  "Face used for displaying the high validity."
  :group 'epa-faces)

(defface epa-field-name
  '((default :weight bold)
    (((class color) (background dark)) :foreground "PaleTurquoise"))
  "Face for the name of the attribute field."
  :group 'epa)

(defface epa-field-body
  '((default :slant italic)
    (((class color) (background dark)) :foreground "turquoise"))
  "Face for the body of the attribute field."
  :group 'epa)

(defcustom epa-validity-face-alist
  '((unknown . epa-validity-disabled)
    (invalid . epa-validity-disabled)
    (disabled . epa-validity-disabled)
    (revoked . epa-validity-disabled)
    (expired . epa-validity-disabled)
    (none . epa-validity-low)
    (undefined . epa-validity-low)
    (never . epa-validity-low)
    (marginal . epa-validity-medium)
    (full . epa-validity-high)
    (ultimate . epa-validity-high))
  "An alist mapping validity values to faces."
  :type '(repeat (cons symbol face))
  :group 'epa)

(defvar epa-font-lock-keywords
  '(("^\\*"
     (0 'epa-mark))
    ("^\t\\([^\t:]+:\\)[ \t]*\\(.*\\)$"
     (1 'epa-field-name)
     (2 'epa-field-body)))
  "Default expressions to addon in epa-mode.")

(defconst epa-pubkey-algorithm-letter-alist
  '((1 . ?R)
    (2 . ?r)
    (3 . ?s)
    (16 . ?g)
    (17 . ?D)
    (20 . ?G)))

(defvar epa-protocol 'OpenPGP
  "The default protocol.
The value can be either OpenPGP or CMS.

You should bind this variable with `let', but do not set it globally.")

(defvar epa-armor nil
  "If non-nil, epa commands create ASCII armored output.

You should bind this variable with `let', but do not set it globally.")

(defvar epa-textmode nil
  "If non-nil, epa commands treat input files as text.

You should bind this variable with `let', but do not set it globally.")

(defvar epa-keys-buffer nil)
(defvar epa-key-buffer-alist nil)
(defvar epa-key nil)
(defvar epa-list-keys-arguments nil)
(defvar epa-info-buffer nil)
(defvar epa-error-buffer nil)
(defvar epa-last-coding-system-specified nil)

(defvar epa-key-list-mode-map
  (let ((keymap (make-sparse-keymap))
	(menu-map (make-sparse-keymap)))
    (define-key keymap "m" 'epa-mark-key)
    (define-key keymap "u" 'epa-unmark-key)
    (define-key keymap "d" 'epa-decrypt-file)
    (define-key keymap "v" 'epa-verify-file)
    (define-key keymap "s" 'epa-sign-file)
    (define-key keymap "e" 'epa-encrypt-file)
    (define-key keymap "r" 'epa-delete-keys)
    (define-key keymap "i" 'epa-import-keys)
    (define-key keymap "o" 'epa-export-keys)
    (define-key keymap "g" 'revert-buffer)
    (define-key keymap "n" 'next-line)
    (define-key keymap "p" 'previous-line)
    (define-key keymap " " 'scroll-up-command)
    (define-key keymap [?\S-\ ] 'scroll-down-command)
    (define-key keymap [delete] 'scroll-down-command)
    (define-key keymap "q" 'epa-exit-buffer)
    (define-key keymap [menu-bar epa-key-list-mode] (cons "Keys" menu-map))
    (define-key menu-map [epa-key-list-unmark-key]
      '(menu-item "Unmark Key" epa-unmark-key
		  :help "Unmark a key"))
    (define-key menu-map [epa-key-list-mark-key]
      '(menu-item "Mark Key" epa-mark-key
		  :help "Mark a key"))
    (define-key menu-map [separator-epa-file] '(menu-item "--"))
    (define-key menu-map [epa-verify-file]
      '(menu-item "Verify File..." epa-verify-file
		  :help "Verify FILE"))
    (define-key menu-map [epa-sign-file]
      '(menu-item "Sign File..." epa-sign-file
		  :help "Sign FILE by SIGNERS keys selected"))
    (define-key menu-map [epa-decrypt-file]
      '(menu-item "Decrypt File..." epa-decrypt-file
		  :help "Decrypt FILE"))
    (define-key menu-map [epa-encrypt-file]
      '(menu-item "Encrypt File..." epa-encrypt-file
		  :help "Encrypt FILE for RECIPIENTS"))
    (define-key menu-map [separator-epa-key-list] '(menu-item "--"))
    (define-key menu-map [epa-key-list-delete-keys]
      '(menu-item "Delete Keys" epa-delete-keys
		  :help "Delete Marked Keys"))
    (define-key menu-map [epa-key-list-import-keys]
      '(menu-item "Import Keys" epa-import-keys
		  :help "Import keys from a file"))
    (define-key menu-map [epa-key-list-export-keys]
      '(menu-item "Export Keys" epa-export-keys
		  :help "Export marked keys to a file"))
    keymap))

(defvar epa-key-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "q" 'epa-exit-buffer)
    keymap))

(defvar epa-info-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "q" 'delete-window)
    keymap))

(defvar epa-exit-buffer-function #'quit-window)

(define-widget 'epa-key 'push-button
  "Button for representing an epg-key object."
  :format "%[%v%]"
  :button-face-get 'epa--key-widget-button-face-get
  :value-create 'epa--key-widget-value-create
  :action 'epa--key-widget-action
  :help-echo 'epa--key-widget-help-echo)

(defun epa--key-widget-action (widget &optional _event)
  (save-selected-window
    (epa--show-key (widget-get widget :value))))

(defun epa--key-widget-value-create (widget)
  (let* ((key (widget-get widget :value))
	 (primary-sub-key (car (epg-key-sub-key-list key)))
	 (primary-user-id (car (epg-key-user-id-list key))))
    (insert (format "%c "
		    (if (epg-sub-key-validity primary-sub-key)
			(car (rassq (epg-sub-key-validity primary-sub-key)
				    epg-key-validity-alist))
		      ? ))
	    (epg-sub-key-id primary-sub-key)
	    " "
	    (if primary-user-id
		(if (stringp (epg-user-id-string primary-user-id))
		    (epg-user-id-string primary-user-id)
		  (epg-decode-dn (epg-user-id-string primary-user-id)))
	      ""))))

(defun epa--key-widget-button-face-get (widget)
  (let ((validity (epg-sub-key-validity (car (epg-key-sub-key-list
					      (widget-get widget :value))))))
    (if validity
	(cdr (assq validity epa-validity-face-alist))
      'default)))

(defun epa--key-widget-help-echo (widget)
  (format "Show %s"
	  (epg-sub-key-id (car (epg-key-sub-key-list
				(widget-get widget :value))))))

(defalias 'epa--encode-coding-string
  (if (fboundp 'encode-coding-string) #'encode-coding-string #'identity))

(defalias 'epa--decode-coding-string
  (if (fboundp 'decode-coding-string) #'decode-coding-string #'identity))

(define-derived-mode epa-key-list-mode special-mode "Keys"
  "Major mode for `epa-list-keys'."
  (buffer-disable-undo)
  (setq truncate-lines t
	buffer-read-only t)
  (setq-local font-lock-defaults '(epa-font-lock-keywords t))
  ;; In XEmacs, auto-initialization of font-lock is not effective
  ;; if buffer-file-name is not set.
  (font-lock-set-defaults)
  (make-local-variable 'epa-exit-buffer-function)
  (setq-local revert-buffer-function #'epa--key-list-revert-buffer))

(define-derived-mode epa-key-mode special-mode "Key"
  "Major mode for a key description."
  (buffer-disable-undo)
  (setq truncate-lines t
	buffer-read-only t)
  (setq-local font-lock-defaults '(epa-font-lock-keywords t))
  ;; In XEmacs, auto-initialization of font-lock is not effective
  ;; if buffer-file-name is not set.
  (font-lock-set-defaults)
  (make-local-variable 'epa-exit-buffer-function))

(define-derived-mode epa-info-mode special-mode "Info"
  "Major mode for `epa-info-buffer'."
  (buffer-disable-undo)
  (setq truncate-lines t
	buffer-read-only t))

(defun epa-mark-key (&optional arg)
  "Mark a key on the current line.
If ARG is non-nil, unmark the key."
  (interactive "P")
  (let ((inhibit-read-only t)
	buffer-read-only
	properties)
    (beginning-of-line)
    (unless (get-text-property (point) 'epa-key)
      (error "No key on this line"))
    (setq properties (text-properties-at (point)))
    (delete-char 1)
    (insert (if arg " " "*"))
    (set-text-properties (1- (point)) (point) properties)
    (forward-line)))

(defun epa-unmark-key (&optional arg)
  "Unmark a key on the current line.
If ARG is non-nil, mark the key."
  (interactive "P")
  (epa-mark-key (not arg)))

(defun epa-exit-buffer ()
  "Exit the current buffer.
`epa-exit-buffer-function' is called if it is set."
  (interactive)
  (funcall epa-exit-buffer-function))

(defun epa--insert-keys (keys)
  (save-excursion
    (save-restriction
      (narrow-to-region (point) (point))
      (let (point)
	(while keys
	  (setq point (point))
	  (insert "  ")
	  (add-text-properties point (point)
			       (list 'epa-key (car keys)
				     'front-sticky nil
				     'rear-nonsticky t
				     'start-open t
				     'end-open t))
	  (widget-create 'epa-key :value (car keys))
	  (insert "\n")
	  (setq keys (cdr keys))))
      (add-text-properties (point-min) (point-max)
			   (list 'epa-list-keys t
				 'front-sticky nil
				 'rear-nonsticky t
				 'start-open t
				 'end-open t)))))

(defun epa--list-keys (name secret)
  (unless (and epa-keys-buffer
	       (buffer-live-p epa-keys-buffer))
    (setq epa-keys-buffer (generate-new-buffer "*Keys*")))
  (set-buffer epa-keys-buffer)
  (epa-key-list-mode)
  (let ((inhibit-read-only t)
	buffer-read-only
	(point (point-min))
	(context (epg-make-context epa-protocol)))
    (unless (get-text-property point 'epa-list-keys)
      (setq point (next-single-property-change point 'epa-list-keys)))
    (when point
      (delete-region point
		     (or (next-single-property-change point 'epa-list-keys)
			 (point-max)))
      (goto-char point))
    (epa--insert-keys (epg-list-keys context name secret))
    (widget-setup)
    (set-keymap-parent (current-local-map) widget-keymap))
  (make-local-variable 'epa-list-keys-arguments)
  (setq epa-list-keys-arguments (list name secret))
  (goto-char (point-min))
  (pop-to-buffer (current-buffer)))

;;;###autoload
(defun epa-list-keys (&optional name)
  "List all keys matched with NAME from the public keyring."
  (interactive
   (if current-prefix-arg
       (let ((name (read-string "Pattern: "
				(if epa-list-keys-arguments
				    (car epa-list-keys-arguments)))))
	 (list (if (equal name "") nil name)))
     (list nil)))
  (epa--list-keys name nil))

;;;###autoload
(defun epa-list-secret-keys (&optional name)
  "List all keys matched with NAME from the private keyring."
  (interactive
   (if current-prefix-arg
       (let ((name (read-string "Pattern: "
				(if epa-list-keys-arguments
				    (car epa-list-keys-arguments)))))
	 (list (if (equal name "") nil name)))
     (list nil)))
  (epa--list-keys name t))

(defun epa--key-list-revert-buffer (&optional _ignore-auto _noconfirm)
  (apply #'epa--list-keys epa-list-keys-arguments))

(defun epa--marked-keys ()
  (or (with-current-buffer epa-keys-buffer
	(goto-char (point-min))
	(let (keys key)
	  (while (re-search-forward "^\\*" nil t)
	    (if (setq key (get-text-property (match-beginning 0)
					     'epa-key))
		(setq keys (cons key keys))))
	  (nreverse keys)))
      (let ((key (get-text-property (point-at-bol) 'epa-key)))
	(if key
	    (list key)))))

(defun epa--select-keys (prompt keys)
  (unless (and epa-keys-buffer
               (buffer-live-p epa-keys-buffer))
    (setq epa-keys-buffer (generate-new-buffer "*Keys*")))
  (with-current-buffer epa-keys-buffer
    (epa-key-list-mode)
    ;; C-c C-c is the usual way to finish the selection (bug#11159).
    (define-key (current-local-map) "\C-c\C-c" 'exit-recursive-edit)
    (let ((inhibit-read-only t)
	  buffer-read-only)
      (erase-buffer)
      (insert prompt "\n"
	      (substitute-command-keys "\
- `\\[epa-mark-key]' to mark a key on the line
- `\\[epa-unmark-key]' to unmark a key on the line\n"))
      (widget-create 'link
		     :notify (lambda (&rest _ignore) (abort-recursive-edit))
		     :help-echo
		     "Click here or \\[abort-recursive-edit] to cancel"
		     "Cancel")
      (widget-create 'link
		     :notify (lambda (&rest _ignore) (exit-recursive-edit))
		     :help-echo
		     "Click here or \\[exit-recursive-edit] to finish"
		     "OK")
      (insert "\n\n")
      (epa--insert-keys keys)
      (widget-setup)
      (set-keymap-parent (current-local-map) widget-keymap)
      (setq epa-exit-buffer-function #'abort-recursive-edit)
      (goto-char (point-min))
      (let ((display-buffer-mark-dedicated 'soft))
        (pop-to-buffer (current-buffer))))
    (unwind-protect
	(progn
	  (recursive-edit)
	  (epa--marked-keys))
      (kill-buffer epa-keys-buffer))))

;;;###autoload
(defun epa-select-keys (context prompt &optional names secret)
  "Display a user's keyring and ask him to select keys.
CONTEXT is an epg-context.
PROMPT is a string to prompt with.
NAMES is a list of strings to be matched with keys.  If it is nil, all
the keys are listed.
If SECRET is non-nil, list secret keys instead of public keys."
  (let ((keys (epg-list-keys context names secret)))
    (epa--select-keys prompt keys)))

(defun epa--show-key (key)
  (let* ((primary-sub-key (car (epg-key-sub-key-list key)))
	 (entry (assoc (epg-sub-key-id primary-sub-key)
		       epa-key-buffer-alist))
	 (inhibit-read-only t)
	 buffer-read-only
	 pointer)
    (unless entry
      (setq entry (cons (epg-sub-key-id primary-sub-key) nil)
	    epa-key-buffer-alist (cons entry epa-key-buffer-alist)))
    (unless (and (cdr entry)
		 (buffer-live-p (cdr entry)))
      (setcdr entry (generate-new-buffer
		     (format "*Key*%s" (epg-sub-key-id primary-sub-key)))))
    (set-buffer (cdr entry))
    (epa-key-mode)
    (make-local-variable 'epa-key)
    (setq epa-key key)
    (erase-buffer)
    (setq pointer (epg-key-user-id-list key))
    (while pointer
      (if (car pointer)
	  (insert " "
		  (if (epg-user-id-validity (car pointer))
		      (char-to-string
		       (car (rassq (epg-user-id-validity (car pointer))
				   epg-key-validity-alist)))
		    " ")
		  " "
		  (if (stringp (epg-user-id-string (car pointer)))
		      (epg-user-id-string (car pointer))
		    (epg-decode-dn (epg-user-id-string (car pointer))))
		  "\n"))
      (setq pointer (cdr pointer)))
    (setq pointer (epg-key-sub-key-list key))
    (while pointer
      (insert " "
	      (if (epg-sub-key-validity (car pointer))
		  (char-to-string
		   (car (rassq (epg-sub-key-validity (car pointer))
			       epg-key-validity-alist)))
		" ")
	      " "
	      (epg-sub-key-id (car pointer))
	      " "
	      (format "%dbits"
		      (epg-sub-key-length (car pointer)))
	      " "
	      (cdr (assq (epg-sub-key-algorithm (car pointer))
			 epg-pubkey-algorithm-alist))
	      "\n\tCreated: "
	      (condition-case nil
		  (format-time-string "%Y-%m-%d"
				      (epg-sub-key-creation-time (car pointer)))
		(error "????-??-??"))
	      (if (epg-sub-key-expiration-time (car pointer))
		  (format (if (time-less-p nil
					   (epg-sub-key-expiration-time
					    (car pointer)))
			      "\n\tExpires: %s"
			    "\n\tExpired: %s")
			  (condition-case nil
			      (format-time-string "%Y-%m-%d"
						  (epg-sub-key-expiration-time
						   (car pointer)))
			    (error "????-??-??")))
		"")
	      "\n\tCapabilities: "
	      (mapconcat #'symbol-name
			 (epg-sub-key-capability (car pointer))
			 " ")
	      "\n\tFingerprint: "
	      (epg-sub-key-fingerprint (car pointer))
	      "\n")
      (setq pointer (cdr pointer)))
    (goto-char (point-min))
    (pop-to-buffer (current-buffer))))

(defun epa-display-info (info)
  (if epa-popup-info-window
      (save-selected-window
	(unless (and epa-info-buffer (buffer-live-p epa-info-buffer))
	  (setq epa-info-buffer (generate-new-buffer "*Info*")))
	(if (get-buffer-window epa-info-buffer)
	    (delete-window (get-buffer-window epa-info-buffer)))
	(with-current-buffer epa-info-buffer
	  (let ((inhibit-read-only t)
		buffer-read-only)
	    (erase-buffer)
	    (insert info))
	  (epa-info-mode)
	  (goto-char (point-min)))
	(if (> (window-height)
	       epa-info-window-height)
	    (set-window-buffer (split-window nil (- (window-height)
						    epa-info-window-height))
			       epa-info-buffer)
	  (pop-to-buffer epa-info-buffer)
	  (if (> (window-height) epa-info-window-height)
	      (shrink-window (- (window-height) epa-info-window-height)))))
    (message "%s" info)))

(defun epa-display-error (context)
  (unless (equal (epg-context-error-output context) "")
    (let ((buffer (get-buffer-create "*Error*")))
      (save-selected-window
	(unless (and epa-error-buffer (buffer-live-p epa-error-buffer))
	  (setq epa-error-buffer (generate-new-buffer "*Error*")))
	(if (get-buffer-window epa-error-buffer)
	    (delete-window (get-buffer-window epa-error-buffer)))
	(with-current-buffer buffer
	  (let ((inhibit-read-only t)
		buffer-read-only)
	    (erase-buffer)
	    (insert (format
		     (pcase (epg-context-operation context)
		       (`decrypt "Error while decrypting with \"%s\":")
		       (`verify "Error while verifying with \"%s\":")
		       (`sign "Error while signing with \"%s\":")
		       (`encrypt "Error while encrypting with \"%s\":")
		       (`import-keys "Error while importing keys with \"%s\":")
		       (`export-keys "Error while exporting keys with \"%s\":")
		       (_ "Error while executing \"%s\":\n\n"))
		     (epg-context-program context))
		    "\n\n"
		    (epg-context-error-output context)))
	  (epa-info-mode)
	  (goto-char (point-min)))
	(display-buffer buffer)))))

(defun epa-display-verify-result (verify-result)
  (declare (obsolete epa-display-info "23.1"))
  (epa-display-info (epg-verify-result-to-string verify-result)))

(defun epa-passphrase-callback-function (context key-id handback)
  (if (eq key-id 'SYM)
      (read-passwd
       (format "Passphrase for symmetric encryption%s: "
	       ;; Add the file name to the prompt, if any.
	       (if (stringp handback)
		   (format " for %s" handback)
		 ""))
       (eq (epg-context-operation context) 'encrypt))
    (read-passwd
     (if (eq key-id 'PIN)
	 "Passphrase for PIN: "
       (let ((entry (assoc key-id epg-user-id-alist)))
	 (if entry
	     (format "Passphrase for %s %s: " key-id (cdr entry))
	   (format "Passphrase for %s: " key-id)))))))

(defun epa-progress-callback-function (_context what _char current total
						handback)
  (let ((prompt (or handback
		    (format "Processing %s: " what))))
    ;; According to gnupg/doc/DETAIL: a "total" of 0 indicates that
    ;; the total amount is not known. The condition TOTAL && CUR ==
    ;; TOTAL may be used to detect the end of an operation.
    (if (> total 0)
	(if (= current total)
	    (message "%s...done" prompt)
	  (message "%s...%d%%" prompt
		   (floor (* 100.0 current) total)))
      (message "%s..." prompt))))

(defun epa-read-file-name (input)
  "Interactively read an output file name based on INPUT file name."
  (setq input (file-name-sans-extension (expand-file-name input)))
  (expand-file-name
   (read-file-name
    (concat "To file (default " (file-name-nondirectory input) ") ")
    (file-name-directory input)
    input)))

;;;###autoload
(defun epa-decrypt-file (decrypt-file &optional plain-file)
  "Decrypt DECRYPT-FILE into PLAIN-FILE.
If you do not specify PLAIN-FILE, this functions prompts for the value to use."
  (interactive
   (let* ((file (read-file-name "File to decrypt: "))
	  (plain (epa-read-file-name file)))
     (list file plain)))
  (or plain-file (setq plain-file (epa-read-file-name decrypt-file)))
  (setq decrypt-file (expand-file-name decrypt-file))
  (let ((context (epg-make-context epa-protocol)))
    (epg-context-set-passphrase-callback context
					 #'epa-passphrase-callback-function)
    (epg-context-set-progress-callback context
				       (cons
					#'epa-progress-callback-function
					(format "Decrypting %s..."
						(file-name-nondirectory decrypt-file))))
    (setf (epg-context-pinentry-mode context) epa-pinentry-mode)
    (message "Decrypting %s..." (file-name-nondirectory decrypt-file))
    (condition-case error
	(epg-decrypt-file context decrypt-file plain-file)
      (error
       (epa-display-error context)
       (signal (car error) (cdr error))))
    (message "Decrypting %s...wrote %s" (file-name-nondirectory decrypt-file)
	     (file-name-nondirectory plain-file))
    (if (epg-context-result-for context 'verify)
	(epa-display-info (epg-verify-result-to-string
			   (epg-context-result-for context 'verify))))))

;;;###autoload
(defun epa-verify-file (file)
  "Verify FILE."
  (interactive "fFile: ")
  (setq file (expand-file-name file))
  (let* ((context (epg-make-context epa-protocol))
	 (plain (if (equal (file-name-extension file) "sig")
		    (file-name-sans-extension file))))
    (epg-context-set-progress-callback context
				       (cons
					#'epa-progress-callback-function
					(format "Verifying %s..."
						(file-name-nondirectory file))))
    (message "Verifying %s..." (file-name-nondirectory file))
    (condition-case error
	(epg-verify-file context file plain)
      (error
       (epa-display-error context)
       (signal (car error) (cdr error))))
    (message "Verifying %s...done" (file-name-nondirectory file))
    (if (epg-context-result-for context 'verify)
	(epa-display-info (epg-verify-result-to-string
			   (epg-context-result-for context 'verify))))))

(defun epa--read-signature-type ()
  (let (type c)
    (while (null type)
      (message "Signature type (n,c,d,?) ")
      (setq c (read-char))
      (cond ((eq c ?c)
	     (setq type 'clear))
	    ((eq c ?d)
	     (setq type 'detached))
	    ((eq c ??)
	     (with-output-to-temp-buffer "*Help*"
	       (with-current-buffer standard-output
		 (insert "\
n - Create a normal signature
c - Create a cleartext signature
d - Create a detached signature
? - Show this help
"))))
	    (t
	     (setq type 'normal))))
    type))

;;;###autoload
(defun epa-sign-file (file signers mode)
  "Sign FILE by SIGNERS keys selected."
  (interactive
   (let ((verbose current-prefix-arg))
     (list (expand-file-name (read-file-name "File: "))
	   (if verbose
	       (epa-select-keys (epg-make-context epa-protocol)
				"Select keys for signing.
If no one is selected, default secret key is used.  "
				nil t))
	   (if verbose
	       (epa--read-signature-type)
	     'clear))))
  (let ((signature (concat file
			   (if (eq epa-protocol 'OpenPGP)
			       (if (or epa-armor
				       (not (memq mode
						  '(nil t normal detached))))
				   ".asc"
				 (if (memq mode '(t detached))
				     ".sig"
				   ".gpg"))
			     (if (memq mode '(t detached))
				 ".p7s"
			       ".p7m"))))
	(context (epg-make-context epa-protocol)))
    (setf (epg-context-armor context) epa-armor)
    (setf (epg-context-textmode context) epa-textmode)
    (setf (epg-context-signers context) signers)
    (epg-context-set-passphrase-callback context
					 #'epa-passphrase-callback-function)
    (epg-context-set-progress-callback context
				       (cons
					#'epa-progress-callback-function
					(format "Signing %s..."
						(file-name-nondirectory file))))
    (setf (epg-context-pinentry-mode context) epa-pinentry-mode)
    (message "Signing %s..." (file-name-nondirectory file))
    (condition-case error
	(epg-sign-file context file signature mode)
      (error
       (epa-display-error context)
       (signal (car error) (cdr error))))
    (message "Signing %s...wrote %s" (file-name-nondirectory file)
	     (file-name-nondirectory signature))))

;;;###autoload
(defun epa-encrypt-file (file recipients)
  "Encrypt FILE for RECIPIENTS."
  (interactive
   (list (expand-file-name (read-file-name "File: "))
	 (epa-select-keys (epg-make-context epa-protocol)
			  "Select recipients for encryption.
If no one is selected, symmetric encryption will be performed.  ")))
  (let ((cipher (concat file (if (eq epa-protocol 'OpenPGP)
				 (if epa-armor ".asc" ".gpg")
			       ".p7m")))
	(context (epg-make-context epa-protocol)))
    (setf (epg-context-armor context) epa-armor)
    (setf (epg-context-textmode context) epa-textmode)
    (epg-context-set-passphrase-callback context
					 #'epa-passphrase-callback-function)
    (epg-context-set-progress-callback context
				       (cons
					#'epa-progress-callback-function
					(format "Encrypting %s..."
						(file-name-nondirectory file))))
    (setf (epg-context-pinentry-mode context) epa-pinentry-mode)
    (message "Encrypting %s..." (file-name-nondirectory file))
    (condition-case error
	(epg-encrypt-file context file recipients cipher)
      (error
       (epa-display-error context)
       (signal (car error) (cdr error))))
    (message "Encrypting %s...wrote %s" (file-name-nondirectory file)
	     (file-name-nondirectory cipher))))

;;;###autoload
(defun epa-decrypt-region (start end &optional make-buffer-function)
  "Decrypt the current region between START and END.

If MAKE-BUFFER-FUNCTION is non-nil, call it to prepare an output buffer.
It should return that buffer.  If it copies the input, it should
delete the text now being decrypted.  It should leave point at the
proper place to insert the plaintext.

Be careful about using this command in Lisp programs!
Since this function operates on regions, it does some tricks such
as coding-system detection and unibyte/multibyte conversion.  If
you are sure how the data in the region should be treated, you
should consider using the string based counterpart
`epg-decrypt-string', or the file based counterpart
`epg-decrypt-file' instead.

For example:

\(let ((context (epg-make-context \\='OpenPGP)))
  (decode-coding-string
    (epg-decrypt-string context (buffer-substring start end))
    \\='utf-8))"
  (interactive "r")
  (save-excursion
    (let ((context (epg-make-context epa-protocol))
	  plain)
      (epg-context-set-passphrase-callback context
					   #'epa-passphrase-callback-function)
      (epg-context-set-progress-callback context
					 (cons
					  #'epa-progress-callback-function
					  "Decrypting..."))
      (setf (epg-context-pinentry-mode context) epa-pinentry-mode)
      (message "Decrypting...")
      (condition-case error
	  (setq plain (epg-decrypt-string context (buffer-substring start end)))
	(error
	 (epa-display-error context)
	 (signal (car error) (cdr error))))
      (message "Decrypting...done")
      (setq plain (epa--decode-coding-string
		   plain
		   (or coding-system-for-read
		       (get-text-property start 'epa-coding-system-used)
		       'undecided)))
      (if make-buffer-function
	  (with-current-buffer (funcall make-buffer-function)
	    (let ((inhibit-read-only t))
	      (insert plain)))
	(if (or (eq epa-replace-original-text t)
                (and epa-replace-original-text
                     (y-or-n-p "Replace the original text? ")))
	    (let ((inhibit-read-only t))
	      (delete-region start end)
	      (goto-char start)
	      (insert plain))
	  (with-output-to-temp-buffer "*Temp*"
	    (set-buffer standard-output)
	    (insert plain)
	    (epa-info-mode))))
      (if (epg-context-result-for context 'verify)
	  (epa-display-info (epg-verify-result-to-string
			     (epg-context-result-for context 'verify)))))))

(defun epa--find-coding-system-for-mime-charset (mime-charset)
  (if (featurep 'xemacs)
      (if (fboundp 'find-coding-system)
	  (find-coding-system mime-charset))
    ;; Find the first coding system which corresponds to MIME-CHARSET.
    (let ((pointer (coding-system-list)))
      (while (and pointer
		  (not (eq (coding-system-get (car pointer) 'mime-charset)
			   mime-charset)))
	(setq pointer (cdr pointer)))
      (car pointer))))

;;;###autoload
(defun epa-decrypt-armor-in-region (start end)
  "Decrypt OpenPGP armors in the current region between START and END.

Don't use this command in Lisp programs!
See the reason described in the `epa-decrypt-region' documentation."
  (declare (interactive-only t))
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char start)
      (let (armor-start armor-end)
	(while (re-search-forward "-----BEGIN PGP MESSAGE-----$" nil t)
	  (setq armor-start (match-beginning 0)
		armor-end (re-search-forward "^-----END PGP MESSAGE-----$"
					     nil t))
	  (unless armor-end
	    (error "Encryption armor beginning has no matching end"))
	  (goto-char armor-start)
	  (let ((coding-system-for-read
		 (or coding-system-for-read
		     (if (re-search-forward "^Charset: \\(.*\\)" armor-end t)
			 (epa--find-coding-system-for-mime-charset
			  (intern (downcase (match-string 1))))))))
	    (goto-char armor-end)
	    (epa-decrypt-region armor-start armor-end)))))))

;;;###autoload
(defun epa-verify-region (start end)
  "Verify the current region between START and END.

Don't use this command in Lisp programs!
Since this function operates on regions, it does some tricks such
as coding-system detection and unibyte/multibyte conversion.  If
you are sure how the data in the region should be treated, you
should consider using the string based counterpart
`epg-verify-string', or the file based counterpart
`epg-verify-file' instead.

For example:

\(let ((context (epg-make-context \\='OpenPGP)))
  (decode-coding-string
    (epg-verify-string context (buffer-substring start end))
    \\='utf-8))"
  (declare (interactive-only t))
  (interactive "r")
  (let ((context (epg-make-context epa-protocol))
	plain)
    (setf (epg-context-progress-callback context)
	  (cons
	   #'epa-progress-callback-function
	   "Verifying..."))
    (message "Verifying...")
    (condition-case error
	(setq plain (epg-verify-string
		     context
		     (epa--encode-coding-string
		      (buffer-substring start end)
		      (or coding-system-for-write
			  (get-text-property start 'epa-coding-system-used)))))
      (error
       (epa-display-error context)
       (signal (car error) (cdr error))))
    (message "Verifying...done")
    (setq plain (epa--decode-coding-string
		 plain
		 (or coding-system-for-read
		     (get-text-property start 'epa-coding-system-used)
		     'undecided)))
    (if (or (eq epa-replace-original-text t)
            (and epa-replace-original-text
                 (y-or-n-p "Replace the original text? ")))
	(let ((inhibit-read-only t)
	      buffer-read-only)
	  (delete-region start end)
	  (goto-char start)
	  (insert plain))
      (with-output-to-temp-buffer "*Temp*"
	(set-buffer standard-output)
	(insert plain)
	(epa-info-mode)))
    (if (epg-context-result-for context 'verify)
	(epa-display-info (epg-verify-result-to-string
			   (epg-context-result-for context 'verify))))))

;;;###autoload
(defun epa-verify-cleartext-in-region (start end)
  "Verify OpenPGP cleartext signed messages in the current region
between START and END.

Don't use this command in Lisp programs!
See the reason described in the `epa-verify-region' documentation."
  (declare (interactive-only t))
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char start)
      (let (cleartext-start cleartext-end)
	(while (re-search-forward "-----BEGIN PGP SIGNED MESSAGE-----$"
				  nil t)
	  (setq cleartext-start (match-beginning 0))
	  (unless (re-search-forward "^-----BEGIN PGP SIGNATURE-----$"
				     nil t)
	    (error "Invalid cleartext signed message"))
	  (setq cleartext-end (re-search-forward
			       "^-----END PGP SIGNATURE-----$"
			       nil t))
	  (unless cleartext-end
	    (error "No cleartext tail"))
	  (epa-verify-region cleartext-start cleartext-end))))))

(defalias 'epa--select-safe-coding-system
  (if (fboundp 'select-safe-coding-system)
      #'select-safe-coding-system
    (lambda (_from _to)
      buffer-file-coding-system)))

;;;###autoload
(defun epa-sign-region (start end signers mode)
  "Sign the current region between START and END by SIGNERS keys selected.

Don't use this command in Lisp programs!
Since this function operates on regions, it does some tricks such
as coding-system detection and unibyte/multibyte conversion.  If
you are sure how the data should be treated, you should consider
using the string based counterpart `epg-sign-string', or the file
based counterpart `epg-sign-file' instead.

For example:

\(let ((context (epg-make-context \\='OpenPGP)))
  (epg-sign-string
    context
    (encode-coding-string (buffer-substring start end) \\='utf-8)))"
  (declare (interactive-only t))
  (interactive
   (let ((verbose current-prefix-arg))
     (setq epa-last-coding-system-specified
	   (or coding-system-for-write
	       (epa--select-safe-coding-system
		(region-beginning) (region-end))))
     (list (region-beginning) (region-end)
	   (if verbose
	       (epa-select-keys (epg-make-context epa-protocol)
				"Select keys for signing.
If no one is selected, default secret key is used.  "
				nil t))
	   (if verbose
	       (epa--read-signature-type)
	     'clear))))
  (save-excursion
    (let ((context (epg-make-context epa-protocol))
	  signature)
      ;;(setf (epg-context-armor context) epa-armor)
      (setf (epg-context-armor context) t)
      ;;(setf (epg-context-textmode context) epa-textmode)
      (setf (epg-context-textmode context) t)
      (setf (epg-context-signers context) signers)
      (epg-context-set-passphrase-callback context
					   #'epa-passphrase-callback-function)
      (epg-context-set-progress-callback context
					 (cons
					  #'epa-progress-callback-function
					  "Signing..."))
      (setf (epg-context-pinentry-mode context) epa-pinentry-mode)
      (message "Signing...")
      (condition-case error
	  (setq signature (epg-sign-string context
					   (epa--encode-coding-string
					    (buffer-substring start end)
					    epa-last-coding-system-specified)
					   mode))
	(error
	 (epa-display-error context)
	 (signal (car error) (cdr error))))
      (message "Signing...done")
      (delete-region start end)
      (goto-char start)
      (add-text-properties (point)
			   (progn
			     (insert (epa--decode-coding-string
				      signature
				      (or coding-system-for-read
					  epa-last-coding-system-specified)))
			     (point))
			   (list 'epa-coding-system-used
				 epa-last-coding-system-specified
				 'front-sticky nil
				 'rear-nonsticky t
				 'start-open t
				 'end-open t)))))

(defalias 'epa--derived-mode-p
  (if (fboundp 'derived-mode-p)
      #'derived-mode-p
    (lambda (&rest modes)
      "Non-nil if the current major mode is derived from one of MODES.
Uses the `derived-mode-parent' property of the symbol to trace backwards."
      (let ((parent major-mode))
        (while (and (not (memq parent modes))
                    (setq parent (get parent 'derived-mode-parent))))
        parent))))

;;;###autoload
(defun epa-encrypt-region (start end recipients sign signers)
  "Encrypt the current region between START and END for RECIPIENTS.

Don't use this command in Lisp programs!
Since this function operates on regions, it does some tricks such
as coding-system detection and unibyte/multibyte conversion.  If
you are sure how the data should be treated, you should consider
using the string based counterpart `epg-encrypt-string', or the
file based counterpart `epg-encrypt-file' instead.

For example:

\(let ((context (epg-make-context \\='OpenPGP)))
  (epg-encrypt-string
    context
    (encode-coding-string (buffer-substring start end) \\='utf-8)
    nil))"
  (declare (interactive-only t))
  (interactive
   (let ((verbose current-prefix-arg)
	 (context (epg-make-context epa-protocol))
	 sign)
     (setq epa-last-coding-system-specified
	   (or coding-system-for-write
	       (epa--select-safe-coding-system
		(region-beginning) (region-end))))
     (list (region-beginning) (region-end)
	   (epa-select-keys context
			    "Select recipients for encryption.
If no one is selected, symmetric encryption will be performed.  ")
	   (setq sign (if verbose (y-or-n-p "Sign? ")))
	   (if sign
	       (epa-select-keys context
				"Select keys for signing.  ")))))
  (save-excursion
    (let ((context (epg-make-context epa-protocol))
	  cipher)
      ;;(setf (epg-context-armor context) epa-armor)
      (setf (epg-context-armor context) t)
      ;;(setf (epg-context-textmode context) epa-textmode)
      (setf (epg-context-textmode context) t)
      (if sign
	  (setf (epg-context-signers context) signers))
      (epg-context-set-passphrase-callback context
					   #'epa-passphrase-callback-function)
      (epg-context-set-progress-callback context
					 (cons
					  #'epa-progress-callback-function
					  "Encrypting..."))
      (setf (epg-context-pinentry-mode context) epa-pinentry-mode)
      (message "Encrypting...")
      (condition-case error
	  (setq cipher (epg-encrypt-string context
					   (epa--encode-coding-string
					    (buffer-substring start end)
					    epa-last-coding-system-specified)
					   recipients
					   sign))
	(error
	 (epa-display-error context)
	 (signal (car error) (cdr error))))
      (message "Encrypting...done")
      (delete-region start end)
      (goto-char start)
      (add-text-properties (point)
			   (progn
			     (insert cipher)
			     (point))
			   (list 'epa-coding-system-used
				 epa-last-coding-system-specified
				 'front-sticky nil
				 'rear-nonsticky t
				 'start-open t
				 'end-open t)))))

;;;###autoload
(defun epa-delete-keys (keys &optional allow-secret)
  "Delete selected KEYS."
  (interactive
   (let ((keys (epa--marked-keys)))
     (unless keys
       (error "No keys selected"))
     (list keys
	   (eq (nth 1 epa-list-keys-arguments) t))))
  (let ((context (epg-make-context epa-protocol)))
    (message "Deleting...")
    (condition-case error
	(epg-delete-keys context keys allow-secret)
      (error
       (epa-display-error context)
       (signal (car error) (cdr error))))
    (message "Deleting...done")
    (apply #'epa--list-keys epa-list-keys-arguments)))

;;;###autoload
(defun epa-import-keys (file)
  "Import keys from FILE."
  (interactive "fFile: ")
  (setq file (expand-file-name file))
  (let ((context (epg-make-context epa-protocol)))
    (message "Importing %s..." (file-name-nondirectory file))
    (condition-case nil
	(progn
	  (epg-import-keys-from-file context file)
	  (message "Importing %s...done" (file-name-nondirectory file)))
      (error
       (epa-display-error context)
       (message "Importing %s...failed" (file-name-nondirectory file))))
    (if (epg-context-result-for context 'import)
	(epa-display-info (epg-import-result-to-string
			   (epg-context-result-for context 'import))))
    ;; FIXME: Why not use the (otherwise unused) epa--derived-mode-p?
    (if (eq major-mode 'epa-key-list-mode)
	(apply #'epa--list-keys epa-list-keys-arguments))))

;;;###autoload
(defun epa-import-keys-region (start end)
  "Import keys from the region."
  (interactive "r")
  (let ((context (epg-make-context epa-protocol)))
    (message "Importing...")
    (condition-case nil
	(progn
	  (epg-import-keys-from-string context (buffer-substring start end))
	  (message "Importing...done"))
      (error
       (epa-display-error context)
       (message "Importing...failed")))
    (if (epg-context-result-for context 'import)
	(epa-display-info (epg-import-result-to-string
			   (epg-context-result-for context 'import))))))

;;;###autoload
(defun epa-import-armor-in-region (start end)
  "Import keys in the OpenPGP armor format in the current region
between START and END."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char start)
      (let (armor-start armor-end)
	(while (re-search-forward
		"-----BEGIN \\(PGP \\(PUBLIC\\|PRIVATE\\) KEY BLOCK\\)-----$"
		nil t)
	  (setq armor-start (match-beginning 0)
		armor-end (re-search-forward
			   (concat "^-----END " (match-string 1) "-----$")
			   nil t))
	  (unless armor-end
	    (error "No armor tail"))
	  (epa-import-keys-region armor-start armor-end))))))

;;;###autoload
(defun epa-export-keys (keys file)
  "Export selected KEYS to FILE."
  (interactive
   (let ((keys (epa--marked-keys))
	 default-name)
     (unless keys
       (error "No keys selected"))
     (setq default-name
	   (expand-file-name
	    (concat (epg-sub-key-id (car (epg-key-sub-key-list (car keys))))
		    (if epa-armor ".asc" ".gpg"))
	    default-directory))
     (list keys
	   (expand-file-name
	    (read-file-name
	     (concat "To file (default "
		     (file-name-nondirectory default-name)
		     ") ")
	     (file-name-directory default-name)
	     default-name)))))
  (let ((context (epg-make-context epa-protocol)))
    (setf (epg-context-armor context) epa-armor)
    (message "Exporting to %s..." (file-name-nondirectory file))
    (condition-case error
	(epg-export-keys-to-file context keys file)
      (error
       (epa-display-error context)
       (signal (car error) (cdr error))))
    (message "Exporting to %s...done" (file-name-nondirectory file))))

;;;###autoload
(defun epa-insert-keys (keys)
  "Insert selected KEYS after the point."
  (interactive
   (list (epa-select-keys (epg-make-context epa-protocol)
			  "Select keys to export.
If no one is selected, default public key is exported.  ")))
  (let ((context (epg-make-context epa-protocol)))
    ;;(setf (epg-context-armor context) epa-armor)
    (setf (epg-context-armor context) t)
    (condition-case error
	(insert (epg-export-keys-to-string context keys))
      (error
       (epa-display-error context)
       (signal (car error) (cdr error))))))

;; (defun epa-sign-keys (keys &optional local)
;;   "Sign selected KEYS.
;; If a prefix-arg is specified, the signature is marked as non exportable.

;; Don't use this command in Lisp programs!"
;;   (declare (interactive-only t))
;;   (interactive
;;    (let ((keys (epa--marked-keys)))
;;      (unless keys
;;        (error "No keys selected"))
;;      (list keys current-prefix-arg)))
;;   (let ((context (epg-make-context epa-protocol)))
;;     (epg-context-set-passphrase-callback context
;; 	                                    #'epa-passphrase-callback-function)
;;     (epg-context-set-progress-callback context
;; 	                                  (cons
;; 	                                    #'epa-progress-callback-function
;; 	                                    "Signing keys..."))
;;     (setf (epg-context-pinentry-mode context) epa-pinentry-mode)
;;     (message "Signing keys...")
;;     (epg-sign-keys context keys local)
;;     (message "Signing keys...done")))
;; (make-obsolete 'epa-sign-keys "Do not use.")

(provide 'epa)

;;; epa.el ends here
