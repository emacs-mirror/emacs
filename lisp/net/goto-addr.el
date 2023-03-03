;;; goto-addr.el --- click to browse URL or to send to e-mail address  -*- lexical-binding: t; -*-

;; Copyright (C) 1995, 2000-2023 Free Software Foundation, Inc.

;; Author: Eric Ding <ericding@alum.mit.edu>
;; Maintainer: emacs-devel@gnu.org
;; Created: 15 Aug 1995
;; Keywords: www, mouse, mail

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

;; This package allows you to click or hit a key sequence while on a
;; URL or e-mail address, and either load the URL into a browser of
;; your choice using the browse-url package, or if it's an e-mail
;; address, to send an e-mail to that address.  By default, we bind to
;; the [mouse-2] and the [C-c return] key sequences.

;; INSTALLATION
;;
;; To use goto-address in a particular mode (this example uses
;; the fictional rich-text-mode), add this to your init file:
;;
;; (add-hook 'rich-text-mode-hook 'goto-address)
;;
;; The mouse click method is bound to [mouse-2] on highlighted URLs or
;; e-mail addresses only; it functions normally everywhere else.  To bind
;; another mouse click to the function, add the following to your .emacs
;; (for example):
;;
;; (setq goto-address-highlight-keymap
;;   (let ((m (make-sparse-keymap)))
;;     (define-key m [S-mouse-2] 'goto-address-at-point)
;;     m))
;;

;; Known bugs/features:
;; * goto-address-mail-regexp only catches foo@bar.org style addressing,
;;   not stuff like X.400 addresses, etc.
;; * regexp also catches Message-Id line, since it is in the format of
;;   an Internet e-mail address (like Compuserve addresses)
;; * If the buffer is fontified after goto-address-fontify is run
;;   (say, using font-lock-fontify-buffer), then font-lock faces will
;;   override goto-address faces.

;;; Code:

(require 'seq)
(require 'thingatpt)
(autoload 'browse-url-url-at-point "browse-url")

(defgroup goto-address nil
  "Click to browse URL or to send to e-mail address."
  :group 'mouse
  :group 'comm)


;; I don't expect users to want fontify'ing without highlighting.
(defcustom goto-address-fontify-p t
  "Non-nil means URLs and e-mail addresses in buffer are fontified.
But only if `goto-address-highlight-p' is also non-nil."
  :type 'boolean)

(defcustom goto-address-highlight-p t
  "Non-nil means URLs and e-mail addresses in buffer are highlighted."
  :type 'boolean)

(defcustom goto-address-fontify-maximum-size 30000
  "Maximum size of file in which to fontify and/or highlight URLs.
A value of t means there is no limit--fontify regardless of the size."
  :type '(choice (integer :tag "Maximum size") (const :tag "No limit" t)))

(defvar goto-address-mail-regexp
  ;; Actually pretty much any char could appear in the username part.  -stef
  "[-a-zA-Z0-9=._+]+@\\([-a-zA-Z0-9_]+\\.\\)+[a-zA-Z0-9]+"
  "A regular expression probably matching an e-mail address.")

(defvar goto-address-uri-schemes-ignored
  ;; By default we exclude `mailto:' (email addresses are matched
  ;; by `goto-address-mail-regexp') and also `data:', as it is not
  ;; terribly useful to follow those URIs, and leaving them causes
  ;; `use Data::Dumper;' to be fontified oddly in Perl files.
  '("mailto:" "data:")
  "List of URI schemes to exclude from `goto-address-uri-schemes'.

Customizations to this variable made after goto-addr is loaded
will have no effect.")

(defvar goto-address-uri-schemes
  ;; We use `thing-at-point-uri-schemes', with a few exclusions,
  ;; as listed in `goto-address-uri-schemes-ignored'.
  (seq-reduce (lambda (accum elt) (delete elt accum))
              goto-address-uri-schemes-ignored
              (copy-sequence thing-at-point-uri-schemes))
  "List of URI schemes matched by `goto-address-url-regexp'.

Customizations to this variable made after goto-addr is loaded
will have no effect.")

(defvar goto-address-url-regexp
  (concat "\\<"
          (regexp-opt goto-address-uri-schemes t)
          thing-at-point-url-path-regexp)
  "A regular expression probably matching a URL.")

(defvar goto-address-highlight-keymap
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "<mouse-2>") #'goto-address-at-point)
    (define-key m (kbd "C-c RET") #'goto-address-at-point)
    m)
  "Keymap to hold goto-addr's mouse key defs under highlighted URLs.")

(defun goto-address-context-menu (menu click)
  "Populate MENU with `goto-address' commands at CLICK."
  (when (mouse-posn-property (event-start click) 'goto-address)
    (define-key menu [goto-address-separator] menu-bar-separator)
    (define-key menu [goto-address-at-mouse]
      '(menu-item "Follow Link" goto-address-at-mouse
                  :help "Follow a link where you click")))
  menu)

(defcustom goto-address-url-face 'link
  "Face to use for URLs."
  :type 'face)

(defcustom goto-address-url-mouse-face 'highlight
  "Face to use for URLs when the mouse is on them."
  :type 'face)

(defcustom goto-address-mail-face 'italic
  "Face to use for e-mail addresses."
  :type 'face)

(defcustom goto-address-mail-mouse-face 'secondary-selection
  "Face to use for e-mail addresses when the mouse is on them."
  :type 'face)

(defun goto-address-unfontify (start end)
  "Remove `goto-address' fontification from the given region."
  (dolist (overlay (overlays-in start end))
    (if (overlay-get overlay 'goto-address)
	(delete-overlay overlay))))

(defvar goto-address-prog-mode)

(defun goto-address-fontify (&optional start end)
  "Fontify the URLs and e-mail addresses in the current buffer.
This function implements `goto-address-highlight-p'
and `goto-address-fontify-p'."
  ;; Clean up from any previous go.
  (goto-address-unfontify (or start (point-min)) (or end (point-max)))
  (save-excursion
    (goto-char (or start (point-min)))
    (when (or (eq t goto-address-fontify-maximum-size)
	      (< (- (or end (point-max)) (point))
                 goto-address-fontify-maximum-size))
      (while (re-search-forward goto-address-url-regexp end t)
	(let* ((s (match-beginning 0))
	       (e (match-end 0))
	       this-overlay)
	  (when (or (not goto-address-prog-mode)
		    ;; This tests for both comment and string
		    ;; syntax.
		    (nth 8 (syntax-ppss)))
	    (setq this-overlay (make-overlay s e))
	    (and goto-address-fontify-p
		 (overlay-put this-overlay 'face goto-address-url-face))
	    (overlay-put this-overlay 'evaporate t)
	    (overlay-put this-overlay
			 'mouse-face goto-address-url-mouse-face)
	    (overlay-put this-overlay 'follow-link t)
	    (overlay-put this-overlay
			 'help-echo "mouse-2, C-c RET: follow URL")
	    (overlay-put this-overlay
			 'keymap goto-address-highlight-keymap)
	    (overlay-put this-overlay 'goto-address t))))
      (goto-char (or start (point-min)))
      (while (re-search-forward goto-address-mail-regexp end t)
	(let* ((s (match-beginning 0))
	       (e (match-end 0))
	       this-overlay)
	  (when (or (not goto-address-prog-mode)
		    ;; This tests for both comment and string
		    ;; syntax.
		    (nth 8 (syntax-ppss)))
	    (setq this-overlay (make-overlay s e))
	    (and goto-address-fontify-p
		 (overlay-put this-overlay 'face goto-address-mail-face))
	    (overlay-put this-overlay 'evaporate t)
	    (overlay-put this-overlay 'mouse-face
			 goto-address-mail-mouse-face)
	    (overlay-put this-overlay 'follow-link t)
	    (overlay-put this-overlay
			 'help-echo "mouse-2, C-c RET: mail this address")
	    (overlay-put this-overlay
			 'keymap goto-address-highlight-keymap)
	    (overlay-put this-overlay 'goto-address t)))))))

(defun goto-address-fontify-region (start end)
  "Fontify URLs and e-mail addresses in the given region."
  (save-excursion
    (let ((beg-line (progn (goto-char start) (line-beginning-position)))
          (end-line (progn (goto-char end) (line-end-position))))
      (goto-address-fontify beg-line end-line))))

;; code to find and goto addresses; much of this has been blatantly
;; snarfed from browse-url.el

;;;###autoload
(defun goto-address-at-point (&optional event)
  "Compose a new message to the e-mail address or open URL at point.

Compose message to address at point.  See documentation for
`goto-address-find-address-at-point'.

If no e-mail address is found at point, open the URL at or before
point using `browse-url'.  With a prefix argument, open the URL
using `browse-url-secondary-browser-function' instead."
  (interactive (list last-input-event))
  (save-excursion
    (if event (posn-set-point (event-end event)))
    (let ((address (save-excursion (goto-address-find-address-at-point))))
      (if (and address
               (save-excursion
                 (goto-char (previous-single-char-property-change
                             (point) 'goto-address nil
                             (line-beginning-position)))
                 (not (looking-at goto-address-url-regexp))))
          (compose-mail address)
        (if-let ((url (browse-url-url-at-point)))
            (browse-url-button-open-url url)
          (error "No e-mail address or URL found"))))))

(defun goto-address-find-address-at-point ()
  "Find e-mail address around or before point.
Then search backwards to beginning of line for the start of an e-mail
address.  If no e-mail address found, return nil."
  (re-search-backward "[^-_A-Za-z0-9.@]" (line-beginning-position) 'lim)
  (if (or (looking-at goto-address-mail-regexp)	; already at start
	  (and (re-search-forward goto-address-mail-regexp
				  (line-end-position) 'lim)
	       (goto-char (match-beginning 0))))
      (match-string-no-properties 0)))

(defun goto-address-at-mouse (click)
  "Send to the e-mail address or load the URL at mouse click."
  (interactive "e")
  (goto-address-at-point click))

;;;###autoload
(defun goto-address ()
  "Sets up goto-address functionality in the current buffer.
Allows user to use mouse/keyboard command to click to go to a URL
or to send e-mail.
By default, goto-address binds `goto-address-at-point' to mouse-2 and C-c RET
only on URLs and e-mail addresses.

Also fontifies the buffer appropriately (see `goto-address-fontify-p' and
`goto-address-highlight-p' for more information)."
  (interactive)
  (if goto-address-highlight-p
      (goto-address-fontify)))
;;;###autoload(put 'goto-address 'safe-local-eval-function t)

;;;###autoload
(define-minor-mode goto-address-mode
  "Minor mode to buttonize URLs and e-mail addresses in the current buffer."
  :lighter ""
  (cond
   (goto-address-mode
    (jit-lock-register #'goto-address-fontify-region)
    (add-hook 'context-menu-functions 'goto-address-context-menu 10 t))
   (t
    (jit-lock-unregister #'goto-address-fontify-region)
    (save-restriction
      (widen)
      (goto-address-unfontify (point-min) (point-max)))
    (remove-hook 'context-menu-functions 'goto-address-context-menu t))))

(defun goto-addr-mode--turn-on ()
  (when (not goto-address-mode)
    (goto-address-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-goto-address-mode
  goto-address-mode goto-addr-mode--turn-on
  :version "28.1")

;;;###autoload
(define-minor-mode goto-address-prog-mode
  "Like `goto-address-mode', but only for comments and strings."
  :lighter ""
  (if goto-address-prog-mode
      (jit-lock-register #'goto-address-fontify-region)
    (jit-lock-unregister #'goto-address-fontify-region)
    (save-restriction
      (widen)
      (goto-address-unfontify (point-min) (point-max)))))

(provide 'goto-addr)

;;; goto-addr.el ends here
