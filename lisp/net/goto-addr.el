;;; goto-addr.el --- click to browse URL or to send to e-mail address

;; Copyright (C) 1995 Free Software Foundation, Inc.

;; Author: Eric Ding <ericding@mit.edu>
;; Maintainer: Eric Ding <ericding@mit.edu>
;; Created: 15 Aug 1995
;; Keywords: mh-e, www, mouse, mail

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

;; This package allows you to click or hit a key sequence while on a
;; URL or e-mail address, and either load the URL into a browser of
;; your choice using the browse-url package, or if it's an e-mail
;; address, to send an e-mail to that address.  By default, we bind to
;; the [mouse-2] and the [C-c return] key sequences.

;; INSTALLATION
;;
;; To use goto-address in a particular mode (for example, while
;; reading mail in mh-e), add something like this in your .emacs file:
;;
;; (add-hook 'mh-show-mode-hook 'goto-address)
;;
;; The mouse click method is bound to [mouse-2] on highlighted URL's or
;; e-mail addresses only; it functions normally everywhere else.  To bind
;; another mouse click to the function, add the following to your .emacs
;; (for example):
;;
;; (setq goto-address-highlight-keymap
;;   (let ((m (make-sparse-keymap)))
;;     (define-key m [S-mouse-2] 'goto-address-at-mouse)
;;     m))
;;

;; BUG REPORTS
;;
;; Please send bug reports to me at ericding@mit.edu.

;; Known bugs/features:
;; * goto-address-mail-regexp only catches foo@bar.org style addressing,
;;   not stuff like X.400 addresses, etc.
;; * regexp also catches Message-Id line, since it is in the format of
;;   an Internet e-mail address (like Compuserve addresses)
;; * If show buffer is fontified after goto-address-fontify is run
;;   (say, using font-lock-fontify-buffer), then font-lock face will
;;   override goto-address faces.

;;; Code:

(require 'browse-url)

(defgroup goto-address nil
  "Click to browse URL or to send to e-mail address."
  :group 'mouse
  :group 'hypermedia)


;;; I don't expect users to want fontify'ing without highlighting.
(defcustom goto-address-fontify-p t
  "*If t, URL's and e-mail addresses in buffer are fontified.
But only if `goto-address-highlight-p' is also non-nil."
  :type 'boolean
  :group 'goto-address)

(defcustom goto-address-highlight-p t
  "*If t, URL's and e-mail addresses in buffer are highlighted."
  :type 'boolean
  :group 'goto-address)

(defcustom goto-address-fontify-maximum-size 30000
  "*Maximum size of file in which to fontify and/or highlight URL's."
  :type 'integer
  :group 'goto-address)

(defvar goto-address-mail-regexp
  "[-a-zA-Z0-9._]+@\\([-a-zA-z0-9_]+\\.\\)+[a-zA-Z0-9]+"
  "A regular expression probably matching an e-mail address.")

(defvar goto-address-url-regexp
  (concat "\\b\\(s?https?\\|ftp\\|file\\|gopher\\|news\\|"
	  "telnet\\|wais\\):\\(//[-a-zA-Z0-9_.]+:"
	  "[0-9]*\\)?[-a-zA-Z0-9_=?#$@~`%&*+|\\/.,]*"
	  "[-a-zA-Z0-9_=#$@~`%&*+|\\/]")
  "A regular expression probably matching a URL.")

(defvar goto-address-highlight-keymap
  (let ((m (make-sparse-keymap)))
    (define-key m [mouse-2] 'goto-address-at-mouse)
    (define-key m "\C-c\r" 'goto-address-at-point)
    m)
  "keymap to hold goto-addr's mouse key defs under highlighted URLs.")

(defcustom goto-address-url-face 'bold
  "*Face to use for URLs."
  :type 'face
  :group 'goto-address)

(defcustom goto-address-url-mouse-face 'highlight
  "*Face to use for URLs when the mouse is on them."
  :type 'face
  :group 'goto-address)

(defcustom goto-address-mail-face 'italic
  "*Face to use for e-mail addresses."
  :type 'face
  :group 'goto-address)

(defcustom goto-address-mail-mouse-face 'secondary-selection
  "*Face to use for e-mail addresses when the mouse is on them."
  :type 'face
  :group 'goto-address)

(defun goto-address-fontify ()
  "Fontify the URL's and e-mail addresses in the current buffer.
This function implements `goto-address-highlight-p'
and `goto-address-fontify-p'."
  (save-excursion
    (let ((inhibit-read-only t)
	  (inhibit-point-motion-hooks t)
	  (modified (buffer-modified-p)))
      (goto-char (point-min))
      (if (< (- (point-max) (point)) goto-address-fontify-maximum-size)
	  (progn
	    (while (re-search-forward goto-address-url-regexp nil t)
              (let* ((s (match-beginning 0))
                     (e (match-end 0))
                     (this-overlay (make-overlay s e)))
		(and goto-address-fontify-p
                     (overlay-put this-overlay 'face goto-address-url-face))
		(overlay-put this-overlay
                             'mouse-face goto-address-url-mouse-face)
		(overlay-put this-overlay
			     'help-echo "mouse-2: follow URL")
		(overlay-put this-overlay
                             'local-map goto-address-highlight-keymap)))
	    (goto-char (point-min))
	    (while (re-search-forward goto-address-mail-regexp nil t)
              (let* ((s (match-beginning 0))
                     (e (match-end 0))
                     (this-overlay (make-overlay s e)))
		(and goto-address-fontify-p
                     (overlay-put this-overlay 'face goto-address-mail-face))
                (overlay-put this-overlay 'mouse-face
                             goto-address-mail-mouse-face)
		(overlay-put this-overlay
			     'help-echo "mouse-2: follow URL")
                (overlay-put this-overlay
                             'local-map goto-address-highlight-keymap)))))
      (and (buffer-modified-p)
	   (not modified)
	   (set-buffer-modified-p nil)))))

;;; code to find and goto addresses; much of this has been blatantly
;;; snarfed from browse-url.el

;;;###autoload
(defun goto-address-at-mouse (event)
  "Send to the e-mail address or load the URL clicked with the mouse.
Send mail to address at position of mouse click.  See documentation for
`goto-address-find-address-at-point'.  If no address is found
there, then load the URL at or before the position of the mouse click."
  (interactive "e")
  (save-excursion
    (let ((posn (event-start event)))
      (set-buffer (window-buffer (posn-window posn)))
      (goto-char (posn-point posn))
      (let ((address
	     (save-excursion (goto-address-find-address-at-point))))
	(if (string-equal address "")
	    (let ((url (browse-url-url-at-point)))
	      (if (string-equal url "")
		  (error "No e-mail address or URL found")
		(browse-url url)))
            (compose-mail address))))))

;;;###autoload
(defun goto-address-at-point ()
  "Send to the e-mail address or load the URL at point.
Send mail to address at point.  See documentation for
`goto-address-find-address-at-point'.  If no address is found
there, then load the URL at or before point."
  (interactive)
  (save-excursion
    (let ((address (save-excursion (goto-address-find-address-at-point))))
      (if (string-equal address "")
	  (let ((url (browse-url-url-at-point)))
	    (if (string-equal url "")
		(error "No e-mail address or URL found")
	      (browse-url url)))
          (compose-mail address)))))

(defun goto-address-find-address-at-point ()
  "Find e-mail address around or before point.
Then search backwards to beginning of line for the start of an e-mail
address.  If no e-mail address found, return the empty string."
  (let ((bol (save-excursion (beginning-of-line) (point))))
    (re-search-backward "[^-_A-z0-9.@]" bol 'lim)
    (if (or (looking-at goto-address-mail-regexp)  ; already at start
	    (let ((eol (save-excursion (end-of-line) (point))))
	      (and (re-search-forward goto-address-mail-regexp eol 'lim)
		   (goto-char (match-beginning 0)))))
	(buffer-substring (match-beginning 0) (match-end 0))
      "")))m

;;;###autoload
(defun goto-address ()
  "Sets up goto-address functionality in the current buffer.
Allows user to use mouse/keyboard command to click to go to a URL
or to send e-mail.
By default, goto-address binds to mouse-2 and C-c RET.

Also fontifies the buffer appropriately (see `goto-address-fontify-p' and
`goto-address-highlight-p' for more information)."
  (interactive)
  (if goto-address-highlight-p
      (goto-address-fontify)))

(provide 'goto-addr)

;;; goto-addr.el ends here.
