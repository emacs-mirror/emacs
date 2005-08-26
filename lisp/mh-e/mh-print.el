;;; mh-print.el --- MH-E printing support

;; Copyright (C) 2003, 2004 Free Software Foundation, Inc.

;; Author: Jeffrey C Honig <jch@honig.net>
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
;;	Pp	Print to lpr              |   Default inline settings
;;      Pf      Print to file             |   Generate a postscript file
;;	Ps	Print show buffer         |   Fails if no show buffer
;;
;;	PA	Toggle inline/attachments
;;	PC	Toggle color
;;	PF	Toggle faces

;;; Change Log:

;;; Code:

(eval-when-compile (require 'mh-acros))
(mh-require-cl)
(require 'ps-print)
(require 'mh-utils)
(require 'mh-funcs)
(eval-when-compile (require 'mh-seq))

(defvar mh-ps-print-mime nil
  "Control printing of MIME parts.
The three possible states are:
  1. nil to not print inline parts
  2. t to print inline parts
  3. non-zero to print inline parts and attachments")

(defvar mh-ps-print-color-option ps-print-color-p
  "MH-E's version of `\\[ps-print-color-p]'.")

(defvar mh-ps-print-func 'ps-spool-buffer-with-faces
  "Function to use to spool a buffer.
Sensible choices are the functions `ps-spool-buffer' and
`ps-spool-buffer-with-faces'.")

;; XXX - If buffer is already being displayed, use that buffer
;; XXX - What about showing MIME content?
;; XXX - Default print buffer is bogus
(defun mh-ps-spool-buffer (buffer)
  "Send BUFFER to printer queue."
  (message (format "mh-ps-spool-buffer %s" buffer))
  (save-excursion
    (set-buffer buffer)
    (let ((ps-print-color-p mh-ps-print-color-option)
      (ps-left-header
       (list
	(concat "("
		(mh-get-header-field "Subject:") ")")
	(concat "("
		(mh-get-header-field "From:") ")")))
      (ps-right-header
       (list
	"/pagenumberstring load"
	(concat "("
		(mh-get-header-field "Date:") ")"))))
    (funcall mh-ps-print-func))))

(defun mh-ps-spool-a-msg (msg buffer)
  "Print MSG.
First the message is decoded in BUFFER before the results are sent to the
printer."
  (message (format "mh-ps-spool-a-msg msg %s buffer %s"
		   msg buffer))
  (let ((mh-show-buffer mh-show-buffer)
	(folder mh-current-folder)
        ;; The following is commented out because
        ;; `clean-message-header-flag' isn't used anywhere. I
        ;; commented rather than deleted in case somebody had some
        ;; future plans for it. --SY.
	;(clean-message-header-flag mh-clean-message-header-flag)
        )
    (unwind-protect
	(progn
	  (setq mh-show-buffer buffer)
	  (save-excursion
	    ;;
	    ;; XXX - Use setting of mh-ps-print-mime
	    ;;
	    (mh-display-msg msg folder)
	    (mh-ps-spool-buffer mh-show-buffer)
      (kill-buffer mh-show-buffer))))))

;;;###mh-autoload
(defun mh-ps-print-msg (range)
  "Print the messages in RANGE.

Check the documentation of `mh-interactive-range' to see how RANGE is read in
interactive use."
  (interactive (list (mh-interactive-range "Print")))
  (message (format "mh-ps-print-msg range %s keys %s"
		    range (this-command-keys)))
  (mh-iterate-on-range msg range
    (let ((buffer (get-buffer-create mh-temp-buffer)))
      (unwind-protect
	  (mh-ps-spool-a-msg msg buffer)
	(kill-buffer buffer)))
    (mh-notate nil mh-note-printed mh-cmd-note))
  (ps-despool nil))

(defun mh-ps-print-preprint (prefix-arg)
  "Replacement for `ps-print-preprint'.
The original function does not handle the fact that MH folders are directories
nicely, when generating the default file name. This function works around
that. The function is passed the interactive PREFIX-ARG."
  (let ((buffer-file-name (format "/tmp/%s" (substring (buffer-name) 1))))
    (ps-print-preprint prefix-arg)))

;;;###mh-autoload
(defun mh-ps-print-msg-file (file range)
  "Print to FILE the messages in RANGE.

Check the documentation of `mh-interactive-range' to see how RANGE is read in
interactive use."
  (interactive (list
		(mh-ps-print-preprint 1)
		(mh-interactive-range "Print")))
  (mh-iterate-on-range msg range
    (let ((buffer (get-buffer-create mh-temp-buffer)))
      (unwind-protect
	  (mh-ps-spool-a-msg msg buffer)
	(kill-buffer buffer)))
    (mh-notate nil mh-note-printed mh-cmd-note))
  (ps-despool file))

;;;###mh-autoload
(defun mh-ps-print-msg-show (file)
  "Print current show buffer to FILE."
  (interactive (list (mh-ps-print-preprint current-prefix-arg)))
  (message (format "mh-ps-print-msg-show file %s keys %s mh-show-buffer %s"
		   file (this-command-keys) mh-show-buffer))
  (let ((msg (mh-get-msg-num t))
        (folder mh-current-folder)
        (show-buffer mh-show-buffer)
        (show-window (get-buffer-window mh-show-buffer)))
    (if (and show-buffer show-window)
	(mh-in-show-buffer (show-buffer)
	  (if (equal (mh-msg-filename msg folder) buffer-file-name)
	      (progn
		(mh-ps-spool-buffer show-buffer)
		(ps-despool file))
	    (message "Current message is not being shown(1).")))
      (message "Current message is not being shown(2)."))))

;;;###mh-autoload
(defun mh-ps-print-toggle-faces ()
 "Toggle whether printing is done with faces or not."
 (interactive)
 (if (eq mh-ps-print-func 'ps-spool-buffer-with-faces)
     (progn
       (setq mh-ps-print-func 'ps-spool-buffer)
       (message "Printing without faces"))
   (setq mh-ps-print-func 'ps-spool-buffer-with-faces)
   (message "Printing with faces")))

;;;###mh-autoload
(defun mh-ps-print-toggle-color ()
  "Toggle whether color is used in printing messages."
 (interactive)
 (if (eq mh-ps-print-color-option nil)
     (progn
       (setq mh-ps-print-color-option 'black-white)
       (message "Colors will be printed as black & white."))
   (if (eq mh-ps-print-color-option 'black-white)
       (progn
	 (setq mh-ps-print-color-option t)
	 (message "Colors will be printed."))
     (setq mh-ps-print-color-option nil)
     (message "Colors will not be printed."))))

;;; XXX: Check option 3. Documentation doesn't sound right.
;;;###mh-autoload
(defun mh-ps-print-toggle-mime ()
  "Cycle through available choices on how MIME parts should be printed.
The available settings are:
  1. Print only inline MIME parts.
  2. Print all MIME parts.
  3. Print no MIME parts."
  (interactive)
  (if (eq mh-ps-print-mime nil)
      (progn
        (setq mh-ps-print-mime t)
        (message "Inline parts will be printed, attachments will not be printed."))
    (if (eq mh-ps-print-mime t)
        (progn
          (setq mh-ps-print-mime 1)
          (message "Both Inline parts and attachments will be printed."))
      (setq mh-ps-print-mime nil)
      (message "Neither inline parts nor attachments will be printed."))))

;;; Old non-PS based printing
;;;###mh-autoload
(defun mh-print-msg (range)
  "Print RANGE on printer.

Check the documentation of `mh-interactive-range' to see how RANGE is read in
interactive use.

The variable `mh-lpr-command-format' is used to generate the print command.
The messages are formatted by mhl. See the variable `mhl-formfile'."
  (interactive (list (mh-interactive-range "Print")))
  (message "Printing...")
  (let (msgs)
    ;; Gather message numbers and add them to "printed" sequence.
    (mh-iterate-on-range msg range
      (mh-add-msgs-to-seq msg 'printed t)
      (mh-notate nil mh-note-printed mh-cmd-note)
      (push msg msgs))
    (setq msgs (nreverse msgs))
    ;; Print scan listing if we have more than one message.
    (if (> (length msgs) 1)
        (let* ((msgs-string
                (mapconcat 'identity (mh-list-to-string
                                      (mh-coalesce-msg-list msgs)) " "))
               (lpr-command
                (format mh-lpr-command-format
                        (cond ((listp range)
                               (format "Folder: %s, Messages: %s"
                                       mh-current-folder msgs-string))
                              ((symbolp range)
                               (format "Folder: %s, Sequence: %s"
                                       mh-current-folder range)))))
               (scan-command
                (format "scan %s | %s" msgs-string lpr-command)))
          (if mh-print-background-flag
              (mh-exec-cmd-daemon shell-file-name nil "-c" scan-command)
            (call-process shell-file-name nil nil nil "-c" scan-command))))
    ;; Print the messages
    (dolist (msg msgs)
      (let* ((mhl-command (format "%s %s %s"
                                  (expand-file-name "mhl" mh-lib-progs)
                                  (if mhl-formfile
                                      (format " -form %s" mhl-formfile)
                                    "")
                                  (mh-msg-filename msg)))
             (lpr-command
              (format mh-lpr-command-format
                      (format "%s/%s" mh-current-folder msg)))
             (print-command
              (format "%s | %s" mhl-command lpr-command)))
        (if mh-print-background-flag
            (mh-exec-cmd-daemon shell-file-name nil "-c" print-command)
          (call-process shell-file-name nil nil nil "-c" print-command)))))
  (message "Printing...done"))

(provide 'mh-print)

;;; Local Variables:
;;; indent-tabs-mode: nil
;;; sentence-end-double-space: nil
;;; End:

;; arch-tag: 8d84d50b-2a49-4d0d-b51e-ba9c9b6fc679
;;; mh-print.el ends here
