;;; erc-truncate.el --- Functions for truncating ERC buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2003-2004, 2006-2024 Free Software Foundation, Inc.

;; Author: Andreas Fuchs <asf@void.at>
;; Maintainer: Amin Bandali <bandali@gnu.org>, F. Jason Park <jp@neverwas.me>
;; URL: https://www.emacswiki.org/emacs/ErcTruncation
;; Keywords: IRC, chat, client, Internet, logging

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

;; This file implements buffer truncation through the `truncate'
;; module, with optional `log' module integration.

;;; Code:

(require 'erc)

(defgroup erc-truncate nil
  "Truncate buffers when they reach a certain size."
  :group 'erc)

(defcustom erc-max-buffer-size 30000
  "Maximum size in chars of each ERC buffer.
Used only when auto-truncation is enabled.
\(see `erc-truncate-buffer' and `erc-insert-post-hook')."
  :type 'integer)

;;;###autoload(autoload 'erc-truncate-mode "erc-truncate" nil t)
(define-erc-module truncate nil
  "Truncate a query buffer if it gets too large.
This prevents the query buffer from getting too large, which can
bring any grown Emacs to its knees after a few days worth of
tracking heavy-traffic channels."
  ;;enable
  ((add-hook 'erc-insert-done-hook #'erc-truncate-buffer)
   (add-hook 'erc-connect-pre-hook #'erc-truncate--warn-about-logging))
  ;; disable
  ((remove-hook 'erc-insert-done-hook #'erc-truncate-buffer)
   (remove-hook 'erc-connect-pre-hook #'erc-truncate--warn-about-logging)))

(defun erc-truncate--warn-about-logging (&rest _)
  (when (and (not erc--target)
             (fboundp 'erc-log--call-when-logging-enabled-sans-module))
    ;; We could also enable `erc-log-mode' here, but the risk of
    ;; lasting damage is nonzero.
    (erc-log--call-when-logging-enabled-sans-module
     (lambda (dirfile)
       ;; Emit a real Emacs warning because the message may be
       ;; truncated away before it can be read if merely inserted.
       (erc-button--display-error-notice-with-keys-and-warn
        "The `truncate' module no longer enables logging implicitly."
        " If you want ERC to write logs before truncating, add `log' to"
        " `erc-modules' using something like \\[customize-option]."
        " To silence this message, don't `require' `erc-log'."
        (and dirfile " Alternatively, change the value of")
        (and dirfile " `erc-log-channels-directory', or move ")
        dirfile (and dirfile " elsewhere."))))))

;;;###autoload
(defun erc-truncate-buffer-to-size (size &optional buffer)
  "Truncate BUFFER or the current buffer to SIZE.
Log the deleted region when the `log' module is active and
`erc-logging-enabled' returns non-nil.

Note that prior to ERC 5.6, whenever erc-log.el happened to be
loaded and the option `erc-enable-logging' was left at its
default value, this function would cause logging to commence
regardless of whether `erc-log-mode' was enabled or `log' was
present in `erc-modules'."
  ;; If buffer is non-nil, but get-buffer does not return anything,
  ;; then this is a bug.  If buffer is a buffer name, get the buffer
  ;; object.  If buffer is nil, use the current buffer.
  (if (not buffer)
      (setq buffer (current-buffer))
    (unless (get-buffer buffer)
      (error "erc-truncate-buffer-to-size: %S is not a buffer" buffer)))
  (when (> (buffer-size buffer) (+ size 512))
    (with-current-buffer buffer
      ;; Note that when erc-insert-post-hook runs, the buffer is
      ;; narrowed to the new message.  So do this delicate widening.
      ;; I am not sure, I think this was not recommended behavior in
      ;; Emacs 20.
      (save-restriction
	(widen)
	(let ((end (- erc-insert-marker size)))
          ;; Truncate at message boundary (formerly line boundary
          ;; before 5.6).
	  (goto-char end)
          (goto-char (or (erc--get-inserted-msg-beg end)
                         (pos-bol)))
	  (setq end (point))
	  ;; try to save the current buffer using
	  ;; `erc-save-buffer-in-logs'.  We use this, in case the
	  ;; user has both `erc-save-buffer-in-logs' and
	  ;; `erc-truncate-buffer' in `erc-insert-post-hook'.  If
	  ;; this is the case, only the non-saved part of the current
	  ;; buffer should be saved.  Rather than appending the
	  ;; deleted part of the buffer to the log file.
	  ;;
	  ;; Alternatively this could be made conditional on:
	  ;; (not (memq 'erc-save-buffer-in-logs
	  ;;             erc-insert-post-hook))
	  ;; Comments?
          ;; The comments above concern pre-5.6 behavior and reflect
          ;; an obsolete understanding of how `erc-logging-enabled'
          ;; behaves in practice.
          (run-hook-with-args 'erc--pre-clear-functions end)
	  ;; disable undoing for the truncating
	  (buffer-disable-undo)
	  (let ((inhibit-read-only t))
	    (delete-region (point-min) end)))
	(buffer-enable-undo)))))

;;;###autoload
(defun erc-truncate-buffer ()
  "Truncate current buffer to `erc-max-buffer-size'."
  (interactive)
  (save-excursion
    (erc-truncate-buffer-to-size erc-max-buffer-size)))

(provide 'erc-truncate)
;;; erc-truncate.el ends here
;;
;; Local Variables:
;; generated-autoload-file: "erc-loaddefs.el"
;; End:
