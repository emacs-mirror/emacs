;;; erc-truncate.el --- Functions for truncating ERC buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2003-2004, 2006-2026 Free Software Foundation, Inc.

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
  "Buffer size in characters after truncation.
Only applies when the `truncate' module is enabled."
  :type 'integer)

(defcustom erc-truncate-padding-size 4096
  "Headroom threshold triggering truncation and determining its frequency.
Truncation occurs when the buffer's size meets or exceeds this value
plus `erc-max-buffer-size'."
  :type 'integer
  :package-version '(ERC . "5.6.1"))

;;;###autoload(autoload 'erc-truncate-mode "erc-truncate" nil t)
(define-erc-module truncate nil
  "Truncate a query buffer if it gets too large.
This prevents the query buffer from getting too large, which can
bring any grown Emacs to its knees after a few days worth of
tracking heavy-traffic channels.

Before ERC 5.6, this module performed logging whenever the \\+`log'
module's library, \\+`erc-log', happened to be loaded, regardless of
whether the \\+`log' module itself was enabled.  (Loading can of course
happen in any number of ways, such as when browsing options via
\\[customize-group] or completing autoloaded symbol names at the
\\[describe-variable] prompt.)  Users of \\+`truncate' who prefer the
old behavior can add \\+`log' to `erc-modules' to get the same effect.
Those who don't want logging but need to load the \\+`erc-log' library
for other purposes should customize either `erc-enable-logging' or
`erc-log-channels-directory' to avoid the annoying warning."
  ;;enable
  ((add-hook 'erc-insert-done-hook #'erc-truncate-buffer)
   (add-hook 'erc-connect-pre-hook #'erc-truncate--warn-about-logging)
   (add-hook 'erc-mode-hook #'erc-truncate--setup)
   (unless erc--updating-modules-p (erc-buffer-do #'erc-truncate--setup)))
  ;; disable
  ((remove-hook 'erc-insert-done-hook #'erc-truncate-buffer)
   (remove-hook 'erc-connect-pre-hook #'erc-truncate--warn-about-logging)
   (remove-hook 'erc-mode-hook #'erc-truncate--setup)
   (erc-buffer-do #'erc-truncate--setup)))

(defvar-local erc-truncate--buffer-size nil
  "Temporary buffer-local override for `erc-max-buffer-size'.")

(defun erc-truncate--setup ()
  "Enable or disable buffer-local `erc-truncate-mode' modifications."
  (if erc-truncate-mode
      (progn
        (when-let* ((priors (or erc--server-reconnecting erc--target-priors))
                    (val (alist-get 'erc-truncate--buffer-size priors)))
          (setq erc-truncate--buffer-size val))
        (add-function :before (local 'erc--clear-function)
                      #'erc-truncate--inhibit-when-local-and-interactive
                      '((depth . 20))))
    (remove-function (local 'erc--clear-function)
                     #'erc-truncate--inhibit-when-local-and-interactive)
    (kill-local-variable 'erc-truncate--buffer-size)))

(defun erc-truncate--warn-about-logging (&rest _)
  (when (and (not erc--target)
             (fboundp 'erc-log--check-legacy-implicit-enabling-by-truncate)
             (erc-log--check-legacy-implicit-enabling-by-truncate))
    ;; Emit a real Emacs warning because the message may be
    ;; truncated away before it can be read if merely inserted.
    (erc-button--display-error-notice-with-keys-and-warn
     "The `truncate' module no longer enables logging implicitly."
     " See the doc string for `erc-truncate-mode' for details.")))

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
  (when (and (> (buffer-size buffer) (+ size erc-truncate-padding-size))
             (not (buffer-local-value 'erc--inhibit-clear-p buffer)))
    (with-current-buffer buffer
      (let ((wc (and (get-buffer-window) (current-window-configuration))))
        (save-excursion
          ;; Widen to preserve pre-5.5 behavior.
          (save-restriction
            (widen)
            (let ((beg (point-min-marker))
                  (end (goto-char (- erc-insert-marker size))))
              ;; Truncate at message boundary (formerly line boundary
              ;; before 5.6).
              (goto-char (or (erc--get-inserted-msg-beg end) (pos-bol)))
              (setq end (point-marker))
              (with-silent-modifications
                (let ((erc--inhibit-clear-p t))
                  (funcall erc--clear-function beg end)))
              (set-marker beg nil)
              (set-marker end nil))))
        (when wc
          (set-window-configuration wc))))))

;;;###autoload
(defun erc-truncate-buffer ()
  "Truncate current buffer to `erc-max-buffer-size'."
  (interactive)
  ;; This `save-excursion' only exists for historical reasons because
  ;; `erc-truncate-buffer-to-size' normally runs in a different buffer.
  (save-excursion
    (if (and erc--parsed-response erc--msg-props)
        (when-let*
            (((not erc--inhibit-clear-p))
             ((not (erc--memq-msg-prop 'erc--skip 'truncate)))
             ;; Determine here because this may be a target buffer and
             ;; the hook always runs in the server buffer.
             (size (if (and erc-truncate--buffer-size
                            (> erc-truncate--buffer-size erc-max-buffer-size))
                       erc-truncate--buffer-size
                     erc-max-buffer-size))
             (symbol (make-symbol "erc-truncate--buffer-deferred"))
             (buffer (current-buffer)))
          (fset symbol
                (lambda (&rest _)
                  (remove-hook 'erc-timer-hook symbol t)
                  (erc-truncate-buffer-to-size size buffer)))
          (erc-with-server-buffer (add-hook 'erc-timer-hook symbol -80 t)))
      (unless erc--inhibit-clear-p
        (erc-truncate-buffer-to-size erc-max-buffer-size)))))

(defun erc-truncate--inhibit-when-local-and-interactive (&rest _)
  "Ensure `erc-truncate--buffer-size' is nil on /CLEAR."
  (when (and erc--called-as-input-p erc-truncate--buffer-size)
    (message "Resetting max buffer size to %d" erc-max-buffer-size)
    (setq erc-truncate--buffer-size nil)))

(provide 'erc-truncate)
;;; erc-truncate.el ends here
;;
;; Local Variables:
;; generated-autoload-file: "erc-loaddefs.el"
;; End:
