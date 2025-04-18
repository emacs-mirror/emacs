;;; emacs-lock.el --- protect buffers against killing or exiting -*- lexical-binding: t -*-

;; Copyright (C) 2011-2025 Free Software Foundation, Inc.

;; Author: Juanma Barranquero <lekktu@gmail.com>
;; Inspired by emacs-lock.el by Tom Wurgler <twurgler@goodyear.com>
;; Maintainer: emacs-devel@gnu.org
;; Keywords: extensions, processes

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

;; This package defines a minor mode Emacs Lock to mark a buffer as
;; protected against accidental killing, or exiting Emacs, or both.
;; Buffers associated with inferior modes, like shell or telnet, can
;; be treated specially, by auto-unlocking them if their inferior
;; processes are dead.

;;; Code:

(defgroup emacs-lock nil
  "Emacs-Lock mode."
  :version "24.1"
  :group 'convenience)

(defcustom emacs-lock-default-locking-mode 'all
  "Default locking mode of Emacs-Locked buffers.

Its value is used as the default for `emacs-lock-mode' (which
see) the first time that Emacs Lock mode is turned on in a buffer
without passing an explicit locking mode.

Possible values are:
 exit   -- Emacs cannot exit while the buffer is locked
 kill   -- the buffer cannot be killed, but Emacs can exit as usual
 all    -- the buffer is locked against both actions
 nil    -- the buffer is not locked"
  :type '(choice
          (const :tag "Do not allow Emacs to exit" exit)
          (const :tag "Do not allow killing the buffer" kill)
          (const :tag "Do not allow killing the buffer or exiting Emacs" all)
          (const :tag "Do not lock the buffer" nil))
  :group 'emacs-lock
  :version "24.1")

;; Note: as auto-unlocking can lead to data loss, it would be better
;; to default to nil; but the value below is for compatibility with
;; the old emacs-lock.el.
(defcustom emacs-lock-unlockable-modes '((shell-mode . all)
                                         (telnet-mode . all))
  "Alist of auto-unlockable modes.
Each element is a pair (MAJOR-MODE . ACTION), where ACTION is
one of `kill', `exit' or `all'.  Buffers with matching major
modes are auto-unlocked for the specific action if their
inferior processes are not alive.  If this variable is t, all
buffers associated to inferior processes are auto-unlockable
for both actions (NOT RECOMMENDED)."
  :type '(choice
          (const :tag "All buffers with inferior processes" t)
          (repeat :tag "Selected modes"
           (cons :tag "Set auto-unlock for"
            (symbol :tag "Major mode")
            (radio
             (const :tag "Allow exiting" exit)
             (const :tag "Allow killing" kill)
             (const :tag "Allow both" all)))))
  :group 'emacs-lock
  :version "24.1")

(defcustom emacs-lock-locked-buffer-functions nil
  "Abnormal hook run when Emacs Lock prevents exiting Emacs, or killing a buffer.
The functions get one argument, the first locked buffer found."
  :type 'hook
  :group 'emacs-lock
  :version "24.3")

(defvar-local emacs-lock-mode nil
  "If non-nil, the current buffer is locked.
It can be one of the following values:
 exit   -- Emacs cannot exit while the buffer is locked
 kill   -- the buffer cannot be killed, but Emacs can exit as usual
 all    -- the buffer is locked against both actions
 nil    -- the buffer is not locked

See also `emacs-lock-unlockable-modes', which exempts buffers under
some major modes from being locked under some circumstances.")
(put 'emacs-lock-mode 'permanent-local t)

(defvar-local emacs-lock--old-mode nil
  "Most recent locking mode set on the buffer.
Internal use only.")
(put 'emacs-lock--old-mode 'permanent-local t)

(defvar-local emacs-lock--try-unlocking nil
  "Non-nil if current buffer should be checked for auto-unlocking.
Internal use only.")
(put 'emacs-lock--try-unlocking 'permanent-local t)

(defun emacs-lock-live-process-p (buffer-or-name)
  "Return t if BUFFER-OR-NAME is associated with a live process."
  (and (process-live-p (get-buffer-process buffer-or-name)) t))

(defun emacs-lock--can-auto-unlock (action)
  "Return t if the current buffer can auto-unlock for ACTION.
ACTION must be one of `kill' or `exit'.
See `emacs-lock-unlockable-modes'."
  (and emacs-lock--try-unlocking
       (not (emacs-lock-live-process-p (current-buffer)))
       (or (eq emacs-lock-unlockable-modes t)
           (let ((unlock (cdr (assq major-mode emacs-lock-unlockable-modes))))
             (or (eq unlock 'all) (eq unlock action))))))

(defun emacs-lock--exit-locked-buffer ()
  "Return the first exit-locked buffer found."
  (save-current-buffer
    (catch :found
      (dolist (buffer (buffer-list))
        (set-buffer buffer)
        (unless (or (emacs-lock--can-auto-unlock 'exit)
                    (memq emacs-lock-mode '(nil kill)))
          (throw :found buffer)))
      nil)))

(defun emacs-lock--kill-emacs-hook ()
  "Signal an error if any buffer is exit-locked.
Used from `kill-emacs-hook' (which see)."
  (let ((locked (emacs-lock--exit-locked-buffer)))
    (when locked
      (run-hook-with-args 'emacs-lock-locked-buffer-functions locked)
      (error "Emacs cannot exit because buffer %S is locked"
             (buffer-name locked)))))

(defun emacs-lock--kill-emacs-query-functions ()
  "Display a message if any buffer is exit-locked.
Return a value appropriate for `kill-emacs-query-functions' (which see)."
  (let ((locked (emacs-lock--exit-locked-buffer)))
    (if (not locked)
        t
      (run-hook-with-args 'emacs-lock-locked-buffer-functions locked)
      (message "Emacs cannot exit because buffer %S is locked"
               (buffer-name locked))
      nil)))

(defun emacs-lock--kill-buffer-query-functions ()
  "Display a message if the current buffer is kill-locked.
Return a value appropriate for `kill-buffer-query-functions' (which see)."
  (if (or (emacs-lock--can-auto-unlock 'kill)
          (memq emacs-lock-mode '(nil exit)))
      t
    (run-hook-with-args 'emacs-lock-locked-buffer-functions (current-buffer))
    (message "Buffer %S is locked and cannot be killed" (buffer-name))
    nil))

(defun emacs-lock--set-mode (mode arg prefix)
  "Setter function for `emacs-lock-mode'."
  (setq emacs-lock-mode
        (cond ((memq arg '(all exit kill))
               ;; explicit locking mode arg, use it
               arg)
              ;; kludgy, but commit 2a4b0da28c converts arg to number
              ((and (eq arg 4) (equal prefix '(4)))
               ;; called with C-u M-x emacs-lock-mode, so ask the user
               (intern (completing-read
                        (format-prompt "Locking mode"
                                       emacs-lock-default-locking-mode)
                        '("all" "exit" "kill")
                        nil t nil nil
                        (symbol-name emacs-lock-default-locking-mode))))
              ((eq mode t)
               ;; turn on, so use previous setting, or customized default
               (or emacs-lock--old-mode emacs-lock-default-locking-mode))
              (t
               ;; anything else (turn off)
               mode))))

;;;###autoload
(define-minor-mode emacs-lock-mode
  "Toggle Emacs Lock mode in the current buffer.
If called with a plain prefix argument, ask for the locking mode
to be used.

Initially, if the user does not pass an explicit locking mode, it
defaults to `emacs-lock-default-locking-mode' (which see);
afterwards, the locking mode most recently set on the buffer is
used instead.

When called from Elisp code, ARG can be any locking mode:

 exit   -- Emacs cannot exit while the buffer is locked
 kill   -- the buffer cannot be killed, but Emacs can exit as usual
 all    -- the buffer is locked against both actions

Other values are interpreted as usual.

See also `emacs-lock-unlockable-modes', which exempts buffers under
some major modes from being locked under some circumstances."
  :init-value nil
  :lighter (""
            (emacs-lock--try-unlocking " locked:" " Locked:")
            (:eval (symbol-name emacs-lock-mode)))
  :group 'emacs-lock
  :variable (emacs-lock-mode .
                             (lambda (mode)
                               (emacs-lock--set-mode mode arg current-prefix-arg)))
  (when emacs-lock-mode
    (setq emacs-lock--old-mode emacs-lock-mode)
    (setq emacs-lock--try-unlocking
          (and (if (eq emacs-lock-unlockable-modes t)
                   (emacs-lock-live-process-p (current-buffer))
                 (assq major-mode emacs-lock-unlockable-modes))
               t))))

(unless noninteractive
  (add-hook 'kill-buffer-query-functions 'emacs-lock--kill-buffer-query-functions)
  ;; We set a hook in both kill-emacs-hook and kill-emacs-query-functions because
  ;; we really want to use k-e-q-f to stop as soon as possible, but don't want to
  ;; be caught by surprise if someone calls `kill-emacs' instead.
  (add-hook 'kill-emacs-hook 'emacs-lock--kill-emacs-hook)
  (add-hook 'kill-emacs-query-functions 'emacs-lock--kill-emacs-query-functions))

(defun emacs-lock-unload-function ()
  "Unload the Emacs Lock library."
  (catch :continue
    (dolist (buffer (buffer-list))
      (set-buffer buffer)
      (when emacs-lock-mode
        (if (y-or-n-p (format "Buffer %S is locked, unlock it? " (buffer-name)))
            (emacs-lock-mode -1)
          (message "Unloading of feature `emacs-lock' aborted.")
          (throw :continue t))))
    ;; continue standard unloading
    nil))

(provide 'emacs-lock)

;;; emacs-lock.el ends here
