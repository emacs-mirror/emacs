;;; erc-desktop-notifications.el --- Send notification on PRIVMSG or mentions -*- lexical-binding:t -*-

;; Copyright (C) 2012-2026 Free Software Foundation, Inc.

;; Author: Julien Danjou <julien@danjou.info>
;; Maintainer: Amin Bandali <bandali@gnu.org>, F. Jason Park <jp@neverwas.me>
;; Keywords: comm

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

;; This implements notifications using `notifications-notify' on
;; PRIVMSG received and on public nickname mentions.

;;; Code:

(require 'erc)
(require 'xml)
(require 'notifications)
(require 'erc-goodies)
(require 'erc-match)
(require 'dbus)

(defgroup erc-notifications nil
  "Send notifications on PRIVMSG or mentions."
  :version "24.3"
  :group 'erc)

(defvar erc-notifications-last-notification nil
  "Last notification id.")

(defcustom erc-notifications-icon nil
  "Icon to use for notification."
  :type '(choice (const :tag "No icon" nil) file))

(defcustom erc-desktop-notifications-ignored-when-focused ()
  "Contexts in which to suppress notification in the selected window.
Assumes `erc-desktop-notifications-focused-p' is a member of
`erc-desktop-notifications-skip-predicates'.  If the option's value
contains the symbol `query', ERC skips all notifications in focused
query buffers.  And if it contains `mention', ERC skips notifications
upon mention of the user's nick in a focused channel buffer."
  :package-version '(ERC . "5.7")
  :type '(set (const query) (const mention)))

(defcustom erc-desktop-notifications-skip-predicates
  '(erc-desktop-notifications-focused-p
    erc-desktop-notifications-untracked-p
    erc-desktop-notifications-fool-p)
  "Abnormal hook whose members return non-nil to suppress notification.
Called in match buffer with a matching `erc-match-user' object."
  :options '(erc-desktop-notifications-focused-p
             erc-desktop-notifications-untracked-p
             erc-desktop-notifications-fool-p)
  :package-version '(ERC . "5.7")
  :type 'hook)

(defcustom erc-notifications-bus :session
  "D-Bus bus to use for notification."
  :version "25.1"
  :type '(choice (const :tag "Session bus" :session) string))

(defvar dbus-debug) ; used in the macroexpansion of dbus-ignore-errors

(declare-function haiku-notifications-notify "haikuselect.c")
(declare-function android-notifications-notify "androidselect.c")

(defun erc-notifications-notify (nick msg &optional privp)
  "Notify that NICK send some MSG, where PRIVP should be non-nil for PRIVMSGs.
This will replace the last notification sent with this function."
  (dbus-ignore-errors
    (setq erc-notifications-last-notification
          (let* ((channel (or (and privp (not (equal nick (erc-target)))
                                   (erc-get-buffer nick))
                              (current-buffer)))
                 (title (if (or privp (equal nick (erc-target)))
                            (erc-compat--xml-escape-string nick t)
                          (format "%s in %s"
                                  (erc-compat--xml-escape-string nick t)
                                  channel)))
                 (body (erc-compat--xml-escape-string
                        (erc-controls-strip msg) t)))
            (funcall (cond ((featurep 'android)
                            #'android-notifications-notify)
                           ((featurep 'haiku)
                            #'haiku-notifications-notify)
                           (t #'notifications-notify))
                     :bus erc-notifications-bus
                     :title title
                     :body body
                     :replaces-id erc-notifications-last-notification
                     :app-icon erc-notifications-icon
                     :actions '("default" "Switch to buffer")
                     :on-action (lambda (&rest _)
                                  (pop-to-buffer channel)))))))

(defun erc-notifications-PRIVMSG (_proc parsed)
  (declare (obsolete "switched to `erc-match-type' API" "31.1"))
  (let ((nick (car (erc-parse-user (erc-response.sender parsed))))
        (target (car (erc-response.command-args parsed)))
        (msg (erc-response.contents parsed)))
    (when (and (erc-current-nick-p target)
               (not (and (boundp 'erc-track-exclude)
                         (member nick erc-track-exclude)))
               (not (erc-is-message-ctcp-and-not-action-p msg)))
      (erc-notifications-notify nick msg t)))
  ;; Return nil to continue processing by ERC
  nil)

(defun erc-desktop-notifications-untracked-p (&rest _)
  "Return non-nil if current buffer's target appears in `erc-track-exclude'."
  (and (boundp 'erc-track-exclude) (member (erc-target) erc-track-exclude)))

(defun erc-desktop-notifications-fool-p (&rest _)
  "Return non-nil if the current message has a \"match type\" of `fool'."
  (erc-match-get-match 'erc-match-opt-fool))

(defun erc-desktop-notifications-focused-p (match)
  "Return non-nil if the frame is focused and suppressed by context.
See `erc-desktop-notifications-ignored-when-focused' for contexts."
  (and (eq (current-buffer) (window-buffer))
       (cond
        ((erc-query-buffer-p)
         (memq 'query erc-desktop-notifications-ignored-when-focused))
        ((erc-match-opt-current-nick-p match)
         (memq 'mention erc-desktop-notifications-ignored-when-focused)))
       (frame-focus-state)))

(defun erc-notifications-notify-on-match (match-type _ msg)
  "Emit MSG if MATCH-TYPE is `current-nick' and other conditions allow."
  (when (eq match-type 'current-nick)
    (let ((match erc-match-highlight-matched))
      (cl-assert (erc-match-opt-current-nick-p match))
      (when-let* ((nick (erc-match-nick match)))
        (unless (run-hook-with-args-until-success
                 'erc-desktop-notifications-skip-predicates
                 match)
          (erc-notifications-notify nick msg))))))

;;;###autoload(autoload 'erc-notifications-mode "erc-desktop-notifications" "" t)
(define-erc-module notifications nil
  "Send notifications on private message reception and mentions."
  ;; Enable
  ((unless erc--updating-modules-p
     (erc-buffer-do #'erc-desktop-notifications--setup))
   (add-hook 'erc-mode-hook #'erc-desktop-notifications--setup))
  ;; Disable
  ((erc-buffer-do #'erc-desktop-notifications--setup)
   (remove-hook 'erc-mode-hook #'erc-desktop-notifications--setup)))

(defun erc-desktop-notifications--setup ()
  (if erc-notifications-mode
      (progn
        (add-hook 'erc-match-functions
                  ;; Run after default value to detect fools.
                  #'erc-desktop-notifications-match-query 20 t)
        (add-hook 'erc-text-matched-hook #'erc-notifications-notify-on-match
                  20 t))
    (remove-hook 'erc-match-functions
                 #'erc-desktop-notifications-match-query t)
    (remove-hook 'erc-text-matched-hook
                 #'erc-notifications-notify-on-match t)))

;; This flag is most likely only temporary and exists as a hedge against
;; a likely thinko involving NOTICEs sent to query buffers.  At the time
;; of writing, it's unclear whether the current behavior of suppressing
;; query NOTICEs outright is TRT.  For example, a user might want
;; NOTICEs from a particular bot to trigger notifications because it's
;; monitoring critical updates to some library they use.  When the
;; picture becomes clearer, the introduction of a new option/predicate
;; pair resembling `erc-desktop-notifications-ignored-when-focused' and
;; `erc-desktop-notifications-focused-p' may be warranted.
(defvar erc-desktop-notifications--query-NOTICE-p nil
  "Whether to notify on receiving a \"NOTICE\" in a query.
Bots and services typically send these.")

(cl-defstruct (erc-desktop-notifications-match-query
               (:constructor erc-desktop-notifications-match-query)
               (:include erc-match-user
                         (category nil)
                         (predicate #'erc-desktop-notifications--query-p)
                         (handler #'erc-desktop-notifications--query-notify)))
  "Desktop notification match type for queries.")

(defun erc-desktop-notifications--query-p (match)
  "Return non-nil if MATCH object describes a \"PRIVMSG\" query."
  (and (erc-query-buffer-p)
       (or erc-desktop-notifications--query-NOTICE-p
           (eq (erc-match-command match) 'PRIVMSG))
       (progn
         (cl-assert (erc-match-nick match))
         (not (run-hook-with-args-until-success
               'erc-desktop-notifications-skip-predicates match)))))

(defun erc-desktop-notifications--query-notify (match)
  ;; No need for PRIVP arg because current buffer is correct.
  (erc-notifications-notify (erc-target)
                            (erc-match-get-message-body match)))


(provide 'erc-desktop-notifications)

;;; erc-desktop-notifications.el ends here

;; Local Variables:
;; generated-autoload-file: "erc-loaddefs.el"
;; End:
