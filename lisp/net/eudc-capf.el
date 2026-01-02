;;; eudc-capf.el --- EUDC - completion-at-point bindings  -*- lexical-binding:t -*-

;; Copyright (C) 2022-2026 Free Software Foundation, Inc.

;; Author: Alexander Adolf
;; Package: eudc

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

;;    This library provides functions to deliver email addresses from
;;    EUDC search results to `completion-at-point'.
;;
;;    Email address completion will likely be desirable only in
;;    situations where designating email recipients plays a role, such
;;    as when composing or replying to email messages, or when posting
;;    to newsgroups, possibly with copies of the post being emailed.
;;    Hence, modes relevant in such contexts, such as for example
;;    `message-mode' and `mail-mode', often at least to some extent
;;    provide infrastructure for different functions to be called when
;;    completing in certain message header fields, or in the body of
;;    the message.  In other modes for editing email messages or
;;    newsgroup posts, which do not provide such infrastructure, any
;;    completion function providing email addresses will need to check
;;    whether the completion attempt occurs in an appropriate context
;;    (that is, in a relevant message header field) before providing
;;    completion candidates.  Two mechanisms are thus provided by this
;;    library.
;;
;;    The first mechanism is intended for use by the modes listed in
;;    `eudc-capf-modes', and relies on these modes adding
;;    `eudc-capf-complete' to `completion-at-point-functions', as
;;    would be usually done for any general-purpose completion
;;    function.  In this mode of operation, and in order to offer
;;    email addresses only in contexts where the user would expect
;;    them, a check is performed whether point is on a line that is a
;;    message header field suitable for email addresses, such as for
;;    example "To:", "Cc:", etc.
;;
;;    The second mechanism is intended for when the user modifies
;;    `message-completion-alist' to replace `message-expand-name' with
;;    the function `eudc-capf-message-expand-name'.  As a result,
;;    minibuffer completion (`completing-read') for email addresses
;;    would no longer enabled in `message-mode', but
;;    `completion-at-point' (in-buffer completion) only.

;;; Usage:

;;    In a major mode, or context where you want email address
;;    completion, you would do something along the lines of:
;;
;;    (require 'eudc-capf)
;;    (add-hook 'completion-at-point-functions #'eudc-capf-complete -1 t)
;;
;;    The minus one argument puts it at the front of the list so it is
;;    called first, and the t value for the LOCAL parameter causes the
;;    setting to be buffer local, so as to avoid modifying any global
;;    setting.
;;
;;    The value of the variable `eudc-capf-modes' indicates which
;;    major modes do such a setup as part of their initialization
;;    code.

;;; Code:

(require 'eudc)

(defvar message-email-recipient-header-regexp)
(defvar mail-abbrev-mode-regexp)
(declare-function mail-abbrev-in-expansion-header-p "mailabbrev" ())

(defconst eudc-capf-modes '(message-mode)
  "List of modes in which email address completion is to be attempted.")

;; completion functions

;;;###autoload
(defun eudc-capf-complete ()
  "Email address completion function for `completion-at-point-functions'.

This function checks whether the current major mode is one of the
modes listed in `eudc-capf-modes', and whether point is on a line
with a message header listing email recipients, that is, a line
whose beginning matches `message-email-recipient-header-regexp',
and, if the check succeeds, searches for records matching the
words before point.

The return value is either nil when no match is found, or a
completion table as required for functions listed in
`completion-at-point-functions'."
  (if (and (seq-some #'derived-mode-p eudc-capf-modes)
           (let ((mail-abbrev-mode-regexp message-email-recipient-header-regexp))
             (mail-abbrev-in-expansion-header-p)))
      (eudc-capf-message-expand-name)))

;;;###autoload
(defun eudc-capf-message-expand-name ()
  "Email address completion function for `message-completion-alist'.

When this function is added to `message-completion-alist',
replacing any existing entry for `message-expand-name' there,
with an appropriate regular expression such as for example
`message-email-recipient-header-regexp', then EUDC will be
queried for email addresses, and the results delivered to
`completion-at-point'."
  (if (or eudc-server eudc-server-hotlist)
      (progn
        (let* ((beg (save-excursion
                      (re-search-backward "\\([:,]\\|^\\)[ \t]*")
                      (match-end 0)))
               (end (point))
               (prefix (save-excursion (buffer-substring-no-properties beg end))))
          (let ((result
                 (eudc-query-with-words (split-string prefix "[ \t]+") t)))
            (when result
              (list beg end
                    (completion-table-with-cache
                     (lambda (_) result) t))))))))

(provide 'eudc-capf)
;;; eudc-capf.el ends here
