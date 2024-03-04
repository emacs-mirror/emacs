;;; textsec-check.el --- Check for suspicious texts  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024 Free Software Foundation, Inc.

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

;;

;;; Code:

(defgroup textsec nil
  "Suspicious text identification."
  :group 'security
  :version "29.1")

(defcustom textsec-check t
  "If non-nil, perform some security-related checks on text objects.
If nil, these checks are disabled."
  :type 'boolean
  :version "29.1")

(defface textsec-suspicious
  '((t (:weight bold :background "red" :foreground "white")))
  "Face used to highlight suspicious strings.")

;;;###autoload
(defun textsec-suspicious-p (object type)
  "Say whether OBJECT is suspicious for use as TYPE.
If OBJECT is suspicious, return a string explaining the reason
for considering it suspicious, otherwise return nil.

Available values of TYPE and corresponding OBJECTs are:

 `url'                   -- a URL; OBJECT should be a URL string.

 `link'                 -- an HTML link; OBJECT should be a cons cell
                           of the form (URL . LINK-TEXT).

 `domain'               -- a Web domain; OBJECT should be a string.

 `local-address'        -- the local part of an email address; OBJECT
                           should be a string.
 `name'                 -- the \"display name\" part of an email address;
                           OBJECT should be a string.

`email-address'         -- a full email address; OBJECT should be a string.

 `email-address-header' -- a raw email address header in RFC 2822 format;
                           OBJECT should be a string.

If the user option `textsec-check' is nil, these checks are
disabled, and this function always returns nil."
  (if (not textsec-check)
      nil
    (require 'textsec)
    (let ((func (intern (format "textsec-%s-suspicious-p" type))))
      (unless (fboundp func)
        (error "%s is not a valid function" func))
      (funcall func object))))

(provide 'textsec-check)

;;; textsec-check.el ends here
