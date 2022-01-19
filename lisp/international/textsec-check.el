;;; textsec-check.el --- Check for suspicious texts  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

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
  "If non-nil, perform some checks on certain texts.
If nil, these checks are disabled."
  :type 'boolean
  :version "29.1")

(defface textsec-suspicious
  '((t (:weight bold :background "red")))
  "Face used to highlight suspicious strings.")

;;;###autoload
(defun textsec-check (string type)
  "Test whether STRING is suspicious when considered as TYPE.
If STRING is suspicious, a string explaining the possible problem
is returned.

Available types include `url', `link', `domain', `local-address',
`name', `email-address', and `email-address-headers'.

If the `textsec-check' user option is nil, these checks are
disabled, and this function always returns nil."
  (if (not textsec-check)
      nil
    (require 'textsec)
    (let ((func (intern (format "textsec-%s-suspicious-p" type))))
      (unless (fboundp func)
        (error "%s is not a valid function" func))
      (funcall func string))))

;;;###autoload
(defun textsec-propertize (string type)
  "Test whether STRING is suspicious when considered as TYPE.
If STRING is suspicious, text properties will be added to the
string to mark it as suspicious, and with tooltip texts that says
what's suspicious about it.  Otherwise STRING is returned
verbatim.

See `texsec-check' for further information about TYPE."
  (let ((warning (textsec-check string type)))
    (if (not warning)
        string
      (propertize string
                  'face 'textsec-suspicious
                  'help-echo warning))))

(provide 'textsec-check)

;;; textsec-check.el ends here
