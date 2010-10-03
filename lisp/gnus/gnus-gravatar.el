;;; gnus-gravatar.el --- Gnus Gravatar support

;; Copyright (C) 2010 Free Software Foundation, Inc.

;; Author: Julien Danjou <julien@danjou.info>
;; Keywords: news

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'gravatar)

(defgroup gnus-gravatar nil
  "Gnus Gravatar."
  :group 'gnus-visual)

(defcustom gnus-gravatar-size 32
  "How big should gravatars be displayed."
  :type 'integer
  :version "24.1"
  :group 'gnus-gravatar)

(defcustom gnus-gravatar-properties '(:ascent center :relief 1)
  "List of image properties applied to Gravatar images."
  :type 'list
  :version "24.1"
  :group 'gnus-gravatar)

(defun gnus-gravatar-transform-address (header category)
  (gnus-with-article-headers
    (let ((addresses
           (mail-header-parse-addresses
            ;; mail-header-parse-addresses does not work (reliably) on
            ;; decoded headers.
            (or
             (ignore-errors
               (mail-encode-encoded-word-string
                (or (mail-fetch-field header) "")))
             (mail-fetch-field header)))))
      (let ((gravatar-size gnus-gravatar-size))
        (dolist (address addresses)
          (gravatar-retrieve
           (car address)
           'gnus-gravatar-insert
           (list header address category)))))))

(defun gnus-gravatar-insert (gravatar header address category)
  "Insert GRAVATAR for ADDRESS in HEADER in current article buffer.
Set image category to CATEGORY."
  (unless (eq gravatar 'error)
    (gnus-with-article-headers
      (gnus-article-goto-header header)
      (mail-header-narrow-to-field)
      (let ((real-name (cdr address))
            (mail-address (car address)))
        (when (if real-name             ; have a realname, go for it!
                  (and (search-forward real-name nil t)
                       (search-backward real-name nil t))
                (and (search-forward mail-address nil t)
                     (search-backward mail-address nil t)))
          (goto-char (1- (point)))
          ;; If we're on the " quoting the name, go backward
          (when (looking-at "[\"<]")
            (goto-char (1- (point))))
          ;; Do not do anything if there's already a gravatar. This can
          ;; happens if the buffer has been regenerated in the mean time, for
          ;; example we were fetching someaddress, and then we change to
          ;; another mail with the same someaddress.
          (unless (memq 'gnus-gravatar (text-properties-at (point)))
            (let ((inhibit-read-only t)
                  (point (point))
                  (gravatar (append
                             gravatar
                             gnus-gravatar-properties)))
              (gnus-put-image gravatar nil category)
              (put-text-property point (point) 'gnus-gravatar address)
              (gnus-add-wash-type category)
              (gnus-add-image category gravatar))))))))

;;;###autoload
(defun gnus-treat-from-gravatar ()
  "Display gravatar in the From header.
If gravatar is already displayed, remove it."
  (interactive)
  (gnus-with-article-buffer
    (if (memq 'from-gravatar gnus-article-wash-types)
        (gnus-delete-images 'from-gravatar)
      (gnus-gravatar-transform-address "from" 'from-gravatar))))

;;;###autoload
(defun gnus-treat-mail-gravatar ()
  "Display gravatars in the Cc and To headers.
If gravatars are already displayed, remove them."
  (interactive)
    (gnus-with-article-buffer
      (if (memq 'mail-gravatar gnus-article-wash-types)
          (gnus-delete-images 'mail-gravatar)
        (gnus-gravatar-transform-address "cc" 'mail-gravatar)
        (gnus-gravatar-transform-address "to" 'mail-gravatar))))

(provide 'gnus-gravatar)

;;; gnus-gravatar.el ends here
