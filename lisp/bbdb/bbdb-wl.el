;;; bbdb-wl.el --- BBDB interface to Wanderlust -*- lexical-binding: t -*-

;; Copyright (C) 2015-2017  Free Software Foundation, Inc.

;; This file is part of the Insidious Big Brother Database (aka BBDB),

;; BBDB is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; BBDB is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with BBDB.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This file contains the BBDB interface to Wl.
;; See the BBDB info manual for documentation.

;;; Code:

(require 'bbdb)
(require 'bbdb-mua)

(defvar wl-summary-mode-map)
(defvar wl-draft-mode-map)
(defvar wl-summary-buffer-elmo-folder)
(eval-and-compile
  (autoload 'wl-summary-message-number "wl-summary")
  (autoload 'elmo-message-entity "elmo-msgdb")
  (autoload 'elmo-message-entity-field "elmo-msgdb"))

;;;###autoload
(defun bbdb/wl-header (header)
  (elmo-message-entity-field
   (elmo-message-entity wl-summary-buffer-elmo-folder
                        (wl-summary-message-number))
   (intern (downcase header)) 'string))

;;;###autoload
(defun bbdb-insinuate-wl ()
  "Hook BBDB into Wanderlust."
  (define-key wl-summary-mode-map (kbd ":") #'bbdb-mua-display-sender)
  (define-key wl-summary-mode-map (kbd ";") #'bbdb-mua-edit-field-sender)
  (when bbdb-complete-mail
    (define-key wl-draft-mode-map (kbd "M-;") #'bbdb-complete-mail)
    (define-key wl-draft-mode-map (kbd "M-<tab>") #'bbdb-complete-mail)))

(provide 'bbdb-wl)

;;; bbdb-wl.el ends here
