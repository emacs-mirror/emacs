;;; bbdb-message.el --- BBDB interface to Mail Composition Packages. -*- lexical-binding: t -*-

;; Copyright (C) 2010-2017  Free Software Foundation, Inc.

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
;;; This file contains the BBDB interface to Mail Composition Packages.
;;; See the BBDB info manual for documentation.

;;; Code:

(require 'bbdb)
(require 'message)
(require 'sendmail)

;;;###autoload
(defun bbdb-insinuate-message ()
  "Hook BBDB into Message Mode.
Do not call this in your init file.  Use `bbdb-initialize'."
  ;; Suggestions welcome: What are good keybindings for the following
  ;; commands that do not collide with existing bindings?
  ;; (define-key message-mode-map "'" 'bbdb-mua-display-recipients)
  ;; (define-key message-mode-map ";" 'bbdb-mua-edit-field-recipients)
  ;; (define-key message-mode-map "/" 'bbdb)
  (if bbdb-complete-mail
      (define-key message-mode-map "\M-\t" 'bbdb-complete-mail)))

;;;###autoload
(defun bbdb-insinuate-mail ()
  "Hook BBDB into Mail Mode.
Do not call this in your init file.  Use `bbdb-initialize'."
  ;; Suggestions welcome: What are good keybindings for the following
  ;; commands that do not collide with existing bindings?
  ;; (define-key mail-mode-map "'" 'bbdb-mua-display-recipients)
  ;; (define-key mail-mode-map ";" 'bbdb-mua-edit-field-recipients)
  ;; (define-key mail-mode-map "/" 'bbdb)
  (if bbdb-complete-mail
      (define-key mail-mode-map "\M-\t" 'bbdb-complete-mail)))

(provide 'bbdb-message)

;;; bbdb-message.el ends here
