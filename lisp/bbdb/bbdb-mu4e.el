;;; bbdb-mu4e.el --- BBDB interface to mu4e -*- lexical-binding: t -*-

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
;; This file contains the BBDB interface to mu4e.
;; See the BBDB info manual for documentation.

;;; Code:

(require 'bbdb)
(if t (require 'mu4e-view))

(defvar mu4e-view-mode-map)

;;;###autoload
(defun bbdb-insinuate-mu4e ()
  "Hook BBDB into mu4e.
Do not call this in your init file.  Use `bbdb-initialize'."
  ;; Tackle headers later
  ;; (define-key mu4e-headers-mode-map ":" 'bbdb-mua-display-sender)
  ;; (define-key mu4e-headers-mode-map ";" 'bbdb-mua-edit-field-sender)
  ;; Do we need keybindings for more commands?  Suggestions welcome.
  (define-key mu4e-view-mode-map ":" 'bbdb-mua-display-sender)
  (define-key mu4e-view-mode-map ";" 'bbdb-mua-edit-field-sender))

(provide 'bbdb-mu4e)

;;; bbdb-mu4e.el ends here
