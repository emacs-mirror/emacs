;;; bbdb-gnus.el --- BBDB interface to Gnus -*- lexical-binding: t -*-

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
;;; This file contains the BBDB interface to Gnus.
;;; See the BBDB info manual for documentation.

;;; Code:

(require 'bbdb)
(require 'bbdb-com)
(require 'bbdb-mua)
(require 'gnus)

;;; Insinuation

;;;###autoload
(defun bbdb-insinuate-gnus ()
  "Hook BBDB into Gnus.
Do not call this in your init file.  Use `bbdb-initialize'."
  ;; `bbdb-mua-display-sender' fails in *Article* buffers, where
  ;; `gnus-article-read-summary-keys' provides an additional wrapper
  ;; that restores the window configuration.
  (define-key gnus-summary-mode-map ":" 'bbdb-mua-display-sender)
  (define-key gnus-article-mode-map ":" 'bbdb-mua-display-sender)
  ;; For `bbdb-mua-edit-field-sender' it is probably OK if
  ;;`gnus-article-read-summary-keys' restores the window configuration.
  (define-key gnus-summary-mode-map ";" 'bbdb-mua-edit-field-sender)
  (define-key gnus-article-mode-map ";" 'bbdb-mua-edit-field-sender)
  ;; Do we need keybindings for more commands?  Suggestions welcome.
  ;; (define-key gnus-summary-mode-map ":" 'bbdb-mua-display-records)
  ;; (define-key gnus-summary-mode-map "'" 'bbdb-mua-display-recipients)
  ;; (define-key gnus-summary-mode-map ";" 'bbdb-mua-edit-field-recipients)

  ;; Set up user field for use in `gnus-summary-line-format'
  ;; (1) Big solution: use whole name
  (if bbdb-mua-summary-unify-format-letter
      (defalias (intern (concat "gnus-user-format-function-"
                                bbdb-mua-summary-unify-format-letter))
        (lambda (header)
          (bbdb-mua-summary-unify (mail-header-from header)))))

  ;; (2) Small solution: a mark for messages whose sender is in BBDB.
  (if bbdb-mua-summary-mark-format-letter
      (defalias (intern (concat "gnus-user-format-function-"
                                bbdb-mua-summary-mark-format-letter))
        (lambda (header)
          (bbdb-mua-summary-mark (mail-header-from header))))))

(provide 'bbdb-gnus)

;;; bbdb-gnus.el ends here
