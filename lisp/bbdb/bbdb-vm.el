;;; bbdb-vm.el --- BBDB interface to VM -*- lexical-binding: t -*-

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
;; This file contains the BBDB interface to VM.
;; See the BBDB info manual for documentation.

;;; Code:

(require 'bbdb)
(require 'bbdb-com)
(require 'bbdb-mua)

(when t     ;Don't require during compilation, since VM might not be installed!
  (require 'vm-autoloads)
  (require 'vm-summary)
  (require 'vm-mime)
  (require 'vm-vars))

(declare-function vm-get-header-contents "vm-summary"
                  (message header-name-regexp &optional clump-sep))
(declare-function vm-decode-mime-encoded-words-in-string "vm-mime"
                  (string))
(declare-function vm-su-interesting-full-name "vm-summary" (m))
(declare-function vm-su-from "vm-summary" (m))
(defvar vm-message-pointer)             ;In vm-vars
(defvar vm-mode-map)                    ;In vm-vars

;;;###autoload
(defun bbdb/vm-header (header)
  (save-current-buffer
    (vm-select-folder-buffer)
    (vm-get-header-contents (car vm-message-pointer)
                            (concat header ":"))))

;;;###autoload
(defun bbdb-insinuate-vm ()
  "Hook BBDB into VM.
Do not call this in your init file.  Use `bbdb-initialize'."
  (define-key vm-mode-map ":" 'bbdb-mua-display-records)
  (define-key vm-mode-map "`" 'bbdb-mua-display-sender)
  (define-key vm-mode-map "'" 'bbdb-mua-display-recipients)
  (define-key vm-mode-map ";" 'bbdb-mua-edit-field-sender)
  ;; Do we need keybindings for more commands?  Suggestions welcome.
  ;; (define-key vm-mode-map "'" 'bbdb-mua-edit-field-recipients)
  (define-key vm-mode-map "/" 'bbdb)
  ;; `mail-mode-map' is the parent of `vm-mail-mode-map'.
  ;; So the following is also done by `bbdb-insinuate-mail'.
  (if (and bbdb-complete-mail (boundp 'vm-mail-mode-map))
      (define-key vm-mail-mode-map "\M-\t" 'bbdb-complete-mail))

  ;; Set up user field for use in `vm-summary-format'
  ;; (1) Big solution: use whole name
  (if bbdb-mua-summary-unify-format-letter
      (defalias (intern (concat "vm-summary-function-"
                                bbdb-mua-summary-unify-format-letter))
        (lambda (m) (bbdb-mua-summary-unify
                ;; VM does not give us the original From header.
                ;; So we have to work backwards.
                (let ((name (vm-decode-mime-encoded-words-in-string
                             (vm-su-interesting-full-name m)))
                      (mail (vm-su-from m)))
                  (if (string= name mail) mail
                    (format "\"%s\" <%s>" name mail)))))))

  ;; (2) Small solution: a mark for messages whos sender is in BBDB.
  (if bbdb-mua-summary-mark-format-letter
      (defalias (intern (concat "vm-summary-function-"
                                bbdb-mua-summary-mark-format-letter))
        ;; VM does not give us the original From header.
        ;; So we assume that the mail address is sufficient to identify
        ;; the BBDB record of the sender.
        (lambda (m) (bbdb-mua-summary-mark (vm-su-from m))))))

(provide 'bbdb-vm)

;;; bbdb-vm.el ends here
