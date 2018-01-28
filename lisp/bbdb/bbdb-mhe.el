;;; bbdb-mhe.el --- BBDB interface to mh-e  -*- lexical-binding: t -*-

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
;;; This file contains the BBDB interface to mh-e.
;;; See the BBDB info manual for documentation.

;;; Code:

(require 'bbdb)
(require 'bbdb-com)
(require 'bbdb-mua)
(require 'mh-e)
(if (fboundp 'mh-version)
    (require 'mh-comp))              ; For mh-e 4.x
(require 'advice)

;; A simplified `mail-fetch-field'.  We could use instead (like rmail):
;; (mail-header (intern-soft (downcase header)) (mail-header-extract))
;;;###autoload
(defun bbdb/mh-header (header)
  "Find and return the value of HEADER in the current buffer.
Returns the empty string if HEADER is not in the message."
  (let ((case-fold-search t))
    (goto-char (point-min))
    ;; This will be fooled if HEADER appears in the body of the message.
    ;; Also, it fails if HEADER appears more than once.
    (cond ((not (re-search-forward header nil t)) "")
          ((looking-at "[\t ]*$") "")
          (t (re-search-forward "[ \t]*\\([^ \t\n].*\\)$" nil t)
           (let ((start (match-beginning 1)))
             (while (progn (forward-line 1)
                           (looking-at "[ \t]")))
             (backward-char 1)
             (buffer-substring-no-properties start (point)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use BBDB for interactive spec of MH-E commands

(defadvice mh-send (before mh-bbdb-send act)
  (interactive
   (list (bbdb-completing-read-mails "To: ")
         (bbdb-completing-read-mails "Cc: ")
         (read-string "Subject: "))))

(defadvice mh-send-other-window (before mh-bbdb-send-other act)
  (interactive
   (list (bbdb-completing-read-mails "To: ")
         (bbdb-completing-read-mails "Cc: ")
         (read-string "Subject: "))))

(defadvice mh-forward (before mh-bbdb-forward act)
  (interactive
   (list (bbdb-completing-read-mails "To: ")
         (bbdb-completing-read-mails "Cc: ")
         (if current-prefix-arg
             (mh-read-seq-default "Forward" t)
           (mh-get-msg-num t)))))

(defadvice mh-redistribute (before mh-bbdb-redist act)
  (interactive
   (list (bbdb-completing-read-mails "Redist-To: ")
         (bbdb-completing-read-mails "Redist-Cc: ")
         (mh-get-msg-num t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun bbdb-insinuate-mh ()
  "Call this function to hook BBDB into MH-E.
Do not call this in your init file.  Use `bbdb-initialize'."
  (define-key mh-folder-mode-map ":" 'bbdb-mua-display-sender)
  (define-key mh-folder-mode-map ";" 'bbdb-mua-edit-field-sender)
  ;; Do we need keybindings for more commands?  Suggestions welcome.
  ;; (define-key mh-folder-mode-map ":" 'bbdb-mua-display-records)
  ;; (define-key mh-folder-mode-map "'" 'bbdb-mua-display-recipients)
  ;; (define-key mh-folder-mode-map ";" 'bbdb-mua-edit-field-recipients)
  (when bbdb-complete-mail
      (define-key mh-letter-mode-map "\M-;" 'bbdb-complete-mail)
      (define-key mh-letter-mode-map "\e\t" 'bbdb-complete-mail)))

(provide 'bbdb-mhe)

;;; bbdb-mhe.el ends here
