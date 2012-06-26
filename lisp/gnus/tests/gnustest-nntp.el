;;; gnustest-nntp.el --- Simple NNTP testing for Gnus
;; Copyright (C) 2011-2012 Free Software Foundation, Inc.

;; Author: David Engster <dengste@eml.cc>

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This test will
;;
;;   - Fire up Gnus
;;   - Connect to Gmane
;;   - Subscribe to gmane.discuss
;;   - Get its active info
;;   - Get one specific article by message-id and check its subject
;;   - Quit Gnus

;;; Code:

(require 'ert)
(require 'net-utils)

(defvar gnustest-nntp-server "news.gmane.org"
  "NNTP server used for testing.")

(defun gnustest-ping-host (host)
  "Ping HOST once and return non-nil if successful."
  (let* ((ping-program-options '("-c" "1"))
	 (buf (ping host))
	 proc)
    (sleep-for 0.5)
    (with-current-buffer buf
      (accept-process-output (get-buffer-process (current-buffer)) 2)
      (goto-char (point-min))
      (prog1
	  (re-search-forward ",[ ]*1.*?received,[ ]*0" nil t)
	(when (setq proc (get-buffer-process (current-buffer)))
	  (set-process-query-on-exit-flag proc nil))
	(kill-buffer)))))

(setq gnus-home-directory (concat temporary-file-directory (make-temp-name "gnus-test-")))
(message "***** Using %s as temporary Gnus home." gnus-home-directory)
(mkdir gnus-home-directory)
(setq-default gnus-init-file nil)

(require 'gnus-load)

(setq gnus-select-method `(nntp ,gnustest-nntp-server))


(if (null (gnustest-ping-host gnustest-nntp-server))
    (message "***** Skipping tests: Gmane doesn't seem to be available.")
  ;; Server seems to be available, so start Gnus.
  (message "***** Firing up Gnus; connecting to Gmane.")
  (gnus)

  (ert-deftest gnustest-nntp-run-simple-test ()
    "Test Gnus with gmane.discuss."
    (set-buffer gnus-group-buffer)
    (gnus-group-jump-to-group "gmane.discuss")
    (gnus-group-get-new-news-this-group 1)
    (gnus-active "gmane.discuss")
    (message "***** Reading active from gmane.discuss.")
    (should (> (car (gnus-active "gmane.discuss")) 0))
    (should (> (cdr (gnus-active "gmane.discuss")) 10000))
    (gnus-group-unsubscribe-current-group)
    (gnus-group-set-current-level 1 1)
    (gnus-group-select-group 5)
    (message "***** Getting article with certain MID and check subject.")
    (set-buffer gnus-summary-buffer)
    (gnus-summary-refer-article "m3mxr8pa1t.fsf@quimbies.gnus.org")
    (should (string= (gnus-summary-article-subject) "Re: gwene idea: strip from from subject if present"))
    (gnus-summary-exit)
    (message "***** Quitting Gnus.")
    (set-buffer gnus-group-buffer)
    (gnus-group-save-newsrc)
    (gnus-group-exit))
)
