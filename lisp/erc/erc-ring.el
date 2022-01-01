;;; erc-ring.el --- Command history handling for erc using ring.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2001-2004, 2006-2022 Free Software Foundation, Inc.

;; Author: Alex Schroeder <alex@gnu.org>
;; Maintainer: Amin Bandali <bandali@gnu.org>
;; Keywords: comm
;; URL: https://www.emacswiki.org/emacs/ErcHistory

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

;; This file implements an input ring -- a history of the stuff you
;; wrote.  To activate:
;;
;; (require 'erc-auto) or (require 'erc-ring)
;; (erc-ring-mode 1)
;;
;; Use M-n and M-p to navigate the ring

;;; Code:

(require 'erc)
(require 'comint)
(require 'ring)

(defgroup erc-ring nil
  "An input ring for ERC."
  :group 'erc)

;;;###autoload(autoload 'erc-ring-mode "erc-ring" nil t)
(define-erc-module ring nil
  "Stores input in a ring so that previous commands and messages can
be recalled using M-p and M-n."
  ((add-hook 'erc-pre-send-functions #'erc-add-to-input-ring)
   (define-key erc-mode-map "\M-p" #'erc-previous-command)
   (define-key erc-mode-map "\M-n" #'erc-next-command))
  ((remove-hook 'erc-pre-send-functions #'erc-add-to-input-ring)
   (define-key erc-mode-map "\M-p" #'undefined)
   (define-key erc-mode-map "\M-n" #'undefined)))

(defvar-local erc-input-ring nil "Input ring for erc.")

(defvar-local erc-input-ring-index nil
  "Position in the input ring for erc.
If nil, the input line is blank and the user is conceptually after
the most recently added item in the ring.  If an integer, the input
line is non-blank and displays the item from the ring indexed by this
variable.")

(defun erc-input-ring-setup ()
  "Do the setup required so that we can use comint style input rings.
Call this function when setting up the mode."
  (unless (ring-p erc-input-ring)
    (setq erc-input-ring (make-ring comint-input-ring-size)))
  (setq erc-input-ring-index nil))

(defun erc-add-to-input-ring (state-or-string)
  "Add STATE-OR-STRING to input ring and reset history position.
STATE-OR-STRING should be a string or an erc-input object."
  (unless erc-input-ring (erc-input-ring-setup))
  (ring-insert erc-input-ring (if (erc-input-p state-or-string)
                                  (erc-input-string state-or-string)
                                state-or-string)) ; string
  (setq erc-input-ring-index nil))

(defun erc-clear-input-ring ()
  "Remove all entries from the input ring, then call `garbage-collect'.
You might use this for security purposes if you have typed a command
containing a password."
  (interactive)
  (setq erc-input-ring (make-ring comint-input-ring-size)
        erc-input-ring-index nil)
  (garbage-collect)
  (message "ERC input ring cleared."))

(defun erc-previous-command ()
  "Replace current command with the previous one from the history."
  (interactive)
  (unless erc-input-ring (erc-input-ring-setup))
  ;; if the ring isn't empty
  (when (> (ring-length erc-input-ring) 0)
    (if (and erc-input-ring-index
             (= (ring-length erc-input-ring) (1+ erc-input-ring-index)))
        (progn
          (erc-replace-current-command "")
          (setq erc-input-ring-index nil))

      ;; If we are not viewing old input and there's text in the input
      ;; area, push it on the history ring before moving back through
      ;; the input history, so it will be there when we return to the
      ;; front.
      (when (and (null erc-input-ring-index)
                 (> (point-max) erc-input-marker))
        (erc-add-to-input-ring (erc-user-input))
        (setq erc-input-ring-index 0))

      (setq erc-input-ring-index (if erc-input-ring-index
                                     (ring-plus1 erc-input-ring-index
                                                 (ring-length erc-input-ring))
                                   0))
      (erc-replace-current-command (ring-ref erc-input-ring
                                             erc-input-ring-index)))))

(defun erc-next-command ()
  "Replace current command with the next one from the history."
  (interactive)
  (unless erc-input-ring (erc-input-ring-setup))
  ;; if the ring isn't empty
  (when (> (ring-length erc-input-ring) 0)
    (if (and erc-input-ring-index
             (= 0 erc-input-ring-index))
        (progn
          (erc-replace-current-command "")
          (setq erc-input-ring-index nil))
      (setq erc-input-ring-index (ring-minus1 (or erc-input-ring-index 0)
                                              (ring-length erc-input-ring)))
      (erc-replace-current-command (ring-ref erc-input-ring
      erc-input-ring-index)))))


(defun erc-replace-current-command (s)
  "Replace current command with string S."
  ;; delete line
  (let ((inhibit-read-only t))
    (delete-region
     (progn (goto-char erc-insert-marker) (erc-bol))
     (goto-char (point-max)))
    (insert s)))

(provide 'erc-ring)

;;; erc-ring.el ends here
;; Local Variables:
;; generated-autoload-file: "erc-loaddefs.el"
;; End:
