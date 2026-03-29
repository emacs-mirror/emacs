;;; emacs-authors-mode.el --- font-locking for etc/AUTHORS  -*- lexical-binding: t -*-

;; Copyright (C) 2021-2026 Free Software Foundation, Inc.

;; Author: Stefan Kangas <stefankangas@gmail.com>
;; Keywords: internal

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

;; Major mode to display the etc/AUTHORS file from the Emacs
;; distribution.  Provides some basic font locking and not much else.

;;; Code:

(require 'subr-x)    ; `emacs-etc--hide-local-variables'

(defgroup emacs-authors-mode nil
  "Display the \"etc/AUTHORS\" file from the Emacs distribution."
  :version "29.1"
  :group 'internal)

(defface emacs-authors-default
  '((t :inherit variable-pitch))
  "Default face used to display the \"etc/AUTHORS\" file.
See also `emacs-authors-mode'."
  :version "29.1")

(defface emacs-authors-author
  '((((class color) (min-colors 88) (background light))
     :foreground "midnight blue"
     :weight bold :height 1.05
     :inherit variable-pitch)
    (((class color) (min-colors 88) (background dark))
     :foreground "cyan"
     :weight bold :height 1.05
     :inherit variable-pitch)
    (((supports :weight bold) (supports :height 1.05))
     :weight bold :height 1.05
     :inherit variable-pitch)
    (((supports :weight bold))
     :weight bold :inherit variable-pitch)
    (t :inherit variable-pitch))
  "Face used for the author in the \"etc/AUTHORS\" file.
See also `emacs-authors-mode'."
  :version "29.1")

(defface emacs-authors-descriptor
  '((((class color) (min-colors 88) (background light))
     :foreground "sienna" :inherit variable-pitch)
    (((class color) (min-colors 88) (background dark))
     :foreground "peru" :inherit variable-pitch)
    (t :inherit variable-pitch))
  "Face used for the description text in the \"etc/AUTHORS\" file.
See also `emacs-authors-mode'."
  :version "29.1")

(defface emacs-authors-other-files
  '((t :inherit emacs-authors-descriptor))
  "Face used for the \"other files\" text in the \"etc/AUTHORS\" file.
See also `emacs-authors-mode'."
  :version "29.1")

(defconst emacs-authors--author-re
  (rx bol (group (not (any blank "\n")) (+? (not (any ":" "\n")))) ":")
  "Regexp matching an author in \"etc/AUTHORS\".")

(defvar emacs-authors-mode-font-lock-keywords
  `((,emacs-authors--author-re
     1 'emacs-authors-author)
    (,(rx (or "wrote"
              (seq (? "and ") (or "co-wrote" "changed"))))
     0 'emacs-authors-descriptor)
    (,(rx "and " (+ digit) " other files")
     0 'emacs-authors-other-files)
    (,(rx bol (not space) (+ not-newline) eol)
     0 'emacs-authors-default)))

(defun emacs-authors-next-author (&optional arg)
  "Move point to the next author in \"etc/AUTHORS\".
With a prefix arg ARG, move point that many authors forward."
  (interactive "p" emacs-authors-mode)
  (if (< 0 arg)
      (progn
        (when (looking-at emacs-authors--author-re)
          (forward-line 1))
        (re-search-forward emacs-authors--author-re nil t arg))
    (when (looking-at emacs-authors--author-re)
      (forward-line -1))
    (re-search-backward emacs-authors--author-re nil t (abs arg)))
  (goto-char (line-beginning-position)))

(defun emacs-authors-prev-author (&optional arg)
  "Move point to the previous author in \"etc/AUTHORS\".
With a prefix arg ARG, move point that many authors backward."
  (interactive "p" emacs-authors-mode)
  (emacs-authors-next-author (- arg)))

(defvar emacs-authors-imenu-generic-expression
  `((nil ,(rx bol (group (+ (not ":"))) ": "
              (or "wrote" "co-wrote" "changed")
              " ")
         1)))

(define-obsolete-variable-alias 'etc-authors-mode-map 'emacs-authors-mode-map "29.1")
(defvar-keymap emacs-authors-mode-map
  :doc "Keymap for `emacs-authors-mode'."
  "n" #'emacs-authors-next-author
  "p" #'emacs-authors-prev-author)

;;;###autoload
(define-derived-mode emacs-authors-mode special-mode "Authors View"
  "Major mode for viewing \"etc/AUTHORS\" from the Emacs distribution.
Provides some basic font locking and not much else."
  (setq-local font-lock-defaults
              '(emacs-authors-mode-font-lock-keywords nil nil ((?_ . "w"))))
  (setq font-lock-multiline nil)
  (setq imenu-generic-expression emacs-authors-imenu-generic-expression)
  (emacs-etc--hide-local-variables)
  (setq-local outline-regexp (rx (+ (not (any ":\n"))) ": "
                                 (or "changed" "co-wrote" "wrote") " ")
              outline-minor-mode-cycle t
              outline-level
              (lambda ()
                (if (looking-at (rx bol
                                    (or (or "  "
                                            (seq "and " (or "co-wrote"
                                                            "changed")))
                                        eol)))
                    2
                  1)))
  (outline-minor-mode))

(define-obsolete-face-alias 'etc-authors-default 'emacs-authors-default "29.1")
(define-obsolete-face-alias 'etc-authors-author 'emacs-authors-author "29.1")
(define-obsolete-face-alias 'etc-authors-descriptor 'emacs-authors-descriptor "29.1")
(define-obsolete-face-alias 'etc-authors-other-files 'emacs-authors-other-files "29.1")
(define-obsolete-function-alias 'etc-authors-next-author #'emacs-authors-next-author "29.1")
(define-obsolete-function-alias 'etc-authors-prev-author #'emacs-authors-prev-author "29.1")
;;;###autoload
(define-obsolete-function-alias 'etc-authors-mode #'emacs-authors-mode "29.1")

(provide 'emacs-authors-mode)
;;; emacs-authors-mode.el ends here
