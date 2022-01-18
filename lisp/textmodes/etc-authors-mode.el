;;; etc-authors-mode.el --- font-locking for etc/AUTHORS  -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Free Software Foundation, Inc.

;; Author: Stefan Kangas <stefan@marxist.se>
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

(defgroup etc-authors-mode nil
  "Display the \"etc/AUTHORS\" file from the Emacs distribution."
  :version "28.1"
  :group 'internal)

(defface etc-authors-default '((t :inherit variable-pitch))
  "Default face used to display the \"etc/AUTHORS\" file.
See also `etc-authors-mode'."
  :version "28.1")

(defface etc-authors-author '((((class color) (min-colors 88) (background light))
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
See also `etc-authors-mode'."
  :version "28.1")

(defface etc-authors-descriptor '((((class color) (min-colors 88) (background light))
                        :foreground "sienna" :inherit variable-pitch)
                       (((class color) (min-colors 88) (background dark))
                        :foreground "peru" :inherit variable-pitch)
                       (t :inherit variable-pitch))
  "Face used for the description text in the \"etc/AUTHORS\" file.
See also `etc-authors-mode'."
  :version "28.1")

(defface etc-authors-other-files '((t :inherit etc-authors-descriptor))
  "Face used for the \"other files\" text in the \"etc/AUTHORS\" file.
See also `etc-authors-mode'."
  :version "28.1")

(defconst etc-authors--author-re
  (rx bol (group (not (any blank "\n")) (+? (not (any ":" "\n")))) ":")
  "Regexp matching an author in \"etc/AUTHORS\".")

(defvar etc-authors-mode-font-lock-keywords
  `((,etc-authors--author-re
     1 'etc-authors-author)
    (,(rx (or "wrote"
              (seq (? "and ") (or "co-wrote" "changed"))))
     0 'etc-authors-descriptor)
    (,(rx "and " (+ digit) " other files")
     0 'etc-authors-other-files)
    (,(rx bol (not space) (+ not-newline) eol)
     0 'etc-authors-default)))

(defun etc-authors-mode--hide-local-variables ()
  "Hide local variables in \"etc/AUTHORS\".  Used by `etc-authors-mode'."
  (narrow-to-region (point-min)
                    (save-excursion
                      (goto-char (point-min))
                      ;; Obfuscate to avoid this being interpreted
                      ;; as a local variable section itself.
                      (if (re-search-forward "^Local\sVariables:$" nil t)
                          (progn (forward-line -1) (point))
                        (point-max)))))

(defun etc-authors-next-author (&optional arg)
  "Move point to the next author in \"etc/AUTHORS\".
With a prefix arg ARG, move point that many authors forward."
  (interactive "p" etc-authors-mode)
  (if (< 0 arg)
      (progn
        (when (looking-at etc-authors--author-re)
          (forward-line 1))
        (re-search-forward etc-authors--author-re nil t arg))
    (when (looking-at etc-authors--author-re)
          (forward-line -1))
    (re-search-backward etc-authors--author-re nil t (abs arg)))
  (goto-char (line-beginning-position)))

(defun etc-authors-prev-author (&optional arg)
  "Move point to the previous author in \"etc/AUTHORS\".
With a prefix arg ARG, move point that many authors backward."
  (interactive "p" etc-authors-mode)
  (etc-authors-next-author (- arg)))

(defvar etc-authors-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" #'etc-authors-next-author)
    (define-key map "p" #'etc-authors-prev-author)
    map)
  "Keymap for `etc-authors-mode'.")

;;;###autoload
(define-derived-mode etc-authors-mode special-mode "Authors View"
  "Major mode for viewing \"etc/AUTHORS\" from the Emacs distribution.
Provides some basic font locking and not much else."
  (setq-local font-lock-defaults
              '(etc-authors-mode-font-lock-keywords nil nil ((?_ . "w"))))
  (setq font-lock-multiline nil)
  (etc-authors-mode--hide-local-variables))

(provide 'etc-authors-mode)
;;; etc-authors-mode.el ends here
