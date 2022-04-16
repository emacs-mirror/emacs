;;; emacs-news-mode.el --- major mode to edit and view the NEWS file -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

;; Keywords: tools

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

;;; Code:

(eval-when-compile (require 'cl-lib))

(defgroup emacs-news-mode nil
  "Major mode for editing and viewing the Emacs NEWS file."
  :group 'lisp)

(defface emacs-news-is-documented
  '((t :inherit font-lock-type-face))
  "Face used for displaying the \"is documented\" tag."
  :version "29.1")

(defface emacs-news-does-not-need-documentation
  '((t :inherit font-lock-preprocessor-face))
  "Face used for displaying the \"does not need documentation\" tag."
  :version "29.1")

(defvar-keymap emacs-news-mode-map
  "C-c C-s" #'emacs-news-next-untagged-entry)

(defvar emacs-news-mode-font-lock-keywords
  `(("^---$" 0 'emacs-news-does-not-need-documentation)
    ("^\\+\\+\\+$" 0 'emacs-news-is-documented)))

;;;###autoload
(define-derived-mode emacs-news-mode text-mode "NEWS"
  "Major mode for editing and viewind the Emacs NEWS file."
  (setq-local font-lock-defaults '(emacs-news-mode-font-lock-keywords t))
  (setq-local outline-regexp "^\\*+ "
              outline-minor-mode-cycle t
              outline-minor-mode-highlight 'append)
  (setq-local fill-paragraph-function #'emacs-news--fill-paragraph)
  (outline-minor-mode 1)
  (when buffer-read-only
    (emacs-news--buttonize)
    (button-mode)))

(defun emacs-news--fill-paragraph (&optional justify)
  (cond
   ;; We're in a heading -- do nothing.
   ((save-excursion
      (beginning-of-line)
      (looking-at "\\*+ "))
    )
   ;; We're in a news item -- exclude the heading before filling.
   ((and (save-excursion
           (re-search-backward (concat "^\\(?:" paragraph-start "\\|\\*+ \\)")
                               nil t))
         (= (char-after (match-beginning 0)) ?*))
    (save-restriction
      (narrow-to-region (save-excursion
                          (goto-char (match-beginning 0))
                          (forward-line 1)
                          (point))
                        (point-max))
      (fill-paragraph justify)))
   ;; Fill normally.
   (t
    (fill-paragraph justify))))

(defun emacs-news-next-untagged-entry ()
  "Go to the next untagged NEWS entry."
  (interactive nil emacs-news-mode)
  (let ((start (point))
        (found nil))
    ;; Don't consider the current line, because that would stop
    ;; progress if calling this command repeatedly.
    (forward-line 1)
    (while (and (not found)
                (re-search-forward "\\(\\*+\\) " nil t)
                (not (save-excursion
                       (forward-line -1)
                       (looking-at "---$\\|\\+\\+\\+$"))))
      ;; We have an entry without a tag before it, but check whether
      ;; it's a heading (which we can determine if the next entry has
      ;; more asterisks).
      (let ((level (length (match-string 1))))
        (when (save-excursion
                (re-search-forward "^\\(\\*+\\) " nil t))
          (when (<= (length (match-string 1)) level)
            ;; It wasn't a sub-heading, so we've found one.
            (setq found t)))))
    (if found
        (progn
          (message "Untagged entry")
          (beginning-of-line))
      (message "No further untagged entries")
      (goto-char start))))

(defun emacs-news--buttonize ()
  "Make manual and symbol references into buttons."
  (save-excursion
    (with-silent-modifications
      (let ((inhibit-read-only t))
        ;; Do functions and variables.
        (goto-char (point-min))
        (search-forward "\f" nil t)
        (while (re-search-forward "'\\([^-][^ \t\n]+\\)'" nil t)
          ;; Filter out references to key sequences.
          (let ((string (match-string 1)))
            (unless (key-valid-p string)
              (when-let ((symbol (intern-soft string)))
                (when (or (boundp symbol)
                          (fboundp symbol))
                  (buttonize-region (match-beginning 1) (match-end 1)
                                    (lambda (symbol)
                                      (describe-symbol symbol))
                                    symbol))))))
        ;; Do manual references.
        (goto-char (point-min))
        (search-forward "\f" nil t)
        (while (re-search-forward "\"\\(([a-z0-9]+)[ \n][^\"]\\{1,80\\}\\)\""
                                  nil t)
          (buttonize-region (match-beginning 1) (match-end 1)
                            (lambda (node) (info node))
                            (match-string 1)))))))

(provide 'emacs-news-mode)

;;; emacs-news-mode.el ends here
