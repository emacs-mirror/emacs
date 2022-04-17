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
  "C-c C-s" #'emacs-news-next-untagged-entry
  "C-c C-r" #'emacs-news-previous-untagged-entry
  "C-c C-g" #'emacs-news-goto-section
  "C-c C-f" #'emacs-news-find-heading
  "C-c C-n" #'emacs-news-count-untagged-entries)

(defvar emacs-news-mode-font-lock-keywords
  `(("^---$" 0 'emacs-news-does-not-need-documentation)
    ("^\\+\\+\\+$" 0 'emacs-news-is-documented)))

(defun emacs-news--mode-common ()
  (setq-local font-lock-defaults '(emacs-news-mode-font-lock-keywords t))
  (setq-local outline-regexp "^\\*+ "
              outline-minor-mode-cycle t
              outline-minor-mode-highlight 'append)
  (outline-minor-mode))

;;;###autoload
(define-derived-mode emacs-news-mode text-mode "NEWS"
  "Major mode for editing the Emacs NEWS file."
  (setq-local fill-paragraph-function #'emacs-news--fill-paragraph)
  (emacs-news--mode-common))

;;;###autoload
(define-derived-mode emacs-news-view-mode special-mode "NEWS"
  "Major mode for viewing the Emacs NEWS file."
  (setq buffer-read-only t)
  (emacs-news--buttonize)
  (button-mode)
  (emacs-news--mode-common))

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

(defun emacs-news-next-untagged-entry (&optional reverse)
  "Go to the next untagged NEWS entry.
If REVERSE (interactively, the prefix), go to the previous
untagged NEWS entry."
  (interactive "P" emacs-news-mode)
  (let ((start (point))
        (found nil))
    ;; Don't consider the current line, because that would stop
    ;; progress if calling this command repeatedly.
    (unless reverse
      (forward-line 1))
    (while (and (not found)
                (funcall (if reverse #'re-search-backward
                           #'re-search-forward)
                         "^\\(\\*+\\) " nil t))
      (when (and (not (save-excursion
                        (forward-line -1)
                        (looking-at "---$\\|\\+\\+\\+$")))
                 ;; We have an entry without a tag before it, but
                 ;; check whether it's a heading (which we can
                 ;; determine if the next entry has more asterisks).
                 (not (emacs-news--heading-p)))
        ;; It wasn't a sub-heading, so we've found one.
        (setq found t)))
    (if found
        (progn
          (push-mark start)
          (message "Untagged entry")
          (beginning-of-line)
          t)
      (message "No further untagged entries")
      (goto-char start)
      nil)))

(defun emacs-news--heading-p ()
  (save-excursion
    (beginning-of-line)
    ;; A heading starts with * characters, and then a blank line, and
    ;; then paragraphs with more * characters than in the heading.
    (and (looking-at "\\(\\*+\\) ")
         (let ((level (length (match-string 1))))
           (forward-line 1)
           (and (looking-at "$")
                (re-search-forward "^\\(\\*+\\) " nil t)
                (> (length (match-string 1)) level))))))

(defun emacs-news-previous-untagged-entry ()
  "Go to the previous untagged NEWS entry."
  (interactive nil emacs-news-mode)
  (emacs-news-next-untagged-entry t))

(defun emacs-news-count-untagged-entries ()
  "Say how many untagged entries there are in the current NEWS buffer."
  (interactive nil emacs-news-mode)
  (save-excursion
    (goto-char (point-min))
    (let ((i 0))
      (while (emacs-news-next-untagged-entry)
        (setq i (1+ i)))
      (message (if (= i 1)
                   "There's 1 untagged entry"
                 (format "There's %s untagged entries" i))))))

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
            (when-let ((symbol (intern-soft string)))
              (when (or (boundp symbol)
                        (fboundp symbol))
                (buttonize-region (match-beginning 1) (match-end 1)
                                  (lambda (symbol)
                                    (describe-symbol symbol))
                                  symbol)))))
        ;; Do manual references.
        (goto-char (point-min))
        (search-forward "\f" nil t)
        (while (re-search-forward "\"\\(([a-z0-9]+)[ \n][^\"]\\{1,80\\}\\)\""
                                  nil t)
          (buttonize-region (match-beginning 1) (match-end 1)
                            (lambda (node) (info node))
                            (match-string 1)))))))

(defun emacs-news--sections (regexp)
  (let ((sections nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward (concat "^" regexp "\\(.*\\)") nil t)
        (when (save-match-data (emacs-news--heading-p))
          (push (buffer-substring-no-properties
                 (match-beginning 1) (match-end 1))
                sections))))
    (nreverse sections)))

(defun emacs-news-goto-section (section)
  "Go to SECTION in the Emacs NEWS file."
  (interactive (list
                (completing-read "Goto section: " (emacs-news--sections "\\* ")
                                 nil t))
               emacs-news-mode)
  (goto-char (point-min))
  (when (search-forward (concat "\n* " section) nil t)
    (beginning-of-line)))

(defun emacs-news-find-heading (heading)
  "Go to HEADING in the Emacs NEWS file."
  (interactive (list
                (completing-read "Goto heading: "
                                 (emacs-news--sections "\\*\\*\\*? ")
                                 nil t))
               emacs-news-mode)
  (goto-char (point-min))
  (when (re-search-forward (concat "^*+ " (regexp-quote heading)) nil t)
    (beginning-of-line)))

(provide 'emacs-news-mode)

;;; emacs-news-mode.el ends here
