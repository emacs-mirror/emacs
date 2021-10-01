;;; erts-mode.el --- major mode to edit erts files  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

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

(defgroup erts-mode nil
  "Major mode for editing Emacs test files."
  :group 'lisp)

(defface erts-mode-specification-name
  '((((class color)
      (background dark))
     :foreground "green")
    (((class color)
      (background light))
     :foreground "cornflower blue")
    (t
     :bold t))
  "Face used for displaying specification names."
  :group 'erts-mode)

(defface erts-mode-specification-value
  '((((class color)
      (background dark))
     :foreground "DeepSkyBlue1")
    (((class color)
      (background light))
     :foreground "blue")
    (t
     :bold t))
  "Face used for displaying specificaton values."
  :group 'erts-mode)

(defface erts-mode-start-test
  '((t :inherit font-lock-keyword-face))
  "Face used for displaying specificaton test start markers."
  :group 'erts-mode)

(defface erts-mode-end-test
  '((t :inherit font-lock-comment-face))
  "Face used for displaying specificaton test start markers."
  :group 'erts-mode)

(defvar erts-mode-map
  (let ((map (make-keymap)))
    (set-keymap-parent map prog-mode-map)
    map))

(defvar erts-mode-font-lock-keywords
  ;; Specifications.
  `((erts-mode--match-not-in-test
     ("^\\([^ \t\n:]+:\\)[ \t]*\\(.*\\(\n[ \t].*\\)*\\)\n?"
      (progn (goto-char (match-beginning 0)) (match-end 0)) nil
      (1 'erts-mode-specification-name)
      (2 'erts-mode-specification-value)))
    ("^=-=$" 0 'erts-mode-start-test)
    ("^=-=-=$" 0 'erts-mode-end-test)))

(defun erts-mode--match-not-in-test (_limit)
  (when (erts-mode--in-test-p (point))
    (erts-mode--end-of-test))
  (let ((start (point)))
    (goto-char
     (if (re-search-forward "^=-=$" nil t)
         (match-beginning 0)
       (point-max)))
    (if (< (point) start)
        nil
      ;; Here we disregard LIMIT so that we may extend the area again.
      (set-match-data (list start (point)))
      (point))))

(defun erts-mode--end-of-test ()
  (search-forward "^=-=-=\n" nil t))

(defun erts-mode--in-test-p (point)
  "Say whether POINT is in a test."
  (save-excursion
    (goto-char point)
    (beginning-of-line)
    (if (looking-at "=-=\\(-=\\)?$")
        t
      (let ((test-start (re-search-backward "^=-=\n" nil t)))
        ;; Before the first test.
        (and test-start
             (let ((test-end (re-search-backward "^=-=-=\n" nil t)))
               (or (null test-end)
                   ;; Between tests.
                   (> test-start test-end))))))))

;;;###autoload
(define-derived-mode erts-mode prog-mode "erts"
  "Major mode for editing erts (Emacs testing) files.
This mode mainly provides some font locking."
  (setq-local font-lock-defaults '(erts-mode-font-lock-keywords t)))

(provide 'erts-mode)

;;; erts-mode.el ends here
