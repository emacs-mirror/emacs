;;; word-wrap-mode.el --- minor mode for `word-wrap' tweaks  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026 Free Software Foundation, Inc.

;; Maintainer: emacs-devel@gnu.org

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

;; The list below lists all characters that have a general-category of
;; Zs, but with the ones we don't want to add here commented out.
(defcustom word-wrap-whitespace-characters
  '(;;?\N{SPACE}
    ;;?\N{NO-BREAK SPACE}
    ?\N{OGHAM SPACE MARK}
    ?\N{EN QUAD}
    ?\N{EM QUAD}
    ?\N{EN SPACE}
    ?\N{EM SPACE}
    ?\N{THREE-PER-EM SPACE}
    ?\N{FOUR-PER-EM SPACE}
    ?\N{SIX-PER-EM SPACE}
    ?\N{FIGURE SPACE}
    ?\N{PUNCTUATION SPACE}
    ?\N{THIN SPACE}
    ?\N{HAIR SPACE}
    ;;?\N{NARROW NO-BREAK SPACE}
    ?\N{MEDIUM MATHEMATICAL SPACE}
    ?\N{IDEOGRAPHIC SPACE}
    ;; Not in the Zs category:
    ?\N{ZERO WIDTH SPACE})
  "Characters that `word-wrap-whitespace-mode' should add to `word-wrap'."
  :version "29.1"
  :type '(repeat character)
  :group 'display)

(defvar word-wrap-mode--previous-state)

;;;###autoload
(define-minor-mode word-wrap-whitespace-mode
  "Allow `word-wrap' to fold on all breaking whitespace characters.
The characters to break on are defined by `word-wrap-whitespace-characters'."
  :group 'display
  (if word-wrap-whitespace-mode
      (progn
        (setq-local word-wrap-mode--previous-state
                    (cons (category-table)
                          (buffer-local-set-state
                           word-wrap-by-category t
                           word-wrap t)))
        (set-category-table (copy-category-table))
        (dolist (char word-wrap-whitespace-characters)
          (modify-category-entry char ?|)))
    (set-category-table (car word-wrap-mode--previous-state))
    (buffer-local-restore-state (cdr word-wrap-mode--previous-state))))

;;;###autoload
(define-globalized-minor-mode global-word-wrap-whitespace-mode
  word-wrap-whitespace-mode word-wrap-whitespace-mode
  :group 'display)

(provide 'word-wrap-mode)

;;; word-wrap-mode.el ends here
