;;; glyphless-mode.el --- minor mode for displaying glyphless characters  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2024 Free Software Foundation, Inc.

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

(defcustom glyphless-mode-types '(all)
  "Which glyphless characters to display.
The value can be any of the groups supported by
`glyphless-char-display-control' (which see), and in addition
`all', for all glyphless characters."
  :version "29.1"
  :type '(repeat (choice (const :tag "All" all)
                         (const :tag "No font" no-font)
                         (const :tag "C0 Control" c0-control)
                         (const :tag "C1 Control" c1-control)
                         (const :tag "Format Control" format-control)
                         (const :tag "Bidirectional Control" bidi-control)
                         (const :tag "Variation Selectors" variation-selectors)
                         (const :tag "No Font" no-font)))
  :group 'display)

;;;###autoload
(define-minor-mode glyphless-display-mode
  "Minor mode for displaying glyphless characters in the current buffer.
If enabled, all glyphless characters will be displayed as boxes
that display their acronyms."
  :lighter " Glyphless"
  (if glyphless-display-mode
      (progn
        (setq-local glyphless-char-display
                    (let ((table (make-display-table)))
                      (set-char-table-parent table glyphless-char-display)
                      table))
        (glyphless-mode--setup))
    (kill-local-variable 'glyphless-char-display)))

(defun glyphless-mode--setup ()
  (let ((types (if (memq 'all glyphless-mode-types)
                   '(c0-control c1-control format-control
                                variation-selectors no-font)
                 glyphless-mode-types)))
    (when types
      (update-glyphless-char-display
       nil (mapcar (lambda (e) (cons e 'acronym)) types)))))

(provide 'glyphless-mode)

;;; glyphless-mode.el ends here
