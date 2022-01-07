;;; shorthands.el --- Read code considering Elisp shorthands  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2022 Free Software Foundation, Inc.

;; Author: João Távora <joaotavora@gmail.com>
;; Keywords: lisp

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

;; Basic helpers for loading files with Shorthands.

;;; Code:
(require 'files)
(require 'mule)
(eval-when-compile (require 'cl-lib))

(defun hack-read-symbol-shorthands ()
  "Compute `read-symbol-shorthands' from Local Variables section."
  ;; FIXME: relies on the `hack-local-variables--find-variables'
  ;; detail of files.el.  That function should be exported,
  ;; possibly be refactored into two parts, since we're only
  ;; interested in basic "Local Variables" parsing.
  (alist-get 'read-symbol-shorthands (hack-local-variables--find-variables)))

(setq hack-read-symbol-shorthands-function #'hack-read-symbol-shorthands)


;; FIXME: move this all to progmodes/elisp-mode.el?  OTOH it'd make
;; more sense there, OTOH all the elisp font-lock stuff is actually in
;; lisp/emacs-lisp/lisp-mode.el, which isn't right either.  So
;; shorthand font-locking logic is probably better here for now.

(defface elisp-shorthand-font-lock-face
  '((t :inherit font-lock-keyword-face :foreground "cyan"))
  "Face for highlighting shorthands in Emacs Lisp."
  :version "28.1"
  :group 'font-lock-faces)

(defun shorthands--mismatch-from-end (str1 str2)
  (cl-loop with l1 = (length str1) with l2 = (length str2)
           for i from 1
           for i1 = (- l1 i) for i2 = (- l2 i)
           while (and (>= i1 0) (>= i2 0) (eq (aref str1 i1) (aref str2 i2)))
           finally (return (1- i))))

(defun shorthands-font-lock-shorthands (limit)
  (when read-symbol-shorthands
    (while (re-search-forward
            (eval-when-compile
              (concat "\\_<\\(" lisp-mode-symbol-regexp "\\)\\_>"))
            limit t)
      (let* ((existing (get-text-property (match-beginning 1) 'face))
             (probe (and (not (memq existing '(font-lock-comment-face
                                               font-lock-string-face)))
                         (intern-soft (match-string 1))))
             (sname (and probe (symbol-name probe)))
             (mm (and sname (shorthands--mismatch-from-end
                             (match-string 1) sname))))
        (unless (or (null mm) (= mm (length sname)))
          (add-face-text-property (match-beginning 1) (1+ (- (match-end 1) mm))
                                  'elisp-shorthand-font-lock-face))))))

(font-lock-add-keywords 'emacs-lisp-mode '((shorthands-font-lock-shorthands)) t)

;;; shorthands.el ends here
