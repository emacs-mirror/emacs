;;; elide-head.el --- hide headers in files  -*- lexical-binding: t; -*-

;; Copyright (C) 1999, 2001-2026 Free Software Foundation, Inc.

;; Author: Dave Love <fx@gnu.org>
;; Keywords: outlines tools

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

;; Functionality for eliding boilerplate text (normally copyright
;; notices) in file headers to avoid clutter when you know what it
;; says.
;;
;; `elide-head-headers-to-hide' controls what is elided by the minor
;; mode `elide-head-mode'.  A buffer-local invisible overlay manages
;; the elision.

;; You might add `elide-head-mode' to appropriate major mode hooks or
;; to `find-file-hook'.  Please do not do this in site init files.  If
;; you do, information may be hidden from users who don't know it
;; already.

;; Note that `hs-minor-mode' will do a similar job by default, but
;; it's not selective about what leading commentary it hides.

;; Inspired by jwz's hide-copyleft.el, for which we don't have an
;; assignment.

;;; Code:

(defgroup elide-head nil
  "Eliding copyright headers and the like in source files."
  :version "21.1"
  :prefix "elide-head"
  :group 'tools)

(defcustom elide-head-headers-to-hide
  (rx-let ((delim
            ;; A line break could be in a non-standard place, and the
            ;; license could be in a comment.
            (or
             ;; Either just some spaces:
             (+ " ")
             ;; Or a newline and some comment starter:
             (: (* (in " \t"))
                "\n"
                (* (in " \t"))
                (* (or (syntax comment-start) (in ";#*-")))
                (* (in " \t"))))))
    `(;; GNU GPL
      ("is free software[:;] you can redistribute it" .
       ,(rx (or (seq "If not, see " (? "<")
                     "http" (? "s") "://www.gnu.org/licenses"
                     (? "/") (? ">") (? " "))
                (seq "Boston," delim "MA" delim
                     (or "02111-1307" "02110-1301" "02111-1301" "02196")
                     (? ",") delim
                     "USA")
                "675 Mass Ave, Cambridge, MA 02139, USA")
            (? ".")))
      ;; FreeBSD license / Modified BSD license (3-clause)
      (,(rx (or "The Regents of the University of California.  All rights reserved."
                "Redistribution and use in source and binary"))
       . "POSSIBILITY OF SUCH DAMAGE\\.")
      ;; X11 and Expat
      ("Permission is hereby granted, free of charge" .
       ,(rx (or "authorization from the X Consortium."          ; X11
                "THE USE OR OTHER DEALINGS IN THE SOFTWARE."))) ; Expat
      ;; Apache
      ("Licensed under the Apache License, Version 2.0" .
       "limitations under the License.")
      ))
  "Alist of regexps defining start and end of text to elide.

The cars of elements of the list are searched for in order.  Text is
elided with an invisible overlay from the end of the line where the
first match is found to the end of the match for the corresponding
cdr.

This affects `elide-head-mode'."
  :type '(alist :key-type  (regexp :tag "Start regexp")
                :value-type (regexp :tag "End regexp"))
  :version "30.1")

(defvar-local elide-head-overlay nil)

(defun elide-head--delete-overlay ()
  "Delete the overlay in `elide-head-overlay'."
  (when (overlayp elide-head-overlay)
    (delete-overlay elide-head-overlay)))

(defun elide-head--hide ()
  "Hide elided (hidden) headers."
  (save-excursion
    (save-restriction
      (let ((rest elide-head-headers-to-hide)
            beg end)
        (widen)
        (goto-char (point-min))
        (while rest
          (save-excursion
            (when (re-search-forward (caar rest) nil t)
              (setq beg (point))
              (when (re-search-forward (cdar rest) nil t)
                (setq end (point-marker)
                      rest nil))))
          (if rest (setq rest (cdr rest))))
        (if (not (and beg end))
            (if (called-interactively-p 'interactive)
                (message "No header found"))
          (goto-char beg)
          (end-of-line)
          (if (overlayp elide-head-overlay)
              (move-overlay elide-head-overlay (point-marker) end)
            (setq elide-head-overlay (make-overlay (point-marker) end)))
          (overlay-put elide-head-overlay 'invisible t)
          (overlay-put elide-head-overlay 'evaporate t)
          (overlay-put elide-head-overlay 'after-string "..."))))))

(defun elide-head--show ()
  "Show elided (hidden) headers."
  (if (and (overlayp elide-head-overlay)
           (overlay-buffer elide-head-overlay))
      (elide-head--delete-overlay)
    (if (called-interactively-p 'interactive)
        (message "No header hidden"))))

;;;###autoload
(define-minor-mode elide-head-mode
  "Toggle eliding (hiding) header material in the current buffer.

When Elide Header mode is enabled, headers are hidden according
to `elide-head-headers-to-hide'.

This is suitable as an entry on `find-file-hook' or appropriate
mode hooks."
  :group 'elide-head
  (if elide-head-mode
      (progn
        (elide-head--hide)
        (add-hook 'change-major-mode-hook 'elide-head--delete-overlay nil 'local))
    (elide-head--show)
    (remove-hook 'change-major-mode-hook 'elide-head--delete-overlay 'local)))


;;; Obsolete

;;;###autoload
(defun elide-head (&optional arg)
  "Hide header material in buffer according to `elide-head-headers-to-hide'.

The header is made invisible with an overlay.  With a prefix
argument ARG, show an elided material again.

This is suitable as an entry on `find-file-hook' or appropriate
mode hooks."
  (declare (obsolete elide-head-mode "29.1"))
  (interactive "P")
  (if arg
      (elide-head-mode -1)
    (elide-head-mode 1)))

(defun elide-head-show ()
  "Show a header in the current buffer elided by \\[elide-head]."
  (declare (obsolete elide-head-mode "29.1"))
  (interactive)
  (elide-head-mode -1))

(provide 'elide-head)

;;; elide-head.el ends here
