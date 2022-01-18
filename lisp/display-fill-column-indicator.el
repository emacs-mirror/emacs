;;; display-fill-column-indicator.el --- interface for display-fill-column-indicator -*- lexical-binding: t -*-

;; Copyright (C) 2019-2022 Free Software Foundation, Inc.

;; Maintainer: emacs-devel@gnu.org
;; Keywords: convenience

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

;; Provides a minor mode interface for `display-fill-column-indicator'.
;;
;; Toggle display of the column indicator with M-x
;; display-fill-column-indicator-mode.  To enable the indicator in
;; all buffers, use M-x global-display-fill-column-indicator-mode.


;; NOTE: Customization variables for
;; `display-fill-column-indicator-column' and
;; `display-fill-column-indicator-character' itself are defined in
;; cus-start.el.

;;; Code:

(defgroup display-fill-column-indicator nil
  "Display a fill column indicator in the buffer."
  :group 'convenience
  :group 'display
  :link '(info-link "(emacs)Displaying Boundaries"))


;;;###autoload
(define-minor-mode display-fill-column-indicator-mode
  "Toggle display of `fill-column' indicator.
This uses `display-fill-column-indicator' internally.

To change the position of the column displayed by default
customize `display-fill-column-indicator-column'.  You can change the
character for the indicator setting `display-fill-column-indicator-character'.
The globalized version is `global-display-fill-column-indicator-mode',
which see.
See Info node `Displaying Boundaries' for details."
  :lighter nil
  (if display-fill-column-indicator-mode
      (progn
        (setq display-fill-column-indicator t)
        (unless display-fill-column-indicator-character
          (setq display-fill-column-indicator-character
                (if (and (char-displayable-p ?\u2502)
                         (or (not (display-graphic-p))
                             (eq (aref (query-font (car (internal-char-font nil ?\u2502))) 0)
                                 (face-font 'default))))
                    ?\u2502
                  ?|))))
    (setq display-fill-column-indicator nil)))

(defun display-fill-column-indicator--turn-on ()
  "Turn on `display-fill-column-indicator-mode'."
  (unless (or (minibufferp)
              (and (daemonp) (null (frame-parameter nil 'client))))
    (display-fill-column-indicator-mode)))

;;;###autoload
(define-globalized-minor-mode global-display-fill-column-indicator-mode
  display-fill-column-indicator-mode display-fill-column-indicator--turn-on
  :predicate '((not special-mode) t))

(provide 'display-fill-column-indicator)

;;; display-fill-column-indicator.el ends here
