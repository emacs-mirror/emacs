;;; tutorial.el --- tutorial for Emacs

;; Copyright (C) 2006-2017 Free Software Foundation, Inc.

;; Maintainer: emacs-devel@gnu.org
;; Keywords: help, internal
;; Package: emacs

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Code for running the Emacs Tutorials written in org-mode.

;;; Code:
(require 'subr-x)

(define-derived-mode tutorial-org-mode org-mode "Tutor"
  "A mode for displaying tutorials."
  (show-all)
  (setq-local org-hide-emphasis-markers t))

(defun tutorial-org--display-buffer (tutorial-buffer-name)
  (when-let ((tutorial-buffer
            (get-buffer-window tutorial-buffer-name t)))
    (raise-frame
     (window-frame
      (select-window tutorial-window))))
  (switch-to-buffer tutorial-buffer)
  ;; Use whole frame for tutorial
  (delete-other-windows))

(defun tutorial-org-display (org-file)
  "Display the org-file as a tutorial"
  (let* ((tutorial-buffer-name
          (file-name-nondirectory
           (file-name-sans-extension
            org-file)))
         (tutorial-buffer (get-buffer-create tutorial-buffer-name)))
    ;; Display it
    (tutorial-org--display-buffer tutorial-buffer)
    ;; Fill it if needed
    (when (= 0 (buffer-size tutorial-buffer))
      (insert-file-contents org-file)
      (tutorial-org-mode))))

(defun tutorial-org--help-with-tutorial-org (lang)
  (tutorial-org-display
   (expand-file-name
    (get-language-info lang 'tutorial-org)
    tutorial-org-directory)))

(provide 'tutorial-org)

;;; tutorial-org.el ends here
