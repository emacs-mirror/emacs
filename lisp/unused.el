;;; unused.el --- editing commands in GNU Emacs that turned out not to be used.
;;; This file is in the public domain, as it was distributed in
;;; 1985 or 1986 without a copyright notice.  Written by RMS.

;; Maintainer: FSF
;; Keywords: emulations

;;; Commentary:

;; These were added with an eye to making possible a more CCA-compatible
;; command set; but that turned out not to be interesting.

;;; Code:

(defun mark-beginning-of-buffer ()
  "Set mark at the beginning of the buffer."
  (interactive)
  (push-mark (point-min)))

(defun mark-end-of-buffer ()
  "Set mark at the end of the buffer."
  (interactive)
  (push-mark (point-max)))

(defun upcase-char (arg)
  "Uppercasify ARG chars starting from point.  Point doesn't move"
  (interactive "p")
  (save-excursion
    (upcase-region (point) (progn (forward-char arg) (point)))))

(defun forward-to-word (arg)
  "Move forward until encountering the beginning of a word.
With argument, do this that many times."
  (interactive "p")
  (or (re-search-forward (if (> arg 0) "\\W\\b" "\\b\\W") nil t arg)
      (goto-char (if (> arg 0) (point-max) (point-min)))))

(defun backward-to-word (arg)
  "Move backward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (forward-to-word (- arg)))

;;; unused.el ends here
