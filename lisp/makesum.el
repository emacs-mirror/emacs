;;; makesum.el --- generate key binding summary for Emacs  -*- lexical-binding:t -*-

;; Copyright (C) 1985, 2001-2020 Free Software Foundation, Inc.

;; Maintainer: emacs-devel@gnu.org
;; Keywords: help

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

;; Displays a nice human-readable summary of all keybindings in a
;; two-column format.

;;; Code:

;;;###autoload
(defun make-command-summary ()
  "Make a summary of current key bindings in the buffer *Summary*.
Previous contents of that buffer are killed first."
  (interactive)
  (message "Making command summary...")
  ;; This puts a description of bindings in a buffer called *Help*.
  (save-window-excursion
   (describe-bindings))
  (with-output-to-temp-buffer "*Summary*"
    (save-excursion
     (let ((cur-mode mode-name))
       (set-buffer standard-output)
       (erase-buffer)
       (insert-buffer-substring "*Help*")
       (goto-char (point-min))
       (delete-region (point) (progn (forward-line 1) (point)))
       (while (search-forward "         " nil t)
	 (replace-match "  "))
       (goto-char (point-min))
       (while (search-forward "-@ " nil t)
	 (replace-match "-SP"))
       (goto-char (point-min))
       (while (search-forward "  .. ~ " nil t)
	 (replace-match "SP .. ~"))
       (goto-char (point-min))
       (while (search-forward "C-?" nil t)
	 (replace-match "DEL"))
       (goto-char (point-min))
       (while (search-forward "C-i" nil t)
	 (replace-match "TAB"))
       (goto-char (point-min))
       (when (re-search-forward "^Local Bindings:" nil t)
         (forward-char -1)
         (insert " for " (format-mode-line cur-mode) " Mode")
         (while (search-forward "??\n" nil t)
           (delete-region (point)
                          (progn
                            (forward-line -1)
                            (point)))))
       (goto-char (point-min))
       (insert "Emacs command summary, " (substring (current-time-string) 0 10)
	       ".\n")
       ;; Delete "key    binding" and underlining of dashes.
       (delete-region (point) (progn (forward-line 2) (point)))
       (forward-line 1)			;Skip blank line
       (while (not (eobp))
	 (let ((beg (point)))
	   (or (re-search-forward "^$" nil t)
	       (goto-char (point-max)))
	   (double-column beg (point))
	   (forward-line 1)))
       (goto-char (point-min)))))
  (message "Making command summary...done"))

(defun double-column (start end)
  "Reformat buffer contents from START to END into two columns."
  (interactive "r")
  (let (half lines
        (nlines (count-lines start end))
	(from-end (- (point-max) end)))
    (when (> nlines 1)
      (setq half (/ (1+ nlines) 2))
      (goto-char start)
      (save-excursion
       (forward-line half)
       (dotimes (_ (- nlines half))
         (push (buffer-substring (point) (line-end-position))
               lines)
	 (delete-region (point) (progn (forward-line 1) (point)))))
      (dolist (line (nreverse lines))
        (end-of-line)
	(indent-to 41)
        (insert line)
        (forward-line 1)))
    (goto-char (- (point-max) from-end))))

(provide 'makesum)

;;; makesum.el ends here
