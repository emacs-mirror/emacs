;;; all.el --- Edit all lines matching a given regexp

;; Copyright (C) 1985-1987,1992,1994,2011-2012 Free Software Foundation, Inc.

;; Author: Per Abrahamsen <abraham@dina.kvl.dk>
;; Version: 1.0
;; Keywords: matching

;; LCD Archive Entry:
;; all|Per Abrahamsen|abraham@dina.kvl.dk|
;; Edit all lines matching a given regexp|
;; $Date: 1997/03/04 10:29:42 $|$Revision: 5.2 $|~/misc/all.Z|

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Just like occur, except that changes in the *All* buffer are
;; propagated to the original buffer.

;; You can no longer use mouse-2 to find a match in the original file,
;; since the default definition of mouse is too useful.
;; However, `C-c C-c' still works.

;; Line numbers are not listed in the *All* buffer.

;; Ok, it is _not_ just like occur.

;; Some limitations:

;; - Undo in the *All* buffer is an ordinary change in the original.
;; - Changes to the original buffer are not reflected in the *All* buffer.
;; - A single change in the *All* buffer must be limited to a single match.

;;; Code: 

(defvar all-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'all-mode-goto)
    map))

(defvar all-buffer nil)
(make-variable-buffer-local 'all-buffer)

(define-derived-mode all-mode fundamental-mode "All"
  "Major mode for output from \\[all].

All changes made in this buffer will be propagated to the buffer where
you ran \\[all].

Press \\[all-mode-goto] to go to the same spot in the original buffer."
  (add-hook 'before-change-functions 'all-before-change-function nil 'local)
  (add-hook 'after-change-functions 'all-after-change-function nil 'local))

(defun all-mode-find (pos)
  ;; Find position in original buffer corresponding to POS.
  (let ((overlay (all-mode-find-overlay pos)))
    (if overlay
	(+ (marker-position (overlay-get overlay 'all-marker))
	   (- pos (overlay-start overlay))))))

(defun all-mode-find-overlay (pos)
  ;; Find the overlay containing POS.
  (let ((overlays (overlays-at pos)))
    (while (and overlays (null (overlay-get (car overlays) 'all-marker)))
      (setq overlays (cdr overlays)))
    (car-safe overlays)))

(defun all-mode-goto ()
  "Move point to the corresponding position in the original buffer."
  (interactive)
  (let ((pos (all-mode-find (point))))
    (if pos
	(pop-to-buffer all-buffer)
      (error "This text is not from the original buffer"))
    (goto-char pos)))

(defvar all-initialization-p nil)

(defun all-before-change-function (from to)
  ;; Check that change is legal.
  (and all-buffer
       (not all-initialization-p)
       (let ((start (all-mode-find-overlay from))
	     (end (all-mode-find-overlay to)))
	 (not (and start (eq start end))))
       (error "Changes should be limited to a single text piece")))

(defun all-after-change-function (from to length)
  ;; Propagate changes from *All* buffer.
  (and all-buffer
       (null all-initialization-p)
       (let ((buffer (current-buffer))
	     (pos (all-mode-find from)))
	 (if pos
	     (with-current-buffer all-buffer
	       (save-excursion
		 (goto-char pos)
                 (delete-region pos (+ pos length))
		 (insert-buffer-substring buffer from to)))))))

;;;###autoload
(defun all (regexp &optional nlines)
  "Show all lines in the current buffer containing a match for REGEXP.

If a match spreads across multiple lines, all those lines are shown.

Each line is displayed with NLINES lines before and after, or -NLINES
before if NLINES is negative.
NLINES defaults to `list-matching-lines-default-context-lines'.
Interactively it is the prefix arg.

The lines are shown in a buffer named `*All*'.
Any changes made in that buffer will be propagated to this buffer."
  (interactive
   (list (let* ((default (car regexp-history)))
           (read-string
            (if default
                (format 
                 "Edit lines matching regexp (default `%s'): " default)
              "Edit lines matching regexp: ")
            nil 'regexp-history default))
         current-prefix-arg))
  (setq nlines (if nlines (prefix-numeric-value nlines)
		 list-matching-lines-default-context-lines))
  (let ((all-initialization-p t)
        (buffer (current-buffer))
	(prevend nil)
	(prevstart nil)
	(prevpos (point-min)))
    (with-output-to-temp-buffer "*All*"
      (with-current-buffer standard-output
	(all-mode)
	(setq all-buffer buffer)
	(insert "Lines matching ")
	(prin1 regexp)
	(insert " in buffer " (buffer-name buffer) ?. ?\n)
	(insert "--------\n"))
      (if (eq buffer standard-output)
	  (goto-char (point-max)))
      (save-excursion
	(goto-char (point-min))
	;; Find next match, but give up if prev match was at end of buffer.
	(while (and (not (= prevpos (point-max)))
		    (re-search-forward regexp nil t))
	  (goto-char (match-beginning 0))
	  (beginning-of-line)
	  (setq prevpos (point))
	  (goto-char (match-end 0))
	  (let* ((start (save-excursion
			  (goto-char (match-beginning 0))
			  (forward-line (if (< nlines 0) nlines (- nlines)))
			  (point)))
		 (end (save-excursion
			(goto-char (match-end 0))
			(if (> nlines 0)
			    (forward-line (1+ nlines))
			    (forward-line 1))
			(point))))
	    (cond ((null prevend)
		   (setq prevstart start
			prevend end))
		  ((> start prevend)
		   (all-insert prevstart prevend regexp nlines)
		   (setq prevstart start
			 prevend end))
		  (t
		   (setq prevend end)))))
	(if prevend
	    (all-insert prevstart prevend regexp nlines))))))

(defun all-insert (start end regexp nlines)
  ;; Insert match.
  (let ((marker (copy-marker start))
        (buffer (current-buffer)))
    (with-current-buffer standard-output
      (let ((from (point))
            to)
        (insert-buffer-substring buffer start end)
        (setq to (point))
        (overlay-put (make-overlay from to) 'all-marker marker)
        (goto-char from)
        (while (re-search-forward regexp to t)
          (put-text-property (match-beginning 0) (match-end 0)
                             'face 'match))
        (goto-char to)
        (if (> nlines 0)
            (insert "--------\n"))))))

(provide 'all)

;;; all.el ends here
