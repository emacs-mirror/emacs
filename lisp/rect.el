;;; rect.el --- rectangle functions for GNU Emacs  -*- lexical-binding:t -*-

;; Copyright (C) 1985, 1999-2018 Free Software Foundation, Inc.

;; Maintainer: Didier Verna <didier@xemacs.org>
;; Keywords: internal
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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides the operations on rectangles that are documented
;; in the Emacs manual.

;; ### NOTE: this file was almost completely rewritten by Didier Verna
;; <didier@xemacs.org> in July 1999.

;;; Code:

(eval-when-compile (require 'cl-lib))

(defgroup rectangle nil
  "Operations on rectangles."
  :version "25.1"
  :group 'editing)

;; FIXME: this function should be replaced by `apply-on-rectangle'
(defun operate-on-rectangle (function start end coerce-tabs)
  "Call FUNCTION for each line of rectangle with corners at START, END.
If COERCE-TABS is non-nil, convert multi-column characters
that span the starting or ending columns on any line
to multiple spaces before calling FUNCTION.
FUNCTION is called with three arguments:
 position of start of segment of this line within the rectangle,
 number of columns that belong to rectangle but are before that position,
 number of columns that belong to rectangle but are after point.
Point is at the end of the segment of this line within the rectangle."
  (apply-on-rectangle
   (lambda (startcol endcol)
     (let (startpos begextra endextra)
       (move-to-column startcol coerce-tabs)
       (setq begextra (- (current-column) startcol))
       (setq startpos (point))
       (move-to-column endcol coerce-tabs)
       ;; If we overshot, move back one character
       ;; so that endextra will be positive.
       (if (and (not coerce-tabs) (> (current-column) endcol))
           (backward-char 1))
       (setq endextra (- endcol (current-column)))
       (if (< begextra 0)
           (setq endextra (+ endextra begextra)
                 begextra 0))
       (funcall function startpos begextra endextra)))
   start end))

;;; Crutches to let rectangle's corners be where point can't be
;; (e.g. in the middle of a TAB, or past the EOL).

(defvar-local rectangle--mark-crutches nil
  "(POS . COL) to override the column to use for the mark.")

(defun rectangle--pos-cols (start end &optional window)
  ;; At this stage, we don't know which of start/end is point/mark :-(
  ;; And in case start=end, it might still be that point and mark have
  ;; different crutches!
  (let ((cw (window-parameter window 'rectangle--point-crutches)))
    (cond
     ((eq start (car cw))
      (let ((sc (cdr cw))
            (ec (if (eq end (car rectangle--mark-crutches))
                    (cdr rectangle--mark-crutches)
                  (if rectangle--mark-crutches
                      (setq rectangle--mark-crutches nil))
                  (goto-char end) (current-column))))
        (if (eq start end) (cons (min sc ec) (max sc ec)) (cons sc ec))))
     ((eq end (car cw))
      (if (eq start (car rectangle--mark-crutches))
          (cons (cdr rectangle--mark-crutches) (cdr cw))
        (if rectangle--mark-crutches (setq rectangle--mark-crutches nil))
        (cons (progn (goto-char start) (current-column)) (cdr cw))))
     ((progn
        (if cw (setf (window-parameter nil 'rectangle--point-crutches) nil))
        (eq start (car rectangle--mark-crutches)))
      (let ((sc (cdr rectangle--mark-crutches))
            (ec (progn (goto-char end) (current-column))))
        (if (eq start end) (cons (min sc ec) (max sc ec)) (cons sc ec))))
     ((eq end (car rectangle--mark-crutches))
      (cons (progn (goto-char start) (current-column))
            (cdr rectangle--mark-crutches)))
     (t
      (if rectangle--mark-crutches (setq rectangle--mark-crutches nil))
      (cons (progn (goto-char start) (current-column))
            (progn (goto-char end) (current-column)))))))

(defun rectangle--col-pos (col kind)
  (let ((c (move-to-column col)))
    (if (and (= c col) (not (eolp)))
        (if (eq kind 'point)
            (if (window-parameter nil 'rectangle--point-crutches)
                (setf (window-parameter nil 'rectangle--point-crutches) nil))
          (if rectangle--mark-crutches (setq rectangle--mark-crutches nil)))
      ;; If move-to-column overshot, move back one char so we're
      ;; at the position where rectangle--highlight-for-redisplay
      ;; will add the overlay (so that the cursor can be drawn at the
      ;; right place).
      (when (> c col) (forward-char -1))
      (setf (if (eq kind 'point)
                (window-parameter nil 'rectangle--point-crutches)
              rectangle--mark-crutches)
            (cons (point) col)))))

(defun rectangle--point-col (pos)
  (let ((pc (window-parameter nil 'rectangle--point-crutches)))
    (if (eq pos (car pc)) (cdr pc)
      (goto-char pos)
      (current-column))))

(defun rectangle--crutches ()
  (cons rectangle--mark-crutches
        (window-parameter nil 'rectangle--point-crutches)))
(defun rectangle--reset-crutches ()
  (kill-local-variable 'rectangle--mark-crutches)
  (if (window-parameter nil 'rectangle--point-crutches)
      (setf (window-parameter nil 'rectangle--point-crutches) nil)))

;;; Rectangle operations.

(defun apply-on-rectangle (function start end &rest args)
  "Call FUNCTION for each line of rectangle with corners at START, END.
FUNCTION is called with two arguments: the start and end columns of the
rectangle, plus ARGS extra arguments.  Point is at the beginning of line when
the function is called.
The final point after the last operation will be returned."
  (save-excursion
    (let* ((cols (rectangle--pos-cols start end))
           (startcol (car cols))
           (endcol (cdr cols))
           (startpt (progn (goto-char start) (line-beginning-position)))
           (endpt (progn (goto-char end)
                         (copy-marker (line-end-position))))
           final-point)
      ;; Ensure the start column is the left one.
      (if (< endcol startcol)
	  (let ((col startcol))
	    (setq startcol endcol endcol col)))
      ;; Start looping over lines.
      (goto-char startpt)
      (while
          (progn
            (apply function startcol endcol args)
            (setq final-point (point))
            (and (zerop (forward-line 1)) (bolp)
                 (<= (point) endpt))))
      final-point)))

(defun delete-rectangle-line (startcol endcol fill)
  (when (= (move-to-column startcol (if fill t 'coerce)) startcol)
    (delete-region (point)
		   (progn (move-to-column endcol 'coerce)
			  (point)))))

(defun delete-extract-rectangle-line (startcol endcol lines fill)
  (let ((pt (point-at-eol)))
    (if (< (move-to-column startcol (if fill t 'coerce)) startcol)
	(setcdr lines (cons (spaces-string (- endcol startcol))
			    (cdr lines)))
      ;; else
      (setq pt (point))
      (move-to-column endcol t)
      (setcdr lines (cons (filter-buffer-substring pt (point) t) (cdr lines))))
    ))

;; This is actually the only function that needs to do complicated
;; stuff like what's happening in `operate-on-rectangle', because the
;; buffer might be read-only.
(defun extract-rectangle-line (startcol endcol lines)
  (let (start end begextra endextra line)
    (move-to-column startcol)
    (setq start (point)
	  begextra (- (current-column) startcol))
    (move-to-column endcol)
    (setq end (point)
	  endextra (- endcol (current-column)))
    (setq line (buffer-substring start (point)))
    (if (< begextra 0)
	(setq endextra (+ endextra begextra)
	      begextra 0))
    (if (< endextra 0)
	(setq endextra 0))
    (goto-char start)
    (while (search-forward "\t" end t)
      (let ((width (- (current-column)
		      (save-excursion (forward-char -1)
				      (current-column)))))
	(setq line (concat (substring line 0 (- (point) end 1))
			   (spaces-string width)
			   (substring line (+ (length line)
					      (- (point) end)))))))
    (if (or (> begextra 0) (> endextra 0))
	(setq line (concat (spaces-string begextra)
			   line
			   (spaces-string endextra))))
    (setcdr lines (cons line (cdr lines)))))

(defconst spaces-strings
  '["" " " "  " "   " "    " "     " "      " "       " "        "])

(defun spaces-string (n)
  "Return a string with N spaces."
  (if (<= n 8) (aref spaces-strings n)
    (make-string n ?\s)))

;;;###autoload
(defun delete-rectangle (start end &optional fill)
  "Delete (don't save) text in the region-rectangle.
The same range of columns is deleted in each line starting with the
line where the region begins and ending with the line where the region
ends.

When called from a program the rectangle's corners are START and END.
With a prefix (or a FILL) argument, also fill lines where nothing has
to be deleted."
  (interactive "*r\nP")
  (apply-on-rectangle 'delete-rectangle-line start end fill))

;;;###autoload
(defun delete-extract-rectangle (start end &optional fill)
  "Delete the contents of the rectangle with corners at START and END.
Return it as a list of strings, one for each line of the rectangle.

When called from a program the rectangle's corners are START and END.
With an optional FILL argument, also fill lines where nothing has to be
deleted."
  (let ((lines (list nil)))
    (apply-on-rectangle 'delete-extract-rectangle-line start end lines fill)
    (nreverse (cdr lines))))

;;;###autoload
(defun extract-rectangle (start end)
  "Return the contents of the rectangle with corners at START and END.
Return it as a list of strings, one for each line of the rectangle."
  (let ((lines (list nil)))
    (apply-on-rectangle 'extract-rectangle-line start end lines)
    (nreverse (cdr lines))))

(defun extract-rectangle-bounds (start end)
  "Return the bounds of the rectangle with corners at START and END.
Return it as a list of (START . END) positions, one for each line of
the rectangle."
  (let (bounds)
    (apply-on-rectangle
     (lambda (startcol endcol)
       (move-to-column startcol)
       (push (cons (prog1 (point) (move-to-column endcol)) (point))
	     bounds))
     start end)
    (nreverse bounds)))

(defvar killed-rectangle nil
  "Rectangle for `yank-rectangle' to insert.")

;;;###autoload
(defun kill-rectangle (start end &optional fill)
  "Delete the region-rectangle and save it as the last killed one.

When called from a program the rectangle's corners are START and END.
You might prefer to use `delete-extract-rectangle' from a program.

With a prefix (or a FILL) argument, also fill lines where nothing has to be
deleted.

If the buffer is read-only, Emacs will beep and refrain from deleting
the rectangle, but put it in `killed-rectangle' anyway.  This means that
you can use this command to copy text from a read-only buffer.
\(If the variable `kill-read-only-ok' is non-nil, then this won't
even beep.)"
  (interactive "r\nP")
  (condition-case nil
      (setq killed-rectangle (delete-extract-rectangle start end fill))
    ((buffer-read-only text-read-only)
     (setq deactivate-mark t)
     (setq killed-rectangle (extract-rectangle start end))
     (if kill-read-only-ok
         (progn (message "Read only text copied to `killed-rectangle'") nil)
       (barf-if-buffer-read-only)
       (signal 'text-read-only (list (current-buffer)))))))

;;;###autoload
(defun copy-rectangle-as-kill (start end)
  "Copy the region-rectangle and save it as the last killed one."
  (interactive "r")
  (setq killed-rectangle (extract-rectangle start end))
  (setq deactivate-mark t)
  (if (called-interactively-p 'interactive)
      (indicate-copied-region (length (car killed-rectangle)))))

;;;###autoload
(defun yank-rectangle ()
  "Yank the last killed rectangle with upper left corner at point."
  (interactive "*")
  (insert-rectangle killed-rectangle))

;;;###autoload
(defun insert-rectangle (rectangle)
  "Insert text of RECTANGLE with upper left corner at point.
RECTANGLE's first line is inserted at point, its second
line is inserted at a point vertically under point, etc.
RECTANGLE should be a list of strings.
After this command, the mark is at the upper left corner
and point is at the lower right corner."
  (let ((lines rectangle)
	(insertcolumn (current-column))
	(first t))
    (push-mark)
    (while lines
      (or first
	  (progn
	   (forward-line 1)
	   (or (bolp) (insert ?\n))
	   (move-to-column insertcolumn t)))
      (setq first nil)
      (insert-for-yank (car lines))
      (setq lines (cdr lines)))))

;;;###autoload
(defun open-rectangle (start end &optional fill)
  "Blank out the region-rectangle, shifting text right.

The text previously in the region is not overwritten by the blanks,
but instead winds up to the right of the rectangle.

When called from a program the rectangle's corners are START and END.
With a prefix (or a FILL) argument, fill with blanks even if there is
no text on the right side of the rectangle."
  (interactive "*r\nP")
  (apply-on-rectangle 'open-rectangle-line start end fill)
  (goto-char start))

(defun open-rectangle-line (startcol endcol fill)
  (when (= (move-to-column startcol (if fill t 'coerce)) startcol)
    (unless (and (not fill)
		 (= (point) (point-at-eol)))
      (indent-to endcol))))

(defun delete-whitespace-rectangle-line (startcol _endcol fill)
  (when (= (move-to-column startcol (if fill t 'coerce)) startcol)
    (unless (= (point) (point-at-eol))
      (delete-region (point) (progn (skip-syntax-forward " " (point-at-eol))
				    (point))))))

;;;###autoload
(defalias 'close-rectangle 'delete-whitespace-rectangle) ;; Old name

;;;###autoload
(defun delete-whitespace-rectangle (start end &optional fill)
  "Delete all whitespace following a specified column in each line.
The left edge of the rectangle specifies the position in each line
at which whitespace deletion should begin.  On each line in the
rectangle, all contiguous whitespace starting at that column is deleted.

When called from a program the rectangle's corners are START and END.
With a prefix (or a FILL) argument, also fill too short lines."
  (interactive "*r\nP")
  (apply-on-rectangle 'delete-whitespace-rectangle-line start end fill))

(defvar string-rectangle-history nil)
(defun string-rectangle-line (startcol endcol string delete)
  (move-to-column startcol t)
  (if delete
      (delete-rectangle-line startcol endcol nil))
  (insert string))

(defvar-local rectangle--string-preview-state nil)
(defvar-local rectangle--string-preview-window nil)

(defun rectangle--string-flush-preview ()
  (mapc #'delete-overlay (nthcdr 3 rectangle--string-preview-state))
  (setf (nthcdr 3 rectangle--string-preview-state) nil))

(defun rectangle--string-erase-preview ()
  (with-selected-window rectangle--string-preview-window
    (rectangle--string-flush-preview)))

(defun rectangle--space-to (col)
  (propertize " " 'display `(space :align-to ,col)))

(defface rectangle-preview '((t :inherit region))
  "The face to use for the `string-rectangle' preview."
  :version "25.1")

(defcustom rectangle-preview t
  "If non-nil, `string-rectangle' will show an on-the-fly preview."
  :version "25.1"
  :type 'boolean)

(defun rectangle--string-preview ()
  (when rectangle-preview
    (let ((str (minibuffer-contents)))
      (when str (setq str (propertize str 'face 'rectangle-preview)))
      (with-selected-window rectangle--string-preview-window
        (unless (or (null rectangle--string-preview-state)
                    (equal str (car rectangle--string-preview-state)))
          (rectangle--string-flush-preview)
          (apply-on-rectangle
           (lambda (startcol endcol)
             (let* ((sc (move-to-column startcol))
                    (start (if (<= sc startcol) (point)
                             (forward-char -1)
                             (setq sc (current-column))
                             (point)))
                    (ec (move-to-column endcol))
                    (end (point))
                    (ol (make-overlay start end)))
               (push ol (nthcdr 3 rectangle--string-preview-state))
               ;; FIXME: The extra spacing doesn't interact correctly with
               ;; the extra spacing added by the rectangular-region-highlight.
               (when (< sc startcol)
                 (overlay-put ol 'before-string (rectangle--space-to startcol)))
               (let ((as (when (< endcol ec)
                           ;; (rectangle--space-to ec)
                           (spaces-string (- ec endcol))
                           )))
                 (if (= start end)
                     (overlay-put ol 'after-string (if as (concat str as) str))
                   (overlay-put ol 'display str)
                   (if as (overlay-put ol 'after-string as))))))
           (nth 1 rectangle--string-preview-state)
           (nth 2 rectangle--string-preview-state)))))))

;; FIXME: Should this be turned into inhibit-region-highlight and made to apply
;; to non-rectangular regions as well?
(defvar rectangle--inhibit-region-highlight nil)

;;;###autoload
(defun string-rectangle (start end string)
  "Replace rectangle contents with STRING on each line.
The length of STRING need not be the same as the rectangle width.

When called interactively and option `rectangle-preview' is
non-nil, display the result as the user enters the string into
the minibuffer.

Called from a program, takes three args; START, END and STRING."
  (interactive
   (progn
     (make-local-variable 'rectangle--string-preview-state)
     (make-local-variable 'rectangle--inhibit-region-highlight)
     (let* ((buf (current-buffer))
            (win (if (eq (window-buffer) buf) (selected-window)))
            (start (region-beginning))
            (end (region-end))
            (rectangle--string-preview-state `(nil ,start ,end))
            ;; Rectangle-region-highlighting doesn't work well in the presence
            ;; of the preview overlays.  We could work harder to try and make
            ;; it work better, but it's easier to just disable it temporarily.
            (rectangle--inhibit-region-highlight t))
       (barf-if-buffer-read-only)
       (list start end
             (minibuffer-with-setup-hook
                 (lambda ()
                   (setq rectangle--string-preview-window win)
                   (add-hook 'minibuffer-exit-hook
                             #'rectangle--string-erase-preview nil t)
                   (add-hook 'post-command-hook
                             #'rectangle--string-preview nil t))
               (read-string (format "String rectangle (default %s): "
                                    (or (car string-rectangle-history) ""))
                            nil 'string-rectangle-history
                            (car string-rectangle-history)))))))
  ;; If we undo this change, we want to have the point back where we
  ;; are now, and not after the first line in the rectangle (which is
  ;; the first line to be changed by the following command).
  (unless (eq buffer-undo-list t)
    (push (point) buffer-undo-list))
  (goto-char
   (apply-on-rectangle 'string-rectangle-line start end string t)))

;;;###autoload
(defalias 'replace-rectangle 'string-rectangle)

;;;###autoload
(defun string-insert-rectangle (start end string)
  "Insert STRING on each line of region-rectangle, shifting text right.

When called from a program, the rectangle's corners are START and END.
The left edge of the rectangle specifies the column for insertion.
This command does not delete or overwrite any existing text."
  (interactive
   (progn (barf-if-buffer-read-only)
	  (list
	   (region-beginning)
	   (region-end)
	   (read-string (format "String insert rectangle (default %s): "
				(or (car string-rectangle-history) ""))
			nil 'string-rectangle-history
			(car string-rectangle-history)))))
  (apply-on-rectangle 'string-rectangle-line start end string nil))

;;;###autoload
(defun clear-rectangle (start end &optional fill)
  "Blank out the region-rectangle.
The text previously in the region is overwritten with blanks.

When called from a program the rectangle's corners are START and END.
With a prefix (or a FILL) argument, also fill with blanks the parts of the
rectangle which were empty."
  (interactive "*r\nP")
  (apply-on-rectangle 'clear-rectangle-line start end fill))

(defun clear-rectangle-line (startcol endcol fill)
  (let ((pt (point-at-eol)))
    (when (= (move-to-column startcol (if fill t 'coerce)) startcol)
      (if (and (not fill)
	       (<= (save-excursion (goto-char pt) (current-column)) endcol))
	  (delete-region (point) pt)
	;; else
	(setq pt (point))
	(move-to-column endcol t)
	(setq endcol (current-column))
	(delete-region pt (point))
	(indent-to endcol)))))

;; Line numbers for `rectangle-number-line-callback'.
(defvar rectangle-number-line-counter)

(defun rectangle-number-line-callback (start _end format-string)
  (move-to-column start t)
  (insert (format format-string rectangle-number-line-counter))
  (setq rectangle-number-line-counter
	(1+ rectangle-number-line-counter)))

(defun rectangle--default-line-number-format (start end start-at)
  (concat "%"
	  (int-to-string (length (int-to-string (+ (count-lines start end)
						   start-at))))
	  "d "))

;;;###autoload
(defun rectangle-number-lines (start end start-at &optional format)
  "Insert numbers in front of the region-rectangle.

START-AT, if non-nil, should be a number from which to begin
counting.  FORMAT, if non-nil, should be a format string to pass
to `format' along with the line count.  When called interactively
with a prefix argument, prompt for START-AT and FORMAT."
  (interactive
   (if current-prefix-arg
       (let* ((start (region-beginning))
	      (end   (region-end))
	      (start-at (read-number "Number to count from: " 1)))
	 (list start end start-at
	       (read-string "Format string: "
			    (rectangle--default-line-number-format
			     start end start-at))))
     (list (region-beginning) (region-end) 1 nil)))
  (unless format
    (setq format (rectangle--default-line-number-format start end start-at)))
  (let ((rectangle-number-line-counter start-at))
    (apply-on-rectangle 'rectangle-number-line-callback
			start end format)))

;;; New rectangle integration with kill-ring.

;; FIXME: known problems with the new rectangle support:
;; - lots of commands handle the region without paying attention to its
;;   rectangular shape.

(add-function :around redisplay-highlight-region-function
              #'rectangle--highlight-for-redisplay)
(add-function :around redisplay-unhighlight-region-function
              #'rectangle--unhighlight-for-redisplay)
(add-function :around region-extract-function
              #'rectangle--extract-region)
(add-function :around region-insert-function
              #'rectangle--insert-region)

(defvar rectangle-mark-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?\C-o] 'open-rectangle)
    (define-key map [?\C-t] 'string-rectangle)
    (define-key map [remap exchange-point-and-mark]
      'rectangle-exchange-point-and-mark)
    (dolist (cmd '(right-char left-char forward-char backward-char
                   next-line previous-line))
      (define-key map (vector 'remap cmd)
        (intern (format "rectangle-%s" cmd))))
    map)
  "Keymap used while marking a rectangular region.")

;;;###autoload
(define-minor-mode rectangle-mark-mode
  "Toggle the region as rectangular.
Activates the region if needed.  Only lasts until the region is deactivated."
  nil nil nil
  (rectangle--reset-crutches)
  (when rectangle-mark-mode
    (add-hook 'deactivate-mark-hook
              (lambda () (rectangle-mark-mode -1)))
    (unless (region-active-p)
      (push-mark (point) t t)
      (message "Mark set (rectangle mode)"))))

(defun rectangle-exchange-point-and-mark (&optional arg)
  "Like `exchange-point-and-mark' but cycles through the rectangle's corners."
  (interactive "P")
  (if arg
      (progn
        (setq this-command 'exchange-point-and-mark)
        (exchange-point-and-mark arg))
    (let* ((p (point))
           (repeat (eq this-command last-command))
	   (m (mark))
           (p<m (< p m))
           (cols (if p<m (rectangle--pos-cols p m) (rectangle--pos-cols m p)))
           (cp (if p<m (car cols) (cdr cols)))
           (cm (if p<m (cdr cols) (car cols))))
      (if repeat (setq this-command 'exchange-point-and-mark))
      (rectangle--reset-crutches)
      (goto-char p)
      (rectangle--col-pos (if repeat cm cp) 'mark)
      (set-mark (point))
      (goto-char m)
      (rectangle--col-pos (if repeat cp cm) 'point))))

(defun rectangle--*-char (cmd n &optional other-cmd)
  ;; Part of the complexity here is that I'm trying to avoid making assumptions
  ;; about the L2R/R2L direction of text around point, but this is largely
  ;; useless since the rectangles implemented in this file are "logical
  ;; rectangles" and not "visual rectangles", so in the presence of
  ;; bidirectional text things won't work well anyway.
  (if (< n 0) (rectangle--*-char other-cmd (- n))
    (let ((col (rectangle--point-col (point)))
          (step 1))
      (while (> n 0)
        (let* ((bol (line-beginning-position))
               (eol (line-end-position))
               (curcol (current-column))
               (nextcol
                (condition-case nil
                    (save-excursion
                      (funcall cmd step)
                      (cond
                       ((> bol (point)) (- curcol 1))
                       ((< eol (point)) (+ col (1+ n)))
                       (t (current-column))))
                  (end-of-buffer (+ col (1+ n)))
                  (beginning-of-buffer (- curcol 1))))
               (diff (abs (- nextcol col))))
          (cond
           ((and (< nextcol curcol) (< curcol col))
            (let ((curdiff (- col curcol)))
              (if (<= curdiff n)
                (progn (cl-decf n curdiff) (setq col curcol))
                (setq col (- col n) n 0))))
           ((< nextcol 0) (ding) (setq n 0 col 0)) ;Bumping into BOL!
           ((= nextcol curcol) (funcall cmd 1))
           (t ;; (> nextcol curcol)
            (if (<= diff n)
                (progn (cl-decf n diff) (setq col nextcol))
              (setq col (if (< col nextcol) (+ col n) (- col n)) n 0))))
          (setq step (1+ step))))
      ;; FIXME: This rectangle--col-pos's move-to-column is wasted!
      (rectangle--col-pos col 'point))))

(defun rectangle-right-char (&optional n)
  "Like `right-char' but steps into wide chars and moves past EOL."
  (interactive "p") (rectangle--*-char #'right-char n #'left-char))
(defun rectangle-left-char (&optional n)
  "Like `left-char' but steps into wide chars and moves past EOL."
  (interactive "p") (rectangle--*-char #'left-char n #'right-char))

(defun rectangle-forward-char (&optional n)
  "Like `forward-char' but steps into wide chars and moves past EOL."
  (interactive "p") (rectangle--*-char #'forward-char n #'backward-char))
(defun rectangle-backward-char (&optional n)
  "Like `backward-char' but steps into wide chars and moves past EOL."
  (interactive "p") (rectangle--*-char #'backward-char n #'forward-char))

(defun rectangle-next-line (&optional n)
  "Like `next-line' but steps into wide chars and moves past EOL.
Ignores `line-move-visual'."
  (interactive "p")
  (let ((col (rectangle--point-col (point))))
    (forward-line n)
    (rectangle--col-pos col 'point)))
(defun rectangle-previous-line (&optional n)
  "Like `previous-line' but steps into wide chars and moves past EOL.
Ignores `line-move-visual'."
  (interactive "p")
  (let ((col (rectangle--point-col (point))))
    (forward-line (- n))
    (rectangle--col-pos col 'point)))


(defun rectangle--extract-region (orig &optional delete)
  (cond
   ((not rectangle-mark-mode)
    (funcall orig delete))
   ((eq delete 'bounds)
    (extract-rectangle-bounds (region-beginning) (region-end)))
   (t
    (let* ((strs (funcall (if delete
                              #'delete-extract-rectangle
                            #'extract-rectangle)
                          (region-beginning) (region-end)))
           (str (mapconcat #'identity strs "\n")))
      (when (eq last-command 'kill-region)
        ;; Try to prevent kill-region from appending this to some
        ;; earlier element.
        (setq last-command 'kill-region-dont-append))
      (when strs
        (put-text-property 0 (length str) 'yank-handler
                           `(rectangle--insert-for-yank ,strs t)
                           str)
        str)))))

(defun rectangle--insert-region (orig strings)
  (cond
   ((not rectangle-mark-mode)
    (funcall orig strings))
   (t
    (funcall #'insert-rectangle strings))))

(defun rectangle--insert-for-yank (strs)
  (push (point) buffer-undo-list)
  (let ((undo-at-start buffer-undo-list))
    (insert-rectangle strs)
    (setq yank-undo-function
          (lambda (_start _end)
            (undo-start)
            (setcar undo-at-start nil)  ;Turn it into a boundary.
            (while (not (eq pending-undo-list (cdr undo-at-start)))
              (undo-more 1))))))

(defun rectangle--place-cursor (leftcol left str)
  (let ((pc (window-parameter nil 'rectangle--point-crutches)))
    (if (and (eq left (car pc)) (eq leftcol (cdr pc)))
        (put-text-property 0 1 'cursor 1 str))))

(defun rectangle--highlight-for-redisplay (orig start end window rol)
  (cond
   ((not rectangle-mark-mode)
    (funcall orig start end window rol))
   (rectangle--inhibit-region-highlight
    (funcall redisplay-unhighlight-region-function rol)
    nil)
   ((and (eq 'rectangle (car-safe rol))
         (eq (nth 1 rol) (buffer-chars-modified-tick))
         (eq start (nth 2 rol))
         (eq end (nth 3 rol))
         (equal (rectangle--crutches) (nth 4 rol)))
    rol)
   (t
    (save-excursion
      (let* ((nrol nil)
             (old (if (eq 'rectangle (car-safe rol))
                      (nthcdr 5 rol)
                    (funcall redisplay-unhighlight-region-function rol)
                    nil)))
        (cl-assert (eq (window-buffer window) (current-buffer)))
        ;; `rectangle--pos-cols' looks up the `selected-window's parameter!
        (with-selected-window window
          (apply-on-rectangle
           (lambda (leftcol rightcol)
             (let* ((mleft (move-to-column leftcol))
                    (left (point))
                    ;; BEWARE: In the presence of other overlays with
                    ;; before/after/display-strings, this happens to move to
                    ;; the column "as if the overlays were not applied", which
                    ;; is sometimes what we want, tho it can be
                    ;; considered a bug in move-to-column (it should arguably
                    ;; pay attention to the before/after-string/display
                    ;; properties when computing the column).
                    (mright (move-to-column rightcol))
                    (right (point))
                    (ol
                     (if (not old)
                         (let ((ol (make-overlay left right)))
                           (overlay-put ol 'window window)
                           (overlay-put ol 'face 'region)
                           ol)
                       (let ((ol (pop old)))
                         (move-overlay ol left right (current-buffer))
                         ol))))
               ;; `move-to-column' may stop before the column (if bumping into
               ;; EOL) or overshoot it a little, when column is in the middle
               ;; of a char.
               (cond
                ((< mleft leftcol)      ;`leftcol' is past EOL.
                 (overlay-put ol 'before-string (rectangle--space-to leftcol))
                 (setq mright (max mright leftcol)))
                ((and (> mleft leftcol) ;`leftcol' is in the middle of a char.
                      (eq (char-before left) ?\t))
                 (setq left (1- left))
                 (move-overlay ol left right)
                 (goto-char left)
                 (overlay-put ol 'before-string (rectangle--space-to leftcol)))
                ((overlay-get ol 'before-string)
                 (overlay-put ol 'before-string nil)))
               (cond
                ;; While doing rectangle--string-preview, the two sets of
                ;; overlays steps on the other's toes.  I fixed some of the
                ;; problems, but others remain.  The main one is the two
                ;; (rectangle--space-to rightcol) below which try to virtually
                ;; insert missing text, but during "preview", the text is not
                ;; missing (it's provided by preview's own overlay).
                (rectangle--string-preview-state
                 (if (overlay-get ol 'after-string)
                     (overlay-put ol 'after-string nil)))
                ((< mright rightcol)    ;`rightcol' is past EOL.
                 (let ((str (rectangle--space-to rightcol)))
                   (put-text-property 0 (length str) 'face 'region str)
                   ;; If cursor happens to be here, draw it at the right place.
                   (rectangle--place-cursor leftcol left str)
                   (overlay-put ol 'after-string str)))
                ((and (> mright rightcol) ;`rightcol's in the middle of a char.
                      (eq (char-before right) ?\t))
                 (setq right (1- right))
                 (move-overlay ol left right)
                 (if (= rightcol leftcol)
                     (overlay-put ol 'after-string nil)
                   (goto-char right)
                   (let ((str (rectangle--space-to rightcol)))
                     (put-text-property 0 (length str) 'face 'region str)
                     (when (= left right)
                       (rectangle--place-cursor leftcol left str))
                     (overlay-put ol 'after-string str))))
                ((overlay-get ol 'after-string)
                 (overlay-put ol 'after-string nil)))
               (when (and (= leftcol rightcol) (display-graphic-p))
                 ;; Make zero-width rectangles visible!
                 (overlay-put ol 'after-string
                              (concat (propertize " "
                                                  'face '(region (:height 0.2)))
                                      (overlay-get ol 'after-string))))
               (push ol nrol)))
           start end))
        (mapc #'delete-overlay old)
        `(rectangle ,(buffer-chars-modified-tick)
                    ,start ,end ,(rectangle--crutches)
                    ,@nrol))))))

(defun rectangle--unhighlight-for-redisplay (orig rol)
  (if (not (eq 'rectangle (car-safe rol)))
      (funcall orig rol)
    (mapc #'delete-overlay (nthcdr 5 rol))
    (setcar (cdr rol) nil)))

(provide 'rect)

;;; rect.el ends here
