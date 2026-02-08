;;; pixel-fill.el --- variable pitch filling functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026 Free Software Foundation, Inc.

;; Maintainer: emacs-devel@gnu.org
;; Keywords: filling

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

;; The main entry point is `pixel-fill-region', but
;; `pixel-fill-find-fill-point' can also be useful by itself.

;;; Code:

(require 'kinsoku)

(defgroup pixel-fill nil
  "Filling based on pixel widths."
  :group 'fill
  :version "29.1")

(defcustom pixel-fill-respect-kinsoku t
  "If nil, fill even if we can't find a good kinsoku point.
Kinsoku is a Japanese word meaning a rule that should not be violated.
In Emacs, it is a term used for characters, e.g. punctuation marks,
parentheses, and so on, that should not be placed in the beginning
of a line or the end of a line."
  :type 'boolean
  :version "29.1")

(defun pixel-fill-width (&optional columns window)
  "Return the pixel width corresponding to COLUMNS in WINDOW.
If COLUMNS is nil or omitted, use the entire window width.

If WINDOW is nil or omitted, this defaults to the selected window."
  (unless window
    (setq window (selected-window)))
  (let ((frame (window-frame window)))
    (if columns
        (* (frame-char-width frame) columns)
      (- (window-body-width nil t)
         (* 2 (frame-char-width frame))
         ;; We need to adjust the available width for when the user
         ;; disables the fringes, which will cause the display
         ;; engine usurp one column for the continuation glyph.
         (if (and (fboundp 'fringe-columns)
                  (or (not (zerop (fringe-columns 'right)))
                      (not (zerop (fringe-columns 'left)))))
             0
           (* (frame-char-width frame) 2))
         1))))

(defun pixel-fill-region (start end pixel-width)
  "Fill the region between START and END.
This will attempt to reformat the text in the region to have no
lines that are visually wider than PIXEL-WIDTH.

If START isn't at the start of a line, the horizontal position of
START, converted to pixel units, will be used as the indentation
prefix on subsequent lines."
  (save-window-excursion
    (set-window-buffer nil (current-buffer))
    (save-excursion
      (goto-char start)
      (let ((indentation
             (car (window-text-pixel-size nil (line-beginning-position)
                                          (point))))
            (newline-end nil))
        (when (> indentation pixel-width)
          (error "The indentation (%s) is wider than the fill width (%s)"
                 indentation pixel-width))
        (save-restriction
          (narrow-to-region start end)
          (goto-char (point-max))
          (when (looking-back "\n[ \t]*" (point-min))
            (setq newline-end t))
          (goto-char (point-min))
          ;; First replace all whitespace with space.
          (while (re-search-forward "[ \t\n]+" nil t)
            (cond
             ((or (= (match-beginning 0) start)
                  (= (match-end 0) end))
              (delete-region (match-beginning 0) (match-end 0)))
             ;; If there's just a single space here, don't replace.
             ((not (and (= (- (match-end 0) (match-beginning 0)) 1)
                        (= (char-after (match-beginning 0)) ?\s)))
              (replace-match
               ;; We need to use a space that has an appropriate width.
               (propertize " " 'face
                           (get-text-property (match-beginning 0) 'face))))))
          (goto-char start)
          (pixel-fill--fill-line pixel-width indentation)
          (goto-char (point-max))
          (when newline-end
            (insert "\n")))))))

(defun pixel-fill--goto-pixel (width)
  (vertical-motion (cons (/ width (frame-char-width)) 0)))

(defun pixel-fill--fill-line (width &optional indentation)
  (let ((start (point)))
    (pixel-fill--goto-pixel width)
    (while (not (eolp))
      ;; We have to do some folding.  First find the first previous
      ;; point suitable for folding.
      (when (or (not (pixel-fill-find-fill-point (line-beginning-position)))
	        (= (point) start))
	;; We had unbreakable text (for this width), so just go to
	;; the first space and carry on.
	(beginning-of-line)
	(skip-chars-forward " ")
	(search-forward " " (line-end-position) 'move))
      (when (= (preceding-char) ?\s)
	(delete-char -1))
      (unless (eobp)
        (insert ?\n)
        (when (> indentation 0)
          (insert (propertize " " 'display
                              (list 'space :align-to (list indentation))))))
      (setq start (point))
      (unless (eobp)
        (pixel-fill--goto-pixel width)))))

(define-inline pixel-fill--char-breakable-p (char)
  "Return non-nil if a line can be broken before and after CHAR."
  (inline-quote (aref fill-find-break-point-function-table ,char)))

(define-inline pixel-fill--char-nospace-p (char)
  "Return non-nil if no space is required before and after CHAR."
  (inline-quote (aref fill-nospace-between-words-table ,char)))

(define-inline pixel-fill--char-kinsoku-bol-p (char)
  "Return non-nil if a line ought not to begin with CHAR."
  (inline-letevals (char)
    (inline-quote (and (not (eq ,char ?'))
                       (aref (char-category-set ,char) ?>)))))

(define-inline pixel-fill--char-kinsoku-eol-p (char)
  "Return non-nil if a line ought not to end with CHAR."
  (inline-quote (aref (char-category-set ,char) ?<)))

(defun pixel-fill-find-fill-point (start)
  "Find a place suitable for breaking the current line.
START should be the earliest buffer position that should be considered
(typically the start of the line), and this function will search
backward in the current buffer from the current position."
  (let ((bp (point))
	(end (point))
	failed)
    (while (not
            (or (setq failed (<= (point) start))
		(eq (preceding-char) ?\s)
		(eq (following-char) ?\s)
		(pixel-fill--char-breakable-p (preceding-char))
		(pixel-fill--char-breakable-p (following-char))
		(and (pixel-fill--char-kinsoku-bol-p (preceding-char))
		     (pixel-fill--char-breakable-p (following-char))
		     (not (pixel-fill--char-kinsoku-bol-p (following-char))))
		(pixel-fill--char-kinsoku-eol-p (following-char))
		(bolp)))
      (backward-char 1))
    (if failed
	;; There's no breakable point, so we give it up.
	(let (found)
	  (goto-char bp)
          ;; Don't overflow the window edge, even if
          ;; `pixel-fill-respect-kinsoku' is t.
	  (when pixel-fill-respect-kinsoku
	    (while (setq found (re-search-forward
				"\\(\\c>\\)\\| \\|\\c<\\|\\c|"
				(line-end-position) 'move)))
	    (if (and found
		     (not (match-beginning 1)))
		(goto-char (match-beginning 0)))))
      (or
       (eolp)
       ;; Don't put kinsoku-bol characters at the beginning of a line,
       ;; or kinsoku-eol characters at the end of a line.
       (cond
        ;; Don't overflow the window edge, even if `pixel-fill-respect-kinsoku'
        ;; is t.
	((not pixel-fill-respect-kinsoku)
	 (while (and (not (eq (preceding-char) ?\s))
		     (or (pixel-fill--char-kinsoku-eol-p (preceding-char))
                         (pixel-fill--char-kinsoku-bol-p (following-char))))
	   (backward-char 1))
	 (when (setq failed (<= (point) start))
	   ;; There's no breakable point that doesn't violate kinsoku,
	   ;; so we look for the second best position.
	   (while (and (progn
			 (forward-char 1)
			 (<= (point) end))
		       (progn
			 (setq bp (point))
			 (pixel-fill--char-kinsoku-eol-p (following-char)))))
	   (goto-char bp)))
	((pixel-fill--char-kinsoku-eol-p (preceding-char))
	 ;; Find backward the point where kinsoku-eol characters begin.
	 (let ((count 4))
	   (while
	       (progn
		 (backward-char 1)
		 (and (> (setq count (1- count)) 0)
		      (not (eq (preceding-char) ?\s))
		      (or (pixel-fill--char-kinsoku-eol-p (preceding-char))
			  (pixel-fill--char-kinsoku-bol-p (following-char)))))))
	 (when (setq failed (<= (point) start))
	   ;; There's no breakable point that doesn't violate kinsoku,
	   ;; so we go to the second best position.
	   (if (looking-at "\\(\\c<+\\)\\c<")
	       (goto-char (match-end 1))
	     (forward-char 1))))
	((pixel-fill--char-kinsoku-bol-p (following-char))
	 ;; Find forward the point where kinsoku-bol characters end.
	 (let ((count 4))
	   (while (progn
		    (forward-char 1)
		    (and (>= (setq count (1- count)) 0)
			 (pixel-fill--char-kinsoku-bol-p (following-char))
			 (pixel-fill--char-breakable-p (following-char))))))))
       (when (eq (following-char) ?\s)
	 (forward-char 1))))
    (not failed)))

(provide 'pixel-fill)

;;; pixel-fill.el ends here
