;;; rmail-header.el --- Header handling code of "RMAIL" mail reader for Emacs

;; Copyright (C) 2002, 2006 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: mail

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

;; Written by Paul Reilly as part of moving BABYL to inbox/mbox format.

(eval-when-compile
  (require 'mail-utils))

(defconst rmail-header-attribute-header "X-BABYL-V6-ATTRIBUTES"
  "The header that persists the Rmail attribute data.")

(defconst rmail-header-keyword-header "X-BABYL-V6-KEYWORDS"
  "The header that persists the Rmail keyword data.")

(defvar rmail-header-overlay-list nil
  "A list of cached overlays used to make headers hidden or visible.")

(defvar rmail-header-display-mode nil
  "Records the current header display mode.
nil means headers are displayed, t indicates headers are not displayed.")

(defun rmail-header-get-limit ()
  "Return the end of the headers."
  (goto-char (point-min))
  (if (search-forward "\n\n" nil t)
      (1- (point))
    (error "Invalid message format.")))

;;; The following functions are presented alphabetically ordered by
;;; name.

(defun rmail-header-add-header (header value)
  "Add HEADER to the list of headers and associate VALUE with it.
The current buffer, possibly narrowed, contains a single message.
If VALUE is nil or the empty string, the header is removed
instead."
  (save-excursion
    (let* ((inhibit-read-only t)
	   (case-fold-search t)
	   (limit (rmail-header-get-limit))
	   start end)
      ;; Search for the given header.  If found, then set it's value.
      ;; If not then add the header to the end of the header section.
      (goto-char (point-min))
      (if (re-search-forward (format "^%s: " header) limit t)
	  (let ((start (match-beginning 0)))
	    (re-search-forward "\n[^ \t]")
	    (goto-char limit)
	    (kill-region start (1+ (match-beginning 0))))
	(goto-char limit))
      (when (> (length value) 0)
	(insert header ": " value "\n")))))

(defun rmail-header-contains-keyword-p (keyword)
  "Return t if KEYWORD exists in the current buffer, nil otherwise."
  (let ((limit (rmail-header-get-limit)))
    (goto-char (point-min))
    (if (re-search-forward (format "^%s: " rmail-header-keyword-header) limit t)
        ;; Some keywords exist.  Now search for the specific keyword.
        (let ((start (point))
              (end (progn (end-of-line) (point))))
          (if (re-search-forward (concat "\\(" keyword ",\\|" keyword "$\\)"))
              t)))))

(defun rmail-header-get-header (header)
  "Return the text value for HEADER, nil if no such header exists.
The current buffer, possibly narrowed, contains a single message."
  (save-excursion
    (save-restriction
      (let ((limit (rmail-header-get-limit)))
	(narrow-to-region (point-min) limit)
	(mail-fetch-field header)))))

(defun rmail-header-get-keywords ()
  "Return the keywords in the current message.
The current buffer, possibly narrowed, contains a single message."
  ;; Search for a keyword header and return the comma separated
  ;; strings as a list.
  (let ((limit (rmail-header-get-limit)) result)
    (goto-char (point-min))
    (if (re-search-forward
         (format "^%s: " rmail-header-keyword-header) limit t)
        (save-excursion
          (save-restriction
            (narrow-to-region (point) (line-end-position))
            (goto-char (point-min))
            (mail-parse-comma-list))))))

(defun rmail-header-hide-headers ()
  "Hide ignored headers.  All others will be visible.
The current buffer, possibly narrowed, contains a single message."
  (save-excursion
    (let ((case-fold-search t)
	  (limit (rmail-header-get-limit))
	  (inhibit-point-motion-hooks t)
	  ;; start end
	  visibility-p
	  ;;overlay
	  overlay-list)
      ;; Record the display state as having headers hidden.
      (setq rmail-header-display-mode t)
      ;; Clear the pool of overlays for reuse.
      (mapcar 'delete-overlay rmail-header-overlay-list)
      (setq overlay-list rmail-header-overlay-list)
      ;; Determine whether to use the displayed headers or the ignored
      ;; headers.
      (if rmail-displayed-headers
	  ;; Set the visibility predicate function to ignore headers
	  ;; marked for display.
	  (setq visibility-p 'rmail-header-show-displayed-p)
	;; Set the visibility predicate function to hide ignored
	;; headers.
	(setq visibility-p 'rmail-header-hide-ignored-p))
      ;; Walk through all the headers marking the non-displayed
      ;; headers as invisible.
      (goto-char (point-min))
      (while (re-search-forward "^[^ \t:]+[ :]" limit t)
	;; Determine if the current header needs to be hidden.
	(beginning-of-line)
	(if (not (funcall visibility-p))
	    ;; It does not.  Move point away from this header.
	    (forward-line 1)
	  ;; It does.  Make this header hidden by setting an overlay
	  ;; with both the invisible and intangible properties set.
	  (let ((start (point))
		overlay)
	    ;; Move to end and pick upp any continuation lines on folded
	    ;; headers.
	    (forward-line 1)
	    (while (looking-at "[ \t]+")
	      (forward-line 1))
	    ;; (setq end (point))
	    ;; Use one of the cleared, cached overlays until they
	    ;; run out.
	    (if (car overlay-list)
		;; Use a cached overlay.
		(progn
		  (setq overlay (car overlay-list)
			overlay-list (cdr overlay-list))
		  (move-overlay overlay start (point)))
	      ;; No overlay exists for this header.  Create one and
	      ;; add it to the cache.
	      (setq overlay (make-overlay start (point)))
	      (overlay-put overlay 'invisible t)
	      (overlay-put overlay 'intangible t)
	      (push overlay rmail-header-overlay-list))))))))

(defun rmail-header-persist-attributes (attributes)
  "Save ATTRIBUTES in the Rmail BABYL header."
  (rmail-header-set-header rmail-header-attribute-header attributes))

(defun rmail-header-set-header (header value)
  "Set the current value of HEADER to VALUE.
The current buffer, possibly narrowed, contains a single message."
  (save-excursion

    ;; Enable the buffer to be written, search for the header case
    ;; insensitively, ignore intangibility and do not record these
    ;; changes in the undo list.
    (let ((inhibit-read-only t)
	  (case-fold-search t)
	  (inhibit-point-motion-hooks t)
	  (buffer-undo-list t)
	  (limit (rmail-header-get-limit))
	  start end)

      ;; Search for the given header.  If found, then set it's value.
      ;; If not generate an error.
      (goto-char (point-min))
      (if (re-search-forward (format "^%s: " header) limit t)

	  ;; Kill the current value and replace it with the new.
	  (progn
	    (setq start (point))
	    (while (progn
		     (forward-line 1)
		     (looking-at "[ \t]+")))
	    (setq end (point-marker))
	    (goto-char start)
	    (insert-and-inherit value)
	    (kill-region (point) (1- (marker-position end))))
	;; Generate an error since the header does not exist.
	(error "Header %s not found." header)))))

(defun rmail-header-show-headers ()
  "Show all headers.
The current buffer, possibly narrowed, contains a single message."
  ;; Remove all the overlays used to control hiding headers.
  (mapcar 'delete-overlay rmail-header-overlay-list)
  (setq rmail-header-display-mode nil))

(defun rmail-header-toggle-visibility (&optional arg)
  "Toggle the visibility of the ignored headers if ARG is nil.
Hide the ignored headers if ARG is greater than 0, otherwise show the
ignored headers.  The current buffer, possibly narrowed, contains a
single message."
  (cond ((eq arg nil)
	 (if rmail-header-display-mode
	     (rmail-header-show-headers)
	   (rmail-header-hide-headers)))
	((or (eq arg t) (> arg 0))
	 (rmail-header-hide-headers))
	(t (rmail-header-show-headers))))

(defun rmail-header-hide-ignored-p ()
  "Test that the header is one of the headers marked to be ignored."
  (looking-at rmail-ignored-headers))

(defun rmail-header-show-displayed-p ()
  "Test that the header is not one of the headers marked for display."
  (not (looking-at rmail-displayed-headers)))

(provide 'rmailhdr)

;;; rmailhdr.el ends here
