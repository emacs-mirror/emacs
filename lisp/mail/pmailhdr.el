;;; pmail-header.el --- Header handling code of "PMAIL" mail reader for Emacs

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

(eval-when-compile
  (require 'mail-utils))

(defconst pmail-header-attribute-header "X-BABYL-V6-ATTRIBUTES"
  "The header that stores the Pmail attribute data.")

(defconst pmail-header-keyword-header "X-BABYL-V6-KEYWORDS"
  "The header that stores the Pmail keyword data.")

(defvar pmail-header-overlay-list nil
  "List of cached overlays used to make headers hidden or visible.")

(defvar pmail-header-display-state nil
  "Records the current header display state.
nil means headers are displayed, t indicates headers are not displayed.")

(defun pmail-header-get-limit ()
  "Return the end of the headers.
The current buffer must show one message.  If you want to narrow
to the headers of a mail by number, use `pmail-narrow-to-header'
instead."
  (save-excursion
    (goto-char (point-min))
    (if (search-forward "\n\n" nil t)
	(1- (point))
      (error "Invalid message format"))))

(defun pmail-header-add-header (header value)
  "Add HEADER to the list of headers and associate VALUE with it.
The current buffer, possibly narrowed, contains a single message.
If VALUE is nil or the empty string, the header is removed
instead."
  (save-excursion
    (let* ((inhibit-read-only t)
	   (case-fold-search t)
	   (inhibit-point-motion-hooks t)
	   (buffer-undo-list t)
	   (limit (pmail-header-get-limit))
	   start end)
      ;; Search for the given header.  If found, then set it's value.
      ;; If not then add the header to the end of the header section.
      (goto-char (point-min))
      (if (re-search-forward (format "^%s: " header) limit t)
	  (let ((start (match-beginning 0)))
	    (re-search-forward "\n[^ \t]")
	    (goto-char limit)
	    (delete-region start (1+ (match-beginning 0))))
	(goto-char limit))
      (when (> (length value) 0)
	(insert header ": " value "\n")))))

(defun pmail-header-contains-keyword-p (keyword)
  "Return t if KEYWORD exists in the current buffer, nil otherwise."
  (let ((limit (pmail-header-get-limit)))
    (goto-char (point-min))
    (if (re-search-forward (format "^%s: " pmail-header-keyword-header) limit t)
        ;; Some keywords exist.  Now search for the specific keyword.
        (let ((start (point))
              (end (progn (end-of-line) (point))))
          (if (re-search-forward (concat "\\(" keyword ",\\|" keyword "$\\)"))
              t)))))

(defun pmail-header-get-header (&rest args)
  "Return the text value for a header or nil if no such header exists.
The arguments ARGS are passed to `mail-fetch-field'.  The first
argument is the header to get.

The current buffer, possibly narrowed, contains a single message.
Note that it is not necessary to call `pmail-header-show-headers'
because `inhibit-point-motion-hooks' is locally bound to t."
  (save-excursion
    (save-restriction
      (let* ((inhibit-point-motion-hooks t)
	     (limit (pmail-header-get-limit)))
	(narrow-to-region (point-min) limit)
	(apply 'mail-fetch-field args)))))

(defun pmail-header-get-keywords ()
  "Return the keywords in the current message.
The current buffer, possibly narrowed, contains a single message."
  ;; Search for a keyword header and return the comma separated
  ;; strings as a list.
  (let ((limit (pmail-header-get-limit)) result)
    (goto-char (point-min))
    (if (re-search-forward
         (format "^%s: " pmail-header-keyword-header) limit t)
        (save-excursion
          (save-restriction
            (narrow-to-region (point) (line-end-position))
            (goto-char (point-min))
            (mail-parse-comma-list))))))

(defun pmail-header-hide-headers ()
  "Hide ignored headers.  All others will be visible.
The current buffer, possibly narrowed, contains a single message."
  (save-excursion
    (pmail-header-show-headers)
    (let ((overlay-list pmail-header-overlay-list)
	  (limit (pmail-header-get-limit))
	  (inhibit-point-motion-hooks t)
	  (case-fold-search t)
	  visibility-p)
      ;; Record the display state as having headers hidden.
      (setq pmail-header-display-state t)
      (if pmail-displayed-headers
	  ;; Set the visibility predicate function to ignore headers
	  ;; marked for display.
	  (setq visibility-p 'pmail-header-show-displayed-p)
	;; Set the visibility predicate function to hide ignored
	;; headers.
	(setq visibility-p 'pmail-header-hide-ignored-p))
      ;; Walk through all the headers marking the non-displayed
      ;; headers as invisible.
      (goto-char (point-min))
      (while (re-search-forward "^[^ \t:]+[ :]" limit t)
	;; Determine if the current header needs to be hidden.
	(forward-line 0)
	(if (not (funcall visibility-p))
	    ;; It does not.  Move point away from this header.
	    (progn
	      (forward-line 1)
	      (while (looking-at "[ \t]+")
		(forward-line 1)))
	  ;; It does.  Make this header hidden by setting an overlay
	  ;; with both the invisible and intangible properties set.
	  (let ((start (point)))
	    ;; Move to end and pick upp any continuation lines on folded
	    ;; headers.
	    (forward-line 1)
	    (while (looking-at "[ \t]+")
	      (forward-line 1))
	    (if (car overlay-list)
		;; Use one of the cleared, cached overlays.
		(let ((overlay (car overlay-list)))
		  (move-overlay overlay start (point))
		  (setq overlay-list (cdr overlay-list)))
	      ;; No overlay exists for this header.  Create one and
	      ;; add it to the cache.
	      (let ((overlay (make-overlay start (point))))
		(overlay-put overlay 'invisible t)
		(overlay-put overlay 'intangible t)
		(push overlay pmail-header-overlay-list)))))))))

(defun pmail-header-show-headers ()
  "Show all headers.
The current buffer, possibly narrowed, contains a single message."
  ;; Remove all the overlays used to control hiding headers.
  (mapcar 'delete-overlay pmail-header-overlay-list)
  (setq pmail-header-display-state nil))

(defun pmail-header-toggle-visibility (&optional arg)
  "Toggle the visibility of the ignored headers if ARG is nil.
Hide the ignored headers if ARG is greater than 0, otherwise show the
ignored headers.  The current buffer, possibly narrowed, contains a
single message."
  (cond ((eq arg nil)
	 (if pmail-header-display-state
	     (pmail-header-show-headers)
	   (pmail-header-hide-headers)))
	((or (eq arg t) (> arg 0))
	 (pmail-header-hide-headers))
	(t (pmail-header-show-headers))))

(defun pmail-header-hide-ignored-p ()
  "Test that the header is one of the headers marked to be ignored."
  (looking-at pmail-ignored-headers))

(defun pmail-header-show-displayed-p ()
  "Test that the header is not one of the headers marked for display."
  (not (looking-at pmail-displayed-headers)))

(provide 'pmailhdr)

;;; pmailhdr.el ends here
