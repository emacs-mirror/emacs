;;; docref.el -- Simple cross references for Elisp documentation strings
;; Copyright (C) 1994 Free Software Foundation, Inc.

;; Author: Vadim Geshel <vadik@unas.cs.kiev.ua>
;; Created: 12 Jul 1994
;; Keywords: docs, help, lisp
;; original name was cross-ref.el.

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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;;
;; This package allows you to use a simple form of cross references in
;; your Emacs Lisp documentation strings. Cross-references look like
;; \\(type@[label@]data), where type defines a method for retrieving
;; reference informatin, data is used by a method routine as an argument,
;; and label "represents" the reference in text. If label is absent, data
;; is used instead.
;; 
;; Special reference labeled `back', when present, can be used to return
;; to the previous contents of help buffer.
;;
;; Cross-referencing currently is intended for use in doc strings only
;; and works only in temporary buffers (created by `with-output-to-temp-buffer').
;; List of temp buffers in which cross-referencing is to be active is specified
;; by variable DOCREF-BUFFERS-LIST, which contains only "*Help*" by default.
;;
;; Documentation strings for this package's functions and variables can serve
;; as examples of usage.
;;
;;; Customization:
;; 
;; See source. The main customization variable is `docref-methods-alist'.
;; It consists of (type . function) pairs, where type is a string which
;; corresponds to type in cross-references and function is called with
;; one argument - reference `data' - when a reference is activated.
;;
;;; Installation:
;;
;; Place this file somewhere in your load-path, byte-compiled it, and add
;; (require 'cross-ref)
;; to your .emacs.

;;; Code:

;; User customizable variables

(defvar docref-highlight-p t
  "*If non-nil, \\(f@docref-subst) highlights cross-references.
Under window system it highlights them with face defined by
\\(v@docref-highlight-face), on character terminal highlighted references
look like cross-references in info mode.")

(defvar docref-highlight-face 'highlight
  "*Face used to highlight cross-references (used by \\(f@docref-subst))")

(defvar docref-methods-alist
  '(("f" . docref-describe-function)	; reference to a function documentation
    ("v" . docref-describe-variable)	; reference to a variable documentation
    ("F" . docref-read-file)		; reference to a file contents
    ("s" . docref-use-string)		; reference to a string 
    ("V" . docref-use-variable-value)	; reference to variable value
    ("0" . beep))			; just highlighted text
  "Alist which maps cross-reference ``types'' to retrieval functions.

The car of each element is a string that serves as `type' in cross-references.
\(See \\(f@docref-subst)).  The cdr is a function of one argument,
to be called to find this reference.")

(defvar docref-back-label "\nback"
  "Label to use by \\(f@docref-subst) for the go-back reference.")

(defvar docref-back-reference nil
  "If non-nil, this is a go-back reference to add to the current buffer.
The value specifies how to go back.  It should be suitable for use
as the second argument to \\(f@docref-insert-label).
\\(f@docref-subst) uses this to set up the go-back reference.")

(defvar docref-last-active-buffer)

;;;###autoload
(defun docref-setup ()
  "Process docref cross-references in the current buffer.
See also \\(f@docref-subst)."
  (interactive)
  (docref-subst (current-buffer))
  (docref-mode))

(defvar docref-mode-map nil)
(or docref-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map [mouse-2] 'docref-follow-mouse)
      (define-key map "\C-c\C-b" 'docref-go-back)
      (define-key map "\C-c\C-c" 'docref-follow)
      (setq docref-mode-map map)))

(defun docref-mode ()
  "Major mode for help buffers that contain cross references.
To follow a reference, move to it and type \\[docref-follow], or use
\\[docref-follow-mouse].  The command \\[docref-go-back] can used to go
back to where you came from."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'docref-mode)
  (setq mode-name "Docref")
  (use-local-map docref-mode-map)
  (run-hooks 'docref-mode))

(defun docref-subst (buf)
  "Parse documentation cross-references in buffer BUF.

Find cross-reference information in a buffer and
highlight them with face defined by \\(v@docref-highlight-face).

Cross-reference has the following format: \\ (TYPE[@LABEL]@DATA), where
TYPE defines method used to retrive xref data (like reading from file or
calling \\(f@describe-function)), DATA is an argument to this method
\(like file name or function name), and LABEL is displayed in text using
\\(v@docref-highlight-face).

The special reference `back' can be used to return back.
The variable \\(v@docref-back-label) specifies the label to use for that.

See \\(v@docref-methods-alist) for currently defined methods."
  (interactive "b")
  (save-excursion
    (set-buffer buf)
    (goto-char (point-min))
    ;; The docref-seen property indicates that we have processed this
    ;; buffer's contents already, so don't do it again.
    (if (not (get-text-property (point-min) 'docref-seen))
	(let ((old-modified (buffer-modified-p)))
	  (while (re-search-forward "[\\](\\([^\)\@]+\\)\\(@[^\)\@]+\\)?@\\([^\)]*\\))"
				    nil t)
	    (let* ((start (match-beginning 0))
		   (type (buffer-substring (match-beginning 1) (match-end 1)))
		   (data (buffer-substring (match-beginning 3) (match-end 3)))
		   (label
		    (if (match-beginning 2)
			(buffer-substring (+ (match-beginning 2) 1) (match-end 2))
		      data)))
	      (replace-match "" t)
	      (docref-insert-label label (cons type data))))

	  ;; Make a back-reference in this buffer, if desired.
	  ;; (This is true if called from docref-follow.)
	  (if docref-back-reference
	      (progn
		(goto-char (point-max))
		(put-text-property (point-min) (1+ (point-min))
				   'docref-back-position (point))
		(docref-insert-label docref-back-label docref-back-reference)))
	  (put-text-property (point-min) (1+ (point-min)) 'docref-seen t)
	  (set-buffer-modified-p old-modified)))))

(defun docref-insert-label (string ref)
  (let ((label (concat string))
	(pos (point)))
    ;; decorate the label
    (let ((leading-space-end (save-match-data
			       (if (string-match "^\\([ \t\n]+\\)" label)
				   (match-end 1)
				 0)))
	  (trailing-space-start (save-match-data
				  (if (string-match "\\([ \t\n]+\\)$" label)
				      (match-beginning 1)
				    (length label)))))
      (if docref-highlight-p	      
	  (if (not window-system)
	      (setq label
		    (concat (substring label 0 leading-space-end)
			    "(*note "
			    (substring label leading-space-end trailing-space-start)
			    ")"
			    (substring label trailing-space-start)))
	    ;; window-system
	    (put-text-property leading-space-end
			       trailing-space-start
			       'face docref-highlight-face label)))
      (put-text-property 0 (length label) 'docref ref label)
      (insert label))))

(defun docref-follow-mouse (click)
  "Follow the cross-reference that you click on."
  (interactive "e")
  (save-excursion
    (let* ((start (event-start click))
	   (window (car start))
	   (pos (car (cdr start)))
	   (docref-last-active-buffer (current-buffer)))
      (set-buffer (window-buffer window))
      (docref-follow pos))))

(defun docref-go-back ()
  "Go back to the previous contents of help buffer."
  (interactive)
  (let ((pos (get-text-property (point-min) 'docref-back-position)))
    (if	pos
	(docref-follow pos)
      (error "No go-back reference"))))

(defun docref-follow (&optional pos)
  "Follow cross-reference at point.
For the cross-reference format, see \\(f@docref-subst).
The special reference named `back' can be used to return back"
  (interactive)
  (or pos (setq pos (point)))
  (let ((docref-data (get-text-property pos 'docref)))
    (if docref-data
	;; There is a reference at point.  Follow it.
	(let* ((type (car docref-data))
	       (name (cdr docref-data))
	       (method (assoc type docref-methods-alist))
	       (cur-contents (buffer-string))
	       (opoint (point))
	       (docref-back-reference (cons "s" cur-contents))
	       success)
	  (if (null method)
	      (error "Unknown cross-reference type: %s" type))
	  (unwind-protect
	      (save-excursion
		(funcall (cdr method) name)
		(setq success t))
	    (or success
		(progn
		  ;; (cdr method) got an error.
		  ;; Put back the text that we had.
		  (erase-buffer)
		  (insert cur-contents)
		  (goto-char opoint)))
	    (set-buffer-modified-p nil))))))

;; Builtin methods for accessing a reference.

(defun docref-describe-function (data)
  (save-excursion
    (if (boundp 'docref-last-active-buffer)
	(set-buffer docref-last-active-buffer))
    (describe-function (intern data))))
  
(defun docref-describe-variable (data)
  (save-excursion
    (if (boundp 'docref-last-active-buffer)
	(set-buffer docref-last-active-buffer))
    (describe-variable (intern data))))

(defun docref-read-file (data)
  (with-output-to-temp-buffer (buffer-name)
    (erase-buffer)
    (insert-file-contents (expand-file-name data))))

(defun docref-use-string (data)
  (with-output-to-temp-buffer (buffer-name)
    (erase-buffer)
    (insert data)))

(defun docref-use-variable-value (data)
  (let ((sym (intern data)))
    (with-output-to-temp-buffer (buffer-name)
      (erase-buffer)
      (princ (symbol-value sym)))))

(provide 'docref)

;;; docref.el ends here

