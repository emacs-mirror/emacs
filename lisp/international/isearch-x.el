;;; isearch-x.el --- extended isearch handling commands

;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.

;; Keywords: multilingual, isearch

;; Author: Kenichi HANDA <handa@etl.go.jp>
;; Maintainer: Kenichi HANDA <handa@etl.go.jp>

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

;;; Code:

;;;###autoload
(defun isearch-toggle-specified-input-method ()
  "Select an input method and turn it on in interactive search."
  (interactive)
  (let ((overriding-terminal-local-map nil))
    (toggle-input-method t))
  (setq isearch-input-method-function input-method-function
	isearch-input-method-local-p t)
  (setq input-method-function nil)
  (isearch-update))

;;;###autoload
(defun isearch-toggle-input-method ()
  "Toggle input method in interactive search."
  (interactive)
  (let ((overriding-terminal-local-map nil))
    (toggle-input-method))
  (setq isearch-input-method-function input-method-function
	isearch-input-method-local-p t)
  (setq input-method-function nil)
  (isearch-update))

(defvar isearch-minibuffer-local-map
  (let ((map (make-keymap)))
    (define-key map [t] 'isearch-minibuffer-non-self-insert)
    (let ((i ?\ ))
      (while (< i 256)
	(define-key map (vector i) 'isearch-minibuffer-self-insert)
	(setq i (1+ i))))
    (let ((l (generic-character-list))
	  (table (nth 1 map)))
      (while l
	(set-char-table-default table (car l) 'isearch-minibuffer-self-insert)
	(setq l (cdr l))))
    (define-key map "\C-m" 'exit-minibuffer)
    (define-key map [return] 'exit-minibuffer)
    (define-key map "\C-g" 'exit-minibuffer)
    map)
  "Keymap of minibuffer to input multibyte characters while isearching.")

(defun isearch-minibuffer-non-self-insert ()
  (interactive)
  (setq unread-command-events (cons last-command-event unread-command-events))
  (exit-minibuffer))

(defun isearch-minibuffer-self-insert ()
  (interactive)
  (let ((events (cons last-command-event unread-post-input-method-events)))
    (catch 'isearch-tag
      (while events
	(let* ((event (car events))
	       (cmd (key-binding (vector event))))
	  (cond ((or (eq cmd 'isearch-printing-char)
		     (eq cmd 'isearch-minibuffer-self-insert))
		 (insert event)
		 (setq events (cdr events)))
		((eq cmd 'exit-minibuffer)
		 (setq events (cdr events))
		 (throw 'isearch-tag nil))
		(t
		 (throw 'isearch-tag nil))))))
    (setq unread-post-input-method-events events)
    (exit-minibuffer)))

;;;###autoload
(defun isearch-process-search-multibyte-characters (last-char)
  (if (eq this-command 'isearch-printing-char)
      (let ((overriding-terminal-local-map nil)
	    ;; Let input method work rather tersely.
	    (input-method-verbose-flag nil)
	    (minibuffer-local-map isearch-minibuffer-local-map)
	    str)
	(setq unread-command-events
	      (cons last-char unread-command-events))
	(setq str (read-multilingual-string
		   (concat (isearch-message-prefix) isearch-message)
		   nil
		   current-input-method))
	(if (and str (> (length str) 0))
	    (let ((unread-command-events nil))
	      (isearch-process-search-string str str))
	  (isearch-update)))
    (isearch-process-search-char last-char)))

;;; isearch-x.el ends here
