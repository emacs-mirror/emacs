;;; isearch-x.el --- extended isearch handling commands

;; Copyright (C) 1995 Free Software Foundation, Inc.
;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.

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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Code:

;;;###autoload
(defun isearch-toggle-specified-input-method ()
  "Select and toggle specified input method in interactive search."
  (interactive)
  ;; Let the command `toggle-input-method' ask users to select input
  ;; method interactively.
  (setq default-input-method nil)
  (isearch-toggle-input-method))

;;;###autoload
(defun isearch-toggle-input-method ()
  "Toggle input method in interactive search."
  (interactive)
  (if isearch-multibyte-characters-flag
      (setq isearch-multibyte-characters-flag nil)
    (condition-case nil
	(progn
	  (if (null default-input-method)
	      (let ((overriding-terminal-local-map nil))
		;; No input method has ever been selected.  Select one
		;; interactively now.  This also sets
		;; `default-input-method-title' to the title of the
		;; selected input method.
		(toggle-input-method)
		;; And, inactivate it for the moment.
		(toggle-input-method)))
	  (setq isearch-multibyte-characters-flag t))
      (error (ding))))
  (isearch-update))

(defun isearch-input-method-after-insert-chunk-function ()
  (funcall inactivate-current-input-method-function))

(defun isearch-process-search-multibyte-characters (last-char)
  (let* ((overriding-terminal-local-map nil)
	 ;; Let input method exit when a chunk is inserted.
	 (input-method-after-insert-chunk-hook
	  '(isearch-input-method-after-insert-chunk-function))
	 (input-method-inactivate-hook '(exit-minibuffer))
	 ;; Let input method work rather tersely.
	 (input-method-tersely-flag t)
	 str)
    (setq unread-command-events (cons last-char unread-command-events))
    (setq str (read-multilingual-string (concat (isearch-message-prefix)
						isearch-message)))
    (isearch-process-search-string str str)))

;;; isearch-x.el ends here
