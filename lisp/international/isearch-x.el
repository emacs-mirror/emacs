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
  (isearch-update))

;;;###autoload
(defun isearch-toggle-input-method ()
  "Toggle input method in interactive search."
  (interactive)
  (let ((overriding-terminal-local-map nil))
    (toggle-input-method))
  (isearch-update))

;;;###autoload
(defun isearch-process-search-multibyte-characters (last-char)
  (let ((overriding-terminal-local-map nil)
	;; Let input method exit when a chunk is inserted.
	(input-method-after-insert-chunk-hook '(inactivate-input-method))
	(input-method-inactivate-hook '(exit-minibuffer))
	;; Let input method work rather tersely.
	(input-method-verbose-flag nil)
	;; A key not handled by the current input method should
	;; terminate the input method.
	(input-method-exit-on-invalid-key t)
	str)
    (setq unread-command-events (cons last-char unread-command-events))
    (setq str (read-multilingual-string
	       (concat (isearch-message-prefix) isearch-message)
	       nil
	       current-input-method))
    (if (and str (> (length str) 0))
	(isearch-process-search-string str str)
      (isearch-update))))

;;; isearch-x.el ends here
