;;; thread.el --- List active threads in a buffer -*- lexical-binding: t -*-

;; Copyright (C) 2018 Free Software Foundation, Inc.

;; Author: Gemini Lasswell <gazally@runbox.com>
;; Maintainer: emacs-devel@gnu.org
;; Keywords: lisp, tools, maint

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

;;; Code:

;;;###autoload
(defun thread-handle-event (event)
  "Handle thread events, propagated by `thread-signal'.
An EVENT has the format
  (thread-event THREAD ERROR-SYMBOL DATA)"
  (interactive "e")
  (if (and (consp event)
           (eq (car event) 'thread-event)
	   (= (length event) 4))
      (let ((thread (cadr event))
            (err (cddr event)))
        (message "Error %s: %S" thread err))))

(make-obsolete 'thread-alive-p 'thread-live-p "27.1")

(provide 'thread)
;;; thread.el ends here
