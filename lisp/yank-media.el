;;; yank-media.el --- Yanking images and HTML  -*- lexical-binding:t -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

;; Author: Lars Ingebrigtsen <larsi@gnus.org>
;; Keywords: utility

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

(require 'cl-lib)

(defvar yank-media--registered-handlers nil)

;;;###autoload
(defun yank-media ()
  "Yank media (images, HTML and the like) from the clipboard.
This command depends on the current major mode having support for
accepting the media type.  The mode has to register itself using
the `register-yank-media-handler' mechanism."
  (interactive)
  (unless yank-media--registered-handlers
    (user-error "The `%s' mode hasn't registered any handlers" major-mode))
  (let ((all-types nil))
    (pcase-dolist (`(,handled-type . ,handler)
                   yank-media--registered-handlers)
      (dolist (type (yank-media--find-matching-media handled-type))
        (push (cons type handler) all-types)))
    (unless all-types
      (user-error
       "No handler in the current buffer for anything on the clipboard"))
    ;; We have a handler in the current buffer; if there's just
    ;; matching type, just call the handler.
    (if (length= all-types 1)
        (funcall (cdar all-types)
                 (yank-media--get-selection (caar all-types)))
      ;; More than one type the user for what type to insert.
      (let ((type
             (intern
              (completing-read "Several types available, choose one: "
                               (mapcar #'car all-types) nil t))))
        (funcall (alist-get type all-types)
                 type (yank-media--get-selection type))))))

(defun yank-media--find-matching-media (handled-type)
  (seq-filter
   (lambda (type)
     (pcase-let ((`(,major ,minor) (split-string (symbol-name type) "/")))
       (if (and (equal major "image")
                (not (image-type-available-p (intern minor))))
           ;; Just filter out all the image types that Emacs doesn't
           ;; support, because the clipboard is full of things like
           ;; `image/x-win-bitmap'.
           nil
         ;; Check that the handler wants this type.
         (and (if (symbolp handled-type)
                  (eq handled-type type)
                (string-match-p handled-type (symbol-name type)))
              ;; An element may be in TARGETS but be empty.
              (yank-media--get-selection type)))))
   (gui-get-selection 'CLIPBOARD 'TARGETS)))

(defun yank-media--get-selection (type)
  (when-let ((data (gui-get-selection 'CLIPBOARD type)))
    (when-let ((charset (get-text-property 0 'charset data)))
      (setq data (encode-coding-string data charset)))
    ;; Some programs add a nul character at the end of text/*
    ;; selections.  Remove that.
    (when (and (string-match-p "\\`text/" (symbol-name type))
               (zerop (elt data (1- (length data)))))
      (setq data (substring data 0 (1- (length data)))))
    data))

;;;###autoload
(defun register-yank-media-handler (types handler)
  "Register HANDLER for dealing with `yank-media' actions for TYPES.
TYPES should be a MIME media type symbol, a regexp, or a list
that can contain both symbols and regexps.

HANDLER is a function that will be called with two arguments: The
MIME type (a symbol on the form `image/png') and the selection
data (a string)."
  (make-local-variable 'yank-media--registered-handlers)
  (dolist (type (ensure-list types))
    (setf (alist-get type yank-media--registered-handlers nil nil #'equal)
          handler)))

(provide 'yank-media)

;;; yank-media.el ends here
