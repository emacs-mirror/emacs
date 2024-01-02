;;; yank-media.el --- Yanking images and HTML  -*- lexical-binding:t -*-

;; Copyright (C) 2021-2024 Free Software Foundation, Inc.

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
(require 'seq)

(defvar yank-media--registered-handlers nil)

;;;###autoload
(defun yank-media ()
  "Yank media (images, HTML and the like) from the clipboard.
This command depends on the current major mode having support for
accepting the media type.  The mode has to register itself using
the `yank-media-handler' mechanism.

Also see `yank-media-types' for a command that lets you explore
all the different selection types."
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
        (funcall (cdar all-types) (caar all-types)
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

(defun yank-media--get-selection (data-type)
  (when-let ((data (gui-backend-get-selection 'CLIPBOARD data-type)))
    (if (string-match-p "\\`text/" (symbol-name data-type))
        (yank-media-types--format data-type data)
      data)))

;;;###autoload
(defun yank-media-handler (types handler)
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

(defun yank-media-types (&optional all)
  "Yank any element present in the primary selection or the clipboard.
This is primarily meant as a debugging tool -- many of the
elements (like images) will be inserted as raw data into the
current buffer.  See `yank-media' instead for a command that
inserts images as images.

By default, data types that aren't supported by
`gui-get-selection' (i.e., that returns nothing if you actually
try to look at the selection) are not included by this command.
If ALL (interactively, the prefix), also include these
non-supported selection data types."
  (interactive "P")
  (let ((elements nil))
    ;; First gather all the data.
    (dolist (type '(PRIMARY CLIPBOARD))
      (when-let ((data-types (gui-get-selection type 'TARGETS)))
        (when (vectorp data-types)
          (seq-do (lambda (data-type)
                    (unless (memq data-type '( TARGETS MULTIPLE
                                               DELETE SAVE_TARGETS))
                      (let ((data (gui-get-selection type data-type)))
                        (when (or data all)
                          ;; Remove duplicates -- the data in PRIMARY and
                          ;; CLIPBOARD are sometimes (mostly) identical,
                          ;; and sometimes not.
                          (let ((old (assq data-type elements)))
                            (when (or (not old)
                                      (not (equal (nth 2 old) data)))
                              (push (list data-type type data)
                                    elements)))))))
                  data-types))))
    ;; Then query the user.
    (unless elements
      (user-error "No elements in the primary selection or the clipboard"))
    (let ((spec
           (completing-read
            "Yank type: "
            (mapcar (lambda (e)
                      (format "%s:%s" (downcase (symbol-name (cadr e)))
                              (car e)))
                    elements)
            nil t)))
      (dolist (elem elements)
        (when (equal (format "%s:%s" (downcase (symbol-name (cadr elem)))
                             (car elem))
                     spec)
          (insert (yank-media-types--format (car elem) (nth 2 elem))))))))

(defun yank-media-types--format (data-type data)
  (cond
   ((not (stringp data))
    (format "%s" data))
   ((string-match-p "\\`text/" (symbol-name data-type))
    ;; We may have utf-16, which Emacs won't detect automatically.
    (let ((coding-system (yank-media--utf-16-p data)))
      (if coding-system
          (decode-coding-string data coding-system)
        ;; Some programs add a nul character at the end of text/*
        ;; selections.  Remove that.
        (if (zerop (elt data (1- (length data))))
            (substring data 0 (1- (length data)))
          data))))
   (t
    data)))

(defun yank-media--utf-16-p (data)
  (and (zerop (mod (length data) 2))
       (let ((stats (vector 0 0)))
         (dotimes (i (length data))
           (when (zerop (elt data i))
             (setf (aref stats (mod i 2))
                   (1+ (aref stats (mod i 2))))))
         ;; If we have more than 90% every-other nul, then it's
         ;; pretty likely to be utf-16.
         (cond
          ((> (/ (float (elt stats 0)) (/ (length data) 2))
              0.9)
           ;; Big endian.
           'utf-16-be)
          ((> (/ (float (elt stats 1)) (/ (length data) 2))
              0.9)
           ;; Little endian.
           'utf-16-le)))))

(provide 'yank-media)

;;; yank-media.el ends here
