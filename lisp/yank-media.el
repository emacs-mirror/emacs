;;; yank-media.el --- Yanking images and HTML  -*- lexical-binding:t -*-

;; Copyright (C) 2021-2026 Free Software Foundation, Inc.

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

(require 'seq)

(defvar yank-media--registered-handlers nil)

(defvar yank-media-autoselect-function #'yank-media-autoselect-function
  "Function to auto select the best MIME types when many are available.
The function is called with a list of MIME types that have handler in
the current buffer, and should return the list of MIME types to use in
order of their priority.  When `yank-media' auto-selects the MIME type,
it will always choose the first one of the returned list.
Major-mode authors can change this variable to influence the selection
process.")

(defvar yank-media-preferred-types
  `(;; Check first since LibreOffice also puts a PNG image in the
    ;; clipboard when a table cell is copied.
    application/x-libreoffice-tsvc
    ;; Give PNG more priority.
    image/png
    image/jpeg
    ;; These are files copied/cut to the clipboard from a file manager
    ;; in a GNU/Linux and/or BSD environment.
    ,@(when (memq window-system '(x pgtk))
        (list (lambda (mimetypes)
                (ensure-list
                 (seq-find (lambda (type)
                             (string-match-p "x-special/\\(gnome\\|KDE\\|mate\\)-copied-files"
                                             (symbol-name type)))
                           mimetypes)))))
    ;; FIXME: We should have a way to handle text/rtf.
    text/html)
  "List of MIME types in the order of preference.
Each element in the list should be a symbol to choose that MIME type
exclusively, or a function of one argument and should return the list of
MIME types to use in order of their priority or nil if no preferred type
is found.
Major-mode authors can change this variable to influence the selection
process, or by directly changing the variable
`yank-media-autoselect-function'.")

(defun yank-media-autoselect-function (mimetypes)
  (catch 'preferred
    (dolist (typ yank-media-preferred-types)
      (let ((ret (if (functionp typ)
                     (funcall typ mimetypes)
                   (and (memq typ mimetypes) (list typ)))))
        (when ret (throw 'preferred ret))))))

;;;###autoload
(defun yank-media (&optional noselect)
  "Yank media (images, HTML and the like) from the clipboard.
This command depends on the current major mode having support for
accepting the media type.  The mode has to register itself using
the `yank-media-handler' mechanism.
Optional argument NOSELECT non-nil (interactively, with a prefix
argument) means to skip auto-selecting the best MIME type and ask for
the MIME type to use.

Also see `yank-media-types' for a command that lets you explore
all the different selection types."
  (interactive "P")
  (unless yank-media--registered-handlers
    (user-error "The `%s' mode hasn't registered any handlers" major-mode))
  (let ((all-types nil)
        pref-type)
    (pcase-dolist (`(,handled-type . ,handler)
                   yank-media--registered-handlers)
      (dolist (type (yank-media--find-matching-media handled-type))
        (push (cons type handler) all-types)))
    (unless all-types
      (user-error
       "No handler in the current buffer for anything on the clipboard"))
    (setq pref-type (and (null noselect)
                         (funcall yank-media-autoselect-function
                                  (mapcar #'car all-types))))
    (cond
     ;; We are asked to autoselect and have a preferred MIME type.
     ((and (null noselect) pref-type)
      (funcall (cdr (assq (car pref-type) all-types))
               (car pref-type)
               (yank-media--get-selection (car pref-type))))
     ;; We are asked to autoselect and no preferred MIME type.
     ((and (null noselect) (null pref-type))
      (message
       (substitute-command-keys
        "No preferred MIME type to yank, try \\[universal-argument] \\[yank-media]")))
     ;; No autoselection and there's only one media type available.
     ((and noselect (length= all-types 1))
      (when (y-or-n-p (format "Yank the `%s' clipboard item?"
                              (caar all-types)))
        (funcall (cdar all-types) (caar all-types)
                 (yank-media--get-selection (caar all-types)))))
     ;; No autoselection and multiple media types available.
     ((and noselect (length> all-types 1))
      (let ((type
             (intern
              (completing-read "Several types available, choose one: "
                               (or pref-type (mapcar #'car all-types))
                               nil t))))
        (funcall (alist-get type all-types)
                 type (yank-media--get-selection type)))))))

(defun yank-media--find-matching-media (handled-type)
  (seq-filter
   (lambda (type)
     (pcase-let ((`(,major ,minor) (split-string (symbol-name type) "/")))
       (if (and (equal major "image")
                (not (image-type-available-p
                      ;; Usually, MIME subtype is the same as Emacs'
                      ;; identifier for an image type.  But for SVG, the
                      ;; identifier is 'svg, while the MIME type is
                      ;; image/svg+xml. So we make the exception here.
                      (intern (if (string= minor "svg+xml") "svg" minor)))))
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
  (when-let* ((data (gui-get-selection 'CLIPBOARD data-type)))
    (if (string-match-p "\\`text/" (symbol-name data-type))
        (yank-media-types--format data-type data)
      data)))

;;;###autoload
(defun yank-media-handler (types handler)
  "Register HANDLER for dealing with `yank-media' actions for TYPES.
TYPES should be a MIME media type symbol, a regexp, or a list
that can contain both symbols and regexps.

HANDLER is a function that will be called with two arguments: The
MIME type (a symbol of the form `image/png') and the selection
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
      (when-let* ((data-types (gui-get-selection type 'TARGETS)))
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
  (and (evenp (length data))
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
