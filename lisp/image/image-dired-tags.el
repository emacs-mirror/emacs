;;; image-dired-tags.el --- Tag support for Image-Dired  -*- lexical-binding: t -*-

;; Copyright (C) 2005-2024 Free Software Foundation, Inc.

;; Author: Mathias Dahl <mathias.rem0veth1s.dahl@gmail.com>
;; Maintainer: Stefan Kangas <stefankangas@gmail.com>
;; Keywords: multimedia
;; Package: image-dired

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

;; See the description of the `image-dired' package.

;;; Code:

(require 'dired)

(require 'image-dired-util)

(declare-function image-dired--with-marked "image-dired")

(defvar image-dired-dir)
(defvar image-dired-thumbnail-storage)
(defvar image-dired-tags-db-file)

(defmacro image-dired--with-db-file (&rest body)
  "Run BODY in a temp buffer containing `image-dired-tags-db-file'.
Return the value of last form in BODY."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (if (file-exists-p image-dired-tags-db-file)
         (insert-file-contents image-dired-tags-db-file))
     ,@body))

(defun image-dired-sane-db-file ()
  "Check if `image-dired-tags-db-file' exists.
If not, try to create it (including any parent directories).
Signal error if there are problems creating it."
  (require 'image-dired)                ; for `image-dired-dir'
  (or (file-exists-p image-dired-tags-db-file)
      (let (dir buf)
        (unless (file-directory-p (setq dir (file-name-directory
                                             image-dired-tags-db-file)))
          (with-file-modes #o700
            (make-directory dir t)))
        (with-current-buffer (setq buf (create-file-buffer
                                        image-dired-tags-db-file))
          (with-file-modes #o600
            (write-file image-dired-tags-db-file)))
        (kill-buffer buf)
        (file-exists-p image-dired-tags-db-file))
      (error "Could not create %s" image-dired-tags-db-file)))

(defvar image-dired-tag-history nil "Variable holding the tag history.")

(defun image-dired-write-tags (file-tags)
  "Write file tags to database.
Write each file and tag in FILE-TAGS to the database.
FILE-TAGS is an alist in the following form:
 ((FILE . TAG) ... )"
  (image-dired-sane-db-file)
  (let (end file tag)
    (image-dired--with-db-file
      (setq buffer-file-name image-dired-tags-db-file)
      (dolist (elt file-tags)
        (setq file (car elt)
              tag (cdr elt))
        (goto-char (point-min))
        (if (search-forward-regexp (format "^%s.*$" file) nil t)
            (progn
              (setq end (point))
              (beginning-of-line)
              (when (not (search-forward (format ";%s" tag) end t))
                (end-of-line)
                (insert (format ";%s" tag))))
          (goto-char (point-max))
          (insert (format "%s;%s\n" file tag))))
      (save-buffer))))

(defun image-dired-remove-tag (files tag)
  "For each file in FILES, remove TAG from the image database.
FILES can be a name of a single file (a string) or a list of file names."
  (image-dired-sane-db-file)
  (image-dired--with-db-file
    (setq buffer-file-name image-dired-tags-db-file)
    (let (end)
      (unless (listp files)
        (if (stringp files)
            (setq files (list files))
          (error "Files must be a string or a list of strings!")))
      (dolist (file files)
        (goto-char (point-min))
        (when (search-forward-regexp (format "^%s;" file) nil t)
          (end-of-line)
          (setq end (point))
          (beginning-of-line)
          (when (search-forward-regexp
                 (format "\\(;%s\\)\\($\\|;\\)" tag) end t)
            (delete-region (match-beginning 1) (match-end 1))
            ;; Check if file should still be in the database.
            ;; If it has no tags or comments, it will be removed.
            (end-of-line)
            (setq end (point))
            (beginning-of-line)
            (when (not (search-forward ";" end t))
              (kill-line 1))))))
    (save-buffer)))

(defun image-dired-list-tags (file)
  "Read all tags for image FILE from the image database.
Value is a list of all tags for FILE."
  (image-dired-sane-db-file)
  (image-dired--with-db-file
    (let (end (tags ""))
      (when (search-forward-regexp (format "^%s" file) nil t)
        (end-of-line)
        (setq end (point))
        (beginning-of-line)
        (if (search-forward ";" end t)
            (if (search-forward "comment:" end t)
                (if (search-forward ";" end t)
                    (setq tags (buffer-substring (point) end)))
              (setq tags (buffer-substring (point) end)))))
      (split-string tags ";"))))

;;;###autoload
(defun image-dired-tag-files (arg)
  "Tag file(s) which are marked in a Dired buffer.
With prefix ARG, tag the file at point."
  (interactive "P" dired-mode)
  (let ((tag (completing-read
              "Tags to add (separate tags with a semicolon): "
              image-dired-tag-history nil nil nil 'image-dired-tag-history))
        files)
    (if arg
        (setq files (list (dired-get-filename)))
      (setq files (dired-get-marked-files)))
    (image-dired-write-tags
     (mapcar
      (lambda (x)
        (cons x tag))
      files))))

(defun image-dired-tag-thumbnail ()
  "Tag current or marked thumbnails."
  (interactive nil image-dired-thumbnail-mode)
  (let ((tag (completing-read
              "Tags to add (separate tags with a semicolon): "
              image-dired-tag-history nil nil nil 'image-dired-tag-history)))
    (image-dired--with-marked
     (image-dired-write-tags
      (list (cons (image-dired-original-file-name) tag)))
     (image-dired-update-property
      'tags (image-dired-list-tags (image-dired-original-file-name))))))

;;;###autoload
(defun image-dired-delete-tag (arg)
  "Remove tag for selected file(s).
With prefix argument ARG, remove tag from file at point."
  (interactive "P" dired-mode)
  (let ((tag (completing-read "Tag to remove: " image-dired-tag-history
                              nil nil nil 'image-dired-tag-history))
        files)
    (if arg
        (setq files (list (dired-get-filename)))
      (setq files (dired-get-marked-files)))
    (image-dired-remove-tag files tag)))

(defun image-dired-tag-thumbnail-remove ()
  "Remove tag from current or marked thumbnails."
  (interactive nil image-dired-thumbnail-mode)
  (let ((tag (completing-read "Tag to remove: " image-dired-tag-history
                              nil nil nil 'image-dired-tag-history)))
    (image-dired--with-marked
     (image-dired-remove-tag (image-dired-original-file-name) tag)
     (image-dired-update-property
      'tags (image-dired-list-tags (image-dired-original-file-name))))))

(defun image-dired-write-comments (file-comments)
  "Write file comments specified by FILE-COMMENTS comments to database.
FILE-COMMENTS is an alist on the following form:
 ((FILE . COMMENT) ... )"
  (image-dired-sane-db-file)
  (let (end comment-beg-pos comment-end-pos file comment)
    (image-dired--with-db-file
      (setq buffer-file-name image-dired-tags-db-file)
      (dolist (elt file-comments)
        (setq file (car elt)
              comment (cdr elt))
        (goto-char (point-min))
        (if (search-forward-regexp (format "^%s.*$" file) nil t)
            (progn
              (setq end (point))
              (beginning-of-line)
              ;; Delete old comment, if any
              (when (search-forward ";comment:" end t)
                (setq comment-beg-pos (match-beginning 0))
                ;; Any tags after the comment?
                (if (search-forward ";" end t)
                    (setq comment-end-pos (- (point) 1))
                  (setq comment-end-pos end))
                ;; Delete comment tag and comment
                (delete-region comment-beg-pos comment-end-pos))
              ;; Insert new comment
              (beginning-of-line)
              (unless (search-forward ";" end t)
                (end-of-line)
                (insert ";"))
              (insert (format "comment:%s;" comment)))
          ;; File does not exist in database - add it.
          (goto-char (point-max))
          (insert (format "%s;comment:%s\n" file comment))))
      (save-buffer))))

(defun image-dired-update-property (prop value)
  "Set text property PROP of text at point to have the given VALUE."
  (let ((inhibit-read-only t))
    (put-text-property
     (point) (1+ (point))
     prop
     value)))

;;;###autoload
(defun image-dired-dired-comment-files ()
  "Add comment to current or marked files in Dired."
  (interactive nil dired-mode)
  (let ((comment (image-dired-read-comment)))
    (image-dired-write-comments
     (mapcar
      (lambda (curr-file)
        (cons curr-file comment))
      (dired-get-marked-files)))))

(defun image-dired-read-comment (&optional file)
  "Read comment for an image.
Optionally use old comment from FILE as initial value."
  (let ((comment
         (read-string
          "Comment: "
          (if file (image-dired-get-comment file)))))
    comment))

(defun image-dired-get-comment (file)
  "Get comment for file FILE."
  (image-dired-sane-db-file)
  (image-dired--with-db-file
    (let (end comment-beg-pos comment-end-pos comment)
      (when (search-forward-regexp (format "^%s" file) nil t)
        (end-of-line)
        (setq end (point))
        (beginning-of-line)
        (when (search-forward ";comment:" end t)
          (setq comment-beg-pos (point))
          (if (search-forward ";" end t)
              (setq comment-end-pos (- (point) 1))
            (setq comment-end-pos end))
          (setq comment (buffer-substring
                         comment-beg-pos comment-end-pos))))
      comment)))


;;; Tag support

(defvar image-dired-widget-list nil
  "List to keep track of meta data in edit buffer.")

(declare-function widget-forward "wid-edit" (arg))

;;;###autoload
(defun image-dired-dired-edit-comment-and-tags ()
  "Edit comment and tags of current or marked image files.
Edit comment and tags for all marked image files in an
easy-to-use form."
  (interactive nil dired-mode)
  (setq image-dired-widget-list nil)
  ;; Setup buffer.
  (let ((files (dired-get-marked-files)))
    (pop-to-buffer-same-window "*Image-Dired Edit Meta Data*")
    (kill-all-local-variables)
    (let ((inhibit-read-only t))
      (erase-buffer))
    (remove-overlays)
    ;; Some help for the user.
    (widget-insert
     (substitute-command-keys
      "\\<widget-field-keymap>
Edit comments and tags for each image.  Separate multiple tags
with a comma.  Move forward between fields using \\[widget-forward] \
or \\[widget-field-activate].
Move to the previous field using \\[widget-backward].  Save by
activating the \"Save\" button at the bottom of the form or
cancel the operation by activating the \"Cancel\" button.\n\n"))
    ;; Here comes all images and a comment and tag field for each
    ;; image.
    (let (thumb-file img comment-widget tag-widget)

      (dolist (file files)

        (setq thumb-file (image-dired-thumb-name file)
              img (create-image thumb-file))

        (insert-image img)
        (widget-insert "\n\nComment: ")
        (setq comment-widget
              (widget-create 'editable-field
                             :size 60
                             :format "%v "
                             :value (or (image-dired-get-comment file) "")))
        (widget-insert "\nTags:    ")
        (setq tag-widget
              (widget-create 'editable-field
                             :size 60
                             :format "%v "
                             :value (or (mapconcat
                                         #'identity
                                         (image-dired-list-tags file)
                                         ",") "")))
        ;; Save information in all widgets so that we can use it when
        ;; the user saves the form.
        (setq image-dired-widget-list
              (append image-dired-widget-list
                      (list (list file comment-widget tag-widget))))
        (widget-insert "\n\n")))

    ;; Footer with Save and Cancel button.
    (widget-insert "\n")
    (widget-create 'push-button
                   :notify
                   (lambda (&rest _ignore)
                     (image-dired-save-information-from-widgets)
                     (bury-buffer)
                     (message "Done"))
                   "Save")
    (widget-insert " ")
    (widget-create 'push-button
                   :notify
                   (lambda (&rest _ignore)
                     (bury-buffer)
                     (message "Operation canceled"))
                   "Cancel")
    (widget-insert "\n")
    (use-local-map widget-keymap)
    (widget-setup)
    ;; Jump to the first widget.
    (widget-forward 1)))

(defun image-dired-save-information-from-widgets ()
  "Save information found in `image-dired-widget-list'.
Use the information in `image-dired-widget-list' to save comments and
tags to their respective image file.  Internal function used by
`image-dired-dired-edit-comment-and-tags'."
  (let (file comment tag-string tag-list lst)
    (image-dired-write-comments
     (mapcar
      (lambda (widget)
        (setq file (car widget)
              comment (widget-value (cadr widget)))
        (cons file comment))
      image-dired-widget-list))
    (image-dired-write-tags
     (dolist (widget image-dired-widget-list lst)
       (setq file (car widget)
             tag-string (widget-value (car (cddr widget)))
             tag-list (split-string tag-string ","))
       (dolist (tag tag-list)
         (push (cons file tag) lst))))))

(provide 'image-dired-tags)

;;; image-dired-tags.el ends here
