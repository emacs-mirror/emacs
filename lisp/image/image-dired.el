;;; image-dired.el --- use dired to browse and manipulate your images -*- lexical-binding: t -*-

;; Copyright (C) 2005-2024 Free Software Foundation, Inc.

;; Author: Mathias Dahl <mathias.rem0veth1s.dahl@gmail.com>
;; Maintainer: Stefan Kangas <stefankangas@gmail.com>
;; Version: 0.5
;; Keywords: multimedia

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

;; BACKGROUND
;; ==========
;;
;;  I needed a program to browse, organize and tag my pictures.  I got
;; tired of the old gallery program I used as it did not allow
;; multi-file operations easily.  Also, it put things out of my
;; control.  Image viewing programs I tested did not allow multi-file
;; operations or did not do what I wanted it to.
;;
;;  So, I got the idea to use the wonderful functionality of Emacs and
;; `dired' to do it.  It would allow me to do almost anything I wanted,
;; which is basically just to browse all my pictures in an easy way,
;; letting me manipulate and tag them in various ways.  `dired' already
;; provide all the file handling and navigation facilities; I only
;; needed to add some functions to display the images.
;;
;;  I briefly tried out thumbs.el, and although it seemed more
;; powerful than this package, it did not work the way I wanted to.  It
;; was too slow to create thumbnails of all files in a directory (I
;; currently keep all my 2000+ images in the same directory) and
;; browsing the thumbnail buffer was slow too.  image-dired.el will not
;; create thumbnails until they are needed and the browsing is done
;; quickly and easily in Dired.  I copied a great deal of ideas and
;; code from there though...  :)
;;
;;  `image-dired' stores the thumbnail files in `image-dired-dir'
;; using the file name format ORIGNAME.thumb.ORIGEXT.  For example
;; ~/.emacs.d/image-dired/myimage01.thumb.jpg.  The "database" is for
;; now just a plain text file with the following format:
;;
;; file-name-non-directory;comment:comment-text;tag1;tag2;tag3;...;tagN
;;
;; PREREQUISITES
;; =============
;;
;; * The GraphicsMagick or ImageMagick package; Image-Dired uses
;;   whichever is available.
;;
;;   A) For GraphicsMagick, `gm' is used.
;;      Find it here:  http://www.graphicsmagick.org/
;;
;;   B) For ImageMagick, `convert' and `mogrify' are used.
;;      Find it here:  https://www.imagemagick.org.
;;
;; * For non-lossy rotation of JPEG images, the JpegTRAN program is
;;   needed.
;;
;; * For `image-dired-set-exif-data' to work, the command line tool `exiftool' is
;;   needed.  It can be found here: https://exiftool.org/.  This
;;   function is, among other things, used for writing comments to
;;   image files using `image-dired-thumbnail-set-image-description'.
;;
;;
;; USAGE
;; =====
;;
;; This information has been moved to the manual.  Type `C-h r' to open
;; the Emacs manual and go to the node Thumbnails by typing `g
;; Image-Dired RET'.
;;
;; Quickstart: M-x image-dired RET DIRNAME RET
;;
;; where DIRNAME is a directory containing image files.
;;
;; LIMITATIONS
;; ===========
;;
;; * Supports all image formats that Emacs and convert supports, but
;;   the thumbnails are hard-coded to JPEG or PNG format.  It uses
;;   JPEG by default, but can optionally follow the Thumbnail Managing
;;   Standard (v0.9.0, Dec 2020), which mandates PNG.  See the user
;;   option `image-dired-thumbnail-storage'.
;;
;; * WARNING: The "database" format used might be changed so keep a
;;   backup of `image-dired-tags-db-file' when testing new versions.
;;
;; TODO
;; ====
;;
;; * Investigate if it is possible to also write the tags to the image
;;   files.
;;
;; * From thumbs.el: Add an option for clean-up/max-size functionality
;;   for thumbnail directory.
;;
;; * Add `image-dired-display-thumbs-ring' and functions to cycle that.  Find out
;;   which is best, saving old batch just before inserting new, or
;;   saving the current batch in the ring when inserting it.  Adding
;;   it probably needs rewriting `image-dired-display-thumbs' to be more general.
;;
;; * Find some way of toggling on and off really nice keybindings in
;;   Dired (for example, using C-n or <down> instead of C-S-n).
;;   Richard suggested that we could keep C-t as prefix for
;;   image-dired commands as it is currently not used in Dired.  He
;;   also suggested that `dired-next-line' and `dired-previous-line'
;;   figure out if image-dired is enabled in the current buffer and,
;;   if it is, call `image-dired-dired-next-line' and `image-dired-dired-previous-line',
;;   respectively.  Update: This is partly done; some bindings have
;;   now been added to Dired.
;;
;; * In some way keep track of buffers and windows and stuff so that
;;   it works as the user expects.
;;
;; * More/better documentation.

;;; Code:

(require 'dired)
(require 'image-mode)
(require 'widget)
(require 'xdg)

(eval-when-compile
  (require 'cl-lib)
  (require 'wid-edit))

(require 'image-dired-external)
(require 'image-dired-tags)
(require 'image-dired-util)


;;; Customizable variables

(defgroup image-dired nil
  "Use Dired to browse your images as thumbnails, and more."
  :prefix "image-dired-"
  :link '(info-link "(emacs) Image-Dired")
  :group 'multimedia)

(defcustom image-dired-dir (locate-user-emacs-file "image-dired/")
  "Directory where thumbnail images are stored.

The value of this option is ignored if Image-Dired is customized
to use the Thumbnail Managing Standard; they will be saved in
\"$XDG_CACHE_HOME/thumbnails/\" instead.  See
`image-dired-thumbnail-storage'."
  :type 'directory)

(defcustom image-dired-thumbnail-storage 'image-dired
  "How `image-dired' stores thumbnail files.
There are three ways that Image-Dired can store and generate
thumbnails:

 1. According to the Thumbnail Managing Standard, which allows
    sharing of thumbnails across different programs.  Thumbnails
    will be stored in \"$XDG_CACHE_HOME/thumbnails/\"

    Set this user option to one of the following symbols:

    - `standard' means use thumbnails sized 128x128.
    - `standard-large' means use thumbnails sized 256x256.
    - `standard-x-large' means use thumbnails sized 512x512.
    - `standard-xx-large' means use thumbnails sized 1024x1024.

 2. In the Image-Dired specific directory, as indicated by
    `image-dired-dir'.

    Set this user option to `image-dired' (`use-image-dired-dir'
    also works, for backward-compatibility reasons).

 3. In a subdirectory \".image-dired\" in the same directory
    as the image files.

    Set this user option to `per-directory'.

To control the default size of thumbnails for alternatives (2)
and (3) above, customize the value of `image-dired-thumb-size'.

With Thumbnail Managing Standard, save thumbnails in the PNG
format, as mandated by that standard; otherwise save them as JPEG.

For more information on the Thumbnail Managing Standard, see:
https://specifications.freedesktop.org/thumbnail-spec/thumbnail-spec-latest.html"
  :type '(choice :tag "How to store thumbnail files"
                 (const :tag "Use image-dired-dir" image-dired)
                 (const :tag "Thumbnail Managing Standard (normal 128x128)"
                        standard)
                 (const :tag "Thumbnail Managing Standard (large 256x256)"
                        standard-large)
                 (const :tag "Thumbnail Managing Standard (larger 512x512)"
                        standard-x-large)
                 (const :tag "Thumbnail Managing Standard (extra large 1024x1024)"
                        standard-xx-large)
                 (const :tag "Per-directory" per-directory))
  :version "29.1")
;;;###autoload(put 'image-dired-thumbnail-storage 'safe-local-variable (lambda (x) (eq x 'per-directory)))

(define-obsolete-variable-alias 'image-dired-db-file
  'image-dired-tags-db-file "29.1")
(defcustom image-dired-tags-db-file
  (expand-file-name ".image-dired_db" image-dired-dir)
  "Database file where image-dired file names and associated tags are stored."
  :type 'file)

(defcustom image-dired-rotate-original-ask-before-overwrite t
  "Confirm overwriting of original file after image-rotate operation.
If non-nil, ask user for confirmation before overwriting the
original image file by `image-dired-temp-rotate-image-file'."
  :type 'boolean)

(defcustom image-dired-thumb-size
  ;; This is ignored when using the Thumbnail Managing Standard, but
  ;; this provides a better default (e.g., when 'image-dired-thumbnail-storage'
  ;; is `image-dired' in a directory local variables).
  (pcase image-dired-thumbnail-storage
    ('standard 128)
    ('standard-large 256)
    ('standard-x-large 512)
    ('standard-xx-large 1024)
    (_ 128))
  "Default size of thumbnails in pixels.
The value of this option is ignored if Image-Dired is customized
to use the Thumbnail Managing Standard; the standard sizes will
be used instead.  See `image-dired-thumbnail-storage'."
  :type 'natnum
  :version "29.1")

(defcustom image-dired-thumb-relief 2
  "Size of button-like border around thumbnails."
  :type 'natnum)

(defcustom image-dired-thumb-margin 2
  "Size of the margin around thumbnails.
This is where you see the cursor."
  :type 'natnum)

(defcustom image-dired-thumb-visible-marks t
  "Make marks and flags visible in thumbnail buffer.
If non-nil, apply the `image-dired-thumb-mark' face to marked
images and `image-dired-thumb-flagged' to images flagged for
deletion."
  :type 'boolean
  :version "28.1")

(defcustom image-dired-line-up-method 'dynamic
  "Default method for line-up of thumbnails in thumbnail buffer.
Used by `image-dired-display-thumbs' and other functions that need
to line-up thumbnails.  The value `dynamic' means to use the
available width of the window containing the thumbnail buffer,
the value `fixed' means to use `image-dired-thumbs-per-row',
the value `interactive' means ask the user, and the
value `none' means that no automatic line-up will be done."
  :type '(choice :tag "Default line-up method"
                 (const :tag "Dynamic" dynamic)
                 (const :tag "Fixed" fixed)
                 (const :tag "Interactive" interactive)
                 (const :tag "No line-up" none)))

(defcustom image-dired-thumbs-per-row 3
  "Number of thumbnails to display per row in thumb buffer."
  :type 'natnum)

(defcustom image-dired-track-movement t
  "The current state of the Image-Dired tracking and mirroring of thumbnails.
For more information, see the documentation of
`image-dired-toggle-movement-tracking'."
  :type 'boolean)

(defcustom image-dired-display-properties-format "%n %d/%f %s %t %c"
  "Display format for thumbnail properties.
This is used for the header line in the Image-Dired buffer.

The following %-specs in the value are replaced by `format-spec' before
displaying:

  \"%f\"  The file name (without a directory) of the
          original image file.
  \"%n\"  The number of this image out of the total (e.g. 1/10).
  \"%b\"  The associated Dired buffer name.
  \"%d\"  The name of the file's directory.
  \"%s\"  The image file size.
  \"%t\"  The list of tags (from the Image-Dired database).
  \"%c\"  The comment (from the Image-Dired database)."
  :type 'string
  :safe #'stringp
  :version "29.1")

(defcustom image-dired-external-viewer
  ;; TODO: Use mailcap, dired-guess-shell-alist-default,
  ;; dired-view-command-alist.
  (cond ((executable-find "display") "display")
        ((executable-find "feh") "feh")
        ((executable-find "gm") "gm display")
        ((executable-find "xli") "xli")
        ((executable-find "qiv") "qiv -t")
        ((executable-find "xloadimage") "xloadimage"))
  "Shell command to invoke the external image viewer program.
Should include command-line arguments if needed.  Used when displaying
original image from `image-dired-thumbnail-mode'."
  :version "29.1"
  :type '(choice string
                 (const :tag "Not Set" nil)))

(defcustom image-dired-main-image-directory
  (or (xdg-user-dir "PICTURES") "~/pics/")
  "Name of main image directory, if any.
Used by `image-dired-copy-with-exif-file-name'."
  :type 'string
  :version "29.1")

(defcustom image-dired-show-all-from-dir-max-files 1000
  "Maximum number of files in directory to show before prompting.

If there are more image files in a selected directory than this number,
the `image-dired-show-all-from-dir' command will ask for
confirmation before creating the thumbnail buffer.  If this
variable is nil, never ask."
  :type '(choice integer
                 (const :tag "Don't ask for confirmation" nil))
  :version "29.1")

(defcustom image-dired-marking-shows-next t
  "If non-nil, marking, unmarking or flagging an image shows the next image.

This affects the following commands:
\\<image-dired-thumbnail-mode-map>
    `image-dired-flag-thumb-original-file'   (bound to \\[image-dired-flag-thumb-original-file])
    `image-dired-mark-thumb-original-file'   (bound to \\[image-dired-mark-thumb-original-file])
    `image-dired-unmark-thumb-original-file' (bound to \\[image-dired-unmark-thumb-original-file])"
  :type 'boolean
  :version "29.1")


;;; Faces

;;;; Header line

(defface image-dired-thumb-header-file-name
  '((default :weight bold))
  "Face for the file name in the header line of the thumbnail buffer."
  :version "29.1")

(defface image-dired-thumb-header-directory-name
  '((default :inherit header-line))
  "Face for the directory name in the header line of the thumbnail buffer."
  :version "29.1")

(defface image-dired-thumb-header-file-size
  '((((class color) (min-colors 88)) :foreground "cadet blue")
    (((class color) (min-colors 16)) :foreground "black")
    (default :inherit header-line))
  "Face for the file size in the header line of the thumbnail buffer."
  :version "29.1")

(defface image-dired-thumb-header-image-count
  '((default :inherit header-line))
  "Face for the image count in the header line of the thumbnail buffer."
  :version "29.1")

;;;; Thumbnail buffer

(defface image-dired-thumb-mark
  '((((class color) (min-colors 16)) :background "DarkOrange")
    (((class color)) :foreground "yellow")
    (default :inherit header-line))
  "Face for marked images in thumbnail buffer."
  :version "29.1")

(defface image-dired-thumb-flagged
  '((((class color) (min-colors 88) (background light)) :background "Red3")
    (((class color) (min-colors 88) (background dark))  :background "Pink")
    (((class color) (min-colors 16) (background light)) :background "Red3")
    (((class color) (min-colors 16) (background dark))  :background "Pink")
    (((class color) (min-colors 8)) :background "red")
    (t :inverse-video t))
  "Face for images flagged for deletion in thumbnail buffer."
  :version "29.1")


;;; Util functions

(defun image-dired--file-name-regexp ()
  (let ((image-file-name-extensions
         (append '("pdf") image-file-name-extensions)))
    (image-file-name-regexp)))

(defun image-dired-insert-image (file type relief margin)
  "Insert at point image FILE of image TYPE, using RELIEF and MARGIN."
  (let ((i `(image :type ,type
                   :file ,file
                   :relief ,relief
                   :margin ,margin)))
    (insert-image i)))

(defun image-dired--get-create-thumbnail-file (file)
  "Return the image descriptor for a thumbnail of image file FILE."
  (unless (string-match-p (image-dired--file-name-regexp) file)
    (error "%s is not a valid image file" file))
  (let* ((thumb-file (image-dired-thumb-name file))
         (thumb-attr (file-attributes thumb-file)))
    (if (or (not thumb-attr)
            (time-less-p (file-attribute-modification-time thumb-attr)
                         (file-attribute-modification-time
                          (file-attributes file))))
        (image-dired-create-thumb file thumb-file)
      (image-dired-debug "Found thumb for %s: %s"
                         (file-name-nondirectory file)
                         (file-name-nondirectory thumb-file)))
    thumb-file))

(defun image-dired-insert-thumbnail ( file original-file-name
                           associated-dired-buffer image-number)
  "Insert thumbnail image FILE.
Add text properties ORIGINAL-FILE-NAME, ASSOCIATED-DIRED-BUFFER
and IMAGE-NUMBER."
  (let (beg end)
    (setq beg (point))
    (image-dired-insert-image
     file
     ;; Thumbnails are created asynchronously, so we might not yet
     ;; have a file.  But if it exists, it might have been cached from
     ;; before and we should use it instead of our current settings.
     (or (and (file-exists-p file)
              (image-type-from-file-header file))
         (and (memq image-dired-thumbnail-storage
                    image-dired--thumbnail-standard-sizes)
              'png)
         'jpeg)
     image-dired-thumb-relief
     image-dired-thumb-margin)
    (setq end (point))
    (add-text-properties
     beg end
     (list 'image-dired-thumbnail t
           ;; Disable `image-map' on thumbnails.
           'keymap nil
           'original-file-name original-file-name
           'associated-dired-buffer associated-dired-buffer
           'image-number image-number
           'tags (image-dired-list-tags original-file-name)
           'mouse-face 'highlight
           'comment (image-dired-get-comment original-file-name)))))

(defmacro image-dired--with-marked (&rest body)
  "Eval BODY with point on each marked thumbnail.
If no marked file could be found, execute BODY on the current
thumbnail.  It's expected that a thumbnail is always followed
by exactly one space or one newline character."
  `(with-current-buffer image-dired-thumbnail-buffer
     (let (found)
       (save-mark-and-excursion
         (goto-char (point-min))
         (while (not (eobp))
           (when (image-dired-thumb-file-marked-p)
             (setq found t)
             ,@body)
           (forward-char 2)))
       (unless found
         ,@body))))

(defun image-dired-create-thumbnail-buffer ()
  "Create thumb buffer and set `image-dired-thumbnail-mode'."
  (let ((buf (get-buffer-create image-dired-thumbnail-buffer)))
    (with-current-buffer buf
      (setq buffer-read-only t)
      (if (not (eq major-mode 'image-dired-thumbnail-mode))
          (image-dired-thumbnail-mode)))
    buf))

(defvar image-dired-saved-window-configuration nil
  "Saved window configuration.")


;;; Starting Image-Dired

;;;###autoload
(defun image-dired-dired-with-window-configuration (dir &optional arg)
  "Open directory DIR and create a default window configuration.

Convenience command that:

 - opens Dired in folder DIR;
 - splits windows in most useful (?) way; and
 - sets `truncate-lines' to t

After the command has finished, you would typically mark some
image files in Dired and type
\\[image-dired-display-thumbs] (`image-dired-display-thumbs').

If called with prefix argument ARG, skip splitting of windows.

The current window configuration is saved and can be restored by
calling `image-dired-restore-window-configuration'."
  (interactive "DDirectory: \nP")
  (let ((buf (image-dired-create-thumbnail-buffer))
        (buf2 (get-buffer-create image-dired-display-image-buffer)))
    (setq image-dired-saved-window-configuration
          (current-window-configuration))
    (dired dir)
    (delete-other-windows)
    (when (not arg)
      (split-window-right)
      (setq truncate-lines t)
      (save-excursion
        (other-window 1)
        (pop-to-buffer-same-window buf)
        (select-window (split-window-below))
        (pop-to-buffer-same-window buf2)
        (other-window -2)))))

(defun image-dired-restore-window-configuration ()
  "Restore window configuration altered by Image-Dired.
Restore any changes to the window configuration made by calling
`image-dired-dired-with-window-configuration'."
  (interactive nil image-dired-thumbnail-mode)
  (if image-dired-saved-window-configuration
      (set-window-configuration image-dired-saved-window-configuration)
    (message "No saved window configuration")))

(defun image-dired--line-up-with-method ()
  "Line up thumbnails according to `image-dired-line-up-method'."
  (cond ((eq 'dynamic image-dired-line-up-method)
         (image-dired-line-up-dynamic))
        ((eq 'fixed image-dired-line-up-method)
         (image-dired-line-up))
        ((eq 'interactive image-dired-line-up-method)
         (image-dired-line-up-interactive))
        ((eq 'none image-dired-line-up-method)
         nil)
        (t
         (image-dired-line-up-dynamic))))

(defvar-local image-dired--number-of-thumbnails 0)

;;;###autoload
(defun image-dired-display-thumbs (&optional arg append do-not-pop)
  "Display thumbnails of all marked files, in `image-dired-thumbnail-buffer'.
If a thumbnail image does not exist for a file, it is created on the
fly.  With prefix argument ARG, display only thumbnail for file at
point (this is useful if you have marked some files but want to show
another one).

Recommended usage is to split the current frame horizontally so that
you have the Dired buffer in the left window and the
`image-dired-thumbnail-buffer' buffer in the right window.

With optional argument APPEND, append thumbnail to thumbnail buffer
instead of erasing it first.

Optional argument DO-NOT-POP controls if `pop-to-buffer' should be
used or not.  If non-nil, use `display-buffer' instead of
`pop-to-buffer'.  This is used from functions like
`image-dired-next-line-and-display' and
`image-dired-previous-line-and-display' where we do not want the
thumbnail buffer to be selected."
  (interactive "P" nil dired-mode)
  (setq image-dired--generate-thumbs-start  (current-time))
  (let ((buf (image-dired-create-thumbnail-buffer))
        files dired-buf)
    (if arg
        (setq files (list (dired-get-filename)))
      (setq files (dired-get-marked-files)))
    (setq dired-buf (current-buffer))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (if (not append)
            (progn
              (setq image-dired--number-of-thumbnails 0)
              (erase-buffer))
          (goto-char (point-max)))
        (dolist (file files)
          (when (string-match-p (image-dired--file-name-regexp) file)
            (image-dired-insert-thumbnail
             (image-dired--get-create-thumbnail-file file) file dired-buf
             (cl-incf image-dired--number-of-thumbnails)))))
      (if (> image-dired--number-of-thumbnails 0)
          (if do-not-pop
              (display-buffer buf)
            (pop-to-buffer buf))
        (message "No images selected"))
      (image-dired--line-up-with-method)
      (image-dired--update-header-line))))

;;;###autoload
(defun image-dired-show-all-from-dir (dir)
  "Make a thumbnail buffer for all images in DIR and display it.
Any file matching `image-dired--file-name-regexp' is considered an
image file.

If the number of image files in DIR exceeds
`image-dired-show-all-from-dir-max-files', ask for confirmation
before creating the thumbnail buffer.  If that variable is nil,
never ask for confirmation."
  (interactive "DShow thumbnails for directory: ")
  (dired dir)
  (dired-mark-files-regexp (image-dired--file-name-regexp))
  (let ((files (dired-get-marked-files nil nil nil t)))
    (cond ((and (null (cdr files)))
           (message "No image files in directory"))
          ((or (not image-dired-show-all-from-dir-max-files)
               (<= (length (cdr files)) image-dired-show-all-from-dir-max-files)
               (and (> (length (cdr files)) image-dired-show-all-from-dir-max-files)
                    (y-or-n-p
                     (format
                      "Directory contains more than %d image files.  Proceed?"
                      image-dired-show-all-from-dir-max-files))))
           (image-dired-display-thumbs)
           (let ((inhibit-message t))
             (dired-unmark-all-marks))
           (pop-to-buffer image-dired-thumbnail-buffer)
           (setq default-directory dir)
           (image-dired--update-header-line))
          (t (message "Image-Dired canceled")))))

;;;###autoload
(defalias 'image-dired 'image-dired-show-all-from-dir)


;;; Movement tracking

(defun image-dired-track-original-file ()
  "Track in the associated Dired buffer the file that corresponds to thumbnail.
See `image-dired-toggle-movement-tracking'.  Interactive use is
only useful if `image-dired-track-movement' is nil."
  (interactive nil image-dired-thumbnail-mode image-dired-image-mode)
  (let ((file-name (image-dired-original-file-name)))
    (image-dired--with-dired-buffer
      (if (not (dired-goto-file file-name))
          (message "Could not find image in Dired buffer for tracking")
        (when-let (window (image-dired-get-buffer-window (current-buffer)))
          (set-window-point window (point)))))))

(defun image-dired-toggle-movement-tracking ()
  "Turn on and off `image-dired-track-movement'.
Tracking of the movements between thumbnail and Dired buffer so that
the movements are \"mirrored\" in the Dired buffer.  When this is on,
moving around in the thumbnail or Dired buffer will move to the matching
position in the other buffer."
  (interactive nil image-dired-thumbnail-mode image-dired-image-mode)
  (setq image-dired-track-movement (not image-dired-track-movement))
  (message "Movement tracking %s" (if image-dired-track-movement "on" "off")))


;;; Navigation

(defun image-dired-forward-image (&optional arg wrap-around)
  "Move to next image in the thumbnail buffer.
Optional prefix ARG says how many images to move; the default is
one image.  Negative means move backwards.
On reaching end or beginning of buffer, stop and show a message.

If optional argument WRAP-AROUND is non-nil, wrap around: if
point is on the last image, move to the last one and vice versa."
  (interactive "p" image-dired-thumbnail-mode)
  (setq arg (or arg 1))
  (let (pos)
    (dotimes (_ (abs arg))
      (if (and (not (if (> arg 0) (eobp) (bobp)))
               (save-excursion
                 (forward-char (if (> arg 0) 1 -1))
                 (while (and (not (if (> arg 0) (eobp) (bobp)))
                             (not (image-dired-image-at-point-p)))
                   (forward-char (if (> arg 0) 1 -1)))
                 (setq pos (point))
                 (image-dired-image-at-point-p)))
          (goto-char pos)
        (if wrap-around
            (goto-char (if (> arg 0)
                           (point-min)
                         ;; There are two spaces after the last image.
                         (- (point-max) 2)))
          (message "At %s image" (if (> arg 0) "last" "first"))))))
  (image-dired--update-header-line)
  (when image-dired-track-movement
    (image-dired-track-original-file)))

(defun image-dired-backward-image (&optional arg)
  "Move to previous image in the thumbnail buffer.
Optional prefix ARG says how many images to move; the default is
one image.  Negative means move forward.
On reaching end or beginning of buffer, stop and show a message."
  (interactive "p" image-dired-thumbnail-mode)
  (image-dired-forward-image (- (or arg 1))))

(defun image-dired--movement-ensure-point-pos (&optional reverse)
  "Ensure point is on an image."
  (while (and (not (image-at-point-p))
              (not (if reverse (bobp) (eobp))))
    (forward-char (if reverse -1 1))))

(defmacro image-dired--movement-command (to &optional reverse)
  `(progn
     (goto-char ,to)
     (image-dired--movement-ensure-point-pos ,reverse)
     (when image-dired-track-movement
       (image-dired-track-original-file))
     (image-dired--update-header-line)))

(defmacro image-dired--movement-command-line (&optional reverse)
  `(image-dired--movement-command
     (let ((goal-column (current-column)))
       (forward-line ,(if reverse -1 1))
       (move-to-column goal-column)
       (point))
     ,reverse))

(defun image-dired-next-line ()
  "Move to next line in the thumbnail buffer."
  (interactive nil image-dired-thumbnail-mode)
  (image-dired--movement-command-line))

(defun image-dired-previous-line ()
  "Move to previous line in the thumbnail buffer."
  (interactive nil image-dired-thumbnail-mode)
  (image-dired--movement-command-line 'reverse))

(defun image-dired-beginning-of-buffer ()
  "Move to the first image in the thumbnail buffer."
  (interactive nil image-dired-thumbnail-mode)
  (image-dired--movement-command (point-min)))

(defun image-dired-end-of-buffer ()
  "Move to the last image in the thumbnail buffer."
  (interactive nil image-dired-thumbnail-mode)
  (image-dired--movement-command (point-max) 'reverse))

(defun image-dired-move-beginning-of-line ()
  "Move to the beginning of current line in thumbnail buffer."
  (interactive nil image-dired-thumbnail-mode)
  (image-dired--movement-command (pos-bol)))

(defun image-dired-move-end-of-line ()
  "Move to the end of current line in thumbnail buffer."
  (interactive nil image-dired-thumbnail-mode)
  (image-dired--movement-command (pos-eol) 'reverse))


;;; Header line

(defun image-dired-format-properties-string (buf file image-count props comment)
  "Format display properties for Image-Dired.
The properties are formatted according to specification
in `image-dired-display-properties-format', which see.
BUF is the associated Dired buffer, FILE is the original image
file name, IMAGE-COUNT is a string like \"N/M\" where N is the
number of this image and M is the total number of images, PROPS
is a stringified list of tags, and COMMENT is the image file's
comment."
  (format-spec
   image-dired-display-properties-format
   `((?b . ,(or buf ""))
     (?d . ,(propertize
             (file-name-nondirectory
              (directory-file-name
               (file-name-directory file)))
             'face 'image-dired-thumb-header-directory-name))
     (?f . ,(propertize (file-name-nondirectory file)
                        'face 'image-dired-thumb-header-file-name))
     (?n . ,(propertize image-count
                        'face 'image-dired-thumb-header-image-count))
     (?s . ,(propertize (if (file-exists-p file)
                            (file-size-human-readable
                             (file-attribute-size
                              (file-attributes file)))
                          "<File missing>")
                        'face 'image-dired-thumb-header-file-size))
     (?t . ,(or props ""))
     (?c . ,(or comment "")))))

(defun image-dired--update-header-line ()
  "Update image information in the header line."
  (when (derived-mode-p 'image-dired-thumbnail-mode)
    (let ((file-name (image-dired-original-file-name))
          (dired-buf (buffer-name (image-dired-associated-dired-buffer)))
          (image-count (format "%s/%s"
                               (get-text-property (point) 'image-number)
                               image-dired--number-of-thumbnails))
          (props (string-join (get-text-property (point) 'tags) ", "))
          (comment (get-text-property (point) 'comment))
          (message-log-max nil))
      (when file-name
        (setq header-line-format
              (image-dired-format-properties-string
               dired-buf
               file-name
               image-count
               props
               comment))))))


;;; Marking and flagging

(defun image-dired-dired-file-marked-p (&optional marker)
  "In Dired, return t if file on current line is marked.
If optional argument MARKER is non-nil, it is a character to look
for.  The default is to look for `dired-marker-char'."
  (setq marker (or marker dired-marker-char))
  (save-excursion
    (beginning-of-line)
    (and (looking-at dired-re-mark)
         (= (aref (match-string 0) 0) marker))))

(defun image-dired-dired-file-flagged-p ()
  "In Dired, return t if file on current line is flagged for deletion."
  (image-dired-dired-file-marked-p dired-del-marker))

(defmacro image-dired--on-file-in-dired-buffer (&rest body)
  "Run BODY in associated Dired buffer with point on current file's line.
Should be called from commands in `image-dired-thumbnail-mode'."
  (declare (indent defun) (debug t))
  `(if-let ((file-name (image-dired-original-file-name)))
       (image-dired--with-dired-buffer
         (when (dired-goto-file file-name)
           ,@body))
     (message "No image with correct properties at point")))

(defmacro image-dired--with-thumbnail-buffer (&rest body)
  (declare (indent defun) (debug t))
  `(if-let ((buf (get-buffer image-dired-thumbnail-buffer)))
       (with-current-buffer buf
         (if-let ((win (get-buffer-window buf)))
             (with-selected-window win
               ,@body)
           ,@body))
     (user-error "No such buffer: %s" image-dired-thumbnail-buffer)))

(defmacro image-dired--do-mark-command (maybe-next update-mark &rest body)
  "Run BODY in Dired buffer.
Helper macro for the mark, unmark and flag commands.

If MAYBE-NEXT is non-nil, show next image according to
`image-dired-marking-shows-next'.

If UPDATE-MARK is non-nil, also update the mark in the thumbnail
buffer with `image-dired--thumb-update-mark-at-point'."
  (declare (indent defun) (debug t))
  `(image-dired--with-thumbnail-buffer
     (image-dired--on-file-in-dired-buffer
       ,@body)
     ,(when update-mark
        '(image-dired--thumb-update-mark-at-point))
     ,(when maybe-next
        '(if image-dired-marking-shows-next
             (image-dired-display-next)
           (image-dired-forward-image)))))

(defun image-dired-mark-thumb-original-file ()
  "Mark original image file in associated Dired buffer."
  (interactive nil image-dired-thumbnail-mode image-dired-image-mode)
  (image-dired--do-mark-command t t
    (dired-mark 1)))

(defun image-dired-unmark-thumb-original-file ()
  "Unmark original image file in associated Dired buffer."
  (interactive nil image-dired-thumbnail-mode image-dired-image-mode)
  (image-dired--do-mark-command t t
    (dired-unmark 1)))

(defun image-dired-flag-thumb-original-file ()
  "Flag original image file for deletion in associated Dired buffer."
  (interactive nil image-dired-thumbnail-mode image-dired-image-mode)
  (image-dired--do-mark-command t t
    (dired-flag-file-deletion 1)))

(defun image-dired-unmark-all-marks ()
  "Remove all marks from all files in associated Dired buffer.
Also update the marks in the thumbnail buffer."
  (interactive nil image-dired-thumbnail-mode image-dired-image-mode)
  (image-dired--do-mark-command nil t
    (dired-unmark-all-marks))
  (image-dired--with-thumbnail-buffer
    (image-dired--thumb-update-marks)))

(defun image-dired-jump-original-dired-buffer ()
  "Jump to the Dired buffer associated with the current image file.
You probably want to use this together with
`image-dired-track-original-file'."
  (interactive nil image-dired-thumbnail-mode)
  (image-dired--with-dired-buffer
    (if-let ((window (image-dired-get-buffer-window (current-buffer))))
        (progn
          (if (not (equal (selected-frame) (window-frame window)))
              (select-frame-set-input-focus (window-frame window)))
          (select-window window))
      (message "Associated Dired buffer not visible"))))


;;; Major modes

(defvar-keymap image-dired-thumbnail-mode-map
  :doc "Keymap for `image-dired-thumbnail-mode'."
  "d"          #'image-dired-flag-thumb-original-file
  "<delete>"   #'image-dired-flag-thumb-original-file
  "m"          #'image-dired-mark-thumb-original-file
  "u"          #'image-dired-unmark-thumb-original-file
  "U"          #'image-dired-unmark-all-marks
  "x"          #'image-dired-do-flagged-delete
  "."          #'image-dired-track-original-file
  "<tab>"      #'image-dired-jump-original-dired-buffer

  "g g"        #'image-dired-line-up-dynamic
  "g f"        #'image-dired-line-up
  "g i"        #'image-dired-line-up-interactive

  "t t"        #'image-dired-tag-thumbnail
  "t r"        #'image-dired-tag-thumbnail-remove

  "RET"        #'image-dired-display-this
  "C-<return>" #'image-dired-thumbnail-display-external

  "L"          #'image-dired-rotate-original-left
  "R"          #'image-dired-rotate-original-right

  "D"          #'image-dired-thumbnail-set-image-description
  "S"          #'image-dired-slideshow-start
  "C-d"        #'image-dired-delete-char
  "SPC"        #'image-dired-display-next
  "DEL"        #'image-dired-display-previous
  "c"          #'image-dired-comment-thumbnail
  "w"          #'image-dired-copy-filename-as-kill
  "W"          #'image-dired-wallpaper-set

  ;; Mouse
  "<mouse-2>"        #'image-dired-mouse-display-image
  "<double-mouse-1>" #'image-dired-mouse-display-image
  "<mouse-1>"        #'image-dired-mouse-select-thumbnail
  "<mouse-3>"        #'image-dired-mouse-select-thumbnail
  "<down-mouse-1>"   #'image-dired-mouse-select-thumbnail
  "<down-mouse-2>"   #'image-dired-mouse-select-thumbnail
  "<down-mouse-3>"   #'image-dired-mouse-select-thumbnail
  "C-<down-mouse-1>" #'ignore           ; Don't open the buffer menu.
  "C-<mouse-1>"      #'image-dired-mouse-toggle-mark

  "<remap> <forward-char>"           #'image-dired-forward-image
  "<remap> <backward-char>"          #'image-dired-backward-image
  "<remap> <next-line>"              #'image-dired-next-line
  "<remap> <previous-line>"          #'image-dired-previous-line
  "<remap> <left-char>"              #'image-dired-backward-image
  "<remap> <right-char>"             #'image-dired-forward-image
  "<remap> <beginning-of-buffer>"    #'image-dired-beginning-of-buffer
  "<remap> <end-of-buffer>"          #'image-dired-end-of-buffer
  "<remap> <move-beginning-of-line>" #'image-dired-move-beginning-of-line
  "<remap> <move-end-of-line>"       #'image-dired-move-end-of-line

  :menu
  '("Image-Dired"
    ["Display image" image-dired-display-this]
    ["Display in external viewer" image-dired-thumbnail-display-external]
    ["Jump to Dired buffer" image-dired-jump-original-dired-buffer]
    "---"
    ["Mark image" image-dired-mark-thumb-original-file]
    ["Unmark image" image-dired-unmark-thumb-original-file]
    ["Unmark all images" image-dired-unmark-all-marks]
    ["Flag for deletion" image-dired-flag-thumb-original-file]
    ["Delete flagged images" image-dired-do-flagged-delete]
    "---"
    ["Rotate original right" image-dired-rotate-original-right]
    ["Rotate original left" image-dired-rotate-original-left]
    "---"
    ["Comment thumbnail" image-dired-comment-thumbnail]
    ["Tag current or marked thumbnails" image-dired-tag-thumbnail]
    ["Remove tag from current or marked thumbnails"
     image-dired-tag-thumbnail-remove]
    ["Start slideshow" image-dired-slideshow-start]
    "---"
    ("View Options"
     ["Toggle movement tracking" image-dired-toggle-movement-tracking
      :style toggle
      :selected image-dired-track-movement]
     "---"
     ["Line up thumbnails" image-dired-line-up]
     ["Dynamic line up" image-dired-line-up-dynamic]
     ["Refresh thumb" image-dired-refresh-thumb])
    ["Quit" quit-window]))

(define-derived-mode image-dired-thumbnail-mode
  special-mode "image-dired-thumbnail"
  "Browse and manipulate thumbnail images using Dired.
Use `image-dired-minor-mode' to get a nice setup."
  :interactive nil
  :group 'image-dired
  (buffer-disable-undo)
  (add-hook 'file-name-at-point-functions 'image-dired-file-name-at-point nil t)
  (setq-local window-resize-pixelwise t)
  (setq-local bookmark-make-record-function #'image-dired-bookmark-make-record)
  ;; Use approximately as much vertical spacing as horizontal.
  (setq-local line-spacing (frame-char-width)))


;;; image-dired-image-mode

(define-obsolete-variable-alias 'image-dired-display-image-mode-map
  'image-dired-image-mode-map "29.1")
(defvar-keymap image-dired-image-mode-map
  :doc "Keymap for `image-dired-image-mode'."
  "S"   #'image-dired-slideshow-start
  "SPC" #'image-dired-display-next
  "DEL" #'image-dired-display-previous
  "n"   #'image-dired-display-next
  "p"   #'image-dired-display-previous
  "m"   #'image-dired-mark-thumb-original-file
  "d"   #'image-dired-flag-thumb-original-file
  "u"   #'image-dired-unmark-thumb-original-file
  "U"   #'image-dired-unmark-all-marks
  ;; Disable keybindings from `image-mode-map' that doesn't make sense here.
  "o" nil)   ; image-save

(define-derived-mode image-dired-image-mode
  image-mode "image-dired-image-display"
  "Mode for displaying and manipulating original image.
Resized or in full-size."
  :interactive nil
  :group 'image-dired
  (setq-local column-number-mode nil)
  (setq-local line-number-mode nil)
  (add-hook 'file-name-at-point-functions #'image-dired-file-name-at-point nil t))


;;; Slideshow

(defcustom image-dired-slideshow-delay 5.0
  "Seconds to wait before showing the next image in a slideshow.
This is used by `image-dired-slideshow-start'."
  :type 'float
  :version "29.1")

(define-obsolete-variable-alias 'image-dired-slideshow-timer
  'image-dired--slideshow-timer "29.1")
(defvar image-dired--slideshow-timer nil
  "Slideshow timer.")

(defvar image-dired--slideshow-current-delay image-dired-slideshow-delay)

(defun image-dired--slideshow-step ()
  "Step to the next image in a slideshow."
  (if-let ((buf (get-buffer image-dired-thumbnail-buffer)))
      (with-current-buffer buf
        (image-dired-display-next))
    (image-dired--slideshow-stop)))

(defun image-dired--slideshow-start-timer ()
  (image-dired--slideshow-stop-timer)
  (setq image-dired--slideshow-timer
        (run-with-timer image-dired--slideshow-current-delay
                        image-dired--slideshow-current-delay
                        'image-dired--slideshow-step)))

(defun image-dired--slideshow-stop-timer ()
  (when image-dired--slideshow-timer
    (cancel-timer image-dired--slideshow-timer)
    (setq image-dired--slideshow-timer nil)))

(defun image-dired-slideshow-start (&optional arg)
  "Start a slideshow, waiting `image-dired-slideshow-delay' seconds between images.

With prefix argument ARG, wait that many seconds before going to
the next image.

With a negative prefix argument, prompt user for the delay."
  (interactive "P" image-dired-thumbnail-mode image-dired-image-mode)
  (let ((delay
         (cond ((not arg)
                image-dired-slideshow-delay)
               ((> arg 0)
                arg)
               ((<= arg 0)
                (string-to-number
                 (let ((delay (number-to-string image-dired-slideshow-delay)))
                   (read-string
                    (format-prompt "Delay, in seconds.  Decimals are accepted"
                                   delay))
                   delay))))))
    (image-dired-display-this)
    (setq image-dired--slideshow-current-delay delay)
    (add-hook 'post-command-hook 'image-dired--slideshow-stop)))

(defun image-dired--slideshow-show-message (&optional suffix)
  "Helper function for `image-dired--slideshow-stop'."
  (message (substitute-command-keys
            (format
             (concat
              "\\[image-dired-display-next] next, "
              "\\[image-dired-display-previous] previous, "
              "\\[image-dired-display-this] pause/unpause, "
              "any other command to stop%s")
             (or suffix "")))))

(defun image-dired--slideshow-stop ()
  "Cancel the currently active slideshow."
  (cond
   ((memq this-command
          '( image-dired-slideshow-start
             image-dired-display-next
             image-dired-display-previous))
    (image-dired--slideshow-start-timer)
    (image-dired--slideshow-show-message))
   ((eq this-command 'image-dired-display-this)
    (let ((pause image-dired--slideshow-timer))
      (if pause
          (image-dired--slideshow-stop-timer)
        (image-dired--slideshow-start-timer))
      (image-dired--slideshow-show-message (and pause "  [PAUSED]"))))
   (t
    (image-dired--slideshow-stop-timer)
    (remove-hook 'post-command-hook 'image-dired--slideshow-stop))))


;;; Thumbnail layout and display

(defun image-dired-delete-char ()
  "Remove current thumbnail from thumbnail buffer and line up."
  (interactive nil image-dired-thumbnail-mode)
  (let ((inhibit-read-only t))
    (delete-char 1))
  (let ((pos (point)))
    (image-dired--line-up-with-method)
    (goto-char pos)))

(defun image-dired-line-up ()
  "Line up thumbnails according to `image-dired-thumbs-per-row'.
See also `image-dired-line-up-dynamic'."
  (interactive nil image-dired-thumbnail-mode)
  (let ((inhibit-read-only t))
    (goto-char (point-min))
    (while (and (not (image-dired-image-at-point-p))
                (not (eobp)))
      (delete-char 1))
    (while (not (eobp))
      (forward-char)
      (while (and (not (image-dired-image-at-point-p))
                  (not (eobp)))
        (delete-char 1)))
    (goto-char (point-min))
    (let ((seen 0)
          (thumb-prev-pos 0)
          (thumb-width-chars
           (ceiling (/ (+ (* 2 image-dired-thumb-relief)
                          (* 2 image-dired-thumb-margin)
                          (image-dired--thumb-size))
                       (float (frame-char-width))))))
      (while (not (eobp))
        (forward-char)
        (if (= image-dired-thumbs-per-row 1)
            (insert "\n")
          (cl-incf thumb-prev-pos thumb-width-chars)
          (insert (propertize " " 'display `(space :align-to ,thumb-prev-pos)))
          (cl-incf seen)
          (when (and (= seen (- image-dired-thumbs-per-row 1))
                     (not (eobp)))
            (forward-char)
            (insert "\n")
            (setq seen 0)
            (setq thumb-prev-pos 0)))))
    (goto-char (point-min))))

(defun image-dired-line-up-dynamic ()
  "Line up thumbnails images dynamically.
Calculate how many thumbnails fit."
  (interactive nil image-dired-thumbnail-mode)
  (let* ((char-width (frame-char-width))
         (width (window-body-width (image-dired-thumbnail-window) t))
         (image-dired-thumbs-per-row
          (/ width
             (+ (* 2 image-dired-thumb-relief)
                (* 2 image-dired-thumb-margin)
                (image-dired--thumb-size)
                char-width))))
    (image-dired-line-up)))

(defun image-dired-line-up-interactive ()
  "Line up thumbnails interactively.
Ask user how many thumbnails should be displayed per row."
  (interactive nil image-dired-thumbnail-mode)
  (let ((image-dired-thumbs-per-row
         (string-to-number (read-string "How many thumbs per row: "))))
    (if (not (> image-dired-thumbs-per-row 0))
        (message "Number must be greater than 0")
      (image-dired-line-up))))


;;; Display image from thumbnail buffer

(defun image-dired-thumbnail-display-external ()
  "Display original image for thumbnail at point using external viewer.
The viewer command is specified by `image-dired-external-viewer'."
  (interactive nil image-dired-thumbnail-mode)
  (let ((file (image-dired-original-file-name)))
    (if (not (image-dired-image-at-point-p))
        (message "No thumbnail at point")
      (if (not file)
          (message "No original file name found")
        (apply #'start-process "image-dired-thumb-external" nil
               (append (string-split image-dired-external-viewer " ")
                       (list file)))))))

(defun image-dired-display-image (file &optional _ignored)
  "Display image FILE in the image buffer window.
If FILE is an image, the window will use `image-dired-image-mode'
which is based on `image-mode'."
  (declare (advertised-calling-convention (file) "29.1"))
  (setq file (expand-file-name file))
  (when (not (file-exists-p file))
    (error "No such file: %s" file))
  (let ((buf (get-buffer image-dired-display-image-buffer))
        (cur-win (selected-window)))
    (when buf
      (kill-buffer buf))
    (when-let ((buf (find-file-noselect file nil t)))
      (pop-to-buffer buf)
      (rename-buffer image-dired-display-image-buffer)
      (if (string-match (image-file-name-regexp) file)
          (image-dired-image-mode)
        ;; Support visiting PDF files.
        (normal-mode))
      (select-window cur-win))))

(defun image-dired-display-this (&optional arg)
  "Display current thumbnail's original image in display buffer.
See documentation for `image-dired-display-image' for more information.
With prefix argument ARG, display image in its original size."
  (interactive "P" image-dired-thumbnail-mode)
  (unless (string-equal major-mode "image-dired-thumbnail-mode")
    (user-error "Not in `image-dired-thumbnail-mode'"))
  (let ((file (image-dired-original-file-name)))
    (cond ((not (image-dired-image-at-point-p))
           (message "No thumbnail at point"))
          ((not file)
           (message "No original file name found"))
          (t
           (image-dired-display-image file arg)))))

(defun image-dired-display-next (&optional arg)
  "Move to the next image in the thumbnail buffer and display it.
With prefix ARG, move that many thumbnails."
  (interactive "p" image-dired-thumbnail-mode image-dired-image-mode)
  (image-dired--with-thumbnail-buffer
    (image-dired-forward-image arg t)
    (image-dired-display-this)))

(defun image-dired-display-previous (arg)
  "Move to the previous image in the thumbnail buffer and display it.
With prefix ARG, move that many thumbnails."
  (interactive "p" image-dired-thumbnail-mode image-dired-image-mode)
  (image-dired-display-next (- arg)))


;;; Misc commands

(defun image-dired-rotate-original-left ()
  "Rotate original image left (counter clockwise) 90 degrees.
The result of the rotation is displayed in the image display area
and a confirmation is needed before the original image files is
overwritten.  This confirmation can be turned off using
`image-dired-rotate-original-ask-before-overwrite'."
  (interactive nil image-dired-thumbnail-mode)
  (image-dired--with-marked
   (image-dired-rotate-original "270")))

(defun image-dired-rotate-original-right ()
  "Rotate original image right (clockwise) 90 degrees.
The result of the rotation is displayed in the image display area
and a confirmation is needed before the original image files is
overwritten.  This confirmation can be turned off using
`image-dired-rotate-original-ask-before-overwrite'."
  (interactive nil image-dired-thumbnail-mode)
  (image-dired--with-marked
   (image-dired-rotate-original "90")))

(defun image-dired-wallpaper-set (file)
  "Set the wallpaper to FILE in a graphical environment."
  (interactive (list (image-dired-original-file-name))
               image-dired-thumbnail-mode)
  (wallpaper-set file))

(defun image-dired-comment-thumbnail ()
  "Add comment to current thumbnail in thumbnail buffer."
  (interactive nil image-dired-thumbnail-mode)
  (let* ((file (image-dired-original-file-name))
         (comment (image-dired-read-comment file)))
    (image-dired-write-comments (list (cons file comment)))
    (image-dired-update-property 'comment comment))
  (image-dired--update-header-line))

(defun image-dired-copy-filename-as-kill (&optional arg)
  "Copy names of marked (or next ARG) files into the kill ring.
This works like `dired-copy-filename-as-kill' (which see)."
  (interactive "P" image-dired-thumbnail-mode)
  (image-dired--with-dired-buffer
    (dired-copy-filename-as-kill arg)))


;;; Mouse support

(defun image-dired-mouse-display-image (event)
  "Use mouse EVENT, call `image-dired-display-image' to display image.
Track this in associated Dired buffer if `image-dired-track-movement' is
non-nil."
  (interactive "e")
  (mouse-set-point event)
  (goto-char (posn-point (event-end event)))
  (unless (image-at-point-p)
    (image-dired-backward-image))
  (let ((file (image-dired-original-file-name)))
    (when file
      (if image-dired-track-movement
          (image-dired-track-original-file))
      (image-dired-display-image file))))

(defun image-dired-mouse-select-thumbnail (event)
  "Use mouse EVENT to select thumbnail image.
Track this in associated Dired buffer if `image-dired-track-movement' is
non-nil."
  (interactive "e")
  (mouse-set-point event)
  (goto-char (posn-point (event-end event)))
  (unless (image-at-point-p)
    (image-dired-backward-image))
  (if image-dired-track-movement
      (image-dired-track-original-file))
  (image-dired--update-header-line))



;;; Dired marks and tags

(defun image-dired-thumb-file-marked-p (&optional flagged)
  "Check if file is marked in associated Dired buffer.
If optional argument FLAGGED is non-nil, check if file is flagged
for deletion instead."
  (let ((file-name (image-dired-original-file-name)))
    (image-dired--with-dired-buffer
      (save-excursion
        (when (dired-goto-file file-name)
          (if flagged
              (image-dired-dired-file-flagged-p)
            (image-dired-dired-file-marked-p)))))))

(defun image-dired-thumb-file-flagged-p ()
  "Check if file is flagged for deletion in associated Dired buffer."
  (image-dired-thumb-file-marked-p t))

(defun image-dired-do-flagged-delete ()
  "Delete flagged thumbnails and associated images."
  (interactive nil image-dired-thumbnail-mode)
  (unless (derived-mode-p 'image-dired-thumbnail-mode)
    (user-error "Not in `image-dired-thumbnail-mode'"))
  (image-dired--with-dired-buffer
    (dired-do-flagged-delete))
  (let (deletions)
    (save-excursion
      (let ((inhibit-read-only t))
        (goto-char (point-min))
        (while (not (eobp))
          (let ((file-name (image-dired-original-file-name)))
            (if (image-dired--with-dired-buffer (dired-goto-file file-name))
                (forward-char 2)
              (delete-char 1)
              (forward-char)
              (setq deletions t))))))
    (if deletions
        (image-dired--line-up-with-method))))

(defun image-dired--thumb-update-mark-at-point ()
  (with-silent-modifications
    (cond ((image-dired-thumb-file-marked-p)
           (add-face-text-property (point) (1+ (point))
                                   'image-dired-thumb-mark))
          ((image-dired-thumb-file-flagged-p)
           (add-face-text-property (point) (1+ (point))
                                   'image-dired-thumb-flagged))
          (t (remove-text-properties (point) (1+ (point))
                                     '(face image-dired-thumb-mark))))))

(defun image-dired--thumb-update-marks ()
  "Update the marks in the thumbnail buffer.
It's expected, that a thumbnail is always followed
by exactly one space or one newline character."
  (when image-dired-thumb-visible-marks
    (with-current-buffer image-dired-thumbnail-buffer
      (save-mark-and-excursion
        (goto-char (point-min))
        (let ((inhibit-read-only t))
          (while (not (eobp))
            (image-dired--thumb-update-mark-at-point)
            (forward-char 2)))))))

(defun image-dired-mouse-toggle-mark-1 ()
  "Toggle Dired mark for current thumbnail.
Track this in associated Dired buffer if
`image-dired-track-movement' is non-nil."
  (when image-dired-track-movement
    (image-dired-track-original-file))
  (image-dired--do-mark-command nil nil
    (if (image-dired-dired-file-marked-p)
        (dired-unmark 1)
      (dired-mark 1))))

(defun image-dired-mouse-toggle-mark (event)
  "Use mouse EVENT to toggle Dired mark for thumbnail.
Toggle marks of all thumbnails in region, if it's active.
Track this in associated Dired buffer if
`image-dired-track-movement' is non-nil."
  (interactive "e")
  (if (use-region-p)
      (let ((end (region-end)))
        (save-excursion
          (goto-char (region-beginning))
          (while (<= (point) end)
            (when (image-dired-image-at-point-p)
              (image-dired-mouse-toggle-mark-1))
            (forward-char))))
    (mouse-set-point event)
    (goto-char (posn-point (event-end event)))
    (image-dired-mouse-toggle-mark-1))
  (image-dired--thumb-update-marks))


;;; bookmark.el support

(declare-function bookmark-make-record-default
                  "bookmark" (&optional no-file no-context posn))
(declare-function bookmark-prop-get "bookmark" (bookmark prop))

(defun image-dired-bookmark-name ()
  "Create a default bookmark name for the current EWW buffer."
  (file-name-nondirectory
   (directory-file-name
    (file-name-directory (image-dired-original-file-name)))))

(defun image-dired-bookmark-make-record ()
  "Create a bookmark for the current EWW buffer."
  `(,(image-dired-bookmark-name)
    ,@(bookmark-make-record-default t)
    (location . ,(file-name-directory (image-dired-original-file-name)))
    (image-dired-file . ,(file-name-nondirectory (image-dired-original-file-name)))
    (handler . image-dired-bookmark-jump)))

;;;###autoload
(defun image-dired-bookmark-jump (bookmark)
  "Default bookmark handler for Image-Dired buffers."
  ;; User already cached thumbnails, so disable any checking.
  (let ((image-dired-show-all-from-dir-max-files nil))
    (image-dired (bookmark-prop-get bookmark 'location))
    ;; TODO: Go to the bookmarked file, if it exists.
    ;; (bookmark-prop-get bookmark 'image-dired-file)
    (goto-char (point-min))))

(put 'image-dired-bookmark-jump 'bookmark-handler-type "Image-Dired")


;;; Obsolete

;;;###autoload
(define-obsolete-function-alias 'tumme #'image-dired "24.4")

;;;###autoload
(define-obsolete-function-alias 'image-dired-setup-dired-keybindings
  #'image-dired-minor-mode "26.1")

(make-obsolete-variable 'image-dired-thumb-width
                        'image-dired-thumb-size "29.1")
(defcustom image-dired-thumb-width image-dired-thumb-size
  "Width of thumbnails, in pixels."
  :type 'natnum)

(make-obsolete-variable 'image-dired-thumb-height
                        'image-dired-thumb-size "29.1")
(defcustom image-dired-thumb-height image-dired-thumb-size
  "Height of thumbnails, in pixels."
  :type 'natnum)

(defcustom image-dired-temp-image-file
  (expand-file-name ".image-dired_temp" image-dired-dir)
  "Name of temporary image file used by various commands."
  :type 'file)
(make-obsolete-variable 'image-dired-temp-image-file
                        "no longer used." "29.1")

(defcustom image-dired-cmd-create-temp-image-program
  (if (executable-find "gm") "gm" "convert")
  "Executable used to create temporary image.
Used together with `image-dired-cmd-create-temp-image-options'."
  :type 'file
  :version "29.1")
(make-obsolete-variable 'image-dired-cmd-create-temp-image-program
                        "no longer used." "29.1")

(defcustom image-dired-cmd-create-temp-image-options
  (let ((opts '("-size" "%wx%h" "%f[0]"
                "-resize" "%wx%h>"
                "-strip" "jpeg:%t")))
    (if (executable-find "gm") (cons "convert" opts) opts))
  "Options of command used to create temporary image for display window.
Used together with `image-dired-cmd-create-temp-image-program',
Available format specifiers are: %w and %h which are replaced by
the calculated max size for width and height in the image display window,
%f which is replaced by the file name of the original image and %t which
is replaced by the file name of the temporary file."
  :version "29.1"
  :type '(repeat (string :tag "Argument")))
(make-obsolete-variable 'image-dired-cmd-create-temp-image-options
                        "no longer used." "29.1")

(defcustom image-dired-display-window-width-correction 1
  "Number to be used to correct image display window width.
Change if the default (1) does not work (i.e. if the image does not
completely fit)."
  :type 'integer)
(make-obsolete-variable 'image-dired-display-window-width-correction
                        "no longer used." "29.1")

(defcustom image-dired-display-window-height-correction 0
  "Number to be used to correct image display window height.
Change if the default (0) does not work (i.e. if the image does not
completely fit)."
  :type 'integer)
(make-obsolete-variable 'image-dired-display-window-height-correction
                        "no longer used." "29.1")

(defun image-dired-toggle-mark-thumb-original-file ()
  "Toggle mark on original image file in associated Dired buffer."
  (declare (obsolete nil "29.1"))
  (interactive nil image-dired-thumbnail-mode image-dired-image-mode)
  (image-dired--do-mark-command nil t
    (if (image-dired-dired-file-marked-p)
        (dired-unmark 1)
      (dired-mark 1))))

(defun image-dired-display-window-width (window)
  "Return width, in pixels, of WINDOW."
  (declare (obsolete nil "29.1"))
  (- (window-body-width window t)
     image-dired-display-window-width-correction))

(defun image-dired-display-window-height (window)
  "Return height, in pixels, of WINDOW."
  (declare (obsolete nil "29.1"))
  (- (image-dired-window-height-pixels window)
     image-dired-display-window-height-correction))

(defun image-dired-window-height-pixels (window)
  "Calculate WINDOW height in pixels."
  (declare (obsolete nil "29.1"))
  ;; Note: The mode-line consumes one line
  (* (- (window-height window) 1) (frame-char-height)))

(defcustom image-dired-cmd-read-exif-data-program "exiftool"
  "Program used to read EXIF data to image.
Used together with `image-dired-cmd-read-exif-data-options'."
  :type 'file)
(make-obsolete-variable 'image-dired-cmd-read-exif-data-program
                        "use `exif-parse-file' and `exif-field' instead." "29.1")

(defcustom image-dired-cmd-read-exif-data-options '("-s" "-s" "-s" "-%t" "%f")
  "Arguments of command used to read EXIF data.
Used with `image-dired-cmd-read-exif-data-program'.
Available format specifiers are: %f which is replaced
by the image file name and %t which is replaced by the tag name."
  :version "26.1"
  :type '(repeat (string :tag "Argument")))
(make-obsolete-variable 'image-dired-cmd-read-exif-data-options
                        "use `exif-parse-file' and `exif-field' instead." "29.1")

(defun image-dired-get-exif-data (file tag-name)
  "From FILE, return EXIF tag TAG-NAME."
  (declare (obsolete "use `exif-parse-file' and `exif-field' instead."  "29.1"))
  (image-dired--check-executable-exists
   'image-dired-cmd-read-exif-data-program)
  (let ((buf (get-buffer-create "*image-dired-get-exif-data*"))
        (spec (list (cons ?f file) (cons ?t tag-name)))
        tag-value)
    (with-current-buffer buf
      (delete-region (point-min) (point-max))
      (if (not (eq (apply #'call-process image-dired-cmd-read-exif-data-program
                          nil t nil
                          (mapcar
                           (lambda (arg) (format-spec arg spec))
                           image-dired-cmd-read-exif-data-options))
                   0))
          (error "Could not get EXIF tag")
        (goto-char (point-min))
        ;; Clean buffer from newlines and carriage returns before
        ;; getting final info
        (while (search-forward-regexp "[\n\r]" nil t)
          (replace-match "" nil t))
        (setq tag-value (buffer-substring (point-min) (point-max)))))
    tag-value))

(defcustom image-dired-cmd-rotate-thumbnail-program
  (if (executable-find "gm") "gm" "mogrify")
  "Executable used to rotate thumbnail.
Used together with `image-dired-cmd-rotate-thumbnail-options'."
  :type 'file
  :version "29.1")
(make-obsolete-variable 'image-dired-cmd-rotate-thumbnail-program nil "29.1")

(defcustom image-dired-cmd-rotate-thumbnail-options
  (let ((opts '("-rotate" "%d" "%t")))
    (if (executable-find "gm") (cons "mogrify" opts) opts))
  "Arguments of command used to rotate thumbnail image.
Used with `image-dired-cmd-rotate-thumbnail-program'.
Available format specifiers are: %d which is replaced by the
number of (positive) degrees to rotate the image, normally 90 or 270
\(for 90 degrees right and left), %t which is replaced by the file name
of the thumbnail file."
  :version "29.1"
  :type '(repeat (string :tag "Argument")))
(make-obsolete-variable 'image-dired-cmd-rotate-thumbnail-options nil "29.1")

(declare-function clear-image-cache "image.c" (&optional filter))

(defun image-dired-rotate-thumbnail (degrees)
  "Rotate thumbnail DEGREES degrees."
  (declare (obsolete image-dired-refresh-thumb "29.1"))
  (image-dired--check-executable-exists
   'image-dired-cmd-rotate-thumbnail-program)
  (if (not (image-dired-image-at-point-p))
      (message "No thumbnail at point")
    (let* ((file (image-dired-thumb-name (image-dired-original-file-name)))
           (thumb (expand-file-name file))
           (spec (list (cons ?d degrees) (cons ?t thumb))))
      (apply #'call-process image-dired-cmd-rotate-thumbnail-program nil nil nil
             (mapcar (lambda (arg) (format-spec arg spec))
                     image-dired-cmd-rotate-thumbnail-options))
      (clear-image-cache thumb))))

(defun image-dired-rotate-thumbnail-left ()
  "Rotate thumbnail left (counter clockwise) 90 degrees."
  (declare (obsolete image-dired-refresh-thumb "29.1"))
  (interactive nil image-dired-thumbnail-mode)
  (with-suppressed-warnings ((obsolete image-dired-rotate-thumbnail))
    (image-dired-rotate-thumbnail "270")))

(defun image-dired-rotate-thumbnail-right ()
  "Rotate thumbnail counter right (clockwise) 90 degrees."
  (declare (obsolete image-dired-refresh-thumb "29.1"))
  (interactive nil image-dired-thumbnail-mode)
  (with-suppressed-warnings ((obsolete image-dired-rotate-thumbnail))
    (image-dired-rotate-thumbnail "90")))

(defun image-dired-modify-mark-on-thumb-original-file (command)
  "Modify mark in Dired buffer.
COMMAND is one of `mark' for marking file in Dired, `unmark' for
unmarking file in Dired or `flag' for flagging file for delete in
Dired."
  (declare (obsolete image-dired--on-file-in-dired-buffer "29.1"))
  (let ((file-name (image-dired-original-file-name))
        (dired-buf (image-dired-associated-dired-buffer)))
    (if (not (and dired-buf file-name))
        (message "No image, or image with correct properties, at point")
      (with-current-buffer dired-buf
        (message "%s" file-name)
        (when (dired-goto-file file-name)
          (cond ((eq command 'mark) (dired-mark 1))
                ((eq command 'unmark) (dired-unmark 1))
                ((eq command 'toggle)
                 (if (image-dired-dired-file-marked-p)
                     (dired-unmark 1)
                   (dired-mark 1)))
                ((eq command 'flag) (dired-flag-file-deletion 1)))
          (image-dired--thumb-update-marks))))))

(defun image-dired-display-current-image-full ()
  "Display current image in full size."
  (declare (obsolete image-transform-reset-to-original "29.1"))
  (interactive nil image-dired-thumbnail-mode)
  (let ((file (image-dired-original-file-name)))
    (if file
        (progn
          (image-dired-display-image file)
          (with-current-buffer image-dired-display-image-buffer
            (image-transform-reset-to-original)))
      (error "No original file name at point"))))

(defun image-dired-display-current-image-sized ()
  "Display current image in sized to fit window dimensions."
  (declare (obsolete image-mode-fit-frame "29.1"))
  (interactive nil image-dired-thumbnail-mode)
  (let ((file (image-dired-original-file-name)))
    (if file
        (progn
          (image-dired-display-image file))
      (error "No original file name at point"))))

(make-obsolete-variable 'image-dired-tag-file-list nil "29.1")
(defvar image-dired-tag-file-list nil
  "List to store tag-file structure.")

(defun image-dired-add-to-tag-file-list (tag file)
  "Add relation between TAG and FILE."
  (declare (obsolete nil "29.1"))
  (let (curr)
    (if image-dired-tag-file-list
        (if (setq curr (assoc tag image-dired-tag-file-list))
            (if (not (member file curr))
                (setcdr curr (cons file (cdr curr))))
          (setcdr image-dired-tag-file-list
                  (cons (list tag file) (cdr image-dired-tag-file-list))))
      (setq image-dired-tag-file-list (list (list tag file))))))

(defvar image-dired-slideshow-count 0
  "Keeping track on number of images in slideshow.")
(make-obsolete-variable 'image-dired-slideshow-count "no longer used." "29.1")

(defvar image-dired-slideshow-times 0
  "Number of pictures to display in slideshow.")
(make-obsolete-variable 'image-dired-slideshow-times "no longer used." "29.1")

(make-obsolete-variable 'image-dired-gallery-dir nil "29.1")
(defcustom image-dired-gallery-dir
  (expand-file-name ".image-dired_gallery" image-dired-dir)
  "Directory to store generated gallery html pages.
The name of this directory needs to be \"shared\" to the public
so that it can access the index.html page that image-dired creates."
  :type 'directory)

(make-obsolete-variable 'image-dired-gallery-image-root-url nil "29.1")
(defcustom image-dired-gallery-image-root-url
  "https://example.org/image-diredpics"
  "URL where the full size images are to be found on your web server.
Note that this URL has to be configured on your web server.
Image-Dired expects to find pictures in this directory.
This is used by `image-dired-gallery-generate'."
  :type 'string
  :version "29.1")

(make-obsolete-variable 'image-dired-gallery-thumb-image-root-url nil "29.1")
(defcustom image-dired-gallery-thumb-image-root-url
  "https://example.org/image-diredthumbs"
  "URL where the thumbnail images are to be found on your web server.
Note that URL path has to be configured on your web server.
Image-Dired expects to find pictures in this directory.
This is used by `image-dired-gallery-generate'."
  :type 'string
  :version "29.1")

(make-obsolete-variable 'image-dired-gallery-hidden-tags nil "29.1")
(defcustom image-dired-gallery-hidden-tags
  (list "private" "hidden" "pending")
  "List of \"hidden\" tags.
Used by `image-dired-gallery-generate' to leave out \"hidden\" images."
  :type '(repeat string))

(make-obsolete-variable 'image-dired-file-tag-list nil "29.1")
(defvar image-dired-file-tag-list nil
  "List to store file-tag structure.")

(make-obsolete-variable 'image-dired-file-comment-list nil "29.1")
(defvar image-dired-file-comment-list nil
  "List to store file comments.")

(defun image-dired--add-to-tag-file-lists (tag file)
  "Helper function used from `image-dired--create-gallery-lists'.

Add TAG to FILE in one list and FILE to TAG in the other.

Lisp structures look like the following:

image-dired-file-tag-list:

  ((\"filename1\" \"tag1\" \"tag2\" \"tag3\" ...)
   (\"filename2\" \"tag1\" \"tag2\" \"tag3\" ...)
   ...)

image-dired-tag-file-list:

 ((\"tag1\" \"filename1\" \"filename2\" \"filename3\" ...)
  (\"tag2\" \"filename1\" \"filename2\" \"filename3\" ...)
  ...)"
  (declare (obsolete nil "29.1"))
  ;; Add tag to file list
  (let (curr)
    (if image-dired-file-tag-list
        (if (setq curr (assoc file image-dired-file-tag-list))
            (setcdr curr (cons tag (cdr curr)))
          (setcdr image-dired-file-tag-list
                  (cons (list file tag) (cdr image-dired-file-tag-list))))
      (setq image-dired-file-tag-list (list (list file tag))))
    ;; Add file to tag list
    (if image-dired-tag-file-list
        (if (setq curr (assoc tag image-dired-tag-file-list))
            (if (not (member file curr))
                (setcdr curr (cons file (cdr curr))))
          (setcdr image-dired-tag-file-list
                  (cons (list tag file) (cdr image-dired-tag-file-list))))
      (setq image-dired-tag-file-list (list (list tag file))))))

(defun image-dired--add-to-file-comment-list (file comment)
  "Helper function used from `image-dired--create-gallery-lists'.

For FILE, add COMMENT to list.

Lisp structure looks like the following:

image-dired-file-comment-list:

  ((\"filename1\" .  \"comment1\")
   (\"filename2\" .  \"comment2\")
   ...)"
  (declare (obsolete nil "29.1"))
  (if image-dired-file-comment-list
      (if (not (assoc file image-dired-file-comment-list))
          (setcdr image-dired-file-comment-list
                  (cons (cons file comment)
                        (cdr image-dired-file-comment-list))))
    (setq image-dired-file-comment-list (list (cons file comment)))))

(defun image-dired--create-gallery-lists ()
  "Create temporary lists used by `image-dired-gallery-generate'."
  (declare (obsolete nil "29.1"))
  (image-dired-sane-db-file)
  (image-dired--with-db-file
    (let (end beg file row-tags)
      (setq image-dired-tag-file-list nil)
      (setq image-dired-file-tag-list nil)
      (setq image-dired-file-comment-list nil)
      (goto-char (point-min))
      (while (search-forward-regexp "^." nil t)
        (end-of-line)
        (setq end (point))
        (beginning-of-line)
        (setq beg (point))
        (unless (search-forward ";" end nil)
          (error "Something is really wrong, check format of database"))
        (setq row-tags (split-string
                        (buffer-substring beg end) ";"))
        (setq file (car row-tags))
        (dolist (x (cdr row-tags))
          (with-suppressed-warnings
              ((obsolete image-dired--add-to-tag-file-lists
                         image-dired--add-to-file-comment-list))
            (if (not (string-match "^comment:\\(.*\\)" x))
                (image-dired--add-to-tag-file-lists x file)
              (image-dired--add-to-file-comment-list file (match-string 1 x))))))))
  ;; Sort tag-file list
  (setq image-dired-tag-file-list
        (sort image-dired-tag-file-list
              (lambda (x y)
                (string< (car x) (car y))))))

(defun image-dired--hidden-p (file)
  "Return t if image FILE has a \"hidden\" tag."
  (declare (obsolete nil "29.1"))
  (cl-loop for tag in (cdr (assoc file image-dired-file-tag-list))
           if (member tag image-dired-gallery-hidden-tags) return t))

(defun image-dired-gallery-generate ()
  "Generate gallery pages.
First we create a couple of Lisp structures from the database to make
it easier to generate, then HTML-files are created in
`image-dired-gallery-dir'."
  (declare (obsolete nil "29.1"))
  (interactive)
  (if (eq 'per-directory image-dired-thumbnail-storage)
      (error "Currently, gallery generation is not supported \
when using per-directory thumbnail file storage"))
  (with-suppressed-warnings ((obsolete image-dired--create-gallery-lists))
    (image-dired--create-gallery-lists))
  (let ((tags image-dired-tag-file-list)
        (index-file (format "%s/index.html" image-dired-gallery-dir))
        count tag tag-file
        comment file-tags tag-link tag-link-list)
    ;; Make sure gallery root exist
    (if (file-exists-p image-dired-gallery-dir)
        (if (not (file-directory-p image-dired-gallery-dir))
            (error "Variable image-dired-gallery-dir is not a directory"))
      ;; FIXME: Should we set umask to 077 here, as we do for thumbnails?
      (make-directory image-dired-gallery-dir))
    ;; Open index file
    (with-temp-file index-file
      (if (file-exists-p index-file)
          (insert-file-contents index-file))
      (insert "<html>\n")
      (insert "  <body>\n")
      (insert "   <h2>Image-Dired Gallery</h2>\n")
      (insert (format "<p>\n    Gallery generated %s\n   <p>\n"
                      (current-time-string)))
      (insert "   <h3>Tag index</h3>\n")
      (setq count 1)
      ;; Pre-generate list of all tag links
      (dolist (curr tags)
        (setq tag (car curr))
        (when (not (member tag image-dired-gallery-hidden-tags))
          (setq tag-link (format "<a href=\"%d.html\">%s</a>" count tag))
          (if tag-link-list
              (setq tag-link-list
                    (append tag-link-list (list (cons tag tag-link))))
            (setq tag-link-list (list (cons tag tag-link))))
          (setq count (1+ count))))
      (setq count 1)
      ;; Main loop where we generated thumbnail pages per tag
      (dolist (curr tags)
        (setq tag (car curr))
        ;; Don't display hidden tags
        (when (not (member tag image-dired-gallery-hidden-tags))
          ;; Insert link to tag page in index
          (insert (format "    %s<br>\n" (cdr (assoc tag tag-link-list))))
          ;; Open per-tag file
          (setq tag-file (format "%s/%s.html" image-dired-gallery-dir count))
          (with-temp-file tag-file
            (if (file-exists-p tag-file)
                (insert-file-contents tag-file))
            (erase-buffer)
            (insert "<html>\n")
            (insert "  <body>\n")
            (insert "  <p><a href=\"index.html\">Index</a></p>\n")
            (insert (format "  <h2>Images with tag &quot;%s&quot;</h2>" tag))
            ;; Main loop for files per tag page
            (dolist (file (cdr curr))
              (unless (image-dired-hidden-p file)
                ;; Insert thumbnail with link to full image
                (insert
                 (format "<a href=\"%s/%s\"><img src=\"%s/%s\"%s></a>\n"
                         image-dired-gallery-image-root-url
                         (file-name-nondirectory file)
                         image-dired-gallery-thumb-image-root-url
                         (file-name-nondirectory (image-dired-thumb-name file)) file))
                ;; Insert comment, if any
                (if (setq comment (cdr (assoc file image-dired-file-comment-list)))
                    (insert (format "<br>\n%s<br>\n" comment))
                  (insert "<br>\n"))
                ;; Insert links to other tags, if any
                (when (> (length
                          (setq file-tags (assoc file image-dired-file-tag-list))) 2)
                  (insert "[ ")
                  (dolist (extra-tag file-tags)
                    ;; Only insert if not file name or the main tag
                    (if (and (not (equal extra-tag tag))
                             (not (equal extra-tag file)))
                        (insert
                         (format "%s " (cdr (assoc extra-tag tag-link-list))))))
                  (insert "]<br>\n"))))
            (insert "  <p><a href=\"index.html\">Index</a></p>\n")
            (insert "  </body>\n")
            (insert "</html>\n"))
          (setq count (1+ count))))
      (insert "  </body>\n")
      (insert "</html>"))))

(define-obsolete-function-alias 'image-dired-slideshow-step #'image-dired--slideshow-step "29.1")
(define-obsolete-function-alias 'image-dired-slideshow-stop #'image-dired--slideshow-stop "29.1")
(define-obsolete-function-alias 'image-dired-create-display-image-buffer
  #'ignore "29.1")
;; These can't use the #' quote as they point to obsolete names.
(define-obsolete-function-alias 'image-dired-create-gallery-lists
  'image-dired--create-gallery-lists "29.1")
(define-obsolete-function-alias 'image-dired-add-to-file-comment-list
  'image-dired--add-to-file-comment-list "29.1")
(define-obsolete-function-alias 'image-dired-add-to-tag-file-lists
  'image-dired--add-to-tag-file-lists "29.1")
(define-obsolete-function-alias 'image-dired-hidden-p
  'image-dired--hidden-p "29.1")
(define-obsolete-function-alias 'image-dired-thumb-update-marks
  #'image-dired--thumb-update-marks "29.1")
(define-obsolete-function-alias 'image-dired-get-thumbnail-image
  #'image-dired--get-create-thumbnail-file "29.1")
(define-obsolete-function-alias 'image-dired-display-thumb-properties
  #'image-dired--update-header-line "29.1")
(define-obsolete-function-alias 'image-dired-delete-marked
  #'image-dired-do-flagged-delete "29.1")
(define-obsolete-function-alias 'image-dired-display-image-mode
  #'image-dired-image-mode "29.1")
(define-obsolete-function-alias 'image-dired-display-thumbnail-original-image
  #'image-dired-display-this "29.1")
(define-obsolete-function-alias 'image-dired-display-next-thumbnail-original
  #'image-dired-display-next "29.1")
(define-obsolete-function-alias 'image-dired-display-previous-thumbnail-original
  #'image-dired-display-previous "29.1")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; TEST-SECTION ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defvar image-dired-dir-max-size 12300000)

;; (defun image-dired-test-clean-old-files ()
;;   "Clean `image-dired-dir' from old thumbnail files.
;; \"Oldness\" measured using last access time.  If the total size of all
;; thumbnail files in `image-dired-dir' is larger than 'image-dired-dir-max-size',
;; old files are deleted until the max size is reached."
;;   (let* ((files
;;           (sort
;;            (mapcar
;;             (lambda (f)
;;               (let ((fattribs (file-attributes f)))
;;                 `(,(file-attribute-access-time fattribs)
;;                   ,(file-attribute-size fattribs) ,f)))
;;             (directory-files (image-dired-dir) t ".+\\.thumb\\..+$"))
;;            ;; Sort function.  Compare time between two files.
;;            (lambda (l1 l2)
;;               (time-less-p (car l1) (car l2)))))
;;          (dirsize (apply '+ (mapcar (lambda (x) (cadr x)) files))))
;;     (while (> dirsize image-dired-dir-max-size)
;;       (y-or-n-p
;;        (format "Size of thumbnail directory: %d, delete old file %s? "
;;                dirsize (cadr (cdar files))))
;;       (delete-file (cadr (cdar files)))
;;       (setq dirsize (- dirsize (car (cdar files))))
;;       (setq files (cdr files)))))

(provide 'image-dired)

;;; image-dired.el ends here
