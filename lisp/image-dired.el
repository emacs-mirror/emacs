;;; image-dired.el --- use dired to browse and manipulate your images -*- lexical-binding: t -*-
;;
;; Copyright (C) 2005-2022 Free Software Foundation, Inc.
;;
;; Version: 0.4.11
;; Keywords: multimedia
;; Author: Mathias Dahl <mathias.rem0veth1s.dahl@gmail.com>

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
;;
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
;; code from there though... :)
;;
;;  `image-dired' stores the thumbnail files in `image-dired-dir'
;; using the file name format ORIGNAME.thumb.ORIGEXT.  For example
;; ~/.emacs.d/image-dired/myimage01.thumb.jpg.  The "database" is for
;; now just a plain text file with the following format:
;;
;; file-name-non-directory;comment:comment-text;tag1;tag2;tag3;...;tagN
;;
;;
;; PREREQUISITES
;; =============
;;
;; * The ImageMagick package.  Currently, `convert' and `mogrify' are
;; used.  Find it here: https://www.imagemagick.org.
;;
;; * For non-lossy rotation of JPEG images, the JpegTRAN program is
;; needed.
;;
;; * For `image-dired-get-exif-data' and `image-dired-set-exif-data' to work,
;; the command line tool `exiftool' is needed.  It can be found here:
;; https://exiftool.org/.  These two functions are, among other
;; things, used for writing comments to image files using
;; `image-dired-thumbnail-set-image-description' and to create
;; "unique" file names using `image-dired-get-exif-file-name' (used by
;; `image-dired-copy-with-exif-file-name').
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
;; the thumbnails are hard-coded to JPEG format.
;;
;; * WARNING: The "database" format used might be changed so keep a
;; backup of `image-dired-db-file' when testing new versions.
;;
;; * `image-dired-display-image-mode' does not support animation
;;
;; TODO
;; ====
;;
;; * Support gallery creation when using per-directory thumbnail
;; storage.
;;
;; * Some sort of auto-rotate function based on rotate info in the
;; EXIF data.
;;
;; * Investigate if it is possible to also write the tags to the image
;; files.
;;
;; * From thumbs.el: Add an option for clean-up/max-size functionality
;;   for thumbnail directory.
;;
;; * From thumbs.el: Add setroot function.
;;
;; * From thumbs.el: Add image resizing, if useful (image-dired's automatic
;;  "image fit" might be enough)
;;
;; * From thumbs.el: Add the "modify" commands (emboss, negate,
;;   monochrome etc).
;;
;; * Add `image-dired-display-thumbs-ring' and functions to cycle that.  Find
;; out which is best, saving old batch just before inserting new, or
;; saving the current batch in the ring when inserting it.  Adding it
;; probably needs rewriting `image-dired-display-thumbs' to be more general.
;;
;; * Find some way of toggling on and off really nice keybindings in
;; dired (for example, using C-n or <down> instead of C-S-n).  Richard
;; suggested that we could keep C-t as prefix for image-dired commands
;; as it is currently not used in dired.  He also suggested that
;; `dired-next-line' and `dired-previous-line' figure out if
;; image-dired is enabled in the current buffer and, if it is, call
;; `image-dired-dired-next-line' and
;; `image-dired-dired-previous-line', respectively.  Update: This is
;; partly done; some bindings have now been added to dired.
;;
;; * Enhanced gallery creation with basic CSS-support and pagination
;; of tag pages with many pictures.
;;
;; * Rewrite `image-dired-modify-mark-on-thumb-original-file' to be
;; less ugly.
;;
;; * In some way keep track of buffers and windows and stuff so that
;; it works as the user expects.
;;
;; * More/better documentation
;;
;;
;;; Code:

(require 'dired)
(require 'image-mode)
(require 'widget)

(eval-when-compile
  (require 'cl-lib)
  (require 'wid-edit))

(defgroup image-dired nil
  "Use Dired to browse your images as thumbnails, and more."
  :prefix "image-dired-"
  :link '(info-link "(emacs) Image-Dired")
  :group 'multimedia)

(defcustom image-dired-dir (locate-user-emacs-file "image-dired/")
  "Directory where thumbnail images are stored.

The value of this option will be ignored if Image Dired is
customized to use the Thumbnail Managing Standard; they will be
saved in \"$XDG_CACHE_HOME/thumbnails/\" instead.  See
`image-dired-thumbnail-storage'."
  :type 'directory)

(defcustom image-dired-thumbnail-storage 'use-image-dired-dir
  "How to store image-dired's thumbnail files.
Image-Dired can store thumbnail files in one of two ways and this is
controlled by this variable.  \"Use image-dired dir\" means that the
thumbnails are stored in a central directory.  \"Per directory\"
means that each thumbnail is stored in a subdirectory called
\".image-dired\" in the same directory where the image file is.
\"Thumbnail Managing Standard\" means that the thumbnails are
stored and generated according to the Thumbnail Managing Standard
that allows sharing of thumbnails across different programs."
  :type '(choice :tag "How to store thumbnail files"
                 (const :tag "Use image-dired-dir" use-image-dired-dir)
                 (const :tag "Thumbnail Managing Standard (normal 128x128)" standard)
                 (const :tag "Thumbnail Managing Standard (large 256x256)" standard-large)
                 (const :tag "Per-directory" per-directory)))

(defcustom image-dired-db-file
  (expand-file-name ".image-dired_db" image-dired-dir)
  "Database file where file names and their associated tags are stored."
  :type 'file)

(defcustom image-dired-temp-image-file
  (expand-file-name ".image-dired_temp" image-dired-dir)
  "Name of temporary image file used by various commands."
  :type 'file)

(defcustom image-dired-gallery-dir
  (expand-file-name ".image-dired_gallery" image-dired-dir)
  "Directory to store generated gallery html pages.
This path needs to be \"shared\" to the public so that it can access
the index.html page that image-dired creates."
  :type 'directory)

(defcustom image-dired-gallery-image-root-url
"https://your.own.server/image-diredpics"
  "URL where the full size images are to be found.
Note that this path has to be configured in your web server.  Image-Dired
expects to find pictures in this directory."
  :type 'string)

(defcustom image-dired-gallery-thumb-image-root-url
"https://your.own.server/image-diredthumbs"
  "URL where the thumbnail images are to be found.
Note that this path has to be configured in your web server.  Image-Dired
expects to find pictures in this directory."
  :type 'string)

(defcustom image-dired-cmd-create-thumbnail-program
  "convert"
  "Executable used to create thumbnail.
Used together with `image-dired-cmd-create-thumbnail-options'."
  :type 'file)

(defcustom image-dired-cmd-create-thumbnail-options
  '("-size" "%wx%h" "%f[0]" "-resize" "%wx%h>" "-strip" "jpeg:%t")
  "Options of command used to create thumbnail image.
Used with `image-dired-cmd-create-thumbnail-program'.
Available format specifiers are: %w which is replaced by
`image-dired-thumb-width', %h which is replaced by `image-dired-thumb-height',
%f which is replaced by the file name of the original image and %t
which is replaced by the file name of the thumbnail file."
  :version "26.1"
  :type '(repeat (string :tag "Argument")))

(defcustom image-dired-cmd-create-temp-image-program "convert"
  "Executable used to create temporary image.
Used together with `image-dired-cmd-create-temp-image-options'."
  :type 'file)

(defcustom image-dired-cmd-create-temp-image-options
  '("-size" "%wx%h" "%f[0]" "-resize" "%wx%h>" "-strip" "jpeg:%t")
  "Options of command used to create temporary image for display window.
Used together with `image-dired-cmd-create-temp-image-program',
Available format specifiers are: %w and %h which are replaced by
the calculated max size for width and height in the image display window,
%f which is replaced by the file name of the original image and %t which
is replaced by the file name of the temporary file."
  :version "26.1"
  :type '(repeat (string :tag "Argument")))

(defcustom image-dired-cmd-pngnq-program
  (or (executable-find "pngnq")
      (executable-find "pngnq-s9"))
  "The file name of the `pngnq' program.
It quantizes colors of PNG images down to 256 colors or fewer
using the NeuQuant algorithm."
  :version "26.1"
  :type '(choice (const :tag "Not Set" nil) file))

(defcustom image-dired-cmd-pngnq-options
  '("-f" "%t")
  "Arguments to pass `image-dired-cmd-pngnq-program'.
Available format specifiers are the same as in
`image-dired-cmd-create-thumbnail-options'."
  :version "26.1"
  :type '(repeat (string :tag "Argument")))

(defcustom image-dired-cmd-pngcrush-program (executable-find "pngcrush")
  "The file name of the `pngcrush' program.
It optimizes the compression of PNG images.  Also it adds PNG textual chunks
with the information required by the Thumbnail Managing Standard."
  :type '(choice (const :tag "Not Set" nil) file))

(defcustom image-dired-cmd-pngcrush-options
  `("-q"
    "-text" "b" "Description" "Thumbnail of file://%f"
    "-text" "b" "Software" ,(emacs-version)
    ;; "-text b \"Thumb::Image::Height\" \"%oh\" "
    ;; "-text b \"Thumb::Image::Mimetype\" \"%mime\" "
    ;; "-text b \"Thumb::Image::Width\" \"%ow\" "
    "-text" "b" "Thumb::MTime" "%m"
    ;; "-text b \"Thumb::Size\" \"%b\" "
    "-text" "b" "Thumb::URI" "file://%f"
    "%q" "%t")
  "Arguments for `image-dired-cmd-pngcrush-program'.
Available format specifiers are the same as in
`image-dired-cmd-create-thumbnail-options', with %q for a
temporary file name (typically generated by pnqnq)."
  :version "26.1"
  :type '(repeat (string :tag "Argument")))

(defcustom image-dired-cmd-optipng-program (executable-find "optipng")
  "The file name of the `optipng' program."
  :version "26.1"
  :type '(choice (const :tag "Not Set" nil) file))

(defcustom image-dired-cmd-optipng-options '("-o5" "%t")
  "Arguments passed to `image-dired-cmd-optipng-program'.
Available format specifiers are described in
`image-dired-cmd-create-thumbnail-options'."
  :version "26.1"
  :type '(repeat (string :tag "Argument"))
  :link '(url-link "man:optipng(1)"))

(defcustom image-dired-cmd-create-standard-thumbnail-options
  (append '("-size" "%wx%h" "%f[0]")
          (unless (or image-dired-cmd-pngcrush-program
                      image-dired-cmd-pngnq-program)
            (list
             "-set" "Thumb::MTime" "%m"
             "-set" "Thumb::URI" "file://%f"
             "-set" "Description" "Thumbnail of file://%f"
             "-set" "Software" (emacs-version)))
          '("-thumbnail" "%wx%h>" "png:%t"))
  "Options for creating thumbnails according to the Thumbnail Managing Standard.
Available format specifiers are the same as in
`image-dired-cmd-create-thumbnail-options', with %m for file modification time."
  :version "26.1"
  :type '(repeat (string :tag "Argument")))

(defcustom image-dired-cmd-rotate-thumbnail-program
  "mogrify"
  "Executable used to rotate thumbnail.
Used together with `image-dired-cmd-rotate-thumbnail-options'."
  :type 'file)

(defcustom image-dired-cmd-rotate-thumbnail-options
  '("-rotate" "%d" "%t")
  "Arguments of command used to rotate thumbnail image.
Used with `image-dired-cmd-rotate-thumbnail-program'.
Available format specifiers are: %d which is replaced by the
number of (positive) degrees to rotate the image, normally 90 or 270
\(for 90 degrees right and left), %t which is replaced by the file name
of the thumbnail file."
  :version "26.1"
  :type '(repeat (string :tag "Argument")))

(defcustom image-dired-cmd-rotate-original-program
  "jpegtran"
  "Executable used to rotate original image.
Used together with `image-dired-cmd-rotate-original-options'."
  :type 'file)

(defcustom image-dired-cmd-rotate-original-options
  '("-rotate" "%d" "-copy" "all" "-outfile" "%t" "%o")
  "Arguments of command used to rotate original image.
Used with `image-dired-cmd-rotate-original-program'.
Available format specifiers are: %d which is replaced by the
number of (positive) degrees to rotate the image, normally 90 or
270 \(for 90 degrees right and left), %o which is replaced by the
original image file name and %t which is replaced by
`image-dired-temp-image-file'."
  :version "26.1"
  :type '(repeat (string :tag "Argument")))

(defcustom image-dired-temp-rotate-image-file
  (expand-file-name ".image-dired_rotate_temp" image-dired-dir)
  "Temporary file for rotate operations."
  :type 'file)

(defcustom image-dired-rotate-original-ask-before-overwrite t
  "Confirm overwrite of original file after rotate operation.
If non-nil, ask user for confirmation before overwriting the
original file with `image-dired-temp-rotate-image-file'."
  :type 'boolean)

(defcustom image-dired-cmd-write-exif-data-program
  "exiftool"
  "Program used to write EXIF data to image.
Used together with `image-dired-cmd-write-exif-data-options'."
  :type 'file)

(defcustom image-dired-cmd-write-exif-data-options
  '("-%t=%v" "%f")
  "Arguments of command used to write EXIF data.
Used with `image-dired-cmd-write-exif-data-program'.
Available format specifiers are: %f which is replaced by
the image file name, %t which is replaced by the tag name and %v
which is replaced by the tag value."
  :version "26.1"
  :type '(repeat (string :tag "Argument")))

(defcustom image-dired-cmd-read-exif-data-program
  "exiftool"
  "Program used to read EXIF data to image.
Used together with `image-dired-cmd-read-exif-data-options'."
  :type 'file)

(defcustom image-dired-cmd-read-exif-data-options
  '("-s" "-s" "-s" "-%t" "%f")
  "Arguments of command used to read EXIF data.
Used with `image-dired-cmd-read-exif-data-program'.
Available format specifiers are: %f which is replaced
by the image file name and %t which is replaced by the tag name."
  :version "26.1"
  :type '(repeat (string :tag "Argument")))

(defcustom image-dired-gallery-hidden-tags
  (list "private" "hidden" "pending")
  "List of \"hidden\" tags.
Used by `image-dired-gallery-generate' to leave out \"hidden\" images."
  :type '(repeat string))

(defcustom image-dired-thumb-size
  (cond
   ((eq 'standard image-dired-thumbnail-storage) 128)
   ((eq 'standard-large image-dired-thumbnail-storage) 256)
   (t 100))
  "Size of thumbnails, in pixels.
This is the default size for both `image-dired-thumb-width'
and `image-dired-thumb-height'.

The value of this option will be ignored if Image Dired is
customized to use the Thumbnail Managing Standard; the standard
sizes will be used instead.  See `image-dired-thumbnail-storage'."
  :type 'integer)

(defcustom image-dired-thumb-width image-dired-thumb-size
  "Width of thumbnails, in pixels."
  :type 'integer)

(defcustom image-dired-thumb-height image-dired-thumb-size
  "Height of thumbnails, in pixels."
  :type 'integer)

(defcustom image-dired-thumb-relief 2
  "Size of button-like border around thumbnails."
  :type 'integer)

(defcustom image-dired-thumb-margin 2
  "Size of the margin around thumbnails.
This is where you see the cursor."
  :type 'integer)

(defcustom image-dired-thumb-visible-marks t
  "Make marks visible in thumbnail buffer.
If non-nil, apply the `image-dired-thumb-mark' face to marked
images."
  :type 'boolean
  :version "28.1")

(defface image-dired-thumb-mark
  '((t (:background "orange")))
  "Background-color for marked images in thumbnail buffer."
  :group 'image-dired
  :version "28.1")

(defcustom image-dired-line-up-method 'dynamic
  "Default method for line-up of thumbnails in thumbnail buffer.
Used by `image-dired-display-thumbs' and other functions that needs
to line-up thumbnails.  Dynamic means to use the available width of
the window containing the thumbnail buffer, Fixed means to use
`image-dired-thumbs-per-row', Interactive is for asking the user,
and No line-up means that no automatic line-up will be done."
  :type '(choice :tag "Default line-up method"
                 (const :tag "Dynamic" dynamic)
		 (const :tag "Fixed" fixed)
		 (const :tag "Interactive" interactive)
                 (const :tag "No line-up" none)))

(defcustom image-dired-thumbs-per-row 3
  "Number of thumbnails to display per row in thumb buffer."
  :type 'integer)

(defcustom image-dired-display-window-width-correction 1
  "Number to be used to correct image display window width.
Change if the default (1) does not work (i.e. if the image does not
completely fit)."
  :type 'integer)

(defcustom image-dired-display-window-height-correction 0
  "Number to be used to correct image display window height.
Change if the default (0) does not work (i.e. if the image does not
completely fit)."
  :type 'integer)

(defcustom image-dired-track-movement t
  "The current state of the tracking and mirroring.
For more information, see the documentation for
`image-dired-toggle-movement-tracking'."
  :type 'boolean)

(defcustom image-dired-append-when-browsing nil
  "Append thumbnails in thumbnail buffer when browsing.
If non-nil, using `image-dired-next-line-and-display' and
`image-dired-previous-line-and-display' will leave a trail of thumbnail
images in the thumbnail buffer.  If you enable this and want to clean
the thumbnail buffer because it is filled with too many thumbnails,
just call `image-dired-display-thumb' to display only the image at point.
This value can be toggled using `image-dired-toggle-append-browsing'."
  :type 'boolean)

(defcustom image-dired-dired-disp-props t
  "If non-nil, display properties for Dired file when browsing.
Used by `image-dired-next-line-and-display',
`image-dired-previous-line-and-display' and `image-dired-mark-and-display-next'.
If the database file is large, this can slow down image browsing in
Dired and you might want to turn it off."
  :type 'boolean)

(defcustom image-dired-display-properties-format "%b: %f (%t): %c"
  "Display format for thumbnail properties.
%b is replaced with associated Dired buffer name, %f with file
name (without path) of original image file, %t with the list of
tags and %c with the comment."
  :type 'string)

(defcustom image-dired-external-viewer
  ;; TODO: Use mailcap, dired-guess-shell-alist-default,
  ;; dired-view-command-alist.
  (cond ((executable-find "display"))
        ((executable-find "xli"))
        ((executable-find "qiv") "qiv -t")
        ((executable-find "feh") "feh"))
  "Name of external viewer.
Including parameters.  Used when displaying original image from
`image-dired-thumbnail-mode'."
  :version "28.1"
  :type '(choice string
                 (const :tag "Not Set" nil)))

(defcustom image-dired-main-image-directory "~/pics/"
  "Name of main image directory, if any.
Used by `image-dired-copy-with-exif-file-name'."
  :type 'string)

(defcustom image-dired-show-all-from-dir-max-files 50
  "Maximum number of files to show using `image-dired-show-all-from-dir'
before warning."
  :type 'integer)

(defmacro image-dired--with-db-file (&rest body)
  "Run BODY in a temp buffer containing `image-dired-db-file'.
Return the last form in BODY."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (if (file-exists-p image-dired-db-file)
	 (insert-file-contents image-dired-db-file))
     ,@body))

(defun image-dired-dir ()
  "Return the current thumbnails directory (from variable `image-dired-dir').
Create the thumbnails directory if it does not exist."
  (let ((image-dired-dir (file-name-as-directory
                    (expand-file-name image-dired-dir))))
    (unless (file-directory-p image-dired-dir)
      (with-file-modes #o700
        (make-directory image-dired-dir t))
      (message "Creating thumbnails directory"))
    image-dired-dir))

(defun image-dired-insert-image (file type relief margin)
  "Insert image FILE of image TYPE, using RELIEF and MARGIN, at point."
  (let ((i `(image :type ,type
                   :file ,file
                   :relief ,relief
                   :margin ,margin)))
    (insert-image i)))

(defun image-dired-get-thumbnail-image (file)
  "Return the image descriptor for a thumbnail of image file FILE."
  (unless (string-match (image-file-name-regexp) file)
    (error "%s is not a valid image file" file))
  (let* ((thumb-file (image-dired-thumb-name file))
	 (thumb-attr (file-attributes thumb-file)))
    (when (or (not thumb-attr)
	      (time-less-p (file-attribute-modification-time thumb-attr)
			   (file-attribute-modification-time
			    (file-attributes file))))
      (image-dired-create-thumb file thumb-file))
    (create-image thumb-file)
;;     (list 'image :type 'jpeg
;;           :file thumb-file
;; 	  :relief image-dired-thumb-relief :margin image-dired-thumb-margin)
    ))

(defun image-dired-insert-thumbnail (file original-file-name
                                     associated-dired-buffer)
  "Insert thumbnail image FILE.
Add text properties ORIGINAL-FILE-NAME and ASSOCIATED-DIRED-BUFFER."
  (let (beg end)
    (setq beg (point))
    (image-dired-insert-image file
                        ;; TODO: this should depend on the real file type
                        (if (memq image-dired-thumbnail-storage
                                  '(standard standard-large))
                            'png 'jpeg)
                        image-dired-thumb-relief
                        image-dired-thumb-margin)
    (setq end (point))
    (add-text-properties
     beg end
     (list 'image-dired-thumbnail t
           'original-file-name original-file-name
           'associated-dired-buffer associated-dired-buffer
           'tags (image-dired-list-tags original-file-name)
           'mouse-face 'highlight
           'comment (image-dired-get-comment original-file-name)))))

(defun image-dired-thumb-name (file)
  "Return thumbnail file name for FILE.
Depending on the value of `image-dired-thumbnail-storage', the file
name will vary.  For central thumbnail file storage, make a
MD5-hash of the image file's directory name and add that to make
the thumbnail file name unique.  For per-directory storage, just
add a subdirectory.  For standard storage, produce the file name
according to the Thumbnail Managing Standard."
  (cond ((memq image-dired-thumbnail-storage '(standard standard-large))
         (let* ((xdg (getenv "XDG_CACHE_HOME"))
                (dir (if (and xdg (file-name-absolute-p xdg))
                         xdg "~/.cache"))
                (thumbdir (cl-case image-dired-thumbnail-storage
                            (standard "thumbnails/normal")
                            (standard-large "thumbnails/large"))))
           (expand-file-name
            (concat (md5 (concat "file://" (expand-file-name file))) ".png")
            (expand-file-name thumbdir dir))))
        ((eq 'use-image-dired-dir image-dired-thumbnail-storage)
         (let* ((f (expand-file-name file))
                (md5-hash
                 ;; Is MD5 hashes fast enough? The checksum of a
                 ;; thumbnail file name need not be that
                 ;; "cryptographically" good so a faster one could
                 ;; be used here.
                 (md5 (file-name-as-directory (file-name-directory f)))))
           (format "%s%s%s.thumb.%s"
                   (file-name-as-directory (expand-file-name (image-dired-dir)))
                   (file-name-base f)
                   (if md5-hash (concat "_" md5-hash) "")
                   (file-name-extension f))))
        ((eq 'per-directory image-dired-thumbnail-storage)
         (let ((f (expand-file-name file)))
           (format "%s.image-dired/%s.thumb.%s"
                   (file-name-directory f)
                   (file-name-base f)
                   (file-name-extension f))))))

(defun image-dired--check-executable-exists (executable)
  (unless (executable-find (symbol-value executable))
    (error "Executable %S not found" executable)))

(defun image-dired-thumb-size (dimension)
  "Return thumb size depending on `image-dired-thumbnail-storage'.
DIMENSION should be either the symbol `width' or `height'."
  (cond
   ((eq 'standard image-dired-thumbnail-storage) 128)
   ((eq 'standard-large image-dired-thumbnail-storage) 256)
   (t (cl-ecase dimension
        (width image-dired-thumb-width)
        (height image-dired-thumb-height)))))

(defvar image-dired-queue nil
  "List of items in the queue.
Each item has the form (ORIGINAL-FILE TARGET-FILE).")

(defvar image-dired-queue-active-jobs 0
  "Number of active jobs in `image-dired-queue'.")

(defvar image-dired-queue-active-limit 2
  "Maximum number of concurrent jobs permitted for generating images.
Increase at own risk.")

(defvar image-dired-tag-history nil "Variable holding the tag history.")

(defun image-dired-pngnq-thumb (spec)
  "Quantize thumbnail described by format SPEC with pngnq(1)."
  (let ((process
         (apply #'start-process "image-dired-pngnq" nil
                image-dired-cmd-pngnq-program
                (mapcar (lambda (arg) (format-spec arg spec))
                        image-dired-cmd-pngnq-options))))
    (setf (process-sentinel process)
          (lambda (process status)
            (if (and (eq (process-status process) 'exit)
                     (zerop (process-exit-status process)))
                ;; Pass off to pngcrush, or just rename the
                ;; THUMB-nq8.png file back to THUMB.png
                (if (and image-dired-cmd-pngcrush-program
                         (executable-find image-dired-cmd-pngcrush-program))
                    (image-dired-pngcrush-thumb spec)
                  (let ((nq8 (cdr (assq ?q spec)))
                        (thumb (cdr (assq ?t spec))))
                    (rename-file nq8 thumb t)))
              (message "command %S %s" (process-command process)
                       (string-replace "\n" "" status)))))
    process))

(defun image-dired-pngcrush-thumb (spec)
  "Optimize thumbnail described by format SPEC with pngcrush(1)."
  ;; If pngnq wasn't run, then the THUMB-nq8.png file does not exist.
  ;; pngcrush needs an infile and outfile, so we just copy THUMB to
  ;; THUMB-nq8.png and use the latter as a temp file.
  (when (not image-dired-cmd-pngnq-program)
    (let ((temp (cdr (assq ?q spec)))
          (thumb (cdr (assq ?t spec))))
      (copy-file thumb temp)))
  (let ((process
         (apply #'start-process "image-dired-pngcrush" nil
                image-dired-cmd-pngcrush-program
                (mapcar (lambda (arg) (format-spec arg spec))
                        image-dired-cmd-pngcrush-options))))
    (setf (process-sentinel process)
          (lambda (process status)
            (unless (and (eq (process-status process) 'exit)
                         (zerop (process-exit-status process)))
              (message "command %S %s" (process-command process)
                       (string-replace "\n" "" status)))
            (when (memq (process-status process) '(exit signal))
              (let ((temp (cdr (assq ?q spec))))
                (delete-file temp)))))
    process))

(defun image-dired-optipng-thumb (spec)
  "Optimize thumbnail described by format SPEC with optipng(1)."
  (let ((process
         (apply #'start-process "image-dired-optipng" nil
                image-dired-cmd-optipng-program
                (mapcar (lambda (arg) (format-spec arg spec))
                        image-dired-cmd-optipng-options))))
    (setf (process-sentinel process)
          (lambda (process status)
            (unless (and (eq (process-status process) 'exit)
                         (zerop (process-exit-status process)))
              (message "command %S %s" (process-command process)
                       (string-replace "\n" "" status)))))
    process))

(defun image-dired-create-thumb-1 (original-file thumbnail-file)
  "For ORIGINAL-FILE, create thumbnail image named THUMBNAIL-FILE."
  (image-dired--check-executable-exists
   'image-dired-cmd-create-thumbnail-program)
  (let* ((width (int-to-string (image-dired-thumb-size 'width)))
         (height (int-to-string (image-dired-thumb-size 'height)))
	 (modif-time (format-time-string
		      "%s" (file-attribute-modification-time
			    (file-attributes original-file))))
         (thumbnail-nq8-file (replace-regexp-in-string ".png\\'" "-nq8.png"
                                                       thumbnail-file))
         (spec
          (list
           (cons ?w width)
           (cons ?h height)
           (cons ?m modif-time)
           (cons ?f original-file)
           (cons ?q thumbnail-nq8-file)
           (cons ?t thumbnail-file)))
         (thumbnail-dir (file-name-directory thumbnail-file))
         process)
    (when (not (file-exists-p thumbnail-dir))
      (message "Creating thumbnail directory")
      (with-file-modes #o700
	(make-directory thumbnail-dir t)))

    ;; Thumbnail file creation processes begin here and are marshaled
    ;; in a queue by `image-dired-create-thumb'.
    (setq process
          (apply #'start-process "image-dired-create-thumbnail" nil
                 image-dired-cmd-create-thumbnail-program
                 (mapcar
                  (lambda (arg) (format-spec arg spec))
                  (if (memq image-dired-thumbnail-storage
                            '(standard standard-large))
                      image-dired-cmd-create-standard-thumbnail-options
                    image-dired-cmd-create-thumbnail-options))))

    (setf (process-sentinel process)
          (lambda (process status)
            ;; Trigger next in queue once a thumbnail has been created
            (cl-decf image-dired-queue-active-jobs)
            (image-dired-thumb-queue-run)
            (if (not (and (eq (process-status process) 'exit)
                          (zerop (process-exit-status process))))
                (message "Thumb could not be created for %s: %s"
                         (abbreviate-file-name original-file)
                         (string-replace "\n" "" status))
              (set-file-modes thumbnail-file #o600)
              (clear-image-cache thumbnail-file)
              ;; PNG thumbnail has been created since we are
              ;; following the XDG thumbnail spec, so try to optimize
              (when (memq image-dired-thumbnail-storage
                          '(standard standard-large))
                (cond
                 ((and image-dired-cmd-pngnq-program
                       (executable-find image-dired-cmd-pngnq-program))
                  (image-dired-pngnq-thumb spec))
                 ((and image-dired-cmd-pngcrush-program
                       (executable-find image-dired-cmd-pngcrush-program))
                  (image-dired-pngcrush-thumb spec))
                 ((and image-dired-cmd-optipng-program
                       (executable-find image-dired-cmd-optipng-program))
                  (image-dired-optipng-thumb spec)))))))
    process))

(defun image-dired-thumb-queue-run ()
  "Run a queued job if one exists and not too many jobs are running.
Queued items live in `image-dired-queue'."
  (while (and image-dired-queue
              (< image-dired-queue-active-jobs
                 image-dired-queue-active-limit))
    (cl-incf image-dired-queue-active-jobs)
    (apply #'image-dired-create-thumb-1 (pop image-dired-queue))))

(defun image-dired-create-thumb (original-file thumbnail-file)
  "Add a job for generating ORIGINAL-FILE thumbnail to `image-dired-queue'.
The new file will be named THUMBNAIL-FILE."
  (setq image-dired-queue
        (nconc image-dired-queue
               (list (list original-file thumbnail-file))))
  (run-at-time 0 nil #'image-dired-thumb-queue-run))

(defmacro image-dired--with-marked (&rest body)
  "Eval BODY with point on each marked thumbnail.
If no marked file could be found, execute BODY on the current
thumbnail."
  `(with-current-buffer image-dired-thumbnail-buffer
     (let (found)
       (save-mark-and-excursion
         (goto-char (point-min))
         (while (not (eobp))
           (when (image-dired-thumb-file-marked-p)
             (setq found t)
             ,@body)
           (forward-char)))
       (unless found
         ,@body))))

;;;###autoload
(defun image-dired-dired-toggle-marked-thumbs (&optional arg)
  "Toggle thumbnails in front of file names in the Dired buffer.
If no marked file could be found, insert or hide thumbnails on the
current line.  ARG, if non-nil, specifies the files to use instead
of the marked files.  If ARG is an integer, use the next ARG (or
previous -ARG, if ARG<0) files."
  (interactive "P")
  (dired-map-over-marks
   (let ((image-pos  (dired-move-to-filename))
         (image-file (dired-get-filename nil t))
         thumb-file
         overlay)
     (when (and image-file
                (string-match-p (image-file-name-regexp) image-file))
       (setq thumb-file (image-dired-get-thumbnail-image image-file))
       ;; If image is not already added, then add it.
       (let ((thumb-ov (cl-loop for ov in (overlays-in (point) (1+ (point)))
                                if (overlay-get ov 'thumb-file) return ov)))
         (if thumb-ov
             (delete-overlay thumb-ov)
	   (put-image thumb-file image-pos)
	   (setq overlay
                 (cl-loop for ov in (overlays-in (point) (1+ (point)))
                          if (overlay-get ov 'put-image) return ov))
	   (overlay-put overlay 'image-file image-file)
	   (overlay-put overlay 'thumb-file thumb-file)))))
   arg             ; Show or hide image on ARG next files.
   'show-progress) ; Update dired display after each image is updated.
  (add-hook 'dired-after-readin-hook
            'image-dired-dired-after-readin-hook nil t))

(defun image-dired-dired-after-readin-hook ()
  "Relocate existing thumbnail overlays in Dired buffer after reverting.
Move them to their corresponding files if they still exist.
Otherwise, delete overlays."
  (mapc (lambda (overlay)
          (when (overlay-get overlay 'put-image)
            (let* ((image-file (overlay-get overlay 'image-file))
                   (image-pos (dired-goto-file image-file)))
              (if image-pos
                  (move-overlay overlay image-pos image-pos)
                (delete-overlay overlay)))))
        (overlays-in (point-min) (point-max))))

(defun image-dired-next-line-and-display ()
  "Move to next Dired line and display thumbnail image."
  (interactive)
  (dired-next-line 1)
  (image-dired-display-thumbs
   t (or image-dired-append-when-browsing nil) t)
  (if image-dired-dired-disp-props
      (image-dired-dired-display-properties)))

(defun image-dired-previous-line-and-display ()
  "Move to previous Dired line and display thumbnail image."
  (interactive)
  (dired-previous-line 1)
  (image-dired-display-thumbs
   t (or image-dired-append-when-browsing nil) t)
  (if image-dired-dired-disp-props
      (image-dired-dired-display-properties)))

(defun image-dired-toggle-append-browsing ()
  "Toggle `image-dired-append-when-browsing'."
  (interactive)
  (setq image-dired-append-when-browsing
        (not image-dired-append-when-browsing))
  (message "Append browsing %s."
           (if image-dired-append-when-browsing
               "on"
             "off")))

(defun image-dired-mark-and-display-next ()
  "Mark current file in Dired and display next thumbnail image."
  (interactive)
  (dired-mark 1)
  (image-dired-display-thumbs
   t (or image-dired-append-when-browsing nil) t)
  (if image-dired-dired-disp-props
      (image-dired-dired-display-properties)))

(defun image-dired-toggle-dired-display-properties ()
  "Toggle `image-dired-dired-disp-props'."
  (interactive)
  (setq image-dired-dired-disp-props
        (not image-dired-dired-disp-props))
  (message "Dired display properties %s."
           (if image-dired-dired-disp-props
               "on"
             "off")))

(defvar image-dired-thumbnail-buffer "*image-dired*"
  "Image-Dired's thumbnail buffer.")

(defun image-dired-create-thumbnail-buffer ()
  "Create thumb buffer and set `image-dired-thumbnail-mode'."
  (let ((buf (get-buffer-create image-dired-thumbnail-buffer)))
    (with-current-buffer buf
      (setq buffer-read-only t)
      (if (not (eq major-mode 'image-dired-thumbnail-mode))
          (image-dired-thumbnail-mode)))
    buf))

(defvar image-dired-display-image-buffer "*image-dired-display-image*"
  "Where larger versions of the images are display.")

(defun image-dired-create-display-image-buffer ()
  "Create image display buffer and set `image-dired-display-image-mode'."
  (let ((buf (get-buffer-create image-dired-display-image-buffer)))
    (with-current-buffer buf
      (setq buffer-read-only t)
      (if (not (eq major-mode 'image-dired-display-image-mode))
          (image-dired-display-image-mode)))
    buf))

(defvar image-dired-saved-window-configuration nil
  "Saved window configuration.")

;;;###autoload
(defun image-dired-dired-with-window-configuration (dir &optional arg)
  "Open directory DIR and create a default window configuration.

Convenience command that:

 - Opens Dired in folder DIR
 - Splits windows in most useful (?) way
 - Sets `truncate-lines' to t

After the command has finished, you would typically mark some
image files in Dired and type
\\[image-dired-display-thumbs] (`image-dired-display-thumbs').

If called with prefix argument ARG, skip splitting of windows.

The current window configuration is saved and can be restored by
calling `image-dired-restore-window-configuration'."
  (interactive "DDirectory: \nP")
  (let ((buf (image-dired-create-thumbnail-buffer))
        (buf2 (image-dired-create-display-image-buffer)))
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
  "Restore window configuration.
Restore any changes to the window configuration made by calling
`image-dired-dired-with-window-configuration'."
  (interactive)
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
  (interactive "P")
  (let ((buf (image-dired-create-thumbnail-buffer))
        thumb-name files dired-buf)
    (if arg
        (setq files (list (dired-get-filename)))
      (setq files (dired-get-marked-files)))
    (setq dired-buf (current-buffer))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (if (not append)
            (erase-buffer)
          (goto-char (point-max)))
        (dolist (curr-file files)
          (setq thumb-name (image-dired-thumb-name curr-file))
          (when (not (file-exists-p thumb-name))
            (image-dired-create-thumb curr-file thumb-name))
          (image-dired-insert-thumbnail thumb-name curr-file dired-buf)))
      (if do-not-pop
          (display-buffer buf)
        (pop-to-buffer buf))
      (image-dired--line-up-with-method))))

;;;###autoload
(defun image-dired-show-all-from-dir (dir)
  "Make a preview buffer for all images in DIR and display it.
If the number of files in DIR matching `image-file-name-regexp'
exceeds `image-dired-show-all-from-dir-max-files', a warning will be
displayed."
  (interactive "DImage Dired: ")
  (dired dir)
  (dired-mark-files-regexp (image-file-name-regexp))
  (let ((files (dired-get-marked-files)))
    (if (or (<= (length files) image-dired-show-all-from-dir-max-files)
            (and (> (length files) image-dired-show-all-from-dir-max-files)
                 (y-or-n-p
                  (format
                   "Directory contains more than %d image files.  Proceed? "
                   image-dired-show-all-from-dir-max-files))))
        (progn
          (image-dired-display-thumbs)
          (pop-to-buffer image-dired-thumbnail-buffer))
      (message "Canceled."))))

;;;###autoload
(defalias 'image-dired 'image-dired-show-all-from-dir)

;;;###autoload
(define-obsolete-function-alias 'tumme 'image-dired "24.4")

(defun image-dired-sane-db-file ()
  "Check if `image-dired-db-file' exists.
If not, try to create it (including any parent directories).
Signal error if there are problems creating it."
  (or (file-exists-p image-dired-db-file)
      (let (dir buf)
        (unless (file-directory-p (setq dir (file-name-directory
                                             image-dired-db-file)))
          (with-file-modes #o700
            (make-directory dir t)))
        (with-current-buffer (setq buf (create-file-buffer
                                        image-dired-db-file))
          (with-file-modes #o600
            (write-file image-dired-db-file)))
        (kill-buffer buf)
        (file-exists-p image-dired-db-file))
      (error "Could not create %s" image-dired-db-file)))

(defun image-dired-write-tags (file-tags)
  "Write file tags to database.
Write each file and tag in FILE-TAGS to the database.
FILE-TAGS is an alist in the following form:
 ((FILE . TAG) ... )"
  (image-dired-sane-db-file)
  (let (end file tag)
    (image-dired--with-db-file
     (setq buffer-file-name image-dired-db-file)
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
  "For all FILES, remove TAG from the image database."
  (image-dired-sane-db-file)
  (image-dired--with-db-file
   (setq buffer-file-name image-dired-db-file)
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
	   ;; Check if file should still be in the database. If
	   ;; it has no tags or comments, it will be removed.
	   (end-of-line)
	   (setq end (point))
	   (beginning-of-line)
	   (when (not (search-forward ";" end t))
	     (kill-line 1))))))
   (save-buffer)))

(defun image-dired-list-tags (file)
  "Read all tags for image FILE from the image database."
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
  "Tag marked file(s) in Dired.  With prefix ARG, tag file at point."
  (interactive "P")
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
  (interactive)
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
  (interactive "P")
  (let ((tag (completing-read "Tag to remove: " image-dired-tag-history
                              nil nil nil 'image-dired-tag-history))
        files)
    (if arg
        (setq files (list (dired-get-filename)))
      (setq files (dired-get-marked-files)))
    (image-dired-remove-tag files tag)))

(defun image-dired-tag-thumbnail-remove ()
  "Remove tag from current or marked thumbnails."
  (interactive)
  (let ((tag (completing-read "Tag to remove: " image-dired-tag-history
                              nil nil nil 'image-dired-tag-history)))
    (image-dired--with-marked
     (image-dired-remove-tag (image-dired-original-file-name) tag)
     (image-dired-update-property
      'tags (image-dired-list-tags (image-dired-original-file-name))))))

(defun image-dired-original-file-name ()
  "Get original file name for thumbnail or display image at point."
  (get-text-property (point) 'original-file-name))

(defun image-dired-file-name-at-point ()
  "Get abbreviated file name for thumbnail or display image at point."
  (let ((f (image-dired-original-file-name)))
    (when f
      (abbreviate-file-name f))))

(defun image-dired-associated-dired-buffer ()
  "Get associated Dired buffer at point."
  (get-text-property (point) 'associated-dired-buffer))

(defun image-dired-get-buffer-window (buf)
  "Return window where buffer BUF is."
  (get-window-with-predicate
   (lambda (window)
     (equal (window-buffer window) buf))
   nil t))

(defun image-dired-track-original-file ()
  "Track the original file in the associated Dired buffer.
See documentation for `image-dired-toggle-movement-tracking'.
Interactive use only useful if `image-dired-track-movement' is nil."
  (interactive)
  (let* ((dired-buf (image-dired-associated-dired-buffer))
         (file-name (image-dired-original-file-name))
         (window (image-dired-get-buffer-window dired-buf)))
    (and (buffer-live-p dired-buf) file-name
         (with-current-buffer dired-buf
           (if (not (dired-goto-file file-name))
               (message "Could not track file")
             (if window (set-window-point window (point))))))))

(defun image-dired-toggle-movement-tracking ()
  "Turn on and off `image-dired-track-movement'.
Tracking of the movements between thumbnail and Dired buffer so that
they are \"mirrored\" in the dired buffer.  When this is on, moving
around in the thumbnail or dired buffer will find the matching
position in the other buffer."
  (interactive)
  (setq image-dired-track-movement (not image-dired-track-movement))
  (message "Tracking %s" (if image-dired-track-movement "on" "off")))

(defun image-dired-track-thumbnail ()
  "Track current Dired file's thumb in `image-dired-thumbnail-buffer'.
This is almost the same as what `image-dired-track-original-file' does,
but the other way around."
  (let ((file (dired-get-filename))
        prop-val found window)
    (when (get-buffer image-dired-thumbnail-buffer)
      (with-current-buffer image-dired-thumbnail-buffer
        (goto-char (point-min))
        (while (and (not (eobp))
                    (not found))
          (if (and (setq prop-val
                         (get-text-property (point) 'original-file-name))
                   (string= prop-val file))
              (setq found t))
          (if (not found)
              (forward-char 1)))
        (when found
          (if (setq window (image-dired-thumbnail-window))
              (set-window-point window (point)))
          (image-dired-display-thumb-properties))))))

(defun image-dired-dired-next-line (&optional arg)
  "Call `dired-next-line', then track thumbnail.
This can safely replace `dired-next-line'.
With prefix argument, move ARG lines."
  (interactive "P")
  (dired-next-line (or arg 1))
  (if image-dired-track-movement
      (image-dired-track-thumbnail)))

(defun image-dired-dired-previous-line (&optional arg)
  "Call `dired-previous-line', then track thumbnail.
This can safely replace `dired-previous-line'.
With prefix argument, move ARG lines."
  (interactive "P")
  (dired-previous-line (or arg 1))
  (if image-dired-track-movement
      (image-dired-track-thumbnail)))

(defun image-dired-forward-image (&optional arg)
  "Move to next image and display properties.
Optional prefix ARG says how many images to move; default is one
image."
  (interactive "p")
  (let (pos (steps (or arg 1)))
    (dotimes (_ steps)
      (if (and (not (eobp))
               (save-excursion
                 (forward-char)
                 (while (and (not (eobp))
                             (not (image-dired-image-at-point-p)))
                   (forward-char))
                 (setq pos (point))
                 (image-dired-image-at-point-p)))
          (goto-char pos)
        (error "At last image"))))
  (when image-dired-track-movement
    (image-dired-track-original-file))
  (image-dired-display-thumb-properties))

(defun image-dired-backward-image (&optional arg)
  "Move to previous image and display properties.
Optional prefix ARG says how many images to move; default is one
image."
  (interactive "p")
  (let (pos (steps (or arg 1)))
    (dotimes (_ steps)
      (if (and (not (bobp))
               (save-excursion
                 (backward-char)
                 (while (and (not (bobp))
                             (not (image-dired-image-at-point-p)))
                   (backward-char))
                 (setq pos (point))
                 (image-dired-image-at-point-p)))
          (goto-char pos)
        (error "At first image"))))
  (when image-dired-track-movement
    (image-dired-track-original-file))
  (image-dired-display-thumb-properties))

(defun image-dired-next-line ()
  "Move to next line and display properties."
  (interactive)
  (let ((goal-column (current-column)))
    (forward-line 1)
    (move-to-column goal-column))
  ;; If we end up in an empty spot, back up to the next thumbnail.
  (if (not (image-dired-image-at-point-p))
      (image-dired-backward-image))
  (if image-dired-track-movement
      (image-dired-track-original-file))
  (image-dired-display-thumb-properties))


(defun image-dired-previous-line ()
  "Move to previous line and display properties."
  (interactive)
  (let ((goal-column (current-column)))
    (forward-line -1)
    (move-to-column goal-column))
  ;; If we end up in an empty spot, back up to the next
  ;; thumbnail. This should only happen if the user deleted a
  ;; thumbnail and did not refresh, so it is not very common. But we
  ;; can handle it in a good manner, so why not?
  (if (not (image-dired-image-at-point-p))
      (image-dired-backward-image))
  (if image-dired-track-movement
      (image-dired-track-original-file))
  (image-dired-display-thumb-properties))

(defun image-dired-format-properties-string (buf file props comment)
  "Format display properties.
BUF is the associated Dired buffer, FILE is the original image file
name, PROPS is a stringified list of tags and COMMENT is the image file's
comment."
  (format-spec
   image-dired-display-properties-format
   (list
    (cons ?b (or buf ""))
    (cons ?f file)
    (cons ?t (or props ""))
    (cons ?c (or comment "")))))

(defun image-dired-display-thumb-properties ()
  "Display thumbnail properties in the echo area."
  (if (not (eobp))
      (let ((file-name (file-name-nondirectory (image-dired-original-file-name)))
            (dired-buf (buffer-name (image-dired-associated-dired-buffer)))
            (props (mapconcat #'identity (get-text-property (point) 'tags) ", "))
            (comment (get-text-property (point) 'comment))
            (message-log-max nil))
        (if file-name
             (message "%s"
             (image-dired-format-properties-string
              dired-buf
              file-name
              props
              comment))))))

(defun image-dired-dired-file-marked-p ()
  "Check whether file on current line is marked or not."
  (save-excursion
    (beginning-of-line)
    (looking-at-p dired-re-mark)))

(defun image-dired-modify-mark-on-thumb-original-file (command)
  "Modify mark in Dired buffer.
COMMAND is one of `mark' for marking file in Dired, `unmark' for
unmarking file in Dired or `flag' for flagging file for delete in
Dired."
  (let ((file-name (image-dired-original-file-name))
        (dired-buf (image-dired-associated-dired-buffer)))
    (if (not (and dired-buf file-name))
        (message "No image, or image with correct properties, at point.")
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
          (image-dired-thumb-update-marks))))))

(defun image-dired-mark-thumb-original-file ()
  "Mark original image file in associated Dired buffer."
  (interactive)
  (image-dired-modify-mark-on-thumb-original-file 'mark)
  (image-dired-forward-image))

(defun image-dired-unmark-thumb-original-file ()
  "Unmark original image file in associated Dired buffer."
  (interactive)
  (image-dired-modify-mark-on-thumb-original-file 'unmark)
  (image-dired-forward-image))

(defun image-dired-flag-thumb-original-file ()
  "Flag original image file for deletion in associated Dired buffer."
  (interactive)
  (image-dired-modify-mark-on-thumb-original-file 'flag)
  (image-dired-forward-image))

(defun image-dired-toggle-mark-thumb-original-file ()
  "Toggle mark on original image file in associated Dired buffer."
  (interactive)
  (image-dired-modify-mark-on-thumb-original-file 'toggle))

(defun image-dired-jump-original-dired-buffer ()
  "Jump to the Dired buffer associated with the current image file.
You probably want to use this together with
`image-dired-track-original-file'."
  (interactive)
  (let ((buf (image-dired-associated-dired-buffer))
        window frame)
    (setq window (image-dired-get-buffer-window buf))
    (if window
        (progn
          (if (not (equal (selected-frame) (setq frame (window-frame window))))
              (select-frame-set-input-focus frame))
          (select-window window))
      (message "Associated dired buffer not visible"))))

;;;###autoload
(defun image-dired-jump-thumbnail-buffer ()
  "Jump to thumbnail buffer."
  (interactive)
  (let ((window (image-dired-thumbnail-window))
        frame)
    (if window
        (progn
          (if (not (equal (selected-frame) (setq frame (window-frame window))))
              (select-frame-set-input-focus frame))
          (select-window window))
      (message "Thumbnail buffer not visible"))))

(defvar image-dired-thumbnail-mode-line-up-map
  (let ((map (make-sparse-keymap)))
    ;; map it to "g" so that the user can press it more quickly
    (define-key map "g" 'image-dired-line-up-dynamic)
    ;; "f" for "fixed" number of thumbs per row
    (define-key map "f" 'image-dired-line-up)
    ;; "i" for "interactive"
    (define-key map "i" 'image-dired-line-up-interactive)
    map)
  "Keymap for line-up commands in `image-dired-thumbnail-mode'.")

(defvar image-dired-thumbnail-mode-tag-map
  (let ((map (make-sparse-keymap)))
    ;; map it to "t" so that the user can press it more quickly
    (define-key map "t" 'image-dired-tag-thumbnail)
    ;; "r" for "remove"
    (define-key map "r" 'image-dired-tag-thumbnail-remove)
    map)
  "Keymap for tag commands in `image-dired-thumbnail-mode'.")

(defvar image-dired-thumbnail-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [right] 'image-dired-forward-image)
    (define-key map [left] 'image-dired-backward-image)
    (define-key map [up] 'image-dired-previous-line)
    (define-key map [down] 'image-dired-next-line)
    (define-key map "\C-f" 'image-dired-forward-image)
    (define-key map "\C-b" 'image-dired-backward-image)
    (define-key map "\C-p" 'image-dired-previous-line)
    (define-key map "\C-n" 'image-dired-next-line)

    (define-key map "d" 'image-dired-flag-thumb-original-file)
    (define-key map [delete] 'image-dired-flag-thumb-original-file)
    (define-key map "m" 'image-dired-mark-thumb-original-file)
    (define-key map "u" 'image-dired-unmark-thumb-original-file)
    (define-key map "." 'image-dired-track-original-file)
    (define-key map [tab] 'image-dired-jump-original-dired-buffer)

    ;; add line-up map
    (define-key map "g" image-dired-thumbnail-mode-line-up-map)
    ;; add tag map
    (define-key map "t" image-dired-thumbnail-mode-tag-map)

    (define-key map "\C-m" 'image-dired-display-thumbnail-original-image)
    (define-key map [C-return] 'image-dired-thumbnail-display-external)

    (define-key map "l" 'image-dired-rotate-thumbnail-left)
    (define-key map "r" 'image-dired-rotate-thumbnail-right)
    (define-key map "L" 'image-dired-rotate-original-left)
    (define-key map "R" 'image-dired-rotate-original-right)

    (define-key map "D" 'image-dired-thumbnail-set-image-description)
    (define-key map "\C-d" 'image-dired-delete-char)
    (define-key map " " 'image-dired-display-next-thumbnail-original)
    (define-key map (kbd "DEL") 'image-dired-display-previous-thumbnail-original)
    (define-key map "c" 'image-dired-comment-thumbnail)

    ;; Mouse
    (define-key map [mouse-2] 'image-dired-mouse-display-image)
    (define-key map [mouse-1] 'image-dired-mouse-select-thumbnail)
    ;; Seems I must first set C-down-mouse-1 to undefined, or else it
    ;; will trigger the buffer menu. If I try to instead bind
    ;; C-down-mouse-1 to `image-dired-mouse-toggle-mark', I get a message
    ;; about C-mouse-1 not being defined afterwards. Annoying, but I
    ;; probably do not completely understand mouse events.
    (define-key map [C-down-mouse-1] 'undefined)
    (define-key map [C-mouse-1] 'image-dired-mouse-toggle-mark)

    ;; Menu
    (easy-menu-define nil map
      "Menu for `image-dired-thumbnail-mode'."
      '("Image-Dired"
        ["Display image" image-dired-display-thumbnail-original-image]
        ["Display in external viewer" image-dired-thumbnail-display-external]

        ["Mark original" image-dired-mark-thumb-original-file]
        ["Unmark original" image-dired-unmark-thumb-original-file]
        ["Flag original for deletion" image-dired-flag-thumb-original-file]

        ["Track original" image-dired-track-original-file]
        ["Jump to dired buffer" image-dired-jump-original-dired-buffer]

        ["Toggle movement tracking on/off" image-dired-toggle-movement-tracking]

        ["Rotate original right" image-dired-rotate-original-right]
        ["Rotate original left" image-dired-rotate-original-left]
        ["Rotate thumbnail right" image-dired-rotate-thumbnail-right]
        ["Rotate thumbnail left" image-dired-rotate-thumbnail-left]

        ["Line up thumbnails" image-dired-line-up]
        ["Dynamic line up" image-dired-line-up-dynamic]
        ["Refresh thumb" image-dired-refresh-thumb]
        ["Comment thumbnail" image-dired-comment-thumbnail]
        ["Tag current or marked thumbnails" image-dired-tag-thumbnail]
        ["Remove tag from current or marked thumbnails"
         image-dired-tag-thumbnail-remove]
        ["Delete marked images" image-dired-delete-marked]
        ["Delete thumbnail from buffer" image-dired-delete-char]
        ["Quit" quit-window]))
    map)
  "Keymap for `image-dired-thumbnail-mode'.")

(defvar image-dired-display-image-mode-map
  (let ((map (make-sparse-keymap)))
    ;; `image-mode-map' has bindings that do not make sense in image-dired
    ;; (set-keymap-parent map image-mode-map)
    (define-key map "f" 'image-dired-display-current-image-full)
    (define-key map "s" 'image-dired-display-current-image-sized)
    (define-key map "g" nil)

    ;; Useful bindings from `image-mode-map'
    (define-key map [remap forward-char] 'image-forward-hscroll)
    (define-key map [remap backward-char] 'image-backward-hscroll)
    (define-key map [remap right-char] 'image-forward-hscroll)
    (define-key map [remap left-char] 'image-backward-hscroll)
    (define-key map [remap previous-line] 'image-previous-line)
    (define-key map [remap next-line] 'image-next-line)
    (define-key map [remap scroll-up] 'image-scroll-up)
    (define-key map [remap scroll-down] 'image-scroll-down)
    (define-key map [remap scroll-up-command] 'image-scroll-up)
    (define-key map [remap scroll-down-command] 'image-scroll-down)
    (define-key map [remap scroll-left] 'image-scroll-left)
    (define-key map [remap scroll-right] 'image-scroll-right)
    (define-key map [remap move-beginning-of-line] 'image-bol)
    (define-key map [remap move-end-of-line] 'image-eol)
    (define-key map [remap beginning-of-buffer] 'image-bob)
    (define-key map [remap end-of-buffer] 'image-eob)

    (easy-menu-define nil map
      "Menu for `image-dired-display-image-mode-map'."
      '("Image-Dired"
        ["Display original, full size" image-dired-display-current-image-full]
        ["Display original, sized to fit" image-dired-display-current-image-sized]
        ["Quit" quit-window]))
    map)
  "Keymap for `image-dired-display-image-mode'.")

(defun image-dired-display-current-image-full ()
  "Display current image in full size."
  (interactive)
  (let ((file (image-dired-original-file-name)))
    (if file
        (progn
          (image-dired-display-image file t)
          (message "Full size image displayed"))
      (error "No original file name at point"))))

(defun image-dired-display-current-image-sized ()
  "Display current image in sized to fit window dimensions."
  (interactive)
  (let ((file (image-dired-original-file-name)))
    (if file
        (progn
          (image-dired-display-image file)
          (message "Fitted image displayed"))
      (error "No original file name at point"))))

(define-derived-mode image-dired-thumbnail-mode
  special-mode "image-dired-thumbnail"
  "Browse and manipulate thumbnail images using Dired.
Use `image-dired-minor-mode' to get a nice setup."
  (buffer-disable-undo)
  (add-hook 'file-name-at-point-functions 'image-dired-file-name-at-point nil t))

(define-derived-mode image-dired-display-image-mode
  special-mode "image-dired-image-display"
  "Mode for displaying and manipulating original image.
Resized or in full-size."
  (buffer-disable-undo)
  (image-mode-setup-winprops)
  (setq cursor-type nil)
  (add-hook 'file-name-at-point-functions 'image-dired-file-name-at-point nil t))

(defvar image-dired-minor-mode-map
  (let ((map (make-sparse-keymap)))
    ;; (set-keymap-parent map dired-mode-map)
    ;; Hijack previous and next line movement. Let C-p and C-b be
    ;; though...
    (define-key map "p" 'image-dired-dired-previous-line)
    (define-key map "n" 'image-dired-dired-next-line)
    (define-key map [up] 'image-dired-dired-previous-line)
    (define-key map [down] 'image-dired-dired-next-line)

    (define-key map (kbd "C-S-n") 'image-dired-next-line-and-display)
    (define-key map (kbd "C-S-p") 'image-dired-previous-line-and-display)
    (define-key map (kbd "C-S-m") 'image-dired-mark-and-display-next)

    (define-key map "\C-td" 'image-dired-display-thumbs)
    (define-key map [tab] 'image-dired-jump-thumbnail-buffer)
    (define-key map "\C-ti" 'image-dired-dired-display-image)
    (define-key map "\C-tx" 'image-dired-dired-display-external)
    (define-key map "\C-ta" 'image-dired-display-thumbs-append)
    (define-key map "\C-t." 'image-dired-display-thumb)
    (define-key map "\C-tc" 'image-dired-dired-comment-files)
    (define-key map "\C-tf" 'image-dired-mark-tagged-files)

    ;; Menu for dired
    (easy-menu-define nil map
      "Menu for `image-dired-minor-mode'."
      '("Image-dired"
        ["Display thumb for next file" image-dired-next-line-and-display]
        ["Display thumb for previous file" image-dired-previous-line-and-display]
        ["Mark and display next" image-dired-mark-and-display-next]

        ["Create thumbnails for marked files" image-dired-create-thumbs]

        ["Display thumbnails append" image-dired-display-thumbs-append]
        ["Display this thumbnail" image-dired-display-thumb]
        ["Display image" image-dired-dired-display-image]
        ["Display in external viewer" image-dired-dired-display-external]

        ["Toggle display properties" image-dired-toggle-dired-display-properties]
        ["Toggle append browsing" image-dired-toggle-append-browsing]
        ["Toggle movement tracking" image-dired-toggle-movement-tracking]

        ["Jump to thumbnail buffer" image-dired-jump-thumbnail-buffer]
        ["Mark tagged files" image-dired-mark-tagged-files]
        ["Comment files" image-dired-dired-comment-files]
        ["Copy with EXIF file name" image-dired-copy-with-exif-file-name]))
    map)
  "Keymap for `image-dired-minor-mode'.")

;;;###autoload
(define-minor-mode image-dired-minor-mode
  "Setup easy-to-use keybindings for the commands to be used in Dired mode.
Note that n, p and <down> and <up> will be hijacked and bound to
`image-dired-dired-x-line'."
  :keymap image-dired-minor-mode-map)

;;;###autoload
(define-obsolete-function-alias 'image-dired-setup-dired-keybindings 'image-dired-minor-mode
  "26.1")

(declare-function clear-image-cache "image.c" (&optional filter))

(defun image-dired-create-thumbs (&optional arg)
  "Create thumbnail images for all marked files in Dired.
With prefix argument ARG, create thumbnails even if they already exist
\(i.e. use this to refresh your thumbnails)."
  (interactive "P")
  (let (thumb-name)
    (dolist (curr-file (dired-get-marked-files))
      (setq thumb-name (image-dired-thumb-name curr-file))
      ;; If the user overrides the exist check, we must clear the
      ;; image cache so that if the user wants to display the
      ;; thumbnail, it is not fetched from cache.
      (when arg
        (clear-image-cache (expand-file-name thumb-name)))
      (when (or (not (file-exists-p thumb-name))
                arg)
        (image-dired-create-thumb curr-file thumb-name)))))

(defvar image-dired-slideshow-timer nil
  "Slideshow timer.")

(defvar image-dired-slideshow-count 0
  "Keeping track on number of images in slideshow.")

(defvar image-dired-slideshow-times 0
  "Number of pictures to display in slideshow.")

(defun image-dired-slideshow-step ()
  "Step to next file, if `image-dired-slideshow-times' has not been reached."
  (if (< image-dired-slideshow-count image-dired-slideshow-times)
      (progn
        (message "%s" (1+ image-dired-slideshow-count))
        (setq image-dired-slideshow-count (1+ image-dired-slideshow-count))
        (image-dired-next-line-and-display))
    (image-dired-slideshow-stop)))

(defun image-dired-slideshow-start ()
  "Start slideshow.
Ask user for number of images to show and the delay in between."
  (interactive)
  (setq image-dired-slideshow-count 0)
  (setq image-dired-slideshow-times (string-to-number (read-string "How many: ")))
  (let ((repeat (string-to-number
                 (read-string
                  "Delay, in seconds. Decimals are accepted : " "1"))))
    (setq image-dired-slideshow-timer
          (run-with-timer
           0 repeat
           'image-dired-slideshow-step))))

(defun image-dired-slideshow-stop ()
  "Cancel slideshow."
  (interactive)
  (cancel-timer image-dired-slideshow-timer))

(defun image-dired-delete-char ()
  "Remove current thumbnail from thumbnail buffer and line up."
  (interactive)
  (let ((inhibit-read-only t))
    (delete-char 1)
    (when (= (following-char) ?\s)
      (delete-char 1))))

;;;###autoload
(defun image-dired-display-thumbs-append ()
  "Append thumbnails to `image-dired-thumbnail-buffer'."
  (interactive)
  (image-dired-display-thumbs nil t t))

;;;###autoload
(defun image-dired-display-thumb ()
  "Shorthand for `image-dired-display-thumbs' with prefix argument."
  (interactive)
  (image-dired-display-thumbs t nil t))

(defun image-dired-line-up ()
  "Line up thumbnails according to `image-dired-thumbs-per-row'.
See also `image-dired-line-up-dynamic'."
  (interactive)
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
    (let ((count 0))
      (while (not (eobp))
        (forward-char)
        (if (= image-dired-thumbs-per-row 1)
            (insert "\n")
          (insert " ")
          (setq count (1+ count))
          (when (and (= count (- image-dired-thumbs-per-row 1))
		     (not (eobp)))
            (forward-char)
            (insert "\n")
            (setq count 0)))))
    (goto-char (point-min))))

(defun image-dired-line-up-dynamic ()
  "Line up thumbnails images dynamically.
Calculate how many thumbnails fit."
  (interactive)
  (let* ((char-width (frame-char-width))
        (width (image-dired-window-width-pixels (image-dired-thumbnail-window)))
        (image-dired-thumbs-per-row
         (/ width
            (+ (* 2 image-dired-thumb-relief)
               (* 2 image-dired-thumb-margin)
               (image-dired-thumb-size 'width)
               char-width))))
    (image-dired-line-up)))

(defun image-dired-line-up-interactive ()
  "Line up thumbnails interactively.
Ask user how many thumbnails should be displayed per row."
  (interactive)
  (let ((image-dired-thumbs-per-row
         (string-to-number (read-string "How many thumbs per row: "))))
    (if (not (> image-dired-thumbs-per-row 0))
        (message "Number must be greater than 0")
      (image-dired-line-up))))

(defun image-dired-thumbnail-display-external ()
  "Display original image for thumbnail at point using external viewer."
  (interactive)
  (let ((file (image-dired-original-file-name)))
    (if (not (image-dired-image-at-point-p))
        (message "No thumbnail at point")
      (if (not file)
          (message "No original file name found")
        (start-process "image-dired-thumb-external" nil
                       image-dired-external-viewer file)))))

;;;###autoload
(defun image-dired-dired-display-external ()
  "Display file at point using an external viewer."
  (interactive)
  (let ((file (dired-get-filename)))
    (start-process "image-dired-external" nil
                   image-dired-external-viewer file)))

(defun image-dired-window-width-pixels (window)
  "Calculate WINDOW width in pixels."
    (* (window-width window) (frame-char-width)))

(defun image-dired-window-height-pixels (window)
  "Calculate WINDOW height in pixels."
  ;; Note: The mode-line consumes one line
    (* (- (window-height window) 1) (frame-char-height)))

(defun image-dired-display-window ()
  "Return window where `image-dired-display-image-buffer' is visible."
  (get-window-with-predicate
   (lambda (window)
     (equal (buffer-name (window-buffer window)) image-dired-display-image-buffer))
   nil t))

(defun image-dired-thumbnail-window ()
  "Return window where `image-dired-thumbnail-buffer' is visible."
  (get-window-with-predicate
   (lambda (window)
     (equal (buffer-name (window-buffer window)) image-dired-thumbnail-buffer))
   nil t))

(defun image-dired-associated-dired-buffer-window ()
  "Return window where associated Dired buffer is visible."
  (let (buf)
    (if (image-dired-image-at-point-p)
        (progn
          (setq buf (image-dired-associated-dired-buffer))
          (get-window-with-predicate
           (lambda (window)
             (equal (window-buffer window) buf))))
      (error "No thumbnail image at point"))))

(defun image-dired-display-window-width (window)
  "Return width, in pixels, of WINDOW."
  (- (image-dired-window-width-pixels window)
     image-dired-display-window-width-correction))

(defun image-dired-display-window-height (window)
  "Return height, in pixels, of WINDOW."
  (- (image-dired-window-height-pixels window)
     image-dired-display-window-height-correction))

(defun image-dired-display-image (file &optional original-size)
  "Display image FILE in image buffer.
Use this when you want to display the image, semi sized, in a new
window.  The image is sized to fit the display window (using a
temporary file, don't worry).  Because of this, it will not be as
quick as opening it directly, but on most modern systems it
should feel snappy enough.

If optional argument ORIGINAL-SIZE is non-nil, display image in its
original size."
  (image-dired--check-executable-exists
   'image-dired-cmd-create-temp-image-program)
  (let ((new-file (expand-file-name image-dired-temp-image-file))
        (window (image-dired-display-window))
        (image-type 'jpeg))
    (setq file (expand-file-name file))
    (if (not original-size)
        (let* ((spec
                (list
                 (cons ?p image-dired-cmd-create-temp-image-program)
                 (cons ?w (image-dired-display-window-width window))
                 (cons ?h (image-dired-display-window-height window))
                 (cons ?f file)
                 (cons ?t new-file)))
               (ret
                (apply #'call-process
                       image-dired-cmd-create-temp-image-program nil nil nil
                       (mapcar
                        (lambda (arg) (format-spec arg spec))
                        image-dired-cmd-create-temp-image-options))))
          (when (not (zerop ret))
            (error "Could not resize image")))
      (setq image-type (image-type-from-file-name file))
      (copy-file file new-file t))
    (with-current-buffer (image-dired-create-display-image-buffer)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (clear-image-cache)
        (image-dired-insert-image image-dired-temp-image-file image-type 0 0)
        (goto-char (point-min))
        (set-window-vscroll window 0)
        (set-window-hscroll window 0)
        (image-dired-update-property 'original-file-name file)))))

(defun image-dired-display-thumbnail-original-image (&optional arg)
  "Display current thumbnail's original image in display buffer.
See documentation for `image-dired-display-image' for more information.
With prefix argument ARG, display image in its original size."
  (interactive "P")
  (let ((file (image-dired-original-file-name)))
    (if (not (string-equal major-mode "image-dired-thumbnail-mode"))
        (message "Not in image-dired-thumbnail-mode")
      (if (not (image-dired-image-at-point-p))
          (message "No thumbnail at point")
        (if (not file)
            (message "No original file name found")
	  (image-dired-create-display-image-buffer)
          (display-buffer image-dired-display-image-buffer)
          (image-dired-display-image file arg))))))


;;;###autoload
(defun image-dired-dired-display-image (&optional arg)
  "Display current image file.
See documentation for `image-dired-display-image' for more information.
With prefix argument ARG, display image in its original size."
  (interactive "P")
  (image-dired-create-display-image-buffer)
  (display-buffer image-dired-display-image-buffer)
  (image-dired-display-image (dired-get-filename) arg))

(defun image-dired-image-at-point-p ()
  "Return non-nil if there is an `image-dired' thumbnail at point."
  (get-text-property (point) 'image-dired-thumbnail))

(defun image-dired-rotate-thumbnail (degrees)
  "Rotate thumbnail DEGREES degrees."
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
  (interactive)
  (image-dired-rotate-thumbnail "270"))

(defun image-dired-rotate-thumbnail-right ()
  "Rotate thumbnail counter right (clockwise) 90 degrees."
  (interactive)
  (image-dired-rotate-thumbnail "90"))

(defun image-dired-refresh-thumb ()
  "Force creation of new image for current thumbnail."
  (interactive)
  (let* ((file (image-dired-original-file-name))
         (thumb (expand-file-name (image-dired-thumb-name file))))
    (clear-image-cache (expand-file-name thumb))
    (image-dired-create-thumb file thumb)))

(defun image-dired-rotate-original (degrees)
  "Rotate original image DEGREES degrees."
  (image-dired--check-executable-exists
   'image-dired-cmd-rotate-original-program)
  (if (not (image-dired-image-at-point-p))
      (message "No image at point")
    (let* ((file (image-dired-original-file-name))
           (spec
            (list
             (cons ?d degrees)
             (cons ?o (expand-file-name file))
             (cons ?t image-dired-temp-rotate-image-file))))
      (unless (eq 'jpeg (image-type file))
        (error "Only JPEG images can be rotated!"))
      (if (not (= 0 (apply #'call-process image-dired-cmd-rotate-original-program
                           nil nil nil
                           (mapcar (lambda (arg) (format-spec arg spec))
                                   image-dired-cmd-rotate-original-options))))
          (error "Could not rotate image")
        (image-dired-display-image image-dired-temp-rotate-image-file)
        (if (or (and image-dired-rotate-original-ask-before-overwrite
                     (y-or-n-p
		      "Rotate to temp file OK.  Overwrite original image? "))
                (not image-dired-rotate-original-ask-before-overwrite))
            (progn
              (copy-file image-dired-temp-rotate-image-file file t)
              (image-dired-refresh-thumb))
          (image-dired-display-image file))))))

(defun image-dired-rotate-original-left ()
  "Rotate original image left (counter clockwise) 90 degrees.
The result of the rotation is displayed in the image display area
and a confirmation is needed before the original image files is
overwritten.  This confirmation can be turned off using
`image-dired-rotate-original-ask-before-overwrite'."
  (interactive)
  (image-dired-rotate-original "270"))

(defun image-dired-rotate-original-right ()
  "Rotate original image right (clockwise) 90 degrees.
The result of the rotation is displayed in the image display area
and a confirmation is needed before the original image files is
overwritten.  This confirmation can be turned off using
`image-dired-rotate-original-ask-before-overwrite'."
  (interactive)
  (image-dired-rotate-original "90"))

(defun image-dired-get-exif-file-name (file)
  "Use the image's EXIF information to return a unique file name.
The file name should be unique as long as you do not take more than
one picture per second.  The original file name is suffixed at the end
for traceability.  The format of the returned file name is
YYYY_MM_DD_HH_MM_DD_ORIG_FILE_NAME.jpg.  Used from
`image-dired-copy-with-exif-file-name'."
  (let (data no-exif-data-found)
    (if (not (eq 'jpeg (image-type (expand-file-name file))))
        (setq no-exif-data-found t
              data (format-time-string
                    "%Y:%m:%d %H:%M:%S"
                    (file-attribute-modification-time
                     (file-attributes (expand-file-name file)))))
      (setq data (image-dired-get-exif-data (expand-file-name file)
                                            "DateTimeOriginal")))
    (while (string-match "[ :]" data)
      (setq data (replace-match "_" nil nil data)))
    (format "%s%s%s" data
            (if no-exif-data-found
                "_noexif_"
              "_")
            (file-name-nondirectory file))))

(defun image-dired-thumbnail-set-image-description ()
  "Set the ImageDescription EXIF tag for the original image.
If the image already has a value for this tag, it is used as the
default value at the prompt."
  (interactive)
  (if (not (image-dired-image-at-point-p))
      (message "No thumbnail at point")
    (let* ((file (image-dired-original-file-name))
           (old-value (image-dired-get-exif-data file "ImageDescription")))
      (if (eq 0
              (image-dired-set-exif-data file "ImageDescription"
                                   (read-string "Value of ImageDescription: "
						old-value)))
          (message "Successfully wrote ImageDescription tag.")
        (error "Could not write ImageDescription tag")))))

(defun image-dired-set-exif-data (file tag-name tag-value)
  "In FILE, set EXIF tag TAG-NAME to value TAG-VALUE."
  (image-dired--check-executable-exists
   'image-dired-cmd-write-exif-data-program)
  (let ((spec
         (list
          (cons ?f (expand-file-name file))
          (cons ?t tag-name)
          (cons ?v tag-value))))
    (apply #'call-process image-dired-cmd-write-exif-data-program nil nil nil
           (mapcar (lambda (arg) (format-spec arg spec))
                   image-dired-cmd-write-exif-data-options))))

(defun image-dired-get-exif-data (file tag-name)
  "From FILE, return EXIF tag TAG-NAME."
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

(defun image-dired-copy-with-exif-file-name ()
  "Copy file with unique name to main image directory.
Copy current or all marked files in dired to a new file in your
main image directory, using a file name generated by
`image-dired-get-exif-file-name'.  A typical usage for this if when
copying images from a digital camera into the image directory.

 Typically, you would open up the folder with the incoming
digital images, mark the files to be copied, and execute this
function.  The result is a couple of new files in
`image-dired-main-image-directory' called
2005_05_08_12_52_00_dscn0319.jpg,
2005_05_08_14_27_45_dscn0320.jpg etc."
  (interactive)
  (let (new-name
        (files (dired-get-marked-files)))
    (mapc
     (lambda (curr-file)
       (setq new-name
             (format "%s/%s"
                     (file-name-as-directory
                      (expand-file-name image-dired-main-image-directory))
                     (image-dired-get-exif-file-name curr-file)))
       (message "Copying %s to %s" curr-file new-name)
       (copy-file curr-file new-name))
     files)))

(defun image-dired-display-next-thumbnail-original ()
  "In thumbnail buffer, move to next thumbnail and display the image."
  (interactive)
  (image-dired-forward-image)
  (image-dired-display-thumbnail-original-image))

(defun image-dired-display-previous-thumbnail-original ()
  "Move to previous thumbnail and display image."
  (interactive)
  (image-dired-backward-image)
  (image-dired-display-thumbnail-original-image))

(defun image-dired-write-comments (file-comments)
  "Write file comments to database.
Write file comments to one or more files.
FILE-COMMENTS is an alist on the following form:
 ((FILE . COMMENT) ... )"
  (image-dired-sane-db-file)
  (let (end comment-beg-pos comment-end-pos file comment)
    (image-dired--with-db-file
     (setq buffer-file-name image-dired-db-file)
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
  "Update text property PROP with value VALUE at point."
  (let ((inhibit-read-only t))
    (put-text-property
     (point) (1+ (point))
     prop
     value)))

;;;###autoload
(defun image-dired-dired-comment-files ()
  "Add comment to current or marked files in Dired."
  (interactive)
  (let ((comment (image-dired-read-comment)))
    (image-dired-write-comments
     (mapcar
      (lambda (curr-file)
        (cons curr-file comment))
      (dired-get-marked-files)))))

(defun image-dired-comment-thumbnail ()
  "Add comment to current thumbnail in thumbnail buffer."
  (interactive)
  (let* ((file (image-dired-original-file-name))
         (comment (image-dired-read-comment file)))
    (image-dired-write-comments (list (cons file comment)))
    (image-dired-update-property 'comment comment))
  (image-dired-display-thumb-properties))

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

;;;###autoload
(defun image-dired-mark-tagged-files ()
  "Use regexp to mark files with matching tag.
A `tag' is a keyword, a piece of meta data, associated with an
image file and stored in image-dired's database file.  This command
lets you input a regexp and this will be matched against all tags
on all image files in the database file.  The files that have a
matching tag will be marked in the Dired buffer."
  (interactive)
  (image-dired-sane-db-file)
  (let ((tag (read-string "Mark tagged files (regexp): "))
        (hits 0)
        files)
    (image-dired--with-db-file
     ;; Collect matches
     (while (search-forward-regexp
	     (concat "\\(^[^;\n]+\\);.*" tag ".*$") nil t)
       (push (match-string 1) files)))
    ;; Mark files
    (dolist (curr-file files)
      ;; I tried using `dired-mark-files-regexp' but it was waaaay to
      ;; slow.  Don't bother about hits found in other directories
      ;; than the current one.
      (when (string= (file-name-as-directory
		      (expand-file-name default-directory))
		     (file-name-as-directory
		      (file-name-directory curr-file)))
	(setq curr-file (file-name-nondirectory curr-file))
	(goto-char (point-min))
	(when (search-forward-regexp (format "\\s %s$" curr-file) nil t)
	  (setq hits (+ hits 1))
	  (dired-mark 1))))
    (message "%d files with matching tag marked." hits)))

(defun image-dired-mouse-display-image (event)
  "Use mouse EVENT, call `image-dired-display-image' to display image.
Track this in associated Dired buffer if `image-dired-track-movement' is
non-nil."
  (interactive "e")
  (mouse-set-point event)
  (goto-char (posn-point (event-end event)))
  (let ((file (image-dired-original-file-name)))
    (when file
      (if image-dired-track-movement
	  (image-dired-track-original-file))
      (image-dired-create-display-image-buffer)
      (display-buffer image-dired-display-image-buffer)
      (image-dired-display-image file))))

(defun image-dired-mouse-select-thumbnail (event)
  "Use mouse EVENT to select thumbnail image.
Track this in associated Dired buffer if `image-dired-track-movement' is
non-nil."
  (interactive "e")
  (mouse-set-point event)
  (goto-char (posn-point (event-end event)))
  (if image-dired-track-movement
      (image-dired-track-original-file))
  (image-dired-display-thumb-properties))

(defun image-dired-thumb-file-marked-p ()
  "Check if file is marked in associated Dired buffer."
  (let ((file-name (image-dired-original-file-name))
        (dired-buf (image-dired-associated-dired-buffer)))
    (when (and dired-buf file-name)
      (with-current-buffer dired-buf
        (save-excursion
          (when (dired-goto-file file-name)
            (image-dired-dired-file-marked-p)))))))

(defun image-dired-delete-marked ()
  "Delete current or marked thumbnails and associated images."
  (interactive)
  (image-dired--with-marked
   (image-dired-delete-char)
   (backward-char))
  (image-dired--line-up-with-method)
  (with-current-buffer (image-dired-associated-dired-buffer)
    (dired-do-delete)))

(defun image-dired-thumb-update-marks ()
  "Update the marks in the thumbnail buffer."
  (when image-dired-thumb-visible-marks
    (with-current-buffer image-dired-thumbnail-buffer
      (save-mark-and-excursion
        (goto-char (point-min))
        (let ((inhibit-read-only t))
          (while (not (eobp))
            (with-silent-modifications
              (if (image-dired-thumb-file-marked-p)
                  (add-face-text-property (point) (1+ (point))
                                          'image-dired-thumb-mark)
                (remove-text-properties (point) (1+ (point))
                                        '(face image-dired-thumb-mark))))
            (forward-char)))))))

(defun image-dired-mouse-toggle-mark-1 ()
  "Toggle Dired mark for current thumbnail.
Track this in associated Dired buffer if
`image-dired-track-movement' is non-nil."
  (when image-dired-track-movement
    (image-dired-track-original-file))
  (image-dired-toggle-mark-thumb-original-file))

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
  (image-dired-thumb-update-marks))

(defun image-dired-dired-display-properties ()
  "Display properties for Dired file in the echo area."
  (interactive)
  (let* ((file (dired-get-filename))
         (file-name (file-name-nondirectory file))
         (dired-buf (buffer-name (current-buffer)))
         (props (mapconcat #'identity (image-dired-list-tags file) ", "))
         (comment (image-dired-get-comment file))
         (message-log-max nil))
    (if file-name
        (message "%s"
         (image-dired-format-properties-string
          dired-buf
          file-name
          props
          comment)))))

(defvar image-dired-tag-file-list nil
  "List to store tag-file structure.")

(defvar image-dired-file-tag-list nil
  "List to store file-tag structure.")

(defvar image-dired-file-comment-list nil
  "List to store file comments.")

(defun image-dired-add-to-tag-file-list (tag file)
  "Add relation between TAG and FILE."
  (let (curr)
    (if image-dired-tag-file-list
        (if (setq curr (assoc tag image-dired-tag-file-list))
            (if (not (member file curr))
                (setcdr curr (cons file (cdr curr))))
          (setcdr image-dired-tag-file-list
                  (cons (list tag file) (cdr image-dired-tag-file-list))))
      (setq image-dired-tag-file-list (list (list tag file))))))

(defun image-dired-add-to-tag-file-lists (tag file)
  "Helper function used from `image-dired-create-gallery-lists'.

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

(defun image-dired-add-to-file-comment-list (file comment)
  "Helper function used from `image-dired-create-gallery-lists'.

For FILE, add COMMENT to list.

Lisp structure looks like the following:

image-dired-file-comment-list:

  ((\"filename1\" .  \"comment1\")
   (\"filename2\" .  \"comment2\")
   ...)"
  (if image-dired-file-comment-list
      (if (not (assoc file image-dired-file-comment-list))
          (setcdr image-dired-file-comment-list
                  (cons (cons file comment)
                        (cdr image-dired-file-comment-list))))
    (setq image-dired-file-comment-list (list (cons file comment)))))

(defun image-dired-create-gallery-lists ()
  "Create temporary lists used by `image-dired-gallery-generate'."
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
	 (if (not (string-match "^comment:\\(.*\\)" x))
	     (image-dired-add-to-tag-file-lists x file)
	   (image-dired-add-to-file-comment-list file (match-string 1 x)))))))
  ;; Sort tag-file list
  (setq image-dired-tag-file-list
        (sort image-dired-tag-file-list
              (lambda (x y)
                (string< (car x) (car y))))))

(defun image-dired-hidden-p (file)
  "Return t if image FILE has a \"hidden\" tag."
  (cl-loop for tag in (cdr (assoc file image-dired-file-tag-list))
           if (member tag image-dired-gallery-hidden-tags) return t))

(defun image-dired-gallery-generate ()
  "Generate gallery pages.
First we create a couple of Lisp structures from the database to make
it easier to generate, then HTML-files are created in
`image-dired-gallery-dir'."
  (interactive)
  (if (eq 'per-directory image-dired-thumbnail-storage)
      (error "Currently, gallery generation is not supported \
when using per-directory thumbnail file storage"))
  (image-dired-create-gallery-lists)
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

(defvar image-dired-widget-list nil
  "List to keep track of meta data in edit buffer.")

(declare-function widget-forward "wid-edit" (arg))

;;;###autoload
(defun image-dired-dired-edit-comment-and-tags ()
  "Edit comment and tags of current or marked image files.
Edit comment and tags for all marked image files in an
easy-to-use form."
  (interactive)
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
"\nEdit comments and tags for each image.  Separate multiple tags
with a comma.  Move forward between fields using TAB or RET.
Move to the previous field using backtab (S-TAB).  Save by
activating the Save button at the bottom of the form or cancel
the operation by activating the Cancel button.\n\n")
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
                   (message "Done."))
                 "Save")
    (widget-insert " ")
    (widget-create 'push-button
                   :notify
                   (lambda (&rest _ignore)
                     (bury-buffer)
                     (message "Operation canceled."))
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
;;            ;; Sort function. Compare time between two files.
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

;;;;;;;;;;;;;;;;;;;;;;,

;; (defun dired-speedbar-buttons (dired-buffer)
;;   (when (and (boundp 'image-dired-use-speedbar)
;; 	     image-dired-use-speedbar)
;;     (let ((filename (with-current-buffer dired-buffer
;; 		      (dired-get-filename))))
;;       (when (and (not (string-equal filename (buffer-string)))
;; 		 (string-match (image-file-name-regexp) filename))
;; 	(erase-buffer)
;; 	(insert (propertize
;; 		 filename
;; 		 'display
;; 		 (image-dired-get-thumbnail-image filename)))))))

;; (setq image-dired-use-speedbar t)

(provide 'image-dired)

;;; image-dired.el ends here
