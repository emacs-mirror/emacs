;;; thumbnails.el --- use dired to browse and manipulate your images
;;
;; Copyright (C) 2005, 2006, 2007 Free Software Foundation, Inc.
;;
;; Version: 0.4.11
;; Keywords: multimedia
;; Author: Mathias Dahl <mathias.rem0veth1s.dahl@gmail.com>

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

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
;; was too slow to created thumbnails of all files in a directory (I
;; currently keep all my 2000+ images in the same directory) and
;; browsing the thumbnail buffer was slow too.  thumbnails.el will not
;; create thumbnails until they are needed and the browsing is done
;; quickly and easily in dired.  I copied a great deal of ideas and
;; code from there though... :)
;;
;;  `thumbnails' stores the thumbnail files in `thumbnails-dir' using the file
;; name format ORIGNAME.thumb.ORIGEXT.  For example
;; ~/.emacs.d/thumbnails/myimage01.thumb.jpg.  The "database" is for now
;; just a plain text file with the following format:
;;
;; file-name-non-directory;comment:comment-text;tag1;tag2;tag3;...;tagN
;;
;;
;; PREREQUISITES
;; =============
;;
;; * The ImageMagick package.  Currently, `convert' and `mogrify' are
;; used.  Find it here: http://www.imagemagick.org.
;;
;; * For non-lossy rotation of JPEG images, the JpegTRAN program is
;; needed.
;;
;; * For `thumbnails-get-exif-data' and `thumbnails-write-exif-data' to work,
;; the command line tool `exiftool' is needed.  It can be found here:
;; http://www.sno.phy.queensu.ca/~phil/exiftool/.  These two functions
;; are, among other things, used for writing comments to image files
;; using `thumbnails-thumbnail-set-image-description' and to create
;; "unique" file names using `thumbnails-get-exif-file-name' (used by
;; `thumbnails-copy-with-exif-file-name').
;;
;;
;; USAGE
;; =====
;;
;; This information has been moved to the manual.  Type `C-h r' to open
;; the Emacs manual and go to the node Thumbnails by typing `g
;; Thumbnails RET'.
;;
;; Quickstart: M-x thumbnails RET DIRNAME RET
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
;; backup of `thumbnails-db-file' when testing new versions.
;;
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
;; * Check if exiftool exist before trying to call it to give a better
;; error message.
;;
;; * Investigate if it is possible to also write the tags to the image
;; files.
;;
;; * From thumbs.el: Add an option for clean-up/max-size functionality
;;   for thumbnail directory.
;;
;; * From thumbs.el: Add setroot function.
;;
;; * From thumbs.el: Add image resizing, if useful (thumbnails's automatic
;;  "image fit" might be enough)
;;
;; * From thumbs.el: Add the "modify" commands (emboss, negate,
;;   monochrome etc).
;;
;; * Asynchronous creation of thumbnails.
;;
;; * Add `thumbnails-display-thumbs-ring' and functions to cycle that.  Find
;; out which is best, saving old batch just before inserting new, or
;; saving the current batch in the ring when inserting it.  Adding it
;; probably needs rewriting `thumbnails-display-thumbs' to be more general.
;;
;; * Find some way of toggling on and off really nice keybindings in
;; dired (for example, using C-n or <down> instead of C-S-n).  Richard
;; suggested that we could keep C-t as prefix for thumbnails commands as it
;; is currently not used in dired.  He also suggested that
;; `dired-next-line' and `dired-previous-line' figure out if thumbnails is
;; enabled in the current buffer and, if it is, call
;; `thumbnails-dired-next-line' and `thumbnails-dired-previous-line',
;; respectively.  Update: This is partly done; some bindings have now
;; been added to dired.
;;
;; * Enhanced gallery creation with basic CSS-support and pagination
;; of tag pages with many pictures.
;;
;; * Rewrite `thumbnails-modify-mark-on-thumb-original-file' to be less
;; ugly.
;;
;; * In some way keep track of buffers and windows and stuff so that
;; it works as the user expects.
;;
;; * More/better documentation
;;
;;
;;; Code:

(require 'dired)
(require 'format-spec)
(require 'widget)

(eval-when-compile
  (require 'wid-edit))

(defgroup thumbnails nil
  "Use dired to browse your images as thumbnails, and more."
  :prefix "thumbnails-"
  :group 'multimedia)

(defcustom thumbnails-dir "~/.emacs.d/thumbnails/"
  "Directory where thumbnail images are stored."
  :type 'string
  :group 'thumbnails)

(defcustom thumbnails-thumbnail-storage 'use-thumbnails-dir
  "How to store thumbnails's thumbnail files.
Thumbnails can store thumbnail files in one of two ways and this is
controlled by this variable.  \"Use thumbnails dir\" means that the
thumbnails are stored in a central directory.  \"Per directory\"
means that each thumbnail is stored in a subdirectory called
\".thumbnails\" in the same directory where the image file is.
\"Thumbnail Managing Standard\" means that the thumbnails are
stored and generated according to the Thumbnail Managing Standard
that allows sharing of thumbnails across different programs."
  :type '(choice :tag "How to store thumbnail files"
                 (const :tag "Thumbnail Managing Standard" standard)
                 (const :tag "Use thumbnails-dir" use-thumbnails-dir)
                 (const :tag "Per-directory" per-directory))
  :group 'thumbnails)

(defcustom thumbnails-db-file "~/.emacs.d/thumbnails/.thumbnails_db"
  "Database file where file names and their associated tags are stored."
  :type 'string
  :group 'thumbnails)

(defcustom thumbnails-temp-image-file "~/.emacs.d/thumbnails/.thumbnails_temp"
  "Name of temporary image file used by various commands."
  :type 'string
  :group 'thumbnails)

(defcustom thumbnails-gallery-dir "~/.emacs.d/thumbnails/.thumbnails_gallery"
  "Directory to store generated gallery html pages.
This path needs to be \"shared\" to the public so that it can access
the index.html page that thumbnails creates."
  :type 'string
  :group 'thumbnails)

(defcustom thumbnails-gallery-image-root-url
"http://your.own.server/thumbnailspics"
  "URL where the full size images are to be found.
Note that this path has to be configured in your web server.  Thumbnails
expects to find pictures in this directory."
  :type 'string
  :group 'thumbnails)

(defcustom thumbnails-gallery-thumb-image-root-url
"http://your.own.server/thumbnailsthumbs"
  "URL where the thumbnail images are to be found.
Note that this path has to be configured in your web server.  Thumbnails
expects to find pictures in this directory."
  :type 'string
  :group 'thumbnails)

(defcustom thumbnails-cmd-create-thumbnail-program
  "convert"
  "Executable used to create thumbnail.
Used together with `thumbnails-cmd-create-thumbnail-options'."
  :type 'string
  :group 'thumbnails)

(defcustom thumbnails-cmd-create-thumbnail-options
  "%p -size %wx%h \"%f\" -resize %wx%h +profile \"*\" jpeg:\"%t\""
  "Format of command used to create thumbnail image.
Available options are %p which is replaced by
`thumbnails-cmd-create-thumbnail-program', %w which is replaced by
`thumbnails-thumb-width', %h which is replaced by `thumbnails-thumb-height',
%f which is replaced by the file name of the original image and %t
which is replaced by the file name of the thumbnail file."
  :type 'string
  :group 'thumbnails)

(defcustom thumbnails-cmd-create-temp-image-program
  "convert"
  "Executable used to create temporary image.
Used together with `thumbnails-cmd-create-temp-image-options'."
  :type 'string
  :group 'thumbnails)

(defcustom thumbnails-cmd-create-temp-image-options
  "%p -size %wx%h \"%f\" -resize %wx%h +profile \"*\" jpeg:\"%t\""
  "Format of command used to create temporary image for display window.
Available options are %p which is replaced by
`thumbnails-cmd-create-temp-image-program', %w and %h which is replaced by
the calculated max size for width and height in the image display window,
%f which is replaced by the file name of the original image and %t which
is replaced by the file name of the temporary file."
  :type 'string
  :group 'thumbnails)

(defcustom thumbnails-cmd-pngnq-program (executable-find "pngnq")
  "The file name of the `pngnq' program.
It quantizes colors of PNG images down to 256 colors."
  :type '(choice (const :tag "Not Set" nil) string)
  :group 'thumbnails)

(defcustom thumbnails-cmd-pngcrush-program (executable-find "pngcrush")
  "The file name of the `pngcrush' program.
It optimizes the compression of PNG images.  Also it adds PNG textual chunks
with the information required by the Thumbnail Managing Standard."
  :type '(choice (const :tag "Not Set" nil) string)
  :group 'thumbnails)

(defcustom thumbnails-cmd-create-standard-thumbnail-command
  (concat
   thumbnails-cmd-create-thumbnail-program " "
   "-size %wx%h \"%f\" "
   (unless (or thumbnails-cmd-pngcrush-program thumbnails-cmd-pngnq-program)
     (concat
      "-set \"Thumb::MTime\" \"%m\" "
      "-set \"Thumb::URI\" \"file://%f\" "
      "-set \"Description\" \"Thumbnail of file://%f\" "
      "-set \"Software\" \"" (emacs-version) "\" "))
   "-thumbnail %wx%h png:\"%t\""
   (if thumbnails-cmd-pngnq-program
       (concat
        " ; " thumbnails-cmd-pngnq-program " -f \"%t\""
        (unless thumbnails-cmd-pngcrush-program
          " ; mv %q %t")))
   (if thumbnails-cmd-pngcrush-program
       (concat
        (unless thumbnails-cmd-pngcrush-program
          " ; cp %t %q")
        " ; " thumbnails-cmd-pngcrush-program " -q "
        "-text b \"Description\" \"Thumbnail of file://%f\" "
        "-text b \"Software\" \"" (emacs-version) "\" "
        ;; "-text b \"Thumb::Image::Height\" \"%oh\" "
        ;; "-text b \"Thumb::Image::Mimetype\" \"%mime\" "
        ;; "-text b \"Thumb::Image::Width\" \"%ow\" "
        "-text b \"Thumb::MTime\" \"%m\" "
        ;; "-text b \"Thumb::Size\" \"%b\" "
        "-text b \"Thumb::URI\" \"file://%f\" "
        "%q %t"
        " ; rm %q")))
  "Command to create thumbnails according to the Thumbnail Managing Standard."
  :type 'string
  :group 'thumbnails)

(defcustom thumbnails-cmd-rotate-thumbnail-program
  "mogrify"
  "Executable used to rotate thumbnail.
Used together with `thumbnails-cmd-rotate-thumbnail-options'."
  :type 'string
  :group 'thumbnails)

(defcustom thumbnails-cmd-rotate-thumbnail-options
  "%p -rotate %d \"%t\""
  "Format of command used to rotate thumbnail image.
Available options are %p which is replaced by
`thumbnails-cmd-rotate-thumbnail-program', %d which is replaced by the
number of (positive) degrees to rotate the image, normally 90 or 270
\(for 90 degrees right and left), %t which is replaced by the file name
of the thumbnail file."
  :type 'string
  :group 'thumbnails)

(defcustom thumbnails-cmd-rotate-original-program
  "jpegtran"
  "Executable used to rotate original image.
Used together with `thumbnails-cmd-rotate-original-options'."
  :type 'string
  :group 'thumbnails)

(defcustom thumbnails-cmd-rotate-original-options
  "%p -rotate %d -copy all -outfile %t \"%o\""
  "Format of command used to rotate original image.
Available options are %p which is replaced by
`thumbnails-cmd-rotate-original-program', %d which is replaced by the
number of (positive) degrees to rotate the image, normally 90 or
270 \(for 90 degrees right and left), %o which is replaced by the
original image file name and %t which is replaced by
`thumbnails-temp-image-file'."
  :type 'string
  :group 'thumbnails)

(defcustom thumbnails-temp-rotate-image-file
  "~/.emacs.d/thumbnails/.thumbnails_rotate_temp"
  "Temporary file for rotate operations."
  :type 'string
  :group 'thumbnails)

(defcustom thumbnails-rotate-original-ask-before-overwrite t
  "Confirm overwrite of original file after rotate operation.
If non-nil, ask user for confirmation before overwriting the
original file with `thumbnails-temp-rotate-image-file'."
  :type 'boolean
  :group 'thumbnails)

(defcustom thumbnails-cmd-write-exif-data-program
  "exiftool"
  "Program used to write EXIF data to image.
Used together with `thumbnails-cmd-write-exif-data-options'."
  :type 'string
  :group 'thumbnails)

(defcustom thumbnails-cmd-write-exif-data-options
  "%p -%t=\"%v\" \"%f\""
  "Format of command used to write EXIF data.
Available options are %p which is replaced by
`thumbnails-cmd-write-exif-data-program', %f which is replaced by the
image file name, %t which is replaced by the tag name and %v
which is replaced by the tag value."
  :type 'string
  :group 'thumbnails)

(defcustom thumbnails-cmd-read-exif-data-program
  "exiftool"
  "Program used to read EXIF data to image.
Used together with `thumbnails-cmd-read-exif-data-program-options'."
  :type 'string
  :group 'thumbnails)

(defcustom thumbnails-cmd-read-exif-data-options
  "%p -s -s -s -%t \"%f\""
  "Format of command used to read EXIF data.
Available options are %p which is replaced by
`thumbnails-cmd-write-exif-data-options', %f which is replaced
by the image file name and %t which is replaced by the tag name."
  :type 'string
  :group 'thumbnails)

(defcustom thumbnails-gallery-hidden-tags
  (list "private" "hidden" "pending")
  "List of \"hidden\" tags.
Used by `thumbnails-gallery-generate' to leave out \"hidden\" images."
  :type '(repeat string)
  :group 'thumbnails)

(defcustom thumbnails-thumb-size (if (eq 'standard thumbnails-thumbnail-storage) 128 100)
  "Size of thumbnails, in pixels.
This is the default size for both `thumbnails-thumb-width' and `thumbnails-thumb-height'."
  :type 'integer
  :group 'thumbnails)

(defcustom thumbnails-thumb-width thumbnails-thumb-size
  "Width of thumbnails, in pixels."
  :type 'integer
  :group 'thumbnails)

(defcustom thumbnails-thumb-height thumbnails-thumb-size
  "Height of thumbnails, in pixels."
  :type 'integer
  :group 'thumbnails)

(defcustom thumbnails-thumb-relief 2
  "Size of button-like border around thumbnails."
  :type 'integer
  :group 'thumbnails)

(defcustom thumbnails-thumb-margin 2
  "Size of the margin around thumbnails.
This is where you see the cursor."
  :type 'integer
  :group 'thumbnails)

(defcustom thumbnails-line-up-method 'dynamic
  "Default method for line-up of thumbnails in thumbnail buffer.
Used by `thumbnails-display-thumbs' and other functions that needs to
line-up thumbnails.  Dynamic means to use the available width of the
window containing the thumbnail buffer, Fixed means to use
`thumbnails-thumbs-per-row', Interactive is for asking the user, and No
line-up means that no automatic line-up will be done."
  :type '(choice :tag "Default line-up method"
                 (const :tag "Dynamic" dynamic)
		 (const :tag "Fixed" fixed)
		 (const :tag "Interactive" interactive)
                 (const :tag "No line-up" none))
  :group 'thumbnails)

(defcustom thumbnails-thumbs-per-row 3
  "Number of thumbnails to display per row in thumb buffer."
  :type 'integer
  :group 'thumbnails)

(defcustom thumbnails-display-window-width-correction 1
  "Number to be used to correct image display window width.
Change if the default (1) does not work (i.e. if the image does not
completely fit)."
  :type 'integer
  :group 'thumbnails)

(defcustom thumbnails-display-window-height-correction 0
  "Number to be used to correct image display window height.
Change if the default (0) does not work (i.e. if the image does not
completely fit)."
  :type 'integer
  :group 'thumbnails)

(defcustom thumbnails-track-movement t
  "The current state of the tracking and mirroring.
For more information, see the documentation for
`thumbnails-toggle-movement-tracking'."
  :type 'boolean
  :group 'thumbnails)

(defcustom thumbnails-append-when-browsing nil
  "Append thumbnails in thumbnail buffer when browsing.
If non-nil, using `thumbnails-next-line-and-display' and
`thumbnails-previous-line-and-display' will leave a trail of thumbnail
images in the thumbnail buffer.  If you enable this and want to clean
the thumbnail buffer because it is filled with too many thumbmnails,
just call `thumbnails-display-thumb' to display only the image at point.
This value can be toggled using `thumbnails-toggle-append-browsing'."
  :type 'boolean
  :group 'thumbnails)

(defcustom thumbnails-dired-disp-props t
  "If non-nil, display properties for dired file when browsing.
Used by `thumbnails-next-line-and-display',
`thumbnails-previous-line-and-display' and `thumbnails-mark-and-display-next'.
If the database file is large, this can slow down image browsing in
dired and you might want to turn it off."
  :type 'boolean
  :group 'thumbnails)

(defcustom thumbnails-display-properties-format "%b: %f (%t): %c"
  "Display format for thumbnail properties.
%b is replaced with associated dired buffer name, %f with file name
\(without path) of original image file, %t with the list of tags and %c
with the comment."
  :type 'string
  :group 'thumbnails)

(defcustom thumbnails-external-viewer
  ;; TODO: Use mailcap, dired-guess-shell-alist-default,
  ;; dired-view-command-alist.
  (cond ((executable-find "display"))
        ((executable-find "xli"))
        ((executable-find "qiv") "qiv -t"))
  "Name of external viewer.
Including parameters.  Used when displaying original image from
`thumbnails-thumbnail-mode'."
  :type 'string
  :group 'thumbnails)

(defcustom thumbnails-main-image-directory "~/pics/"
  "Name of main image directory, if any.
Used by `thumbnails-copy-with-exif-file-name'."
  :type 'string
  :group 'thumbnails)

(defcustom thumbnails-show-all-from-dir-max-files 50
  "Maximum number of files to show using `thumbnails-show-all-from-dir'.
before warning the user."
  :type 'integer
  :group 'thumbnails)

(defun thumbnails-dir ()
  "Return the current thumbnails directory (from variable `thumbnails-dir').
Create the thumbnails directory if it does not exist."
  (let ((thumbnails-dir (file-name-as-directory
                    (expand-file-name thumbnails-dir))))
    (unless (file-directory-p thumbnails-dir)
      (make-directory thumbnails-dir t)
      (message "Creating thumbnails directory"))
    thumbnails-dir))

(defun thumbnails-insert-image (file type relief margin)
  "Insert image FILE of image TYPE, using RELIEF and MARGIN, at point."

  (let ((i `(image :type ,type
                   :file ,file
                   :relief ,relief
                   :margin ,margin)))
    (insert-image i)))

(defun thumbnails-get-thumbnail-image (file)
  "Return the image descriptor for a thumbnail of image file FILE."
  (unless (string-match (image-file-name-regexp) file)
    (error "%s is not a valid image file" file))
  (let ((thumb-file (thumbnails-thumb-name file)))
    (unless (and (file-exists-p thumb-file)
		 (<= (float-time (nth 5 (file-attributes file)))
		     (float-time (nth 5 (file-attributes thumb-file)))))
      (thumbnails-create-thumb file thumb-file))
    (create-image thumb-file)
;;     (list 'image :type 'jpeg
;;           :file thumb-file
;; 	  :relief thumbnails-thumb-relief :margin thumbnails-thumb-margin)
    ))

(defun thumbnails-insert-thumbnail (file original-file-name
                                    associated-dired-buffer)
  "Insert thumbnail image FILE.
Add text properties ORIGINAL-FILE-NAME and ASSOCIATED-DIRED-BUFFER."
  (let (beg end)
    (setq beg (point))
    (thumbnails-insert-image file
                        ;; TODO: this should depend on the real file type
                        (if (eq 'standard thumbnails-thumbnail-storage)
                            'png 'jpeg)
                        thumbnails-thumb-relief
                        thumbnails-thumb-margin)
    (setq end (point))
    (add-text-properties
     beg end
     (list 'thumbnails-thumbnail t
           'original-file-name original-file-name
           'associated-dired-buffer associated-dired-buffer
           'tags (thumbnails-list-tags original-file-name)
           'mouse-face 'highlight
           'comment (thumbnails-get-comment original-file-name)))))

(defun thumbnails-thumb-name (file)
  "Return thumbnail file name for FILE.
Depending on the value of `thumbnails-thumbnail-storage', the file
name will vary.  For central thumbnail file storage, make a
MD5-hash of the image file's directory name and add that to make
the thumbnail file name unique.  For per-directory storage, just
add a subdirectory.  For standard storage, produce the file name
according to the Thumbnail Managing Standard."
  (cond ((eq 'standard thumbnails-thumbnail-storage)
         (expand-file-name
          (concat "~/.thumbnails/normal/"
                  (md5 (concat "file://" (expand-file-name file))) ".png")))
        ((eq 'use-thumbnails-dir thumbnails-thumbnail-storage)
         (let* ((f (expand-file-name file))
                (md5-hash
                 ;; Is MD5 hashes fast enough? The checksum of a
                 ;; thumbnail file name need not be that
                 ;; "cryptographically" good so a faster one could
                 ;; be used here.
                 (md5 (file-name-as-directory (file-name-directory f)))))
           (format "%s%s%s.thumb.%s"
                   (file-name-as-directory (expand-file-name (thumbnails-dir)))
                   (file-name-sans-extension (file-name-nondirectory f))
                   (if md5-hash (concat "_" md5-hash) "")
                   (file-name-extension f))))
        ((eq 'per-directory thumbnails-thumbnail-storage)
         (let ((f (expand-file-name file)))
           (format "%s.thumbnails/%s.thumb.%s"
                   (file-name-directory f)
                   (file-name-sans-extension (file-name-nondirectory f))
                   (file-name-extension f))))))

(defun thumbnails-create-thumb (original-file thumbnail-file)
  "For ORIGINAL-FILE, create thumbnail image named THUMBNAIL-FILE."
  (let* ((width (int-to-string thumbnails-thumb-width))
         (height (int-to-string thumbnails-thumb-height))
         (modif-time (format "%.0f" (float-time (nth 5 (file-attributes
                                                        original-file)))))
         (thumbnail-nq8-file (replace-regexp-in-string ".png\\'" "-nq8.png"
                                                       thumbnail-file))
         (command
          (format-spec
           (if (eq 'standard thumbnails-thumbnail-storage)
               thumbnails-cmd-create-standard-thumbnail-command
             thumbnails-cmd-create-thumbnail-options)
           (list
            (cons ?p thumbnails-cmd-create-thumbnail-program)
            (cons ?w width)
            (cons ?h height)
            (cons ?m modif-time)
            (cons ?f original-file)
            (cons ?q thumbnail-nq8-file)
            (cons ?t thumbnail-file))))
         thumbnail-dir)
    (when (not (file-exists-p
                (setq thumbnail-dir (file-name-directory thumbnail-file))))
      (message "Creating thumbnail directory.")
      (make-directory thumbnail-dir))
    (call-process shell-file-name nil nil nil shell-command-switch command)))

;;;###autoload
(defun thumbnails-dired-insert-marked-thumbs ()
  "Insert thumbnails before file names of marked files in the dired buffer."
  (interactive)
  (dired-map-over-marks
   (let* ((image-pos (dired-move-to-filename))
          (image-file (dired-get-filename))
          (thumb-file (thumbnails-get-thumbnail-image image-file))
          overlay)
     ;; If image is not already added, then add it.
     (unless (delq nil (mapcar (lambda (o) (overlay-get o 'put-image))
                               ;; Can't use (overlays-at (point)), BUG?
                               (overlays-in (point) (1+ (point)))))
       (put-image thumb-file image-pos)
       (setq
	overlay
	(car (delq nil (mapcar (lambda (o) (and (overlay-get o 'put-image) o))
			       (overlays-in (point) (1+ (point)))))))
       (overlay-put overlay 'image-file image-file)
       (overlay-put overlay 'thumb-file thumb-file)))
   nil)
  (add-hook 'dired-after-readin-hook 'thumbnails-dired-after-readin-hook nil t))

(defun thumbnails-dired-after-readin-hook ()
  "Relocate existing thumbnail overlays in dired buffer after reverting.
Move them to their corresponding files if they are still exist.
Otherwise, delete overlays."
  (mapc (lambda (overlay)
          (when (overlay-get overlay 'put-image)
            (let* ((image-file (overlay-get overlay 'image-file))
                   (image-pos (dired-goto-file image-file)))
              (if image-pos
                  (move-overlay overlay image-pos image-pos)
                (delete-overlay overlay)))))
        (overlays-in (point-min) (point-max))))

(defun thumbnails-next-line-and-display ()
  "Move to next dired line and display thumbnail image."
  (interactive)
  (dired-next-line 1)
  (thumbnails-display-thumbs
   t (or thumbnails-append-when-browsing nil) t)
  (if thumbnails-dired-disp-props
      (thumbnails-dired-display-properties)))

(defun thumbnails-previous-line-and-display ()
  "Move to previous dired line and display thumbnail image."
  (interactive)
  (dired-previous-line 1)
  (thumbnails-display-thumbs
   t (or thumbnails-append-when-browsing nil) t)
  (if thumbnails-dired-disp-props
      (thumbnails-dired-display-properties)))

(defun thumbnails-toggle-append-browsing ()
  "Toggle `thumbnails-append-when-browsing'."
  (interactive)
  (setq thumbnails-append-when-browsing
        (not thumbnails-append-when-browsing))
  (message "Append browsing %s."
           (if thumbnails-append-when-browsing
               "on"
             "off")))

(defun thumbnails-mark-and-display-next ()
  "Mark current file in dired and display next thumbnail image."
  (interactive)
  (dired-mark 1)
  (thumbnails-display-thumbs
   t (or thumbnails-append-when-browsing nil) t)
  (if thumbnails-dired-disp-props
      (thumbnails-dired-display-properties)))

(defun thumbnails-toggle-dired-display-properties ()
  "Toggle `thumbnails-dired-disp-props'."
  (interactive)
  (setq thumbnails-dired-disp-props
        (not thumbnails-dired-disp-props))
  (message "Dired display properties %s."
           (if thumbnails-dired-disp-props
               "on"
             "off")))

(defvar thumbnails-thumbnail-buffer "*thumbnails*"
  "Thumbnails's thumbnail buffer.")

(defun thumbnails-create-thumbnail-buffer ()
  "Create thumb buffer and set `thumbnails-thumbnail-mode'."
  (let ((buf (get-buffer-create thumbnails-thumbnail-buffer)))
    (with-current-buffer buf
      (setq buffer-read-only t)
      (if (not (eq major-mode 'thumbnails-thumbnail-mode))
          (thumbnails-thumbnail-mode)))
    buf))

(defvar thumbnails-display-image-buffer "*thumbnails-display-image*"
  "Where larger versions of the images are display.")

(defun thumbnails-create-display-image-buffer ()
  "Create image display buffer and set `thumbnails-display-image-mode'."
  (let ((buf (get-buffer-create thumbnails-display-image-buffer)))
    (with-current-buffer buf
      (setq buffer-read-only t)
      (if (not (eq major-mode 'thumbnails-display-image-mode))
          (thumbnails-display-image-mode)))
    buf))

(defvar thumbnails-saved-window-configuration nil
  "Saved window configuration.")

;;;###autoload
(defun thumbnails-dired-with-window-configuration (dir &optional arg)
  "Open directory DIR and create a default window configuration.

Convenience command that:

 - Opens dired in folder DIR
 - Splits windows in most useful (?) way
 - Set `truncate-lines' to t

After the command has finished, you would typically mark some
image files in dired and type
\\[thumbnails-display-thumbs] (`thumbnails-display-thumbs').

If called with prefix argument ARG, skip splitting of windows.

The current window configuration is saved and can be restored by
calling `thumbnails-restore-window-configuration'."
  (interactive "DDirectory: \nP")
  (let ((buf (thumbnails-create-thumbnail-buffer))
        (buf2 (thumbnails-create-display-image-buffer)))
    (setq thumbnails-saved-window-configuration
          (current-window-configuration))
    (dired dir)
    (delete-other-windows)
    (when (not arg)
      (split-window-horizontally)
      (setq truncate-lines t)
      (save-excursion
        (other-window 1)
        (switch-to-buffer buf)
        (split-window-vertically)
        (other-window 1)
        (switch-to-buffer buf2)
        (other-window -2)))))

(defun thumbnails-restore-window-configuration ()
  "Restore window configuration.
Restore any changes to the window configuration made by calling
`thumbnails-dired-with-window-configuration'."
  (interactive)
  (if thumbnails-saved-window-configuration
      (set-window-configuration thumbnails-saved-window-configuration)
    (message "No saved window configuration")))

;;;###autoload
(defun thumbnails-display-thumbs (&optional arg append do-not-pop)
  "Display thumbnails of all marked files, in `thumbnails-thumbnail-buffer'.
If a thumbnail image does not exist for a file, it is created on the
fly.  With prefix argument ARG, display only thumbnail for file at
point (this is useful if you have marked some files but want to show
another one).

Recommended usage is to split the current frame horizontally so that
you have the dired buffer in the left window and the
`thumbnails-thumbnail-buffer' buffer in the right window.

With optional argument APPEND, append thumbnail to thumbnail buffer
instead of erasing it first.

Option argument DO-NOT-POP controls if `pop-to-buffer' should be
used or not.  If non-nil, use `display-buffer' instead of
`pop-to-buffer'.  This is used from functions like
`thumbnails-next-line-and-display' and
`thumbnails-previous-line-and-display' where we do not want the
thumbnail buffer to be selected."
  (interactive "P")
  (let ((buf (thumbnails-create-thumbnail-buffer))
        curr-file thumb-name files count dired-buf beg)
    (if arg
        (setq files (list (dired-get-filename)))
      (setq files (dired-get-marked-files)))
    (setq dired-buf (current-buffer))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (if (not append)
            (erase-buffer)
          (goto-char (point-max)))
        (mapcar
         (lambda (curr-file)
           (setq thumb-name (thumbnails-thumb-name curr-file))
           (if (and (not (file-exists-p thumb-name))
                    (not (= 0 (thumbnails-create-thumb curr-file thumb-name))))
               (message "Thumb could not be created for file %s" curr-file)
             (thumbnails-insert-thumbnail thumb-name curr-file dired-buf)))
         files))
      (cond ((eq 'dynamic thumbnails-line-up-method)
             (thumbnails-line-up-dynamic))
            ((eq 'fixed thumbnails-line-up-method)
             (thumbnails-line-up))
            ((eq 'interactive thumbnails-line-up-method)
             (thumbnails-line-up-interactive))
            ((eq 'none thumbnails-line-up-method)
             nil)
            (t
             (thumbnails-line-up-dynamic))))
    (if do-not-pop
        (display-buffer thumbnails-thumbnail-buffer)
      (pop-to-buffer thumbnails-thumbnail-buffer))))

;;;###autoload
(defun thumbnails-show-all-from-dir (dir)
  "Make a preview buffer for all images in DIR and display it.
If the number of files in DIR matching `image-file-name-regexp'
exceeds `thumbnails-show-all-from-dir-max-files', a warning will be
displayed."
  (interactive "DDir: ")
  (dired dir)
  (dired-mark-files-regexp (image-file-name-regexp))
  (let ((files (dired-get-marked-files)))
    (if (or (<= (length files) thumbnails-show-all-from-dir-max-files)
            (and (> (length files) thumbnails-show-all-from-dir-max-files)
                 (y-or-n-p
                  (format
                   "Directory contains more than %d image files.  Proceed? "
                   thumbnails-show-all-from-dir-max-files))))
        (progn
          (thumbnails-display-thumbs)
          (pop-to-buffer thumbnails-thumbnail-buffer))
      (message "Cancelled."))))

;;;###autoload
(defalias 'thumbnails 'thumbnails-show-all-from-dir)

;;;###autoload
(defalias 'tumme 'thumbnails-show-all-from-dir)

(defun thumbnails-write-tags (file-tags)
  "Write file tags to database.
Write each file and tag in FILE-TAGS to the database.  FILE-TAGS
is an alist in the following form:
 ((FILE . TAG) ... )"
  (let (end file tag)
    (with-temp-file thumbnails-db-file
      (insert-file-contents thumbnails-db-file)
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
	  (insert (format "\n%s;%s" file tag)))))))

(defun thumbnails-remove-tag (files tag)
  "For all FILES, remove TAG from the image database."
  (save-excursion
    (let (end buf start)
      (setq buf (find-file thumbnails-db-file))
      (if (not (listp files))
          (if (stringp files)
              (setq files (list files))
            (error "Files must be a string or a list of strings!")))
      (mapcar
       (lambda (file)
         (goto-char (point-min))
         (when (search-forward-regexp
                (format "^%s" file) nil t)
           (end-of-line)
           (setq end (point))
           (beginning-of-line)
           (when (search-forward-regexp (format "\\(;%s\\)" tag) end t)
             (delete-region (match-beginning 1) (match-end 1))
             ;; Check if file should still be in the database. If
             ;; it has no tags or comments, it will be removed.
             (end-of-line)
             (setq end (point))
             (beginning-of-line)
             (when (not (search-forward ";" end t))
               (kill-line 1)
               ;; If on empty line at end of buffer
               (when (and (eobp)
                          (looking-at "^$"))
                 (delete-backward-char 1))))))
       files)
      (save-buffer)
      (kill-buffer buf))))

(defun thumbnails-list-tags (file)
  "Read all tags for image FILE from the image database."
  (save-excursion
    (let (end buf (tags ""))
      (setq buf (find-file thumbnails-db-file))
      (goto-char (point-min))
      (when (search-forward-regexp
             (format "^%s" file) nil t)
        (end-of-line)
        (setq end (point))
        (beginning-of-line)
        (if (search-forward ";" end t)
            (if (search-forward "comment:" end t)
                (if (search-forward ";" end t)
                    (setq tags (buffer-substring (point) end)))
              (setq tags (buffer-substring (point) end)))))
      (kill-buffer buf)
      (split-string tags ";"))))

;;;###autoload
(defun thumbnails-tag-files (arg)
  "Tag marked file(s) in dired.  With prefix ARG, tag file at point."
  (interactive "P")
  (let ((tag (read-string "Tags to add (separate tags with a semicolon): "))
        curr-file files)
    (if arg
        (setq files (list (dired-get-filename)))
      (setq files (dired-get-marked-files)))
    (thumbnails-write-tags
     (mapcar
      (lambda (x)
        (cons x tag))
      files))))

(defun thumbnails-tag-thumbnail ()
  "Tag current thumbnail."
  (interactive)
  (let ((tag (read-string "Tags to add (separate tags with a semicolon): ")))
    (thumbnails-write-tags (list (cons (thumbnails-original-file-name) tag))))
  (thumbnails-update-property
   'tags (thumbnails-list-tags (thumbnails-original-file-name))))

;;;###autoload
(defun thumbnails-delete-tag (arg)
  "Remove tag for selected file(s).
With prefix argument ARG, remove tag from file at point."
  (interactive "P")
  (let ((tag (read-string "Tag to remove: "))
        files)
    (if arg
        (setq files (list (dired-get-filename)))
      (setq files (dired-get-marked-files)))
    (thumbnails-remove-tag files tag)))

(defun thumbnails-tag-thumbnail-remove ()
  "Remove tag from thumbnail."
  (interactive)
  (let ((tag (read-string "Tag to remove: ")))
    (thumbnails-remove-tag (thumbnails-original-file-name) tag))
  (thumbnails-update-property
   'tags (thumbnails-list-tags (thumbnails-original-file-name))))

(defun thumbnails-original-file-name ()
  "Get original file name for thumbnail or display image at point."
  (get-text-property (point) 'original-file-name))

(defun thumbnails-associated-dired-buffer ()
  "Get associated dired buffer at point."
  (get-text-property (point) 'associated-dired-buffer))

(defun thumbnails-get-buffer-window (buf)
  "Return window where buffer BUF is."
  (get-window-with-predicate
   (lambda (window)
     (equal (window-buffer window) buf))
   nil t))

(defun thumbnails-track-original-file ()
  "Track the original file in the associated dired buffer.
See documentation for `thumbnails-toggle-movement-tracking'.  Interactive
use only useful if `thumbnails-track-movement' is nil."
  (interactive)
  (let ((old-buf (current-buffer))
        (dired-buf (thumbnails-associated-dired-buffer))
        (file-name (thumbnails-original-file-name)))
    (when (and (buffer-live-p dired-buf) file-name)
      (setq file-name (file-name-nondirectory file-name))
      (set-buffer dired-buf)
      (goto-char (point-min))
      (if (not (search-forward file-name nil t))
          (message "Could not track file")
        (dired-move-to-filename)
        (set-window-point
         (thumbnails-get-buffer-window dired-buf) (point)))
      (set-buffer old-buf))))

(defun thumbnails-toggle-movement-tracking ()
  "Turn on and off `thumbnails-track-movement'.
Tracking of the movements between thumbnail and dired buffer so that
they are \"mirrored\" in the dired buffer.  When this is on, moving
around in the thumbnail or dired buffer will find the matching
position in the other buffer."
  (interactive)
  (setq thumbnails-track-movement (not thumbnails-track-movement))
  (message "Tracking %s" (if thumbnails-track-movement "on" "off")))

(defun thumbnails-track-thumbnail ()
  "Track current dired file's thumb in `thumbnails-thumbnail-buffer'.
This is almost the same as what `thumbnails-track-original-file' does, but
the other way around."
  (let ((file (dired-get-filename))
        (old-buf (current-buffer))
        prop-val found)
    (when (get-buffer thumbnails-thumbnail-buffer)
      (set-buffer thumbnails-thumbnail-buffer)
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
        (set-window-point
         (thumbnails-thumbnail-window) (point))
        (thumbnails-display-thumb-properties))
      (set-buffer old-buf))))

(defun thumbnails-dired-next-line (&optional arg)
  "Call `dired-next-line', then track thumbnail.
This can safely replace `dired-next-line'.  With prefix argument, move
ARG lines."
  (interactive "P")
  (dired-next-line (or arg 1))
  (if thumbnails-track-movement
      (thumbnails-track-thumbnail)))

(defun thumbnails-dired-previous-line (&optional arg)
  "Call `dired-previous-line', then track thumbnail.
This can safely replace `dired-previous-line'.  With prefix argument,
move ARG lines."
  (interactive "P")
  (dired-previous-line (or arg 1))
  (if thumbnails-track-movement
      (thumbnails-track-thumbnail)))

(defun thumbnails-forward-image (&optional arg)
  "Move to next image and display properties.
Optional prefix ARG says how many images to move; default is one
image."
  (interactive "p")
  (let (pos (steps (or arg 1)))
    (dotimes (i steps)
      (if (and (not (eobp))
               (save-excursion
                 (forward-char)
                 (while (and (not (eobp))
                             (not (thumbnails-image-at-point-p)))
                   (forward-char))
                 (setq pos (point))
                 (thumbnails-image-at-point-p)))
          (goto-char pos)
        (error "At last image"))))
  (when thumbnails-track-movement
    (thumbnails-track-original-file))
  (thumbnails-display-thumb-properties))

(defun thumbnails-backward-image (&optional arg)
  "Move to previous image and display properties.
Optional prefix ARG says how many images to move; default is one
image."
  (interactive "p")
  (let (pos (steps (or arg 1)))
    (dotimes (i steps)
      (if (and (not (bobp))
               (save-excursion
                 (backward-char)
                 (while (and (not (bobp))
                             (not (thumbnails-image-at-point-p)))
                   (backward-char))
                 (setq pos (point))
                 (thumbnails-image-at-point-p)))
          (goto-char pos)
        (error "At first image"))))
  (when thumbnails-track-movement
    (thumbnails-track-original-file))
  (thumbnails-display-thumb-properties))

(defun thumbnails-next-line ()
  "Move to next line and display properties."
  (interactive)
  (next-line 1)
  ;; If we end up in an empty spot, back up to the next thumbnail.
  (if (not (thumbnails-image-at-point-p))
      (thumbnails-backward-image))
  (if thumbnails-track-movement
      (thumbnails-track-original-file))
  (thumbnails-display-thumb-properties))


(defun thumbnails-previous-line ()
  "Move to previous line and display properties."
  (interactive)
  (previous-line 1)
  ;; If we end up in an empty spot, back up to the next
  ;; thumbnail. This should only happen if the user deleted a
  ;; thumbnail and did not refresh, so it is not very common. But we
  ;; can handle it in a good manner, so why not?
  (if (not (thumbnails-image-at-point-p))
      (thumbnails-backward-image))
  (if thumbnails-track-movement
      (thumbnails-track-original-file))
  (thumbnails-display-thumb-properties))

(defun thumbnails-format-properties-string (buf file props comment)
  "Format display properties.
BUF is the associated dired buffer, FILE is the original image file
name, PROPS is a list of tags and COMMENT is the image files's
comment."
  (format-spec
   thumbnails-display-properties-format
   (list
    (cons ?b (or buf ""))
    (cons ?f file)
    (cons ?t (or (princ props) ""))
    (cons ?c (or comment "")))))

(defun thumbnails-display-thumb-properties ()
  "Display thumbnail properties in the echo area."
  (if (not (eobp))
      (let ((file-name (file-name-nondirectory (thumbnails-original-file-name)))
            (dired-buf (buffer-name (thumbnails-associated-dired-buffer)))
            (props (mapconcat
                    'princ
                    (get-text-property (point) 'tags)
                    ", "))
            (comment (get-text-property (point) 'comment)))
        (if file-name
            (message
             (thumbnails-format-properties-string
              dired-buf
              file-name
              props
              comment))))))

(defun thumbnails-dired-file-marked-p ()
  "Check whether file on current line is marked or not."
  (save-excursion
    (beginning-of-line)
    (not (looking-at "^ .*$"))))

(defun thumbnails-modify-mark-on-thumb-original-file (command)
  "Modify mark in dired buffer.
This is quite ugly but I don't know how to implemented in a better
way.  COMMAND is one of 'mark for marking file in dired, 'unmark for
unmarking file in dired or 'flag for flagging file for delete in
dired."
  (let ((file-name (thumbnails-original-file-name))
        (dired-buf (thumbnails-associated-dired-buffer)))
    (if (not (and dired-buf file-name))
        (message "No image, or image with correct properties, at point.")
    (with-current-buffer dired-buf
        (message file-name)
        (setq file-name (file-name-nondirectory file-name))
        (goto-char (point-min))
        (if (search-forward file-name nil t)
            (cond ((eq command 'mark) (dired-mark 1))
                  ((eq command 'unmark) (dired-unmark 1))
                  ((eq command 'toggle)
                   (if (thumbnails-dired-file-marked-p)
                       (dired-unmark 1)
                     (dired-mark 1)))
                  ((eq command 'flag) (dired-flag-file-deletion 1))))))))

(defun thumbnails-mark-thumb-original-file ()
  "Mark original image file in associated dired buffer."
  (interactive)
  (thumbnails-modify-mark-on-thumb-original-file 'mark)
  (thumbnails-forward-image))

(defun thumbnails-unmark-thumb-original-file ()
  "Unmark original image file in associated dired buffer."
  (interactive)
  (thumbnails-modify-mark-on-thumb-original-file 'unmark)
  (thumbnails-forward-image))

(defun thumbnails-flag-thumb-original-file ()
  "Flag original image file for deletion in associated dired buffer."
  (interactive)
  (thumbnails-modify-mark-on-thumb-original-file 'flag)
  (thumbnails-forward-image))

(defun thumbnails-toggle-mark-thumb-original-file ()
  "Toggle mark on original image file in associated dired buffer."
  (interactive)
  (thumbnails-modify-mark-on-thumb-original-file 'toggle))

(defun thumbnails-jump-original-dired-buffer ()
  "Jump to the dired buffer associated with the current image file.
You probably want to use this together with
`thumbnails-track-original-file'."
  (interactive)
  (let ((buf (thumbnails-associated-dired-buffer))
        window frame)
    (setq window (thumbnails-get-buffer-window buf))
    (if window
        (progn
          (if (not (equal (selected-frame) (setq frame (window-frame window))))
              (select-frame-set-input-focus frame))
          (select-window window))
      (message "Associated dired buffer not visible"))))

;;;###autoload
(defun thumbnails-jump-thumbnail-buffer ()
  "Jump to thumbnail buffer."
  (interactive)
  (let ((window (thumbnails-thumbnail-window))
        frame)
    (if window
        (progn
          (if (not (equal (selected-frame) (setq frame (window-frame window))))
              (select-frame-set-input-focus frame))
          (select-window window))
      (message "Thumbnail buffer not visible"))))

(defvar thumbnails-thumbnail-mode-map (make-sparse-keymap)
  "Keymap for `thumbnails-thumbnail-mode'.")

(defvar thumbnails-thumbnail-mode-line-up-map (make-sparse-keymap)
  "Keymap for line-up commands in `thumbnails-thumbnail-mode'.")

(defvar thumbnails-thumbnail-mode-tag-map (make-sparse-keymap)
  "Keymap for tag commands in `thumbnails-thumbnail-mode'.")

(defun thumbnails-define-thumbnail-mode-keymap ()
  "Define keymap for `thumbnails-thumbnail-mode'."

  ;; Keys
  (define-key thumbnails-thumbnail-mode-map [right] 'thumbnails-forward-image)
  (define-key thumbnails-thumbnail-mode-map [left] 'thumbnails-backward-image)
  (define-key thumbnails-thumbnail-mode-map [up] 'thumbnails-previous-line)
  (define-key thumbnails-thumbnail-mode-map [down] 'thumbnails-next-line)
  (define-key thumbnails-thumbnail-mode-map "\C-f" 'thumbnails-forward-image)
  (define-key thumbnails-thumbnail-mode-map "\C-b" 'thumbnails-backward-image)
  (define-key thumbnails-thumbnail-mode-map "\C-p" 'thumbnails-previous-line)
  (define-key thumbnails-thumbnail-mode-map "\C-n" 'thumbnails-next-line)

  (define-key thumbnails-thumbnail-mode-map "d" 'thumbnails-flag-thumb-original-file)
  (define-key thumbnails-thumbnail-mode-map [delete]
    'thumbnails-flag-thumb-original-file)
  (define-key thumbnails-thumbnail-mode-map "m" 'thumbnails-mark-thumb-original-file)
  (define-key thumbnails-thumbnail-mode-map "u" 'thumbnails-unmark-thumb-original-file)
  (define-key thumbnails-thumbnail-mode-map "." 'thumbnails-track-original-file)
  (define-key thumbnails-thumbnail-mode-map [tab] 'thumbnails-jump-original-dired-buffer)

  ;; add line-up map
  (define-key thumbnails-thumbnail-mode-map "g" thumbnails-thumbnail-mode-line-up-map)

  ;; map it to "g" so that the user can press it more quickly
  (define-key thumbnails-thumbnail-mode-line-up-map "g" 'thumbnails-line-up-dynamic)
  ;; "f" for "fixed" number of thumbs per row
  (define-key thumbnails-thumbnail-mode-line-up-map "f" 'thumbnails-line-up)
  ;; "i" for "interactive"
  (define-key thumbnails-thumbnail-mode-line-up-map "i" 'thumbnails-line-up-interactive)

  ;; add tag map
  (define-key thumbnails-thumbnail-mode-map "t" thumbnails-thumbnail-mode-tag-map)

  ;; map it to "t" so that the user can press it more quickly
  (define-key thumbnails-thumbnail-mode-tag-map "t" 'thumbnails-tag-thumbnail)
  ;; "r" for "remove"
  (define-key thumbnails-thumbnail-mode-tag-map "r" 'thumbnails-tag-thumbnail-remove)

  (define-key thumbnails-thumbnail-mode-map "\C-m"
    'thumbnails-display-thumbnail-original-image)
  (define-key thumbnails-thumbnail-mode-map [C-return]
    'thumbnails-thumbnail-display-external)

  (define-key thumbnails-thumbnail-mode-map "l" 'thumbnails-rotate-thumbnail-left)
  (define-key thumbnails-thumbnail-mode-map "r" 'thumbnails-rotate-thumbnail-right)

  (define-key thumbnails-thumbnail-mode-map "L" 'thumbnails-rotate-original-left)
  (define-key thumbnails-thumbnail-mode-map "R" 'thumbnails-rotate-original-right)

  (define-key thumbnails-thumbnail-mode-map "D"
    'thumbnails-thumbnail-set-image-description)

  (define-key thumbnails-thumbnail-mode-map "\C-d" 'thumbnails-delete-char)
  (define-key thumbnails-thumbnail-mode-map " "
    'thumbnails-display-next-thumbnail-original)
  (define-key thumbnails-thumbnail-mode-map
    (kbd "DEL") 'thumbnails-display-previous-thumbnail-original)
  (define-key thumbnails-thumbnail-mode-map "c" 'thumbnails-comment-thumbnail)
  (define-key thumbnails-thumbnail-mode-map "q" 'thumbnails-kill-buffer-and-window)

  ;; Mouse
  (define-key thumbnails-thumbnail-mode-map [mouse-2] 'thumbnails-mouse-display-image)
  (define-key thumbnails-thumbnail-mode-map [mouse-1] 'thumbnails-mouse-select-thumbnail)

  ;; Seems I must first set C-down-mouse-1 to undefined, or else it
  ;; will trigger the buffer menu. If I try to instead bind
  ;; C-down-mouse-1 to `thumbnails-mouse-toggle-mark', I get a message
  ;; about C-mouse-1 not being defined afterwards. Annoying, but I
  ;; probably do not completely understand mouse events.

  (define-key thumbnails-thumbnail-mode-map [C-down-mouse-1] 'undefined)
  (define-key thumbnails-thumbnail-mode-map [C-mouse-1] 'thumbnails-mouse-toggle-mark)

  ;; Menu
  (define-key thumbnails-thumbnail-mode-map [menu-bar thumbnails]
    (cons "Thumbnails" (make-sparse-keymap "Thumbnails")))

  (define-key thumbnails-thumbnail-mode-map
    [menu-bar thumbnails thumbnails-kill-buffer-and-window]
    '("Quit" . thumbnails-kill-buffer-and-window))

  (define-key thumbnails-thumbnail-mode-map
    [menu-bar thumbnails thumbnails-delete-char]
    '("Delete thumbnail from buffer" . thumbnails-delete-char))

  (define-key thumbnails-thumbnail-mode-map
    [menu-bar thumbnails thumbnails-tag-thumbnail-remove]
    '("Remove tag from thumbnail" . thumbnails-tag-thumbnail-remove))

  (define-key thumbnails-thumbnail-mode-map
    [menu-bar thumbnails thumbnails-tag-thumbnail]
    '("Tag thumbnail" . thumbnails-tag-thumbnail))

  (define-key thumbnails-thumbnail-mode-map
    [menu-bar thumbnails thumbnails-comment-thumbnail]
    '("Comment thumbnail" . thumbnails-comment-thumbnail))

  (define-key thumbnails-thumbnail-mode-map
    [menu-bar thumbnails thumbnails-refresh-thumb]
    '("Refresh thumb" . thumbnails-refresh-thumb))
  (define-key thumbnails-thumbnail-mode-map
    [menu-bar thumbnails thumbnails-line-up-dynamic]
    '("Dynamic line up" . thumbnails-line-up-dynamic))
  (define-key thumbnails-thumbnail-mode-map
    [menu-bar thumbnails thumbnails-line-up]
    '("Line up thumbnails" . thumbnails-line-up))

  (define-key thumbnails-thumbnail-mode-map
    [menu-bar thumbnails thumbnails-rotate-thumbnail-left]
    '("Rotate thumbnail left" . thumbnails-rotate-thumbnail-left))
  (define-key thumbnails-thumbnail-mode-map
    [menu-bar thumbnails thumbnails-rotate-thumbnail-right]
    '("Rotate thumbnail right" . thumbnails-rotate-thumbnail-right))

  (define-key thumbnails-thumbnail-mode-map
    [menu-bar thumbnails thumbnails-rotate-original-left]
    '("Rotate original left" . thumbnails-rotate-original-left))
  (define-key thumbnails-thumbnail-mode-map
    [menu-bar thumbnails thumbnails-rotate-original-right]
    '("Rotate original right" . thumbnails-rotate-original-right))

  (define-key thumbnails-thumbnail-mode-map
    [menu-bar thumbnails thumbnails-toggle-movement-tracking]
    '("Toggle movement tracking on/off" . thumbnails-toggle-movement-tracking))

  (define-key thumbnails-thumbnail-mode-map
    [menu-bar thumbnails thumbnails-jump-original-dired-buffer]
    '("Jump to dired buffer" . thumbnails-jump-original-dired-buffer))
  (define-key thumbnails-thumbnail-mode-map
    [menu-bar thumbnails thumbnails-track-original-file]
    '("Track original" . thumbnails-track-original-file))

  (define-key thumbnails-thumbnail-mode-map
    [menu-bar thumbnails thumbnails-flag-thumb-original-file]
    '("Flag original for deletion" . thumbnails-flag-thumb-original-file))
  (define-key thumbnails-thumbnail-mode-map
    [menu-bar thumbnails thumbnails-unmark-thumb-original-file]
    '("Unmark original" . thumbnails-unmark-thumb-original-file))
  (define-key thumbnails-thumbnail-mode-map
    [menu-bar thumbnails thumbnails-mark-thumb-original-file]
    '("Mark original" . thumbnails-mark-thumb-original-file))

  (define-key thumbnails-thumbnail-mode-map
    [menu-bar thumbnails thumbnails-thumbnail-display-external]
    '("Display in external viewer" . thumbnails-thumbnail-display-external))
  (define-key thumbnails-thumbnail-mode-map
    [menu-bar thumbnails thumbnails-display-thumbnail-original-image]
    '("Display image" . thumbnails-display-thumbnail-original-image)))

(defvar thumbnails-display-image-mode-map (make-sparse-keymap)
  "Keymap for `thumbnails-display-image-mode'.")

(defun thumbnails-define-display-image-mode-keymap ()
  "Define keymap for `thumbnails-display-image-mode'."

  ;; Keys
  (define-key thumbnails-display-image-mode-map "q" 'thumbnails-kill-buffer-and-window)

  (define-key thumbnails-display-image-mode-map "f"
    'thumbnails-display-current-image-full)

  (define-key thumbnails-display-image-mode-map "s"
    'thumbnails-display-current-image-sized)

  ;; Menu
  (define-key thumbnails-display-image-mode-map [menu-bar thumbnails]
    (cons "Thumbnails" (make-sparse-keymap "Thumbnails")))

  (define-key thumbnails-display-image-mode-map
    [menu-bar thumbnails thumbnails-kill-buffer-and-window]
    '("Quit" . thumbnails-kill-buffer-and-window))

  (define-key thumbnails-display-image-mode-map
    [menu-bar thumbnails thumbnails-display-current-image-sized]
    '("Display original, sized to fit" . thumbnails-display-current-image-sized))

  (define-key thumbnails-display-image-mode-map
    [menu-bar thumbnails thumbnails-display-current-image-full]
    '("Display original, full size" . thumbnails-display-current-image-full))

  )

(defun thumbnails-display-current-image-full ()
  "Display current image in full size."
  (interactive)
  (let ((file (thumbnails-original-file-name)))
    (if file
        (progn
          (thumbnails-display-image file t)
          (message "Full size image displayed"))
      (error "No original file name at point"))))

(defun thumbnails-display-current-image-sized ()
  "Display current image in sized to fit window dimensions."
  (interactive)
  (let ((file (thumbnails-original-file-name)))
    (if file
        (progn
          (thumbnails-display-image file)
          (message "Full size image displayed"))
      (error "No original file name at point"))))

(define-derived-mode thumbnails-thumbnail-mode
  fundamental-mode "thumbnail"
  "Browse and manipulate thumbnail images using dired.
Use `thumbnails-dired' and `thumbnails-setup-dired-keybindings' to get a
nice setup to start with."
  (thumbnails-define-thumbnail-mode-keymap)
  (message "thumbnails-thumbnail-mode enabled"))

(define-derived-mode thumbnails-display-image-mode
  fundamental-mode "thumbnails-image-display"
  "Mode for displaying and manipulating original image.
Resized or in full-size."
  (thumbnails-define-display-image-mode-keymap)
  (message "thumbnails-display-image-mode enabled"))

;;;###autoload
(defun thumbnails-setup-dired-keybindings ()
  "Setup easy-to-use keybindings for the commands to be used in dired mode.
Note that n, p and <down> and <up> will be hijacked and bound to
`thumbnails-dired-x-line'."
  (interactive)

  ;; Hijack previous and next line movement. Let C-p and C-b be
  ;; though...

  (define-key dired-mode-map "p" 'thumbnails-dired-previous-line)
  (define-key dired-mode-map "n" 'thumbnails-dired-next-line)
  (define-key dired-mode-map [up] 'thumbnails-dired-previous-line)
  (define-key dired-mode-map [down] 'thumbnails-dired-next-line)

  (define-key dired-mode-map (kbd "C-S-n") 'thumbnails-next-line-and-display)
  (define-key dired-mode-map (kbd "C-S-p") 'thumbnails-previous-line-and-display)
  (define-key dired-mode-map (kbd "C-S-m") 'thumbnails-mark-and-display-next)

  (define-key dired-mode-map "\C-td" 'thumbnails-display-thumbs)
  (define-key dired-mode-map "\C-tt" 'thumbnails-tag-files)
  (define-key dired-mode-map "\C-tr" 'thumbnails-delete-tag)
  (define-key dired-mode-map [tab] 'thumbnails-jump-thumbnail-buffer)
  (define-key dired-mode-map "\C-ti" 'thumbnails-dired-display-image)
  (define-key dired-mode-map "\C-tx" 'thumbnails-dired-display-external)
  (define-key dired-mode-map "\C-ta" 'thumbnails-display-thumbs-append)
  (define-key dired-mode-map "\C-t." 'thumbnails-display-thumb)
  (define-key dired-mode-map "\C-tc" 'thumbnails-dired-comment-files)
  (define-key dired-mode-map "\C-tf" 'thumbnails-mark-tagged-files)

  ;; Menu for dired
  (define-key dired-mode-map [menu-bar thumbnails]
    (cons "Thumbnails" (make-sparse-keymap "Thumbnails")))

  (define-key dired-mode-map [menu-bar thumbnails thumbnails-copy-with-exif-file-name]
    '("Copy with EXIF file name" . thumbnails-copy-with-exif-file-name))

  (define-key dired-mode-map [menu-bar thumbnails thumbnails-dired-comment-files]
    '("Comment files" . thumbnails-dired-comment-files))

  (define-key dired-mode-map [menu-bar thumbnails thumbnails-mark-tagged-files]
    '("Mark tagged files" . thumbnails-mark-tagged-files))

  (define-key dired-mode-map [menu-bar thumbnails thumbnails-delete-tag]
    '("Remove tag from files" . thumbnails-delete-tag))

  (define-key dired-mode-map [menu-bar thumbnails thumbnails-tag-files]
    '("Tag files" . thumbnails-tag-files))

  (define-key dired-mode-map [menu-bar thumbnails thumbnails-jump-thumbnail-buffer]
    '("Jump to thumbnail buffer" . thumbnails-jump-thumbnail-buffer))

  (define-key dired-mode-map [menu-bar thumbnails thumbnails-toggle-movement-tracking]
    '("Toggle movement tracking" . thumbnails-toggle-movement-tracking))

  (define-key dired-mode-map
    [menu-bar thumbnails thumbnails-toggle-append-browsing]
    '("Toggle append browsing" . thumbnails-toggle-append-browsing))

  (define-key dired-mode-map
    [menu-bar thumbnails thumbnails-toggle-disp-props]
    '("Toggle display properties" . thumbnails-toggle-dired-display-properties))

  (define-key dired-mode-map
    [menu-bar thumbnails thumbnails-dired-display-external]
    '("Display in external viewer" . thumbnails-dired-display-external))
  (define-key dired-mode-map
    [menu-bar thumbnails thumbnails-dired-display-image]
    '("Display image" . thumbnails-dired-display-image))
  (define-key dired-mode-map
    [menu-bar thumbnails thumbnails-display-thumb]
    '("Display this thumbnail" . thumbnails-display-thumb))
  (define-key dired-mode-map
    [menu-bar thumbnails thumbnails-display-thumbs-append]
    '("Display thumbnails append" . thumbnails-display-thumbs-append))
  (define-key dired-mode-map
    [menu-bar thumbnails thumbnails-display-thumbs]
    '("Display thumbnails" . thumbnails-display-thumbs))

  (define-key dired-mode-map
    [menu-bar thumbnails thumbnails-create-thumbs]
    '("Create thumbnails for marked files" . thumbnails-create-thumbs))

  (define-key dired-mode-map
    [menu-bar thumbnails thumbnails-mark-and-display-next]
    '("Mark and display next" . thumbnails-mark-and-display-next))
  (define-key dired-mode-map
    [menu-bar thumbnails thumbnails-previous-line-and-display]
    '("Display thumb for previous file" . thumbnails-previous-line-and-display))
  (define-key dired-mode-map
    [menu-bar thumbnails thumbnails-next-line-and-display]
    '("Display thumb for next file" . thumbnails-next-line-and-display)))

(defun thumbnails-create-thumbs (&optional arg)
  "Create thumbnail images for all marked files in dired.
With prefix argument ARG, create thumbnails even if they already exist
\(i.e.  use this to refresh your thumbnails)."
  (interactive "P")
  (let (curr-file thumb-name files count)
    (setq files (dired-get-marked-files))
    (mapcar
     (lambda (curr-file)
       (setq thumb-name (thumbnails-thumb-name curr-file))
       ;; If the user overrides the exist check, we must clear the
       ;; image cache so that if the user wants to display the
       ;; thumnail, it is not fetched from cache.
       (if arg
           (clear-image-cache))
       (if (or (not (file-exists-p thumb-name))
               arg)
           (if (not (= 0 (thumbnails-create-thumb curr-file
                                             (thumbnails-thumb-name curr-file))))
               (error "Thumb could not be created"))))
     files)))

(defvar thumbnails-slideshow-timer nil
  "Slideshow timer.")

(defvar thumbnails-slideshow-count 0
  "Keeping track on number of images in slideshow.")

(defvar thumbnails-slideshow-times 0
  "Number of pictures to display in slideshow.")

(defun thumbnails-slideshow-step ()
  "Step to next file, if `thumbnails-slideshow-times' has not been reached."
  (if (< thumbnails-slideshow-count thumbnails-slideshow-times)
      (progn
        (message "%s" (1+ thumbnails-slideshow-count))
        (setq thumbnails-slideshow-count (1+ thumbnails-slideshow-count))
        (thumbnails-next-line-and-display))
    (thumbnails-slideshow-stop)))

(defun thumbnails-slideshow-start ()
  "Start slideshow.
Ask user for number of images to show and the delay in between."
  (interactive)
  (setq thumbnails-slideshow-count 0)
  (setq thumbnails-slideshow-times (string-to-number (read-string "How many: ")))
  (let ((repeat (string-to-number
                 (read-string
                  "Delay, in seconds. Decimals are accepted : " "1"))))
    (setq thumbnails-slideshow-timer
          (run-with-timer
           0 repeat
           'thumbnails-slideshow-step))))

(defun thumbnails-slideshow-stop ()
  "Cancel slideshow."
  (interactive)
  (cancel-timer thumbnails-slideshow-timer))

(defun thumbnails-delete-char ()
  "Remove current thumbnail from thumbnail buffer and line up."
  (interactive)
  (let ((inhibit-read-only t))
    (delete-char 1)
    (if (looking-at " ")
        (delete-char 1))))

;;;###autoload
(defun thumbnails-display-thumbs-append ()
  "Append thumbnails to `thumbnails-thumbnail-buffer'."
  (interactive)
  (thumbnails-display-thumbs nil t t))

;;;###autoload
(defun thumbnails-display-thumb ()
  "Shorthand for `thumbnails-display-thumbs' with prefix argument."
  (interactive)
  (thumbnails-display-thumbs t nil t))

(defun thumbnails-line-up ()
  "Line up thumbnails according to `thumbnails-thumbs-per-row'.
See also `thumbnails-line-up-dynamic'."
  (interactive)
  (let ((inhibit-read-only t))
    (goto-char (point-min))
    (while (and (not (thumbnails-image-at-point-p))
                (not (eobp)))
      (delete-char 1))
    (while (not (eobp))
      (forward-char)
      (while (and (not (thumbnails-image-at-point-p))
                  (not (eobp)))
        (delete-char 1)))
    (goto-char (point-min))
    (let ((count 0))
      (while (not (eobp))
        (forward-char)
        (if (= thumbnails-thumbs-per-row 1)
            (insert "\n")
          (insert " ")
          (setq count (1+ count))
          (when (and (= count (- thumbnails-thumbs-per-row 1))
		     (not (eobp)))
            (forward-char)
            (insert "\n")
            (setq count 0)))))
    (goto-char (point-min))))

(defun thumbnails-line-up-dynamic ()
  "Line up thumbnails images dynamically.
Calculate how many thumbnails fit."
  (interactive)
  (let* ((char-width (frame-char-width))
        (width (thumbnails-window-width-pixels (thumbnails-thumbnail-window)))
        (thumbnails-thumbs-per-row
         (/ width
            (+ (* 2 thumbnails-thumb-relief)
               (* 2 thumbnails-thumb-margin)
               thumbnails-thumb-width char-width))))
    (thumbnails-line-up)))

(defun thumbnails-line-up-interactive ()
  "Line up thumbnails interactively.
Ask user how many thumbnails should be displayed per row."
  (interactive)
  (let ((thumbnails-thumbs-per-row
         (string-to-number (read-string "How many thumbs per row: "))))
    (if (not (> thumbnails-thumbs-per-row 0))
        (message "Number must be greater than 0")
      (thumbnails-line-up))))

(defun thumbnails-thumbnail-display-external ()
  "Display original image for thumbnail at point using external viewer."
  (interactive)
  (let ((file (thumbnails-original-file-name)))
    (if (not (thumbnails-image-at-point-p))
        (message "No thumbnail at point")
      (if (not file)
          (message "No original file name found")
        (call-process shell-file-name nil nil nil shell-command-switch
		      (format "%s \"%s\"" thumbnails-external-viewer file))))))

;;;###autoload
(defun thumbnails-dired-display-external ()
  "Display file at point using an external viewer."
  (interactive)
  (let ((file (dired-get-filename)))
    (call-process shell-file-name nil nil nil shell-command-switch
		  (format "%s \"%s\"" thumbnails-external-viewer file))))

(defun thumbnails-window-width-pixels (window)
  "Calculate WINDOW width in pixels."
    (* (window-width window) (frame-char-width)))

(defun thumbnails-window-height-pixels (window)
  "Calculate WINDOW height in pixels."
  ;; Note: The mode-line consumes one line
    (* (- (window-height window) 1) (frame-char-height)))

(defun thumbnails-display-window ()
  "Return window where `thumbnails-display-image-buffer' is visible."
  (get-window-with-predicate
   (lambda (window)
     (equal (buffer-name (window-buffer window)) thumbnails-display-image-buffer))
   nil t))

(defun thumbnails-thumbnail-window ()
  "Return window where `thumbnails-thumbnail-buffer' is visible."
  (get-window-with-predicate
   (lambda (window)
     (equal (buffer-name (window-buffer window)) thumbnails-thumbnail-buffer))
   nil t))

(defun thumbnails-associated-dired-buffer-window ()
  "Return window where associated dired buffer is visible."
  (let (buf)
    (if (thumbnails-image-at-point-p)
        (progn
          (setq buf (thumbnails-associated-dired-buffer))
          (get-window-with-predicate
           (lambda (window)
             (equal (window-buffer window) buf))))
      (error "No thumbnail image at point"))))

(defun thumbnails-display-window-width ()
  "Return width, in pixels, of thumbnails's image display window."
  (- (thumbnails-window-width-pixels (thumbnails-display-window))
     thumbnails-display-window-width-correction))

(defun thumbnails-display-window-height ()
  "Return height, in pixels, of thumbnails's image display window."
  (- (thumbnails-window-height-pixels (thumbnails-display-window))
     thumbnails-display-window-height-correction))

(defun thumbnails-display-image (file &optional original-size)
  "Display image FILE in image buffer.
Use this when you want to display the image, semi sized, in a new
window.  The image is sized to fit the display window (using a
temporary file, don't worry).  Because of this, it will not be as
quick as opening it directly, but on most modern systems it
should feel snappy enough.

If optional argument ORIGINAL-SIZE is non-nil, display image in its
original size."
  (let ((new-file (expand-file-name thumbnails-temp-image-file))
        width height command ret)
    (setq file (expand-file-name file))
    (if (not original-size)
        (progn
          (setq width (thumbnails-display-window-width))
          (setq height (thumbnails-display-window-height))
          (setq command
                (format-spec
                 thumbnails-cmd-create-temp-image-options
                 (list
                  (cons ?p thumbnails-cmd-create-temp-image-program)
                  (cons ?w width)
                  (cons ?h height)
                  (cons ?f file)
                  (cons ?t new-file))))
          (setq ret (call-process shell-file-name nil nil nil
				  shell-command-switch command))
          (if (not (= 0 ret))
              (error "Could not resize image")))
      (copy-file file new-file t))
    (with-current-buffer (thumbnails-create-display-image-buffer)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (clear-image-cache)
        (thumbnails-insert-image thumbnails-temp-image-file 'jpeg 0 0)
        (goto-char (point-min))
        (thumbnails-update-property 'original-file-name file)))))

(defun thumbnails-display-thumbnail-original-image (&optional arg)
  "Display current thumbnail's original image in display buffer.
See documentation for `thumbnails-display-image' for more information.
With prefix argument ARG, display image in its original size."
  (interactive "P")
  (let ((file (thumbnails-original-file-name)))
    (if (not (string-equal major-mode "thumbnails-thumbnail-mode"))
        (message "Not in thumbnails-thumbnail-mode")
      (if (not (thumbnails-image-at-point-p))
          (message "No thumbnail at point")
        (if (not file)
            (message "No original file name found")
	  (thumbnails-create-display-image-buffer)
          (display-buffer thumbnails-display-image-buffer)
          (thumbnails-display-image file arg))))))


;;;###autoload
(defun thumbnails-dired-display-image (&optional arg)
  "Display current image file.
See documentation for `thumbnails-display-image' for more information.
With prefix argument ARG, display image in its original size."
  (interactive "P")
  (thumbnails-create-display-image-buffer)
  (display-buffer thumbnails-display-image-buffer)
  (thumbnails-display-image (dired-get-filename) arg))

(defun thumbnails-image-at-point-p ()
  "Return true if there is a thumbnails thumbnail at point."
  (get-text-property (point) 'thumbnails-thumbnail))

(defun thumbnails-rotate-thumbnail (degrees)
  "Rotate thumbnail DEGREES degrees."
  (if (not (thumbnails-image-at-point-p))
      (message "No thumbnail at point")
    (let ((file (thumbnails-thumb-name (thumbnails-original-file-name)))
          command)
      (setq command (format-spec
                     thumbnails-cmd-rotate-thumbnail-options
                     (list
                      (cons ?p thumbnails-cmd-rotate-thumbnail-program)
                      (cons ?d degrees)
                      (cons ?t (expand-file-name file)))))
      (call-process shell-file-name nil nil nil shell-command-switch command)
      ;; Clear the cache to refresh image. I wish I could just refresh
      ;; the current file but I do not know how to do that. Yet...
      (clear-image-cache))))

(defun thumbnails-rotate-thumbnail-left ()
  "Rotate thumbnail left (counter clockwise) 90 degrees.
The result of the rotation is displayed in the image display area
and a confirmation is needed before the original image files is
overwritten.  This confirmation can be turned off using
`thumbnails-rotate-original-ask-before-overwrite'."
  (interactive)
  (thumbnails-rotate-thumbnail "270"))

(defun thumbnails-rotate-thumbnail-right ()
  "Rotate thumbnail counter right (clockwise) 90 degrees.
The result of the rotation is displayed in the image display area
and a confirmation is needed before the original image files is
overwritten.  This confirmation can be turned off using
`thumbnails-rotate-original-ask-before-overwrite'."
  (interactive)
  (thumbnails-rotate-thumbnail "90"))

(defun thumbnails-refresh-thumb ()
  "Force creation of new image for current thumbnail."
  (interactive)
  (let ((file (thumbnails-original-file-name)))
    (clear-image-cache)
    (thumbnails-create-thumb file (thumbnails-thumb-name file))))

(defun thumbnails-rotate-original (degrees)
  "Rotate original image DEGREES degrees."
  (if (not (thumbnails-image-at-point-p))
      (message "No image at point")
    (let ((file (thumbnails-original-file-name))
          command temp-file)
      (if (not (string-match "\.[jJ][pP[eE]?[gG]$" file))
          (error "Only JPEG images can be rotated!"))
      (setq command (format-spec
                     thumbnails-cmd-rotate-original-options
                     (list
                      (cons ?p thumbnails-cmd-rotate-original-program)
                      (cons ?d degrees)
                      (cons ?o (expand-file-name file))
                      (cons ?t thumbnails-temp-rotate-image-file))))
      (if (not (= 0 (call-process shell-file-name nil nil nil
				  shell-command-switch command)))
          (error "Could not rotate image")
        (thumbnails-display-image thumbnails-temp-rotate-image-file)
        (if (or (and thumbnails-rotate-original-ask-before-overwrite
                     (y-or-n-p
		      "Rotate to temp file OK.  Overwrite original image? "))
                (not thumbnails-rotate-original-ask-before-overwrite))
            (progn
              (copy-file thumbnails-temp-rotate-image-file file t)
              (thumbnails-refresh-thumb))
          (thumbnails-display-image file))))))

(defun thumbnails-rotate-original-left ()
  "Rotate original image left (counter clockwise) 90 degrees."
  (interactive)
  (thumbnails-rotate-original "270"))

(defun thumbnails-rotate-original-right ()
  "Rotate original image right (clockwise) 90 degrees."
  (interactive)
  (thumbnails-rotate-original "90"))

(defun thumbnails-get-exif-file-name (file)
  "Use the image's EXIF information to return a unique file name.
The file name should be unique as long as you do not take more than
one picture per second.  The original file name is suffixed at the end
for traceability.  The format of the returned file name is
YYYY_MM_DD_HH_MM_DD_ORIG_FILE_NAME.jpg.  Used from
`thumbnails-copy-with-exif-file-name'."
  (let (data no-exif-data-found)
    (if (not (string-match "\.[Jj][Pp][Ee]?[Gg]$" (expand-file-name file)))
        (progn
          (setq no-exif-data-found t)
          (setq data
                (format-time-string
                 "%Y:%m:%d %H:%M:%S"
                 (nth 5 (file-attributes (expand-file-name file))))))
      (setq data (thumbnails-get-exif-data (expand-file-name file)
				      "DateTimeOriginal")))
    (while (string-match "[ :]" data)
      (setq data (replace-match "_" nil nil data)))
    (format "%s%s%s" data
            (if no-exif-data-found
                "_noexif_"
              "_")
            (file-name-nondirectory file))))

(defun thumbnails-thumbnail-set-image-description ()
  "Set the ImageDescription EXIF tag for the original image.
If the image already has a value for this tag, it is used as the
default value at the prompt."
  (interactive)
  (if (not (thumbnails-image-at-point-p))
      (message "No thumbnail at point")
    (let* ((file (thumbnails-original-file-name))
           (old-value (thumbnails-get-exif-data file "ImageDescription")))
      (if (eq 0
              (thumbnails-set-exif-data file "ImageDescription"
                                   (read-string "Value of ImageDescription: "
						old-value)))
          (message "Successfully wrote ImageDescription tag.")
        (error "Could not write ImageDescription tag")))))

(defun thumbnails-set-exif-data (file tag-name tag-value)
  "In FILE, set EXIF tag TAG-NAME to value TAG-VALUE."
  (let (command)
    (setq command (format-spec
                   thumbnails-cmd-write-exif-data-options
                   (list
                    (cons ?p thumbnails-cmd-write-exif-data-program)
                    (cons ?f (expand-file-name file))
                    (cons ?t tag-name)
                    (cons ?v tag-value))))
    (call-process shell-file-name nil nil nil shell-command-switch command)))

(defun thumbnails-get-exif-data (file tag-name)
  "From FILE, return EXIF tag TAG-NAME."
  (let ((buf (get-buffer-create "*thumbnails-get-exif-data*"))
        command tag-value)
    (setq command (format-spec
                   thumbnails-cmd-read-exif-data-options
                   (list
                    (cons ?p thumbnails-cmd-read-exif-data-program)
                    (cons ?f file)
                    (cons ?t tag-name))))
    (with-current-buffer buf
      (delete-region (point-min) (point-max))
      (if (not (eq (call-process shell-file-name nil t nil
				 shell-command-switch command) 0))
          (error "Could not get EXIF tag")
        (goto-char (point-min))
        ;; Clean buffer from newlines and carriage returns before
        ;; getting final info
        (while (search-forward-regexp "[\n\r]" nil t)
          (replace-match "" nil t))
        (setq tag-value (buffer-substring (point-min) (point-max)))))
    tag-value))

(defun thumbnails-copy-with-exif-file-name ()
  "Copy file with unique name to main image directory.
Copy current or all marked files in dired to a new file in your
main image directory, using a file name generated by
`thumbnails-get-exif-file-name'.  A typical usage for this if when
copying images from a digital camera into the image directory.

 Typically, you would open up the folder with the incoming
digital images, mark the files to be copied, and execute this
function.  The result is a couple of new files in
`thumbnails-main-image-directory' called
2005_05_08_12_52_00_dscn0319.jpg,
2005_05_08_14_27_45_dscn0320.jpg etc."
  (interactive)
  (let (new-name
        (files (dired-get-marked-files)))
    (mapcar
     (lambda (curr-file)
       (setq new-name
             (format "%s/%s"
                     (file-name-as-directory
                      (expand-file-name thumbnails-main-image-directory))
                     (thumbnails-get-exif-file-name curr-file)))
       (message "Copying %s to %s" curr-file new-name)
       (copy-file curr-file new-name))
     files)))

(defun thumbnails-display-next-thumbnail-original ()
  "In thubnail buffer, move to next thumbnail and display the image."
  (interactive)
  (thumbnails-forward-image)
  (thumbnails-display-thumbnail-original-image))

(defun thumbnails-display-previous-thumbnail-original ()
  "Move to previous thumbnail and display image."
  (interactive)
  (thumbnails-backward-image)
  (thumbnails-display-thumbnail-original-image))

(defun thumbnails-write-comments (file-comments)
  "Write file comments to database.
Write file comments to one or more files.  FILE-COMMENTS is an alist on
the following form:
 ((FILE . COMMENT) ... )"
  (let (end comment-beg-pos comment-end-pos file comment)
    (with-temp-file thumbnails-db-file
      (insert-file-contents thumbnails-db-file)
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
	  (insert (format "\n%s;comment:%s" file comment)))))))

(defun thumbnails-update-property (prop value)
  "Update text property PROP with value VALUE at point."
  (let ((inhibit-read-only t))
    (put-text-property
     (point) (1+ (point))
     prop
     value)))

;;;###autoload
(defun thumbnails-dired-comment-files ()
  "Add comment to current or marked files in dired."
  (interactive)
  (let ((comment (thumbnails-read-comment)))
    (thumbnails-write-comments
     (mapcar
      (lambda (curr-file)
        (cons curr-file comment))
      (dired-get-marked-files)))))

(defun thumbnails-comment-thumbnail ()
  "Add comment to current thumbnail in thumbnail buffer."
  (interactive)
  (let* ((file (thumbnails-original-file-name))
         (comment (thumbnails-read-comment file)))
    (thumbnails-write-comments (list (cons file comment)))
    (thumbnails-update-property 'comment comment))
  (thumbnails-display-thumb-properties))

(defun thumbnails-read-comment (&optional file)
  "Read comment for an image.
Read comment for an image, optionally using old comment from FILE
as initial value."
  (let ((comment
         (read-string
          "Comment: "
          (if file (thumbnails-get-comment file)))))
    comment))

(defun thumbnails-get-comment (file)
  "Get comment for file FILE."
  (save-excursion
    (let (end buf comment-beg-pos comment-end-pos comment)
      (setq buf (find-file thumbnails-db-file))
      (goto-char (point-min))
      (when (search-forward-regexp
             (format "^%s" file) nil t)
        (end-of-line)
        (setq end (point))
        (beginning-of-line)
        (cond ((search-forward ";comment:" end t)
               (setq comment-beg-pos (point))
               (if (search-forward ";" end t)
                   (setq comment-end-pos (- (point) 1))
                 (setq comment-end-pos end))
               (setq comment (buffer-substring
                              comment-beg-pos comment-end-pos)))))
      (kill-buffer buf)
      comment)))

;;;###autoload
(defun thumbnails-mark-tagged-files ()
  "Use regexp to mark files with matching tag.
A `tag' is a keyword, a piece of meta data, associated with an
image file and stored in thumbnails's database file.  This command
lets you input a regexp and this will be matched against all tags
on all image files in the database file.  The files that have a
matching tags will be marked in the dired buffer."
  (interactive)
  (let ((tag (read-string "Mark tagged files (regexp): "))
        (hits 0)
        files buf)
    (save-excursion
      (setq buf (find-file thumbnails-db-file))
      (goto-char (point-min))
      ;; Collect matches
      (while (search-forward-regexp
              (concat "\\(^[^;\n]+\\);.*" tag ".*$") nil t)
        (setq files (append (list (match-string 1)) files)))
      (kill-buffer buf)
      ;; Mark files
      (mapcar
       ;; I tried using `dired-mark-files-regexp' but it was
       ;; waaaay to slow.
       (lambda (curr-file)
         ;; Don't bother about hits found in other directories than
         ;; the current one.
         (when (string= (file-name-as-directory
                         (expand-file-name default-directory))
                      (file-name-as-directory
                       (file-name-directory curr-file)))
           (setq curr-file (file-name-nondirectory curr-file))
           (goto-char (point-min))
           (when (search-forward-regexp (format "\\s %s$" curr-file) nil t)
             (setq hits (+ hits 1))
             (dired-mark 1))))
       files))
    (message "%d files with matching tag marked." hits)))

(defun thumbnails-mouse-display-image (event)
  "Use mouse EVENT, call `thumbnails-display-image' to display image.
Track this in associated dired buffer if `thumbnails-track-movement' is
non-nil."
  (interactive "e")
  (let (file)
    (mouse-set-point event)
    (goto-char (posn-point (event-end event)))
    (setq file (thumbnails-original-file-name))
    (if thumbnails-track-movement
        (thumbnails-track-original-file))
    (thumbnails-create-display-image-buffer)
    (display-buffer thumbnails-display-image-buffer)
    (thumbnails-display-image file)))

(defun thumbnails-mouse-select-thumbnail (event)
  "Use mouse EVENT to select thumbnail image.
Track this in associated dired buffer if `thumbnails-track-movement' is
non-nil."
  (interactive "e")
  (let (file)
    (mouse-set-point event)
    (goto-char (posn-point (event-end event)))
    (if thumbnails-track-movement
        (thumbnails-track-original-file)))
  (thumbnails-display-thumb-properties))

(defun thumbnails-mouse-toggle-mark (event)
  "Use mouse EVENT to toggle dired mark for thumbnail.
Track this in associated dired buffer if `thumbnails-track-movement' is
non-nil."
  (interactive "e")
  (let (file)
    (mouse-set-point event)
    (goto-char (posn-point (event-end event)))
    (if thumbnails-track-movement
        (thumbnails-track-original-file)))
  (thumbnails-toggle-mark-thumb-original-file))

(defun thumbnails-dired-display-properties ()
  "Display properties for dired file in the echo area."
  (interactive)
  (let* ((file (dired-get-filename))
         (file-name (file-name-nondirectory file))
         (dired-buf (buffer-name (current-buffer)))
         (props (mapconcat
                 'princ
                 (thumbnails-list-tags file)
                 ", "))
         (comment (thumbnails-get-comment file)))
    (if file-name
        (message
         (thumbnails-format-properties-string
          dired-buf
          file-name
          props
          comment)))))

(defvar thumbnails-tag-file-list nil
  "List to store tag-file structure.")

(defvar thumbnails-file-tag-list nil
  "List to store file-tag structure.")

(defvar thumbnails-file-comment-list nil
  "List to store file comments.")

(defun thumbnails-add-to-tag-file-list (tag file)
  "Add relation between TAG and FILE."
  (let (curr)
    (if thumbnails-tag-file-list
        (if (setq curr (assoc tag thumbnails-tag-file-list))
            (if (not (member file curr))
                (setcdr curr (cons file (cdr curr))))
          (setcdr thumbnails-tag-file-list
                  (cons (list tag file) (cdr thumbnails-tag-file-list))))
      (setq thumbnails-tag-file-list (list (list tag file))))))

(defun thumbnails-add-to-tag-file-lists (tag file)
  "Helper function used from `thumbnails-create-gallery-lists'.

Add TAG to FILE in one list and FILE to TAG in the other.

Lisp structures look like the following:

thumbnails-file-tag-list:

  ((\"filename1\" \"tag1\" \"tag2\" \"tag3\" ...)
   (\"filename2\" \"tag1\" \"tag2\" \"tag3\" ...)
   ...)

thumbnails-tag-file-list:

 ((\"tag1\" \"filename1\" \"filename2\" \"filename3\" ...)
  (\"tag2\" \"filename1\" \"filename2\" \"filename3\" ...)
  ...)"
  ;; Add tag to file list
  (let (curr)
    (if thumbnails-file-tag-list
        (if (setq curr (assoc file thumbnails-file-tag-list))
            (setcdr curr (cons tag (cdr curr)))
          (setcdr thumbnails-file-tag-list
                  (cons (list file tag) (cdr thumbnails-file-tag-list))))
      (setq thumbnails-file-tag-list (list (list file tag))))
    ;; Add file to tag list
    (if thumbnails-tag-file-list
        (if (setq curr (assoc tag thumbnails-tag-file-list))
            (if (not (member file curr))
                (setcdr curr (cons file (cdr curr))))
          (setcdr thumbnails-tag-file-list
                  (cons (list tag file) (cdr thumbnails-tag-file-list))))
      (setq thumbnails-tag-file-list (list (list tag file))))))

(defun thumbnails-add-to-file-comment-list (file comment)
  "Helper function used from `thumbnails-create-gallery-lists'.

For FILE, add COMMENT to list.

Lisp structure looks like the following:

thumbnails-file-comment-list:

  ((\"filename1\" .  \"comment1\")
   (\"filename2\" .  \"comment2\")
   ...)"
  (if thumbnails-file-comment-list
      (if (not (assoc file thumbnails-file-comment-list))
          (setcdr thumbnails-file-comment-list
                  (cons (cons file comment)
                        (cdr thumbnails-file-comment-list))))
    (setq thumbnails-file-comment-list (list (cons file comment)))))

(defun thumbnails-create-gallery-lists ()
  "Create temporary lists used by `thumbnails-gallery-generate'."
  (let ((buf (find-file thumbnails-db-file))
        end beg file row-tags)
    (setq thumbnails-tag-file-list nil)
    (setq thumbnails-file-tag-list nil)
    (setq thumbnails-file-comment-list nil)
    (goto-char (point-min))
    (while (search-forward-regexp "^." nil t)
      (end-of-line)
      (setq end (point))
      (beginning-of-line)
      (setq beg (point))
      (if (not (search-forward ";" end nil))
          (error "Something is really wrong, check format of database"))
      (setq row-tags (split-string
                      (buffer-substring beg end) ";"))
      (setq file (car row-tags))
      (mapc
       (lambda (x)
         (if (not (string-match "^comment:\\(.*\\)" x))
             (thumbnails-add-to-tag-file-lists x file)
           (thumbnails-add-to-file-comment-list file (match-string 1 x))))
       (cdr row-tags)))
    (kill-buffer buf))
  ;; Sort tag-file list
  (setq thumbnails-tag-file-list
        (sort thumbnails-tag-file-list
              (lambda (x y)
                (string< (car x) (car y))))))

(defun thumbnails-hidden-p (file)
  "Return t if image FILE has a \"hidden\" tag."
  (let (hidden)
    (mapc
     (lambda (tag)
       (if (member tag thumbnails-gallery-hidden-tags)
           (setq hidden t)))
     (cdr (assoc file thumbnails-file-tag-list)))
    hidden))

(defun thumbnails-gallery-generate ()
  "Generate gallery pages.
First we create a couple of Lisp structures from the database to make
it easier to generate, then HTML-files are created in
`thumbnails-gallery-dir'"
  (interactive)
  (if (eq 'per-directory thumbnails-thumbnail-storage)
      (error "Currently, gallery generation is not supported \
when using per-directory thumbnail file storage"))
  (thumbnails-create-gallery-lists)
  (let ((tags thumbnails-tag-file-list)
        count curr tag index-buf tag-buf
        comment file-tags tag-link tag-link-list)
    ;; Make sure gallery root exist
    (if (file-exists-p thumbnails-gallery-dir)
        (if (not (file-directory-p thumbnails-gallery-dir))
            (error "Variable thumbnails-gallery-dir is not a directory"))
      (make-directory thumbnails-gallery-dir))
    ;; Open index file
    (setq index-buf (find-file
                     (format "%s/index.html" thumbnails-gallery-dir)))
    (erase-buffer)
    (insert "<html>\n")
    (insert "  <body>\n")
    (insert "   <h2>Thumbnails Gallery</h2>\n")
    (insert (format "<p>\n    Gallery generated %s\n   <p>\n"
                    (current-time-string)))
    (insert "   <h3>Tag index</h3>\n")
    (setq count 1)
    ;; Pre-generate list of all tag links
    (mapc
     (lambda (curr)
       (setq tag (car curr))
       (when (not (member tag thumbnails-gallery-hidden-tags))
         (setq tag-link (format "<a href=\"%d.html\">%s</a>" count tag))
         (if tag-link-list
             (setq tag-link-list
                   (append tag-link-list (list (cons tag tag-link))))
           (setq tag-link-list (list (cons tag tag-link))))
         (setq count (1+ count))))
     tags)
    (setq count 1)
    ;; Main loop where we generated thumbnail pages per tag
    (mapc
     (lambda (curr)
       (setq tag (car curr))
       ;; Don't display hidden tags
       (when (not (member tag thumbnails-gallery-hidden-tags))
         ;; Insert link to tag page in index
         (insert (format "    %s<br>\n" (cdr (assoc tag tag-link-list))))
         ;; Open per-tag file
         (setq tag-buf (find-file
                        (format "%s/%s.html" thumbnails-gallery-dir count)))
         (erase-buffer)
         (insert "<html>\n")
         (insert "  <body>\n")
         (insert "  <p><a href=\"index.html\">Index</a></p>\n")
         (insert (format "  <h2>Images with tag &quot;%s&quot;</h2>" tag))
         ;; Main loop for files per tag page
         (mapc
          (lambda (file)
            (when (not (thumbnails-hidden-p file))
              ;; Insert thumbnail with link to full image
              (insert
               (format "<a href=\"%s/%s\"><img src=\"%s/%s\"%s></a>\n"
                       thumbnails-gallery-image-root-url
		       (file-name-nondirectory file)
                       thumbnails-gallery-thumb-image-root-url
                       (file-name-nondirectory (thumbnails-thumb-name file)) file))
              ;; Insert comment, if any
              (if (setq comment (cdr (assoc file thumbnails-file-comment-list)))
                  (insert (format "<br>\n%s<br>\n" comment))
                (insert "<br>\n"))
              ;; Insert links to other tags, if any
              (when (> (length
                        (setq file-tags (assoc file thumbnails-file-tag-list))) 2)
                (insert "[ ")
                (mapc
                 (lambda (extra-tag)
                   ;; Only insert if not file name or the main tag
                   (if (and (not (equal extra-tag tag))
                            (not (equal extra-tag file)))
                       (insert
                        (format "%s " (cdr (assoc extra-tag tag-link-list))))))
                 file-tags)
                (insert "]<br>\n"))))
          (cdr curr))
         (insert "  <p><a href=\"index.html\">Index</a></p>\n")
         (insert "  </body>\n")
         (insert "</html>\n")
         (save-buffer)
         (kill-buffer tag-buf)
         (setq count (1+ count))))
       tags)
    (insert "  </body>\n")
    (insert "</html>")
    (save-buffer)
    (kill-buffer index-buf)))

(defun thumbnails-kill-buffer-and-window ()
  "Kill the current buffer and, if possible, also the window."
  (interactive)
  (let ((buffer (current-buffer)))
    (condition-case nil
        (delete-window (selected-window))
      (error nil))
    (kill-buffer buffer)))

(defvar thumbnails-widget-list nil
  "List to keep track of meta data in edit buffer.")

;;;###autoload
(defun thumbnails-dired-edit-comment-and-tags ()
  "Edit comment and tags of current or marked image files.
Edit comment and tags for all marked image files in an
easy-to-use form."
  (interactive)
  (setq thumbnails-widget-list nil)
  ;; Setup buffer.
  (let ((files (dired-get-marked-files)))
    (switch-to-buffer "*Thumbnails Edit Meta Data*")
    (kill-all-local-variables)
    (make-local-variable 'widget-example-repeat)
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

       (setq thumb-file (thumbnails-thumb-name file)
             img (create-image thumb-file))

       (insert-image img)
       (widget-insert "\n\nComment: ")
       (setq comment-widget
             (widget-create 'editable-field
                            :size 60
                            :format "%v "
                            :value (or (thumbnails-get-comment file) "")))
       (widget-insert "\nTags:    ")
       (setq tag-widget
             (widget-create 'editable-field
                            :size 60
                            :format "%v "
                            :value (or (mapconcat
                                        (lambda (tag)
                                          tag)
                                        (thumbnails-list-tags file)
                                        ",") "")))
       ;; Save information in all widgets so that we can use it when
       ;; the user saves the form.
       (setq thumbnails-widget-list
             (append thumbnails-widget-list
                     (list (list file comment-widget tag-widget))))
       (widget-insert "\n\n")))

    ;; Footer with Save and Cancel button.
    (widget-insert "\n")
    (widget-create 'push-button
                 :notify
                 (lambda (&rest ignore)
                   (thumbnails-save-information-from-widgets)
                   (bury-buffer)
                   (message "Done."))
                 "Save")
    (widget-insert " ")
    (widget-create 'push-button
                   :notify
                   (lambda (&rest ignore)
                     (bury-buffer)
                     (message "Operation canceled."))
                   "Cancel")
    (widget-insert "\n")
    (use-local-map widget-keymap)
    (widget-setup)
    ;; Jump to the first widget.
    (widget-forward 1)))

(defun thumbnails-save-information-from-widgets ()
  "Save information found in `thumbnails-widget-list'.
Use the information in `thumbnails-widget-list' to save comments and
tags to their respective image file.  Internal function used by
`thumbnails-dired-edit-comment-and-tags'."
  (let (file comment tag-string tag-list lst)
    (thumbnails-write-comments
          (mapcar
           (lambda (widget)
             (setq file (car widget)
                   comment (widget-value (cadr widget)))
             (cons file comment))
           thumbnails-widget-list))
    (thumbnails-write-tags
     (dolist (widget thumbnails-widget-list lst)
       (setq file (car widget)
             tag-string (widget-value (car (cddr widget)))
             tag-list (split-string tag-string ","))
       (dolist (tag tag-list)
         (push (cons file tag) lst))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; TEST-SECTION ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defvar thumbnails-dir-max-size 12300000)

;; (defun thumbnails-test-clean-old-files ()
;;   "Clean `thumbnails-dir' from old thumbnail files.
;; \"Oldness\" measured using last access time.  If the total size of all
;; thumbnail files in `thumbnails-dir' is larger than 'thumbnails-dir-max-size',
;; old files are deleted until the max size is reached."
;;   (let* ((files
;;           (sort
;;            (mapcar
;;             (lambda (f)
;;               (let ((fattribs (file-attributes f)))
;;                 ;; Get last access time and file size
;;                 `(,(nth 4 fattribs) ,(nth 7 fattribs) ,f)))
;;             (directory-files (thumbnails-dir) t ".+\.thumb\..+$"))
;;            ;; Sort function. Compare time between two files.
;;            '(lambda (l1 l2)
;;               (time-less-p (car l1) (car l2)))))
;;          (dirsize (apply '+ (mapcar (lambda (x) (cadr x)) files))))
;;     (while (> dirsize thumbnails-dir-max-size)
;;       (y-or-n-p
;;        (format "Size of thumbnail directory: %d, delete old file %s? "
;;                dirsize (cadr (cdar files))))
;;       (delete-file (cadr (cdar files)))
;;       (setq dirsize (- dirsize (car (cdar files))))
;;       (setq files (cdr files)))))

;;;;;;;;;;;;;;;;;;;;;;,

;; (defun dired-speedbar-buttons (dired-buffer)
;;   (when (and (boundp 'thumbnails-use-speedbar)
;; 	     thumbnails-use-speedbar)
;;     (let ((filename (with-current-buffer dired-buffer
;; 		      (dired-get-filename))))
;;       (when (and (not (string-equal filename (buffer-string)))
;; 		 (string-match (image-file-name-regexp) filename))
;; 	(erase-buffer)
;; 	(insert (propertize
;; 		 filename
;; 		 'display
;; 		 (thumbnails-get-thumbnail-image filename)))))))

;; (setq thumbnails-use-speedbar t)

(provide 'thumbnails)

;; arch-tag: 9d11411d-331f-4380-8b44-8adfe3a0343e
;;; thumbnails.el ends here
