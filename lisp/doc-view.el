;;; doc-view.el --- Document viewer for Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2007-2026 Free Software Foundation, Inc.
;;
;; Author: Tassilo Horn <tsdh@gnu.org>
;; Keywords: files, pdf, ps, dvi, djvu, epub, cbz, fb2, xps, openxps

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

;;; Requirements:

;; Viewing PS/PDF/DVI files requires Ghostscript, `dvipdf' (comes with
;; Ghostscript) or `dvipdfm' (comes with teTeX or TeXLive) and
;; `pdftotext', which comes with xpdf (https://www.foolabs.com/xpdf/)
;; or poppler (https://poppler.freedesktop.org/). EPUB, CBZ, FB2, XPS
;; and OXPS documents require `mutool' which comes with mupdf
;; (https://mupdf.com/index.html). DjVu documents require `ddjvu'
;; (from DjVuLibre).  ODF files require `soffice' (from LibreOffice).
;; `djvused' (from DjVuLibre) can be optionally used to generate imenu
;; outline for DjVu documents when available.

;;; Commentary:

;; DocView is a document viewer for Emacs.  It converts a number of
;; document formats (including PDF, PS, DVI, Djvu, ODF, EPUB, CBZ,
;; FB2, XPS and OXPS files) to a set of PNG (or TIFF for djvu) files,
;; one image for each page, and displays the images inside an Emacs
;; buffer.  This buffer uses `doc-view-mode' which provides convenient
;; key bindings for browsing the document.
;;
;; To use it simply open a document file with
;;
;;     C-x C-f ~/path/to/document RET
;;
;; and the document will be converted and displayed, if your Emacs supports PNG
;; images.  With `C-c C-c' you can toggle between the rendered images
;; representation and the source text representation of the document.
;;
;; Since conversion may take some time all the PNG images are cached in a
;; subdirectory of `doc-view-cache-directory' and reused when you want to view
;; that file again.  To reconvert a document hit `g' (`doc-view-reconvert-doc')
;; when displaying the document.  To delete all cached files use
;; `doc-view-clear-cache'.  To open the cache with Dired, so that you can tidy
;; it out use `doc-view-dired-cache'.
;;
;; When conversion is underway the first page will be displayed as soon as it
;; is available and the available pages are refreshed every
;; `doc-view-conversion-refresh-interval' seconds.  If that variable is nil the
;; pages won't be displayed before conversion of the document finished
;; completely.
;;
;; DocView lets you select a slice of the displayed pages.  This slice
;; will be remembered and applied to all pages of the current
;; document.  This enables you to cut away the margins of a document
;; to save some space.  To select a slice you can use
;; `doc-view-set-slice' (bound to `c s') which will query you for the
;; coordinates of the slice's top-left corner and its width and
;; height.  A much more convenient way to do the same is offered by
;; the command `doc-view-set-slice-using-mouse' (bound to `c m').
;; After invocation you only have to press mouse-1 at the top-left
;; corner and drag it to the bottom-right corner of the desired slice.
;; Even more accurate and convenient is to use
;; `doc-view-set-slice-from-bounding-box' (bound to `c b') which uses
;; the BoundingBox information of the current page to set an optimal
;; slice.  To reset the slice use `doc-view-reset-slice' (bound to `c
;; r').
;;
;; You can also search within the document.  The command `doc-view-search'
;; (bound to `C-s') queries for a search regexp and initializes a list of all
;; matching pages and messages how many match-pages were found.  After that you
;; can jump to the next page containing a match with an additional `C-s'.  With
;; `C-r' you can do the same, but backwards.  To search for a new regexp give a
;; prefix arg to one of the search functions, e.g. by typing `C-u C-s'.  The
;; searching works by using a plain text representation of the document.  If
;; that doesn't already exist the first invocation of `doc-view-search' (or
;; `doc-view-search-backward') starts the conversion.  When that finishes and
;; you're still viewing the document (i.e. you didn't switch to another buffer)
;; you're queried for the regexp then.
;;
;; Dired users can simply hit `v' on a document file.  If it's a PS, PDF or DVI
;; it will be opened using `doc-view-mode'.
;;

;;; Configuration:

;; If the images are too small or too big you should set the "-rXXX" option in
;; `doc-view-ghostscript-options' to another value.  (The bigger your screen,
;; the higher the value.)
;;
;; This and all other options can be set with the customization interface.
;; Simply do
;;
;;     M-x customize-group RET doc-view RET
;;
;; and modify them to your needs.

;;; Todo:

;; - add print command.
;; - share more code with image-mode.
;; - better menu.
;; - Bind slicing to a drag event.
;; - zoom the region around the cursor (like xdvi).
;; - get rid of the silly arrow in the fringe.
;; - improve anti-aliasing (pdf-utils gets it better).

;;;; About isearch support

;; I tried implementing isearch by setting
;; `isearch-search-fun-function' buffer-locally, but that didn't
;; work too good.  The function doing the real search was called
;; endlessly somehow.  But even if we'd get that working no real
;; isearch feeling comes up due to the missing match highlighting.
;; Currently I display all lines containing a match in a tooltip and
;; each C-s or C-r jumps directly to the next/previous page with a
;; match.  With isearch we could only display the current match.  So
;; we had to decide if another C-s jumps to the next page with a
;; match (thus only the first match in a page will be displayed in a
;; tooltip) or to the next match, which would do nothing visible
;; (except the tooltip) if the next match is on the same page.

;; And it's much slower than the current search facility, because
;; isearch really searches for each step forward or backward whereas
;; the current approach searches once and then it knows to which
;; pages to jump.

;; Anyway, if someone with better isearch knowledge wants to give it a try,
;; feel free to do it.  --Tassilo

;;; Code:

(require 'cl-lib)
(require 'dired)
(require 'image-mode)
(require 'jka-compr)
(require 'filenotify)
(eval-when-compile (require 'subr-x))

(autoload 'imenu-unavailable-error "imenu")

;;;; Customization Options

(defgroup doc-view nil
  "In-buffer document viewer.
The viewer handles PDF, PostScript, DVI, DJVU, ODF, EPUB, CBZ,
FB2, XPS and OXPS files, if the appropriate converter programs
are available (see Info node `(emacs)Document View')."
  :link '(function-link doc-view)
  :version "22.2"
  :group 'applications
  :group 'data
  :group 'multimedia
  :prefix "doc-view-")

(defcustom doc-view-ghostscript-program
  (cond
   ((memq system-type '(windows-nt ms-dos))
    (cond
     ;; Windows Ghostscript
     ((executable-find "gswin64c") "gswin64c")
     ((executable-find "gswin32c") "gswin32c")
     ;; The GS wrapper coming with TeX Live
     ((executable-find "rungs") "rungs")
     ;; The MikTeX builtin GS Check if mgs is functional for external
     ;; non-MikTeX apps.  Was available under:
     ;; http://blog.miktex.org/post/2005/04/07/Starting-mgsexe-at-the-DOS-Prompt.aspx
     ((and (executable-find "mgs")
           (eql 0 (shell-command "mgs -q -dNODISPLAY -c quit")))
      "mgs")))
   (t "gs"))
  "Program to convert PS and PDF files to PNG."
  :type 'file
  :version "27.1")

(defcustom doc-view-pdfdraw-program
  (cond
   ((executable-find "mutool") "mutool")
   ((executable-find "pdfdraw") "pdfdraw")
   ((executable-find "mudraw") "mudraw")
   (t "mudraw"))
  "Name of MuPDF's program to convert PDF files to PNG."
  :type 'file
  :version "31.1")

(defcustom doc-view-pdftotext-program-args '("-raw")
  "Parameters to give to the pdftotext command."
  :version "27.1"
  :type '(repeat string))

(defcustom doc-view-pdf->png-converter-function
  (if (executable-find doc-view-pdfdraw-program)
      #'doc-view-pdf->png-converter-mupdf
    #'doc-view-pdf->png-converter-ghostscript)
  "Function to call to convert a PDF file into a PNG file."
  :type '(radio
          (function-item :doc "Use Ghostscript"
                         doc-view-pdf->png-converter-ghostscript)
          (function-item :doc "Use MuPDF"
                         doc-view-pdf->png-converter-mupdf)
          function)
  :version "24.4")

(defcustom doc-view-mupdf-use-svg (image-type-available-p 'svg)
  "Whether to use svg images for PDF files."
  :type 'boolean
  :version "30.1")

(defcustom doc-view-djvused-program (and (executable-find "djvused")
                                         "djvused")
  "Name of \"djvused\" program to generate imenu outline for DjVu files.
This is part of DjVuLibre."
  :type '(choice (const nil) file)
  :version "31.1")

(defcustom doc-view-imenu-enabled (and (or (executable-find "mutool")
                                           (executable-find "djvused"))
                                       t)
  "Whether to generate imenu outline for PDF and DjVu files.
This uses \"mutool\" for PDF files and \"djvused\" for DjVu files."
  :type 'boolean
  :version "31.1")
(make-obsolete-variable 'doc-view-imenu-enabled
   "Imenu index is generated unconditionally when available."
   "31.1")

(defcustom doc-view-imenu-title-format "%t (%p)"
  "Format spec for imenu's display of section titles from docview documents.

The special markers '%t' and '%p' are replaced by the section
title and page number in this format string, which uses
`format-spec'.

For instance, setting this variable to \"%t\" will produce items
showing only titles and no page number."
  :type 'string
  :version "29.1")

(defcustom doc-view-imenu-flatten nil
  "Whether to flatten the list of sections in an imenu or show it nested."
  :type 'boolean
  :version "29.1")

(defface doc-view-svg-face '((t :inherit default
                                :background "white"
                                :foreground "black"))
  "Face used for SVG images.
See `doc-view-mupdf-use-svg'.

Only background and foreground colors are used as the SVG image's
descriptors; see (info \"(elisp) SVG Images\").  Custom values may
cause low-contrast issues with certain documents."
  :version "30.1")

(make-obsolete 'doc-view-svg-background 'doc-view-svg-face "30.1")
(make-obsolete 'doc-view-svg-foreground 'doc-view-svg-face "30.1")

(defcustom doc-view-ghostscript-options
  '("-dSAFER" ;; Avoid security problems when rendering files from untrusted
    ;; sources.
    "-dNOPAUSE" "-dTextAlphaBits=4"
    "-dBATCH" "-dGraphicsAlphaBits=4" "-dQUIET")
  "A list of options to give to ghostscript."
  :type '(repeat string))

(defcustom doc-view-ghostscript-device "png16m"
  "Output device to give to ghostscript."
  :type 'string
  :version "27.1")

(defcustom doc-view-resolution 100
  "Dots per inch resolution used to render the documents.
Higher values result in larger images."
  :type 'natnum)

(defvar doc-view-doc-type nil
  "The type of document in the current buffer.
Can be `dvi', `pdf', `ps', `djvu', `odf', `epub', `cbz', `fb2',
`xps' or `oxps'.")

(defvar doc-view--epub-stylesheet-watcher nil
  "File watcher for `doc-view-epub-user-stylesheet'.")

(defun doc-view--epub-reconvert (&optional _event)
  "Reconvert all epub buffers.

EVENT is unused, but necessary to work with the filenotify API."
  (dolist (x (buffer-list))
    (with-current-buffer x
      (when (eq doc-view-doc-type 'epub)
        (doc-view-reconvert-doc)))))

(defun doc-view-custom-set-epub-user-stylesheet (option-name new-value)
  "Setter for `doc-view-epub-user-stylesheet'.

Reconverts existing epub buffers when the file used as a user
stylesheet is switched, or its contents modified."
  (set-default option-name new-value)
  (file-notify-rm-watch doc-view--epub-stylesheet-watcher)
  (doc-view--epub-reconvert)
  (setq doc-view--epub-stylesheet-watcher
         (when new-value
           (file-notify-add-watch new-value '(change) #'doc-view--epub-reconvert))))

(defcustom doc-view-epub-user-stylesheet nil
  "User stylesheet to use when converting EPUB documents to PDF."
  :type '(choice (const nil)
                 (file :must-match t))
  :version "29.1"
  :set #'doc-view-custom-set-epub-user-stylesheet)

(defun doc-view-custom-set-epub-font-size (option-name new-value)
  (set-default option-name new-value)
  (doc-view--epub-reconvert))

;; FIXME: The doc-view-current-* definitions below are macros because they
;; map to accessors which we want to use via `setf' as well!
(defmacro doc-view-current-page (&optional win)
  `(image-mode-window-get 'page ,win))
(defmacro doc-view-current-info () '(image-mode-window-get 'info))
(defmacro doc-view-current-overlay () '(image-mode-window-get 'overlay))
(defmacro doc-view-current-image () '(image-mode-window-get 'image))
(defmacro doc-view-current-slice () '(image-mode-window-get 'slice))

(defvar-local doc-view--current-cache-dir nil
  "Only used internally.")

(defcustom doc-view-epub-font-size nil
  "Font size in points for EPUB layout."
  :type '(choice (const nil) integer)
  :set #'doc-view-custom-set-epub-font-size
  :version "29.1")

(defcustom doc-view-scale-internally t
  "Whether we should try to rescale images ourselves.
If nil, the document is re-rendered every time the scaling factor is modified.
This only has an effect if the image libraries linked with Emacs support
scaling."
  :version "24.4"
  :type 'boolean)

(defcustom doc-view-image-width 850
  "Default image width.
Has only an effect if `doc-view-scale-internally' is non-nil and support for
scaling is compiled into Emacs."
  :version "24.1"
  :type 'natnum)

(defcustom doc-view-dvipdfm-program "dvipdfm"
  "Program to convert DVI files to PDF.

DVI file will be converted to PDF before the resulting PDF is
converted to PNG.

If this and `doc-view-dvipdf-program' are set,
`doc-view-dvipdf-program' will be preferred."
  :type 'file)

(defcustom doc-view-dvipdf-program "dvipdf"
  "Program to convert DVI files to PDF.

DVI file will be converted to PDF before the resulting PDF is
converted to PNG.

If this and `doc-view-dvipdfm-program' are set,
`doc-view-dvipdf-program' will be preferred."
  :type 'file)

(define-obsolete-variable-alias 'doc-view-unoconv-program 'doc-view-odf->pdf-converter-program "24.4")

(defcustom doc-view-odf->pdf-converter-program
  (cond
   ((executable-find "soffice") "soffice")
   ((executable-find "unoconv") "unoconv")
   (t "soffice"))
  "Program to convert any file type readable by OpenOffice.org to PDF.

Needed for viewing OpenOffice.org (and MS Office) files."
  :version "24.4"
  :type 'file)

(defcustom doc-view-odf->pdf-converter-function
  (if (string-suffix-p "unoconv" doc-view-odf->pdf-converter-program)
      #'doc-view-odf->pdf-converter-unoconv
    #'doc-view-odf->pdf-converter-soffice)
  "Function to call to convert an ODF file into a PDF file."
  :type '(radio
          (function-item :doc "Use LibreOffice"
                         doc-view-odf->pdf-converter-soffice)
          (function-item :doc "Use unoconv"
                         doc-view-odf->pdf-converter-unoconv)
          function)
  :version "24.4")

(defcustom doc-view-ps2pdf-program "ps2pdf"
  "Program to convert PS files to PDF.

PS files will be converted to PDF before searching is possible."
  :type 'file)

(defcustom doc-view-pdftotext-program "pdftotext"
  "Program to convert PDF files to plain text.

Needed for searching."
  :type 'file)

(defcustom doc-view-cache-directory
  (expand-file-name (format "docview%d" (user-uid))
		    temporary-file-directory)
  "The base directory, where the PNG images will be saved."
  :type 'directory)

(defvar doc-view-conversion-buffer " *doc-view conversion output*"
  "The buffer where messages from the converter programs go to.")

(defcustom doc-view-conversion-refresh-interval 1
  "Interval in seconds between refreshes of the DocView buffer while converting.
After such a refresh newly converted pages will be available for
viewing.  If set to nil there won't be any refreshes and the
pages won't be displayed before conversion of the whole document
has finished."
  :type '(choice natnum
                 (const :value nil :tag "No refreshes")))

(defcustom doc-view-continuous nil
  "In Continuous mode reaching the page edge advances to next/previous page.
When non-nil, scrolling a line upward at the bottom edge of the page
moves to the next page, and scrolling a line downward at the top edge
of the page moves to the previous page."
  :type 'boolean
  :version "23.2")

;;;; Internal Variables

(defvar-local doc-view--current-converter-processes nil
  "Only used internally.")

(defun doc-view-new-window-function (winprops)
  ;; (message "New window %s for buf %s" (car winprops) (current-buffer))
  ;;
  ;; If the window configuration changed, `image-mode-reapply-winprops'
  ;; will have erased any previous property list for this window, but
  ;; without removing existing overlays for the same, so that they must
  ;; be located and erased before a new overlay is created.
  (dolist (tem (car (overlay-lists)))
    (when (and (eq (overlay-get tem 'window) (car winprops))
               (overlay-get tem 'doc-view))
      (delete-overlay tem)))
  (cl-assert (or (eq t (car winprops))
                 (eq (window-buffer (car winprops)) (current-buffer))))
  (let ((ol (image-mode-window-get 'overlay winprops)))
    (if ol
        (progn
          (setq ol (copy-overlay ol))
          ;; `ol' might actually be dead.
          (move-overlay ol (point-min) (point-max)))
      (setq ol (make-overlay (point-min) (point-max) nil t))
      (overlay-put ol 'doc-view t))
    (overlay-put ol 'window (car winprops))
    (unless (windowp (car winprops))
      ;; It's a pseudo entry.  Let's make sure it's not displayed (the
      ;; `window' property is only effective if its value is a window).
      (cl-assert (eq t (car winprops)))
      (delete-overlay ol))
    (image-mode-window-put 'overlay ol winprops)
    (when (and (windowp (car winprops))
               (stringp (overlay-get ol 'display))
               (null doc-view--current-converter-processes))
      ;; We're not displaying an image yet, so let's do so.  This happens when
      ;; the buffer is displayed for the first time.
      ;; Don't do it if there's a conversion is running, since in that case, it
      ;; will be done later.
      (with-selected-window (car winprops)
        (doc-view-goto-page (image-mode-window-get 'page t))))))

(defvar-local doc-view--current-files nil
  "Only used internally.")

(defvar-local doc-view--current-timer nil
  "Only used internally.")

(defvar-local doc-view--current-search-matches nil
  "Only used internally.")

(defvar doc-view--pending-cache-flush nil
  "Only used internally.")

(defvar doc-view--buffer-file-name nil
  "Only used internally.
The file name used for conversion.  Normally it's the same as
`buffer-file-name', but for remote files, compressed files and
files inside an archive it is a temporary copy of
the (uncompressed, extracted) file residing in
`doc-view-cache-directory'.")

(defvar doc-view-single-page-converter-function nil
  "Function to call to convert a single page of the document to a bitmap file.
May operate on the source document or on some intermediate (typically PDF)
conversion of it.")

(defvar-local doc-view--image-type nil
  "The type of image in the current buffer.
Can be `png' or `tiff'.")

(defvar-local doc-view--image-file-pattern nil
  "The `format' pattern of image file names.
Typically \"page-%s.png\".")

;;;; DocView Keymaps

(defvar-keymap doc-view-mode-map
  :doc "Keymap used by `doc-view-mode' when displaying a doc as a set of images."
  :parent image-mode-map
  ;; Navigation in the document
  "n"       #'doc-view-next-page
  "p"       #'doc-view-previous-page
  "<next>"  #'forward-page
  "<prior>" #'backward-page
  "<remap> <forward-page>"  #'doc-view-next-page
  "<remap> <backward-page>" #'doc-view-previous-page
  "SPC"     #'doc-view-scroll-up-or-next-page
  "S-SPC"   #'doc-view-scroll-down-or-previous-page
  "DEL"     #'doc-view-scroll-down-or-previous-page
  "C-n"     #'doc-view-next-line-or-next-page
  "<down>"  #'doc-view-next-line-or-next-page
  "C-p"     #'doc-view-previous-line-or-previous-page
  "<up>"    #'doc-view-previous-line-or-previous-page
  "M-<"     #'doc-view-first-page
  "M->"     #'doc-view-last-page
  "<remap> <goto-line>" #'doc-view-goto-page
  "RET"     #'image-next-line
  ;; Zoom in/out.
  "+"       #'doc-view-enlarge
  "="       #'doc-view-enlarge
  "-"       #'doc-view-shrink
  "0"       #'doc-view-scale-reset
  "<remap> <text-scale-adjust>" #'doc-view-scale-adjust
  ;; Fit the image to the window
  "W"       #'doc-view-fit-width-to-window
  "H"       #'doc-view-fit-height-to-window
  "P"       #'doc-view-fit-page-to-window
  "F"       #'doc-view-fit-window-to-page ;F = frame
  ;; Killing the buffer (and the process)
  "k"       #'image-kill-buffer
  "K"       #'doc-view-kill-proc
  ;; Slicing the image
  "c s"     #'doc-view-set-slice
  "c m"     #'doc-view-set-slice-using-mouse
  "c b"     #'doc-view-set-slice-from-bounding-box
  "c r"     #'doc-view-reset-slice
  ;; Centering the image
  "c h"     #'doc-view-center-page-horizontally
  "c v"     #'doc-view-center-page-vertically
  ;; Searching
  "C-s"     #'doc-view-search
  "<find>"  #'doc-view-search
  "C-r"     #'doc-view-search-backward
  ;; Show the tooltip
  "C-t"     #'doc-view-show-tooltip
  ;; Toggle between text and image display or editing
  "C-c C-c" #'doc-view-toggle-display
  ;; Open a new buffer with doc's text contents
  "C-c C-t" #'doc-view-open-text
  "r"       #'revert-buffer
  ;; Registers
  "m"       #'doc-view-page-to-register
  "'"       #'doc-view-jump-to-register)

(define-obsolete-function-alias 'doc-view-revert-buffer #'revert-buffer "27.1")
(defvar revert-buffer-preserve-modes)
(defun doc-view--revert-buffer (orig-fun &rest args)
  "Preserve the buffer's current mode and check PDF sanity."
  (if (< undo-outer-limit (* 2 (buffer-size)))
      ;; It's normal for this operation to result in a very large undo entry.
      (setq-local undo-outer-limit (* 2 (buffer-size))))
  (cl-labels ((revert ()
                (let ((revert-buffer-preserve-modes t))
                  (apply orig-fun args)
                  ;; Update the cached version of the pdf file, too.
                  ;; This is the one that's used when rendering
                  ;; (bug#26996).  doc-view--buffer-file-name is nil in
                  ;; the case where we've switched to the editing mode
                  ;; (bug#76478).  In that case, we'll update the cached
                  ;; version when switching back to doc-view-mode.
                  (when (and doc-view--buffer-file-name
                             (not (equal buffer-file-name
                                         doc-view--buffer-file-name)))
                    ;; FIXME: Lars says he needed to recreate
                    ;; the dir, we should figure out why.
                    (doc-view-make-safe-dir doc-view-cache-directory)
                    (write-region nil nil doc-view--buffer-file-name)))))
    (if (and (eq 'pdf doc-view-doc-type)
             (executable-find "pdfinfo"))
        ;; We don't want to revert if the PDF file is corrupted which
        ;; might happen when it is currently recompiled from a tex
        ;; file.  (TODO: We'd like to have something like that also
        ;; for other types, at least PS, but I don't know a good way
        ;; to test if a PS file is complete.)
        (if (eql 0 (call-process "pdfinfo" nil nil nil
                                 doc-view--buffer-file-name))
            (revert)
          (when (called-interactively-p 'interactive)
            (message "Can't revert right now because the file is corrupted.")))
      (revert))))


(easy-menu-define doc-view-menu doc-view-mode-map
  "Menu for Doc View mode."
  '("DocView"
    ["Next page"                doc-view-next-page
     :help                      "Go to the next page"]
    ["Previous page"            doc-view-previous-page
     :help                      "Go to the previous page"]
    ("Other Navigation"
     ["Go to page..."           doc-view-goto-page
      :help                     "Go to specific page"]
     "---"
     ["First page"              doc-view-first-page
      :help                     "View the first page"]
     ["Last page"               doc-view-last-page
      :help                     "View the last page"]
     "---"
     ["Move forward"            doc-view-scroll-up-or-next-page
      :help                     "Scroll page up or go to next page"]
     ["Move backward"           doc-view-scroll-down-or-previous-page
      :help                     "Scroll page down or go to previous page"])
    ("Continuous Scrolling"
     ["Off"                     (setq doc-view-continuous nil)
      :style radio :selected    (eq doc-view-continuous nil)
      :help                     "Scrolling stops at page beginning and end"]
     ["On"		        (setq doc-view-continuous t)
      :style radio :selected    (eq doc-view-continuous t)
      :help                     "Scrolling continues to next or previous page"]
     "---"
     ["Save as Default"         (customize-save-variable 'doc-view-continuous doc-view-continuous)
      :help                     "Save current continuous scrolling option as default"]
     )
    "---"
    ("Toggle edit/display"
     ["Edit document"           doc-view-toggle-display
      :style radio :selected    (eq major-mode 'doc-view--text-view-mode)]
     ["Display document"        (lambda ()) ; ignore but show no keybinding
      :style radio :selected    (eq major-mode 'doc-view-mode)])
    ("Adjust Display"
     ["Fit to window"           doc-view-fit-page-to-window
      :help                     "Fit the image to the window"]
     ["Fit width"               doc-view-fit-width-to-window
      :help                     "Fit the image width to the window width"]
     ["Fit height"              doc-view-fit-height-to-window
      :help                     "Fit the image height to the window height"]
     "---"
     ["Enlarge"                 doc-view-enlarge
      :help                     "Enlarge the document"]
     ["Shrink"                  doc-view-shrink
      :help                     "Shrink the document"]
     "---"
     ["Set Slice"               doc-view-set-slice-using-mouse
      :help                     "Set the slice of the images that should be displayed"]
     ["Set Slice (BoundingBox)" doc-view-set-slice-from-bounding-box
      :help                     "Set the slice from the document's BoundingBox information"]
     ["Set Slice (manual)"	doc-view-set-slice
      :help                     "Set the slice of the images that should be displayed"]
     ["Reset Slice"		doc-view-reset-slice
      :help                     "Reset the current slice"
      :enabled                  (image-mode-window-get 'slice)])
    "---"
    ["New Search"               doc-view-new-search
     :help                      "Initiate a new search"]
    ["Search Forward"           doc-view-search
     :help                      "Jump to the next match or initiate a new search"]
    ["Search Backward"          doc-view-search-backward
     :help                      "Jump to the previous match or initiate a new search"]
    ))

(defvar-keymap doc-view-minor-mode-map
  :doc "Keymap used by `doc-view-minor-mode'."
  ;; Toggle between text and image display or editing
  "C-c C-c" #'doc-view-toggle-display)

(easy-menu-define doc-view-minor-mode-menu doc-view-minor-mode-map
  "Menu for Doc View minor mode."
  '("DocView (edit)"
    ("Toggle edit/display"
     ["Edit document"           (lambda ()) ; ignore but show no keybinding
      ;; This is always selected since its menu is singular to the
      ;; display minor mode.
      :style radio :selected    t]
     ["Display document"        doc-view-toggle-display
      :style radio :selected    (eq major-mode 'doc-view-mode)])
    ["Exit DocView Mode" doc-view-minor-mode]))

(defvar doc-view-tool-bar-map
  (let ((map (make-sparse-keymap)))
    ;; Most of these items are the same as in the default tool bar
    ;; map, but with extraneous items removed, and with extra search
    ;; and navigation items.
    (tool-bar-local-item-from-menu 'find-file "new" map
                                   nil :label "New File"
			           :vert-only t)
    (tool-bar-local-item-from-menu 'menu-find-file-existing "open" map
                                   nil :label "Open" :vert-only t)
    (tool-bar-local-item-from-menu 'dired "diropen" map nil :vert-only t)
    (tool-bar-local-item-from-menu 'kill-this-buffer "close" map nil
                                   :vert-only t)
    (define-key-after map [separator-1] menu-bar-separator)
    (tool-bar-local-item-from-menu 'doc-view-new-search "search"
			           map doc-view-mode-map :vert-only t
                                   :help "Start a new search query.")
    (tool-bar-local-item-from-menu 'doc-view-search-backward "left-arrow"
			           map doc-view-mode-map
                                   :vert-only t
                                   :enable 'doc-view--current-search-matches
                                   :help "Move to the last search result.")
    (tool-bar-local-item-from-menu 'doc-view-search "right-arrow"
			           map doc-view-mode-map :vert-only t
                                   :enable 'doc-view--current-search-matches
                                   :help "Move to the next search result.")
    (define-key-after map [separator-2] menu-bar-separator)
    (tool-bar-local-item-from-menu 'doc-view-previous-page "last-page"
                                   map doc-view-mode-map :vert-only t
                                   :enable '(> (doc-view-current-page) 1)
                                   :help "Move to the previous page.")
    (tool-bar-local-item-from-menu 'doc-view-next-page "next-page"
                                   map doc-view-mode-map :vert-only t
                                   :enable '(< (doc-view-current-page)
                                               (doc-view-last-page-number))
                                   :help "Move to the next page.")
    map)
  "Like the default `tool-bar-map', but with additions for DocView.")

;;;; Navigation Commands

(defun doc-view-last-page-number ()
  (length doc-view--current-files))

(defun doc-view-goto-page (page)
  "View the page given by PAGE."
  (interactive "nPage: ")
  (let ((len (doc-view-last-page-number)))
    (if (< page 1)
	(setq page 1)
      (when (and (> page len)
                 ;; As long as the converter is running, we don't know
                 ;; how many pages will be available.
                 (null doc-view--current-converter-processes))
	(setq page len)))
    (force-mode-line-update)            ;To update `current-page'.
    (setf (doc-view-current-page) page
	  (doc-view-current-info)
	  (concat
	   (propertize
	    (format "Page %d of %d." page len) 'face 'bold)
	   ;; Tell user if converting isn't finished yet
           (and doc-view--current-converter-processes
                " (still converting...)")
           ;; Display context infos if this page matches the last search
           (when (and doc-view--current-search-matches
                      (assq page doc-view--current-search-matches))
             (concat "\n" (propertize "Search matches:" 'face 'bold)
		     (let ((contexts ""))
		       (dolist (m (cdr (assq page
					     doc-view--current-search-matches)))
			 (setq contexts (concat contexts "\n  - \"" m "\"")))
		       contexts)))))
    ;; Update the buffer
    ;; We used to find the file name from doc-view--current-files but
    ;; that's not right if the pages are not generated sequentially
    ;; or if the page isn't in doc-view--current-files yet.
    (let ((file (expand-file-name
                 (format doc-view--image-file-pattern page)
                 (doc-view--current-cache-dir))))
      (doc-view-insert-image file :pointer 'arrow)
      (when (and (not (file-exists-p file))
                 doc-view--current-converter-processes)
        ;; The PNG file hasn't been generated yet.
        (funcall doc-view-single-page-converter-function
		 doc-view--buffer-file-name file page
		 (let ((win (selected-window)))
		   (lambda ()
		     (and (eq (current-buffer) (window-buffer win))
			  ;; If we changed page in the mean
			  ;; time, don't mess things up.
			  (eq (doc-view-current-page win) page)
			  ;; Make sure we don't infloop.
			  (file-readable-p file)
			  (with-selected-window win
			    (doc-view-goto-page page))))))))
    (overlay-put (doc-view-current-overlay)
		 'help-echo (doc-view-current-info))))

(defun doc-view-next-page (&optional arg)
  "Browse ARG pages forward."
  (interactive "p")
  (doc-view-goto-page (+ (doc-view-current-page) (or arg 1))))

(defun doc-view-previous-page (&optional arg)
  "Browse ARG pages backward."
  (interactive "p")
  (doc-view-goto-page (- (doc-view-current-page) (or arg 1))))

(defun doc-view-first-page ()
  "View the first page."
  (interactive)
  (doc-view-goto-page 1))

(defun doc-view-last-page ()
  "View the last page."
  (interactive)
  (doc-view-goto-page (doc-view-last-page-number)))

(defun doc-view-scroll-up-or-next-page (&optional arg)
  "Scroll page up ARG lines if possible, else goto next page.
When `doc-view-continuous' is non-nil, scrolling upward
at the bottom edge of the page moves to the next page.
Otherwise, goto next page only on typing SPC (ARG is nil)."
  (interactive "P")
  (if (or doc-view-continuous (null arg))
      (let ((hscroll (window-hscroll))
	    (cur-page (doc-view-current-page)))
	(when (= (window-vscroll nil t) (image-scroll-up arg))
	  (doc-view-next-page)
	  (when (/= cur-page (doc-view-current-page))
	    (image-bob)
	    (image-bol 1))
	  (set-window-hscroll (selected-window) hscroll)))
    (image-scroll-up arg)))

(defun doc-view-scroll-down-or-previous-page (&optional arg)
  "Scroll page down ARG lines if possible, else goto previous page.
When `doc-view-continuous' is non-nil, scrolling downward
at the top edge of the page moves to the previous page.
Otherwise, goto previous page only on typing DEL (ARG is nil)."
  (interactive "P")
  (if (or doc-view-continuous (null arg))
      (let ((hscroll (window-hscroll))
	    (cur-page (doc-view-current-page)))
	(when (= (window-vscroll nil t) (image-scroll-down arg))
	  (doc-view-previous-page)
	  (when (/= cur-page (doc-view-current-page))
	    (image-eob)
	    (image-bol 1))
	  (set-window-hscroll (selected-window) hscroll)))
    (image-scroll-down arg)))

(defun doc-view-next-line-or-next-page (&optional arg)
  "Scroll upward by ARG lines if possible, else goto next page.
When `doc-view-continuous' is non-nil, scrolling a line upward
at the bottom edge of the page moves to the next page."
  (interactive "p")
  (if doc-view-continuous
      (let ((hscroll (window-hscroll))
	    (cur-page (doc-view-current-page)))
	(when (= (window-vscroll nil t) (image-next-line arg))
	  (doc-view-next-page)
	  (when (/= cur-page (doc-view-current-page))
	    (image-bob)
	    (image-bol 1))
	  (set-window-hscroll (selected-window) hscroll)))
    (image-next-line arg)))

(defun doc-view-previous-line-or-previous-page (&optional arg)
  "Scroll downward by ARG lines if possible, else goto previous page.
When `doc-view-continuous' is non-nil, scrolling a line downward
at the top edge of the page moves to the previous page."
  (interactive "p")
  (if doc-view-continuous
      (let ((hscroll (window-hscroll))
	    (cur-page (doc-view-current-page)))
	(when (= (window-vscroll nil t) (image-previous-line arg))
	  (doc-view-previous-page)
	  (when (/= cur-page (doc-view-current-page))
	    (image-eob)
	    (image-bol 1))
	  (set-window-hscroll (selected-window) hscroll)))
    (image-previous-line arg)))

;;;; Utility Functions

(defun doc-view-kill-proc ()
  "Kill the current converter process(es)."
  (interactive)
  (while (consp doc-view--current-converter-processes)
    (ignore-errors ;; Some entries might not be processes, and maybe
                    ; some are dead already?
      (kill-process (pop doc-view--current-converter-processes))))
  (when doc-view--current-timer
    (cancel-timer doc-view--current-timer)
    (setq doc-view--current-timer nil))
  (setq mode-line-process nil))

(define-obsolete-function-alias 'doc-view-kill-proc-and-buffer
  #'image-kill-buffer "25.1")

(defun doc-view-make-safe-dir (dir)
  (condition-case nil
      ;; Create temp files with strict access rights.  It's easy to
      ;; loosen them later, whereas it's impossible to close the
      ;; time-window of loose permissions otherwise.
      (with-file-modes #o0700 (make-directory dir))
    (file-already-exists
     ;; In case it was created earlier with looser rights.
     ;; We could check the mode info returned by file-attributes, but it's
     ;; a pain to parse and it may not tell you what we want under
     ;; non-standard file-systems.  So let's just say what we want and let
     ;; the underlying C code and file-system figure it out.
     ;; This also ends up checking a bunch of useful conditions: it makes
     ;; sure we have write-access to the directory and that we own it, thus
     ;; closing a bunch of security holes.
     (condition-case error
	 (set-file-modes dir #o0700 'nofollow)
       (file-error
	(error
	 (format "Unable to use temporary directory %s: %s"
		 dir (mapconcat #'identity (cdr error) " "))))))))

(defun doc-view--current-cache-dir ()
  "Return the directory where the png files of the current doc should be saved.
It's a subdirectory of `doc-view-cache-directory'."
  (if doc-view--current-cache-dir
      doc-view--current-cache-dir
    ;; Try and make sure doc-view-cache-directory exists and is safe.
    (doc-view-make-safe-dir doc-view-cache-directory)
    ;; Now compute the subdirectory to use.
    (setq doc-view--current-cache-dir
	  (file-name-as-directory
	   (expand-file-name
	    (concat (thread-last
                      (file-name-nondirectory doc-view--buffer-file-name)
                      ;; bug#13679
                      (subst-char-in-string ?% ?_)
                      ;; arc-mode concatenates archive name and file name
                      ;; with colon, which isn't allowed on MS-Windows.
                      (subst-char-in-string ?: ?_))
                    "-"
                    (let ((file doc-view--buffer-file-name))
                      (with-temp-buffer
                        (set-buffer-multibyte nil)
                        (insert-file-contents-literally file)
                        (md5 (current-buffer)))))
            doc-view-cache-directory)))))

;;;###autoload
(defun doc-view-mode-p (type)
  "Return non-nil if document type TYPE is available for `doc-view'.
Document types are symbols like `dvi', `ps', `pdf', `epub',
`cbz', `fb2', `xps', `oxps', or`odf' (any OpenDocument format)."
  (and (display-graphic-p)
       (image-type-available-p 'png)
       (cond
	((eq type 'dvi)
	 (and (doc-view-mode-p 'pdf)
	      (or (and doc-view-dvipdf-program
		       (executable-find doc-view-dvipdf-program))
		  (and doc-view-dvipdfm-program
		       (executable-find doc-view-dvipdfm-program)))))
	((memq type '(postscript ps eps pdf))
         (or (and doc-view-ghostscript-program
	          (executable-find doc-view-ghostscript-program))
             ;; for pdf also check for `doc-view-pdfdraw-program'
             (when (eq type 'pdf)
               (and doc-view-pdfdraw-program
                    (executable-find doc-view-pdfdraw-program)))))
	((eq type 'odf)
         (and (executable-find doc-view-odf->pdf-converter-program)
	      (doc-view-mode-p 'pdf)))
	((eq type 'djvu)
	 (executable-find "ddjvu"))
        ((memq type '(epub cbz fb2 xps oxps))
         ;; first check if `doc-view-pdfdraw-program' is set to mutool
         (and (string= doc-view-pdfdraw-program "mutool")
              (executable-find "mutool")))
	(t ;; unknown image type
	 nil))))

;;;; Conversion Functions

(defvar doc-view-shrink-factor 1.125)

(defun doc-view-enlarge (factor)
  "Enlarge the document by FACTOR."
  (interactive (list doc-view-shrink-factor))
  (if doc-view-scale-internally
      (let ((new (ceiling (* factor doc-view-image-width))))
        (unless (equal new doc-view-image-width)
          (setq-local doc-view-image-width new)
          (doc-view-insert-image
           (plist-get (cdr (doc-view-current-image)) :file)
           :width doc-view-image-width)))
    (let ((new (ceiling (* factor doc-view-resolution))))
      (unless (equal new doc-view-resolution)
        (setq-local doc-view-resolution new)
        (doc-view-reconvert-doc)))))

(defun doc-view-shrink (factor)
  "Shrink the document by FACTOR."
  (interactive (list doc-view-shrink-factor))
  (doc-view-enlarge (/ 1.0 factor)))

(defun doc-view-scale-reset ()
  "Reset the document size/zoom level to the initial one."
  (interactive)
  (if doc-view-scale-internally
      (progn
	(kill-local-variable 'doc-view-image-width)
	(doc-view-insert-image
	 (plist-get (cdr (doc-view-current-image)) :file)
	 :width doc-view-image-width))
    (kill-local-variable 'doc-view-resolution)
    (doc-view-reconvert-doc)))

(defun doc-view-scale-adjust (factor)
  "Adjust the scale of the DocView page images by FACTOR.
FACTOR defaults to `doc-view-shrink-factor'.

The actual adjustment made depends on the final component of the
keybinding used to invoke the command, with all modifiers removed:

   +, =   Increase the image scale by FACTOR
   -      Decrease the image scale by FACTOR
   0      Reset the image scale to the initial scale"
  (interactive (list doc-view-shrink-factor))
  (let ((ev last-command-event)
	(echo-keystrokes nil))
    (pcase (event-basic-type ev)
      ((or ?+ ?=) (doc-view-enlarge factor))
      (?-         (doc-view-shrink factor))
      (?0         (doc-view-scale-reset)))))

(defun doc-view-fit-width-to-window ()
  "Fit the image width to the window width."
  (interactive)
  (let ((win-width (- (nth 2 (window-inside-pixel-edges))
                      (nth 0 (window-inside-pixel-edges))))
        (slice (doc-view-current-slice)))
    (if (not slice)
        (let ((img-width (car (image-display-size
                               (image-get-display-property) t))))
          (doc-view-enlarge (/ (float win-width) (float img-width))))

      ;; If slice is set
      (let* ((slice-width (nth 2 slice))
             (scale-factor (/ (float win-width) (float slice-width)))
             (new-slice (mapcar (lambda (x) (ceiling (* scale-factor x))) slice)))

        (doc-view-enlarge scale-factor)
        (setf (doc-view-current-slice) new-slice)
        (doc-view-goto-page (doc-view-current-page))))))

(defun doc-view-fit-height-to-window ()
  "Fit the image height to the window height."
  (interactive)
  (let ((win-height (- (nth 3 (window-inside-pixel-edges))
                       (nth 1 (window-inside-pixel-edges))))
        (slice (doc-view-current-slice)))
    (if (not slice)
        (let ((img-height (cdr (image-display-size
                                (image-get-display-property) t))))
          ;; When users call 'doc-view-fit-height-to-window',
          ;; they might want to go to next page by typing SPC
          ;; ONLY once. So I used '(- win-height 1)' instead of
          ;; 'win-height'
          (doc-view-enlarge (/ (float (- win-height 1)) (float img-height))))

      ;; If slice is set
      (let* ((slice-height (nth 3 slice))
             (scale-factor (/ (float (- win-height 1)) (float slice-height)))
             (new-slice (mapcar (lambda (x) (ceiling (* scale-factor x))) slice)))

        (doc-view-enlarge scale-factor)
        (setf (doc-view-current-slice) new-slice)
        (doc-view-goto-page (doc-view-current-page))))))

(defun doc-view-fit-page-to-window ()
  "Fit the image to the window.
More specifically, this function enlarges image by:

min {(window-width / image-width), (window-height / image-height)} times."
  (interactive)
  (let ((win-width (- (nth 2 (window-inside-pixel-edges))
                      (nth 0 (window-inside-pixel-edges))))
        (win-height (- (nth 3 (window-inside-pixel-edges))
                       (nth 1 (window-inside-pixel-edges))))
        (slice (doc-view-current-slice)))
    (if (not slice)
        (let ((img-width (car (image-display-size
                               (image-get-display-property) t)))
              (img-height (cdr (image-display-size
                                (image-get-display-property) t))))
          (doc-view-enlarge (min (/ (float win-width) (float img-width))
                                 (/ (float (- win-height 1))
                                    (float img-height)))))
      ;; If slice is set
      (let* ((slice-width (nth 2 slice))
             (slice-height (nth 3 slice))
             (scale-factor (min (/ (float win-width) (float slice-width))
                                (/ (float (- win-height 1))
                                   (float slice-height))))
             (new-slice (mapcar (lambda (x) (ceiling (* scale-factor x))) slice)))
        (doc-view-enlarge scale-factor)
        (setf (doc-view-current-slice) new-slice)
        (doc-view-goto-page (doc-view-current-page))))))

(defun doc-view-fit-window-to-page ()
  "Resize selected window so it just fits the current page.
Resize the containing frame if needed."
  (interactive)
  (let* ((slice (doc-view-current-slice))
         (img-width  (if slice (nth 2 slice)
                       (car (image-display-size
                             (image-get-display-property) t))))
         (img-height (if slice (nth 3 slice)
                       (cdr (image-display-size
                             (image-get-display-property) t))))
         (win-width  (- (nth 2 (window-inside-pixel-edges))
                        (nth 0 (window-inside-pixel-edges))))
         (win-height (- (nth 3 (window-inside-pixel-edges))
                        (nth 1 (window-inside-pixel-edges))))
         (width-diff  (- img-width  win-width))
         (height-diff (- img-height win-height))
         (new-frame-params
          ;; If we can't resize the window, try and resize the frame.
          ;; We used to compare the `window-width/height` and the
          ;; `frame-width/height` instead of catching the errors, but
          ;; it's too fiddly (e.g. in the presence of the miniwindow,
          ;; the height the frame should be equal to the height of the
          ;; root window +1).
          (append
           (condition-case nil
               (progn
                 (enlarge-window (/ width-diff (frame-char-width)) 'horiz)
                 nil)
             (error
              `((width  . (text-pixels
                           . ,(+ (frame-text-width) width-diff))))))
           (condition-case nil
               (progn
                 (enlarge-window (/ height-diff (frame-char-height)) nil)
                 nil)
             (error
              `((height  . (text-pixels
                            . ,(+ (frame-text-height) height-diff)))))))))
    (when new-frame-params
      (modify-frame-parameters (selected-frame) new-frame-params))))

(defun doc-view-center-page-horizontally ()
  "Center page horizontally when page is wider than window."
  (interactive)
  (let ((page-width (car (image-size (doc-view-current-image) 'pixel)))
        (window-width (window-body-width nil 'pixel))
        ;; How much do we scroll in order to center the page?
        (pixel-hscroll 0)
        ;; How many pixels are there in a column?
        (col-in-pixel (/ (window-body-width nil 'pixel)
                         (window-body-width nil))))
    (when (> page-width window-width)
      (setq pixel-hscroll (/ (- page-width window-width) 2))
      (set-window-hscroll (selected-window)
                          (/ pixel-hscroll col-in-pixel)))))

(defun doc-view-center-page-vertically ()
  "Center page vertically when page is wider than window."
  (interactive)
  (let ((page-height (cdr (image-size (doc-view-current-image) 'pixel)))
        (window-height (window-body-height nil 'pixel))
        ;; How much do we scroll in order to center the page?
        (pixel-scroll 0))
    (when (> page-height window-height)
      (setq pixel-scroll (/ (- page-height window-height) 2))
      (set-window-vscroll (selected-window) pixel-scroll 'pixel))))

(defun doc-view-reconvert-doc ()
  "Reconvert the current document.
Should be invoked when the cached images aren't up-to-date."
  (interactive)
  (doc-view-kill-proc)
  ;; Clear the old cached files
  (when (file-exists-p (doc-view--current-cache-dir))
    (delete-directory (doc-view--current-cache-dir) 'recursive))
  (kill-local-variable 'doc-view-last-page-number)
  (doc-view-initiate-display))

(defun doc-view-sentinel (proc event)
  "Generic sentinel for doc-view conversion processes."
  (if (not (string-match "finished" event))
      (message "DocView: process %s changed status to %s."
               (process-name proc)
	       (if (string-match "\\(.+\\)\n?\\'" event)
		   (match-string 1 event)
		 event))
    (when (buffer-live-p (process-get proc 'buffer))
      (with-current-buffer (process-get proc 'buffer)
        (setq doc-view--current-converter-processes
              (delq proc doc-view--current-converter-processes))
        (setq mode-line-process
              (if doc-view--current-converter-processes
                  (format ":%s" (car doc-view--current-converter-processes))))
        (funcall (process-get proc 'callback))))))

(defun doc-view-start-process (name program args callback)
  ;; Make sure the process is started in an existing directory, (rather than
  ;; some file-name-handler-managed dir, for example).
  (let* ((default-directory (or (unhandled-file-name-directory
                                 default-directory)
			        (expand-file-name "~/")))
         (proc (apply #'start-process name doc-view-conversion-buffer
                      program args)))
    (push proc doc-view--current-converter-processes)
    (setq mode-line-process (list (format ":%s" proc)))
    (set-process-sentinel proc 'doc-view-sentinel)
    (process-put proc 'buffer   (current-buffer))
    (process-put proc 'callback callback)))

(defun doc-view-dvi->pdf (dvi pdf callback)
  "Convert DVI to PDF asynchronously and call CALLBACK when finished."
  ;; Prefer dvipdf over dvipdfm, because the latter has problems if the DVI
  ;; references and includes other PS files.
  (if (and doc-view-dvipdf-program
	   (executable-find doc-view-dvipdf-program))
      (doc-view-start-process "dvi->pdf" doc-view-dvipdf-program
			      (list dvi pdf)
			      callback)
    (doc-view-start-process "dvi->pdf" doc-view-dvipdfm-program
			    (list "-o" pdf dvi)
			    callback)))

(defun doc-view-pdf-password-protected-ghostscript-p (pdf)
  "Return non-nil if a PDF file is password-protected.
The test is performed using `doc-view-ghostscript-program'."
  (with-temp-buffer
    (apply #'call-process doc-view-ghostscript-program nil (current-buffer)
           nil `(,@doc-view-ghostscript-options
                 "-sNODISPLAY"
                 ,pdf))
    (goto-char (point-min))
    (search-forward "This file requires a password for access." nil t)))

(defun doc-view-pdf->png-converter-ghostscript (pdf png page callback)
  (let ((pdf-passwd (if (doc-view-pdf-password-protected-ghostscript-p pdf)
                        (read-passwd "Enter password for PDF file: "))))
    (doc-view-start-process
     "pdf/ps->png" doc-view-ghostscript-program
     `(,@doc-view-ghostscript-options
       ,(concat "-sDEVICE=" doc-view-ghostscript-device)
       ,(format "-r%d" (round doc-view-resolution))
       ,@(if page `(,(format "-dFirstPage=%d" page)))
       ,@(if page `(,(format "-dLastPage=%d" page)))
       ,@(if pdf-passwd `(,(format "-sPDFPassword=%s" pdf-passwd)))
       ,(concat "-sOutputFile=" png)
       ,pdf)
     callback)))

(defalias 'doc-view-ps->png-converter-ghostscript
  'doc-view-pdf->png-converter-ghostscript)

(defun doc-view-djvu->tiff-converter-ddjvu (djvu tiff page callback)
  "Convert PAGE of a DJVU file to bitmap(s) asynchronously.
Call CALLBACK with no arguments when done.
If PAGE is nil, convert the whole document."
  (doc-view-start-process
   "djvu->tiff" "ddjvu"
   `("-format=tiff"
     ;; ddjvu only accepts the range 1-999.
     ,(format "-scale=%d" (round doc-view-resolution))
     ;; -eachpage was only added after djvulibre-3.5.25.3!
     ,@(unless page '("-eachpage"))
     ,@(if page `(,(format "-page=%d" page)))
     ,djvu
     ,tiff)
   callback))

(defun doc-view-pdfdraw-program-subcommand ()
  "Return the mutool subcommand replacing mudraw.
Recent MuPDF distributions replaced `mudraw' with `mutool draw'."
  (when (string-match "mutool[^/\\]*$" doc-view-pdfdraw-program)
    '("draw")))

(defun doc-view-pdf-password-protected-pdfdraw-p (pdf)
  "Return non-nil if a PDF file is password-protected.
The test is performed using `doc-view-pdfdraw-program'."
  (with-temp-buffer
    (apply #'call-process doc-view-pdfdraw-program nil (current-buffer) nil
           `(,@(doc-view-pdfdraw-program-subcommand)
             ,(concat "-o" null-device)
             ;; In case PDF isn't password-protected, "draw" only one page.
             ,pdf "1"))
    (goto-char (point-min))
    (search-forward "error: cannot authenticate password" nil t)))

(defun doc-view-pdf->png-converter-mupdf (pdf png page callback)
  (let* ((pdf-passwd (if (doc-view-pdf-password-protected-pdfdraw-p pdf)
                         (read-passwd "Enter password for PDF file: ")))
         (options `(,(concat "-o" png)
                    ,(format "-r%d" (round doc-view-resolution))
                    ,@(if pdf-passwd `("-p" ,pdf-passwd)))))
    (when (eq doc-view-doc-type 'epub)
      (when doc-view-epub-font-size
        (setq options (append options
                              (list (format "-S%s" doc-view-epub-font-size)))))
      (when doc-view-epub-user-stylesheet
        (setq options
              (append options
                      (list (format "-U%s"
                                    (expand-file-name
                                     doc-view-epub-user-stylesheet)))))))
    (doc-view-start-process
     (concat "pdf->" (symbol-name doc-view--image-type))
     doc-view-pdfdraw-program
     `(,@(doc-view-pdfdraw-program-subcommand)
       ,@options
       ,pdf
       ,@(if page `(,(format "%d" page))))
     callback)))

(defun doc-view-odf->pdf-converter-unoconv (odf callback)
  "Convert ODF to PDF asynchronously and call CALLBACK when finished.
The converted PDF is put into the current cache directory, and it
is named like ODF with the extension turned to pdf."
  (doc-view-start-process "odf->pdf" doc-view-odf->pdf-converter-program
			  (list "-f" "pdf" "-o" (doc-view--current-cache-dir) odf)
			  callback))

(defun doc-view-odf->pdf-converter-soffice (odf callback)
  "Convert ODF to PDF asynchronously and call CALLBACK when finished.
The converted PDF is put into the current cache directory, and it
is named like ODF with the extension turned to pdf."
  ;; FIXME: soffice doesn't work when there's another running
  ;; LibreOffice instance, in which case it returns success without
  ;; actually doing anything.  See LibreOffice bug
  ;; https://bugs.freedesktop.org/show_bug.cgi?id=37531.  A workaround
  ;; is to start soffice with a separate UserInstallation directory.
  (let ((tmp-user-install-dir (make-temp-file "libreoffice-docview" t)))
    (doc-view-start-process "odf->pdf" doc-view-odf->pdf-converter-program
			    (list
			     (concat "-env:UserInstallation=file://"
                                     ;; The URL must be
                                     ;; file:///C:/tmp/dir on Windows.
                                     ;; https://wiki.documentfoundation.org/UserProfile.
                                     (when (eq system-type 'windows-nt)
                                       "/")
				     tmp-user-install-dir)
			     "--headless" "--convert-to" "pdf"
			     "--outdir" (doc-view--current-cache-dir) odf)
			    (lambda ()
			      (delete-directory tmp-user-install-dir t)
			      (funcall callback)))))

(defun doc-view-pdf/ps->png (pdf-ps png)
  ;; FIXME: Fix name and docstring to account for djvu&tiff.
  "Convert PDF-PS to PNG asynchronously."
  (funcall
   (pcase doc-view-doc-type
     ((or 'pdf 'odf 'epub 'cbz 'fb2 'xps 'oxps 'dvi)
      doc-view-pdf->png-converter-function)
     ('djvu #'doc-view-djvu->tiff-converter-ddjvu)
     (_ #'doc-view-ps->png-converter-ghostscript))
   pdf-ps png nil
   (let ((resolution doc-view-resolution))
     (lambda ()
       ;; Only create the resolution file when it's all done, so it also
       ;; serves as a witness that the conversion is complete.
       (write-region (prin1-to-string resolution) nil
                     (expand-file-name "resolution.el"
                                       (doc-view--current-cache-dir))
                     nil 'silently)
       (when doc-view--current-timer
         (cancel-timer doc-view--current-timer)
         (setq doc-view--current-timer nil))
       (doc-view-display (current-buffer) 'force))))

  ;; Update the displayed pages as soon as they're done generating.
  (when doc-view-conversion-refresh-interval
    (setq doc-view--current-timer
          (run-at-time "1 secs" doc-view-conversion-refresh-interval
                       'doc-view-display
                       (current-buffer)))))

(declare-function clear-image-cache "image.c" (&optional filter))

(defun doc-view-document->bitmap (pdf png pages)
  "Convert a document file to bitmap images asynchronously.
Start by converting PAGES, and then the rest."
  (if (null pages)
      (doc-view-pdf/ps->png pdf png)
    ;; We could render several `pages' with a single process if they're
    ;; (almost) consecutive, but since in 99% of the cases, there'll be only
    ;; a single page anyway, and of the remaining 1%, few cases will have
    ;; consecutive pages, it's not worth the trouble.
    (let ((rest (cdr pages)))
      (funcall doc-view-single-page-converter-function
	       pdf (format png (car pages)) (car pages)
               (lambda ()
                 (if rest
                     (doc-view-document->bitmap pdf png rest)
                   ;; Yippie, the important pages are done, update the display.
                   (clear-image-cache)
                   ;; For the windows that have a message (like "Welcome to
                   ;; DocView") display property, clearing the image cache is
                   ;; not sufficient.
                   (dolist (win (get-buffer-window-list (current-buffer) nil 'visible))
                     (with-selected-window win
	               (when (stringp (overlay-get (doc-view-current-overlay) 'display))
		         (doc-view-goto-page (doc-view-current-page)))))
                   ;; Convert the rest of the pages.
                   (doc-view-pdf/ps->png pdf png)))))))

(defun doc-view-pdf->txt (pdf txt callback)
  "Convert PDF to TXT asynchronously and call CALLBACK when finished."
  (or (executable-find doc-view-pdftotext-program)
      (error "You need the `pdftotext' program to convert a PDF to text"))
  (doc-view-start-process "pdf->txt" doc-view-pdftotext-program
                          (append doc-view-pdftotext-program-args
                                  (list pdf txt))
                          callback))

(defun doc-view-current-cache-doc-pdf ()
  "Return the name of the doc.pdf in the current cache dir.
This file exists only if the current document isn't a PDF or PS file already."
  (expand-file-name "doc.pdf" (doc-view--current-cache-dir)))

(defun doc-view-doc->txt (txt callback)
  "Convert the current document to text and call CALLBACK when done."
  (make-directory (doc-view--current-cache-dir) t)
  (pcase doc-view-doc-type
    ('pdf
     ;; Doc is a PDF, so convert it to TXT
     (doc-view-pdf->txt doc-view--buffer-file-name txt callback))
    ('ps
     ;; Doc is a PS, so convert it to PDF (which will be converted to
     ;; TXT thereafter).
     (let ((pdf (doc-view-current-cache-doc-pdf)))
       (doc-view-ps->pdf doc-view--buffer-file-name pdf
                         (lambda () (doc-view-pdf->txt pdf txt callback)))))
    ('dvi
     ;; Doc is a DVI.  This means that a doc.pdf already exists in its
     ;; cache subdirectory.
     (doc-view-pdf->txt (doc-view-current-cache-doc-pdf) txt callback))
    ('odf
     ;; Doc is some ODF (or MS Office) doc.  This means that a doc.pdf
     ;; already exists in its cache subdirectory.
     (doc-view-pdf->txt (doc-view-current-cache-doc-pdf) txt callback))
    (_ (error "DocView doesn't know what to do"))))

(defun doc-view-ps->pdf (ps pdf callback)
  "Convert PS to PDF asynchronously and call CALLBACK when finished."
  (or (executable-find doc-view-ps2pdf-program)
      (error "You need the `ps2pdf' program to convert PS to PDF"))
  (doc-view-start-process "ps->pdf" doc-view-ps2pdf-program
                          (list
                           ;; Avoid security problems when rendering files from
                           ;; untrusted sources.
                           "-dSAFER"
                           ;; in-file and out-file
                           ps pdf)
                          callback))

(defun doc-view-active-pages ()
  (let ((pages ()))
    (dolist (win (get-buffer-window-list (current-buffer) nil 'visible))
      (let ((page (image-mode-window-get 'page win)))
        (unless (memq page pages) (push page pages))))
    pages))

(defun doc-view-convert-current-doc ()
  "Convert `doc-view--buffer-file-name' to a set of png files, one file per page.
Those files are saved in the directory given by the function
`doc-view--current-cache-dir'."
  ;; Let stale files still display while we recompute the new ones, so only
  ;; flush the cache when the conversion is over.  One of the reasons why it
  ;; is important to keep displaying the stale page is so that revert-buffer
  ;; preserves the horizontal/vertical scroll settings (which are otherwise
  ;; reset during the redisplay).
  (setq doc-view--pending-cache-flush t)
  (let ((png-file (expand-file-name
                   (format doc-view--image-file-pattern "%d")
                   (doc-view--current-cache-dir))))
    (make-directory (doc-view--current-cache-dir) t)
    (pcase doc-view-doc-type
      ('dvi
       ;; DVI files have to be converted to PDF before Ghostscript can process
       ;; it.
       (let ((pdf (doc-view-current-cache-doc-pdf)))
         (doc-view-dvi->pdf doc-view--buffer-file-name pdf
                            (lambda () (doc-view-pdf/ps->png pdf png-file)))))
      ('odf
       ;; ODF files have to be converted to PDF before Ghostscript can
       ;; process it.
       (let ((pdf (doc-view-current-cache-doc-pdf))
             (opdf (expand-file-name
                    (concat (file-name-base doc-view--buffer-file-name)
                            ".pdf")
                    doc-view--current-cache-dir))
             (png-file png-file))
	 ;; The unoconv tool only supports an output directory, but no
	 ;; file name.  It's named like the input file with the
	 ;; extension replaced by pdf.
         (funcall doc-view-odf->pdf-converter-function doc-view--buffer-file-name
                  (lambda ()
		    ;; Rename to doc.pdf
		    (rename-file opdf pdf)
		    (doc-view-pdf/ps->png pdf png-file)))))
      ;; The doc-view-mode-p check ensures that epub, cbz, fb2 and
      ;; (o)xps are handled with mutool
      ((or 'pdf 'djvu 'epub 'cbz 'fb2 'xps 'oxps)
       (let ((pages (doc-view-active-pages)))
         ;; Convert doc to bitmap images starting with the active pages.
         (doc-view-document->bitmap doc-view--buffer-file-name png-file pages)))
      (_
       ;; Convert to PNG images.
       (doc-view-pdf/ps->png doc-view--buffer-file-name png-file)))))

;;;; Slicing

(declare-function image-size "image.c" (spec &optional pixels frame))

(defun doc-view-set-slice (x y width height)
  "Set the slice of the images that should be displayed.
You can use this function to tell doc-view not to display the
margins of the document.  It prompts for the top-left corner (X
and Y) of the slice to display and its WIDTH and HEIGHT.

See `doc-view-set-slice-using-mouse' and
`doc-view-set-slice-from-bounding-box' for more convenient ways
to do that.  To reset the slice use `doc-view-reset-slice'."
  (interactive
   (let* ((size (image-size (doc-view-current-image) t))
	  (a (read-number (format "Top-left X (0..%d): " (car size))))
	  (b (read-number (format "Top-left Y (0..%d): " (cdr size))))
	  (c (read-number (format "Width (0..%d): " (- (car size) a))))
	  (d (read-number (format "Height (0..%d): " (- (cdr size) b)))))
     (list a b c d)))
  (setf (doc-view-current-slice) (list x y width height))
  ;; Redisplay
  (doc-view-goto-page (doc-view-current-page)))

(defvar touch-screen-simple-mouse-conversion) ; Defined in touch-screen.el.

(defun doc-view-set-slice-using-mouse ()
  "Set the slice of the images that should be displayed.
You set the slice by pressing mouse-1 at its top-left corner and
dragging it to its bottom-right corner.  See also
`doc-view-set-slice' and `doc-view-reset-slice'."
  (interactive)
  (let ((touch-screen-simple-mouse-conversion t)
        x y w h done)
    (while (not done)
      (let ((e (read-key
		(concat "Press mouse-1 at the top-left corner and "
			"drag it to the bottom-right corner!"))))
	(when (eq (car e) 'drag-mouse-1)
	  (setq x (car (posn-object-x-y (event-start e))))
	  (setq y (cdr (posn-object-x-y (event-start e))))
	  (setq w (- (car (posn-object-x-y (event-end e))) x))
	  (setq h (- (cdr (posn-object-x-y (event-end e))) y))
	  (setq done t))))
    (doc-view-set-slice x y w h)))

(defun doc-view-get-bounding-box ()
  "Get the BoundingBox information of the current page."
  (let ((page (doc-view-current-page))
	(doc (let ((cache-doc (doc-view-current-cache-doc-pdf)))
	       (if (file-exists-p cache-doc)
		   cache-doc
		 doc-view--buffer-file-name))))
    (with-temp-buffer
      (when (eq 0 (ignore-errors
		    (process-file doc-view-ghostscript-program nil t
				  nil "-dSAFER" "-dBATCH" "-dNOPAUSE" "-q"
				  "-sDEVICE=bbox"
				  (format "-dFirstPage=%s" page)
				  (format "-dLastPage=%s" page)
				  doc)))
	(goto-char (point-min))
	(save-match-data
	  (when (re-search-forward
		 (concat "%%BoundingBox: "
			 "\\([[:digit:]]+\\) \\([[:digit:]]+\\) "
			 "\\([[:digit:]]+\\) \\([[:digit:]]+\\)")
                 nil t)
	    (mapcar #'string-to-number
		    (list (match-string 1)
			  (match-string 2)
			  (match-string 3)
			  (match-string 4)))))))))

(defvar doc-view-paper-sizes
  '((a4 595 842)
    (a4-landscape 842 595)
    (letter 612 792)
    (letter-landscape 792 612)
    (legal 612 1008)
    (legal-landscape 1008 612)
    (a3 842 1191)
    (a3-landscape 1191 842)
    (tabloid 792 1224)
    (ledger 1224 792))
  "An alist from paper size names to dimensions.")

(defun doc-view-guess-paper-size (iw ih)
  "Guess the paper size according to the aspect ratio."
  (cl-labels ((div (x y)
		(round (/ (* 100.0 x) y))))
    (let ((ar (div iw ih))
	  (al (mapcar (lambda (l)
			(list (div (nth 1 l) (nth 2 l)) (car l)))
		      doc-view-paper-sizes)))
      (cadr (assoc ar al)))))

(defun doc-view-scale-bounding-box (ps iw ih bb)
  (list (/ (* (nth 0 bb) iw) (nth 1 (assoc ps doc-view-paper-sizes)))
	(/ (* (nth 1 bb) ih) (nth 2 (assoc ps doc-view-paper-sizes)))
	(/ (* (nth 2 bb) iw) (nth 1 (assoc ps doc-view-paper-sizes)))
	(/ (* (nth 3 bb) ih) (nth 2 (assoc ps doc-view-paper-sizes)))))

(defun doc-view-set-slice-from-bounding-box (&optional force-paper-size)
  "Set the slice from the document's BoundingBox information.
The result is that the margins are almost completely cropped,
much more accurate than could be done manually using
`doc-view-set-slice-using-mouse'."
  (interactive "P")
  (let ((bb (doc-view-get-bounding-box)))
    (if (not bb)
	(message "BoundingBox couldn't be determined")
      (let* ((is (image-size (doc-view-current-image) t))
	     (iw (car is))
	     (ih (cdr is))
	     (ps (or (and (null force-paper-size)
                          (doc-view-guess-paper-size iw ih))
		     (intern (completing-read "Paper size: "
                                              doc-view-paper-sizes
					      nil t))))
	     (bb (doc-view-scale-bounding-box ps iw ih bb))
	     (x1 (nth 0 bb))
	     (y1 (nth 1 bb))
	     (x2 (nth 2 bb))
	     (y2 (nth 3 bb)))
	;; We keep a 2 pixel margin.
	(doc-view-set-slice (- x1 2) (- ih y2 2)
			    (+ (- x2 x1) 4) (+ (- y2 y1) 4))))))

(defun doc-view-reset-slice ()
  "Reset the current slice.
After calling this function whole pages will be visible again."
  (interactive)
  (setf (doc-view-current-slice) nil)
  ;; Redisplay
  (doc-view-goto-page (doc-view-current-page)))

;;;; Display

(defun doc-view-insert-image (file &rest args)
  "Insert the given png FILE.
ARGS is a list of image descriptors."
  (when doc-view--pending-cache-flush
    (clear-image-cache)
    (setq doc-view--pending-cache-flush nil))
  (let ((ol (doc-view-current-overlay)))
    ;; Only insert the image if the buffer is visible.
    (when (window-live-p (overlay-get ol 'window))
      (let* ((image (if (and file (file-readable-p file))
			(if (not doc-view-scale-internally)
			    (apply #'create-image file doc-view--image-type nil args)
			  (unless (member :width args)
			    (setq args `(,@args :width ,doc-view-image-width)))
                          (unless (member :transform-smoothing args)
                            (setq args `(,@args :transform-smoothing t)))
                          (when (eq doc-view--image-type 'svg)
                            (setq args `(,@args :background ,(face-background 'doc-view-svg-face)
                                                :foreground ,(face-foreground 'doc-view-svg-face)
                                                :css "svg{fill:currentcolor;}")))
			  (apply #'create-image file doc-view--image-type nil args))))
	     (slice (doc-view-current-slice))
	     (img-width (and image (car (image-size image))))
	     (displayed-img-width (if (and image slice)
				      (* (/ (float (nth 2 slice))
					    (car (image-size image 'pixels)))
					 img-width)
				    img-width))
	     (window-width (window-width)))
	(setf (doc-view-current-image) image)
	(move-overlay ol (point-min) (point-max))
	;; In case the window is wider than the image, center the image
	;; horizontally.
	(overlay-put ol 'before-string
		     (when (and image (> window-width displayed-img-width))
		       (propertize " " 'display
				   `(space :align-to (+ center (-0.5 . ,displayed-img-width))))))
	(overlay-put ol 'display
		     (cond
		      (image
		       (if slice
			   (list (cons 'slice slice) image)
			 image))
		      ;; We're trying to display a page that doesn't exist.
		      (doc-view--current-converter-processes
		       ;; Maybe the page doesn't exist *yet*.
		       "Cannot display this page (yet)!")
		      (t
		       ;; Typically happens if the conversion process somehow
		       ;; failed.  Better not signal an error here because it
		       ;; could prevent a subsequent reconversion from fixing
		       ;; the problem.
		       (concat "Cannot display this page!\n"
			       "Maybe because of a conversion failure!"))))
	(let ((win (overlay-get ol 'window)))
	  (if (stringp (overlay-get ol 'display))
	      (progn            ;Make sure the text is not scrolled out of view.
		(set-window-hscroll win 0)
		(set-window-vscroll win 0))
	    (let ((hscroll (image-mode-window-get 'hscroll win))
		  (vscroll (image-mode-window-get 'vscroll win)))
	      ;; Reset scroll settings, in case they were changed.
	      (if hscroll (set-window-hscroll win hscroll))
	      (if vscroll (set-window-vscroll win vscroll t)))))))))

(defun doc-view-sort (a b)
  "Return non-nil if A should be sorted before B.
Predicate for sorting `doc-view--current-files'."
  (or (< (length a) (length b))
      (and (= (length a) (length b))
           (string< a b))))

(defun doc-view-display (buffer &optional force)
  "Start viewing the document in BUFFER.
If FORCE is non-nil, start viewing even if the document does not
have the page we want to view."
  (with-current-buffer buffer
    (let ((prev-pages doc-view--current-files))
      (setq doc-view--current-files
            (sort (directory-files (doc-view--current-cache-dir) t
                                   (format doc-view--image-file-pattern
                                           "[0-9]+")
                                   t)
                  'doc-view-sort))
      (unless (eq (length prev-pages) (length doc-view--current-files))
	(force-mode-line-update))
      (dolist (win (or (get-buffer-window-list buffer nil t)
		       (list t)))
	(let* ((page (doc-view-current-page win))
	       (pagefile (expand-file-name
                          (format doc-view--image-file-pattern page)
                          (doc-view--current-cache-dir))))
	  (when (or force
		    (and (not (member pagefile prev-pages))
			 (member pagefile doc-view--current-files)))
	    (if (windowp win)
		(with-selected-window win
		  (cl-assert (eq (current-buffer) buffer) t)
		  (doc-view-goto-page page))
	      (doc-view-goto-page page))))))))

(defun doc-view-buffer-message ()
  ;; Only show this message initially, not when refreshing the buffer (in which
  ;; case it's better to keep displaying the "stale" page while computing
  ;; the fresh new ones).
  (unless (overlay-get (doc-view-current-overlay) 'display)
    (overlay-put (doc-view-current-overlay) 'display
                 (concat (propertize "Welcome to DocView!" 'face 'bold)
                         "\n"
                         (substitute-command-keys "
If you see this buffer it means that the document you want to view is being
converted to PNG and the conversion of the first page hasn't finished yet or
`doc-view-conversion-refresh-interval' is set to nil.

For now these keys are useful:
\\<doc-view-mode-map>
\\[quit-window] : Bury this buffer.  Conversion will go on in background.
\\[image-kill-buffer] : Kill the conversion process and this buffer.
\\[doc-view-kill-proc] : Kill the conversion process.\n")))))

(declare-function tooltip-show "tooltip" (text &optional use-echo-area
                                               text-face default-face))

(defun doc-view-show-tooltip ()
  (interactive)
  (tooltip-show (doc-view-current-info)))

;; We define an own major mode for DocView's text display so that we
;; can easily distinguish when we want to toggle back because
;; text-mode is a likely candidate for a default major-mode
;; (bug#34451).
(define-derived-mode doc-view--text-view-mode text-mode "DV/Text"
  "View mode used in DocView's text buffers."
  (view-mode))

(defun doc-view-open-text ()
  "Display the current doc's contents as text."
  (interactive)
  (if doc-view--current-converter-processes
      (message "DocView: please wait till conversion finished.")
    (let ((txt (expand-file-name "doc.txt" (doc-view--current-cache-dir)))
          (page (or (doc-view-current-page) 1)))
      (if (file-readable-p txt)
          (let ((dv-bfn doc-view--buffer-file-name)
                (dv-text-buffer-name (format "%s/text" (buffer-name))))
            ;; Prepare the text buffer
            (with-current-buffer (get-buffer-create dv-text-buffer-name)
              (let ((inhibit-read-only t)
                    (buffer-undo-list t))
                (erase-buffer)
                (set-buffer-multibyte t)
                (insert-file-contents txt)
                (doc-view--text-view-mode)
                (setq-local doc-view--buffer-file-name dv-bfn)
                ;; Pages are separated by form feed characters.
                (setq-local page-delimiter "")
                (set-buffer-modified-p nil)
                (doc-view-minor-mode)
                (goto-char (point-min))
                ;; Put point at the start of the page the user was
                ;; reading.  Pages are separated by Control-L characters.
                (re-search-forward page-delimiter nil t (1- page))))
            (switch-to-buffer (get-buffer dv-text-buffer-name)))
        (doc-view-doc->txt txt 'doc-view-open-text)))))

;;;;; Toggle between editing and viewing

(defvar-local doc-view-saved-settings nil
  "Doc-view settings saved while in some other mode.")
(put 'doc-view-saved-settings 'permanent-local t)

(defun doc-view-toggle-display ()
  "Toggle between editing a document as text or viewing it."
  (interactive)
  (cond
   ((eq major-mode 'doc-view-mode)
    ;; Switch to editing mode
    (doc-view-kill-proc)
    (setq buffer-read-only nil)
    ;; Switch to the previously used major mode or fall back to
    ;; normal mode.
    (doc-view-fallback-mode)
    (doc-view-minor-mode 1))
   ((eq major-mode 'doc-view--text-view-mode)
    ;; We're currently viewing the document's text contents, switch to
    ;; the buffer visiting the real document and kill myself.
    (let ((dv-buffer (find-buffer-visiting doc-view--buffer-file-name)))
      (kill-buffer)
      (switch-to-buffer dv-buffer)))
   (t
    ;; Switch to doc-view-mode
    (when (and (buffer-modified-p)
	       (y-or-n-p "The buffer has been modified.  Save the changes? "))
      (save-buffer))
    (doc-view-mode))))

;;;; Searching


(defun doc-view-search-internal (regexp file)
  "Return a list of FILE's pages that contain text matching REGEXP.
The value is an alist of the form (PAGE CONTEXTS) where PAGE is
the pagenumber and CONTEXTS are all lines of text containing a match."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((page 1)
	  (lastpage 1)
	  matches)
      (while (re-search-forward (concat "\\(?:\\([]\\)\\|\\("
					regexp "\\)\\)") nil t)
	(when (match-string 1) (setq page (1+ page)))
	(when (match-string 2)
	  (if (/= page lastpage)
	      (push (cons page
			  (list (buffer-substring
				 (line-beginning-position)
				 (line-end-position))))
		    matches)
	    (setq matches (cons
			   (append
			    (or
			     ;; This page already is a match.
			     (car matches)
			     ;; This is the first match on page.
			     (list page))
			    (list (buffer-substring
				   (line-beginning-position)
				   (line-end-position))))
			   (cdr matches))))
	  (setq lastpage page)))
      (nreverse matches))))

(defun doc-view-search-no-of-matches (list)
  "Extract the number of matches from the search result LIST."
  (let ((no 0))
    (dolist (p list)
      (setq no (+ no (1- (length p)))))
    no))

(defun doc-view-search-backward (new-query)
  "Call `doc-view-search' for backward search.
If prefix NEW-QUERY is given, ask for a new regexp."
  (interactive "P")
  (doc-view-search new-query t))

(defun doc-view-search (new-query &optional backward)
  "Jump to the next match or initiate a new search if NEW-QUERY is given.
If the current document hasn't been transformed to plain text
till now do that first.
If BACKWARD is non-nil, jump to the previous match."
  (interactive "P")
  (if (and (not new-query)
	   doc-view--current-search-matches)
      (if backward
	  (doc-view-search-previous-match 1)
	(doc-view-search-next-match 1))
    ;; New search, so forget the old results.
    (setq doc-view--current-search-matches nil)
    (let ((txt (expand-file-name "doc.txt"
				 (doc-view--current-cache-dir))))
      (if (file-readable-p txt)
	  (progn
	    (setq doc-view--current-search-matches
		  (doc-view-search-internal
		   (read-from-minibuffer "Regexp: ")
		   txt))
	    (message "DocView: search yielded %d matches."
		     (doc-view-search-no-of-matches
		      doc-view--current-search-matches)))
	;; We must convert to TXT first!
	(if doc-view--current-converter-processes
	    (message "DocView: please wait till conversion finished.")
	  (doc-view-doc->txt txt (lambda () (doc-view-search nil))))))
    ;; Update the tool bar items.
    (force-mode-line-update)))

(defun doc-view-new-search ()
  "Initiate a new search query.
Prompt for a string, then search for its appearances within
the document text."
  (interactive)
  (doc-view-search t nil))

(defun doc-view-search-next-match (arg)
  "Go to the ARGth next matching page."
  (interactive "p")
  (let* ((next-pages (cl-remove-if
		      (lambda (i) (<= (car i) (doc-view-current-page)))
		      doc-view--current-search-matches))
	 (page (car (nth (1- arg) next-pages))))
    (if page
	(doc-view-goto-page page)
      (when (and
	     doc-view--current-search-matches
	     (y-or-n-p "No more matches after current page.  Wrap to first match? "))
	(doc-view-goto-page (caar doc-view--current-search-matches))))))

(defun doc-view-search-previous-match (arg)
  "Go to the ARGth previous matching page."
  (interactive "p")
  (let* ((prev-pages (cl-remove-if
		      (lambda (i) (>= (car i) (doc-view-current-page)))
		      doc-view--current-search-matches))
	 (page (car (nth (1- arg) (nreverse prev-pages)))))
    (if page
	(doc-view-goto-page page)
      (when (and
	     doc-view--current-search-matches
	     (y-or-n-p "No more matches before current page.  Wrap to last match? "))
	(doc-view-goto-page (caar (last doc-view--current-search-matches)))))))

;;;; Imenu support
(defvar-local doc-view--outline nil
  "Cached document outline, so that it is only computed once per document.
It can be the symbol `unavailable' to indicate that outline is
unavailable for the document.")

(defvar doc-view--mutool-pdf-outline-script
  "var document = new Document.openDocument(\"%s\", \"application/pdf\");
var outline = document.loadOutline();
if(!outline) quit();
function pp(outl, level){print(\"((level . \" + level + \")\");\
print(\"(title . \" + repr(outl.title) + \")\");\
print(\"(page . \" + (document.resolveLink(outl.uri)+1) + \"))\");\
if(outl.down){for(var i=0; i<outl.down.length; i++){pp(outl.down[i], level+1);}}};
function run(){print(\"BEGIN(\");\
for(var i=0; i<outline.length; i++){pp(outline[i], 1);}print(\")\");};
run()"
  "JS script to extract the PDF's outline using mutool.
The script has to be minified to pass it to the REPL.  The \"BEGIN\"
marker is here to skip past the prompt characters.")

(defun doc-view--pdf-outline (&optional file-name)
  "Return a list describing the outline of FILE-NAME.
Return a list describing the current file if FILE-NAME is nil.

Each element in the returned list contains information about a section's
title, nesting level and page number.  The list is flat: its tree
structure is extracted by `doc-view--imenu-subtree'."
  (let ((fn (or file-name (buffer-file-name))))
    (when fn
      (with-temp-buffer
        (let ((proc (make-process
                     :name "doc-view-pdf-outline"
                     :command (list "mutool" "run")
                     :buffer (current-buffer))))
          (process-send-string proc (format doc-view--mutool-pdf-outline-script
                                            (expand-file-name fn)))
          ;; Need to send this twice for some reason...
          (process-send-eof)
          (process-send-eof)
          (while (accept-process-output proc))
          (unless (eq (process-status proc) 'exit)
            (setq doc-view--outline 'unavailable)
            (imenu-unavailable-error "Unable to create imenu index using `mutool'"))
          (goto-char (point-min))
          (when (search-forward "BEGIN" nil t)
            (condition-case nil
                (read (current-buffer))
              (end-of-file nil))))))))

(defun doc-view--djvu-outline (&optional file-name)
  "Return a list describing the outline of FILE-NAME.
If FILE-NAME is nil or omitted, it defaults to the current buffer's file
name.

For the format, see `doc-view--pdf-outline'."
  (unless file-name (setq file-name (buffer-file-name)))
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8))
      ;; Pass "-u" to make `djvused' emit UTF-8 encoded text to avoid
      ;; unescaping octal escapes for non-ASCII text.
      (call-process doc-view-djvused-program nil (current-buffer) nil
                    "-u" "-e" "print-outline" file-name)
      (goto-char (point-min))
      (when (eobp)
        (setq doc-view--outline 'unavailable)
        (imenu-unavailable-error "Unable to create imenu index using `djvused'"))
      (nreverse (doc-view--parse-djvu-outline (read (current-buffer)))))))

(defun doc-view--parse-djvu-outline (bookmark &optional level)
  "Return a list describing the djvu outline from BOOKMARK.
Optional argument LEVEL is the current heading level, which defaults to 1."
  (unless level (setq level 1))
  (let ((res))
    (unless (eq (car bookmark) 'bookmarks)
      (user-error "Unknown outline type: %S" (car bookmark)))
    (pcase-dolist (`(,title ,page . ,rest) (cdr bookmark))
      (push `((level . ,level)
              (title . ,title)
              (page . ,(string-to-number (string-remove-prefix "#" page))))
            res)
      (when (and rest (listp (car rest)))
        (setq res (append
                   (doc-view--parse-djvu-outline
                    (cons 'bookmarks rest)
                    (+ level 1))
                   res))))
    res))

(defun doc-view--imenu-subtree (outline act)
  "Construct a tree of imenu items for the given outline list and action.

This auxliary function constructs recursively all the items for
the first node in the outline and all its siblings at the same
level.  Returns that imenu alist together with any other pending outline
entries at an upper level."
  (let ((level (alist-get 'level (car outline)))
        (nested (not doc-view-imenu-flatten))
        (index nil))
    (while (and (car outline)
                (or (not nested)
                    (<= level (alist-get 'level (car outline)))))
      (let-alist (car outline)
        (let ((title (format-spec doc-view-imenu-title-format
                                  `((?t . ,.title) (?p . ,.page)))))
          (if (and nested (> .level level))
              (let ((sub (doc-view--imenu-subtree outline act))
                    (fst (car index)))
                (setq index (cdr index))
                (push (cons (car fst) (cons fst (car sub))) index)
                (setq outline (cdr sub)))
            (push `(,title 0 ,act ,.page) index)
            (setq outline (cdr outline))))))
    (cons (nreverse index) outline)))

(defun doc-view-imenu-index (&optional file-name goto-page-fn)
  "Create an imenu index using \"mutool\" to extract its outline.

For extensibility, callers can specify a FILE-NAME to indicate
the buffer other than the current buffer, and a jumping function
GOTO-PAGE-FN other than `doc-view-goto-page'."
  (unless doc-view--outline
    (setq doc-view--outline (doc-view--outline file-name)))
  (unless (eq doc-view--outline 'unavailable)
    (let* ((goto (or goto-page-fn #'doc-view-goto-page))
           (act (lambda (_name _pos page) (funcall goto page)))
           (outline doc-view--outline))
      (car (doc-view--imenu-subtree outline act)))))

(defun doc-view--outline (&optional file-name)
  "Return the outline for the file FILE-NAME.
If FILE-NAME is nil, use the current file instead."
  (unless file-name (setq file-name (buffer-file-name)))
  (let ((outline
         (pcase doc-view-doc-type
           ('djvu
            (when doc-view-djvused-program
              (doc-view--djvu-outline file-name)))
           ('odf
            (doc-view--pdf-outline (doc-view-current-cache-doc-pdf)))
           (_
            (doc-view--pdf-outline file-name)))))
    (when outline (imenu-add-to-menubar "Outline"))
    ;; When the outline could not be made due to unavailability of the
    ;; required program, or its absence from the document, return
    ;; 'unavailable'.
    (or outline 'unavailable)))

(defun doc-view-imenu-setup ()
  "Set up local state in the current buffer for imenu, if needed."
  (setq-local imenu-create-index-function #'doc-view-imenu-index
              imenu-submenus-on-top nil
              imenu-sort-function nil))

;;;; User interface commands and the mode

(put 'doc-view-mode 'mode-class 'special)

(defun doc-view-already-converted-p ()
  "Return non-nil if the current doc was already converted."
  (and (file-exists-p (doc-view--current-cache-dir))
       ;; Check that the resolution info is there, otherwise it means
       ;; the conversion is incomplete.
       (file-readable-p (expand-file-name "resolution.el"
                                          (doc-view--current-cache-dir)))
       (> (length (directory-files
                   (doc-view--current-cache-dir)
                   nil (format doc-view--image-file-pattern "[0-9]+")))
          0)))

(defun doc-view-initiate-display ()
  ;; Switch to image display if possible.
  (if (doc-view-mode-p doc-view-doc-type)
      ;; Inhibit the echo area display of the "Type C-c C-c..." message
      ;; if the doc-view buffer is not shown in the selected window
      ;; which can happen due to auto-reverting the buffer (bug#79145).
      (let ((inhibit-message (not (eq (current-buffer)
                                      (window-buffer)))))
	(doc-view-buffer-message)
	(setf (doc-view-current-page) (or (doc-view-current-page) 1))
	(if (doc-view-already-converted-p)
	    (progn
	      (message "DocView: using cached files!")
	      ;; Load the saved resolution.
	      (let* ((res-file
                      (expand-file-name "resolution.el"
                                        (doc-view--current-cache-dir)))
                     (res
                      (with-temp-buffer
                        (when (file-readable-p res-file)
                          (insert-file-contents res-file)
                          (read (current-buffer))))))
                (when (numberp res)
		  (setq-local doc-view-resolution res)))
	      (doc-view-display (current-buffer) 'force))
	  (doc-view-convert-current-doc))
	(message
	 "%s"
	 (substitute-command-keys
	  (concat "Type \\[doc-view-toggle-display] to toggle between "
		  "editing or viewing the document."))))
    (if (image-type-available-p 'png)
        (message "Conversion utility \"%s\" not available for %s"
                 doc-view-ghostscript-program
	         (file-name-extension doc-view--buffer-file-name))
      (message "PNG support not available; can't view document"))
    (if (and (executable-find doc-view-pdftotext-program)
	     (y-or-n-p
	      "Unable to render file.  View extracted text instead? "))
	(doc-view-open-text)
      (doc-view-toggle-display))))

(defvar bookmark-make-record-function)

(defun doc-view-clone-buffer-hook ()
  ;; FIXME: There are several potential problems linked with reconversion
  ;; and auto-revert when we have indirect buffers because they share their
  ;; /tmp cache directory.  This sharing is good (you'd rather not reconvert
  ;; for each clone), but that means that clones need to collaborate a bit.
  ;; I guess it mostly means: detect when a reconversion process is already
  ;; running, and run the sentinel in all clones.
  ;;
  ;; Maybe the clones should really have a separate /tmp directory
  ;; so they could have a different resolution and you could use clones
  ;; for zooming.
  (remove-overlays (point-min) (point-max) 'doc-view t)
  (if (consp image-mode-winprops-alist) (setq image-mode-winprops-alist nil)))

(defun doc-view-set-doc-type ()
  "Figure out the current document type (`doc-view-doc-type')."
  (let ((name-types
         (cdr (assoc-string
               (file-name-extension
                (or buffer-file-name (buffer-name (current-buffer))))
               '(
                 ;; DVI
                 ("dvi" dvi)
                 ;; PDF
                 ("pdf" pdf) ("epdf" pdf)
                 ;; EPUB
                 ("epub" epub)
                 ;; PostScript
                 ("ps" ps) ("eps" ps)
                 ;; DjVu
                 ("djvu" djvu)
                 ;; OpenDocument formats.
                 ("odt" odf) ("ods" odf) ("odp" odf) ("odg" odf)
                 ("odc" odf) ("odi" odf) ("odm" odf) ("ott" odf)
                 ("ots" odf) ("otp" odf) ("otg" odf)
                 ;; Microsoft Office formats (also handled by the odf
                 ;; conversion chain).
                 ("doc" odf) ("docx" odf) ("xls" odf) ("xlsx" odf)
                 ("ppt" odf) ("pps" odf) ("pptx" odf) ("rtf" odf)
                 ;; CBZ
                 ("cbz" cbz)
                 ;; FB2
                 ("fb2" fb2)
                 ;; (Open)XPS
                 ("xps" xps) ("oxps" oxps))
               t)))
	(content-types
	 (save-excursion
	   (goto-char (point-min))
	   (cond
	    ((looking-at "%!") '(ps))
	    ((looking-at "%PDF") '(pdf))
	    ((looking-at "\367\002") '(dvi))
	    ((looking-at "AT&TFORM") '(djvu))
            ;; The following pattern actually is for recognizing
            ;; zip-archives, so that this same association is used for
            ;; cbz files. This is fine, as cbz files should be handled
            ;; like epub anyway.
            ((looking-at "PK") '(epub odf cbz))))))
    (setq-local
     doc-view-doc-type
     (car (or (nreverse (seq-intersection name-types content-types #'eq))
              (when (and name-types content-types)
                (error "Conflicting types: name says %s but content says %s"
                       name-types content-types))
              name-types content-types
              (error "Cannot determine the document type"))))))

(defun doc-view-set-up-single-converter ()
  "Find the right single-page converter for the current document type."
  (pcase-let ((`(,conv-function ,type ,extension)
               (pcase doc-view-doc-type
                 ('djvu (list #'doc-view-djvu->tiff-converter-ddjvu 'tiff "tif"))
                 ((or 'ps 'postscript 'eps)
                  (list #'doc-view-ps->png-converter-ghostscript 'png "png"))
                 (_ (if (and (eq doc-view-pdf->png-converter-function
                                 #'doc-view-pdf->png-converter-mupdf)
                             doc-view-mupdf-use-svg)
                        (list doc-view-pdf->png-converter-function 'svg "svg")
                      (list doc-view-pdf->png-converter-function 'png "png"))))))
    (setq-local doc-view-single-page-converter-function conv-function)
    (setq-local doc-view--image-type type)
    (setq-local doc-view--image-file-pattern (concat "page-%s." extension))))

;; desktop.el integration

(defun doc-view-desktop-save-buffer (_desktop-dirname)
  ;; FIXME: This is wrong, since this info is per-window but we only do it once
  ;; here for the buffer.  IOW it should be saved via something like
  ;; `window-persistent-parameters'.
  `((page . ,(doc-view-current-page))
    (slice . ,(doc-view-current-slice))))

(declare-function desktop-restore-file-buffer "desktop"
                  (buffer-filename buffer-name buffer-misc))

(defun doc-view-restore-desktop-buffer (file name misc)
  (let ((page  (cdr (assq 'page misc)))
	(slice (cdr (assq 'slice misc))))
    (desktop-restore-file-buffer file name misc)
    ;; FIXME: We need to run this code after displaying the buffer.
    (with-selected-window (or (get-buffer-window (current-buffer) 0)
			      (selected-window))
      ;; FIXME: This should be done for all windows restored that show
      ;; this buffer.  Basically, the page/slice should be saved as
      ;; window-parameters in the window-state(s) and then restoring this
      ;; window-state should call us back (to interpret/use those parameters).
      (doc-view-goto-page page)
      (when slice (apply #'doc-view-set-slice slice))
      (current-buffer))))

(add-to-list 'desktop-buffer-mode-handlers
	     '(doc-view-mode . doc-view-restore-desktop-buffer))

;;;###autoload
(defun doc-view-mode ()
  "Major mode in DocView buffers.

DocView Mode is an Emacs document viewer.  It displays PDF, PS
and DVI files (as PNG or SVG images) in Emacs buffers.

You can use \\<doc-view-mode-map>\\[doc-view-toggle-display] to
toggle between displaying the document or editing it as text.
\\{doc-view-mode-map}"
  (interactive)

  (if (= (point-min) (point-max))
      ;; The doc is empty or doesn't exist at all, so fallback to
      ;; another mode.  We used to also check file-exists-p, but this
      ;; returns nil for tar members.
      (doc-view-fallback-mode)

    (major-mode-suspend)

    (dolist (var doc-view-saved-settings)
      (set (make-local-variable (car var)) (cdr var)))

    ;; Figure out the document type.
    (unless doc-view-doc-type
      (doc-view-set-doc-type))
    (doc-view-set-up-single-converter)
    (unless (memq doc-view-doc-type '(ps))
      (setq-local require-final-newline nil))

    ;; These modes will just display "1", so they're not very useful
    ;; in this mode.
    (setq-local global-linum-mode nil
                display-line-numbers-mode nil)

    (doc-view-make-safe-dir doc-view-cache-directory)
    ;; Handle compressed files, remote files, files inside archives
    (setq-local doc-view--buffer-file-name
		(convert-standard-filename
                 (cond
                  (jka-compr-really-do-compress
                   ;; FIXME: there's a risk of name conflicts here.
                   (expand-file-name
                    (file-name-nondirectory
                     (file-name-sans-extension buffer-file-name))
                    doc-view-cache-directory))
                  ;; Is the file readable by local processes?
                  ;; We used to use `file-remote-p' but it's unclear what it's
                  ;; supposed to return nil for things like local files accessed
                  ;; via `su' or via file://...
                  ((let ((file-name-handler-alist nil))
                     (or (not (and buffer-file-name
                                   (file-readable-p buffer-file-name)))
                         ;; If the system is Android and the file name
                         ;; begins with /content or /assets, it's not
                         ;; readable by local processes.
                         (and (eq system-type 'android)
                              (string-match-p "/\\(content\\|assets\\)[/$]"
                                              (expand-file-name
                                               buffer-file-name)))))
                   ;; FIXME: there's a risk of name conflicts here.
                   (expand-file-name
                    (if buffer-file-name
			(file-name-nondirectory buffer-file-name)
                      (buffer-name))
                    doc-view-cache-directory))
                  (t buffer-file-name))))
    (when (not (string= doc-view--buffer-file-name buffer-file-name))
      (write-region nil nil doc-view--buffer-file-name))

    (add-function :around (local 'revert-buffer-function) #'doc-view--revert-buffer)

    (add-hook 'change-major-mode-hook
	      (lambda ()
		(doc-view-kill-proc)
		(remove-overlays (point-min) (point-max) 'doc-view t))
	      nil t)
    (add-hook 'clone-indirect-buffer-hook #'doc-view-clone-buffer-hook nil t)
    (add-hook 'kill-buffer-hook #'doc-view-kill-proc nil t)
    (setq-local desktop-save-buffer #'doc-view-desktop-save-buffer)

    (remove-overlays (point-min) (point-max) 'doc-view t) ;Just in case.
    ;; Keep track of display info ([vh]scroll, page number, overlay,
    ;; ...)  for each window in which this document is shown.
    (add-hook 'image-mode-new-window-functions
	      #'doc-view-new-window-function nil t)
    (image-mode-setup-winprops)

    (setq-local mode-line-position
                '(" P" (:eval (number-to-string (doc-view-current-page)))
                  "/" (:eval (number-to-string (doc-view-last-page-number)))))
    ;; Don't scroll unless the user specifically asked for it.
    (setq-local auto-hscroll-mode nil)
    (if (boundp 'mwheel-scroll-up-function) ; not --without-x build
        (setq-local mwheel-scroll-up-function
                    #'doc-view-scroll-up-or-next-page))
    (if (boundp 'mwheel-scroll-down-function)
        (setq-local mwheel-scroll-down-function
                    #'doc-view-scroll-down-or-previous-page))
    (setq-local cursor-type nil)
    (use-local-map doc-view-mode-map)
    (add-hook 'after-revert-hook #'doc-view-reconvert-doc nil t)
    (setq-local bookmark-make-record-function
                #'doc-view-bookmark-make-record)
    (setq mode-name "DocView"
	  buffer-read-only t
	  major-mode 'doc-view-mode)
    (condition-case imenu-error
        (doc-view-imenu-setup)
      (imenu-unavailable (message "imenu support unavailable: %s"
                                  (cadr imenu-error))))
    (doc-view-initiate-display)
    ;; Replace the tool bar map with `doc-view-tool-bar-map'.
    (setq-local tool-bar-map doc-view-tool-bar-map)
    ;; Switch off view-mode explicitly, because doc-view-mode is the
    ;; canonical view mode for PDF/PS/DVI files.  This could be
    ;; switched on automatically depending on the value of
    ;; `view-read-only'.
    (setq-local view-read-only nil)
    (run-mode-hooks 'doc-view-mode-hook)))

(defun doc-view-fallback-mode ()
  "Fallback to the previous or next best major mode."
  (let ((vars (if (derived-mode-p 'doc-view-mode)
                  (mapcar (lambda (var) (cons var (symbol-value var)))
                          '(doc-view-resolution
                            image-mode-winprops-alist)))))
    (remove-overlays (point-min) (point-max) 'doc-view t)
    (major-mode-restore '(doc-view-mode-maybe doc-view-mode))
    (when vars
      (setq-local doc-view-saved-settings vars))))

;;;###autoload
(defun doc-view-mode-maybe ()
  "Switch to `doc-view-mode' if possible.
If the required external tools are not available, then fallback
to the next best mode."
  (condition-case nil
      (doc-view-set-doc-type)
    (error (doc-view-fallback-mode)))
  (if (doc-view-mode-p doc-view-doc-type)
      (doc-view-mode)
    (doc-view-fallback-mode)))

;;;###autoload
(define-minor-mode doc-view-minor-mode
  "Toggle displaying buffer via Doc View (Doc View minor mode).

See the command `doc-view-mode' for more information on this mode."
  :lighter " DocView"
  (when doc-view-minor-mode
    (add-hook 'change-major-mode-hook
              (lambda ()
                (doc-view-minor-mode -1))
              nil t)
    ;; OpenDocuments are archive files, so their editing mode is
    ;; archive-mode.  When editing and saving a file in that archive,
    ;; it'll automatically revert the archive buffer.  Take care to
    ;; re-enable `doc-view-minor-mode' in that case.
    (add-hook 'revert-buffer-restore-functions
              (lambda ()
                (lambda ()
                  (unless (derived-mode-p 'doc-view-mode)
                    (doc-view-minor-mode 1))))
              nil t)
    (message
     "%s"
     (substitute-command-keys
      "Type \\[doc-view-toggle-display] to toggle between editing or viewing the document."))))

(defun doc-view-clear-cache ()
  "Delete the whole cache (`doc-view-cache-directory')."
  (interactive)
  (dired-delete-file doc-view-cache-directory 'always))

(defun doc-view-dired-cache ()
  "Open `dired' in `doc-view-cache-directory'."
  (interactive)
  (dired doc-view-cache-directory))

;;;; Presentation mode

(defvar-keymap doc-view-presentation-mode-map
  "ESC"  #'doc-view-presentation-exit
  "q"    #'doc-view-presentation-exit
  ;; "C" #'doc-view-convert-all-pages
  )

(defvar-local doc-view-presentation--src-data nil)

(defun doc-view-presentation-exit ()
  "Leave Doc-View's presentation mode."
  (interactive)
  (doc-view-presentation-mode -1))

(define-minor-mode doc-view-presentation-mode
  "Minor mode used while in presentation mode."
  :init-value nil :keymap doc-view-presentation-mode-map
  (if doc-view-presentation-mode
      (progn
        (setq-local mode-line-format nil)
        (doc-view-fit-page-to-window)
        ;; (doc-view-convert-all-pages)
        )
    (kill-local-variable 'mode-line-format)
    (let ((pn (doc-view-current-page))
          (win (selected-window)))
      (doc-view-presentation--propagate-pn doc-view-presentation--src-data pn)
      (setq doc-view-presentation--src-data nil)
      (with-selected-window win
        (if (and (one-window-p) (window-dedicated-p))
            (delete-frame))))))

(defun doc-view-presentation--propagate-pn (src-data pn)
  (when src-data
    (let ((win (car src-data)))
      (when (and (window-live-p win)
                 (eq (current-buffer) (window-buffer win)))
        (select-window win))
      (when (eq (doc-view-current-page) (cdr src-data))
        (doc-view-goto-page pn)))))

(defun doc-view-presentation ()
  "Put Doc-View in presentation mode."
  (interactive)
  (let* ((src-data (cons (selected-window) (doc-view-current-page)))
         (mal (display-monitor-attributes-list))
         (monitor-top 0)
         (monitor-left 0)
         (monitor-height (display-pixel-height))
         (monitor-width (display-pixel-width)))
    (dolist (attrs mal)
      (when (memq (selected-frame) (alist-get 'frames attrs))
        (let ((geom (alist-get 'geometry attrs)))
          (when geom
            (setq monitor-left (nth 0 geom))
            (setq monitor-top (nth 1 geom))
            (setq monitor-width (nth 2 geom))
            (setq monitor-height (nth 3 geom))))))
    (let ((frame (make-frame
                  `((minibuffer . nil)
                    (fullscreen . fullboth)
                    (height . ,(ceiling monitor-height (frame-char-height)))
                    ;; Don't use `ceiling' here since doc-view will center the
                    ;; image instead.
                    (width . ,(ceiling monitor-width (frame-char-width)))
                    (name . "Doc-View-Presentation")
                    (top . ,monitor-top) (left . ,monitor-left) (user-position . t)
                    (vertical-scroll-bars . nil)
                    (left-fringe . 0) (right-fringe . 0)
                    (menu-bar-lines . 0)
                    (tool-bar-lines . 0)))))
      (select-window (frame-root-window frame))
      (setq doc-view-presentation--src-data src-data)
      (set-window-dedicated-p (selected-window) t)
      (doc-view-presentation-mode 1))))


;;;; Bookmark integration

(declare-function bookmark-make-record-default
                  "bookmark" (&optional no-file no-context posn))
(declare-function bookmark-prop-get "bookmark" (bookmark prop))
(declare-function bookmark-default-handler "bookmark" (bmk))

(defun doc-view-bookmark-make-record ()
  (nconc (bookmark-make-record-default)
         `((page     . ,(doc-view-current-page))
           (handler  . doc-view-bookmark-jump))))

;;;###autoload
(defun doc-view-bookmark-jump (bmk)
  ;; This implements the `handler' function interface for record type
  ;; returned by `doc-view-bookmark-make-record', which see.
  (let ((page (bookmark-prop-get bmk 'page))
	(show-fn-sym (make-symbol "doc-view-bookmark-after-jump-hook")))
    (fset show-fn-sym
	  (lambda ()
	    (remove-hook 'bookmark-after-jump-hook show-fn-sym)
	    (when (not (eq major-mode 'doc-view-mode))
	      (doc-view-toggle-display))
	    (with-selected-window
		(or (get-buffer-window (current-buffer) 0)
		    (selected-window))
	      (doc-view-goto-page page))))
    (add-hook 'bookmark-after-jump-hook show-fn-sym)
    (bookmark-default-handler bmk)))

(put 'doc-view-bookmark-jump 'bookmark-handler-type "DocView")

;;; Register integration

(defvar-local doc-view-register-alist nil
  "Register alist containing only doc-view registers for current buffer.
Each doc-view register entry is of the form (doc-view . ALIST) where
ALIST has the keys `buffer', `file', and `page'.  The value of `buffer'
is the buffer which visits the file specified by the value of `file'.
The value of `page' is the page stored in the register.")

(defun doc-view-page-to-register (register)
  "Store the current page to the specified REGISTER."
  (interactive
   (let ((register-alist doc-view-register-alist))
     (list (register-read-with-preview "Page to register: "))))
  (let ((register-alist doc-view-register-alist))
    (set-register register
                  `(doc-view
                    (buffer . ,(current-buffer))
                    (file . ,(buffer-file-name))
                    (page . ,(doc-view-current-page))))
    (setq doc-view-register-alist register-alist)))

(defun doc-view-jump-to-register (register)
  "Jump to the specified REGISTER."
  (interactive
   (let ((register-alist doc-view-register-alist))
     (list (register-read-with-preview "Jump to register: "))))
  (let ((register-alist doc-view-register-alist))
    (jump-to-register register)))

(cl-defmethod register-val-insert ((val (head doc-view)))
  (prin1 val))

(cl-defmethod register-val-describe ((val (head doc-view)) _verbose)
  (let* ((alist (cdr val))
         (name (or (file-name-nondirectory (alist-get 'file alist))
                   (buffer-name (alist-get 'buffer alist)))))
    (princ name)
    (princ " p. ")
    (princ (alist-get 'page alist))))

(cl-defmethod register-val-jump-to ((val (head doc-view)) _arg)
  (let* ((alist (cdr val))
         (buffer (or (alist-get 'buffer alist)
                     (find-buffer-visiting (alist-get 'file alist)))))
    (unless buffer
      (user-error "Cannot find the doc-view buffer to jump to"))
    (switch-to-buffer buffer)
    (doc-view-goto-page (alist-get 'page alist))))

;; Obsolete.

(defun doc-view-intersection (l1 l2)
  (declare (obsolete seq-intersection "28.1"))
  (nreverse (seq-intersection l1 l2 #'eq)))

(provide 'doc-view)

;; Local Variables:
;; eval: (outline-minor-mode 1)
;; End:

;;; doc-view.el ends here
