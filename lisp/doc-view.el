;;; doc-view.el --- View PDF/PostScript/DVI files in Emacs

;; Copyright (C) 2007, 2008 Free Software Foundation, Inc.
;;
;; Author: Tassilo Horn <tassilo@member.fsf.org>
;; Maintainer: Tassilo Horn <tassilo@member.fsf.org>
;; Keywords: files, pdf, ps, dvi

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Requirements:

;; doc-view.el requires GNU Emacs 22.1 or newer.  You also need Ghostscript,
;; `dvipdfm' which comes with teTeX and `pdftotext', which comes with xpdf
;; (http://www.foolabs.com/xpdf/) or poppler (http://poppler.freedesktop.org/).

;;; Commentary:

;; DocView is a document viewer for Emacs.  It converts PDF, PS and DVI files
;; to a set of PNG files, one PNG for each page, and displays the PNG images
;; inside an Emacs buffer.  This buffer uses `doc-view-mode' which provides
;; convenient key bindings for browsing the document.
;;
;; To use it simply open a document file with
;;
;;     C-x C-f ~/path/to/document RET
;;
;; and the document will be converted and displayed, if your emacs supports png
;; images.  With `C-c C-c' you can toggle between the rendered images
;; representation and the source text representation of the document.
;;
;; Since conversion may take some time all the PNG images are cached in a
;; subdirectory of `doc-view-cache-directory' and reused when you want to view
;; that file again.  To reconvert a document hit `g' (`doc-view-reconvert-doc')
;; when displaying the document.  To delete all cached files use
;; `doc-view-clear-cache'.  To open the cache with dired, so that you can tidy
;; it out use `doc-view-dired-cache'.
;;
;; When conversion in underway the first page will be displayed as soon as it
;; is available and the available pages are refreshed every
;; `doc-view-conversion-refresh-interval' seconds.  If that variable is nil the
;; pages won't be displayed before conversion of the document finished
;; completely.
;;
;; DocView lets you select a slice of the displayed pages.  This slice will be
;; remembered and applied to all pages of the current document.  This enables
;; you to cut away the margins of a document to save some space.  To select a
;; slice you can use `doc-view-set-slice' (bound to `s s') which will query you
;; for the coordinates of the slice's top-left corner and its width and height.
;; A much more convenient way to do the same is offered by the command
;; `doc-view-set-slice-using-mouse' (bound to `s m').  After invokation you
;; only have to press mouse-1 at the top-left corner and drag it to the
;; bottom-right corner of the desired slice.  To reset the slice use
;; `doc-view-reset-slice' (bound to `s r').
;;
;; You can also search within the document.  The command `doc-view-search'
;; (bound to `C-s') queries for a search regexp and initializes a list of all
;; matching pages and messages how many match-pages were found.  After that you
;; can jump to the next page containing a match with an additional `C-s'.  With
;; `C-r' you can do the same, but backwards.  To search for a new regexp give a
;; prefix arg to one of the search functions, e.g. by typing `C-u C-s'.  The
;; searching works by using a plain text representation of the document.  If
;; that doesn't already exist the first invokation of `doc-view-search' (or
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

;; - better menu.
;; - Bind slicing to a drag event.
;; - doc-view-fit-doc-to-window and doc-view-fit-window-to-doc.
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
;; isearch really searches for each step forward or backward wheras
;; the current approach searches once and then it knows to which
;; pages to jump.

;; Anyway, if someone with better isearch knowledge wants to give it a try,
;; feel free to do it.  --Tassilo

;;; Code:

(require 'dired)
(require 'image-mode)
(require 'jka-compr)

;;;; Customization Options

(defgroup doc-view nil
  "In-buffer viewer for PDF, PostScript and DVI files."
  :link '(function-link doc-view)
  :version "22.2"
  :group 'applications
  :group 'multimedia
  :prefix "doc-view-")

(defcustom doc-view-ghostscript-program (executable-find "gs")
  "Program to convert PS and PDF files to PNG."
  :type 'file
  :group 'doc-view)

(defcustom doc-view-ghostscript-options
  '("-dSAFER" ;; Avoid security problems when rendering files from untrusted
	      ;; sources.
    "-dNOPAUSE" "-sDEVICE=png16m" "-dTextAlphaBits=4"
    "-dBATCH" "-dGraphicsAlphaBits=4" "-dQUIET")
  "A list of options to give to ghostscript."
  :type '(repeat string)
  :group 'doc-view)

(defcustom doc-view-resolution 100
  "Dots per inch resolution used to render the documents.
Higher values result in larger images."
  :type 'number
  :group 'doc-view)

(defcustom doc-view-dvipdfm-program (executable-find "dvipdfm")
  "Program to convert DVI files to PDF.

DVI file will be converted to PDF before the resulting PDF is
converted to PNG."
  :type 'file
  :group 'doc-view)

(defcustom doc-view-ps2pdf-program (executable-find "ps2pdf")
  "Program to convert PS files to PDF.

PS files will be converted to PDF before searching is possible."
  :type 'file
  :group 'doc-view)

(defcustom doc-view-pdftotext-program (executable-find "pdftotext")
  "Program to convert PDF files to plain text.

Needed for searching."
  :type 'file
  :group 'doc-view)

(defcustom doc-view-cache-directory
  (expand-file-name (format "docview%d" (user-uid))
		    temporary-file-directory)
  "The base directory, where the PNG images will be saved."
  :type 'directory
  :group 'doc-view)

(defcustom doc-view-conversion-buffer "*doc-view conversion output*"
  "The buffer where messages from the converter programs go to."
  :type 'string
  :group 'doc-view)

(defcustom doc-view-conversion-refresh-interval 3
  "Interval in seconds between refreshes of the DocView buffer while converting.
After such a refresh newly converted pages will be available for
viewing.  If set to nil there won't be any refreshes and the
pages won't be displayed before conversion of the whole document
has finished."
  :type 'integer
  :group 'doc-view)

;;;; Internal Variables

(defvar doc-view-current-files nil
  "Only used internally.")

(defvar doc-view-current-page nil
  "Only used internally.")

(defvar doc-view-current-converter-process nil
  "Only used internally.")

(defvar doc-view-current-timer nil
  "Only used internally.")

(defvar doc-view-current-slice nil
  "Only used internally.")

(defvar doc-view-current-cache-dir nil
  "Only used internally.")

(defvar doc-view-current-search-matches nil
  "Only used internally.")

(defvar doc-view-current-image nil
  "Only used internally.")

(defvar doc-view-current-overlay nil
  "Only used internally.")

(defvar doc-view-pending-cache-flush nil
  "Only used internally.")

(defvar doc-view-current-info nil
  "Only used internally.")

(defvar doc-view-previous-major-mode nil
  "Only used internally.")

(defvar doc-view-buffer-file-name nil
  "Only used internally.
The file name used for conversion.  Normally it's the same as
`buffer-file-name', but for remote files, compressed files and
files inside an archive it is a temporary copy of
the (uncompressed, extracted) file residing in
`doc-view-cache-directory'.")

;;;; DocView Keymaps

(defvar doc-view-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    ;; Navigation in the document
    (define-key map (kbd "n")         'doc-view-next-page)
    (define-key map (kbd "p")         'doc-view-previous-page)
    (define-key map (kbd "<next>")    'forward-page)
    (define-key map (kbd "<prior>")   'backward-page)
    (define-key map [remap forward-page]  'doc-view-next-page)
    (define-key map [remap backward-page] 'doc-view-previous-page)
    (define-key map (kbd "SPC")       'doc-view-scroll-up-or-next-page)
    (define-key map (kbd "DEL")       'doc-view-scroll-down-or-previous-page)
    (define-key map (kbd "M-<")       'doc-view-first-page)
    (define-key map (kbd "M->")       'doc-view-last-page)
    (define-key map [remap goto-line] 'doc-view-goto-page)
    (define-key map [remap scroll-up] 'image-scroll-up)
    (define-key map [remap scroll-down] 'image-scroll-down)
    ;; Zoom in/out.
    (define-key map "+"               'doc-view-enlarge)
    (define-key map "-"               'doc-view-shrink)
    ;; Killing/burying the buffer (and the process)
    (define-key map (kbd "q")         'bury-buffer)
    (define-key map (kbd "k")         'doc-view-kill-proc-and-buffer)
    (define-key map (kbd "K")         'doc-view-kill-proc)
    ;; Slicing the image
    (define-key map (kbd "s s")       'doc-view-set-slice)
    (define-key map (kbd "s m")       'doc-view-set-slice-using-mouse)
    (define-key map (kbd "s r")       'doc-view-reset-slice)
    ;; Searching
    (define-key map (kbd "C-s")       'doc-view-search)
    (define-key map (kbd "<find>")    'doc-view-search)
    (define-key map (kbd "C-r")       'doc-view-search-backward)
    ;; Scrolling
    (define-key map [remap forward-char]  'image-forward-hscroll)
    (define-key map [remap backward-char] 'image-backward-hscroll)
    (define-key map [remap next-line]     'image-next-line)
    (define-key map [remap previous-line] 'image-previous-line)
    ;; Show the tooltip
    (define-key map (kbd "C-t")       'doc-view-show-tooltip)
    ;; Toggle between text and image display or editing
    (define-key map (kbd "C-c C-c")   'doc-view-toggle-display)
    ;; Reconvert the current document
    (define-key map (kbd "g")         'revert-buffer)
    (define-key map (kbd "r")         'revert-buffer)
    map)
  "Keymap used by `doc-view-mode' when displaying a doc as a set of images.")

(easy-menu-define doc-view-menu doc-view-mode-map
  "Menu for Doc View mode."
  '("DocView"
    ["Set Slice"		doc-view-set-slice-using-mouse]
    ["Set Slice (manual)"	doc-view-set-slice]
    ["Reset Slice"		doc-view-reset-slice]
    "---"
    ["Search"			doc-view-search]
    ["Search Backwards"         doc-view-search-backward]
    ["Toggle display"		doc-view-toggle-display]
    ))

(defvar doc-view-minor-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Toggle between text and image display or editing
    (define-key map (kbd "C-c C-c") 'doc-view-toggle-display)
    map)
  "Keymap used by `doc-minor-view-mode'.")

;;;; Navigation Commands

(defun doc-view-goto-page (page)
  "View the page given by PAGE."
  (interactive "nPage: ")
  (let ((len (length doc-view-current-files)))
    (if (< page 1)
	(setq page 1)
      (when (> page len)
	(setq page len)))
    (setq doc-view-current-page page
	  doc-view-current-info
	  (concat
	   (propertize
	    (format "Page %d of %d."
		    doc-view-current-page
		    len) 'face 'bold)
	   ;; Tell user if converting isn't finished yet
	   (if doc-view-current-converter-process
	       " (still converting...)\n"
	     "\n")
	   ;; Display context infos if this page matches the last search
	   (when (and doc-view-current-search-matches
		      (assq doc-view-current-page
			    doc-view-current-search-matches))
	     (concat (propertize "Search matches:\n" 'face 'bold)
		     (let ((contexts ""))
		       (dolist (m (cdr (assq doc-view-current-page
					     doc-view-current-search-matches)))
			 (setq contexts (concat contexts "  - \"" m "\"\n")))
		       contexts)))))
    ;; Update the buffer
    (doc-view-insert-image (nth (1- page) doc-view-current-files)
                           :pointer 'arrow)
    (overlay-put doc-view-current-overlay 'help-echo doc-view-current-info)
    (goto-char (point-min))
    ;; This seems to be needed for set-window-hscroll (in
    ;; image-forward-hscroll) to do something useful, I don't have time to
    ;; debug this now.  :-(  --Stef
    (forward-char)))

(defun doc-view-next-page (&optional arg)
  "Browse ARG pages forward."
  (interactive "p")
  (doc-view-goto-page (+ doc-view-current-page (or arg 1))))

(defun doc-view-previous-page (&optional arg)
  "Browse ARG pages backward."
  (interactive "p")
  (doc-view-goto-page (- doc-view-current-page (or arg 1))))

(defun doc-view-first-page ()
  "View the first page."
  (interactive)
  (doc-view-goto-page 1))

(defun doc-view-last-page ()
  "View the last page."
  (interactive)
  (doc-view-goto-page (length doc-view-current-files)))

(defun doc-view-scroll-up-or-next-page ()
  "Scroll page up if possible, else goto next page."
  (interactive)
  (when (= (window-vscroll) (image-scroll-up nil))
    (let ((cur-page doc-view-current-page))
      (doc-view-next-page)
      (when (/= cur-page doc-view-current-page)
	(set-window-vscroll nil 0)))))

(defun doc-view-scroll-down-or-previous-page ()
  "Scroll page down if possible, else goto previous page."
  (interactive)
  (when (= (window-vscroll) (image-scroll-down nil))
    (let ((cur-page doc-view-current-page))
      (doc-view-previous-page)
      (when (/= cur-page doc-view-current-page)
	(image-scroll-up nil)))))

;;;; Utility Functions

(defun doc-view-kill-proc ()
  "Kill the current converter process."
  (interactive)
  (when doc-view-current-converter-process
    (kill-process doc-view-current-converter-process)
    (setq doc-view-current-converter-process nil))
  (when doc-view-current-timer
    (cancel-timer doc-view-current-timer)
    (setq doc-view-current-timer nil))
  (setq mode-line-process nil))

(defun doc-view-kill-proc-and-buffer ()
  "Kill the current converter process and buffer."
  (interactive)
  (doc-view-kill-proc)
  (when (eq major-mode 'doc-view-mode)
    (kill-buffer (current-buffer))))

(defun doc-view-make-safe-dir (dir)
  (condition-case nil
      (let ((umask (default-file-modes)))
        (unwind-protect
            (progn
              ;; Create temp files with strict access rights.  It's easy to
              ;; loosen them later, whereas it's impossible to close the
              ;; time-window of loose permissions otherwise.
              (set-default-file-modes #o0700)
              (make-directory dir))
          ;; Reset the umask.
          (set-default-file-modes umask)))
    (file-already-exists
     (if (file-symlink-p dir)
         (error "Danger: %s points to a symbolic link" dir))
     ;; In case it was created earlier with looser rights.
     ;; We could check the mode info returned by file-attributes, but it's
     ;; a pain to parse and it may not tell you what we want under
     ;; non-standard file-systems.  So let's just say what we want and let
     ;; the underlying C code and file-system figure it out.
     ;; This also ends up checking a bunch of useful conditions: it makes
     ;; sure we have write-access to the directory and that we own it, thus
     ;; closing a bunch of security holes.
     (set-file-modes dir #o0700))))

(defun doc-view-current-cache-dir ()
  "Return the directory where the png files of the current doc should be saved.
It's a subdirectory of `doc-view-cache-directory'."
  (if doc-view-current-cache-dir
      doc-view-current-cache-dir
    ;; Try and make sure doc-view-cache-directory exists and is safe.
    (doc-view-make-safe-dir doc-view-cache-directory)
    ;; Now compute the subdirectory to use.
    (setq doc-view-current-cache-dir
	  (file-name-as-directory
	   (expand-file-name
	    (concat (file-name-nondirectory buffer-file-name)
		    "-"
		    (let ((file doc-view-buffer-file-name))
		      (with-temp-buffer
			(insert-file-contents-literally file)
			(md5 (current-buffer)))))
            doc-view-cache-directory)))))

(defun doc-view-remove-if (predicate list)
  "Return LIST with all items removed that satisfy PREDICATE."
  (let (new-list)
    (dolist (item list (nreverse new-list))
      (when (not (funcall predicate item))
	(setq new-list (cons item new-list))))))

;;;###autoload
(defun doc-view-mode-p (type)
  "Return non-nil if image type TYPE is available for `doc-view'.
Image types are symbols like `dvi', `postscript' or `pdf'."
  (and (display-graphic-p)
       (image-type-available-p 'png)
       (cond
	((eq type 'dvi)
	 (and (doc-view-mode-p 'pdf)
	      doc-view-dvipdfm-program
	      (executable-find doc-view-dvipdfm-program)))
	((or (eq type 'postscript) (eq type 'ps) (eq type 'eps)
	     (eq type 'pdf))
	 (and doc-view-ghostscript-program
	      (executable-find doc-view-ghostscript-program)))
	(t ;; unknown image type
	 nil))))

;;;; Conversion Functions

(defvar doc-view-shrink-factor 1.125)

(defun doc-view-enlarge (factor)
  "Enlarge the document."
  (interactive (list doc-view-shrink-factor))
  (set (make-local-variable 'doc-view-resolution)
       (* factor doc-view-resolution))
  (doc-view-reconvert-doc))

(defun doc-view-shrink (factor)
  "Shrink the document."
  (interactive (list doc-view-shrink-factor))
  (doc-view-enlarge (/ 1.0 factor)))

(defun doc-view-reconvert-doc ()
  "Reconvert the current document.
Should be invoked when the cached images aren't up-to-date."
  (interactive)
  (doc-view-kill-proc)
  ;; Clear the old cached files
  (when (file-exists-p (doc-view-current-cache-dir))
    (dired-delete-file (doc-view-current-cache-dir) 'always))
  (doc-view-initiate-display))

(defun doc-view-dvi->pdf-sentinel (proc event)
  "If DVI->PDF conversion was successful, convert the PDF to PNG now."
  (if (not (string-match "finished" event))
      (message "DocView: dvi->pdf process changed status to %s." event)
    (with-current-buffer (process-get proc 'buffer)
      (setq doc-view-current-converter-process nil
            mode-line-process nil)
      ;; Now go on converting this PDF to a set of PNG files.
      (let* ((pdf (process-get proc 'pdf-file))
             (png (expand-file-name "page-%d.png"
                                    (doc-view-current-cache-dir))))
        (doc-view-pdf/ps->png pdf png)))))

(defun doc-view-dvi->pdf (dvi pdf)
  "Convert DVI to PDF asynchronously."
  (setq doc-view-current-converter-process
	(start-process "dvi->pdf" doc-view-conversion-buffer
		       doc-view-dvipdfm-program
		       "-o" pdf dvi)
	mode-line-process (list (format ":%s" doc-view-current-converter-process)))
  (set-process-sentinel doc-view-current-converter-process
			'doc-view-dvi->pdf-sentinel)
  (process-put doc-view-current-converter-process 'buffer   (current-buffer))
  (process-put doc-view-current-converter-process 'pdf-file pdf))

(defun doc-view-pdf/ps->png-sentinel (proc event)
  "If PDF/PS->PNG conversion was successful, update the display."
  (if (not (string-match "finished" event))
      (message "DocView: converter process changed status to %s." event)
    (with-current-buffer (process-get proc 'buffer)
      (setq doc-view-current-converter-process nil
            mode-line-process nil)
      (when doc-view-current-timer
        (cancel-timer doc-view-current-timer)
        (setq doc-view-current-timer nil))
      ;; Yippie, finished.  Update the display!
      (doc-view-display buffer-file-name 'force))))

(defun doc-view-pdf/ps->png (pdf-ps png)
  "Convert PDF-PS to PNG asynchronously."
  (setq doc-view-current-converter-process
        ;; Make sure the process is started in an existing directory,
        ;; (rather than some file-name-handler-managed dir, for example).
        (let ((default-directory (file-name-directory pdf-ps)))
          (apply 'start-process
                 (append (list "pdf/ps->png" doc-view-conversion-buffer
                               doc-view-ghostscript-program)
                         doc-view-ghostscript-options
                         (list (format "-r%d" (round doc-view-resolution)))
                         (list (concat "-sOutputFile=" png))
                         (list pdf-ps))))
	mode-line-process (list (format ":%s" doc-view-current-converter-process)))
  (process-put doc-view-current-converter-process
	       'buffer (current-buffer))
  (set-process-sentinel doc-view-current-converter-process
			'doc-view-pdf/ps->png-sentinel)
  (when doc-view-conversion-refresh-interval
    (setq doc-view-current-timer
	  (run-at-time "1 secs" doc-view-conversion-refresh-interval
		       'doc-view-display
		       buffer-file-name))))

(defun doc-view-pdf->txt-sentinel (proc event)
  (if (not (string-match "finished" event))
      (message "DocView: converter process changed status to %s." event)
    (let ((current-buffer (current-buffer))
	  (proc-buffer    (process-get proc 'buffer)))
      (with-current-buffer proc-buffer
        (setq doc-view-current-converter-process nil
              mode-line-process nil)
        ;; If the user looks at the DocView buffer where the conversion was
        ;; performed, search anew.  This time it will be queried for a regexp.
        (when (eq current-buffer proc-buffer)
          (doc-view-search nil))))))

(defun doc-view-pdf->txt (pdf txt)
  "Convert PDF to TXT asynchronously."
  (setq doc-view-current-converter-process
	(start-process "pdf->txt" doc-view-conversion-buffer
		       doc-view-pdftotext-program "-raw"
		       pdf txt)
	mode-line-process (list (format ":%s" doc-view-current-converter-process)))
  (set-process-sentinel doc-view-current-converter-process
			'doc-view-pdf->txt-sentinel)
  (process-put doc-view-current-converter-process 'buffer (current-buffer)))

(defun doc-view-ps->pdf-sentinel (proc event)
  (if (not (string-match "finished" event))
      (message "DocView: converter process changed status to %s." event)
    (with-current-buffer (process-get proc 'buffer)
      (setq doc-view-current-converter-process nil
            mode-line-process nil)
      ;; Now we can transform to plain text.
      (doc-view-pdf->txt (process-get proc 'pdf-file)
                         (expand-file-name "doc.txt"
                                           (doc-view-current-cache-dir))))))

(defun doc-view-ps->pdf (ps pdf)
  "Convert PS to PDF asynchronously."
  (setq doc-view-current-converter-process
	(start-process "ps->pdf" doc-view-conversion-buffer
		       doc-view-ps2pdf-program
		       ;; Avoid security problems when rendering files from
		       ;; untrusted sources.
		       "-dSAFER"
		       ;; in-file and out-file
		       ps pdf)
	mode-line-process (list (format ":%s" doc-view-current-converter-process)))
  (set-process-sentinel doc-view-current-converter-process
			'doc-view-ps->pdf-sentinel)
  (process-put doc-view-current-converter-process 'buffer   (current-buffer))
  (process-put doc-view-current-converter-process 'pdf-file pdf))

(defun doc-view-convert-current-doc ()
  "Convert `doc-view-buffer-file-name' to a set of png files, one file per page.
Those files are saved in the directory given by the function
`doc-view-current-cache-dir'."
  ;; Let stale files still display while we recompute the new ones, so only
  ;; flush the cache when the conversion is over.  One of the reasons why it
  ;; is important to keep displaying the stale page is so that revert-buffer
  ;; preserves the horizontal/vertical scroll settings (which are otherwise
  ;; resets during the redisplay).
  (setq doc-view-pending-cache-flush t)
  (let ((png-file (expand-file-name "page-%d.png"
                                    (doc-view-current-cache-dir))))
    (make-directory (doc-view-current-cache-dir))
    (if (not (string= (file-name-extension doc-view-buffer-file-name) "dvi"))
	;; Convert to PNG images.
	(doc-view-pdf/ps->png doc-view-buffer-file-name png-file)
      ;; DVI files have to be converted to PDF before Ghostscript can process
      ;; it.
      (doc-view-dvi->pdf doc-view-buffer-file-name
			 (expand-file-name "doc.pdf"
                                           doc-view-current-cache-dir)))))

;;;; Slicing

(defun doc-view-set-slice (x y width height)
  "Set the slice of the images that should be displayed.
You can use this function to tell doc-view not to display the
margins of the document.  It prompts for the top-left corner (X
and Y) of the slice to display and its WIDTH and HEIGHT.

See `doc-view-set-slice-using-mouse' for a more convenient way to
do that.  To reset the slice use `doc-view-reset-slice'."
  (interactive
   (let* ((size (image-size doc-view-current-image t))
	  (a (read-number (format "Top-left X (0..%d): " (car size))))
	  (b (read-number (format "Top-left Y (0..%d): " (cdr size))))
	  (c (read-number (format "Width (0..%d): " (- (car size) a))))
	  (d (read-number (format "Height (0..%d): " (- (cdr size) b)))))
     (list a b c d)))
  (setq doc-view-current-slice (list x y width height))
  ;; Redisplay
  (doc-view-goto-page doc-view-current-page))

(defun doc-view-set-slice-using-mouse ()
  "Set the slice of the images that should be displayed.
You set the slice by pressing mouse-1 at its top-left corner and
dragging it to its bottom-right corner.  See also
`doc-view-set-slice' and `doc-view-reset-slice'."
  (interactive)
  (let (x y w h done)
    (while (not done)
      (let ((e (read-event
		(concat "Press mouse-1 at the top-left corner and "
			"drag it to the bottom-right corner!"))))
	(when (eq (car e) 'drag-mouse-1)
	  (setq x (car (posn-object-x-y (event-start e))))
	  (setq y (cdr (posn-object-x-y (event-start e))))
	  (setq w (- (car (posn-object-x-y (event-end e))) x))
	  (setq h (- (cdr (posn-object-x-y (event-end e))) y))
	  (setq done t))))
    (doc-view-set-slice x y w h)))

(defun doc-view-reset-slice ()
  "Reset the current slice.
After calling this function whole pages will be visible again."
  (interactive)
  (setq doc-view-current-slice nil)
  ;; Redisplay
  (doc-view-goto-page doc-view-current-page))

;;;; Display

(defun doc-view-insert-image (file &rest args)
  "Insert the given png FILE.
ARGS is a list of image descriptors."
  (when doc-view-pending-cache-flush
    (clear-image-cache)
    (setq doc-view-pending-cache-flush nil))
  (if (null file)
      ;; We're trying to display a page that doesn't exist.  Typically happens
      ;; if the conversion process somehow failed.  Better not signal an
      ;; error here because it could prevent a subsequent reconversion from
      ;; fixing the problem.
      (progn
        (setq doc-view-current-image nil)
        (move-overlay doc-view-current-overlay (point-min) (point-max))
        (overlay-put doc-view-current-overlay 'display
                     "Cannot display this page!  Probably a conversion failure!"))
    (let ((image (apply 'create-image file 'png nil args)))
      (setq doc-view-current-image image)
      (move-overlay doc-view-current-overlay (point-min) (point-max))
      (overlay-put doc-view-current-overlay 'display
                   (if doc-view-current-slice
                       (list (cons 'slice doc-view-current-slice) image)
                     image)))))

(defun doc-view-sort (a b)
  "Return non-nil if A should be sorted before B.
Predicate for sorting `doc-view-current-files'."
  (or (< (length a) (length b))
      (and (= (length a) (length b))
           (string< a b))))

(defun doc-view-display (doc &optional force)
  "Start viewing the document DOC.
If FORCE is non-nil, start viewing even if the document does not
have the page we want to view."
  (with-current-buffer (get-file-buffer doc)
    (setq doc-view-current-files
          (sort (directory-files (doc-view-current-cache-dir) t
                                 "page-[0-9]+\\.png" t)
                'doc-view-sort))
    (when (or force
              (>= (length doc-view-current-files)
                  (or doc-view-current-page 1)))
      (doc-view-goto-page doc-view-current-page))))

(defun doc-view-buffer-message ()
  ;; Only show this message initially, not when refreshing the buffer (in which
  ;; case it's better to keep displaying the "stale" page while computing
  ;; the fresh new ones).
  (unless (overlay-get doc-view-current-overlay 'display)
    (overlay-put doc-view-current-overlay 'display
                 (concat (propertize "Welcome to DocView!" 'face 'bold)
                         "\n"
                         "
If you see this buffer it means that the document you want to view is being
converted to PNG and the conversion of the first page hasn't finished yet or
`doc-view-conversion-refresh-interval' is set to nil.

For now these keys are useful:

`q' : Bury this buffer.  Conversion will go on in background.
`k' : Kill the conversion process and this buffer.
`K' : Kill the conversion process.\n"))))

(defun doc-view-show-tooltip ()
  (interactive)
  (tooltip-show doc-view-current-info))

;;;;; Toggle between editing and viewing

(defun doc-view-toggle-display ()
  "Toggle between editing a document as text or viewing it."
  (interactive)
  (if (eq major-mode 'doc-view-mode)
      ;; Switch to editing mode
      (progn
	(doc-view-kill-proc)
	(setq buffer-read-only nil)
        (delete-overlay doc-view-current-overlay)
	;; Switch to the previously used major mode or fall back to fundamental
	;; mode.
	(if doc-view-previous-major-mode
	    (funcall doc-view-previous-major-mode)
	  (fundamental-mode))
	(doc-view-minor-mode 1))
    ;; Switch to doc-view-mode
    (when (and (buffer-modified-p)
	       (y-or-n-p "The buffer has been modified.  Save the changes? "))
      (save-buffer))
    (doc-view-mode)))

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
	   doc-view-current-search-matches)
      (if backward
	  (doc-view-search-previous-match 1)
	(doc-view-search-next-match 1))
    ;; New search, so forget the old results.
    (setq doc-view-current-search-matches nil)
    (let ((txt (expand-file-name "doc.txt"
				 (doc-view-current-cache-dir))))
      (if (file-readable-p txt)
	  (progn
	    (setq doc-view-current-search-matches
		  (doc-view-search-internal
		   (read-from-minibuffer "Regexp: ")
		   txt))
	    (message "DocView: search yielded %d matches."
		     (doc-view-search-no-of-matches
		      doc-view-current-search-matches)))
	;; We must convert to TXT first!
	(if doc-view-current-converter-process
	    (message "DocView: please wait till conversion finished.")
	  (let ((ext (file-name-extension doc-view-buffer-file-name)))
	    (cond
	     ((string= ext "pdf")
	      ;; Doc is a PDF, so convert it to TXT
	      (doc-view-pdf->txt doc-view-buffer-file-name txt))
	     ((string= ext "ps")
	      ;; Doc is a PS, so convert it to PDF (which will be converted to
	      ;; TXT thereafter).
	      (doc-view-ps->pdf doc-view-buffer-file-name
				(expand-file-name "doc.pdf"
						  (doc-view-current-cache-dir))))
	     ((string= ext "dvi")
	      ;; Doc is a DVI.  This means that a doc.pdf already exists in its
	      ;; cache subdirectory.
	      (doc-view-pdf->txt (expand-file-name "doc.pdf"
						   (doc-view-current-cache-dir))
				 txt))
	     (t (error "DocView doesn't know what to do")))))))))

(defun doc-view-search-next-match (arg)
  "Go to the ARGth next matching page."
  (interactive "p")
  (let* ((next-pages (doc-view-remove-if
		      (lambda (i) (<= (car i) doc-view-current-page))
		      doc-view-current-search-matches))
	 (page (car (nth (1- arg) next-pages))))
    (if page
	(doc-view-goto-page page)
      (when (and
	     doc-view-current-search-matches
	     (y-or-n-p "No more matches after current page.  Wrap to first match? "))
	(doc-view-goto-page (caar doc-view-current-search-matches))))))

(defun doc-view-search-previous-match (arg)
  "Go to the ARGth previous matching page."
  (interactive "p")
  (let* ((prev-pages (doc-view-remove-if
		      (lambda (i) (>= (car i) doc-view-current-page))
		      doc-view-current-search-matches))
	 (page (car (nth (1- arg) (nreverse prev-pages)))))
    (if page
	(doc-view-goto-page page)
      (when (and
	     doc-view-current-search-matches
	     (y-or-n-p "No more matches before current page.  Wrap to last match? "))
	(doc-view-goto-page (caar (last doc-view-current-search-matches)))))))

;;;; User interface commands and the mode

;; (put 'doc-view-mode 'mode-class 'special)

(defun doc-view-initiate-display ()
  ;; Switch to image display if possible
  (if (doc-view-mode-p (intern (file-name-extension doc-view-buffer-file-name)))
      (progn
	(doc-view-buffer-message)
	(setq doc-view-current-page (or doc-view-current-page 1))
	(if (file-exists-p (doc-view-current-cache-dir))
	    (progn
	      (message "DocView: using cached files!")
	      (doc-view-display buffer-file-name 'force))
	  (doc-view-convert-current-doc))
	(message
	 "%s"
	 (substitute-command-keys
	  (concat "Type \\[doc-view-toggle-display] to toggle between "
		  "editing or viewing the document."))))
    (message
     "%s"
     (substitute-command-keys
      (concat "No image (png) support available or some conversion utility for "
	      (file-name-extension doc-view-buffer-file-name)" files is missing.  "
	      "Type \\[doc-view-toggle-display] to switch to an editing mode.")))))

(defvar bookmark-make-cell-function)

;;;###autoload
(defun doc-view-mode ()
  "Major mode in DocView buffers.
You can use \\<doc-view-mode-map>\\[doc-view-toggle-display] to
toggle between displaying the document or editing it as text."
  (interactive)

  (let* ((prev-major-mode (if (eq major-mode 'doc-view-mode)
			      doc-view-previous-major-mode
			    major-mode)))
    (kill-all-local-variables)
    (set (make-local-variable 'doc-view-previous-major-mode) prev-major-mode))

  ;; Handle compressed files, remote files, files inside archives
  (set (make-local-variable 'doc-view-buffer-file-name)
       (cond
	(jka-compr-really-do-compress
	 (expand-file-name
	  (file-name-nondirectory
	   (file-name-sans-extension buffer-file-name))
	  doc-view-cache-directory))
        ;; Is the file readable by local processes?
        ;; We used to use `file-remote-p' but it's unclear what it's
        ;; supposed to return nil for things like local files accessed via
        ;; `su' or via file://...
	((let ((file-name-handler-alist nil))
           (not (file-readable-p buffer-file-name)))
	 (expand-file-name
	  (file-name-nondirectory buffer-file-name)
	  doc-view-cache-directory))
	(t buffer-file-name)))
  (when (not (string= doc-view-buffer-file-name buffer-file-name))
    (write-region nil nil doc-view-buffer-file-name))

  (make-local-variable 'doc-view-current-files)
  (make-local-variable 'doc-view-current-image)
  (make-local-variable 'doc-view-current-page)
  (make-local-variable 'doc-view-current-converter-process)
  (make-local-variable 'doc-view-current-timer)
  (make-local-variable 'doc-view-current-slice)
  (make-local-variable 'doc-view-current-cache-dir)
  (make-local-variable 'doc-view-current-info)
  (make-local-variable 'doc-view-current-search-matches)
  (set (make-local-variable 'doc-view-current-overlay)
       (make-overlay (point-min) (point-max) nil t))
  (add-hook 'change-major-mode-hook
	    (lambda () (delete-overlay doc-view-current-overlay))
	    nil t)
  (set (make-local-variable 'mode-line-position)
       '(" P" (:eval (number-to-string doc-view-current-page))
	 "/" (:eval (number-to-string (length doc-view-current-files)))))
  (set (make-local-variable 'cursor-type) nil)
  (use-local-map doc-view-mode-map)
  (set (make-local-variable 'after-revert-hook) 'doc-view-reconvert-doc)
  (set (make-local-variable 'bookmark-make-cell-function)
       'doc-view-bookmark-make-cell)
  (setq mode-name "DocView"
	buffer-read-only t
	major-mode 'doc-view-mode)
  (doc-view-initiate-display)
  (run-mode-hooks 'doc-view-mode-hook))

;;;###autoload
(define-minor-mode doc-view-minor-mode
  "Toggle Doc view minor mode.
With arg, turn Doc view minor mode on if arg is positive, off otherwise.
See the command `doc-view-mode' for more information on this mode."
  nil " DocView" doc-view-minor-mode-map
  :group 'doc-view
  (when doc-view-minor-mode
    (add-hook 'change-major-mode-hook (lambda () (doc-view-minor-mode -1)) nil t)
    (message
     "%s"
     (substitute-command-keys
      "Type \\[doc-view-toggle-display] to toggle between editing or viewing the document."))))

(defun doc-view-clear-cache ()
  "Delete the whole cache (`doc-view-cache-directory')."
  (interactive)
  (dired-delete-file doc-view-cache-directory 'always)
  (make-directory doc-view-cache-directory))

(defun doc-view-dired-cache ()
  "Open `dired' in `doc-view-cache-directory'."
  (interactive)
  (dired doc-view-cache-directory))


;;;; Bookmark integration

(defun doc-view-bookmark-make-cell (annotation &rest args)
  (let ((the-record
         `((filename . ,buffer-file-name)
           (page     . ,doc-view-current-page)
           (handler  . doc-view-bookmark-jump))))

    ;; Take no chances with text properties
    (set-text-properties 0 (length annotation) nil annotation)

    (when annotation
      (nconc the-record (list (cons 'annotation annotation))))

    ;; Finally, return the completed record.
    the-record))


(declare-function bookmark-get-filename        "bookmark" (bookmark))
(declare-function bookmark-get-bookmark-record "bookmark" (bookmark))

;;;###autoload
(defun doc-view-bookmark-jump (bmk)
  ;; This implements the `handler' function interface for record type
  ;; returned by `bookmark-make-cell-function', which see.
  (save-window-excursion
    (let ((filename (bookmark-get-filename bmk))
	  (page (cdr (assq 'page (bookmark-get-bookmark-record bmk)))))
      (find-file filename)
      (when (not (eq major-mode 'doc-view-mode))
	(doc-view-toggle-display))
      (doc-view-goto-page page)
      `((buffer ,(current-buffer)) (position ,1)))))


(provide 'doc-view)

;; Local Variables:
;; mode: outline-minor
;; End:

;; arch-tag: 5d6e5c5e-095f-489e-b4e4-1ca90a7d79be
;;; doc-view.el ends here
