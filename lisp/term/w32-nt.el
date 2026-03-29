;;; w32-nt.el --- MS-Windows native build specific definitions -*- lexical-binding: t -*-

;; Copyright (C) 1993-1994, 2001-2026 Free Software Foundation, Inc.

;; Author: FSF
;; Keywords: terminals

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

;; w32-nt.el: this file contains MS-Windows native build specific
;; definitions formerly included in w32-win.el.

;;; Code:


(eval-when-compile (require 'cl-lib))


(defvar dynamic-library-alist)
(defvar libpng-version)                 ; image.c #ifdef HAVE_NTGUI
(defvar libgif-version)
(defvar libjpeg-version)

(defvar libgnutls-version)              ; gnutls.c

(defvar tree-sitter--library-abi)       ; treesit.c

;;; Set default known names for external libraries
(setq dynamic-library-alist
      (list
       '(gdiplus "gdiplus.dll")
       '(shlwapi "shlwapi.dll")
       '(xpm "libxpm.dll" "xpm4.dll" "libXpm-nox4.dll")
       ;; Versions of libpng 1.4.x and later are incompatible with
       ;; earlier versions.  Set up the list of libraries according to
       ;; the version we were compiled against.  (If we were compiled
       ;; without PNG support, libpng-version's value is -1.)
       (if (>= libpng-version 10400)
	   (let ((major (/ libpng-version 10000))
		 (minor (mod (/ libpng-version 100) 10)))
	     (list 'png
		   ;; libpngXY.dll is the default name when building
		   ;; with CMake or from a lpngXYY tarball on w32,
		   ;; libpngXY-XY.dll is the DLL name when building
		   ;; with libtool / autotools
		   (format "libpng%d%d.dll" major minor)
		   (format "libpng%d%d-%d%d.dll" major minor major minor)))
	 '(png "libpng12d.dll" "libpng12.dll" "libpng3.dll" "libpng.dll"
	       ;; these are libpng 1.2.8 from GTK+
	       "libpng13d.dll" "libpng13.dll"))
       '(tiff "libtiff-5.dll" "libtiff3.dll" "libtiff.dll")
       (if (> libjpeg-version 62)
	   ;; Versions of libjpeg after 6b are incompatible with
	   ;; earlier versions, and each of versions 7, 8, and 9 is
	   ;; also incompatible with the preceding ones (the core data
	   ;; structures used for communications with the library
	   ;; gained additional members with each new version).  So we
	   ;; must use only the version of the library which Emacs was
	   ;; compiled against.
	   (list 'jpeg (format "libjpeg-%d.dll" (/ libjpeg-version 10)))
	 '(jpeg "jpeg62.dll" "libjpeg.dll" "jpeg-62.dll" "jpeg.dll"))
       ;; Versions of giflib 5.0.0 and later changed signatures of
       ;; several functions used by Emacs, which makes those versions
       ;; incompatible with previous ones.  We select the correct
       ;; libraries according to the version of giflib we were
       ;; compiled against.  (If we were compiled without GIF support,
       ;; libgif-version's value is -1.)
       (if (>= libgif-version 50100)
	   ;; Yes, giflib 5.0 uses 6 as the major version of the API,
	   ;; and giflib 5.1 uses 7, thus "libgif-7.dll" and
	   ;; "libgif-6.dll" below (giflib 4.x used 5 as the major API
	   ;; version).  giflib5.dll is from the lua-files project,
	   ;; and gif.dll is from luapower.
	   '(gif "libgif-7.dll")
	 (if (>= libgif-version 50000)
	     '(gif "libgif-6.dll" "giflib5.dll" "gif.dll")
	 '(gif "libgif-5.dll" "giflib4.dll" "libungif4.dll" "libungif.dll")))
       '(svg "librsvg-2-2.dll")
       '(webp "libwebp-7.dll" "libwebp.dll")
       '(webpdemux "libwebpdemux-2.dll" "libwebpdemux.dll")
       '(sqlite3 "libsqlite3-0.dll")
       '(gdk-pixbuf "libgdk_pixbuf-2.0-0.dll")
       '(glib "libglib-2.0-0.dll")
       '(gio "libgio-2.0-0.dll")
       '(gobject "libgobject-2.0-0.dll")
       (if (>= libgnutls-version 30400)
	   '(gnutls "libgnutls-30.dll")
	 '(gnutls "libgnutls-28.dll" "libgnutls-26.dll"))
       '(libxml2 "libxml2-16.dll" "libxml2-2.dll" "libxml2.dll")
       '(zlib "zlib1.dll" "libz-1.dll")
       '(lcms2 "liblcms2-2.dll")
       '(gccjit "libgccjit-0.dll")
       ;; MSYS2 distributes libtree-sitter.dll, without API version
       ;; number, upto and including version 0.24.3-2; later versions
       ;; come with libtree-sitter-major.minor.dll (as in
       ;; libtree-sitter-0.24.dll).  Sadly, the header files don't have
       ;; any symbols for library version, so we can only use the
       ;; library-language ABI version; according to
       ;; https://github.com/tree-sitter/tree-sitter/issues/3925, the
       ;; language ABI must change when the library's ABI is modified.
       (if (<= tree-sitter--library-abi 14)
           '(tree-sitter "libtree-sitter-0.24.dll"
                         "libtree-sitter.dll"
                         "libtree-sitter-0.dll")
         ;; Supported ABI 13..15
         '(tree-sitter "libtree-sitter-0.26.dll"
                       "libtree-sitter-0.25.dll"))))


;;;; Selections

(declare-function w32--set-selection "w32-win.el")

(declare-function w32--get-selection "w32-win.el")

(declare-function w32--selection-owner-p "w32-win.el")

(declare-function w32-selection-exists-p "w32select.c"
                  (&optional selection terminal))

;; Make copy&pasting in w32's console interact with the system's clipboard!
(cl-defmethod gui-backend-set-selection (type value
                                              &context (window-system nil))
  (w32--set-selection type value))

(cl-defmethod gui-backend-get-selection (type data-type
                                              &context (window-system nil))
  (w32--get-selection type data-type))

(cl-defmethod gui-backend-selection-owner-p (selection
                                             &context (window-system nil))
  (w32--selection-owner-p selection))

(cl-defmethod gui-selection-exists-p (selection
                                      &context (window-system nil))
  (w32-selection-exists-p selection))

(provide 'term/w32-nt)

;;; w32-nt.el ends here
