;;; ebrowsehook.el --- Find file hook for ebrowse.el

;; Copyright (C) 2000 Free Software Foundation Inc.

;; Author: Gerd Moellmann <gerd@gnu.org>
;; Maintainer: FSF
;; Keywords: C++ tags tools

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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;; Code:

;;;###autoload
(defun ebrowse-find-file-hook-fn ()
  "Function installed on `find-file-hooks'.
Load an Ebrowse class tree when there's special signature at
the beginning of the file."
  (when (looking-at "\\[ebrowse-hs")
    (ebrowse-load buffer-file-name)))

;;;###autoload
(add-hook 'find-file-hooks 'ebrowse-find-file-hook-fn)


