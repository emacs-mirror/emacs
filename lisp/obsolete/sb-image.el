;;; sb-image.el --- Image management for speedbar  -*- lexical-binding: t; -*-

;; Copyright (C) 1999-2003, 2005-2019, 2021-2022 Free Software
;; Foundation, Inc.

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: file, tags, tools
;; Obsolete-since: 28.1

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

;; This file is obsolete.
;;
;; Supporting Image display for Emacs 20 and less, Emacs 21, and XEmacs,
;; is a challenging task, which doesn't take kindly to being byte compiled.
;; When sharing speedbar.elc between these three applications, the Image
;; support can get lost.
;;
;; By splitting out that hard part into this file, and avoiding byte
;; compilation, one copy speedbar can support all these platforms together.
;;
;; This file requires the `image' package if it is available.

(require 'ezimage)

;;; Code:

(defalias 'defimage-speedbar 'defezimage)

(provide 'sb-image)

;;; sb-image.el ends here
