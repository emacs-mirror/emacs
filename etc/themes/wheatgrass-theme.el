;;; wheatgrass-theme.el --- custom theme for faces

;; Copyright (C) 2010 Free Software Foundation, Inc.

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(deftheme wheatgrass
  "Theme for basic, Font Lock, Isearch, Gnus, and Message faces.
The default face is wheat on a black background.  Other faces
are in shades of green, brown, and blue.")

(custom-theme-set-faces
 'wheatgrass
 '(default ((t (:foreground "wheat" :background "black"))))
 '(cursor ((t (:foreground "black" :background "thistle"))))
 '(highlight ((t (:foreground "white" :background "dark green"))))
 '(region ((t (:foreground "white" :background "dark green"))))
 '(font-lock-builtin-face ((t (:foreground "LightSteelBlue"))))
 '(font-lock-comment-face ((t (:foreground "SpringGreen3"))))
 '(font-lock-constant-face ((t (:foreground "turquoise"))))
 '(font-lock-function-name-face ((t (:foreground "pale green"))))
 '(font-lock-keyword-face ((t (:foreground "white"))))
 '(font-lock-string-face ((t (:foreground "dark khaki"))))
 '(font-lock-type-face ((t (:foreground "aquamarine"))))
 '(font-lock-variable-name-face ((t (:foreground "yellow green"))))
 '(font-lock-warning-face ((t (:foreground "salmon1"))))
 '(button ((t (:underline t :foreground "cyan"))))
 '(link ((t (:underline t :foreground "cyan"))))
 '(link-visited ((t (:underline t :foreground "dark cyan"))))
 '(isearch ((t (:foreground "white" :background "dark goldenrod"))))
 '(lazy-highlight ((t (:background "gray25"))))
 '(gnus-header-content ((t (:weight normal :foreground "yellow green"))))
 '(gnus-header-from ((t (:foreground "pale green"))))
 '(gnus-header-subject ((t (:foreground "pale turquoise"))))
 '(gnus-header-name ((t (:foreground "dark sea green"))))
 '(gnus-header-newsgroups ((t (:foreground "dark khaki"))))
 '(message-header-name ((t (:foreground "dark turquoise"))))
 '(message-header-cc ((t (:foreground "yellow green"))))
 '(message-header-other ((t (:foreground "dark khaki"))))
 '(message-header-subject ((t (:foreground "pale turquoise"))))
 '(message-header-to ((t (:foreground "pale green"))))
 '(message-cited-text ((t (:foreground "SpringGreen3"))))
 '(message-separator ((t (:foreground "deep sky blue")))))

(provide-theme 'wheatgrass)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; wheatgrass-theme.el ends here
