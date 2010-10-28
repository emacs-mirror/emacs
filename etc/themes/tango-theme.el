;;; tango-theme.el --- Tango-based custom theme for faces

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

;;; Commentary

;; The colors in this theme come from the Tango palette, which is in
;; the public domain: http://tango.freedesktop.org/

;;; Code:

(deftheme tango
  "Theme for faces, based on the Tango palette on a light background.
Basic, Font Lock, Isearch, Gnus, and Message faces are included.")

(custom-theme-set-faces
 'tango
 '(default ((t (:foreground "#16191a" :background "#eeeeec"))))
 '(cursor ((t (:foreground "#eeeeec" :background "#204a87"))))
 '(highlight ((t (:background "#babdb6"))))
 '(region ((t (:background "#babdb6"))))
 '(font-lock-builtin-face ((t (:weight bold :foreground "#204a87"))))
 '(font-lock-comment-face ((t (:foreground "#204a87"))))
 '(font-lock-constant-face ((t (:weight bold :foreground "#5c3566"))))
 '(font-lock-function-name-face ((t (:weight bold :foreground "#ce5c00"))))
 '(font-lock-keyword-face ((t (:foreground "#a40000"))))
 '(font-lock-string-face ((t (:foreground "#5c3566"))))
 '(font-lock-type-face ((t (:weight bold :foreground "#4e9a06"))))
 '(font-lock-variable-name-face ((t (:weight bold :foreground "#c17d11"))))
 '(font-lock-warning-face ((t (:foreground "#cc0000"))))
 '(button ((t (:underline t :foreground "#204a87"))))
 '(link ((t (:underline t :foreground "#204a87"))))
 '(link-visited ((t (:underline t :foreground "#3465a4"))))
 '(mode-line ((t (:box (:line-width -1 :style released-button)
		       :background "#d3d7cf" :foreground "black"))))
 '(mode-line-inactive ((t (:box (:line-width -1 :style released-button)
				:background "#babdb6" :foreground "black"))))
 '(isearch ((t (:foreground "#ffffff" :background "#ce5c00"))))
 '(lazy-highlight ((t (:background "#e9b96e"))))
 '(gnus-group-news-1 ((t (:weight bold :foreground "#5c3566"))))
 '(gnus-group-news-1-low ((t (:foreground "#5c3566"))))
 '(gnus-group-news-2 ((t (:weight bold :foreground "#204a87"))))
 '(gnus-group-news-2-low ((t (:foreground "#204a87"))))
 '(gnus-group-news-3 ((t (:weight bold :foreground "#4e0a06"))))
 '(gnus-group-news-3-low ((t (:foreground "#4e0a06"))))
 '(gnus-group-news-4 ((t (:weight bold :foreground "#7a4c02"))))
 '(gnus-group-news-4-low ((t (:foreground "#7a4c02"))))
 '(gnus-group-news-5 ((t (:weight bold :foreground "#ce5c00"))))
 '(gnus-group-news-5-low ((t (:foreground "#ce5c00"))))
 '(gnus-group-news-low ((t (:foreground "#888a85"))))
 '(gnus-group-mail-1 ((t (:weight bold :foreground "#5c3566"))))
 '(gnus-group-mail-1-low ((t (:foreground "#5c3566"))))
 '(gnus-group-mail-2 ((t (:weight bold :foreground "#204a87"))))
 '(gnus-group-mail-2-low ((t (:foreground "#204a87"))))
 '(gnus-group-mail-3 ((t (:weight bold :foreground "#4e0a06"))))
 '(gnus-group-mail-3-low ((t (:foreground "#4e0a06"))))
 '(gnus-group-mail-low ((t (:foreground "#888a85"))))
 '(gnus-header-content ((t (:foreground "#4e9a06"))))
 '(gnus-header-from ((t (:weight bold :foreground "#c4a000"))))
 '(gnus-header-subject ((t (:foreground "#4e0a06"))))
 '(gnus-header-name ((t (:foreground "#204a87"))))
 '(gnus-header-newsgroups ((t (:foreground "#888a85"))))
 '(message-header-name ((t (:foreground "#204a87"))))
 '(message-header-cc ((t (:foreground "#c4a000"))))
 '(message-header-other ((t (:foreground "#c17d11"))))
 '(message-header-subject ((t (:foreground "#4e0a06"))))
 '(message-header-to ((t (:weight bold :foreground "#c4a000"))))
 '(message-cited-text ((t (:foreground "#888a85"))))
 '(message-separator ((t (:weight bold :foreground "#4e9a06")))))

(provide-theme 'tango)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; tango-theme.el ends here
