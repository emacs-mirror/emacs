;;; tango-dark-theme.el --- Tango-based custom theme for faces

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

(deftheme tango-dark
  "Theme for faces, based on the Tango palette on a dark background.
Basic, Font Lock, Isearch, Gnus, and Message faces are included.")

(custom-theme-set-faces
 'tango-dark
 '(default ((t (:foreground "#eeeeec" :background "#2e3436"))))
 '(cursor ((t (:foreground "#2e3436" :background "#fce94f"))))
 '(highlight ((t (:foreground "#2e3436" :background "#edd400"))))
 '(region ((t (:background "#555753"))))
 '(font-lock-builtin-face ((t (:foreground "#ad7fa8"))))
 '(font-lock-comment-face ((t (:foreground "#73d216"))))
 '(font-lock-constant-face ((t (:foreground "#e6a8df"))))
 '(font-lock-function-name-face ((t (:foreground "#fce94f"))))
 '(font-lock-keyword-face ((t (:foreground "#8cc4ff"))))
 '(font-lock-string-face ((t (:foreground "#e9b96e"))))
 '(font-lock-type-face ((t (:foreground "#a5ff4d"))))
 '(font-lock-variable-name-face ((t (:foreground "#fcaf3e"))))
 '(font-lock-warning-face ((t (:foreground "#ef2929"))))
 '(button ((t (:underline t :foreground "#729fcf"))))
 '(link ((t (:underline t :foreground "#729fcf"))))
 '(link-visited ((t (:underline t :foreground "#3465a4"))))
 '(mode-line ((t (:box (:line-width -1 :style released-button)
		       :background "#d3d7cf" :foreground "black"))))
 '(mode-line-inactive ((t (:box (:line-width -1 :style released-button)
				:background "#555753" :foreground "white"))))
 '(isearch ((t (:foreground "#ffffff" :background "#ce5c00"))))
 '(lazy-highlight ((t (:background "#8f5902"))))
 '(gnus-group-news-1 ((t (:foreground "#ad7fa8"))))
 '(gnus-group-news-1-low ((t (:foreground "#75507b"))))
 '(gnus-group-news-2 ((t (:foreground "#729fcf"))))
 '(gnus-group-news-2-low ((t (:foreground "#3465a4"))))
 '(gnus-group-news-3 ((t (:foreground "#8ae234"))))
 '(gnus-group-news-3-low ((t (:foreground "#73d216"))))
 '(gnus-group-news-4 ((t (:foreground "#e9b9e6"))))
 '(gnus-group-news-4-low ((t (:foreground "#c17d11"))))
 '(gnus-group-news-5 ((t (:foreground "#fcaf3e"))))
 '(gnus-group-news-5-low ((t (:foreground "#f57900"))))
 '(gnus-group-news-low ((t (:foreground "#edd400"))))
 '(gnus-group-mail-1 ((t (:foreground "#ad7fa8"))))
 '(gnus-group-mail-1-low ((t (:foreground "#75507b"))))
 '(gnus-group-mail-2 ((t (:foreground "#729fcf"))))
 '(gnus-group-mail-2-low ((t (:foreground "#3465a4"))))
 '(gnus-group-mail-3 ((t (:foreground "#8ae234"))))
 '(gnus-group-mail-3-low ((t (:foreground "#73d216"))))
 '(gnus-group-mail-low ((t (:foreground "#edd400"))))
 '(gnus-header-content ((t (:weight normal :foreground "#c4a000"))))
 '(gnus-header-from ((t (:foreground "#edd400"))))
 '(gnus-header-subject ((t (:foreground "#8ae234"))))
 '(gnus-header-name ((t (:foreground "#729fcf"))))
 '(gnus-header-newsgroups ((t (:foreground "#c17d11"))))
 '(message-header-name ((t (:foreground "#729fcf"))))
 '(message-header-cc ((t (:foreground "#c4a000"))))
 '(message-header-other ((t (:foreground "#c17d11"))))
 '(message-header-subject ((t (:foreground "#8ae234"))))
 '(message-header-to ((t (:foreground "#edd400"))))
 '(message-cited-text ((t (:foreground "#8ae234"))))
 '(message-separator ((t (:foreground "#ad7fa8")))))

(provide-theme 'tango-dark)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; tango-dark-theme.el ends here
