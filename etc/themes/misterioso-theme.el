;;; misterioso-theme.el --- Custom face theme for Emacs  -*- lexical-binding:t -*-

;; Copyright (C) 2011-2022 Free Software Foundation, Inc.

;; Author: Sebastian Hermida

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

;;; Code:

(deftheme misterioso
  "Predominantly blue/cyan faces on a dark cyan background.")

(let ((class '((class color) (min-colors 89))))

  (custom-theme-set-faces
   'misterioso
   ;; Ensure sufficient contrast on 256-color xterms.
   `(default ((((class color) (min-colors 4096))
	       (:background "#2d3743" :foreground "#e1e1e0"))
	      (,class
	       (:background "#3a3a3a" :foreground "#e1e1e0"))))
   `(cursor ((,class (:background "#415160"))))
   ;; Highlighting faces
   `(fringe ((,class (:background "#2e3748"))))
   `(highlight ((,class (:background "#338f86" :foreground "#e1e1e0"))))
   `(region ((,class (:background "#2d4948" :foreground "#e1e1e0"))))
   `(isearch ((,class (:background "#fcffad" :foreground "#000000"))))
   `(lazy-highlight ((,class (:background "#338f86"))))
   `(trailing-whitespace ((,class (:background "#ff4242"))))
   ;; Mode line faces
   `(mode-line ((,class (:background "#212931" :foreground "#eeeeec"))))
   `(mode-line-inactive
     ((,class (:background "#878787" :foreground "#eeeeec"))))
   `(header-line ((,class (:background "#808080" :foreground "#333333"))))
   ;; Escape and prompt faces
   `(minibuffer-prompt ((,class (:foreground "#729fcf" :weight bold))))
   ;; Font lock faces
   `(font-lock-builtin-face ((,class (:foreground "#23d7d7"))))
   `(font-lock-comment-face ((,class (:foreground "#74af68"))))
   `(font-lock-constant-face ((,class (:foreground "#008b8b"))))
   `(font-lock-function-name-face
     ((,class (:foreground "#00ede1" :weight bold))))
   `(font-lock-keyword-face ((,class (:foreground "#ffad29" :weight bold))))
   `(font-lock-string-face ((,class (:foreground "#e67128"))))
   `(font-lock-type-face ((,class (:foreground "#34cae2"))))
   `(font-lock-variable-name-face ((,class (:foreground "#dbdb95"))))
   `(font-lock-warning-face ((,class (:foreground "#ff4242" :weight bold))))
   ;; Buttons and links
   `(button ((,class (:underline t))))
   `(link ((,class (:foreground "#59e9ff" :underline t))))
   `(link-visited ((,class (:foreground "#ed74cd" :underline t))))
   ;; Ediff
   `(ediff-even-diff-A ((,class (:background "#1d2430"))))
   `(ediff-even-diff-B ((,class (:background "#1d2430"))))
   `(ediff-even-diff-C ((,class (:background "#1d2430"))))
   `(ediff-odd-diff-A ((,class (:background "#415160"))))
   `(ediff-odd-diff-B ((,class (:background "#415160"))))
   `(ediff-odd-diff-C ((,class (:background "#415160"))))
   ;; Gnus faces
   `(gnus-group-news-1 ((,class (:foreground "#ff4242" :weight bold))))
   `(gnus-group-news-1-low ((,class (:foreground "#ff4242"))))
   `(gnus-group-news-2 ((,class (:foreground "#00ede1" :weight bold))))
   `(gnus-group-news-2-low ((,class (:foreground "#00ede1"))))
   `(gnus-group-news-3 ((,class (:foreground "#23d7d7" :weight bold))))
   `(gnus-group-news-3-low ((,class (:foreground "#23d7d7"))))
   `(gnus-group-news-4 ((,class (:foreground "#74af68" :weight bold))))
   `(gnus-group-news-4-low ((,class (:foreground "#74af68"))))
   `(gnus-group-news-5 ((,class (:foreground "#dbdb95" :weight bold))))
   `(gnus-group-news-5-low ((,class (:foreground "#dbdb95"))))
   `(gnus-group-news-low ((,class (:foreground "#008b8b"))))
   `(gnus-group-mail-1 ((,class (:foreground "#ff4242" :weight bold))))
   `(gnus-group-mail-1-low ((,class (:foreground "#ff4242"))))
   `(gnus-group-mail-2 ((,class (:foreground "#00ede1" :weight bold))))
   `(gnus-group-mail-2-low ((,class (:foreground "#00ede1"))))
   `(gnus-group-mail-3 ((,class (:foreground "#23d7d7"  :weight bold))))
   `(gnus-group-mail-3-low ((,class (:foreground "#23d7d7"))))
   `(gnus-group-mail-low ((,class (:foreground "#008b8b"))))
   `(gnus-header-content ((,class (:weight normal :foreground "#ffad29"))))
   `(gnus-header-from ((,class (:foreground "#e67128" :weight bold))))
   `(gnus-header-subject ((,class (:foreground "#dbdb95"))))
   `(gnus-header-name ((,class (:foreground "#00ede1"))))
   `(gnus-header-newsgroups ((,class (:foreground "#e67128"))))
   ;; Message faces
   `(message-header-name ((,class (:foreground "#ffad29" :weight bold))))
   `(message-header-cc ((,class (:foreground "#e67128"))))
   `(message-header-other ((,class (:foreground "#e67128"))))
   `(message-header-subject ((,class (:foreground "#dbdb95"))))
   `(message-header-to ((,class (:foreground "#00ede1"))))
   `(message-cited-text ((,class (:foreground "#74af68"))))
   `(message-separator ((,class (:foreground "#23d7d7"))))
   ;; ANSI colors
   `(ansi-color-black ((,class (:background "#2d3743" :foreground "#2d3743"))))
   `(ansi-color-red ((,class (:background "#da3938" :foreground "#da3938"))))
   `(ansi-color-green ((,class (:background "#74af68" :foreground "#74af68"))))
   `(ansi-color-yellow ((,class (:background "#dbdb95" :foreground "#dbdb95"))))
   `(ansi-color-blue ((,class (:background "#34cae2" :foreground "#34cae2"))))
   `(ansi-color-magenta ((,class (:background "#b33c97"
				  :foreground "#b33c97"))))
   `(ansi-color-cyan ((,class (:background "#008b8b" :foreground "#008b8b"))))
   `(ansi-color-white ((,class (:background "#e1e1e0" :foreground "#e1e1e0"))))
   `(ansi-color-bright-black ((,class (:background "#415160"
                                       :foreground "#415160"))))
   `(ansi-color-bright-red ((,class (:background "#ff4242"
                                     :foreground "#ff4242"))))
   `(ansi-color-bright-green ((,class (:background "#74cd65"
                                       :foreground "#74cd65"))))
   `(ansi-color-bright-yellow ((,class (:background "#ffad29"
                                        :foreground "#ffad29"))))
   `(ansi-color-bright-blue ((,class (:background "#59e9ff"
                                      :foreground "#59e9ff"))))
   `(ansi-color-bright-magenta ((,class (:background "#ed74cd"
                                         :foreground "#ed74cd"))))
   `(ansi-color-bright-cyan ((,class (:background "#00ede1"
                                      :foreground "#00ede1"))))
   `(ansi-color-bright-white ((,class (:background "#eeeeec"
                                       :foreground "#eeeeec"))))))

(provide-theme 'misterioso)

;;; misterioso-theme.el  ends here
