;;; dichromacy-theme.el --- color theme suitable for color-blind users  -*- lexical-binding:t -*-

;; Copyright (C) 2011-2023 Free Software Foundation, Inc.

;; Author: Chong Yidong <cyd@stupidchicken>

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

;;;###theme-autoload
(deftheme dichromacy
  "Face colors suitable for red/green color-blind users.
The color palette is from B. Wong, Nature Methods 8, 441 (2011).
It is intended to provide good variability while being easily
differentiated by individuals with protanopia or deuteranopia.

Basic, Font Lock, Isearch, Gnus, Message, Flyspell, and
Ansi-Color faces are included."
  :background-mode 'light
  :kind 'color-scheme)

(let ((class '((class color) (min-colors 89)))
      (orange "#e69f00")
      (skyblue "#56b4e9")
      (bluegreen "#009e73")
      (yellow "#f8ec59")
      (blue "#0072b2")
      (vermilion "#d55e00")
      (redpurple "#cc79a7")
      (bluegray "#848ea9"))
  (custom-theme-set-faces
   'dichromacy
   `(default ((,class (:foreground "black" :background "white"))))
   `(cursor ((,class (:background "black"))))
   ;; Highlighting faces
   `(fringe ((,class (:background "#f7f7f7"))))
   `(highlight ((,class (:foreground ,blue :background "#e5e5e5"))))
   `(region ((,class (:foreground unspecified :background ,yellow))))
   `(secondary-selection ((,class (:background "#e5e5e5"))))
   `(isearch ((,class (:foreground "white" :background ,vermilion))))
   `(lazy-highlight ((,class (:foreground "white" :background ,redpurple))))
   `(trailing-whitespace ((,class (:background ,vermilion))))
   ;; Mode line faces
   `(mode-line ((,class (:box (:line-width -1 :style released-button)
			      :background "#e5e5e5" :foreground "black"))))
   `(mode-line-inactive ((,class (:box (:line-width -1 :style released-button)
				       :background "#b0b0b0"
				       :foreground "black"))))
   ;; Escape and prompt faces
   `(minibuffer-prompt ((,class (:weight bold :foreground ,blue))))
   `(escape-glyph ((,class (:foreground ,vermilion))))
   `(homoglyph ((,class (:foreground ,vermilion))))
   `(error ((,class (:weight bold :slant italic
			     :foreground ,vermilion))))
   `(warning ((,class (:foreground ,orange))))
   `(success ((,class (:foreground ,bluegreen))))
   ;; Font lock faces
   `(font-lock-builtin-face ((,class (:foreground ,blue))))
   `(font-lock-comment-face ((,class (:slant italic :foreground ,bluegreen))))
   `(font-lock-constant-face ((,class (:weight bold :foreground ,vermilion))))
   `(font-lock-function-name-face ((,class (:foreground ,vermilion))))
   `(font-lock-keyword-face ((,class (:weight bold :foreground ,skyblue))))
   `(font-lock-string-face ((,class (:foreground ,bluegray))))
   `(font-lock-type-face ((,class (:weight bold :foreground ,blue))))
   `(font-lock-variable-name-face ((,class (:weight bold :foreground ,orange))))
   ;; Button and link faces
   `(link ((,class (:underline t :foreground ,blue))))
   `(link-visited ((,class (:underline t :foreground ,redpurple))))
   ;; Gnus faces
   `(gnus-group-news-1 ((,class (:weight bold :foreground ,vermilion))))
   `(gnus-group-news-1-low ((,class (:foreground ,vermilion))))
   `(gnus-group-news-2 ((,class (:weight bold :foreground ,orange))))
   `(gnus-group-news-2-low ((,class (:foreground ,orange))))
   `(gnus-group-news-3 ((,class (:weight bold :foreground ,skyblue))))
   `(gnus-group-news-3-low ((,class (:foreground ,skyblue))))
   `(gnus-group-news-4 ((,class (:weight bold :foreground ,redpurple))))
   `(gnus-group-news-4-low ((,class (:foreground ,redpurple))))
   `(gnus-group-news-5 ((,class (:weight bold :foreground ,blue))))
   `(gnus-group-news-5-low ((,class (:foreground ,blue))))
   `(gnus-group-news-low ((,class (:foreground ,bluegreen))))
   `(gnus-group-mail-1 ((,class (:weight bold :foreground ,vermilion))))
   `(gnus-group-mail-1-low ((,class (:foreground ,vermilion))))
   `(gnus-group-mail-2 ((,class (:weight bold :foreground ,orange))))
   `(gnus-group-mail-2-low ((,class (:foreground ,orange))))
   `(gnus-group-mail-3 ((,class (:weight bold :foreground ,skyblue))))
   `(gnus-group-mail-3-low ((,class (:foreground ,skyblue))))
   `(gnus-group-mail-low ((,class (:foreground ,bluegreen))))
   `(gnus-header-content ((,class (:foreground ,redpurple))))
   `(gnus-header-from ((,class (:weight bold :foreground ,blue))))
   `(gnus-header-subject ((,class (:foreground ,orange))))
   `(gnus-header-name ((,class (:foreground ,skyblue))))
   `(gnus-header-newsgroups ((,class (:foreground ,vermilion))))
   ;; Image-Dired
   `(image-dired-thumb-flagged ((,class (:background ,vermilion))))
   `(image-dired-thumb-mark ((,class (:background ,orange))))
   ;; Message faces
   `(message-header-name ((,class (:foreground ,skyblue))))
   `(message-header-cc ((,class (:foreground ,vermilion))))
   `(message-header-other ((,class (:foreground ,bluegreen))))
   `(message-header-subject ((,class (:foreground ,orange))))
   `(message-header-to ((,class (:weight bold :foreground ,blue))))
   `(message-cited-text ((,class (:slant italic :foreground ,bluegreen))))
   `(message-separator ((,class (:weight bold :foreground ,redpurple))))
   ;; Flyspell
   `(flyspell-duplicate ((,class (:weight unspecified :foreground unspecified
				  :slant unspecified :underline ,orange))))
   `(flyspell-incorrect ((,class (:weight unspecified :foreground unspecified
				  :slant unspecified :underline ,redpurple))))
   ;; ANSI color
   `(ansi-color-black ((,class (:background "black" :foreground "black"))))
   `(ansi-color-red ((,class (:background ,vermilion
			      :foreground ,vermilion))))
   `(ansi-color-green ((,class (:background ,bluegreen
				:foreground ,bluegreen))))
   `(ansi-color-yellow ((,class (:background ,yellow :foreground ,yellow))))
   `(ansi-color-blue ((,class (:background ,blue :foreground ,blue))))
   `(ansi-color-magenta ((,class (:background ,redpurple
				  :foreground ,redpurple))))
   `(ansi-color-cyan ((,class (:background ,skyblue :foreground ,skyblue))))
   `(ansi-color-white ((,class (:background "gray90" :foreground "gray90"))))
   `(ansi-color-bright-black ((,class (:background "black"
				       :foreground "black"))))
   `(ansi-color-bright-red ((,class (:background ,vermilion
				     :foreground ,vermilion))))
   `(ansi-color-bright-green ((,class (:background ,bluegreen
				       :foreground ,bluegreen))))
   `(ansi-color-bright-yellow ((,class (:background ,yellow
					:foreground ,yellow))))
   `(ansi-color-bright-blue ((,class (:background ,blue :foreground ,blue))))
   `(ansi-color-bright-magenta ((,class (:background ,redpurple
					 :foreground ,redpurple))))
   `(ansi-color-bright-cyan ((,class (:background ,skyblue
				      :foreground ,skyblue))))
   `(ansi-color-bright-white ((,class (:background "gray90"
				       :foreground "gray90"))))))

(provide-theme 'dichromacy)

;;; dichromacy-theme.el ends here
