;;; simple-16-theme.el --- Custom theme for faces  -*- lexical-binding:t -*-

;; Copyright (C) 2020 Free Software Foundation, Inc.

;; Author: Jimmy Aguilar Mena <spacibba@aol.com>

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

(deftheme simple-16
  "Dark theme with a set of simple 16 colors only.")

(defmacro simple-16-theme-color (colorname)
  "Get color by name COLORNAME from `simple-16-color-theme-alist'."
  (alist-get colorname '((black . "#000000")
                         (red . "#cd0000")
                         (green . "#00cd00")
                         (yellow . "#cdcd00")
                         (blue . "#0000ee")
                         (magenta . "#cd00cd")
                         (cyan . "#00cdcd")
                         (white . "#e5e5e5")
                         (brightblack . "#444444") ;;
                         (brightred . "#ff0000")
                         (brightgreen . "#00ff00")
                         (brightyellow . "#ffff00")
                         (brightblue . "#5c5cff")
                         (brightmagenta . "#ff00ff")
                         (brightcyan . "#00ffff")
                         (brightwhite . "#ffffff"))))

(custom-theme-set-faces
 'simple-16
 `(default ((t (:background ,(simple-16-theme-color black)
                                 :foreground ,(simple-16-theme-color white)))))

 `(font-lock-preprocessor-face ((t (:foreground ,(simple-16-theme-color magenta)))))
 `(font-lock-comment-face ((t (:foreground ,(simple-16-theme-color cyan)))))
 `(font-lock-doc-face ((t (:foreground ,(simple-16-theme-color cyan)))))
 `(font-lock-string-face ((t (:foreground ,(simple-16-theme-color red)))))
 `(font-lock-function-name-face ((t (:foreground ,(simple-16-theme-color white)))))
 `(font-lock-variable-name-face ((t (:foreground ,(simple-16-theme-color white)))))
 `(font-lock-constant-face ((t (:foreground ,(simple-16-theme-color magenta)))))
 `(font-lock-type-face ((t (:foreground ,(simple-16-theme-color green)))))
 `(font-lock-keyword-face ((t (:foreground ,(simple-16-theme-color yellow)))))
 `(font-lock-builtin-face ((t (:foreground ,(simple-16-theme-color green)))))

 `(highlight ((t (:background ,(simple-16-theme-color brightblack)
                                   :foreground nil))))
 `(secondary-selection ((t (:background ,(simple-16-theme-color brightblack)
                                             :foreground nil))))

 `(isearch ((t (:background ,(simple-16-theme-color blue)
			         :foreground ,(simple-16-theme-color white)
			         :weight ultrabold))))
 `(lazy-highlight ((t (:background ,(simple-16-theme-color brightblue)))))

 `(region ((t (:background ,(simple-16-theme-color brightblue)))))

 `(mode-line ((t (:background ,(simple-16-theme-color blue)
			           :foreground ,(simple-16-theme-color white)))))
 `(mode-line-inactive ((t (:background ,(simple-16-theme-color brightblack)
			                    :foreground ,(simple-16-theme-color white)))))

 `(line-number ((t (:foreground ,(simple-16-theme-color brightblack)))))
 `(line-number-current-line ((t (:foreground ,(simple-16-theme-color green)))))
 `(fill-column-indicator ((t (:foreground ,(simple-16-theme-color brightblack)))))

 `(show-paren-match ((t (:background ,(simple-16-theme-color brightblack)
                                          :inherit nil))))

 `(tab-bar ((t (:background ,(simple-16-theme-color black)
                                 :foreground ,(simple-16-theme-color white)
                                 :inverse-video nil))))
 `(tab-bar-tab ((t (:inherit tab-bar :weight ultra-bold :underline t))))

 `(tab-bar-tab-inactive ((t (:background ,(simple-16-theme-color black)
                                              :foreground ,(simple-16-theme-color brightwhite)
                                              :weight normal
                                              :underline nil))))

 `(Man-overstrike ((t (:inherit font-lock-type-face :bold t))))
 `(Man-overstrike ((t (:inherit font-lock-keyword-face :underline t))))

 `(which-func ((t (:background nil :foreground ,(simple-16-theme-color white)))))

 ;; External packages
 ;; Company
 `(company-tooltip ((t (:background ,(simple-16-theme-color brightblack)
                                         :foreground ,(simple-16-theme-color white)))))
 `(company-tooltip-common ((t (:inherit company-tooltip
                                        :foreground ,(simple-16-theme-color green)))))
 `(company-tooltip-selection ((t (:background ,(simple-16-theme-color blue)
                                              :weight ultra-bold))))
 `(company-scrollbar-bg ((t (:background ,(simple-16-theme-color brightblack)))))
 `(company-scrollbar-fg ((t (:background ,(simple-16-theme-color blue)))))

 `(company-scrollbar-fg ((t (:foreground ,(simple-16-theme-color cyan)))))

 ;; Ivy
 `(ivy-minibuffer-match-face-1 ((t (:inherit nil :background nil
                                                  :foreground nil
                                                  :underline t))))
 ;; Ivy like lazy-highlight
 `(ivy-minibuffer-match-face-2 ((t (:background ,(simple-16-theme-color brightblue)))))
 `(ivy-minibuffer-match-face-3 ((t (:background ,(simple-16-theme-color brightblue)))))
 `(ivy-minibuffer-match-face-4 ((t (:background ,(simple-16-theme-color brightblue)))))

 ;; Swiper
 `(swiper-match-face-1 ((t (:inherit nil :background nil :underline t))))
 ;; Swiper like lazy-highlight
 `(swiper-background-match-face-2 ((t (:background ,(simple-16-theme-color brightblue)))))
 `(swiper-background-match-face-3 ((t (:background ,(simple-16-theme-color brightblue)))))
 `(swiper-background-match-face-4 ((t (:background ,(simple-16-theme-color brightblue)))))
 ;; Swiper background like isearch
 `(swiper-match-face-2 ((t (:background ,(simple-16-theme-color brightblue)))))
 `(swiper-match-face-3 ((t (:background ,(simple-16-theme-color brightblue)))))
 `(swiper-match-face-4 ((t (:background ,(simple-16-theme-color brightblue)))))

 ;; Avy
 `(avy-lead-face ((t (:background ,(simple-16-theme-color blue)
                                       :foreground ,(simple-16-theme-color red)))))
 )

(provide-theme 'simple-16)

;;; simple-16-theme.el ends here
