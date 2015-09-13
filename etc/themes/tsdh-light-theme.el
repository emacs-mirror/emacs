;;; tsdh-light-theme.el --- Tassilo's light custom theme

;; Copyright (C) 2011-2015 Free Software Foundation, Inc.

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

(deftheme tsdh-light
  "Minor tweaks to the Emacs white-background defaults.
Used and created by Tassilo Horn.")

(custom-theme-set-faces
 'tsdh-light
 '(Info-quoted ((t (:weight bold))))
 '(ace-jump-face-foreground ((t (:foreground "DeepPink" :box nil :weight bold))))
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground))))
 '(default ((t (:background "white" :foreground "black"))))
 '(diff-added ((t (:inherit diff-changed :background "light green"))))
 '(diff-changed ((t (:background "light steel blue"))))
 '(diff-indicator-added ((t (:inherit diff-indicator-changed))))
 '(diff-indicator-changed ((t (:weight bold))))
 '(diff-indicator-removed ((t (:inherit diff-indicator-changed))))
 '(diff-removed ((t (:inherit diff-changed :background "sandy brown"))))
 '(dired-directory ((t (:inherit font-lock-function-name-face :weight bold))))
 '(gnus-button ((t (:inherit button))))
 '(gnus-header-name ((t (:box (:line-width 1 :style released-button) :weight bold))))
 '(gnus-group-mail-1 ((t (:inherit gnus-group-mail-1-empty :weight bold))))
 '(gnus-group-mail-1-empty ((t (:foreground "DodgerBlue4"))))
 '(gnus-group-mail-2 ((t (:inherit gnus-group-mail-2-empty :weight bold))))
 '(gnus-group-mail-2-empty ((t (:foreground "DodgerBlue3"))))
 '(gnus-group-mail-3 ((t (:inherit gnus-group-mail-3-empty :weight bold))))
 '(gnus-group-mail-3-empty ((t (:foreground "DodgerBlue2"))))
 '(gnus-group-news-1 ((t (:inherit gnus-group-news-1-empty :weight bold))))
 '(gnus-group-news-1-empty ((t (:foreground "tomato4"))))
 '(gnus-group-news-2 ((t (:inherit gnus-group-news-2-empty :weight bold))))
 '(gnus-group-news-2-empty ((t (:foreground "tomato3"))))
 '(gnus-group-news-3 ((t (:inherit gnus-group-news-3-empty :weight bold))))
 '(gnus-group-news-3-empty ((t (:foreground "tomato2")))) '(header-line ((t (:inherit mode-line :inverse-video t))))
 '(hl-line ((t (:background "grey95"))))
 '(hl-paren-face ((t (:weight bold))) t)
 '(minibuffer-prompt ((t (:background "yellow" :foreground "medium blue" :box (:line-width -1 :color "red" :style released-button) :weight bold))))
 '(mode-line ((t (:background "wheat" :foreground "black" :box (:line-width 1 :color "tan") :family "DejaVu Sans"))))
 '(mode-line-inactive ((t (:inherit mode-line :foreground "dark gray"))))
 '(org-agenda-date ((t (:inherit org-agenda-structure))))
 '(org-agenda-date-today ((t (:inherit org-agenda-date :underline t))))
 '(org-agenda-date-weekend ((t (:inherit org-agenda-date :foreground "dark green"))))
 '(org-agenda-structure ((t (:foreground "Blue1" :weight bold :height 1.1 :family "DeJaVu Sans"))))
 '(org-hide ((t (:foreground "white"))))
 '(org-level-1 ((t (:inherit outline-1 :box nil))))
 '(org-level-2 ((t (:inherit outline-2 :box nil))))
 '(org-level-3 ((t (:inherit outline-3 :box nil))))
 '(org-level-4 ((t (:inherit outline-4 :box nil))))
 '(org-level-5 ((t (:inherit outline-5 :box nil))))
 '(org-level-6 ((t (:inherit outline-6 :box nil))))
 '(org-level-7 ((t (:inherit outline-7 :box nil))))
 '(org-level-8 ((t (:inherit outline-8 :box nil))))
 '(org-tag ((t (:weight bold))))
 '(outline-1 ((t (:inherit font-lock-function-name-face :weight bold))))
 '(outline-2 ((t (:inherit font-lock-variable-name-face :weight bold))))
 '(outline-3 ((t (:inherit font-lock-keyword-face :weight bold))))
 '(outline-4 ((t (:inherit font-lock-comment-face :weight bold))))
 '(outline-5 ((t (:inherit font-lock-type-face :weight bold))))
 '(outline-6 ((t (:inherit font-lock-constant-face :weight bold))))
 '(outline-7 ((t (:inherit font-lock-builtin-face :weight bold))))
 '(outline-8 ((t (:inherit font-lock-string-face :weight bold))))
 '(rcirc-my-nick ((t (:foreground "LightSkyBlue" :weight bold))))
 '(region ((t (:background "lightgoldenrod1"))))
 '(show-paren-match ((t (:background "Cyan1" :weight bold))))
 '(show-paren-mismatch ((t (:background "deep pink" :weight bold))))
 '(window-number-face ((t (:foreground "red" :weight bold)))))

(provide-theme 'tsdh-light)


;; Local Variables:
;; no-byte-compile: t
;; End:

;;; tsdh-light-theme.el ends here
