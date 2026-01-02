;;; leuven-dark-theme.el --- Awesome Emacs color theme on dark background  -*- lexical-binding: t; -*-

;; Copyright (C) 2003-2026 Free Software Foundation, Inc.

;; Author: Fabrice Niessen <(concat "fniessen" at-sign "pirilampo.org")>
;; Contributor: Thibault Polge <(concat "thibault" at-sign "thb.lt")>
;; URL: https://github.com/fniessen/emacs-leuven-theme
;; Version: 1.2.0
;; Last-Updated: 2022-10-04 01:15
;; Keywords: color theme

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

;; This elegant Org-enhancing color theme "leuven-dark" ROCKS!
;; ... and not just for Org mode.
;;
;; To use it, put the following in your Emacs configuration file:
;;
;;   (load-theme 'leuven-dark t)
;;
;; Requirements: Emacs 24+.

;;; Code:

;;; Options.

(defgroup leuven-dark nil
  "Leuven theme options.
The theme has to be reloaded after changing anything in this group."
  :group 'faces)

(defcustom leuven-dark-scale-org-document-title t
  "Scale Org document title.
This can be nil for unscaled, t for using the theme default, or a scaling
number."
  :type '(choice
          (const :tag "Unscaled" nil)
          (const :tag "Default provided by theme" t)
          (number :tag "Set scaling")))

(defcustom leuven-dark-scale-outline-headlines t
  "Scale `outline' (and `org') level-1 headlines.
This can be nil for unscaled, t for using the theme default, or a scaling
number."
  :type '(choice
          (const :tag "Unscaled" nil)
          (const :tag "Default provided by theme" t)
          (number :tag "Set scaling")))

(defcustom leuven-dark-scale-org-agenda-structure t
  "Scale Org agenda structure lines, like dates.
This can be nil for unscaled, t for using the theme default, or a scaling
number."
  :type '(choice
          (const :tag "Unscaled" nil)
          (const :tag "Default provided by theme" t)
          (number :tag "Set scaling")))

(defcustom leuven-dark-scale-volatile-highlight t
  "Increase size in the `next-error' face.
This can be nil for unscaled, t for using the theme default, or a scaling
number."
  :type '(choice
          (const :tag "Unscaled" nil)
          (const :tag "Default provided by theme" t)
          (number :tag "Set scaling")))

;;;###autoload
(defun leuven-dark-scale-font (control default-height)
  "Function for splicing optional font heights into face descriptions.
CONTROL can be a number, nil, or t.  When t, use DEFAULT-HEIGHT."
  (cond
   ((numberp control) (list :height control))
   ((eq t control) (list :height default-height))
   (t nil)))

;;; Theme Faces.

;;;###theme-autoload
(deftheme leuven-dark
  "Face colors with a dark background.
Basic, Font Lock, Isearch, Gnus, Message, Org mode, Diff, Ediff,
Flyspell, Semantic, and Ansi-Color faces are included -- and much
more..."
  :background-mode 'dark
  :family 'leuven
  :kind 'color-scheme)

(let ((class '((class color) (min-colors 89)))

      ;; Leuven generic colors.
      (cancel '(:slant italic :strike-through t :foreground "#5b5660"))
      (clock-line '(:box (:line-width 1 :color "#cfa161") :foreground "#ffffff" :background "#1636ff"))
      (code-block '(:foreground "#ffff7f" :background "#252046"))
      (code-inline '(:foreground "#ff9bff" :background "#262031"))
      (column '(:height 1.0 :weight normal :slant normal :underline nil :strike-through nil :foreground "#1e52b8" :background "#252c48"))
      (completion-inline '(:weight normal :foreground "#443f49" :inherit hl-line)) ; Like Google.
      (completion-other-candidates '(:weight bold :foreground "#ffffff" :background "#372a2a"))
      (completion-selected-candidate '(:weight bold :foreground "#25202a" :background "#ffad65"))
      (diff-added '(:background "#442049"))
      (diff-changed '(:foreground "#ffff0b" :background "#443f2a"))
      (diff-header '(:weight bold :foreground "#83ffff" :background "#252073"))
      (diff-hunk-header '(:foreground "#6bff6f" :background "#252f2a"))
      (diff-none '(:foreground "#7b777f"))
      (diff-refine-added '(:background "#6d0d73"))
      (diff-refine-removed '(:background "#06494f"))
      (diff-removed '(:background "#25353e"))
      (directory '(:weight bold :foreground "#ffff0b" :background "#252053"))
      (file '(:foreground "#ffffff"))
      (function-param '(:foreground "#de8d83"))
      (grep-file-name '(:weight bold :foreground "#d8b76b")) ; Used for grep hits.
      (grep-line-number '(:weight bold :foreground "#5fca5b"))
      (highlight-blue '(:background "#3c312a"))
      (highlight-blue2 '(:background "#3e2d2f"))
      (highlight-gray '(:background "#3e3944"))
      (highlight-green '(:background "#2f0e3a"))
      (highlight-red '(:background "#063741"))
      (highlight-yellow '(:background "#2d2058"))
      (link '(:weight normal :underline t :foreground "#ff925a"))
      (link-no-underline '(:weight normal :foreground "#ff925a"))
      (mail-header-name '(:family "Sans Serif" :weight normal :foreground "#615c67"))
      (mail-header-other '(:family "Sans Serif" :slant normal :foreground "#9d99a1"))
      (mail-read '(:foreground "#77737b"))
      (mail-read-high '(:foreground "#837f87"))
      (mail-ticked '(:foreground "#06ccff"))
      (mail-to '(:family "Sans Serif" :underline nil :foreground "#ff925a"))
      (mail-unread '(:weight bold :foreground "#ffffff"))
      (mail-unread-high '(:weight bold :foreground "#eea682"))
      (marked-line '(:foreground "#5affff" :background "#06555f"))
      (match '(:weight bold :background "#0601ff")) ; occur patterns + match in helm for files + match in Org files.
      (ol1 `(,@(leuven-dark-scale-font leuven-dark-scale-outline-headlines 1.3) :weight bold :overline "#5d5862" :foreground "#c7c3cb" :background "#322d37"))
      (ol2 '(:height 1.0 :weight bold :overline "#efcab2" :foreground "#efcab2" :background "#3d2a2d"))
      (ol3 '(:height 1.0 :weight bold :foreground "#ffaae3" :background "#332038"))
      (ol4 '(:height 1.0 :weight bold :slant normal :foreground "#1a9cff"))
      (ol5 '(:height 1.0 :weight bold :slant normal :foreground "#21da7a"))
      (ol6 '(:height 1.0 :weight bold :slant italic :foreground "#ff883d"))
      (ol7 '(:height 1.0 :weight bold :slant italic :foreground "#d451d9"))
      (ol8 '(:height 1.0 :weight bold :slant italic :foreground "#077ffa"))
      (paren-matched '(:background "#7B4B98")) ; XXX Edited by hqnd.
      (paren-unmatched '(:weight bold :underline "#06ffff" :foreground "#ffffff" :background "#065a64"))
      (region '(:background "#752c0b"))
      (shadow '(:foreground "#848088"))
      (string '(:foreground "#ff7fff")) ; or #34c8d8
      (subject '(:family "Sans Serif" :weight bold :foreground "#ffffff"))
      (symlink '(:foreground "#e37233"))
      (tab '(:foreground "#3a353f" :background "#25202a"))
      (trailing '(:foreground "#3a353f" :background "#252076"))
      (volatile-highlight '(:underline nil :foreground "#25202a" :background "#66c96f"))
      (volatile-highlight-supersize `(,@(leuven-dark-scale-font leuven-dark-scale-volatile-highlight 1.1) :underline nil :foreground "#25202a" :background "#66c96f")) ; flash-region
      (vc-branch '(:box (:line-width 1 :color "#ff33d2") :foreground "#ffffff" :background "#5a015f"))
      (xml-attribute '(:foreground "#119cd0"))
      (xml-tag '(:foreground "#56e46f"))
      (highlight-current-tag '(:background "#3a352a")) ; #342b32 or #0614df
      )

  (custom-theme-set-faces
   'leuven-dark
   `(default ((,class (:foreground "#cfccd2" :background "#25202a"))))
   `(bold ((,class (:weight bold :foreground "#ffffff"))))
   `(bold-italic ((,class (:weight bold :slant italic :foreground "#ffffff"))))
   `(italic ((,class (:slant italic :foreground "#e8e5eb"))))
   `(underline ((,class (:underline t))))
   `(cursor ((,class (:background "#e1420b"))))

   ;; Lucid toolkit emacs menus.
   `(menu ((,class (:foreground "#25202a" :background "#cfccd2"))))

   ;; Highlighting faces.
   `(fringe ((,class (:foreground "#b76130" :background "#25202a"))))
   `(highlight ((,class ,highlight-blue)))
   `(region ((,class ,region)))
   `(secondary-selection ((,class ,match))) ; Used by Org-mode for highlighting matched entries and keywords.
   `(isearch ((,class (:underline "#ffffff" :foreground "#25202a" :background "#aa8b5e"))))
   `(isearch-fail ((,class (:weight bold :foreground "#ffffff" :background "#06333d"))))
   `(lazy-highlight ((,class (:foreground "#ffffff" :background "#0601ff")))) ; Isearch others (see `match').
   `(trailing-whitespace ((,class ,trailing)))
   `(query-replace ((,class (:inherit isearch))))
   `(whitespace-hspace ((,class (:foreground "#322d37")))) ; see also `nobreak-space'
   `(whitespace-indentation ((,class ,tab)))
   `(whitespace-line ((,class (:foreground "#38ffff" :background "#06017f"))))
   `(whitespace-tab ((,class ,tab)))
   `(whitespace-trailing ((,class ,trailing)))

   ;; Mode line faces.
   `(mode-line ((,class (:box (:line-width 1 :color "#e8d0b3") :foreground "#7e311e" :background "#cfa161"))))
   `(mode-line-inactive ((,class (:box (:line-width 1 :color "#b5b1bb") :foreground "#322d38" :background "#696371"))))
   `(mode-line-buffer-id ((,class (:weight bold :foreground "#25202a"))))
   `(mode-line-emphasis ((,class (:weight bold :foreground "#25202a"))))
   `(mode-line-highlight ((,class (:foreground "#0601ff"))))

   ;; Escape and prompt faces.
   `(minibuffer-prompt ((,class (:weight bold :foreground "#ffffff" :background "#0628ff"))))
   `(minibuffer-noticeable-prompt ((,class (:weight bold :foreground "#ffffff" :background "#0628ff"))))
   `(escape-glyph ((,class (:foreground "#ff7138"))))
   `(error ((,class (:foreground "#06ffff"))))
   `(warning ((,class (:weight bold :foreground "#065aff"))))
   `(success ((,class (:foreground "#ff01ff"))))

   ;; Font lock faces.
   `(font-lock-builtin-face ((,class (:foreground "#ff9029"))))
   `(font-lock-comment-delimiter-face ((,class (:foreground "#767283")))) ; #9a969e
   `(font-lock-comment-face ((,class (:slant italic :foreground "#767283")))) ; #9a969e
   `(font-lock-constant-face ((,class (:foreground "#34c8d8"))))
   `(font-lock-doc-face ((,class (:foreground "#fd95fa"))))
   ;; `(font-lock-doc-string-face ((,class (:foreground "#ff7fff")))) ; XEmacs only, but is used for HTML exports from org2html (and not interactively)
   `(font-lock-function-name-face ((,class (:weight normal :foreground "#ff996f"))))
   `(font-lock-keyword-face ((,class (:bold nil :foreground "#ffff0b")))) ; #ccab2d
   `(font-lock-preprocessor-face ((,class (:foreground "#837f87"))))
   `(font-lock-regexp-grouping-backslash ((,class (:weight bold :inherit nil))))
   `(font-lock-regexp-grouping-construct ((,class (:weight bold :inherit nil))))
   `(font-lock-string-face ((,class ,string)))
   `(font-lock-type-face ((,class (:weight normal :foreground "#9fcb66"))))
   `(font-lock-variable-name-face ((,class (:weight normal :foreground "#4ac964")))) ; #83ff87
   `(font-lock-warning-face ((,class (:weight bold :foreground "#06ffff"))))

   ;; Button and link faces.
   `(link ((,class ,link)))
   `(link-visited ((,class (:underline t :foreground "#1f879a"))))
   `(button ((,class (:underline t :foreground "#ff925a"))))
   `(header-line ((,class (:box (:line-width 1 :color "#ffffff") :foreground "#ffffff" :background "#322d37"))))

   ;; Gnus faces.
   `(gnus-button ((,class (:weight normal))))
   `(gnus-cite-attribution-face ((,class (:foreground "#b3af59"))))
   `(gnus-cite-1 ((,class (:foreground "#b3af59" :background "#2d2832"))))
   `(gnus-cite-2 ((,class (:foreground "#9dffa1" :background "#2d2832"))))
   `(gnus-cite-3 ((,class (:foreground "#ff8890" :background "#2d2832"))))
   `(gnus-cite-4 ((,class (:foreground "#6bffff" :background "#2d2832"))))
   `(gnus-cite-5 ((,class (:foreground "#ffff6f" :background "#2d2832"))))
   `(gnus-cite-6 ((,class (:foreground "#4999ff" :background "#2d2832"))))
   `(gnus-cite-7 ((,class (:foreground "#b3af59" :background "#2d2832"))))
   `(gnus-cite-8 ((,class (:foreground "#9dffa1" :background "#2d2832"))))
   `(gnus-cite-9 ((,class (:foreground "#ff8890" :background "#2d2832"))))
   `(gnus-cite-10 ((,class (:foreground "#6bffff" :background "#2d2832"))))
   `(gnus-emphasis-bold ((,class (:weight bold))))
   `(gnus-emphasis-highlight-words ((,class (:foreground "#0601ff" :background "#ffffff"))))
   `(gnus-group-mail-1 ((,class (:weight bold :foreground "#06af59"))))
   `(gnus-group-mail-1-empty ((,class (:foreground "#b3af59"))))
   `(gnus-group-mail-2 ((,class (:weight bold :foreground "#06ffa1"))))
   `(gnus-group-mail-2-empty ((,class (:foreground "#9dffa1"))))
   `(gnus-group-mail-3 ((,class ,mail-unread)))
   `(gnus-group-mail-3-empty ((,class ,mail-read)))
   `(gnus-group-mail-low ((,class ,cancel)))
   `(gnus-group-mail-low-empty ((,class ,cancel)))
   `(gnus-group-news-1 ((,class (:weight bold :foreground "#06af59"))))
   `(gnus-group-news-1-empty ((,class (:foreground "#b3af59"))))
   `(gnus-group-news-2 ((,class (:weight bold :foreground "#06ffa1"))))
   `(gnus-group-news-2-empty ((,class (:foreground "#9dffa1"))))
   `(gnus-group-news-3 ((,class ,mail-unread)))
   `(gnus-group-news-3-empty ((,class ,mail-read)))
   `(gnus-group-news-4 ((,class (:weight bold :foreground "#06ffff"))))
   `(gnus-group-news-4-empty ((,class (:foreground "#6bffff"))))
   `(gnus-group-news-5 ((,class (:weight bold :foreground "#06ff6f"))))
   `(gnus-group-news-5-empty ((,class (:foreground "#ffff6f"))))
   `(gnus-group-news-6 ((,class (:weight bold :foreground "#848088"))))
   `(gnus-group-news-6-empty ((,class (:foreground "#837f87"))))
   `(gnus-header-content ((,class ,mail-header-other)))
   `(gnus-header-from ((,class (:family "Sans Serif" :foreground "#ffffff"))))
   `(gnus-header-name ((,class ,mail-header-name)))
   `(gnus-header-newsgroups ((,class (:family "Sans Serif" :foreground "#cf663d"))))
   `(gnus-header-subject ((,class ,subject)))
   `(gnus-picon ((,class (:foreground "#0601ff" :background "#25202a"))))
   `(gnus-picon-xbm ((,class (:foreground "#0601ff" :background "#25202a"))))
   `(gnus-server-closed ((,class (:slant italic :foreground "#ffff0b" :background "#25202a"))))
   `(gnus-server-denied ((,class (:weight bold :foreground "#06ffff" :background "#25202a"))))
   `(gnus-server-opened ((,class (:family "Sans Serif" :foreground "#bd9432"))))
   `(gnus-signature ((,class (:slant italic :foreground "#787279"))))
   `(gnus-splash ((,class (:foreground "#0673ff"))))
   `(gnus-summary-cancelled ((,class ,cancel)))
   `(gnus-summary-high-ancient ((,class ,mail-unread-high)))
   `(gnus-summary-high-read ((,class ,mail-read-high)))
   `(gnus-summary-high-ticked ((,class ,mail-ticked)))
   `(gnus-summary-high-unread ((,class ,mail-unread-high)))
   `(gnus-summary-low-ancient ((,class (:slant italic :foreground "#ffffff"))))
   `(gnus-summary-low-read ((,class (:slant italic :foreground "#6b666f" :background "#413c46"))))
   `(gnus-summary-low-ticked ((,class ,mail-ticked)))
   `(gnus-summary-low-unread ((,class (:slant italic :foreground "#ffffff"))))
   `(gnus-summary-normal-ancient ((,class ,mail-read)))
   `(gnus-summary-normal-read ((,class ,mail-read)))
   `(gnus-summary-normal-ticked ((,class ,mail-ticked)))
   `(gnus-summary-normal-unread ((,class ,mail-unread)))
   `(gnus-summary-selected ((,class (:foreground "#25202a" :background "#ff7332"))))
   `(gnus-x-face ((,class (:foreground "#ffffff" :background "#25202a"))))

   ;; Message faces.
   `(message-header-name ((,class ,mail-header-name)))
   `(message-header-cc ((,class ,mail-to)))
   `(message-header-other ((,class ,mail-header-other)))
   `(message-header-subject ((,class ,subject)))
   `(message-header-to ((,class ,mail-to)))
   `(message-cited-text ((,class (:foreground "#b3af59" :background "#2d2832"))))
   `(message-separator ((,class (:family "Sans Serif" :weight normal :foreground "#473d43"))))
   `(message-header-newsgroups ((,class (:family "Sans Serif" :foreground "#cf663d"))))
   `(message-header-xheader ((,class ,mail-header-other)))
   `(message-mml ((,class (:foreground "#e074e3"))))

   ;; Diff.
   `(diff-added ((,class ,diff-added)))
   `(diff-changed ((,class ,diff-changed)))
   `(diff-context ((,class ,diff-none)))
   `(diff-file-header ((,class ,diff-header)))
   `(diff-file1-hunk-header ((,class (:foreground "#78ff7c" :background "#382c33"))))
   `(diff-file2-hunk-header ((,class (:foreground "#d781db" :background "#382c33"))))
   `(diff-function ((,class (:foreground "#38663d"))))
   `(diff-header ((,class ,diff-header)))
   `(diff-hunk-header ((,class ,diff-hunk-header)))
   `(diff-index ((,class ,diff-header)))
   `(diff-indicator-added ((,class (:foreground "#c966cc" :background "#53204e"))))
   `(diff-indicator-changed ((,class (:background "#46302a"))))
   `(diff-indicator-removed ((,class (:foreground "#38ccd2" :background "#254046"))))
   `(diff-refine-added ((,class ,diff-refine-added)))
   `(diff-refine-change ((,class (:background "#443f2a"))))
   `(diff-refine-removed ((,class ,diff-refine-removed)))
   `(diff-removed ((,class ,diff-removed)))

   ;; SMerge.
   `(smerge-mine ((,class ,diff-changed)))
   `(smerge-other ((,class ,diff-added)))
   `(smerge-base ((,class ,diff-removed)))
   `(smerge-markers ((,class (:background "#253859"))))
   `(smerge-refined-changed ((,class (:background "#5a550b"))))

   ;; Ediff.
   `(ediff-current-diff-A ((,class (:background "#253f49"))))
   `(ediff-current-diff-B ((,class (:background "#442049"))))
   `(ediff-current-diff-C ((,class (:background "#ff010b"))))
   `(ediff-even-diff-A ((,class (:background "#312c36"))))
   `(ediff-even-diff-B ((,class (:background "#312c36"))))
   `(ediff-fine-diff-A ((,class (:background "#06555f"))))
   `(ediff-fine-diff-B ((,class (:background "#ae01b2"))))
   `(ediff-odd-diff-A ((,class (:background "#312c36"))))
   `(ediff-odd-diff-B ((,class (:background "#312c36"))))

   ;; Flyspell.
   (if (version< emacs-version "24.4")
       `(flyspell-duplicate ((,class (:underline "#101487" :inherit nil))))
     `(flyspell-duplicate ((,class (:underline (:style wave :color "#101487") :background "#292759" :inherit nil)))))
   (if (version< emacs-version "24.4")
       `(flyspell-incorrect ((,class (:underline "#0a5864" :inherit nil))))
     `(flyspell-incorrect ((,class (:underline (:style wave :color "#0a5864") :background "#2f454c":inherit nil)))))

   ;; ;; Semantic faces.
   ;; `(semantic-decoration-on-includes ((,class (:underline ,cham-4))))
   ;; `(semantic-decoration-on-private-members-face ((,class (:background ,alum-2))))
   ;; `(semantic-decoration-on-protected-members-face ((,class (:background ,alum-2))))
   `(semantic-decoration-on-unknown-includes ((,class (:background "#252630"))))
   ;; `(semantic-decoration-on-unparsed-includes ((,class (:underline ,orange-3))))
   `(semantic-highlight-func-current-tag-face ((,class ,highlight-current-tag)))
   `(semantic-tag-boundary-face ((,class (:overline "#8c8890")))) ; Method separator.
   ;; `(semantic-unmatched-syntax-face ((,class (:underline ,red-1))))

   `(Info-title-1-face ((,class ,ol1)))
   `(Info-title-2-face ((,class ,ol2)))
   `(Info-title-3-face ((,class ,ol3)))
   `(Info-title-4-face ((,class ,ol4)))
   `(ace-jump-face-foreground ((,class (:weight bold :foreground "#ffffff" :background "#065aff"))))
   `(ahs-face ((,class (:background "#3e392a"))))
   `(ahs-definition-face ((,class (:background "#064943"))))
   `(ahs-plugin-defalt-face ((,class (:background "#25392a")))) ; Current.
   `(anzu-match-1 ((,class (:foreground "#ffffff" :background "#840135"))))
   `(anzu-match-2 ((,class (:foreground "#ffffff" :background "springgreen"))))
   `(anzu-match-3 ((,class (:foreground "#ffffff" :background "#06ffff"))))
   `(anzu-mode-line ((,class (:foreground "#ffffff" :background "#830187"))))
   `(anzu-mode-line-no-match ((,class (:foreground "#ffffff" :background "#067f87"))))
   `(anzu-replace-highlight ((,class (:inherit query-replace))))
   `(anzu-replace-to ((,class (:weight bold :foreground "#47cc0c" :background "#0742d2"))))
   `(auto-dim-other-buffers-face ((,class (:background "#2c2731"))))
   `(avy-background-face ((,class (:background "#5b5660"))))
   `(avy-lead-face ((,class (:weight bold :foreground "#ffffff" :background "#065aff"))))
   `(bbdb-company ((,class (:slant italic :foreground "#bd7d55"))))
   `(bbdb-field-name ((,class (:weight bold :foreground "#bd7d55"))))
   `(bbdb-field-value ((,class (:foreground "#bd7d55"))))
   `(bbdb-name ((,class (:underline t :foreground "#0699d2"))))
   `(bmkp-light-autonamed ((,class (:background "#322d37"))))
   `(bmkp-light-fringe-autonamed ((,class (:foreground "#a9a5ad" :background "#302b35"))))
   `(bmkp-light-fringe-non-autonamed ((,class (:foreground "#252059" :background "#fe010e")))) ; default
   `(bmkp-light-non-autonamed ((,class (:background "#60202a"))))
   `(bmkp-no-local ((,class (:background "#063f3e"))))
   `(browse-kill-ring-separator-face ((,class (:foreground "#06ffff"))))
   `(calendar-month-header ((,class (:weight bold :foreground "#b4b5ca" :background "#252059"))))
   `(calendar-today ((,class (:weight bold :foreground "#b4b5ca" :background "#252059"))))
   `(calendar-weekday-header ((,class (:weight bold :foreground "#ec9d5a"))))
   `(calendar-weekend-header ((,class (:weight bold :foreground "#b5b1b9"))))
   `(cfw:face-annotation ((,class (:foreground "#ff01ff" :background "#06ffff"))))
   `(cfw:face-day-title ((,class (:foreground "#3b3640"))))
   `(cfw:face-default-content ((,class (:foreground "#d9ad66"))))
   `(cfw:face-default-day ((,class (:weight bold))))
   `(cfw:face-disable ((,class (:foreground "#5b5660"))))
   `(cfw:face-grid ((,class (:foreground "#27222c"))))
   `(cfw:face-header ((,class (:foreground "#ec9d5a" :background "#25202a" :weight bold))))
   `(cfw:face-holiday ((,class (:foreground "#8c8890" :background "#3e322a"))))
   `(cfw:face-periods ((,class (:foreground "#25202a" :background "#9d7330" :slant italic))))
   `(cfw:face-saturday ((,class (:foreground "#b5b1b9" :background "#25202a" :weight bold))))
   `(cfw:face-select ((,class (:foreground "#b96a1e" :background "#352d2e"))))
   `(cfw:face-sunday ((,class (:foreground "#b5b1b9" :background "#25202a" :weight bold))))
   `(cfw:face-title ((,class (:height 2.0 :foreground "#9c98a0" :weight bold :inherit variable-pitch))))
   `(cfw:face-today ((,class (:foreground "#b4b5ca" :background "#252059"))))
   `(cfw:face-today-title ((,class (:foreground "#25202a" :background "#eb9958"))))
   `(cfw:face-toolbar ((,class (:background "#25202a"))))
   `(cfw:face-toolbar-button-off ((,class (:foreground "#35303a" :background "#25202a"))))
   `(cfw:face-toolbar-button-on ((,class (:foreground "#a5a1a9" :background "#2d2832"))))
   `(change-log-date ((,class (:foreground "#64df19"))))
   `(change-log-file ((,class (:weight bold :foreground "#c27c45"))))
   `(change-log-list ((,class (:foreground "#ffffff" :background "#8e1142"))))
   `(change-log-name ((,class (:foreground "#ff7fff"))))
   `(circe-highlight-all-nicks-face ((,class (:foreground "#ffff0b" :background "#322d37")))) ; other nick names
   `(circe-highlight-nick-face ((,class (:foreground "#ff6cff" :background "#322d37")))) ; messages with my nick cited
   `(circe-my-message-face ((,class (:foreground "#78747c" :background "#322d37"))))
   `(circe-originator-face ((,class (:foreground "#ffff0b"))))
   `(circe-prompt-face ((,class (:foreground "#06ffff"))))
   `(circe-server-face ((,class (:foreground "#6b3524"))))
   `(comint-highlight-input ((,class (:weight bold :foreground "#ffff0b" :inherit nil))))
   ;; `(comint-highlight-prompt ((,class (:weight bold :foreground "#ffffff" :background "#0628ff"))))
   `(comint-highlight-prompt ((,class (:weight bold :foreground "#ffff0b" :inherit nil))))

   ;; `(ac-selection-face ((,class ,completion-selected-candidate)))
   `(ac-selection-face ((,class (:weight bold :foreground "#25202a" :background "#065aff")))) ; TEMP For diff'ing AC from Comp.
   `(ac-candidate-face ((,class ,completion-other-candidates)))
   `(ac-completion-face ((,class ,completion-inline)))
   `(ac-candidate-mouse-face ((,class (:inherit highlight))))
   `(popup-scroll-bar-background-face ((,class (:background "#372a2a"))))
   `(popup-scroll-bar-foreground-face ((,class (:background "#332525")))) ; Scrollbar (visible).

   ;; Company.
   `(company-tooltip-common-selection ((,class (:weight normal :foreground "#2a3159" :inherit company-tooltip-selection)))) ; Prefix + common part in tooltip (for selection).
   `(company-tooltip-selection ((,class ,completion-selected-candidate))) ; Suffix in tooltip (for selection).
   `(company-tooltip-annotation-selection ((,class (:weight normal :foreground "#2a3159")))) ; Annotation (for selection).

   `(company-tooltip-common ((,class (:weight normal :foreground "#54ff59" :inherit company-tooltip)))) ; Prefix + common part in tooltip.
   `(company-tooltip ((,class ,completion-other-candidates))) ; Suffix in tooltip.
   `(company-tooltip-annotation ((,class (:weight normal :foreground "#deea0b")))) ; Annotation.
   `(company-preview ((,class ,completion-inline)))
   `(company-preview-common ((,class ,completion-inline)))
   `(company-scrollbar-bg ((,class (:background "#372a2a"))))
   `(company-scrollbar-fg ((,class (:background "#332525")))) ; Scrollbar (visible).

   `(compare-windows ((,class (:background "#0601ff"))))
   ;; `(completions-common-part ((,class (:foreground "#06ffff" :weight bold))))
   ;; `(completions-first-difference ((,class (:foreground "#ff01ff" :weight bold))))
   `(compilation-error ((,class (:weight bold :foreground "#06ffff")))) ; Used for grep error messages.
   `(compilation-info ((,class ,grep-file-name)))
   `(compilation-line-number ((,class ,grep-line-number)))
   `(compilation-warning ((,class (:weight bold :foreground "#065aff"))))
   `(compilation-mode-line-exit ((,class (:weight bold :foreground "#ff01ff")))) ; :exit[matched]
   `(compilation-mode-line-fail ((,class (:weight bold :foreground "#167d1b")))) ; :exit[no match]
   `(compilation-mode-line-run ((,class (:weight bold :foreground "#065aff")))) ; :run
   `(css-property ((,class (:foreground "#ff55ff"))))
   `(css-selector ((,class (:weight bold :foreground "#ffff0b"))))
   `(custom-button ((,class (:box (:line-width 2 :style released-button) :foreground "#ffffff" :background "lightgrey"))))
   `(custom-button-mouse ((,class (:box (:line-width 2 :style released-button) :foreground "#ffffff" :background "#3d3842"))))
   `(custom-button-pressed ((,class (:box (:line-width 2 :style pressed-button) :foreground "#ffffff" :background "#312c36"))))
   `(custom-button-pressed-unraised ((,class (:underline t :foreground "#78ff7c"))))
   `(custom-button-unraised ((,class (:underline t))))
   `(custom-changed ((,class (:foreground "#25202a" :background "#ffff0b"))))
   `(custom-comment ((,class (:background "#2b2630"))))
   `(custom-comment-tag ((,class (:foreground "#ffff7c"))))
   `(custom-documentation ((,class (nil))))
   `(custom-face-tag ((,class (:family "Sans Serif" :height 1.2 :weight bold))))
   `(custom-group-tag ((,class (:height 1.2 :weight bold :foreground "#ffff0b"))))
   `(custom-group-tag-1 ((,class (:family "Sans Serif" :height 1.2 :weight bold :foreground "#06ffff"))))
   `(custom-invalid ((,class (:foreground "#0601ff" :background "#06ffff"))))
   `(custom-link ((,class (:underline t :foreground "#ffff0b"))))
   `(custom-modified ((,class (:foreground "#25202a" :background "#ffff0b"))))
   `(custom-rogue ((,class (:foreground "#063f3e" :background "#ffffff"))))
   `(custom-saved ((,class (:underline t))))
   `(custom-set ((,class (:foreground "#ffff0b" :background "#25202a"))))
   `(custom-state ((,class (:foreground "#ff74ff"))))
   `(custom-themed ((,class (:foreground "#25202a" :background "#ffff0b"))))
   `(custom-variable-button ((,class (:weight bold :underline t))))
   `(custom-variable-tag ((,class (:family "Sans Serif" :height 1.2 :weight bold :foreground "#ffff0b"))))
   `(custom-visibility ((,class ,link)))
   `(diff-hl-change ((,class (:foreground "#ffff3c" :background "#46302a"))))
   `(diff-hl-delete ((,class (:foreground "#37ffff" :background "#254046"))))
   `(diff-hl-dired-change ((,class (:weight bold :foreground "#ffffff" :background "#065cd0"))))
   `(diff-hl-dired-delete ((,class (:weight bold :foreground "#2dc6ef"))))
   `(diff-hl-dired-ignored ((,class (:weight bold :foreground "#25202a" :background "#44445e"))))
   `(diff-hl-dired-insert ((,class (:weight bold :foreground "#4b464f"))))
   `(diff-hl-dired-unknown ((,class (:foreground "#25202a" :background "#c4c455"))))
   `(diff-hl-insert ((,class (:foreground "#ff74ff" :background "#53204e"))))
   `(diff-hl-unknown ((,class (:foreground "#25202a" :background "#c4c455"))))
   `(diary-face ((,class (:foreground "#7c360d"))))
   `(dircolors-face-asm ((,class (:foreground "#ffffff"))))
   `(dircolors-face-backup ((,class (:foreground "#ffffff"))))
   `(dircolors-face-compress ((,class (:foreground "#06ffff"))))
   `(dircolors-face-dir ((,class ,directory)))
   `(dircolors-face-doc ((,class (:foreground "#ffffff"))))
   `(dircolors-face-dos ((,class (:foreground "#e074e3"))))
   `(dircolors-face-emacs ((,class (:foreground "#ffffff"))))
   `(dircolors-face-exec ((,class (:foreground "#e074e3"))))
   `(dircolors-face-html ((,class (:foreground "#ffffff"))))
   `(dircolors-face-img ((,class (:foreground "#37ff3c"))))
   `(dircolors-face-lang ((,class (:foreground "#ffffff"))))
   `(dircolors-face-lang-interface ((,class (:foreground "#ffffff"))))
   `(dircolors-face-make ((,class (:foreground "#ffffff"))))
   `(dircolors-face-objet ((,class (:foreground "#ffffff"))))
   `(dircolors-face-package ((,class (:foreground "#ffffff"))))
   `(dircolors-face-paddb ((,class (:foreground "#ffffff"))))
   `(dircolors-face-ps ((,class (:foreground "#ffffff"))))
   `(dircolors-face-sound ((,class (:foreground "#ff400b"))))
   `(dircolors-face-tar ((,class (:foreground "#06ffff"))))
   `(dircolors-face-text ((,class (:foreground "#ffffff"))))
   `(dircolors-face-yacc ((,class (:foreground "#ffffff"))))
   `(dired-directory ((,class ,directory)))
   `(dired-header ((,class ,directory)))
   `(dired-ignored ((,class (:strike-through t :foreground "#06ffff"))))
   `(dired-mark ((,class ,marked-line)))
   `(dired-marked ((,class ,marked-line)))
   `(dired-symlink ((,class ,symlink)))
   `(diredfl-compressed-file-suffix ((,class (:foreground "#ffffff" :background "#2526c0"))))
   `(diredp-compressed-file-suffix ((,class (:foreground "#06ffff"))))
   `(diredp-date-time ((,class (:foreground "#64df19"))))
   `(diredp-dir-heading ((,class ,directory)))
   `(diredp-dir-name ((,class ,directory)))
   `(diredp-dir-priv ((,class ,directory)))
   `(diredp-exec-priv ((,class (:background "#fd3fcb"))))
   `(diredp-executable-tag ((,class (:foreground "#e074e3" :background "#25202a"))))
   `(diredp-file-name ((,class ,file)))
   `(diredp-file-suffix ((,class (:foreground "#443f49"))))
   `(diredp-flag-mark-line ((,class ,marked-line)))
   `(diredp-ignored-file-name ((,class ,shadow)))
   `(diredp-read-priv ((,class (:background "#f7660b"))))
   `(diredp-write-priv ((,class (:foreground "#25202a" :background "#06bfc7"))))
   `(doom-modeline-panel ((,class (:foreground "#ffffff" :background "#2526c0"))))
   `(eldoc-highlight-function-argument ((,class (:weight bold :foreground "#06ffff" :background "#25392a"))))
   `(elfeed-search-filter-face ((,class (:foreground "#46414b"))))
   ;; `(eww-form-checkbox ((,class ())))
   ;; `(eww-form-select ((,class ())))
   ;; `(eww-form-submit ((,class ())))
   `(eww-form-text ((,class (:weight bold :foreground "#c3a798" :background "#5d3218"))))
   ;; `(eww-form-textarea ((,class ())))
   `(file-name-shadow ((,class ,shadow)))
   `(flycheck-error ((,class (:underline (:color "#06dae7" :style wave) :weight bold :background "#253c46"))))
   `(flycheck-error-list-line-number ((,class (:foreground "#5fca5b"))))
   `(flycheck-fringe-error ((,class (:foreground "#06dae7"))))
   `(flycheck-fringe-info ((,class (:foreground "#ed75ef"))))
   `(flycheck-fringe-warning ((,class (:foreground "#1056cd"))))
   `(flycheck-info ((,class (:underline (:color "#ed75ef" :style wave) :weight bold))))
   `(flycheck-warning ((,class (:underline (:color "#1056cd" :style wave) :weight bold :background "#252066"))))
   `(font-latex-bold-face ((,class (:weight bold :foreground "#ffffff"))))
   `(fancy-narrow-blocked-face ((,class (:foreground "#6b6765"))))
   `(flycheck-color-mode-line-error-face ((, class (:background "#35a4b1"))))
   `(flycheck-color-mode-line-warning-face ((, class (:background "#1938ff"))))
   `(flycheck-color-mode-line-info-face ((, class (:background "#0601ff"))))
   `(font-latex-italic-face ((,class (:slant italic :foreground "#e8e5eb"))))
   `(font-latex-math-face ((,class (:foreground "#ffff0b"))))
   `(font-latex-sectioning-1-face ((,class (:family "Sans Serif" :height 2.7 :weight bold :foreground "#9f6a1c"))))
   `(font-latex-sectioning-2-face ((,class ,ol1)))
   `(font-latex-sectioning-3-face ((,class ,ol2)))
   `(font-latex-sectioning-4-face ((,class ,ol3)))
   `(font-latex-sectioning-5-face ((,class ,ol4)))
   `(font-latex-sedate-face ((,class (:foreground "#06aaff"))))
   `(font-latex-string-face ((,class (:weight bold :foreground "#ff990b"))))
   `(font-latex-verbatim-face ((,class (:foreground "#ffff7f" :background "#252046" :inherit nil))))
   `(git-commit-summary-face ((,class (:foreground "#ffffff"))))
   `(git-commit-comment-face ((,class (:slant italic :foreground "#9a969e"))))
   `(git-timemachine-commit ((,class ,diff-removed)))
   `(git-timemachine-minibuffer-author-face ((,class ,diff-added)))
   `(git-timemachine-minibuffer-detail-face ((,class ,diff-header)))
   `(google-translate-text-face ((,class (:foreground "#8c8890" :background "#2e2933"))))
   `(google-translate-phonetic-face ((,class (:inherit shadow))))
   `(google-translate-translation-face ((,class (:weight normal :foreground "#d2861c" :background "#3f3336"))))
   `(google-translate-suggestion-label-face ((,class (:foreground "#06ffff"))))
   `(google-translate-suggestion-face ((,class (:slant italic :underline t))))
   `(google-translate-listen-button-face ((,class (:height 0.8))))
   `(helm-action ((,class (:foreground "#ffffff"))))
   `(helm-bookmark-file ((,class ,file)))
   `(helm-bookmarks-su-face ((,class (:foreground "#06ffff"))))
   `(helm-buffer-directory ((,class ,directory)))
   ;; `(helm-non-file-buffer ((,class (:slant italic :foreground "#ffff0b"))))
   ;; `(helm-buffer-file ((,class (:foreground "#cfccd2"))))
   `(helm-buffer-modified ((,class (:slant italic :foreground "#4ac964"))))
   `(helm-buffer-process ((,class (:foreground "#ff7dff"))))
   `(helm-candidate-number ((,class (:foreground "#ffffff" :background "#0601a1"))))
   `(helm-dir-heading ((,class (:foreground "#ffff0b" :background "#063f3e"))))
   `(helm-dir-priv ((,class (:foreground "#78ffff" :background "#312c36"))))
   `(helm-ff-directory ((,class ,directory)))
   `(helm-ff-dotted-directory ((,class ,directory)))
   `(helm-ff-executable ((,class (:foreground "#ff32ff" :background "#25202a"))))
   `(helm-ff-file ((,class (:foreground "#ffffff"))))
   `(helm-ff-invalid-symlink ((,class (:foreground "#0601ff" :background "#06ffff"))))
   `(helm-ff-symlink ((,class ,symlink)))
   `(helm-file-name ((,class (:foreground "#ffff0b"))))
   `(helm-gentoo-match-face ((,class (:foreground "#06ffff"))))
   `(helm-grep-file ((,class ,grep-file-name)))
   `(helm-grep-lineno ((,class ,grep-line-number)))
   `(helm-grep-match ((,class ,match)))
   `(helm-grep-running ((,class (:weight bold :foreground "#25202a"))))
   `(helm-isearch-match ((,class (:background "#38013d"))))
   `(helm-lisp-show-completion ((,class ,volatile-highlight-supersize))) ; See `helm-dabbrev'.
   ;; `(helm-ls-git-added-copied-face ((,class (:foreground ""))))
   ;; `(helm-ls-git-added-modified-face ((,class (:foreground ""))))
   ;; `(helm-ls-git-conflict-face ((,class (:foreground ""))))
   ;; `(helm-ls-git-deleted-and-staged-face ((,class (:foreground ""))))
   ;; `(helm-ls-git-deleted-not-staged-face ((,class (:foreground ""))))
   ;; `(helm-ls-git-modified-and-staged-face ((,class (:foreground ""))))
   `(helm-ls-git-modified-not-staged-face ((,class (:foreground "#4ac964"))))
   ;; `(helm-ls-git-renamed-modified-face ((,class (:foreground ""))))
   ;; `(helm-ls-git-untracked-face ((,class (:foreground ""))))
   `(helm-match ((,class ,match)))
   `(helm-moccur-buffer ((,class (:foreground "#ff993d"))))
   `(helm-selection ((,class (:background "#cb8a33" :foreground "#25202a"))))
   `(helm-selection-line ((,class ,highlight-gray))) ; ???
   `(helm-separator ((,class (:foreground "#06ffff"))))
   `(helm-source-header ((,class (:weight bold :box (:line-width 1 :color "#3d3842") :background "#433e48" :foreground "#ffffff"))))
   `(helm-swoop-target-line-block-face ((,class (:background "#3833ff" :foreground "#e0dde3"))))
   `(helm-swoop-target-line-face ((,class (:background "#38330b"))))
   `(helm-swoop-target-word-face ((,class (:weight bold :foreground unspecified :background "#0742d2"))))
   `(helm-visible-mark ((,class ,marked-line)))
   `(helm-w3m-bookmarks-face ((,class (:underline t :foreground "#ff010b"))))
   `(highlight-changes ((,class (:foreground unspecified)))) ;; blue "#d4f754"
   `(highlight-changes-delete ((,class (:strike-through nil :foreground unspecified)))) ;; red "#4ff7d7"
   `(highlight-symbol-face ((,class (:background "#252080"))))
   `(hl-line ((,class ,highlight-yellow))) ; Highlight current line.
   `(hl-tags-face ((,class ,highlight-current-tag))) ; ~ Pair highlighting (matching tags).
   `(holiday-face ((,class (:foreground "#8c8890" :background "#3e322a"))))
   `(html-helper-bold-face ((,class (:weight bold :foreground "#ffffff"))))
   `(html-helper-italic-face ((,class (:slant italic :foreground "#ffffff"))))
   `(html-helper-underline-face ((,class (:underline t :foreground "#ffffff"))))
   `(html-tag-face ((,class (:foreground "#ffff0b"))))
   `(ilog-non-change-face ((,class (:height 2.0 :foreground "#9fcb66"))))
   `(ilog-change-face ((,class (:height 2.0 :foreground "#ff7dff"))))
   `(ilog-echo-face ((,class (:height 2.0 :foreground "#ff9029"))))
   `(ilog-load-face ((,class (:foreground "#4ac964"))))
   `(ilog-message-face ((,class (:foreground "#837f87"))))
   `(indent-guide-face ((,class (:foreground "#312c36"))))
   `(info-file ((,class (:family "Sans Serif" :height 1.8 :weight bold :box (:line-width 1 :color "#ffff3d") :foreground "#9f6a1c" :background "#563c2a"))))
   `(info-header-node ((,class (:underline t :foreground "#065aff")))) ; nodes in header
   `(info-header-xref ((,class (:underline t :foreground "#e46f0b")))) ; cross references in header
   `(info-index-match ((,class (:weight bold :foreground unspecified :background "#0742d2")))) ; when using `i'
   `(info-menu-header ((,class ,ol2))) ; menu titles (headers) -- major topics
   `(info-menu-star ((,class (:foreground "#ffffff")))) ; every 3rd menu item
   `(info-node ((,class (:underline t :foreground "#ffff0b")))) ; node names
   `(info-quoted-name ((,class ,code-inline)))
   `(info-string ((,class ,string)))
   `(info-title-1 ((,class ,ol1)))
   `(info-xref ((,class (:underline t :foreground "#ff925a")))) ; unvisited cross-references
   `(info-xref-visited ((,class (:underline t :foreground "#78ff7c")))) ; previously visited cross-references
   ;; js2-highlight-vars-face (~ auto-highlight-symbol)
   `(js2-error ((,class (:box (:line-width 1 :color "#06c8cf") :background "#063741")))) ; DONE.
   `(js2-external-variable ((,class (:foreground "#06ffff" :background "#252630")))) ; DONE.
   `(js2-function-param ((,class ,function-param)))
   `(js2-instance-member ((,class (:foreground "#6bcd3d"))))
   `(js2-jsdoc-html-tag-delimiter ((,class (:foreground "#34c8d8"))))
   `(js2-jsdoc-html-tag-name ((,class (:foreground "#34c8d8"))))
   `(js2-jsdoc-tag ((,class (:weight normal :foreground "#9fcb66"))))
   `(js2-jsdoc-type ((,class (:foreground "#bd7d55"))))
   `(js2-jsdoc-value ((,class (:weight normal :foreground "#4ac964")))) ; #83ff87
   `(js2-magic-paren ((,class (:underline t))))
   `(js2-private-function-call ((,class (:foreground "#2a5ae5"))))
   `(js2-private-member ((,class (:foreground "#375073"))))
   `(js2-warning ((,class (:underline "#065aff"))))

   ;; Org non-standard faces.
   `(leuven-dark-org-deadline-overdue ((,class (:foreground "#12d9ae"))))
   `(leuven-dark-org-deadline-today ((,class (:weight bold :foreground "#b4b5ca" :background "#252059"))))
   `(leuven-dark-org-deadline-tomorrow ((,class (:foreground "#c357f8"))))
   `(leuven-dark-org-deadline-future ((,class (:foreground "#c357f8"))))
   `(leuven-dark-gnus-unseen ((,class (:weight bold :foreground "#088dfd"))))
   `(leuven-dark-gnus-date ((,class (:foreground "#067f4a"))))
   `(leuven-dark-gnus-size ((,class (:foreground "#7440a7"))))
   `(leuven-dark-todo-items-face ((,class (:weight bold :foreground "#06cee0" :background "#06017f"))))

   `(light-symbol-face ((,class (:background "#252080"))))
   `(linum ((,class (:foreground "#6a656f" :background "#35303a"))))
   `(log-view-file ((,class (:foreground "#ffff3d" :background "#382c33"))))
   `(log-view-message ((,class (:foreground "#ffffff" :background "#171593"))))
   `(lsp-modeline-code-actions-preferred-face ((,class (:foreground "#ffffff" :background "#2526c0"))))
   `(lsp-ui-doc-background ((,class (:background "#2d2058"))))
   `(lsp-ui-sideline-code-action ((,class (:foreground "#ffffff" :background "#2526c0"))))
   `(lui-button-face ((,class ,link)))
   `(lui-highlight-face ((,class (:box (:line-width 1 :color "#38ffff") :foreground "#38ffff" :background "#06017f")))) ; my nickname
   `(lui-time-stamp-face ((,class (:foreground "#64df19"))))
   `(magit-blame-header ((,class (:inherit magit-diff-file-header))))
   `(magit-blame-heading ((,class (:overline "#5d5862" :foreground "#06ffff" :background "#3c3741"))))
   `(magit-blame-hash ((,class (:overline "#5d5862" :foreground "#06ffff" :background "#3c3741"))))
   `(magit-blame-name ((,class (:overline "#5d5862" :foreground "#fd95fa" :background "#3c3741"))))
   `(magit-blame-date ((,class (:overline "#5d5862" :foreground "#ffff0b" :background "#3c3741"))))
   `(magit-blame-summary ((,class (:overline "#5d5862" :weight bold :foreground "#938f97" :background "#3c3741"))))
   `(magit-branch ((,class ,vc-branch)))
   `(magit-diff-add ((,class ,diff-added)))
   `(magit-diff-del ((,class ,diff-removed)))
   `(magit-diff-file-header ((,class (:height 1.1 :weight bold :foreground "#c27c45"))))
   `(magit-diff-hunk-header ((,class ,diff-hunk-header)))
   `(magit-diff-none ((,class ,diff-none)))
   `(magit-header ((,class (:foreground "#25202a" :background "#06bfc7"))))
   `(magit-item-highlight ((,class (:background "#382c33"))))
   `(magit-item-mark ((,class ,marked-line)))
   `(magit-log-head-label ((,class (:box (:line-width 1 :color "#ffff0b" :style nil)))))
   `(magit-log-tag-label ((,class (:box (:line-width 1 :color "#ff33ff" :style nil)))))
   `(magit-section-highlight ((,class (:background  "#2d2058"))))
   `(magit-section-title ((,class (:family "Sans Serif" :height 1.8 :weight bold :foreground "#9f6a1c" :inherit nil))))
   `(makefile-space-face ((,class (:background "#069655"))))
   `(makefile-targets ((,class (:weight bold :foreground "#ffff0b"))))
   ;; `(markdown-blockquote-face ((,class ())))
   `(markdown-bold-face ((,class (:inherit bold))))
   ;; `(markdown-comment-face ((,class ())))
   ;; `(markdown-footnote-face ((,class ())))
   ;; `(markdown-header-delimiter-face ((,class ())))
   ;; `(markdown-header-face ((,class ())))
   `(markdown-header-face-1 ((,class ,ol1)))
   `(markdown-header-face-2 ((,class ,ol2)))
   `(markdown-header-face-3 ((,class ,ol3)))
   `(markdown-header-face-4 ((,class ,ol4)))
   `(markdown-header-face-5 ((,class ,ol5)))
   `(markdown-header-face-6 ((,class ,ol6)))
   ;; `(markdown-header-rule-face ((,class ())))
   `(markdown-inline-code-face ((,class ,code-inline)))
   `(markdown-italic-face ((,class (:inherit italic))))
   `(markdown-language-keyword-face ((,class (:inherit org-block-begin-line))))
   ;; `(markdown-line-break-face ((,class ())))
   `(markdown-link-face ((,class ,link-no-underline)))
   ;; `(markdown-link-title-face ((,class ())))
   ;; `(markdown-list-face ((,class ())))
   ;; `(markdown-math-face ((,class ())))
   ;; `(markdown-metadata-key-face ((,class ())))
   ;; `(markdown-metadata-value-face ((,class ())))
   ;; `(markdown-missing-link-face ((,class ())))
   `(markdown-pre-face ((,class (:inherit org-block-background))))
   ;; `(markdown-reference-face ((,class ())))
   ;; `(markdown-strike-through-face ((,class ())))
   `(markdown-url-face ((,class ,link)))
   `(match ((,class ,match)))           ; Used for grep matches.
   `(mc/cursor-bar-face ((,class (:height 1.0 :foreground "#ec9b45" :background "#ec9b45"))))
   `(mc/cursor-face ((,class (:inverse-video t))))
   `(mc/region-face ((,class (:inherit region))))
   `(mm-uu-extract ((,class ,code-block)))
   `(moccur-current-line-face ((,class (:foreground "#ffffff" :background "#252059"))))
   `(moccur-face ((,class (:foreground "#ffffff" :background "#06016f"))))
   `(next-error ((,class ,volatile-highlight-supersize)))
   `(nobreak-space ((,class (:background "#543532"))))
   `(nxml-attribute-local-name-face ((,class ,xml-attribute)))
   `(nxml-attribute-value-delimiter-face ((,class (:foreground "#ff74ff"))))
   `(nxml-attribute-value-face ((,class (:foreground "#ff74ff"))))
   `(nxml-comment-content-face ((,class (:slant italic :foreground "#06ffff"))))
   `(nxml-comment-delimiter-face ((,class (:foreground "#06ffff"))))
   `(nxml-element-local-name ((,class ,xml-tag)))
   `(nxml-element-local-name-face ((,class (:foreground "#ffff0b"))))
   `(nxml-processing-instruction-target-face ((,class (:foreground "#69cf0b"))))
   `(nxml-tag-delimiter-face ((,class (:foreground "#ffff0b"))))
   `(nxml-tag-slash-face ((,class (:foreground "#ffff0b"))))
   `(org-agenda-block-count ((,class (:weight bold :foreground "#5f5a64"))))
   `(org-agenda-calendar-event ((,class (:weight bold :foreground "#cc8b3d" :background "#3e322a"))))
   `(org-agenda-calendar-sexp ((,class (:foreground "#d0853c" :background "#30272c"))))
   `(org-agenda-clocking ((,class (:foreground "#ffffff" :background "#1636ff"))))
   `(org-agenda-column-dateline ((,class ,column)))
   `(org-agenda-current-time ((,class (:underline t :foreground "#ec9d5a"))))
   `(org-agenda-date ((,class (,@(leuven-dark-scale-font leuven-dark-scale-org-agenda-structure 1.6) :weight bold :foreground "#ec9d5a"))))
   `(org-agenda-date-today ((,class (,@(leuven-dark-scale-font leuven-dark-scale-org-agenda-structure 1.6) :weight bold :foreground "#b4b5ca" :background "#252059"))))
   `(org-agenda-date-weekend ((,class (,@(leuven-dark-scale-font leuven-dark-scale-org-agenda-structure 1.6) :weight bold :foreground "#b5b1b9"))))
   `(org-agenda-diary ((,class (:weight bold :foreground "#ff74ff" :background "#572723"))))
   `(org-agenda-dimmed-todo-face ((,class (:foreground "#1636ff"))))
   `(org-agenda-done ((,class (:foreground "#aeaab2"))))
   `(org-agenda-filter-category ((,class (:weight bold :foreground "#065aff"))))
   `(org-agenda-filter-effort ((,class (:weight bold :foreground "#065aff"))))
   `(org-agenda-filter-regexp ((,class (:weight bold :foreground "#065aff"))))
   `(org-agenda-filter-tags ((,class (:weight bold :foreground "#065aff"))))
   `(org-agenda-restriction-lock ((,class (:background "#1d82a4"))))
   `(org-agenda-structure ((,class (,@(leuven-dark-scale-font leuven-dark-scale-org-agenda-structure 1.6) :weight bold :foreground "#e37233"))))
   `(org-archived ((,class (:foreground "#514c56"))))
   `(org-beamer-tag ((,class (:box (:line-width 1 :color "#0a43ed") :foreground "#d6d3d9" :background "#252655"))))
   `(org-block ((,class ,code-block)))
   `(org-block-background ((,class (:background "#252046")))) ;; :inherit fixed-pitch))))
   `(org-block-begin-line ((,class (:underline "#5d595f" :foreground "#aeaab2" :background "#221e34"))))
   `(org-block-end-line ((,class (:overline "#5d595f" :foreground "#aeaab2" :background "#221e34"))))
   `(org-checkbox ((,class (:weight bold :box (:line-width 1 :style pressed-button) :foreground "#efcab2" :background "#615c66"))))
   `(org-clock-overlay ((,class (:foreground "#25202a" :background "#b98f7c"))))
   `(org-code ((,class ,code-inline)))
   `(org-column ((,class ,column)))
   `(org-column-title ((,class ,column)))
   `(org-date ((,class (:underline t :foreground "#ffba6b"))))
   `(org-default ((,class (:foreground "#cfccd2" :background "#25202a"))))
   `(org-dim ((,class (:foreground "#5a555f"))))
   `(org-document-info ((,class (:foreground "#bbb7bf"))))
   `(org-document-info-keyword ((,class (:foreground "#ff7138" :background "#38332a"))))
   `(org-document-title ((,class (,@(leuven-dark-scale-font leuven-dark-scale-org-document-title 1.8)  :weight bold :foreground "#ffffff"))))
   `(org-done ((,class (:weight bold :box (:line-width 1 :color "#49444e") :foreground "#49444e" :background "#322d37"))))
   `(org-drawer ((,class (:weight bold :foreground "#ff44ff" :background "#38203d"))))
   `(org-ellipsis ((,class (:underline nil :foreground "#6b666f")))) ; #0611a5
   `(org-example ((,class (:foreground "#ffff0b" :background "#38203d"))))
   `(org-footnote ((,class (:underline t :foreground "#ff7138"))))
   `(org-formula ((,class (:foreground "#0680e1"))))
   ;; org-habit colors are thanks to zenburn
   `(org-habit-ready-face ((t :background "#7F9F7F"))) ; ,zenburn-green
   `(org-habit-alert-face ((t :background "#E0CF9F" :foreground "#3F3F3F"))) ; ,zenburn-yellow-1 fg ,zenburn-bg
   `(org-habit-clear-face ((t :background "#5C888B")))                       ; ,zenburn-blue-3
   `(org-habit-overdue-face ((t :background "#9C6363")))                     ; ,zenburn-red-3
   `(org-habit-clear-future-face ((t :background "#4C7073")))                ; ,zenburn-blue-4
   `(org-habit-ready-future-face ((t :background "#5F7F5F")))                ; ,zenburn-green-2
   `(org-habit-alert-future-face ((t :background "#D0BF8F" :foreground "#3F3F3F"))) ; ,zenburn-yellow-2 fg ,zenburn-bg
   `(org-habit-overdue-future-face ((t :background "#8C5353"))) ; ,zenburn-red-4
   `(org-headline-done ((,class (:height 1.0 :weight normal :foreground "#57525c"))))
   `(org-hide ((,class (:foreground "#403b45"))))
   `(org-inlinetask ((,class (:box (:line-width 1 :color "#37323c") :foreground "#8c8890" :background "#252050"))))
   `(org-latex-and-related ((,class (:foreground "#cf996f" :background "#25202a"))))
   `(org-level-1 ((,class ,ol1)))
   `(org-level-2 ((,class ,ol2)))
   `(org-level-3 ((,class ,ol3)))
   `(org-level-4 ((,class ,ol4)))
   `(org-level-5 ((,class ,ol5)))
   `(org-level-6 ((,class ,ol6)))
   `(org-level-7 ((,class ,ol7)))
   `(org-level-8 ((,class ,ol8)))
   `(org-link ((,class ,link)))
   `(org-list-dt ((,class (:weight bold :foreground "#cfa161"))))
   `(org-macro ((,class (:weight bold :foreground "#1747fd"))))
   `(org-meta-line ((,class (:slant normal :foreground "#ff7138" :background "#38332a"))))
   `(org-mode-line-clock ((,class (:box (:line-width 1 :color "#cfa161") :foreground "#ffffff" :background "#065cd0"))))
   `(org-mode-line-clock-overrun ((,class (:weight bold :box (:line-width 1 :color "#cfa161") :foreground "#25202a" :background "#06bfc7"))))
   `(org-number-of-items ((,class (:weight bold :foreground "#25202a" :background "#8a458e"))))
   `(org-property-value ((,class (:foreground "#ff5fff"))))
   `(org-quote ((,class (:slant italic :foreground "#9a969e" :background "#252046"))))
   `(org-scheduled ((,class (:foreground "#cfccd2"))))
   `(org-scheduled-previously ((,class (:foreground "#ed9943"))))
   `(org-scheduled-today ((,class (:weight bold :foreground "#b4b5ca" :background "#252059"))))
   `(org-sexp-date ((,class (:foreground "#cc8b3d"))))
   `(org-special-keyword ((,class (:weight bold :foreground "#ff44ff" :background "#38203d"))))
   `(org-table ((,class (:foreground "#ff9bff" :background "#38203d")))) ;; :inherit fixed-pitch))))
   `(org-tag ((,class (:weight normal :slant italic :foreground "#6a6065" :background "#25202a"))))
   `(org-target ((,class (:foreground "#06925a"))))
   `(org-time-grid ((,class (:foreground "#35303a"))))
   `(org-todo ((,class (:weight bold :box (:line-width 1 :color "#2c5462") :foreground "#2c5462" :background "#253743"))))
   `(org-upcoming-deadline ((,class (:foreground "#06aab2"))))
   `(org-verbatim ((,class (:foreground "#ff993d" :background "#2c212a"))))
   `(org-verse ((,class (:slant italic :foreground "#9a969e" :background "#342f39"))))
   `(org-warning ((,class (:weight bold :foreground "#ffffff" :background "#54362a"))))
   `(outline-1 ((,class ,ol1)))
   `(outline-2 ((,class ,ol2)))
   `(outline-3 ((,class ,ol3)))
   `(outline-4 ((,class ,ol4)))
   `(outline-5 ((,class ,ol5)))
   `(outline-6 ((,class ,ol6)))
   `(outline-7 ((,class ,ol7)))
   `(outline-8 ((,class ,ol8)))
   `(pabbrev-debug-display-label-face ((,class (:foreground "#25202a" :background "#5edeb3"))))
   `(pabbrev-suggestions-face ((,class (:weight bold :foreground "#25202a" :background "#06ffff"))))
   `(pabbrev-suggestions-label-face ((,class (:weight bold :foreground "#25202a" :background "#64df19"))))
   `(paren-face-match ((,class ,paren-matched)))
   `(paren-face-mismatch ((,class ,paren-unmatched)))
   `(paren-face-no-match ((,class ,paren-unmatched)))
   `(persp-selected-face ((,class (:weight bold :foreground "#34292a"))))
   `(powerline-active1 ((,class (:foreground "#7e311e" :background "#cbc7ce" :inherit mode-line))))
   `(powerline-active2 ((,class (:foreground "#7e311e" :background "#c38f53" :inherit mode-line))))
   `(powerline-inactive1 ((,class (:foreground "#322d38" :background "#9b979f" :inherit mode-line-inactive))))
   `(powerline-inactive2 ((,class (:foreground "#322d38" :background "#5b5660" :inherit mode-line-inactive))))
   `(rainbow-delimiters-depth-1-face ((,class (:foreground "#938e84"))))
   `(rainbow-delimiters-depth-2-face ((,class (:foreground "#907733"))))
   `(rainbow-delimiters-depth-3-face ((,class (:foreground "#736e84"))))
   `(rainbow-delimiters-depth-4-face ((,class (:foreground "#936797"))))
   `(rainbow-delimiters-depth-5-face ((,class (:foreground "#738c94"))))
   `(rainbow-delimiters-depth-6-face ((,class (:foreground "#a1894f"))))
   `(rainbow-delimiters-depth-7-face ((,class (:foreground "#7e7a87"))))
   `(rainbow-delimiters-depth-8-face ((,class (:foreground "#835787"))))
   `(rainbow-delimiters-depth-9-face ((,class (:foreground "#7b8f97"))))
   `(rainbow-delimiters-mismatched-face ((,class ,paren-unmatched)))
   `(rainbow-delimiters-unmatched-face ((,class ,paren-unmatched)))
   `(recover-this-file ((,class (:weight bold :background "#06c0c8"))))
   `(rng-error ((,class (:weight bold :foreground "#06ffff" :background "#283a43"))))
   `(sh-heredoc ((,class (:foreground "#ffff0b" :background "#34292a"))))
   `(sh-quoted-exec ((,class (:foreground "#06eb74"))))
   `(shadow ((,class ,shadow)))         ; Used for grep context lines.
   `(shell-option-face ((,class (:foreground "#e074e3"))))
   `(shell-output-2-face ((,class (:foreground "#ffff0b"))))
   `(shell-output-3-face ((,class (:foreground "#64df19"))))
   `(shell-output-face ((,class (:foreground "#ffffff"))))
   ;; `(shell-prompt-face ((,class (:weight bold :foreground "#0601ff"))))
   `(shm-current-face ((,class (:background "#343551"))))
   `(shm-quarantine-face ((,class (:background "lemonchiffon"))))
   `(show-paren-match ((,class ,paren-matched)))
   `(show-paren-mismatch ((,class ,paren-unmatched)))
   `(sml-modeline-end-face ((,class (:background "#985213")))) ; #cfa161
   `(sml-modeline-vis-face ((,class (:background "#e9863f"))))
   `(term ((,class (:foreground "#cfccd2" :background "#25202a"))))

   ;; `(sp-pair-overlay-face ((,class ())))
   ;; `(sp-show-pair-enclosing ((,class ())))
   ;; `(sp-show-pair-match-face ((,class ()))) ; ~ Pair highlighting (matching tags).
   ;; `(sp-show-pair-mismatch-face ((,class ())))
   ;; `(sp-wrap-overlay-closing-pair ((,class ())))
   ;; `(sp-wrap-overlay-face ((,class ())))
   ;; `(sp-wrap-overlay-opening-pair ((,class ())))
   ;; `(sp-wrap-tag-overlay-face ((,class ())))

   `(speedbar-button-face ((,class (:foreground "#ff74ff"))))
   `(speedbar-directory-face ((,class (:foreground "#ffff7c"))))
   `(speedbar-file-face ((,class (:foreground "#ff747c"))))
   `(speedbar-highlight-face ((,class ,volatile-highlight)))
   `(speedbar-selected-face ((,class (:underline t :foreground "#06ffff"))))
   `(speedbar-tag-face ((,class (:foreground "#5fd5db"))))
   `(svn-status-directory-face ((,class ,directory)))
   `(svn-status-filename-face ((,class (:weight bold :foreground "#c27c45"))))
   `(svn-status-locked-face ((,class (:weight bold :foreground "#06ffff"))))
   `(svn-status-marked-face ((,class ,marked-line)))
   `(svn-status-marked-popup-face ((,class (:weight bold :foreground "#ff32ff"))))
   `(svn-status-switched-face ((,class (:slant italic :foreground "#77737b"))))
   `(svn-status-symlink-face ((,class ,symlink)))
   `(svn-status-update-available-face ((,class (:foreground "#065aff"))))
   `(tex-verbatim ((,class (:foreground "#ffff0b"))))
   `(tool-bar ((,class (:box (:line-width 1 :style released-button) :foreground "#ffffff" :background "#45404a"))))
   `(tooltip ((,class (:foreground "#ffffff" :background "#252046"))))
   `(traverse-match-face ((,class (:weight bold :foreground "#79d427"))))
   `(vc-annotate-face-3F3FFF ((,class (:foreground "#c4c00b" :background "#ffffff"))))
   `(vc-annotate-face-3F6CFF ((,class (:foreground "#c4c00b" :background "#ffffff"))))
   `(vc-annotate-face-3F99FF ((,class (:foreground "#c4660b" :background "#ffffff"))))
   `(vc-annotate-face-3FC6FF ((,class (:foreground "#c4660b" :background "#ffffff"))))
   `(vc-annotate-face-3FF3FF ((,class (:foreground "#c40c0b" :background "#ffffff"))))
   `(vc-annotate-face-3FFF56 ((,class (:foreground "#b801bc" :background "#ffffff"))))
   `(vc-annotate-face-3FFF83 ((,class (:foreground "#c40159" :background "#ffffff"))))
   `(vc-annotate-face-3FFFB0 ((,class (:foreground "#c40159" :background "#ffffff"))))
   `(vc-annotate-face-3FFFDD ((,class (:foreground "#c40c0b" :background "#ffffff"))))
   `(vc-annotate-face-56FF3F ((,class (:foreground "#b801bc" :background "#ffffff"))))
   `(vc-annotate-face-83FF3F ((,class (:foreground "#5401c8" :background "#ffffff"))))
   `(vc-annotate-face-B0FF3F ((,class (:foreground "#5401c8" :background "#ffffff"))))
   `(vc-annotate-face-DDFF3F ((,class (:foreground "#060cc8" :background "#ffffff"))))
   `(vc-annotate-face-F6FFCC ((,class (:foreground "#ffffff" :background "#252064"))))
   `(vc-annotate-face-FF3F3F ((,class (:foreground "#06c0c8" :background "#ffffff"))))
   `(vc-annotate-face-FF6C3F ((,class (:foreground "#06c0c8" :background "#ffffff"))))
   `(vc-annotate-face-FF993F ((,class (:foreground "#0666c8" :background "#ffffff"))))
   `(vc-annotate-face-FFC63F ((,class (:foreground "#0666c8" :background "#ffffff"))))
   `(vc-annotate-face-FFF33F ((,class (:foreground "#060cc8" :background "#ffffff"))))

   ;; ;; vc
   ;; (vc-up-to-date-state    ((,c :foreground ,(gc 'green-1))))
   ;; (vc-edited-state        ((,c :foreground ,(gc 'yellow+1))))
   ;; (vc-missing-state       ((,c :foreground ,(gc 'red))))
   ;; (vc-conflict-state      ((,c :foreground ,(gc 'red+2) :weight bold)))
   ;; (vc-locked-state        ((,c :foreground ,(gc 'cyan-1))))
   ;; (vc-locally-added-state ((,c :foreground ,(gc 'blue))))
   ;; (vc-needs-update-state  ((,c :foreground ,(gc 'magenta))))
   ;; (vc-removed-state       ((,c :foreground ,(gc 'red-1))))

   `(vhl/default-face ((,class ,volatile-highlight))) ; `volatile-highlights.el' (for undo, yank).
   `(w3m-anchor ((,class ,link)))
   `(w3m-arrived-anchor ((,class (:foreground "#69cf0b"))))
   `(w3m-bitmap-image-face ((,class (:foreground "#f7f5f9" :background "#ff01ff"))))
   `(w3m-bold ((,class (:weight bold :foreground "#ffffff"))))
   `(w3m-current-anchor ((,class (:weight bold :underline t :foreground "#ffff0b"))))
   `(w3m-form ((,class (:underline t :foreground "#065ab8"))))
   `(w3m-form-button-face ((,class (:weight bold :underline t :foreground "#f7f5f9" :background "#312c36"))))
   `(w3m-form-button-mouse-face ((,class (:underline t :foreground "#312c36" :background "#d781db"))))
   `(w3m-form-button-pressed-face ((,class (:weight bold :underline t :foreground "#f7f5f9" :background "#312c36"))))
   `(w3m-header-line-location-content-face ((,class (:foreground "#848088":background "#2c2731"))))
   `(w3m-header-line-location-title-face ((,class (:foreground "#d6aa58" :background "#2c2731"))))
   `(w3m-history-current-url-face ((,class (:foreground "#252458"))))
   `(w3m-image-face ((,class (:weight bold :foreground "#501155"))))
   `(w3m-link-numbering ((,class (:foreground "#50381e")))) ; mouseless browsing
   `(w3m-strike-through-face ((,class (:strike-through t))))
   `(w3m-underline-face ((,class (:underline t))))

   ;; `(web-mode-block-attr-name-face ((,class ())))
   ;; `(web-mode-block-attr-value-face ((,class ())))
   ;; `(web-mode-block-comment-face ((,class ())))
   ;; `(web-mode-block-control-face ((,class ())))
   ;; `(web-mode-block-delimiter-face ((,class ())))
   ;; `(web-mode-block-face ((,class ())))
   ;; `(web-mode-block-string-face ((,class ())))
   ;; `(web-mode-bold-face ((,class ())))
   ;; `(web-mode-builtin-face ((,class ())))
   ;; `(web-mode-comment-face ((,class ())))
   ;; `(web-mode-comment-keyword-face ((,class ())))
   ;; `(web-mode-constant-face ((,class ())))
   ;; `(web-mode-css-at-rule-face ((,class ())))
   ;; `(web-mode-css-color-face ((,class ())))
   ;; `(web-mode-css-comment-face ((,class ())))
   ;; `(web-mode-css-function-face ((,class ())))
   ;; `(web-mode-css-priority-face ((,class ())))
   ;; `(web-mode-css-property-name-face ((,class ())))
   ;; `(web-mode-css-pseudo-class-face ((,class ())))
   ;; `(web-mode-css-selector-face ((,class ())))
   ;; `(web-mode-css-string-face ((,class ())))
   ;; `(web-mode-css-variable-face ((,class ())))
   ;; `(web-mode-current-column-highlight-face ((,class ())))
   `(web-mode-current-element-highlight-face ((,class (:background "#6b330b")))) ; #061187
   ;; `(web-mode-doctype-face ((,class ())))
   ;; `(web-mode-error-face ((,class ())))
   ;; `(web-mode-filter-face ((,class ())))
   `(web-mode-folded-face ((,class (:box (:line-width 1 :color "#8c8890") :foreground "#6a659d" :background "#110cbe"))))
   ;; `(web-mode-function-call-face ((,class ())))
   ;; `(web-mode-function-name-face ((,class ())))
   ;; `(web-mode-html-attr-custom-face ((,class ())))
   ;; `(web-mode-html-attr-engine-face ((,class ())))
   ;; `(web-mode-html-attr-equal-face ((,class ())))
   `(web-mode-html-attr-name-face ((,class ,xml-attribute)))
   ;; `(web-mode-html-attr-value-face ((,class ())))
   ;; `(web-mode-html-entity-face ((,class ())))
   `(web-mode-html-tag-bracket-face ((,class ,xml-tag)))
   ;; `(web-mode-html-tag-custom-face ((,class ())))
   `(web-mode-html-tag-face ((,class ,xml-tag)))
   ;; `(web-mode-html-tag-namespaced-face ((,class ())))
   ;; `(web-mode-inlay-face ((,class ())))
   ;; `(web-mode-italic-face ((,class ())))
   ;; `(web-mode-javascript-comment-face ((,class ())))
   ;; `(web-mode-javascript-string-face ((,class ())))
   ;; `(web-mode-json-comment-face ((,class ())))
   ;; `(web-mode-json-context-face ((,class ())))
   ;; `(web-mode-json-key-face ((,class ())))
   ;; `(web-mode-json-string-face ((,class ())))
   ;; `(web-mode-jsx-depth-1-face ((,class ())))
   ;; `(web-mode-jsx-depth-2-face ((,class ())))
   ;; `(web-mode-jsx-depth-3-face ((,class ())))
   ;; `(web-mode-jsx-depth-4-face ((,class ())))
   ;; `(web-mode-keyword-face ((,class ())))
   ;; `(web-mode-param-name-face ((,class ())))
   ;; `(web-mode-part-comment-face ((,class ())))
   `(web-mode-part-face ((,class (:background "#252046"))))
   ;; `(web-mode-part-string-face ((,class ())))
   ;; `(web-mode-preprocessor-face ((,class ())))
   `(web-mode-script-face ((,class (:background "#332d37"))))
   ;; `(web-mode-sql-keyword-face ((,class ())))
   ;; `(web-mode-string-face ((,class ())))
   ;; `(web-mode-style-face ((,class ())))
   ;; `(web-mode-symbol-face ((,class ())))
   ;; `(web-mode-type-face ((,class ())))
   ;; `(web-mode-underline-face ((,class ())))
   ;; `(web-mode-variable-name-face ((,class ())))
   ;; `(web-mode-warning-face ((,class ())))
   ;; `(web-mode-whitespace-face ((,class ())))

   `(which-func ((,class (:weight bold :slant italic :foreground "#25202a"))))
   ;; `(which-key-command-description-face)
   ;; `(which-key-group-description-face)
   ;; `(which-key-highlighted-command-face)
   ;; `(which-key-key-face)
   `(which-key-local-map-description-face ((,class (:weight bold :background "#30272c" :inherit which-key-command-description-face))))
   ;; `(which-key-note-face)
   ;; `(which-key-separator-face)
   ;; `(which-key-special-key-face)
   `(widget-button ((,class ,link)))
   `(widget-button-pressed ((,class (:foreground "#06ffff"))))
   `(widget-documentation ((,class (:foreground "#ff74ff"))))
   `(widget-field ((,class (:background "#2b2630"))))
   `(widget-inactive ((,class (:foreground "#9a969e"))))
   `(widget-single-line-field ((,class (:background "#2b2630"))))
   `(woman-bold ((,class (:weight bold :foreground "#13c2ca"))))
   `(woman-italic ((,class (:weight bold :slant italic :foreground "#bd41ea"))))
   `(woman-symbol ((,class (:weight bold :foreground "#64df19"))))
   `(yas-field-debug-face ((,class (:foreground "#25202a" :background "#5edeb3"))))
   `(yas-field-highlight-face ((,class (:box (:line-width 1 :color "#807c84") :foreground "#ffffff" :background "#302331"))))

   ;; `(ztreep-arrow-face ((,class ())))
   ;; `(ztreep-diff-header-face ((,class ())))
   ;; `(ztreep-diff-header-small-face ((,class ())))
   `(ztreep-diff-model-add-face ((,class (:weight bold :foreground "#ff77ff"))))
   `(ztreep-diff-model-diff-face ((,class (:weight bold :foreground "#ffbb2c"))))
   `(ztreep-diff-model-ignored-face ((,class (:strike-through t :foreground "#66616b"))))
   `(ztreep-diff-model-normal-face ((,class (:foreground "#ffffff"))))
   ;; `(ztreep-expand-sign-face ((,class ())))
   ;; `(ztreep-header-face ((,class ())))
   ;; `(ztreep-leaf-face ((,class ())))
   ;; `(ztreep-node-face ((,class ())))

   ))

(custom-theme-set-variables 'leuven-dark

  ;; highlight-sexp-mode.
  '(hl-sexp-background-color "#33323e")

  '(ansi-color-faces-vector
    [default default default italic underline success warning error])

  ;; Colors used in Shell mode.
  '(ansi-color-names-vector
    ["#ffffff" "#37ffff" "#e074e3" "#3732ff" "#ffff0b" "#37ff3c" "#ff400b" "#848088"])
 )

;;;###autoload
(when (and (boundp 'custom-theme-load-path)
           load-file-name)
  ;; Add theme folder to `custom-theme-load-path' when installing over MELPA.
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'leuven-dark)

;; This is for the sake of Emacs.
;; Local Variables:
;; time-stamp-end: "$"
;; time-stamp-format: "%Y-%02m-%02d %02H:%02M"
;; time-stamp-start: "Last-Updated: "
;; End:

;;; leuven-dark-theme.el ends here
