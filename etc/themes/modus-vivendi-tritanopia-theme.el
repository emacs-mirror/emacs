;;; modus-vivendi-tritanopia-theme.el --- Tritanopia-optimized theme with a black background -*- lexical-binding:t -*-

;; Copyright (C) 2019-2023  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Modus-Themes Development <~protesilaos/modus-themes@lists.sr.ht>
;; URL: https://git.sr.ht/~protesilaos/modus-themes
;; Mailing-List: https://lists.sr.ht/~protesilaos/modus-themes

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; The Modus themes conform with the highest standard for
;; color-contrast accessibility between background and foreground
;; values (WCAG AAA).  Please refer to the official Info manual for
;; further documentation (distributed with the themes, or available
;; at: <https://protesilaos.com/emacs/modus-themes>).

;;; Code:



(eval-and-compile
  (unless (and (fboundp 'require-theme)
               load-file-name
               (equal (file-name-directory load-file-name)
                      (expand-file-name "themes/" data-directory))
               (require-theme 'modus-themes t))
    (require 'modus-themes))

  (deftheme modus-vivendi-tritanopia
    "Tritanopia-optimized theme with a black background.
This variant is optimized for users with blue-yellow color
deficiency (tritanopia).  It conforms with the highest
legibility standard for color contrast between background and
foreground in any given piece of text, which corresponds to a
minimum contrast in relative luminance of 7:1 (WCAG AAA
standard).")

  (defconst modus-vivendi-tritanopia-palette
    '(
;;; Basic values

      (bg-main          "#000000")
      (bg-dim           "#1e1e1e")
      (fg-main          "#ffffff")
      (fg-dim           "#989898")
      (fg-alt           "#c6daff")
      (bg-active        "#535353")
      (bg-inactive      "#303030")
      (border           "#646464")

;;; Common accent foregrounds

      (red             "#ff5f59")
      (red-warmer      "#ff6740")
      (red-cooler      "#ff6f9f")
      (red-faint       "#ff9070")
      (red-intense     "#ff5f5f")
      (green           "#44bc44")
      (green-warmer    "#70b900")
      (green-cooler    "#00c06f")
      (green-faint     "#88ca9f")
      (green-intense   "#44df44")
      (yellow          "#cabf00")
      (yellow-warmer   "#ffa00f")
      (yellow-cooler   "#d8af7a")
      (yellow-faint    "#d2b580")
      (yellow-intense  "#efef00")
      (blue            "#2fafff")
      (blue-warmer     "#79a8ff")
      (blue-cooler     "#00bcff")
      (blue-faint      "#82b0ec")
      (blue-intense    "#338fff")
      (magenta         "#feacd0")
      (magenta-warmer  "#f78fe7")
      (magenta-cooler  "#b6a0ff")
      (magenta-faint   "#caa6df")
      (magenta-intense "#ef7fff")
      (cyan            "#00d3d0")
      (cyan-warmer     "#4ae2ff")
      (cyan-cooler     "#6ae4b9")
      (cyan-faint      "#7fdbdf")
      (cyan-intense    "#00eff0")

;;; Uncommon accent foregrounds

      (rust       "#db7b5f")
      (gold       "#c0965b")
      (olive      "#9cbd6f")
      (slate      "#76afbf")
      (indigo     "#9099d9")
      (maroon     "#cf7fa7")
      (pink       "#d09dc0")

;;; Common accent backgrounds

      (bg-red-intense     "#9d1f1f")
      (bg-green-intense   "#2f822f")
      (bg-yellow-intense  "#7a6100")
      (bg-blue-intense    "#1640b0")
      (bg-magenta-intense "#7030af")
      (bg-cyan-intense    "#2266ae")

      (bg-red-subtle      "#620f2a")
      (bg-green-subtle    "#00422a")
      (bg-yellow-subtle   "#4a4000")
      (bg-blue-subtle     "#242679")
      (bg-magenta-subtle  "#552f5f")
      (bg-cyan-subtle     "#004065")

      (bg-red-nuanced     "#2c0614")
      (bg-green-nuanced   "#001904")
      (bg-yellow-nuanced  "#221000")
      (bg-blue-nuanced    "#0f0e39")
      (bg-magenta-nuanced "#230631")
      (bg-cyan-nuanced    "#041529")

;;; Uncommon accent backgrounds

      (bg-ochre    "#442c2f")
      (bg-lavender "#38325c")
      (bg-sage     "#0f3d30")

;;; Graphs

      (bg-graph-red-0     "#b52c2c")
      (bg-graph-red-1     "#702020")
      (bg-graph-green-0   "#afd1c0")
      (bg-graph-green-1   "#607a8f")
      (bg-graph-yellow-0  "#facfd6")
      (bg-graph-yellow-1  "#b57b85")
      (bg-graph-blue-0    "#4f9fdf")
      (bg-graph-blue-1    "#004559")
      (bg-graph-magenta-0 "#b6427f")
      (bg-graph-magenta-1 "#7f506f")
      (bg-graph-cyan-0    "#57dfea")
      (bg-graph-cyan-1    "#00808f")

;;; Special purpose

      (bg-completion       "#004253")
      (bg-hover            "#8e3e3b")
      (bg-hover-secondary  "#00405f")
      (bg-hl-line          "#2f3849")
      (bg-region           "#5a5a5a")
      (fg-region           "#ffffff")

      (bg-char-0 "#922a00")
      (bg-char-1 "#4f3f7f")
      (bg-char-2 "#00709f")

      (bg-mode-line-active        "#003c52")
      (fg-mode-line-active        "#f0f0f0")
      (border-mode-line-active    "#5f8fb4")
      (bg-mode-line-inactive      "#2d2d2d")
      (fg-mode-line-inactive      "#969696")
      (border-mode-line-inactive  "#606060")

      (modeline-err     "#ff7fbf")
      (modeline-warning "#df9f93")
      (modeline-info    "#4fcfef")

      (bg-tab-bar      "#313131")
      (bg-tab-current  "#000000")
      (bg-tab-other    "#545454")

;;; Diffs

      (bg-added           "#004254")
      (bg-added-faint     "#003042")
      (bg-added-refine    "#004f7f")
      (bg-added-fringe    "#008fcf")
      (fg-added           "#9fdfdf")
      (fg-added-intense   "#50c0ef")

      (bg-changed         "#2f123f")
      (bg-changed-faint   "#1f022f")
      (bg-changed-refine  "#3f325f")
      (bg-changed-fringe  "#7f55a0")
      (fg-changed         "#e3cfff")
      (fg-changed-intense "#cf9fe2")

      (bg-removed         "#4f1119")
      (bg-removed-faint   "#380a0f")
      (bg-removed-refine  "#781a1f")
      (bg-removed-fringe  "#b81a1f")
      (fg-removed         "#ffbfbf")
      (fg-removed-intense "#ff9095")

      (bg-diff-context    "#1a1a1a")

;;; Paren match

      (bg-paren-match        "#2f7f9f")
      (bg-paren-expression   "#453040")
      (underline-paren-match unspecified)

;;; Mappings

;;;; General mappings

      (fringe bg-dim)
      (cursor red-intense)

      (keybind red)
      (name red-cooler)
      (identifier red-faint)

      (err red-warmer)
      (warning magenta)
      (info cyan)

      (underline-err red-intense)
      (underline-warning magenta-intense)
      (underline-note cyan-intense)

      (bg-prominent-err bg-red-intense)
      (fg-prominent-err fg-main)
      (bg-prominent-warning bg-magenta-intense)
      (fg-prominent-warning fg-main)
      (bg-prominent-note bg-cyan-intense)
      (fg-prominent-note fg-main)

;;;; Code mappings

      (builtin magenta)
      (comment red-faint)
      (constant green-faint)
      (docstring fg-alt)
      (docmarkup magenta-faint)
      (fnname cyan-warmer)
      (keyword red-cooler)
      (preprocessor red-warmer)
      (string cyan)
      (type blue-warmer)
      (variable cyan-cooler)
      (rx-construct red)
      (rx-backslash magenta)

;;;; Accent mappings

      (accent-0 cyan)
      (accent-1 red-warmer)
      (accent-2 cyan-cooler)
      (accent-3 magenta)

;;;; Button mappings

      (fg-button-active fg-main)
      (fg-button-inactive fg-dim)
      (bg-button-active bg-active)
      (bg-button-inactive bg-dim)

;;;; Completion mappings

      (fg-completion-match-0 cyan)
      (fg-completion-match-1 red-warmer)
      (fg-completion-match-2 magenta)
      (fg-completion-match-3 cyan-cooler)
      (bg-completion-match-0 unspecified)
      (bg-completion-match-1 unspecified)
      (bg-completion-match-2 unspecified)
      (bg-completion-match-3 unspecified)

;;;; Date mappings

      (date-common cyan-cooler)
      (date-deadline red)
      (date-event fg-alt)
      (date-holiday red-intense)
      (date-now fg-main)
      (date-range fg-alt)
      (date-scheduled magenta)
      (date-weekday cyan)
      (date-weekend red-faint)

;;;; Line number mappings

      (fg-line-number-inactive fg-dim)
      (fg-line-number-active fg-main)
      (bg-line-number-inactive bg-dim)
      (bg-line-number-active bg-active)

;;;; Link mappings

      (fg-link cyan)
      (bg-link unspecified)
      (underline-link cyan)

      (fg-link-symbolic cyan-cooler)
      (bg-link-symbolic unspecified)
      (underline-link-symbolic cyan-cooler)

      (fg-link-visited magenta)
      (bg-link-visited unspecified)
      (underline-link-visited magenta)

;;;; Mail mappings

      (mail-cite-0 cyan-faint)
      (mail-cite-1 red-faint)
      (mail-cite-2 magenta-warmer)
      (mail-cite-3 cyan-warmer)
      (mail-part cyan-cooler)
      (mail-recipient cyan)
      (mail-subject red-cooler)
      (mail-other cyan)

;;;; Mark mappings

      (bg-mark-delete bg-red-subtle)
      (fg-mark-delete red)
      (bg-mark-select bg-cyan-subtle)
      (fg-mark-select cyan)
      (bg-mark-other bg-magenta-subtle)
      (fg-mark-other magenta-warmer)

;;;; Prompt mappings

      (fg-prompt cyan-cooler)
      (bg-prompt unspecified)

;;;; Prose mappings

      (prose-block fg-dim)
      (prose-code cyan)
      (prose-done cyan)
      (prose-macro red-warmer)
      (prose-metadata fg-dim)
      (prose-metadata-value fg-alt)
      (prose-table fg-alt)
      (prose-tag fg-alt)
      (prose-todo red)
      (prose-verbatim magenta-warmer)

;;;; Rainbow mappings

      (rainbow-0 cyan)
      (rainbow-1 red)
      (rainbow-2 cyan-warmer)
      (rainbow-3 red-cooler)
      (rainbow-4 cyan-cooler)
      (rainbow-5 magenta)
      (rainbow-6 cyan-faint)
      (rainbow-7 magenta-faint)
      (rainbow-8 red-faint)

;;;; Space mappings

      (bg-space unspecified)
      (fg-space border)
      (bg-space-err bg-red-intense)

;;;; Heading mappings

      (fg-heading-0 cyan-cooler)
      (fg-heading-1 fg-main)
      (fg-heading-2 red-faint)
      (fg-heading-3 cyan-faint)
      (fg-heading-4 magenta)
      (fg-heading-5 green-faint)
      (fg-heading-6 magenta-faint)
      (fg-heading-7 cyan-faint)
      (fg-heading-8 fg-dim)

      (bg-heading-0 unspecified)
      (bg-heading-1 unspecified)
      (bg-heading-2 unspecified)
      (bg-heading-3 unspecified)
      (bg-heading-4 unspecified)
      (bg-heading-5 unspecified)
      (bg-heading-6 unspecified)
      (bg-heading-7 unspecified)
      (bg-heading-8 unspecified)

      (overline-heading-0 unspecified)
      (overline-heading-1 unspecified)
      (overline-heading-2 unspecified)
      (overline-heading-3 unspecified)
      (overline-heading-4 unspecified)
      (overline-heading-5 unspecified)
      (overline-heading-6 unspecified)
      (overline-heading-7 unspecified)
      (overline-heading-8 unspecified))
    "The entire palette of the `modus-vivendi-tritanopia' theme.

Named colors have the form (COLOR-NAME HEX-VALUE) with the former
as a symbol and the latter as a string.

Semantic color mappings have the form (MAPPING-NAME COLOR-NAME)
with both as symbols.  The latter is a named color that already
exists in the palette and is associated with a HEX-VALUE.")

  (defcustom modus-vivendi-tritanopia-palette-overrides nil
    "Overrides for `modus-vivendi-tritanopia-palette'.

Mirror the elements of the aforementioned palette, overriding
their value.

For overrides that are shared across all of the Modus themes,
refer to `modus-themes-common-palette-overrides'.

Theme-specific overrides take precedence over shared overrides.
The idea of common overrides is to change semantic color
mappings, such as to make the cursor red.  Wherea theme-specific
overrides can also be used to change the value of a named color,
such as what hexadecimal RGB value the red-warmer symbol
represents."
    :group 'modus-themes
    :package-version '(modus-themes . "4.0.0")
    :version "30.1"
    :type '(repeat (list symbol (choice symbol string)))
    :set #'modus-themes--set-option
    :initialize #'custom-initialize-default
    :link '(info-link "(modus-themes) Palette overrides"))

  (modus-themes-theme modus-vivendi-tritanopia
                      modus-vivendi-tritanopia-palette
                      modus-vivendi-tritanopia-palette-overrides)

  (provide-theme 'modus-vivendi-tritanopia))

;;;###theme-autoload
(put 'modus-vivendi-tritanopia 'theme-properties '(:background-mode dark :kind color-scheme :family modus))

;;; modus-vivendi-tritanopia-theme.el ends here
