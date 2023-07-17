;;; modus-operandi-tritanopia-theme.el --- Tritanopia-optimized theme with a white background -*- lexical-binding:t -*-

;; Copyright (C) 2019-2023  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Modus-Themes Development <~protesilaos/modus-themes@lists.sr.ht>
;; URL: https://git.sr.ht/~protesilaos/modus-themes
;; Mailing-List: https://lists.sr.ht/~protesilaos/modus-themes
;; Keywords: faces, theme, accessibility

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

  (deftheme modus-operandi-tritanopia
    "Tritanopia-optimized theme with a white background.
This variant is optimized for users with blue-yellow color
deficiency (tritanopia).  It conforms with the highest
legibility standard for color contrast between background and
foreground in any given piece of text, which corresponds to a
minimum contrast in relative luminance of 7:1 (WCAG AAA
standard).")

  (defconst modus-operandi-tritanopia-palette
    '(
;;; Basic values

      (bg-main          "#ffffff")
      (bg-dim           "#f0f0f0")
      (fg-main          "#000000")
      (fg-dim           "#595959")
      (fg-alt           "#193668")
      (bg-active        "#c4c4c4")
      (bg-inactive      "#e0e0e0")
      (border           "#9f9f9f")

;;; Common accent foregrounds

      (red             "#a60000")
      (red-warmer      "#b21100")
      (red-cooler      "#a0132f")
      (red-faint       "#702000")
      (red-intense     "#d00000")
      (green           "#006800")
      (green-warmer    "#316500")
      (green-cooler    "#00663f")
      (green-faint     "#2a5045")
      (green-intense   "#008900")
      (yellow          "#695500")
      (yellow-warmer   "#973300")
      (yellow-cooler   "#77492f")
      (yellow-faint    "#624416")
      (yellow-intense  "#808000")
      (blue            "#0031a9")
      (blue-warmer     "#3548cf")
      (blue-cooler     "#0000b0")
      (blue-faint      "#003497")
      (blue-intense    "#0000ff")
      (magenta         "#721045")
      (magenta-warmer  "#8f0075")
      (magenta-cooler  "#531ab6")
      (magenta-faint   "#7c318f")
      (magenta-intense "#cd22bd")
      (cyan            "#005e8b")
      (cyan-warmer     "#3f578f")
      (cyan-cooler     "#005f5f")
      (cyan-faint      "#004f5f")
      (cyan-intense    "#008899")

;;; Uncommon accent foregrounds

      (rust       "#8a290f")
      (gold       "#80601f")
      (olive      "#56692d")
      (slate      "#2f3f83")
      (indigo     "#4a3a8a")
      (maroon     "#731c52")
      (pink       "#7b435c")

;;; Common accent backgrounds

      (bg-red-intense     "#ff8f88")
      (bg-green-intense   "#8adf80")
      (bg-yellow-intense  "#f3d000")
      (bg-blue-intense    "#bfc9ff")
      (bg-magenta-intense "#dfa0f0")
      (bg-cyan-intense    "#a4d5f9")

      (bg-red-subtle      "#ffcfbf")
      (bg-green-subtle    "#b3fabf")
      (bg-yellow-subtle   "#fff576")
      (bg-blue-subtle     "#ccdfff")
      (bg-magenta-subtle  "#ffddff")
      (bg-cyan-subtle     "#bfefff")

      (bg-red-nuanced     "#fff1f0")
      (bg-green-nuanced   "#ecf7ed")
      (bg-yellow-nuanced  "#fff3da")
      (bg-blue-nuanced    "#f3f3ff")
      (bg-magenta-nuanced "#fdf0ff")
      (bg-cyan-nuanced    "#ebf6fa")

;;; Uncommon accent backgrounds

      (bg-ochre    "#f0e0cc")
      (bg-lavender "#dfdbfa")
      (bg-sage     "#c0e7d4")

;;; Graphs

      (bg-graph-red-0     "#ef7969")
      (bg-graph-red-1     "#ffaab4")
      (bg-graph-green-0   "#70c3b0")
      (bg-graph-green-1   "#a3dfe5")
      (bg-graph-yellow-0  "#d99f9f")
      (bg-graph-yellow-1  "#ffb58f")
      (bg-graph-blue-0    "#80a0df")
      (bg-graph-blue-1    "#9fcaff")
      (bg-graph-magenta-0 "#efafcf")
      (bg-graph-magenta-1 "#ffdaef")
      (bg-graph-cyan-0    "#7fd3ed")
      (bg-graph-cyan-1    "#afefff")

;;; Special purpose

      (bg-completion       "#afdfef")
      (bg-hover            "#ffafbc")
      (bg-hover-secondary  "#9fdfff")
      (bg-hl-line          "#dfeaec")
      (bg-region           "#bdbdbd")
      (fg-region           "#000000")

      (bg-char-0 "#ff8a5f")
      (bg-char-1 "#bf7aff")
      (bg-char-2 "#7fe0e0")

      (bg-mode-line-active        "#afe0f2")
      (fg-mode-line-active        "#0f0f0f")
      (border-mode-line-active    "#2f4f44")
      (bg-mode-line-inactive      "#e6e6e6")
      (fg-mode-line-inactive      "#585858")
      (border-mode-line-inactive  "#a3a3a3")

      (modeline-err     "#8f0000")
      (modeline-warning "#6f306f")
      (modeline-info    "#00445f")

      (bg-tab-bar      "#dfdfdf")
      (bg-tab-current  "#ffffff")
      (bg-tab-other    "#c2c2c2")

;;; Diffs

      (bg-added           "#b5e7ff")
      (bg-added-faint     "#c6f6ff")
      (bg-added-refine    "#9adcef")
      (bg-added-fringe    "#1782cc")
      (fg-added           "#005079")
      (fg-added-intense   "#0043aa")

      (bg-changed         "#eecfdf")
      (bg-changed-faint   "#f0dde5")
      (bg-changed-refine  "#e0b0d0")
      (bg-changed-fringe  "#9f6ab0")
      (fg-changed         "#6f1343")
      (fg-changed-intense "#7f0f9f")

      (bg-removed         "#ffd8d5")
      (bg-removed-faint   "#ffe9e9")
      (bg-removed-refine  "#f3b5af")
      (bg-removed-fringe  "#d84a4f")
      (fg-removed         "#8f1313")
      (fg-removed-intense "#aa2222")

      (bg-diff-context    "#f3f3f3")

;;; Paren match

      (bg-paren-match        "#5fcfff")
      (bg-paren-expression   "#efd3f5")
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
      (constant green-cooler)
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
      (date-holiday red)
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
      (fg-mark-other magenta)

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
      (fg-heading-7 cyan-warmer)
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
    "The entire palette of the `modus-operandi-tritanopia' theme.

Named colors have the form (COLOR-NAME HEX-VALUE) with the former
as a symbol and the latter as a string.

Semantic color mappings have the form (MAPPING-NAME COLOR-NAME)
with both as symbols.  The latter is a named color that already
exists in the palette and is associated with a HEX-VALUE.")

  (defcustom modus-operandi-tritanopia-palette-overrides nil
    "Overrides for `modus-operandi-tritanopia-palette'.

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

  (modus-themes-theme modus-operandi-tritanopia
                      modus-operandi-tritanopia-palette
                      modus-operandi-tritanopia-palette-overrides)

  (provide-theme 'modus-operandi-tritanopia))

;;;###theme-autoload
(put 'modus-operandi-tritanopia 'theme-properties '(:background-mode light :kind color-scheme :family modus))

;;; modus-operandi-tritanopia-theme.el ends here
