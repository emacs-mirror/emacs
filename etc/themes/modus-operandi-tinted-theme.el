;;; modus-operandi-tinted-theme.el --- Elegant, highly legible and customizable light theme -*- lexical-binding:t -*-

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

  (deftheme modus-operandi-tinted
    "Elegant, highly legible and customizable light theme.
Conforms with the highest legibility standard for color contrast
between background and foreground in any given piece of text,
which corresponds to a minimum contrast in relative luminance of
7:1 (WCAG AAA standard).")

  (defconst modus-operandi-tinted-palette
    '(
;;; Basic values

      (bg-main          "#fbf7f0")
      (bg-dim           "#ede7db")
      (fg-main          "#000000")
      (fg-dim           "#595959")
      (fg-alt           "#193668")
      (bg-active        "#c9b9b0")
      (bg-inactive      "#dfd5cf")
      (border           "#9f9690")

;;; Common accent foregrounds

      (red             "#a60000")
      (red-warmer      "#972500")
      (red-cooler      "#a0132f")
      (red-faint       "#7f0000")
      (red-intense     "#d00000")
      (green           "#006800")
      (green-warmer    "#316500")
      (green-cooler    "#00663f")
      (green-faint     "#2a5045")
      (green-intense   "#008900")
      (yellow          "#6f5500")
      (yellow-warmer   "#884900")
      (yellow-cooler   "#7a4f2f")
      (yellow-faint    "#624416")
      (yellow-intense  "#808000")
      (blue            "#0031a9")
      (blue-warmer     "#354fcf")
      (blue-cooler     "#0000b0")
      (blue-faint      "#003497")
      (blue-intense    "#0000ff")
      (magenta         "#721045")
      (magenta-warmer  "#8f0075")
      (magenta-cooler  "#531ab6")
      (magenta-faint   "#7c318f")
      (magenta-intense "#dd22dd")
      (cyan            "#005e8b")
      (cyan-warmer     "#3f578f")
      (cyan-cooler     "#005f5f")
      (cyan-faint      "#005077")
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
      (bg-graph-green-0   "#4faa09")
      (bg-graph-green-1   "#8fef00")
      (bg-graph-yellow-0  "#ffcf00")
      (bg-graph-yellow-1  "#f9ff00")
      (bg-graph-blue-0    "#7090ff")
      (bg-graph-blue-1    "#9fc6ff")
      (bg-graph-magenta-0 "#e07fff")
      (bg-graph-magenta-1 "#fad0ff")
      (bg-graph-cyan-0    "#70d3f0")
      (bg-graph-cyan-1    "#afefff")

;;; Special purpose

      (bg-completion       "#f0c1cf")
      (bg-hover            "#94d4ff")
      (bg-hover-secondary  "#f5d0a0")
      (bg-hl-line          "#f1d5d0")
      (bg-region           "#c2bcb5")
      (fg-region           "#000000")

      (bg-char-0 "#7feaff")
      (bg-char-1 "#ffaaff")
      (bg-char-2 "#dff000")

      (bg-mode-line-active        "#cab9b2")
      (fg-mode-line-active        "#000000")
      (border-mode-line-active    "#545454")
      (bg-mode-line-inactive      "#dfd9cf")
      (fg-mode-line-inactive      "#585858")
      (border-mode-line-inactive  "#a59a94")

      (modeline-err     "#7f0000")
      (modeline-warning "#5f0070")
      (modeline-info    "#002580")

      (bg-tab-bar      "#e0d4ce")
      (bg-tab-current  "#fbf7f0")
      (bg-tab-other    "#c8b8b2")

;;; Diffs

      (bg-added           "#c3ebc1")
      (bg-added-faint     "#dcf8d1")
      (bg-added-refine    "#acd6a5")
      (bg-added-intense   "#8cca8c")
      (fg-added           "#005000")
      (fg-added-intense   "#006700")

      (bg-changed         "#ffdfa9")
      (bg-changed-faint   "#ffefbf")
      (bg-changed-refine  "#fac090")
      (bg-changed-intense "#d7c20a")
      (fg-changed         "#553d00")
      (fg-changed-intense "#655000")

      (bg-removed         "#f4d0cf")
      (bg-removed-faint   "#ffe9e5")
      (bg-removed-refine  "#f3b5a7")
      (bg-removed-intense "#d84a4f")
      (fg-removed         "#8f1313")
      (fg-removed-intense "#aa2222")

;;; Paren match

      (bg-paren-match        "#7fdfcf")
      (bg-paren-expression   "#efd3f5")
      (underline-paren-match unspecified)

;;; Mappings

;;;; General mappings

      (fringe bg-dim)
      (cursor red)

      (keybind blue-cooler)
      (name magenta)
      (identifier yellow-cooler)

      (err red)
      (warning yellow-warmer)
      (info cyan-cooler)

      (underline-err red-intense)
      (underline-warning yellow-intense)
      (underline-note cyan-intense)

;;;; Code mappings

      (builtin magenta-warmer)
      (comment red-faint)
      (constant blue-cooler)
      (docstring green-faint)
      (docmarkup magenta-faint)
      (fnname magenta)
      (keyword magenta-cooler)
      (preprocessor red-cooler)
      (string blue-warmer)
      (type cyan-cooler)
      (variable cyan)
      (rx-construct green-cooler)
      (rx-backslash magenta)

;;;; Accent mappings

      (accent-0 blue)
      (accent-1 magenta-warmer)
      (accent-2 cyan)
      (accent-3 red)

;;;; Button mappings

      (fg-button-active fg-main)
      (fg-button-inactive fg-dim)
      (bg-button-active bg-active)
      (bg-button-inactive bg-dim)

;;;; Completion mappings

      (fg-completion-match-0 blue)
      (fg-completion-match-1 magenta-warmer)
      (fg-completion-match-2 cyan)
      (fg-completion-match-3 red)
      (bg-completion-match-0 unspecified)
      (bg-completion-match-1 unspecified)
      (bg-completion-match-2 unspecified)
      (bg-completion-match-3 unspecified)

;;;; Date mappings

      (date-common cyan)
      (date-deadline red)
      (date-event fg-alt)
      (date-holiday magenta)
      (date-now fg-main)
      (date-scheduled yellow-warmer)
      (date-weekday cyan)
      (date-weekend red-faint)

;;;; Line number mappings

      (fg-line-number-inactive fg-dim)
      (fg-line-number-active fg-main)
      (bg-line-number-inactive bg-dim)
      (bg-line-number-active bg-active)

;;;; Link mappings

      (fg-link blue-warmer)
      (bg-link unspecified)
      (underline-link blue-warmer)

      (fg-link-symbolic cyan)
      (bg-link-symbolic unspecified)
      (underline-link-symbolic cyan)

      (fg-link-visited magenta)
      (bg-link-visited unspecified)
      (underline-link-visited magenta)

;;;; Mail mappings

      (mail-cite-0 blue-faint)
      (mail-cite-1 yellow-warmer)
      (mail-cite-2 cyan-cooler)
      (mail-cite-3 red-cooler)
      (mail-part cyan)
      (mail-recipient magenta-cooler)
      (mail-subject magenta-warmer)
      (mail-other magenta-faint)

;;;; Prompt mappings

      (fg-prompt cyan-cooler)
      (bg-prompt unspecified)

;;;; Prose mappings

      (prose-block fg-dim)
      (prose-code green-cooler)
      (prose-done green)
      (prose-macro magenta-cooler)
      (prose-metadata fg-dim)
      (prose-metadata-value fg-alt)
      (prose-table fg-alt)
      (prose-tag magenta-faint)
      (prose-todo red)
      (prose-verbatim magenta-warmer)

;;;; Rainbow mappings

      (rainbow-0 fg-main)
      (rainbow-1 magenta-intense)
      (rainbow-2 cyan-intense)
      (rainbow-3 red-warmer)
      (rainbow-4 yellow-intense)
      (rainbow-5 magenta-cooler)
      (rainbow-6 green-intense)
      (rainbow-7 blue-warmer)
      (rainbow-8 magenta-warmer)

;;;; Heading mappings

      (fg-heading-0 cyan-cooler)
      (fg-heading-1 fg-main)
      (fg-heading-2 yellow-faint)
      (fg-heading-3 fg-alt)
      (fg-heading-4 magenta)
      (fg-heading-5 green-faint)
      (fg-heading-6 red-faint)
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
    "The entire palette of the `modus-operandi-tinted' theme.

Named colors have the form (COLOR-NAME HEX-VALUE) with the former
as a symbol and the latter as a string.

Semantic color mappings have the form (MAPPING-NAME COLOR-NAME)
with both as symbols.  The latter is a named color that already
exists in the palette and is associated with a HEX-VALUE.

Semantic color mappings cannot be recursive: their value must be
either COLOR-NAME or HEX-VALUE.")

  (defcustom modus-operandi-tinted-palette-overrides nil
    "Overrides for `modus-operandi-tinted-palette'.

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

  (modus-themes-theme modus-operandi-tinted
                      modus-operandi-tinted-palette
                      modus-operandi-tinted-palette-overrides)

  (provide-theme 'modus-operandi-tinted))

;;;###theme-autoload
(put 'modus-operandi-tinted 'theme-properties '(:background-mode light :kind color-scheme :family modus))

;;; modus-operandi-tinted-theme.el ends here
