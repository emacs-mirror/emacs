;;; modus-operandi-deuteranopia-theme.el --- Deuteranopia-optimized theme with a white background -*- lexical-binding:t -*-

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

  (deftheme modus-operandi-deuteranopia
    "Deuteranopia-optimized theme with a white background.
This variant is optimized for users with red-green color
deficiency (deuteranopia).  It conforms with the highest
legibility standard for color contrast between background and
foreground in any given piece of text, which corresponds to a
minimum contrast in relative luminance of 7:1 (WCAG AAA
standard).")

  (defconst modus-operandi-deuteranopia-palette
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
      (red-warmer      "#972500")
      (red-cooler      "#a0132f")
      (red-faint       "#7f0000")
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

      (bg-graph-red-0     "#d0b029")
      (bg-graph-red-1     "#e0cab4")
      (bg-graph-green-0   "#8ad080")
      (bg-graph-green-1   "#afdfa5")
      (bg-graph-yellow-0  "#ffcf00")
      (bg-graph-yellow-1  "#f9ff00")
      (bg-graph-blue-0    "#7f9fff")
      (bg-graph-blue-1    "#9fc6ff")
      (bg-graph-magenta-0 "#b0b0d0")
      (bg-graph-magenta-1 "#d0dfdf")
      (bg-graph-cyan-0    "#6faad9")
      (bg-graph-cyan-1    "#bfe0ff")

;;; Special purpose

      (bg-completion       "#c0deff")
      (bg-hover            "#97dfed")
      (bg-hover-secondary  "#f5d0a0")
      (bg-hl-line          "#dae5ec")
      (bg-region           "#bdbdbd")
      (fg-region           "#000000")

      (bg-char-0 "#7feaff")
      (bg-char-1 "#ffaaff")
      (bg-char-2 "#dff000")

      (bg-mode-line-active        "#d0d6ff")
      (fg-mode-line-active        "#0f0f0f")
      (border-mode-line-active    "#4f4f74")
      (bg-mode-line-inactive      "#e6e6e6")
      (fg-mode-line-inactive      "#585858")
      (border-mode-line-inactive  "#a3a3a3")

      (modeline-err     "#603a00")
      (modeline-warning "#454500")
      (modeline-info    "#023d92")

      (bg-tab-bar      "#dfdfdf")
      (bg-tab-current  "#ffffff")
      (bg-tab-other    "#c2c2c2")

;;; Diffs

      (bg-added           "#d5d7ff")
      (bg-added-faint     "#e6e6ff")
      (bg-added-refine    "#babcef")
      (bg-added-fringe    "#275acc")
      (fg-added           "#303099")
      (fg-added-intense   "#0303cc")

      (bg-changed         "#eecfdf")
      (bg-changed-faint   "#f0dde5")
      (bg-changed-refine  "#e0b0d0")
      (bg-changed-fringe  "#9f6ab0")
      (fg-changed         "#6f1343")
      (fg-changed-intense "#7f0f9f")

      (bg-removed         "#f4f099")
      (bg-removed-faint   "#f6f6b7")
      (bg-removed-refine  "#f0e56f")
      (bg-removed-fringe  "#c0b200")
      (fg-removed         "#553d00")
      (fg-removed-intense "#7f6f00")

      (bg-diff-context    "#f3f3f3")

;;; Paren match

      (bg-paren-match        "#5fcfff")
      (bg-paren-expression   "#efd3f5")
      (underline-paren-match unspecified)

;;; Mappings

;;;; General mappings

      (fringe bg-dim)
      (cursor blue-intense)

      (keybind blue-cooler)
      (name blue-cooler)
      (identifier yellow-faint)

      (err yellow-warmer)
      (warning yellow)
      (info blue)

      (underline-err yellow-intense)
      (underline-warning magenta-faint)
      (underline-note cyan)

      (bg-prominent-err bg-yellow-intense)
      (fg-prominent-err fg-main)
      (bg-prominent-warning bg-magenta-intense)
      (fg-prominent-warning fg-main)
      (bg-prominent-note bg-cyan-intense)
      (fg-prominent-note fg-main)

;;;; Code mappings

      (builtin magenta-warmer)
      (comment yellow-cooler)
      (constant blue-cooler)
      (docstring green-faint)
      (docmarkup magenta-faint)
      (fnname magenta)
      (keyword magenta-cooler)
      (preprocessor red-cooler)
      (string blue-warmer)
      (type cyan-cooler)
      (variable cyan)
      (rx-construct yellow-cooler)
      (rx-backslash blue-cooler)

;;;; Accent mappings

      (accent-0 blue)
      (accent-1 yellow-warmer)
      (accent-2 cyan)
      (accent-3 magenta-cooler)

;;;; Button mappings

      (fg-button-active fg-main)
      (fg-button-inactive fg-dim)
      (bg-button-active bg-active)
      (bg-button-inactive bg-dim)

;;;; Completion mappings

      (fg-completion-match-0 blue)
      (fg-completion-match-1 yellow-warmer)
      (fg-completion-match-2 cyan)
      (fg-completion-match-3 magenta-cooler)
      (bg-completion-match-0 unspecified)
      (bg-completion-match-1 unspecified)
      (bg-completion-match-2 unspecified)
      (bg-completion-match-3 unspecified)

;;;; Date mappings

      (date-common cyan)
      (date-deadline yellow-warmer)
      (date-event fg-alt)
      (date-holiday yellow-warmer)
      (date-now blue-faint)
      (date-range fg-alt)
      (date-scheduled yellow-cooler)
      (date-weekday cyan)
      (date-weekend yellow-faint)

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

      (fg-link-visited yellow-faint)
      (bg-link-visited unspecified)
      (underline-link-visited yellow-faint)

;;;; Mail mappings

      (mail-cite-0 blue-warmer)
      (mail-cite-1 yellow)
      (mail-cite-2 cyan-faint)
      (mail-cite-3 yellow-faint)
      (mail-part blue)
      (mail-recipient blue)
      (mail-subject yellow-cooler)
      (mail-other cyan-faint)

;;;; Mark mappings

      (bg-mark-delete bg-yellow-subtle)
      (fg-mark-delete yellow)
      (bg-mark-select bg-cyan-subtle)
      (fg-mark-select cyan)
      (bg-mark-other bg-magenta-subtle)
      (fg-mark-other magenta)

;;;; Prompt mappings

      (fg-prompt blue)
      (bg-prompt unspecified)

;;;; Prose mappings

      (prose-block fg-dim)
      (prose-code cyan-cooler)
      (prose-done blue)
      (prose-macro magenta-cooler)
      (prose-metadata fg-dim)
      (prose-metadata-value fg-alt)
      (prose-table fg-alt)
      (prose-tag magenta-faint)
      (prose-todo yellow-warmer)
      (prose-verbatim magenta-warmer)

;;;; Rainbow mappings

      (rainbow-0 blue)
      (rainbow-1 yellow)
      (rainbow-2 blue-warmer)
      (rainbow-3 yellow-cooler)
      (rainbow-4 blue-cooler)
      (rainbow-5 yellow-warmer)
      (rainbow-6 blue-faint)
      (rainbow-7 yellow-faint)
      (rainbow-8 cyan)

;;;; Space mappings

      (bg-space unspecified)
      (fg-space border)
      (bg-space-err bg-yellow-intense)

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
    "The entire palette of the `modus-operandi-deuteranopia' theme.

Named colors have the form (COLOR-NAME HEX-VALUE) with the former
as a symbol and the latter as a string.

Semantic color mappings have the form (MAPPING-NAME COLOR-NAME)
with both as symbols.  The latter is a named color that already
exists in the palette and is associated with a HEX-VALUE.")

  (defcustom modus-operandi-deuteranopia-palette-overrides nil
    "Overrides for `modus-operandi-deuteranopia-palette'.

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

  (modus-themes-theme modus-operandi-deuteranopia
                      modus-operandi-deuteranopia-palette
                      modus-operandi-deuteranopia-palette-overrides)

  (provide-theme 'modus-operandi-deuteranopia))

;;;###theme-autoload
(put 'modus-operandi-deuteranopia 'theme-properties '(:background-mode light :kind color-scheme :family modus))

;;; modus-operandi-deuteranopia-theme.el ends here
