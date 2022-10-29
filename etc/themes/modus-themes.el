;;; modus-themes.el --- Elegant, highly legible and customizable themes -*- lexical-binding:t -*-

;; Copyright (C) 2019-2022  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Modus-Themes Development <~protesilaos/modus-themes@lists.sr.ht>
;; URL: https://git.sr.ht/~protesilaos/modus-themes
;; Mailing-List: https://lists.sr.ht/~protesilaos/modus-themes
;; Version: 3.0.0
;; Package-Requires: ((emacs "27.1"))
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
;; The Modus themes conform with the highest standard for color-contrast
;; accessibility between background and foreground values (WCAG AAA).
;; This file contains all customization variables, helper functions,
;; interactive commands, and face specifications.  Please refer to the
;; official Info manual for further documentation (distributed with the
;; themes, or available at: <https://protesilaos.com/emacs/modus-themes>).
;;
;; The themes share the following customization variables:
;;
;;     modus-themes-completions                    (alist)
;;     modus-themes-headings                       (alist)
;;     modus-themes-org-agenda                     (alist)
;;     modus-themes-bold-constructs                (boolean)
;;     modus-themes-deuteranopia                   (boolean)
;;     modus-themes-inhibit-reload                 (boolean)
;;     modus-themes-intense-mouseovers             (boolean)
;;     modus-themes-italic-constructs              (boolean)
;;     modus-themes-mixed-fonts                    (boolean)
;;     modus-themes-subtle-line-numbers            (boolean)
;;     modus-themes-variable-pitch-ui              (boolean)
;;     modus-themes-box-buttons                    (choice)
;;     modus-themes-diffs                          (choice)
;;     modus-themes-fringes                        (choice)
;;     modus-themes-hl-line                        (choice)
;;     modus-themes-lang-checkers                  (choice)
;;     modus-themes-links                          (choice)
;;     modus-themes-mail-citations                 (choice)
;;     modus-themes-markup                         (choice)
;;     modus-themes-mode-line                      (choice)
;;     modus-themes-org-blocks                     (choice)
;;     modus-themes-paren-match                    (choice)
;;     modus-themes-prompts                        (choice)
;;     modus-themes-region                         (choice)
;;     modus-themes-syntax                         (choice)
;;
;; There also exist two unique customization variables for overriding
;; color palette values.  The specifics are documented in the manual.
;; The symbols are:
;;
;;     modus-themes-operandi-color-overrides       (alist)
;;     modus-themes-vivendi-color-overrides        (alist)
;;
;; Check the manual for all supported packages (there are hundreds of
;; them).
;;
;; For a complete view of the project, also refer to the following files
;; (should be distributed in the same repository/directory as the
;; current item):
;;
;; - modus-operandi-theme.el    (Light theme)
;; - modus-vivendi-theme.el     (Dark theme)

;;; Code:



(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

(defgroup modus-themes ()
  "Options for `modus-operandi', `modus-vivendi' themes.
The Modus themes conform with the WCAG AAA standard for color
contrast between background and foreground combinations (a
minimum contrast of 7:1---the highest standard of its kind).  The
themes also strive to empower users with red-green color
deficiency: this is achieved through customization variables that
replace all relevant instances of green with blue, as well as the
overall design of the themes which relies mostly on colors that
cover the blue-cyan-magenta side of the spectrum."
  :group 'faces
  :link '(info-link "(modus-themes) Top")
  :prefix "modus-themes-"
  :tag "Modus Themes")

(defgroup modus-themes-faces ()
  "Faces defined by `modus-operandi' and `modus-vivendi' themes."
  :group 'modus-themes
  :link '(info-link "(modus-themes) Top")
  :prefix "modus-themes-"
  :tag "Modus Themes Faces")

(defvar modus-themes--version "3.0.0"
  "Current version of the Modus themes.

The version either is the last tagged release, such as '1.0.0',
or an in-development version like '1.1.0-dev'.  As we use
semantic versioning, tags of the '1.0.1' sort are not reported:
those would count as part of '1.1.0-dev'.")

;;;###autoload
(defun modus-themes-version (&optional insert)
  "Print `modus-themes--version' in the echo area.
If optional INSERT argument is provided from Lisp or as a prefix
argument, insert the `modus-themes--version' at point."
  (interactive "P")
  (funcall (if insert 'insert 'message) modus-themes--version))

;;;###autoload
(defun modus-themes-report-bug ()
  "Submit a bug report or issue to the Modus themes developers."
  (interactive)
  (reporter-submit-bug-report
   "~protesilaos/modus-themes@lists.sr.ht"
   (format "modus-themes (%s)\n" modus-themes--version)
   ;; I am just getting started with this.  Let's first see what people
   ;; think about it.
   nil nil nil nil))

;;; Variables for each theme variant

;;;; Modus Operandi

(defconst modus-themes-operandi-colors
  '(;; base values
    (bg-main . "#ffffff") (fg-main . "#000000")
    (bg-dim . "#f8f8f8") (fg-dim . "#282828")
    (bg-alt . "#f0f0f0") (fg-alt . "#505050")
    ;; specifically for on/off states and must be combined with
    ;; themselves, though the backgrounds are also meant to be used with
    ;; other "active" values, defined further below; bg-active-accent
    ;; can work as a substitute for bg-active
    (bg-active . "#d7d7d7") (fg-active . "#0a0a0a")
    (bg-inactive . "#efefef") (fg-inactive . "#404148")
    (bg-active-accent . "#d0d6ff")
    ;; these special values are intended as alternatives to the base
    ;; values for cases where we need to avoid confusion between the
    ;; highlighted constructs; they must either be used as pairs based
    ;; on their name or each can be combined with {fg,bg}-{main,alt,dim}
    ;; always in accordance with their role as background or foreground
    (bg-special-cold . "#dde3f4") (bg-special-faint-cold . "#f0f1ff") (fg-special-cold . "#093060")
    (bg-special-mild . "#c4ede0") (bg-special-faint-mild . "#ebf5eb") (fg-special-mild . "#184034")
    (bg-special-warm . "#f0e0d4") (bg-special-faint-warm . "#fef2ea") (fg-special-warm . "#5d3026")
    (bg-special-calm . "#f8ddea") (bg-special-faint-calm . "#faeff9") (fg-special-calm . "#61284f")
    ;; foregrounds that can be combined with bg-main, bg-dim, bg-alt
    (red . "#a60000")
    (red-alt . "#972500")
    (red-alt-other . "#a0132f")
    (red-faint . "#7f1010")
    (red-alt-faint . "#702f00")
    (red-alt-other-faint . "#7f002f")
    (green . "#005e00")
    (green-alt . "#315b00")
    (green-alt-other . "#145c33")
    (green-faint . "#104410")
    (green-alt-faint . "#30440f")
    (green-alt-other-faint . "#0f443f")
    (yellow . "#813e00")
    (yellow-alt . "#70480f")
    (yellow-alt-other . "#863927")
    (yellow-faint . "#5f4400")
    (yellow-alt-faint . "#5d5000")
    (yellow-alt-other-faint . "#5e3a20")
    (blue . "#0031a9")
    (blue-alt . "#2544bb")
    (blue-alt-other . "#0000c0")
    (blue-faint . "#003497")
    (blue-alt-faint . "#0f3d8c")
    (blue-alt-other-faint . "#001087")
    (magenta . "#721045")
    (magenta-alt . "#8f0075")
    (magenta-alt-other . "#5317ac")
    (magenta-faint . "#752f50")
    (magenta-alt-faint . "#7b206f")
    (magenta-alt-other-faint . "#55348e")
    (cyan . "#00538b")
    (cyan-alt . "#30517f")
    (cyan-alt-other . "#005a5f")
    (cyan-faint . "#005077")
    (cyan-alt-faint . "#354f6f")
    (cyan-alt-other-faint . "#125458")
    ;; these foreground values can only be combined with bg-main and are
    ;; thus not suitable for general purpose highlighting
    (red-intense . "#b60000")
    (orange-intense . "#904200")
    (green-intense . "#006800")
    (yellow-intense . "#605b00")
    (blue-intense . "#1f1fce")
    (magenta-intense . "#a8007f")
    (purple-intense . "#7f10d0")
    (cyan-intense . "#005f88")
    ;; those foregrounds are meant exclusively for bg-active, bg-inactive
    (red-active . "#8a0000")
    (green-active . "#004c2e")
    (yellow-active . "#702f00")
    (blue-active . "#0030b4")
    (magenta-active . "#5c2092")
    (cyan-active . "#003f8a")
    ;; the "subtle" values below be combined with fg-dim, while the
    ;; "intense" should be paired with fg-main
    (red-subtle-bg . "#f2b0a2")
    (red-intense-bg . "#ff9f9f")
    (green-subtle-bg . "#aecf90")
    (green-intense-bg . "#5ada88")
    (yellow-subtle-bg . "#e4c340")
    (yellow-intense-bg . "#f5df23")
    (blue-subtle-bg . "#b5d0ff")
    (blue-intense-bg . "#77baff")
    (magenta-subtle-bg . "#f0d3ff")
    (magenta-intense-bg . "#d5baff")
    (cyan-subtle-bg . "#c0efff")
    (cyan-intense-bg . "#42cbd4")
    ;; those background values must be combined with fg-main and should
    ;; only be used for indicators that are placed on the fringes
    (red-fringe-bg . "#f08290")
    (green-fringe-bg . "#62c86a")
    (yellow-fringe-bg . "#dbba3f")
    (blue-fringe-bg . "#82afff")
    (magenta-fringe-bg . "#e0a3ff")
    (cyan-fringe-bg . "#2fcddf")
    ;; those background values should only be used for graphs or similar
    ;; applications where colored blocks are expected to be positioned
    ;; next to each other
    (red-graph-0-bg . "#ef7969")
    (red-graph-1-bg . "#ffaab4")
    (green-graph-0-bg . "#4faa09")
    (green-graph-1-bg . "#8fef00")
    (yellow-graph-0-bg . "#ffcf00")
    (yellow-graph-1-bg . "#f9ff00")
    (blue-graph-0-bg . "#7090ff")
    (blue-graph-1-bg . "#9fc6ff")
    (magenta-graph-0-bg . "#e07fff")
    (magenta-graph-1-bg . "#fad0ff")
    (cyan-graph-0-bg . "#70d3f0")
    (cyan-graph-1-bg . "#afefff")
    ;; the following are for cases where both the foreground and the
    ;; background need to have a similar hue and so must be combined
    ;; with themselves, even though the foregrounds can be paired with
    ;; any of the base backgrounds
    (red-refine-bg . "#ffcccc") (red-refine-fg . "#780000")
    (green-refine-bg . "#aceaac") (green-refine-fg . "#004c00")
    (yellow-refine-bg . "#fff29a") (yellow-refine-fg . "#604000")
    (blue-refine-bg . "#8fcfff") (blue-refine-fg . "#002f88")
    (magenta-refine-bg . "#ffccff") (magenta-refine-fg . "#770077")
    (cyan-refine-bg . "#8eecf4") (cyan-refine-fg . "#004850")
    ;; the "nuanced" backgrounds can be combined with all of the above
    ;; foregrounds, as well as those included here, while the "nuanced"
    ;; foregrounds can in turn also be combined with bg-main, bg-dim,
    ;; bg-alt
    (red-nuanced-bg . "#fff1f0") (red-nuanced-fg . "#5f0000")
    (green-nuanced-bg . "#ecf7ed") (green-nuanced-fg . "#004000")
    (yellow-nuanced-bg . "#fff3da") (yellow-nuanced-fg . "#3f3000")
    (blue-nuanced-bg . "#f3f3ff") (blue-nuanced-fg . "#201f55")
    (magenta-nuanced-bg . "#fdf0ff") (magenta-nuanced-fg . "#541f4f")
    (cyan-nuanced-bg . "#ebf6fa") (cyan-nuanced-fg . "#0f3360")
    ;; the following are reserved for specific cases
    ;;
    ;; bg-hl-line is between bg-dim and bg-alt, so it should
    ;; work with all accents that cover those two, plus bg-main
    ;;
    ;; bg-hl-alt and bg-hl-alt-intense should only be used when no
    ;; other grayscale or fairly neutral background is available to
    ;; properly draw attention to a given construct
    ;;
    ;; bg-header is between bg-active and bg-inactive, so it
    ;; can be combined with any of the "active" values, plus the
    ;; "special" and base foreground colors
    ;;
    ;; bg-paren-match, bg-paren-match-intense, bg-region,
    ;; bg-region-accent and bg-tab-active must be combined with fg-main,
    ;; while bg-tab-inactive should be combined with fg-dim, whereas
    ;; bg-tab-inactive-alt goes together with fg-main
    ;;
    ;; bg-completion-* and bg-char-* variants are meant to be combined
    ;; with fg-main
    ;;
    ;; fg-escape-char-construct and fg-escape-char-backslash can
    ;; be combined bg-main, bg-dim, bg-alt
    ;;
    ;; fg-lang-error, fg-lang-warning, fg-lang-note can be
    ;; combined with bg-main, bg-dim, bg-alt
    ;;
    ;; fg-mark-sel, fg-mark-del, fg-mark-alt can be combined
    ;; with bg-main, bg-dim, bg-alt, bg-hl-line
    ;;
    ;; fg-unfocused must be combined with bg-main
    ;;
    ;; fg-docstring, fg-comment-yellow can be combined with
    ;; bg-main, bg-dim, bg-alt
    ;;
    ;; the window divider colors apply to faces with just an fg value
    ;;
    ;; all pairs are combinable with themselves
    (bg-hl-line . "#f2eff3")
    (bg-hl-line-intense . "#e0e0e0")
    (bg-hl-line-intense-accent . "#cfe2ff")
    (bg-hl-alt . "#fbeee0")
    (bg-hl-alt-intense . "#e8dfd1")
    (bg-paren-match . "#e0af82")
    (bg-paren-match-intense . "#c488ff")
    (bg-paren-expression . "#dff0ff")
    (bg-region . "#bcbcbc")
    (bg-region-accent . "#afafef")
    (bg-region-accent-subtle . "#efdfff")

    (bg-completion . "#b7dbff")
    (bg-completion-subtle . "#def3ff")

    (bg-char-0 . "#7feaff")
    (bg-char-1 . "#ffaaff")
    (bg-char-2 . "#dff000")

    (bg-tab-active . "#f6f6f6")
    (bg-tab-inactive . "#b7b7b7")
    (bg-tab-inactive-accent . "#a9b4f6")
    (bg-tab-inactive-alt . "#9f9f9f")
    (bg-tab-inactive-alt-accent . "#9fa6d0")

    (red-tab . "#680000")
    (green-tab . "#003900")
    (yellow-tab . "#393000")
    (orange-tab . "#502300")
    (blue-tab . "#000080")
    (cyan-tab . "#052f60")
    (magenta-tab . "#5f004d")
    (purple-tab . "#400487")

    (fg-escape-char-construct . "#8b1030")
    (fg-escape-char-backslash . "#654d0f")

    (fg-lang-error . "#9f004f")
    (fg-lang-warning . "#604f0f")
    (fg-lang-note . "#4040ae")
    (fg-lang-underline-error . "#ef4f54")
    (fg-lang-underline-warning . "#cf9f00")
    (fg-lang-underline-note . "#3f6fef")

    (fg-window-divider-inner . "#888888")
    (fg-window-divider-outer . "#585858")

    (fg-unfocused . "#56576d")

    (fg-docstring . "#2a486a")
    (fg-comment-yellow . "#794319")

    (bg-header . "#e5e5e5") (fg-header . "#2a2a2a")

    (bg-whitespace . "#f5efef") (fg-whitespace . "#624956")

    (bg-diff-heading . "#b7cfe0") (fg-diff-heading . "#041645")
    (bg-diff-added . "#d4fad4") (fg-diff-added . "#004500")
    (bg-diff-added-deuteran . "#daefff") (fg-diff-added-deuteran . "#002044")
    (bg-diff-changed . "#fcefcf") (fg-diff-changed . "#524200")
    (bg-diff-removed . "#ffe8ef") (fg-diff-removed . "#691616")

    (bg-diff-refine-added . "#94cf94") (fg-diff-refine-added . "#002a00")
    (bg-diff-refine-added-deuteran . "#77c0ef") (fg-diff-refine-added-deuteran . "#000035")
    (bg-diff-refine-changed . "#cccf8f") (fg-diff-refine-changed . "#302010")
    (bg-diff-refine-removed . "#daa2b0") (fg-diff-refine-removed . "#400000")

    (bg-diff-focus-added . "#bbeabb") (fg-diff-focus-added . "#002c00")
    (bg-diff-focus-added-deuteran . "#bacfff") (fg-diff-focus-added-deuteran . "#001755")
    (bg-diff-focus-changed . "#ecdfbf") (fg-diff-focus-changed . "#392900")
    (bg-diff-focus-removed . "#efcbcf") (fg-diff-focus-removed . "#4a0000")

    (bg-mark-sel . "#a0f0cf") (fg-mark-sel . "#005040")
    (bg-mark-del . "#ffccbb") (fg-mark-del . "#840040")
    (bg-mark-alt . "#f5d88f") (fg-mark-alt . "#782900"))
  "The entire palette of the `modus-operandi' theme.
Each element has the form (NAME . HEX) with the former as a
symbol and the latter as a string.")

;;;; Modus Vivendi

(defconst modus-themes-vivendi-colors
  '(;; base values
    (bg-main . "#000000") (fg-main . "#ffffff")
    (bg-dim . "#100f10") (fg-dim . "#e0e6f0")
    (bg-alt . "#191a1b") (fg-alt . "#a8a8a8")
    ;; specifically for on/off states and must be combined with
    ;; themselves, though the backgrounds are also meant to be used with
    ;; other "active" values, defined further below; bg-active-accent
    ;; can work as a substitute for bg-active
    (bg-active . "#323232") (fg-active . "#f4f4f4")
    (bg-inactive . "#1e1e1e") (fg-inactive . "#bfc0c4")
    (bg-active-accent . "#2a2a66")
    ;; these special values are intended as alternatives to the base
    ;; values for cases where we need to avoid confusion between the
    ;; highlighted constructs; they must either be used as pairs based
    ;; on their name or each can be combined with {fg,bg}-{main,alt,dim}
    ;; always in accordance with their role as background or foreground
    (bg-special-cold . "#203448") (bg-special-faint-cold . "#0e183a") (fg-special-cold . "#c6eaff")
    (bg-special-mild . "#00322e") (bg-special-faint-mild . "#001f1a") (fg-special-mild . "#bfebe0")
    (bg-special-warm . "#382f27") (bg-special-faint-warm . "#241613") (fg-special-warm . "#f8dec0")
    (bg-special-calm . "#392a48") (bg-special-faint-calm . "#251232") (fg-special-calm . "#fbd6f4")
    ;; foregrounds that can be combined with bg-main, bg-dim, bg-alt
    (red . "#ff8059")
    (red-alt . "#ef8b50")
    (red-alt-other . "#ff9077")
    (red-faint . "#ffa0a0")
    (red-alt-faint . "#f5aa80")
    (red-alt-other-faint . "#ff9fbf")
    (green . "#44bc44")
    (green-alt . "#70b900")
    (green-alt-other . "#00c06f")
    (green-faint . "#78bf78")
    (green-alt-faint . "#99b56f")
    (green-alt-other-faint . "#88bf99")
    (yellow . "#d0bc00")
    (yellow-alt . "#c0c530")
    (yellow-alt-other . "#d3b55f")
    (yellow-faint . "#d2b580")
    (yellow-alt-faint . "#cabf77")
    (yellow-alt-other-faint . "#d0ba95")
    (blue . "#2fafff")
    (blue-alt . "#79a8ff" )
    (blue-alt-other . "#00bcff")
    (blue-faint . "#82b0ec")
    (blue-alt-faint . "#a0acef")
    (blue-alt-other-faint . "#80b2f0")
    (magenta . "#feacd0")
    (magenta-alt . "#f78fe7")
    (magenta-alt-other . "#b6a0ff")
    (magenta-faint . "#e0b2d6")
    (magenta-alt-faint . "#ef9fe4")
    (magenta-alt-other-faint . "#cfa6ff")
    (cyan . "#00d3d0")
    (cyan-alt . "#4ae2f0")
    (cyan-alt-other . "#6ae4b9")
    (cyan-faint . "#90c4ed")
    (cyan-alt-faint . "#a0bfdf")
    (cyan-alt-other-faint . "#a4d0bb")
    ;; these foreground values can only be combined with bg-main and are
    ;; thus not suitable for general purpose highlighting
    (red-intense . "#fe6060")
    (orange-intense . "#fba849")
    (green-intense . "#4fe42f")
    (yellow-intense . "#f0dd60")
    (blue-intense . "#4fafff")
    (magenta-intense . "#ff62d4")
    (purple-intense . "#9f80ff")
    (cyan-intense . "#3fdfd0")
    ;; those foregrounds are meant exclusively for bg-active, bg-inactive
    (red-active . "#ffa7ba")
    (green-active . "#70d73f")
    (yellow-active . "#dbbe5f")
    (blue-active . "#34cfff")
    (magenta-active . "#d5b1ff")
    (cyan-active . "#00d8b4")
    ;; the "subtle" values below be combined with fg-dim, while the
    ;; "intense" should be paired with fg-main
    (red-subtle-bg . "#762422")
    (red-intense-bg . "#a4202a")
    (green-subtle-bg . "#2f4a00")
    (green-intense-bg . "#006800")
    (yellow-subtle-bg . "#604200")
    (yellow-intense-bg . "#874900")
    (blue-subtle-bg . "#10387c")
    (blue-intense-bg . "#2a40b8")
    (magenta-subtle-bg . "#49366e")
    (magenta-intense-bg . "#7042a2")
    (cyan-subtle-bg . "#00415e")
    (cyan-intense-bg . "#005f88")
    ;; those background values must be combined with fg-main and should
    ;; only be used for indicators that are placed on the fringes
    (red-fringe-bg . "#8f1f4b")
    (green-fringe-bg . "#006700")
    (yellow-fringe-bg . "#6f4f00")
    (blue-fringe-bg . "#3f33af")
    (magenta-fringe-bg . "#6f2f89")
    (cyan-fringe-bg . "#004f8f")
    ;; those background values should only be used for graphs or similar
    ;; applications where colored blocks are expected to be positioned
    ;; next to each other
    (red-graph-0-bg . "#b52c2c")
    (red-graph-1-bg . "#702020")
    (green-graph-0-bg . "#4fd100")
    (green-graph-1-bg . "#007800")
    (yellow-graph-0-bg . "#f1e00a")
    (yellow-graph-1-bg . "#b08600")
    (blue-graph-0-bg . "#2fafef")
    (blue-graph-1-bg . "#1f2f8f")
    (magenta-graph-0-bg . "#bf94fe")
    (magenta-graph-1-bg . "#5f509f")
    (cyan-graph-0-bg . "#47dfea")
    (cyan-graph-1-bg . "#00808f")
    ;; the following are for cases where both the foreground and the
    ;; background need to have a similar hue and so must be combined
    ;; with themselves, even though the foregrounds can be paired with
    ;; any of the base backgrounds
    (red-refine-bg . "#77002a") (red-refine-fg . "#ffb9ab")
    (green-refine-bg . "#00422a") (green-refine-fg . "#9ff0cf")
    (yellow-refine-bg . "#693200") (yellow-refine-fg . "#e2d980")
    (blue-refine-bg . "#242679") (blue-refine-fg . "#8ecfff")
    (magenta-refine-bg . "#71206a") (magenta-refine-fg . "#ffcaf0")
    (cyan-refine-bg . "#004065") (cyan-refine-fg . "#8ae4f2")
    ;; the "nuanced" backgrounds can be combined with all of the above
    ;; foregrounds, as well as those included here, while the "nuanced"
    ;; foregrounds can in turn also be combined with bg-main, bg-dim,
    ;; bg-alt
    (red-nuanced-bg . "#2c0614") (red-nuanced-fg . "#ffcccc")
    (green-nuanced-bg . "#001904") (green-nuanced-fg . "#b8e2b8")
    (yellow-nuanced-bg . "#221000") (yellow-nuanced-fg . "#dfdfb0")
    (blue-nuanced-bg . "#0f0e39") (blue-nuanced-fg . "#bfd9ff")
    (magenta-nuanced-bg . "#230631") (magenta-nuanced-fg . "#e5cfef")
    (cyan-nuanced-bg . "#041529") (cyan-nuanced-fg . "#a8e5e5")
    ;; the following are reserved for specific cases
    ;;
    ;; bg-hl-line is between bg-dim and bg-alt, so it should
    ;; work with all accents that cover those two, plus bg-main
    ;;
    ;; bg-hl-alt and bg-hl-alt-intense should only be used when no
    ;; other grayscale or fairly neutral background is available to
    ;; properly draw attention to a given construct
    ;;
    ;; bg-header is between bg-active and bg-inactive, so it
    ;; can be combined with any of the "active" values, plus the
    ;; "special" and base foreground colors
    ;;
    ;; bg-paren-match, bg-paren-match-intense, bg-region,
    ;; bg-region-accent and bg-tab-active must be combined with fg-main,
    ;; while bg-tab-inactive should be combined with fg-dim, whereas
    ;; bg-tab-inactive-alt goes together with fg-main
    ;;
    ;; bg-completion-* and bg-char-* variants are meant to be combined
    ;; with fg-main
    ;;
    ;; fg-escape-char-construct and fg-escape-char-backslash can
    ;; be combined bg-main, bg-dim, bg-alt
    ;;
    ;; fg-lang-error, fg-lang-warning, fg-lang-note can be
    ;; combined with bg-main, bg-dim, bg-alt
    ;;
    ;; fg-mark-sel, fg-mark-del, fg-mark-alt can be combined
    ;; with bg-main, bg-dim, bg-alt, bg-hl-line
    ;;
    ;; fg-unfocused must be combined with bg-main
    ;;
    ;; fg-docstring, fg-comment-yellow can be combined with
    ;; bg-main, bg-dim, bg-alt
    ;;
    ;; the window divider colors apply to faces with just an fg value
    ;;
    ;; all pairs are combinable with themselves
    (bg-hl-line . "#151823")
    (bg-hl-line-intense . "#292929")
    (bg-hl-line-intense-accent . "#002a4f")
    (bg-hl-alt . "#181732")
    (bg-hl-alt-intense . "#282e46")
    (bg-paren-match . "#6f3355")
    (bg-paren-match-intense . "#7416b5")
    (bg-paren-expression . "#221044")
    (bg-region . "#3c3c3c")
    (bg-region-accent . "#4f3d88")
    (bg-region-accent-subtle . "#240f55")

    (bg-completion . "#142f69")
    (bg-completion-subtle . "#0e194b")

    (bg-char-0 . "#0050af")
    (bg-char-1 . "#7f1f7f")
    (bg-char-2 . "#625a00")

    (bg-tab-active . "#0e0e0e")
    (bg-tab-inactive . "#424242")
    (bg-tab-inactive-accent . "#35398f")
    (bg-tab-inactive-alt . "#595959")
    (bg-tab-inactive-alt-accent . "#505588")

    (red-tab . "#ffc0bf")
    (green-tab . "#88ef88")
    (yellow-tab . "#d2e580")
    (orange-tab . "#f5ca80")
    (blue-tab . "#92d9ff")
    (cyan-tab . "#60e7e0")
    (magenta-tab . "#ffb8ff")
    (purple-tab . "#cfcaff")

    (fg-escape-char-construct . "#e7a59a")
    (fg-escape-char-backslash . "#abab00")

    (fg-lang-error . "#ef8690")
    (fg-lang-warning . "#b0aa00")
    (fg-lang-note . "#9d9def")
    (fg-lang-underline-error . "#ff4a6f")
    (fg-lang-underline-warning . "#d0de00")
    (fg-lang-underline-note . "#5f6fff")

    (fg-window-divider-inner . "#646464")
    (fg-window-divider-outer . "#969696")

    (fg-unfocused . "#93959b")

    (fg-docstring . "#b0d6f5")
    (fg-comment-yellow . "#d0a070")

    (bg-header . "#212121") (fg-header . "#dddddd")

    (bg-whitespace . "#101424") (fg-whitespace . "#aa9e9f")

    (bg-diff-heading . "#304466") (fg-diff-heading . "#dae7ff")
    (bg-diff-added . "#0a280a") (fg-diff-added . "#94ba94")
    (bg-diff-added-deuteran . "#001a3f") (fg-diff-added-deuteran . "#c4cdf2")
    (bg-diff-changed . "#2a2000") (fg-diff-changed . "#b0ba9f")
    (bg-diff-removed . "#40160f") (fg-diff-removed . "#c6adaa")

    (bg-diff-refine-added . "#005a36") (fg-diff-refine-added . "#e0f6e0")
    (bg-diff-refine-added-deuteran . "#234f8f") (fg-diff-refine-added-deuteran . "#dde4ff")
    (bg-diff-refine-changed . "#585800") (fg-diff-refine-changed . "#ffffcc")
    (bg-diff-refine-removed . "#852828") (fg-diff-refine-removed . "#ffd9eb")

    (bg-diff-focus-added . "#1d3c25") (fg-diff-focus-added . "#b4ddb4")
    (bg-diff-focus-added-deuteran . "#003959") (fg-diff-focus-added-deuteran . "#bfe4ff")
    (bg-diff-focus-changed . "#424200") (fg-diff-focus-changed . "#d0daaf")
    (bg-diff-focus-removed . "#601f29") (fg-diff-focus-removed . "#eebdba")

    (bg-mark-sel . "#002f2f") (fg-mark-sel . "#60cfa2")
    (bg-mark-del . "#5a0000") (fg-mark-del . "#ff99aa")
    (bg-mark-alt . "#3f2210") (fg-mark-alt . "#f0aa20"))
  "The entire palette of the `modus-vivendi' theme.
Each element has the form (NAME . HEX) with the former as a
symbol and the latter as a string.")



;;; Custom faces

;; These faces are used internally to ensure consistency between various
;; groups and to streamline the evaluation of relevant customization
;; options.
(defface modus-themes-subtle-red nil
  "Subtle red background combined with a dimmed foreground.
This is used for general purpose highlighting, mostly in buffers
or for completion interfaces.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-subtle-green nil
  "Subtle green background combined with a dimmed foreground.
This is used for general purpose highlighting, mostly in buffers
or for completion interfaces.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-subtle-yellow nil
  "Subtle yellow background combined with a dimmed foreground.
This is used for general purpose highlighting, mostly in buffers
or for completion interfaces.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-subtle-blue nil
  "Subtle blue background combined with a dimmed foreground.
This is used for general purpose highlighting, mostly in buffers
or for completion interfaces.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-subtle-magenta nil
  "Subtle magenta background combined with a dimmed foreground.
This is used for general purpose highlighting, mostly in buffers
or for completion interfaces.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-subtle-cyan nil
  "Subtle cyan background combined with a dimmed foreground.
This is used for general purpose highlighting, mostly in buffers
or for completion interfaces.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-subtle-neutral nil
  "Subtle gray background combined with a dimmed foreground.
This is used for general purpose highlighting, mostly in buffers
or for completion interfaces.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-intense-red nil
  "Intense red background combined with the main foreground.
This is used for general purpose highlighting, mostly in buffers
or for completion interfaces.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-intense-green nil
  "Intense green background combined with the main foreground.
This is used for general purpose highlighting, mostly in buffers
or for completion interfaces.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-intense-yellow nil
  "Intense yellow background combined with the main foreground.
This is used for general purpose highlighting, mostly in buffers
or for completion interfaces.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-intense-blue nil
  "Intense blue background combined with the main foreground.
This is used for general purpose highlighting, mostly in buffers
or for completion interfaces.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-intense-magenta nil
  "Intense magenta background combined with the main foreground.
This is used for general purpose highlighting, mostly in buffers
or for completion interfaces.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-intense-cyan nil
  "Intense cyan background combined with the main foreground.
This is used for general purpose highlighting, mostly in buffers
or for completion interfaces.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-intense-neutral nil
  "Intense gray background combined with the main foreground.
This is used for general purpose highlighting, mostly in buffers
or for completion interfaces.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-refine-red nil
  "Combination of accented red background and foreground.
This is used for general purpose highlighting, mostly in buffers
or for completion interfaces.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-refine-green nil
  "Combination of accented green background and foreground.
This is used for general purpose highlighting, mostly in buffers
or for completion interfaces.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-refine-yellow nil
  "Combination of accented yellow background and foreground.
This is used for general purpose highlighting, mostly in buffers
or for completion interfaces.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-refine-blue nil
  "Combination of accented blue background and foreground.
This is used for general purpose highlighting, mostly in buffers
or for completion interfaces.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-refine-magenta nil
  "Combination of accented magenta background and foreground.
This is used for general purpose highlighting, mostly in buffers
or for completion interfaces.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-refine-cyan nil
  "Combination of accented cyan background and foreground.
This is used for general purpose highlighting, mostly in buffers
or for completion interfaces.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-active-red nil
  "A red background meant for use on the mode line or similar.
This is combined with the mode lines primary foreground value.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-active-green nil
  "A green background meant for use on the mode line or similar.
This is combined with the mode lines primary foreground value.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-active-yellow nil
  "A yellow background meant for use on the mode line or similar.
This is combined with the mode lines primary foreground value.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-active-blue nil
  "A blue background meant for use on the mode line or similar.
This is combined with the mode lines primary foreground value.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-active-magenta nil
  "A magenta background meant for use on the mode line or similar.
This is combined with the mode lines primary foreground value.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-active-cyan nil
  "A cyan background meant for use on the mode line or similar.
This is combined with the mode lines primary foreground value.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-fringe-red nil
  "A red background meant for use on the fringe or similar.
This is combined with the main foreground value.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-fringe-green nil
  "A green background meant for use on the fringe or similar.
This is combined with the main foreground value.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-fringe-yellow nil
  "A yellow background meant for use on the fringe or similar.
This is combined with the main foreground value.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-fringe-blue nil
  "A blue background meant for use on the fringe or similar.
This is combined with the main foreground value.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-fringe-magenta nil
  "A magenta background meant for use on the fringe or similar.
This is combined with the main foreground value.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-fringe-cyan nil
  "A cyan background meant for use on the fringe or similar.
This is combined with the main foreground value.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-nuanced-red nil
  "A nuanced red background.
This does not specify a foreground of its own.  Instead it is
meant to serve as the backdrop for elements such as Org blocks,
headings, and any other surface that needs to retain the colors
on display.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-nuanced-green nil
  "A nuanced green background.
This does not specify a foreground of its own.  Instead it is
meant to serve as the backdrop for elements such as Org blocks,
headings, and any other surface that needs to retain the colors
on display.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-nuanced-yellow nil
  "A nuanced yellow background.
This does not specify a foreground of its own.  Instead it is
meant to serve as the backdrop for elements such as Org blocks,
headings, and any other surface that needs to retain the colors
on display.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-nuanced-blue nil
  "A nuanced blue background.
This does not specify a foreground of its own.  Instead it is
meant to serve as the backdrop for elements such as Org blocks,
headings, and any other surface that needs to retain the colors
on display.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-nuanced-magenta nil
  "A nuanced magenta background.
This does not specify a foreground of its own.  Instead it is
meant to serve as the backdrop for elements such as Org blocks,
headings, and any other surface that needs to retain the colors
on display.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-nuanced-cyan nil
  "A nuanced cyan background.
This does not specify a foreground of its own.  Instead it is
meant to serve as the backdrop for elements such as Org blocks,
headings, and any other surface that needs to retain the colors
on display.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-special-cold nil
  "Combines the special cold background and foreground values.
This is intended for cases when a neutral gray background is not
suitable and where a combination of more saturated colors would
not be appropriate.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-special-mild nil
  "Combines the special mild background and foreground values.
This is intended for cases when a neutral gray background is not
suitable and where a combination of more saturated colors would
not be appropriate.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-special-warm nil
  "Combines the special warm background and foreground values.
This is intended for cases when a neutral gray background is not
suitable and where a combination of more saturated colors would
not be appropriate.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-special-calm nil
  "Combines the special calm background and foreground values.
This is intended for cases when a neutral gray background is not
suitable and where a combination of more saturated colors would
not be appropriate.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-diff-added nil
  "Combines green colors for the added state in diffs.
The applied colors are contingent on the value assigned to
`modus-themes-diffs'.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-diff-changed nil
  "Combines yellow colors for the changed state in diffs.
The applied colors are contingent on the value assigned to
`modus-themes-diffs'.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-diff-removed nil
  "Combines red colors for the removed state in diffs.
The applied colors are contingent on the value assigned to
`modus-themes-diffs'.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-diff-refine-added nil
  "Combines green colors for word-wise added state in diffs.
The applied colors are contingent on the value assigned to
`modus-themes-diffs'.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-diff-refine-changed nil
  "Combines yellow colors for word-wise changed state in diffs.
The applied colors are contingent on the value assigned to
`modus-themes-diffs'.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-diff-refine-removed nil
  "Combines red colors for word-wise removed state in diffs.
The applied colors are contingent on the value assigned to
`modus-themes-diffs'.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-diff-focus-added nil
  "Combines green colors for the focused added state in diffs.
The applied colors are contingent on the value assigned to
`modus-themes-diffs'.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-diff-focus-changed nil
  "Combines yellow colors for the focused changed state in diffs.
The applied colors are contingent on the value assigned to
`modus-themes-diffs'.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-diff-focus-removed nil
  "Combines red colors for the focused removed state in diffs.
The applied colors are contingent on the value assigned to
`modus-themes-diffs'.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-diff-heading nil
  "Combines blue colors for the diff hunk heading.
The applied colors are contingent on the value assigned to
`modus-themes-diffs'.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-pseudo-header nil
  "Generic style for some elements that function like headings.
The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-mark-alt nil
  "Combines yellow colors for marking special lines.
This is intended for use in modes such as Dired, Ibuffer, Proced.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-mark-del nil
  "Combines red colors for marking deletable lines.
This is intended for use in modes such as Dired, Ibuffer, Proced.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-mark-sel nil
  "Combines green colors for marking lines.
This is intended for use in modes such as Dired, Ibuffer, Proced.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-mark-symbol nil
  "Applies a blue color and other styles for mark indicators.
This is intended for use in modes such as Dired, Ibuffer, Proced.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-heading-0 nil
  "General purpose face for use as the document's title.
The exact attributes assigned to this face are contingent on the
values assigned to the `modus-themes-headings' variable.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-heading-1 nil
  "General purpose face for use in headings level 1.
The exact attributes assigned to this face are contingent on the
values assigned to the `modus-themes-headings' variable.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-heading-2 nil
  "General purpose face for use in headings level 2.
The exact attributes assigned to this face are contingent on the
values assigned to the `modus-themes-headings' variable.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-heading-3 nil
  "General purpose face for use in headings level 3.
The exact attributes assigned to this face are contingent on the
values assigned to the `modus-themes-headings' variable.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-heading-4 nil
  "General purpose face for use in headings level 4.
The exact attributes assigned to this face are contingent on the
values assigned to the `modus-themes-headings' variable.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-heading-5 nil
  "General purpose face for use in headings level 5.
The exact attributes assigned to this face are contingent on the
values assigned to the `modus-themes-headings' variable.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-heading-6 nil
  "General purpose face for use in headings level 6.
The exact attributes assigned to this face are contingent on the
values assigned to the `modus-themes-headings' variable.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-heading-7 nil
  "General purpose face for use in headings level 7.
The exact attributes assigned to this face are contingent on the
values assigned to the `modus-themes-headings' variable.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-heading-8 nil
  "General purpose face for use in headings level 8.
The exact attributes assigned to this face are contingent on the
values assigned to the `modus-themes-headings' variable.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-hl-line nil
  "General purpose face for the current line.
The exact attributes assigned to this face are contingent on the
values assigned to the `modus-themes-hl-line' variable.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-bold nil
  "Generic face for applying a conditional bold weight.
This behaves in accordance with `modus-themes-bold-constructs'.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-slant nil
  "Generic face for applying a conditional slant (italics).
This behaves in accordance with `modus-themes-italic-constructs'.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-variable-pitch nil
  "Generic face for applying a conditional `variable-pitch'.
This behaves in accordance with `modus-themes-mixed-fonts' and/or
`modus-themes-variable-pitch-ui'.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-fixed-pitch nil
  "Generic face for applying a conditional `fixed-pitch'.
This behaves in accordance with `modus-themes-mixed-fonts'.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-ui-variable-pitch nil
  "Face for `modus-themes-variable-pitch-ui'.
The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-lang-note nil
  "Generic face for linter or spell checker notes.
The exact attributes and color combinations are controlled by
`modus-themes-lang-checkers'.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-lang-warning nil
  "Generic face for linter or spell checker warnings.
The exact attributes and color combinations are controlled by
`modus-themes-lang-checkers'.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-lang-error nil
  "Generic face for linter or spell checker errors.
The exact attributes and color combinations are controlled by
`modus-themes-lang-checkers'.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-reset-soft nil
  "Generic face to set most face properties to nil.

This is intended to be inherited by faces that should not retain
properties from their context (e.g. an overlay over an underlined
text should not be underlined as well) yet still blend in.  Also
see `modus-themes-reset-hard'.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-reset-hard nil
  "Generic face to set all face properties to nil.

This is intended to be inherited by faces that should not retain
properties from their context (e.g. an overlay over an underlined
text should not be underlined as well) and not blend in.  Also
see `modus-themes-reset-soft'.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-key-binding nil
  "Generic face for key bindings.
The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-search-success nil
  "Generic face for successful search.
The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-search-success-modeline nil
  "Generic mode line indicator for successful search.
The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-search-success-lazy nil
  "Generic face for successful, lazily highlighted search.
The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-prompt nil
  "Generic face for command prompts.
The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

;; "Grue" is "green" and "blue".
(defface modus-themes-grue nil
  "Generic face for `modus-themes-deuteranopia' foreground.
The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-grue-active nil
  "Face for `modus-themes-deuteranopia' active foreground.
The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-grue-nuanced nil
  "Face for `modus-themes-deuteranopia' nuanced foreground.
The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-grue-background-active nil
  "Face for `modus-themes-deuteranopia' active background.
The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-grue-background-intense nil
  "Face for `modus-themes-deuteranopia' intense background.
The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-grue-background-subtle nil
  "Face for `modus-themes-deuteranopia' subtle background.
The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-grue-background-refine nil
  "Face for `modus-themes-deuteranopia' refined background.
The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-link-symlink nil
  "Face for `modus-themes-links' symbolic link.
The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-link-broken nil
  "Face for `modus-themes-links' broken link.
The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-tab-backdrop nil
  "Face of backdrop in tabbed interfaces.
The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-tab-active nil
  "Face of active tab.
The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-tab-inactive nil
  "Face of inactive tab.
The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-markup-code nil
  "Face of inline code markup.
The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-markup-macro nil
  "Face of macro markup.
The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-markup-verbatim nil
  "Face of verbatim markup.
The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-completion-selected nil
  "Face for current selection in completion UIs.
The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-completion-selected-popup nil
  "Face for current selection in completion UI popups.
The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-completion-match-0 nil
  "Face for completions matches 0.
The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-completion-match-1 nil
  "Face for completions matches 1.
The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-completion-match-2 nil
  "Face for completions matches 2.
The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-completion-match-3 nil
  "Face for completions matches 3.
The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-box-button nil
  "Face for widget buttons (e.g. in the Custom UI).
The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)

(defface modus-themes-box-button-pressed nil
  "Face for pressed widget buttons (e.g. in the Custom UI).
The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-themes-faces)



;;; Customization variables

(defcustom modus-themes-inhibit-reload t
  "Control theme reload when setting options with Customize.

By default, customizing a theme-related user option through the
Custom interfaces or with `customize-set-variable' will not
reload the currently active Modus theme.

Enable this behavior by setting this variable to nil."
  :group 'modus-themes
  :package-version '(modus-themes . "1.5.0")
  :version "28.1"
  :type 'boolean
  :link '(info-link "(modus-themes) Custom reload theme"))

(defun modus-themes--set-option (sym val)
  "Custom setter for theme related user options.
Will set SYM to VAL, and reload the current theme, unless
`modus-themes-inhibit-reload' is non-nil."
  (set-default sym val)
  (unless (or modus-themes-inhibit-reload
              ;; Check if a theme is being loaded, in which case we
              ;; don't want to reload a theme if the setter is
              ;; invoked. `custom--inhibit-theme-enable' is set to nil
              ;; by `enable-theme'.
              (null (bound-and-true-p custom--inhibit-theme-enable)))
    (let ((modus-themes-inhibit-reload t))
      (pcase (modus-themes--current-theme)
        ('modus-operandi (modus-themes-load-operandi))
        ('modus-vivendi (modus-themes-load-vivendi))))))

(defcustom modus-themes-operandi-color-overrides nil
  "Override colors in the Modus Operandi palette.

For form, see `modus-themes-operandi-colors'."
  :group 'modus-themes
  :package-version '(modus-themes . "1.1.0")
  :version "28.1"
  :type '(alist :key-type symbol :value-type color)
  :set #'modus-themes--set-option
  :initialize #'custom-initialize-default
  :link '(info-link "(modus-themes) Override colors"))

(defcustom modus-themes-vivendi-color-overrides nil
  "Override colors in the Modus Vivendi palette.

For form, see `modus-themes-vivendi-colors'."
  :group 'modus-themes
  :package-version '(modus-themes . "1.1.0")
  :version "28.1"
  :type '(alist :key-type symbol :value-type color)
  :set #'modus-themes--set-option
  :initialize #'custom-initialize-default
  :link '(info-link "(modus-themes) Override colors"))

;; The byte compiler complains when a defcustom isn't a top level form
(let* ((names (mapcar (lambda (pair)
                        (symbol-name (car pair)))
                      modus-themes-operandi-colors))
       (colors (mapcar #'intern (sort names #'string<))))
  (put 'modus-themes-operandi-color-overrides
       'custom-options (copy-sequence colors))
  (put 'modus-themes-vivendi-color-overrides
       'custom-options (copy-sequence colors)))

(defvaralias 'modus-themes-slanted-constructs 'modus-themes-italic-constructs)

(defcustom modus-themes-italic-constructs nil
  "Use italic font forms in more code constructs."
  :group 'modus-themes
  :package-version '(modus-themes . "1.5.0")
  :version "28.1"
  :type 'boolean
  :set #'modus-themes--set-option
  :initialize #'custom-initialize-default
  :link '(info-link "(modus-themes) Italic constructs"))

(defcustom modus-themes-bold-constructs nil
  "Use bold text in more code constructs."
  :group 'modus-themes
  :package-version '(modus-themes . "1.0.0")
  :version "28.1"
  :type 'boolean
  :set #'modus-themes--set-option
  :initialize #'custom-initialize-default
  :link '(info-link "(modus-themes) Bold constructs"))

(defcustom modus-themes-variable-pitch-ui nil
  "Use proportional fonts (variable-pitch) in UI elements.
This includes the mode line, header line, tab bar, and tab line."
  :group 'modus-themes
  :package-version '(modus-themes . "1.1.0")
  :version "28.1"
  :type 'boolean
  :set #'modus-themes--set-option
  :initialize #'custom-initialize-default
  :link '(info-link "(modus-themes) UI typeface"))

(defcustom modus-themes-mixed-fonts nil
  "Non-nil to enable inheritance from `fixed-pitch' in some faces.

This is done to allow spacing-sensitive constructs, such as Org
tables and code blocks, to remain monospaced when users opt for
something like the command `variable-pitch-mode'.

Users may need to explicitly configure the font family of
`fixed-pitch' in order to get a consistent experience."
  :group 'modus-themes
  :package-version '(modus-themes . "1.7.0")
  :version "29.1"
  :type 'boolean
  :set #'modus-themes--set-option
  :initialize #'custom-initialize-default
  :link '(info-link "(modus-themes) Mixed fonts"))

(defcustom modus-themes-intense-mouseovers nil
  "When non-nil use more intense style for mouse hover effects.

This affects the generic `highlight' face which, strictly
speaking, is not limited to mouse usage."
  :group 'modus-themes
  :package-version '(modus-themes . "2.3.0")
  :version "29.1"
  :type 'boolean
  :set #'modus-themes--set-option
  :initialize #'custom-initialize-default
  :link '(info-link "(modus-themes) Mouse hover effects"))

(defconst modus-themes--headings-choice
  '(set :tag "Properties" :greedy t
        (const :tag "Background color" background)
        (const :tag "Proportionately spaced font (variable-pitch)" variable-pitch)
        (const :tag "Overline" overline)
        (choice :tag "Font weight (must be supported by the typeface)"
                (const :tag "Bold (default)" nil)
                (const :tag "Thin" thin)
                (const :tag "Ultra-light" ultralight)
                (const :tag "Extra-light" extralight)
                (const :tag "Light" light)
                (const :tag "Semi-light" semilight)
                (const :tag "Regular" regular)
                (const :tag "Medium" medium)
                (const :tag "Semi-bold" semibold)
                (const :tag "Extra-bold" extrabold)
                (const :tag "Ultra-bold" ultrabold))
        (radio :tag "Height"
               (float :tag "Floating point to adjust height by")
               (cons :tag "Cons cell of `(height . FLOAT)'"
                     (const :tag "The `height' key (constant)" height)
                     (float :tag "Floating point")))
        (choice :tag "Colors"
                (const :tag "Subtle colors" nil)
                (const :tag "Rainbow colors" rainbow)
                (const :tag "Monochrome" monochrome)))
  "Refer to the doc string of `modus-themes-headings'.
This is a helper variable intended for internal use.")

(defcustom modus-themes-headings nil
  "Heading styles with optional list of values for levels 0-8.

This is an alist that accepts a (key . list-of-values)
combination.  The key is either a number, representing the
heading's level (0-8) or t, which pertains to the fallback style.

Level 0 is a special heading: it is used for what counts as a
document title or equivalent, such as the #+title construct we
find in Org files.  Levels 1-8 are regular headings.

The list of values covers symbols that refer to properties, as
described below.  Here is a complete sample, followed by a
presentation of all available properties:

    (setq modus-themes-headings
          (quote ((1 . (background overline variable-pitch 1.5))
                  (2 . (overline rainbow 1.3))
                  (3 . (overline 1.1))
                  (t . (monochrome)))))

By default (a nil value for this variable), all headings have a
bold typographic weight, use a desaturated text color, have a
font family that is the same as the `default' face (typically
monospaced), and a height that is equal to the `default' face's
height.

A `rainbow' property makes the text color more saturated.

An `overline' property draws a line above the area of the
heading.

A `background' property applies a subtle tinted color to the
background of the heading.

A `monochrome' property makes the heading the same as the base
color, which is that of the `default' face's foreground.  When
`background' is also set, `monochrome' changes its color to gray.
If both `monochrome' and `rainbow' are set, the former takes
precedence.

A `variable-pitch' property changes the font family of the
heading to that of the `variable-pitch' face (normally a
proportionately spaced typeface).

The symbol of a weight attribute adjusts the font of the heading
accordingly, such as `light', `semibold', etc.  Valid symbols are
defined in the variable `modus-themes-weights'.  The absence of a
weight means that bold will be used by virtue of inheriting the
`bold' face (check the manual for tweaking bold and italic
faces).  For backward compatibility, the `no-bold' value is
accepted, though users are encouraged to specify a `regular'
weight instead.

A number, expressed as a floating point (e.g. 1.5), adjusts the
height of the heading to that many times the base font size.  The
default height is the same as 1.0, though it need not be
explicitly stated.  Instead of a floating point, an acceptable
value can be in the form of a cons cell like (height . FLOAT)
or (height FLOAT), where FLOAT is the given number.

Combinations of any of those properties are expressed as a list,
like in these examples:

    (semibold)
    (rainbow background)
    (overline monochrome semibold 1.3)
    (overline monochrome semibold (height 1.3)) ; same as above
    (overline monochrome semibold (height . 1.3)) ; same as above

The order in which the properties are set is not significant.

In user configuration files the form may look like this:

    (setq modus-themes-headings
          (quote ((1 . (background overline rainbow 1.5))
                  (2 . (background overline 1.3))
                  (t . (overline semibold)))))

When defining the styles per heading level, it is possible to
pass a non-nil value (t) instead of a list of properties.  This
will retain the original aesthetic for that level.  For example:

    (setq modus-themes-headings
          (quote ((1 . t)           ; keep the default style
                  (2 . (background overline))
                  (t . (rainbow))))) ; style for all other headings

    (setq modus-themes-headings
          (quote ((1 . (background overline))
                  (2 . (rainbow semibold))
                  (t . t)))) ; default style for all other levels

For Org users, the extent of the heading depends on the variable
`org-fontify-whole-heading-line'.  This affects the `overline'
and `background' properties.  Depending on the version of Org,
there may be others, such as `org-fontify-done-headline'."
  :group 'modus-themes
  :package-version '(modus-themes . "2.5.0")
  :version "29.1"
  :type `(alist
          :options ,(mapcar (lambda (el)
                              (list el modus-themes--headings-choice))
                            '(0 1 2 3 4 5 6 7 8 t))
          :key-type symbol
          :value-type ,modus-themes--headings-choice)
  :set #'modus-themes--set-option
  :initialize #'custom-initialize-default
  :link '(info-link "(modus-themes) Heading styles"))

(defcustom modus-themes-org-agenda nil
  "Control the style of individual Org agenda constructs.

This is an alist that accepts a (key . value) combination.  Here
is a sample, followed by a description of all possible
combinations:

    (setq modus-themes-org-agenda
          (quote ((header-block . (variable-pitch 1.5 semibold))
                  (header-date . (grayscale workaholic bold-today 1.2))
                  (event . (accented italic varied))
                  (scheduled . uniform)
                  (habit . traffic-light))))

A `header-block' key applies to elements that concern the
headings which demarcate blocks in the structure of the agenda.
By default (a nil value) those are rendered in a bold typographic
weight, plus a height that is slightly taller than the default
font size.  Acceptable values come in the form of a list that can
include either or both of those properties:

- `variable-pitch' to use a proportionately spaced typeface;

- A number as a floating point (e.g. 1.5) to set the height of
  the text to that many times the default font height.  A float
  of 1.0 or the symbol `no-scale' have the same effect of making
  the font the same height as the rest of the buffer.  When
  neither a number nor `no-scale' are present, the default is a
  small increase in height (a value of 1.15).

  Instead of a floating point, an acceptable value can be in the
  form of a cons cell like (height . FLOAT) or (height FLOAT),
  where FLOAT is the given number.

- The symbol of a weight attribute adjusts the font of the
  heading accordingly, such as `light', `semibold', etc.  Valid
  symbols are defined in the variable `modus-themes-weights'.
  The absence of a weight means that bold will be used by virtue
  of inheriting the `bold' face (check the manual for tweaking
  bold and italic faces).

In case both a number and `no-scale' are in the list, the latter
takes precedence.  If two numbers are specified, the first one is
applied.

Example usage:

    (header-block . nil)
    (header-block . (1.5))
    (header-block . (no-scale))
    (header-block . (variable-pitch 1.5))
    (header-block . (variable-pitch 1.5 semibold))

A `header-date' key covers date headings.  Dates use only a
foreground color by default (a nil value), with weekdays and
weekends having a slight difference in hueness.  The current date
has an added gray background.  This key accepts a list of values
that can include any of the following properties:

- `grayscale' to make weekdays use the main foreground color and
  weekends a more subtle gray;

- `workaholic' to make weekdays and weekends look the same in
  terms of color;

- `bold-today' to apply a bold typographic weight to the current
  date;

- `bold-all' to render all date headings in a bold weight;

- `underline-today' applies an underline to the current date
  while removing the background it has by default;

- A number as a floating point (e.g. 1.2) to set the height of
  the text to that many times the default font height.  The
  default is the same as the base font height (the equivalent of
  1.0).  Instead of a floating point, an acceptable value can be
  in the form of a cons cell like (height . FLOAT) or (height
  FLOAT), where FLOAT is the given number.

For example:

    (header-date . nil)
    (header-date . (workaholic))
    (header-date . (grayscale bold-all))
    (header-date . (grayscale workaholic))
    (header-date . (grayscale workaholic bold-today))
    (header-date . (grayscale workaholic bold-today 1.2))

An `event' key covers (i) headings with a plain time stamp that
are shown on the agenda, also known as events, (ii) entries
imported from the diary, and (iii) other items that derive from a
symbolic expression or sexp (phases of the moon, holidays, etc.).
By default all those look the same and have a subtle foreground
color (the default is a nil value or an empty list).  This key
accepts a list of properties.  Those are:

- `accented' applies an accent value to the event's foreground,
  replacing the original gray.  It makes all entries stand out more.
- `italic' adds a slant to the font's forms (italic or oblique
  forms, depending on the typeface).
- `varied' differentiates between events with a plain time stamp
  and entries that are generated from either the diary or a
  symbolic expression.  It generally puts more emphasis on
  events.  When `varied' is combined with `accented', it makes
  only events use an accent color, while diary/sexp entries
  retain their original subtle foreground.  When `varied' is used
  in tandem with `italic', it applies a slant only to diary and
  sexp entries, not events.  And when `varied' is the sole
  property passed to the `event' key, it has the same meaning as
  the list (italic varied).  The combination of `varied',
  `accented', `italic' covers all of the aforementioned cases.

For example:

    (event . nil)
    (event . (italic))
    (event . (accented italic))
    (event . (accented italic varied))

A `scheduled' key applies to tasks with a scheduled date.  By
default (a nil value), these use varying shades of yellow to
denote (i) a past or current date and (ii) a future date.  Valid
values are symbols:

- nil (default);
- `uniform' to make all scheduled dates the same color;
- `rainbow' to use contrasting colors for past, present, future
  scheduled dates.

For example:

    (scheduled . nil)
    (scheduled . uniform)
    (scheduled . rainbow)

A `habit' key applies to the `org-habit' graph.  All possible
value are passed as a symbol.  Those are:

- The default (nil) is meant to conform with the original
  aesthetic of `org-habit'.  It employs all four color codes that
  correspond to the org-habit states---clear, ready, alert, and
  overdue---while distinguishing between their present and future
  variants.  This results in a total of eight colors in use: red,
  yellow, green, blue, in tinted and shaded versions.  They cover
  the full set of information provided by the `org-habit'
  consistency graph.

- `simplified' is like the default except that it removes the
  dichotomy between current and future variants by applying
  uniform color-coded values.  It applies a total of four colors:
  red, yellow, green, blue.  They produce a simplified
  consistency graph that is more legible (or less \"busy\") than
  the default.  The intent is to shift focus towards the
  distinction between the four states of a habit task, rather
  than each state's present/future outlook.

- `traffic-light' further reduces the available colors to red,
  yellow, and green.  As in `simplified', present and future
  variants appear uniformly, but differently from it, the CLEAR
  state is rendered in a green hue, instead of the original blue.
  This is meant to capture the use-case where a habit task being
  too early is less important than it being too late.  The
  difference between READY and CLEAR states is attenuated by
  painting both of them using shades of green.  This option thus
  highlights the alert and overdue states.

- When `modus-themes-deuteranopia' is non-nil the exact style of
  the habit graph adapts to the needs of users with red-green
  color deficiency by substituting every instance of green with
  blue or cyan (depending on the specifics).

For example:

    (habit . nil)
    (habit . simplified)
    (habit . traffic-light)"
  :group 'modus-themes
  :package-version '(modus-themes . "2.3.0")
  :version "29.1"
  :type '(set
          (cons :tag "Block header"
                (const header-block)
                (set :tag "Header presentation" :greedy t
                     (choice :tag "Font style"
                             (const :tag "Use the original typeface (default)" nil)
                             (const :tag "Use `variable-pitch' font" variable-pitch))
                     (choice :tag "Font weight (must be supported by the typeface)"
                             (const :tag "Bold (default)" nil)
                             (const :tag "Thin" thin)
                             (const :tag "Ultra-light" ultralight)
                             (const :tag "Extra-light" extralight)
                             (const :tag "Light" light)
                             (const :tag "Semi-light" semilight)
                             (const :tag "Regular" regular)
                             (const :tag "Medium" medium)
                             (const :tag "Semi-bold" semibold)
                             (const :tag "Extra-bold" extrabold)
                             (const :tag "Ultra-bold" ultrabold))
                     (radio :tag "Scaling"
                             (const :tag "Slight increase in height (default)" nil)
                             (const :tag "Do not scale" no-scale)
                             (radio :tag "Number (float) to adjust height by"
                                    (float :tag "Just the number")
                                    (cons :tag "Cons cell of `(height . FLOAT)'"
                                          (const :tag "The `height' key (constant)" height)
                                          (float :tag "Floating point"))))))
          (cons :tag "Date header" :greedy t
                (const header-date)
                (set :tag "Header presentation" :greedy t
                     (const :tag "Use grayscale for date headers" grayscale)
                     (const :tag "Do not differentiate weekdays from weekends" workaholic)
                     (const :tag "Make today bold" bold-today)
                     (const :tag "Make all dates bold" bold-all)
                     (const :tag "Make today underlined; remove the background" underline-today)
                     (radio :tag "Number (float) to adjust height by"
                                    (float :tag "Just the number")
                                    (cons :tag "Cons cell of `(height . FLOAT)'"
                                          (const :tag "The `height' key (constant)" height)
                                          (float :tag "Floating point")))))
          (cons :tag "Event entry" :greedy t
                (const event)
                (set :tag "Text presentation" :greedy t
                     (const :tag "Apply an accent color" accented)
                     (const :tag "Italic font slant (oblique forms)" italic)
                     (const :tag "Differentiate events from diary/sexp entries" varied)))
          (cons :tag "Scheduled tasks"
                (const scheduled)
                (choice (const :tag "Yellow colors to distinguish current and future tasks (default)" nil)
                        (const :tag "Uniform subtle warm color for all scheduled tasks" uniform)
                        (const :tag "Rainbow-colored scheduled tasks" rainbow)))
          (cons :tag "Habit graph"
                (const habit)
                (choice (const :tag "Follow the original design of `org-habit' (default)" nil)
                        (const :tag "Do not distinguish between present and future variants" simplified)
                        (const :tag "Use only red, yellow, green" traffic-light))))
  :set #'modus-themes--set-option
  :initialize #'custom-initialize-default
  :link '(info-link "(modus-themes) Org agenda"))

(defcustom modus-themes-fringes 'subtle
  "Control the visibility of fringes.

When the value is nil, do not apply a distinct background color.

With a value of `subtle' use a gray background color that is
visible yet close to the main background color.

With `intense' use a more pronounced gray background color."
  :group 'modus-themes
  :package-version '(modus-themes . "3.0.0")
  :version "29.1"
  :type '(choice
          (const :format "[%v] %t\n" :tag "No visible fringes" nil)
          (const :format "[%v] %t\n" :tag "Subtle gray background" subtle)
          (const :format "[%v] %t\n" :tag "Intense gray background" intense))
  :set #'modus-themes--set-option
  :initialize #'custom-initialize-default
  :link '(info-link "(modus-themes) Fringes"))

(defcustom modus-themes-lang-checkers nil
  "Control the style of spelling and code checkers/linters.

The value is a list of properties, each designated by a symbol.
The default (nil) applies a color-coded underline to the affected
text, while it leaves the original foreground intact.  If the
display spec of Emacs has support for it, the underline's style
is that of a wave, otherwise it is a straight line.

The property `straight-underline' ensures that the underline
under the affected text is always drawn as a straight line.

The property `text-also' applies the same color of the underline
to the affected text.

The property `background' adds a color-coded background.

The property `intense' amplifies the applicable colors if
`background' and/or `text-also' are set.  If `intense' is set on
its own, then it implies `text-also'.

The property `faint' uses nuanced colors for the underline and
for the foreground when `text-also' is included.  If both `faint'
and `intense' are specified, the former takes precedence.

Combinations of any of those properties can be expressed in a
list, as in those examples:

    (background)
    (straight-underline intense)
    (background text-also straight-underline)

The order in which the properties are set is not significant.

In user configuration files the form may look like this:

    (setq modus-themes-lang-checkers (quote (text-also background)))

NOTE: The placement of the straight underline, though not the
wave style, is controlled by the built-in variables
`underline-minimum-offset', `x-underline-at-descent-line',
`x-use-underline-position-properties'.

To disable fringe indicators for Flymake or Flycheck, refer to
variables `flymake-fringe-indicator-position' and
`flycheck-indication-mode', respectively."
  :group 'modus-themes
  :package-version '(modus-themes . "1.7.0")
  :version "29.1"
  :type '(set :tag "Properties" :greedy t
              (const :tag "Straight underline" straight-underline)
              (const :tag "Colorise text as well" text-also)
              (const :tag "With background" background)
              (choice :tag "Overall coloration"
                      (const :tag "Intense colors" intense)
                      (const :tag "Faint colors" faint)))
  :set #'modus-themes--set-option
  :initialize #'custom-initialize-default
  :link '(info-link "(modus-themes) Language checkers"))

(defcustom modus-themes-org-blocks nil
  "Set the overall style of Org code blocks, quotes, and the like.

Nil (the default) means that the block has no background of its
own: it uses the one that applies to the rest of the buffer.  In
this case, the delimiter lines have a gray color for their text,
making them look exactly like all other Org properties.

Option `gray-background' applies a subtle gray background to the
block's contents.  It also affects the begin and end lines of the
block as they get another shade of gray as their background,
which differentiates them from the contents of the block.  All
background colors extend to the edge of the window, giving the
area a rectangular, \"blocky\" presentation.

Option `tinted-background' uses a slightly colored background for
the contents of the block.  The exact color will depend on the
programming language and is controlled by the variable
`org-src-block-faces' (refer to the theme's source code for the
current association list).  For this to take effect, the Org
buffer needs to be restarted with `org-mode-restart'.  In this
scenario, it may be better to inhibit the extension of the
delimiter lines' background to the edge of the window because Org
does not provide a mechanism to update their colors depending on
the contents of the block.  Disable the extension of such
backgrounds by setting `org-fontify-whole-block-delimiter-line'
to nil.

Code blocks use their major mode's colors only when the variable
`org-src-fontify-natively' is non-nil.  While quote/verse blocks
require setting `org-fontify-quote-and-verse-blocks' to a non-nil
value.

Older versions of the themes provided options `grayscale' (or
`greyscale') and `rainbow'.  Those will continue to work as they
are aliases for `gray-background' and `tinted-background',
respectively."
  :group 'modus-themes
  :package-version '(modus-themes . "2.1.0")
  :version "28.1"
  :type '(choice
          (const :format "[%v] %t\n" :tag "No Org block background (default)" nil)
          (const :format "[%v] %t\n" :tag "Subtle gray block background" gray-background)
          (const :format "[%v] %t\n" :tag "Alias for `gray-background'" grayscale) ; for backward compatibility
          (const :format "[%v] %t\n" :tag "Alias for `gray-background'" greyscale)
          (const :format "[%v] %t\n" :tag "Color-coded background per programming language" tinted-background)
          (const :format "[%v] %t\n" :tag "Alias for `tinted-background'" rainbow)) ; back compat
  :set #'modus-themes--set-option
  :initialize #'custom-initialize-default
  :link '(info-link "(modus-themes) Org mode blocks"))

(defcustom modus-themes-mode-line nil
  "Control the overall style of the mode line.

The value is a list of properties, each designated by a symbol.
The default (a nil value or an empty list) is a two-dimensional
rectangle with a border around it.  The active and the inactive
mode lines use different shades of grayscale values for the
background, foreground, border.

The `3d' property applies a three-dimensional effect to the
active mode line.  The inactive mode lines remain two-dimensional
and are toned down a bit, relative to the default style.

The `moody' property optimizes the mode line for use with the
library of the same name (hereinafter referred to as Moody).
In practice, it removes the box effect and replaces it with
underline and overline properties.  It also tones down the
inactive mode lines.  Despite its intended purpose, this option
can also be used without the Moody library (please consult the
themes' manual on this point for more details).  If both `3d' and
`moody' properties are set, the latter takes precedence.

The `borderless' property removes the color of the borders.  It
does not actually remove the borders, but only makes their color
the same as the background, effectively creating some padding.

The `accented' property ensures that the active mode line uses a
colored background instead of the standard shade of gray.

A positive integer (natural number or natnum) applies a padding
effect of NATNUM pixels at the boundaries of the mode lines.  The
default value is 1 and does not need to be specified explicitly.
The padding has no effect when the `moody' property is also used,
because Moody already applies its own tweaks.  To ensure that the
underline is placed at the bottom of the mode line, set
`x-underline-at-descent-line' to non-nil (this is not needed when
the `borderless' property is also set).  For users on Emacs 29,
the `x-use-underline-position-properties' variable must also be
set to nil.

The padding can also be expressed as a cons cell in the form
of (padding . NATNUM) or (padding NATNUM) where the key is
constant and NATNUM is the desired natural number.

A floating point (e.g. 0.9) applies an adjusted height to the
mode line's text as a multiple of the main font size.  The
default rate is 1.0 and does not need to be specified.  Apart
from a floating point, the height may also be expressed as a cons
cell in the form of (height . FLOAT) or (height FLOAT) where the
key is constant and the FLOAT is the desired number.

Combinations of any of those properties are expressed as a list,
like in these examples:

    (accented)
    (borderless 3d)
    (moody accented borderless)

Same as above, using the padding and height as an example (these
all yield the same result):

    (accented borderless 4 0.9)
    (accented borderless (padding . 4) (height . 0.9))
    (accented borderless (padding 4) (height 0.9))

The order in which the properties are set is not significant.

In user configuration files the form may look like this:

    (setq modus-themes-mode-line (quote (borderless accented)))

Note that Moody does not expose any faces that the themes could
style directly.  Instead it re-purposes existing ones to render
its tabs and ribbons.  As such, there may be cases where the
contrast ratio falls below the 7:1 target that the themes conform
with (WCAG AAA).  To hedge against this, we configure a fallback
foreground for the `moody' property, which will come into effect
when the background of the mode line changes to something less
accessible, such as Moody ribbons (read the doc string of
`set-face-attribute', specifically `:distant-foreground').  This
fallback is activated when Emacs determines that the background
and foreground of the given construct are too close to each other
in terms of color distance.  In practice, users will need to
experiment with the variable `face-near-same-color-threshold' to
trigger the effect.  We find that a value of 45000 shall suffice,
contrary to the default 30000.  Though for the combinations that
involve the `accented' and `moody' properties, as mentioned
above, that should be raised up to 70000.  Do not set it too
high, because it has the adverse effect of always overriding the
default colors (which have been carefully designed to be highly
accessible).

Furthermore, because Moody expects an underline and overline
instead of a box style, it is strongly advised to set
`x-underline-at-descent-line' to a non-nil value."
  :group 'modus-themes
  :package-version '(modus-themes . "2.3.0")
  :version "29.1"
  :type '(set :tag "Properties" :greedy t
              (choice :tag "Overall style"
                      (const :tag "Rectangular Border" nil)
                      (const :tag "3d borders" 3d)
                      (const :tag "No box effects (Moody-compatible)" moody))
              (const :tag "Colored background" accented)
              (const :tag "Without border color" borderless)
              (radio :tag "Padding"
               (natnum :tag "Natural number (e.g. 4)")
               (cons :tag "Cons cell of `(padding . NATNUM)'"
                     (const :tag "The `padding' key (constant)" padding)
                     (natnum :tag "Natural number")))
              (radio :tag "Height"
               (float :tag "Floating point (e.g. 0.9)")
               (cons :tag "Cons cell of `(height . FLOAT)'"
                     (const :tag "The `height' key (constant)" height)
                     (float :tag "Floating point"))))
  :set #'modus-themes--set-option
  :initialize #'custom-initialize-default
  :link '(info-link "(modus-themes) Mode line"))

(defcustom modus-themes-diffs nil
  "Adjust the overall style of diffs.

The default (nil) uses fairly intense color combinations for
diffs, by applying prominently colored backgrounds, with
appropriately tinted foregrounds.

Option `desaturated' follows the same principles as with the
default (nil), though it tones down all relevant colors.

Option `bg-only' applies a background but does not override the
text's foreground.  This makes it suitable for a non-nil value
passed to `diff-font-lock-syntax' (note: Magit does not support
syntax highlighting in diffs---last checked on 2021-12-02).

When the user option `modus-themes-deuteranopia' is non-nil, all
diffs will use a red/blue color-coding system instead of the
standard red/green.  Other stylistic changes are made in the
interest of optimizing for such a use-case."
  :group 'modus-themes
  :package-version '(modus-themes . "2.0.0")
  :version "29.1"
  :type '(choice
          (const :format "[%v] %t\n" :tag "Intensely colored backgrounds (default)" nil)
          (const :format "[%v] %t\n" :tag "Slightly accented backgrounds with tinted text" desaturated)
          (const :format "[%v] %t\n" :tag "Apply color-coded backgrounds; keep syntax colors intact" bg-only))
  :set #'modus-themes--set-option
  :initialize #'custom-initialize-default
  :link '(info-link "(modus-themes) Diffs"))

(defcustom modus-themes-completions
  '((selection . (intense))
    (popup . (intense)))
  "Control the style of completion user interfaces.

This affects Company, Corfu, Flx, Helm, Icomplete/Fido, Ido, Ivy,
Orderless, Selectrum, Vertico.  The value is an alist that takes
the form of a (KEY . PROPERTIES) combination.  KEY is a symbol,
while PROPERTIES is a list.  Here is a sample, followed by a
description of the particularities:

    (setq modus-themes-completions
          (quote ((matches . (extrabold background intense))
                  (selection . (semibold accented intense))
                  (popup . (accented)))))

The `matches' key refers to the highlighted characters that
correspond to the user's input.  When its properties are nil or
an empty list, matching characters in the user interface will
have a bold weight and a colored foreground.  The list of
properties may include any of the following symbols regardless of
the order they may appear in:

- `background' to add a background color;

- `intense' to increase the overall coloration (also amplifies
  the `background', if present);

- `underline' to draw a line below the characters;

- `italic' to use a slanted font (italic or oblique forms);

- The symbol of a font weight attribute such as `light',
  `semibold', et cetera.  Valid symbols are defined in the
  variable `modus-themes-weights'.  The absence of a weight means
  that bold will be used.

The `selection' key applies to the current line or currently
matched candidate, depending on the specifics of the user
interface.  When its properties are nil or an empty list, it has
a subtle gray background, a bold weight, and the base foreground
value for the text.  The list of properties it accepts is as
follows (order is not significant):

- `accented' to make the background colorful instead of gray;

- `text-also' to apply extra color to the text of the selected
  line;

- `intense' to increase the overall coloration;

- `underline' to draw a line below the characters;

- `italic' to use a slanted font (italic or oblique forms);

- The symbol of a font weight attribute such as `light',
  `semibold', et cetera.  Valid symbols are defined in the
  variable `modus-themes-weights'.  The absence of a weight means
  that bold will be used.

The `popup' key takes the same values as `selection'.  The only
difference is that it applies specifically to user interfaces
that display an inline popup and thus have slightly different
styling requirements than the minibuffer.  The two prominent
packages are `company' and `corfu'.

Apart from specifying each key separately, a fallback list is
accepted.  This is only useful when the desired aesthetic is the
same across all keys that are not explicitly referenced.  For
example, this:

    (setq modus-themes-completions
          (quote ((t . (extrabold intense)))))

Is the same as:

    (setq modus-themes-completions
          (quote ((matches . (extrabold intense))
                  (selection . (extrabold intense))
                  (popup . (extrabold intense)))))

In the case of the fallback, any property that does not apply to
the corresponding key is simply ignored (`matches' does not have
`accented' and `text-also', while `selection' and `popup' do not
have `background').

Check the manual for tweaking `bold' and `italic' faces: Info
node `(modus-themes) Configure bold and italic faces'.

Also refer to the documentation of the `orderless' package for
its intersection with `company' (if you choose to use those in
tandem)."
  :group 'modus-themes
  :package-version '(modus-themes . "3.0.0")
  :version "29.1"
  :type `(set
          (cons :tag "Matches"
                (const matches)
                (set :tag "Style of matches" :greedy t
                     (choice :tag "Font weight (must be supported by the typeface)"
                             (const :tag "Bold (default)" nil)
                             (const :tag "Thin" thin)
                             (const :tag "Ultra-light" ultralight)
                             (const :tag "Extra-light" extralight)
                             (const :tag "Light" light)
                             (const :tag "Semi-light" semilight)
                             (const :tag "Regular" regular)
                             (const :tag "Medium" medium)
                             (const :tag "Semi-bold" semibold)
                             (const :tag "Extra-bold" extrabold)
                             (const :tag "Ultra-bold" ultrabold))
                     (const :tag "With added background" background)
                     (const :tag "Increased coloration" intense)
                     (const :tag "Italic font (oblique or slanted forms)" italic)
                     (const :tag "Underline" underline)))
          (cons :tag "Selection"
                (const selection)
                (set :tag "Style of selection" :greedy t
                     (choice :tag "Font weight (must be supported by the typeface)"
                             (const :tag "Bold (default)" nil)
                             (const :tag "Thin" thin)
                             (const :tag "Ultra-light" ultralight)
                             (const :tag "Extra-light" extralight)
                             (const :tag "Light" light)
                             (const :tag "Semi-light" semilight)
                             (const :tag "Regular" regular)
                             (const :tag "Medium" medium)
                             (const :tag "Semi-bold" semibold)
                             (const :tag "Extra-bold" extrabold)
                             (const :tag "Ultra-bold" ultrabold))
                     (const :tag "Apply color to the line's text" text-also)
                     (const :tag "With accented background" accented)
                     (const :tag "Increased coloration" intense)
                     (const :tag "Italic font (oblique or slanted forms)" italic)
                     (const :tag "Underline" underline)))
          (cons :tag "Popup"
                (const popup)
                (set :tag "Style of completion pop-ups" :greedy t
                     (choice :tag "Font weight (must be supported by the typeface)"
                             (const :tag "Bold (default)" nil)
                             (const :tag "Thin" thin)
                             (const :tag "Ultra-light" ultralight)
                             (const :tag "Extra-light" extralight)
                             (const :tag "Light" light)
                             (const :tag "Semi-light" semilight)
                             (const :tag "Regular" regular)
                             (const :tag "Medium" medium)
                             (const :tag "Semi-bold" semibold)
                             (const :tag "Extra-bold" extrabold)
                             (const :tag "Ultra-bold" ultrabold))
                     (const :tag "Apply color to the line's text" text-also)
                     (const :tag "With accented background" accented)
                     (const :tag "Increased coloration" intense)
                     (const :tag "Italic font (oblique or slanted forms)" italic)
                     (const :tag "Underline" underline))))
  :set #'modus-themes--set-option
  :initialize #'custom-initialize-default
  :link '(info-link "(modus-themes) Completion UIs"))

(defcustom modus-themes-prompts nil
  "Use subtle or intense styles for minibuffer and REPL prompts.

The value is a list of properties, each designated by a symbol.
The default (a nil value or an empty list) means to only use a
subtle accented foreground color.

The property `background' applies a background color to the
prompt's text.  By default, this is a subtle accented value.

The property `intense' makes the foreground color more prominent.
If the `background' property is also set, it amplifies the value
of the background as well.

The property `gray' changes the prompt's colors to grayscale.
This affects the foreground and, if the `background' property is
also set, the background.  Its effect is subtle, unless it is
combined with the `intense' property.

The property `bold' makes the text use a bold typographic weight.
Similarly, `italic' adds a slant to the font's forms (italic or
oblique forms, depending on the typeface).

Combinations of any of those properties are expressed as a list,
like in these examples:

    (intense)
    (bold intense)
    (intense bold gray)
    (intense background gray bold)

The order in which the properties are set is not significant.

In user configuration files the form may look like this:

    (setq modus-themes-prompts (quote (background gray)))"
  :group 'modus-themes
  :package-version '(modus-themes . "1.5.0")
  :version "28.1"
  :type '(set :tag "Properties" :greedy t
              (const :tag "With Background" background)
              (const :tag "Intense" intense)
              (const :tag "Grayscale" gray)
              (const :tag "Bold font weight" bold)
              (const :tag "Italic font slant" italic))
  :set #'modus-themes--set-option
  :initialize #'custom-initialize-default
  :link '(info-link "(modus-themes) Command prompts"))

(defcustom modus-themes-hl-line '(intense)
  "Control the current line highlight of `hl-line-mode'.

The value is a list of properties, each designated by a symbol.
With a nil value, or an empty list, the style is a subtle gray
background color.

The property `accented' changes the background to a colored
variant.

An `underline' property draws a line below the highlighted area.
Its color is similar to the background, so gray by default or an
accent color when `accented' is also set.

An `intense' property amplifies the colors in use, which may be
both the background and the underline.

Combinations of any of those properties are expressed as a list,
like in these examples:

    (intense)
    (underline intense)
    (accented intense underline)

The order in which the properties are set is not significant.

In user configuration files the form may look like this:

    (setq modus-themes-hl-line (quote (underline accented)))

Set `x-underline-at-descent-line' to a non-nil value so that the
placement of the underline coincides with the lower boundary of
the colored background."
  :group 'modus-themes
  :package-version '(modus-themes . "3.0.0")
  :version "29.1"
  :type '(set :tag "Properties" :greedy t
              (const :tag "Colored background" accented)
              (const :tag "Underline" underline)
              (const :tag "Intense style" intense))
  :set #'modus-themes--set-option
  :initialize #'custom-initialize-default
  :link '(info-link "(modus-themes) Line highlighting"))

(defcustom modus-themes-subtle-line-numbers nil
  "Use more subtle style for command `display-line-numbers-mode'."
  :group 'modus-themes
  :package-version '(modus-themes . "1.2.0")
  :version "28.1"
  :type 'boolean
  :set #'modus-themes--set-option
  :initialize #'custom-initialize-default
  :link '(info-link "(modus-themes) Line numbers"))

(defcustom modus-themes-markup nil
  "Style markup in Org, Markdown, and others.

This affects constructs such as Org's =verbatim= and ~code~.

The value is a list of properties, each designated by a symbol.
The default (a nil value or an empty list) is a foreground
color.

The `italic' property applies a typographic slant (italics).

The `bold' property applies a heavier typographic weight.

The `background' property adds a background color.  The
background is a shade of gray, unless the `intense' property is
also set.

The `intense' property amplifies the existing coloration.  When
`background' is used, the background color is enhanced as well
and becomes tinted instead of being gray.

Combinations of any of those properties are expressed as a list,
like in these examples:

    (bold)
    (bold italic)
    (bold italic intense)
    (bold italic intense background)

The order in which the properties are set is not significant.

In user configuration files the form may look like this:

    (setq modus-themes-markup (quote (bold italic)))

Also check the variables `org-hide-emphasis-markers',
`org-hide-macro-markers'."
  :group 'modus-themes
  :package-version '(modus-themes . "2.1.0")
  :version "29.1"
  :type '(set :tag "Properties" :greedy t
              (const :tag "Added background" background)
              (const :tag "Intense colors" intense)
              (const :tag "Bold weight" bold)
              (const :tag "Italics (slanted text)" italic))
  :set #'modus-themes--set-option
  :initialize #'custom-initialize-default
  :link '(info-link "(modus-themes) Markup"))

(defcustom modus-themes-paren-match nil
  "Control the style of matching parentheses or delimiters.

The value is a list of properties, each designated by a symbol.
The default (a nil value or an empty list) is a subtle background
color.

The `bold' property adds a bold weight to the characters of the
matching delimiters.

The `intense' property applies a more prominent background color
to the delimiters.

The `underline' property draws a straight line under the affected
text.

Combinations of any of those properties are expressed as a list,
like in these examples:

    (bold)
    (underline intense)
    (bold intense underline)

The order in which the properties are set is not significant.

In user configuration files the form may look like this:

    (setq modus-themes-paren-match (quote (bold intense)))"
  :group 'modus-themes
  :package-version '(modus-themes . "1.5.0")
  :version "28.1"
  :type '(set :tag "Properties" :greedy t
              (const :tag "Bold weight" bold)
              (const :tag "Intense background color" intense)
              (const :tag "Underline" underline))
  :set #'modus-themes--set-option
  :initialize #'custom-initialize-default
  :link '(info-link "(modus-themes) Matching parentheses"))

(defcustom modus-themes-syntax nil
  "Control the overall style of code syntax highlighting.

The value is a list of properties, each designated by a symbol.
The default (a nil value or an empty list) is to use a balanced
combination of colors on the cyan-blue-magenta side of the
spectrum.  There is little to no use of greens, yellows, and
reds.  Comments are gray, strings are blue colored, doc strings
are a shade of cyan, while color combinations are designed to
avoid exaggerations.

The property `faint' fades the saturation of all applicable
colors, where that is possible or appropriate.

The property `yellow-comments' applies a yellow color to
comments.

The property `green-strings' applies a green color to strings and
a green tint to doc strings.

The property `alt-syntax' changes the combination of colors
beyond strings and comments, so that the effective palette is
broadened to provide greater variety relative to the default.

Combinations of any of those properties are expressed as a list,
like in these examples:

    (faint)
    (green-strings yellow-comments)
    (alt-syntax green-strings yellow-comments)
    (faint alt-syntax green-strings yellow-comments)

The order in which the properties are set is not significant.

In user configuration files the form may look like this:

    (setq modus-themes-syntax (quote (faint alt-syntax)))

Independent of this variable, users may also control the use of a
bold weight or italic text: `modus-themes-bold-constructs' and
`modus-themes-italic-constructs'."
  :group 'modus-themes
  :package-version '(modus-themes . "1.5.0")
  :version "28.1"
  :type '(set :tag "Properties" :greedy t
              (const :tag "Faint colors" faint)
              (const :tag "Yellow comments" yellow-comments)
              (const :tag "Green strings" green-strings)
              (const :tag "Alternative set of colors" alt-syntax))
  :set #'modus-themes--set-option
  :initialize #'custom-initialize-default
  :link '(info-link "(modus-themes) Syntax styles"))

(defcustom modus-themes-links nil
  "Set the style of links.

The value is a list of properties, each designated by a symbol.
The default (a nil value or an empty list) is a prominent text
color, typically blue, with an underline of the same color.

For the style of the underline, a `neutral-underline' property
turns the color of the line into a subtle gray, while the
`no-underline' property removes the line altogether.  If both of
those are set, the latter takes precedence.

For text coloration, a `faint' property desaturates the color of
the text and the underline, unless the underline is affected by
the aforementioned properties.  While a `no-color' property
removes the color from the text.  If both of those are set, the
latter takes precedence.

A `bold' property applies a heavy typographic weight to the text
of the link.

An `italic' property adds a slant to the link's text (italic or
oblique forms, depending on the typeface).

A `background' property applies a subtle tinted background color.

In case both `no-underline' and `no-color' are set, then a subtle
gray background is applied to all links.  This can still be
combined with the `bold' and `italic' properties.

Combinations of any of those properties are expressed as a list,
like in these examples:

    (faint)
    (no-underline faint)
    (no-color no-underline bold)
    (italic bold background no-color no-underline)

The order in which the properties are set is not significant.

In user configuration files the form may look like this:

    (setq modus-themes-links (quote (neutral-underline background)))

The placement of the underline, meaning its proximity to the
text, is controlled by `x-use-underline-position-properties',
`x-underline-at-descent-line', `underline-minimum-offset'.
Please refer to their documentation strings."
  :group 'modus-themes
  :package-version '(modus-themes . "1.5.0")
  :version "28.1"
  :type '(set :tag "Properties" :greedy t
              (choice :tag "Text coloration"
                      (const :tag "Saturared color (default)" nil)
                      (const :tag "Faint coloration" faint)
                      (const :tag "No color (use main black/white)" no-color))
              (choice :tag "Underline"
                      (const :tag "Same color as text (default)" nil)
                      (const :tag "Neutral (gray) underline color" neutral-underline)
                      (const :tag "No underline" no-underline))
              (const :tag "Bold font weight" bold)
              (const :tag "Italic font slant" italic)
              (const :tag "Subtle background color" background))
  :set #'modus-themes--set-option
  :initialize #'custom-initialize-default
  :link '(info-link "(modus-themes) Link styles"))

(defcustom modus-themes-region nil
  "Control the overall style of the active region.

The value is a list of properties, each designated by a symbol.
The default (a nil value or an empty list) is a prominent gray
background that overrides all foreground colors in the area it
encompasses.  Its reach extends to the edge of the window.

The `no-extend' property limits the region to the end of the
line, so that it does not reach the edge of the window.

The `bg-only' property makes the region's background color more
subtle to allow the underlying text to retain its foreground
colors.

The `accented' property applies a more colorful background to the
region.

Combinations of any of those properties are expressed as a list,
like in these examples:

    (no-extend)
    (bg-only accented)
    (accented bg-only no-extend)

The order in which the properties are set is not significant.

In user configuration files the form may look like this:

    (setq modus-themes-region (quote (bg-only no-extend)))"
  :group 'modus-themes
  :package-version '(modus-themes . "1.5.0")
  :version "28.1"
  :type '(set :tag "Properties" :greedy t
              (const :tag "Do not extend to the edge of the window" no-extend)
              (const :tag "Background only (preserve underlying colors)" bg-only)
              (const :tag "Accented background" accented))
  :set #'modus-themes--set-option
  :initialize #'custom-initialize-default
  :link '(info-link "(modus-themes) Active region"))

(defcustom modus-themes-deuteranopia nil
  "When non-nil use red/blue color-coding instead of red/green.

This is to account for red-green color deficiency, also know as
deuteranopia and variants.  It applies to all contexts where
there can be a color-coded distinction between failure or
success, a to-do or done state, a mark for deletion versus a mark
for selection (e.g. in Dired), current and lazily highlighted
search matches, removed lines in diffs as opposed to added ones,
and so on.

Note that this does not change all colors throughout the active
theme, but only applies to cases that have color-coding
significance.  For example, regular code syntax highlighting is
not affected.  There is no such need because of the themes'
overarching commitment to the highest legibility standard, which
ensures that text is readable regardless of hue, as well as the
predominance of colors on the blue-cyan-magenta-purple side of
the spectrum."
  :group 'modus-themes
  :package-version '(modus-themes . "2.0.0")
  :version "29.1"
  :type 'boolean
  :set #'modus-themes--set-option
  :initialize #'custom-initialize-default
  :link '(info-link "(modus-themes) Deuteranopia style"))

(defcustom modus-themes-mail-citations nil
  "Control the color of citations/quotes in messages or emails.

By default (a nil value) citations are styled with contrasting
hues to denote their depth.  Colors are easy to tell apart
because they complement each other, but they otherwise are not
very prominent.

Option `intense' is similar to the default in terms of using
contrasting and complementary hues, but applies more saturated
colors.

Option `faint' maintains the same color-based distinction between
citation levels though the colors it uses have subtle differences
between them.

Option `monochrome' turns all quotes into a shade of gray.

Whatever the value assigned to this variable, citations in emails
are controlled by typographic elements and/or indentation, which
the themes do not touch."
  :group 'modus-themes
  :package-version '(modus-themes . "2.1.0")
  :version "29.1"
  :type '(choice
          (const :format "[%v] %t\n" :tag "Colorful email citations with contrasting hues (default)" nil)
          (const :format "[%v] %t\n" :tag "Like the default, but with more saturated colors" intense)
          (const :format "[%v] %t\n" :tag "Like the default, but with less saturated colors" faint)
          (const :format "[%v] %t\n" :tag "Deprecated alias of `faint'" desaturated)
          (const :format "[%v] %t\n" :tag "Uniformly gray mail citations" monochrome))
  :set #'modus-themes--set-option
  :initialize #'custom-initialize-default
  :link '(info-link "(modus-themes) Mail citations"))

(defcustom modus-themes-tabs-accented nil
  "Toggle accented tab backgrounds, instead of the default gray.
This affects the built-in tab-bar mode and tab-line mode, as well
as the Centaur tabs package."
  :group 'modus-themes
  :package-version '(modus-themes . "1.6.0")
  :version "28.1"
  :type 'boolean
  :set #'modus-themes--set-option
  :initialize #'custom-initialize-default
  :link '(info-link "(modus-themes) Tab style"))

(defcustom modus-themes-box-buttons nil
  "Control the style of buttons in the Custom UI and related.

The value is a list of properties, each designated by a symbol.
The default (a nil value or an empty list) is a gray background
combined with a pseudo three-dimensional effect.

The `flat' property makes the button two dimensional.

The `accented' property changes the background from gray to an
accent color.

The `faint' property reduces the overall coloration.

The `variable-pitch' property applies a proportionately spaced
typeface to the button's text.

The `underline' property draws a line below the affected text and
removes whatever box effect.  This is optimal when Emacs runs
inside a terminal emulator.  If `flat' and `underline' are
defined together, the latter takes precedence.

The symbol of a weight attribute adjusts the font of the button
accordingly, such as `light', `semibold', etc.  Valid symbols are
defined in the variable `modus-themes-weights'.

A number, expressed as a floating point (e.g. 0.9), adjusts the
height of the button's text to that many times the base font
size.  The default height is the same as 1.0, though it need not
be explicitly stated.  Instead of a floating point, an acceptable
value can be in the form of a cons cell like (height . FLOAT)
or (height FLOAT), where FLOAT is the given number.

The `all-buttons' property extends the box button effect (or the
aforementioned properties) to the faces of the generic widget
library.  By default, those do not look like the buttons of the
Custom UI as they are ordinary text wrapped in square brackets.

Combinations of any of those properties are expressed as a list,
like in these examples:

    (flat)
    (variable-pitch flat)
    (variable-pitch flat semibold 0.9)
    (variable-pitch flat semibold (height 0.9)) ; same as above
    (variable-pitch flat semibold (height . 0.9)) ; same as above

The order in which the properties are set is not significant.

In user configuration files the form may look like this:

    (setq modus-themes-box-buttons (quote (variable-pitch flat 0.9)))"
  :group 'modus-themes
  :package-version '(modus-themes . "2.3.0")
  :version "29.1"
  :type '(set :tag "Properties" :greedy t
              (const :tag "Two-dimensional button" flat)
              (const :tag "Accented background instead of gray" accented)
              (const :tag "Reduce overall coloration" faint)
              (const :tag "Proportionately spaced font (variable-pitch)" variable-pitch)
              (const :tag "Underline instead of a box effect" underline)
              (const :tag "Apply box button style to generic widget faces" all-buttons)
              (choice :tag "Font weight (must be supported by the typeface)"
                      (const :tag "Thin" thin)
                      (const :tag "Ultra-light" ultralight)
                      (const :tag "Extra-light" extralight)
                      (const :tag "Light" light)
                      (const :tag "Semi-light" semilight)
                      (const :tag "Regular (default)" nil)
                      (const :tag "Medium" medium)
                      (const :tag "Bold" bold)
                      (const :tag "Semi-bold" semibold)
                      (const :tag "Extra-bold" extrabold)
                      (const :tag "Ultra-bold" ultrabold))
              (radio :tag "Height"
                     (float :tag "Floating point to adjust height by")
                     (cons :tag "Cons cell of `(height . FLOAT)'"
                           (const :tag "The `height' key (constant)" height)
                           (float :tag "Floating point"))))
  :set #'modus-themes--set-option
  :initialize #'custom-initialize-default
  :link '(info-link "(modus-themes) Box buttons"))



;;; Internal functions

(defun modus-themes--warn (option)
  "Warn that OPTION has changed."
  (prog1 nil
    (display-warning
     'modus-themes
     (format "`%s' has changed; please read the updated documentation" option)
     :warning)))

(defun modus-themes--list-or-warn (option)
  "Return list or nil value of OPTION, else `modus-themes--warn'."
  (let* ((value (symbol-value option)))
    (if (or (null value) (listp value))
        value
      (modus-themes--warn option))))

(defun modus-themes--property-lookup (properties alist-key list-pred default)
  "Return value from property alist or list.
Check PROPERTIES for an alist value that corresponds to
ALIST-KEY.  If no alist is present, search the PROPERTIES
list given LIST-PRED, using DEFAULT as a fallback."
  (if-let* ((val (or (alist-get alist-key properties)
                     (cl-loop for x in properties
                              if (funcall list-pred x) return x)
                     default))
            ((listp val)))
      (car val)
    val))

(defun modus-themes--palette (theme)
  "Return color palette for Modus theme THEME.
THEME is a symbol, either `modus-operandi' or `modus-vivendi'."
  (pcase theme
    ('modus-operandi
     (append modus-themes-operandi-color-overrides
             modus-themes-operandi-colors))
    ('modus-vivendi
     (append modus-themes-vivendi-color-overrides
             modus-themes-vivendi-colors))
    (_theme
     (error "'%s' is not a Modus theme" theme))))

(defvar modus-themes-faces)
(defvar modus-themes-custom-variables)

(defmacro modus-themes-theme (name)
  "Bind NAME's color palette around face specs and variables.

NAME should be the proper name of a Modus theme, either
`modus-operandi' or `modus-vivendi'.

Face specifications are passed to `custom-theme-set-faces'.
While variables are handled by `custom-theme-set-variables'.
Those are stored in `modus-themes-faces' and
`modus-themes-custom-variables' respectively."
  (declare (indent 0))
  (let ((palette-sym (gensym))
        (colors (mapcar #'car modus-themes-operandi-colors)))
    `(let* ((class '((class color) (min-colors 89)))
            (,palette-sym (modus-themes--palette ',name))
            ,@(mapcar (lambda (color)
                        (list color `(alist-get ',color ,palette-sym)))
                      colors))
       (custom-theme-set-faces ',name ,@modus-themes-faces)
       (custom-theme-set-variables ',name ,@modus-themes-custom-variables))))

(defun modus-themes--current-theme ()
  "Return current modus theme."
  (car
   (seq-filter
    (lambda (theme)
      (string-match-p "^modus" (symbol-name theme)))
    custom-enabled-themes)))

;; Helper functions that are meant to ease the implementation of the
;; above customization variables.
(defun modus-themes--bold-weight ()
  "Conditional use of a heavier text weight."
  (when modus-themes-bold-constructs
    (list :inherit 'bold)))

(defun modus-themes--slant ()
  "Conditional use of italics for slant attribute."
  (when modus-themes-italic-constructs
    (list :inherit 'italic)))

(defun modus-themes--fixed-pitch ()
  "Conditional application of `fixed-pitch' inheritance."
  (when modus-themes-mixed-fonts
    (list :inherit 'fixed-pitch)))

(defun modus-themes--variable-pitch-ui ()
  "Conditional use of `variable-pitch' in UI elements."
  (when modus-themes-variable-pitch-ui
    (list :inherit 'variable-pitch)))

(defun modus-themes--fringe (mainbg subtlebg intensebg)
  "Conditional use of background colors for fringes.
MAINBG is the default.  SUBTLEBG should be a subtle grayscale
value.  INTENSEBG must be a more pronounced grayscale color."
  (pcase modus-themes-fringes
    ('intense (list :background intensebg))
    ('subtle (list :background subtlebg))
    (_ (list :background mainbg))))

(defun modus-themes--line-numbers (mainfg mainbg altfg &optional altbg)
  "Conditional use of colors for line numbers.
MAINBG and MAINFG are the default colors.  ALTFG is a color that
combines with the theme's primary background (white/black)."
  (if modus-themes-subtle-line-numbers
      (list :background (or altbg 'unspecified) :foreground altfg)
    (list :background mainbg :foreground mainfg)))

(defun modus-themes--markup (mainfg intensefg subtlebg intensebg)
  "Conditional use of colors for markup in Org and others.
MAINFG is the default foreground.  SUBTLEBG is a gray background.
INTENSEBG is a colorful background for use with the main
foreground.  INTENSEFG is an alternative to the default."
  (let ((properties modus-themes-markup))
    (list
     :inherit
     (cond
      ((and (memq 'bold properties)
            (memq 'italic properties))
       (list 'bold-italic 'modus-themes-fixed-pitch))
      ((memq 'italic properties)
       (list 'italic 'modus-themes-fixed-pitch))
      ((memq 'bold properties)
       (list 'bold 'modus-themes-fixed-pitch))
      (t 'modus-themes-fixed-pitch))
     :background
     (cond
      ((and (memq 'background properties)
            (memq 'intense properties))
       intensebg)
      ((memq 'background properties)
       subtlebg)
      (t
       'unspecified))
     :foreground
     (cond
      ((and (memq 'background properties)
            (memq 'intense properties))
       mainfg)
      ((memq 'intense properties)
       intensefg)
      (t
       mainfg)))))

(defun modus-themes--lang-check (underline subtlefg intensefg intensefg-alt subtlebg intensebg faintfg)
  "Conditional use of foreground colors for language checkers.
UNDERLINE is a color-code value for the affected text's underline
property.  SUBTLEFG and INTENSEFG follow the same color-coding
pattern and represent a value that is faint or vibrant
respectively.  INTENSEFG-ALT is used when the intensity is high.
SUBTLEBG and INTENSEBG are color-coded background colors that
differ in overall intensity.  FAINTFG is a nuanced color."
  (let ((properties (modus-themes--list-or-warn 'modus-themes-lang-checkers)))
    (list :underline
          (list :color
                (if (memq 'faint properties)
                    faintfg underline)
                :style
                (if (memq 'straight-underline properties)
                    'line 'wave))
          :background
          (cond
           ((and (memq 'background properties)
                 (memq 'faint properties))
            subtlebg)
           ((and (memq 'background properties)
                 (memq 'intense properties))
            intensebg)
           ((memq 'background properties)
            subtlebg)
           ('unspecified))
          :foreground
          (cond
           ((and (memq 'faint properties)
                 (memq 'text-also properties))
            faintfg)
           ((and (memq 'background properties)
                 (memq 'intense properties))
            intensefg-alt)
           ((memq 'intense properties)
            intensefg)
           ((memq 'text-also properties)
            subtlefg)
           ('unspecified)))))

(defun modus-themes--prompt (mainfg intensefg grayfg subtlebg intensebg intensebg-fg subtlebggray intensebggray)
  "Conditional use of colors for text prompt faces.
MAINFG is the prompt's standard foreground.  INTENSEFG is a more
prominent alternative to the main foreground, while GRAYFG is a
less luminant shade of gray.

SUBTLEBG is a subtle accented background that works with either
MAINFG or INTENSEFG.

INTENSEBG is a more pronounced accented background color that
should be combinable with INTENSEBG-FG.

SUBTLEBGGRAY and INTENSEBGGRAY are background values.  The former
can be combined with GRAYFG, while the latter only works with the
theme's fallback text color."
  (let ((properties (modus-themes--list-or-warn 'modus-themes-prompts)))
    (list :foreground
          (cond
           ((and (memq 'gray properties)
                 (memq 'intense properties))
            'unspecified)
           ((memq 'gray properties)
            grayfg)
           ((and (memq 'background properties)
                 (memq 'intense properties))
            intensebg-fg)
           ((memq 'intense properties)
            intensefg)
           (mainfg))
          :background
          (cond
           ((and (memq 'gray properties)
                 (memq 'background properties)
                 (memq 'intense properties))
            intensebggray)
           ((and (memq 'gray properties)
                 (memq 'background properties))
            subtlebggray)
           ((and (memq 'background properties)
                 (memq 'intense properties))
            intensebg)
           ((memq 'background properties)
            subtlebg)
           ('unspecified))
          :inherit
          (cond
           ((and (memq 'bold properties)
                 (memq 'italic properties))
            'bold-italic)
           ((memq 'italic properties)
            'italic)
           ((memq 'bold properties)
            'bold)
           ('unspecified)))))

(defun modus-themes--paren (normalbg intensebg)
  "Conditional use of intense colors for matching parentheses.
NORMALBG should be the special palette color bg-paren-match or
something similar.  INTENSEBG must be easier to discern next to
other backgrounds, such as the special palette color
bg-paren-match-intense."
  (let ((properties (modus-themes--list-or-warn 'modus-themes-paren-match)))
    (list :inherit
          (if (memq 'bold properties)
              'bold
            'unspecified)
          :background
          (if (memq 'intense properties)
              intensebg
            normalbg)
          :underline
          (if (memq 'underline properties)
              t
            nil))))

(defun modus-themes--syntax-foreground (fg faint)
  "Apply foreground value to code syntax.
FG is the default.  FAINT is typically the same color in its
desaturated version."
  (let ((properties (modus-themes--list-or-warn 'modus-themes-syntax)))
    (list :foreground
          (cond
           ((memq 'faint properties)
            faint)
           (fg)))))

(defun modus-themes--syntax-extra (fg faint alt &optional faint-alt)
  "Apply foreground value to code syntax.
FG is the default.  FAINT is typically the same color in its
desaturated version.  ALT is another hue while optional FAINT-ALT
is its subtle alternative."
  (let ((properties (modus-themes--list-or-warn 'modus-themes-syntax)))
    (list :foreground
          (cond
           ((and (memq 'alt-syntax properties)
                 (memq 'faint properties))
            (or faint-alt alt))
           ((memq 'faint properties)
            faint)
           ((memq 'alt-syntax properties)
            alt)
           (fg)))))

(defun modus-themes--syntax-string (fg faint green alt &optional faint-green faint-alt)
  "Apply foreground value to strings in code syntax.
FG is the default.  FAINT is typically the same color in its
desaturated version.  GREEN is a color variant in that side of
the spectrum.  ALT is another hue.  Optional FAINT-GREEN is a
subtle alternative to GREEN.  Optional FAINT-ALT is a subtle
alternative to ALT."
  (let ((properties (modus-themes--list-or-warn 'modus-themes-syntax)))
    (list :foreground
          (cond
           ((and (memq 'faint properties)
                 (memq 'green-strings properties))
            (or faint-green green))
           ((and (memq 'alt-syntax properties)
                 (memq 'faint properties))
            (or faint-alt faint))
           ((memq 'faint properties)
            faint)
           ((memq 'green-strings properties)
            green)
           ((memq 'alt-syntax properties)
            alt)
           (fg)))))

(defun modus-themes--syntax-comment (fg yellow &optional faint-yellow faint)
  "Apply foreground value to strings in code syntax.
FG is the default.  YELLOW is a color variant of that name while
optional FAINT-YELLOW is its subtle variant.  Optional FAINT is
an alternative to the default value."
  (let ((properties (modus-themes--list-or-warn 'modus-themes-syntax)))
    (list :foreground
          (cond
           ((and (memq 'faint properties)
                 (memq 'yellow-comments properties))
            (or faint-yellow yellow))
           ((and (memq 'alt-syntax properties)
                 (memq 'yellow-comments properties)
                 (not (memq 'green-strings properties)))
            yellow)
           ((memq 'yellow-comments properties)
            yellow)
           ((memq 'faint properties)
            (or faint fg))
           (fg)))))

(defun modus-themes--key-cdr (key alist)
  "Get cdr of KEY in ALIST."
  (cdr (assoc key alist)))

(defconst modus-themes-weights
  '( thin ultralight extralight light semilight regular medium
     semibold bold heavy extrabold ultrabold)
  "List of font weights.")

(defun modus-themes--weight (list)
  "Search for `modus-themes--heading' weight in LIST."
  (catch 'found
    (dolist (elt list)
      (when (memq elt modus-themes-weights)
        (throw 'found elt)))))

(defun modus-themes--heading (level fg fg-alt bg bg-gray border)
  "Conditional styles for `modus-themes-headings'.

LEVEL is the heading's position in their order.  FG is the
default text color.  FG-ALT is an accented, more saturated value
than the default.  BG is a nuanced, typically accented,
background that can work well with either of the foreground
values.  BG-GRAY is a gray background.  BORDER is a color value
that combines well with the background and foreground."
  (let* ((key (modus-themes--key-cdr level modus-themes-headings))
         (style (or key (modus-themes--key-cdr t modus-themes-headings)))
         (style-listp (listp style))
         (properties style)
         (var (when (memq 'variable-pitch properties) 'variable-pitch))
         (varbold (if var
                      (append (list 'bold) (list var))
                    'bold))
         (weight (when style-listp (modus-themes--weight style))))
    (list :inherit
          (cond
           ;; `no-bold' is for backward compatibility because we cannot
           ;; deprecate a variable's value.
           ((or weight (memq 'no-bold properties))
            var)
           (varbold))
          :background
          (cond
           ((and (memq 'monochrome properties)
                 (memq 'background properties))
            bg-gray)
           ((memq 'background properties)
            bg)
           ('unspecified))
          :foreground
          (cond
           ((memq 'monochrome properties)
            'unspecified)
           ((memq 'rainbow properties)
            fg-alt)
           (fg))
          :height
          (modus-themes--property-lookup properties 'height #'floatp 'unspecified)
          :weight
          (or weight 'unspecified)
          :overline
          (if (memq 'overline properties)
              border
            'unspecified))))

(defun modus-themes--agenda-structure (fg)
  "Control the style of the Org agenda structure.
FG is the foreground color to use."
  (let* ((properties (modus-themes--key-cdr 'header-block modus-themes-org-agenda))
         (weight (modus-themes--weight properties)))
    (list :inherit
          (cond
           ((and weight (memq 'variable-pitch properties))
            'variable-pitch)
           (weight 'unspecified)
           ((memq 'variable-pitch properties)
            (list 'bold 'variable-pitch))
           ('bold))
          :weight
          (or weight 'unspecified)
          :height
          (cond ((memq 'no-scale properties) 'unspecified)
                ((modus-themes--property-lookup properties 'height #'floatp 1.15)))
          :foreground fg)))

(defun modus-themes--agenda-date (defaultfg grayscalefg &optional workaholicfg grayscaleworkaholicfg bg bold ul)
  "Control the style of date headings in Org agenda buffers.
DEFAULTFG is the original accent color for the foreground.
GRAYSCALEFG is a neutral color.  Optional WORKAHOLICFG and
GRAYSCALEWORKAHOLICFG are alternative foreground colors.
Optional BG is a background color.  Optional BOLD applies a bold
weight.  Optional UL applies an underline."
  (let ((properties (modus-themes--key-cdr 'header-date modus-themes-org-agenda)))
    (list :inherit
          (cond
           ((or (memq 'bold-all properties)
                (and bold (memq 'bold-today properties)))
            'bold)
           (t
            'unspecified))
          :background
          (cond
           ((memq 'underline-today properties)
            'unspecified)
           ((or bg 'unspecified)))
          :foreground
          (cond
           ((and (memq 'grayscale properties)
                 (memq 'workaholic properties))
            (or grayscaleworkaholicfg grayscalefg))
           ((memq 'grayscale properties)
            grayscalefg)
           ((memq 'workaholic properties)
            (or workaholicfg defaultfg))
           (t
            defaultfg))
          :height
          (modus-themes--property-lookup properties 'height #'floatp 'unspecified)
          :underline
          (if (and ul (memq 'underline-today properties))
              t
            'unspecified))))

(defun modus-themes--agenda-event (fg-accent &optional varied)
  "Control the style of the Org agenda events.
FG-ACCENT is the accent color to use.  Optional VARIED is a
toggle to behave in accordance with the semantics of the `varied'
property that the `event' key accepts in
`modus-themes-org-agenda'."
  (let ((properties (modus-themes--key-cdr 'event modus-themes-org-agenda)))
    (list :foreground
          (cond
           ((or (and (memq 'varied properties) varied)
                (and (memq 'accented properties)
                     (memq 'varied properties)
                     varied))
            'unspecified)
           ((memq 'accented properties)
            fg-accent)
           ('unspecified))
          :inherit
          (cond
           ((and (memq 'italic properties)
                 (memq 'varied properties)
                 varied)
            '(shadow italic))
           ((and (memq 'accented properties)
                 (memq 'varied properties)
                 varied)
            'shadow)
           ((or (and (memq 'varied properties) varied)
                (and (memq 'italic properties) varied))
            '(shadow italic))
           ((and (memq 'italic properties)
                 (not (memq 'varied properties)))
            '(shadow italic))
           ('shadow)))))

(defun modus-themes--agenda-scheduled (defaultfg uniformfg rainbowfg)
  "Control the style of the Org agenda scheduled tasks.
DEFAULTFG is an accented foreground color that is meant to
differentiate between past or present and future tasks.
UNIFORMFG is a more subtle color that eliminates the color coding
for scheduled tasks.  RAINBOWFG is a prominent accent value that
clearly distinguishes past, present, future tasks."
  (pcase (modus-themes--key-cdr 'scheduled modus-themes-org-agenda)
    ('uniform (list :foreground uniformfg))
    ('rainbow (list :foreground rainbowfg))
    (_ (list :foreground defaultfg))))

(defun modus-themes--agenda-habit (default traffic simple &optional default-d traffic-d simple-d)
  "Specify background values for `modus-themes-org-agenda' habits.
DEFAULT is the original foregrounc color.  TRAFFIC is to be used
when the traffic-light style is applied, while SIMPLE corresponds
to the simplified style.

Optional DEFAULT-D, TRAFFIC-D, SIMPLE-D are alternatives to the
main colors, meant for dopia when `modus-themes-deuteranopia' is
non-nil."
  (let ((habit (modus-themes--key-cdr 'habit modus-themes-org-agenda)))
    (cond
     ((and modus-themes-deuteranopia (null habit))
      (list :background (or default-d default)))
     ((and modus-themes-deuteranopia (eq habit 'traffic-light))
      (list :background (or traffic-d traffic)))
     ((and modus-themes-deuteranopia (eq habit 'simplified))
      (list :background (or simple-d simple)))
     (t
      (pcase habit
        ('traffic-light (list :background traffic))
        ('simplified (list :background simple))
        (_ (list :background default)))))))

(defun modus-themes--org-block (bgblk fgdefault &optional fgblk)
  "Conditionally set the background of Org blocks.
BGBLK applies to a distinct neutral background.  Else blocks have
no background of their own (the default), so they look the same
as the rest of the buffer.  FGDEFAULT is used when no distinct
background is present.  While optional FGBLK specifies a
foreground value that can be combined with BGBLK.

`modus-themes-org-blocks' also accepts `tinted-background' (alias
`rainbow') as a value which applies to `org-src-block-faces' (see
the theme's source code)."
  (if (or (eq modus-themes-org-blocks 'gray-background)
          (eq modus-themes-org-blocks 'grayscale)
          (eq modus-themes-org-blocks 'greyscale))
      (list :background bgblk :foreground (or fgblk fgdefault) :extend t)
    (list :background 'unspecified :foreground fgdefault)))

(defun modus-themes--org-block-delim (bgaccent fgaccent bg fg)
  "Conditionally set the styles of Org block delimiters.
BG, FG, BGACCENT, FGACCENT apply a background and foreground
color respectively.

The former pair is a grayscale combination that should be more
distinct than the background of the block.  It is applied to the
default styles or when `modus-themes-org-blocks' is set
to `grayscale' (or `greyscale').

The latter pair should be more subtle than the background of the
block, as it is used when `modus-themes-org-blocks' is
set to `rainbow'."
  (pcase modus-themes-org-blocks
    ('gray-background (list :background bg :foreground fg :extend t))
    ('grayscale (list :background bg :foreground fg :extend t))
    ('greyscale (list :background bg :foreground fg :extend t))
    ('tinted-background (list :background bgaccent :foreground fgaccent :extend nil))
    ('rainbow (list :background bgaccent :foreground fgaccent :extend nil))
    (_ (list :foreground fg :extend nil))))

(defun modus-themes--mode-line-attrs
    (fg bg fg-alt bg-alt fg-accent bg-accent border border-3d &optional alt-style fg-distant)
  "Color combinations for `modus-themes-mode-line'.

FG and BG are the default colors.  FG-ALT and BG-ALT are meant to
accommodate the options for a 3D mode line or a `moody' compliant
one.  FG-ACCENT and BG-ACCENT are used for all variants.  BORDER
applies to all permutations of the mode line, except the
three-dimensional effect, where BORDER-3D is used instead.

Optional ALT-STYLE applies an appropriate style to the mode
line's box property.

Optional FG-DISTANT should be close to the main background
values.  It is intended to be used as a distant-foreground
property."
  (let* ((properties (modus-themes--list-or-warn 'modus-themes-mode-line))
         (padding (modus-themes--property-lookup properties 'padding #'natnump 1))
         (height (modus-themes--property-lookup properties 'height #'floatp 'unspecified))
         (padded (> padding 1))
         (base (cond ((memq 'accented properties)
                      (cons fg-accent bg-accent))
                     ((and (or (memq 'moody properties)
                               (memq '3d properties))
                           (not (memq 'borderless properties)))
                      (cons fg-alt bg-alt))
                     ((cons fg bg))))
         (line (cond ((not (or (memq 'moody properties) padded))
                      'unspecified)
                     ((and (not (memq 'moody properties))
                           padded
                           (memq 'borderless properties))
                      'unspecified)
                     ((and (memq 'borderless properties)
                           (memq 'accented properties))
                      bg-accent)
                     ((memq 'borderless properties)
                      bg)
                     (border))))
    (list :foreground (car base)
          :background (cdr base)
          :height height
          :box
          (cond ((memq 'moody properties)
                 'unspecified)
                ((and (memq '3d properties) padded)
                 (list :line-width padding
                       :color
                       (cond ((and (memq 'accented properties)
                                   (memq 'borderless properties))
                              bg-accent)
                             ((or (memq 'accented properties)
                                  (memq 'borderless properties))
                              bg)
                             (bg-alt))
                       :style (when alt-style 'released-button)))
                ((and (memq 'accented properties) padded)
                 (list :line-width padding :color bg-accent))
                ((memq '3d properties)
                 (list :line-width padding
                       :color
                       (cond ((and (memq 'accented properties)
                                   (memq 'borderless properties))
                              bg-accent)
                             ((memq 'borderless properties) bg)
                             (border-3d))
                       :style (when alt-style 'released-button)))
                ((and (memq 'accented properties)
                      (memq 'borderless properties))
                 (list :line-width padding :color bg-accent))
                ((or (memq 'borderless properties) padded)
                 (list :line-width padding :color bg))
                (border))
          :overline line
          :underline line
          :distant-foreground
          (if (memq 'moody properties)
              fg-distant
            'unspecified))))

;; Basically this is just for the keycast key indicator.
(defun modus-themes--mode-line-padded-box (color)
  "Set padding of mode line box attribute with given COLOR."
  (list :box (list :color color
                   :line-width
                   (or (cl-loop
                        for x in modus-themes-mode-line
                        if (natnump x) return x)
                       1))))

(defun modus-themes--diff (mainbg mainfg altbg altfg &optional deubg deufg deualtbg deualtfg bg-only-fg)
  "Color combinations for `modus-themes-diffs'.

MAINBG must be one of the dedicated backgrounds for diffs while
MAINFG must be the same for the foreground.

ALTBG needs to be a slightly accented background that is meant to
be combined with ALTFG.  Both must be less intense than MAINBG
and MAINFG respectively.

DEUBG and DEUFG must be combinations of colors that account for
red-green color defficiency (deuteranopia).  They are the
equivalent of MAINBG and MAINFG.

DEUALTBG and DEUALTFG are the equivalent of ALTBG and ALTFG for
deuteranopia.

Optional non-nil BG-ONLY-FG applies ALTFG else leaves the
foreground unspecified."
  (if modus-themes-deuteranopia
      (pcase modus-themes-diffs
        ('desaturated (list :background (or deualtbg altbg) :foreground (or deualtfg altfg)))
        ('bg-only (list :background (or deualtbg altbg) :foreground (if bg-only-fg (or deualtfg altfg) 'unspecified)))
        (_ (list :background (or deubg mainbg) :foreground (or deufg mainfg))))
    (pcase modus-themes-diffs
      ('desaturated (list :background altbg :foreground altfg))
      ('bg-only (list :background altbg :foreground (if bg-only-fg altfg 'unspecified)))
      (_ (list :background mainbg :foreground mainfg)))))

(defun modus-themes--deuteran (deuteran main)
  "Determine whether to color-code success as DEUTERAN or MAIN."
  (if modus-themes-deuteranopia
      (list deuteran)
    (list main)))

(defun modus-themes--completion-line (key bg fg bgintense fgintense &optional bgaccent bgaccentintense)
  "Styles for `modus-themes-completions'.
KEY is the key of a cons cell.  BG and FG are the main colors.
BGINTENSE works with the main foreground.  FGINTENSE works on its
own.  BGACCENT and BGACCENTINTENSE are colorful variants of the
other backgrounds."
  (let* ((var (modus-themes--list-or-warn 'modus-themes-completions))
         (properties (or (alist-get key var) (alist-get t var)))
         (popup (eq key 'popup))
         (selection (eq key 'selection))
         (line (or popup selection))
         (text (memq 'text-also properties))
         (accented (memq 'accented properties))
         (intense (memq 'intense properties))
         (italic (memq 'italic properties))
         (weight (modus-themes--weight properties))
         (bold (when (and weight (eq weight 'bold)) 'bold)))
    (list
     :inherit
     (cond
      ((and italic weight (not (eq weight 'bold)))
       'italic)
      ((and weight (not (eq weight 'bold)))
       'unspecified)
      (italic 'bold-italic)
      ('bold))
     :background
     (cond
      ((and accented intense line)
       bgaccentintense)
      ((and accented line)
       bgaccent)
      (intense bgintense)
      (bg))
     :foreground
     (cond
      ((and line text intense)
       fgintense)
      ((and line text)
       fg)
      ('unspecified))
     :underline
     (if (memq 'underline properties) t 'unspecified)
     :weight
     (if (and weight (null bold)) weight 'unspecified))))

(defun modus-themes--completion-match (key bg fg bgintense fgintense)
  "Styles for `modus-themes-completions'.
KEY is the key of a cons cell.  BG and FG are the main colors.
BGINTENSE works with the main foreground.  FGINTENSE works on its
own."
  (let* ((var (modus-themes--list-or-warn 'modus-themes-completions))
         (properties (or (alist-get key var) (alist-get t var)))
         (background (memq 'background properties))
         (intense (memq 'intense properties))
         (italic (memq 'italic properties))
         (weight (modus-themes--weight properties))
         (bold (when (and weight (eq weight 'bold)) 'bold)))
    (list
     :inherit
     (cond
      ((and italic weight (not (eq weight 'bold)))
       'italic)
      ((and weight (not (eq weight 'bold)))
       'unspecified)
      (italic 'bold-italic)
      ('bold))
     :background
     (cond
      ((and background intense)
       bgintense)
      (background bg)
      ('unspecified))
     :foreground
     (cond
      ((and background intense)
       'unspecified)
      (background fg)
      (intense fgintense)
      (fg))
     :underline
     (if (memq 'underline properties) t 'unspecified)
     :weight
     (if (and weight (null bold)) weight 'unspecified))))

(defun modus-themes--link (fg fgfaint underline bg bgneutral)
  "Conditional application of link styles.
FG is the link's default color for its text and underline
property.  FGFAINT is a desaturated color for the text and
underline.  UNDERLINE is a gray color only for the undeline.  BG
is a background color and BGNEUTRAL is its fallback value."
  (let ((properties (modus-themes--list-or-warn 'modus-themes-links)))
    (list :inherit
          (cond
           ((and (memq 'bold properties)
                 (memq 'italic properties))
            'bold-italic)
           ((memq 'italic properties)
            'italic)
           ((memq 'bold properties)
            'bold)
           ('unspecified))
          :background
          (cond
           ((and (memq 'no-color properties)
                 (memq 'no-underline properties))
            bgneutral)
           ((memq 'background properties)
            bg)
           ('unspecified))
          :foreground
          (cond
           ((memq 'no-color properties)
            'unspecified)
           ((memq 'faint properties)
            fgfaint)
           (fg))
          :underline
          (cond
           ((memq 'no-underline properties)
            'unspecified)
           ((memq 'neutral-underline properties)
            underline)
           (t)))))

(defun modus-themes--link-color (fg fgfaint &optional neutralfg)
  "Extend `modus-themes--link'.
FG is the main accented foreground.  FGFAINT is also accented,
yet desaturated.  Optional NEUTRALFG is a gray value."
  (let ((properties (modus-themes--list-or-warn 'modus-themes-links)))
    (list :foreground
          (cond
           ((memq 'no-color properties)
            (or neutralfg 'unspecified))
           ((memq 'faint properties)
            fgfaint)
           (fg))
          :underline
          (cond
           ((memq 'no-underline properties)
            'unspecified)
           ((memq 'neutral-underline properties)
            (or neutralfg 'unspecified))
           (t)))))

(defun modus-themes--region (bg fg bgsubtle bgaccent bgaccentsubtle)
  "Apply `modus-themes-region' styles.

BG and FG are the main values that are used by default.  BGSUBTLE
is a subtle background value that can be combined with all colors
used to fontify text and code syntax.  BGACCENT is a colored
background that combines well with FG.  BGACCENTSUBTLE can be
combined with all colors used to fontify text."
  (let ((properties (modus-themes--list-or-warn 'modus-themes-region)))
    (list :background
          (cond
           ((and (memq 'accented properties)
                 (memq 'bg-only properties))
            bgaccentsubtle)
           ((memq 'accented properties)
            bgaccent)
           ((memq 'bg-only properties)
            bgsubtle)
           (bg))
          :foreground
          (cond
           ((and (memq 'accented properties)
                 (memq 'bg-only properties))
            'unspecified)
           ((memq 'bg-only properties)
            'unspecified)
           (fg))
          :extend
          (cond
           ((memq 'no-extend properties)
            nil)
           (t)))))

(defun modus-themes--hl-line
    (bgdefault bgintense bgaccent bgaccentsubtle lineneutral lineaccent lineneutralintense lineaccentintense)
  "Apply `modus-themes-hl-line' styles.

BGDEFAULT is a subtle neutral background.  BGINTENSE is like the
default, but more prominent.  BGACCENT is a prominent accented
background, while BGACCENTSUBTLE is more subtle.  LINENEUTRAL and
LINEACCENT are color values that can remain distinct against the
buffer's possible backgrounds: the former is neutral, the latter
is accented.  LINENEUTRALINTENSE and LINEACCENTINTENSE are their
more prominent alternatives."
  (let ((properties (modus-themes--list-or-warn 'modus-themes-hl-line)))
    (list :background
          (cond
           ((and (memq 'intense properties)
                 (memq 'accented properties))
            bgaccent)
           ((memq 'accented properties)
            bgaccentsubtle)
           ((memq 'intense properties)
            bgintense)
           (bgdefault))
          :underline
          (cond
           ((and (memq 'intense properties)
                 (memq 'accented properties)
                 (memq 'underline properties))
            lineaccentintense)
           ((and (memq 'accented properties)
                 (memq 'underline properties))
            lineaccent)
           ((and (memq 'intense properties)
                 (memq 'underline properties))
            lineneutralintense)
           ((or (memq 'no-background properties)
                (memq 'underline properties))
            lineneutral)
           ('unspecified)))))

(defun modus-themes--mail-cite (mainfg intensefg subtlefg)
  "Combinations for `modus-themes-mail-citations'.

MAINFG is an accented foreground value.  SUBTLEFG is its
desaturated counterpart.  INTENSEFG is a more saturated variant."
  (pcase modus-themes-mail-citations
    ('monochrome (list :inherit 'shadow))
    ('intense (list :foreground intensefg))
    ('faint (list :foreground subtlefg))
    ('desaturated (list :foreground subtlefg))
    (_ (list :foreground mainfg))))

(defun modus-themes--tab (bg &optional bgaccent fg fgaccent box-p bold-p var-p)
  "Helper function for tabs.
BG is the default background, while BGACCENT is its more colorful
alternative.  Optional FG is a foreground color that combines
with BG.  Same principle FGACCENT.

BOX-P and BOLD-P determine the use of a box property and the
application of a bold weight, respectively.  VAR-P controls the
application of a variable-pitch font."
  (let ((background (if modus-themes-tabs-accented (or bgaccent bg) bg))
        (foreground (if modus-themes-tabs-accented (or fgaccent fg) fg)))
    (list
     :inherit (cond
               ((and bold-p var-p)
                (if modus-themes-variable-pitch-ui
                    '(variable-pitch bold)
                  '(bold)))
               (bold-p 'bold)
               (var-p (when modus-themes-variable-pitch-ui 'variable-pitch))
               ('unspecified))
     :background background
     :foreground (or foreground 'unspecified)
     :box (if box-p (list :line-width 2 :color background) 'unspecified))))

(defun modus-themes--button (bg bgfaint bgaccent bgaccentfaint border &optional pressed-button-p)
  "Apply `modus-themes-box-buttons' styles.

BG is the main background.  BGFAINT is its subtle alternative.
BGACCENT is its accented variant and BGACCENTFAINT is the same
but less intense.  BORDER is the color around the box.

When optional PRESSED-BUTTON-P is non-nil, the box uses the
pressed button style, else the released button."
  (let* ((properties modus-themes-box-buttons)
         (weight (modus-themes--weight properties)))
    (list :inherit
          (cond
           ((and (memq 'variable-pitch properties)
                 (eq weight 'bold))
            (list 'bold 'variable-pitch))
           ((memq 'variable-pitch properties)
            'variable-pitch)
           ((eq weight 'bold)
            'bold)
           ('unspecified))
          :background
          (cond
           ((and (memq 'accented properties)
                 (memq 'faint properties)
                 bgaccentfaint))
           ((memq 'faint properties)
            bgfaint)
           ((memq 'accented properties)
            bgaccent)
           (bg))
          :box
          (cond
           ((memq 'underline properties)
            'unspecified)
           ((memq 'flat properties)
            (list :line-width -1 :color border))
           ((list :line-width -1
                  :style (if pressed-button-p
                             'pressed-button
                           'released-button)
                  :color border)))
          :weight
          (cond
           ((eq weight 'bold)
            'unspecified) ; we :inherit the `bold' face above
           (weight weight)
           ('unspecified))
          :height
          (modus-themes--property-lookup properties 'height #'floatp 'unspecified)
          :underline
          (if (memq 'underline properties)
              t
            'unspecified))))



;;;; Utilities for DIY users

;;;;; List colors (a variant of M-x list-colors-display)

(defun modus-themes--list-colors-render (buffer theme &rest _)
  "Render colors in BUFFER from THEME.
Routine for `modus-themes-list-colors'."
  (let ((palette (seq-uniq (modus-themes--palette theme)
                           (lambda (x y)
                             (eq (car x) (car y)))))
        (current-buffer buffer)
        (current-theme theme))
    (with-help-window buffer
      (with-current-buffer standard-output
        (erase-buffer)
        (when (<= (display-color-cells) 256)
          (insert (concat "Your display terminal may not render all color previews!\n"
                          "It seems to only support <= 256 colors.\n\n"))
          (put-text-property (point-min) (point) 'face 'warning))
        ;; We need this to properly render the first line.
        (insert " ")
        (dolist (cell palette)
          (let* ((name (car cell))
                 (color (cdr cell))
                 (fg (readable-foreground-color color))
                 (pad (make-string 5 ?\s)))
            (let ((old-point (point)))
              (insert (format "%s %s" color pad))
              (put-text-property old-point (point) 'face `( :foreground ,color)))
            (let ((old-point (point)))
              (insert (format " %s %s %s\n" color pad name))
              (put-text-property old-point (point)
                                 'face `( :background ,color
                                          :foreground ,fg
                                          :extend t)))
            ;; We need this to properly render the last line.
            (insert " ")))
        (setq-local revert-buffer-function
                    (lambda (_ignore-auto _noconfirm)
                       (modus-themes--list-colors-render current-buffer current-theme)))))))

(defvar modus-themes--list-colors-prompt-history '()
  "Minibuffer history for `modus-themes--list-colors-prompt'.")

(defun modus-themes--list-colors-prompt ()
  "Prompt for Modus theme.
Helper function for `modus-themes-list-colors'."
  (let ((def (format "%s" (modus-themes--current-theme))))
    (completing-read
     (format "Use palette from theme [%s]: " def)
     '(modus-operandi modus-vivendi) nil t nil
     'modus-themes--list-colors-prompt-history def)))

(defun modus-themes-list-colors (theme)
  "Preview palette of the Modus THEME of choice."
  (interactive (list (intern (modus-themes--list-colors-prompt))))
  (modus-themes--list-colors-render
   (format "*%s-list-colors*" theme)
   theme))

(defun modus-themes-list-colors-current ()
  "Call `modus-themes-list-colors' for the current Modus theme."
  (interactive)
  (modus-themes-list-colors (modus-themes--current-theme)))

;;;;; Formula to measure relative luminance

;; This is the WCAG formula: https://www.w3.org/TR/WCAG20-TECHS/G18.html
(defun modus-themes-wcag-formula (hex)
  "Get WCAG value of color value HEX.
The value is defined in hexadecimal RGB notation, such as those in
`modus-themes-operandi-colors' and `modus-themes-vivendi-colors'."
  (cl-loop for k in '(0.2126 0.7152 0.0722)
           for x in (color-name-to-rgb hex)
           sum (* k (if (<= x 0.03928)
                        (/ x 12.92)
                      (expt (/ (+ x 0.055) 1.055) 2.4)))))

;;;###autoload
(defun modus-themes-contrast (c1 c2)
  "Measure WCAG contrast ratio between C1 and C2.
C1 and C2 are color values written in hexadecimal RGB."
  (let ((ct (/ (+ (modus-themes-wcag-formula c1) 0.05)
               (+ (modus-themes-wcag-formula c2) 0.05))))
    (max ct (/ ct))))

;;;;; Retrieve colors from the themes

(defun modus-themes-current-palette ()
  "Return current color palette."
  (modus-themes--palette (modus-themes--current-theme)))

;;;###autoload
(defun modus-themes-color (color)
  "Return color value for COLOR from current palette.
COLOR is a key in `modus-themes-operandi-colors' or
`modus-themes-vivendi-colors'."
  (alist-get color (modus-themes-current-palette)))

;;;###autoload
(defun modus-themes-color-alts (light-color dark-color)
  "Return color value from current palette.
When Modus Operandi is enabled, return color value for color
LIGHT-COLOR.  When Modus Vivendi is enabled, return color value
for DARK-COLOR.  LIGHT-COLOR and DARK-COLOR are keys in
`modus-themes-operandi-colors' or `modus-themes-vivendi-colors'."
  (let* ((theme (modus-themes--current-theme))
         (color (pcase theme
                  ('modus-operandi light-color)
                  ('modus-vivendi dark-color)
                  (_theme
                   (error "'%s' is not a Modus theme" theme)))))
    (alist-get color (modus-themes--palette theme))))

(defmacro modus-themes-with-colors (&rest body)
  "Evaluate BODY with colors from current palette bound.
For colors bound, see `modus-themes-operandi-colors' or
`modus-themes-vivendi-colors'."
  (declare (indent 0))
  (let ((palette-sym (gensym))
        (colors (mapcar #'car modus-themes-operandi-colors)))
    `(let* ((class '((class color) (min-colors 89)))
            (,palette-sym (modus-themes-current-palette))
            ,@(mapcar (lambda (color)
                        (list color `(alist-get ',color ,palette-sym)))
                      colors))
       (ignore class ,@colors)          ; Silence unused variable warnings
       ,@body)))



;;;; Commands

;;;###autoload
(defun modus-themes-load-themes ()
  "Ensure that the Modus themes are in `custom-enabled-themes'.

This function is intended for use in package declarations such as
those defined with the help of `use-package'.  The idea is to add
this function to the `:init' stage of the package's loading, so
that subsequent calls that assume the presence of a loaded theme,
like `modus-themes-toggle' or `modus-themes-load-operandi', will
continue to work as intended even if they are lazy-loaded (such
as when they are declared in the `:config' phase)."
  (unless (or (custom-theme-p 'modus-operandi)
              (custom-theme-p 'modus-vivendi))
    (load-theme 'modus-operandi t t)
    (load-theme 'modus-vivendi t t)))

(defvar modus-themes-after-load-theme-hook nil
  "Hook that runs after the `modus-themes-toggle' routines.")

;;;###autoload
(defun modus-themes-load-operandi ()
  "Load `modus-operandi' and disable `modus-vivendi'.
Also run `modus-themes-after-load-theme-hook'."
  (interactive)
  (disable-theme 'modus-vivendi)
  (load-theme 'modus-operandi t)
  (run-hooks 'modus-themes-after-load-theme-hook))

;;;###autoload
(defun modus-themes-load-vivendi ()
  "Load `modus-vivendi' and disable `modus-operandi'.
Also run `modus-themes-after-load-theme-hook'."
  (interactive)
  (disable-theme 'modus-operandi)
  (load-theme 'modus-vivendi t)
  (run-hooks 'modus-themes-after-load-theme-hook))

(defun modus-themes--load-prompt ()
  "Helper for `modus-themes-toggle'."
  (let ((theme
         (intern
          (completing-read "Load Modus theme (will disable all others): "
                           '(modus-operandi modus-vivendi) nil t))))
    (mapc #'disable-theme custom-enabled-themes)
    (pcase theme
      ('modus-operandi (modus-themes-load-operandi))
      ('modus-vivendi (modus-themes-load-vivendi)))))

;;;###autoload
(defun modus-themes-toggle ()
  "Toggle between `modus-operandi' and `modus-vivendi' themes.
Also runs `modus-themes-after-load-theme-hook' at its last stage
by virtue of calling either of `modus-themes-load-operandi' and
`modus-themes-load-vivendi' functions."
  (interactive)
  (modus-themes-load-themes)
  (pcase (modus-themes--current-theme)
    ('modus-operandi (modus-themes-load-vivendi))
    ('modus-vivendi (modus-themes-load-operandi))
    (_ (modus-themes--load-prompt))))



;;;; Face specifications

(defconst modus-themes-faces
  '(
;;;; custom faces
    ;; these bespoke faces are inherited by other constructs below
;;;;; subtle colored backgrounds
    `(modus-themes-subtle-red ((,class :background ,red-subtle-bg :foreground ,fg-dim)))
    `(modus-themes-subtle-green ((,class :background ,green-subtle-bg :foreground ,fg-dim)))
    `(modus-themes-subtle-yellow ((,class :background ,yellow-subtle-bg :foreground ,fg-dim)))
    `(modus-themes-subtle-blue ((,class :background ,blue-subtle-bg :foreground ,fg-dim)))
    `(modus-themes-subtle-magenta ((,class :background ,magenta-subtle-bg :foreground ,fg-dim)))
    `(modus-themes-subtle-cyan ((,class :background ,cyan-subtle-bg :foreground ,fg-dim)))
    `(modus-themes-subtle-neutral ((,class :background ,bg-inactive :foreground ,fg-inactive)))
;;;;; intense colored backgrounds
    `(modus-themes-intense-red ((,class :background ,red-intense-bg :foreground ,fg-main)))
    `(modus-themes-intense-green ((,class :background ,green-intense-bg :foreground ,fg-main)))
    `(modus-themes-intense-yellow ((,class :background ,yellow-intense-bg :foreground ,fg-main)))
    `(modus-themes-intense-blue ((,class :background ,blue-intense-bg :foreground ,fg-main)))
    `(modus-themes-intense-magenta ((,class :background ,magenta-intense-bg :foreground ,fg-main)))
    `(modus-themes-intense-cyan ((,class :background ,cyan-intense-bg :foreground ,fg-main)))
    `(modus-themes-intense-neutral ((,class :background ,bg-active :foreground ,fg-main)))
;;;;; refined background and foreground combinations
    ;; general purpose styles that use an accented foreground against an
    ;; accented background
    `(modus-themes-refine-red ((,class :background ,red-refine-bg :foreground ,red-refine-fg)))
    `(modus-themes-refine-green ((,class :background ,green-refine-bg :foreground ,green-refine-fg)))
    `(modus-themes-refine-yellow ((,class :background ,yellow-refine-bg :foreground ,yellow-refine-fg)))
    `(modus-themes-refine-blue ((,class :background ,blue-refine-bg :foreground ,blue-refine-fg)))
    `(modus-themes-refine-magenta ((,class :background ,magenta-refine-bg :foreground ,magenta-refine-fg)))
    `(modus-themes-refine-cyan ((,class :background ,cyan-refine-bg :foreground ,cyan-refine-fg)))
;;;;; "active" combinations, mostly for use on the mode line
    `(modus-themes-active-red ((,class :background ,red-active :foreground ,bg-active)))
    `(modus-themes-active-green ((,class :background ,green-active :foreground ,bg-active)))
    `(modus-themes-active-yellow ((,class :background ,yellow-active :foreground ,bg-active)))
    `(modus-themes-active-blue ((,class :background ,blue-active :foreground ,bg-active)))
    `(modus-themes-active-magenta ((,class :background ,magenta-active :foreground ,bg-active)))
    `(modus-themes-active-cyan ((,class :background ,cyan-active :foreground ,bg-active)))
;;;;; nuanced backgrounds
    ;; useful for adding an accented background that is suitable for all
    ;; main foreground colors (intended for use in Org source blocks)
    `(modus-themes-nuanced-red ((,class :background ,red-nuanced-bg :extend t)))
    `(modus-themes-nuanced-green ((,class :background ,green-nuanced-bg :extend t)))
    `(modus-themes-nuanced-yellow ((,class :background ,yellow-nuanced-bg :extend t)))
    `(modus-themes-nuanced-blue ((,class :background ,blue-nuanced-bg :extend t)))
    `(modus-themes-nuanced-magenta ((,class :background ,magenta-nuanced-bg :extend t)))
    `(modus-themes-nuanced-cyan ((,class :background ,cyan-nuanced-bg :extend t)))
;;;;; fringe-specific combinations
    `(modus-themes-fringe-red ((,class :background ,red-fringe-bg :foreground ,fg-main)))
    `(modus-themes-fringe-green ((,class :background ,green-fringe-bg :foreground ,fg-main)))
    `(modus-themes-fringe-yellow ((,class :background ,yellow-fringe-bg :foreground ,fg-main)))
    `(modus-themes-fringe-blue ((,class :background ,blue-fringe-bg :foreground ,fg-main)))
    `(modus-themes-fringe-magenta ((,class :background ,magenta-fringe-bg :foreground ,fg-main)))
    `(modus-themes-fringe-cyan ((,class :background ,cyan-fringe-bg :foreground ,fg-main)))
;;;;; special base values
    ;; these are closer to the grayscale than the accents defined above
    ;; and should only be used when the next closest alternative would be
    ;; a grayscale value than an accented one
    `(modus-themes-special-cold ((,class :background ,bg-special-cold :foreground ,fg-special-cold)))
    `(modus-themes-special-mild ((,class :background ,bg-special-mild :foreground ,fg-special-mild)))
    `(modus-themes-special-warm ((,class :background ,bg-special-warm :foreground ,fg-special-warm)))
    `(modus-themes-special-calm ((,class :background ,bg-special-calm :foreground ,fg-special-calm)))
;;;;; diff-specific combinations
    ;; intended for `diff-mode' or equivalent
    `(modus-themes-diff-added
      ((,class ,@(modus-themes--diff
                  bg-diff-focus-added fg-diff-focus-added
                  green-nuanced-bg fg-diff-added
                  bg-diff-focus-added-deuteran fg-diff-focus-added-deuteran
                  blue-nuanced-bg fg-diff-added-deuteran))))
    `(modus-themes-diff-changed
      ((,class ,@(modus-themes--diff
                  bg-diff-focus-changed fg-diff-focus-changed
                  yellow-nuanced-bg fg-diff-changed))))
    `(modus-themes-diff-removed
      ((,class ,@(modus-themes--diff
                  bg-diff-focus-removed fg-diff-focus-removed
                  red-nuanced-bg fg-diff-removed))))
    `(modus-themes-diff-refine-added
      ((,class ,@(modus-themes--diff
                  bg-diff-refine-added fg-diff-refine-added
                  bg-diff-focus-added fg-diff-focus-added
                  bg-diff-refine-added-deuteran fg-diff-refine-added-deuteran
                  bg-diff-focus-added-deuteran fg-diff-focus-added-deuteran))))
    `(modus-themes-diff-refine-changed
      ((,class ,@(modus-themes--diff
                  bg-diff-refine-changed fg-diff-refine-changed
                  bg-diff-focus-changed fg-diff-focus-changed))))
    `(modus-themes-diff-refine-removed
      ((,class ,@(modus-themes--diff
                  bg-diff-refine-removed fg-diff-refine-removed
                  bg-diff-focus-removed fg-diff-focus-removed))))
    `(modus-themes-diff-focus-added
      ((,class ,@(modus-themes--diff
                  bg-diff-focus-added fg-diff-focus-added
                  bg-diff-added fg-diff-added
                  bg-diff-focus-added-deuteran fg-diff-focus-added-deuteran
                  bg-diff-added-deuteran fg-diff-added-deuteran))))
    `(modus-themes-diff-focus-changed
      ((,class ,@(modus-themes--diff
                  bg-diff-focus-changed fg-diff-focus-changed
                  bg-diff-changed fg-diff-changed))))
    `(modus-themes-diff-focus-removed
      ((,class ,@(modus-themes--diff
                  bg-diff-focus-removed fg-diff-focus-removed
                  bg-diff-removed fg-diff-removed))))
    `(modus-themes-diff-heading
      ((,class ,@(modus-themes--diff
                  bg-diff-heading fg-diff-heading
                  cyan-nuanced-bg cyan-nuanced-fg
                  bg-header fg-main
                  bg-header fg-main
                  t))))
;;;;; deuteranopia-specific
    `(modus-themes-grue ((,class :foreground ,@(modus-themes--deuteran blue green))))
    `(modus-themes-grue-active ((,class :foreground ,@(modus-themes--deuteran blue-active green-active))))
    `(modus-themes-grue-nuanced ((,class :foreground ,@(modus-themes--deuteran blue-nuanced-fg green-nuanced-fg))))
    `(modus-themes-grue-background-active ((,class :inherit ,@(modus-themes--deuteran
                                                               'modus-themes-fringe-blue
                                                               'modus-themes-fringe-green))))
    `(modus-themes-grue-background-intense ((,class :inherit ,@(modus-themes--deuteran
                                                                'modus-themes-intense-blue
                                                                'modus-themes-intense-green))))
    `(modus-themes-grue-background-subtle ((,class :inherit ,@(modus-themes--deuteran
                                                               'modus-themes-subtle-blue
                                                               'modus-themes-subtle-green))))
    `(modus-themes-grue-background-subtle ((,class :inherit ,@(modus-themes--deuteran
                                                               'modus-themes-refine-blue
                                                               'modus-themes-refine-green))))
;;;;; mark indicators
    ;; color combinations intended for Dired, Ibuffer, or equivalent
    `(modus-themes-pseudo-header ((,class :inherit bold :foreground ,fg-main)))
    `(modus-themes-mark-alt ((,class :inherit bold :background ,bg-mark-alt :foreground ,fg-mark-alt)))
    `(modus-themes-mark-del ((,class :inherit bold :background ,bg-mark-del :foreground ,fg-mark-del)))
    `(modus-themes-mark-sel ((,class :inherit bold
                                     :background ,@(modus-themes--deuteran
                                                    cyan-refine-bg
                                                    bg-mark-sel)
                                     :foreground ,fg-mark-sel)))
    `(modus-themes-mark-symbol ((,class :inherit bold :foreground ,blue-alt)))
;;;;; heading levels
    ;; styles for regular headings used in Org, Markdown, Info, etc.
    `(modus-themes-heading-0
      ((,class ,@(modus-themes--heading
                  0 cyan-alt-other blue-alt
                  cyan-nuanced-bg bg-alt bg-region))))
    `(modus-themes-heading-1
      ((,class ,@(modus-themes--heading
                  1 fg-main magenta-alt-other
                  magenta-nuanced-bg bg-alt bg-region))))
    `(modus-themes-heading-2
      ((,class ,@(modus-themes--heading
                  2 fg-special-warm magenta-alt
                  red-nuanced-bg bg-alt bg-region))))
    `(modus-themes-heading-3
      ((,class ,@(modus-themes--heading
                  3 fg-special-cold blue
                  blue-nuanced-bg bg-alt bg-region))))
    `(modus-themes-heading-4
      ((,class ,@(modus-themes--heading
                  4 fg-special-mild cyan
                  cyan-nuanced-bg bg-alt bg-region))))
    `(modus-themes-heading-5
      ((,class ,@(modus-themes--heading
                  5 fg-special-calm green-alt-other
                  green-nuanced-bg bg-alt bg-region))))
    `(modus-themes-heading-6
      ((,class ,@(modus-themes--heading
                  6 yellow-nuanced-fg yellow-alt-other
                  yellow-nuanced-bg bg-alt bg-region))))
    `(modus-themes-heading-7
      ((,class ,@(modus-themes--heading
                  7 red-nuanced-fg red-alt
                  red-nuanced-bg bg-alt bg-region))))
    `(modus-themes-heading-8
      ((,class ,@(modus-themes--heading
                  8 magenta-nuanced-fg magenta
                  bg-alt bg-alt bg-region))))
;;;;; language checkers
    `(modus-themes-lang-error ((,class ,@(modus-themes--lang-check
                                          fg-lang-underline-error fg-lang-error
                                          red red-refine-fg red-nuanced-bg red-refine-bg red-faint))))
    `(modus-themes-lang-note ((,class ,@(modus-themes--lang-check
                                         fg-lang-underline-note fg-lang-note
                                         blue-alt blue-refine-fg blue-nuanced-bg blue-refine-bg blue-faint))))
    `(modus-themes-lang-warning ((,class ,@(modus-themes--lang-check
                                            fg-lang-underline-warning fg-lang-warning
                                            yellow yellow-refine-fg yellow-nuanced-bg yellow-refine-bg yellow-faint))))
;;;;; links
    `(modus-themes-link-broken ((,class :inherit button ,@(modus-themes--link-color red red-faint))))
    `(modus-themes-link-symlink ((,class :inherit button ,@(modus-themes--link-color cyan cyan-faint))))
;;;;; markup
    `(modus-themes-markup-code
      ((,class ,@(modus-themes--markup cyan-alt-other cyan-intense bg-alt
                                       bg-special-faint-mild))))
    `(modus-themes-markup-macro
      ((,class ,@(modus-themes--markup magenta-alt-other purple-intense bg-alt
                                       bg-special-faint-cold))))
    `(modus-themes-markup-verbatim
      ((,class ,@(modus-themes--markup magenta-alt magenta-intense bg-alt
                                       bg-special-faint-calm))))
;;;;; search
    `(modus-themes-search-success ((,class :inherit modus-themes-intense-yellow)))
    `(modus-themes-search-success-lazy ((,class :inherit modus-themes-subtle-cyan)))
    `(modus-themes-search-success-modeline ((,class :foreground ,@(modus-themes--deuteran
                                                                   blue-active
                                                                   green-active))))
;;;;; tabs
    `(modus-themes-tab-active ((,class ,@(modus-themes--tab bg-tab-active nil nil nil t t))))
    `(modus-themes-tab-backdrop ((,class ,@(modus-themes--tab bg-active bg-active-accent nil nil nil nil t))))
    `(modus-themes-tab-inactive ((,class ,@(modus-themes--tab bg-tab-inactive bg-tab-inactive-accent fg-dim nil t))))
;;;;; completion frameworks
    `(modus-themes-completion-match-0
      ((,class ,@(modus-themes--completion-match
                  'matches bg-special-faint-calm magenta-alt
                  magenta-subtle-bg magenta-intense))))
    `(modus-themes-completion-match-1
      ((,class ,@(modus-themes--completion-match
                  'matches bg-special-faint-cold blue
                  blue-subtle-bg blue-intense))))
    `(modus-themes-completion-match-2
      ((,class ,@(modus-themes--completion-match
                  'matches bg-special-faint-mild green
                  green-subtle-bg green-intense))))
    `(modus-themes-completion-match-3
      ((,class ,@(modus-themes--completion-match
                  'matches bg-special-faint-warm yellow
                  yellow-subtle-bg orange-intense))))
    `(modus-themes-completion-selected
      ((,class ,@(modus-themes--completion-line
                  'selection bg-inactive blue-alt
                  bg-active blue-active
                  bg-completion-subtle bg-completion))))
    `(modus-themes-completion-selected-popup
      ((,class ,@(modus-themes--completion-line
                  'popup bg-active blue-alt
                  bg-region blue-active
                  cyan-subtle-bg cyan-refine-bg))))
;;;;; buttons
    `(modus-themes-box-button
      ((,class ,@(modus-themes--button bg-active bg-main bg-active-accent
                                       bg-special-cold bg-region))))
    `(modus-themes-box-button-pressed
      ((,class ,@(modus-themes--button bg-active bg-main bg-active-accent
                                       bg-special-cold bg-region t))))
;;;;; typography
    `(modus-themes-bold ((,class ,@(modus-themes--bold-weight))))
    `(modus-themes-fixed-pitch ((,class ,@(modus-themes--fixed-pitch))))
    `(modus-themes-slant ((,class ,@(modus-themes--slant))))
    `(modus-themes-ui-variable-pitch ((,class ,@(modus-themes--variable-pitch-ui))))
;;;;; other custom faces
    `(modus-themes-hl-line ((,class ,@(modus-themes--hl-line
                                       bg-hl-line bg-hl-line-intense
                                       bg-hl-line-intense-accent blue-nuanced-bg
                                       bg-region blue-intense-bg
                                       fg-alt blue-intense)
                                    :extend t)))
    `(modus-themes-key-binding ((,class :inherit (bold modus-themes-fixed-pitch)
                                        :foreground ,blue-alt-other)))
    `(modus-themes-prompt ((,class ,@(modus-themes--prompt
                                      cyan-alt-other blue-alt-other fg-alt
                                      cyan-nuanced-bg blue-refine-bg fg-main
                                      bg-alt bg-active))))
    `(modus-themes-reset-hard ((,class :inherit (fixed-pitch modus-themes-reset-soft)
                                       :family ,(face-attribute 'default :family))))
    `(modus-themes-reset-soft ((,class :background ,bg-main :foreground ,fg-main
                                       :weight normal :slant normal :strike-through nil
                                       :box nil :underline nil :overline nil :extend nil)))
;;;; standard faces
;;;;; absolute essentials
    `(default ((,class :background ,bg-main :foreground ,fg-main)))
    `(cursor ((,class :background ,fg-main)))
    `(fringe ((,class ,@(modus-themes--fringe bg-main bg-inactive bg-active)
                      :foreground ,fg-main)))
    `(vertical-border ((,class :foreground ,fg-window-divider-inner)))
;;;;; basic and/or ungrouped styles
    `(bold ((,class :weight bold)))
    `(bold-italic ((,class :inherit (bold italic))))
    `(underline ((,class :underline ,fg-alt)))
    `(buffer-menu-buffer ((,class :inherit bold)))
    `(child-frame-border ((,class :background ,fg-window-divider-inner)))
    `(comint-highlight-input ((,class :inherit bold)))
    `(comint-highlight-prompt ((,class :inherit modus-themes-prompt)))
    `(confusingly-reordered ((,class :inherit modus-themes-lang-error)))
    `(edmacro-label ((,class :inherit bold :foreground ,cyan)))
    `(elisp-shorthand-font-lock-face ((,class :inherit font-lock-variable-name-face)))
    `(error ((,class :inherit bold :foreground ,red)))
    `(escape-glyph ((,class :foreground ,fg-escape-char-construct)))
    `(file-name-shadow ((,class :inherit shadow)))
    `(header-line ((,class :inherit modus-themes-ui-variable-pitch
                           :background ,bg-header :foreground ,fg-header)))
    `(header-line-highlight ((,class :inherit highlight)))
    `(help-argument-name ((,class :inherit modus-themes-slant :foreground ,cyan)))
    `(help-key-binding ((,class :inherit modus-themes-key-binding)))
    `(homoglyph ((,class :foreground ,red-alt-faint)))
    `(ibuffer-locked-buffer ((,class :foreground ,yellow-alt-other-faint)))
    `(icon-button ((,class :inherit modus-themes-box-button)))
    `(italic ((,class :slant italic)))
    `(nobreak-hyphen ((,class :foreground ,fg-escape-char-construct)))
    `(nobreak-space ((,class :foreground ,fg-escape-char-construct :underline t)))
    `(menu ((,class :inverse-video unspecified :inherit modus-themes-intense-neutral)))
    `(minibuffer-prompt ((,class :inherit modus-themes-prompt)))
    `(mm-command-output ((,class :foreground ,red-alt-other)))
    `(mm-uu-extract ((,class :background ,bg-dim :foreground ,fg-special-mild)))
    `(next-error ((,class :inherit modus-themes-subtle-red :extend t)))
    `(pgtk-im-0 ((,class :inherit modus-themes-refine-cyan)))
    `(read-multiple-choice-face ((,class :inherit (bold modus-themes-mark-alt))))
    `(rectangle-preview ((,class :inherit modus-themes-special-warm)))
    `(region ((,class ,@(modus-themes--region bg-region fg-main
                                              bg-hl-alt-intense bg-region-accent
                                              bg-region-accent-subtle))))
    `(secondary-selection ((,class :inherit modus-themes-special-cold)))
    `(separator-line ((,class :underline ,bg-region)))
    `(shadow ((,class :foreground ,fg-alt)))
    `(success ((,class :inherit (bold modus-themes-grue))))
    `(trailing-whitespace ((,class :background ,red-intense-bg)))
    `(warning ((,class :inherit bold :foreground ,yellow)))
;;;;; buttons, links, widgets
    `(button ((,class ,@(modus-themes--link
                         blue-alt-other blue-alt-other-faint
                         bg-region blue-nuanced-bg bg-alt))))
    `(link ((,class :inherit button)))
    `(link-visited ((,class :inherit button
                            ,@(modus-themes--link-color
                               magenta-alt-other magenta-alt-other-faint fg-alt))))
    `(tooltip ((,class :background ,bg-special-cold :foreground ,fg-main)))
    `(widget-button ((,class ,@(if (memq 'all-buttons modus-themes-box-buttons)
                                   (list :inherit 'modus-themes-box-button)
                                 (list :inherit 'bold :foreground blue-alt)))))
    `(widget-button-pressed ((,class ,@(if (memq 'all-buttons modus-themes-box-buttons)
                                           (list :inherit 'modus-themes-box-button-pressed)
                                         (list :inherit 'bold :foreground magenta-alt)))))
    `(widget-documentation ((,class :foreground ,green)))
    `(widget-field ((,class :background ,bg-alt :foreground ,fg-main :extend nil)))
    `(widget-inactive ((,class :inherit shadow :background ,bg-dim)))
    `(widget-single-line-field ((,class :inherit widget-field)))
;;;;; alert
    `(alert-high-face ((,class :inherit bold :foreground ,red-alt)))
    `(alert-low-face ((,class :foreground ,fg-special-mild)))
    `(alert-moderate-face ((,class :inherit bold :foreground ,yellow)))
    `(alert-trivial-face ((,class :foreground ,fg-special-calm)))
    `(alert-urgent-face ((,class :inherit bold :foreground ,red-intense)))
;;;;; all-the-icons
    `(all-the-icons-blue ((,class :foreground ,blue-alt-other)))
    `(all-the-icons-blue-alt ((,class :foreground ,blue-alt)))
    `(all-the-icons-cyan ((,class :foreground ,cyan-intense)))
    `(all-the-icons-cyan-alt ((,class :foreground ,cyan-alt)))
    `(all-the-icons-dblue ((,class :foreground ,blue-faint)))
    `(all-the-icons-dcyan ((,class :foreground ,cyan-faint)))
    `(all-the-icons-dgreen ((,class :foreground ,green)))
    `(all-the-icons-dmaroon ((,class :foreground ,magenta-alt-faint)))
    `(all-the-icons-dorange ((,class :foreground ,red-alt-faint)))
    `(all-the-icons-dpink ((,class :foreground ,magenta-faint)))
    `(all-the-icons-dpurple ((,class :foreground ,magenta-alt-other-faint)))
    `(all-the-icons-dred ((,class :foreground ,red-faint)))
    `(all-the-icons-dsilver ((,class :foreground ,cyan-alt-faint)))
    `(all-the-icons-dyellow ((,class :foreground ,yellow-alt-faint)))
    `(all-the-icons-green ((,class :foreground ,green-intense)))
    `(all-the-icons-lblue ((,class :foreground ,blue-alt-other)))
    `(all-the-icons-lcyan ((,class :foreground ,cyan)))
    `(all-the-icons-lgreen ((,class :foreground ,green-alt-other)))
    `(all-the-icons-lmaroon ((,class :foreground ,magenta-alt)))
    `(all-the-icons-lorange ((,class :foreground ,red-alt)))
    `(all-the-icons-lpink ((,class :foreground ,magenta)))
    `(all-the-icons-lpurple ((,class :foreground ,magenta-faint)))
    `(all-the-icons-lred ((,class :foreground ,red)))
    `(all-the-icons-lsilver ((,class :foreground ,fg-docstring)))
    `(all-the-icons-lyellow ((,class :foreground ,yellow-alt)))
    `(all-the-icons-maroon ((,class :foreground ,magenta-intense)))
    `(all-the-icons-orange ((,class :foreground ,orange-intense)))
    `(all-the-icons-pink ((,class :foreground ,fg-special-calm)))
    `(all-the-icons-purple ((,class :foreground ,magenta-alt-other)))
    `(all-the-icons-purple-alt ((,class :foreground ,purple-intense)))
    `(all-the-icons-red ((,class :foreground ,red-intense)))
    `(all-the-icons-red-alt ((,class :foreground ,red-alt-other)))
    `(all-the-icons-silver ((,class :foreground ,fg-special-cold)))
    `(all-the-icons-yellow ((,class :foreground ,yellow)))
;;;;; all-the-icons-dired
    `(all-the-icons-dired-dir-face ((,class :foreground ,cyan-faint)))
;;;;; all-the-icons-ibuffer
    `(all-the-icons-ibuffer-dir-face ((,class :foreground ,cyan-faint)))
    `(all-the-icons-ibuffer-file-face ((,class :foreground ,blue-faint)))
    `(all-the-icons-ibuffer-mode-face ((,class :foreground ,cyan)))
    `(all-the-icons-ibuffer-size-face ((,class :foreground ,cyan-alt-other)))
;;;;; annotate
    `(annotate-annotation ((,class :inherit modus-themes-subtle-blue)))
    `(annotate-annotation-secondary ((,class :inherit modus-themes-subtle-green)))
    `(annotate-highlight ((,class :background ,blue-nuanced-bg :underline ,blue-intense)))
    `(annotate-highlight-secondary ((,class :background ,green-nuanced-bg :underline ,green-intense)))
;;;;; ansi-color
    ;; Those are in Emacs28.
    `(ansi-color-black ((,class :background "black" :foreground "black")))
    `(ansi-color-blue ((,class :background ,blue :foreground ,blue)))
    `(ansi-color-bold ((,class :inherit bold)))
    `(ansi-color-bright-black ((,class :background "gray35" :foreground "gray35")))
    `(ansi-color-bright-blue ((,class :background ,blue-alt :foreground ,blue-alt)))
    `(ansi-color-bright-cyan ((,class :background ,cyan-alt-other :foreground ,cyan-alt-other)))
    `(ansi-color-bright-green ((,class :background ,green-alt-other :foreground ,green-alt-other)))
    `(ansi-color-bright-magenta ((,class :background ,magenta-alt-other :foreground ,magenta-alt-other)))
    `(ansi-color-bright-red ((,class :background ,red-alt :foreground ,red-alt)))
    `(ansi-color-bright-white ((,class :background "white" :foreground "white")))
    `(ansi-color-bright-yellow ((,class :background ,yellow-alt :foreground ,yellow-alt)))
    `(ansi-color-cyan ((,class :background ,cyan :foreground ,cyan)))
    `(ansi-color-green ((,class :background ,green :foreground ,green)))
    `(ansi-color-magenta ((,class :background ,magenta :foreground ,magenta)))
    `(ansi-color-red ((,class :background ,red :foreground ,red)))
    `(ansi-color-white ((,class :background "gray65" :foreground "gray65")))
    `(ansi-color-yellow ((,class :background ,yellow :foreground ,yellow)))
;;;;; anzu
    `(anzu-match-1 ((,class :inherit modus-themes-subtle-cyan)))
    `(anzu-match-2 ((,class :inherit modus-themes-search-success)))
    `(anzu-match-3 ((,class :inherit modus-themes-subtle-yellow)))
    `(anzu-mode-line ((,class :inherit (bold modus-themes-search-success-modeline))))
    `(anzu-mode-line-no-match ((,class :inherit bold :foreground ,red-active)))
    `(anzu-replace-highlight ((,class :inherit modus-themes-refine-red :underline t)))
    `(anzu-replace-to ((,class :inherit modus-themes-search-success)))
;;;;; apropos
    `(apropos-button ((,class :foreground ,magenta-alt-other)))
    `(apropos-function-button ((,class :foreground ,magenta)))
    `(apropos-keybinding ((,class :inherit modus-themes-key-binding)))
    `(apropos-misc-button ((,class :foreground ,green-alt-other)))
    `(apropos-property ((,class :inherit modus-themes-bold :foreground ,magenta-alt)))
    `(apropos-symbol ((,class :inherit modus-themes-pseudo-header)))
    `(apropos-user-option-button ((,class :foreground ,cyan)))
    `(apropos-variable-button ((,class :foreground ,blue-alt)))
;;;;; artbollocks-mode
    `(artbollocks-face ((,class :inherit modus-themes-lang-note)))
    `(artbollocks-lexical-illusions-face ((,class :background ,bg-alt :foreground ,red-alt :underline t)))
    `(artbollocks-passive-voice-face ((,class :inherit modus-themes-lang-warning)))
    `(artbollocks-weasel-words-face ((,class :inherit modus-themes-lang-error)))
;;;;; auctex and Tex
    `(font-latex-bold-face ((,class :inherit bold)))
    `(font-latex-doctex-documentation-face ((,class :inherit font-lock-doc-face)))
    `(font-latex-doctex-preprocessor-face ((,class :inherit font-lock-preprocessor-face)))
    `(font-latex-italic-face ((,class :inherit italic)))
    `(font-latex-math-face ((,class :inherit font-lock-constant-face)))
    `(font-latex-script-char-face ((,class :inherit font-lock-builtin-face)))
    `(font-latex-sectioning-5-face ((,class :inherit (bold modus-themes-variable-pitch) :foreground ,blue-nuanced-fg)))
    `(font-latex-sedate-face ((,class :inherit font-lock-keyword-face)))
    `(font-latex-slide-title-face ((,class :inherit modus-themes-heading-1)))
    `(font-latex-string-face ((,class :inherit font-lock-string-face)))
    `(font-latex-subscript-face ((,class :height 0.95)))
    `(font-latex-superscript-face ((,class :height 0.95)))
    `(font-latex-underline-face ((,class :inherit underline)))
    `(font-latex-verbatim-face ((,class :inherit modus-themes-markup-verbatim)))
    `(font-latex-warning-face ((,class :inherit font-lock-warning-face)))
    `(tex-verbatim ((,class :inherit modus-themes-markup-verbatim)))
    `(texinfo-heading ((,class :foreground ,magenta)))
    `(TeX-error-description-error ((,class :inherit error)))
    `(TeX-error-description-help ((,class :inherit success)))
    `(TeX-error-description-tex-said ((,class :inherit success)))
    `(TeX-error-description-warning ((,class :inherit warning)))
;;;;; auto-dim-other-buffers
    `(auto-dim-other-buffers-face ((,class :background ,bg-alt)))
;;;;; avy
    `(avy-background-face ((,class :background ,bg-dim :foreground ,fg-dim :extend t)))
    `(avy-goto-char-timer-face ((,class :inherit (modus-themes-intense-neutral bold))))
    `(avy-lead-face ((,class :inherit (bold modus-themes-reset-soft) :background ,bg-char-0)))
    `(avy-lead-face-0 ((,class :inherit (bold modus-themes-reset-soft) :background ,bg-char-1)))
    `(avy-lead-face-1 ((,class :inherit (modus-themes-special-warm modus-themes-reset-soft))))
    `(avy-lead-face-2 ((,class :inherit (bold modus-themes-reset-soft) :background ,bg-char-2)))
;;;;; aw (ace-window)
    `(aw-background-face ((,class :foreground ,fg-unfocused)))
    `(aw-key-face ((,class :inherit modus-themes-key-binding)))
    `(aw-leading-char-face ((,class :inherit (bold modus-themes-reset-soft) :height 1.5
                                    :foreground ,red-intense)))
    `(aw-minibuffer-leading-char-face ((,class :inherit (modus-themes-intense-red bold))))
    `(aw-mode-line-face ((,class :inherit bold)))
;;;;; awesome-tray
    `(awesome-tray-module-awesome-tab-face ((,class :inherit bold :foreground ,red-alt-other)))
    `(awesome-tray-module-battery-face ((,class :inherit bold :foreground ,cyan-alt-other)))
    `(awesome-tray-module-buffer-name-face ((,class :inherit bold :foreground ,yellow-alt-other)))
    `(awesome-tray-module-circe-face ((,class :inherit bold :foreground ,blue-alt)))
    `(awesome-tray-module-date-face ((,class :inherit bold :foreground ,fg-dim)))
    `(awesome-tray-module-evil-face ((,class :inherit bold :foreground ,green-alt)))
    `(awesome-tray-module-git-face ((,class :inherit bold :foreground ,magenta)))
    `(awesome-tray-module-last-command-face ((,class :inherit bold :foreground ,blue-alt-other)))
    `(awesome-tray-module-location-face ((,class :inherit bold :foreground ,yellow)))
    `(awesome-tray-module-mode-name-face ((,class :inherit bold :foreground ,green)))
    `(awesome-tray-module-parent-dir-face ((,class :inherit bold :foreground ,cyan)))
    `(awesome-tray-module-rvm-face ((,class :inherit bold :foreground ,magenta-alt-other)))
;;;;; bbdb
    `(bbdb-name ((,class :foreground ,magenta-alt-other)))
    `(bbdb-organization ((,class :foreground ,red-alt-other)))
    `(bbdb-field-name ((,class :foreground ,cyan-alt-other)))
;;;;; binder
    `(binder-sidebar-highlight ((,class :inherit modus-themes-subtle-cyan)))
    `(binder-sidebar-marked ((,class :inherit modus-themes-mark-sel)))
    `(binder-sidebar-missing ((,class :inherit modus-themes-subtle-red)))
    `(binder-sidebar-tags ((,class :foreground ,cyan)))
;;;;; bm
    `(bm-face ((,class :inherit modus-themes-subtle-yellow :extend t)))
    `(bm-fringe-face ((,class :inherit modus-themes-fringe-yellow)))
    `(bm-fringe-persistent-face ((,class :inherit modus-themes-fringe-blue)))
    `(bm-persistent-face ((,class :inherit modus-themes-intense-blue :extend t)))
;;;;; bongo
    `(bongo-album-title ((,class :foreground ,fg-active)))
    `(bongo-artist ((,class :foreground ,magenta-active)))
    `(bongo-currently-playing-track ((,class :inherit bold)))
    `(bongo-elapsed-track-part ((,class :inherit modus-themes-subtle-magenta :underline t)))
    `(bongo-filled-seek-bar ((,class :background ,blue-intense-bg :foreground ,fg-main)))
    `(bongo-marked-track ((,class :foreground ,fg-mark-alt)))
    `(bongo-marked-track-line ((,class :background ,bg-mark-alt)))
    `(bongo-played-track ((,class :foreground ,fg-unfocused :strike-through t)))
    `(bongo-track-length ((,class :inherit shadow)))
    `(bongo-track-title ((,class :foreground ,blue-active)))
    `(bongo-unfilled-seek-bar ((,class :background ,bg-special-cold :foreground ,fg-main)))
;;;;; boon
    `(boon-modeline-cmd ((,class :inherit modus-themes-active-blue)))
    `(boon-modeline-ins ((,class :inherit modus-themes-active-red)))
    `(boon-modeline-off ((,class :inherit modus-themes-active-yellow)))
    `(boon-modeline-spc ((,class :inherit modus-themes-active-green)))
;;;;; bookmark
    `(bookmark-face ((,class :inherit modus-themes-fringe-cyan)))
    `(bookmark-menu-bookmark ((,class :inherit bold)))
;;;;; breakpoint (built-in gdb-mi.el)
    `(breakpoint-disabled ((,class :inherit shadow)))
    `(breakpoint-enabled ((,class :inherit bold :foreground ,red)))
;;;;; calendar and diary
    `(calendar-month-header ((,class :inherit modus-themes-pseudo-header)))
    `(calendar-today ((,class :inherit bold :underline t)))
    `(calendar-weekday-header ((,class :foreground ,fg-unfocused)))
    `(calendar-weekend-header ((,class :foreground ,red-faint)))
    `(diary ((,class :background ,blue-nuanced-bg :foreground ,blue-alt-other)))
    `(diary-anniversary ((,class :foreground ,red-alt-other)))
    `(diary-time ((,class :foreground ,cyan)))
    `(holiday ((,class :background ,magenta-nuanced-bg :foreground ,magenta-alt)))
;;;;; calfw
    `(cfw:face-annotation ((,class :foreground ,fg-special-warm)))
    `(cfw:face-day-title ((,class :foreground ,fg-main)))
    `(cfw:face-default-content ((,class :foreground ,green-alt)))
    `(cfw:face-default-day ((,class :inherit (cfw:face-day-title bold))))
    `(cfw:face-disable ((,class :foreground ,fg-unfocused)))
    `(cfw:face-grid ((,class :foreground ,fg-window-divider-outer)))
    `(cfw:face-header ((,class :inherit bold :foreground ,fg-main)))
    `(cfw:face-holiday ((,class :foreground ,magenta-alt-other)))
    `(cfw:face-periods ((,class :foreground ,cyan-alt-other)))
    `(cfw:face-saturday ((,class :inherit bold :foreground ,cyan-alt-other)))
    `(cfw:face-select ((,class :inherit modus-themes-intense-blue)))
    `(cfw:face-sunday ((,class :inherit bold :foreground ,cyan-alt-other)))
    `(cfw:face-title ((,class :inherit modus-themes-heading-1 :background ,bg-main :overline nil :foreground ,fg-special-cold)))
    `(cfw:face-today ((,class :background ,bg-inactive)))
    `(cfw:face-today-title ((,class :background ,bg-active)))
    `(cfw:face-toolbar ((,class :background ,bg-alt :foreground ,bg-alt)))
    `(cfw:face-toolbar-button-off ((,class :inherit shadow)))
    `(cfw:face-toolbar-button-on ((,class :inherit bold :background ,blue-nuanced-bg
                                          :foreground ,blue-alt)))
;;;;; calibredb
    `(calibredb-archive-face ((,class :foreground ,magenta-alt-faint)))
    `(calibredb-author-face ((,class :foreground ,blue-faint)))
    `(calibredb-comment-face ((,class :inherit shadow)))
    `(calibredb-date-face ((,class :foreground ,cyan)))
    `(calibredb-edit-annotation-header-title-face ((,class :inherit bold)))
    `(calibredb-favorite-face ((,class :foreground ,red-alt)))
    `(calibredb-file-face (( )))
    `(calibredb-format-face ((,class :foreground ,cyan-faint)))
    `(calibredb-highlight-face ((,class :inherit success)))
    `(calibredb-id-face (( )))
    `(calibredb-ids-face (( )))
    `(calibredb-search-header-highlight-face ((,class :inherit modus-themes-hl-line)))
    `(calibredb-search-header-library-name-face ((,class :foreground ,blue-active)))
    `(calibredb-search-header-library-path-face ((,class :inherit bold)))
    `(calibredb-search-header-sort-face ((,class :inherit bold :foreground ,magenta-active)))
    `(calibredb-search-header-total-face ((,class :inherit bold :foreground ,cyan-active)))
    `(calibredb-search-header-filter-face ((,class :inherit bold)))
    `(calibredb-mark-face ((,class :inherit modus-themes-mark-sel)))
    `(calibredb-size-face (( )))
    `(calibredb-tag-face ((,class :foreground ,magenta-alt-faint)))
;;;;; centaur-tabs
    `(centaur-tabs-active-bar-face ((,class :background ,blue-active)))
    `(centaur-tabs-close-mouse-face ((,class :inherit bold :foreground ,red-active :underline t)))
    `(centaur-tabs-close-selected ((,class :inherit centaur-tabs-selected)))
    `(centaur-tabs-close-unselected ((,class :inherit centaur-tabs-unselected)))
    `(centaur-tabs-modified-marker-selected ((,class :inherit centaur-tabs-selected)))
    `(centaur-tabs-modified-marker-unselected ((,class :inherit centaur-tabs-unselected)))
    `(centaur-tabs-default ((,class :background ,bg-main)))
    `(centaur-tabs-selected ((,class :inherit modus-themes-tab-active)))
    `(centaur-tabs-selected-modified ((,class :inherit (italic centaur-tabs-selected))))
    `(centaur-tabs-unselected ((,class :inherit modus-themes-tab-inactive)))
    `(centaur-tabs-unselected-modified ((,class :inherit (italic centaur-tabs-unselected))))
;;;;; cfrs
    `(cfrs-border-color ((,class :background ,fg-window-divider-inner)))
;;;;; change-log and log-view (`vc-print-log' and `vc-print-root-log')
    `(change-log-acknowledgment ((,class :inherit shadow)))
    `(change-log-conditionals ((,class :foreground ,yellow)))
    `(change-log-date ((,class :foreground ,cyan)))
    `(change-log-email ((,class :foreground ,cyan-alt-other)))
    `(change-log-file ((,class :inherit bold :foreground ,fg-special-cold)))
    `(change-log-function ((,class :foreground ,green-alt-other)))
    `(change-log-list ((,class :foreground ,magenta-alt)))
    `(change-log-name ((,class :foreground ,magenta-alt-other)))
    `(log-edit-header ((,class :foreground ,fg-special-warm)))
    `(log-edit-headers-separator ((,class :height 1 :background ,fg-window-divider-inner :extend t)))
    `(log-edit-summary ((,class :inherit bold :foreground ,blue)))
    `(log-edit-unknown-header ((,class :inherit shadow)))
    `(log-view-commit-body ((,class :foreground ,blue-nuanced-fg)))
    `(log-view-file ((,class :inherit bold :foreground ,fg-special-cold)))
    `(log-view-message ((,class :background ,bg-alt :foreground ,fg-alt)))
;;;;; cider
    `(cider-debug-code-overlay-face ((,class :background ,bg-alt)))
    `(cider-debug-prompt-face ((,class :foreground ,magenta-alt :underline t)))
    `(cider-deprecated-face ((,class :inherit modus-themes-refine-yellow)))
    `(cider-docview-emphasis-face ((,class :inherit italic :foreground ,fg-special-cold)))
    `(cider-docview-literal-face ((,class :foreground ,blue-alt)))
    `(cider-docview-strong-face ((,class :inherit bold :foreground ,fg-special-cold)))
    `(cider-docview-table-border-face ((,class :inherit shadow)))
    `(cider-enlightened-face ((,class :box (:line-width -1 :color ,yellow-alt :style nil) :background ,bg-dim)))
    `(cider-enlightened-local-face ((,class :inherit bold :foreground ,yellow-alt-other)))
    `(cider-error-highlight-face ((,class :foreground ,red :underline t)))
    `(cider-fragile-button-face ((,class :box (:line-width 3 :color ,fg-alt :style released-button) :foreground ,yellow)))
    `(cider-fringe-good-face ((,class :foreground ,green-active)))
    `(cider-instrumented-face ((,class :box (:line-width -1 :color ,red :style nil) :background ,bg-dim)))
    `(cider-reader-conditional-face ((,class :inherit italic :foreground ,fg-special-warm)))
    `(cider-repl-input-face ((,class :inherit bold)))
    `(cider-repl-prompt-face ((,class :inherit modus-themes-prompt)))
    `(cider-repl-stderr-face ((,class :inherit bold :foreground ,red)))
    `(cider-repl-stdout-face ((,class :foreground ,blue)))
    `(cider-result-overlay-face ((,class :box (:line-width -1 :color ,blue :style nil) :background ,bg-dim)))
    `(cider-stacktrace-error-class-face ((,class :inherit bold :foreground ,red)))
    `(cider-stacktrace-error-message-face ((,class :inherit italic :foreground ,red-alt-other)))
    `(cider-stacktrace-face ((,class :foreground ,fg-main)))
    `(cider-stacktrace-filter-active-face ((,class :foreground ,cyan-alt :underline t)))
    `(cider-stacktrace-filter-inactive-face ((,class :foreground ,cyan-alt)))
    `(cider-stacktrace-fn-face ((,class :inherit bold :foreground ,fg-main)))
    `(cider-stacktrace-ns-face ((,class :inherit (shadow italic))))
    `(cider-stacktrace-promoted-button-face ((,class :box (:line-width 3 :color ,fg-alt :style released-button) :foreground ,red)))
    `(cider-stacktrace-suppressed-button-face ((,class :box (:line-width 3 :color ,fg-alt :style pressed-button)
                                                       :background ,bg-alt :foreground ,fg-alt)))
    `(cider-test-error-face ((,class :inherit modus-themes-subtle-red)))
    `(cider-test-failure-face ((,class :inherit (modus-themes-intense-red bold))))
    `(cider-test-success-face ((,class :inherit modus-themes-grue-background-intense)))
    `(cider-traced-face ((,class :box (:line-width -1 :color ,cyan :style nil) :background ,bg-dim)))
    `(cider-warning-highlight-face ((,class :foreground ,yellow :underline t)))
;;;;; circe (and lui)
    `(circe-fool-face ((,class :inherit shadow)))
    `(circe-highlight-nick-face ((,class :inherit bold :foreground ,blue)))
    `(circe-prompt-face ((,class :inherit modus-themes-prompt)))
    `(circe-server-face ((,class :foreground ,fg-unfocused)))
    `(lui-button-face ((,class :inherit button)))
    `(lui-highlight-face ((,class :foreground ,magenta-alt)))
    `(lui-time-stamp-face ((,class :foreground ,blue-nuanced-fg)))
;;;;; citar
    `(citar ((,class :inherit shadow)))
    `(citar-highlight (( )))
;;;;; color-rg
    `(color-rg-font-lock-column-number ((,class :foreground ,magenta-alt-other)))
    `(color-rg-font-lock-command ((,class :inherit bold :foreground ,fg-main)))
    `(color-rg-font-lock-file ((,class :inherit bold :foreground ,fg-special-cold)))
    `(color-rg-font-lock-flash ((,class :inherit modus-themes-intense-blue)))
    `(color-rg-font-lock-function-location ((,class :inherit modus-themes-special-calm)))
    `(color-rg-font-lock-header-line-directory ((,class :foreground ,blue-active)))
    `(color-rg-font-lock-header-line-edit-mode ((,class :foreground ,magenta-active)))
    `(color-rg-font-lock-header-line-keyword ((,class :foreground ,green-active)))
    `(color-rg-font-lock-header-line-text ((,class :foreground ,fg-active)))
    `(color-rg-font-lock-line-number ((,class :foreground ,fg-special-warm)))
    `(color-rg-font-lock-mark-changed ((,class :inherit bold :foreground ,blue)))
    `(color-rg-font-lock-mark-deleted ((,class :inherit bold :foreground ,red)))
    `(color-rg-font-lock-match ((,class :inherit modus-themes-special-calm)))
    `(color-rg-font-lock-position-splitter ((,class :inherit shadow)))
;;;;; column-enforce-mode
    `(column-enforce-face ((,class :inherit modus-themes-refine-yellow)))
;;;;; company-mode
    `(company-echo-common ((,class :inherit modus-themes-completion-match-0)))
    `(company-preview ((,class :background ,bg-dim :foreground ,fg-dim)))
    `(company-preview-common ((,class :inherit company-echo-common)))
    `(company-preview-search ((,class :inherit modus-themes-special-calm)))
    `(company-template-field ((,class :inherit modus-themes-intense-magenta)))
    `(company-scrollbar-bg ((,class :background ,bg-active)))
    `(company-scrollbar-fg ((,class :background ,fg-active)))
    `(company-tooltip ((,class :background ,bg-alt)))
    `(company-tooltip-annotation ((,class :inherit completions-annotations)))
    `(company-tooltip-common ((,class :inherit company-echo-common)))
    `(company-tooltip-deprecated ((,class :inherit company-tooltip :strike-through t)))
    `(company-tooltip-mouse ((,class :inherit highlight)))
    `(company-tooltip-scrollbar-thumb ((,class :background ,fg-active)))
    `(company-tooltip-scrollbar-track ((,class :background ,bg-active)))
    `(company-tooltip-search ((,class :inherit (modus-themes-search-success-lazy bold))))
    `(company-tooltip-search-selection ((,class :inherit modus-themes-search-success :underline t)))
    `(company-tooltip-selection ((,class :inherit modus-themes-completion-selected-popup)))
;;;;; company-posframe
    `(company-posframe-active-backend-name ((,class :inherit bold :background ,bg-active :foreground ,blue-active)))
    `(company-posframe-inactive-backend-name ((,class :background ,bg-active :foreground ,fg-active)))
    `(company-posframe-metadata ((,class :background ,bg-inactive :foreground ,fg-inactive)))
;;;;; compilation
    `(compilation-column-number ((,class :inherit compilation-line-number)))
    `(compilation-error ((,class :inherit modus-themes-bold :foreground ,red)))
    `(compilation-info ((,class :inherit modus-themes-bold :foreground ,fg-special-cold)))
    `(compilation-line-number ((,class :foreground ,fg-special-warm)))
    `(compilation-mode-line-exit ((,class :inherit bold)))
    `(compilation-mode-line-fail ((,class :inherit modus-themes-bold :foreground ,red-active)))
    `(compilation-mode-line-run ((,class :inherit modus-themes-bold :foreground ,cyan-active)))
    `(compilation-warning ((,class :inherit modus-themes-bold :foreground ,yellow-alt)))
;;;;; completions
    `(completions-annotations ((,class :inherit modus-themes-slant :foreground ,cyan-faint)))
    `(completions-common-part ((,class :inherit modus-themes-completion-match-0)))
    `(completions-first-difference ((,class :inherit modus-themes-completion-match-1)))
;;;;; consult
    `(consult-async-running ((,class :inherit bold :foreground ,blue)))
    `(consult-async-split ((,class :foreground ,magenta-alt)))
    `(consult-bookmark ((,class :foreground ,blue)))
    `(consult-file ((,class :foreground ,fg-special-cold)))
    `(consult-imenu-prefix ((,class :inherit shadow)))
    `(consult-key ((,class :inherit modus-themes-key-binding)))
    `(consult-line-number ((,class :foreground ,fg-special-warm)))
    `(consult-line-number-prefix ((,class :foreground ,fg-unfocused)))
    `(consult-narrow-indicator ((,class :foreground ,magenta-alt)))
    `(consult-preview-cursor ((,class :inherit modus-themes-intense-blue)))
    `(consult-preview-insertion ((,class :inherit modus-themes-special-warm)))
;;;;; corfu
    `(corfu-current ((,class :inherit modus-themes-completion-selected-popup)))
    `(corfu-bar ((,class :background ,fg-alt)))
    `(corfu-border ((,class :background ,bg-active)))
    `(corfu-default ((,class :background ,bg-alt)))
;;;;; corfu-quick
    `(corfu-quick1 ((,class :inherit bold :background ,bg-char-0)))
    `(corfu-quick2 ((,class :inherit bold :background ,bg-char-1)))
;;;;; counsel
    `(counsel-active-mode ((,class :foreground ,magenta-alt-other)))
    `(counsel-application-name ((,class :foreground ,red-alt-other)))
    `(counsel-key-binding ((,class :inherit modus-themes-key-binding)))
    `(counsel-outline-1 ((,class :inherit org-level-1)))
    `(counsel-outline-2 ((,class :inherit org-level-2)))
    `(counsel-outline-3 ((,class :inherit org-level-3)))
    `(counsel-outline-4 ((,class :inherit org-level-4)))
    `(counsel-outline-5 ((,class :inherit org-level-5)))
    `(counsel-outline-6 ((,class :inherit org-level-6)))
    `(counsel-outline-7 ((,class :inherit org-level-7)))
    `(counsel-outline-8 ((,class :inherit org-level-8)))
    `(counsel-outline-default ((,class :foreground ,fg-main)))
    `(counsel-variable-documentation ((,class :inherit modus-themes-slant :foreground ,yellow-alt-other)))
;;;;; counsel-css
    `(counsel-css-selector-depth-face-1 ((,class :foreground ,blue)))
    `(counsel-css-selector-depth-face-2 ((,class :foreground ,cyan)))
    `(counsel-css-selector-depth-face-3 ((,class :foreground ,green)))
    `(counsel-css-selector-depth-face-4 ((,class :foreground ,yellow)))
    `(counsel-css-selector-depth-face-5 ((,class :foreground ,magenta)))
    `(counsel-css-selector-depth-face-6 ((,class :foreground ,red)))
;;;;; cov
    `(cov-coverage-not-run-face ((,class :foreground ,red-intense)))
    `(cov-coverage-run-face ((,class :foreground ,green-intense)))
    `(cov-heavy-face ((,class :foreground ,magenta-intense)))
    `(cov-light-face ((,class :foreground ,blue-intense)))
    `(cov-med-face ((,class :foreground ,yellow-intense)))
    `(cov-none-face ((,class :foreground ,cyan-intense)))
;;;;; cperl-mode
    `(cperl-nonoverridable-face ((,class :foreground unspecified)))
    `(cperl-array-face ((,class :inherit font-lock-keyword-face)))
    `(cperl-hash-face ((,class :inherit font-lock-variable-name-face)))
;;;;; crontab-mode
    `(crontab-minute ((,class :foreground ,blue-alt)))
    `(crontab-hour ((,class :foreground ,magenta-alt-other)))
    `(crontab-month-day ((,class :foreground ,magenta-alt)))
    `(crontab-month ((,class :foreground ,blue)))
    `(crontab-week-day ((,class :foreground ,cyan)))
    `(crontab-predefined ((,class :foreground ,blue-alt)))
;;;;; css-mode
    `(css-property ((,class :inherit font-lock-type-face)))
    `(css-selector ((,class :inherit font-lock-keyword-face)))
;;;;; csv-mode
    `(csv-separator-face ((,class :foreground ,red-intense)))
;;;;; ctrlf
    `(ctrlf-highlight-active ((,class :inherit modus-themes-search-success)))
    `(ctrlf-highlight-line ((,class :inherit modus-themes-hl-line)))
    `(ctrlf-highlight-passive ((,class :inherit modus-themes-search-success-lazy)))
;;;;; custom (M-x customize)
    `(custom-button ((,class :inherit modus-themes-box-button)))
    `(custom-button-mouse ((,class :inherit (highlight custom-button))))
    `(custom-button-pressed ((,class :inherit modus-themes-box-button-pressed)))
    `(custom-changed ((,class :inherit modus-themes-subtle-cyan)))
    `(custom-comment ((,class :inherit shadow)))
    `(custom-comment-tag ((,class :background ,bg-alt :foreground ,yellow-alt-other)))
    `(custom-face-tag ((,class :inherit bold :foreground ,blue-intense)))
    `(custom-group-tag ((,class :inherit modus-themes-pseudo-header :foreground ,magenta-alt)))
    `(custom-group-tag-1 ((,class :inherit modus-themes-special-warm)))
    `(custom-invalid ((,class :inherit (modus-themes-intense-red bold))))
    `(custom-modified ((,class :inherit modus-themes-subtle-cyan)))
    `(custom-rogue ((,class :inherit modus-themes-refine-magenta)))
    `(custom-set ((,class :foreground ,blue-alt)))
    `(custom-state ((,class :foreground ,red-alt-faint)))
    `(custom-themed ((,class :inherit modus-themes-subtle-blue)))
    `(custom-variable-obsolete ((,class :inherit shadow)))
    `(custom-variable-tag ((,class :foreground ,cyan)))
;;;;; dap-mode
    `(dap-mouse-eval-thing-face ((,class :box (:line-width -1 :color ,blue-active :style nil)
                                         :background ,bg-active :foreground ,fg-main)))
    `(dap-result-overlay-face ((,class :box (:line-width -1 :color ,bg-active :style nil)
                                       :background ,bg-active :foreground ,fg-main)))
    `(dap-ui-breakpoint-verified-fringe ((,class :inherit bold :foreground ,green-active)))
    `(dap-ui-compile-errline ((,class :inherit bold :foreground ,red-intense)))
    `(dap-ui-locals-scope-face ((,class :inherit bold :foreground ,magenta :underline t)))
    `(dap-ui-locals-variable-face ((,class :inherit bold :foreground ,cyan)))
    `(dap-ui-locals-variable-leaf-face ((,class :inherit italic :foreground ,cyan-alt-other)))
    `(dap-ui-marker-face ((,class :inherit modus-themes-subtle-blue)))
    `(dap-ui-sessions-stack-frame-face ((,class :inherit bold :foreground ,magenta-alt)))
    `(dap-ui-sessions-terminated-active-face ((,class :inherit bold :foreground ,fg-alt)))
    `(dap-ui-sessions-terminated-face ((,class :inherit shadow)))
;;;;; deadgrep
    `(deadgrep-filename-face ((,class :inherit bold :foreground ,fg-special-cold)))
    `(deadgrep-match-face ((,class :inherit modus-themes-special-calm)))
    `(deadgrep-meta-face ((,class :inherit shadow)))
    `(deadgrep-regexp-metachar-face ((,class :inherit bold :foreground ,yellow-intense)))
    `(deadgrep-search-term-face ((,class :inherit bold :foreground ,green-intense)))
;;;;; debbugs
    `(debbugs-gnu-archived ((,class :inverse-video t)))
    `(debbugs-gnu-done ((,class :inherit shadow)))
    `(debbugs-gnu-forwarded ((,class :foreground ,fg-special-warm)))
    `(debbugs-gnu-handled ((,class :foreground ,blue)))
    `(debbugs-gnu-new ((,class :foreground ,red)))
    `(debbugs-gnu-pending ((,class :foreground ,cyan)))
    `(debbugs-gnu-stale-1 ((,class :foreground ,yellow-nuanced-fg)))
    `(debbugs-gnu-stale-2 ((,class :foreground ,yellow)))
    `(debbugs-gnu-stale-3 ((,class :foreground ,yellow-alt)))
    `(debbugs-gnu-stale-4 ((,class :foreground ,yellow-alt-other)))
    `(debbugs-gnu-stale-5 ((,class :foreground ,red-alt)))
    `(debbugs-gnu-tagged ((,class :foreground ,magenta-alt)))
;;;;; deft
    `(deft-filter-string-face ((,class :inherit bold :foreground ,blue)))
    `(deft-header-face ((,class :foreground ,fg-special-warm)))
    `(deft-separator-face ((,class :foreground "gray50")))
    `(deft-summary-face ((,class :inherit (shadow modus-themes-slant))))
    `(deft-time-face ((,class :foreground ,cyan)))
    `(deft-title-face ((,class :inherit bold)))
;;;;; denote
    `(denote-faces-date ((,class :foreground ,cyan)))
    `(denote-faces-keywords ((,class :inherit modus-themes-bold :foreground ,magenta-alt)))
;;;;; devdocs
    `(devdocs-code-block ((,class :inherit modus-themes-fixed-pitch :background ,bg-dim :extend t)))
;;;;; dictionary
    `(dictionary-button-face ((,class :inherit bold :foreground ,fg-special-cold)))
    `(dictionary-reference-face ((,class :inherit button)))
    `(dictionary-word-definition-face (()))
    `(dictionary-word-entry-face ((,class :inherit font-lock-comment-face)))
;;;;; diff-hl
    `(diff-hl-change ((,class :inherit modus-themes-fringe-yellow)))
    `(diff-hl-delete ((,class :inherit modus-themes-fringe-red)))
    `(diff-hl-insert ((,class :inherit modus-themes-grue-background-active)))
    `(diff-hl-reverted-hunk-highlight ((,class :background ,fg-main :foreground ,bg-main)))
;;;;; diff-mode
    `(diff-added ((,class :inherit modus-themes-diff-added)))
    `(diff-changed ((,class :inherit modus-themes-diff-changed :extend t)))
    `(diff-changed-unspecified ((,class :inherit diff-changed)))
    `(diff-context ((,class ,@(unless (eq modus-themes-diffs 'bg-only) (list :foreground fg-unfocused)))))
    `(diff-error ((,class :inherit modus-themes-intense-red)))
    `(diff-file-header ((,class :inherit (bold diff-header))))
    `(diff-function ((,class :inherit modus-themes-diff-heading)))
    `(diff-header ((,class :foreground ,fg-main)))
    `(diff-hunk-header ((,class :inherit (bold modus-themes-diff-heading))))
    `(diff-index ((,class :inherit bold :foreground ,blue-alt)))
    `(diff-indicator-added ((,class :inherit (modus-themes-grue diff-added bold))))
    `(diff-indicator-changed ((,class :inherit (diff-changed bold) :foreground ,yellow)))
    `(diff-indicator-removed ((,class :inherit (diff-removed bold) :foreground ,red)))
    `(diff-nonexistent ((,class :inherit (modus-themes-neutral bold))))
    `(diff-refine-added ((,class :inherit modus-themes-diff-refine-added)))
    `(diff-refine-changed ((,class :inherit modus-themes-diff-refine-changed)))
    `(diff-refine-removed ((,class :inherit modus-themes-diff-refine-removed)))
    `(diff-removed ((,class :inherit modus-themes-diff-removed)))
;;;;; dim-autoload
    `(dim-autoload-cookie-line ((,class :inherit font-lock-comment-face)))
;;;;; dir-treeview
    `(dir-treeview-archive-face ((,class :foreground ,fg-special-warm)))
    `(dir-treeview-archive-icon-face ((,class :inherit dir-treeview-default-icon-face :foreground ,yellow)))
    `(dir-treeview-audio-face ((,class :foreground ,magenta)))
    `(dir-treeview-audio-icon-face ((,class :inherit dir-treeview-default-icon-face :foreground ,magenta-alt)))
    `(dir-treeview-control-face ((,class :inherit shadow)))
    `(dir-treeview-control-mouse-face ((,class :inherit highlight)))
    `(dir-treeview-default-icon-face ((,class :inherit (shadow bold) :family "Font Awesome")))
    `(dir-treeview-default-filename-face ((,class :foreground ,fg-main)))
    `(dir-treeview-directory-face ((,class :foreground ,blue)))
    `(dir-treeview-directory-icon-face ((,class :inherit dir-treeview-default-icon-face :foreground ,blue-alt)))
    `(dir-treeview-executable-face ((,class :foreground ,red-alt)))
    `(dir-treeview-executable-icon-face ((,class :inherit dir-treeview-default-icon-face :foreground ,red-alt-other)))
    `(dir-treeview-image-face ((,class :foreground ,green-alt-other)))
    `(dir-treeview-image-icon-face ((,class :inherit dir-treeview-default-icon-face :foreground ,green-alt)))
    `(dir-treeview-indent-face ((,class :inherit shadow)))
    `(dir-treeview-label-mouse-face ((,class :inherit highlight)))
    `(dir-treeview-start-dir-face ((,class :inherit modus-themes-pseudo-header)))
    `(dir-treeview-symlink-face ((,class :inherit modus-themes-link-symlink)))
    `(dir-treeview-video-face ((,class :foreground ,magenta-alt-other)))
    `(dir-treeview-video-icon-face ((,class :inherit dir-treeview-default-icon-face :foreground ,magenta-alt-other)))
;;;;; dired
    `(dired-broken-symlink ((,class :inherit modus-themes-link-broken)))
    `(dired-directory ((,class :foreground ,blue)))
    `(dired-flagged ((,class :inherit modus-themes-mark-del)))
    `(dired-header ((,class :inherit modus-themes-pseudo-header)))
    `(dired-ignored ((,class :inherit shadow)))
    `(dired-mark ((,class :inherit modus-themes-mark-symbol)))
    `(dired-marked ((,class :inherit modus-themes-mark-sel)))
    `(dired-perm-write ((,class :foreground ,fg-special-warm)))
    `(dired-symlink ((,class :inherit modus-themes-link-symlink)))
    `(dired-warning ((,class :inherit bold :foreground ,yellow)))
;;;;; dired-async
    `(dired-async-failures ((,class :inherit bold :foreground ,red-active)))
    `(dired-async-message ((,class :inherit bold :foreground ,blue-active)))
    `(dired-async-mode-message ((,class :inherit bold :foreground ,cyan-active)))
;;;;; dired-git
    `(dired-git-branch-else ((,class :inherit bold :foreground ,magenta-alt)))
    `(dired-git-branch-master ((,class :inherit bold :foreground ,magenta-alt-other)))
;;;;; dired-git-info
    `(dgi-commit-message-face ((,class :foreground ,cyan-alt-other)))
;;;;; dired-narrow
    `(dired-narrow-blink ((,class :inherit (modus-themes-subtle-cyan bold))))
;;;;; dired-subtree
    ;; remove backgrounds from dired-subtree faces, else they break
    ;; dired-{flagged,marked} and any other face that sets a background
    ;; such as hl-line.  Also, denoting depth by varying shades of gray
    ;; is not good for accessibility.
    `(dired-subtree-depth-1-face (()))
    `(dired-subtree-depth-2-face (()))
    `(dired-subtree-depth-3-face (()))
    `(dired-subtree-depth-4-face (()))
    `(dired-subtree-depth-5-face (()))
    `(dired-subtree-depth-6-face (()))
;;;;; diredfl
    `(diredfl-autofile-name ((,class :inherit modus-themes-special-cold)))
    `(diredfl-compressed-file-name ((,class :foreground ,fg-special-warm)))
    `(diredfl-compressed-file-suffix ((,class :foreground ,red-alt)))
    `(diredfl-date-time ((,class :foreground ,cyan)))
    `(diredfl-deletion ((,class :inherit modus-themes-mark-del)))
    `(diredfl-deletion-file-name ((,class :inherit modus-themes-mark-del)))
    `(diredfl-dir-heading ((,class :inherit modus-themes-pseudo-header)))
    `(diredfl-dir-name ((,class :inherit dired-directory)))
    `(diredfl-dir-priv ((,class :foreground ,blue-alt)))
    `(diredfl-exec-priv ((,class :foreground ,magenta-alt)))
    `(diredfl-executable-tag ((,class :foreground ,magenta-alt)))
    `(diredfl-file-name ((,class :foreground ,fg-main)))
    `(diredfl-file-suffix ((,class :foreground ,magenta-alt-other)))
    `(diredfl-flag-mark ((,class :inherit modus-themes-mark-sel)))
    `(diredfl-flag-mark-line ((,class :inherit modus-themes-mark-sel)))
    `(diredfl-ignored-file-name ((,class :inherit shadow)))
    `(diredfl-link-priv ((,class :foreground ,blue-alt-other)))
    `(diredfl-no-priv ((,class :foreground "gray50")))
    `(diredfl-number ((,class :foreground ,cyan-alt-other-faint)))
    `(diredfl-other-priv ((,class :foreground ,yellow)))
    `(diredfl-rare-priv ((,class :foreground ,red)))
    `(diredfl-read-priv ((,class :foreground ,fg-main)))
    `(diredfl-symlink ((,class :inherit dired-symlink)))
    `(diredfl-tagged-autofile-name ((,class :inherit modus-themes-refine-magenta)))
    `(diredfl-write-priv ((,class :foreground ,cyan)))
;;;;; dired+
    `(diredp-autofile-name ((,class :inherit modus-themes-special-cold)))
    `(diredp-compressed-file-name ((,class :foreground ,fg-special-warm)))
    `(diredp-compressed-file-suffix ((,class :foreground ,red-alt)))
    `(diredp-date-time ((,class :foreground ,cyan)))
    `(diredp-deletion ((,class :inherit modus-themes-mark-del)))
    `(diredp-deletion-file-name ((,class :inherit modus-themes-mark-del)))
    `(diredp-dir-heading ((,class :inherit modus-themes-pseudo-header)))
    `(diredp-dir-name ((,class :inherit dired-directory)))
    `(diredp-dir-priv ((,class :foreground ,blue-alt)))
    `(diredp-exec-priv ((,class :foreground ,magenta-alt)))
    `(diredp-executable-tag ((,class :foreground ,magenta-alt)))
    `(diredp-file-name ((,class :foreground ,fg-main)))
    `(diredp-file-suffix ((,class :foreground ,magenta-alt-other)))
    `(diredp-flag-mark ((,class :inherit modus-themes-mark-sel)))
    `(diredp-flag-mark-line ((,class :inherit modus-themes-mark-sel)))
    `(diredp-ignored-file-name ((,class :inherit shadow)))
    `(diredp-link-priv ((,class :foreground ,blue-alt-other)))
    `(diredp-mode-line-flagged ((,class :foreground ,red-active)))
    `(diredp-mode-line-marked ((,class :foreground ,green-active)))
    `(diredp-no-priv ((,class :foreground "gray50")))
    `(diredp-number ((,class :foreground ,cyan-alt-other-faint)))
    `(diredp-omit-file-name ((,class :inherit shadow :strike-through t)))
    `(diredp-other-priv ((,class :foreground ,yellow)))
    `(diredp-rare-priv ((,class :foreground ,red)))
    `(diredp-read-priv ((,class :foreground ,fg-main)))
    `(diredp-symlink ((,class :inherit dired-symlink)))
    `(diredp-tagged-autofile-name ((,class :inherit modus-themes-refine-magenta)))
    `(diredp-write-priv ((,class :foreground ,cyan)))
;;;;; display-fill-column-indicator-mode
    `(fill-column-indicator ((,class :height 1 :background ,bg-region :foreground ,bg-region)))
;;;;; doom-modeline
    `(doom-modeline-bar ((,class :inherit modus-themes-active-blue)))
    `(doom-modeline-bar-inactive ((,class :background ,fg-inactive :foreground ,bg-main)))
    `(doom-modeline-battery-charging ((,class :foreground ,green-active)))
    `(doom-modeline-battery-critical ((,class :inherit bold :foreground ,red-active)))
    `(doom-modeline-battery-error ((,class :inherit bold :box (:line-width -2)
                                           :foreground ,red-active)))
    `(doom-modeline-battery-full ((,class :foreground ,blue-active)))
    `(doom-modeline-battery-normal ((,class :foreground ,fg-active)))
    `(doom-modeline-battery-warning ((,class :inherit bold :foreground ,yellow-active)))
    `(doom-modeline-buffer-file ((,class :inherit bold :foreground ,fg-active)))
    `(doom-modeline-buffer-major-mode ((,class :inherit bold :foreground ,cyan-active)))
    `(doom-modeline-buffer-minor-mode ((,class :foreground ,fg-inactive)))
    `(doom-modeline-buffer-modified ((,class :inherit bold :foreground ,magenta-active)))
    `(doom-modeline-buffer-path ((,class :inherit bold :foreground ,fg-active)))
    `(doom-modeline-debug ((,class :inherit bold :foreground ,yellow-active)))
    `(doom-modeline-debug-visual ((,class :inherit bold :foreground ,red-active)))
    `(doom-modeline-evil-emacs-state ((,class :inherit bold :foreground ,magenta-active)))
    `(doom-modeline-evil-insert-state ((,class :inherit bold :foreground ,green-active)))
    `(doom-modeline-evil-motion-state ((,class :inherit bold :foreground ,fg-inactive)))
    `(doom-modeline-evil-normal-state ((,class :inherit bold :foreground ,fg-active)))
    `(doom-modeline-evil-operator-state ((,class :inherit bold :foreground ,blue-active)))
    `(doom-modeline-evil-replace-state ((,class :inherit bold :foreground ,red-active)))
    `(doom-modeline-evil-visual-state ((,class :inherit bold :foreground ,cyan-active)))
    `(doom-modeline-highlight ((,class :inherit bold :foreground ,blue-active)))
    `(doom-modeline-host ((,class :inherit italic)))
    `(doom-modeline-info ((,class :foreground ,green-active)))
    `(doom-modeline-lsp-error ((,class :inherit bold :foreground ,red-active)))
    `(doom-modeline-lsp-success ((,class :inherit (bold modus-themes-grue-active))))
    `(doom-modeline-lsp-warning ((,class :inherit bold :foreground ,yellow-active)))
    `(doom-modeline-panel ((,class :inherit modus-themes-active-blue)))
    `(doom-modeline-persp-buffer-not-in-persp ((,class :inherit italic :foreground ,yellow-active)))
    `(doom-modeline-persp-name ((,class :foreground ,fg-active)))
    `(doom-modeline-project-dir ((,class :inherit bold :foreground ,blue-active)))
    `(doom-modeline-project-parent-dir ((,class :foreground ,blue-active)))
    `(doom-modeline-project-root-dir ((,class :foreground ,fg-active)))
    `(doom-modeline-unread-number ((,class :inherit italic :foreground ,fg-active)))
    `(doom-modeline-urgent ((,class :inherit bold :foreground ,red-active)))
    `(doom-modeline-warning ((,class :inherit bold :foreground ,yellow-active)))
;;;;; easy-jekyll
    `(easy-jekyll-help-face ((,class :background ,bg-dim :foreground ,blue-alt-other)))
;;;;; ebdb
    `(ebdb-address-default ((,class :foreground ,fg-special-calm)))
    `(ebdb-defunct ((,class :inherit shadow)))
    `(ebdb-field-hidden ((,class :foreground ,magenta)))
    `(ebdb-label ((,class :foreground ,cyan-alt-other)))
    `(ebdb-mail-default ((,class :foreground ,fg-main)))
    `(ebdb-mail-primary ((,class :foreground ,magenta-alt)))
    `(ebdb-marked ((,class :background ,cyan-intense-bg)))
    `(ebdb-organization-name ((,class :foreground ,red-alt-other)))
    `(ebdb-person-name ((,class :foreground ,magenta-alt-other)))
    `(ebdb-phone-default ((,class :foreground ,cyan)))
    `(eieio-custom-slot-tag-face ((,class :foreground ,red-alt)))
;;;;; ediff
    `(ediff-current-diff-A ((,class :inherit modus-themes-diff-removed)))
    `(ediff-current-diff-Ancestor ((,class ,@(modus-themes--diff
                                              bg-special-cold fg-special-cold
                                              blue-nuanced-bg blue))))
    `(ediff-current-diff-B ((,class :inherit modus-themes-diff-added)))
    `(ediff-current-diff-C ((,class :inherit modus-themes-diff-changed)))
    `(ediff-even-diff-A ((,class :background ,bg-alt)))
    `(ediff-even-diff-Ancestor ((,class :background ,bg-alt)))
    `(ediff-even-diff-B ((,class :background ,bg-alt)))
    `(ediff-even-diff-C ((,class :background ,bg-alt)))
    `(ediff-fine-diff-A ((,class :inherit modus-themes-diff-refine-removed)))
    `(ediff-fine-diff-Ancestor ((,class :inherit modus-themes-refine-cyan)))
    `(ediff-fine-diff-B ((,class :inherit modus-themes-diff-refine-added)))
    `(ediff-fine-diff-C ((,class :inherit modus-themes-diff-refine-changed)))
    `(ediff-odd-diff-A ((,class :inherit ediff-even-diff-A)))
    `(ediff-odd-diff-Ancestor ((,class :inherit ediff-even-diff-Ancestor)))
    `(ediff-odd-diff-B ((,class :inherit ediff-even-diff-B)))
    `(ediff-odd-diff-C ((,class :inherit ediff-even-diff-C)))
;;;;; ein (Emacs IPython Notebook)
    `(ein:basecell-input-area-face ((,class :background ,bg-dim :extend t)))
    `(ein:cell-output-area (( )))
    `(ein:cell-output-area-error ((,class :background ,red-nuanced-bg :extend t)))
    `(ein:cell-output-stderr ((,class :background ,red-nuanced-bg :extend t)))
    `(ein:markdowncell-input-area-face (( )))
    `(ein:notification-tab-normal ((,class :underline t)))
;;;;; eglot
    `(eglot-mode-line ((,class :inherit modus-themes-bold :foreground ,magenta-active)))
;;;;; el-search
    `(el-search-highlight-in-prompt-face ((,class :inherit bold :foreground ,magenta-alt)))
    `(el-search-match ((,class :inherit modus-themes-search-success)))
    `(el-search-other-match ((,class :inherit modus-themes-special-mild)))
    `(el-search-occur-match ((,class :inherit modus-themes-special-calm)))
;;;;; eldoc
    ;; NOTE: see https://github.com/purcell/package-lint/issues/187
    (list 'eldoc-highlight-function-argument `((,class :inherit bold
                                                       :background ,yellow-nuanced-bg
                                                       :foreground ,yellow-alt-other)))
;;;;; eldoc-box
    `(eldoc-box-body ((,class :background ,bg-alt :foreground ,fg-main)))
    `(eldoc-box-border ((,class :background ,fg-alt)))
;;;;; elfeed
    `(elfeed-log-date-face ((,class :inherit elfeed-search-date-face)))
    `(elfeed-log-debug-level-face ((,class :inherit elfeed-search-filter-face)))
    `(elfeed-log-error-level-face ((,class :inherit error)))
    `(elfeed-log-info-level-face ((,class :inherit success)))
    `(elfeed-log-warn-level-face ((,class :inherit warning)))
    `(elfeed-search-date-face ((,class :foreground ,cyan)))
    `(elfeed-search-feed-face ((,class :foreground ,blue-faint)))
    `(elfeed-search-filter-face ((,class :inherit bold :foreground ,magenta-active)))
    `(elfeed-search-last-update-face ((,class :inherit bold :foreground ,cyan-active)))
    `(elfeed-search-tag-face ((,class :foreground ,magenta-alt-faint)))
    `(elfeed-search-title-face ((,class :foreground ,fg-dim)))
    `(elfeed-search-unread-count-face ((,class :inherit bold :foreground ,fg-active)))
    `(elfeed-search-unread-title-face ((,class :inherit bold :foreground ,fg-main)))
;;;;; elfeed-score
    `(elfeed-score-date-face ((,class :foreground ,blue)))
    `(elfeed-score-debug-level-face ((,class :foreground ,magenta-alt-other)))
    `(elfeed-score-error-level-face ((,class :foreground ,red)))
    `(elfeed-score-info-level-face ((,class :foreground ,cyan)))
    `(elfeed-score-warn-level-face ((,class :foreground ,yellow)))
;;;;; elpher
    `(elpher-gemini-heading1 ((,class :inherit modus-themes-heading-1)))
    `(elpher-gemini-heading2 ((,class :inherit modus-themes-heading-2)))
    `(elpher-gemini-heading3 ((,class :inherit modus-themes-heading-3)))
;;;;; embark
    `(embark-keybinding ((,class :inherit modus-themes-key-binding)))
    `(embark-collect-marked ((,class :inherit modus-themes-mark-sel)))
;;;;; ement (ement.el)
    `(ement-room-fully-read-marker ((,class :background ,cyan-subtle-bg)))
    `(ement-room-membership ((,class :inherit shadow)))
    `(ement-room-mention ((,class :background ,bg-hl-alt-intense)))
    `(ement-room-name ((,class :inherit bold)))
    `(ement-room-reactions ((,class :inherit shadow)))
    `(ement-room-read-receipt-marker ((,class :background ,yellow-subtle-bg)))
    `(ement-room-self ((,class :inherit bold :foreground ,magenta)))
    `(ement-room-self-message ((,class :foreground ,magenta-faint)))
    `(ement-room-timestamp ((,class :inherit shadow)))
    `(ement-room-timestamp-header ((,class :inherit bold :foreground ,cyan)))
    `(ement-room-user ((,class :inherit bold :foreground ,blue)))
;;;;; emms
    `(emms-browser-album-face ((,class :foreground ,magenta-alt-other)))
    `(emms-browser-artist-face ((,class :foreground ,cyan)))
    `(emms-browser-composer-face ((,class :foreground ,magenta-alt)))
    `(emms-browser-performer-face ((,class :inherit emms-browser-artist-face)))
    `(emms-browser-track-face ((,class :inherit emms-playlist-track-face)))
    `(emms-browser-year/genre-face ((,class :foreground ,cyan-alt-other)))
    `(emms-playlist-track-face ((,class :foreground ,blue-alt)))
    `(emms-playlist-selected-face ((,class :inherit bold :foreground ,blue-alt-other)))
    `(emms-metaplaylist-mode-current-face ((,class :inherit emms-playlist-selected-face)))
    `(emms-metaplaylist-mode-face ((,class :foreground ,cyan)))
;;;;; enh-ruby-mode (enhanced-ruby-mode)
    `(enh-ruby-heredoc-delimiter-face ((,class :inherit font-lock-constant-face)))
    `(enh-ruby-op-face ((,class :foreground ,fg-main)))
    `(enh-ruby-regexp-delimiter-face ((,class :inherit font-lock-regexp-grouping-construct)))
    `(enh-ruby-regexp-face ((,class :inherit font-lock-string-face)))
    `(enh-ruby-string-delimiter-face ((,class :inherit font-lock-string-face)))
    `(erm-syn-errline ((,class :inherit modus-themes-lang-error)))
    `(erm-syn-warnline ((,class :inherit modus-themes-lang-warning)))
;;;;; epa
    `(epa-field-body ((,class :foreground ,fg-main)))
    `(epa-field-name ((,class :inherit bold :foreground ,fg-dim)))
    `(epa-mark ((,class :inherit bold :foreground ,magenta)))
    `(epa-string ((,class :foreground ,blue-alt)))
    `(epa-validity-disabled ((,class :foreground ,red)))
    `(epa-validity-high ((,class :inherit bold :foreground ,cyan)))
    `(epa-validity-low ((,class :inherit shadow)))
    `(epa-validity-medium ((,class :foreground ,green-alt)))
;;;;; equake
    `(equake-buffer-face ((,class :background ,bg-main :foreground ,fg-main)))
    `(equake-shell-type-eshell ((,class :background ,bg-inactive :foreground ,blue-active)))
    `(equake-shell-type-rash ((,class :background ,bg-inactive :foreground ,red-active)))
    `(equake-shell-type-shell ((,class :background ,bg-inactive :foreground ,cyan-active)))
    `(equake-shell-type-term ((,class :background ,bg-inactive :foreground ,yellow-active)))
    `(equake-shell-type-vterm ((,class :background ,bg-inactive :foreground ,magenta-active)))
    `(equake-tab-active ((,class :background ,fg-alt :foreground ,bg-alt)))
    `(equake-tab-inactive ((,class :foreground ,fg-inactive)))
;;;;; erc
    `(erc-action-face ((,class :foreground ,cyan-alt-other)))
    `(erc-bold-face ((,class :inherit bold)))
    `(erc-button ((,class :inherit button)))
    `(erc-command-indicator-face ((,class :inherit bold :foreground ,cyan-alt)))
    `(erc-current-nick-face ((,class :inherit bold :foreground ,red-alt)))
    `(erc-dangerous-host-face ((,class :inherit modus-themes-intense-red)))
    `(erc-direct-msg-face ((,class :foreground ,fg-special-warm)))
    `(erc-error-face ((,class :inherit bold :foreground ,red)))
    `(erc-fool-face ((,class :inherit shadow)))
    `(erc-header-line ((,class :background ,bg-header :foreground ,fg-header)))
    `(erc-input-face ((,class :foreground ,magenta)))
    `(erc-inverse-face ((,class :inherit erc-default-face :inverse-video t)))
    `(erc-keyword-face ((,class :inherit bold :foreground ,magenta-alt-other)))
    `(erc-my-nick-face ((,class :inherit bold :foreground ,magenta)))
    `(erc-my-nick-prefix-face ((,class :inherit erc-my-nick-face)))
    `(erc-nick-default-face ((,class :inherit bold :foreground ,blue)))
    `(erc-nick-msg-face ((,class :inherit warning)))
    `(erc-nick-prefix-face ((,class :inherit erc-nick-default-face)))
    `(erc-notice-face ((,class :inherit font-lock-comment-face)))
    `(erc-pal-face ((,class :inherit bold :foreground ,magenta-alt)))
    `(erc-prompt-face ((,class :inherit modus-themes-prompt)))
    `(erc-timestamp-face ((,class :foreground ,cyan)))
    `(erc-underline-face ((,class :underline t)))
    `(bg:erc-color-face0 ((,class :background "white")))
    `(bg:erc-color-face1 ((,class :background "black")))
    `(bg:erc-color-face10 ((,class :background ,cyan-subtle-bg)))
    `(bg:erc-color-face11 ((,class :background ,cyan-intense-bg)))
    `(bg:erc-color-face12 ((,class :background ,blue-subtle-bg)))
    `(bg:erc-color-face13 ((,class :background ,magenta-subtle-bg)))
    `(bg:erc-color-face14 ((,class :background "gray60")))
    `(bg:erc-color-face15 ((,class :background "gray80")))
    `(bg:erc-color-face2 ((,class :background ,blue-intense-bg)))
    `(bg:erc-color-face3 ((,class :background ,green-intense-bg)))
    `(bg:erc-color-face4 ((,class :background ,red-subtle-bg)))
    `(bg:erc-color-face5 ((,class :background ,red-intense-bg)))
    `(bg:erc-color-face6 ((,class :background ,magenta-refine-bg)))
    `(bg:erc-color-face7 ((,class :background ,yellow-subtle-bg)))
    `(bg:erc-color-face8 ((,class :background ,yellow-refine-bg)))
    `(bg:erc-color-face9 ((,class :background ,green-subtle-bg)))
    `(fg:erc-color-face0 ((,class :foreground "white")))
    `(fg:erc-color-face1 ((,class :foreground "black")))
    `(fg:erc-color-face10 ((,class :foreground ,cyan)))
    `(fg:erc-color-face11 ((,class :foreground ,cyan-alt-other)))
    `(fg:erc-color-face12 ((,class :foreground ,blue)))
    `(fg:erc-color-face13 ((,class :foreground ,magenta-alt)))
    `(fg:erc-color-face14 ((,class :foreground "gray60")))
    `(fg:erc-color-face15 ((,class :foreground "gray80")))
    `(fg:erc-color-face2 ((,class :foreground ,blue-alt-other)))
    `(fg:erc-color-face3 ((,class :foreground ,green)))
    `(fg:erc-color-face4 ((,class :foreground ,red)))
    `(fg:erc-color-face5 ((,class :foreground ,red-alt)))
    `(fg:erc-color-face6 ((,class :foreground ,magenta-alt-other)))
    `(fg:erc-color-face7 ((,class :foreground ,yellow-alt-other)))
    `(fg:erc-color-face8 ((,class :foreground ,yellow-alt)))
    `(fg:erc-color-face9 ((,class :foreground ,green-alt-other)))
;;;;; eros
    `(eros-result-overlay-face ((,class :box (:line-width -1 :color ,blue)
                                        :background ,bg-dim :foreground ,fg-dim)))
;;;;; ert
    `(ert-test-result-expected ((,class :inherit modus-themes-intense-green)))
    `(ert-test-result-unexpected ((,class :inherit modus-themes-intense-red)))
;;;;; eshell
    `(eshell-ls-archive ((,class :foreground ,cyan-alt)))
    `(eshell-ls-backup ((,class :inherit shadow)))
    `(eshell-ls-clutter ((,class :foreground ,red-alt)))
    `(eshell-ls-directory ((,class :foreground ,blue-alt)))
    `(eshell-ls-executable ((,class :foreground ,magenta-alt)))
    `(eshell-ls-missing ((,class :inherit modus-themes-intense-red)))
    `(eshell-ls-product ((,class :inherit shadow)))
    `(eshell-ls-readonly ((,class :foreground ,yellow-faint)))
    `(eshell-ls-special ((,class :foreground ,magenta)))
    `(eshell-ls-symlink ((,class :inherit modus-themes-link-symlink)))
    `(eshell-ls-unreadable ((,class :background ,bg-inactive :foreground ,fg-inactive)))
    `(eshell-prompt ((,class :inherit modus-themes-prompt)))
;;;;; eshell-fringe-status
    `(eshell-fringe-status-failure ((,class :inherit error)))
    `(eshell-fringe-status-success ((,class :inherit success)))
;;;;; eshell-git-prompt
    `(eshell-git-prompt-add-face ((,class :foreground ,magenta-alt-other)))
    `(eshell-git-prompt-branch-face ((,class :foreground ,magenta-alt)))
    `(eshell-git-prompt-directory-face ((,class :inherit bold :foreground ,blue)))
    `(eshell-git-prompt-exit-fail-face ((,class :inherit error)))
    `(eshell-git-prompt-exit-success-face ((,class :inherit success)))
    `(eshell-git-prompt-modified-face ((,class :foreground ,yellow)))
    `(eshell-git-prompt-powerline-clean-face ((,class :background ,green-refine-bg)))
    `(eshell-git-prompt-powerline-dir-face ((,class :background ,blue-refine-bg)))
    `(eshell-git-prompt-powerline-not-clean-face ((,class :background ,yellow-fringe-bg)))
    `(eshell-git-prompt-robyrussell-branch-face ((,class :foreground ,magenta-alt)))
    `(eshell-git-prompt-robyrussell-git-dirty-face ((,class :foreground ,yellow)))
    `(eshell-git-prompt-robyrussell-git-face ((,class :foreground ,magenta-alt-other)))
;;;;; eshell-prompt-extras (epe)
    `(epe-dir-face ((,class :inherit bold :foreground ,blue)))
    `(epe-git-dir-face ((,class :foreground ,red-alt-other)))
    `(epe-git-face ((,class :foreground ,magenta-alt)))
    `(epe-pipeline-delimiter-face ((,class :inherit shadow)))
    `(epe-pipeline-host-face ((,class :foreground ,fg-main)))
    `(epe-pipeline-time-face ((,class :foreground ,fg-main)))
    `(epe-pipeline-user-face ((,class :foreground ,magenta-alt-other)))
    `(epe-remote-face ((,class :inherit (shadow modus-themes-slant))))
    `(epe-status-face ((,class :foreground ,magenta-alt-other)))
    `(epe-venv-face ((,class :inherit (shadow modus-themes-slant))))
;;;;; eshell-syntax-highlighting
    `(eshell-syntax-highlighting-directory-face ((,class :inherit eshell-ls-directory)))
    `(eshell-syntax-highlighting-invalid-face ((,class :foreground ,red)))
    `(eshell-syntax-highlighting-shell-command-face ((,class :foreground ,fg-main)))
;;;;; evil-mode
    `(evil-ex-commands ((,class :foreground ,magenta-alt-other)))
    `(evil-ex-info ((,class :foreground ,cyan-alt-other)))
    `(evil-ex-lazy-highlight ((,class :inherit modus-themes-search-success-lazy)))
    `(evil-ex-search ((,class :inherit modus-themes-search-success)))
    `(evil-ex-substitute-matches ((,class :inherit modus-themes-refine-yellow :underline t)))
    `(evil-ex-substitute-replacement ((,class :inherit modus-themes-search-success)))
;;;;; evil-goggles
    `(evil-goggles-change-face ((,class :inherit modus-themes-refine-yellow)))
    `(evil-goggles-commentary-face ((,class :inherit (modus-themes-subtle-neutral modus-themes-slant))))
    `(evil-goggles-default-face ((,class :inherit modus-themes-subtle-neutral)))
    `(evil-goggles-delete-face ((,class :inherit modus-themes-refine-red)))
    `(evil-goggles-fill-and-move-face ((,class :inherit evil-goggles-default-face)))
    `(evil-goggles-indent-face ((,class :inherit evil-goggles-default-face)))
    `(evil-goggles-join-face ((,class :inherit modus-themes-subtle-green)))
    `(evil-goggles-nerd-commenter-face ((,class :inherit evil-goggles-commentary-face)))
    `(evil-goggles-paste-face ((,class :inherit modus-themes-subtle-cyan)))
    `(evil-goggles-record-macro-face ((,class :inherit modus-themes-special-cold)))
    `(evil-goggles-replace-with-register-face ((,class :inherit modus-themes-refine-magenta)))
    `(evil-goggles-set-marker-face ((,class :inherit modus-themes-intense-magenta)))
    `(evil-goggles-shift-face ((,class :inherit evil-goggles-default-face)))
    `(evil-goggles-surround-face ((,class :inherit evil-goggles-default-face)))
    `(evil-goggles-yank-face ((,class :inherit modus-themes-subtle-blue)))
;;;;; evil-snipe
    `(evil-snipe-first-match-face ((,class :inherit (bold modus-themes-intense-blue))))
    `(evil-snipe-matches-face ((,class :inherit modus-themes-refine-magenta)))
;;;;; evil-visual-mark-mode
    `(evil-visual-mark-face ((,class :inherit modus-themes-intense-magenta)))
;;;;; eww
    `(eww-invalid-certificate ((,class :foreground ,red-faint)))
    `(eww-valid-certificate ((,class :foreground ,blue-faint)))
    `(eww-form-checkbox ((,class :inherit eww-form-text)))
    `(eww-form-file ((,class :inherit eww-form-submit)))
    `(eww-form-select ((,class :inherit eww-form-submit)))
    `(eww-form-submit ((,class :inherit modus-themes-box-button)))
    `(eww-form-text ((,class :inherit widget-field)))
    `(eww-form-textarea ((,class :inherit eww-form-text)))
;;;;; eyebrowse
    `(eyebrowse-mode-line-active ((,class :inherit bold :foreground ,blue-active)))
;;;;; fancy-dabbrev
    `(fancy-dabbrev-menu-face ((,class :background ,bg-alt :foreground ,fg-alt)))
    `(fancy-dabbrev-preview-face ((,class :inherit shadow :underline t)))
    `(fancy-dabbrev-selection-face ((,class :inherit (modus-themes-intense-cyan bold))))
;;;;; flycheck
    `(flycheck-error ((,class :inherit modus-themes-lang-error)))
    `(flycheck-error-list-checker-name ((,class :foreground ,magenta-active)))
    `(flycheck-error-list-column-number ((,class :foreground ,fg-special-cold)))
    `(flycheck-error-list-error ((,class :inherit modus-themes-bold :foreground ,red)))
    `(flycheck-error-list-filename ((,class :foreground ,blue)))
    `(flycheck-error-list-highlight ((,class :inherit modus-themes-hl-line)))
    `(flycheck-error-list-id ((,class :foreground ,magenta-alt-other)))
    `(flycheck-error-list-id-with-explainer ((,class :inherit flycheck-error-list-id :box t)))
    `(flycheck-error-list-info ((,class :foreground ,cyan)))
    `(flycheck-error-list-line-number ((,class :foreground ,fg-special-warm)))
    `(flycheck-error-list-warning ((,class :foreground ,yellow)))
    `(flycheck-fringe-error ((,class :inherit modus-themes-fringe-red)))
    `(flycheck-fringe-info ((,class :inherit modus-themes-fringe-cyan)))
    `(flycheck-fringe-warning ((,class :inherit modus-themes-fringe-yellow)))
    `(flycheck-info ((,class :inherit modus-themes-lang-note)))
    `(flycheck-verify-select-checker ((,class :box (:line-width 1 :color nil :style released-button))))
    `(flycheck-warning ((,class :inherit modus-themes-lang-warning)))
;;;;; flycheck-color-mode-line
    `(flycheck-color-mode-line-error-face ((,class :inherit flycheck-fringe-error)))
    `(flycheck-color-mode-line-info-face ((,class :inherit flycheck-fringe-info)))
    `(flycheck-color-mode-line-running-face ((,class :inherit italic :foreground ,fg-inactive)))
    `(flycheck-color-mode-line-info-face ((,class :inherit flycheck-fringe-warning)))
;;;;; flycheck-indicator
    `(flycheck-indicator-disabled ((,class :inherit modus-themes-slant :foreground ,fg-inactive)))
    `(flycheck-indicator-error ((,class :inherit modus-themes-bold :foreground ,red-active)))
    `(flycheck-indicator-info ((,class :inherit modus-themes-bold :foreground ,blue-active)))
    `(flycheck-indicator-running ((,class :inherit modus-themes-bold :foreground ,magenta-active)))
    `(flycheck-indicator-success ((,class :inherit (modus-themes-bold modus-themes-grue-active))))
    `(flycheck-indicator-warning ((,class :inherit modus-themes-bold :foreground ,yellow-active)))
;;;;; flycheck-posframe
    `(flycheck-posframe-background-face ((,class :background ,bg-alt)))
    `(flycheck-posframe-border-face ((,class :inherit shadow)))
    `(flycheck-posframe-error-face ((,class :inherit bold :foreground ,red)))
    `(flycheck-posframe-face ((,class :inherit modus-themes-slant :foreground ,fg-main)))
    `(flycheck-posframe-info-face ((,class :inherit bold :foreground ,cyan)))
    `(flycheck-posframe-warning-face ((,class :inherit bold :foreground ,yellow)))
;;;;; flymake
    `(flymake-error ((,class :inherit modus-themes-lang-error)))
    `(flymake-note ((,class :inherit modus-themes-lang-note)))
    `(flymake-warning ((,class :inherit modus-themes-lang-warning)))
;;;;; flyspell
    `(flyspell-duplicate ((,class :inherit modus-themes-lang-warning)))
    `(flyspell-incorrect ((,class :inherit modus-themes-lang-error)))
;;;;; flx
    `(flx-highlight-face ((,class :inherit modus-themes-completion-match-0)))
;;;;; freeze-it
    `(freeze-it-show ((,class :background ,bg-dim :foreground ,fg-special-warm)))
;;;;; focus
    `(focus-unfocused ((,class :foreground ,fg-unfocused)))
;;;;; fold-this
    `(fold-this-overlay ((,class :inherit modus-themes-special-mild)))
;;;;; font-lock
    `(font-lock-builtin-face ((,class :inherit modus-themes-bold
                                      ,@(modus-themes--syntax-extra
                                         magenta-alt magenta-alt-faint
                                         magenta magenta-faint))))
    `(font-lock-comment-delimiter-face ((,class :inherit font-lock-comment-face)))
    `(font-lock-comment-face ((,class :inherit modus-themes-slant
                                      ,@(modus-themes--syntax-comment
                                         fg-alt fg-comment-yellow yellow-alt-other-faint))))
    `(font-lock-constant-face ((,class ,@(modus-themes--syntax-extra
                                          blue-alt-other blue-alt-other-faint
                                          magenta-alt-other magenta-alt-other-faint))))
    `(font-lock-doc-face ((,class :inherit modus-themes-slant
                                  ,@(modus-themes--syntax-string
                                     fg-docstring fg-special-cold
                                     fg-special-mild fg-special-calm
                                     fg-special-mild magenta-nuanced-fg))))
    `(font-lock-function-name-face ((,class ,@(modus-themes--syntax-extra
                                               magenta magenta-faint
                                               magenta-alt magenta-alt-faint))))
    `(font-lock-keyword-face ((,class :inherit modus-themes-bold
                                      ,@(modus-themes--syntax-extra
                                         magenta-alt-other magenta-alt-other-faint
                                         cyan cyan-faint))))
    `(font-lock-negation-char-face ((,class :inherit modus-themes-bold
                                            ,@(modus-themes--syntax-foreground
                                               yellow yellow-faint))))
    `(font-lock-preprocessor-face ((,class ,@(modus-themes--syntax-extra
                                              red-alt-other red-alt-other-faint
                                              cyan-alt-other cyan-alt-faint))))
    `(font-lock-regexp-grouping-backslash ((,class :inherit modus-themes-bold
                                                   ,@(modus-themes--syntax-string
                                                      fg-escape-char-backslash yellow-alt-faint
                                                      yellow-alt magenta-alt
                                                      red-faint green-alt-other-faint))))
    `(font-lock-regexp-grouping-construct ((,class :inherit modus-themes-bold
                                                   ,@(modus-themes--syntax-string
                                                      fg-escape-char-construct red-alt-other-faint
                                                      red-alt-other blue-alt-other
                                                      blue-faint blue-alt-other-faint))))
    `(font-lock-string-face ((,class ,@(modus-themes--syntax-string
                                        blue-alt blue-alt-faint
                                        green-alt-other red-alt-other
                                        green-alt-faint red-alt-faint))))
    `(font-lock-type-face ((,class :inherit modus-themes-bold
                                   ,@(modus-themes--syntax-extra
                                      cyan-alt-other cyan-alt-faint
                                      magenta-alt-other magenta-alt-other-faint))))
    `(font-lock-variable-name-face ((,class ,@(modus-themes--syntax-extra
                                               cyan cyan-faint
                                               blue-alt blue-alt-faint))))
    `(font-lock-warning-face ((,class :inherit modus-themes-bold
                                      ,@(modus-themes--syntax-comment
                                         yellow red yellow-alt-faint red-faint))))
;;;;; forge
    `(forge-post-author ((,class :inherit bold :foreground ,fg-main)))
    `(forge-post-date ((,class :foreground ,fg-special-cold)))
    `(forge-topic-closed ((,class :inherit shadow)))
    `(forge-topic-merged ((,class :inherit shadow)))
    `(forge-topic-open ((,class :foreground ,fg-special-mild)))
    `(forge-topic-unmerged ((,class :inherit modus-themes-slant :foreground ,magenta)))
    `(forge-topic-unread ((,class :inherit bold :foreground ,fg-main)))
;;;;; fountain-mode
    `(fountain-character ((,class :foreground ,blue-alt-other)))
    `(fountain-comment ((,class :inherit font-lock-comment-face)))
    `(fountain-dialog ((,class :foreground ,blue-alt)))
    `(fountain-metadata-key ((,class :foreground ,green-alt-other)))
    `(fountain-metadata-value ((,class :foreground ,blue)))
    `(fountain-non-printing ((,class :inherit shadow)))
    `(fountain-note ((,class :inherit modus-themes-slant :foreground ,yellow)))
    `(fountain-page-break ((,class :inherit bold :foreground ,red-alt)))
    `(fountain-page-number ((,class :inherit bold :foreground ,red-alt-other)))
    `(fountain-paren ((,class :foreground ,cyan)))
    `(fountain-scene-heading ((,class :inherit bold :foreground ,blue-nuanced-fg)))
    `(fountain-section-heading ((,class :inherit modus-themes-heading-1)))
    `(fountain-section-heading-1 ((,class :inherit modus-themes-heading-1)))
    `(fountain-section-heading-2 ((,class :inherit modus-themes-heading-2)))
    `(fountain-section-heading-3 ((,class :inherit modus-themes-heading-3)))
    `(fountain-section-heading-4 ((,class :inherit modus-themes-heading-4)))
    `(fountain-section-heading-5 ((,class :inherit modus-themes-heading-5)))
    `(fountain-synopsis ((,class :foreground ,cyan-alt)))
    `(fountain-trans ((,class :foreground ,yellow-alt-other)))
;;;;; geiser
    `(geiser-font-lock-autodoc-current-arg ((,class :inherit bold
                                                    :background ,yellow-nuanced-bg
                                                    :foreground ,yellow-alt-other)))
    `(geiser-font-lock-autodoc-identifier ((,class :foreground ,cyan)))
    `(geiser-font-lock-doc-button ((,class :inherit button :foreground ,fg-docstring)))
    `(geiser-font-lock-doc-link ((,class :inherit button)))
    `(geiser-font-lock-error-link ((,class :inherit button :foreground ,red)))
    `(geiser-font-lock-image-button ((,class :inherit button :foreground ,green-alt)))
    `(geiser-font-lock-repl-input ((,class :inherit bold)))
    `(geiser-font-lock-repl-output ((,class :inherit font-lock-keyword-face)))
    `(geiser-font-lock-repl-prompt ((,class :inherit modus-themes-prompt)))
    `(geiser-font-lock-xref-header ((,class :inherit bold)))
    `(geiser-font-lock-xref-link ((,class :inherit button)))
;;;;; git-commit
    `(git-commit-comment-action ((,class :inherit font-lock-comment-face)))
    `(git-commit-comment-branch-local ((,class :inherit font-lock-comment-face :foreground ,blue-alt)))
    `(git-commit-comment-branch-remote ((,class :inherit font-lock-comment-face :foreground ,magenta-alt)))
    `(git-commit-comment-detached ((,class :inherit font-lock-comment-face :foreground ,cyan-alt)))
    `(git-commit-comment-file ((,class :inherit font-lock-comment-face :foreground ,cyan)))
    `(git-commit-comment-heading ((,class :inherit (bold font-lock-comment-face))))
    `(git-commit-keyword ((,class :foreground ,magenta)))
    `(git-commit-known-pseudo-header ((,class :foreground ,cyan-alt-other)))
    `(git-commit-nonempty-second-line ((,class :inherit error)))
    `(git-commit-overlong-summary ((,class :inherit warning)))
    `(git-commit-pseudo-header ((,class :foreground ,blue)))
    `(git-commit-summary ((,class :inherit bold :foreground ,blue)))
;;;;; git-gutter
    `(git-gutter:added ((,class :inherit modus-themes-grue-background-active)))
    `(git-gutter:deleted ((,class :inherit modus-themes-fringe-red)))
    `(git-gutter:modified ((,class :inherit modus-themes-fringe-yellow)))
    `(git-gutter:separator ((,class :inherit modus-themes-fringe-cyan)))
    `(git-gutter:unchanged ((,class :inherit modus-themes-fringe-magenta)))
;;;;; git-gutter-fr
    `(git-gutter-fr:added ((,class :inherit modus-themes-grue-background-active)))
    `(git-gutter-fr:deleted ((,class :inherit modus-themes-fringe-red)))
    `(git-gutter-fr:modified ((,class :inherit modus-themes-fringe-yellow)))
;;;;; git-rebase
    `(git-rebase-comment-hash ((,class :inherit font-lock-comment-face :foreground ,cyan)))
    `(git-rebase-comment-heading  ((,class :inherit (bold font-lock-comment-face))))
    `(git-rebase-description ((,class :foreground ,fg-main)))
    `(git-rebase-hash ((,class :foreground ,cyan-alt-other)))
;;;;; git-timemachine
    `(git-timemachine-commit ((,class :inherit bold :foreground ,yellow-active)))
    `(git-timemachine-minibuffer-author-face ((,class :foreground ,fg-special-warm)))
    `(git-timemachine-minibuffer-detail-face ((,class :foreground ,red-alt)))
;;;;; gnus
    `(gnus-button ((,class :inherit button)))
    `(gnus-cite-1 ((,class :inherit message-cited-text-1)))
    `(gnus-cite-2 ((,class :inherit message-cited-text-2)))
    `(gnus-cite-3 ((,class :inherit message-cited-text-3)))
    `(gnus-cite-4 ((,class :inherit message-cited-text-4)))
    `(gnus-cite-5 ((,class :inherit gnus-cite-1)))
    `(gnus-cite-6 ((,class :inherit gnus-cite-2)))
    `(gnus-cite-7 ((,class :inherit gnus-cite-3)))
    `(gnus-cite-8 ((,class :inherit gnus-cite-4)))
    `(gnus-cite-9 ((,class :inherit gnus-cite-1)))
    `(gnus-cite-10 ((,class :inherit gnus-cite-2)))
    `(gnus-cite-11 ((,class :inherit gnus-cite-3)))
    `(gnus-cite-attribution ((,class :inherit italic :foreground ,fg-main)))
    `(gnus-emphasis-bold ((,class :inherit bold)))
    `(gnus-emphasis-bold-italic ((,class :inherit bold-italic)))
    `(gnus-emphasis-highlight-words ((,class :inherit modus-themes-refine-yellow)))
    `(gnus-emphasis-italic ((,class :inherit italic)))
    `(gnus-emphasis-underline-bold ((,class :inherit gnus-emphasis-bold :underline t)))
    `(gnus-emphasis-underline-bold-italic ((,class :inherit gnus-emphasis-bold-italic :underline t)))
    `(gnus-emphasis-underline-italic ((,class :inherit gnus-emphasis-italic :underline t)))
    `(gnus-group-mail-1 ((,class :inherit bold :foreground ,magenta-alt)))
    `(gnus-group-mail-1-empty ((,class :foreground ,magenta-alt)))
    `(gnus-group-mail-2 ((,class :inherit bold :foreground ,magenta)))
    `(gnus-group-mail-2-empty ((,class :foreground ,magenta)))
    `(gnus-group-mail-3 ((,class :inherit bold :foreground ,magenta-alt-other)))
    `(gnus-group-mail-3-empty ((,class :foreground ,magenta-alt-other)))
    `(gnus-group-mail-low ((,class :inherit bold :foreground ,magenta-nuanced-fg)))
    `(gnus-group-mail-low-empty ((,class :foreground ,magenta-nuanced-fg)))
    `(gnus-group-news-1 ((,class :inherit bold :foreground ,green)))
    `(gnus-group-news-1-empty ((,class :foreground ,green)))
    `(gnus-group-news-2 ((,class :inherit bold :foreground ,cyan)))
    `(gnus-group-news-2-empty ((,class :foreground ,cyan)))
    `(gnus-group-news-3 ((,class :inherit bold :foreground ,yellow-nuanced-fg)))
    `(gnus-group-news-3-empty ((,class :foreground ,yellow-nuanced-fg)))
    `(gnus-group-news-4 ((,class :inherit bold :foreground ,cyan-nuanced-fg)))
    `(gnus-group-news-4-empty ((,class :foreground ,cyan-nuanced-fg)))
    `(gnus-group-news-5 ((,class :inherit bold :foreground ,red-nuanced-fg)))
    `(gnus-group-news-5-empty ((,class :foreground ,red-nuanced-fg)))
    `(gnus-group-news-6 ((,class :inherit bold :foreground ,fg-unfocused)))
    `(gnus-group-news-6-empty ((,class :foreground ,fg-unfocused)))
    `(gnus-group-news-low ((,class :inherit bold :foreground ,green-nuanced-fg)))
    `(gnus-group-news-low-empty ((,class :foreground ,green-nuanced-fg)))
    `(gnus-header-content ((,class :inherit message-header-other)))
    `(gnus-header-from ((,class :inherit message-header-to :underline nil)))
    `(gnus-header-name ((,class :inherit message-header-name)))
    `(gnus-header-newsgroups ((,class :inherit message-header-newsgroups)))
    `(gnus-header-subject ((,class :inherit message-header-subject)))
    `(gnus-server-agent ((,class :inherit bold :foreground ,cyan)))
    `(gnus-server-closed ((,class :inherit bold :foreground ,magenta)))
    `(gnus-server-cloud ((,class :inherit bold :foreground ,cyan-alt)))
    `(gnus-server-cloud-host ((,class :inherit modus-themes-refine-cyan)))
    `(gnus-server-denied ((,class :inherit bold :foreground ,red)))
    `(gnus-server-offline ((,class :inherit bold :foreground ,yellow)))
    `(gnus-server-opened ((,class :inherit bold :foreground ,green)))
    `(gnus-signature ((,class :inherit italic :foreground ,fg-special-cold)))
    `(gnus-splash ((,class :inherit shadow)))
    `(gnus-summary-cancelled ((,class :inherit modus-themes-mark-alt :extend t)))
    `(gnus-summary-high-ancient ((,class :inherit bold :foreground ,fg-alt)))
    `(gnus-summary-high-read ((,class :inherit bold :foreground ,fg-special-cold)))
    `(gnus-summary-high-ticked ((,class :inherit bold :foreground ,red-alt-other)))
    `(gnus-summary-high-undownloaded ((,class :inherit bold :foreground ,yellow)))
    `(gnus-summary-high-unread ((,class :inherit bold :foreground ,fg-main)))
    `(gnus-summary-low-ancient ((,class :inherit italic :foreground ,fg-alt)))
    `(gnus-summary-low-read ((,class :inherit italic :foreground ,fg-alt)))
    `(gnus-summary-low-ticked ((,class :inherit italic :foreground ,red-refine-fg)))
    `(gnus-summary-low-undownloaded ((,class :inherit italic :foreground ,yellow-refine-fg)))
    `(gnus-summary-low-unread ((,class :inherit italic :foreground ,fg-special-cold)))
    `(gnus-summary-normal-ancient ((,class :foreground ,fg-special-calm)))
    `(gnus-summary-normal-read ((,class :inherit shadow)))
    `(gnus-summary-normal-ticked ((,class :foreground ,red-alt-other)))
    `(gnus-summary-normal-undownloaded ((,class :foreground ,yellow)))
    `(gnus-summary-normal-unread ((,class :foreground ,fg-main)))
    `(gnus-summary-selected ((,class :inherit highlight :extend t)))
;;;;; gotest
    `(go-test--ok-face ((,class :inherit success)))
    `(go-test--error-face ((,class :inherit error)))
    `(go-test--warning-face ((,class :inherit warning)))
    `(go-test--pointer-face ((,class :foreground ,magenta-alt-other)))
    `(go-test--standard-face ((,class :foreground ,fg-special-cold)))
;;;;; golden-ratio-scroll-screen
    `(golden-ratio-scroll-highlight-line-face ((,class :background ,cyan-subtle-bg :foreground ,fg-main)))
;;;;; helm
    `(helm-M-x-key ((,class :inherit modus-themes-key-binding)))
    `(helm-action ((,class :underline t)))
    `(helm-bookmark-addressbook ((,class :foreground ,green-alt)))
    `(helm-bookmark-directory ((,class :inherit bold :foreground ,blue)))
    `(helm-bookmark-file ((,class :foreground ,fg-main)))
    `(helm-bookmark-file-not-found ((,class :background ,bg-alt :foreground ,fg-alt)))
    `(helm-bookmark-gnus ((,class :foreground ,magenta)))
    `(helm-bookmark-info ((,class :foreground ,cyan-alt)))
    `(helm-bookmark-man ((,class :foreground ,yellow-alt)))
    `(helm-bookmark-w3m ((,class :foreground ,blue-alt)))
    `(helm-buffer-archive ((,class :inherit bold :foreground ,cyan)))
    `(helm-buffer-directory ((,class :inherit bold :foreground ,blue)))
    `(helm-buffer-file ((,class :foreground ,fg-main)))
    `(helm-buffer-modified ((,class :foreground ,yellow-alt)))
    `(helm-buffer-not-saved ((,class :foreground ,red-alt)))
    `(helm-buffer-process ((,class :foreground ,magenta)))
    `(helm-buffer-saved-out ((,class :inherit bold :background ,bg-alt :foreground ,red)))
    `(helm-buffer-size ((,class :inherit shadow)))
    `(helm-candidate-number ((,class :foreground ,cyan-active)))
    `(helm-candidate-number-suspended ((,class :foreground ,yellow-active)))
    `(helm-comint-prompts-buffer-name ((,class :foreground ,green-active)))
    `(helm-comint-prompts-promptidx ((,class :foreground ,cyan-active)))
    `(helm-delete-async-message ((,class :inherit bold :foreground ,magenta-active)))
    `(helm-eob-line ((,class :background ,bg-main :foreground ,fg-main)))
    `(helm-eshell-prompts-buffer-name ((,class :foreground ,green-active)))
    `(helm-eshell-prompts-promptidx ((,class :foreground ,cyan-active)))
    `(helm-etags-file ((,class :foreground ,fg-dim :underline t)))
    `(helm-ff-backup-file ((,class :inherit shadow)))
    `(helm-ff-denied ((,class :inherit modus-themes-intense-red)))
    `(helm-ff-directory ((,class :inherit helm-buffer-directory)))
    `(helm-ff-dirs ((,class :inherit bold :foreground ,blue-alt-other)))
    `(helm-ff-dotted-directory ((,class :inherit bold :background ,bg-alt :foreground ,fg-alt)))
    `(helm-ff-dotted-symlink-directory ((,class :inherit (button helm-ff-dotted-directory))))
    `(helm-ff-executable ((,class :foreground ,magenta-alt)))
    `(helm-ff-file ((,class :foreground ,fg-main)))
    `(helm-ff-file-extension ((,class :foreground ,fg-special-warm)))
    `(helm-ff-invalid-symlink ((,class :inherit modus-themes-link-broken)))
    `(helm-ff-pipe ((,class :inherit modus-themes-special-calm)))
    `(helm-ff-prefix ((,class :inherit modus-themes-special-warm)))
    `(helm-ff-socket ((,class :foreground ,red-alt-other)))
    `(helm-ff-suid ((,class :inherit modus-themes-special-warm)))
    `(helm-ff-symlink ((,class :inherit modus-themes-link-symlink)))
    `(helm-ff-truename ((,class :foreground ,blue-alt-other)))
    `(helm-fd-finish ((,class :inherit success)))
    `(helm-grep-cmd-line ((,class :foreground ,yellow-alt-other)))
    `(helm-grep-file ((,class :inherit bold :foreground ,fg-special-cold)))
    `(helm-grep-finish ((,class :inherit bold)))
    `(helm-grep-lineno ((,class :foreground ,fg-special-warm)))
    `(helm-grep-match ((,class :inherit modus-themes-special-calm)))
    `(helm-header ((,class :inherit bold :foreground ,fg-special-cold)))
    `(helm-header-line-left-margin ((,class :inherit bold :foreground ,yellow-intense)))
    `(helm-history-deleted ((,class :inherit modus-themes-special-warm)))
    `(helm-history-remote ((,class :foreground ,red-alt-other)))
    `(helm-lisp-completion-info ((,class :inherit modus-themes-bold :foreground ,fg-special-cold)))
    `(helm-lisp-show-completion ((,class :inherit modus-themes-special-warm)))
    `(helm-locate-finish ((,class :inherit success)))
    `(helm-match ((,class :inherit modus-themes-completion-match-0)))
    `(helm-match-item ((,class :inherit helm-match)))
    `(helm-minibuffer-prompt ((,class :inherit modus-themes-prompt)))
    `(helm-moccur-buffer ((,class :inherit button :foreground ,cyan-alt-other)))
    `(helm-mode-prefix ((,class :inherit modus-themes-special-calm)))
    `(helm-non-file-buffer ((,class :inherit shadow)))
    `(helm-prefarg ((,class :foreground ,red-active)))
    `(helm-resume-need-update ((,class :inherit modus-themes-special-calm)))
    `(helm-selection ((,class :inherit modus-themes-completion-selected)))
    `(helm-selection-line ((,class :background ,bg-hl-alt-intense)))
    `(helm-separator ((,class :foreground ,fg-special-mild)))
    `(helm-time-zone-current ((,class :foreground ,green)))
    `(helm-time-zone-home ((,class :foreground ,magenta)))
    `(helm-source-header ((,class :inherit modus-themes-pseudo-header :foreground ,fg-special-warm)))
    `(helm-top-columns ((,class :inherit helm-header)))
    `(helm-ucs-char ((,class :foreground ,yellow-alt-other)))
    `(helm-visible-mark ((,class :inherit modus-themes-subtle-cyan)))
;;;;; helm-ls-git
    `(helm-ls-git-added-copied-face ((,class :foreground ,green-intense)))
    `(helm-ls-git-added-modified-face ((,class :foreground ,yellow-intense)))
    `(helm-ls-git-conflict-face ((,class :inherit bold :foreground ,red-intense)))
    `(helm-ls-git-deleted-and-staged-face ((,class :foreground ,red-nuanced-fg)))
    `(helm-ls-git-deleted-not-staged-face ((,class :foreground ,red)))
    `(helm-ls-git-modified-and-staged-face ((,class :foreground ,yellow-nuanced-fg)))
    `(helm-ls-git-modified-not-staged-face ((,class :foreground ,yellow)))
    `(helm-ls-git-renamed-modified-face ((,class :foreground ,magenta)))
    `(helm-ls-git-untracked-face ((,class :foreground ,fg-special-cold)))
;;;;; helm-switch-shell
    `(helm-switch-shell-new-shell-face ((,class :inherit modus-themes-completion-match-0)))
;;;;; helm-xref
    `(helm-xref-file-name ((,class :inherit modus-themes-bold :foreground ,fg-special-cold)))
;;;;; helpful
    `(helpful-heading ((,class :inherit modus-themes-heading-1)))
;;;;; highlight region or ad-hoc regexp
    ;; HACK 2022-06-23: The :inverse-video prevents hl-line-mode from
    ;; overriding the background.  Such an override really defeats the
    ;; purpose of setting those highlights.
    ;;
    ;; NOTE 2022-10-04: We do not use the ,class here but instead
    ;; hardcode color values.  We have to do this as the themes lack
    ;; entries in their palette for such an edge case.  Defining those
    ;; entries is not appropriate.
    `(hi-aquamarine ((((class color) (min-colors 88) (background light))
                      :background "white" :foreground "#227f9f" :inverse-video t)
                     (((class color) (min-colors 88) (background dark))
                      :background "black" :foreground "#66cbdc" :inverse-video t)))
    `(hi-black-b ((,class :inverse-video t)))
    `(hi-black-hb ((,class :background ,bg-main :foreground ,fg-alt :inverse-video t)))
    `(hi-blue ((((class color) (min-colors 88) (background light))
                :background "white" :foreground "#3366dd" :inverse-video t)
               (((class color) (min-colors 88) (background dark))
                :background "black" :foreground "#aaccff" :inverse-video t)))
    `(hi-blue-b ((,class :inherit (bold hi-blue))))
    `(hi-green ((((class color) (min-colors 88) (background light))
                  :background "white" :foreground "#008a00" :inverse-video t)
                 (((class color) (min-colors 88) (background dark))
                  :background "black" :foreground "#66dd66" :inverse-video t)))
    `(hi-green-b ((,class :inherit (bold hi-green))))
    `(hi-pink ((((class color) (min-colors 88) (background light))
                  :background "white" :foreground "#bd30aa" :inverse-video t)
                 (((class color) (min-colors 88) (background dark))
                  :background "black" :foreground "#ff88ee" :inverse-video t)))
    `(hi-red-b ((((class color) (min-colors 88) (background light))
                  :background "white" :foreground "#dd0000" :inverse-video t)
                 (((class color) (min-colors 88) (background dark))
                  :background "black" :foreground "#f06666" :inverse-video t)))
    `(hi-salmon ((((class color) (min-colors 88) (background light))
                  :background "white" :foreground "#bf555a" :inverse-video t)
                 (((class color) (min-colors 88) (background dark))
                  :background "black" :foreground "#e08a50" :inverse-video t)))
    `(hi-yellow ((((class color) (min-colors 88) (background light))
                  :background "white" :foreground "#af6400" :inverse-video t)
                 (((class color) (min-colors 88) (background dark))
                  :background "black" :foreground "#faea00" :inverse-video t)))
    `(highlight ((,class ,@(if modus-themes-intense-mouseovers
                               (list :background blue-intense-bg :foreground fg-main)
                             (list :background cyan-subtle-bg :foreground fg-main)))))
    `(highlight-changes ((,class :foreground ,red-alt :underline nil)))
    `(highlight-changes-delete ((,class :background ,red-nuanced-bg
                                        :foreground ,red :underline t)))
    `(hl-line ((,class :inherit modus-themes-hl-line)))
;;;;; highlight-indentation
    `(highlight-indentation-face ((,class :inherit modus-themes-hl-line)))
    `(highlight-indentation-current-column-face ((,class :background ,bg-active)))
;;;;; highlight-numbers
    `(highlight-numbers-number ((,class :foreground ,blue-alt-other)))
;;;;; highlight-thing
    `(highlight-thing ((,class :inherit modus-themes-special-calm)))
;;;;; hl-defined
    `(hdefd-functions ((,class :foreground ,blue)))
    `(hdefd-undefined ((,class :foreground ,red-alt)))
    `(hdefd-variables ((,class :foreground ,cyan-alt)))
;;;;; hl-fill-column
    `(hl-fill-column-face ((,class :background ,bg-active :foreground ,fg-active)))
;;;;; hl-todo
    `(hl-todo ((,class :inherit (bold modus-themes-slant) :foreground ,red-alt-other)))
;;;;; hydra
    `(hydra-face-amaranth ((,class :inherit bold :foreground ,yellow-alt)))
    `(hydra-face-blue ((,class :inherit bold :foreground ,blue)))
    `(hydra-face-pink ((,class :inherit bold :foreground ,magenta-alt-faint)))
    `(hydra-face-red ((,class :inherit bold :foreground ,red-faint)))
    `(hydra-face-teal ((,class :inherit bold :foreground ,cyan-alt-other)))
;;;;; icomplete
    `(icomplete-first-match ((,class :inherit modus-themes-completion-match-0)))
    `(icomplete-selected-match ((,class :inherit modus-themes-completion-selected)))
;;;;; icomplete-vertical
    `(icomplete-vertical-separator ((,class :inherit shadow)))
;;;;; ido-mode
    `(ido-first-match ((,class :inherit modus-themes-completion-match-0)))
    `(ido-incomplete-regexp ((,class :inherit error)))
    `(ido-indicator ((,class :inherit modus-themes-subtle-yellow)))
    `(ido-only-match ((,class :inherit ido-first-match)))
    `(ido-subdir ((,class :foreground ,blue)))
    `(ido-virtual ((,class :foreground ,magenta-alt-other)))
;;;;; iedit
    `(iedit-occurrence ((,class :inherit modus-themes-refine-blue)))
    `(iedit-read-only-occurrence ((,class :inherit modus-themes-intense-yellow)))
;;;;; iflipb
    `(iflipb-current-buffer-face ((,class :inherit bold :foreground ,cyan-alt)))
    `(iflipb-other-buffer-face ((,class :inherit shadow)))
;;;;; image-dired
    `(image-dired-thumb-flagged ((,class :background ,red-intense-bg)))
    `(image-dired-thumb-header-file-name ((,class :inherit bold)))
    `(image-dired-thumb-header-file-size ((,class :foreground ,blue-active)))
    `(image-dired-thumb-mark ((,class :inherit modus-themes-grue-background-intense)))
;;;;; imenu-list
    `(imenu-list-entry-face-0 ((,class :foreground ,cyan)))
    `(imenu-list-entry-face-1 ((,class :foreground ,blue)))
    `(imenu-list-entry-face-2 ((,class :foreground ,cyan-alt-other)))
    `(imenu-list-entry-face-3 ((,class :foreground ,blue-alt)))
    `(imenu-list-entry-subalist-face-0 ((,class :inherit bold :foreground ,magenta-alt-other :underline t)))
    `(imenu-list-entry-subalist-face-1 ((,class :inherit bold :foreground ,magenta :underline t)))
    `(imenu-list-entry-subalist-face-2 ((,class :inherit bold :foreground ,green-alt-other :underline t)))
    `(imenu-list-entry-subalist-face-3 ((,class :inherit bold :foreground ,red-alt-other :underline t)))
;;;;; indium
    `(indium-breakpoint-face ((,class :foreground ,red-active)))
    `(indium-frame-url-face ((,class :inherit (shadow button))))
    `(indium-keyword-face ((,class :inherit font-lock-keyword-face)))
    `(indium-litable-face ((,class :inherit modus-themes-slant :foreground ,fg-special-warm)))
    `(indium-repl-error-face ((,class :inherit error)))
    `(indium-repl-prompt-face ((,class :inherit modus-themes-prompt)))
    `(indium-repl-stdout-face ((,class :foreground ,fg-main)))
;;;;; info
    `(Info-quoted ((,class :inherit modus-themes-markup-verbatim))) ; the capitalization is canonical
    `(info-header-node ((,class :inherit (shadow bold))))
    `(info-header-xref ((,class :foreground ,blue-active)))
    `(info-index-match ((,class :inherit match)))
    `(info-menu-header ((,class :inherit modus-themes-pseudo-header)))
    `(info-menu-star ((,class :foreground ,red)))
    `(info-node ((,class :inherit bold)))
    `(info-title-1 ((,class :inherit modus-themes-heading-1)))
    `(info-title-2 ((,class :inherit modus-themes-heading-2)))
    `(info-title-3 ((,class :inherit modus-themes-heading-3)))
    `(info-title-4 ((,class :inherit modus-themes-heading-4)))
;;;;; info+ (info-plus)
    `(info-command-ref-item ((,class :inherit font-lock-function-name-face)))
    `(info-constant-ref-item ((,class :inherit font-lock-constant-face)))
    `(info-custom-delimited ((,class :inherit modus-themes-markup-verbatim)))
    `(info-double-quoted-name ((,class :inherit font-lock-string-face)))
    `(info-file (( )))
    `(info-function-ref-item ((,class :inherit font-lock-function-name-face)))
    `(info-glossary-word ((,class :inherit modus-themes-box-button)))
    `(info-indented-text (( )))
    `(info-isolated-backquote (( )))
    `(info-isolated-quote (( )))
    `(info-macro-ref-item ((,class :inherit font-lock-keyword-face)))
    `(info-menu ((,class :inherit bold)))
    `(info-quoted-name ((,class :inherit modus-themes-markup-verbatim)))
    `(info-reference-item ((,class :inherit bold)))
    `(info-special-form-ref-item ((,class :inherit warning)))
    `(info-string ((,class :inherit font-lock-string-face)))
    `(info-syntax-class-item ((,class :inherit modus-themes-markup-code)))
    `(info-user-option-ref-item ((,class :inherit font-lock-variable-name-face)))
    `(info-variable-ref-item ((,class :inherit font-lock-variable-name-face)))
;;;;; info-colors
    `(info-colors-lisp-code-block ((,class :inherit modus-themes-fixed-pitch)))
    `(info-colors-ref-item-command ((,class :inherit font-lock-function-name-face)))
    `(info-colors-ref-item-constant ((,class :inherit font-lock-constant-face)))
    `(info-colors-ref-item-function ((,class :inherit font-lock-function-name-face)))
    `(info-colors-ref-item-macro ((,class :inherit font-lock-keyword-face)))
    `(info-colors-ref-item-other ((,class :inherit font-lock-doc-face)))
    `(info-colors-ref-item-special-form ((,class :inherit font-lock-keyword-face)))
    `(info-colors-ref-item-syntax-class ((,class :inherit font-lock-builtin-face)))
    `(info-colors-ref-item-type ((,class :inherit font-lock-type-face)))
    `(info-colors-ref-item-user-option ((,class :inherit font-lock-variable-name-face)))
    `(info-colors-ref-item-variable ((,class :inherit font-lock-variable-name-face)))
;;;;; interaction-log
    `(ilog-buffer-face ((,class :foreground ,magenta-alt-other)))
    `(ilog-change-face ((,class :foreground ,magenta-alt)))
    `(ilog-echo-face ((,class :foreground ,yellow-alt-other)))
    `(ilog-load-face ((,class :foreground ,green)))
    `(ilog-message-face ((,class :inherit shadow)))
    `(ilog-non-change-face ((,class :foreground ,blue)))
;;;;; ioccur
    `(ioccur-cursor ((,class :foreground ,fg-main)))
    `(ioccur-invalid-regexp ((,class :foreground ,red)))
    `(ioccur-match-face ((,class :inherit modus-themes-special-calm)))
    `(ioccur-match-overlay-face ((,class :inherit modus-themes-special-cold :extend t)))
    `(ioccur-num-line-face ((,class :foreground ,fg-special-warm)))
    `(ioccur-overlay-face ((,class :inherit modus-themes-refine-blue :extend t)))
    `(ioccur-regexp-face ((,class :inherit (modus-themes-intense-magenta bold))))
    `(ioccur-title-face ((,class :inherit modus-themes-pseudo-header :foreground ,fg-special-cold)))
;;;;; isearch, occur, and the like
    `(isearch ((,class :inherit modus-themes-search-success)))
    `(isearch-fail ((,class :inherit modus-themes-refine-red)))
    `(isearch-group-1 ((,class :inherit modus-themes-refine-blue)))
    `(isearch-group-2 ((,class :inherit modus-themes-refine-magenta)))
    `(lazy-highlight ((,class :inherit modus-themes-search-success-lazy)))
    `(match ((,class :inherit modus-themes-special-calm)))
    `(query-replace ((,class :inherit modus-themes-intense-red)))
;;;;; ivy
    `(ivy-action ((,class :inherit modus-themes-key-binding)))
    `(ivy-confirm-face ((,class :inherit success)))
    `(ivy-current-match ((,class :inherit modus-themes-completion-selected)))
    `(ivy-cursor ((,class :background ,fg-main :foreground ,bg-main)))
    `(ivy-highlight-face ((,class :foreground ,magenta)))
    `(ivy-match-required-face ((,class :inherit error)))
    `(ivy-minibuffer-match-face-1 (( )))
    `(ivy-minibuffer-match-face-2 ((,class :inherit modus-themes-completion-match-0)))
    `(ivy-minibuffer-match-face-3 ((,class :inherit modus-themes-completion-match-1)))
    `(ivy-minibuffer-match-face-4 ((,class :inherit modus-themes-completion-match-2)))
    `(ivy-org ((,class :foreground ,cyan-alt-other)))
    `(ivy-remote ((,class :foreground ,magenta)))
    `(ivy-separator ((,class :inherit shadow)))
    `(ivy-subdir ((,class :foreground ,blue)))
    `(ivy-virtual ((,class :foreground ,magenta-alt-other)))
;;;;; ivy-posframe
    `(ivy-posframe-border ((,class :background ,fg-window-divider-inner)))
    `(ivy-posframe-cursor ((,class :background ,fg-main :foreground ,bg-main)))
;;;;; jira (org-jira)
    `(jiralib-comment-face ((,class :background ,bg-alt)))
    `(jiralib-comment-header-face ((,class :inherit bold)))
    `(jiralib-issue-info-face ((,class :inherit modus-themes-special-warm)))
    `(jiralib-issue-info-header-face ((,class :inherit (modus-themes-special-warm bold))))
    `(jiralib-issue-summary-face ((,class :inherit bold)))
    `(jiralib-link-filter-face ((,class :underline t)))
    `(jiralib-link-issue-face ((,class :underline t)))
    `(jiralib-link-project-face ((,class :underline t)))
;;;;; journalctl-mode
    `(journalctl-error-face ((,class :inherit error)))
    `(journalctl-finished-face ((,class :inherit success)))
    `(journalctl-host-face ((,class :foreground ,blue)))
    `(journalctl-process-face ((,class :foreground ,cyan-alt-other)))
    `(journalctl-starting-face ((,class :foreground ,green)))
    `(journalctl-timestamp-face ((,class :foreground ,fg-special-cold)))
    `(journalctl-warning-face ((,class :inherit warning)))
;;;;; js2-mode
    `(js2-error ((,class :inherit modus-themes-lang-error)))
    `(js2-external-variable ((,class :inherit font-lock-variable-name-face)))
    `(js2-function-call ((,class :inherit font-lock-function-name-face)))
    `(js2-function-param ((,class :inherit font-lock-constant-face)))
    `(js2-instance-member ((,class :inherit font-lock-keyword-face)))
    `(js2-jsdoc-html-tag-delimiter ((,class :foreground ,fg-main)))
    `(js2-jsdoc-html-tag-name ((,class :inherit font-lock-function-name-face)))
    `(js2-jsdoc-tag ((,class :inherit (font-lock-builtin-face font-lock-comment-face) :weight normal)))
    `(js2-jsdoc-type ((,class :inherit (font-lock-type-face font-lock-comment-face) :weight normal)))
    `(js2-jsdoc-value ((,class :inherit (font-lock-constant-face font-lock-comment-face) :weight normal)))
    `(js2-object-property ((,class :foreground ,fg-main)))
    `(js2-object-property-access ((,class :foreground ,fg-main)))
    `(js2-private-function-call ((,class :inherit font-lock-preprocessor-face)))
    `(js2-private-member ((,class :inherit font-lock-warning-face)))
    `(js2-warning ((,class :inherit modus-themes-lang-warning)))
;;;;; julia
    `(julia-macro-face ((,class :inherit font-lock-builtin-face)))
    `(julia-quoted-symbol-face ((,class :inherit font-lock-constant-face)))
;;;;; jupyter
    `(jupyter-eval-overlay ((,class :inherit bold :foreground ,blue)))
    `(jupyter-repl-input-prompt ((,class :foreground ,cyan-alt-other)))
    `(jupyter-repl-output-prompt ((,class :foreground ,magenta-alt-other)))
    `(jupyter-repl-traceback ((,class :inherit modus-themes-intense-red)))
;;;;; kaocha-runner
    `(kaocha-runner-error-face ((,class :inherit error)))
    `(kaocha-runner-success-face ((,class :inherit success)))
    `(kaocha-runner-warning-face ((,class :inherit warning)))
;;;;; keycast
    `(keycast-command ((,class :inherit bold :foreground ,blue-active)))
    ;; FIXME 2022-05-03: The padding breaks `keycast-tab-bar-mode'
    `(keycast-key ((,class ;; ,@(modus-themes--mode-line-padded-box blue-active)
                           :background ,blue-active :foreground ,bg-main)))
;;;;; ledger-mode
    `(ledger-font-auto-xact-face ((,class :foreground ,magenta)))
    `(ledger-font-account-name-face ((,class :foreground ,fg-special-cold)))
    `(ledger-font-directive-face ((,class :foreground ,magenta-alt-other)))
    `(ledger-font-posting-date-face ((,class :inherit bold :foreground ,fg-main)))
    `(ledger-font-periodic-xact-face ((,class :foreground ,cyan-alt-other)))
    `(ledger-font-posting-amount-face ((,class :foreground ,fg-special-mild)))
    `(ledger-font-payee-cleared-face ((,class :foreground ,blue-alt)))
    `(ledger-font-payee-pending-face ((,class :foreground ,yellow)))
    `(ledger-font-payee-uncleared-face ((,class :foreground ,red-alt-other)))
    `(ledger-font-xact-highlight-face ((,class :background ,bg-hl-alt)))
;;;;; leerzeichen
    `(leerzeichen ((,class :background ,bg-whitespace :foreground ,fg-whitespace)))
;;;;; line numbers (display-line-numbers-mode and global variant)
    ;; Here we cannot inherit `modus-themes-fixed-pitch'.  We need to
    ;; fall back to `default' otherwise line numbers do not scale when
    ;; using `text-scale-adjust'.
    `(line-number
      ((,class :inherit ,(if modus-themes-mixed-fonts '(fixed-pitch default) 'default)
               ,@(modus-themes--line-numbers
                  fg-alt bg-dim
                  fg-unfocused))))
    `(line-number-current-line
      ((,class :inherit (bold line-number)
               ,@(modus-themes--line-numbers
                  fg-main bg-active
                  blue-alt-other))))
    `(line-number-major-tick
      ((,class :inherit (bold line-number)
               ,@(modus-themes--line-numbers
                  yellow-nuanced-fg yellow-nuanced-bg
                  red-alt))))
    `(line-number-minor-tick
      ((,class :inherit (bold line-number)
               ,@(modus-themes--line-numbers
                  fg-alt bg-inactive
                  fg-inactive))))
;;;;; lsp-mode
    `(lsp-face-highlight-read ((,class :inherit modus-themes-subtle-blue :underline t)))
    `(lsp-face-highlight-textual ((,class :inherit modus-themes-subtle-blue)))
    `(lsp-face-highlight-write ((,class :inherit (modus-themes-refine-blue bold))))
    `(lsp-face-semhl-constant ((,class :foreground ,blue-alt-other)))
    `(lsp-face-semhl-deprecated ((,class :inherit modus-themes-lang-warning)))
    `(lsp-face-semhl-enummember ((,class :foreground ,blue-alt-other)))
    `(lsp-face-semhl-field ((,class :foreground ,cyan-alt)))
    `(lsp-face-semhl-field-static ((,class :inherit modus-themes-slant :foreground ,cyan-alt)))
    `(lsp-face-semhl-function ((,class :foreground ,magenta)))
    `(lsp-face-semhl-method ((,class :foreground ,magenta)))
    `(lsp-face-semhl-namespace ((,class :inherit modus-themes-bold :foreground ,magenta-alt)))
    `(lsp-face-semhl-preprocessor ((,class :foreground ,red-alt-other)))
    `(lsp-face-semhl-static-method ((,class :inherit modus-themes-slant :foreground ,magenta)))
    `(lsp-face-semhl-type-class ((,class :foreground ,magenta-alt)))
    `(lsp-face-semhl-type-enum ((,class :foreground ,magenta-alt)))
    `(lsp-face-semhl-type-primitive ((,class :inherit modus-themes-slant :foreground ,magenta-alt)))
    `(lsp-face-semhl-type-template ((,class :inherit modus-themes-slant :foreground ,magenta-alt)))
    `(lsp-face-semhl-type-typedef ((,class :inherit modus-themes-slant :foreground ,magenta-alt)))
    `(lsp-face-semhl-variable ((,class :foreground ,cyan)))
    `(lsp-face-semhl-variable-local ((,class :foreground ,cyan)))
    `(lsp-face-semhl-variable-parameter ((,class :foreground ,cyan-alt-other)))
    `(lsp-lens-face ((,class  :inherit shadow :height 0.8)))
    `(lsp-lens-mouse-face ((,class :height 0.8 :foreground ,blue-alt-other :underline t)))
    `(lsp-ui-doc-background ((,class :background ,bg-alt)))
    `(lsp-ui-doc-header ((,class :background ,bg-header :foreground ,fg-header)))
    `(lsp-ui-doc-url ((,class :inherit button)))
    `(lsp-ui-peek-filename ((,class :foreground ,fg-special-warm)))
    `(lsp-ui-peek-footer ((,class :background ,bg-header :foreground ,fg-header)))
    `(lsp-ui-peek-header ((,class :background ,bg-header :foreground ,fg-header)))
    `(lsp-ui-peek-highlight ((,class :inherit modus-themes-subtle-blue)))
    `(lsp-ui-peek-line-number ((,class :inherit shadow)))
    `(lsp-ui-peek-list ((,class :background ,bg-dim)))
    `(lsp-ui-peek-peek ((,class :background ,bg-alt)))
    `(lsp-ui-peek-selection ((,class :inherit modus-themes-subtle-cyan)))
    `(lsp-ui-sideline-code-action ((,class :foreground ,yellow)))
    `(lsp-ui-sideline-current-symbol ((,class :inherit bold :height 0.99 :box (:line-width -1 :style nil) :foreground ,fg-main)))
    `(lsp-ui-sideline-symbol ((,class :inherit bold :height 0.99 :box (:line-width -1 :style nil) :foreground ,fg-alt)))
    `(lsp-ui-sideline-symbol-info ((,class :inherit italic :height 0.99)))
;;;;; macrostep
    `(macrostep-compiler-macro-face ((,class :inherit italic)))
    `(macrostep-expansion-highlight-face ((,class :background ,blue-nuanced-bg)))
    `(macrostep-gensym-1 ((,class :inherit bold :foreground ,blue :box t)))
    `(macrostep-gensym-2 ((,class :inherit bold :foreground ,green :box t)))
    `(macrostep-gensym-3 ((,class :inherit bold :foreground ,yellow :box t)))
    `(macrostep-gensym-4 ((,class :inherit bold :foreground ,red :box t)))
    `(macrostep-gensym-5 ((,class :inherit bold :foreground ,magenta :box t)))
    `(macrostep-macro-face ((,class :inherit button :foreground ,green-alt)))
;;;;; magit
    `(magit-bisect-bad ((,class :inherit error)))
    `(magit-bisect-good ((,class :inherit success)))
    `(magit-bisect-skip ((,class :inherit warning)))
    `(magit-blame-date ((,class :foreground ,blue)))
    `(magit-blame-dimmed ((,class :inherit (shadow modus-themes-reset-hard))))
    `(magit-blame-hash ((,class :foreground ,fg-special-warm)))
    `(magit-blame-heading ((,class :inherit modus-themes-reset-hard :background ,bg-alt :extend t)))
    `(magit-blame-highlight ((,class :inherit modus-themes-nuanced-cyan)))
    `(magit-blame-margin ((,class :inherit (magit-blame-highlight modus-themes-reset-hard))))
    `(magit-blame-name ((,class :foreground ,magenta-alt-other)))
    `(magit-blame-summary ((,class :foreground ,cyan-alt-other)))
    ;; ;; NOTE 2021-11-23: we do not set the `magit-branch-current'
    ;; ;; because its definition checks if the :box attribute can be set
    ;; ;; and if not, it uses :inverse-video.  Useful for terminal
    ;; ;; emulators.
    ;;
    ;; `(magit-branch-current ((,class :foreground ,blue-alt-other :box t)))
    `(magit-branch-local ((,class :foreground ,blue-alt)))
    `(magit-branch-remote ((,class :foreground ,magenta-alt)))
    `(magit-branch-remote-head ((,class :foreground ,magenta-alt-other :box t)))
    `(magit-branch-upstream ((,class :inherit italic)))
    `(magit-branch-warning ((,class :inherit warning)))
    `(magit-cherry-equivalent ((,class :background ,bg-main :foreground ,magenta-intense)))
    `(magit-cherry-unmatched ((,class :background ,bg-main :foreground ,cyan-intense)))
    ;; NOTE: here we break from the pattern of inheriting from the
    ;; modus-themes-diff-* faces, though only for the standard actions,
    ;; not the highlighted ones.  This is because Magit's interaction
    ;; model relies on highlighting the current diff hunk.
    `(magit-diff-added ((,class ,@(modus-themes--diff
                                   bg-diff-added fg-diff-added
                                   green-nuanced-bg fg-diff-added
                                   bg-diff-added-deuteran fg-diff-added-deuteran
                                   blue-nuanced-bg fg-diff-added-deuteran))))
    `(magit-diff-added-highlight ((,class :inherit modus-themes-diff-focus-added)))
    `(magit-diff-base ((,class ,@(modus-themes--diff
                                  bg-diff-changed fg-diff-changed
                                  yellow-nuanced-bg fg-diff-changed))))
    `(magit-diff-base-highlight ((,class :inherit modus-themes-diff-focus-changed)))
    `(magit-diff-context ((,class ,@(unless (eq modus-themes-diffs 'bg-only) (list :foreground fg-unfocused)))))
    `(magit-diff-context-highlight ((,class ,@(modus-themes--diff
                                               bg-inactive fg-inactive
                                               bg-dim fg-alt
                                               bg-dim fg-alt))))
    `(magit-diff-file-heading ((,class :inherit bold :foreground ,fg-special-cold)))
    `(magit-diff-file-heading-highlight ((,class :inherit (modus-themes-special-cold bold))))
    `(magit-diff-file-heading-selection ((,class :inherit modus-themes-refine-cyan)))
    ;; NOTE: here we break from the pattern of inheriting from the
    ;; modus-themes-diff-* faces.
    `(magit-diff-hunk-heading ((,class :inherit bold
                                       ,@(modus-themes--diff
                                          bg-active fg-inactive
                                          bg-inactive fg-inactive
                                          bg-inactive fg-inactive
                                          nil nil
                                          t))))
    ;; NOTE: we do not follow the pattern of inheriting from
    ;; modus-themes-grue-* faces, as this is a special case.
    `(magit-diff-hunk-heading-highlight
      ((,class :inherit bold
               :background ,@(modus-themes--deuteran bg-active bg-diff-heading)
               :foreground ,@(modus-themes--deuteran fg-main fg-diff-heading))))
    `(magit-diff-hunk-heading-selection ((,class :inherit modus-themes-refine-blue)))
    `(magit-diff-hunk-region ((,class :inherit bold)))
    `(magit-diff-lines-boundary ((,class :background ,fg-main)))
    `(magit-diff-lines-heading ((,class :inherit modus-themes-refine-magenta)))
    `(magit-diff-removed ((,class ,@(modus-themes--diff
                                     bg-diff-removed fg-diff-removed
                                     red-nuanced-bg fg-diff-removed))))
    `(magit-diff-removed-highlight ((,class :inherit modus-themes-diff-focus-removed)))
    `(magit-diffstat-added ((,class :inherit modus-themes-grue)))
    `(magit-diffstat-removed ((,class :foreground ,red)))
    `(magit-dimmed ((,class :foreground ,fg-unfocused)))
    `(magit-filename ((,class :foreground ,fg-special-cold)))
    `(magit-hash ((,class :inherit shadow)))
    `(magit-head ((,class :inherit magit-branch-local)))
    `(magit-header-line ((,class :inherit bold :foreground ,magenta-active)))
    `(magit-header-line-key ((,class :inherit modus-themes-key-binding)))
    `(magit-header-line-log-select ((,class :inherit bold :foreground ,fg-main)))
    `(magit-keyword ((,class :foreground ,magenta)))
    `(magit-keyword-squash ((,class :inherit bold :foreground ,yellow-alt-other)))
    `(magit-log-author ((,class :foreground ,cyan)))
    `(magit-log-date ((,class :inherit shadow)))
    `(magit-log-graph ((,class :foreground ,fg-dim)))
    `(magit-mode-line-process ((,class :inherit bold :foreground ,cyan-active)))
    `(magit-mode-line-process-error ((,class :inherit bold :foreground ,red-active)))
    `(magit-process-ng ((,class :inherit error)))
    `(magit-process-ok ((,class :inherit success)))
    `(magit-reflog-amend ((,class :inherit warning)))
    `(magit-reflog-checkout ((,class :inherit bold :foreground ,blue-alt)))
    `(magit-reflog-cherry-pick ((,class :inherit success)))
    `(magit-reflog-commit ((,class :inherit bold)))
    `(magit-reflog-merge ((,class :inherit success)))
    `(magit-reflog-other ((,class :inherit bold :foreground ,cyan)))
    `(magit-reflog-rebase ((,class :inherit bold :foreground ,magenta)))
    `(magit-reflog-remote ((,class :inherit bold :foreground ,magenta-alt-other)))
    `(magit-reflog-reset ((,class :inherit error)))
    `(magit-refname ((,class :inherit shadow)))
    `(magit-refname-pullreq ((,class :inherit shadow)))
    `(magit-refname-stash ((,class :inherit shadow)))
    `(magit-refname-wip ((,class :inherit shadow)))
    `(magit-section ((,class :background ,bg-dim :foreground ,fg-main)))
    `(magit-section-heading ((,class :inherit bold :foreground ,cyan)))
    `(magit-section-heading-selection ((,class :inherit (modus-themes-refine-cyan bold))))
    `(magit-section-highlight ((,class :background ,bg-alt)))
    `(magit-sequence-done ((,class :inherit success)))
    `(magit-sequence-drop ((,class :inherit error)))
    `(magit-sequence-exec ((,class :inherit bold :foreground ,magenta-alt)))
    `(magit-sequence-head ((,class :inherit bold :foreground ,cyan-alt)))
    `(magit-sequence-onto ((,class :inherit (bold shadow))))
    `(magit-sequence-part ((,class :inherit warning)))
    `(magit-sequence-pick ((,class :inherit bold)))
    `(magit-sequence-stop ((,class :inherit error)))
    `(magit-signature-bad ((,class :inherit error)))
    `(magit-signature-error ((,class :inherit error)))
    `(magit-signature-expired ((,class :inherit warning)))
    `(magit-signature-expired-key ((,class :foreground ,yellow)))
    `(magit-signature-good ((,class :inherit success)))
    `(magit-signature-revoked ((,class :inherit bold :foreground ,magenta)))
    `(magit-signature-untrusted ((,class :inherit (bold shadow))))
    `(magit-tag ((,class :foreground ,yellow-alt-other)))
;;;;; magit-imerge
    `(magit-imerge-overriding-value ((,class :inherit bold :foreground ,red-alt)))
;;;;; make-mode (makefiles)
    `(makefile-makepp-perl ((,class :background ,cyan-nuanced-bg)))
    `(makefile-space ((,class :background ,magenta-nuanced-bg)))
;;;;; man
    `(Man-overstrike ((,class :inherit bold :foreground ,magenta-alt)))
    `(Man-reverse ((,class :inherit modus-themes-subtle-magenta)))
    `(Man-underline ((,class :foreground ,cyan-alt-other :underline t)))
;;;;; marginalia
    `(marginalia-archive ((,class :foreground ,cyan-alt-other)))
    `(marginalia-char ((,class :foreground ,magenta)))
    `(marginalia-date ((,class :foreground ,cyan)))
    `(marginalia-documentation ((,class :inherit modus-themes-slant :foreground ,fg-docstring)))
    `(marginalia-file-name ((,class :foreground ,blue-faint)))
    `(marginalia-file-owner ((,class :foreground ,red-faint)))
    `(marginalia-file-priv-dir ((,class :foreground ,blue-alt)))
    `(marginalia-file-priv-exec ((,class :foreground ,magenta-alt)))
    `(marginalia-file-priv-link ((,class :foreground ,blue-alt-other)))
    `(marginalia-file-priv-no ((,class :foreground "gray50")))
    `(marginalia-file-priv-other ((,class :foreground ,yellow)))
    `(marginalia-file-priv-rare ((,class :foreground ,red)))
    `(marginalia-file-priv-read ((,class :foreground ,fg-main)))
    `(marginalia-file-priv-write ((,class :foreground ,cyan)))
    `(marginalia-function ((,class :foreground ,magenta-alt-faint)))
    `(marginalia-key ((,class :inherit modus-themes-key-binding)))
    `(marginalia-lighter ((,class :foreground ,blue-alt)))
    `(marginalia-list ((,class :foreground ,magenta-alt-other-faint)))
    `(marginalia-mode ((,class :foreground ,cyan)))
    `(marginalia-modified ((,class :foreground ,magenta-alt-faint)))
    `(marginalia-null ((,class :inherit shadow)))
    `(marginalia-number ((,class :foreground ,cyan)))
    `(marginalia-size ((,class :foreground ,cyan-alt-other-faint)))
    `(marginalia-string ((,class :foreground ,blue-alt)))
    `(marginalia-symbol ((,class :foreground ,blue-alt-other-faint)))
    `(marginalia-true ((,class :foreground ,fg-main)))
    `(marginalia-type ((,class :foreground ,cyan-alt-other)))
    `(marginalia-value ((,class :foreground ,cyan)))
    `(marginalia-version ((,class :foreground ,cyan)))
;;;;; markdown-mode
    `(markdown-blockquote-face ((,class :inherit modus-themes-slant :foreground ,fg-special-cold)))
    `(markdown-bold-face ((,class :inherit bold)))
    `(markdown-code-face ((,class :inherit modus-themes-fixed-pitch :background ,bg-dim :extend t)))
    `(markdown-comment-face ((,class :inherit font-lock-comment-face)))
    `(markdown-footnote-marker-face ((,class :inherit bold :foreground ,cyan-alt)))
    `(markdown-footnote-text-face ((,class :inherit modus-themes-slant :foreground ,fg-main)))
    `(markdown-gfm-checkbox-face ((,class :foreground ,yellow-alt-other)))
    `(markdown-header-delimiter-face ((,class :inherit modus-themes-bold :foreground ,fg-dim)))
    `(markdown-header-face ((t nil)))
    `(markdown-header-face-1 ((,class :inherit modus-themes-heading-1)))
    `(markdown-header-face-2 ((,class :inherit modus-themes-heading-2)))
    `(markdown-header-face-3 ((,class :inherit modus-themes-heading-3)))
    `(markdown-header-face-4 ((,class :inherit modus-themes-heading-4)))
    `(markdown-header-face-5 ((,class :inherit modus-themes-heading-5)))
    `(markdown-header-face-6 ((,class :inherit modus-themes-heading-6)))
    `(markdown-header-rule-face ((,class :inherit bold :foreground ,fg-special-warm)))
    `(markdown-highlighting-face ((,class :inherit modus-themes-refine-yellow)))
    `(markdown-hr-face ((,class :inherit bold :foreground ,fg-special-warm)))
    `(markdown-html-attr-name-face ((,class :inherit modus-themes-fixed-pitch
                                            :foreground ,cyan)))
    `(markdown-html-attr-value-face ((,class :inherit modus-themes-fixed-pitch
                                             :foreground ,blue)))
    `(markdown-html-entity-face ((,class :inherit modus-themes-fixed-pitch
                                         :foreground ,cyan)))
    `(markdown-html-tag-delimiter-face ((,class :inherit modus-themes-fixed-pitch
                                                :foreground ,fg-special-mild)))
    `(markdown-html-tag-name-face ((,class :inherit modus-themes-fixed-pitch
                                           :foreground ,magenta-alt)))
    `(markdown-inline-code-face ((,class :inherit modus-themes-markup-verbatim)))
    `(markdown-italic-face ((,class :inherit italic)))
    `(markdown-language-info-face ((,class :inherit modus-themes-fixed-pitch
                                           :foreground ,fg-special-cold)))
    `(markdown-language-keyword-face ((,class :inherit modus-themes-fixed-pitch
                                              :background ,bg-alt
                                              :foreground ,fg-alt)))
    `(markdown-line-break-face ((,class :inherit modus-themes-refine-cyan :underline t)))
    `(markdown-link-face ((,class :inherit button)))
    `(markdown-link-title-face ((,class :inherit modus-themes-slant :foreground ,fg-special-cold)))
    `(markdown-list-face ((,class :foreground ,fg-dim)))
    `(markdown-markup-face ((,class :inherit shadow)))
    `(markdown-math-face ((,class :foreground ,magenta-alt-other)))
    `(markdown-metadata-key-face ((,class :foreground ,cyan-alt-other)))
    `(markdown-metadata-value-face ((,class :foreground ,blue-alt)))
    `(markdown-missing-link-face ((,class :inherit bold :foreground ,yellow)))
    `(markdown-plain-url-face ((,class :inherit markdown-link-face)))
    `(markdown-pre-face ((,class :inherit markdown-code-face :foreground ,fg-special-mild)))
    `(markdown-reference-face ((,class :inherit markdown-markup-face)))
    `(markdown-strike-through-face ((,class :strike-through t)))
    `(markdown-table-face ((,class :inherit modus-themes-fixed-pitch
                                   :foreground ,fg-special-cold)))
    `(markdown-url-face ((,class :foreground ,blue-alt)))
;;;;; markup-faces (`adoc-mode')
    `(markup-attribute-face ((,class :inherit (italic markup-meta-face))))
    `(markup-bold-face ((,class :inherit bold :foreground ,red-nuanced-fg)))
    `(markup-code-face ((,class :foreground ,magenta)))
    `(markup-comment-face ((,class :inherit font-lock-comment-face)))
    `(markup-complex-replacement-face ((,class :background ,magenta-nuanced-bg :foreground ,magenta-alt-other)))
    `(markup-emphasis-face ((,class :inherit markup-italic-face)))
    `(markup-error-face ((,class :inherit error)))
    `(markup-gen-face ((,class :foreground ,magenta-alt)))
    `(markup-internal-reference-face ((,class :inherit modus-themes-slant :foreground ,fg-alt)))
    `(markup-italic-face ((,class :inherit italic)))
    `(markup-list-face ((,class :inherit modus-themes-special-cold)))
    `(markup-meta-face ((,class :inherit (modus-themes-fixed-pitch shadow))))
    `(markup-meta-hide-face ((,class :foreground "gray50")))
    `(markup-reference-face ((,class :inherit modus-themes-slant :foreground ,blue-alt)))
    `(markup-replacement-face ((,class :inherit modus-themes-fixed-pitch :foreground ,red-alt)))
    `(markup-secondary-text-face ((,class :height 0.9 :foreground ,cyan-alt-other)))
    `(markup-small-face ((,class :inherit markup-gen-face :height 0.9)))
    `(markup-strong-face ((,class :inherit markup-bold-face)))
    `(markup-subscript-face ((,class :height 0.9 :foreground ,magenta-alt-other)))
    `(markup-superscript-face ((,class :height 0.9 :foreground ,magenta-alt-other)))
    `(markup-table-cell-face ((,class :inherit modus-themes-subtle-neutral)))
    `(markup-table-face ((,class :inherit modus-themes-subtle-neutral)))
    `(markup-table-row-face ((,class :inherit modus-themes-special-cold)))
    `(markup-title-0-face ((,class :inherit modus-themes-heading-1)))
    `(markup-title-1-face ((,class :inherit modus-themes-heading-2)))
    `(markup-title-2-face ((,class :inherit modus-themes-heading-3)))
    `(markup-title-3-face ((,class :inherit modus-themes-heading-4)))
    `(markup-title-4-face ((,class :inherit modus-themes-heading-5)))
    `(markup-title-5-face ((,class :inherit modus-themes-heading-6)))
    `(markup-verbatim-face ((,class :inherit modus-themes-fixed-pitch :background ,bg-alt)))
;;;;; mentor
    `(mentor-download-message ((,class :foreground ,fg-special-warm)))
    `(mentor-download-name ((,class :foreground ,fg-special-cold)))
    `(mentor-download-progress ((,class :foreground ,blue-alt-other)))
    `(mentor-download-size ((,class :foreground ,magenta-alt-other)))
    `(mentor-download-speed-down ((,class :foreground ,cyan-alt)))
    `(mentor-download-speed-up ((,class :foreground ,red-alt)))
    `(mentor-download-state ((,class :foreground ,yellow-alt)))
    `(mentor-highlight-face ((,class :inherit modus-themes-subtle-blue)))
    `(mentor-tracker-name ((,class :foreground ,magenta-alt)))
;;;;; messages
    `(message-cited-text-1 ((,class ,@(modus-themes--mail-cite blue-faint blue fg-special-cold))))
    `(message-cited-text-2 ((,class ,@(modus-themes--mail-cite yellow-faint yellow yellow-alt-faint))))
    `(message-cited-text-3 ((,class ,@(modus-themes--mail-cite magenta-alt-faint magenta-alt fg-special-calm))))
    `(message-cited-text-4 ((,class ,@(modus-themes--mail-cite cyan-alt-other-faint cyan-alt-other fg-special-mild))))
    `(message-header-cc ((,class :foreground ,blue-alt-other)))
    `(message-header-name ((,class :inherit bold :foreground ,cyan)))
    `(message-header-newsgroups ((,class :inherit message-header-other)))
    `(message-header-other ((,class :foreground ,fg-special-calm)))
    `(message-header-subject ((,class :inherit bold :foreground ,magenta-alt)))
    `(message-header-to ((,class :inherit bold :foreground ,magenta-alt-other)))
    `(message-header-xheader ((,class :foreground ,blue-alt)))
    `(message-mml ((,class :foreground ,cyan-alt-other)))
    `(message-separator ((,class :inherit modus-themes-intense-neutral)))
;;;;; mini-modeline
    `(mini-modeline-mode-line ((,class :background ,blue-intense :height 0.14)))
    `(mini-modeline-mode-line-inactive ((,class :background ,fg-window-divider-inner :height 0.1)))
;;;;; minimap
    `(minimap-active-region-background ((,class :background ,bg-active)))
    `(minimap-current-line-face ((,class :background ,cyan-intense-bg :foreground ,fg-main)))
;;;;; mmm-mode
    `(mmm-cleanup-submode-face ((,class :background ,yellow-nuanced-bg)))
    `(mmm-code-submode-face ((,class :background ,bg-alt)))
    `(mmm-comment-submode-face ((,class :background ,blue-nuanced-bg)))
    `(mmm-declaration-submode-face ((,class :background ,cyan-nuanced-bg)))
    `(mmm-default-submode-face ((,class :background ,bg-dim)))
    `(mmm-init-submode-face ((,class :background ,magenta-nuanced-bg)))
    `(mmm-output-submode-face ((,class :background ,red-nuanced-bg)))
    `(mmm-special-submode-face ((,class :background ,green-nuanced-bg)))
;;;;; mode-line
    `(mode-line ((,class :inherit modus-themes-ui-variable-pitch
                         ,@(modus-themes--mode-line-attrs
                            fg-active bg-active
                            fg-dim bg-active
                            fg-main bg-active-accent
                            fg-alt bg-active
                            'alt-style bg-main))))
    `(mode-line-active ((,class :inherit mode-line)))
    `(mode-line-buffer-id ((,class :inherit bold)))
    `(mode-line-emphasis ((,class :inherit bold :foreground ,magenta-active)))
    `(mode-line-highlight ((,class ,@(if modus-themes-intense-mouseovers
                                         (list :inherit 'modus-themes-active-blue)
                                       (list :inherit 'highlight)))))
    `(mode-line-inactive ((,class :inherit modus-themes-ui-variable-pitch
                                  ,@(modus-themes--mode-line-attrs
                                     fg-inactive bg-inactive
                                     fg-alt bg-dim
                                     fg-inactive bg-inactive
                                     bg-region bg-active))))
;;;;; mood-line
    `(mood-line-modified ((,class :foreground ,magenta-active)))
    `(mood-line-status-error ((,class :inherit bold :foreground ,red-active)))
    `(mood-line-status-info ((,class :foreground ,cyan-active)))
    `(mood-line-status-neutral ((,class :foreground ,blue-active)))
    `(mood-line-status-success ((,class :inherit modus-themes-grue-active)))
    `(mood-line-status-warning ((,class :inherit bold :foreground ,yellow-active)))
    `(mood-line-unimportant ((,class :foreground ,fg-inactive)))
;;;;; mpdel
    `(mpdel-browser-directory-face ((,class :foreground ,blue)))
    `(mpdel-playlist-current-song-face ((,class :inherit bold :foreground ,blue-alt-other)))
;;;;; mu4e
    `(mu4e-attach-number-face ((,class :inherit bold :foreground ,fg-dim)))
    `(mu4e-cited-1-face ((,class :inherit message-cited-text-1)))
    `(mu4e-cited-2-face ((,class :inherit message-cited-text-2)))
    `(mu4e-cited-3-face ((,class :inherit message-cited-text-3)))
    `(mu4e-cited-4-face ((,class :inherit message-cited-text-4)))
    `(mu4e-cited-5-face ((,class :inherit message-cited-text-1)))
    `(mu4e-cited-6-face ((,class :inherit message-cited-text-2)))
    `(mu4e-cited-7-face ((,class :inherit message-cited-text-3)))
    `(mu4e-compose-header-face ((,class :inherit mu4e-compose-separator-face)))
    `(mu4e-compose-separator-face ((,class :inherit modus-themes-intense-neutral)))
    `(mu4e-contact-face ((,class :inherit message-header-to)))
    `(mu4e-context-face ((,class :foreground ,blue-active)))
    `(mu4e-draft-face ((,class :foreground ,magenta-alt)))
    `(mu4e-flagged-face ((,class :foreground ,red-alt-other)))
    `(mu4e-footer-face ((,class :inherit modus-themes-slant :foreground ,fg-special-cold)))
    `(mu4e-forwarded-face ((,class :foreground ,magenta-alt-other)))
    `(mu4e-header-face ((,class :inherit shadow)))
    `(mu4e-header-highlight-face ((,class :inherit modus-themes-hl-line)))
    `(mu4e-header-key-face ((,class :inherit message-header-name)))
    `(mu4e-header-marks-face ((,class :inherit mu4e-special-header-value-face)))
    `(mu4e-header-title-face ((,class :foreground ,fg-special-mild)))
    `(mu4e-header-value-face ((,class :inherit message-header-other)))
    `(mu4e-highlight-face ((,class :inherit modus-themes-key-binding)))
    `(mu4e-link-face ((,class :inherit button)))
    `(mu4e-modeline-face ((,class :foreground ,magenta-active)))
    `(mu4e-moved-face ((,class :inherit modus-themes-slant :foreground ,yellow)))
    `(mu4e-ok-face ((,class :inherit bold :foreground ,green)))
    `(mu4e-region-code ((,class :inherit modus-themes-special-calm)))
    `(mu4e-related-face ((,class :inherit (italic shadow))))
    `(mu4e-replied-face ((,class :foreground ,blue)))
    `(mu4e-special-header-value-face ((,class :inherit message-header-subject)))
    `(mu4e-system-face ((,class :inherit modus-themes-slant :foreground ,fg-mark-del)))
    `(mu4e-title-face ((,class :foreground ,fg-main)))
    `(mu4e-trashed-face ((,class :foreground ,red)))
    `(mu4e-unread-face ((,class :inherit bold)))
    `(mu4e-url-number-face ((,class :inherit shadow)))
    `(mu4e-view-body-face ((,class :foreground ,fg-main)))
    `(mu4e-warning-face ((,class :inherit warning)))
;;;;; multiple-cursors
    `(mc/cursor-bar-face ((,class :height 1 :background ,fg-main)))
    `(mc/cursor-face ((,class :inverse-video t)))
    `(mc/region-face ((,class :inherit region)))
;;;;; nano-modeline
    `(nano-modeline-active-primary ((,class :inherit mode-line :foreground ,fg-special-mild)))
    `(nano-modeline-active-secondary ((,class :inherit mode-line :foreground ,fg-special-cold)))
    `(nano-modeline-active-status-** ((,class :inherit mode-line :background ,yellow-subtle-bg)))
    `(nano-modeline-active-status-RO ((,class :inherit mode-line :background ,red-subtle-bg)))
    `(nano-modeline-active-status-RW ((,class :inherit mode-line :background ,cyan-subtle-bg)))
    `(nano-modeline-inactive-primary ((,class :inherit mode-line-inactive :foreground ,fg-inactive)))
    `(nano-modeline-inactive-secondary ((,class :inherit mode-line-inactive :foreground ,fg-inactive)))
    `(nano-modeline-inactive-status-** ((,class :inherit mode-line-inactive :foreground ,yellow-active)))
    `(nano-modeline-inactive-status-RO ((,class :inherit mode-line-inactive :foreground ,red-active)))
    `(nano-modeline-inactive-status-RW ((,class :inherit mode-line-inactive :foreground ,cyan-active)))
;;;;; neotree
    `(neo-banner-face ((,class :foreground ,magenta)))
    `(neo-button-face ((,class :inherit button)))
    `(neo-dir-link-face ((,class :inherit bold :foreground ,blue)))
    `(neo-expand-btn-face ((,class :foreground ,cyan)))
    `(neo-file-link-face ((,class :foreground ,fg-main)))
    `(neo-header-face ((,class :inherit bold :foreground ,fg-main)))
    `(neo-root-dir-face ((,class :inherit bold :foreground ,cyan-alt)))
    `(neo-vc-added-face ((,class :inherit modus-themes-grue)))
    `(neo-vc-conflict-face ((,class :inherit error)))
    `(neo-vc-default-face ((,class :foreground ,fg-main)))
    `(neo-vc-edited-face ((,class :foreground ,yellow)))
    `(neo-vc-ignored-face ((,class :foreground ,fg-inactive)))
    `(neo-vc-missing-face ((,class :foreground ,red-alt)))
    `(neo-vc-needs-merge-face ((,class :foreground ,magenta-alt)))
    `(neo-vc-needs-update-face ((,class :underline t)))
    `(neo-vc-removed-face ((,class :strike-through t)))
    `(neo-vc-unlocked-changes-face ((,class :inherit modus-themes-refine-blue)))
    `(neo-vc-up-to-date-face ((,class :inherit shadow)))
    `(neo-vc-user-face ((,class :foreground ,magenta)))
;;;;; notmuch
    `(notmuch-crypto-decryption ((,class :inherit (shadow bold))))
    `(notmuch-crypto-part-header ((,class :foreground ,magenta-alt-other)))
    `(notmuch-crypto-signature-bad ((,class :inherit error)))
    `(notmuch-crypto-signature-good ((,class :inherit success)))
    `(notmuch-crypto-signature-good-key ((,class :inherit bold :foreground ,cyan)))
    `(notmuch-crypto-signature-unknown ((,class :inherit warning)))
    `(notmuch-hello-logo-background ((,class :background "gray50")))
    `(notmuch-jump-key ((,class :inherit modus-themes-key-binding)))
    `(notmuch-message-summary-face ((,class :inherit (bold modus-themes-nuanced-cyan))))
    `(notmuch-search-count ((,class :inherit shadow)))
    `(notmuch-search-date ((,class :foreground ,cyan)))
    `(notmuch-search-flagged-face ((,class :foreground ,red-alt-other)))
    `(notmuch-search-matching-authors ((,class :foreground ,fg-special-cold)))
    `(notmuch-search-non-matching-authors ((,class :inherit shadow)))
    `(notmuch-search-subject ((,class :foreground ,fg-main)))
    `(notmuch-search-unread-face ((,class :inherit bold)))
    `(notmuch-tag-added ((,class :underline ,blue)))
    `(notmuch-tag-deleted ((,class :strike-through ,red)))
    `(notmuch-tag-face ((,class :foreground ,blue)))
    `(notmuch-tag-flagged ((,class :foreground ,red-alt)))
    `(notmuch-tag-unread ((,class :foreground ,magenta-alt)))
    `(notmuch-tree-match-author-face ((,class :inherit notmuch-search-matching-authors)))
    `(notmuch-tree-match-date-face ((,class :inherit notmuch-search-date)))
    `(notmuch-tree-match-face ((,class :foreground ,fg-main)))
    `(notmuch-tree-match-tag-face ((,class :inherit notmuch-tag-face)))
    `(notmuch-tree-no-match-face ((,class :inherit shadow)))
    `(notmuch-tree-no-match-date-face ((,class :inherit shadow)))
    `(notmuch-wash-cited-text ((,class :inherit message-cited-text-1)))
    `(notmuch-wash-toggle-button ((,class :background ,bg-alt :foreground ,fg-alt)))
;;;;; num3-mode
    `(num3-face-even ((,class :inherit bold :background ,bg-alt)))
;;;;; nxml-mode
    `(nxml-attribute-colon ((,class :foreground ,fg-main)))
    `(nxml-attribute-local-name ((,class :inherit font-lock-variable-name-face)))
    `(nxml-attribute-prefix ((,class  :inherit font-lock-type-face)))
    `(nxml-attribute-value ((,class :inherit font-lock-constant-face)))
    `(nxml-cdata-section-CDATA ((,class :inherit error)))
    `(nxml-cdata-section-delimiter ((,class :inherit error)))
    `(nxml-char-ref-delimiter ((,class :foreground ,fg-special-mild)))
    `(nxml-char-ref-number ((,class :inherit modus-themes-bold :foreground ,fg-special-mild)))
    `(nxml-delimited-data ((,class :inherit modus-themes-slant :foreground ,fg-special-cold)))
    `(nxml-delimiter ((,class :foreground ,fg-dim)))
    `(nxml-element-colon ((,class :foreground ,fg-main)))
    `(nxml-element-local-name ((,class :inherit font-lock-function-name-face)))
    `(nxml-element-prefix ((,class :inherit font-lock-builtin-face)))
    `(nxml-entity-ref-delimiter ((,class :foreground ,fg-special-mild)))
    `(nxml-entity-ref-name ((,class :inherit modus-themes-bold :foreground ,fg-special-mild)))
    `(nxml-glyph ((,class :inherit modus-themes-intense-neutral)))
    `(nxml-hash ((,class :inherit (bold font-lock-string-face))))
    `(nxml-heading ((,class :inherit bold)))
    `(nxml-name ((,class :inherit font-lock-builtin-face)))
    `(nxml-namespace-attribute-colon ((,class :foreground ,fg-main)))
    `(nxml-namespace-attribute-prefix ((,class :inherit font-lock-variable-name-face)))
    `(nxml-processing-instruction-target ((,class :inherit font-lock-keyword-face)))
    `(nxml-prolog-keyword ((,class :inherit font-lock-keyword-face)))
    `(nxml-ref ((,class :inherit modus-themes-bold :foreground ,fg-special-mild)))
    `(rng-error ((,class :inherit error)))
;;;;; olivetti
    `(olivetti-fringe ((,class :background ,bg-main)))
;;;;; orderless
    `(orderless-match-face-0 ((,class :inherit modus-themes-completion-match-0)))
    `(orderless-match-face-1 ((,class :inherit modus-themes-completion-match-1)))
    `(orderless-match-face-2 ((,class :inherit modus-themes-completion-match-2)))
    `(orderless-match-face-3 ((,class :inherit modus-themes-completion-match-3)))
;;;;; org
    `(org-agenda-calendar-event ((,class ,@(modus-themes--agenda-event blue-alt))))
    `(org-agenda-calendar-sexp ((,class ,@(modus-themes--agenda-event blue-alt t))))
    `(org-agenda-clocking ((,class :background ,yellow-nuanced-bg :foreground ,red-alt)))
    `(org-agenda-column-dateline ((,class :background ,bg-alt)))
    `(org-agenda-current-time ((,class :foreground ,blue-alt-other-faint)))
    `(org-agenda-date ((,class ,@(modus-themes--agenda-date cyan fg-main))))
    `(org-agenda-date-today
      ((,class ,@(modus-themes--agenda-date cyan fg-main nil nil bg-special-cold t t))))
    `(org-agenda-date-weekend
      ((,class ,@(modus-themes--agenda-date cyan-alt-other-faint fg-alt cyan fg-main))))
    `(org-agenda-date-weekend-today
      ((,class ,@(modus-themes--agenda-date cyan-alt-other-faint fg-alt cyan fg-main bg-special-cold t t))))
    `(org-agenda-diary ((,class :inherit org-agenda-calendar-sexp)))
    `(org-agenda-dimmed-todo-face ((,class :inherit shadow)))
    `(org-agenda-done ((,class :inherit modus-themes-grue-nuanced)))
    `(org-agenda-filter-category ((,class :inherit bold :foreground ,cyan-active)))
    `(org-agenda-filter-effort ((,class :inherit bold :foreground ,cyan-active)))
    `(org-agenda-filter-regexp ((,class :inherit bold :foreground ,cyan-active)))
    `(org-agenda-filter-tags ((,class :inherit bold :foreground ,cyan-active)))
    `(org-agenda-restriction-lock ((,class :background ,bg-dim :foreground ,fg-dim)))
    `(org-agenda-structure ((,class ,@(modus-themes--agenda-structure blue-alt))))
    `(org-agenda-structure-filter ((,class :inherit org-agenda-structure :foreground ,yellow)))
    `(org-agenda-structure-secondary ((,class :foreground ,cyan)))
    `(org-archived ((,class :background ,bg-alt :foreground ,fg-alt)))
    `(org-block ((,class :inherit modus-themes-fixed-pitch
                         ,@(modus-themes--org-block bg-dim fg-main))))
    `(org-block-begin-line ((,class :inherit modus-themes-fixed-pitch
                                    ,@(modus-themes--org-block-delim
                                       bg-dim fg-special-cold
                                       bg-alt fg-alt))))
    `(org-block-end-line ((,class :inherit org-block-begin-line)))
    `(org-checkbox ((,class :foreground ,yellow-alt-other)))
    `(org-checkbox-statistics-done ((,class :inherit org-done)))
    `(org-checkbox-statistics-todo ((,class :inherit org-todo)))
    `(org-clock-overlay ((,class :background ,yellow-nuanced-bg :foreground ,red-alt-faint)))
    `(org-code ((,class :inherit modus-themes-markup-code :extend t)))
    `(org-column ((,class :inherit (modus-themes-fixed-pitch default)
                          :background ,bg-alt)))
    `(org-column-title ((,class :inherit (bold modus-themes-fixed-pitch default)
                                :underline t :background ,bg-alt)))
    `(org-date ((,class :inherit (modus-themes-link-symlink modus-themes-fixed-pitch))))
    `(org-date-selected ((,class :foreground ,blue-alt :inverse-video t)))
    `(org-dispatcher-highlight ((,class :inherit (bold modus-themes-mark-alt))))
    `(org-document-info ((,class :foreground ,fg-special-cold)))
    `(org-document-info-keyword ((,class :inherit (shadow modus-themes-fixed-pitch))))
    `(org-document-title ((,class :inherit modus-themes-heading-0)))
    `(org-done ((,class :inherit modus-themes-grue)))
    `(org-drawer ((,class :inherit (shadow modus-themes-fixed-pitch))))
    `(org-ellipsis (())) ; inherits from the heading's color
    `(org-footnote ((,class :inherit button
                            ,@(modus-themes--link-color
                               blue-alt blue-alt-faint))))
    `(org-formula ((,class :inherit modus-themes-fixed-pitch :foreground ,red-alt)))
    `(org-habit-alert-face ((,class ,@(modus-themes--agenda-habit
                                       yellow-graph-0-bg
                                       yellow-graph-0-bg
                                       yellow-graph-1-bg)
                                    :foreground "black"))) ; special case
    `(org-habit-alert-future-face ((,class ,@(modus-themes--agenda-habit
                                              yellow-graph-1-bg
                                              yellow-graph-0-bg
                                              yellow-graph-1-bg))))
    `(org-habit-clear-face ((,class ,@(modus-themes--agenda-habit
                                       blue-graph-0-bg
                                       green-graph-1-bg
                                       blue-graph-1-bg
                                       blue-graph-1-bg
                                       blue-graph-1-bg)
                                    :foreground "black"))) ; special case
    `(org-habit-clear-future-face ((,class ,@(modus-themes--agenda-habit
                                              blue-graph-1-bg
                                              green-graph-1-bg
                                              blue-graph-1-bg
                                              blue-graph-1-bg
                                              blue-graph-1-bg))))
    `(org-habit-overdue-face ((,class ,@(modus-themes--agenda-habit
                                         red-graph-0-bg
                                         red-graph-0-bg
                                         red-graph-1-bg))))
    `(org-habit-overdue-future-face ((,class ,@(modus-themes--agenda-habit
                                                red-graph-1-bg
                                                red-graph-0-bg
                                                red-graph-1-bg))))
    `(org-habit-ready-face ((,class ,@(modus-themes--agenda-habit
                                       green-graph-0-bg
                                       green-graph-0-bg
                                       green-graph-1-bg
                                       cyan-graph-0-bg
                                       blue-graph-0-bg
                                       cyan-graph-1-bg)
                                    :foreground "black"))) ; special case
    `(org-habit-ready-future-face ((,class ,@(modus-themes--agenda-habit
                                              green-graph-1-bg
                                              green-graph-0-bg
                                              green-graph-1-bg
                                              cyan-graph-1-bg
                                              blue-graph-0-bg
                                              cyan-graph-1-bg))))
    `(org-headline-done ((,class :inherit (modus-themes-variable-pitch modus-themes-grue-nuanced))))
    `(org-headline-todo ((,class :inherit modus-themes-variable-pitch :foreground ,red-nuanced-fg)))
    `(org-hide ((,class :foreground ,bg-main)))
    `(org-indent ((,class :inherit (fixed-pitch org-hide))))
    `(org-imminent-deadline ((,class :foreground ,red-intense)))
    `(org-latex-and-related ((,class :foreground ,magenta-faint)))
    `(org-level-1 ((,class :inherit modus-themes-heading-1)))
    `(org-level-2 ((,class :inherit modus-themes-heading-2)))
    `(org-level-3 ((,class :inherit modus-themes-heading-3)))
    `(org-level-4 ((,class :inherit modus-themes-heading-4)))
    `(org-level-5 ((,class :inherit modus-themes-heading-5)))
    `(org-level-6 ((,class :inherit modus-themes-heading-6)))
    `(org-level-7 ((,class :inherit modus-themes-heading-7)))
    `(org-level-8 ((,class :inherit modus-themes-heading-8)))
    `(org-link ((,class :inherit button)))
    `(org-list-dt ((,class :inherit bold)))
    `(org-macro ((,class :inherit modus-themes-markup-macro)))
    `(org-meta-line ((,class :inherit (shadow modus-themes-fixed-pitch))))
    `(org-mode-line-clock ((,class :foreground ,fg-main)))
    `(org-mode-line-clock-overrun ((,class :inherit bold :foreground ,red-active)))
    `(org-priority ((,class :foreground ,magenta)))
    `(org-property-value ((,class :inherit modus-themes-fixed-pitch :foreground ,fg-special-cold)))
    `(org-quote ((,class ,@(modus-themes--org-block bg-dim fg-special-cold fg-main))))
    `(org-scheduled ((,class ,@(modus-themes--agenda-scheduled yellow-faint fg-special-warm magenta-alt))))
    `(org-scheduled-previously ((,class ,@(modus-themes--agenda-scheduled yellow fg-special-warm yellow-alt-other))))
    `(org-scheduled-today ((,class ,@(modus-themes--agenda-scheduled yellow fg-special-warm magenta-alt-other))))
    `(org-sexp-date ((,class :foreground ,cyan-alt-other)))
    `(org-special-keyword ((,class :inherit (shadow modus-themes-fixed-pitch))))
    `(org-table ((,class :inherit modus-themes-fixed-pitch :foreground ,fg-special-cold)))
    `(org-table-header ((,class :inherit (fixed-pitch modus-themes-special-cold))))
    `(org-tag ((,class :foreground ,magenta-nuanced-fg)))
    `(org-tag-group ((,class :inherit bold :foreground ,cyan-nuanced-fg)))
    `(org-target ((,class :underline t)))
    `(org-time-grid ((,class :inherit shadow)))
    `(org-todo ((,class :foreground ,red)))
    `(org-upcoming-deadline ((,class :foreground ,red-alt-other)))
    `(org-upcoming-distant-deadline ((,class :foreground ,red-faint)))
    `(org-verbatim ((,class :inherit modus-themes-markup-verbatim)))
    `(org-verse ((,class :inherit org-quote)))
    `(org-warning ((,class :inherit bold :foreground ,red-alt-other)))
;;;;; org-journal
    `(org-journal-calendar-entry-face ((,class :inherit modus-themes-slant :foreground ,yellow-alt-other)))
    `(org-journal-calendar-scheduled-face ((,class :inherit modus-themes-slant :foreground ,red-alt-other)))
    `(org-journal-highlight ((,class :foreground ,magenta-alt)))
;;;;; org-noter
    `(org-noter-no-notes-exist-face ((,class :inherit error)))
    `(org-noter-notes-exist-face ((,class :inherit success)))
;;;;; org-pomodoro
    `(org-pomodoro-mode-line ((,class :foreground ,red-active)))
    `(org-pomodoro-mode-line-break ((,class :foreground ,cyan-active)))
    `(org-pomodoro-mode-line-overtime ((,class :inherit bold :foreground ,red-active)))
;;;;; org-recur
    `(org-recur ((,class :foreground ,magenta-active)))
;;;;; org-roam
    `(org-roam-dim ((,class :foreground "gray50")))
    `(org-roam-header-line ((,class :inherit bold :foreground ,magenta-active)))
    `(org-roam-olp ((,class :inherit shadow)))
    `(org-roam-preview-heading ((,class :inherit modus-themes-subtle-neutral)))
    `(org-roam-preview-heading-highlight ((,class :inherit modus-themes-intense-neutral)))
    `(org-roam-preview-heading-selection ((,class :inherit modus-themes-special-cold)))
    `(org-roam-preview-region ((,class :inherit bold)))
    `(org-roam-title ((,class :inherit modus-themes-pseudo-header)))
;;;;; org-superstar
    `(org-superstar-item ((,class :foreground ,fg-main)))
    `(org-superstar-leading ((,class :foreground ,fg-whitespace)))
;;;;; org-table-sticky-header
    `(org-table-sticky-header-face ((,class :inherit modus-themes-special-cold)))
;;;;; org-tree-slide
    `(org-tree-slide-header-overlay-face ((,class :inherit org-document-title)))
;;;;; origami
    `(origami-fold-header-face ((,class :background ,bg-dim :foreground ,fg-dim :box t)))
    `(origami-fold-replacement-face ((,class :background ,bg-alt :foreground ,fg-alt)))
;;;;; outline-mode
    `(outline-1 ((,class :inherit modus-themes-heading-1)))
    `(outline-2 ((,class :inherit modus-themes-heading-2)))
    `(outline-3 ((,class :inherit modus-themes-heading-3)))
    `(outline-4 ((,class :inherit modus-themes-heading-4)))
    `(outline-5 ((,class :inherit modus-themes-heading-5)))
    `(outline-6 ((,class :inherit modus-themes-heading-6)))
    `(outline-7 ((,class :inherit modus-themes-heading-7)))
    `(outline-8 ((,class :inherit modus-themes-heading-8)))
;;;;; outline-minor-faces
    `(outline-minor-0 (()))
;;;;; package (M-x list-packages)
    `(package-description ((,class :foreground ,fg-special-cold)))
    `(package-help-section-name ((,class :inherit bold :foreground ,cyan)))
    `(package-name ((,class :inherit button)))
    `(package-status-available ((,class :foreground ,cyan-alt-other)))
    `(package-status-avail-obso ((,class :inherit error)))
    `(package-status-built-in ((,class :foreground ,magenta)))
    `(package-status-dependency ((,class :foreground ,magenta-alt-other)))
    `(package-status-disabled ((,class :inherit modus-themes-subtle-red)))
    `(package-status-external ((,class :foreground ,cyan-alt-other)))
    `(package-status-held ((,class :foreground ,yellow-alt)))
    `(package-status-incompat ((,class :inherit warning)))
    `(package-status-installed ((,class :foreground ,fg-special-warm)))
    `(package-status-new ((,class :inherit success)))
    `(package-status-unsigned ((,class :inherit error)))
;;;;; page-break-lines
    `(page-break-lines ((,class :inherit default :foreground ,fg-window-divider-outer)))
;;;;; pandoc-mode
    `(pandoc-citation-key-face ((,class :background ,bg-dim :foreground ,magenta-alt)))
    `(pandoc-directive-@@-face ((,class :background ,bg-dim :foreground ,blue-alt-other)))
    `(pandoc-directive-braces-face ((,class :foreground ,blue-alt-other)))
    `(pandoc-directive-contents-face ((,class :foreground ,cyan-alt-other)))
    `(pandoc-directive-type-face ((,class :foreground ,magenta)))
;;;;; paren-face
    `(parenthesis ((,class :foreground ,fg-unfocused)))
;;;;; pass
    `(pass-mode-directory-face ((,class :inherit bold :foreground ,fg-special-cold)))
    `(pass-mode-entry-face ((,class :background ,bg-main :foreground ,fg-main)))
    `(pass-mode-header-face ((,class :foreground ,fg-special-warm)))
;;;;; pdf-tools
    `(pdf-links-read-link ((,class :background ,fg-main :foreground ,magenta-intense-bg :inherit bold))) ; Foreground is background and vice versa
    `(pdf-occur-document-face ((,class :inherit shadow)))
    `(pdf-occur-page-face ((,class :inherit shadow)))
;;;;; persp-mode
    `(persp-face-lighter-buffer-not-in-persp ((,class :inherit modus-themes-intense-red)))
    `(persp-face-lighter-default ((,class :inherit bold :foreground ,blue-active)))
    `(persp-face-lighter-nil-persp ((,class :inherit bold :foreground ,fg-active)))
;;;;; perspective
    `(persp-selected-face ((,class :inherit bold :foreground ,blue-active)))
;;;;; phi-grep
    `(phi-grep-heading-face ((,class :inherit modus-themes-pseudo-header :foreground ,fg-special-cold)))
    `(phi-grep-line-number-face ((,class :foreground ,fg-special-warm)))
    `(phi-grep-match-face ((,class :inherit modus-themes-special-calm)))
    `(phi-grep-modified-face ((,class :inherit modus-themes-refine-yellow)))
    `(phi-grep-overlay-face ((,class :inherit modus-themes-refine-blue)))
;;;;; pomidor
    `(pomidor-break-face ((,class :foreground ,blue-alt-other)))
    `(pomidor-overwork-face ((,class :foreground ,red-alt-other)))
    `(pomidor-skip-face ((,class :inherit (shadow modus-themes-slant))))
    `(pomidor-work-face ((,class :inherit modus-themes-grue)))
;;;;; popup
    `(popup-face ((,class :background ,bg-alt :foreground ,fg-main)))
    `(popup-isearch-match ((,class :inherit modus-themes-search-success)))
    `(popup-menu-mouse-face ((,class :inherit highlight)))
    `(popup-menu-selection-face ((,class :inherit modus-themes-completion-selected-popup)))
    `(popup-scroll-bar-background-face ((,class :background ,bg-active)))
    `(popup-scroll-bar-foreground-face ((,class :foreground ,fg-active)))
    `(popup-summary-face ((,class :background ,bg-active :foreground ,fg-inactive)))
    `(popup-tip-face ((,class :inherit modus-themes-refine-yellow)))
;;;;; powerline
    `(powerline-active0 ((,class :background ,fg-unfocused :foreground ,bg-main)))
    `(powerline-active1 ((,class :inherit mode-line-active)))
    `(powerline-active2 ((,class :inherit mode-line-inactive)))
    `(powerline-inactive0 ((,class :background ,bg-active :foreground ,fg-alt)))
    `(powerline-inactive1 ((,class :background ,bg-main :foreground ,fg-alt)))
    `(powerline-inactive2 ((,class :inherit mode-line-inactive)))
;;;;; powerline-evil
    `(powerline-evil-base-face ((,class :background ,fg-main :foreground ,bg-main)))
    `(powerline-evil-emacs-face ((,class :inherit modus-themes-active-magenta)))
    `(powerline-evil-insert-face ((,class :inherit modus-themes-active-green)))
    `(powerline-evil-motion-face ((,class :inherit modus-themes-active-blue)))
    `(powerline-evil-normal-face ((,class :background ,fg-alt :foreground ,bg-main)))
    `(powerline-evil-operator-face ((,class :inherit modus-themes-active-yellow)))
    `(powerline-evil-replace-face ((,class :inherit modus-themes-active-red)))
    `(powerline-evil-visual-face ((,class :inherit modus-themes-active-cyan)))
;;;;; prescient
    `(prescient-primary-highlight ((,class :inherit modus-themes-completion-match-0)))
    `(prescient-secondary-highlight ((,class :inherit modus-themes-completion-match-1)))
;;;;; proced
    `(proced-mark ((,class :inherit modus-themes-mark-symbol)))
    `(proced-marked ((,class :inherit modus-themes-mark-alt)))
    `(proced-sort-header ((,class :inherit bold :foreground ,fg-special-calm :underline t)))
;;;;; prodigy
    `(prodigy-green-face ((,class :inherit success)))
    `(prodigy-red-face ((,class :inherit error)))
    `(prodigy-yellow-face ((,class :inherit warning)))
;;;;; pulse
    `(pulse-highlight-start-face ((,class :background ,bg-active-accent :extend t)))
;;;;; pyim
    `(pyim-page ((,class :background ,bg-active :foreground ,fg-active)))
    `(pyim-page-selection ((,class :inherit bold :background ,bg-active :foreground ,blue-active)))
    `(pyim-page-subword ((,class :background ,bg-inactive)))
;;;;; quick-peek
    `(quick-peek-background-face ((,class :background ,bg-alt)))
    `(quick-peek-border-face ((,class :background ,fg-window-divider-inner :height 1)))
    `(quick-peek-padding-face ((,class :background ,bg-alt :height 0.15)))
;;;;; racket-mode
    `(racket-debug-break-face ((,class :inherit modus-themes-intense-red)))
    `(racket-debug-locals-face ((,class :box (:line-width -1 :color nil)
                                        :foreground ,green-alt-other)))
    `(racket-debug-result-face ((,class :inherit bold :box (:line-width -1 :color nil)
                                        :foreground ,green)))
    `(racket-here-string-face ((,class :foreground ,blue-alt)))
    `(racket-keyword-argument-face ((,class :foreground ,red-alt)))
    `(racket-logger-config-face ((,class :inherit (shadow modus-themes-slant))))
    `(racket-logger-debug-face ((,class :foreground ,blue-alt-other)))
    `(racket-logger-info-face ((,class :foreground ,fg-lang-note)))
    `(racket-logger-topic-face ((,class :inherit modus-themes-slant :foreground ,magenta)))
    `(racket-selfeval-face ((,class :foreground ,green-alt)))
    `(racket-xp-error-face ((,class :inherit modus-themes-lang-error)))
;;;;; rainbow-blocks
    `(rainbow-blocks-depth-1-face ((,class :foreground ,magenta-alt-other)))
    `(rainbow-blocks-depth-2-face ((,class :foreground ,blue)))
    `(rainbow-blocks-depth-3-face ((,class :foreground ,magenta-alt)))
    `(rainbow-blocks-depth-4-face ((,class :foreground ,green)))
    `(rainbow-blocks-depth-5-face ((,class :foreground ,magenta)))
    `(rainbow-blocks-depth-6-face ((,class :foreground ,cyan)))
    `(rainbow-blocks-depth-7-face ((,class :foreground ,yellow)))
    `(rainbow-blocks-depth-8-face ((,class :foreground ,cyan-alt)))
    `(rainbow-blocks-depth-9-face ((,class :foreground ,red-alt)))
    `(rainbow-blocks-unmatched-face ((,class :foreground ,red)))
;;;;; rainbow-delimiters
    `(rainbow-delimiters-base-error-face ((,class :background ,red-subtle-bg :foreground ,fg-main)))
    `(rainbow-delimiters-base-face ((,class :foreground ,fg-main)))
    `(rainbow-delimiters-depth-1-face ((,class :foreground ,fg-main)))
    `(rainbow-delimiters-depth-2-face ((,class :foreground ,magenta-intense)))
    `(rainbow-delimiters-depth-3-face ((,class :foreground ,cyan-intense)))
    `(rainbow-delimiters-depth-4-face ((,class :foreground ,orange-intense)))
    `(rainbow-delimiters-depth-5-face ((,class :foreground ,purple-intense)))
    `(rainbow-delimiters-depth-6-face ((,class :foreground ,green-intense)))
    `(rainbow-delimiters-depth-7-face ((,class :foreground ,red-intense)))
    `(rainbow-delimiters-depth-8-face ((,class :foreground ,blue-intense)))
    `(rainbow-delimiters-depth-9-face ((,class :foreground ,yellow-intense)))
    `(rainbow-delimiters-mismatched-face ((,class :inherit (bold modus-themes-refine-yellow))))
    `(rainbow-delimiters-unmatched-face ((,class :inherit (bold modus-themes-refine-red))))
;;;;; rcirc
    `(rcirc-bright-nick ((,class :inherit bold :foreground ,magenta-intense)))
    `(rcirc-dim-nick ((,class :inherit shadow)))
    `(rcirc-monospace-text ((,class :inherit fixed-pitch)))
    `(rcirc-my-nick ((,class :inherit bold :foreground ,magenta)))
    `(rcirc-nick-in-message ((,class :inherit bold :foreground ,red-alt)))
    `(rcirc-nick-in-message-full-line ((,class :inherit bold :foreground ,cyan-alt-other)))
    `(rcirc-other-nick ((,class :inherit bold :foreground ,blue)))
    `(rcirc-prompt ((,class :inherit modus-themes-prompt)))
    `(rcirc-server ((,class :inherit shadow)))
    `(rcirc-timestamp ((,class :foreground ,cyan)))
    `(rcirc-track-keyword ((,class :inherit bold)))
    `(rcirc-track-nick ((,class :inherit bold :foreground ,red-active)))
    `(rcirc-url ((,class :inherit link)))
;;;;; recursion-indicator
    `(recursion-indicator-general ((,class :foreground ,blue-active)))
    `(recursion-indicator-minibuffer ((,class :foreground ,red-active)))
;;;;; regexp-builder (re-builder)
    `(reb-match-0 ((,class :inherit modus-themes-refine-cyan)))
    `(reb-match-1 ((,class :inherit modus-themes-subtle-magenta)))
    `(reb-match-2 ((,class :inherit modus-themes-subtle-green)))
    `(reb-match-3 ((,class :inherit modus-themes-refine-yellow)))
    `(reb-regexp-grouping-backslash ((,class :inherit font-lock-regexp-grouping-backslash)))
    `(reb-regexp-grouping-construct ((,class :inherit font-lock-regexp-grouping-construct)))
;;;;; rg (rg.el)
    `(rg-column-number-face ((,class :foreground ,magenta-alt-other)))
    `(rg-context-face ((,class :foreground ,fg-unfocused)))
    `(rg-error-face ((,class :inherit bold :foreground ,red)))
    `(rg-file-tag-face ((,class :foreground ,fg-special-cold)))
    `(rg-filename-face ((,class :inherit bold :foreground ,fg-special-cold)))
    `(rg-line-number-face ((,class :foreground ,fg-special-warm)))
    `(rg-literal-face ((,class :foreground ,blue-alt)))
    `(rg-match-face ((,class :inherit modus-themes-special-calm)))
    `(rg-regexp-face ((,class :foreground ,magenta-active)))
    `(rg-toggle-off-face ((,class :inherit bold :foreground ,fg-inactive)))
    `(rg-toggle-on-face ((,class :inherit bold :foreground ,cyan-active)))
    `(rg-warning-face ((,class :inherit bold :foreground ,yellow)))
;;;;; ripgrep
    `(ripgrep-context-face ((,class :foreground ,fg-unfocused)))
    `(ripgrep-error-face ((,class :inherit bold :foreground ,red)))
    `(ripgrep-hit-face ((,class :foreground ,cyan)))
    `(ripgrep-match-face ((,class :inherit modus-themes-special-calm)))
;;;;; rmail
    `(rmail-header-name ((,class :foreground ,cyan-alt-other)))
    `(rmail-highlight ((,class :inherit bold :foreground ,magenta-alt)))
;;;;; ruler-mode
    `(ruler-mode-column-number ((,class :inherit ruler-mode-default :foreground ,fg-main)))
    `(ruler-mode-comment-column ((,class :inherit ruler-mode-default :foreground ,red)))
    `(ruler-mode-current-column ((,class :inherit ruler-mode-default :background ,blue-subtle-bg :foreground ,fg-main)))
    `(ruler-mode-default ((,class :inherit default :background ,bg-alt :foreground ,fg-unfocused)))
    `(ruler-mode-fill-column ((,class :inherit ruler-mode-default :foreground ,green)))
    `(ruler-mode-fringes ((,class :inherit ruler-mode-default :foreground ,cyan)))
    `(ruler-mode-goal-column ((,class :inherit ruler-mode-default :foreground ,blue)))
    `(ruler-mode-margins ((,class :inherit ruler-mode-default :foreground ,bg-main)))
    `(ruler-mode-pad ((,class :inherit ruler-mode-default :background ,bg-active :foreground ,fg-inactive)))
    `(ruler-mode-tab-stop ((,class :inherit ruler-mode-default :foreground ,fg-special-warm)))
;;;;; selectrum
    `(selectrum-current-candidate ((,class :inherit modus-themes-completion-selected)))
    `(selectrum-mouse-highlight ((,class :inherit highlight)))
    `(selectrum-quick-keys-highlight ((,class :inherit bold :background ,bg-char-0)))
    `(selectrum-quick-keys-match ((,class :inherit bold :background ,bg-char-1)))
;;;;; semantic
    `(semantic-complete-inline-face ((,class :foreground ,fg-special-warm :underline t)))
    `(semantic-decoration-on-fileless-includes ((,class :inherit modus-themes-refine-green)))
    `(semantic-decoration-on-private-members-face ((,class :inherit modus-themes-refine-cyan)))
    `(semantic-decoration-on-protected-members-face ((,class :background ,bg-dim)))
    `(semantic-decoration-on-unknown-includes ((,class :inherit modus-themes-refine-red)))
    `(semantic-decoration-on-unparsed-includes ((,class :inherit modus-themes-refine-yellow)))
    `(semantic-highlight-edits-face ((,class :background ,bg-alt)))
    `(semantic-highlight-func-current-tag-face ((,class :background ,bg-alt)))
    `(semantic-idle-symbol-highlight ((,class :inherit modus-themes-special-mild)))
    `(semantic-tag-boundary-face ((,class :overline ,blue-intense)))
    `(semantic-unmatched-syntax-face ((,class :underline ,fg-lang-error)))
;;;;; sesman
    `(sesman-browser-button-face ((,class :inherit button)))
    `(sesman-browser-highligh-face ((,class :inherit highlight)))
    `(sesman-buffer-face ((,class :foreground ,magenta)))
    `(sesman-directory-face ((,class :inherit bold :foreground ,blue)))
    `(sesman-project-face ((,class :inherit bold :foreground ,magenta-alt-other)))
;;;;; shell-script-mode
    `(sh-heredoc ((,class :foreground ,blue-alt)))
    `(sh-quoted-exec ((,class :inherit modus-themes-bold :foreground ,magenta-alt)))
;;;;; shortdoc
    `(shortdoc-heading ((,class :inherit modus-themes-pseudo-header)))
    `(shortdoc-section (())) ; remove the default's variable-pitch style
;;;;; show-paren-mode
    `(show-paren-match ((,class ,@(modus-themes--paren bg-paren-match
                                                       bg-paren-match-intense)
                                :foreground ,fg-main)))
    `(show-paren-match-expression ((,class :background ,bg-paren-expression)))
    `(show-paren-mismatch ((,class :inherit modus-themes-intense-red)))
;;;;; shr
    `(shr-abbreviation ((,class :inherit modus-themes-lang-note)))
    `(shr-code ((,class :inherit modus-themes-markup-verbatim)))
    `(shr-h1 ((,class :inherit modus-themes-heading-1)))
    `(shr-h2 ((,class :inherit modus-themes-heading-2)))
    `(shr-h3 ((,class :inherit modus-themes-heading-3)))
    `(shr-h4 ((,class :inherit modus-themes-heading-4)))
    `(shr-h5 ((,class :inherit modus-themes-heading-5)))
    `(shr-h6 ((,class :inherit modus-themes-heading-6)))
    `(shr-selected-link ((,class :inherit modus-themes-subtle-red)))
;;;;; side-notes
    `(side-notes ((,class :background ,bg-dim :foreground ,fg-dim)))
;;;;; sieve-mode
    `(sieve-action-commands ((,class :inherit font-lock-builtin-face)))
    `(sieve-control-commands ((,class :inherit font-lock-keyword-face)))
    `(sieve-tagged-arguments ((,class :inherit font-lock-type-face)))
    `(sieve-test-commands ((,class :inherit font-lock-function-name-face)))
;;;;; skewer-mode
    `(skewer-error-face ((,class :foreground ,red :underline t)))
;;;;; slime (sldb)
    `(sldb-condition-face ((,class :inherit font-lock-preprocessor-face)))
    `(sldb-restart-number-face ((,class :inherit bold)))
    `(sldb-restart-type-face ((,class :inherit font-lock-type-face)))
    `(sldb-restartable-frame-line-face ((,class :inherit success)))
    `(sldb-section-face ((,class :inherit modus-themes-pseudo-header)))
    `(slime-error-face ((,class :inherit modus-themes-lang-error)))
    `(slime-note-face ((,class :underline t)))
    `(slime-repl-input-face ((,class :inherit bold)))
    `(slime-repl-inputed-output-face ((,class :inherit font-lock-string-face)))
    `(slime-repl-output-mouseover-face ((,class :inherit highlight)))
    `(slime-repl-prompt-face ((,class :inherit modus-themes-prompt)))
    `(slime-style-warning-face ((,class :inherit modus-themes-lang-note)))
    `(slime-warning-face ((,class :inherit modus-themes-lang-warning)))
;;;;; sly
    `(sly-action-face ((,class :inherit font-lock-type-face)))
    `(sly-db-condition-face ((,class :inherit font-lock-preprocessor-face)))
    `(sly-db-restartable-frame-line-face ((,class :inherit success)))
    `(sly-error-face ((,class :inherit modus-themes-lang-error)))
    `(sly-mode-line ((,class :inherit mode-line-emphasis)))
    `(sly-mrepl-output-face ((,class :inherit font-lock-string-face)))
    `(sly-mrepl-output-face ((,class :inherit font-lock-string-face)))
    `(sly-mrepl-prompt-face ((,class :inherit modus-themes-prompt)))
    `(sly-note-face ((,class :inherit modus-themes-lang-note)))
    `(sly-stickers-placed-face ((,class :inherit modus-themes-subtle-neutral)))
    `(sly-style-warning-face ((,class :inherit modus-themes-lang-note)))
    `(sly-warning-face ((,class :inherit modus-themes-lang-warning)))
;;;;; smart-mode-line
    `(sml/charging ((,class :foreground ,green-active)))
    `(sml/discharging ((,class :foreground ,red-active)))
    `(sml/filename ((,class :inherit bold :foreground ,blue-active)))
    `(sml/folder ((,class :foreground ,fg-active)))
    `(sml/git ((,class :inherit bold :foreground ,green-active)))
    `(sml/global ((,class :foreground ,fg-active)))
    `(sml/line-number ((,class :inherit sml/global)))
    `(sml/minor-modes ((,class :inherit sml/global)))
    `(sml/modes ((,class :inherit bold :foreground ,fg-active)))
    `(sml/modified ((,class :inherit bold :foreground ,magenta-active)))
    `(sml/mule-info ((,class :inherit sml/global)))
    `(sml/name-filling ((,class :foreground ,yellow-active)))
    `(sml/not-modified ((,class :inherit sml/global)))
    `(sml/numbers-separator ((,class :inherit sml/global)))
    `(sml/outside-modified ((,class :inherit modus-themes-intense-red)))
    `(sml/position-percentage ((,class :inherit sml/global)))
    `(sml/prefix ((,class :foreground ,green-active)))
    `(sml/process ((,class :inherit sml/prefix)))
    `(sml/projectile ((,class :inherit sml/git)))
    `(sml/read-only ((,class :inherit bold :foreground ,cyan-active)))
    `(sml/remote ((,class :inherit sml/global)))
    `(sml/sudo ((,class :inherit modus-themes-subtle-red)))
    `(sml/time ((,class :inherit sml/global)))
    `(sml/vc ((,class :inherit sml/git)))
    `(sml/vc-edited ((,class :inherit bold :foreground ,yellow-active)))
;;;;; smartparens
    `(sp-pair-overlay-face ((,class :inherit modus-themes-special-warm)))
    `(sp-show-pair-enclosing ((,class :inherit modus-themes-special-mild)))
    `(sp-show-pair-match-face ((,class ,@(modus-themes--paren bg-paren-match
                                                              bg-paren-match-intense)
                                       :foreground ,fg-main)))
    `(sp-show-pair-mismatch-face ((,class :inherit modus-themes-intense-red)))
    `(sp-wrap-overlay-closing-pair ((,class :inherit sp-pair-overlay-face)))
    `(sp-wrap-overlay-face ((,class :inherit sp-pair-overlay-face)))
    `(sp-wrap-overlay-opening-pair ((,class :inherit sp-pair-overlay-face)))
    `(sp-wrap-tag-overlay-face ((,class :inherit sp-pair-overlay-face)))
;;;;; smerge
    `(smerge-base ((,class :inherit modus-themes-diff-changed)))
    `(smerge-lower ((,class :inherit modus-themes-diff-added)))
    `(smerge-markers ((,class :inherit modus-themes-diff-heading)))
    `(smerge-refined-added ((,class :inherit modus-themes-diff-refine-added)))
    `(smerge-refined-changed (()))
    `(smerge-refined-removed ((,class :inherit modus-themes-diff-refine-removed)))
    `(smerge-upper ((,class :inherit modus-themes-diff-removed)))
;;;;; spaceline
    `(spaceline-evil-emacs ((,class :inherit modus-themes-active-magenta)))
    `(spaceline-evil-insert ((,class :inherit modus-themes-active-green)))
    `(spaceline-evil-motion ((,class :inherit modus-themes-active-blue)))
    `(spaceline-evil-normal ((,class :background ,fg-alt :foreground ,bg-alt)))
    `(spaceline-evil-replace ((,class :inherit modus-themes-active-red)))
    `(spaceline-evil-visual ((,class :inherit modus-themes-active-cyan)))
    `(spaceline-flycheck-error ((,class :foreground ,red-active)))
    `(spaceline-flycheck-info ((,class :foreground ,cyan-active)))
    `(spaceline-flycheck-warning ((,class :foreground ,yellow-active)))
    `(spaceline-highlight-face ((,class :inherit modus-themes-fringe-blue)))
    `(spaceline-modified ((,class :inherit modus-themes-fringe-magenta)))
    `(spaceline-python-venv ((,class :foreground ,magenta-active)))
    `(spaceline-read-only ((,class :inherit modus-themes-fringe-red)))
    `(spaceline-unmodified ((,class :inherit modus-themes-fringe-cyan)))
;;;;; speedbar
    `(speedbar-button-face ((,class :inherit button)))
    `(speedbar-directory-face ((,class :inherit bold :foreground ,blue)))
    `(speedbar-file-face ((,class :foreground ,fg-main)))
    `(speedbar-highlight-face ((,class :inherit highlight)))
    `(speedbar-selected-face ((,class :inherit bold :foreground ,cyan)))
    `(speedbar-separator-face ((,class :inherit modus-themes-intense-neutral)))
    `(speedbar-tag-face ((,class :foreground ,yellow-alt-other)))
;;;;; spell-fu
    `(spell-fu-incorrect-face ((,class :inherit modus-themes-lang-error)))
;;;;; stripes
    `(stripes ((,class :background ,bg-alt)))
;;;;; suggest
    `(suggest-heading ((,class :inherit bold :foreground ,yellow-alt-other)))
;;;;; switch-window
    `(switch-window-background ((,class :background ,bg-dim)))
    `(switch-window-label ((,class :height 3.0 :foreground ,blue-intense)))
;;;;; swiper
    `(swiper-background-match-face-1 (( )))
    `(swiper-background-match-face-2 ((,class :inherit modus-themes-completion-match-0)))
    `(swiper-background-match-face-3 ((,class :inherit modus-themes-completion-match-1)))
    `(swiper-background-match-face-4 ((,class :inherit modus-themes-completion-match-2)))
    `(swiper-line-face ((,class :background ,bg-hl-alt-intense)))
    `(swiper-match-face-1 (( )))
    `(swiper-match-face-2 ((,class :inherit modus-themes-completion-match-0)))
    `(swiper-match-face-3 ((,class :inherit modus-themes-completion-match-1)))
    `(swiper-match-face-4 ((,class :inherit modus-themes-completion-match-2)))
;;;;; sx
    `(sx-inbox-item-type ((,class :foreground ,magenta-alt-other)))
    `(sx-inbox-item-type-unread ((,class :inherit (sx-inbox-item-type bold))))
    `(sx-question-list-answers ((,class :foreground ,green)))
    `(sx-question-list-answers-accepted ((,class :box t :foreground ,green)))
    `(sx-question-list-bounty ((,class :inherit bold :background ,bg-alt :foreground ,yellow)))
    `(sx-question-list-date ((,class :foreground ,fg-special-cold)))
    `(sx-question-list-favorite ((,class :inherit bold :foreground ,fg-special-warm)))
    `(sx-question-list-parent ((,class :foreground ,fg-main)))
    `(sx-question-list-read-question ((,class :inherit shadow)))
    `(sx-question-list-score ((,class :foreground ,fg-special-mild)))
    `(sx-question-list-score-upvoted ((,class :inherit (sx-question-list-score bold))))
    `(sx-question-list-unread-question ((,class :inherit bold :foreground ,fg-main)))
    `(sx-question-mode-accepted ((,class :inherit bold :height 1.3 :foreground ,green)))
    `(sx-question-mode-closed ((,class :inherit modus-themes-active-yellow :box (:line-width 2 :color nil))))
    `(sx-question-mode-closed-reason ((,class :box (:line-width 2 :color nil) :foreground ,fg-main)))
    `(sx-question-mode-content-face ((,class :background ,bg-dim)))
    `(sx-question-mode-date ((,class :foreground ,blue)))
    `(sx-question-mode-header ((,class :inherit bold :foreground ,cyan)))
    `(sx-question-mode-kbd-tag ((,class :inherit bold :height 0.9 :box (:line-width 3 :color ,fg-main :style released-button) :foreground ,fg-main)))
    `(sx-question-mode-score ((,class :foreground ,fg-dim)))
    `(sx-question-mode-score-downvoted ((,class :foreground ,yellow)))
    `(sx-question-mode-score-upvoted ((,class :inherit bold :foreground ,magenta)))
    `(sx-question-mode-title ((,class :inherit bold :foreground ,fg-main)))
    `(sx-question-mode-title-comments ((,class :inherit (shadow bold))))
    `(sx-tag ((,class :foreground ,magenta-alt)))
    `(sx-user-name ((,class :foreground ,blue-alt)))
    `(sx-user-reputation ((,class :inherit shadow)))
;;;;; symbol-overlay
    `(symbol-overlay-default-face ((,class :inherit modus-themes-special-warm)))
    `(symbol-overlay-face-1 ((,class :inherit modus-themes-intense-blue)))
    `(symbol-overlay-face-2 ((,class :inherit modus-themes-refine-magenta)))
    `(symbol-overlay-face-3 ((,class :inherit modus-themes-intense-yellow)))
    `(symbol-overlay-face-4 ((,class :inherit modus-themes-intense-magenta)))
    `(symbol-overlay-face-5 ((,class :inherit modus-themes-intense-red)))
    `(symbol-overlay-face-6 ((,class :inherit modus-themes-refine-red)))
    `(symbol-overlay-face-7 ((,class :inherit modus-themes-intense-cyan)))
    `(symbol-overlay-face-8 ((,class :inherit modus-themes-refine-cyan)))
;;;;; syslog-mode
    `(syslog-debug ((,class :inherit bold :foreground ,cyan-alt-other)))
    `(syslog-error ((,class :inherit error)))
    `(syslog-file ((,class :inherit bold :foreground ,fg-special-cold)))
    `(syslog-hide ((,class :background ,bg-main :foreground ,fg-main)))
    `(syslog-hour ((,class :inherit bold :foreground ,magenta-alt-other)))
    `(syslog-info ((,class :inherit success)))
    `(syslog-ip ((,class :inherit bold :foreground ,fg-special-mild :underline t)))
    `(syslog-su ((,class :inherit bold :foreground ,red-alt)))
    `(syslog-warn ((,class :inherit warning)))
;;;;; tab-bar-groups
    `(tab-bar-groups-tab-1 ((,class :inherit modus-themes-ui-variable-pitch :foreground ,blue-tab)))
    `(tab-bar-groups-tab-2 ((,class :inherit modus-themes-ui-variable-pitch :foreground ,red-tab)))
    `(tab-bar-groups-tab-3 ((,class :inherit modus-themes-ui-variable-pitch :foreground ,green-tab)))
    `(tab-bar-groups-tab-4 ((,class :inherit modus-themes-ui-variable-pitch :foreground ,orange-tab)))
    `(tab-bar-groups-tab-5 ((,class :inherit modus-themes-ui-variable-pitch :foreground ,purple-tab)))
    `(tab-bar-groups-tab-6 ((,class :inherit modus-themes-ui-variable-pitch :foreground ,cyan-tab)))
    `(tab-bar-groups-tab-7 ((,class :inherit modus-themes-ui-variable-pitch :foreground ,yellow-tab)))
    `(tab-bar-groups-tab-8 ((,class :inherit modus-themes-ui-variable-pitch :foreground ,magenta-tab)))
;;;;; tab-bar-mode
    `(tab-bar ((,class :inherit modus-themes-tab-backdrop)))
    `(tab-bar-tab-group-current ((,class ,@(modus-themes--tab bg-tab-active)
                                         :box (:line-width (2 . -2) :color "gray50"))))
    `(tab-bar-tab-group-inactive ((,class ,@(modus-themes--tab bg-tab-inactive bg-tab-inactive-accent fg-dim)
                                          :box (:line-width (2 . -2) :color "gray50"))))
    `(tab-bar-tab ((,class :inherit modus-themes-tab-active)))
    `(tab-bar-tab-inactive ((,class :inherit modus-themes-tab-inactive)))
;;;;; tab-line-mode
    `(tab-line ((,class :inherit modus-themes-tab-backdrop :height 0.95)))
    `(tab-line-close-highlight ((,class :foreground ,red)))
    `(tab-line-highlight ((,class :inherit modus-themes-active-blue)))
    `(tab-line-tab ((,class :inherit modus-themes-tab-active)))
    `(tab-line-tab-current ((,class :inherit tab-line-tab)))
    `(tab-line-tab-inactive ((,class :inherit modus-themes-tab-inactive)))
    `(tab-line-tab-inactive-alternate ((,class ,@(modus-themes--tab bg-tab-inactive-alt
                                                                    bg-tab-inactive-alt-accent fg-main nil t))))
    `(tab-line-tab-modified ((,class :foreground ,red-alt-other-faint)))
;;;;; table (built-in table.el)
    `(table-cell ((,class :background ,blue-nuanced-bg)))
;;;;; telega
    `(telega-button ((,class :box t :foreground ,blue)))
    `(telega-button-active ((,class :box ,blue-intense-bg :background ,blue-intense-bg :foreground ,fg-main)))
    `(telega-button-highlight ((,class :inherit modus-themes-subtle-magenta)))
    `(telega-chat-prompt ((,class :inherit bold)))
    `(telega-entity-type-code ((,class :inherit modus-themes-markup-verbatim)))
    `(telega-entity-type-mention ((,class :foreground ,cyan)))
    `(telega-entity-type-pre ((,class :inherit modus-themes-markup-code)))
    `(telega-entity-type-spoiler ((,class :background ,fg-main :foreground ,fg-main)))
    `(telega-msg-heading ((,class :background ,bg-alt)))
    `(telega-msg-self-title ((,class :inherit bold)))
    `(telega-root-heading ((,class :inherit modus-themes-subtle-neutral)))
    `(telega-secret-title ((,class :foreground ,magenta-alt)))
    `(telega-unmuted-count ((,class :foreground ,blue-alt-other)))
    `(telega-user-online-status ((,class :foreground ,cyan-active)))
    `(telega-username ((,class :foreground ,cyan-alt-other)))
    `(telega-webpage-chat-link ((,class :background ,bg-alt)))
    `(telega-webpage-fixed ((,class :inherit modus-themes-fixed-pitch :height 0.85)))
    `(telega-webpage-header ((,class :inherit modus-themes-variable-pitch :height 1.3)))
    `(telega-webpage-preformatted ((,class :inherit modus-themes-fixed-pitch :background ,bg-alt)))
    `(telega-webpage-subheader ((,class :inherit modus-themes-variable-pitch :height 1.15)))
;;;;; telephone-line
    `(telephone-line-accent-active ((,class :background ,fg-inactive :foreground ,bg-inactive)))
    `(telephone-line-accent-inactive ((,class :background ,bg-active :foreground ,fg-active)))
    `(telephone-line-error ((,class :inherit bold :foreground ,red-active)))
    `(telephone-line-evil ((,class :foreground ,fg-main)))
    `(telephone-line-evil-emacs ((,class :inherit telephone-line-evil :background ,magenta-intense-bg)))
    `(telephone-line-evil-insert ((,class :inherit telephone-line-evil :background ,green-intense-bg)))
    `(telephone-line-evil-motion ((,class :inherit telephone-line-evil :background ,yellow-intense-bg)))
    `(telephone-line-evil-normal ((,class :inherit telephone-line-evil :background ,bg-alt)))
    `(telephone-line-evil-operator ((,class :inherit telephone-line-evil :background ,yellow-subtle-bg)))
    `(telephone-line-evil-replace ((,class :inherit telephone-line-evil :background ,red-intense-bg)))
    `(telephone-line-evil-visual ((,class :inherit telephone-line-evil :background ,cyan-intense-bg)))
    `(telephone-line-projectile ((,class :foreground ,cyan-active)))
    `(telephone-line-unimportant ((,class :foreground ,fg-inactive)))
    `(telephone-line-warning ((,class :inherit bold :foreground ,yellow-active)))
;;;;; terraform-mode
    `(terraform--resource-name-face ((,class ,@(modus-themes--syntax-string
                                                magenta-alt-other magenta-alt-other-faint
                                                red-alt red-alt))))
    `(terraform--resource-type-face ((,class ,@(modus-themes--syntax-string
                                                green green-faint
                                                blue-alt magenta-alt))))
;;;;; term
    `(term ((,class :background ,bg-main :foreground ,fg-main)))
    `(term-bold ((,class :inherit bold)))
    `(term-color-black ((,class :background "gray35" :foreground "gray35")))
    `(term-color-blue ((,class :background ,blue :foreground ,blue)))
    `(term-color-cyan ((,class :background ,cyan :foreground ,cyan)))
    `(term-color-green ((,class :background ,green :foreground ,green)))
    `(term-color-magenta ((,class :background ,magenta :foreground ,magenta)))
    `(term-color-red ((,class :background ,red :foreground ,red)))
    `(term-color-white ((,class :background "gray65" :foreground "gray65")))
    `(term-color-yellow ((,class :background ,yellow :foreground ,yellow)))
    `(term-underline ((,class :underline t)))
;;;;; textsec
    `(textsec-suspicious (()))
;;;;; tomatinho
    `(tomatinho-ok-face ((,class :foreground ,blue-intense)))
    `(tomatinho-pause-face ((,class :foreground ,yellow-intense)))
    `(tomatinho-reset-face ((,class :inherit shadow)))
;;;;; transient
    `(transient-active-infix ((,class :inherit modus-themes-special-mild)))
    `(transient-amaranth ((,class :inherit bold :foreground ,yellow-alt)))
    ;; Placate the compiler for what is a spurious warning.  We also
    ;; have to do this with `eldoc-highlight-function-argument'.
    (list 'transient-argument `((,class :inherit bold :background ,cyan-nuanced-bg :foreground ,cyan)))
    `(transient-blue ((,class :inherit bold :foreground ,blue)))
    `(transient-disabled-suffix ((,class :inherit modus-themes-intense-red)))
    `(transient-enabled-suffix ((,class :inherit modus-themes-grue-background-subtle)))
    `(transient-heading ((,class :inherit bold :foreground ,fg-main)))
    `(transient-inactive-argument ((,class :inherit shadow)))
    `(transient-inactive-value ((,class :inherit shadow)))
    `(transient-key ((,class :inherit modus-themes-key-binding)))
    `(transient-mismatched-key ((,class :underline t)))
    `(transient-nonstandard-key ((,class :underline t)))
    `(transient-pink ((,class :inherit bold :foreground ,magenta-alt-faint)))
    `(transient-purple ((,class :inherit bold :foreground ,magenta-alt-other)))
    `(transient-red ((,class :inherit bold :foreground ,red-faint)))
    `(transient-teal ((,class :inherit bold :foreground ,cyan-alt-other)))
    `(transient-unreachable ((,class :inherit shadow)))
    `(transient-unreachable-key ((,class :inherit shadow)))
    `(transient-value ((,class :inherit bold :background ,yellow-nuanced-bg :foreground ,yellow-alt-other)))
;;;;; trashed
    `(trashed-deleted ((,class :inherit modus-themes-mark-del)))
    `(trashed-directory ((,class :foreground ,blue)))
    `(trashed-mark ((,class :inherit modus-themes-mark-symbol)))
    `(trashed-marked ((,class :inherit modus-themes-mark-alt)))
    `(trashed-restored ((,class :inherit modus-themes-mark-sel)))
    `(trashed-symlink ((,class :inherit modus-themes-link-symlink)))
;;;;; tree-sitter
    `(tree-sitter-hl-face:attribute ((,class :inherit font-lock-variable-name-face)))
    `(tree-sitter-hl-face:constant.builtin ((,class :inherit tree-sitter-hl-face:constant)))
    `(tree-sitter-hl-face:escape ((,class :inherit font-lock-regexp-grouping-backslash)))
    `(tree-sitter-hl-face:function ((,class :inherit font-lock-function-name-face)))
    `(tree-sitter-hl-face:function.call ((,class :inherit tree-sitter-hl-face:function)))
    `(tree-sitter-hl-face:label (( )))
    `(tree-sitter-hl-face:method.call (( )))
    `(tree-sitter-hl-face:operator ((,class :inherit modus-themes-bold)))
    `(tree-sitter-hl-face:property (( )))
    `(tree-sitter-hl-face:property.definition ((,class :inherit font-lock-variable-name-face)))
    `(tree-sitter-hl-face:punctuation (( )))
    `(tree-sitter-hl-face:punctuation.bracket (( )))
    `(tree-sitter-hl-face:punctuation.delimiter (( )))
    `(tree-sitter-hl-face:punctuation.special ((,class :inherit font-lock-regexp-grouping-construct)))
    `(tree-sitter-hl-face:string.special ((,class :inherit tree-sitter-hl-face:string)))
    `(tree-sitter-hl-face:tag ((,class :inherit font-lock-function-name-face)))
    `(tree-sitter-hl-face:type.argument (( )))
;;;;; treemacs
    `(treemacs-directory-collapsed-face ((,class :foreground ,magenta-alt)))
    `(treemacs-directory-face ((,class :inherit dired-directory)))
    `(treemacs-file-face ((,class :foreground ,fg-main)))
    `(treemacs-fringe-indicator-face ((,class :foreground ,fg-main)))
    `(treemacs-git-added-face ((,class :inherit success)))
    `(treemacs-git-conflict-face ((,class :inherit error)))
    `(treemacs-git-ignored-face ((,class :inherit shadow)))
    `(treemacs-git-modified-face ((,class :inherit warning)))
    `(treemacs-git-renamed-face ((,class :inherit italic)))
    `(treemacs-git-unmodified-face ((,class :foreground ,fg-main)))
    `(treemacs-git-untracked-face ((,class :inherit shadow)))
    `(treemacs-help-column-face ((,class :inherit modus-themes-bold :foreground ,magenta-alt-other :underline t)))
    `(treemacs-help-title-face ((,class :foreground ,blue-alt-other)))
    `(treemacs-on-failure-pulse-face ((,class :inherit modus-themes-intense-red)))
    `(treemacs-on-success-pulse-face ((,class :inherit modus-themes-grue-background-intense)))
    `(treemacs-root-face ((,class :inherit bold :foreground ,blue-alt-other :height 1.2 :underline t)))
    `(treemacs-root-remote-disconnected-face ((,class :inherit treemacs-root-remote-face :foreground ,yellow)))
    `(treemacs-root-remote-face ((,class :inherit treemacs-root-face :foreground ,magenta)))
    `(treemacs-root-remote-unreadable-face ((,class :inherit treemacs-root-unreadable-face)))
    `(treemacs-root-unreadable-face ((,class :inherit treemacs-root-face :strike-through t)))
    `(treemacs-tags-face ((,class :foreground ,blue-alt)))
;;;;; tty-menu
    `(tty-menu-disabled-face ((,class :background ,bg-alt :foreground ,fg-alt)))
    `(tty-menu-enabled-face ((,class :inherit bold :background ,bg-alt :foreground ,fg-main)))
    `(tty-menu-selected-face ((,class :inherit modus-themes-intense-blue)))
;;;;; tuareg
    `(caml-types-def-face ((,class :inherit modus-themes-subtle-red)))
    `(caml-types-expr-face ((,class :inherit modus-themes-subtle-green)))
    `(caml-types-occ-face ((,class :inherit modus-themes-subtle-green)))
    `(caml-types-scope-face ((,class :inherit modus-themes-subtle-blue)))
    `(caml-types-typed-face ((,class :inherit modus-themes-subtle-magenta)))
    `(tuareg-font-double-semicolon-face ((,class :inherit font-lock-preprocessor-face)))
    `(tuareg-font-lock-attribute-face ((,class :inherit font-lock-function-name-face)))
    `(tuareg-font-lock-constructor-face ((,class :foreground ,fg-main)))
    `(tuareg-font-lock-error-face ((,class :inherit (modus-themes-intense-red bold))))
    `(tuareg-font-lock-extension-node-face ((,class :background ,bg-alt :foreground ,magenta)))
    `(tuareg-font-lock-governing-face ((,class :inherit bold :foreground ,fg-main)))
    `(tuareg-font-lock-infix-extension-node-face ((,class :inherit font-lock-function-name-face)))
    `(tuareg-font-lock-interactive-directive-face ((,class :foreground ,fg-special-cold)))
    `(tuareg-font-lock-interactive-error-face ((,class :inherit error)))
    `(tuareg-font-lock-interactive-output-face ((,class :inherit font-lock-constant-face)))
    `(tuareg-font-lock-label-face ((,class :inherit font-lock-type-face)))
    `(tuareg-font-lock-line-number-face ((,class :foreground ,fg-special-warm)))
    `(tuareg-font-lock-module-face ((,class :inherit font-lock-builtin-face)))
    `(tuareg-font-lock-multistage-face ((,class :inherit bold :background ,bg-alt :foreground ,blue)))
    `(tuareg-font-lock-operator-face ((,class :inherit font-lock-preprocessor-face)))
    `(tuareg-opam-error-face ((,class :inherit error)))
    `(tuareg-opam-pkg-variable-name-face ((,class :inherit font-lock-variable-name-face)))
;;;;; typescript
    `(typescript-jsdoc-tag ((,class :inherit (font-lock-builtin-face font-lock-comment-face) :weight normal)))
    `(typescript-jsdoc-type ((,class :inherit (font-lock-type-face font-lock-comment-face) :weight normal)))
    `(typescript-jsdoc-value ((,class :inherit (font-lock-constant-face font-lock-comment-face) :weight normal)))
;;;;; undo-tree
    `(undo-tree-visualizer-active-branch-face ((,class :inherit bold :foreground ,fg-main)))
    `(undo-tree-visualizer-current-face ((,class :foreground ,blue-intense)))
    `(undo-tree-visualizer-default-face ((,class :inherit shadow)))
    `(undo-tree-visualizer-register-face ((,class :foreground ,magenta-intense)))
    `(undo-tree-visualizer-unmodified-face ((,class :foreground ,green-intense)))
;;;;; vc (vc-dir.el, vc-hooks.el)
    `(vc-dir-directory ((,class :foreground ,blue)))
    `(vc-dir-file ((,class :foreground ,fg-main)))
    `(vc-dir-header ((,class :foreground ,cyan-alt-other)))
    `(vc-dir-header-value ((,class :foreground ,magenta-alt-other)))
    `(vc-dir-mark-indicator ((,class :foreground ,blue-alt-other)))
    `(vc-dir-status-edited ((,class :foreground ,yellow)))
    `(vc-dir-status-ignored ((,class :inherit shadow)))
    `(vc-dir-status-up-to-date ((,class :foreground ,cyan)))
    `(vc-dir-status-warning ((,class :inherit error)))
    `(vc-conflict-state ((,class :inherit bold :foreground ,red-active)))
    `(vc-edited-state ((,class :foreground ,yellow-active)))
    `(vc-locally-added-state ((,class :foreground ,cyan-active)))
    `(vc-locked-state ((,class :foreground ,blue-active)))
    `(vc-missing-state ((,class :inherit modus-themes-slant :foreground ,magenta-active)))
    `(vc-needs-update-state ((,class :inherit modus-themes-slant :foreground ,green-active)))
    `(vc-removed-state ((,class :foreground ,red-active)))
    `(vc-state-base ((,class :foreground ,fg-active)))
    `(vc-up-to-date-state ((,class :foreground ,fg-special-cold)))
;;;;; vertico
    `(vertico-current ((,class :inherit modus-themes-completion-selected)))
;;;;; vertico-quick
    `(vertico-quick1 ((,class :inherit bold :background ,bg-char-0)))
    `(vertico-quick2 ((,class :inherit bold :background ,bg-char-1)))
;;;;; vimish-fold
    `(vimish-fold-fringe ((,class :foreground ,cyan-active)))
    `(vimish-fold-mouse-face ((,class :inherit modus-themes-intense-blue)))
    `(vimish-fold-overlay ((,class :background ,bg-alt :foreground ,fg-special-cold)))
;;;;; visible-mark
    `(visible-mark-active ((,class :background ,blue-intense-bg)))
    `(visible-mark-face1 ((,class :background ,cyan-intense-bg)))
    `(visible-mark-face2 ((,class :background ,yellow-intense-bg)))
    `(visible-mark-forward-face1 ((,class :background ,magenta-intense-bg)))
    `(visible-mark-forward-face2 ((,class :background ,green-intense-bg)))
;;;;; visual-regexp
    `(vr/group-0 ((,class :inherit modus-themes-intense-blue)))
    `(vr/group-1 ((,class :inherit modus-themes-intense-magenta)))
    `(vr/group-2 ((,class :inherit modus-themes-intense-green)))
    `(vr/match-0 ((,class :inherit modus-themes-refine-yellow)))
    `(vr/match-1 ((,class :inherit modus-themes-refine-yellow)))
    `(vr/match-separator-face ((,class :inherit (modus-themes-intense-neutral bold))))
;;;;; vterm
    `(vterm-color-black ((,class :background "gray35" :foreground "gray35")))
    `(vterm-color-blue ((,class :background ,blue :foreground ,blue)))
    `(vterm-color-cyan ((,class :background ,cyan :foreground ,cyan)))
    `(vterm-color-default ((,class :background ,bg-main :foreground ,fg-main)))
    `(vterm-color-green ((,class :background ,green :foreground ,green)))
    `(vterm-color-inverse-video ((,class :background ,bg-main :inverse-video t)))
    `(vterm-color-magenta ((,class :background ,magenta :foreground ,magenta)))
    `(vterm-color-red ((,class :background ,red :foreground ,red)))
    `(vterm-color-underline ((,class :foreground ,fg-special-warm :underline t)))
    `(vterm-color-white ((,class :background "gray65" :foreground "gray65")))
    `(vterm-color-yellow ((,class :background ,yellow :foreground ,yellow)))
;;;;; vundo
    `(vundo-highlight ((,class :inherit (bold vundo-node) :foreground ,red-intense)))
;;;;; wcheck-mode
    `(wcheck-default-face ((,class :foreground ,red :underline t)))
;;;;; web-mode
    `(web-mode-annotation-face ((,class :inherit web-mode-comment-face)))
    `(web-mode-annotation-html-face ((,class :inherit web-mode-comment-face)))
    `(web-mode-annotation-tag-face ((,class :inherit web-mode-comment-face :underline t)))
    `(web-mode-block-attr-name-face ((,class :inherit font-lock-constant-face)))
    `(web-mode-block-attr-value-face ((,class :inherit font-lock-type-face)))
    `(web-mode-block-comment-face ((,class :inherit web-mode-comment-face)))
    `(web-mode-block-control-face ((,class :inherit font-lock-builtin-face)))
    `(web-mode-block-delimiter-face ((,class :foreground ,fg-main)))
    `(web-mode-block-face ((,class :background ,bg-dim)))
    `(web-mode-block-string-face ((,class :inherit web-mode-string-face)))
    `(web-mode-bold-face ((,class :inherit bold)))
    `(web-mode-builtin-face ((,class :inherit font-lock-builtin-face)))
    `(web-mode-comment-face ((,class :inherit font-lock-comment-face)))
    `(web-mode-comment-keyword-face ((,class :inherit font-lock-warning-face)))
    `(web-mode-constant-face ((,class :inherit font-lock-constant-face)))
    `(web-mode-css-at-rule-face ((,class :inherit font-lock-constant-face)))
    `(web-mode-css-color-face ((,class :inherit font-lock-builtin-face)))
    `(web-mode-css-comment-face ((,class :inherit web-mode-comment-face)))
    `(web-mode-css-function-face ((,class :inherit font-lock-builtin-face)))
    `(web-mode-css-priority-face ((,class :inherit font-lock-warning-face)))
    `(web-mode-css-property-name-face ((,class :inherit font-lock-keyword-face)))
    `(web-mode-css-pseudo-class-face ((,class :inherit font-lock-doc-face)))
    `(web-mode-css-selector-face ((,class :inherit font-lock-keyword-face)))
    `(web-mode-css-string-face ((,class :inherit web-mode-string-face)))
    `(web-mode-css-variable-face ((,class :foreground ,fg-special-warm)))
    `(web-mode-current-column-highlight-face ((,class :background ,bg-alt)))
    `(web-mode-current-element-highlight-face ((,class :inherit modus-themes-special-mild)))
    `(web-mode-doctype-face ((,class :inherit modus-themes-slant :foreground ,fg-special-cold)))
    `(web-mode-error-face ((,class :inherit modus-themes-intense-red)))
    `(web-mode-filter-face ((,class :inherit font-lock-function-name-face)))
    `(web-mode-folded-face ((,class :underline t)))
    `(web-mode-function-call-face ((,class :inherit font-lock-function-name-face)))
    `(web-mode-function-name-face ((,class :inherit font-lock-function-name-face)))
    `(web-mode-html-attr-custom-face ((,class :inherit font-lock-variable-name-face)))
    `(web-mode-html-attr-engine-face ((,class :foreground ,fg-main)))
    `(web-mode-html-attr-equal-face ((,class :foreground ,fg-main)))
    `(web-mode-html-attr-name-face ((,class :inherit font-lock-variable-name-face)))
    `(web-mode-html-attr-value-face ((,class :inherit font-lock-constant-face)))
    `(web-mode-html-entity-face ((,class :inherit font-lock-negation-char-face)))
    `(web-mode-html-tag-bracket-face ((,class :foreground ,fg-dim)))
    `(web-mode-html-tag-custom-face ((,class :inherit font-lock-function-name-face)))
    `(web-mode-html-tag-face ((,class :inherit font-lock-function-name-face)))
    `(web-mode-html-tag-namespaced-face ((,class :inherit font-lock-builtin-face)))
    `(web-mode-html-tag-unclosed-face ((,class :inherit error :underline t)))
    `(web-mode-inlay-face ((,class :background ,bg-alt)))
    `(web-mode-italic-face ((,class :inherit italic)))
    `(web-mode-javascript-comment-face ((,class :inherit web-mode-comment-face)))
    `(web-mode-javascript-string-face ((,class :inherit web-mode-string-face)))
    `(web-mode-json-comment-face ((,class :inherit web-mode-comment-face)))
    `(web-mode-json-context-face ((,class :inherit font-lock-builtin-face)))
    `(web-mode-json-key-face ((,class :foreground ,blue-nuanced-fg)))
    `(web-mode-json-string-face ((,class :inherit web-mode-string-face)))
    `(web-mode-jsx-depth-1-face ((,class :background ,blue-intense-bg :foreground ,fg-main)))
    `(web-mode-jsx-depth-2-face ((,class :background ,blue-subtle-bg :foreground ,fg-main)))
    `(web-mode-jsx-depth-3-face ((,class :background ,bg-special-cold :foreground ,fg-special-cold)))
    `(web-mode-jsx-depth-4-face ((,class :background ,bg-alt :foreground ,blue-refine-fg)))
    `(web-mode-jsx-depth-5-face ((,class :background ,bg-alt :foreground ,blue-nuanced-fg)))
    `(web-mode-keyword-face ((,class :inherit font-lock-keyword-face)))
    `(web-mode-param-name-face ((,class :inherit font-lock-function-name-face)))
    `(web-mode-part-comment-face ((,class :inherit web-mode-comment-face)))
    `(web-mode-part-face ((,class :inherit web-mode-block-face)))
    `(web-mode-part-string-face ((,class :inherit web-mode-string-face)))
    `(web-mode-preprocessor-face ((,class :inherit font-lock-preprocessor-face)))
    `(web-mode-script-face ((,class :inherit web-mode-part-face)))
    `(web-mode-sql-keyword-face ((,class :inherit font-lock-negation-char-face)))
    `(web-mode-string-face ((,class :inherit font-lock-string-face)))
    `(web-mode-style-face ((,class :inherit web-mode-part-face)))
    `(web-mode-symbol-face ((,class :inherit font-lock-constant-face)))
    `(web-mode-type-face ((,class :inherit font-lock-builtin-face)))
    `(web-mode-underline-face ((,class :underline t)))
    `(web-mode-variable-name-face ((,class :inherit font-lock-variable-name-face)))
    `(web-mode-warning-face ((,class :inherit font-lock-warning-face)))
    `(web-mode-whitespace-face ((,class :background ,bg-whitespace :foreground ,fg-whitespace)))
;;;;; wgrep
    `(wgrep-delete-face ((,class :inherit warning)))
    `(wgrep-done-face ((,class :inherit success)))
    `(wgrep-face ((,class :inherit bold)))
    `(wgrep-file-face ((,class :foreground ,fg-special-warm)))
    `(wgrep-reject-face ((,class :inherit error)))
;;;;; which-function-mode
    `(which-func ((,class :foreground ,magenta-active)))
;;;;; which-key
    `(which-key-command-description-face ((,class :foreground ,fg-main)))
    `(which-key-group-description-face ((,class :foreground ,magenta-alt)))
    `(which-key-highlighted-command-face ((,class :foreground ,yellow :underline t)))
    `(which-key-key-face ((,class :inherit modus-themes-key-binding)))
    `(which-key-local-map-description-face ((,class :foreground ,fg-main)))
    `(which-key-note-face ((,class :foreground ,fg-special-warm)))
    `(which-key-separator-face ((,class :inherit shadow)))
    `(which-key-special-key-face ((,class :inherit bold :foreground ,red-alt)))
;;;;; whitespace-mode
    `(whitespace-big-indent ((,class :inherit modus-themes-subtle-red)))
    `(whitespace-empty ((,class :inherit modus-themes-intense-magenta)))
    `(whitespace-hspace ((,class :background ,bg-whitespace :foreground ,fg-whitespace)))
    `(whitespace-indentation ((,class :background ,bg-whitespace :foreground ,fg-whitespace)))
    `(whitespace-line ((,class :inherit modus-themes-subtle-yellow)))
    `(whitespace-newline ((,class :background ,bg-whitespace :foreground ,fg-whitespace)))
    `(whitespace-space ((,class :background ,bg-whitespace :foreground ,fg-whitespace)))
    `(whitespace-space-after-tab ((,class :inherit modus-themes-subtle-magenta)))
    `(whitespace-space-before-tab ((,class :inherit modus-themes-subtle-cyan)))
    `(whitespace-tab ((,class :background ,bg-whitespace :foreground ,fg-whitespace)))
    `(whitespace-trailing ((,class :inherit modus-themes-intense-red)))
;;;;; window-divider-mode
    `(window-divider ((,class :foreground ,fg-window-divider-inner)))
    `(window-divider-first-pixel ((,class :foreground ,fg-window-divider-outer)))
    `(window-divider-last-pixel ((,class :foreground ,fg-window-divider-outer)))
;;;;; winum
    `(winum-face ((,class :inherit modus-themes-bold :foreground ,cyan-active)))
;;;;; writegood-mode
    `(writegood-duplicates-face ((,class :background ,bg-alt :foreground ,red-alt :underline t)))
    `(writegood-passive-voice-face ((,class :inherit modus-themes-lang-warning)))
    `(writegood-weasels-face ((,class :inherit modus-themes-lang-error)))
;;;;; woman
    `(woman-addition ((,class :foreground ,magenta-alt-other)))
    `(woman-bold ((,class :inherit bold :foreground ,magenta-alt)))
    `(woman-italic ((,class :inherit italic :foreground ,cyan)))
    `(woman-unknown ((,class :foreground ,green-alt)))
;;;;; xah-elisp-mode
    `(xah-elisp-at-symbol ((,class :inherit font-lock-warning-face)))
    `(xah-elisp-cap-variable ((,class :inherit font-lock-preprocessor-face)))
    `(xah-elisp-command-face ((,class :inherit font-lock-type-face)))
    `(xah-elisp-dollar-symbol ((,class :inherit font-lock-variable-name-face)))
;;;;; xref
    `(xref-file-header ((,class :inherit bold :foreground ,fg-special-cold)))
    `(xref-line-number ((,class :inherit shadow)))
    `(xref-match ((,class :inherit match)))
;;;;; yaml-mode
    `(yaml-tab-face ((,class :inherit modus-themes-intense-red)))
;;;;; yasnippet
    `(yas-field-highlight-face ((,class :background ,bg-hl-alt-intense)))
;;;;; ztree
    `(ztreep-arrow-face ((,class :foreground ,fg-inactive)))
    `(ztreep-diff-header-face ((,class :inherit bold :height 1.2 :foreground ,fg-special-cold)))
    `(ztreep-diff-header-small-face ((,class :foreground ,fg-main)))
    `(ztreep-diff-model-add-face ((,class :inherit modus-themes-grue)))
    `(ztreep-diff-model-diff-face ((,class :foreground ,red)))
    `(ztreep-diff-model-ignored-face ((,class :inherit shadow :strike-through t)))
    `(ztreep-diff-model-normal-face ((,class :inherit shadow)))
    `(ztreep-expand-sign-face ((,class :inherit ztreep-arrow-face)))
    `(ztreep-header-face ((,class :inherit bold :height 1.2 :foreground ,fg-special-cold)))
    `(ztreep-leaf-face ((,class :foreground ,cyan)))
    `(ztreep-node-count-children-face ((,class :foreground ,fg-special-warm)))
    `(ztreep-node-face ((,class :foreground ,fg-main))))
  "Face specs for use with `modus-themes-theme'.")

(defconst modus-themes-custom-variables
  '(
;;;; ansi-colors
    `(ansi-color-faces-vector [default bold shadow italic underline success warning error])
    `(ansi-color-names-vector ["gray35" ,red ,green ,yellow ,blue ,magenta ,cyan "gray65"])
;;;; awesome-tray
    `(awesome-tray-mode-line-active-color ,blue)
    `(awesome-tray-mode-line-inactive-color ,bg-active)
;;;; chart
    `(chart-face-color-list
      '( ,red-graph-0-bg ,green-graph-0-bg ,yellow-graph-0-bg ,blue-graph-0-bg ,magenta-graph-0-bg ,cyan-graph-0-bg
         ,red-graph-1-bg ,green-graph-1-bg ,yellow-graph-1-bg ,blue-graph-1-bg ,magenta-graph-1-bg ,cyan-graph-1-bg))
;;;; exwm
    `(exwm-floating-border-color ,fg-window-divider-inner)
;;;; flymake fringe indicators
    `(flymake-error-bitmap '(flymake-double-exclamation-mark modus-themes-fringe-red))
    `(flymake-warning-bitmap '(exclamation-mark modus-themes-fringe-yellow))
    `(flymake-note-bitmap '(exclamation-mark modus-themes-fringe-cyan))
;;;; highlight-changes
    `(highlight-changes-colors nil)
    `(highlight-changes-face-list '(success warning error bold bold-italic))
;;;; ibuffer
    `(ibuffer-deletion-face 'modus-themes-mark-del)
    `(ibuffer-filter-group-name-face 'modus-themes-pseudo-header)
    `(ibuffer-marked-face 'modus-themes-mark-sel)
    `(ibuffer-title-face 'default)
;;;; hl-todo
    `(hl-todo-keyword-faces
      '(("HOLD" . ,yellow-alt)
        ("TODO" . ,magenta)
        ("NEXT" . ,magenta-alt-other)
        ("THEM" . ,magenta-alt)
        ("PROG" . ,cyan)
        ("OKAY" . ,cyan-alt)
        ("DONT" . ,green-alt)
        ("FAIL" . ,red)
        ("BUG" . ,red)
        ("DONE" . ,green)
        ("NOTE" . ,yellow-alt-other)
        ("KLUDGE" . ,yellow)
        ("HACK" . ,yellow)
        ("TEMP" . ,red-nuanced-fg)
        ("FIXME" . ,red-alt-other)
        ("XXX+" . ,red-alt)
        ("REVIEW" . ,cyan-alt-other)
        ("DEPRECATED" . ,blue-nuanced-fg)))
;;;; mini-modeline
    `(mini-modeline-face-attr '(:background unspecified))
;;;; pdf-tools
    `(pdf-view-midnight-colors
      '(,fg-main . ,bg-dim))
;;;; wid-edit
    `(widget-link-prefix ,(if (memq 'all-buttons modus-themes-box-buttons)
                              " "
                            "["))
    `(widget-link-suffix ,(if (memq 'all-buttons modus-themes-box-buttons)
                              " "
                            "]"))
    `(widget-mouse-face '(highlight widget-button))
    `(widget-push-button-prefix ,(if (memq 'all-buttons modus-themes-box-buttons)
                                     " "
                                   "["))
    `(widget-push-button-suffix ,(if (memq 'all-buttons modus-themes-box-buttons)
                                     " "
                                   "]"))
;;;; xterm-color
    `(xterm-color-names ["black" ,red ,green ,yellow ,blue ,magenta ,cyan "gray65"])
    `(xterm-color-names-bright ["gray35" ,red-alt ,green-alt ,yellow-alt ,blue-alt ,magenta-alt ,cyan-alt "white"])
    (if (or (eq modus-themes-org-blocks 'tinted-background)
            (eq modus-themes-org-blocks 'rainbow))
        `(org-src-block-faces
          `(("emacs-lisp" modus-themes-nuanced-magenta)
            ("elisp" modus-themes-nuanced-magenta)
            ("clojure" modus-themes-nuanced-magenta)
            ("clojurescript" modus-themes-nuanced-magenta)
            ("c" modus-themes-nuanced-blue)
            ("c++" modus-themes-nuanced-blue)
            ("sh" modus-themes-nuanced-green)
            ("shell" modus-themes-nuanced-green)
            ("html" modus-themes-nuanced-yellow)
            ("xml" modus-themes-nuanced-yellow)
            ("css" modus-themes-nuanced-red)
            ("scss" modus-themes-nuanced-red)
            ("python" modus-themes-nuanced-green)
            ("ipython" modus-themes-nuanced-magenta)
            ("r" modus-themes-nuanced-cyan)
            ("yaml" modus-themes-nuanced-cyan)
            ("conf" modus-themes-nuanced-cyan)
            ("docker" modus-themes-nuanced-cyan)))
      `(org-src-block-faces '())))
  "Custom variables for `modus-themes-theme'.")

;;;###autoload
(when load-file-name
  (let ((dir (file-name-directory load-file-name)))
    (unless (equal dir (expand-file-name "themes/" data-directory))
      (add-to-list 'custom-theme-load-path dir))))

(provide 'modus-themes)
;;; modus-themes.el ends here
