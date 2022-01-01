;;; modus-themes.el --- Highly accessible themes (WCAG AAA) -*- lexical-binding:t -*-

;; Copyright (C) 2019-2022 Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://gitlab.com/protesilaos/modus-themes
;; Version: 1.6.0
;; Last-Modified: <2021-09-29 08:47:03 +0300>
;; Package-Requires: ((emacs "27.1"))
;; Keywords: faces, theme, accessibility

;; This file is part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; The Modus themes conform with the highest standard for color-contrast
;; accessibility between background and foreground values (WCAG AAA).
;; This file contains all customization variables, helper functions,
;; interactive commands, and face specifications.  Please refer to the
;; official Info manual for further documentation (distributed with the
;; themes, or available at: <https://protesilaos.com/modus-themes>).
;;
;; The themes share the following customization variables:
;;
;;     modus-themes-headings                       (alist)
;;     modus-themes-org-agenda                     (alist)
;;     modus-themes-bold-constructs                (boolean)
;;     modus-themes-inhibit-reload                 (boolean)
;;     modus-themes-italic-constructs              (boolean)
;;     modus-themes-no-mixed-fonts                 (boolean)
;;     modus-themes-scale-headings                 (boolean)
;;     modus-themes-subtle-line-numbers            (boolean)
;;     modus-themes-success-deuteranopia           (boolean)
;;     modus-themes-variable-pitch-headings        (boolean)
;;     modus-themes-variable-pitch-ui              (boolean)
;;     modus-themes-completions                    (choice)
;;     modus-themes-diffs                          (choice)
;;     modus-themes-fringes                        (choice)
;;     modus-themes-hl-line                        (choice)
;;     modus-themes-lang-checkers                  (choice)
;;     modus-themes-links                          (choice)
;;     modus-themes-mail-citations                 (choice)
;;     modus-themes-mode-line                      (choice)
;;     modus-themes-org-blocks                     (choice)
;;     modus-themes-paren-match                    (choice)
;;     modus-themes-prompts                        (choice)
;;     modus-themes-region                         (choice)
;;     modus-themes-syntax                         (choice)
;;
;; The default scale for headings is as follows (it can be customized as
;; well---remember, no scaling takes place by default):
;;
;;     modus-themes-scale-1                        1.05
;;     modus-themes-scale-2                        1.1
;;     modus-themes-scale-3                        1.15
;;     modus-themes-scale-4                        1.2
;;     modus-themes-scale-title                    1.3
;;
;; There is another scaling-related option, which however is reserved
;; for special cases and is not used for headings:
;;
;;     modus-themes-scale-small                    0.9
;;
;; There also exist two unique customization variables for overriding
;; color palette values.  The specifics are documented in the manual.
;; The symbols are:
;;
;;     modus-themes-operandi-color-overrides       (alist)
;;     modus-themes-vivendi-color-overrides        (alist)
;;
;; Below is the list of explicitly supported packages or face groups
;; (there are implicitly supported packages as well, which inherit from
;; font-lock or some basic group).  You are encouraged to report any
;; missing package or change you would like to see.
;;
;;     ace-window
;;     ag
;;     alert
;;     all-the-icons
;;     annotate
;;     ansi-color
;;     anzu
;;     apropos
;;     apt-sources-list
;;     artbollocks-mode
;;     auctex and TeX
;;     auto-dim-other-buffers
;;     avy
;;     awesome-tray
;;     bbdb
;;     binder
;;     bm
;;     bongo
;;     boon
;;     bookmark
;;     breakpoint (provided by built-in gdb-mi.el)
;;     buffer-expose
;;     calendar and diary
;;     calfw
;;     centaur-tabs
;;     cfrs
;;     change-log and log-view (`vc-print-log' and `vc-print-root-log')
;;     cider
;;     circe
;;     color-rg
;;     column-enforce-mode
;;     company-mode
;;     company-posframe
;;     compilation-mode
;;     completions
;;     consult
;;     corfu
;;     counsel
;;     counsel-css
;;     counsel-org-capture-string
;;     cov
;;     cperl-mode
;;     css-mode
;;     csv-mode
;;     ctrlf
;;     cursor-flash
;;     custom (M-x customize)
;;     dap-mode
;;     dashboard (emacs-dashboard)
;;     deadgrep
;;     debbugs
;;     define-word
;;     deft
;;     dictionary
;;     diff-hl
;;     diff-mode
;;     dim-autoload
;;     dir-treeview
;;     dired
;;     dired-async
;;     dired-git
;;     dired-git-info
;;     dired-narrow
;;     dired-subtree
;;     diredc
;;     diredfl
;;     diredp (dired+)
;;     disk-usage
;;     display-fill-column-indicator-mode
;;     doom-modeline
;;     dynamic-ruler
;;     easy-jekyll
;;     easy-kill
;;     ebdb
;;     ediff
;;     eglot
;;     el-search
;;     eldoc
;;     eldoc-box
;;     elfeed
;;     elfeed-score
;;     elpher
;;     embark
;;     emms
;;     enh-ruby-mode (enhanced-ruby-mode)
;;     epa
;;     equake
;;     erc
;;     eros
;;     ert
;;     eshell
;;     eshell-fringe-status
;;     eshell-git-prompt
;;     eshell-prompt-extras (epe)
;;     eshell-syntax-highlighting
;;     evil (evil-mode)
;;     evil-goggles
;;     evil-snipe
;;     evil-visual-mark-mode
;;     eww
;;     exwm
;;     eyebrowse
;;     fancy-dabbrev
;;     flycheck
;;     flycheck-color-mode-line
;;     flycheck-indicator
;;     flycheck-posframe
;;     flymake
;;     flyspell
;;     flyspell-correct
;;     flx
;;     freeze-it
;;     frog-menu
;;     focus
;;     fold-this
;;     font-lock (generic syntax highlighting)
;;     forge
;;     fountain (fountain-mode)
;;     geiser
;;     git-commit
;;     git-gutter (and variants)
;;     git-lens
;;     git-rebase
;;     git-timemachine
;;     git-walktree
;;     gnus
;;     gotest
;;     golden-ratio-scroll-screen
;;     helm
;;     helm-ls-git
;;     helm-switch-shell
;;     helm-xref
;;     helpful
;;     highlight-blocks
;;     highlight-defined
;;     highlight-escape-sequences (`hes-mode')
;;     highlight-indentation
;;     highlight-numbers
;;     highlight-symbol
;;     highlight-tail
;;     highlight-thing
;;     hl-defined
;;     hl-fill-column
;;     hl-line-mode
;;     hl-todo
;;     hydra
;;     hyperlist
;;     ibuffer
;;     icomplete
;;     ido-mode
;;     iedit
;;     iflipb
;;     imenu-list
;;     indium
;;     info
;;     info-colors
;;     interaction-log
;;     ioccur
;;     isearch, occur, etc.
;;     isl (isearch-light)
;;     ivy
;;     ivy-posframe
;;     jira (org-jira)
;;     journalctl-mode
;;     js2-mode
;;     julia
;;     jupyter
;;     kaocha-runner
;;     keycast
;;     ledger-mode
;;     line numbers (`display-line-numbers-mode' and global variant)
;;     lsp-mode
;;     lsp-ui
;;     macrostep
;;     magit
;;     magit-imerge
;;     make-mode
;;     man
;;     marginalia
;;     markdown-mode
;;     markup-faces (`adoc-mode')
;;     mentor
;;     messages
;;     minibuffer-line
;;     minimap
;;     mmm-mode
;;     mode-line
;;     mood-line
;;     mpdel
;;     mu4e
;;     mu4e-conversation
;;     multiple-cursors
;;     neotree
;;     no-emoji
;;     notmuch
;;     num3-mode
;;     nxml-mode
;;     objed
;;     orderless
;;     org
;;     org-journal
;;     org-noter
;;     org-pomodoro
;;     org-recur
;;     org-roam
;;     org-superstar
;;     org-table-sticky-header
;;     org-tree-slide
;;     org-treescope
;;     origami
;;     outline-mode
;;     outline-minor-faces
;;     package (M-x list-packages)
;;     page-break-lines
;;     pandoc-mode
;;     paradox
;;     paren-face
;;     parrot
;;     pass
;;     pdf-tools
;;     persp-mode
;;     perspective
;;     phi-grep
;;     phi-search
;;     pkgbuild-mode
;;     pomidor
;;     popup
;;     powerline
;;     powerline-evil
;;     prism (see "Note for prism.el" in the manual)
;;     proced
;;     prodigy
;;     pulse
;;     quick-peek
;;     racket-mode
;;     rainbow-blocks
;;     rainbow-identifiers
;;     rainbow-delimiters
;;     rcirc
;;     recursion-indicator
;;     regexp-builder (also known as `re-builder')
;;     rg
;;     ripgrep
;;     rmail
;;     ruler-mode
;;     sallet
;;     selectrum
;;     selectrum-prescient
;;     semantic
;;     sesman
;;     shell-script-mode
;;     shortdoc
;;     show-paren-mode
;;     shr
;;     side-notes
;;     sieve-mode
;;     skewer-mode
;;     smart-mode-line
;;     smartparens
;;     smerge
;;     spaceline
;;     speedbar
;;     spell-fu
;;     spray
;;     stripes
;;     suggest
;;     switch-window
;;     swiper
;;     swoop
;;     sx
;;     symbol-overlay
;;     syslog-mode
;;     tab-bar-groups
;;     tab-bar-mode
;;     tab-line-mode
;;     table (built-in table.el)
;;     telega
;;     telephone-line
;;     terraform-mode
;;     term
;;     tomatinho
;;     transient (pop-up windows like Magit's)
;;     trashed
;;     treemacs
;;     tty-menu
;;     tuareg
;;     typescript
;;     undo-tree
;;     vc (vc-dir.el, vc-hooks.el)
;;     vc-annotate (C-x v g)
;;     vdiff
;;     vertico
;;     vimish-fold
;;     visible-mark
;;     visual-regexp
;;     volatile-highlights
;;     vterm
;;     wcheck-mode
;;     web-mode
;;     wgrep
;;     which-function-mode
;;     which-key
;;     whitespace-mode
;;     window-divider-mode
;;     winum
;;     writegood-mode
;;     woman
;;     xah-elisp-mode
;;     xref
;;     xterm-color (and ansi-colors)
;;     yaml-mode
;;     yasnippet
;;     ztree
;;
;; For a complete view of the project, also refer to the following files
;; (should be distributed in the same repository/directory as the
;; current item):
;;
;; - modus-operandi-theme.el    (Light theme)
;; - modus-vivendi-theme.el     (Dark theme)

;;; Code:



(eval-when-compile (require 'cl-lib))

(defgroup modus-themes ()
  "Options for `modus-operandi', `modus-vivendi'.
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
  "Faces defined my `modus-operandi' and `modus-vivendi'."
  :group 'modus-themes
  :link '(info-link "(modus-themes) Top")
  :prefix "modus-themes-"
  :tag "Modus Themes Faces")

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
    (bg-special-cold . "#dde3f4") (fg-special-cold . "#093060")
    (bg-special-mild . "#c4ede0") (fg-special-mild . "#184034")
    (bg-special-warm . "#f0e0d4") (fg-special-warm . "#5d3026")
    (bg-special-calm . "#f8ddea") (fg-special-calm . "#61284f")
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
    (red-graph-0-bg . "#ef6f79")
    (red-graph-1-bg . "#ff9f9f")
    (green-graph-0-bg . "#49d239")
    (green-graph-1-bg . "#6dec6d")
    (yellow-graph-0-bg . "#efec08")
    (yellow-graph-1-bg . "#dbff4e")
    (blue-graph-0-bg . "#55a2f0")
    (blue-graph-1-bg . "#7fcfff")
    (magenta-graph-0-bg . "#ba86ef")
    (magenta-graph-1-bg . "#e7afff")
    (cyan-graph-0-bg . "#30d3f0")
    (cyan-graph-1-bg . "#6fefff")
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
    ;; bg-tab-bar is only intended for the bar that holds the tabs and
    ;; can only be combined with fg-main
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
    (bg-hl-line-intense-accent . "#b9e1ef")
    (bg-hl-alt . "#fbeee0")
    (bg-hl-alt-intense . "#e8dfd1")
    (bg-paren-match . "#e0af82")
    (bg-paren-match-intense . "#c488ff")
    (bg-paren-expression . "#dff0ff")
    (bg-region . "#bcbcbc")
    (bg-region-accent . "#afafef")
    (bg-region-accent-subtle . "#efdfff")

    (bg-tab-bar . "#d5d5d5")
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
    (bg-special-cold . "#203448") (fg-special-cold . "#c6eaff")
    (bg-special-mild . "#00322e") (fg-special-mild . "#bfebe0")
    (bg-special-warm . "#382f27") (fg-special-warm . "#f8dec0")
    (bg-special-calm . "#392a48") (fg-special-calm . "#fbd6f4")
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
    (red-graph-0-bg . "#af0404")
    (red-graph-1-bg . "#801f2f")
    (green-graph-0-bg . "#24ba2f")
    (green-graph-1-bg . "#0f8f07")
    (yellow-graph-0-bg . "#ffd03e")
    (yellow-graph-1-bg . "#d7d800")
    (blue-graph-0-bg . "#406fff")
    (blue-graph-1-bg . "#2f50c8")
    (magenta-graph-0-bg . "#af7bee")
    (magenta-graph-1-bg . "#7f59cf")
    (cyan-graph-0-bg . "#47dcfa")
    (cyan-graph-1-bg . "#0bc0df")
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
    ;; bg-tab-bar is only intended for the bar that holds the tabs and
    ;; can only be combined with fg-main
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
    (bg-hl-line-intense-accent . "#00353f")
    (bg-hl-alt . "#181732")
    (bg-hl-alt-intense . "#282e46")
    (bg-paren-match . "#5f362f")
    (bg-paren-match-intense . "#7416b5")
    (bg-paren-expression . "#221044")
    (bg-region . "#3c3c3c")
    (bg-region-accent . "#4f3d88")
    (bg-region-accent-subtle . "#240f55")

    (bg-tab-bar . "#2c2c2c")
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

    (bg-diff-focus-added . "#203d20") (fg-diff-focus-added . "#b4ddb4")
    (bg-diff-focus-added-deuteran . "#00405f") (fg-diff-focus-added-deuteran . "#bfe4ff")
    (bg-diff-focus-changed . "#4a3a10") (fg-diff-focus-changed . "#d0daaf")
    (bg-diff-focus-removed . "#5e2526") (fg-diff-focus-removed . "#eebdba")

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
  :group 'modus-theme-faces)

(defface modus-themes-subtle-green nil
  "Subtle green background combined with a dimmed foreground.
This is used for general purpose highlighting, mostly in buffers
or for completion interfaces.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-subtle-yellow nil
  "Subtle yellow background combined with a dimmed foreground.
This is used for general purpose highlighting, mostly in buffers
or for completion interfaces.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-subtle-blue nil
  "Subtle blue background combined with a dimmed foreground.
This is used for general purpose highlighting, mostly in buffers
or for completion interfaces.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-subtle-magenta nil
  "Subtle magenta background combined with a dimmed foreground.
This is used for general purpose highlighting, mostly in buffers
or for completion interfaces.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-subtle-cyan nil
  "Subtle cyan background combined with a dimmed foreground.
This is used for general purpose highlighting, mostly in buffers
or for completion interfaces.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-subtle-neutral nil
  "Subtle gray background combined with a dimmed foreground.
This is used for general purpose highlighting, mostly in buffers
or for completion interfaces.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-intense-red nil
  "Intense red background combined with the main foreground.
This is used for general purpose highlighting, mostly in buffers
or for completion interfaces.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-intense-green nil
  "Intense green background combined with the main foreground.
This is used for general purpose highlighting, mostly in buffers
or for completion interfaces.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-intense-yellow nil
  "Intense yellow background combined with the main foreground.
This is used for general purpose highlighting, mostly in buffers
or for completion interfaces.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-intense-blue nil
  "Intense blue background combined with the main foreground.
This is used for general purpose highlighting, mostly in buffers
or for completion interfaces.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-intense-magenta nil
  "Intense magenta background combined with the main foreground.
This is used for general purpose highlighting, mostly in buffers
or for completion interfaces.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-intense-cyan nil
  "Intense cyan background combined with the main foreground.
This is used for general purpose highlighting, mostly in buffers
or for completion interfaces.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-intense-neutral nil
  "Intense gray background combined with the main foreground.
This is used for general purpose highlighting, mostly in buffers
or for completion interfaces.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-refine-red nil
  "Combination of accented red background and foreground.
This is used for general purpose highlighting, mostly in buffers
or for completion interfaces.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-refine-green nil
  "Combination of accented green background and foreground.
This is used for general purpose highlighting, mostly in buffers
or for completion interfaces.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-refine-yellow nil
  "Combination of accented yellow background and foreground.
This is used for general purpose highlighting, mostly in buffers
or for completion interfaces.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-refine-blue nil
  "Combination of accented blue background and foreground.
This is used for general purpose highlighting, mostly in buffers
or for completion interfaces.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-refine-magenta nil
  "Combination of accented magenta background and foreground.
This is used for general purpose highlighting, mostly in buffers
or for completion interfaces.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-refine-cyan nil
  "Combination of accented cyan background and foreground.
This is used for general purpose highlighting, mostly in buffers
or for completion interfaces.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-active-red nil
  "A red background meant for use on the mode line or similar.
This is combined with the mode lines primary foreground value.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-active-green nil
  "A green background meant for use on the mode line or similar.
This is combined with the mode lines primary foreground value.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-active-yellow nil
  "A yellow background meant for use on the mode line or similar.
This is combined with the mode lines primary foreground value.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-active-blue nil
  "A blue background meant for use on the mode line or similar.
This is combined with the mode lines primary foreground value.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-active-magenta nil
  "A magenta background meant for use on the mode line or similar.
This is combined with the mode lines primary foreground value.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-active-cyan nil
  "A cyan background meant for use on the mode line or similar.
This is combined with the mode lines primary foreground value.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-fringe-red nil
  "A red background meant for use on the fringe or similar.
This is combined with the main foreground value.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-fringe-green nil
  "A green background meant for use on the fringe or similar.
This is combined with the main foreground value.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-fringe-yellow nil
  "A yellow background meant for use on the fringe or similar.
This is combined with the main foreground value.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-fringe-blue nil
  "A blue background meant for use on the fringe or similar.
This is combined with the main foreground value.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-fringe-magenta nil
  "A magenta background meant for use on the fringe or similar.
This is combined with the main foreground value.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-fringe-cyan nil
  "A cyan background meant for use on the fringe or similar.
This is combined with the main foreground value.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-nuanced-red nil
  "A nuanced red background.
This does not specify a foreground of its own.  Instead it is
meant to serve as the backdrop for elements such as Org blocks,
headings, and any other surface that needs to retain the colors
on display.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-nuanced-green nil
  "A nuanced green background.
This does not specify a foreground of its own.  Instead it is
meant to serve as the backdrop for elements such as Org blocks,
headings, and any other surface that needs to retain the colors
on display.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-nuanced-yellow nil
  "A nuanced yellow background.
This does not specify a foreground of its own.  Instead it is
meant to serve as the backdrop for elements such as Org blocks,
headings, and any other surface that needs to retain the colors
on display.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-nuanced-blue nil
  "A nuanced blue background.
This does not specify a foreground of its own.  Instead it is
meant to serve as the backdrop for elements such as Org blocks,
headings, and any other surface that needs to retain the colors
on display.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-nuanced-magenta nil
  "A nuanced magenta background.
This does not specify a foreground of its own.  Instead it is
meant to serve as the backdrop for elements such as Org blocks,
headings, and any other surface that needs to retain the colors
on display.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-nuanced-cyan nil
  "A nuanced cyan background.
This does not specify a foreground of its own.  Instead it is
meant to serve as the backdrop for elements such as Org blocks,
headings, and any other surface that needs to retain the colors
on display.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-special-cold nil
  "Combines the 'special cold' background and foreground values.
This is intended for cases when a neutral gray background is not
suitable and where a combination of more saturated colors would
not be appropriate.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-special-mild nil
  "Combines the 'special mild' background and foreground values.
This is intended for cases when a neutral gray background is not
suitable and where a combination of more saturated colors would
not be appropriate.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-special-warm nil
  "Combines the 'special warm' background and foreground values.
This is intended for cases when a neutral gray background is not
suitable and where a combination of more saturated colors would
not be appropriate.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-special-calm nil
  "Combines the 'special calm' background and foreground values.
This is intended for cases when a neutral gray background is not
suitable and where a combination of more saturated colors would
not be appropriate.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-diff-added nil
  "Combines green colors for the 'added' state in diffs.
The applied colors are contingent on the value assigned to
`modus-themes-diffs'.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-diff-changed nil
  "Combines yellow colors for the 'changed' state in diffs.
The applied colors are contingent on the value assigned to
`modus-themes-diffs'.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-diff-removed nil
  "Combines red colors for the 'removed' state in diffs.
The applied colors are contingent on the value assigned to
`modus-themes-diffs'.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-diff-refine-added nil
  "Combines green colors for word-wise 'added' state in diffs.
The applied colors are contingent on the value assigned to
`modus-themes-diffs'.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-diff-refine-changed nil
  "Combines yellow colors for word-wise 'changed' state in diffs.
The applied colors are contingent on the value assigned to
`modus-themes-diffs'.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-diff-refine-removed nil
  "Combines red colors for word-wise 'removed' state in diffs.
The applied colors are contingent on the value assigned to
`modus-themes-diffs'.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-diff-focus-added nil
  "Combines green colors for the focused 'added' state in diffs.
The applied colors are contingent on the value assigned to
`modus-themes-diffs'.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-diff-focus-changed nil
  "Combines yellow colors for the focused 'changed' state in.
The applied colors are contingent on the value assigned to
`modus-themes-diffs'.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-diff-focus-removed nil
  "Combines red colors for the focused 'removed' state in diffs.
The applied colors are contingent on the value assigned to
`modus-themes-diffs'.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-diff-heading nil
  "Combines blue colors for the diff hunk heading.
The applied colors are contingent on the value assigned to
`modus-themes-diffs'.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-pseudo-header nil
  "Generic style for some elements that function like headings.
The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-mark-alt nil
  "Combines yellow colors for marking special lines.
This is intended for use in modes such as Dired, Ibuffer, Proced.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-mark-del nil
  "Combines red colors for marking deletable lines.
This is intended for use in modes such as Dired, Ibuffer, Proced.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-mark-sel nil
  "Combines green colors for marking lines.
This is intended for use in modes such as Dired, Ibuffer, Proced.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-mark-symbol nil
  "Applies a blue color and other styles for mark indicators.
This is intended for use in modes such as Dired, Ibuffer, Proced.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-heading-1 nil
  "General purpose face for use in headings level 1.
The exact attributes assigned to this face are contingent on the
values assigned to the `modus-themes-headings' variable.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-heading-2 nil
  "General purpose face for use in headings level 2.
The exact attributes assigned to this face are contingent on the
values assigned to the `modus-themes-headings' variable.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-heading-3 nil
  "General purpose face for use in headings level 3.
The exact attributes assigned to this face are contingent on the
values assigned to the `modus-themes-headings' variable.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-heading-4 nil
  "General purpose face for use in headings level 4.
The exact attributes assigned to this face are contingent on the
values assigned to the `modus-themes-headings' variable.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-heading-5 nil
  "General purpose face for use in headings level 5.
The exact attributes assigned to this face are contingent on the
values assigned to the `modus-themes-headings' variable.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-heading-6 nil
  "General purpose face for use in headings level 6.
The exact attributes assigned to this face are contingent on the
values assigned to the `modus-themes-headings' variable.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-heading-7 nil
  "General purpose face for use in headings level 7.
The exact attributes assigned to this face are contingent on the
values assigned to the `modus-themes-headings' variable.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-heading-8 nil
  "General purpose face for use in headings level 8.
The exact attributes assigned to this face are contingent on the
values assigned to the `modus-themes-headings' variable.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-hl-line nil
  "General purpose face for the current line.
The exact attributes assigned to this face are contingent on the
values assigned to the `modus-themes-hl-line' variable.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-bold nil
  "Generic face for applying a conditional bold weight.
This behaves in accordance with `modus-themes-bold-constructs'.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-slant nil
  "Generic face for applying a conditional slant (italics).
This behaves in accordance with `modus-themes-italic-constructs'.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-variable-pitch nil
  "Generic face for applying a conditional `variable-pitch'.
This behaves in accordance with `modus-themes-no-mixed-fonts',
`modus-themes-variable-pitch-headings' for all heading levels,
and `modus-themes-variable-pitch-ui'.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-fixed-pitch nil
  "Generic face for applying a conditional `fixed-pitch'.
This behaves in accordance with `modus-themes-no-mixed-fonts'.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-graph-red-0 nil
  "Special subdued red face for use in graphs.
This is intended to be applied in contexts such as the Org agenda
habit graph where faithfulness to the semantics of a color value
is of paramount importance.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-graph-red-1 nil
  "Special prominent red face for use in graphs.
This is intended to be applied in contexts such as the Org agenda
habit graph where faithfulness to the semantics of a color value
is of paramount importance.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-graph-green-0 nil
  "Special subdued green face for use in graphs.
This is intended to be applied in contexts such as the Org agenda
habit graph where faithfulness to the semantics of a color value
is of paramount importance.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-graph-green-1 nil
  "Special prominent green face for use in graphs.
This is intended to be applied in contexts such as the Org agenda
habit graph where faithfulness to the semantics of a color value
is of paramount importance.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-graph-yellow-0 nil
  "Special subdued yellow face for use in graphs.
This is intended to be applied in contexts such as the Org agenda
habit graph where faithfulness to the semantics of a color value
is of paramount importance.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-graph-yellow-1 nil
  "Special prominent yellow face for use in graphs.
This is intended to be applied in contexts such as the Org agenda
habit graph where faithfulness to the semantics of a color value
is of paramount importance.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-graph-blue-0 nil
  "Special subdued blue face for use in graphs.
This is intended to be applied in contexts such as the Org agenda
habit graph where faithfulness to the semantics of a color value
is of paramount importance.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-graph-blue-1 nil
  "Special prominent blue face for use in graphs.
This is intended to be applied in contexts such as the Org agenda
habit graph where faithfulness to the semantics of a color value
is of paramount importance.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-graph-magenta-0 nil
  "Special subdued magenta face for use in graphs.
This is intended to be applied in contexts such as the Org agenda
habit graph where faithfulness to the semantics of a color value
is of paramount importance.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-graph-magenta-1 nil
  "Special prominent magenta face for use in graphs.
This is intended to be applied in contexts such as the Org agenda
habit graph where faithfulness to the semantics of a color value
is of paramount importance.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-graph-cyan-0 nil
  "Special subdued cyan face for use in graphs.
This is intended to be applied in contexts such as the Org agenda
habit graph where faithfulness to the semantics of a color value
is of paramount importance.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-graph-cyan-1 nil
  "Special prominent cyan face for use in graphs.
This is intended to be applied in contexts such as the Org agenda
habit graph where faithfulness to the semantics of a color value
is of paramount importance.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-lang-note nil
  "Generic face for linter or spell checker notes.
The exact attributes and color combinations are controlled by
`modus-themes-lang-checkers'.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-lang-warning nil
  "Generic face for linter or spell checker warnings.
The exact attributes and color combinations are controlled by
`modus-themes-lang-checkers'.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-lang-error nil
  "Generic face for linter or spell checker errors.
The exact attributes and color combinations are controlled by
`modus-themes-lang-checkers'.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-reset-soft nil
  "Generic face to set most face properties to nil.

This is intended to be inherited by faces that should not retain
properties from their context (e.g. an overlay over an underlined
text should not be underlined as well) yet still blend in.  Also
see `modus-themes-reset-hard'.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-reset-hard nil
  "Generic face to set all face properties to nil.

This is intended to be inherited by faces that should not retain
properties from their context (e.g. an overlay over an underlined
text should not be underlined as well) and not blend in.  Also
see `modus-themes-reset-soft'.

The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-key-binding nil
  "Generic face for key bindings.
The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-search-success nil
  "Generic face for successful search.
The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-search-success-modeline nil
  "Generic mode line indicator for successful search.
The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-search-success-lazy nil
  "Generic face for successful, lazily highlighted search.
The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)

(defface modus-themes-prompt nil
  "Generic face for command prompts.
The actual styling of the face is done by `modus-themes-faces'."
  :group 'modus-theme-faces)



;;; Customization variables

(defcustom modus-themes-inhibit-reload t
  "Control theme reload when setting options with Customize.

By default, customizing a theme-related user option through the
Custom interfaces or with `customize-set-variable' will not
reload the currently active Modus theme.

Enable this behaviour by setting this variable to nil."
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
  :link '(info-link "(modus-themes) Override colors (DIY)"))

(defcustom modus-themes-vivendi-color-overrides nil
  "Override colors in the Modus Vivendi palette.

For form, see `modus-themes-vivendi-colors'."
  :group 'modus-themes
  :package-version '(modus-themes . "1.1.0")
  :version "28.1"
  :type '(alist :key-type symbol :value-type color)
  :set #'modus-themes--set-option
  :initialize #'custom-initialize-default
  :link '(info-link "(modus-themes) Override colors (DIY)"))

;; The byte compiler complains when a defcustom isn't a top level form
(let* ((names (mapcar (lambda (pair)
                        (symbol-name (car pair)))
                      modus-themes-operandi-colors))
       (colors (mapcar #'intern (sort names #'string<))))
  (put 'modus-themes-operandi-color-overrides
       'custom-options (copy-sequence colors))
  (put 'modus-themes-vivendi-color-overrides
       'custom-options (copy-sequence colors)))

(define-obsolete-variable-alias
  'modus-themes-slanted-constructs
  'modus-themes-italic-constructs
  "1.5.0")

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

(defcustom modus-themes-variable-pitch-headings nil
  "Use proportional fonts (variable-pitch) in headings."
  :group 'modus-themes
  :package-version '(modus-themes . "1.0.0")
  :version "28.1"
  :type 'boolean
  :set #'modus-themes--set-option
  :initialize #'custom-initialize-default
  :link '(info-link "(modus-themes) Headings' typeface"))

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

(defcustom modus-themes-no-mixed-fonts nil
  "Disable inheritance from `fixed-pitch' in some faces.

This is done by default to allow spacing-sensitive constructs,
such as Org tables and code blocks, to remain monospaced when
users opt for something like the command `variable-pitch-mode'.
The downside with the default is that users need to explicitly
configure the font family of `fixed-pitch' in order to get a
consistent experience.  That may be something they do not want to
do.  Hence this option to disable any kind of technique for
mixing fonts."
  :group 'modus-themes
  :package-version '(modus-themes . "1.0.0")
  :version "28.1"
  :type 'boolean
  :set #'modus-themes--set-option
  :initialize #'custom-initialize-default
  :link '(info-link "(modus-themes) No mixed fonts"))

(defconst modus-themes--headings-choice
  '(set :tag "Properties" :greedy t
        (const :tag "Background color" background)
        (const :tag "Overline" overline)
        (const :tag "No bold weight" no-bold)
        (choice :tag "Colors"
                (const :tag "Subtle colors" nil)
                (const :tag "Rainbow colors" rainbow)
                (const :tag "Monochrome" monochrome)))
  "Refer to the doc string of `modus-themes-headings'.
This is a helper variable intended for internal use.")

(defcustom modus-themes-headings nil
  "Heading styles with optional list of values for levels 1-8.

This is an alist that accepts a (key . list-of-values)
combination.  The key is either a number, representing the
heading's level or t, which pertains to the fallback style.  The
list of values covers symbols that refer to properties, as
described below.  Here is a sample, followed by a presentation of
all available properties:

    (setq modus-themes-headings
          '((1 . (background overline))
            (2 . (overline rainbow))
            (t . (monochrome))))

By default (a nil value for this variable), all headings have a
bold typographic weight and use a desaturated text color.

A `rainbow' property makes the text color more saturated.

An `overline' property draws a line above the area of the
heading.

A `background' property adds a subtle tinted color to the
background of the heading.

A `no-bold' property removes the bold weight from the heading's
text.

A `monochrome' property makes all headings the same base color,
which is that of the default for the active theme (black/white).
When `background' is also set, `monochrome' changes its color to
gray.  If both `monochrome' and `rainbow' are set, the former
takes precedence.

Combinations of any of those properties are expressed as a list,
like in these examples:

    (no-bold)
    (rainbow background)
    (overline monochrome no-bold)

The order in which the properties are set is not significant.

In user configuration files the form may look like this:

    (setq modus-themes-headings
          '((1 . (background overline rainbow))
            (2 . (background overline))
            (t . (overline no-bold))))

When defining the styles per heading level, it is possible to
pass a non-nil value (t) instead of a list of properties.  This
will retain the original aesthetic for that level.  For example:

    (setq modus-themes-headings
          '((1 . t)           ; keep the default style
            (2 . (background overline))
            (t . (rainbow)))) ; style for all other headings

    (setq modus-themes-headings
          '((1 . (background overline))
            (2 . (rainbow no-bold))
            (t . t))) ; default style for all other levels

For Org users, the extent of the heading depends on the variable
`org-fontify-whole-heading-line'.  This affects the `overline'
and `background' properties.  Depending on the version of Org,
there may be others, such as `org-fontify-done-headline'.

Also read `modus-themes-scale-headings' to change the height of
headings and `modus-themes-variable-pitch-headings' to make them
use a proportionately spaced font."
  :group 'modus-themes
  :package-version '(modus-themes . "1.5.0")
  :version "28.1"
  :type `(alist
          :options ,(mapcar (lambda (el)
                              (list el modus-themes--headings-choice))
                            '(1 2 3 4 5 6 7 8 t))
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
          '((header-block . (variable-pitch scale-title))
            (header-date . (grayscale workaholic bold-today))
            (event . (accented scale-small))
            (scheduled . uniform)
            (habit . traffic-light)))

A `header-block' key applies to elements that concern the
headings which demarcate blocks in the structure of the agenda.
By default (a nil value) those are rendered in a bold typographic
weight, plus a height that is slightly taller than the default
font size.  Acceptable values come in the form of a list that can
include either or both of those properties:

- `variable-pitch' to use a proportionately spaced typeface;
- `scale-title' to increase height to `modus-themes-scale-title'
  OR `no-scale' to set the font to the same height as the rest of
  the buffer.

In case both `scale-title' and `no-scale' are in the list, the
latter takes precedence.

Example usage:

    (header-block . nil)
    (header-block . (scale-title))
    (header-block . (no-scale))
    (header-block . (variable-pitch scale-title))

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
- `bold-all' to render all date headings in a bold weight.
- `scale-heading' increases the height of the date headings to
  the value of `modus-themes-scale-1' (which is the first step in
  the scale for regular headings).
- `underline-today' applies an underline to the current date
  while removing the background it has by default.

For example:

    (header-date . nil)
    (header-date . (workaholic))
    (header-date . (grayscale bold-all))
    (header-date . (grayscale workaholic))
    (header-date . (grayscale workaholic bold-today))
    (header-date . (grayscale workaholic bold-today scale-heading))

An `event' key covers events from the diary and other entries
that derive from a symbolic expression or sexp (e.g. phases of
the moon, holidays).  By default those have a gray
foreground (the default is a nil value or an empty list).  This
key accepts a list of properties.  Those are:

- `scale-small' reduces the height of the entries to the value of
  the user option `modus-themes-scale-small' (0.9 the height of
  the main font size by default).
- `accented' applies an accent value to the event's foreground,
  replacing the original gray.
- `italic' adds a slant to the font's forms (italic or oblique
  forms, depending on the typeface)

For example:

    (event . nil)
    (event . (scale-small))
    (event . (scale-small accented))
    (event . (scale-small accented italic))

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
  variants appear uniformly, but differently from it, the 'clear'
  state is rendered in a green hue, instead of the original blue.
  This is meant to capture the use-case where a habit task being
  \"too early\" is less important than it being \"too late\".
  The difference between ready and clear states is attenuated by
  painting both of them using shades of green.  This option thus
  highlights the alert and overdue states.
- `traffic-light-deuteranopia' is like the `traffic-light' except
  its three colors are red, yellow, and blue to be suitable for
  users with red-green color deficiency (deuteranopia).

For example:

    (habit . nil)
    (habit . simplified)
    (habit . traffic-light)"
  :group 'modus-themes
  :package-version '(modus-themes . "1.6.0")
  :version "28.1"
  :type '(set
          (cons :tag "Block header"
                (const header-block)
                (set :tag "Header presentation" :greedy t
                     (choice :tag "Font style"
                             (const :tag "Use the original typeface (default)" nil)
                             (const :tag "Use `variable-pitch' font" variable-pitch))
                     (choice :tag "Scaling"
                             (const :tag "Slight increase in height (default)" nil)
                             (const :tag "Do not scale" no-scale)
                             (const :tag "Scale to match `modus-themes-scale-title'" scale-title))))
          (cons :tag "Date header" :greedy t
                (const header-date)
                (set :tag "Header presentation" :greedy t
                     (const :tag "Use grayscale for date headers" grayscale)
                     (const :tag "Do not differentiate weekdays from weekends" workaholic)
                     (const :tag "Make today bold" bold-today)
                     (const :tag "Make all dates bold" bold-all)
                     (const :tag "Increase font size (`modus-themes-scale-1')" scale-heading)
                     (const :tag "Make today underlined; remove the background" underline-today)))
          (cons :tag "Event entry" :greedy t
                (const event)
                (set :tag "Text presentation" :greedy t
                     (const :tag "Use smaller font size (`modus-themes-scale-small')" scale-small)
                     (const :tag "Apply an accent color" accented)
                     (const :tag "Italic font slant (oblique forms)" italic)))
          (cons :tag "Scheduled tasks"
                (const scheduled)
                (choice (const :tag "Yellow colors to distinguish current and future tasks (default)" nil)
                        (const :tag "Uniform subtle warm color for all scheduled tasks" uniform)
                        (const :tag "Rainbow-colored scheduled tasks" rainbow)))
          (cons :tag "Habit graph"
                (const habit)
                (choice (const :tag "Follow the original design of `org-habit' (default)" nil)
                        (const :tag "Do not distinguish between present and future variants" simplified)
                        (const :tag "Use only red, yellow, green" traffic-light)
                        (const :tag "Use only red, yellow, blue" traffic-light-deuteranopia))))
  :set #'modus-themes--set-option
  :initialize #'custom-initialize-default
  :link '(info-link "(modus-themes) Org agenda"))

(defcustom modus-themes-scale-headings nil
  "Use font scaling for headings.

For regular headings the scale is controlled by the variables
`modus-themes-scale-1' (smallest increase) and its variants all
the way up to `modus-themes-scale-4' (largest increase).

While `modus-themes-scale-title' is reserved for special headings
that nominally are the largest on the scale (though that is not a
requirement).

A special heading is, in this context, one that does not fit into
the syntax for heading levels that apply to the given mode.  For
example, Org's #+title keyword lies outside the normal eight
levels of headings.  Whereas, say, Markdown does not have such a
special heading."
  :group 'modus-themes
  :package-version '(modus-themes . "1.2.0")
  :version "28.1"
  :type 'boolean
  :set #'modus-themes--set-option
  :initialize #'custom-initialize-default
  :link '(info-link "(modus-themes) Scaled headings"))

(defcustom modus-themes-scale-1 1.05
  "Font size that is slightly larger than the base value.

This size is used for level 4 headings, such as in Org and
Markdown files.

The default value is a floating point that is interpreted as a
multiple of the base font size.  It is recommended to use such a
value.

However, the variable also accepts an integer, understood as an
absolute height that is 1/10 of the typeface's point size (e.g. a
value of 140 is the same as setting the font at 14 point size).
This will ignore the base font size and, thus, will not scale in
accordance with it in cases where it changes, such as while using
`text-scale-adjust'."
  :group 'modus-themes
  :package-version '(modus-themes . "1.2.0")
  :version "28.1"
  :type 'number
  :set #'modus-themes--set-option
  :initialize #'custom-initialize-default
  :link '(info-link "(modus-themes) Scaled heading sizes"))

(defcustom modus-themes-scale-2 1.1
  "Font size slightly larger than `modus-themes-scale-1'.

This size is used for level 3 headings, such as in Org and
Markdown files.

The default value is a floating point that is interpreted as a
multiple of the base font size.  It is recommended to use such a
value.

However, the variable also accepts an integer, understood as an
absolute height that is 1/10 of the typeface's point size (e.g. a
value of 140 is the same as setting the font at 14 point size).
This will ignore the base font size and, thus, will not scale in
accordance with it in cases where it changes, such as while using
`text-scale-adjust'."
  :group 'modus-themes
  :package-version '(modus-themes . "1.2.0")
  :version "28.1"
  :type 'number
  :set #'modus-themes--set-option
  :initialize #'custom-initialize-default
  :link '(info-link "(modus-themes) Scaled heading sizes"))

(defcustom modus-themes-scale-3 1.15
  "Font size slightly larger than `modus-themes-scale-2'.

This size is used for level 2 headings, such as in Org and
Markdown files.

The default value is a floating point that is interpreted as a
multiple of the base font size.  It is recommended to use such a
value.

However, the variable also accepts an integer, understood as an
absolute height that is 1/10 of the typeface's point size (e.g. a
value of 140 is the same as setting the font at 14 point size).
This will ignore the base font size and, thus, will not scale in
accordance with it in cases where it changes, such as while using
`text-scale-adjust'."
  :group 'modus-themes
  :package-version '(modus-themes . "1.2.0")
  :version "28.1"
  :type 'number
  :set #'modus-themes--set-option
  :initialize #'custom-initialize-default
  :link '(info-link "(modus-themes) Scaled heading sizes"))

(defcustom modus-themes-scale-4 1.2
  "Font size slightly larger than `modus-themes-scale-3'.

This size is used for level 1 headings, such as in Org and
Markdown files.

The default value is a floating point that is interpreted as a
multiple of the base font size.  It is recommended to use such a
value.

However, the variable also accepts an integer, understood as an
absolute height that is 1/10 of the typeface's point size (e.g. a
value of 140 is the same as setting the font at 14 point size).
This will ignore the base font size and, thus, will not scale in
accordance with it in cases where it changes, such as while using
`text-scale-adjust'."
  :group 'modus-themes
  :package-version '(modus-themes . "1.2.0")
  :version "28.1"
  :type 'number
  :set #'modus-themes--set-option
  :initialize #'custom-initialize-default
  :link '(info-link "(modus-themes) Scaled heading sizes"))

(define-obsolete-variable-alias 'modus-themes-scale-5 'modus-themes-scale-title "1.5.0")

(defcustom modus-themes-scale-title 1.3
  "Font size slightly larger than `modus-themes-scale-4'.

This size is only used for 'special' top level headings, such as
Org's file title heading, denoted by the #+title key word, and
the Org agenda structure headers (see `modus-themes-org-agenda').

The default value is a floating point that is interpreted as a
multiple of the base font size.  It is recommended to use such a
value.

However, the variable also accepts an integer, understood as an
absolute height that is 1/10 of the typeface's point size (e.g. a
value of 140 is the same as setting the font at 14 point size).
This will ignore the base font size and, thus, will not scale in
accordance with it in cases where it changes, such as while using
`text-scale-adjust'."
  :group 'modus-themes
  :package-version '(modus-themes . "1.5.0")
  :version "28.1"
  :type 'number
  :set #'modus-themes--set-option
  :initialize #'custom-initialize-default
  :link '(info-link "(modus-themes) Scaled heading sizes"))

(defcustom modus-themes-scale-small 0.9
  "Font size smaller than the default value.

This size is only used in special contexts where users are
presented with the option to have smaller text on display (see
`modus-themes-org-agenda').

The default value is a floating point that is interpreted as a
multiple of the base font size.  It is recommended to use such a
value.

However, the variable also accepts an integer, understood as an
absolute height that is 1/10 of the typeface's point size (e.g. a
value of 140 is the same as setting the font at 14 point size).
This will ignore the base font size and, thus, will not scale in
accordance with it in cases where it changes, such as while using
`text-scale-adjust'."
  :group 'modus-themes
  :package-version '(modus-themes . "1.6.0")
  :version "28.1"
  :type 'number
  :set #'modus-themes--set-option
  :initialize #'custom-initialize-default
  :link '(info-link "(modus-themes) Scaled heading sizes"))

(defcustom modus-themes-fringes nil
  "Define the visibility of fringes.

Nil means the fringes have no background color.  Option `subtle'
will apply a grayscale value that is visible yet close to the
main buffer background color.  Option `intense' will use a more
pronounced grayscale value."
  :group 'modus-themes
  :package-version '(modus-themes . "1.0.0")
  :version "28.1"
  :type '(choice
          (const :format "[%v] %t\n" :tag "No visible fringes (default)" nil)
          (const :format "[%v] %t\n" :tag "Subtle grayscale background" subtle)
          (const :format "[%v] %t\n" :tag "Intense grayscale background" intense))
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
`background' and/or `text-only' are set.  If `intense' is set on
its own, then it implies `text-only'.

To disable fringe indicators for Flymake or Flycheck, refer to
variables `flymake-fringe-indicator-position' and
`flycheck-indication-mode', respectively.

Combinations of any of those properties can be expressed in a
list, as in those examples:

    (background)
    (straight-underline intense)
    (background text-also straight-underline)

The order in which the properties are set is not significant.

In user configuration files the form may look like this:

    (setq modus-themes-lang-checkers '(text-also background))

NOTE: The placement of the straight underline, though not the
wave style, is controlled by the built-in variables
`underline-minimum-offset', `x-underline-at-descent-line',
`x-use-underline-position-properties'."
  :group 'modus-themes
  :package-version '(modus-themes . "1.5.0")
  :version "28.1"
  :type '(set :tag "Properties" :greedy t
              (const :tag "Straight underline" straight-underline)
              (const :tag "Colorise text as well" text-also)
              (const :tag "Increase color intensity" intense)
              (const :tag "With background" background))
  :set #'modus-themes--set-option
  :initialize #'custom-initialize-default
  :link '(info-link "(modus-themes) Language checkers"))

(defcustom modus-themes-org-blocks nil
  "Use a subtle gray or color-coded background for Org blocks.

Nil (the default) means that the block has no distinct background
of its own and uses the one that applies to the rest of the
buffer.

Option `gray-background' applies a subtle gray background to the
block's contents.  It also affects the begin and end lines of the
block: their background extends to the edge of the window for
Emacs version >= 27 where the ':extend' keyword is recognized by
`set-face-attribute' (this is contingent on the variable
`org-fontify-whole-block-delimiter-line').

Option `tinted-background' uses a slightly colored background for
the contents of the block.  The exact color will depend on the
programming language and is controlled by the variable
`org-src-block-faces' (refer to the theme's source code for the
current association list).  For this to take effect, the Org
buffer needs to be restarted with `org-mode-restart'.

Code blocks use their major mode's colors only when the variable
`org-src-fontify-natively' is non-nil.  While quote/verse blocks
require setting `org-fontify-quote-and-verse-blocks' to a non-nil
value.

Older versions of the themes provided options `grayscale' (or
`greyscale') and `rainbow'.  Those will continue to work as they
are aliases for `gray-background' and `tinted-background',
respectively."
  :group 'modus-themes
  :package-version '(modus-themes . "1.4.0")
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

(defcustom modus-themes-org-habit nil
  "Deprecated in version 1.5.0 favor of `modus-themes-org-agenda'.

Control the presentation of the `org-habit' graph.

The default is meant to conform with the original aesthetic of
`org-habit'.  It employs all four color codes that correspond to
the org-habit states---clear, ready, alert, and overdue---while
distinguishing between their present and future variants.  This
results in a total of eight colors in use: red, yellow, green,
blue, in tinted and shaded versions.  They cover the full set of
information provided by the `org-habit' consistency graph.

Option `simplified' is like the default except that it removes
the dichotomy between current and future variants by applying
uniform color-coded values.  It applies a total of four colors:
red, yellow, green, blue.  They produce a simplified consistency
graph that is more legible (or less \"busy\") than the default.
The intent is to shift focus towards the distinction between the
four states of a habit task, rather than each state's
present/future outlook.

Option `traffic-light' further reduces the available colors to
red, yellow, and green.  As in `simplified', present and future
variants appear uniformly, but differently from it, the 'clear'
state is rendered in a green hue, instead of the original blue.
This is meant to capture the use-case where a habit task being
\"too early\" is less important than it being \"too late\".  The
difference between ready and clear states is attenuated by
painting both of them using shades of green.  This option thus
highlights the alert and overdue states."
  :group 'modus-themes
  :package-version '(modus-themes . "1.1.0")
  :version "28.1"
  :type '(choice
          (const :format "[%v] %t\n" :tag "Respect the original design of org-habit (default)" nil)
          (const :format "[%v] %t\n" :tag "Like the default, but do not distinguish between present and future variants" simplified)
          (const :format "[%v] %t\n" :tag "Like `simplified', but only use red, yellow, green" traffic-light))
  :set #'modus-themes--set-option
  :initialize #'custom-initialize-default
  :link '(info-link "(modus-themes) Org agenda habits"))

(make-obsolete 'modus-themes-org-habit 'modus-themes-org-agenda "1.5.0")

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
library of the same name (hereinafter referred to as 'Moody').
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

The `padded' property increases the apparent height of the mode
line.  This is done by applying box effects and combining them
with an underline and overline.  To ensure that the underline is
placed at the bottom, set `x-underline-at-descent-line' to
non-nil.  The `padded' property has no effect when the `moody'
property is also used, because Moody already applies its own
padding.

Combinations of any of those properties are expressed as a list,
like in these examples:

    (accented)
    (borderless 3d)
    (moody accented borderless)

The order in which the properties are set is not significant.

In user configuration files the form may look like this:

    (setq modus-themes-mode-line '(borderless accented))

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
instead of a box style, it is advised to set
`x-underline-at-descent-line' to a non-nil value."
  :group 'modus-themes
  :package-version '(modus-themes . "1.6.0")
  :version "28.1"
  :type '(set :tag "Properties" :greedy t
              (choice :tag "Overall style"
                      (const :tag "Rectangular Border" nil)
                      (const :tag "3d borders" 3d)
                      (const :tag "No box effects (Moody-compatible)" moody))
              (const :tag "Colored background" accented)
              (const :tag "Without border color" borderless)
              (const :tag "With extra padding" padded))
  :set #'modus-themes--set-option
  :initialize #'custom-initialize-default
  :link '(info-link "(modus-themes) Mode line"))

(defcustom modus-themes-diffs nil
  "Adjust the overall style of diffs.

The default (nil) uses fairly intense color combinations for
diffs, by applying prominently colored backgrounds, with
appropriate foregrounds.

Option `desaturated' follows the same principles as with the
default (nil), though it tones down all relevant colors.

Option `bg-only' applies a background but does not override the
text's foreground.  This makes it suitable for a non-nil value
passed to `diff-font-lock-syntax' (note: Magit does not support
syntax highlighting in diffs---last checked on 2021-04-21).

Option `deuteranopia' is like the default (nil) in terms of using
prominently colored backgrounds, except that it also accounts for
red-green color defficiency by replacing all instances of green
with colors on the blue side of the spectrum.  Other stylistic
changes are made in the interest of optimizing for such a
use-case.

Option `fg-only-deuteranopia' removes all colored backgrounds,
except from word-wise or refined changes.  Instead, it only uses
color-coded foreground values to differentiate between added,
removed, and changed lines.  If a background is necessary to
denote context, a subtle grayscale value is applied.  The color
used for added lines is a variant of blue to account for
red-green color defficiency but also because green text alone is
hard to discern in the diff's context (hard for our accessibility
purposes).  The `fg-only' option that existed in older versions
of the themes is now an alias of `fg-only-deuteranopia', in the
interest of backward compatibility."
  :group 'modus-themes
  :package-version '(modus-themes . "1.4.0")
  :version "28.1"
  :type '(choice
          (const :format "[%v] %t\n" :tag "Intensely colored backgrounds (default)" nil)
          (const :format "[%v] %t\n" :tag "Slightly accented backgrounds with tinted text" desaturated)
          (const :format "[%v] %t\n" :tag "Apply color-coded backgrounds; keep syntax colors intact" bg-only)
          (const :format "[%v] %t\n" :tag "Like the default (nil), though optimized for red-green color defficiency" deuteranopia)
          (const :format "[%v] %t\n" :tag "No backgrounds, except for refined diffs" fg-only-deuteranopia)
          (const :format "[%v] %t\n" :tag "Alias of `fg-only-deuteranopia' for backward compatibility" fg-only))
  :set #'modus-themes--set-option
  :initialize #'custom-initialize-default
  :link '(info-link "(modus-themes) Diffs"))

(defcustom modus-themes-completions nil
  "Control the style of the completion framework's interface.

This is a special option that has different effects depending on
the completion UI.  The interfaces can be grouped in two
categories, based on their default aesthetics: (i) those that
only or mostly use foreground colors for their interaction model,
and (ii) those that combine background and foreground values for
some of their metaphors.  The former category encompasses
Icomplete, Ido, Selectrum, Vertico, as well as pattern matching
styles like Orderless and Flx.  The latter covers Helm, Ivy, and
Sallet.

A value of nil (the default) will simply respect the metaphors of
each completion framework.

Option `moderate' applies a combination of background and
foreground that is fairly subtle.  For Icomplete and friends this
constitutes a departure from their default aesthetics, however
the difference is small.  While Helm, Ivy et al appear slightly
different than their original looks, as they are toned down a
bit.

Option `opinionated' uses color combinations that refashion the
completion UI.  For the Icomplete camp this means that intense
background and foreground combinations are used: in effect their
looks emulate those of Helm, Ivy and company in their original
style.  Whereas the other group of packages will revert to an
even more nuanced aesthetic with some additional changes to the
choice of hues.

To appreciate the scope of this customization option, you should
spend some time with every one of the nil (default), `moderate',
and `opinionated' possibilities."
  :group 'modus-themes
  :package-version '(modus-themes . "1.0.0")
  :version "28.1"
  :type '(choice
          (const :format "[%v] %t\n" :tag "Respect the framework's established aesthetic (default)" nil)
          (const :format "[%v] %t\n" :tag "Subtle backgrounds for various elements" moderate)
          (const :format "[%v] %t\n" :tag "Radical alternative to the framework's looks" opinionated))
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

    (setq modus-themes-prompts '(background gray))"
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

(defcustom modus-themes-intense-hl-line nil
  "Use a more prominent background for command `hl-line-mode'."
  :group 'modus-themes
  :package-version '(modus-themes . "1.0.0")
  :version "28.1"
  :type 'boolean
  :set #'modus-themes--set-option
  :initialize #'custom-initialize-default
  :link '(info-link "(modus-themes) Line highlighting"))

(make-obsolete 'modus-themes-intense-hl-line 'modus-themes-hl-line "1.3.0")

(defcustom modus-themes-hl-line nil
  "Control the current line highlight of HL-line mode.

The value is a list of properties, each designated by a symbol.
The default (a nil value or an empty list) is a subtle gray
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

    (setq modus-themes-hl-line '(underline accented))

Set `x-underline-at-descent-line' to a non-nil value for better
results with underlines."
  :group 'modus-themes
  :package-version '(modus-themes . "1.5.0")
  :version "28.1"
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

    (setq modus-themes-paren-match '(bold intense))"
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

    (setq modus-themes-syntax '(faint alt-syntax))

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

    (setq modus-themes-links '(neutral-underline background))

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

    (setq modus-themes-region '(bg-only no-extend))"
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

(defcustom modus-themes-success-deuteranopia nil
  "Color-code 'success' or 'done' as blue instead of green.

This is to account for red-green color deficiency.

The present customization option should apply to all contexts where
there can be a color-coded distinction between success and failure,
to-do and done, and so on.

Diffs, which have a red/green dichotomy by default, can also be
configured to conform with deuteranopia: `modus-themes-diffs'."
  :group 'modus-themes
  :package-version '(modus-themes . "1.4.0")
  :version "28.1"
  :type 'boolean
  :set #'modus-themes--set-option
  :initialize #'custom-initialize-default
  :link '(info-link "(modus-themes) Success' color-code"))

(defcustom modus-themes-mail-citations nil
  "Control the color of citations in messages or email clients.

Nil (the default) means to use a variety of contrasting hues to
denote depth in nested citations.  Colors are fairly easy to tell
apart.

Option `faint' maintains a color-based distinction between
citation levels but the colors it applies have very subtle
differences between them.

Option `monochrome' turns all citations that would otherwise be
colored into a uniform shade of shade of gray."
  :group 'modus-themes
  :package-version '(modus-themes . "1.4.0")
  :version "28.1"
  :type '(choice
          (const :format "[%v] %t\n" :tag "Colorful mail citations with contrasting hues (default)" nil)
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



;;; Internal functions

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
  "Return current theme."
  (car custom-enabled-themes))

;; Helper functions that are meant to ease the implementation of the
;; above customization variables.
(defun modus-themes--bold-weight ()
  "Conditional use of a heavier text weight."
  (when modus-themes-bold-constructs
    (list :inherit 'bold)))

(defun modus-themes--slant ()
  "Conditional use of italics for slant attribute."
  (if modus-themes-italic-constructs
      (list 'italic)
    (list 'normal)))

(defun modus-themes--fixed-pitch ()
  "Conditional application of `fixed-pitch' inheritance."
  (unless modus-themes-no-mixed-fonts
    (list :inherit 'fixed-pitch)))

(defun modus-themes--variable-pitch ()
  "Conditional use of `variable-pitch' in headings."
  (when modus-themes-variable-pitch-headings
    (list :inherit 'variable-pitch)))

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

(defun modus-themes--lang-check (underline subtlefg intensefg intensefg-alt subtlebg intensebg)
  "Conditional use of foreground colors for language checkers.
UNDERLINE is a color-code value for the affected text's underline
property.  SUBTLEFG and INTENSEFG follow the same color-coding
pattern and represent a value that is faint or vibrant
respectively.  INTENSEFG-ALT is used when the intensity is high.
SUBTLEBG and INTENSEBG are color-coded background colors that
differ in overall intensity."
  (let ((modus-themes-lang-checkers
         (if (listp modus-themes-lang-checkers)
             modus-themes-lang-checkers
           (pcase modus-themes-lang-checkers
             ('colored-background '(background intense))
             ('intense-foreground '(intense))
             ('intense-foreground-straight-underline '(intense straight-underline))
             ('subtle-foreground '(text-also))
             ('subtle-foreground-straight-underline '(text-also straight-underline))
             ('straight-underline '(straight-underline))))))
    (list :underline
          (list :color
                underline
                :style
                (if (memq 'straight-underline modus-themes-lang-checkers)
                    'line 'wave))
          :background
          (cond
           ((and (memq 'background modus-themes-lang-checkers)
                 (memq 'intense modus-themes-lang-checkers))
            intensebg)
           ((memq 'background modus-themes-lang-checkers)
            subtlebg))
          :foreground
          (cond
           ((and (memq 'background modus-themes-lang-checkers)
                 (memq 'intense modus-themes-lang-checkers))
            intensefg-alt)
           ((memq 'intense modus-themes-lang-checkers)
            intensefg)
           ((memq 'text-also modus-themes-lang-checkers)
            subtlefg)))))

(defun modus-themes--prompt (mainfg intensefg grayfg subtlebg intensebg intensebg-fg subtlebggray intensebggray)
  "Conditional use of colors for prompts.
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
  (let ((modus-themes-prompts
         (if (listp modus-themes-prompts)
             modus-themes-prompts
           ;; translation layer for legacy values
           (pcase modus-themes-prompts
             ;; `subtle' is the same as `subtle-accented', while `intense' is
             ;; equal to `intense-accented' for backward compatibility
             ('subtle '(background))
             ('subtle-accented '(background))
             ('subtle-gray '(background gray))
             ('intense '(background intense))
             ('intense-accented '(background intense))
             ('intense-gray '(background intense gray))))))
    (list :foreground
          (cond
           ((and (memq 'gray modus-themes-prompts)
                 (memq 'intense modus-themes-prompts))
            'unspecified)
           ((memq 'gray modus-themes-prompts)
            grayfg)
           ((and (memq 'background modus-themes-prompts)
                 (memq 'intense modus-themes-prompts))
            intensebg-fg)
           ((memq 'intense modus-themes-prompts)
            intensefg)
           (mainfg))
          :background
          (cond
           ((and (memq 'gray modus-themes-prompts)
                 (memq 'background modus-themes-prompts)
                 (memq 'intense modus-themes-prompts))
            intensebggray)
           ((and (memq 'gray modus-themes-prompts)
                 (memq 'background modus-themes-prompts))
            subtlebggray)
           ((and (memq 'background modus-themes-prompts)
                 (memq 'intense modus-themes-prompts))
            intensebg)
           ((memq 'background modus-themes-prompts)
            subtlebg)
           ('unspecified))
          :inherit
          (cond
           ((and (memq 'bold modus-themes-prompts)
                 (memq 'italic modus-themes-prompts))
            'bold-italic)
           ((memq 'italic modus-themes-prompts)
            'italic)
           ((memq 'bold modus-themes-prompts)
            'bold)
           ('unspecified)))))

(defun modus-themes--paren (normalbg intensebg)
  "Conditional use of intense colors for matching parentheses.
NORMALBG should be the special palette color 'bg-paren-match' or
something similar.  INTENSEBG must be easier to discern next to
other backgrounds, such as the special palette color
'bg-paren-match-intense'."
  (let ((modus-themes-paren-match
         (if (listp modus-themes-paren-match)
             modus-themes-paren-match
           ;; translation layer for legacy values
           (pcase modus-themes-paren-match
             ;; `subtle' is the same as `subtle-accented', while `intense' is
             ;; equal to `intense-accented' for backward compatibility
             ('intense-bold '(intense bold))
             ('subtle-bold '(bold))
             ('intense '(intense))))))
    (list :inherit
          (if (memq 'bold modus-themes-paren-match)
              'bold
            'unspecified)
          :background
          (if (memq 'intense modus-themes-paren-match)
              intensebg
            normalbg)
          :underline
          (if (memq 'underline modus-themes-paren-match)
              t
            nil))))

(defun modus-themes--syntax-foreground (fg faint)
  "Apply foreground value to code syntax.
FG is the default.  FAINT is typically the same color in its
desaturated version."
  (let ((modus-themes-syntax
         (if (listp modus-themes-syntax)
             modus-themes-syntax
           ;; translation layer for legacy values
           (pcase modus-themes-syntax
             ('faint '(faint))
             ('faint-yellow-comments '(faint yellow-comments))
             ('green-strings '(green-strings))
             ('yellow-comments '(yellow-comments))
             ('yellow-comments-green-strings '(green-strings yellow-comments))
             ('alt-syntax '(alt-syntax))
             ('alt-syntax-yellow-comments '(alt-syntax yellow-comments))))))
    (list :foreground
          (cond
           ((memq 'faint modus-themes-syntax)
            faint)
           (fg)))))

(defun modus-themes--syntax-extra (fg faint alt &optional faint-alt)
  "Apply foreground value to code syntax.
FG is the default.  FAINT is typically the same color in its
desaturated version.  ALT is another hue while optional FAINT-ALT
is its subtle alternative."
  (let ((modus-themes-syntax
         (if (listp modus-themes-syntax)
             modus-themes-syntax
           ;; translation layer for legacy values
           (pcase modus-themes-syntax
             ('faint '(faint))
             ('faint-yellow-comments '(faint yellow-comments))
             ('green-strings '(green-strings))
             ('yellow-comments '(yellow-comments))
             ('yellow-comments-green-strings '(green-strings yellow-comments))
             ('alt-syntax '(alt-syntax))
             ('alt-syntax-yellow-comments '(alt-syntax yellow-comments))))))
    (list :foreground
          (cond
           ((and (memq 'alt-syntax modus-themes-syntax)
                 (memq 'faint modus-themes-syntax))
            (or faint-alt alt))
           ((memq 'faint modus-themes-syntax)
            faint)
           ((memq 'alt-syntax modus-themes-syntax)
            alt)
           (fg)))))

(defun modus-themes--syntax-string (fg faint green alt &optional faint-green faint-alt)
  "Apply foreground value to strings in code syntax.
FG is the default.  FAINT is typically the same color in its
desaturated version.  GREEN is a color variant in that side of
the spectrum.  ALT is another hue.  Optional FAINT-GREEN is a
subtle alternative to GREEN.  Optional FAINT-ALT is a subtle
alternative to ALT."
  (let ((modus-themes-syntax
         (if (listp modus-themes-syntax)
             modus-themes-syntax
           ;; translation layer for legacy values
           (pcase modus-themes-syntax
             ('faint '(faint))
             ('faint-yellow-comments '(faint yellow-comments))
             ('green-strings '(green-strings))
             ('yellow-comments '(yellow-comments))
             ('yellow-comments-green-strings '(green-strings yellow-comments))
             ('alt-syntax '(alt-syntax))
             ('alt-syntax-yellow-comments '(alt-syntax yellow-comments))))))
    (list :foreground
          (cond
           ((and (memq 'faint modus-themes-syntax)
                 (memq 'green-strings modus-themes-syntax))
            (or faint-green green))
           ((and (memq 'alt-syntax modus-themes-syntax)
                 (memq 'faint modus-themes-syntax))
            (or faint-alt faint))
           ((memq 'faint modus-themes-syntax)
            faint)
           ((memq 'green-strings modus-themes-syntax)
            green)
           ((memq 'alt-syntax modus-themes-syntax)
            alt)
           (fg)))))

(defun modus-themes--syntax-comment (fg yellow &optional faint-yellow faint)
  "Apply foreground value to strings in code syntax.
FG is the default.  YELLOW is a color variant of that name while
optional FAINT-YELLOW is its subtle variant.  Optional FAINT is
an alternative to the default value."
  (let ((modus-themes-syntax
         (if (listp modus-themes-syntax)
             modus-themes-syntax
           ;; translation layer for legacy values
           (pcase modus-themes-syntax
             ('faint '(faint))
             ('faint-yellow-comments '(faint yellow-comments))
             ('green-strings '(green-strings))
             ('yellow-comments '(yellow-comments))
             ('yellow-comments-green-strings '(green-strings yellow-comments))
             ('alt-syntax '(alt-syntax))
             ('alt-syntax-yellow-comments '(alt-syntax yellow-comments))))))
    (list :foreground
          (cond
           ((and (memq 'faint modus-themes-syntax)
                 (memq 'yellow-comments modus-themes-syntax))
            (or faint-yellow yellow))
           ((and (memq 'alt-syntax modus-themes-syntax)
                 (memq 'yellow-comments modus-themes-syntax)
                 (not (memq 'green-strings modus-themes-syntax)))
            (or faint-yellow yellow))
           ((memq 'yellow-comments modus-themes-syntax)
            yellow)
           ((memq 'faint modus-themes-syntax)
            (or faint fg))
           (fg)))))

(defun modus-themes--key-cdr (key alist)
  "Get cdr of KEY in ALIST."
  (cdr (assoc key alist)))

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
         (modus-themes-headings
          (if (listp style)
              style
            ;; translation layer for legacy values
            (pcase style
              ('highlight '(background))
              ('highlight-no-bold '(background no-bold))
              ('line '(overline))
              ('line-no-bold '(no-bold overline))
              ('no-bold '(no-bold))
              ('no-color '(monochrome))
              ('no-color-no-bold '(no-bold monochrome))
              ('rainbow '(rainbow))
              ('rainbow-highlight '(rainbow background))
              ('rainbow-highlight-no-bold '(no-bold rainbow background))
              ('rainbow-line '(rainbow overline))
              ('rainbow-no-bold '(no-bold rainbow))
              ('rainbow-line-no-bold '(rainbow overline no-bold))
              ('rainbow-section '(rainbow overline background))
              ('rainbow-section-no-bold '(no-bold rainbow background overline))
              ('section '(background overline))
              ('section-no-bold '(background overline no-bold)))))
         (var (if modus-themes-variable-pitch-headings
                  'variable-pitch
                'unspecified))
         (varbold (if var
                      (append (list 'bold) (list var))
                    'bold)))
    (list :inherit
          (cond
           ((memq 'no-bold modus-themes-headings)
            var)
           (varbold))
          :background
          (cond
           ((and (memq 'monochrome modus-themes-headings)
                 (memq 'background modus-themes-headings))
            bg-gray)
           ((memq 'background modus-themes-headings)
            bg)
           ('unspecified))
          :foreground
          (cond
           ((memq 'monochrome modus-themes-headings)
            'unspecified)
           ((memq 'rainbow modus-themes-headings)
            fg-alt)
           (fg))
          :overline
          (if (memq 'overline modus-themes-headings)
              border
            'unspecified))))

(defun modus-themes--agenda-structure (fg)
  "Control the style of the Org agenda structure.
FG is the foreground color to use."
  (let* ((properties (modus-themes--key-cdr 'header-block modus-themes-org-agenda))
         (inherit (cond ((memq 'variable-pitch properties)
                         (list 'bold 'variable-pitch))
                        ('bold)))
         (height (cond ((memq 'no-scale properties)
                        1.0)
                       ((memq 'scale-title properties)
                        modus-themes-scale-title)
                       (1.15))))
    (list :inherit inherit
          :height height
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
          (unless (memq 'underline-today properties)
            bg)
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
          (if (memq 'scale-heading properties)
              modus-themes-scale-1
            'unspecified)
          :underline
          (if (and ul (memq 'underline-today properties))
              t
            'unspecified))))

(defun modus-themes--agenda-event (fg)
  "Control the style of the Org agenda events.
FG is the accent color to use."
  (let ((properties (modus-themes--key-cdr 'event modus-themes-org-agenda)))
    (list :height
          (if (memq 'scale-small properties)
              modus-themes-scale-small
            'unspecified)
          :foreground
          (if (memq 'accented properties)
              fg
            'unspecified)
          :inherit
          (cond
           ((and (memq 'accented properties)
                 (memq 'italic properties))
            'italic)
           ((memq 'italic properties)
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

(defun modus-themes--agenda-habit (default traffic simple &optional traffic-deuteran)
  "Specify background values for `modus-themes-org-agenda' habits.
DEFAULT is the original foregrounc color.  TRAFFIC is to be used
when the 'traffic-light' style is applied, while SIMPLE
corresponds to the 'simplified style'.  Optional TRAFFIC-DEUTERAN
is an alternative to TRAFFIC, meant for deuteranopia."
  (pcase (modus-themes--key-cdr 'habit modus-themes-org-agenda)
    ('traffic-light (list :background traffic))
    ('traffic-light-deuteranopia (list :background (or traffic-deuteran traffic)))
    ('simplified (list :background simple))
    (_ (list :background default))))

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
    ('rainbow (list :background bgaccent :foreground fgaccent))
    (_ (list :background bg :foreground fg))))

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
  (let ((modus-themes-mode-line
         (if (listp modus-themes-mode-line)
             modus-themes-mode-line
           ;; translation layer for legacy values
           (alist-get modus-themes-mode-line
                      '((3d . (3d))
                        (moody . (moody))
                        (borderless . (borderless))
                        (borderless-3d . (borderless 3d))
                        (borderless-moody . (borderless moody))
                        (accented . (accented))
                        (accented-3d . (accented 3d))
                        (accented-moody . (accented moody))
                        (borderless-accented . (borderless accented))
                        (borderless-accented-3d . (borderless accented 3d))
                        (borderless-accented-moody . (borderless accented moody)))))))
    (let ((base (cond ((memq 'accented modus-themes-mode-line)
                       (cons fg-accent bg-accent))
                      ((and (or (memq 'moody modus-themes-mode-line)
                                (memq '3d modus-themes-mode-line))
                            (not (memq 'borderless modus-themes-mode-line)))
                       (cons fg-alt bg-alt))
                      ((cons fg bg))))
          (box (cond ((memq 'moody modus-themes-mode-line)
                      nil)
                     ((and (memq '3d modus-themes-mode-line)
                           (memq 'padded modus-themes-mode-line))
                      (list :line-width 4
                            :color
                            (cond ((and (memq 'accented modus-themes-mode-line)
                                        (memq 'borderless modus-themes-mode-line))
                                   bg-accent)
                                  ((or (memq 'accented modus-themes-mode-line)
                                       (memq 'borderless modus-themes-mode-line))
                                   bg)
                                  (bg-alt))
                            :style (when alt-style 'released-button)))
                     ((and (memq 'accented modus-themes-mode-line)
                           (memq 'padded modus-themes-mode-line))
                      (list :line-width 6 :color bg-accent))
                     ((memq 'padded modus-themes-mode-line)
                      (list :line-width 6 :color bg))
                     ((memq '3d modus-themes-mode-line)
                      (list :line-width 1
                            :color
                            (cond ((and (memq 'accented modus-themes-mode-line)
                                        (memq 'borderless modus-themes-mode-line))
                                   bg-accent)
                                  ((memq 'borderless modus-themes-mode-line) bg)
                                  (border-3d))
                            :style (when alt-style 'released-button)))
                     ((memq 'borderless modus-themes-mode-line)
                      bg)
                     ((memq 'padded modus-themes-mode-line)
                      (list :line-width 6 :color bg))
                     (border)))
          (line (cond ((not (or (memq 'moody modus-themes-mode-line)
                                (memq 'padded modus-themes-mode-line)))
                       nil)
                      ((and (memq 'borderless modus-themes-mode-line)
                            (memq 'accented modus-themes-mode-line))
                       bg-accent)
                      ((memq 'borderless modus-themes-mode-line)
                       bg)
                      (border))))
      (list :foreground (car base)
            :background (cdr base)
            :box box
            :overline line
            :underline line
            :distant-foreground
            (when (memq 'moody modus-themes-mode-line)
              fg-distant)))))

(defun modus-themes--diff
    (fg-only-bg fg-only-fg mainbg mainfg altbg altfg &optional deuteranbg deuteranfg  bg-only-fg)
  "Color combinations for `modus-themes-diffs'.

FG-ONLY-BG should be similar or the same as the main background.
FG-ONLY-FG should be a saturated accent value that can be
combined with the former.

MAINBG must be one of the dedicated backgrounds for diffs while
MAINFG must be the same for the foreground.

ALTBG needs to be a slightly accented background that is meant to
be combined with ALTFG.  Both must be less intense than MAINBG
and MAINFG respectively.

DEUTERANBG and DEUTERANFG must be combinations of colors that account
for red-green color defficiency (deuteranopia).

Optional BG-ONLY-FG applies ALTFG else leaves the foreground
unspecified."
  (pcase modus-themes-diffs
    ('fg-only (list :background fg-only-bg :foreground fg-only-fg))
    ('fg-only-deuteranopia (list :background fg-only-bg :foreground fg-only-fg))
    ('desaturated (list :background altbg :foreground altfg))
    ('deuteranopia (list :background (or deuteranbg mainbg) :foreground (or deuteranfg mainfg)))
    ('bg-only (list :background altbg :foreground (if bg-only-fg altfg 'unspecified)))
    (_ (list :background mainbg :foreground mainfg))))

(defun modus-themes--diff-deuteran (deuteran main)
  "Determine whether the DEUTERAN or MAIN color should be used.
This is based on whether `modus-themes-diffs' has the value
`deuteranopia'."
  (if (or (eq modus-themes-diffs 'deuteranopia)
          (eq modus-themes-diffs 'fg-only-deuteranopia)
          (eq modus-themes-diffs 'fg-only))
      (list deuteran)
    (list main)))

(defun modus-themes--success-deuteran (deuteran main)
  "Determine whether to color-code success as DEUTERAN or MAIN."
  (if modus-themes-success-deuteranopia
      (list deuteran)
    (list main)))

(defun modus-themes--standard-completions (mainfg subtlebg intensebg intensefg)
  "Combinations for `modus-themes-completions'.

MAINFG is an accented foreground value.  SUBTLEBG is an accented
background value that can be combined with MAINFG.  INTENSEBG and
INTENSEFG are accented colors that are designed to be used in
tandem.

These are intended for Icomplete, Ido, and related."
  (pcase modus-themes-completions
    ('opinionated (list :background intensebg :foreground intensefg))
    ('moderate (list :background subtlebg :foreground mainfg))
    (_ (list :foreground mainfg))))

(defun modus-themes--extra-completions (subtleface intenseface altface &optional altfg bold)
  "Combinations for `modus-themes-completions'.

SUBTLEFACE and INTENSEFACE are custom theme faces that combine a
background and foreground value.  The difference between the two
is a matter of degree.

ALTFACE is a combination of colors that represents a departure
from the UI's default aesthetics.  Optional ALTFG is meant to be
used in tandem with it.

Optional BOLD will apply a heavier weight to the text.

These are intended for Helm, Ivy, etc."
  (pcase modus-themes-completions
    ('opinionated (list :inherit (list altface bold)
                        :foreground (or altfg 'unspecified)))
    ('moderate (list :inherit (list subtleface bold)))
    (_ (list :inherit (list intenseface bold)))))

(defun modus-themes--link (fg fgfaint underline bg bgneutral)
  "Conditional application of link styles.
FG is the link's default color for its text and underline
property.  FGFAINT is a desaturated color for the text and
underline.  UNDERLINE is a gray color only for the undeline.  BG
is a background color and BGNEUTRAL is its fallback value."
  (let ((modus-themes-links
         (if (listp modus-themes-links)
             modus-themes-links
           ;; translation layer for legacy values
           (pcase modus-themes-links
             ('faint '(faint))
             ('neutral-underline '(neutral-underline))
             ('faint-neutral-underline '(neutral-underline faint))
             ('no-underline '(no-underline))
             ('underline-only '(no-color))
             ('neutral-underline-only '(no-color neutral-underline))))))
    (list :inherit
          (cond
           ((and (memq 'bold modus-themes-links)
                 (memq 'italic modus-themes-links))
            'bold-italic)
           ((memq 'italic modus-themes-links)
            'italic)
           ((memq 'bold modus-themes-links)
            'bold)
           ('unspecified))
          :background
          (cond
           ((and (memq 'no-color modus-themes-links)
                 (memq 'no-underline modus-themes-links))
            bgneutral)
           ((memq 'background modus-themes-links)
            bg)
           ('unspecified))
          :foreground
          (cond
           ((memq 'no-color modus-themes-links)
            'unspecified)
           ((memq 'faint modus-themes-links)
            fgfaint)
           (fg))
          :underline
          (cond
           ((memq 'no-underline modus-themes-links)
            'unspecified)
           ((memq 'neutral-underline modus-themes-links)
            underline)
           (t)))))

(defun modus-themes--link-color (fg fgfaint &optional neutralfg)
  "Extends `modus-themes--link'.
FG is the main accented foreground.  FGFAINT is also accented,
yet desaturated.  Optional NEUTRALFG is a gray value."
  (let ((modus-themes-links
         (if (listp modus-themes-links)
             modus-themes-links
           ;; translation layer for legacy values
           (pcase modus-themes-links
             ('faint '(faint))
             ('neutral-underline '(neutral-underline))
             ('faint-neutral-underline '(neutral-underline faint))
             ('no-underline '(no-underline))
             ('underline-only '(no-color))
             ('neutral-underline-only '(no-color neutral-underline))))))
    (list :foreground
          (cond
           ((memq 'no-color modus-themes-links)
            (or neutralfg 'unspecified))
           ((memq 'faint modus-themes-links)
            fgfaint)
           (fg))
          :underline
          (cond
           ((memq 'no-underline modus-themes-links)
            'unspecified)
           ((memq 'neutral-underline modus-themes-links)
            (or neutralfg 'unspecified))
           (t)))))

(defun modus-themes--scale (amount)
  "Scale heading by AMOUNT.
AMOUNT is a customization option."
  (when modus-themes-scale-headings
    (list :height amount)))

(defun modus-themes--region (bg fg bgsubtle bgaccent bgaccentsubtle)
  "Apply `modus-themes-region' styles.

BG and FG are the main values that are used by default.  BGSUBTLE
is a subtle background value that can be combined with all colors
used to fontify text and code syntax.  BGACCENT is a colored
background that combines well with FG.  BGACCENTSUBTLE can be
combined with all colors used to fontify text."
  (let ((modus-themes-region
         (if (listp modus-themes-region)
             modus-themes-region
           ;; translation layer for legacy values
           (pcase modus-themes-region
             ('bg-only '(bg-only))
             ('bg-only-no-extend '(bg-only no-extend))
             ('accent '(accented))
             ('accent-no-extend '(accented no-extend))
             ('no-extend '(no-extend))))))
    (list :background
          (cond
           ((and (memq 'accented modus-themes-region)
                 (memq 'bg-only modus-themes-region))
            bgaccentsubtle)
           ((memq 'accented modus-themes-region)
            bgaccent)
           ((memq 'bg-only modus-themes-region)
            bgsubtle)
           (bg))
          :foreground
          (cond
           ((and (memq 'accented modus-themes-region)
                 (memq 'bg-only modus-themes-region))
            'unspecified)
           ((memq 'bg-only modus-themes-region)
            'unspecified)
           (fg))
          :extend
          (cond
           ((memq 'no-extend modus-themes-region)
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
  (let ((modus-themes-hl-line
         (if (listp modus-themes-hl-line)
             modus-themes-hl-line
           ;; translation layer for legacy values
           (pcase modus-themes-hl-line
             ('intense-background '(intense))
             ('accented-background '(accented))
             ('underline-neutral '(underline))
             ('underline-accented '(underline accented))
             ('underline-only-neutral '(underline)) ; only underline styles have been removed
             ('underline-only-accented '(underline accented))))))
    (list :background
          (cond
           ((and (memq 'intense modus-themes-hl-line)
                 (memq 'accented modus-themes-hl-line))
            bgaccent)
           ((memq 'accented modus-themes-hl-line)
            bgaccentsubtle)
           ((memq 'intense modus-themes-hl-line)
            bgintense)
           (bgdefault))
          :underline
          (cond
           ((and (memq 'intense modus-themes-hl-line)
                 (memq 'accented modus-themes-hl-line)
                 (memq 'underline modus-themes-hl-line))
            lineaccentintense)
           ((and (memq 'accented modus-themes-hl-line)
                 (memq 'underline modus-themes-hl-line))
            lineaccent)
           ((and (memq 'intense modus-themes-hl-line)
                 (memq 'underline modus-themes-hl-line))
            lineneutralintense)
           ((or (memq 'no-background modus-themes-hl-line)
                (memq 'underline modus-themes-hl-line))
            lineneutral)
           ('unspecified)))))

(defun modus-themes--mail-cite (mainfg subtlefg)
  "Combinations for `modus-themes-mail-citations'.

MAINFG is an accented foreground value.  SUBTLEFG is its
desaturated counterpart."
  (pcase modus-themes-mail-citations
    ('monochrome (list :inherit 'shadow))
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



;;;; Utilities for DIY users

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

;; The reason we use `load-theme' instead of `enable-theme' is that the
;; former does a kind of "reset" on the face specs.  So it plays nicely
;; with `custom-set-faces', as well as defcustom user customizations,
;; including the likes of `modus-themes-operandi-color-overrides'.
;;
;; Tests show that `enable-theme' does not re-read those variables, so
;; it might appear to the unsuspecting user that the themes are somehow
;; broken.
;;
;; This "reset", however, comes at the cost of being a bit slower than
;; `enable-theme'.  User who have a stable setup and seldom update their
;; variables during a given Emacs session, are better off using
;; something like this:
;;
;; (defun modus-themes-toggle-enabled ()
;;   "Toggle between `modus-operandi' and `modus-vivendi' themes."
;;   (interactive)
;;   (pcase (modus-themes--current-theme)
;;     ('modus-operandi (progn (enable-theme 'modus-vivendi)
;;                             (disable-theme 'modus-operandi)))
;;     ('modus-vivendi (progn (enable-theme 'modus-operandi)
;;                             (disable-theme 'modus-vivendi)))
;;     (_ (error "No Modus theme is loaded; evaluate `modus-themes-load-themes' first"))))

;;;###autoload
(defun modus-themes-load-operandi ()
  "Load `modus-operandi' and disable `modus-vivendi'.
Also run `modus-themes-after-load-theme-hook'."
  (disable-theme 'modus-vivendi)
  (load-theme 'modus-operandi t)
  (run-hooks 'modus-themes-after-load-theme-hook))

;;;###autoload
(defun modus-themes-load-vivendi ()
  "Load `modus-vivendi' and disable `modus-operandi'.
Also run `modus-themes-after-load-theme-hook'."
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
                  bg-main blue-alt-other
                  bg-diff-focus-added fg-diff-focus-added
                  green-nuanced-bg fg-diff-added
                  bg-diff-focus-added-deuteran fg-diff-focus-added-deuteran))))
    `(modus-themes-diff-changed
      ((,class ,@(modus-themes--diff
                  bg-main yellow
                  bg-diff-focus-changed fg-diff-focus-changed
                  yellow-nuanced-bg fg-diff-changed))))
    `(modus-themes-diff-removed
      ((,class ,@(modus-themes--diff
                  bg-main red
                  bg-diff-focus-removed fg-diff-focus-removed
                  red-nuanced-bg fg-diff-removed))))
    `(modus-themes-diff-refine-added
      ((,class ,@(modus-themes--diff
                  bg-diff-added-deuteran fg-diff-added-deuteran
                  bg-diff-refine-added fg-diff-refine-added
                  bg-diff-focus-added fg-diff-focus-added
                  bg-diff-refine-added-deuteran fg-diff-refine-added-deuteran))))
    `(modus-themes-diff-refine-changed
      ((,class ,@(modus-themes--diff
                  bg-diff-changed fg-diff-changed
                  bg-diff-refine-changed fg-diff-refine-changed
                  bg-diff-focus-changed fg-diff-focus-changed))))
    `(modus-themes-diff-refine-removed
      ((,class ,@(modus-themes--diff
                  bg-diff-removed fg-diff-removed
                  bg-diff-refine-removed fg-diff-refine-removed
                  bg-diff-focus-removed fg-diff-focus-removed))))
    `(modus-themes-diff-focus-added
      ((,class ,@(modus-themes--diff
                  bg-dim blue-alt-other
                  bg-diff-focus-added fg-diff-focus-added
                  bg-diff-added fg-diff-added
                  bg-diff-focus-added-deuteran fg-diff-focus-added-deuteran))))
    `(modus-themes-diff-focus-changed
      ((,class ,@(modus-themes--diff
                  bg-dim yellow
                  bg-diff-focus-changed fg-diff-focus-changed
                  bg-diff-changed fg-diff-changed))))
    `(modus-themes-diff-focus-removed
      ((,class ,@(modus-themes--diff
                  bg-dim red
                  bg-diff-focus-removed fg-diff-focus-removed
                  bg-diff-removed fg-diff-removed))))
    `(modus-themes-diff-heading
      ((,class ,@(modus-themes--diff
                  bg-alt fg-main
                  bg-diff-heading fg-diff-heading
                  cyan-nuanced-bg cyan-nuanced-fg
                  bg-header fg-main
                  t))))
;;;;; mark indicators
    ;; color combinations intended for Dired, Ibuffer, or equivalent
    `(modus-themes-pseudo-header ((,class :inherit bold :foreground ,fg-main)))
    `(modus-themes-mark-alt ((,class :inherit bold :background ,bg-mark-alt :foreground ,fg-mark-alt)))
    `(modus-themes-mark-del ((,class :inherit bold :background ,bg-mark-del :foreground ,fg-mark-del)))
    `(modus-themes-mark-sel ((,class :inherit bold :background ,bg-mark-sel :foreground ,fg-mark-sel)))
    `(modus-themes-mark-symbol ((,class :inherit bold :foreground ,blue-alt)))
;;;;; heading levels
    ;; styles for regular headings used in Org, Markdown, Info, etc.
    `(modus-themes-heading-1
      ((,class ,@(modus-themes--heading
                  1 fg-main magenta-alt-other
                  magenta-nuanced-bg bg-alt bg-region)
               ,@(modus-themes--scale modus-themes-scale-4))))
    `(modus-themes-heading-2
      ((,class ,@(modus-themes--heading
                  2 fg-special-warm magenta-alt
                  red-nuanced-bg bg-alt bg-region)
               ,@(modus-themes--scale modus-themes-scale-3))))
    `(modus-themes-heading-3
      ((,class ,@(modus-themes--heading
                  3 fg-special-cold blue
                  blue-nuanced-bg bg-alt bg-region)
               ,@(modus-themes--scale modus-themes-scale-2))))
    `(modus-themes-heading-4
      ((,class ,@(modus-themes--heading
                  4 fg-special-mild cyan
                  cyan-nuanced-bg bg-alt bg-region)
               ,@(modus-themes--scale modus-themes-scale-1))))
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
;;;;; graph-specific faces
    `(modus-themes-graph-red-0 ((,class :background ,red-graph-0-bg)))
    `(modus-themes-graph-red-1 ((,class :background ,red-graph-1-bg)))
    `(modus-themes-graph-green-0 ((,class :background ,green-graph-0-bg)))
    `(modus-themes-graph-green-1 ((,class :background ,green-graph-1-bg)))
    `(modus-themes-graph-yellow-0 ((,class :background ,yellow-graph-0-bg)))
    `(modus-themes-graph-yellow-1 ((,class :background ,yellow-graph-1-bg)))
    `(modus-themes-graph-blue-0 ((,class :background ,blue-graph-0-bg)))
    `(modus-themes-graph-blue-1 ((,class :background ,blue-graph-1-bg)))
    `(modus-themes-graph-magenta-0 ((,class :background ,magenta-graph-0-bg)))
    `(modus-themes-graph-magenta-1 ((,class :background ,magenta-graph-1-bg)))
    `(modus-themes-graph-cyan-0 ((,class :background ,cyan-graph-0-bg)))
    `(modus-themes-graph-cyan-1 ((,class :background ,cyan-graph-1-bg)))
;;;;; language checkers
    `(modus-themes-lang-error ((,class ,@(modus-themes--lang-check
                                          fg-lang-underline-error fg-lang-error
                                          red red-refine-fg red-nuanced-bg red-refine-bg))))
    `(modus-themes-lang-note ((,class ,@(modus-themes--lang-check
                                         fg-lang-underline-note fg-lang-note
                                         blue-alt blue-refine-fg blue-nuanced-bg blue-refine-bg))))
    `(modus-themes-lang-warning ((,class ,@(modus-themes--lang-check
                                            fg-lang-underline-warning fg-lang-warning
                                            yellow yellow-refine-fg yellow-nuanced-bg yellow-refine-bg))))
;;;;; other custom faces
    `(modus-themes-bold ((,class ,@(modus-themes--bold-weight))))
    `(modus-themes-hl-line ((,class ,@(modus-themes--hl-line
                                       bg-hl-line bg-hl-line-intense
                                       bg-hl-line-intense-accent blue-nuanced-bg
                                       bg-region blue-intense-bg
                                       fg-alt cyan-intense)
                                    :extend t)))
    `(modus-themes-key-binding ((,class ,@(if (facep 'help-key-binding) ; check emacs28 face
                                              (list :inherit 'help-key-binding)
                                            (list :inherit 'bold :foreground blue-alt-other)))))
    `(modus-themes-prompt ((,class ,@(modus-themes--prompt
                                      cyan-alt-other blue-alt-other fg-alt
                                      cyan-nuanced-bg blue-refine-bg fg-main
                                      bg-alt bg-active))))
    `(modus-themes-reset-hard ((,class :inherit (fixed-pitch modus-themes-reset-soft))))
    `(modus-themes-reset-soft ((,class :background ,bg-main :foreground ,fg-main
                                       :weight normal :slant normal :strike-through nil
                                       :box nil :underline nil :overline nil :extend nil)))
    `(modus-themes-search-success ((,class :inherit ,@(modus-themes--success-deuteran
                                                       'modus-themes-intense-blue
                                                       'modus-themes-intense-green))))
    `(modus-themes-search-success-lazy ((,class :inherit ,@(modus-themes--success-deuteran
                                                            'modus-themes-special-mild
                                                            'modus-themes-refine-cyan))))
    `(modus-themes-search-success-modeline ((,class :foreground ,@(modus-themes--success-deuteran
                                                                   blue-active
                                                                   green-active))))
    `(modus-themes-slant ((,class :inherit italic :slant ,@(modus-themes--slant))))
    `(modus-themes-variable-pitch ((,class ,@(modus-themes--variable-pitch))))
    `(modus-themes-fixed-pitch ((,class ,@(modus-themes--fixed-pitch))))
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
    `(buffer-menu-buffer ((,class :inherit bold)))
    `(comint-highlight-input ((,class :inherit bold)))
    `(comint-highlight-prompt ((,class :inherit modus-themes-prompt)))
    `(error ((,class :inherit bold :foreground ,red)))
    `(escape-glyph ((,class :foreground ,fg-escape-char-construct)))
    `(file-name-shadow ((,class :foreground ,fg-unfocused)))
    `(header-line ((,class ,@(modus-themes--variable-pitch-ui)
                           :background ,bg-header :foreground ,fg-header)))
    `(header-line-highlight ((,class :inherit modus-themes-active-blue)))
    `(help-argument-name ((,class :inherit modus-themes-slant :foreground ,cyan)))
    `(help-key-binding ((,class :box (:line-width (1 . -1) :color ,bg-region) ; NOTE: box syntax is for Emacs28
                                :background ,bg-inactive)))
    `(homoglyph ((,class :foreground ,red-alt-faint)))
    `(ibuffer-locked-buffer ((,class :foreground ,yellow-alt-other-faint)))
    `(italic ((,class :slant italic)))
    `(nobreak-hyphen ((,class :foreground ,fg-escape-char-construct)))
    `(nobreak-space ((,class :foreground ,fg-escape-char-construct :underline t)))
    `(minibuffer-prompt ((,class :inherit modus-themes-prompt)))
    `(mm-command-output ((,class :foreground ,red-alt-other)))
    `(mm-uu-extract ((,class :background ,bg-dim :foreground ,fg-special-mild)))
    `(next-error ((,class :inherit modus-themes-subtle-red :extend t)))
    `(rectangle-preview ((,class :inherit modus-themes-special-mild)))
    `(region ((,class ,@(modus-themes--region bg-region fg-main
                                              bg-hl-alt-intense bg-region-accent
                                              bg-region-accent-subtle))))
    `(secondary-selection ((,class :inherit modus-themes-special-cold)))
    `(shadow ((,class :foreground ,fg-alt)))
    `(success ((,class :inherit bold :foreground ,@(modus-themes--success-deuteran blue green))))
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
    `(widget-button ((,class :inherit bold :foreground ,blue-alt)))
    `(widget-button-pressed ((,class :inherit widget-button :foreground ,magenta)))
    `(widget-documentation ((,class :foreground ,green)))
    `(widget-field ((,class :background ,bg-alt :foreground ,fg-dim)))
    `(widget-inactive ((,class :foreground ,fg-alt)))
    `(widget-single-line-field ((,class :inherit widget-field)))
;;;;; ag
    `(ag-hit-face ((,class :foreground ,fg-special-cold)))
    `(ag-match-face ((,class :inherit modus-themes-special-calm)))
;;;;; alert
    `(alert-high-face ((,class :inherit bold :foreground ,red-alt)))
    `(alert-low-face ((,class :foreground ,fg-special-mild)))
    `(alert-moderate-face ((,class :inherit bold :foreground ,yellow)))
    `(alert-trivial-face ((,class :foreground ,fg-special-calm)))
    `(alert-urgent-face ((,class :inherit bold :foreground ,red-intense)))
;;;;; all-the-icons
    `(all-the-icons-blue ((,class :foreground ,blue)))
    `(all-the-icons-blue-alt ((,class :foreground ,blue-alt)))
    `(all-the-icons-cyan ((,class :foreground ,cyan)))
    `(all-the-icons-cyan-alt ((,class :foreground ,cyan-alt)))
    `(all-the-icons-dblue ((,class :foreground ,blue-alt-other)))
    `(all-the-icons-dcyan ((,class :foreground ,cyan-alt-other)))
    `(all-the-icons-dgreen ((,class :foreground ,green-alt-other)))
    `(all-the-icons-dired-dir-face ((,class :foreground ,blue)))
    `(all-the-icons-dmaroon ((,class :foreground ,magenta-alt-other)))
    `(all-the-icons-dorange ((,class :foreground ,red-alt-other)))
    `(all-the-icons-dpink ((,class :foreground ,magenta)))
    `(all-the-icons-dpurple ((,class :foreground ,magenta-alt)))
    `(all-the-icons-dred ((,class :foreground ,red)))
    `(all-the-icons-dsilver ((,class :foreground ,fg-special-cold)))
    `(all-the-icons-dyellow ((,class :foreground ,yellow)))
    `(all-the-icons-green ((,class :foreground ,green)))
    `(all-the-icons-lblue ((,class :foreground ,blue-refine-fg)))
    `(all-the-icons-lcyan ((,class :foreground ,cyan-refine-fg)))
    `(all-the-icons-lgreen ((,class :foreground ,green-refine-fg)))
    `(all-the-icons-lmaroon ((,class :foreground ,magenta-refine-fg)))
    `(all-the-icons-lorange ((,class :foreground ,red-refine-fg)))
    `(all-the-icons-lpink ((,class :foreground ,magenta-refine-fg)))
    `(all-the-icons-lpurple ((,class :foreground ,magenta-refine-fg)))
    `(all-the-icons-lred ((,class :foreground ,red-refine-fg)))
    `(all-the-icons-lsilver ((,class :foreground ,fg-special-cold)))
    `(all-the-icons-lyellow ((,class :foreground ,yellow-refine-fg)))
    `(all-the-icons-maroon ((,class :foreground ,magenta)))
    `(all-the-icons-orange ((,class :foreground ,red-alt)))
    `(all-the-icons-pink ((,class :foreground ,magenta)))
    `(all-the-icons-purple ((,class :foreground ,magenta-alt)))
    `(all-the-icons-purple-alt ((,class :foreground ,magenta-alt-other)))
    `(all-the-icons-red ((,class :foreground ,red)))
    `(all-the-icons-red-alt ((,class :foreground ,red-alt)))
    `(all-the-icons-silver ((,class :foreground ,fg-special-cold)))
    `(all-the-icons-yellow ((,class :foreground ,yellow)))
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
    `(anzu-replace-highlight ((,class :inherit modus-themes-refine-yellow :underline t)))
    `(anzu-replace-to ((,class :inherit (modus-themes-search-success bold))))
;;;;; apropos
    `(apropos-button ((,class :inherit button
                              ,@(modus-themes--link-color
                                 magenta-alt-other magenta-alt-other-faint))))
    `(apropos-function-button ((,class :inherit button
                                       ,@(modus-themes--link-color
                                          magenta magenta-faint))))
    `(apropos-keybinding ((,class :inherit modus-themes-key-binding)))
    `(apropos-misc-button ((,class :inherit button
                                   ,@(modus-themes--link-color
                                      green-alt-other green-alt-other-faint))))
    `(apropos-property ((,class :inherit modus-themes-bold :foreground ,magenta-alt)))
    `(apropos-symbol ((,class :inherit modus-themes-pseudo-header)))
    `(apropos-user-option-button ((,class :inherit button
                                          ,@(modus-themes--link-color
                                             cyan cyan-faint))))
    `(apropos-variable-button ((,class :inherit button
                                       ,@(modus-themes--link-color
                                          blue-alt blue-alt-faint))))
;;;;; apt-sources-list
    `(apt-sources-list-components ((,class :foreground ,cyan)))
    `(apt-sources-list-options ((,class :foreground ,yellow)))
    `(apt-sources-list-suite ((,class :foreground ,green)))
    `(apt-sources-list-type ((,class :foreground ,magenta)))
    `(apt-sources-list-uri ((,class :foreground ,blue)))
;;;;; artbollocks-mode
    `(artbollocks-face ((,class :inherit modus-themes-lang-note)))
    `(artbollocks-lexical-illusions-face ((,class :background ,bg-alt :foreground ,red-alt :underline t)))
    `(artbollocks-passive-voice-face ((,class :inherit modus-themes-lang-warning)))
    `(artbollocks-weasel-words-face ((,class :inherit modus-themes-lang-error)))
;;;;; auctex and Tex
    `(font-latex-bold-face ((,class :inherit bold :foreground ,fg-special-calm)))
    `(font-latex-doctex-documentation-face ((,class :inherit modus-themes-slant :foreground ,fg-special-cold)))
    `(font-latex-doctex-preprocessor-face ((,class :inherit modus-themes-bold :foreground ,red-alt-other)))
    `(font-latex-italic-face ((,class :inherit italic)))
    `(font-latex-math-face ((,class :foreground ,cyan-alt-other)))
    `(font-latex-script-char-face ((,class :foreground ,cyan-alt-other)))
    `(font-latex-sectioning-0-face ((,class :inherit modus-themes-variable-pitch :foreground ,blue-nuanced-fg)))
    `(font-latex-sectioning-1-face ((,class :inherit (bold modus-themes-variable-pitch) :foreground ,blue-nuanced-fg)))
    `(font-latex-sectioning-2-face ((,class :inherit (bold modus-themes-variable-pitch) :foreground ,blue-nuanced-fg)))
    `(font-latex-sectioning-3-face ((,class :inherit (bold modus-themes-variable-pitch) :foreground ,blue-nuanced-fg)))
    `(font-latex-sectioning-4-face ((,class :inherit (bold modus-themes-variable-pitch) :foreground ,blue-nuanced-fg)))
    `(font-latex-sectioning-5-face ((,class :inherit modus-themes-variable-pitch :foreground ,blue-nuanced-fg)))
    `(font-latex-sedate-face ((,class :inherit modus-themes-bold :foreground ,magenta-alt-other)))
    `(font-latex-slide-title-face ((,class :inherit (bold modus-themes-variable-pitch) :foreground ,cyan-nuanced-fg
                                           ,@(modus-themes--scale modus-themes-scale-4))))
    `(font-latex-string-face ((,class :inherit font-lock-string-face)))
    `(font-latex-subscript-face ((,class :height 0.95)))
    `(font-latex-superscript-face ((,class :height 0.95)))
    `(font-latex-verbatim-face ((,class :background ,bg-dim :foreground ,fg-special-mild)))
    `(font-latex-warning-face ((,class :inherit font-lock-warning-face)))
    `(tex-match ((,class :foreground ,blue-alt-other)))
    `(tex-verbatim ((,class :background ,bg-dim :foreground ,fg-special-mild)))
    `(texinfo-heading ((,class :foreground ,magenta)))
    `(TeX-error-description-error ((,class :inherit error)))
    `(TeX-error-description-help ((,class :foreground ,blue)))
    `(TeX-error-description-tex-said ((,class :foreground ,blue)))
    `(TeX-error-description-warning ((,class :inherit warning)))
;;;;; auto-dim-other-buffers
    `(auto-dim-other-buffers-face ((,class :background ,bg-alt)))
;;;;; avy
    `(avy-background-face ((,class :background ,bg-dim :foreground ,fg-dim :extend t)))
    `(avy-goto-char-timer-face ((,class :inherit (modus-themes-intense-yellow bold))))
    `(avy-lead-face ((,class :inherit (modus-themes-intense-magenta bold modus-themes-reset-soft))))
    `(avy-lead-face-0 ((,class :inherit (modus-themes-refine-cyan bold modus-themes-reset-soft))))
    `(avy-lead-face-1 ((,class :inherit (modus-themes-intense-neutral bold modus-themes-reset-soft))))
    `(avy-lead-face-2 ((,class :inherit (modus-themes-refine-red bold modus-themes-reset-soft))))
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
    `(bongo-track-length ((,class :foreground ,fg-alt)))
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
;;;;; buffer-expose
    `(buffer-expose-ace-char-face ((,class :inherit bold :foreground ,red-active)))
    `(buffer-expose-mode-line-face ((,class :foreground ,cyan-active)))
    `(buffer-expose-selected-face ((,class :inherit modus-themes-special-mild)))
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
    `(cfw:face-title ((,class :inherit modus-themes-variable-pitch
                              :foreground ,fg-special-cold
                              ,@(modus-themes--scale modus-themes-scale-title))))
    `(cfw:face-today ((,class :background ,bg-inactive)))
    `(cfw:face-today-title ((,class :background ,bg-active)))
    `(cfw:face-toolbar ((,class :background ,bg-alt :foreground ,bg-alt)))
    `(cfw:face-toolbar-button-off ((,class :inherit shadow)))
    `(cfw:face-toolbar-button-on ((,class :inherit bold :background ,blue-nuanced-bg
                                          :foreground ,blue-alt)))
;;;;; centaur-tabs
    `(centaur-tabs-active-bar-face ((,class :background ,blue-active)))
    `(centaur-tabs-close-mouse-face ((,class :inherit bold :foreground ,red-active :underline t)))
    `(centaur-tabs-close-selected ((,class :inherit centaur-tabs-selected)))
    `(centaur-tabs-close-unselected ((,class :inherit centaur-tabs-unselected)))
    `(centaur-tabs-modified-marker-selected ((,class :inherit centaur-tabs-selected)))
    `(centaur-tabs-modified-marker-unselected ((,class :inherit centaur-tabs-unselected)))
    `(centaur-tabs-default (( )))
    `(centaur-tabs-selected ((,class ,@(modus-themes--tab bg-tab-active nil nil nil t t))))
    `(centaur-tabs-selected-modified ((,class :inherit (italic centaur-tabs-selected))))
    `(centaur-tabs-unselected ((,class ,@(modus-themes--tab bg-tab-inactive bg-tab-inactive-accent fg-dim nil t))))
    `(centaur-tabs-unselected-modified ((,class :inherit (italic centaur-tabs-unselected))))
;;;;; cfrs
    `(cfrs-border-color ((,class :background ,fg-window-divider-inner)))
;;;;; change-log and log-view (`vc-print-log' and `vc-print-root-log')
    `(change-log-acknowledgment ((,class :foreground ,fg-alt)))
    `(change-log-conditionals ((,class :foreground ,yellow)))
    `(change-log-date ((,class :foreground ,cyan)))
    `(change-log-email ((,class :foreground ,cyan-alt-other)))
    `(change-log-file ((,class :inherit bold :foreground ,fg-special-cold)))
    `(change-log-function ((,class :foreground ,green-alt-other)))
    `(change-log-list ((,class :foreground ,magenta-alt)))
    `(change-log-name ((,class :foreground ,magenta-alt-other)))
    `(log-edit-header ((,class :foreground ,fg-special-warm)))
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
    `(cider-stacktrace-ns-face ((,class :inherit italic :foreground ,fg-alt)))
    `(cider-stacktrace-promoted-button-face ((,class :box (:line-width 3 :color ,fg-alt :style released-button) :foreground ,red)))
    `(cider-stacktrace-suppressed-button-face ((,class :box (:line-width 3 :color ,fg-alt :style pressed-button)
                                                       :background ,bg-alt :foreground ,fg-alt)))
    `(cider-test-error-face ((,class :inherit modus-themes-subtle-red)))
    `(cider-test-failure-face ((,class :inherit (modus-themes-intense-red bold))))
    `(cider-test-success-face ((,class :inherit ,@(modus-themes--success-deuteran
                                                   'modus-themes-intense-blue
                                                   'modus-themes-intense-green))))
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
    `(company-echo-common ((,class :foreground ,magenta-alt-other)))
    `(company-preview ((,class :background ,bg-dim :foreground ,fg-dim)))
    `(company-preview-common ((,class :foreground ,blue-alt)))
    `(company-preview-search ((,class :inherit modus-themes-special-calm)))
    `(company-scrollbar-bg ((,class :background ,bg-active)))
    `(company-scrollbar-fg ((,class :background ,fg-active)))
    `(company-template-field ((,class :inherit modus-themes-intense-magenta)))
    `(company-tooltip ((,class :background ,bg-alt :foreground ,fg-alt)))
    `(company-tooltip-annotation ((,class :inherit modus-themes-slant :foreground ,fg-special-cold)))
    `(company-tooltip-annotation-selection ((,class :inherit bold :foreground ,fg-main)))
    `(company-tooltip-common ((,class :inherit bold :foreground ,blue-alt)))
    `(company-tooltip-common-selection ((,class :foreground ,fg-main)))
    `(company-tooltip-mouse ((,class :inherit modus-themes-intense-blue)))
    `(company-tooltip-search ((,class :inherit (modus-themes-search-success-lazy bold))))
    `(company-tooltip-search-selection ((,class :inherit (modus-themes-search-success bold) :underline t)))
    `(company-tooltip-selection ((,class :inherit (modus-themes-subtle-cyan bold))))
;;;;; company-posframe
    `(company-posframe-active-backend-name ((,class :inherit bold :background ,bg-active :foreground ,blue-active)))
    `(company-posframe-inactive-backend-name ((,class :background ,bg-active :foreground ,fg-active)))
    `(company-posframe-metadata ((,class :background ,bg-inactive :foreground ,fg-inactive)))
;;;;; compilation feedback
    `(compilation-column-number ((,class :foreground ,magenta-alt-other)))
    `(compilation-error ((,class :inherit modus-themes-bold :foreground ,red)))
    `(compilation-info ((,class :inherit modus-themes-bold :foreground ,fg-special-cold)))
    `(compilation-line-number ((,class :foreground ,fg-special-warm)))
    `(compilation-mode-line-exit ((,class :inherit modus-themes-bold :foreground ,blue-active)))
    `(compilation-mode-line-fail ((,class :inherit modus-themes-bold :foreground ,red-active)))
    `(compilation-mode-line-run ((,class :inherit modus-themes-bold :foreground ,magenta-active)))
    `(compilation-warning ((,class :inherit modus-themes-bold :foreground ,yellow)))
;;;;; completions
    `(completions-annotations ((,class :inherit modus-themes-slant :foreground ,cyan-faint)))
    `(completions-common-part ((,class ,@(modus-themes--standard-completions
                                          blue-alt blue-nuanced-bg
                                          cyan-refine-bg cyan-refine-fg))))
    `(completions-first-difference ((,class :inherit bold
                                            ,@(modus-themes--standard-completions
                                               magenta-alt blue-nuanced-bg
                                               magenta-intense-bg fg-main))))
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
    `(consult-preview-error ((,class :inherit modus-themes-intense-red)))
    `(consult-preview-line ((,class :background ,bg-hl-alt-intense)))
;;;;; corfu
    `(corfu-background ((,class :background ,bg-alt)))
    `(corfu-current ((,class :inherit bold :background ,cyan-subtle-bg)))
    `(corfu-bar ((,class :background ,fg-alt)))
    `(corfu-border ((,class :background ,bg-active)))
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
;;;;; counsel-org-capture-string
    `(counsel-org-capture-string-template-body-face ((,class :foreground ,fg-special-cold)))
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
;;;;; css-mode
    `(css-property ((,class :inherit font-lock-type-face)))
    `(css-selector ((,class :inherit font-lock-keyword-face)))
;;;;; csv-mode
    `(csv-separator-face ((,class :foreground ,red-intense)))
;;;;; ctrlf
    `(ctrlf-highlight-active ((,class :inherit (modus-themes-search-success bold))))
    `(ctrlf-highlight-line ((,class :inherit modus-themes-hl-line)))
    `(ctrlf-highlight-passive ((,class :inherit modus-themes-search-success-lazy)))
;;;;; cursor-flash
    `(cursor-flash-face ((,class :inherit modus-themes-intense-blue)))
;;;;; custom (M-x customize)
    `(custom-button ((,class :box (:line-width 2 :color nil :style released-button)
                             :background ,bg-active :foreground ,fg-main)))
    `(custom-button-mouse ((,class :box (:line-width 2 :color nil :style released-button)
                                   :background ,bg-active :foreground ,fg-active)))
    `(custom-button-pressed ((,class :box (:line-width 2 :color nil :style pressed-button)
                                     :background ,bg-active :foreground ,fg-main)))
    `(custom-changed ((,class :inherit modus-themes-subtle-cyan)))
    `(custom-comment ((,class :inherit shadow)))
    `(custom-comment-tag ((,class :background ,bg-alt :foreground ,yellow-alt-other)))
    `(custom-face-tag ((,class :inherit bold :foreground ,blue-intense)))
    `(custom-group-tag ((,class :inherit bold :foreground ,green-intense)))
    `(custom-group-tag-1 ((,class :inherit modus-themes-special-warm)))
    `(custom-invalid ((,class :inherit (modus-themes-intense-red bold))))
    `(custom-modified ((,class :inherit modus-themes-subtle-cyan)))
    `(custom-rogue ((,class :inherit modus-themes-refine-magenta)))
    `(custom-set ((,class :foreground ,blue-alt)))
    `(custom-state ((,class :foreground ,cyan-alt-other)))
    `(custom-themed ((,class :inherit modus-themes-subtle-blue)))
    `(custom-variable-tag ((,class :inherit bold :foreground ,cyan)))
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
;;;;; dashboard (emacs-dashboard)
    `(dashboard-banner-logo-title ((,class :inherit bold :foreground ,fg-special-cold)))
    `(dashboard-footer ((,class :inherit bold :foreground ,fg-special-mild)))
    `(dashboard-heading ((,class :inherit bold :foreground ,fg-special-warm)))
    `(dashboard-navigator ((,class :foreground ,cyan-alt-other)))
    `(dashboard-text-banner ((,class :foreground ,fg-dim)))
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
;;;;; define-word
    `(define-word-face-1 ((,class :foreground ,yellow)))
    `(define-word-face-2 ((,class :foreground ,fg-main)))
;;;;; deft
    `(deft-filter-string-error-face ((,class :inherit modus-themes-refine-red)))
    `(deft-filter-string-face ((,class :foreground ,green-intense)))
    `(deft-header-face ((,class :inherit bold :foreground ,fg-special-warm)))
    `(deft-separator-face ((,class :inherit shadow)))
    `(deft-summary-face ((,class :inherit modus-themes-slant :foreground ,fg-alt)))
    `(deft-time-face ((,class :foreground ,fg-special-cold)))
    `(deft-title-face ((,class :inherit bold :foreground ,fg-main)))
;;;;; dictionary
    `(dictionary-button-face ((,class :inherit bold :foreground ,fg-special-cold)))
    `(dictionary-reference-face ((,class :inherit button)))
    `(dictionary-word-definition-face (()))
    `(dictionary-word-entry-face ((,class :inherit font-lock-comment-face)))
;;;;; diff-hl
    `(diff-hl-change ((,class :inherit modus-themes-fringe-yellow)))
    `(diff-hl-delete ((,class :inherit modus-themes-fringe-red)))
    `(diff-hl-dired-change ((,class :inherit diff-hl-change)))
    `(diff-hl-dired-delete ((,class :inherit diff-hl-delete)))
    `(diff-hl-dired-ignored ((,class :inherit dired-ignored)))
    `(diff-hl-dired-insert ((,class :inherit diff-hl-insert)))
    `(diff-hl-dired-unknown ((,class :inherit dired-ignored)))
    `(diff-hl-insert ((,class :inherit ,@(modus-themes--diff-deuteran
                                          'modus-themes-fringe-blue
                                          'modus-themes-fringe-green))))
    `(diff-hl-reverted-hunk-highlight ((,class :background ,fg-main :foreground ,bg-main)))
;;;;; diff-mode
    `(diff-added ((,class :inherit modus-themes-diff-added)))
    `(diff-changed ((,class :inherit modus-themes-diff-changed :extend t)))
    `(diff-context ((,class ,@(unless (eq modus-themes-diffs 'bg-only) (list :foreground fg-unfocused)))))
    `(diff-error ((,class :inherit modus-themes-intense-red)))
    `(diff-file-header ((,class :inherit (bold diff-header))))
    `(diff-function ((,class :inherit modus-themes-diff-heading)))
    `(diff-header ((,class :foreground ,fg-main)))
    `(diff-hunk-header ((,class :inherit (bold modus-themes-diff-heading))))
    `(diff-index ((,class :inherit bold :foreground ,blue-alt)))
    `(diff-indicator-added ((,class :inherit (diff-added bold)
                                    :foreground ,@(modus-themes--diff-deuteran blue green))))
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
    `(dir-treeview-default-icon-face ((,class :inherit bold :family "Font Awesome" :foreground ,fg-alt)))
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
    `(dir-treeview-symlink-face ((,class :inherit button
                                         ,@(modus-themes--link-color
                                            cyan cyan-faint))))
    `(dir-treeview-video-face ((,class :foreground ,magenta-alt-other)))
    `(dir-treeview-video-icon-face ((,class :inherit dir-treeview-default-icon-face :foreground ,magenta-alt-other)))
;;;;; dired
    `(dired-broken-symlink ((,class :inherit button :foreground ,red)))
    `(dired-directory ((,class :foreground ,blue)))
    `(dired-flagged ((,class :inherit modus-themes-mark-del)))
    `(dired-header ((,class :inherit modus-themes-pseudo-header)))
    `(dired-ignored ((,class :inherit shadow)))
    `(dired-mark ((,class :inherit modus-themes-mark-symbol)))
    `(dired-marked ((,class :inherit modus-themes-mark-sel)))
    `(dired-perm-write ((,class :foreground ,fg-special-warm)))
    `(dired-symlink ((,class :inherit button
                             ,@(modus-themes--link-color
                                cyan-alt cyan-alt-faint))))
    `(dired-warning ((,class :inherit bold :foreground ,yellow)))
;;;;; dired-async
    `(dired-async-failures ((,class :inherit bold :foreground ,red-active)))
    `(dired-async-message ((,class :inherit bold :foreground ,blue-active)))
    `(dired-async-mode-message ((,class :inherit bold :foreground ,cyan-active)))
;;;;; dired-git
    `(dired-git-branch-else ((,class :inherit bold :foreground ,magenta-alt)))
    `(dired-git-branch-master ((,class :inherit bold :foreground ,magenta-alt-other)))
;;;;; dired-git-info
    `(dgi-commit-message-face ((,class :foreground ,fg-special-mild)))
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
;;;;; diredc
    `(diredc-face-chmod-font-lock-dir ((,class :foreground ,blue-alt)))
    `(diredc-face-chmod-font-lock-exec ((,class :foreground ,magenta)))
    `(diredc-face-chmod-font-lock-read ((,class :foreground ,fg-main)))
    `(diredc-face-chmod-font-lock-write ((,class :foreground ,cyan)))
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
;;;;; disk-usage
    `(disk-usage-children ((,class :foreground ,yellow)))
    `(disk-usage-inaccessible ((,class :inherit bold :foreground ,red)))
    `(disk-usage-percent ((,class :foreground ,green)))
    `(disk-usage-size ((,class :foreground ,cyan)))
    `(disk-usage-symlink ((,class :inherit button)))
    `(disk-usage-symlink-directory ((,class :inherit bold :foreground ,blue-alt)))
;;;;; display-fill-column-indicator-mode
    `(fill-column-indicator ((,class :foreground ,bg-active)))
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
    `(doom-modeline-lsp-success ((,class :inherit bold :foreground ,@(modus-themes--success-deuteran
                                                                      blue-active
                                                                      green-active))))
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
;;;;; dynamic-ruler
    `(dynamic-ruler-negative-face ((,class :inherit modus-themes-intense-neutral)))
    `(dynamic-ruler-positive-face ((,class :inherit modus-themes-intense-yellow)))
;;;;; easy-jekyll
    `(easy-jekyll-help-face ((,class :background ,bg-dim :foreground ,cyan-alt-other)))
;;;;; easy-kill
    `(easy-kill-origin ((,class :inherit modus-themes-subtle-red)))
    `(easy-kill-selection ((,class :inherit modus-themes-subtle-yellow)))
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
                                              bg-alt fg-special-cold
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
;;;;; eglot
    `(eglot-mode-line ((,class :inherit modus-themes-bold :foreground ,magenta-active)))
;;;;; el-search
    `(el-search-highlight-in-prompt-face ((,class :inherit bold :foreground ,magenta-alt)))
    `(el-search-match ((,class :inherit modus-themes-search-success)))
    `(el-search-other-match ((,class :inherit modus-themes-special-mild)))
    `(el-search-occur-match ((,class :inherit modus-themes-special-calm)))
;;;;; eldoc
    ;; NOTE: see https://github.com/purcell/package-lint/issues/187
    (list 'eldoc-highlight-function-argument `((,class :inherit bold :foreground ,blue-alt-other)))
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
    `(elfeed-search-last-update-face ((,class :foreground ,cyan-active)))
    `(elfeed-search-tag-face ((,class :foreground ,cyan-alt-other)))
    `(elfeed-search-title-face ((,class :foreground ,fg-dim)))
    `(elfeed-search-unread-count-face ((,class :foreground ,green-active)))
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
;;;;; emms
    `(emms-browser-album-face ((,class :foreground ,magenta-alt-other ,@(modus-themes--scale modus-themes-scale-2))))
    `(emms-browser-artist-face ((,class :foreground ,cyan ,@(modus-themes--scale modus-themes-scale-3))))
    `(emms-browser-composer-face ((,class :foreground ,magenta-alt ,@(modus-themes--scale modus-themes-scale-3))))
    `(emms-browser-performer-face ((,class :inherit emms-browser-artist-face)))
    `(emms-browser-track-face ((,class :inherit emms-playlist-track-face)))
    `(emms-browser-year/genre-face ((,class :foreground ,cyan-alt-other ,@(modus-themes--scale modus-themes-scale-4))))
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
    `(erc-action-face ((,class :inherit bold :foreground ,cyan)))
    `(erc-bold-face ((,class :inherit bold)))
    `(erc-button ((,class :inherit button)))
    `(erc-command-indicator-face ((,class :inherit bold :foreground ,cyan-alt)))
    `(erc-current-nick-face ((,class :foreground ,magenta-alt-other)))
    `(erc-dangerous-host-face ((,class :inherit modus-themes-intense-red)))
    `(erc-direct-msg-face ((,class :foreground ,magenta)))
    `(erc-error-face ((,class :inherit bold :foreground ,red)))
    `(erc-fool-face ((,class :foreground ,fg-inactive)))
    `(erc-header-line ((,class :background ,bg-header :foreground ,fg-header)))
    `(erc-input-face ((,class :foreground ,fg-special-calm)))
    `(erc-inverse-face ((,class :inherit erc-default-face :inverse-video t)))
    `(erc-keyword-face ((,class :inherit bold :foreground ,magenta-alt)))
    `(erc-my-nick-face ((,class :inherit bold :foreground ,magenta)))
    `(erc-my-nick-prefix-face ((,class :inherit erc-my-nick-face)))
    `(erc-nick-default-face ((,class :inherit bold :foreground ,blue)))
    `(erc-nick-msg-face ((,class :inherit bold :foreground ,green)))
    `(erc-nick-prefix-face ((,class :inherit erc-nick-default-face)))
    `(erc-notice-face ((,class :foreground ,fg-unfocused)))
    `(erc-pal-face ((,class :inherit bold :foreground ,red-alt)))
    `(erc-prompt-face ((,class :inherit modus-themes-prompt)))
    `(erc-timestamp-face ((,class :foreground ,blue-nuanced-fg)))
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
    `(eshell-ls-symlink ((,class :foreground ,cyan)))
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
    `(evil-ex-substitute-replacement ((,class :inherit (modus-themes-search-success bold))))
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
    `(eww-form-submit ((,class :box (:line-width 2 :style released-button)
                               :background ,bg-active)))
    `(eww-form-text ((,class :box ,bg-active :background ,bg-alt)))
    `(eww-form-textarea ((,class :background ,bg-alt)))
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
    `(flycheck-indicator-success ((,class :inherit modus-themes-bold
                                          :foreground ,@(modus-themes--success-deuteran
                                                         blue-active
                                                         green-active))))
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
;;;;; flyspell-correct
    `(flyspell-correct-highlight-face ((,class :inherit modus-themes-refine-green)))
;;;;; flx
    `(flx-highlight-face ((,class ,@(modus-themes--extra-completions
                                     'modus-themes-subtle-magenta
                                     'modus-themes-intense-magenta
                                     'modus-themes-nuanced-magenta
                                     magenta-alt
                                     'bold))))
;;;;; freeze-it
    `(freeze-it-show ((,class :background ,bg-dim :foreground ,fg-special-warm)))
;;;;; frog-menu
    `(frog-menu-action-keybinding-face ((,class :inherit modus-themes-key-binding)))
    `(frog-menu-actions-face ((,class :foreground ,magenta)))
    `(frog-menu-border ((,class :background ,bg-active)))
    `(frog-menu-candidates-face ((,class :foreground ,fg-main)))
    `(frog-menu-posframe-background-face ((,class :background ,bg-dim)))
    `(frog-menu-prompt-face ((,class :foreground ,cyan)))
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
                                     fg-special-mild magenta-nuanced-fg
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
                                              blue-alt blue-alt-faint))))
    `(font-lock-regexp-grouping-backslash ((,class :inherit bold
                                                   ,@(modus-themes--syntax-string
                                                      fg-escape-char-backslash yellow-alt-faint
                                                      yellow magenta-alt
                                                      yellow-faint red-faint))))
    `(font-lock-regexp-grouping-construct ((,class :inherit bold
                                                   ,@(modus-themes--syntax-string
                                                      fg-escape-char-construct red-alt-other-faint
                                                      blue blue-alt-other
                                                      blue-faint blue-alt-other-faint))))
    `(font-lock-string-face ((,class ,@(modus-themes--syntax-string
                                        blue-alt blue-alt-faint
                                        green red
                                        green-faint red-faint))))
    `(font-lock-type-face ((,class :inherit modus-themes-bold
                                   ,@(modus-themes--syntax-foreground
                                      cyan-alt-other cyan-alt-faint))))
    `(font-lock-variable-name-face ((,class ,@(modus-themes--syntax-extra
                                               cyan cyan-faint
                                               blue-alt-other blue-alt-other-faint))))
    `(font-lock-warning-face ((,class :inherit modus-themes-bold
                                      ,@(modus-themes--syntax-comment
                                         yellow-active red-active red-faint yellow-faint))))
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
    `(fountain-comment ((,class :inherit modus-themes-slant :foreground ,fg-alt)))
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
    `(geiser-font-lock-autodoc-current-arg ((,class :inherit font-lock-function-name-face)))
    `(geiser-font-lock-autodoc-identifier ((,class :inherit font-lock-constant-face)))
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
    `(git-commit-comment-branch-local ((,class :inherit modus-themes-slant :foreground ,blue-alt)))
    `(git-commit-comment-branch-remote ((,class :inherit modus-themes-slant :foreground ,magenta-alt)))
    `(git-commit-comment-detached ((,class :inherit modus-themes-slant :foreground ,cyan-alt)))
    `(git-commit-comment-file ((,class :inherit modus-themes-slant
                                       ,@(modus-themes--syntax-comment
                                          fg-special-cold red-nuanced-fg))))
    `(git-commit-comment-heading ((,class :inherit (bold modus-themes-slant)
                                          ,@(modus-themes--syntax-comment
                                             fg-dim fg-special-warm))))
    `(git-commit-keyword ((,class :foreground ,magenta)))
    `(git-commit-known-pseudo-header ((,class :foreground ,cyan-alt-other)))
    `(git-commit-nonempty-second-line ((,class :inherit modus-themes-refine-yellow)))
    `(git-commit-overlong-summary ((,class :inherit modus-themes-refine-yellow)))
    `(git-commit-pseudo-header ((,class :foreground ,blue)))
    `(git-commit-summary ((,class :inherit bold :foreground ,cyan)))
;;;;; git-gutter
    `(git-gutter:added ((,class :inherit ,@(modus-themes--diff-deuteran
                                            'modus-themes-fringe-blue
                                            'modus-themes-fringe-green))))
    `(git-gutter:deleted ((,class :inherit modus-themes-fringe-red)))
    `(git-gutter:modified ((,class :inherit modus-themes-fringe-yellow)))
    `(git-gutter:separator ((,class :inherit modus-themes-fringe-cyan)))
    `(git-gutter:unchanged ((,class :inherit modus-themes-fringe-magenta)))
;;;;; git-gutter-fr
    `(git-gutter-fr:added ((,class :inherit ,@(modus-themes--diff-deuteran
                                               'modus-themes-fringe-blue
                                               'modus-themes-fringe-green))))
    `(git-gutter-fr:deleted ((,class :inherit modus-themes-fringe-red)))
    `(git-gutter-fr:modified ((,class :inherit modus-themes-fringe-yellow)))
;;;;; git-{gutter,fringe}+
    `(git-gutter+-added ((,class :inherit ,@(modus-themes--diff-deuteran
                                          'modus-themes-fringe-blue
                                          'modus-themes-fringe-green))))
    `(git-gutter+-deleted ((,class :inherit modus-themes-fringe-red)))
    `(git-gutter+-modified ((,class :inherit modus-themes-fringe-yellow)))
    `(git-gutter+-separator ((,class :inherit modus-themes-fringe-cyan)))
    `(git-gutter+-unchanged ((,class :inherit modus-themes-fringe-magenta)))
    `(git-gutter-fr+-added ((,class :inherit modus-themes-fringe-green)))
    `(git-gutter-fr+-deleted ((,class :inherit modus-themes-fringe-red)))
    `(git-gutter-fr+-modified ((,class :inherit modus-themes-fringe-yellow)))
;;;;; git-lens
    `(git-lens-added ((,class :inherit bold :foreground ,@(modus-themes--diff-deuteran blue green))))
    `(git-lens-deleted ((,class :inherit bold :foreground ,red)))
    `(git-lens-header ((,class :inherit bold :height 1.1 :foreground ,cyan)))
    `(git-lens-modified ((,class :inherit bold :foreground ,yellow)))
    `(git-lens-renamed ((,class :inherit bold :foreground ,magenta)))
;;;;; git-rebase
    `(git-rebase-comment-hash ((,class :inherit modus-themes-slant
                                       ,@(modus-themes--syntax-comment
                                          fg-special-cold red-nuanced-fg))))
    `(git-rebase-comment-heading  ((,class :inherit (bold modus-themes-slant)
                                           ,@(modus-themes--syntax-comment
                                              fg-dim fg-special-warm))))
    `(git-rebase-description ((,class :foreground ,fg-main)))
    `(git-rebase-hash ((,class :foreground ,cyan-alt-other)))
;;;;; git-timemachine
    `(git-timemachine-commit ((,class :inherit bold :foreground ,yellow-active)))
    `(git-timemachine-minibuffer-author-face ((,class :foreground ,fg-special-warm)))
    `(git-timemachine-minibuffer-detail-face ((,class :foreground ,red-alt)))
;;;;; git-walktree
    `(git-walktree-commit-face ((,class :foreground ,yellow)))
    `(git-walktree-symlink-face ((,class :inherit button)))
    `(git-walktree-tree-face ((,class :foreground ,magenta)))
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
    `(gnus-summary-low-unread ((,class :inherit bold :foreground ,fg-special-cold)))
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
    `(helm-ff-denied ((,class ,@(modus-themes--extra-completions
                                 'modus-themes-subtle-red
                                 'modus-themes-intense-red
                                 'modus-themes-nuanced-red
                                 red))))
    `(helm-ff-directory ((,class :inherit helm-buffer-directory)))
    `(helm-ff-dirs ((,class :inherit bold :foreground ,blue-alt-other)))
    `(helm-ff-dotted-directory ((,class :inherit bold :background ,bg-alt :foreground ,fg-alt)))
    `(helm-ff-dotted-symlink-directory ((,class :inherit (button helm-ff-dotted-directory))))
    `(helm-ff-executable ((,class :foreground ,magenta-alt)))
    `(helm-ff-file ((,class :foreground ,fg-main)))
    `(helm-ff-file-extension ((,class :foreground ,fg-special-warm)))
    `(helm-ff-invalid-symlink ((,class :inherit button
                                       ,@(modus-themes--link-color
                                          red red-faint))))
    `(helm-ff-pipe ((,class ,@(modus-themes--extra-completions
                               'modus-themes-refine-magenta
                               'modus-themes-subtle-magenta
                               'modus-themes-nuanced-magenta
                               magenta))))
    `(helm-ff-prefix ((,class ,@(modus-themes--extra-completions
                                 'modus-themes-refine-yellow
                                 'modus-themes-subtle-yellow
                                 'modus-themes-nuanced-yellow
                                 yellow-alt-other))))
    `(helm-ff-socket ((,class :foreground ,red-alt-other)))
    `(helm-ff-suid ((,class ,@(modus-themes--extra-completions
                               'modus-themes-subtle-red
                               'modus-themes-refine-red
                               'modus-themes-nuanced-yellow
                               red-alt))))
    `(helm-ff-symlink ((,class :inherit button
                               ,@(modus-themes--link-color
                                  cyan cyan-faint))))
    `(helm-ff-truename ((,class :foreground ,blue-alt-other)))
    `(helm-fd-finish ((,class :foreground ,green-active)))
    `(helm-grep-cmd-line ((,class :foreground ,yellow-alt-other)))
    `(helm-grep-file ((,class :inherit bold :foreground ,fg-special-cold)))
    `(helm-grep-finish ((,class :foreground ,green-active)))
    `(helm-grep-lineno ((,class :foreground ,fg-special-warm)))
    `(helm-grep-match ((,class :inherit modus-themes-special-calm)))
    `(helm-header ((,class :inherit bold :foreground ,fg-special-cold)))
    `(helm-header-line-left-margin ((,class :inherit bold :foreground ,yellow-intense)))
    `(helm-history-deleted ((,class ,@(modus-themes--extra-completions
                                       'modus-themes-subtle-red
                                       'modus-themes-intense-red
                                       'modus-themes-nuanced-red
                                       red
                                       'bold))))
    `(helm-history-remote ((,class :foreground ,red-alt-other)))
    `(helm-lisp-completion-info ((,class :foreground ,fg-special-warm)))
    `(helm-lisp-show-completion ((,class ,@(modus-themes--extra-completions
                                            'modus-themes-subtle-yellow
                                            'modus-themes-refine-yellow
                                            'modus-themes-nuanced-yellow
                                            yellow
                                            'bold))))
    `(helm-locate-finish ((,class :foreground ,green-active)))
    `(helm-match ((,class ,@(modus-themes--extra-completions
                             'modus-themes-subtle-cyan
                             'modus-themes-refine-cyan
                             'modus-themes-nuanced-cyan
                             cyan
                             'bold))))
    `(helm-match-item ((,class ,@(modus-themes--extra-completions
                                  'modus-themes-subtle-neutral
                                  'modus-themes-subtle-cyan
                                  'modus-themes-nuanced-cyan
                                  cyan-alt-other))))
    `(helm-minibuffer-prompt ((,class :inherit modus-themes-prompt)))
    `(helm-moccur-buffer ((,class :inherit button
                                  ,@(modus-themes--link-color
                                     cyan-alt-other cyan-alt-other-faint))))
    `(helm-mode-prefix ((,class ,@(modus-themes--extra-completions
                                   'modus-themes-subtle-magenta
                                   'modus-themes-intense-magenta
                                   'modus-themes-nuanced-magenta
                                   magenta-alt
                                   'bold))))
    `(helm-non-file-buffer ((,class :inherit shadow)))
    `(helm-prefarg ((,class :foreground ,red-active)))
    `(helm-resume-need-update ((,class ,@(modus-themes--extra-completions
                                          'modus-themes-subtle-magenta
                                          'modus-themes-refine-magenta
                                          'modus-themes-nuanced-magenta
                                          magenta-alt-other))))
    `(helm-selection ((,class ,@(modus-themes--extra-completions
                                 'modus-themes-subtle-blue
                                 'modus-themes-refine-blue
                                 'modus-themes-special-cold
                                 nil
                                 'bold))))
    `(helm-selection-line ((,class :inherit modus-themes-special-cold)))
    `(helm-separator ((,class :foreground ,fg-special-mild)))
    `(helm-time-zone-current ((,class :foreground ,green)))
    `(helm-time-zone-home ((,class :foreground ,magenta)))
    `(helm-source-header ((,class :inherit bold :foreground ,red-alt
                                  ,@(modus-themes--scale modus-themes-scale-4))))
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
    `(helm-switch-shell-new-shell-face ((,class ,@(modus-themes--extra-completions
                                                   'modus-themes-subtle-magenta
                                                   'modus-themes-refine-magenta
                                                   'modus-themes-nuanced-magenta
                                                   magenta-alt-other
                                                   'bold))))
;;;;; helm-xref
    `(helm-xref-file-name ((,class :inherit bold :foreground ,fg-special-cold)))
    `(helm-xref-file-name ((,class :foreground ,fg-special-warm)))
;;;;; helpful
    `(helpful-heading ((,class :inherit modus-themes-heading-1)))
;;;;; highlight region or ad-hoc regexp
    `(hi-aquamarine ((,class :background ,cyan-subtle-bg :foreground ,fg-main)))
    `(hi-black-b ((,class :inherit bold :background ,fg-main :foreground ,bg-main)))
    `(hi-black-hb ((,class :inherit bold :background ,fg-alt :foreground ,bg-main)))
    `(hi-blue ((,class :background ,blue-subtle-bg :foreground ,fg-main)))
    `(hi-blue-b ((,class :inherit (bold hi-blue))))
    `(hi-green ((,class :background ,green-subtle-bg :foreground ,fg-main)))
    `(hi-green-b ((,class :inherit (bold hi-green))))
    `(hi-pink ((,class :background ,magenta-subtle-bg :foreground ,fg-main)))
    `(hi-pink-b ((,class :inherit (bold hi-pink))))
    `(hi-red-b ((,class :inherit bold :background ,red-intense-bg :foreground ,fg-main)))
    `(hi-salmon ((,class :background ,red-subtle-bg :foreground ,fg-main)))
    `(hi-yellow ((,class :background ,yellow-subtle-bg :foreground ,fg-main)))
    `(highlight ((,class :background ,blue-subtle-bg :foreground ,fg-main)))
    `(highlight-changes ((,class :foreground ,red-alt :underline nil)))
    `(highlight-changes-delete ((,class :background ,red-nuanced-bg
                                        :foreground ,red :underline t)))
    `(hl-line ((,class :inherit modus-themes-hl-line)))
;;;;; highlight-blocks
    `(highlight-blocks-depth-1-face ((,class :background ,bg-dim :foreground ,fg-main)))
    `(highlight-blocks-depth-2-face ((,class :background ,bg-alt :foreground ,fg-main)))
    `(highlight-blocks-depth-3-face ((,class :background ,bg-special-cold :foreground ,fg-main)))
    `(highlight-blocks-depth-4-face ((,class :background ,bg-special-calm :foreground ,fg-main)))
    `(highlight-blocks-depth-5-face ((,class :background ,bg-special-warm :foreground ,fg-main)))
    `(highlight-blocks-depth-6-face ((,class :background ,bg-special-mild :foreground ,fg-main)))
    `(highlight-blocks-depth-7-face ((,class :background ,bg-inactive :foreground ,fg-main)))
    `(highlight-blocks-depth-8-face ((,class :background ,bg-active :foreground ,fg-main)))
    `(highlight-blocks-depth-9-face ((,class :background ,cyan-subtle-bg :foreground ,fg-main)))
;;;;; highlight-defined
    `(highlight-defined-builtin-function-name-face ((,class :foreground ,magenta)))
    `(highlight-defined-face-name-face ((,class :foreground ,fg-main)))
    `(highlight-defined-function-name-face ((,class :foreground ,magenta)))
    `(highlight-defined-macro-name-face ((,class :foreground ,magenta-alt)))
    `(highlight-defined-special-form-name-face ((,class :foreground ,magenta-alt-other)))
    `(highlight-defined-variable-name-face ((,class :foreground ,cyan)))
;;;;; highlight-escape-sequences (`hes-mode')
    `(hes-escape-backslash-face ((,class :inherit font-lock-regexp-grouping-construct)))
    `(hes-escape-sequence-face ((,class :inherit font-lock-regexp-grouping-backslash)))
;;;;; highlight-indentation
    `(highlight-indentation-face ((,class :inherit modus-themes-hl-line)))
    `(highlight-indentation-current-column-face ((,class :background ,bg-active)))
;;;;; highlight-numbers
    `(highlight-numbers-number ((,class :foreground ,blue-alt-other)))
;;;;; highlight-symbol
    `(highlight-symbol-face ((,class :inherit modus-themes-special-mild)))
;;;;; highlight-thing
    `(highlight-thing ((,class :background ,bg-alt :foreground ,cyan)))
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
;;;;; hyperlist
    `(hyperlist-condition ((,class :foreground ,green)))
    `(hyperlist-hashtag ((,class :foreground ,yellow)))
    `(hyperlist-operator ((,class :foreground ,blue-alt)))
    `(hyperlist-paren ((,class :foreground ,cyan-alt-other)))
    `(hyperlist-quote ((,class :foreground ,cyan-alt)))
    `(hyperlist-ref ((,class :foreground ,magenta-alt-other)))
    `(hyperlist-stars ((,class :inherit shadow)))
    `(hyperlist-tag ((,class :foreground ,red)))
    `(hyperlist-toplevel ((,class :inherit bold :foreground ,fg-main)))
;;;;; icomplete
    `(icomplete-first-match ((,class :inherit bold
                                     ,@(modus-themes--standard-completions
                                        magenta bg-alt
                                        bg-active fg-main))))
    `(icomplete-selected-match ((,class :inherit bold :foreground ,fg-main
                                        :background ,@(pcase modus-themes-completions
                                                        ('opinionated (list bg-active))
                                                        (_ (list bg-inactive))))))
;;;;; icomplete-vertical
    `(icomplete-vertical-separator ((,class :inherit shadow)))
;;;;; ido-mode
    `(ido-first-match ((,class :inherit bold
                               ,@(modus-themes--standard-completions
                                  magenta bg-alt
                                  bg-active fg-main))))
    `(ido-incomplete-regexp ((,class :inherit error)))
    `(ido-indicator ((,class :inherit modus-themes-subtle-yellow)))
    `(ido-only-match ((,class :inherit bold
                              ,@(modus-themes--standard-completions
                                 green green-nuanced-bg
                                 green-intense-bg fg-main))))
    `(ido-subdir ((,class :foreground ,blue)))
    `(ido-virtual ((,class :foreground ,fg-special-warm)))
;;;;; iedit
    `(iedit-occurrence ((,class :inherit modus-themes-refine-blue)))
    `(iedit-read-only-occurrence ((,class :inherit modus-themes-intense-yellow)))
;;;;; iflipb
    `(iflipb-current-buffer-face ((,class :inherit bold :foreground ,cyan-alt)))
    `(iflipb-other-buffer-face ((,class :inherit shadow)))
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
    `(indium-frame-url-face ((,class :inherit button :foreground ,fg-alt)))
    `(indium-keyword-face ((,class :inherit font-lock-keyword-face)))
    `(indium-litable-face ((,class :inherit modus-themes-slant :foreground ,fg-special-warm)))
    `(indium-repl-error-face ((,class :inherit error)))
    `(indium-repl-prompt-face ((,class :inherit modus-themes-prompt)))
    `(indium-repl-stdout-face ((,class :foreground ,fg-main)))
;;;;; info
    `(Info-quoted ((,class :inherit modus-themes-fixed-pitch ; the capitalization is canonical
                           :background ,bg-alt :foreground ,fg-special-calm)))
    `(info-header-node ((,class :inherit bold :foreground ,fg-alt)))
    `(info-header-xref ((,class :foreground ,blue-active)))
    `(info-index-match ((,class :inherit match)))
    `(info-menu-header ((,class :inherit modus-themes-heading-3)))
    `(info-menu-star ((,class :foreground ,red)))
    `(info-node ((,class :inherit bold)))
    `(info-title-1 ((,class :inherit modus-themes-heading-1)))
    `(info-title-2 ((,class :inherit modus-themes-heading-2)))
    `(info-title-3 ((,class :inherit modus-themes-heading-3)))
    `(info-title-4 ((,class :inherit modus-themes-heading-4)))
;;;;; info-colors
    `(info-colors-lisp-code-block ((,class :inherit fixed-pitch)))
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
    `(ioccur-title-face ((,class :inherit bold :foreground ,red-alt
                                 ,@(modus-themes--scale modus-themes-scale-4))))
;;;;; isearch, occur, and the like
    `(isearch ((,class :inherit (modus-themes-search-success bold))))
    `(isearch-fail ((,class :inherit modus-themes-refine-red)))
    `(isearch-group-1 ((,class :inherit modus-themes-refine-blue)))
    `(isearch-group-2 ((,class :inherit modus-themes-refine-magenta)))
    `(lazy-highlight ((,class :inherit modus-themes-search-success-lazy)))
    `(match ((,class :inherit modus-themes-special-calm)))
    `(query-replace ((,class :inherit (modus-themes-intense-yellow bold))))
;;;;; isl (isearch-light)
    `(isl-line ((,class :inherit ,@(modus-themes--success-deuteran
                                    'modus-themes-subtle-blue
                                    'modus-themes-subtle-green))))
    `(isl-match ((,class :inherit modus-themes-search-success-lazy)))
    `(isl-number ((,class :inherit (modus-themes-bold modus-themes-search-success-modeline))))
    `(isl-on ((,class :inherit (bold modus-themes-search-success))))
    `(isl-string ((,class :inherit modus-themes-bold :foreground ,cyan-active)))
;;;;; ivy
    `(ivy-action ((,class :inherit bold :foreground ,red-alt)))
    `(ivy-completions-annotations ((,class :inherit completions-annotations)))
    `(ivy-confirm-face ((,class :foreground ,cyan)))
    `(ivy-current-match ((,class ,@(modus-themes--extra-completions
                                    'modus-themes-refine-cyan
                                    'modus-themes-intense-cyan
                                    'modus-themes-special-cold
                                    nil
                                    'bold))))
    `(ivy-cursor ((,class :background ,fg-main :foreground ,bg-main)))
    `(ivy-grep-info ((,class :foreground ,cyan-alt)))
    `(ivy-grep-line-number ((,class :foreground ,fg-special-warm)))
    `(ivy-highlight-face ((,class :foreground ,magenta)))
    `(ivy-match-required-face ((,class :inherit error)))
    `(ivy-minibuffer-match-face-1 ((,class ,@(modus-themes--extra-completions
                                              'modus-themes-subtle-neutral
                                              'modus-themes-intense-neutral
                                              'modus-themes-nuanced-cyan
                                              fg-alt))))
    `(ivy-minibuffer-match-face-2 ((,class ,@(modus-themes--extra-completions
                                              'modus-themes-subtle-green
                                              'modus-themes-refine-green
                                              'modus-themes-nuanced-green
                                              green-alt-other
                                              'bold))))
    `(ivy-minibuffer-match-face-3 ((,class ,@(modus-themes--extra-completions
                                              'modus-themes-subtle-blue
                                              'modus-themes-refine-blue
                                              'modus-themes-nuanced-blue
                                              blue-alt-other
                                              'bold))))
    `(ivy-minibuffer-match-face-4 ((,class ,@(modus-themes--extra-completions
                                              'modus-themes-subtle-magenta
                                              'modus-themes-refine-magenta
                                              'modus-themes-nuanced-magenta
                                              magenta-alt-other
                                              'bold))))
    `(ivy-minibuffer-match-highlight ((,class ,@(modus-themes--extra-completions
                                                 'modus-themes-subtle-cyan
                                                 'modus-themes-intense-cyan
                                                 'modus-themes-nuanced-cyan
                                                 cyan-alt-other
                                                 'bold))))
    `(ivy-modified-buffer ((,class :inherit modus-themes-slant :foreground ,yellow)))
    `(ivy-modified-outside-buffer ((,class :inherit modus-themes-slant :foreground ,yellow-alt)))
    `(ivy-org ((,class :foreground ,cyan-alt-other)))
    `(ivy-prompt-match ((,class :inherit ivy-current-match)))
    `(ivy-remote ((,class :foreground ,magenta)))
    `(ivy-separator ((,class :inherit shadow)))
    `(ivy-subdir ((,class :foreground ,blue-alt-other)))
    `(ivy-virtual ((,class :foreground ,magenta-alt-other)))
    `(ivy-yanked-word ((,class ,@(modus-themes--extra-completions
                                  'modus-themes-subtle-blue
                                  'modus-themes-refine-blue
                                  'modus-themes-nuanced-blue
                                  blue-alt))))
;;;;; ivy-posframe
    `(ivy-posframe ((,class :background ,bg-dim :foreground ,fg-main)))
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
    `(keycast-key ((,class :background ,blue-active :foreground ,bg-main)))
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
;;;;; line numbers (display-line-numbers-mode and global variant)
    `(line-number
      ((,class :inherit default
               ,@(modus-themes--line-numbers
                  fg-alt bg-dim
                  fg-unfocused))))
    `(line-number-current-line
      ((,class :inherit (bold default)
               ,@(modus-themes--line-numbers
                  fg-main bg-active
                  blue-alt-other))))
    `(line-number-major-tick
      ((,class :inherit (bold default)
               ,@(modus-themes--line-numbers
                  yellow-nuanced-fg yellow-nuanced-bg
                  red-alt))))
    `(line-number-minor-tick
      ((,class :inherit (bold default)
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
    `(lsp-lens-face ((,class :height 0.8 :foreground ,fg-alt)))
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
    `(magit-bisect-bad ((,class :foreground ,red-alt-other)))
    `(magit-bisect-good ((,class :foreground ,green-alt-other)))
    `(magit-bisect-skip ((,class :foreground ,yellow-alt-other)))
    `(magit-blame-date ((,class :foreground ,blue)))
    `(magit-blame-dimmed ((,class :inherit (shadow modus-themes-reset-hard))))
    `(magit-blame-hash ((,class :foreground ,fg-special-warm)))
    `(magit-blame-heading ((,class :inherit modus-themes-reset-hard :background ,bg-alt :extend t)))
    `(magit-blame-highlight ((,class :inherit modus-themes-nuanced-cyan)))
    `(magit-blame-margin ((,class :inherit (magit-blame-highlight modus-themes-reset-hard))))
    `(magit-blame-name ((,class :foreground ,magenta-alt-other)))
    `(magit-blame-summary ((,class :foreground ,cyan-alt-other)))
    `(magit-branch-current ((,class :foreground ,blue-alt-other :box t)))
    `(magit-branch-local ((,class :foreground ,blue-alt)))
    `(magit-branch-remote ((,class :foreground ,magenta-alt)))
    `(magit-branch-remote-head ((,class :foreground ,magenta-alt-other :box t)))
    `(magit-branch-upstream ((,class :inherit italic)))
    `(magit-cherry-equivalent ((,class :background ,bg-main :foreground ,magenta-intense)))
    `(magit-cherry-unmatched ((,class :background ,bg-main :foreground ,cyan-intense)))
    ;; NOTE: here we break from the pattern of inheriting from the
    ;; modus-themes-diff-* faces, though only for the standard actions,
    ;; not the highlighted ones.  This is because Magit's interaction
    ;; model relies on highlighting the current diff hunk.
    `(magit-diff-added ((,class ,@(modus-themes--diff
                                   bg-main blue-alt-other
                                   bg-diff-added fg-diff-added
                                   green-nuanced-bg fg-diff-added
                                   bg-diff-added-deuteran fg-diff-added-deuteran))))
    `(magit-diff-added-highlight ((,class :inherit modus-themes-diff-focus-added)))
    `(magit-diff-base ((,class ,@(modus-themes--diff
                                  bg-main yellow
                                  bg-diff-changed fg-diff-changed
                                  yellow-nuanced-bg fg-diff-changed))))
    `(magit-diff-base-highlight ((,class :inherit modus-themes-diff-focus-changed)))
    `(magit-diff-context ((,class ,@(unless (eq modus-themes-diffs 'bg-only) (list :foreground fg-unfocused)))))
    `(magit-diff-context-highlight ((,class ,@(modus-themes--diff
                                               bg-dim fg-dim
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
                                          bg-alt fg-alt
                                          bg-active fg-inactive
                                          bg-inactive fg-inactive
                                          bg-inactive fg-inactive
                                          t))))
    `(magit-diff-hunk-heading-highlight
      ((,class :inherit bold
               :background ,@(modus-themes--diff-deuteran bg-active bg-diff-heading)
               :foreground ,@(modus-themes--diff-deuteran fg-main fg-diff-heading))))
    `(magit-diff-hunk-heading-selection ((,class :inherit modus-themes-refine-blue)))
    `(magit-diff-hunk-region ((,class :inherit bold)))
    `(magit-diff-lines-boundary ((,class :background ,fg-main)))
    `(magit-diff-lines-heading ((,class :inherit modus-themes-refine-magenta)))
    `(magit-diff-removed ((,class ,@(modus-themes--diff
                                     bg-main red
                                     bg-diff-removed fg-diff-removed
                                     red-nuanced-bg fg-diff-removed))))
    `(magit-diff-removed-highlight ((,class :inherit modus-themes-diff-focus-removed)))
    `(magit-diffstat-added ((,class :foreground ,@(modus-themes--diff-deuteran blue green))))
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
    `(magit-mode-line-process ((,class :inherit bold :foreground ,blue-active)))
    `(magit-mode-line-process-error ((,class :inherit bold :foreground ,red-active)))
    `(magit-process-ng ((,class :inherit error)))
    `(magit-process-ok ((,class :inherit success)))
    `(magit-reflog-amend ((,class :background ,bg-main :foreground ,magenta-intense)))
    `(magit-reflog-checkout ((,class :background ,bg-main :foreground ,blue-intense)))
    `(magit-reflog-cherry-pick ((,class :background ,bg-main :foreground ,green-intense)))
    `(magit-reflog-commit ((,class :background ,bg-main :foreground ,green-intense)))
    `(magit-reflog-merge ((,class :background ,bg-main :foreground ,green-intense)))
    `(magit-reflog-other ((,class :background ,bg-main :foreground ,cyan-intense)))
    `(magit-reflog-rebase ((,class :background ,bg-main :foreground ,magenta-intense)))
    `(magit-reflog-remote ((,class :background ,bg-main :foreground ,cyan-intense)))
    `(magit-reflog-reset ((,class :background ,bg-main :foreground ,red-intense)))
    `(magit-refname ((,class :inherit shadow)))
    `(magit-refname-pullreq ((,class :inherit shadow)))
    `(magit-refname-stash ((,class :inherit shadow)))
    `(magit-refname-wip ((,class :inherit shadow)))
    `(magit-section ((,class :background ,bg-dim :foreground ,fg-main)))
    `(magit-section-heading ((,class :inherit bold :foreground ,cyan)))
    `(magit-section-heading-selection ((,class :inherit (modus-themes-refine-cyan bold))))
    `(magit-section-highlight ((,class :background ,bg-alt)))
    `(magit-sequence-done ((,class :foreground ,@(modus-themes--success-deuteran
                                                  blue
                                                  green))))
    `(magit-sequence-drop ((,class :foreground ,red-alt)))
    `(magit-sequence-exec ((,class :foreground ,magenta-alt)))
    `(magit-sequence-head ((,class :foreground ,cyan-alt)))
    `(magit-sequence-onto ((,class :inherit shadow)))
    `(magit-sequence-part ((,class :foreground ,yellow-alt)))
    `(magit-sequence-pick ((,class :foreground ,blue-alt)))
    `(magit-sequence-stop ((,class :foreground ,red)))
    `(magit-signature-bad ((,class :inherit bold :foreground ,red)))
    `(magit-signature-error ((,class :foreground ,red-alt)))
    `(magit-signature-expired ((,class :foreground ,yellow)))
    `(magit-signature-expired-key ((,class :foreground ,yellow)))
    `(magit-signature-good ((,class :foreground ,@(modus-themes--success-deuteran
                                                   blue
                                                   green))))
    `(magit-signature-revoked ((,class :foreground ,magenta)))
    `(magit-signature-untrusted ((,class :foreground ,cyan)))
    `(magit-tag ((,class :foreground ,yellow-alt-other)))
;;;;; magit-imerge
    `(magit-imerge-overriding-value ((,class :inherit bold :foreground ,red-alt)))
;;;;; make-mode (makefiles)
    `(makefile-makepp-perl ((,class :background ,cyan-nuanced-bg)))
    `(makefile-space ((,class :background ,magenta-nuanced-bg)))
;;;;; man
    `(Man-overstrike ((,class :inherit bold :foreground ,magenta)))
    `(Man-reverse ((,class :inherit modus-themes-subtle-magenta)))
    `(Man-underline ((,class :foreground ,cyan :underline t)))
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
    ;; Here we make an exception of not applying the bespoke
    ;; `modus-themes-key-binding' for two reasons: (1) completion
    ;; highlights can be fairly intense, so we do not want more
    ;; components to compete with them for attention, (2) the
    ;; `marginalia-key' may not be used for key bindings specifically,
    ;; so we might end up applying styles in places we should not.
    `(marginalia-function ((,class :foreground ,magenta-alt-faint)))
    `(marginalia-key ((,class :foreground ,magenta-alt-other)))
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
    `(markdown-gfm-checkbox-face ((,class :foreground ,cyan-alt-other)))
    `(markdown-header-delimiter-face ((,class :inherit modus-themes-bold :foreground ,fg-dim)))
    `(markdown-header-face ((t nil)))
    `(markdown-header-face-1 ((,class :inherit modus-themes-heading-1)))
    `(markdown-header-face-2 ((,class :inherit modus-themes-heading-2)))
    `(markdown-header-face-3 ((,class :inherit modus-themes-heading-3)))
    `(markdown-header-face-4 ((,class :inherit modus-themes-heading-4)))
    `(markdown-header-face-5 ((,class :inherit modus-themes-heading-5)))
    `(markdown-header-face-6 ((,class :inherit modus-themes-heading-6)))
    `(markdown-header-rule-face ((,class :inherit bold :foreground ,fg-special-warm)))
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
    `(markdown-inline-code-face ((,class :inherit modus-themes-fixed-pitch
                                         :background ,bg-alt :foreground ,fg-special-calm)))
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
    `(markup-complex-replacement-face ((,class :background ,magenta-nuanced-bg
                                               :foreground ,magenta-alt-other
                                               :underline ,magenta-alt-other)))
    `(markup-emphasis-face ((,class :inherit markup-italic-face)))
    `(markup-error-face ((,class :inherit error)))
    `(markup-gen-face ((,class :foreground ,magenta-alt)))
    `(markup-internal-reference-face ((,class :foreground ,fg-alt :underline ,bg-region)))
    `(markup-italic-face ((,class :inherit italic)))
    `(markup-list-face ((,class :inherit modus-themes-special-cold)))
    `(markup-meta-face ((,class :inherit shadow)))
    `(markup-meta-hide-face ((,class :foreground "gray50")))
    `(markup-reference-face ((,class :foreground ,blue-alt :underline ,bg-region)))
    `(markup-replacement-face ((,class :inherit fixed-pitch :foreground ,red-alt)))
    `(markup-secondary-text-face ((,class :height 0.9 :foreground ,cyan-alt-other)))
    `(markup-small-face ((,class :inherit markup-gen-face :height 0.9)))
    `(markup-strong-face ((,class :inherit markup-bold-face)))
    `(markup-subscript-face ((,class :height 0.9 :foreground ,magenta-alt-other)))
    `(markup-superscript-face ((,class :height 0.9 :foreground ,magenta-alt-other)))
    `(markup-table-cell-face ((,class :inherit modus-themes-subtle-neutral)))
    `(markup-table-face ((,class :inherit modus-themes-subtle-neutral)))
    `(markup-table-row-face ((,class :inherit modus-themes-special-cold)))
    `(markup-title-0-face ((,class :inherit (bold modus-themes-variable-pitch)
                                   :foreground ,blue-nuanced-fg
                                   ,@(modus-themes--scale modus-themes-scale-title))))
    `(markup-title-1-face ((,class :inherit (bold modus-themes-variable-pitch)
                                   :foreground ,blue-nuanced-fg
                                   ,@(modus-themes--scale modus-themes-scale-1))))
    `(markup-title-2-face ((,class :inherit (bold modus-themes-variable-pitch)
                                   :foreground ,blue-nuanced-fg
                                   ,@(modus-themes--scale modus-themes-scale-2))))
    `(markup-title-3-face ((,class :inherit (bold modus-themes-variable-pitch)
                                   :foreground ,blue-nuanced-fg
                                   ,@(modus-themes--scale modus-themes-scale-3))))
    `(markup-title-4-face ((,class :inherit (bold modus-themes-variable-pitch)
                                   :foreground ,blue-nuanced-fg
                                   ,@(modus-themes--scale modus-themes-scale-4))))
    `(markup-title-5-face ((,class :inherit (bold modus-themes-variable-pitch)
                                   :foreground ,blue-nuanced-fg)))
    `(markup-verbatim-face ((,class :background ,bg-alt)))
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
    `(message-cited-text-1 ((,class ,@(modus-themes--mail-cite blue-faint fg-alt))))
    `(message-cited-text-2 ((,class ,@(modus-themes--mail-cite green-faint fg-comment-yellow))))
    `(message-cited-text-3 ((,class ,@(modus-themes--mail-cite red-faint fg-special-cold))))
    `(message-cited-text-4 ((,class ,@(modus-themes--mail-cite yellow-faint fg-special-calm))))
    `(message-header-cc ((,class :foreground ,blue-alt-other)))
    `(message-header-name ((,class :inherit bold :foreground ,cyan)))
    `(message-header-newsgroups ((,class :inherit message-header-other)))
    `(message-header-other ((,class :foreground ,fg-special-calm)))
    `(message-header-subject ((,class :inherit bold :foreground ,magenta-alt)))
    `(message-header-to ((,class :inherit bold :foreground ,magenta-alt-other)))
    `(message-header-xheader ((,class :foreground ,blue-alt)))
    `(message-mml ((,class :foreground ,cyan-alt-other)))
    `(message-separator ((,class :inherit modus-themes-intense-neutral)))
;;;;; minibuffer-line
    `(minibuffer-line ((,class :foreground ,fg-main)))
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
    `(mode-line ((,class ,@(modus-themes--variable-pitch-ui)
                         ,@(modus-themes--mode-line-attrs
                            fg-active bg-active
                            fg-dim bg-active
                            fg-main bg-active-accent
                            fg-alt bg-active
                            'alt-style bg-main))))
    `(mode-line-buffer-id ((,class :inherit bold)))
    `(mode-line-emphasis ((,class :inherit bold :foreground ,blue-active)))
    `(mode-line-highlight ((,class :inherit modus-themes-active-blue :box (:line-width -1 :style pressed-button))))
    `(mode-line-inactive ((,class ,@(modus-themes--variable-pitch-ui)
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
    `(mood-line-status-success ((,class :foreground ,@(modus-themes--success-deuteran
                                                       blue-active
                                                       green-active))))
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
    `(mu4e-replied-face ((,class :foreground ,blue)))
    `(mu4e-special-header-value-face ((,class :inherit message-header-subject)))
    `(mu4e-system-face ((,class :inherit modus-themes-slant :foreground ,fg-mark-del)))
    `(mu4e-title-face ((,class :foreground ,fg-main)))
    `(mu4e-trashed-face ((,class :foreground ,red)))
    `(mu4e-unread-face ((,class :inherit bold)))
    `(mu4e-url-number-face ((,class :foreground ,fg-alt)))
    `(mu4e-view-body-face ((,class :foreground ,fg-main)))
    `(mu4e-warning-face ((,class :inherit warning)))
;;;;; mu4e-conversation
    `(mu4e-conversation-header ((,class :inherit modus-themes-special-cold)))
    `(mu4e-conversation-sender-1 ((,class :foreground ,fg-special-warm)))
    `(mu4e-conversation-sender-2 ((,class :foreground ,fg-special-cold)))
    `(mu4e-conversation-sender-3 ((,class :foreground ,fg-special-mild)))
    `(mu4e-conversation-sender-4 ((,class :inherit shadow)))
    `(mu4e-conversation-sender-5 ((,class :foreground ,yellow-refine-fg)))
    `(mu4e-conversation-sender-6 ((,class :foreground ,cyan-refine-fg)))
    `(mu4e-conversation-sender-7 ((,class :foreground ,green-refine-fg)))
    `(mu4e-conversation-sender-8 ((,class :foreground ,blue-refine-fg)))
    `(mu4e-conversation-sender-me ((,class :foreground ,fg-main)))
    `(mu4e-conversation-unread ((,class :inherit bold)))
;;;;; multiple-cursors
    `(mc/cursor-bar-face ((,class :height 1 :background ,fg-main)))
    `(mc/cursor-face ((,class :inverse-video t)))
    `(mc/region-face ((,class :inherit region)))
;;;;; neotree
    `(neo-banner-face ((,class :foreground ,magenta)))
    `(neo-button-face ((,class :inherit button)))
    `(neo-dir-link-face ((,class :inherit bold :foreground ,blue)))
    `(neo-expand-btn-face ((,class :foreground ,cyan)))
    `(neo-file-link-face ((,class :foreground ,fg-main)))
    `(neo-header-face ((,class :inherit bold :foreground ,fg-main)))
    `(neo-root-dir-face ((,class :inherit bold :foreground ,cyan-alt)))
    `(neo-vc-added-face ((,class :foreground ,@(modus-themes--diff-deuteran blue green))))
    `(neo-vc-conflict-face ((,class :inherit bold :foreground ,red)))
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
;;;;; no-emoji
    `(no-emoji ((,class :foreground ,cyan)))
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
;;;;; objed
    `(objed-hl ((,class :background ,(if modus-themes-hl-line bg-hl-alt-intense bg-hl-alt))))
    `(objed-mark ((,class :background ,bg-active)))
    `(objed-mode-line ((,class :foreground ,cyan-active)))
;;;;; orderless
    `(orderless-match-face-0 ((,class :inherit bold
                                      ,@(modus-themes--standard-completions
                                         blue-alt-other blue-nuanced-bg
                                         blue-refine-bg blue-refine-fg))))
    `(orderless-match-face-1 ((,class :inherit bold
                                      ,@(modus-themes--standard-completions
                                         magenta-alt magenta-nuanced-bg
                                         magenta-refine-bg magenta-refine-fg))))
    `(orderless-match-face-2 ((,class :inherit bold
                                      ,@(modus-themes--standard-completions
                                         green green-nuanced-bg
                                         green-refine-bg green-refine-fg))))
    `(orderless-match-face-3 ((,class :inherit bold
                                      ,@(modus-themes--standard-completions
                                         yellow yellow-nuanced-bg
                                         yellow-refine-bg yellow-refine-fg))))
;;;;; org
    `(org-agenda-calendar-event ((,class ,@(modus-themes--agenda-event blue-alt))))
    `(org-agenda-calendar-sexp ((,class :inherit org-agenda-calendar-event)))
    `(org-agenda-clocking ((,class :inherit modus-themes-special-cold :extend t)))
    `(org-agenda-column-dateline ((,class :background ,bg-alt)))
    `(org-agenda-current-time ((,class :foreground ,blue-alt-other-faint)))
    `(org-agenda-date ((,class ,@(modus-themes--agenda-date cyan fg-main))))
    `(org-agenda-date-today ((,class ,@(modus-themes--agenda-date blue-active fg-main
                                                                  cyan-active fg-main
                                                                  bg-active t t))))
    `(org-agenda-date-weekend ((,class ,@(modus-themes--agenda-date cyan-alt-other fg-alt
                                                                    cyan fg-main))))
    `(org-agenda-diary ((,class :inherit org-agenda-calendar-event)))
    `(org-agenda-dimmed-todo-face ((,class :inherit shadow)))
    `(org-agenda-done ((,class :foreground ,@(modus-themes--success-deuteran
                                              blue-nuanced-fg
                                              green-nuanced-fg))))
    `(org-agenda-filter-category ((,class :inherit bold :foreground ,cyan-active)))
    `(org-agenda-filter-effort ((,class :inherit bold :foreground ,cyan-active)))
    `(org-agenda-filter-regexp ((,class :inherit bold :foreground ,cyan-active)))
    `(org-agenda-filter-tags ((,class :inherit bold :foreground ,cyan-active)))
    `(org-agenda-restriction-lock ((,class :background ,bg-dim :foreground ,fg-dim)))
    `(org-agenda-structure ((,class ,@(modus-themes--agenda-structure blue-alt))))
    `(org-archived ((,class :background ,bg-alt :foreground ,fg-alt)))
    `(org-block ((,class :inherit modus-themes-fixed-pitch
                         ,@(modus-themes--org-block bg-dim fg-main))))
    `(org-block-begin-line ((,class :inherit modus-themes-fixed-pitch
                                    ,@(modus-themes--org-block-delim
                                       bg-dim fg-special-cold
                                       bg-alt fg-alt))))
    `(org-block-end-line ((,class :inherit org-block-begin-line)))
    `(org-checkbox ((,class :box (:line-width 1 :color ,bg-active)
                            :background ,bg-inactive :foreground ,fg-active)))
    `(org-checkbox-statistics-done ((,class :inherit org-done)))
    `(org-checkbox-statistics-todo ((,class :inherit org-todo)))
    `(org-clock-overlay ((,class :inherit modus-themes-special-cold)))
    `(org-code ((,class :inherit modus-themes-fixed-pitch
                        :background ,bg-alt :foreground ,fg-special-mild
                        :extend t)))
    `(org-column ((,class :background ,bg-alt)))
    `(org-column-title ((,class :inherit bold :underline t :background ,bg-alt)))
    `(org-date ((,class :inherit ,(if modus-themes-no-mixed-fonts
                                      'button
                                    '(button fixed-pitch))
                        ,@(modus-themes--link-color
                           cyan cyan-faint))))
    `(org-date-selected ((,class :inherit bold :foreground ,blue-alt :inverse-video t)))
    `(org-dispatcher-highlight ((,class :inherit (bold modus-themes-mark-alt))))
    `(org-document-info ((,class :foreground ,fg-special-cold)))
    `(org-document-info-keyword ((,class :inherit modus-themes-fixed-pitch :foreground ,fg-alt)))
    `(org-document-title ((,class :inherit (bold modus-themes-variable-pitch) :foreground ,fg-special-cold
                                  ,@(modus-themes--scale modus-themes-scale-title))))
    `(org-done ((,class :foreground ,@(modus-themes--success-deuteran blue green))))
    `(org-drawer ((,class :inherit modus-themes-fixed-pitch :foreground ,fg-alt)))
    `(org-ellipsis (())) ; inherits from the heading's color
    `(org-footnote ((,class :inherit button
                            ,@(modus-themes--link-color
                               blue-alt blue-alt-faint))))
    `(org-formula ((,class :inherit modus-themes-fixed-pitch :foreground ,red-alt)))
    `(org-habit-alert-face ((,class ,@(modus-themes--agenda-habit
                                       yellow-graph-0-bg
                                       yellow-graph-0-bg
                                       yellow-graph-1-bg))))
    `(org-habit-alert-future-face ((,class ,@(modus-themes--agenda-habit
                                              yellow-graph-1-bg
                                              yellow-graph-0-bg
                                              yellow-graph-1-bg))))
    `(org-habit-clear-face ((,class ,@(modus-themes--agenda-habit
                                       blue-graph-0-bg
                                       green-graph-1-bg
                                       blue-graph-1-bg
                                       blue-graph-1-bg))))
    `(org-habit-clear-future-face ((,class ,@(modus-themes--agenda-habit
                                              blue-graph-1-bg
                                              green-graph-1-bg
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
                                       blue-graph-0-bg))))
    `(org-habit-ready-future-face ((,class ,@(modus-themes--agenda-habit
                                              green-graph-1-bg
                                              green-graph-0-bg
                                              green-graph-1-bg
                                              blue-graph-0-bg))))
    `(org-headline-done ((,class :inherit modus-themes-variable-pitch
                                 :foreground ,@(modus-themes--success-deuteran
                                                blue-nuanced-fg
                                                green-nuanced-fg))))
    `(org-headline-todo ((,class :inherit modus-themes-variable-pitch :foreground ,red-nuanced-fg)))
    `(org-hide ((,class :foreground ,bg-main)))
    `(org-indent ((,class :inherit (fixed-pitch org-hide))))
    `(org-latex-and-related ((,class :foreground ,magenta-refine-fg)))
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
    `(org-macro ((,class :inherit modus-themes-fixed-pitch
                         :background ,cyan-nuanced-bg :foreground ,cyan-nuanced-fg)))
    `(org-meta-line ((,class :inherit modus-themes-fixed-pitch :foreground ,fg-alt)))
    `(org-mode-line-clock ((,class :foreground ,fg-main)))
    `(org-mode-line-clock-overrun ((,class :inherit bold :foreground ,red-active)))
    `(org-priority ((,class :foreground ,magenta)))
    `(org-property-value ((,class :inherit modus-themes-fixed-pitch :foreground ,fg-special-cold)))
    `(org-quote ((,class ,@(modus-themes--org-block bg-dim fg-special-cold fg-main))))
    `(org-scheduled ((,class ,@(modus-themes--agenda-scheduled yellow-faint fg-special-warm magenta-alt))))
    `(org-scheduled-previously ((,class ,@(modus-themes--agenda-scheduled yellow fg-special-warm yellow-alt-other))))
    `(org-scheduled-today ((,class ,@(modus-themes--agenda-scheduled yellow fg-special-warm magenta-alt-other))))
    `(org-sexp-date ((,class :inherit org-date)))
    `(org-special-keyword ((,class :inherit modus-themes-fixed-pitch :foreground ,fg-alt)))
    `(org-table ((,class :inherit modus-themes-fixed-pitch :foreground ,fg-special-cold)))
    `(org-table-header ((,class :inherit (fixed-pitch modus-themes-intense-neutral))))
    `(org-tag ((,class :foreground ,magenta-nuanced-fg)))
    `(org-tag-group ((,class :inherit bold :foreground ,cyan-nuanced-fg)))
    `(org-target ((,class :underline t)))
    `(org-time-grid ((,class :foreground ,fg-unfocused)))
    `(org-todo ((,class :foreground ,red)))
    `(org-upcoming-deadline ((,class :foreground ,red-alt-other)))
    `(org-upcoming-distant-deadline ((,class :foreground ,red-faint)))
    `(org-verbatim ((,class :inherit modus-themes-fixed-pitch
                            :background ,bg-alt :foreground ,fg-special-calm)))
    `(org-verse ((,class :inherit org-quote)))
    `(org-warning ((,class :inherit bold :foreground ,red-alt-other)))
;;;;; org-journal
    `(org-journal-calendar-entry-face ((,class :inherit modus-themes-slant :foreground ,yellow-alt-other)))
    `(org-journal-calendar-scheduled-face ((,class :inherit modus-themes-slant :foreground ,red-alt-other)))
    `(org-journal-highlight ((,class :foreground ,magenta-alt)))
;;;;; org-noter
    `(org-noter-no-notes-exist-face ((,class :inherit bold :foreground ,red-active)))
    `(org-noter-notes-exist-face ((,class :inherit bold :foreground ,green-active)))
;;;;; org-pomodoro
    `(org-pomodoro-mode-line ((,class :foreground ,red-active)))
    `(org-pomodoro-mode-line-break ((,class :foreground ,cyan-active)))
    `(org-pomodoro-mode-line-overtime ((,class :inherit bold :foreground ,red-active)))
;;;;; org-recur
    `(org-recur ((,class :foreground ,magenta-active)))
;;;;; org-roam
    `(org-roam-link ((,class :inherit button
                             ,@(modus-themes--link-color
                                green green-faint))))
    `(org-roam-link-current ((,class :inherit button
                                     ,@(modus-themes--link-color
                                        green-alt green-alt-faint))))
    `(org-roam-link-invalid ((,class :inherit button
                                     ,@(modus-themes--link-color
                                        red red-faint))))
    `(org-roam-link-shielded ((,class :inherit button
                                      ,@(modus-themes--link-color
                                         yellow yellow-faint))))
    `(org-roam-tag ((,class :inherit italic :foreground ,fg-alt)))
;;;;; org-superstar
    `(org-superstar-item ((,class :foreground ,fg-main)))
    `(org-superstar-leading ((,class :foreground ,fg-whitespace)))
;;;;; org-table-sticky-header
    `(org-table-sticky-header-face ((,class :inherit modus-themes-intense-neutral)))
;;;;; org-tree-slide
    `(org-tree-slide-header-overlay-face
      ((,class :inherit (bold modus-themes-variable-pitch) :background ,bg-main
               :foreground ,fg-special-cold :overline nil
               ,@(modus-themes--scale modus-themes-scale-title))))
;;;;; org-treescope
    `(org-treescope-faces--markerinternal-midday ((,class :inherit modus-themes-intense-blue)))
    `(org-treescope-faces--markerinternal-range ((,class :inherit modus-themes-special-mild)))
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
    `(package-help-section-name ((,class :inherit bold :foreground ,magenta-alt-other)))
    `(package-name ((,class :inherit button)))
    `(package-status-avail-obso ((,class :inherit bold :foreground ,red)))
    `(package-status-available ((,class :foreground ,fg-special-mild)))
    `(package-status-built-in ((,class :foreground ,magenta)))
    `(package-status-dependency ((,class :foreground ,magenta-alt-other)))
    `(package-status-disabled ((,class :inherit modus-themes-subtle-red)))
    `(package-status-external ((,class :foreground ,cyan-alt-other)))
    `(package-status-held ((,class :foreground ,yellow-alt)))
    `(package-status-incompat ((,class :inherit bold :foreground ,yellow)))
    `(package-status-installed ((,class :foreground ,fg-special-warm)))
    `(package-status-new ((,class :inherit bold :foreground ,green)))
    `(package-status-unsigned ((,class :inherit bold :foreground ,red-alt)))
;;;;; page-break-lines
    `(page-break-lines ((,class :inherit default :foreground ,fg-window-divider-outer)))
;;;;; pandoc-mode
    `(pandoc-citation-key-face ((,class :background ,bg-dim :foreground ,magenta-alt)))
    `(pandoc-directive-@@-face ((,class :background ,bg-dim :foreground ,blue-alt-other)))
    `(pandoc-directive-braces-face ((,class :foreground ,blue-alt-other)))
    `(pandoc-directive-contents-face ((,class :foreground ,cyan-alt-other)))
    `(pandoc-directive-type-face ((,class :foreground ,magenta)))
;;;;; paradox
    `(paradox-archive-face ((,class :foreground ,fg-special-mild)))
    `(paradox-comment-face ((,class :inherit font-lock-comment-face)))
    `(paradox-commit-tag-face ((,class :inherit modus-themes-refine-magenta :box t)))
    `(paradox-description-face ((,class :foreground ,fg-special-cold)))
    `(paradox-description-face-multiline ((,class :foreground ,fg-special-cold)))
    `(paradox-download-face ((,class :inherit modus-themes-bold :foreground ,blue-alt-other)))
    `(paradox-highlight-face ((,class :inherit modus-themes-bold :foreground ,cyan-alt-other)))
    `(paradox-homepage-button-face ((,class :foreground ,magenta-alt-other :underline t)))
    `(paradox-mode-line-face ((,class :inherit bold :foreground ,cyan-active)))
    `(paradox-name-face ((,class :foreground ,blue :underline t)))
    `(paradox-star-face ((,class :foreground ,magenta)))
    `(paradox-starred-face ((,class :foreground ,magenta-alt)))
;;;;; paren-face
    `(parenthesis ((,class :foreground ,fg-unfocused)))
;;;;; parrot
    `(parrot-rotate-rotation-highlight-face ((,class :inherit modus-themes-refine-magenta)))
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
    `(phi-grep-heading-face  ((,class :inherit bold :foreground ,red-alt
                                      ,@(modus-themes--scale modus-themes-scale-4))))
    `(phi-grep-line-number-face ((,class :foreground ,fg-special-warm)))
    `(phi-grep-match-face ((,class :inherit modus-themes-special-calm)))
    `(phi-grep-modified-face ((,class :inherit modus-themes-refine-yellow)))
    `(phi-grep-overlay-face ((,class :inherit modus-themes-refine-blue)))
;;;;; phi-search
    `(phi-replace-preview-face ((,class :inherit modus-themes-intense-magenta)))
    `(phi-search-failpart-face ((,class :inherit modus-themes-refine-red)))
    `(phi-search-match-face ((,class :inherit modus-themes-search-success-lazy)))
    `(phi-search-selection-face ((,class :inherit (modus-themes-search-success bold))))
;;;;; pkgbuild-mode
    `(pkgbuild-error-face ((,class :inherit modus-themes-lang-error)))
;;;;; pomidor
    `(pomidor-break-face ((,class :foreground ,blue-alt-other)))
    `(pomidor-overwork-face ((,class :foreground ,red-alt-other)))
    `(pomidor-skip-face ((,class :inherit modus-themes-slant :foreground ,fg-alt)))
    `(pomidor-work-face ((,class :foreground ,@(modus-themes--success-deuteran
                                                blue-alt
                                                green-alt-other))))
;;;;; popup
    `(popup-face ((,class :background ,bg-alt :foreground ,fg-main)))
    `(popup-isearch-match ((,class :inherit (modus-themes-refine-cyan bold))))
    `(popup-menu-mouse-face ((,class :inherit modus-themes-intense-blue)))
    `(popup-menu-selection-face ((,class :inherit (modus-themes-subtle-cyan bold))))
    `(popup-scroll-bar-background-face ((,class :background ,bg-active)))
    `(popup-scroll-bar-foreground-face ((,class :foreground ,fg-active)))
    `(popup-summary-face ((,class :background ,bg-active :foreground ,fg-inactive)))
    `(popup-tip-face ((,class :inherit modus-themes-refine-yellow)))
;;;;; powerline
    `(powerline-active0 ((,class :background ,bg-main :foreground ,blue-faint :inverse-video t)))
    `(powerline-active1 ((,class :background ,blue-nuanced-bg :foreground ,blue-nuanced-fg)))
    `(powerline-active2 ((,class :background ,bg-active :foreground ,fg-active)))
    `(powerline-inactive0 ((,class :background ,bg-special-cold :foreground ,fg-special-cold)))
    `(powerline-inactive1 ((,class :background ,bg-dim :foreground ,fg-inactive)))
    `(powerline-inactive2 ((,class :background ,bg-inactive :foreground ,fg-inactive)))
;;;;; powerline-evil
    `(powerline-evil-base-face ((,class :background ,fg-main :foreground ,bg-main)))
    `(powerline-evil-emacs-face ((,class :inherit modus-themes-active-magenta)))
    `(powerline-evil-insert-face ((,class :inherit modus-themes-active-green)))
    `(powerline-evil-motion-face ((,class :inherit modus-themes-active-blue)))
    `(powerline-evil-normal-face ((,class :background ,fg-alt :foreground ,bg-main)))
    `(powerline-evil-operator-face ((,class :inherit modus-themes-active-yellow)))
    `(powerline-evil-replace-face ((,class :inherit modus-themes-active-red)))
    `(powerline-evil-visual-face ((,class :inherit modus-themes-active-cyan)))
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
    `(racket-logger-config-face ((,class :inherit modus-themes-slant :foreground ,fg-alt)))
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
;;;;; rainbow-identifiers
    `(rainbow-identifiers-identifier-1 ((,class :foreground ,green-alt-other)))
    `(rainbow-identifiers-identifier-2 ((,class :foreground ,magenta-alt-other)))
    `(rainbow-identifiers-identifier-3 ((,class :foreground ,cyan-alt-other)))
    `(rainbow-identifiers-identifier-4 ((,class :foreground ,yellow-alt-other)))
    `(rainbow-identifiers-identifier-5 ((,class :foreground ,blue-alt-other)))
    `(rainbow-identifiers-identifier-6 ((,class :foreground ,green-alt)))
    `(rainbow-identifiers-identifier-7 ((,class :foreground ,magenta-alt)))
    `(rainbow-identifiers-identifier-8 ((,class :foreground ,cyan-alt)))
    `(rainbow-identifiers-identifier-9 ((,class :foreground ,yellow-alt)))
    `(rainbow-identifiers-identifier-10 ((,class :foreground ,green)))
    `(rainbow-identifiers-identifier-11 ((,class :foreground ,magenta)))
    `(rainbow-identifiers-identifier-12 ((,class :foreground ,cyan)))
    `(rainbow-identifiers-identifier-13 ((,class :foreground ,yellow)))
    `(rainbow-identifiers-identifier-14 ((,class :foreground ,blue-alt)))
    `(rainbow-identifiers-identifier-15 ((,class :foreground ,red-alt)))
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
    `(rcirc-bright-nick ((,class :inherit bold :foreground ,magenta-alt)))
    `(rcirc-dim-nick ((,class :inherit shadow)))
    `(rcirc-my-nick ((,class :inherit bold :foreground ,magenta)))
    `(rcirc-nick-in-message ((,class :foreground ,magenta-alt-other)))
    `(rcirc-nick-in-message-full-line ((,class :inherit bold :foreground ,fg-special-mild)))
    `(rcirc-other-nick ((,class :inherit bold :foreground ,fg-special-cold)))
    `(rcirc-prompt ((,class :inherit modus-themes-prompt)))
    `(rcirc-server ((,class :foreground ,fg-unfocused)))
    `(rcirc-timestamp ((,class :foreground ,blue-nuanced-fg)))
    `(rcirc-url ((,class :foreground ,blue :underline t)))
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
;;;;; sallet
    `(sallet-buffer-compressed ((,class :inherit italic :foreground ,yellow-nuanced-fg)))
    `(sallet-buffer-default-directory ((,class :foreground ,cyan-nuanced-fg)))
    `(sallet-buffer-directory ((,class :foreground ,blue-nuanced-fg)))
    `(sallet-buffer-help ((,class :foreground ,fg-special-cold)))
    `(sallet-buffer-modified ((,class :inherit italic :foreground ,yellow-alt-other)))
    `(sallet-buffer-ordinary ((,class :foreground ,fg-main)))
    `(sallet-buffer-read-only ((,class :foreground ,yellow-alt)))
    `(sallet-buffer-size ((,class :foreground ,fg-special-calm)))
    `(sallet-buffer-special ((,class :foreground ,magenta-alt-other)))
    `(sallet-flx-match ((,class ,@(modus-themes--extra-completions
                                   'modus-themes-subtle-cyan
                                   'modus-themes-refine-cyan
                                   'modus-themes-nuanced-cyan
                                   cyan-alt-other))))
    `(sallet-recentf-buffer-name ((,class :foreground ,blue-nuanced-fg)))
    `(sallet-recentf-file-path ((,class :foreground ,fg-special-mild)))
    `(sallet-regexp-match ((,class ,@(modus-themes--extra-completions
                                      'modus-themes-subtle-magenta
                                      'modus-themes-refine-magenta
                                      'modus-themes-nuanced-magenta
                                      magenta-alt-other))))
    `(sallet-source-header ((,class :inherit bold :foreground ,red-alt
                                    ,@(modus-themes--scale modus-themes-scale-4))))
    `(sallet-substring-match ((,class ,@(modus-themes--extra-completions
                                         'modus-themes-subtle-blue
                                         'modus-themes-refine-blue
                                         'modus-themes-nuanced-blue
                                         blue-alt-other))))
;;;;; selectrum
    ;; NOTE 2021-02-22: The `selectrum-primary-highlight' and
    ;; `selectrum-secondary-highlight' are deprecated upstream in favour
    ;; of their selectrum-prescient counterparts.  We shall remove those
    ;; faces from the themes once we are certain that they are no longer
    ;; relevant.
    `(selectrum-current-candidate
      ((,class :inherit bold :foreground ,fg-main
               :background ,@(pcase modus-themes-completions
                               ('opinionated (list bg-active))
                               (_ (list bg-inactive))))))
    `(selectrum-mouse-highlight ((,class :inherit highlight)))
    `(selectrum-primary-highlight
      ((,class :inherit bold
               ,@(modus-themes--standard-completions
                  magenta-alt magenta-nuanced-bg
                  magenta-refine-bg magenta-refine-fg))))
    `(selectrum-secondary-highlight
      ((,class :inherit bold
               ,@(modus-themes--standard-completions
                  cyan-alt-other cyan-nuanced-bg
                  cyan-refine-bg cyan-refine-fg))))
    `(selectrum-quick-keys-highlight
      ((,class :inherit modus-themes-refine-red)))
    `(selectrum-quick-keys-match
      ((,class :inherit (bold modus-themes-search-success))))
;;;;; selectrum-prescient
    `(selectrum-prescient-primary-highlight
      ((,class :inherit bold
               ,@(modus-themes--standard-completions
                  magenta-alt magenta-nuanced-bg
                  magenta-refine-bg magenta-refine-fg))))
    `(selectrum-prescient-secondary-highlight
      ((,class :inherit bold
               ,@(modus-themes--standard-completions
                  cyan-alt-other cyan-nuanced-bg
                  cyan-refine-bg cyan-refine-fg))))
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
    `(sesman-browser-button-face ((,class :foreground ,blue-alt-other :underline t)))
    `(sesman-browser-highligh-face ((,class :inherit modus-themes-subtle-blue)))
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
    `(shr-h1 ((,class :inherit modus-themes-heading-1)))
    `(shr-h2 ((,class :inherit modus-themes-heading-2)))
    `(shr-h3 ((,class :inherit modus-themes-heading-3)))
    `(shr-h4 ((,class :inherit modus-themes-heading-4)))
    `(shr-h5 ((,class :inherit modus-themes-heading-5)))
    `(shr-h6 ((,class :inherit modus-themes-heading-6)))
    `(shr-abbreviation ((,class :inherit modus-themes-lang-note)))
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
;;;;; solaire
    `(solaire-default-face ((,class :inherit default :background ,bg-alt :foreground ,fg-dim)))
    `(solaire-line-number-face ((,class :inherit solaire-default-face :foreground ,fg-unfocused)))
    `(solaire-hl-line-face ((,class :background ,bg-active)))
    `(solaire-org-hide-face ((,class :background ,bg-alt :foreground ,bg-alt)))
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
    `(speedbar-highlight-face ((,class :inherit modus-themes-subtle-blue)))
    `(speedbar-selected-face ((,class :inherit bold :foreground ,cyan)))
    `(speedbar-separator-face ((,class :inherit modus-themes-intense-neutral)))
    `(speedbar-tag-face ((,class :foreground ,yellow-alt-other)))
;;;;; spell-fu
    `(spell-fu-incorrect-face ((,class :inherit modus-themes-lang-error)))
;;;;; spray
    `(spray-accent-face ((,class :foreground ,red-intense)))
    `(spray-base-face ((,class :inherit default :foreground ,fg-special-cold)))
;;;;; stripes
    `(stripes ((,class :background ,bg-alt)))
;;;;; success
    `(suggest-heading ((,class :inherit bold :foreground ,yellow-alt-other)))
;;;;; switch-window
    `(switch-window-background ((,class :background ,bg-dim)))
    `(switch-window-label ((,class :height 3.0 :foreground ,blue-intense)))
;;;;; swiper
    `(swiper-background-match-face-1 ((,class :inherit modus-themes-subtle-neutral)))
    `(swiper-background-match-face-2 ((,class :inherit modus-themes-refine-cyan)))
    `(swiper-background-match-face-3 ((,class :inherit modus-themes-refine-magenta)))
    `(swiper-background-match-face-4 ((,class :inherit modus-themes-refine-yellow)))
    `(swiper-line-face ((,class :inherit modus-themes-special-cold)))
    `(swiper-match-face-1 ((,class :inherit (bold modus-themes-intense-neutral))))
    `(swiper-match-face-2 ((,class :inherit (bold modus-themes-intense-green))))
    `(swiper-match-face-3 ((,class :inherit (bold modus-themes-intense-blue))))
    `(swiper-match-face-4 ((,class :inherit (bold modus-themes-intense-red))))
;;;;; swoop
    `(swoop-face-header-format-line ((,class :inherit bold :foreground ,red-alt
                                             ,@(modus-themes--scale modus-themes-scale-3))))
    `(swoop-face-line-buffer-name ((,class :inherit bold :foreground ,blue-alt
                                           ,@(modus-themes--scale modus-themes-scale-4))))
    `(swoop-face-line-number ((,class :foreground ,fg-special-warm)))
    `(swoop-face-target-line ((,class :inherit modus-themes-intense-blue :extend t)))
    `(swoop-face-target-words ((,class :inherit modus-themes-refine-cyan)))
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
    `(sx-question-mode-title-comments ((,class :inherit bold :foreground ,fg-alt)))
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
    `(syslog-error ((,class :inherit bold :foreground ,red)))
    `(syslog-file ((,class :inherit bold :foreground ,fg-special-cold)))
    `(syslog-hide ((,class :background ,bg-main :foreground ,fg-main)))
    `(syslog-hour ((,class :inherit bold :foreground ,magenta-alt-other)))
    `(syslog-info ((,class :inherit bold :foreground ,blue-alt-other)))
    `(syslog-ip ((,class :inherit bold :foreground ,fg-special-mild :underline t)))
    `(syslog-su ((,class :inherit bold :foreground ,red-alt)))
    `(syslog-warn ((,class :inherit bold :foreground ,yellow)))
;;;;; tab-bar-groups
    `(tab-bar-groups-tab-1 ((,class ,@(modus-themes--variable-pitch-ui) :foreground ,blue-tab)))
    `(tab-bar-groups-tab-2 ((,class ,@(modus-themes--variable-pitch-ui) :foreground ,red-tab)))
    `(tab-bar-groups-tab-3 ((,class ,@(modus-themes--variable-pitch-ui) :foreground ,green-tab)))
    `(tab-bar-groups-tab-4 ((,class ,@(modus-themes--variable-pitch-ui) :foreground ,orange-tab)))
    `(tab-bar-groups-tab-5 ((,class ,@(modus-themes--variable-pitch-ui) :foreground ,purple-tab)))
    `(tab-bar-groups-tab-6 ((,class ,@(modus-themes--variable-pitch-ui) :foreground ,cyan-tab)))
    `(tab-bar-groups-tab-7 ((,class ,@(modus-themes--variable-pitch-ui) :foreground ,yellow-tab)))
    `(tab-bar-groups-tab-8 ((,class ,@(modus-themes--variable-pitch-ui) :foreground ,magenta-tab)))
;;;;; tab-bar-mode
    `(tab-bar ((,class ,@(modus-themes--tab bg-active bg-active-accent nil nil nil nil t))))
    `(tab-bar-tab-group-current ((,class ,@(modus-themes--tab bg-tab-active)
                                         :box (:line-width (2 . -2) :color "gray50"))))
    `(tab-bar-tab-group-inactive ((,class ,@(modus-themes--tab bg-tab-inactive bg-tab-inactive-accent fg-dim)
                                          :box (:line-width (2 . -2) :color "gray50"))))
    `(tab-bar-tab ((,class ,@(modus-themes--tab bg-tab-active nil nil nil t t))))
    `(tab-bar-tab-inactive ((,class ,@(modus-themes--tab bg-tab-inactive bg-tab-inactive-accent fg-dim nil t))))
;;;;; tab-line-mode
    `(tab-line ((,class ,@(modus-themes--tab bg-active bg-active-accent nil nil nil nil t)
                        :height 0.95)))
    `(tab-line-close-highlight ((,class :foreground ,red)))
    `(tab-line-highlight ((,class :inherit modus-themes-active-blue)))
    `(tab-line-tab ((,class ,@(modus-themes--tab bg-tab-active nil nil nil t t))))
    `(tab-line-tab-current ((,class :inherit tab-line-tab)))
    `(tab-line-tab-inactive ((,class ,@(modus-themes--tab bg-tab-inactive bg-tab-inactive-accent fg-dim nil t))))
    `(tab-line-tab-inactive-alternate ((,class ,@(modus-themes--tab bg-tab-inactive-alt
                                                                    bg-tab-inactive-alt-accent fg-main nil t))))
    `(tab-line-tab-modified ((,class :foreground ,red-alt-other-faint)))
;;;;; table (built-in table.el)
    `(table-cell ((,class :background ,blue-nuanced-bg)))
;;;;; telega
    ;; FIXME 2021-03-28: Some aspects of `telega' are not fully
    ;; supported or have not been tested thoroughly.  Please understand
    ;; that I do not use that service because it requires a smartphone
    ;; and I have none.  Help with testing is appreciated.
    `(telega-button ((,class :box t :foreground ,blue)))
    `(telega-button-active ((,class :box ,blue-intense-bg :background ,blue-intense-bg :foreground ,fg-main)))
    `(telega-button-highlight ((,class :inherit modus-themes-subtle-magenta)))
    `(telega-chat-prompt ((,class :inherit bold)))
    `(telega-entity-type-code ((,class :inherit fixed-pitch)))
    `(telega-entity-type-mention ((,class :foreground ,cyan)))
    `(telega-entity-type-pre ((,class :inherit fixed-pitch)))
    `(telega-msg-heading ((,class :background ,bg-alt)))
    `(telega-msg-self-title ((,class :inherit bold)))
    `(telega-root-heading ((,class :inherit modus-themes-subtle-neutral)))
    `(telega-secret-title ((,class :foreground ,magenta-alt)))
    `(telega-unmuted-count ((,class :foreground ,blue-alt-other)))
    `(telega-user-online-status ((,class :foreground ,cyan-active)))
    `(telega-username ((,class :foreground ,cyan-alt-other)))
    `(telega-webpage-chat-link ((,class :background ,bg-alt)))
    `(telega-webpage-fixed ((,class :inherit fixed-pitch :height 0.85)))
    `(telega-webpage-header ((,class :inherit modus-themes-variable-pitch :height 1.3)))
    `(telega-webpage-preformatted ((,class :inherit fixed-pitch :background ,bg-alt)))
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
;;;;; tomatinho
    `(tomatinho-ok-face ((,class :foreground ,blue-intense)))
    `(tomatinho-pause-face ((,class :foreground ,yellow-intense)))
    `(tomatinho-reset-face ((,class :inherit shadow)))
;;;;; transient
    `(transient-active-infix ((,class :inherit modus-themes-special-mild)))
    `(transient-amaranth ((,class :inherit bold :foreground ,yellow-alt)))
    `(transient-argument ((,class :inherit bold :foreground ,green)))
    `(transient-blue ((,class :inherit bold :foreground ,blue)))
    `(transient-disabled-suffix ((,class :inherit modus-themes-intense-red)))
    `(transient-enabled-suffix ((,class :inherit ,@(modus-themes--success-deuteran
                                                    'modus-themes-subtle-blue
                                                    'modus-themes-subtle-green))))
    `(transient-heading ((,class :inherit bold :foreground ,fg-main)))
    `(transient-inactive-argument ((,class :inherit shadow)))
    `(transient-inactive-value ((,class :inherit shadow)))
    ;; FIXME 2021-08-28: using `modus-themes-key-binding' leads to
    ;; misalignments because of the added box property.
    ;; `(transient-key ((,class :inherit modus-themes-key-binding)))
    `(transient-key ((,class :inherit bold :foreground ,blue-alt-other)))
    `(transient-mismatched-key ((,class :underline t)))
    `(transient-nonstandard-key ((,class :underline t)))
    `(transient-pink ((,class :inherit bold :foreground ,magenta-alt-faint)))
    `(transient-red ((,class :inherit bold :foreground ,red-faint)))
    `(transient-teal ((,class :inherit bold :foreground ,cyan-alt-other)))
    `(transient-unreachable ((,class :foreground ,fg-unfocused)))
    `(transient-unreachable-key ((,class :foreground ,fg-unfocused)))
    `(transient-value ((,class :inherit bold :foreground ,magenta-alt-other)))
;;;;; trashed
    `(trashed-deleted ((,class :inherit modus-themes-mark-del)))
    `(trashed-directory ((,class :foreground ,blue)))
    `(trashed-mark ((,class :inherit modus-themes-mark-symbol)))
    `(trashed-marked ((,class :inherit modus-themes-mark-alt)))
    `(trashed-restored ((,class :inherit modus-themes-mark-sel)))
    `(trashed-symlink ((,class :inherit button
                               ,@(modus-themes--link-color
                                  cyan-alt cyan-alt-faint))))
;;;;; treemacs
    `(treemacs-directory-collapsed-face ((,class :foreground ,magenta-alt)))
    `(treemacs-directory-face ((,class :inherit dired-directory)))
    `(treemacs-file-face ((,class :foreground ,fg-main)))
    `(treemacs-fringe-indicator-face ((,class :foreground ,fg-main)))
    `(treemacs-git-added-face ((,class :foreground ,green-intense)))
    `(treemacs-git-conflict-face ((,class :inherit (modus-themes-intense-red bold))))
    `(treemacs-git-ignored-face ((,class :inherit shadow)))
    `(treemacs-git-modified-face ((,class :foreground ,yellow-alt-other)))
    `(treemacs-git-renamed-face ((,class :foreground ,cyan-alt-other)))
    `(treemacs-git-unmodified-face ((,class :foreground ,fg-main)))
    `(treemacs-git-untracked-face ((,class :foreground ,red-alt-other)))
    `(treemacs-help-column-face ((,class :inherit modus-themes-bold :foreground ,magenta-alt-other :underline t)))
    `(treemacs-help-title-face ((,class :foreground ,blue-alt-other)))
    `(treemacs-on-failure-pulse-face ((,class :inherit modus-themes-intense-red)))
    `(treemacs-on-success-pulse-face ((,class :inherit ,@(modus-themes--success-deuteran
                                                          'modus-themes-intense-blue
                                                          'modus-themes-intense-green))))
    `(treemacs-root-face ((,class :inherit bold :foreground ,blue-alt-other :height 1.2 :underline t)))
    `(treemacs-root-remote-disconnected-face ((,class :inherit treemacs-root-remote-face :foreground ,yellow)))
    `(treemacs-root-remote-face ((,class :inherit treemacs-root-face :foreground ,magenta)))
    `(treemacs-root-remote-unreadable-face ((,class :inherit treemacs-root-unreadable-face)))
    `(treemacs-root-unreadable-face ((,class :inherit treemacs-root-face :strike-through t)))
    `(treemacs-tags-face ((,class :foreground ,blue-alt)))
    `(treemacs-tags-face ((,class :foreground ,magenta-alt)))
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
    `(vc-dir-status-ignored ((,class :foreground ,fg-unfocused)))
    `(vc-dir-status-up-to-date ((,class :foreground ,cyan)))
    `(vc-dir-status-warning ((,class :foreground ,red)))
    `(vc-conflict-state ((,class :inherit modus-themes-slant :foreground ,red-active)))
    `(vc-edited-state ((,class :foreground ,yellow-active)))
    `(vc-locally-added-state ((,class :foreground ,cyan-active)))
    `(vc-locked-state ((,class :foreground ,blue-active)))
    `(vc-missing-state ((,class :inherit modus-themes-slant :foreground ,magenta-active)))
    `(vc-needs-update-state ((,class :inherit modus-themes-slant :foreground ,green-active)))
    `(vc-removed-state ((,class :foreground ,red-active)))
    `(vc-state-base ((,class :foreground ,fg-active)))
    `(vc-up-to-date-state ((,class :foreground ,fg-special-cold)))
;;;;; vdiff
    `(vdiff-addition-face ((,class :inherit modus-themes-diff-added)))
    `(vdiff-change-face ((,class :inherit modus-themes-diff-changed)))
    `(vdiff-closed-fold-face ((,class :inherit modus-themes-diff-heading)))
    `(vdiff-refine-added ((,class :inherit modus-themes-diff-refine-added)))
    `(vdiff-refine-changed ((,class :inherit modus-themes-diff-refine-changed)))
    `(vdiff-subtraction-face ((,class :inherit modus-themes-diff-removed)))
    `(vdiff-target-face ((,class :inherit modus-themes-intense-blue)))
;;;;; vertico
    `(vertico-current ((,class :inherit bold :foreground ,fg-main
                               :background ,@(pcase modus-themes-completions
                                               ('opinionated (list bg-active))
                                               (_ (list bg-inactive))))))
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
;;;;; volatile-highlights
    `(vhl/default-face ((,class :background ,bg-alt :foreground ,blue-nuanced-fg :extend t)))
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
    `(wgrep-delete-face ((,class :inherit modus-themes-refine-yellow)))
    `(wgrep-done-face ((,class :inherit modus-themes-refine-blue)))
    `(wgrep-face ((,class :inherit modus-themes-refine-green)))
    `(wgrep-file-face ((,class :foreground ,fg-special-warm)))
    `(wgrep-reject-face ((,class :inherit (modus-themes-intense-red bold))))
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
    `(which-key-special-key-face ((,class :inherit bold :foreground ,orange-intense)))
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
    `(woman-bold ((,class :inherit bold :foreground ,magenta)))
    `(woman-italic ((,class :inherit italic :foreground ,cyan)))
    `(woman-unknown ((,class :inherit italic :foreground ,yellow)))
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
    `(ztreep-diff-model-add-face ((,class :foreground ,@(modus-themes--diff-deuteran blue green))))
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
;;;; exwm
    `(exwm-floating-border-color ,fg-window-divider-inner)
;;;; flymake fringe indicators
    `(flymake-error-bitmap '(flymake-double-exclamation-mark modus-themes-fringe-red))
    `(flymake-warning-bitmap '(exclamation-mark modus-themes-fringe-yellow))
    `(flymake-note-bitmap '(exclamation-mark modus-themes-fringe-cyan))
;;;; ibuffer
    `(ibuffer-deletion-face 'modus-themes-mark-del)
    `(ibuffer-filter-group-name-face 'modus-themes-pseudo-header)
    `(ibuffer-marked-face 'modus-themes-mark-sel)
    `(ibuffer-title-face 'default)
;;;; highlight-tail
    `(highlight-tail-colors
      '((,green-subtle-bg . 0)
        (,cyan-subtle-bg . 20)))
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
;;;; pdf-tools
    `(pdf-view-midnight-colors
      '(,fg-main . ,bg-dim))
;;;; vc-annotate (C-x v g)
    `(vc-annotate-background nil)
    `(vc-annotate-background-mode nil)
    `(vc-annotate-color-map
      '((20 . ,red)
        (40 . ,magenta)
        (60 . ,magenta-alt)
        (80 . ,red-alt)
        (100 . ,yellow)
        (120 . ,yellow-alt)
        (140 . ,fg-special-warm)
        (160 . ,fg-special-mild)
        (180 . ,green)
        (200 . ,green-alt)
        (220 . ,cyan-alt-other)
        (240 . ,cyan-alt)
        (260 . ,cyan)
        (280 . ,fg-special-cold)
        (300 . ,blue)
        (320 . ,blue-alt)
        (340 . ,blue-alt-other)
        (360 . ,magenta-alt-other)))
    `(vc-annotate-very-old-color nil)
;;;; xterm-color
    `(xterm-color-names ["black" ,red ,green ,yellow ,blue ,magenta ,cyan "gray65"])
    `(xterm-color-names-bright ["gray35" ,red-alt ,green-alt ,yellow-alt ,blue-alt ,magenta-alt ,cyan-alt "white"])
    (if (or (eq modus-themes-org-blocks 'tinted-background)
            (eq modus-themes-org-blocks 'rainbow))
        `(org-src-block-faces              ; TODO this list should be expanded
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

;; Local Variables:
;; time-stamp-pattern: "Last-Modified: <%Y-%02m-%02d %02H:%02M:%02S %5z>"
;; End:

;;; modus-themes.el ends here
