;;; modus-vivendi-theme.el --- Accessible dark theme (WCAG AAA) -*- lexical-binding:t -*-

;; Copyright (C) 2019-2020 Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://gitlab.com/protesilaos/modus-themes
;; Version: 0.13.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: faces, theme, accessibility

;; This file is part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This theme is designed for colour-contrast accessibility.
;;
;; 1. Provide a consistent minimum contrast ratio between background and
;; foreground values of 7:1 or higher.  This meets the highest such
;; accessibility criterion per the guidelines of the Worldwide Web
;; Consortium's Working Group on Accessibility (WCAG AAA standard).
;;
;; 2. Offer as close to full face coverage as possible.  The list is
;; already quite long (see further below), with more additions to follow
;; as part of the ongoing development process.
;;
;; The theme provides the following customisation options, all of which
;; are disabled by default:
;;
;;     modus-vivendi-theme-slanted-constructs             (boolean)
;;     modus-vivendi-theme-bold-constructs                (boolean)
;;     modus-vivendi-theme-variable-pitch-headings        (boolean)
;;     modus-vivendi-theme-no-mixed-fonts                 (boolean)
;;     modus-vivendi-theme-headings                       (alist)
;;     modus-vivendi-theme-scale-headings                 (boolean)
;;     modus-vivendi-theme-fringes                        (choice)
;;     modus-vivendi-theme-org-blocks                     (choice)
;;     modus-vivendi-theme-prompts                        (choice)
;;     modus-vivendi-theme-mode-line                      (choice)
;;     modus-vivendi-theme-diffs                          (choice)
;;     modus-vivendi-theme-faint-syntax                   (boolean)
;;     modus-vivendi-theme-intense-hl-line                (boolean)
;;     modus-vivendi-theme-intense-paren-match            (boolean)
;;     modus-vivendi-theme-no-link-underline              (boolean)
;;     modus-vivendi-theme-completions                    (choice)
;;     modus-vivendi-theme-override-colors-alist          (alist)
;;
;; The default scale is as follows (it can be customised as well):
;;
;;     modus-vivendi-theme-scale-1 1.05
;;     modus-vivendi-theme-scale-2 1.1
;;     modus-vivendi-theme-scale-3 1.15
;;     modus-vivendi-theme-scale-4 1.2
;;     modus-vivendi-theme-scale-5 1.3
;;
;; What follows is the list of explicitly supported packages or face
;; groups (there are implicitly supported packages as well, which
;; inherit from font-lock or some basic group).  You are encouraged to
;; notify me of any missing package or change you would like to see.
;;
;;     ace-window
;;     ag
;;     alert
;;     all-the-icons
;;     annotate
;;     anzu
;;     apropos
;;     apt-sources-list
;;     artbollocks-mode
;;     auctex and TeX
;;     auto-dim-other-buffers
;;     avy
;;     awesome-tray
;;     binder
;;     bm
;;     bongo
;;     boon
;;     breakpoint (provided by built-in gdb-mi.el)
;;     buffer-expose
;;     calendar and diary
;;     calfw
;;     centaur-tabs
;;     change-log and log-view (`vc-print-log' and `vc-print-root-log')
;;     cider
;;     circe
;;     color-rg
;;     column-enforce-mode
;;     company-mode
;;     company-posframe
;;     compilation-mode
;;     completions
;;     counsel
;;     counsel-css
;;     counsel-notmuch
;;     counsel-org-capture-string
;;     cov
;;     cperl-mode
;;     csv-mode
;;     ctrlf
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
;;     diredfl
;;     disk-usage
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
;;     emms
;;     enhanced-ruby-mode
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
;;     evil-visual-mark-mode
;;     eww
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
;;     ivy
;;     ivy-posframe
;;     jira (org-jira)
;;     journalctl-mode
;;     js2-mode
;;     julia
;;     jupyter
;;     kaocha-runner
;;     keycast
;;     line numbers (`display-line-numbers-mode' and global variant)
;;     lsp-mode
;;     lsp-ui
;;     magit
;;     magit-imerge
;;     man
;;     markdown-mode
;;     markup-faces (`adoc-mode')
;;     mentor
;;     messages
;;     minibuffer-line
;;     minimap
;;     modeline
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
;;     org-treescope
;;     origami
;;     outline-mode
;;     outline-minor-faces
;;     package (M-x list-packages)
;;     page-break-lines
;;     paradox
;;     paren-face
;;     parrot
;;     pass
;;     persp-mode
;;     perspective
;;     phi-grep
;;     phi-search
;;     pkgbuild-mode
;;     pomidor
;;     powerline
;;     powerline-evil
;;     proced
;;     prodigy
;;     racket-mode
;;     rainbow-blocks
;;     rainbow-identifiers
;;     rainbow-delimiters
;;     rcirc
;;     regexp-builder (also known as `re-builder')
;;     rg
;;     ripgrep
;;     rmail
;;     ruler-mode
;;     sallet
;;     selectrum
;;     semantic
;;     sesman
;;     shell-script-mode
;;     show-paren-mode
;;     side-notes
;;     skewer-mode
;;     smart-mode-line
;;     smartparens
;;     smerge
;;     spaceline
;;     speedbar
;;     spell-fu
;;     stripes
;;     suggest
;;     switch-window
;;     swiper
;;     swoop
;;     sx
;;     symbol-overlay
;;     tab-bar-mode
;;     tab-line-mode
;;     syslog-mode
;;     table (built-in table.el)
;;     telephone-line
;;     term
;;     tomatinho
;;     transient (pop-up windows like Magit's)
;;     trashed
;;     treemacs
;;     tty-menu
;;     tuareg
;;     typescript
;;     undo-tree
;;     vc (built-in mode line status for version control)
;;     vc-annotate (C-x v g)
;;     vdiff
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

;;; Code:



(deftheme modus-vivendi
  "Dark theme that conforms with the highest accessibility
  standard for colour contrast between background and
  foreground elements (WCAG AAA).")

;;; Custom faces

;; These faces will be inherited by actual constructs.  They are meant
;; for those cases where a face needs to distinguish its output from
;; the rest of the text, such as `isearch' and `occur'…  We define
;; these separately in order to combine each colour with its
;; appropriate foreground value.  This is to ensure a consistent
;; contrast ratio of >= 7:1.
(defgroup modus-theme ()
  "Theme that ensures WCAG AAA accessibility (contrast ratio
between foreground and background is >= 7:1)."
  :group 'faces
  :prefix "modus-theme-"
  :link '(url-link :tag "GitLab" "https://gitlab.com/protesilaos/modus-themes")
  :tag "Modus Vivendi")

(defface modus-theme-subtle-red nil nil)
(defface modus-theme-subtle-green nil nil)
(defface modus-theme-subtle-yellow nil nil)
(defface modus-theme-subtle-blue nil nil)
(defface modus-theme-subtle-magenta nil nil)
(defface modus-theme-subtle-cyan nil nil)
(defface modus-theme-subtle-neutral nil nil)
(defface modus-theme-intense-red nil nil)
(defface modus-theme-intense-green nil nil)
(defface modus-theme-intense-yellow nil nil)
(defface modus-theme-intense-blue nil nil)
(defface modus-theme-intense-magenta nil nil)
(defface modus-theme-intense-cyan nil nil)
(defface modus-theme-intense-neutral nil nil)
(defface modus-theme-refine-red nil nil)
(defface modus-theme-refine-green nil nil)
(defface modus-theme-refine-yellow nil nil)
(defface modus-theme-refine-blue nil nil)
(defface modus-theme-refine-magenta nil nil)
(defface modus-theme-refine-cyan nil nil)
(defface modus-theme-active-red nil nil)
(defface modus-theme-active-green nil nil)
(defface modus-theme-active-yellow nil nil)
(defface modus-theme-active-blue nil nil)
(defface modus-theme-active-magenta nil nil)
(defface modus-theme-active-cyan nil nil)
(defface modus-theme-fringe-red nil nil)
(defface modus-theme-fringe-green nil nil)
(defface modus-theme-fringe-yellow nil nil)
(defface modus-theme-fringe-blue nil nil)
(defface modus-theme-fringe-magenta nil nil)
(defface modus-theme-fringe-cyan nil nil)
(defface modus-theme-nuanced-red nil nil)
(defface modus-theme-nuanced-green nil nil)
(defface modus-theme-nuanced-yellow nil nil)
(defface modus-theme-nuanced-blue nil nil)
(defface modus-theme-nuanced-magenta nil nil)
(defface modus-theme-nuanced-cyan nil nil)
(defface modus-theme-special-cold nil nil)
(defface modus-theme-special-mild nil nil)
(defface modus-theme-special-warm nil nil)
(defface modus-theme-special-calm nil nil)
(defface modus-theme-diff-added nil nil)
(defface modus-theme-diff-changed nil nil)
(defface modus-theme-diff-removed nil nil)
(defface modus-theme-diff-refine-added nil nil)
(defface modus-theme-diff-refine-changed nil nil)
(defface modus-theme-diff-refine-removed nil nil)
(defface modus-theme-diff-focus-added nil nil)
(defface modus-theme-diff-focus-changed nil nil)
(defface modus-theme-diff-focus-removed nil nil)
(defface modus-theme-diff-heading nil nil)
(defface modus-theme-pseudo-header nil nil)
(defface modus-theme-mark-alt nil nil)
(defface modus-theme-mark-del nil nil)
(defface modus-theme-mark-sel nil nil)
(defface modus-theme-mark-symbol nil nil)
(defface modus-theme-heading-1 nil nil)
(defface modus-theme-heading-2 nil nil)
(defface modus-theme-heading-3 nil nil)
(defface modus-theme-heading-4 nil nil)
(defface modus-theme-heading-5 nil nil)
(defface modus-theme-heading-6 nil nil)
(defface modus-theme-heading-7 nil nil)
(defface modus-theme-heading-8 nil nil)
(defface modus-theme-hl-line nil nil)

;;; Customisation options

;; User-facing customisation options.  They are all deactivated by
;; default (users must opt in).
(defcustom modus-vivendi-theme-slanted-constructs nil
  "Use slanted text in more code constructs (italics or oblique)."
  :type 'boolean)

(defcustom modus-vivendi-theme-bold-constructs nil
  "Use bold text in more code constructs."
  :type 'boolean)

(define-obsolete-variable-alias 'modus-vivendi-theme-proportional-fonts
  'modus-vivendi-theme-variable-pitch-headings "`modus-vivendi-theme' 0.11.0")

(defcustom modus-vivendi-theme-proportional-fonts nil
  "Use proportional fonts (variable-pitch) in headings."
  :type 'boolean)

(defcustom modus-vivendi-theme-variable-pitch-headings nil
  "Use proportional fonts (variable-pitch) in headings."
  :type 'boolean)

(defcustom modus-vivendi-theme-no-mixed-fonts nil
  "Disable inheritance from `fixed-pitch' in some faces.

This is done by default to allow spacing-sensitive constructs,
such as Org tables and code blocks, to remain monospaced when
users opt for something like the command `variable-pitch-mode'.
The downside with the default is that users need to explicitly
configure the font family of `fixed-pitch' in order to get a
consistent experience.  That may be something they do not want to
do.  Hence this option to disable any kind of technique for
mixing fonts."
  :type 'boolean)

(make-obsolete 'modus-vivendi-theme-rainbow-headings
               'modus-vivendi-theme-headings
               "`modus-vivendi-theme' 0.13.0")

(defcustom modus-vivendi-theme-rainbow-headings nil
  "Use more saturated colours for headings."
  :type 'boolean)

(make-obsolete 'modus-vivendi-theme-section-headings
               'modus-vivendi-theme-headings
               "`modus-vivendi-theme' 0.13.0")

(defcustom modus-vivendi-theme-section-headings nil
  "Use a background and an overline in headings."
  :type 'boolean)

(defcustom modus-vivendi-theme-headings
  '((t . nil))
  "Alist of styles for headings, with optional value per level.

To control faces per level from 1-8, use something like this:

  (setq modus-vivendi-theme-headings
        '((1 . highlight)
          (2 . line)
          (t . rainbow-line-no-bold)))

To set a uniform value for all heading levels, use this pattern:

  (setq modus-vivendi-theme-headings
        '((t . rainbow-line-no-bold)))

The default uses a fairly desaturated foreground value in
combination with a bold typographic weight.  To specify this
style for a given level N (assuming you wish to have another
fallback option), just specify the value t like this:

  (setq modus-vivendi-theme-headings
        '((1 . t)
          (2 . line)
          (t . rainbow-line-no-bold)))

A description of all possible values:

+ `no-bold' retains the default text colour while removing
  the typographic weight.

+ `line' is the same as the default plus an overline over the
  heading.

+ `line-no-bold' is the same as `line' without bold weight.

+ `rainbow' uses a more colourful foreground in combination
  with bold weight.

+ `rainbow-line' is the same as `rainbow' plus an overline.

+ `rainbow-line-no-bold' is the same as `rainbow-line' without
  the bold weight.

+ `highlight' retains the default style of a fairly desaturated
  foreground combined with a bold weight and add to it a subtle
  accented background.

+ `highlight-no-bold' is the same as `highlight' without a bold
  weight.

+ `rainbow-highlight' is the same as `highlight' but with a more
  colourful foreground.

+ `rainbow-highlight-no-bold' is the same as `rainbow-highlight'
  without a bold weight.

+ `section' retains the default looks and adds to them both an
  overline and a slightly accented background.  It is, in effect,
  a combination of the `line' and `highlight' values.

+ `section-no-bold' is the same as `section' without a bold
  weight.

+ `rainbow-section' is the same as `section' but with a more
  colourful foreground.

+ `rainbow-section-no-bold' is the same as `rainbow-section'
  without a bold weight."
  :type
  '(alist
    :key-type symbol
    :value-type
    (choice (const :tag "Fairly desaturated foreground with bold weight (default)" t)
            (const :tag "Like the default without bold weight" no-bold)
            (const :tag "Like the default plus overline" line)
            (const :tag "Like `line' without bold weight" line-no-bold)
            (const :tag "Like the default but with more colourful foreground" rainbow)
            (const :tag "Like `rainbow' plus overline" rainbow-line)
            (const :tag "Like `rainbow' without bold weight" rainbow-no-bold)
            (const :tag "Like `rainbow-line' without bold weight" rainbow-line-no-bold)
            (const :tag "Like the default plus subtle background" highlight)
            (const :tag "Like `highlight' without bold weight" highlight-no-bold)
            (const :tag "Like `highlight' with more colourful foreground" rainbow-highlight)
            (const :tag "Like `rainbow-highlight' without bold weight" rainbow-highlight-no-bold)
            (const :tag "Like `highlight' plus overline" section)
            (const :tag "Like `section' without bold weight" section-no-bold)
            (const :tag "Like `section' with more colourful foreground" rainbow-section)
            (const :tag "Like `rainbow-section' without bold weight" rainbow-section-no-bold))))

(defcustom modus-vivendi-theme-scale-headings nil
  "Use font scaling for headings."
  :type 'boolean)

(defcustom modus-vivendi-theme-scale-1 1.05
  "Font size that is slightly larger than the base value.
The default is a floating point that is interpreted as a multiple
of the base font size.  However, the variable also accepts an
integer, understood as an absolute height (e.g. a value of 140 is
the same as setting the font at 14 point size).

For more on the matter, read the documentation of
`set-face-attribute', specifically the ':height' section."
  :type 'number)

(defcustom modus-vivendi-theme-scale-2 1.1
  "Font size slightly larger than `modus-vivendi-theme-scale-1'.
The default is a floating point that is interpreted as a multiple
of the base font size.  However, the variable also accepts an
integer, understood as an absolute height (e.g. a value of 140 is
the same as setting the font at 14 point size).

For more on the matter, read the documentation of
`set-face-attribute', specifically the ':height' section."
  :type 'number)

(defcustom modus-vivendi-theme-scale-3 1.15
  "Font size slightly larger than `modus-vivendi-theme-scale-2'.
The default is a floating point that is interpreted as a multiple
of the base font size.  However, the variable also accepts an
integer, understood as an absolute height (e.g. a value of 140 is
the same as setting the font at 14 point size).

For more on the matter, read the documentation of
`set-face-attribute', specifically the ':height' section."
  :type 'number)

(defcustom modus-vivendi-theme-scale-4 1.2
  "Font size slightly larger than `modus-vivendi-theme-scale-3'.
The default is a floating point that is interpreted as a multiple
of the base font size.  However, the variable also accepts an
integer, understood as an absolute height (e.g. a value of 140 is
the same as setting the font at 14 point size).

For more on the matter, read the documentation of
`set-face-attribute', specifically the ':height' section."
  :type 'number)

(defcustom modus-vivendi-theme-scale-5 1.3
  "Font size slightly larger than `modus-vivendi-theme-scale-4'.
The default is a floating point that is interpreted as a multiple
of the base font size.  However, the variable also accepts an
integer, understood as an absolute height (e.g. a value of 140 is
the same as setting the font at 14 point size).

For more on the matter, read the documentation of
`set-face-attribute', specifically the ':height' section."
  :type 'number)

(make-obsolete 'modus-vivendi-theme-visible-fringes
               'modus-vivendi-theme-fringes
               "`modus-vivendi-theme' 0.12.0")

(defcustom modus-vivendi-theme-visible-fringes nil
  "Use a visible style for fringes."
  :type 'boolean)

(defcustom modus-vivendi-theme-fringes nil
  "Define the visibility of fringes.

Nil means the fringes have no background colour.  Option `subtle'
will apply a greyscale value that is visible yet close to the
main buffer background colour.  Option `intense' will use a more
pronounced greyscale value."
  :type '(choice
          (const :tag "No visible fringes (default)" nil)
          (const :tag "Subtle greyscale background" subtle)
          (const :tag "Intense greyscale background" intense)))

(make-obsolete 'modus-vivendi-theme-distinct-org-blocks
               'modus-vivendi-theme-org-blocks
               "`modus-vivendi-theme' 0.11.0")

(defcustom modus-vivendi-theme-distinct-org-blocks nil
  "Use a distinct neutral background for `org-mode' blocks."
  :type 'boolean)

(make-obsolete 'modus-vivendi-theme-rainbow-org-src-blocks
               'modus-vivendi-theme-org-blocks
               "`modus-vivendi-theme' 0.11.0")

(defcustom modus-vivendi-theme-rainbow-org-src-blocks nil
  "Use colour-coded backgrounds for `org-mode' source blocks.
The colour in use depends on the language (send feedback to
include more languages)."
  :type 'boolean)

(defcustom modus-vivendi-theme-org-blocks nil
  "Use a subtle grey or colour-coded background for Org blocks.

Nil means that the block will have no background of its own and
will use the default that applies to the rest of the buffer.

Option `greyscale' will apply a subtle neutral grey background to
the block's contents.  It also affects the begin and end lines of
the block: their background will be extended to the edge of the
window for Emacs version >= 27 where the ':extend' keyword is
recognised by `set-face-attribute'.

Option `rainbow' will use an accented background for the contents
of the block.  The exact colour will depend on the programming
language and is controlled by the `org-src-block-faces'
variable (refer to the theme's source code for the current
association list)."
  :type '(choice
          (const :tag "No Org block background (default)" nil)
          (const :tag "Subtle grey block background" greyscale)
          (const :tag "Colour-coded background per programming language" rainbow)))

(make-obsolete 'modus-vivendi-theme-3d-modeline
               'modus-vivendi-theme-mode-line
               "`modus-vivendi-theme' 0.13.0")

(defcustom modus-vivendi-theme-3d-modeline nil
  "Use a three-dimensional style for the active mode line."
  :type 'boolean)

(defcustom modus-vivendi-theme-mode-line nil
  "Adjust the overall style of the mode line.

Nil is a two-dimensional rectangle with a border around it.  The
active and the inactive modelines use different shades of
greyscale values for the background and foreground.

A `3d' value will apply a three-dimensional effect to the active
modeline.  The inactive modelines remain two-dimensional and are
toned down a bit, relative to the nil value.

The `moody' option is meant to optimise the modeline for use with
the library of the same name.  This practically means to remove
the box effect and rely on underline and overline properties
instead.  It also tones down the inactive modelines.  Despite its
intended purpose, this option can also be used without the
`moody' library."
  :type '(choice
          (const :tag "Two-dimensional box (default)" nil)
          (const :tag "Three-dimensional style for the active mode line" 3d)
          (const :tag "No box effects, which are optimal for use with the `moody' library" moody)))

(make-obsolete 'modus-vivendi-theme-subtle-diffs
               'modus-vivendi-theme-diffs
               "`modus-vivendi-theme' 0.13.0")

(defcustom modus-vivendi-theme-subtle-diffs nil
  "Use fewer/dim backgrounds in `diff-mode', `ediff',`magit'."
  :type 'boolean)

(defcustom modus-vivendi-theme-diffs nil
  "Adjust the overall styles of diffs.

Nil means to use fairly intense colour combinations for diffs.
For example, you get a rich green background with a green
foreground for added lines.  Word-wise or 'refined' diffs follow
the same pattern but use different shades of those colours to
remain distinct.

A `desaturated' value follows the same principles as with the nil
option, while it tones down all relevant colours.

Option `fg-only' will remove all accented backgrounds, except
from word-wise changes.  It instead uses colour-coded foreground
values to differentiate between added/removed/changed lines.  If
a background is necessary, such as with `ediff', then a subtle
greyscale value is used."
  :type '(choice
          (const :tag "Intensely coloured backgrounds (default)" nil)
          (const :tag "Slightly accented backgrounds with tinted text" desaturated)
          (const :tag "No backgrounds, except for refined diffs" fg-only)))

(make-obsolete 'modus-vivendi-theme-intense-standard-completions
               'modus-vivendi-theme-completions
               "`modus-vivendi-theme' 0.12.0")

(defcustom modus-vivendi-theme-intense-standard-completions nil
  "Use prominent backgrounds for Icomplete, Ido, or similar."
  :type 'boolean)

(defcustom modus-vivendi-theme-completions nil
  "Apply special styles to the UI of completion frameworks.

This concerns Icomplete, Ivy, Helm, Selectrum, Ido, as well as
any other tool meant to enhance their experience.  The effect
will vary depending on the completion framework.

Nil means to remain faithful to the metaphors that each UI
establishes.  For example, Icomplete and Ido only use foreground
colours to style their matches, whereas Ivy or Helm rely on an
aesthetic that combines coloured backgrounds with appropriate
text colour.

Option `moderate' will apply a combination of background and
foreground that is fairly subtle.  For Icomplete and the like,
this constitutes a departure from their standard style.  While
Ivy, Helm, and the others, will use less pronounced colours for
applicable contexts.

Option `opinionated' will apply colour combinations that
refashion the completion UI.  So Icomplete et al will now use
styles that resemble the defaults of Ivy and co., while the
latter group will revert to an even more nuanced aesthetic."
  :type '(choice
          (const :tag "Respect the framework's established aesthetic (default)" nil)
          (const :tag "Subtle backgrounds for various elements" moderate)
          (const :tag "Radical alternative to the framework's looks" opinionated)))

(defcustom modus-vivendi-theme-prompts nil
  "Use subtle or intense styles for minibuffer and REPL prompts.

Nil means to only use an accented foreground colour.

Options `subtle' and `intense' will change both the background
and the foreground values.  The latter has a more pronounced
effect than the former."
  :type '(choice
          (const :tag "No prompt background (default)" nil)
          (const :tag "Subtle accented background for the prompt" subtle)
          (const :tag "Intense background and foreground for the prompt" intense)))

(defcustom modus-vivendi-theme-intense-hl-line nil
  "Use more prominent background for command `hl-line-mode'."
  :type 'boolean)

(defcustom modus-vivendi-theme-intense-paren-match nil
  "Use more prominent colour for parenthesis matching."
  :type 'boolean)

(defcustom modus-vivendi-theme-faint-syntax nil
  "Use less saturated colours for code syntax highlighting."
  :type 'boolean)

(defcustom modus-vivendi-theme-no-link-underline nil
  "Do not underline links."
  :type 'boolean)

;;; Internal functions

;; Helper functions that are meant to ease the implementation of the
;; above customisation options.
(defun modus-vivendi-theme-bold-weight ()
  "Conditional use of a heavier text weight."
  (when modus-vivendi-theme-bold-constructs
    (list :inherit 'bold)))

(defun modus-vivendi-theme-mixed-fonts ()
  "Conditional application of `fixed-pitch' inheritance."
  (unless modus-vivendi-theme-no-mixed-fonts
    (list :inherit 'fixed-pitch)))

(defun modus-vivendi-theme-fringe (subtlebg intensebg)
  "Conditional use of background colours for fringes.
SUBTLEBG should be a subtle greyscale value.  INTENSEBG must be a
more pronounced greyscale colour."
  (pcase modus-vivendi-theme-fringes
    ('intense (list :background intensebg))
    ('subtle (list :background subtlebg))
    (_ (list :background nil))))

(defun modus-vivendi-theme-prompt (mainfg subtlebg subtlefg intensebg intensefg)
  "Conditional use of background colours for prompts.
MAINFG is the prompt's standard foreground.  SUBTLEBG should be a
subtle accented background that works with SUBTLEFG.  INTENSEBG
must be a more pronounced accented colour that should be
combinable with INTENSEFG."
  (pcase modus-vivendi-theme-prompts
    ('intense (list :background intensebg :foreground intensefg))
    ('subtle (list :background subtlebg :foreground subtlefg))
    (_ (list :background nil :foreground mainfg))))

(defun modus-vivendi-theme-paren (normalbg intensebg)
  "Conditional use of intense colours for matching parentheses.
NORMALBG should the special palette colour 'bg-paren-match' or
something similar.  INTENSEBG must be easier to discern next to
other backgrounds, such as the special palette colour
'bg-paren-match-intense'."
  (if modus-vivendi-theme-intense-paren-match
      (list :background intensebg)
    (list :background normalbg)))

(defun modus-vivendi-theme-syntax-foreground (normal faint)
  "Apply foreground value to code syntax.
NORMAL is the more saturated colour, which should be the default.
FAINT is the less saturated colour."
  (if modus-vivendi-theme-faint-syntax
      (list :foreground faint)
    (list :foreground normal)))

(defun modus-vivendi-theme-heading-p (key)
  "Query style of KEY in `modus-vivendi-theme-headings'."
  (cdr (assoc key modus-vivendi-theme-headings)))

(defun modus-vivendi-theme-heading (level fg fg-alt bg border)
  "Conditional styles for `modus-vivendi-theme-headings'.

LEVEL is the heading's position in their order.  FG is the
default text colour.  FG-ALT is an accented, more saturated value
than the default.  BG is a nuanced, typically accented,
background that can work well with either of the foreground
values.  BORDER is a colour value that combines well with the
background and alternative foreground."
  (let* ((key (modus-vivendi-theme-heading-p `,level))
         (style (or key (modus-vivendi-theme-heading-p t)))
         (var (if modus-vivendi-theme-variable-pitch-headings
                  'variable-pitch
                'default)))
    (pcase style
      ('no-bold
       (list :inherit `,var :foreground fg))
      ('line
       (list :inherit `(bold ,var) :foreground fg :overline border))
      ('line-no-bold
       (list :inherit `,var :foreground fg :overline border))
      ('rainbow
       (list :inherit `(bold ,var) :foreground fg-alt))
      ('rainbow-no-bold
       (list :inherit `,var :foreground fg-alt))
      ('rainbow-line
       (list :inherit `(bold ,var) :foreground fg-alt :overline border))
      ('rainbow-line-no-bold
       (list :inherit `,var :foreground fg-alt :overline border))
      ('highlight
       (list :inherit `(bold ,var) :background bg :foreground fg))
      ('highlight-no-bold
       (list :inherit `,var :background bg :foreground fg))
      ('rainbow-highlight
       (list :inherit `(bold ,var) :background bg :foreground fg-alt))
      ('rainbow-highlight-no-bold
       (list :inherit `,var :background bg :foreground fg-alt))
      ('section
       (append
        (and (>= emacs-major-version 27) '(:extend t))
        (list :inherit `(bold ,var) :background bg :foreground fg :overline border)))
      ('section-no-bold
       (append
        (and (>= emacs-major-version 27) '(:extend t))
        (list :inherit `,var :background bg :foreground fg :overline border)))
      ('rainbow-section
       (append
        (and (>= emacs-major-version 27) '(:extend t))
        (list :inherit `(bold ,var) :background bg :foreground fg-alt :overline border)))
      ('rainbow-section-no-bold
       (append
        (and (>= emacs-major-version 27) '(:extend t))
        (list :inherit `,var :background bg :foreground fg-alt :overline border)))
      (_
       (list :inherit `(bold ,var) :foreground fg)))))

(defun modus-vivendi-theme-org-block (bgblk)
  "Conditionally set the background of Org blocks.
BGBLK applies to a distinct neutral background.  Else blocks have
no background of their own (the default), so they look the same
as the rest of the buffer.

`modus-vivendi-theme-org-blocks' also accepts a `rainbow' option
which is applied conditionally to `org-src-block-faces' (see the
theme's source code)."
  (if (eq modus-vivendi-theme-org-blocks 'greyscale)
      (append
       (and (>= emacs-major-version 27) '(:extend t))
       (list :background bgblk))
    (list :background nil)))

(defun modus-vivendi-theme-org-block-delim (bgaccent fgaccent bg fg)
  "Conditionally set the styles of Org block delimiters.
BG, FG, BGACCENT, FGACCENT apply a background and foreground
colour respectively.

The former pair is a greyscale combination that should be more
distinct than the background of the block.  It is applied to the
default styles or when `modus-vivendi-theme-org-blocks' is set
to `greyscale'.

The latter pair should be more subtle than the background of the
block, as it is used when `modus-vivendi-theme-org-blocks' is
set to `rainbow'."
  (pcase modus-vivendi-theme-org-blocks
    ('greyscale (append (and (>= emacs-major-version 27) '(:extend t))
                        (list :background bg :foreground fg)))
    ('rainbow (list :background bgaccent :foreground fgaccent))
    (_ (list :background bg :foreground fg))))

(defun modus-vivendi-theme-mode-line-attrs
    (fg bg fg-alt bg-alt border border-3d &optional alt-style border-width fg-distant)
  "Colour combinations for `modus-vivendi-theme-mode-line'.

FG and BG are the default colours.  FG-ALT and BG-ALT are meant
to accommodate the options for a 3D modeline or a `moody'
compliant one.  BORDER applies to all permutations of the
modeline, except the three-dimensional effect, where BORDER-3D is
used instead.

Optional ALT-STYLE applies an appropriate style to the mode
line's box property.

Optional BORDER-WIDTH specifies an integer for the width of the
rectangle that produces the box effect.

Optional FG-DISTANT should be close to the main background
values.  It is intended to be used as a distant-foreground
property."
  (pcase modus-vivendi-theme-mode-line
    ('3d
     `(:background ,bg-alt :foreground ,fg-alt
                   :box (:line-width ,(or border-width 1)
                                     :color ,border-3d
                                     :style ,(and alt-style 'released-button))))
    ('moody
     `(:background ,bg-alt :foreground ,fg-alt :underline ,border :overline ,border
                   :distant-foreground ,fg-distant))
    (_
     `(:foreground ,fg :background ,bg :box ,border))))

(defun modus-vivendi-theme-diff (fg-only-bg fg-only-fg mainbg mainfg altbg altfg)
  "Colour combinations for `modus-vivendi-theme-diffs'.

FG-ONLY-BG should be similar or the same as the main background.
FG-ONLY-FG should be a saturated accent value that can be
combined with the former.

MAINBG must be one of the dedicated backgrounds for diffs while
MAINFG must be the same for the foreground.

ALTBG needs to be a slightly accented background that is meant to
be combined with ALTFG.  Both must be less intense than MAINBG
and MAINFG respectively."
  (pcase modus-vivendi-theme-diffs
    ('fg-only (list :background fg-only-bg :foreground fg-only-fg))
    ('desaturated (list :background altbg :foreground altfg))
    (_ (list :background mainbg :foreground mainfg))))

(defun modus-vivendi-theme-standard-completions (mainfg subtlebg intensebg intensefg)
  "Combinations for `modus-vivendi-theme-completions'.

MAINFG is an accented foreground value.  SUBTLEBG is an accented
background value that can be combined with MAINFG.  INTENSEBG and
INTENSEFG are accented colours that are designed to be used in
tandem.

These are intended for Icomplete, Ido, and related."
  (pcase modus-vivendi-theme-completions
    ('opinionated (list :background intensebg :foreground intensefg))
    ('moderate (list :background subtlebg :foreground mainfg))
    (_ (list :foreground mainfg))))

(defun modus-vivendi-theme-extra-completions (subtleface intenseface altface &optional altfg bold)
  "Combinations for `modus-vivendi-theme-completions'.

SUBTLEFACE and INTENSEFACE are custom theme faces that combine a
background and foreground value.  The difference between the two
is a matter of degree.

ALTFACE is a combination of colours that represents a departure
from the UI's default aesthetics.  Optional ALTFG is meant to be
used in tandem with it.

Optional BOLD will apply a heavier weight to the text.

These are intended for Helm, Ivy, etc."
  (pcase modus-vivendi-theme-completions
    ('opinionated (list :inherit (list altface bold)
                        :foreground (or altfg 'unspecified)))
    ('moderate (list :inherit (list subtleface bold)))
    (_ (list :inherit (list intenseface bold)))))

(defun modus-vivendi-theme-scale (amount)
  "Scale heading by AMOUNT.

AMOUNT is a customisation option."
  (when modus-vivendi-theme-scale-headings
    (list :height amount)))

;;; Colour palette

;; Define colour palette.  Each colour must have a >= 7:1 contrast
;; ratio relative to the foreground/background colour it is rendered
;; against.
;;
;; The design of the colour palette as a macro that maps it to faces is
;; adapted from zenbern-theme.el, last seen at commit 7dd7968:
;; https://github.com/bbatsov/zenburn-emacs
(eval-and-compile
  (defconst modus-vivendi-theme-default-colors-alist
    '(;; base values
      ("bg-main" . "#000000") ("fg-main" . "#ffffff")
      ("bg-alt" . "#181a20") ("fg-alt" . "#a8a8a8")
      ("bg-dim" . "#110b11") ("fg-dim" . "#e0e6f0")
      ;; specifically for on/off states (e.g. `mode-line')
      ;;
      ;; must be combined with themselves
      ("bg-active" . "#323232") ("fg-active" . "#f4f4f4")
      ("bg-inactive" . "#1e1e1e") ("fg-inactive" . "#bfc0c4")
      ;; special base values, used only for cases where the above
      ;; fg-* or bg-* cannot or should not be used (to avoid confusion)
      ;; must be combined with: {fg,bg}-{main,alt,dim}
      ("bg-special-cold" . "#203448") ("fg-special-cold" . "#c6eaff")
      ("bg-special-mild" . "#00322e") ("fg-special-mild" . "#bfebe0")
      ("bg-special-warm" . "#382f27") ("fg-special-warm" . "#f8dec0")
      ("bg-special-calm" . "#392a48") ("fg-special-calm" . "#fbd6f4")
      ;; styles for the main constructs
      ;;
      ;; must be combined with: `bg-main', `bg-alt', `bg-dim'
      ("red" . "#ff8059") ("green" . "#44bc44")
      ("yellow" . "#eecc00") ("blue" . "#2fafff")
      ("magenta" . "#feacd0") ("cyan" . "#00d3d0")
      ;; styles for common, but still specialised constructs
      ;;
      ;; must be combined with: `bg-main', `bg-alt', `bg-dim'
      ("red-alt" . "#f4923b") ("green-alt" . "#80d200")
      ("yellow-alt" . "#cfdf30") ("blue-alt" . "#79a8ff")
      ("magenta-alt" . "#f78fe7") ("cyan-alt" . "#4ae8fc")
      ;; same purpose as above, just slight differences
      ;;
      ;; must be combined with: `bg-main', `bg-alt', `bg-dim'
      ("red-alt-other" . "#ff9977") ("green-alt-other" . "#00cd68")
      ("yellow-alt-other" . "#f0ce43") ("blue-alt-other" . "#00bcff")
      ("magenta-alt-other" . "#b6a0ff") ("cyan-alt-other" . "#6ae4b9")
      ;; styles for desaturated foreground text, intended for use with
      ;; the `modus-vivendi-theme-faint-syntax' option
      ;;
      ;; must be combined with: `bg-main', `bg-alt', `bg-dim'
      ("red-faint" . "#ffa0a0") ("green-faint" . "#88cf88")
      ("yellow-faint" . "#d2b580") ("blue-faint" . "#92baff")
      ("magenta-faint" . "#e0b2d6") ("cyan-faint" . "#a0bfdf")

      ("red-alt-faint" . "#f5aa80") ("green-alt-faint" . "#a8cf88")
      ("yellow-alt-faint" . "#cabf77") ("blue-alt-faint" . "#a4b0ff")
      ("magenta-alt-faint" . "#ef9fe4") ("cyan-alt-faint" . "#90c4ed")

      ("red-alt-other-faint" . "#ff9fbf") ("green-alt-other-faint" . "#88cfaf")
      ("yellow-alt-other-faint" . "#d0ba95") ("blue-alt-other-faint" . "#8fc5ff")
      ("magenta-alt-other-faint" . "#d0b4ff") ("cyan-alt-other-faint" . "#a4d0bb")
      ;; styles for elements that should be very subtle, yet accented
      ;;
      ;; must be combined with: `bg-main', `bg-alt', `bg-dim' or any of
      ;; the "nuanced" backgrounds
      ("red-nuanced" . "#ffcccc") ("green-nuanced" . "#b8e2b8")
      ("yellow-nuanced" . "#dfdfb0") ("blue-nuanced" . "#bfd9ff")
      ("magenta-nuanced" . "#e5cfef") ("cyan-nuanced" . "#a8e5e5")
      ;; styles for slightly accented background
      ;;
      ;; must be combined with any of the above foreground values
      ("red-nuanced-bg" . "#2c0614") ("green-nuanced-bg" . "#001904")
      ("yellow-nuanced-bg" . "#221000") ("blue-nuanced-bg" . "#0f0e39")
      ("magenta-nuanced-bg" . "#230631") ("cyan-nuanced-bg" . "#041529")
      ;; styles for elements that should draw attention to themselves
      ;;
      ;; must be combined with: `bg-main'
      ("red-intense" . "#fb6859") ("green-intense" . "#00fc50")
      ("yellow-intense" . "#ffdd00") ("blue-intense" . "#00a2ff")
      ("magenta-intense" . "#ff8bd4") ("cyan-intense" . "#30ffc0")
      ;; styles for background elements that should be visible yet
      ;; subtle
      ;;
      ;; must be combined with: `fg-dim'
      ("red-subtle-bg" . "#762422") ("green-subtle-bg" . "#2f4a00")
      ("yellow-subtle-bg" . "#604200") ("blue-subtle-bg" . "#10387c")
      ("magenta-subtle-bg" . "#49366e") ("cyan-subtle-bg" . "#00415e")
      ;; styles for background elements that should be visible and
      ;; distinguishable
      ;;
      ;; must be combined with: `fg-main'
      ("red-intense-bg" . "#a4202a") ("green-intense-bg" . "#006800")
      ("yellow-intense-bg" . "#874900") ("blue-intense-bg" . "#2a40b8")
      ("magenta-intense-bg" . "#7042a2") ("cyan-intense-bg" . "#005f88")
      ;; styles for refined contexts where both the foreground and the
      ;; background need to have the same/similar hue
      ;;
      ;; must be combined with themselves OR the foregrounds can be
      ;; combined with any of the base backgrounds
      ("red-refine-bg" . "#77002a") ("red-refine-fg" . "#ffb9ab")
      ("green-refine-bg" . "#00422a") ("green-refine-fg" . "#9ff0cf")
      ("yellow-refine-bg" . "#693200") ("yellow-refine-fg" . "#e2d980")
      ("blue-refine-bg" . "#242679") ("blue-refine-fg" . "#8ec6ff")
      ("magenta-refine-bg" . "#71206a") ("magenta-refine-fg" . "#ffcaf0")
      ("cyan-refine-bg" . "#004065") ("cyan-refine-fg" . "#8ae4f2")
      ;; styles that are meant exclusively for the mode line
      ;;
      ;; must be combined with: `bg-active', `bg-inactive'
      ("red-active" . "#ffa7ba") ("green-active" . "#70d73f")
      ("yellow-active" . "#dbbe5f") ("blue-active" . "#34cfff")
      ("magenta-active" . "#d5b1ff") ("cyan-active" . "#00d8b4")
      ;; styles that are meant exclusively for the fringes
      ;;
      ;; must be combined with `fg-main'
      ("red-fringe-bg" . "#8f1f4b") ("green-fringe-bg" . "#006700")
      ("yellow-fringe-bg" . "#6f4f00") ("blue-fringe-bg" . "#3f33af")
      ("magenta-fringe-bg" . "#6f2f89") ("cyan-fringe-bg" . "#004f8f")
      ;; styles reserved for specific faces
      ;;
      ;; `bg-hl-line' is between `bg-dim' and `bg-alt', so it should
      ;; work with all accents that cover those two, plus `bg-main'
      ;;
      ;; `bg-hl-alt' and `bg-hl-alt-intense' should only be used when no
      ;; other greyscale or fairly neutral background is available to
      ;; properly draw attention to a given construct
      ;;
      ;; `bg-header' is between `bg-active' and `bg-inactive', so it
      ;; can be combined with any of the "active" values, plus the
      ;; "special" and base foreground colours
      ;;
      ;; `bg-paren-match', `bg-paren-match-intense', `bg-region' and
      ;; `bg-tab-active' must be combined with `fg-main', while
      ;; `bg-tab-inactive' should be combined with `fg-dim'
      ;;
      ;; `bg-tab-bar' is only intended for the bar that holds the tabs and
      ;; can only be combined with `fg-main'
      ;;
      ;; `fg-tab-active' is meant to be combined with `bg-tab-active',
      ;; though only for styling special elements, such as underlining
      ;; the current tab
      ;;
      ;; `fg-escape-char-construct' and `fg-escape-char-backslash' can
      ;; be combined `bg-main', `bg-dim', `bg-alt'
      ;;
      ;; `fg-lang-error', `fg-lang-warning', `fg-lang-note' can be
      ;; combined with `bg-main', `bg-dim', `bg-alt'
      ;;
      ;; `fg-mark-sel', `fg-mark-del', `fg-mark-alt' can be combined
      ;; with `bg-main', `bg-dim', `bg-alt', `bg-hl-line'
      ;;
      ;; `fg-unfocused' must be combined with `fg-main'
      ;;
      ;; the window divider colours apply to faces with just an fg value
      ;;
      ;; all pairs are combinable with themselves
      ("bg-hl-line" . "#151823")
      ("bg-hl-line-intense" . "#2f2f2f")
      ("bg-hl-alt" . "#181732")
      ("bg-hl-alt-intense" . "#282e46")
      ("bg-paren-match" . "#5f362f")
      ("bg-paren-match-intense" . "#7416b5")
      ("bg-region" . "#3c3c3c")

      ("bg-tab-bar" . "#2c2c2c")
      ("bg-tab-active" . "#0e0e0e")
      ("bg-tab-inactive" . "#3d3d3d")
      ("fg-tab-active" . "#5ac3cf")

      ("fg-escape-char-construct" . "#e7a59a")
      ("fg-escape-char-backslash" . "#abab00")

      ("fg-lang-error" . "#ef8690")
      ("fg-lang-warning" . "#b0aa00")
      ("fg-lang-note" . "#9d9def")

      ("fg-window-divider-inner" . "#646464")
      ("fg-window-divider-outer" . "#969696")

      ("fg-unfocused" . "#93959b")

      ("bg-header" . "#212121") ("fg-header" . "#dddddd")

      ("bg-whitespace" . "#170016") ("fg-whitespace" . "#a4959f")

      ("bg-diff-heading" . "#304466") ("fg-diff-heading" . "#dadffe")
      ("bg-diff-added" . "#0a280a") ("fg-diff-added" . "#94ba94")
      ("bg-diff-changed" . "#2a2000") ("fg-diff-changed" . "#b0ba9f")
      ("bg-diff-removed" . "#40160f") ("fg-diff-removed" . "#c6adaa")

      ("bg-diff-refine-added" . "#005a36") ("fg-diff-refine-added" . "#e0f6e0")
      ("bg-diff-refine-changed" . "#585800") ("fg-diff-refine-changed" . "#ffffcc")
      ("bg-diff-refine-removed" . "#852828") ("fg-diff-refine-removed" . "#ffd9eb")

      ("bg-diff-focus-added" . "#203d20") ("fg-diff-focus-added" . "#b4ddb4")
      ("bg-diff-focus-changed" . "#4a3a10") ("fg-diff-focus-changed" . "#d0daaf")
      ("bg-diff-focus-removed" . "#5e2526") ("fg-diff-focus-removed" . "#eebdba")

      ("bg-diff-neutral-0" . "#575757") ("fg-diff-neutral-0" . "#fcfcfc")
      ("bg-diff-neutral-1" . "#454545") ("fg-diff-neutral-1" . "#dddddd")
      ("bg-diff-neutral-2" . "#313131") ("fg-diff-neutral-2" . "#bfbfbf")

      ("bg-mark-sel" . "#002f2f") ("fg-mark-sel" . "#60cfa2")
      ("bg-mark-del" . "#5a0000") ("fg-mark-del" . "#ff99aa")
      ("bg-mark-alt" . "#3f2210") ("fg-mark-alt" . "#f0aa20"))
    "The entire palette of `modus-vivendi-theme'.
Each element has the form (NAME . HEX).")

  (defcustom modus-vivendi-theme-override-colors-alist '()
    "Association list of palette colour overrides.
Values can be mapped to variables, using the same syntax as the
one present in `modus-vivendi-theme-default-colors-alist'.

This is only meant for do-it-yourself usage, with the
understanding that the user is responsible for the resulting
contrast ratio between new and existing colours."
    :type '(alist
            :key-type (string :tag "Name")
            :value-type (string :tag " Hex")))

  (defmacro modus-vivendi-theme-with-color-variables (&rest body)
    "`let' bind all colours around BODY.
Also bind `class' to ((class color) (min-colors 89))."
    (declare (indent 0))
    `(let ((class '((class color) (min-colors 89)))
           ,@(mapcar (lambda (cons)
                       (list (intern (car cons)) (cdr cons)))
                     (append modus-vivendi-theme-default-colors-alist
                             modus-vivendi-theme-override-colors-alist))
           ;; simple conditional styles that evaluate user-facing
           ;; customisation options
           (modus-theme-slant
            (if modus-vivendi-theme-slanted-constructs 'italic 'normal))
           (modus-theme-variable-pitch
            (if modus-vivendi-theme-variable-pitch-headings 'variable-pitch 'default)))
       ,@body)))



;;; Faces

(modus-vivendi-theme-with-color-variables
  (custom-theme-set-faces
   'modus-vivendi
;;;; custom faces
   ;; these bespoke faces are inherited by other constructs below
;;;;; subtle coloured backgrounds
   `(modus-theme-subtle-red ((,class :background ,red-subtle-bg :foreground ,fg-dim)))
   `(modus-theme-subtle-green ((,class :background ,green-subtle-bg :foreground ,fg-dim)))
   `(modus-theme-subtle-yellow ((,class :background ,yellow-subtle-bg :foreground ,fg-dim)))
   `(modus-theme-subtle-blue ((,class :background ,blue-subtle-bg :foreground ,fg-dim)))
   `(modus-theme-subtle-magenta ((,class :background ,magenta-subtle-bg :foreground ,fg-dim)))
   `(modus-theme-subtle-cyan ((,class :background ,cyan-subtle-bg :foreground ,fg-dim)))
   `(modus-theme-subtle-neutral ((,class :background ,bg-inactive :foreground ,fg-inactive)))
;;;;; intense coloured backgrounds
   `(modus-theme-intense-red ((,class :background ,red-intense-bg :foreground ,fg-main)))
   `(modus-theme-intense-green ((,class :background ,green-intense-bg :foreground ,fg-main)))
   `(modus-theme-intense-yellow ((,class :background ,yellow-intense-bg :foreground ,fg-main)))
   `(modus-theme-intense-blue ((,class :background ,blue-intense-bg :foreground ,fg-main)))
   `(modus-theme-intense-magenta ((,class :background ,magenta-intense-bg :foreground ,fg-main)))
   `(modus-theme-intense-cyan ((,class :background ,cyan-intense-bg :foreground ,fg-main)))
   `(modus-theme-intense-neutral ((,class :background ,bg-active :foreground ,fg-main)))
;;;;; refined background and foreground combinations
   ;; general purpose styles that use an accented foreground against an
   ;; accented background
   `(modus-theme-refine-red ((,class :background ,red-refine-bg :foreground ,red-refine-fg)))
   `(modus-theme-refine-green ((,class :background ,green-refine-bg :foreground ,green-refine-fg)))
   `(modus-theme-refine-yellow ((,class :background ,yellow-refine-bg :foreground ,yellow-refine-fg)))
   `(modus-theme-refine-blue ((,class :background ,blue-refine-bg :foreground ,blue-refine-fg)))
   `(modus-theme-refine-magenta ((,class :background ,magenta-refine-bg :foreground ,magenta-refine-fg)))
   `(modus-theme-refine-cyan ((,class :background ,cyan-refine-bg :foreground ,cyan-refine-fg)))
;;;;; "active" combinations, mostly for use on the mode line
   `(modus-theme-active-red ((,class :background ,red-active :foreground ,bg-active)))
   `(modus-theme-active-green ((,class :background ,green-active :foreground ,bg-active)))
   `(modus-theme-active-yellow ((,class :background ,yellow-active :foreground ,bg-active)))
   `(modus-theme-active-blue ((,class :background ,blue-active :foreground ,bg-active)))
   `(modus-theme-active-magenta ((,class :background ,magenta-active :foreground ,bg-active)))
   `(modus-theme-active-cyan ((,class :background ,cyan-active :foreground ,bg-active)))
;;;;; nuanced backgrounds
   ;; useful for adding an accented background that is suitable for all
   ;; main foreground colours (intended for use in Org source blocks)
   `(modus-theme-nuanced-red ((,class :background ,red-nuanced-bg
                                      ,@(and (>= emacs-major-version 27) '(:extend t)))))
   `(modus-theme-nuanced-green ((,class :background ,green-nuanced-bg
                                        ,@(and (>= emacs-major-version 27) '(:extend t)))))
   `(modus-theme-nuanced-yellow ((,class :background ,yellow-nuanced-bg
                                         ,@(and (>= emacs-major-version 27) '(:extend t)))))
   `(modus-theme-nuanced-blue ((,class :background ,blue-nuanced-bg
                                       ,@(and (>= emacs-major-version 27) '(:extend t)))))
   `(modus-theme-nuanced-magenta ((,class :background ,magenta-nuanced-bg
                                          ,@(and (>= emacs-major-version 27) '(:extend t)))))
   `(modus-theme-nuanced-cyan ((,class :background ,cyan-nuanced-bg
                                       ,@(and (>= emacs-major-version 27) '(:extend t)))))
;;;;; fringe-specific combinations
   `(modus-theme-fringe-red ((,class :background ,red-fringe-bg :foreground ,fg-main)))
   `(modus-theme-fringe-green ((,class :background ,green-fringe-bg :foreground ,fg-main)))
   `(modus-theme-fringe-yellow ((,class :background ,yellow-fringe-bg :foreground ,fg-main)))
   `(modus-theme-fringe-blue ((,class :background ,blue-fringe-bg :foreground ,fg-main)))
   `(modus-theme-fringe-magenta ((,class :background ,magenta-fringe-bg :foreground ,fg-main)))
   `(modus-theme-fringe-cyan ((,class :background ,cyan-fringe-bg :foreground ,fg-main)))
;;;;; special base values
   ;; these are closer to the grayscale than the accents defined above
   ;; and should only be used when the next closest alternative would be
   ;; a greyscale value than an accented one
   `(modus-theme-special-cold ((,class :background ,bg-special-cold :foreground ,fg-special-cold)))
   `(modus-theme-special-mild ((,class :background ,bg-special-mild :foreground ,fg-special-mild)))
   `(modus-theme-special-warm ((,class :background ,bg-special-warm :foreground ,fg-special-warm)))
   `(modus-theme-special-calm ((,class :background ,bg-special-calm :foreground ,fg-special-calm)))
;;;;; diff-specific combinations
   ;; intended for `diff-mode' or equivalent
   `(modus-theme-diff-added
     ((,class ,@(modus-vivendi-theme-diff
                 bg-main green
                 bg-diff-focus-added fg-diff-focus-added
                 green-nuanced-bg fg-diff-added))))
   `(modus-theme-diff-changed
     ((,class ,@(modus-vivendi-theme-diff
                 bg-main yellow
                 bg-diff-focus-changed fg-diff-focus-changed
                 yellow-nuanced-bg fg-diff-changed))))
   `(modus-theme-diff-removed
     ((,class ,@(modus-vivendi-theme-diff
                 bg-main red
                 bg-diff-focus-removed fg-diff-focus-removed
                 red-nuanced-bg fg-diff-removed))))
   `(modus-theme-diff-refine-added
     ((,class ,@(modus-vivendi-theme-diff
                 bg-diff-added fg-diff-added
                 bg-diff-refine-added fg-diff-refine-added
                 bg-diff-focus-added fg-diff-focus-added))))
   `(modus-theme-diff-refine-changed
     ((,class ,@(modus-vivendi-theme-diff
                 bg-diff-changed fg-diff-changed
                 bg-diff-refine-changed fg-diff-refine-changed
                 bg-diff-focus-changed fg-diff-focus-changed))))
   `(modus-theme-diff-refine-removed
     ((,class ,@(modus-vivendi-theme-diff
                 bg-diff-removed fg-diff-removed
                 bg-diff-refine-removed fg-diff-refine-removed
                 bg-diff-focus-removed fg-diff-focus-removed))))
   `(modus-theme-diff-focus-added
     ((,class ,@(modus-vivendi-theme-diff
                 bg-dim green
                 bg-diff-focus-added fg-diff-focus-added
                 bg-diff-added fg-diff-added))))
   `(modus-theme-diff-focus-changed
     ((,class ,@(modus-vivendi-theme-diff
                 bg-dim yellow
                 bg-diff-focus-changed fg-diff-focus-changed
                 bg-diff-changed fg-diff-changed))))
   `(modus-theme-diff-focus-removed
     ((,class ,@(modus-vivendi-theme-diff
                 bg-dim red
                 bg-diff-focus-removed fg-diff-focus-removed
                 bg-diff-removed fg-diff-removed))))
   `(modus-theme-diff-heading
     ((,class ,@(modus-vivendi-theme-diff
                 bg-alt blue-alt
                 bg-diff-heading fg-diff-heading
                 blue-nuanced-bg blue))))
;;;;; mark indicators
   ;; colour combinations intended for Dired, Ibuffer, or equivalent
   `(modus-theme-pseudo-header ((,class :inherit bold :foreground ,fg-main)))
   `(modus-theme-mark-alt ((,class :inherit bold :background ,bg-mark-alt :foreground ,fg-mark-alt)))
   `(modus-theme-mark-del ((,class :inherit bold :background ,bg-mark-del :foreground ,fg-mark-del)))
   `(modus-theme-mark-sel ((,class :inherit bold :background ,bg-mark-sel :foreground ,fg-mark-sel)))
   `(modus-theme-mark-symbol ((,class :inherit bold :foreground ,blue-alt)))
;;;;; heading levels
   ;; styles for regular headings used in Org, Markdown, Info, etc.
   `(modus-theme-heading-1
     ((,class ,@(modus-vivendi-theme-heading
                 1 fg-main magenta-alt-other magenta-nuanced-bg bg-region)
              ,@(modus-vivendi-theme-scale modus-vivendi-theme-scale-4))))
   `(modus-theme-heading-2
     ((,class ,@(modus-vivendi-theme-heading
                 2 fg-special-warm magenta-alt red-nuanced-bg bg-region)
              ,@(modus-vivendi-theme-scale modus-vivendi-theme-scale-3))))
   `(modus-theme-heading-3
     ((,class ,@(modus-vivendi-theme-heading
                 3 fg-special-cold blue blue-nuanced-bg bg-region)
              ,@(modus-vivendi-theme-scale modus-vivendi-theme-scale-2))))
   `(modus-theme-heading-4
     ((,class ,@(modus-vivendi-theme-heading
                 4 fg-special-mild cyan cyan-nuanced-bg bg-region)
              ,@(modus-vivendi-theme-scale modus-vivendi-theme-scale-1))))
   `(modus-theme-heading-5
     ((,class ,@(modus-vivendi-theme-heading
                 5 fg-special-calm green-alt-other green-nuanced-bg bg-region))))
   `(modus-theme-heading-6
     ((,class ,@(modus-vivendi-theme-heading
                 6 yellow-nuanced yellow-alt-other yellow-nuanced-bg bg-region))))
   `(modus-theme-heading-7
     ((,class ,@(modus-vivendi-theme-heading
                 7 red-nuanced red-alt red-nuanced-bg bg-region))))
   `(modus-theme-heading-8
     ((,class ,@(modus-vivendi-theme-heading
                 8 fg-dim magenta bg-alt bg-region))))
;;;;; other custom faces
   `(modus-theme-hl-line ((,class :background ,(if modus-vivendi-theme-intense-hl-line
                                                   bg-hl-line-intense bg-hl-line)
                                  (and (>= emacs-major-version 27) '(:extend t)))))
;;;; standard faces
;;;;; absolute essentials
   `(default ((,class :background ,bg-main :foreground ,fg-main)))
   `(cursor ((,class :background ,fg-main)))
   `(fringe ((,class ,@(modus-vivendi-theme-fringe bg-inactive bg-active)
                     :foreground ,fg-main)))
   `(vertical-border ((,class :foreground ,fg-window-divider-inner)))
;;;;; basic and/or ungrouped styles
   ;; Modify the `bold' face to change the weight of all "bold" elements
   ;; defined by the theme.  You need a typeface that supports a
   ;; multitude of heavier weights than the regular one and then you
   ;; must specify the exact name of the one you wish to apply.  Example
   ;; for your init.el:
   ;;
   ;; (set-face-attribute 'bold nil :weight 'semibold)
   `(bold ((,class :weight bold)))
   `(comint-highlight-input ((,class :inherit bold)))
   `(comint-highlight-prompt ((,class ,@(modus-vivendi-theme-bold-weight)
                                      ,@(modus-vivendi-theme-prompt
                                         cyan
                                         blue-nuanced-bg blue-alt
                                         blue-refine-bg fg-main))))
   `(error ((,class :inherit bold :foreground ,red)))
   `(escape-glyph ((,class :foreground ,fg-escape-char-construct)))
   `(file-name-shadow ((,class :foreground ,fg-unfocused)))
   `(header-line ((,class :background ,bg-header :foreground ,fg-header)))
   `(header-line-highlight ((,class :inherit modus-theme-active-blue)))
   `(help-argument-name ((,class :foreground ,cyan :slant ,modus-theme-slant)))
   `(homoglyph ((,class :foreground ,fg-escape-char-construct)))
   `(ibuffer-locked-buffer ((,class :foreground ,yellow-alt-other)))
   `(italic ((,class :slant italic)))
   `(nobreak-hyphen ((,class :foreground ,fg-escape-char-construct)))
   `(nobreak-space ((,class :foreground ,fg-escape-char-construct :underline t)))
   `(minibuffer-prompt ((,class ,@(modus-vivendi-theme-prompt
                                   cyan-alt-other
                                   cyan-nuanced-bg cyan
                                   cyan-refine-bg fg-main))))
   `(mm-command-output ((,class :foreground ,red-alt-other)))
   `(mm-uu-extract ((,class :background ,bg-dim :foreground ,fg-special-mild)))
   `(next-error ((,class :inherit modus-theme-subtle-red)))
   `(rectangle-preview ((,class :inherit modus-theme-special-mild)))
   `(region ((,class :background ,bg-region :foreground ,fg-main)))
   `(secondary-selection ((,class :inherit modus-theme-special-cold)))
   `(shadow ((,class :foreground ,fg-alt)))
   `(success ((,class :inherit bold :foreground ,green)))
   `(trailing-whitespace ((,class :background ,red-intense-bg)))
   `(warning ((,class :inherit bold :foreground ,yellow)))
;;;;; buttons, links, widgets
   `(button ((,class :foreground ,blue-alt-other
                     ,@(unless modus-vivendi-theme-no-link-underline
                         (list :underline t)))))
   `(link ((,class :inherit button)))
   `(link-visited ((,class :inherit link :foreground ,magenta-alt-other)))
   `(tooltip ((,class :background ,bg-special-cold :foreground ,fg-main)))
   `(widget-button ((,class :inherit button)))
   `(widget-button-pressed ((,class :inherit button :foreground ,magenta)))
   `(widget-documentation ((,class :foreground ,green)))
   `(widget-field ((,class :background ,bg-alt :foreground ,fg-dim)))
   `(widget-inactive ((,class :background ,bg-inactive :foreground ,fg-inactive)))
   `(widget-single-line-field ((,class :inherit widget-field)))
;;;;; ag
   `(ag-hit-face ((,class :foreground ,fg-special-cold)))
   `(ag-match-face ((,class :inherit modus-theme-special-calm)))
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
   `(annotate-annotation ((,class :inherit modus-theme-subtle-blue)))
   `(annotate-annotation-secondary ((,class :inherit modus-theme-subtle-green)))
   `(annotate-highlight ((,class :background ,blue-nuanced-bg :underline ,blue-intense)))
   `(annotate-highlight-secondary ((,class :background ,green-nuanced-bg :underline ,green-intense)))
;;;;; anzu
   `(anzu-match-1 ((,class :inherit modus-theme-subtle-cyan)))
   `(anzu-match-2 ((,class :inherit modus-theme-subtle-green)))
   `(anzu-match-3 ((,class :inherit modus-theme-subtle-yellow)))
   `(anzu-mode-line ((,class :inherit bold :foreground ,green-active)))
   `(anzu-mode-line-no-match ((,class :inherit bold :foreground ,red-active)))
   `(anzu-replace-highlight ((,class :inherit modus-theme-refine-yellow :underline t)))
   `(anzu-replace-to ((,class :inherit (modus-theme-intense-green bold))))
;;;;; apropos
   `(apropos-function-button ((,class :inherit button :foreground ,magenta-alt-other)))
   `(apropos-keybinding ((,class :inherit bold :foreground ,cyan)))
   `(apropos-misc-button ((,class :inherit button :foreground ,cyan-alt-other)))
   `(apropos-property ((,class ,@(modus-vivendi-theme-bold-weight) :foreground ,magenta-alt)))
   `(apropos-symbol ((,class ,@(modus-vivendi-theme-bold-weight) :foreground ,blue-alt-other)))
   `(apropos-user-option-button ((,class :inherit button :foreground ,green-alt-other)))
   `(apropos-variable-button ((,class :inherit button :foreground ,blue)))
;;;;; apt-sources-list
   `(apt-sources-list-components ((,class :foreground ,cyan)))
   `(apt-sources-list-options ((,class :foreground ,yellow)))
   `(apt-sources-list-suite ((,class :foreground ,green)))
   `(apt-sources-list-type ((,class :foreground ,magenta)))
   `(apt-sources-list-uri ((,class :foreground ,blue)))
;;;;; artbollocks-mode
   `(artbollocks-face ((,class :foreground ,cyan-nuanced :underline ,fg-lang-note)))
   `(artbollocks-lexical-illusions-face ((,class :background ,bg-alt :foreground ,red-alt :underline t)))
   `(artbollocks-passive-voice-face ((,class :foreground ,yellow-nuanced :underline ,fg-lang-warning)))
   `(artbollocks-weasel-words-face ((,class :foreground ,red-nuanced :underline ,fg-lang-error)))
;;;;; auctex and Tex
   `(font-latex-bold-face ((,class :inherit bold :foreground ,fg-special-calm)))
   `(font-latex-doctex-documentation-face ((,class :foreground ,fg-special-cold :slant ,modus-theme-slant)))
   `(font-latex-doctex-preprocessor-face ((,class ,@(modus-vivendi-theme-bold-weight) :foreground ,red-alt-other)))
   `(font-latex-italic-face ((,class :foreground ,fg-special-calm :slant italic)))
   `(font-latex-math-face ((,class :foreground ,cyan-alt-other)))
   `(font-latex-script-char-face ((,class :foreground ,cyan-alt-other)))
   `(font-latex-sectioning-0-face ((,class :inherit ,modus-theme-variable-pitch :foreground ,blue-nuanced)))
   `(font-latex-sectioning-1-face ((,class :inherit (bold ,modus-theme-variable-pitch) :foreground ,blue-nuanced)))
   `(font-latex-sectioning-2-face ((,class :inherit (bold ,modus-theme-variable-pitch) :foreground ,blue-nuanced)))
   `(font-latex-sectioning-3-face ((,class :inherit (bold ,modus-theme-variable-pitch) :foreground ,blue-nuanced)))
   `(font-latex-sectioning-4-face ((,class :inherit (bold ,modus-theme-variable-pitch) :foreground ,blue-nuanced)))
   `(font-latex-sectioning-5-face ((,class :inherit ,modus-theme-variable-pitch :foreground ,blue-nuanced)))
   `(font-latex-sedate-face ((,class ,@(modus-vivendi-theme-bold-weight) :foreground ,magenta-alt-other)))
   `(font-latex-slide-title-face ((,class :inherit (bold ,modus-theme-variable-pitch) :foreground ,cyan-nuanced
                                          ,@(modus-vivendi-theme-scale modus-vivendi-theme-scale-4))))
   `(font-latex-string-face ((,class :foreground ,blue-alt)))
   `(font-latex-subscript-face ((,class :height 0.95)))
   `(font-latex-superscript-face ((,class :height 0.95)))
   `(font-latex-verbatim-face ((,class :background ,bg-dim :foreground ,fg-special-mild)))
   `(font-latex-warning-face ((,class :foreground ,yellow-alt-other)))
   `(tex-match ((,class :foreground ,blue-alt-other)))
   `(tex-verbatim ((,class :background ,bg-dim :foreground ,fg-special-mild)))
   `(texinfo-heading ((,class :foreground ,magenta)))
   `(TeX-error-description-error ((,class :inherit bold :foreground ,red)))
   `(TeX-error-description-help ((,class :foreground ,blue)))
   `(TeX-error-description-tex-said ((,class :foreground ,blue)))
   `(TeX-error-description-warning ((,class :inherit bold :foreground ,yellow)))
;;;;; auto-dim-other-buffers
   `(auto-dim-other-buffers-face ((,class :background ,bg-alt)))
;;;;; avy
   `(avy-background-face ((,class :background ,bg-dim :foreground ,fg-dim)))
   `(avy-goto-char-timer-face ((,class :inherit (modus-theme-intense-yellow bold))))
   `(avy-lead-face ((,class :inherit (modus-theme-intense-magenta bold))))
   `(avy-lead-face-0 ((,class :inherit (modus-theme-intense-blue bold))))
   `(avy-lead-face-1 ((,class :inherit (modus-theme-intense-red bold))))
   `(avy-lead-face-2 ((,class :inherit (modus-theme-intense-green bold))))
;;;;; aw (ace-window)
   `(aw-background-face ((,class :background ,bg-dim :foreground ,fg-dim)))
   `(aw-key-face ((,class :inherit bold :foreground ,blue-intense)))
   `(aw-leading-char-face ((,class :inherit bold :height 1.5 :background ,bg-main :foreground ,red-intense)))
   `(aw-minibuffer-leading-char-face ((,class :foreground ,magenta-active)))
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
;;;;; binder
   `(binder-sidebar-highlight ((,class :inherit modus-theme-subtle-cyan)))
   `(binder-sidebar-marked ((,class :inherit modus-theme-mark-sel)))
   `(binder-sidebar-missing ((,class :inherit modus-theme-subtle-red)))
   `(binder-sidebar-tags ((,class :foreground ,cyan)))
;;;;; bm
   `(bm-face ((,class :inherit modus-theme-subtle-yellow
                      ,@(and (>= emacs-major-version 27) '(:extend t)))))
   `(bm-fringe-face ((,class :inherit modus-theme-fringe-yellow)))
   `(bm-fringe-persistent-face ((,class :inherit modus-theme-fringe-blue)))
   `(bm-persistent-face ((,class :inherit modus-theme-intense-blue
                                 ,@(and (>= emacs-major-version 27) '(:extend t)))))
;;;;; bongo
   `(bongo-album-title ((,class :foreground ,cyan-active)))
   `(bongo-artist ((,class :foreground ,magenta-active)))
   `(bongo-currently-playing-track ((,class :inherit bold)))
   `(bongo-elapsed-track-part ((,class :inherit modus-theme-subtle-magenta :underline t)))
   `(bongo-filled-seek-bar ((,class :background ,blue-subtle-bg :foreground ,fg-main)))
   `(bongo-marked-track ((,class :foreground ,fg-mark-alt)))
   `(bongo-marked-track-line ((,class :background ,bg-mark-alt)))
   `(bongo-played-track ((,class :foreground ,fg-unfocused :strike-through t)))
   `(bongo-track-length ((,class :foreground ,blue-alt-other)))
   `(bongo-track-title ((,class :foreground ,blue-active)))
   `(bongo-unfilled-seek-bar ((,class :background ,blue-nuanced-bg :foreground ,fg-main)))
;;;;; boon
   `(boon-modeline-cmd ((,class :inherit modus-theme-active-blue)))
   `(boon-modeline-ins ((,class :inherit modus-theme-active-red)))
   `(boon-modeline-off ((,class :inherit modus-theme-active-yellow)))
   `(boon-modeline-spc ((,class :inherit modus-theme-active-green)))
;;;;; breakpoint (built-in gdb-mi.el)
   `(breakpoint-disabled ((,class :foreground ,fg-alt)))
   `(breakpoint-enabled ((,class :inherit bold :foreground ,red)))
;;;;; buffer-expose
   `(buffer-expose-ace-char-face ((,class :inherit bold :foreground ,red-active)))
   `(buffer-expose-mode-line-face ((,class :foreground ,cyan-active)))
   `(buffer-expose-selected-face ((,class :inherit modus-theme-special-mild)))
;;;;; calendar and diary
   `(calendar-month-header ((,class :inherit bold :foreground ,fg-main)))
   `(calendar-today ((,class :underline t)))
   `(calendar-weekday-header ((,class :foreground ,fg-dim)))
   `(calendar-weekend-header ((,class :foreground ,fg-alt)))
   `(diary ((,class :foreground ,cyan-alt-other)))
   `(diary-anniversary ((,class :foreground ,red-alt-other)))
   `(diary-time ((,class :foreground ,blue-alt)))
   `(holiday ((,class :foreground ,magenta-alt)))
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
   `(cfw:face-select ((,class :inherit modus-theme-intense-blue)))
   `(cfw:face-sunday ((,class :inherit bold :foreground ,cyan-alt-other)))
   `(cfw:face-title ((,class :inherit ,modus-theme-variable-pitch
                             :foreground ,fg-special-cold
                             ,@(modus-vivendi-theme-scale modus-vivendi-theme-scale-5))))
   `(cfw:face-today ((,class :background ,bg-inactive)))
   `(cfw:face-today-title ((,class :background ,bg-active)))
   `(cfw:face-toolbar ((,class :background ,bg-alt :foreground ,bg-alt)))
   `(cfw:face-toolbar-button-off ((,class :foreground ,fg-alt)))
   `(cfw:face-toolbar-button-on ((,class :inherit bold :background ,blue-nuanced-bg
                                         :foreground ,blue-alt)))
;;;;; centaur-tabs
   `(centaur-tabs-active-bar-face ((,class :background ,fg-tab-active)))
   `(centaur-tabs-close-mouse-face ((,class :inherit bold :foreground ,red-active :underline t)))
   `(centaur-tabs-close-selected ((,class :inherit centaur-tabs-selected)))
   `(centaur-tabs-close-unselected ((,class :inherit centaur-tabs-unselected)))
   `(centaur-tabs-modified-marker-selected ((,class :inherit centaur-tabs-selected)))
   `(centaur-tabs-modified-marker-unselected ((,class :inherit centaur-tabs-unselected)))
   `(centaur-tabs-default ((,class :background ,bg-main :foreground ,bg-main)))
   `(centaur-tabs-selected ((,class :inherit bold :background ,bg-tab-active :foreground ,fg-main)))
   `(centaur-tabs-selected-modified ((,class :background ,bg-tab-active :foreground ,fg-main :slant italic)))
   `(centaur-tabs-unselected ((,class :background ,bg-tab-inactive :foreground ,fg-dim)))
   `(centaur-tabs-unselected-modified ((,class :background ,bg-tab-inactive :foreground ,fg-dim :slant italic)))
;;;;; change-log and log-view (`vc-print-log' and `vc-print-root-log')
   `(change-log-acknowledgment ((,class :foreground ,fg-alt)))
   `(change-log-conditionals ((,class :foreground ,magenta-alt)))
   `(change-log-date ((,class :foreground ,cyan-alt-other)))
   `(change-log-email ((,class :foreground ,cyan)))
   `(change-log-file ((,class :foreground ,blue)))
   `(change-log-function ((,class :foreground ,green-alt-other)))
   `(change-log-list ((,class :foreground ,magenta-alt-other)))
   `(change-log-name ((,class :foreground ,cyan)))
   `(log-edit-header ((,class :foreground ,fg-special-warm)))
   `(log-edit-summary ((,class :inherit bold :foreground ,cyan)))
   `(log-edit-unknown-header ((,class :foreground ,fg-alt)))
   `(log-view-file ((,class :inherit bold :foreground ,fg-special-cold)))
   `(log-view-message ((,class :foreground ,fg-alt)))
;;;;; cider
   `(cider-debug-code-overlay-face ((,class :background ,bg-alt)))
   `(cider-debug-prompt-face ((,class :foreground ,magenta-alt :underline t)))
   `(cider-deprecated-face ((,class :inherit modus-theme-refine-yellow)))
   `(cider-docview-emphasis-face ((,class :foreground ,fg-special-cold :slant italic)))
   `(cider-docview-literal-face ((,class :foreground ,blue-alt)))
   `(cider-docview-strong-face ((,class :inherit bold :foreground ,fg-special-cold)))
   `(cider-docview-table-border-face ((,class :foreground ,fg-alt)))
   `(cider-enlightened-face ((,class :box (:line-width -1 :color ,yellow-alt :style nil) :background ,bg-dim)))
   `(cider-enlightened-local-face ((,class :inherit bold :foreground ,yellow-alt-other)))
   `(cider-error-highlight-face ((,class :foreground ,red :underline t)))
   `(cider-fragile-button-face ((,class :box (:line-width 3 :color ,fg-alt :style released-button) :foreground ,yellow)))
   `(cider-fringe-good-face ((,class :foreground ,green-active)))
   `(cider-instrumented-face ((,class :box (:line-width -1 :color ,red :style nil) :background ,bg-dim)))
   `(cider-reader-conditional-face ((,class :foreground ,fg-special-warm :slant italic)))
   `(cider-repl-input-face ((,class :inherit bold)))
   `(cider-repl-prompt-face ((,class :foreground ,cyan-alt-other)))
   `(cider-repl-stderr-face ((,class :inherit bold :foreground ,red)))
   `(cider-repl-stdout-face ((,class :foreground ,blue)))
   `(cider-result-overlay-face ((,class :box (:line-width -1 :color ,blue :style nil) :background ,bg-dim)))
   `(cider-stacktrace-error-class-face ((,class :inherit bold :foreground ,red)))
   `(cider-stacktrace-error-message-face ((,class :foreground ,red-alt-other :slant italic)))
   `(cider-stacktrace-face ((,class :foreground ,fg-main)))
   `(cider-stacktrace-filter-active-face ((,class :foreground ,cyan-alt :underline t)))
   `(cider-stacktrace-filter-inactive-face ((,class :foreground ,cyan-alt)))
   `(cider-stacktrace-fn-face ((,class :inherit bold :foreground ,fg-main)))
   `(cider-stacktrace-ns-face ((,class :foreground ,fg-alt :slant italic)))
   `(cider-stacktrace-promoted-button-face ((,class :box (:line-width 3 :color ,fg-alt :style released-button) :foreground ,red)))
   `(cider-stacktrace-suppressed-button-face ((,class :box (:line-width 3 :color ,fg-alt :style pressed-button)
                                                      :background ,bg-alt :foreground ,fg-alt)))
   `(cider-test-error-face ((,class :inherit modus-theme-subtle-red)))
   `(cider-test-failure-face ((,class :inherit (modus-theme-intense-red bold))))
   `(cider-test-success-face ((,class :inherit modus-theme-intense-green)))
   `(cider-traced-face ((,class :box (:line-width -1 :color ,cyan :style nil) :background ,bg-dim)))
   `(cider-warning-highlight-face ((,class :foreground ,yellow :underline t)))
;;;;; circe (and lui)
   `(circe-fool-face ((,class :foreground ,fg-alt)))
   `(circe-highlight-nick-face ((,class :inherit bold :foreground ,blue)))
   `(circe-prompt-face ((,class :inherit bold :foreground ,cyan-alt-other)))
   `(circe-server-face ((,class :foreground ,fg-unfocused)))
   `(lui-button-face ((,class :inherit button :foreground ,blue)))
   `(lui-highlight-face ((,class :foreground ,magenta-alt)))
   `(lui-time-stamp-face ((,class :foreground ,blue-nuanced)))
;;;;; color-rg
   `(color-rg-font-lock-column-number ((,class :foreground ,magenta-alt-other)))
   `(color-rg-font-lock-command ((,class :inherit bold :foreground ,fg-main)))
   `(color-rg-font-lock-file ((,class :inherit bold :foreground ,fg-special-cold)))
   `(color-rg-font-lock-flash ((,class :inherit modus-theme-intense-blue)))
   `(color-rg-font-lock-function-location ((,class :inherit modus-theme-special-calm)))
   `(color-rg-font-lock-header-line-directory ((,class :foreground ,blue-active)))
   `(color-rg-font-lock-header-line-edit-mode ((,class :foreground ,magenta-active)))
   `(color-rg-font-lock-header-line-keyword ((,class :foreground ,green-active)))
   `(color-rg-font-lock-header-line-text ((,class :foreground ,fg-active)))
   `(color-rg-font-lock-line-number ((,class :foreground ,fg-special-warm)))
   `(color-rg-font-lock-mark-changed ((,class :inherit bold :foreground ,blue)))
   `(color-rg-font-lock-mark-deleted ((,class :inherit bold :foreground ,red)))
   `(color-rg-font-lock-match ((,class :inherit modus-theme-special-calm)))
   `(color-rg-font-lock-position-splitter ((,class :foreground ,fg-alt)))
;;;;; column-enforce-mode
   `(column-enforce-face ((,class :inherit modus-theme-refine-yellow)))
;;;;; company-mode
   `(company-echo-common ((,class :foreground ,magenta-alt-other)))
   `(company-preview ((,class :background ,bg-dim :foreground ,fg-dim)))
   `(company-preview-common ((,class :foreground ,blue-alt)))
   `(company-preview-search ((,class :inherit modus-theme-special-calm)))
   `(company-scrollbar-bg ((,class :background ,bg-active)))
   `(company-scrollbar-fg ((,class :background ,fg-active)))
   `(company-template-field ((,class :inherit modus-theme-intense-magenta)))
   `(company-tooltip ((,class :background ,bg-alt :foreground ,fg-alt)))
   `(company-tooltip-annotation ((,class :foreground ,fg-special-cold :slant ,modus-theme-slant)))
   `(company-tooltip-annotation-selection ((,class :inherit bold :foreground ,fg-main)))
   `(company-tooltip-common ((,class :inherit bold :foreground ,blue-alt)))
   `(company-tooltip-common-selection ((,class :foreground ,fg-main)))
   `(company-tooltip-mouse ((,class :inherit modus-theme-intense-blue)))
   `(company-tooltip-search ((,class :inherit (modus-theme-refine-cyan bold))))
   `(company-tooltip-search-selection ((,class :inherit (modus-theme-intense-green bold) :underline t)))
   `(company-tooltip-selection ((,class :inherit (modus-theme-subtle-cyan bold))))
;;;;; company-posframe
   `(company-posframe-active-backend-name ((,class :inherit bold :background ,bg-active :foreground ,blue-active)))
   `(company-posframe-inactive-backend-name ((,class :background ,bg-active :foreground ,fg-active)))
   `(company-posframe-metadata ((,class :background ,bg-inactive :foreground ,fg-inactive)))
;;;;; compilation feedback
   `(compilation-column-number ((,class :foreground ,magenta-alt-other)))
   `(compilation-error ((,class ,@(modus-vivendi-theme-bold-weight) :foreground ,red)))
   `(compilation-info ((,class :foreground ,fg-special-cold)))
   `(compilation-line-number ((,class :foreground ,fg-special-warm)))
   `(compilation-mode-line-exit ((,class ,@(modus-vivendi-theme-bold-weight) :foreground ,blue-active)))
   `(compilation-mode-line-fail ((,class ,@(modus-vivendi-theme-bold-weight) :foreground ,red-active)))
   `(compilation-mode-line-run ((,class ,@(modus-vivendi-theme-bold-weight) :foreground ,magenta-active)))
   `(compilation-warning ((,class ,@(modus-vivendi-theme-bold-weight) :foreground ,yellow)))
;;;;; completions
   `(completions-annotations ((,class :foreground ,fg-special-cold :slant ,modus-theme-slant)))
   `(completions-common-part ((,class ,@(modus-vivendi-theme-standard-completions
                                         blue-alt blue-nuanced-bg
                                         cyan-refine-bg cyan-refine-fg))))
   `(completions-first-difference ((,class :inherit bold
                                           ,@(modus-vivendi-theme-standard-completions
                                              magenta-alt blue-nuanced-bg
                                              magenta-intense-bg fg-main))))
;;;;; counsel
   `(counsel-active-mode ((,class :foreground ,magenta-alt-other)))
   `(counsel-application-name ((,class :foreground ,red-alt-other)))
   `(counsel-key-binding ((,class :inherit bold :foreground ,blue-alt-other)))
   `(counsel-outline-1 ((,class :inherit outline-1)))
   `(counsel-outline-2 ((,class :inherit outline-2)))
   `(counsel-outline-3 ((,class :inherit outline-3)))
   `(counsel-outline-4 ((,class :inherit outline-4)))
   `(counsel-outline-5 ((,class :inherit outline-5)))
   `(counsel-outline-6 ((,class :inherit outline-6)))
   `(counsel-outline-7 ((,class :inherit outline-7)))
   `(counsel-outline-8 ((,class :inherit outline-8)))
   `(counsel-outline-default ((,class :inherit bold :foreground ,green-alt-other)))
   `(counsel-variable-documentation ((,class :foreground ,yellow-alt-other :slant ,modus-theme-slant)))
;;;;; counsel-css
   `(counsel-css-selector-depth-face-1 ((,class :foreground ,blue)))
   `(counsel-css-selector-depth-face-2 ((,class :foreground ,cyan)))
   `(counsel-css-selector-depth-face-3 ((,class :foreground ,green)))
   `(counsel-css-selector-depth-face-4 ((,class :foreground ,yellow)))
   `(counsel-css-selector-depth-face-5 ((,class :foreground ,magenta)))
   `(counsel-css-selector-depth-face-6 ((,class :foreground ,red)))
;;;;; counsel-notmuch
   `(counsel-notmuch-count-face ((,class :foreground ,cyan)))
   `(counsel-notmuch-date-face ((,class :foreground ,blue)))
   `(counsel-notmuch-people-face ((,class :foreground ,magenta)))
   `(counsel-notmuch-subject-face ((,class :foreground ,magenta-alt-other)))
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
   `(cperl-nonoverridable-face ((,class :foreground ,yellow-alt-other)))
   `(cperl-array-face ((,class :inherit bold :background ,bg-alt :foreground ,magenta-alt)))
   `(cperl-hash-face ((,class :inherit bold :background ,bg-alt :foreground ,red-alt :slant ,modus-theme-slant)))
;;;;; csv-mode
   `(csv-separator-face ((,class :background ,bg-special-cold :foreground ,fg-main)))
;;;;; ctrlf
   `(ctrlf-highlight-active ((,class :inherit (modus-theme-intense-green bold))))
   `(ctrlf-highlight-line ((,class :inherit modus-theme-hl-line)))
   `(ctrlf-highlight-passive ((,class :inherit modus-theme-refine-cyan)))
;;;;; custom (M-x customize)
   `(custom-button ((,class :box (:line-width 2 :color nil :style released-button)
                            :background ,bg-active :foreground ,fg-main)))
   `(custom-button-mouse ((,class :box (:line-width 2 :color nil :style released-button)
                                  :background ,bg-active :foreground ,fg-active)))
   `(custom-button-pressed ((,class :box (:line-width 2 :color nil :style pressed-button)
                                    :background ,bg-active :foreground ,fg-main)))
   `(custom-changed ((,class :inherit modus-theme-subtle-cyan)))
   `(custom-comment ((,class :foreground ,fg-alt)))
   `(custom-comment-tag ((,class :background ,bg-alt :foreground ,yellow-alt-other)))
   `(custom-face-tag ((,class :inherit bold :foreground ,blue-intense)))
   `(custom-group-tag ((,class :inherit bold :foreground ,green-intense)))
   `(custom-group-tag-1 ((,class :inherit modus-theme-special-warm)))
   `(custom-invalid ((,class :inherit (modus-theme-intense-red bold))))
   `(custom-modified ((,class :inherit modus-theme-subtle-cyan)))
   `(custom-rogue ((,class :inherit modus-theme-refine-magenta)))
   `(custom-set ((,class :foreground ,blue-alt)))
   `(custom-state ((,class :foreground ,cyan-alt-other)))
   `(custom-themed ((,class :inherit modus-theme-subtle-blue)))
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
   `(dap-ui-locals-variable-leaf-face ((,class :foreground ,cyan-alt-other :slant italic)))
   `(dap-ui-marker-face ((,class :inherit modus-theme-subtle-blue)))
   `(dap-ui-sessions-stack-frame-face ((,class :inherit bold :foreground ,magenta-alt)))
   `(dap-ui-sessions-terminated-active-face ((,class :inherit bold :foreground ,fg-alt)))
   `(dap-ui-sessions-terminated-face ((,class :foreground ,fg-alt)))
;;;;; dashboard (emacs-dashboard)
   `(dashboard-banner-logo-title ((,class :inherit bold :foreground ,fg-special-cold)))
   `(dashboard-footer ((,class :inherit bold :foreground ,fg-special-mild)))
   `(dashboard-heading ((,class :inherit bold :foreground ,fg-special-warm)))
   `(dashboard-navigator ((,class :foreground ,cyan-alt-other)))
   `(dashboard-text-banner ((,class :foreground ,fg-dim)))
;;;;; deadgrep
   `(deadgrep-filename-face ((,class :inherit bold :foreground ,fg-special-cold)))
   `(deadgrep-match-face ((,class :inherit modus-theme-special-calm)))
   `(deadgrep-meta-face ((,class :foreground ,fg-alt)))
   `(deadgrep-regexp-metachar-face ((,class :inherit bold :foreground ,yellow-intense)))
   `(deadgrep-search-term-face ((,class :inherit bold :foreground ,green-intense)))
;;;;; debbugs
   `(debbugs-gnu-archived ((,class :inverse-video t)))
   `(debbugs-gnu-done ((,class :foreground ,fg-alt)))
   `(debbugs-gnu-forwarded ((,class :foreground ,fg-special-warm)))
   `(debbugs-gnu-handled ((,class :foreground ,green)))
   `(debbugs-gnu-new ((,class :foreground ,red)))
   `(debbugs-gnu-pending ((,class :foreground ,cyan)))
   `(debbugs-gnu-stale-1 ((,class :foreground ,yellow-nuanced)))
   `(debbugs-gnu-stale-2 ((,class :foreground ,yellow)))
   `(debbugs-gnu-stale-3 ((,class :foreground ,yellow-alt)))
   `(debbugs-gnu-stale-4 ((,class :foreground ,yellow-alt-other)))
   `(debbugs-gnu-stale-5 ((,class :foreground ,red-alt)))
   `(debbugs-gnu-tagged ((,class :foreground ,magenta-alt)))
;;;;; define-word
   `(define-word-face-1 ((,class :foreground ,yellow)))
   `(define-word-face-2 ((,class :foreground ,fg-main)))
;;;;; deft
   `(deft-filter-string-error-face ((,class :inherit modus-theme-refine-red)))
   `(deft-filter-string-face ((,class :foreground ,green-intense)))
   `(deft-header-face ((,class :inherit bold :foreground ,fg-special-warm)))
   `(deft-separator-face ((,class :foreground ,fg-alt)))
   `(deft-summary-face ((,class :foreground ,fg-alt :slant ,modus-theme-slant)))
   `(deft-time-face ((,class :foreground ,fg-special-cold)))
   `(deft-title-face ((,class :inherit bold :foreground ,fg-main)))
;;;;; dictionary
   `(dictionary-button-face ((,class :inherit bold :foreground ,fg-special-cold)))
   `(dictionary-reference-face ((,class :inherit button :foreground ,blue-alt-other)))
   `(dictionary-word-definition-face ((,class :foreground ,fg-main)))
   `(dictionary-word-entry-face ((,class :foreground ,fg-special-cold :slant ,modus-theme-slant)))
;;;;; diff-hl
   `(diff-hl-change ((,class :inherit modus-theme-fringe-yellow)))
   `(diff-hl-delete ((,class :inherit modus-theme-fringe-red)))
   `(diff-hl-dired-change ((,class :inherit diff-hl-change)))
   `(diff-hl-dired-delete ((,class :inherit diff-hl-delete)))
   `(diff-hl-dired-ignored ((,class :inherit dired-ignored)))
   `(diff-hl-dired-insert ((,class :inherit diff-hl-insert)))
   `(diff-hl-dired-unknown ((,class :inherit dired-ignored)))
   `(diff-hl-insert ((,class :inherit modus-theme-fringe-green)))
   `(diff-hl-reverted-hunk-highlight ((,class :inherit (modus-theme-active-magenta bold))))
;;;;; diff-mode
   `(diff-added ((,class :inherit modus-theme-diff-added)))
   `(diff-changed ((,class :inherit modus-theme-diff-changed)))
   `(diff-context ((,class :foreground ,fg-unfocused)))
   `(diff-file-header ((,class :inherit bold :foreground ,blue)))
   `(diff-function ((,class :foreground ,fg-special-cold)))
   `(diff-header ((,class :foreground ,blue-nuanced)))
   `(diff-hunk-header ((,class :inherit modus-theme-diff-heading)))
   `(diff-index ((,class :inherit bold :foreground ,blue-alt)))
   `(diff-indicator-added ((,class :inherit diff-added)))
   `(diff-indicator-changed ((,class :inherit diff-changed)))
   `(diff-indicator-removed ((,class :inherit diff-removed)))
   `(diff-nonexistent ((,class :inherit (modus-theme-neutral bold))))
   `(diff-refine-added ((,class :inherit modus-theme-diff-refine-added)))
   `(diff-refine-changed ((,class :inherit modus-theme-diff-refine-changed)))
   `(diff-refine-removed ((,class :inherit modus-theme-diff-refine-removed)))
   `(diff-removed ((,class :inherit modus-theme-diff-removed)))
;;;;; dim-autoload
   `(dim-autoload-cookie-line ((,class :foreground ,fg-alt :slant ,modus-theme-slant)))
;;;;; dir-treeview
   `(dir-treeview-archive-face ((,class :foreground ,fg-special-warm)))
   `(dir-treeview-archive-icon-face ((,class :inherit dir-treeview-default-icon-face :foreground ,yellow)))
   `(dir-treeview-audio-face ((,class :foreground ,magenta)))
   `(dir-treeview-audio-icon-face ((,class :inherit dir-treeview-default-icon-face :foreground ,magenta-alt)))
   `(dir-treeview-control-face ((,class :foreground ,fg-alt)))
   `(dir-treeview-control-mouse-face ((,class :inherit highlight)))
   `(dir-treeview-default-icon-face ((,class :inherit bold :family "Font Awesome" :foreground ,fg-alt)))
   `(dir-treeview-default-filename-face ((,class :foreground ,fg-main)))
   `(dir-treeview-directory-face ((,class :foreground ,blue)))
   `(dir-treeview-directory-icon-face ((,class :inherit dir-treeview-default-icon-face :foreground ,blue-alt)))
   `(dir-treeview-executable-face ((,class :foreground ,red-alt)))
   `(dir-treeview-executable-icon-face ((,class :inherit dir-treeview-default-icon-face :foreground ,red-alt-other)))
   `(dir-treeview-image-face ((,class :foreground ,green-alt-other)))
   `(dir-treeview-image-icon-face ((,class :inherit dir-treeview-default-icon-face :foreground ,green-alt)))
   `(dir-treeview-indent-face ((,class :foreground ,fg-alt)))
   `(dir-treeview-label-mouse-face ((,class :inherit highlight)))
   `(dir-treeview-start-dir-face ((,class :inherit modus-theme-pseudo-header)))
   `(dir-treeview-symlink-face ((,class :inherit button :foreground ,cyan)))
   `(dir-treeview-video-face ((,class :foreground ,magenta-alt-other)))
   `(dir-treeview-video-icon-face ((,class :inherit dir-treeview-default-icon-face :foreground ,magenta-alt-other)))
;;;;; dired
   `(dired-directory ((,class :foreground ,blue)))
   `(dired-flagged ((,class :inherit modus-theme-mark-del)))
   `(dired-header ((,class :inherit modus-theme-pseudo-header)))
   `(dired-ignored ((,class :foreground ,fg-alt)))
   `(dired-mark ((,class :inherit modus-theme-mark-symbol)))
   `(dired-marked ((,class :inherit modus-theme-mark-sel)))
   `(dired-perm-write ((,class :foreground ,fg-special-warm)))
   `(dired-symlink ((,class :inherit button :foreground ,cyan-alt)))
   `(dired-warning ((,class :inherit bold :foreground ,yellow)))
;;;;; dired-async
   `(dired-async-failures ((,class ,@(modus-vivendi-theme-bold-weight) :foreground ,red-active)))
   `(dired-async-message ((,class ,@(modus-vivendi-theme-bold-weight) :foreground ,green-active)))
   `(dired-async-mode-message ((,class ,@(modus-vivendi-theme-bold-weight) :foreground ,cyan-active)))
;;;;; dired-git
   `(dired-git-branch-else ((,class :inherit bold :foreground ,magenta-alt)))
   `(dired-git-branch-master ((,class :inherit bold :foreground ,magenta-alt-other)))
;;;;; dired-git-info
   `(dgi-commit-message-face ((,class :foreground ,fg-special-mild)))
;;;;; dired-narrow
   `(dired-narrow-blink ((,class :inherit (modus-theme-subtle-cyan bold))))
;;;;; dired-subtree
   ;; remove background from dired-subtree, else it breaks
   ;; dired-{flagged,marked} and any other face that sets a background
   ;; such as hl-line
   `(dired-subtree-depth-1-face ((,class :background nil)))
   `(dired-subtree-depth-2-face ((,class :background nil)))
   `(dired-subtree-depth-3-face ((,class :background nil)))
   `(dired-subtree-depth-4-face ((,class :background nil)))
   `(dired-subtree-depth-5-face ((,class :background nil)))
   `(dired-subtree-depth-6-face ((,class :background nil)))
;;;;; diredfl
   `(diredfl-autofile-name ((,class :inherit modus-theme-special-cold)))
   `(diredfl-compressed-file-name ((,class :foreground ,fg-special-warm)))
   `(diredfl-compressed-file-suffix ((,class :foreground ,red-alt)))
   `(diredfl-date-time ((,class :foreground ,cyan-alt-other)))
   `(diredfl-deletion ((,class :inherit modus-theme-mark-del)))
   `(diredfl-deletion-file-name ((,class :inherit modus-theme-mark-del)))
   `(diredfl-dir-heading ((,class :inherit modus-theme-pseudo-header)))
   `(diredfl-dir-name ((,class :inherit dired-directory)))
   `(diredfl-dir-priv ((,class :foreground ,blue-alt)))
   `(diredfl-exec-priv ((,class :foreground ,magenta)))
   `(diredfl-executable-tag ((,class :foreground ,magenta-alt)))
   `(diredfl-file-name ((,class :foreground ,fg-main)))
   `(diredfl-file-suffix ((,class :foreground ,cyan)))
   `(diredfl-flag-mark ((,class :inherit modus-theme-mark-sel)))
   `(diredfl-flag-mark-line ((,class :inherit modus-theme-mark-sel)))
   `(diredfl-ignored-file-name ((,class :foreground ,fg-alt)))
   `(diredfl-link-priv ((,class :foreground ,blue-alt-other)))
   `(diredfl-no-priv ((,class :foreground ,fg-alt)))
   `(diredfl-number ((,class :foreground ,cyan-alt)))
   `(diredfl-other-priv ((,class :foreground ,yellow)))
   `(diredfl-rare-priv ((,class :foreground ,red-alt)))
   `(diredfl-read-priv ((,class :foreground ,fg-main)))
   `(diredfl-symlink ((,class :inherit dired-symlink)))
   `(diredfl-tagged-autofile-name ((,class :inherit modus-theme-refine-magenta)))
   `(diredfl-write-priv ((,class :foreground ,cyan)))
;;;;; disk-usage
   `(disk-usage-children ((,class :foreground ,yellow)))
   `(disk-usage-inaccessible ((,class :inherit bold :foreground ,red)))
   `(disk-usage-percent ((,class :foreground ,green)))
   `(disk-usage-size ((,class :foreground ,cyan)))
   `(disk-usage-symlink ((,class :inherit button :foreground ,blue)))
   `(disk-usage-symlink-directory ((,class :inherit bold :foreground ,blue-alt)))
;;;;; doom-modeline
   `(doom-modeline-bar ((,class :inherit modus-theme-active-blue)))
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
   `(doom-modeline-host ((,class :slant italic)))
   `(doom-modeline-info ((,class :foreground ,green-active)))
   `(doom-modeline-lsp-error ((,class :inherit bold :foreground ,red-active)))
   `(doom-modeline-lsp-success ((,class :inherit bold :foreground ,green-active)))
   `(doom-modeline-lsp-warning ((,class :inherit bold :foreground ,yellow-active)))
   `(doom-modeline-panel ((,class :inherit modus-theme-active-blue)))
   `(doom-modeline-persp-buffer-not-in-persp ((,class :foreground ,yellow-active :slant italic)))
   `(doom-modeline-persp-name ((,class :foreground ,fg-active)))
   `(doom-modeline-project-dir ((,class :inherit bold :foreground ,blue-active)))
   `(doom-modeline-project-parent-dir ((,class :foreground ,blue-active)))
   `(doom-modeline-project-root-dir ((,class :foreground ,fg-active)))
   `(doom-modeline-unread-number ((,class :foreground ,fg-active :slant italic)))
   `(doom-modeline-urgent ((,class :inherit bold :foreground ,red-active)))
   `(doom-modeline-warning ((,class :inherit bold :foreground ,yellow-active)))
;;;;; dynamic-ruler
   `(dynamic-ruler-negative-face ((,class :inherit modus-theme-intense-neutral)))
   `(dynamic-ruler-positive-face ((,class :inherit modus-theme-intense-yellow)))
;;;;; easy-jekyll
   `(easy-jekyll-help-face ((,class :background ,bg-dim :foreground ,cyan-alt-other)))
;;;;; easy-kill
   `(easy-kill-origin ((,class :inherit modus-theme-subtle-red)))
   `(easy-kill-selection ((,class :inherit modus-theme-subtle-yellow)))
;;;;; ebdb
   `(ebdb-address-default ((,class :foreground ,fg-main)))
   `(ebdb-db-char ((,class :foreground ,fg-special-cold)))
   `(ebdb-defunct ((,class :foreground ,fg-alt)))
   `(ebdb-field-hidden ((,class :foreground ,magenta)))
   `(ebdb-field-url ((,class :foreground ,blue)))
   `(ebdb-label ((,class :foreground ,cyan-alt-other)))
   `(ebdb-mail-default ((,class :foreground ,fg-main)))
   `(ebdb-mail-primary ((,class :foreground ,blue-alt)))
   `(ebdb-marked ((,class :background ,cyan-intense-bg)))
   `(ebdb-organization-name ((,class :foreground ,fg-special-calm)))
   `(ebdb-person-name ((,class :foreground ,magenta-alt-other)))
   `(ebdb-phone-default ((,class :foreground ,fg-special-warm)))
   `(ebdb-role-defunct ((,class :foreground ,fg-alt)))
   `(eieio-custom-slot-tag-face ((,class :foreground ,red-alt)))
;;;;; ediff
   ;; NOTE: here we break from the pattern of inheriting from the
   ;; modus-theme-diff-* faces.
   `(ediff-current-diff-A ((,class ,@(modus-vivendi-theme-diff
                                      bg-dim red
                                      bg-diff-removed fg-diff-removed
                                      red-nuanced-bg red-faint))))
   `(ediff-current-diff-Ancestor ((,class ,@(modus-vivendi-theme-diff
                                             bg-dim fg-special-cold
                                             bg-special-cold fg-special-cold
                                             blue-nuanced-bg blue))))
   `(ediff-current-diff-B ((,class ,@(modus-vivendi-theme-diff
                                      bg-dim green
                                      bg-diff-added fg-diff-added
                                      green-nuanced-bg green-faint))))
   `(ediff-current-diff-C ((,class ,@(modus-vivendi-theme-diff
                                      bg-dim yellow
                                      bg-diff-changed fg-diff-changed
                                      yellow-nuanced-bg yellow-faint))))
   `(ediff-even-diff-A ((,class :background ,bg-diff-neutral-1 :foreground ,fg-diff-neutral-1)))
   `(ediff-even-diff-Ancestor ((,class :background ,bg-diff-neutral-2 :foreground ,fg-diff-neutral-1)))
   `(ediff-even-diff-B ((,class :background ,bg-diff-neutral-1 :foreground ,fg-diff-neutral-1)))
   `(ediff-even-diff-C ((,class :background ,bg-diff-neutral-2 :foreground ,fg-diff-neutral-2)))
   `(ediff-fine-diff-A ((,class :background ,bg-diff-focus-removed :foreground ,fg-diff-focus-removed)))
   `(ediff-fine-diff-Ancestor ((,class :inherit modus-theme-refine-cyan)))
   `(ediff-fine-diff-B ((,class :background ,bg-diff-focus-added :foreground ,fg-diff-focus-added)))
   `(ediff-fine-diff-C ((,class :background ,bg-diff-focus-changed :foreground ,fg-diff-focus-changed)))
   `(ediff-odd-diff-A ((,class :background ,bg-diff-neutral-2 :foreground ,fg-diff-neutral-2)))
   `(ediff-odd-diff-Ancestor ((,class :background ,bg-diff-neutral-0 :foreground ,fg-diff-neutral-0)))
   `(ediff-odd-diff-B ((,class :background ,bg-diff-neutral-2 :foreground ,fg-diff-neutral-2)))
   `(ediff-odd-diff-C ((,class :background ,bg-diff-neutral-1 :foreground ,fg-diff-neutral-1)))
;;;;; eglot
   `(eglot-mode-line ((,class ,@(modus-vivendi-theme-bold-weight) :foreground ,magenta-active)))
;;;;; el-search
   `(el-search-highlight-in-prompt-face ((,class :inherit bold :foreground ,magenta-alt)))
   `(el-search-match ((,class :inherit modus-theme-intense-green)))
   `(el-search-other-match ((,class :inherit modus-theme-special-mild)))
   `(el-search-occur-match ((,class :inherit modus-theme-special-calm)))
;;;;; eldoc
   ;; NOTE: see https://github.com/purcell/package-lint/issues/187
   (list 'eldoc-highlight-function-argument `((,class :inherit bold :foreground ,blue-alt-other)))
;;;;; eldoc-box
   `(eldoc-box-body ((,class :background ,bg-alt :foreground ,fg-main)))
   `(eldoc-box-border ((,class :background ,fg-alt)))
;;;;; elfeed
   `(elfeed-log-date-face ((,class :foreground ,cyan-alt)))
   `(elfeed-log-debug-level-face ((,class :foreground ,magenta)))
   `(elfeed-log-error-level-face ((,class :foreground ,red)))
   `(elfeed-log-info-level-face ((,class :foreground ,green)))
   `(elfeed-log-warn-level-face ((,class :foreground ,yellow)))
   `(elfeed-search-date-face ((,class :foreground ,blue-nuanced)))
   `(elfeed-search-feed-face ((,class :foreground ,cyan)))
   `(elfeed-search-filter-face ((,class :inherit bold :foreground ,magenta-active)))
   `(elfeed-search-last-update-face ((,class :foreground ,cyan-active)))
   `(elfeed-search-tag-face ((,class :foreground ,blue-nuanced)))
   `(elfeed-search-title-face ((,class :foreground ,fg-dim)))
   `(elfeed-search-unread-count-face ((,class :foreground ,green-active)))
   `(elfeed-search-unread-title-face ((,class :inherit bold :foreground ,fg-main)))
;;;;; elfeed-score
   `(elfeed-score-date-face ((,class :foreground ,blue)))
   `(elfeed-score-debug-level-face ((,class :foreground ,magenta-alt-other)))
   `(elfeed-score-error-level-face ((,class :foreground ,red)))
   `(elfeed-score-info-level-face ((,class :foreground ,cyan)))
   `(elfeed-score-warn-level-face ((,class :foreground ,yellow)))
;;;;; emms
   `(emms-playlist-track-face ((,class :foreground ,blue)))
   `(emms-playlist-selected-face ((,class :inherit bold :foreground ,magenta)))
;;;;; enhanced-ruby-mode
   `(enh-ruby-heredoc-delimiter-face ((,class :foreground ,blue-alt-other)))
   `(enh-ruby-op-face ((,class :foreground ,fg-main)))
   `(enh-ruby-regexp-delimiter-face ((,class :foreground ,green)))
   `(enh-ruby-regexp-face ((,class :foreground ,magenta)))
   `(enh-ruby-string-delimiter-face ((,class :foreground ,blue-alt)))
   `(erm-syn-errline ((,class :foreground ,red :underline t)))
   `(erm-syn-warnline ((,class :foreground ,yellow :underline t)))
;;;;; epa
   `(epa-field-body ((,class :foreground ,fg-main)))
   `(epa-field-name ((,class :inherit bold :foreground ,fg-dim)))
   `(epa-mark ((,class :inherit bold :foreground ,magenta)))
   `(epa-string ((,class :foreground ,blue-alt)))
   `(epa-validity-disabled ((,class :inherit modus-theme-refine-red)))
   `(epa-validity-high ((,class :inherit bold :foreground ,green-alt-other)))
   `(epa-validity-low ((,class :foreground ,fg-alt)))
   `(epa-validity-medium ((,class :foreground ,green-alt)))
;;;;; equake
   `(equake-buffer-face ((,class :background ,bg-main :foreground ,fg-main)))
   `(equake-shell-type-eshell ((,class :background ,bg-inactive :foreground ,green-active)))
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
   `(erc-dangerous-host-face ((,class :inherit modus-theme-intense-red)))
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
   `(erc-prompt-face ((,class :inherit bold :foreground ,cyan-alt-other)))
   `(erc-timestamp-face ((,class :foreground ,blue-nuanced)))
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
   `(ert-test-result-expected ((,class :inherit modus-theme-intense-green)))
   `(ert-test-result-unexpected ((,class :inherit modus-theme-intense-red)))
;;;;; eshell
   `(eshell-ls-archive ((,class :inherit bold :foreground ,cyan-alt)))
   `(eshell-ls-backup ((,class :foreground ,yellow-alt)))
   `(eshell-ls-clutter ((,class :foreground ,red-alt)))
   `(eshell-ls-directory ((,class :inherit bold :foreground ,blue-alt)))
   `(eshell-ls-executable ((,class :foreground ,magenta-alt)))
   `(eshell-ls-missing ((,class :inherit modus-theme-intense-red)))
   `(eshell-ls-product ((,class :foreground ,fg-special-warm)))
   `(eshell-ls-readonly ((,class :foreground ,fg-special-cold)))
   `(eshell-ls-special ((,class :inherit bold :foreground ,magenta)))
   `(eshell-ls-symlink ((,class :inherit button :foreground ,cyan)))
   `(eshell-ls-unreadable ((,class :background ,bg-inactive :foreground ,fg-inactive)))
   `(eshell-prompt ((,class ,@(modus-vivendi-theme-bold-weight)
                            ,@(modus-vivendi-theme-prompt
                               green-alt-other
                               green-nuanced-bg green-alt
                               green-refine-bg fg-main))))
;;;;; eshell-fringe-status
   `(eshell-fringe-status-failure ((,class :foreground ,red)))
   `(eshell-fringe-status-success ((,class :foreground ,green)))
;;;;; eshell-git-prompt
   `(eshell-git-prompt-add-face ((,class :foreground ,fg-alt)))
   `(eshell-git-prompt-branch-face ((,class :foreground ,fg-alt)))
   `(eshell-git-prompt-directory-face ((,class :foreground ,cyan)))
   `(eshell-git-prompt-exit-fail-face ((,class :foreground ,red)))
   `(eshell-git-prompt-exit-success-face ((,class :foreground ,green)))
   `(eshell-git-prompt-modified-face ((,class :foreground ,yellow)))
   `(eshell-git-prompt-powerline-clean-face ((,class :background ,green-refine-bg)))
   `(eshell-git-prompt-powerline-dir-face ((,class :background ,blue-refine-bg)))
   `(eshell-git-prompt-powerline-not-clean-face ((,class :background ,magenta-refine-bg)))
   `(eshell-git-prompt-robyrussell-branch-face ((,class :foreground ,red)))
   `(eshell-git-prompt-robyrussell-git-dirty-face ((,class :foreground ,yellow)))
   `(eshell-git-prompt-robyrussell-git-face ((,class :foreground ,blue)))
;;;;; eshell-prompt-extras (epe)
   `(epe-dir-face ((,class ,@(modus-vivendi-theme-bold-weight) :foreground ,blue)))
   `(epe-git-dir-face ((,class :foreground ,red-alt-other)))
   `(epe-git-face ((,class :foreground ,cyan-alt)))
   `(epe-pipeline-delimiter-face ((,class :foreground ,green-alt)))
   `(epe-pipeline-host-face ((,class :foreground ,blue)))
   `(epe-pipeline-time-face ((,class :foreground ,fg-special-warm)))
   `(epe-pipeline-user-face ((,class :foreground ,magenta)))
   `(epe-remote-face ((,class :foreground ,fg-alt :slant ,modus-theme-slant)))
   `(epe-status-face ((,class :foreground ,magenta-alt-other)))
   `(epe-venv-face ((,class :foreground ,fg-alt :slant ,modus-theme-slant)))
;;;;; eshell-syntax-highlighting
   `(eshell-syntax-highlighting-alias-face ((,class :foreground ,cyan)))
   `(eshell-syntax-highlighting-comment-face ((,class :foreground ,fg-alt)))
   `(eshell-syntax-highlighting-directory-face ((,class :foreground ,blue)))
   `(eshell-syntax-highlighting-envvar-face ((,class :foreground ,magenta-alt)))
   `(eshell-syntax-highlighting-invalid-face ((,class :foreground ,red)))
   `(eshell-syntax-highlighting-lisp-function-face ((,class :foreground ,magenta)))
   `(eshell-syntax-highlighting-shell-command-face ((,class :foreground ,cyan-alt-other)))
   `(eshell-syntax-highlighting-string-face ((,class :foreground ,blue-alt)))
;;;;; evil-mode
   `(evil-ex-commands ((,class :foreground ,magenta-alt-other)))
   `(evil-ex-info ((,class :foreground ,cyan-alt-other)))
   `(evil-ex-lazy-highlight ((,class :inherit modus-theme-refine-cyan)))
   `(evil-ex-search ((,class :inherit modus-theme-intense-green)))
   `(evil-ex-substitute-matches ((,class :inherit modus-theme-refine-yellow :underline t)))
   `(evil-ex-substitute-replacement ((,class :inherit (modus-theme-intense-green bold))))
;;;;; evil-goggles
   `(evil-goggles-change-face ((,class :inherit modus-theme-refine-yellow)))
   `(evil-goggles-commentary-face ((,class :inherit modus-theme-subtle-neutral :slant ,modus-theme-slant)))
   `(evil-goggles-default-face ((,class :inherit modus-theme-subtle-neutral)))
   `(evil-goggles-delete-face ((,class :inherit modus-theme-refine-red)))
   `(evil-goggles-fill-and-move-face ((,class :inherit evil-goggles-default-face)))
   `(evil-goggles-indent-face ((,class :inherit evil-goggles-default-face)))
   `(evil-goggles-join-face ((,class :inherit modus-theme-subtle-green)))
   `(evil-goggles-nerd-commenter-face ((,class :inherit evil-goggles-commentary-face)))
   `(evil-goggles-paste-face ((,class :inherit modus-theme-subtle-cyan)))
   `(evil-goggles-record-macro-face ((,class :inherit modus-theme-special-cold)))
   `(evil-goggles-replace-with-register-face ((,class :inherit modus-theme-refine-magenta)))
   `(evil-goggles-set-marker-face ((,class :inherit modus-theme-intense-magenta)))
   `(evil-goggles-shift-face ((,class :inherit evil-goggles-default-face)))
   `(evil-goggles-surround-face ((,class :inherit evil-goggles-default-face)))
   `(evil-goggles-yank-face ((,class :inherit modus-theme-subtle-blue)))
;;;;; evil-visual-mark-mode
   `(evil-visual-mark-face ((,class :inherit modus-theme-intense-magenta)))
;;;;; eww
   `(eww-invalid-certificate ((,class :foreground ,red-active)))
   `(eww-valid-certificate ((,class :foreground ,green-active)))
   `(eww-form-checkbox ((,class :box (:line-width 1 :color ,fg-inactive :style released-button) :background ,bg-inactive :foreground ,fg-main)))
   `(eww-form-file ((,class :box (:line-width 1 :color ,fg-inactive :style released-button) :background ,bg-active :foreground ,fg-main)))
   `(eww-form-select ((,class :inherit eww-form-checkbox)))
   `(eww-form-submit ((,class :inherit eww-form-file)))
   `(eww-form-text ((,class :box (:line-width 1 :color ,fg-inactive :style none) :background ,bg-active :foreground ,fg-active)))
   `(eww-form-textarea ((,class :background ,bg-alt :foreground ,fg-main)))
;;;;; eyebrowse
   `(eyebrowse-mode-line-active ((,class :inherit bold :foreground ,blue-active)))
;;;;; fancy-dabbrev
   `(fancy-dabbrev-menu-face ((,class :background ,bg-alt :foreground ,fg-alt)))
   `(fancy-dabbrev-preview-face ((,class :foreground ,fg-alt :underline t)))
   `(fancy-dabbrev-selection-face ((,class :inherit (modus-theme-intense-cyan bold))))
;;;;; flycheck
   `(flycheck-error
     ((,(append '((supports :underline (:style wave))) class)
       :underline (:color ,fg-lang-error :style wave))
      (,class :foreground ,fg-lang-error :underline t)))
   `(flycheck-error-list-checker-name ((,class :foreground ,magenta-active)))
   `(flycheck-error-list-column-number ((,class :foreground ,fg-special-cold)))
   `(flycheck-error-list-error ((,class ,@(modus-vivendi-theme-bold-weight) :foreground ,red)))
   `(flycheck-error-list-filename ((,class :foreground ,blue)))
   `(flycheck-error-list-highlight ((,class :inherit modus-theme-hl-line)))
   `(flycheck-error-list-id ((,class :foreground ,magenta-alt-other)))
   `(flycheck-error-list-id-with-explainer ((,class :inherit flycheck-error-list-id :box t)))
   `(flycheck-error-list-info ((,class :foreground ,cyan)))
   `(flycheck-error-list-line-number ((,class :foreground ,fg-special-warm)))
   `(flycheck-error-list-warning ((,class :foreground ,yellow)))
   `(flycheck-fringe-error ((,class :inherit modus-theme-fringe-red)))
   `(flycheck-fringe-info ((,class :inherit modus-theme-fringe-cyan)))
   `(flycheck-fringe-warning ((,class :inherit modus-theme-fringe-yellow)))
   `(flycheck-info
     ((,(append '((supports :underline (:style wave))) class)
       :underline (:color ,fg-lang-note :style wave))
      (,class :foreground ,fg-lang-note :underline t)))
   `(flycheck-verify-select-checker ((,class :box (:line-width 1 :color nil :style released-button))))
   `(flycheck-warning
     ((,(append '((supports :underline (:style wave))) class)
       :underline (:color ,fg-lang-warning :style wave))
      (,class :foreground ,fg-lang-warning :underline t)))
;;;;; flycheck-color-mode-line
   `(flycheck-color-mode-line-error-face ((,class :inherit flycheck-fringe-error)))
   `(flycheck-color-mode-line-info-face ((,class :inherit flycheck-fringe-info)))
   `(flycheck-color-mode-line-running-face ((,class :foreground ,fg-inactive :slant italic)))
   `(flycheck-color-mode-line-info-face ((,class :inherit flycheck-fringe-warning)))
;;;;; flycheck-indicator
   `(flycheck-indicator-disabled ((,class :foreground ,fg-inactive :slant ,modus-theme-slant)))
   `(flycheck-indicator-error ((,class ,@(modus-vivendi-theme-bold-weight) :foreground ,red-active)))
   `(flycheck-indicator-info ((,class ,@(modus-vivendi-theme-bold-weight) :foreground ,blue-active)))
   `(flycheck-indicator-running ((,class ,@(modus-vivendi-theme-bold-weight) :foreground ,magenta-active)))
   `(flycheck-indicator-success ((,class ,@(modus-vivendi-theme-bold-weight) :foreground ,green-active)))
   `(flycheck-indicator-warning ((,class ,@(modus-vivendi-theme-bold-weight) :foreground ,yellow-active)))
;;;;; flycheck-posframe
   `(flycheck-posframe-background-face ((,class :background ,bg-alt)))
   `(flycheck-posframe-border-face ((,class :foreground ,fg-alt)))
   `(flycheck-posframe-error-face ((,class :inherit bold :foreground ,red)))
   `(flycheck-posframe-face ((,class :foreground ,fg-main :slant ,modus-theme-slant)))
   `(flycheck-posframe-info-face ((,class :inherit bold :foreground ,cyan)))
   `(flycheck-posframe-warning-face ((,class :inherit bold :foreground ,yellow)))
;;;;; flymake
   `(flymake-error
     ((,(append '((supports :underline (:style wave))) class)
       :underline (:color ,fg-lang-error :style wave))
      (,class :foreground ,fg-lang-error :underline t)))
   `(flymake-note
     ((,(append '((supports :underline (:style wave))) class)
       :underline (:color ,fg-lang-note :style wave))
      (,class :foreground ,fg-lang-note :underline t)))
   `(flymake-warning
     ((,(append '((supports :underline (:style wave))) class)
       :underline (:color ,fg-lang-warning :style wave))
      (,class :foreground ,fg-lang-warning :underline t)))
;;;;; flyspell
   `(flyspell-duplicate
     ((,(append '((supports :underline (:style wave))) class)
       :underline (:color ,fg-lang-warning :style wave))
      (,class :foreground ,fg-lang-warning :underline t)))
   `(flyspell-incorrect
     ((,(append '((supports :underline (:style wave))) class)
       :underline (:color ,fg-lang-error :style wave))
      (,class :foreground ,fg-lang-error :underline t)))
;;;;; flyspell-correct
   `(flyspell-correct-highlight-face ((,class :inherit modus-theme-refine-green)))
;;;;; flx
   `(flx-highlight-face ((,class ,@(modus-vivendi-theme-extra-completions
                                    'modus-theme-subtle-magenta
                                    'modus-theme-intense-magenta
                                    'modus-theme-nuanced-magenta
                                    magenta-alt
                                    'bold))))
;;;;; freeze-it
   `(freeze-it-show ((,class :background ,bg-dim :foreground ,fg-special-warm)))
;;;;; frog-menu
   `(frog-menu-action-keybinding-face ((,class :foreground ,blue-alt-other)))
   `(frog-menu-actions-face ((,class :foreground ,magenta)))
   `(frog-menu-border ((,class :background ,bg-active)))
   `(frog-menu-candidates-face ((,class :foreground ,fg-main)))
   `(frog-menu-posframe-background-face ((,class :background ,bg-dim)))
   `(frog-menu-prompt-face ((,class :foreground ,cyan)))
;;;;; focus
   `(focus-unfocused ((,class :foreground ,fg-unfocused)))
;;;;; fold-this
   `(fold-this-overlay ((,class :inherit modus-theme-special-mild)))
;;;;; font-lock
   `(font-lock-builtin-face ((,class ,@(modus-vivendi-theme-syntax-foreground
                                        magenta-alt magenta-alt-faint)
                                     ,@(modus-vivendi-theme-bold-weight))))
   `(font-lock-comment-delimiter-face ((,class :foreground ,fg-alt :slant ,modus-theme-slant)))
   `(font-lock-comment-face ((,class :foreground ,fg-alt :slant ,modus-theme-slant)))
   `(font-lock-constant-face ((,class ,@(modus-vivendi-theme-syntax-foreground
                                         blue-alt-other blue-alt-other-faint))))
   `(font-lock-doc-face ((,class ,@(modus-vivendi-theme-syntax-foreground
                                    fg-special-cold cyan-alt-other-faint)
                                 :slant ,modus-theme-slant)))
   `(font-lock-function-name-face ((,class ,@(modus-vivendi-theme-syntax-foreground
                                              magenta magenta-faint))))
   `(font-lock-keyword-face ((,class ,@(modus-vivendi-theme-syntax-foreground
                                        magenta-alt-other magenta-alt-other-faint)
                                     ,@(modus-vivendi-theme-bold-weight))))
   `(font-lock-negation-char-face ((,class ,@(modus-vivendi-theme-syntax-foreground
                                              yellow yellow-faint)
                                           ,@(modus-vivendi-theme-bold-weight))))
   `(font-lock-preprocessor-face ((,class ,@(modus-vivendi-theme-syntax-foreground
                                             red-alt-other red-alt-other-faint))))
   `(font-lock-regexp-grouping-backslash ((,class :inherit bold :foreground ,fg-escape-char-backslash)))
   `(font-lock-regexp-grouping-construct ((,class :inherit bold :foreground ,fg-escape-char-construct)))
   `(font-lock-string-face ((,class ,@(modus-vivendi-theme-syntax-foreground
                                       blue-alt blue-alt-faint))))
   `(font-lock-type-face ((,class ,@(modus-vivendi-theme-syntax-foreground
                                     magenta-alt magenta-alt-faint))))
   `(font-lock-variable-name-face ((,class ,@(modus-vivendi-theme-syntax-foreground
                                              cyan cyan-faint))))
   `(font-lock-warning-face ((,class ,@(modus-vivendi-theme-syntax-foreground
                                        yellow-active yellow-alt-faint)
                                     ,@(modus-vivendi-theme-bold-weight))))
;;;;; forge
   `(forge-post-author ((,class :inherit bold :foreground ,fg-main)))
   `(forge-post-date ((,class :foreground ,fg-special-cold)))
   `(forge-topic-closed ((,class :foreground ,fg-alt)))
   `(forge-topic-merged ((,class :foreground ,fg-alt)))
   `(forge-topic-open ((,class :foreground ,fg-special-mild)))
   `(forge-topic-unmerged ((,class :foreground ,magenta :slant ,modus-theme-slant)))
   `(forge-topic-unread ((,class :inherit bold :foreground ,fg-main)))
;;;;; fountain-mode
   `(fountain-character ((,class :foreground ,blue-alt-other)))
   `(fountain-comment ((,class :foreground ,fg-alt :slant ,modus-theme-slant)))
   `(fountain-dialog ((,class :foreground ,blue-alt)))
   `(fountain-metadata-key ((,class :foreground ,green-alt-other)))
   `(fountain-metadata-value ((,class :foreground ,blue)))
   `(fountain-non-printing ((,class :foreground ,fg-alt)))
   `(fountain-note ((,class :foreground ,yellow :slant ,modus-theme-slant)))
   `(fountain-page-break ((,class :inherit bold :foreground ,red-alt)))
   `(fountain-page-number ((,class :inherit bold :foreground ,red-alt-other)))
   `(fountain-paren ((,class :foreground ,cyan)))
   `(fountain-scene-heading ((,class :inherit bold :foreground ,blue-nuanced)))
   `(fountain-section-heading ((,class :inherit (bold ,modus-theme-variable-pitch) :foreground ,fg-main
                                       ,@(modus-vivendi-theme-scale modus-vivendi-theme-scale-4))))
   `(fountain-section-heading-1 ((,class :inherit (bold ,modus-theme-variable-pitch) :foreground ,fg-main
                                         ,@(modus-vivendi-theme-scale modus-vivendi-theme-scale-4))))
   `(fountain-section-heading-2 ((,class :inherit (bold ,modus-theme-variable-pitch) :foreground ,fg-special-warm
                                         ,@(modus-vivendi-theme-scale modus-vivendi-theme-scale-3))))
   `(fountain-section-heading-3 ((,class :inherit (bold ,modus-theme-variable-pitch) :foreground ,fg-special-mild
                                         ,@(modus-vivendi-theme-scale modus-vivendi-theme-scale-2))))
   `(fountain-section-heading-4 ((,class :inherit (bold ,modus-theme-variable-pitch) :foreground ,fg-special-calm
                                         ,@(modus-vivendi-theme-scale modus-vivendi-theme-scale-1))))
   `(fountain-section-heading-5 ((,class :inherit (bold ,modus-theme-variable-pitch) :foreground ,fg-special-calm)))
   `(fountain-synopsis ((,class :foreground ,cyan-alt)))
   `(fountain-trans ((,class :foreground ,yellow-alt-other)))
;;;;; geiser
   `(geiser-font-lock-autodoc-current-arg ((,class ,@(modus-vivendi-theme-syntax-foreground
                                                      magenta magenta-faint))))
   `(geiser-font-lock-autodoc-identifier ((,class ,@(modus-vivendi-theme-syntax-foreground
                                                     blue blue-faint))))
   `(geiser-font-lock-doc-button ((,class ,@(modus-vivendi-theme-syntax-foreground
                                             cyan-alt cyan-alt-faint)
                                          :underline t)))
   `(geiser-font-lock-doc-link ((,class :inherit link)))
   `(geiser-font-lock-error-link ((,class ,@(modus-vivendi-theme-syntax-foreground
                                             red-alt red-alt-faint)
                                          :underline t)))
   `(geiser-font-lock-image-button ((,class ,@(modus-vivendi-theme-syntax-foreground
                                               green-alt green-alt-faint)
                                            :underline t)))
   `(geiser-font-lock-repl-input ((,class :inherit bold)))
   `(geiser-font-lock-repl-output ((,class ,@(modus-vivendi-theme-syntax-foreground
                                              magenta-alt-other magenta-alt-other-faint))))
   `(geiser-font-lock-repl-prompt ((,class ,@(modus-vivendi-theme-syntax-foreground
                                              cyan-alt-other cyan-alt-other-faint))))
   `(geiser-font-lock-xref-header ((,class :inherit bold)))
   `(geiser-font-lock-xref-link ((,class :inherit link)))
;;;;; git-commit
   `(git-commit-comment-action ((,class :foreground ,fg-alt :slant ,modus-theme-slant)))
   `(git-commit-comment-branch-local ((,class :foreground ,blue-alt :slant ,modus-theme-slant)))
   `(git-commit-comment-branch-remote ((,class :foreground ,magenta-alt :slant ,modus-theme-slant)))
   `(git-commit-comment-detached ((,class :foreground ,cyan-alt :slant ,modus-theme-slant)))
   `(git-commit-comment-file ((,class :foreground ,fg-special-cold :slant ,modus-theme-slant)))
   `(git-commit-comment-heading ((,class :inherit bold :foreground ,fg-dim :slant ,modus-theme-slant)))
   `(git-commit-keyword ((,class :foreground ,magenta)))
   `(git-commit-known-pseudo-header ((,class :foreground ,cyan-alt-other)))
   `(git-commit-nonempty-second-line ((,class :inherit modus-theme-refine-yellow)))
   `(git-commit-overlong-summary ((,class :inherit modus-theme-refine-yellow)))
   `(git-commit-pseudo-header ((,class :foreground ,blue)))
   `(git-commit-summary ((,class :inherit bold :foreground ,cyan)))
;;;;; git-gutter
   `(git-gutter:added ((,class :inherit modus-theme-fringe-green)))
   `(git-gutter:deleted ((,class :inherit modus-theme-fringe-red)))
   `(git-gutter:modified ((,class :inherit modus-theme-fringe-yellow)))
   `(git-gutter:separator ((,class :inherit modus-theme-fringe-cyan)))
   `(git-gutter:unchanged ((,class :inherit modus-theme-fringe-magenta)))
;;;;; git-gutter-fr
   `(git-gutter-fr:added ((,class :inherit modus-theme-fringe-green)))
   `(git-gutter-fr:deleted ((,class :inherit modus-theme-fringe-red)))
   `(git-gutter-fr:modified ((,class :inherit modus-theme-fringe-yellow)))
;;;;; git-{gutter,fringe}+
   `(git-gutter+-added ((,class :inherit modus-theme-fringe-green)))
   `(git-gutter+-deleted ((,class :inherit modus-theme-fringe-red)))
   `(git-gutter+-modified ((,class :inherit modus-theme-fringe-yellow)))
   `(git-gutter+-separator ((,class :inherit modus-theme-fringe-cyan)))
   `(git-gutter+-unchanged ((,class :inherit modus-theme-fringe-magenta)))
   `(git-gutter-fr+-added ((,class :inherit modus-theme-fringe-green)))
   `(git-gutter-fr+-deleted ((,class :inherit modus-theme-fringe-red)))
   `(git-gutter-fr+-modified ((,class :inherit modus-theme-fringe-yellow)))
;;;;; git-lens
   `(git-lens-added ((,class :inherit bold :foreground ,green)))
   `(git-lens-deleted ((,class :inherit bold :foreground ,red)))
   `(git-lens-header ((,class :inherit bold :height 1.1 :foreground ,cyan)))
   `(git-lens-modified ((,class :inherit bold :foreground ,yellow)))
   `(git-lens-renamed ((,class :inherit bold :foreground ,magenta)))
;;;;; git-rebase
   `(git-rebase-comment-hash ((,class :foreground ,fg-special-cold :slant ,modus-theme-slant)))
   `(git-rebase-comment-heading ((,class :inherit bold :foreground ,fg-dim :slant ,modus-theme-slant)))
   `(git-rebase-description ((,class :foreground ,fg-main)))
   `(git-rebase-hash ((,class :foreground ,cyan-alt-other)))
;;;;; git-timemachine
   `(git-timemachine-commit ((,class :inherit bold :foreground ,yellow-active)))
   `(git-timemachine-minibuffer-author-face ((,class :foreground ,fg-special-warm)))
   `(git-timemachine-minibuffer-detail-face ((,class :foreground ,red-alt)))
;;;;; git-walktree
   `(git-walktree-commit-face ((,class :foreground ,yellow)))
   `(git-walktree-symlink-face ((,class :inherit button :foreground ,cyan)))
   `(git-walktree-tree-face ((,class :foreground ,magenta)))
;;;;; gnus
   `(gnus-button ((,class :inherit button)))
   `(gnus-cite-1 ((,class :foreground ,blue-alt)))
   `(gnus-cite-10 ((,class :foreground ,magenta-alt-other)))
   `(gnus-cite-11 ((,class :foreground ,yellow-alt-other)))
   `(gnus-cite-2 ((,class :foreground ,red-alt)))
   `(gnus-cite-3 ((,class :foreground ,green-alt)))
   `(gnus-cite-4 ((,class :foreground ,magenta-alt)))
   `(gnus-cite-5 ((,class :foreground ,yellow-alt)))
   `(gnus-cite-6 ((,class :foreground ,cyan-alt)))
   `(gnus-cite-7 ((,class :foreground ,blue-alt-other)))
   `(gnus-cite-8 ((,class :foreground ,red-alt-other)))
   `(gnus-cite-9 ((,class :foreground ,green-alt-other)))
   `(gnus-cite-attribution ((,class :foreground ,fg-main :slant italic)))
   `(gnus-emphasis-highlight-words ((,class :inherit modus-theme-refine-yellow)))
   `(gnus-group-mail-1 ((,class :inherit bold :foreground ,magenta-alt)))
   `(gnus-group-mail-1-empty ((,class :foreground ,magenta-alt)))
   `(gnus-group-mail-2 ((,class :inherit bold :foreground ,magenta)))
   `(gnus-group-mail-2-empty ((,class :foreground ,magenta)))
   `(gnus-group-mail-3 ((,class :inherit bold :foreground ,magenta-alt-other)))
   `(gnus-group-mail-3-empty ((,class :foreground ,magenta-alt-other)))
   `(gnus-group-mail-low ((,class :inherit bold :foreground ,magenta-nuanced)))
   `(gnus-group-mail-low-empty ((,class :foreground ,magenta-nuanced)))
   `(gnus-group-news-1 ((,class :inherit bold :foreground ,green)))
   `(gnus-group-news-1-empty ((,class :foreground ,green)))
   `(gnus-group-news-2 ((,class :inherit bold :foreground ,cyan)))
   `(gnus-group-news-2-empty ((,class :foreground ,cyan)))
   `(gnus-group-news-3 ((,class :inherit bold :foreground ,yellow-nuanced)))
   `(gnus-group-news-3-empty ((,class :foreground ,yellow-nuanced)))
   `(gnus-group-news-4 ((,class :inherit bold :foreground ,cyan-nuanced)))
   `(gnus-group-news-4-empty ((,class :foreground ,cyan-nuanced)))
   `(gnus-group-news-5 ((,class :inherit bold :foreground ,red-nuanced)))
   `(gnus-group-news-5-empty ((,class :foreground ,red-nuanced)))
   `(gnus-group-news-6 ((,class :inherit bold :foreground ,fg-alt)))
   `(gnus-group-news-6-empty ((,class :foreground ,fg-alt)))
   `(gnus-group-news-low ((,class :inherit bold :foreground ,green-nuanced)))
   `(gnus-group-news-low-empty ((,class :foreground ,green-nuanced)))
   `(gnus-header-content ((,class :foreground ,cyan)))
   `(gnus-header-from ((,class :inherit bold :foreground ,cyan-alt-other :underline nil)))
   `(gnus-header-name ((,class :foreground ,green)))
   `(gnus-header-newsgroups ((,class :inherit bold :foreground ,blue-alt)))
   `(gnus-header-subject ((,class :inherit bold :foreground ,magenta-alt-other)))
   `(gnus-server-agent ((,class :inherit bold :foreground ,cyan)))
   `(gnus-server-closed ((,class :inherit bold :foreground ,magenta)))
   `(gnus-server-cloud ((,class :inherit bold :foreground ,cyan-alt)))
   `(gnus-server-cloud-host ((,class :inherit modus-theme-refine-cyan)))
   `(gnus-server-denied ((,class :inherit bold :foreground ,red)))
   `(gnus-server-offline ((,class :inherit bold :foreground ,yellow)))
   `(gnus-server-opened ((,class :inherit bold :foreground ,green)))
   `(gnus-signature ((,class :foreground ,fg-special-cold :slant italic)))
   `(gnus-splash ((,class :foreground ,fg-alt)))
   `(gnus-summary-cancelled ((,class :inherit modus-theme-mark-alt)))
   `(gnus-summary-high-ancient ((,class :inherit bold :foreground ,fg-alt)))
   `(gnus-summary-high-read ((,class :inherit bold :foreground ,fg-special-cold)))
   `(gnus-summary-high-ticked ((,class :inherit bold :foreground ,red-alt-other)))
   `(gnus-summary-high-undownloaded ((,class :inherit bold :foreground ,yellow)))
   `(gnus-summary-high-unread ((,class :inherit bold :foreground ,fg-main)))
   `(gnus-summary-low-ancient ((,class :foreground ,fg-alt :slant italic)))
   `(gnus-summary-low-read ((,class :foreground ,fg-alt :slant italic)))
   `(gnus-summary-low-ticked ((,class :foreground ,red-refine-fg :slant italic)))
   `(gnus-summary-low-undownloaded ((,class :foreground ,yellow-refine-fg :slant italic)))
   `(gnus-summary-low-unread ((,class :inherit bold :foreground ,fg-special-cold)))
   `(gnus-summary-normal-ancient ((,class :foreground ,fg-special-calm)))
   `(gnus-summary-normal-read ((,class :foreground ,fg-alt)))
   `(gnus-summary-normal-ticked ((,class :foreground ,red-alt-other)))
   `(gnus-summary-normal-undownloaded ((,class :foreground ,yellow)))
   `(gnus-summary-normal-unread ((,class :foreground ,fg-main)))
   `(gnus-summary-selected ((,class :inherit modus-theme-subtle-blue)))
;;;;; golden-ratio-scroll-screen
   `(golden-ratio-scroll-highlight-line-face ((,class :background ,cyan-subtle-bg :foreground ,fg-main)))
;;;;; helm
   `(helm-M-x-key ((,class :inherit bold :foreground ,magenta-alt-other)))
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
   `(helm-buffer-size ((,class :foreground ,fg-alt)))
   `(helm-candidate-number ((,class :foreground ,cyan-active)))
   `(helm-candidate-number-suspended ((,class :foreground ,yellow-active)))
   `(helm-comint-prompts-buffer-name ((,class :foreground ,green-active)))
   `(helm-comint-prompts-promptidx ((,class :foreground ,cyan-active)))
   `(helm-delete-async-message ((,class :inherit bold :foreground ,magenta-active)))
   `(helm-eob-line ((,class :background ,bg-main :foreground ,fg-main)))
   `(helm-eshell-prompts-buffer-name ((,class :foreground ,green-active)))
   `(helm-eshell-prompts-promptidx ((,class :foreground ,cyan-active)))
   `(helm-etags-file ((,class :foreground ,fg-dim :underline t)))
   `(helm-ff-backup-file ((,class :foreground ,fg-alt)))
   `(helm-ff-denied ((,class ,@(modus-vivendi-theme-extra-completions
                                'modus-theme-subtle-red
                                'modus-theme-intense-red
                                'modus-theme-nuanced-red
                                red))))
   `(helm-ff-directory ((,class :inherit helm-buffer-directory)))
   `(helm-ff-dirs ((,class :inherit bold :foreground ,blue-alt-other)))
   `(helm-ff-dotted-directory ((,class :inherit bold :background ,bg-alt :foreground ,fg-alt)))
   `(helm-ff-dotted-symlink-directory ((,class :inherit (button helm-ff-dotted-directory))))
   `(helm-ff-executable ((,class :foreground ,magenta-alt)))
   `(helm-ff-file ((,class :foreground ,fg-main)))
   `(helm-ff-file-extension ((,class :foreground ,fg-special-warm)))
   `(helm-ff-invalid-symlink ((,class :inherit button :foreground ,red)))
   `(helm-ff-pipe ((,class ,@(modus-vivendi-theme-extra-completions
                              'modus-theme-refine-magenta
                              'modus-theme-subtle-magenta
                              'modus-theme-nuanced-magenta
                              magenta))))
   `(helm-ff-prefix ((,class ,@(modus-vivendi-theme-extra-completions
                                'modus-theme-refine-yellow
                                'modus-theme-subtle-yellow
                                'modus-theme-nuanced-yellow
                                yellow-alt-other))))
   `(helm-ff-socket ((,class :foreground ,red-alt-other)))
   `(helm-ff-suid ((,class ,@(modus-vivendi-theme-extra-completions
                              'modus-theme-subtle-red
                              'modus-theme-refine-red
                              'modus-theme-nuanced-yellow
                              red-alt))))
   `(helm-ff-symlink ((,class :inherit button :foreground ,cyan)))
   `(helm-ff-truename ((,class :foreground ,blue-alt-other)))
   `(helm-grep-cmd-line ((,class :foreground ,yellow-alt-other)))
   `(helm-grep-file ((,class :inherit bold :foreground ,fg-special-cold)))
   `(helm-grep-finish ((,class :foreground ,green-active)))
   `(helm-grep-lineno ((,class :foreground ,fg-special-warm)))
   `(helm-grep-match ((,class :inherit modus-theme-special-calm)))
   `(helm-header ((,class :inherit bold :foreground ,fg-special-cold)))
   `(helm-header-line-left-margin ((,class :inherit bold :foreground ,yellow-intense)))
   `(helm-history-deleted ((,class ,@(modus-vivendi-theme-extra-completions
                                      'modus-theme-subtle-red
                                      'modus-theme-intense-red
                                      'modus-theme-nuanced-red
                                      red
                                      'bold))))
   `(helm-history-remote ((,class :foreground ,red-alt-other)))
   `(helm-lisp-completion-info ((,class :foreground ,fg-special-warm)))
   `(helm-lisp-show-completion ((,class ,@(modus-vivendi-theme-extra-completions
                                           'modus-theme-subtle-yellow
                                           'modus-theme-refine-yellow
                                           'modus-theme-nuanced-yellow
                                           yellow
                                           'bold))))
   `(helm-locate-finish ((,class :foreground ,green-active)))
   `(helm-match ((,class ,@(modus-vivendi-theme-extra-completions
                            'modus-theme-subtle-cyan
                            'modus-theme-refine-cyan
                            'modus-theme-nuanced-cyan
                            cyan
                            'bold))))
   `(helm-match-item ((,class ,@(modus-vivendi-theme-extra-completions
                                 'modus-theme-subtle-neutral
                                 'modus-theme-subtle-cyan
                                 'modus-theme-nuanced-cyan
                                 cyan-alt-other))))
   `(helm-minibuffer-prompt ((,class :inherit minibuffer-prompt)))
   `(helm-moccur-buffer ((,class :inherit button :foreground ,cyan-alt-other)))
   `(helm-mode-prefix ((,class ,@(modus-vivendi-theme-extra-completions
                                  'modus-theme-subtle-magenta
                                  'modus-theme-intense-magenta
                                  'modus-theme-nuanced-magenta
                                  magenta-alt
                                  'bold))))
   `(helm-non-file-buffer ((,class :foreground ,fg-alt)))
   `(helm-prefarg ((,class :foreground ,red-active)))
   `(helm-resume-need-update ((,class ,@(modus-vivendi-theme-extra-completions
                                         'modus-theme-subtle-magenta
                                         'modus-theme-refine-magenta
                                         'modus-theme-nuanced-magenta
                                         magenta-alt-other))))
   `(helm-selection ((,class ,@(modus-vivendi-theme-extra-completions
                                'modus-theme-subtle-blue
                                'modus-theme-refine-blue
                                'modus-theme-special-cold
                                nil
                                'bold))))
   `(helm-selection-line ((,class :inherit modus-theme-special-cold)))
   `(helm-separator ((,class :foreground ,fg-special-mild)))
   `(helm-time-zone-current ((,class :foreground ,green)))
   `(helm-time-zone-home ((,class :foreground ,magenta)))
   `(helm-source-header ((,class :inherit bold :foreground ,red-alt
                                 ,@(modus-vivendi-theme-scale modus-vivendi-theme-scale-4))))
   `(helm-top-columns ((,class :inherit helm-header)))
   `(helm-ucs-char ((,class :foreground ,yellow-alt-other)))
   `(helm-visible-mark ((,class :inherit modus-theme-subtle-cyan)))
;;;;; helm-ls-git
   `(helm-ls-git-added-copied-face ((,class :foreground ,green-intense)))
   `(helm-ls-git-added-modified-face ((,class :foreground ,yellow-intense)))
   `(helm-ls-git-conflict-face ((,class :inherit bold :foreground ,red-intense)))
   `(helm-ls-git-deleted-and-staged-face ((,class :foreground ,red-nuanced)))
   `(helm-ls-git-deleted-not-staged-face ((,class :foreground ,red)))
   `(helm-ls-git-modified-and-staged-face ((,class :foreground ,yellow-nuanced)))
   `(helm-ls-git-modified-not-staged-face ((,class :foreground ,yellow)))
   `(helm-ls-git-renamed-modified-face ((,class :foreground ,magenta)))
   `(helm-ls-git-untracked-face ((,class :foreground ,fg-special-cold)))
;;;;; helm-switch-shell
   `(helm-switch-shell-new-shell-face ((,class ,@(modus-vivendi-theme-extra-completions
                                                  'modus-theme-subtle-magenta
                                                  'modus-theme-refine-magenta
                                                  'modus-theme-nuanced-magenta
                                                  magenta-alt-other
                                                  'bold))))
;;;;; helm-xref
   `(helm-xref-file-name ((,class :inherit bold :foreground ,fg-special-cold)))
   `(helm-xref-file-name ((,class :foreground ,fg-special-warm)))
;;;;; helpful
   `(helpful-heading ((,class :inherit modus-theme-heading-1)))
;;;;; highlight region or ad-hoc regexp
   `(hi-black-b ((,class :background ,fg-main :foreground ,bg-main)))
   `(hi-blue ((,class :background ,bg-alt :foreground ,blue :underline t)))
   `(hi-blue-b ((,class :inherit modus-theme-intense-blue)))
   `(hi-green ((,class :background ,bg-alt :foreground ,green :underline t)))
   `(hi-green-b ((,class :inherit modus-theme-intense-green)))
   `(hi-pink ((,class :background ,bg-alt :foreground ,magenta :underline t)))
   `(hi-red-b ((,class :inherit modus-theme-intense-red)))
   `(hi-yellow ((,class :background ,bg-alt :foreground ,yellow :underline t)))
   `(highlight ((,class :inherit modus-theme-subtle-blue)))
   `(highlight-changes ((,class :foreground ,yellow-alt-other)))
   `(highlight-changes-delete ((,class :foreground ,red-alt-other :underline t)))
   `(hl-line ((,class :inherit modus-theme-hl-line)))
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
   `(hes-escape-backslash-face ((,class :inherit bold :foreground ,fg-escape-char-construct)))
   `(hes-escape-sequence-face ((,class :inherit bold :foreground ,fg-escape-char-backslash)))
;;;;; highlight-indentation
   `(highlight-indentation-face ((,class :inherit modus-theme-hl-line)))
   `(highlight-indentation-current-column-face ((,class :background ,bg-active)))
;;;;; highlight-numbers
   `(highlight-numbers-number ((,class :foreground ,blue-alt-other)))
;;;;; highlight-symbol
   `(highlight-symbol-face ((,class :inherit modus-theme-special-mild)))
;;;;; highlight-thing
   `(highlight-thing ((,class :background ,bg-alt :foreground ,cyan)))
;;;;; hl-defined
   `(hdefd-functions ((,class :foreground ,blue)))
   `(hdefd-undefined ((,class :foreground ,red-alt)))
   `(hdefd-variables ((,class :foreground ,cyan-alt)))
;;;;; hl-fill-column
   `(hl-fill-column-face ((,class :background ,bg-active :foreground ,fg-active)))
;;;;; hl-todo
   `(hl-todo ((,class :inherit bold :foreground ,red-alt-other :slant ,modus-theme-slant)))
;;;;; hydra
   `(hydra-face-amaranth ((,class :inherit bold :foreground ,yellow)))
   `(hydra-face-blue ((,class :inherit bold :foreground ,blue-alt)))
   `(hydra-face-pink ((,class :inherit bold :foreground ,magenta-alt)))
   `(hydra-face-red ((,class :inherit bold :foreground ,red)))
   `(hydra-face-teal ((,class :inherit bold :foreground ,cyan)))
;;;;; hyperlist
   `(hyperlist-condition ((,class :foreground ,green)))
   `(hyperlist-hashtag ((,class :foreground ,yellow)))
   `(hyperlist-operator ((,class :foreground ,blue-alt)))
   `(hyperlist-paren ((,class :foreground ,cyan-alt-other)))
   `(hyperlist-quote ((,class :foreground ,cyan-alt)))
   `(hyperlist-ref ((,class :foreground ,magenta-alt-other)))
   `(hyperlist-stars ((,class :foreground ,fg-alt)))
   `(hyperlist-tag ((,class :foreground ,red)))
   `(hyperlist-toplevel ((,class :inherit bold :foreground ,fg-main)))
;;;;; icomplete
   `(icomplete-first-match ((,class :inherit bold
                                    ,@(modus-vivendi-theme-standard-completions
                                       magenta bg-alt
                                       bg-active fg-main))))
;;;;; icomplete-vertical
   `(icomplete-vertical-separator ((,class :foreground ,fg-alt)))
;;;;; ido-mode
   `(ido-first-match ((,class :inherit bold
                              ,@(modus-vivendi-theme-standard-completions
                                 magenta bg-alt
                                 bg-active fg-main))))
   `(ido-incomplete-regexp ((,class :inherit error)))
   `(ido-indicator ((,class :inherit modus-theme-subtle-yellow)))
   `(ido-only-match ((,class :inherit bold
                             ,@(modus-vivendi-theme-standard-completions
                                green green-nuanced-bg
                                green-intense-bg fg-main))))
   `(ido-subdir ((,class :foreground ,blue)))
   `(ido-virtual ((,class :foreground ,fg-special-warm)))
;;;;; iedit
   `(iedit-occurrence ((,class :inherit modus-theme-refine-blue)))
   `(iedit-read-only-occurrence ((,class :inherit modus-theme-intense-yellow)))
;;;;; iflipb
   `(iflipb-current-buffer-face ((,class :inherit bold :foreground ,cyan-alt)))
   `(iflipb-other-buffer-face ((,class :foreground ,fg-alt)))
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
   `(indium-keyword-face ((,class :foreground ,magenta-alt-other)))
   `(indium-litable-face ((,class :foreground ,fg-special-warm :slant ,modus-theme-slant)))
   `(indium-repl-error-face ((,class :inherit bold :foreground ,red)))
   `(indium-repl-prompt-face ((,class :foreground ,cyan-alt-other)))
   `(indium-repl-stdout-face ((,class :foreground ,fg-main)))
;;;;; info
   `(Info-quoted ((,class ,@(modus-vivendi-theme-mixed-fonts)
                          :foreground ,magenta))) ; the capitalisation is canonical
   `(info-header-node ((,class :inherit bold :foreground ,fg-alt)))
   `(info-header-xref ((,class :foreground ,blue-active)))
   `(info-index-match ((,class :inherit match)))
   `(info-menu-header ((,class :inherit modus-theme-heading-3)))
   `(info-menu-star ((,class :foreground ,red)))
   `(info-node ((,class :inherit bold)))
   `(info-title-1 ((,class :inherit modus-theme-heading-1)))
   `(info-title-2 ((,class :inherit modus-theme-heading-2)))
   `(info-title-3 ((,class :inherit modus-theme-heading-3)))
   `(info-title-4 ((,class :inherit modus-theme-heading-4)))
;;;;; info-colors
   `(info-colors-lisp-code-block ((,class :inherit fixed-pitch)))
   `(info-colors-ref-item-command ((,class :foreground ,magenta)))
   `(info-colors-ref-item-constant ((,class :foreground ,blue-alt-other)))
   `(info-colors-ref-item-function ((,class :foreground ,magenta)))
   `(info-colors-ref-item-macro ((,class ,@(modus-vivendi-theme-bold-weight) :foreground ,magenta-alt-other)))
   `(info-colors-ref-item-other ((,class :foreground ,cyan)))
   `(info-colors-ref-item-special-form ((,class ,@(modus-vivendi-theme-bold-weight) :foreground ,magenta-alt-other)))
   `(info-colors-ref-item-syntax-class ((,class :foreground ,magenta)))
   `(info-colors-ref-item-type ((,class :foreground ,magenta-alt)))
   `(info-colors-ref-item-user-option ((,class :foreground ,cyan)))
   `(info-colors-ref-item-variable ((,class :foreground ,cyan)))
;;;;; interaction-log
   `(ilog-buffer-face ((,class :foreground ,magenta-alt-other)))
   `(ilog-change-face ((,class :foreground ,magenta-alt)))
   `(ilog-echo-face ((,class :foreground ,yellow-alt-other)))
   `(ilog-load-face ((,class :foreground ,green)))
   `(ilog-message-face ((,class :foreground ,fg-alt)))
   `(ilog-non-change-face ((,class :foreground ,blue)))
;;;;; ioccur
   `(ioccur-cursor ((,class :foreground ,fg-main)))
   `(ioccur-invalid-regexp ((,class :foreground ,red)))
   `(ioccur-match-face ((,class :inherit modus-theme-special-calm)))
   `(ioccur-match-overlay-face ((,class ,@(and (>= emacs-major-version 27) '(:extend t))
                                        :inherit modus-theme-special-cold)))
   `(ioccur-num-line-face ((,class :foreground ,fg-special-warm)))
   `(ioccur-overlay-face ((,class ,@(and (>= emacs-major-version 27) '(:extend t))
                                  :inherit modus-theme-refine-blue)))
   `(ioccur-regexp-face ((,class :inherit (modus-theme-intense-magenta bold))))
   `(ioccur-title-face ((,class :inherit bold :foreground ,red-alt
                                ,@(modus-vivendi-theme-scale modus-vivendi-theme-scale-4))))
;;;;; isearch, occur, and the like
   `(isearch ((,class :inherit (modus-theme-intense-green bold))))
   `(isearch-fail ((,class :inherit modus-theme-refine-red)))
   `(lazy-highlight ((,class :inherit modus-theme-refine-cyan)))
   `(match ((,class :inherit modus-theme-special-calm)))
   `(query-replace ((,class :inherit (modus-theme-intense-yellow bold))))
;;;;; ivy
   `(ivy-action ((,class :inherit bold :foreground ,red-alt)))
   `(ivy-completions-annotations ((,class :foreground ,fg-special-cold :slant ,modus-theme-slant)))
   `(ivy-confirm-face ((,class :foreground ,cyan)))
   `(ivy-current-match ((,class ,@(modus-vivendi-theme-extra-completions
                                   'modus-theme-refine-cyan
                                   'modus-theme-intense-cyan
                                   'modus-theme-special-warm
                                   nil
                                   'bold))))
   `(ivy-cursor ((,class :background ,fg-main :foreground ,bg-main)))
   `(ivy-grep-info ((,class :foreground ,cyan-alt)))
   `(ivy-grep-line-number ((,class :foreground ,fg-special-warm)))
   `(ivy-highlight-face ((,class :foreground ,magenta)))
   `(ivy-match-required-face ((,class :inherit error)))
   `(ivy-minibuffer-match-face-1 ((,class ,@(modus-vivendi-theme-extra-completions
                                             'modus-theme-subtle-neutral
                                             'modus-theme-intense-neutral
                                             'modus-theme-subtle-neutral
                                             fg-alt))))
   `(ivy-minibuffer-match-face-2 ((,class ,@(modus-vivendi-theme-extra-completions
                                             'modus-theme-subtle-green
                                             'modus-theme-refine-green
                                             'modus-theme-nuanced-green
                                             green-alt-other
                                             'bold))))
   `(ivy-minibuffer-match-face-3 ((,class ,@(modus-vivendi-theme-extra-completions
                                             'modus-theme-subtle-cyan
                                             'modus-theme-refine-cyan
                                             'modus-theme-nuanced-cyan
                                             cyan-alt-other
                                             'bold))))
   `(ivy-minibuffer-match-face-4 ((,class ,@(modus-vivendi-theme-extra-completions
                                             'modus-theme-subtle-magenta
                                             'modus-theme-refine-magenta
                                             'modus-theme-nuanced-magenta
                                             magenta-alt-other
                                             'bold))))
   `(ivy-minibuffer-match-highlight ((,class ,@(modus-vivendi-theme-extra-completions
                                                'modus-theme-subtle-blue
                                                'modus-theme-intense-blue
                                                'modus-theme-nuanced-blue
                                                blue-alt-other
                                                'bold))))
   `(ivy-modified-buffer ((,class :foreground ,yellow :slant ,modus-theme-slant)))
   `(ivy-modified-outside-buffer ((,class :foreground ,yellow-alt :slant ,modus-theme-slant)))
   `(ivy-org ((,class :foreground ,cyan-alt-other)))
   `(ivy-prompt-match ((,class :inherit ivy-current-match)))
   `(ivy-remote ((,class :foreground ,magenta)))
   `(ivy-separator ((,class :foreground ,fg-alt)))
   `(ivy-subdir ((,class :foreground ,blue-alt-other)))
   `(ivy-virtual ((,class :foreground ,magenta-alt-other)))
   `(ivy-yanked-word ((,class ,@(modus-vivendi-theme-extra-completions
                                 'modus-theme-subtle-blue
                                 'modus-theme-refine-blue
                                 'modus-theme-nuanced-blue
                                 blue-alt))))
;;;;; ivy-posframe
   `(ivy-posframe ((,class :background ,bg-dim :foreground ,fg-main)))
   `(ivy-posframe-border ((,class :background ,bg-active)))
   `(ivy-posframe-cursor ((,class :background ,fg-main :foreground ,bg-main)))
;;;;; jira (org-jira)
   `(jiralib-comment-face ((,class :background ,bg-alt)))
   `(jiralib-comment-header-face ((,class :inherit bold)))
   `(jiralib-issue-info-face ((,class :inherit modus-theme-special-warm)))
   `(jiralib-issue-info-header-face ((,class :inherit (modus-theme-special-warm bold))))
   `(jiralib-issue-summary-face ((,class :inherit bold)))
   `(jiralib-link-filter-face ((,class :underline t)))
   `(jiralib-link-issue-face ((,class :underline t)))
   `(jiralib-link-project-face ((,class :underline t)))
;;;;; journalctl-mode
   `(journalctl-error-face ((,class :inherit bold :foreground ,red)))
   `(journalctl-finished-face ((,class :inherit bold :foreground ,green)))
   `(journalctl-host-face ((,class :foreground ,blue)))
   `(journalctl-process-face ((,class :foreground ,cyan-alt-other)))
   `(journalctl-starting-face ((,class :foreground ,green)))
   `(journalctl-timestamp-face ((,class :foreground ,fg-special-cold)))
   `(journalctl-warning-face ((,class :inherit bold :foreground ,yellow)))
;;;;; js2-mode
   `(js2-error ((,class :foreground ,red)))
   `(js2-external-variable ((,class :foreground ,cyan-alt-other)))
   `(js2-function-call ((,class :foreground ,magenta)))
   `(js2-function-param ((,class :foreground ,blue)))
   `(js2-instance-member ((,class :foreground ,magenta-alt-other)))
   `(js2-jsdoc-html-tag-delimiter ((,class :foreground ,fg-main)))
   `(js2-jsdoc-html-tag-name ((,class :foreground ,cyan)))
   `(js2-jsdoc-tag ((,class :foreground ,fg-special-calm)))
   `(js2-jsdoc-type ((,class :foreground ,fg-special-cold)))
   `(js2-jsdoc-value ((,class :foreground ,fg-special-warm)))
   `(js2-object-property ((,class :foreground ,fg-main)))
   `(js2-object-property-access ((,class :foreground ,fg-main)))
   `(js2-private-function-call ((,class :foreground ,green-alt-other)))
   `(js2-private-member ((,class :foreground ,fg-special-mild)))
   `(js2-warning ((,class :foreground ,yellow-alt :underline t)))
;;;;; julia
   `(julia-macro-face ((,class ,@(modus-vivendi-theme-bold-weight) :foreground ,magenta)))
   `(julia-quoted-symbol-face ((,class :foreground ,blue-alt-other)))
;;;;; jupyter
   `(jupyter-eval-overlay ((,class :inherit bold :foreground ,blue)))
   `(jupyter-repl-input-prompt ((,class :foreground ,cyan-alt-other)))
   `(jupyter-repl-output-prompt ((,class :foreground ,magenta-alt-other)))
   `(jupyter-repl-traceback ((,class :inherit modus-theme-intense-red)))
;;;;; kaocha-runner
   `(kaocha-runner-error-face ((,class :foreground ,red)))
   `(kaocha-runner-success-face ((,class :foreground ,green)))
   `(kaocha-runner-warning-face ((,class :foreground ,yellow)))
;;;;; keycast
   `(keycast-command ((,class :inherit bold :foreground ,blue-active)))
   `(keycast-key ((,class ,@(modus-vivendi-theme-mode-line-attrs
                             bg-main blue-active
                             bg-main blue-active
                             blue-active blue-intense
                             'alt-style -3))))
;;;;; line numbers (display-line-numbers-mode and global variant)
   `(line-number ((,class :inherit default :background ,bg-dim :foreground ,fg-alt)))
   `(line-number-current-line ((,class :inherit default :background ,bg-active :foreground ,fg-main)))
;;;;; lsp-mode
   `(lsp-face-highlight-read ((,class :inherit modus-theme-subtle-blue :underline t)))
   `(lsp-face-highlight-textual ((,class :inherit modus-theme-subtle-blue)))
   `(lsp-face-highlight-write ((,class :inherit (modus-theme-refine-blue bold))))
   `(lsp-face-semhl-constant ((,class :foreground ,blue-alt-other)))
   `(lsp-face-semhl-deprecated
     ((,(append '((supports :underline (:style wave))) class)
       :foreground ,yellow :underline (:style wave))
      (,class :foreground ,yellow :underline t)))
   `(lsp-face-semhl-enummember ((,class :foreground ,blue-alt-other)))
   `(lsp-face-semhl-field ((,class :foreground ,cyan-alt)))
   `(lsp-face-semhl-field-static ((,class :foreground ,cyan-alt :slant ,modus-theme-slant)))
   `(lsp-face-semhl-function ((,class :foreground ,magenta)))
   `(lsp-face-semhl-method ((,class :foreground ,magenta)))
   `(lsp-face-semhl-namespace ((,class ,@(modus-vivendi-theme-bold-weight) :foreground ,magenta-alt)))
   `(lsp-face-semhl-preprocessor ((,class :foreground ,red-alt-other)))
   `(lsp-face-semhl-static-method ((,class :foreground ,magenta :slant ,modus-theme-slant)))
   `(lsp-face-semhl-type-class ((,class :foreground ,magenta-alt)))
   `(lsp-face-semhl-type-enum ((,class :foreground ,magenta-alt)))
   `(lsp-face-semhl-type-primitive ((,class :foreground ,magenta-alt :slant ,modus-theme-slant)))
   `(lsp-face-semhl-type-template ((,class :foreground ,magenta-alt :slant ,modus-theme-slant)))
   `(lsp-face-semhl-type-typedef ((,class :foreground ,magenta-alt :slant ,modus-theme-slant)))
   `(lsp-face-semhl-variable ((,class :foreground ,cyan)))
   `(lsp-face-semhl-variable-local ((,class :foreground ,cyan)))
   `(lsp-face-semhl-variable-parameter ((,class :foreground ,cyan-alt-other)))
   `(lsp-lens-face ((,class :height 0.8 :foreground ,fg-alt)))
   `(lsp-lens-mouse-face ((,class :height 0.8 :foreground ,blue-alt-other :underline t)))
   `(lsp-ui-doc-background ((,class :background ,bg-alt)))
   `(lsp-ui-doc-header ((,class :background ,bg-header :foreground ,fg-header)))
   `(lsp-ui-doc-url ((,class :inherit button :foreground ,blue-alt-other)))
   `(lsp-ui-peek-filename ((,class :foreground ,fg-special-warm)))
   `(lsp-ui-peek-footer ((,class :background ,bg-header :foreground ,fg-header)))
   `(lsp-ui-peek-header ((,class :background ,bg-header :foreground ,fg-header)))
   `(lsp-ui-peek-highlight ((,class :inherit modus-theme-subtle-blue)))
   `(lsp-ui-peek-line-number ((,class :foreground ,fg-alt)))
   `(lsp-ui-peek-list ((,class :background ,bg-dim)))
   `(lsp-ui-peek-peek ((,class :background ,bg-alt)))
   `(lsp-ui-peek-selection ((,class :inherit modus-theme-subtle-cyan)))
   `(lsp-ui-sideline-code-action ((,class :foreground ,yellow)))
   `(lsp-ui-sideline-current-symbol ((,class :inherit bold :height 0.99 :box (:line-width -1 :style nil) :foreground ,fg-main)))
   `(lsp-ui-sideline-symbol ((,class :inherit bold :height 0.99 :box (:line-width -1 :style nil) :foreground ,fg-alt)))
   `(lsp-ui-sideline-symbol-info ((,class :height 0.99 :slant italic)))
;;;;; magit
   `(magit-bisect-bad ((,class :foreground ,red-alt-other)))
   `(magit-bisect-good ((,class :foreground ,green-alt-other)))
   `(magit-bisect-skip ((,class :foreground ,yellow-alt-other)))
   `(magit-blame-date ((,class :foreground ,blue)))
   `(magit-blame-dimmed ((,class :foreground ,fg-alt)))
   `(magit-blame-hash ((,class :foreground ,fg-special-warm)))
   `(magit-blame-heading ((,class :background ,bg-alt)))
   `(magit-blame-highlight ((,class :inherit modus-theme-nuanced-cyan)))
   `(magit-blame-margin ((,class :inherit magit-blame-highlight)))
   `(magit-blame-name ((,class :foreground ,magenta-alt-other)))
   `(magit-blame-summary ((,class :foreground ,cyan-alt-other)))
   `(magit-branch-current ((,class :foreground ,blue-alt-other :box t)))
   `(magit-branch-local ((,class :foreground ,blue-alt)))
   `(magit-branch-remote ((,class :foreground ,magenta-alt)))
   `(magit-branch-remote-head ((,class :foreground ,magenta-alt-other :box t)))
   `(magit-branch-upstream ((,class :slant italic)))
   `(magit-cherry-equivalent ((,class :background ,bg-main :foreground ,magenta-intense)))
   `(magit-cherry-unmatched ((,class :background ,bg-main :foreground ,cyan-intense)))
   ;; NOTE: here we break from the pattern of inheriting from the
   ;; modus-theme-diff-* faces, though only for the standard actions,
   ;; not the highlighted ones.  This is because Magit's interaction
   ;; model relies on highlighting the current diff hunk.
   `(magit-diff-added ((,class ,@(modus-vivendi-theme-diff
                                  bg-main green
                                  bg-diff-added fg-diff-added
                                  green-nuanced-bg fg-diff-added))))
   `(magit-diff-added-highlight ((,class :inherit modus-theme-diff-focus-added)))
   `(magit-diff-base ((,class ,@(modus-vivendi-theme-diff
                                 bg-main yellow
                                 bg-diff-changed fg-diff-changed
                                 yellow-nuanced-bg fg-diff-changed))))
   `(magit-diff-base-highlight ((,class :inherit modus-theme-diff-focus-changed)))
   `(magit-diff-context ((,class :foreground ,fg-unfocused)))
   `(magit-diff-context-highlight ((,class ,@(modus-vivendi-theme-diff
                                              bg-dim fg-dim
                                              bg-inactive fg-inactive
                                              bg-dim fg-alt))))
   `(magit-diff-file-heading ((,class :inherit bold :foreground ,fg-special-cold)))
   `(magit-diff-file-heading-highlight ((,class :inherit (modus-theme-special-cold bold))))
   `(magit-diff-file-heading-selection ((,class :inherit modus-theme-refine-cyan)))
   ;; NOTE: here we break from the pattern of inheriting from the
   ;; modus-theme-diff-* faces.
   `(magit-diff-hunk-heading ((,class :inherit bold :background ,bg-active
                                      :foreground ,fg-inactive)))
   `(magit-diff-hunk-heading-highlight ((,class :inherit bold :background ,bg-diff-heading
                                                :foreground ,fg-diff-heading)))
   `(magit-diff-hunk-heading-selection ((,class :inherit modus-theme-refine-blue)))
   `(magit-diff-hunk-region ((,class :inherit bold)))
   `(magit-diff-lines-boundary ((,class :background ,fg-main)))
   `(magit-diff-lines-heading ((,class :inherit modus-theme-refine-magenta)))
   `(magit-diff-removed ((,class ,@(modus-vivendi-theme-diff
                                    bg-main red
                                    bg-diff-removed fg-diff-removed
                                    red-nuanced-bg fg-diff-removed))))
   `(magit-diff-removed-highlight ((,class :inherit modus-theme-diff-focus-removed)))
   `(magit-diffstat-added ((,class :foreground ,green)))
   `(magit-diffstat-removed ((,class :foreground ,red)))
   `(magit-dimmed ((,class :foreground ,fg-unfocused)))
   `(magit-filename ((,class :foreground ,fg-special-cold)))
   `(magit-hash ((,class :foreground ,fg-alt)))
   `(magit-head ((,class :inherit magit-branch-local)))
   `(magit-header-line ((,class :inherit bold :foreground ,magenta-active)))
   `(magit-header-line-key ((,class :inherit bold :foreground ,red-active)))
   `(magit-header-line-log-select ((,class :inherit bold :foreground ,fg-main)))
   `(magit-keyword ((,class :foreground ,magenta)))
   `(magit-keyword-squash ((,class :inherit bold :foreground ,yellow-alt-other)))
   `(magit-log-author ((,class :foreground ,cyan)))
   `(magit-log-date ((,class :foreground ,fg-alt)))
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
   `(magit-refname ((,class :foreground ,fg-alt)))
   `(magit-refname-pullreq ((,class :foreground ,fg-alt)))
   `(magit-refname-stash ((,class :foreground ,fg-alt)))
   `(magit-refname-wip ((,class :foreground ,fg-alt)))
   `(magit-section ((,class :background ,bg-dim :foreground ,fg-main)))
   `(magit-section-heading ((,class :inherit bold :foreground ,cyan)))
   `(magit-section-heading-selection ((,class :inherit (modus-theme-refine-cyan bold))))
   `(magit-section-highlight ((,class :background ,bg-alt)))
   `(magit-sequence-done ((,class :foreground ,green-alt)))
   `(magit-sequence-drop ((,class :foreground ,red-alt)))
   `(magit-sequence-exec ((,class :foreground ,magenta-alt)))
   `(magit-sequence-head ((,class :foreground ,cyan-alt)))
   `(magit-sequence-onto ((,class :foreground ,fg-alt)))
   `(magit-sequence-part ((,class :foreground ,yellow-alt)))
   `(magit-sequence-pick ((,class :foreground ,blue-alt)))
   `(magit-sequence-stop ((,class :foreground ,red)))
   `(magit-signature-bad ((,class :inherit bold :foreground ,red)))
   `(magit-signature-error ((,class :foreground ,red-alt)))
   `(magit-signature-expired ((,class :foreground ,yellow)))
   `(magit-signature-expired-key ((,class :foreground ,yellow)))
   `(magit-signature-good ((,class :foreground ,green)))
   `(magit-signature-revoked ((,class :foreground ,magenta)))
   `(magit-signature-untrusted ((,class :foreground ,cyan)))
   `(magit-tag ((,class :foreground ,yellow-alt-other)))
;;;;; magit-imerge
   `(magit-imerge-overriding-value ((,class :inherit bold :foreground ,red-alt)))
;;;;; man
   `(Man-overstrike ((,class :inherit bold :foreground ,magenta)))
   `(Man-reverse ((,class :inherit modus-theme-subtle-magenta)))
   `(Man-underline ((,class :foreground ,cyan :underline t)))
;;;;; markdown-mode
   `(markdown-blockquote-face ((,class :foreground ,fg-special-cold :slant ,modus-theme-slant)))
   `(markdown-bold-face ((,class :inherit bold)))
   `(markdown-code-face ((,class ,@(modus-vivendi-theme-mixed-fonts))))
   `(markdown-comment-face ((,class :foreground ,fg-alt :slant ,modus-theme-slant)))
   `(markdown-footnote-marker-face ((,class :inherit bold :foreground ,cyan-alt)))
   `(markdown-footnote-text-face ((,class :foreground ,fg-main :slant ,modus-theme-slant)))
   `(markdown-gfm-checkbox-face ((,class :foreground ,cyan-alt-other)))
   `(markdown-header-delimiter-face ((,class ,@(modus-vivendi-theme-bold-weight) :foreground ,fg-dim)))
   `(markdown-header-face ((t nil)))
   `(markdown-header-face-1 ((,class :inherit modus-theme-heading-1)))
   `(markdown-header-face-2 ((,class :inherit modus-theme-heading-2)))
   `(markdown-header-face-3 ((,class :inherit modus-theme-heading-3)))
   `(markdown-header-face-4 ((,class :inherit modus-theme-heading-4)))
   `(markdown-header-face-5 ((,class :inherit modus-theme-heading-5)))
   `(markdown-header-face-6 ((,class :inherit modus-theme-heading-6)))
   `(markdown-header-rule-face ((,class :inherit bold :foreground ,fg-special-warm)))
   `(markdown-hr-face ((,class :inherit bold :foreground ,fg-special-warm)))
   `(markdown-html-attr-name-face ((,class ,@(modus-vivendi-theme-mixed-fonts)
                                           :foreground ,cyan)))
   `(markdown-html-attr-value-face ((,class ,@(modus-vivendi-theme-mixed-fonts)
                                            :foreground ,blue)))
   `(markdown-html-entity-face ((,class ,@(modus-vivendi-theme-mixed-fonts)
                                        :foreground ,cyan)))
   `(markdown-html-tag-delimiter-face ((,class ,@(modus-vivendi-theme-mixed-fonts)
                                               :foreground ,fg-special-mild)))
   `(markdown-html-tag-name-face ((,class ,@(modus-vivendi-theme-mixed-fonts)
                                          :foreground ,magenta-alt)))
   `(markdown-inline-code-face ((,class ,@(modus-vivendi-theme-mixed-fonts)
                                        :foreground ,magenta)))
   `(markdown-italic-face ((,class :foreground ,fg-special-cold :slant italic)))
   `(markdown-language-info-face ((,class ,@(modus-vivendi-theme-mixed-fonts)
                                          :foreground ,fg-special-cold)))
   `(markdown-language-keyword-face ((,class ,@(modus-vivendi-theme-mixed-fonts)
                                             :foreground ,green-alt-other)))
   `(markdown-line-break-face ((,class :inherit modus-theme-refine-cyan :underline t)))
   `(markdown-link-face ((,class :inherit link)))
   `(markdown-link-title-face ((,class :foreground ,fg-special-cold :slant ,modus-theme-slant)))
   `(markdown-list-face ((,class :foreground ,fg-dim)))
   `(markdown-markup-face ((,class :foreground ,fg-alt)))
   `(markdown-math-face ((,class :foreground ,magenta-alt-other)))
   `(markdown-metadata-key-face ((,class :foreground ,cyan-alt-other)))
   `(markdown-metadata-value-face ((,class :foreground ,blue-alt)))
   `(markdown-missing-link-face ((,class :inherit bold :foreground ,yellow)))
   `(markdown-plain-url-face ((,class :inherit markdown-link-face)))
   `(markdown-pre-face ((,class ,@(and (>= emacs-major-version 27) '(:extend t))
                                ,@(modus-vivendi-theme-mixed-fonts)
                                :background ,bg-dim
                                :foreground ,fg-special-mild)))
   `(markdown-reference-face ((,class :inherit markdown-markup-face)))
   `(markdown-strike-through-face ((,class :strike-through t)))
   `(markdown-table-face ((,class ,@(modus-vivendi-theme-mixed-fonts)
                                  :foreground ,fg-special-cold)))
   `(markdown-url-face ((,class :foreground ,blue-alt)))
;;;;; markup-faces (`adoc-mode')
   `(markup-anchor-face ((,class :foreground ,fg-inactive)))
   `(markup-attribute-face ((,class :foreground ,fg-inactive :slant italic)))
   `(markup-big-face ((,class :height 1.3 :foreground ,blue-nuanced)))
   `(markup-bold-face ((,class :inherit bold :foreground ,red-nuanced)))
   `(markup-code-face ((,class :inherit fixed-pitch :foreground ,magenta)))
   `(markup-command-face ((,class :foreground ,fg-inactive)))
   `(markup-comment-face ((,class :foreground ,fg-alt :slant ,modus-theme-slant)))
   `(markup-complex-replacement-face ((,class :box (:line-width 2 :color nil :style released-button)
                                              :inherit modus-theme-refine-magenta)))
   `(markup-emphasis-face ((,class :foreground ,fg-special-cold :slant italic)))
   `(markup-error-face ((,class :inherit bold :foreground ,red)))
   `(markup-gen-face ((,class :foreground ,magenta-alt)))
   `(markup-internal-reference-face ((,class :inherit button :foreground ,fg-inactive)))
   `(markup-italic-face ((,class :foreground ,fg-special-cold :slant italic)))
   `(markup-list-face ((,class :inherit modus-theme-special-calm)))
   `(markup-meta-face ((,class :foreground ,fg-inactive)))
   `(markup-meta-hide-face ((,class :foreground ,fg-alt)))
   `(markup-passthrough-face ((,class :inherit fixed-pitch :foreground ,cyan)))
   `(markup-preprocessor-face ((,class :foreground ,red-alt-other)))
   `(markup-replacement-face ((,class :foreground ,yellow-alt-other)))
   `(markup-secondary-text-face ((,class :height 0.8 :foreground ,magenta-nuanced)))
   `(markup-small-face ((,class :height 0.8 :foreground ,fg-main)))
   `(markup-strong-face ((,class :inherit bold :foreground ,red-nuanced)))
   `(markup-subscript-face ((,class :height 0.8 :foreground ,fg-special-cold)))
   `(markup-superscript-face ((,class :height 0.8 :foreground ,fg-special-cold)))
   `(markup-table-cell-face ((,class :inherit modus-theme-special-cold)))
   `(markup-table-face ((,class :inherit modus-theme-subtle-cyan)))
   `(markup-table-row-face ((,class :inherit modus-theme-subtle-cyan)))
   `(markup-title-0-face ((,class :height 3.0 :foreground ,blue-nuanced)))
   `(markup-title-1-face ((,class :height 2.4 :foreground ,blue-nuanced)))
   `(markup-title-2-face ((,class :height 1.8 :foreground ,blue-nuanced)))
   `(markup-title-3-face ((,class :height 1.4 :foreground ,blue-nuanced)))
   `(markup-title-4-face ((,class :height 1.2 :foreground ,blue-nuanced)))
   `(markup-title-5-face ((,class :height 1.2 :foreground ,blue-nuanced :underline t)))
   `(markup-value-face ((,class :foreground ,fg-inactive)))
   `(markup-verbatim-face ((,class :inherit modus-theme-special-mild)))
;;;;; mentor
   `(mentor-download-message ((,class :foreground ,fg-special-warm)))
   `(mentor-download-name ((,class :foreground ,fg-special-cold)))
   `(mentor-download-progress ((,class :foreground ,blue-alt-other)))
   `(mentor-download-size ((,class :foreground ,magenta-alt-other)))
   `(mentor-download-speed-down ((,class :foreground ,cyan-alt)))
   `(mentor-download-speed-up ((,class :foreground ,red-alt)))
   `(mentor-download-state ((,class :foreground ,yellow-alt)))
   `(mentor-highlight-face ((,class :inherit modus-theme-subtle-blue)))
   `(mentor-tracker-name ((,class :foreground ,magenta-alt)))
;;;;; messages
   `(message-cited-text-1 ((,class :foreground ,blue-alt)))
   `(message-cited-text-2 ((,class :foreground ,red-alt)))
   `(message-cited-text-3 ((,class :foreground ,green-alt)))
   `(message-cited-text-4 ((,class :foreground ,magenta-alt)))
   `(message-header-cc ((,class :inherit bold :foreground ,cyan-alt)))
   `(message-header-name ((,class :foreground ,green-alt-other)))
   `(message-header-newsgroups ((,class :inherit bold :foreground ,green-alt)))
   `(message-header-other ((,class :inherit bold :foreground ,cyan-alt-other)))
   `(message-header-subject ((,class :inherit bold :foreground ,magenta-alt-other)))
   `(message-header-to ((,class :inherit bold :foreground ,blue)))
   `(message-header-xheader ((,class :foreground ,cyan)))
   `(message-mml ((,class :foreground ,fg-special-warm)))
   `(message-separator ((,class :inherit modus-theme-intense-neutral)))
;;;;; minibuffer-line
   `(minibuffer-line ((,class :foreground ,fg-main)))
;;;;; minimap
   `(minimap-active-region-background ((,class :background ,bg-active)))
   `(minimap-current-line-face ((,class :background ,cyan-intense-bg :foreground ,fg-main)))
;;;;; modeline
   `(mode-line ((,class ,@(modus-vivendi-theme-mode-line-attrs
                           fg-active bg-active fg-dim bg-active
                           fg-alt bg-active 'alt-style nil bg-main))))
   `(mode-line-buffer-id ((,class :inherit bold)))
   `(mode-line-emphasis ((,class :inherit bold :foreground ,blue-active)))
   `(mode-line-highlight ((,class :inherit modus-theme-active-blue :box (:line-width -1 :style pressed-button))))
   `(mode-line-inactive ((,class ,@(modus-vivendi-theme-mode-line-attrs
                                    fg-inactive bg-inactive fg-alt bg-dim
                                    bg-region bg-active))))
;;;;; mood-line
   `(mood-line-modified ((,class :foreground ,magenta-active)))
   `(mood-line-status-error ((,class :inherit bold :foreground ,red-active)))
   `(mood-line-status-info ((,class :foreground ,cyan-active)))
   `(mood-line-status-neutral ((,class :foreground ,blue-active)))
   `(mood-line-status-success ((,class :foreground ,green-active)))
   `(mood-line-status-warning ((,class :inherit bold :foreground ,yellow-active)))
   `(mood-line-unimportant ((,class :foreground ,fg-inactive)))
;;;;; mpdel
   `(mpdel-browser-directory-face ((,class :foreground ,blue)))
   `(mpdel-playlist-current-song-face ((,class :inherit bold :foreground ,blue-alt-other)))
;;;;; mu4e
   `(mu4e-attach-number-face ((,class :inherit bold :foreground ,cyan-alt)))
   `(mu4e-cited-1-face ((,class :foreground ,blue-alt)))
   `(mu4e-cited-2-face ((,class :foreground ,red-alt)))
   `(mu4e-cited-3-face ((,class :foreground ,green-alt)))
   `(mu4e-cited-4-face ((,class :foreground ,magenta-alt)))
   `(mu4e-cited-5-face ((,class :foreground ,yellow-alt)))
   `(mu4e-cited-6-face ((,class :foreground ,cyan-alt)))
   `(mu4e-cited-7-face ((,class :foreground ,magenta)))
   `(mu4e-compose-header-face ((,class :inherit mu4e-compose-separator-face)))
   `(mu4e-compose-separator-face ((,class :inherit modus-theme-intense-neutral)))
   `(mu4e-contact-face ((,class :inherit bold :foreground ,cyan-alt-other)))
   `(mu4e-context-face ((,class :foreground ,blue-active)))
   `(mu4e-draft-face ((,class :foreground ,magenta-alt)))
   `(mu4e-flagged-face ((,class :foreground ,red-alt)))
   `(mu4e-footer-face ((,class :foreground ,fg-special-cold :slant ,modus-theme-slant)))
   `(mu4e-forwarded-face ((,class :foreground ,magenta-alt-other)))
   `(mu4e-header-face ((,class :foreground ,fg-alt)))
   `(mu4e-header-highlight-face ((,class :inherit modus-theme-hl-line)))
   `(mu4e-header-key-face ((,class :foreground ,cyan)))
   `(mu4e-header-marks-face ((,class :inherit bold :foreground ,magenta-alt)))
   `(mu4e-header-title-face ((,class :foreground ,fg-special-mild)))
   `(mu4e-header-value-face ((,class :inherit bold :foreground ,magenta-alt-other)))
   `(mu4e-highlight-face ((,class :inherit bold :foreground ,blue-alt-other)))
   `(mu4e-link-face ((,class :inherit link)))
   `(mu4e-modeline-face ((,class :foreground ,magenta-active)))
   `(mu4e-moved-face ((,class :foreground ,yellow :slant ,modus-theme-slant)))
   `(mu4e-ok-face ((,class :inherit bold :foreground ,green)))
   `(mu4e-region-code ((,class :inherit modus-theme-special-calm)))
   `(mu4e-replied-face ((,class :foreground ,blue-faint)))
   `(mu4e-special-header-value-face ((,class :inherit bold :foreground ,blue-alt-other)))
   `(mu4e-system-face ((,class :foreground ,fg-mark-del :slant ,modus-theme-slant)))
   `(mu4e-title-face ((,class :foreground ,fg-main)))
   `(mu4e-trashed-face ((,class :foreground ,red)))
   `(mu4e-unread-face ((,class :inherit bold :foreground ,fg-main)))
   `(mu4e-url-number-face ((,class :inherit bold :foreground ,cyan-alt-other)))
   `(mu4e-view-body-face ((,class :foreground ,fg-main)))
   `(mu4e-warning-face ((,class :inherit warning)))
;;;;; mu4e-conversation
   `(mu4e-conversation-header ((,class :inherit modus-theme-special-cold)))
   `(mu4e-conversation-sender-1 ((,class :foreground ,fg-special-warm)))
   `(mu4e-conversation-sender-2 ((,class :foreground ,fg-special-cold)))
   `(mu4e-conversation-sender-3 ((,class :foreground ,fg-special-mild)))
   `(mu4e-conversation-sender-4 ((,class :foreground ,fg-alt)))
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
   `(neo-vc-added-face ((,class :foreground ,green)))
   `(neo-vc-conflict-face ((,class :inherit bold :foreground ,red)))
   `(neo-vc-default-face ((,class :foreground ,fg-main)))
   `(neo-vc-edited-face ((,class :foreground ,yellow)))
   `(neo-vc-ignored-face ((,class :foreground ,fg-inactive)))
   `(neo-vc-missing-face ((,class :foreground ,red-alt)))
   `(neo-vc-needs-merge-face ((,class :foreground ,magenta-alt)))
   `(neo-vc-needs-update-face ((,class :underline t)))
   `(neo-vc-removed-face ((,class :strike-through t)))
   `(neo-vc-unlocked-changes-face ((,class :inherit modus-theme-refine-blue)))
   `(neo-vc-up-to-date-face ((,class :foreground ,fg-alt)))
   `(neo-vc-user-face ((,class :foreground ,magenta)))
;;;;; no-emoji
   `(no-emoji ((,class :foreground ,cyan)))
;;;;; notmuch
   `(notmuch-crypto-decryption ((,class :inherit modus-theme-refine-magenta)))
   `(notmuch-crypto-part-header ((,class :foreground ,magenta-alt-other)))
   `(notmuch-crypto-signature-bad ((,class :inherit modus-theme-intense-red)))
   `(notmuch-crypto-signature-good ((,class :inherit modus-theme-refine-green)))
   `(notmuch-crypto-signature-good-key ((,class :inherit modus-theme-refine-yellow)))
   `(notmuch-crypto-signature-unknown ((,class :inherit modus-theme-refine-red)))
   `(notmuch-hello-logo-background ((,class :background ,bg-main)))
   `(notmuch-message-summary-face ((,class :inherit modus-theme-nuanced-cyan)))
   `(notmuch-search-flagged-face ((,class :foreground ,red-alt)))
   `(notmuch-search-matching-authors ((,class :foreground ,fg-main)))
   `(notmuch-search-non-matching-authors ((,class :foreground ,fg-alt)))
   `(notmuch-search-unread-face ((,class :inherit bold)))
   `(notmuch-tag-added
     ((,(append '((supports :underline (:style wave))) class)
       :underline (:color ,green :style wave))
      (,class :foreground ,green :underline t)))
   `(notmuch-tag-deleted
     ((,(append '((supports :underline (:style wave))) class)
       :underline (:color ,red :style wave))
      (,class :foreground ,red :underline t)))
   `(notmuch-tag-face ((,class :inherit bold :foreground ,blue-alt)))
   `(notmuch-tag-flagged ((,class :foreground ,red-alt)))
   `(notmuch-tag-unread ((,class :foreground ,magenta-alt)))
   `(notmuch-tree-match-author-face ((,class :foreground ,fg-special-cold)))
   `(notmuch-tree-match-face ((,class :foreground ,fg-main)))
   `(notmuch-tree-match-tag-face ((,class :inherit bold :foreground ,blue-alt)))
   `(notmuch-tree-no-match-face ((,class :foreground ,fg-alt)))
   `(notmuch-wash-cited-text ((,class :foreground ,cyan)))
   `(notmuch-wash-toggle-button ((,class :background ,bg-alt :foreground ,fg-alt)))
;;;;; num3-mode
   `(num3-face-even ((,class :inherit bold :background ,bg-alt)))
;;;;; nxml-mode
   `(nxml-attribute-colon ((,class :foreground ,fg-main)))
   `(nxml-attribute-local-name ((,class ,@(modus-vivendi-theme-syntax-foreground
                                           cyan-alt cyan-alt-faint))))
   `(nxml-attribute-prefix ((,class ,@(modus-vivendi-theme-syntax-foreground
                                       cyan-alt-other cyan-alt-other-faint)
                                    ,@(modus-vivendi-theme-bold-weight))))
   `(nxml-attribute-value ((,class ,@(modus-vivendi-theme-syntax-foreground
                                      blue blue-faint))))
   `(nxml-cdata-section-CDATA ((,class ,@(modus-vivendi-theme-syntax-foreground
                                          red-alt red-alt-faint)
                                       ,@(modus-vivendi-theme-bold-weight))))
   `(nxml-cdata-section-delimiter ((,class ,@(modus-vivendi-theme-syntax-foreground
                                              red-alt red-alt-faint))))
   `(nxml-char-ref-delimiter ((,class ,@(modus-vivendi-theme-syntax-foreground
                                         green-alt-other green-alt-other-faint))))
   `(nxml-char-ref-number ((,class ,@(modus-vivendi-theme-syntax-foreground
                                      green-alt-other green-alt-other-faint)
                                   ,@(modus-vivendi-theme-bold-weight))))
   `(nxml-delimited-data ((,class :foreground ,fg-special-cold :slant ,modus-theme-slant)))
   `(nxml-delimiter ((,class :foreground ,fg-dim)))
   `(nxml-element-colon ((,class :foreground ,fg-main)))
   `(nxml-element-local-name ((,class ,@(modus-vivendi-theme-syntax-foreground
                                         magenta magenta-faint))))
   `(nxml-element-prefix ((,class ,@(modus-vivendi-theme-syntax-foreground
                                     magenta-alt magenta-alt-faint)
                                  ,@(modus-vivendi-theme-bold-weight))))
   `(nxml-entity-ref-delimiter ((,class ,@(modus-vivendi-theme-syntax-foreground
                                           green-alt-other green-alt-other-faint))))
   `(nxml-entity-ref-name ((,class ,@(modus-vivendi-theme-syntax-foreground
                                      green-alt-other green-alt-other-faint)
                                   ,@(modus-vivendi-theme-bold-weight))))
   `(nxml-glyph ((,class :inherit modus-theme-intense-neutral)))
   `(nxml-hash ((,class ,@(modus-vivendi-theme-syntax-foreground
                           blue-alt blue-alt-faint)
                        ,@(modus-vivendi-theme-bold-weight))))
   `(nxml-heading ((,class :inherit bold)))
   `(nxml-name ((,class ,@(modus-vivendi-theme-syntax-foreground
                           magenta-alt magenta-alt-faint)
                        ,@(modus-vivendi-theme-bold-weight))))
   `(nxml-namespace-attribute-colon ((,class :foreground ,fg-main)))
   `(nxml-namespace-attribute-prefix ((,class ,@(modus-vivendi-theme-syntax-foreground
                                                 cyan cyan-faint))))
   `(nxml-processing-instruction-target ((,class ,@(modus-vivendi-theme-syntax-foreground
                                                    magenta-alt-other magenta-alt-other-faint)
                                                 ,@(modus-vivendi-theme-bold-weight))))
   `(nxml-prolog-keyword ((,class ,@(modus-vivendi-theme-syntax-foreground
                                     magenta-alt-other magenta-alt-other-faint)
                                  ,@(modus-vivendi-theme-bold-weight))))
   `(nxml-ref ((,class ,@(modus-vivendi-theme-syntax-foreground
                          green-alt-other green-alt-other-faint)
                       ,@(modus-vivendi-theme-bold-weight))))
;;;;; objed
   `(objed-hl ((,class :background ,(if modus-vivendi-theme-intense-hl-line
                                        bg-hl-alt-intense bg-hl-alt))))
   `(objed-mark ((,class :background ,bg-active)))
   `(objed-mode-line ((,class :foreground ,cyan-active)))
;;;;; orderless
   `(orderless-match-face-0 ((,class :inherit bold
                                     ,@(modus-vivendi-theme-standard-completions
                                        blue-alt-other blue-nuanced-bg
                                        blue-refine-bg blue-refine-fg))))
   `(orderless-match-face-1 ((,class :inherit bold
                                     ,@(modus-vivendi-theme-standard-completions
                                        magenta-alt magenta-nuanced-bg
                                        magenta-refine-bg magenta-refine-fg))))
   `(orderless-match-face-2 ((,class :inherit bold
                                     ,@(modus-vivendi-theme-standard-completions
                                        green green-nuanced-bg
                                        green-refine-bg green-refine-fg))))
   `(orderless-match-face-3 ((,class :inherit bold
                                     ,@(modus-vivendi-theme-standard-completions
                                        yellow yellow-nuanced-bg
                                        yellow-refine-bg yellow-refine-fg))))
;;;;; org
   `(org-agenda-calendar-event ((,class :foreground ,fg-main)))
   `(org-agenda-calendar-sexp ((,class :foreground ,cyan-alt)))
   `(org-agenda-clocking ((,class :inherit modus-theme-special-cold
                                  ,@(and (>= emacs-major-version 27) '(:extend t)))))
   `(org-agenda-column-dateline ((,class :background ,bg-alt)))
   `(org-agenda-current-time ((,class :inherit bold :foreground ,blue-alt-other)))
   `(org-agenda-date ((,class :foreground ,cyan)))
   `(org-agenda-date-today ((,class :inherit bold :foreground ,fg-main :underline t)))
   `(org-agenda-date-weekend ((,class :foreground ,cyan-alt-other)))
   `(org-agenda-diary ((,class :foreground ,fg-main)))
   `(org-agenda-dimmed-todo-face ((,class :inherit bold :foreground ,fg-alt)))
   `(org-agenda-done ((,class :foreground ,green-alt)))
   `(org-agenda-filter-category ((,class :inherit bold :foreground ,magenta-active)))
   `(org-agenda-filter-effort ((,class :inherit bold :foreground ,magenta-active)))
   `(org-agenda-filter-regexp ((,class :inherit bold :foreground ,magenta-active)))
   `(org-agenda-filter-tags ((,class :inherit bold :foreground ,magenta-active)))
   `(org-agenda-restriction-lock ((,class :background ,bg-dim :foreground ,fg-dim)))
   `(org-agenda-structure ((,class :foreground ,blue-alt)))
   `(org-archived ((,class :background ,bg-alt :foreground ,fg-alt)))
   `(org-block ((,class ,@(modus-vivendi-theme-mixed-fonts)
                        ,@(modus-vivendi-theme-org-block bg-dim)
                        :foreground ,fg-main)))
   `(org-block-begin-line ((,class ,@(modus-vivendi-theme-mixed-fonts)
                                   ,@(modus-vivendi-theme-org-block-delim
                                      bg-dim fg-special-cold
                                      bg-alt fg-special-mild))))
   `(org-block-end-line ((,class :inherit org-block-begin-line)))
   `(org-checkbox ((,class :box (:line-width 1 :color ,bg-active)
                           :background ,bg-inactive :foreground ,fg-active)))
   `(org-checkbox-statistics-done ((,class :inherit org-done)))
   `(org-checkbox-statistics-todo ((,class :inherit org-todo)))
   `(org-clock-overlay ((,class :inherit modus-theme-special-cold)))
   `(org-code ((,class ,@(modus-vivendi-theme-mixed-fonts) :foreground ,magenta)))
   `(org-column ((,class :background ,bg-alt)))
   `(org-column-title ((,class :inherit bold :underline t :background ,bg-alt)))
   `(org-date ((,class :inherit (button fixed-pitch) :foreground ,cyan-alt-other)))
   `(org-date-selected ((,class :inherit bold :foreground ,blue-alt :inverse-video t)))
   `(org-document-info ((,class :foreground ,fg-special-cold)))
   `(org-document-info-keyword ((,class ,@(modus-vivendi-theme-mixed-fonts)
                                        :foreground ,fg-alt)))
   `(org-document-title ((,class :inherit (bold ,modus-theme-variable-pitch) :foreground ,fg-special-cold
                                 ,@(modus-vivendi-theme-scale modus-vivendi-theme-scale-5))))
   `(org-done ((,class :box ,bg-region :background ,bg-dim :foreground ,green)))
   `(org-drawer ((,class ,@(modus-vivendi-theme-mixed-fonts)
                         :foreground ,cyan)))
   `(org-ellipsis ((,class :foreground nil))) ; inherits from the heading's colour
   `(org-footnote ((,class :inherit button :foreground ,blue-alt)))
   `(org-formula ((,class ,@(modus-vivendi-theme-mixed-fonts)
                          :foreground ,red-alt)))
   `(org-habit-alert-face ((,class :inherit modus-theme-intense-yellow)))
   `(org-habit-alert-future-face ((,class :inherit modus-theme-refine-yellow)))
   `(org-habit-clear-face ((,class :inherit modus-theme-intense-magenta)))
   `(org-habit-clear-future-face ((,class :inherit modus-theme-refine-magenta)))
   `(org-habit-overdue-face ((,class :inherit modus-theme-intense-red)))
   `(org-habit-overdue-future-face ((,class :inherit modus-theme-refine-red)))
   `(org-habit-ready-face ((,class :inherit modus-theme-intense-blue)))
   `(org-habit-ready-future-face ((,class :inherit modus-theme-refine-blue)))
   `(org-headline-done ((,class :inherit ,modus-theme-variable-pitch :foreground ,green-nuanced)))
   `(org-headline-todo ((,class :inherit ,modus-theme-variable-pitch :foreground ,red-nuanced)))
   `(org-hide ((,class :foreground ,bg-main)))
   `(org-indent ((,class :inherit (fixed-pitch org-hide))))
   `(org-latex-and-related ((,class :foreground ,magenta-refine-fg)))
   `(org-level-1 ((,class :inherit modus-theme-heading-1)))
   `(org-level-2 ((,class :inherit modus-theme-heading-2)))
   `(org-level-3 ((,class :inherit modus-theme-heading-3)))
   `(org-level-4 ((,class :inherit modus-theme-heading-4)))
   `(org-level-5 ((,class :inherit modus-theme-heading-5)))
   `(org-level-6 ((,class :inherit modus-theme-heading-6)))
   `(org-level-7 ((,class :inherit modus-theme-heading-7)))
   `(org-level-8 ((,class :inherit modus-theme-heading-8)))
   `(org-link ((,class :inherit link)))
   `(org-list-dt ((,class :inherit bold)))
   `(org-macro ((,class :background ,blue-nuanced-bg :foreground ,magenta-alt-other)))
   `(org-meta-line ((,class ,@(modus-vivendi-theme-mixed-fonts) :foreground ,fg-alt)))
   `(org-mode-line-clock ((,class :foreground ,fg-main)))
   `(org-mode-line-clock-overrun ((,class :inherit modus-theme-active-red)))
   `(org-priority ((,class :box ,bg-region :background ,bg-dim :foreground ,magenta)))
   `(org-property-value ((,class ,@(modus-vivendi-theme-mixed-fonts)
                                 :foreground ,cyan-alt-other)))
   `(org-quote ((,class ,@(modus-vivendi-theme-org-block bg-dim)
                        :foreground ,fg-special-cold :slant ,modus-theme-slant)))
   `(org-scheduled ((,class :foreground ,fg-special-warm)))
   `(org-scheduled-previously ((,class :foreground ,yellow-alt-other)))
   `(org-scheduled-today ((,class :foreground ,magenta-alt-other)))
   `(org-sexp-date ((,class :inherit org-date)))
   `(org-special-keyword ((,class ,@(modus-vivendi-theme-mixed-fonts)
                                  :foreground ,blue-nuanced)))
   `(org-table ((,class ,@(modus-vivendi-theme-mixed-fonts)
                        :foreground ,fg-special-cold)))
   `(org-table-header ((,class :inherit (fixed-pitch modus-theme-intense-neutral))))
   `(org-tag ((,class :foreground ,magenta-nuanced)))
   `(org-tag-group ((,class :inherit bold :foreground ,cyan-nuanced)))
   `(org-target ((,class :underline t)))
   `(org-time-grid ((,class :foreground ,fg-unfocused)))
   `(org-todo ((,class :box ,bg-region :background ,bg-dim :foreground ,red-alt)))
   `(org-upcoming-deadline ((,class :foreground ,red-alt-other)))
   `(org-upcoming-distant-deadline ((,class :foreground ,red-nuanced)))
   `(org-verbatim ((,class ,@(modus-vivendi-theme-mixed-fonts)
                           :background ,bg-alt :foreground ,fg-special-calm)))
   `(org-verse ((,class :inherit org-quote)))
   `(org-warning ((,class :inherit bold :foreground ,red-alt-other)))
;;;;; org-journal
   `(org-journal-calendar-entry-face ((,class :foreground ,yellow-alt-other :slant ,modus-theme-slant)))
   `(org-journal-calendar-scheduled-face ((,class :foreground ,red-alt-other :slant ,modus-theme-slant)))
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
   `(org-roam-link ((,class :inherit button :foreground ,green)))
   `(org-roam-link-current ((,class :inherit button :foreground ,green-alt)))
   `(org-roam-link-invalid ((,class :inherit button :foreground ,red)))
   `(org-roam-link-shielded ((,class :inherit button :foreground ,yellow)))
   `(org-roam-tag ((,class :foreground ,fg-alt :slant italic)))
;;;;; org-superstar
   `(org-superstar-item ((,class :foreground ,fg-main)))
   `(org-superstar-leading ((,class :foreground ,fg-whitespace)))
;;;;; org-table-sticky-header
   `(org-table-sticky-header-face ((,class :inherit modus-theme-intense-neutral)))
;;;;; org-treescope
   `(org-treescope-faces--markerinternal-midday ((,class :inherit modus-theme-intense-blue)))
   `(org-treescope-faces--markerinternal-range ((,class :inherit modus-theme-special-mild)))
;;;;; origami
   `(origami-fold-header-face ((,class :background ,bg-dim :foreground ,fg-dim :box t)))
   `(origami-fold-replacement-face ((,class :background ,bg-alt :foreground ,fg-alt)))
;;;;; outline-mode
   `(outline-1 ((,class :inherit modus-theme-heading-1)))
   `(outline-2 ((,class :inherit modus-theme-heading-2)))
   `(outline-3 ((,class :inherit modus-theme-heading-3)))
   `(outline-4 ((,class :inherit modus-theme-heading-4)))
   `(outline-5 ((,class :inherit modus-theme-heading-5)))
   `(outline-6 ((,class :inherit modus-theme-heading-6)))
   `(outline-7 ((,class :inherit modus-theme-heading-7)))
   `(outline-8 ((,class :inherit modus-theme-heading-8)))
;;;;; outline-minor-faces
   `(outline-minor-0 ((,class nil)))
;;;;; package (M-x list-packages)
   `(package-description ((,class :foreground ,fg-special-cold)))
   `(package-help-section-name ((,class :inherit bold :foreground ,magenta-alt-other)))
   `(package-name ((,class :inherit link)))
   `(package-status-avail-obso ((,class :inherit bold :foreground ,red)))
   `(package-status-available ((,class :foreground ,fg-special-mild)))
   `(package-status-built-in ((,class :foreground ,magenta)))
   `(package-status-dependency ((,class :foreground ,magenta-alt-other)))
   `(package-status-disabled ((,class :inherit modus-theme-subtle-red)))
   `(package-status-external ((,class :foreground ,cyan-alt-other)))
   `(package-status-held ((,class :foreground ,yellow-alt)))
   `(package-status-incompat ((,class :inherit bold :foreground ,yellow)))
   `(package-status-installed ((,class :foreground ,fg-special-warm)))
   `(package-status-new ((,class :inherit bold :foreground ,green)))
   `(package-status-unsigned ((,class :inherit bold :foreground ,red-alt)))
;;;;; page-break-lines
   `(page-break-lines ((,class :inherit default :foreground ,fg-window-divider-outer)))
;;;;; paradox
   `(paradox-archive-face ((,class :foreground ,fg-special-mild)))
   `(paradox-comment-face ((,class :foreground ,fg-alt :slant ,modus-theme-slant)))
   `(paradox-commit-tag-face ((,class :inherit modus-theme-refine-magenta :box t)))
   `(paradox-description-face ((,class :foreground ,fg-special-cold)))
   `(paradox-description-face-multiline ((,class :foreground ,fg-special-cold)))
   `(paradox-download-face ((,class ,@(modus-vivendi-theme-bold-weight) :foreground ,blue-alt-other)))
   `(paradox-highlight-face ((,class ,@(modus-vivendi-theme-bold-weight) :foreground ,cyan-alt-other)))
   `(paradox-homepage-button-face ((,class :foreground ,magenta-alt-other :underline t)))
   `(paradox-mode-line-face ((,class :inherit bold :foreground ,cyan-active)))
   `(paradox-name-face ((,class :foreground ,blue :underline t)))
   `(paradox-star-face ((,class :foreground ,magenta)))
   `(paradox-starred-face ((,class :foreground ,magenta-alt)))
;;;;; paren-face
   `(parenthesis ((,class :foreground ,fg-unfocused)))
;;;;; parrot
   `(parrot-rotate-rotation-highlight-face ((,class :inherit modus-theme-refine-magenta)))
;;;;; pass
   `(pass-mode-directory-face ((,class :inherit bold :foreground ,fg-special-cold)))
   `(pass-mode-entry-face ((,class :background ,bg-main :foreground ,fg-main)))
   `(pass-mode-header-face ((,class :foreground ,fg-special-warm)))
;;;;; persp-mode
   `(persp-face-lighter-buffer-not-in-persp ((,class :inherit modus-theme-intense-red)))
   `(persp-face-lighter-default ((,class :inherit bold :foreground ,blue-active)))
   `(persp-face-lighter-nil-persp ((,class :inherit bold :foreground ,fg-active)))
;;;;; perspective
   `(persp-selected-face ((,class :inherit bold :foreground ,blue-active)))
;;;;; phi-grep
   `(phi-grep-heading-face  ((,class :inherit bold :foreground ,red-alt
                                     ,@(modus-vivendi-theme-scale modus-vivendi-theme-scale-4))))
   `(phi-grep-line-number-face ((,class :foreground ,fg-special-warm)))
   `(phi-grep-match-face ((,class :inherit modus-theme-special-calm)))
   `(phi-grep-modified-face ((,class :inherit modus-theme-refine-yellow)))
   `(phi-grep-overlay-face ((,class :inherit modus-theme-refine-blue)))
;;;;; phi-search
   `(phi-replace-preview-face ((,class :inherit modus-theme-intense-magenta)))
   `(phi-search-failpart-face ((,class :inherit modus-theme-refine-red)))
   `(phi-search-match-face ((,class :inherit modus-theme-refine-cyan)))
   `(phi-search-selection-face ((,class :inherit (modus-theme-intense-green bold))))
;;;;; pkgbuild-mode
   `(pkgbuild-error-face ((,class :underline ,fg-lang-error)))
;;;;; pomidor
   `(pomidor-break-face ((,class :foreground ,blue-alt-other)))
   `(pomidor-overwork-face ((,class :foreground ,red-alt-other)))
   `(pomidor-skip-face ((,class :foreground ,fg-alt :slant ,modus-theme-slant)))
   `(pomidor-work-face ((,class :foreground ,green-alt-other)))
;;;;; powerline
   `(powerline-active0 ((,class :background ,bg-main :foreground ,blue-faint :inverse-video t)))
   `(powerline-active1 ((,class :background ,blue-nuanced-bg :foreground ,blue-nuanced)))
   `(powerline-active2 ((,class :background ,bg-active :foreground ,fg-active)))
   `(powerline-inactive0 ((,class :background ,bg-special-cold :foreground ,fg-special-cold)))
   `(powerline-inactive1 ((,class :background ,bg-dim :foreground ,fg-inactive)))
   `(powerline-inactive2 ((,class :background ,bg-inactive :foreground ,fg-inactive)))
;;;;; powerline-evil
   `(powerline-evil-base-face ((,class :background ,fg-main :foreground ,bg-main)))
   `(powerline-evil-emacs-face ((,class :inherit modus-theme-active-magenta)))
   `(powerline-evil-insert-face ((,class :inherit modus-theme-active-green)))
   `(powerline-evil-motion-face ((,class :inherit modus-theme-active-blue)))
   `(powerline-evil-normal-face ((,class :background ,fg-alt :foreground ,bg-main)))
   `(powerline-evil-operator-face ((,class :inherit modus-theme-active-yellow)))
   `(powerline-evil-replace-face ((,class :inherit modus-theme-active-red)))
   `(powerline-evil-visual-face ((,class :inherit modus-theme-active-cyan)))
;;;;; proced
   `(proced-mark ((,class :inherit modus-theme-mark-symbol)))
   `(proced-marked ((,class :inherit modus-theme-mark-alt)))
   `(proced-sort-header ((,class :inherit bold :foreground ,fg-special-calm :underline t)))
;;;;; prodigy
   `(prodigy-green-face ((,class :foreground ,green)))
   `(prodigy-red-face ((,class :foreground ,red)))
   `(prodigy-yellow-face ((,class :foreground ,yellow)))
;;;;; racket-mode
   `(racket-debug-break-face ((,class :inherit modus-theme-intense-red)))
   `(racket-debug-locals-face ((,class :box (:line-width -1 :color nil)
                                       :foreground ,green-alt-other)))
   `(racket-debug-result-face ((,class :inherit bold :box (:line-width -1 :color nil)
                                       :foreground ,green)))
   `(racket-here-string-face ((,class :foreground ,blue-alt)))
   `(racket-keyword-argument-face ((,class :foreground ,red-alt)))
   `(racket-logger-config-face ((,class :foreground ,fg-alt :slant ,modus-theme-slant)))
   `(racket-logger-debug-face ((,class :foreground ,blue-alt-other)))
   `(racket-logger-info-face ((,class :foreground ,fg-lang-note)))
   `(racket-logger-topic-face ((,class :foreground ,magenta :slant ,modus-theme-slant)))
   `(racket-selfeval-face ((,class :foreground ,green-alt)))
   `(racket-xp-error-face
     ((,(append '((supports :underline (:style wave))) class)
       :underline (:color ,fg-lang-error :style wave))
      (,class :foreground ,fg-lang-error :underline t)))
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
   `(rainbow-delimiters-base-face-error ((,class :foreground ,red)))
   `(rainbow-delimiters-base-face ((,class :foreground ,fg-main)))
   `(rainbow-delimiters-depth-1-face ((,class :foreground ,green-alt-other)))
   `(rainbow-delimiters-depth-2-face ((,class :foreground ,magenta-alt-other)))
   `(rainbow-delimiters-depth-3-face ((,class :foreground ,cyan-alt-other)))
   `(rainbow-delimiters-depth-4-face ((,class :foreground ,yellow-alt-other)))
   `(rainbow-delimiters-depth-5-face ((,class :foreground ,blue-alt-other)))
   `(rainbow-delimiters-depth-6-face ((,class :foreground ,green-alt)))
   `(rainbow-delimiters-depth-7-face ((,class :foreground ,magenta-alt)))
   `(rainbow-delimiters-depth-8-face ((,class :foreground ,cyan-alt)))
   `(rainbow-delimiters-depth-9-face ((,class :foreground ,yellow-alt)))
   `(rainbow-delimiters-mismatched-face ((,class :inherit bold :foreground ,red-alt)))
   `(rainbow-delimiters-unmatched-face ((,class :inherit bold :foreground ,red)))
;;;;; rcirc
   `(rcirc-bright-nick ((,class :inherit bold :foreground ,magenta-alt)))
   `(rcirc-dim-nick ((,class :foreground ,fg-alt)))
   `(rcirc-my-nick ((,class :inherit bold :foreground ,magenta)))
   `(rcirc-nick-in-message ((,class :foreground ,magenta-alt-other)))
   `(rcirc-nick-in-message-full-line ((,class :inherit bold :foreground ,fg-special-mild)))
   `(rcirc-other-nick ((,class :inherit bold :foreground ,fg-special-cold)))
   `(rcirc-prompt ((,class :inherit bold :foreground ,cyan-alt-other)))
   `(rcirc-server ((,class :foreground ,fg-unfocused)))
   `(rcirc-timestamp ((,class :foreground ,blue-nuanced)))
   `(rcirc-url ((,class :foreground ,blue :underline t)))
;;;;; regexp-builder (re-builder)
   `(reb-match-0 ((,class :inherit modus-theme-intense-blue)))
   `(reb-match-1 ((,class :inherit modus-theme-intense-magenta)))
   `(reb-match-2 ((,class :inherit modus-theme-intense-green)))
   `(reb-match-3 ((,class :inherit modus-theme-intense-red)))
   `(reb-regexp-grouping-backslash ((,class :inherit bold :foreground ,fg-escape-char-backslash)))
   `(reb-regexp-grouping-construct ((,class :inherit bold :foreground ,fg-escape-char-construct)))
;;;;; rg (rg.el)
   `(rg-column-number-face ((,class :foreground ,magenta-alt-other)))
   `(rg-context-face ((,class :foreground ,fg-unfocused)))
   `(rg-error-face ((,class :inherit bold :foreground ,red)))
   `(rg-file-tag-face ((,class :foreground ,fg-special-cold)))
   `(rg-filename-face ((,class :inherit bold :foreground ,fg-special-cold)))
   `(rg-line-number-face ((,class :foreground ,fg-special-warm)))
   `(rg-literal-face ((,class :foreground ,blue-alt)))
   `(rg-match-face ((,class :inherit modus-theme-special-calm)))
   `(rg-regexp-face ((,class :foreground ,magenta-active)))
   `(rg-toggle-off-face ((,class :inherit bold :foreground ,fg-inactive)))
   `(rg-toggle-on-face ((,class :inherit bold :foreground ,cyan-active)))
   `(rg-warning-face ((,class :inherit bold :foreground ,yellow)))
;;;;; ripgrep
   `(ripgrep-context-face ((,class :foreground ,fg-unfocused)))
   `(ripgrep-error-face ((,class :inherit bold :foreground ,red)))
   `(ripgrep-hit-face ((,class :foreground ,cyan)))
   `(ripgrep-match-face ((,class :inherit modus-theme-special-calm)))
;;;;; rmail
   `(rmail-header-name ((,class :foreground ,cyan-alt-other)))
   `(rmail-highlight ((,class :inherit bold :foreground ,magenta-alt)))
;;;;; ruler-mode
   `(ruler-mode-column-number ((,class :inherit (ruler-mode-default bold) :foreground ,fg-main)))
   `(ruler-mode-comment-column ((,class :inherit ruler-mode-default :foreground ,red-active)))
   `(ruler-mode-current-column ((,class :inherit ruler-mode-default :foreground ,cyan-active :box t)))
   `(ruler-mode-default ((,class :background ,bg-inactive :foreground ,fg-inactive)))
   `(ruler-mode-fill-column ((,class :inherit ruler-mode-default :foreground ,green-active)))
   `(ruler-mode-fringes ((,class :inherit ruler-mode-default :foreground ,blue-active)))
   `(ruler-mode-goal-column ((,class :inherit ruler-mode-default :foreground ,magenta-active)))
   `(ruler-mode-margins ((,class :inherit ruler-mode-default :foreground ,bg-main)))
   `(ruler-mode-pad ((,class :background ,bg-active :foreground ,fg-inactive)))
   `(ruler-mode-tab-stop ((,class :inherit ruler-mode-default :foreground ,yellow-active)))
;;;;; sallet
   `(sallet-buffer-compressed ((,class :foreground ,yellow-nuanced :slant italic)))
   `(sallet-buffer-default-directory ((,class :foreground ,cyan-nuanced)))
   `(sallet-buffer-directory ((,class :foreground ,blue-nuanced)))
   `(sallet-buffer-help ((,class :foreground ,fg-special-cold)))
   `(sallet-buffer-modified ((,class :foreground ,yellow-alt-other :slant italic)))
   `(sallet-buffer-ordinary ((,class :foreground ,fg-main)))
   `(sallet-buffer-read-only ((,class :foreground ,yellow-alt)))
   `(sallet-buffer-size ((,class :foreground ,fg-special-calm)))
   `(sallet-buffer-special ((,class :foreground ,magenta-alt-other)))
   `(sallet-flx-match ((,class ,@(modus-vivendi-theme-extra-completions
                                  'modus-theme-subtle-cyan
                                  'modus-theme-refine-cyan
                                  'modus-theme-nuanced-cyan
                                  cyan-alt-other))))
   `(sallet-recentf-buffer-name ((,class :foreground ,blue-nuanced)))
   `(sallet-recentf-file-path ((,class :foreground ,fg-special-mild)))
   `(sallet-regexp-match ((,class ,@(modus-vivendi-theme-extra-completions
                                     'modus-theme-subtle-magenta
                                     'modus-theme-refine-magenta
                                     'modus-theme-nuanced-magenta
                                     magenta-alt-other))))
   `(sallet-source-header ((,class :inherit bold :foreground ,red-alt
                                   ,@(modus-vivendi-theme-scale modus-vivendi-theme-scale-4))))
   `(sallet-substring-match ((,class ,@(modus-vivendi-theme-extra-completions
                                        'modus-theme-subtle-blue
                                        'modus-theme-refine-blue
                                        'modus-theme-nuanced-blue
                                        blue-alt-other))))
;;;;; selectrum
   `(selectrum-current-candidate
     ((,class :inherit bold :foreground ,fg-main :underline ,fg-main
              :background ,@(pcase modus-vivendi-theme-completions
                              ('opinionated (list bg-active))
                              (_ (list bg-inactive))))))
   `(selectrum-primary-highlight ((,class :inherit bold
                                          ,@(modus-vivendi-theme-standard-completions
                                             magenta-alt magenta-nuanced-bg
                                             magenta-refine-bg magenta-refine-fg))))
   `(selectrum-secondary-highlight ((,class :inherit bold
                                            ,@(modus-vivendi-theme-standard-completions
                                               cyan-alt-other cyan-nuanced-bg
                                               cyan-refine-bg cyan-refine-fg))))
;;;;; semantic
   `(semantic-complete-inline-face ((,class :foreground ,fg-special-warm :underline t)))
   `(semantic-decoration-on-private-members-face ((,class :inherit modus-theme-refine-cyan)))
   `(semantic-decoration-on-protected-members-face ((,class :background ,bg-dim)))
   `(semantic-highlight-edits-face ((,class :background ,bg-alt)))
   `(semantic-highlight-func-current-tag-face ((,class :background ,bg-alt)))
   `(semantic-idle-symbol-highlight ((,class :inherit modus-theme-special-mild)))
   `(semantic-tag-boundary-face ((,class :overline ,blue-intense)))
   `(semantic-unmatched-syntax-face ((,class :underline ,fg-lang-error)))
;;;;; sesman
   `(sesman-browser-button-face ((,class :foreground ,blue-alt-other :underline t)))
   `(sesman-browser-highligh-face ((,class :inherit modus-theme-subtle-blue)))
   `(sesman-buffer-face ((,class :foreground ,magenta)))
   `(sesman-directory-face ((,class :inherit bold :foreground ,blue)))
   `(sesman-project-face ((,class :inherit bold :foreground ,magenta-alt-other)))
;;;;; shell-script-mode
   `(sh-heredoc ((,class :foreground ,blue-alt)))
   `(sh-quoted-exec ((,class ,@(modus-vivendi-theme-bold-weight) :foreground ,magenta-alt)))
;;;;; show-paren-mode
   `(show-paren-match ((,class ,@(modus-vivendi-theme-paren bg-paren-match
                                                            bg-paren-match-intense)
                               :foreground ,fg-main)))
   `(show-paren-match-expression ((,class :inherit modus-theme-special-calm)))
   `(show-paren-mismatch ((,class :inherit modus-theme-intense-red)))
;;;;; side-notes
   `(side-notes ((,class :background ,bg-dim :foreground ,fg-dim)))
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
   `(sml/outside-modified ((,class :inherit modus-theme-intense-red)))
   `(sml/position-percentage ((,class :inherit sml/global)))
   `(sml/prefix ((,class :foreground ,green-active)))
   `(sml/process ((,class :inherit sml/prefix)))
   `(sml/projectile ((,class :inherit sml/git)))
   `(sml/read-only ((,class :inherit bold :foreground ,cyan-active)))
   `(sml/remote ((,class :inherit sml/global)))
   `(sml/sudo ((,class :inherit modus-theme-subtle-red)))
   `(sml/time ((,class :inherit sml/global)))
   `(sml/vc ((,class :inherit sml/git)))
   `(sml/vc-edited ((,class :inherit bold :foreground ,yellow-active)))
;;;;; smartparens
   `(sp-pair-overlay-face ((,class :inherit modus-theme-special-warm)))
   `(sp-show-pair-enclosing ((,class :inherit modus-theme-special-mild)))
   `(sp-show-pair-match-face ((,class ,@(modus-vivendi-theme-paren bg-paren-match
                                                                   bg-paren-match-intense)
                                      :foreground ,fg-main)))
   `(sp-show-pair-mismatch-face ((,class :inherit modus-theme-intense-red)))
   `(sp-wrap-overlay-closing-pair ((,class :inherit sp-pair-overlay-face)))
   `(sp-wrap-overlay-face ((,class :inherit sp-pair-overlay-face)))
   `(sp-wrap-overlay-opening-pair ((,class :inherit sp-pair-overlay-face)))
   `(sp-wrap-tag-overlay-face ((,class :inherit sp-pair-overlay-face)))
;;;;; smerge
   `(smerge-base ((,class :inherit modus-theme-diff-changed)))
   `(smerge-lower ((,class :inherit modus-theme-diff-added)))
   `(smerge-markers ((,class :background ,bg-diff-neutral-2 :foreground ,fg-diff-neutral-2)))
   `(smerge-refined-added ((,class :inherit modus-theme-diff-refine-added)))
   `(smerge-refined-changed ((,class)))
   `(smerge-refined-removed ((,class :inherit modus-theme-diff-refine-removed)))
   `(smerge-upper ((,class :inherit modus-theme-diff-removed)))
;;;;; spaceline
   `(spaceline-evil-emacs ((,class :inherit modus-theme-active-magenta)))
   `(spaceline-evil-insert ((,class :inherit modus-theme-active-green)))
   `(spaceline-evil-motion ((,class :inherit modus-theme-active-blue)))
   `(spaceline-evil-normal ((,class :background ,fg-alt :foreground ,bg-alt)))
   `(spaceline-evil-replace ((,class :inherit modus-theme-active-red)))
   `(spaceline-evil-visual ((,class :inherit modus-theme-active-cyan)))
   `(spaceline-flycheck-error ((,class :foreground ,red-active)))
   `(spaceline-flycheck-info ((,class :foreground ,cyan-active)))
   `(spaceline-flycheck-warning ((,class :foreground ,yellow-active)))
   `(spaceline-highlight-face ((,class :inherit modus-theme-fringe-blue)))
   `(spaceline-modified ((,class :inherit modus-theme-fringe-magenta)))
   `(spaceline-python-venv ((,class :foreground ,magenta-active)))
   `(spaceline-read-only ((,class :inherit modus-theme-fringe-red)))
   `(spaceline-unmodified ((,class :inherit modus-theme-fringe-cyan)))
;;;;; speedbar
   `(speedbar-button-face ((,class :inherit link)))
   `(speedbar-directory-face ((,class :inherit bold :foreground ,blue)))
   `(speedbar-file-face ((,class :foreground ,fg-main)))
   `(speedbar-highlight-face ((,class :inherit modus-theme-subtle-blue)))
   `(speedbar-selected-face ((,class :inherit bold :foreground ,cyan)))
   `(speedbar-separator-face ((,class :inherit modus-theme-intense-neutral)))
   `(speedbar-tag-face ((,class :foreground ,yellow-alt-other)))
;;;;; spell-fu
   `(spell-fu-incorrect-face
     ((,(append '((supports :underline (:style wave))) class)
       :foreground ,fg-lang-error :underline (:style wave))
      (,class :foreground ,fg-lang-error :underline t)))
;;;;; stripes
   `(stripes ((,class :inherit modus-theme-hl-line)))
;;;;; success
   `(suggest-heading ((,class :inherit bold :foreground ,yellow-alt-other)))
;;;;; switch-window
   `(switch-window-background ((,class :background ,bg-dim)))
   `(switch-window-label ((,class :height 3.0 :foreground ,blue-intense)))
;;;;; swiper
   `(swiper-background-match-face-1 ((,class :inherit modus-theme-subtle-neutral)))
   `(swiper-background-match-face-2 ((,class :inherit modus-theme-subtle-cyan)))
   `(swiper-background-match-face-3 ((,class :inherit modus-theme-subtle-magenta)))
   `(swiper-background-match-face-4 ((,class :inherit modus-theme-subtle-green)))
   `(swiper-line-face ((,class ,@(and (>= emacs-major-version 27) '(:extend t))
                               :inherit modus-theme-special-cold)))
   `(swiper-match-face-1 ((,class :inherit swiper-line-face)))
   `(swiper-match-face-2 ((,class :inherit swiper-line-face)))
   `(swiper-match-face-3 ((,class :inherit swiper-line-face)))
   `(swiper-match-face-4 ((,class :inherit swiper-line-face)))
;;;;; swoop
   `(swoop-face-header-format-line ((,class :inherit bold :foreground ,red-alt
                                            ,@(modus-vivendi-theme-scale modus-vivendi-theme-scale-3))))
   `(swoop-face-line-buffer-name ((,class :inherit bold :foreground ,blue-alt
                                          ,@(modus-vivendi-theme-scale modus-vivendi-theme-scale-4))))
   `(swoop-face-line-number ((,class :foreground ,fg-special-warm)))
   `(swoop-face-target-line ((,class :inherit modus-theme-intense-blue
                                     ,@(and (>= emacs-major-version 27) '(:extend t)))))
   `(swoop-face-target-words ((,class :inherit modus-theme-refine-cyan)))
;;;;; sx
   `(sx-inbox-item-type ((,class :foreground ,magenta-alt-other)))
   `(sx-inbox-item-type-unread ((,class :inherit (sx-inbox-item-type bold))))
   `(sx-question-list-answers ((,class :foreground ,green)))
   `(sx-question-list-answers-accepted ((,class :box t :foreground ,green)))
   `(sx-question-list-bounty ((,class :inherit bold :background ,bg-alt :foreground ,yellow)))
   `(sx-question-list-date ((,class :foreground ,fg-special-cold)))
   `(sx-question-list-favorite ((,class :inherit bold :foreground ,fg-special-warm)))
   `(sx-question-list-parent ((,class :foreground ,fg-main)))
   `(sx-question-list-read-question ((,class :foreground ,fg-alt)))
   `(sx-question-list-score ((,class :foreground ,fg-special-mild)))
   `(sx-question-list-score-upvoted ((,class :inherit (sx-question-list-score bold))))
   `(sx-question-list-unread-question ((,class :inherit bold :foreground ,fg-main)))
   `(sx-question-mode-accepted ((,class :inherit bold :height 1.3 :foreground ,green)))
   `(sx-question-mode-closed ((,class :inherit modus-theme-active-yellow :box (:line-width 2 :color nil))))
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
   `(sx-user-reputation ((,class :foreground ,fg-alt)))
;;;;; symbol-overlay
   `(symbol-overlay-default-face ((,class :inherit modus-theme-special-warm)))
   `(symbol-overlay-face-1 ((,class :inherit modus-theme-intense-blue)))
   `(symbol-overlay-face-2 ((,class :inherit modus-theme-refine-magenta)))
   `(symbol-overlay-face-3 ((,class :inherit modus-theme-intense-yellow)))
   `(symbol-overlay-face-4 ((,class :inherit modus-theme-intense-magenta)))
   `(symbol-overlay-face-5 ((,class :inherit modus-theme-intense-red)))
   `(symbol-overlay-face-6 ((,class :inherit modus-theme-refine-red)))
   `(symbol-overlay-face-7 ((,class :inherit modus-theme-intense-cyan)))
   `(symbol-overlay-face-8 ((,class :inherit modus-theme-refine-cyan)))
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
;;;;; table (built-in table.el)
   `(table-cell ((,class :background ,blue-nuanced-bg)))
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
;;;;; term
   `(term ((,class :background ,bg-main :foreground ,fg-main)))
   `(term-bold ((,class :inherit bold)))
   `(term-color-blue ((,class :background ,blue :foreground ,blue)))
   `(term-color-cyan ((,class :background ,cyan :foreground ,cyan)))
   `(term-color-green ((,class :background ,green :foreground ,green)))
   `(term-color-magenta ((,class :background ,magenta :foreground ,magenta)))
   `(term-color-red ((,class :background ,red :foreground ,red)))
   `(term-color-yellow ((,class :background ,yellow :foreground ,yellow)))
   `(term-underline ((,class :underline t)))
;;;;; tomatinho
   `(tomatinho-ok-face ((,class :foreground ,blue-intense)))
   `(tomatinho-pause-face ((,class :foreground ,yellow-intense)))
   `(tomatinho-reset-face ((,class :foreground ,fg-alt)))
;;;;; transient
   `(transient-active-infix ((,class :inherit modus-theme-special-mild)))
   `(transient-amaranth ((,class :inherit bold :foreground ,yellow)))
   `(transient-argument ((,class :inherit bold :foreground ,red-alt)))
   `(transient-blue ((,class :inherit bold :foreground ,blue)))
   `(transient-disabled-suffix ((,class :inherit modus-theme-intense-red)))
   `(transient-enabled-suffix ((,class :inherit modus-theme-intense-green)))
   `(transient-heading ((,class :inherit bold :foreground ,fg-main)))
   `(transient-inactive-argument ((,class :foreground ,fg-alt)))
   `(transient-inactive-value ((,class :foreground ,fg-alt)))
   `(transient-key ((,class :inherit bold :foreground ,blue)))
   `(transient-mismatched-key ((,class :underline t)))
   `(transient-nonstandard-key ((,class :underline t)))
   `(transient-pink ((,class :inherit bold :foreground ,magenta)))
   `(transient-red ((,class :inherit bold :foreground ,red-intense)))
   `(transient-teal ((,class :inherit bold :foreground ,cyan-alt-other)))
   `(transient-unreachable ((,class :foreground ,fg-unfocused)))
   `(transient-unreachable-key ((,class :foreground ,fg-unfocused)))
   `(transient-value ((,class :foreground ,magenta-alt)))
;;;;; trashed
   `(trashed-deleted ((,class :inherit modus-theme-mark-del)))
   `(trashed-directory ((,class :foreground ,blue)))
   `(trashed-mark ((,class :inherit modus-theme-mark-symbol)))
   `(trashed-marked ((,class :inherit modus-theme-mark-alt)))
   `(trashed-restored ((,class :inherit modus-theme-mark-sel)))
   `(trashed-symlink ((,class :inherit button :foreground ,cyan-alt)))
;;;;; treemacs
   `(treemacs-directory-collapsed-face ((,class :foreground ,magenta-alt)))
   `(treemacs-directory-face ((,class :inherit dired-directory)))
   `(treemacs-file-face ((,class :foreground ,fg-main)))
   `(treemacs-fringe-indicator-face ((,class :foreground ,fg-main)))
   `(treemacs-git-added-face ((,class :foreground ,green-intense)))
   `(treemacs-git-conflict-face ((,class :inherit (modus-theme-intense-red bold))))
   `(treemacs-git-ignored-face ((,class :foreground ,fg-alt)))
   `(treemacs-git-modified-face ((,class :foreground ,yellow-alt-other)))
   `(treemacs-git-renamed-face ((,class :foreground ,cyan-alt-other)))
   `(treemacs-git-unmodified-face ((,class :foreground ,fg-main)))
   `(treemacs-git-untracked-face ((,class :foreground ,red-alt-other)))
   `(treemacs-help-column-face ((,class ,@(modus-vivendi-theme-bold-weight) :foreground ,magenta-alt-other :underline t)))
   `(treemacs-help-title-face ((,class :foreground ,blue-alt-other)))
   `(treemacs-on-failure-pulse-face ((,class :inherit modus-theme-intense-red)))
   `(treemacs-on-success-pulse-face ((,class :inherit modus-theme-intense-green)))
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
   `(tty-menu-selected-face ((,class :inherit modus-theme-intense-blue)))
;;;;; tuareg
   `(caml-types-def-face ((,class :inherit modus-theme-subtle-red)))
   `(caml-types-expr-face ((,class :inherit modus-theme-subtle-green)))
   `(caml-types-occ-face ((,class :inherit modus-theme-subtle-green)))
   `(caml-types-scope-face ((,class :inherit modus-theme-subtle-blue)))
   `(caml-types-typed-face ((,class :inherit modus-theme-subtle-magenta)))
   `(tuareg-font-double-semicolon-face ((,class ,@(modus-vivendi-theme-syntax-foreground
                                                   red-alt red-alt-faint))))
   `(tuareg-font-lock-attribute-face ((,class ,@(modus-vivendi-theme-syntax-foreground
                                                 magenta magenta-faint))))
   `(tuareg-font-lock-constructor-face ((,class :foreground ,fg-main)))
   `(tuareg-font-lock-error-face ((,class :inherit (modus-theme-intense-red bold))))
   `(tuareg-font-lock-extension-node-face ((,class :background ,bg-alt :foreground ,magenta)))
   `(tuareg-font-lock-governing-face ((,class :inherit bold :foreground ,fg-main)))
   `(tuareg-font-lock-infix-extension-node-face ((,class ,@(modus-vivendi-theme-syntax-foreground
                                                            magenta magenta-faint))))
   `(tuareg-font-lock-interactive-directive-face ((,class :foreground ,fg-special-cold)))
   `(tuareg-font-lock-interactive-error-face ((,class :inherit bold
                                                      ,@(modus-vivendi-theme-syntax-foreground
                                                         red red-faint))))
   `(tuareg-font-lock-interactive-output-face ((,class ,@(modus-vivendi-theme-syntax-foreground
                                                          blue-alt-other blue-alt-other-faint))))
   `(tuareg-font-lock-label-face ((,class ,@(modus-vivendi-theme-syntax-foreground
                                             cyan-alt-other cyan-alt-other-faint))))
   `(tuareg-font-lock-line-number-face ((,class :foreground ,fg-special-warm)))
   `(tuareg-font-lock-module-face ((,class ,@(modus-vivendi-theme-syntax-foreground
                                              magenta-alt magenta-alt-faint))))
   `(tuareg-font-lock-multistage-face ((,class :inherit bold :background ,bg-alt
                                               ,@(modus-vivendi-theme-syntax-foreground
                                                  blue blue-faint))))
   `(tuareg-font-lock-operator-face ((,class ,@(modus-vivendi-theme-syntax-foreground
                                                red-alt red-alt-faint))))
   `(tuareg-opam-error-face ((,class :inherit bold
                                     ,@(modus-vivendi-theme-syntax-foreground
                                        red red-faint))))
   `(tuareg-opam-pkg-variable-name-face ((,class ,@(modus-vivendi-theme-syntax-foreground
                                                    cyan cyan-faint)
                                                 :slant ,modus-theme-slant)))
;;;;; typescript
   `(typescript-jsdoc-tag ((,class :foreground ,fg-special-mild :slant ,modus-theme-slant)))
   `(typescript-jsdoc-type ((,class :foreground ,fg-special-calm :slant ,modus-theme-slant)))
   `(typescript-jsdoc-value ((,class :foreground ,fg-special-cold :slant ,modus-theme-slant)))
;;;;; undo-tree
   `(undo-tree-visualizer-active-branch-face ((,class :inherit bold :foreground ,fg-main)))
   `(undo-tree-visualizer-current-face ((,class :foreground ,blue-intense)))
   `(undo-tree-visualizer-default-face ((,class :foreground ,fg-alt)))
   `(undo-tree-visualizer-register-face ((,class :foreground ,magenta-intense)))
   `(undo-tree-visualizer-unmodified-face ((,class :foreground ,green-intense)))
;;;;; vc (vc-hooks.el)
   `(vc-conflict-state ((,class :foreground ,red-active :slant ,modus-theme-slant)))
   `(vc-edited-state ((,class :foreground ,yellow-active)))
   `(vc-locally-added-state ((,class :foreground ,cyan-active)))
   `(vc-locked-state ((,class :foreground ,blue-active)))
   `(vc-missing-state ((,class :foreground ,magenta-active :slant ,modus-theme-slant)))
   `(vc-needs-update-state ((,class :foreground ,green-active :slant ,modus-theme-slant)))
   `(vc-removed-state ((,class :foreground ,red-active)))
   `(vc-state-base ((,class :foreground ,fg-active)))
   `(vc-up-to-date-state ((,class :foreground ,fg-special-cold)))
;;;;; vdiff
   `(vdiff-addition-face ((,class :inherit modus-theme-diff-added)))
   `(vdiff-change-face ((,class :inherit modus-theme-diff-changed)))
   `(vdiff-closed-fold-face ((,class :background ,bg-diff-neutral-1 :foreground ,fg-diff-neutral-1)))
   `(vdiff-refine-added ((,class :inherit modus-theme-diff-refine-added)))
   `(vdiff-refine-changed ((,class :inherit modus-theme-diff-refine-changed)))
   `(vdiff-subtraction-face ((,class :inherit modus-theme-diff-removed)))
   `(vdiff-target-face ((,class :inherit modus-theme-intense-blue)))
;;;;; vimish-fold
   `(vimish-fold-fringe ((,class :foreground ,cyan-active)))
   `(vimish-fold-mouse-face ((,class :inherit modus-theme-intense-blue)))
   `(vimish-fold-overlay ((,class :background ,bg-alt :foreground ,fg-special-cold)))
;;;;; visible-mark
   `(visible-mark-active ((,class :background ,blue-intense-bg)))
   `(visible-mark-face1 ((,class :background ,cyan-intense-bg)))
   `(visible-mark-face2 ((,class :background ,yellow-intense-bg)))
   `(visible-mark-forward-face1 ((,class :background ,magenta-intense-bg)))
   `(visible-mark-forward-face2 ((,class :background ,green-intense-bg)))
;;;;; visual-regexp
   `(vr/group-0 ((,class :inherit modus-theme-intense-blue)))
   `(vr/group-1 ((,class :inherit modus-theme-intense-magenta)))
   `(vr/group-2 ((,class :inherit modus-theme-intense-green)))
   `(vr/match-0 ((,class :inherit modus-theme-refine-yellow)))
   `(vr/match-1 ((,class :inherit modus-theme-refine-yellow)))
   `(vr/match-separator-face ((,class :inherit (modus-theme-intense-neutral bold))))
;;;;; volatile-highlights
   `(vhl/default-face ((,class ,@(and (>= emacs-major-version 27) '(:extend t))
                               :background ,bg-alt :foreground ,blue-nuanced)))
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
   `(web-mode-block-attr-name-face ((,class ,@(modus-vivendi-theme-syntax-foreground
                                               blue blue-faint))))
   `(web-mode-block-attr-value-face ((,class ,@(modus-vivendi-theme-syntax-foreground
                                                cyan-alt-other cyan-alt-other-faint))))
   `(web-mode-block-comment-face ((,class :inherit web-mode-comment-face)))
   `(web-mode-block-control-face ((,class ,@(modus-vivendi-theme-syntax-foreground
                                             magenta-alt magenta-alt-faint)
                                          ,@(modus-vivendi-theme-bold-weight))))
   `(web-mode-block-delimiter-face ((,class :foreground ,fg-main)))
   `(web-mode-block-face ((,class :background ,bg-dim)))
   `(web-mode-block-string-face ((,class :inherit web-mode-string-face)))
   `(web-mode-bold-face ((,class :inherit bold)))
   `(web-mode-builtin-face ((,class ,@(modus-vivendi-theme-syntax-foreground
                                       magenta-alt magenta-alt-faint)
                                    ,@(modus-vivendi-theme-bold-weight))))
   `(web-mode-comment-face ((,class :foreground ,fg-alt :slant ,modus-theme-slant)))
   `(web-mode-comment-keyword-face ((,class :inherit bold :background ,bg-dim
                                            ,@(modus-vivendi-theme-syntax-foreground
                                               yellow yellow-faint))))
   `(web-mode-constant-face ((,class ,@(modus-vivendi-theme-syntax-foreground
                                        blue-alt-other blue-alt-other-faint))))
   `(web-mode-css-at-rule-face ((,class ,@(modus-vivendi-theme-syntax-foreground
                                           blue-alt-other blue-alt-other-faint))))
   `(web-mode-css-color-face ((,class ,@(modus-vivendi-theme-syntax-foreground
                                         magenta-alt magenta-alt-faint)
                                      ,@(modus-vivendi-theme-bold-weight))))
   `(web-mode-css-comment-face ((,class :inherit web-mode-comment-face)))
   `(web-mode-css-function-face ((,class ,@(modus-vivendi-theme-syntax-foreground
                                            magenta-alt magenta-alt-faint)
                                         ,@(modus-vivendi-theme-bold-weight))))
   `(web-mode-css-priority-face ((,class ,@(modus-vivendi-theme-syntax-foreground
                                            yellow-alt yellow-alt-faint)
                                         ,@(modus-vivendi-theme-bold-weight))))
   `(web-mode-css-property-name-face ((,class ,@(modus-vivendi-theme-syntax-foreground
                                                 cyan cyan-faint))))
   `(web-mode-css-pseudo-class-face ((,class ,@(modus-vivendi-theme-syntax-foreground
                                                cyan-alt-other cyan-alt-other-faint))))
   `(web-mode-css-selector-face ((,class ,@(modus-vivendi-theme-syntax-foreground
                                            magenta-alt-other magenta-alt-other-faint)
                                         ,@(modus-vivendi-theme-bold-weight))))
   `(web-mode-css-string-face ((,class :inherit web-mode-string-face)))
   `(web-mode-css-variable-face ((,class :foreground ,fg-special-warm)))
   `(web-mode-current-column-highlight-face ((,class :background ,bg-alt)))
   `(web-mode-current-element-highlight-face ((,class :inherit modus-theme-special-mild)))
   `(web-mode-doctype-face ((,class :foreground ,fg-special-cold :slant ,modus-theme-slant)))
   `(web-mode-error-face ((,class :inherit modus-theme-intense-red)))
   `(web-mode-filter-face ((,class ,@(modus-vivendi-theme-syntax-foreground
                                      magenta magenta-faint))))
   `(web-mode-folded-face ((,class :underline t)))
   `(web-mode-function-call-face ((,class ,@(modus-vivendi-theme-syntax-foreground
                                             magenta magenta-faint))))
   `(web-mode-function-name-face ((,class ,@(modus-vivendi-theme-syntax-foreground
                                             magenta magenta-faint))))
   `(web-mode-html-attr-custom-face ((,class ,@(modus-vivendi-theme-syntax-foreground
                                                cyan cyan-faint))))
   `(web-mode-html-attr-engine-face ((,class :foreground ,fg-main)))
   `(web-mode-html-attr-equal-face ((,class :foreground ,fg-main)))
   `(web-mode-html-attr-name-face ((,class ,@(modus-vivendi-theme-syntax-foreground
                                              cyan cyan-faint))))
   `(web-mode-html-attr-value-face ((,class ,@(modus-vivendi-theme-syntax-foreground
                                               blue-alt-other blue-alt-other-faint))))
   `(web-mode-html-entity-face ((,class ,@(modus-vivendi-theme-syntax-foreground
                                           yellow-alt-other yellow-alt-other-faint)
                                        :slant ,modus-theme-slant)))
   `(web-mode-html-tag-bracket-face ((,class :foreground ,fg-dim)))
   `(web-mode-html-tag-custom-face ((,class ,@(modus-vivendi-theme-syntax-foreground
                                               magenta magenta-faint))))
   `(web-mode-html-tag-face ((,class ,@(modus-vivendi-theme-syntax-foreground
                                        magenta magenta-faint))))
   `(web-mode-html-tag-namespaced-face ((,class ,@(modus-vivendi-theme-syntax-foreground
                                                   magenta-alt magenta-alt-faint)
                                                ,@(modus-vivendi-theme-bold-weight))))
   `(web-mode-html-tag-unclosed-face ((,class ,@(modus-vivendi-theme-syntax-foreground
                                                 red red-faint)
                                              :underline t)))
   `(web-mode-inlay-face ((,class :background ,bg-alt)))
   `(web-mode-italic-face ((,class :slant italic)))
   `(web-mode-javascript-comment-face ((,class :inherit web-mode-comment-face)))
   `(web-mode-javascript-string-face ((,class :inherit web-mode-string-face)))
   `(web-mode-json-comment-face ((,class :inherit web-mode-comment-face)))
   `(web-mode-json-context-face ((,class ,@(modus-vivendi-theme-syntax-foreground
                                            magenta-alt magenta-alt-faint))))
   `(web-mode-json-key-face ((,class :foreground ,blue-nuanced)))
   `(web-mode-json-string-face ((,class :inherit web-mode-string-face)))
   `(web-mode-jsx-depth-1-face ((,class :background ,blue-intense-bg :foreground ,fg-main)))
   `(web-mode-jsx-depth-2-face ((,class :background ,blue-subtle-bg :foreground ,fg-main)))
   `(web-mode-jsx-depth-3-face ((,class :background ,bg-special-cold :foreground ,fg-special-cold)))
   `(web-mode-jsx-depth-4-face ((,class :background ,bg-alt :foreground ,blue-refine-fg)))
   `(web-mode-jsx-depth-5-face ((,class :background ,bg-alt :foreground ,blue-nuanced)))
   `(web-mode-keyword-face ((,class ,@(modus-vivendi-theme-syntax-foreground
                                       magenta-alt-other magenta-alt-other-faint)
                                    ,@(modus-vivendi-theme-bold-weight))))
   `(web-mode-param-name-face ((,class ,@(modus-vivendi-theme-syntax-foreground
                                          magenta magenta-faint))))
   `(web-mode-part-comment-face ((,class :inherit web-mode-comment-face)))
   `(web-mode-part-face ((,class :inherit web-mode-block-face)))
   `(web-mode-part-string-face ((,class :inherit web-mode-string-face)))
   `(web-mode-preprocessor-face ((,class ,@(modus-vivendi-theme-syntax-foreground
                                            red-alt-other red-alt-other-faint))))
   `(web-mode-script-face ((,class :inherit web-mode-part-face)))
   `(web-mode-sql-keyword-face ((,class :inherit bold
                                        ,@(modus-vivendi-theme-syntax-foreground
                                           yellow yellow-faint))))
   `(web-mode-string-face ((,class ,@(modus-vivendi-theme-syntax-foreground
                                      blue-alt blue-alt-faint))))
   `(web-mode-style-face ((,class :inherit web-mode-part-face)))
   `(web-mode-symbol-face ((,class ,@(modus-vivendi-theme-syntax-foreground
                                      blue-alt-other blue-alt-other-faint))))
   `(web-mode-type-face ((,class ,@(modus-vivendi-theme-syntax-foreground
                                    magenta-alt magenta-alt-faint))))
   `(web-mode-underline-face ((,class :underline t)))
   `(web-mode-variable-name-face ((,class ,@(modus-vivendi-theme-syntax-foreground
                                             cyan cyan-faint))))
   `(web-mode-warning-face ((,class :inherit bold :background ,bg-alt
                                    ,@(modus-vivendi-theme-syntax-foreground
                                       yellow-alt-other yellow-alt-other-faint))))
   `(web-mode-whitespace-face ((,class :background ,bg-whitespace :foreground ,fg-whitespace)))
;;;;; wgrep
   `(wgrep-delete-face ((,class :inherit modus-theme-refine-yellow)))
   `(wgrep-done-face ((,class :inherit modus-theme-refine-blue)))
   `(wgrep-face ((,class :inherit modus-theme-refine-green)))
   `(wgrep-file-face ((,class :foreground ,fg-special-warm)))
   `(wgrep-reject-face ((,class :inherit (modus-theme-intense-red bold))))
;;;;; which-function-mode
   `(which-func ((,class :foreground ,magenta-active)))
;;;;; which-key
   `(which-key-command-description-face ((,class :foreground ,cyan)))
   `(which-key-group-description-face ((,class :foreground ,magenta-alt)))
   `(which-key-highlighted-command-face ((,class :foreground ,cyan-alt :underline t)))
   `(which-key-key-face ((,class :inherit bold :foreground ,blue-intense)))
   `(which-key-local-map-description-face ((,class :foreground ,fg-main)))
   `(which-key-note-face ((,class :background ,bg-dim :foreground ,fg-special-mild)))
   `(which-key-separator-face ((,class :foreground ,fg-alt)))
   `(which-key-special-key-face ((,class :inherit bold :foreground ,yellow-intense)))
;;;;; whitespace-mode
   `(whitespace-big-indent ((,class :inherit modus-theme-subtle-red)))
   `(whitespace-empty ((,class :inherit modus-theme-intense-magenta)))
   `(whitespace-hspace ((,class :background ,bg-whitespace :foreground ,fg-whitespace)))
   `(whitespace-indentation ((,class :background ,bg-whitespace :foreground ,fg-whitespace)))
   `(whitespace-line ((,class :inherit modus-theme-special-warm)))
   `(whitespace-newline ((,class :background ,bg-whitespace :foreground ,fg-whitespace)))
   `(whitespace-space ((,class :background ,bg-whitespace :foreground ,fg-whitespace)))
   `(whitespace-space-after-tab ((,class :inherit modus-theme-subtle-magenta)))
   `(whitespace-space-before-tab ((,class :inherit modus-theme-subtle-cyan)))
   `(whitespace-tab ((,class :background ,bg-whitespace :foreground ,fg-whitespace)))
   `(whitespace-trailing ((,class :inherit modus-theme-intense-red)))
;;;;; window-divider-mode
   `(window-divider ((,class :foreground ,fg-window-divider-inner)))
   `(window-divider-first-pixel ((,class :foreground ,fg-window-divider-outer)))
   `(window-divider-last-pixel ((,class :foreground ,fg-window-divider-outer)))
;;;;; winum
   `(winum-face ((,class ,@(modus-vivendi-theme-bold-weight) :foreground ,cyan-active)))
;;;;; writegood-mode
   `(writegood-duplicates-face ((,class :background ,bg-alt :foreground ,red-alt :underline t)))
   `(writegood-passive-voice-face ((,class :foreground ,yellow-nuanced :underline ,fg-lang-warning)))
   `(writegood-weasels-face ((,class :foreground ,red-nuanced :underline ,fg-lang-error)))
;;;;; woman
   `(woman-addition ((,class :foreground ,magenta-alt-other)))
   `(woman-bold ((,class :inherit bold :foreground ,magenta)))
   `(woman-italic ((,class :foreground ,cyan :slant italic)))
   `(woman-unknown ((,class :foreground ,yellow :slant italic)))
;;;;; xah-elisp-mode
   `(xah-elisp-at-symbol ((,class :inherit bold
                                  ,@(modus-vivendi-theme-syntax-foreground
                                     red-alt red-alt-faint))))
   `(xah-elisp-cap-variable ((,class ,@(modus-vivendi-theme-syntax-foreground
                                        red-alt-other red-alt-other-faint))))
   `(xah-elisp-command-face ((,class ,@(modus-vivendi-theme-syntax-foreground
                                        cyan-alt-other cyan-alt-other-faint))))
   `(xah-elisp-dollar-symbol ((,class ,@(modus-vivendi-theme-syntax-foreground
                                         green green-faint))))
;;;;; xref
   `(xref-file-header ((,class :inherit bold :foreground ,fg-special-cold)))
   `(xref-line-number ((,class :foreground ,fg-alt)))
   `(xref-match ((,class :inherit match)))
;;;;; yaml-mode
   `(yaml-tab-face ((,class :inherit modus-theme-intense-red)))
;;;;; yasnippet
   `(yas-field-highlight-face ((,class :background ,bg-alt :foreground ,fg-main)))
;;;;; ztree
   `(ztreep-arrow-face ((,class :foreground ,fg-inactive)))
   `(ztreep-diff-header-face ((,class :inherit bold :height 1.2 :foreground ,fg-special-cold)))
   `(ztreep-diff-header-small-face ((,class :inherit bold :foreground ,fg-special-mild)))
   `(ztreep-diff-model-add-face ((,class :foreground ,green)))
   `(ztreep-diff-model-diff-face ((,class :foreground ,red)))
   `(ztreep-diff-model-ignored-face ((,class :foreground ,fg-alt :strike-through t)))
   `(ztreep-diff-model-normal-face ((,class :foreground ,fg-alt)))
   `(ztreep-expand-sign-face ((,class :foreground ,blue)))
   `(ztreep-header-face ((,class :inherit bold :height 1.2 :foreground ,fg-special-cold)))
   `(ztreep-leaf-face ((,class :foreground ,cyan)))
   `(ztreep-node-count-children-face ((,class :foreground ,fg-special-warm)))
   `(ztreep-node-face ((,class :foreground ,fg-main))))
;;;; Emacs 27+
  (when (>= emacs-major-version 27)
    (custom-theme-set-faces
     'modus-vivendi
;;;;; line numbers (`display-line-numbers-mode' and global variant)
     ;; NOTE that this is specifically for the faces that were
     ;; introduced in Emacs 27, as the other faces are already
     ;; supported.
     `(line-number-major-tick ((,class :inherit (bold default)
                                       :background ,yellow-nuanced-bg
                                       :foreground ,yellow-nuanced)))
     `(line-number-minor-tick ((,class :inherit (bold default)
                                       :background ,bg-inactive
                                       :foreground ,fg-inactive)))
;;;;; tab-bar-mode
     `(tab-bar ((,class :background ,bg-tab-bar :foreground ,fg-main)))
     `(tab-bar-tab ((,class :inherit bold :box (:line-width 2 :color ,bg-tab-active)
                            :background ,bg-tab-active :foreground ,fg-main)))
     `(tab-bar-tab-inactive ((,class :box (:line-width 2 :color ,bg-tab-inactive)
                                     :background ,bg-tab-inactive :foreground ,fg-dim)))
;;;;; tab-line-mode
     `(tab-line ((,class :height 0.95 :background ,bg-tab-bar :foreground ,fg-main)))
     `(tab-line-close-highlight ((,class :foreground ,red)))
     `(tab-line-highlight ((,class :background ,blue-subtle-bg :foreground ,fg-dim)))
     `(tab-line-tab ((,class :inherit bold :box (:line-width 2 :color ,bg-tab-active)
                             :background ,bg-tab-active :foreground ,fg-main)))
     `(tab-line-tab-current ((,class :inherit tab-line-tab)))
     `(tab-line-tab-inactive ((,class :box (:line-width 2 :color ,bg-tab-inactive)
                                      :background ,bg-tab-inactive :foreground ,fg-dim)))))
;;;; Emacs 28+
  (when (>= emacs-major-version 28)
    (custom-theme-set-faces
     'modus-vivendi
;;;;; isearch regexp groups
     `(isearch-group-1 ((,class :inherit modus-theme-intense-blue)))
     `(isearch-group-2 ((,class :inherit modus-theme-intense-magenta)))))
;;; variables
  (custom-theme-set-variables
   'modus-vivendi
;;;; ansi-colors
   `(ansi-color-faces-vector [default bold shadow italic underline success warning error])
   `(ansi-color-names-vector [,bg-main ,red ,green ,yellow ,blue ,magenta ,cyan ,fg-main])
;;;; awesome-tray
   `(awesome-tray-mode-line-active-color ,blue)
   `(awesome-tray-mode-line-inactive-color ,bg-active)
;;;; flymake fringe indicators
   `(flymake-error-bitmap '(flymake-double-exclamation-mark modus-theme-fringe-red))
   `(flymake-warning-bitmap '(exclamation-mark modus-theme-fringe-yellow))
   `(flymake-note-bitmap '(exclamation-mark modus-theme-fringe-cyan))
;;;; ibuffer
   `(ibuffer-deletion-face 'modus-theme-mark-del)
   `(ibuffer-filter-group-name-face 'modus-theme-mark-symbol)
   `(ibuffer-marked-face 'modus-theme-mark-sel)
   `(ibuffer-title-face 'modus-theme-pseudo-header)
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
       ("TEMP" . ,red-nuanced)
       ("FIXME" . ,red-alt-other)
       ("XXX+" . ,red-alt)
       ("REVIEW" . ,cyan-alt-other)
       ("DEPRECATED" . ,blue-nuanced)))
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
   `(xterm-color-names [,bg-main ,red ,green ,yellow ,blue ,magenta ,cyan ,fg-alt])
   `(xterm-color-names-bright [,bg-alt ,red-alt ,green-alt ,yellow-alt ,blue-alt ,magenta-alt ,cyan-alt ,fg-main]))
;;; Conditional theme variables
;;;; org-src-block-faces
  ;; this is a user option to add a colour-coded background to source
  ;; blocks for various programming languages
  (when (eq modus-vivendi-theme-org-blocks 'rainbow)
    (custom-theme-set-variables
     'modus-vivendi
     `(org-src-block-faces              ; TODO this list should be expanded
       `(("emacs-lisp" modus-theme-nuanced-magenta)
         ("elisp" modus-theme-nuanced-magenta)
         ("clojure" modus-theme-nuanced-magenta)
         ("clojurescript" modus-theme-nuanced-magenta)
         ("c" modus-theme-nuanced-blue)
         ("c++" modus-theme-nuanced-blue)
         ("sh" modus-theme-nuanced-green)
         ("shell" modus-theme-nuanced-green)
         ("html" modus-theme-nuanced-yellow)
         ("xml" modus-theme-nuanced-yellow)
         ("css" modus-theme-nuanced-red)
         ("scss" modus-theme-nuanced-red)
         ("python" modus-theme-nuanced-green)
         ("ipython" modus-theme-nuanced-magenta)
         ("r" modus-theme-nuanced-cyan)
         ("yaml" modus-theme-nuanced-cyan)
         ("conf" modus-theme-nuanced-cyan)
         ("docker" modus-theme-nuanced-cyan)
         ("json" modus-theme-nuanced-cyan))))))

;;; library provides
;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'modus-vivendi)

(provide 'modus-vivendi-theme)

;;; modus-vivendi-theme.el ends here
