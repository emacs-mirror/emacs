;;; modus-themes.el --- Elegant, highly legible and customizable themes -*- lexical-binding:t -*-

;; Copyright (C) 2019-2026 Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://github.com/protesilaos/modus-themes
;; Version: 5.2.0
;; Package-Requires: ((emacs "28.1"))
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
;; values (WCAG AAA).  They are also highly customizable and can even
;; be used as the basis for other themes.  Please refer to the official
;; Info manual for further documentation (distributed with the themes,
;; or available at: <https://protesilaos.com/emacs/modus-themes>).

;;; Code:



(require 'color)
(eval-when-compile
  (require 'subr-x)
  (require 'cl-lib))

(defgroup modus-themes ()
  "User options for the Modus themes.
The Modus themes conform with the WCAG AAA standard for color
contrast between background and foreground combinations (a
minimum contrast of 7:1---the highest standard of its kind).

The Modus themes collection includes themes that are optimized
for people with red-green or blue-yellow color
deficiency (deuteranopia or tritanopia, respectively)."
  :group 'faces
  :link '(info-link "(modus-themes) Top")
  :link '(url-link :tag "Homepage" "https://protesilaos.com/emacs/modus-themes")
  :link '(url-link :tag "Sample pictures" "https://protesilaos.com/emacs/modus-themes-pictures")
  :prefix "modus-themes-"
  :tag "Modus Themes")

(defgroup modus-themes-faces ()
  "Faces defined by the Modus themes."
  :group 'modus-themes
  :link '(info-link "(modus-themes) Top")
  :link '(url-link :tag "Homepage" "https://protesilaos.com/emacs/modus-themes")
  :link '(url-link :tag "Sample pictures" "https://protesilaos.com/emacs/modus-themes-pictures")
  :prefix "modus-themes-"
  :tag "Modus Themes Faces")



;;;; Custom faces

;; NOTE 2025-10-25: I could not find the `make-obsolete' equivalent for faces.
(define-obsolete-face-alias 'modus-themes-fg-blue nil "5.0.0")
(define-obsolete-face-alias 'modus-themes-fg-blue-cooler nil "5.0.0")
(define-obsolete-face-alias 'modus-themes-fg-blue-faint nil "5.0.0")
(define-obsolete-face-alias 'modus-themes-fg-blue-intense nil "5.0.0")
(define-obsolete-face-alias 'modus-themes-fg-blue-warmer nil "5.0.0")
(define-obsolete-face-alias 'modus-themes-fg-cyan nil "5.0.0")
(define-obsolete-face-alias 'modus-themes-fg-cyan-cooler nil "5.0.0")
(define-obsolete-face-alias 'modus-themes-fg-cyan-faint nil "5.0.0")
(define-obsolete-face-alias 'modus-themes-fg-cyan-intense nil "5.0.0")
(define-obsolete-face-alias 'modus-themes-fg-cyan-warmer nil "5.0.0")
(define-obsolete-face-alias 'modus-themes-fg-green nil "5.0.0")
(define-obsolete-face-alias 'modus-themes-fg-green-cooler nil "5.0.0")
(define-obsolete-face-alias 'modus-themes-fg-green-faint nil "5.0.0")
(define-obsolete-face-alias 'modus-themes-fg-green-intense nil "5.0.0")
(define-obsolete-face-alias 'modus-themes-fg-green-warmer nil "5.0.0")
(define-obsolete-face-alias 'modus-themes-fg-magenta nil "5.0.0")
(define-obsolete-face-alias 'modus-themes-fg-magenta-cooler nil "5.0.0")
(define-obsolete-face-alias 'modus-themes-fg-magenta-faint nil "5.0.0")
(define-obsolete-face-alias 'modus-themes-fg-magenta-intense nil "5.0.0")
(define-obsolete-face-alias 'modus-themes-fg-magenta-warmer nil "5.0.0")
(define-obsolete-face-alias 'modus-themes-fg-red nil "5.0.0")
(define-obsolete-face-alias 'modus-themes-fg-red-cooler nil "5.0.0")
(define-obsolete-face-alias 'modus-themes-fg-red-faint nil "5.0.0")
(define-obsolete-face-alias 'modus-themes-fg-red-intense nil "5.0.0")
(define-obsolete-face-alias 'modus-themes-fg-red-warmer nil "5.0.0")
(define-obsolete-face-alias 'modus-themes-fg-yellow nil "5.0.0")
(define-obsolete-face-alias 'modus-themes-fg-yellow-cooler nil "5.0.0")
(define-obsolete-face-alias 'modus-themes-fg-yellow-faint nil "5.0.0")
(define-obsolete-face-alias 'modus-themes-fg-yellow-intense nil "5.0.0")
(define-obsolete-face-alias 'modus-themes-fg-yellow-warmer nil "5.0.0")
(define-obsolete-face-alias 'modus-themes-intense-blue nil "5.0.0")
(define-obsolete-face-alias 'modus-themes-intense-cyan nil "5.0.0")
(define-obsolete-face-alias 'modus-themes-intense-green nil "5.0.0")
(define-obsolete-face-alias 'modus-themes-intense-magenta nil "5.0.0")
(define-obsolete-face-alias 'modus-themes-intense-red nil "5.0.0")
(define-obsolete-face-alias 'modus-themes-intense-yellow nil "5.0.0")
(define-obsolete-face-alias 'modus-themes-key-binding nil "5.0.0")
(define-obsolete-face-alias 'modus-themes-lang-error nil "5.0.0")
(define-obsolete-face-alias 'modus-themes-lang-note nil "5.0.0")
(define-obsolete-face-alias 'modus-themes-lang-warning nil "5.0.0")
(define-obsolete-face-alias 'modus-themes-mark-alt nil "5.0.0")
(define-obsolete-face-alias 'modus-themes-mark-del nil "5.0.0")
(define-obsolete-face-alias 'modus-themes-mark-sel nil "5.0.0")
(define-obsolete-face-alias 'modus-themes-nuanced-blue nil "5.0.0")
(define-obsolete-face-alias 'modus-themes-nuanced-cyan nil "5.0.0")
(define-obsolete-face-alias 'modus-themes-nuanced-green nil "5.0.0")
(define-obsolete-face-alias 'modus-themes-nuanced-magenta nil "5.0.0")
(define-obsolete-face-alias 'modus-themes-nuanced-red nil "5.0.0")
(define-obsolete-face-alias 'modus-themes-nuanced-yellow nil "5.0.0")
(define-obsolete-face-alias 'modus-themes-prominent-error nil "5.0.0")
(define-obsolete-face-alias 'modus-themes-prominent-note nil "5.0.0")
(define-obsolete-face-alias 'modus-themes-prominent-warning nil "5.0.0")
(define-obsolete-face-alias 'modus-themes-prose-code nil "5.0.0")
(define-obsolete-face-alias 'modus-themes-prose-macro nil "5.0.0")
(define-obsolete-face-alias 'modus-themes-prose-verbatim nil "5.0.0")
(define-obsolete-face-alias 'modus-themes-search-current nil "5.0.0")
(define-obsolete-face-alias 'modus-themes-search-lazy nil "5.0.0")
(define-obsolete-face-alias 'modus-themes-search-replace nil "5.0.0")
(define-obsolete-face-alias 'modus-themes-search-rx-group-0 nil "5.0.0")
(define-obsolete-face-alias 'modus-themes-search-rx-group-1 nil "5.0.0")
(define-obsolete-face-alias 'modus-themes-search-rx-group-2 nil "5.0.0")
(define-obsolete-face-alias 'modus-themes-search-rx-group-3 nil "5.0.0")
(define-obsolete-face-alias 'modus-themes-search-static nil "5.0.0")
(define-obsolete-face-alias 'modus-themes-subtle-blue nil "5.0.0")
(define-obsolete-face-alias 'modus-themes-subtle-cyan nil "5.0.0")
(define-obsolete-face-alias 'modus-themes-subtle-green nil "5.0.0")
(define-obsolete-face-alias 'modus-themes-subtle-magenta nil "5.0.0")
(define-obsolete-face-alias 'modus-themes-subtle-red nil "5.0.0")
(define-obsolete-face-alias 'modus-themes-subtle-yellow nil "5.0.0")

;; These faces are used internally to ensure consistency between various
;; groups and to streamline the evaluation of relevant customization
;; options.

(dotimes (n 9)
  (custom-declare-face
   (intern (format "modus-themes-heading-%d" n))
   nil (format "Level %d heading." n)
   :package-version '(modus-themes . "4.0.0")
   :version "30.1"
   :group 'modus-themes-faces))

(defface modus-themes-bold nil
  "Generic face for applying a conditional bold weight.
This behaves in accordance with `modus-themes-bold-constructs'."
  :package-version '(modus-themes . "4.0.0")
  :version "30.1"
  :group 'modus-themes-faces)

(defface modus-themes-slant nil
  "Generic face for applying a conditional slant (italics).
This behaves in accordance with `modus-themes-italic-constructs'."
  :package-version '(modus-themes . "4.0.0")
  :version "30.1"
  :group 'modus-themes-faces)

(defface modus-themes-fixed-pitch nil
  "Face for `fixed-pitch' if `modus-themes-mixed-fonts' is non-nil."
  :package-version '(modus-themes . "4.0.0")
  :version "30.1"
  :group 'modus-themes-faces)

(defface modus-themes-ui-variable-pitch nil
  "Face for `variable-pitch' if `modus-themes-variable-pitch-ui' is non-nil."
  :package-version '(modus-themes . "4.0.0")
  :version "30.1"
  :group 'modus-themes-faces)

(defface modus-themes-reset-soft nil
  "Generic face to set most face properties to nil.

This is intended to be inherited by faces that should not retain
properties from their context (e.g. an overlay over an underlined
text should not be underlined as well) yet still blend in."
  :group 'modus-themes-faces)

(defface modus-themes-prompt nil
  "Generic face for command prompts."
  :group 'modus-themes-faces)

(defface modus-themes-button nil
  "Face for graphical buttons."
  :group 'modus-themes-faces)

(defface modus-themes-completion-selected nil
  "Face for current selection in completion UIs."
  :group 'modus-themes-faces)

(dotimes (n 4)
  (custom-declare-face
   (intern (format "modus-themes-completion-match-%d" n))
   nil (format "Completions match level %d." n)
   :package-version '(modus-themes . "4.0.0")
   :version "30.1"
   :group 'modus-themes-faces))



;;;; Customization variables

(make-obsolete-variable
 'modus-themes-custom-auto-reload
 "reload the theme manually for changes to take effect"
 "5.0.0")

(defcustom modus-themes-disable-other-themes t
  "Disable all other themes when loading a Modus theme.

When the value is non-nil, the commands `modus-themes-toggle' and
`modus-themes-select', as well as the `modus-themes-load-theme'
function, will disable all other themes while loading the
specified Modus theme.  This is done to ensure that Emacs does
not blend two or more themes: such blends lead to awkward results
that undermine the work of the designer.

When the value is nil, the aforementioned commands and function
will only disable other themes within the Modus collection.

This option is provided because Emacs themes are not necessarily
limited to colors/faces: they can consist of an arbitrary set of
customizations.  Users who use such customization bundles must
set this variable to a nil value."
  :group 'modus-themes
  :package-version '(modus-themes . "4.1.0")
  :version "30.1"
  :type 'boolean
  :link '(info-link "(modus-themes) Disable other themes"))

(defvaralias 'modus-themes-collection 'modus-themes-items
  "Alias of `modus-themes-items'.")

(defconst modus-themes-items
  '( modus-operandi
     modus-operandi-tinted
     modus-operandi-deuteranopia
     modus-operandi-tritanopia
     modus-vivendi
     modus-vivendi-tinted
     modus-vivendi-deuteranopia
     modus-vivendi-tritanopia)
  "Symbols of the Modus themes.")

(defcustom modus-themes-to-toggle '(modus-operandi modus-vivendi)
  "Specify two Modus themes for `modus-themes-toggle' command.
The variable `modus-themes-items' contains the symbols of all
official themes that form part of this collection.

The default value of this user option includes the original
themes: `modus-operandi' (light) and `modus-vivendi' (dark).

If the value is nil or otherwise does not specify two valid Modus
themes, the command `modus-themes-toggle' reverts to selecting a
theme from the list of available Modus themes.  In effect, it is
the same as using the command `modus-themes-select'."
  :type (let ((themes (mapcar (lambda (theme) (list 'const theme)) modus-themes-items)))
          `(choice
            (const :tag "No toggle" nil)
            (list :tag "Pick two themes to toggle between"
                  (choice :tag "Theme one of two" ,@themes)
                  (choice :tag "Theme two of two" ,@themes))))
  :package-version '(modus-themes . "4.0.0")
  :version "30.1"
  :group 'modus-themes)

(defcustom modus-themes-to-rotate nil
  "List of Modus themes to rotate, per `modus-themes-rotate'.
If the value is nil, then rotation applies to all themes returned by
`modus-themes-get-themes'."
  :type `(repeat
          (choice :tag "A theme among the `modus-themes-items'"
                  ,@(mapcar
                     (lambda (theme)
                       (list 'const theme))
                     (if (fboundp 'modus-themes-get-themes)
                         (modus-themes-get-themes)
                       modus-themes-items))))
  :package-version '(modus-themes . "5.0.0")
  :version "31.1"
  :group 'modus-themes)

(defvaralias 'modus-themes-post-load-hook 'modus-themes-after-load-theme-hook
  "Alias for `modus-themes-after-load-theme-hook'.")

(defcustom modus-themes-after-load-theme-hook nil
  "Hook that runs after loading a Modus theme.
This is used by the commands `modus-themes-toggle',
`modus-themes-rotate', `modus-themes-select', as well as the function
`modus-themes-load-theme'."
  :type 'hook
  :package-version '(modus-themes . "4.0.0")
  :version "30.1"
  :group 'modus-themes)

(defvaralias 'modus-themes-slanted-constructs 'modus-themes-italic-constructs)

(defcustom modus-themes-italic-constructs nil
  "Use italic font forms in more code constructs."
  :group 'modus-themes
  :package-version '(modus-themes . "1.5.0")
  :version "28.1"
  :type 'boolean
  :link '(info-link "(modus-themes) Italic constructs"))

(defcustom modus-themes-bold-constructs nil
  "Use bold text in more code constructs."
  :group 'modus-themes
  :package-version '(modus-themes . "1.0.0")
  :version "28.1"
  :type 'boolean
  :link '(info-link "(modus-themes) Bold constructs"))

(defcustom modus-themes-variable-pitch-ui nil
  "Use proportional fonts (variable-pitch) in UI elements.
This includes the mode line, header line, tab bar, and tab line."
  :group 'modus-themes
  :package-version '(modus-themes . "1.1.0")
  :version "28.1"
  :type 'boolean
  :link '(info-link "(modus-themes) UI typeface"))

(defcustom modus-themes-mixed-fonts nil
  "Non-nil to enable inheritance from `fixed-pitch' in some faces.

This is done to allow spacing-sensitive constructs, such as Org
tables and code blocks, to remain monospaced when users opt for
something like the command `variable-pitch-mode'.

Users may need to explicitly configure the font family of
`fixed-pitch' in order to get a consistent experience with their
typography (also check the `fontaine' package on GNU ELPA (by
Protesilaos))."
  :group 'modus-themes
  :package-version '(modus-themes . "1.7.0")
  :version "29.1"
  :type 'boolean
  :link '(info-link "(modus-themes) Mixed fonts"))

(defconst modus-themes--weight-widget
  '(choice :tag "Font weight (must be supported by the typeface)"
           (const :tag "Unspecified (use whatever the default is)" nil)
           (const :tag "Thin" thin)
           (const :tag "Ultra-light" ultralight)
           (const :tag "Extra-light" extralight)
           (const :tag "Light" light)
           (const :tag "Semi-light" semilight)
           (const :tag "Regular" regular)
           (const :tag "Medium" medium)
           (const :tag "Semi-bold" semibold)
           (const :tag "Bold" bold)
           (const :tag "Extra-bold" extrabold)
           (const :tag "Ultra-bold" ultrabold))
  "List of supported font weights used by `defcustom' forms.")

(defconst modus-themes--headings-widget
  `(set :tag "Properties" :greedy t
        (const :tag "Proportionately spaced font (variable-pitch)" variable-pitch)
        ,modus-themes--weight-widget
        (radio :tag "Height"
               (float :tag "Floating point to adjust height by")
               (cons :tag "Cons cell of `(height . FLOAT)'"
                     (const :tag "The `height' key (constant)" height)
                     (float :tag "Floating point"))))
  "Refer to the doc string of `modus-themes-headings'.
This is a helper variable intended for internal use.")

(defcustom modus-themes-headings nil
  "Heading styles with optional list of values per heading level.

This is an alist that accepts a (KEY . LIST-OF-VALUES)
combination.  The KEY is either a number, representing the
heading's level (0-8) or t, which pertains to the fallback style.
The named keys `agenda-date' and `agenda-structure' apply to the
Org agenda.

Level 0 is used for what counts as a document title or
equivalent, such as the #+title construct we find in Org files.
Levels 1-8 are regular headings.

The LIST-OF-VALUES covers symbols that refer to properties, as
described below.  Here is a complete sample with various
stylistic combinations, followed by a presentation of all
available properties:

    (setq modus-themes-headings
          (quote ((1 . (variable-pitch 1.5))
                  (2 . (1.3))
                  (agenda-date . (1.3))
                  (agenda-structure . (variable-pitch light 1.8))
                  (t . (1.1)))))

By default (a nil value for this variable), all headings have a
bold typographic weight, use a desaturated text color, have a
font family that is the same as the `default' face (typically
monospaced), and a height that is equal to the `default' face's
height.

A `variable-pitch' property changes the font family of the
heading to that of the `variable-pitch' face (normally a
proportionately spaced typeface).

The symbol of a weight attribute adjusts the font of the heading
accordingly, such as `light', `semibold', etc.  Valid symbols are
defined in the variable `modus-themes-weights'.  The absence of a
weight means that bold will be used by virtue of inheriting the
`bold' face (check the manual for tweaking bold and italic
faces).

A number, expressed as a floating point (e.g. 1.5), adjusts the
height of the heading to that many times the base font size.  The
default height is the same as 1.0, though it need not be
explicitly stated.  Instead of a floating point, an acceptable
value can be in the form of a cons cell like (height . FLOAT)
or (height FLOAT), where FLOAT is the given number.

Combinations of any of those properties are expressed as a list,
like in these examples:

    (semibold)
    (variable-pitch semibold 1.3)
    (variable-pitch semibold (height 1.3)) ; same as above
    (variable-pitch semibold (height . 1.3)) ; same as above

The order in which the properties are set is not significant.

In user configuration files the form may look like this:

    (setq modus-themes-headings
          (quote ((1 . (variable-pitch 1.5))
                  (2 . (1.3))
                  (agenda-date . (1.3))
                  (agenda-structure . (variable-pitch light 1.8))
                  (t . (1.1)))))

When defining the styles per heading level, it is possible to
pass a non-nil value (t) instead of a list of properties.  This
will retain the original aesthetic for that level.  For example:

    (setq modus-themes-headings
          (quote ((1 . t)           ; keep the default style
                  (2 . (semibold 1.2))
                  (t . (variable-pitch))))) ; style for all other headings

    (setq modus-themes-headings
          (quote ((1 . (variable-pitch extrabold 1.5))
                  (2 . (semibold))
                  (t . t)))) ; default style for all other levels

Note that the text color of headings, of their background, and
overline can all be set via the overrides.  It is possible to
have any color combination for any heading level (something that
could not be done in older versions of the themes).

Read Info node `(modus-themes) Option for palette overrides' as
well as Info node `(modus-themes) Make headings more or less
colorful'.  Else check `modus-themes-common-palette-overrides'
and related user options."
  :group 'modus-themes
  :package-version '(modus-themes . "4.0.0")
  :version "30.1"
  :type `(alist
          :options ,(mapcar (lambda (el)
                              (list el modus-themes--headings-widget))
                            '(0 1 2 3 4 5 6 7 8 t agenda-date agenda-structure))
          :key-type symbol
          :value-type ,modus-themes--headings-widget)
  :link '(info-link "(modus-themes) Heading styles"))

(defcustom modus-themes-completions nil
  "Control the style of completion user interfaces.

This affects Company, Corfu, Flx, Icomplete/Fido, Ido, Ivy,
Orderless, Vertico, and the standard *Completions* buffer.  The
value is an alist of expressions, each of which takes the form
of (KEY . LIST-OF-PROPERTIES).  KEY is a symbol, while PROPERTIES
is a list.  Here is a sample, followed by a description of the
particularities:

    (setq modus-themes-completions
          (quote ((matches . (extrabold underline))
                  (selection . (semibold italic)))))

The `matches' key refers to the highlighted characters that
correspond to the user's input.  When its properties are nil or
an empty list, matching characters in the user interface will
have a bold weight and a colored foreground.  The list of
properties may include any of the following symbols regardless of
the order they may appear in:

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

- `underline' to draw a line below the characters;

- `italic' to use a slanted font (italic or oblique forms);

- The symbol of a font weight attribute such as `light',
  `semibold', et cetera.  Valid symbols are defined in the
  variable `modus-themes-weights'.  The absence of a weight means
  that bold will be used.

Apart from specifying each key separately, a catch-all list is
accepted.  This is only useful when the desired aesthetic is the
same across all keys that are not explicitly referenced.  For
example, this:

    (setq modus-themes-completions
          (quote ((t . (extrabold underline)))))

Is the same as:

    (setq modus-themes-completions
          (quote ((matches . (extrabold underline))
                  (selection . (extrabold underline)))))"
  :group 'modus-themes
  :package-version '(modus-themes . "4.0.0")
  :version "30.1"
  :type `(set
          (cons :tag "Matches"
                (const matches)
                (set :tag "Style of matches" :greedy t
                     ,modus-themes--weight-widget
                     (const :tag "Italic font (oblique or slanted forms)" italic)
                     (const :tag "Underline" underline)))
          (cons :tag "Selection"
                (const selection)
                (set :tag "Style of selection" :greedy t
                     ,modus-themes--weight-widget
                     (const :tag "Italic font (oblique or slanted forms)" italic)
                     (const :tag "Underline" underline)))
          (cons :tag "Fallback for both matches and selection"
                (const t)
                (set :tag "Style of both matches and selection" :greedy t
                     ,modus-themes--weight-widget
                     (const :tag "Italic font (oblique or slanted forms)" italic)
                     (const :tag "Underline" underline))))
  :link '(info-link "(modus-themes) Completion UIs"))

(defcustom modus-themes-prompts nil
  "Use subtle or intense styles for minibuffer and REPL prompts.

The value is a list of properties, each designated by a symbol.
The default (a nil value or an empty list) means to only use a
subtle colored foreground color.

The `italic' property adds a slant to the font's forms (italic or
oblique forms, depending on the typeface).

The symbol of a font weight attribute such as `light', `semibold',
et cetera, adds the given weight to links.  Valid symbols are
defined in the variable `modus-themes-weights'.  The absence of a
weight means that the one of the underlying text will be used.

Combinations of any of those properties are expressed as a list,
like in these examples:

    (bold italic)
    (italic semibold)

The order in which the properties are set is not significant.

In user configuration files the form may look like this:

    (setq modus-themes-prompts (quote (extrabold italic)))"
  :group 'modus-themes
  :package-version '(modus-themes . "4.0.0")
  :version "30.1"
  :type `(set :tag "Properties" :greedy t
              (const :tag "Italic font slant" italic)
              ,modus-themes--weight-widget)
  :link '(info-link "(modus-themes) Command prompts"))


(defcustom modus-themes-common-palette-user nil
  "Common user-defined colors to extend all the themes' palettes.
This is meant to extend the palette of the active Modus theme with
custom named colors and/or semantic palette mappings.  Those may then be
used in combination with palette overrides (see
`modus-themes-common-palette-overrides')."
  :group 'modus-themes
  :package-version '(modus-themes . "4.5.0")
  :type '(repeat (list symbol (choice symbol string)))
  :link '(info-link "(modus-themes) Extend the palette for use with overrides"))

(defcustom modus-themes-common-palette-overrides nil
  "Set palette overrides for all the Modus themes.

Mirror the elements of a theme's palette, overriding their value.
The palette variables are named THEME-NAME-palette, while
individual theme overrides are THEME-NAME-palette-overrides.  The
THEME-NAME is one of the symbols in `modus-themes-items'.  For
example:

- `modus-operandi-palette'
- `modus-operandi-palette-overrides'

Individual theme overrides take precedence over these common
overrides.

The idea of common overrides is to change semantic color
mappings, such as to make the cursor red.  Whereas theme-specific
overrides can also be used to change the value of a named color,
such as what hexadecimal RGB value the red-warmer symbol
represents."
  :group 'modus-themes
  :package-version '(modus-themes . "4.0.0")
  :version "30.1"
  :type '(repeat (list symbol (choice symbol string)))
  :link '(info-link "(modus-themes) Palette overrides"))



;;;; Theme palettes

(define-obsolete-variable-alias 'modus-operandi-palette 'modus-themes-operandi-palette "5.0.0")
(define-obsolete-variable-alias 'modus-operandi-tinted-palette 'modus-themes-operandi-tinted-palette "5.0.0")
(define-obsolete-variable-alias 'modus-operandi-deuteranopia-palette 'modus-themes-operandi-deuteranopia-palette "5.0.0")
(define-obsolete-variable-alias 'modus-operandi-tritanopia-palette 'modus-themes-operandi-tritanopia-palette "5.0.0")
(define-obsolete-variable-alias 'modus-vivendi-palette 'modus-themes-vivendi-palette "5.0.0")
(define-obsolete-variable-alias 'modus-vivendi-tinted-palette 'modus-themes-vivendi-tinted-palette "5.0.0")
(define-obsolete-variable-alias 'modus-vivendi-deuteranopia-palette 'modus-themes-vivendi-deuteranopia-palette "5.0.0")
(define-obsolete-variable-alias 'modus-vivendi-tritanopia-palette 'modus-themes-vivendi-tritanopia-palette "5.0.0")

(defconst modus-themes-common-palette-mappings
  '((fringe bg-dim)

    ;; Button mappings

    (fg-button-active fg-main)
    (fg-button-inactive fg-dim)
    (bg-button-active bg-active)
    (bg-button-inactive bg-dim)

    ;; Code mappings

    (bracket fg-main)
    (delimiter fg-main)
    (docmarkup magenta-faint)
    (number fg-main)
    (operator fg-main)
    (punctuation fg-main)

    ;; Completion mappings

    (bg-completion-match-0 unspecified)
    (bg-completion-match-1 unspecified)
    (bg-completion-match-2 unspecified)
    (bg-completion-match-3 unspecified)

    ;; Date mappings

    (date-now fg-main)

    ;; Line number mappings

    (fg-line-number-inactive fg-dim)
    (fg-line-number-active fg-main)
    (bg-line-number-inactive bg-dim)
    (bg-line-number-active bg-active)

    ;; Link mappings

    (bg-link unspecified)
    (bg-link-symbolic unspecified)
    (bg-link-visited unspecified)

    ;; Paren match

    (fg-paren-match fg-main)
    (underline-paren-match unspecified)

    ;; Prompt mappings

    (bg-prompt unspecified)

    ;; Prose mappings

    (bg-prose-block-delimiter bg-dim)
    (fg-prose-block-delimiter fg-dim)
    (bg-prose-block-contents bg-dim)
    (bg-prose-code unspecified)
    (bg-prose-macro unspecified)
    (bg-prose-verbatim unspecified)

    ;; Search mappings
    (fg-search-current fg-main)
    (fg-search-lazy fg-main)
    (fg-search-static fg-main)
    (fg-search-replace fg-main)

    (fg-search-rx-group-0 fg-main)
    (fg-search-rx-group-1 fg-main)
    (fg-search-rx-group-2 fg-main)
    (fg-search-rx-group-3 fg-main)

    ;; Space mappings

    (bg-space unspecified)
    (fg-space border)
    (bg-space-err bg-red-intense)

    ;; Terminal mappings

    (bg-term-black           "#000000")
    (fg-term-black           "#000000")
    (bg-term-black-bright    "#595959")
    (fg-term-black-bright    "#595959")

    (bg-term-red             red)
    (fg-term-red             red)
    (bg-term-red-bright      red-warmer)
    (fg-term-red-bright      red-warmer)

    (bg-term-green           green)
    (fg-term-green           green)
    (bg-term-green-bright    green-cooler)
    (fg-term-green-bright    green-cooler)

    (bg-term-yellow          yellow)
    (fg-term-yellow          yellow)
    (bg-term-yellow-bright   yellow-warmer)
    (fg-term-yellow-bright   yellow-warmer)

    (bg-term-blue            blue)
    (fg-term-blue            blue)
    (bg-term-blue-bright     blue-warmer)
    (fg-term-blue-bright     blue-warmer)

    (bg-term-magenta         magenta)
    (fg-term-magenta         magenta)
    (bg-term-magenta-bright  magenta-cooler)
    (fg-term-magenta-bright  magenta-cooler)

    (bg-term-cyan            cyan)
    (fg-term-cyan            cyan)

    (bg-term-cyan-bright     cyan-cooler)
    (fg-term-cyan-bright     cyan-cooler)

    (bg-term-white           "#a6a6a6")
    (fg-term-white           "#a6a6a6")
    (bg-term-white-bright    "#ffffff")
    (fg-term-white-bright    "#ffffff")

    ;; Heading mappings

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
    (overline-heading-8 unspecified)))

;; FIXME 2025-10-29: Can we not autoload these palettes? I would
;; rather have them in their respective files. I tried to declare them
;; with `defcustom'/`defvar' and do `custom-autoload' plus `provide'
;; the feature of the file. No matter how I do it and where I call
;; `custom-autoload' from, it does not have the desired effect: the
;; palette is always void if the feature is not explicitly `require'd.

(defconst modus-themes-operandi-palette
  (append
   '(
     ;; Basic values

     (bg-main          "#ffffff")
     (bg-dim           "#f2f2f2")
     (fg-main          "#000000")
     (fg-dim           "#595959")
     (fg-alt           "#193668")
     (bg-active        "#c4c4c4")
     (bg-inactive      "#e0e0e0")
     (border           "#9f9f9f")

     ;; Common accent foregrounds

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

     ;; Uncommon accent foregrounds

     (rust       "#8a290f")
     (gold       "#6c501c")
     (olive      "#4c6000")
     (slate      "#2f3f83")
     (indigo     "#4a3a8a")
     (maroon     "#731c52")
     (pink       "#7b435c")

     ;; Common accent backgrounds

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

     (bg-red-nuanced     "#ffe8e8")
     (bg-green-nuanced   "#e0f6e0")
     (bg-yellow-nuanced  "#f8f0d0")
     (bg-blue-nuanced    "#ecedff")
     (bg-magenta-nuanced "#f8e6f5")
     (bg-cyan-nuanced    "#e0f2fa")

     ;; Uncommon accent background and foreground pairs

     (bg-clay     "#f1c8b5")
     (fg-clay     "#63192a")

     (bg-ochre    "#f0e3c0")
     (fg-ochre    "#573a30")

     (bg-lavender "#dfcdfa")
     (fg-lavender "#443379")

     (bg-sage     "#c0e7d4")
     (fg-sage     "#124b41")

     ;; Graphs

     (bg-graph-red-0     "#ef7969")
     (bg-graph-red-1     "#ffaab4")
     (bg-graph-green-0   "#45c050")
     (bg-graph-green-1   "#75ef30")
     (bg-graph-yellow-0  "#ffcf00")
     (bg-graph-yellow-1  "#f9ff00")
     (bg-graph-blue-0    "#7f90ff")
     (bg-graph-blue-1    "#a6c0ff")
     (bg-graph-magenta-0 "#e07fff")
     (bg-graph-magenta-1 "#fad0ff")
     (bg-graph-cyan-0    "#70d3f0")
     (bg-graph-cyan-1    "#afefff")

     ;; Special purpose

     (bg-completion       "#c0deff")
     (bg-hover            "#b2e4dc")
     (bg-hover-secondary  "#f5d0a0")
     (bg-hl-line          "#dae5ec")
     (bg-region           "#bdbdbd")
     (fg-region           "#000000")

     (bg-mode-line-active        "#c8c8c8")
     (fg-mode-line-active        "#000000")
     (border-mode-line-active    "#5a5a5a")
     (bg-mode-line-inactive      "#e6e6e6")
     (fg-mode-line-inactive      "#585858")
     (border-mode-line-inactive  "#a3a3a3")

     (modeline-err     "#7f0000")
     (modeline-warning "#5f0070")
     (modeline-info    "#002580")

     (bg-tab-bar      "#dfdfdf")
     (bg-tab-current  "#ffffff")
     (bg-tab-other    "#c2c2c2")

     ;; Diffs

     (bg-added           "#c1f2d1")
     (bg-added-faint     "#d8f8e1")
     (bg-added-refine    "#aee5be")
     (bg-added-fringe    "#6cc06c")
     (fg-added           "#005000")
     (fg-added-intense   "#006700")

     (bg-changed         "#ffdfa9")
     (bg-changed-faint   "#ffefbf")
     (bg-changed-refine  "#fac090")
     (bg-changed-fringe  "#d7c20a")
     (fg-changed         "#553d00")
     (fg-changed-intense "#655000")

     (bg-removed         "#ffd8d5")
     (bg-removed-faint   "#ffe9e9")
     (bg-removed-refine  "#f3b5af")
     (bg-removed-fringe  "#d84a4f")
     (fg-removed         "#8f1313")
     (fg-removed-intense "#aa2222")

     (bg-diff-context    "#f3f3f3")

     ;; Paren match

     (bg-paren-match        "#5fcfff")
     (bg-paren-expression   "#efd3f5")
     (underline-paren-match unspecified)

     ;; General mappings

     (cursor fg-main)
     (keybind blue-cooler)
     (name magenta)
     (identifier yellow-cooler)

     (err red)
     (warning yellow-warmer)
     (info cyan-cooler)

     (underline-err red-intense)
     (underline-warning yellow-intense)
     (underline-note cyan-intense)

     (bg-prominent-err bg-red-intense)
     (fg-prominent-err fg-main)
     (bg-prominent-warning bg-yellow-intense)
     (fg-prominent-warning fg-main)
     (bg-prominent-note bg-cyan-intense)
     (fg-prominent-note fg-main)

     (bg-active-argument bg-yellow-nuanced)
     (fg-active-argument yellow-warmer)
     (bg-active-value bg-cyan-nuanced)
     (fg-active-value cyan-warmer)

     ;; Code mappings

     (builtin magenta-warmer)
     (comment fg-dim)
     (constant blue-cooler)
     (docstring green-faint)
     (fnname magenta)
     (fnname-call pink)
     (keyword magenta-cooler)
     (preprocessor red-cooler)
     (property cyan)
     (rx-backslash magenta)
     (rx-construct green-cooler)
     (string blue-warmer)
     (type cyan-cooler)
     (variable cyan)
     (variable-use slate)

     ;; Accent mappings

     (accent-0 blue)
     (accent-1 magenta-warmer)
     (accent-2 cyan)
     (accent-3 red)

     ;; Completion mappings

     (fg-completion-match-0 blue)
     (fg-completion-match-1 magenta-warmer)
     (fg-completion-match-2 cyan)
     (fg-completion-match-3 red)

     ;; Date mappings

     (date-common cyan)
     (date-deadline red-cooler)
     (date-deadline-subtle red-faint)
     (date-event fg-alt)
     (date-holiday red)
     (date-holiday-other blue)
     (date-range fg-alt)
     (date-scheduled yellow)
     (date-scheduled-subtle yellow-faint)
     (date-weekday cyan)
     (date-weekend magenta)

     ;; Link mappings

     (fg-link blue-warmer)
     (underline-link blue-warmer)
     (fg-link-symbolic cyan)
     (underline-link-symbolic cyan)
     (fg-link-visited magenta)
     (underline-link-visited magenta)

     ;; Mail mappings

     (mail-cite-0 blue-faint)
     (mail-cite-1 yellow-warmer)
     (mail-cite-2 cyan-cooler)
     (mail-cite-3 red-cooler)
     (mail-part cyan)
     (mail-recipient magenta-cooler)
     (mail-subject magenta-warmer)
     (mail-other magenta-faint)

     ;; Mark mappings

     (bg-mark-delete bg-red-subtle)
     (fg-mark-delete red)
     (bg-mark-select bg-cyan-subtle)
     (fg-mark-select cyan)
     (bg-mark-other bg-yellow-subtle)
     (fg-mark-other yellow)

     ;; Prompt mappings

     (fg-prompt cyan-cooler)

     ;; Prose mappings


     (fg-prose-code cyan-cooler)
     (fg-prose-macro magenta-cooler)
     (fg-prose-verbatim magenta-warmer)
     (prose-done green)
     (prose-todo red)
     (prose-metadata fg-dim)
     (prose-metadata-value fg-alt)
     (prose-table fg-alt)
     (prose-table-formula magenta-warmer)
     (prose-tag magenta-faint)

     ;; Rainbow mappings

     (rainbow-0 fg-main)
     (rainbow-1 magenta-intense)
     (rainbow-2 cyan-intense)
     (rainbow-3 red-warmer)
     (rainbow-4 yellow-intense)
     (rainbow-5 magenta-cooler)
     (rainbow-6 green-intense)
     (rainbow-7 blue-warmer)
     (rainbow-8 magenta-warmer)

     ;; Search mappings

     (bg-search-current bg-yellow-intense)
     (bg-search-lazy bg-cyan-intense)
     (bg-search-static bg-magenta-subtle)
     (bg-search-replace bg-red-intense)

     (bg-search-rx-group-0 bg-blue-intense)
     (bg-search-rx-group-1 bg-green-intense)
     (bg-search-rx-group-2 bg-red-subtle)
     (bg-search-rx-group-3 bg-magenta-subtle)

     ;; Heading mappings

     (fg-heading-0 cyan-cooler)
     (fg-heading-1 fg-main)
     (fg-heading-2 yellow-faint)
     (fg-heading-3 fg-alt)
     (fg-heading-4 magenta)
     (fg-heading-5 green-faint)
     (fg-heading-6 red-faint)
     (fg-heading-7 cyan-warmer)
     (fg-heading-8 fg-dim))
   modus-themes-common-palette-mappings)
  "The entire palette of the `modus-operandi' theme.

Named colors have the form (COLOR-NAME HEX-VALUE) with the former
as a symbol and the latter as a string.

Semantic color mappings have the form (MAPPING-NAME COLOR-NAME)
with both as symbols.  The latter is a named color that already
exists in the palette and is associated with a HEX-VALUE.")

(defconst modus-themes-operandi-tinted-palette
  (append
   '(
     ;; Basic values

     (bg-main          "#fbf7f0")
     (bg-dim           "#efe9dd")
     (fg-main          "#000000")
     (fg-dim           "#595959")
     (fg-alt           "#193668")
     (bg-active        "#c9b9b0")
     (bg-inactive      "#dfd5cf")
     (border           "#9f9690")

     ;; Common accent foregrounds

     (red             "#a60000")
     (red-warmer      "#972500")
     (red-cooler      "#a0132f")
     (red-faint       "#7f0000")
     (red-intense     "#d00000")
     (green           "#006300")
     (green-warmer    "#306010")
     (green-cooler    "#00603f")
     (green-faint     "#2a5045")
     (green-intense   "#008900")
     (yellow          "#6d5000")
     (yellow-warmer   "#894000")
     (yellow-cooler   "#602938")
     (yellow-faint    "#574316")
     (yellow-intense  "#808000")
     (blue            "#0031a9")
     (blue-warmer     "#3546c2")
     (blue-cooler     "#0000b0")
     (blue-faint      "#003497")
     (blue-intense    "#0000ff")
     (magenta         "#721045")
     (magenta-warmer  "#8f0075")
     (magenta-cooler  "#531ab6")
     (magenta-faint   "#7c318f")
     (magenta-intense "#dd22dd")
     (cyan            "#00598b")
     (cyan-warmer     "#32548f")
     (cyan-cooler     "#005f5f")
     (cyan-faint      "#304463")
     (cyan-intense    "#008899")

     ;; Uncommon accent foregrounds

     (rust       "#8a290f")
     (gold       "#6c501c")
     (olive      "#425d00")
     (slate      "#2f3f83")
     (indigo     "#4a3a8a")
     (maroon     "#731c52")
     (pink       "#7b435c")

     ;; Common accent backgrounds

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

     (bg-red-nuanced     "#ffe8e8")
     (bg-green-nuanced   "#e0f6e0")
     (bg-yellow-nuanced  "#f8f0d0")
     (bg-blue-nuanced    "#ecedff")
     (bg-magenta-nuanced "#f8e6f5")
     (bg-cyan-nuanced    "#e0f2fa")

     ;; Uncommon accent background and foreground pairs

     (bg-clay     "#f1c8b5")
     (fg-clay     "#63192a")

     (bg-ochre    "#f0e3c0")
     (fg-ochre    "#573a30")

     (bg-lavender "#dfcdfa")
     (fg-lavender "#443379")

     (bg-sage     "#c0e7d4")
     (fg-sage     "#124b41")

     ;; Graphs

     (bg-graph-red-0     "#ef7969")
     (bg-graph-red-1     "#ffaab4")
     (bg-graph-green-0   "#45c050")
     (bg-graph-green-1   "#75ef30")
     (bg-graph-yellow-0  "#ffcf00")
     (bg-graph-yellow-1  "#f9ff00")
     (bg-graph-blue-0    "#7f90ff")
     (bg-graph-blue-1    "#a6c0ff")
     (bg-graph-magenta-0 "#e07fff")
     (bg-graph-magenta-1 "#fad0ff")
     (bg-graph-cyan-0    "#70d3f0")
     (bg-graph-cyan-1    "#afefff")

     ;; Special purpose

     (bg-completion       "#f0c1cf")
     (bg-hover            "#b2e4dc")
     (bg-hover-secondary  "#dfe09f")
     (bg-hl-line          "#f1d5d0")
     (bg-region           "#c2bcb5")
     (fg-region           "#000000")

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

     ;; Diffs

     (bg-added           "#c3ebc1")
     (bg-added-faint     "#dcf8d1")
     (bg-added-refine    "#acd6a5")
     (bg-added-fringe    "#6cc06c")
     (fg-added           "#005000")
     (fg-added-intense   "#006700")

     (bg-changed         "#ffdfa9")
     (bg-changed-faint   "#ffefbf")
     (bg-changed-refine  "#fac090")
     (bg-changed-fringe  "#c0b200")
     (fg-changed         "#553d00")
     (fg-changed-intense "#655000")

     (bg-removed         "#f4d0cf")
     (bg-removed-faint   "#ffe9e5")
     (bg-removed-refine  "#f3b5a7")
     (bg-removed-fringe  "#d84a4f")
     (fg-removed         "#8f1313")
     (fg-removed-intense "#aa2222")

     (bg-diff-context    "#efe9df")

     ;; Paren match

     (bg-paren-match        "#7fdfcf")
     (bg-paren-expression   "#efd3f5")

     ;; General mappings

     (cursor red-intense)
     (keybind red)
     (name magenta)
     (identifier yellow-faint)

     (err red)
     (warning yellow)
     (info green)

     (underline-err red-intense)
     (underline-warning yellow-intense)
     (underline-note cyan-intense)

     (bg-prominent-err bg-red-intense)
     (fg-prominent-err fg-main)
     (bg-prominent-warning bg-yellow-intense)
     (fg-prominent-warning fg-main)
     (bg-prominent-note bg-cyan-intense)
     (fg-prominent-note fg-main)

     (bg-active-argument bg-yellow-nuanced)
     (fg-active-argument yellow-warmer)
     (bg-active-value bg-cyan-nuanced)
     (fg-active-value cyan-warmer)

     ;; Code mappings

     (builtin magenta)
     (comment red-faint)
     (constant magenta-cooler)
     (docstring cyan-faint)
     (fnname yellow-cooler)
     (fnname-call pink)
     (keyword blue)
     (preprocessor yellow-warmer)
     (property green-cooler)
     (rx-backslash magenta-warmer)
     (rx-construct magenta-cooler)
     (string cyan)
     (type green-warmer)
     (variable green-cooler)
     (variable-use green-faint)

     ;; Accent mappings

     (accent-0 red-cooler)
     (accent-1 cyan)
     (accent-2 magenta-cooler)
     (accent-3 yellow-warmer)

     ;; Completion mappings

     (fg-completion-match-0 blue)
     (fg-completion-match-1 magenta-warmer)
     (fg-completion-match-2 cyan)
     (fg-completion-match-3 red)

     ;; Date mappings

     (date-common cyan)
     (date-deadline red-cooler)
     (date-deadline-subtle red-faint)
     (date-event fg-alt)
     (date-holiday red)
     (date-holiday-other blue)
     (date-range fg-alt)
     (date-scheduled yellow)
     (date-scheduled-subtle yellow-faint)
     (date-weekday cyan)
     (date-weekend magenta)

     ;; Link mappings

     (fg-link blue-warmer)
     (underline-link blue-warmer)
     (fg-link-symbolic cyan)
     (underline-link-symbolic cyan)
     (fg-link-visited magenta)
     (underline-link-visited magenta)

     ;; Mail mappings

     (mail-cite-0 cyan)
     (mail-cite-1 yellow)
     (mail-cite-2 green-warmer)
     (mail-cite-3 red-cooler)
     (mail-part green-cooler)
     (mail-recipient blue-warmer)
     (mail-subject magenta-warmer)
     (mail-other magenta)

     ;; Mark mappings

     (bg-mark-delete bg-red-subtle)
     (fg-mark-delete red)
     (bg-mark-select bg-cyan-subtle)
     (fg-mark-select cyan)
     (bg-mark-other bg-yellow-subtle)
     (fg-mark-other yellow)

     ;; Prompt mappings

     (fg-prompt green-cooler)

     ;; Prose mappings

     (fg-prose-code green-cooler)
     (fg-prose-macro blue-cooler)
     (fg-prose-verbatim yellow-warmer)
     (prose-done green)
     (prose-todo red)
     (prose-metadata fg-dim)
     (prose-metadata-value fg-alt)
     (prose-table fg-alt)
     (prose-table-formula magenta-warmer)
     (prose-tag magenta-faint)

     ;; Rainbow mappings

     (rainbow-0 fg-main)
     (rainbow-1 magenta-intense)
     (rainbow-2 cyan-intense)
     (rainbow-3 red-warmer)
     (rainbow-4 yellow-intense)
     (rainbow-5 magenta-cooler)
     (rainbow-6 green-intense)
     (rainbow-7 blue-warmer)
     (rainbow-8 magenta-warmer)

     ;; Search mappings

     (bg-search-current bg-yellow-intense)
     (bg-search-lazy bg-cyan-intense)
     (bg-search-static bg-magenta-subtle)
     (bg-search-replace bg-red-intense)

     (bg-search-rx-group-0 bg-blue-intense)
     (bg-search-rx-group-1 bg-green-intense)
     (bg-search-rx-group-2 bg-red-subtle)
     (bg-search-rx-group-3 bg-magenta-subtle)

     ;; Heading mappings

     (fg-heading-0 green-cooler)
     (fg-heading-1 fg-main)
     (fg-heading-2 yellow-faint)
     (fg-heading-3 fg-alt)
     (fg-heading-4 magenta)
     (fg-heading-5 green-faint)
     (fg-heading-6 red-faint)
     (fg-heading-7 cyan-warmer)
     (fg-heading-8 fg-dim))
   modus-themes-common-palette-mappings)
  "The entire palette of the `modus-operandi-tinted' theme.

Named colors have the form (COLOR-NAME HEX-VALUE) with the former
as a symbol and the latter as a string.

Semantic color mappings have the form (MAPPING-NAME COLOR-NAME)
with both as symbols.  The latter is a named color that already
exists in the palette and is associated with a HEX-VALUE.")

(defconst modus-themes-operandi-deuteranopia-palette
  (append
   '(
     ;; Basic values

     (bg-main          "#ffffff")
     (bg-dim           "#f2f2f2")
     (fg-main          "#000000")
     (fg-dim           "#595959")
     (fg-alt           "#193668")
     (bg-active        "#c4c4c4")
     (bg-inactive      "#e0e0e0")
     (border           "#9f9f9f")

     ;; Common accent foregrounds

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

     ;; Uncommon accent foregrounds

     (rust       "#8a290f")
     (gold       "#70550f")
     (olive      "#4c6000")
     (slate      "#2f3f83")
     (indigo     "#4a3a8a")
     (maroon     "#731c52")
     (pink       "#7b435c")

     ;; Common accent backgrounds

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

     (bg-red-nuanced     "#ffe8e8")
     (bg-green-nuanced   "#e0f6e0")
     (bg-yellow-nuanced  "#f8f0d0")
     (bg-blue-nuanced    "#ecedff")
     (bg-magenta-nuanced "#f8e6f5")
     (bg-cyan-nuanced    "#e0f2fa")

     ;; Uncommon accent background and foreground pairs

     (bg-clay     "#f1c8b5")
     (fg-clay     "#63192a")

     (bg-ochre    "#f0e3c0")
     (fg-ochre    "#573a30")

     (bg-lavender "#dfcdfa")
     (fg-lavender "#443379")

     (bg-sage     "#c0e7d4")
     (fg-sage     "#124b41")

     ;; Graphs

     (bg-graph-red-0     "#d0b029")
     (bg-graph-red-1     "#e0cab4")
     (bg-graph-green-0   "#8ac050")
     (bg-graph-green-1   "#afdfa5")
     (bg-graph-yellow-0  "#ffcf00")
     (bg-graph-yellow-1  "#f9ff00")
     (bg-graph-blue-0    "#7f9fff")
     (bg-graph-blue-1    "#afd0ff")
     (bg-graph-magenta-0 "#b0b0d0")
     (bg-graph-magenta-1 "#d0dfdf")
     (bg-graph-cyan-0    "#6faad9")
     (bg-graph-cyan-1    "#bfe0ff")

     ;; Special purpose

     (bg-completion       "#c0deff")
     (bg-hover            "#b2e4dc")
     (bg-hover-secondary  "#e5d7a0")
     (bg-hl-line          "#dae5ec")
     (bg-region           "#bdbdbd")
     (fg-region           "#000000")

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

     ;; Diffs

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
     (bg-removed-refine  "#ede06f")
     (bg-removed-fringe  "#c0b200")
     (fg-removed         "#553d00")
     (fg-removed-intense "#7f6f00")

     (bg-diff-context    "#f3f3f3")

     ;; Paren match

     (bg-paren-match        "#5fcfff")

     (bg-paren-expression   "#efd3f5")
     (underline-paren-match unspecified)

     ;; General mappings

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

     (bg-active-argument bg-yellow-nuanced)
     (fg-active-argument yellow-warmer)
     (bg-active-value bg-blue-nuanced)
     (fg-active-value blue-warmer)

     ;; Code mappings

     (builtin yellow)
     (comment yellow-cooler)
     (constant blue-faint)
     (docstring green-faint)
     (fnname yellow-warmer)
     (fnname-call gold)
     (keyword blue-cooler)
     (preprocessor magenta-cooler)
     (property cyan)
     (rx-backslash blue-cooler)
     (rx-construct yellow-cooler)
     (string blue-warmer)
     (type cyan-cooler)
     (variable cyan)
     (variable-use indigo)

     ;; Accent mappings

     (accent-0 blue-warmer)
     (accent-1 yellow-warmer)
     (accent-2 cyan)
     (accent-3 yellow-cooler)

     ;; Completion mappings

     (fg-completion-match-0 blue-warmer)
     (fg-completion-match-1 yellow-warmer)
     (fg-completion-match-2 cyan)
     (fg-completion-match-3 yellow-cooler)

     ;; Date mappings

     (date-common cyan)
     (date-deadline yellow-warmer)
     (date-deadline-subtle red-faint)
     (date-event fg-alt)
     (date-holiday yellow-warmer)
     (date-holiday-other blue)
     (date-range fg-alt)
     (date-scheduled yellow-cooler)
     (date-scheduled-subtle yellow-faint)
     (date-weekday cyan)
     (date-weekend blue-cooler)

     ;; Link mappings

     (fg-link blue-warmer)
     (underline-link blue-warmer)
     (fg-link-symbolic cyan)
     (underline-link-symbolic cyan)
     (fg-link-visited yellow-faint)
     (underline-link-visited yellow-faint)

     ;; Mail mappings

     (mail-cite-0 blue-warmer)
     (mail-cite-1 yellow)
     (mail-cite-2 cyan-faint)
     (mail-cite-3 yellow-faint)
     (mail-part blue)
     (mail-recipient blue)
     (mail-subject yellow-cooler)
     (mail-other cyan-faint)

     ;; Mark mappings

     (bg-mark-delete bg-yellow-subtle)
     (fg-mark-delete yellow)
     (bg-mark-select bg-cyan-subtle)
     (fg-mark-select cyan)
     (bg-mark-other bg-magenta-subtle)
     (fg-mark-other magenta)

     ;; Prompt mappings

     (fg-prompt blue)

     ;; Prose mappings

     (fg-prose-code cyan-cooler)
     (fg-prose-macro magenta-cooler)
     (fg-prose-verbatim yellow)
     (prose-done blue)
     (prose-todo yellow-warmer)
     (prose-metadata fg-dim)
     (prose-metadata-value fg-alt)
     (prose-table fg-alt)
     (prose-table-formula yellow-warmer)
     (prose-tag fg-alt)

     ;; Rainbow mappings

     (rainbow-0 blue)
     (rainbow-1 yellow)
     (rainbow-2 blue-warmer)
     (rainbow-3 yellow-cooler)
     (rainbow-4 blue-cooler)
     (rainbow-5 yellow-warmer)
     (rainbow-6 blue-faint)
     (rainbow-7 yellow-faint)
     (rainbow-8 cyan)

     ;; Search mappings

     (bg-search-current bg-yellow-intense)
     (bg-search-lazy bg-blue-intense)
     (bg-search-static bg-cyan-subtle)
     (bg-search-replace bg-yellow-intense)

     (bg-search-rx-group-0 bg-cyan-intense)
     (bg-search-rx-group-1 bg-magenta-intense)
     (bg-search-rx-group-2 bg-blue-subtle)
     (bg-search-rx-group-3 bg-yellow-subtle)

     ;; Heading mappings

     (fg-heading-0 cyan-cooler)
     (fg-heading-1 fg-main)
     (fg-heading-2 yellow-faint)
     (fg-heading-3 blue-faint)
     (fg-heading-4 green-faint)
     (fg-heading-5 magenta-cooler)
     (fg-heading-6 yellow-cooler)
     (fg-heading-7 cyan)
     (fg-heading-8 fg-dim))
   modus-themes-common-palette-mappings)
  "The entire palette of the `modus-operandi-deuteranopia' theme.

Named colors have the form (COLOR-NAME HEX-VALUE) with the former
as a symbol and the latter as a string.

Semantic color mappings have the form (MAPPING-NAME COLOR-NAME)
with both as symbols.  The latter is a named color that already
exists in the palette and is associated with a HEX-VALUE.")

(defconst modus-themes-operandi-tritanopia-palette
  (append
   '(
     ;; Basic values

     (bg-main          "#ffffff")
     (bg-dim           "#f2f2f2")
     (fg-main          "#000000")
     (fg-dim           "#595959")
     (fg-alt           "#224960")
     (bg-active        "#c4c4c4")
     (bg-inactive      "#e0e0e0")
     (border           "#9f9f9f")

     ;; Common accent foregrounds

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

     ;; Uncommon accent foregrounds

     (rust       "#8a290f")
     (gold       "#70550f")
     (olive      "#4c6000")
     (slate      "#104860")
     (indigo     "#4a3a8a")
     (maroon     "#731c52")
     (pink       "#7b435c")

     ;; Common accent backgrounds

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

     (bg-red-nuanced     "#ffe8e8")
     (bg-green-nuanced   "#e0f6e0")
     (bg-yellow-nuanced  "#f8f0d0")
     (bg-blue-nuanced    "#ecedff")
     (bg-magenta-nuanced "#f8e6f5")
     (bg-cyan-nuanced    "#e0f2fa")

     ;; Uncommon accent background and foreground pairs

     (bg-clay     "#f1c8b5")
     (fg-clay     "#63192a")

     (bg-ochre    "#f0e3c0")
     (fg-ochre    "#573a30")

     (bg-lavender "#dfcdfa")
     (fg-lavender "#443379")

     (bg-sage     "#c0e7d4")
     (fg-sage     "#124b41")

     ;; Graphs

     (bg-graph-red-0     "#ef7969")
     (bg-graph-red-1     "#ffaab4")
     (bg-graph-green-0   "#68c0a0")
     (bg-graph-green-1   "#a5dfd0")
     (bg-graph-yellow-0  "#d99f9f")
     (bg-graph-yellow-1  "#ffb58f")
     (bg-graph-blue-0    "#80a0df")
     (bg-graph-blue-1    "#a8cfff")
     (bg-graph-magenta-0 "#efafcf")
     (bg-graph-magenta-1 "#ffdaef")
     (bg-graph-cyan-0    "#7fd3ed")
     (bg-graph-cyan-1    "#afefff")

     ;; Special purpose

     (bg-completion       "#afdfef")
     (bg-hover            "#ffafbc")
     (bg-hover-secondary  "#abdfdd")
     (bg-hl-line          "#dfeaec")
     (bg-region           "#bdbdbd")
     (fg-region           "#000000")

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

     ;; Diffs

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

     ;; Paren match

     (bg-paren-match        "#5fcfff")
     (bg-paren-expression   "#efd3f5")

     ;; General mappings

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

     (bg-active-argument bg-red-nuanced)
     (fg-active-argument red-warmer)
     (bg-active-value bg-cyan-nuanced)
     (fg-active-value cyan)

     ;; Code mappings

     (builtin magenta)
     (comment red-faint)
     (constant green-cooler)
     (docstring fg-alt)
     (fnname cyan-warmer)
     (fnname-call indigo)
     (keyword red-cooler)
     (preprocessor red-warmer)
     (property cyan-cooler)
     (rx-backslash magenta)
     (rx-construct red)
     (string cyan)
     (type blue-warmer)
     (variable cyan-cooler)
     (variable-use slate)

     ;; Accent mappings

     (accent-0 cyan)
     (accent-1 red-warmer)
     (accent-2 cyan-cooler)
     (accent-3 magenta)

     ;; Completion mappings

     (fg-completion-match-0 cyan)
     (fg-completion-match-1 red-warmer)
     (fg-completion-match-2 magenta)
     (fg-completion-match-3 cyan-cooler)

     ;; Date mappings

     (date-common cyan-cooler)
     (date-deadline red)
     (date-deadline-subtle red-faint)
     (date-event fg-alt)
     (date-holiday red)
     (date-holiday-other cyan)
     (date-range fg-alt)
     (date-scheduled magenta)
     (date-scheduled-subtle magenta-faint)
     (date-weekday cyan)
     (date-weekend magenta-warmer)

     ;; Link mappings

     (fg-link cyan)
     (underline-link cyan)
     (fg-link-symbolic cyan-cooler)
     (underline-link-symbolic cyan-cooler)
     (fg-link-visited magenta)
     (underline-link-visited magenta)

     ;; Mail mappings

     (mail-cite-0 cyan-faint)
     (mail-cite-1 red-faint)
     (mail-cite-2 magenta-warmer)
     (mail-cite-3 cyan-warmer)
     (mail-part cyan-cooler)
     (mail-recipient cyan)
     (mail-subject red-cooler)
     (mail-other cyan)

     ;; Mark mappings

     (bg-mark-delete bg-red-subtle)
     (fg-mark-delete red)
     (bg-mark-select bg-cyan-subtle)
     (fg-mark-select cyan)
     (bg-mark-other bg-magenta-subtle)
     (fg-mark-other magenta)

     ;; Prompt mappings

     (fg-prompt cyan-cooler)

     ;; Prose mappings

     (fg-prose-code cyan)
     (fg-prose-macro red-warmer)
     (fg-prose-verbatim magenta-warmer)
     (prose-done cyan)
     (prose-todo red)
     (prose-metadata fg-dim)
     (prose-metadata-value fg-alt)
     (prose-table fg-alt)
     (prose-table-formula red-cooler)
     (prose-tag fg-alt)

     ;; Rainbow mappings

     (rainbow-0 cyan)
     (rainbow-1 red)
     (rainbow-2 cyan-warmer)
     (rainbow-3 red-cooler)
     (rainbow-4 cyan-cooler)
     (rainbow-5 magenta)
     (rainbow-6 cyan-faint)
     (rainbow-7 magenta-faint)
     (rainbow-8 red-faint)

     ;; Search mappings

     (bg-search-current bg-red-intense)
     (bg-search-lazy bg-cyan-intense)
     (bg-search-static bg-magenta-subtle)
     (bg-search-replace bg-magenta-intense)

     (bg-search-rx-group-0 bg-blue-intense)
     (bg-search-rx-group-1 bg-magenta-intense)
     (bg-search-rx-group-2 bg-cyan-subtle)
     (bg-search-rx-group-3 bg-red-subtle)

     ;; Heading mappings

     (fg-heading-0 cyan-cooler)
     (fg-heading-1 fg-main)
     (fg-heading-2 red-faint)
     (fg-heading-3 cyan-faint)
     (fg-heading-4 magenta)
     (fg-heading-5 green-faint)
     (fg-heading-6 magenta-faint)
     (fg-heading-7 cyan-warmer)
     (fg-heading-8 fg-dim))
   modus-themes-common-palette-mappings)
  "The entire palette of the `modus-operandi-tritanopia' theme.

Named colors have the form (COLOR-NAME HEX-VALUE) with the former
as a symbol and the latter as a string.

Semantic color mappings have the form (MAPPING-NAME COLOR-NAME)
with both as symbols.  The latter is a named color that already
exists in the palette and is associated with a HEX-VALUE.")

(defconst modus-themes-vivendi-palette
  (append
   '(
     ;; Basic values

     (bg-main          "#000000")
     (bg-dim           "#1e1e1e")
     (fg-main          "#ffffff")
     (fg-dim           "#989898")
     (fg-alt           "#c6daff")
     (bg-active        "#535353")
     (bg-inactive      "#303030")
     (border           "#646464")

     ;; Common accent foregrounds

     (red             "#ff5f59")
     (red-warmer      "#ff6b55")
     (red-cooler      "#ff7f86")
     (red-faint       "#ff9580")
     (red-intense     "#ff5f5f")
     (green           "#44bc44")
     (green-warmer    "#70b900")
     (green-cooler    "#00c06f")
     (green-faint     "#88ca9f")
     (green-intense   "#44df44")
     (yellow          "#d0bc00")
     (yellow-warmer   "#fec43f")
     (yellow-cooler   "#dfaf7a")
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
     (magenta-intense "#ff66ff")
     (cyan            "#00d3d0")
     (cyan-warmer     "#4ae2f0")
     (cyan-cooler     "#6ae4b9")
     (cyan-faint      "#9ac8e0")
     (cyan-intense    "#00eff0")

     ;; Uncommon accent foregrounds

     (rust       "#db7b5f")
     (gold       "#c0965b")
     (olive      "#9cbd6f")
     (slate      "#76afbf")
     (indigo     "#9099d9")
     (maroon     "#cf7fa7")
     (pink       "#d09dc0")

     ;; Common accent backgrounds

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

     (bg-red-nuanced     "#3a0c14")
     (bg-green-nuanced   "#092f1f")
     (bg-yellow-nuanced  "#381d0f")
     (bg-blue-nuanced    "#12154a")
     (bg-magenta-nuanced "#2f0c3f")
     (bg-cyan-nuanced    "#042837")

     ;; Uncommon accent background and foreground pairs

     (bg-clay     "#49191a")
     (fg-clay     "#f1b090")

     (bg-ochre    "#462f20")
     (fg-ochre    "#e0d38c")

     (bg-lavender "#38325c")
     (fg-lavender "#dfc0f0")

     (bg-sage     "#143e32")
     (fg-sage     "#c3e7d4")

     ;; Graphs

     (bg-graph-red-0     "#b52c2c")
     (bg-graph-red-1     "#702020")
     (bg-graph-green-0   "#0fed00")
     (bg-graph-green-1   "#007800")
     (bg-graph-yellow-0  "#f1e00a")
     (bg-graph-yellow-1  "#b08940")
     (bg-graph-blue-0    "#2fafef")
     (bg-graph-blue-1    "#1f2f8f")
     (bg-graph-magenta-0 "#bf94fe")
     (bg-graph-magenta-1 "#5f509f")
     (bg-graph-cyan-0    "#47dfea")
     (bg-graph-cyan-1    "#00808f")

     ;; Special purpose

     (bg-completion       "#2f447f")
     (bg-hover            "#45605e")
     (bg-hover-secondary  "#654a39")
     (bg-hl-line          "#2f3849")
     (bg-region           "#5a5a5a")
     (fg-region           "#ffffff")

     (bg-mode-line-active        "#505050")
     (fg-mode-line-active        "#ffffff")
     (border-mode-line-active    "#959595")
     (bg-mode-line-inactive      "#2d2d2d")
     (fg-mode-line-inactive      "#969696")
     (border-mode-line-inactive  "#606060")

     (modeline-err     "#ffa9bf")
     (modeline-warning "#dfcf43")
     (modeline-info    "#9fefff")

     (bg-tab-bar      "#313131")
     (bg-tab-current  "#000000")
     (bg-tab-other    "#545454")

     ;; Diffs

     (bg-added           "#00381f")
     (bg-added-faint     "#002910")
     (bg-added-refine    "#034f2f")
     (bg-added-fringe    "#237f3f")
     (fg-added           "#a0e0a0")
     (fg-added-intense   "#80e080")

     (bg-changed         "#363300")
     (bg-changed-faint   "#2a1f00")
     (bg-changed-refine  "#4a4a00")
     (bg-changed-fringe  "#8a7a00")
     (fg-changed         "#efef80")
     (fg-changed-intense "#c0b05f")

     (bg-removed         "#4f1119")
     (bg-removed-faint   "#380a0f")
     (bg-removed-refine  "#781a1f")
     (bg-removed-fringe  "#b81a1f")
     (fg-removed         "#ffbfbf")
     (fg-removed-intense "#ff9095")

     (bg-diff-context    "#1a1a1a")

     ;; Paren match

     (bg-paren-match        "#2f7f9f")
     (bg-paren-expression   "#453040")

     ;; General mappings

     (cursor fg-main)
     (keybind blue-cooler)
     (name magenta)
     (identifier yellow-faint)

     (err red)
     (warning yellow-warmer)
     (info cyan-cooler)

     (underline-err red-intense)
     (underline-warning yellow)
     (underline-note cyan)

     (bg-prominent-err bg-red-intense)
     (fg-prominent-err fg-main)
     (bg-prominent-warning bg-yellow-intense)
     (fg-prominent-warning fg-main)
     (bg-prominent-note bg-cyan-intense)
     (fg-prominent-note fg-main)

     (bg-active-argument bg-yellow-nuanced)
     (fg-active-argument yellow-cooler)
     (bg-active-value bg-cyan-nuanced)
     (fg-active-value cyan-cooler)

     ;; Code mappings

     (builtin magenta-warmer)
     (comment fg-dim)
     (constant blue-cooler)
     (docstring cyan-faint)
     (fnname magenta)
     (fnname-call pink)
     (keyword magenta-cooler)
     (preprocessor red-cooler)
     (property cyan)
     (rx-backslash magenta)
     (rx-construct green-cooler)
     (string blue-warmer)
     (type cyan-cooler)
     (variable cyan)
     (variable-use slate)

     ;; Accent mappings

     (accent-0 blue-cooler)
     (accent-1 magenta-warmer)
     (accent-2 cyan-cooler)
     (accent-3 yellow)

     ;; Completion mappings

     (fg-completion-match-0 blue-cooler)
     (fg-completion-match-1 magenta-warmer)
     (fg-completion-match-2 cyan-cooler)
     (fg-completion-match-3 yellow)

     ;; Date mappings

     (date-common cyan)
     (date-deadline red-cooler)
     (date-deadline-subtle red-faint)
     (date-event fg-alt)
     (date-holiday magenta-warmer)
     (date-holiday-other blue)
     (date-range fg-alt)
     (date-scheduled yellow-cooler)
     (date-scheduled-subtle yellow-faint)
     (date-weekday cyan)
     (date-weekend magenta)

     ;; Link mappings

     (fg-link blue-warmer)
     (underline-link blue-warmer)
     (fg-link-symbolic cyan)
     (underline-link-symbolic cyan)
     (fg-link-visited magenta)
     (underline-link-visited magenta)

     ;; Mail mappings

     (mail-cite-0 blue-warmer)
     (mail-cite-1 yellow-cooler)
     (mail-cite-2 cyan-cooler)
     (mail-cite-3 red-cooler)
     (mail-part blue)
     (mail-recipient magenta-cooler)
     (mail-subject magenta-warmer)
     (mail-other magenta-faint)

     ;; Mark mappings

     (bg-mark-delete bg-red-subtle)
     (fg-mark-delete red-cooler)
     (bg-mark-select bg-cyan-subtle)
     (fg-mark-select cyan)
     (bg-mark-other bg-yellow-subtle)
     (fg-mark-other yellow)

     ;; Prompt mappings

     (fg-prompt cyan-cooler)

     ;; Prose mappings

     (fg-prose-code cyan-cooler)
     (fg-prose-macro magenta-cooler)
     (fg-prose-verbatim magenta-warmer)
     (prose-done green)
     (prose-todo red)
     (prose-metadata fg-dim)
     (prose-metadata-value fg-alt)
     (prose-table fg-alt)
     (prose-table-formula magenta-warmer)
     (prose-tag magenta-faint)

     ;; Rainbow mappings

     (rainbow-0 fg-main)
     (rainbow-1 magenta-intense)
     (rainbow-2 cyan-intense)
     (rainbow-3 red-warmer)
     (rainbow-4 yellow-intense)
     (rainbow-5 magenta-cooler)
     (rainbow-6 green-intense)
     (rainbow-7 blue-warmer)
     (rainbow-8 magenta-warmer)

     ;; Search mappings

     (bg-search-current bg-yellow-intense)
     (bg-search-lazy bg-cyan-intense)
     (bg-search-static bg-magenta-subtle)
     (bg-search-replace bg-red-intense)

     (bg-search-rx-group-0 bg-blue-intense)
     (bg-search-rx-group-1 bg-green-intense)
     (bg-search-rx-group-2 bg-red-subtle)
     (bg-search-rx-group-3 bg-magenta-subtle)

     ;; Heading mappings

     (fg-heading-0 cyan-cooler)
     (fg-heading-1 fg-main)
     (fg-heading-2 yellow-faint)
     (fg-heading-3 blue-faint)
     (fg-heading-4 magenta)
     (fg-heading-5 green-faint)
     (fg-heading-6 red-faint)
     (fg-heading-7 cyan-faint)
     (fg-heading-8 fg-dim))
   modus-themes-common-palette-mappings)
  "The entire palette of the `modus-vivendi' theme.

Named colors have the form (COLOR-NAME HEX-VALUE) with the former
as a symbol and the latter as a string.

Semantic color mappings have the form (MAPPING-NAME COLOR-NAME)
with both as symbols.  The latter is a named color that already
exists in the palette and is associated with a HEX-VALUE.")

(defconst modus-themes-vivendi-tinted-palette
  (append
   '(
     ;; Basic values

     (bg-main          "#0d0e1c")
     (bg-dim           "#1d2235")
     (fg-main          "#ffffff")
     (fg-dim           "#989898")
     (fg-alt           "#c6daff")
     (bg-active        "#4a4f69")
     (bg-inactive      "#2b3045")
     (border           "#61647a")

     ;; Common accent foregrounds

     (red             "#ff5f59")
     (red-warmer      "#ff6b55")
     (red-cooler      "#ff7f86")
     (red-faint       "#ef8386")
     (red-intense     "#ff5f5f")
     (green           "#44bc44")
     (green-warmer    "#75c13e")
     (green-cooler    "#11c777")
     (green-faint     "#88ca9f")
     (green-intense   "#44df44")
     (yellow          "#d0bc00")
     (yellow-warmer   "#fec43f")
     (yellow-cooler   "#dfaf7a")
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
     (magenta-intense "#ff66ff")
     (cyan            "#00d3d0")
     (cyan-warmer     "#4ae2f0")
     (cyan-cooler     "#6ae4b9")
     (cyan-faint      "#9ac8e0")
     (cyan-intense    "#00eff0")

     ;; Uncommon accent foregrounds

     (rust       "#db8b3f")
     (gold       "#c0965b")
     (olive      "#9cbd6f")
     (slate      "#76afbf")
     (indigo     "#9099d9")
     (maroon     "#cf7fa7")
     (pink       "#d09dc0")

     ;; Common accent backgrounds

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

     (bg-red-nuanced     "#3a0c14")
     (bg-green-nuanced   "#092f1f")
     (bg-yellow-nuanced  "#381d0f")
     (bg-blue-nuanced    "#12154a")
     (bg-magenta-nuanced "#2f0c3f")
     (bg-cyan-nuanced    "#042837")

     ;; Uncommon accent background and foreground pairs

     (bg-clay     "#49191a")
     (fg-clay     "#f1b090")

     (bg-ochre    "#462f20")
     (fg-ochre    "#e0d09c")

     (bg-lavender "#38325c")
     (fg-lavender "#dfc0f0")

     (bg-sage     "#143e32")
     (fg-sage     "#83d7ac")

     ;; Graphs

     (bg-graph-red-0     "#b52c2c")
     (bg-graph-red-1     "#702020")
     (bg-graph-green-0   "#0fed00")
     (bg-graph-green-1   "#007800")
     (bg-graph-yellow-0  "#f1e00a")
     (bg-graph-yellow-1  "#b08940")
     (bg-graph-blue-0    "#2fafef")
     (bg-graph-blue-1    "#1f2f8f")
     (bg-graph-magenta-0 "#bf94fe")
     (bg-graph-magenta-1 "#5f509f")
     (bg-graph-cyan-0    "#47dfea")
     (bg-graph-cyan-1    "#00808f")

     ;; Special purpose

     (bg-completion       "#483d8a")
     (bg-hover            "#45605e")
     (bg-hover-secondary  "#64404f")
     (bg-hl-line          "#303a6f")
     (bg-region           "#555a66")
     (fg-region           "#ffffff")

     (bg-mode-line-active        "#484d67")
     (fg-mode-line-active        "#ffffff")
     (border-mode-line-active    "#979797")
     (bg-mode-line-inactive      "#292d48")
     (fg-mode-line-inactive      "#969696")
     (border-mode-line-inactive  "#606270")

     (modeline-err     "#ffa9bf")
     (modeline-warning "#dfcf43")
     (modeline-info    "#9fefff")

     (bg-tab-bar      "#2c3045")
     (bg-tab-current  "#0d0e1c")
     (bg-tab-other    "#4a4f6a")

     ;; Diffs

     (bg-added           "#003a2f")
     (bg-added-faint     "#002922")
     (bg-added-refine    "#035542")
     (bg-added-fringe    "#23884f")
     (fg-added           "#a0e0a0")
     (fg-added-intense   "#80e080")

     (bg-changed         "#363300")
     (bg-changed-faint   "#2a1f00")
     (bg-changed-refine  "#4a4a00")
     (bg-changed-fringe  "#8f7a30")
     (fg-changed         "#efef80")
     (fg-changed-intense "#c0b05f")

     (bg-removed         "#4f1127")
     (bg-removed-faint   "#380a19")
     (bg-removed-refine  "#781a3a")
     (bg-removed-fringe  "#b81a26")
     (fg-removed         "#ffbfbf")
     (fg-removed-intense "#ff9095")

     (bg-diff-context    "#1a1f30")

     ;; Paren match

     (bg-paren-match        "#4f7f9f")
     (bg-paren-expression   "#453040")

     ;; General mappings

     (cursor magenta-intense)
     (keybind magenta-cooler)
     (name magenta)
     (identifier yellow-faint)

     (err red)
     (warning yellow)
     (info green-cooler)

     (underline-err red-intense)
     (underline-warning yellow)
     (underline-note cyan)

     (bg-prominent-err bg-red-intense)
     (fg-prominent-err fg-main)
     (bg-prominent-warning bg-yellow-intense)
     (fg-prominent-warning fg-main)
     (bg-prominent-note bg-cyan-intense)
     (fg-prominent-note fg-main)

     (bg-active-argument bg-yellow-nuanced)
     (fg-active-argument yellow-cooler)
     (bg-active-value bg-cyan-nuanced)
     (fg-active-value cyan-cooler)

     ;; Code mappings

     (builtin magenta)
     (comment red-faint)
     (constant magenta-cooler)
     (docstring cyan-faint)
     (fnname magenta-warmer)
     (fnname-call pink)
     (keyword blue-warmer)
     (preprocessor red-cooler)
     (property cyan-warmer)
     (rx-backslash magenta-warmer)
     (rx-construct magenta-cooler)
     (string blue)
     (type green-cooler)
     (variable cyan-warmer)
     (variable-use slate)

     ;; Accent mappings

     (accent-0 magenta-cooler)
     (accent-1 cyan)
     (accent-2 magenta-warmer)
     (accent-3 yellow-warmer)

     ;; Completion mappings

     (fg-completion-match-0 blue-cooler)
     (fg-completion-match-1 magenta-warmer)
     (fg-completion-match-2 cyan-cooler)
     (fg-completion-match-3 yellow)

     ;; Date mappings

     (date-common cyan)
     (date-deadline red-cooler)
     (date-deadline-subtle red-faint)
     (date-event fg-alt)
     (date-holiday magenta-warmer)
     (date-holiday-other blue)
     (date-range fg-alt)
     (date-scheduled yellow-cooler)
     (date-scheduled-subtle yellow-faint)
     (date-weekday cyan)
     (date-weekend magenta)

     ;; Link mappings

     (fg-link blue-warmer)
     (underline-link blue-warmer)
     (fg-link-symbolic cyan)
     (underline-link-symbolic cyan)
     (fg-link-visited magenta)
     (underline-link-visited magenta)

     ;; Mail mappings

     (mail-cite-0 blue-faint)
     (mail-cite-1 yellow-cooler)
     (mail-cite-2 cyan-cooler)
     (mail-cite-3 red-cooler)
     (mail-part blue)
     (mail-recipient blue-warmer)
     (mail-subject magenta-warmer)
     (mail-other magenta)

     ;; Mark mappings

     (bg-mark-delete bg-red-subtle)
     (fg-mark-delete red-cooler)
     (bg-mark-select bg-cyan-subtle)
     (fg-mark-select cyan)
     (bg-mark-other bg-yellow-subtle)
     (fg-mark-other yellow)

     ;; Prompt mappings

     (fg-prompt cyan-warmer)

     ;; Prose mappings

     (fg-prose-code cyan-cooler)
     (fg-prose-macro magenta-cooler)
     (fg-prose-verbatim magenta-warmer)
     (prose-done green)
     (prose-todo red)
     (prose-metadata fg-dim)
     (prose-metadata-value fg-alt)
     (prose-table fg-alt)
     (prose-table-formula magenta-warmer)
     (prose-tag magenta-faint)

     ;; Rainbow mappings

     (rainbow-0 fg-main)
     (rainbow-1 magenta-intense)
     (rainbow-2 cyan-intense)
     (rainbow-3 red-warmer)
     (rainbow-4 yellow-intense)
     (rainbow-5 magenta-cooler)
     (rainbow-6 green-intense)
     (rainbow-7 blue-warmer)
     (rainbow-8 magenta-warmer)

     ;; Search mappings

     (bg-search-current bg-yellow-intense)
     (bg-search-lazy bg-cyan-intense)
     (bg-search-static bg-magenta-subtle)
     (bg-search-replace bg-red-intense)

     (bg-search-rx-group-0 bg-blue-intense)
     (bg-search-rx-group-1 bg-green-intense)
     (bg-search-rx-group-2 bg-red-subtle)
     (bg-search-rx-group-3 bg-magenta-subtle)

     ;; Heading mappings

     (fg-heading-0 cyan-cooler)
     (fg-heading-1 fg-main)
     (fg-heading-2 yellow-faint)
     (fg-heading-3 blue-faint)
     (fg-heading-4 magenta)
     (fg-heading-5 green-faint)
     (fg-heading-6 red-faint)
     (fg-heading-7 cyan-faint)
     (fg-heading-8 fg-dim))
   modus-themes-common-palette-mappings)
  "The entire palette of the `modus-vivendi-tinted' theme.

Named colors have the form (COLOR-NAME HEX-VALUE) with the former
as a symbol and the latter as a string.

Semantic color mappings have the form (MAPPING-NAME COLOR-NAME)
with both as symbols.  The latter is a named color that already
exists in the palette and is associated with a HEX-VALUE.")

(defconst modus-themes-vivendi-deuteranopia-palette
  (append
   '(
     ;; Basic values

     (bg-main          "#000000")
     (bg-dim           "#1e1e1e")
     (fg-main          "#ffffff")
     (fg-dim           "#989898")
     (fg-alt           "#c6daff")
     (bg-active        "#535353")
     (bg-inactive      "#303030")
     (border           "#646464")

     ;; Common accent foregrounds

     (red             "#ff5f59")
     (red-warmer      "#ff6b55")
     (red-cooler      "#ff7f86")
     (red-faint       "#ff9580")
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
     (magenta-intense "#ff66ff")
     (cyan            "#00d3d0")
     (cyan-warmer     "#4ae2f0")
     (cyan-cooler     "#6ae4b9")
     (cyan-faint      "#9ac8e0")
     (cyan-intense    "#00eff0")

     ;; Uncommon accent foregrounds

     (rust       "#db7b5f")
     (gold       "#c0965b")
     (olive      "#9cbd6f")
     (slate      "#76afbf")
     (indigo     "#9099d9")
     (maroon     "#cf7fa7")
     (pink       "#d09dc0")

     ;; Common accent backgrounds

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

     (bg-red-nuanced     "#3a0c14")
     (bg-green-nuanced   "#092f1f")
     (bg-yellow-nuanced  "#381d0f")
     (bg-blue-nuanced    "#12154a")
     (bg-magenta-nuanced "#2f0c3f")
     (bg-cyan-nuanced    "#042837")

     ;; Uncommon accent background and foreground pairs

     (bg-clay     "#49191a")
     (fg-clay     "#f1b090")

     (bg-ochre    "#462f20")
     (fg-ochre    "#e0d09c")

     (bg-lavender "#38325c")
     (fg-lavender "#dfc0f0")

     (bg-sage     "#143e32")
     (fg-sage     "#c3e7d4")

     ;; Graphs

     (bg-graph-red-0     "#bf6000")
     (bg-graph-red-1     "#733500")
     (bg-graph-green-0   "#6fbf8f")
     (bg-graph-green-1   "#2f5f4f")
     (bg-graph-yellow-0  "#c1c00a")
     (bg-graph-yellow-1  "#7f6640")
     (bg-graph-blue-0    "#0f90ef")
     (bg-graph-blue-1    "#1f2f8f")
     (bg-graph-magenta-0 "#7f7f8e")
     (bg-graph-magenta-1 "#4f4f5f")
     (bg-graph-cyan-0    "#376f9a")
     (bg-graph-cyan-1    "#00404f")

     ;; Special purpose

     (bg-completion       "#2f447f")
     (bg-hover            "#45605e")
     (bg-hover-secondary  "#604c30")
     (bg-hl-line          "#2f3849")
     (bg-region           "#5a5a5a")
     (fg-region           "#ffffff")

     (bg-mode-line-active        "#2a2a6a")
     (fg-mode-line-active        "#f0f0f0")
     (border-mode-line-active    "#8080a7")
     (bg-mode-line-inactive      "#2d2d2d")
     (fg-mode-line-inactive      "#969696")
     (border-mode-line-inactive  "#606060")

     (modeline-err     "#e5bf00")
     (modeline-warning "#c0cf35")
     (modeline-info    "#abeadf")

     (bg-tab-bar      "#313131")
     (bg-tab-current  "#000000")
     (bg-tab-other    "#545454")

     ;; Diffs

     (bg-added           "#003066")
     (bg-added-faint     "#001a4f")
     (bg-added-refine    "#0f4a77")
     (bg-added-fringe    "#006fff")
     (fg-added           "#c4d5ff")
     (fg-added-intense   "#8080ff")

     (bg-changed         "#2f123f")
     (bg-changed-faint   "#1f022f")
     (bg-changed-refine  "#3f325f")
     (bg-changed-fringe  "#7f55a0")
     (fg-changed         "#e3cfff")
     (fg-changed-intense "#cf9fe2")

     (bg-removed         "#3d3d00")
     (bg-removed-faint   "#281f00")
     (bg-removed-refine  "#555500")
     (bg-removed-fringe  "#d0c03f")
     (fg-removed         "#d4d48f")
     (fg-removed-intense "#d0b05f")

     (bg-diff-context    "#1a1a1a")

     ;; Paren match

     (bg-paren-match        "#2f7f9f")
     (bg-paren-expression   "#453040")

     ;; General mappings

     (cursor yellow-intense)
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

     (bg-active-argument bg-yellow-nuanced)
     (fg-active-argument yellow-warmer)
     (bg-active-value bg-blue-nuanced)
     (fg-active-value blue-warmer)

     ;; Code mappings

     (builtin yellow)
     (comment yellow-cooler)
     (constant blue-faint)
     (docstring cyan-faint)
     (fnname yellow-warmer)
     (fnname-call gold)
     (keyword blue-cooler)
     (preprocessor magenta-cooler)
     (property cyan)
     (rx-backslash blue-cooler)
     (rx-construct yellow-cooler)
     (string blue-warmer)
     (type cyan-cooler)
     (variable cyan)
     (variable-use slate)

     ;; Accent mappings

     (accent-0 blue-warmer)
     (accent-1 yellow)
     (accent-2 cyan-cooler)
     (accent-3 yellow-cooler)

     ;; Completion mappings

     (fg-completion-match-0 blue-cooler)
     (fg-completion-match-1 yellow)
     (fg-completion-match-2 cyan-cooler)
     (fg-completion-match-3 yellow-cooler)

     ;; Date mappings

     (date-common cyan)
     (date-deadline yellow-warmer)
     (date-deadline-subtle red-faint)
     (date-event fg-alt)
     (date-holiday yellow-warmer)
     (date-holiday-other blue)
     (date-range fg-alt)
     (date-scheduled yellow-cooler)
     (date-scheduled-subtle yellow-faint)
     (date-weekday cyan)
     (date-weekend magenta-cooler)

     ;; Link mappings

     (fg-link blue-warmer)
     (underline-link blue-warmer)
     (fg-link-symbolic cyan)
     (underline-link-symbolic cyan)
     (fg-link-visited yellow-faint)
     (underline-link-visited yellow-faint)

     ;; Mail mappings

     (mail-cite-0 blue-warmer)
     (mail-cite-1 yellow-cooler)
     (mail-cite-2 cyan-faint)
     (mail-cite-3 yellow)
     (mail-part blue)
     (mail-recipient blue)
     (mail-subject yellow-warmer)
     (mail-other cyan-faint)

     ;; Mark mappings

     (bg-mark-delete bg-yellow-subtle)
     (fg-mark-delete yellow)
     (bg-mark-select bg-cyan-subtle)
     (fg-mark-select cyan)
     (bg-mark-other bg-magenta-subtle)
     (fg-mark-other magenta-warmer)

     ;; Prompt mappings

     (fg-prompt blue)

     ;; Prose mappings

     (fg-prose-code cyan-cooler)
     (fg-prose-macro magenta-cooler)
     (fg-prose-verbatim yellow)
     (prose-done blue)
     (prose-todo yellow-warmer)
     (prose-metadata fg-dim)
     (prose-metadata-value fg-alt)
     (prose-table fg-alt)
     (prose-table-formula yellow-warmer)
     (prose-tag fg-alt)

     ;; Rainbow mappings

     (rainbow-0 yellow-warmer)
     (rainbow-1 blue)
     (rainbow-2 yellow-cooler)
     (rainbow-3 blue-warmer)
     (rainbow-4 yellow)
     (rainbow-5 cyan-warmer)
     (rainbow-6 yellow-faint)
     (rainbow-7 blue-faint)
     (rainbow-8 magenta-faint)

     ;; Search mappings

     (bg-search-current bg-yellow-intense)
     (bg-search-lazy bg-blue-intense)
     (bg-search-static bg-cyan-subtle)
     (bg-search-replace bg-yellow-intense)

     (bg-search-rx-group-0 bg-cyan-intense)
     (bg-search-rx-group-1 bg-magenta-intense)
     (bg-search-rx-group-2 bg-blue-subtle)
     (bg-search-rx-group-3 bg-yellow-subtle)

     ;; Heading mappings

     (fg-heading-0 cyan-cooler)
     (fg-heading-1 fg-main)
     (fg-heading-2 yellow-faint)
     (fg-heading-3 blue-faint)
     (fg-heading-4 green-faint)
     (fg-heading-5 magenta-cooler)
     (fg-heading-6 yellow-cooler)
     (fg-heading-7 cyan)
     (fg-heading-8 fg-dim))
   modus-themes-common-palette-mappings)
  "The entire palette of the `modus-vivendi-deuteranopia' theme.

Named colors have the form (COLOR-NAME HEX-VALUE) with the former
as a symbol and the latter as a string.

Semantic color mappings have the form (MAPPING-NAME COLOR-NAME)
with both as symbols.  The latter is a named color that already
exists in the palette and is associated with a HEX-VALUE.")

(defconst modus-themes-vivendi-tritanopia-palette
  (append
   '(
     ;; Basic values

     (bg-main          "#000000")
     (bg-dim           "#1e1e1e")
     (fg-main          "#ffffff")
     (fg-dim           "#989898")
     (fg-alt           "#a0d7f2")
     (bg-active        "#535353")
     (bg-inactive      "#303030")
     (border           "#646464")

     ;; Common accent foregrounds

     (red             "#ff5f59")
     (red-warmer      "#ff6740")
     (red-cooler      "#ff7f86")
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

     ;; Uncommon accent foregrounds

     (rust       "#db7b5f")
     (gold       "#c0965b")
     (olive      "#9cbd6f")
     (slate      "#76afbf")
     (indigo     "#9099d9")
     (maroon     "#cf7fa7")
     (pink       "#d09dc0")

     ;; Common accent backgrounds

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

     (bg-red-nuanced     "#3a0c14")
     (bg-green-nuanced   "#092f1f")
     (bg-yellow-nuanced  "#381d0f")
     (bg-blue-nuanced    "#12154a")
     (bg-magenta-nuanced "#2f0c3f")
     (bg-cyan-nuanced    "#042837")

     ;; Uncommon accent background and foreground pairs

     (bg-clay     "#49191a")
     (fg-clay     "#f1b090")

     (bg-ochre    "#462f20")
     (fg-ochre    "#e0d09c")

     (bg-lavender "#38325c")
     (fg-lavender "#dfc0f0")

     (bg-sage     "#143e32")
     (fg-sage     "#c3e7d4")

     ;; Graphs

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

     ;; Special purpose

     (bg-completion       "#004253")
     (bg-hover            "#8e3e3b")
     (bg-hover-secondary  "#204853")
     (bg-hl-line          "#2f3849")
     (bg-region           "#5a5a5a")
     (fg-region           "#ffffff")

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

     ;; Diffs

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

     ;; Paren match

     (bg-paren-match        "#2f7f9f")

     (bg-paren-expression   "#453040")

     ;; General mappings

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

     (bg-active-argument bg-red-nuanced)
     (fg-active-argument red-warmer)
     (bg-active-value bg-cyan-nuanced)
     (fg-active-value cyan)

     ;; Code mappings

     (builtin magenta)
     (comment red-faint)
     (constant green-faint)
     (docstring fg-alt)
     (fnname cyan-warmer)
     (fnname-call indigo)
     (keyword red-cooler)
     (preprocessor red-warmer)
     (property cyan-cooler)
     (rx-backslash magenta)
     (rx-construct red)
     (string cyan)
     (type blue-warmer)
     (variable cyan-cooler)
     (variable-use slate)

     ;; Accent mappings

     (accent-0 cyan)
     (accent-1 red-warmer)
     (accent-2 cyan-cooler)
     (accent-3 magenta)

     ;; Completion mappings

     (fg-completion-match-0 cyan)
     (fg-completion-match-1 red-warmer)
     (fg-completion-match-2 magenta)
     (fg-completion-match-3 cyan-cooler)

     ;; Date mappings

     (date-common cyan-cooler)
     (date-deadline red)
     (date-deadline-subtle red-faint)
     (date-event fg-alt)
     (date-holiday red-intense)
     (date-holiday-other cyan-warmer)
     (date-range fg-alt)
     (date-scheduled magenta)
     (date-scheduled-subtle magenta-faint)
     (date-weekday cyan)
     (date-weekend magenta-warmer)

     ;; Link mappings

     (fg-link cyan)
     (underline-link cyan)

     (fg-link-symbolic cyan-cooler)
     (underline-link-symbolic cyan-cooler)

     (fg-link-visited magenta)
     (underline-link-visited magenta)

     ;; Mail mappings

     (mail-cite-0 cyan-faint)
     (mail-cite-1 red-faint)
     (mail-cite-2 magenta-warmer)
     (mail-cite-3 cyan-warmer)
     (mail-part cyan-cooler)
     (mail-recipient cyan)
     (mail-subject red-cooler)
     (mail-other cyan)

     ;; Mark mappings

     (bg-mark-delete bg-red-subtle)
     (fg-mark-delete red)
     (bg-mark-select bg-cyan-subtle)
     (fg-mark-select cyan)
     (bg-mark-other bg-magenta-subtle)
     (fg-mark-other magenta-warmer)

     ;; Prompt mappings

     (fg-prompt cyan-cooler)

     ;; Prose mappings

     (fg-prose-code cyan)
     (fg-prose-macro red-warmer)
     (fg-prose-verbatim magenta-warmer)
     (prose-done cyan)
     (prose-todo red)
     (prose-metadata fg-dim)
     (prose-metadata-value fg-alt)
     (prose-table fg-alt)
     (prose-table-formula red-cooler)
     (prose-tag fg-alt)

     ;; Rainbow mappings

     (rainbow-0 cyan)
     (rainbow-1 red)
     (rainbow-2 cyan-warmer)
     (rainbow-3 red-cooler)
     (rainbow-4 cyan-cooler)
     (rainbow-5 magenta)
     (rainbow-6 cyan-faint)
     (rainbow-7 magenta-faint)
     (rainbow-8 red-faint)

     ;; Search mappings

     (bg-search-current bg-red-intense)
     (bg-search-lazy bg-cyan-intense)
     (bg-search-static bg-magenta-subtle)
     (bg-search-replace bg-magenta-intense)

     (bg-search-rx-group-0 bg-blue-intense)
     (bg-search-rx-group-1 bg-magenta-intense)
     (bg-search-rx-group-2 bg-cyan-subtle)
     (bg-search-rx-group-3 bg-red-subtle)

     ;; Heading mappings

     (fg-heading-0 cyan-cooler)
     (fg-heading-1 fg-main)
     (fg-heading-2 red-faint)
     (fg-heading-3 cyan-faint)
     (fg-heading-4 magenta)
     (fg-heading-5 green-faint)
     (fg-heading-6 magenta-faint)
     (fg-heading-7 cyan-faint)
     (fg-heading-8 fg-dim))
   modus-themes-common-palette-mappings)
  "The entire palette of the `modus-vivendi-tritanopia' theme.

Named colors have the form (COLOR-NAME HEX-VALUE) with the former
as a symbol and the latter as a string.

Semantic color mappings have the form (MAPPING-NAME COLOR-NAME)
with both as symbols.  The latter is a named color that already
exists in the palette and is associated with a HEX-VALUE.")



;;;; Presets of palette overrides

(defvar modus-themes-preset-overrides-faint
  '((bg-completion       bg-inactive)
    (bg-hl-line          bg-dim)
    (bg-paren-match      bg-cyan-subtle)
    (bg-region           bg-active)

    (bg-mode-line-active        bg-inactive)
    (border-mode-line-active    fg-dim)
    (bg-mode-line-inactive      bg-dim)
    (border-mode-line-inactive  bg-active)

    (bg-tab-bar      bg-inactive)
    (bg-tab-current  bg-main)
    (bg-tab-other    bg-active)

    (fringe unspecified)
    (builtin maroon)
    (comment fg-dim)
    (constant blue-faint)
    (docstring fg-alt)
    (docmarkup magenta-faint)
    (fnname pink)
    (keyword indigo)
    (preprocessor rust)
    (string slate)
    (type cyan-faint)
    (variable cyan-faint)
    (rx-construct gold)
    (rx-backslash olive)

    (underline-err red-faint)
    (underline-warning yellow-faint)
    (underline-note cyan-faint)

    (bg-button-active bg-main)
    (fg-button-active fg-main)
    (bg-button-inactive bg-inactive)
    (fg-button-inactive "gray50")

    (date-common cyan-faint)
    (date-deadline red-faint)
    (date-event fg-alt)
    (date-holiday magenta)
    (date-scheduled yellow-faint)
    (date-weekday fg-dim)
    (date-weekend fg-dim)

    (name maroon)
    (identifier fg-dim)

    (fg-line-number-active fg-main)
    (fg-line-number-inactive "gray50")
    (bg-line-number-active unspecified)
    (bg-line-number-inactive unspecified)

    (fg-link blue-faint)
    (bg-link unspecified)
    (underline-link bg-active)

    (fg-link-symbolic cyan-faint)
    (bg-link-symbolic unspecified)
    (underline-link-symbolic bg-active)

    (fg-link-visited magenta-faint)
    (bg-link-visited unspecified)
    (underline-link-visited bg-active)

    (mail-cite-0 cyan-faint)
    (mail-cite-1 yellow-faint)
    (mail-cite-2 green-faint)
    (mail-cite-3 red-faint)
    (mail-part olive)
    (mail-recipient indigo)
    (mail-subject maroon)
    (mail-other slate)

    (fg-prompt cyan-faint)

    (fg-prose-code olive)
    (fg-prose-macro indigo)
    (fg-prose-verbatim maroon)

    (prose-done green-faint)
    (prose-tag rust)
    (prose-todo red-faint)

    (rainbow-0 fg-main)
    (rainbow-1 magenta)
    (rainbow-2 cyan)
    (rainbow-3 red-faint)
    (rainbow-4 yellow-faint)
    (rainbow-5 magenta-cooler)
    (rainbow-6 green)
    (rainbow-7 blue-warmer)
    (rainbow-8 magenta-faint))
  "Preset for palette overrides with faint coloration.

This changes many parts of the theme to make them look less
colorful/intense.  Grays are toned down, gray backgrounds are
removed from some contexts, and almost all accent colors are
desaturated.

All the preset overrides the themes provide (including this one):

- `modus-themes-preset-overrides-faint'
- `modus-themes-preset-overrides-intense'
- `modus-themes-preset-overrides-cooler'
- `modus-themes-preset-overrides-warmer'

To set a preset, assign its symbol without a quote as the value
of the `modus-themes-common-palette-overrides' or as the value of
theme-specific options such as `modus-operandi-palette-overrides'.

For overriding named colors and/or semantic color mappings read
Info node `(modus-themes) Option for palette overrides'.")

(defvar modus-themes-preset-overrides-intense
  '((bg-region bg-cyan-intense)

    (bg-completion       bg-cyan-subtle)
    (bg-hover            bg-yellow-intense)
    (bg-hover-secondary  bg-magenta-intense)
    (bg-hl-line          bg-cyan-subtle)

    (bg-mode-line-active      bg-blue-subtle)
    (fg-mode-line-active      fg-main)
    (border-mode-line-active  blue-intense)

    (fringe bg-inactive)
    (comment red-faint)

    (date-common cyan)
    (date-deadline red)
    (date-event blue)
    (date-holiday magenta-warmer)
    (date-now blue-faint)
    (date-range blue)
    (date-scheduled yellow-warmer)
    (date-weekday fg-main)
    (date-weekend red-faint)

    (keybind blue-intense)

    (mail-cite-0 blue)
    (mail-cite-1 yellow-cooler)
    (mail-cite-2 green-warmer)
    (mail-cite-3 magenta)
    (mail-part cyan)
    (mail-recipient magenta-cooler)
    (mail-subject red-warmer)
    (mail-other cyan-cooler)

    (fg-prompt blue-intense)

    (bg-prose-block-delimiter bg-dim)
    (fg-prose-block-delimiter red-faint)
    (prose-done green-intense)
    (prose-metadata magenta-faint)
    (prose-metadata-value blue-cooler)
    (prose-table blue)
    (prose-todo red-intense)

    (fg-heading-0 blue-cooler)
    (fg-heading-1 magenta-cooler)
    (fg-heading-2 magenta-warmer)
    (fg-heading-3 blue)
    (fg-heading-4 cyan)
    (fg-heading-5 green-warmer)
    (fg-heading-6 yellow)
    (fg-heading-7 red)
    (fg-heading-8 magenta)

    (bg-heading-0 unspecified)
    (bg-heading-1 bg-magenta-nuanced)
    (bg-heading-2 bg-red-nuanced)
    (bg-heading-3 bg-blue-nuanced)
    (bg-heading-4 bg-cyan-nuanced)
    (bg-heading-5 bg-green-nuanced)
    (bg-heading-6 bg-yellow-nuanced)
    (bg-heading-7 bg-red-nuanced)
    (bg-heading-8 bg-magenta-nuanced)

    (overline-heading-0 unspecified)
    (overline-heading-1 magenta-cooler)
    (overline-heading-2 magenta-warmer)
    (overline-heading-3 blue)
    (overline-heading-4 cyan)
    (overline-heading-5 green)
    (overline-heading-6 yellow-cooler)
    (overline-heading-7 red-cooler)
    (overline-heading-8 magenta))
  "Preset for palette overrides with intense coloration.

This changes many parts of the theme to make them look more
colorful/intense.  Many background colors are accented and
coloration is increased to pop out more.

All the preset overrides the themes provide (including this one):

- `modus-themes-preset-overrides-faint'
- `modus-themes-preset-overrides-intense'
- `modus-themes-preset-overrides-cooler'
- `modus-themes-preset-overrides-warmer'

To set a preset, assign its symbol without a quote as the value
of the `modus-themes-common-palette-overrides' or as the value of
theme-specific options such as `modus-operandi-palette-overrides'.

For overriding named colors and/or semantic color mappings read
Info node `(modus-themes) Option for palette overrides'.")

(defvar modus-themes-preset-overrides-cooler
  '((fg-prompt blue-cooler)

    (builtin magenta-faint)
    (constant blue-cooler)
    (fnname cyan-cooler)
    (keyword magenta-cooler)
    (preprocessor blue)
    (string blue-warmer)
    (type green-cooler)
    (variable cyan)
    (rx-construct blue-cooler)
    (rx-backslash red)

    (name blue-warmer)
    (identifier magenta-faint)

    (date-deadline magenta-cooler)
    (date-scheduled yellow-cooler)
    (date-weekday blue-faint)
    (date-weekend red-faint)

    (mail-cite-0 blue-faint)
    (mail-cite-1 cyan-cooler)
    (mail-cite-2 magenta-faint)
    (mail-cite-3 yellow-cooler)
    (mail-part cyan)
    (mail-recipient blue-warmer)
    (mail-subject magenta-cooler)
    (mail-other blue)

    (prose-tag fg-dim)
    (fg-prose-verbatim blue-cooler))
  "Preset of palette overrides with cooler colors.

This changes parts of the palette to use more blue and
blue-tinted colors.

All the preset overrides the themes provide (including this one):

- `modus-themes-preset-overrides-faint'
- `modus-themes-preset-overrides-intense'
- `modus-themes-preset-overrides-cooler'
- `modus-themes-preset-overrides-warmer'

To set a preset, assign its symbol without a quote as the value
of the `modus-themes-common-palette-overrides' or as the value of
theme-specific options such as `modus-operandi-palette-overrides'.

For overriding named colors and/or semantic color mappings read
Info node `(modus-themes) Option for palette overrides'.")

(defvar modus-themes-preset-overrides-warmer
  '((fg-prompt magenta-warmer)

    (builtin magenta)
    (constant blue-warmer)
    (fnname magenta-cooler)
    (keyword magenta-warmer)
    (preprocessor red-cooler)
    (string green-warmer)
    (type cyan-cooler)
    (variable cyan)
    (rx-construct blue-cooler)
    (rx-backslash red-warmer)

    (name blue-warmer)
    (identifier magenta)
    (keybind magenta-warmer)

    (accent-0 magenta-warmer)
    (accent-1 cyan)
    (accent-2 blue-warmer)
    (accent-3 red-cooler)

    (date-common cyan-cooler)
    (date-holiday magenta-warmer)

    (mail-cite-0 magenta-faint)
    (mail-cite-1 cyan-cooler)
    (mail-cite-2 green-warmer)
    (mail-cite-3 red-faint)
    (mail-part cyan)
    (mail-recipient magenta)
    (mail-subject blue-warmer)
    (mail-other magenta-warmer)

    (fg-prose-macro red-cooler)
    (prose-tag fg-dim))
  "Preset of palette overrides with warmer colors.

This changes many parts of the theme to use warmer colors,
including green and yellow.

All the preset overrides the themes provide (including this one):

- `modus-themes-preset-overrides-faint'
- `modus-themes-preset-overrides-intense'
- `modus-themes-preset-overrides-cooler'
- `modus-themes-preset-overrides-warmer'

To set a preset, assign its symbol without a quote as the value
of the `modus-themes-common-palette-overrides' or as the value of
theme-specific options such as `modus-operandi-palette-overrides'.

For overriding named colors and/or semantic color mappings read
Info node `(modus-themes) Option for palette overrides'.")



;;;; Helper functions for theme setup

;; This is the WCAG formula: https://www.w3.org/TR/WCAG20-TECHS/G18.html
(defun modus-themes--wcag-contribution (channel weight)
  "Return the CHANNEL contribution to overall luminance given WEIGHT."
  (* weight
     (if (<= channel 0.03928)
         (/ channel 12.92)
       (expt (/ (+ channel 0.055) 1.055) 2.4))))

(defun modus-themes-wcag-formula (hex)
  "Get WCAG value of color value HEX.
The value is defined in hexadecimal RGB notation, such #123456."
  (let ((channels (color-name-to-rgb hex))
        (weights '(0.2126 0.7152 0.0722))
        contribution)
    (while channels
      (push (modus-themes--wcag-contribution (pop channels) (pop weights)) contribution))
    (apply #'+ contribution)))

;;;###autoload
(defun modus-themes-contrast (c1 c2)
  "Measure WCAG contrast ratio between C1 and C2.
C1 and C2 are color values written in hexadecimal RGB."
  (let ((ct (/ (+ (modus-themes-wcag-formula c1) 0.05)
               (+ (modus-themes-wcag-formula c2) 0.05))))
    (max ct (/ ct))))

(defvar modus-themes-registered-items nil
  "List of defined themes.
This list is instantiated by the `modus-themes-theme' macro.  Themes
that build on top of Modus but for some reason cannot use that macro
must define theme properties to include those that the macro specifies.

Also see `modus-themes-get-themes'.")

(defvar modus-themes--activated-themes nil
  "List of themes that `modus-themes-activate' operated on.")

;;;###autoload
(defun modus-themes-activate (theme)
  "Load THEME if neeeded, so that it can be used by other commands."
  ;; If it is already in the `modus-themes--activated-themes', then we
  ;; have already processed it.
  (unless (memq theme modus-themes--activated-themes)
    (let ((properties (get theme 'theme-properties)))
      ;; If it has no properties, then we need to load it so that
      ;; those are reified.
      (if (null properties)
          (progn
            (push theme modus-themes--activated-themes)
            (load-theme theme t t))
        (let ((core-palette (plist-get properties :modus-core-palette))
              (user-palette (plist-get properties :modus-user-palette)))
          ;; If its core palette is void or nil, then we need to load it.
          ;; Same if its user palette is void, but it is okay if that
          ;; one is nil.
          (unless (and (boundp core-palette)
                       core-palette
                       (boundp user-palette))
            (push theme modus-themes--activated-themes)
            (load-theme theme t t)))))))

(defun modus-themes--belongs-to-family-p (theme family)
  "Return non-nil if THEME has FAMILY property."
  (when-let* ((properties (get theme 'theme-properties))
              (theme-family (plist-get properties :family)))
    (eq theme-family family)))

(defun modus-themes-get-all-known-themes (&optional theme-family)
  "Return all known Modus themes or derivatives, enabling them if needed.
With optional THEME-FAMILY, operate only on the themes whose :family
property is that.  Else consider the Modus themes as well as all their
derivatives.

Also see `modus-themes-sort'."
  (let ((themes (pcase theme-family
                  ('modus-themes modus-themes-items)
                  ((pred identity) modus-themes-registered-items)
                  (_ (seq-union modus-themes-items modus-themes-registered-items)))))
    (if theme-family
        (seq-filter
         (lambda (theme)
           (modus-themes--belongs-to-family-p theme theme-family))
         themes)
      themes)))

(defun modus-themes--background-p (theme background-mode)
  "Return non-nil if THEME has BACKGROUND-MODE :background-mode property."
  (when-let* ((properties (get theme 'theme-properties))
              (background (plist-get properties :background-mode)))
    (eq background background-mode)))

(defun modus-themes-sort (themes background-mode)
  "Reorder THEMES so that those with BACKGROUND-MODE come first.
BACKGROUND-MODE is either `dark' or `light'."
  (unless (memq background-mode '(dark light))
    (error "The BACKGROUND-MODE can be either `dark' or `light'"))
  (sort
   themes
   (lambda (theme1 theme2)
     (and (modus-themes--background-p theme1 background-mode)
          (modus-themes--background-p theme2 (if (eq background-mode 'dark) 'light 'dark))))))

(cl-defgeneric modus-themes-get-themes ()
  "Return a list of all themes with `modus-themes' :family property.
Use `modus-themes-sort' to sort by light and then dark background."
  (if-let* ((themes (modus-themes-get-all-known-themes 'modus-themes))
            (sorted-themes (modus-themes-sort themes 'light)))
      sorted-themes
    modus-themes-items))

(defun modus-themes-known-p (themes)
  "Return THEMES if they are among `modus-themes-get-themes' else nil.
THEMES is either a list of symbols, like `modus-themes-items' or a
symbol.

With optional SHOW-ERROR, throw an error instead of returning nil."
  (let ((known-themes (modus-themes-get-all-known-themes)))
    (cond
     ((symbolp themes)
      (memq themes known-themes))
     ((listp themes)
      (when (seq-every-p (lambda (theme) (memq theme known-themes)) themes)
        themes))
     (t
      (error "Themes `%S' is not a symbol or a list of symbols" themes)))))

(defun modus-themes--modus-theme-p (theme)
  "Return non-nil if THEME has a :modus-core-palette property."
  (when-let* ((properties (get theme 'theme-properties))
              (core (plist-get properties :modus-core-palette)))
    theme))

(defun modus-themes-get-current-theme ()
  "Return currently enabled Modus theme.
More specifically, return the first of the currently enabled Modus
themes among the `custom-enabled-themes'.

Assume that a Modus theme has a `theme-properties' entry of
`:modus-core-palette'."
  (seq-find #'modus-themes--modus-theme-p custom-enabled-themes))

(defun modus-themes--get-theme-palette-subr (theme with-overrides with-user-palette)
  "Get THEME palette without `modus-themes-known-p'.
WITH-OVERRIDES and WITH-USER-PALETTE are described in
`modus-themes-get-theme-palette'.

If THEME does not have at least a `:modus-core-palette' among its
`theme-properties', return nil.

Else return (append OVERRIDES USER CORE)."
  (when-let* ((properties (get theme 'theme-properties))
              (core-palette (symbol-value (plist-get properties :modus-core-palette))))
    (let* ((user-palette (when with-user-palette (symbol-value (plist-get properties :modus-user-palette))))
           (overrides-palette (when with-overrides (symbol-value (plist-get properties :modus-overrides-palette))))
           (all-overrides (when with-overrides (append overrides-palette modus-themes-common-palette-overrides))))
      (append all-overrides user-palette core-palette))))

(defun modus-themes-get-theme-palette (&optional theme with-overrides with-user-palette)
  "Return palette value of active `modus-themes-get-themes' THEME.
If THEME is nil, use the return value of
`modus-themes-get-current-theme'.  With WITH-OVERRIDES, include all
overrides in the combined palette.  With WITH-USER-PALETTE do the same
for the user-defined palette extension.

If THEME is unknown, return nil.  Else return (append OVERRIDES USER CORE)."
  (modus-themes--get-theme-palette-subr (or theme (modus-themes-get-current-theme)) with-overrides with-user-palette))

(defun modus-themes--disable-themes (themes)
  "Disable THEMES per `modus-themes-disable-other-themes'."
  (mapc #'disable-theme
        (if modus-themes-disable-other-themes
            themes
          (seq-filter #'modus-themes--modus-theme-p themes))))

(defun modus-themes-load-theme (theme &optional hook)
  "Load THEME while disabling other themes.

Which themes are disabled is determined by the user option
`modus-themes-disable-other-themes'.

Run the `modus-themes-after-load-theme-hook' as the final step
after loading the THEME.  If HOOK, then call that instead.

Return THEME."
  (load-theme theme :no-confirm)
  (modus-themes--disable-themes (remq theme custom-enabled-themes))
  (run-hooks (or hook 'modus-themes-after-load-theme-hook))
  theme)

(defun modus-themes--retrieve-palette-value (color palette)
  "Return COLOR from PALETTE.
Use recursion until COLOR is retrieved as a string.  Refrain from
doing so if the value of COLOR is not a key in the PALETTE.

Return `unspecified' if the value of COLOR cannot be determined.
This symbol is accepted by faces and is thus harmless.

This function is used in the macros `modus-themes-theme',
`modus-themes-with-colors'."
  (let ((value (car (alist-get color palette))))
    (cond
     ((or (stringp value)
          (eq value 'unspecified))
      value)
     ((and (symbolp value)
           value)
      (modus-themes--retrieve-palette-value value palette))
     (t
      'unspecified))))

;; NOTE 2025-09-29: We keep THEME at that position for backward-compatibility.
(defun modus-themes-get-color-value (color &optional with-overrides theme)
  "Return color value of named COLOR for current Modus theme.

COLOR is a symbol that represents a named color entry in the
palette.

If the value is the name of another color entry in the
palette (so a mapping), recur until you find the underlying color
value.

With optional WITH-OVERRIDES as a non-nil value, include palette
overrides.  Else use the default palette.

With optional THEME among `modus-themes-get-themes', use the palette of
that item.  Else use the current theme.

If COLOR is not present in the palette, return the `unspecified'
symbol, which is safe when used as a face attribute's value."
  (when theme
    (modus-themes-activate theme))
  (if-let* ((palette (modus-themes-get-theme-palette theme with-overrides :with-user-palette))
            (value (modus-themes--retrieve-palette-value color palette)))
      value
    'unspecified))

;;;; Commands

;;;;; Select a theme with completion

(defvar modus-themes--select-theme-history nil
  "Minibuffer history of `modus-themes-select-prompt'.")

(defun modus-themes--annotate-theme (theme)
  "Return description of THEME ."
  (when-let* ((symbol (intern-soft theme))
              (properties (get symbol 'theme-properties))
              (doc-string (or (get symbol 'theme-documentation)
                              (plist-get properties :modus-documentation))))
    (format " %s"
            (propertize (concat "-- " (car (split-string doc-string "\\.")))
                        'face 'completions-annotations))))

(defun modus-themes--group-themes (theme transform)
  "Group THEME by its background for minibuffer completion.
If TRANSFORM is non-nil, return THEME as-is."
  (let ((symbol (intern-soft theme)))
    (cond
     (transform
      theme)
     ((eq symbol (modus-themes-get-current-theme))
      "Current")
     ((when-let* ((properties (get symbol 'theme-properties))
                  (background (plist-get properties :background-mode)))
        (capitalize (format "%s" background)))))))

(defun modus-themes--display-sort (themes)
  "Put the current theme before other THEMES for minibuffer completion."
  (let* ((current (modus-themes-get-current-theme))
         (current-theme-p (lambda (theme) (eq (intern-soft theme) current))))
    (nconc
     (seq-filter current-theme-p themes)
     (seq-remove current-theme-p themes))))

(defun modus-themes--completion-table (themes)
  "Pass appropriate metadata to THEMES for minibuffer completion."
  (lambda (string pred action)
    (if (eq action 'metadata)
        (list 'metadata
              (cons 'category 'theme)
              (cons 'annotation-function #'modus-themes--annotate-theme)
              (cons 'group-function #'modus-themes--group-themes)
              (cons 'display-sort-function #'modus-themes--display-sort))
      (complete-with-action action themes string pred))))

(defun modus-themes-select-prompt (&optional prompt background-mode)
  "Minibuffer prompt to select a Modus theme.
With optional PROMPT string, use it as the first argument of
`format-prompt'.  Else use a generic prompt.

With optional BACKGROUND-MODE as either `dark' or `light' limit the
themes accordingly."
  (intern
   (completing-read
    (format-prompt (or prompt "Select theme") nil)
    (modus-themes--completion-table
     (if background-mode
         (modus-themes-filter-by-background-mode
          (modus-themes-get-themes)
          background-mode)
       (modus-themes-get-themes)))
    nil t nil
    'modus-themes--select-theme-history)))

;;;###autoload
(defun modus-themes-select (theme)
  "Load a Modus THEME using minibuffer completion.
With optional prefix argument, prompt to limit the set of themes to
either dark or light variants.

Run `modus-themes-after-load-theme-hook' after loading the theme.
Disable other themes per `modus-themes-disable-other-themes'."
  (interactive
   (list
    (if current-prefix-arg
        (modus-themes-select-prompt nil (modus-themes-background-mode-prompt))
      (modus-themes-select-prompt))))
  (modus-themes-load-theme theme))

;;;###autoload
(defun modus-themes-select-dark (theme)
  "Like `modus-themes-select' for a dark THEME."
  (interactive (list (modus-themes-select-prompt nil 'dark)))
  (modus-themes-load-theme theme))

;;;###autoload
(defun modus-themes-select-light (theme)
  "Like `modus-themes-select' for a light THEME."
  (interactive (list (modus-themes-select-prompt nil 'light)))
  (modus-themes-load-theme theme))

;;;;; Toggle between two themes

;;;###autoload
(defun modus-themes-toggle ()
  "Toggle between the two `modus-themes-to-toggle'.
If `modus-themes-to-toggle' does not specify two Modus themes,
prompt with completion for a theme among our collection (this is
practically the same as the `modus-themes-select' command).

Run `modus-themes-after-load-theme-hook' after loading the theme.
Disable other themes per `modus-themes-disable-other-themes'."
  (declare (interactive-only t))
  (interactive)
  (if-let* ((themes (modus-themes-known-p modus-themes-to-toggle))
            (one (car themes))
            (two (cadr themes))
            (current (modus-themes-get-current-theme)))
      (modus-themes-load-theme (if (eq current one) two one))
    (modus-themes-load-theme (modus-themes-select-prompt "No valid theme to toggle; select other"))))

;;;;; Rotate through a list of themes

(defun modus-themes-rotate-subr (themes &optional reverse)
  "Return a new theme for `modus-themes-rotate'.
The theme is the next among THEMES if it is possible to rotate to it.
When REVERSE is non-nil, move to the left, else to the right."
  (if-let* ((valid-themes (modus-themes-known-p themes)))
      (if-let* ((index (or (seq-position valid-themes (modus-themes-get-current-theme)) -1))
                (offset (mod
                         (if reverse (1- index) (1+ index))
                         (length valid-themes)))
                (new-theme (nth offset valid-themes)))
          new-theme
        (modus-themes-select-prompt "Cannot determine next rotation; select other"))
    (modus-themes-select-prompt "No valid theme to rotate; select other")))

;;;###autoload
(defun modus-themes-rotate (themes &optional reverse)
  "Rotate to the next theme among THEMES.
When called interactively THEMES is the value of `modus-themes-to-rotate'
and REVERSE is the prefix argument.

If the current theme is already the next in line, then move to the one
after.  The rotation is performed rightwards if REVERSE is nil (the
default), and leftwards if REVERSE is non-nil.  Perform the rotation
such that the current element in the list becomes the last.  Do not
modify THEMES in the process."
  (interactive
   (list
    (or modus-themes-to-rotate (modus-themes-get-themes))
    current-prefix-arg))
  (let ((theme (modus-themes-rotate-subr themes reverse)))
    (message "Rotating to `%s'" theme)
    (modus-themes-load-theme theme)))

;;;;; Load a random theme

(defun modus-themes-filter-by-background-mode (themes background-mode)
  "Return list of THEMES by BACKGROUND-MODE property.
BACKGROUND-MODE is the symbol `dark' or `light'.  It corresponds to the
theme property of :background-mode.  Any other value means to use all
THEMES."
  (if background-mode
      (progn
        (unless (memq background-mode '(dark light))
          (error "The BACKGROUND-MODE can be either `dark' or `light'"))
        (seq-filter
         (lambda (theme)
           (modus-themes--background-p theme background-mode))
         themes))
    themes))

(defun modus-themes-background-mode-prompt ()
  "Select `dark' or `light' and return it as a symbol."
  (intern
   (cadr
    (read-multiple-choice
     "Variant"
     '((?d "dark" "Load a random dark theme")
       (?l "light" "Load a random light theme"))
     "Limit to the dark or light subset of the themes."))))

(defun modus-themes-load-random-subr (background-mode)
  "Return theme for `modus-themes-load-random' given BACKGROUND-MODE."
  (let* ((themes (modus-themes-filter-by-background-mode
                  (modus-themes-get-themes)
                  background-mode))
         (current (modus-themes-get-current-theme))
         (themes-minus-current (delete current (copy-sequence themes))))
    (or (nth (random (length themes-minus-current)) themes-minus-current)
        (car themes-minus-current))))

;;;###autoload
(defun modus-themes-load-random (&optional background-mode)
  "Load a Modus theme at random, excluding the current one.

With optional BACKGROUND-MODE as a prefix argument, prompt to limit the
set of themes to either dark or light variants.  When called from Lisp,
BACKGROUND-MODE is either the `dark' or `light' symbol.

Run `modus-themes-after-load-theme-hook' after loading a theme."
  (interactive
   (list
    (when current-prefix-arg
      (modus-themes-background-mode-prompt))))
  (if-let* ((theme (modus-themes-load-random-subr background-mode)))
      (progn
        (message "Loading `%s'" theme)
        (modus-themes-load-theme theme))
    (error "Could not find a theme to load at random")))

;;;###autoload
(defun modus-themes-load-random-dark ()
  "Load a random dark theme."
  (declare (interactive-only t))
  (interactive)
  (modus-themes-load-random 'dark))

;;;###autoload
(defun modus-themes-load-random-light ()
  "Load a random light theme."
  (declare (interactive-only t))
  (interactive)
  (modus-themes-load-random 'light))

;;;;; Preview a theme palette

(defun modus-themes--list-colors-get-mappings (palette)
  "Get the semantic palette entries in PALETTE.
PALETTE is the value of a variable like `modus-operandi-palette'."
  (seq-remove
   (lambda (cell)
     (stringp (cadr cell)))
   palette))

(defun modus-themes--list-colors-tabulated (theme &optional mappings)
  "Return a data structure of THEME palette or MAPPINGS for tabulated list."
  (let* ((current-palette (modus-themes-get-theme-palette theme :with-overrides :with-user-palette))
         (palette (if mappings
                      (modus-themes--list-colors-get-mappings current-palette)
                    current-palette)))
    (mapcar
     (lambda (entry)
       (pcase-let* ((`(,name ,value) entry)
                    (name-string (format "%s" name))
                    (value-string (format "%s" value))
                    (value-string-padded (format "%-30s" value-string))
                    (color (modus-themes-get-color-value name :with-overrides theme))) ; resolve a semantic mapping
         (list
          (cons entry color)
          (vector
           (pcase value
             ('unspecified "---")
             ((pred symbolp) "Yes")
             (_ ""))
           name-string
           (propertize value-string 'face `( :foreground ,color))
           (propertize value-string-padded 'face `( :background ,color
                                                    :foreground ,(if (string= color "unspecified")
                                                                     (readable-foreground-color (modus-themes-get-color-value 'bg-main nil theme))
                                                                   (readable-foreground-color color))))))))
     palette)))

(defvar modus-themes-current-preview nil)
(defvar modus-themes-current-preview-show-mappings nil)

(defun modus-themes--set-tabulated-entries ()
  "Set the value of `tabulated-list-entries' with palette entries."
  (setq-local tabulated-list-entries
              (modus-themes--list-colors-tabulated modus-themes-current-preview modus-themes-current-preview-show-mappings)))

(defun modus-themes-list-colors (theme &optional mappings)
  "Preview the palette of the Modus THEME of choice.
With optional prefix argument for MAPPINGS preview only the semantic
color mappings instead of the complete palette."
  (interactive
   (let ((prompt (if current-prefix-arg
                     "Preview palette mappings of THEME"
                   "Preview palette of THEME")))
     (list
      (modus-themes-select-prompt prompt)
      current-prefix-arg)))
  (modus-themes-activate theme)
  (let ((buffer (get-buffer-create (format (if mappings "*%s-list-mappings*" "*%s-list-all*") theme))))
    (with-current-buffer buffer
      (let ((modus-themes-current-preview theme)
            (modus-themes-current-preview-show-mappings mappings))
        (modus-themes-preview-mode)))
    (pop-to-buffer buffer)))

(defalias 'modus-themes-preview-colors 'modus-themes-list-colors
  "Alias for `modus-themes-list-colors'.")

(defun modus-themes-list-colors-current (&optional mappings)
  "Like `modus-themes-list-colors' with optional MAPPINGS for the current theme."
  (interactive "P")
  (modus-themes-list-colors (modus-themes-get-current-theme) mappings))

(defalias 'modus-themes-preview-colors-current 'modus-themes-list-colors-current
  "Alias for `modus-themes-list-colors-current'.")

(defvar-local modus-themes-preview-mode--marked-entries nil
  "List of entries marked in the `modus-themes-list-colors' buffer.")

(defun modus-themes-preview-mode-mark ()
  "Mark a palette entry in the `modus-themes-list-colors' buffer."
  (declare (interactive-only t))
  (interactive nil modus-themes-preview-mode)
  (unless (derived-mode-p 'modus-themes-preview-mode)
    (user-error "Only use this command inside the `modus-themes-preview-mode'"))
  (when-let* ((id-at-point (tabulated-list-get-id)))
    (add-to-list 'modus-themes-preview-mode--marked-entries id-at-point)
    (tabulated-list-put-tag "*" t)))

(defun modus-themes-preview-mode-mark-all ()
  "Mark all palette entries in the `modus-themes-list-colors' buffer."
  (declare (interactive-only t))
  (interactive nil modus-themes-preview-mode)
  (unless (derived-mode-p 'modus-themes-preview-mode)
    (user-error "Only use this command inside the `modus-themes-preview-mode'"))
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward (format "^\s\\{%d,\\}" tabulated-list-padding) nil t)
      (call-interactively 'modus-themes-preview-mode-mark))))

(defun modus-themes-preview-mode-unmark ()
  "Unmark a palette entry in the `modus-themes-list-colors' buffer."
  (declare (interactive-only t))
  (interactive nil modus-themes-preview-mode)
  (unless (derived-mode-p 'modus-themes-preview-mode)
    (user-error "Only use this command inside the `modus-themes-preview-mode'"))
  (when-let* ((id-at-point (tabulated-list-get-id)))
    (setq-local modus-themes-preview-mode--marked-entries (delq id-at-point modus-themes-preview-mode--marked-entries))
    (tabulated-list-put-tag " " t)))

(defun modus-themes-preview-mode-unmark-all ()
  "Unmark all palette entries in the `modus-themes-list-colors' buffer."
  (declare (interactive-only t))
  (interactive nil modus-themes-preview-mode)
  (unless (derived-mode-p 'modus-themes-preview-mode)
    (user-error "Only use this command inside the `modus-themes-preview-mode'"))
  (setq-local modus-themes-preview-mode--marked-entries nil)
  (tabulated-list-clear-all-tags))

(defun modus-themes-preview-mode-copy-entry ()
  "Copy marked entries or entry at point in the `modus-themes-list-colors' buffer.
Each entry is of the form that appears is the underlying palette.  This
is useful as a starting point for writing palette overrides.

Also see `modus-themes-preview-mode-copy-color'."
  (declare (interactive-only t))
  (interactive nil modus-themes-preview-mode)
  (unless (derived-mode-p 'modus-themes-preview-mode)
    (user-error "Only use this command inside the `modus-themes-preview-mode'"))
  (cond
   (modus-themes-preview-mode--marked-entries
    (let* ((entries (copy-sequence modus-themes-preview-mode--marked-entries))
           (colors (mapcar #'car (nreverse entries))))
      (kill-new (format "%S" colors))
      (message "Copied all marked entries: `%S'" colors)))
   ((when-let* ((color (car (tabulated-list-get-id)))
                (string (format "%S" color)))
      (kill-new string)
      (message "Copied palette entry: `%s'" (propertize string 'face 'success))))
   (t
    (user-error "Nothing to copy"))))

(defun modus-themes-preview-mode-copy-color ()
  "Copy marked colors or colors at point in the `modus-themes-list-colors' buffer.
Each color is a string.  Also see `modus-themes-preview-mode-copy-entry'."
  (declare (interactive-only t))
  (interactive nil modus-themes-preview-mode)
  (unless (derived-mode-p 'modus-themes-preview-mode)
    (user-error "Only use this command inside the `modus-themes-preview-mode'"))
  (cond
   (modus-themes-preview-mode--marked-entries
    (let* ((entries (copy-sequence modus-themes-preview-mode--marked-entries))
           (colors (mapcar #'cdr (nreverse entries))))
      (kill-new (format "%S" colors))
      (message "Copied all marked entries: `%S'" colors)))
   ((when-let* ((color (cdr (tabulated-list-get-id)))
                (string (format "%S" color)))
      (kill-new string)
      (message "Copied palette entry: `%s'" (propertize string 'face 'success))))
   (t
    (user-error "Nothing to copy"))))

(defvar modus-themes-preview-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "m") #'modus-themes-preview-mode-mark)
    (define-key map (kbd "M") #'modus-themes-preview-mode-mark-all)
    (define-key map (kbd "u") #'modus-themes-preview-mode-unmark)
    (define-key map (kbd "U") #'modus-themes-preview-mode-unmark-all)
    (define-key map (kbd "w") #'modus-themes-preview-mode-copy-color)
    (define-key map (kbd "W") #'modus-themes-preview-mode-copy-entry)
    map)
  "Key map for `modus-themes-preview-mode'.")

(define-derived-mode modus-themes-preview-mode tabulated-list-mode "Modus palette"
  "Major mode to display a Modus themes palette."
  :interactive nil
  (setq-local modus-themes-preview-mode--marked-entries nil)
  (setq-local tabulated-list-padding 2)
  (setq-local tabulated-list-format
              [("Mapping?" 10 t)
               ("Symbol name" 30 t)
               ("As foreground" 30 t)
               ("As background" 0 t)])
  (modus-themes--set-tabulated-entries)
  (tabulated-list-init-header)
  (tabulated-list-print))



;;;; Internal functions

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
                     (seq-filter (lambda (x) (funcall list-pred x)) properties)
                     default))
            ((listp val)))
      (car val)
    val))

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

(defun modus-themes--prompt (fg bg)
  "Conditional use of colors for text prompt faces.
FG is the prompt's standard foreground.  BG is a background
color that is combined with FG-FOR-BG."
  (let* ((properties (modus-themes--list-or-warn 'modus-themes-prompts))
         (weight (modus-themes--weight properties)))
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
          :background bg
          :foreground fg
          :weight
          ;; If we have `bold' specifically, we inherit the face of
          ;; the same name.  This allows the user to customise that
          ;; face, such as to change its font family.
          (if (and weight (not (eq weight 'bold)))
              weight
            'unspecified))))

(defconst modus-themes-weights
  '( thin ultralight extralight light semilight regular medium
     semibold bold heavy extrabold ultrabold)
  "List of font weights.")

(defun modus-themes--weight (list)
  "Search for `modus-themes-weights' weight in LIST."
  (catch 'found
    (dolist (elt list)
      (when (memq elt modus-themes-weights)
        (throw 'found elt)))))

(defun modus-themes--heading (level fg &optional bg ol)
  "Conditional styles for `modus-themes-headings'.

LEVEL is the heading's position in their order.  FG is the
default text color.  Optional BG is an appropriate background.
Optional OL is the color of an overline."
  (let* ((key (alist-get level modus-themes-headings))
         (style (or key (alist-get t modus-themes-headings)))
         (style-listp (listp style))
         (properties style)
         (var (when (and style-listp (memq 'variable-pitch properties)) 'variable-pitch))
         (weight (when style-listp (modus-themes--weight style))))
    (list :inherit (cond
                    ((not style-listp) 'bold)
                    ;; `no-bold' is for backward compatibility because we cannot
                    ;; deprecate a variable's value.
                    ((or weight (memq 'no-bold properties))
                     var)
                    (var (append (list 'bold) (list var)))
                    (t 'bold))
          :background (or bg 'unspecified)
          :foreground fg
          :overline (or ol 'unspecified)
          :height (if style-listp
                      (modus-themes--property-lookup properties 'height #'floatp 'unspecified)
                    'unspecified)
          :weight (or weight 'unspecified))))

(defun modus-themes--completion-line (bg)
  "Styles for `modus-themes-completions' with BG as the background."
  (let* ((var (modus-themes--list-or-warn 'modus-themes-completions))
         (properties (or (alist-get 'selection var) (alist-get t var)))
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
     :background bg
     :foreground 'unspecified
     :underline
     (if (memq 'underline properties) t 'unspecified)
     :weight
     (if (and weight (null bold)) weight 'unspecified))))

(defun modus-themes--completion-match (fg bg)
  "Styles for `modus-themes-completions'.
FG and BG are the main colors."
  (let* ((var (modus-themes--list-or-warn 'modus-themes-completions))
         (properties (or (alist-get 'matches var) (alist-get t var)))
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
     :background bg
     :foreground fg
     :underline
     (if (memq 'underline properties) t 'unspecified)
     :weight
     (if (and weight (null bold)) weight 'unspecified))))


;; NOTE 2025-11-23: In theory we need the `modus-themes--box'
;; equivalent for this:
;;
;;     :underline (:style wave :color unspecified)
;;
;; I checked all the relevant faces and feel that users will not be
;; benefitting form such a style anyway.  What would be the point of a
;; spell checker that cannot highlight its errors, for example?
;; Granted, we could have another kind of highlight, but I am here
;; focusing on the use of this:
;;
;;     :underline unspecified
(defun modus-themes--box (color width style)
  "Return :box COLOR, WIDTH, STYLE if appropriate.
If COLOR is unspecified, then return :box unspecified."
  (cond
   ((eq color 'unspecified)
    '(:box unspecified))
   ((and width style)
    `(:box (:line-width ,width :color ,color :style ,style)))
   (width
    `(:box (:line-width ,width :color ,color)))
   (style
    `(:box (:color ,color :style ,style)))
   (t
    `(:box ,color))))



;;;; Face specifications

(defconst modus-themes-faces
  '(
;;;; custom faces
    ;; these bespoke faces are inherited by other constructs below
;;;;; heading levels
    ;; styles for regular headings used in Org, Markdown, Info, etc.
    `(modus-themes-heading-0 ((,c ,@(modus-themes--heading 0 fg-heading-0 bg-heading-0 overline-heading-0))))
    `(modus-themes-heading-1 ((,c ,@(modus-themes--heading 1 fg-heading-1 bg-heading-1 overline-heading-1))))
    `(modus-themes-heading-2 ((,c ,@(modus-themes--heading 2 fg-heading-2 bg-heading-2 overline-heading-2))))
    `(modus-themes-heading-3 ((,c ,@(modus-themes--heading 3 fg-heading-3 bg-heading-3 overline-heading-3))))
    `(modus-themes-heading-4 ((,c ,@(modus-themes--heading 4 fg-heading-4 bg-heading-4 overline-heading-4))))
    `(modus-themes-heading-5 ((,c ,@(modus-themes--heading 5 fg-heading-5 bg-heading-5 overline-heading-5))))
    `(modus-themes-heading-6 ((,c ,@(modus-themes--heading 6 fg-heading-6 bg-heading-6 overline-heading-6))))
    `(modus-themes-heading-7 ((,c ,@(modus-themes--heading 7 fg-heading-7 bg-heading-7 overline-heading-7))))
    `(modus-themes-heading-8 ((,c ,@(modus-themes--heading 8 fg-heading-8 bg-heading-8 overline-heading-8))))
;;;;; completion frameworks
    `(modus-themes-completion-match-0 ((,c ,@(modus-themes--completion-match fg-completion-match-0 bg-completion-match-0))))
    `(modus-themes-completion-match-1 ((,c ,@(modus-themes--completion-match fg-completion-match-1 bg-completion-match-1))))
    `(modus-themes-completion-match-2 ((,c ,@(modus-themes--completion-match fg-completion-match-2 bg-completion-match-2))))
    `(modus-themes-completion-match-3 ((,c ,@(modus-themes--completion-match fg-completion-match-3 bg-completion-match-3))))
    `(modus-themes-completion-selected ((,c ,@(modus-themes--completion-line bg-completion))))
;;;;; typography
    `(modus-themes-bold ((,c ,@(modus-themes--bold-weight))))
    `(modus-themes-fixed-pitch ((,c ,@(modus-themes--fixed-pitch))))
    `(modus-themes-slant ((,c ,@(modus-themes--slant))))
    `(modus-themes-ui-variable-pitch ((,c ,@(modus-themes--variable-pitch-ui))))
;;;;; other custom faces
    `(modus-themes-button
      ((default :inherit variable-pitch :background ,bg-button-active :foreground ,fg-button-active)
       (((supports :box t))
        ,@(modus-themes--box border 1 'released-button))
       (t :underline ,border)))
    `(modus-themes-prompt ((,c ,@(modus-themes--prompt fg-prompt bg-prompt))))
    `(modus-themes-reset-soft ((,c :background ,bg-main :foreground ,fg-main
                                   :weight normal :slant normal :strike-through nil
                                   :box nil :underline nil :overline nil :extend nil)))
;;;; standard faces
;;;;; absolute essentials
    `(default ((,c :background ,bg-main :foreground ,fg-main)))
    `(bold ((,c :weight bold)))
    `(bold-italic ((,c :inherit (bold italic))))
    `(underline ((,c :underline ,fg-dim)))
    `(italic ((,c :slant italic)))
    `(cursor ((,c :background ,cursor)))
    `(fringe ((,c :background ,fringe :foreground ,fg-main)))
    `(scroll-bar ((,c :background ,fringe :foreground ,border)))
    `(tool-bar ((,c :background ,bg-dim :foreground ,fg-main)))
    `(vertical-border ((,c :foreground ,border)))
;;;;; basic and/or ungrouped styles
    `(abbrev-table-name ((,c :inherit modus-themes-heading-1)))
    `(appt-notification ((,c :inherit modus-themes-bold :foreground ,modeline-err)))
    `(blink-matching-paren-offscreen ((,c :background ,bg-paren-match)))
    `(buffer-menu-buffer ((,c :foreground ,name)))
    `(child-frame-border ((,c :background ,border)))
    `(comint-highlight-input ((,c :inherit modus-themes-bold)))
    `(comint-highlight-prompt ((,c :inherit modus-themes-prompt)))
    `(confusingly-reordered ((,c :underline (:style wave :color ,underline-err))))
    `(edmacro-label ((,c :inherit modus-themes-bold :foreground ,accent-0)))
    `(error ((,c :inherit modus-themes-bold :foreground ,err)))
    `(escape-glyph ((,c :inherit modus-themes-bold :foreground ,keybind)))
    `(file-name-shadow ((,c :foreground ,fg-dim)))
    `(header-line ((,c :inherit modus-themes-ui-variable-pitch :background ,bg-dim)))
    `(header-line-inactive ((,c :inherit modus-themes-ui-variable-pitch :background ,bg-dim :foreground ,fg-dim)))
    `(header-line-highlight ((,c :background ,bg-hover :foreground ,fg-main :box ,fg-main)))
    `(help-argument-name ((,c :inherit modus-themes-slant :foreground ,variable)))
    `(help-key-binding ((,c :inherit (bold modus-themes-fixed-pitch) :foreground ,keybind)))
    `(highlight ((,c :background ,bg-hover :foreground ,fg-main)))
    `(homoglyph ((,c :foreground ,warning)))
    `(ibuffer-locked-buffer ((,c :foreground ,warning)))
    `(icon-button ((,c :inherit modus-themes-button)))
    `(nobreak-hyphen ((,c :foreground ,err)))
    `(nobreak-space ((,c :foreground ,err :underline t)))
    `(menu ((,c :inverse-video unspecified :background ,bg-active :foreground ,fg-main)))
    `(minibuffer-prompt ((,c :inherit modus-themes-prompt)))
    `(minibuffer-nonselected ((,c :inverse-video t)))
    `(mm-command-output ((,c :foreground ,mail-part)))
    `(mm-uu-extract ((,c :foreground ,mail-part)))
    `(next-error ((,c :background ,bg-prominent-err :foreground ,fg-prominent-err)))
    `(pgtk-im-0 ((,c :background ,bg-prominent-note :foreground ,fg-prominent-note)))
    `(read-multiple-choice-face ((,c :inherit bold :background ,bg-mark-select :foreground ,fg-mark-select)))
    `(rectangle-preview ((,c :background ,bg-active :foreground ,fg-main)))
    `(region ((,c :background ,bg-region :foreground ,fg-region)))
    `(secondary-selection ((,c :background ,bg-hover-secondary :foreground ,fg-main)))
    `(separator-line ((,c :underline ,bg-active)))
    `(shadow ((,c :foreground ,fg-dim)))
    `(success ((,c :inherit modus-themes-bold :foreground ,info)))
    `(trailing-whitespace ((,c :background ,bg-space-err)))
    ;; NOTE 2024-06-22: I use `list' here to suppress a bogus warning
    ;; from the compiler: it says I should depend on Emacs 29 to use
    ;; vtable.
    (list 'vtable `((,c :inherit modus-themes-fixed-pitch)))
    `(warning ((,c :inherit modus-themes-bold :foreground ,warning)))
;;;;; buttons, links, widgets
    `(button ((,c :background ,bg-link :foreground ,fg-link :underline ,underline-link)))
    `(link ((,c :background ,bg-link :foreground ,fg-link :underline ,underline-link)))
    `(link-visited ((,c :background ,bg-link-visited :foreground ,fg-link-visited :underline ,underline-link-visited)))
    `(tooltip ((,c :background ,bg-active :foreground ,fg-main)))
;;;;; adoc-mode
    `(adoc-code-face ((,c :inherit modus-themes-fixed-pitch :background ,bg-prose-code :foreground ,fg-prose-code)))
    `(adoc-command-face ((,c :inherit modus-themes-fixed-pitch :background ,bg-prose-macro :foreground ,fg-prose-macro)))
    `(adoc-complex-replacement-face ((,c :inherit modus-themes-fixed-pitch :background ,bg-prose-macro :foreground ,fg-prose-macro)))
    `(adoc-emphasis-face ((,c :inherit bold)))
    `(adoc-gen-face ((,c :foreground ,fg-alt)))
    `(adoc-meta-face ((,c :inherit modus-themes-fixed-pitch :foreground ,prose-metadata)))
    `(adoc-meta-hide-face ((,c :inherit modus-themes-fixed-pitch :foreground ,prose-metadata)))
    `(adoc-replacement-face ((,c :foreground ,warning)))
    `(adoc-secondary-text-face ((,c :inherit modus-themes-fixed-pitch :foreground ,prose-metadata-value)))
    `(adoc-table-face ((,c :inherit modus-themes-fixed-pitch :foreground ,prose-table)))
    `(adoc-title-0-face ((,c :inherit modus-themes-heading-0)))
    `(adoc-title-1-face ((,c :inherit modus-themes-heading-1)))
    `(adoc-title-2-face ((,c :inherit modus-themes-heading-2)))
    `(adoc-title-3-face ((,c :inherit modus-themes-heading-3)))
    `(adoc-title-4-face ((,c :inherit modus-themes-heading-4)))
    `(adoc-title-5-face ((,c :inherit modus-themes-heading-5)))
    `(adoc-typewriter-face ((,c :inherit modus-themes-fixed-pitch :background ,bg-prose-verbatim :foreground ,fg-prose-verbatim)))
    `(adoc-verbatim-face ((,c :inherit modus-themes-fixed-pitch :background ,bg-prose-verbatim :foreground ,fg-prose-verbatim)))
;;;;; all-the-icons
    `(all-the-icons-blue ((,c :foreground ,blue-cooler)))
    `(all-the-icons-blue-alt ((,c :foreground ,blue-warmer)))
    `(all-the-icons-cyan ((,c :foreground ,cyan)))
    `(all-the-icons-cyan-alt ((,c :foreground ,cyan-warmer)))
    `(all-the-icons-dblue ((,c :foreground ,blue-faint)))
    `(all-the-icons-dcyan ((,c :foreground ,cyan-faint)))
    `(all-the-icons-dgreen ((,c :foreground ,green-faint)))
    `(all-the-icons-dmaroon ((,c :foreground ,magenta-faint)))
    `(all-the-icons-dorange ((,c :foreground ,red-faint)))
    `(all-the-icons-dpink ((,c :foreground ,magenta-faint)))
    `(all-the-icons-dpurple ((,c :foreground ,magenta-cooler)))
    `(all-the-icons-dred ((,c :foreground ,red)))
    `(all-the-icons-dsilver ((,c :foreground ,cyan-faint)))
    `(all-the-icons-dyellow ((,c :foreground ,yellow-faint)))
    `(all-the-icons-green ((,c :foreground ,green)))
    `(all-the-icons-lblue ((,c :foreground ,blue-cooler)))
    `(all-the-icons-lcyan ((,c :foreground ,cyan)))
    `(all-the-icons-lgreen ((,c :foreground ,green-warmer)))
    `(all-the-icons-lmaroon ((,c :foreground ,magenta-warmer)))
    `(all-the-icons-lorange ((,c :foreground ,red-warmer)))
    `(all-the-icons-lpink ((,c :foreground ,magenta)))
    `(all-the-icons-lpurple ((,c :foreground ,magenta-faint)))
    `(all-the-icons-lred ((,c :foreground ,red-faint)))
    `(all-the-icons-lsilver ((,c :foreground "gray50")))
    `(all-the-icons-lyellow ((,c :foreground ,yellow-warmer)))
    `(all-the-icons-maroon ((,c :foreground ,magenta)))
    `(all-the-icons-orange ((,c :foreground ,yellow-warmer)))
    `(all-the-icons-pink ((,c :foreground ,magenta-warmer)))
    `(all-the-icons-purple ((,c :foreground ,magenta-cooler)))
    `(all-the-icons-purple-alt ((,c :foreground ,blue-warmer)))
    `(all-the-icons-red ((,c :foreground ,red)))
    `(all-the-icons-red-alt ((,c :foreground ,red-cooler)))
    `(all-the-icons-silver ((,c :foreground "gray50")))
    `(all-the-icons-yellow ((,c :foreground ,yellow)))
;;;;; all-the-icons-dired
    `(all-the-icons-dired-dir-face ((,c :foreground ,accent-0)))
;;;;; all-the-icons-ibuffer
    `(all-the-icons-ibuffer-dir-face ((,c :foreground ,accent-0)))
    `(all-the-icons-ibuffer-file-face ((,c :foreground ,docstring)))
    `(all-the-icons-ibuffer-mode-face ((,c :foreground ,type)))
    `(all-the-icons-ibuffer-size-face ((,c :foreground ,variable)))
;;;;; annotate
    `(annotate-annotation ((,c :background ,bg-blue-subtle :foreground ,fg-main)))
    `(annotate-annotation-secondary ((,c :background ,bg-magenta-subtle :foreground ,fg-main)))
    `(annotate-highlight ((,c :background ,bg-blue-subtle :underline ,blue-intense)))
    `(annotate-highlight-secondary ((,c :background ,bg-magenta-subtle :underline ,magenta-intense)))
;;;;; ansi-color
    ;; Those are in Emacs28.
    `(ansi-color-black ((,c :background ,bg-term-black :foreground ,fg-term-black)))
    `(ansi-color-blue ((,c :background ,bg-term-blue :foreground ,fg-term-blue)))
    `(ansi-color-bold ((,c :inherit bold)))
    `(ansi-color-bright-black ((,c :background ,bg-term-black-bright :foreground ,fg-term-black-bright)))
    `(ansi-color-bright-blue ((,c :background ,bg-term-blue-bright :foreground ,fg-term-blue-bright)))
    `(ansi-color-bright-cyan ((,c :background ,bg-term-cyan-bright :foreground ,fg-term-cyan-bright)))
    `(ansi-color-bright-green ((,c :background ,bg-term-green-bright :foreground ,fg-term-green-bright)))
    `(ansi-color-bright-magenta ((,c :background ,bg-term-magenta-bright :foreground ,fg-term-magenta-bright)))
    `(ansi-color-bright-red ((,c :background ,bg-term-red-bright :foreground ,fg-term-red-bright)))
    `(ansi-color-bright-white ((,c :background ,bg-term-white-bright :foreground ,fg-term-white-bright)))
    `(ansi-color-bright-yellow ((,c :background ,bg-term-yellow-bright :foreground ,fg-term-yellow-bright)))
    `(ansi-color-cyan ((,c :background ,bg-term-cyan :foreground ,fg-term-cyan)))
    `(ansi-color-green ((,c :background ,bg-term-green :foreground ,fg-term-green)))
    `(ansi-color-magenta ((,c :background ,bg-term-magenta :foreground ,fg-term-magenta)))
    `(ansi-color-red ((,c :background ,bg-term-red :foreground ,fg-term-red)))
    `(ansi-color-white ((,c :background ,bg-term-white :foreground ,fg-term-white)))
    `(ansi-color-yellow ((,c :background ,bg-term-yellow :foreground ,fg-term-yellow)))
;;;;; anzu
    `(anzu-match-1 ((,c :background ,bg-search-rx-group-0 :foreground ,fg-search-rx-group-0)))
    `(anzu-match-2 ((,c :background ,bg-search-rx-group-1 :foreground ,fg-search-rx-group-1)))
    `(anzu-match-3 ((,c :background ,bg-search-rx-group-2 :foreground ,fg-search-rx-group-2)))
    `(anzu-mode-line ((,c modus-themes-bold bold)))
    `(anzu-mode-line-no-match ((,c :inherit modus-themes-bold :foreground ,modeline-err)))
    `(anzu-replace-highlight ((,c :background ,bg-search-replace :foreground ,fg-search-replace)))
    `(anzu-replace-to ((,c :background ,bg-search-current :foreground ,fg-search-current)))
;;;;; auctex and Tex
    `(font-latex-bold-face ((,c :inherit bold)))
    `(font-latex-doctex-documentation-face ((,c :inherit modus-themes-slant :foreground ,docstring)))
    `(font-latex-doctex-preprocessor-face ((,c :foreground ,preprocessor)))
    `(font-latex-italic-face ((,c :inherit italic)))
    `(font-latex-math-face ((,c :foreground ,constant)))
    `(font-latex-script-char-face ((,c :inherit modus-themes-bold :foreground ,builtin)))
    `(font-latex-sectioning-5-face ((,c :inherit modus-themes-bold :foreground ,fg-alt)))
    `(font-latex-sedate-face ((,c :inherit modus-themes-bold :foreground ,keyword)))
    `(font-latex-slide-title-face ((,c :inherit modus-themes-heading-1)))
    `(font-latex-string-face ((,c :foreground ,string)))
    `(font-latex-subscript-face ((,c :height 0.9)))
    `(font-latex-superscript-face ((,c :height 0.9)))
    `(font-latex-underline-face ((,c :inherit underline)))
    `(font-latex-verbatim-face ((,c :inherit modus-themes-fixed-pitch :background ,bg-prose-verbatim :foreground ,fg-prose-verbatim)))
    `(font-latex-warning-face ((,c :inherit modus-themes-bold :foreground ,warning)))
    `(tex-verbatim ((,c :inherit modus-themes-fixed-pitch :background ,bg-prose-verbatim :foreground ,fg-prose-verbatim)))
    ;; `(texinfo-heading ((,c :foreground ,magenta)))
    `(TeX-error-description-error ((,c :foreground ,err)))
    `(TeX-error-description-help ((,c :foreground ,info)))
    `(TeX-error-description-tex-said ((,c :foreground ,info)))
    `(TeX-error-description-warning ((,c :foreground ,warning)))
;;;;; auto-dim-other-buffers
    `(auto-dim-other-buffers-face ((,c :background ,bg-inactive)))
    `(auto-dim-other-buffers-hide-face ((,c :foreground ,bg-inactive :background ,bg-inactive)))
;;;;; avy
    `(avy-background-face ((,c :background ,bg-dim :foreground ,fg-dim :extend t)))
    `(avy-goto-char-timer-face ((,c :inherit (bold modus-themes-reset-soft) :background ,bg-search-lazy :foreground ,fg-search-lazy)))
    `(avy-lead-face ((,c :inherit (bold modus-themes-reset-soft) :background ,bg-search-current :foreground ,fg-search-current)))
    `(avy-lead-face-0 ((,c :inherit (bold modus-themes-reset-soft) :background ,bg-search-current :foreground ,fg-search-current)))
    `(avy-lead-face-1 ((,c :inherit (bold modus-themes-reset-soft) :background ,bg-search-current :foreground ,fg-search-current)))
    `(avy-lead-face-2 ((,c :inherit (bold modus-themes-reset-soft) :background ,bg-search-current :foreground ,fg-search-current)))
;;;;; aw (ace-window)
    `(aw-background-face ((,c :foreground "gray50")))
    `(aw-key-face ((,c :inherit (bold modus-themes-fixed-pitch) :foreground ,keybind)))
    `(aw-leading-char-face ((,c :inherit (bold modus-themes-reset-soft) :height 1.5 :foreground ,err))) ; same as `switch-window-label'
    `(aw-minibuffer-leading-char-face ((,c :inherit (bold modus-themes-fixed-pitch) :foreground ,keybind)))
    `(aw-mode-line-face ((,c :inherit modus-themes-bold)))
;;;;; binder
    `(binder-sidebar-highlight ((,c :inherit modus-themes-hl-line)))
    `(binder-sidebar-marked ((,c :inherit bold :background ,bg-mark-select :foreground ,fg-mark-select)))
    `(binder-sidebar-missing ((,c :inherit bold :background ,bg-mark-delete :foreground ,fg-mark-delete)))
    `(binder-sidebar-tags ((,c :foreground ,variable)))
;;;;; breadcrumb
    `(breadcrumb-face ((,c :foreground ,fg-alt)))
    `(breadcrumb-imenu-leaf-face ((,c :inherit modus-themes-bold :foreground ,modeline-info))) ; same as `which-func'
    `(breadcrumb-project-leaf-face ((,c :inherit modus-themes-bold)))
;;;;; bongo
    `(bongo-album-title (( )))
    `(bongo-artist ((,c :foreground ,accent-0)))
    `(bongo-currently-playing-track ((,c :inherit modus-themes-bold)))
    `(bongo-elapsed-track-part ((,c :background ,bg-inactive :underline t)))
    `(bongo-filled-seek-bar ((,c :background ,bg-hover)))
    `(bongo-marked-track ((,c :inherit bold :background ,bg-mark-other :foreground ,fg-mark-other)))
    `(bongo-marked-track-line ((,c :background ,bg-dim)))
    `(bongo-played-track ((,c :foreground ,fg-dim :strike-through t)))
    `(bongo-track-length ((,c :foreground ,fg-dim)))
    `(bongo-track-title ((,c :foreground ,accent-1)))
    `(bongo-unfilled-seek-bar ((,c :background ,bg-dim)))
;;;;; boon
    `(boon-modeline-cmd ((,c :background ,bg-blue-intense :foreground ,fg-main)))
    `(boon-modeline-ins ((,c :background ,bg-red-intense :foreground ,fg-main)))
    `(boon-modeline-off ((,c :background ,bg-yellow-intense :foreground ,fg-main)))
    `(boon-modeline-spc ((,c :background ,bg-green-intense :foreground ,fg-main)))
;;;;; bookmark
    `(bookmark-face ((,c :foreground ,info)))
    `(bookmark-menu-bookmark ((,c :foreground ,name)))
;;;;; calendar and diary
    `(calendar-month-header ((,c :inherit modus-themes-bold)))
    `(calendar-today
      ((default :foreground ,date-now :inverse-video t)
       (((supports :box t))
        ,@(modus-themes--box date-now '(-1 . -1) nil))))
    `(calendar-weekday-header ((,c :foreground ,date-weekday)))
    `(calendar-weekend-header ((,c :foreground ,date-weekend)))
    `(diary ((,c :foreground ,date-common)))
    `(diary-anniversary ((,c :foreground ,date-holiday)))
    `(diary-time ((,c :foreground ,date-common)))
    `(holiday ((,c :foreground ,date-holiday)))
;;;;; calibredb
    ;; NOTE 2022-12-27: Calibredb needs to be reviewed.  I had to
    ;; change the applicable colors for the transition to
    ;; modus-themes version 4, but I cannot test this currently (it
    ;; depends on an external program).
    `(calibredb-archive-face ((,c :foreground ,accent-3)))
    `(calibredb-author-face ((,c :foreground ,name)))
    `(calibredb-comment-face ((,c :foreground ,comment)))
    `(calibredb-date-face ((,c :foreground ,date-common)))
    `(calibredb-edit-annotation-header-title-face ((,c :inherit modus-themes-bold)))
    `(calibredb-favorite-face ((,c :inherit modus-themes-bold)))
    `(calibredb-file-face (( )))
    `(calibredb-format-face ((,c :foreground ,fg-alt)))
    `(calibredb-highlight-face ((,c :foreground ,info)))
    `(calibredb-id-face (( )))
    `(calibredb-ids-face (( )))
    `(calibredb-search-header-highlight-face ((,c :background ,bg-hl-line :extend t)))
    `(calibredb-search-header-library-name-face ((,c :foreground ,accent-2)))
    `(calibredb-search-header-library-path-face ((,c :inherit modus-themes-bold)))
    `(calibredb-search-header-sort-face ((,c :inherit modus-themes-bold :foreground ,accent-1)))
    `(calibredb-search-header-total-face ((,c :inherit modus-themes-bold :foreground ,accent-0)))
    `(calibredb-search-header-filter-face ((,c :inherit modus-themes-bold)))
    `(calibredb-mark-face ((,c :inherit bold :background ,bg-mark-select :foreground ,fg-mark-select)))
    `(calibredb-size-face (( )))
    `(calibredb-tag-face ((,c :foreground ,fg-alt)))
;;;;; centaur-tabs
    `(centaur-tabs-active-bar-face ((,c :background ,blue))) ; special case like `doom-modeline-bar'
    `(centaur-tabs-close-mouse-face ((,c :inherit modus-themes-bold :foreground ,warning :underline t)))
    `(centaur-tabs-close-selected ((,c :inherit modus-themes-bold :background ,bg-tab-current ,@(modus-themes--box bg-tab-current -2 nil))))
    `(centaur-tabs-close-unselected ((,c :inherit modus-themes-bold :background ,bg-tab-other ,@(modus-themes--box bg-tab-other -2 nil))))
    `(centaur-tabs-modified-marker-selected ((,c :inherit modus-themes-bold :background ,bg-tab-current ,@(modus-themes--box bg-tab-current -2 nil))))
    `(centaur-tabs-modified-marker-unselected ((,c :background ,bg-tab-other ,@(modus-themes--box bg-tab-other -2 nil))))
    `(centaur-tabs-default ((,c :background ,bg-main)))
    `(centaur-tabs-selected ((,c :inherit modus-themes-bold :background ,bg-tab-current ,@(modus-themes--box bg-tab-current -2 nil))))
    `(centaur-tabs-selected-modified ((,c :inherit modus-themes-slant :background ,bg-tab-current ,@(modus-themes--box bg-tab-current -2 nil))))
    `(centaur-tabs-unselected ((,c :background ,bg-tab-other ,@(modus-themes--box bg-tab-other -2 nil))))
    `(centaur-tabs-unselected-modified ((,c :inherit modus-themes-slant :background ,bg-tab-other ,@(modus-themes--box bg-tab-other -2 nil))))
;;;;; change-log and log-view (`vc-print-log' and `vc-print-root-log')
    `(change-log-acknowledgment ((,c :foreground ,identifier)))
    `(change-log-conditionals ((,c :foreground ,err)))
    `(change-log-date ((,c :foreground ,date-common)))
    `(change-log-email ((,c :foreground ,fg-alt)))
    `(change-log-file ((,c :inherit modus-themes-bold)))
    `(change-log-function ((,c :foreground ,warning)))
    `(change-log-list ((,c :inherit modus-themes-bold)))
    `(change-log-name ((,c :foreground ,name)))
    `(log-edit-header ((,c :inherit modus-themes-bold)))
    `(log-edit-headers-separator ((,c :height 1 :background ,border :extend t)))
    `(log-edit-summary ((,c :inherit modus-themes-bold :foreground ,fg-alt)))
    `(log-edit-unknown-header ((,c :foreground ,fg-dim)))
    `(log-view-commit-body (( )))
    `(log-view-file ((,c :inherit modus-themes-bold)))
    `(log-view-message ((,c :foreground ,identifier)))
;;;;; cider
    `(cider-deprecated-face ((,c :foreground ,warning)))
    `(cider-enlightened-face ((,c :box ,warning)))
    `(cider-enlightened-local-face ((,c :foreground ,warning)))
    `(cider-error-highlight-face ((,c :underline (:style wave :color ,underline-err))))
    `(cider-fringe-good-face ((,c :foreground ,info)))
    `(cider-instrumented-face ((,c :box ,err)))
    `(cider-reader-conditional-face ((,c :inherit modus-themes-bold :foreground ,type)))
    `(cider-repl-prompt-face ((,c :inherit modus-themes-prompt)))
    `(cider-repl-stderr-face ((,c :foreground ,err)))
    `(cider-repl-stdout-face (( )))
    `(cider-warning-highlight-face ((,c :underline (:style wave :color ,underline-warning))))
;;;;; circe (and lui)
    `(circe-fool-face ((,c :foreground ,fg-dim)))
    `(circe-highlight-nick-face ((,c :foreground ,err)))
    `(circe-prompt-face ((,c :inherit modus-themes-prompt)))
    `(circe-server-face ((,c :foreground ,fg-dim)))
    `(lui-button-face ((,c :background ,bg-link :foreground ,fg-link :underline ,underline-link)))
    `(lui-highlight-face ((,c :foreground ,err)))
    `(lui-time-stamp-face ((,c :foreground ,date-common)))
;;;;; citar
    `(citar ((,c :foreground ,fg-dim)))
    `(citar-highlight (( )))
;;;;; clojure-mode
    `(clojure-keyword-face ((,c :inherit modus-themes-bold :foreground ,keyword)))
;;;;; column-enforce-mode
    `(column-enforce-face ((,c :background ,bg-prominent-err :foreground ,fg-prominent-err)))
;;;;; company-mode
    `(company-echo-common ((,c :inherit modus-themes-completion-match-0)))
    `(company-preview ((,c :background ,bg-dim :foreground ,fg-dim)))
    `(company-preview-common ((,c :inherit modus-themes-completion-match-0)))
    `(company-preview-search ((,c :background ,bg-yellow-intense)))
    `(company-scrollbar-bg ((,c :background ,bg-active)))
    `(company-scrollbar-fg ((,c :background ,fg-main)))
    `(company-template-field ((,c :background ,bg-active)))
    `(company-tooltip ((,c :inherit modus-themes-fixed-pitch :background ,bg-dim)))
    `(company-tooltip-annotation ((,c :inherit modus-themes-slant :foreground ,docstring)))
    `(company-tooltip-common ((,c :inherit modus-themes-completion-match-0)))
    `(company-tooltip-deprecated ((,c :inherit modus-themes-fixed-pitch :background ,bg-dim :strike-through t)))
    `(company-tooltip-mouse ((,c :background ,bg-hover :foreground ,fg-main)))
    `(company-tooltip-scrollbar-thumb ((,c :background ,fg-alt)))
    `(company-tooltip-scrollbar-track ((,c :background ,bg-inactive)))
    `(company-tooltip-search ((,c :background ,bg-hover-secondary :foreground ,fg-main)))
    `(company-tooltip-search-selection ((,c :background ,bg-hover-secondary :foreground ,fg-main :underline t)))
    `(company-tooltip-selection ((,c :inherit modus-themes-completion-selected)))
;;;;; compilation
    `(compilation-column-number ((,c :foreground ,fg-dim)))
    `(compilation-error ((,c :inherit modus-themes-bold :foreground ,err)))
    `(compilation-info ((,c :inherit modus-themes-bold :foreground ,info)))
    `(compilation-line-number ((,c :foreground ,fg-dim)))
    `(compilation-mode-line-exit ((,c :inherit modus-themes-bold)))
    `(compilation-mode-line-fail ((,c :inherit modus-themes-bold :foreground ,modeline-err)))
    `(compilation-mode-line-run ((,c :inherit modus-themes-bold :foreground ,modeline-warning)))
    `(compilation-warning ((,c :inherit modus-themes-bold :foreground ,warning)))
;;;;; completion-preview
    `(completion-preview ((,c :foreground ,fg-dim)))
    ;; We set the following faces to inherit from `completion-preview',
    ;; as they do by default.  If we ever want them not to inherit from
    ;; `completion-preview', then we should remember to customize
    ;; `completion-preview-adapt-background-color' accordingly.
    `(completion-preview-common ((,c :inherit completion-preview :underline t)))
    `(completion-preview-exact ((,c :inherit (modus-themes-completion-match-0 completion-preview))))
;;;;; completions
    `(completions-annotations ((,c :inherit modus-themes-slant :foreground ,docstring)))
    `(completions-common-part ((,c :inherit modus-themes-completion-match-0)))
    `(completions-first-difference ((,c :inherit modus-themes-completion-match-1)))
    `(completions-highlight ((,c :inherit modus-themes-completion-selected)))
;;;;; consult
    `(consult-async-split ((,c :foreground ,err)))
    `(consult-file ((,c :inherit modus-themes-bold :foreground ,info)))
    `(consult-highlight-mark ((,c :background ,bg-search-static :foreground ,fg-search-static)))
    `(consult-highlight-match ((,c :background ,bg-search-static :foreground ,fg-search-static)))
    `(consult-imenu-prefix ((,c :foreground ,fg-dim)))
    `(consult-key ((,c :inherit (bold modus-themes-fixed-pitch) :foreground ,keybind)))
    `(consult-line-number ((,c :foreground ,fg-dim)))
    `(consult-line-number-prefix ((,c :foreground ,fg-dim)))
    `(consult-preview-insertion ((,c :background ,bg-dim)))
;;;;; corfu
    `(corfu-current ((,c :inherit modus-themes-completion-selected)))
    `(corfu-bar ((,c :background ,fg-dim)))
    `(corfu-border ((,c :background ,bg-active)))
    `(corfu-default ((,c :inherit modus-themes-fixed-pitch :background ,bg-dim)))
;;;;; corfu-candidate-overlay
    `(corfu-candidate-overlay-face ((,c :foreground ,fg-dim)))
;;;;; corfu-quick
    `(corfu-quick1 ((,c :inherit bold :background ,bg-search-current :foreground ,fg-search-current)))
    `(corfu-quick2 ((,c :inherit bold :background ,bg-search-current :foreground ,fg-search-current)))
;;;;; counsel
    `(counsel-active-mode ((,c :foreground ,keyword)))
    `(counsel-application-name ((,c :foreground ,name)))
    `(counsel-key-binding ((,c :inherit (bold modus-themes-fixed-pitch) :foreground ,keybind)))
    `(counsel-outline-default ((,c :foreground ,fg-main)))
    `(counsel-variable-documentation ((,c :inherit modus-themes-slant :foreground ,docstring)))
;;;;; cperl-mode
    `(cperl-nonoverridable-face ((,c :foreground unspecified)))
    `(cperl-array-face ((,c :inherit modus-themes-bold :foreground ,keyword)))
    `(cperl-hash-face ((,c :foreground ,variable)))
;;;;; crontab-mode
    `(crontab-minute ((,c :foreground ,string)))
    `(crontab-hour ((,c :foreground ,keyword)))
    `(crontab-month-day ((,c :foreground ,builtin)))
    `(crontab-month ((,c :foreground ,constant)))
    `(crontab-week-day ((,c :foreground ,variable)))
    `(crontab-predefined ((,c :foreground ,string)))
;;;;; csv-mode
    `(csv-separator-face (( )))
;;;;; ctrlf
    `(ctrlf-highlight-active ((,c :background ,bg-search-current :foreground ,fg-search-current)))
    `(ctrlf-highlight-line ((,c :background ,bg-hl-line :extend t)))
    `(ctrlf-highlight-passive ((,c :background ,bg-search-lazy :foreground ,fg-search-lazy)))
;;;;; custom (M-x customize)
    `(custom-button ((,c :inherit modus-themes-button)))
    `(custom-button-mouse
      ((default :inherit variable-pitch :background ,bg-hover :foreground ,fg-main)
       (((supports :box t))
        ,@(modus-themes--box border 1 'released-button))
       (t :underline ,border)))
    `(custom-button-pressed
      ((default :inherit variable-pitch :background ,bg-main :foreground ,fg-main)
       (((supports :box t))
        ,@(modus-themes--box border 1 'pressed-button))
       (t :underline ,border)))
    `(custom-changed ((,c :background ,bg-changed)))
    `(custom-comment ((,c :foreground ,comment)))
    `(custom-comment-tag ((,c :inherit modus-themes-bold :foreground ,comment)))
    `(custom-invalid ((,c :foreground ,err :strike-through t)))
    `(custom-modified ((,c :background ,bg-changed)))
    `(custom-rogue ((,c :foreground ,err :strike-through t)))
    `(custom-set ((,c :foreground ,info)))
    `(custom-state ((,c :foreground ,warning)))
    `(custom-themed ((,c :background ,bg-changed)))
    `(custom-variable-obsolete ((,c :foreground ,fg-dim)))
    `(custom-face-tag ((,c :inherit modus-themes-bold :foreground ,type)))
    `(custom-group-tag ((,c :inherit modus-themes-bold :foreground ,builtin)))
    `(custom-group-tag-1 ((,c :inherit modus-themes-bold :foreground ,constant)))
    `(custom-variable-tag ((,c :inherit modus-themes-bold :foreground ,variable)))
;;;;; dashboard
    `(dashboard-heading ((,c :foreground ,name)))
    `(dashboard-items-face (( ))) ; use the underlying style of all-the-icons
;;;;; deadgrep
    `(deadgrep-filename-face ((,c :inherit modus-themes-bold :foreground ,name)))
    `(deadgrep-match-face ((,c :background ,bg-search-static :foreground ,fg-search-static)))
    `(deadgrep-meta-face ((,c :foreground ,fg-dim)))
    `(deadgrep-regexp-metachar-face ((,c :inherit modus-themes-bold :foreground ,rx-construct)))
    `(deadgrep-search-term-face ((,c :foreground ,info)))
;;;;; debbugs
    `(debbugs-gnu-archived ((,c :background ,bg-inactive :foreground ,fg-dim)))
    `(debbugs-gnu-done ((,c :foreground ,info)))
    `(debbugs-gnu-forwarded ((,c :inherit modus-themes-slant)))
    `(debbugs-gnu-handled (( )))
    `(debbugs-gnu-marked ((,c :inherit bold :background ,bg-mark-select :foreground ,fg-mark-select)))
    `(debbugs-gnu-marked-stale ((,c :inherit bold :background ,bg-mark-other :foreground ,fg-mark-other)))
    `(debbugs-gnu-new ((,c :inherit modus-themes-bold :foreground ,err)))
    `(debbugs-gnu-pending ((,c :inherit bold-italic :foreground ,warning)))
    `(debbugs-gnu-stale-1 ((,c :foreground ,err)))
    `(debbugs-gnu-stale-2 ((,c :foreground ,warning)))
    `(debbugs-gnu-stale-3 ((,c :foreground ,info)))
    `(debbugs-gnu-stale-4 ((,c :foreground ,fg-alt)))
    `(debbugs-gnu-stale-5 ((,c :foreground ,fg-dim)))
    `(debbugs-gnu-tagged ((,c :inherit bold :background ,bg-mark-other :foreground ,fg-mark-other)))
    `(debbugs-gnu-title ((,c :inherit modus-themes-heading-1)))
;;;;; deft
    `(deft-filter-string-face ((,c :foreground ,info)))
    `(deft-header-face ((,c :foreground ,fg-dim)))
    `(deft-separator-face ((,c :foreground ,border)))
    `(deft-summary-face ((,c :inherit modus-themes-slant :foreground ,fg-dim)))
    `(deft-time-face ((,c :foreground ,date-common)))
    `(deft-title-face ((,c :inherit modus-themes-heading-1)))
;;;;; denote
    `(denote-faces-date ((,c :foreground ,date-common)))
    `(denote-faces-delimiter ((,c :foreground ,fg-dim)))
    `(denote-faces-extension ((,c :foreground ,fg-dim)))
    `(denote-faces-keywords ((,c :inherit modus-themes-bold :foreground ,keyword)))
    `(denote-faces-link ((,c :background ,bg-link :foreground ,fg-link :underline ,underline-link)))
    `(denote-faces-prompt-current-name ((,c :inherit modus-themes-slant :foreground ,fg-changed-intense)))
    `(denote-faces-prompt-new-name ((,c :inherit modus-themes-slant :foreground ,fg-added-intense)))
    `(denote-faces-prompt-old-name ((,c :inherit modus-themes-slant :foreground ,fg-removed-intense)))
    `(denote-faces-signature ((,c :inherit modus-themes-bold :foreground ,string)))
    `(denote-faces-subdirectory ((,c :inherit modus-themes-bold :foreground ,fg-alt)))
    `(denote-faces-time ((,c :foreground ,date-common)))
    `(denote-faces-time-delimiter ((,c :foreground ,fg-dim)))
    `(denote-faces-title (( )))
;;;;; devdocs
    `(devdocs-code-block ((,c :inherit modus-themes-fixed-pitch :background ,bg-dim :extend t)))
;;;;; dictionary
    `(dictionary-button-face ((,c :inherit modus-themes-bold)))
    `(dictionary-reference-face ((,c :background ,bg-link :foreground ,fg-link :underline ,underline-link)))
    `(dictionary-word-definition-face (( )))
    `(dictionary-word-entry-face ((,c :inherit modus-themes-slant :foreground ,comment)))
;;;;; diff-hl
    `(diff-hl-change ((,c :background ,bg-changed-fringe)))
    `(diff-hl-delete ((,c :background ,bg-removed-fringe)))
    `(diff-hl-insert ((,c :background ,bg-added-fringe)))
    `(diff-hl-reverted-hunk-highlight ((,c :background ,fg-main :foreground ,bg-main)))
;;;;; diff-mode
    `(diff-added ((,c :background ,bg-added :foreground ,fg-added :extend t)))
    `(diff-changed ((,c :background ,bg-changed :foreground ,fg-changed :extend t)))
    `(diff-changed-unspecified ((,c :background ,bg-changed :foreground ,fg-changed :extend t)))
    `(diff-removed ((,c :background ,bg-removed :foreground ,fg-removed :extend t)))
    `(diff-refine-added ((,c :background ,bg-added-refine :foreground ,fg-added)))
    `(diff-refine-changed ((,c :background ,bg-changed-refine :foreground ,fg-changed)))
    `(diff-refine-removed ((,c :background ,bg-removed-refine :foreground ,fg-removed)))
    `(diff-indicator-added ((,c :background ,bg-added :foreground ,fg-added-intense)))
    `(diff-indicator-changed ((,c :background ,bg-changed :foreground ,fg-changed-intense)))
    `(diff-indicator-removed ((,c :background ,bg-removed :foreground ,fg-removed-intense)))
    `(diff-context (( )))
    `(diff-error ((,c :foreground ,err)))
    `(diff-file-header ((,c :inherit modus-themes-bold)))
    `(diff-function ((,c :background ,bg-inactive)))
    `(diff-header (( )))
    `(diff-hunk-header ((,c :inherit modus-themes-bold :background ,bg-inactive)))
    `(diff-index ((,c :inherit modus-themes-slant)))
    `(diff-nonexistent ((,c :inherit modus-themes-bold)))
;;;;; dim-autoload
    `(dim-autoload-cookie-line ((,c :inherit modus-themes-slant :foreground ,comment)))
;;;;; dired
    `(dired-broken-symlink ((,c :foreground ,err :underline t)))
    `(dired-directory ((,c :foreground ,accent-0)))
    `(dired-flagged ((,c :inherit bold :background ,bg-mark-delete :foreground ,fg-mark-delete)))
    `(dired-header ((,c :inherit modus-themes-bold)))
    `(dired-ignored ((,c :foreground ,fg-dim)))
    `(dired-mark ((,c :inherit modus-themes-bold)))
    `(dired-marked ((,c :inherit bold :background ,bg-mark-select :foreground ,fg-mark-select)))
    `(dired-perm-write ((,c :foreground ,fg-dim)))
    `(dired-symlink ((,c :background ,bg-link-symbolic :foreground ,fg-link-symbolic :underline ,underline-link-symbolic)))
    `(dired-warning ((,c :foreground ,warning)))
;;;;; dired-async
    `(dired-async-failures ((,c :foreground ,err)))
    `(dired-async-message ((,c :inherit modus-themes-bold)))
    `(dired-async-mode-message ((,c :inherit modus-themes-bold)))
;;;;; dired-git
    `(dired-git-branch-else ((,c :inherit modus-themes-bold :foreground ,accent-0)))
    `(dired-git-branch-master ((,c :inherit modus-themes-bold :foreground ,accent-1)))
;;;;; dired-git-info
    `(dgi-commit-message-face ((,c :foreground ,docstring)))
;;;;; dired-narrow
    `(dired-narrow-blink ((,c :background ,bg-prominent-warning :foreground ,fg-prominent-warning)))
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
    `(diredfl-autofile-name ((,c :background ,bg-inactive)))
    `(diredfl-compressed-file-name ((,c :foreground ,warning)))
    `(diredfl-compressed-file-suffix ((,c :foreground ,err)))
    `(diredfl-date-time ((,c :foreground ,date-common)))
    `(diredfl-deletion ((,c :inherit bold :background ,bg-mark-delete :foreground ,fg-mark-delete)))
    `(diredfl-deletion-file-name ((,c :inherit bold :background ,bg-mark-delete :foreground ,fg-mark-delete)))
    `(diredfl-dir-heading ((,c :inherit modus-themes-bold)))
    `(diredfl-dir-name ((,c :foreground ,accent-0)))
    `(diredfl-dir-priv ((,c :foreground ,accent-0)))
    `(diredfl-exec-priv ((,c :foreground ,accent-1)))
    `(diredfl-executable-tag ((,c :foreground ,accent-1)))
    `(diredfl-file-name ((,c :foreground ,fg-main)))
    `(diredfl-file-suffix ((,c :foreground ,variable)))
    `(diredfl-flag-mark ((,c :inherit bold :background ,bg-mark-select :foreground ,fg-mark-select)))
    `(diredfl-flag-mark-line ((,c :inherit bold :background ,bg-mark-select :foreground ,fg-mark-select)))
    `(diredfl-ignored-file-name ((,c :foreground ,fg-dim)))
    `(diredfl-link-priv ((,c :foreground ,fg-link)))
    `(diredfl-no-priv ((,c :foreground ,fg-dim)))
    `(diredfl-number ((,c :foreground ,fg-dim)))
    `(diredfl-other-priv ((,c :foreground ,accent-2)))
    `(diredfl-rare-priv ((,c :foreground ,accent-3)))
    `(diredfl-read-priv ((,c :foreground ,fg-main)))
    `(diredfl-symlink ((,c :background ,bg-link-symbolic :foreground ,fg-link-symbolic :underline ,underline-link-symbolic)))
    `(diredfl-tagged-autofile-name ((,c :background ,bg-inactive)))
    `(diredfl-write-priv ((,c :foreground ,accent-0)))
;;;;; disk-usage
    `(disk-usage-inaccessible ((,c :foreground ,err)))
    `(disk-usage-percent ((,c :foreground ,accent-0)))
    `(disk-usage-size ((,c :foreground ,accent-1)))
    `(disk-usage-symlink ((,c :background ,bg-link-symbolic :foreground ,fg-link-symbolic :underline ,underline-link-symbolic)))
    `(disk-usage-symlink-directory ((,c :background ,bg-link-symbolic :foreground ,fg-link-symbolic :underline ,underline-link-symbolic)))
;;;;; display-fill-column-indicator-mode
    `(fill-column-indicator ((,c :height 1 :background ,bg-active :foreground ,bg-active)))
;;;;; doom-modeline
    `(doom-modeline-bar ((,c :background ,blue))) ; special case like `centaur-tabs-active-bar-face'
    `(doom-modeline-bar-inactive ((,c :background ,border)))
    `(doom-modeline-battery-charging ((,c :foreground ,modeline-info)))
    `(doom-modeline-battery-critical ((,c :underline t :foreground ,modeline-err)))
    `(doom-modeline-battery-error ((,c :underline t :foreground ,modeline-err)))
    `(doom-modeline-battery-full (( )))
    `(doom-modeline-battery-warning ((,c :foreground ,warning)))
    `(doom-modeline-buffer-file ((,c :inherit modus-themes-bold)))
    `(doom-modeline-buffer-major-mode (( )))
    `(doom-modeline-buffer-minor-mode (( )))
    `(doom-modeline-buffer-modified ((,c :foreground ,modeline-err)))
    `(doom-modeline-buffer-path (( )))
    `(doom-modeline-evil-emacs-state ((,c :inherit modus-themes-slant)))
    `(doom-modeline-evil-insert-state ((,c :foreground ,modeline-info)))
    `(doom-modeline-evil-motion-state (( )))
    `(doom-modeline-evil-normal-state (( )))
    `(doom-modeline-evil-operator-state ((,c :inherit modus-themes-bold)))
    `(doom-modeline-evil-replace-state ((,c :foreground ,err)))
    `(doom-modeline-evil-visual-state ((,c :foreground ,warning)))
    `(doom-modeline-info ((,c :foreground ,info)))
    `(doom-modeline-input-method (( )))
    `(doom-modeline-lsp-error ((,c :inherit modus-themes-bold)))
    `(doom-modeline-lsp-running (( )))
    `(doom-modeline-lsp-success ((,c :foreground ,info)))
    `(doom-modeline-lsp-warning ((,c :foreground ,warning)))
    `(doom-modeline-notification ((,c :foreground ,err)))
    `(doom-modeline-project-dir (( )))
    `(doom-modeline-project-parent-dir (( )))
    `(doom-modeline-project-root-dir (( )))
    `(doom-modeline-repl-success ((,c :foreground ,info)))
    `(doom-modeline-repl-warning ((,c :foreground ,warning)))
    `(doom-modeline-time (( )))
    `(doom-modeline-urgent ((,c :inherit modus-themes-bold :foreground ,modeline-err)))
    `(doom-modeline-warning ((,c :foreground ,warning)))
;;;;; ediff
    `(ediff-current-diff-A ((,c :background ,bg-removed :foreground ,fg-removed)))
    `(ediff-current-diff-Ancestor ((,c :background ,bg-region)))
    `(ediff-current-diff-B ((,c :background ,bg-added :foreground ,fg-added)))
    `(ediff-current-diff-C ((,c :background ,bg-changed :foreground ,fg-changed)))
    `(ediff-even-diff-A ((,c :background ,bg-diff-context)))
    `(ediff-even-diff-Ancestor ((,c :background ,bg-diff-context)))
    `(ediff-even-diff-B ((,c :background ,bg-diff-context)))
    `(ediff-even-diff-C ((,c :background ,bg-diff-context)))
    `(ediff-fine-diff-A ((,c :background ,bg-removed-refine :foreground ,fg-removed)))
    `(ediff-fine-diff-Ancestor ((,c :background ,bg-active :foreground ,fg-main)))
    `(ediff-fine-diff-B ((,c :background ,bg-added-refine :foreground ,fg-added)))
    `(ediff-fine-diff-C ((,c :background ,bg-changed-refine :foreground ,fg-changed)))
    `(ediff-odd-diff-A ((,c :background ,bg-diff-context)))
    `(ediff-odd-diff-Ancestor ((,c :background ,bg-diff-context)))
    `(ediff-odd-diff-B ((,c :background ,bg-diff-context)))
    `(ediff-odd-diff-C ((,c :background ,bg-diff-context)))
;;;;; ein (Emacs IPython Notebook)
    `(ein:basecell-input-area-face ((,c :background ,bg-dim :extend t)))
    `(ein:cell-output-area (( )))
    `(ein:cell-output-area-error ((,c :background ,bg-removed :extend t)))
    `(ein:cell-output-stderr ((,c :background ,bg-removed :extend t)))
    `(ein:markdowncell-input-area-face (( )))
    `(ein:notification-tab-normal ((,c :inherit underline)))
;;;;; eglot
    `(eglot-highlight-symbol-face ((,c :underline ,border)))
    `(eglot-mode-line ((,c :inherit modus-themes-bold :foreground ,modeline-info)))
    `(eglot-diagnostic-tag-unnecessary-face ((,c :underline (:style wave :color ,underline-note))))
;;;;; el-search
    `(el-search-highlight-in-prompt-face ((,c :inherit modus-themes-slant)))
    `(el-search-match ((,c :background ,bg-search-current :foreground ,fg-search-current)))
    `(el-search-other-match ((,c :background ,bg-search-lazy :foreground ,fg-search-lazy)))
    `(el-search-occur-match ((,c :background ,bg-search-static :foreground ,fg-search-static)))
;;;;; eldoc
    ;; NOTE: see https://github.com/purcell/package-lint/issues/187
    (list 'eldoc-highlight-function-argument `((,c :inherit modus-themes-bold :background ,bg-active-argument :foreground ,fg-active-argument)))
;;;;; eldoc-box
    `(eldoc-box-body ((,c :background ,bg-dim :foreground ,fg-main)))
    `(eldoc-box-border ((,c :background ,border)))
;;;;; elfeed
    `(elfeed-log-date-face ((,c :foreground ,date-common)))
    `(elfeed-log-debug-level-face ((,c :inherit modus-themes-bold)))
    `(elfeed-log-error-level-face ((,c :foreground ,err)))
    `(elfeed-log-info-level-face ((,c :foreground ,info)))
    `(elfeed-log-warn-level-face ((,c :foreground ,warning)))
    `(elfeed-search-date-face ((,c :foreground ,date-common)))
    `(elfeed-search-feed-face ((,c :foreground ,accent-1)))
    `(elfeed-search-filter-face ((,c :inherit modus-themes-bold)))
    `(elfeed-search-last-update-face ((,c :inherit modus-themes-bold :foreground ,date-common)))
    `(elfeed-search-tag-face ((,c :foreground ,accent-0)))
    `(elfeed-search-title-face ((,c :foreground ,fg-dim)))
    `(elfeed-search-unread-count-face (( )))
    `(elfeed-search-unread-title-face ((,c :inherit bold :foreground ,fg-main)))
;;;;; elfeed-score
    `(elfeed-score-date-face ((,c :foreground ,date-common)))
    `(elfeed-score-debug-level-face ((,c :inherit modus-themes-bold)))
    `(elfeed-score-error-level-face ((,c :foreground ,err)))
    `(elfeed-score-info-level-face ((,c :foreground ,info)))
    `(elfeed-score-warn-level-face ((,c :foreground ,warning)))
;;;;; elisp semantic highlighting
    `(elisp-ampersand ((,c :inherit modus-themes-bold :foreground ,type)))
    `(elisp-binding-variable (( )))
    `(elisp-bound-variable ((,c :foreground ,variable-use)))
    `(elisp-charset (( )))
    `(elisp-coding (( )))
    `(elisp-completion-category (( )))
    `(elisp-completion-category-definition ((,c :foreground ,constant)))
    `(elisp-condition ((,c :inherit modus-themes-bold :foreground ,builtin)))
    `(elisp-constant (( )))
    `(elisp-defcharset ((,c :foreground ,type)))
    `(elisp-defcoding ((,c :foreground ,type)))
    `(elisp-defface ((,c :foreground ,type)))
    `(elisp-deficon ((,c :foreground ,type)))
    `(elisp-defmacro ((,c :foreground ,fnname)))
    `(elisp-defoclosure ((,c :foreground ,type)))
    `(elisp-defun ((,c :foreground ,fnname)))
    `(elisp-defvar ((,c :foreground ,variable)))
    `(elisp-face (( )))
    `(elisp-feature ((,c :foreground ,constant)))
    `(elisp-free-variable (( )))
    `(elisp-function ((,c :foreground ,fnname-call)))
    `(elisp-function-property-declaration ((,c :foreground ,builtin)))
    `(elisp-group (( )))
    `(elisp-icon (( )))
    `(elisp-macro ((,c :inherit modus-themes-bold :foreground ,keyword)))
    `(elisp-major-mode-name ((,c :foreground ,type)))
    `(elisp-nnoo-backend ((,c :foreground ,type)))
    `(elisp-non-local-exit ((,c :inherit modus-themes-bold :foreground ,warning)))
    `(elisp-oclosure (( )))
    `(elisp-rx ((,c :inherit modus-themes-bold :foreground ,builtin)))
    `(elisp-shadowed-variable ((,c :foreground ,variable-use)))
    `(elisp-shadowing-variable (( )))
    `(elisp-shorthand-font-lock-face ((,c :inherit modus-themes-slant :foreground ,preprocessor)))
    `(elisp-slot ((,c :foreground ,builtin)))
    `(elisp-special-form ((,c :inherit modus-themes-bold :foreground ,keyword)))
    `(elisp-special-variable-declaration (( )))
    `(elisp-symbol-at-mouse ((,c :background ,bg-hover :foreground ,fg-main)))
    `(elisp-symbol-role (( )))
    `(elisp-symbol-role-definition ((,c :foreground ,type)))
    `(elisp-theme (( )))
    `(elisp-thing (( )))
    `(elisp-throw-tag ((,c :inherit modus-themes-bold :foreground ,fg-main)))
    `(elisp-type (( )))
    `(elisp-unknown-call ((,c :inherit modus-themes-slant :foreground ,fnname-call)))
    `(elisp-variable-at-point ((,c :underline ,border)))
    `(elisp-warning-type ((,c :foreground ,warning)))
    `(elisp-widget-type (( )))
;;;;; elpher
    `(elpher-gemini-heading1 ((,c :inherit modus-themes-heading-1)))
    `(elpher-gemini-heading2 ((,c :inherit modus-themes-heading-2)))
    `(elpher-gemini-heading3 ((,c :inherit modus-themes-heading-3)))
;;;;; embark
    `(embark-collect-group-title ((,c :inherit modus-themes-bold :foreground ,name)))
    `(embark-keybinding ((,c :inherit (bold modus-themes-fixed-pitch) :foreground ,keybind)))
    `(embark-keybinding-repeat ((,c :inherit modus-themes-bold)))
    `(embark-selected ((,c :inherit bold :background ,bg-mark-select :foreground ,fg-mark-select)))
;;;;; ement (ement.el)
    `(ement-room-fully-read-marker ((,c :foreground ,info)))
    `(ement-room-membership ((,c :foreground ,fg-dim)))
    `(ement-room-mention ((,c :background ,bg-hover :foreground ,fg-main)))
    `(ement-room-name ((,c :inherit modus-themes-bold)))
    `(ement-room-reactions ((,c :foreground ,fg-dim)))
    `(ement-room-read-receipt-marker ((,c :background ,bg-search-static :foreground ,fg-search-static)))
    `(ement-room-self ((,c :inherit modus-themes-bold :foreground ,accent-1)))
    `(ement-room-self-message ((,c :foreground ,fg-alt)))
    `(ement-room-timestamp ((,c :foreground ,fg-dim)))
    `(ement-room-timestamp-header ((,c :inherit modus-themes-bold :foreground ,date-common)))
    `(ement-room-user ((,c :inherit modus-themes-bold :foreground ,accent-0)))
;;;;; emms
    `(emms-browser-album-face ((,c :foreground ,keyword)))
    `(emms-browser-artist-face ((,c :foreground ,variable)))
    `(emms-browser-composer-face ((,c :foreground ,builtin)))
    `(emms-browser-performer-face ((,c :foreground ,variable)))
    `(emms-browser-track-face ((,c :foreground ,string)))
    `(emms-browser-year/genre-face ((,c :foreground ,type)))
    `(emms-playlist-track-face ((,c :foreground ,string)))
    `(emms-playlist-selected-face ((,c :inherit modus-themes-bold :foreground ,constant)))
    `(emms-metaplaylist-mode-current-face ((,c :inherit modus-themes-bold :foreground ,constant)))
    `(emms-metaplaylist-mode-face ((,c :foreground ,variable)))
;;;;; epa
    `(epa-field-body (( )))
    `(epa-field-name ((,c :inherit modus-themes-bold :foreground ,fg-dim)))
    `(epa-mark ((,c :inherit modus-themes-bold)))
    `(epa-string ((,c :foreground ,string)))
    `(epa-validity-disabled ((,c :foreground ,err)))
    `(epa-validity-high ((,c :foreground ,info)))
    `(epa-validity-low ((,c :foreground ,fg-dim)))
    `(epa-validity-medium ((,c :foreground ,info)))
;;;;; erc
    `(erc-action-face ((,c :foreground ,accent-2)))
    `(erc-bold-face ((,c :inherit bold)))
    `(erc-button ((,c :background ,bg-link :foreground ,fg-link :underline ,underline-link)))
    `(erc-command-indicator-face ((,c :inherit modus-themes-bold :foreground ,accent-3)))
    `(erc-current-nick-face ((,c :background ,bg-search-static :foreground ,fg-search-static)))
    `(erc-dangerous-host-face ((,c :foreground ,err)))
    `(erc-direct-msg-face ((,c :foreground ,fg-dim)))
    `(erc-error-face ((,c :foreground ,err)))
    `(erc-fill-wrap-merge-indicator-face ((,c :foreground ,fg-dim)))
    `(erc-fool-face ((,c :foreground ,fg-dim)))
    `(erc-input-face ((,c :foreground ,fnname)))
    `(erc-inverse-face ((,c :inherit erc-default-face :inverse-video t)))
    `(erc-keep-place-indicator-arrow ((,c :foreground ,info)))
    `(erc-keyword-face ((,c :inherit modus-themes-bold :foreground ,keyword)))
    `(erc-my-nick-face ((,c :inherit modus-themes-bold :foreground ,name)))
    `(erc-my-nick-prefix-face ((,c :inherit modus-themes-bold :foreground ,name)))
    `(erc-nick-default-face ((,c :inherit modus-themes-bold :foreground ,accent-0)))
    `(erc-nick-msg-face ((,c :foreground ,warning)))
    `(erc-nick-prefix-face ((,c :inherit erc-nick-default-face)))
    `(erc-notice-face ((,c :inherit modus-themes-slant :foreground ,comment)))
    `(erc-pal-face ((,c :inherit modus-themes-bold :foreground ,accent-1)))
    `(erc-prompt-face ((,c :inherit modus-themes-prompt)))
    `(erc-timestamp-face ((,c :foreground ,date-common)))
    `(erc-underline-face ((,c :inherit underline)))
;;;;; ert
    `(ert-test-result-expected ((,c :background ,bg-prominent-note :foreground ,fg-prominent-note)))
    `(ert-test-result-unexpected ((,c :background ,bg-prominent-err :foreground ,fg-prominent-err)))
;;;;; erts-mode
    `(erts-mode-end-test ((,c :foreground ,err)))
    `(erts-mode-specification-name ((,c :inherit modus-themes-bold)))
    `(erts-mode-specification-value ((,c :foreground ,string)))
    `(erts-mode-start-test ((,c :foreground ,info)))
;;;;; eshell
    `(eshell-ls-archive ((,c :foreground ,accent-2)))
    `(eshell-ls-backup ((,c :foreground ,fg-dim)))
    `(eshell-ls-clutter ((,c :foreground ,fg-dim)))
    `(eshell-ls-directory ((,c :foreground ,accent-0)))
    `(eshell-ls-executable ((,c :foreground ,accent-1)))
    `(eshell-ls-missing ((,c :foreground ,err)))
    `(eshell-ls-product ((,c :foreground ,fg-dim)))
    `(eshell-ls-readonly ((,c :foreground ,warning)))
    `(eshell-ls-special ((,c :foreground ,accent-3)))
    `(eshell-ls-symlink ((,c :background ,bg-link :foreground ,fg-link :underline ,underline-link)))
    `(eshell-ls-unreadable ((,c :foreground ,fg-dim)))
    `(eshell-prompt ((,c :inherit modus-themes-prompt)))
;;;;; eshell-fringe-status
    `(eshell-fringe-status-failure ((,c :foreground ,err)))
    `(eshell-fringe-status-success ((,c :foreground ,info)))
;;;;; evil-mode
    `(evil-ex-commands ((,c :inherit modus-themes-bold :foreground ,keyword)))
    `(evil-ex-info ((,c :inherit modus-themes-bold :foreground ,type)))
    `(evil-ex-lazy-highlight ((,c :background ,bg-search-lazy :foreground ,fg-search-lazy)))
    `(evil-ex-search ((,c :background ,bg-search-current :foreground ,fg-search-current)))
    `(evil-ex-substitute-matches ((,c :background ,bg-search-replace :foreground ,fg-search-replace)))
    `(evil-ex-substitute-replacement ((,c :background ,bg-search-current :foreground ,fg-search-current)))
;;;;; eww
    `(eww-invalid-certificate ((,c :foreground ,err)))
    `(eww-valid-certificate ((,c :foreground ,info)))
    `(eww-form-checkbox ((,c :background ,bg-button-inactive :foreground ,fg-button-active)))
    `(eww-form-file ((,c :inherit modus-themes-button)))
    `(eww-form-select ((,c :inherit modus-themes-button)))
    `(eww-form-submit ((,c :inherit modus-themes-button)))
    `(eww-form-text ((,c :background ,bg-button-inactive :foreground ,fg-button-active :extend nil :underline (:position t))))
    `(eww-form-textarea ((,c :background ,bg-button-inactive :foreground ,fg-button-active :extend nil :underline (:position t))))
;;;;; eyebrowse
    `(eyebrowse-mode-line-active ((,c :inherit italic :foreground ,modeline-info)))
;;;;; flycheck
    `(flycheck-error ((,c :underline (:style wave :color ,underline-err))))
    `(flycheck-fringe-error ((,c :background ,bg-prominent-err :foreground ,fg-prominent-err)))
    `(flycheck-fringe-info ((,c :background ,bg-prominent-note :foreground ,fg-prominent-note)))
    `(flycheck-fringe-warning ((,c :background ,bg-prominent-warning :foreground ,fg-prominent-warning)))
    `(flycheck-info ((,c :underline (:style wave :color ,underline-note))))
    `(flycheck-warning ((,c :underline (:style wave :color ,underline-warning))))
;;;;; flycheck-color-mode-line
    `(flycheck-color-mode-line-error-face ((,c :background ,bg-prominent-err :foreground ,fg-prominent-err)))
    `(flycheck-color-mode-line-info-face ((,c :background ,bg-prominent-note :foreground ,fg-prominent-note)))
    `(flycheck-color-mode-line-running-face ((,c :inherit modus-themes-slant)))
    `(flycheck-color-mode-line-warning-face ((,c :background ,bg-prominent-warning :foreground ,fg-prominent-warning)))
;;;;; flycheck-indicator
    `(flycheck-indicator-disabled ((,c :inherit modus-themes-slant :foreground ,fg-dim)))
    `(flycheck-indicator-error ((,c :foreground ,err)))
    `(flycheck-indicator-info ((,c :inherit modus-themes-bold)))
    `(flycheck-indicator-running ((,c :inherit modus-themes-slant)))
    `(flycheck-indicator-success ((,c :foreground ,info)))
    `(flycheck-indicator-warning ((,c :foreground ,warning)))
;;;;; flymake
    `(flymake-end-of-line-diagnostics-face ((,c :inherit modus-themes-slant :height 0.85 :box ,border)))
    `(flymake-error ((,c :underline (:style wave :color ,underline-err))))
    `(flymake-error-echo ((,c :foreground ,err)))
    `(flymake-error-echo-at-eol ((,c :inherit modus-themes-slant :foreground ,err :height 0.85 :box ,border)))
    `(flymake-error-fringe ((,c :background ,bg-prominent-err :foreground ,fg-prominent-err)))
    `(flymake-note ((,c :underline (:style wave :color ,underline-note))))
    `(flymake-note-echo ((,c :foreground ,info)))
    `(flymake-note-echo-at-eol ((,c :inherit modus-themes-slant :foreground ,info :height 0.85 :box ,border)))
    `(flymake-note-fringe ((,c :background ,bg-prominent-note :foreground ,fg-prominent-note)))
    `(flymake-warning ((,c :underline (:style wave :color ,underline-warning))))
    `(flymake-warning-echo ((,c :foreground ,warning)))
    `(flymake-warning-echo-at-eol ((,c :inherit modus-themes-slant :foreground ,warning :height 0.85 :box ,border)))
    `(flymake-warning-fringe ((,c :background ,bg-prominent-warning :foreground ,fg-prominent-warning)))
;;;;; flyspell
    `(flyspell-duplicate ((,c :underline (:style wave :color ,underline-warning))))
    `(flyspell-incorrect ((,c :underline (:style wave :color ,underline-err))))
;;;;; flx
    `(flx-highlight-face ((,c :inherit modus-themes-completion-match-0)))
;;;;; focus
    `(focus-unfocused ((,c :foreground "gray50")))
;;;;; fold-this
    `(fold-this-overlay ((,c :background ,bg-inactive)))
;;;;; font-lock
    `(font-lock-bracket-face ((,c :foreground ,bracket)))
    `(font-lock-builtin-face ((,c :inherit modus-themes-bold :foreground ,builtin)))
    `(font-lock-comment-delimiter-face ((,c :inherit modus-themes-slant :foreground ,comment)))
    `(font-lock-comment-face ((,c :inherit modus-themes-slant :foreground ,comment)))
    `(font-lock-constant-face ((,c :foreground ,constant)))
    `(font-lock-delimiter-face ((,c :foreground ,delimiter)))
    `(font-lock-doc-face ((,c :inherit modus-themes-slant :foreground ,docstring)))
    `(font-lock-doc-markup-face ((,c :inherit modus-themes-slant :foreground ,docmarkup)))
    `(font-lock-function-call-face ((,c :foreground ,fnname-call)))
    `(font-lock-function-name-face ((,c :foreground ,fnname)))
    `(font-lock-keyword-face ((,c :inherit modus-themes-bold :foreground ,keyword)))
    `(font-lock-negation-char-face ((,c :foreground ,err)))
    `(font-lock-number-face ((,c :foreground ,number)))
    `(font-lock-operator-face ((,c :foreground ,operator)))
    `(font-lock-preprocessor-face ((,c :foreground ,preprocessor)))
    `(font-lock-property-name-face ((,c :foreground ,property)))
    `(font-lock-punctuation-face ((,c :foreground ,punctuation)))
    `(font-lock-regexp-grouping-backslash ((,c :inherit modus-themes-bold :foreground ,rx-backslash)))
    `(font-lock-regexp-grouping-construct ((,c :inherit modus-themes-bold :foreground ,rx-construct)))
    `(font-lock-string-face ((,c :foreground ,string)))
    `(font-lock-type-face ((,c :inherit modus-themes-bold :foreground ,type)))
    `(font-lock-variable-name-face ((,c :foreground ,variable)))
    `(font-lock-variable-use-face ((,c :foreground ,variable-use)))
    `(font-lock-warning-face ((,c :inherit modus-themes-bold :foreground ,warning)))
;;;;; forge
    `(forge-dimmed ((,c :foreground ,fg-dim)))
    `(forge-issue-completed ((,c :foreground ,fg-dim)))
    `(forge-issue-open (( )))
    `(forge-issue-unplanned ((,c :foreground ,fg-dim :strike-through t)))
    `(forge-post-author ((,c :inherit modus-themes-bold :foreground ,name)))
    `(forge-post-date ((,c :inherit modus-themes-bold :foreground ,date-common)))
    `(forge-pullreq-merged ((,c :foreground ,fg-alt)))
    `(forge-pullreq-open ((,c :foreground ,info)))
    `(forge-pullreq-rejected ((,c :foreground ,err :strike-through t)))
    `(forge-topic-done (( )))
    `(forge-topic-pending ((,c :inherit modus-themes-slant)))
    `(forge-topic-slug-completed ((,c :foreground ,fg-dim)))
    `(forge-topic-slug-open ((,c :foreground ,fg-dim)))
    `(forge-topic-slug-saved ((,c :foreground ,info)))
    `(forge-topic-slug-unplanned ((,c :foreground ,fg-dim :strike-through t)))
    `(forge-topic-unread ((,c :inherit bold)))
;;;;; geiser
    `(geiser-font-lock-autodoc-current-arg ((,c :inherit modus-themes-bold :background ,bg-active-argument :foreground ,fg-active-argument)))
    `(geiser-font-lock-autodoc-identifier ((,c :foreground ,docstring)))
    `(geiser-font-lock-doc-button ((,c :background ,bg-link :foreground ,fg-link :underline ,underline-link)))
    `(geiser-font-lock-doc-link ((,c :background ,bg-link :foreground ,fg-link :underline ,underline-link)))
    `(geiser-font-lock-error-link ((,c :foreground ,err :underline t)))
    `(geiser-font-lock-image-button ((,c :foreground ,info :underline t)))
    `(geiser-font-lock-repl-input ((,c :inherit modus-themes-bold)))
    `(geiser-font-lock-repl-output ((,c :inherit modus-themes-bold :foreground ,keyword)))
    `(geiser-font-lock-repl-prompt ((,c :inherit modus-themes-prompt)))
    `(geiser-font-lock-xref-header ((,c :inherit modus-themes-bold)))
    `(geiser-font-lock-xref-link ((,c :background ,bg-link :foreground ,fg-link :underline ,underline-link)))
;;;;; git-commit
    `(git-commit-comment-action ((,c :inherit modus-themes-slant :foreground ,comment)))
    `(git-commit-comment-branch-local ((,c :inherit modus-themes-slant :foreground ,accent-0)))
    `(git-commit-comment-branch-remote ((,c :inherit modus-themes-slant :foreground ,accent-1)))
    `(git-commit-comment-heading ((,c :inherit (modus-themes-bold modus-themes-slant) :foreground ,comment)))
    `(git-commit-comment-file ((,c :inherit modus-themes-slant :foreground ,accent-2))) ; like `magit-filename'
    `(git-commit-keyword ((,c :foreground ,keyword)))
    `(git-commit-nonempty-second-line ((,c :foreground ,err)))
    `(git-commit-overlong-summary ((,c :foreground ,warning)))
    `(git-commit-summary ((,c :inherit modus-themes-bold :foreground ,fg-alt)))
;;;;; git-gutter
    `(git-gutter:added ((,c :background ,bg-added-fringe)))
    `(git-gutter:deleted ((,c :background ,bg-removed-fringe)))
    `(git-gutter:modified ((,c :background ,bg-changed-fringe)))
    `(git-gutter:separator (( )))
    `(git-gutter:unchanged (( )))
;;;;; git-gutter-fr
    `(git-gutter-fr:added ((,c :background ,bg-added-fringe)))
    `(git-gutter-fr:deleted ((,c :background ,bg-removed-fringe)))
    `(git-gutter-fr:modified ((,c :background ,bg-changed-fringe)))
;;;;; git-rebase
    `(git-rebase-comment-hash ((,c :inherit (modus-themes-bold modus-themes-slant) :foreground ,identifier)))
    `(git-rebase-comment-heading  ((,c :inherit (modus-themes-bold modus-themes-slant) :foreground ,comment)))
    `(git-rebase-description ((,c :foreground ,fg-main)))
    `(git-rebase-hash ((,c :foreground ,identifier)))
;;;;; git-timemachine
    `(git-timemachine-commit ((,c :foreground ,warning)))
    `(git-timemachine-minibuffer-author-face ((,c :foreground ,name)))
    `(git-timemachine-minibuffer-detail-face ((,c :foreground ,fg-main)))
;;;;; gnus
    `(gnus-button ((,c :underline ,border)))
    `(gnus-cite-1 ((,c :foreground ,mail-cite-0)))
    `(gnus-cite-2 ((,c :foreground ,mail-cite-1)))
    `(gnus-cite-3 ((,c :foreground ,mail-cite-2)))
    `(gnus-cite-4 ((,c :foreground ,mail-cite-3)))
    `(gnus-cite-5 ((,c :foreground ,mail-cite-0)))
    `(gnus-cite-6 ((,c :foreground ,mail-cite-1)))
    `(gnus-cite-7 ((,c :foreground ,mail-cite-2)))
    `(gnus-cite-8 ((,c :foreground ,mail-cite-3)))
    `(gnus-cite-9 ((,c :foreground ,mail-cite-0)))
    `(gnus-cite-10 ((,c :foreground ,mail-cite-1)))
    `(gnus-cite-11 ((,c :foreground ,mail-cite-2)))
    `(gnus-cite-attribution ((,c :inherit modus-themes-slant)))
    `(gnus-emphasis-bold ((,c :inherit modus-themes-bold)))
    `(gnus-emphasis-bold-italic ((,c :inherit bold-italic)))
    `(gnus-emphasis-highlight-words ((,c :foreground ,warning)))
    `(gnus-emphasis-italic ((,c :inherit italic)))
    `(gnus-emphasis-underline-bold ((,c :inherit gnus-emphasis-bold :underline t)))
    `(gnus-emphasis-underline-bold-italic ((,c :inherit gnus-emphasis-bold-italic :underline t)))
    `(gnus-emphasis-underline-italic ((,c :inherit gnus-emphasis-italic :underline t)))
    `(gnus-group-mail-1 ((,c :inherit bold :foreground ,accent-0)))
    `(gnus-group-mail-1-empty ((,c :foreground ,accent-0)))
    `(gnus-group-mail-2 ((,c :inherit modus-themes-bold :foreground ,accent-1)))
    `(gnus-group-mail-2-empty ((,c :foreground ,accent-1)))
    `(gnus-group-mail-3 ((,c :inherit modus-themes-bold :foreground ,accent-2)))
    `(gnus-group-mail-3-empty ((,c :foreground ,accent-2)))
    `(gnus-group-mail-low ((,c :inherit modus-themes-bold :foreground ,fg-dim)))
    `(gnus-group-mail-low-empty ((,c :foreground ,fg-dim)))
    `(gnus-group-news-1 ((,c :inherit modus-themes-bold :foreground ,green)))
    `(gnus-group-news-1-empty ((,c :foreground ,green)))
    `(gnus-group-news-2 ((,c :inherit modus-themes-bold :foreground ,cyan)))
    `(gnus-group-news-2-empty ((,c :foreground ,cyan)))
    `(gnus-group-news-3 ((,c :inherit modus-themes-bold :foreground ,yellow-faint)))
    `(gnus-group-news-3-empty ((,c :foreground ,yellow-faint)))
    `(gnus-group-news-4 ((,c :inherit modus-themes-bold :foreground ,magenta-faint)))
    `(gnus-group-news-4-empty ((,c :foreground ,magenta-faint)))
    `(gnus-group-news-5 ((,c :inherit modus-themes-bold :foreground ,fg-alt)))
    `(gnus-group-news-5-empty ((,c :foreground ,fg-alt)))
    `(gnus-group-news-6 ((,c :inherit modus-themes-bold :foreground ,fg-dim)))
    `(gnus-group-news-6-empty ((,c :foreground ,fg-dim)))
    `(gnus-group-news-low ((,c :inherit modus-themes-bold :foreground ,fg-dim)))
    `(gnus-group-news-low-empty ((,c :foreground ,fg-dim)))
    `(gnus-header-content ((,c :foreground ,mail-other)))
    `(gnus-header-from ((,c :inherit modus-themes-bold :foreground ,mail-recipient)))
    `(gnus-header-name ((,c :inherit modus-themes-bold)))
    `(gnus-header-newsgroups ((,c :foreground ,mail-other)))
    `(gnus-header-subject ((,c :inherit modus-themes-bold :foreground ,mail-subject)))
    `(gnus-server-agent ((,c :inherit modus-themes-bold)))
    `(gnus-server-closed ((,c :inherit modus-themes-slant)))
    `(gnus-server-cloud ((,c :inherit modus-themes-bold :foreground ,fg-alt)))
    `(gnus-server-cloud-host ((,c :inherit modus-themes-bold :foreground ,fg-alt :underline t)))
    `(gnus-server-denied ((,c :foreground ,err)))
    `(gnus-server-offline ((,c :foreground ,fg-dim)))
    `(gnus-server-opened ((,c :foreground ,info)))
    `(gnus-summary-cancelled ((,c :inherit modus-themes-slant :foreground ,warning)))
    `(gnus-summary-high-ancient ((,c :inherit modus-themes-bold :foreground ,fg-alt)))
    `(gnus-summary-high-read ((,c :inherit modus-themes-bold :foreground ,fg-dim)))
    `(gnus-summary-high-ticked ((,c :inherit modus-themes-bold :foreground ,err)))
    `(gnus-summary-high-undownloaded ((,c :inherit modus-themes-bold :foreground ,warning)))
    `(gnus-summary-high-unread ((,c :inherit bold)))
    `(gnus-summary-low-ancient ((,c :inherit modus-themes-slant)))
    `(gnus-summary-low-read ((,c :inherit modus-themes-slant :foreground ,fg-dim)))
    `(gnus-summary-low-ticked ((,c :inherit modus-themes-slant :foreground ,err)))
    `(gnus-summary-low-undownloaded ((,c :inherit modus-themes-slant :foreground ,warning)))
    `(gnus-summary-low-unread ((,c :inherit modus-themes-slant)))
    `(gnus-summary-normal-ancient ((,c :foreground ,fg-dim)))
    `(gnus-summary-normal-read ((,c :foreground ,fg-dim)))
    `(gnus-summary-normal-ticked ((,c :foreground ,err)))
    `(gnus-summary-normal-undownloaded ((,c :foreground ,warning)))
    `(gnus-summary-normal-unread (( )))
    `(gnus-summary-selected ((,c :background ,bg-hover :foreground ,fg-main)))
;;;;; gotest
    `(go-test--ok-face ((,c :foreground ,info)))
    `(go-test--error-face ((,c :foreground ,err)))
    `(go-test--warning-face ((,c :foreground ,warning)))
    `(go-test--pointer-face ((,c :foreground ,accent-0)))
    `(go-test--standard-face (( )))
;;;;; golden-ratio-scroll-screen
    `(golden-ratio-scroll-highlight-line-face ((,c :background ,bg-cyan-subtle :foreground ,fg-main)))
;;;;; helpful
    `(helpful-heading ((,c :inherit modus-themes-heading-1)))
;;;;; hexl-mode
    `(hexl-address-region ((,c :foreground ,constant)))
    `(hexl-ascii-region ((,c :foreground ,variable)))
;;;;; hideshow
    `(hs-ellipsis (( )))
;;;;; highlight region or ad-hoc regexp
    `(hi-aquamarine ((,c :background ,bg-cyan-intense :foreground ,fg-main)))
    `(hi-black-b ((,c :background ,fg-main :foreground ,bg-main)))
    `(hi-black-hb ((,c :background ,fg-alt :foreground ,bg-main)))
    `(hi-blue ((,c :background ,bg-blue-subtle :foreground ,fg-main)))
    `(hi-blue-b ((,c :background ,bg-blue-intense :foreground ,fg-main)))
    `(hi-green ((,c :background ,bg-green-subtle :foreground ,fg-main)))
    `(hi-green-b ((,c :background ,bg-green-intense :foreground ,fg-main)))
    `(hi-pink ((,c :background ,bg-magenta-intense :foreground ,fg-main)))
    `(hi-red-b ((,c :background ,bg-red-intense :foreground ,fg-main)))
    `(hi-salmon ((,c :background ,bg-clay :foreground ,fg-clay)))
    `(hi-yellow ((,c :background ,bg-yellow-intense :foreground ,fg-main)))
    `(highlight-changes ((,c :foreground ,warning :underline nil)))
    `(highlight-changes-delete ((,c :foreground ,err :underline t)))
    `(hl-line ((,c :background ,bg-hl-line :extend t)))
;;;;; highlight-numbers
    `(highlight-numbers-number ((,c :foreground ,constant)))
;;;;; highlight-thing
    `(highlight-thing ((,c :background ,bg-search-static :foreground ,fg-search-static)))
;;;;; hl-fill-column
    `(hl-fill-column-face ((,c :background ,bg-active)))
;;;;; hl-todo
    `(hl-todo ((,c :inherit (modus-themes-bold modus-themes-slant) :foreground ,prose-todo)))
;;;;; howm
    `(action-lock-face ((,c :background ,bg-link :foreground ,fg-link :underline ,underline-link)))
    `(howm-mode-keyword-face (( )))
    `(howm-mode-ref-face ((,c :background ,bg-link :foreground ,fg-link :underline ,underline-link)))
    `(howm-mode-title-face ((,c :inherit modus-themes-heading-0)))
    `(howm-mode-wiki-face ((,c :background ,bg-link :foreground ,fg-link :underline ,underline-link)))
    `(howm-reminder-deadline-face ((,c :foreground ,date-deadline)))
    `(howm-reminder-late-deadline-face ((,c :inherit modus-themes-bold :foreground ,date-deadline)))
    `(howm-reminder-defer-face ((,c :foreground ,date-scheduled)))
    `(howm-reminder-scheduled-face ((,c :foreground ,date-scheduled)))
    `(howm-reminder-done-face ((,c :foreground ,prose-done)))
    `(howm-reminder-todo-face ((,c :foreground ,prose-todo)))
    `(howm-reminder-normal-face ((,c :foreground ,date-common)))
    `(howm-reminder-today-face ((,c :inherit modus-themes-bold :foreground ,date-common)))
    `(howm-reminder-tomorrow-face ((,c :inherit modus-themes-bold :foreground ,date-scheduled)))
    `(howm-simulate-todo-mode-line-face ((,c :inherit modus-themes-bold)))
    `(howm-view-empty-face (( )))
    `(howm-view-hilit-face ((,c :background ,bg-search-static :foreground ,fg-search-static)))
    `(howm-view-name-face ((,c :inherit modus-themes-bold)))
    `(iigrep-counts-face1 ((,c :foreground ,rainbow-1)))
    `(iigrep-counts-face2 ((,c :foreground ,rainbow-2)))
    `(iigrep-counts-face3 ((,c :foreground ,rainbow-3)))
    `(iigrep-counts-face4 ((,c :foreground ,rainbow-4)))
    `(iigrep-counts-face5 ((,c :foreground ,rainbow-5)))
;;;;; hydra
    `(hydra-face-amaranth ((,c :inherit bold :foreground ,yellow-warmer)))
    `(hydra-face-blue ((,c :inherit bold :foreground ,blue)))
    `(hydra-face-pink ((,c :inherit bold :foreground ,magenta)))
    `(hydra-face-red ((,c :inherit bold :foreground ,red-faint)))
    `(hydra-face-teal ((,c :inherit bold :foreground ,cyan-cooler)))
;;;;; hyperbole
    `(hbut-item-face ((,c :foreground ,info)))
    `(hbut-face ((,c :inherit modus-themes-button)))
    `(hbut-flash ((,c :background ,bg-search-replace :foreground ,fg-search-replace)))
    `(ibut-face ((,c :background ,bg-link-symbolic :foreground ,fg-link-symbolic :underline ,underline-link-symbolic)))
;;;;; ibuffer
    `(ibuffer-deletion ((,c :inherit bold :background ,bg-mark-delete :foreground ,fg-mark-delete)))
    `(ibuffer-filter-group-name ((,c :inherit bold)))
    `(ibuffer-marked ((,c :inherit bold :background ,bg-mark-select :foreground ,fg-mark-select)))
    `(ibuffer-title ((,c :inherit bold)))
;;;;; icomplete
    `(icomplete-first-match ((,c :inherit modus-themes-completion-match-0)))
    `(icomplete-vertical-selected-prefix-indicator-face ((,c :inherit modus-themes-bold :foreground ,keybind)))
    `(icomplete-vertical-unselected-prefix-indicator-face ((,c :foreground ,fg-dim)))
    `(icomplete-selected-match ((,c :inherit modus-themes-completion-selected)))
;;;;; ido-mode
    `(ido-first-match ((,c :inherit modus-themes-completion-match-0)))
    `(ido-incomplete-regexp ((,c :foreground ,err)))
    `(ido-indicator ((,c :inherit modus-themes-bold)))
    `(ido-only-match ((,c :inherit modus-themes-completion-match-0)))
    `(ido-subdir ((,c :foreground ,keyword)))
    `(ido-virtual ((,c :foreground ,warning)))
;;;;; iedit
    `(iedit-occurrence ((,c :background ,bg-search-lazy :foreground ,fg-search-lazy)))
    `(iedit-read-only-occurrence ((,c :background ,bg-search-current :foreground ,fg-search-current)))
;;;;; iflipb
    `(iflipb-current-buffer-face ((,c :inherit modus-themes-bold :foreground ,name)))
    `(iflipb-other-buffer-face ((,c :foreground ,fg-dim)))
;;;;; image-dired
    `(image-dired-thumb-flagged ((,c :background ,bg-mark-delete :foreground ,fg-mark-delete :box (:line-width -3))))
    `(image-dired-thumb-header-file-name ((,c :inherit modus-themes-bold)))
    `(image-dired-thumb-header-file-size ((,c :foreground ,constant)))
    `(image-dired-thumb-mark ((,c :background ,bg-mark-select :foreground ,fg-mark-select :box (:line-width -3))))
;;;;; imenu-list
    `(imenu-list-entry-face-0 ((,c :foreground ,fg-heading-1)))
    `(imenu-list-entry-face-1 ((,c :foreground ,fg-heading-2)))
    `(imenu-list-entry-face-2 ((,c :foreground ,fg-heading-3)))
    `(imenu-list-entry-face-3 ((,c :foreground ,fg-heading-4)))
    `(imenu-list-entry-subalist-face-0 ((,c :inherit modus-themes-bold :foreground ,fg-heading-1 :underline t)))
    `(imenu-list-entry-subalist-face-1 ((,c :inherit modus-themes-bold :foreground ,fg-heading-2 :underline t)))
    `(imenu-list-entry-subalist-face-2 ((,c :inherit modus-themes-bold :foreground ,fg-heading-3 :underline t)))
    `(imenu-list-entry-subalist-face-3 ((,c :inherit modus-themes-bold :foreground ,fg-heading-4 :underline t)))
;;;;; info
    `(Info-quoted ((,c :inherit modus-themes-fixed-pitch :background ,bg-prose-verbatim :foreground ,fg-prose-verbatim))) ; the capitalization is canonical
    `(info-header-node ((,c :inherit modus-themes-bold :foreground ,fg-dim)))
    `(info-header-xref ((,c :foreground ,fg-link)))
    `(info-index-match ((,c :background ,bg-search-static :foreground ,fg-search-static)))
    `(info-menu-header ((,c :inherit modus-themes-heading-5)))
    `(info-menu-star ((,c :foreground ,err)))
    `(info-node ((,c :inherit modus-themes-bold)))
    `(info-title-1 ((,c :inherit modus-themes-heading-1)))
    `(info-title-2 ((,c :inherit modus-themes-heading-2)))
    `(info-title-3 ((,c :inherit modus-themes-heading-3)))
    `(info-title-4 ((,c :inherit modus-themes-heading-4)))
;;;;; info+ (info-plus)
    `(info-command-ref-item ((,c :foreground ,fnname)))
    `(info-constant-ref-item ((,c :foreground ,constant)))
    `(info-custom-delimited ((,c :inherit modus-themes-fixed-pitch :background ,bg-prose-verbatim :foreground ,fg-prose-verbatim)))
    `(info-double-quoted-name ((,c :foreground ,string)))
    `(info-file (( )))
    `(info-function-ref-item ((,c :foreground ,fnname)))
    `(info-glossary-word ((,c :inherit modus-themes-button)))
    `(info-indented-text (( )))
    `(info-isolated-backquote (( )))
    `(info-isolated-quote (( )))
    `(info-macro-ref-item ((,c :inherit modus-themes-bold :foreground ,keyword)))
    `(info-menu ((,c :inherit modus-themes-bold)))
    `(info-quoted-name ((,c :inherit modus-themes-fixed-pitch :background ,bg-prose-verbatim :foreground ,fg-prose-verbatim)))
    `(info-reference-item ((,c :inherit modus-themes-bold)))
    `(info-special-form-ref-item ((,c :foreground ,warning)))
    `(info-string ((,c :foreground ,string)))
    `(info-syntax-class-item ((,c :inherit modus-themes-fixed-pitch :background ,bg-prose-code :foreground ,fg-prose-code)))
    `(info-user-option-ref-item ((,c :foreground ,variable)))
    `(info-variable-ref-item ((,c :foreground ,variable)))
;;;;; info-colors
    `(info-colors-lisp-code-block ((,c :inherit modus-themes-fixed-pitch)))
    `(info-colors-ref-item-command ((,c :foreground ,fnname)))
    `(info-colors-ref-item-constant ((,c :foreground ,constant)))
    `(info-colors-ref-item-function ((,c :foreground ,fnname)))
    `(info-colors-ref-item-macro ((,c :inherit modus-themes-bold :foreground ,keyword)))
    `(info-colors-ref-item-other ((,c :inherit modus-themes-slant :foreground ,docstring)))
    `(info-colors-ref-item-special-form ((,c :inherit modus-themes-bold :foreground ,keyword)))
    `(info-colors-ref-item-syntax-class ((,c :inherit modus-themes-bold :foreground ,builtin)))
    `(info-colors-ref-item-type ((,c :inherit modus-themes-bold :foreground ,type)))
    `(info-colors-ref-item-user-option ((,c :foreground ,variable)))
    `(info-colors-ref-item-variable ((,c :foreground ,variable)))
;;;;; ioccur
    `(ioccur-cursor ((,c :foreground ,fg-main)))
    `(ioccur-invalid-regexp ((,c :foreground ,err)))
    `(ioccur-match-face ((,c :background ,bg-search-static :foreground ,fg-search-static)))
    `(ioccur-match-overlay-face ((,c :background ,bg-inactive :extend t)))
    `(ioccur-num-line-face ((,c :foreground ,fg-dim)))
    `(ioccur-overlay-face ((,c :background ,bg-hl-line :extend t)))
    `(ioccur-regexp-face ((,c :background ,bg-search-current :foreground ,fg-search-current)))
    `(ioccur-title-face ((,c :inherit modus-themes-bold :foreground ,name)))
;;;;; isearch, occur, and the like
    `(isearch ((,c :background ,bg-search-current :foreground ,fg-search-current)))
    `(isearch-fail ((,c :background ,bg-prominent-err :foreground ,fg-prominent-err)))
    `(isearch-group-1 ((,c :background ,bg-search-rx-group-0 :foreground ,fg-search-rx-group-0)))
    `(isearch-group-2 ((,c :background ,bg-search-rx-group-1 :foreground ,fg-search-rx-group-1)))
    `(lazy-highlight ((,c :background ,bg-search-lazy :foreground ,fg-search-lazy)))
    `(match ((,c :background ,bg-search-static :foreground ,fg-search-static)))
    `(query-replace ((,c :background ,bg-search-replace :foreground ,fg-search-replace)))
;;;;; ivy
    `(ivy-action ((,c :inherit (bold modus-themes-fixed-pitch) :foreground ,keybind)))
    `(ivy-confirm-face ((,c :foreground ,info)))
    `(ivy-current-match ((,c :inherit modus-themes-completion-selected)))
    `(ivy-match-required-face ((,c :foreground ,err)))
    `(ivy-minibuffer-match-face-1 (( )))
    `(ivy-minibuffer-match-face-2 ((,c :inherit modus-themes-completion-match-0)))
    `(ivy-minibuffer-match-face-3 ((,c :inherit modus-themes-completion-match-1)))
    `(ivy-minibuffer-match-face-4 ((,c :inherit modus-themes-completion-match-2)))
    `(ivy-remote ((,c :inherit modus-themes-slant)))
    `(ivy-separator ((,c :foreground ,fg-dim)))
    `(ivy-subdir ((,c :foreground ,keyword)))
    `(ivy-virtual ((,c :foreground ,warning)))
;;;;; ivy-posframe
    `(ivy-posframe-border ((,c :background ,border)))
    `(ivy-posframe-cursor ((,c :background ,fg-main :foreground ,bg-main)))
;;;;; jabber
    `(jabber-activity-face ((,c :foreground ,modeline-info)))
    `(jabber-roster-user-away ((,c :foreground ,fg-dim)))
    `(jabber-roster-user-xa ((,c :foreground ,fg-alt)))
    `(jabber-roster-user-dnd ((,c :foreground ,warning)))
    `(jabber-roster-user-chatty ((,c :foreground ,warning)))
    `(jabber-roster-user-error ((,c :foreground ,err)))
    `(jabber-roster-user-offline ((,c :foreground ,fg-dim)))
    `(jabber-roster-user-online ((,c :inherit modus-themes-bold :foreground ,info)))
    `(jabber-chat-prompt-foreign ((,c :inherit modus-themes-bold :foreground ,err)))
    `(jabber-chat-prompt-system ((,c :foreground ,warning)))
    `(jabber-chat-prompt-local ((,c :foreground ,info)))
    `(jabber-chat-error ((,c :foreground ,err)))
    `(jabber-activity-personal-face ((,c :foreground ,modeline-info)))
    `(jabber-rare-time-face ((,c :foreground ,date-common)))
    `(jabber-title-small ((,c :inherit modus-themes-heading-3)))
    `(jabber-title-medium ((,c :inherit modus-themes-heading-2)))
    `(jabber-title-large ((,c :inherit modus-themes-heading-1)))
;;;;; japanese-holidays
    `(japanese-holiday-saturday ((,c :foreground ,date-holiday-other)))
;;;;; jira (org-jira)
    `(jiralib-comment-face ((,c :background ,bg-inactive)))
    `(jiralib-comment-header-face ((,c :inherit modus-themes-bold)))
    `(jiralib-issue-info-face ((,c :background ,bg-inactive)))
    `(jiralib-issue-info-header-face ((,c :inherit modus-themes-bold :background ,bg-inactive)))
    `(jiralib-issue-summary-face ((,c :inherit modus-themes-bold)))
    `(jiralib-link-filter-face ((,c :inherit underline)))
    `(jiralib-link-issue-face ((,c :inherit underline)))
    `(jiralib-link-project-face ((,c :inherit underline)))
;;;;; jit-spell
    `(jit-spell-misspelling ((,c :underline (:style wave :color ,underline-err))))
;;;;; jinx
    `(jinx-misspelled ((,c :underline (:style wave :color ,underline-warning))))
;;;;; journalctl-mode
    `(journalctl-error-face ((,c :foreground ,err)))
    `(journalctl-finished-face ((,c :foreground ,info)))
    `(journalctl-host-face ((,c :foreground ,name)))
    `(journalctl-process-face ((,c :foreground ,warning)))
    `(journalctl-starting-face ((,c :foreground ,info)))
    `(journalctl-timestamp-face ((,c :foreground ,date-common)))
    `(journalctl-warning-face ((,c :foreground ,warning)))
;;;;; js2-mode
    `(js2-error ((,c :underline (:style wave :color ,underline-err))))
    `(js2-external-variable ((,c :foreground ,variable)))
    `(js2-function-call ((,c :foreground ,fnname)))
    `(js2-function-param ((,c :foreground ,constant)))
    `(js2-instance-member ((,c :inherit modus-themes-bold :foreground ,keyword)))
    `(js2-jsdoc-html-tag-delimiter ((,c :foreground ,fg-main)))
    `(js2-jsdoc-html-tag-name ((,c :foreground ,fnname)))
    `(js2-jsdoc-tag ((,c :inherit modus-themes-slant :foreground ,builtin)))
    `(js2-jsdoc-type ((,c :inherit modus-themes-slant :foreground ,type)))
    `(js2-jsdoc-value ((,c :inherit modus-themes-slant :foreground ,string)))
    `(js2-object-property ((,c :foreground ,fg-main)))
    `(js2-object-property-access ((,c :foreground ,fg-main)))
    `(js2-private-function-call ((,c :foreground ,preprocessor)))
    `(js2-private-member ((,c :inherit modus-themes-bold :foreground ,warning)))
    `(js2-warning ((,c :underline (:style wave :color ,underline-warning))))
;;;;; julia
    `(julia-macro-face ((,c :inherit modus-themes-bold :foreground ,builtin)))
    `(julia-quoted-symbol-face ((,c :foreground ,constant)))
;;;;; kaocha-runner
    `(kaocha-runner-error-face ((,c :foreground ,err)))
    `(kaocha-runner-success-face ((,c :foreground ,info)))
    `(kaocha-runner-warning-face ((,c :foreground ,warning)))
;;;;; keycast
    `(keycast-command ((,c :inherit bold)))
    `(keycast-key ((,c :inherit modus-themes-bold :background ,keybind :foreground ,bg-main)))
;;;;; kmacro-menu
    ;; Use `list' here to avoid a spurious warning about `kmacro-menu-mark'.
    (list 'kmacro-menu-mark `((,c :inherit modus-themes-bold)))
    `(kmacro-menu-marked ((,c :inherit bold :background ,bg-mark-select :foreground ,fg-mark-select)))
    `(kmacro-menu-flagged ((,c :inherit bold :background ,bg-mark-delete :foreground ,fg-mark-delete)))
;;;;; ledger-mode
    `(ledger-font-auto-xact-face ((,c :inherit modus-themes-bold :foreground ,builtin)))
    `(ledger-font-account-name-face ((,c :foreground ,name)))
    `(ledger-font-directive-face ((,c :inherit modus-themes-bold :foreground ,keyword)))
    `(ledger-font-posting-date-face ((,c :inherit modus-themes-bold :foreground ,date-common)))
    `(ledger-font-periodic-xact-face ((,c :foreground ,variable)))
    `(ledger-font-posting-amount-face ((,c :foreground ,constant)))
    `(ledger-font-payee-cleared-face ((,c :foreground ,info)))
    `(ledger-font-payee-pending-face ((,c :foreground ,warning)))
    `(ledger-font-payee-uncleared-face ((,c :foreground ,err)))
    `(ledger-font-xact-highlight-face ((,c :background ,bg-hl-line :extend t)))
;;;;; leerzeichen
    `(leerzeichen ((,c :background ,bg-inactive)))
;;;;; line numbers (display-line-numbers-mode and global variant)
    ;; Here we cannot inherit `modus-themes-fixed-pitch'.  We need to
    ;; fall back to `default' otherwise line numbers do not scale when
    ;; using `text-scale-adjust'.
    `(line-number ((,c :inherit ,(if modus-themes-mixed-fonts '(fixed-pitch default) 'default) :background ,bg-line-number-inactive :foreground ,fg-line-number-inactive)))
    `(line-number-current-line ((,c :inherit ,(if modus-themes-mixed-fonts '(bold fixed-pitch default) '(bold default)) :background ,bg-line-number-active :foreground ,fg-line-number-active)))
    `(line-number-major-tick ((,c :inherit ,(if modus-themes-mixed-fonts '(fixed-pitch default) 'default) :background ,bg-line-number-inactive :foreground ,accent-0)))
    `(line-number-minor-tick ((,c :inherit ,(if modus-themes-mixed-fonts '(fixed-pitch default) 'default) :background ,bg-line-number-inactive :foreground ,accent-1)))
;;;;; lsp
    `(lsp-details-face ((,c :height 0.9 :foreground ,fg-dim)))
    `(lsp-face-rename ((,c :background ,bg-search-replace :foreground ,fg-search-replace)))
    `(lsp-headerline-breadcrumb-separator-face ((,c :foreground ,fg-dim)))
    `(lsp-headerline-breadcrumb-path-face ((,c :foreground ,string)))
    `(lsp-headerline-breadcrumb-path-error-face ((,c :underline (:style wave :color ,underline-err))))
    `(lsp-headerline-breadcrumb-path-warning-face ((,c :underline (:style wave :color ,underline-warning))))
    `(lsp-headerline-breadcrumb-path-info-face ((,c :underline (:style wave :color ,underline-note))))
    `(lsp-headerline-breadcrumb-path-hint-face ((,c :underline (:style wave :color ,underline-note))))
    `(lsp-headerline-breadcrumb-project-prefix-face ((,c :inherit modus-themes-bold :foreground ,name)))
    `(lsp-headerline-breadcrumb-unknown-project-prefix-face ((,c :inherit modus-themes-bold :foreground ,fg-dim)))
    `(lsp-headerline-breadcrumb-symbols-face ((,c :inherit modus-themes-bold :foreground ,docstring)))
    `(lsp-headerline-breadcrumb-symbols-error-face ((,c :underline (:style wave :color ,underline-err))))
    `(lsp-headerline-breadcrumb-symbols-warning-face ((,c :underline (:style wave :color ,underline-warning))))
    `(lsp-headerline-breadcrumb-symbols-info-face ((,c :underline (:style wave :color ,underline-note))))
    `(lsp-headerline-breadcrumb-symbols-hint-face ((,c :underline (:style wave :color ,underline-note))))
    `(lsp-installation-buffer-face ((,c :foreground ,info)))
    `(lsp-installation-finished-buffer-face ((,c :foreground ,info)))
    `(lsp-lens-mouse-face ((,c :background ,bg-link :foreground ,fg-link :underline ,underline-link :height 0.9)))
    `(lsp-modeline-code-actions-face ((,c :foreground ,modeline-warning)))
    `(lsp-modeline-code-actions-preferred-face ((,c :foreground ,modeline-warning)))
    `(lsp-signature-posframe ((,c :background ,bg-dim :foreground ,fg-main)))
    (list 'lsp-signature-highlight-function-argument `((,c :inherit modus-themes-bold :background ,bg-active-argument :foreground ,fg-active-argument)))
;;;;; lsp-ui
    `(lsp-ui-doc-background ((,c :background ,bg-dim)))
    `(lsp-ui-doc-header ((,c :inherit modus-themes-bold)))
    `(lsp-ui-peek-peek ((,c :background ,bg-dim)))
    `(lsp-ui-peek-list ((,c :background ,bg-dim)))
    `(lsp-ui-peek-filename ((,c :inherit modus-themes-bold :foreground ,name)))
    `(lsp-ui-peek-line-number ((,c :foreground ,fg-dim)))
    `(lsp-ui-peek-highlight ((,c :background ,bg-dim ,@(modus-themes--box border -1 nil))))
    `(lsp-ui-peek-header ((,c :inherit modus-themes-bold)))
    `(lsp-ui-peek-selection ((,c :background ,bg-region :foreground ,fg-region)))
    `(lsp-ui-sideline-symbol ((,c :foreground ,fg-dim ,@(modus-themes--box border -1 nil))))
    `(lsp-ui-sideline-current-symbol ((,c :inherit modus-themes-bold :foreground ,fg-main ,@(modus-themes--box border -1 nil))))
    `(lsp-ui-sideline-code-action ((,c :foreground ,modeline-warning)))
    `(lsp-ui-sideline-symbol-info ((,c :inherit modus-themes-slant)))
;;;;; magit
    `(magit-bisect-bad ((,c :foreground ,err)))
    `(magit-bisect-good ((,c :foreground ,info)))
    `(magit-bisect-skip ((,c :foreground ,warning)))
    `(magit-blame-date (( )))
    `(magit-blame-dimmed ((,c :foreground ,fg-dim)))
    `(magit-blame-hash (( )))
    `(magit-blame-highlight ((,c :background ,bg-active :foreground ,fg-main)))
    `(magit-blame-name (( )))
    `(magit-blame-summary ((  )))
    `(magit-branch-local ((,c :foreground ,accent-0)))
    `(magit-branch-remote ((,c :foreground ,accent-1)))
    `(magit-branch-upstream ((,c :inherit modus-themes-slant)))
    `(magit-branch-warning ((,c :foreground ,warning)))
    `(magit-cherry-equivalent (( )))
    `(magit-cherry-unmatched ((,c :foreground ,err)))
    `(magit-diff-added ((,c :background ,bg-added-faint :foreground ,fg-added)))
    `(magit-diff-added-highlight ((,c :background ,bg-added :foreground ,fg-added)))
    `(magit-diff-base ((,c :background ,bg-changed-faint :foreground ,fg-changed)))
    `(magit-diff-base-highlight ((,c :background ,bg-changed :foreground ,fg-changed)))
    `(magit-diff-context ((,c :foreground ,fg-dim)))
    `(magit-diff-context-highlight ((,c :background ,bg-diff-context)))
    `(magit-diff-file-heading ((,c :inherit modus-themes-bold :foreground ,accent-0)))
    `(magit-diff-file-heading-highlight ((,c :inherit modus-themes-bold :background ,bg-inactive :foreground ,accent-0)))
    `(magit-diff-file-heading-selection ((,c :inherit modus-themes-bold :background ,bg-hover-secondary)))
    `(magit-diff-hunk-heading ((,c :background ,bg-inactive)))
    `(magit-diff-hunk-heading-highlight ((,c :inherit modus-themes-bold :background ,bg-active)))
    `(magit-diff-hunk-heading-selection ((,c :inherit modus-themes-bold :background ,bg-hover-secondary)))
    `(magit-diff-hunk-region ((,c :inherit modus-themes-bold)))
    `(magit-diff-lines-boundary ((,c :background ,fg-main)))
    `(magit-diff-lines-heading ((,c :background ,fg-dim :foreground ,bg-main)))
    `(magit-diff-removed ((,c :background ,bg-removed-faint :foreground ,fg-removed)))
    `(magit-diff-removed-highlight ((,c :background ,bg-removed :foreground ,fg-removed)))
    `(magit-diffstat-added ((,c :foreground ,fg-added-intense)))
    `(magit-diffstat-removed ((,c :foreground ,fg-removed-intense)))
    `(magit-dimmed ((,c :foreground ,fg-dim)))
    `(magit-filename ((,c :foreground ,accent-2)))
    `(magit-hash ((,c :foreground ,identifier)))
    `(magit-head ((,c :foreground ,accent-0)))
    `(magit-header-line ((,c :inherit modus-themes-bold)))
    `(magit-header-line-key ((,c :inherit (bold modus-themes-fixed-pitch) :foreground ,keybind)))
    `(magit-header-line-log-select ((,c :inherit modus-themes-bold)))
    `(magit-keyword ((,c :foreground ,keyword)))
    `(magit-keyword-squash ((,c :inherit modus-themes-bold :foreground ,warning)))
    `(magit-log-author ((,c :foreground ,name)))
    `(magit-log-date ((,c :foreground ,date-common)))
    `(magit-log-graph ((,c :foreground ,fg-dim)))
    `(magit-mode-line-process ((,c :inherit modus-themes-bold :foreground ,modeline-info)))
    `(magit-mode-line-process-error ((,c :inherit modus-themes-bold :foreground ,modeline-err)))
    `(magit-process-ng ((,c :foreground ,err)))
    `(magit-process-ok ((,c :foreground ,info)))
    `(magit-reflog-amend ((,c :foreground ,warning)))
    `(magit-reflog-checkout ((,c :inherit modus-themes-bold)))
    `(magit-reflog-cherry-pick ((,c :foreground ,info)))
    `(magit-reflog-commit ((,c :inherit modus-themes-bold)))
    `(magit-reflog-merge ((,c :foreground ,info)))
    `(magit-reflog-other ((,c :inherit modus-themes-bold :foreground ,accent-3)))
    `(magit-reflog-rebase ((,c :inherit modus-themes-bold :foreground ,accent-2)))
    `(magit-reflog-remote ((,c :inherit modus-themes-bold :foreground ,accent-1)))
    `(magit-reflog-reset ((,c :foreground ,err)))
    `(magit-refname ((,c :foreground ,fg-dim)))
    `(magit-refname-pullreq ((,c :foreground ,fg-dim)))
    `(magit-refname-stash ((,c :foreground ,fg-dim)))
    `(magit-refname-wip ((,c :foreground ,fg-dim)))
    `(magit-section ((,c :background ,bg-dim :foreground ,fg-main)))
    `(magit-section-heading ((,c :inherit modus-themes-bold :foreground ,fg-alt)))
    `(magit-section-heading-selection ((,c :inherit modus-themes-bold :background ,bg-hover-secondary)))
    `(magit-section-highlight ((,c :background ,bg-dim)))
    `(magit-section-secondary-heading ((,c :inherit modus-themes-bold)))
    `(magit-sequence-done ((,c :foreground ,info)))
    `(magit-sequence-drop ((,c :foreground ,err)))
    `(magit-sequence-exec ((,c :inherit modus-themes-bold)))
    `(magit-sequence-head ((,c :inherit modus-themes-bold :foreground ,accent-0)))
    `(magit-sequence-onto ((,c :inherit modus-themes-bold :foreground ,fg-dim)))
    `(magit-sequence-part ((,c :foreground ,warning)))
    `(magit-sequence-pick ((,c :inherit modus-themes-bold)))
    `(magit-sequence-stop ((,c :foreground ,err)))
    `(magit-signature-bad ((,c :foreground ,err)))
    `(magit-signature-error ((,c :foreground ,err)))
    `(magit-signature-expired ((,c :foreground ,warning)))
    `(magit-signature-expired-key ((,c :foreground ,warning)))
    `(magit-signature-good ((,c :foreground ,info)))
    `(magit-signature-revoked ((,c :inherit modus-themes-bold :foreground ,warning)))
    `(magit-signature-untrusted ((,c :inherit modus-themes-bold :foreground ,fg-dim)))
    `(magit-tag ((,c :foreground ,accent-3))) ; compare with branches
;;;;; make-mode (makefiles)
    `(makefile-makepp-perl ((,c :background ,bg-dim)))
    `(makefile-space ((,c :background ,bg-inactive)))
;;;;; man
    `(Man-overstrike ((,c :inherit bold :foreground ,accent-0)))
    `(Man-underline ((,c :foreground ,accent-1 :underline t)))
;;;;; marginalia
    `(marginalia-archive ((,c :foreground ,accent-0)))
    `(marginalia-char ((,c :foreground ,accent-2)))
    `(marginalia-date ((,c :foreground ,date-common)))
    `(marginalia-documentation ((,c :inherit modus-themes-slant :foreground ,docstring)))
    `(marginalia-file-name ((,c :foreground ,fg-dim)))
    `(marginalia-file-owner ((,c :foreground ,fg-dim)))
    `(marginalia-file-priv-dir ((,c :foreground ,accent-0)))
    `(marginalia-file-priv-exec ((,c :foreground ,accent-1)))
    `(marginalia-file-priv-link ((,c :foreground ,fg-link)))
    `(marginalia-file-priv-no ((,c :foreground ,fg-dim)))
    `(marginalia-file-priv-other ((,c :foreground ,accent-2)))
    `(marginalia-file-priv-rare ((,c :foreground ,accent-3)))
    `(marginalia-file-priv-read ((,c :foreground ,fg-main)))
    `(marginalia-file-priv-write ((,c :foreground ,accent-0)))
    `(marginalia-function ((,c :foreground ,fnname)))
    `(marginalia-key ((,c :inherit (bold modus-themes-fixed-pitch) :foreground ,keybind)))
    `(marginalia-lighter ((,c :foreground ,fg-dim)))
    `(marginalia-liqst ((,c :foreground ,fg-dim)))
    `(marginalia-mode ((,c :foreground ,constant)))
    `(marginalia-modified ((,c :foreground ,warning)))
    `(marginalia-null ((,c :foreground ,fg-dim)))
    `(marginalia-number ((,c :foreground ,constant)))
    `(marginalia-size ((,c :foreground ,variable)))
    `(marginalia-string ((,c :foreground ,string)))
    `(marginalia-symbol ((,c :foreground ,builtin)))
    `(marginalia-true (( )))
    `(marginalia-type ((,c :foreground ,type)))
    `(marginalia-value ((,c :foreground ,fg-dim)))
    `(marginalia-version ((,c :foreground ,date-common)))
;;;;; markdown-mode
    `(markdown-blockquote-face ((,c :inherit modus-themes-slant :foreground ,docstring)))
    `(markdown-bold-face ((,c :inherit bold)))
    `(markdown-code-face ((,c :inherit modus-themes-fixed-pitch :background ,bg-prose-block-contents :extend t)))
    `(markdown-gfm-checkbox-face ((,c :foreground ,warning)))
    `(markdown-header-face (( )))
    `(markdown-header-face-1 ((,c :inherit modus-themes-heading-1)))
    `(markdown-header-face-2 ((,c :inherit modus-themes-heading-2)))
    `(markdown-header-face-3 ((,c :inherit modus-themes-heading-3)))
    `(markdown-header-face-4 ((,c :inherit modus-themes-heading-4)))
    `(markdown-header-face-5 ((,c :inherit modus-themes-heading-5)))
    `(markdown-header-face-6 ((,c :inherit modus-themes-heading-6)))
    `(markdown-highlighting-face ((,c :background ,bg-hover-secondary :foreground ,fg-main)))
    `(markdown-inline-code-face ((,c :inherit modus-themes-fixed-pitch :background ,bg-prose-code :foreground ,fg-prose-code)))
    `(markdown-italic-face ((,c :inherit italic)))
    `(markdown-language-keyword-face ((,c :inherit modus-themes-fixed-pitch :background ,bg-prose-block-delimiter :foreground ,fg-prose-block-delimiter)))
    `(markdown-line-break-face ((,c :foreground ,err :underline t)))
    `(markdown-link-face ((,c :background ,bg-link :foreground ,fg-link :underline ,underline-link)))
    `(markdown-markup-face ((,c :foreground ,fg-dim)))
    `(markdown-metadata-key-face ((,c :inherit modus-themes-bold)))
    `(markdown-metadata-value-face ((,c :foreground ,string)))
    `(markdown-missing-link-face ((,c :foreground ,warning)))
    `(markdown-pre-face ((,c :inherit modus-themes-fixed-pitch :background ,bg-prose-block-contents :extend t)))
    `(markdown-table-face ((,c :inherit modus-themes-fixed-pitch :foreground ,prose-table)))
    `(markdown-url-face ((,c :foreground ,fg-alt)))
;;;;; markup-faces (`adoc-mode')
    `(markup-attribute-face ((,c :inherit modus-themes-fixed-pitch :foreground ,fg-dim)))
    `(markup-bold-face ((,c :inherit bold)))
    `(markup-code-face ((,c :inherit modus-themes-fixed-pitch :background ,bg-prose-code :foreground ,fg-prose-code)))
    `(markup-comment-face ((,c :inherit modus-themes-slant :foreground ,comment)))
    `(markup-complex-replacement-face ((,c :inherit modus-themes-fixed-pitch :background ,bg-prose-macro :foreground ,fg-prose-macro)))
    `(markup-emphasis-face ((,c :inherit markup-italic-face)))
    `(markup-error-face ((,c :foreground ,err)))
    `(markup-gen-face ((,c :inherit modus-themes-fixed-pitch :background ,bg-prose-verbatim :foreground ,fg-prose-verbatim)))
    `(markup-internal-reference-face ((,c :inherit modus-themes-slant :foreground ,fg-dim)))
    `(markup-italic-face ((,c :inherit modus-themes-slant)))
    `(markup-list-face ((,c :background ,bg-inactive)))
    `(markup-meta-face ((,c :inherit modus-themes-fixed-pitch :foreground ,fg-dim)))
    `(markup-meta-hide-face ((,c :foreground "gray50")))
    `(markup-reference-face ((,c :inherit modus-themes-slant :foreground ,fg-alt)))
    `(markup-replacement-face ((,c :inherit modus-themes-fixed-pitch :foreground ,err)))
    `(markup-secondary-text-face ((,c :height 0.9 :foreground ,fg-alt)))
    `(markup-small-face ((,c :inherit modus-themes-fixed-pitch :background ,bg-prose-verbatim :foreground ,fg-prose-verbatim :height 0.9)))
    `(markup-strong-face ((,c :inherit bold)))
    `(markup-subscript-face ((,c :height 0.9 :foreground ,fg-alt)))
    `(markup-superscript-face ((,c :height 0.9 :foreground ,fg-alt)))
    `(markup-table-cell-face (( )))
    `(markup-table-face ((,c :foreground ,prose-table)))
    `(markup-table-row-face (( )))
    `(markup-title-0-face ((,c :inherit modus-themes-heading-1)))
    `(markup-title-1-face ((,c :inherit modus-themes-heading-2)))
    `(markup-title-2-face ((,c :inherit modus-themes-heading-3)))
    `(markup-title-3-face ((,c :inherit modus-themes-heading-4)))
    `(markup-title-4-face ((,c :inherit modus-themes-heading-5)))
    `(markup-title-5-face ((,c :inherit modus-themes-heading-6)))
    `(markup-verbatim-face ((,c :inherit modus-themes-fixed-pitch :background ,bg-prose-verbatim :foreground ,fg-prose-verbatim)))
;;;;; mbdepth
    `(minibuffer-depth-indicator ((,c :inverse-video t)))
;;;;; mct
    `(mct-highlight-candidate ((,c :inherit modus-themes-completion-selected)))
;;;;; messages
    `(message-cited-text-1 ((,c :foreground ,mail-cite-0)))
    `(message-cited-text-2 ((,c :foreground ,mail-cite-1)))
    `(message-cited-text-3 ((,c :foreground ,mail-cite-2)))
    `(message-cited-text-4 ((,c :foreground ,mail-cite-3)))
    `(message-header-name ((,c :inherit modus-themes-bold)))
    `(message-header-newsgroups ((,c :foreground ,mail-other)))
    `(message-header-to ((,c :inherit modus-themes-bold :foreground ,mail-recipient)))
    `(message-header-cc ((,c :foreground ,mail-recipient)))
    `(message-header-subject ((,c :inherit modus-themes-bold :foreground ,mail-subject)))
    `(message-header-xheader ((,c :foreground ,mail-other)))
    `(message-header-other ((,c :foreground ,mail-other)))
    `(message-mml ((,c :foreground ,mail-part)))
    `(message-separator ((,c :background ,bg-inactive :foreground ,fg-main)))
;;;;; minimap
    `(minimap-active-region-background ((,c :background ,bg-active)))
    `(minimap-current-line-face ((,c :background ,bg-cyan-intense :foreground ,fg-main)))
;;;;; mode-line
    `(mode-line
      ((default :inherit modus-themes-ui-variable-pitch
                :background ,bg-mode-line-active
                :foreground ,fg-mode-line-active)
       (((supports :box t))
        ,@(modus-themes--box border-mode-line-active nil nil))
       (t :underline ,border-mode-line-active)))
    `(mode-line-active
      ((default :inherit modus-themes-ui-variable-pitch
                :background ,bg-mode-line-active
                :foreground ,fg-mode-line-active)
       (((supports :box t))
        ,@(modus-themes--box border-mode-line-active nil nil))
       (t :underline ,border-mode-line-active)))
    `(mode-line-buffer-id ((,c :inherit bold)))
    `(mode-line-emphasis ((,c :inherit italic :foreground ,modeline-info)))
    `(mode-line-highlight
      ((default :background ,bg-hover :foreground ,fg-main)
       (((supports :box t))
        ,@(modus-themes--box fg-main nil nil))
       (t :underline ,fg-main)))
    `(mode-line-inactive
      ((default :inherit modus-themes-ui-variable-pitch
                :background ,bg-mode-line-inactive
                :foreground ,fg-mode-line-inactive)
       (((supports :box t))
        ,@(modus-themes--box border-mode-line-inactive nil nil))
       (t :underline ,border-mode-line-inactive)))
;;;;; mood-line
    `(mood-line-modified ((,c :inherit modus-themes-slant)))
    `(mood-line-status-error ((,c :foreground ,err)))
    `(mood-line-status-info ((,c :foreground ,info)))
    `(mood-line-status-neutral (( )))
    `(mood-line-status-success ((,c :foreground ,info)))
    `(mood-line-status-warning ((,c :foreground ,warning)))
    `(mood-line-unimportant ((,c :foreground ,fg-dim)))
;;;;; mpdel
    `(mpdel-browser-directory-face ((,c :foreground ,accent-0)))
    `(mpdel-playlist-current-song-face ((,c :inherit modus-themes-bold :foreground ,accent-0)))
;;;;; mu4e
    `(mu4e-attach-number-face ((,c :inherit modus-themes-bold :foreground ,fg-dim)))
    `(mu4e-cited-1-face ((,c :foreground ,mail-cite-0)))
    `(mu4e-cited-2-face ((,c :foreground ,mail-cite-1)))
    `(mu4e-cited-3-face ((,c :foreground ,mail-cite-2)))
    `(mu4e-cited-4-face ((,c :foreground ,mail-cite-3)))
    `(mu4e-cited-5-face ((,c :foreground ,mail-cite-0)))
    `(mu4e-cited-6-face ((,c :foreground ,mail-cite-1)))
    `(mu4e-cited-7-face ((,c :foreground ,mail-cite-2)))
    `(mu4e-compose-header-face ((,c :background ,bg-inactive :foreground ,fg-main)))
    `(mu4e-compose-separator-face ((,c :background ,bg-inactive :foreground ,fg-main)))
    `(mu4e-contact-face ((,c :inherit modus-themes-bold :foreground ,mail-recipient)))
    `(mu4e-context-face ((,c :inherit modus-themes-bold)))
    `(mu4e-draft-face ((,c :foreground ,warning)))
    `(mu4e-flagged-face ((,c :foreground ,keyword)))
    `(mu4e-footer-face ((,c :inherit modus-themes-slant :foreground ,fg-alt)))
    `(mu4e-forwarded-face ((,c :inherit modus-themes-slant :foreground ,info)))
    `(mu4e-header-face ((,c :foreground ,fg-dim)))
    `(mu4e-header-highlight-face ((,c :background ,bg-hl-line :extend t)))
    `(mu4e-header-key-face ((,c :inherit modus-themes-bold)))
    `(mu4e-header-marks-face ((,c :inherit modus-themes-bold :foreground ,mail-subject)))
    `(mu4e-header-title-face ((,c :foreground ,fg-alt)))
    `(mu4e-header-value-face ((,c :foreground ,mail-other)))
    `(mu4e-highlight-face ((,c :inherit (bold modus-themes-fixed-pitch) :foreground ,keybind)))
    `(mu4e-link-face ((,c :background ,bg-link :foreground ,fg-link :underline ,underline-link)))
    `(mu4e-modeline-face (( )))
    `(mu4e-moved-face ((,c :inherit modus-themes-slant :foreground ,warning)))
    `(mu4e-ok-face ((,c :foreground ,info)))
    `(mu4e-region-code ((,c :foreground ,builtin)))
    `(mu4e-related-face ((,c :inherit modus-themes-slant :foreground ,fg-dim)))
    `(mu4e-replied-face ((,c :foreground ,info)))
    `(mu4e-special-header-value-face ((,c :inherit modus-themes-bold :foreground ,mail-subject)))
    `(mu4e-system-face ((,c :inherit modus-themes-slant)))
    `(mu4e-thread-fold-face ((,c :foreground ,border)))
    `(mu4e-title-face (( )))
    `(mu4e-trashed-face ((,c :foreground ,err)))
    `(mu4e-unread-face ((,c :inherit bold)))
    `(mu4e-url-number-face ((,c :foreground ,fg-dim)))
    `(mu4e-view-body-face (( )))
    `(mu4e-warning-face ((,c :foreground ,warning)))
;;;;; multiple-cursors
    `(mc/cursor-bar-face ((,c :height 1 :foreground ,fg-main :background ,bg-main)))
    `(mc/cursor-face ((,c :inverse-video t)))
    `(mc/region-face ((,c :background ,bg-region :foreground ,fg-region)))
;;;;; nerd-icons
    `(nerd-icons-blue ((,c :foreground ,blue-cooler)))
    `(nerd-icons-blue-alt ((,c :foreground ,blue-warmer)))
    `(nerd-icons-cyan ((,c :foreground ,cyan)))
    `(nerd-icons-cyan-alt ((,c :foreground ,cyan-warmer)))
    `(nerd-icons-dblue ((,c :foreground ,blue-faint)))
    `(nerd-icons-dcyan ((,c :foreground ,cyan-faint)))
    `(nerd-icons-dgreen ((,c :foreground ,green-faint)))
    `(nerd-icons-dmaroon ((,c :foreground ,magenta-faint)))
    `(nerd-icons-dorange ((,c :foreground ,red-faint)))
    `(nerd-icons-dpink ((,c :foreground ,magenta-faint)))
    `(nerd-icons-dpurple ((,c :foreground ,magenta-cooler)))
    `(nerd-icons-dred ((,c :foreground ,red)))
    `(nerd-icons-dsilver ((,c :foreground ,cyan-faint)))
    `(nerd-icons-dyellow ((,c :foreground ,yellow-faint)))
    `(nerd-icons-green ((,c :foreground ,green)))
    `(nerd-icons-lblue ((,c :foreground ,blue-cooler)))
    `(nerd-icons-lcyan ((,c :foreground ,cyan)))
    `(nerd-icons-lgreen ((,c :foreground ,green-warmer)))
    `(nerd-icons-lmaroon ((,c :foreground ,magenta-warmer)))
    `(nerd-icons-lorange ((,c :foreground ,red-warmer)))
    `(nerd-icons-lpink ((,c :foreground ,magenta)))
    `(nerd-icons-lpurple ((,c :foreground ,magenta-faint)))
    `(nerd-icons-lred ((,c :foreground ,red-faint)))
    `(nerd-icons-lsilver ((,c :foreground "gray50")))
    `(nerd-icons-lyellow ((,c :foreground ,yellow-warmer)))
    `(nerd-icons-maroon ((,c :foreground ,magenta)))
    `(nerd-icons-orange ((,c :foreground ,yellow-warmer)))
    `(nerd-icons-pink ((,c :foreground ,magenta-warmer)))
    `(nerd-icons-purple ((,c :foreground ,magenta-cooler)))
    `(nerd-icons-purple-alt ((,c :foreground ,blue-warmer)))
    `(nerd-icons-red ((,c :foreground ,red)))
    `(nerd-icons-red-alt ((,c :foreground ,red-cooler)))
    `(nerd-icons-silver ((,c :foreground "gray50")))
    `(nerd-icons-yellow ((,c :foreground ,yellow)))
;;;;; nerd-icons-completion
    `(nerd-icons-completion-dir-face ((,c :foreground ,accent-0)))
;;;;; nerd-icons-dired
    `(nerd-icons-dired-dir-face ((,c :foreground ,accent-0)))
;;;;; nerd-icons-ibuffer
    `(nerd-icons-ibuffer-dir-face ((,c :foreground ,accent-0)))
    `(nerd-icons-ibuffer-file-face ((,c :foreground ,docstring)))
    `(nerd-icons-ibuffer-mode-face ((,c :foreground ,type)))
    `(nerd-icons-ibuffer-size-face ((,c :foreground ,variable)))
;;;;; neotree
    `(neo-banner-face ((,c :foreground ,accent-0)))
    `(neo-button-face ((,c :background ,bg-link :foreground ,fg-link :underline ,underline-link)))
    `(neo-dir-link-face (( )))
    `(neo-expand-btn-face (( )))
    `(neo-file-link-face (( )))
    `(neo-header-face ((,c :inherit modus-themes-bold)))
    `(neo-root-dir-face ((,c :inherit modus-themes-bold :foreground ,accent-0)))
    `(neo-vc-added-face ((,c :foreground ,info)))
    `(neo-vc-conflict-face ((,c :foreground ,err)))
    `(neo-vc-default-face (( )))
    `(neo-vc-edited-face ((,c :inherit modus-themes-slant)))
    `(neo-vc-ignored-face ((,c :foreground ,fg-dim)))
    `(neo-vc-missing-face ((,c :foreground ,err)))
    `(neo-vc-needs-merge-face ((,c :inherit modus-themes-slant)))
    `(neo-vc-needs-update-face ((,c :inherit underline)))
    `(neo-vc-removed-face ((,c :underline (:style wave :color ,underline-err) :foreground ,err)))
    `(neo-vc-unlocked-changes-face ((,c :foreground ,info)))
    `(neo-vc-up-to-date-face (( )))
    `(neo-vc-user-face ((,c :foreground ,warning)))
;;;;; notmuch
    `(notmuch-crypto-decryption ((,c :inherit modus-themes-bold)))
    `(notmuch-crypto-part-header ((,c :foreground ,mail-part))) ; like `message-mml'
    `(notmuch-crypto-signature-bad ((,c :foreground ,err)))
    `(notmuch-crypto-signature-good ((,c :foreground ,info)))
    `(notmuch-crypto-signature-good-key ((,c :foreground ,info)))
    `(notmuch-crypto-signature-unknown ((,c :foreground ,warning)))
    `(notmuch-jump-key ((,c :inherit (bold modus-themes-fixed-pitch) :foreground ,keybind)))
    `(notmuch-message-summary-face
      ((default :inherit modus-themes-bold :background ,bg-inactive)
       (((supports :overline t))
        :overline ,border)))
    `(notmuch-search-count ((,c :foreground ,fg-dim)))
    `(notmuch-search-date ((,c :foreground ,date-common)))
    `(notmuch-search-flagged-face ((,c :foreground ,keyword)))
    `(notmuch-search-matching-authors ((,c :foreground ,mail-recipient)))
    `(notmuch-search-non-matching-authors ((,c :foreground ,fg-dim)))
    `(notmuch-search-subject ((,c :foreground ,fg-main)))
    `(notmuch-search-unread-face ((,c :inherit bold)))
    `(notmuch-tag-added ((,c :underline (:style wave :color ,underline-note) :foreground ,info)))
    `(notmuch-tag-deleted ((,c :underline (:style wave :color ,underline-err) :foreground ,err)))
    `(notmuch-tag-face ((,c :foreground ,accent-0)))
    `(notmuch-tag-flagged ((,c :foreground ,keyword)))
    `(notmuch-tag-unread ((,c :foreground ,accent-1)))
    `(notmuch-tree-match-author-face ((,c :foreground ,mail-recipient)))
    `(notmuch-tree-match-date-face ((,c :foreground ,date-common)))
    `(notmuch-tree-match-face ((,c :foreground ,fg-dim)))
    `(notmuch-tree-match-subject-face ((,c :foreground ,fg-main)))
    `(notmuch-tree-match-tag-face ((,c :foreground ,accent-0)))
    `(notmuch-tree-no-match-face ((,c :foreground ,fg-dim)))
    `(notmuch-tree-no-match-date-face ((,c :foreground ,fg-dim)))
    `(notmuch-wash-cited-text ((,c :foreground ,mail-cite-0)))
    `(notmuch-wash-toggle-button ((,c :background ,bg-dim)))
;;;;; num3-mode
    `(num3-face-even ((,c :inherit modus-themes-bold :background ,bg-inactive)))
;;;;; nxml-mode
    `(nxml-attribute-colon ((,c :foreground ,fg-main)))
    `(nxml-attribute-local-name ((,c :foreground ,variable)))
    `(nxml-attribute-prefix ((,c :inherit modus-themes-bold :foreground ,type)))
    `(nxml-attribute-value ((,c :foreground ,constant)))
    `(nxml-cdata-section-CDATA ((,c :foreground ,err)))
    `(nxml-cdata-section-delimiter ((,c :foreground ,err)))
    `(nxml-char-ref-delimiter ((,c :foreground ,fg-dim)))
    `(nxml-char-ref-number ((,c :inherit modus-themes-bold :foreground ,fg-dim)))
    `(nxml-delimited-data ((,c :inherit modus-themes-slant :foreground ,fg-dim)))
    `(nxml-delimiter ((,c :foreground ,fg-dim)))
    `(nxml-element-colon ((,c :foreground ,fg-main)))
    `(nxml-element-local-name ((,c :foreground ,fnname)))
    `(nxml-element-prefix ((,c :inherit modus-themes-bold :foreground ,builtin)))
    `(nxml-entity-ref-delimiter ((,c :foreground ,fg-dim)))
    `(nxml-entity-ref-name ((,c :inherit modus-themes-bold :foreground ,fg-dim)))
    `(nxml-glyph ((,c :background ,bg-active :foreground ,fg-main)))
    `(nxml-hash ((,c :inherit modus-themes-bold :foreground ,string)))
    `(nxml-heading ((,c :inherit modus-themes-bold)))
    `(nxml-name ((,c :inherit modus-themes-bold :foreground ,builtin)))
    `(nxml-namespace-attribute-colon ((,c :foreground ,fg-main)))
    `(nxml-namespace-attribute-prefix ((,c :foreground ,variable)))
    `(nxml-processing-instruction-target ((,c :inherit modus-themes-bold :foreground ,keyword)))
    `(nxml-prolog-keyword ((,c :inherit modus-themes-bold :foreground ,keyword)))
    `(nxml-ref ((,c :inherit modus-themes-bold :foreground ,fg-dim)))
    `(rng-error ((,c :foreground ,err)))
;;;;; olivetti
    `(olivetti-fringe ((,c :background ,fringe)))
;;;;; orderless
    `(orderless-match-face-0 ((,c :inherit modus-themes-completion-match-0)))
    `(orderless-match-face-1 ((,c :inherit modus-themes-completion-match-1)))
    `(orderless-match-face-2 ((,c :inherit modus-themes-completion-match-2)))
    `(orderless-match-face-3 ((,c :inherit modus-themes-completion-match-3)))
;;;;; org
    `(org-agenda-calendar-daterange ((,c :foreground ,date-range)))
    `(org-agenda-calendar-event ((,c :foreground ,date-event)))
    `(org-agenda-calendar-sexp ((,c :inherit modus-themes-slant :foreground ,date-event)))
    `(org-agenda-clocking ((,c :inherit modus-themes-bold :background ,bg-active-argument :foreground ,fg-active-argument)))
    `(org-agenda-column-dateline ((,c :background ,bg-inactive)))
    `(org-agenda-current-time ((,c :foreground ,date-now)))
    `(org-agenda-date ((,c ,@(modus-themes--heading 'agenda-date date-weekday))))
    `(org-agenda-date-today ((,c ,@(modus-themes--heading 'agenda-date date-weekday) :underline t)))
    `(org-agenda-date-weekend ((,c ,@(modus-themes--heading 'agenda-date date-weekend))))
    `(org-agenda-date-weekend-today ((,c ,@(modus-themes--heading 'agenda-date date-weekend) :underline t)))
    `(org-agenda-diary ((,c :inherit modus-themes-slant :foreground ,date-event)))
    `(org-agenda-dimmed-todo-face ((,c :foreground ,fg-dim)))
    `(org-agenda-done ((,c :foreground ,prose-done)))
    `(org-agenda-filter-category ((,c :inherit modus-themes-bold :foreground ,modeline-err)))
    `(org-agenda-filter-effort ((,c :inherit modus-themes-bold :foreground ,modeline-err)))
    `(org-agenda-filter-regexp ((,c :inherit modus-themes-bold :foreground ,modeline-err)))
    `(org-agenda-filter-tags ((,c :inherit modus-themes-bold :foreground ,modeline-err)))
    `(org-agenda-restriction-lock ((,c :background ,bg-dim :foreground ,fg-dim)))
    `(org-agenda-structure ((,c ,@(modus-themes--heading 'agenda-structure fg-alt))))
    `(org-agenda-structure-filter ((,c ,@(modus-themes--heading 'agenda-structure warning))))
    `(org-agenda-structure-secondary ((,c :inherit modus-themes-slant :foreground ,docstring)))
    `(org-archived ((,c :background ,bg-inactive :foreground ,fg-main)))
    `(org-block ((,c :inherit modus-themes-fixed-pitch :background ,bg-prose-block-contents :extend t)))
    `(org-block-begin-line ((,c :inherit modus-themes-fixed-pitch :background ,bg-prose-block-delimiter :foreground ,fg-prose-block-delimiter :extend t)))
    `(org-block-end-line ((,c :inherit modus-themes-fixed-pitch :background ,bg-prose-block-delimiter :foreground ,fg-prose-block-delimiter :extend t)))
    `(org-checkbox ((,c :inherit modus-themes-fixed-pitch :foreground ,warning)))
    `(org-checkbox-statistics-done ((,c :foreground ,prose-done)))
    `(org-checkbox-statistics-todo ((,c :foreground ,prose-todo)))
    `(org-clock-overlay ((,c :background ,bg-hover-secondary :foreground ,fg-main)))
    `(org-code ((,c :inherit modus-themes-fixed-pitch :background ,bg-prose-code :foreground ,fg-prose-code)))
    `(org-column ((,c :inherit default :background ,bg-dim)))
    `(org-column-title ((,c :inherit (modus-themes-fixed-pitch bold default) :underline t :background ,bg-dim)))
    `(org-date ((,c :inherit modus-themes-fixed-pitch :foreground ,date-common)))
    `(org-date-selected
      ((default :foreground ,date-common :inverse-video t)
       (((supports :box t))
        ,@(modus-themes--box fg-main '(-1 . -1) nil))))
    ;; NOTE 2024-03-17: Normally we do not want to add this padding
    ;; with the :box, but I do it here because the keys are otherwise
    ;; very hard to read.  The square brackets around them are not
    ;; colored, which is what is causing the problem.
    `(org-dispatcher-highlight
      ((default :inherit bold :background ,bg-mark-select :foreground ,fg-mark-select)
       (((supports :box t))
        ,@(modus-themes--box bg-mark-select 2 nil))
       (t :underline ,border)))
    `(org-document-info ((,c :foreground ,prose-metadata-value)))
    `(org-document-info-keyword ((,c :inherit modus-themes-fixed-pitch :foreground ,prose-metadata)))
    `(org-document-title ((,c :inherit modus-themes-heading-0)))
    `(org-done ((,c :foreground ,prose-done)))
    `(org-drawer ((,c :inherit modus-themes-fixed-pitch :foreground ,prose-metadata)))
    `(org-ellipsis (( ))) ; inherits from the heading's color
    `(org-footnote ((,c :background ,bg-link :foreground ,fg-link :underline ,underline-link)))
    `(org-formula ((,c :inherit modus-themes-fixed-pitch :foreground ,prose-table-formula)))
    `(org-headline-done ((,c :foreground ,prose-done)))
    `(org-headline-todo ((,c :foreground ,prose-todo)))
    `(org-hide ((,c :foreground ,bg-main)))
    `(org-indent ((,c :inherit fixed-pitch :foreground ,bg-main)))
    `(org-imminent-deadline ((,c :inherit modus-themes-bold :foreground ,date-deadline)))
    `(org-latex-and-related ((,c :foreground ,type)))
    `(org-level-1 ((,c :inherit modus-themes-heading-1)))
    `(org-level-2 ((,c :inherit modus-themes-heading-2)))
    `(org-level-3 ((,c :inherit modus-themes-heading-3)))
    `(org-level-4 ((,c :inherit modus-themes-heading-4)))
    `(org-level-5 ((,c :inherit modus-themes-heading-5)))
    `(org-level-6 ((,c :inherit modus-themes-heading-6)))
    `(org-level-7 ((,c :inherit modus-themes-heading-7)))
    `(org-level-8 ((,c :inherit modus-themes-heading-8)))
    `(org-link ((,c :background ,bg-link :foreground ,fg-link :underline ,underline-link)))
    `(org-list-dt ((,c :inherit modus-themes-bold :foreground ,fg-alt)))
    `(org-macro ((,c :inherit modus-themes-fixed-pitch :background ,bg-prose-macro :foreground ,fg-prose-macro)))
    `(org-meta-line ((,c :inherit modus-themes-fixed-pitch :foreground ,prose-metadata)))
    `(org-mode-line-clock (( )))
    `(org-mode-line-clock-overrun ((,c :inherit modus-themes-bold :foreground ,modeline-err)))
    `(org-priority ((,c :foreground ,prose-tag)))
    `(org-property-value ((,c :inherit modus-themes-fixed-pitch :foreground ,prose-metadata-value)))
    `(org-quote ((,c :inherit modus-themes-fixed-pitch :background ,bg-prose-block-contents :extend t)))
    `(org-scheduled ((,c :foreground ,date-scheduled-subtle)))
    `(org-scheduled-previously ((,c :inherit (bold org-scheduled-today))))
    `(org-scheduled-today ((,c :foreground ,date-scheduled)))
    `(org-sexp-date ((,c :foreground ,date-common)))
    `(org-special-keyword ((,c :inherit modus-themes-fixed-pitch :foreground ,prose-metadata)))
    `(org-table ((,c :inherit modus-themes-fixed-pitch :foreground ,prose-table)))
    `(org-table-header ((,c :inherit (bold modus-themes-fixed-pitch) :foreground ,prose-table)))
    `(org-tag ((,c :foreground ,prose-tag)))
    `(org-tag-group ((,c :inherit modus-themes-bold :foreground ,prose-tag)))
    `(org-target ((,c :inherit underline)))
    `(org-time-grid ((,c :foreground ,fg-dim)))
    `(org-todo ((,c :foreground ,prose-todo)))
    `(org-upcoming-deadline ((,c :foreground ,date-deadline-subtle)))
    `(org-upcoming-distant-deadline ((,c :foreground ,fg-main)))
    `(org-verbatim ((,c :inherit modus-themes-fixed-pitch :background ,bg-prose-verbatim :foreground ,fg-prose-verbatim)))
    `(org-verse ((,c :inherit modus-themes-fixed-pitch :background ,bg-prose-block-contents :extend t)))
    `(org-warning ((,c :foreground ,warning)))
;;;;; org-habit
    ;; NOTE 2025-11-12: We used to have `readable-foreground-color'
    ;; for the foreground values of these faces, but that function
    ;; breaks the theme if it is loaded in the early-init.el.  Maybe
    ;; we can find a better solution.  I do not want to introduce new
    ;; palette entries or a new function just for these faces though.
    `(org-habit-alert-face ((,c :background ,bg-graph-yellow-0)))
    `(org-habit-alert-future-face ((,c :background ,bg-graph-yellow-1)))
    `(org-habit-clear-face ((,c :background ,bg-graph-blue-0)))
    `(org-habit-clear-future-face ((,c :background ,bg-graph-blue-1)))
    `(org-habit-overdue-face ((,c :background ,bg-graph-red-0)))
    `(org-habit-overdue-future-face ((,c :background ,bg-graph-red-1)))
    `(org-habit-ready-face ((,c :background ,bg-graph-green-0)))
    `(org-habit-ready-future-face ((,c :background ,bg-graph-green-1)))
;;;;; org-journal
    `(org-journal-calendar-entry-face ((,c :inherit modus-themes-slant :foreground ,date-common)))
    `(org-journal-calendar-scheduled-face ((,c :inherit modus-themes-slant :foreground ,date-scheduled-subtle)))
    `(org-journal-highlight ((,c :foreground ,err)))
;;;;; org-noter
    `(org-noter-no-notes-exist-face ((,c :foreground ,err)))
    `(org-noter-notes-exist-face ((,c :foreground ,info)))
;;;;; org-pomodoro
    `(org-pomodoro-mode-line ((,c :foreground ,err)))
    `(org-pomodoro-mode-line-break ((,c :foreground ,info)))
    `(org-pomodoro-mode-line-overtime ((,c :foreground ,err)))
;;;;; org-recur
    `(org-recur ((,c :foreground ,fg-alt)))
;;;;; org-roam
    `(org-roam-dim ((,c :foreground "gray50")))
    `(org-roam-olp ((,c :foreground ,fg-dim)))
    `(org-roam-preview-heading ((,c :background ,bg-inactive)))
    `(org-roam-preview-heading-highlight ((,c :background ,bg-active :foreground ,fg-main)))
    `(org-roam-preview-region ((,c :inherit bold)))
    `(org-roam-title ((,c :inherit modus-themes-bold)))
;;;;; org-superstar
    `(org-superstar-item ((,c :foreground ,fg-main)))
;;;;; org-tree-slide
    `(org-tree-slide-header-overlay-face ((,c :inherit modus-themes-heading-0)))
;;;;; origami
    `(origami-fold-header-face ((,c :background ,bg-dim :foreground ,fg-dim :box t)))
    `(origami-fold-replacement-face ((,c :background ,bg-inactive :foreground ,fg-dim)))
;;;;; outline-mode
    `(outline-1 ((,c :inherit modus-themes-heading-1)))
    `(outline-2 ((,c :inherit modus-themes-heading-2)))
    `(outline-3 ((,c :inherit modus-themes-heading-3)))
    `(outline-4 ((,c :inherit modus-themes-heading-4)))
    `(outline-5 ((,c :inherit modus-themes-heading-5)))
    `(outline-6 ((,c :inherit modus-themes-heading-6)))
    `(outline-7 ((,c :inherit modus-themes-heading-7)))
    `(outline-8 ((,c :inherit modus-themes-heading-8)))
;;;;; outline-minor-faces
    `(outline-minor-0 (()))
;;;;; package (M-x list-packages)
    `(package-description ((,c :foreground ,docstring)))
    `(package-help-section-name ((,c :inherit modus-themes-bold)))
    `(package-mark-delete-line ((,c :inherit bold :background ,bg-mark-delete :foreground ,fg-mark-delete)))
    `(package-mark-install-line ((,c :inherit bold :background ,bg-mark-select :foreground ,fg-mark-select)))
    `(package-name ((,c :background ,bg-link :foreground ,fg-link :underline ,underline-link)))
    `(package-status-available ((,c :foreground ,date-common)))
    `(package-status-avail-obso ((,c :foreground ,err)))
    `(package-status-built-in ((,c :foreground ,builtin)))
    `(package-status-dependency ((,c :foreground ,warning)))
    `(package-status-disabled ((,c :foreground ,err :strike-through t)))
    `(package-status-from-source ((,c :foreground ,type)))
    `(package-status-held ((,c :foreground ,warning)))
    `(package-status-incompat ((,c :foreground ,warning)))
    `(package-status-installed ((,c :foreground ,fg-alt)))
    `(package-status-new ((,c :foreground ,info)))
    `(package-status-unsigned ((,c :foreground ,err)))
;;;;; page-break-lines
    `(page-break-lines ((,c :inherit default :foreground "gray50")))
;;;;; pandoc-mode
    `(pandoc-citation-key-face ((,c :inherit modus-themes-bold :foreground ,builtin)))
    `(pandoc-directive-@@-face ((,c :inherit modus-themes-bold :foreground ,keyword)))
    `(pandoc-directive-braces-face ((,c :foreground ,constant)))
    `(pandoc-directive-contents-face ((,c :foreground ,string)))
    `(pandoc-directive-type-face ((,c :inherit modus-themes-bold :foreground ,type)))
;;;;; paren-face
    `(parenthesis ((,c :foreground ,fg-dim)))
;;;;; pass
    `(pass-mode-directory-face ((,c :inherit modus-themes-bold :foreground ,accent-0)))
    `(pass-mode-entry-face ((,c :background ,bg-main :foreground ,fg-main)))
    `(pass-mode-header-face ((,c :foreground ,fg-dim)))
;;;;; pdf-tools
    `(pdf-links-read-link ((,c :inherit modus-themes-bold :background ,fg-main :foreground ,bg-magenta-intense))) ; Foreground is background and vice versa
    `(pdf-occur-document-face ((,c :foreground ,fg-dim)))
    `(pdf-occur-page-face ((,c :foreground ,fg-dim)))
;;;;; persp-mode
    `(persp-face-lighter-buffer-not-in-persp ((,c :foreground ,err)))
    `(persp-face-lighter-default ((,c :inherit modus-themes-bold :foreground ,name)))
    `(persp-face-lighter-nil-persp ((,c :inherit modus-themes-bold)))
;;;;; perspective
    `(persp-selected-face ((,c :inherit modus-themes-bold :foreground ,name)))
;;;;; proced
    `(proced-cpu ((,c :foreground ,keyword)))
    `(proced-emacs-pid ((,c :foreground ,identifier :underline t)))
    `(proced-executable ((,c :foreground ,name)))
    `(proced-interruptible-sleep-status-code ((,c :foreground ,fg-dim)))
    `(proced-mem ((,c :foreground ,type)))
    `(proced-memory-high-usage ((,c :foreground ,err)))
    `(proced-memory-low-usage ((,c :foreground ,info)))
    `(proced-memory-medium-usage ((,c :foreground ,warning)))
    `(proced-pgrp ((,c :foreground ,identifier)))
    `(proced-pid ((,c :foreground ,identifier)))
    `(proced-ppid ((,c :foreground ,identifier)))
    `(proced-run-status-code ((,c :foreground ,info)))
    `(proced-sess ((,c :foreground ,identifier)))
    `(proced-session-leader-pid ((,c :inherit modus-themes-bold :foreground ,identifier)))
    `(proced-time-colon (( )))
    `(proced-uninterruptible-sleep-status-code ((,c :foreground ,err)))
    `(proced-user (( )))
;;;;; popup
    `(popup-face ((,c :background ,bg-inactive :foreground ,fg-main)))
    `(popup-isearch-match ((,c :background ,bg-search-current :foreground ,fg-search-current)))
    `(popup-menu-mouse-face ((,c :background ,bg-hover :foreground ,fg-main)))
    `(popup-menu-selection-face ((,c :inherit modus-themes-completion-selected)))
    `(popup-scroll-bar-background-face ((,c :background ,bg-active)))
    `(popup-scroll-bar-foreground-face (( )))
    `(popup-summary-face ((,c :background ,bg-active :foreground ,fg-dim)))
    `(popup-tip-face ((,c :background ,bg-yellow-intense :foreground ,fg-main)))
;;;;; powerline
    `(powerline-active0 ((,c :background ,fg-dim :foreground ,bg-main)))
    `(powerline-active1
      ((default :inherit modus-themes-ui-variable-pitch
                :background ,bg-mode-line-active
                :foreground ,fg-mode-line-active)
       (((supports :box t))
        :box ,border-mode-line-active)
       (t :underline ,border-mode-line-active)))
    `(powerline-active2
      ((default :inherit modus-themes-ui-variable-pitch
                :background ,bg-mode-line-inactive
                :foreground ,fg-mode-line-inactive)
       (((supports :box t))
        :box ,border-mode-line-inactive)
       (t :underline ,border-mode-line-inactive)))
    `(powerline-inactive0 ((,c :background ,bg-active :foreground ,fg-dim)))
    `(powerline-inactive1 ((,c :background ,bg-main :foreground ,fg-dim)))
    `(powerline-inactive2
      ((default :inherit modus-themes-ui-variable-pitch
                :background ,bg-mode-line-inactive
                :foreground ,fg-mode-line-inactive)
       (((supports :box t))
        :box ,border-mode-line-inactive)
       (t :underline ,border-mode-line-inactive)))
;;;;; powerline-evil
    `(powerline-evil-base-face ((,c :background ,fg-main :foreground ,bg-main)))
    `(powerline-evil-emacs-face ((,c :inherit modus-themes-bold :background ,bg-main)))
    `(powerline-evil-insert-face ((,c :background ,bg-main :foreground ,info)))
    `(powerline-evil-motion-face ((,c :inherit modus-themes-slant :background ,bg-main)))
    `(powerline-evil-normal-face ((,c :background ,bg-main :foreground ,fg-alt)))
    `(powerline-evil-operator-face ((,c :background ,bg-main :foreground ,warning)))
    `(powerline-evil-replace-face ((,c :background ,bg-main :foreground ,err)))
    `(powerline-evil-visual-face ((,c :inherit modus-themes-bold :background ,bg-main)))
;;;;; prescient
    `(prescient-primary-highlight ((,c :inherit modus-themes-completion-match-0)))
    `(prescient-secondary-highlight ((,c :inherit modus-themes-completion-match-1)))
;;;;; proced
    `(proced-mark ((,c :inherit modus-themes-bold)))
    `(proced-marked ((,c :inherit bold :background ,bg-mark-other :foreground ,fg-mark-other)))
    `(proced-sort-header ((,c :inherit modus-themes-bold :underline t)))
;;;;; prodigy
    `(prodigy-green-face ((,c :foreground ,info)))
    `(prodigy-red-face ((,c :foreground ,err)))
    `(prodigy-yellow-face ((,c :foreground ,warning)))
;;;;; pulse
    `(pulse-highlight-start-face ((,c :background ,bg-blue-intense :extend t)))
;;;;; pyim
    `(pyim-page ((,c :background ,bg-active)))
    `(pyim-page-selection ((,c :inherit modus-themes-bold :background ,bg-active :foreground ,info)))
    `(pyim-page-subword ((,c :background ,bg-inactive)))
;;;;; quick-peek
    `(quick-peek-background-face ((,c :background ,bg-inactive)))
    `(quick-peek-border-face ((,c :background ,border :height 1)))
    `(quick-peek-padding-face ((,c :background ,bg-inactive :height 0.15)))
;;;;; rainbow-delimiters
    `(rainbow-delimiters-base-error-face ((,c :background ,bg-prominent-err :foreground ,fg-prominent-err)))
    `(rainbow-delimiters-base-face ((,c :foreground ,fg-main)))
    `(rainbow-delimiters-depth-1-face ((,c :foreground ,rainbow-0)))
    `(rainbow-delimiters-depth-2-face ((,c :foreground ,rainbow-1)))
    `(rainbow-delimiters-depth-3-face ((,c :foreground ,rainbow-2)))
    `(rainbow-delimiters-depth-4-face ((,c :foreground ,rainbow-3)))
    `(rainbow-delimiters-depth-5-face ((,c :foreground ,rainbow-4)))
    `(rainbow-delimiters-depth-6-face ((,c :foreground ,rainbow-5)))
    `(rainbow-delimiters-depth-7-face ((,c :foreground ,rainbow-6)))
    `(rainbow-delimiters-depth-8-face ((,c :foreground ,rainbow-7)))
    `(rainbow-delimiters-depth-9-face ((,c :foreground ,rainbow-8)))
    `(rainbow-delimiters-mismatched-face ((,c :background ,bg-prominent-warning :foreground ,fg-prominent-warning)))
    `(rainbow-delimiters-unmatched-face ((,c :background ,bg-prominent-err :foreground ,fg-prominent-err)))
;;;;; rcirc
    `(rcirc-bright-nick ((,c :inherit modus-themes-bold :foreground ,accent-2)))
    `(rcirc-dim-nick ((,c :foreground ,fg-dim)))
    `(rcirc-monospace-text ((,c :inherit fixed-pitch)))
    `(rcirc-my-nick ((,c :inherit modus-themes-bold :foreground ,accent-1)))
    `(rcirc-nick-in-message ((,c :inherit modus-themes-bold :foreground ,accent-1)))
    `(rcirc-nick-in-message-full-line ((,c :inherit modus-themes-bold :foreground ,accent-1)))
    `(rcirc-other-nick ((,c :inherit modus-themes-bold :foreground ,accent-0)))
    `(rcirc-prompt ((,c :inherit modus-themes-prompt)))
    `(rcirc-server ((,c :inherit modus-themes-slant :foreground ,comment)))
    `(rcirc-timestamp ((,c :foreground ,date-common)))
    `(rcirc-track-keyword ((,c :inherit modus-themes-bold :foreground ,modeline-warning)))
    `(rcirc-track-nick ((,c :inherit modus-themes-bold :foreground ,accent-1)))
    `(rcirc-url ((,c :background ,bg-link :foreground ,fg-link :underline ,underline-link)))
;;;;; recursion-indicator
    `(recursion-indicator-general ((,c :foreground ,modeline-err)))
    `(recursion-indicator-minibuffer ((,c :foreground ,modeline-info)))
;;;;; regexp-builder (re-builder)
    `(reb-match-0 ((,c :background ,bg-search-rx-group-0 :foreground ,fg-search-rx-group-0)))
    `(reb-match-1 ((,c :background ,bg-search-rx-group-1 :foreground ,fg-search-rx-group-1)))
    `(reb-match-2 ((,c :background ,bg-search-rx-group-2 :foreground ,fg-search-rx-group-2)))
    `(reb-match-3 ((,c :background ,bg-search-rx-group-3 :foreground ,fg-search-rx-group-3)))
    `(reb-regexp-grouping-backslash ((,c :inherit modus-themes-bold :foreground ,rx-backslash)))
    `(reb-regexp-grouping-construct ((,c :inherit modus-themes-bold :foreground ,rx-construct)))
;;;;; rg (rg.el)
    `(rg-column-number-face ((,c :foreground ,fg-dim)))
    `(rg-context-face ((,c :foreground ,fg-dim)))
    `(rg-error-face ((,c :foreground ,err)))
    `(rg-file-tag-face ((,c :inherit modus-themes-bold :foreground ,builtin)))
    `(rg-filename-face ((,c :inherit modus-themes-bold :foreground ,name)))
    `(rg-line-number-face ((,c :foreground ,fg-dim)))
    `(rg-literal-face ((,c :foreground ,constant)))
    `(rg-match-face ((,c :background ,bg-search-static :foreground ,fg-search-static)))
    `(rg-regexp-face ((,c :foreground ,name)))
    `(rg-toggle-off-face ((,c :inherit modus-themes-bold :foreground ,fg-dim)))
    `(rg-toggle-on-face ((,c :foreground ,info)))
    `(rg-warning-face ((,c :foreground ,warning)))
;;;;; ripgrep
    `(ripgrep-context-face ((,c :foreground ,fg-dim)))
    `(ripgrep-error-face ((,c :foreground ,err)))
    `(ripgrep-hit-face ((,c :foreground ,info)))
    `(ripgrep-match-face ((,c :background ,bg-search-static :foreground ,fg-search-static)))
;;;;; rmail
    `(rmail-header-name ((,c :inherit bold)))
    `(rmail-highlight ((,c :inherit bold :foreground ,mail-other)))
;;;;; rst-mode
    `(rst-level-1 ((,c :inherit modus-themes-heading-1)))
    `(rst-level-2 ((,c :inherit modus-themes-heading-2)))
    `(rst-level-3 ((,c :inherit modus-themes-heading-3)))
    `(rst-level-4 ((,c :inherit modus-themes-heading-4)))
    `(rst-level-5 ((,c :inherit modus-themes-heading-5)))
    `(rst-level-6 ((,c :inherit modus-themes-heading-6)))
;;;;; ruler-mode
    ;; NOTE 2025-10-24: All the faces of `ruler-mode' need to inherit
    ;; from `default' to yield the expected results.  Otherwise the
    ;; ruler is shorter.  I am not sure what is happening, but it
    ;; seems important.  Its default face definitions also inherit
    ;; from `default' and then from `ruler-mode-default'.
    `(ruler-mode-column-number ((,c :inherit default :background ,bg-dim :foreground ,fg-dim)))
    `(ruler-mode-comment-column ((,c :inherit default :foreground ,info)))
    `(ruler-mode-current-column ((,c :inherit default :background ,bg-active :foreground ,fg-main)))
    `(ruler-mode-default ((,c :inherit default :background ,bg-dim :foreground ,fg-dim)))
    `(ruler-mode-fill-column ((,c :inherit default :foreground ,info)))
    `(ruler-mode-fringes ((,c :inherit default :foreground ,fg-dim)))
    `(ruler-mode-goal-column ((,c :inherit default :foreground ,info)))
    `(ruler-mode-margins ((,c :inherit default :foreground ,bg-main)))
    `(ruler-mode-pad ((,c :inherit default :background ,bg-inactive :foreground ,fg-dim)))
    `(ruler-mode-tab-stop ((,c :inherit default :foreground ,warning)))
;;;;; sesman
    `(sesman-browser-button-face ((,c :background ,bg-link :foreground ,fg-link :underline ,underline-link)))
    `(sesman-browser-highligh-face ((,c :background ,bg-hover :foreground ,fg-main)))
    `(sesman-buffer-face ((,c :foreground ,accent-1)))
    `(sesman-directory-face ((,c :inherit modus-themes-bold :foreground ,accent-0)))
    `(sesman-project-face ((,c :inherit modus-themes-bold :foreground ,accent-2)))
;;;;; shell-script-mode
    `(sh-heredoc ((,c :foreground ,string)))
    `(sh-quoted-exec ((,c :inherit modus-themes-bold :foreground ,builtin)))
;;;;; shortdoc
    `(shortdoc-heading ((,c :inherit modus-themes-heading-1)))
    `(shortdoc-section (())) ; remove the default's variable-pitch style
;;;;; show-paren-mode
    `(show-paren-match ((,c :background ,bg-paren-match :foreground ,fg-paren-match :underline ,underline-paren-match)))
    `(show-paren-match-expression ((,c :background ,bg-paren-expression)))
    `(show-paren-mismatch ((,c :background ,bg-prominent-err :foreground ,fg-prominent-err)))
;;;;; shr
    `(shr-abbreviation ((,c :underline (:style wave :color ,underline-note))))
    `(shr-code ((,c :inherit modus-themes-fixed-pitch :background ,bg-prose-verbatim :foreground ,fg-prose-verbatim)))
    `(shr-h1 ((,c :inherit modus-themes-heading-1)))
    `(shr-h2 ((,c :inherit modus-themes-heading-2)))
    `(shr-h3 ((,c :inherit modus-themes-heading-3)))
    `(shr-h4 ((,c :inherit modus-themes-heading-4)))
    `(shr-h5 ((,c :inherit modus-themes-heading-5)))
    `(shr-h6 ((,c :inherit modus-themes-heading-6)))
    `(shr-mark ((,c :background ,bg-search-static :foreground ,fg-search-static)))
    `(shr-selected-link ((,c :inherit bold :background ,bg-mark-select :foreground ,fg-mark-select)))
;;;;; side-notes
    `(side-notes ((,c :background ,bg-dim :foreground ,fg-dim)))
;;;;; sieve-mode
    `(sieve-action-commands ((,c :inherit modus-themes-bold :foreground ,builtin)))
    `(sieve-control-commands ((,c :inherit modus-themes-bold :foreground ,keyword)))
    `(sieve-tagged-arguments ((,c :inherit modus-themes-bold :foreground ,type)))
    `(sieve-test-commands ((,c :foreground ,fnname)))
;;;;; skewer-mode
    `(skewer-error-face ((,c :underline (:style wave :color ,underline-err))))
;;;;; slime (sldb)
    `(sldb-condition-face ((,c :foreground ,preprocessor)))
    `(sldb-restart-number-face ((,c :inherit modus-themes-bold)))
    `(sldb-restart-type-face ((,c :inherit modus-themes-bold :foreground ,type)))
    `(sldb-restartable-frame-line-face ((,c :foreground ,info)))
    `(sldb-section-face ((,c :inherit modus-themes-bold)))
    `(slime-error-face ((,c :underline (:style wave :color ,underline-err))))
    `(slime-note-face ((,c :inherit underline)))
    `(slime-repl-input-face ((,c :inherit modus-themes-bold)))
    `(slime-repl-inputed-output-face ((,c :foreground ,string)))
    `(slime-repl-output-mouseover-face ((,c :background ,bg-hover :foreground ,fg-main)))
    `(slime-repl-prompt-face ((,c :inherit modus-themes-prompt)))
    `(slime-style-warning-face ((,c :underline (:style wave :color ,underline-note))))
    `(slime-warning-face ((,c :underline (:style wave :color ,underline-warning))))
;;;;; sly
    `(sly-action-face ((,c :inherit modus-themes-bold :foreground ,type)))
    `(sly-db-condition-face ((,c :foreground ,preprocessor)))
    `(sly-db-restartable-frame-line-face ((,c :foreground ,info)))
    `(sly-error-face ((,c :underline (:style wave :color ,underline-err))))
    `(sly-mode-line ((,c :inherit italic :foreground ,modeline-info)))
    `(sly-mrepl-output-face ((,c :foreground ,string)))
    `(sly-mrepl-prompt-face ((,c :inherit modus-themes-prompt)))
    `(sly-note-face ((,c :underline (:style wave :color ,underline-note))))
    `(sly-stickers-placed-face ((,c :background ,bg-inactive)))
    `(sly-style-warning-face ((,c :underline (:style wave :color ,underline-note))))
    `(sly-warning-face ((,c :underline (:style wave :color ,underline-warning))))
;;;;; smart-mode-line
    `(sml/charging ((,c :foreground ,info)))
    `(sml/discharging ((,c :foreground ,err)))
    `(sml/filename ((,c :inherit modus-themes-bold :foreground ,name)))
    `(sml/folder (( )))
    `(sml/git ((,c :foreground ,info)))
    `(sml/global (( )))
    `(sml/line-number (( )))
    `(sml/minor-modes (( )))
    `(sml/modes ((,c :inherit modus-themes-bold)))
    `(sml/modified ((,c :inherit modus-themes-slant)))
    `(sml/mule-info (( )))
    `(sml/name-filling ((,c :foreground ,warning)))
    `(sml/not-modified (( )))
    `(sml/numbers-separator (( )))
    `(sml/outside-modified ((,c :background ,bg-prominent-err :foreground ,fg-prominent-err)))
    `(sml/position-percentage (( )))
    `(sml/prefix ((,c :foreground ,fg-alt)))
    `(sml/process ((,c :foreground ,fg-alt)))
    `(sml/projectile ((,c :foreground ,info)))
    `(sml/read-only (( )))
    `(sml/remote (( )))
    `(sml/sudo ((,c :foreground ,warning)))
    `(sml/time (( )))
    `(sml/vc ((,c :foreground ,info)))
    `(sml/vc-edited ((,c :inherit modus-themes-slant)))
;;;;; smerge
    `(smerge-base ((,c :background ,bg-changed :foreground ,fg-changed :extend t)))
    `(smerge-lower ((,c :background ,bg-added :foreground ,fg-added :extend t)))
    `(smerge-markers (( )))
    `(smerge-refined-added ((,c :background ,bg-added-refine :foreground ,fg-added)))
    `(smerge-refined-changed ((,c :background ,bg-changed-refine :foreground ,fg-changed)))
    `(smerge-refined-removed ((,c :background ,bg-removed-refine :foreground ,fg-removed)))
    `(smerge-upper ((,c :background ,bg-removed :foreground ,fg-removed :extend t)))
;;;;; spacious-padding
    `(spacious-padding-line-active ((,c :foreground ,accent-0)))
    `(spacious-padding-line-inactive ((,c :foreground ,border)))
    `(spacious-padding-subtle-mode-line-active ((,c :foreground ,accent-0)))
    `(spacious-padding-subtle-mode-line-inactive ((,c :foreground ,border)))
;;;;; speedbar
    `(speedbar-button-face ((,c :background ,bg-link :foreground ,fg-link :underline ,underline-link)))
    `(speedbar-directory-face ((,c :inherit modus-themes-bold :foreground ,accent-0)))
    `(speedbar-file-face ((,c :foreground ,fg-main)))
    `(speedbar-highlight-face ((,c :background ,bg-hover :foreground ,fg-main)))
    `(speedbar-selected-face ((,c :inherit bold :background ,bg-mark-select :foreground ,fg-mark-select)))
    `(speedbar-separator-face ((,c :background ,bg-active :foreground ,fg-main)))
    `(speedbar-tag-face ((,c :foreground ,accent-1)))
;;;;; spell-fu
    `(spell-fu-incorrect-face ((,c :underline (:style wave :color ,underline-err))))
;;;;; stripes
    `(stripes ((,c :background ,bg-inactive)))
;;;;; suggest
    `(suggest-heading ((,c :foreground ,warning)))
;;;;; switch-window
    `(switch-window-background ((,c :background ,bg-inactive)))
    `(switch-window-label ((,c :inherit (bold modus-themes-reset-soft) :height 1.5 :foreground ,err))) ; same as `aw-leading-char-face'
;;;;; swiper
    `(swiper-background-match-face-1 (( )))
    `(swiper-background-match-face-2 ((,c :inherit modus-themes-completion-match-0)))
    `(swiper-background-match-face-3 ((,c :inherit modus-themes-completion-match-1)))
    `(swiper-background-match-face-4 ((,c :inherit modus-themes-completion-match-2)))
    `(swiper-line-face ((,c :background ,bg-hl-line :extend t)))
    `(swiper-match-face-1 (( )))
    `(swiper-match-face-2 ((,c :inherit modus-themes-completion-match-0)))
    `(swiper-match-face-3 ((,c :inherit modus-themes-completion-match-1)))
    `(swiper-match-face-4 ((,c :inherit modus-themes-completion-match-2)))
;;;;; symbol-overlay
    `(symbol-overlay-default-face ((,c :background ,bg-inactive)))
    `(symbol-overlay-face-1 ((,c :background ,bg-blue-intense :foreground ,fg-main)))
    `(symbol-overlay-face-2 ((,c :background ,bg-magenta-intense :foreground ,fg-main)))
    `(symbol-overlay-face-3 ((,c :background ,bg-yellow-intense :foreground ,fg-main)))
    `(symbol-overlay-face-4 ((,c :background ,bg-cyan-intense :foreground ,fg-main)))
    `(symbol-overlay-face-5 ((,c :background ,bg-red-intense :foreground ,fg-main)))
    `(symbol-overlay-face-6 ((,c :background ,bg-green-intense :foreground ,fg-main)))
    `(symbol-overlay-face-7 ((,c :background ,bg-red-subtle :foreground ,fg-main)))
    `(symbol-overlay-face-8 ((,c :background ,bg-cyan-subtle :foreground ,fg-main)))
;;;;; syslog-mode
    `(syslog-debug ((,c :inherit modus-themes-slant)))
    `(syslog-error ((,c :foreground ,err)))
    `(syslog-file ((,c :inherit modus-themes-bold :foreground ,name)))
    `(syslog-hide ((,c :background ,bg-main :foreground ,fg-main)))
    `(syslog-hour ((,c :inherit modus-themes-bold :foreground ,date-common)))
    `(syslog-info ((,c :foreground ,info)))
    `(syslog-ip ((,c :inherit modus-themes-bold :foreground ,name :underline t)))
    `(syslog-su ((,c :foreground ,err :underline t)))
    `(syslog-warn ((,c :foreground ,warning)))
;;;;; tab-bar-mode
    `(tab-bar ((,c :inherit modus-themes-ui-variable-pitch :background ,bg-tab-bar)))
    `(tab-bar-tab-group-current ((,c :inherit modus-themes-bold :background ,bg-tab-current :foreground ,fg-alt ,@(modus-themes--box bg-tab-current -2 nil))))
    `(tab-bar-tab-group-inactive ((,c :background ,bg-tab-bar :foreground ,fg-alt ,@(modus-themes--box bg-tab-bar -2 nil))))
    `(tab-bar-tab ((,c :inherit modus-themes-bold :background ,bg-tab-current ,@(modus-themes--box bg-tab-current -2 nil))))
    `(tab-bar-tab-highlight ((,c :background ,bg-hover :foreground ,fg-main :box t)))
    `(tab-bar-tab-inactive ((,c :background ,bg-tab-other ,@(modus-themes--box bg-tab-other -2 nil))))
    `(tab-bar-tab-ungrouped ((,c :background ,bg-tab-other ,@(modus-themes--box bg-tab-other -2 nil))))
;;;;; tab-line-mode
    `(tab-line ((,c :inherit modus-themes-ui-variable-pitch :background ,bg-tab-bar :height 0.95)))
    `(tab-line-close-highlight ((,c :foreground ,err)))
    `(tab-line-highlight ((,c :background ,bg-hover :foreground ,fg-main)))
    `(tab-line-tab (( )))
    `(tab-line-tab-current ((,c :inherit modus-themes-bold :background ,bg-tab-current ,@(modus-themes--box bg-tab-current -2 nil))))
    `(tab-line-tab-inactive ((,c :background ,bg-tab-other ,@(modus-themes--box bg-tab-other -2 nil))))
    `(tab-line-tab-inactive-alternate ((,c :background ,bg-tab-other :foreground ,fg-alt ,@(modus-themes--box bg-tab-other -2 nil))))
    `(tab-line-tab-modified ((,c :foreground ,warning)))
;;;;; table (built-in table.el)
    `(table-cell ((,c :background ,bg-dim)))
;;;;; telega
    `(telega-button ((,c :box t :foreground ,fg-link)))
    `(telega-button-active ((,c :box ,fg-link :background ,fg-link :foreground ,bg-main)))
    `(telega-button-highlight ((,c :background ,bg-hover :foreground ,fg-main)))
    `(telega-chat-prompt ((,c :inherit modus-themes-prompt)))
    `(telega-entity-type-code ((,c :inherit modus-themes-fixed-pitch :background ,bg-prose-verbatim :foreground ,fg-prose-verbatim)))
    `(telega-entity-type-mention ((,c :foreground ,type)))
    `(telega-entity-type-pre ((,c :inherit modus-themes-fixed-pitch :background ,bg-prose-code :foreground ,fg-prose-code)))
    `(telega-entity-type-spoiler ((,c :background ,fg-main :foreground ,fg-main)))
    `(telega-msg-heading ((,c :background ,bg-inactive)))
    `(telega-msg-self-title ((,c :inherit modus-themes-bold)))
    `(telega-root-heading ((,c :background ,bg-inactive)))
    `(telega-secret-title ((,c :foreground ,warning)))
    `(telega-unmuted-count ((,c :foreground ,number)))
    `(telega-user-online-status ((,c :foreground ,info)))
    `(telega-username ((,c :foreground ,name)))
    `(telega-webpage-chat-link ((,c :background ,bg-inactive)))
    `(telega-webpage-fixed ((,c :inherit modus-themes-fixed-pitch :height 0.85)))
    `(telega-webpage-header ((,c :height 1.3)))
    `(telega-webpage-preformatted ((,c :inherit modus-themes-fixed-pitch :background ,bg-inactive)))
    `(telega-webpage-subheader ((,c :height 1.15)))
;;;;; terraform-mode
    `(terraform--resource-name-face ((,c :foreground ,keyword)))
    `(terraform--resource-type-face ((,c :foreground ,type)))
;;;;; term
    ;; NOTE 2023-08-10: `term-color-black' and `term-color-white' use
    ;; the "bright" semantic color mappings to make sure they are
    ;; distinct from `term'.
    `(term ((,c :background ,bg-main :foreground ,fg-main)))
    `(term-bold ((,c :inherit bold)))
    `(term-color-black ((,c :background ,bg-term-black-bright :foreground ,fg-term-black-bright)))
    `(term-color-blue ((,c :background ,bg-term-blue :foreground ,fg-term-blue)))
    `(term-color-cyan ((,c :background ,bg-term-cyan :foreground ,fg-term-cyan)))
    `(term-color-green ((,c :background ,bg-term-green :foreground ,fg-term-green)))
    `(term-color-magenta ((,c :background ,bg-term-magenta :foreground ,fg-term-magenta)))
    `(term-color-red ((,c :background ,bg-term-red :foreground ,fg-term-red)))
    `(term-color-white ((,c :background ,bg-term-white-bright :foreground ,fg-term-white-bright)))
    `(term-color-yellow ((,c :background ,bg-term-yellow :foreground ,fg-term-yellow)))
    `(term-underline ((,c :inherit underline)))
;;;;; textsec
    `(textsec-suspicious (( )))
;;;;; tldr
    `(tldr-code-block (( )))
    `(tldr-command-argument ((,c :foreground ,string)))
    `(tldr-command-itself ((,c :inherit modus-themes-bold :foreground ,builtin)))
    `(tldr-description ((,c :inherit modus-themes-slant :foreground ,docstring)))
    `(tldr-introduction ((,c :inherit modus-themes-slant :foreground ,comment)))
    `(tldr-title ((,c :inherit modus-themes-heading-1)))
;;;;; tmr
    `(tmr-mode-line-active ((,c :inherit modus-themes-bold :foreground ,modeline-info)))
    `(tmr-mode-line-soon ((,c :inherit modus-themes-bold :foreground ,modeline-warning)))
    `(tmr-mode-line-urgent ((,c :inherit modus-themes-bold :foreground ,modeline-err)))
    `(tmr-tabulated-description ((,c :foreground ,docstring)))
    `(tmr-tabulated-end-time ((,c :foreground ,date-deadline)))
    `(tmr-tabulated-remaining-time ((,c :foreground ,date-scheduled)))
    `(tmr-tabulated-start-time ((,c :foreground ,date-common)))
;;;;; transient
    `(transient-active-infix ((,c :background ,bg-hover :foreground ,fg-main)))
    ;; Placate the compiler for what is a spurious warning.  We also
    ;; have to do this with `eldoc-highlight-function-argument'.
    (list 'transient-argument `((,c :inherit modus-themes-bold :background ,bg-active-argument :foreground ,fg-active-argument)))
    `(transient-disabled-suffix ((,c :inherit bold :background ,bg-mark-delete :foreground ,fg-mark-delete)))
    `(transient-enabled-suffix ((,c :inherit bold :background ,bg-mark-select :foreground ,fg-mark-select)))
    `(transient-heading ((,c :inherit bold :foreground ,fg-main)))
    `(transient-inactive-argument ((,c :foreground ,fg-dim)))
    `(transient-inactive-value ((,c :foreground ,fg-dim)))
    `(transient-inapt-argument ((,c :inherit bold :foreground ,fg-dim)))
    `(transient-inapt-suffix ((,c :inherit italic :foreground ,fg-dim)))
    `(transient-key ((,c :inherit (bold modus-themes-fixed-pitch) :foreground ,keybind)))
    ;; NOTE 2025-11-19: With regard to `transient-semantic-coloring',
    ;; also see `modus-themes-faces-deuteranopia' and `modus-themes-faces-tritanopia'.
    `(transient-key-exit ((,c :inherit (bold modus-themes-fixed-pitch) :foreground ,red-warmer)))
    `(transient-key-noop ((,c :inherit modus-themes-fixed-pitch :foreground ,fg-dim)))
    `(transient-key-recurse ((,c :inherit (bold modus-themes-fixed-pitch) :foreground ,fg-main)))
    `(transient-key-return ((,c :inherit (bold modus-themes-fixed-pitch) :foreground ,yellow)))
    `(transient-key-stack ((,c :inherit (bold modus-themes-fixed-pitch) :foreground ,blue-cooler)))
    `(transient-key-stay ((,c :inherit (bold modus-themes-fixed-pitch) :foreground ,green-cooler)))
    `(transient-mismatched-key ((,c :inherit underline)))
    `(transient-nonstandard-key ((,c :inherit underline)))
    `(transient-unreachable ((,c :foreground ,fg-dim)))
    `(transient-unreachable-key ((,c :inherit modus-themes-fixed-pitch :foreground ,fg-dim)))
    `(transient-value ((,c :inherit modus-themes-bold :background ,bg-active-value :foreground ,fg-active-value)))
;;;;; trashed
    `(trashed-deleted ((,c :inherit bold :background ,bg-mark-delete :foreground ,fg-mark-delete)))
    `(trashed-directory ((,c :foreground ,accent-0)))
    `(trashed-mark ((,c :inherit modus-themes-bold)))
    `(trashed-marked ((,c :inherit bold :background ,bg-mark-other :foreground ,fg-mark-other)))
    `(trashed-restored ((,c :inherit bold :background ,bg-mark-select :foreground ,fg-mark-select)))
;;;;; treemacs
    `(treemacs-async-loading-face ((,c :foreground ,fg-main)))
    `(treemacs-directory-face ((,c :foreground ,accent-0)))
    `(treemacs-directory-collapsed-face ((,c :foreground ,accent-0)))
    `(treemacs-file-face ((,c :foreground ,fg-main)))
    `(treemacs-fringe-indicator-face ((,c :foreground ,fg-main)))
    `(treemacs-git-added-face ((,c :foreground ,info)))
    `(treemacs-git-commit-diff-face ((,c :foreground ,err)))
    `(treemacs-git-conflict-face ((,c :foreground ,err)))
    `(treemacs-git-ignored-face ((,c :foreground ,fg-dim)))
    `(treemacs-git-modified-face ((,c :foreground ,warning)))
    `(treemacs-git-renamed-face ((,c :inherit modus-themes-slant)))
    `(treemacs-git-unmodified-face ((,c :foreground ,fg-main)))
    `(treemacs-git-untracked-face ((,c :foreground ,info)))
    `(treemacs-header-button-face ((,c :foreground ,fg-main)))
    `(treemacs-help-column-face ((,c :inherit modus-themes-bold :foreground ,keyword)))
    `(treemacs-help-title-face ((,c :foreground ,fg-main)))
    `(treemacs-hl-line-face ((,c :background ,bg-hl-line :extend t)))
    `(treemacs-marked-file-face ((,c :inherit bold :background ,bg-mark-other :foreground ,fg-mark-other)))
    `(treemacs-nerd-icons-face ((,c :foreground ,accent-0)))
    `(treemacs-on-failure-pulse-face ((,c :foreground ,fg-main)))
    `(treemacs-on-success-pulse-face ((,c :foreground ,fg-main)))
    `(treemacs-peek-mode-indicator-face ((,c :foreground ,fg-main)))
    `(treemacs-remote-face ((,c :foreground ,fg-main)))
    `(treemacs-root-face ((,c :foreground ,accent-0)))
    `(treemacs-root-remote-disconnected-face ((,c :foreground ,warning)))
    `(treemacs-root-remote-unreadable-face ((,c :foreground ,warning)))
    `(treemacs-root-unreadable-face ((,c :foreground ,err)))
    `(treemacs-tags-face ((,c :foreground ,fg-main)))
    `(treemacs-term-node-face ((,c :inherit modus-themes-bold :foreground ,keyword)))
    `(treemacs-window-background-face ((,c :background ,bg-main)))
    `(treemacs-nerd-icons-root-face ((,c :foreground ,accent-0)))
    `(treemacs-nerd-icons-file-face ((,c :foreground ,accent-0)))
;;;;; tty-menu
    `(tty-menu-disabled-face ((,c :background ,bg-inactive :foreground ,fg-dim)))
    `(tty-menu-enabled-face ((,c :inherit bold :background ,bg-inactive :foreground ,fg-main)))
    `(tty-menu-selected-face ((,c :background ,bg-blue-intense :foreground ,fg-main)))
;;;;; tuareg
    `(caml-types-def-face ((,c :background ,bg-red-subtle :foreground ,fg-main)))
    `(caml-types-expr-face ((,c :background ,bg-green-subtle :foreground ,fg-main)))
    `(caml-types-occ-face ((,c :background ,bg-green-subtle :foreground ,fg-main)))
    `(caml-types-scope-face ((,c :background ,bg-blue-subtle :foreground ,fg-main)))
    `(caml-types-typed-face ((,c :background ,bg-magenta-subtle :foreground ,fg-main)))
    `(tuareg-font-double-semicolon-face ((,c :foreground ,preprocessor)))
    `(tuareg-font-lock-attribute-face ((,c :foreground ,fnname)))
    `(tuareg-font-lock-constructor-face ((,c :foreground ,fg-main)))
    `(tuareg-font-lock-error-face ((,c :background ,bg-prominent-err :foreground ,fg-prominent-err)))
    ;; `(tuareg-font-lock-extension-node-face ((,c :background ,bg-inactive :foreground ,magenta)))
    `(tuareg-font-lock-governing-face ((,c :inherit modus-themes-bold :foreground ,fg-main)))
    `(tuareg-font-lock-infix-extension-node-face ((,c :foreground ,fnname)))
    `(tuareg-font-lock-interactive-directive-face ((,c :foreground ,preprocessor)))
    `(tuareg-font-lock-interactive-error-face ((,c :foreground ,err)))
    `(tuareg-font-lock-interactive-output-face ((,c :foreground ,constant)))
    `(tuareg-font-lock-label-face ((,c :inherit modus-themes-bold :foreground ,type)))
    `(tuareg-font-lock-line-number-face ((,c :foreground ,fg-dim)))
    `(tuareg-font-lock-module-face ((,c :inherit modus-themes-bold :foreground ,builtin)))
    ;; `(tuareg-font-lock-multistage-face ((,c :inherit bold :background ,bg-inactive :foreground ,blue)))
    `(tuareg-font-lock-operator-face ((,c :foreground ,preprocessor)))
    `(tuareg-opam-error-face ((,c :foreground ,err)))
    `(tuareg-opam-pkg-variable-name-face ((,c :foreground ,variable)))
;;;;; typescript
    `(typescript-jsdoc-tag ((,c :inherit modus-themes-slant :foreground ,builtin)))
    `(typescript-jsdoc-type ((,c :inherit modus-themes-slant :foreground ,type)))
    `(typescript-jsdoc-value ((,c :inherit modus-themes-slant :foreground ,string)))
;;;;; undo-tree
    `(undo-tree-visualizer-active-branch-face ((,c :inherit modus-themes-bold :foreground ,fg-main)))
    `(undo-tree-visualizer-current-face ((,c :foreground ,err)))
    `(undo-tree-visualizer-default-face ((,c :foreground ,fg-dim)))
    `(undo-tree-visualizer-register-face ((,c :foreground ,info)))
    `(undo-tree-visualizer-unmodified-face ((,c :foreground ,fg-dim)))
;;;;; vc (vc-dir.el, vc-hooks.el)
    `(vc-dir-directory (( )))
    `(vc-dir-file ((,c :foreground ,name)))
    `(vc-dir-header ((,c :inherit modus-themes-bold)))
    `(vc-dir-header-value ((,c :foreground ,string)))
    `(vc-dir-mark-indicator (( )))
    `(vc-dir-status-edited ((,c :inherit modus-themes-slant)))
    `(vc-dir-status-ignored ((,c :foreground ,fg-dim)))
    `(vc-dir-status-up-to-date ((,c :foreground ,info)))
    `(vc-dir-status-warning ((,c :foreground ,warning)))
    `(vc-conflict-state ((,c :foreground ,err)))
    `(vc-edited-state ((,c :inherit modus-themes-slant)))
    `(vc-git-log-edit-summary-max-warning ((,c :foreground ,err)))
    `(vc-git-log-edit-summary-target-warning ((,c :foreground ,warning)))
    `(vc-locally-added-state ((,c :inherit modus-themes-slant)))
    `(vc-locked-state ((,c :foreground ,info)))
    `(vc-missing-state ((,c :underline (:style wave :color ,underline-warning) :foreground ,warning)))
    `(vc-needs-update-state ((,c :foreground ,err)))
    `(vc-removed-state ((,c :underline (:style wave :color ,underline-err) :foreground ,err)))
    `(vc-state-base (( )))
    `(vc-up-to-date-state (( )))
;;;;; vertico
    `(vertico-current ((,c :inherit modus-themes-completion-selected)))
;;;;; vertico-quick
    `(vertico-quick1 ((,c :inherit bold :background ,bg-search-current :foreground ,fg-search-current)))
    `(vertico-quick2 ((,c :inherit bold :background ,bg-search-current :foreground ,fg-search-current)))
;;;;; vimish-fold
    `(vimish-fold-fringe ((,c :foreground ,info)))
    `(vimish-fold-mouse-face ((,c :background ,bg-hover :foreground ,fg-main)))
    `(vimish-fold-overlay ((,c :background ,bg-inactive)))
;;;;; viper
    `(viper-search ((,c :background ,bg-search-current :foreground ,fg-search-current)))
    `(viper-replace-overlay ((,c :background ,bg-search-replace :foreground ,fg-search-replace)))
    `(viper-minibuffer-emacs (( )))
    `(viper-minibuffer-insert (( )))
    `(viper-minibuffer-vi (( )))
;;;;; visible-mark
    `(visible-mark-active ((,c :background ,bg-search-static :foreground ,fg-search-static)))
    `(visible-mark-face1 ((,c :background ,bg-search-rx-group-0 :foreground ,fg-search-rx-group-0)))
    `(visible-mark-face2 ((,c :background ,bg-search-rx-group-1 :foreground ,fg-search-rx-group-1)))
    `(visible-mark-forward-face1 ((,c :background ,bg-search-rx-group-2 :foreground ,fg-search-rx-group-2)))
    `(visible-mark-forward-face2 ((,c :background ,bg-search-rx-group-3 :foreground ,fg-search-rx-group-3)))
;;;;; visual-regexp
    `(vr/group-0 ((,c :background ,bg-search-rx-group-0 :foreground ,fg-search-rx-group-0)))
    `(vr/group-1 ((,c :background ,bg-search-rx-group-1 :foreground ,fg-search-rx-group-1)))
    `(vr/group-2 ((,c :background ,bg-search-rx-group-2 :foreground ,fg-search-rx-group-2)))
    `(vr/match-0 ((,c :background ,bg-search-current :foreground ,fg-search-current)))
    `(vr/match-1 ((,c :background ,bg-search-lazy :foreground ,fg-search-lazy)))
    `(vr/match-separator-face ((,c :inherit bold :background ,bg-active)))
;;;;; vterm
    `(vterm-color-black ((,c :background ,bg-term-black :foreground ,fg-term-black)))
    `(vterm-color-bright-black ((,c :background ,bg-term-black-bright :foreground ,fg-term-black-bright)))
    `(vterm-color-red ((,c :background ,bg-term-red :foreground ,fg-term-red)))
    `(vterm-color-bright-red ((,c :background ,bg-term-red-bright :foreground ,fg-term-red-bright)))
    `(vterm-color-green ((,c :background ,bg-term-green :foreground ,fg-term-green)))
    `(vterm-color-bright-green ((,c :background ,bg-term-green-bright :foreground ,fg-term-green-bright)))
    `(vterm-color-yellow ((,c :background ,bg-term-yellow :foreground ,fg-term-yellow)))
    `(vterm-color-bright-yellow ((,c :background ,bg-term-yellow-bright :foreground ,fg-term-yellow-bright)))
    `(vterm-color-blue ((,c :background ,bg-term-blue :foreground ,fg-term-blue)))
    `(vterm-color-bright-blue ((,c :background ,bg-term-blue-bright :foreground ,fg-term-blue-bright)))
    `(vterm-color-magenta ((,c :background ,bg-term-magenta :foreground ,fg-term-magenta)))
    `(vterm-color-bright-magenta ((,c :background ,bg-term-magenta-bright :foreground ,fg-term-magenta-bright)))
    `(vterm-color-cyan ((,c :background ,bg-term-cyan :foreground ,fg-term-cyan)))
    `(vterm-color-bright-cyan ((,c :background ,bg-term-cyan-bright :foreground ,fg-term-cyan-bright)))
    `(vterm-color-white ((,c :background ,bg-term-white :foreground ,fg-term-white)))
    `(vterm-color-bright-white ((,c :background ,bg-term-white-bright :foreground ,fg-term-white-bright)))
    `(vterm-color-inverse-video ((,c :background ,bg-main :inverse-video t)))
    `(vterm-color-underline ((,c :inherit underline)))
;;;;; vundo
    `(vundo-default ((,c :foreground ,fg-dim)))
    `(vundo-highlight ((,c :inherit modus-themes-bold :foreground ,err)))
    `(vundo-last-saved ((,c :inherit modus-themes-bold :foreground ,info)))
    `(vundo-saved ((,c :foreground ,info)))
;;;;; wcheck-mode
    `(wcheck-default-face ((,c :foreground ,err :underline t)))
;;;;; web-mode
    `(web-mode-annotation-face ((,c :inherit modus-themes-slant :foreground ,comment)))
    `(web-mode-annotation-html-face ((,c :inherit modus-themes-slant :foreground ,comment)))
    `(web-mode-annotation-tag-face ((,c :inherit modus-themes-slant :foreground ,comment :underline t)))
    `(web-mode-block-attr-name-face ((,c :foreground ,constant)))
    `(web-mode-block-attr-value-face ((,c :inherit modus-themes-bold :foreground ,type)))
    `(web-mode-block-comment-face ((,c :inherit modus-themes-slant :foreground ,comment)))
    `(web-mode-block-control-face ((,c :inherit modus-themes-bold :foreground ,builtin)))
    `(web-mode-block-delimiter-face ((,c :foreground ,fg-main)))
    `(web-mode-block-face ((,c :background ,bg-dim)))
    `(web-mode-block-string-face ((,c :foreground ,string)))
    `(web-mode-bold-face ((,c :inherit bold)))
    `(web-mode-builtin-face ((,c :inherit modus-themes-bold :foreground ,builtin)))
    `(web-mode-comment-face ((,c :inherit modus-themes-slant :foreground ,comment)))
    `(web-mode-comment-keyword-face ((,c :inherit modus-themes-bold :foreground ,warning)))
    `(web-mode-constant-face ((,c :foreground ,constant)))
    `(web-mode-css-at-rule-face ((,c :foreground ,constant)))
    `(web-mode-css-color-face ((,c :inherit modus-themes-bold :foreground ,builtin)))
    `(web-mode-css-comment-face ((,c :inherit modus-themes-slant :foreground ,comment)))
    `(web-mode-css-function-face ((,c :inherit modus-themes-bold :foreground ,builtin)))
    `(web-mode-css-priority-face ((,c :inherit modus-themes-bold :foreground ,warning)))
    `(web-mode-css-property-name-face ((,c :inherit modus-themes-bold :foreground ,keyword)))
    `(web-mode-css-pseudo-class-face ((,c :inherit modus-themes-slant :foreground ,docstring)))
    `(web-mode-css-selector-face ((,c :inherit modus-themes-bold :foreground ,keyword)))
    `(web-mode-css-string-face ((,c :foreground ,string)))
    `(web-mode-css-variable-face ((,c :foreground ,variable)))
    `(web-mode-current-column-highlight-face ((,c :background ,bg-inactive)))
    `(web-mode-current-element-highlight-face ((,c :background ,bg-paren-match :foreground ,fg-paren-match :underline ,underline-paren-match)))
    `(web-mode-doctype-face ((,c :inherit modus-themes-slant :foreground ,docstring)))
    `(web-mode-error-face ((,c :foreground ,err)))
    `(web-mode-filter-face ((,c :foreground ,fnname)))
    `(web-mode-folded-face ((,c :inherit underline)))
    `(web-mode-function-call-face ((,c :foreground ,fnname)))
    `(web-mode-function-name-face ((,c :foreground ,fnname)))
    `(web-mode-html-attr-custom-face ((,c :foreground ,variable)))
    `(web-mode-html-attr-engine-face ((,c :foreground ,fg-main)))
    `(web-mode-html-attr-equal-face ((,c :foreground ,fg-main)))
    `(web-mode-html-attr-name-face ((,c :foreground ,variable)))
    `(web-mode-html-attr-value-face ((,c :foreground ,constant)))
    `(web-mode-html-entity-face ((,c :foreground ,err)))
    `(web-mode-html-tag-bracket-face ((,c :foreground ,fg-dim)))
    `(web-mode-html-tag-custom-face ((,c :foreground ,fnname)))
    `(web-mode-html-tag-face ((,c :foreground ,fnname)))
    `(web-mode-html-tag-namespaced-face ((,c :inherit modus-themes-bold :foreground ,builtin)))
    `(web-mode-html-tag-unclosed-face ((,c :foreground ,err :underline t)))
    `(web-mode-inlay-face ((,c :background ,bg-inactive)))
    `(web-mode-italic-face ((,c :inherit italic)))
    `(web-mode-javascript-comment-face ((,c :inherit modus-themes-slant :foreground ,comment)))
    `(web-mode-javascript-string-face ((,c :foreground ,string)))
    `(web-mode-json-comment-face ((,c :inherit modus-themes-slant :foreground ,comment)))
    `(web-mode-json-context-face ((,c :inherit modus-themes-bold :foreground ,builtin)))
    `(web-mode-json-key-face ((,c :foreground ,string)))
    `(web-mode-json-string-face ((,c :foreground ,string)))
    `(web-mode-keyword-face ((,c :inherit modus-themes-bold :foreground ,keyword)))
    `(web-mode-param-name-face ((,c :foreground ,fnname)))
    `(web-mode-part-comment-face ((,c :inherit modus-themes-slant :foreground ,comment)))
    `(web-mode-part-face ((,c :background ,bg-dim)))
    `(web-mode-part-string-face ((,c :foreground ,string)))
    `(web-mode-preprocessor-face ((,c :foreground ,preprocessor)))
    `(web-mode-script-face ((,c :background ,bg-dim)))
    `(web-mode-sql-keyword-face ((,c :foreground ,err)))
    `(web-mode-string-face ((,c :foreground ,string)))
    `(web-mode-style-face ((,c :background ,bg-dim)))
    `(web-mode-symbol-face ((,c :foreground ,constant)))
    `(web-mode-type-face ((,c :inherit modus-themes-bold :foreground ,builtin)))
    `(web-mode-underline-face ((,c :inherit underline)))
    `(web-mode-variable-name-face ((,c :foreground ,variable)))
    `(web-mode-warning-face ((,c :foreground ,warning)))
    `(web-mode-whitespace-face ((,c :background ,bg-inactive)))
;;;;; wgrep
    `(wgrep-delete-face ((,c :foreground ,warning)))
    `(wgrep-done-face ((,c :foreground ,info)))
    `(wgrep-face ((,c :inherit bold)))
    `(wgrep-file-face ((,c :foreground ,fg-alt)))
    `(wgrep-reject-face ((,c :foreground ,err)))
;;;;; which-function-mode
    `(which-func ((,c :inherit modus-themes-bold :foreground ,modeline-info))) ; same as `breadcrumb-imenu-leaf-face'
;;;;; which-key
    `(which-key-command-description-face ((,c :foreground ,fg-main)))
    `(which-key-group-description-face ((,c :foreground ,type)))
    `(which-key-highlighted-command-face ((,c :foreground ,warning :underline t)))
    `(which-key-key-face ((,c :inherit (bold modus-themes-fixed-pitch) :foreground ,keybind)))
    `(which-key-local-map-description-face ((,c :foreground ,fg-main)))
    `(which-key-note-face ((,c :foreground ,fg-dim)))
    `(which-key-separator-face ((,c :foreground ,fg-dim)))
    `(which-key-special-key-face ((,c :foreground ,err)))
;;;;; whitespace-mode
    `(whitespace-big-indent ((,c :background ,bg-space-err)))
    `(whitespace-empty ((,c :background ,bg-space)))
    `(whitespace-hspace ((,c :background ,bg-space :foreground ,fg-space)))
    `(whitespace-indentation ((,c :background ,bg-space :foreground ,fg-space)))
    `(whitespace-line ((,c :background ,bg-space :foreground ,warning)))
    `(whitespace-newline ((,c :background ,bg-space :foreground ,fg-space)))
    `(whitespace-space ((,c :background ,bg-space :foreground ,fg-space)))
    `(whitespace-space-after-tab ((,c :background ,bg-space :foreground ,warning)))
    `(whitespace-space-before-tab ((,c :background ,bg-space :foreground ,warning)))
    `(whitespace-tab ((,c :background ,bg-space :foreground ,fg-space)))
    `(whitespace-trailing ((,c :background ,bg-space-err)))
;;;;; window-divider-mode
    `(window-divider ((,c :foreground ,border)))
    `(window-divider-first-pixel ((,c :foreground ,bg-inactive)))
    `(window-divider-last-pixel ((,c :foreground ,bg-inactive)))
;;;;; window-tool-bar-mode
    `(window-tool-bar-button ((,c :inherit modus-themes-button)))
    `(window-tool-bar-button-hover ((,c :inherit modus-themes-button :background ,bg-hover :foreground ,fg-main)))
    `(window-tool-bar-button-disabled ((,c :inherit modus-themes-button :background ,bg-button-inactive :foreground ,fg-button-inactive)))
;;;;; widget
    `(widget-button ((,c :inherit modus-themes-bold :foreground ,fg-link)))
    `(widget-button-pressed ((,c :inherit modus-themes-bold :foreground ,fg-link-visited)))
    `(widget-documentation ((,c :inherit modus-themes-slant :foreground ,docstring)))
    `(widget-field ((,c :background ,bg-button-inactive :foreground ,fg-button-active :extend nil :underline (:position t))))
    `(widget-inactive ((,c :background ,bg-button-inactive :foreground ,fg-button-inactive)))
    `(widget-single-line-field ((,c :background ,bg-button-inactive :foreground ,fg-button-active :extend nil :underline (:position t))))
;;;;; writegood-mode
    `(writegood-duplicates-face ((,c :underline (:style wave :color ,underline-err))))
    `(writegood-passive-voice-face ((,c :underline (:style wave :color ,underline-warning))))
    `(writegood-weasels-face ((,c :underline (:style wave :color ,underline-warning))))
;;;;; woman
    `(woman-addition ((,c :foreground ,accent-2)))
    `(woman-bold ((,c :inherit bold :foreground ,accent-0)))
    `(woman-italic ((,c :inherit italic :foreground ,accent-1)))
    `(woman-unknown ((,c :foreground ,accent-3)))
;;;;; xah-elisp-mode
    `(xah-elisp-at-symbol ((,c :inherit modus-themes-bold :foreground ,warning)))
    `(xah-elisp-cap-variable ((,c :foreground ,preprocessor)))
    `(xah-elisp-command-face ((,c :inherit modus-themes-bold :foreground ,type)))
    `(xah-elisp-dollar-symbol ((,c :foreground ,variable)))
;;;;; xref
    `(xref-file-header ((,c :foreground ,name)))
;;;;; yaml-mode
    `(yaml-tab-face ((,c :background ,bg-space-err)))
;;;;; yasnippet
    `(yas-field-highlight-face ((,c :background ,bg-hover :foreground ,fg-main)))
;;;;; ztree
    `(ztreep-arrow-face ((,c :foreground ,fg-dim)))
    `(ztreep-diff-header-face ((,c :inherit modus-themes-heading-0)))
    `(ztreep-diff-header-small-face ((,c :inherit modus-themes-slant :foreground ,docstring)))
    `(ztreep-diff-model-add-face ((,c :foreground ,info)))
    `(ztreep-diff-model-diff-face ((,c :foreground ,err)))
    `(ztreep-diff-model-ignored-face ((,c :foreground ,fg-dim :strike-through t)))
    `(ztreep-diff-model-normal-face (( )))
    `(ztreep-expand-sign-face ((,c :foreground ,fg-dim)))
    `(ztreep-header-face ((,c :inherit modus-themes-heading-0)))
    `(ztreep-leaf-face (( )))
    `(ztreep-node-count-children-face ((,c :inherit modus-themes-slant :foreground ,fg-dim)))
    `(ztreep-node-face ((,c :foreground ,accent-0))))
  "Face specs for use with `modus-themes-theme'.")

(defconst modus-themes-faces-deuteranopia
  '(
    `(transient-key-exit ((,c :inherit (bold modus-themes-fixed-pitch) :foreground ,yellow-warmer)))
    `(transient-key-noop ((,c :inherit modus-themes-fixed-pitch :foreground ,fg-dim)))
    `(transient-key-recurse ((,c :inherit (bold modus-themes-fixed-pitch) :foreground ,fg-main)))
    `(transient-key-return ((,c :inherit (bold modus-themes-fixed-pitch) :foreground ,yellow-cooler)))
    `(transient-key-stack ((,c :inherit (bold modus-themes-fixed-pitch) :foreground ,blue-cooler)))
    `(transient-key-stay ((,c :inherit (bold modus-themes-fixed-pitch) :foreground ,cyan-faint))))
  "Faces that must be used by themes that are optimized for deuteranopia.")

(defconst modus-themes-faces-tritanopia
  '(
    `(transient-key-exit ((,c :inherit (bold modus-themes-fixed-pitch) :foreground ,red-warmer)))
    `(transient-key-noop ((,c :inherit modus-themes-fixed-pitch :foreground ,fg-dim)))
    `(transient-key-recurse ((,c :inherit (bold modus-themes-fixed-pitch) :foreground ,fg-main)))
    `(transient-key-return ((,c :inherit (bold modus-themes-fixed-pitch) :foreground ,magenta)))
    `(transient-key-stack ((,c :inherit (bold modus-themes-fixed-pitch) :foreground ,cyan)))
    `(transient-key-stay ((,c :inherit (bold modus-themes-fixed-pitch) :foreground ,cyan-faint))))
  "Faces that must be used by themes that are optimized for tritanopia.")

(defconst modus-themes-custom-variables
  '(
;;;; ansi-colors
    `(ansi-color-faces-vector [default bold shadow italic underline success warning error])
    `(ansi-color-names-vector ["#595959" ,red ,green ,yellow ,blue ,magenta ,cyan "#a6a6a6"])
;;;; chart
    `(chart-face-color-list
      '( ,bg-graph-red-0 ,bg-graph-green-0 ,bg-graph-yellow-0 ,bg-graph-blue-0 ,bg-graph-magenta-0 ,bg-graph-cyan-0
         ,bg-graph-red-1 ,bg-graph-green-1 ,bg-graph-yellow-1 ,bg-graph-blue-1 ,bg-graph-magenta-1 ,bg-graph-cyan-1))
;;;; exwm
    `(exwm-floating-border-color ,border)
;;;; highlight-changes
    `(highlight-changes-colors nil)
    `(highlight-changes-face-list '(success warning error bold bold-italic))
;;;; ibuffer
    `(ibuffer-filter-group-name-face 'bold)
    `(ibuffer-title-face 'default)
;;;; hl-todo
    `(hl-todo-keyword-faces
      '(("HOLD" . ,warning)
        ("TODO" . ,err)
        ("NEXT" . ,fg-alt)
        ("THEM" . ,fg-alt)
        ("PROG" . ,info)
        ("OKAY" . ,info)
        ("DONT" . ,warning)
        ("FAIL" . ,err)
        ("BUG" . ,err)
        ("DONE" . ,info)
        ("NOTE" . ,warning)
        ("KLUDGE" . ,warning)
        ("HACK" . ,warning)
        ("TEMP" . ,warning)
        ("FIXME" . ,err)
        ("XXX+" . ,err)
        ("REVIEW" . ,info)
        ("DEPRECATED" . ,info)))
;;;; pdf-tools
    `(pdf-view-midnight-colors '(,fg-main . ,bg-dim))
;;;; rustic-ansi-faces
    `(rustic-ansi-faces
      [,fg-term-black
       ,fg-term-red
       ,fg-term-green
       ,fg-term-yellow
       ,fg-term-blue
       ,fg-term-magenta
       ,fg-term-cyan
       ,fg-term-white])
;;;; viper
    `(viper-replace-overlay-cursor-color ,err)
    `(viper-insert-state-cursor-color ,info)
    `(viper-emacs-state-cursor-color ,fg-main)
;;;; xterm-color
    `(xterm-color-names
      [,fg-term-black
       ,fg-term-red
       ,fg-term-green
       ,fg-term-yellow
       ,fg-term-blue
       ,fg-term-magenta
       ,fg-term-cyan
       ,fg-term-white])
    `(xterm-color-names-bright
      [,fg-term-black-bright
       ,fg-term-red-bright
       ,fg-term-green-bright
       ,fg-term-yellow-bright
       ,fg-term-blue-bright
       ,fg-term-magenta-bright
       ,fg-term-cyan-bright
       ,fg-term-white-bright]))
  "Custom variables for `modus-themes-theme'.")

;;; Theme macros

;;;; Instantiate a Modus theme

(defun modus-themes-declare (name family description background-mode core-palette user-palette overrides-palette)
  "Declare NAME theme that belongs to FAMILY.
All of DESCRIPTION, BACKGROUND-MODE, CORE-PALETTE, USER-PALETTE,
OVERRIDES-PALETTE have the same meaning as in `modus-themes-theme'.

To simply register the theme, use `modus-themes-register'."
  (custom-declare-theme
   name (intern (format "%s-theme" name))
   description
   (list :kind 'color-scheme :background-mode background-mode :family family
         :modus-core-palette core-palette :modus-user-palette user-palette
         :modus-overrides-palette overrides-palette :modus-documentation description)))

(defun modus-themes-register (name)
  "Add NAME theme to `modus-themes-registered-items'.
To actually declare a theme, instantiating with together with its
properties, use `modus-themes-declare'."
  (add-to-list 'modus-themes-registered-items name))

;;;###autoload
(defun modus-themes-theme (name family description background-mode core-palette user-palette overrides-palette &optional custom-faces custom-variables)
  "Define a Modus theme or derivative thereof.
NAME is the name of the new theme.  FAMILY is the collection of themes
it belongs to.  DESCRIPTION is its documentation string.
BACKGROUND-MODE is either `dark' or `light', in reference to the theme's
background color.  The CORE-PALETTE, USER-PALETTE, and OVERRIDES-PALETTE
are symbols of variables which define palettes commensurate with
`modus-themes-operandi-palette'.

The optional CUSTOM-FACES and CUSTOM-VARIABLES are joined together with
the `modus-themes-faces' and `modus-themes-custom-variables',
respectively.  A derivative theme defining those is thus overriding what
the Modus themes have by default.

Consult the manual for details on how to build a theme on top of the
`modus-themes': Info node `(modus-themes) Build on top of the Modus themes'."
  (let ((theme-exists-p (custom-theme-p name))
        (faces (append
                (symbol-value custom-faces)
                modus-themes-faces))
        (variables (append
                    modus-themes-custom-variables
                    (symbol-value custom-variables)
                    (list `'(frame-background-mode ',background-mode)))))
    (unless theme-exists-p
      (modus-themes-declare
       name family
       description background-mode
       core-palette user-palette overrides-palette)
      (unless (eq family 'modus-themes)
        (modus-themes-register name)))
    (let ((palette (modus-themes--get-theme-palette-subr name :with-overrides :with-user-palette)))
      (eval
       `(let* ((c '((class color) (min-colors 256)))
               (palette (modus-themes--get-theme-palette-subr ',name :with-overrides :with-user-palette))
               ,@(mapcar
                  (lambda (entry)
                    (let ((name (car entry)))
                      (list name `(modus-themes--retrieve-palette-value ',name palette))))
                  palette))
          (custom-theme-set-faces
           ',name
           ,@faces)
          (custom-theme-set-variables
           ',name
           ,@variables))
       :lexical))
    (unless theme-exists-p
      (provide-theme name))))

;;;; Use theme colors

(defun modus-themes--with-colors-get-palette (theme)
  "Get THEME palette for `modus-themes-with-colors'.
Return (list CORE USER OVERRIDES) palettes."
  (when-let* ((properties (get theme 'theme-properties))
              (core-palette (symbol-value (plist-get properties :modus-core-palette))))
    (let* ((user-palette (symbol-value (plist-get properties :modus-user-palette)))
           (overrides-palette (symbol-value (plist-get properties :modus-overrides-palette)))
           (all-overrides (append overrides-palette modus-themes-common-palette-overrides)))
      (list core-palette user-palette all-overrides))))

(defun modus-themes--with-colors-resolve-palette-sort (colors)
  "Sort all COLORS in the theme's palette.
Put all named colors before semantic color mappings.  A named color is a
symbol whose value is a string.  A semantic color mapping is a symbol
whose value is another symbol, which ultimately resolves to a string or
`unspecified'."
  (let ((named nil)
        (semantic nil))
    (dolist (group colors)
      (dolist (color group)
        (if (stringp (cadr color))
            (push color named)
          (push color semantic))))
    (let* ((unique-fn (lambda (sequence)
                        (seq-uniq sequence
                                  (lambda (elt1 elt2)
                                    (eq (car elt1) (car elt2))))))
           (named-unique (funcall unique-fn named))
           (semantic-unique (funcall unique-fn semantic)))
      (nreverse (nconc semantic-unique named-unique)))))

(defun modus-themes-with-colors-subr (body)
  "Do the work of `modus-themes-with-colors' for BODY."
  (condition-case data
      (when-let* ((modus-themes-with-colors--current (modus-themes-get-current-theme))
                  (palette (modus-themes--with-colors-get-palette modus-themes-with-colors--current))
                  (sorted (modus-themes--with-colors-resolve-palette-sort palette)))
        (eval
         `(let* ((c '((class color) (min-colors 256)))
                 (unspecified 'unspecified)
                 ,@sorted)
            (funcall ',body))))
    (error (message "Error in `modus-themes-with-colors': %s" data))))

(defmacro modus-themes-with-colors (&rest body)
  "Evaluate BODY with colors from current palette bound."
  (declare (indent 0))
  `(modus-themes-with-colors-subr (lambda () ,@body)))

;;;; Declare all the Modus themes

(defconst modus-themes-with-properties
  '((modus-operandi-deuteranopia modus-themes "Deuteranopia-optimized theme with a white background." light modus-themes-operandi-deuteranopia-palette modus-operandi-deuteranopia-palette-user modus-operandi-deuteranopia-palette-overrides)
    (modus-operandi modus-themes "Elegant, highly legible theme with a white background." light modus-themes-operandi-palette modus-operandi-palette-user modus-operandi-palette-overrides)
    (modus-operandi-tinted modus-themes "Elegant, highly legible theme with a light ochre background." light modus-themes-operandi-tinted-palette modus-operandi-tinted-palette-user modus-operandi-tinted-palette-overrides)
    (modus-operandi-tritanopia modus-themes "Tritanopia-optimized theme with a white background." light modus-themes-operandi-tritanopia-palette modus-operandi-tritanopia-palette-user modus-operandi-tritanopia-palette-overrides)
    (modus-vivendi-deuteranopia modus-themes "Deuteranopia-optimized theme with a black background." dark modus-themes-vivendi-deuteranopia-palette modus-vivendi-deuteranopia-palette-user modus-vivendi-deuteranopia-palette-overrides)
    (modus-vivendi modus-themes "Elegant, highly legible theme with a black background." dark modus-themes-vivendi-palette modus-vivendi-palette-user modus-vivendi-palette-overrides)
    (modus-vivendi-tinted modus-themes "Elegant, highly legible theme with a night sky background." dark modus-themes-vivendi-tinted-palette modus-vivendi-tinted-palette-user modus-vivendi-tinted-palette-overrides)
    (modus-vivendi-tritanopia modus-themes "Tritanopia-optimized theme with a black background." dark modus-themes-vivendi-tritanopia-palette modus-vivendi-tritanopia-palette-user modus-vivendi-tritanopia-palette-overrides)))

(defvar modus-themes--declared-p nil)

(defun modus-themes-declare-themes ()
  "Declare the Modus themes."
  (unless modus-themes--declared-p
    (dolist (theme modus-themes-with-properties)
      (apply #'modus-themes-declare theme))
    (setq modus-themes--declared-p t)))

(modus-themes-declare-themes)

;;;; Accept all Modus themes and their derivatives

;;;###autoload
(define-minor-mode modus-themes-include-derivatives-mode
  "When enabled, all Modus themes commands cover derivatives as well.
Otherwise, they only consider the `modus-themes-items'.

Derivative theme projects can implement the equivalent of this minor
mode plus a method for `modus-themes-get-themes' to filter themes
accordingly."
  :global t
  :init-value nil)

(cl-defmethod modus-themes-get-themes (&context (modus-themes-include-derivatives-mode (eql t)))
  "Return list of Modus themes per `modus-themes-include-derivatives-mode'."
  (if-let* ((themes (modus-themes-get-all-known-themes nil))
            (sorted-themes (modus-themes-sort themes 'light)))
      sorted-themes
    modus-themes-items))

;;;; Let derivative themes create commands to load only their themes

(defvar modus-themes-define-derivative-command-known-suffixes
  '( toggle rotate select select-dark select-light
     load-random load-random-dark load-random-light
     list-colors list-colors-current)
  "Command suffixes accepted by `modus-themes-define-derivative-command'.")

(defmacro modus-themes-define-derivative-command (family suffix)
  "Define convenience command with SUFFIX to load only FAMILY themes.
SUFFIX is a symbol among those listed in the variable
`modus-themes-define-derivative-command-known-suffixes'.  The newly
defined command's symbol is FAMILY-SUFFIX, like `modus-themes-rotate'."
  (unless (memq suffix modus-themes-define-derivative-command-known-suffixes)
    (error "Cannot define command with unknown suffix `%s'" suffix))
  (let ((modus-command (intern (format "modus-themes-%s" suffix))))
    `(defun ,(intern (format "%s-%s" family suffix)) ()
       ,(format "Like `%s' but only for the `%s'" modus-command family)
       (interactive)
       (cl-letf (((symbol-function 'modus-themes-get-themes)
                  (lambda ()
                    (modus-themes-get-all-known-themes ',family))))
         (call-interactively ',modus-command)))))

;;;; Generate a palette given the base colors

;; NOTE 2025-11-25: This is a copy of `color-blend' from Emacs 31.  We
;; should remove this in the future.
(defun modus-themes-blend (a b &optional alpha)
  "Blend the two colors A and B in linear space with ALPHA.
A and B should be lists (RED GREEN BLUE), where each element is
between 0.0 and 1.0, inclusive.  ALPHA controls the influence A
has on the result and should be between 0.0 and 1.0, inclusive.

For instance:

   (modus-themes-blend \\='(1 0.5 1) \\='(0 0 0) 0.75)
      => (0.75 0.375 0.75)"
  (setq alpha (or alpha 0.5))
  (let (blend)
    (dotimes (i 3)
      (push (+ (* (nth i a) alpha) (* (nth i b) (- 1 alpha))) blend))
    (nreverse blend)))

(defun modus-themes--color-six-digits (color)
  "Reduce representation of hexadecimal RGB COLOR to six digits."
  (let ((color-no-hash (substring color 1)))
    (if (= (length color-no-hash) 6)
        color
      (let* ((triplets (seq-split color-no-hash 4))
             (triplets-shortened (mapcar
                                  (lambda (string)
                                    (substring string 0 2))
                                  triplets)))
        (concat "#" (string-join triplets-shortened))))))

(defun modus-themes-generate-color-blend (color blended-with alpha)
  "Return hexadecimal RGB of COLOR with BLENDED-WITH given ALPHA.
BLENDED-WITH is commensurate with COLOR.  ALPHA is between 0.0 and 1.0,
inclusive."
  (let* ((blend-rgb (modus-themes-blend (color-name-to-rgb color) (color-name-to-rgb blended-with) alpha))
         (blend-hex (apply #'color-rgb-to-hex blend-rgb)))
    (modus-themes--color-six-digits blend-hex)))

(defun modus-themes-generate-color-warmer (color alpha)
  "Return warmer COLOR by ALPHA, per `modus-themes-generate-color-blend'."
  (modus-themes-generate-color-blend color "#ff0000" alpha))

(defun modus-themes-generate-color-cooler (color alpha)
  "Return cooler COLOR by ALPHA, per `modus-themes-generate-color-blend'."
  (modus-themes-generate-color-blend color "#0000ff" alpha))

;; NOTE 2025-11-24: I originally wrote a variation of this for my Doric themes.
(defun modus-themes-generate-gradient (color percent)
  "Adjust value of COLOR by PERCENT."
  (pcase-let* ((`(,r ,g ,b) (color-name-to-rgb color))
               (color-luminance-dark-limit 0.5)
               (gradient (funcall (if (color-dark-p (list r g b))
                                      #'color-lighten-name
                                    #'color-darken-name)
                                  color
                                  percent)))
    (modus-themes--color-six-digits gradient)))

;; NOTE 2025-11-25: I used to rely on `color-distance', thinking that
;; it would do the right thing here:
;;
;;     (> (color-distance color "#ff0000") (color-distance color "#0000ff"))
;;
;; But my understanding of "warm" versus "cool" is simple, so better
;; do it my way.
(defun modus-themes-color-warm-p (color)
  "Return non-nil if COLOR is warm.
A warm color has more contribution from the red channel of light than
the blue one."
  (pcase-let ((`(,r ,_ ,b) (color-name-to-rgb color)))
    (> r b)))

(defun modus-themes-color-is-warm-or-cool-p (color)
  "Return `warm' or `cool' for COLOR depending on its value."
  (if (modus-themes-color-warm-p color)
      'warm
    'cool))

(defun modus-themes-generate-color-warmer-or-cooler (color alpha &optional preference)
  "Return COLOR variant by ALPHA and PREFERENCE.
PREFERENCE is either `cool' or `warm'.  An unknown PREFERENCE means
`cool'.  Without PREFERENCE, rely on the return value of
`modus-themes-color-is-warm-or-cool-p'."
  (let ((kind (or preference (modus-themes-color-is-warm-or-cool-p color))))
    (funcall
     (if (eq kind 'warm)
         #'modus-themes-generate-color-warmer
       'modus-themes-generate-color-cooler)
     color
     alpha)))

;;;###autoload
(defun modus-themes-generate-palette (base-colors &optional cool-or-warm-preference core-palette mappings)
  "Generate a palette given the BASE-COLORS.
BASE-COLORS consists of lists in the form (NAME VALUE).  NAME is at
least a symbol of `bg-main' or `fg-main', while VALUE is a string
representing a color either by name like in `list-colors-display' or
hexadecimal RGB of the form #123456.  See the value of a core Modus
palette, like `modus-themes-operandi-palette' for all current NAME
symbols.

BASE-COLORS is used to derive a palette.  Any entry whose name is
already present in BASE-COLORS is not derived but taken as-is.  The rest
are generated automatically.  The generated palette can be used as-is by
a derivative theme (per `modus-themes-theme') or as a starting point for
further refinements.

With optional COOL-OR-WARM-PREFERENCE as a symbol of either `cool' or
`warm' make relevant color choices for derivative values.  If
COOL-OR-WARM-PREFERENCE is nil, derive the implied preference from the
value of the `bg-main' color in BASE-COLORS.  If the value of `bg-main'
satisfies `color-gray-p', then fall back to `cool'.  For our purposes,
`cool' means that the color is closer to pure blue than pure red, while
`warm' is the opposite.

With optional CORE-PALETTE use it to fill in any of the remaining
entries.  This can be a symbol like `modus-themes-operandi-palette'.  Do
not try to enforce a core palette among those defined in modus-themes.el
and let the user assume responsibility for any incompatibilities.  If
CORE-PALETTE is nil, then infer a suitable palette based on whether the
`bg-main' value in BASE-COLORS is light or dark and then the
COOL-OR-WARM-PREFERENCE.  This inferred palette will be
`modus-themes-operandi-palette' for a light `bg-main' and
`modus-themes-vivendi-palette' for a dark `bg-main'.  The `cool' or
`warm' shall yield the tinted variants of those palettes, namely,
`modus-themes-operandi-tinted-palette' and
`modus-themes-vivendi-tinted-palette'.

With optional MAPPINGS use them instead of trying to derive new ones.
If MAPPINGS is nil, generate some essential color mappings and let the
rest come from CORE-PALETTE."
  (when (seq-some
         (lambda (entry)
           (not (stringp (cadr entry))))
         base-colors)
    (error "Base colors can only be references to string color values, not symbols"))
  (when (seq-some
         (lambda (entry)
           (stringp (cadr entry)))
         mappings)
    (error "Mappings can only be references to named colors, not color values"))
  (let ((bg-main (alist-get 'bg-main base-colors))
        (fg-main (alist-get 'fg-main base-colors)))
    (unless (and bg-main fg-main)
      (error "The palette must define at least a bg-main and fg-main entry with their values"))
    (let* ((bg-main (car bg-main))
           (bg-main-dark-p (color-dark-p (color-name-to-rgb bg-main)))
           (fg-main (car fg-main))
           (six-colors (seq-filter
                        (lambda (color)
                          (memq (car color) '(red green yellow blue magenta cyan)))
                        base-colors))
           (prefers-cool-p (cond
                            (cool-or-warm-preference (eq cool-or-warm-preference 'cool))
                            (t (eq (modus-themes-color-is-warm-or-cool-p bg-main) 'cool))))
           (derived-colors nil)
           (derived-mappings nil)
           (push-derived-value-fn (lambda (name value)
                                    (unless (assq name base-colors)
                                      (push (list name value) derived-colors))))
           (push-mapping-fn (lambda (name value)
                              (unless (assq name mappings)
                                (push (list name value) derived-mappings)))))
      ;; Base entries
      (funcall push-derived-value-fn 'bg-dim (modus-themes-generate-gradient bg-main 5))
      (funcall push-derived-value-fn 'bg-active (modus-themes-generate-gradient bg-main 10))
      (funcall push-derived-value-fn 'bg-inactive (modus-themes-generate-gradient bg-main 8))
      (funcall push-derived-value-fn 'border (modus-themes-generate-gradient bg-main 20))
      (funcall push-derived-value-fn 'fg-dim (modus-themes-generate-gradient fg-main 20))
      (funcall push-derived-value-fn 'fg-alt (modus-themes-generate-color-warmer-or-cooler (modus-themes-generate-gradient fg-main 10) 0.8 prefers-cool-p))
      ;; Primary and secondary colors
      (pcase-dolist (`(,name ,value) six-colors)
        (funcall push-derived-value-fn (intern (format "%s-warmer" name)) (modus-themes-generate-gradient (modus-themes-generate-color-warmer value 0.9) (if bg-main-dark-p 20 -20)))
        (funcall push-derived-value-fn (intern (format "%s-cooler" name)) (modus-themes-generate-gradient (modus-themes-generate-color-cooler value 0.9) (if bg-main-dark-p 20 -20)))
        (funcall push-derived-value-fn (intern (format "%s-faint" name)) (modus-themes-generate-gradient value (if bg-main-dark-p 10 -10)))
        (funcall push-derived-value-fn (intern (format "%s-intense" name)) (modus-themes-generate-gradient value (if bg-main-dark-p -5 5)))
        ;; TODO 2025-12-06: We should have a function here that adjusts the value also up to a
        ;; maximum distance from bg-main.  Basically, we want to avoid the scenario where a given
        ;; base value produces something that is virtually indistinguishable from bg-main.
        (funcall push-derived-value-fn (intern (format "bg-%s-intense" name)) (modus-themes-generate-gradient value (if bg-main-dark-p -40 40)))
        (funcall push-derived-value-fn (intern (format "bg-%s-subtle" name)) (modus-themes-generate-gradient value (if bg-main-dark-p -60 60)))
        (funcall push-derived-value-fn (intern (format "bg-%s-nuanced" name)) (modus-themes-generate-gradient value (if bg-main-dark-p -80 80))))
      ;; Mappings
      (funcall push-mapping-fn 'bg-completion (if prefers-cool-p 'bg-cyan-subtle 'bg-yellow-subtle))
      (funcall push-mapping-fn 'bg-hover (if prefers-cool-p 'bg-green-intense 'bg-magenta-intense))
      (funcall push-mapping-fn 'bg-hover-secondary (if prefers-cool-p 'bg-green-subtle 'bg-magenta-subtle))
      (funcall push-mapping-fn 'bg-hl-line (if prefers-cool-p 'bg-cyan-nuanced 'bg-yellow-nuanced))
      (funcall push-mapping-fn 'bg-paren-match (if prefers-cool-p 'bg-green-intense 'bg-yellow-subtle))
      (funcall push-mapping-fn 'bg-paren-expression (if prefers-cool-p 'bg-green-nuanced 'bg-yellow-nuanced))
      (funcall push-mapping-fn 'bg-region 'bg-active)
      (funcall push-mapping-fn 'fg-region 'fg-main)

      (funcall push-mapping-fn 'bg-mode-line-active 'bg-active)
      (funcall push-mapping-fn 'fg-mode-line-active 'fg-main)
      (funcall push-mapping-fn 'border-mode-line-active 'border)
      (funcall push-mapping-fn 'bg-mode-line-inactive 'bg-inactive)
      (funcall push-mapping-fn 'fg-mode-line-inactive 'fg-dim)
      (funcall push-mapping-fn 'border-mode-line-inactive 'border)

      (funcall push-mapping-fn 'modeline-err 'red-faint)
      (funcall push-mapping-fn 'modeline-warning 'yellow-faint)
      (funcall push-mapping-fn 'modeline-info 'blue-faint)

      (funcall push-mapping-fn 'bg-search-current 'bg-yellow-subtle)
      (funcall push-mapping-fn 'bg-search-lazy 'bg-magenta-subtle)
      (funcall push-mapping-fn 'bg-search-replace 'bg-red-subtle)
      (funcall push-mapping-fn 'bg-search-rx-group-0 'bg-blue-subtle)
      (funcall push-mapping-fn 'bg-search-rx-group-1 'bg-green-subtle)
      (funcall push-mapping-fn 'bg-search-rx-group-2 'bg-red-subtle)
      (funcall push-mapping-fn 'bg-search-rx-group-3 'bg-magenta-subtle)

      (funcall push-mapping-fn 'fg-search-current 'yellow-warmer)
      (funcall push-mapping-fn 'fg-search-lazy 'magenta-cooler)
      (funcall push-mapping-fn 'fg-search-replace 'red-cooler)
      (funcall push-mapping-fn 'fg-search-rx-group-0 'blue-warmer)
      (funcall push-mapping-fn 'fg-search-rx-group-1 'green-warmer)
      (funcall push-mapping-fn 'fg-search-rx-group-2 'red-cooler)
      (funcall push-mapping-fn 'fg-search-rx-group-3 'magenta-cooler)

      (funcall push-mapping-fn 'bg-prominent-err 'unspecified)
      (funcall push-mapping-fn 'bg-prominent-warning 'unspecified)
      (funcall push-mapping-fn 'bg-prominent-note 'unspecified)
      (funcall push-mapping-fn 'fg-prominent-err 'red-intense)
      (funcall push-mapping-fn 'fg-prominent-warning 'yellow-intense)
      (funcall push-mapping-fn 'fg-prominent-note 'green-intense)

      (funcall push-mapping-fn 'bg-active-argument (if prefers-cool-p 'bg-cyan-subtle 'bg-yellow-subtle))
      (funcall push-mapping-fn 'fg-active-argument (if prefers-cool-p 'cyan-cooler 'yellow-warmer))
      (funcall push-mapping-fn 'bg-active-value (if prefers-cool-p 'bg-magenta-subtle 'bg-blue-subtle))
      (funcall push-mapping-fn 'fg-active-value (if prefers-cool-p 'magenta-cooler 'blue-warmer))

      (funcall push-mapping-fn 'bg-tab-bar 'bg-dim)
      (funcall push-mapping-fn 'bg-tab-current 'bg-main)
      (funcall push-mapping-fn 'bg-tab-other 'bg-inactive)

      (funcall push-mapping-fn 'bg-added 'bg-green-subtle)
      (funcall push-mapping-fn 'bg-added-faint 'bg-green-nuanced)
      (funcall push-mapping-fn 'bg-added-refine 'bg-green-intense)
      (funcall push-mapping-fn 'fg-added 'green-faint)
      (funcall push-mapping-fn 'fg-added-intense 'green-intense)

      (funcall push-mapping-fn 'bg-changed 'bg-yellow-subtle)
      (funcall push-mapping-fn 'bg-changed-faint 'bg-yellow-nuanced)
      (funcall push-mapping-fn 'bg-changed-refine 'bg-yellow-intense)
      (funcall push-mapping-fn 'fg-changed 'yellow-faint)
      (funcall push-mapping-fn 'fg-changed-intense 'yellow-intense)

      (funcall push-mapping-fn 'bg-removed 'bg-red-subtle)
      (funcall push-mapping-fn 'bg-removed-faint 'bg-red-nuanced)
      (funcall push-mapping-fn 'bg-removed-refine 'bg-red-intense)
      (funcall push-mapping-fn 'fg-removed 'red-faint)
      (funcall push-mapping-fn 'fg-removed-intense 'red-intense)

      (funcall push-mapping-fn 'fg-heading-0 'fg-alt)
      (funcall push-mapping-fn 'fg-heading-1 'fg-main)
      (funcall push-mapping-fn 'fg-heading-2 (if prefers-cool-p 'cyan 'yellow))
      (funcall push-mapping-fn 'fg-heading-3 (if prefers-cool-p 'green 'magenta))
      (funcall push-mapping-fn 'fg-heading-4 (if prefers-cool-p 'blue 'red))
      (funcall push-mapping-fn 'fg-heading-5 (if prefers-cool-p 'yellow 'cyan))
      (funcall push-mapping-fn 'fg-heading-6 (if prefers-cool-p 'magenta 'green))
      (funcall push-mapping-fn 'fg-heading-7 (if prefers-cool-p 'red 'blue))
      (funcall push-mapping-fn 'fg-heading-8 'fg-dim)

      (funcall push-mapping-fn 'bg-term-black (if bg-main-dark-p 'bg-main 'fg-main))
      (funcall push-mapping-fn 'bg-term-black-bright (if bg-main-dark-p 'bg-active 'fg-dim))
      (funcall push-mapping-fn 'fg-term-black (if bg-main-dark-p 'bg-main 'fg-main))
      (funcall push-mapping-fn 'fg-term-black-bright (if bg-main-dark-p 'bg-active 'fg-dim))

      (funcall push-mapping-fn 'bg-term-white (if bg-main-dark-p 'fg-dim 'bg-active))
      (funcall push-mapping-fn 'bg-term-white-bright (if bg-main-dark-p 'fg-main 'bg-main))
      (funcall push-mapping-fn 'fg-term-white (if bg-main-dark-p 'fg-dim 'bg-active))
      (funcall push-mapping-fn 'fg-term-white-bright (if bg-main-dark-p 'fg-main 'bg-main))

      (let* ((new-colors (append base-colors derived-colors))
             (new-mappings (append mappings derived-mappings))
             ;; We have to add one of the core palettes to make sure
             ;; there are no missing entries.  We will then remove
             ;; duplicates.
             (core (or core-palette
                       (if bg-main-dark-p
                           (if prefers-cool-p modus-themes-vivendi-palette modus-themes-vivendi-tinted-palette)
                         (if prefers-cool-p modus-themes-operandi-palette modus-themes-operandi-tinted-palette))))
             (combined-new-palette (append new-colors new-mappings core))
             (no-duplicates (seq-uniq
                             combined-new-palette
                             (lambda (element1 element2)
                               (eq (car element1) (car element2)))))
             (named-values (seq-filter
                            (lambda (entry)
                              (stringp (cadr entry)))
                            no-duplicates))
             (mapping-values (seq-filter
                              (lambda (entry)
                                (symbolp (cadr entry)))
                              no-duplicates)))
        (append named-values mapping-values)))))

;;;; Add themes from package to path

;;;###autoload
(when load-file-name
  (let ((dir (file-name-directory load-file-name)))
    (unless (equal dir (expand-file-name "themes/" data-directory))
      (add-to-list 'custom-theme-load-path dir))))

(provide 'modus-themes)
;;; modus-themes.el ends here
