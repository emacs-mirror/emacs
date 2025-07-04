;;; css-mode.el --- Major mode to edit CSS files  -*- lexical-binding: t -*-

;; Copyright (C) 2006-2025 Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Maintainer: Simen Heggestøyl <simenheg@gmail.com>
;; Keywords: hypermedia

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

;;; Tree-sitter language versions
;;
;; css-ts-mode has been tested with the following grammars and version:
;; - tree-sitter-css: v0.23.1-1-g6a442a3
;;
;; We try our best to make builtin modes work with latest grammar
;; versions, so a more recent grammar has a good chance to work too.
;; Send us a bug report if it doesn't.

;;; Commentary:

;; Yet another CSS mode.

;;; Todo:

;; - filling code with auto-fill-mode
;; - fix font-lock errors with multi-line selectors

;;; Code:

(require 'cl-lib)
(require 'color)
(require 'eww)
(require 'imenu)
(require 'seq)
(require 'sgml-mode)
(require 'smie)
(require 'thingatpt)
(eval-when-compile (require 'subr-x)
                   (require 'rx))
(require 'treesit)

(declare-function treesit-parser-create "treesit.c")
(declare-function treesit-induce-sparse-tree "treesit.c")
(declare-function treesit-node-type "treesit.c")
(declare-function treesit-node-start "treesit.c")
(declare-function treesit-node-child "treesit.c")


(defgroup css nil
  "Cascading Style Sheets (CSS) editing mode."
  :group 'languages)

(defconst css-pseudo-class-ids
  '("active" "checked" "default" "disabled" "empty" "enabled" "first"
    "first-child" "first-of-type" "focus" "focus-within" "hover"
    "in-range" "indeterminate" "invalid" "lang" "last-child"
    "last-of-type" "left" "link" "not" "nth-child" "nth-last-child"
    "nth-last-of-type" "nth-of-type" "only-child" "only-of-type"
    "optional" "out-of-range" "read-only" "read-write" "required"
    "right" "root" "scope" "target" "valid" "visited")
  "Identifiers for pseudo-classes.")

(defconst css-pseudo-element-ids
  '("after" "before" "first-letter" "first-line" "selection")
  "Identifiers for pseudo-elements.")

(defconst css-at-ids
  '("charset" "font-face" "import" "keyframes" "media" "namespace"
    "page" "supports")
  "Identifiers that appear in the form @foo.")

(defconst scss-at-ids
  '("at-root" "content" "debug" "each" "else" "else if" "error" "extend"
    "for" "function" "if" "import" "include" "mixin" "return" "use" "warn"
    "while")
  "Additional identifiers that appear in the form @foo in SCSS.")

(defvar-local css--at-ids css-at-ids
  "List of at-rules for the current mode.")

(defconst css-bang-ids
  '("important")
  "Identifiers that appear in the form !foo.")

(defconst scss-bang-ids
  '("default" "global" "optional")
  "Additional identifiers that appear in the form !foo in SCSS.")

(defvar-local css--bang-ids css-bang-ids
  "List of bang-rules for the current mode.")

(defconst css-descriptor-ids
  '("ascent" "baseline" "bbox" "cap-height" "centerline" "definition-src"
    "descent" "font-family" "font-size" "font-stretch" "font-style"
    "font-variant" "font-weight" "mathline" "panose-1" "slope" "src" "stemh"
    "stemv" "topline" "unicode-range" "units-per-em" "widths" "x-height")
  "Identifiers for font descriptors.")

(defconst css-media-ids
  '("all" "aural" "bitmap" "continuous" "grid" "paged" "static" "tactile"
    "visual")
  "Identifiers for types of media.")

(defconst css-property-alist
  ;; CSS 2.1 properties (https://www.w3.org/TR/CSS21/propidx.html).
  ;;
  ;; Properties duplicated by any of the CSS3 modules below have been
  ;; removed.
  '(("azimuth" angle "left-side" "far-left" "left" "center-left"
     "center" "center-right" "right" "far-right" "right-side" "behind"
     "leftwards" "rightwards")
    ("border-collapse" "collapse" "separate")
    ("border-spacing" length)
    ("bottom" length percentage "auto")
    ("caption-side" "top" "bottom")
    ("clear" "none" "left" "right" "both")
    ("content" "normal" "none" string uri counter "attr()"
     "open-quote" "close-quote" "no-open-quote" "no-close-quote")
    ("counter-increment" identifier integer "none")
    ("counter-reset" identifier integer "none")
    ("cue" cue-before cue-after)
    ("cue-after" uri "none")
    ("cue-before" uri "none")
    ("display" "inline" "block" "list-item" "inline-block" "table"
     "inline-table" "table-row-group" "table-header-group"
     "table-footer-group" "table-row" "table-column-group"
     "table-column" "table-cell" "table-caption" "none"
     ;; CSS Flexible Box Layout Module Level 1
     ;; (https://www.w3.org/TR/css3-flexbox/#valdef-display-flex)
     "flex" "inline-flex"
     ;; CSS Grid Layout Module Level 1
     ;; (https://www.w3.org/TR/css-grid-1/#grid-containers)
     "grid" "inline-grid" "subgrid")
    ("elevation" angle "below" "level" "above" "higher" "lower")
    ("empty-cells" "show" "hide")
    ("float" "left" "right" "none")
    ("height" length percentage "auto")
    ("left" length percentage "auto")
    ("line-height" "normal" number length percentage)
    ("list-style" list-style-type list-style-position
     list-style-image)
    ("list-style-image" uri "none")
    ("list-style-position" "inside" "outside")
    ("list-style-type" "disc" "circle" "square" "decimal"
     "decimal-leading-zero" "lower-roman" "upper-roman" "lower-greek"
     "lower-latin" "upper-latin" "armenian" "georgian" "lower-alpha"
     "upper-alpha" "none")
    ("margin" margin-width)
    ("margin-bottom" margin-width)
    ("margin-left" margin-width)
    ("margin-right" margin-width)
    ("margin-top" margin-width)
    ("max-height" length percentage "none")
    ("max-width" length percentage "none")
    ("min-height" length percentage)
    ("min-width" length percentage)
    ("padding" padding-width)
    ("padding-bottom" padding-width)
    ("padding-left" padding-width)
    ("padding-right" padding-width)
    ("padding-top" padding-width)
    ("page-break-after" "auto" "always" "avoid" "left" "right")
    ("page-break-before" "auto" "always" "avoid" "left" "right")
    ("page-break-inside" "avoid" "auto")
    ("pause" time percentage)
    ("pause-after" time percentage)
    ("pause-before" time percentage)
    ("pitch" frequency "x-low" "low" "medium" "high" "x-high")
    ("pitch-range" number)
    ("play-during" uri "mix" "repeat" "auto" "none")
    ("position" "static" "relative" "absolute" "fixed")
    ("quotes" string "none")
    ("richness" number)
    ("right" length percentage "auto")
    ("speak" "normal" "none" "spell-out")
    ("speak-header" "once" "always")
    ("speak-numeral" "digits" "continuous")
    ("speak-punctuation" "code" "none")
    ("speech-rate" number "x-slow" "slow" "medium" "fast" "x-fast"
     "faster" "slower")
    ("stress" number)
    ("table-layout" "auto" "fixed")
    ("top" length percentage "auto")
    ("vertical-align" "baseline" "sub" "super" "top" "text-top"
     "middle" "bottom" "text-bottom" percentage length)
    ("visibility" "visible" "hidden" "collapse")
    ("voice-family" specific-voice generic-voice specific-voice
     generic-voice)
    ("volume" number percentage "silent" "x-soft" "soft" "medium"
     "loud" "x-loud")
    ("width" length percentage "auto")
    ("z-index" "auto" integer)

    ;; CSS Animations
    ;; (https://www.w3.org/TR/css3-animations/#property-index)
    ("animation" single-animation-name time single-timing-function
     single-animation-iteration-count single-animation-direction
     single-animation-fill-mode single-animation-play-state)
    ("animation-delay" time)
    ("animation-direction" single-animation-direction)
    ("animation-duration" time)
    ("animation-fill-mode" single-animation-fill-mode)
    ("animation-iteration-count" single-animation-iteration-count)
    ("animation-name" single-animation-name)
    ("animation-play-state" single-animation-play-state)
    ("animation-timing-function" single-timing-function)

    ;; CSS Backgrounds and Borders Module Level 3
    ;; (https://www.w3.org/TR/css3-background/#property-index)
    ("background" bg-layer final-bg-layer)
    ("background-attachment" attachment)
    ("background-clip" box)
    ("background-color" color)
    ("background-image" bg-image)
    ("background-origin" box)
    ("background-position" position)
    ("background-repeat" repeat-style)
    ("background-size" bg-size)
    ("border" line-width line-style color)
    ("border-bottom" line-width line-style color)
    ("border-bottom-color" color)
    ("border-bottom-left-radius" length percentage)
    ("border-bottom-right-radius" length percentage)
    ("border-bottom-style" line-style)
    ("border-bottom-width" line-width)
    ("border-color" color)
    ("border-image" border-image-source border-image-slice
     border-image-width border-image-outset border-image-repeat)
    ("border-image-outset" length number)
    ("border-image-repeat" "stretch" "repeat" "round" "space")
    ("border-image-slice" number percentage "fill")
    ("border-image-source" "none" image)
    ("border-image-width" length percentage number "auto")
    ("border-left" line-width line-style color)
    ("border-left-color" color)
    ("border-left-style" line-style)
    ("border-left-width" line-width)
    ("border-radius" length percentage)
    ("border-right" line-width line-style color)
    ("border-right-color" color)
    ("border-right-style" line-style)
    ("border-right-width" line-width)
    ("border-style" line-style)
    ("border-top" line-width line-style color)
    ("border-top-color" color)
    ("border-top-left-radius" length percentage)
    ("border-top-right-radius" length percentage)
    ("border-top-style" line-style)
    ("border-top-width" line-width)
    ("border-width" line-width)
    ("box-shadow" "none" shadow)

    ;; CSS Basic User Interface Module Level 3 (CSS3 UI)
    ;; (https://www.w3.org/TR/css3-ui/#property-index)
    ("box-sizing" "content-box" "border-box")
    ("caret-color" "auto" color)
    ("cursor" uri x y "auto" "default" "none" "context-menu" "help"
     "pointer" "progress" "wait" "cell" "crosshair" "text"
     "vertical-text" "alias" "copy" "move" "no-drop" "not-allowed"
     "grab" "grabbing" "e-resize" "n-resize" "ne-resize" "nw-resize"
     "s-resize" "se-resize" "sw-resize" "w-resize" "ew-resize"
     "ns-resize" "nesw-resize" "nwse-resize" "col-resize" "row-resize"
     "all-scroll" "zoom-in" "zoom-out")
    ("nav-down" "auto" id "current" "root" target-name)
    ("nav-left" "auto" id "current" "root" target-name)
    ("nav-right" "auto" id "current" "root" target-name)
    ("nav-up" "auto" id "current" "root" target-name)
    ("outline" outline-color outline-style outline-width)
    ("outline-color" color "invert")
    ("outline-offset" length)
    ("outline-style" "auto" border-style)
    ("outline-width" border-width)
    ("resize" "none" "both" "horizontal" "vertical")
    ("text-overflow" "clip" "ellipsis" string)

    ;; CSS Cascading and Inheritance Level 3
    ;; (https://www.w3.org/TR/css-cascade-3/#property-index)
    ("all")

    ;; CSS Color Module Level 3
    ;; (https://www.w3.org/TR/css3-color/#property)
    ("color" color)
    ("opacity" alphavalue)

    ;; CSS Containment Module Level 2
    ;; (https://www.w3.org/TR/css-contain-2/#property-index)
    ("contain" "none" "strict" "content" "size" "layout" "style" "paint")
    ("content-visibility" "visible" "auto" "hidden")

    ;; CSS Grid Layout Module Level 2
    ;; (https://www.w3.org/TR/css-grid-2/#property-index)
    ("grid" grid-template grid-template-rows "auto-flow" "dense"
     grid-auto-columns grid-auto-rows grid-template-columns)
    ("grid-area" grid-line)
    ("grid-auto-columns" track-size)
    ("grid-auto-flow" "row" "column" "dense")
    ("grid-auto-rows" track-size)
    ("grid-column" grid-line)
    ("grid-column-end" grid-line)
    ("grid-column-gap" length-percentage)
    ("grid-column-start" grid-line)
    ("grid-gap" grid-row-gap grid-column-gap)
    ("grid-row" grid-line)
    ("grid-row-end" grid-line)
    ("grid-row-gap" length-percentage)
    ("grid-row-start" grid-line)
    ("grid-template" "none" grid-template-rows grid-template-columns
     line-names string track-size line-names explicit-track-list)
    ("grid-template-areas" "none" string)
    ("grid-template-columns" "none" track-list auto-track-list "subgrid")
    ("grid-template-rows" "none" track-list auto-track-list "subgrid")

    ;; CSS Box Alignment Module Level 3
    ;; (https://www.w3.org/TR/css-align-3/#property-index)
    ("align-content" baseline-position content-distribution
     overflow-position content-position)
    ("align-items" "normal" "stretch" baseline-position
     overflow-position self-position)
    ("align-self" "auto" "normal" "stretch" baseline-position
     overflow-position self-position)
    ("column-gap" "normal" length-percentage)
    ("gap" row-gap column-gap)
    ("justify-content" "normal" content-distribution overflow-position
     content-position "left" "right")
    ("justify-items" "normal" "stretch" baseline-position
     overflow-position self-position "left" "right" "legacy" "center")
    ("justify-self" "auto" "normal" "stretch" baseline-position
     overflow-position self-position "left" "right")
    ("place-content" align-content justify-content)
    ("place-items" align-items justify-items)
    ("place-self" justify-self align-self)
    ("row-gap" "normal" length-percentage)

    ;; CSS Flexible Box Layout Module Level 1
    ;; (https://www.w3.org/TR/css-flexbox-1/#property-index)
    ("flex" "none" flex-grow flex-shrink flex-basis)
    ("flex-basis" "auto" "content" width)
    ("flex-direction" "row" "row-reverse" "column" "column-reverse")
    ("flex-flow" flex-direction flex-wrap)
    ("flex-grow" number)
    ("flex-shrink" number)
    ("flex-wrap" "nowrap" "wrap" "wrap-reverse")
    ("order" integer)

    ;; CSS Fonts Module Level 3
    ;; (https://www.w3.org/TR/css3-fonts/#property-index)
    ("font" font-style font-variant-css21 font-weight font-stretch
     font-size line-height font-family "caption" "icon" "menu"
     "message-box" "small-caption" "status-bar")
    ("font-family" family-name generic-family)
    ("font-feature-settings" "normal" feature-tag-value)
    ("font-kerning" "auto" "normal" "none")
    ("font-language-override" "normal" string)
    ("font-size" absolute-size relative-size length percentage)
    ("font-size-adjust" "none" number)
    ("font-stretch" "normal" "ultra-condensed" "extra-condensed"
     "condensed" "semi-condensed" "semi-expanded" "expanded"
     "extra-expanded" "ultra-expanded")
    ("font-style" "normal" "italic" "oblique")
    ("font-synthesis" "none" "weight" "style")
    ("font-variant" "normal" "none" common-lig-values
     discretionary-lig-values historical-lig-values
     contextual-alt-values "stylistic()" "historical-forms"
     "styleset()" "character-variant()" "swash()" "ornaments()"
     "annotation()" "small-caps" "all-small-caps" "petite-caps"
     "all-petite-caps" "unicase" "titling-caps" numeric-figure-values
     numeric-spacing-values numeric-fraction-values "ordinal"
     "slashed-zero" east-asian-variant-values east-asian-width-values
     "ruby")
    ("font-variant-alternates" "normal" "stylistic()"
     "historical-forms" "styleset()" "character-variant()" "swash()"
     "ornaments()" "annotation()")
    ("font-variant-caps" "normal" "small-caps" "all-small-caps"
     "petite-caps" "all-petite-caps" "unicase" "titling-caps")
    ("font-variant-east-asian" "normal" east-asian-variant-values
     east-asian-width-values "ruby")
    ("font-variant-ligatures" "normal" "none" common-lig-values
     discretionary-lig-values historical-lig-values
     contextual-alt-values)
    ("font-variant-numeric" "normal" numeric-figure-values
     numeric-spacing-values numeric-fraction-values "ordinal"
     "slashed-zero")
    ("font-variant-position" "normal" "sub" "super")
    ("font-weight" "normal" "bold" "bolder" "lighter" "100" "200"
     "300" "400" "500" "600" "700" "800" "900")

    ;; CSS Fragmentation Module Level 3
    ;; (https://www.w3.org/TR/css-break-3/#property-index)
    ("box-decoration-break" "slice" "clone")
    ("break-after" "auto" "avoid" "avoid-page" "page" "left" "right"
     "recto" "verso" "avoid-column" "column" "avoid-region" "region")
    ("break-before" "auto" "avoid" "avoid-page" "page" "left" "right"
     "recto" "verso" "avoid-column" "column" "avoid-region" "region")
    ("break-inside" "auto" "avoid" "avoid-page" "avoid-column"
     "avoid-region")
    ("orphans" integer)
    ("widows" integer)

    ;; CSS Masking Module Level 1
    ;; (https://www.w3.org/TR/css-masking-1/#property-index)
    ("clip-path" clip-source basic-shape geometry-box "none")
    ("clip-rule" "nonzero" "evenodd")
    ("mask-image" mask-reference)
    ("mask-mode" masking-mode)
    ("mask-repeat" repeat-style)
    ("mask-position" position)
    ("mask-clip" geometry-box "no-clip")
    ("mask-origin" geometry-box)
    ("mask-size" bg-size)
    ("mask-composite" compositing-operator)
    ("mask" mask-layer)
    ("mask-border-source" "none" image)
    ("mask-border-mode" "luminance" "alpha")
    ("mask-border-slice" number percentage "fill")
    ("mask-border-width" length percentage number "auto")
    ("mask-border-outset" length number)
    ("mask-border-repeat" "stretch" "repeat" "round" "space")
    ("mask-border" mask-border-source mask-border-slice
     mask-border-width mask-border-outset mask-border-repeat
     mask-border-mode)
    ("mask-type" "luminance" "alpha")
    ("clip" "rect()" "auto")

    ;; CSS Multi-column Layout Module Level 1
    ;; (https://www.w3.org/TR/css3-multicol/#property-index)
    ;; "break-after", "break-before", and "break-inside" are left out
    ;; below, because they're already included in CSS Fragmentation
    ;; Module Level 3.
    ("column-count" "auto" integer)
    ("column-fill" "auto" "balance" "balance-all")
    ("column-rule" column-rule-width column-rule-style
     column-rule-color)
    ("column-rule-color" color)
    ("column-rule-style" line-style)
    ("column-rule-width" line-width)
    ("column-span" "none" "all")
    ("column-width" "auto" length)
    ("columns" column-width column-count)

    ;; CSS Overflow Module Level 3
    ;; (https://www.w3.org/TR/css-overflow-3/#property-index)
    ("max-lines" "none" integer)
    ("overflow" "visible" "hidden" "scroll" "auto" "paged-x" "paged-y"
     "paged-x-controls" "paged-y-controls" "fragments")
    ("overflow-x" "visible" "hidden" "scroll" "auto" "paged-x"
     "paged-y" "paged-x-controls" "paged-y-controls" "fragments")
    ("overflow-y" "visible" "hidden" "scroll" "auto" "paged-x"
     "paged-y" "paged-x-controls" "paged-y-controls" "fragments")

    ;; CSS Text Decoration Module Level 3
    ;; (https://dev.w3.org/csswg/css-text-decor-3/#property-index)
    ("text-decoration" text-decoration-line text-decoration-style
     text-decoration-color)
    ("text-decoration-color" color)
    ("text-decoration-line" "none" "underline" "overline"
     "line-through" "blink")
    ("text-decoration-skip" "none" "objects" "spaces" "ink" "edges"
     "box-decoration")
    ("text-decoration-style" "solid" "double" "dotted" "dashed"
     "wavy")
    ("text-emphasis" text-emphasis-style text-emphasis-color)
    ("text-emphasis-color" color)
    ("text-emphasis-position" "over" "under" "right" "left")
    ("text-emphasis-style" "none" "filled" "open" "dot" "circle"
     "double-circle" "triangle" "sesame" string)
    ("text-shadow" "none" length color)
    ("text-underline-position" "auto" "under" "left" "right")

    ;; CSS Text Module Level 3
    ;; (https://www.w3.org/TR/css3-text/#property-index)
    ("hanging-punctuation" "none" "first" "force-end" "allow-end"
     "last")
    ("hyphens" "none" "manual" "auto")
    ("letter-spacing" "normal" length)
    ("line-break" "auto" "loose" "normal" "strict")
    ("overflow-wrap" "normal" "break-word")
    ("tab-size" integer length)
    ("text-align" "start" "end" "left" "right" "center" "justify"
     "match-parent")
    ("text-align-last" "auto" "start" "end" "left" "right" "center"
     "justify")
    ("text-indent" length percentage)
    ("text-justify" "auto" "none" "inter-word" "distribute")
    ("text-transform" "none" "capitalize" "uppercase" "lowercase"
     "full-width")
    ("white-space" "normal" "pre" "nowrap" "pre-wrap" "pre-line")
    ("word-break" "normal" "keep-all" "break-all")
    ("word-spacing" "normal" length percentage)
    ("word-wrap" "normal" "break-word")

    ;; CSS Transforms Module Level 1
    ;; (https://www.w3.org/TR/css3-2d-transforms/#property-index)
    ("backface-visibility" "visible" "hidden")
    ("perspective" "none" length)
    ("perspective-origin" "left" "center" "right" "top" "bottom"
     percentage length)
    ("transform" "none" transform-list)
    ("transform-origin" "left" "center" "right" "top" "bottom"
     percentage length)
    ("transform-style" "flat" "preserve-3d")

    ;; CSS Transitions
    ;; (https://www.w3.org/TR/css3-transitions/#property-index)
    ("transition" single-transition)
    ("transition-delay" time)
    ("transition-duration" time)
    ("transition-property" "none" single-transition-property "all")
    ("transition-timing-function" single-transition-timing-function)

    ;; CSS Will Change Module Level 1
    ;; (https://www.w3.org/TR/css-will-change-1/#property-index)
    ("will-change" "auto" animateable-feature)

    ;; CSS Writing Modes Level 3
    ;; (https://www.w3.org/TR/css-writing-modes-3/#property-index)
    ;; "glyph-orientation-vertical" is obsolete and left out.
    ("direction" "ltr" "rtl")
    ("text-combine-upright" "none" "all")
    ("text-orientation" "mixed" "upright" "sideways")
    ("unicode-bidi" "normal" "embed" "isolate" "bidi-override"
     "isolate-override" "plaintext")
    ("writing-mode" "horizontal-tb" "vertical-rl" "vertical-lr")

    ;; Filter Effects Module Level 1
    ;; (https://www.w3.org/TR/filter-effects/#property-index)
    ("color-interpolation-filters" "auto" "sRGB" "linearRGB")
    ("filter" "none" filter-function-list)
    ("flood-color" color)
    ("flood-opacity" number percentage)
    ("lighting-color" color)

    ;; Pointer Events
    ;; (https://www.w3.org/TR/pointerevents/#the-touch-action-css-property)
    ("touch-action" "auto" "none" "pan-x" "pan-y" "manipulation"))
  "Identifiers for properties and their possible values.
The CAR of each entry is the name of a property, while the CDR is
a list of possible values for that property.  String values in
the CDRs represent literal values, while symbols represent one of
the value classes found in `css-value-class-alist'.  If a symbol
is not found in `css-value-class-alist', it's interpreted as a
reference back to one of the properties in this list.  Some
symbols, such as `number' or `identifier', don't produce any
further value candidates, since that list would be infinite.")

(defconst css-property-ids
  (mapcar #'car css-property-alist)
  "Identifiers for properties.")

(defconst css--color-map
  '(("black" . "#000000")
    ("silver" . "#c0c0c0")
    ("gray" . "#808080")
    ("white" . "#ffffff")
    ("maroon" . "#800000")
    ("red" . "#ff0000")
    ("purple" . "#800080")
    ("fuchsia" . "#ff00ff")
    ("magenta" . "#ff00ff")
    ("green" . "#008000")
    ("lime" . "#00ff00")
    ("olive" . "#808000")
    ("yellow" . "#ffff00")
    ("navy" . "#000080")
    ("blue" . "#0000ff")
    ("teal" . "#008080")
    ("aqua" . "#00ffff")
    ("cyan" . "#00ffff")
    ("orange" . "#ffa500")
    ("aliceblue" . "#f0f8ff")
    ("antiquewhite" . "#faebd7")
    ("aquamarine" . "#7fffd4")
    ("azure" . "#f0ffff")
    ("beige" . "#f5f5dc")
    ("bisque" . "#ffe4c4")
    ("blanchedalmond" . "#ffebcd")
    ("blueviolet" . "#8a2be2")
    ("brown" . "#a52a2a")
    ("burlywood" . "#deb887")
    ("cadetblue" . "#5f9ea0")
    ("chartreuse" . "#7fff00")
    ("chocolate" . "#d2691e")
    ("coral" . "#ff7f50")
    ("cornflowerblue" . "#6495ed")
    ("cornsilk" . "#fff8dc")
    ("crimson" . "#dc143c")
    ("darkblue" . "#00008b")
    ("darkcyan" . "#008b8b")
    ("darkgoldenrod" . "#b8860b")
    ("darkgray" . "#a9a9a9")
    ("darkgreen" . "#006400")
    ("darkgrey" . "#a9a9a9")
    ("darkkhaki" . "#bdb76b")
    ("darkmagenta" . "#8b008b")
    ("darkolivegreen" . "#556b2f")
    ("darkorange" . "#ff8c00")
    ("darkorchid" . "#9932cc")
    ("darkred" . "#8b0000")
    ("darksalmon" . "#e9967a")
    ("darkseagreen" . "#8fbc8f")
    ("darkslateblue" . "#483d8b")
    ("darkslategray" . "#2f4f4f")
    ("darkslategrey" . "#2f4f4f")
    ("darkturquoise" . "#00ced1")
    ("darkviolet" . "#9400d3")
    ("deeppink" . "#ff1493")
    ("deepskyblue" . "#00bfff")
    ("dimgray" . "#696969")
    ("dimgrey" . "#696969")
    ("dodgerblue" . "#1e90ff")
    ("firebrick" . "#b22222")
    ("floralwhite" . "#fffaf0")
    ("forestgreen" . "#228b22")
    ("gainsboro" . "#dcdcdc")
    ("ghostwhite" . "#f8f8ff")
    ("gold" . "#ffd700")
    ("goldenrod" . "#daa520")
    ("greenyellow" . "#adff2f")
    ("grey" . "#808080")
    ("honeydew" . "#f0fff0")
    ("hotpink" . "#ff69b4")
    ("indianred" . "#cd5c5c")
    ("indigo" . "#4b0082")
    ("ivory" . "#fffff0")
    ("khaki" . "#f0e68c")
    ("lavender" . "#e6e6fa")
    ("lavenderblush" . "#fff0f5")
    ("lawngreen" . "#7cfc00")
    ("lemonchiffon" . "#fffacd")
    ("lightblue" . "#add8e6")
    ("lightcoral" . "#f08080")
    ("lightcyan" . "#e0ffff")
    ("lightgoldenrodyellow" . "#fafad2")
    ("lightgray" . "#d3d3d3")
    ("lightgreen" . "#90ee90")
    ("lightgrey" . "#d3d3d3")
    ("lightpink" . "#ffb6c1")
    ("lightsalmon" . "#ffa07a")
    ("lightseagreen" . "#20b2aa")
    ("lightskyblue" . "#87cefa")
    ("lightslategray" . "#778899")
    ("lightslategrey" . "#778899")
    ("lightsteelblue" . "#b0c4de")
    ("lightyellow" . "#ffffe0")
    ("limegreen" . "#32cd32")
    ("linen" . "#faf0e6")
    ("mediumaquamarine" . "#66cdaa")
    ("mediumblue" . "#0000cd")
    ("mediumorchid" . "#ba55d3")
    ("mediumpurple" . "#9370db")
    ("mediumseagreen" . "#3cb371")
    ("mediumslateblue" . "#7b68ee")
    ("mediumspringgreen" . "#00fa9a")
    ("mediumturquoise" . "#48d1cc")
    ("mediumvioletred" . "#c71585")
    ("midnightblue" . "#191970")
    ("mintcream" . "#f5fffa")
    ("mistyrose" . "#ffe4e1")
    ("moccasin" . "#ffe4b5")
    ("navajowhite" . "#ffdead")
    ("oldlace" . "#fdf5e6")
    ("olivedrab" . "#6b8e23")
    ("orangered" . "#ff4500")
    ("orchid" . "#da70d6")
    ("palegoldenrod" . "#eee8aa")
    ("palegreen" . "#98fb98")
    ("paleturquoise" . "#afeeee")
    ("palevioletred" . "#db7093")
    ("papayawhip" . "#ffefd5")
    ("peachpuff" . "#ffdab9")
    ("peru" . "#cd853f")
    ("pink" . "#ffc0cb")
    ("plum" . "#dda0dd")
    ("powderblue" . "#b0e0e6")
    ("rosybrown" . "#bc8f8f")
    ("royalblue" . "#4169e1")
    ("saddlebrown" . "#8b4513")
    ("salmon" . "#fa8072")
    ("sandybrown" . "#f4a460")
    ("seagreen" . "#2e8b57")
    ("seashell" . "#fff5ee")
    ("sienna" . "#a0522d")
    ("skyblue" . "#87ceeb")
    ("slateblue" . "#6a5acd")
    ("slategray" . "#708090")
    ("slategrey" . "#708090")
    ("snow" . "#fffafa")
    ("springgreen" . "#00ff7f")
    ("steelblue" . "#4682b4")
    ("tan" . "#d2b48c")
    ("thistle" . "#d8bfd8")
    ("tomato" . "#ff6347")
    ("turquoise" . "#40e0d0")
    ("violet" . "#ee82ee")
    ("wheat" . "#f5deb3")
    ("whitesmoke" . "#f5f5f5")
    ("yellowgreen" . "#9acd32")
    ("rebeccapurple" . "#663399"))
  "Map CSS named colors to their hex RGB value.")

(defconst css-value-class-alist
  `((absolute-size
     "xx-small" "x-small" "small" "medium" "large" "x-large"
     "xx-large")
    (alphavalue number)
    (angle "calc()")
    (animateable-feature "scroll-position" "contents" custom-ident)
    (attachment "scroll" "fixed" "local")
    (auto-repeat "repeat()")
    (auto-track-list line-names fixed-size fixed-repeat auto-repeat)
    (basic-shape "inset()" "circle()" "ellipse()" "polygon()")
    (bg-image image "none")
    (bg-layer bg-image position repeat-style attachment box)
    (bg-size length percentage "auto" "cover" "contain")
    (box "border-box" "padding-box" "content-box")
    (clip-source uri)
    (color
     "rgb()" "rgba()" "hsl()" "hsla()" named-color "transparent"
     "currentColor")
    (common-lig-values "common-ligatures" "no-common-ligatures")
    (compositing-operator "add" "subtract" "intersect" "exclude")
    (contextual-alt-values "contextual" "no-contextual")
    (counter "counter()" "counters()")
    (discretionary-lig-values
     "discretionary-ligatures" "no-discretionary-ligatures")
    (east-asian-variant-values
     "jis78" "jis83" "jis90" "jis04" "simplified" "traditional")
    (east-asian-width-values "full-width" "proportional-width")
    (explicit-track-list line-names track-size)
    (family-name "Courier" "Helvetica" "Times")
    (feature-tag-value string integer "on" "off")
    (filter-function
     "blur()" "brightness()" "contrast()" "drop-shadow()"
     "grayscale()" "hue-rotate()" "invert()" "opacity()" "sepia()"
     "saturate()")
    (filter-function-list filter-function uri)
    (final-bg-layer
     bg-image position repeat-style attachment box color)
    (fixed-breadth length-percentage)
    (fixed-repeat "repeat()")
    (fixed-size fixed-breadth "minmax()")
    (font-variant-css21 "normal" "small-caps")
    (frequency "calc()")
    (generic-family
     "serif" "sans-serif" "cursive" "fantasy" "monospace")
    (generic-voice "male" "female" "child")
    (geometry-box shape-box "fill-box" "stroke-box" "view-box")
    (gradient
     linear-gradient radial-gradient repeating-linear-gradient
     repeating-radial-gradient)
    (grid-line "auto" custom-ident integer "span")
    (historical-lig-values
     "historical-ligatures" "no-historical-ligatures")
    (image uri image-list element-reference gradient)
    (image-list "image()")
    (inflexible-breadth length-percentage "min-content" "max-content"
                        "auto")
    (integer "calc()")
    (length "calc()" number)
    (line-height "normal" number length percentage)
    (line-names custom-ident)
    (line-style
     "none" "hidden" "dotted" "dashed" "solid" "double" "groove"
     "ridge" "inset" "outset")
    (line-width length "thin" "medium" "thick")
    (linear-gradient "linear-gradient()")
    (margin-width "auto" length percentage)
    (mask-layer
     mask-reference masking-mode position bg-size repeat-style
     geometry-box "no-clip" compositing-operator)
    (mask-reference "none" image mask-source)
    (mask-source uri)
    (masking-mode "alpha" "luminance" "auto")
    (named-color . ,(mapcar #'car css--color-map))
    (number "calc()")
    (numeric-figure-values "lining-nums" "oldstyle-nums")
    (numeric-fraction-values "diagonal-fractions" "stacked-fractions")
    (numeric-spacing-values "proportional-nums" "tabular-nums")
    (padding-width length percentage)
    (position
     "left" "center" "right" "top" "bottom" percentage length)
    (baseline-position "left" "right" "baseline")
    (content-distribution
     "space-between" "space-around" "space-evenly" "stretch")
    (overflow-position "unsafe" "safe")
    (content-position "center" "start" "end" "flex-start" "flex-end")
    (self-position
     "center" "start" "end" "self-start" "self-end" "flex-start" "flex-end")
    (radial-gradient "radial-gradient()")
    (relative-size "larger" "smaller")
    (repeat-style
     "repeat-x" "repeat-y" "repeat" "space" "round" "no-repeat")
    (repeating-linear-gradient "repeating-linear-gradient()")
    (repeating-radial-gradient "repeating-radial-gradient()")
    (shadow "inset" length color)
    (shape-box box "margin-box")
    (single-animation-direction
     "normal" "reverse" "alternate" "alternate-reverse")
    (single-animation-fill-mode "none" "forwards" "backwards" "both")
    (single-animation-iteration-count "infinite" number)
    (single-animation-name "none" identifier)
    (single-animation-play-state "running" "paused")
    (single-timing-function single-transition-timing-function)
    (single-transition
     "none" single-transition-property time
     single-transition-timing-function)
    (single-transition-property "all" identifier)
    (single-transition-timing-function
     "ease" "linear" "ease-in" "ease-out" "ease-in-out" "step-start"
     "step-end" "steps()" "cubic-bezier()")
    (specific-voice identifier)
    (target-name string)
    (time "calc()")
    (track-breadth length-percentage flex "min-content" "max-content"
                   "auto")
    (track-list line-names track-size track-repeat)
    (track-repeat "repeat()")
    (track-size track-breadth "minmax()" "fit-content()")
    (transform-list
     "matrix()" "translate()" "translateX()" "translateY()" "scale()"
     "scaleX()" "scaleY()" "rotate()" "skew()" "skewX()" "skewY()"
     "matrix3d()" "translate3d()" "translateZ()" "scale3d()"
     "scaleZ()" "rotate3d()" "rotateX()" "rotateY()" "rotateZ()"
     "perspective()")
    (uri "url()")
    (width length percentage "auto")
    (x number)
    (y number))
  "Property value classes and their values.
The format is similar to that of `css-property-alist', except
that the CARs aren't actual CSS properties, but rather a name for
a class of values, and that symbols in the CDRs always refer to
other entries in this list, not to properties.

The following classes have been left out above because they
cannot be completed sensibly: `custom-ident',
`element-reference', `flex', `id', `identifier',
`length-percentage', `percentage', and `string'.")

(defcustom css-electric-keys '(?\} ?\;) ;; '()
  "Self inserting keys which should trigger re-indentation."
  :version "22.2"
  :type '(repeat character)
  :group 'css)

(defvar css-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; C-style comments.
    (modify-syntax-entry ?/ ". 14" st)
    (modify-syntax-entry ?* ". 23b" st)
    ;; Strings.
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?\' "\"" st)
    ;; Blocks.
    (modify-syntax-entry ?\{ "(}" st)
    (modify-syntax-entry ?\} "){" st)
    ;; Args in url(...) thingies and other "function calls".
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\) ")(" st)
    ;; To match attributes in selectors.
    (modify-syntax-entry ?\[ "(]" st)
    (modify-syntax-entry ?\] ")[" st)
    ;; Special chars that sometimes come at the beginning of words.
    ;; We'll treat them as symbol constituents.
    (modify-syntax-entry ?@ "_" st)
    (modify-syntax-entry ?# "_" st)
    (modify-syntax-entry ?. "_" st)
    ;; Distinction between words and symbols.
    (modify-syntax-entry ?- "_" st)

    (modify-syntax-entry ?! "." st)
    (modify-syntax-entry ?$ "." st)
    (modify-syntax-entry ?% "." st)
    (modify-syntax-entry ?& "." st)
    (modify-syntax-entry ?+ "." st)
    (modify-syntax-entry ?, "." st)
    (modify-syntax-entry ?< "." st)
    (modify-syntax-entry ?> "." st)
    (modify-syntax-entry ?= "." st)
    (modify-syntax-entry ?? "." st)
    st))

(defvar css-mode--menu
  '("CSS"
    :help "CSS-specific features"
    ["Reformat block" fill-paragraph
     :help "Reformat declaration block or fill comment at point"]
    ["Cycle color format" css-cycle-color-format
     :help "Cycle color at point between different formats"]
    "-"
    ["Describe symbol" css-lookup-symbol
     :help "Display documentation for a CSS symbol"]
    ["Complete symbol" completion-at-point
     :help "Complete symbol before point"])
    "Menu bar for `css-mode'")

(defvar-keymap css-mode-map
  :doc "Keymap used in `css-mode'."
  "<remap> <info-lookup-symbol>" #'css-lookup-symbol
  ;; `info-complete-symbol' is not used.
  "<remap> <complete-symbol>" #'completion-at-point
  "C-c C-f" #'css-cycle-color-format
  :menu
  css-mode--menu)

(eval-and-compile
  (defconst css--uri-re
    (concat
     "url\\((\\)[[:space:]]*\\(?:\\\\.\\|[^()[:space:]\n'\"]\\)+"
     "[[:space:]]*\\()\\)")))

(defconst css-syntax-propertize-function
  (syntax-propertize-rules
   (css--uri-re (1 "|") (2 "|"))))

(defconst css-escapes-re
  "\\\\\\(?:[^\000-\037\177]\\|[[:xdigit:]]+[ \n\t\r\f]?\\)")
(defconst css-nmchar-re (concat "\\(?:[-_[:alnum:]]\\|" css-escapes-re "\\)"))
(defconst css-nmstart-re (concat "\\(?:[[:alpha:]]\\|" css-escapes-re "\\)"))
(defconst css-ident-re ;; (concat css-nmstart-re css-nmchar-re "*")
  ;; Apparently, "at rules" names can start with a dash, e.g. @-moz-keyframes.
  (concat css-nmchar-re "+"))
(defconst css-proprietary-nmstart-re ;; Vendor-specific properties.
  (concat "[-_]" (regexp-opt '("ms" "moz" "o" "khtml" "webkit")) "-"))
(defconst css-name-re (concat css-nmchar-re "+"))

(defconst scss--hash-re "#\\(?:{[$-_[:alnum:]]+}\\|[[:alnum:]]+\\)")

(defface css-selector '((t :inherit font-lock-function-name-face))
  "Face to use for selectors."
  :group 'css)
(defface css-property '((t :inherit font-lock-keyword-face))
  "Face to use for properties."
  :group 'css)
(defface css-proprietary-property '((t :inherit (css-property italic)))
  "Face to use for vendor-specific properties.")

(defun css--selector-regexp (sassy)
  (concat
   "\\(?:"
   (if (not sassy)
       "[-_%*#.>[:alnum:]]+"
     ;; Same as for non-sassy except we do want to allow { and }
     ;; chars in selectors in the case of #{$foo}
     ;; variable interpolation!
     (concat "\\(?:[-_%*#.>&+~[:alnum:]]*" scss--hash-re
             "\\|[-_%*#.>&+~[:alnum:]]+\\)"))
   ;; Even though pseudo-elements should be prefixed by ::, a
   ;; single colon is accepted for backward compatibility.
   "\\(?:\\(:" (regexp-opt (append css-pseudo-class-ids
                                   css-pseudo-element-ids)
                           t)
   "\\|::" (regexp-opt css-pseudo-element-ids t) "\\)\\)?"
   ;; Braces after selectors.
   "\\(?:\\[[^]\n]+\\]\\)?"
   ;; Parentheses after selectors.
   "\\(?:([^)]+)\\)?"
   ;; Main bit over.  But perhaps just [target]?
   "\\|\\[[^]\n]+\\]"
   ;; :root, ::marker and the like.
   "\\|::?[[:alnum:]]+\\(?:([^)]+)\\)?"
   "\\)"))

(defun css--font-lock-keywords (&optional sassy)
  `((,(concat "!\\s-*" (regexp-opt css--bang-ids))
     (0 font-lock-builtin-face))
    ;; Atrules keywords.  IDs not in css-at-ids are valid (ignored).
    ;; In fact the regexp should probably be
    ;; (,(concat "\\(@" css-ident-re "\\)\\([ \t\n][^;{]*\\)[;{]")
    ;;  (1 font-lock-builtin-face))
    ;; Since "An at-rule consists of everything up to and including the next
    ;; semicolon (;) or the next block, whichever comes first."
    (,(concat "@" css-ident-re) (0 font-lock-builtin-face))
    ;; Selectors.
    ;; Allow plain ":root" as a selector.
    ("^[ \t]*\\(:root\\)[\n \t]*{" (1 'css-selector keep))
    ;; FIXME: attribute selectors don't work well because they may contain
    ;; strings which have already been highlighted as f-l-string-face and
    ;; thus prevent this highlighting from being applied (actually now that
    ;; I use `keep' this should work better).  But really the part of the
    ;; selector between [...] should simply not be highlighted.
    (,(concat
       "^[ \t]*\\("
       ;; We have at least one selector.
       (css--selector-regexp sassy)
       ;; And then possibly more.
       "\\(?:"
       ;; Separators between selectors.
       "[ \n\t,+~>]+"
       (css--selector-regexp sassy)
       "\\)*"
       ;; And then a brace.
       "\\)[ \n\t]*{")
     (1 'css-selector keep))
    ;; In the above rule, we allow the open-brace to be on some subsequent
    ;; line.  This will only work if we properly mark the intervening text
    ;; as being part of a multiline element (and even then, this only
    ;; ensures proper refontification, but not proper discovery).
    ("^[ \t]*{" (0 (save-excursion
                     (goto-char (match-beginning 0))
                     (skip-chars-backward " \n\t")
                     (put-text-property (point) (match-end 0)
                                        'font-lock-multiline t)
                     ;; No face.
                     nil)))
    ;; Variables.
    (,(concat (rx symbol-start) "--" css-ident-re) (0 font-lock-variable-name-face))
    ;; Properties.  Again, we don't limit ourselves to css-property-ids.
    (,(concat "\\(?:[{;]\\|^\\)[ \t]*\\("
              "\\(?:\\(" css-proprietary-nmstart-re "\\)\\|"
              css-nmstart-re "\\)" css-nmchar-re "*"
              "\\)\\s-*:")
     (1 (if (match-end 2) 'css-proprietary-property 'css-property)))
    ;; Make sure the parens in a url(...) expression receive the
    ;; default face. This is done because the parens may sometimes
    ;; receive generic string delimiter syntax (see
    ;; `css-syntax-propertize-function').
    (,css--uri-re
     (1 'default t) (2 'default t))))

(defvar css-font-lock-keywords (css--font-lock-keywords))

(defvar css-font-lock-defaults
  '(css-font-lock-keywords nil t))

(defconst css--number-regexp
  "\\(\\(?:[0-9]*\\.[0-9]+\\(?:[eE][0-9]+\\)?\\)\\|[0-9]+\\)"
  "A regular expression matching a CSS number.")

(defconst css--percent-regexp "\\([0-9]+\\)%"
  "A regular expression matching a CSS percentage.")

(defconst css--number-or-percent-regexp
  (concat "\\(?:" css--percent-regexp "\\)\\|\\(?:" css--number-regexp "\\)")
  "A regular expression matching a CSS number or a CSS percentage.")

(defconst css--angle-regexp
  (concat css--number-regexp
	  (regexp-opt '("deg" "grad" "rad" "turn") t)
	  "?")
  "A regular expression matching a CSS angle.")

(defun css--color-skip-blanks ()
  "Skip blanks and comments."
  (while (forward-comment 1)))

(cl-defun css--rgb-color (&optional include-alpha)
  "Parse a CSS rgb() or rgba() color.
Point should be just after the open paren.
Returns a hex RGB color, or nil if the color could not be recognized.
This recognizes CSS-color-4 extensions.
When INCLUDE-ALPHA is non-nil, the alpha component is included in
the returned hex string."
  (let ((result '())
	(iter 0))
    (while (< iter 4)
      (css--color-skip-blanks)
      (unless (looking-at css--number-or-percent-regexp)
	(cl-return-from css--rgb-color nil))
      (let* ((is-percent (match-beginning 1))
	     (str (match-string (if is-percent 1 2)))
	     (number (string-to-number str)))
	(if is-percent
	    (setq number (* 255 (/ number 100.0)))
          (when (and include-alpha (= iter 3))
            (setq number (* number 255))))
        (push (min (max 0 (round number)) 255) result)
	(goto-char (match-end 0))
	(css--color-skip-blanks)
        (incf iter)
	;; Accept a superset of the CSS syntax since I'm feeling lazy.
	(when (and (= (skip-chars-forward ",/") 0)
		   (= iter 3))
	  ;; The alpha is optional.
          (incf iter))
	(css--color-skip-blanks)))
    (when (looking-at ")")
      (forward-char)
      (apply #'format
             (if (and include-alpha (= (length result) 4))
                 "#%02x%02x%02x%02x"
               "#%02x%02x%02x")
             (nreverse result)))))

(cl-defun css--hsl-color ()
  "Parse a CSS hsl() or hsla() color.
Point should be just after the open paren.
Returns a hex RGB color, or nil if the color could not be recognized.
This recognizes CSS-color-4 extensions."
  (let ((result '()))
    ;; First parse the hue.
    (css--color-skip-blanks)
    (unless (looking-at css--angle-regexp)
      (cl-return-from css--hsl-color nil))
    (let ((hue (string-to-number (match-string 1)))
	  (unit (match-string 2)))
      (goto-char (match-end 0))
      ;; Note that here "turn" is just passed through.
      (cond
       ((or (not unit) (equal unit "deg"))
	;; Degrees.
	(setq hue (/ hue 360.0)))
       ((equal unit "grad")
	(setq hue (/ hue 400.0)))
       ((equal unit "rad")
	(setq hue (/ hue (* 2 float-pi)))))
      (push (mod hue 1.0) result))
    (dotimes (_ 2)
      (skip-chars-forward ",")
      (css--color-skip-blanks)
      (unless (looking-at css--percent-regexp)
        (cl-return-from css--hsl-color nil))
      (let ((number (string-to-number (match-string 1))))
        (setq number (/ number 100.0))
        (push (min (max number 0.0) 1.0) result)
        (goto-char (match-end 0))
        (css--color-skip-blanks)))
    (css--color-skip-blanks)
    ;; Accept a superset of the CSS syntax since I'm feeling lazy.
    (when (> (skip-chars-forward ",/") 0)
      (css--color-skip-blanks)
      (unless (looking-at css--number-or-percent-regexp)
        (cl-return-from css--hsl-color nil))
      (goto-char (match-end 0))
      (css--color-skip-blanks))
    (when (looking-at ")")
      (forward-char)
      (apply #'color-rgb-to-hex
	     (nconc (apply #'color-hsl-to-rgb (nreverse result)) '(2))))))

(defconst css--colors-regexp
  (concat
   ;; Named colors.
   (regexp-opt (mapcar #'car css--color-map) 'symbols)
   "\\|"
   ;; Short hex.  css-color-4 adds alpha.
   "\\(#[[:xdigit:]]\\{3,4\\}\\b\\)"
   "\\|"
   ;; Long hex.  css-color-4 adds alpha.
   "\\(#\\(?:[[:xdigit:]][[:xdigit:]]\\)\\{3,4\\}\\b\\)"
   "\\|"
   ;; RGB.
   "\\(\\_<rgba?(\\)"
   "\\|"
   ;; HSL.
   "\\(\\_<hsla?(\\)")
  "A regular expression that matches the start of a CSS color.")

(defun css--hex-color (str)
  "Convert a CSS hex color to an Emacs hex color.
STR is the incoming CSS hex color.
This function simply drops any transparency."
  ;; Either #RGB or #RRGGBB, drop the "A" or "AA".
  (substring str 0 (if (> (length str) 5) 7 4)))

(defun css--hex-alpha (hex)
  "Return the alpha component of CSS color HEX.
HEX can either be in the #RGBA or #RRGGBBAA format.  Return nil
if the color doesn't have an alpha component."
  (cl-case (length hex)
    (5 (string (elt hex 4)))
    (9 (substring hex 7 9))))

(defun css--named-color (start-point str)
  "Check whether STR, seen at point, is CSS named color.
Returns STR if it is a valid color.  Special care is taken
to exclude some SCSS constructs."
  (when-let* ((color (assoc str css--color-map)))
    (save-excursion
      (goto-char start-point)
      (forward-comment (- (point)))
      (skip-chars-backward "@[:alpha:]")
      (unless (looking-at-p "@\\(mixin\\|include\\)")
        (cdr color)))))

(defun css--compute-color (start-point match)
  "Return the CSS color at point.
Point should be just after the start of a CSS color, as recognized
by `css--colors-regexp'.  START-POINT is the start of the color,
and MATCH is the string matched by the regexp.

This function will either return the color, as a hex RGB string;
or nil if no color could be recognized.  When this function
returns, point will be at the end of the recognized color."
  (cond
   ((eq (aref match 0) ?#)
    (css--hex-color match))
   ((member match '("rgb(" "rgba("))
    (css--rgb-color))
   ((member match '("hsl(" "hsla("))
    (css--hsl-color))
   ;; Evaluate to the color if the name is found.
   ((css--named-color start-point match))))

(defcustom css-fontify-colors t
  "Whether CSS colors should be fontified using the color as the background.
When non-nil, a text representing CSS color will be fontified
such that its background is the color itself.  E.g., #ff0000 will
be fontified with a red background."
  :version "26.1"
  :group 'css
  :type 'boolean
  :safe 'booleanp)

(defun css--fontify-region (start end &optional loudly)
  "Fontify a CSS buffer between START and END.
START and END are buffer positions."
  (let ((extended-region (font-lock-default-fontify-region start end loudly)))
    (when css-fontify-colors
      (when (and (consp extended-region)
		 (eq (car extended-region) 'jit-lock-bounds))
	(setq start (cadr extended-region))
	(setq end (cddr extended-region)))
      (save-excursion
	(let ((case-fold-search t))
	  (goto-char start)
	  (while (re-search-forward css--colors-regexp end t)
	    ;; Skip comments and strings.
	    (unless (nth 8 (syntax-ppss))
	      (let* ((start (match-beginning 0))
                     (color (css--compute-color start (match-string 0))))
		(when color
		  (with-silent-modifications
		    ;; Use the color as the background, to make it more
		    ;; clear.  Use a contrasting color as the foreground,
		    ;; to make it readable.  Finally, have a small box
		    ;; using the existing foreground color, to make sure
		    ;; it stands out a bit from any other text; in
		    ;; particular this is nice when the color matches the
		    ;; buffer's background color.
		    (add-text-properties
		     start (point)
		     (list 'face (list :background color
				       :foreground (readable-foreground-color
                                                    color)
				       :box '(:line-width -1))))))))))))
    extended-region))

(defcustom css-indent-offset 4
  "Basic size of one indentation step."
  :version "22.2"
  :type 'integer
  :safe 'integerp)

(defconst css-smie-grammar
  (smie-prec2->grammar
   (smie-precs->prec2
    '((assoc ";")
      ;; Colons that belong to a CSS property.  These get a higher
      ;; precedence than other colons, such as colons in selectors,
      ;; which are represented by a plain ":" token.
      (left ":-property")
      (assoc ",")
      (assoc ":")))))

(defun css--colon-inside-selector-p ()
  "Return t if point looks to be inside a CSS selector.
This function is intended to be good enough to help SMIE during
tokenization, but should not be regarded as a reliable function
for determining whether point is within a selector."
  (save-excursion
    (re-search-forward "[{};]" nil t)
    (eq (char-before) ?\{)))

(defun css--colon-inside-funcall ()
  "Return t if point is inside a function call."
  (when-let* ((opening-paren-pos (nth 1 (syntax-ppss))))
    (save-excursion
      (goto-char opening-paren-pos)
      (eq (char-after) ?\())))

(defun css-smie--forward-token ()
  (cond
   ((and (eq (char-before) ?\})
         (scss-smie--not-interpolation-p)
         ;; FIXME: If the next char is not whitespace, what should we do?
         (or (memq (char-after) '(?\s ?\t ?\n))
             (looking-at comment-start-skip)))
    (if (memq (char-after) '(?\s ?\t ?\n))
        (forward-char 1) (forward-comment 1))
    ";")
   ((progn (forward-comment (point-max))
           (looking-at "[;,:]"))
    (forward-char 1)
    (if (equal (match-string 0) ":")
        (if (or (css--colon-inside-selector-p)
                (css--colon-inside-funcall))
            ":"
          ":-property")
      (match-string 0)))
   (t (smie-default-forward-token))))

(defun css-smie--backward-token ()
  (let ((pos (point)))
    (forward-comment (- (point)))
    (cond
     ;; FIXME: If the next char is not whitespace, what should we do?
     ((and (eq (char-before) ?\}) (scss-smie--not-interpolation-p)
           (> pos (point))) ";")
     ((memq (char-before) '(?\; ?\, ?\:))
      (forward-char -1)
      (if (eq (char-after) ?\:)
          (if (or (css--colon-inside-selector-p)
                  (css--colon-inside-funcall))
              ":"
            ":-property")
        (string (char-after))))
     (t (smie-default-backward-token)))))

(defun css-smie-rules (kind token)
  (pcase (cons kind token)
    ('(:elem . basic) css-indent-offset)
    ('(:elem . arg) 0)
    ;; "" stands for BOB (bug#15467).
    (`(:list-intro . ,(or ";" "" ":-property")) t)
    ('(:before . "{")
     (when (or (smie-rule-hanging-p) (smie-rule-bolp))
       (smie-backward-sexp ";")
       (unless (eq (char-after) ?\{)
         (smie-indent-virtual))))
    ('(:before . "(")
     (cond
      ((smie-rule-hanging-p) (smie-rule-parent 0))
      ((not (smie-rule-bolp)) 0)))
    ('(:after . ":-property")
     (when (smie-rule-hanging-p)
       css-indent-offset))))

;;; Tree-sitter

(add-to-list
 'treesit-language-source-alist
 '(css "https://github.com/tree-sitter/tree-sitter-css"
       :commit "6a442a3cf461b0ce275339e5afa178693484c927")
 t)

(defvar css-ts-mode-map (copy-keymap css-mode-map)
  "Keymap used in `css-ts-mode'.")

(defvar css--treesit-indent-rules
  '((css
     ((node-is "}") parent-bol 0)
     ((node-is ")") parent-bol 0)
     ((node-is "]") parent-bol 0)

     ((parent-is "block") parent-bol css-indent-offset)
     ((parent-is "arguments") parent-bol css-indent-offset)
     ((match nil "declaration" nil 0 3) parent-bol css-indent-offset)
     ((match nil "declaration" nil 3) (nth-sibling 2) 0)))
  "Tree-sitter indentation rules for `css-ts-mode'.")

(defvar css--treesit-settings
  (treesit-font-lock-rules
   :feature 'comment
   :language 'css
   '((comment) @font-lock-comment-face)

   :feature 'string
   :language 'css
   '((string_value) @font-lock-string-face)

   :feature 'keyword
   :language 'css
   '(["@media"
      "@import"
      "@charset"
      "@namespace"
      "@keyframes"
      "@supports"] @font-lock-builtin-face
      (at_keyword) @font-lock-builtin-face
      ["and"
       "or"
       "not"
       "only"
       "selector"] @font-lock-keyword-face)

   :feature 'variable
   :language 'css
   '((plain_value) @font-lock-variable-name-face)

   :language 'css
   :feature 'operator
   `(["=" "~=" "^=" "|=" "*=" "$="] @font-lock-operator-face)

   :feature 'selector
   :language 'css
   '((class_selector) @css-selector
     (child_selector) @css-selector
     (id_selector) @css-selector
     (tag_name) @css-selector
     (class_name) @css-selector
     (keyframe_block (integer_value) @css-selector) )

   :feature 'property
   :language 'css
   `((property_name) @css-property)

   :feature 'function
   :language 'css
   '((function_name) @font-lock-function-name-face)

   :feature 'constant
   :language 'css
   '((integer_value) @font-lock-number-face
     (float_value) @font-lock-number-face
     (unit) @font-lock-constant-face
     (important) @font-lock-builtin-face)

   :feature 'query
   :language 'css
   '((keyword_query) @font-lock-property-use-face
     (feature_name) @font-lock-property-use-face
     (keyframes_name) @font-lock-property-use-face)

   :feature 'bracket
   :language 'css
   '((["(" ")" "[" "]" "{" "}"]) @font-lock-bracket-face)

   :feature 'error
   :language 'css
   '((ERROR) @error))
  "Tree-sitter font-lock settings for `css-ts-mode'.")

(defun css--treesit-defun-name (node)
  "Return the defun name of NODE.
Return nil if there is no name or if NODE is not a defun node."
  (pcase (treesit-node-type node)
    ((or "rule_set" "keyframe_block")
     (treesit-node-text
      (treesit-node-child node 0) t))
    ((or "media_statement" "keyframes_statement" "supports_statement")
     (let ((block (treesit-node-child node -1)))
       (string-trim
        (buffer-substring-no-properties
         (treesit-node-start node)
         (treesit-node-start block)))))))

;;; Completion

(defun css--complete-property ()
  "Complete property at point."
  (save-excursion
    (let ((pos (point)))
      (skip-chars-backward "-[:alnum:]")
      (let ((start (point)))
        (skip-chars-backward " \t\r\n")
        (when (memq (char-before) '(?\{ ?\;))
          (list start pos css-property-ids))))))

(defun css--complete-bang-rule ()
  "Complete bang-rule at point."
  (save-excursion
    (let ((pos (point)))
      (skip-chars-backward "-[:alnum:]")
      (when (eq (char-before) ?\!)
        (list (point) pos css--bang-ids)))))

(defun css--complete-pseudo-element-or-class ()
  "Complete pseudo-element or pseudo-class at point."
  (save-excursion
    (let ((pos (point)))
      (skip-chars-backward "-[:alnum:]")
      (when (eq (char-before) ?\:)
        (let ((double-colon (eq (char-before (- (point) 1)) ?\:)))
          (list (- (point) (if double-colon 2 1))
                pos
                (nconc
                 (unless double-colon
                   (mapcar (lambda (id) (concat ":" id)) css-pseudo-class-ids))
                 (mapcar (lambda (id) (concat "::" id)) css-pseudo-element-ids))
                :company-kind (lambda (_) 'function)))))))

(defun css--complete-at-rule ()
  "Complete at-rule (statement beginning with `@') at point."
  (save-excursion
    (let ((pos (point)))
      (skip-chars-backward "-[:alnum:]")
      (when (eq (char-before) ?\@)
        (list (point) pos css--at-ids
              :company-kind (lambda (_) 'keyword))))))

(defvar css--property-value-cache
  (make-hash-table :test 'equal :size (length css-property-alist))
  "Cache of previously completed property values.")

(defun css--value-class-lookup (value-class)
  "Return a list of value completion candidates for VALUE-CLASS.
Completion candidates are looked up in `css-value-class-alist' by
the symbol VALUE-CLASS."
  (seq-uniq
   (seq-mapcat
    (lambda (value)
      (if (stringp value)
          (list value)
        (css--value-class-lookup value)))
    (cdr (assq value-class css-value-class-alist)))))

(defun css--property-values (property)
  "Return a list of value completion candidates for PROPERTY.
Completion candidates are looked up in `css-property-alist' by
the string PROPERTY."
  (or (gethash property css--property-value-cache)
      (let ((values
             (seq-uniq
              (seq-mapcat
               (lambda (value)
                 (if (stringp value)
                     (list value)
                   (or (css--value-class-lookup value)
                       (css--property-values (symbol-name value)))))
               (cdr (assoc property css-property-alist))))))
        (puthash property values css--property-value-cache))))

(defun css--complete-property-value ()
  "Complete property value at point."
  (let ((property (and (looking-back "\\([[:alnum:]-]+\\):[^/][^;]*"
                                     (or (ppss-innermost-start (syntax-ppss))
                                         (point))
                                     t)
                       (member (match-string-no-properties 1)
                               css-property-ids))))
    (when property
      (let ((end (point)))
        (save-excursion
          (skip-chars-backward "[:graph:]")
          (list (point) end
                (append '("inherit" "initial" "unset")
                        (css--property-values (car property)))
                :company-kind (lambda (_) 'value)))))))

(defvar css--html-tags (mapcar #'car html-tag-alist)
  "List of HTML tags.
Used to provide completion of HTML tags in selectors.")

(defvar-local css--nested-selectors-allowed nil
  "Non-nil if nested selectors are allowed in the current mode.")

(defvar css-class-list-function #'ignore
  "Called to provide completions of class names.
This can be bound by buffers that are able to suggest class name
completions, such as HTML mode buffers.")

(defvar css-id-list-function #'ignore
  "Called to provide completions of IDs.
This can be bound by buffers that are able to suggest ID
completions, such as HTML mode buffers.")

(defun css--foreign-completions (extractor)
  "Return a list of completions provided by other buffers.
EXTRACTOR should be the name of a function that may be defined in
one or more buffers.  In each of the buffers where EXTRACTOR is
defined, EXTRACTOR is called and the results are accumulated into
a list of completions."
  (delete-dups
   (seq-mapcat
    (lambda (buf)
      (with-current-buffer buf
        (funcall (symbol-value extractor))))
    (buffer-list))))

(defun css--complete-selector ()
  "Complete part of a CSS selector at point."
  (when (or (= (nth 0 (syntax-ppss)) 0) css--nested-selectors-allowed)
    (let ((end (point)))
      (save-excursion
        (skip-chars-backward "-[:alnum:]")
        (let ((start-char (char-before)))
          (list
           (point) end
           (completion-table-dynamic
            (lambda (_)
              (cond
               ((eq start-char ?.)
                (css--foreign-completions 'css-class-list-function))
               ((eq start-char ?#)
                (css--foreign-completions 'css-id-list-function))
               (t css--html-tags))))))))))

(defun css-completion-at-point ()
  "Complete current symbol at point.
Currently supports completion of CSS properties, property values,
pseudo-elements, pseudo-classes, at-rules, bang-rules, and HTML
tags, classes and IDs."
  (or (css--complete-bang-rule)
      (css--complete-property-value)
      (css--complete-pseudo-element-or-class)
      (css--complete-at-rule)
      (seq-let (prop-beg prop-end prop-table) (css--complete-property)
        (seq-let (sel-beg sel-end sel-table) (css--complete-selector)
          (when (or prop-table sel-table)
            ;; FIXME: If both prop-table and sel-table are set but
            ;; prop-beg/prop-end is different from sel-beg/sel-end
            ;; we have a problem!
            `(,@(if prop-table
                    (list prop-beg prop-end)
                  (list sel-beg sel-end))
              ,(completion-table-merge prop-table sel-table)
              :company-kind
              ,(lambda (s) (if (test-completion s prop-table) 'property 'keyword))
              :exit-function
              ,(lambda (string status)
                 (and (eq status 'finished)
                      (eolp)
                      prop-table
                      (test-completion string prop-table)
                      (not (and sel-table
                                (test-completion string sel-table)))
                      (progn (insert ": ;")
                             (forward-char -1))))))))))

(defun css--color-to-4-dpc (hex)
  "Convert the CSS color HEX to four digits per component.
CSS colors use one or two digits per component for RGB hex
values.  Convert the given color to four digits per component.

Note that this function handles CSS colors specifically, and
should not be mixed with those in color.el."
  (let ((six-digits (= (length hex) 7)))
    (apply
     #'concat
     `("#"
       ,@(seq-mapcat
          (apply-partially #'make-list (if six-digits 2 4))
          (seq-partition (seq-drop hex 1) (if six-digits 2 1)))))))

(defun css--format-hex (hex)
  "Format a CSS hex color by shortening it if possible."
  (let ((parts (seq-partition (seq-drop hex 1) 2)))
    (if (and (>= (length hex) 6)
             (seq-every-p (lambda (p) (eq (elt p 0) (elt p 1))) parts))
        (apply #'string
               (cons ?# (mapcar (lambda (p) (elt p 0)) parts)))
      hex)))

(defun css--named-color-to-hex ()
  "Convert named CSS color at point to hex format.
Return non-nil if a conversion was made.

Note that this function handles CSS colors specifically, and
should not be mixed with those in color.el."
  (save-excursion
    (unless (or (looking-at css--colors-regexp)
                (eq (char-before) ?#))
      (backward-word))
    (when (member (word-at-point) (mapcar #'car css--color-map))
      (looking-at css--colors-regexp)
      (let ((color (css--compute-color (point) (match-string 0))))
        (replace-match (css--format-hex color)))
      t)))

(defun css--format-rgba-alpha (alpha)
  "Return ALPHA component formatted for use in rgba()."
  (let ((a (string-to-number (format "%.2f" alpha))))
    (if (or (= a 0)
            (= a 1))
        (format "%d" a)
      (string-remove-suffix "0" (number-to-string a)))))

(defun css--hex-to-rgb ()
  "Convert CSS hex color at point to RGB format.
Return non-nil if a conversion was made.

Note that this function handles CSS colors specifically, and
should not be mixed with those in color.el."
  (save-excursion
    (unless (or (eq (char-after) ?#)
                (eq (char-before) ?\())
      (backward-sexp))
    (when-let* ((hex (when (looking-at css--colors-regexp)
                       (and (eq (elt (match-string 0) 0) ?#)
                            (match-string 0))))
                (rgb (css--hex-color hex)))
      (seq-let (r g b)
          (mapcar (lambda (x) (round (* x 255)))
                  (color-name-to-rgb (css--color-to-4-dpc rgb)))
        (replace-match
         (if-let* ((alpha (css--hex-alpha hex))
                   (a (css--format-rgba-alpha
                       (/ (string-to-number alpha 16)
                          (float (- (expt 16 (length alpha)) 1))))))
             (format "rgba(%d, %d, %d, %s)" r g b a)
           (format "rgb(%d, %d, %d)" r g b))
         t))
      t)))

(defun css--rgb-to-named-color-or-hex ()
  "Convert CSS RGB color at point to a named color or hex format.
Convert to a named color if the color at point has a name, else
convert to hex format.  Return non-nil if a conversion was made.

Note that this function handles CSS colors specifically, and
should not be mixed with those in color.el."
  (save-excursion
    (when-let* ((open-paren-pos (nth 1 (syntax-ppss))))
      (when (save-excursion
              (goto-char open-paren-pos)
              (looking-back "rgba?" (- (point) 4)))
        (goto-char (nth 1 (syntax-ppss)))))
    (when (eq (char-before) ?\))
      (backward-sexp))
    (skip-chars-backward "rgba")
    (when (looking-at css--colors-regexp)
      (let* ((start (match-end 0))
             (color (save-excursion
                      (goto-char start)
                      (css--rgb-color t))))
        (when color
          (kill-sexp)
          (kill-sexp)
          (let ((named-color (seq-find (lambda (x) (equal (cdr x) color))
                                       css--color-map)))
            (insert (if named-color
                        (car named-color)
                      (css--format-hex color))))
          t)))))

(defun css-cycle-color-format ()
  "Cycle the color at point between different CSS color formats.
Supported formats are by name (if possible), hexadecimal, and
rgb()/rgba()."
  (interactive)
  (or (css--named-color-to-hex)
      (css--hex-to-rgb)
      (css--rgb-to-named-color-or-hex)
      (message "It doesn't look like a color at point")))

(defun css--join-nested-selectors (selectors)
  "Join a list of nested CSS selectors."
  (let ((processed '())
        (prev nil))
    (dolist (sel selectors)
      (cond
       ((seq-contains-p sel ?&)
        (setq sel (replace-regexp-in-string "&" prev sel))
        (pop processed))
       ;; Unless this is the first selector, separate this one and the
       ;; previous one by a space.
       (processed
        (push " " processed)))
      (push sel processed)
      (setq prev sel))
    (apply #'concat (nreverse processed))))

(defun css--prev-index-position ()
  (when (nth 7 (syntax-ppss))
    (goto-char (comment-beginning)))
  (forward-comment (- (point)))
  (when (search-backward "{" (point-min) t)
    (if (re-search-backward "}\\|;\\|{" (point-min) t)
        (forward-char)
      (goto-char (point-min)))
    (forward-comment (point-max))
    (save-excursion (re-search-forward "[^{;]*"))))

(defun css--extract-index-name ()
  (save-excursion
    (let ((res (list (match-string-no-properties 0))))
      (condition-case nil
          (while t
            (goto-char (nth 1 (syntax-ppss)))
            (if (re-search-backward "}\\|;\\|{" (point-min) t)
                (forward-char)
              (goto-char (point-min)))
            (forward-comment (point-max))
            (when (save-excursion
                    (re-search-forward "[^{;]*"))
              (push (match-string-no-properties 0) res)))
        (error
         (css--join-nested-selectors
          (mapcar
           (lambda (s)
             (string-trim
              (replace-regexp-in-string "[\n ]+" " " s)))
           res)))))))

(defvar css--treesit-thing-settings
  `((css (sexp
          (not (or (and named
                        ,(rx bos (or "stylesheet" "comment") eos))
                   (and anonymous
                        ,(rx (or "{" "}" "[" "]"
                                 "(" ")" ","))))))
         (list
          ,(rx bos (or "keyframe_block_list"
                       "block"
                       "pseudo_class_arguments"
                       "pseudo_class_with_selector_arguments"
                       "pseudo_class_nth_child_arguments"
                       "pseudo_element_arguments"
                       "feature_query"
                       "parenthesized_query"
                       "selector_query"
                       "parenthesized_value"
                       "grid_value"
                       "arguments")
               eos))
         (sentence
          ,(rx bos (or "import_statement"
                       "charset_statement"
                       "namespace_statement"
                       "postcss_statement"
                       "at_rule"
                       "declaration")
               eos))
         (text
          ,(rx bos (or "comment" "string_value") eos))))
  "Settings for `treesit-thing-settings'.")

(defvar css--treesit-font-lock-feature-list
  '((selector comment query keyword)
    (property constant string)
    (error variable function operator bracket))
  "Settings for `treesit-font-lock-feature-list'.")

(defvar css--treesit-simple-imenu-settings
  `(( nil ,(rx bos (or "rule_set"
                       "media_statement"
                       "keyframes_statement"
                       "keyframe_block"
                       "supports_statement")
               eos)
      nil nil))
  "Settings for `treesit-simple-imenu'.")

(defvar css-ts-mode--outline-predicate
  (rx bos (or "rule_set"
              "media_statement"
              "keyframes_statement"
              "keyframe_block"
              "supports_statement"
              "at_rule")
      eos)
  "Predicate for `treesit-outline-predicate'.")

(defvar css--treesit-defun-type-regexp
  (rx bos (or "rule_set" "keyframe_block") eos)
  "Settings for `treesit-defun-type-regexp'.")

(define-derived-mode css-base-mode prog-mode "CSS"
  "Generic mode to edit Cascading Style Sheets (CSS).

This is a generic major mode intended to be inherited by a
concrete implementation.  Currently there are two concrete
implementations: `css-mode' and `css-ts-mode'."
  (setq-local comment-start "/*")
  (setq-local comment-start-skip "/\\*+[ \t]*")
  (setq-local comment-end "*/")
  (setq-local comment-end-skip "[ \t]*\\*+/")
  (setq-local electric-indent-chars
              (append css-electric-keys electric-indent-chars))
  ;; The default "." creates ambiguity with class selectors.
  (setq-local imenu-space-replacement " "))

;;;###autoload
(define-derived-mode css-ts-mode css-base-mode "CSS"
  "Major mode to edit Cascading Style Sheets (CSS).
\\<css-ts-mode-map>

This mode provides syntax highlighting, indentation, completion,
and documentation lookup for CSS, based on the tree-sitter
library.

Use `\\[completion-at-point]' to complete CSS properties,
property values, pseudo-elements, pseudo-classes, at-rules,
bang-rules, and HTML tags, classes and IDs.  Completion
candidates for HTML class names and IDs are found by looking
through open HTML mode buffers.

Use `\\[info-lookup-symbol]' to look up documentation of CSS
properties, at-rules, pseudo-classes, and pseudo-elements on the
Mozilla Developer Network (MDN).

Use `\\[fill-paragraph]' to reformat CSS declaration blocks.  It
can also be used to fill comments.

\\{css-mode-map}"
  :syntax-table css-mode-syntax-table
  (when (treesit-ensure-installed 'css)
    ;; Borrowed from `css-mode'.
    (setq-local syntax-propertize-function
                css-syntax-propertize-function)
    (add-hook 'completion-at-point-functions
              #'css-completion-at-point nil 'local)
    (setq-local fill-paragraph-function #'css-fill-paragraph)
    (setq-local adaptive-fill-function #'css-adaptive-fill)
    ;; `css--fontify-region' first calls the default function, which
    ;; will call tree-sitter's function, then it fontifies colors.
    (setq-local font-lock-fontify-region-function #'css--fontify-region)

    ;; Tree-sitter specific setup.
    (setq treesit-primary-parser (treesit-parser-create 'css))
    (setq-local treesit-simple-indent-rules css--treesit-indent-rules)
    (setq-local treesit-defun-type-regexp css--treesit-defun-type-regexp)
    (setq-local treesit-defun-name-function #'css--treesit-defun-name)
    (setq-local treesit-font-lock-settings css--treesit-settings)
    (setq-local treesit-font-lock-feature-list css--treesit-font-lock-feature-list)
    (setq-local treesit-simple-imenu-settings css--treesit-simple-imenu-settings)
    (setq-local treesit-outline-predicate css-ts-mode--outline-predicate)
    (setq-local treesit-thing-settings css--treesit-thing-settings)

    (treesit-major-mode-setup)

    (add-to-list 'auto-mode-alist '("\\.css\\'" . css-ts-mode))))

(derived-mode-add-parents 'css-ts-mode '(css-mode))

;;;###autoload
(define-derived-mode css-mode css-base-mode "CSS"
  "Major mode to edit Cascading Style Sheets (CSS).
\\<css-mode-map>
This mode provides syntax highlighting, indentation, completion,
and documentation lookup for CSS.

Use `\\[completion-at-point]' to complete CSS properties, property values,
pseudo-elements, pseudo-classes, at-rules, bang-rules, and HTML
tags, classes and IDs.  Completion candidates for HTML class
names and IDs are found by looking through open HTML mode
buffers.

Use `\\[info-lookup-symbol]' to look up documentation of CSS properties, at-rules,
pseudo-classes, and pseudo-elements on the Mozilla Developer
Network (MDN).

Use `\\[fill-paragraph]' to reformat CSS declaration blocks.  It can also
be used to fill comments.

\\{css-mode-map}"
  (setq-local font-lock-defaults css-font-lock-defaults)
  (setq-local syntax-propertize-function
              css-syntax-propertize-function)
  (setq-local fill-paragraph-function #'css-fill-paragraph)
  (setq-local adaptive-fill-function #'css-adaptive-fill)
  (setq-local add-log-current-defun-function #'css-current-defun-name)
  (smie-setup css-smie-grammar #'css-smie-rules
              :forward-token #'css-smie--forward-token
              :backward-token #'css-smie--backward-token)
  (setq-local font-lock-fontify-region-function #'css--fontify-region)
  (add-hook 'completion-at-point-functions
            #'css-completion-at-point nil 'local)
  (setq-local imenu-prev-index-position-function
              #'css--prev-index-position)
  (setq-local imenu-extract-index-name-function
              #'css--extract-index-name))

(defvar comment-continue)

(defun css-fill-paragraph (&optional justify)
  (save-excursion
    ;; Fill succeeding comment when invoked right before a multi-line
    ;; comment.
    (when (save-excursion
            (beginning-of-line)
            (comment-search-forward (line-end-position) t))
      (goto-char (match-end 0)))
    (let ((ppss (syntax-ppss))
          (eol (line-end-position)))
      (cond
       ((and (nth 4 ppss)
             (save-excursion
               (goto-char (nth 8 ppss))
               (forward-comment 1)
               (prog1 (not (bolp))
                 (setq eol (point)))))
        ;; Filling inside a comment whose comment-end marker is not \n.
        ;; This code is meant to be generic, so that it works not only for
        ;; css-mode but for all modes.
        (save-restriction
          (narrow-to-region (nth 8 ppss) eol)
          (comment-normalize-vars)      ;Will define comment-continue.
          (let ((fill-paragraph-function nil)
                (paragraph-separate
                 (if (and comment-continue
                          (string-match "[^ \t]" comment-continue))
                     (concat "\\(?:[ \t]*\\(?:"
                             (regexp-quote comment-continue) "\\|"
                             comment-start-skip "\\|"
                             comment-end-skip "\\)\\)?"
                             "\\(?:" paragraph-separate "\\)")
                   paragraph-separate))
                (paragraph-start
                 (if (and comment-continue
                          (string-match "[^ \t]" comment-continue))
                     (concat "\\(?:[ \t]*" (regexp-quote comment-continue)
                             "\\)?\\(?:" paragraph-start "\\)")
                   paragraph-start)))
            (fill-paragraph justify)
            ;; Don't try filling again.
            t)))

       ((and (null (nth 8 ppss))
             (or (nth 1 ppss)
                 (and (ignore-errors
                        (down-list 1)
                        (when (<= (point) eol)
                          (setq ppss (syntax-ppss)))))))
        (goto-char (nth 1 ppss))
        (let ((end (save-excursion
                     (ignore-errors (forward-sexp 1) (copy-marker (point) t)))))
          (when end
            (while (re-search-forward "[{;}]" end t)
              (cond
               ;; This is a false positive inside a string or comment.
               ((nth 8 (syntax-ppss)) nil)
               ;; This is a false positive when encountering an
               ;; interpolated variable (bug#19751).
               ((eq (char-before (- (point) 1)) ?#) nil)
               ((eq (char-before) ?\})
                (save-excursion
                  (forward-char -1)
                  (skip-chars-backward " \t")
                  (when (and (not (bolp))
                             (scss-smie--not-interpolation-p))
                    (newline))))
               (t
                (while
                    (progn
                      (setq eol (line-end-position))
                      (and (forward-comment 1)
                           (> (point) eol)
                           ;; A multi-line comment should be on its own line.
                           (save-excursion (forward-comment -1)
                                           (when (< (point) eol)
                                             (newline)
                                             t)))))
                (if (< (point) eol) (newline)))))
            (goto-char (nth 1 ppss))
            (indent-region (line-beginning-position 2) end)
            ;; Don't use the default filling code.
            t)))))))

(defun css-adaptive-fill ()
  (when (looking-at "[ \t]*/\\*[ \t]*")
    (let ((str (match-string 0)))
      (and (string-match "/\\*" str)
           (replace-match " *" t t str)))))

(defun css-current-defun-name ()
  "Return the name of the CSS section at point, or nil."
  (save-excursion
    (when (css--prev-index-position)
      (css--extract-index-name))))

;;; SCSS mode

(defvar scss-mode-syntax-table
  (let ((st (make-syntax-table css-mode-syntax-table)))
    (modify-syntax-entry ?/ ". 124" st)
    (modify-syntax-entry ?\n ">" st)
    ;; Variable names are prefixed by $.
    (modify-syntax-entry ?$ "_" st)
    (modify-syntax-entry ?% "_" st)
    st))

(defun scss-font-lock-keywords ()
  (append `((,(concat "$" css-ident-re) (0 font-lock-variable-name-face)))
          (css--font-lock-keywords 'sassy)
          `((,(concat "@mixin[ \t]+\\(" css-ident-re "\\)[ \t]*(")
             (1 font-lock-function-name-face)))))

(defun scss-smie--not-interpolation-p ()
  (save-excursion
    (forward-char -1)
    (or (zerop (skip-chars-backward "-[:alnum:]"))
        (not (looking-back "#{\\$" (- (point) 3))))))

;;;###autoload (add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
;;;###autoload
(define-derived-mode scss-mode css-mode "SCSS"
  "Major mode to edit \"Sassy CSS\" files."
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local comment-continue " *")
  (setq-local comment-start-skip "/[*/]+[ \t]*")
  (setq-local comment-end-skip "[ \t]*\\(?:\n\\|\\*+/\\)")
  (setq-local css--at-ids (append css-at-ids scss-at-ids))
  (setq-local css--bang-ids (append css-bang-ids scss-bang-ids))
  (setq-local css--nested-selectors-allowed t)
  (setq-local font-lock-defaults
              (list (scss-font-lock-keywords) nil t)))



(defvar css--mdn-lookup-history nil)

(defcustom css-lookup-url-format
  "https://developer.mozilla.org/en-US/docs/Web/CSS/%s?raw&macros"
  "Format for a URL where CSS documentation can be found.
The format should include a single \"%s\" substitution.
The name of the CSS property, @-id, pseudo-class, or pseudo-element
to look up will be substituted there."
  :version "26.1"
  :type 'string
  :group 'css)

(defun css--mdn-after-render ()
  (setf header-line-format nil)
  (goto-char (point-min))
  (let ((window (get-buffer-window (current-buffer) 'visible)))
    (when window
      (when (re-search-forward "^\\(Summary\\|Syntax\\)" nil 'move)
        (beginning-of-line)
        (set-window-start window (point))))))

(defconst css--mdn-symbol-regexp
  (concat "\\("
	  ;; @-ids.
	  "\\(@" (regexp-opt css-at-ids) "\\)"
	  "\\|"
	  ;; ;; Known properties.
	  (regexp-opt css-property-ids t)
	  "\\|"
	  ;; Pseudo-classes.
	  "\\(:" (regexp-opt css-pseudo-class-ids) "\\)"
	  "\\|"
	  ;; Pseudo-elements with either one or two ":"s.
	  "\\(::?" (regexp-opt css-pseudo-element-ids) "\\)"
	  "\\)")
  "Regular expression to match the CSS symbol at point.")

(defconst css--mdn-property-regexp
  (concat "\\_<" (regexp-opt css-property-ids t) "\\s-*\\(?:\\=\\|:\\)")
  "Regular expression to match a CSS property.")

(defconst css--mdn-completion-list
  (nconc
   ;; @-ids.
   (mapcar (lambda (atrule) (concat "@" atrule)) css-at-ids)
   ;; Pseudo-classes.
   (mapcar (lambda (class) (concat ":" class)) css-pseudo-class-ids)
   ;; Pseudo-elements with either one or two ":"s.
   (mapcar (lambda (elt) (concat ":" elt)) css-pseudo-element-ids)
   (mapcar (lambda (elt) (concat "::" elt)) css-pseudo-element-ids)
   ;; Properties.
   css-property-ids)
  "List of all symbols available for lookup via MDN.")

(defun css--mdn-find-symbol ()
  "A helper for `css-lookup-symbol' that finds the symbol at point.
Returns the symbol, a string, or nil if none found."
  (save-excursion
    ;; Skip any whitespace between the word and point.
    (skip-chars-backward "- \t")
    ;; Skip backward over a word.
    (skip-chars-backward "-[:alnum:]")
    ;; Now skip ":" or "@" to see if it's a pseudo-element or at-id.
    (skip-chars-backward "@:")
    (if (looking-at css--mdn-symbol-regexp)
	(match-string-no-properties 0)
      (let ((bound (save-excursion
		     (beginning-of-line)
		     (point))))
	(when (re-search-backward css--mdn-property-regexp bound t)
	  (match-string-no-properties 1))))))

;;;###autoload
(defun css-lookup-symbol (symbol)
  "Display the CSS documentation for SYMBOL, as found on MDN.
When this command is used interactively, it picks a default
symbol based on the CSS text before point -- either an @-keyword,
a property name, a pseudo-class, or a pseudo-element, depending
on what is seen near point."
  (interactive
   (list
    (let* ((sym (css--mdn-find-symbol))
	   (enable-recursive-minibuffers t)
	   (value (completing-read (format-prompt "Describe CSS symbol" sym)
		                   css--mdn-completion-list nil nil nil
		                   'css--mdn-lookup-history sym)))
      (if (equal value "") sym value))))
  (when symbol
    ;; If we see a single-colon pseudo-element like ":after", turn it
    ;; into "::after".
    (when (and (eq (aref symbol 0) ?:)
	       (member (substring symbol 1) css-pseudo-element-ids))
      (setq symbol (concat ":" symbol)))
    (let ((url (format css-lookup-url-format symbol))
          (buffer (get-buffer-create "*MDN CSS*")))
      ;; Make sure to display the buffer before calling `eww', as that
      ;; calls `pop-to-buffer-same-window'.
      (switch-to-buffer-other-window buffer)
      (with-current-buffer buffer
        (eww-mode)
        (add-hook 'eww-after-render-hook #'css--mdn-after-render nil t)
        (eww url)))))

(provide 'css-mode)
;;; css-mode.el ends here
