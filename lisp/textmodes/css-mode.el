;;; css-mode.el --- Major mode to edit CSS files  -*- lexical-binding: t -*-

;; Copyright (C) 2006-2015 Free Software Foundation, Inc.

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Yet another CSS mode.

;;; Todo:

;; - electric ; and }
;; - filling code with auto-fill-mode
;; - attribute value completion
;; - fix font-lock errors with multi-line selectors

;;; Code:

(defgroup css nil
  "Cascading Style Sheets (CSS) editing mode."
  :group 'languages)

(defconst css-pseudo-class-ids
  '("active" "checked" "disabled" "empty" "enabled" "first"
    "first-child" "first-of-type" "focus" "hover" "indeterminate" "lang"
    "last-child" "last-of-type" "left" "link" "not" "nth-child"
    "nth-last-child" "nth-last-of-type" "nth-of-type" "only-child"
    "only-of-type" "right" "root" "target" "visited")
  "Identifiers for pseudo-classes.")

(defconst css-pseudo-element-ids
  '("after" "before" "first-letter" "first-line")
  "Identifiers for pseudo-elements.")

(defconst css-at-ids
  '("charset" "font-face" "import" "media" "namespace" "page")
  "Identifiers that appear in the form @foo.")

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

(defconst css-property-ids
  '(;; CSS 2.1 properties (http://www.w3.org/TR/CSS21/propidx.html).
    ;;
    ;; Properties duplicated by any of the CSS3 modules below have
    ;; been removed.
    "azimuth" "border-collapse" "border-spacing" "bottom"
    "caption-side" "clear" "clip" "content" "counter-increment"
    "counter-reset" "cue" "cue-after" "cue-before" "direction" "display"
    "elevation" "empty-cells" "float" "height" "left" "line-height"
    "list-style" "list-style-image" "list-style-position"
    "list-style-type" "margin" "margin-bottom" "margin-left"
    "margin-right" "margin-top" "max-height" "max-width" "min-height"
    "min-width" "orphans" "padding" "padding-bottom" "padding-left"
    "padding-right" "padding-top" "page-break-after"
    "page-break-before" "page-break-inside" "pause" "pause-after"
    "pause-before" "pitch" "pitch-range" "play-during" "position"
    "quotes" "richness" "right" "speak" "speak-header" "speak-numeral"
    "speak-punctuation" "speech-rate" "stress" "table-layout" "top"
    "unicode-bidi" "vertical-align" "visibility" "voice-family" "volume"
    "widows" "width" "z-index"

    ;; CSS Animations
    ;; (http://www.w3.org/TR/css3-animations/#property-index)
    "animation" "animation-delay" "animation-direction"
    "animation-duration" "animation-fill-mode"
    "animation-iteration-count" "animation-name"
    "animation-play-state" "animation-timing-function"

    ;; CSS Backgrounds and Borders Module Level 3
    ;; (http://www.w3.org/TR/css3-background/#property-index)
    "background" "background-attachment" "background-clip"
    "background-color" "background-image" "background-origin"
    "background-position" "background-repeat" "background-size"
    "border" "border-bottom" "border-bottom-color"
    "border-bottom-left-radius" "border-bottom-right-radius"
    "border-bottom-style" "border-bottom-width" "border-color"
    "border-image" "border-image-outset" "border-image-repeat"
    "border-image-slice" "border-image-source" "border-image-width"
    "border-left" "border-left-color" "border-left-style"
    "border-left-width" "border-radius" "border-right"
    "border-right-color" "border-right-style" "border-right-width"
    "border-style" "border-top" "border-top-color"
    "border-top-left-radius" "border-top-right-radius"
    "border-top-style" "border-top-width" "border-width" "box-shadow"

    ;; CSS Basic User Interface Module Level 3 (CSS3 UI)
    ;; (http://www.w3.org/TR/css3-ui/#property-index)
    "box-sizing" "caret-color" "cursor" "nav-down" "nav-left"
    "nav-right" "nav-up" "outline" "outline-color" "outline-offset"
    "outline-style" "outline-width" "resize" "text-overflow"

    ;; CSS Color Module Level 3
    ;; (http://www.w3.org/TR/css3-color/#property)
    "color" "opacity"

    ;; CSS Flexible Box Layout Module Level 1
    ;; (http://www.w3.org/TR/css-flexbox-1/#property-index)
    "align-content" "align-items" "align-self" "flex" "flex-basis"
    "flex-direction" "flex-flow" "flex-grow" "flex-shrink" "flex-wrap"
    "justify-content" "order"

    ;; CSS Fonts Module Level 3
    ;; (http://www.w3.org/TR/css3-fonts/#property-index)
    "font" "font-family" "font-feature-settings" "font-kerning"
    "font-language-override" "font-size" "font-size-adjust"
    "font-stretch" "font-style" "font-synthesis" "font-variant"
    "font-variant-alternates" "font-variant-caps"
    "font-variant-east-asian" "font-variant-ligatures"
    "font-variant-numeric" "font-variant-position" "font-weight"

    ;; CSS Overflow Module Level 3
    ;; (http://www.w3.org/TR/css-overflow-3/#property-index)
    "max-lines" "overflow" "overflow-x" "overflow-y"

    ;; CSS Text Decoration Module Level 3
    ;; (http://dev.w3.org/csswg/css-text-decor-3/#property-index)
    "text-decoration" "text-decoration-color" "text-decoration-line"
    "text-decoration-skip" "text-decoration-style" "text-emphasis"
    "text-emphasis-color" "text-emphasis-position" "text-emphasis-style"
    "text-shadow" "text-underline-position"

    ;; CSS Text Module Level 3
    ;; (http://www.w3.org/TR/css3-text/#property-index)
    "hanging-punctuation" "hyphens" "letter-spacing" "line-break"
    "overflow-wrap" "tab-size" "text-align" "text-align-last"
    "text-indent" "text-justify" "text-transform" "white-space"
    "word-break" "word-spacing" "word-wrap"

    ;; CSS Transforms Module Level 1
    ;; (http://www.w3.org/TR/css3-2d-transforms/#property-index)
    "backface-visibility" "perspective" "perspective-origin"
    "transform" "transform-origin" "transform-style"

    ;; CSS Transitions
    ;; (http://www.w3.org/TR/css3-transitions/#property-index)
    "transition" "transition-delay" "transition-duration"
    "transition-property" "transition-timing-function"

    ;; Filter Effects Module Level 1
    ;; (http://www.w3.org/TR/filter-effects/#property-index)
    "color-interpolation-filters" "filter" "flood-color"
    "flood-opacity" "lighting-color")
  "Identifiers for properties.")

(defcustom css-electric-keys '(?\} ?\;) ;; '()
  "Self inserting keys which should trigger re-indentation."
  :version "22.2"
  :type '(repeat character)
  :options '((?\} ?\;))
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
    (modify-syntax-entry ?@ "'" st)
    ;; (modify-syntax-entry ?: "'" st)
    (modify-syntax-entry ?# "'" st)
    ;; Distinction between words and symbols.
    (modify-syntax-entry ?- "_" st)
    st))

(eval-and-compile
  (defconst css--uri-re
    (concat
     "url\\((\\)[[:space:]]*\\(?:\\\\.\\|[^()[:space:]\n'\"]\\)+"
     "[[:space:]]*\\()\\)")))

(defconst css-syntax-propertize-function
  (syntax-propertize-rules
   (css--uri-re (1 "|") (2 "|"))))

(defconst css-escapes-re
  "\\\\\\(?:[^\000-\037\177]\\|[0-9a-fA-F]+[ \n\t\r\f]?\\)")
(defconst css-nmchar-re (concat "\\(?:[-[:alnum:]]\\|" css-escapes-re "\\)"))
(defconst css-nmstart-re (concat "\\(?:--\\)?\\(?:[[:alpha:]]\\|" css-escapes-re "\\)"))
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
(defface css-property '((t :inherit font-lock-variable-name-face))
  "Face to use for properties."
  :group 'css)
(defface css-proprietary-property '((t :inherit (css-property italic)))
  "Face to use for vendor-specific properties.")

(defun css--font-lock-keywords (&optional sassy)
  `((,(concat "!\\s-*"
              (regexp-opt (append (if sassy '("global"))
                                  '("important"))))
     (0 font-lock-builtin-face))
    ;; Atrules keywords.  IDs not in css-at-ids are valid (ignored).
    ;; In fact the regexp should probably be
    ;; (,(concat "\\(@" css-ident-re "\\)\\([ \t\n][^;{]*\\)[;{]")
    ;;  (1 font-lock-builtin-face))
    ;; Since "An at-rule consists of everything up to and including the next
    ;; semicolon (;) or the next block, whichever comes first."
    (,(concat "@" css-ident-re) (0 font-lock-builtin-face))
    ;; Selectors.
    ;; FIXME: attribute selectors don't work well because they may contain
    ;; strings which have already been highlighted as f-l-string-face and
    ;; thus prevent this highlighting from being applied (actually now that
    ;; I use `keep' this should work better).  But really the part of the
    ;; selector between [...] should simply not be highlighted.
    (,(concat
       "^[ \t]*\\("
       (if (not sassy)
           ;; We don't allow / as first char, so as not to
           ;; take a comment as the beginning of a selector.
           "[^@/:{} \t\n][^:{}]+"
         ;; Same as for non-sassy except we do want to allow { and }
         ;; chars in selectors in the case of #{$foo}
         ;; variable interpolation!
         (concat "\\(?:" scss--hash-re
                 "\\|[^@/:{} \t\n#]\\)"
                 "[^:{}#]*\\(?:" scss--hash-re "[^:{}#]*\\)*"))
       ;; Even though pseudo-elements should be prefixed by ::, a
       ;; single colon is accepted for backward compatibility.
       "\\(?:\\(:" (regexp-opt (append css-pseudo-class-ids
                                       css-pseudo-element-ids) t)
       "\\|\\::" (regexp-opt css-pseudo-element-ids t) "\\)"
       "\\(?:([^)]+)\\)?"
       (if (not sassy)
           "[^:{}\n]*"
         (concat "[^:{}\n#]*\\(?:" scss--hash-re "[^:{}\n#]*\\)*"))
       "\\)*"
       "\\)\\(?:\n[ \t]*\\)*{")
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

(defcustom css-indent-offset 4
  "Basic size of one indentation step."
  :version "22.2"
  :type 'integer)

(require 'smie)

(defconst css-smie-grammar
  (smie-prec2->grammar
   (smie-precs->prec2 '((assoc ";") (assoc ",") (left ":")))))

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
    (forward-char 1) (match-string 0))
   (t (smie-default-forward-token))))

(defun css-smie--backward-token ()
  (let ((pos (point)))
    (forward-comment (- (point)))
    (cond
     ;; FIXME: If the next char is not whitespace, what should we do?
     ((and (eq (char-before) ?\}) (scss-smie--not-interpolation-p)
           (> pos (point))) ";")
     ((memq (char-before) '(?\; ?\, ?\:))
      (forward-char -1) (string (char-after)))
     (t (smie-default-backward-token)))))

(defun css-smie-rules (kind token)
  (pcase (cons kind token)
    (`(:elem . basic) css-indent-offset)
    (`(:elem . arg) 0)
    (`(:list-intro . ,(or `";" `"")) t) ;"" stands for BOB (bug#15467).
    (`(:before . "{")
     (when (or (smie-rule-hanging-p) (smie-rule-bolp))
       (smie-backward-sexp ";")
       (smie-indent-virtual)))
    (`(:before . ,(or "{" "("))
     (if (smie-rule-hanging-p) (smie-rule-parent 0)))))

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

(defun css--complete-pseudo-element-or-class ()
  "Complete pseudo-element or pseudo-class at point."
  (save-excursion
    (let ((pos (point)))
      (skip-chars-backward "-[:alnum:]")
      (when (eq (char-before) ?\:)
        (list (point) pos
              (if (eq (char-before (- (point) 1)) ?\:)
                  css-pseudo-element-ids
                css-pseudo-class-ids))))))

(defun css--complete-at-rule ()
  "Complete at-rule (statement beginning with `@') at point."
  (save-excursion
    (let ((pos (point)))
      (skip-chars-backward "-[:alnum:]")
      (when (eq (char-before) ?\@)
        (list (point) pos css-at-ids)))))

(defun css-completion-at-point ()
  "Complete current symbol at point.
Currently supports completion of CSS properties, pseudo-elements,
pseudo-classes, and at-rules."
  (or (css--complete-property)
      (css--complete-pseudo-element-or-class)
      (css--complete-at-rule)))

;;;###autoload
(define-derived-mode css-mode prog-mode "CSS"
  "Major mode to edit Cascading Style Sheets."
  (setq-local font-lock-defaults css-font-lock-defaults)
  (setq-local comment-start "/*")
  (setq-local comment-start-skip "/\\*+[ \t]*")
  (setq-local comment-end "*/")
  (setq-local comment-end-skip "[ \t]*\\*+/")
  (setq-local syntax-propertize-function
              css-syntax-propertize-function)
  (setq-local fill-paragraph-function #'css-fill-paragraph)
  (setq-local adaptive-fill-function #'css-adaptive-fill)
  (setq-local add-log-current-defun-function #'css-current-defun-name)
  (smie-setup css-smie-grammar #'css-smie-rules
              :forward-token #'css-smie--forward-token
              :backward-token #'css-smie--backward-token)
  (setq-local electric-indent-chars
              (append css-electric-keys electric-indent-chars))
  (add-hook 'completion-at-point-functions
            #'css-completion-at-point nil 'local))

(defvar comment-continue)

(defun css-fill-paragraph (&optional justify)
  (save-excursion
    ;; Fill succeeding comment when invoked right before a multi-line
    ;; comment.
    (when (save-excursion
            (beginning-of-line)
            (comment-search-forward (point-at-eol) t))
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
    (let ((max (max (point-min) (- (point) 1600))))  ; approx 20 lines back
      (when (search-backward "{" max t)
	(skip-chars-backward " \t\r\n")
	(beginning-of-line)
	(if (looking-at "^[ \t]*\\([^{\r\n]*[^ {\t\r\n]\\)")
	    (match-string-no-properties 1))))))

;;; SCSS mode

(defvar scss-mode-syntax-table
  (let ((st (make-syntax-table css-mode-syntax-table)))
    (modify-syntax-entry ?/ ". 124" st)
    (modify-syntax-entry ?\n ">" st)
    st))

(defvar scss-font-lock-keywords
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
  (setq-local font-lock-defaults '(scss-font-lock-keywords nil t)))

(provide 'css-mode)
;;; css-mode.el ends here
