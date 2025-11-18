;;; hideshow.el --- minor mode cmds to selectively display code/comment blocks  -*- lexical-binding:t -*-

;; Copyright (C) 1994-2025 Free Software Foundation, Inc.

;; Author: Thien-Thi Nguyen <ttn@gnu.org>
;;      Dan Nicolaescu <dann@gnu.org>
;; Keywords: C C++ java lisp tools editing comments blocks hiding outlines
;; Maintainer-Version: 5.65.2.2
;; Time-of-Day-Author-Most-Likely-to-be-Recalcitrant: early morning

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

;;; Commentary:

;; * Commands provided
;;
;; This file provides the Hideshow minor mode.  When active, nine commands
;; are available, implementing block hiding and showing.  They (and their
;; keybindings) are:
;;
;;   `hs-hide-block'                      C-c @ C-h
;;   `hs-show-block'                      C-c @ C-s
;;   `hs-hide-all'                        C-c @ C-M-h
;;   `hs-show-all'                        C-c @ C-M-s
;;   `hs-hide-level'                      C-c @ C-l
;;   `hs-toggle-hiding'                   C-c @ C-c
;;   `hs-toggle-hiding'                   S-<mouse-2>
;;   `hs-hide-initial-comment-block'
;;
;; All these commands are defined in `hs-prefix-map',
;; `hs-minor-mode-map' and `hs-indicators-map'.
;;
;; Blocks are defined per mode.  In c-mode, c++-mode and java-mode, they
;; are simply text between curly braces, while in Lisp-ish modes parens
;; are used.  Multi-line comment blocks can also be hidden.  Read-only
;; buffers are not a problem, since hideshow doesn't modify the text.
;;
;; The command `M-x hs-minor-mode' toggles the minor mode or sets it
;; (similar to other minor modes).

;; * Suggested usage
;;
;; Add the following to your init file:
;;
;;     (require 'hideshow)
;;     (add-hook 'X-mode-hook #'hs-minor-mode)       ; other modes similarly
;;
;; where X = {emacs-lisp,c,c++,perl,...}.  You can also manually toggle
;; hideshow minor mode by typing `M-x hs-minor-mode'.  After hideshow is
;; activated or deactivated, `hs-minor-mode-hook' is run with `run-hooks'.
;;
;; Additionally, Joseph Eydelnant writes:
;;   I enjoy your package hideshow.el Version 5.24 2001/02/13
;;   a lot and I've been looking for the following functionality:
;;   toggle hide/show all with a single key.
;;   Here are a few lines of code that lets me do just that.
;;
;;     (defvar my-hs-hide nil "Current state of hideshow for toggling all.")
;;     ;;;###autoload
;;     (defun my-toggle-hideshow-all () "Toggle hideshow all."
;;       (interactive)
;;       (setq my-hs-hide (not my-hs-hide))
;;       (if my-hs-hide
;;           (hs-hide-all)
;;         (hs-show-all)))
;;
;; [Your hideshow hacks here!]

;; * Customization
;;
;; You can use `M-x customize-variable' on the following variables:
;;
;; - `hs-hide-comments-when-hiding-all' -- self-explanatory!
;; - `hs-hide-all-non-comment-function' -- if non-nil, when doing a
;;                                         `hs-hide-all', this function
;;                                         is called with no arguments
;; - `hs-isearch-open'                  -- what kind of hidden blocks to
;;                                         open when doing isearch
;; - `hs-display-lines-hidden'          -- displays the number of hidden
;;                                         lines next to the ellipsis.
;; - `hs-show-indicators'               -- display indicators to show
;;                                         and toggle the block hiding.
;; - `hs-indicator-type'                -- which indicator type should be
;;                                         used for the block indicators.
;; - `hs-indicator-maximum-buffer-size' -- max buffer size in bytes where
;;                                         the indicators should be enabled.
;;
;; Some languages (e.g., Java) are deeply nested, so the normal behavior
;; of `hs-hide-all' (hiding all but top-level blocks) results in very
;; little information shown, which is not very useful.  You can use the
;; variable `hs-hide-all-non-comment-function' to implement your idea of
;; what is more useful.  For example, the following code shows the next
;; nested level in addition to the top-level:
;;
;;     (defun ttn-hs-hide-level-1 ()
;;       (when (funcall hs-looking-at-block-start-predicate)
;;         (hs-hide-level 1))
;;       (forward-sexp 1))
;;     (setq hs-hide-all-non-comment-function 'ttn-hs-hide-level-1)
;;
;; Hideshow works with incremental search (isearch) by setting the variable
;; `hs-headline', which is the line of text at the beginning of a hidden
;; block that contains a match for the search.  You can have this show up
;; in the mode line by modifying the variable `mode-line-format'.  For
;; example, the following code prepends this info to the mode line:
;;
;;     (unless (memq 'hs-headline mode-line-format)
;;       (setq mode-line-format
;;             (append '("-" hs-headline) mode-line-format)))
;;
;; See documentation for `mode-line-format' for more info.
;;
;; Hooks are run after some commands:
;;
;;   hs-hide-hook     in      hs-hide-block, hs-hide-all, hs-hide-level
;;   hs-show-hook             hs-show-block, hs-show-all
;;
;; One of `hs-hide-hook' or `hs-show-hook' is run for the toggling
;; commands when the result of the toggle is to hide or show blocks,
;; respectively.  All hooks are run with `run-hooks'.  See the
;; documentation for each variable or hook for more information.
;;
;; See also variable `hs-set-up-overlay' for per-block customization of
;; appearance or other effects associated with overlays.  For example:
;;
;; (setq hs-set-up-overlay
;;       (defun my-display-code-line-counts (ov)
;;         (when (eq 'code (overlay-get ov 'hs))
;;           (overlay-put ov 'display
;;                        (propertize
;;                         (format " ... <%d>"
;;                                 (count-lines (overlay-start ov)
;;                                              (overlay-end ov)))
;;                         'face 'font-lock-type-face)))))

;; * Extending hideshow

;; ** Adding support for a major mode
;;
;; Normally, hideshow tries to determine appropriate values for block
;; and comment definitions by examining the major mode settings.  If the
;; major mode is not derived from `prog-mode', hideshow will not
;; activate.  If you want to override this, you can set any of the
;; following variables: `hs-block-start-regexp',
;; `hs-block-start-mdata-select', `hs-block-end-regexp',
;; `hs-c-start-regexp', `hs-forward-sexp-function',
;; `hs-adjust-block-beginning-function', `hs-adjust-block-end-function',
;; `hs-find-block-beginning-function', `hs-find-next-block-function',
;; `hs-looking-at-block-start-predicate', `hs-inside-comment-predicate',
;; `hs-treesit-things'.
;;
;; These variables help hideshow know what is considered a block, which
;; function to use to get the block positions, etc.
;;
;; A block is defined as text surrounded by `hs-block-start-regexp' and
;; `hs-block-end-regexp'.
;;
;; For some major modes, forward-sexp does not work properly.  In those
;; cases, `hs-forward-sexp-function' specifies another function to use
;; instead.

;; *** Tree-sitter support
;;
;; All the treesit based modes already have support for hidding/showing
;; using the treesit thing `list' (see `treesit-major-mode-setup').
;;
;; However, for some modes the `list' thing is not enough for detecting
;; the proper code block and the range to hide, you can set the variable
;; `hs-treesit-things' to override this, but ensure you have the proper
;; values in `hs-adjust-block-end-function' and `hs-adjust-block-beginning-function' to
;; properly hide the code block.

;; ** Migrating from `hs-special-modes-alist'
;;
;; Starting with Emacs 31, `hs-special-modes-alist' has been deprecated.
;; Instead, modes should use the buffer-local variables that replace
;; each of the options in `hs-special-modes-alist'.  The following table
;; shows the old elements of `hs-special-modes-alist' and their
;; replacement buffer-local variables:
;;
;;   Instead of this                Use this
;;   -----------------------------------------------------------------------
;;   START                         `hs-block-start-regexp'
;;   (START . MDATA)               `hs-block-start-regexp' and `hs-block-start-mdata-select'
;;   END                           `hs-block-end-regexp'
;;   COMMENT-START                 `hs-c-start-regexp'
;;   FORWARD-SEXP-FUNC             `hs-forward-sexp-function'
;;   ADJUST-BEG-FUNC               `hs-adjust-block-beginning-function'
;;   FIND-BLOCK-BEGINNING-FUNC     `hs-find-block-beginning-function'
;;   FIND-NEXT-BLOCK-FUNC          `hs-find-next-block-function'
;;   LOOKING-AT-BLOCK-START-P-FUNC `hs-looking-at-block-start-predicate')

;; * Bugs
;;
;; (1) Sometimes `hs-headline' can become out of sync.  To reset, type
;;     `M-x hs-minor-mode' twice (that is, deactivate then re-activate
;;     hideshow).
;;
;; (2) Some buffers can't be `byte-compile-file'd properly.  This is because
;;     `byte-compile-file' inserts the file to be compiled in a temporary
;;     buffer and switches `normal-mode' on.  In the case where you have
;;     `hs-hide-initial-comment-block' in `hs-minor-mode-hook', the hiding of
;;     the initial comment sometimes hides parts of the first statement (seems
;;     to be only in `normal-mode'), so there are unbalanced "(" and ")".
;;
;;     The workaround is to clear `hs-minor-mode-hook' when byte-compiling:
;;
;;     (defadvice byte-compile-file (around
;;                                   byte-compile-file-hideshow-off
;;                                   act)
;;       (let ((hs-minor-mode-hook nil))
;;         ad-do-it))
;;
;; (3) Hideshow interacts badly with Ediff and `vc-diff'.  At the moment, the
;;     suggested workaround is to turn off hideshow entirely, for example:
;;
;;     (add-hook 'ediff-prepare-buffer-hook #'turn-off-hideshow)
;;     (add-hook 'vc-before-checkin-hook #'turn-off-hideshow)
;;
;;     In the case of `vc-diff', here is a less invasive workaround:
;;
;;     (add-hook 'vc-before-checkin-hook
;;               (lambda ()
;;                 (goto-char (point-min))
;;                 (hs-show-block)))
;;
;;     Unfortunately, these workarounds do not restore hideshow state.
;;     If someone figures out a better way, please let me know.

;; * Correspondence
;;
;; Correspondence welcome; please indicate version number.  Send bug
;; reports and inquiries to <ttn@gnu.org>.

;; * Thanks
;;
;; Thanks go to the following people for valuable ideas, code and
;; bug reports.
;;
;;  Dean Andrews, Alf-Ivar Holm, Holger Bauer, Christoph Conrad, Dave Love,
;;  Dirk Herrmann, Gael Marziou, Jan Djarv, Guillaume Leray, Moody Ahmad,
;;  Preston F. Crow, Lars Lindberg, Reto Zimmermann, Keith Sheffield,
;;  Chew Meng Kuan, Tony Lam, Pete Ware, François Pinard, Stefan Monnier,
;;  Joseph Eydelnant, Michael Ernst, Peter Heslin
;;
;; Special thanks go to Dan Nicolaescu, who reimplemented hideshow using
;; overlays (rather than selective display), added isearch magic, folded
;; in custom.el compatibility, generalized comment handling, incorporated
;; mouse support, and maintained the code in general.  Version 4.0 is
;; largely due to his efforts.

;; * History
;;
;; Hideshow was inspired when I learned about selective display.  It was
;; reimplemented to use overlays for 4.0 (see above).  WRT older history,
;; entries in the masterfile corresponding to versions 1.x and 2.x have
;; been lost.  XEmacs support is reliable as of 4.29.  State save and
;; restore was added in 3.5 (not widely distributed), and reliable as of
;; 4.30.  Otherwise, the code seems stable.  Passes checkdoc as of 4.32.
;; Version 5.x uses new algorithms for block selection and traversal,
;; unbundles state save and restore, and includes more isearch support.

;;; Code:
(require 'mule-util) ; For `truncate-string-ellipsis'
;; For indicators
(require 'icons)
(require 'fringe)

;;---------------------------------------------------------------------------
;; user-configurable variables

(defgroup hideshow nil
  "Minor mode for hiding and showing program and comment blocks."
  :prefix "hs-"
  :group 'languages)

(defface hs-ellipsis
  '((t :height 0.80 :box (:line-width -1) :inherit (shadow default)))
  "Face used for hideshow ellipsis.
Note: If `selective-display' ellipsis already has a face, hideshow will
use that face for the ellipsis instead."
  :version "31.1")

(defface hs-indicator-hide
  '((t :inherit (shadow default)))
  "Face used in hideshow indicator to indicate a hidden block."
  :version "31.1")

(defface hs-indicator-show
  '((t :inherit hs-indicator-hide :weight bold))
  "Face used in hideshow indicator to indicate a shown block."
  :version "31.1")

(defcustom hs-hide-comments-when-hiding-all t
  "Hide the comments too when you do an `hs-hide-all'."
  :type 'boolean)

(defcustom hs-hide-block-behavior 'after-bol
  "How hideshow should hide a block.
If set to `after-bol', hide the innermost block to which the current
line belongs.

If set to `after-cursor', hide the block after cursor position.

This only has effect in `hs-hide-block' and `hs-toggle-hiding'
commands."
  :type
  '(choice
    (const :tag "Hide the block after cursor" after-cursor)
    (const :tag "Hide the block after beginning of current line" after-bol))
  :version "31.1")

(defcustom hs-display-lines-hidden nil
  "If non-nil, display the number of hidden lines next to the ellipsis."
  :type 'boolean
  :version "31.1")

(defcustom hs-minor-mode-hook nil
  "Hook called when hideshow minor mode is activated or deactivated."
  :type 'hook
  :version "21.1")

(defcustom hs-isearch-open 'code
  "What kind of hidden blocks to open when doing `isearch'.
One of the following symbols:

  code    -- open only code blocks
  comment -- open only comment blocks
  t       -- open both code and comment blocks
  nil     -- open neither code nor comment blocks

This has effect only if `search-invisible' is set to `open'."
  :type '(choice (const :tag "open only code blocks" code)
                 (const :tag "open only comment blocks" comment)
                 (const :tag "open both code and comment blocks" t)
                 (const :tag "don't open any of them" nil)))

(defcustom hs-show-indicators nil
  "Whether hideshow should display block hide/show indicators.
If non-nil, hideshow will display indicators for toggling the visibility
of code blocks.

The indicators appearance are specified in `hs-indicator-type' (which see)."
  :type 'boolean
  :version "31.1")

(defcustom hs-indicator-type 'fringe
  "Indicate which indicator type to use for the block indicators.

The possible values can be:

 - `fringe', display the indicators in the fringe.
 - `margin', display the indicators in the margin.
 - nil, display the indicators at end-of-line.

This only has effect if `hs-show-indicators' is non-nil."
  :type '(choice
          (const :tag "Fringes" fringe)
          (const :tag "Margins" margin)
          (const :tag "Indicator at end-of-line" nil))
  :version "31.1")

(defcustom hs-indicator-maximum-buffer-size 2000000 ;2mb
  "Max buffer size in bytes where the indicators should be enabled.
If current buffer is larger than this variable value, the indicators
will be disabled.

If set to nil, the indicators will be activated regardless of the buffer
size."
  :type '(choice natnum (const :tag "No limit" nil))
  :version "31.1")

(define-fringe-bitmap
  'hs-hide
  [#b0000000
   #b1000001
   #b1100011
   #b0110110
   #b0011100
   #b0001000
   #b0000000])

(define-fringe-bitmap
  'hs-show
  [#b0110000
   #b0011000
   #b0001100
   #b0000110
   #b0001100
   #b0011000
   #b0110000])

(define-icon hs-indicator-hide nil
  `((image "outline-open.svg" "outline-open.pbm"
           :face hs-indicator-hide
           :height (0.6 . em)
           :ascent center)
    (symbol "▾" "▼" :face hs-indicator-hide)
    (text "-" :face hs-indicator-hide))
  "Icon used for hide block at point.
This is only used if `hs-indicator-type' is set to `margin' or nil."
  :version "31.1")

(define-icon hs-indicator-show nil
  `((image "outline-close.svg" "outline-close.pbm"
           :face hs-indicator-show
           :height (0.6 . em)
           :ascent center)
    (symbol "▸" "▶" :face hs-indicator-show)
    (text "+" :face hs-indicator-show))
  "Icon used for show block at point.
This is only used if `hs-indicator-type' is set to `margin' or nil."
  :version "31.1")

;;;###autoload
(defvar hs-special-modes-alist nil)
(make-obsolete-variable 'hs-special-modes-alist
                        "use the buffer-local variables instead"
                        "31.1")

(defcustom hs-allow-nesting nil
  "If non-nil, hiding remembers internal blocks.
This means that when the outer block is shown again,
any previously hidden internal blocks remain hidden."
  :type 'boolean
  :version "31.1")

(defvar hs-hide-hook nil
  "Hook called (with `run-hooks') at the end of commands to hide text.
These commands include the toggling commands (when the result is to hide
a block), `hs-hide-all', `hs-hide-block' and `hs-hide-level'.")

(defvar hs-show-hook nil
  "Hook called (with `run-hooks') at the end of commands to show text.
These commands include the toggling commands (when the result is to show
a block), `hs-show-all' and `hs-show-block'.")

(defcustom hs-set-up-overlay #'ignore
  "Function called with one arg, OV, a newly initialized overlay.
Hideshow puts a unique overlay on each range of text to be hidden
in the buffer.  Here is a simple example of how to use this variable:

  (defun display-code-line-counts (ov)
    (when (eq \\='code (overlay-get ov \\='hs))
      (overlay-put ov \\='display
                   (format \"... / %d\"
                           (count-lines (overlay-start ov)
                                        (overlay-end ov))))))

  (setq hs-set-up-overlay #\\='display-code-line-counts)

This example shows how to get information from the overlay as well
as how to set its `display' property.  See `hs-make-overlay' and
info node `(elisp)Overlays'."
  :type 'function
  :version "28.1")

(defcustom hs-cycle-filter nil
  "Control where typing a \\`TAB' cycles the visibility.
This option determines on which parts of a line where a block
begins \\`TAB' will be bound to visibility-cycling commands such
as `hs-toggle-hiding'.  The value t means you can type \\`TAB'
anywhere on a headline.  The value nil means \\`TAB' always has its
usual binding.  The value can also be a function of no arguments,
then \\`TAB' will invoke the visibility-cycling commands where that
function returns non-nil.  For example, if the value is `bolp',
those commands will be invoked at the headline's beginning.
This allows to preserve the usual bindings, as determined by the
major mode, elsewhere on the headlines."
  :type `(choice (const :tag "Nowhere" nil)
                 (const :tag "Everywhere on the headline" t)
                 (const :tag "At block beginning"
                        ,(lambda ()
                           (pcase-let ((`(,beg ,end) (hs-block-positions)))
                             (and beg (hs-hideable-region-p beg end)))))
                 (const :tag "At line beginning" bolp)
                 (const :tag "Not at line beginning"
                        ,(lambda () (not (bolp))))
                 (const :tag "At line end" eolp)
                 (function :tag "Custom filter function"))
  :version "31.1")

;;---------------------------------------------------------------------------
;; internal variables

(defvar hs-minor-mode nil
  "Non-nil if using hideshow mode as a minor mode of some other mode.
Use the command `hs-minor-mode' to toggle or set this variable.")

(defvar-keymap hs-prefix-map
  :doc "Keymap for hideshow commands."
  :prefix t
  ;; These bindings roughly imitate those used by Outline mode.
  "C-h"   #'hs-hide-block
  "C-s"   #'hs-show-block
  "C-M-h" #'hs-hide-all
  "C-M-s" #'hs-show-all
  "C-l"   #'hs-hide-level
  "C-c"   #'hs-toggle-hiding
  "C-a"   #'hs-show-all
  "C-t"   #'hs-hide-all
  "C-d"   #'hs-hide-block
  "C-e"   #'hs-toggle-hiding)

(defvar-keymap hs-minor-mode-map
  :doc "Keymap for hideshow minor mode."
  "S-<mouse-2>" #'hs-toggle-hiding
  "C-c @" hs-prefix-map
  "TAB" `(menu-item
          "" hs-toggle-hiding
          :filter
          ,(lambda (cmd)
             (when (and hs-cycle-filter
                        ;; On the headline with hideable blocks
                        (save-excursion
                          (goto-char (line-beginning-position))
                          (hs-get-first-block))
                        (or (not (functionp hs-cycle-filter))
                            (funcall hs-cycle-filter)))
               cmd)))
  "<left-fringe> <mouse-1>" #'hs-indicator-mouse-toggle-hiding)

(defvar-keymap hs-indicators-map
  :doc "Keymap for hideshow indicators."
  "<left-margin> <mouse-1>" #'hs-indicator-mouse-toggle-hiding
  "<mouse-1>" #'hs-toggle-hiding)

(easy-menu-define hs-minor-mode-menu hs-minor-mode-map
  "Menu used when hideshow minor mode is active."
  '("Hide/Show"
    ["Hide Block"    hs-hide-block
     :help "Hide the code or comment block at point"]
    ["Show Block"    hs-show-block
     :help "Show the code or comment block at point"]
    ["Hide All"      hs-hide-all
     :help "Hide all the blocks in the buffer"]
    ["Show All"      hs-show-all
     :help "Show all the blocks in the buffer"]
    ["Hide Level"    hs-hide-level
     :help "Hide all block at levels below the current block"]
    ["Toggle Hiding" hs-toggle-hiding
     :help "Toggle the hiding state of the current block"]
    "----"
    ["Hide comments when hiding all"
     (setq hs-hide-comments-when-hiding-all
    	   (not hs-hide-comments-when-hiding-all))
     :help "If t also hide comment blocks when doing `hs-hide-all'"
     :style toggle :selected hs-hide-comments-when-hiding-all]
   ("Reveal on isearch"
     ["Code blocks" (setq hs-isearch-open 'code)
      :help "Show hidden code blocks when isearch matches inside them"
      :active t :style radio   :selected (eq hs-isearch-open 'code)]
     ["Comment blocks" (setq hs-isearch-open 'comment)
      :help "Show hidden comment blocks when isearch matches inside them"
      :active t :style radio :selected (eq hs-isearch-open 'comment)]
     ["Code and Comment blocks" (setq hs-isearch-open t)
      :help "\
Show both hidden code and comment blocks when isearch matches inside them"
      :active t :style radio :selected (eq hs-isearch-open t)]
     ["None" (setq hs-isearch-open nil)
      :help "\
Do not show hidden code or comment blocks when isearch matches inside them"
      :active t :style radio :selected (eq hs-isearch-open nil)])))

(defvar hs-hide-all-non-comment-function nil
  "Function called if non-nil when doing `hs-hide-all' for non-comments.")

(defvar hs-headline nil
  "Text of the line where a hidden block begins, set during isearch.
You can display this in the mode line by adding the symbol `hs-headline'
to the variable `mode-line-format'.  For example,

  (unless (memq \\='hs-headline mode-line-format)
    (setq mode-line-format
          (append \\='(\"-\" hs-headline) mode-line-format)))

Note that `mode-line-format' is buffer-local.")

;;---------------------------------------------------------------------------
;; API variables

(defvar-local hs-block-start-regexp "\\s("
  "Regexp for beginning of block.")

(defvar-local hs-block-start-mdata-select 0
  "Element in `hs-block-start-regexp' match data to consider as block start.
The internal function `hs-forward-sexp' moves point to the beginning of this
element (using `match-beginning') before calling `hs-forward-sexp-function'.")

(defvar-local hs-block-end-regexp "\\s)"
  "Regexp for end of block.")

(defvar-local hs-c-start-regexp nil
  "Regexp for beginning of comments.
Differs from mode-specific comment regexps in that surrounding
whitespace is stripped.

If not bound, hideshow will use current `comment-start' value without
any trailing whitespace.")

(define-obsolete-variable-alias
  'hs-forward-sexp-func
  'hs-forward-sexp-function
  "31.1")

(defvar-local hs-forward-sexp-function #'forward-sexp
  "Function used to do a `forward-sexp'.
Should change for Algol-ish modes.  For single-character block
delimiters -- ie, the syntax table regexp for the character is
either `(' or `)' -- `hs-forward-sexp-function' would just be
`forward-sexp'.  For other modes such as simula, a more specialized
function is necessary.")

(define-obsolete-variable-alias
  'hs-adjust-block-beginning
  'hs-adjust-block-beginning-function
  "31.1")

(defvar-local hs-adjust-block-beginning-function nil
  "Function used to tweak the block beginning.
The block is hidden from the position returned by this function,
as opposed to hiding it from the position returned when searching
for `hs-block-start-regexp'.

For example, in c-like modes, if we wish to also hide the curly braces
\(if you think they occupy too much space on the screen), this function
should return the starting point (at the end of line) of the hidden
region.

It is called with a single argument ARG which is the position in
buffer after the block beginning.

It should return the position from where we should start hiding.

It should not move the point.

See `hs-c-like-adjust-block-beginning' for an example of using this.")

(defvar-local hs-adjust-block-end-function nil
  "Function used to tweak the block end.
This is useful to ensure some characters such as parenthesis or curly
braces get properly hidden in python-like modes.

It is called with one argument, which is the start position where the
overlay will be created, and should return either the last position to
hide or nil.  If it returns nil, hideshow will guess the end position.")

(define-obsolete-variable-alias
  'hs-find-block-beginning-func
  'hs-find-block-beginning-function
  "31.1")

(defvar-local hs-find-block-beginning-function #'hs-find-block-beginning
  "Function used to do `hs-find-block-beginning'.
It should reposition point at the beginning of the current block
and return point, or nil if original point was not in a block.

Specifying this function is necessary for languages such as
Python, where regexp search and `syntax-ppss' check is not enough
to find the beginning of the current block.")

(define-obsolete-variable-alias
  'hs-find-next-block-func
  'hs-find-next-block-function
  "31.1")

(defvar-local hs-find-next-block-function #'hs-find-next-block
  "Function used to do `hs-find-next-block'.
It should reposition point at next block start.

It is called with three arguments REGEXP, MAXP, and COMMENTS.
REGEXP is a regexp representing block start.  When block start is
found, `match-data' should be set using REGEXP.  MAXP is a buffer
position that limits the search.  When COMMENTS is nil, comments
should be skipped.  When COMMENTS is not nil, REGEXP matches not
only beginning of a block but also beginning of a comment.  In
this case, the function should find nearest block or comment.

Specifying this function is necessary for languages such as
Python, where regexp search is not enough to find the beginning
of the next block.")

(define-obsolete-variable-alias
  'hs-looking-at-block-start-p-func
  'hs-looking-at-block-start-predicate
  "31.1")

(defvar-local hs-looking-at-block-start-predicate #'hs-looking-at-block-start-p
  "Function used to do `hs-looking-at-block-start-p'.
It should return non-nil if the point is at the block start.

Specifying this function is necessary for languages such as
Python, where `looking-at' and `syntax-ppss' check is not enough
to check if the point is at the block start.")

(defvar-local hs-inside-comment-predicate #'hs-inside-comment-p--default
  "Function used to check if point is inside a comment.
If point is inside a comment, the function should return a list
containing the buffer position of the start and the end of the
comment, otherwise it should return nil.

A comment block can be hidden only if on its starting line there is only
whitespace preceding the actual comment beginning.  If point is inside
a comment but this condition is not met, the function can return a list
having nil as its `car' and the end of comment position as its `cdr'.")

(defvar-local hs-treesit-things 'list
  "Treesit things to check if point is at a valid block.
The value should be a thing defined in `treesit-thing-settings' for the
current buffer's major mode.")

;;---------------------------------------------------------------------------
;; support functions

(defun hs-discard-overlays (from to)
  "Delete hideshow overlays in region defined by FROM and TO.
Skip \"internal\" overlays if `hs-allow-nesting' is non-nil."
  (when (< to from)
    (setq from (prog1 to (setq to from))))
  (if hs-allow-nesting
      (let ((from from) ov)
        (while (> to (setq from (next-overlay-change from)))
          (when (setq ov (hs-overlay-at from))
            (setq from (overlay-end ov))
            (delete-overlay ov))))
    (dolist (ov (overlays-in from to))
      (when (overlay-get ov 'hs)
        (delete-overlay ov))))
  (hs--refresh-indicators from to))

(defun hs-hideable-region-p (&optional beg end)
  "Return t if region between BEG and END can be hidden.
If BEG and END are not specified, try to check the current
block at point."
  ;; Check if BEG and END are not in the same line number,
  ;; since using `count-lines' is slow.
  (if (and beg end)
      (< beg (save-excursion (goto-char end) (line-beginning-position)))
    (when-let* ((block (hs-block-positions)))
      (apply #'hs-hideable-region-p block))))

(defun hs--discard-overlay-after-change (o &rest _r)
  "Remove overlay O after changes.
Intended to be used in `modification-hooks', `insert-in-front-hooks' and
`insert-behind-hooks'."
  (let ((beg (overlay-start o))
        (end (overlay-end o)))
    (delete-overlay o)
    (hs--refresh-indicators beg end)))

(defun hs-make-overlay (b e kind &optional b-offset e-offset)
  "Return a new overlay in region defined by B and E with type KIND.
KIND is either `code' or `comment'.  Optional fourth arg B-OFFSET
when added to B specifies the actual buffer position where the block
begins.  Likewise for optional fifth arg E-OFFSET.  If unspecified
they are taken to be 0 (zero).  The following properties are set
in the overlay: `invisible' `hs' `hs-b-offset' `hs-e-offset'.  Also,
depending on variable `hs-isearch-open', the following properties may
be present: `isearch-open-invisible' `isearch-open-invisible-temporary'.
If variable `hs-set-up-overlay' is non-nil it should specify a function
to call with the newly initialized overlay."
  (unless b-offset (setq b-offset 0))
  (unless e-offset (setq e-offset 0))
  (let ((ov (make-overlay b e))
        (io (if (eq 'block hs-isearch-open)
                ;; backward compatibility -- `block'<=>`code'
                'code
              hs-isearch-open)))
    (overlay-put ov 'invisible 'hs)
    (overlay-put ov 'display
                 (propertize
                  (hs--get-ellipsis b e)
                  'mouse-face
                  'highlight
                  'help-echo "mouse-1: show hidden lines"
                  'keymap '(keymap (mouse-1 . hs-toggle-hiding))))
    ;; Internal properties
    (overlay-put ov 'hs kind)
    (overlay-put ov 'hs-b-offset b-offset)
    (overlay-put ov 'hs-e-offset e-offset)
    ;; Isearch integration
    (when (or (eq io t) (eq io kind))
      (overlay-put ov 'isearch-open-invisible 'hs-isearch-show)
      (overlay-put ov 'isearch-open-invisible-temporary
                   'hs-isearch-show-temporary))
    ;; Remove overlay after modifications
    (overlay-put ov 'modification-hooks    '(hs--discard-overlay-after-change))
    (overlay-put ov 'insert-in-front-hooks '(hs--discard-overlay-after-change))
    (overlay-put ov 'insert-behind-hooks   '(hs--discard-overlay-after-change))

    (when hs-set-up-overlay (funcall hs-set-up-overlay ov))
    (hs--refresh-indicators b e)
    ov))

(defun hs-block-positions ()
  "Return the current code block positions.
This returns a list with the current code block beginning and end
positions.  This does nothing if there is not a code block at current
point."
  ;; `catch' is used here if the search fails due unbalanced parentheses
  ;; or any other unknown error caused in `hs-forward-sexp'.
  (catch 'hs-sexp-error
    (save-match-data
      (save-excursion
        (when (funcall hs-looking-at-block-start-predicate)
          (let ((mdata (match-data t))
                (header-end (match-end 0))
                block-beg block-end)
            ;; `block-start' is the point at the end of the block
            ;; beginning, which may need to be adjusted
            (save-excursion
              (when hs-adjust-block-beginning-function
                (goto-char (funcall hs-adjust-block-beginning-function header-end)))
              (setq block-beg (line-end-position)))
            ;; `block-end' is the point at the end of the block
            (condition-case _
                (hs-forward-sexp mdata 1)
              (scan-error (throw 'hs-sexp-error nil)))
            (setq block-end
                  (cond ((and (stringp hs-block-end-regexp)
                              (looking-back hs-block-end-regexp nil))
                         (match-beginning 0))
                        ((functionp hs-block-end-regexp)
                         (funcall hs-block-end-regexp)
                         (match-beginning 0))
                        (t (point))))
            ;; adjust block end (if needed)
            (when hs-adjust-block-end-function
              (setq block-end
                    (or (funcall hs-adjust-block-end-function block-beg)
                        block-end)))
            (list block-beg block-end)))))))

(defun hs--make-indicators-overlays (beg)
  "Helper function to make the indicators overlays."
  (let ((hiddenp (eq 'hs (get-char-property (pos-eol) 'invisible))))
    ;; If we are going to use the EOL indicators, then
    ;; ignore the invisible lines which mostly are already
    ;; hidden blocks.
    (when (or hs-indicator-type (not hiddenp))
      (let* ((o (make-overlay
                 (if hs-indicator-type beg (pos-eol))
                 (1+ (if hs-indicator-type beg (pos-eol)))))
             (fringe-type (if hiddenp 'hs-show 'hs-hide))
             (face-or-icon (if hiddenp 'hs-indicator-show 'hs-indicator-hide)))

        (overlay-put o 'hs-indicator t)
        (overlay-put o 'hs-indicator-block-start beg)
        (overlay-put o 'evaporate t)
        (overlay-put o 'priority -50)

        (overlay-put
         o 'before-string
         (pcase hs-indicator-type
           ;; Fringes
           ('fringe
            (propertize
             "+" 'display
             `(left-fringe ,fringe-type ,face-or-icon)))
           ;; Margins
           ('margin
            (propertize
             "+" 'display
             `((margin left-margin)
               ,(or (plist-get (icon-elements face-or-icon) 'image)
                    (propertize (icon-string face-or-icon)
                                'keymap hs-indicators-map)))
             'face face-or-icon
             'keymap hs-indicators-map))
           ;; EOL string
           ('nil
            (concat
             (propertize " " 'cursor t)
             (propertize
              (icon-string face-or-icon)
              'mouse-face 'highlight
              'keymap hs-indicators-map)))))))))

(defun hs--add-indicators (&optional beg end)
  "Add hideable indicators from BEG to END."
  (save-excursion
    (setq beg (if (null beg) (window-start) (goto-char beg) (pos-bol))
          end (if (null end) (window-end) (goto-char end) (pos-bol))))
  (goto-char beg)
  (remove-overlays beg end 'hs-indicator t)

  (while (not (>= (point) end))
    (save-excursion
      (when-let* ((b-beg (hs-get-first-block)))
        (hs--make-indicators-overlays b-beg)))
    ;; Only 1 indicator per line
    (forward-line))
  `(jit-lock-bounds ,beg . ,end))

(defun hs--refresh-indicators (from to)
  "Update indicator appearance in FROM and TO."
  (when (and hs-show-indicators hs-minor-mode)
    (save-match-data
      (save-excursion
        (hs--add-indicators from to)))))

(defun hs--get-ellipsis (b e)
  "Helper function for `hs-make-overlay'.
This returns the ellipsis string to use and its face."
  (let* ((standard-display-table
          (or standard-display-table (make-display-table)))
         (d-t-ellipsis
          (display-table-slot standard-display-table 'selective-display))
         ;; Convert ellipsis vector to a propertized string
         (ellipsis
          (and (vectorp d-t-ellipsis) ; Ensure the vector is not empty
               (not (length= d-t-ellipsis 0))
               (mapconcat
                (lambda (g)
                  (apply #'propertize (char-to-string (glyph-char g))
                         (and (glyph-face g) (list 'face (glyph-face g)))))
                d-t-ellipsis)))
         (ellipsis-face (and ellipsis (get-text-property 0 'face ellipsis)))
         (apply-face (lambda (str)
                       (apply #'propertize str
                              (and ellipsis-face (list 'face ellipsis-face)))))
         (lines (when-let* (hs-display-lines-hidden
                            (l (1- (count-lines b e)))
                            (l-str (format "%d %s" l
                                           (if (= l 1) "line" "lines"))))
                  (funcall apply-face l-str)))
         (tty-strings (and hs-display-lines-hidden (not (display-graphic-p))))
         (string
          (concat (and tty-strings (funcall apply-face "["))
                  lines
                  (or ellipsis (truncate-string-ellipsis))
                  (and tty-strings (funcall apply-face "]")))))
    (if ellipsis-face
        ;; Return ELLIPSIS and LINES if ELLIPSIS has no face
        string
      ;; Otherwise propertize both with `hs-ellipsis'
      (propertize string 'face 'hs-ellipsis))))

(defun hs-isearch-show (ov)
  "Delete overlay OV, and set `hs-headline' to nil.

This function is meant to be used as the `isearch-open-invisible'
property of an overlay."
  (setq hs-headline nil)
  (delete-overlay ov))

(defun hs-isearch-show-temporary (ov hide-p)
  "Hide or show overlay OV, and set `hs-headline', all depending on HIDE-P.
If HIDE-P is non-nil, `hs-headline' is set to nil and overlay OV is hidden.
Otherwise, `hs-headline' is set to the line of text at the head of OV, and
OV is shown.

This function is meant to be used as the `isearch-open-invisible-temporary'
property of an overlay."
  (setq hs-headline
        (if hide-p
            nil
          (or hs-headline
              (let ((start (overlay-start ov)))
                (buffer-substring
                 (save-excursion (goto-char start)
                                 (beginning-of-line)
                                 (skip-chars-forward " \t")
                                 (point))
                 start)))))
  (force-mode-line-update)
  ;; handle `display' property specially
  (let (value)
    (if hide-p
        (when (setq value (overlay-get ov 'hs-isearch-display))
          (overlay-put ov 'display value)
          (overlay-put ov 'hs-isearch-display nil))
      (when (setq value (overlay-get ov 'display))
        (overlay-put ov 'hs-isearch-display value)
        (overlay-put ov 'display nil))))
  (overlay-put ov 'invisible (and hide-p 'hs)))

(defun hs-looking-at-block-start-p ()
  "Return non-nil if the point is at the block start."
  (and (looking-at hs-block-start-regexp)
       (save-match-data (not (nth 8 (syntax-ppss))))))

(defun hs-forward-sexp (match-data arg)
  "Adjust point based on MATCH-DATA and call `hs-forward-sexp-function' with ARG.
Original match data is restored upon return."
  (save-match-data
    (set-match-data match-data)
    (goto-char (match-beginning hs-block-start-mdata-select))
    (funcall hs-forward-sexp-function arg)))

(defun hs-hide-comment-region (beg end &optional repos-end)
  "Hide a region from BEG to END, marking it as a comment.
Optional arg REPOS-END means reposition at end."
  (let ((goal-col (current-column))
        (beg-bol (progn (goto-char beg) (line-beginning-position)))
        (beg-eol (line-end-position))
        (end-eol (progn (goto-char end) (line-end-position))))
    (hs-discard-overlays beg-eol end-eol)
    (hs-make-overlay beg-eol end-eol 'comment beg end)
    (goto-char (if repos-end end (min end (+ beg-bol goal-col))))))

(defun hs-hide-block-at-point (&optional end comment-reg)
  "Hide block if on block beginning.
Optional arg END means reposition at end.
Optional arg COMMENT-REG is a list of the form (BEGIN END) and
specifies the limits of the comment, or nil if the block is not
a comment.

The block beginning is adjusted by `hs-adjust-block-beginning-function'
and then further adjusted to be at the end of the line.

If hiding the block is successful, return non-nil.
Otherwise, return nil."
  (if comment-reg
      (hs-hide-comment-region (car comment-reg) (cadr comment-reg) end)
    (when-let* ((block (hs-block-positions)))
      (let ((p (car block))
            (q (cadr block))
            ov)
        (if (hs-hideable-region-p p q)
            (progn
              (cond ((and hs-allow-nesting (setq ov (hs-overlay-at p)))
                     (delete-overlay ov))
                    ((not hs-allow-nesting)
                     (hs-discard-overlays p q)))
              (goto-char q)
              (hs-make-overlay p q 'code (- (match-end 0) p)))
          (goto-char (if end q (min p (match-end 0))))
          nil)))))

(defun hs-get-first-block ()
  "Return the position of the first valid block found on the current line.
This searches for a valid block on the current line and returns the
first block found.  Otherwise, if no block is found, it returns nil."
  (let (exit)
    (while (and (not exit)
                (funcall hs-find-next-block-function
                         hs-block-start-regexp
                         (line-end-position) nil)
                (save-excursion
                  (goto-char (match-beginning 0))
                  (if (hs-hideable-region-p)
                      (setq exit (match-beginning 0))
                    t))))
    exit))

(defun hs-inside-comment-p ()
  (declare (obsolete "Call `hs-inside-comment-predicate' instead." "31.1"))
  (funcall hs-inside-comment-predicate))

(defun hs-inside-comment-p--default ()
  (save-excursion
    ;; the idea is to look backwards for a comment start regexp, do a
    ;; forward comment, and see if we are inside, then extend
    ;; forward and backward as long as we have comments
    (let ((q (point)))
      (skip-chars-forward "[:blank:]")
      (when (or (looking-at hs-c-start-regexp)
                (re-search-backward hs-c-start-regexp (point-min) t))
        ;; first get to the beginning of this comment...
        (while (and (not (bobp))
                    (= (point) (progn (forward-comment -1) (point))))
          (forward-char -1))
        ;; ...then extend backwards
        (forward-comment (- (buffer-size)))
        (skip-chars-forward " \t\n\f")
        (let ((p (point))
              (hideable t))
          (beginning-of-line)
          (unless (looking-at (concat "[ \t]*" hs-c-start-regexp))
            ;; we are in this situation: (example)
            ;; (defun bar ()
            ;;      (foo)
            ;;                ) ; comment
            ;;                 ^
            ;;   the point was here before doing (beginning-of-line)
            ;; here we should advance till the next comment which
            ;; eventually has only white spaces preceding it on the same
            ;; line
            (goto-char p)
            (forward-comment 1)
            (skip-chars-forward " \t\n\f")
            (setq p (point))
            (while (and (< (point) q)
                        (> (point) p)
                        (not (looking-at hs-c-start-regexp)))
              ;; avoid an infinite cycle
              (setq p (point))
              (forward-comment 1)
              (skip-chars-forward " \t\n\f"))
            (when (or (not (looking-at hs-c-start-regexp))
                      (> (point) q))
              ;; we cannot hide this comment block
              (setq hideable nil)))
          ;; goto the end of the comment
          (forward-comment (buffer-size))
          (skip-chars-backward " \t\n\f")
          (end-of-line)
          (when (>= (point) q)
            (list (and hideable p) (point))))))))

(defun hs--set-variable (var nth &optional default)
  "Set Hideshow VAR if already not set.
This function is meant to be used for backward compatibility with
`hs-special-modes-alist'.

NTH must be a number indicating NTH element of
`hs-special-modes-alist' or a function.

DEFAULT is a value to use as fallback."
  (unless (local-variable-p var) ; Already set, nothing to do.
    (if-let* ((old-lookup (assoc major-mode hs-special-modes-alist))
              (val (if (integerp nth)
                       (nth nth old-lookup)
                     (funcall nth old-lookup))))
        (set (make-local-variable var) val)
      (when default
        (set (make-local-variable var) default)))))

;; TODO: When `hs-special-modes-alist' is removed, `hs-grok-mode-type'
;; and `hs--set-variable' will no longer be necessary, but
;; `hs-c-start-regexp' will still have to be set manually after enabling
;; `hs-minor-mode'.

(defun hs-grok-mode-type ()
  "Set up hideshow variables for new buffers.
If `hs-special-modes-alist' has information associated with the current
buffer's major mode, use that.  Otherwise, guess start, end and
`comment-start' regexps; `forward-sexp' function; and
adjust-block-beginning function."
  (hs--set-variable 'hs-block-start-regexp
                    (lambda (v) (car (ensure-list (nth 1 v)))))
  (hs--set-variable 'hs-block-start-mdata-select
                    (lambda (v) (cadr (ensure-list (nth 1 v)))))
  (hs--set-variable 'hs-block-end-regexp 2)
  (hs--set-variable 'hs-c-start-regexp 3
                    (string-trim-right (regexp-quote comment-start)))
  (hs--set-variable 'hs-forward-sexp-function 4)
  (hs--set-variable 'hs-adjust-block-beginning-function 5)
  (hs--set-variable 'hs-find-block-beginning-function 6)
  (hs--set-variable 'hs-find-next-block-function 7)
  (hs--set-variable 'hs-looking-at-block-start-predicate 8))

(defun hs-find-block-beginning ()
  "Reposition point at block-start.
Return point, or nil if original point was not in a block."
  (let ((done nil)
        (here (point)))
    ;; look if current line is block start
    (if (funcall hs-looking-at-block-start-predicate)
        (point)
      ;; look backward for the start of a block that contains the cursor
      (while (and (re-search-backward hs-block-start-regexp nil t)
		  ;; go again if in a comment or a string
		  (or (save-match-data (nth 8 (syntax-ppss)))
		      (not (setq done
				 (< here (save-excursion
					   (hs-forward-sexp (match-data t) 1)
					   (point))))))))
      (if done
          (point)
        (goto-char here)
        nil))))

(defun hs-find-next-block (regexp maxp comments)
  "Reposition point at next block-start.
Skip comments if COMMENTS is nil, and search for REGEXP in
region (point MAXP)."
  (when (not comments)
    (forward-comment (point-max)))
  (and (< (point) maxp)
       (re-search-forward regexp maxp t)))

(defun hs-hide-level-recursive (arg minp maxp)
  "Recursively hide blocks ARG levels below point in region (MINP MAXP)."
  (when (funcall hs-find-block-beginning-function)
    (setq minp (1+ (point)))
    (funcall hs-forward-sexp-function 1)
    (setq maxp (1- (point))))
  (unless hs-allow-nesting
    (hs-discard-overlays minp maxp))
  (goto-char minp)
  (while (funcall hs-find-next-block-function hs-block-start-regexp maxp nil)
    (when (save-match-data
	    (not (nth 8 (syntax-ppss)))) ; not inside comments or strings
      (if (> arg 1)
	  (hs-hide-level-recursive (1- arg) minp maxp)
        ;; `hs-hide-block-at-point' already moves the cursor, but if it
        ;; fails, return to the previous position where we were.
	(unless (and (goto-char (match-beginning hs-block-start-mdata-select))
	             (hs-hide-block-at-point t))
            (goto-char (match-end hs-block-start-mdata-select))))))
  (goto-char maxp))

(defmacro hs-life-goes-on (&rest body)
  "Evaluate BODY forms if variable `hs-minor-mode' is non-nil.
In the dynamic context of this macro, `case-fold-search' is t."
  (declare (debug t))
  `(when hs-minor-mode
     (let ((case-fold-search t))
       (save-match-data
         (save-excursion ,@body)))))

(defun hs-find-block-beginning-match ()
  "Reposition point at the end of match of the block-start regexp.
Return point, or nil if original point was not in a block."
  (when (and (funcall hs-find-block-beginning-function)
	     (funcall hs-looking-at-block-start-predicate))
    ;; point is inside a block
    (goto-char (match-end 0))))

(defun hs-overlay-at (position)
  "Return hideshow overlay at POSITION, or nil if none to be found."
  (let ((overlays (overlays-at position))
        ov found)
    (while (and (not found) (setq ov (car overlays)))
      (setq found (and (overlay-get ov 'hs) ov)
            overlays (cdr overlays)))
    found))

(defun hs-already-hidden-p ()
  "Return non-nil if point is in an already-hidden block, otherwise nil."
  (save-excursion
    (let ((c-reg (funcall hs-inside-comment-predicate)))
      (when (and c-reg (nth 0 c-reg))
        ;; point is inside a comment, and that comment is hideable
        (goto-char (nth 0 c-reg))))
    ;; Search for a hidden block at EOL ...
    (or (eq 'hs (get-char-property (line-end-position) 'invisible))
        ;; ... or behind the current cursor position
        (eq 'hs (get-char-property (if (bobp) (point) (1- (point))) 'invisible)))))

;; This function is not used anymore (Bug#700).
(defun hs-c-like-adjust-block-beginning (initial)
  "Adjust INITIAL, the buffer position after `hs-block-start-regexp'.
Actually, point is never moved; a new position is returned that is
the end of the C-function header.  This adjustment function is meant
to be assigned to `hs-adjust-block-beginning-function' for C-like modes."
  (save-excursion
    (goto-char (1- initial))
    (forward-comment (- (buffer-size)))
    (point)))

;;---------------------------------------------------------------------------
;; commands

(defun hs-hide-all ()
  "Hide all top level blocks, displaying only first and last lines.
Move point to the beginning of the line, and run the normal hook
`hs-hide-hook'.  See documentation for `run-hooks'.
If `hs-hide-comments-when-hiding-all' is non-nil, also hide the comments."
  (interactive)
  (hs-life-goes-on
   (save-excursion
     (unless hs-allow-nesting
       (hs-discard-overlays (point-min) (point-max)))
     (goto-char (point-min))
     (syntax-propertize (point-max))
     (let ((spew (make-progress-reporter "Hiding all blocks..."
                                         (point-min) (point-max)))
           (re (when (stringp hs-block-start-regexp)
                 (concat "\\("
                         hs-block-start-regexp
                         "\\)"
                         (if (and hs-hide-comments-when-hiding-all
                                  (stringp hs-c-start-regexp))
                             (concat "\\|\\("
                                     hs-c-start-regexp
                                     "\\)")
                           "")))))
       (while (funcall hs-find-next-block-function re (point-max)
                       hs-hide-comments-when-hiding-all)
         (if (match-beginning 1)
             ;; We have found a block beginning.
             (progn
               (goto-char (match-beginning 1))
	       (unless (if hs-hide-all-non-comment-function
			   (funcall hs-hide-all-non-comment-function)
			 (hs-hide-block-at-point t))
		 ;; Go to end of matched data to prevent from getting stuck
		 ;; with an endless loop.
                 (when (if (stringp hs-block-start-regexp)
                           (looking-at hs-block-start-regexp)
                         (eq (point) (match-beginning 0)))
		   (goto-char (match-end 0)))))
           ;; found a comment, probably
           (let ((c-reg (funcall hs-inside-comment-predicate)))
             (when (and c-reg (car c-reg))
               (if (hs-hideable-region-p (car c-reg) (nth 1 c-reg))
                   (hs-hide-block-at-point t c-reg)
                 (goto-char (nth 1 c-reg))))))
         (progress-reporter-update spew (point)))
       (progress-reporter-done spew)))
   (beginning-of-line)
   (run-hooks 'hs-hide-hook)))

(defun hs-show-all ()
  "Show everything then run `hs-show-hook'.  See `run-hooks'."
  (interactive)
  (hs-life-goes-on
   (message "Showing all blocks ...")
   (let ((hs-allow-nesting nil))
     (hs-discard-overlays (point-min) (point-max)))
   (message "Showing all blocks ... done")
   (run-hooks 'hs-show-hook)))

(defun hs-hide-block (&optional end)
  "Select a block and hide it.  With prefix arg, reposition at END.
Upon completion, point is repositioned and the normal hook
`hs-hide-hook' is run.  See documentation for `run-hooks'."
  (interactive "P")
  (hs-life-goes-on
   (let ((c-reg (funcall hs-inside-comment-predicate)))
     (cond
      ((and c-reg (or (null (nth 0 c-reg))
                      (not (hs-hideable-region-p (car c-reg) (nth 1 c-reg)))))
       (user-error "(not enough comment lines to hide)"))

      (c-reg (hs-hide-block-at-point end c-reg))

      ((save-excursion
         (and-let* ((_ (eq hs-hide-block-behavior 'after-bol))
                    (_ (goto-char (line-beginning-position)))
                    (pos (hs-get-first-block))
                    (_ (goto-char pos))
                    (_ (hs-hide-block-at-point end))))))

      ((or (funcall hs-looking-at-block-start-predicate)
           (and (goto-char (line-beginning-position))
                (funcall hs-find-block-beginning-function)))
       (hs-hide-block-at-point end)))

     (run-hooks 'hs-hide-hook))))

(defun hs-show-block (&optional end)
  "Select a block and show it.
With prefix arg, reposition at END.  Upon completion, point is
repositioned and the normal hook `hs-show-hook' is run.
See documentation for functions `hs-hide-block' and `run-hooks'."
  (interactive "P")
  (hs-life-goes-on
   (or
    ;; first see if we have something at the end of the line
    (let ((ov (hs-overlay-at (line-end-position)))
          (here (point))
          ov-start ov-end)
      (when ov
        (goto-char
         (cond (end (overlay-end ov))
               ((eq 'comment (overlay-get ov 'hs)) here)
               (t (+ (overlay-start ov) (overlay-get ov 'hs-b-offset)))))
        (setq ov-start (overlay-start ov))
        (setq ov-end   (overlay-end ov))
        (delete-overlay ov)
        (hs--refresh-indicators ov-start ov-end)
        t))
    ;; not immediately obvious, look for a suitable block
    (let ((c-reg (funcall hs-inside-comment-predicate))
          p q)
      (cond (c-reg
             (when (car c-reg)
               (setq p (car c-reg)
                     q (cadr c-reg))))
            ((and (funcall hs-find-block-beginning-function)
                  ;; ugh, fresh match-data
                  (funcall hs-looking-at-block-start-predicate))
             (setq p (point)
                   q (progn (hs-forward-sexp (match-data t) 1) (point)))))
      (when (and p q)
        (hs-discard-overlays p q)
        (goto-char (if end q (1+ p))))))
   (run-hooks 'hs-show-hook)))

(defun hs-hide-level (arg)
  "Hide all blocks ARG levels below this block.
The hook `hs-hide-hook' is run; see `run-hooks'."
  (interactive "p")
  (hs-life-goes-on
   (save-excursion
     (message "Hiding blocks ...")
     (hs-hide-level-recursive arg (point-min) (point-max))
     (message "Hiding blocks ... done"))
   (run-hooks 'hs-hide-hook)))

(defun hs-toggle-hiding (&optional e)
  "Toggle hiding/showing of a block.
See `hs-hide-block' and `hs-show-block'.
Argument E should be the event that triggered this action."
  (interactive (list last-nonmenu-event))
  (hs-life-goes-on
   (when e (posn-set-point (event-end e)))
   (if (hs-already-hidden-p)
       (hs-show-block)
     (hs-hide-block))))

(define-obsolete-function-alias
  'hs-mouse-toggle-hiding #'hs-toggle-hiding "27.1")

(defun hs-indicator-mouse-toggle-hiding (event)
  "Toggle block hiding with indicators."
  (interactive "e")
  (hs-life-goes-on
   (when hs-show-indicators
     (when (mouse-event-p event)
       (mouse-set-point event))
     (let* ((overlays (save-excursion
                        (goto-char (posn-point (event-end event)))
                        (overlays-in (pos-bol) (pos-eol))))
            (pos (catch 'hs--indicator-ov
                   (dolist (ov overlays)
                     (when-let* ((ov (overlay-get ov 'hs-indicator-block-start)))
                       (throw 'hs--indicator-ov ov))))))
       (when pos
         (goto-char pos)
         (hs-toggle-hiding))))))

(defun hs-hide-initial-comment-block ()
  "Hide the first block of comments in a file.
This can be useful if you have huge RCS logs in those comments."
  (interactive)
  (hs-life-goes-on
   (let ((c-reg (save-excursion
                  (goto-char (point-min))
                  (skip-chars-forward " \t\n\f")
                  (funcall hs-inside-comment-predicate))))
     (when c-reg
       (let ((beg (car c-reg)) (end (cadr c-reg)))
         ;; see if we have enough comment lines to hide
         (when (hs-hideable-region-p beg end)
           (hs-hide-comment-region beg end)))))))

;;;###autoload
(define-minor-mode hs-minor-mode
  "Minor mode to selectively hide/show code and comment blocks.

When hideshow minor mode is on, the menu bar is augmented with hideshow
commands and the hideshow commands are enabled.
The value (hs . t) is added to `buffer-invisibility-spec'.

The main commands are: `hs-hide-all', `hs-show-all', `hs-hide-block',
`hs-show-block', `hs-hide-level' and `hs-toggle-hiding'.  There is also
`hs-hide-initial-comment-block'.

Turning hideshow minor mode off reverts the menu bar and the
variables to default values and disables the hideshow commands.

Lastly, the normal hook `hs-minor-mode-hook' is run using `run-hooks'.

Key bindings:
\\{hs-minor-mode-map}"
  :group 'hideshow
  :lighter " hs"
  :keymap hs-minor-mode-map
  (setq hs-headline nil)

  (if hs-minor-mode
      (progn
        (unless (and comment-start comment-end)
          (setq hs-minor-mode nil)
          (user-error "%S doesn't support the Hideshow minor mode"
                      major-mode))

        ;; Set the variables
        (hs-grok-mode-type)
        ;; Turn off this mode if we change major modes.
        (add-hook 'change-major-mode-hook
                  #'turn-off-hideshow
                  nil t)
        (setq-local line-move-ignore-invisible t)
        (add-to-invisibility-spec '(hs . t))
        ;; Add block indicators
        (when (and hs-show-indicators
                   (or (and (integerp hs-indicator-maximum-buffer-size)
                            (< (buffer-size) hs-indicator-maximum-buffer-size))
                       (not hs-indicator-maximum-buffer-size)))
          (when (and (not (display-graphic-p))
                     (eq hs-indicator-type 'fringe))
            (setq-local hs-indicator-type 'margin))
          (jit-lock-register #'hs--add-indicators)))

    (remove-from-invisibility-spec '(hs . t))
    ;; hs-show-all does nothing unless h-m-m is non-nil.
    (let ((hs-minor-mode t))
      (hs-show-all))
    (when hs-show-indicators
      (jit-lock-unregister #'hs--add-indicators)
      (remove-overlays nil nil 'hs-indicator t))))

;;;###autoload
(defun turn-off-hideshow ()
  "Unconditionally turn off `hs-minor-mode'."
  (hs-minor-mode -1))

;;---------------------------------------------------------------------------
;; that's it

(provide 'hideshow)

;;; hideshow.el ends here
