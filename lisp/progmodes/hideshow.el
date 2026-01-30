;;; hideshow.el --- Minor mode to hide/show comment or code blocks  -*- lexical-binding:t -*-

;; Copyright (C) 1994-2026 Free Software Foundation, Inc.

;; Author: Thien-Thi Nguyen <ttn@gnu.org>
;;      Dan Nicolaescu <dann@gnu.org>
;; Maintainer: emacs-devel@gnu.org
;; Keywords: c tools outlines
;; Maintainer-Version: 6.0

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
;; This file provides the Hideshow minor mode, it includes the
;; following commands (and their keybindings) to hiding and showing
;; code and comment blocks:
;;
;;   `hs-hide-block'                      C-c @ C-h/C-d
;;   `hs-show-block'                      C-c @ C-s
;;   `hs-hide-all'                        C-c @ C-M-h/C-t
;;   `hs-show-all'                        C-c @ C-M-s/C-a
;;   `hs-hide-level'                      C-c @ C-l
;;   `hs-toggle-hiding'                   C-c @ C-c/C-e or S-<mouse-2>
;;   `hs-hide-initial-comment-block'
;;   `hs-cycle'                           C-c @ TAB
;;   `hs-toggle-all'                      C-c @ <backtab>
;;
;; All these commands are defined in `hs-prefix-map',
;; `hs-minor-mode-map' and `hs-indicators-map'.
;;
;; Blocks are defined per mode.  For example, in c-mode and similar,
;; they are simply text between curly braces, while in Lisp-ish modes
;; parens are used.  Multi-line comment blocks can also be hidden.
;; Read-only buffers are not a problem, since hideshow doesn't modify
;; the text.
;;
;; The command `M-x hs-minor-mode' toggles the minor mode or sets it
;; buffer-local.

;; * Suggested usage
;;
;; Add the following to your init file:
;;
;;     (add-hook 'X-mode-hook #'hs-minor-mode) ; other modes similarly
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

;; * Customization
;;
;; Hideshow provides the following user options:
;;
;; - `hs-hide-comments-when-hiding-all'
;;   If non-nil, `hs-hide-all', `hs-cycle' and `hs-hide-level' will hide
;;   comments too.
;; - `hs-hide-all-non-comment-function'
;;   If non-nil, after calling `hs-hide-all', this function is called
;;   with no arguments.
;; - `hs-isearch-open'
;;   What kind of hidden blocks to open when doing isearch.
;; - `hs-set-up-overlay'
;;   Function called with one arg (an overlay), intended to customize
;;   the block hiding appearance.
;; - `hs-display-lines-hidden'
;;   Displays the number of hidden lines next to the ellipsis.
;; - `hs-show-indicators'
;;   Display indicators to show and toggle the block hiding.
;; - `hs-indicator-type'
;;   Which indicator type should be used for the block indicators.
;; - `hs-indicator-maximum-buffer-size'
;;   Max buffer size in bytes where the indicators should be enabled.
;; - `hs-allow-nesting'
;;   If non-nil, hiding remembers internal blocks.
;; - `hs-cycle-filter'
;;   Control where typing a `TAB' cycles the visibility.
;;
;; The variable `hs-hide-all-non-comment-function' may be useful if you
;; only want to hide some N levels blocks for some languages/files or
;; implement your idea of what is more useful.  For example, the
;; following code shows the next nested level in addition to the
;; top-level for java:
;;
;;     (defun ttn-hs-hide-level-2 ()
;;       (when (funcall hs-looking-at-block-start-predicate)
;;         (hs-hide-level 2)))
;;     (add-hook 'java-mode-hook
;;               (lambda ()
;;                 (setq-local hs-hide-all-non-comment-function
;;                             #'ttn-hs-hide-level-2)))
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
;;
;; The following hooks are run after some commands:
;;
;;   hs-hide-hook  =>  hs-hide-block hs-hide-all hs-hide-level hs-cycle
;;   hs-show-hook  =>  hs-show-block hs-show-all hs-cycle
;;
;; The variable `hs-set-up-overlay' allow customize the appearance of
;; the hidden block and other effects associated with overlays.  For
;; example:
;;
;;     (setopt hs-set-up-overlay
;;             (defun my-display-code-line-counts (ov)
;;               (when (eq 'code (overlay-get ov 'hs))
;;                 (overlay-put ov 'display
;;                              (propertize
;;                               (format " [... <%d>] "
;;                                       (count-lines (overlay-start ov)
;;                                                    (overlay-end ov)))
;;                               'face 'font-lock-type-face)))))

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
;; A (code) block is defined as text surrounded by
;; `hs-block-start-regexp' and `hs-block-end-regexp'.
;;
;; For some major modes, forward-sexp does not work properly.  In those
;; cases, `hs-forward-sexp-function' specifies another function to use
;; instead.

;; *** Non-regexp matching
;;
;; By default, Hideshow uses regular expressions to match blocks.  For
;; something more advanced than regexp is necessary to modify these
;; variables (see their docstring):
;; - `hs-forward-sexp-function'
;; - `hs-find-block-beginning-function'
;; - `hs-find-next-block-function'
;; - `hs-looking-at-block-start-predicate'
;; - `hs-inside-comment-predicate' (For comments)
;; - `hs-block-end-regexp' (Preferably, this should be set to nil)
;;
;; *** Tree-sitter support
;;
;; All the treesit based modes already have support for hiding/showing
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
;; 1) Sometimes `hs-headline' can become out of sync.  To reset, type
;;    `M-x hs-minor-mode' twice (that is, deactivate then re-activate
;;    hideshow).
;;
;; 2) Some buffers can't be `byte-compile-file'd properly.  This is because
;;    `byte-compile-file' inserts the file to be compiled in a temporary
;;    buffer and switches `normal-mode' on.  In the case where you have
;;    `hs-hide-initial-comment-block' in `hs-minor-mode-hook', the hiding of
;;    the initial comment sometimes hides parts of the first statement (seems
;;    to be only in `normal-mode'), so there are unbalanced parenthesis.
;;
;;    The workaround is to clear `hs-minor-mode-hook' when byte-compiling:
;;
;;    (define-advice byte-compile-file (:around
;;                                      (fn &rest rest)
;;                                      byte-compile-file-hideshow-off)
;;      (let (hs-minor-mode-hook)
;;        (apply #'fn rest)))
;;
;; 3) Hideshow interacts badly with Ediff and `vc-diff'.  At the moment, the
;;    suggested workaround is to turn off hideshow entirely, for example:
;;
;;    (add-hook 'ediff-prepare-buffer-hook #'turn-off-hideshow)
;;    (add-hook 'vc-before-checkin-hook #'turn-off-hideshow)
;;
;;    In the case of `vc-diff', here is a less invasive workaround:
;;
;;    (add-hook 'vc-before-checkin-hook
;;              (lambda ()
;;                (goto-char (point-min))
;;                (hs-show-block)))
;;
;;    Unfortunately, these workarounds do not restore hideshow state.

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

;; * History (author commentary)
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


;;;; Libraries

(require 'mule-util) ; For `truncate-string-ellipsis'
;; For indicators
(require 'icons)
(require 'fringe)


(defgroup hideshow nil
  "Minor mode for hiding and showing program and comment blocks."
  :prefix "hs-"
  :group 'languages)

;;;; Faces

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

;;;; Options

(defcustom hs-hide-hook nil
  "Hook called (with `run-hooks') at the end of commands to hide text.
These commands include the toggling commands (when the result is to hide
a block), `hs-hide-all', `hs-hide-block' and `hs-hide-level'."
  :type 'hook
  :version "31.1")

(defcustom hs-show-hook nil
  "Hook called (with `run-hooks') at the end of commands to show text.
These commands include the toggling commands (when the result is to show
a block), `hs-show-all' and `hs-show-block'."
  :type 'hook
  :version "31.1")

(defcustom hs-hide-comments-when-hiding-all t
  "Whether the comments should be hidden.
If non-nil, `hs-hide-all', `hs-cycle' and `hs-hide-level' will hide
comments too."
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

(defcustom hs-allow-nesting nil
  "If non-nil, hiding remembers internal blocks.
This means that when the outer block is shown again,
any previously hidden internal blocks remain hidden."
  :type 'boolean
  :version "31.1")

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
major mode, elsewhere on the headlines.
Currently it affects only the command `hs-toggle-hiding' by default,
but it can be easily replaced with the command `hs-cycle'."
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

;;;; Icons

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


;;;; Keymaps

(defvar-keymap hs-prefix-map
  :doc "Keymap for hideshow commands."
  :prefix t
  ;; These bindings roughly imitate those used by Outline mode.
  "C-h"       #'hs-hide-block
  "C-d"       #'hs-hide-block
  "C-s"       #'hs-show-block
  "C-M-h"     #'hs-hide-all
  "C-t"       #'hs-hide-all
  "C-M-s"     #'hs-show-all
  "C-a"       #'hs-show-all
  "C-l"       #'hs-hide-level
  "C-c"       #'hs-toggle-hiding
  "C-e"       #'hs-toggle-hiding
  "TAB"       #'hs-cycle
  "<backtab>" #'hs-toggle-all)

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
                          (forward-line 0)
                          (hs-get-first-block-on-line))
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
     :help "\
If t also hide comment blocks when doing `hs-hide-all', `hs-cycle' or `hs-hide-level'"
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


;;;; Internal variables

(defvar hs-minor-mode)

(defvar hs-hide-all-non-comment-function nil
  "Function called if non-nil when doing `hs-hide-all' for non-comments.")

(defvar hs-headline nil
  "Text of the line where a hidden block begins, set during isearch.
You can display this in the mode line by adding the symbol `hs-headline'
to the variable `mode-line-format'.  For example:

  (unless (memq \\='hs-headline mode-line-format)
    (setq mode-line-format
          (append \\='(\"-\" hs-headline) mode-line-format)))

Note that `mode-line-format' is buffer-local.")

;; Used in `hs-toggle-all'
(defvar-local hs--toggle-all-state)


;;;; API variables

;;;###autoload
(defvar hs-special-modes-alist nil)
(make-obsolete-variable
 'hs-special-modes-alist
 "use the buffer-local variables instead" "31.1")

(defvar-local hs-block-start-regexp "\\s("
  "Regexp for beginning of block.")

(defvar-local hs-block-start-mdata-select 0
  "Element in `hs-block-start-regexp' match data to consider as block start.
This is used by `hs-block-positions' to move point to the beginning of
this element (using `match-beginning') before calling
`hs-forward-sexp-function'.

This is used for regexp matching.")

(defvar-local hs-block-end-regexp "\\s)"
  "Regexp for end of block.
This is mostly used to determine if point is at the end of the block.

As a special case, it can be nil (to use the position from
`hs-forward-sexp-function'), or a function without arguments.  If it's a
function, it should return non-nil if point is at end of a block, and
set `match-data' to that position.")

(defvar-local hs-c-start-regexp nil
  "Regexp for beginning of comments.
If not bound, Hideshow will use current `comment-start' value without
any trailing whitespace.")

(define-obsolete-variable-alias
  'hs-forward-sexp-func
  'hs-forward-sexp-function "31.1")

(defvar-local hs-forward-sexp-function #'forward-sexp
  "Function used to reposition point to the end of the region to hide.
For backward compatibility, the function is called with one argument,
which can be ignored.

The function is called in front of the beginning of the block (usually the
current value of `hs-block-start-regexp' in the buffer) and should
reposition point to the end of the block.")

(define-obsolete-variable-alias
  'hs-adjust-block-beginning
  'hs-adjust-block-beginning-function "31.1")

(defvar-local hs-adjust-block-beginning-function nil
  "Function used to tweak the block beginning.
It is called at the beginning of the block (usually the current value of
`hs-block-start-regexp' in the buffer) and should return the start
position of the region in the buffer that will be hidden.

It is called with a single argument ARG which is the position in
buffer after the block beginning.")

(defvar-local hs-adjust-block-end-function nil
  "Function used to tweak the block end.
It is called at the end of the block with one argument, the start
position of the region in the buffer that will be hidden.  It should
return either the last position to hide or nil.  If it returns nil,
Hideshow will guess the end position.

This is useful to ensure some characters such as parenthesis or curly
braces get properly hidden in modes without parenthesis pairs
delimiters (such as python).")

(define-obsolete-variable-alias
  'hs-find-block-beginning-func
  'hs-find-block-beginning-function
  "31.1")

(defvar-local hs-find-block-beginning-function
  #'hs-find-block-beg-fn--default
  "Function used to reposition point at the beginning of current block.
If it finds the block beginning, it should reposition point there and
return non-nil, otherwise it should return nil.")

(define-obsolete-variable-alias
  'hs-find-next-block-func
  'hs-find-next-block-function
  "31.1")

(defvar-local hs-find-next-block-function
  #'hs-find-next-block-fn--default
  "Function to find the start of the next block.
It should reposition point at next block start.

It is called with three arguments REGEXP, BOUND, and COMMENTS.

REGEXP is a regexp representing block start.  When block start is found,
should set the match data according to the beginning position of the
matched REGEXP or block start position.

BOUND is a buffer position that limits the search.

When COMMENTS is non-nil, REGEXP matches not only beginning of a block
but also beginning of a comment.  In this case, the function should find
the nearest block or comment and return non-nil.")

(define-obsolete-variable-alias
  'hs-looking-at-block-start-p-func
  'hs-looking-at-block-start-predicate
  "31.1")

(defvar-local hs-looking-at-block-start-predicate
  #'hs-looking-at-block-start-p--default
  "Function used to check if point is at the block start.
It should return non-nil if point is at the block start and modify the
match data to the block beginning start and end positions (specifically,
for `match-end').")

(defvar-local hs-inside-comment-predicate #'hs-inside-comment-p--default
  "Function used to get comment positions.
If point is inside a comment, the function should return a list
containing the buffer position of the start and the end of the
comment, otherwise it should return nil.")

(defvar-local hs-treesit-things 'list
  "Treesit things to check if point is at a valid block.
The value should be a thing defined in `treesit-thing-settings' for the
current buffer's major mode.")


;;;; API functions

(defmacro hs-life-goes-on (&rest body)
  "Evaluate BODY forms if variable `hs-minor-mode' is non-nil.
In the dynamic context of this macro, `case-fold-search' is t.

This macro encloses BODY in `save-match-data' and `save-excursion'.

Intended to be used for commands."
  (declare (debug t))
  `(when hs-minor-mode
     (let ((case-fold-search t))
       (save-match-data
         (save-excursion ,@body)))))

(defun hs-discard-overlays (beg end)
  "Delete hideshow overlays in region defined by BEG and END.
Skip \"internal\" overlays if `hs-allow-nesting' is non-nil."
  (when (< end beg)
    (setq beg (prog1 end (setq end beg))))
  (if hs-allow-nesting
      (let ((beg beg))
        (while (> end (setq beg (next-overlay-change beg)))
          (when-let* ((ov (hs-overlay-at beg)))
            ;; Reposition point to the end of the overlay, so we avoid
            ;; removing the nested overlays too.
            (setq beg (overlay-end ov))
            (delete-overlay ov))))
    (remove-overlays beg end 'invisible 'hs))
  (hs--refresh-indicators beg end))

(defun hs-overlay-at (position)
  "Return hideshow overlay at POSITION, or nil if none to be found."
  (seq-find
   (lambda (ov) (overlay-get ov 'hs))
   (overlays-at position)))

(defun hs-hideable-region-p (beg end)
  "Return t if region between BEG and END can be hidden."
  ;; Check if BEG and END are not in the same line number,
  ;; since using `count-lines' is slow.
  (and beg end
       (< beg (save-excursion (goto-char end) (pos-bol)))))

(defun hs-already-hidden-p ()
  "Return non-nil if point is in an already-hidden block, otherwise nil."
  (save-excursion
    ;; Reposition point if it is inside a comment, and if that comment
    ;; is hideable
    (when-let* ((c-reg (funcall hs-inside-comment-predicate)))
      (goto-char (car c-reg)))
    ;; Search for a hidden block at EOL ...
    (eq 'hs
        (or (get-char-property (pos-eol) 'invisible)
            ;; ... or behind the current cursor position
            (get-char-property (if (bobp) (point) (1- (point)))
                               'invisible)))))

(defun hs-block-positions (&optional adjust-beg adjust-end)
  "Return the current code block positions.
This returns a list with the current code block beginning and end
positions.  This does nothing if there is not a code block at current
point.

If either ADJUST-BEG or ADJUST-END are non-nil, adjust block positions
according to `hs-adjust-block-beginning', `hs-adjust-block-end-function'
and `hs-block-end-regexp'.

This is for code block positions only, for comments use
`hs-inside-comment-predicate'."
  ;; `catch' is used here if the search fails due unbalanced parentheses
  ;; or any other unknown error caused in `hs-forward-sexp-function'.
  (catch 'hs--block-exit
    (save-match-data
      (save-excursion
        (when (funcall hs-looking-at-block-start-predicate)
          (let* ((beg (match-end 0)) end)
            ;; `beg' is the point at the block beginning, which may need
            ;; to be adjusted
            (when adjust-beg
              (setq beg (pos-eol))
              (save-excursion
                (when hs-adjust-block-beginning-function
                  (goto-char (funcall hs-adjust-block-beginning-function beg)))))

            (goto-char (match-beginning hs-block-start-mdata-select))
            (condition-case _
                (funcall hs-forward-sexp-function 1)
              (scan-error (throw 'hs--block-exit nil)))
            ;; `end' is the point at the end of the block
            (setq end (cond ((not adjust-end) (point))
                            ((and (stringp hs-block-end-regexp)
                                  (looking-back hs-block-end-regexp nil))
                             (match-beginning 0))
                            ((functionp hs-block-end-regexp)
                             (funcall hs-block-end-regexp)
                             (match-beginning 0))
                            (t (point))))
            ;; adjust block end (if needed)
            (when (and adjust-end hs-adjust-block-end-function)
              (setq end (or (funcall hs-adjust-block-end-function beg)
                            end)))
            (list beg end)))))))

(defun hs-hide-comment-region (beg end &optional _repos-end)
  "Hide a region from BEG to END, marking it as a comment.
Optional arg REPOS-END means reposition at end."
  (declare (obsolete "Use `hs-hide-block-at-point' instead." "31.1"))
  (hs-hide-block-at-point (list beg end)))

(defun hs-hide-block-at-point (&optional comment-reg)
  "Hide block if on block beginning.
Optional arg COMMENT-REG is a list of the form (BEGIN END) and
specifies the limits of the comment, or nil if the block is not
a comment.

If hiding the block is successful, return non-nil.
Otherwise, return nil."
  (when-let* ((block (or comment-reg (hs-block-positions :a-beg :a-end))))
    (let ((beg (if comment-reg (save-excursion (goto-char (car block)) (pos-eol))
                 (car block)))
          (end (cadr block))
          ov)
      (if (hs-hideable-region-p beg end)
          (progn
            (cond (comment-reg (let (hs-allow-nesting)
                                 (hs-discard-overlays beg end)))
                  ((and hs-allow-nesting (setq ov (hs-overlay-at beg)))
                   (delete-overlay ov))
                  ((not hs-allow-nesting)
                   (hs-discard-overlays beg end)))
            (goto-char end)
            (hs-make-overlay beg end (if comment-reg 'comment 'code)))
        (when comment-reg (goto-char end))
        nil))))

(defun hs-get-first-block-on-line (&optional include-comments)
  "Reposition point to the first valid block found on the current line.
This searches for a valid block from current point to the end of current
line and returns the start position of the first block found.
Otherwise, if no block is found, it returns nil.

If INCLUDE-COMMENTS is non-nil, also search for a comment block."
  (let ((bk-point (point))
        (regexp (if include-comments
                    (concat "\\(" hs-block-start-regexp "\\)"
                            "\\|\\(" hs-c-start-regexp "\\)")
                  hs-block-start-regexp))
        exit)
    (while (and (not exit)
                (funcall hs-find-next-block-function regexp (pos-eol) include-comments)
                (save-excursion
                  (goto-char (match-beginning 0))
                  (pcase-let ((`(,beg ,end)
                               (or (and include-comments
                                        (funcall hs-inside-comment-predicate))
                                   (hs-block-positions))))
                    (if (and beg (hs-hideable-region-p beg end))
                        (setq exit (point))
                      t)))))
    (unless exit (goto-char bk-point))
    exit))

(defun hs-get-near-block (&optional include-comment)
  "Reposition point to a near block around point.
It search for a valid block before and after point and return t if one
is found.

If INCLUDE-COMMENT is non-nil, it also searches for a comment block,
returning `comment' if one is found.

Intended to be used in commands."
  (let ((c-reg (when include-comment (funcall hs-inside-comment-predicate)))
        pos)
    (cond
     ((and c-reg (apply #'hs-hideable-region-p c-reg))
      (goto-char (car c-reg))
      'comment)

     ((and (eq hs-hide-block-behavior 'after-bol)
           (save-excursion
             (forward-line 0)
             (setq pos (hs-get-first-block-on-line))))
      (goto-char pos)
      t)

     ((and (or (funcall hs-looking-at-block-start-predicate)
               (and (forward-line 0)
                    (funcall hs-find-block-beginning-function)))
           (apply #'hs-hideable-region-p (hs-block-positions)))
      t))))

(defun hs-hide-level-recursive (arg beg end &optional include-comments func progress)
  "Recursively hide blocks between BEG and END that are ARG levels below point.
If INCLUDE-COMMENTS is non-nil, also hide recursive comment blocks.  If
FUNC is non-nil, call this function to hide the block instead.  If
PROGRESS is non-nil, also update a progress object, intended for
commands."
  ;; Show all blocks in that region
  (unless hs-allow-nesting (hs-discard-overlays beg end))
  (goto-char beg)
  (while (not (>= (point) end))
    (when-let* ((_ (not (invisible-p (point)))) ; Skip invisible lines
                (b-start (hs-get-first-block-on-line include-comments)))
      (goto-char b-start)
      (let ((comment (and include-comments (funcall hs-inside-comment-predicate)))
            (code (hs-block-positions)))
        ;; Find a block recursively according to ARG.
        (if (> arg 1)
            ;; Nested comment blocks in a comment block are impossible,
            ;; so skip them.
            (if comment
                (goto-char (cadr comment))
              (pcase-let ((`(,beg ,end) code))
                (hs-hide-level-recursive (1- arg) beg end include-comments)))
          ;; Now hide the block we found.
          (if func (funcall func) (hs-hide-block-at-point comment))
          (when progress (progress-reporter-update progress (point))))))
    (forward-line 1))
  (goto-char end))


;;;; Internal functions

(defun hs--discard-overlay-before-changes (o &rest _r)
  "Remove overlay O before changes.
Intended to be used in `modification-hooks', `insert-in-front-hooks' and
`insert-behind-hooks'."
  (let ((beg (overlay-start o))
        (end (overlay-end o)))
    (delete-overlay o)
    (hs--refresh-indicators beg end)))

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

(defun hs-make-overlay (b e kind)
  "Return a new overlay in region defined by B and E with type KIND.
KIND is either `code' or `comment'.  The following properties are set in
the overlay: `invisible' `hs'.  Also, depending on variable
`hs-isearch-open', the following properties may be present:
`isearch-open-invisible' `isearch-open-invisible-temporary'."
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
    ;; Isearch integration
    (when (or (eq io t) (eq io kind))
      (overlay-put ov 'isearch-open-invisible 'hs-isearch-show)
      (overlay-put ov 'isearch-open-invisible-temporary
                   'hs-isearch-show-temporary))
    ;; Remove overlay after modifications
    (overlay-put ov 'modification-hooks    '(hs--discard-overlay-before-changes))
    (overlay-put ov 'insert-in-front-hooks '(hs--discard-overlay-before-changes))
    (overlay-put ov 'insert-behind-hooks   '(hs--discard-overlay-before-changes))

    (when hs-set-up-overlay (funcall hs-set-up-overlay ov))
    (hs--refresh-indicators b (1+ b))
    ov))

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
  (setq beg (progn (goto-char beg) (pos-bol))
        end (progn (goto-char end)
                   ;; Include the EOL indicator positions
                   (min (1+ (pos-eol)) (point-max))))
  (goto-char beg)
  (remove-overlays beg end 'hs-indicator t)

  (while (not (>= (point) end))
    (when-let* ((_ (not (invisible-p (point)))) ; Skip invisible lines
                (b-beg (hs-get-first-block-on-line)))
      (hs--make-indicators-overlays b-beg))
    ;; Only 1 indicator per line
    (forward-line))
  `(jit-lock-bounds ,beg . ,end))

(defun hs--refresh-indicators (from to)
  "Update indicator appearance in FROM and TO."
  (when (and hs-show-indicators hs-minor-mode)
    (save-match-data
      (save-excursion
        (hs--add-indicators from to)))))

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
        (unless hide-p
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
        (overlay-put ov 'display nil)
        (overlay-put ov 'hs-isearch-display value))))
  (overlay-put ov 'invisible (and hide-p 'hs)))

(defun hs-looking-at-block-start-p--default ()
  "Return non-nil if point is at the block start."
  (and (looking-at hs-block-start-regexp)
       (save-match-data (not (nth 8 (syntax-ppss))))))

(defun hs-inside-comment-p ()
  (declare (obsolete "Call `hs-inside-comment-predicate' instead." "31.1"))
  (funcall hs-inside-comment-predicate))

(defun hs-inside-comment-p--default ()
  (save-excursion
    ;; the idea is to look backwards for a comment start regexp, do a
    ;; forward comment, and see if we are inside, then extend
    ;; forward and backward as long as we have comments
    (let ((amount (buffer-size))
          (rx (concat "^[[:blank:]]*\\(" hs-c-start-regexp "\\)"))
          beg end)
      (when (or (and (skip-chars-forward "[:blank:]")
                     (looking-at-p hs-c-start-regexp)
                     ;; Check if there are not whitespaces before the comment
                     (if (save-excursion
                           (forward-line 0) (not (looking-at-p rx)))
                         (setq amount 1)
                       t))
                (and (re-search-backward rx (pos-bol) t)
                     (goto-char (match-beginning 1))))

        (setq beg (if (= amount 1)
                      (pos-eol)
                    (forward-comment (- amount))
                    (skip-chars-forward " \t\n\f")
                    (unless (save-excursion
                              (forward-line 0) (looking-at-p rx))
                      (forward-comment 1)
                      (skip-chars-forward " \t\n\f"))
                    (pos-eol))
              end (progn (forward-comment amount)
                         (skip-chars-backward " \t\n\f")
                         (point)))
        (list beg end)))))

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

(defun hs-forward-sexp (match-data _arg)
  "Adjust point based on MATCH-DATA and call `hs-forward-sexp-function' with ARG.
Original match data is restored upon return."
  (declare (obsolete "Use `hs-block-positions' instead." "31.1"))
  (save-match-data
    (set-match-data match-data)
    (goto-char (match-beginning hs-block-start-mdata-select))
    (funcall hs-forward-sexp-function 1)))

(define-obsolete-function-alias
  'hs-find-next-block 'hs-find-next-block-fn--default "31.1")

(defun hs-find-next-block-fn--default (regexp bound comments)
  "Reposition point at next block-start.
Skip comments if COMMENTS is nil, and search for REGEXP in
region (point BOUND)."
  (when (not comments)
    (forward-comment (point-max)))
  (and (< (point) bound)
       (re-search-forward regexp bound t)))

(define-obsolete-function-alias
  'hs-find-block-beginning 'hs-find-block-beg-fn--default "31.1")

(defun hs-find-block-beg-fn--default ()
  "Reposition point at block-start.
Return point, or nil if original point was not in a block."
  (let ((here (point)) done)
    ;; look if current line is block start
    (if (funcall hs-looking-at-block-start-predicate)
        here
      ;; look backward for the start of a block that contains the cursor
      (save-excursion
        (while (and (re-search-backward hs-block-start-regexp nil t)
                    (goto-char (match-beginning 0))
		    ;; go again if in a comment or a string
		    (or (save-match-data (nth 8 (syntax-ppss)))
		        (not (setq done (and (<= here (cadr (hs-block-positions)))
                                             (point))))))))
      (when done (goto-char done)))))

;; This function is not used anymore (Bug#700).
(defun hs-c-like-adjust-block-beginning (initial)
  "Adjust INITIAL, the buffer position after `hs-block-start-regexp'.
Actually, point is never moved; a new position is returned that is
the end of the C-function header.  This adjustment function is meant
to be assigned to `hs-adjust-block-beginning-function' for C-like modes."
  (declare (obsolete "Use `hs-adjust-block-beginning-function' instead." "31.1"))
  (save-excursion
    (goto-char (1- initial))
    (forward-comment (- (buffer-size)))
    (point)))

;;;###autoload
(defun turn-off-hideshow ()
  "Unconditionally turn off `hs-minor-mode'."
  (hs-minor-mode -1))


;;;; Commands

(defun hs-hide-all ()
  "Hide all top level blocks.
This command runs `hs-hide-hook'.
If `hs-hide-comments-when-hiding-all' is non-nil, also hide the
comments."
  (interactive)
  (hs-life-goes-on
   (let ((spew (make-progress-reporter
                "Hiding all blocks..." (point-min) (point-max))))
     (hs-hide-level-recursive
      1 (point-min) (point-max)
      hs-hide-comments-when-hiding-all
      hs-hide-all-non-comment-function
      spew)
     (progress-reporter-done spew))
   (run-hooks 'hs-hide-hook)))

(defun hs-show-all ()
  "Show everything then run `hs-show-hook'.  See `run-hooks'."
  (interactive)
  (hs-life-goes-on
   (message "Showing all blocks ...")
   (let (hs-allow-nesting)
     (hs-discard-overlays (point-min) (point-max)))
   (message "Showing all blocks ... done")
   (run-hooks 'hs-show-hook)))

(defun hs-hide-block ()
  "Select a block and hide it.
This command runs `hs-hide-hook'."
  (interactive)
  (hs-life-goes-on
   (let ((c-reg (funcall hs-inside-comment-predicate)))
     (cond
      ((and c-reg (not (apply #'hs-hideable-region-p c-reg)))
       (user-error "(not enough comment lines to hide)"))
      ((or c-reg (hs-get-near-block))
       (hs-hide-block-at-point c-reg)))
     (run-hooks 'hs-hide-hook))))

(defun hs-show-block ()
  "Select a block and show it.
This command runs `hs-show-hook'.  See documentation for functions
`hs-hide-block' and `run-hooks'."
  (interactive)
  (hs-life-goes-on
   (if-let* ((ov (hs-overlay-at (pos-eol)))
             (ov-start (overlay-start ov))
             (ov-end (overlay-end ov)))
       (progn
         (hs-discard-overlays (1- ov-start) ov-end)
         (hs--refresh-indicators ov-start ov-end))
     (when-let* ((block
                  (or (funcall hs-inside-comment-predicate)
                      (and (funcall hs-find-block-beginning-function)
                           (hs-block-positions)))))
       (hs-discard-overlays (car block) (cadr block))))
   (run-hooks 'hs-show-hook)))

(defun hs-hide-level (arg)
  "Hide all blocks ARG levels below this block.
If point is not in a block, hide all the ARG levels blocks in the whole
buffer.

The hook `hs-hide-hook' is run; see `run-hooks'."
  (interactive "p")
  (hs-life-goes-on
   (save-excursion
     (message "Hiding blocks ...")
     (if (hs-get-near-block)
         ;; Hide block if we are looking at one.
         (pcase-let ((`(,beg ,end) (hs-block-positions)))
           (hs-hide-level-recursive arg beg end hs-hide-comments-when-hiding-all))
       ;; Otherwise hide all the blocks in the current buffer
       (hs-hide-level-recursive
        ;; Increment ARG by 1, avoiding it acts like
        ;; `hs-hide-all'
        (1+ arg) (point-min) (point-max)
        hs-hide-comments-when-hiding-all))
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
   (goto-char (point-min))
   (skip-chars-forward " \t\n\f")
   (when-let* ((c-reg (funcall hs-inside-comment-predicate)))
     (hs-hide-block-at-point c-reg))))

(defun hs-cycle (&optional level)
  "Cycle the visibility state of the current block.
This cycles the visibility of the current block between hide the parent
block, hide the nested blocks only, and show the parent and nested
blocks.

If LEVEL is specified (interactively, the prefix numeric argument), hide
only blocks which are that many levels below the level of point."
  (interactive "p")
  (hs-life-goes-on
   (when-let* ((ret (hs-get-near-block :include-comments)))
     (cond ((eq ret 'comment)
            (hs-toggle-hiding)
            (message "Toggle visibility"))
           ((> level 1)
            (pcase-let ((`(,beg ,end) (hs-block-positions)))
              (hs-hide-level-recursive
               level beg end hs-hide-comments-when-hiding-all))
            (message "Hide %d level" level))
           (t
            (let* (hs-allow-nesting
                   (block (hs-block-positions :ad-beg :ad-end))
                   (ov (seq-find
                        (lambda (o)
                          (and (eq (overlay-get o 'invisible) 'hs)))
                        (overlays-in (car block) (cadr block)))))
              (cond
               ;; Hide all if there are no hidden blocks
               ((not ov)
                (hs-hide-block)
                (message "Hide block and nested blocks"))
               ;; Hide the children blocks if the parent block is hidden
               ((and (= (overlay-start ov) (car block))
                     (= (overlay-end ov) (cadr block)))
                (hs-hide-level-recursive
                 1 (car block) (cadr block)
                 hs-hide-comments-when-hiding-all)
                (message "Hide first nested blocks"))
               ;; Otherwise show all in the parent block, we cannot use
               ;; `hs-show-block' here because we already know the
               ;; positions.
               (ov (hs-discard-overlays (car block) (cadr block))
                   (message "Show block and nested blocks")
                   (run-hooks 'hs-show-hook)))))))))

(defun hs-toggle-all ()
  "Hide or show all the blocks in the current buffer."
  (interactive)
  (if hs--toggle-all-state
      (let (hs-allow-nesting)
        (hs-discard-overlays (point-min) (point-max)))
    (hs-hide-all))
  (setq-local hs--toggle-all-state (not hs--toggle-all-state)))

;;;###autoload
(define-minor-mode hs-minor-mode
  "Minor mode to selectively hide/show code and comment blocks.

When hideshow minor mode is on, the menu bar is augmented with hideshow
commands and the hideshow commands are enabled.
The value (hs . t) is added to `buffer-invisibility-spec'.

Turning hideshow minor mode off reverts the menu bar and the
variables to default values and disables the hideshow commands.

Lastly, the normal hook `hs-minor-mode-hook' is run using `run-hooks'.

Key bindings:
\\{hs-minor-mode-map}"
  :group 'hideshow
  :lighter " hs"
  :keymap hs-minor-mode-map
  (setq hs-headline nil)

  (cond
   ((and hs-minor-mode
         (not (and comment-start comment-end)))
    (setq hs-minor-mode nil)
    (message "%S doesn't support the Hideshow minor mode"
             major-mode))

   (hs-minor-mode
    ;; Set the old variables
    (hs-grok-mode-type)
    ;; Turn off this mode if we change major modes.
    (add-hook 'change-major-mode-hook
              #'turn-off-hideshow nil t)
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
      (when (eq hs-indicator-type 'margin)
        (setq-local left-margin-width (1+ left-margin-width))
        (setq-local fringes-outside-margins t)
        ;; Force display of margins
        (when (eq (current-buffer) (window-buffer))
          (set-window-buffer nil (window-buffer))))
      (jit-lock-register #'hs--add-indicators)))

   (t
    (remove-from-invisibility-spec '(hs . t))
    (remove-overlays nil nil 'hs-indicator t)
    (remove-overlays nil nil 'invisible 'hs)
    (when hs-show-indicators
      (jit-lock-unregister #'hs--add-indicators)
      (when (and (eq hs-indicator-type 'margin)
                 (< 0 left-margin-width))
        (setq-local left-margin-width (1- left-margin-width))
        (kill-local-variable 'fringes-outside-margins)
        ;; Force removal of margins
        (when (eq (current-buffer) (window-buffer))
          (set-window-buffer nil (window-buffer))))))))


;;;; that's it
(provide 'hideshow)
;;; hideshow.el ends here
