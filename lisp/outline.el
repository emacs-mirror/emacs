;;; outline.el --- outline mode commands for Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 1986-2026 Free Software Foundation, Inc.

;; Maintainer: emacs-devel@gnu.org
;; Keywords: outlines

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

;; This package is a major mode for editing outline-format documents.
;; An outline can be `abstracted' to show headers at any given level,
;; with all stuff below hidden.  See the Emacs manual for details.

;;; Todo:

;; - subtree-terminators
;; - better handle comments before function bodies (i.e. heading)
;; - don't bother hiding whitespace

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))
(require 'icons)

(defgroup outlines nil
  "Support for hierarchical outlining."
  :prefix "outline-"
  :group 'text)

(defvar outline-regexp "[*\^L]+"
  "Regular expression to match the beginning of a heading.
Any line whose beginning matches this regexp is considered to start a heading.
Note that Outline mode only checks this regexp at the start of a line,
so the regexp need not (and usually does not) start with `^'.
The recommended way to set this is with a Local Variables: list
in the file it applies to.  See also `outline-heading-end-regexp'.")
;;;###autoload(put 'outline-regexp 'safe-local-variable 'stringp)

(defvar outline-heading-end-regexp "\n"
  "Regular expression to match the end of a heading line.
You can assume that point is at the beginning of a heading when this
regexp is searched for.  The heading ends at the end of the match.
The recommended way to set this is with a `Local Variables:' list
in the file it applies to.")
;;;###autoload(put 'outline-heading-end-regexp 'safe-local-variable 'stringp)

(defvar outline-search-function nil
  "Function to search the next outline heading.
The function is called with four optional arguments: BOUND, MOVE, BACKWARD,
LOOKING-AT.  The first two arguments BOUND and MOVE are almost the same as
the BOUND and NOERROR arguments of `re-search-forward', with the difference
that MOVE accepts only a boolean, either nil or non-nil.  When the argument
BACKWARD is non-nil, the search should search backward like
`re-search-backward' does.  In case of a successful search, the
function should return non-nil, move point, and set match-data
appropriately.  When the argument LOOKING-AT is non-nil, it should
imitate the function `looking-at'.")

(defvar-keymap outline-mode-prefix-map
  "@"   #'outline-mark-subtree
  "C-n" #'outline-next-visible-heading
  "C-p" #'outline-previous-visible-heading
  "C-i" #'outline-show-children
  "C-s" #'outline-show-subtree
  "C-d" #'outline-hide-subtree
  "C-u" #'outline-up-heading
  "C-f" #'outline-forward-same-level
  "C-b" #'outline-backward-same-level
  "C-t" #'outline-hide-body
  "C-a" #'outline-show-all
  "C-c" #'outline-hide-entry
  "C-e" #'outline-show-entry
  "C-l" #'outline-hide-leaves
  "C-k" #'outline-show-branches
  "C-q" #'outline-hide-sublevels
  "C-o" #'outline-hide-other
  "C-^" #'outline-move-subtree-up
  "C-v" #'outline-move-subtree-down
  "/ s" #'outline-show-by-heading-regexp
  "/ h" #'outline-hide-by-heading-regexp
  "C-<" #'outline-promote
  "C->" #'outline-demote
  "RET" #'outline-insert-heading)

(defvar outline-mode-menu-bar-map
  (let ((map (make-sparse-keymap)))
    (define-key map [hide] (cons "Hide" (make-sparse-keymap "Hide")))
    (define-key map [hide outline-hide-other]
      '(menu-item "Hide Other" outline-hide-other
		  :help "Hide everything except current body and parent and top-level headings"))
    (define-key map [hide outline-hide-sublevels]
      '(menu-item "Hide Sublevels" outline-hide-sublevels
		  :help "Hide everything but the top LEVELS levels of headers, in whole buffer"))
    (define-key map [hide outline-hide-subtree]
      '(menu-item "Hide Subtree" outline-hide-subtree
		  :help "Hide everything after this heading at deeper levels"))
    (define-key map [hide outline-hide-entry]
      '(menu-item "Hide Entry" outline-hide-entry
		  :help "Hide the body directly following this heading"))
    (define-key map [hide outline-hide-body]
      '(menu-item "Hide Body" outline-hide-body
		  :help "Hide all body lines in buffer, leaving all headings visible"))
    (define-key map [hide outline-hide-leaves]
      '(menu-item "Hide Leaves" outline-hide-leaves
		  :help "Hide the body after this heading and at deeper levels"))
    (define-key map [show] (cons "Show" (make-sparse-keymap "Show")))
    (define-key map [show outline-show-subtree]
      '(menu-item "Show Subtree" outline-show-subtree
		  :help "Show everything after this heading at deeper levels"))
    (define-key map [show outline-show-children]
      '(menu-item "Show Children" outline-show-children
		  :help "Show all direct subheadings of this heading"))
    (define-key map [show outline-show-branches]
      '(menu-item "Show Branches" outline-show-branches
		  :help "Show all subheadings of this heading, but not their bodies"))
    (define-key map [show outline-show-entry]
      '(menu-item "Show Entry" outline-show-entry
		  :help "Show the body directly following this heading"))
    (define-key map [show outline-show-all]
      '(menu-item "Show All" outline-show-all
		  :help "Show all of the text in the buffer"))
    (define-key map [headings]
      (cons "Headings" (make-sparse-keymap "Headings")))
    (define-key map [headings demote-subtree]
      '(menu-item "Demote Subtree" outline-demote
		  :help "Demote headings lower down the tree"))
    (define-key map [headings promote-subtree]
      '(menu-item "Promote Subtree" outline-promote
		  :help "Promote headings higher up the tree"))
    (define-key map [headings move-subtree-down]
      '(menu-item "Move Subtree Down" outline-move-subtree-down
		  :help "Move the current subtree down past arg headlines of the same level"))
    (define-key map [headings move-subtree-up]
      '(menu-item "Move Subtree Up" outline-move-subtree-up
		  :help "Move the current subtree up past arg headlines of the same level"))
    (define-key map [headings copy]
      '(menu-item "Copy to Kill Ring" outline-headers-as-kill
		  :enable mark-active
		  :help "Save the visible outline headers in region at the start of the kill ring"))
    (define-key map [headings outline-insert-heading]
      '(menu-item "New Heading" outline-insert-heading
		  :help "Insert a new heading at same depth at point"))
    (define-key map [headings outline-backward-same-level]
      '(menu-item "Previous Same Level" outline-backward-same-level
		  :help "Move backward to the arg'th subheading at same level as this one."))
    (define-key map [headings outline-forward-same-level]
      '(menu-item "Next Same Level" outline-forward-same-level
		  :help "Move forward to the arg'th subheading at same level as this one"))
    (define-key map [headings outline-previous-visible-heading]
      '(menu-item "Previous" outline-previous-visible-heading
		  :help "Move to the previous heading line"))
    (define-key map [headings outline-next-visible-heading]
      '(menu-item "Next" outline-next-visible-heading
		  :help "Move to the next visible heading line"))
    (define-key map [headings outline-up-heading]
      '(menu-item "Up" outline-up-heading
		  :help "Move to the visible heading line of which the present line is a subheading"))
    map))

(defvar outline-minor-mode-menu-bar-map
  (let ((map (make-sparse-keymap)))
    (define-key map [outline]
      (cons "Outline"
	    (nconc (make-sparse-keymap "Outline")
		   ;; Remove extra separator
		   (cdr
		    ;; Flatten the major mode's menus into a single menu.
		    (apply #'append
			   (mapcar (lambda (x)
				     (if (consp x)
					 ;; Add a separator between each
					 ;; part of the unified menu.
					 (cons '(--- "---") (cdr x))))
				   outline-mode-menu-bar-map))))))
    map))

(defcustom outline-minor-mode-cycle-filter nil
  "Control where on a heading the visibility-cycling commands are bound to keys.
This option controls, in Outline minor mode, where on a heading typing
the key sequences bound to visibility-cycling commands like `outline-cycle'
and `outline-cycle-buffer' will invoke those commands.  By default, you can
invoke these commands by typing \\`TAB' and \\`S-TAB' anywhere on a heading line,
but customizing this option can make those bindings be in effect only at
specific positions on the heading, like only at the line's beginning or
line's end.  This allows these keys to be bound to their usual commands,
as determined by the major mode, elsewhere on the heading lines.
This option is only in effect when `outline-minor-mode-cycle' is non-nil."
  :type `(choice (const :tag "Everywhere" nil)
                 (const :tag "At line beginning" bolp)
                 (const :tag "Not at line beginning"
                        ,(lambda () (not (bolp))))
                 (const :tag "At line end" eolp)
                 (function :tag "Custom filter function"))
  :version "28.1")

(defvar outline-minor-mode-cycle)
(defvar outline-minor-mode-cycle-map)
(defun outline-minor-mode-cycle--bind (map key binding &optional filter)
  "Define KEY as BINDING in MAP using FILTER.
The key takes effect only on the following conditions:
`outline-minor-mode-cycle' is non-nil, point is located on the heading line,
FILTER or `outline-minor-mode-cycle-filter' is nil or returns non-nil.
The argument MAP is optional and defaults to `outline-minor-mode-cycle-map'."
  (define-key (or map outline-minor-mode-cycle-map) key
    `(menu-item
      "" ,binding
      ;; Filter out specific positions on the heading.
      :filter
      ,(or filter
           (lambda (cmd)
             (when (and outline-minor-mode-cycle
                        (outline-on-heading-p t)
                        (or (not (functionp outline-minor-mode-cycle-filter))
                            (funcall outline-minor-mode-cycle-filter)))
               cmd))))))

(defvar outline-minor-mode-cycle-map
  (let ((map (make-sparse-keymap)))
    (outline-minor-mode-cycle--bind map (kbd "TAB") #'outline-cycle)
    (outline-minor-mode-cycle--bind map (kbd "<backtab>") #'outline-cycle-buffer)
    (keymap-set map "<left-margin> <mouse-1>" #'outline-cycle)
    (keymap-set map "<right-margin> <mouse-1>" #'outline-cycle)
    (keymap-set map "<left-margin> S-<mouse-1>" #'outline-cycle-buffer)
    (keymap-set map "<right-margin> S-<mouse-1>" #'outline-cycle-buffer)
    map)
  "Keymap used as a parent of the `outline-minor-mode' keymap.
It contains key bindings that can be used to cycle visibility.
The recommended way to bind keys is with `outline-minor-mode-cycle--bind'
when the key should be enabled only when `outline-minor-mode-cycle' is
non-nil and point is located on the heading line.")

(defvar outline-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c" outline-mode-prefix-map)
    (define-key map [menu-bar] outline-mode-menu-bar-map)
    ;; Only takes effect if point is on a heading.
    (define-key map (kbd "TAB")
      `(menu-item "" outline-cycle
                  :filter ,(lambda (cmd)
                             (when (outline-on-heading-p) cmd))))
    (define-key map (kbd "<backtab>") #'outline-cycle-buffer)
    map))

(defvar outline-font-lock-keywords
  `(
    ;; Highlight headings according to the level.
    (eval . (list (or (when outline-search-function
                        ,(lambda (limit)
                           (when-let* ((ret (funcall outline-search-function limit)))
                             ;; This is equivalent to adding ".*" in the regexp below.
                             (set-match-data
                              (list (match-beginning 0)
                                    (or (save-excursion
                                          (save-match-data
                                            (re-search-forward
                                             (concat ".*" outline-heading-end-regexp) nil t)))
                                        ;; Fall back to eol when there is no newline
                                        ;; at the end of outline at eob.
                                        (pos-eol))))
                             ret)))
                      (concat "^\\(?:" outline-regexp "\\).*" outline-heading-end-regexp))
                  0 '(if outline-minor-mode
                         (if outline-minor-mode-highlight
                             (list 'face (outline-font-lock-face)))
                       (outline-font-lock-face))
                  (when outline-minor-mode
                    (pcase outline-minor-mode-highlight
                      ('override t)
                      ('append 'append)))
                  t)))
  "Additional expressions to highlight in Outline mode.")

(defface outline-1
  '((t :inherit font-lock-function-name-face))
  "Level 1.")

(defface outline-2
  '((t :inherit font-lock-variable-name-face))
  "Level 2.")

(defface outline-3
  '((t :inherit font-lock-keyword-face))
  "Level 3.")

(defface outline-4
  '((t :inherit font-lock-comment-face))
  "Level 4.")

(defface outline-5
  '((t :inherit font-lock-type-face))
  "Level 5.")

(defface outline-6
  '((t :inherit font-lock-constant-face))
  "Level 6.")

(defface outline-7
  '((t :inherit font-lock-builtin-face))
  "Level 7.")

(defface outline-8
  '((t :inherit font-lock-string-face))
  "Level 8.")

(defvar outline-font-lock-faces
  [outline-1 outline-2 outline-3 outline-4
   outline-5 outline-6 outline-7 outline-8])

(defcustom outline-minor-mode-use-buttons nil
  "Whether to display clickable buttons on the headings.
These buttons can be used to hide and show the body under the heading.
When the value is `insert', additional placeholders for buttons are
inserted to the buffer, so buttons are not only clickable,
but also typing `RET' on them can hide and show the body.
Using the value `insert' is not recommended in editable
buffers because it modifies them.
When the value is `in-margins', then clickable buttons are
displayed in the margins before the headings.
When the value is t, clickable buttons are displayed
in the buffer before the headings.  The values t and
`in-margins' can be used in editing buffers because they
don't modify the buffer."
  ;; The value `insert' is not intended to be customizable.
  :type '(choice (const :tag "Do not use outline buttons" nil)
                 (const :tag "Show outline buttons in margins" in-margins)
                 (const :tag "Show outline buttons in buffer" t))
  :safe #'symbolp
  :version "29.1")

(defvar-local outline--button-icons nil
  "A list of pre-computed button icons.")

(defvar-local outline--use-rtl nil
  "Non-nil when direction of clickable buttons is right-to-left.")

(defvar-local outline--margin-width nil
  "Current margin width.")

(defvar-local outline-margin-width nil
  "Default margin width.")

(define-icon outline-open nil
  '((image "outline-open.svg" "outline-open.pbm" :height (0.8 . em))
    (emoji "🔽")
    (symbol " ▼ ")
    (text " open "))
  "Icon used for buttons for opened sections in outline buffers."
  :version "29.1"
  :help-echo "Close this section")

(define-icon outline-close nil
  '((image "outline-close.svg" "outline-close.pbm" :height (0.8 . em))
    (emoji "▶️")
    (symbol " ▶ ")
    (text " close "))
  "Icon used for buttons for closed sections in outline buffers."
  :version "29.1"
  :help-echo "Open this section")

(define-icon outline-close-rtl outline-close
  '((image "outline-close.svg" "outline-close.pbm" :height (0.8 . em)
           :rotation 180)
    (emoji "◀️")
    (symbol " ◀ "))
  "Right-to-left icon used for buttons in closed outline sections."
  :version "29.1")

(define-icon outline-open-in-margins nil
  '((image "outline-open.svg" "outline-open.pbm" :width font)
    (emoji "🔽")
    (symbol "▼")
    (text "v"))
  "Icon used for buttons for opened sections in margins."
  :version "29.1")

(define-icon outline-close-in-margins nil
  '((image "outline-open.svg" "outline-open.pbm" :width font :rotation -90)
    (emoji "▶️")
    (symbol "▶")
    (text ">"))
  "Icon used for buttons for closed sections in margins."
  :version "29.1")

(define-icon outline-close-rtl-in-margins nil
  '((image "outline-open.svg" "outline-open.pbm" :width font :rotation 90)
    (emoji "◀️")
    (symbol "◀")
    (text "<"))
  "Right-to-left icon used for closed sections in margins."
  :version "29.1")


(defvar outline-level #'outline-level
  "Function of no args to compute a header's nesting level in an outline.
It can assume point is at the beginning of a header line and that the match
data reflects the `outline-regexp'.")
;;;###autoload(put 'outline-level 'risky-local-variable t)

(defun outline-font-lock-face ()
  "Return one of `outline-font-lock-faces' for current level."
  (save-excursion
    (goto-char (match-beginning 0))
    (if outline-search-function
        (funcall outline-search-function nil nil nil t)
      (looking-at outline-regexp))
    (aref outline-font-lock-faces
          (% (1- (funcall outline-level))
             (length outline-font-lock-faces)))))

(defvar outline-view-change-hook nil
  "Normal hook to be run after outline visibility changes.")
(make-obsolete-variable 'outline-view-change-hook nil "29.1")

(defvar outline-mode-hook nil
  "This hook is run when outline mode starts.")

(defcustom outline-blank-line nil
  "Non-nil means to leave an unhidden blank line before headings."
  :type 'boolean
  :safe #'booleanp
  :version "22.1")

(defvar outline-imenu-generic-expression
  (list (list nil (concat "^\\(?:" outline-regexp "\\).*$") 0))
  "Value for `imenu-generic-expression' in Outline mode.")

;;;###autoload
(define-derived-mode outline-mode text-mode "Outline"
  "Set major mode for editing outlines with selective display.
Headings are lines which start with asterisks: one for major headings,
two for subheadings, etc.  Lines not starting with asterisks are body lines.

Body text or subheadings under a heading can be made temporarily
invisible, or visible again.  Invisible lines are attached to the end
of the heading, so they move with it, if the line is killed and yanked
back.  A heading with text hidden under it is marked with an ellipsis (...).

\\{outline-mode-map}
The commands `outline-hide-subtree', `outline-show-subtree',
`outline-show-children', `outline-hide-entry',
`outline-show-entry', `outline-hide-leaves', and `outline-show-branches'
are used when point is on a heading line.

The variable `outline-regexp' can be changed to control what is a heading.
A line is a heading if `outline-regexp' matches something at the
beginning of the line.  The longer the match, the deeper the level.

Turning on outline mode calls the value of `text-mode-hook' and then of
`outline-mode-hook', if they are non-nil."
  (setq-local line-move-ignore-invisible t)
  ;; Cause use of ellipses for invisible text.
  (add-to-invisibility-spec '(outline . t))
  (setq-local paragraph-start
              (concat paragraph-start "\\|\\(?:" outline-regexp "\\)"))
  ;; Inhibit auto-filling of header lines.
  (setq-local auto-fill-inhibit-regexp outline-regexp)
  (setq-local paragraph-separate
              (concat paragraph-separate "\\|\\(?:" outline-regexp "\\)"))
  (setq-local font-lock-defaults
              '(outline-font-lock-keywords t nil nil backward-paragraph))
  (setq-local imenu-generic-expression outline-imenu-generic-expression)
  (add-hook 'change-major-mode-hook #'outline-show-all nil t)
  (add-hook 'hack-local-variables-hook #'outline-apply-default-state nil t))

(defvar outline-minor-mode-map)

(defcustom outline-minor-mode-prefix "\C-c@"
  "Prefix key to use for Outline commands in Outline minor mode.
The value of this variable is checked as part of loading Outline mode.
After that, changing the prefix key requires manipulating keymaps."
  :type 'key-sequence
  :initialize #'custom-initialize-default
  :set (lambda (sym val)
         (define-key outline-minor-mode-map outline-minor-mode-prefix nil)
         (define-key outline-minor-mode-map val outline-mode-prefix-map)
         (set-default sym val)))

(defcustom outline-minor-mode-cycle nil
  "Enable visibility-cycling commands on headings in `outline-minor-mode'.
If enabled, typing \\`TAB' on a heading line cycles the visibility
state of that heading's body between `hide all', `headings only'
and `show all' (`outline-cycle'), and typing \\`S-TAB' on a heading
line likewise cycles the visibility state of the whole buffer
\(`outline-cycle-buffer').
Typing these keys anywhere outside heading lines invokes their default
bindings, per the current major mode."
  :type 'boolean
  :safe #'booleanp
  :version "28.1")

(defcustom outline-minor-mode-highlight nil
  "Whether to highlight headings in `outline-minor-mode' using font-lock keywords.
This option controls whether `outline-minor-mode' will use its font-lock
keywords to highlight headings, which could potentially conflict with
font-lock faces defined by the major mode.  Thus, a non-nil value will
work well only when there's no such conflict.
If the value is t, use outline faces only if there are no major mode's
font-lock faces on headings.  When `override', completely overwrite major
mode's font-lock faces with outline faces.  When `append', try to append
outline font-lock faces to those of major mode."
  :type '(choice (const :tag "Do not use outline font-lock highlighting" nil)
                 (const :tag "Overwrite major mode font-lock faces" override)
                 (const :tag "Append outline font-lock faces to major mode's"
                        append)
                 (const :tag "Highlight with outline font-lock faces only if major mode doesn't" t))
  :safe #'symbolp
  :version "28.1")

(defun outline-minor-mode-highlight-buffer ()
  ;; Fallback to overlays when font-lock is unsupported.
  (save-excursion
    (goto-char (point-min))
    (let ((regexp (unless outline-search-function
                    (concat "^\\(?:" outline-regexp "\\).*" outline-heading-end-regexp))))
      (while (if outline-search-function
                 (when-let* ((ret (funcall outline-search-function)))
                   ;; This is equivalent to adding ".*" in the regexp above.
                   (set-match-data
                    (list (match-beginning 0)
                          (or (save-excursion
                                (save-match-data
                                  (re-search-forward
                                   (concat ".*" outline-heading-end-regexp) nil t)))
                              ;; Fall back to eol when there is no newline
                              ;; at the end of outline at eob.
                              (pos-eol))))
                   ret)
               (re-search-forward regexp nil t))
        (let ((overlay (make-overlay (match-beginning 0) (match-end 0))))
          (overlay-put overlay 'outline-highlight t)
          ;; FIXME: Is it possible to override all underlying face attributes?
          (when (or (memq outline-minor-mode-highlight '(append override))
                    (and (eq outline-minor-mode-highlight t)
                         (not (get-text-property (match-beginning 0) 'face))))
            (overlay-put overlay 'face (outline-font-lock-face))))
        (goto-char (match-end 0))))))

;;;###autoload
(define-minor-mode outline-minor-mode
  "Toggle Outline minor mode.

See the command `outline-mode' for more information on this mode."
  :lighter " Outl"
  :keymap (define-keymap
            :parent outline-minor-mode-cycle-map
            "<menu-bar>" outline-minor-mode-menu-bar-map
            (key-description outline-minor-mode-prefix) outline-mode-prefix-map)
  (if outline-minor-mode
      (progn
        (when outline-minor-mode-use-buttons
          (jit-lock-register #'outline--fix-buttons)
          (when (eq (current-bidi-paragraph-direction) 'right-to-left)
            (setq-local outline--use-rtl t))
          (setq-local outline--button-icons (outline--create-button-icons))
          (when (and (eq outline-minor-mode-use-buttons 'in-margins)
                     (null outline--margin-width))
            (setq outline--margin-width
                  (or outline-margin-width
                      (ceiling
                       (/ (seq-max
                           (seq-map #'string-pixel-width
                                    (seq-map #'icon-string
                                             `(outline-open-in-margins
                                               ,(if outline--use-rtl
                                                    'outline-close-rtl-in-margins
                                                  'outline-close-in-margins)))))
                          (* (default-font-width) 1.0)))))
            (if outline--use-rtl
                (setq-local right-margin-width (+ right-margin-width
                                                  outline--margin-width))
              (setq-local left-margin-width (+ left-margin-width
                                               outline--margin-width)))
            (setq-local fringes-outside-margins t)
            ;; Force display of margins
            (when (eq (current-buffer) (window-buffer))
              (set-window-buffer nil (window-buffer)))))
        (when outline-minor-mode-highlight
          (if (and global-font-lock-mode (font-lock-specified-p major-mode))
              (progn
                (font-lock-add-keywords nil outline-font-lock-keywords t)
                (font-lock-flush))
            (progn
              (outline-minor-mode-highlight-buffer)
              (add-hook 'revert-buffer-restore-functions
                        #'outline-revert-buffer-rehighlight nil t))))
	;; Turn off this mode if we change major modes.
	(add-hook 'change-major-mode-hook
		  (lambda () (outline-minor-mode -1))
		  nil t)
        (add-hook 'revert-buffer-restore-functions
                  #'outline-revert-buffer-restore-visibility nil t)
        (setq-local line-move-ignore-invisible t)
	;; Cause use of ellipses for invisible text.
	(add-to-invisibility-spec '(outline . t))
	(outline-apply-default-state))
    (jit-lock-unregister #'outline--fix-buttons)
    (remove-hook 'revert-buffer-restore-functions
                 #'outline-revert-buffer-restore-visibility t)
    (remove-hook 'revert-buffer-restore-functions
                 #'outline-revert-buffer-rehighlight t)
    (setq line-move-ignore-invisible nil)
    ;; Cause use of ellipses for invisible text.
    (remove-from-invisibility-spec '(outline . t))
    ;; When turning off outline mode, get rid of any outline hiding.
    (outline-show-all)
    (when outline-minor-mode-highlight
      (if font-lock-fontified
          (font-lock-remove-keywords nil outline-font-lock-keywords))
      (font-lock-flush)
      (remove-overlays nil nil 'outline-highlight t))
    (when outline-minor-mode-use-buttons
      (outline--remove-buttons (point-min) (point-max))
      (when (and (eq outline-minor-mode-use-buttons 'in-margins)
                 outline--margin-width
                 (< 0 (if outline--use-rtl right-margin-width
                        left-margin-width)))
        (if outline--use-rtl
            (setq-local right-margin-width (- right-margin-width
                                              outline--margin-width))
          (setq-local left-margin-width (- left-margin-width
                                           outline--margin-width)))
        (setq-local outline--margin-width nil)
        (kill-local-variable 'fringes-outside-margins)
        ;; Force removal of margins
        (when (eq (current-buffer) (window-buffer))
          (set-window-buffer nil (window-buffer)))))))

(defvar-local outline-heading-alist ()
  "Alist associating a heading for every possible level.
Each entry is of the form (HEADING . LEVEL).
This alist is used two ways: to find the heading corresponding to
a given level and to find the level of a given heading.
If a mode or document needs several sets of outline headings (for example
numbered and unnumbered sections), list them set by set and sorted by level
within each set.  For example in texinfo mode:

     (setq outline-heading-alist
      \\='((\"@chapter\" . 2) (\"@section\" . 3) (\"@subsection\" . 4)
           (\"@subsubsection\" . 5)
        (\"@unnumbered\" . 2) (\"@unnumberedsec\" . 3)
           (\"@unnumberedsubsec\" . 4)  (\"@unnumberedsubsubsec\" . 5)
        (\"@appendix\" . 2) (\"@appendixsec\" . 3)...
           (\"@appendixsubsec\" . 4) (\"@appendixsubsubsec\" . 5) ..))

Instead of sorting the entries in each set, you can also separate the
sets with nil.")

;; This used to count columns rather than characters, but that made ^L
;; appear to be at level 2 instead of 1.  Columns would be better for
;; tab handling, but the default regexp doesn't use tabs, and anyone
;; who changes the regexp can also redefine the outline-level variable
;; as appropriate.
(defun outline-level ()
  "Return the depth to which a statement is nested in the outline.
Point must be at the beginning of a header line.
This is actually either the level specified in `outline-heading-alist'
or else the number of characters matched by `outline-regexp'."
  (or (cdr (assoc (match-string 0) outline-heading-alist))
      (- (match-end 0) (match-beginning 0))))

(defun outline-next-preface ()
  "Skip forward to just before the next heading line.
If there's no following heading line, stop before the newline
at the end of the buffer."
  (when (if outline-search-function
            (progn
              ;; Emulate "\n" to force finding the next preface
              (unless (eobp) (forward-char 1))
              (funcall outline-search-function nil t))
          (re-search-forward (concat "\n\\(?:" outline-regexp "\\)")
			     nil 'move))
    (goto-char (match-beginning 0))
    ;; Compensate "\n" from the beginning of regexp
    (when (and outline-search-function (not (bobp))) (forward-char -1)))
  ;; FIXME: Use `outline--end-of-previous'.
  (when (and (bolp) (or outline-blank-line (eobp)) (not (bobp)))
    (forward-char -1)))

(defun outline-next-heading ()
  "Move to the next (possibly invisible) heading line."
  (interactive)
  ;; Make sure we don't match the heading we're at.
  (when (and (bolp) (not (eobp))) (forward-char 1))
  (when (if outline-search-function
            (funcall outline-search-function nil t)
          (re-search-forward (concat "^\\(?:" outline-regexp "\\)")
			     nil 'move))
    (goto-char (match-beginning 0))))

(defun outline-previous-heading ()
  "Move to the previous (possibly invisible) heading line."
  (interactive)
  (if outline-search-function
      (funcall outline-search-function nil t t)
    (re-search-backward (concat "^\\(?:" outline-regexp "\\)")
		        nil 'move)))

(defsubst outline-invisible-p (&optional pos)
  "Non-nil if the character after POS has outline invisible property.
If POS is nil, use `point' instead."
  (eq (get-char-property (or pos (point)) 'invisible) 'outline))

(define-error 'outline-before-first-heading "Before first heading")

(defun outline-back-to-heading (&optional invisible-ok)
  "Move to previous heading line, or beg of this line if it's a heading.
Only visible heading lines are considered, unless INVISIBLE-OK is non-nil."
  (forward-line 0)
  (or (outline-on-heading-p invisible-ok)
      (let (found)
	(save-excursion
	  (while (not found)
	    (or (if outline-search-function
                    (funcall outline-search-function nil nil t)
                  (re-search-backward (concat "^\\(?:" outline-regexp "\\)")
				      nil t))
                (signal 'outline-before-first-heading nil))
	    (setq found (and (or invisible-ok (not (outline-invisible-p)))
			     (point)))))
	(goto-char found)
	found)))

(defun outline-on-heading-p (&optional invisible-ok)
  "Return t if point is on a (visible) heading line.
If INVISIBLE-OK is non-nil, an invisible heading line is ok too."
  (save-excursion
    (forward-line 0)
    (and (bolp) (or invisible-ok (not (outline-invisible-p)))
	 (if outline-search-function
             (funcall outline-search-function nil nil nil t)
           (looking-at outline-regexp)))))

(defun outline-insert-heading ()
  "Insert a new heading at same depth at point."
  (interactive)
  (let ((head (save-excursion
		(condition-case nil
		    (outline-back-to-heading)
		  (error (outline-next-heading)))
		(if (eobp)
		    (or (caar outline-heading-alist) "")
		  (match-string 0)))))
    (unless (or (string-match "[ \t]\\'" head)
		(not (string-match (concat "\\`\\(?:" outline-regexp "\\)")
				   (concat head " "))))
      (setq head (concat head " ")))
    (unless (bolp) (goto-char (pos-eol)) (newline))
    (insert head)
    (unless (eolp)
      (save-excursion (newline-and-indent)))
    (run-hooks 'outline-insert-heading-hook)))

(defun outline-invent-heading (head up)
  "Create a heading by using heading HEAD as a template.
When UP is non-nil, the created heading will be one level above.
Otherwise, it will be one level below."
  (save-match-data
    ;; Let's try to invent one by repeating or deleting the last char.
    (let ((new-head (if up (substring head 0 -1)
                      (concat head (substring head -1)))))
      (if (string-match (concat "\\`\\(?:" outline-regexp "\\)")
                        new-head)
          ;; Why bother checking that it is indeed higher/lower level ?
          new-head
        ;; Didn't work, so ask what to do.
        (read-string (format-message "%s heading for `%s': "
				     (if up "Parent" "Demoted") head)
                     head nil nil t)))))

(defun outline-promote (&optional which)
  "Promote headings higher up the tree.
If `transient-mark-mode' is on, and mark is active, promote headings in
the region (from a Lisp program, pass `region' for WHICH).  Otherwise:
without prefix argument, promote current heading and all headings in the
subtree (from a Lisp program, pass `subtree' for WHICH); with prefix
argument, promote just the current heading (from a Lisp program, pass
nil for WHICH, or do not pass any argument)."
  (interactive
   (list (if (and transient-mark-mode mark-active) 'region
	   (outline-back-to-heading)
	   (if current-prefix-arg nil 'subtree))))
  (cond
   ((eq which 'region)
    (outline-map-region #'outline-promote (region-beginning) (region-end)))
   (which
    (outline-map-region #'outline-promote
			(point)
			(save-excursion (outline-get-next-sibling) (point))))
   (t
    (outline-back-to-heading t)
    (let* ((head (match-string-no-properties 0))
	   (level (save-match-data (funcall outline-level)))
	   (up-head (or (outline-head-from-level (1- level) head)
			;; Use the parent heading, if it is really
			;; one level less.
			(save-excursion
			  (save-match-data
			    (outline-up-heading 1 t)
			    (and (= (1- level) (funcall outline-level))
				 (match-string-no-properties 0))))
                        ;; Bummer!! There is no lower level heading.
                        (outline-invent-heading head 'up))))

      (unless (rassoc level outline-heading-alist)
	(push (cons head level) outline-heading-alist))

      (replace-match up-head nil t)))))

(defun outline-demote (&optional which)
  "Demote headings lower down the tree.
If `transient-mark-mode' is on, and mark is active, demote headings in
the region (from a Lisp program, pass `region' for WHICH).  Otherwise:
without prefix argument, demote current heading and all headings in the
subtree (from a Lisp program, pass `subtree' for WHICH); with prefix
argument, demote just the current heading (from a Lisp program, pass
nil for WHICH, or do not pass any argument)."
  (interactive
   (list (if (and transient-mark-mode mark-active) 'region
	   (outline-back-to-heading)
	   (if current-prefix-arg nil 'subtree))))
  (cond
   ((eq which 'region)
    (outline-map-region #'outline-demote (region-beginning) (region-end)))
   (which
    (outline-map-region #'outline-demote
			(point)
			(save-excursion (outline-get-next-sibling) (point))))
   (t
    (let* ((head (match-string-no-properties 0))
	   (level (save-match-data (funcall outline-level)))
	   (down-head
	    (or (outline-head-from-level (1+ level) head)
		(save-excursion
		  (save-match-data
		    (while (and (progn (outline-next-heading) (not (eobp)))
				(<= (funcall outline-level) level)))
		    (when (eobp)
		      ;; Try again from the beginning of the buffer.
		      (goto-char (point-min))
		      (while (and (progn (outline-next-heading) (not (eobp)))
				  (<= (funcall outline-level) level))))
		    (unless (eobp)
		      (if outline-search-function
                          (funcall outline-search-function nil nil nil t)
                        (looking-at outline-regexp))
		      (match-string-no-properties 0))))
                ;; Bummer!! There is no higher-level heading in the buffer.
                (outline-invent-heading head nil))))

      (unless (rassoc level outline-heading-alist)
	(push (cons head level) outline-heading-alist))
      (replace-match down-head nil t)))))

(defun outline-head-from-level (level head &optional alist)
  "Get new heading with level LEVEL, closest to HEAD, from ALIST.
If there are no such entries, return nil.
ALIST defaults to `outline-heading-alist'.
Similar to (car (rassoc LEVEL ALIST)).
If there are several different entries with same new level, choose the
one with the smallest distance to the association of HEAD in the alist.
This makes it possible for promotion to work in modes with several
independent sets of headings (numbered, unnumbered, appendix...)."
  (unless alist (setq alist outline-heading-alist))
  (let ((l (rassoc level alist))
	ll h hl l2 l2l)
    (cond
     ((null l) nil)
     ;; If there's no HEAD after L, any other entry for LEVEL after L
     ;; can't be much better than L.
     ((null (setq h (assoc head (setq ll (memq l alist))))) (car l))
     ;; If there's no other entry for LEVEL, just keep L.
     ((null (setq l2 (rassoc level (cdr ll)))) (car l))
     ;; Now we have L, L2, and H: see if L2 seems better than L.
     ;; If H is after L2, L2 is better.
     ((memq h (setq l2l (memq l2 (cdr ll))))
      (outline-head-from-level level head l2l))
     ;; Now we have H between L and L2.
     ;; If there's a separator between L and H, prefer L2.
     ((memq h (memq nil ll))
      (outline-head-from-level level head l2l))
     ;; If there's a separator between L2 and H, prefer L.
     ((memq l2 (memq nil (setq hl (memq h ll)))) (car l))
     ;; No separator between L and L2, check the distance.
     ((< (* 2 (length hl)) (+ (length ll) (length l2l)))
      (outline-head-from-level level head l2l))
     ;; If all else fails, just keep L.
     (t (car l)))))

(defun outline-map-region (fun beg end)
  "Call FUN for every heading between BEG and END.
When FUN is called, point is at the beginning of the heading and
the match data is set appropriately."
  (save-excursion
    (setq end (copy-marker end))
    (goto-char beg)
    (when (if outline-search-function
              (funcall outline-search-function end)
            (re-search-forward (concat "^\\(?:" outline-regexp "\\)") end t))
      (goto-char (match-beginning 0))
      (funcall fun)
      (while (and (progn
		    (outline-next-heading)
		    (< (point) end))
		  (not (eobp)))
	(funcall fun)))))

;; Vertical tree motion

(defun outline-move-subtree-up (&optional arg)
  "Move the current subtree up past ARG headlines of the same level."
  (interactive "p")
  (outline-move-subtree-down (- arg)))

(defun outline-move-subtree-down (&optional arg)
  "Move the current subtree down past ARG headlines of the same level."
  (interactive "p")
  (outline-back-to-heading)
  (let* ((movfunc (if (> arg 0) 'outline-get-next-sibling
		    'outline-get-last-sibling))
	 ;; Find the end of the subtree to be moved as well as the point to
	 ;; move it to, adding a newline if necessary, to ensure these points
	 ;; are at bol on the line below the subtree.
         (end-point-func (lambda ()
			   (let ((outline-blank-line nil))
			     (outline-end-of-subtree))
			   (if (eq (char-after) ?\n) (forward-char 1)
				(if (and (eobp) (not (bolp))) (insert "\n")))
			   (point)))
         (beg (point))
         (folded (save-match-data
		   (outline-end-of-heading)
		   (outline-invisible-p)))
         (end (save-match-data
		(funcall end-point-func)))
         (ins-point (make-marker))
         (cnt (abs arg)))
    ;; Find insertion point, with error handling.
    (goto-char beg)
    (while (> cnt 0)
      (or (funcall movfunc)
	  (progn (goto-char beg)
		 (user-error "Cannot move past superior level")))
      (setq cnt (1- cnt)))
    (if (> arg 0)
	;; Moving forward - still need to move over subtree.
	(funcall end-point-func))
    (move-marker ins-point (point))
    (insert (delete-and-extract-region beg end))
    (goto-char ins-point)
    (if folded (outline-hide-subtree))
    (move-marker ins-point nil)))

(defun outline-end-of-heading ()
  "Move to one char before the next `outline-heading-end-regexp'."
  (if (re-search-forward outline-heading-end-regexp nil 'move)
      (forward-char -1)))

(defun outline-next-visible-heading (arg)
  "Move to the next visible heading line.
With ARG, repeats or can move backward if negative.
A heading line is one that starts with a `*' (or that
`outline-regexp' matches)."
  (interactive "p")
  (goto-char (if (< arg 0) (pos-bol) (pos-eol)))
  (let ((regexp (unless outline-search-function
                  (concat "^\\(?:" outline-regexp "\\)")))
        found-heading-p)
    (while (and (not (bobp)) (< arg 0))
      (while (and (not (bobp))
		  (setq found-heading-p
			(if outline-search-function
                            (funcall outline-search-function nil t t)
                          (re-search-backward regexp nil 'move)))
		  (outline-invisible-p)))
      (setq arg (1+ arg)))
    (while (and (not (eobp)) (> arg 0))
      (while (and (not (eobp))
		  (setq found-heading-p
			(if outline-search-function
                            (funcall outline-search-function nil t)
                          (re-search-forward regexp nil 'move)))
		  (outline-invisible-p (match-beginning 0))))
      (setq arg (1- arg)))
    (if found-heading-p (forward-line 0))))

(defun outline-previous-visible-heading (arg)
  "Move to the previous heading line.
With ARG, repeats or can move forward if negative.
A heading line is one that starts with a `*' (or that
`outline-regexp' matches)."
  (interactive "p")
  (outline-next-visible-heading (- arg)))

(defun outline-mark-subtree ()
  "Mark the current subtree in an outlined document.
This puts point at the start of the current subtree, and mark at the end."
  (interactive)
  (let ((beg))
    (if (outline-on-heading-p)
	;; we are already looking at a heading
        (forward-line 0)
      ;; else go back to previous heading
      (outline-previous-visible-heading 1))
    (setq beg (point))
    (outline-end-of-subtree)
    (push-mark (point) nil t)
    (goto-char beg)))


(defvar outline-isearch-open-invisible-function
  #'outline-isearch-open-invisible
  "Function called if `isearch' finishes in an invisible overlay.
The function is called with the overlay as its only argument.")

(put 'outline 'reveal-toggle-invisible #'outline-reveal-toggle-invisible)
(defun outline-flag-region (from to flag)
  "Hide or show lines from FROM to TO, according to FLAG.
If FLAG is nil then text is shown, while if FLAG is t the text is hidden."
  (remove-overlays from to 'invisible 'outline)
  (when flag
    ;; We use `front-advance' here because the invisible text begins at the
    ;; very end of the heading, before the newline, so text inserted at FROM
    ;; belongs to the heading rather than to the entry.
    (let ((o (make-overlay from to nil 'front-advance)))
      (overlay-put o 'evaporate t)
      (overlay-put o 'invisible 'outline)
      (overlay-put o 'isearch-open-invisible
		   (or outline-isearch-open-invisible-function
		       #'outline-isearch-open-invisible))))
  ;; Jit-lock won't be triggered because we only touched overlays, so we have
  ;; to update "by hand".
  (outline--fix-buttons from to)
  (run-hooks 'outline-view-change-hook))

(defun outline-reveal-toggle-invisible (o hidep)
  (save-excursion
    (goto-char (overlay-start o))
    (if hidep
        ;; When hiding the area again, we could just clean it up and let
        ;; reveal do the rest, by simply doing:
        ;; (remove-overlays (overlay-start o) (overlay-end o)
        ;;                  'invisible 'outline)
        ;;
        ;; That works fine as long as everything is in sync, but if the
        ;; structure of the document is changed while revealing parts of it,
        ;; the resulting behavior can be ugly.  I.e. we need to make
        ;; sure that we hide exactly a subtree.
        (progn
          (let ((end (overlay-end o)))
            (delete-overlay o)
            (while (progn
                     (outline-hide-subtree)
                     (outline-next-visible-heading 1)
                     (and (not (eobp)) (< (point) end))))))

      ;; When revealing, we just need to reveal sublevels.  If point is
      ;; inside one of the sublevels, reveal will call us again.
      ;; But we need to preserve the original overlay.
      (let ((o1 (copy-overlay o)))
        (overlay-put o 'invisible nil)  ;Show (most of) the text.
        (while (progn
                 (outline-show-entry)
                 (outline-show-children)
                 ;; Normally just the above is needed.
                 ;; But in odd cases, the above might fail to show anything.
                 ;; To avoid an infinite loop, we have to make sure that
                 ;; *something* gets shown.
                 (and (equal (overlay-start o) (overlay-start o1))
                      (< (point) (overlay-end o))
                      (= 0 (forward-line 1)))))
        ;; If still nothing was shown, just kill the damn thing.
        (when (equal (overlay-start o) (overlay-start o1))
          ;; I've seen it happen at the end of buffer.
          (delete-overlay o1))))))

;; Function to be set as an outline-isearch-open-invisible' property
;; to the overlay that makes the outline invisible (see
;; `outline-flag-region').
(defun outline-isearch-open-invisible (_overlay)
  ;; We rely on the fact that isearch places point on the matched text.
  (outline-show-entry))

(defun outline-hide-entry ()
  "Hide the body directly following this heading."
  (interactive)
  (save-excursion
    (outline-back-to-heading)
    (outline-end-of-heading)
    (outline-flag-region (point) (progn (outline-next-preface) (point)) t)))

(define-obsolete-function-alias 'hide-entry #'outline-hide-entry "25.1")

(defun outline-show-entry ()
  "Show the body directly following this heading.
Show the heading too, if it is currently invisible."
  (interactive)
  (save-excursion
    (outline-back-to-heading t)
    (outline-flag-region (1- (point))
                         (progn
                           (outline-next-preface)
                           (if (= 1 (- (point-max) (point)))
                               (point-max)
                             (point)))
                         nil)))

(define-obsolete-function-alias 'show-entry #'outline-show-entry "25.1")

(defun outline-hide-body ()
  "Hide all body lines in buffer, leaving all headings visible.
Note that this does not hide the lines preceding the first heading line."
  (interactive)
  (outline-hide-region-body (point-min) (point-max)))

(define-obsolete-function-alias 'hide-body #'outline-hide-body "25.1")

(defun outline-hide-region-body (start end)
  "Hide all body lines between START and END, but not headings."
  ;; Nullify the hook to avoid repeated calls to `outline-flag-region'
  ;; wasting lots of time running `lazy-lock-fontify-after-outline'
  ;; and run the hook finally.
  ;; FIXME: The above comment seems outdated, as lazy-lock has been
  ;;        removed from Emacs.
  (let (outline-view-change-hook)
    (save-excursion
      (save-restriction
	(narrow-to-region start end)
	(goto-char (point-min))
	(if (outline-on-heading-p)
	    (outline-end-of-heading)
	  (outline-next-preface))
	(while (not (eobp))
	  (outline-flag-region (point)
			       (progn (outline-next-preface) (point)) t)
	  (unless (eobp)
	    (forward-char (if (looking-at "\n\n") 2 1))
	    (outline-end-of-heading))))))
  (run-hooks 'outline-view-change-hook))

(define-obsolete-function-alias
    'hide-region-body #'outline-hide-region-body "25.1")

(defun outline-show-all ()
  "Show all of the text in the buffer."
  (interactive)
  (outline-flag-region (point-min) (point-max) nil))

(define-obsolete-function-alias 'show-all #'outline-show-all "25.1")

(defun outline-hide-subtree (&optional event)
  "Hide everything after this heading at deeper levels.
If non-nil, EVENT should be a mouse event."
  (interactive (list last-nonmenu-event))
  (save-excursion
    (when (mouse-event-p event)
      (mouse-set-point event))
    (outline-flag-subtree t)))

(define-obsolete-function-alias 'hide-subtree #'outline-hide-subtree "25.1")

(defun outline-hide-leaves ()
  "Hide the body after this heading and at deeper levels."
  (interactive)
  (save-excursion
    (outline-back-to-heading)
    ;; Turned off to fix bug reported by Otto Maddox on 22 Nov 2005.
    ;;    (outline-end-of-heading)
    (outline-hide-region-body
     (point)
     (progn (outline-end-of-subtree) (point)))))

(define-obsolete-function-alias 'hide-leaves #'outline-hide-leaves "25.1")

(defun outline-show-subtree (&optional event)
  "Show everything after this heading at deeper levels.
If non-nil, EVENT should be a mouse event."
  (interactive (list last-nonmenu-event))
  (save-excursion
    (when (mouse-event-p event)
      (mouse-set-point event))
    (outline-flag-subtree nil)))

(define-obsolete-function-alias 'show-subtree #'outline-show-subtree "25.1")

(defun outline-show-heading ()
  "Show the current heading and move to its end."
  (outline-flag-region (- (point)
 			  (if (bobp) 0
 			    (if (and outline-blank-line
                                     (eq (char-before (1- (point))) ?\n))
 				2 1)))
		       (progn (outline-end-of-heading) (point))
		       nil))

(defun outline-hide-sublevels (levels)
  "Hide everything but the top LEVELS levels of headers, in whole buffer.
This also unhides the top heading-less body, if any.

Interactively, the prefix argument supplies the value of LEVELS.
When invoked without a prefix argument, LEVELS defaults to the level
of the current heading, or to 1 if the current line is not a heading."
  (interactive (list
		(cond
		 (current-prefix-arg (prefix-numeric-value current-prefix-arg))
		 ((save-excursion
                    (forward-line 0)
		    (if outline-search-function
                        (funcall outline-search-function nil nil nil t)
                      (looking-at outline-regexp)))
		  (funcall outline-level))
		 (t 1))))
  (if (< levels 1)
      (error "Must keep at least one level of headers"))
  (save-excursion
    (let* (outline-view-change-hook
           (beg (progn
                  (goto-char (point-min))
                  ;; Skip the prelude, if any.
                  (unless (outline-on-heading-p t) (outline-next-heading))
                  (point)))
           (end (progn
                  (goto-char (point-max))
                  ;; Keep empty last line, if available.
                  (if (bolp) (1- (point)) (point)))))
      (if (< end beg)
	  (setq beg (prog1 end (setq end beg))))
      ;; First hide everything.
      (outline-flag-region beg end t)
      ;; Then unhide the top level headers.
      (outline-map-region
       (lambda ()
	 (if (<= (funcall outline-level) levels)
	     (outline-show-heading)))
       beg end)
      ;; Finally unhide any trailing newline.
      (goto-char (point-max))
      (if (and (bolp) (not (bobp)) (outline-invisible-p (1- (point))))
          (outline-flag-region (1- (point)) (point) nil))))
  (run-hooks 'outline-view-change-hook))

(define-obsolete-function-alias 'hide-sublevels #'outline-hide-sublevels "25.1")

(defun outline-hide-other ()
  "Hide everything except current body and parent and top-level headings.
This also unhides the top heading-less body, if any."
  (interactive)
  (outline-hide-sublevels 1)
  (let (outline-view-change-hook)
    (save-excursion
      (outline-back-to-heading t)
      (outline-show-entry)
      (while (condition-case nil (progn (outline-up-heading 1 t) (not (bobp)))
	       (error nil))
	(outline-flag-region (1- (point))
			     (save-excursion (forward-line 1) (point))
			     nil))))
  (run-hooks 'outline-view-change-hook))

(define-obsolete-function-alias 'hide-other #'outline-hide-other "25.1")

(defun outline-toggle-children ()
  "Show or hide the current subtree depending on its current state."
  (interactive)
  (save-excursion
    (outline-back-to-heading)
    (if (not (outline-invisible-p (pos-eol)))
        (outline-hide-subtree)
      (outline-show-children)
      (outline-show-entry))))

(defun outline-flag-subtree (flag)
  "Assign FLAG to the current subtree."
  (save-excursion
    (outline-back-to-heading)
    (outline-end-of-heading)
    (outline-flag-region (point)
			  (progn (outline-end-of-subtree) (point))
			  flag)))

(defun outline--end-of-previous ()
  "Go back from BOH (or EOB) to end of previous element."
  (if (eobp)
      (if (bolp) (forward-char -1))
    ;; Go to end of line before heading
    (forward-char -1)
    (if (and outline-blank-line (bolp))
        ;; leave blank line before heading
        (forward-char -1))))

(defun outline-end-of-subtree ()
  "Move to the end of the current subtree."
  (outline-back-to-heading)
  (let ((first t)
	(level (funcall outline-level)))
    (while (and (not (eobp))
		(or first (> (funcall outline-level) level)))
      (setq first nil)
      (outline-next-heading))
    (if (and (bolp) (not (eolp)))
	;; We stopped at a nonempty line (the next heading).
	(outline--end-of-previous))))

(defun outline-show-branches ()
  "Show all subheadings of this heading, but not their bodies."
  (interactive)
  (outline-show-children 1000))

(define-obsolete-function-alias 'show-branches #'outline-show-branches "25.1")

(defun outline-show-children (&optional level)
  "Show all direct subheadings of this heading.
Prefix arg LEVEL is how many levels below the current level should be shown.
Default is enough to cause the following heading to appear."
  (interactive "P")
  (setq level
	(if level (prefix-numeric-value level)
	  (save-excursion
	    (outline-back-to-heading)
	    (let ((start-level (funcall outline-level)))
	      (outline-next-heading)
	      (if (eobp)
		  1
		(max 1 (- (funcall outline-level) start-level)))))))
  (let (outline-view-change-hook)
    (save-excursion
      (outline-back-to-heading)
      (setq level (+ level (funcall outline-level)))
      (outline-map-region
       (lambda ()
	 (if (<= (funcall outline-level) level)
	     (outline-show-heading)))
       (point)
       (progn (outline-end-of-subtree)
	      (if (eobp) (point-max) (1+ (point)))))))
  (run-hooks 'outline-view-change-hook))

(define-obsolete-function-alias 'show-children #'outline-show-children "25.1")



(defun outline-up-heading (arg &optional invisible-ok)
  "Move to the visible heading line of which the present line is a subheading.
With argument, move up ARG levels.
If INVISIBLE-OK is non-nil, also consider invisible lines."
  (interactive "p")
  (and (eq this-command 'outline-up-heading)
       (or (eq last-command 'outline-up-heading) (push-mark)))
  (outline-back-to-heading invisible-ok)
  (let ((start-level (funcall outline-level)))
    (when (<= start-level 1)
      (error "Already at top level of the outline"))
    (while (and (> start-level 1) (> arg 0) (not (bobp)))
      (let ((level start-level))
	(while (not (or (< level start-level) (bobp)))
	  (if invisible-ok
	      (outline-previous-heading)
	    (outline-previous-visible-heading 1))
	  (setq level (funcall outline-level)))
	(setq start-level level))
      (setq arg (- arg 1))))
  (if outline-search-function
      (funcall outline-search-function nil nil nil t)
    (looking-at outline-regexp)))

(defun outline-forward-same-level (arg)
  "Move forward to the ARG'th subheading at same level as this one.
Stop at the first and last subheadings of a superior heading."
  (interactive "p")
  (outline-back-to-heading)
  (while (> arg 0)
    (let ((point-to-move-to (save-excursion
			      (outline-get-next-sibling))))
      (if point-to-move-to
	  (progn
	    (goto-char point-to-move-to)
	    (setq arg (1- arg)))
	(progn
	  (setq arg 0)
	  (error "No following same-level heading"))))))

(defun outline-get-next-sibling ()
  "Move to next heading of the same level, and return point.
If there is no such heading, return nil."
  (let ((level (funcall outline-level)))
    (outline-next-visible-heading 1)
    (while (and (not (eobp)) (> (funcall outline-level) level))
      (outline-next-visible-heading 1))
    (if (or (eobp) (< (funcall outline-level) level))
	nil
      (point))))

(defun outline-backward-same-level (arg)
  "Move backward to the ARG'th subheading at same level as this one.
Stop at the first and last subheadings of a superior heading."
  (interactive "p")
  (outline-back-to-heading)
  (while (> arg 0)
    (let ((point-to-move-to (save-excursion
			      (outline-get-last-sibling))))
      (if point-to-move-to
	  (progn
	    (goto-char point-to-move-to)
	    (setq arg (1- arg)))
	(progn
	  (setq arg 0)
	  (error "No previous same-level heading"))))))

(defun outline-get-last-sibling ()
  "Move to previous heading of the same level, and return point.
If there is no such heading, return nil."
  (let ((opoint (point))
	(level (funcall outline-level)))
    (outline-previous-visible-heading 1)
    (when (and (/= (point) opoint) (outline-on-heading-p))
      (while (and (> (funcall outline-level) level)
		  (not (bobp)))
	(outline-previous-visible-heading 1))
      (if (< (funcall outline-level) level)
	  nil
        (point)))))


;;; Search text-property for outline headings

;;;###autoload
(defun outline-search-level (&optional bound move backward looking-at)
  "Search for the next text property `outline-level'.
The arguments are the same as in `outline-search-text-property',
except the hard-coded property name `outline-level'.
This function is intended to be used in `outline-search-function'."
  (outline-search-text-property 'outline-level nil bound move backward looking-at))

(autoload 'text-property-search-forward "text-property-search")
(autoload 'text-property-search-backward "text-property-search")

(defun outline-search-text-property (property &optional value bound move backward looking-at)
  "Search for the next text property PROPERTY with VALUE.
The rest of arguments are described in `outline-search-function'."
  (if looking-at
      (when (if value (eq (get-text-property (point) property) value)
              (get-text-property (point) property))
        (set-match-data (list (pos-bol) (pos-eol)))
        t)
    ;; Go to the end when in the middle of heading
    (when (and (not backward)
               (if value (eq (get-text-property (point) property) value)
                 (get-text-property (point) property))
               (not (or (bobp)
                        (not (if value
                                 (eq (get-text-property (1- (point)) property) value)
                               (get-text-property (1- (point)) property))))))
      (goto-char (1+ (pos-eol))))
    (let ((prop-match (if backward
                          (text-property-search-backward property value (and value t))
                        (text-property-search-forward property value (and value t)))))
      (if prop-match
          (let ((beg (prop-match-beginning prop-match))
                (end (prop-match-end prop-match)))
            (if (or (null bound) (if backward (>= beg bound) (<= end bound)))
                (cond (backward
                       (goto-char beg)
                       (goto-char (pos-bol))
                       (set-match-data (list (point) end))
                       t)
                      (t
                       (goto-char end)
                       (goto-char (if (bolp) (1- (point)) (pos-eol)))
                       (set-match-data (list beg (point)))
                       t))
              (when move (goto-char bound))
              nil))
        (when move (goto-char (or bound (if backward (point-min) (point-max)))))
        nil))))


(defun outline-headers-as-kill (beg end)
  "Save the visible outline headers between BEG and END to the kill ring.

Text shown between the headers isn't copied.  Two newlines are
inserted between saved headers.  Yanking the result may be a
convenient way to make a table of contents of the buffer."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ((buffer (current-buffer)) start end)
        (with-temp-buffer
          (let ((temp-buffer (current-buffer)))
            (with-current-buffer buffer
              ;; Boundary condition: starting on heading:
              (when (outline-on-heading-p)
                (outline-back-to-heading)
                (setq start (point)
                      end (progn (outline-end-of-heading) (point)))
                (with-current-buffer temp-buffer
                  (insert-buffer-substring buffer start end)
                  (insert "\n\n")))
              (while (outline-next-heading)
                (unless (outline-invisible-p)
                  (setq start (point)
                        end (progn (outline-end-of-heading) (point)))
                  (with-current-buffer temp-buffer
                    (insert-buffer-substring buffer start end)
                    (insert "\n\n"))))))
          (kill-new (buffer-string)))))))


;;; Initial visibility

(defcustom outline-default-state nil
  "If non-nil, some headings are initially outlined.

Note that the default state is applied when Outline major and
minor modes are set or when the command
`outline-apply-default-state' is called interactively.

When nil, no default state is defined and
`outline-apply-default-state' is a no-op.

If equal to `outline-show-all', all text of buffer is shown.

If equal to `outline-show-only-headings', show only headings,
whatever their level is.

If equal to a number, show only headings up to and including the
corresponding level.  See `outline-default-rules' to customize
visibility of the subtree at that level.

If equal to a lambda function or function name, this function is
expected to toggle headings visibility, and will be called
without arguments after the mode is enabled.  Heading visibility
can be changed with functions such as `outline-show-subtree',
`outline-show-entry', `outline-hide-entry' etc."
  :version "29.1"
  :type '(choice (const :tag "Disabled" nil)
                 (const :tag "Show all" outline-show-all)
                 (const :tag "Only headings" outline-show-only-headings)
                 (natnum :tag "Show headings up to level" :value 1)
                 (function :tag "Custom function")))

(defcustom outline-default-rules nil
  "Determines visibility of subtree starting at `outline-default-state' level.

The rules apply if and only if `outline-default-state' is a
number.

When nil, the subtree is hidden unconditionally.

When equal to a list, each element should be one of the following:

- A cons cell with CAR `match-regexp' and CDR a regexp, the
  subtree will be hidden when the outline heading match the
  regexp.

- `subtree-has-long-lines' to only show the heading branches when
  long lines are detected in its subtree (see
  `outline-default-long-line' for the definition of long lines).

- `subtree-is-long' to only show the heading branches when its
  subtree contains more than `outline-default-line-count' lines.

- A cons cell of the form (custom-function . FUNCTION) where
  FUNCTION is a lambda function or function name which will be
  called without arguments with point at the beginning of the
  heading and the match data set appropriately, the function
  being expected to toggle the heading visibility."
  :version "29.1"
  :type '(choice (const :tag "Hide subtree" nil)
                 (set :tag "Show subtree unless"
                      (cons :tag "Heading match regexp"
                            (const match-regexp)  string)
                      (const :tag "Subtree has long lines"
                             subtree-has-long-lines)
                      (const :tag "Subtree is long"
                             subtree-is-long)
                      (cons :tag "Custom function"
                            (const custom-function) function))))

(defcustom outline-default-long-line 1000
  "Minimal number of characters in a line for a heading to be outlined."
  :version "29.1"
  :type '(natnum :tag "Number of characters"))

(defcustom outline-default-line-count 50
  "Minimal number of lines for a heading to be outlined."
  :version "29.1"
  :type '(natnum :tag "Number of lines"))

(defun outline-apply-default-state ()
  "Apply the outline state defined by `outline-default-state'."
  (interactive)
  (cond
   ((integerp outline-default-state)
    (outline--show-headings-up-to-level outline-default-state))
   ((functionp outline-default-state)
    (funcall outline-default-state))))

(defun outline-show-only-headings ()
  "Show only headings."
  (interactive)
  (outline-show-all)
  (outline-hide-region-body (point-min) (point-max)))

(eval-when-compile (require 'so-long))
(autoload 'so-long-detected-long-line-p "so-long")
(defvar so-long-skip-leading-comments)
(defvar so-long-threshold)
(defvar so-long-max-lines)

(defun outline--show-headings-up-to-level (level)
  "Show only headings up to a LEVEL level.

Like `outline-hide-sublevels' but, for each heading at level
LEVEL, decides of subtree visibility according to
`outline-default-rules'."
  (if (not outline-default-rules)
      (outline-hide-sublevels level)
    (if (< level 1)
        (error "Must keep at least one level of headers"))
    (save-excursion
      (let* (outline-view-change-hook
             (beg (progn
                    (goto-char (point-min))
                    ;; Skip the prelude, if any.
                    (unless (outline-on-heading-p t) (outline-next-heading))
                    (point)))
             (end (progn
                    (goto-char (point-max))
                    ;; Keep empty last line, if available.
                    (if (bolp) (1- (point)) (point))))
             (heading-regexp
              (cdr-safe
               (assoc 'match-regexp outline-default-rules)))
             (check-line-count
              (memq 'subtree-is-long outline-default-rules))
             (check-long-lines
              (memq 'subtree-has-long-lines outline-default-rules))
             (custom-function
              (cdr-safe
               (assoc 'custom-function outline-default-rules))))
        (if (< end beg)
	    (setq beg (prog1 end (setq end beg))))
        ;; First hide everything.
        (outline-hide-sublevels level)
        ;; Then unhide the top level headers.
        (outline-map-region
         (lambda ()
           (let ((current-level (funcall outline-level)))
	     (when (< current-level level)
               (outline-show-heading)
               (outline-show-entry))
             (when (= current-level level)
               (cond
                ((and heading-regexp
                      (let ((beg (point))
                            (end (progn (outline-end-of-heading) (point))))
                        (string-match-p heading-regexp (buffer-substring beg end))))
                 ;; hide entry when heading match regexp
                 (outline-hide-entry))
                ((and check-line-count
                      (save-excursion
                        (let ((beg (point))
                              (end (progn (outline-end-of-subtree) (point))))
                          (<= outline-default-line-count (count-lines beg end)))))
                 ;; show only branches when line count of subtree >
                 ;; threshold
                 (outline-show-branches))
                ((and check-long-lines
                      (save-excursion
                        (let ((beg (point))
                              (end (progn (outline-end-of-subtree) (point))))
                          (save-restriction
                            (narrow-to-region beg end)
                            (let ((so-long-skip-leading-comments nil)
                                  (so-long-threshold outline-default-long-line)
                                  (so-long-max-lines nil))
                              (so-long-detected-long-line-p))))))
                 ;; show only branches when long lines are detected
                 ;; in subtree
                 (outline-show-branches))
                (custom-function
                 ;; call custom function if defined
                 (funcall custom-function))
                (t
                 ;; if no previous clause succeeds, show subtree
                 (outline-show-subtree))))))
         beg end)))
    (run-hooks 'outline-view-change-hook)))

(defun outline-show-by-heading-regexp (regexp)
  "Show outlines whose headings match REGEXP."
  (interactive (list (read-regexp "Regexp to show outlines")))
  (let (outline-view-change-hook)
    (outline-map-region
     (lambda ()
       (when (string-match-p regexp (buffer-substring (pos-bol) (pos-eol)))
         (outline-show-branches) ;; To reveal all parent headings
         (outline-show-entry)))
     (point-min) (point-max)))
  (run-hooks 'outline-view-change-hook))

(defun outline-hide-by-heading-regexp (regexp)
  "Hide outlines whose headings match REGEXP."
  (interactive (list (read-regexp "Regexp to hide outlines")))
  (let (outline-view-change-hook)
    (outline-map-region
     (lambda ()
       (when (string-match-p regexp (buffer-substring (pos-bol) (pos-eol)))
         (outline-hide-subtree)))
     (point-min) (point-max)))
  (run-hooks 'outline-view-change-hook))

(defun outline--hidden-headings-paths ()
  "Return (HASH-TABLE CURRENT-HEADING).
HASH-TABLE holds the headings of currently hidden outlines.
Every key is a list whose elements compose a complete path
of headings descending from the top level down to the bottom level.
Every entry's value is non-nil if that entry should be hidden.
The specific non-nil vale can be t to hide just the entry, or a number
LEVEL to mean that not just the entry should be hidden but also all the
subsequent elements of level higher or equal to LEVEL.
This is useful to save the hidden outlines and restore them later
after reverting the buffer.
CURRENT-HEADING is the heading where point is located."
  (let* ((paths (make-hash-table :test #'equal))
         path current-path
         (current-heading-p (outline-on-heading-p))
         (current-beg (when current-heading-p (pos-bol)))
         (current-end (when current-heading-p (pos-eol))))
    (outline-map-region
     (lambda ()
       (let ((level (funcall outline-level)))
         (if (outline-invisible-p)
             ;; Covered by "the" previous heading.
             (cl-callf (lambda (l) (if (numberp l) (min l level) level))
                 (gethash (mapcar #'car path) paths))
           (let ((heading (buffer-substring-no-properties (pos-bol) (pos-eol))))
             (while (and path (>= (cdar path) level))
               (pop path))
             (push (cons heading level) path)
             (when (save-excursion
                     (outline-end-of-heading)
                     (outline-invisible-p))
               (setf (gethash (mapcar #'car path) paths) t))))
         (when (and current-heading-p (<= current-beg (point) current-end))
           (setq current-path (mapcar #'car path)))))
     (point-min) (point-max))
    (list paths current-path)))

(defun outline--hidden-headings-restore-paths (paths current-path)
  "Restore hidden outlines from a hash-table of hidden headings.
This is useful after reverting the buffer to restore the outlines
hidden by `outline--hidden-headings-paths'.  Also restore point
on the same outline where point was before reverting the buffer."
  (let ((hidelevel nil) (hidestart nil)
        path current-point outline-view-change-hook)
    (outline-map-region
     (lambda ()
       (let ((level (funcall outline-level)))
         (if (and (numberp hidelevel) (<= hidelevel level))
             nil
           (when hidestart
             (outline-flag-region hidestart
                                  (save-excursion (outline--end-of-previous)
                                                  (point))
                                  t)
             (setq hidestart nil))
           (let* ((heading (buffer-substring-no-properties
                            (pos-bol) (pos-eol))))
             (while (and path (>= (cdar path) level))
               (pop path))
             (push (cons heading level) path)
             (when (setq hidelevel (gethash (mapcar #'car path) paths))
               (setq hidestart (save-excursion (outline-end-of-heading)
                                               (point))))))
         (when (and current-path (equal current-path (mapcar #'car path)))
           (setq current-point (point)))))
     (point-min) (point-max))
    (when hidestart
      (outline-flag-region hidestart
                           (save-excursion
                             (goto-char (point-max))
                             (outline--end-of-previous)
                             (point))
                           t))
    (when current-point (goto-char current-point))))

(defun outline-revert-buffer-restore-visibility ()
  "Preserve visibility when reverting buffer under `outline-minor-mode'.
This function restores the visibility of outlines after the buffer
under `outline-minor-mode' is reverted by `revert-buffer'."
  (let ((paths (outline--hidden-headings-paths)))
    (unless (and (hash-table-empty-p (nth 0 paths))
                 (null (nth 1 paths)))
      (lambda ()
        (outline--hidden-headings-restore-paths
         (nth 0 paths) (nth 1 paths))))))

(defun outline-revert-buffer-rehighlight ()
  "Rehighlight outlines when reverting buffer under `outline-minor-mode'.
This function rehighlights outlines after the buffer under
`outline-minor-mode' is reverted by `revert-buffer' when font-lock
can't update highlighting for `outline-minor-mode-highlight'."
  (lambda () (outline-minor-mode-highlight-buffer)))


;;; Visibility cycling

(defun outline--cycle-state ()
  "Return the cycle state of current heading.
Return either `hide-all', `headings-only', or `show-all'."
  (save-excursion
    (let (start end ov-list heading-end)
      (outline-back-to-heading)
      (setq start (point))
      (outline-end-of-heading)
      (setq heading-end (point))
      (outline-end-of-subtree)
      (setq end (point))
      (setq ov-list
            (seq-filter
             (lambda (o)
               (and (eq (overlay-get o 'invisible) 'outline)
                    (save-excursion
                      (goto-char (overlay-start o))
                      (outline-on-heading-p t))))
             (overlays-in start end)))
      (cond ((null ov-list) 'show-all)
            ((and (or (= end (point-max)
                         (1+ (overlay-end (car ov-list))))
                      (= (overlay-end (car ov-list)) end))
                  (= (overlay-start (car ov-list)) heading-end))
             'hide-all)
            (t 'headings-only)))))

(defun outline-has-subheading-p ()
  "Return t if this heading has subheadings, nil otherwise."
  (save-excursion
    (outline-back-to-heading)
    (< (save-excursion (outline-next-heading) (point))
       (save-excursion (outline-end-of-subtree) (point)))))

(defun outline-cycle (&optional event)
  "Cycle visibility state of the current heading line's body.

This cycles the visibility of the current heading line's subheadings
and body between `hide all', `headings only' and `show all'.

`Hide all' means hide all the subheadings and their bodies.
`Headings only' means show the subheadings, but not their bodies.
`Show all' means show all the subheadings and their bodies.

If non-nil, EVENT should be a mouse event."
  (interactive (list last-nonmenu-event))
  (save-excursion
    (when (mouse-event-p event)
      (mouse-set-point event))
    (condition-case nil
        (pcase (outline--cycle-state)
          ('hide-all
           (if (outline-has-subheading-p)
               (progn (outline-show-children)
                      (message "Only headings"))
             (outline-show-subtree)
             (message "Show all")))
          ('headings-only
           (outline-show-subtree)
           (message "Show all"))
          ('show-all
           (outline-hide-subtree)
           (message "Hide all")))
      (outline-before-first-heading nil))))

(defvar-local outline--cycle-buffer-state 'show-all
  "Internal variable used for tracking buffer cycle state.")

(defun outline-cycle-buffer (&optional level)
  "Cycle visibility state of the body lines of the whole buffer.

This cycles the visibility of all the subheadings and bodies of all
the heading lines in the buffer.  It cycles them between `hide all',
`headings only' and `show all'.

`Hide all' means hide all the buffer's subheadings and their bodies.
`Headings only' means show all the subheadings, but not their bodies.
`Show all' means show all the buffer's subheadings and their bodies.

With a prefix argument, show headings up to that LEVEL."
  (interactive (list (when current-prefix-arg
                       (prefix-numeric-value current-prefix-arg))))
  (let (top-level)
    (save-excursion
      (goto-char (point-min))
      (while (not (or (eq top-level 1) (eobp)))
        (when-let* ((level (and (outline-on-heading-p t)
                                (funcall outline-level))))
          (when (< level (or top-level most-positive-fixnum))
            (setq top-level (max level 1))))
        (outline-next-heading)))
    (cond
     (level
      (outline-hide-sublevels level)
      (setq outline--cycle-buffer-state 'all-heading)
      (message "All headings up to level %s" level))
     ((and (eq outline--cycle-buffer-state 'show-all)
           top-level)
      (outline-hide-sublevels top-level)
      (setq outline--cycle-buffer-state 'top-level)
      (message "Top level headings"))
     ((or (eq outline--cycle-buffer-state 'show-all)
          (eq outline--cycle-buffer-state 'top-level))
      (outline-show-all)
      (outline-hide-region-body (point-min) (point-max))
      (setq outline--cycle-buffer-state 'all-heading)
      (message "All headings"))
     (t
      (outline-show-all)
      (setq outline--cycle-buffer-state 'show-all)
      (message "Show all")))))


;;; Button/margin indicators

(defvar-keymap outline-button-icon-map
  "<mouse-2>" #'outline-cycle
  ;; Need to override the global binding
  ;; `mouse-appearance-menu' with <down->:
  "S-<down-mouse-1>" #'ignore
  "S-<mouse-1>" #'outline-cycle-buffer)

(defvar-keymap outline-overlay-button-map
  "RET" #'outline-cycle)

(defvar-keymap outline-inserted-button-map
  :parent (make-composed-keymap outline-button-icon-map
                                outline-overlay-button-map))

(defun outline--create-button-icons ()
  (pcase outline-minor-mode-use-buttons
    ('in-margins
     (mapcar
      (lambda (icon-name)
        (let* ((icon (icon-elements icon-name))
               (face   (plist-get icon 'face))
               (string (plist-get icon 'string))
               (image  (plist-get icon 'image))
               (display `((margin ,(if outline--use-rtl
                                       'right-margin 'left-margin))
                          ,(or image (if face (propertize
                                               string 'face face)
                                       string))))
               (space (propertize " " 'display display)))
          (if (and image face) (propertize space 'face face) space)))
      (list 'outline-open-in-margins
            (if outline--use-rtl
                'outline-close-rtl-in-margins
              'outline-close-in-margins))))
    ('insert
     (mapcar
      (lambda (icon-name)
        (icon-elements icon-name))
      (list 'outline-open
            (if outline--use-rtl 'outline-close-rtl 'outline-close))))
    (_
     (mapcar
      (lambda (icon-name)
        (propertize (icon-string icon-name)
                    'mouse-face 'default
                    'follow-link 'mouse-face
                    'keymap outline-button-icon-map))
      (list 'outline-open
            (if outline--use-rtl 'outline-close-rtl 'outline-close))))))

(defun outline--insert-button (type)
  (save-excursion
    (forward-line 0)
    (let ((icon (nth (if (eq type 'close) 1 0) outline--button-icons))
          (o (seq-find (lambda (o) (overlay-get o 'outline-button))
                       (overlays-at (point)))))
      (unless o
        (when (eq outline-minor-mode-use-buttons 'insert)
          (let ((inhibit-read-only t))
            (insert (apply #'propertize "  " (text-properties-at (point))))
            (forward-line 0)))
        (setq o (make-overlay (point) (1+ (point))))
        (overlay-put o 'outline-button t)
        (overlay-put o 'evaporate t))
      (pcase outline-minor-mode-use-buttons
        ('insert
         (overlay-put o 'display (or (plist-get icon 'image)
                                     (plist-get icon 'string)))
         (overlay-put o 'face (plist-get icon 'face))
         (overlay-put o 'follow-link 'mouse-face)
         (overlay-put o 'mouse-face 'highlight)
         (overlay-put o 'keymap outline-inserted-button-map))
        ('in-margins
         (overlay-put o 'before-string icon)
         (overlay-put o 'keymap outline-overlay-button-map))
        (_
         (overlay-put o 'before-string icon)
         (overlay-put o 'keymap outline-overlay-button-map))))))

(defun outline--fix-up-all-buttons (from to)
  (when outline-minor-mode-use-buttons
    ;; If `outline-minor-mode-use-buttons' is `insert',
    ;; `outline--insert-button' can modify the buffer's text.  We shouldn't
    ;; use `with-silent-modifications' around changes to the buffer's text,
    ;; but we still don't want to mark the buffer as modified whenever
    ;; we expand/collapse an element.
    (let ((modified (buffer-modified-p)))
      (outline-map-region
       (lambda ()
         (let ((close-p (save-excursion
                          (outline-end-of-heading)
                          (seq-some (lambda (o)
                                      (eq (overlay-get o 'invisible) 'outline))
                                    (overlays-at (point))))))
           (outline--insert-button (if close-p 'close 'open))))
       from to)
      (restore-buffer-modified-p modified))))

(defun outline--fix-buttons (&optional beg end)
  ;; Handle whole lines
  (save-excursion
    (setq beg (if (null beg) (point-min) (goto-char beg) (pos-bol)))
    ;; Include a final newline in the region, otherwise
    ;; `outline-search-text-property' may consider a heading to be outside
    ;; of the bounds.
    (setq end (if (null end) (point-max) (goto-char end) (pos-bol 2)))
    (when (eq outline-minor-mode-use-buttons 'insert)
      ;; `outline--remove-buttons' may change the buffer's text.
      (setq end (copy-marker end t)))
    (outline--remove-buttons beg end)
    (save-match-data (outline--fix-up-all-buttons beg end))
    `(jit-lock-bounds ,beg . ,end)))

(defun outline--remove-buttons (beg end)
  (if (not (eq outline-minor-mode-use-buttons 'insert))
      (remove-overlays beg end 'outline-button t)
    (save-excursion
      (dolist (ol (overlays-in beg end))
        (when (overlay-get ol 'outline-button)
          (goto-char (overlay-start ol))
          (let ((inhibit-read-only t))
            (when (looking-at "  ") (delete-char 2)))
          (delete-overlay ol))))))


(defvar-keymap outline-navigation-repeat-map
  :repeat t
  "C-b" #'outline-backward-same-level
  "b"   #'outline-backward-same-level
  "C-f" #'outline-forward-same-level
  "f"   #'outline-forward-same-level
  "C-n" #'outline-next-visible-heading
  "n"   #'outline-next-visible-heading
  "C-p" #'outline-previous-visible-heading
  "p"   #'outline-previous-visible-heading
  "C-u" #'outline-up-heading
  "u"   #'outline-up-heading)

(defvar-keymap outline-editing-repeat-map
  :repeat t
  "C-v" #'outline-move-subtree-down
  "v"   #'outline-move-subtree-down
  "C-^" #'outline-move-subtree-up
  "^"   #'outline-move-subtree-up
  "C->" #'outline-demote
  ">"   #'outline-demote
  "C-<" #'outline-promote
  "<"   #'outline-promote)


(provide 'outline)
(provide 'noutline)

;;; outline.el ends here
