;;; which-key.el --- Display available keybindings in popup  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Justin Burkett

;; Author: Justin Burkett <justin@burkett.cc>
;; URL: https://github.com/justbur/emacs-which-key
;; Version: 0.7
;; Keywords:
;; Package-Requires: ((emacs "24.3"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; which-key is a minor mode for Emacs that displays the key bindings following
;; your currently entered incomplete command (a prefix) in a popup. For example,
;; after enabling the minor mode if you enter C-x and wait for the default of 1
;; second the minibuffer will expand with all of the available key bindings that
;; follow C-x (or as many as space allows given your settings). This includes
;; prefixes like C-x 8 which are shown in a different face. Screenshots of what
;; the popup will look like along with information about additional features can
;; be found at https://github.com/justbur/emacs-which-key.
;;
;; which-key started as a rewrite of guide-key
;; (https://github.com/kai2nenobu/guide-key), but the feature sets have since
;; diverged.

;;; Code:

(require 'cl-lib)

(eval-when-compile
  (defvar golden-ratio-mode))

(defgroup which-key nil
  "Customization options for which-key-mode"
  :group 'help
  :prefix "which-key-")

(defcustom which-key-idle-delay 1.0
  "Delay (in seconds) for which-key buffer to popup."
  :group 'which-key
  :type 'float)

(defcustom which-key-echo-keystrokes 0
  "Value to use for `echo-keystrokes'.
This only applies if `which-key-popup-type' is minibuffer or
`which-key-show-prefix' is echo. It needs to be less than
`which-key-idle-delay' or else the keystroke echo will erase the
which-key popup."
  :group 'which-key
  :type 'float)

(defcustom which-key-max-description-length 27
  "Truncate the description of keys to this length.
Also adds \"..\". If nil, disable any truncation."
  :group 'which-key
  :type 'integer)

(defcustom which-key-separator " → "
  "Separator to use between key and description."
  :group 'which-key
  :type 'string)

(defcustom which-key-unicode-correction 3
  "Correction for wide unicode characters.
Since we measure width in terms of the number of characters,
Unicode characters that are wider than ASCII characters throw off
the calculation for available width in the which-key buffer.  This
variable allows you to adjust for the wide unicode characters by
artificially reducing the available width in the buffer.

The default of 3 means allow for the total extra width
contributed by any wide unicode characters to be up to one
additional ASCII character in the which-key buffer.  Increase this
number if you are seeing charaters get cutoff on the right side
of the which-key popup."
  :group 'which-key
  :type 'integer)

(defcustom which-key-key-replacement-alist
  '(("<\\([[:alnum:]-]+\\)>" . "\\1") ("left" . "←") ("right" . "→"))
  "The strings in the car of each cons are replaced with the
strings in the cdr for each key.  Elisp regexp can be used as
in the first example."
  :group 'which-key
  :type '(alist :key-type regexp :value-type string))

(defcustom which-key-description-replacement-alist
  '(("Prefix Command" . "prefix") ("which-key-show-next-page" . "wk next pg")
    ("\\`\\?\\?\\'" . "lambda"))
  "See `which-key-key-replacement-alist'.
This is a list of lists for replacing descriptions."
  :group 'which-key
  :type '(alist :key-type regexp :value-type string))

(defcustom which-key-highlighted-command-list '()
  "A list of strings and/or cons cells used to highlight certain
commands. If the element is a string, assume it is a regexp
pattern for matching command names and use
`which-key-highlighted-command-face' for any matching names. If
the element is a cons cell, it should take the form (regexp .
face to apply)."
  :group 'which-key)

(defcustom which-key-special-keys '("SPC" "TAB" "RET" "ESC" "DEL")
  "These keys will automatically be truncated to one character
and have `which-key-special-key-face' applied to them."
  :group 'which-key
  :type '(repeat string))

(defcustom which-key-buffer-name " *which-key*"
  "Name of which-key buffer."
  :group 'which-key
  :type 'string)

(defcustom which-key-show-prefix 'echo
  "Whether to and where to display the current prefix sequence.
Possible choices are echo for echo area (the default), left, top
and nil. Nil turns the feature off."
  :group 'which-key
  :type '(radio (const :tag "Left of keys" left)
                (const :tag "In first line" top)
                (const :tag "In echo area" echo)
                (const :tag "Hide" nil)))

(defcustom which-key-popup-type 'side-window
  "Supported types are minibuffer, side-window, frame, and custom."
  :group 'which-key
  :type '(radio (const :tag "Show in minibuffer" minibuffer)
                (const :tag "Show in side window" side-window)
                (const :tag "Show in popup frame" frame)
                (const :tag "Use your custom display functions" custom)))

(defcustom which-key-min-display-lines 1
  "The minimum number of horizontal lines to display in the
  which-key buffer."
  :group 'which-key
  :type 'integer)

(defcustom which-key-side-window-location 'bottom
  "Location of which-key popup when `which-key-popup-type' is side-window.
Should be one of top, bottom, left or right. You can also specify
a list of two locations, like (right bottom). In this case, the
first location is tried. If there is not enough room, the second
location is tried."
  :group 'which-key
  :type '(radio (const right)
                (const bottom)
                (const left)
                (const top)
                (const (right bottom))
                (const (bottom right))))

(defcustom which-key-side-window-max-width 0.333
  "Maximum width of which-key popup when type is side-window and
location is left or right.
This variable can also be a number between 0 and 1. In that case, it denotes
a percentage out of the frame's width."
  :group 'which-key
  :type 'float)

(defcustom which-key-side-window-max-height 0.25
  "Maximum height of which-key popup when type is side-window and
location is top or bottom.
This variable can also be a number between 0 and 1. In that case, it denotes
a percentage out of the frame's height."
  :group 'which-key
  :type 'float)

(defcustom which-key-frame-max-width 60
  "Maximum width of which-key popup when type is frame."
  :group 'which-key
  :type 'integer)

(defcustom which-key-frame-max-height 20
  "Maximum height of which-key popup when type is frame."
  :group 'which-key
  :type 'integer)

(defcustom which-key-show-remaining-keys nil
  "Show remaining keys in last slot, when keys are hidden."
  :group 'which-key
  :type '(radio (const :tag "Yes" t)
                (const :tag "No" nil)))

(defcustom which-key-sort-order 'which-key-key-order
  "If nil, do not resort the output from
`describe-buffer-bindings' which groups by mode. Ordering options
are

1. `which-key-key-order': by key (default)
2. `which-key-description-order': by description
3. `which-key-prefix-then-key-order': prefix (no prefix first) then key

See the README and the docstrings for those functions for more
information."
  :group 'which-key
  :type 'function)

(defcustom which-key-paging-prefixes '()
  "Enable paging for these prefixes."
  :group 'which-key
  :type '(repeat string))

(defcustom which-key-paging-key "<f5>"
  "Key to use for changing pages. Bound after each of the
prefixes in `which-key-paging-prefixes'"
  :group 'which-key
  :type 'string)

;; (defcustom which-key-undo-key nil
;;   "Key (string) to use for undoing keypresses. Bound recursively
;; in each of the maps in `which-key-undo-keymaps'."
;;   :group 'which-key
;;   :type 'string)

;; (defcustom which-key-undo-keymaps '()
;;   "Keymaps in which to bind `which-key-undo-key'"
;;   :group 'which-key
;;   :type '(repeat symbol))

(defcustom which-key-use-C-h-for-paging t
  "Use C-h for paging if non-nil. Normally C-h after a prefix
  calls `describe-prefix-bindings'. This changes that command to
  a which-key paging command when which-key-mode is active."
  :group 'which-key
  :type 'boolean)

(defcustom which-key-prevent-C-h-from-cycling t
  "When using C-h for paging, which-key overrides the default
  behavior of calling `describe-prefix-bindings'. Setting this
  variable to t makes it so that when on the last page, pressing
  C-h calls the default function instead of cycling pages. If you
  want which-key to cycle, set this to nil."
  :group 'which-key
  :type 'boolean)

(defcustom which-key-allow-evil-operators (boundp 'evil-this-operator)
  "Allow popup to show for evil operators. The popup is normally
  inhibited in the middle of commands, but setting this to
  non-nil will override this behavior for evil operators."
  :group 'which-key
  :type 'boolean)

;; Faces
(defgroup which-key-faces nil
  "Faces for which-key-mode"
  :group 'which-key
  :prefix "which-key-")

(defface which-key-key-face
  '((t . (:inherit font-lock-constant-face)))
  "Face for which-key keys"
  :group 'which-key-faces)

(defface which-key-separator-face
  '((t . (:inherit font-lock-comment-face)))
  "Face for the separator (default separator is an arrow)"
  :group 'which-key-faces)

(defface which-key-note-face
  '((t . (:inherit which-key-separator-face)))
  "Face for notes or hints occasionally provided"
  :group 'which-key-faces)

(defface which-key-command-description-face
  '((t . (:inherit font-lock-function-name-face)))
  "Face for the key description when it is a command"
  :group 'which-key-faces)

(defface which-key-local-map-description-face
  '((t . (:inherit which-key-command-description-face)))
  "Face for the key description when it is found in `current-local-map'"
  :group 'which-key-faces)

(defface which-key-highlighted-command-face
  '((t . (:inherit which-key-command-description-face :underline t)))
  "Default face for the command description when it is a command
and it matches a string in `which-key-highlighted-command-list'."
  :group 'which-key-faces)

(defface which-key-group-description-face
  '((t . (:inherit font-lock-keyword-face)))
  "Face for the key description when it is a group or prefix"
  :group 'which-key-faces)

(defface which-key-special-key-face
  '((t . (:inherit which-key-key-face :inverse-video t :weight bold)))
  "Face for special keys (SPC, TAB, RET)"
  :group 'which-key-faces)

;; Custom popup
(defcustom which-key-custom-popup-max-dimensions-function nil
  "Variable to hold a custom max-dimensions function.
Will be passed the width of the active window and is expected to
return the maximum height in lines and width in characters of the
which-key popup in the form a cons cell (height . width)."
  :group 'which-key
  :type 'function)

(defcustom which-key-custom-hide-popup-function nil
  "Variable to hold a custom hide-popup function.
It takes no arguments and the return value is ignored."
  :group 'which-key
  :type 'function)

(defcustom which-key-custom-show-popup-function nil
  "Variable to hold a custom show-popup function.
Will be passed the required dimensions in the form (height .
width) in lines and characters respectively.  The return value is
ignored."
  :group 'which-key
  :type 'function)

(defvar which-key-inhibit nil
  "Prevent which-key from popping up momentarily by setting this
to a non-nil value for the execution of a command. Like this

\(let \(\(which-key-inhibit t\)\)
...\)")

;; Internal Vars
(defvar which-key--buffer nil
  "Internal: Holds reference to which-key buffer.")
(defvar which-key--window nil
  "Internal: Holds reference to which-key window.")
(defvar which-key--timer nil
  "Internal: Holds reference to open window timer.")
(defvar which-key--paging-timer nil
  "Internal: Holds reference to timer for paging.")
(defvar which-key--is-setup nil
  "Internal: Non-nil if which-key buffer has been setup.")
(defvar which-key--frame nil
  "Internal: Holds reference to which-key frame.
Used when `which-key-popup-type' is frame.")
(defvar which-key--echo-keystrokes-backup nil
  "Internal: Backup the initial value of `echo-keystrokes'.")
(defvar which-key--prefix-help-cmd-backup nil
  "Internal: Backup the value of `prefix-help-command'.")
(defvar which-key--pages-plist nil
  "Internal: Holds page objects")
(defvar which-key--lighter-backup nil
  "Internal: Holds lighter backup")
(defvar which-key--current-prefix nil
  "Internal: Holds current prefix")
(defvar which-key--current-page-n nil
  "Internal: Current pages of showing buffer. Nil means no buffer
showing.")
(defvar which-key--on-last-page nil
  "Internal: Non-nil if showing last page.")
(defvar which-key--last-try-2-loc nil
  "Internal: Last location of side-window when two locations
used.")
(defvar which-key--multiple-locations nil)

(defvar which-key-key-based-description-replacement-alist '()
  "New version of
`which-key-key-based-description-replacement-alist'. Use
`which-key-add-key-based-replacements' or
`which-key-add-major-mode-key-based-replacements' to set this
variable.")

(defvar which-key-prefix-name-alist '()
  "An alist with elements of the form (key-sequence . prefix-name).
key-sequence is a sequence of the sort produced by applying `kbd'
then `listify-key-sequence' to create a canonical version of the
key sequence. prefix-name is a string.")

(defvar which-key-prefix-title-alist '()
  "An alist with elements of the form (key-sequence . prefix-title).
key-sequence is a sequence of the sort produced by applying `kbd'
then `listify-key-sequence' to create a canonical version of the
key sequence. prefix-title is a string. The title is displayed
alongside the actual current key sequence when
`which-key-show-prefix' is set to either top or echo.")


;;;###autoload
(define-minor-mode which-key-mode
  "Toggle which-key-mode."
  :global t
  :lighter " WK"
  :keymap (let ((map (make-sparse-keymap)))
            (mapc
             (lambda (prefix)
               (define-key map
                 (kbd (concat prefix " " which-key-paging-key))
                 #'which-key-show-next-page))
             which-key-paging-prefixes)
            map)
  (if which-key-mode
      (progn
        (setq which-key--echo-keystrokes-backup echo-keystrokes)
        (unless which-key--is-setup (which-key--setup))
        (unless (eq prefix-help-command 'which-key-show-next-page)
          (setq which-key--prefix-help-cmd-backup prefix-help-command))
        (when which-key-use-C-h-for-paging
            (setq prefix-help-command #'which-key-show-next-page))
        (when which-key-show-remaining-keys
          (add-hook 'pre-command-hook #'which-key--lighter-restore))
        (add-hook 'pre-command-hook #'which-key--hide-popup)
        (add-hook 'focus-out-hook #'which-key--stop-timer)
        (add-hook 'focus-in-hook #'which-key--start-timer)
        (which-key--start-timer))
    (setq echo-keystrokes which-key--echo-keystrokes-backup)
    (when which-key--prefix-help-cmd-backup
      (setq prefix-help-command which-key--prefix-help-cmd-backup))
    (when which-key-show-remaining-keys
      (remove-hook 'pre-command-hook #'which-key--lighter-restore))
    (remove-hook 'pre-command-hook #'which-key--hide-popup)
    (remove-hook 'focus-out-hook #'which-key--stop-timer)
    (remove-hook 'focus-in-hook #'which-key--start-timer)
    (which-key--stop-timer)))

(defun which-key--init-buffer ()
  "Initialize which-key buffer"
  (unless (buffer-live-p which-key--buffer)
    (setq which-key--buffer (get-buffer-create which-key-buffer-name))
    (with-current-buffer which-key--buffer
      ;; suppress confusing minibuffer message
      (let (message-log-max)
        (toggle-truncate-lines 1)
        (message ""))
      (setq-local cursor-type nil)
      (setq-local cursor-in-non-selected-windows nil)
      (setq-local mode-line-format nil))))

(defun which-key--setup ()
  "Initial setup for which-key.
Reduce `echo-keystrokes' if necessary (it will interfer if it's
set too high) and setup which-key buffer."
  (when (or (eq which-key-show-prefix 'echo)
            (eq which-key-popup-type 'minibuffer))
    (which-key--setup-echo-keystrokes))
  (which-key--check-key-based-alist)
  ;; (which-key--setup-undo-key)
  (which-key--init-buffer)
  (setq which-key--is-setup t))

(defun which-key--setup-echo-keystrokes ()
  "Reduce `echo-keystrokes' if necessary (it will interfer if
it's set too high)."
  (let (;(previous echo-keystrokes)
        )
    (when (and echo-keystrokes
               (> (abs (- echo-keystrokes which-key-echo-keystrokes)) 0.000001))
      (if (> which-key-idle-delay which-key-echo-keystrokes)
          (setq echo-keystrokes which-key-echo-keystrokes)
        (setq which-key-echo-keystrokes (/ (float which-key-idle-delay) 4)
              echo-keystrokes which-key-echo-keystrokes))
      ;; (message "which-key: echo-keystrokes changed from %s to %s"
      ;;          previous echo-keystrokes)
      )))

;; (defun which-key--setup-undo-key ()
;;   "Bind `which-key-undo-key' in `which-key-undo-keymaps'."
;;   (when (and which-key-undo-key which-key-undo-keymaps)
;;     (dolist (map which-key-undo-keymaps)
;;       (which-key-define-key-recursively
;;        map (kbd which-key-undo-key) 'which-key-undo))))

(defun which-key--check-key-based-alist ()
  "Check (and fix if necessary) `which-key-key-based-description-replacement-alist'"
  (let ((alist which-key-key-based-description-replacement-alist)
        old-style res)
    (dolist (cns alist)
      (cond ((listp (car cns))
             (push cns res))
            ((stringp (car cns))
             (setq old-style t)
             (push (cons (listify-key-sequence (kbd (car cns))) (cdr cns)) res))
            ((symbolp (car cns))
             (let (new-mode-alist)
               (dolist (cns2 (cdr cns))
                 (cond ((listp (car cns2))
                        (push cns2 new-mode-alist))
                       ((stringp (car cns2))
                        (setq old-style t)
                        (push (cons (listify-key-sequence (kbd (car cns2))) (cdr cns2))
                              new-mode-alist))))
               (push (cons (car cns) new-mode-alist) res)))
            (t (message "which-key: there's a problem with the \
entry %s in which-key-key-based-replacement-alist" cns))))
    (setq which-key-key-based-description-replacement-alist res)
    (when old-style
      (message "which-key: \
 `which-key-key-based-description-replacement-alist' has changed format and you\
 seem to be using the old format. Please use the functions \
`which-key-add-key-based-replacements' and \
`which-key-add-major-mode-key-based-replacements' instead."))))

;; Default configuration functions for use by users. Should be the "best"
;; configurations

;;;###autoload
(defun which-key-setup-side-window-right ()
  "Apply suggested settings for side-window that opens on right."
  (interactive)
  (setq which-key-popup-type 'side-window
        which-key-side-window-location 'right
        which-key-show-prefix 'top))

;;;###autoload
(defun which-key-setup-side-window-right-bottom ()
  "Apply suggested settings for side-window that opens on right
if there is space and the bottom otherwise."
  (interactive)
  (setq which-key-popup-type 'side-window
        which-key-side-window-location '(right bottom)
        which-key-show-prefix 'top))

;;;###autoload
(defun which-key-setup-side-window-bottom ()
  "Apply suggested settings for side-window that opens on
bottom."
  (interactive)
  (which-key--setup-echo-keystrokes)
  (setq which-key-popup-type 'side-window
        which-key-side-window-location 'bottom
        which-key-show-prefix 'echo))

;;;###autoload
(defun which-key-setup-minibuffer ()
  "Apply suggested settings for minibuffer."
  (interactive)
  (which-key--setup-echo-keystrokes)
  (setq which-key-popup-type 'minibuffer
        which-key-show-prefix 'left))

;; Helper functions to modify replacement lists.

(defun which-key--add-key-val-to-alist (alist key value &optional alist-name)
  "Internal function to add (KEY . VALUE) to ALIST."
  (when (or (not (stringp key)) (not (stringp value)))
    (error "which-key: Error %s (key) and %s (value) should be strings"
           key value))
  (let ((key-lst (listify-key-sequence (kbd key))))
    (cond ((null alist) (list (cons key-lst value)))
          ((assoc key-lst alist)
           (when (not (string-equal (cdr (assoc key-lst alist)) value))
             (message "which-key: changing %s name from %s to %s in the %s alist"
                      key (cdr (assoc key-lst alist)) value alist-name)
             (setcdr (assoc key-lst alist) value))
           alist)
          (t (cons (cons key-lst value) alist)))))

;;;###autoload
(defun which-key-add-key-based-replacements (key-sequence replacement &rest more)
  "Replace the description of KEY-SEQUENCE with REPLACEMENT.
Both KEY-SEQUENCE and REPLACEMENT should be strings.  For Example,

\(which-key-add-key-based-replacements \"C-x 1\" \"maximize\"\)

MORE allows you to specifcy additional KEY REPL pairs.  All
replacements are added to
`which-key-key-based-description-replacement-alist'."
  ;; TODO: Make interactive
  (while key-sequence
    (setq which-key-key-based-description-replacement-alist
          (which-key--add-key-val-to-alist
           which-key-key-based-description-replacement-alist
           key-sequence replacement "key-based"))
    (setq key-sequence (pop more) replacement (pop more))))
(put 'which-key-add-key-based-replacements 'lisp-indent-function 'defun)

;;;###autoload
(defun which-key-add-major-mode-key-based-replacements (mode key-sequence replacement &rest more)
  "Functions like `which-key-add-key-based-replacements'.
The difference is that MODE specifies the `major-mode' that must
be active for KEY-SEQUENCE and REPLACEMENT (MORE contains
addition KEY-SEQUENCE REPLACEMENT pairs) to apply."
  ;; TODO: Make interactive
  (when (not (symbolp mode))
    (error "MODE should be a symbol corresponding to a value of major-mode"))
  (let ((mode-alist (cdr (assq mode which-key-key-based-description-replacement-alist))))
    (while key-sequence
      (setq mode-alist (which-key--add-key-val-to-alist
                        mode-alist key-sequence replacement
                        (format "key-based-%s" mode)))
      (setq key-sequence (pop more) replacement (pop more)))
    (if (assq mode which-key-key-based-description-replacement-alist)
        (setcdr (assq mode which-key-key-based-description-replacement-alist) mode-alist)
      (push (cons mode mode-alist) which-key-key-based-description-replacement-alist))))
(put 'which-key-add-major-mode-key-based-replacements 'lisp-indent-function 'defun)

;;;###autoload
(defun which-key-add-prefix-title (key-seq-str title &optional force)
  "Deprecated in favor of `which-key-declare-prefixes'.

Add title for KEY-SEQ-STR given by TITLE. FORCE, if non-nil, will
add the new title even if one already exists. KEY-SEQ-STR should
be a key sequence string suitable for `kbd' and TITLE should be a
string."
  (let ((key-seq-lst (listify-key-sequence (kbd key-seq-str))))
    (if (and (null force)
             (assoc key-seq-lst which-key-prefix-title-alist))
        (message "which-key: Prefix title not added. A title exists for this prefix.")
      (push (cons key-seq-lst title) which-key-prefix-title-alist))))

;;;###autoload
(defun which-key-declare-prefixes (key-sequence name &rest more)
  "Name the KEY-SEQUENCE prefix NAME.
KEY-SEQUENCE should be a string, acceptable to `kbd'. NAME can be
a string or a cons cell of two strings. In the first case, the
string is used as both the name and the title (the title is
displayed in the echo area only). For Example,

\(which-key-declare-prefixes \"C-x 8\" \"unicode\"\)

or

\(which-key-declare-prefixes \"C-x 8\" (\"unicode\" . \"Unicode Chararcters\")\)

MORE allows you to specifcy additional KEY-SEQUENCE NAME pairs.
All names are added to `which-key-prefix-names-alist' and titles
to `which-key-prefix-title-alist'."
  (while key-sequence
    (let ((-name (if (consp name) (car name) name))
          (-title (if (consp name) (cdr name) name)))
        (setq which-key-prefix-name-alist
              (which-key--add-key-val-to-alist
               which-key-prefix-name-alist key-sequence -name "prefix-name")
              which-key-prefix-title-alist
              (which-key--add-key-val-to-alist
               which-key-prefix-title-alist key-sequence -title "prefix-title")))
    (setq key-sequence (pop more) name (pop more))))
(put 'which-key-declare-prefixes 'lisp-indent-function 'defun)

;;;###autoload
(defun which-key-declare-prefixes-for-mode (mode key-sequence name &rest more)
  "Functions like `which-key-declare-prefix-names'.
The difference is that MODE specifies the `major-mode' that must
be active for KEY-SEQUENCE and NAME (MORE contains
addition KEY-SEQUENCE NAME pairs) to apply."
  (when (not (symbolp mode))
    (error "MODE should be a symbol corresponding to a value of major-mode"))
  (let ((mode-name-alist (cdr (assq mode which-key-prefix-name-alist)))
        (mode-title-alist (cdr (assq mode which-key-prefix-title-alist)))
        (-name (if (consp name) (car name) name))
        (-title (if (consp name) (cdr name) name)))
    (while key-sequence
      (setq mode-name-alist (which-key--add-key-val-to-alist
                             mode-name-alist key-sequence -name
                             (format "prefix-name-%s" mode))
            mode-title-alist (which-key--add-key-val-to-alist
                              mode-title-alist key-sequence -title
                              (format "prefix-name-%s" mode)))
      (setq key-sequence (pop more) name (pop more)))
    (if (assq mode which-key-prefix-name-alist)
        (setcdr (assq mode which-key-prefix-name-alist) mode-name-alist)
      (push (cons mode mode-name-alist) which-key-prefix-name-alist))))
(put 'which-key-declare-prefixes-for-mode 'lisp-indent-function 'defun)

(defun which-key-define-key-recursively (map key def &optional recursing)
  "Recursively bind KEY in MAP to DEF on every level of MAP except the first.
RECURSING is for internal use."
  (when recursing (define-key map key def))
  (map-keymap
   (lambda (_ev df)
     (when (keymapp df)
       (which-key-define-key-recursively df key def t)))
   map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions for computing window sizes

(defun which-key--text-width-to-total (text-width)
  "Convert window text-width to window total-width.
TEXT-WIDTH is the desired text width of the window.  The function
calculates what total width is required for a window in the
selected to have a text-width of TEXT-WIDTH columns.  The
calculation considers possible fringes and scroll bars.  This
function assumes that the desired window has the same character
width as the frame."
  (let ((char-width (frame-char-width)))
    (+ text-width
       (/ (frame-fringe-width) char-width)
       (/ (frame-scroll-bar-width) char-width)
       (if (which-key--char-enlarged-p) 1 0)
       ;; add padding to account for possible wide (unicode) characters
       3)))

(defun which-key--total-width-to-text (total-width)
  "Convert window total-width to window text-width.
TOTAL-WIDTH is the desired total width of the window.  The function calculates
what text width fits such a window.  The calculation considers possible fringes
and scroll bars.  This function assumes that the desired window has the same
character width as the frame."
  (let ((char-width (frame-char-width)))
    (- total-width
       (/ (frame-fringe-width) char-width)
       (/ (frame-scroll-bar-width) char-width)
       (if (which-key--char-enlarged-p) 1 0)
       ;; add padding to account for possible wide (unicode) characters
       3)))

(defun which-key--char-enlarged-p (&optional _frame)
  (> (frame-char-width)
     (/ (float (frame-pixel-width)) (window-total-width (frame-root-window)))))

(defun which-key--char-reduced-p (&optional _frame)
  (< (frame-char-width)
     (/ (float (frame-pixel-width)) (window-total-width (frame-root-window)))))

(defun which-key--char-exact-p (&optional _frame)
  (= (frame-char-width)
     (/ (float (frame-pixel-width)) (window-total-width (frame-root-window)))))

(defun which-key--width-or-percentage-to-width (width-or-percentage)
  "Return window total width.
If WIDTH-OR-PERCENTAGE is a whole number, return it unchanged.  Otherwise, it
should be a percentage (a number between 0 and 1) out of the frame's width.
More precisely, it should be a percentage out of the frame's root window's
total width."
  (if (wholenump width-or-percentage)
      width-or-percentage
    (round (* width-or-percentage (window-total-width (frame-root-window))))))

(defun which-key--height-or-percentage-to-height (height-or-percentage)
  "Return window total height.
If HEIGHT-OR-PERCENTAGE is a whole number, return it unchanged.  Otherwise, it
should be a percentage (a number between 0 and 1) out of the frame's height.
More precisely, it should be a percentage out of the frame's root window's
total height."
  (if (wholenump height-or-percentage)
      height-or-percentage
    (round (* height-or-percentage (window-total-height (frame-root-window))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Show/hide which-key buffer

(defun which-key--hide-popup ()
  "This function is called to hide the which-key buffer."
  (unless (eq real-this-command 'which-key-show-next-page)
    (setq which-key--current-page-n nil
          which-key--on-last-page nil)
    (cl-case which-key-popup-type
      ;; Not necessary to hide minibuffer
      ;; (minibuffer (which-key--hide-buffer-minibuffer))
      (side-window (which-key--hide-buffer-side-window))
      (frame (which-key--hide-buffer-frame))
      (custom (funcall which-key-custom-hide-popup-function)))))

(defun which-key--hide-popup-ignore-command ()
  "Version of `which-key--hide-popup' without the check of
`real-this-command'."
  (cl-case which-key-popup-type
    (side-window (which-key--hide-buffer-side-window))
    (frame (which-key--hide-buffer-frame))
    (custom (funcall which-key-custom-hide-popup-function))))

(defun which-key--hide-buffer-side-window ()
  "Hide which-key buffer when side-window popup is used."
  (when (buffer-live-p which-key--buffer)
    ;; in case which-key buffer was shown in an existing window, `quit-window'
    ;; will re-show the previous buffer, instead of closing the window
    (quit-windows-on which-key--buffer)))

(defun which-key--hide-buffer-frame ()
  "Hide which-key buffer when frame popup is used."
  (when (frame-live-p which-key--frame)
    (delete-frame which-key--frame)))

(defun which-key--show-popup (act-popup-dim)
  "Show the which-key buffer.
ACT-POPUP-DIM includes the dimensions, (height . width) of the
buffer text to be displayed in the popup.  Return nil if no window
is shown, or if there is no need to start the closing timer."
  (when (and (> (car act-popup-dim) 0) (> (cdr act-popup-dim) 0))
    (cl-case which-key-popup-type
      ;; Not called for minibuffer
      ;; (minibuffer (which-key--show-buffer-minibuffer act-popup-dim))
      (side-window (which-key--show-buffer-side-window act-popup-dim))
      (frame (which-key--show-buffer-frame act-popup-dim))
      (custom (funcall which-key-custom-show-popup-function act-popup-dim)))))

(defun which-key--fit-buffer-to-window-horizontally (&optional window &rest params)
  "Slightly modified version of `fit-buffer-to-window'.
Use &rest params because `fit-buffer-to-window' has a different
call signature in different emacs versions"
  (let ((fit-window-to-buffer-horizontally t))
    (apply #'fit-window-to-buffer window params)))

(defun which-key--show-buffer-side-window (_act-popup-dim)
  "Show which-key buffer when popup type is side-window."
  (let* ((side which-key-side-window-location)
         (alist '((window-width . which-key--fit-buffer-to-window-horizontally)
                  (window-height . (lambda (w) (fit-window-to-buffer w nil 1))))))
    ;; Note: `display-buffer-in-side-window' and `display-buffer-in-major-side-window'
    ;; were added in Emacs 24.3

    ;; If two side windows exist in the same side, `display-buffer-in-side-window'
    ;; will use on of them, which isn't desirable. `display-buffer-in-major-side-window'
    ;; will pop a new window, so we use that.
    ;; +-------------------------+         +-------------------------+
    ;; |     regular window      |         |     regular window      |
    ;; |                         |         +------------+------------+
    ;; +------------+------------+   -->   | side-win 1 | side-win 2 |
    ;; | side-win 1 | side-win 2 |         |------------+------------|
    ;; |            |            |         |     which-key window    |
    ;; +------------+------------+         +------------+------------+
    ;; (display-buffer which-key--buffer (cons 'display-buffer-in-side-window alist))
    ;; side defaults to bottom
    (cond
     ((eq which-key--multiple-locations t)
      ;; possibly want to switch sides in this case so we can't reuse the window
      (delete-windows-on which-key--buffer)
      (display-buffer-in-major-side-window which-key--buffer side 0 alist))
     ((get-buffer-window which-key--buffer)
      (display-buffer-reuse-window which-key--buffer alist))
     (t
      (display-buffer-in-major-side-window which-key--buffer side 0 alist)))))

(defun which-key--show-buffer-frame (act-popup-dim)
  "Show which-key buffer when popup type is frame."
  (let* (;(orig-window (selected-window))
         (frame-height (+ (car act-popup-dim)
                          (if (with-current-buffer which-key--buffer
                                mode-line-format)
                              1
                            0)))
         ;; without adding 2, frame sometimes isn't wide enough for the buffer.
         ;; this is probably because of the fringes. however, setting fringes
         ;; sizes to 0 (instead of adding 2) didn't always make the frame wide
         ;; enough. don't know why it is so.
         (frame-width (+ (cdr act-popup-dim) 2))
         (new-window (if (and (frame-live-p which-key--frame)
                              (eq which-key--buffer
                                  (window-buffer (frame-root-window which-key--frame))))
                         (which-key--show-buffer-reuse-frame frame-height frame-width)
                       (which-key--show-buffer-new-frame frame-height frame-width))))
    (when new-window
      ;; display successful
      (setq which-key--frame (window-frame new-window))
      new-window)))

(defun which-key--show-buffer-new-frame (frame-height frame-width)
  "Helper for `which-key--show-buffer-frame'."
  (let* ((frame-params `((height . ,frame-height)
                         (width . ,frame-width)
                         ;; tell the window manager to respect the given sizes
                         (user-size . t)
                         ;; which-key frame doesn't need a minibuffer
                         (minibuffer . nil)
                         (name . "which-key")
                         ;; no need for scroll bars in which-key frame
                         (vertical-scroll-bars . nil)
                         ;; (left-fringe . 0)
                         ;; (right-fringe . 0)
                         ;; (right-divider-width . 0)
                         ;; make sure frame is visible
                         (visibility . t)))
         (alist `((pop-up-frame-parameters . ,frame-params)))
         (orig-frame (selected-frame))
         (new-window (display-buffer-pop-up-frame which-key--buffer alist)))
    (when new-window
      ;; display successful
      (redirect-frame-focus (window-frame new-window) orig-frame)
      new-window)))

(defun which-key--show-buffer-reuse-frame (frame-height frame-width)
  "Helper for `which-key--show-buffer-frame'."
  (let ((window
         (display-buffer-reuse-window which-key--buffer
                                      `((reusable-frames . ,which-key--frame)))))
    (when window
      ;; display successful
      (set-frame-size (window-frame window) frame-width frame-height)
      window)))

;; Keep for popwin maybe (Used to work)
;; (defun which-key-show-buffer-popwin (height width)
;;   "Using popwin popup buffer with dimensions HEIGHT and WIDTH."
;;   (popwin:popup-buffer which-key-buffer-name
;;                        :height height
;;                        :width width
;;                        :noselect t
;;                        :position which-key-side-window-location))

;; (defun which-key-hide-buffer-popwin ()
;;   "Hide popwin buffer."
;;   (when (eq popwin:popup-buffer (get-buffer which-key--buffer))
;;     (popwin:close-popup-window)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Max dimension of available window functions

(defun which-key--popup-max-dimensions (selected-window-width)
  "Dimesion functions should return the maximum possible (height
. width) of the intended popup. SELECTED-WINDOW-WIDTH is the
width of currently active window, not the which-key buffer
window."
  (cl-case which-key-popup-type
    (minibuffer (which-key--minibuffer-max-dimensions))
    (side-window (which-key--side-window-max-dimensions))
    (frame (which-key--frame-max-dimensions))
    (custom (funcall which-key-custom-popup-max-dimensions-function selected-window-width))))

(defun which-key--minibuffer-max-dimensions ()
  "Return max-dimensions of minibuffer (height . width).
Measured in lines and characters respectively."
  (cons
   ;; height
   (if (floatp max-mini-window-height)
       (floor (* (frame-text-lines)
                 max-mini-window-height))
     max-mini-window-height)
   ;; width
   (frame-text-cols)))

(defun which-key--side-window-max-dimensions ()
  "Return max-dimensions of the side-window popup (height .
width) in lines and characters respectively."
  (cons
   ;; height
   (if (member which-key-side-window-location '(left right))
       ;; 1 is a kludge to make sure there is no overlap
       (- (frame-height) (window-text-height (minibuffer-window)) 1)
     ;; (window-mode-line-height which-key--window))
     ;; FIXME: change to something like (min which-*-height (calculate-max-height))
     (which-key--height-or-percentage-to-height which-key-side-window-max-height))
   ;; width
   (if (member which-key-side-window-location '(left right))
       (which-key--total-width-to-text (which-key--width-or-percentage-to-width
                                        which-key-side-window-max-width))
     (frame-width))))

(defun which-key--frame-max-dimensions ()
  "Return max-dimensions of the frame popup (height .
width) in lines and characters respectively."
  (cons which-key-frame-max-height which-key-frame-max-width))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sorting functions

(defun which-key--alpha< (a b)
  (let ((da (downcase a))
        (db (downcase b)))
    (if (string-equal da db)
        (not (string-lessp a b))
      (string-lessp da db))))

(defun which-key--key-description-alpha< (a b)
  "Sorting function used for `which-key-key-order-alpha'."
  (let* ((aem? (string-equal a ""))
         (bem? (string-equal b ""))
         (a1? (= 1 (length a)))
         (b1? (= 1 (length b)))
         (srgxp "^\\(RET\\|SPC\\|TAB\\|DEL\\|LFD\\|ESC\\|NUL\\)")
         (asp? (string-match-p srgxp a))
         (bsp? (string-match-p srgxp b))
         (prrgxp "^\\(M\\|C\\|S\\|A\\|H\\|s\\)-")
         (apr? (string-match-p prrgxp a))
         (bpr? (string-match-p prrgxp b)))
    (cond ((or aem? bem?) (and aem? (not bem?)))
          ((and asp? bsp?)
           (if (string-equal (substring a 0 3) (substring b 0 3))
               (which-key--key-description< (substring a 3) (substring b 3))
             (string-lessp a b)))
          ((or asp? bsp?) asp?)
          ((and a1? b1?) (which-key--alpha< a b))
          ((or a1? b1?) a1?)
          ((and apr? bpr?)
           (if (string-equal (substring a 0 2) (substring b 0 2))
               (which-key--key-description< (substring a 2) (substring b 2))
             (string-lessp a b)))
          ((or apr? bpr?) apr?)
          (t (string-lessp a b)))))

(defsubst which-key-key-order-alpha (acons bcons)
  "Order key descriptions A and B.
Order is lexicographic within a \"class\", where the classes and
the ordering of classes are listed below.

special (SPC,TAB,...) < single char < mod (C-,M-,...) < other.
Sorts single characters alphabetically with lowercase coming
before upper."
  (which-key--key-description-alpha< (car acons) (car bcons)))

(defun which-key--key-description< (a b)
  "Sorting function used for `which-key-key-order'."
  (let* ((aem? (string-equal a ""))
         (bem? (string-equal b ""))
         (a1? (= 1 (length a)))
         (b1? (= 1 (length b)))
         (srgxp "^\\(RET\\|SPC\\|TAB\\|DEL\\|LFD\\|ESC\\|NUL\\)")
         (asp? (string-match-p srgxp a))
         (bsp? (string-match-p srgxp b))
         (prrgxp "^\\(M\\|C\\|S\\|A\\|H\\|s\\)-")
         (apr? (string-match-p prrgxp a))
         (bpr? (string-match-p prrgxp b)))
    (cond ((or aem? bem?) (and aem? (not bem?)))
          ((and asp? bsp?)
           (if (string-equal (substring a 0 3) (substring b 0 3))
               (which-key--key-description< (substring a 3) (substring b 3))
             (string-lessp a b)))
          ((or asp? bsp?) asp?)
          ((and a1? b1?) (string-lessp a b))
          ((or a1? b1?) a1?)
          ((and apr? bpr?)
           (if (string-equal (substring a 0 2) (substring b 0 2))
               (which-key--key-description< (substring a 2) (substring b 2))
             (string-lessp a b)))
          ((or apr? bpr?) apr?)
          (t (string-lessp a b)))))

(defsubst which-key-key-order (acons bcons)
  "Order key descriptions A and B.
Order is lexicographic within a \"class\", where the classes and
the ordering of classes are listed below.

special (SPC,TAB,...) < single char < mod (C-,M-,...) < other."
  (which-key--key-description< (car acons) (car bcons)))

(defsubst which-key-description-order (acons bcons)
  "Order descriptions of A and B.
Uses `string-lessp' after applying lowercase."
  (string-lessp (downcase (cdr acons)) (downcase (cdr bcons))))

(defun which-key-prefix-then-key-order (acons bcons)
  "Order first by whether A and/or B is a prefix with no prefix
coming before a prefix. Within these categories order using
`which-key-key-order'."
  (let ((apref? (which-key--group-p (cdr acons)))
        (bpref? (which-key--group-p (cdr bcons))))
    (if (not (eq apref? bpref?))
        (and (not apref?) bpref?)
      (which-key-key-order acons bcons))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions for retrieving and formatting keys

(defsubst which-key--safe-lookup-key (keymap key)
  "Version of `lookup-key' that allows KEYMAP to be nil. KEY is not checked."
  (when (keymapp keymap) (lookup-key keymap key)))

(defun which-key--maybe-replace (string repl-alist &optional literal)
  "Perform replacements on STRING.
REPL-ALIST is an alist where the car of each element is the text
to replace and the cdr is the replacement text.  Unless LITERAL is
non-nil regexp is used in the replacements.  Whether or not a
replacement occurs return the new STRING."
  (save-match-data
    (let ((new-string string))
      (dolist (repl repl-alist)
        (when (string-match (car repl) new-string)
          (setq new-string
                (replace-match (cdr repl) t literal new-string))))
      new-string)))

(defsubst which-key--current-key-list (&optional key-str)
  (append (listify-key-sequence which-key--current-prefix)
          (when key-str
            (listify-key-sequence (kbd key-str)))))

(defsubst which-key--current-key-string (&optional key-str)
  (key-description
   (append (listify-key-sequence which-key--current-prefix)
           (when key-str
             (listify-key-sequence (kbd key-str))))))

(defun which-key--maybe-replace-prefix-name (key-lst desc)
  "KEY-LST is a list of keys produced by `listify-key-sequences'
and DESC is the description that is possibly replaced using the
`which-key-prefix-name-alist'. Whether or not a replacement
occurs return the new STRING."
  (let* ((alist which-key-prefix-name-alist)
         (res (assoc key-lst alist))
         (mode-alist (assq major-mode alist))
         (mode-res (when mode-alist (assoc key-lst mode-alist))))
    (cond (mode-res (cdr mode-res))
          (res (cdr res))
          (t desc))))

(defun which-key--maybe-get-prefix-title (key-lst)
  "KEY-LST is a list of keys produced by `listify-key-sequences'.
A title is possibly returned using `which-key-prefix-title-alist'.
An empty stiring is returned if no title exists."
  (let* ((alist which-key-prefix-title-alist)
         (res (assoc key-lst alist))
         (mode-alist (assq major-mode alist))
         (mode-res (when mode-alist (assoc key-lst mode-alist))))
    (cond (mode-res (cdr mode-res))
          (res (cdr res))
          (t ""))))

(defun which-key--maybe-replace-key-based (string key-lst)
  "KEY-LST is a list of keys produced by `listify-key-sequences'
and STRING is the description that is possibly replaced using the
`which-key-key-based-description-replacement-alist'. Whether or
not a replacement occurs return the new STRING."
  (let* ((alist which-key-key-based-description-replacement-alist)
         (str-res (assoc key-lst alist))
         (mode-alist (assq major-mode alist))
         (mode-res (when mode-alist (assoc key-lst mode-alist))))
    (cond (mode-res (cdr mode-res))
          (str-res (cdr str-res))
          (t string))))

(defun which-key--propertize-key (key)
  "Add a face to KEY.
If KEY contains any \"special keys\" defined in
`which-key-special-keys' then truncate and add the corresponding
`which-key-special-key-face'."
  (let ((key-w-face (propertize key 'face 'which-key-key-face))
        (regexp (concat "\\("
                        (mapconcat 'identity which-key-special-keys
                                   "\\|") "\\)")))
    (save-match-data
      (if (and which-key-special-keys
               (string-match regexp key))
          (let ((beg (match-beginning 0)) (end (match-end 0)))
            (concat (substring key-w-face 0 beg)
                    (propertize (substring key-w-face beg (1+ beg))
                                'face 'which-key-special-key-face)
                    (substring key-w-face end (string-width key-w-face))))
        key-w-face))))

(defsubst which-key--truncate-description (desc)
  "Truncate DESC description to `which-key-max-description-length'."
  (if (and which-key-max-description-length
           (> (string-width desc) which-key-max-description-length))
      (concat (substring desc 0 which-key-max-description-length) "..")
    desc))

(defsubst which-key--group-p (description)
  (or (string-match-p "^\\(group:\\|Prefix\\)" description)
      (keymapp (intern description))))

(defun which-key--highlight-face (description)
  "Return the highlight face for DESCRIPTION if it has one."
  (let (face)
    (dolist (el which-key-highlighted-command-list)
      (unless face
        (cond ((consp el)
               (when (string-match-p (car el) description)
                 (setq face (cdr el))))
              ((stringp el)
               (when (string-match-p el description)
                 (setq face 'which-key-highlighted-command-face)))
              (t
               (message "which-key: warning: element %s of \
which-key-highlighted-command-list is not a string or a cons
cell" el)))))
    face))

(defun which-key--propertize-description (description group local hl-face)
  "Add face to DESCRIPTION where the face chosen depends on
whether the description represents a group or a command. Also
make some minor adjustments to the description string, like
removing a \"group:\" prefix."
  (let* ((desc description)
         (desc (if (string-match-p "^group:" desc)
                   (substring desc 6) desc))
         (desc (if group (concat "+" desc) desc))
         (desc (which-key--truncate-description desc)))
    (propertize desc 'face
                (cond (hl-face hl-face)
                      (group 'which-key-group-description-face)
                      (local 'which-key-local-map-description-face)
                      (t 'which-key-command-description-face)))))

(defun which-key--format-and-replace (unformatted)
  "Take a list of (key . desc) cons cells in UNFORMATTED, add
faces and perform replacements according to the three replacement
alists. Returns a list (key separator description)."
  (let ((sep-w-face
         (propertize which-key-separator 'face 'which-key-separator-face))
        (local-map (current-local-map)))
    (mapcar
     (lambda (key-desc-cons)
       (let* ((key (car key-desc-cons))
              (desc (cdr key-desc-cons))
              (group (which-key--group-p desc))
              (keys (which-key--current-key-string key))
              (key-lst (which-key--current-key-list key))
              (local (eq (which-key--safe-lookup-key local-map (kbd keys))
                         (intern desc)))
              (hl-face (which-key--highlight-face desc))
              (key (which-key--maybe-replace
                    key which-key-key-replacement-alist))
              (desc (which-key--maybe-replace
                     desc which-key-description-replacement-alist))
              (desc (which-key--maybe-replace-key-based desc key-lst))
              (desc (if group
                        (which-key--maybe-replace-prefix-name key-lst desc)
                      desc))
              (key-w-face (which-key--propertize-key key))
              (desc-w-face (which-key--propertize-description desc group local hl-face)))
         (list key-w-face sep-w-face desc-w-face)))
     unformatted)))

(defun which-key--get-formatted-key-bindings ()
  "Uses `describe-buffer-bindings' to collect the key bindings in
BUFFER that follow the key sequence KEY-SEQ."
  (let ((key-str-qt (regexp-quote (key-description which-key--current-prefix)))
        (buffer (current-buffer))
        key-match desc-match unformatted)
    (save-match-data
      (with-temp-buffer
        (describe-buffer-bindings buffer which-key--current-prefix)
        (goto-char (point-max)) ; want to put last keys in first
        (while (re-search-backward
                (format "^%s \\([^ \t]+\\)[ \t]+\\(\\(?:[^ \t\n]+ ?\\)+\\)$"
                        key-str-qt)
                nil t)
          (setq key-match (match-string 1)
                desc-match (match-string 2))
          (cl-pushnew (cons key-match desc-match) unformatted
                      :test (lambda (x y) (string-equal (car x) (car y)))))))
    (when which-key-sort-order
      (setq unformatted
            (sort unformatted (lambda (a b) (funcall which-key-sort-order a b)))))
    (which-key--format-and-replace unformatted)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions for laying out which-key buffer pages

(defun which-key--pad (columns)
  "Pad COLUMNS to the same length using empty strings."
  (let ((max-len (cl-reduce (lambda (a x) (max a (length x))) columns
                            :initial-value 0)))
    (mapcar
     (lambda (c)
       (if (< (length c) max-len)
           (append c (make-list (- max-len (length c)) ""))
         c))
     columns)))

(defsubst which-key--join-columns (columns)
  "Transpose columns into rows, concat rows into lines and rows into page."
  (let* ((padded (which-key--pad (nreverse columns)))
         (rows (apply #'cl-mapcar #'list padded)))
    (mapconcat (lambda (row) (mapconcat #'identity row " ")) rows "\n")))

(defsubst which-key--max-len (keys index)
  "Internal function for finding the max length of the INDEX
element in each list element of KEYS."
  (cl-reduce
   (lambda (x y) (max x (string-width (nth index y)))) keys :initial-value 0))

(defun which-key--pad-column (col-keys)
  "Take a column of (key separator description) COL-KEYS,
calculate the max width in the column and pad all cells out to
that width."
  (let* ((col-key-width  (which-key--max-len col-keys 0))
         (col-sep-width  (which-key--max-len col-keys 1))
         (col-desc-width (which-key--max-len col-keys 2))
         (col-width      (+ 1 col-key-width col-sep-width col-desc-width)))
    (cons col-width
          (mapcar (lambda (k)
                    (format (concat "%" (int-to-string col-key-width)
                                    "s%s%-" (int-to-string col-desc-width) "s")
                            (nth 0 k) (nth 1 k) (nth 2 k)))
                  col-keys))))

(defun which-key--partition-list (n list)
  "Partition LIST into N-sized sublists."
  (let (res)
    (while list
      (setq res (cons (cl-subseq list 0 (min n (length list))) res)
            list (nthcdr n list)))
    (nreverse res)))

(defun which-key--partition-columns (keys avl-lines avl-width)
  "Convert list of KEYS to columns based on dimensions AVL-LINES and AVL-WIDTH.
Returns a plist that holds the page strings, as well as metadata."
  (let ((cols-w-widths (mapcar #'which-key--pad-column
                               (which-key--partition-list avl-lines keys)))
        (page-width 0) (n-pages 0) (n-keys 0)
        page-cols pages page-widths keys/page col)
    (if (> (apply #'max (mapcar #'car cols-w-widths)) avl-width)
        ;; give up if no columns fit
        (list :pages nil :page-height 0 :page-widths '(0)
              :keys/page '(0) :n-pages 0 :tot-keys 0)
      (while cols-w-widths
        ;; start new page
        (cl-incf n-pages)
        (setq col (pop cols-w-widths)
              page-cols (list (cdr col))
              page-width (car col)
              n-keys (length (cdr col)))
        ;; add additional columns as long as they fit
        (while (and cols-w-widths
                    (<= (+ (caar cols-w-widths) page-width) avl-width))
          (setq col (pop cols-w-widths))
          (push (cdr col) page-cols)
          (cl-incf page-width (car col))
          (cl-incf n-keys (length (cdr col))))
        (push (which-key--join-columns page-cols) pages)
        (push n-keys keys/page)
        (push page-width page-widths))
      (list :pages (nreverse pages) :page-height avl-lines
            :page-widths (nreverse page-widths)
            :keys/page (nreverse keys/page) :n-pages n-pages
            :tot-keys (apply #'+ keys/page)))))

(defun which-key--create-pages (keys sel-win-width)
  "Create page strings using `which-key--partition-columns'.
Will try to find the best number of rows and columns using the
given dimensions and the length and wdiths of KEYS. SEL-WIN-WIDTH
is the width of the live window."
  (let* ((max-dims (which-key--popup-max-dimensions sel-win-width))
         (max-lines (car max-dims))
         (max-width (cdr max-dims))
         (prefix-keys-desc (key-description which-key--current-prefix))
         (prefix-w-face (which-key--propertize-key prefix-keys-desc))
         (prefix-left (when (eq which-key-show-prefix 'left)
                        (+ 2 (string-width prefix-w-face))))
         (prefix-top (eq which-key-show-prefix 'top))
         (avl-lines (if prefix-top (- max-lines 1) max-lines))
         (min-lines (min avl-lines which-key-min-display-lines))
         (avl-width (if prefix-left (- max-width prefix-left) max-width))
         (vertical (and (eq which-key-popup-type 'side-window)
                        (member which-key-side-window-location '(left right))))
         (result (which-key--partition-columns keys avl-lines avl-width))
         found prev-result)
    (cond ((or vertical (> (plist-get result :n-pages) 1) (= 1 avl-lines))
           result)
          ;; do a simple search for the smallest number of lines
          (t (while (and (> avl-lines min-lines) (not found))
               (setq avl-lines (- avl-lines 1)
                     prev-result result
                     result (which-key--partition-columns
                             keys avl-lines avl-width)
                     found (> (plist-get result :n-pages) 1)))
             (if found prev-result result)))))

(defun which-key--lighter-status (n-shown n-tot)
  "Possibly show N-SHOWN keys and N-TOT keys in the mode line."
  (when which-key-show-remaining-keys
    (setq which-key--lighter-backup (cadr (assq 'which-key-mode minor-mode-alist)))
    (setcar (cdr (assq 'which-key-mode minor-mode-alist))
            (format " WK: %s/%s keys" n-shown n-tot))))

(defun which-key--lighter-restore ()
  "Restore the lighter for which-key."
  (when which-key-show-remaining-keys
    (setcar (cdr (assq 'which-key-mode minor-mode-alist)) which-key--lighter-backup)))

(defun which-key--echo (text)
  "Echo TEXT to minibuffer without logging.
Slight delay gets around evil functions that clear the echo
area."
  (let* ((minibuffer (eq which-key-popup-type 'minibuffer))
         (delay (if minibuffer 0.2 0.01))
         message-log-max)
    (unless minibuffer (message "%s" text))
    (run-with-idle-timer
     delay nil (lambda () (let (message-log-max)
                            (message "%s" text))))))

(defun which-key--next-page-hint (prefix-keys page-n n-pages)
  "Return string for next page hint."
  (let* ((paging-key (concat prefix-keys " " which-key-paging-key))
         (paging-key-bound (eq 'which-key-show-next-page
                               (key-binding (kbd paging-key))))
         (key (if paging-key-bound which-key-paging-key "C-h"))
         (next-page-n (format "pg %s" (1+ (mod (1+ page-n) n-pages))))
         (use-descbind (and which-key--on-last-page which-key-use-C-h-for-paging
                            which-key-prevent-C-h-from-cycling)))
    (when (and (or (and (< 1 n-pages) which-key-use-C-h-for-paging)
                   (and (< 1 n-pages) paging-key-bound)
                   use-descbind)
               (not (and which-key-allow-evil-operators
                         (boundp 'evil-this-operator)
                         evil-this-operator)))
      (propertize (format "[%s %s]" key
                          (if use-descbind "help" next-page-n))
                  'face 'which-key-note-face))))

(defun which-key--show-page (n)
  "Show page N, starting from 0."
  (which-key--init-buffer) ;; in case it was killed
  (let ((n-pages (plist-get which-key--pages-plist :n-pages))
        (prefix-keys (key-description which-key--current-prefix))
        page-n golden-ratio-mode)
    (if (= 0 n-pages)
        (message "%s- which-key can't show keys: There is not \
enough space based on your settings and frame size." prefix-keys)
      (setq page-n (mod n n-pages)
            which-key--current-page-n page-n)
      (when (= n-pages (1+ n)) (setq which-key--on-last-page t))
      (let* ((page (nth page-n (plist-get which-key--pages-plist :pages)))
             (height (plist-get which-key--pages-plist :page-height))
             (width (nth page-n (plist-get which-key--pages-plist :page-widths)))
             (n-shown (nth page-n (plist-get which-key--pages-plist :keys/page)))
             (n-tot (plist-get which-key--pages-plist :tot-keys))
             (prefix-w-face (which-key--propertize-key prefix-keys))
             (dash-w-face (propertize "-" 'face 'which-key-key-face))
             (status-left (propertize (format "%s/%s" (1+ page-n) n-pages)
                                      'face 'which-key-separator-face))
             (status-top (propertize (which-key--maybe-get-prefix-title
                                      (which-key--current-key-list))
                                     'face 'which-key-note-face))
             (status-top (concat status-top
                                 (when (< 1 n-pages)
                                   (propertize (format " (%s of %s)"
                                                       (1+ page-n) n-pages)
                                               'face 'which-key-note-face))))
             (first-col-width (+ 2 (max (string-width prefix-w-face)
                                        (string-width status-left))))
             (prefix-left (format (concat "%-" (int-to-string first-col-width) "s")
                                  prefix-w-face))
             (status-left (format (concat "%-" (int-to-string first-col-width) "s")
                                  status-left))
             (nxt-pg-hint (which-key--next-page-hint prefix-keys page-n n-pages))
             new-end lines first)
        (cond ((and (< 1 n-pages)
                    (eq which-key-show-prefix 'left))
               (setq lines (split-string page "\n")
                     first (concat prefix-left (car lines) "\n" status-left)
                     new-end (concat "\n" (make-string first-col-width 32))
                     page  (concat first (mapconcat #'identity (cdr lines) new-end))))
              ((eq which-key-show-prefix 'left)
               (if (= 1 height)
                   (setq page (concat prefix-left page))
                 (setq lines (split-string page "\n")
                       first (concat prefix-left (car lines)
                                     "\n" (make-string first-col-width 32))
                       new-end (concat "\n" (make-string first-col-width 32))
                       page  (concat first (mapconcat #'identity (cdr lines) new-end)))))
              ((eq which-key-show-prefix 'top)
               (setq page (concat prefix-w-face dash-w-face " "
                                  status-top " " nxt-pg-hint "\n" page)))
              ((eq which-key-show-prefix 'echo)
               (which-key--echo (concat prefix-w-face dash-w-face " "
                                        status-top " " nxt-pg-hint))))
        (which-key--lighter-status n-shown n-tot)
        (if (eq which-key-popup-type 'minibuffer)
            (which-key--echo page)
          (with-current-buffer which-key--buffer
            (erase-buffer)
            (insert page)
            (goto-char (point-min)))
          (which-key--show-popup (cons height width)))))))

(defun which-key-show-next-page ()
  "Show the next page of keys.
Will force an update if called before `which-key--update'."
  (interactive)
  (cond
   ;; on last page and want default C-h behavior
   ((and which-key--current-page-n
         which-key--on-last-page
         which-key-use-C-h-for-paging
         which-key-prevent-C-h-from-cycling)
    (which-key--hide-popup-ignore-command)
    (which-key--stop-timer)
    (funcall which-key--prefix-help-cmd-backup)
    (which-key--start-timer))
   ;; No which-key buffer showing
   ((null which-key--current-page-n)
    (let* ((keysbl
            (vconcat (butlast (append (this-single-command-keys) nil))))
           (next-event
            (mapcar (lambda (ev) (cons t ev)) (listify-key-sequence keysbl))))
      (which-key--stop-timer)
      (setq unread-command-events next-event)
      (which-key--create-buffer-and-show keysbl)
      (which-key--start-timer)))
   ;; which-key buffer showing. turn page
   (t
    (let ((next-event
           (mapcar (lambda (ev) (cons t ev)) (which-key--current-key-list)))
          (next-page
           (if which-key--current-page-n (1+ which-key--current-page-n) 0)))
      (which-key--stop-timer)
      (setq unread-command-events next-event)
      (if which-key--last-try-2-loc
          (let ((which-key-side-window-location which-key--last-try-2-loc)
                (which-key--multiple-locations t))
            (which-key--show-page next-page))
        (which-key--show-page next-page))
      (which-key--start-paging-timer)))))

(defun which-key-undo ()
  "Undo last keypress and force which-key update."
  (interactive)
  (let* ((key-str (this-command-keys))
         (key-str (substring key-str 0 (- (length key-str) 2)))
         (ev (mapcar (lambda (ev) (cons t ev)) (listify-key-sequence key-str))))
    (which-key--stop-timer)
    (setq unread-command-events ev)
    (which-key--create-buffer-and-show key-str)
    (which-key--start-timer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Update

(defun which-key--try-2-side-windows (keys page-n loc1 loc2 &rest _ignore)
  "Try to show KEYS (PAGE-N) in LOC1 first. Only if no keys fit fallback to LOC2."
  (let (pages1)
    (let ((which-key-side-window-location loc1)
          (which-key--multiple-locations t))
      (setq pages1 (which-key--create-pages keys (window-width))))
    (if (< 0 (plist-get pages1 :n-pages))
        (progn
          (setq which-key--pages-plist pages1)
          (let ((which-key-side-window-location loc1)
                (which-key--multiple-locations t))
            (which-key--show-page page-n))
          loc1)
      (let ((which-key-side-window-location loc2)
            (which-key--multiple-locations t))
        (setq which-key--pages-plist (which-key--create-pages
                                      keys (window-width)))
        (which-key--show-page page-n)
        loc2))))


(defun which-key--create-buffer-and-show (prefix-keys)
  "Fill `which-key--buffer' with key descriptions and reformat.
Finally, show the buffer."
  (setq which-key--current-prefix prefix-keys
        which-key--last-try-2-loc nil)
  (let ((formatted-keys (which-key--get-formatted-key-bindings))
        (prefix-keys-desc (key-description prefix-keys)))
    (cond ((= (length formatted-keys) 0)
           (message "%s-  which-key: There are no keys to show" prefix-keys-desc))
          ((listp which-key-side-window-location)
           (setq which-key--last-try-2-loc
                 (apply #'which-key--try-2-side-windows
                        formatted-keys 0 which-key-side-window-location)))
          (t (setq which-key--pages-plist
                   (which-key--create-pages formatted-keys (window-width)))
             (which-key--show-page 0)))))

(defun which-key--update ()
  "Function run by timer to possibly trigger `which-key--create-buffer-and-show'."
  (let ((prefix-keys (this-single-command-keys)))
    ;; (when (> (length prefix-keys) 0)
    ;;  (message "key: %s" (key-description prefix-keys)))
    ;; (when (> (length prefix-keys) 0)
    ;;  (message "key binding: %s" (key-binding prefix-keys)))
    (when (and (> (length prefix-keys) 0)
               (or (keymapp (key-binding prefix-keys))
                   ;; Some keymaps are stored here like iso-transl-ctl-x-8-map
                   (keymapp (which-key--safe-lookup-key
                             key-translation-map prefix-keys))
                   ;; just in case someone uses one of these
                   (keymapp (which-key--safe-lookup-key
                             function-key-map prefix-keys)))
               (not which-key-inhibit)
               ;; Do not display the popup if a command is currently being
               ;; executed
               (or (and which-key-allow-evil-operators evil-this-operator)
                   (null this-command)))
      (which-key--create-buffer-and-show prefix-keys))))

;; Timers

(defun which-key--start-timer ()
  "Activate idle timer to trigger `which-key--update'."
  (which-key--stop-timer)
  (setq which-key--timer
        (run-with-idle-timer which-key-idle-delay t #'which-key--update)))

(defun which-key--stop-timer ()
  "Deactivate idle timer for `which-key--update'."
  (when which-key--timer (cancel-timer which-key--timer)))

(defun which-key--start-paging-timer ()
  "Activate timer to restart which-key after paging."
  (when which-key--paging-timer (cancel-timer which-key--paging-timer))
  (setq which-key--paging-timer
        (run-with-idle-timer
         0.2 t (lambda ()
                 (when (or (not (eq real-last-command 'which-key-show-next-page))
                           (and (< 0 (length (this-single-command-keys)))
                                (not (equal which-key--current-prefix
                                            (this-single-command-keys)))))
                   (setq which-key--current-page-n nil
                         which-key--on-last-page nil)
                   (cancel-timer which-key--paging-timer)
                   (which-key--start-timer))))))

(provide 'which-key)
;;; which-key.el ends here
