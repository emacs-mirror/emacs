;;; which-key.el --- Display available keybindings in popup  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Justin Burkett

;; Author: Justin Burkett <justin@burkett.cc>
;; URL: https://github.com/justbur/emacs-which-key
;; Version: 1.1.8
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
(require 'button)

;; For compiler
(defvar evil-operator-shortcut-map)
(defvar evil-operator-state-map)
(defvar evil-motion-state-map)
(defvar golden-ratio-mode)
(declare-function evil-get-command-property "ext:evil-common.el")

(defgroup which-key nil
  "Customization options for which-key-mode"
  :group 'help
  :prefix "which-key-")

(defcustom which-key-idle-delay 1.0
  "Delay (in seconds) for which-key buffer to popup."
  :group 'which-key
  :type 'float)

(defcustom which-key-idle-secondary-delay nil
  "Once the which-key buffer shows once for a key sequence reduce
the idle time to this amount (in seconds). This makes it possible
to shorten the delay for subsequent popups in the same key
sequence. The default is for this value to be nil, which disables
this behavior."
  :group 'which-key
  :type 'float)

(defcustom which-key-echo-keystrokes (if (and echo-keystrokes
                                              (> echo-keystrokes
                                                 which-key-idle-delay))
                                         (/ (float which-key-idle-delay) 4)
                                       echo-keystrokes)
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

(defcustom which-key-add-column-padding 0
  "Additional padding (number of spaces) to add to the left of
each key column."
  :group 'which-key
  :type 'integer)

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

(defcustom which-key-dont-use-unicode nil
  "If non-nil, don't use any unicode characters in default setup."
  :group 'which-key
  :type 'integer)

(defcustom which-key-separator
  (if which-key-dont-use-unicode " : " " → ")
  "Separator to use between key and description. Default is \" →
\", unless `which-key-dont-use-unicode' is non nil, in which case
the default is \" : \"."
  :group 'which-key
  :type 'string)

(defcustom which-key-prefix-prefix "+"
  "String to insert in front of prefix commands (i.e., commands
that represent a sub-map). Default is \"+\"."
  :group 'which-key
  :type 'string)

(defcustom which-key-key-replacement-alist
  (if which-key-dont-use-unicode
      '(("<\\([[:alnum:]-]+\\)>" . "\\1"))
    '(("<\\([[:alnum:]-]+\\)>" . "\\1") ("left" . "←") ("right" . "→")))
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

(defcustom which-key-special-keys '()
  "These keys will automatically be truncated to one character
and have `which-key-special-key-face' applied to them. This is
disabled by default. Try this to see the effect.

\(setq which-key-special-keys '(\"SPC\" \"TAB\" \"RET\" \"ESC\" \"DEL\")\)"
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
  :type '(radio (const :tag "Left of the keys" left)
                (const :tag "In the first line" top)
                (const :tag "In the last line" bottom)
                (const :tag "In the echo area" echo)
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

(defcustom which-key-use-C-h-commands t
  "Use C-h for paging if non-nil. Normally C-h after a prefix
  calls `describe-prefix-bindings'. This changes that command to
  a which-key paging command when which-key-mode is active."
  :group 'which-key
  :type 'boolean)
(defvaralias 'which-key-use-C-h-for-paging
  'which-key-use-C-h-commands)
(make-obsolete-variable 'which-key-use-C-h-for-paging
                        'which-key-use-C-h-commands
                        "2015-12-2")

(defcustom which-key-is-verbose nil
  "Whether to warn about potential mistakes in configuration."
  :group 'which-key
  :type 'boolean)

(defvar which-key-C-h-map
  (let ((map (make-sparse-keymap)))
    (dolist (bind '(("\C-a" . which-key-abort)
                    ("a" . which-key-abort)
                    ("\C-h" . which-key-show-standard-help)
                    ("h" . which-key-show-standard-help)
                    ("\C-n" . which-key-show-next-page-cycle)
                    ("n" . which-key-show-next-page-cycle)
                    ("\C-p" . which-key-show-previous-page-cycle)
                    ("p" . which-key-show-previous-page-cycle)
                    ("\C-u" . which-key-undo-key)
                    ("u" . which-key-undo-key)))
      (define-key map (car bind) (cdr bind)))
    map)
  "Keymap for C-h commands.")

(defvar which-key--paging-functions '(which-key-C-h-dispatch
                                      which-key-turn-page
                                      which-key-show-next-page
                                      which-key-show-next-page-cycle
                                      which-key-show-next-page-no-cycle
                                      which-key-show-previous-page-cycle
                                      which-key-show-previous-page-no-cycle
                                      which-key-undo-key
                                      which-key-undo))

(defcustom which-key-prevent-C-h-from-cycling t
  "When using C-h for paging, which-key overrides the default
  behavior of calling `describe-prefix-bindings'. Setting this
  variable to t makes it so that when on the last page, pressing
  C-h calls the default function instead of cycling pages. If you
  want which-key to cycle, set this to nil."
  :group 'which-key
  :type 'boolean)
(make-obsolete-variable 'which-key-prevent-C-h-from-cycling
                        "No longer applies. See `which-key-C-h-dispatch'"
                        "2015-12-2")

(defcustom which-key-hide-alt-key-translations t
  "Hide key translations using Alt key if non nil.
These translations are not relevant most of the times since a lot
of terminals issue META modifier for the Alt key.

See http://www.gnu.org/software/emacs/manual/html_node/emacs/Modifier-Keys.html"
  :group 'which-key
  :type 'boolean)

;; Hooks
(defvar which-key-init-buffer-hook '()
  "Hook run when which-key buffer is initialized.")

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

(defcustom which-key-lighter " WK"
  "Minor mode lighter to use in the mode-line."
  :group 'which-key
  :type 'string)

(defvar which-key-inhibit nil
  "Prevent which-key from popping up momentarily by setting this
to a non-nil value for the execution of a command. Like this

\(let \(\(which-key-inhibit t\)\)
...\)")

(defvar which-key-keymap-history nil
  "History of keymap selections in functions like
`which-key-show-keymap'.")

;; Internal Vars
(defvar which-key--buffer nil
  "Internal: Holds reference to which-key buffer.")
;; (defvar which-key--window nil
;;   "Internal: Holds reference to which-key window.")
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
(defvar which-key--using-top-level nil)
(defvar which-key--using-show-keymap nil)
(defvar which-key--using-show-operator-keymap nil)
(defvar which-key--inhibit-next-operator-popup nil)
(defvar which-key--current-show-keymap-name nil)
(defvar which-key--prior-show-keymap-args nil)

(defvar which-key-key-based-description-replacement-alist '()
  "New version of
`which-key-key-based-description-replacement-alist'. Use
`which-key-add-key-based-replacements' or
`which-key-add-major-mode-key-based-replacements' to set this
variable.")

(defvar which-key-prefix-name-alist '()
  "An alist with elements of the form (key-sequence . prefix-name).
key-sequence is a sequence of the sort produced by applying
`key-description' to create a canonical version of the key
sequence. prefix-name is a string.")

(defvar which-key-prefix-title-alist '()
  "An alist with elements of the form (key-sequence . prefix-title).
key-sequence is a sequence of the sort produced by applying
`key-description' to create a canonical version of the key
sequence. prefix-title is a string. The title is displayed
alongside the actual current key sequence when
`which-key-show-prefix' is set to either top or echo.")


;; Third-party library support

;; Evil
(defcustom which-key-allow-evil-operators (boundp 'evil-this-operator)
  "Allow popup to show for evil operators. The popup is normally
  inhibited in the middle of commands, but setting this to
  non-nil will override this behavior for evil operators."
  :group 'which-key
  :type 'boolean)

(defcustom which-key-show-operator-state-maps nil
  "Experimental: Try to show the right keys following an evil
command that reads a motion, such as \"y\", \"d\" and \"c\" from
normal state. This is experimental, because there might be some
valid keys missing and it might be showing some invalid keys."
  :group 'which-key
  :type 'boolean)

;; God-mode
(defvar which-key--god-mode-support-enabled nil
  "Support god-mode if non-nil. This is experimental,
so you need to explicitly opt-in for now. Please report any
problems at github.")

(defvar which-key--god-mode-key-string nil
  "Holds key string to use for god-mode support.")

(defadvice god-mode-lookup-command
    (around which-key--god-mode-lookup-command-advice disable)
  (setq which-key--god-mode-key-string (ad-get-arg 0))
  (unwind-protect
      ad-do-it
    (when (bound-and-true-p which-key-mode)
      (which-key--hide-popup))))

(defun which-key-enable-god-mode-support (&optional disable)
  "Enable support for god-mode if non-nil. This is experimental,
so you need to explicitly opt-in for now. Please report any
problems at github. If DISABLE is non-nil disable support."
  (interactive "P")
  (setq which-key--god-mode-support-enabled (null disable))
  (if disable
      (ad-disable-advice
       'god-mode-lookup-command
       'around 'which-key--god-mode-lookup-command-advice)
    (ad-enable-advice
     'god-mode-lookup-command
     'around 'which-key--god-mode-lookup-command-advice))
  (ad-activate 'god-mode-lookup-command))

;;;###autoload
(define-minor-mode which-key-mode
  "Toggle which-key-mode."
  :global t
  :lighter which-key-lighter
  :keymap (let ((map (make-sparse-keymap)))
            (mapc
             (lambda (prefix)
               (define-key map
                 (kbd (concat prefix " " which-key-paging-key))
                 #'which-key-C-h-dispatch))
             which-key-paging-prefixes)
            map)
  (if which-key-mode
      (progn
        (setq which-key--echo-keystrokes-backup echo-keystrokes)
        (unless which-key--is-setup (which-key--setup))
        (unless (member prefix-help-command which-key--paging-functions)
          (setq which-key--prefix-help-cmd-backup prefix-help-command))
        (when which-key-use-C-h-commands
          (setq prefix-help-command #'which-key-C-h-dispatch))
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
      (setq-local mode-line-format nil)
      (setq-local word-wrap nil)
      (setq-local show-trailing-whitespace nil)
      (run-hooks 'which-key-init-buffer-hook))))

(defun which-key--setup ()
  "Initial setup for which-key.
Reduce `echo-keystrokes' if necessary (it will interfere if it's
set too high) and setup which-key buffer."
  (when (or (eq which-key-show-prefix 'echo)
            (eq which-key-popup-type 'minibuffer))
    (which-key--setup-echo-keystrokes))
  ;; (which-key--check-key-based-alist)
  ;; (which-key--setup-undo-key)
  (which-key--init-buffer)
  (setq which-key--is-setup t))

(defun which-key--setup-echo-keystrokes ()
  "Reduce `echo-keystrokes' if necessary (it will interfere if
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

(defun which-key-remove-default-unicode-chars ()
  "Use of `which-key-dont-use-unicode' is preferred to this
function, but it's included here in case someone cannot set that
variable early enough in their configuration, if they are using a
starter kit for example."
  (when (string-equal which-key-separator " → ")
    (setq which-key-separator " : "))
  (setq which-key-key-replacement-alist
        (delete '("left" . "←") which-key-key-replacement-alist))
  (setq which-key-key-replacement-alist
        (delete '("right" . "→") which-key-key-replacement-alist)))

;; (defun which-key--check-key-based-alist ()
;;   "Check (and fix if necessary) `which-key-key-based-description-replacement-alist'"
;;   (let ((alist which-key-key-based-description-replacement-alist)
;;         old-style res)
;;     (dolist (cns alist)
;;       (cond ((listp (car cns))
;;              (push cns res))
;;             ((stringp (car cns))
;;              (setq old-style t)
;;              (push (cons (listify-key-sequence (kbd (car cns))) (cdr cns)) res))
;;             ((symbolp (car cns))
;;              (let (new-mode-alist)
;;                (dolist (cns2 (cdr cns))
;;                  (cond ((listp (car cns2))
;;                         (push cns2 new-mode-alist))
;;                        ((stringp (car cns2))
;;                         (setq old-style t)
;;                         (push (cons (listify-key-sequence (kbd (car cns2))) (cdr cns2))
;;                               new-mode-alist))))
;;                (push (cons (car cns) new-mode-alist) res)))
;;             (t (message "which-key: there's a problem with the \
;; entry %s in which-key-key-based-replacement-alist" cns))))
;;     (setq which-key-key-based-description-replacement-alist res)
;;     (when old-style
;;       (message "which-key: \
;;  `which-key-key-based-description-replacement-alist' has changed format and you\
;;  seem to be using the old format. Please use the functions \
;; `which-key-add-key-based-replacements' and \
;; `which-key-add-major-mode-key-based-replacements' instead."))))

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
  (let ((keys (key-description (kbd key))))
    (cond ((null alist) (list (cons keys value)))
          ((assoc-string keys alist)
           (when (not (string-equal (cdr (assoc-string keys alist)) value))
             (when which-key-is-verbose
               (message "which-key: changing %s name from %s to %s in the %s alist"
                        key (cdr (assoc-string keys alist)) value alist-name))
             (setcdr (assoc-string keys alist) value))
           alist)
          (t (cons (cons keys value) alist)))))

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
  (let ((keys (key-description (kbd key-seq-str))))
    (if (and (null force)
             (assoc-string keys which-key-prefix-title-alist))
        (when which-key-is-verbose
          (message "which-key: Prefix title not added. A title exists for this prefix."))
      (push (cons keys title) which-key-prefix-title-alist))))

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
    (let ((name (if (consp name) (car name) name))
          (title (if (consp name) (cdr name) name)))
        (setq which-key-prefix-name-alist
              (which-key--add-key-val-to-alist
               which-key-prefix-name-alist key-sequence name "prefix-name")
              which-key-prefix-title-alist
              (which-key--add-key-val-to-alist
               which-key-prefix-title-alist key-sequence title "prefix-title")))
    (setq key-sequence (pop more) name (pop more))))
(put 'which-key-declare-prefixes 'lisp-indent-function 'defun)

;;;###autoload
(defun which-key-declare-prefixes-for-mode (mode key-sequence name &rest more)
  "Functions like `which-key-declare-prefixes'.
The difference is that MODE specifies the `major-mode' that must
be active for KEY-SEQUENCE and NAME (MORE contains
addition KEY-SEQUENCE NAME pairs) to apply."
  (when (not (symbolp mode))
    (error "MODE should be a symbol corresponding to a value of major-mode"))
  (let ((mode-name-alist (cdr (assq mode which-key-prefix-name-alist)))
        (mode-title-alist (cdr (assq mode which-key-prefix-title-alist))))
    (while key-sequence
      (let ((name (if (consp name) (car name) name))
            (title (if (consp name) (cdr name) name)))
        (setq mode-name-alist (which-key--add-key-val-to-alist
                               mode-name-alist key-sequence name
                               (format "prefix-name-%s" mode))
              mode-title-alist (which-key--add-key-val-to-alist
                                mode-title-alist key-sequence title
                                (format "prefix-name-%s" mode))))
      (setq key-sequence (pop more) name (pop more)))
    (if (assq mode which-key-prefix-name-alist)
        (setcdr (assq mode which-key-prefix-name-alist) mode-name-alist)
      (push (cons mode mode-name-alist) which-key-prefix-name-alist))
    (if (assq mode which-key-prefix-title-alist)
        (setcdr (assq mode which-key-prefix-title-alist) mode-title-alist)
      (push (cons mode mode-title-alist) which-key-prefix-title-alist))))
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
  (unless (member real-this-command which-key--paging-functions)
    (setq which-key--current-page-n nil
          which-key--using-top-level nil
          which-key--using-show-keymap nil
          which-key--using-show-operator-keymap nil
          which-key--current-show-keymap-name nil
          which-key--prior-show-keymap-args nil
          which-key--on-last-page nil)
    (when which-key-idle-secondary-delay
      (which-key--start-timer))
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

(defun which-key--popup-showing-p ()
  (window-live-p (get-buffer-window which-key--buffer)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Max dimension of available window functions

(defun which-key--popup-max-dimensions ()
  "Dimesion functions should return the maximum possible (height
. width) of the intended popup. SELECTED-WINDOW-WIDTH is the
width of currently active window, not the which-key buffer
window."
  (cl-case which-key-popup-type
    (minibuffer (which-key--minibuffer-max-dimensions))
    (side-window (which-key--side-window-max-dimensions))
    (frame (which-key--frame-max-dimensions))
    (custom (funcall which-key-custom-popup-max-dimensions-function
                     (window-width)))))

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
   (max 0 (- (frame-text-cols) which-key-unicode-correction))))

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
   (max 0
        (- (if (member which-key-side-window-location '(left right))
               (which-key--total-width-to-text (which-key--width-or-percentage-to-width
                                                which-key-side-window-max-width))
             (which-key--total-width-to-text (which-key--width-or-percentage-to-width
                                              1.0)))
           which-key-unicode-correction))))

(defun which-key--frame-max-dimensions ()
  "Return max-dimensions of the frame popup (height .
width) in lines and characters respectively."
  (cons which-key-frame-max-height which-key-frame-max-width))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sorting functions

(defun which-key--string< (a b &optional alpha)
  (if alpha
      (let ((da (downcase a))
            (db (downcase b)))
        (if (string-equal da db)
            (not (string-lessp a b))
          (string-lessp da db)))
    (string-lessp a b)))

(defun which-key--key-description< (a b &optional alpha)
  "Sorting function used for `which-key-key-order' and
`which-key-key-order-alpha'."
  (save-match-data
    (let* ((rngrgxp "^\\([^ ]+\\) \\.\\. [^ ]+")
           (a (if (string-match rngrgxp a) (match-string 1 a) a))
           (b (if (string-match rngrgxp b) (match-string 1 b) b))
           (aem? (string-equal a ""))
           (bem? (string-equal b ""))
           (a1? (= 1 (length a)))
           (b1? (= 1 (length b)))
           (srgxp "^\\(RET\\|SPC\\|TAB\\|DEL\\|LFD\\|ESC\\|NUL\\)")
           (asp? (string-match-p srgxp a))
           (bsp? (string-match-p srgxp b))
           (prrgxp "^\\(M\\|C\\|S\\|A\\|H\\|s\\)-")
           (apr? (string-match-p prrgxp a))
           (bpr? (string-match-p prrgxp b))
           (afn? (string-match-p "<f[0-9]+>" a))
           (bfn? (string-match-p "<f[0-9]+>" b)))
      (cond ((or aem? bem?) (and aem? (not bem?)))
            ((and asp? bsp?)
             (if (string-equal (substring a 0 3) (substring b 0 3))
                 (which-key--key-description< (substring a 3) (substring b 3) alpha)
               (string-lessp a b)))
            ((or asp? bsp?) asp?)
            ((and a1? b1?) (which-key--string< a b alpha))
            ((or a1? b1?) a1?)
            ((and afn? bfn?)
             (< (string-to-number (replace-regexp-in-string "<f\\([0-9]+\\)>" "\\1" a))
                (string-to-number (replace-regexp-in-string "<f\\([0-9]+\\)>" "\\1" b))))
            ((or afn? bfn?) afn?)
            ((and apr? bpr?)
             (if (string-equal (substring a 0 2) (substring b 0 2))
                 (which-key--key-description< (substring a 2) (substring b 2) alpha)
               (string-lessp a b)))
            ((or apr? bpr?) apr?)
            (t (string-lessp a b))))))

(defsubst which-key-key-order-alpha (acons bcons)
  "Order key descriptions A and B.
Order is lexicographic within a \"class\", where the classes and
the ordering of classes are listed below.

special (SPC,TAB,...) < single char < mod (C-,M-,...) < other.
Sorts single characters alphabetically with lowercase coming
before upper."
  (which-key--key-description< (car acons) (car bcons) t))

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

(defsubst which-key--group-p (description)
  (or (string-match-p "^\\(group:\\|Prefix\\)" description)
      (keymapp (intern description))))

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

(defsubst which-key--string-width (maybe-string)
  "If MAYBE-STRING is a string use `which-key--string-width' o/w return 0."
  (if (stringp maybe-string) (string-width maybe-string) 0))

(defsubst which-key--safe-lookup-key (keymap key)
  "Version of `lookup-key' that allows KEYMAP to be nil. KEY is not checked."
  (when (keymapp keymap) (lookup-key keymap key)))

(defsubst which-key--butlast-string (str)
  (mapconcat #'identity (butlast (split-string str)) " "))

(defun which-key--maybe-replace (string repl-alist &optional literal)
  "Perform replacements on STRING.
REPL-ALIST is an alist where the car of each element is the text
to replace and the cdr is the replacement text.  Unless LITERAL is
non-nil regexp is used in the replacements.  Whether or not a
replacement occurs return the new STRING."
  (save-match-data
    (let ((new-string string)
          case-fold-search)
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
  (key-description (which-key--current-key-list key-str)))

(defun which-key--maybe-replace-prefix-name (keys desc)
  "KEYS is a list of keys produced by `listify-key-sequences' and
`key-description'. DESC is the description that is possibly
replaced using the `which-key-prefix-name-alist'. Whether or not
a replacement occurs return the new STRING."
  (let* ((alist which-key-prefix-name-alist)
         (res (assoc-string keys alist))
         (mode-alist (assq major-mode alist))
         (mode-res (when mode-alist
                     (assoc-string keys mode-alist))))
    (cond (mode-res (cdr mode-res))
          (res (cdr res))
          (t desc))))

(defun which-key--maybe-get-prefix-title (keys)
  "KEYS is a string produced by `key-description'.
A title is possibly returned using `which-key-prefix-title-alist'.
An empty stiring is returned if no title exists."
  (cond
   ((not (string-equal keys ""))
    (let* ((alist which-key-prefix-title-alist)
           (res (assoc-string keys alist))
           (mode-alist (assq major-mode alist))
           (mode-res (when mode-alist
                       (assoc-string keys mode-alist)))
           (binding (key-binding keys))
           (alternate (when (and binding (symbolp binding))
                        (symbol-name binding))))
      (cond (mode-res (cdr mode-res))
            (res (cdr res))
            ((and (eq which-key-show-prefix 'echo) alternate)
             alternate)
            ((and (member which-key-show-prefix '(bottom top))
                  (eq which-key-side-window-location 'bottom)
                  echo-keystrokes)
             (if alternate alternate
               (concat "Following " keys)))
            (t ""))))
    (which-key--using-top-level "Top-level bindings")
    (which-key--current-show-keymap-name
     which-key--current-show-keymap-name)
    (t "")))

(defun which-key--maybe-replace-key-based (string keys)
  "KEYS is a string produced by `key-description'
and STRING is the description that is possibly replaced using the
`which-key-key-based-description-replacement-alist'. Whether or
not a replacement occurs return the new STRING."
  (let* ((alist which-key-key-based-description-replacement-alist)
         (str-res (assoc-string keys alist))
         (mode-alist (assq major-mode alist))
         (mode-res (when mode-alist (assoc-string keys mode-alist))))
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
                                   "\\|") "\\)"))
        case-fold-search)
    (save-match-data
      (if (and which-key-special-keys
               (string-match regexp key))
          (let ((beg (match-beginning 0)) (end (match-end 0)))
            (concat (substring key-w-face 0 beg)
                    (propertize (substring key-w-face beg (1+ beg))
                                'face 'which-key-special-key-face)
                    (substring key-w-face end
                               (which-key--string-width key-w-face))))
        key-w-face))))

(defsubst which-key--truncate-description (desc)
  "Truncate DESC description to `which-key-max-description-length'."
  (if (and which-key-max-description-length
           (> (length desc) which-key-max-description-length))
      (concat (substring desc 0 which-key-max-description-length) "..")
    desc))

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

(defun which-key--propertize-description
    (description group local hl-face &optional original-description)
  "Add face to DESCRIPTION where the face chosen depends on
whether the description represents a group or a command. Also
make some minor adjustments to the description string, like
removing a \"group:\" prefix.

ORIGINAL-DESCRIPTION is the description given by
`describe-buffer-bindings'."
  (let* ((desc description)
         (desc (if (string-match-p "^group:" desc)
                   (substring desc 6) desc))
         (desc (if group (concat which-key-prefix-prefix desc) desc))
         (desc (which-key--truncate-description desc)))
    (make-text-button desc nil
      'face (cond (hl-face hl-face)
                  (group 'which-key-group-description-face)
                  (local 'which-key-local-map-description-face)
                  (t 'which-key-command-description-face))
      'help-echo (cond
                  ((and original-description
                        (fboundp (intern original-description))
                        (documentation (intern original-description))
                        ;; tooltip-mode doesn't exist in emacs-nox
                        (boundp 'tooltip-mode) tooltip-mode)
                   (documentation (intern original-description)))
                  ((and original-description
                        (fboundp (intern original-description))
                        (documentation (intern original-description))
                        (let* ((doc (documentation (intern original-description)))
                               (str (replace-regexp-in-string "\n" " " doc))
                               (max (floor (* (frame-width) 0.8))))
                          (if (> (length str) max)
                              (concat (substring str 0 max) "...")
                            str))))))
    desc))

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
              (orig-desc (cdr key-desc-cons))
              (group (which-key--group-p orig-desc))
              (keys (which-key--current-key-string key))
              (local (eq (which-key--safe-lookup-key local-map (kbd keys))
                         (intern orig-desc)))
              (hl-face (which-key--highlight-face orig-desc))
              (key (which-key--maybe-replace
                    key which-key-key-replacement-alist))
              (desc (which-key--maybe-replace
                     orig-desc which-key-description-replacement-alist))
              (desc (which-key--maybe-replace-key-based desc keys))
              (desc (if group
                        (which-key--maybe-replace-prefix-name keys desc)
                      desc))
              (key-w-face (which-key--propertize-key key))
              (desc-w-face (which-key--propertize-description
                            desc group local hl-face orig-desc)))
         (list key-w-face sep-w-face desc-w-face)))
     unformatted)))

(defun which-key--get-keymap-bindings (keymap &optional filter)
  "Retrieve top-level bindings from KEYMAP."
  (let (bindings)
    (map-keymap
     (lambda (ev def)
       (unless (and (functionp filter) (funcall filter ev def))
         (cl-pushnew
          (cons (key-description (list ev))
                (cond ((keymapp def) "Prefix Command")
                      ((symbolp def) (copy-sequence (symbol-name def)))
                      ((eq 'lambda (car-safe def)) "lambda")
                      (t (format "%s" def))))
          bindings :test (lambda (a b) (string= (car a) (car b))))))
     keymap)
    bindings))

;; adapted from helm-descbinds
(defun which-key--get-current-bindings ()
  (let ((key-str-qt (regexp-quote (key-description which-key--current-prefix)))
        (buffer (current-buffer))
        (ignore-bindings '("self-insert-command" "ignore" "ignore-event" "company-ignore"))
        (ignore-keys-regexp "mouse-\\|wheel-\\|remap\\|drag-\\|scroll-bar\\|select-window\\|switch-frame\\|-state")
        (ignore-sections-regexp "\\(Key translations\\|Function key map translations\\|Input decoding map translations\\)"))
    (with-temp-buffer
      (let ((indent-tabs-mode t))
        (describe-buffer-bindings buffer which-key--current-prefix))
      (goto-char (point-min))
      (let ((header-p (not (= (char-after) ?\f)))
            bindings header)
        (while (not (eobp))
          (cond
           (header-p
            (setq header (buffer-substring-no-properties
                          (point)
                          (line-end-position)))
            (setq header-p nil)
            (forward-line 3))
           ((= (char-after) ?\f)
            ;; (push (cons header (nreverse section)) bindings)
            ;; (setq section nil)
            (setq header-p t))
           ((looking-at "^[ \t]*$")
            ;; ignore
            )
           ((or (not (string-match-p ignore-sections-regexp header))
                which-key--current-prefix)
            (let ((binding-start (save-excursion
                                   (and (re-search-forward "\t+" nil t)
                                        (match-end 0))))
                  key binding)
              (when binding-start
                (setq key (buffer-substring-no-properties (point) binding-start)
                      ;; key (replace-regexp-in-string"^[ \t\n]+" "" key)
                      ;; key (replace-regexp-in-string"[ \t\n]+$" "" key)
                      )
                (setq binding (buffer-substring-no-properties
                               binding-start
                               (line-end-position)))
                (save-match-data
                  (cond
                   ((member binding ignore-bindings))
                   ((string-match-p ignore-keys-regexp key))
                   ((and which-key--current-prefix
                         (string-match (format "^%s[ \t]\\([^ \t]+\\)[ \t]+$" key-str-qt) key))
                    (unless (assoc-string (match-string 1 key) bindings)
                      (push (cons (match-string 1 key) binding) bindings)))
                   ((and which-key--current-prefix
                         (string-match
                          (format
                           "^%s[ \t]\\([^ \t]+\\) \\.\\. %s[ \t]\\([^ \t]+\\)[ \t]+$"
                           key-str-qt key-str-qt) key))
                    (let ((stripped-key
                           (concat (match-string 1 key) " \.\. " (match-string 2 key))))
                      (unless (assoc-string stripped-key bindings)
                        (push (cons stripped-key binding) bindings))))
                   ((string-match "^\\([^ \t]+\\|[^ \t]+ \\.\\. [^ \t]+\\)[ \t]+$" key)
                    (unless (assoc-string (match-string 1 key) bindings)
                      (push (cons (match-string 1 key) binding) bindings)))))))))
          (forward-line))
        (nreverse bindings)))))

(defun which-key--get-formatted-key-bindings (&optional bindings)
  "Uses `describe-buffer-bindings' to collect the key bindings in
BUFFER that follow the key sequence KEY-SEQ."
  (let* ((unformatted (if bindings bindings (which-key--get-current-bindings))))
    (when which-key-sort-order
      (setq unformatted
            (sort unformatted (lambda (a b) (funcall which-key-sort-order a b)))))
    (which-key--format-and-replace unformatted)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions for laying out which-key buffer pages

(defun which-key--normalize-columns (columns)
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
  (let* ((padded (which-key--normalize-columns (nreverse columns)))
         (rows (apply #'cl-mapcar #'list padded)))
    (mapconcat (lambda (row) (mapconcat #'identity row " ")) rows "\n")))

(defsubst which-key--max-len (keys index)
  "Internal function for finding the max length of the INDEX
element in each list element of KEYS."
  (cl-reduce
   (lambda (x y) (max x (which-key--string-width (nth index y))))
   keys :initial-value 0))

(defun which-key--pad-column (col-keys)
  "Take a column of (key separator description) COL-KEYS,
calculate the max width in the column and pad all cells out to
that width."
  (let* ((col-key-width  (+ which-key-add-column-padding
                            (which-key--max-len col-keys 0)))
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

(defun which-key--list-to-pages (keys avl-lines avl-width)
  "Convert list of KEYS to columns based on dimensions AVL-LINES and AVL-WIDTH.
Returns a plist that holds the page strings, as well as
metadata."
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
            :keys/page (reverse keys/page) :n-pages n-pages
            :tot-keys (apply #'+ keys/page)))))

(defun which-key--create-pages-1
    (keys available-lines available-width &optional min-lines vertical)
  "Create page strings using `which-key--list-to-pages'.
Will try to find the best number of rows and columns using the
given dimensions and the length and widths of ITEMS. Use VERTICAL
if the ITEMS are laid out vertically and the number of columns
should be minimized."
  (let ((result (which-key--list-to-pages
                 keys available-lines available-width))
        (min-lines (or min-lines 0))
        found prev-result)
    (if (or vertical
            (> (plist-get result :n-pages) 1)
            (= 1 available-lines))
        result
      ;; simple search for a fitting page
      (while (and (> available-lines min-lines)
                  (not found))
        (setq available-lines (- available-lines 1)
              prev-result result
              result (which-key--list-to-pages
                      keys available-lines available-width)
              found (> (plist-get result :n-pages) 1)))
      (if found prev-result result))))

(defun which-key--create-pages (keys)
  "Create page strings using `which-key--list-to-pages'.
Will try to find the best number of rows and columns using the
given dimensions and the length and wdiths of KEYS. SEL-WIN-WIDTH
is the width of the live window."
  (let* ((max-dims (which-key--popup-max-dimensions))
         (max-lines (car max-dims))
         (max-width (cdr max-dims))
         (prefix-keys-desc (key-description which-key--current-prefix))
         (full-prefix (which-key--full-prefix prefix-keys-desc))
         (prefix (when (eq which-key-show-prefix 'left)
                   (+ 2 (which-key--string-width full-prefix))))
         (prefix-top-bottom (member which-key-show-prefix '(bottom top)))
         (avl-lines (if prefix-top-bottom (- max-lines 1) max-lines))
         (min-lines (min avl-lines which-key-min-display-lines))
         (avl-width (if prefix (- max-width prefix) max-width))
         (vertical (and (eq which-key-popup-type 'side-window)
                        (member which-key-side-window-location '(left right)))))
    (which-key--create-pages-1 keys avl-lines avl-width min-lines vertical)))

(defun which-key--lighter-status (page-n)
  "Possibly show number of keys and total in the mode line."
  (when which-key-show-remaining-keys
    (let ((n-shown (nth page-n (plist-get which-key--pages-plist :keys/page)))
          (n-tot (plist-get which-key--pages-plist :tot-keys)))
      (setq which-key--lighter-backup (cadr (assq 'which-key-mode minor-mode-alist)))
      (setcar (cdr (assq 'which-key-mode minor-mode-alist))
              (format " WK: %s/%s keys" n-shown n-tot)))))

(defun which-key--lighter-restore ()
  "Restore the lighter for which-key."
  (when which-key-show-remaining-keys
    (setcar (cdr (assq 'which-key-mode minor-mode-alist)) which-key--lighter-backup)))

(defun which-key--echo (text)
  "Echo TEXT to minibuffer without logging.
Slight delay gets around evil functions that clear the echo
area."
  (let* ((minibuffer (eq which-key-popup-type 'minibuffer))
         (delay (if minibuffer 0.2 (+ echo-keystrokes 0.001)))
         message-log-max)
    (unless minibuffer (message "%s" text))
    (run-with-idle-timer
     delay nil (lambda () (let (message-log-max)
                            (message "%s" text))))))

(defun which-key--next-page-hint (prefix-keys)
  "Return string for next page hint."
  (let* ((paging-key (concat prefix-keys " " which-key-paging-key))
         (paging-key-bound (eq 'which-key-C-h-dispatch
                               (key-binding (kbd paging-key))))
         (key (if paging-key-bound which-key-paging-key "C-h")))
    (when (and which-key-use-C-h-commands
               (or which-key--using-show-operator-keymap
                   (not (and which-key-allow-evil-operators
                             (bound-and-true-p evil-this-operator)))))
      (propertize (format "[%s paging/help]" key)
                  'face 'which-key-note-face))))

(eval-and-compile
  (if (fboundp 'universal-argument--description)
      (defalias 'which-key--universal-argument--description
        'universal-argument--description)
    (defun which-key--universal-argument--description ()
      ;; Backport of the definition of universal-argument--description in emacs25
      ;; on 2015-12-04
      (when prefix-arg
        (concat "C-u"
                (pcase prefix-arg
                  (`(-) " -")
                  (`(,(and (pred integerp) n))
                   (let ((str ""))
                     (while (and (> n 4) (= (mod n 4) 0))
                       (setq str (concat str " C-u"))
                       (setq n (/ n 4)))
                     (if (= n 4) str (format " %s" prefix-arg))))
                  (_ (format " %s" prefix-arg))))))))

(defun which-key--full-prefix (prefix-keys &optional -prefix-arg dont-prop-keys)
  "Return a description of the full key sequence up to now,
including prefix arguments."
  (let* ((left (eq which-key-show-prefix 'left))
         (prefix-arg (if -prefix-arg -prefix-arg prefix-arg))
         (str (concat
               (which-key--universal-argument--description)
               (when prefix-arg " ")
               prefix-keys))
         (dash (if (and which-key--current-prefix
                        (null left)) "-" "")))
    (if (or (eq which-key-show-prefix 'echo) dont-prop-keys)
        (concat str dash)
      (concat (which-key--propertize-key str)
              (propertize dash 'face 'which-key-key-face)))))

(defun which-key--get-popup-map ()
  (unless which-key--current-prefix
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd which-key-paging-key) #'which-key-C-h-dispatch)
      (when which-key-use-C-h-commands
        ;; Show next page even when C-h is pressed
        (define-key map (kbd "C-h") #'which-key-C-h-dispatch))
      map)))

(defun which-key--process-page (page-n pages-plist)
  (let* ((page (nth page-n (plist-get pages-plist :pages)))
         (height (plist-get pages-plist :page-height))
         (n-pages (plist-get pages-plist :n-pages))
         (prefix-keys (key-description which-key--current-prefix))
         (full-prefix (which-key--full-prefix prefix-keys))
         (nxt-pg-hint (which-key--next-page-hint prefix-keys))
         ;; not used in left case
         (status-line
          (concat (propertize (which-key--maybe-get-prefix-title
                               (which-key--current-key-string))
                              'face 'which-key-note-face)
                  (when (< 1 n-pages)
                    (propertize (format " (%s of %s)"
                                        (1+ page-n) n-pages)
                                'face 'which-key-note-face)))))
    (pcase which-key-show-prefix
      (`left
       (let* ((page-cnt (propertize (format "%s/%s" (1+ page-n) n-pages)
                                    'face 'which-key-separator-face))
              (first-col-width (+ 2 (max (which-key--string-width full-prefix)
                                         (which-key--string-width page-cnt))))
              (prefix (format (concat "%-" (int-to-string first-col-width) "s")
                              full-prefix))
              (page-cnt (if (> n-pages 1)
                            (format (concat "%-" (int-to-string first-col-width) "s")
                                    page-cnt)
                          (make-string first-col-width 32)))
              lines first-line new-end)
         (if (= 1 height)
             (cons (concat prefix page) nil)
           (setq lines (split-string page "\n")
                 first-line (concat prefix (car lines) "\n" page-cnt)
                 new-end (concat "\n" (make-string first-col-width 32)))
           (cons
            (concat first-line (mapconcat #'identity (cdr lines) new-end))
            nil))))
      (`top
       (cons
        (concat (when (or (= 0 echo-keystrokes)
                          (not (eq which-key-side-window-location 'bottom)))
                  (concat full-prefix " "))
                status-line " " nxt-pg-hint "\n" page)
        nil))
      (`bottom
       (cons
        (concat page "\n"
                (when (or (= 0 echo-keystrokes)
                          (not (eq which-key-side-window-location 'bottom)))
                  (concat full-prefix " "))
                status-line " " nxt-pg-hint)
        nil))
      (`echo
       (cons page
             (concat full-prefix (when prefix-keys " ")
                     status-line (when status-line " ")
                     nxt-pg-hint)))
      (_ (cons page nil)))))

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
      (let ((page-echo (which-key--process-page page-n which-key--pages-plist))
            (height (plist-get which-key--pages-plist :page-height))
            (width (nth page-n (plist-get which-key--pages-plist :page-widths))))
        (which-key--lighter-status page-n)
        (if (eq which-key-popup-type 'minibuffer)
            (which-key--echo (car page-echo))
          (with-current-buffer which-key--buffer
            (erase-buffer)
            (insert (car page-echo))
            (goto-char (point-min)))
          (when (cdr page-echo) (which-key--echo (cdr page-echo)))
          (which-key--show-popup (cons height width)))))
    ;; used for paging at top-level
    (if (fboundp 'set-transient-map)
        (set-transient-map (which-key--get-popup-map))
      (with-no-warnings
        (set-temporary-overlay-map (which-key--get-popup-map))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; paging functions

;;;###autoload
(defun which-key-reload-key-sequence (key-seq)
  "Simulate entering the key sequence KEY-SEQ.
KEY-SEQ should be a list of events as produced by
`listify-key-sequence'. Any prefix arguments that were used are
reapplied to the new key sequence."
  (let ((next-event (mapcar (lambda (ev) (cons t ev)) key-seq)))
    (setq prefix-arg current-prefix-arg
          unread-command-events next-event)))

(defun which-key-turn-page (delta)
  "Show the next page of keys."
  (let ((next-page (if which-key--current-page-n
                       (+ which-key--current-page-n delta) 0)))
    (which-key-reload-key-sequence (which-key--current-key-list))
    (if which-key--last-try-2-loc
        (let ((which-key-side-window-location which-key--last-try-2-loc)
              (which-key--multiple-locations t))
          (which-key--show-page next-page))
      (which-key--show-page next-page))
    (which-key--start-paging-timer)))

;;;###autoload
(defun which-key-show-standard-help ()
  "Call the command in `which-key--prefix-help-cmd-backup'.
Usually this is `describe-prefix-bindings'."
  (interactive)
  (let ((which-key-inhibit t))
    (which-key--hide-popup-ignore-command)
    (cond ((eq which-key--prefix-help-cmd-backup
               'describe-prefix-bindings)
           ;; This is essentially what `describe-prefix-bindings' does
           (describe-bindings
            (kbd (which-key--current-key-string))))
          ((functionp which-key--prefix-help-cmd-backup)
           (funcall which-key--prefix-help-cmd-backup)))))

;;;###autoload
(defun which-key-show-next-page-no-cycle ()
  "Show next page of keys unless on the last page, in which case
call `which-key-show-standard-help'."
  (interactive)
  (let ((which-key-inhibit t))
    (if (and which-key--current-page-n
             which-key--on-last-page)
        (which-key-show-standard-help)
      (which-key-turn-page 1))))
(defalias 'which-key-show-next-page 'which-key-show-next-page-no-cycle)
(make-obsolete 'which-key-show-next-page 'which-key-show-next-page-no-cycle
               "2015-12-2")

;;;###autoload
(defun which-key-show-previous-page-no-cycle ()
  "Show previous page of keys unless on the first page, in which
case do nothing."
  (interactive)
  (let ((which-key-inhibit t))
    (if (and which-key--current-page-n
             (eq which-key--current-page-n 0))
        (which-key-turn-page 0)
      (which-key-turn-page -1))))

;;;###autoload
(defun which-key-show-next-page-cycle ()
  "Show the next page of keys, cycling from end to beginning
after last page."
  (interactive)
  (let ((which-key-inhibit t))
    (which-key-turn-page 1)))

;;;###autoload
(defun which-key-show-previous-page-cycle ()
  "Show the previous page of keys, cycling from beginning to end
after first page."
  (interactive)
  (let ((which-key-inhibit t))
    (which-key-turn-page -1)))

;;;###autoload
(defun which-key-show-top-level ()
  "Show top-level bindings."
  (interactive)
  (setq which-key--using-top-level t)
  (which-key--create-buffer-and-show nil))

;;;###autoload
(defun which-key-undo-key ()
  "Undo last keypress and force which-key update."
  (interactive)
  (let* ((key-lst (butlast (which-key--current-key-list)))
         (which-key-inhibit t))
    (cond ((stringp which-key--current-show-keymap-name)
           (if (keymapp (cdr (car-safe which-key--prior-show-keymap-args)))
               (let ((args (pop which-key--prior-show-keymap-args)))
                 (which-key--show-keymap (car args) (cdr args)))
             (which-key--hide-popup)))
          (key-lst
           (which-key-reload-key-sequence key-lst)
           (which-key--create-buffer-and-show (apply #'vector key-lst)))
          (t (which-key-show-top-level)))))
(defalias 'which-key-undo 'which-key-undo-key)

(defun which-key-abort ()
  "Abort key sequence."
  (interactive)
  (let ((which-key-inhibit t))
    (which-key--hide-popup-ignore-command)
    (keyboard-quit)))

;;;###autoload
(defun which-key-C-h-dispatch ()
  "Dispatch C-h commands by looking up key in
`which-key-C-h-map'. This command is always accessible (from any
prefix) if `which-key-use-C-h-commands' is non nil."
  (interactive)
  (let* ((prefix-keys (key-description which-key--current-prefix))
         (full-prefix (which-key--full-prefix prefix-keys current-prefix-arg t))
         (prompt (concat (when (string-equal prefix-keys "")
                           (propertize (concat " "
                                               (or which-key--current-show-keymap-name
                                                   "Top-level bindings"))
                                       'face 'which-key-note-face))
                         full-prefix
                         (propertize
                          (substitute-command-keys
                           (concat
                            " \\<which-key-C-h-map>"
                            " \\[which-key-show-next-page-cycle]" which-key-separator "next-page,"
                            " \\[which-key-show-previous-page-cycle]" which-key-separator "previous-page,"
                            " \\[which-key-undo-key]" which-key-separator "undo-key,"
                            " \\[which-key-show-standard-help]" which-key-separator "help,"
                            " \\[which-key-abort]" which-key-separator "abort"))
                          'face 'which-key-note-face)))
         (key (string (read-key prompt)))
         (cmd (lookup-key which-key-C-h-map key))
         (which-key-inhibit t))
    (if cmd (funcall cmd) (which-key-turn-page 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Update

(defun which-key--try-2-side-windows (keys page-n loc1 loc2 &rest _ignore)
  "Try to show KEYS (PAGE-N) in LOC1 first. Only if no keys fit fallback to LOC2."
  (let (pages1)
    (let ((which-key-side-window-location loc1)
          (which-key--multiple-locations t))
      (setq pages1 (which-key--create-pages keys)))
    (if (< 0 (plist-get pages1 :n-pages))
        (progn
          (setq which-key--pages-plist pages1)
          (let ((which-key-side-window-location loc1)
                (which-key--multiple-locations t))
            (which-key--show-page page-n))
          loc1)
      (let ((which-key-side-window-location loc2)
            (which-key--multiple-locations t))
        (setq which-key--pages-plist
              (which-key--create-pages keys))
        (which-key--show-page page-n)
        loc2))))

(defun which-key-show-keymap ()
  "Show the top-level bindings in KEYMAP using which-key. KEYMAP
is selected interactively from all available keymaps."
  (interactive)
  (let ((keymap-sym (intern
                     (completing-read
                      "Keymap: " obarray
                      (lambda (m)
                        (and (boundp m)
                             (keymapp (symbol-value m))
                             (not (equal (symbol-value m) (make-sparse-keymap)))))
                      t nil 'which-key-keymap-history))))
    (which-key--show-keymap (symbol-name keymap-sym) (symbol-value keymap-sym))))

(defun which-key-show-minor-mode-keymap ()
  "Show the top-level bindings in KEYMAP using which-key. KEYMAP
is selected interactively by mode in `minor-mode-map-alist'."
  (interactive)
  (let ((mode-sym
         (intern
          (completing-read
           "Minor Mode: "
           (mapcar 'car
                   (cl-remove-if-not
                    (lambda (entry)
                      (and (symbol-value (car entry))
                           (not (equal (cdr entry) (make-sparse-keymap)))))
                    minor-mode-map-alist))
           nil t nil 'which-key-keymap-history))))
    (which-key--show-keymap (symbol-name mode-sym)
                            (cdr (assq mode-sym minor-mode-map-alist)))))

(defun which-key--show-keymap (keymap-name keymap &optional prior-args)
  (setq which-key--current-prefix nil
        which-key--current-show-keymap-name keymap-name
        which-key--using-show-keymap t)
  (when prior-args (push prior-args which-key--prior-show-keymap-args))
  (when (keymapp keymap)
    (let ((formatted-keys (which-key--get-formatted-key-bindings
                           (which-key--get-keymap-bindings keymap))))
      (cond ((= (length formatted-keys) 0)
             (message "which-key: Keymap empty"))
            ((listp which-key-side-window-location)
             (setq which-key--last-try-2-loc
                   (apply #'which-key--try-2-side-windows
                          formatted-keys 0 which-key-side-window-location)))
            (t (setq which-key--pages-plist
                     (which-key--create-pages formatted-keys))
               (which-key--show-page 0)))))
  (let* ((key (key-description (list (read-key))))
         (next-def (lookup-key keymap (kbd key))))
    (cond ((and which-key-use-C-h-commands (string= "C-h" key))
           (which-key-C-h-dispatch))
          ((keymapp next-def)
           (which-key--hide-popup-ignore-command)
           (which-key--show-keymap (concat keymap-name " " key) next-def
                                   (cons keymap-name keymap)))
          (t (which-key--hide-popup)))))

(defun which-key--evil-operator-filter (_ev def)
  (and (functionp def)
       (evil-get-command-property def :suppress-operator)))

(defun which-key--show-evil-operator-keymap ()
  (if which-key--inhibit-next-operator-popup
      (setq which-key--inhibit-next-operator-popup nil)
    (let ((keymap
           (make-composed-keymap (list evil-operator-shortcut-map
                                       evil-operator-state-map
                                       evil-motion-state-map))))
      (setq which-key--current-prefix nil
            which-key--current-show-keymap-name "evil operator/motion keys"
            which-key--using-show-operator-keymap t)
      (when (keymapp keymap)
        (let ((formatted-keys (which-key--get-formatted-key-bindings
                               (which-key--get-keymap-bindings
                                keymap 'which-key--evil-operator-filter))))
          (cond ((= (length formatted-keys) 0)
                 (message "which-key: Keymap empty"))
                ((listp which-key-side-window-location)
                 (setq which-key--last-try-2-loc
                       (apply #'which-key--try-2-side-windows
                              formatted-keys 0 which-key-side-window-location)))
                (t (setq which-key--pages-plist
                         (which-key--create-pages formatted-keys))
                   (which-key--show-page 0)))))
      (let* ((key (key-description (list (read-key)))))
        (when (string= key "`")
          ;; evil-goto-mark reads the next char manually
          (setq which-key--inhibit-next-operator-popup t))
        (cond ((and which-key-use-C-h-commands (string= "C-h" key))
               (which-key-C-h-dispatch))
              ((string= key "ESC")
               (which-key--hide-popup)
               (keyboard-quit))
              (t
               (which-key--hide-popup)
               (setq unread-command-events (listify-key-sequence key))))))))

(defun which-key--create-buffer-and-show (&optional prefix-keys)
  "Fill `which-key--buffer' with key descriptions and reformat.
Finally, show the buffer."
  (setq which-key--current-prefix prefix-keys
        which-key--last-try-2-loc nil)
  (let ((formatted-keys (which-key--get-formatted-key-bindings))
        (prefix-keys (key-description which-key--current-prefix)))
    (cond ((= (length formatted-keys) 0)
           (message "%s-  which-key: There are no keys to show" prefix-keys))
          ((listp which-key-side-window-location)
           (setq which-key--last-try-2-loc
                 (apply #'which-key--try-2-side-windows
                        formatted-keys 0 which-key-side-window-location)))
          (t (setq which-key--pages-plist
                   (which-key--create-pages formatted-keys))
             (which-key--show-page 0)))))

(defun which-key--update ()
  "Function run by timer to possibly trigger `which-key--create-buffer-and-show'."
  (let ((prefix-keys (this-single-command-keys)))
    ;; (when (> (length prefix-keys) 0)
    ;;  (message "key: %s" (key-description prefix-keys)))
    ;; (when (> (length prefix-keys) 0)
    ;;  (message "key binding: %s" (key-binding prefix-keys)))
    ;; Taken from guide-key
    (when (and (equal prefix-keys [key-chord])
               (bound-and-true-p key-chord-mode))
      (setq prefix-keys
            (condition-case nil
                (let ((rkeys (recent-keys)))
                  (vector 'key-chord
                          ;; Take the two preceding the last one, because the
                          ;; read-event call in key-chord seems to add a
                          ;; spurious key press to this list. Note this is
                          ;; different from guide-key's method which didn't work
                          ;; for me.
                          (aref rkeys (- (length rkeys) 3))
                          (aref rkeys (- (length rkeys) 2))))
              (error (progn
                       (message "which-key error in key-chord handling")
                       [key-chord])))))
    (when (and which-key--god-mode-support-enabled
               (bound-and-true-p god-local-mode)
               (eq this-command 'god-mode-self-insert))
      (setq prefix-keys (when which-key--god-mode-key-string
                          (kbd which-key--god-mode-key-string))))
    (cond ((and (> (length prefix-keys) 0)
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
                (or (and which-key-allow-evil-operators
                         (bound-and-true-p evil-this-operator))
                    (and which-key--god-mode-support-enabled
                         (bound-and-true-p god-local-mode)
                         (eq this-command 'god-mode-self-insert))
                    (null this-command)))
           (which-key--create-buffer-and-show prefix-keys)
           (when which-key-idle-secondary-delay
             (which-key--start-timer which-key-idle-secondary-delay)))
          ((and which-key-show-operator-state-maps
                (bound-and-true-p evil-state)
                (eq evil-state 'operator)
                (not which-key--using-show-operator-keymap))
           (which-key--show-evil-operator-keymap))
          ((and which-key--current-page-n
                (not which-key--using-top-level)
                (not which-key--using-show-operator-keymap)
                (not which-key--using-show-keymap))
           (which-key--hide-popup)))))

;; Timers

(defun which-key--start-timer (&optional delay)
  "Activate idle timer to trigger `which-key--update'."
  (which-key--stop-timer)
  (setq which-key--timer
        (run-with-idle-timer
         (if delay
             delay
           which-key-idle-delay) t #'which-key--update)))

(defun which-key--stop-timer ()
  "Deactivate idle timer for `which-key--update'."
  (when which-key--timer (cancel-timer which-key--timer)))

(defun which-key--start-paging-timer ()
  "Activate timer to restart which-key after paging."
  (when which-key--paging-timer (cancel-timer which-key--paging-timer))
  (which-key--stop-timer)
  (setq which-key--paging-timer
        (run-with-idle-timer
         0.2 t (lambda ()
                 (when (or (not (member real-last-command which-key--paging-functions))
                           (and (< 0 (length (this-single-command-keys)))
                                (not (equal which-key--current-prefix
                                            (this-single-command-keys)))))
                   (setq which-key--current-page-n nil
                         which-key--on-last-page nil)
                   (cancel-timer which-key--paging-timer)
                   (which-key--start-timer))))))

;; backport some functions for 24.3

;; found at https://github.com/Lindydancer/andersl-old-emacs-support/blob/master/andersl-old-emacs-support.el
(unless (fboundp 'frame-fringe-width)
  (defun frame-fringe-width (&optional frame)
    "Return fringe width of FRAME in pixels."
    (let ((left-pair (assq 'left-fringe (frame-parameters frame)))
          (right-pair (assq 'right-fringe (frame-parameters frame))))
      (+ (if left-pair (cdr left-pair) 0)
         (if right-pair (cdr right-pair) 0)))))

(unless (fboundp 'frame-scroll-bar-width)
  (defun frame-scroll-bar-width (&optional frame)
    "Return scroll bar width of FRAME in pixels."
    (let ((pair (assq 'scroll-bar-width (frame-parameters frame))))
      (if pair
          (cdr pair)
        0))))

(provide 'which-key)
;;; which-key.el ends here
