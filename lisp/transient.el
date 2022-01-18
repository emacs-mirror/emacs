;;; transient.el --- Transient commands          -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2022 Free Software Foundation, Inc.

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Homepage: https://github.com/magit/transient
;; Keywords: bindings

;; Package-Requires: ((emacs "25.1"))
;; Package-Version: 0.3.7

;; SPDX-License-Identifier: GPL-3.0-or-later

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; This file is part of GNU Emacs.

;;; Commentary:

;; Taking inspiration from prefix keys and prefix arguments, Transient
;; implements a similar abstraction involving a prefix command, infix
;; arguments and suffix commands.  We could call this abstraction a
;; "transient command", but because it always involves at least two
;; commands (a prefix and a suffix) we prefer to call it just a
;; "transient".

;; When the user calls a transient prefix command, then a transient
;; (temporary) keymap is activated, which binds the transient's infix
;; and suffix commands, and functions that control the transient state
;; are added to `pre-command-hook' and `post-command-hook'.  The
;; available suffix and infix commands and their state are shown in
;; the echo area until the transient is exited by invoking a suffix
;; command.

;; Calling an infix command causes its value to be changed, possibly
;; by reading a new value in the minibuffer.

;; Calling a suffix command usually causes the transient to be exited
;; but suffix commands can also be configured to not exit the
;; transient state.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'edmacro)
(require 'format-spec)
(require 'seq)

(eval-when-compile
  (require 'subr-x))

(declare-function info 'info)
(declare-function Man-find-section 'man)
(declare-function Man-next-section 'man)
(declare-function Man-getpage-in-background 'man)

(defvar Man-notify-method)

(define-obsolete-function-alias 'define-transient-command
  'transient-define-prefix "Transient 0.3.0")
(define-obsolete-function-alias 'define-suffix-command
  'transient-define-suffix "Transient 0.3.0")
(define-obsolete-function-alias 'define-infix-command
  'transient-define-infix "Transient 0.3.0")
(define-obsolete-function-alias 'define-infix-argument
  #'transient-define-argument "Transient 0.3.0")

(define-obsolete-variable-alias 'current-transient-prefix
  'transient-current-prefix "Transient 0.3.0")
(define-obsolete-variable-alias 'current-transient-command
  'transient-current-command "Transient 0.3.0")
(define-obsolete-variable-alias 'current-transient-suffixes
  'transient-current-suffixes "Transient 0.3.0")
(define-obsolete-variable-alias 'post-transient-hook
  'transient-exit-hook "Transient 0.3.0")

(defmacro transient--with-emergency-exit (&rest body)
  (declare (indent defun))
  `(condition-case err
       (let ((debugger #'transient--exit-and-debug))
         ,(macroexp-progn body))
     ((debug error)
      (transient--emergency-exit)
      (signal (car err) (cdr err)))))

(defun transient--exit-and-debug (&rest args)
  (transient--emergency-exit)
  (apply #'debug args))

;;; Options

(defgroup transient nil
  "Transient commands."
  :group 'extensions)

(defcustom transient-show-popup t
  "Whether to show the current transient in a popup buffer.

- If t, then show the popup as soon as a transient prefix command
  is invoked.

- If nil, then do not show the popup unless the user explicitly
  requests it, by pressing an incomplete prefix key sequence.

- If a number, then delay displaying the popup and instead show
  a brief one-line summary.  If zero or negative, then suppress
  even showing that summary and display the pressed key only.

  Show the popup when the user explicitly requests it by pressing
  an incomplete prefix key sequence.  Unless zero, then also show
  the popup after that many seconds of inactivity (using the
  absolute value)."
  :package-version '(transient . "0.1.0")
  :group 'transient
  :type '(choice (const  :tag "instantly" t)
                 (const  :tag "on demand" nil)
                 (const  :tag "on demand (no summary)" 0)
                 (number :tag "after delay" 1)))

(defcustom transient-enable-popup-navigation nil
  "Whether navigation commands are enabled in the transient popup.

While a transient is active the transient popup buffer is not the
current buffer, making it necessary to use dedicated commands to
act on that buffer itself.  If this non-nil, then the following
features are available:

- \"<up>\" moves the cursor to the previous suffix.
  \"<down>\" moves the cursor to the next suffix.
  \"RET\" invokes the suffix the cursor is on.
- \"<mouse-1>\" invokes the clicked on suffix.
- \"C-s\" and \"C-r\" start isearch in the popup buffer."
  :package-version '(transient . "0.2.0")
  :group 'transient
  :type 'boolean)

(defcustom transient-display-buffer-action
  '(display-buffer-in-side-window
    (side . bottom)
    (dedicated . t)
    (inhibit-same-window . t)
    (window-parameters (no-other-window . t)))
  "The action used to display the transient popup buffer.

The transient popup buffer is displayed in a window using

  (display-buffer BUFFER transient-display-buffer-action)

The value of this option has the form (FUNCTION . ALIST),
where FUNCTION is a function or a list of functions.  Each such
function should accept two arguments: a buffer to display and an
alist of the same form as ALIST.  See info node `(elisp)Choosing
Window' for details.

The default is:

  (display-buffer-in-side-window
    (side . bottom)
    (dedicated . t)
    (inhibit-same-window . t)
    (window-parameters (no-other-window . t)))

This displays the window at the bottom of the selected frame.
Another useful FUNCTION is `display-buffer-below-selected', which
is what `magit-popup' used by default.  For more alternatives see
info node `(elisp)Display Action Functions' and info node
`(elisp)Buffer Display Action Alists'.

Note that the buffer that was current before the transient buffer
is shown should remain the current buffer.  Many suffix commands
act on the thing at point, if appropriate, and if the transient
buffer became the current buffer, then that would change what is
at point.  To that effect `inhibit-same-window' ensures that the
selected window is not used to show the transient buffer.

It may be possible to display the window in another frame, but
whether that works in practice depends on the window-manager.
If the window manager selects the new window (Emacs frame),
then that unfortunately changes which buffer is current.

If you change the value of this option, then you might also
want to change the value of `transient-mode-line-format'."
  :package-version '(transient . "0.3.0")
  :group 'transient
  :type '(cons (choice function (repeat :tag "Functions" function))
               alist))

(defcustom transient-mode-line-format 'line
  "The mode-line format for the transient popup buffer.

If nil, then the buffer has no mode-line.  If the buffer is not
displayed right above the echo area, then this probably is not
a good value.

If `line' (the default), then the buffer also has no mode-line,
but a thin line is drawn instead, using the background color of
the face `transient-separator'.  Termcap frames cannot display
thin lines and therefore fallback to treating `line' like nil.

Otherwise this can be any mode-line format.
See `mode-line-format' for details."
  :package-version '(transient . "0.2.0")
  :group 'transient
  :type '(choice (const :tag "hide mode-line" nil)
                 (const :tag "substitute thin line" line)
                 (const :tag "name of prefix command"
                        ("%e" mode-line-front-space
                         mode-line-buffer-identification))
                 (sexp  :tag "custom mode-line format")))

(defcustom transient-show-common-commands nil
  "Whether to show common transient suffixes in the popup buffer.

These commands are always shown after typing the prefix key
\"C-x\" when a transient command is active.  To toggle the value
of this variable use \"C-x t\" when a transient is active."
  :package-version '(transient . "0.1.0")
  :group 'transient
  :type 'boolean)

(defcustom transient-read-with-initial-input nil
  "Whether to use the last history element as initial minibuffer input."
  :package-version '(transient . "0.2.0")
  :group 'transient
  :type 'boolean)

(defcustom transient-highlight-mismatched-keys nil
  "Whether to highlight keys that do not match their argument.

This only affects infix arguments that represent command-line
arguments.  When this option is non-nil, then the key binding
for infix argument are highlighted when only a long argument
\(e.g. \"--verbose\") is specified but no shor-thand (e.g \"-v\").
In the rare case that a short-hand is specified but does not
match the key binding, then it is highlighed differently.

The highlighting is done using using `transient-mismatched-key'
and `transient-nonstandard-key'."
  :package-version '(transient . "0.1.0")
  :group 'transient
  :type 'boolean)

(defcustom transient-highlight-higher-levels nil
  "Whether to highlight suffixes on higher levels.

This is primarily intended for package authors.

When non-nil then highlight the description of suffixes whose
level is above 4, the default of `transient-default-level'.
Assuming you have set that variable to 7, this highlights all
suffixes that won't be available to users without them making
the same customization."
  :package-version '(transient . "0.3.6")
  :group 'transient
  :type 'boolean)

(defcustom transient-substitute-key-function nil
  "Function used to modify key bindings.

This function is called with one argument, the prefix object,
and must return a key binding description, either the existing
key description it finds in the `key' slot, or a substitution.

This is intended to let users replace certain prefix keys.  It
could also be used to make other substitutions, but that is
discouraged.

For example, \"=\" is hard to reach using my custom keyboard
layout, so I substitute \"(\" for that, which is easy to reach
using a layout optimized for Lisp.

  (setq transient-substitute-key-function
        (lambda (obj)
          (let ((key (oref obj key)))
            (if (string-match \"\\\\`\\\\(=\\\\)[a-zA-Z]\" key)
                (replace-match \"(\" t t key 1)
              key)))))"
  :package-version '(transient . "0.1.0")
  :group 'transient
  :type '(choice (const :tag "Transform no keys (nil)" nil) function))

(defcustom transient-semantic-coloring nil
  "Whether to color prefixes and suffixes in Hydra-like fashion.
This feature is experimental.

If non-nil, then the key binding of each suffix is colorized to
indicate whether it exits the transient state or not.  The color
of the prefix is indicated using the line that is drawn when the
value of `transient-mode-line-format' is `line'.

For more information about how Hydra uses colors see
https://github.com/abo-abo/hydra#color and
https://oremacs.com/2015/02/19/hydra-colors-reloaded."
  :package-version '(transient . "0.3.0")
  :group 'transient
  :type 'boolean)

(defcustom transient-detect-key-conflicts nil
  "Whether to detect key binding conflicts.

Conflicts are detected when a transient prefix command is invoked
and results in an error, which prevents the transient from being
used."
  :package-version '(transient . "0.1.0")
  :group 'transient
  :type 'boolean)

(defcustom transient-force-fixed-pitch nil
  "Whether to force use of monospaced font in the popup buffer.

Even if you use a proportional font for the `default' face,
you might still want to use a monospaced font in transient's
popup buffer.  Setting this option to t causes `default' to
be remapped to `fixed-pitch' in that buffer."
  :package-version '(transient . "0.2.0")
  :group 'transient
  :type 'boolean)

(defcustom transient-force-single-column nil
  "Whether to force use of a single column to display suffixes.

This might be useful for users with low vision who use large
text and might otherwise have to scroll in two dimensions."
  :package-version '(transient . "0.3.6")
  :group 'transient
  :type 'boolean)

(defconst transient--default-child-level 1)

(defconst transient--default-prefix-level 4)

(defcustom transient-default-level transient--default-prefix-level
  "Control what suffix levels are made available by default.

Each suffix command is placed on a level and each prefix command
has a level, which controls which suffix commands are available.
Integers between 1 and 7 (inclusive) are valid levels.

The levels of individual transients and/or their individual
suffixes can be changed individually, by invoking the prefix and
then pressing \"C-x l\".

The default level for both transients and their suffixes is 4.
This option only controls the default for transients.  The default
suffix level is always 4.  The author of a transient should place
certain suffixes on a higher level if they expect that it won't be
of use to most users, and they should place very important suffixes
on a lower level so that they remain available even if the user
lowers the transient level.

\(Magit currently places nearly all suffixes on level 4 and lower
levels are not used at all yet.  So for the time being you should
not set a lower level here and using a higher level might not
give you as many additional suffixes as you hoped.)"
  :package-version '(transient . "0.1.0")
  :group 'transient
  :type '(choice (const :tag "1 - fewest suffixes" 1)
                 (const 2)
                 (const 3)
                 (const :tag "4 - default" 4)
                 (const 5)
                 (const 6)
                 (const :tag "7 - most suffixes" 7)))

(defcustom transient-levels-file
  (locate-user-emacs-file (convert-standard-filename "transient/levels.el"))
  "File used to save levels of transients and their suffixes."
  :package-version '(transient . "0.1.0")
  :group 'transient
  :type 'file)

(defcustom transient-values-file
  (locate-user-emacs-file (convert-standard-filename "transient/values.el"))
  "File used to save values of transients."
  :package-version '(transient . "0.1.0")
  :group 'transient
  :type 'file)

(defcustom transient-history-file
  (locate-user-emacs-file (convert-standard-filename "transient/history.el"))
  "File used to save history of transients and their infixes."
  :package-version '(transient . "0.1.0")
  :group 'transient
  :type 'file)

(defcustom transient-history-limit 10
  "Number of history elements to keep when saving to file."
  :package-version '(transient . "0.1.0")
  :group 'transient
  :type 'integer)

(defcustom transient-save-history t
  "Whether to save history of transient commands when exiting Emacs."
  :package-version '(transient . "0.1.0")
  :group 'transient
  :type 'boolean)

;;; Faces

(defgroup transient-faces nil
  "Faces used by Transient."
  :group 'transient)

(defface transient-heading '((t :inherit font-lock-keyword-face))
  "Face used for headings."
  :group 'transient-faces)

(defface transient-key '((t :inherit font-lock-builtin-face))
  "Face used for keys."
  :group 'transient-faces)

(defface transient-argument '((t :inherit font-lock-warning-face))
  "Face used for enabled arguments."
  :group 'transient-faces)

(defface transient-value '((t :inherit font-lock-string-face))
  "Face used for values."
  :group 'transient-faces)

(defface transient-inactive-argument '((t :inherit shadow))
  "Face used for inactive arguments."
  :group 'transient-faces)

(defface transient-inactive-value '((t :inherit shadow))
  "Face used for inactive values."
  :group 'transient-faces)

(defface transient-unreachable '((t :inherit shadow))
  "Face used for suffixes unreachable from the current prefix sequence."
  :group 'transient-faces)

(defface transient-active-infix '((t :inherit secondary-selection))
  "Face used for the infix for which the value is being read."
  :group 'transient-faces)

(defface transient-unreachable-key '((t :inherit shadow))
  "Face used for keys unreachable from the current prefix sequence."
  :group 'transient-faces)

(defface transient-nonstandard-key '((t :underline t))
  "Face optionally used to highlight keys conflicting with short-argument.
Also see option `transient-highlight-mismatched-keys'."
  :group 'transient-faces)

(defface transient-mismatched-key '((t :underline t))
  "Face optionally used to highlight keys without a short-argument.
Also see option `transient-highlight-mismatched-keys'."
  :group 'transient-faces)

(defface transient-inapt-suffix '((t :inherit shadow :italic t))
  "Face used for suffixes that are inapt at this time."
  :group 'transient-faces)

(defface transient-enabled-suffix
  '((t :background "green" :foreground "black" :weight bold))
  "Face used for enabled levels while editing suffix levels.
See info node `(transient)Enabling and Disabling Suffixes'."
  :group 'transient-faces)

(defface transient-disabled-suffix
  '((t :background "red" :foreground "black" :weight bold))
  "Face used for disabled levels while editing suffix levels.
See info node `(transient)Enabling and Disabling Suffixes'."
  :group 'transient-faces)

(defface transient-higher-level '((t :underline t))
  "Face optionally used to highlight suffixes on higher levels.
Also see option `transient-highlight-higher-levels'."
  :group 'transient-faces)

(defface transient-separator
  `((((class color) (background light))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :background "grey80")
    (((class color) (background  dark))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :background "grey30"))
  "Face used to draw line below transient popup window.
This is only used if `transient-mode-line-format' is `line'.
Only the background color is significant."
  :group 'transient-faces)

(defgroup transient-color-faces
  '((transient-semantic-coloring custom-variable))
  "Faces used by Transient for Hydra-like command coloring.
These faces are only used if `transient-semantic-coloring'
\(which see) is non-nil."
  :group 'transient-faces)

(defface transient-red
  '((t :inherit transient-key :foreground "red"))
  "Face used for red prefixes and suffixes."
  :group 'transient-color-faces)

(defface transient-blue
  '((t :inherit transient-key :foreground "blue"))
  "Face used for blue prefixes and suffixes."
  :group 'transient-color-faces)

(defface transient-amaranth
  '((t :inherit transient-key :foreground "#E52B50"))
  "Face used for amaranth prefixes."
  :group 'transient-color-faces)

(defface transient-pink
  '((t :inherit transient-key :foreground "#FF6EB4"))
  "Face used for pink prefixes."
  :group 'transient-color-faces)

(defface transient-teal
  '((t :inherit transient-key :foreground "#367588"))
  "Face used for teal prefixes."
  :group 'transient-color-faces)

;;; Persistence

(defun transient--read-file-contents (file)
  (with-demoted-errors "Transient error: %S"
    (and (file-exists-p file)
         (with-temp-buffer
           (insert-file-contents file)
           (read (current-buffer))))))

(defun transient--pp-to-file (list file)
  (make-directory (file-name-directory file) t)
  (setq list (cl-sort (copy-sequence list) #'string< :key #'car))
  (with-temp-file file
    (let ((print-level nil)
          (print-length nil))
      (pp list (current-buffer)))))

(defvar transient-values
  (transient--read-file-contents transient-values-file)
  "Values of transient commands.
The value of this variable persists between Emacs sessions
and you usually should not change it manually.")

(defun transient-save-values ()
  (transient--pp-to-file transient-values transient-values-file))

(defvar transient-levels
  (transient--read-file-contents transient-levels-file)
  "Levels of transient commands.
The value of this variable persists between Emacs sessions
and you usually should not change it manually.")

(defun transient-save-levels ()
  (transient--pp-to-file transient-levels transient-levels-file))

(defvar transient-history
  (transient--read-file-contents transient-history-file)
  "History of transient commands and infix arguments.
The value of this variable persists between Emacs sessions
\(unless `transient-save-history' is nil) and you usually
should not change it manually.")

(defun transient-save-history ()
  (setq transient-history
        (cl-sort (mapcar (pcase-lambda (`(,key . ,val))
                           (cons key (seq-take (delete-dups val)
                                               transient-history-limit)))
                         transient-history)
                 #'string< :key #'car))
  (transient--pp-to-file transient-history transient-history-file))

(defun transient-maybe-save-history ()
  "Save the value of `transient-history'.
If `transient-save-history' is nil, then do nothing."
  (when transient-save-history
    (transient-save-history)))

(unless noninteractive
  (add-hook 'kill-emacs-hook #'transient-maybe-save-history))

;;; Classes
;;;; Prefix

(defclass transient-prefix ()
  ((prototype   :initarg :prototype)
   (command     :initarg :command)
   (level       :initarg :level)
   (variable    :initarg :variable    :initform nil)
   (init-value  :initarg :init-value)
   (value) (default-value :initarg :value)
   (scope       :initarg :scope       :initform nil)
   (history     :initarg :history     :initform nil)
   (history-pos :initarg :history-pos :initform 0)
   (history-key :initarg :history-key :initform nil)
   (show-help   :initarg :show-help   :initform nil)
   (info-manual :initarg :info-manual :initform nil)
   (man-page    :initarg :man-page    :initform nil)
   (transient-suffix     :initarg :transient-suffix     :initform nil)
   (transient-non-suffix :initarg :transient-non-suffix :initform nil)
   (incompatible         :initarg :incompatible         :initform nil)
   (suffix-description   :initarg :suffix-description)
   (variable-pitch       :initarg :variable-pitch       :initform nil))
  "Transient prefix command.

Each transient prefix command consists of a command, which is
stored in a symbol's function slot and an object, which is
stored in the `transient--prefix' property of the same symbol.

When a transient prefix command is invoked, then a clone of that
object is stored in the global variable `transient--prefix' and
the prototype is stored in the clone's `prototype' slot.")

;;;; Suffix

(defclass transient-child ()
  ((level
    :initarg :level
    :initform (symbol-value 'transient--default-child-level)
    :documentation "Enable if level of prefix is equal or greater.")
   (if
    :initarg :if
    :initform nil
    :documentation "Enable if predicate returns non-nil.")
   (if-not
    :initarg :if-not
    :initform nil
    :documentation "Enable if predicate returns nil.")
   (if-non-nil
    :initarg :if-non-nil
    :initform nil
    :documentation "Enable if variable's value is non-nil.")
   (if-nil
    :initarg :if-nil
    :initform nil
    :documentation "Enable if variable's value is nil.")
   (if-mode
    :initarg :if-mode
    :initform nil
    :documentation "Enable if major-mode matches value.")
   (if-not-mode
    :initarg :if-not-mode
    :initform nil
    :documentation "Enable if major-mode does not match value.")
   (if-derived
    :initarg :if-derived
    :initform nil
    :documentation "Enable if major-mode derives from value.")
   (if-not-derived
    :initarg :if-not-derived
    :initform nil
    :documentation "Enable if major-mode does not derive from value."))
  "Abstract superclass for group and suffix classes.

It is undefined what happens if more than one `if*' predicate
slot is non-nil."
  :abstract t)

(defclass transient-suffix (transient-child)
  ((key         :initarg :key)
   (command     :initarg :command)
   (transient   :initarg :transient)
   (format      :initarg :format      :initform " %k %d")
   (description :initarg :description :initform nil)
   (show-help   :initarg :show-help   :initform nil)
   (inapt                             :initform nil)
   (inapt-if
    :initarg :inapt-if
    :initform nil
    :documentation "Inapt if predicate returns non-nil.")
   (inapt-if-not
    :initarg :inapt-if-not
    :initform nil
    :documentation "Inapt if predicate returns nil.")
   (inapt-if-non-nil
    :initarg :inapt-if-non-nil
    :initform nil
    :documentation "Inapt if variable's value is non-nil.")
   (inapt-if-nil
    :initarg :inapt-if-nil
    :initform nil
    :documentation "Inapt if variable's value is nil.")
   (inapt-if-mode
    :initarg :inapt-if-mode
    :initform nil
    :documentation "Inapt if major-mode matches value.")
   (inapt-if-not-mode
    :initarg :inapt-if-not-mode
    :initform nil
    :documentation "Inapt if major-mode does not match value.")
   (inapt-if-derived
    :initarg :inapt-if-derived
    :initform nil
    :documentation "Inapt if major-mode derives from value.")
   (inapt-if-not-derived
    :initarg :inapt-if-not-derived
    :initform nil
    :documentation "Inapt if major-mode does not derive from value."))
  "Superclass for suffix command.")

(defclass transient-infix (transient-suffix)
  ((transient                         :initform t)
   (argument    :initarg :argument)
   (shortarg    :initarg :shortarg)
   (value                             :initform nil)
   (init-value  :initarg :init-value)
   (unsavable   :initarg :unsavable   :initform nil)
   (multi-value :initarg :multi-value :initform nil)
   (always-read :initarg :always-read :initform nil)
   (allow-empty :initarg :allow-empty :initform nil)
   (history-key :initarg :history-key :initform nil)
   (reader      :initarg :reader      :initform nil)
   (prompt      :initarg :prompt      :initform nil)
   (choices     :initarg :choices     :initform nil)
   (format                            :initform " %k %d (%v)"))
  "Transient infix command."
  :abstract t)

(defclass transient-argument (transient-infix) ()
  "Abstract superclass for infix arguments."
  :abstract t)

(defclass transient-switch (transient-argument) ()
  "Class used for command-line argument that can be turned on and off.")

(defclass transient-option (transient-argument) ()
  "Class used for command-line argument that can take a value.")

(defclass transient-variable (transient-infix)
  ((variable    :initarg :variable)
   (format                            :initform " %k %d %v"))
  "Abstract superclass for infix commands that set a variable."
  :abstract t)

(defclass transient-switches (transient-argument)
  ((argument-format  :initarg :argument-format)
   (argument-regexp  :initarg :argument-regexp))
  "Class used for sets of mutually exclusive command-line switches.")

(defclass transient-files (transient-option) ()
  ((key         :initform "--")
   (argument    :initform "--")
   (multi-value :initform rest)
   (reader      :initform transient-read-files))
  "Class used for the \"--\" argument or similar.
All remaining arguments are treated as files.
They become the value of this argument.")

;;;; Group

(defclass transient-group (transient-child)
  ((suffixes       :initarg :suffixes       :initform nil)
   (hide           :initarg :hide           :initform nil)
   (description    :initarg :description    :initform nil)
   (setup-children :initarg :setup-children)
   (pad-keys       :initarg :pad-keys))
  "Abstract superclass of all group classes."
  :abstract t)

(defclass transient-column (transient-group) ()
  "Group class that displays each element on a separate line.")

(defclass transient-row (transient-group) ()
  "Group class that displays all elements on a single line.")

(defclass transient-columns (transient-group) ()
  "Group class that displays elements organized in columns.
Direct elements have to be groups whose elements have to be
commands or string.  Each subgroup represents a column.  This
class takes care of inserting the subgroups' elements.")

(defclass transient-subgroups (transient-group) ()
  "Group class that wraps other groups.

Direct elements have to be groups whose elements have to be
commands or strings.  This group inserts an empty line between
subgroups.  The subgroups are responsible for displaying their
elements themselves.")

;;; Define

(defmacro transient-define-prefix (name arglist &rest args)
  "Define NAME as a transient prefix command.

ARGLIST are the arguments that command takes.
DOCSTRING is the documentation string and is optional.

These arguments can optionally be followed by key-value pairs.
Each key has to be a keyword symbol, either `:class' or a keyword
argument supported by the constructor of that class.  The
`transient-prefix' class is used if the class is not specified
explicitly.

GROUPs add key bindings for infix and suffix commands and specify
how these bindings are presented in the popup buffer.  At least
one GROUP has to be specified.  See info node `(transient)Binding
Suffix and Infix Commands'.

The BODY is optional.  If it is omitted, then ARGLIST is also
ignored and the function definition becomes:

  (lambda ()
    (interactive)
    (transient-setup \\='NAME))

If BODY is specified, then it must begin with an `interactive'
form that matches ARGLIST, and it must call `transient-setup'.
It may however call that function only when some condition is
satisfied; that is one of the reason why you might want to use
an explicit BODY.

All transients have a (possibly nil) value, which is exported
when suffix commands are called, so that they can consume that
value.  For some transients it might be necessary to have a sort
of secondary value, called a scope.  Such a scope would usually
be set in the commands `interactive' form and has to be passed
to the setup function:

  (transient-setup \\='NAME nil nil :scope SCOPE)

\(fn NAME ARGLIST [DOCSTRING] [KEYWORD VALUE]... GROUP... [BODY...])"
  (declare (debug (&define name lambda-list
                           [&optional lambda-doc]
                           [&rest keywordp sexp]
                           [&rest vectorp]
                           [&optional ("interactive" interactive) def-body]))
           (indent defun)
           (doc-string 3))
  (pcase-let ((`(,class ,slots ,suffixes ,docstr ,body)
               (transient--expand-define-args args)))
    `(progn
       (defalias ',name
         ,(if body
              `(lambda ,arglist ,@body)
            `(lambda ()
               (interactive)
               (transient-setup ',name))))
       (put ',name 'interactive-only t)
       (put ',name 'function-documentation ,docstr)
       (put ',name 'transient--prefix
            (,(or class 'transient-prefix) :command ',name ,@slots))
       (put ',name 'transient--layout
            ',(cl-mapcan (lambda (s) (transient--parse-child name s))
                         suffixes)))))

(defmacro transient-define-suffix (name arglist &rest args)
  "Define NAME as a transient suffix command.

ARGLIST are the arguments that the command takes.
DOCSTRING is the documentation string and is optional.

These arguments can optionally be followed by key-value pairs.
Each key has to be a keyword symbol, either `:class' or a
keyword argument supported by the constructor of that class.
The `transient-suffix' class is used if the class is not
specified explicitly.

The BODY must begin with an `interactive' form that matches
ARGLIST.  The infix arguments are usually accessed by using
`transient-args' inside `interactive'.

\(fn NAME ARGLIST [DOCSTRING] [KEYWORD VALUE]... BODY...)"
  (declare (debug (&define name lambda-list
                           [&optional lambda-doc]
                           [&rest keywordp sexp]
                           ("interactive" interactive)
                           def-body))
           (indent defun)
           (doc-string 3))
  (pcase-let ((`(,class ,slots ,_ ,docstr ,body)
               (transient--expand-define-args args)))
    `(progn
       (defalias ',name (lambda ,arglist ,@body))
       (put ',name 'interactive-only t)
       (put ',name 'function-documentation ,docstr)
       (put ',name 'transient--suffix
            (,(or class 'transient-suffix) :command ',name ,@slots)))))

(defmacro transient-define-infix (name _arglist &rest args)
  "Define NAME as a transient infix command.

ARGLIST is always ignored and reserved for future use.
DOCSTRING is the documentation string and is optional.

The key-value pairs are mandatory.  All transient infix commands
are equal to each other (but not eq), so it is meaningless to
define an infix command without also setting at least `:class'
and one other keyword (which it is depends on the used class,
usually `:argument' or `:variable').

Each key has to be a keyword symbol, either `:class' or a keyword
argument supported by the constructor of that class.  The
`transient-switch' class is used if the class is not specified
explicitly.

The function definitions is always:

   (lambda ()
     (interactive)
     (let ((obj (transient-suffix-object)))
       (transient-infix-set obj (transient-infix-read obj)))
     (transient--show))

`transient-infix-read' and `transient-infix-set' are generic
functions.  Different infix commands behave differently because
the concrete methods are different for different infix command
classes.  In rare case the above command function might not be
suitable, even if you define your own infix command class.  In
that case you have to use `transient-suffix-command' to define
the infix command and use t as the value of the `:transient'
keyword.

\(fn NAME ARGLIST [DOCSTRING] [KEYWORD VALUE]...)"
  (declare (debug (&define name lambda-list
                           [&optional lambda-doc]
                           [&rest keywordp sexp]))
           (indent defun)
           (doc-string 3))
  (pcase-let ((`(,class ,slots ,_ ,docstr ,_)
               (transient--expand-define-args args)))
    `(progn
       (defalias ',name ,(transient--default-infix-command))
       (put ',name 'interactive-only t)
       (put ',name 'function-documentation ,docstr)
       (put ',name 'transient--suffix
            (,(or class 'transient-switch) :command ',name ,@slots)))))

(defalias 'transient-define-argument #'transient-define-infix
  "Define NAME as a transient infix command.

Only use this alias to define an infix command that actually
sets an infix argument.  To define a infix command that, for
example, sets a variable use `transient-define-infix' instead.

\(fn NAME ARGLIST [DOCSTRING] [KEYWORD VALUE]...)")

(defun transient--expand-define-args (args)
  (let (class keys suffixes docstr)
    (when (stringp (car args))
      (setq docstr (pop args)))
    (while (keywordp (car args))
      (let ((k (pop args))
            (v (pop args)))
        (if (eq k :class)
            (setq class v)
          (push k keys)
          (push v keys))))
    (while (let ((arg (car args)))
             (or (vectorp arg)
                 (and arg (symbolp arg))))
      (push (pop args) suffixes))
    (list (if (eq (car-safe class) 'quote)
              (cadr class)
            class)
          (nreverse keys)
          (nreverse suffixes)
          docstr
          args)))

(defun transient--parse-child (prefix spec)
  (cl-etypecase spec
    (symbol  (let ((value (symbol-value spec)))
               (if (and (listp value)
                        (or (listp (car value))
                            (vectorp (car value))))
                   (cl-mapcan (lambda (s) (transient--parse-child prefix s)) value)
                 (transient--parse-child prefix value))))
    (vector  (when-let ((c (transient--parse-group  prefix spec))) (list c)))
    (list    (when-let ((c (transient--parse-suffix prefix spec))) (list c)))
    (string  (list spec))))

(defun transient--parse-group (prefix spec)
  (setq spec (append spec nil))
  (cl-symbol-macrolet
      ((car (car spec))
       (pop (pop spec)))
    (let (level class args)
      (when (integerp car)
        (setq level pop))
      (when (stringp car)
        (setq args (plist-put args :description pop)))
      (while (keywordp car)
        (let ((k pop))
          (if (eq k :class)
              (setq class pop)
            (setq args (plist-put args k pop)))))
      (vector (or level transient--default-child-level)
              (or class
                  (if (vectorp car)
                      'transient-columns
                    'transient-column))
              args
              (cl-mapcan (lambda (s) (transient--parse-child prefix s)) spec)))))

(defun transient--parse-suffix (prefix spec)
  (let (level class args)
    (cl-symbol-macrolet
        ((car (car spec))
         (pop (pop spec)))
      (when (integerp car)
        (setq level pop))
      (when (or (stringp car)
                (vectorp car))
        (setq args (plist-put args :key pop)))
      (when (or (stringp car)
                (eq (car-safe car) 'lambda)
                (and (symbolp car)
                     (not (commandp car))
                     (commandp (cadr spec))))
        (setq args (plist-put args :description pop)))
      (cond
       ((keywordp car)
        (error "Need command, got %S" car))
       ((symbolp car)
        (setq args (plist-put args :command pop)))
       ((and (commandp car)
             (not (stringp car)))
        (let ((cmd pop)
              (sym (intern (format "transient:%s:%s"
                                   prefix
                                   (or (plist-get args :description)
                                       (plist-get args :key))))))
          (defalias sym cmd)
          (setq args (plist-put args :command sym))))
       ((or (stringp car)
            (and car (listp car)))
        (let ((arg pop))
          (cl-typecase arg
            (list
             (setq args (plist-put args :shortarg (car  arg)))
             (setq args (plist-put args :argument (cadr arg)))
             (setq arg  (cadr arg)))
            (string
             (when-let ((shortarg (transient--derive-shortarg arg)))
               (setq args (plist-put args :shortarg shortarg)))
             (setq args (plist-put args :argument arg))))
          (setq args (plist-put args :command
                                (intern (format "transient:%s:%s"
                                                prefix arg))))
          (cond ((and car (not (keywordp car)))
                 (setq class 'transient-option)
                 (setq args (plist-put args :reader pop)))
                ((not (string-suffix-p "=" arg))
                 (setq class 'transient-switch))
                (t
                 (setq class 'transient-option)))))
       (t
        (error "Needed command or argument, got %S" car)))
      (while (keywordp car)
        (let ((k pop))
          (cl-case k
            (:class (setq class pop))
            (:level (setq level pop))
            (t (setq args (plist-put args k pop)))))))
    (unless (plist-get args :key)
      (when-let ((shortarg (plist-get args :shortarg)))
        (setq args (plist-put args :key shortarg))))
    (list (or level transient--default-child-level)
          (or class 'transient-suffix)
          args)))

(defun transient--default-infix-command ()
  (cons 'lambda
        '(()
          (interactive)
          (let ((obj (transient-suffix-object)))
            (transient-infix-set obj (transient-infix-read obj)))
          (transient--show))))

(defun transient--ensure-infix-command (obj)
  (let ((cmd (oref obj command)))
    (unless (or (commandp cmd)
                (get cmd 'transient--infix-command))
      (if (or (cl-typep obj 'transient-switch)
              (cl-typep obj 'transient-option))
          (put cmd 'transient--infix-command
               (transient--default-infix-command))
        ;; This is not an anonymous infix argument.
        (when (transient--use-suffix-p obj)
          (error "Suffix %s is not defined or autoloaded as a command" cmd))))))

(defun transient--derive-shortarg (arg)
  (save-match-data
    (and (string-match "\\`\\(-[a-zA-Z]\\)\\(\\'\\|=\\)" arg)
         (match-string 1 arg))))

;;; Edit

(defun transient--insert-suffix (prefix loc suffix action)
  (let* ((suf (cl-etypecase suffix
                (vector (transient--parse-group  prefix suffix))
                (list   (transient--parse-suffix prefix suffix))
                (string suffix)))
         (mem (transient--layout-member loc prefix))
         (elt (car mem)))
    (cond
     ((not mem)
      (message "Cannot insert %S into %s; %s not found"
               suffix prefix loc))
     ((or (and (vectorp suffix) (not (vectorp elt)))
          (and (listp   suffix) (vectorp elt))
          (and (stringp suffix) (vectorp elt)))
      (message "Cannot place %S into %s at %s; %s"
               suffix prefix loc
               "suffixes and groups cannot be siblings"))
     (t
      (when (and (listp suffix)
                 (listp elt))
        ;; Both suffixes are key bindings; not heading strings.
        (let ((key (transient--spec-key suf)))
          (if (equal (transient--kbd key)
                     (transient--kbd (transient--spec-key elt)))
              ;; We must keep `mem' until after we have inserted
              ;; behind it, which `transient-remove-suffix' does
              ;; not allow us to do.
              (let ((spred (transient--suffix-predicate suf))
                    (epred (transient--suffix-predicate elt)))
                ;; If both suffixes have a predicate and they
                ;; are not identical, then there is a high
                ;; probability that we want to keep both.
                (when (or (not spred)
                          (not epred)
                          (equal spred epred))
                  (setq action 'replace)))
            (transient-remove-suffix prefix key))))
      (cl-ecase action
        (insert  (setcdr mem (cons elt (cdr mem)))
                 (setcar mem suf))
        (append  (setcdr mem (cons suf (cdr mem))))
        (replace (setcar mem suf)))))))

;;;###autoload
(defun transient-insert-suffix (prefix loc suffix)
  "Insert a SUFFIX into PREFIX before LOC.
PREFIX is a prefix command, a symbol.
SUFFIX is a suffix command or a group specification (of
  the same forms as expected by `transient-define-prefix').
LOC is a command, a key vector, a key description (a string
  as returned by `key-description'), or a coordination list
  (whose last element may also be a command or key).
See info node `(transient)Modifying Existing Transients'."
  (declare (indent defun))
  (transient--insert-suffix prefix loc suffix 'insert))

;;;###autoload
(defun transient-append-suffix (prefix loc suffix)
  "Insert a SUFFIX into PREFIX after LOC.
PREFIX is a prefix command, a symbol.
SUFFIX is a suffix command or a group specification (of
  the same forms as expected by `transient-define-prefix').
LOC is a command, a key vector, a key description (a string
  as returned by `key-description'), or a coordination list
  (whose last element may also be a command or key).
See info node `(transient)Modifying Existing Transients'."
  (declare (indent defun))
  (transient--insert-suffix prefix loc suffix 'append))

;;;###autoload
(defun transient-replace-suffix (prefix loc suffix)
  "Replace the suffix at LOC in PREFIX with SUFFIX.
PREFIX is a prefix command, a symbol.
SUFFIX is a suffix command or a group specification (of
  the same forms as expected by `transient-define-prefix').
LOC is a command, a key vector, a key description (a string
  as returned by `key-description'), or a coordination list
  (whose last element may also be a command or key).
See info node `(transient)Modifying Existing Transients'."
  (declare (indent defun))
  (transient--insert-suffix prefix loc suffix 'replace))

;;;###autoload
(defun transient-remove-suffix (prefix loc)
  "Remove the suffix or group at LOC in PREFIX.
PREFIX is a prefix command, a symbol.
LOC is a command, a key vector, a key description (a string
  as returned by `key-description'), or a coordination list
  (whose last element may also be a command or key).
See info node `(transient)Modifying Existing Transients'."
  (declare (indent defun))
  (transient--layout-member loc prefix 'remove))

(defun transient-get-suffix (prefix loc)
  "Return the suffix or group at LOC in PREFIX.
PREFIX is a prefix command, a symbol.
LOC is a command, a key vector, a key description (a string
  as returned by `key-description'), or a coordination list
  (whose last element may also be a command or key).
See info node `(transient)Modifying Existing Transients'."
  (if-let ((mem (transient--layout-member loc prefix)))
      (car mem)
    (error "%s not found in %s" loc prefix)))

(defun transient-suffix-put (prefix loc prop value)
  "Edit the suffix at LOC in PREFIX, setting PROP to VALUE.
PREFIX is a prefix command, a symbol.
SUFFIX is a suffix command or a group specification (of
  the same forms as expected by `transient-define-prefix').
LOC is a command, a key vector, a key description (a string
  as returned by `key-description'), or a coordination list
  (whose last element may also be a command or key).
See info node `(transient)Modifying Existing Transients'."
  (let ((suf (transient-get-suffix prefix loc)))
    (setf (elt suf 2)
          (plist-put (elt suf 2) prop value))))

(defun transient--layout-member (loc prefix &optional remove)
  (let ((val (or (get prefix 'transient--layout)
                 (error "%s is not a transient command" prefix))))
    (when (listp loc)
      (while (integerp (car loc))
        (let* ((children (if (vectorp val) (aref val 3) val))
               (mem (transient--nthcdr (pop loc) children)))
          (if (and remove (not loc))
              (let ((rest (delq (car mem) children)))
                (if (vectorp val)
                    (aset val 3 rest)
                  (put prefix 'transient--layout rest))
                (setq val nil))
            (setq val (if loc (car mem) mem)))))
      (setq loc (car loc)))
    (if loc
        (transient--layout-member-1 (transient--kbd loc) val remove)
      val)))

(defun transient--layout-member-1 (loc layout remove)
  (cond ((listp layout)
         (seq-some (lambda (elt) (transient--layout-member-1 loc elt remove))
                   layout))
        ((vectorp (car (aref layout 3)))
         (seq-some (lambda (elt) (transient--layout-member-1 loc elt remove))
                   (aref layout 3)))
        (remove
         (aset layout 3
               (delq (car (transient--group-member loc layout))
                     (aref layout 3)))
         nil)
        (t (transient--group-member loc layout))))

(defun transient--group-member (loc group)
  (cl-member-if (lambda (suffix)
                  (and (listp suffix)
                       (let* ((def (nth 2 suffix))
                              (cmd (plist-get def :command)))
                         (if (symbolp loc)
                             (eq cmd loc)
                           (equal (transient--kbd
                                   (or (plist-get def :key)
                                       (transient--command-key cmd)))
                                  loc)))))
                (aref group 3)))

(defun transient--kbd (keys)
  (when (vectorp keys)
    (setq keys (key-description keys)))
  (when (stringp keys)
    (setq keys (kbd keys)))
  keys)

(defun transient--spec-key (spec)
  (let ((plist (nth 2 spec)))
    (or (plist-get plist :key)
        (transient--command-key
         (plist-get plist :command)))))

(defun transient--command-key (cmd)
  (when-let ((obj (get cmd 'transient--suffix)))
    (cond ((slot-boundp obj 'key)
           (oref obj key))
          ((slot-exists-p obj 'shortarg)
           (if (slot-boundp obj 'shortarg)
               (oref obj shortarg)
             (transient--derive-shortarg (oref obj argument)))))))

(defun transient--nthcdr (n list)
  (nthcdr (if (< n 0) (- (length list) (abs n)) n) list))

;;; Variables

(defvar transient-current-prefix nil
  "The transient from which this suffix command was invoked.
This is an object representing that transient, use
`transient-current-command' to get the respective command.")

(defvar transient-current-command nil
  "The transient from which this suffix command was invoked.
This is a symbol representing that transient, use
`current-transient-object' to get the respective object.")

(defvar transient-current-suffixes nil
  "The suffixes of the transient from which this suffix command was invoked.
This is a list of objects.  Usually it is sufficient to instead
use the function `transient-args', which returns a list of
values.  In complex cases it might be necessary to use this
variable instead.")

(defvar transient-exit-hook nil
  "Hook run after exiting a transient.")

(defvar transient--prefix nil)
(defvar transient--layout nil)
(defvar transient--suffixes nil)

(defconst transient--stay t   "Do not exit the transient.")
(defconst transient--exit nil "Do exit the transient.")

(defvar transient--exitp nil "Whether to exit the transient.")
(defvar transient--showp nil "Whether the transient is show in a popup buffer.")
(defvar transient--helpp nil "Whether help-mode is active.")
(defvar transient--editp nil "Whether edit-mode is active.")

(defvar transient--active-infix nil "The active infix awaiting user input.")

(defvar transient--timer nil)

(defvar transient--stack nil)

(defvar transient--buffer-name " *transient*"
  "Name of the transient buffer.")

(defvar transient--window nil
  "The window used to display the transient popup.")

(defvar transient--original-window nil
  "The window that was selected before the transient was invoked.
Usually it remains selected while the transient is active.")

(define-obsolete-variable-alias 'transient--source-buffer
  'transient--original-buffer "Transient 0.2.0")

(defvar transient--original-buffer nil
  "The buffer that was current before the transient was invoked.
Usually it remains current while the transient is active.")

(defvar transient--debug nil "Whether put debug information into *Messages*.")

(defvar transient--history nil)

(defvar transient--scroll-commands
  '(transient-scroll-up
    transient-scroll-down
    mwheel-scroll
    scroll-bar-toolkit-scroll))

;;; Identities

(defun transient-suffix-object (&optional command)
  "Return the object associated with the current suffix command.

Each suffix commands is associated with an object, which holds
additional information about the suffix, such as its value (in
the case of an infix command, which is a kind of suffix command).

This function is intended to be called by infix commands, whose
command definition usually (at least when defined using
`transient-define-infix') is this:

   (lambda ()
     (interactive)
     (let ((obj (transient-suffix-object)))
       (transient-infix-set obj (transient-infix-read obj)))
     (transient--show))

\(User input is read outside of `interactive' to prevent the
command from being added to `command-history'.  See #23.)

Such commands need to be able to access their associated object
to guide how `transient-infix-read' reads the new value and to
store the read value.  Other suffix commands (including non-infix
commands) may also need the object to guide their behavior.

This function attempts to return the object associated with the
current suffix command even if the suffix command was not invoked
from a transient.  (For some suffix command that is a valid thing
to do, for others it is not.)  In that case nil may be returned
if the command was not defined using one of the macros intended
to define such commands.

The optional argument COMMAND is intended for internal use.  If
you are contemplating using it in your own code, then you should
probably use this instead:

  (get COMMAND 'transient--suffix)"
  (when command
    (cl-check-type command command))
  (if (or transient--prefix
          transient-current-prefix)
      (cl-find-if (lambda (obj)
                    (eq (transient--suffix-command obj)
                        (or command this-original-command)))
                  (or transient--suffixes
                      transient-current-suffixes))
    (when-let ((obj (get (or command this-command) 'transient--suffix))
               (obj (clone obj)))
      (transient-init-scope obj)
      (transient-init-value obj)
      obj)))

(defun transient--suffix-command (object)
  "Return the command represented by OBJECT.

If the value of OBJECT's `command' slot is a command, then return
that.  Otherwise it is a symbol whose `transient--infix-command'
property holds an anonymous command, which is returned instead."
  (cl-check-type object transient-suffix)
  (let ((sym (oref object command)))
    (if (commandp sym)
        sym
      (get sym 'transient--infix-command))))

(defun transient--suffix-symbol (arg)
  "Return a symbol representing ARG.

ARG must be a command and/or a symbol.  If it is a symbol,
then just return it.  Otherwise return the symbol whose
`transient--infix-command' property's value is ARG."
  (or (cl-typep arg 'command)
      (cl-typep arg 'symbol)
      (signal 'wrong-type-argument `((command symbol) ,arg)))
  (if (symbolp arg)
      arg
    (let* ((obj (transient-suffix-object))
           (sym (oref obj command)))
      (if (eq (get sym 'transient--infix-command) arg)
          sym
        (catch 'found
          (mapatoms (lambda (sym)
                      (when (eq (get sym 'transient--infix-command) arg)
                        (throw 'found sym)))))))))

;;; Keymaps

(defvar transient-base-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "ESC ESC ESC") #'transient-quit-all)
    (define-key map (kbd "C-g")   #'transient-quit-one)
    (define-key map (kbd "C-q")   #'transient-quit-all)
    (define-key map (kbd "C-z")   #'transient-suspend)
    (define-key map (kbd "C-v")   #'transient-scroll-up)
    (define-key map (kbd "C-M-v") #'transient-scroll-down)
    (define-key map [next]        #'transient-scroll-up)
    (define-key map [prior]       #'transient-scroll-down)
    map)
  "Parent of other keymaps used by Transient.

This is the parent keymap of all the keymaps that are used in
all transients: `transient-map' (which in turn is the parent
of the transient-specific keymaps), `transient-edit-map' and
`transient-sticky-map'.

If you change a binding here, then you might also have to edit
`transient-sticky-map' and `transient-common-commands'.  While
the latter isn't a proper transient prefix command, it can be
edited using the same functions as used for transients.

If you add a new command here, then you must also add a binding
to `transient-predicate-map'.")

(defvar transient-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map transient-base-map)
    (define-key map (kbd "C-u")   #'universal-argument)
    (define-key map (kbd "C--")   #'negative-argument)
    (define-key map (kbd "C-t")   #'transient-show)
    (define-key map (kbd "?")     #'transient-help)
    (define-key map (kbd "C-h")   #'transient-help)
    ;; Also bound to "C-x p" and "C-x n" in transient-common-commands.
    (define-key map (kbd "C-M-p") #'transient-history-prev)
    (define-key map (kbd "C-M-n") #'transient-history-next)
    map)
  "Top-level keymap used by all transients.

If you add a new command here, then you must also add a binding
to `transient-predicate-map'.  Also see `transient-base-map'.")

(defvar transient-edit-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map transient-base-map)
    (define-key map (kbd "?")     #'transient-help)
    (define-key map (kbd "C-h")   #'transient-help)
    (define-key map (kbd "C-x l") #'transient-set-level)
    map)
  "Keymap that is active while a transient in is in \"edit mode\".")

(defvar transient-sticky-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map transient-base-map)
    (define-key map (kbd "C-g") #'transient-quit-seq)
    map)
  "Keymap that is active while an incomplete key sequence is active.")

(defvar transient--common-command-prefixes '(?\C-x))

(put 'transient-common-commands
     'transient--layout
     (cl-mapcan
      (lambda (s) (transient--parse-child 'transient-common-commands s))
      '([:hide (lambda ()
                 (and (not (memq (car transient--redisplay-key)
                                 transient--common-command-prefixes))
                      (not transient-show-common-commands)))
         ["Value commands"
          ("C-x s  " "Set"            transient-set)
          ("C-x C-s" "Save"           transient-save)
          ("C-x p  " "Previous value" transient-history-prev)
          ("C-x n  " "Next value"     transient-history-next)]
         ["Sticky commands"
          ;; Like `transient-sticky-map' except that
          ;; "C-g" has to be bound to a different command.
          ("C-g" "Quit prefix or transient" transient-quit-one)
          ("C-q" "Quit transient stack"     transient-quit-all)
          ("C-z" "Suspend transient stack"  transient-suspend)]
         ["Customize"
          ("C-x t" transient-toggle-common
           :description (lambda ()
                          (if transient-show-common-commands
                              "Hide common commands"
                            "Show common permanently")))
          ("C-x l" "Show/hide suffixes" transient-set-level)]])))

(defvar transient-predicate-map
  (let ((map (make-sparse-keymap)))
    (define-key map [handle-switch-frame]     #'transient--do-suspend)
    (define-key map [transient-suspend]       #'transient--do-suspend)
    (define-key map [transient-help]          #'transient--do-stay)
    (define-key map [transient-set-level]     #'transient--do-stay)
    (define-key map [transient-history-prev]  #'transient--do-stay)
    (define-key map [transient-history-next]  #'transient--do-stay)
    (define-key map [universal-argument]      #'transient--do-stay)
    (define-key map [negative-argument]       #'transient--do-stay)
    (define-key map [digit-argument]          #'transient--do-stay)
    (define-key map [transient-quit-all]      #'transient--do-quit-all)
    (define-key map [transient-quit-one]      #'transient--do-quit-one)
    (define-key map [transient-quit-seq]      #'transient--do-stay)
    (define-key map [transient-show]          #'transient--do-stay)
    (define-key map [transient-update]        #'transient--do-stay)
    (define-key map [transient-toggle-common] #'transient--do-stay)
    (define-key map [transient-set]           #'transient--do-call)
    (define-key map [transient-save]          #'transient--do-call)
    (define-key map [describe-key-briefly]    #'transient--do-stay)
    (define-key map [describe-key]            #'transient--do-stay)
    (define-key map [transient-scroll-up]     #'transient--do-stay)
    (define-key map [transient-scroll-down]   #'transient--do-stay)
    (define-key map [mwheel-scroll]           #'transient--do-stay)
    (define-key map [scroll-bar-toolkit-scroll]   #'transient--do-stay)
    (define-key map [transient-noop]              #'transient--do-noop)
    (define-key map [transient-mouse-push-button] #'transient--do-move)
    (define-key map [transient-push-button]       #'transient--do-move)
    (define-key map [transient-backward-button]   #'transient--do-move)
    (define-key map [transient-forward-button]    #'transient--do-move)
    (define-key map [transient-isearch-backward]  #'transient--do-move)
    (define-key map [transient-isearch-forward]   #'transient--do-move)
    map)
  "Base keymap used to map common commands to their transient behavior.

The \"transient behavior\" of a command controls, among other
things, whether invoking the command causes the transient to be
exited or not and whether infix arguments are exported before
doing so.

Each \"key\" is a command that is common to all transients and
that is bound in `transient-map', `transient-edit-map',
`transient-sticky-map' and/or `transient-common-command'.

Each binding is a \"pre-command\", a function that controls the
transient behavior of the respective command.

For transient commands that are bound in individual transients,
the transient behavior is specified using the `:transient' slot
of the corresponding object.")

(defvar transient-popup-navigation-map)

(defvar transient--transient-map nil)
(defvar transient--predicate-map nil)
(defvar transient--redisplay-map nil)
(defvar transient--redisplay-key nil)

(defun transient--push-keymap (map)
  (transient--debug "   push %s%s" map (if (symbol-value map) "" " VOID"))
  (with-demoted-errors "transient--push-keymap: %S"
    (internal-push-keymap (symbol-value map) 'overriding-terminal-local-map)))

(defun transient--pop-keymap (map)
  (transient--debug "   pop  %s%s" map (if (symbol-value map) "" " VOID"))
  (with-demoted-errors "transient--pop-keymap: %S"
    (internal-pop-keymap (symbol-value map) 'overriding-terminal-local-map)))

(defun transient--make-transient-map ()
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map (if transient--editp
                               transient-edit-map
                             transient-map))
    (dolist (obj transient--suffixes)
      (let ((key (oref obj key)))
        (when (vectorp key)
          (setq key (key-description key))
          (oset obj key key))
        (when transient-substitute-key-function
          (setq key (save-match-data
                      (funcall transient-substitute-key-function obj)))
          (oset obj key key))
        (let ((kbd (kbd key))
              (cmd (transient--suffix-command obj)))
          (when-let ((conflict (and transient-detect-key-conflicts
                                    (transient--lookup-key map kbd))))
            (unless (eq cmd conflict)
              (error "Cannot bind %S to %s and also %s"
                     (string-trim key)
                     cmd conflict)))
          (define-key map kbd cmd))))
    (when transient-enable-popup-navigation
      (setq map
            (make-composed-keymap (list map transient-popup-navigation-map))))
    map))

(defun transient--make-predicate-map ()
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map transient-predicate-map)
    (dolist (obj transient--suffixes)
      (let* ((cmd (oref obj command))
             (sub-prefix (and (symbolp cmd) (get cmd 'transient--prefix)))
             (sym (transient--suffix-symbol cmd)))
        (cond
         ((oref obj inapt)
          (define-key map (vector sym) #'transient--do-warn-inapt))
         ((slot-boundp obj 'transient)
          (define-key map (vector sym)
            (let ((do (oref obj transient)))
              (pcase do
                (`t (cond (sub-prefix #'transient--do-replace)
                          ((cl-typep obj 'transient-infix)
                           #'transient--do-stay)
                          (t #'transient--do-call)))
                (`nil 'transient--do-exit)
                (_ do)))))
         ((not (lookup-key transient-predicate-map (vector sym)))
          (define-key map (vector sym)
            (if sub-prefix
                #'transient--do-replace
              (or (oref transient--prefix transient-suffix)
                  #'transient--do-exit)))))))
    map))

(defun transient--make-redisplay-map ()
  (setq transient--redisplay-key
        (cl-case this-command
          (transient-update
           (setq transient--showp t)
           (setq unread-command-events
                 (listify-key-sequence (this-single-command-raw-keys))))
          (transient-quit-seq
           (setq unread-command-events
                 (butlast (listify-key-sequence
                           (this-single-command-raw-keys))
                          2))
           (butlast transient--redisplay-key))
          (t nil)))
  (let ((topmap (make-sparse-keymap))
        (submap (make-sparse-keymap)))
    (when transient--redisplay-key
      (define-key topmap (vconcat transient--redisplay-key) submap)
      (set-keymap-parent submap transient-sticky-map))
    (map-keymap-internal
     (lambda (key def)
       (when (and (not (eq key ?\e))
                  (listp def)
                  (keymapp def))
         (define-key topmap (vconcat transient--redisplay-key (list key))
           #'transient-update)))
     (if transient--redisplay-key
         (lookup-key transient--transient-map (vconcat transient--redisplay-key))
       transient--transient-map))
    topmap))

;;; Setup

(defun transient-setup (&optional name layout edit &rest params)
  "Setup the transient specified by NAME.

This function is called by transient prefix commands to setup the
transient.  In that case NAME is mandatory, LAYOUT and EDIT must
be nil and PARAMS may be (but usually is not) used to set e.g. the
\"scope\" of the transient (see `transient-define-prefix').

This function is also called internally in which case LAYOUT and
EDIT may be non-nil."
  (transient--debug 'setup)
  (when (> (minibuffer-depth) 0)
    (user-error "Cannot invoke transient %s while minibuffer is active" name))
  (transient--with-emergency-exit
    (cond
     ((not name)
      ;; Switching between regular and edit mode.
      (transient--pop-keymap 'transient--transient-map)
      (transient--pop-keymap 'transient--redisplay-map)
      (setq name (oref transient--prefix command))
      (setq params (list :scope (oref transient--prefix scope))))
     (transient--prefix
      ;; Invoked as a ":transient-non-suffix 'transient--do-{stay,call}"
      ;; of an outer prefix.  Unlike the usual `transient--do-replace',
      ;; these predicates fail to clean up after the outer prefix.
      (transient--pop-keymap 'transient--transient-map)
      (transient--pop-keymap 'transient--redisplay-map))
     ((not (or layout                      ; resuming parent/suspended prefix
               transient-current-command)) ; entering child prefix
      (transient--stack-zap))              ; replace suspended prefix, if any
     (edit
      ;; Returning from help to edit.
      (setq transient--editp t)))
    (transient--init-objects name layout params)
    (transient--history-init transient--prefix)
    (setq transient--predicate-map (transient--make-predicate-map))
    (setq transient--transient-map (transient--make-transient-map))
    (setq transient--redisplay-map (transient--make-redisplay-map))
    (setq transient--original-window (selected-window))
    (setq transient--original-buffer (current-buffer))
    (transient--redisplay)
    (transient--init-transient)
    (transient--suspend-which-key-mode)))

(cl-defgeneric transient-setup-children (group children)
  "Setup the CHILDREN of GROUP.
If the value of the `setup-children' slot is non-nil, then call
that function with CHILDREN as the only argument and return the
value.  Otherwise return CHILDREN as is."
  (if (slot-boundp group 'setup-children)
      (funcall (oref group setup-children) children)
    children))

(defun transient--init-objects (name layout params)
  (setq transient--prefix (transient--init-prefix name params))
  (setq transient--layout (or layout (transient--init-suffixes name)))
  (setq transient--suffixes (transient--flatten-suffixes transient--layout)))

(defun transient--init-prefix (name &optional params)
  (let ((obj (let ((proto (get name 'transient--prefix)))
               (apply #'clone proto
                      :prototype proto
                      :level (or (alist-get t (alist-get name transient-levels))
                                 transient-default-level)
                      params))))
    (transient-init-value obj)
    obj))

(defun transient--init-suffixes (name)
  (let ((levels (alist-get name transient-levels)))
    (cl-mapcan (lambda (c) (transient--init-child levels c))
               (append (get name 'transient--layout)
                       (and (not transient--editp)
                            (get 'transient-common-commands
                                 'transient--layout))))))

(defun transient--flatten-suffixes (layout)
  (cl-labels ((s (def)
                 (cond
                  ((stringp def) nil)
                  ((listp def) (cl-mapcan #'s def))
                  ((transient-group--eieio-childp def)
                   (cl-mapcan #'s (oref def suffixes)))
                  ((transient-suffix--eieio-childp def)
                   (list def)))))
    (cl-mapcan #'s layout)))

(defun transient--init-child (levels spec)
  (cl-etypecase spec
    (vector  (transient--init-group  levels spec))
    (list    (transient--init-suffix levels spec))
    (string  (list spec))))

(defun transient--init-group (levels spec)
  (pcase-let ((`(,level ,class ,args ,children) (append spec nil)))
    (when (transient--use-level-p level)
      (let ((obj (apply class :level level args)))
        (when (transient--use-suffix-p obj)
          (when-let ((suffixes
                      (cl-mapcan (lambda (c) (transient--init-child levels c))
                                 (transient-setup-children obj children))))
            (oset obj suffixes suffixes)
            (list obj)))))))

(defun transient--init-suffix (levels spec)
  (pcase-let* ((`(,level ,class ,args) spec)
               (cmd (plist-get args :command))
               (level (or (alist-get (transient--suffix-symbol cmd) levels)
                          level)))
    (let ((fn (and (symbolp cmd)
                   (symbol-function cmd))))
      (when (autoloadp fn)
        (transient--debug "   autoload %s" cmd)
        (autoload-do-load fn)))
    (when (transient--use-level-p level)
      (let ((obj (if-let ((proto (and cmd
                                      (symbolp cmd)
                                      (get cmd 'transient--suffix))))
                     (apply #'clone proto :level level args)
                   (apply class :level level args))))
        (transient--init-suffix-key obj)
        (transient--ensure-infix-command obj)
        (when (transient--use-suffix-p obj)
          (if (transient--inapt-suffix-p obj)
              (oset obj inapt t)
            (transient-init-scope obj)
            (transient-init-value obj))
          (list obj))))))

(cl-defmethod transient--init-suffix-key ((obj transient-suffix))
  (unless (slot-boundp obj 'key)
    (error "No key for %s" (oref obj command))))

(cl-defmethod transient--init-suffix-key ((obj transient-argument))
  (if (transient-switches--eieio-childp obj)
      (cl-call-next-method obj)
    (unless (slot-boundp obj 'shortarg)
      (when-let ((shortarg (transient--derive-shortarg (oref obj argument))))
        (oset obj shortarg shortarg)))
    (unless (slot-boundp obj 'key)
      (if (slot-boundp obj 'shortarg)
          (oset obj key (oref obj shortarg))
        (error "No key for %s" (oref obj command))))))

(defun transient--use-level-p (level &optional edit)
  (or (and transient--editp (not edit))
      (and (>= level 1)
           (<= level (oref transient--prefix level)))))

(defun transient--use-suffix-p (obj)
  (transient--do-suffix-p
   (oref obj if)
   (oref obj if-not)
   (oref obj if-nil)
   (oref obj if-non-nil)
   (oref obj if-mode)
   (oref obj if-not-mode)
   (oref obj if-derived)
   (oref obj if-not-derived)
   t))

(defun transient--inapt-suffix-p (obj)
  (transient--do-suffix-p
   (oref obj inapt-if)
   (oref obj inapt-if-not)
   (oref obj inapt-if-nil)
   (oref obj inapt-if-non-nil)
   (oref obj inapt-if-mode)
   (oref obj inapt-if-not-mode)
   (oref obj inapt-if-derived)
   (oref obj inapt-if-not-derived)
   nil))

(defun transient--do-suffix-p
    (if if-not if-nil if-non-nil if-mode if-not-mode if-derived if-not-derived
        default)
  (cond
   (if                  (funcall if))
   (if-not         (not (funcall if-not)))
   (if-non-nil          (symbol-value if-non-nil))
   (if-nil         (not (symbol-value if-nil)))
   (if-mode             (if (atom if-mode)
                            (eq major-mode if-mode)
                          (memq major-mode if-mode)))
   (if-not-mode    (not (if (atom if-not-mode)
                            (eq major-mode if-not-mode)
                          (memq major-mode if-not-mode))))
   (if-derived          (if (atom if-derived)
                            (derived-mode-p if-derived)
                          (apply #'derived-mode-p if-derived)))
   (if-not-derived (not (if (atom if-not-derived)
                            (derived-mode-p if-not-derived)
                          (apply #'derived-mode-p if-not-derived))))
   (t default)))

(defun transient--suffix-predicate (spec)
  (let ((plist (nth 2 spec)))
    (seq-some (lambda (prop)
                (when-let ((pred (plist-get plist prop)))
                  (list prop pred)))
              '( :if :if-not
                 :if-nil :if-non-nil
                 :if-mode :if-not-mode
                 :if-derived :if-not-derived
                 :inapt-if :inapt-if-not
                 :inapt-if-nil :inapt-if-non-nil
                 :inapt-if-mode :inapt-if-not-mode
                 :inapt-if-derived :inapt-if-not-derived))))

;;; Flow-Control

(defun transient--init-transient ()
  (transient--debug 'init-transient)
  (transient--push-keymap 'transient--transient-map)
  (transient--push-keymap 'transient--redisplay-map)
  (add-hook 'pre-command-hook      #'transient--pre-command)
  (add-hook 'minibuffer-setup-hook #'transient--minibuffer-setup)
  (add-hook 'minibuffer-exit-hook  #'transient--minibuffer-exit)
  (add-hook 'post-command-hook     #'transient--post-command)
  (advice-add 'abort-recursive-edit :after #'transient--minibuffer-exit)
  (when transient--exitp
    ;; This prefix command was invoked as the suffix of another.
    ;; Prevent `transient--post-command' from removing the hooks
    ;; that we just added.
    (setq transient--exitp 'replace)))

(defun transient--pre-command ()
  (transient--debug 'pre-command)
  (cond
   ((memq this-command '(transient-update transient-quit-seq))
    (transient--pop-keymap 'transient--redisplay-map))
   ((and transient--helpp
         (not (memq this-command '(transient-quit-one
                                   transient-quit-all))))
    (cond
     ((transient-help)
      (transient--do-suspend)
      (setq this-command 'transient-suspend)
      (transient--pre-exit))
     ((not (transient--edebug-command-p))
      (setq this-command 'transient-undefined))))
   ((and transient--editp
         (transient-suffix-object)
         (not (memq this-command '(transient-quit-one
                                   transient-quit-all
                                   transient-help))))
    (setq this-command 'transient-set-level))
   (t
    (setq transient--exitp nil)
    (when (eq (if-let ((fn (transient--get-predicate-for
                            this-original-command)))
                  (let ((action (funcall fn)))
                    (when (eq action transient--exit)
                      (setq transient--exitp (or transient--exitp t)))
                    action)
                (if (let ((keys (this-command-keys-vector)))
                      (eq (aref keys (1- (length keys))) ?\C-g))
                    (setq this-command 'transient-noop)
                  (unless (transient--edebug-command-p)
                    (setq this-command 'transient-undefined)))
                transient--stay)
              transient--exit)
      (transient--pre-exit)))))

(defun transient--get-predicate-for (cmd)
  (or (lookup-key transient--predicate-map
                  (vector (transient--suffix-symbol cmd)))
      (oref transient--prefix transient-non-suffix)))

(defun transient--pre-exit ()
  (transient--debug 'pre-exit)
  (transient--delete-window)
  (transient--timer-cancel)
  (transient--pop-keymap 'transient--transient-map)
  (transient--pop-keymap 'transient--redisplay-map)
  (remove-hook 'pre-command-hook #'transient--pre-command)
  (unless transient--showp
    (let ((message-log-max nil))
      (message "")))
  (setq transient--transient-map nil)
  (setq transient--predicate-map nil)
  (setq transient--redisplay-map nil)
  (setq transient--redisplay-key nil)
  (setq transient--showp nil)
  (setq transient--helpp nil)
  (setq transient--editp nil)
  (setq transient--prefix nil)
  (setq transient--layout nil)
  (setq transient--suffixes nil)
  (setq transient--original-window nil)
  (setq transient--original-buffer nil)
  (setq transient--window nil))

(defun transient--delete-window ()
  (when (window-live-p transient--window)
    (let ((buf (window-buffer transient--window)))
      ;; Only delete the window if it never showed another buffer.
      (unless (eq (car (window-parameter transient--window 'quit-restore)) 'other)
        (with-demoted-errors "Error while exiting transient: %S"
          (delete-window transient--window)))
      (kill-buffer buf))))

(defun transient--export ()
  (setq transient-current-prefix transient--prefix)
  (setq transient-current-command (oref transient--prefix command))
  (setq transient-current-suffixes transient--suffixes)
  (transient--history-push transient--prefix))

(defun transient--minibuffer-setup ()
  (transient--debug 'minibuffer-setup)
  (unless (> (minibuffer-depth) 1)
    (unless transient--exitp
      (transient--pop-keymap 'transient--transient-map)
      (transient--pop-keymap 'transient--redisplay-map)
      (remove-hook 'pre-command-hook #'transient--pre-command))
    (remove-hook 'post-command-hook #'transient--post-command)))

(defun transient--minibuffer-exit ()
  (transient--debug 'minibuffer-exit)
  (unless (> (minibuffer-depth) 1)
    (unless transient--exitp
      (transient--push-keymap 'transient--transient-map)
      (transient--push-keymap 'transient--redisplay-map)
      (add-hook 'pre-command-hook #'transient--pre-command))
    (add-hook 'post-command-hook #'transient--post-command)))

(defun transient--suspend-override (&optional minibuffer-hooks)
  (transient--debug 'suspend-override)
  (transient--pop-keymap 'transient--transient-map)
  (transient--pop-keymap 'transient--redisplay-map)
  (remove-hook 'pre-command-hook  #'transient--pre-command)
  (remove-hook 'post-command-hook #'transient--post-command)
  (when minibuffer-hooks
    (remove-hook   'minibuffer-setup-hook #'transient--minibuffer-setup)
    (remove-hook   'minibuffer-exit-hook  #'transient--minibuffer-exit)
    (advice-remove 'abort-recursive-edit  #'transient--minibuffer-exit)))

(defun transient--resume-override (&optional minibuffer-hooks)
  (transient--debug 'resume-override)
  (transient--push-keymap 'transient--transient-map)
  (transient--push-keymap 'transient--redisplay-map)
  (add-hook 'pre-command-hook  #'transient--pre-command)
  (add-hook 'post-command-hook #'transient--post-command)
  (when minibuffer-hooks
    (add-hook   'minibuffer-setup-hook #'transient--minibuffer-setup)
    (add-hook   'minibuffer-exit-hook  #'transient--minibuffer-exit)
    (advice-add 'abort-recursive-edit :after #'transient--minibuffer-exit)))

(defun transient--post-command ()
  (transient--debug 'post-command)
  (unless this-command
    (transient--debug "-- force pre-exit from post-command")
    (message "Quit transient!")
    (transient--pre-exit)
    (setq transient--exitp t))
  (if transient--exitp
      (progn
        (unless (and (eq transient--exitp 'replace)
                     (or transient--prefix
                         ;; The current command could act as a prefix,
                         ;; but decided not to call `transient-setup'.
                         (prog1 nil (transient--stack-zap))))
          (remove-hook   'minibuffer-setup-hook #'transient--minibuffer-setup)
          (remove-hook   'minibuffer-exit-hook  #'transient--minibuffer-exit)
          (advice-remove 'abort-recursive-edit  #'transient--minibuffer-exit)
          (remove-hook   'post-command-hook     #'transient--post-command))
        (setq transient-current-prefix nil)
        (setq transient-current-command nil)
        (setq transient-current-suffixes nil)
        (let ((resume (and transient--stack
                           (not (memq transient--exitp '(replace suspend))))))
          (setq transient--exitp nil)
          (setq transient--helpp nil)
          (setq transient--editp nil)
          (run-hooks 'transient-exit-hook)
          (when resume
            (transient--stack-pop))))
    (transient--pop-keymap 'transient--redisplay-map)
    (setq transient--redisplay-map (transient--make-redisplay-map))
    (transient--push-keymap 'transient--redisplay-map)
    (unless (eq this-command (oref transient--prefix command))
      (transient--redisplay))))

(defun transient--stack-push ()
  (transient--debug 'stack-push)
  (push (list (oref transient--prefix command)
              transient--layout
              transient--editp
              :scope (oref transient--prefix scope))
        transient--stack))

(defun transient--stack-pop ()
  (transient--debug 'stack-pop)
  (and transient--stack
       (prog1 t (apply #'transient-setup (pop transient--stack)))))

(defun transient--stack-zap ()
  (transient--debug 'stack-zap)
  (setq transient--stack nil))

(defun transient--redisplay ()
  (if (or (eq transient-show-popup t)
          transient--showp)
      (unless (memq this-command transient--scroll-commands)
        (transient--show))
    (when (and (numberp transient-show-popup)
               (not (zerop transient-show-popup))
               (not transient--timer))
      (transient--timer-start))
    (transient--show-brief)))

(defun transient--timer-start ()
  (setq transient--timer
        (run-at-time (abs transient-show-popup) nil
                     (lambda ()
                       (transient--timer-cancel)
                       (transient--show)
                       (let ((message-log-max nil))
                         (message ""))))))

(defun transient--timer-cancel ()
  (when transient--timer
    (cancel-timer transient--timer)
    (setq transient--timer nil)))

(defun transient--debug (arg &rest args)
  (when transient--debug
    (if (symbolp arg)
        (message "-- %-16s (cmd: %s, event: %S, exit: %s)"
                 arg
                 (or (transient--suffix-symbol this-command)
                     (list this-command this-original-command last-command))
                 (key-description (this-command-keys-vector))
                 transient--exitp)
      (apply #'message arg args))))

(defun transient--emergency-exit ()
  "Exit the current transient command after an error occurred.
When no transient is active (i.e. when `transient--prefix' is
nil) then do nothing."
  (transient--debug 'emergency-exit)
  (when transient--prefix
    (setq transient--stack nil)
    (setq transient--exitp t)
    (transient--pre-exit)
    (transient--post-command)))

;;; Pre-Commands

(defun transient--do-stay ()
  "Call the command without exporting variables and stay transient."
  transient--stay)

(defun transient--do-noop ()
  "Call `transient-noop' and stay transient."
  (setq this-command 'transient-noop)
  transient--stay)

(defun transient--do-warn ()
  "Call `transient-undefined' and stay transient."
  (setq this-command 'transient-undefined)
  transient--stay)

(defun transient--do-warn-inapt ()
  "Call `transient-inapt' and stay transient."
  (setq this-command 'transient-inapt)
  transient--stay)

(defun transient--do-call ()
  "Call the command after exporting variables and stay transient."
  (transient--export)
  transient--stay)

(defun transient--do-exit ()
  "Call the command after exporting variables and exit the transient."
  (transient--export)
  (transient--stack-zap)
  transient--exit)

(defun transient--do-replace ()
  "Call the transient prefix command, replacing the active transient."
  (transient--export)
  (transient--stack-push)
  (setq transient--exitp 'replace)
  transient--exit)

(defun transient--do-suspend ()
  "Suspend the active transient, saving the transient stack."
  (transient--stack-push)
  (setq transient--exitp 'suspend)
  transient--exit)

(defun transient--do-quit-one ()
  "If active, quit help or edit mode, else exit the active transient."
  (cond (transient--helpp
         (setq transient--helpp nil)
         transient--stay)
        (transient--editp
         (setq transient--editp nil)
         (transient-setup)
         transient--stay)
        (t transient--exit)))

(defun transient--do-quit-all ()
  "Exit all transients without saving the transient stack."
  (transient--stack-zap)
  transient--exit)

(defun transient--do-move ()
  "Call the command if `transient-enable-popup-navigation' is non-nil.
In that case behave like `transient--do-stay', otherwise similar
to `transient--do-warn'."
  (unless transient-enable-popup-navigation
    (setq this-command 'transient-popup-navigation-help))
  transient--stay)

(put 'transient--do-stay       'transient-color 'transient-red)
(put 'transient--do-noop       'transient-color 'transient-red)
(put 'transient--do-warn       'transient-color 'transient-red)
(put 'transient--do-warn-inapt 'transient-color 'transient-red)
(put 'transient--do-call       'transient-color 'transient-red)
(put 'transient--do-exit       'transient-color 'transient-blue)
(put 'transient--do-replace    'transient-color 'transient-blue)
(put 'transient--do-suspend    'transient-color 'transient-blue)
(put 'transient--do-quit-one   'transient-color 'transient-blue)
(put 'transient--do-quit-all   'transient-color 'transient-blue)
(put 'transient--do-move       'transient-color 'transient-red)

;;; Commands

(defun transient-noop ()
  "Do nothing at all."
  (interactive))

(defun transient-undefined ()
  "Warn the user that the pressed key is not bound to any suffix."
  (interactive)
  (transient--invalid "Unbound suffix"))

(defun transient-inapt ()
  "Warn the user that the invoked command is inapt."
  (interactive)
  (transient--invalid "Inapt command"))

(defun transient--invalid (msg)
  (ding)
  (message "%s: `%s' (Use `%s' to abort, `%s' for help) [%s]"
           msg
           (propertize (key-description (this-single-command-keys))
                       'face 'font-lock-warning-face)
           (propertize "C-g" 'face 'transient-key)
           (propertize "?"   'face 'transient-key)
           (propertize (symbol-name (transient--suffix-symbol
                                     this-original-command))
                       'face 'font-lock-warning-face))
  (unless (and transient--transient-map
               (memq transient--transient-map overriding-terminal-local-map))
    (let ((transient--prefix (or transient--prefix 'sic)))
      (transient--emergency-exit))
    (view-lossage)
    (other-window 1)
    (display-warning 'transient "Inconsistent transient state detected.
This should never happen.
Please open an issue and post the shown command log.
This is a heisenbug, so any additional details might help.
Thanks!" :error)))

(defun transient-toggle-common ()
  "Toggle whether common commands are always shown."
  (interactive)
  (setq transient-show-common-commands (not transient-show-common-commands)))

(defun transient-suspend ()
  "Suspend the current transient.
It can later be resumed using `transient-resume' while no other
transient is active."
  (interactive))

(defun transient-quit-all ()
  "Exit all transients without saving the transient stack."
  (interactive))

(defun transient-quit-one ()
  "Exit the current transients, possibly returning to the previous."
  (interactive))

(defun transient-quit-seq ()
  "Abort the current incomplete key sequence."
  (interactive))

(defun transient-update ()
  "Redraw the transient's state in the popup buffer."
  (interactive))

(defun transient-show ()
  "Show the transient's state in the popup buffer."
  (interactive)
  (setq transient--showp t))

(defvar-local transient--restore-winconf nil)

(defvar transient-resume-mode)

(defun transient-help ()
  "Show help for the active transient or one of its suffixes."
  (interactive)
  (if (called-interactively-p 'any)
      (setq transient--helpp t)
    (with-demoted-errors "transient-help: %S"
      (when (lookup-key transient--transient-map
                        (this-single-command-raw-keys))
        (setq transient--helpp nil)
        (let ((winconf (current-window-configuration)))
          (transient-show-help
           (if (eq this-original-command 'transient-help)
               transient--prefix
             (or (transient-suffix-object)
                 this-original-command)))
          (setq transient--restore-winconf winconf))
        (fit-window-to-buffer nil (frame-height) (window-height))
        (transient-resume-mode)
        (message "Type \"q\" to resume transient command.")
        t))))

(defun transient-set-level (&optional command level)
  "Set the level of the transient or one of its suffix commands."
  (interactive
   (let ((command this-original-command)
         (prefix (oref transient--prefix command)))
     (and (or (not (eq command 'transient-set-level))
              (and transient--editp
                   (setq command prefix)))
          (list command
                (let ((keys (this-single-command-raw-keys)))
                  (and (lookup-key transient--transient-map keys)
                       (string-to-number
                        (let ((transient--active-infix
                               (transient-suffix-object command)))
                          (transient--show)
                          (transient--read-number-N
                           (format "Set level for `%s': "
                                   (transient--suffix-symbol command))
                           nil nil (not (eq command prefix)))))))))))
  (cond
   ((not command)
    (setq transient--editp t)
    (transient-setup))
   (level
    (let* ((prefix (oref transient--prefix command))
           (alist (alist-get prefix transient-levels))
           (sym (transient--suffix-symbol command)))
      (if (eq command prefix)
          (progn (oset transient--prefix level level)
                 (setq sym t))
        (oset (transient-suffix-object command) level level))
      (setf (alist-get sym alist) level)
      (setf (alist-get prefix transient-levels) alist))
    (transient-save-levels))
   (t
    (transient-undefined))))

(defun transient-set ()
  "Save the value of the active transient for this Emacs session."
  (interactive)
  (transient-set-value (or transient--prefix transient-current-prefix)))

(defun transient-save ()
  "Save the value of the active transient persistenly across Emacs sessions."
  (interactive)
  (transient-save-value (or transient--prefix transient-current-prefix)))

(defun transient-history-next ()
  "Switch to the next value used for the active transient."
  (interactive)
  (let* ((obj transient--prefix)
         (pos (1- (oref obj history-pos)))
         (hst (oref obj history)))
    (if (< pos 0)
        (user-error "End of history")
      (oset obj history-pos pos)
      (oset obj value (nth pos hst))
      (mapc #'transient-init-value transient--suffixes))))

(defun transient-history-prev ()
  "Switch to the previous value used for the active transient."
  (interactive)
  (let* ((obj transient--prefix)
         (pos (1+ (oref obj history-pos)))
         (hst (oref obj history))
         (len (length hst)))
    (if (> pos (1- len))
        (user-error "End of history")
      (oset obj history-pos pos)
      (oset obj value (nth pos hst))
      (mapc #'transient-init-value transient--suffixes))))

(defun transient-scroll-up (&optional arg)
  "Scroll text of transient popup window upward ARG lines.
If ARG is nil scroll near full screen.  This is a wrapper
around `scroll-up-command' (which see)."
  (interactive "^P")
  (with-selected-window transient--window
    (scroll-up-command arg)))

(defun transient-scroll-down (&optional arg)
  "Scroll text of transient popup window down ARG lines.
If ARG is nil scroll near full screen.  This is a wrapper
around `scroll-down-command' (which see)."
  (interactive "^P")
  (with-selected-window transient--window
    (scroll-down-command arg)))

(defun transient-resume ()
  "Resume a previously suspended stack of transients."
  (interactive)
  (cond (transient--stack
         (let ((winconf transient--restore-winconf))
           (kill-local-variable 'transient--restore-winconf)
           (when transient-resume-mode
             (transient-resume-mode -1)
             (quit-window))
           (when winconf
             (set-window-configuration winconf)))
         (transient--stack-pop))
        (transient-resume-mode
         (kill-local-variable 'transient--restore-winconf)
         (transient-resume-mode -1)
         (quit-window))
        (t
         (message "No suspended transient command"))))

;;; Value
;;;; Init

(cl-defgeneric transient-init-scope (obj)
  "Set the scope of the suffix object OBJ.

The scope is actually a property of the transient prefix, not of
individual suffixes.  However it is possible to invoke a suffix
command directly instead of from a transient.  In that case, if
the suffix expects a scope, then it has to determine that itself
and store it in its `scope' slot.

This function is called for all suffix commands, but unless a
concrete method is implemented this falls through to the default
implementation, which is a noop.")

(cl-defmethod transient-init-scope ((_   transient-suffix))
  "Noop." nil)

(cl-defgeneric transient-init-value (_)
  "Set the initial value of the object OBJ.

This function is called for all prefix and suffix commands.

For suffix commands (including infix argument commands) the
default implementation is a noop.  Classes derived from the
abstract `transient-infix' class must implement this function.
Non-infix suffix commands usually don't have a value."
  nil)

(cl-defmethod transient-init-value :around ((obj transient-prefix))
  "If bound, then call OBJ's `init-value' function.
Otherwise call the primary method according to object's class."
  (if (slot-boundp obj 'init-value)
      (funcall (oref obj init-value) obj)
    (cl-call-next-method obj)))

(cl-defmethod transient-init-value :around ((obj transient-infix))
  "If bound, then call OBJ's `init-value' function.
Otherwise call the primary method according to object's class."
  (if (slot-boundp obj 'init-value)
      (funcall (oref obj init-value) obj)
    (cl-call-next-method obj)))

(cl-defmethod transient-init-value ((obj transient-prefix))
  (if (slot-boundp obj 'value)
      (oref obj value)
    (oset obj value
          (if-let ((saved (assq (oref obj command) transient-values)))
              (cdr saved)
            (if-let ((default (and (slot-boundp obj 'default-value)
                                   (oref obj default-value))))
                (if (functionp default)
                    (funcall default)
                  default)
              nil)))))

(cl-defmethod transient-init-value ((obj transient-argument))
  (oset obj value
        (let ((value (oref transient--prefix value))
              (argument (and (slot-boundp obj 'argument)
                             (oref obj argument)))
              (multi-value (oref obj multi-value))
              (regexp (if (slot-exists-p obj 'argument-regexp)
                          (oref obj argument-regexp)
                        (format "\\`%s\\(.*\\)" (oref obj argument)))))
          (if (memq multi-value '(t rest))
              (cdr (assoc argument value))
            (let ((match (lambda (v)
                           (and (stringp v)
                                (string-match regexp v)
                                (match-string 1 v)))))
              (if multi-value
                  (delq nil (mapcar match value))
                (cl-some match value)))))))

(cl-defmethod transient-init-value ((obj transient-switch))
  (oset obj value
        (car (member (oref obj argument)
                     (oref transient--prefix value)))))

;;;; Read

(cl-defgeneric transient-infix-read (obj)
  "Determine the new value of the infix object OBJ.

This function merely determines the value; `transient-infix-set'
is used to actually store the new value in the object.

For most infix classes this is done by reading a value from the
user using the reader specified by the `reader' slot (using the
`transient-infix' method described below).

For some infix classes the value is changed without reading
anything in the minibuffer, i.e. the mere act of invoking the
infix command determines what the new value should be, based
on the previous value.")

(cl-defmethod transient-infix-read :around ((obj transient-infix))
  "Highlight the infix in the popup buffer.

Also arrange for the transient to be exited in case of an error
because otherwise Emacs would get stuck in an inconsistent state,
which might make it necessary to kill it from the outside."
  (let ((transient--active-infix obj))
    (transient--show))
  (transient--with-emergency-exit
    (cl-call-next-method obj)))

(cl-defmethod transient-infix-read ((obj transient-infix))
  "Read a value while taking care of history.

This method is suitable for a wide variety of infix commands,
including but not limited to inline arguments and variables.

If you do not use this method for your own infix class, then
you should likely replicate a lot of the behavior of this
method.  If you fail to do so, then users might not appreciate
the lack of history, for example.

Only for very simple classes that toggle or cycle through a very
limited number of possible values should you replace this with a
simple method that does not handle history.  (E.g. for a command
line switch the only possible values are \"use it\" and \"don't use
it\", in which case it is pointless to preserve history.)"
  (with-slots (value multi-value always-read allow-empty choices) obj
    (if (and value
             (not multi-value)
             (not always-read)
             transient--prefix)
        (oset obj value nil)
      (let* ((overriding-terminal-local-map nil)
             (reader (oref obj reader))
             (prompt (transient-prompt obj))
             (value (if multi-value (mapconcat #'identity value ",") value))
             (history-key (or (oref obj history-key)
                              (oref obj command)))
             (transient--history (alist-get history-key transient-history))
             (transient--history (if (or (null value)
                                         (eq value (car transient--history)))
                                     transient--history
                                   (cons value transient--history)))
             (initial-input (and transient-read-with-initial-input
                                 (car transient--history)))
             (history (if initial-input
                          (cons 'transient--history 1)
                        'transient--history))
             (value
              (cond
               (reader (funcall reader prompt initial-input history))
               (multi-value
                (completing-read-multiple prompt choices nil nil
                                          initial-input history))
               (choices
                (completing-read prompt choices nil t initial-input history))
               (t (read-string prompt initial-input history)))))
        (cond ((and (equal value "") (not allow-empty))
               (setq value nil))
              ((and (equal value "\"\"") allow-empty)
               (setq value "")))
        (when value
          (when (and (bound-and-true-p ivy-mode)
                     (stringp (car transient--history)))
            (set-text-properties 0 (length (car transient--history)) nil
                                 (car transient--history)))
          (setf (alist-get history-key transient-history)
                (delete-dups transient--history)))
        value))))

(cl-defmethod transient-infix-read ((obj transient-switch))
  "Toggle the switch on or off."
  (if (oref obj value) nil (oref obj argument)))

(cl-defmethod transient-infix-read ((obj transient-switches))
  "Cycle through the mutually exclusive switches.
The last value is \"don't use any of these switches\"."
  (let ((choices (mapcar (apply-partially #'format (oref obj argument-format))
                         (oref obj choices))))
    (if-let ((value (oref obj value)))
        (cadr (member value choices))
      (car choices))))

(cl-defmethod transient-infix-read ((command symbol))
  "Elsewhere use the reader of the infix command COMMAND.
Use this if you want to share an infix's history with a regular
stand-alone command."
  (cl-letf (((symbol-function #'transient--show) #'ignore))
    (transient-infix-read (get command 'transient--suffix))))

;;;; Readers

(defun transient-read-file (prompt _initial-input _history)
  "Read a file."
  (file-local-name (expand-file-name (read-file-name prompt))))

(defun transient-read-existing-file (prompt _initial-input _history)
  "Read an existing file."
  (file-local-name (expand-file-name (read-file-name prompt nil nil t))))

(defun transient-read-directory (prompt _initial-input _history)
  "Read a directory."
  (file-local-name (expand-file-name (read-directory-name prompt))))

(defun transient-read-existing-directory (prompt _initial-input _history)
  "Read an existing directory."
  (file-local-name (expand-file-name (read-directory-name prompt nil nil t))))

(defun transient-read-number-N0 (prompt initial-input history)
  "Read a natural number (including zero) and return it as a string."
  (transient--read-number-N prompt initial-input history t))

(defun transient-read-number-N+ (prompt initial-input history)
  "Read a natural number (excluding zero) and return it as a string."
  (transient--read-number-N prompt initial-input history nil))

(defun transient--read-number-N (prompt initial-input history include-zero)
  (save-match-data
    (cl-block nil
      (while t
        (let ((str (read-from-minibuffer prompt initial-input nil nil history)))
          (when (or (string-equal str "")
                    (string-match-p (if include-zero
                                        "\\`\\(0\\|[1-9][0-9]*\\)\\'"
                                      "\\`[1-9][0-9]*\\'")
                                    str))
            (cl-return str)))
        (message "Please enter a natural number (%s zero)."
                 (if include-zero "including" "excluding"))
        (sit-for 1)))))

(defun transient-read-date (prompt default-time _history)
  "Read a date using `org-read-date' (which see)."
  (require 'org)
  (when (fboundp 'org-read-date)
    (org-read-date 'with-time nil nil prompt default-time)))

;;;; Prompt

(cl-defgeneric transient-prompt (obj)
  "Return the prompt to be used to read infix object OBJ's value.")

(cl-defmethod transient-prompt ((obj transient-infix))
  "Return the prompt to be used to read infix object OBJ's value.

This implementation should be suitable for almost all infix
commands.

If the value of OBJ's `prompt' slot is non-nil, then it must be
a string or a function.  If it is a string, then use that.  If
it is a function, then call that with OBJ as the only argument.
That function must return a string, which is then used as the
prompt.

Otherwise, if the value of either the `argument' or `variable'
slot of OBJ is a string, then base the prompt on that (preferring
the former), appending either \"=\" (if it appears to be a
command-line option) or \": \".

Finally fall through to using \"(BUG: no prompt): \" as the
prompt."
  (if-let ((prompt (oref obj prompt)))
      (let ((prompt (if (functionp prompt)
                        (funcall prompt obj)
                      prompt)))
        (if (stringp prompt)
            prompt
          "(BUG: no prompt): "))
    (or (when-let ((arg (and (slot-boundp obj 'argument) (oref obj argument))))
          (if (and (stringp arg) (string-suffix-p "=" arg))
              arg
            (concat arg ": ")))
        (when-let ((var (and (slot-boundp obj 'variable) (oref obj variable))))
          (and (stringp var)
               (concat var ": ")))
        "(BUG: no prompt): ")))

;;;; Set

(defvar transient--unset-incompatible t)

(cl-defgeneric transient-infix-set (obj value)
  "Set the value of infix object OBJ to value.")

(cl-defmethod transient-infix-set ((obj transient-infix) value)
  "Set the value of infix object OBJ to value."
  (oset obj value value))

(cl-defmethod transient-infix-set :around ((obj transient-argument) value)
  "Unset incompatible infix arguments."
  (let ((arg (if (slot-boundp obj 'argument)
                 (oref obj argument)
               (oref obj argument-regexp))))
    (if-let ((sic (and value arg transient--unset-incompatible))
             (spec (oref transient--prefix incompatible))
             (incomp (cl-mapcan (lambda (rule)
                                  (and (member arg rule)
                                       (remove arg rule)))
                                spec)))
        (progn
          (cl-call-next-method obj value)
          (dolist (arg incomp)
            (when-let ((obj (cl-find-if (lambda (obj)
                                          (and (slot-boundp obj 'argument)
                                               (equal (oref obj argument) arg)))
                                        transient--suffixes)))
              (let ((transient--unset-incompatible nil))
                (transient-infix-set obj nil)))))
      (cl-call-next-method obj value))))

(cl-defmethod transient-set-value ((obj transient-prefix))
  (oset (oref obj prototype) value (transient-get-value))
  (transient--history-push obj))

;;;; Save

(cl-defmethod transient-save-value ((obj transient-prefix))
  (let ((value (transient-get-value)))
    (oset (oref obj prototype) value value)
    (setf (alist-get (oref obj command) transient-values) value)
    (transient-save-values))
  (transient--history-push obj))

;;;; Get

(defun transient-args (prefix)
  "Return the value of the transient prefix command PREFIX.
If the current command was invoked from the transient prefix
command PREFIX, then return the active infix arguments.  If
the current command was not invoked from PREFIX, then return
the set, saved or default value for PREFIX."
  (cl-mapcan #'transient--get-wrapped-value (transient-suffixes prefix)))

(defun transient-suffixes (prefix)
  "Return the suffix objects of the transient prefix command PREFIX."
  (if (eq transient-current-command prefix)
      transient-current-suffixes
    (let ((transient--prefix (transient--init-prefix prefix)))
      (transient--flatten-suffixes
       (transient--init-suffixes prefix)))))

(defun transient-get-value ()
  (transient--with-emergency-exit
    (cl-mapcan (lambda (obj)
                 (and (or (not (slot-exists-p obj 'unsavable))
                          (not (oref obj unsavable)))
                      (transient--get-wrapped-value obj)))
               transient-current-suffixes)))

(defun transient--get-wrapped-value (obj)
  (when-let ((value (transient-infix-value obj)))
    (cl-ecase (and (slot-exists-p obj 'multi-value)
                   (oref obj multi-value))
      ((nil)    (list value))
      ((t rest) (list value))
      (repeat   value))))

(cl-defgeneric transient-infix-value (obj)
  "Return the value of the suffix object OBJ.

This function is called by `transient-args' (which see), meaning
this function is how the value of a transient is determined so
that the invoked suffix command can use it.

Currently most values are strings, but that is not set in stone.
Nil is not a value, it means \"no value\".

Usually only infixes have a value, but see the method for
`transient-suffix'.")

(cl-defmethod transient-infix-value ((_   transient-suffix))
  "Return nil, which means \"no value\".

Infix arguments contribute the transient's value while suffix
commands consume it.  This function is called for suffixes anyway
because a command that both contributes to the transient's value
and also consumes it is not completely unconceivable.

If you define such a command, then you must define a derived
class and implement this function because this default method
does nothing." nil)

(cl-defmethod transient-infix-value ((obj transient-infix))
  "Return the value of OBJ's `value' slot."
  (oref obj value))

(cl-defmethod transient-infix-value ((obj transient-option))
  "Return ARGUMENT and VALUE as a unit or nil if the latter is nil."
  (when-let ((value (oref obj value)))
    (let ((arg (oref obj argument)))
      (cl-ecase (oref obj multi-value)
        ((nil)    (concat arg value))
        ((t rest) (cons arg value))
        (repeat   (mapcar (lambda (v) (concat arg v)) value))))))

(cl-defmethod transient-infix-value ((_   transient-variable))
  "Return nil, which means \"no value\".

Setting the value of a variable is done by, well, setting the
value of the variable.  I.e. this is a side-effect and does not
contribute to the value of the transient."
  nil)

;;;; Utilities

(defun transient-arg-value (arg args)
  "Return the value of ARG as it appears in ARGS.

For a switch return a boolean.  For an option return the value as
a string, using the empty string for the empty value, or nil if
the option does not appear in ARGS."
  (if (string-match-p "=\\'" arg)
      (save-match-data
        (when-let ((match (let ((re (format "\\`%s\\(?:=\\(.+\\)\\)?\\'"
                                            (substring arg 0 -1))))
                            (cl-find-if (lambda (a)
                                          (and (stringp a)
                                               (string-match re a)))
                                        args))))
          (or (match-string 1 match) "")))
    (and (member arg args) t)))

;;; History

(cl-defgeneric transient--history-key (obj)
  "Return OBJ's history key.
If the value of the `history-key' slot is non-nil, then return
that.  Otherwise return the value of the `command' slot."
  (or (oref obj history-key)
      (oref obj command)))

(cl-defgeneric transient--history-push (obj)
  "Push the current value of OBJ to its entry in `transient-history'."
  (let ((key (transient--history-key obj)))
    (setf (alist-get key transient-history)
          (let ((args (transient-get-value)))
            (cons args (delete args (alist-get key transient-history)))))))

(cl-defgeneric transient--history-init (obj)
  "Initialize OBJ's `history' slot.
This is the transient-wide history; many individual infixes also
have a history of their own.")

(cl-defmethod transient--history-init ((obj transient-prefix))
  "Initialize OBJ's `history' slot from the variable `transient-history'."
  (let ((val (oref obj value)))
    (oset obj history
          (cons val (delete val (alist-get (transient--history-key obj)
                                           transient-history))))))

;;; Draw

(defun transient--show-brief ()
  (let ((message-log-max nil))
    (if (and transient-show-popup (<= transient-show-popup 0))
        (message "%s-" (key-description (this-command-keys)))
      (message
       "%s- [%s] %s"
       (key-description (this-command-keys))
       (oref transient--prefix command)
       (mapconcat
        #'identity
        (sort
         (cl-mapcan
          (lambda (suffix)
            (let ((key (kbd (oref suffix key))))
              ;; Don't list any common commands.
              (and (not (memq (oref suffix command)
                              `(,(lookup-key transient-map key)
                                ,(lookup-key transient-sticky-map key)
                                ;; From transient-common-commands:
                                transient-set
                                transient-save
                                transient-history-prev
                                transient-history-next
                                transient-quit-one
                                transient-toggle-common
                                transient-set-level)))
                   (list (propertize (oref suffix key) 'face 'transient-key)))))
          transient--suffixes)
         #'string<)
        (propertize "|" 'face 'transient-unreachable-key))))))

(defun transient--show ()
  (transient--timer-cancel)
  (setq transient--showp t)
  (let ((buf (get-buffer-create transient--buffer-name))
        (focus nil))
    (with-current-buffer buf
      (when transient-enable-popup-navigation
        (setq focus (or (button-get (point) 'command)
                        (transient--heading-at-point))))
      (erase-buffer)
      (setq window-size-fixed t)
      (when (bound-and-true-p tab-line-format)
        (setq tab-line-format nil))
      (setq mode-line-format (if (eq transient-mode-line-format 'line)
                                 nil
                               transient-mode-line-format))
      (setq mode-line-buffer-identification
            (symbol-name (oref transient--prefix command)))
      (if transient-enable-popup-navigation
          (setq-local cursor-in-non-selected-windows 'box)
        (setq cursor-type nil))
      (setq display-line-numbers nil)
      (setq show-trailing-whitespace nil)
      (transient--insert-groups)
      (when (or transient--helpp transient--editp)
        (transient--insert-help))
      (when (and (eq transient-mode-line-format 'line)
                 window-system)
        (let ((face
               (if-let ((f (and (transient--semantic-coloring-p)
                                (transient--prefix-color transient--prefix))))
                   `(,@(and (>= emacs-major-version 27) '(:extend t))
                     :background ,(face-foreground f))
                 'transient-separator)))
          (insert (propertize "__" 'face face 'display '(space :height (1))))
          (insert (propertize "\n" 'face face 'line-height t))))
      (when transient-force-fixed-pitch
        (transient--force-fixed-pitch)))
    (unless (window-live-p transient--window)
      (setq transient--window
            (display-buffer buf transient-display-buffer-action)))
    (when (window-live-p transient--window)
      (with-selected-window transient--window
        (goto-char (point-min))
        (when transient-enable-popup-navigation
          (transient--goto-button focus))
        (magit--fit-window-to-buffer transient--window)))))

(defun magit--fit-window-to-buffer (window)
  (let ((window-resize-pixelwise t)
        (window-size-fixed nil))
    (if (eq (car (window-parameter window 'quit-restore)) 'other)
        ;; Grow but never shrink window that previously displayed
        ;; another buffer and is going to display that again.
        (fit-window-to-buffer window nil (window-height window))
      (fit-window-to-buffer window nil 1))))

(defun transient--insert-groups ()
  (let ((groups (cl-mapcan (lambda (group)
                             (let ((hide (oref group hide)))
                               (and (not (and (functionp hide)
                                              (funcall   hide)))
                                    (list group))))
                           transient--layout))
        group)
    (while (setq group (pop groups))
      (transient--insert-group group)
      (when groups
        (insert ?\n)))))

(cl-defgeneric transient--insert-group (group)
  "Format GROUP and its elements and insert the result.")

(cl-defmethod transient--insert-group :before ((group transient-group))
  "Insert GROUP's description, if any."
  (when-let ((desc (transient-format-description group)))
    (insert desc ?\n)))

(cl-defmethod transient--insert-group ((group transient-row))
  (transient--maybe-pad-keys group)
  (dolist (suffix (oref group suffixes))
    (insert (transient-format suffix))
    (insert "   "))
  (insert ?\n))

(cl-defmethod transient--insert-group ((group transient-column))
  (transient--maybe-pad-keys group)
  (dolist (suffix (oref group suffixes))
    (let ((str (transient-format suffix)))
      (insert str)
      (unless (string-match-p ".\n\\'" str)
        (insert ?\n)))))

(cl-defmethod transient--insert-group ((group transient-columns))
  (let* ((columns
          (mapcar
           (lambda (column)
             (transient--maybe-pad-keys column group)
             (let ((rows (mapcar #'transient-format (oref column suffixes))))
               (when-let ((desc (transient-format-description column)))
                 (push desc rows))
               rows))
           (oref group suffixes)))
         (vp (oref transient--prefix variable-pitch))
         (rs (apply #'max (mapcar #'length columns)))
         (cs (length columns))
         (cw (mapcar (lambda (col)
                       (apply #'max
                              (mapcar (if vp #'transient--pixel-width #'length)
                                      col)))
                     columns))
         (cc (transient--seq-reductions-from
              (apply-partially #'+ (* 3 (if vp (transient--pixel-width " ") 1)))
              cw 0)))
    (if transient-force-single-column
        (dotimes (c cs)
          (dotimes (r rs)
            (when-let ((cell (nth r (nth c columns))))
              (unless (equal cell "")
                (insert cell ?\n))))
          (unless (= c (1- cs))
            (insert ?\n)))
      (dotimes (r rs)
        (dotimes (c cs)
          (if vp
              (progn
                (when-let ((cell (nth r (nth c columns))))
                  (insert cell))
                (if (= c (1- cs))
                    (insert ?\n)
                  (insert (propertize " " 'display
                                      `(space :align-to (,(nth (1+ c) cc)))))))
            (insert (make-string (- (nth c cc) (current-column)) ?\s))
            (when-let ((cell (nth r (nth c columns))))
              (insert cell))
            (when (= c (1- cs))
              (insert ?\n))))))))

(defun transient--pixel-width (string)
  (save-window-excursion
    (with-temp-buffer
      (insert string)
      (set-window-dedicated-p nil nil)
      (set-window-buffer nil (current-buffer))
      (car (window-text-pixel-size
            nil (line-beginning-position) (point))))))

(cl-defmethod transient--insert-group ((group transient-subgroups))
  (let* ((subgroups (oref group suffixes))
         (n (length subgroups)))
    (dotimes (s n)
      (let ((subgroup (nth s subgroups)))
        (transient--maybe-pad-keys subgroup group)
        (transient--insert-group subgroup)
        (when (< s (1- n))
          (insert ?\n))))))

(cl-defgeneric transient-format (obj)
  "Format and return OBJ for display.

When this function is called, then the current buffer is some
temporary buffer.  If you need the buffer from which the prefix
command was invoked to be current, then do so by temporarily
making `transient--original-buffer' current.")

(cl-defmethod transient-format ((arg string))
  "Return the string ARG after applying the `transient-heading' face."
  (propertize arg 'face 'transient-heading))

(cl-defmethod transient-format ((_   null))
  "Return a string containing just the newline character."
  "\n")

(cl-defmethod transient-format ((arg integer))
  "Return a string containing just the ARG character."
  (char-to-string arg))

(cl-defmethod transient-format :around ((obj transient-infix))
  "When reading user input for this infix, then highlight it."
  (let ((str (cl-call-next-method obj)))
    (when (eq obj transient--active-infix)
      (setq str (concat str "\n"))
      (add-face-text-property
       (if (eq this-command 'transient-set-level) 3 0)
       (length str)
       'transient-active-infix nil str))
    str))

(cl-defmethod transient-format :around ((obj transient-suffix))
  "When edit-mode is enabled, then prepend the level information.
Optional support for popup buttons is also implemented here."
  (let ((str (concat
              (and transient--editp
                   (let ((level (oref obj level)))
                     (propertize (format " %s " level)
                                 'face (if (transient--use-level-p level t)
                                           'transient-enabled-suffix
                                         'transient-disabled-suffix))))
              (cl-call-next-method obj))))
    (when (oref obj inapt)
      (add-face-text-property 0 (length str) 'transient-inapt-suffix nil str))
    (if transient-enable-popup-navigation
        (make-text-button str nil
                          'type 'transient-button
                          'command (transient--suffix-command obj))
      str)))

(cl-defmethod transient-format ((obj transient-infix))
  "Return a string generated using OBJ's `format'.
%k is formatted using `transient-format-key'.
%d is formatted using `transient-format-description'.
%v is formatted using `transient-format-value'."
  (format-spec (oref obj format)
               `((?k . ,(transient-format-key obj))
                 (?d . ,(transient-format-description obj))
                 (?v . ,(transient-format-value obj)))))

(cl-defmethod transient-format ((obj transient-suffix))
  "Return a string generated using OBJ's `format'.
%k is formatted using `transient-format-key'.
%d is formatted using `transient-format-description'."
  (format-spec (oref obj format)
               `((?k . ,(transient-format-key obj))
                 (?d . ,(transient-format-description obj)))))

(cl-defgeneric transient-format-key (obj)
  "Format OBJ's `key' for display and return the result.")

(cl-defmethod transient-format-key ((obj transient-suffix))
  "Format OBJ's `key' for display and return the result."
  (let ((key (oref obj key))
        (cmd (oref obj command)))
    (if transient--redisplay-key
        (let ((len (length transient--redisplay-key))
              (seq (cl-coerce (edmacro-parse-keys key t) 'list)))
          (cond
           ((equal (seq-take seq len) transient--redisplay-key)
            (let ((pre (key-description (vconcat (seq-take seq len))))
                  (suf (key-description (vconcat (seq-drop seq len)))))
              (setq pre (replace-regexp-in-string "RET" "C-m" pre t))
              (setq pre (replace-regexp-in-string "TAB" "C-i" pre t))
              (setq suf (replace-regexp-in-string "RET" "C-m" suf t))
              (setq suf (replace-regexp-in-string "TAB" "C-i" suf t))
              ;; We use e.g. "-k" instead of the more correct "- k",
              ;; because the former is prettier.  If we did that in
              ;; the definition, then we want to drop the space that
              ;; is reinserted above.  False-positives are possible
              ;; for silly bindings like "-C-c C-c".
              (unless (string-match-p " " key)
                (setq pre (replace-regexp-in-string " " "" pre))
                (setq suf (replace-regexp-in-string " " "" suf)))
              (concat (propertize pre 'face 'default)
                      (and (string-prefix-p (concat pre " ") key) " ")
                      (transient--colorize-key suf cmd)
                      (save-excursion
                        (when (string-match " +\\'" key)
                          (match-string 0 key))))))
           ((transient--lookup-key transient-sticky-map (kbd key))
            (transient--colorize-key key cmd))
           (t
            (propertize key 'face 'transient-unreachable-key))))
      (transient--colorize-key key cmd))))

(defun transient--colorize-key (key command)
  (propertize key 'face
              (or (and (transient--semantic-coloring-p)
                       (transient--suffix-color command))
                  'transient-key)))

(cl-defmethod transient-format-key :around ((obj transient-argument))
  (let ((key (cl-call-next-method obj)))
    (cond ((not transient-highlight-mismatched-keys))
          ((not (slot-boundp obj 'shortarg))
           (add-face-text-property
            0 (length key) 'transient-nonstandard-key nil key))
          ((not (string-equal key (oref obj shortarg)))
           (add-face-text-property
            0 (length key) 'transient-mismatched-key nil key)))
    key))

(cl-defgeneric transient-format-description (obj)
  "Format OBJ's `description' for display and return the result.")

(cl-defmethod transient-format-description ((obj transient-child))
  "The `description' slot may be a function, in which case that is
called inside the correct buffer (see `transient-insert-group')
and its value is returned to the caller."
  (when-let ((desc (oref obj description)))
    (if (functionp desc)
        (with-current-buffer transient--original-buffer
          (funcall desc))
      desc)))

(cl-defmethod transient-format-description ((obj transient-group))
  "Format the description by calling the next method.  If the result
doesn't use the `face' property at all, then apply the face
`transient-heading' to the complete string."
  (when-let ((desc (cl-call-next-method obj)))
    (if (text-property-not-all 0 (length desc) 'face nil desc)
        desc
      (propertize desc 'face 'transient-heading))))

(cl-defmethod transient-format-description :around ((obj transient-suffix))
  "Format the description by calling the next method.  If the result
is nil, then use \"(BUG: no description)\" as the description.
If the OBJ's `key' is currently unreachable, then apply the face
`transient-unreachable' to the complete string."
  (let ((desc (or (cl-call-next-method obj)
                  (and (slot-boundp transient--prefix 'suffix-description)
                       (funcall (oref transient--prefix suffix-description)
                                obj))
                  (propertize "(BUG: no description)" 'face 'error))))
    (cond ((transient--key-unreachable-p obj)
           (propertize desc 'face 'transient-unreachable))
          ((and transient-highlight-higher-levels
                (> (oref obj level) transient--default-prefix-level))
           (add-face-text-property
            0 (length desc) 'transient-higher-level nil desc)
           desc)
          (t
           desc))))

(cl-defgeneric transient-format-value (obj)
  "Format OBJ's value for display and return the result.")

(cl-defmethod transient-format-value ((obj transient-suffix))
  (propertize (oref obj argument)
              'face (if (oref obj value)
                        'transient-argument
                      'transient-inactive-argument)))

(cl-defmethod transient-format-value ((obj transient-option))
  (let ((argument (oref obj argument)))
    (if-let ((value (oref obj value)))
        (propertize
         (cl-ecase (oref obj multi-value)
           ((nil)    (concat argument value))
           ((t rest) (concat argument
                             (and (not (string-suffix-p " " argument)) " ")
                             (mapconcat #'prin1-to-string value " ")))
           (repeat   (mapconcat (lambda (v) (concat argument v)) value " ")))
         'face 'transient-value)
      (propertize argument 'face 'transient-inactive-value))))

(cl-defmethod transient-format-value ((obj transient-switches))
  (with-slots (value argument-format choices) obj
    (format (propertize argument-format
                        'face (if value
                                  'transient-value
                                'transient-inactive-value))
            (concat
             (propertize "[" 'face 'transient-inactive-value)
             (mapconcat
              (lambda (choice)
                (propertize choice 'face
                            (if (equal (format argument-format choice) value)
                                'transient-value
                              'transient-inactive-value)))
              choices
              (propertize "|" 'face 'transient-inactive-value))
             (propertize "]" 'face 'transient-inactive-value)))))

(defun transient--key-unreachable-p (obj)
  (and transient--redisplay-key
       (let ((key (oref obj key)))
         (not (or (equal (seq-take (cl-coerce (edmacro-parse-keys key t) 'list)
                                   (length transient--redisplay-key))
                         transient--redisplay-key)
                  (transient--lookup-key transient-sticky-map (kbd key)))))))

(defun transient--lookup-key (keymap key)
  (let ((val (lookup-key keymap key)))
    (and val (not (integerp val)) val)))

(defun transient--maybe-pad-keys (group &optional parent)
  (when-let ((pad (if (slot-boundp group 'pad-keys)
                      (oref group pad-keys)
                    (and parent
                         (slot-boundp parent 'pad-keys)
                         (oref parent pad-keys)))))
    (let ((width (apply #'max
                        (cons (if (integerp pad) pad 0)
                              (mapcar (lambda (suffix)
                                        (length (oref suffix key)))
                                      (oref group suffixes))))))
      (dolist (suffix (oref group suffixes))
        (oset suffix key
              (truncate-string-to-width (oref suffix key) width nil ?\s))))))

(defun transient-command-summary-or-name (obj)
  "Return the summary or name of the command represented by OBJ.

If the command has a doc-string, then return the first line of
that, else its name.

Intended to be temporarily used as the `:suffix-description' of
a prefix command, while porting a regular keymap to a transient."
  (let ((command (transient--suffix-symbol (oref obj command))))
    (if-let ((doc (documentation command)))
        (propertize (car (split-string doc "\n")) 'face 'font-lock-doc-face)
      (propertize (symbol-name command) 'face 'font-lock-function-name-face))))

;;; Help

(cl-defgeneric transient-show-help (obj)
  "Show documentation for the command represented by OBJ.")

(cl-defmethod transient-show-help ((obj transient-prefix))
  "Call `show-help' if non-nil, else show `info-manual',
if non-nil, else show the `man-page' if non-nil, else use
`describe-function'."
  (with-slots (show-help info-manual man-page command) obj
    (cond (show-help (funcall show-help obj))
          (info-manual (transient--show-manual info-manual))
          (man-page (transient--show-manpage man-page))
          (t (transient--describe-function command)))))

(cl-defmethod transient-show-help ((obj transient-suffix))
  "Call `show-help' if non-nil, else use `describe-function'.
Also used to dispatch showing documentation for the current
prefix.  If the suffix is a sub-prefix, then also call the
prefix method."
  (cond
   ((eq this-command 'transient-help)
    (transient-show-help transient--prefix))
   ((let ((prefix (get (transient--suffix-command obj)
                       'transient--prefix)))
      (and prefix (not (eq (oref transient--prefix command) this-command))
           (prog1 t (transient-show-help prefix)))))
   (t (if-let ((show-help (oref obj show-help)))
          (funcall show-help obj)
        (transient--describe-function this-command)))))

(cl-defmethod transient-show-help ((obj transient-infix))
  "Call `show-help' if non-nil, else show the `man-page'
if non-nil, else use `describe-function'.  When showing the
manpage, then try to jump to the correct location."
  (if-let ((show-help (oref obj show-help)))
      (funcall show-help obj)
    (if-let ((man-page (oref transient--prefix man-page))
             (argument (and (slot-boundp obj 'argument)
                            (oref obj argument))))
        (transient--show-manpage man-page argument)
      (transient--describe-function this-command))))

;; `cl-generic-generalizers' doesn't support `command' et al.
(cl-defmethod transient-show-help (cmd)
  "Show the command doc-string."
  (transient--describe-function cmd))

(defun transient--describe-function (fn)
  (describe-function fn)
  (select-window (get-buffer-window (help-buffer))))

(defun transient--show-manual (manual)
  (info manual))

(defun transient--show-manpage (manpage &optional argument)
  (require 'man)
  (let* ((Man-notify-method 'meek)
         (buf (Man-getpage-in-background manpage))
         (proc (get-buffer-process buf)))
    (while (and proc (eq (process-status proc) 'run))
      (accept-process-output proc))
    (switch-to-buffer buf)
    (when argument
      (transient--goto-argument-description argument))))

(defun transient--goto-argument-description (arg)
  (goto-char (point-min))
  (let ((case-fold-search nil)
        ;; This matches preceding/proceeding options.  Options
        ;; such as "-a", "-S[<keyid>]", and "--grep=<pattern>"
        ;; are matched by this regex without the shy group.
        ;; The ". " in the shy group is for options such as
        ;; "-m parent-number", and the "-[^[:space:]]+ " is
        ;; for options such as "--mainline parent-number"
        (others "-\\(?:. \\|-[^[:space:]]+ \\)?[^[:space:]]+"))
    (when (re-search-forward
           (if (equal arg "--")
               ;; Special case.
               "^[\t\s]+\\(--\\(?: \\|$\\)\\|\\[--\\]\\)"
             ;; Should start with whitespace and may have
             ;; any number of options before and/or after.
             (format
              "^[\t\s]+\\(?:%s, \\)*?\\(?1:%s\\)%s\\(?:, %s\\)*$"
              others
              ;; Options don't necessarily end in an "="
              ;; (e.g., "--gpg-sign[=<keyid>]")
              (string-remove-suffix "=" arg)
              ;; Simple options don't end in an "=".  Splitting this
              ;; into 2 cases should make getting false positives
              ;; less likely.
              (if (string-suffix-p "=" arg)
                  ;; "[^[:space:]]*[^.[:space:]]" matches the option
                  ;; value, which is usually after the option name
                  ;; and either '=' or '[='.  The value can't end in
                  ;; a period, as that means it's being used at the
                  ;; end of a sentence.  The space is for options
                  ;; such as '--mainline parent-number'.
                  "\\(?: \\|\\[?=\\)[^[:space:]]*[^.[:space:]]"
                ;; Either this doesn't match anything (e.g., "-a"),
                ;; or the option is followed by a value delimited
                ;; by a "[", "<", or ":".  A space might appear
                ;; before this value, as in "-f <file>".  The
                ;; space alternative is for options such as
                ;; "-m parent-number".
                "\\(?:\\(?: \\| ?[\\[<:]\\)[^[:space:]]*[^.[:space:]]\\)?")
              others))
           nil t)
      (goto-char (match-beginning 1)))))

(defun transient--insert-help ()
  (unless (looking-back "\n\n" 2)
    (insert "\n"))
  (when transient--helpp
    (insert
     (format (propertize "\
Type a %s to show help for that suffix command, or %s to show manual.
Type %s to exit help.\n"
                         'face 'transient-heading)
             (propertize "<KEY>" 'face 'transient-key)
             (propertize "?"     'face 'transient-key)
             (propertize "C-g"   'face 'transient-key))))
  (when transient--editp
    (unless transient--helpp
      (insert
       (format (propertize "\
Type a %s to set level for that suffix command.
Type %s to set what levels are available for this prefix command.\n"
                           'face 'transient-heading)
               (propertize "<KEY>"   'face 'transient-key)
               (propertize "C-x l" 'face 'transient-key))))
    (with-slots (level) transient--prefix
      (insert
       (format (propertize "
Suffixes on levels %s are available.
Suffixes on levels %s and %s are unavailable.\n"
                           'face 'transient-heading)
               (propertize (format "1-%s" level)
                           'face 'transient-enabled-suffix)
               (propertize " 0 "
                           'face 'transient-disabled-suffix)
               (propertize (format ">=%s" (1+ level))
                           'face 'transient-disabled-suffix))))))

(defvar transient-resume-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap Man-quit]    #'transient-resume)
    (define-key map [remap Info-exit]   #'transient-resume)
    (define-key map [remap quit-window] #'transient-resume)
    map)
  "Keymap for `transient-resume-mode'.

This keymap remaps every command that would usually just quit the
documentation buffer to `transient-resume', which additionally
resumes the suspended transient.")

(define-minor-mode transient-resume-mode
  "Auxiliary minor-mode used to resume a transient after viewing help.")

;;; Compatibility
;;;; Popup Navigation

(defun transient-popup-navigation-help ()
  "Inform the user how to enable popup navigation commands."
  (interactive)
  (message "This command is only available if `%s' is non-nil"
           'transient-enable-popup-navigation))

(define-button-type 'transient-button
  'face nil
  'action (lambda (button)
            (let ((command (button-get button 'command)))
              ;; Yes, I know that this is wrong(tm).
              ;; Unfortunately it is also necessary.
              (setq this-original-command command)
              (transient--pre-command)
              (call-interactively command))))

(defvar transient-popup-navigation-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<down-mouse-1>") #'transient-noop)
    (define-key map (kbd "<mouse-1>") #'transient-mouse-push-button)
    (define-key map (kbd "RET")       #'transient-push-button)
    (define-key map (kbd "<up>")      #'transient-backward-button)
    (define-key map (kbd "C-p")       #'transient-backward-button)
    (define-key map (kbd "<down>")    #'transient-forward-button)
    (define-key map (kbd "C-n")       #'transient-forward-button)
    (define-key map (kbd "C-r")       #'transient-isearch-backward)
    (define-key map (kbd "C-s")       #'transient-isearch-forward)
    map))

(defun transient-mouse-push-button (&optional pos)
  "Invoke the suffix the user clicks on."
  (interactive (list last-command-event))
  (push-button pos))

(defun transient-push-button ()
  "Invoke the selected suffix command."
  (interactive)
  (with-selected-window transient--window
    (push-button)))

(defun transient-backward-button (n)
  "Move to the previous button in the transient popup buffer.
See `backward-button' for information about N."
  (interactive "p")
  (with-selected-window transient--window
    (backward-button n t)))

(defun transient-forward-button (n)
  "Move to the next button in the transient popup buffer.
See `forward-button' for information about N."
  (interactive "p")
  (with-selected-window transient--window
    (forward-button n t)))

(defun transient--goto-button (command)
  (cond
   ((stringp command)
    (when (re-search-forward (concat "^" (regexp-quote command)) nil t)
      (goto-char (match-beginning 0))))
   (command
    (while (and (ignore-errors (forward-button 1))
                (not (eq (button-get (button-at (point)) 'command) command))))
    (unless (eq (button-get (button-at (point)) 'command) command)
      (goto-char (point-min))
      (forward-button 1)))))

(defun transient--heading-at-point ()
  (and (eq (get-text-property (point) 'face) 'transient-heading)
       (let ((beg (line-beginning-position)))
         (buffer-substring-no-properties
          beg (next-single-property-change
               beg 'face nil (line-end-position))))))

;;;; Popup Isearch

(defvar transient--isearch-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map isearch-mode-map)
    (define-key map [remap isearch-exit]   #'transient-isearch-exit)
    (define-key map [remap isearch-cancel] #'transient-isearch-cancel)
    (define-key map [remap isearch-abort]  #'transient-isearch-abort)
    map))

(defun transient-isearch-backward (&optional regexp-p)
  "Do incremental search backward.
With a prefix argument, do an incremental regular expression
search instead."
  (interactive "P")
  (transient--isearch-setup)
  (let ((isearch-mode-map transient--isearch-mode-map))
    (isearch-mode nil regexp-p)))

(defun transient-isearch-forward (&optional regexp-p)
  "Do incremental search forward.
With a prefix argument, do an incremental regular expression
search instead."
  (interactive "P")
  (transient--isearch-setup)
  (let ((isearch-mode-map transient--isearch-mode-map))
    (isearch-mode t regexp-p)))

(defun transient-isearch-exit ()
  "Like `isearch-exit' but adapted for `transient'."
  (interactive)
  (isearch-exit)
  (transient--isearch-exit))

(defun transient-isearch-cancel ()
  "Like `isearch-cancel' but adapted for `transient'."
  (interactive)
  (condition-case nil (isearch-cancel) (quit))
  (transient--isearch-exit))

(defun transient-isearch-abort ()
  "Like `isearch-abort' but adapted for `transient'."
  (interactive)
  (condition-case nil (isearch-abort) (quit))
  (transient--isearch-exit))

(defun transient--isearch-setup ()
  (select-window transient--window)
  (transient--suspend-override))

(defun transient--isearch-exit ()
  (select-window transient--original-window)
  (transient--resume-override))

;;;; Hydra Color Emulation

(defun transient--semantic-coloring-p ()
  (and transient-semantic-coloring
       (not transient--helpp)
       (not transient--editp)))

(defun transient--suffix-color (command)
  (or (get command 'transient-color)
      (get (transient--get-predicate-for command) 'transient-color)))

(defun transient--prefix-color (command)
  (let* ((nonsuf (or (oref command transient-non-suffix)
                     'transient--do-warn))
         (nonsuf (if (memq nonsuf '(transient--do-noop transient--do-warn))
                     'disallow
                   (get nonsuf 'transient-color)))
         (suffix (if-let ((pred (oref command transient-suffix)))
                     (get pred 'transient-color)
                   (if (eq nonsuf 'transient-red)
                       'transient-red
                     'transient-blue))))
    (pcase (list suffix nonsuf)
      (`(transient-red  disallow)       'transient-amaranth)
      (`(transient-blue disallow)       'transient-teal)
      (`(transient-red  transient-red)  'transient-pink)
      (`(transient-red  transient-blue) 'transient-red)
      (`(transient-blue transient-blue) 'transient-blue))))

;;;; Edebug

(defun transient--edebug--recursive-edit (fn arg-mode)
  (transient--debug 'edebug--recursive-edit)
  (if (not transient--prefix)
      (funcall fn arg-mode)
    (transient--suspend-override t)
    (funcall fn arg-mode)
    (transient--resume-override t)))

(advice-add 'edebug--recursive-edit :around #'transient--edebug--recursive-edit)

(defun transient--abort-edebug ()
  (when (bound-and-true-p edebug-active)
    (transient--emergency-exit)))

(advice-add 'abort-recursive-edit :before #'transient--abort-edebug)
(advice-add 'top-level :before #'transient--abort-edebug)

(defun transient--edebug-command-p ()
  (and (bound-and-true-p edebug-active)
       (or (memq this-command '(top-level abort-recursive-edit))
           (string-prefix-p "edebug" (symbol-name this-command)))))

;;;; Miscellaneous

(declare-function which-key-mode "which-key" (&optional arg))

(defun transient--suspend-which-key-mode ()
  (when (bound-and-true-p which-key-mode)
    (which-key-mode -1)
    (add-hook 'transient-exit-hook #'transient--resume-which-key-mode)))

(defun transient--resume-which-key-mode ()
  (unless transient--prefix
    (which-key-mode 1)
    (remove-hook 'transient-exit-hook #'transient--resume-which-key-mode)))

(defun transient-bind-q-to-quit ()
  "Modify some keymaps to bind \"q\" to the appropriate quit command.

\"C-g\" is the default binding for such commands now, but Transient's
predecessor Magit-Popup used \"q\" instead.  If you would like to get
that binding back, then call this function in your init file like so:

  (with-eval-after-load \\='transient
    (transient-bind-q-to-quit))

Individual transients may already bind \"q\" to something else
and such a binding would shadow the quit binding.  If that is the
case then \"Q\" is bound to whatever \"q\" would have been bound
to by setting `transient-substitute-key-function' to a function
that does that.  Of course \"Q\" may already be bound to something
else, so that function binds \"M-q\" to that command instead.
Of course \"M-q\" may already be bound to something else, but
we stop there."
  (define-key transient-base-map   "q" #'transient-quit-one)
  (define-key transient-sticky-map "q" #'transient-quit-seq)
  (setq transient-substitute-key-function
        #'transient-rebind-quit-commands))

(defun transient-rebind-quit-commands (obj)
  "See `transient-bind-q-to-quit'."
  (let ((key (oref obj key)))
    (cond ((string-equal key "q") "Q")
          ((string-equal key "Q") "M-q")
          (t key))))

(defun transient--force-fixed-pitch ()
  (require 'face-remap)
  (face-remap-reset-base 'default)
  (face-remap-add-relative 'default 'fixed-pitch))

;;;; Missing from Emacs

(defun transient--seq-reductions-from (function sequence initial-value)
  (let ((acc (list initial-value)))
    (seq-doseq (elt sequence)
      (push (funcall function (car acc) elt) acc))
    (nreverse acc)))

(defun transient-plist-to-alist (plist)
  (let (alist)
    (while plist
      (push (cons (let* ((symbol (pop plist))
                         (name (symbol-name symbol)))
                    (if (eq (aref name 0) ?:)
                        (intern (substring name 1))
                      symbol))
                  (pop plist))
            alist))
    (nreverse alist)))

;;; Font-Lock

(defconst transient-font-lock-keywords
  (eval-when-compile
    `((,(concat "("
                (regexp-opt (list "transient-define-prefix"
                                  "transient-define-infix"
                                  "transient-define-argument"
                                  "transient-define-suffix")
                            t)
                "\\_>[ \t'(]*"
                "\\(\\(?:\\sw\\|\\s_\\)+\\)?")
       (1 'font-lock-keyword-face)
       (2 'font-lock-function-name-face nil t)))))

(font-lock-add-keywords 'emacs-lisp-mode transient-font-lock-keywords)

;;; Auxiliary Classes
;;;; `transient-lisp-variable'

(defclass transient-lisp-variable (transient-variable)
  ((reader :initform #'transient-lisp-variable--reader)
   (always-read :initform t)
   (set-value :initarg :set-value :initform #'set))
  "[Experimental] Class used for Lisp variables.")

(cl-defmethod transient-init-value ((obj transient-lisp-variable))
  (oset obj value (symbol-value (oref obj variable))))

(cl-defmethod transient-infix-set ((obj transient-lisp-variable) value)
  (funcall (oref obj set-value)
           (oref obj variable)
           (oset obj value value)))

(cl-defmethod transient-format-description ((obj transient-lisp-variable))
  (or (oref obj description)
      (symbol-name (oref obj variable))))

(cl-defmethod transient-format-value ((obj transient-lisp-variable))
  (propertize (prin1-to-string (oref obj value))
              'face 'transient-value))

(cl-defmethod transient-prompt ((obj transient-lisp-variable))
  (format "Set %s: " (oref obj variable)))

(defun transient-lisp-variable--reader (prompt initial-input _history)
  (read--expression prompt initial-input))

;;; _
(provide 'transient)
;; Local Variables:
;; indent-tabs-mode: nil
;; checkdoc-symbol-words: ("command-line" "edit-mode" "help-mode")
;; End:
;;; transient.el ends here
