;;; transient.el --- Transient commands  -*- lexical-binding:t -*-

;; Copyright (C) 2018-2026 Free Software Foundation, Inc.

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; URL: https://github.com/magit/transient
;; Keywords: extensions
;; Version: 0.12.0

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is part of GNU Emacs.

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

;;; Commentary:

;; Transient is the library used to implement the keyboard-driven menus
;; in Magit.  It is distributed as a separate package, so that it can be
;; used to implement similar menus in other packages.

;;; Code:
;;;; Frontmatter

(defconst transient-version "v0.12.0-15-gfe5214e6-builtin")

(require 'cl-lib)
(require 'eieio)
(require 'edmacro)
(require 'format-spec)
(require 'pcase)
(require 'pp)
(require 'seq)

(eval-when-compile (require 'subr-x))

(declare-function info "info" (&optional file-or-node buffer))
(declare-function Man-find-section "man" (section))
(declare-function Man-next-section "man" (n))
(declare-function Man-getpage-in-background "man" (topic))

(defvar Man-notify-method)

(defvar transient-common-command-prefix)

(defmacro transient--with-emergency-exit (id &rest body)
  (declare (indent defun))
  (unless (keywordp id)
    (setq body (cons id body))
    (setq id nil))
  `(condition-case err
       (let ((debugger #'transient--exit-and-debug))
         ,(macroexp-progn body))
     ((debug error)
      (transient--emergency-exit ,id)
      (signal (car err) (cdr err)))))

(defun transient--exit-and-debug (&rest args)
  (transient--emergency-exit :debugger)
  (apply #'debug args))

;;;; Options

(defgroup transient nil
  "Transient commands."
  :group 'extensions)

(defcustom transient-show-popup t
  "Whether and when to show transient's menu in a buffer.

\\<transient-map>\
- If t (the default), then show the buffer as soon as a transient
  prefix command is invoked.

- If nil, then do not show the buffer unless the user explicitly
  requests it, by pressing \\[transient-show] or a prefix key.

- If a number, then delay displaying the buffer and instead show
  a brief one-line summary.  If zero or negative, then suppress
  even showing that summary and display the pressed key only.

  Show the buffer once the user explicitly requests it by pressing
  \\[transient-show] or a prefix key.  Unless zero, then also show the buffer
  after that many seconds of inactivity (using the absolute value)."
  :package-version '(transient . "0.1.0")
  :group 'transient
  :type '(choice (const  :tag "Instantly" t)
                 (const  :tag "On demand" nil)
                 (const  :tag "On demand (no summary)" 0)
                 (number :tag "After delay" 1)))

(defcustom transient-enable-popup-navigation 'verbose
  "Whether navigation commands are enabled in the menu buffer.

If the value is `verbose' (the default), additionally show brief
documentation about the command under point in the echo area.

While a transient is active transient's menu buffer is not the
current buffer, making it necessary to use dedicated commands to
act on that buffer itself.  If this is non-nil, then the following
bindings are available:

\\<transient-popup-navigation-map>\
- \\[transient-backward-button] moves the cursor to the previous suffix.
- \\[transient-forward-button] moves the cursor to the next suffix.
- \\[transient-push-button] invokes the suffix the cursor is on.
\\<transient-button-map>\
- \\`<mouse-1>' and \\`<mouse-2>' invoke the clicked on suffix.
\\<transient-popup-navigation-map>\
- \\[transient-isearch-backward]\
 and \\[transient-isearch-forward] start isearch in the menu buffer.

\\`<mouse-1>' and \\`<mouse-2>' are bound in `transient-push-button'.
All other bindings are in `transient-popup-navigation-map'.

By default \\`M-RET' is bound to `transient-push-button', instead of
\\`RET', because if a transient allows the invocation of non-suffixes,
then it is likely, that you would want \\`RET' to do what it would do
if no transient were active."
  :package-version '(transient . "0.7.8")
  :group 'transient
  :type '(choice (const :tag "Enable navigation and echo summary" verbose)
                 (const :tag "Enable navigation commands" t)
                 (const :tag "Disable navigation commands" nil)))

(defcustom transient-display-buffer-action
  '(display-buffer-in-side-window
    (side . bottom)
    (dedicated . t)
    (inhibit-same-window . t))
  "The action used to display transient's menu buffer.

The transient menu buffer is displayed in a window using

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
    (inhibit-same-window . t))

This displays the window at the bottom of the selected frame.
For alternatives see info node `(elisp)Display Action Functions'
and info node `(elisp)Buffer Display Action Alists'.

When you switch to a different ACTION, you should keep the ALIST
entries for `dedicated' and `inhibit-same-window' in most cases.
Do not drop them because you are unsure whether they are needed;
if you are unsure, then keep them.

Note that the buffer that was current before the transient buffer
is shown should remain the current buffer.  Many suffix commands
act on the thing at point, if appropriate, and if the transient
buffer became the current buffer, then that would change what is
at point.  To that effect `inhibit-same-window' ensures that the
selected window is not used to show the transient buffer.

The use of a horizontal split to display the menu window can lead
to incompatibilities and is thus discouraged.  Transient tries to
mitigate such issue but cannot proactively deal with all possible
configurations and combinations of third-party packages.

It may be possible to display the window in another frame, but
whether that works in practice depends on the window-manager.
If the window manager selects the new window (Emacs frame),
then that unfortunately changes which buffer is current.

If you change the value of this option, then you might also
want to change the value of `transient-mode-line-format'."
  :package-version '(transient . "0.7.5")
  :group 'transient
  :type '(cons (choice function (repeat :tag "Functions" function))
               alist))

(defcustom transient-minimal-frame-width 83
  "Minimal width of dedicated frame used to display transient menu.

This is only used if the transient menu is actually displayed in a
dedicated frame (see `transient-display-buffer-action').  The value
is in characters."
  :package-version '(transient . "0.8.1")
  :group 'transient
  :type 'natnum)

(defcustom transient-mode-line-format 'line
  "The mode-line format for transient's menu buffer.

If nil, then the buffer has no mode-line.  If the buffer is not
displayed right above the echo area, then this probably is not
a good value.

If `line' (the default) or a natural number, then the buffer has no
mode-line, but a line is drawn in its place.  If a number is used,
that specifies the thickness of the line.  On termcap frames we
cannot draw lines, so there `line' and numbers are synonyms for nil.

The color of the line is used to indicate if non-suffixes are
allowed and whether they exit the transient.  The foreground
color of `transient-key-noop' (if non-suffixes are disallowed),
`transient-key-stay' (if allowed and transient stays active), or
`transient-key-exit' (if allowed and they exit the transient) is
used to draw the line.

Otherwise this can be any mode-line format.
See `mode-line-format' for details."
  :package-version '(transient . "0.2.0")
  :group 'transient
  :type '(choice (const  :tag "Hide mode-line" nil)
                 (const  :tag "Substitute thin line" line)
                 (number :tag "Substitute line with thickness")
                 (const  :tag "Name of prefix command"
                         ("%e" mode-line-front-space
                          mode-line-buffer-identification))
                 (sexp   :tag "Custom mode-line format")))

(defcustom transient-show-common-commands nil
  "Whether to permanently show common suffix commands in transient menus.

By default these commands are only temporarily shown after typing their
shared prefix key \
\\<transient--docstr-hint-1>\\[transient-common-command-prefix], \
while a transient menu is active.  When the value
of this option is non-nil, then these commands are permanently shown.
To toggle the value for the current Emacs session only type \
\\<transient--docstr-hint-2>\\[transient-toggle-common] while
any transient menu is active."
  :package-version '(transient . "0.1.0")
  :group 'transient
  :type 'boolean)

(defcustom transient-show-during-minibuffer-read nil
  "Whether to show the transient menu while reading in the minibuffer.

This is only relevant to commands that do not close the menu, such as
commands that set infix arguments.  If a command exits the menu, and
uses the minibuffer, then the menu is always closed before the
minibuffer is entered, irrespective of the value of this option.

When nil (the default), hide the menu while the minibuffer is in use.
When t, keep showing the menu, but allow for the menu window to be
resized, to ensure that completion candidates can be displayed.

When `fixed', keep showing the menu and prevent it from being resized,
which may make it impossible to display the completion candidates.  If
that ever happens for you, consider using t or an integer, as described
below.

If the value is `fixed' and the menu window uses the full height of its
frame, then the former is ignored and resizing is allowed anyway.  This
is necessary because individual menus may use unusual display actions
different from what `transient-display-buffer-action' specifies (likely
to display that menu in a side-window).

When using a third-party mode, which automatically resizes windows
\(e.g., by calling `balance-windows' on `post-command-hook'), then
`fixed' (or nil) is likely a better choice than t.

The value can also be an integer, in which case the behavior depends on
whether at least that many lines are left to display windows other than
the menu window.  If that is the case, display the menu and preserve the
size of that window.  Otherwise, allow resizing the menu window if the
number is positive, or hide the menu if it is negative."
  :package-version '(transient . "0.8.0")
  :group 'transient
  :type '(choice
          (const :tag "Hide menu" nil)
          (const :tag "Show menu and preserve size" fixed)
          (const :tag "Show menu and allow resizing" t)
          (natnum :tag "Show menu, allow resizing if less than N lines left"
                  :format "\n   %t: %v"
                  :value 20)
          (integer :tag "Show menu, except if less than N lines left"
                   :format "\n   %t: %v"
                   :value -20)))

(defcustom transient-show-docstring-format "%s"
  "How to display suffix docstrings.

The command `transient-toggle-docstrings' toggles between showing suffix
descriptions as usual, and instead or additionally displaying the suffix
docstrings.  The format specified here controls how that is done.  %c is
the description and %s is the docstring.  Use \"%-14c %s\" or similar to
display both.

This command is not bound by default, see its docstring for instructions."
  :package-version '(transient . "0.8.4")
  :group 'transient
  :type 'string)

(defcustom transient-read-with-initial-input nil
  "Whether to use the last history element as initial minibuffer input."
  :package-version '(transient . "0.2.0")
  :group 'transient
  :type 'boolean)

(defcustom transient-highlight-mismatched-keys nil
  "Whether to highlight keys that do not match their argument.

This is mostly intended for authors of transient menus and disabled by
default.

This only affects infix arguments that represent command-line arguments.
When this option is non-nil, then the key binding for infix argument are
highlighted when only a long argument \(e.g., \"--verbose\") is specified
but no shorthand (e.g., \"-v\"). In the rare case that a short-hand is
specified but does not match the key binding, then it is highlighted
differently.

The highlighting is done using `transient-mismatched-key'
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

(defcustom transient-semantic-coloring t
  "Whether to use colors to indicate transient behavior.

If non-nil, then the key binding of each suffix is colorized to
indicate whether it exits the transient state or not, and the
line that is drawn below transient's menu buffer is used to
indicate the behavior of non-suffix commands."
  :package-version '(transient . "0.5.0")
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

(defcustom transient-error-on-insert-failure nil
  "Whether to signal an error when failing to insert a suffix.

When `transient-insert-suffix' and `transient-append-suffix' fail
to insert a suffix into an existing prefix, they usually just show
a warning.  If this is non-nil, they signal an error instead."
  :package-version '(transient . "0.8.8")
  :group 'transient
  :type 'boolean)

(defcustom transient-align-variable-pitch nil
  "Whether to align columns pixel-wise in the menu buffer.

If this is non-nil, then columns are aligned pixel-wise to
support variable-pitch fonts.  Keys are not aligned, so you
should use a fixed-pitch font for the `transient-key' face.
Other key faces inherit from that face unless a theme is
used that breaks that relationship.

This option is intended for users who use a variable-pitch
font for the `default' face.

See also `transient-force-fixed-pitch'."
  :package-version '(transient . "0.4.0")
  :group 'transient
  :type 'boolean)

(defcustom transient-force-fixed-pitch nil
  "Whether to force use of monospaced font in the menu buffer.

Even if you use a proportional font for the `default' face,
you might still want to use a monospaced font in transient's
menu buffer.  Setting this option to t causes `default' to
be remapped to `fixed-pitch' in that buffer.

See also `transient-align-variable-pitch'."
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

(defconst transient--max-level 7)
(defconst transient--default-child-level 1)
(defconst transient--default-prefix-level 4)

(defcustom transient-default-level transient--default-prefix-level
  "Control what suffix levels are made available by default.

Each suffix command is placed on a level and each prefix command
has a level, which controls which suffix commands are available.
Integers between 1 and 7 (inclusive) are valid levels.

The levels of individual transients and/or their individual
suffixes can be changed individually, by invoking the prefix and
then pressing \\<transient--docstr-hint-2>\\[transient-set-level].

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
  (locate-user-emacs-file "transient/levels.el")
  "File used to save levels of transients and their suffixes."
  :package-version '(transient . "0.1.0")
  :group 'transient
  :type 'file)

(defcustom transient-values-file
  (locate-user-emacs-file "transient/values.el")
  "File used to save values of transients."
  :package-version '(transient . "0.1.0")
  :group 'transient
  :type 'file)

(defcustom transient-history-file
  (locate-user-emacs-file "transient/history.el")
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

;;;; Faces

(defgroup transient-faces nil
  "Faces used by Transient."
  :group 'transient)

(defface transient-heading '((t :inherit font-lock-keyword-face))
  "Face used for headings."
  :group 'transient-faces)

(defface transient-argument '((t :inherit font-lock-string-face :weight bold))
  "Face used for enabled arguments."
  :group 'transient-faces)

(defface transient-inactive-argument '((t :inherit shadow))
  "Face used for inactive arguments."
  :group 'transient-faces)

(defface transient-inapt-argument '((t :inherit shadow :weight bold))
  "Face used for inapt arguments with a (currently ignored) value.
Depending on the type this is used for the argument and/or value."
  :group 'transient-faces)

(defface transient-value '((t :inherit font-lock-string-face :weight bold))
  "Face used for values."
  :group 'transient-faces)

(defface transient-inactive-value '((t :inherit shadow))
  "Face used for inactive values."
  :group 'transient-faces)

(defface transient-unreachable '((t :inherit shadow))
  "Face used for suffixes unreachable from the current prefix sequence."
  :group 'transient-faces)

(defface transient-inapt-suffix '((t :inherit shadow :slant italic))
  "Face used for suffixes that are inapt at this time."
  :group 'transient-faces)

(defface transient-active-infix '((t :inherit highlight))
  "Face used for the infix for which the value is being read."
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

(defface transient-higher-level
  (let* ((color (face-attribute 'shadow :foreground t t))
         (color (if (eq color 'unspecified) "grey60" color)))
    `((t :box (:line-width (-1 . -1) :color ,color))))
  "Face optionally used to highlight suffixes on higher levels.
See also option `transient-highlight-higher-levels'."
  :group 'transient-faces)

(defface transient-delimiter '((t :inherit shadow))
  "Face used for delimiters and separators.
This includes the parentheses around values and the pipe
character used to separate possible values from each other."
  :group 'transient-faces)

(defface transient-key '((t :inherit font-lock-builtin-face))
  "Face used for keys."
  :group 'transient-faces)

(defface transient-key-stay
  `((((class color) (background light))
     :inherit transient-key
     :foreground "#22aa22")
    (((class color) (background dark))
     :inherit transient-key
     :foreground "#ddffdd"))
  "Face used for keys of suffixes that don't exit the menu."
  :group 'transient-faces)

(defface transient-key-noop
  `((((class color) (background light))
     :inherit transient-key
     :foreground "grey80")
    (((class color) (background dark))
     :inherit transient-key
     :foreground "grey30"))
  "Face used for keys of suffixes that currently cannot be invoked."
  :group 'transient-faces)

(defface transient-key-return
  `((((class color) (background light))
     :inherit transient-key
     :foreground "#aaaa11")
    (((class color) (background dark))
     :inherit transient-key
     :foreground "#ffffcc"))
  "Face used for keys of suffixes that return to the parent menu."
  :group 'transient-faces)

(defface transient-key-recurse
  `((((class color) (background light))
     :inherit transient-key
     :foreground "#2266ff")
    (((class color) (background dark))
     :inherit transient-key
     :foreground "#2299ff"))
  "Face used for keys of sub-menus whose suffixes return to the parent menu."
  :group 'transient-faces)

(defface transient-key-stack
  `((((class color) (background light))
     :inherit transient-key
     :foreground "#dd4488")
    (((class color) (background dark))
     :inherit transient-key
     :foreground "#ff6699"))
  "Face used for keys of sub-menus that exit the parent menu."
  :group 'transient-faces)

(defface transient-key-exit
  `((((class color) (background light))
     :inherit transient-key
     :foreground "#aa2222")
    (((class color) (background dark))
     :inherit transient-key
     :foreground "#ffdddd"))
  "Face used for keys of suffixes that exit the menu."
  :group 'transient-faces)

(defface transient-unreachable-key
  '((t :inherit (shadow transient-key) :weight normal))
  "Face used for keys unreachable from the current prefix sequence."
  :group 'transient-faces)

(defface transient-nonstandard-key
  `((t :box (:line-width (-1 . -1) :color "cyan")))
  "Face optionally used to highlight keys conflicting with short-argument.
See also option `transient-highlight-mismatched-keys'."
  :group 'transient-faces)

(defface transient-mismatched-key
  `((t :box (:line-width (-1 . -1) :color "magenta")))
  "Face optionally used to highlight keys without a short-argument.
See also option `transient-highlight-mismatched-keys'."
  :group 'transient-faces)

;;;; Persistence

(defun transient--read-file-contents (file)
  (with-demoted-errors "Transient error: %S"
    (and (file-exists-p file)
         (with-temp-buffer
           (insert-file-contents file)
           (read (current-buffer))))))

(defun transient--pp-to-file (value file)
  (when (or value (file-exists-p file))
    (make-directory (file-name-directory file) t)
    (setq value (cl-sort (copy-sequence value) #'string< :key #'car))
    (with-temp-file file
      (let ((print-level nil)
            (print-length nil)
            (pp-default-function 'pp-28)
            (fill-column 999))
        (pp value (current-buffer))))))

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
    (with-demoted-errors "Error saving transient history: %S"
      (transient-save-history))))

(unless noninteractive
  (add-hook 'kill-emacs-hook #'transient-maybe-save-history))

;;;; Classes
;;;; Prefix

(defclass transient-prefix ()
  ((prototype   :initarg :prototype)
   (command     :initarg :command)
   (level       :initarg :level)
   (init-value  :initarg :init-value)
   (value) (default-value :initarg :value)
   (return      :initarg :return      :initform nil)
   (scope       :initarg :scope       :initform nil)
   (history     :initarg :history     :initform nil)
   (history-pos :initarg :history-pos :initform 0)
   (history-key :initarg :history-key :initform nil)
   (show-help   :initarg :show-help   :initform nil)
   (info-manual :initarg :info-manual :initform nil)
   (man-page    :initarg :man-page    :initform nil)
   (transient-suffix     :initarg :transient-suffix     :initform nil)
   (transient-non-suffix :initarg :transient-non-suffix :initform nil)
   (transient-switch-frame :initarg :transient-switch-frame)
   (refresh-suffixes     :initarg :refresh-suffixes     :initform nil)
   (remember-value       :initarg :remember-value       :initform nil)
   (environment          :initarg :environment          :initform nil)
   (incompatible         :initarg :incompatible         :initform nil)
   (suffix-description   :initarg :suffix-description)
   (display-action       :initarg :display-action       :initform nil)
   (mode-line-format     :initarg :mode-line-format)
   (variable-pitch       :initarg :variable-pitch       :initform nil)
   (column-widths        :initarg :column-widths        :initform nil)
   (unwind-suffix        :documentation "Internal use." :initform nil))
  "Transient prefix command.

Each transient prefix command consists of a command, which is
stored in a symbol's function slot and an object, which is
stored in the `transient--prefix' property of the same symbol.

When a transient prefix command is invoked, then a clone of that
object is stored in the global variable `transient--prefix' and
the prototype is stored in the clone's `prototype' slot.")

;;;; Suffix

(defclass transient-child ()
  ((parent
    :initarg :parent
    :initform nil
    :documentation "The parent group object.")
   (level
    :initarg :level
    :initform nil
    :documentation "Enable if level of prefix is equal or greater.")
   (inactive
    :initform nil)
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
    :documentation "Enable if major-mode does not derive from value.")
   (inapt
    :initform nil)
   (inapt-face
    :initarg :inapt-face
    :initform 'transient-inapt-suffix)
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
    :documentation "Inapt if major-mode does not derive from value.")
   (advice
    :initarg :advice
    :initform nil
    :documentation "Advise applied to the command body.")
   (advice*
    :initarg :advice*
    :initform nil
    :documentation "Advise applied to the command body and interactive spec."))
  "Abstract superclass for group and suffix classes.

It is undefined which predicates are used if more than one `if*'
predicate slots or more than one `inapt-if*' slots are non-nil."
  :abstract t)

(defclass transient-suffix (transient-child)
  ((definition  :allocation :class    :initform nil)
   (key         :initarg :key)
   (command     :initarg :command)
   (transient   :initarg :transient)
   (format      :initarg :format      :initform " %k %d")
   (description :initarg :description :initform nil)
   (face        :initarg :face        :initform nil)
   (show-help   :initarg :show-help   :initform nil)
   (summary     :initarg :summary     :initform nil))
  "Superclass for suffix command.")

(defclass transient-information (transient-suffix)
  ((format :initform " %k %d")
   (key    :initform " "))
  "Display-only information, aligned with suffix keys.
Technically a suffix object with no associated command.")

(defclass transient-information* (transient-information)
  ((format :initform " %d"))
  "Display-only information, aligned with suffix descriptions.
Technically a suffix object with no associated command.")

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

(defclass transient-value-preset (transient-suffix)
  ((transient :initform t)
   (set :initarg := :initform nil))
  "Class used by the `transient-preset' suffix command.")

(defclass transient-describe-target (transient-suffix)
  ((transient :initform #'transient--do-suspend)
   (helper :initarg :helper :initform nil)
   (target :initarg := :initform nil))
  "Class used by the `transient-describe' suffix command.")

;;;; Group

(defclass transient-group (transient-child)
  ((suffixes       :initarg :suffixes       :initform nil)
   (hide           :initarg :hide           :initform nil)
   (description    :initarg :description    :initform nil)
   (pad-keys       :initarg :pad-keys       :initform nil)
   (info-format    :initarg :info-format    :initform nil)
   (setup-children :initarg :setup-children))
  "Abstract superclass of all group classes."
  :abstract t)

(defclass transient-column (transient-group) ()
  "Group class that displays each element on a separate line.")

(defclass transient-row (transient-group) ()
  "Group class that displays all elements on a single line.")

(defclass transient-columns (transient-group) ()
  "Group class that displays elements organized in columns.
Direct elements have to be groups whose elements have to be
commands or strings.  Each subgroup represents a column.
This class takes care of inserting the subgroups' elements.")

(defclass transient-subgroups (transient-group) ()
  "Group class that wraps other groups.

Direct elements have to be groups whose elements have to be
commands or strings.  This group inserts an empty line between
subgroups.  The subgroups are responsible for displaying their
elements themselves.")

;;;; Define

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
how these bindings are presented in the menu buffer.  At least
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
  (declare (debug ( &define name lambda-list
                    [&optional lambda-doc]
                    [&rest keywordp sexp]
                    [&rest vectorp]
                    [&optional ("interactive" interactive) def-body]))
           (indent defun)
           (doc-string 3))
  (pcase-let
      ((`(,class ,slots ,groups ,docstr ,body ,interactive-only)
        (transient--expand-define-args args arglist 'transient-define-prefix)))
    `(progn
       (defalias ',name
         ,(if body
              `(lambda ,arglist ,@body)
            `(lambda ()
               (interactive)
               (transient-setup ',name))))
       (put ',name 'interactive-only ,interactive-only)
       (put ',name 'function-documentation ,docstr)
       (put ',name 'transient--prefix
            (,(or class 'transient-prefix) :command ',name ,@slots))
       (transient--set-layout
        ',name
        (list ,@(mapcan (lambda (s) (transient--parse-child name s)) groups))))))

(defmacro transient-define-group (name &rest groups)
  "Define one or more groups and store them in symbol NAME.

Groups defined using this macro, can be used inside the
definition of transient prefix commands, by using the symbol
NAME where a group vector is expected.  GROUPS has the same
form as for `transient-define-prefix'."
  (declare (debug (&define name [&rest vectorp]))
           (indent defun))
  `(transient--set-layout
    ',name
    (list ,@(mapcan (lambda (s) (transient--parse-child name s)) groups))))

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

\(fn NAME ARGLIST [DOCSTRING] [KEYWORD VALUE]... [BODY...])"
  (declare (debug ( &define name lambda-list
                    [&optional lambda-doc]
                    [&rest keywordp sexp]
                    [&optional ("interactive" interactive) def-body]))
           (indent defun)
           (doc-string 3))
  (pcase-let
      ((`(,class ,slots ,_ ,docstr ,body ,interactive-only)
        (transient--expand-define-args args arglist 'transient-define-suffix)))
    `(progn
       (defalias ',name
         ,(if (and (not body) class (oref-default class definition))
              `(oref-default ',class definition)
            `(lambda ,arglist ,@body)))
       (put ',name 'interactive-only ,interactive-only)
       (put ',name 'function-documentation ,docstr)
       (put ',name 'transient--suffix
            (,(or class 'transient-suffix) :command ',name ,@slots)))))

(defmacro transient-augment-suffix (name &rest args)
  "Augment existing command NAME with a new transient suffix object.
Similar to `transient-define-suffix' but define a suffix object only.
\n\(fn NAME [KEYWORD VALUE]...)"
  (declare (debug (&define name [&rest keywordp sexp]))
           (indent defun))
  (pcase-let
      ((`(,class ,slots)
        (transient--expand-define-args args nil 'transient-augment-suffix t)))
    `(put ',name 'transient--suffix
          (,(or class 'transient-suffix) :command ',name ,@slots))))

(defmacro transient-define-infix (name arglist &rest args)
  "Define NAME as a transient infix command.

ARGLIST is always ignored and reserved for future use.
DOCSTRING is the documentation string and is optional.

At least one key-value pair is required.  All transient infix
commands are equal to each other (but not eq).  It is meaning-
less to define an infix command, without providing at least one
keyword argument (usually `:argument' or `:variable', depending
on the class).  The suffix class defaults to `transient-switch'
and can be set using the `:class' keyword.

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
that case you have to use `transient-define-suffix' to define
the infix command and use t as the value of the `:transient'
keyword.

\(fn NAME ARGLIST [DOCSTRING] KEYWORD VALUE [KEYWORD VALUE]...)"
  (declare (debug ( &define name lambda-list
                    [&optional lambda-doc]
                    keywordp sexp
                    [&rest keywordp sexp]))
           (indent defun)
           (doc-string 3))
  (pcase-let
      ((`(,class ,slots ,_ ,docstr ,_ ,interactive-only)
        (transient--expand-define-args args arglist 'transient-define-infix t)))
    `(progn
       (defalias ',name #'transient--default-infix-command)
       (put ',name 'interactive-only ,interactive-only)
       (put ',name 'completion-predicate #'transient--suffix-only)
       (put ',name 'function-documentation ,docstr)
       (put ',name 'transient--suffix
            (,(or class 'transient-switch) :command ',name ,@slots)))))

(defalias 'transient-define-argument #'transient-define-infix
  "Define NAME as a transient infix command.

Only use this alias to define an infix command that actually
sets an infix argument.  To define a infix command that, for
example, sets a variable, use `transient-define-infix' instead.

\(fn NAME ARGLIST [DOCSTRING] [KEYWORD VALUE]...)")

(defun transient--default-infix-command ()
  ;; Most infix commands are but an alias for this command.
  "Cannot show any documentation for this transient infix command.

When you request help for an infix command using `transient-help', that
usually shows the respective man-page and tries to jump to the location
where the respective argument is being described.

If no man-page is specified for the containing transient menu, then the
docstring is displayed instead, if any.

If the infix command doesn't have a docstring, as is the case here, then
this docstring is displayed instead, because technically infix commands
are aliases for `transient--default-infix-command'.

`describe-function' also shows the docstring of the infix command,
falling back to that of the same aliased command."
  (interactive)
  (let ((obj (transient-suffix-object)))
    (transient-infix-set obj (transient-infix-read obj)))
  (transient--show))
(put 'transient--default-infix-command 'interactive-only t)
(put 'transient--default-infix-command 'completion-predicate
     #'transient--suffix-only)

(define-advice find-function-advised-original
    (:around (fn func) transient-default-infix)
  "Return nil instead of `transient--default-infix-command'.
When using `find-function' to jump to the definition of a transient
infix command/argument, then we want to actually jump to that, not to
the definition of `transient--default-infix-command', which all infix
commands are aliases for."
  (let ((val (funcall fn func)))
    (and val (not (eq val 'transient--default-infix-command)) val)))

(eval-and-compile ;transient--expand-define-args
  (defun transient--expand-define-args (args &optional arglist form nobody)
    ;; ARGLIST and FORM are only optional for backward compatibility.
    ;; This is necessary because "emoji.el" from Emacs 29 calls this
    ;; function directly, with just one argument.
    (declare (advertised-calling-convention
              (args arglist form &optional nobody) "0.7.1"))
    (unless (listp arglist)
      (error "Mandatory ARGLIST is missing"))
    (let (class keys suffixes docstr declare (interactive-only t))
      (when (stringp (car args))
        (setq docstr (pop args)))
      (while (keywordp (car args))
        (let ((k (pop args))
              (v (pop args)))
          (if (eq k :class)
              (setq class v)
            (push k keys)
            (push v keys))))
      (while-let
          ((arg (car args))
           (arg (cond
                  ;; Inline group definition.
                  ((vectorp arg)
                   (pop args))
                  ;; Quoted include, as one would expect.
                  ((eq (car-safe arg) 'quote)
                   (cadr (pop args)))
                  ;; Unquoted include, for compatibility.
                  ((and arg (symbolp arg))
                   (pop args)))))
        (push arg suffixes))
      (when (eq (car-safe (car args)) 'declare)
        (setq declare (car args))
        (setq args (cdr args))
        (when-let* ((int (assq 'interactive-only declare)))
          (setq interactive-only (cadr int))
          (delq int declare))
        (unless (cdr declare)
          (setq declare nil)))
      (cond
        ((not args))
        (nobody
         (error "%s: No function body allowed" form))
        ((not (eq (car-safe (nth (if declare 1 0) args)) 'interactive))
         (error "%s: Interactive form missing" form)))
      (list (if (eq (car-safe class) 'quote)
                (cadr class)
              class)
            (nreverse keys)
            (nreverse suffixes)
            docstr
            (if declare (cons declare args) args)
            interactive-only))))

(defun transient--parse-child (prefix spec)
  (cl-typecase spec
    (null    (error "Invalid transient--parse-child spec: %s" spec))
    (symbol  (list `',spec))
    (vector  (and-let* ((c (transient--parse-group  prefix spec))) (list c)))
    (list    (and-let* ((c (transient--parse-suffix prefix spec))) (list c)))
    (string  (list spec))
    (t       (error "Invalid transient--parse-child spec: %s" spec))))

(defun transient--parse-group (prefix spec)
  (let (class args)
    (setq spec (append spec nil))
    (when (integerp (car spec))
      (setq args (plist-put args :level (pop spec))))
    (when (stringp (car spec))
      (setq args (plist-put args :description (pop spec))))
    (while (keywordp (car spec))
      (let* ((key (pop spec))
             (val (if spec (pop spec) (error "No value for `%s'" key))))
        (cond ((eq key :class)
               (setq class val))
              ((or (symbolp val)
                   (and (listp val)
                        (not (memq (car val) (list 'lambda (intern ""))))))
               (setq args (plist-put args key (macroexp-quote val))))
              ((setq args (plist-put args key val))))))
    (unless (or spec class (not (plist-get args :setup-children)))
      (message "WARNING: %s: When %s is used, %s must also be specified"
               'transient-define-prefix :setup-children :class))
    (list 'vector
          (list 'quote
                (cond (class)
                      ((cl-typep (car spec)
                                 '(or vector (and symbol (not null))))
                       'transient-columns)
                      ('transient-column)))
          (and args (cons 'list args))
          (cons 'list
                (mapcan (lambda (s) (transient--parse-child prefix s)) spec)))))

(defun transient--parse-suffix (prefix spec)
  (let (class args)
    (cl-flet ((use (prop value)
                (setq args (plist-put args prop value))))
      (pcase (car spec)
        ((cl-type integer)
         (use :level (pop spec))))
      (pcase (car spec)
        ((cl-type (or string vector))
         (use :key (pop spec))))
      (pcase (car spec)
        ((guard (or (stringp (car spec))
                    (and (eq (car-safe (car spec)) 'lambda)
                         (not (commandp (car spec))))))
         (use :description (pop spec)))
        ((and (cl-type (and symbol (not keyword) (not command)))
              (guard (commandp (cadr spec))))
         (use :description (macroexp-quote (pop spec)))))
      (pcase (car spec)
        ((or :info :info* :cons))
        ((and (cl-type keyword) invalid)
         (error "Need command, argument, `:info', `:info*' or `:cons'; got `%s'"
                invalid))
        ((cl-type symbol)
         (use :command (macroexp-quote (pop spec))))
        ;; During macro-expansion this is expected to be a `lambda'
        ;; expression (i.e., source code).  When this is called from a
        ;; `:setup-children' function, it may also be a function object
        ;; (a.k.a a function value).  However, we never treat a string
        ;; as a command, so we have to check for that explicitly.
        ((cl-type (and command (not string)))
         (let ((cmd (pop spec))
               (sym (intern
                     (format
                      "transient:%s:%s:%d" prefix
                      (replace-regexp-in-string (plist-get args :key) " " "")
                      (prog1 gensym-counter (cl-incf gensym-counter))))))
           (use :command
                `(prog1 ',sym
                   (put ',sym 'interactive-only t)
                   (put ',sym 'completion-predicate #'transient--suffix-only)
                   (defalias ',sym ,cmd)))))
        ((cl-type (or string (and list (not null))))
         (let ((arg (pop spec)))
           (cl-typecase arg
             (list
              (use :shortarg (car arg))
              (use :argument (cadr arg))
              (setq arg (cadr arg)))
             (string
              (when-let* ((shortarg (transient--derive-shortarg arg)))
                (use :shortarg shortarg))
              (use :argument arg)))
           (use :command
                (let ((sym (intern (format "transient:%s:%s" prefix arg))))
                  `(prog1 ',sym
                     (put ',sym 'interactive-only t)
                     (put ',sym 'completion-predicate #'transient--suffix-only)
                     (defalias ',sym #'transient--default-infix-command))))
           (pcase (car spec)
             ((cl-type (and (not null) (not keyword)))
              (setq class 'transient-option)
              (use :reader (macroexp-quote (pop spec))))
             ((guard (string-suffix-p "=" arg))
              (setq class 'transient-option))
             (_ (setq class 'transient-switch)))))
        (invalid
         (error "Need command, argument, `:info' or `:info*'; got %s" invalid)))
      (while (keywordp (car spec))
        (let* ((key (pop spec))
               (val (if spec (pop spec) (error "No value for `%s'" key))))
          (pcase key
            (:class (setq class val))
            (:info  (setq class 'transient-information)
                    (use :description val))
            (:info* (setq class 'transient-information*)
                    (use :description val))
            (:cons
             (setq class 'transient-cons-option)
             (use :command
                  (let ((sym (intern (format "transient:%s:%s" prefix val))))
                    `(prog1 ',sym
                       (put ',sym 'interactive-only t)
                       (put ',sym 'completion-predicate #'transient--suffix-only)
                       (defalias ',sym #'transient--default-infix-command))))
             (use :argument val))
            ((guard (eq (car-safe val) '\,))
             (use key (cadr val)))
            ((guard (or (symbolp val)
                        (and (listp val)
                             (not (memq (car val) (list 'lambda (intern "")))))))
             (use key (macroexp-quote val)))
            (_ (use key val)))))
      (when spec
        (error "Need keyword, got %S" (car spec)))
      (cond*
        ((bind-and* (key (plist-get args :key)))
         (when (string-match "\\`\\({p}\\)" key)
           (use :key
                (replace-match transient-common-command-prefix t t key 1))))
        ((bind-and* (shortarg (plist-get args :shortarg)))
         (use :key shortarg))))
    (list 'cons
          (macroexp-quote (or class 'transient-suffix))
          (cons 'list args))))

(defun transient--derive-shortarg (arg)
  (save-match-data
    (and (string-match "\\`\\(-[a-zA-Z]\\)\\(\\'\\|=\\)" arg)
         (match-string 1 arg))))

(defun transient-command-completion-not-suffix-only-p (symbol _buffer)
  "Say whether SYMBOL should be offered as a completion.
If the value of SYMBOL's `completion-predicate' property is
`transient--suffix-only', then return nil, otherwise return t.
This is the case when a command should only ever be used as a
suffix of a transient prefix command (as opposed to bindings
in regular keymaps or by using `execute-extended-command')."
  (not (eq (get symbol 'completion-predicate) 'transient--suffix-only)))

(defalias 'transient--suffix-only #'ignore
  "Ignore ARGUMENTS, do nothing, and return nil.
See also `transient-command-completion-not-suffix-only-p'.
Only use this alias as the value of the `completion-predicate'
symbol property.")

(unless read-extended-command-predicate
  (setq read-extended-command-predicate
        #'transient-command-completion-not-suffix-only-p))

(defun transient--set-layout (prefix layout)
  (put prefix 'transient--layout (vector 2 nil layout)))

(defun transient--get-layout (prefix)
  (cond*
    ((bind*
      (layout
       (or (get prefix 'transient--layout)
           ;; Migrate unparsed legacy group definition.
           (condition-case-unless-debug err
               (and-let* ((value (symbol-value prefix)))
                 (transient--set-layout
                  prefix
                  (if (and (listp value)
                           (or (listp (car value))
                               (vectorp (car value))))
                      (transient-parse-suffixes prefix value)
                    (list (transient-parse-suffix prefix value)))))
             (error
              (message "Not a legacy group definition: %s: %S" prefix err)
              nil))))))
    ((not layout)
     (error "Not a transient prefix command or group definition: %s" prefix))
    ((vectorp layout)
     (let ((version (aref layout 0)))
       (if (= version 2)
           layout
         (error "Unsupported layout version %s for %s" version prefix))))
    (t
     ;; Upgrade from version 1.
     (transient--set-layout
      prefix
      (named-let upgrade ((spec layout))
        (cond ((vectorp spec)
               (pcase-let ((`[,level ,class ,args ,children] spec))
                 (when level
                   (setq args (plist-put args :level level)))
                 (vector class args (mapcar #'upgrade children))))
              ((and (listp spec)
                    (length= spec 3)
                    (or (null (car spec))
                        (natnump (car spec)))
                    (symbolp (cadr spec)))
               (pcase-let ((`(,level ,class ,args) spec))
                 (when level
                   (setq args (plist-put args :level level)))
                 (cons class args)))
              ((listp spec)
               (mapcar #'upgrade spec))
              (t spec)))))))

(defun transient--get-children (prefix)
  (aref (transient--get-layout prefix) 2))

(defun transient-parse-suffix (prefix suffix)
  "Parse SUFFIX, to be added to PREFIX.
PREFIX is a prefix command symbol or object.
SUFFIX is a suffix command or a group specification (of
  the same forms as expected by `transient-define-prefix').
Intended for use in a group's `:setup-children' function."
  (when (cl-typep prefix 'transient-prefix)
    (setq prefix (oref prefix command)))
  (eval (car (transient--parse-child prefix suffix)) t))

(defun transient-parse-suffixes (prefix suffixes)
  "Parse SUFFIXES, to be added to PREFIX.
PREFIX is a prefix command symbol or object.
SUFFIXES is a list of suffix command or a group specification
  (of the same forms as expected by `transient-define-prefix').
Intended for use in a group's `:setup-children' function."
  (when (cl-typep prefix 'transient-prefix)
    (setq prefix (oref prefix command)))
  (mapcar (apply-partially #'transient-parse-suffix prefix) suffixes))

;;;; Edit

(defun transient--insert-suffix (prefix loc suffix action &optional keep-other)
  (pcase-let* ((suf (cl-etypecase suffix
                      (vector (eval (transient--parse-group  prefix suffix) t))
                      (list   (eval (transient--parse-suffix prefix suffix) t))
                      (string suffix)
                      (symbol suffix)))
               (`(,elt ,group) (transient--locate-child prefix loc)))
    (cond
      ((not elt)
       (funcall (if transient-error-on-insert-failure #'error #'message)
                "Cannot insert %S into %s; %s not found"
                suffix prefix loc))
      ((or (and (vectorp suffix) (not (vectorp elt)))
           (and (listp   suffix) (vectorp elt))
           (and (stringp suffix) (vectorp elt)))
       (funcall (if transient-error-on-insert-failure #'error #'message)
                "Cannot place %S into %s at %s; %s"
                suffix prefix loc
                "suffixes and groups cannot be siblings"))
      (t
       (when-let* ((_(not (eq keep-other 'always)))
                   (bindingp (listp suf))
                   (key (transient--suffix-key suf))
                   (conflict (car (transient--locate-child prefix key)))
                   (conflictp
                    (and (not (and (eq action 'replace)
                                   (eq conflict elt)))
                         (or (not keep-other)
                             (eq (plist-get (transient--suffix-props suf)
                                            :command)
                                 (plist-get (transient--suffix-props conflict)
                                            :command)))
                         (equal (transient--suffix-predicate suf)
                                (transient--suffix-predicate conflict)))))
         (transient-remove-suffix prefix key)
         (pcase-setq `(,elt ,group) (transient--locate-child prefix loc)))
       (let ((mem (memq elt (aref group 2))))
         (pcase-exhaustive action
           ('insert  (setcdr mem (cons elt (cdr mem)))
                     (setcar mem suf))
           ('append  (setcdr mem (cons suf (cdr mem))))
           ('replace (setcar mem suf))))))))

;;;###autoload
(defun transient-insert-suffix (prefix loc suffix &optional keep-other)
  "Insert a SUFFIX into PREFIX before LOC.
PREFIX is a prefix command, a symbol.
SUFFIX is a suffix command or a group specification (of
  the same forms as expected by `transient-define-prefix').
LOC is a command, a key vector, a key description (a string
  as returned by `key-description'), or a coordination list
  (whose last element may also be a command or key).
Remove a conflicting binding unless optional KEEP-OTHER is
  non-nil.  When the conflict appears to be a false-positive,
  non-nil KEEP-OTHER may be ignored, which can be prevented
  by using `always'.
See info node `(transient)Modifying Existing Transients'."
  (declare (indent defun))
  (transient--insert-suffix prefix loc suffix 'insert keep-other))

;;;###autoload
(defun transient-append-suffix (prefix loc suffix &optional keep-other)
  "Insert a SUFFIX into PREFIX after LOC.
PREFIX is a prefix command, a symbol.
SUFFIX is a suffix command or a group specification (of
  the same forms as expected by `transient-define-prefix').
LOC is a command, a key vector, a key description (a string
  as returned by `key-description'), or a coordination list
  (whose last element may also be a command or key).
Remove a conflicting binding unless optional KEEP-OTHER is
  non-nil.  When the conflict appears to be a false-positive,
  non-nil KEEP-OTHER may be ignored, which can be prevented
  by using `always'.
See info node `(transient)Modifying Existing Transients'."
  (declare (indent defun))
  (transient--insert-suffix prefix loc suffix 'append keep-other))

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
(defun transient-inline-group (prefix group)
  "Inline the included GROUP into PREFIX.
Replace the symbol GROUP with its expanded layout in the
layout of PREFIX."
  (declare (indent defun))
  (cl-assert (symbolp group))
  (pcase-let ((`(,suffix ,parent) (transient--locate-child prefix group)))
    (when suffix
      (let* ((siblings (aref parent 2))
             (pos (cl-position group siblings)))
        (aset parent 2
              (nconc (seq-take siblings pos)
                     (transient--get-children group)
                     (seq-drop siblings (1+ pos))))))))

;;;###autoload
(defun transient-remove-suffix (prefix loc)
  "Remove the suffix or group at LOC in PREFIX.
PREFIX is a prefix command, a symbol.
LOC is a command, a key vector, a key description (a string
  as returned by `key-description'), or a coordination list
  (whose last element may also be a command or key).
See info node `(transient)Modifying Existing Transients'."
  (declare (indent defun))
  (pcase-let ((`(,suffix ,group) (transient--locate-child prefix loc)))
    (when suffix
      (aset group 2 (delq suffix (aref group 2))))))

(defun transient-suffix-put (prefix loc prop value)
  "Edit the suffix at LOC in PREFIX, setting PROP to VALUE.
PREFIX is a prefix command, a symbol.
SUFFIX is a suffix command or a group specification (of
  the same forms as expected by `transient-define-prefix').
LOC is a command, a key vector, a key description (a string
  as returned by `key-description'), or a coordination list
  (whose last element may also be a command or key).
See info node `(transient)Modifying Existing Transients'."
  (let ((child (transient-get-suffix prefix loc)))
    (if (vectorp child)
        (aset child 1 (plist-put (aref child 1) prop value))
      (setcdr child (plist-put (transient--suffix-props child) prop value)))))

(defalias 'transient--suffix-props #'cdr)

(defun transient-get-suffix (prefix loc)
  "Return the suffix or group at LOC in PREFIX.
PREFIX is a prefix command, a symbol.
LOC is a command, a key vector, a key description (a string
  as returned by `key-description'), or a coordination list
  (whose last element may also be a command or key).
See info node `(transient)Modifying Existing Transients'."
  (or (car (transient--locate-child prefix loc))
      (error "%s not found in %s" loc prefix)))

(defun transient--locate-child (group loc)
  (when (symbolp group)
    (setq group (transient--get-layout group)))
  (when (vectorp loc)
    (setq loc (append loc nil)))
  (cond*
    ((atom loc)
     (seq-some (lambda (child)
                 (transient--match-child group loc child))
               (aref group 2)))
    ((bind-and* (match (transient--nth (pop loc) (aref group 2))))
     (cond (loc (transient--locate-child
                 match (cond ((or (stringp (car loc))
                                  (symbolp (car loc)))
                              (car loc))
                             ((symbolp match)
                              (vconcat (cons 0 loc)))
                             ((vconcat loc)))))
           ((list match group))))))

(defun transient--match-child (group loc child)
  (cl-etypecase child
    (string nil)
    (symbol (cond*
              ((symbolp loc)
               (and (eq child loc)
                    (list child group)))
              ((bind-and* (include (transient--get-layout child)))
               (transient--locate-child include loc))))
    (vector (seq-some (lambda (subgroup)
                        (transient--locate-child subgroup loc))
                      (aref group 2)))
    (list   (and (if (symbolp loc)
                     (eq (plist-get (transient--suffix-props child) :command)
                         loc)
                   (equal (kbd (transient--suffix-key child))
                          (kbd loc)))
                 (list child group)))))

(defun transient--nth (n list)
  (nth (if (< n 0) (- (length list) (abs n)) n) list))

(defun transient--suffix-key (spec)
  (let ((props (transient--suffix-props spec)))
    (or (plist-get props :key)
        (transient--command-key
         (plist-get props :command)))))

(defun transient--command-key (cmd)
  (and-let* ((obj (transient--suffix-prototype cmd)))
    (cond ((slot-boundp obj 'key)
           (oref obj key))
          ((slot-exists-p obj 'shortarg)
           (if (slot-boundp obj 'shortarg)
               (oref obj shortarg)
             (transient--derive-shortarg (oref obj argument)))))))

(defun transient-set-default-level (command level)
  "Set the default level of suffix COMMAND to LEVEL.

The default level is shadowed if the binding of the suffix in a
prefix menu specifies a level, and also if the user changes the
level of such a binding.

The default level can only be set for commands that were defined
using `transient-define-suffix', `transient-define-infix' or
`transient-define-argument'."
  (if-let* ((proto (transient--suffix-prototype command)))
      (oset proto level level)
    (user-error "Cannot set level for `%s'; no prototype object exists"
                command)))

;;;; Variables

(defvar transient-current-prefix nil
  "The transient from which this suffix command was invoked.
This is an object representing that transient, use
`transient-current-command' to get the respective command.")

(defvar transient-current-command nil
  "The transient from which this suffix command was invoked.
This is a symbol representing that transient, use
`transient-current-prefix' to get the respective object.")

(defvar transient-current-suffixes nil
  "The suffixes of the transient from which this suffix command was invoked.
This is a list of objects.  Usually it is sufficient to instead
use the function `transient-args', which returns a list of
values.  In complex cases it might be necessary to use this
variable instead.")

(defvar transient-exit-hook nil
  "Hook run after exiting a transient menu.
Unlike `transient-post-exit-hook', this runs even if another transient
menu becomes active at the same time. ")

(defvar transient-post-exit-hook nil
  "Hook run after exiting all transient menus.
Unlike `transient-exit-hook', this does not run if another transient
menu becomes active at the same time.")

(defvar transient-setup-buffer-hook nil
  "Hook run when setting up the transient buffer.
That buffer is current and empty when this hook runs.")

(defvar transient--prefix nil)
(defvar transient--layout nil)
(defvar transient--suffixes nil)

(defconst transient--stay t   "Do not exit the transient.")
(defconst transient--exit nil "Do exit the transient.")

(defvar transient--exitp nil "Whether to exit the transient.")
(defvar transient--showp nil "Whether to show the transient menu buffer.")
(defvar transient--helpp nil "Whether help-mode is active.")
(defvar transient--docsp nil "Whether docstring-mode is active.")
(defvar transient--editp nil "Whether edit-mode is active.")

(defvar transient--refreshp nil
  "Whether to refresh the transient completely.")

(defvar transient--all-levels-p nil
  "Whether temporary display of suffixes on all levels is active.")

(defvar transient--timer nil)

(defvar transient--stack nil)

(defvar transient--minibuffer-depth 0)

(defvar transient--buffer-name " *transient*"
  "Name of the transient buffer.")

(defvar transient--buffer nil
  "The transient menu buffer.")

(defvar transient--window nil
  "The window used to display transient's menu buffer.")

(defvar transient--original-window nil
  "The window that was selected before the transient was invoked.
Usually it remains selected while the transient is active.")

(defvar transient--original-buffer nil
  "The buffer that was current before the transient was invoked.
Usually it remains current while the transient is active.")

(defvar transient--restore-winconf nil
  "Window configuration to restore after exiting help.")

(defvar transient--shadowed-buffer nil
  "The buffer that is temporarily shadowed by the transient buffer.
This is bound while the suffix predicate is being evaluated and while
drawing in the transient buffer.")

(defvar transient--pending-suffix nil
  "The suffix that is currently being processed.
This is bound while the suffix predicate is being evaluated,
and while functions that return faces are being evaluated.")

(defvar transient--current-suffix nil
  "The suffix currently being invoked using a mouse event.
Do not use this; instead use function `transient-suffix-object'.")

(defvar transient--pending-group nil
  "The group that is currently being processed.
This is bound while the suffixes are drawn in the transient buffer.")

(defvar transient--debug nil
  "Whether to put debug information into *Messages*.")

(defvar transient--history nil)

(defvar transient--scroll-commands
  '(transient-scroll-up
    transient-scroll-down
    mwheel-scroll
    scroll-bar-toolkit-scroll))

(defvar transient--quit-commands
  '(transient-quit-one
    transient-quit-all))

;;;; Identities

(defun transient-active-prefix (&optional prefixes)
  "Return the active transient object.

Return nil if there is no active transient, if the transient buffer
isn't shown, and while the active transient is suspended (e.g., while
the minibuffer is in use).

Unlike `transient-current-prefix', which is only ever non-nil in code
that is run directly by a command that is invoked while a transient
is current, this function is also suitable for use in asynchronous
code, such as timers and callbacks (this function's main use-case).

If optional PREFIXES is non-nil, it must be a prefix command symbol
or a list of symbols, in which case the active transient object is
only returned if it matches one of PREFIXES."
  (and transient--showp
       transient--prefix
       (or (not prefixes)
           (memq (oref transient--prefix command) (ensure-list prefixes)))
       (or (memq 'transient--pre-command pre-command-hook)
           (and (memq t pre-command-hook)
                (memq 'transient--pre-command
                      (default-value 'pre-command-hook))))
       transient--prefix))

(defun transient-prefix-object ()
  "Return the current prefix as an object.

While a transient is being setup or refreshed (which involves
preparing its suffixes) the variable `transient--prefix' can be
used to access the prefix object.  Thus this is what has to be
used in suffix methods such as `transient-format-description',
and in object-specific functions that are stored in suffix slots
such as `description'.

When a suffix command is invoked (i.e., in its `interactive' form
and function body) then the variable `transient-current-prefix'
has to be used instead.

Two distinct variables are needed, because any prefix may itself
be used as a suffix of another prefix, and such sub-prefixes have
to be able to tell themselves apart from the prefix they were
invoked from.

Regular suffix commands, which are not prefixes, do not have to
concern themselves with this distinction, so they can use this
function instead.  In the context of a plain suffix, it always
returns the value of the appropriate variable."
  (or transient--prefix transient-current-prefix))

(defun transient-suffix-object (&optional command)
  "Return the object associated with the current suffix command.

Each suffix commands is associated with an object, which holds
additional information about the suffix, such as its value (in
the case of an infix command, which is a kind of suffix command).

This function is intended to be called by infix commands, which
are usually aliases of `transient--default-infix-command', which
is defined like this:

  (defun transient--default-infix-command ()
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
to do, for others it is not.)  In that case nil may be returned,
if the command was not defined using one of the macros intended
to define such commands.

The optional argument COMMAND is intended for internal use.  If
you are contemplating using it in your own code, then you should
probably use this instead:

  (get COMMAND \\='transient--suffix)"
  (when command
    (cl-check-type command command))
  (cond*
    (transient--pending-suffix)
    (transient--current-suffix)
    ((or transient--prefix
         transient-current-prefix)
     (let ((suffixes
            (cl-remove-if-not
             (lambda (obj)
               (eq (oref obj command)
                   (or command
                       (if (eq this-command 'transient-set-level)
                           ;; This is how it can look up for which
                           ;; command it is setting the level.
                           this-original-command
                         this-command))))
             (or transient--suffixes
                 transient-current-suffixes))))
       (cond
         ((length= suffixes 1)
          (car suffixes))
         ((cl-find-if (lambda (obj)
                        (equal (listify-key-sequence (kbd (oref obj key)))
                               (listify-key-sequence (this-command-keys))))
                      suffixes))
         ;; COMMAND is only provided if `this-command' is meaningless, in
         ;; which case `this-command-keys' is also meaningless, making it
         ;; impossible to disambiguate bindings for the same command.
         (command (car suffixes))
         ;; If COMMAND is nil, then failure to disambiguate likely means
         ;; that there is a bug somewhere.
         ((length> suffixes 1)
          (error "BUG: Cannot unambiguously determine suffix object"))
         ;; It is legitimate to use this function as a predicate of sorts.
         ;; `transient--pre-command' and `transient-help' are examples.
         (t nil))))
    ((bind-and* (obj (transient--suffix-prototype (or command this-command)))
                (obj (clone obj)))
     (transient-init-scope obj)
     (transient-init-value obj)
     obj)))

(defun transient--suffix-prototype (command)
  (or (get command 'transient--suffix)
      (seq-some (lambda (cmd) (get cmd 'transient--suffix))
                (function-alias-p command))))

;;;; Keymaps

(defvar-keymap transient-base-map
  :doc "Parent of other keymaps used by Transient.

This is the parent keymap of all the keymaps that are used in
all transients: `transient-map' (which in turn is the parent
of the transient-specific keymaps), `transient-edit-map' and
`transient-sticky-map'.

If you change a binding here, then you might also have to edit
`transient-sticky-map' and `transient-common-commands'.  While
the latter isn't a proper transient prefix command, it can be
edited using the same functions as used for transients.

If you add a new command here, then you must also add a binding
to `transient-predicate-map'."
  "ESC ESC ESC" #'transient-quit-all
  "C-g"     #'transient-quit-one
  "C-q"     #'transient-quit-all
  "C-z"     #'transient-suspend
  "C-v"     #'transient-scroll-up
  "C-M-v"   #'transient-scroll-down
  "<next>"  #'transient-scroll-up
  "<prior>" #'transient-scroll-down)

(defvar-keymap transient-map
  :doc "Top-level keymap used by all transients.

If you add a new command here, then you must also add a binding
to `transient-predicate-map'.  See also `transient-base-map'."
  :parent transient-base-map
  "C-u"     #'universal-argument
  "C--"     #'negative-argument
  "C-t"     #'transient-show
  "?"       #'transient-help
  "C-h"     #'transient-help
  "C-x 5 5" #'other-frame-prefix
  "C-x 4 4" #'other-window-prefix
  ;; These have additional bindings in transient-common-commands.
  "C-M-p"   #'transient-history-prev
  "C-M-n"   #'transient-history-next)

(defvar-keymap transient-edit-map
  :doc "Keymap that is active while a transient in is in \"edit mode\"."
  :parent transient-base-map
  "?"   #'transient-help
  "C-h" #'transient-help)

(defvar-keymap transient-sticky-map
  :doc "Keymap that is active while an incomplete key sequence is active."
  :parent transient-base-map
  "C-g" #'transient-quit-seq)

(defvar transient-common-commands
  [:hide (lambda ()
           (defvar transient--redisplay-key)
           (and (not (equal (vconcat transient--redisplay-key)
                            (read-kbd-macro transient-common-command-prefix)))
                (not transient-show-common-commands)))
   ["Value commands"
    ("{p} s  " "Set"            transient-set)
    ("{p} C-s" "Save"           transient-save)
    ("{p} C-k" "Reset"          transient-reset)
    ("{p} p  " "Previous value" transient-history-prev)
    ("{p} n  " "Next value"     transient-history-next)]
   ["Sticky commands"
    ;; Like `transient-sticky-map' except that
    ;; "C-g" has to be bound to a different command.
    ("C-g" "Quit prefix or transient" transient-quit-one)
    ("C-q" "Quit transient stack"     transient-quit-all)
    ("C-z" "Suspend transient stack"  transient-suspend)]
   ["Customize"
    ("{p} t" transient-toggle-common)
    ("{p} l" "Show/hide suffixes" transient-set-level)
    ("{p} a" transient-toggle-level-limit)]]
  "Commands available in all transient menus.

The same functions, that are used to change bindings in transient prefix
commands and transient groups (defined using `transient-define-group'),
should be used to modify these bindings as well.  The actual layout is
stored in the symbol's `transient--layout' property.  The variable value
is only used when customizing `transient-common-command-prefix', which
resets the value of `transient--layout' based on the values of that
option and this variable.")

(defun transient--init-common-commands ()
  (transient--set-layout
   'transient-common-commands
   (list (eval (car (transient--parse-child 'transient-common-commands
                                            transient-common-commands))
               t)))
  (defvar transient-common-command-prefix)
  (defvar transient--docstr-hint-1)
  (defvar transient--docstr-hint-2)
  (setq transient--docstr-hint-1
        (define-keymap transient-common-command-prefix
          'transient-common-command-prefix))
  (setq transient--docstr-hint-2
        (define-keymap (concat transient-common-command-prefix " t")
          'transient-toggle-common)))

(defcustom transient-common-command-prefix "C-x"
  "The prefix key used for most commands common to all menus.

Some shared commands are available in all transient menus, most of
which share a common prefix specified by this option.  By default the
bindings for these shared commands are only shown after pressing that
prefix key and before following that up with a valid key binding.

For historic reasons \\`C-x' is used by default, but users are
encouraged to pick another key, preferably one that is not commonly used
in Emacs but is still convenient to them.  See info node `(transient)
Common Suffix Commands'."
  :type 'key
  :initialize (lambda (symbol exp)
                (custom-initialize-default symbol exp)
                (transient--init-common-commands))
  :set (lambda (symbol value)
         (set-default symbol value)
         (transient--init-common-commands)))

(defvar-keymap transient-popup-navigation-map
  :doc "One of the keymaps used when menu navigation is enabled.
See `transient-enable-popup-navigation'."
  "<down-mouse-1>" #'transient-noop
  "<up>"   #'transient-backward-button
  "<down>" #'transient-forward-button
  "C-r"    #'transient-isearch-backward
  "C-s"    #'transient-isearch-forward
  "M-RET"  #'transient-push-button)

(defvar-keymap transient-button-map
  :doc "One of the keymaps used when menu navigation is enabled.
See `transient-enable-popup-navigation'."
  "<mouse-1>" #'transient-push-button
  "<mouse-2>" #'transient-push-button)

(defvar-keymap transient-resume-mode-map
  :doc "Keymap for `transient-resume-mode'.

This keymap remaps every command that would usually just quit the
documentation buffer to `transient-resume', which additionally
resumes the suspended transient."
  "<remap> <Man-quit>"    #'transient-resume
  "<remap> <Info-exit>"   #'transient-resume
  "<remap> <quit-window>" #'transient-resume)

(defvar-keymap transient-predicate-map
  :doc "Base keymap used to map common commands to their transient behavior.

The \"transient behavior\" of a command controls, among other
things, whether invoking the command causes the transient to be
exited or not, and whether infix arguments are exported before
doing so.

Each \"key\" is a command that is common to all transients and
that is bound in `transient-map', `transient-edit-map',
`transient-sticky-map' and/or `transient-common-command'.

Each binding is a \"pre-command\", a function that controls the
transient behavior of the respective command.

For transient commands that are bound in individual transients,
the transient behavior is specified using the `:transient' slot
of the corresponding object."
  "<transient-suspend>"           #'transient--do-suspend
  "<transient-help>"              #'transient--do-stay
  "<transient-set-level>"         #'transient--do-stay
  "<transient-history-prev>"      #'transient--do-stay
  "<transient-history-next>"      #'transient--do-stay
  "<universal-argument>"          #'transient--do-stay
  "<universal-argument-more>"     #'transient--do-stay
  "<negative-argument>"           #'transient--do-minus
  "<digit-argument>"              #'transient--do-stay
  "<other-frame-prefix>"          #'transient--do-stay
  "<other-window-prefix>"         #'transient--do-stay
  "<top-level>"                   #'transient--do-quit-all
  "<transient-quit-all>"          #'transient--do-quit-all
  "<transient-quit-one>"          #'transient--do-quit-one
  "<transient-quit-seq>"          #'transient--do-stay
  "<transient-show>"              #'transient--do-stay
  "<transient-update>"            #'transient--do-stay
  "<transient-set>"               #'transient--do-call
  "<transient-set-and-exit>"      #'transient--do-exit
  "<transient-save>"              #'transient--do-call
  "<transient-save-and-exit>"     #'transient--do-exit
  "<transient-reset>"             #'transient--do-call
  "<describe-key-briefly>"        #'transient--do-stay
  "<describe-key>"                #'transient--do-stay
  "<transient-scroll-up>"         #'transient--do-stay
  "<transient-scroll-down>"       #'transient--do-stay
  "<mwheel-scroll>"               #'transient--do-stay
  "<scroll-bar-toolkit-scroll>"   #'transient--do-stay
  "<transient-noop>"              #'transient--do-noop
  "<transient-mouse-push-button>" #'transient--do-move
  "<transient-push-button>"       #'transient--do-push-button
  "<transient-backward-button>"   #'transient--do-move
  "<transient-forward-button>"    #'transient--do-move
  "<transient-isearch-backward>"  #'transient--do-move
  "<transient-isearch-forward>"   #'transient--do-move
  "<transient-copy-menu-text>"    #'transient--do-stay
  "<transient-toggle-docstrings>" #'transient--do-stay
  ;; If a valid but incomplete prefix sequence is followed by
  ;; an unbound key, then Emacs calls the `undefined' command
  ;; but does not set `this-command', `this-original-command'
  ;; or `real-this-command' accordingly.  Instead they are nil.
  "<nil>"                         #'transient--do-warn
  ;; Bound to the `mouse-movement' event, this command is similar
  ;; to `ignore'.
  "<ignore-preserving-kill-region>" #'transient--do-noop)

(defvar transient--transient-map nil)
(defvar transient--predicate-map nil)
(defvar transient--redisplay-map nil)
(defvar transient--redisplay-key nil)

(defun transient--push-keymap (var)
  (let ((map (symbol-value var)))
    (transient--debug "     push %s%s" var (if map "" " VOID"))
    (when map
      (with-demoted-errors "transient--push-keymap: %S"
        (internal-push-keymap map 'overriding-terminal-local-map)))))

(defun transient--pop-keymap (var)
  (let ((map (symbol-value var)))
    (when map
      (transient--debug "     pop  %s" var)
      (with-demoted-errors "transient--pop-keymap: %S"
        (internal-pop-keymap map 'overriding-terminal-local-map)))))

(defun transient--make-transient-map ()
  (let ((map (make-sparse-keymap)))
    (cond (transient--editp
           (keymap-set map (concat transient-common-command-prefix " l")
                       #'transient-set-level)
           (set-keymap-parent map transient-edit-map))
          ((set-keymap-parent map transient-map)))
    (dolist (obj transient--suffixes)
      (let ((key (oref obj key)))
        (when (vectorp key)
          (setq key (key-description key))
          (oset obj key key))
        (when transient-substitute-key-function
          (setq key (save-match-data
                      (funcall transient-substitute-key-function obj)))
          (oset obj key key))
        (let* ((kbd (kbd key))
               (cmd (oref obj command))
               (alt (transient--lookup-key map kbd)))
          (cond ((not alt)
                 (define-key map kbd cmd))
                ((eq alt cmd))
                ((oref obj inactive))
                ((oref obj inapt))
                ((and-let* ((alt (transient-suffix-object alt)))
                   (or (oref alt inactive)
                       (oref alt inapt)))
                 (define-key map kbd cmd))
                (transient-detect-key-conflicts
                 (error "Cannot bind %S to %s and also %s"
                        (string-trim key) cmd alt))
                ((define-key map kbd cmd))))))
    (when-let* ((b (keymap-lookup map "-"))) (keymap-set map "<kp-subtract>" b))
    (when-let* ((b (keymap-lookup map "="))) (keymap-set map "<kp-equal>" b))
    (when-let* ((b (keymap-lookup map "+"))) (keymap-set map "<kp-add>" b))
    (when transient-enable-popup-navigation
      ;; `transient--make-redisplay-map' maps only over bindings that are
      ;; directly in the base keymap, so that cannot be a composed keymap.
      (set-keymap-parent
       map (make-composed-keymap
            (keymap-parent map)
            transient-popup-navigation-map)))
    map))

(defun transient--make-predicate-map ()
  (let ((default (transient--resolve-pre-command
                  (oref transient--prefix transient-suffix)))
        (return (and transient--stack (oref transient--prefix return)))
        (map (make-sparse-keymap)))
    (set-keymap-parent map transient-predicate-map)
    (when (or (and (slot-boundp transient--prefix 'transient-switch-frame)
                   (transient--resolve-pre-command
                    (not (oref transient--prefix transient-switch-frame))))
              (memq (transient--resolve-pre-command
                     (oref transient--prefix transient-non-suffix))
                    '(nil transient--do-warn transient--do-noop)))
      (define-key map [handle-switch-frame] #'transient--do-suspend))
    (dolist (obj transient--suffixes)
      (let* ((cmd (oref obj command))
             (id (vector cmd))
             (kind (cond ((get cmd 'transient--prefix)    'prefix)
                         ((cl-typep obj 'transient-infix) 'infix)
                         (t                               'suffix)))
             (pre (cond
                    ((oref obj inactive) nil)
                    ((oref obj inapt) #'transient--do-warn-inapt)
                    ((slot-boundp obj 'transient)
                     (pcase (list kind
                                  (transient--resolve-pre-command
                                   (oref obj transient) nil t)
                                  return)
                       (`(prefix   t  ,_) #'transient--do-recurse)
                       (`(prefix nil  ,_) #'transient--do-stack)
                       (`(infix    t  ,_) #'transient--do-stay)
                       (`(suffix   t  ,_) #'transient--do-call)
                       ('(suffix nil   t) #'transient--do-return)
                       (`(,_     nil  ,_) #'transient--do-exit)
                       (`(,_     ,do  ,_) do)))
                    ((not (lookup-key transient-predicate-map id))
                     (pcase (list kind default return)
                       (`(prefix ,(or 'transient--do-stay 'transient--do-call) ,_)
                        #'transient--do-recurse)
                       (`(prefix   t  ,_) #'transient--do-recurse)
                       (`(prefix  ,_  ,_) #'transient--do-stack)
                       (`(infix   ,_  ,_) #'transient--do-stay)
                       (`(suffix   t  ,_) #'transient--do-call)
                       ('(suffix nil   t) #'transient--do-return)
                       (`(suffix nil nil) #'transient--do-exit)
                       (`(suffix ,do  ,_) do))))))
        (when pre
          (if-let* ((alt (lookup-key map id)))
              (unless (eq alt pre)
                (define-key map (vconcat (oref obj key) id) pre))
            (define-key map id pre)))))
    map))

(defun transient--make-redisplay-map ()
  (setq transient--redisplay-key
        (pcase this-command
          ('transient-update
           (setq transient--showp t)
           (let ((keys (listify-key-sequence (this-single-command-raw-keys))))
             (setq unread-command-events (mapcar (lambda (key) (cons t key)) keys))
             keys))
          ('transient-quit-seq
           (setq unread-command-events
                 (butlast (listify-key-sequence
                           (this-single-command-raw-keys))
                          2))
           (butlast transient--redisplay-key))
          (_ nil)))
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
         (let ((key (vconcat transient--redisplay-key)))
           (or (lookup-key transient--transient-map key)
               (and-let* ((regular (lookup-key local-function-key-map key)))
                 (lookup-key transient--transient-map (vconcat regular)))))
       transient--transient-map))
    topmap))

;;;; Setup

(defun transient-setup (&optional name layout edit &rest params)
  "Setup the transient specified by NAME.

This function is called by transient prefix commands to setup the
transient.  In that case NAME is mandatory, LAYOUT and EDIT must
be nil and PARAMS may be (but usually is not) used to set, e.g.,
the \"scope\" of the transient (see `transient-define-prefix').

This function is also called internally, in which case LAYOUT and
EDIT may be non-nil."
  (transient--debug 'setup)
  (transient--with-emergency-exit :setup
    (cond
      ((not name)
       ;; Switching between regular and edit mode.
       (transient--pop-keymap 'transient--transient-map)
       (transient--pop-keymap 'transient--redisplay-map)
       (setq name (oref transient--prefix command))
       (setq params (list :scope (oref transient--prefix scope))))
      (transient--prefix
       ;; Invoked as a ":transient-non-suffix 'transient--do-{stay,call}"
       ;; of an outer prefix.  Unlike the usual `transient--do-stack',
       ;; these predicates fail to clean up after the outer prefix.
       (transient--pop-keymap 'transient--transient-map)
       (transient--pop-keymap 'transient--redisplay-map))
      ((not (or layout                      ; resuming parent/suspended prefix
                transient-current-command)) ; entering child prefix
       (transient--stack-zap))              ; replace suspended prefix, if any
      (edit
       ;; Returning from help to edit.
       (setq transient--editp t)))
    (transient--env-apply
     (lambda ()
       (transient--init-transient name layout params)
       (transient--history-init transient--prefix)
       (setq transient--original-window (selected-window))
       (setq transient--original-buffer (current-buffer))
       (setq transient--minibuffer-depth (minibuffer-depth))
       (transient--redisplay))
     (get name 'transient--prefix))
    (transient--suspend-text-conversion-style)
    (transient--setup-transient)
    (transient--suspend-which-key-mode)))

(cl-defgeneric transient-setup-children (group children)
  "Setup the CHILDREN of GROUP.
If the value of the `setup-children' slot is non-nil, then call
that function with CHILDREN as the only argument and return the
value.  Otherwise return CHILDREN as is.")

(cl-defmethod transient-setup-children ((group transient-group) children)
  (if (slot-boundp group 'setup-children)
      (funcall (oref group setup-children) children)
    children))

(defun transient--env-apply (fn &optional prefix)
  (if-let* ((env (oref (or prefix transient--prefix) environment)))
      (funcall env fn)
    (funcall fn)))

(defun transient--init-transient (&optional name layout params)
  (unless name
    ;; Re-init.
    (if (eq transient--refreshp 'updated-value)
        ;; Preserve the prefix value this once, because the
        ;; invoked suffix indicates that it has updated that.
        (setq transient--refreshp (oref transient--prefix refresh-suffixes))
      ;; Otherwise update the prefix value from suffix values.
      (oset transient--prefix value (transient--get-extended-value))))
  (transient--init-objects name layout params)
  (transient--init-keymaps))

(defun transient--init-keymaps ()
  (setq transient--predicate-map (transient--make-predicate-map))
  (setq transient--transient-map (transient--make-transient-map))
  (setq transient--redisplay-map (transient--make-redisplay-map)))

(defun transient--init-objects (&optional name layout params)
  (if name
      (setq transient--prefix (transient--init-prefix name params))
    (setq name (oref transient--prefix command)))
  (setq transient--refreshp (oref transient--prefix refresh-suffixes))
  (cond ((and (not transient--refreshp) layout)
         (setq transient--layout layout)
         (setq transient--suffixes (transient--flatten-suffixes layout)))
        (t
         (setq transient--suffixes nil)
         (setq transient--layout (transient--init-suffixes name))
         (setq transient--suffixes (nreverse transient--suffixes))))
  (slot-makeunbound transient--prefix 'value))

(defun transient--init-prefix (name &optional params)
  (let ((obj (let ((proto (get name 'transient--prefix)))
               (apply #'clone proto
                      :prototype proto
                      :level (or (alist-get t (alist-get name transient-levels))
                                 transient-default-level)
                      params))))
    (transient-init-value  obj)
    (transient-init-return obj)
    (transient-init-scope  obj)
    obj))

(defun transient--init-suffixes (name)
  (let ((levels (alist-get name transient-levels)))
    (mapcan (lambda (c) (transient--init-child levels c nil))
            (append (transient--get-children name)
                    (and (not transient--editp)
                         (transient--get-children 'transient-common-commands))))))

(defun transient--flatten-suffixes (layout)
  (named-let flatten ((def layout))
    (cond ((stringp def) nil)
          ((cl-typep def 'transient-information) nil)
          ((listp def) (mapcan #'flatten def))
          ((cl-typep def 'transient-group)
           (mapcan #'flatten (oref def suffixes)))
          ((cl-typep def 'transient-suffix)
           (list def)))))

(defun transient--init-child (levels spec parent)
  (cl-etypecase spec
    (symbol (mapcan (lambda (c) (transient--init-child levels c parent))
                    (transient--get-children spec)))
    (vector (transient--init-group  levels spec parent))
    (list   (transient--init-suffix levels spec parent))
    (string (list spec))))

(defun transient--init-group (levels spec parent)
  (pcase-let* ((`[,class ,args ,children] spec)
               (level (or (plist-get args :level)
                          transient--default-child-level)))
    (and-let* ((_(transient--use-level-p level))
               (obj (apply class :parent parent :level level args))
               (_(transient--use-suffix-p obj))
               (_(prog1 t
                   (when (transient--inapt-suffix-p obj)
                     (oset obj inapt t))))
               (suffixes (mapcan (lambda (c) (transient--init-child levels c obj))
                                 (transient-setup-children obj children))))
      (progn
        (oset obj suffixes suffixes)
        (list obj)))))

(defun transient--init-suffix (levels spec parent)
  (let* ((class  (car spec))
         (args   (cdr spec))
         (cmd    (plist-get args :command))
         (_      (transient--load-command-if-autoload cmd))
         (key    (plist-get args :key))
         (key    (and key (kbd key)))
         (proto  (and cmd (transient--suffix-prototype cmd)))
         (level  (or (alist-get (cons cmd key) levels nil nil #'equal)
                     (alist-get cmd levels)
                     (plist-get args :level)
                     (and proto (oref proto level))
                     transient--default-child-level))
         (args   (plist-put (copy-sequence args) :level level))
         (obj    (if (child-of-class-p class 'transient-information)
                     (apply class :parent parent args)
                   (unless (and cmd (symbolp cmd))
                     (error "BUG: Non-symbolic suffix command: %s" cmd))
                   (if proto
                       (apply #'clone proto :parent parent args)
                     (apply class :command cmd :parent parent args))))
         (active (and (transient--use-level-p level)
                      (transient--use-suffix-p obj)))
         (inapt  (and active (transient--inapt-suffix-p obj)))
         (active (and active (not inapt))))
    (cond (inapt
           (oset obj inapt t))
          ((not active)
           (oset obj inactive t)))
    (cond ((not cmd))
          ((commandp cmd))
          ((or (cl-typep obj 'transient-switch)
               (cl-typep obj 'transient-option))
           ;; As a temporary special case, if the package was compiled
           ;; with an older version of Transient, then we must define
           ;; "anonymous" switch and option commands here.
           (defalias cmd #'transient--default-infix-command))
          (active
           (error "Suffix command %s is not defined or autoloaded" cmd)))
    (cond ((not (cl-typep obj 'transient-information))
           (transient--init-suffix-key obj)
           (transient-init-scope obj)
           (transient-init-value obj)
           (push obj transient--suffixes)))
    (list obj)))

(cl-defmethod transient--init-suffix-key ((obj transient-suffix))
  (unless (slot-boundp obj 'key)
    (error "No key for %s" (oref obj command))))

(cl-defmethod transient--init-suffix-key ((obj transient-argument))
  (if (transient-switches--eieio-childp obj)
      (cl-call-next-method obj)
    (when-let* ((_(not (slot-boundp obj 'shortarg)))
                (argument (oref obj argument))
                (_(stringp argument))
                (shortarg (transient--derive-shortarg argument)))
      (oset obj shortarg shortarg))
    (unless (slot-boundp obj 'key)
      (if (slot-boundp obj 'shortarg)
          (oset obj key (oref obj shortarg))
        (error "No key for %s" (oref obj command))))))

(defun transient--use-level-p (level &optional edit)
  (or transient--all-levels-p
      (and transient--editp (not edit))
      (and (>= level 1)
           (<= level (oref transient--prefix level)))))

(defun transient--use-suffix-p (obj)
  (let ((transient--shadowed-buffer (current-buffer))
        (transient--pending-suffix obj))
    (transient--do-suffix-p
     (oref obj if)
     (oref obj if-not)
     (oref obj if-nil)
     (oref obj if-non-nil)
     (oref obj if-mode)
     (oref obj if-not-mode)
     (oref obj if-derived)
     (oref obj if-not-derived)
     t)))

(defun transient--inapt-suffix-p (obj)
  (or (and-let* ((parent (oref obj parent)))
        (oref parent inapt))
      (let ((transient--shadowed-buffer (current-buffer))
            (transient--pending-suffix obj))
        (transient--do-suffix-p
         (oref obj inapt-if)
         (oref obj inapt-if-not)
         (oref obj inapt-if-nil)
         (oref obj inapt-if-non-nil)
         (oref obj inapt-if-mode)
         (oref obj inapt-if-not-mode)
         (oref obj inapt-if-derived)
         (oref obj inapt-if-not-derived)
         nil))))

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
    (if-derived          (if (or (atom if-derived)
                                 (>= emacs-major-version 30))
                             (derived-mode-p if-derived)
                           (apply #'derived-mode-p if-derived)))
    (if-not-derived (not (if (or (atom if-not-derived)
                                 (>= emacs-major-version 30))
                             (derived-mode-p if-not-derived)
                           (apply #'derived-mode-p if-not-derived))))
    (default)))

(defun transient--suffix-predicate (spec)
  (let ((props (transient--suffix-props spec)))
    (seq-some (lambda (prop)
                (and-let* ((pred (plist-get props prop)))
                  (list prop pred)))
              '( :if :if-not
                 :if-nil :if-non-nil
                 :if-mode :if-not-mode
                 :if-derived :if-not-derived
                 :inapt-if :inapt-if-not
                 :inapt-if-nil :inapt-if-non-nil
                 :inapt-if-mode :inapt-if-not-mode
                 :inapt-if-derived :inapt-if-not-derived))))

(defun transient--load-command-if-autoload (cmd)
  (when-let* ((_(symbolp cmd))
              (fn (symbol-function cmd))
              (_(autoloadp fn)))
    (transient--debug "   autoload %s" cmd)
    (autoload-do-load fn)))

;;;; Flow-Control

(defun transient--setup-transient ()
  (transient--debug 'setup-transient)
  (transient--push-keymap 'transient--transient-map)
  (transient--push-keymap 'transient--redisplay-map)
  (add-hook 'pre-command-hook  #'transient--pre-command 99)
  (add-hook 'post-command-hook #'transient--post-command)
  (advice-add 'recursive-edit :around #'transient--recursive-edit)
  (transient--quit-kludge 'enable)
  (when transient--exitp
    ;; This prefix command was invoked as the suffix of another.
    ;; Prevent `transient--post-command' from removing the hooks
    ;; that we just added.
    (setq transient--exitp 'replace)))

(defun transient--refresh-transient ()
  (transient--debug 'refresh-transient)
  (transient--pop-keymap 'transient--predicate-map)
  (transient--pop-keymap 'transient--transient-map)
  (transient--pop-keymap 'transient--redisplay-map)
  (transient--init-transient)
  (transient--push-keymap 'transient--transient-map)
  (transient--push-keymap 'transient--redisplay-map)
  (transient--redisplay))

(defun transient--pre-command ()
  (transient--debug 'pre-command)
  (transient--with-emergency-exit :pre-command
    ;; The use of `overriding-terminal-local-map' does not prevent the
    ;; lookup of command remappings in the overridden maps, which can
    ;; lead to a suffix being remapped to a non-suffix.  We have to undo
    ;; the remapping in that case.  However, remapping a non-suffix to
    ;; another should remain possible.
    (when (and (transient--get-pre-command this-original-command nil 'suffix)
               (not (transient--get-pre-command this-command nil 'suffix)))
      (setq this-command this-original-command))
    (cond
      ((memq this-command '(transient-update transient-quit-seq))
       (transient--pop-keymap 'transient--redisplay-map))
      ((and transient--helpp
            (not (memq this-command transient--quit-commands)))
       (cond
         ((transient-help)
          (transient--do-suspend)
          (setq this-command 'transient-suspend)
          (transient--pre-exit))
         ((not (transient--edebug-command-p))
          (setq this-command 'transient-undefined))))
      ((and transient--editp
            (transient-suffix-object)
            (not (memq this-command
                       (cons 'transient-help transient--quit-commands))))
       (setq this-command 'transient-set-level)
       (transient--wrap-command))
      (t
       (setq transient--exitp nil)
       (let ((exitp (eq (transient--call-pre-command) transient--exit)))
         (transient--wrap-command)
         (when exitp
           (transient--maybe-set-value 'exit)
           (transient--pre-exit)))))))

(defun transient--pre-exit ()
  (transient--debug 'pre-exit)
  (transient--delete-window)
  (transient--timer-cancel)
  (transient--pop-keymap 'transient--transient-map)
  (transient--pop-keymap 'transient--redisplay-map)
  (unless transient--showp
    (let ((message-log-max nil))
      (message "")))
  (setq transient--transient-map nil)
  (setq transient--predicate-map nil)
  (setq transient--redisplay-map nil)
  (setq transient--redisplay-key nil)
  (setq transient--helpp nil)
  (unless (eq transient--docsp 'permanent)
    (setq transient--docsp nil))
  (setq transient--editp nil)
  (setq transient--prefix nil)
  (setq transient--layout nil)
  (setq transient--suffixes nil)
  (setq transient--original-window nil)
  (setq transient--original-buffer nil)
  (setq transient--window nil))

(defun transient--export ()
  (setq transient-current-prefix transient--prefix)
  (setq transient-current-command (oref transient--prefix command))
  (setq transient-current-suffixes transient--suffixes)
  (unless (transient--maybe-set-value 'export)
    (transient--history-push transient--prefix)))

(defun transient--suspend-override (&optional nohide)
  (transient--debug 'suspend-override)
  (transient--timer-cancel)
  (let ((show (transient--preserve-window-p nohide)))
    (cond ((not show)
           (transient--delete-window))
          ((and transient--prefix transient--redisplay-key)
           (setq transient--redisplay-key nil)
           (when transient--showp
             (if-let* ((win (minibuffer-selected-window)))
                 (with-selected-window win
                   (transient--show))
               (transient--show)))))
    (when (and (window-live-p transient--window)
               (and show
                    (or (not (eq show 'fixed))
                        (window-full-height-p transient--window))))
      (set-window-parameter transient--window 'window-preserved-size
                            (list (window-buffer transient--window) nil nil))))
  (transient--pop-keymap 'transient--transient-map)
  (transient--pop-keymap 'transient--redisplay-map)
  (remove-hook 'pre-command-hook  #'transient--pre-command)
  (remove-hook 'post-command-hook #'transient--post-command))

(defun transient--resume-override (&optional _ignore)
  (transient--debug 'resume-override)
  (when (window-live-p transient--window)
    (transient--fit-window-to-buffer transient--window))
  (transient--push-keymap 'transient--transient-map)
  (transient--push-keymap 'transient--redisplay-map)
  (add-hook 'pre-command-hook  #'transient--pre-command)
  (add-hook 'post-command-hook #'transient--post-command))

(defun transient--recursive-edit (fn)
  (transient--debug 'recursive-edit)
  (if (not transient--prefix)
      (funcall fn)
    (transient--suspend-override (bound-and-true-p edebug-active))
    (funcall fn) ; Already unwind protected.
    (cond ((memq this-command '(top-level abort-recursive-edit))
           (setq transient--exitp t)
           (transient--post-exit this-command)
           (transient--delete-window))
          (transient--prefix
           (transient--resume-override)))))

(defmacro transient--with-suspended-override (&rest body)
  (let ((depth (make-symbol "depth"))
        (setup (make-symbol "setup"))
        (exit  (make-symbol "exit")))
    `(if (and transient--transient-map
              (memq transient--transient-map
                    overriding-terminal-local-map))
         (let ((,depth (1+ (minibuffer-depth))) ,setup ,exit)
           (setq ,setup
                 (lambda () "@transient--with-suspended-override"
                   (transient--debug 'minibuffer-setup)
                   (remove-hook 'minibuffer-setup-hook ,setup)
                   (transient--suspend-override)))
           (setq ,exit
                 (lambda () "@transient--with-suspended-override"
                   (transient--debug 'minibuffer-exit)
                   (when (= (minibuffer-depth) ,depth)
                     (transient--resume-override))))
           (unwind-protect
               (progn
                 (add-hook 'minibuffer-setup-hook ,setup)
                 (add-hook 'minibuffer-exit-hook ,exit)
                 ,@body)
             (remove-hook 'minibuffer-setup-hook ,setup)
             (remove-hook 'minibuffer-exit-hook ,exit)))
       ,@body)))

(defun transient--wrap-command ()
  (transient--load-command-if-autoload this-command)
  (letrec
      ((command this-command)
       (suffix (transient-suffix-object this-command))
       (prefix transient--prefix)
       (advice
        (lambda (fn &rest args)
          (interactive
            (lambda (spec)
              (let ((abort t))
                (unwind-protect
                    (prog1 (let ((debugger #'transient--exit-and-debug))
                             (if-let* ((obj suffix)
                                       (grp (oref obj parent))
                                       (adv (or (oref obj advice*)
                                                (oref grp advice*))))
                                 (funcall
                                  adv #'advice-eval-interactive-spec spec)
                               (advice-eval-interactive-spec spec)))
                      (setq abort nil))
                  (when abort
                    (when-let* ((unwind (oref prefix unwind-suffix)))
                      (transient--debug 'unwind-interactive)
                      (funcall unwind command))
                    (when (symbolp command)
                      (remove-function (symbol-function command) advice))
                    (oset prefix unwind-suffix nil))))))
          (unwind-protect
              (let ((debugger #'transient--exit-and-debug))
                (if-let* ((obj suffix)
                          (grp (oref obj parent))
                          (adv (or (oref obj advice)
                                   (oref obj advice*)
                                   (oref grp advice)
                                   (oref grp advice*))))
                    (apply adv fn args)
                  (apply fn args)))
            (when-let* ((unwind (oref prefix unwind-suffix)))
              (transient--debug 'unwind-command)
              (funcall unwind command))
            (when (symbolp command)
              (remove-function (symbol-function command) advice))
            (oset prefix unwind-suffix nil)))))
    (add-function :around (if (symbolp this-command)
                              (symbol-function this-command)
                            this-command)
                  advice '((depth . -99)))))

(defun transient--premature-post-command ()
  (and (equal (this-command-keys-vector) [])
       (= (minibuffer-depth)
          (1+ transient--minibuffer-depth))
       (progn
         (transient--debug 'premature-post-command)
         (transient--suspend-override)
         (oset (or transient--prefix transient-current-prefix)
               unwind-suffix
               (if transient--exitp
                   #'transient--post-exit
                 #'transient--resume-override))
         t)))

(defun transient--post-command ()
  (unless (transient--premature-post-command)
    (transient--debug 'post-command)
    (transient--with-emergency-exit :post-command
      (cond (transient--exitp (transient--post-exit))
            ;; If `this-command' is the current transient prefix, then we
            ;; have already taken care of updating the transient buffer...
            ((and (eq this-command (oref transient--prefix command))
                  ;; ... but if `prefix-arg' is non-nil, then the values
                  ;; of `this-command' and `real-this-command' are untrue
                  ;; because `prefix-command-preserve-state' changes them.
                  ;; We cannot use `current-prefix-arg' because it is set
                  ;; too late (in `command-execute'), and if it were set
                  ;; earlier, then we likely still would not be able to
                  ;; rely on it, and `prefix-command-preserve-state-hook'
                  ;; would have to be used to record that a universal
                  ;; argument is in effect.
                  (not prefix-arg)))
            (transient--refreshp
             (transient--env-apply #'transient--refresh-transient))
            ((let ((old transient--redisplay-map)
                   (new (transient--make-redisplay-map)))
               (unless (equal old new)
                 (transient--pop-keymap 'transient--redisplay-map)
                 (setq transient--redisplay-map new)
                 (transient--push-keymap 'transient--redisplay-map))
               (transient--env-apply #'transient--redisplay)))))
    (setq transient-current-prefix nil)
    (setq transient-current-command nil)
    (setq transient-current-suffixes nil)
    (setq transient--current-suffix nil)))

(defun transient--post-exit (&optional command)
  (transient--debug 'post-exit)
  (unless (and (eq transient--exitp 'replace)
               (or transient--prefix
                   ;; The current command could act as a prefix,
                   ;; but decided not to call `transient-setup',
                   ;; or it is prevented from doing so because it
                   ;; uses the minibuffer and the user aborted
                   ;; that.
                   (prog1 nil
                     (if (let ((obj (transient-suffix-object command)))
                           (and (slot-boundp obj 'transient)
                                (oref obj transient)))
                         ;; This sub-prefix is a transient suffix;
                         ;; go back to outer prefix, by calling
                         ;; `transient--stack-pop' further down.
                         (setq transient--exitp nil)
                       (transient--stack-zap)))))
    (remove-hook 'pre-command-hook  #'transient--pre-command)
    (remove-hook 'post-command-hook #'transient--post-command)
    (advice-remove 'recursive-edit #'transient--recursive-edit))
  (let ((replace (eq transient--exitp 'replace))
        (resume (and transient--stack
                     (not (memq transient--exitp '(replace suspend))))))
    (unless (or resume replace)
      (setq transient--showp nil))
    (setq transient--exitp nil)
    (setq transient--helpp nil)
    (setq transient--editp nil)
    (setq transient--all-levels-p nil)
    (setq transient--minibuffer-depth 0)
    (run-hooks 'transient-exit-hook)
    (when command
      (setq transient-current-prefix nil)
      (setq transient-current-command nil)
      (setq transient-current-suffixes nil)
      (setq transient--current-suffix nil))
    (cond (resume (transient--stack-pop))
          ((not replace)
           (transient--quit-kludge 'disable)
           (run-hooks 'transient-post-exit-hook)))))

(defun transient--stack-push ()
  (transient--debug 'stack-push)
  (push (list (oref transient--prefix command)
              transient--layout
              transient--editp
              :value  (transient--get-extended-value)
              :return (oref transient--prefix return)
              :scope  (oref transient--prefix scope))
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
      (unless
          (or (memq this-command transient--scroll-commands)
              (and (or (memq this-command '(mouse-drag-region
                                            mouse-set-region))
                       (equal (key-description (this-command-keys-vector))
                              "<mouse-movement>"))
                   (and (eq (current-buffer) transient--buffer))))
        (transient--show))
    (when (and (numberp transient-show-popup)
               (not (zerop transient-show-popup))
               (not transient--timer))
      (transient--timer-start))
    (transient--show-hint)))

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
    (let ((inhibit-message (not (eq transient--debug 'message))))
      (if (symbolp arg)
          (message "-- %-22s (cmd: %s, event: %S, exit: %s%s)"
                   arg
                   (cond ((and (symbolp this-command) this-command))
                         ((fboundp 'help-fns-function-name)
                          (help-fns-function-name this-command))
                         ((byte-code-function-p this-command)
                          "#[...]")
                         (this-command))
                   (key-description (this-command-keys-vector))
                   transient--exitp
                   (cond ((keywordp (car args))
                          (format ", from: %s"
                                  (substring (symbol-name (car args)) 1)))
                         ((stringp (car args))
                          (concat ", " (apply #'format args)))
                         ((functionp (car args))
                          (concat ", " (apply (car args) (cdr args))))
                         ("")))
        (apply #'message arg args)))))

(defun transient--emergency-exit (&optional id)
  "Exit the current transient command after an error occurred.
When no transient is active (i.e., when `transient--prefix' is
nil) then only reset `inhibit-quit'.  Optional ID is a keyword
identifying the exit."
  (transient--debug 'emergency-exit id)
  (transient--quit-kludge 'disable)
  (when transient--prefix
    (setq transient--stack nil)
    (setq transient--exitp t)
    (transient--pre-exit)
    (transient--post-exit this-command)))

(defun transient--quit-kludge (action)
  (static-if (boundp 'redisplay-can-quit) ;Emacs 31
      action
    (pcase-exhaustive action
      ('enable
       (add-function
        :around command-error-function
        (let (unreadp)
          (lambda (orig data context fn)
            (cond ((not (eq (car data) 'quit))
                   (funcall orig data context fn)
                   (setq unreadp nil))
                  (unreadp
                   (remove-function command-error-function "inhibit-quit")
                   (funcall orig data context fn))
                  (t
                   (push ?\C-g unread-command-events)
                   (setq unreadp t)))))
        '((name . "inhibit-quit"))))
      ('disable
       (remove-function command-error-function "inhibit-quit")))))

;;;; Pre-Commands

(defun transient--call-pre-command ()
  (if-let* ((fn (transient--get-pre-command this-command
                                            (this-command-keys-vector))))
      (let ((action (funcall fn)))
        (when (eq action transient--exit)
          (setq transient--exitp (or transient--exitp t)))
        action)
    (if (let ((keys (this-command-keys-vector)))
          (eq (aref keys (1- (length keys))) ?\C-g))
        (setq this-command 'transient-noop)
      (unless (transient--edebug-command-p)
        (setq this-command 'transient-undefined)))
    transient--stay))

(defun transient--get-pre-command (&optional cmd key enforce-type)
  (or (and (not (eq enforce-type 'non-suffix))
           (symbolp cmd)
           (or (and key
                    (let ((def (lookup-key transient--predicate-map
                                           (vconcat key (list cmd)))))
                      (and (symbolp def) def)))
               (lookup-key transient--predicate-map (vector cmd))))
      (and (not (eq enforce-type 'suffix))
           (transient--resolve-pre-command
            (oref transient--prefix transient-non-suffix)
            t))))

(defun transient--resolve-pre-command (pre &optional resolve-boolean correct)
  (setq pre (cond ((booleanp pre)
                   (if resolve-boolean
                       (if pre #'transient--do-stay #'transient--do-warn)
                     pre))
                  ((string-match-p "--do-" (symbol-name pre)) pre)
                  ((let ((sym (intern (format "transient--do-%s" pre))))
                     (if (functionp sym) sym pre)))))
  (cond ((not correct) pre)
        ((and (eq pre 'transient--do-return)
              (not transient--stack))
         'transient--do-exit)
        (pre)))

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

(defun transient--do-return ()
  "Call the command after exporting variables and return to parent prefix.
If there is no parent prefix, then behave like `transient--do-exit'."
  (if (not transient--stack)
      (transient--do-exit)
    (transient--export)
    transient--exit))

(defun transient--do-exit ()
  "Call the command after exporting variables and exit the transient."
  (transient--export)
  (transient--stack-zap)
  transient--exit)

(defun transient--do-leave ()
  "Call the command without exporting variables and exit the transient."
  (transient--stack-zap)
  transient--exit)

(defun transient--do-push-button ()
  "Call the command represented by the activated button.
Use that command's pre-command to determine transient behavior."
  (if (and (mouse-event-p last-command-event)
           (not (eq (posn-window (event-start last-command-event))
                    transient--window)))
      transient--stay
    (with-selected-window transient--window
      (let ((pos (if (mouse-event-p last-command-event)
                     (posn-point (event-start last-command-event))
                   (point))))
        (setq this-command (get-text-property pos 'command))
        (setq transient--current-suffix (get-text-property pos 'suffix))))
    (transient--call-pre-command)))

(defun transient--do-recurse ()
  "Call the transient prefix command, preparing for return to outer transient.
If there is no parent prefix, then just call the command."
  (transient--do-stack))

(defun transient--do-stack ()
  "Call the transient prefix command, stacking the active transient.
Push the active transient to the transient stack."
  (transient--export)
  (transient--stack-push)
  (setq transient--exitp 'replace)
  transient--exit)

(defun transient--do-replace ()
  "Call the transient prefix command, replacing the active transient.
Do not push the active transient to the transient stack."
  (transient--export)
  (setq transient--exitp 'replace)
  transient--exit)

(defun transient--do-suspend ()
  "Suspend the active transient, saving the transient stack."
  ;; Export so that `transient-describe' instances can use
  ;; `transient-suffix-object' to get their respective object.
  (transient--export)
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
        (prefix-arg
         transient--stay)
        (transient--exit)))

(defun transient--do-quit-all ()
  "Exit all transients without saving the transient stack."
  (transient--stack-zap)
  transient--exit)

(defun transient--do-move ()
  "Call the command if `transient-enable-popup-navigation' is non-nil.
In that case behave like `transient--do-stay', otherwise similar
to `transient--do-warn'."
  (unless transient-enable-popup-navigation
    (setq this-command 'transient-inhibit-move))
  transient--stay)

(defun transient--do-minus ()
  "Call `negative-argument' or pivot to `transient-update'.
If `negative-argument' is invoked using \"-\" then preserve the
prefix argument and pivot to `transient-update'."
  (when (equal (this-command-keys) "-")
    (setq this-command 'transient-update))
  transient--stay)

(put 'transient--do-stay       'transient-face 'transient-key-stay)
(put 'transient--do-noop       'transient-face 'transient-key-noop)
(put 'transient--do-warn       'transient-face 'transient-key-noop)
(put 'transient--do-warn-inapt 'transient-face 'transient-key-noop)
(put 'transient--do-call       'transient-face 'transient-key-stay)
(put 'transient--do-return     'transient-face 'transient-key-return)
(put 'transient--do-exit       'transient-face 'transient-key-exit)
(put 'transient--do-leave      'transient-face 'transient-key-exit)

(put 'transient--do-recurse    'transient-face 'transient-key-recurse)
(put 'transient--do-stack      'transient-face 'transient-key-stack)
(put 'transient--do-replace    'transient-face 'transient-key-exit)
(put 'transient--do-suspend    'transient-face 'transient-key-exit)

(put 'transient--do-quit-one   'transient-face 'transient-key-return)
(put 'transient--do-quit-all   'transient-face 'transient-key-exit)
(put 'transient--do-move       'transient-face 'transient-key-stay)
(put 'transient--do-minus      'transient-face 'transient-key-stay)

;;;; Commands
;;;; Noop

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
  (message "%s: `%s' (Use `%s' to abort, `%s' for help)%s"
           msg
           (propertize (key-description (this-single-command-keys))
                       'face 'font-lock-warning-face)
           (propertize "C-g" 'face 'transient-key)
           (propertize "?"   'face 'transient-key)
           ;; `this-command' is `transient-undefined' or `transient-inapt'.
           ;; Show the command (`this-original-command') the user actually
           ;; tried to invoke.
           (if-let* ((cmd (or (ignore-errors (symbol-name this-original-command))
                              (ignore-errors (symbol-name this-command)))))
               (format " [%s]" (propertize cmd 'face 'font-lock-warning-face))
             ""))
  (unless (and transient--transient-map
               (memq transient--transient-map overriding-terminal-local-map))
    (let ((transient--prefix (or transient--prefix 'sic)))
      (transient--emergency-exit))
    (view-lossage)
    (other-window 1)
    (display-warning 'transient "Inconsistent transient state detected.
This should never happen.
Please open an issue and post the shown command log." :error)))

(defun transient-inhibit-move ()
  "Warn the user that menu navigation is disabled."
  (interactive)
  (message "To enable use of `%s', please customize `%s'"
           this-original-command
           'transient-enable-popup-navigation))

;;;; Core

(defun transient-quit-all ()
  "Exit all transients without saving the transient stack."
  (interactive))

(defun transient-quit-one ()
  "Exit the current transients, returning to outer transient, if any."
  (interactive))

(defun transient-quit-seq ()
  "Abort the current incomplete key sequence."
  (interactive))

(defun transient-update ()
  "Redraw the transient's state in the menu buffer."
  (interactive)
  (setq prefix-arg current-prefix-arg))

(defun transient-show ()
  "Show the transient's state in the menu buffer."
  (interactive)
  (setq transient--showp t))

(defun transient-push-button ()
  "Invoke the suffix command represented by this button."
  (interactive))

;;;; Suspend

(defun transient-suspend ()
  "Suspend the current transient.
It can later be resumed using `transient-resume', while no other
transient is active."
  (interactive))

(define-minor-mode transient-resume-mode
  "Auxiliary minor-mode used to resume a transient after viewing help.")

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

;;;; Help

(defun transient-help (&optional interactivep)
  "Show help for the active transient or one of its suffixes.
\n(fn)"
  (interactive (list t))
  (cond
    (interactivep
     (setq transient--helpp t))
    ((lookup-key transient--transient-map
                 (this-single-command-raw-keys))
     (setq transient--helpp nil)
     (with-demoted-errors "transient-help: %S"
       (transient--display-help #'transient-show-help
                                (if (eq this-original-command 'transient-help)
                                    transient--prefix
                                  (or (transient-suffix-object)
                                      this-original-command)))))))

(transient-define-suffix transient-describe ()
  "From a transient menu, describe something in another buffer.

This command can be bound multiple times to describe different targets.
Each binding must specify the thing it describes, be setting the value
of its `target' slot, using the keyword argument `:='.

The `helper' slot specifies the low-level function used to describe the
target, and can be omitted, in which case `transient--describe-function'
is used for a symbol, `transient--show-manual' is used for a string
beginning with a parenthesis, and `transient--show-manpage' is used for
any other string.

For example:
  [(\"e\" \"about emacs\" transient-describe := \"(emacs)\")
   (\"g\" \"about git\"   transient-describe := \"git\")]"
  :class 'transient-describe-target
  (interactive)
  (with-slots (helper target) (transient-suffix-object)
    (transient--display-help helper target)))

;;;; Level

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
                        (progn
                          (transient--show)
                          (string-to-number
                           (transient--read-number-N
                            (format "Set level for `%s': " command)
                            nil nil (not (eq command prefix)))))))))))
  (cond
    ((not command)
     (setq transient--editp t)
     (transient-setup))
    (level
     (let* ((prefix (oref transient--prefix command))
            (alist (alist-get prefix transient-levels))
            (akey command))
       (cond ((eq command prefix)
              (oset transient--prefix level level)
              (setq akey t))
             (t
              (oset (transient-suffix-object command) level level)
              (when (cdr (cl-remove-if-not (lambda (obj)
                                             (eq (oref obj command) command))
                                           transient--suffixes))
                (setq akey (cons command (this-command-keys))))))
       (setf (alist-get akey alist) level)
       (setf (alist-get prefix transient-levels) alist))
     (transient-save-levels)
     (transient--show))
    (t
     (transient-undefined))))

(transient-define-suffix transient-toggle-level-limit ()
  "Toggle whether to temporarily display suffixes on all levels."
  :description
  (lambda ()
    (cond
      (transient--all-levels-p
       (format "Hide suffix %s"
               (propertize
                (format "levels > %s" (oref (transient-prefix-object) level))
                'face 'transient-higher-level)))
      ("Show all suffix levels")))
  :transient t
  (interactive)
  (setq transient--all-levels-p (not transient--all-levels-p))
  (setq transient--refreshp t))

;;;; Value

(defun transient-set ()
  "Set active transient's value for this Emacs session."
  (interactive)
  (transient-set-value (transient-prefix-object)))

(defalias 'transient-set-and-exit #'transient-set
  "Set active transient's value for this Emacs session and exit.")

(defun transient-save ()
  "Save active transient's value for this and future Emacs sessions."
  (interactive)
  (transient-save-value (transient-prefix-object)))

(defalias 'transient-save-and-exit #'transient-save
  "Save active transient's value for this and future Emacs sessions and exit.")

(defun transient-reset ()
  "Clear the set and saved values of the active transient."
  (interactive)
  (transient-reset-value (transient-prefix-object)))

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

(transient-define-suffix transient-preset ()
  "Put this preset into action."
  :class transient-value-preset
  (interactive)
  (transient-prefix-set (oref (transient-suffix-object) set)))

;;;; Auxiliary

(transient-define-suffix transient-toggle-common ()
  "Toggle whether common commands are permanently shown."
  :transient t
  :description (lambda ()
                 (if transient-show-common-commands
                     "Hide common commands"
                   "Show common permanently"))
  (interactive)
  (setq transient-show-common-commands (not transient-show-common-commands)))

(transient-define-suffix transient-toggle-docstrings (&optional permanent)
  "Toggle whether to show docstrings instead of suffix descriptions.

By default this is only enabled temporarily for the current transient
menu invocation.  With a prefix argument, enable this until explicitly
disabled again.

Infix arguments are not affected by this, because otherwise many menus
would likely become unreadable.  To make this command available in all
menus, bind it in `transient-map'.  `transient-show-docstring-format'
controls how the docstrings are displayed and whether descriptions are
also displayed."
  :transient t
  (interactive (list current-prefix-arg))
  (setq transient--docsp (if permanent 'permanent (not transient--docsp))))

(defun transient-toggle-debug ()
  "Toggle debugging statements for transient commands."
  (interactive)
  (setq transient--debug (not transient--debug))
  (message "Debugging transient %s"
           (if transient--debug "enabled" "disabled")))

(defun transient-copy-menu-text ()
  "Copy the contents of the menu buffer to the kill ring.
To make this available in all menus, bind it in `transient-map'"
  (interactive)
  (transient--show)
  (with-current-buffer (get-buffer transient--buffer-name)
    (copy-region-as-kill (point-min) (point-max))))

(transient-define-suffix transient-echo-arguments (arguments)
  "Show the transient's active ARGUMENTS in the echo area.
Intended for use in prefixes used for demonstration purposes,
such as when suggesting a new feature or reporting an issue."
  :transient t
  :description "Echo arguments"
  :key "x"
  (interactive (list (transient-args transient-current-command)))
  (if (seq-every-p #'stringp arguments)
      (message "%s: %s" (key-description (this-command-keys))
               (mapconcat (lambda (arg)
                            (propertize (if (string-match-p " " arg)
                                            (format "%S" arg)
                                          arg)
                                        'face 'transient-argument))
                          arguments " "))
    (message "%s: %S" (key-description (this-command-keys)) arguments)))

;;;; Value
;;;; Init

(cl-defgeneric transient-init-value (obj)
  "Set the initial value of the prefix or suffix object OBJ.

This function is called for all prefix and suffix commands.

Third-party subclasses of `transient-infix' must implement a primary
method.")

(cl-defmethod transient-init-value :around ((obj transient-prefix))
  "If bound, use the value returned by OBJ' `init-value' function.
If the value of OBJ's `init-value' is non-nil, call that function to
determine the value.  Otherwise call the primary method according to
OBJ's class."
  (if (slot-boundp obj 'init-value)
      (funcall (oref obj init-value) obj)
    (cl-call-next-method obj)))

(cl-defmethod transient-init-value :around ((obj transient-infix))
  "If bound, use the value returned by OBJ's `init-value' function.
If the value of OBJ's `init-value' is non-nil, call that function to
determine the value.  Otherwise call the primary method according to
OBJ's class."
  (if (slot-boundp obj 'init-value)
      (funcall (oref obj init-value) obj)
    (cl-call-next-method obj)))

(cl-defmethod transient-init-value ((obj transient-prefix))
  "Set OBJ's initial value to the set, saved or default value.
Use `transient-default-value' to determine the default value."
  (if (slot-boundp obj 'value)
      ;; Already set because the live object is cloned from
      ;; the prototype, were the set (if any) value is stored.
      (oref obj value)
    (oset obj value
          (if-let* ((saved (assq (oref obj command) transient-values)))
              (cdr saved)
            (transient-default-value obj)))))

(cl-defmethod transient-init-value ((obj transient-suffix))
  "Non-infix suffixes usually don't have a value.
Call `transient-default-value' but because that is a noop for
`transient-suffix', this function is effectively also a noop."
  (let ((value (transient-default-value obj)))
    (unless (eq value eieio--unbound)
      (oset obj value value))))

(cl-defmethod transient-init-value ((obj transient-argument))
  "Extract OBJ's value from the value of the prefix object."
  (oset obj value
        (let ((value (oref transient--prefix value))
              (argument (and (slot-boundp obj 'argument)
                             (oref obj argument)))
              (multi-value (oref obj multi-value))
              (case-fold-search nil)
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
  "Extract OBJ's value from the value of the prefix object."
  (oset obj value
        (car (member (oref obj argument)
                     (oref transient--prefix value)))))

;;;; Default

(cl-defgeneric transient-default-value (obj)
  "Return the default value.")

(cl-defmethod transient-default-value ((obj transient-prefix))
  "Return the default value as specified by the `default-value' slot.
If the value of the `default-value' slot is a function, call it to
determine the value.  If the slot's value isn't a function, return
that.  If the slot is unbound, return nil."
  (if-let* ((default (and (slot-boundp obj 'default-value)
                          (oref obj default-value))))
      (if (functionp default)
          (funcall default)
        default)
    nil))

(cl-defmethod transient-default-value ((_   transient-suffix))
  "Return `eieio--unbound' to indicate that there is no default value.
Doing so causes `transient-init-value' to skip setting the `value' slot."
  eieio--unbound)

;;;; Read

(cl-defgeneric transient-infix-read (obj)
  "Determine the new value of the infix object OBJ.

This function merely determines the value; `transient-infix-set'
is used to actually store the new value in the object.

For most infix classes this is done by reading a value from the
user using the reader specified by the `reader' slot (using the
method for `transient-infix', described below).

For some infix classes the value is changed without reading
anything in the minibuffer, i.e., the mere act of invoking the
infix command determines what the new value should be, based
on the previous value.")

(cl-defmethod transient-infix-read :around ((obj transient-infix))
  "Refresh the transient buffer and call the next method.

Also wrap `cl-call-next-method' with two macros:
- `transient--with-suspended-override' allows use of minibuffer.
- `transient--with-emergency-exit' arranges for the transient to
  be exited in case of an error."
  (transient--show)
  (transient--with-emergency-exit :infix-read
    (transient--with-suspended-override
     (cl-call-next-method obj))))

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
simple method that does not handle history.  (E.g., for a command
line switch the only possible values are \"use it\" and \"don't use
it\", in which case it is pointless to preserve history.)"
  (with-slots (value multi-value always-read allow-empty choices) obj
    (if (and value
             (not multi-value)
             (not always-read)
             transient--prefix)
        (oset obj value nil)
      (let* ((enable-recursive-minibuffers t)
             (reader (oref obj reader))
             (choices (if (functionp choices) (funcall choices) choices))
             (prompt (transient-prompt obj))
             (value (if multi-value (string-join value ",") value))
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
                ((read-string prompt initial-input history)))))
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
    (if-let* ((value (oref obj value)))
        (cadr (member value choices))
      (car choices))))

(cl-defmethod transient-infix-read ((command symbol))
  "Elsewhere use the reader of the infix command COMMAND.
Use this if you want to share an infix's history with a regular
stand-alone command."
  (if-let* ((obj (transient--suffix-prototype command)))
      (cl-letf (((symbol-function #'transient--show) #'ignore))
        (transient-infix-read obj))
    (error "Not a suffix command: `%s'" command)))

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
  (cond*
    ((bind-and* (prompt (oref obj prompt)))
     (let ((prompt (if (functionp prompt)
                       (funcall prompt obj)
                     prompt)))
       (if (stringp prompt)
           prompt
         "[BUG: invalid prompt]: ")))
    ((bind-and*
      (name (or (and (slot-boundp obj 'argument) (oref obj argument))
                (and (slot-boundp obj 'variable) (oref obj variable)))))
     (if (and (stringp name)
              (string-suffix-p "=" name))
         name
       (format "%s: " name)))
    ("[BUG: no prompt]: ")))

;;;; Set

(cl-defgeneric transient-infix-set (obj value)
  "Set the value of infix object OBJ to VALUE.")

(cl-defmethod transient-infix-set ((obj transient-infix) value)
  "Set the value of infix object OBJ to VALUE."
  (oset obj value value))

(cl-defmethod transient-infix-set :after ((obj transient-argument) value)
  "Unset incompatible infix arguments."
  (when-let* ((_ value)
              (val (transient-infix-value obj))
              (arg (if (slot-boundp obj 'argument)
                       (oref obj argument)
                     (oref obj argument-format)))
              (spec (oref transient--prefix incompatible))
              (filter (lambda (x rule)
                        (and (member x rule)
                             (remove x rule))))
              (incomp (nconc
                       (mapcan (apply-partially filter arg) spec)
                       (and (not (equal val arg))
                            (mapcan (apply-partially filter val) spec)))))
    (dolist (obj transient--suffixes)
      (when-let* ((_(cl-typep obj 'transient-argument))
                  (val (transient-infix-value obj))
                  (arg (if (slot-boundp obj 'argument)
                           (oref obj argument)
                         (oref obj argument-format)))
                  (_(if (equal val arg)
                        (member arg incomp)
                      (or (member val incomp)
                          (member arg incomp)))))
        (transient-infix-set obj nil)))))

(defun transient-prefix-set (value)
  "Set the value of the active transient prefix to VALUE.
Intended for use by transient suffix commands."
  (oset transient--prefix value value)
  (setq transient--refreshp 'updated-value))

(cl-defgeneric transient-set-value (obj)
  "Persist the value of the transient prefix OBJ.
Only intended for use by `transient-set'.
See also `transient-prefix-set'.")

(cl-defmethod transient-set-value ((obj transient-prefix))
  (let ((value (transient--get-savable-value)))
    (oset (oref obj prototype) value value)
    (transient--history-push obj value)))

(defun transient--maybe-set-value (event)
  "Maybe set the value, subject to EVENT and the `remember-value' slot."
  (let* ((event (if (and (eq event 'exit)
                         (memq this-command transient--quit-commands))
                    'quit
                  event))
         (spec (oref transient--prefix remember-value))
         (spec (cond ((listp spec) spec)
                     ((memq spec '(export exit quit))
                      (list spec))
                     ((boundp spec)
                      (symbol-value spec)))))
    (and (memq event spec)
         (prog1 t
           (if (memq 'save spec)
               (transient-save-value transient--prefix)
             (transient-set-value transient--prefix))))))

;;;; Save

(cl-defgeneric transient-save-value (obj)
  "Save the value of the transient prefix OBJ.")

(cl-defmethod transient-save-value ((obj transient-prefix))
  (let ((value (transient--get-savable-value)))
    (oset (oref obj prototype) value value)
    (setf (alist-get (oref obj command) transient-values) value)
    (transient-save-values)
    (transient--history-push obj value)))

;;;; Reset

(cl-defgeneric transient-reset-value (obj)
  "Clear the set and saved values of the transient prefix OBJ.")

(cl-defmethod transient-reset-value ((obj transient-prefix))
  (let ((value (transient-default-value obj)))
    (oset obj value value)
    (oset (oref obj prototype) value value)
    (setf (alist-get (oref obj command) transient-values nil 'remove) nil)
    (transient-save-values)
    (transient--history-push obj value))
  (mapc #'transient-init-value transient--suffixes))

;;;; Get

(defun transient-args (prefix)
  "Return the value of the transient prefix command PREFIX.

If the current command was invoked from the transient prefix command
PREFIX, then return the active infix arguments.  If the current command
was not invoked from PREFIX, then return the set, saved or default value
for PREFIX.

PREFIX may also be a list of prefixes.  If no prefix is active, the
fallback value of the first of these prefixes is used.

The generic function `transient-prefix-value' is used to determine the
returned value.

This function is intended to be used by suffix commands, whether they
are invoked from a menu or not.  It is not intended to be used when
setting up a menu and its suffixes, in which case `transient-get-value'
should be used."
  (when (listp prefix)
    (setq prefix (car (or (memq transient-current-command prefix) prefix))))
  (if-let* ((obj (get prefix 'transient--prefix)))
      ;; This OBJ is only used for dispatch purposes; see below.
      (transient-prefix-value obj)
    (error "Not a transient prefix: %s" prefix)))

(cl-defgeneric transient-prefix-value (obj)
  "Return a list of the values of the suffixes of the specified prefix.

OBJ is a prototype object and is only used to select the appropriate
method of this generic function.  Transient itself only provides one
such method, which should be suitable for most prefixes.

This function is only intended to be used by `transient-args'.  It is
not defined as an internal function because third-party packages may
define their own methods.  That does not mean that it would be a good
idea to call it for any other purpose.")

(cl-defmethod transient-prefix-value ((obj transient-prefix))
  "Return a list of the values of the suffixes of the specified prefix.

OBJ is a prototype object.  This method does not return the value of
that object.  Instead it extracts the name of the respective command
from the object and uses that to collect the current values from the
suffixes of the prefix from which the current command was invoked.
If the current command was not invoked from the identified prefix,
then this method returns the set, save or default value, as described
for `transient-args'.

This method uses `transient-suffixes' (which see) to determine the
suffix objects and then extracts the value(s) from those objects."
  (mapcan (lambda (obj)
            (and (not (oref obj inactive))
                 (not (oref obj inapt))
                 (transient--get-wrapped-value obj)))
          (transient-suffixes (oref obj command))))

(defun transient-suffixes (prefix)
  "Return the suffix objects of the transient prefix command PREFIX.

If PREFIX is not the current prefix, initialize the suffixes so that
they can be returned.  That does not cause the menu to be displayed."
  (if (eq transient-current-command prefix)
      transient-current-suffixes
    (let ((transient--prefix (transient--init-prefix prefix)))
      (transient--flatten-suffixes
       (transient--init-suffixes prefix)))))

(defun transient-get-value ()
  "Return the value of the extant prefix.

This function is intended to be used when setting up a menu and its
suffixes.  It is not intended to be used when a suffix command is
invoked, whether from a menu or not, in which case `transient-args'
should be used."
  (transient--with-emergency-exit :get-value
    (mapcan (lambda (obj)
              (and (not (oref obj inactive))
                   (not (oref obj inapt))
                   (transient--get-wrapped-value obj)))
            transient--suffixes)))

(defun transient--get-extended-value ()
  "Return the extended value of the extant prefix.

Unlike `transient-get-value' also include the values of inactive and
inapt arguments.  This function is mainly intended for internal use.
It is used to preserve the full value when a menu is being refreshed,
including the presently ineffective parts."
  (transient--with-emergency-exit :get-value
    (mapcan #'transient--get-wrapped-value transient--suffixes)))

(defun transient--get-savable-value ()
  "Return the value of the extant prefix, excluding unsavable parts.

This function is only intended for internal use.  It is used to save
the value."
  (transient--with-emergency-exit :get-savable-value
    (mapcan (lambda (obj)
              (and (not (and (slot-exists-p obj 'unsavable)
                             (oref obj unsavable)))
                   (transient--get-wrapped-value obj)))
            (or transient--suffixes transient-current-suffixes))))

(defun transient--get-wrapped-value (obj)
  "Return a list of the value(s) of suffix object OBJ.

Internally a suffix only ever has one value, stored in its `value'
slot, but callers of `transient-args' wish to treat the values of
certain suffixes as multiple values.  That translation is handled
here.  The object's `multi-value' slot specifies whether and how
to interpret the `value' as multiple values."
  (and-let* ((value (transient-infix-value obj)))
    (pcase-exhaustive (and (slot-exists-p obj 'multi-value)
                           (oref obj multi-value))
      ('nil          (list value))
      ((or 't 'rest) (list value))
      ('repeat       value))))

(cl-defgeneric transient-infix-value (obj)
  "Return the value of the suffix object OBJ.

By default this function is involved when determining the prefix's
overall value, returned by `transient-args' (which see),  so that
the invoked suffix command can use that.

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
  (and-let* ((value (oref obj value)))
    (let ((arg (oref obj argument)))
      (pcase-exhaustive (oref obj multi-value)
        ('nil          (concat arg value))
        ((or 't 'rest) (cons arg value))
        ('repeat       (mapcar (lambda (v) (concat arg v)) value))))))

(cl-defmethod transient-infix-value ((_   transient-variable))
  "Return nil, which means \"no value\".

Setting the value of a variable is done by, well, setting the
value of the variable.  I.e., this is a side-effect and does
not contribute to the value of the transient."
  nil)

;;;; Utilities

(defun transient-arg-value (arg args)
  "Return the value of ARG as it appears in ARGS.

For a switch return a boolean.  For an option return the value as
a string, using the empty string for the empty value, or nil if
the option does not appear in ARGS.

Append \"=\ to ARG to indicate that it is an option."
  (save-match-data
    (cond*
      ((member arg args) t)
      ((bind-and*
        (_(string-suffix-p "=" arg))
        (match (let ((case-fold-search nil)
                     (re (format "\\`%s\\(?:=\\(.+\\)\\)?\\'"
                                 (substring arg 0 -1))))
                 (cl-find-if (lambda (a)
                               (and (stringp a)
                                    (string-match re a)))
                             args))))
       (match-string 1 match)))))

;;;; Return

(defun transient-init-return (obj)
  (when-let* ((_ transient--stack)
              (command (oref obj command))
              (suffix-obj (transient-suffix-object command))
              (_(memq (if (slot-boundp suffix-obj 'transient)
                          (oref suffix-obj transient)
                        (oref transient-current-prefix transient-suffix))
                      (list t 'recurse #'transient--do-recurse))))
    (oset obj return t)))

;;;; Scope
;;;; Init

(cl-defgeneric transient-init-scope (obj)
  "Set the scope of the prefix or suffix object OBJ.

The scope is actually a property of the transient prefix, not of
individual suffixes.  However it is possible to invoke a suffix
command directly instead of from a transient.  In that case, if
the suffix expects a scope, then it has to determine that itself
and store it in its `scope' slot.

This function is called for all prefix and suffix commands, but
unless a concrete method is implemented, this falls through to
a default implementation, which is a noop.")

(cl-defmethod transient-init-scope ((_   transient-prefix))
  "Noop." nil)

(cl-defmethod transient-init-scope ((_   transient-suffix))
  "Noop." nil)

;;;; Get

(defun transient-scope (&optional prefixes classes)
  "Return the scope of the active or current transient prefix command.

If optional PREFIXES and CLASSES are both nil, return the scope of
the prefix currently being setup, making this variation useful, e.g.,
in `:if*' predicates.  If no prefix is being setup, but the current
command was invoked from some prefix, then return the scope of that.

If PREFIXES is non-nil, it must be a prefix command or a list of such
commands.  If CLASSES is non-nil, it must be a prefix class or a list
of such classes.  When this function is called from the body or the
`interactive' form of a suffix command, PREFIXES and/or CLASSES should
be non-nil.  If either is non-nil, try the following in order:

- If the current suffix command was invoked from a prefix, which
  appears in PREFIXES, return the scope of that prefix.

- If the current suffix command was invoked from a prefix, and its
  class derives from one of the CLASSES, return the scope of that
  prefix.

- If a prefix is being setup and it appears in PREFIXES, return its
  scope.

- If a prefix is being setup and its class derives from one of the
  CLASSES, return its scope.

- Finally try to return the default scope of the first command in
  PREFIXES.  This only works if that slot is set in the respective
  class definition or using its `transient-init-scope' method.

If no prefix matches, return nil."
  (cond*
    ((or prefixes classes)
     (let* ((prefixes (ensure-list prefixes))
            (type (if (symbolp classes) classes (cons 'or classes)))
            (match (lambda (obj)
                     (and obj
                          (or (memq (oref obj command) prefixes)
                              (cl-typep obj type))
                          obj))))
       (cond*
         ((bind-and* (obj (or (funcall match transient-current-prefix)
                              (funcall match transient--prefix))))
          (oref obj scope))
         ((get (car prefixes) 'transient--prefix)
          (oref (transient--init-prefix (car prefixes)) scope)))))
    ((bind-and* (obj (transient-prefix-object)))
     (oref obj scope))))

;;;; History

(cl-defgeneric transient--history-key (obj)
  "Return OBJ's history key.")

(cl-defmethod transient--history-key ((obj transient-prefix))
  "If the value of the `history-key' slot is non-nil, return that.
Otherwise return the value of the `command' slot."
  (or (oref obj history-key)
      (oref obj command)))

(cl-defgeneric transient--history-push (obj value)
  "Push VALUE to OBJ's entry in `transient-history'.")

(cl-defmethod transient--history-push
  ((obj transient-prefix)
   &optional (value (transient--get-savable-value)))
  (let ((key (transient--history-key obj)))
    (setf (alist-get key transient-history)
          (cons value (delete value (alist-get key transient-history))))))

(cl-defgeneric transient--history-init (obj)
  "Initialize OBJ's `history' slot.
This is the transient-wide history; many individual infixes also
have a history of their own.")

(cl-defmethod transient--history-init ((obj transient-prefix))
  "Initialize OBJ's `history' slot from the variable `transient-history'."
  (oset obj history
        (let ((val (transient--get-extended-value)))
          (cons val (delete val (alist-get (transient--history-key obj)
                                           transient-history))))))

;;;; Display

(defun transient--show-hint ()
  (let ((message-log-max nil))
    (message "%s" (transient--format-hint))))

(defun transient--show ()
  (transient--timer-cancel)
  (setq transient--showp t)
  (let ((transient--shadowed-buffer (current-buffer))
        (setup (not (get-buffer transient--buffer-name)))
        (focus nil))
    (setq transient--buffer (get-buffer-create transient--buffer-name))
    (with-current-buffer transient--buffer
      (when transient-enable-popup-navigation
        (setq focus (or (button-get (point) 'command)
                        (and (not (bobp))
                             (button-get (1- (point)) 'command))
                        (transient--heading-at-point))))
      (erase-buffer)
      (transient--insert-menu setup))
    (unless (window-live-p transient--window)
      (setq transient--window
            (display-buffer transient--buffer
                            (transient--display-action)))
      (with-selected-window transient--window
        (set-window-parameter nil 'prev--no-other-window
                              (window-parameter nil 'no-other-window))))
    (when (window-live-p transient--window)
      (with-selected-window transient--window
        (set-window-parameter nil 'no-other-window t)
        (goto-char (point-min))
        (when transient-enable-popup-navigation
          (transient--goto-button focus))
        (transient--fit-window-to-buffer transient--window)))))

(defun transient--display-action ()
  (let ((action
         (cond ((oref transient--prefix display-action))
               ((memq 'display-buffer-full-frame
                      (ensure-list (car transient-display-buffer-action)))
                (user-error "%s disallowed in %s"
                            'display-buffer-full-frame
                            'transient-display-buffer-action))
               (transient-display-buffer-action))))
    (when (and (assq 'pop-up-frame-parameters (cdr action))
               (fboundp 'buffer-line-statistics)) ; Emacs >= 28.1
      (setq action (copy-tree action))
      (pcase-let ((`(,height ,width)
                   (buffer-line-statistics transient--buffer))
                  (params (assq 'pop-up-frame-parameters (cdr action))))
        (setf (alist-get 'height params) height)
        (setf (alist-get 'width params)
              (max width (or transient-minimal-frame-width 0)))))
    action))

(defun transient--fit-window-to-buffer (window)
  (set-window-parameter window 'window-preserved-size nil)
  (let ((fit-window-to-buffer-horizontally t)
        (window-resize-pixelwise t)
        (window-size-fixed nil))
    (cond ((not (window-parent window))
           (fit-frame-to-buffer (window-frame window) nil nil nil
                                transient-minimal-frame-width))
          ((eq (car (window-parameter window 'quit-restore)) 'other)
           ;; Grow but never shrink window that previously displayed
           ;; another buffer and is going to display that again.
           (fit-window-to-buffer window nil (window-height window)))
          ((fit-window-to-buffer window nil 1))))
  (set-window-parameter window 'window-preserved-size
                        (list (window-buffer window)
                              (window-body-width window t)
                              (window-body-height window t))))

;;;; Delete

(defun transient--delete-window ()
  (when (window-live-p transient--window)
    (let ((win transient--window)
          (remain-in-minibuffer-window
           (and (minibuffer-selected-window)
                (selected-window))))
      (cond
        ((eq (car (window-parameter win 'quit-restore)) 'other)
         ;; Window used to display another buffer.
         (set-window-parameter win 'no-other-window
                               (window-parameter win 'prev--no-other-window))
         (set-window-parameter win 'prev--no-other-window nil))
        ((with-demoted-errors "Error while exiting transient: %S"
           (if (window-parent win)
               (delete-window win)
             (delete-frame (window-frame win) t)))))
      (when remain-in-minibuffer-window
        (select-window remain-in-minibuffer-window))))
  (when (buffer-live-p transient--buffer)
    (kill-buffer transient--buffer))
  (setq transient--buffer nil))

(defun transient--preserve-window-p (&optional nohide)
  (let ((show (if nohide 'fixed transient-show-during-minibuffer-read)))
    (when (and (integerp show)
               (window-live-p transient--window)
               (< (frame-height (window-frame transient--window))
                  (+ (abs show)
                     (window-height transient--window))))
      (setq show (natnump show)))
    show))

;;;; Format

(defun transient--format-hint ()
  (if (and transient-show-popup (<= transient-show-popup 0))
      (format "%s-" (key-description (this-command-keys)))
    (format
     "%s- [%s] %s"
     (key-description (this-command-keys))
     (oref transient--prefix command)
     (mapconcat
      #'identity
      (sort
       (mapcan
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
      (propertize "|" 'face 'transient-delimiter)))))

(defun transient--insert-menu (setup)
  (when setup
    (when transient-force-fixed-pitch
      (transient--force-fixed-pitch))
    (when (bound-and-true-p tab-line-format)
      (setq tab-line-format nil))
    (setq header-line-format nil)
    (setq mode-line-format
          (let ((format (transient--mode-line-format)))
            (if (or (natnump format) (eq format 'line)) nil format)))
    (setq mode-line-buffer-identification
          (symbol-name (oref transient--prefix command)))
    (if transient-enable-popup-navigation
        (setq-local cursor-in-non-selected-windows 'box)
      (setq cursor-type nil))
    (setq display-line-numbers nil)
    (setq show-trailing-whitespace nil)
    (run-hooks 'transient-setup-buffer-hook))
  (transient--insert-groups)
  (when (or transient--helpp transient--editp)
    (transient--insert-help))
  (when-let* ((line (transient--separator-line)))
    (insert line)))

(defun transient--mode-line-format ()
  (if (slot-boundp transient--prefix 'mode-line-format)
      (oref transient--prefix mode-line-format)
    transient-mode-line-format))

(defun transient--separator-line ()
  (and-let* ((format (transient--mode-line-format))
             (height (cond ((not window-system) nil)
                           ((natnump format) format)
                           ((eq format 'line) 1)))
             (face `(:background ,(transient--prefix-color) :extend t)))
    (concat (propertize "__" 'face face 'display `(space :height (,height)))
            (propertize "\n" 'face face 'line-height t))))

(defun transient--prefix-color ()
  (or (face-foreground (transient--key-face nil nil 'non-suffix) nil t)
      "#gray60"))

(defmacro transient-with-shadowed-buffer (&rest body)
  "While in the transient buffer, temporarily make the shadowed buffer current."
  (declare (indent 0) (debug t))
  `(with-current-buffer (or transient--shadowed-buffer (current-buffer))
     ,@body))

(defun transient--insert-groups ()
  (let ((groups (mapcan (lambda (group)
                          (let ((hide (oref group hide)))
                            (and (not (and (functionp hide)
                                           (transient-with-shadowed-buffer
                                             (funcall hide))))
                                 (list group))))
                        transient--layout)))
    (while-let ((group (pop groups)))
      (transient--insert-group group)
      (when groups
        (insert ?\n)))))

(defun transient--active-suffixes (group)
  (seq-remove (lambda (suffix)
                (and (cl-typep suffix 'transient-suffix)
                     (oref suffix inactive)))
              (oref group suffixes)))

(defvar transient--max-group-level 1)

(cl-defgeneric transient--insert-group (group)
  "Format GROUP and its elements and insert the result.")

(cl-defmethod transient--insert-group :around ((group transient-group)
                                               &optional _)
  "Insert GROUP's description, if any."
  (when-let* ((desc (transient-with-shadowed-buffer
                      (transient-format-description group))))
    (insert desc ?\n))
  (let ((transient--max-group-level
         (max (oref group level) transient--max-group-level))
        (transient--pending-group group))
    (cl-call-next-method group)))

(cl-defmethod transient--insert-group ((group transient-row))
  (transient--maybe-pad-keys group)
  (dolist (suffix (transient--active-suffixes group))
    (insert (transient-with-shadowed-buffer (transient-format suffix)))
    (insert "   "))
  (insert ?\n))

(cl-defmethod transient--insert-group ((group transient-column)
                                       &optional skip-empty)
  (transient--maybe-pad-keys group)
  (dolist (suffix (transient--active-suffixes group))
    (let ((str (transient-with-shadowed-buffer (transient-format suffix))))
      (unless (and (not skip-empty) (equal str ""))
        (insert str)
        (unless (string-match-p ".\n\\'" str)
          (insert ?\n))))))

(cl-defmethod transient--insert-group ((group transient-columns))
  (if (or transient-force-single-column transient--docsp)
      (dolist (group (oref group suffixes))
        (transient--insert-group group t))
    (let* ((columns
            (mapcar
             (lambda (column)
               (transient--maybe-pad-keys column group)
               (transient-with-shadowed-buffer
                 `(,@(and-let* ((desc (transient-format-description column)))
                       (list desc))
                   ,@(let ((transient--pending-group column))
                       (mapcar #'transient-format
                               (transient--active-suffixes column))))))
             (oref group suffixes)))
           (stops (transient--column-stops columns)))
      (dolist (row (apply #'transient--mapn #'list columns))
        (let ((stops stops))
          (dolist (cell row)
            (let ((stop (pop stops)))
              (when cell
                (transient--align-to stop)
                (insert cell)))))
        (insert ?\n)))))

(cl-defmethod transient--insert-group ((group transient-subgroups))
  (let ((subgroups (oref group suffixes)))
    (while-let ((subgroup (pop subgroups)))
      (transient--maybe-pad-keys subgroup group)
      (transient--insert-group subgroup)
      (when subgroups
        (insert ?\n)))))

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

(cl-defmethod transient-format :around ((obj transient-suffix))
  "Add additional formatting if appropriate.
When reading user input for this infix, then highlight it.
When edit-mode is enabled, then prepend the level information.
When `transient-enable-popup-navigation' is non-nil then format
as a button."
  (let ((str (cl-call-next-method obj)))
    (when (and (cl-typep obj 'transient-infix)
               (eq (oref obj command) this-original-command)
               (active-minibuffer-window))
      (setq str (transient--add-face str 'transient-active-infix)))
    (when transient--editp
      (setq str (concat (let ((level (oref obj level)))
                          (propertize (format " %s " level)
                                      'face (if (transient--use-level-p level t)
                                                'transient-enabled-suffix
                                              'transient-disabled-suffix)))
                        str)))
    (when (and transient-enable-popup-navigation
               (slot-boundp obj 'command))
      (setq str (make-text-button str nil
                                  'type 'transient
                                  'suffix obj
                                  'command (oref obj command))))
    str))

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
  (let ((key (if (slot-boundp obj 'key) (oref obj key) ""))
        (cmd (and (slot-boundp obj 'command) (oref obj command))))
    (when-let* ((width (oref transient--pending-group pad-keys)))
      (setq key (truncate-string-to-width key width nil ?\s)))
    (if transient--redisplay-key
        (let ((len (length transient--redisplay-key))
              (seq (cl-coerce (edmacro-parse-keys key t) 'list)))
          (cond
            ((member (seq-take seq len)
                     (list transient--redisplay-key
                           (thread-last transient--redisplay-key
                             (cl-substitute ?- 'kp-subtract)
                             (cl-substitute ?= 'kp-equal)
                             (cl-substitute ?+ 'kp-add))))
             (let ((pre (key-description (vconcat (seq-take seq len))))
                   (suf (key-description (vconcat (seq-drop seq len)))))
               (setq pre (string-replace "RET" "C-m" pre))
               (setq pre (string-replace "TAB" "C-i" pre))
               (setq suf (string-replace "RET" "C-m" suf))
               (setq suf (string-replace "TAB" "C-i" suf))
               ;; We use e.g., "-k" instead of the more correct "- k",
               ;; because the former is prettier.  If we did that in
               ;; the definition, then we want to drop the space that
               ;; is reinserted above.  False-positives are possible
               ;; for silly bindings like "-C-c C-c".
               (unless (string-search " " key)
                 (setq pre (string-replace " " "" pre))
                 (setq suf (string-replace " " "" suf)))
               (concat (propertize pre 'face 'transient-unreachable-key)
                       (and (string-prefix-p (concat pre " ") key) " ")
                       (propertize suf 'face (transient--key-face cmd key))
                       (save-excursion
                         (and (string-match " +\\'" key)
                              (propertize (match-string 0 key)
                                          'face 'fixed-pitch))))))
            ((transient--lookup-key transient-sticky-map (kbd key))
             (propertize key 'face (transient--key-face cmd key)))
            (t
             (propertize key 'face 'transient-unreachable-key))))
      (propertize key 'face (transient--key-face cmd key)))))

(cl-defmethod transient-format-key :around ((obj transient-argument))
  "Handle `transient-highlight-mismatched-keys'."
  (let ((key (cl-call-next-method obj)))
    (cond
      ((not transient-highlight-mismatched-keys) key)
      ((not (slot-boundp obj 'shortarg))
       (transient--add-face key 'transient-nonstandard-key))
      ((not (string-equal key (oref obj shortarg)))
       (transient--add-face key 'transient-mismatched-key))
      (key))))

(cl-defgeneric transient-format-description (obj)
  "Format OBJ's `description' for display and return the result.")

(cl-defmethod transient-format-description ((obj transient-suffix))
  "The `description' slot may be a function, in which case that is
called inside the correct buffer (see `transient--insert-group')
and its value is returned to the caller."
  (transient--get-description obj))

(cl-defmethod transient-format-description ((obj transient-value-preset))
  (pcase-let* (((eieio description key set) obj)
               (value (transient--get-extended-value))
               (active (seq-set-equal-p set value)))
    (format
     "%s %s"
     (propertize (or description (format "Preset %s" key))
                 'face (and active 'transient-argument))
     (format (propertize "(%s)" 'face 'transient-delimiter)
             (mapconcat (lambda (arg)
                          (propertize
                           arg 'face (cond (active 'transient-argument)
                                           ((member arg value)
                                            '((:weight demibold)
                                              transient-inactive-argument))
                                           ('transient-inactive-argument))))
                        set " ")))))

(cl-defmethod transient-format-description ((obj transient-group))
  "Format the description by calling the next method.
If the result doesn't use the `face' property at all, then apply the
face `transient-heading' to the complete string."
  (and-let* ((desc (transient--get-description obj)))
    (cond ((oref obj inapt)
           (propertize desc 'face 'transient-inapt-suffix))
          ((text-property-not-all 0 (length desc) 'face nil desc)
           desc)
          ((propertize desc 'face 'transient-heading)))))

(cl-defmethod transient-format-description :around ((obj transient-suffix))
  "Format the description by calling the next method.
If the result is nil, then use \"(BUG: no description)\" as the
description.  If the OBJ's `key' is currently unreachable, then
apply the face `transient-unreachable' to the complete string."
  (let ((desc (or (cl-call-next-method obj)
                  (and (slot-boundp transient--prefix 'suffix-description)
                       (funcall (oref transient--prefix suffix-description)
                                obj)))))
    (when-let* ((_ transient--docsp)
                (_(slot-boundp obj 'command))
                (cmd (oref obj command))
                (_(not (memq 'transient--default-infix-command
                             (function-alias-p cmd))))
                (docstr (ignore-errors (documentation cmd)))
                (docstr (string-trim
                         (substring docstr 0 (string-match "\\.?\n" docstr))))
                (_(not (equal docstr ""))))
      (setq desc (format-spec transient-show-docstring-format
                              `((?c . ,desc)
                                (?s . ,docstr)))))
    (if desc
        (when-let* ((face (transient--get-face obj 'face)))
          (setq desc (transient--add-face desc face t)))
      (setq desc (propertize "(BUG: no description)" 'face 'error)))
    (when (cond (transient--all-levels-p
                 (> (oref obj level) transient--default-prefix-level))
                (transient-highlight-higher-levels
                 (> (max (oref obj level) transient--max-group-level)
                    transient--default-prefix-level)))
      (setq desc (transient--add-face desc 'transient-higher-level)))
    (when-let* ((inapt-face (and (oref obj inapt)
                                 (transient--get-face obj 'inapt-face))))
      (setq desc (transient--add-face desc inapt-face)))
    (when (and (slot-boundp obj 'key)
               (transient--key-unreachable-p obj))
      (setq desc (transient--add-face desc 'transient-unreachable)))
    desc))

(cl-defgeneric transient-format-value (obj)
  "Format OBJ's value for display and return the result.")

(cl-defmethod transient-format-value ((obj transient-suffix))
  (propertize (oref obj argument)
              'face (if (oref obj value)
                        (if (oref obj inapt)
                            'transient-inapt-argument
                          'transient-argument)
                      'transient-inactive-argument)))

(cl-defmethod transient-format-value ((obj transient-option))
  (let ((argument (prin1-to-string (oref obj argument) t)))
    (if-let* ((value (oref obj value)))
        (let* ((inapt (oref obj inapt))
               (aface (if inapt 'transient-inapt-argument 'transient-argument))
               (vface (if inapt 'transient-inapt-argument 'transient-value)))
          (pcase-exhaustive (oref obj multi-value)
            ('nil
             (concat (propertize argument 'face aface)
                     (propertize value    'face vface)))
            ((or 't 'rest)
             (concat (propertize (if (string-suffix-p " " argument)
                                     argument
                                   (concat argument " "))
                                 'face aface)
                     (propertize (mapconcat #'prin1-to-string value " ")
                                 'face vface)))
            ('repeat
             (mapconcat (lambda (value)
                          (concat (propertize argument 'face aface)
                                  (propertize value    'face vface)))
                        value " "))))
      (propertize argument 'face 'transient-inactive-argument))))

(cl-defmethod transient-format-value ((obj transient-switches))
  (with-slots (value argument-format choices) obj
    (format (propertize argument-format
                        'face (if value
                                  'transient-argument
                                'transient-inactive-argument))
            (format
             (propertize "[%s]" 'face 'transient-delimiter)
             (mapconcat
              (lambda (choice)
                (propertize choice 'face
                            (if (equal (format argument-format choice) value)
                                (if (oref obj inapt)
                                    'transient-inapt-argument
                                  'transient-value)
                              'transient-inactive-value)))
              choices
              (propertize "|" 'face 'transient-delimiter))))))

(cl-defmethod transient--get-description ((obj transient-child))
  (cond*
    ((bind* (desc (oref obj description))))
    ((functionp desc)
     (condition-case nil
         (funcall desc obj)
       (wrong-number-of-arguments (funcall desc))))
    (desc)))

(cl-defmethod transient--get-face ((obj transient-suffix) slot)
  (cond*
    ((not (slot-boundp obj slot)) nil)
    ((bind* (face (slot-value obj slot))))
    ((facep face) face)
    ((functionp face)
     (let ((transient--pending-suffix obj))
       (condition-case nil
           (funcall face obj)
         (wrong-number-of-arguments (funcall face)))))))

(defun transient--add-face (string face &optional append beg end)
  (let ((str (copy-sequence string)))
    (add-face-text-property (or beg 0) (or end (length str)) face append str)
    str))

(defun transient--key-face (cmd key &optional enforce-type)
  (or (and transient-semantic-coloring
           (not transient--helpp)
           (not transient--editp)
           (get (transient--get-pre-command cmd key enforce-type)
                'transient-face))
      (if cmd 'transient-key 'transient-key-noop)))

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
  (when-let* ((pad (or (oref group pad-keys)
                       (and parent (oref parent pad-keys)))))
    (oset group pad-keys
          (apply #'max
                 (if (integerp pad) pad 0)
                 (seq-keep (lambda (suffix)
                             (and (eieio-object-p suffix)
                                  (slot-boundp suffix 'key)
                                  (length (oref suffix key))))
                           (oref group suffixes))))))

(defun transient--pixel-width (string)
  (save-window-excursion
    (with-temp-buffer
      (insert string)
      (set-window-dedicated-p nil nil)
      (set-window-buffer nil (current-buffer))
      (car (window-text-pixel-size
            nil (line-beginning-position) (point))))))

(defun transient--column-stops (columns)
  (let* ((var-pitch (or transient-align-variable-pitch
                        (oref transient--prefix variable-pitch)))
         (char-width (and var-pitch (transient--pixel-width " "))))
    (transient--seq-reductions-from
     (apply-partially #'+ (* 2 (if var-pitch char-width 1)))
     (transient--mapn
      (lambda (cells min)
        (apply #'max
               (if min (if var-pitch (* min char-width) min) 0)
               (mapcar (if var-pitch #'transient--pixel-width #'length) cells)))
      columns
      (oref transient--prefix column-widths))
     0)))

(defun transient--align-to (stop)
  (unless (zerop stop)
    (insert (if (or transient-align-variable-pitch
                    (oref transient--prefix variable-pitch))
                (propertize " " 'display `(space :align-to (,stop)))
              (make-string (max 0 (- stop (current-column))) ?\s)))))

(defun transient-command-summary-or-name (obj)
  "Return the summary or name of the command represented by OBJ.

If the command has a doc-string, then return the first line of
that, else its name.

Intended to be temporarily used as the `:suffix-description' of
a prefix command, while porting a regular keymap to a transient."
  (let ((command (oref obj command)))
    (if-let* ((doc (documentation command)))
        (propertize (car (split-string doc "\n")) 'face 'font-lock-doc-face)
      (propertize (symbol-name command) 'face 'font-lock-function-name-face))))

;;;; Help

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
          ((transient--describe-function command)))))

(cl-defmethod transient-show-help ((obj transient-suffix))
  "Call `show-help' if non-nil, else use `describe-function'.
Also used to dispatch showing documentation for the current
prefix.  If the suffix is a sub-prefix, then also call the
prefix method."
  (cond*
    ((eq this-command 'transient-help)
     (transient-show-help transient--prefix))
    ((bind-and* (prefix (get (oref obj command) 'transient--prefix))
                (_(not (eq (oref transient--prefix command) this-command))))
     (transient-show-help prefix))
    ((bind-and* (show-help (oref obj show-help)))
     (funcall show-help obj))
    ((transient--describe-function this-command))))

(cl-defmethod transient-show-help ((obj transient-infix))
  "Call `show-help' if non-nil, else show the `man-page'
if non-nil, else use `describe-function'.  When showing the
manpage, then try to jump to the correct location."
  (cond*
    ((bind-and* (show-help (oref obj show-help)))
     (funcall show-help obj))
    ((bind-and* (man-page (oref transient--prefix man-page))
                (argument (and (slot-boundp obj 'argument)
                               (oref obj argument))))
     (transient--show-manpage man-page argument))
    ((transient--describe-function this-command))))

;; `cl-generic-generalizers' doesn't support `command' et al.
(cl-defmethod transient-show-help (cmd)
  "Show the command doc-string."
  (transient--describe-function cmd))

(defmacro transient-with-help-window (&rest body)
  "Evaluate BODY, send output to *Help* buffer, and display it in a window.
Select the help window, and make the help buffer current and return it."
  (declare (indent 0))
  `(let ((buffer nil)
         (help-window-select t))
     (with-help-window (help-buffer)
       ,@body
       (setq buffer (current-buffer)))
     (set-buffer buffer)))

(defun transient--display-help (helper target)
  (let ((winconf (current-window-configuration)))
    (funcall (cond (helper)
                   ((symbolp target) #'transient--describe-function)
                   ((stringp target)
                    (if (string-prefix-p "(" target)
                        #'transient--show-manual
                      #'transient--show-manpage))
                   ((error "Unknown how to show help for %S" target)))
             target)
    (setq-local transient--restore-winconf winconf))
  (fit-window-to-buffer nil (frame-height) (window-height))
  (transient-resume-mode)
  (message (substitute-command-keys "Type \\`q' to resume transient command.")))

(defun transient--describe-function (fn)
  (let* ((buffer nil)
         (help-window-select t)
         (temp-buffer-window-setup-hook
          (cons (lambda () (setq buffer (current-buffer)))
                temp-buffer-window-setup-hook)))
    (describe-function fn)
    (set-buffer buffer)))

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
     (format
      (propertize "\
Type a %s to show help for that suffix command, or %s to show manual.
Type %s to exit help.\n"
                  'face 'transient-heading)
      (propertize "<KEY>" 'face 'transient-key)
      (propertize "?"     'face 'transient-key)
      (propertize "C-g"   'face 'transient-key))))
  (when transient--editp
    (unless transient--helpp
      (insert
       (format
        (propertize "\
Type %s and then %s to put the respective suffix command on level %s.
Type %s and then %s to display suffixes up to level %s in this menu.
Type %s and then %s to describe the respective suffix command.\n"
                    'face 'transient-heading)
        (propertize "<KEY>" 'face 'transient-key)
        (propertize "<N>"   'face 'transient-key)
        (propertize " N "   'face 'transient-enabled-suffix)
        (propertize (concat transient-common-command-prefix " l")
                    'face 'transient-key)
        (propertize "<N>"   'face 'transient-key)
        (propertize " N "   'face 'transient-enabled-suffix)
        (propertize "C-h"   'face 'transient-key)
        (propertize "<KEY>" 'face 'transient-key))))
    (with-slots (level) transient--prefix
      (insert
       (format
        (propertize "
The current level of this menu is %s, so
  commands on levels %s are displayed, and
  commands on levels %s and %s are not displayed.\n"
                    'face 'transient-heading)
        (propertize (format " %s " level)    'face 'transient-enabled-suffix)
        (propertize (format " 1..%s " level) 'face 'transient-enabled-suffix)
        (propertize (format " >= %s " (1+ level))
                    'face 'transient-disabled-suffix)
        (propertize " 0 " 'face 'transient-disabled-suffix))))))

(cl-defgeneric transient-show-summary (obj &optional return)
  "Show brief summary about the command at point in the echo area.

If OBJ's `summary' slot is a string, use that.  If it is a function,
call that with OBJ as the only argument and use the returned string.
If `summary' is or returns something other than a string or nil,
show no summary.  If `summary' is or returns nil, use the first line
of the documentation string, if any.

If RETURN is non-nil, return the summary instead of showing it.
This is used when a tooltip is needed.")

(cl-defmethod transient-show-summary ((obj transient-suffix) &optional return)
  (with-slots (command summary) obj
    (when-let*
        ((doc (cond ((functionp summary)
                     (funcall summary obj))
                    (summary)
                    ((documentation command)
                     (car (split-string (documentation command) "\n")))))
         (_(stringp doc))
         (_(not (equal doc
                       (car (split-string (documentation
                                           'transient--default-infix-command)
                                          "\n"))))))
      (when (string-suffix-p "." doc)
        (setq doc (substring doc 0 -1)))
      (if return
          doc
        (let ((message-log-max nil))
          (message "%s" doc))))))

;;;; Menu Navigation

(defun transient-scroll-up (&optional arg)
  "Scroll text of transient's menu window upward ARG lines.
If ARG is nil scroll near full screen.  This is a wrapper
around `scroll-up-command' (which see)."
  (interactive "^P")
  (with-selected-window transient--window
    (scroll-up-command arg)))

(defun transient-scroll-down (&optional arg)
  "Scroll text of transient's menu window down ARG lines.
If ARG is nil scroll near full screen.  This is a wrapper
around `scroll-down-command' (which see)."
  (interactive "^P")
  (with-selected-window transient--window
    (scroll-down-command arg)))

(defun transient-backward-button (n)
  "Move to the previous button in transient's menu buffer.
See `backward-button' for information about N."
  (interactive "p")
  (with-selected-window transient--window
    (backward-button n t)
    (when (eq transient-enable-popup-navigation 'verbose)
      (transient-show-summary (get-text-property (point) 'suffix)))))

(defun transient-forward-button (n)
  "Move to the next button in transient's menu buffer.
See `forward-button' for information about N."
  (interactive "p")
  (with-selected-window transient--window
    (forward-button n t)
    (when (eq transient-enable-popup-navigation 'verbose)
      (transient-show-summary (get-text-property (point) 'suffix)))))

(define-button-type 'transient
  'face nil
  'keymap transient-button-map
  'help-echo (lambda (win buf pos)
               (with-selected-window win
                 (with-current-buffer buf
                   (transient-show-summary
                    (get-text-property pos 'suffix) t)))))

(defun transient--goto-button (command)
  (cond
    ((stringp command)
     (when (re-search-forward (concat "^" (regexp-quote command)) nil t)
       (goto-char (match-beginning 0))))
    (command
     (cl-flet ((found ()
                 (and-let* ((button (button-at (point))))
                   (eq (button-get button 'command) command))))
       (while (and (ignore-errors (forward-button 1))
                   (not (found))))
       (unless (found)
         (goto-char (point-min))
         (ignore-errors (forward-button 1))
         (unless (found)
           (goto-char (point-min))))))))

(defun transient--heading-at-point ()
  (and (eq (get-text-property (point) 'face) 'transient-heading)
       (let ((beg (line-beginning-position)))
         (buffer-substring-no-properties
          beg (next-single-property-change
               beg 'face nil (line-end-position))))))

;;;; Compatibility
;;;; Menu Isearch

(defvar-keymap transient--isearch-mode-map
  :parent isearch-mode-map
  "<t>"                      #'transient-isearch-exit
  "<remap> <isearch-exit>"   #'transient-isearch-exit
  "<remap> <isearch-cancel>" #'transient-isearch-cancel
  "<remap> <isearch-abort>"  #'transient-isearch-abort)

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
  (let ((around (lambda (fn)
                  (condition-case nil (funcall fn) (quit))
                  (transient--isearch-exit))))
    (advice-add 'isearch-cancel :around around)
    (unwind-protect
        (isearch-abort)
      (advice-remove 'isearch-cancel around))))

(defun transient--isearch-setup ()
  (select-window transient--window)
  (transient--suspend-override t))

(defun transient--isearch-exit ()
  (select-window transient--original-window)
  (transient--resume-override))

;;;; Edebug

(defun transient--edebug-command-p ()
  (and (bound-and-true-p edebug-active)
       (or (memq this-command '(top-level abort-recursive-edit))
           (string-prefix-p "edebug" (symbol-name this-command)))))

;;;; Miscellaneous

(cl-pushnew (list nil (concat "^\\s-*("
                              (eval-when-compile
                                (regexp-opt
                                 '("transient-define-prefix"
                                   "transient-define-suffix"
                                   "transient-define-infix"
                                   "transient-define-argument")
                                 t))
                              "\\s-+\\(" lisp-mode-symbol-regexp "\\)")
                  2)
            lisp-imenu-generic-expression :test #'equal)

(defun transient--suspend-text-conversion-style ()
  (static-if (boundp 'overriding-text-conversion-style) ; since Emacs 30.1
      (when text-conversion-style
        (letrec ((suspended overriding-text-conversion-style)
                 (fn (lambda ()
                       (setq overriding-text-conversion-style nil)
                       (remove-hook 'transient-exit-hook fn))))
          (setq overriding-text-conversion-style suspended)
          (add-hook 'transient-exit-hook fn)))))

(declare-function which-key-mode "ext:which-key" (&optional arg))

(defun transient--suspend-which-key-mode ()
  (when (bound-and-true-p which-key-mode)
    (which-key-mode -1)
    (add-hook 'transient-exit-hook #'transient--resume-which-key-mode)))

(defun transient--resume-which-key-mode ()
  (unless transient--prefix
    (which-key-mode 1)
    (remove-hook 'transient-exit-hook #'transient--resume-which-key-mode)))

(defun transient-bind-q-to-quit ()
  "Modify some keymaps to bind \\`q' to the appropriate quit command.

\\`C-g' is the default binding for such commands now, but Transient's
predecessor Magit-Popup used \\`q' instead.  If you would like to get
that binding back, then call this function in your init file like so:

  (with-eval-after-load \\='transient
    (transient-bind-q-to-quit))

Individual transients may already bind \\`q' to something else
and such a binding would shadow the quit binding.  If that is the
case then \\`Q' is bound to whatever \\`q' would have been bound
to, by setting `transient-substitute-key-function' to a function
that does that.  Of course \\`Q' may already be bound to something
else, so that function binds \\`M-q' to that command instead.
Of course \\`M-q' may already be bound to something else, but
we stop there."
  (keymap-set transient-base-map   "q" #'transient-quit-one)
  (keymap-set transient-sticky-map "q" #'transient-quit-seq)
  (setq transient-substitute-key-function
        #'transient-rebind-quit-commands))

(defun transient-rebind-quit-commands (obj)
  "See `transient-bind-q-to-quit'."
  (let ((key (oref obj key)))
    (cond ((string-equal key "q") "Q")
          ((string-equal key "Q") "M-q")
          (key))))

(defun transient--force-fixed-pitch ()
  (require 'face-remap)
  (face-remap-reset-base 'default)
  (face-remap-add-relative 'default 'fixed-pitch))

(defun transient--seq-reductions-from (function sequence initial-value)
  (let ((acc (list initial-value)))
    (seq-doseq (elt sequence)
      (push (funcall function (car acc) elt) acc))
    (nreverse acc)))

(defun transient--mapn (function &rest lists)
  "Apply FUNCTION to elements of LISTS.
Like `cl-mapcar' but while that stops when the shortest list
is exhausted, continue until the longest list is, using nil
as stand-in for elements of exhausted lists."
  (let (result)
    (while (catch 'more (mapc (lambda (l) (and l (throw 'more t))) lists) nil)
      (push (apply function (mapcar #'car-safe lists)) result)
      (setq lists (mapcar #'cdr lists)))
    (nreverse result)))

;;;; Font-Lock

(defconst transient-font-lock-keywords
  (eval-when-compile
    `((,(concat "("
                (regexp-opt (list "transient-define-prefix"
                                  "transient-define-group"
                                  "transient-define-infix"
                                  "transient-define-argument"
                                  "transient-define-suffix")
                            t)
                "\\_>[ \t'(]*"
                "\\(\\(?:\\sw\\|\\s_\\)+\\)?")
       (1 'font-lock-keyword-face)
       (2 'font-lock-function-name-face nil t)))))

(font-lock-add-keywords 'emacs-lisp-mode transient-font-lock-keywords)

;;;; Auxiliary Classes
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
  (or (cl-call-next-method obj)
      (symbol-name (oref obj variable))))

(cl-defmethod transient-format-value ((obj transient-lisp-variable))
  (propertize (prin1-to-string (oref obj value))
              'face 'transient-value))

(cl-defmethod transient-prompt ((obj transient-lisp-variable))
  (if (and (slot-boundp obj 'prompt)
           (oref obj prompt))
      (cl-call-next-method obj)
    (format "Set %s: " (oref obj variable))))

(defun transient-lisp-variable--reader (prompt initial-input _history)
  (read--expression prompt initial-input))

;;;; `transient-cons-option'

(defclass transient-cons-option (transient-option)
  ((format :initform " %k %d: %v"))
  "[Experimental] Class used for unencoded key-value pairs.")

(cl-defmethod transient-infix-value ((obj transient-cons-option))
  "Return ARGUMENT and VALUE as a cons-cell or nil if the latter is nil."
  (and-let* ((value (oref obj value)))
    (cons (oref obj argument) value)))

(cl-defmethod transient-format-description ((obj transient-cons-option))
  (or (oref obj description)
      (let ((description (prin1-to-string (oref obj argument) t)))
        (if (string-prefix-p ":" description)
            (substring description 1)
          description))))

(cl-defmethod transient-format-value ((obj transient-cons-option))
  (let ((value (oref obj value)))
    (propertize (prin1-to-string value t) 'face
                (if value 'transient-value 'transient-inactive-value))))

;;;; _
(provide 'transient)
;; Local Variables:
;; checkdoc-symbol-words: ("command-line" "edit-mode" "help-mode")
;; indent-tabs-mode: nil
;; lisp-indent-local-overrides: (
;;   (cond . 0)
;;   (cond* . 0)
;;   (interactive . 0))
;; End:
;;; transient.el ends here
