;;; so-long.el --- Say farewell to performance problems with minified code.  -*- lexical-binding:t -*-
;;
;; Copyright (C) 2015-2016, 2018-2020 Free Software Foundation, Inc.

;; Author: Phil Sainty <psainty@orcon.net.nz>
;; Maintainer: Phil Sainty <psainty@orcon.net.nz>
;; URL: https://savannah.nongnu.org/projects/so-long
;; Keywords: convenience
;; Created: 23 Dec 2015
;; Package-Requires: ((emacs "24.4"))
;; Version: 1.0

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
;;
;; * Introduction
;; --------------
;; When the lines in a file are so long that performance could suffer to an
;; unacceptable degree, we say "so long" to the slow modes and options enabled
;; in that buffer, and invoke something much more basic in their place.
;;
;; Many Emacs modes struggle with buffers which contain excessively long lines.
;; This is commonly on account of 'minified' code (i.e. code that has been
;; compacted into the smallest file size possible, which often entails removing
;; newlines should they not be strictly necessary).  This can result in lines
;; which are many thousands of characters long, and most programming modes
;; simply aren't optimized (remotely) for this scenario, so performance can
;; suffer significantly.
;;
;; When such files are detected, the command `so-long' is automatically called,
;; overriding certain minor modes and variables with performance implications
;; (all configurable), in order to enhance performance in the buffer.
;;
;; The default action enables the major mode `so-long-mode' in place of the mode
;; that Emacs selected.  This ensures that the original major mode cannot affect
;; performance further, as well as making the so-long activity more obvious to
;; the user.  These kinds of minified files are typically not intended to be
;; edited, so not providing the usual editing mode in such cases will rarely be
;; an issue.  However, should the user wish to do so, the original state of the
;; buffer may be reinstated by calling `so-long-revert' (the key binding for
;; which is advertised when the major mode change occurs).  If you prefer that
;; the major mode not be changed, the `so-long-minor-mode' action can be
;; configured.
;;
;; The user options `so-long-action' and `so-long-action-alist' determine what
;; will happen when `so-long' and `so-long-revert' are invoked, allowing
;; alternative actions (including custom actions) to be configured.  As well as
;; the major and minor mode actions provided by this library, `longlines-mode'
;; is also supported by default as an alternative action.
;;
;; Note that while the measures taken can improve performance dramatically when
;; dealing with such files, this library does not have any effect on the
;; fundamental limitations of the Emacs redisplay code itself; and so if you do
;; need to edit the file, performance may still degrade as you get deeper into
;; the long lines.  In such circumstances you may find that `longlines-mode' is
;; the most helpful facility.
;;
;; Note also that the mitigation is automatically triggered when visiting a
;; file.  The library does not automatically detect if long lines are inserted
;; into an existing buffer (although the `so-long' command can be invoked
;; manually in such situations).

;; * Installation
;; --------------
;; Use M-x global-so-long-mode to enable/toggle the functionality.  To enable
;; the functionality by default, either customize the `global-so-long-mode' user
;; option, or add the following to your init file:
;;
;;   ;; Avoid performance issues in files with very long lines.
;;   (global-so-long-mode 1)
;;
;; If necessary, ensure that so-long.el is in a directory in your load-path, and
;; that the library has been loaded.  (These steps are not necessary if you are
;; using Emacs 27+, or have installed the GNU ELPA package.)

;; * Overview of modes and commands
;; --------------------------------
;; - `global-so-long-mode' - A global minor mode which enables the automated
;;    behavior, causing the user's preferred action to be invoked whenever a
;;    newly-visited file contains excessively long lines.
;; - `so-long-mode' - A major mode, and the default action.
;; - `so-long-minor-mode' - A minor mode version of the major mode, and an
;;    alternative action.
;; - `longlines-mode' - A minor mode provided by the longlines.el library,
;;    and another alternative action.
;; - `so-long' - Manually invoke the user's preferred action, enabling its
;;    performance improvements for the current buffer.
;; - `so-long-revert' - Restore the original state of the buffer.
;; - `so-long-customize' - Configure the user options.
;; - `so-long-commentary' - Read this documentation in outline-mode.

;; * Usage
;; -------
;; In most cases you will simply enable `global-so-long-mode' and leave it to
;; act automatically as required, in accordance with your configuration (see
;; "Basic configuration" below).
;;
;; On rare occasions you may choose to manually invoke the `so-long' command,
;; which invokes your preferred `so-long-action' (exactly as the automatic
;; behavior would do if it had detected long lines).  You might use this if a
;; problematic file did not meet your configured criteria, and you wished to
;; trigger the performance improvements manually.
;;
;; It is also possible to directly use `so-long-mode' or `so-long-minor-mode'
;; (major and minor modes, respectively).  Both of these modes are actions
;; available to `so-long' but, like any other mode, they can be invoked directly
;; if you have a need to do that (see also "Other ways of using so-long" below).
;;
;; If the behavior ever triggers when you did not want it to, you can use the
;; `so-long-revert' command to restore the buffer to its original state.

;; * Basic configuration
;; ---------------------
;; Use M-x customize-group RET so-long RET
;; (or M-x so-long-customize RET)
;;
;; The user options `so-long-target-modes', `so-long-threshold', and
;; `so-long-max-lines' determine whether action will be taken automatically when
;; visiting a file, and `so-long-action' determines what will be done.

;; * Actions and menus
;; -------------------
;; The user options `so-long-action' and `so-long-action-alist' determine what
;; will happen when `so-long' and `so-long-revert' are invoked, and you can add
;; your own custom actions if you wish.  The selected action can be invoked
;; manually with M-x so-long; and in general M-x so-long-revert will undo the
;; effects of whichever action was used (which is particularly useful when the
;; action is triggered automatically, but the detection was a 'false positive'.)
;;
;; All defined actions are presented in the "So Long" menu, which is visible
;; whenever long lines have been detected.  Selecting an action from the menu
;; will firstly cause the current action (if any) to be reverted, before the
;; newly-selected action is invoked.
;;
;; Aside from the menu bar, the menu is also available in the mode line --
;; either via the major mode construct (when `so-long-mode' is active), or in
;; a separate mode line construct when some other major mode is active.

;; * Files with a file-local 'mode'
;; --------------------------------
;; A file-local major mode is likely to be safe even if long lines are detected
;; (as the author of the file would otherwise be unlikely to have set that mode),
;; and so these files are treated as special cases.  When a file-local 'mode' is
;; present, the function defined by the `so-long-file-local-mode-function' user
;; option is called.  The default value will cause the `so-long-minor-mode'
;; action to be used instead of the `so-long-mode' action, if the latter was
;; going to be used for this file.  This is still a conservative default, but
;; this option can also be configured to inhibit so-long entirely in this
;; scenario, or to not treat a file-local mode as a special case at all.

;; * Buffers which are not displayed in a window
;; ---------------------------------------------
;; When a file with long lines is visited and the buffer is not displayed right
;; away, it may be that it is not intended to be displayed at all, and that it
;; has instead been visited for behind-the-scenes processing by some library.
;; Invisible buffers are less likely to cause performance issues, and it also
;; might be surprising to the other library if such a buffer were manipulated by
;; `so-long' (which might in turn lead to confusing errors for the user); so in
;; these situations the `so-long-invisible-buffer-function' value is called
;; instead.  By default this arranges for `so-long' to be invoked on the buffer
;; if and when it is displayed, but not otherwise.
;;
;; This 'deferred call' is actually the most common scenario -- even when a
;; visited file is displayed "right away", it is normal for the buffer to be
;; invisible when `global-so-long-mode' processes it, and the gap between
;; "arranging to call" and "calling" `so-long' is simply extremely brief.

;; * Inhibiting and disabling minor modes
;; --------------------------------------
;; Certain minor modes cause significant performance issues in the presence of
;; very long lines, and should be disabled automatically in this situation.
;;
;; The simple way to disable most buffer-local minor modes is to add the mode
;; symbol to the `so-long-minor-modes' list.  Several modes are targeted by
;; default, and it is a good idea to customize this variable to add any
;; additional buffer-local minor modes that you use which you know to have
;; performance implications.
;;
;; These minor modes are disabled if `so-long-action' is set to either
;; `so-long-mode' or `so-long-minor-mode'.  If `so-long-revert' is called, then
;; the original values are restored.
;;
;; In the case of globalized minor modes, be sure to specify the buffer-local
;; minor mode, and not the global mode which controls it.
;;
;; Note that `so-long-minor-modes' is not useful for other global minor modes
;; (as distinguished from globalized minor modes), but in some cases it will be
;; possible to inhibit or otherwise counter-act the behavior of a global mode
;; by overriding variables, or by employing hooks (see below).  You would need
;; to inspect the code for a given global mode (on a case by case basis) to
;; determine whether it's possible to inhibit it for a single buffer -- and if
;; so, how best to do that, as not all modes are alike.

;; * Overriding variables
;; ----------------------
;; `so-long-variable-overrides' is an alist mapping variable symbols to values.
;; If `so-long-action' is set to either `so-long-mode' or `so-long-minor-mode',
;; the buffer-local value for each variable in the list is set to the associated
;; value in the alist.  Use this to enforce values which will improve
;; performance or otherwise avoid undesirable behaviors.  If `so-long-revert'
;; is called, then the original values are restored.

;; * Hooks
;; -------
;; `so-long-hook' runs at the end of the `so-long' command, after the configured
;; action has been invoked.
;;
;; Likewise, if the `so-long-revert' command is used to restore the original
;; state then, once that has happened, `so-long-revert-hook' is run.
;;
;; The major and minor modes also provide the usual hooks: `so-long-mode-hook'
;; for the major mode (running between `change-major-mode-after-body-hook' and
;; `after-change-major-mode-hook'); and `so-long-minor-mode-hook' (when that
;; mode is enabled or disabled).

;; * Troubleshooting
;; -----------------
;; Any elisp library has the potential to cause performance problems; so while
;; the default configuration addresses some important common cases, it's
;; entirely possible that your own config introduces problem cases which are
;; unknown to this library.
;;
;; If visiting a file is still taking a very long time with so-long enabled,
;; you should test the following command:
;;
;; emacs -Q -l so-long -f global-so-long-mode <file>
;;
;; For versions of Emacs < 27, use:
;; emacs -Q -l /path/to/so-long.el -f global-so-long-mode <file>
;;
;; If the file loads quickly when that command is used, you'll know that
;; something in your personal configuration is causing problems.  If this
;; turns out to be a buffer-local minor mode, or a user option, you can
;; likely alleviate the issue by customizing `so-long-minor-modes' or
;; `so-long-variable-overrides' accordingly.
;;
;; The in-built profiler can be an effective way of discovering the cause
;; of such problems.  Refer to M-: (info "(elisp) Profiling") RET
;;
;; In some cases it may be useful to set a file-local `mode' variable to
;; `so-long-mode', completely bypassing the automated decision process.
;; Refer to M-: (info "(emacs) Specifying File Variables") RET
;;
;; If so-long itself is causing problems, it can be inhibited by setting the
;; `so-long-enabled' variable to nil, or by disabling the global mode with
;; M-- M-x global-so-long-mode, or M-: (global-so-long-mode 0)

;; * Example configuration
;; -----------------------
;; If you prefer to configure in code rather than via the customize interface,
;; then you might use something along these lines:
;;
;;   ;; Enable so-long library.
;;   (when (require 'so-long nil :noerror)
;;     (global-so-long-mode 1)
;;     ;; Basic settings.
;;     (setq so-long-action 'so-long-minor-mode)
;;     (setq so-long-threshold 1000)
;;     (setq so-long-max-lines 100)
;;     ;; Additional target major modes to trigger for.
;;     (mapc (apply-partially #'add-to-list 'so-long-target-modes)
;;           '(sgml-mode nxml-mode))
;;     ;; Additional buffer-local minor modes to disable.
;;     (mapc (apply-partially #'add-to-list 'so-long-minor-modes)
;;           '(diff-hl-mode diff-hl-amend-mode diff-hl-flydiff-mode))
;;     ;; Additional variables to override.
;;     (mapc (apply-partially #'add-to-list 'so-long-variable-overrides)
;;           '((show-trailing-whitespace . nil)
;;             (truncate-lines . nil))))

;; * Other ways of using so-long
;; -----------------------------
;; It may prove useful to automatically invoke major mode `so-long-mode' for
;; certain files, irrespective of whether they contain long lines.
;;
;; To target specific files and extensions, using `auto-mode-alist' is the
;; simplest method.  To add such an entry, use:
;; (add-to-list 'auto-mode-alist (cons REGEXP 'so-long-mode))
;; Where REGEXP is a regular expression matching the filename.  e.g.:
;;
;; - Any filename with a particular extension ".foo":
;;   (rx ".foo" eos)
;;
;; - Any file in a specific directory:
;;   (rx bos "/path/to/directory/")
;;
;; - Only *.c filenames under that directory:
;;   (rx bos "/path/to/directory/" (zero-or-more not-newline) ".c" eos)
;;
;; - Match some sub-path anywhere in a filename:
;;   (rx "/sub/path/foo")
;;
;; - A specific individual file:
;;   (rx bos "/path/to/file" eos)
;;
;; Another way to target individual files is to set a file-local `mode'
;; variable.  Refer to M-: (info "(emacs) Specifying File Variables") RET
;;
;; `so-long-minor-mode' can also be called directly if desired.  e.g.:
;; (add-hook 'FOO-mode-hook 'so-long-minor-mode)
;;
;; In Emacs 26.1 or later (see "Caveats" below) you also have the option of
;; using file-local and directory-local variables to determine how `so-long'
;; behaves.  e.g. -*- so-long-action: longlines-mode; -*-
;;
;; The buffer-local `so-long-function' and `so-long-revert-function' values may
;; be set directly (in a major mode hook, for instance), as any existing value
;; for these variables will be used in preference to the values defined by the
;; selected action.  For file-local or directory-local usage it is preferable to
;; set only `so-long-action', as all function variables are marked as 'risky',
;; meaning you would need to add to `safe-local-variable-values' in order to
;; avoid being queried about them.
;;
;; Finally, the `so-long-predicate' user option enables the automated behavior
;; to be determined by a custom function, if greater control is needed.

;; * Implementation notes
;; ----------------------
;; This library advises `set-auto-mode' (in order to react after Emacs has
;; chosen the major mode for a buffer), and `hack-local-variables' (so that we
;; may behave differently when a file-local mode is set).  In earlier versions
;; of Emacs (< 26.1) we also advise `hack-one-local-variable' (to prevent a
;; file-local mode from restoring the original major mode if we had changed it).
;;
;; Many variables are permanent-local because after the normal major mode has
;; been set, we potentially change the major mode to `so-long-mode', and it's
;; important that the values which were established prior to that are retained.

;; * Caveats
;; ---------
;; The variables affecting the automated behavior of this library (such as
;; `so-long-action') can be used as file- or dir-local values in Emacs 26+, but
;; not in previous versions of Emacs.  This is on account of improvements made
;; to `normal-mode' in 26.1, which altered the execution order with respect to
;; when local variables are processed.  In earlier versions of Emacs the local
;; variables are processed too late, and hence have no effect on the decision-
;; making process for invoking `so-long'.  It is unlikely that equivalent
;; support will be implemented for older versions of Emacs.  The exception to
;; this caveat is the `mode' pseudo-variable, which is processed early in all
;; versions of Emacs, and can be set to `so-long-mode' if desired.

;; * Change Log:
;;
;; 1.0   - Included in Emacs 27.1, and in GNU ELPA for prior versions of Emacs.
;;       - New global mode `global-so-long-mode' to enable/disable the library.
;;       - New user option `so-long-action'.
;;       - New user option `so-long-action-alist' defining alternative actions.
;;       - New user option `so-long-variable-overrides'.
;;       - New user option `so-long-skip-leading-comments'.
;;       - New user option `so-long-file-local-mode-function'.
;;       - New user option `so-long-invisible-buffer-function'.
;;       - New user option `so-long-predicate'.
;;       - New variable and function `so-long-function'.
;;       - New variable and function `so-long-revert-function'.
;;       - New command `so-long' to invoke `so-long-function' interactively.
;;       - New command `so-long-revert' to invoke `so-long-revert-function'.
;;       - New minor mode action `so-long-minor-mode' facilitates retaining the
;;         original major mode, while still disabling minor modes and overriding
;;         variables like the major mode `so-long-mode'.
;;       - Support `longlines-mode' as a `so-long-action' option.
;;       - Added "So Long" menu, including all selectable actions.
;;       - Added mode-line indicator, user option `so-long-mode-line-label',
;;         and faces `so-long-mode-line-active', `so-long-mode-line-inactive'.
;;       - New help commands `so-long-commentary' and `so-long-customize'.
;;       - Renamed `so-long-mode-enabled' to `so-long-enabled'.
;;       - Refactored the default hook values using variable overrides
;;         (and returning all the hooks to nil default values).
;;       - Performance improvements for `so-long-detected-long-line-p'.
;;       - Converted defadvice to nadvice.
;; 0.7.6 - Bug fix for `so-long-mode-hook' losing its default value.
;; 0.7.5 - Documentation.
;;       - Added sgml-mode and nxml-mode to `so-long-target-modes'.
;; 0.7.4 - Refactored the handling of `whitespace-mode'.
;; 0.7.3 - Added customize group `so-long' with user options.
;;       - Added `so-long-original-values' to generalize the storage and
;;         restoration of values from the original mode upon `so-long-revert'.
;;       - Added `so-long-revert-hook'.
;; 0.7.2 - Remember the original major mode even with M-x `so-long-mode'.
;; 0.7.1 - Clarified interaction with globalized minor modes.
;; 0.7   - Handle header 'mode' declarations.
;;       - Hack local variables after reverting to the original major mode.
;;       - Reverted `so-long-max-lines' to a default value of 5.
;; 0.6.5 - Inhibit globalized `hl-line-mode' and `whitespace-mode'.
;;       - Set `buffer-read-only' by default.
;; 0.6   - Added `so-long-minor-modes' and `so-long-hook'.
;; 0.5   - Renamed library to "so-long.el".
;;       - Added explicit `so-long-enable' command to activate our advice.
;; 0.4   - Amended/documented behavior with file-local 'mode' variables.
;; 0.3   - Defer to a file-local 'mode' variable.
;; 0.2   - Initial release to EmacsWiki.
;; 0.1   - Experimental.

;;; Code:

(require 'cl-lib)

(add-to-list 'customize-package-emacs-version-alist
             '(so-long ("1.0" . "27.1")))

(defconst so-long--latest-version "1.0")

(declare-function longlines-mode "longlines")
(defvar longlines-mode)
(defvar so-long-enabled nil
  "Set to nil to prevent `so-long' from being triggered automatically.

Has no effect if `global-so-long-mode' is not enabled.")

(defvar-local so-long--active nil ; internal use
  "Non-nil when `so-long' mitigation is in effect.")

(defvar so-long--set-auto-mode nil ; internal use
  "Non-nil while `set-auto-mode' is executing.")

(defvar so-long--hack-local-variables-no-mode nil ; internal use
  "Non-nil to prevent `hack-local-variables' applying a `mode' variable.")

(defvar-local so-long--inhibited nil ; internal use
  "When non-nil, prevents the `set-auto-mode' advice from calling `so-long'.")
(put 'so-long--inhibited 'permanent-local t)

(defvar so-long--calling nil ; internal use
  ;; This prevents infinite recursion if eval:(so-long) is specified
  ;; as a file- or dir-local variable, and `so-long-action' is set to
  ;; `so-long-mode' (as that major mode would once again process the
  ;; local variables, and hence call itself).
  "Non-nil while `so-long' or `so-long-revert' is executing.")

(defvar-local so-long-detected-p nil
  "Non-nil if `so-long' has been invoked (even if subsequently reverted).")
(put 'so-long-detected-p 'permanent-local t)

(defgroup so-long nil
  "Prevent unacceptable performance degradation with very long lines."
  :prefix "so-long"
  :group 'convenience)

(defcustom so-long-threshold 250
  "Maximum line length permitted before invoking `so-long-function'.

See `so-long-detected-long-line-p' for details."
  :type 'integer
  :package-version '(so-long . "1.0"))

(defcustom so-long-max-lines 5
  "Number of non-blank, non-comment lines to test for excessive length.

If nil then all lines will be tested, until either a long line is detected,
or the end of the buffer is reached.

If `so-long-skip-leading-comments' is nil then comments and blank lines will
be counted.

See `so-long-detected-long-line-p' for details."
  :type '(choice (integer :tag "Limit")
                 (const :tag "Unlimited" nil))
  :package-version '(so-long . "1.0"))

(defcustom so-long-skip-leading-comments t
  "Non-nil to ignore all leading comments and whitespace.

If the file begins with a shebang (#!), this option also causes that line to be
ignored even if it doesn't match the buffer's comment syntax, to ensure that
comments following the shebang will be ignored.

See `so-long-detected-long-line-p' for details."
  :type 'boolean
  :package-version '(so-long . "1.0"))

(defcustom so-long-target-modes
  '(prog-mode css-mode sgml-mode nxml-mode)
  "`so-long' affects only these modes and their derivatives.

Our primary use-case is minified programming code, so `prog-mode' covers
most cases, but there are some exceptions to this.

If t, then all modes are targeted.  Note that this is only useful with a
custom `so-long-predicate', as many file types (archives and binary files,
for example) can safely contain long lines, and invoking `so-long' on such
files would prevent Emacs from handling them correctly."
  ;; Use 'symbol', as 'function' may be unknown => mismatch.
  :type '(choice (repeat :tag "Specified modes" symbol)
                 (const :tag "All modes" t))
  :package-version '(so-long . "1.0"))

(defcustom so-long-invisible-buffer-function #'so-long-deferred
  "Function called in place of `so-long' when the buffer is not displayed.

This affects the behavior of `global-so-long-mode'.

We treat invisible buffers differently from displayed buffers because, in
cases where a library is using a buffer for behind-the-scenes processing,
it might be surprising if that buffer were unexpectedly manipulated by
`so-long' (which might in turn lead to confusing errors for the user).
Invisible buffers are less likely to cause performance issues related to
long lines, so this differentiation is generally satisfactory.

The default value `so-long-deferred' prevents `global-so-long-mode' from
triggering `so-long' for any given buffer until such time as the buffer is
displayed in a window.

\(Note that buffers are normally invisible at this point -- when `find-file'
is used, the buffer is not displayed in a window until a short time after
`global-so-long-mode' has seen it.)

The value nil or `so-long' means that `so-long' will be called directly; in
which case it may be problematic for `so-long-variable-overrides' to enable
`buffer-read-only', or for `so-long-action' to be set to `so-long-mode'.
This is because the buffer may not be intended to be displayed at all, and
the mentioned options might interfere with some intended processing."
  :type '(radio (const so-long-deferred)
                (const :tag "nil: Call so-long as normal" nil)
                (function :tag "Custom function"))
  :package-version '(so-long . "1.0"))

(defcustom so-long-predicate 'so-long-detected-long-line-p
  "Function, called after `set-auto-mode' to decide whether action is needed.

Only called if the major mode is a member of `so-long-target-modes'.

The specified function will be called with no arguments.  If it returns non-nil
then `so-long' will be invoked.

Defaults to `so-long-detected-long-line-p'."
  :type '(radio (const so-long-detected-long-line-p)
                (function :tag "Custom function"))
  :package-version '(so-long . "1.0"))

;; Silence byte-compiler warning.  `so-long-action-alist' is defined below
;; as a user option; but the definition sequence required for its setter
;; function means we also need to declare it beforehand.
(defvar so-long-action-alist)

(defun so-long--action-type ()
  "Generate a :type for `so-long-action' based on `so-long-action-alist'."
  ;; :type seemingly cannot be a form to be evaluated on demand, so we
  ;; endeavor to keep it up-to-date with `so-long-action-alist' by
  ;; calling this from `so-long--action-alist-setter'.
  `(radio ,@(mapcar (lambda (x) (list 'const :tag (cadr x) (car x)))
                    (assq-delete-all nil so-long-action-alist))
          (const :tag "Do nothing" nil)))

(defun so-long--action-alist-setter (option value)
  "The customize :set function for `so-long-action-alist'."
  ;; Set the value as normal.
  (set-default option value)
  ;; Update the :type of `so-long-action' to present the updated values.
  (put 'so-long-action 'custom-type (so-long--action-type)))

(defcustom so-long-action-alist
  '((so-long-mode
     "Change major mode to so-long-mode"
     so-long-mode
     so-long-mode-revert)
    (so-long-minor-mode
     "Enable so-long-minor-mode"
     turn-on-so-long-minor-mode
     turn-off-so-long-minor-mode)
    (longlines-mode
     "Enable longlines-mode"
     so-long-function-longlines-mode
     so-long-revert-function-longlines-mode))
  "Options for `so-long-action'.

Each element is a list comprising (KEY LABEL ACTION REVERT)

KEY is a symbol which is a valid value for `so-long-action', and LABEL is a
string which describes and represents the key in that option's customize
interface, and in the \"So Long\" menu.  ACTION and REVERT are functions:

ACTION will be the `so-long-function' value when `so-long' is called, and
REVERT will be the `so-long-revert-function' value, if `so-long-revert' is
subsequently called."
  :type '(alist :key-type (symbol :tag "Key" :value <action>)
                :value-type (list (string :tag "Label" :value "<description>")
                                  (function :tag "Action")
                                  (function :tag "Revert")))
  :set #'so-long--action-alist-setter
  :package-version '(so-long . "1.0"))
(put 'so-long-action-alist 'risky-local-variable t)

(defcustom so-long-action 'so-long-mode
  "The action taken by `so-long' when long lines are detected.

\(Long lines are determined by `so-long-predicate' after `set-auto-mode'.)

The value is a key to one of the options defined by `so-long-action-alist'.

The default action is to replace the original major mode with `so-long-mode'.
Alternatively, the `so-long-minor-mode' action retains the original major mode
while still disabling minor modes and overriding variables.  These are the only
standard values for which `so-long-minor-modes' and `so-long-variable-overrides'
will be automatically processed; but custom actions can also do these things.

The value `longlines-mode' causes that minor mode to be enabled.  See
longlines.el for more details.

Each action likewise determines the behavior of `so-long-revert'.

If the value is nil, or not defined in `so-long-action-alist', then no action
will be taken."
  :type (so-long--action-type)
  :package-version '(so-long . "1.0"))

(defvar-local so-long-function nil
  "The function called by `so-long'.

This should be set in conjunction with `so-long-revert-function'.  This usually
happens automatically, based on the value of `so-long-action'.

The specified function will be called with no arguments, after which
`so-long-hook' runs.")
(put 'so-long-function 'permanent-local t)

(defvar-local so-long-revert-function nil
  "The function called by `so-long-revert'.

This should be set in conjunction with `so-long-function'.  This usually
happens automatically, based on the value of `so-long-action'.

The specified function will be called with no arguments, after which
`so-long-revert-hook' runs.")
(put 'so-long-revert-function 'permanent-local t)

(defun so-long-function (&optional action-arg)
  "The value of `so-long-function', else derive from `so-long-action'.

If ACTION-ARG is provided, it is used in place of `so-long-action'."
  (or so-long-function
      (and (or action-arg
               (setq action-arg so-long-action))
           (let ((action (assq action-arg so-long-action-alist)))
             (nth 2 action)))))

(defun so-long-revert-function (&optional action-arg)
  "The value of `so-long-revert-function', else derive from `so-long-action'.

If ACTION-ARG is provided, it is used in place of `so-long-action'."
  (or so-long-revert-function
      (and (or action-arg
               (setq action-arg so-long-action))
           (let ((action (assq action-arg so-long-action-alist)))
             (nth 3 action)))))

(defcustom so-long-file-local-mode-function 'so-long-mode-downgrade
  "Function to call during `set-auto-mode' when a file-local mode is set.

The specified function will be called with a single argument, being the
file-local mode which was established.

This happens before `so-long' is called, and so this function can modify the
subsequent action.

The value `so-long-mode-downgrade' means `so-long-minor-mode' will be used in
place of `so-long-mode' -- therefore respecting the file-local mode value, yet
still overriding minor modes and variables (as if `so-long-action' had been set
to `so-long-minor-mode').

The value `so-long-inhibit' means that so-long will not take any action at all
for this file.

If nil, then do not treat files with file-local modes any differently to other
files.

Note that this function is called if a file-local mode is set even if `so-long'
will not be called, and also if the file-local mode is `so-long-mode'.  Custom
functions may need to test for these cases -- see `so-long-mode-downgrade' for
an example."
  :type '(radio (const so-long-mode-downgrade)
                (const so-long-inhibit)
                (const :tag "nil: Use so-long-function as normal" nil)
                (function :tag "Custom function"))
  :package-version '(so-long . "1.0"))
(make-variable-buffer-local 'so-long-file-local-mode-function)

;; `provided-mode-derived-p' was added in 26.1
(unless (fboundp 'provided-mode-derived-p)
  (defun provided-mode-derived-p (mode &rest modes)
    "Non-nil if MODE is derived from one of MODES.
Uses the `derived-mode-parent' property of the symbol to trace backwards.
If you just want to check `major-mode', use `derived-mode-p'."
    (while (and (not (memq mode modes))
                (setq mode (get mode 'derived-mode-parent))))
    mode))

(defun so-long-handle-file-local-mode (mode)
  "Wrapper for calling `so-long-file-local-mode-function'.

The function is called with one argument, MODE, being the file-local mode which
was established."
  ;; Handle the special case whereby the file-local mode was `so-long-mode'.
  ;; In this instance we set `so-long--inhibited', because the file-local mode
  ;; is already going to do everything that is wanted.
  (when (provided-mode-derived-p mode 'so-long-mode)
    (setq so-long--inhibited t))
  ;; Call `so-long-file-local-mode-function'.
  (when so-long-file-local-mode-function
    (funcall so-long-file-local-mode-function mode)))

(defcustom so-long-minor-modes
  ;; In sorted groups.
  '(font-lock-mode ;; (Generally the most important).
    ;; Other standard minor modes:
    display-line-numbers-mode
    flymake-mode
    flyspell-mode
    goto-address-mode
    goto-address-prog-mode
    hi-lock-mode
    highlight-changes-mode
    hl-line-mode
    linum-mode
    nlinum-mode
    prettify-symbols-mode
    visual-line-mode
    whitespace-mode
    ;; Known third-party modes-of-interest:
    diff-hl-amend-mode
    diff-hl-flydiff-mode
    diff-hl-mode
    dtrt-indent-mode
    flycheck-mode
    hl-sexp-mode
    idle-highlight-mode
    rainbow-delimiters-mode
    )
  ;; It's not clear to me whether all of these would be problematic, but they
  ;; seemed like reasonable targets.  Some are certainly excessive in smaller
  ;; buffers of minified code, but we should be aiming to maximize performance
  ;; by default, so that Emacs is as responsive as we can manage in even very
  ;; large buffers of minified code.
  "List of buffer-local minor modes to explicitly disable.

The ones which were originally enabled in the buffer are disabled by calling
them with the numeric argument 0.  Unknown modes, and modes which were not
enabled, are ignored.

This happens after any globalized minor modes have acted, so that buffer-local
modes controlled by globalized modes can also be targeted.

By default this happens if `so-long-action' is set to either `so-long-mode'
or `so-long-minor-mode'.  If `so-long-revert' is subsequently invoked, then the
disabled modes are re-enabled by calling them with the numeric argument 1.

`so-long-hook' can be used where more custom behavior is desired.

Please submit bug reports to recommend additional modes for this list, whether
they are in Emacs core, GNU ELPA, or elsewhere."
  :type '(repeat symbol) ;; not function, as may be unknown => mismatch.
  :package-version '(so-long . "1.0"))

(defcustom so-long-variable-overrides
  '((bidi-inhibit-bpa . t)
    (bidi-paragraph-direction . left-to-right)
    (buffer-read-only . t)
    (global-hl-line-mode . nil)
    (line-move-visual . t)
    (show-paren-mode . nil)
    (truncate-lines . nil)
    (which-func-mode . nil))
  "Variables to override, and the values to override them with.

The variables are given buffer-local values.  By default this happens if
`so-long-action' is set to either `so-long-mode' or `so-long-minor-mode'.

If `so-long-revert' is subsequently invoked, then the variables are restored
to their original states.

The combination of `line-move-visual' (enabled) and `truncate-lines' (disabled)
is important for maximizing responsiveness when moving vertically within an
extremely long line, as otherwise the full length of the line may need to be
scanned to find the next position."
  :type '(alist :key-type (variable :tag "Variable")
                :value-type (sexp :tag "Value"))
  :options '((bidi-inhibit-bpa boolean)
             (bidi-paragraph-direction (choice (const left-to-right)
                                               (const right-to-left)
                                               (const nil)))
             (buffer-read-only boolean)
             (global-hl-line-mode boolean)
             (line-move-visual boolean)
             (show-paren-mode boolean)
             (truncate-lines boolean)
             (which-func-mode boolean))
  :package-version '(so-long . "1.0"))

(defcustom so-long-hook nil
  "List of functions to call after `so-long' is called.

See also `so-long-revert-hook'."
  :type 'hook
  :package-version '(so-long . "1.0"))

(defcustom so-long-revert-hook nil
  "List of functions to call after `so-long-revert' is called.

See also `so-long-hook'."
  :type 'hook
  :package-version '(so-long . "1.0"))

(defcustom so-long-mode-line-label "So Long"
  "Text label of `so-long-mode-line-info' when long lines are detected.

If nil, no mode line indicator will be displayed."
  :type '(choice (string :tag "String")
                 (const :tag "None" nil))
  :package-version '(so-long . "1.0"))

(defface so-long-mode-line-active
  '((t :inherit mode-line-emphasis))
  "Face for `so-long-mode-line-info' when mitigation is active."
  :package-version '(so-long . "1.0"))

(defface so-long-mode-line-inactive
  '((t :inherit mode-line-inactive))
  "Face for `so-long-mode-line-info' when mitigation has been reverted."
  :package-version '(so-long . "1.0"))

;; Modes that go slowly and line lengths excessive
;; Font-lock performance becoming oppressive
;; All of my CPU tied up with strings
;; These are a few of my least-favorite things

(defvar-local so-long-original-values nil
  "Alist holding the buffer's original `major-mode' value, and other data.

Any values to be restored by `so-long-revert' can be stored here by the
`so-long-function' or during `so-long-hook'.  `so-long' itself stores the
original states for `so-long-variable-overrides' and `so-long-minor-modes',
so these values are available to custom actions by default.

See also `so-long-remember' and `so-long-original'.")
(put 'so-long-original-values 'permanent-local t)

(defun so-long-original (key &optional exists)
  "Return the current value for KEY in `so-long-original-values'.

If you need to differentiate between a stored value of nil and no stored value
at all, make EXISTS non-nil.  This then returns the result of `assq' directly:
nil if no value was set, and a cons cell otherwise."
  (if exists
      (assq key so-long-original-values)
    (cadr (assq key so-long-original-values))))

(defun so-long-remember (variable)
  "Store the value of VARIABLE in `so-long-original-values'.

We additionally store a boolean value which indicates whether that value was
buffer-local."
  (when (boundp variable)
    (setq so-long-original-values
          (assq-delete-all variable so-long-original-values))
    (push (list variable
                (symbol-value variable)
                (local-variable-p variable))
          so-long-original-values)))

(defun so-long-remember-all (&optional reset)
  "Remember the current variable and minor mode values.

Stores the existing value for each entry in `so-long-variable-overrides'.
Stores the name of each enabled mode from the list `so-long-minor-modes'.

If RESET is non-nil, remove any existing values before storing the new ones."
  (when reset
    (setq so-long-original-values nil))
  (dolist (ovar so-long-variable-overrides)
    (so-long-remember (car ovar)))
  (dolist (mode so-long-minor-modes)
    (when (and (boundp mode) mode)
      (so-long-remember mode))))

(defun so-long-menu ()
  "Dynamically generate the \"So Long\" menu."
  ;; (info "(elisp) Menu Example")
  (let ((map (make-sparse-keymap "So Long"))
        (help-map (make-sparse-keymap "Help")))
    ;; `so-long-revert'.
    (define-key-after map [so-long-revert]
      '(menu-item "Revert to normal" so-long-revert
                  :enable (and so-long-revert-function
                               so-long--active)))
    ;; `so-long-menu-item-replace-action' over `so-long-action-alist'.
    (define-key-after map [so-long-actions-separator]
      '(menu-item "--"))
    (dolist (item so-long-action-alist)
      (cl-destructuring-bind (key label actionfunc revertfunc)
          item
        (define-key-after map (vector key)
          `(menu-item
            ,label
            ,(let ((sym (make-symbol "so-long-menu-item-replace-action")))
               ;; We make a symbol so that `describe-key' on the menu item
               ;; produces something more descriptive than byte code.  There is
               ;; no interned `so-long-menu-item-replace-action' which might
               ;; make this slightly confusing -- but only in the rare situation
               ;; when someone uses `describe-key' on one of these menu items,
               ;; and then wants to find more information.  We mitigate this by
               ;; making the following docstring very clear.
               (defalias sym (lambda () (interactive "@") (so-long key))
                 ;; We use "@" as commands in the mode-line menu may be
                 ;; triggered by mouse when some other window is selected.
                 "Revert the current action and invoke the chosen replacement.

This command calls `so-long' with the selected action as an argument.")
               sym)
            :enable (not (and so-long--active
                              (eq ',actionfunc so-long-function)
                              (eq ',revertfunc so-long-revert-function)))))))
    ;; "Help" sub-menu.
    (define-key-after map [so-long-help-separator]
      '(menu-item "--"))
    (define-key-after map [so-long-help]
      `(menu-item "Help" ,help-map))
    (define-key-after help-map [so-long-commentary]
      '(menu-item "Commentary" so-long-commentary))
    (define-key-after help-map [so-long-customize]
      '(menu-item "Customize" so-long-customize))
    map))

;;;###autoload
(defun so-long-commentary ()
  "View the so-long documentation in `outline-mode'."
  (interactive)
  (let ((buf "*So Long: Commentary*"))
    (when (buffer-live-p (get-buffer buf))
      (kill-buffer buf))
    ;; Use `finder-commentary' to generate the buffer.
    (require 'finder)
    (cl-letf (((symbol-function 'finder-summary) #'ignore))
      (finder-commentary "so-long"))
    (let ((inhibit-read-only t))
      (if (looking-at "^Commentary:\n\n")
          (replace-match "so-long.el\n\n")
        (insert "so-long.el\n")
        (forward-line 1))
      (save-excursion
        (while (re-search-forward "^-+$" nil :noerror)
          (replace-match ""))))
    (rename-buffer buf)
    ;; Enable `outline-mode' and `view-mode' for user convenience.
    (outline-mode)
    (declare-function outline-next-visible-heading "outline")
    (declare-function outline-previous-visible-heading "outline")
    (declare-function outline-toggle-children "outline")
    (declare-function outline-toggle-children "outline")
    (view-mode 1)
    ;; Add some custom local bindings.
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "TAB") #'outline-toggle-children)
      (define-key map (kbd "<M-tab>") #'outline-toggle-children)
      (define-key map (kbd "M-n") #'outline-next-visible-heading)
      (define-key map (kbd "M-p") #'outline-previous-visible-heading)
      (set-keymap-parent map (current-local-map))
      (use-local-map map))
    ;; Display the So Long menu.
    (so-long--ensure-enabled)
    (let ((so-long-action nil))
      (so-long))))

;;;###autoload
(defun so-long-customize ()
  "Open the so-long `customize' group."
  (interactive)
  (customize-group 'so-long))

(defvar-local so-long-mode-line-info nil
  "Mode line construct displayed when `so-long' has been triggered.

Displayed as part of `mode-line-misc-info'.

`so-long-mode-line-label' defines the text to be displayed (if any).

Face `so-long-mode-line-active' is used while mitigation is active, and
`so-long-mode-line-inactive' is used if `so-long-revert' is called.

Not displayed when `so-long-mode' is enabled, as the major mode construct
serves the same purpose.")

;; Ensure we can display text properties on this value in the mode line.
;; See (info "(elisp) Mode Line Data") or (info "(elisp) Properties in Mode").
(put 'so-long-mode-line-info 'risky-local-variable t)

(defun so-long-mode-line-info ()
  "Return the mode line construct for variable `so-long-mode-line-info'."
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<mode-line> <down-mouse-1>")
      `(menu-item "" nil
                  :filter ,(lambda (_cmd) (so-long-menu))))
    ;; Mode line construct.
    ;; n.b. It's necessary for `so-long-mode-line-info' to have a non-nil
    ;; risky-local-variable property, as otherwise the text properties won't
    ;; be rendered.
    `(so-long-mode-line-label
      ("" (:eval (propertize so-long-mode-line-label
                             'mouse-face 'highlight
                             'keymap ',map
                             'help-echo t ;; Suppress the mode-line value
                             'face (if so-long--active
                                       'so-long-mode-line-active
                                     'so-long-mode-line-inactive)))
       " "))))

;; When the line's long
;; When the mode's slow
;; When Emacs is sad
;; We change automatically to faster code
;; And then I won't feel so mad

(defun so-long-detected-long-line-p ()
  "Determine whether the current buffer contains long lines.

Following any initial comments and blank lines, the next N lines of the buffer
will be tested for excessive length (where \"excessive\" means above
`so-long-threshold', and N is `so-long-max-lines').

Returns non-nil if any such excessive-length line is detected.

If `so-long-skip-leading-comments' is nil then the N lines will be counted
starting from the first line of the buffer.  In this instance you will likely
want to increase `so-long-max-lines' to allow for possible comments.

This is the default value of `so-long-predicate'."
  (let ((count 0) start)
    (save-excursion
      (goto-char (point-min))
      (when so-long-skip-leading-comments
        ;; Skip the shebang line, if any.  This is not necessarily comment
        ;; syntax, so we need to treat it specially.
        (when (looking-at "#!")
          (forward-line 1))
        ;; Move past any leading whitespace and/or comments.
        ;; We use narrowing to limit the amount of text being processed at any
        ;; given time, where possible, as this makes things more efficient.
        (setq start (point))
        (while (save-restriction
                 (narrow-to-region start (min (+ (point) so-long-threshold)
                                              (point-max)))
                 (goto-char start)
                 ;; Possibilities for `comment-forward' are:
                 ;; 0. No comment; no movement; return nil.
                 ;; 1. Comment is <= point-max; move end of comment; return t.
                 ;; 2. Comment is truncated; move point-max; return nil.
                 ;; 3. Only whitespace; move end of WS; return nil.
                 (prog1 (or (comment-forward 1) ;; Moved past a comment.
                            (and (eobp) ;; Truncated, or WS up to point-max.
                                 (progn ;; Widen and retry.
                                   (widen)
                                   (goto-char start)
                                   (comment-forward 1))))
                   ;; Otherwise there was no comment, and we return nil.
                   ;; If there was whitespace, we moved past it.
                   (setq start (point)))))
        ;; We're at the first non-comment line, but we may have moved past
        ;; indentation whitespace, so move back to the beginning of the line
        ;; unless we're at the end of the buffer (in which case there was no
        ;; non-comment/whitespace content in the buffer at all).
        (unless (eobp)
          (forward-line 0)))
      ;; Start looking for long lines.
      ;; `while' will ultimately return nil if we do not `throw' a result.
      (catch 'excessive
        (while (and (not (eobp))
                    (or (not so-long-max-lines)
                        (< count so-long-max-lines)))
          (setq start (point))
          (save-restriction
            (narrow-to-region start (min (+ start 1 so-long-threshold)
                                         (point-max)))
            (forward-line 1))
          ;; If point is not now at the beginning of a line, then the previous
          ;; line was long -- with the exception of when point is at the end of
          ;; the buffer (bearing in mind that we have widened again), in which
          ;; case there was a short final line with no newline.  There is an
          ;; edge case when such a final line is exactly (1+ so-long-threshold)
          ;; chars long, so if we're at (eobp) we need to verify the length in
          ;; order to be consistent.
          (unless (or (bolp)
                      (and (eobp) (<= (- (point) start)
                                      so-long-threshold)))
            (throw 'excessive t))
          (setq count (1+ count)))))))

(defun so-long-function-longlines-mode ()
  "Enable minor mode `longlines-mode'."
  (require 'longlines)
  (so-long-remember 'longlines-mode)
  (longlines-mode 1))

(defun so-long-revert-function-longlines-mode ()
  "Restore original state of `longlines-mode'."
  (require 'longlines)
  (let ((state (so-long-original 'longlines-mode :exists)))
    (if state
        (unless (equal (cadr state) longlines-mode)
          (longlines-mode (if (cadr state) 1 0)))
      (longlines-mode 0))))

(defun turn-on-so-long-minor-mode ()
  "Enable minor mode `so-long-minor-mode'."
  (so-long-minor-mode 1))

(defun turn-off-so-long-minor-mode ()
  "Disable minor mode `so-long-minor-mode'."
  (so-long-minor-mode 0))

;;;###autoload
(define-minor-mode so-long-minor-mode
  "This is the minor mode equivalent of `so-long-mode'.

Any active minor modes listed in `so-long-minor-modes' are disabled for the
current buffer, and buffer-local values are assigned to variables in accordance
with `so-long-variable-overrides'.

This minor mode is a standard `so-long-action' option."
  nil nil nil
  (if so-long-minor-mode ;; We are enabling the mode.
      (progn
        ;; Housekeeping.  `so-long-minor-mode' might be invoked directly rather
        ;; than via `so-long', so replicate the necessary behaviors.  The minor
        ;; mode also cares about whether `so-long' was already active, as we do
        ;; not want to remember values which were potentially overridden already.
        (unless (or so-long--calling so-long--active)
          (so-long--ensure-enabled)
          (setq so-long--active t
                so-long-detected-p t
                so-long-function 'turn-on-so-long-minor-mode
                so-long-revert-function 'turn-off-so-long-minor-mode)
          (so-long-remember-all :reset)
          (unless (derived-mode-p 'so-long-mode)
            (setq so-long-mode-line-info (so-long-mode-line-info))))
        ;; Now perform the overrides.
        (so-long-disable-minor-modes)
        (so-long-override-variables))
    ;; We are disabling the mode.
    (unless so-long--calling ;; Housekeeping.
      (when (eq so-long-function 'turn-on-so-long-minor-mode)
        (setq so-long--active nil))
      (unless (derived-mode-p 'so-long-mode)
        (setq so-long-mode-line-info (so-long-mode-line-info))))
    ;; Restore the overridden settings.
    (so-long-restore-minor-modes)
    (so-long-restore-variables)))

;; How do you solve a problem like a long line?
;; How do you stop a mode from slowing down?
;; How do you cope with processing a long line?
;; A bit of advice! A mode! A workaround!

(defvar so-long-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'so-long-revert)
    ;; Define the major mode menu.  We have an awkward issue whereby
    ;; [menu-bar so-long] is already defined in the global map and is
    ;; :visible so-long-detected-p, but we also want this to be
    ;; available via the major mode construct in the mode line.
    ;; The following achieves the desired end result, as :visible nil
    ;; prevents this from duplicating its contents in the menu bar,
    ;; but still includes it in the mode line.
    (define-key map [menu-bar so-long]
      `(menu-item "" nil
                  :visible nil
                  :filter ,(lambda (_cmd) (so-long-menu))))
    map)
  "Major mode keymap and menu for `so-long-mode'.")

;;;###autoload
(define-derived-mode so-long-mode nil "So Long"
  "This major mode is the default `so-long-action' option.

The normal reason for this mode being active is that `global-so-long-mode' is
enabled, and `so-long-predicate' has detected that the file contains long lines.

Many Emacs modes struggle with buffers which contain excessively long lines,
and may consequently cause unacceptable performance issues.

This is commonly on account of \"minified\" code (i.e. code that has been
compacted into the smallest file size possible, which often entails removing
newlines should they not be strictly necessary).  These kinds of files are
typically not intended to be edited, so not providing the usual editing mode
in these cases will rarely be an issue.

This major mode disables any active minor modes listed in `so-long-minor-modes'
for the current buffer, and buffer-local values are assigned to variables in
accordance with `so-long-variable-overrides'.

To restore the original major mode (along with the minor modes and variable
values), despite potential performance issues, type \\[so-long-revert].

Use \\[so-long-commentary] for more information.

Use \\[so-long-customize] to configure the behavior."
  ;; Housekeeping.  `so-long-mode' might be invoked directly rather than via
  ;; `so-long', so replicate the necessary behaviors.  We could use this same
  ;; test in `so-long-after-change-major-mode' to run `so-long-hook', but that's
  ;; not so obviously the right thing to do, so I've omitted it for now.
  (unless so-long--calling
    (so-long--ensure-enabled)
    (setq so-long--active t
          so-long-detected-p t
          so-long-function 'so-long-mode
          so-long-revert-function 'so-long-mode-revert))
  ;; Use `after-change-major-mode-hook' to disable minor modes and override
  ;; variables.  Append, to act after any globalized modes have acted.
  (add-hook 'after-change-major-mode-hook
            'so-long-after-change-major-mode :append :local)
  ;; Override variables.  This is the first of two instances where we do this
  ;; (the other being `so-long-after-change-major-mode').  It is desirable to
  ;; set variables here in order to cover cases where the setting of a variable
  ;; influences how a global minor mode behaves in this buffer.
  (so-long-override-variables)
  ;; Hide redundant mode-line information (our major mode info replicates this).
  (setq so-long-mode-line-info nil)
  ;; Inform the user about our major mode hijacking.
  (unless (or so-long--inhibited so-long--set-auto-mode)
    (message (concat "Changed to %s (from %s)"
                     (unless (or (eq this-command 'so-long)
                                 (and (symbolp this-command)
                                      (provided-mode-derived-p this-command
                                                               'so-long-mode)))
                       " on account of line length")
                     ".  %s to revert.")
             major-mode
             (or (so-long-original 'major-mode) "<unknown>")
             (substitute-command-keys "\\[so-long-revert]"))))

(defun so-long--change-major-mode ()
  ;; Advice, enabled with:
  ;; (advice-add 'so-long-mode :before #'so-long--change-major-mode)
  ;;
  ;; n.b. `major-mode-suspend' and `major-mode-restore' are new in Emacs 27, and
  ;; related to what we're doing here; but it's not worth going to the effort of
  ;; using those (conditionally, only for 27+) when we have our own framework
  ;; for remembering and restoring this buffer state (amongst other things).
  "Ensure that `so-long-mode' knows the original `major-mode'.

This advice acts before `so-long-mode', with the previous mode still active."
  (unless (derived-mode-p 'so-long-mode)
    ;; Housekeeping.  `so-long-mode' might be invoked directly rather than
    ;; via `so-long', so replicate the necessary behaviors.
    (unless so-long--calling
      (so-long-remember-all :reset))
    ;; Remember the original major mode, regardless.
    (so-long-remember 'major-mode)))

(advice-add 'so-long-mode :before #'so-long--change-major-mode)

(defun so-long-after-change-major-mode ()
  "Run by `so-long-mode' in `after-change-major-mode-hook'.

Calls `so-long-disable-minor-modes' and `so-long-override-variables'."
  ;; Disable minor modes.
  (so-long-disable-minor-modes)
  ;; Override variables (again).  We already did this in `so-long-mode' in
  ;; order that variables which affect global/globalized minor modes can have
  ;; that effect; however it's feasible that one of the minor modes disabled
  ;; above might have reverted one of these variables, so we re-enforce them.
  ;; (For example, disabling `visual-line-mode' sets `line-move-visual' to
  ;; nil, when for our purposes it is preferable for it to be non-nil).
  (so-long-override-variables))

(defun so-long-disable-minor-modes ()
  "Disable any active minor modes listed in `so-long-minor-modes'."
  (dolist (mode so-long-minor-modes)
    (when (and (boundp mode) mode)
      (funcall mode 0))))

(defun so-long-restore-minor-modes ()
  "Restore the minor modes which were disabled.

The modes are enabled in accordance with what was remembered in `so-long'."
  (dolist (mode so-long-minor-modes)
    (when (and (so-long-original mode)
               (boundp mode)
               (not (symbol-value mode)))
      (funcall mode 1))))

(defun so-long-override-variables ()
  "Set the buffer-local values defined by `so-long-variable-overrides'."
  (dolist (ovar so-long-variable-overrides)
    (set (make-local-variable (car ovar)) (cdr ovar))))

(defun so-long-restore-variables ()
  "Restore the remembered values for the overridden variables.

The variables are set in accordance with what was remembered in `so-long'."
  (dolist (ovar so-long-variable-overrides)
    (so-long-restore-variable (car ovar))))

(defun so-long-restore-variable (variable)
  "Restore the remembered value (if any) for VARIABLE."
  ;; In the instance where `so-long-mode-revert' has just reverted the major
  ;; mode, note that `kill-all-local-variables' was already called by the
  ;; original mode function, and so these 'overridden' variables may now have
  ;; global rather than buffer-local values.
  (let* ((remembered (so-long-original variable :exists))
         (originally-local (nth 2 remembered)))
    (if originally-local
        ;; The variable originally existed with a buffer-local value, so we
        ;; restore it as such (even if the global value is a match).
        (set (make-local-variable variable) (cadr remembered))
      ;; Either this variable did not exist initially, or it did not have a
      ;; buffer-local value at that time.  In either case we kill the current
      ;; buffer-local value (if any) in order to restore the original state.
      ;;
      ;; It's possible that the global value has *changed* in the interim; but
      ;; we can't know whether it's best to use the new global value, or retain
      ;; the old value as a buffer-local value, so we keep it simple.
      (kill-local-variable variable))))

(defun so-long-mode-revert ()
  "Call the `major-mode' which was selected before `so-long-mode' replaced it.

Re-process local variables, and restore overridden variables and minor modes.

This is the `so-long-revert-function' for `so-long-mode'."
  (interactive)
  (let ((so-long-original-mode (so-long-original 'major-mode)))
    (unless so-long-original-mode
      (error "Original mode unknown."))
    (funcall so-long-original-mode)
    ;; Emacs 26+ has already called `hack-local-variables' (during
    ;; `run-mode-hooks'; provided there was a `buffer-file-name'), but for older
    ;; versions we need to call it here.  In Emacs 26+ the revised 'HANDLE-MODE'
    ;; argument is set to `no-mode' (being the non-nil-and-non-t behavior),
    ;; which we mimic here by binding `so-long--hack-local-variables-no-mode',
    ;; in order to prevent a local 'mode' variable from clobbering the major
    ;; mode we have just called.
    (when (< emacs-major-version 26)
      (let ((so-long--hack-local-variables-no-mode t))
        (hack-local-variables)))
    ;; Restore minor modes.
    (so-long-restore-minor-modes)
    ;; Restore overridden variables.
    ;; `kill-all-local-variables' was already called by the original mode
    ;; function, so we may be seeing global values.
    (so-long-restore-variables)
    ;; Restore the mode line construct.
    (unless (derived-mode-p 'so-long-mode)
      (setq so-long-mode-line-info (so-long-mode-line-info)))))

(defun so-long-mode-downgrade (&optional mode)
  "The default value for `so-long-file-local-mode-function'.

A buffer-local \"downgrade\" from `so-long-mode' to `so-long-minor-mode'.

When `so-long-function' is set to `so-long-mode', then we change it to
`turn-on-so-long-minor-mode' instead -- retaining the file-local major
mode, but still doing everything else that `so-long-mode' would have done.
`so-long-revert-function' is likewise updated.

If `so-long-function' has any value other than `so-long-mode', we do nothing,
as if `so-long-file-local-mode-function' was nil.

We also do nothing if MODE (the file-local mode) has the value `so-long-mode',
because we do not want to downgrade the major mode in that scenario."
  ;; Do nothing if the file-local mode was `so-long-mode'.
  (unless (provided-mode-derived-p mode 'so-long-mode)
    ;; Act only if `so-long-mode' would be enabled by the current action.
    (when (and (symbolp (so-long-function))
               (provided-mode-derived-p (so-long-function) 'so-long-mode))
      ;; Downgrade from `so-long-mode' to the `so-long-minor-mode' behavior.
      (setq so-long-function 'turn-on-so-long-minor-mode
            so-long-revert-function 'turn-off-so-long-minor-mode))))

(defun so-long-inhibit (&optional _mode)
  "Prevent so-long from having any effect at all.

This is a `so-long-file-local-mode-function' option."
  (setq so-long--inhibited t))

(defun so-long--check-header-modes ()
  ;; See also "Files with a file-local 'mode'" in the Commentary.
  "Handle the header-comments processing in `set-auto-mode'.

`set-auto-mode' has some special-case code to handle the `mode' pseudo-variable
when set in the header comment.  This runs outside of `hack-local-variables'
and cannot be conveniently intercepted, so we are forced to replicate it here.

This special-case code will ultimately be removed from Emacs, as it exists to
deal with a deprecated feature; but until then we need to replicate it in order
to inhibit our own behavior in the presence of a header comment `mode'
declaration.

If a file-local mode is detected in the header comment, then we call the
function defined by `so-long-file-local-mode-function'."
  ;; The following code for processing MODE declarations in the header
  ;; comments is copied verbatim from `set-auto-mode', because we have
  ;; no way of intercepting it.
  ;;
  (let ((try-locals (not (inhibit-local-variables-p)))
        end _done _mode modes)
    ;; Once we drop the deprecated feature where mode: is also allowed to
    ;; specify minor-modes (ie, there can be more than one "mode:"), we can
    ;; remove this section and just let (hack-local-variables t) handle it.
    ;; Find a -*- mode tag.
    (save-excursion
      (goto-char (point-min))
      (skip-chars-forward " \t\n")
      ;; Note by design local-enable-local-variables does not matter here.
      (and enable-local-variables
           try-locals
           (setq end (set-auto-mode-1))
           (if (save-excursion (search-forward ":" end t))
               ;; Find all specifications for the `mode:' variable
               ;; and execute them left to right.
               (while (let ((case-fold-search t))
                        (or (and (looking-at "mode:")
                                 (goto-char (match-end 0)))
                            (re-search-forward "[ \t;]mode:" end t)))
                 (skip-chars-forward " \t")
                 (let ((beg (point)))
                   (if (search-forward ";" end t)
                       (forward-char -1)
                     (goto-char end))
                   (skip-chars-backward " \t")
                   (push (intern (concat (downcase (buffer-substring
                                                    beg (point)))
                                         "-mode"))
                         modes)))
             ;; Simple -*-MODE-*- case.
             (push (intern (concat (downcase (buffer-substring (point) end))
                                   "-mode"))
                   modes))))

    ;; `so-long' now processes the resulting mode list.  If any modes were
    ;; listed, we assume that one of them is a major mode.  It's possible that
    ;; this isn't true, but the buffer would remain in fundamental-mode if that
    ;; were the case, so it is very unlikely.  For the purposes of passing a
    ;; value to `so-long-handle-file-local-mode' we assume the major mode was
    ;; the first mode specified (in which case it is the last in the list).
    (when modes
      (so-long-handle-file-local-mode (car (last modes))))))

;; Lisp advice, Lisp advice
;; Every calling you greet me
;; Code of mine, redefined
;; You look happy to tweak me

(defun so-long--hack-local-variables (orig-fun &optional handle-mode &rest args)
  ;; Advice, enabled with:
  ;; (advice-add 'hack-local-variables :around #'so-long--hack-local-variables)
  ;;
  ;; See also "Files with a file-local 'mode'" in the Commentary.
  "Ensure that `so-long' defers to file-local mode declarations if necessary.

If a file-local mode is detected, then we call the function defined by
`so-long-file-local-mode-function'.

This advice acts after the HANDLE-MODE:t call to `hack-local-variables'.
\(MODE-ONLY in Emacs versions < 26).

File-local header comments are currently an exception, and are processed by
`so-long--check-header-modes' (see which for details)."
  ;; The first arg to `hack-local-variables' is HANDLE-MODE since Emacs 26.1,
  ;; and MODE-ONLY in earlier versions.  In either case we are interested in
  ;; whether it has the value `t'.
  (let ((retval (apply orig-fun handle-mode args)))
    (and (eq handle-mode t)
         retval ; A file-local mode was set.
         (so-long-handle-file-local-mode retval))
    retval))

(defun so-long--set-auto-mode (orig-fun &rest args)
  ;; Advice, enabled with:
  ;; (advice-add 'set-auto-mode :around #'so-long--set-auto-mode)
  "Maybe call `so-long' for files with very long lines.

This advice acts after `set-auto-mode' has set the buffer's major mode.

We can't act before this point, because some major modes must be exempt
\(binary file modes, for example).  Instead, we act only when the selected
major mode is a member (or derivative of a member) of `so-long-target-modes'.

`so-long-predicate' then determines whether the mode change is needed."
  (setq so-long--inhibited nil) ; is permanent-local
  (when so-long-enabled
    (so-long--check-header-modes)) ; may cause `so-long--inhibited' to be set.
  (let ((so-long--set-auto-mode t))
    ;; Call `set-auto-mode'.
    (apply orig-fun args)) ; may cause `so-long--inhibited' to be set.
  ;; Test the new major mode for long lines.
  (and so-long-enabled
       (not so-long--inhibited)
       (not so-long--calling)
       (or (eq so-long-target-modes t)
           (apply #'derived-mode-p so-long-target-modes))
       (setq so-long-detected-p (funcall so-long-predicate))
       ;; `so-long' should be called; but only if and when the buffer is
       ;; displayed in a window.  Long lines in invisible buffers are generally
       ;; not problematic, whereas it might cause problems if an invisible
       ;; buffer being used for behind-the-scenes processing is manipulated
       ;; unexpectedly.  The default `so-long-invisible-buffer-function' value
       ;; is `so-long-deferred', which arranges to call `so-long' as soon as
       ;; the buffer is displayed.
       (if (or (get-buffer-window (current-buffer) t)
               (not so-long-invisible-buffer-function))
           (so-long)
         (funcall so-long-invisible-buffer-function))))

(defun so-long--hack-one-local-variable (orig-fun var val)
  ;; Advice, enabled with:
  ;; (advice-add 'hack-one-local-variable :around
  ;;             #'so-long--hack-one-local-variable)
  "Prevent the original major mode being restored after `so-long-mode'.

This advice is needed and enabled only for Emacs versions < 26.1.

If the local `mode' pseudo-variable is used, `set-auto-mode-0' will call it
firstly, and subsequently `hack-one-local-variable' may call it again.

Usually `hack-one-local-variable' tries to avoid processing that second call,
by testing the value against `major-mode'; but as we may have changed the
major mode to `so-long-mode' by this point, that protection is insufficient
and so we need to perform our own test.

We likewise need to support an equivalent of the `no-mode' behavior in 26.1+
to ensure that `so-long-mode-revert' will not restore a file-local mode again
after it has already reverted to the original mode.

The changes to `normal-mode' in Emacs 26.1 modified the execution order, and
makes this advice unnecessary.  The relevant NEWS entry is:

** File local and directory local variables are now initialized each
time the major mode is set, not just when the file is first visited.
These local variables will thus not vanish on setting a major mode."
  (if (eq var 'mode)
      ;; Adapted directly from `hack-one-local-variable'
      (let ((mode (intern (concat (downcase (symbol-name val))
                                  "-mode"))))
        (unless (or so-long--hack-local-variables-no-mode
                    (let ((origmode (so-long-original 'major-mode)))
                      ;; We bind origmode because (indirect-function nil) is an
                      ;; error in Emacs versions < 25.1, and so we need to test
                      ;; it first.
                      (and origmode
                           (eq (indirect-function mode)
                               (indirect-function origmode)))))
          (funcall orig-fun var val)))
    ;; VAR is not the 'mode' pseudo-variable.
    (funcall orig-fun var val)))

(defun so-long-deferred ()
  "Arrange to call `so-long' if the current buffer is displayed in a window."
  ;; The first time that a window-configuration change results in the buffer
  ;; being displayed in a window, `so-long' will be called (with the window
  ;; selected and the buffer set as current).  Because `so-long' removes this
  ;; buffer-local hook value, it triggers once at most.
  (add-hook 'window-configuration-change-hook #'so-long nil :local))

;;;###autoload
(defun so-long (&optional action)
  "Invoke `so-long-action' and run `so-long-hook'.

This command is called automatically when long lines are detected, when
`global-so-long-mode' is enabled.

The effects of the action can be undone by calling `so-long-revert'.

If ACTION is provided, it is used instead of `so-long-action'.

With a prefix argument, select the action to use interactively.

If an action was already active in the buffer, it will be reverted before
invoking the new action."
  (interactive
   (list (and current-prefix-arg
              (intern
               (completing-read "Action (none): "
                                (mapcar #'car so-long-action-alist)
                                nil :require-match)))))
  ;; Ensure that `so-long-deferred' only triggers `so-long' once (at most).
  (remove-hook 'window-configuration-change-hook #'so-long :local)
  (unless so-long--calling
    ;; Revert the existing action, if any.
    (when so-long--active
      (so-long-revert))
    ;; Invoke the new action.
    (let ((so-long--calling t))
      (so-long--ensure-enabled)
      ;; ACTION takes precedence if supplied.
      (when action
        (setq so-long-function nil
              so-long-revert-function nil))
      ;; Some of these settings need to be duplicated in `so-long-mode' to cover
      ;; the case when that mode is invoked directly.
      (setq so-long-detected-p t) ;; ensure menu is present.
      (unless so-long-function
        (setq so-long-function (so-long-function action)))
      (unless so-long-revert-function
        (setq so-long-revert-function (so-long-revert-function action)))
      ;; Remember original settings.
      (so-long-remember-all :reset)
      ;; Call the configured `so-long-function'.
      (when so-long-function
        (funcall so-long-function)
        ;; Set `so-long--active' last, as it isn't permanent-local.
        (setq so-long--active t))
      ;; Display mode line info, unless we are in `so-long-mode' (which provides
      ;; equivalent information in the mode line construct for the major mode).
      (unless (derived-mode-p 'so-long-mode)
        (setq so-long-mode-line-info (so-long-mode-line-info)))
      ;; Run `so-long-hook'.
      ;; By default we set `buffer-read-only', which can cause problems if hook
      ;; functions need to modify the buffer.  We use `inhibit-read-only' to
      ;; side-step the issue (and likewise in `so-long-revert').
      (let ((inhibit-read-only t))
        (run-hooks 'so-long-hook)))))

(defun so-long-revert ()
  "Revert the active `so-long-action' and run `so-long-revert-hook'.

This undoes the effects of the `so-long' command (which is normally called
automatically by `global-so-long-mode').

For the default action, reverting will restore the original major mode, and
restore the minor modes and settings which were overridden when `so-long' was
invoked."
  (interactive "@")
  ;; We use "@" as commands in the mode-line menu may be triggered by mouse
  ;; when some other window is selected.
  (unless so-long--calling
    (let ((so-long--calling t))
      (when so-long-revert-function
        (funcall so-long-revert-function)
        (setq so-long--active nil))
      (let ((inhibit-read-only t))
        (run-hooks 'so-long-revert-hook)))))

;;;###autoload
(defun so-long-enable ()
  "Enable the so-long library's functionality.

Equivalent to calling (global-so-long-mode 1)"
  (interactive)
  (global-so-long-mode 1))

(defun so-long-disable ()
  "Disable the so-long library's functionality.

Equivalent to calling (global-so-long-mode 0)"
  (interactive)
  (global-so-long-mode 0))

(make-obsolete 'so-long-enable 'global-so-long-mode "so-long 1.0")
(make-obsolete 'so-long-disable 'global-so-long-mode "so-long 1.0")

;;;###autoload
(define-minor-mode global-so-long-mode
  "Toggle automated performance mitigation for files with long lines.

Many Emacs modes struggle with buffers which contain excessively long lines,
and may consequently cause unacceptable performance issues.

This is commonly on account of \"minified\" code (i.e. code that has been
compacted into the smallest file size possible, which often entails removing
newlines should they not be strictly necessary).

When such files are detected by `so-long-predicate', we invoke the selected
`so-long-action' to mitigate potential performance problems in the buffer.

Use \\[so-long-commentary] for more information.

Use \\[so-long-customize] to configure the behavior."
  :global t
  :group 'so-long
  (if global-so-long-mode
      ;; Enable
      (progn
        (so-long--enable)
        (advice-add 'hack-local-variables :around
                    #'so-long--hack-local-variables)
        (advice-add 'set-auto-mode :around
                    #'so-long--set-auto-mode)
        (when (< emacs-major-version 26)
          (advice-add 'hack-one-local-variable :around
                      #'so-long--hack-one-local-variable)))
    ;; Disable
    (so-long--disable)
    (advice-remove 'hack-local-variables #'so-long--hack-local-variables)
    (advice-remove 'set-auto-mode #'so-long--set-auto-mode)
    (when (< emacs-major-version 26)
      (advice-remove 'hack-one-local-variable
                     #'so-long--hack-one-local-variable))))

(put 'global-so-long-mode 'variable-documentation
     "Non-nil if the so-long library's automated functionality is enabled.

Use \\[so-long-commentary] for more information.

Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-so-long-mode'.")

(defun so-long--ensure-enabled ()
  "Enable essential functionality, if not already enabled."
  (unless so-long-enabled
    (so-long--enable)))

(defun so-long--enable ()
  "Enable functionality other than `global-so-long-mode'."
  (add-to-list 'mode-line-misc-info '("" so-long-mode-line-info))
  (define-key-after (current-global-map) [menu-bar so-long]
    `(menu-item "So Long" nil
                ;; See also `so-long-mode-map'.
                :visible (or so-long--active
                             so-long-detected-p
                             (derived-mode-p 'so-long-mode))
                :filter ,(lambda (_cmd) (so-long-menu))))
  (setq so-long-enabled t))

(defun so-long--disable ()
  "Disable functionality other than `global-so-long-mode'."
  (setq mode-line-misc-info
        (delete '("" so-long-mode-line-info) mode-line-misc-info))
  (define-key (current-global-map) [menu-bar so-long] nil)
  (setq so-long-enabled nil))

(defun so-long-unload-function ()
  "Handler for `unload-feature'."
  (condition-case err
      (progn
        (global-so-long-mode 0)
        ;; Process existing buffers.
        (dolist (buf (buffer-list))
          (with-current-buffer buf
            ;; Remove buffer-local `window-configuration-change-hook' values set
            ;; by `so-long-deferred'.
            (remove-hook 'window-configuration-change-hook #'so-long :local)
            ;; Call `so-long-revert' in all buffers where so-long is active.
            (when (bound-and-true-p so-long--active)
              (so-long-revert))))
        ;; Un-define our buffer-local variables, as `unload-feature' will not do
        ;; this automatically.  We remove them from `unload-function-defs-list'
        ;; as well, to prevent them being redefined.  n.b.: `so-long--active' is
        ;; tested (above) using `bound-and-true-p' because that is one of the
        ;; variables which we unbind (below); and if something subsequent to
        ;; this handler signals an error, the user may need to call this again.
        (defvar unload-function-defs-list)
        (dolist (var '(so-long--active
                       so-long--inhibited
                       so-long-detected-p
                       so-long-file-local-mode-function
                       so-long-function
                       so-long-minor-mode
                       so-long-mode-abbrev-table
                       so-long-mode-line-info
                       so-long-mode-syntax-table
                       so-long-original-values
                       so-long-revert-function))
          (makunbound var)
          (setq unload-function-defs-list
                (delq var unload-function-defs-list)))
        ;; Return nil if unloading was successful.  Refer to `unload-feature'.
        nil)
    ;; If any error occurred, return non-nil.
    (error (progn
             (message "Error unloading so-long: %S %S" (car err) (cdr err))
             t))))

;; Backwards-compatibility definitions.
;;
;; The following obsolete functions may exist in the user's customized hook
;; values dating from versions < 1.0, so we need to ensure that such saved
;; values will not trigger errors.
(cl-flet ((ignore () nil))
  (dolist (hookfunc '((so-long-inhibit-whitespace-mode . so-long-hook)
                      (so-long-make-buffer-read-only . so-long-hook)
                      (so-long-revert-buffer-read-only . so-long-revert-hook)
                      (so-long-inhibit-global-hl-line-mode . so-long-mode-hook)))
    (defalias (car hookfunc) #'ignore
      (format "Obsolete function.  It now does nothing.

If it appears in `%s', you should remove it."
              (cdr hookfunc)))
    (make-obsolete (car hookfunc) nil "so-long.el version 1.0")))

;; Live upgrades, for when a newer version is loaded over an older one.
;;
;; If `so-long-version' was already bound then that tells us which version we
;; should upgrade from.  If `so-long-version' is unbound then most likely there
;; was no older version loaded; however, prior to version 1.0 `so-long-version'
;; was not defined at all, and so we also need to detect that scenario, which
;; we can do by testing for the presence of a symbol which was removed in 1.0.
;;
;; The variable `so-long-mode-enabled' covers versions 0.5 - 0.7.6, which is
;; every pre-1.0 release using the name "so-long.el".
(defvar so-long-version (if (boundp 'so-long-mode-enabled)
                            "0.5" ;; >= 0.5 and < 1.0
                          so-long--latest-version)
  "The loaded version of so-long.el.")

;; Version-specific updates.
(when (version< so-long-version so-long--latest-version)
  ;; Perform each update in sequence, as necessary.
  ;; Update to version 1.0 from earlier versions:
  (when (version< so-long-version "1.0")
    (remove-hook 'change-major-mode-hook 'so-long-change-major-mode)
    (require 'advice)
    (declare-function ad-find-advice "advice")
    (declare-function ad-remove-advice "advice")
    (when (ad-find-advice 'hack-local-variables 'after 'so-long--file-local-mode)
      (ad-remove-advice 'hack-local-variables 'after 'so-long--file-local-mode)
      (ad-activate 'hack-local-variables))
    (when (ad-find-advice 'set-auto-mode 'around 'so-long--set-auto-mode)
      (ad-remove-advice 'set-auto-mode 'around 'so-long--set-auto-mode)
      (ad-activate 'set-auto-mode))
    (when (boundp 'so-long-mode-map)
      (define-key so-long-mode-map [remap so-long-mode-revert] #'so-long-revert))
    (dolist (var '(so-long-mode--inhibited
                   so-long-original-mode))
      (makunbound var))
    (dolist (func '(so-long-change-major-mode
                    so-long-check-header-modes
                    so-long-line-detected-p))
      (fmakunbound func))
    (defvar so-long-mode-enabled)
    (when so-long-mode-enabled
      (unless global-so-long-mode
        (global-so-long-mode 1)))
    (makunbound 'so-long-mode-enabled))
  ;; Update to version 1.N:
  ;; (when (version< so-long-version "1.N") ...)
  ;;
  ;; All updates completed.
  (setq so-long-version so-long--latest-version))


(provide 'so-long)

;; Local Variables:
;; emacs-lisp-docstring-fill-column: 80
;; fill-column: 80
;; indent-tabs-mode: nil
;; ispell-check-comments: exclusive
;; ispell-local-dictionary: "british"
;; End:

;; This library is extensively documented in British English, contrary to the
;; preference for American English in Emacs.  I hope the benefits of the library
;; will outweigh any discontent you may experience regarding the spelling (or
;; that you find the spelling to be an agreeable bonus).  Certain standard Emacs
;; terminology, and text quoted from elsewhere in Emacs, retains its original
;; spelling.  The following LocalWords should result in no misspellings from
;; M-x ispell-buffer (using aspell).

; LocalWords:  LocalWords british ispell aspell hunspell emacs elisp el init dir
; LocalWords:  customize customized customizing Customization globalized amongst
; LocalWords:  initialized profiler boolean minified pre redisplay config keymap
; LocalWords:  noerror selectable mapc sgml nxml hl flydiff defs arg Phil Sainty
; LocalWords:  defadvice nadvice whitespace ie bos eos eobp origmode un Un setq
; LocalWords:  docstring auf Wiedersehen longlines alist autoload Refactored Inc
; LocalWords:  MERCHANTABILITY RET REGEXP VAR ELPA WS EmacsWiki eval
; LocalWords:  rx filename filenames

;; So long, farewell, auf Wiedersehen, goodbye
;; You have to go, this code is minified
;; Goodbye!

;;; so-long.el ends here
