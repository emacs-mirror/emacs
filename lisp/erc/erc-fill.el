;;; erc-fill.el --- Filling IRC messages in various ways  -*- lexical-binding: t; -*-

;; Copyright (C) 2001-2004, 2006-2023 Free Software Foundation, Inc.

;; Author: Andreas Fuchs <asf@void.at>
;;         Mario Lang <mlang@delysid.org>
;; Maintainer: Amin Bandali <bandali@gnu.org>, F. Jason Park <jp@neverwas.me>
;; URL: https://www.emacswiki.org/emacs/ErcFilling

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

;; This package implements filling of messages sent and received.  Use
;; `erc-fill-mode' to switch it on.  Customize `erc-fill-function' to
;; change the style.

;; TODO: redo `erc-fill-wrap-nudge' using transient after ERC drops
;; support for Emacs 27.

;;; Code:

(require 'erc)
(require 'erc-stamp); for the timestamp stuff

(defgroup erc-fill nil
  "Filling means to reformat long lines in different ways."
  :group 'erc)

;;;###autoload(autoload 'erc-fill-mode "erc-fill" nil t)
(define-erc-module fill nil
  "Manage filling in ERC buffers.
ERC fill mode is a global minor mode.  When enabled, messages in
the channel buffers are filled."
  ;; FIXME ensure a consistent ordering relative to hook members from
  ;; other modules.  Ideally, this module's processing should happen
  ;; after "morphological" modifications to a message's text but
  ;; before superficial decorations.
  ((add-hook 'erc-insert-modify-hook #'erc-fill)
   (add-hook 'erc-send-modify-hook #'erc-fill))
  ((remove-hook 'erc-insert-modify-hook #'erc-fill)
   (remove-hook 'erc-send-modify-hook #'erc-fill)))

(defcustom erc-fill-prefix nil
  "Values used as `fill-prefix' for `erc-fill-variable'.
nil means fill with space, a string means fill with this string."
  :type '(choice (const nil) string))

(defcustom erc-fill-function 'erc-fill-variable
  "Function to use for filling messages.

Variable Filling with an `erc-fill-prefix' of nil:

<shortnick> this is a very very very long message with no
	    meaning at all

Variable Filling with an `erc-fill-prefix' of four spaces:

<shortnick> this is a very very very long message with no
    meaning at all

Static Filling with `erc-fill-static-center' of 27:

		<shortnick> foo bar baz
	 <a-very-long-nick> foo bar baz quuuuux
		<shortnick> this is a very very very long message with no
			    meaning at all

These two styles are implemented using `erc-fill-variable' and
`erc-fill-static'.  You can, of course, define your own filling
function.  Narrowing to the region in question is in effect while your
function is called.

A third style resembles static filling but \"wraps\" instead of
fills, thanks to `visual-line-mode' mode, which ERC automatically
enables when this option is `erc-fill-wrap' or when
`erc-fill-wrap-mode' is active.  Set `erc-fill-static-center' to
your preferred initial \"prefix\" width.  For adjusting the width
during a session, see the command `erc-fill-wrap-nudge'."
  :type '(choice (const :tag "Variable Filling" erc-fill-variable)
                 (const :tag "Static Filling" erc-fill-static)
                 (const :tag "Dynamic word-wrap" erc-fill-wrap)
                 function))

(defcustom erc-fill-static-center 27
  "Number of columns to \"outdent\" the first line of a message.
During early message handing, ERC prepends a span of
non-whitespace characters to every message, such as a bracketed
\"<nickname>\" or an `erc-notice-prefix'.  The
`erc-fill-function' variants `erc-fill-static' and
`erc-fill-wrap' look to this option to determine the amount of
padding to apply to that portion until the filled (or wrapped)
message content aligns with the indicated column.  See also
https://en.wikipedia.org/wiki/Hanging_indent."
  :type 'integer)

(defcustom erc-fill-variable-maximum-indentation 17
  "Don't indent a line after a long nick more than this many characters.
Set to nil to disable."
  :type 'integer)

(defcustom erc-fill-column 78
  "The column at which a filled paragraph is broken."
  :type 'integer)

(defcustom erc-fill-line-spacing nil
  "Extra space between messages on graphical displays.
This may need adjusting depending on how your faces are
configured.  Its value should be larger than that of the variable
`line-spacing', if set.  If unsure, try 0.5."
  :package-version '(ERC . "5.6") ; FIXME sync on release
  :type '(choice (const nil) number))

(defcustom erc-fill-spaced-commands '(PRIVMSG NOTICE)
  "Types of messages to add space between on graphical displays.
Only considered when `erc-fill-line-spacing' is non-nil."
  :package-version '(ERC . "5.6") ; FIXME sync on release
  :type '(repeat (choice integer symbol)))

(defvar-local erc-fill--function nil
  "Internal copy of `erc-fill-function'.
Takes precedence over the latter when non-nil.")

;;;###autoload
(defun erc-fill ()
  "Fill a region using the function referenced in `erc-fill-function'.
You can put this on `erc-insert-modify-hook' and/or `erc-send-modify-hook'."
  (unless (erc-string-invisible-p (buffer-substring (point-min) (point-max)))
    (when (or erc-fill--function erc-fill-function)
      ;; skip initial empty lines
      (goto-char (point-min))
      (save-match-data
        (while (and (looking-at "[ \t\n]*$")
                    (= (forward-line 1) 0))))
      (unless (eobp)
        (save-restriction
          (narrow-to-region (point) (point-max))
          (funcall (or erc-fill--function erc-fill-function))
          (when-let* ((erc-fill-line-spacing)
                      (p (point-min)))
            (widen)
            (when (or (and-let* ((cmd (get-text-property p 'erc-command)))
                        (memq cmd erc-fill-spaced-commands))
                      (and-let* ((cmd (save-excursion
                                        (forward-line -1)
                                        (get-text-property (point)
                                                           'erc-command))))
                        (memq cmd erc-fill-spaced-commands)))
              (put-text-property (1- p) p
                                 'line-spacing erc-fill-line-spacing))))))))

(defun erc-fill-static ()
  "Fills a text such that messages start at column `erc-fill-static-center'."
  (save-restriction
    (goto-char (point-min))
    (looking-at "^\\(\\S-+\\)")
    (let ((nick (match-string 1)))
        (let ((fill-column (- erc-fill-column (erc-timestamp-offset)))
              (fill-prefix (make-string erc-fill-static-center 32)))
          (insert (make-string (max 0 (- erc-fill-static-center
                                         (length nick) 1))
                               32))
          (erc-fill-regarding-timestamp))
        (erc-restore-text-properties))))

(defun erc-fill-variable ()
  "Fill from `point-min' to `point-max'."
  (let ((fill-prefix erc-fill-prefix)
        (fill-column (or erc-fill-column fill-column)))
    (goto-char (point-min))
    (if fill-prefix
        (let ((first-line-offset (make-string (erc-timestamp-offset) 32)))
          (insert first-line-offset)
          (fill-region (point-min) (point-max) t t)
          (goto-char (point-min))
          (delete-char (length first-line-offset)))
      (save-match-data
        (let* ((nickp (looking-at "^\\(\\S-+\\)"))
               (nick (if nickp
                         (match-string 1)
                       ""))
               (fill-column (- erc-fill-column (erc-timestamp-offset)))
               (fill-prefix (make-string (min (+ 1 (length nick))
                                              (- fill-column 1)
                                              (or erc-fill-variable-maximum-indentation
                                                  fill-column))
                                         32)))
          (erc-fill-regarding-timestamp))))
    (erc-restore-text-properties)))

(defvar-local erc-fill--wrap-value nil)
(defvar-local erc-fill--wrap-visual-keys nil)

(defcustom erc-fill-wrap-use-pixels t
  "Whether to calculate padding in pixels when possible.
A value of nil means ERC should use columns, which may happen
regardless, depending on the Emacs version.  This option only
matters when `erc-fill-wrap-mode' is enabled."
  :package-version '(ERC . "5.6") ; FIXME sync on release
  :type 'boolean)

(defcustom erc-fill-wrap-visual-keys 'non-input
  "Whether to retain keys defined by `visual-line-mode'.
A value of t tells ERC to use movement commands defined by
`visual-line-mode' everywhere in an ERC buffer along with visual
editing commands in the input area.  A value of nil means to
never do so.  A value of `non-input' tells ERC to act like the
value is nil in the input area and t elsewhere.  This option only
plays a role when `erc-fill-wrap-mode' is enabled."
  :package-version '(ERC . "5.6") ; FIXME sync on release
  :type '(choice (const nil) (const t) (const non-input)))

(defcustom erc-fill-wrap-merge t
  "Whether to consolidate messages from the same speaker.
This tells ERC to omit redundant speaker labels for subsequent
messages less than a day apart."
  :package-version '(ERC . "5.6") ; FIXME sync on release
  :type 'boolean)

(defun erc-fill--wrap-move (normal-cmd visual-cmd arg)
  (funcall (pcase erc-fill--wrap-visual-keys
             ('non-input
              (if (>= (point) erc-input-marker) normal-cmd visual-cmd))
             ('t visual-cmd)
             (_ normal-cmd))
           arg))

(defun erc-fill--wrap-kill-line (arg)
  "Defer to `kill-line' or `kill-visual-line'."
  (interactive "P")
  ;; ERC buffers are read-only outside of the input area, but we run
  ;; `kill-line' anyway so that users can see the error.
  (erc-fill--wrap-move #'kill-line #'kill-visual-line arg))

(defun erc-fill--wrap-beginning-of-line (arg)
  "Defer to `move-beginning-of-line' or `beginning-of-visual-line'."
  (interactive "^p")
  (let ((inhibit-field-text-motion t))
    (erc-fill--wrap-move #'move-beginning-of-line
                         #'beginning-of-visual-line arg))
  (if (get-text-property (point) 'erc-prompt)
      (goto-char erc-input-marker)
    ;; Mimic what `move-beginning-of-line' does with invisible text.
    (when-let ((erc-fill-wrap-merge)
               (empty (get-text-property (point) 'display))
               ((string-empty-p empty)))
      (goto-char (text-property-not-all (point) (pos-eol) 'display empty)))))

(defun erc-fill--wrap-end-of-line (arg)
  "Defer to `move-end-of-line' or `end-of-visual-line'."
  (interactive "^p")
  (erc-fill--wrap-move #'move-end-of-line #'end-of-visual-line arg))

(defun erc-fill-wrap-cycle-visual-movement (arg)
  "Cycle through `erc-fill-wrap-visual-keys' styles ARG times.
Go from nil to t to `non-input' and back around, but set internal
state instead of mutating `erc-fill-wrap-visual-keys'.  When ARG
is 0, reset to value of `erc-fill-wrap-visual-keys'."
  (interactive "^p")
  (when (zerop arg)
    (setq erc-fill--wrap-visual-keys erc-fill-wrap-visual-keys))
  (while (not (zerop arg))
    (cl-incf arg (- (abs arg)))
    (setq erc-fill--wrap-visual-keys (pcase erc-fill--wrap-visual-keys
                                       ('nil t)
                                       ('t 'non-input)
                                       ('non-input nil))))
  (message "erc-fill-wrap movement: %S" erc-fill--wrap-visual-keys))

(defvar-keymap erc-fill-wrap-mode-map ; Compat 29
  :doc "Keymap for ERC's `fill-wrap' module."
  :parent visual-line-mode-map
  "<remap> <kill-line>" #'erc-fill--wrap-kill-line
  "<remap> <move-end-of-line>" #'erc-fill--wrap-end-of-line
  "<remap> <move-beginning-of-line>" #'erc-fill--wrap-beginning-of-line
  "C-c a" #'erc-fill-wrap-cycle-visual-movement
  ;; Not sure if this is problematic because `erc-bol' takes no args.
  "<remap> <erc-bol>" #'erc-fill--wrap-beginning-of-line)

(defvar erc-match-mode)
(defvar erc-button-mode)
(defvar erc-match--hide-fools-offset-bounds)

(defun erc-fill--wrap-ensure-dependencies ()
  (let (missing-deps)
    (unless erc-fill-mode
      (push 'fill missing-deps)
      (erc-fill-mode +1))
    (when erc-fill-wrap-merge
      (require 'erc-button)
      (unless erc-button-mode
        (push 'button missing-deps)
        (erc-button-mode +1))
      (require 'erc-stamp)
      (unless erc-stamp-mode
        (push 'stamp missing-deps)
        (erc-stamp-mode +1)))
    (when missing-deps
      (erc--warn-once-before-connect 'erc-fill-wrap-mode
        "Enabling missing global modules %s needed by local"
        " module `fill-wrap'. This will impact \C-]all\C-] ERC"
        " sessions. Add them to `erc-modules' to avoid this"
        " warning. See Info:\"(erc) Modules\" for more."
        (mapcar (lambda (s) (format "`%s'" s)) missing-deps)))))

;;;###autoload(put 'fill-wrap 'erc--feature 'erc-fill)
(define-erc-module fill-wrap nil
  "Fill style leveraging `visual-line-mode'.
This local module displays nicks overhanging leftward to a common
offset, as determined by the option `erc-fill-static-center'.  It
depends on the `fill' and `button' modules and assumes the option
`erc-insert-timestamp-function' is `erc-insert-timestamp-right'
or the default `erc-insert-timestamp-left-and-right', so that it
can display right-hand stamps in the right margin.  A value of
`erc-insert-timestamp-left' is unsupported.  To use it, either
include `fill-wrap' in `erc-modules' or set `erc-fill-function'
to `erc-fill-wrap' (recommended).  You can also manually invoke
one of the minor-mode toggles if really necessary."
  ((erc-fill--wrap-ensure-dependencies)
   ;; Restore or initialize local state variables.
   (erc--restore-initialize-priors erc-fill-wrap-mode
     erc-fill--wrap-visual-keys erc-fill-wrap-visual-keys
     erc-fill--wrap-value erc-fill-static-center)
   (setq erc-fill--function #'erc-fill-wrap)
   ;; Internal integrations.
   (add-function :after (local 'erc-stamp--insert-date-function)
                 #'erc-fill--wrap-stamp-insert-prefixed-date)
   (when (or erc-stamp-mode (memq 'stamp erc-modules))
     (erc-stamp--display-margin-mode +1))
   (when (or (bound-and-true-p erc-match-mode) (memq 'match erc-modules))
     (require 'erc-match)
     (setq erc-match--hide-fools-offset-bounds t))
   (when erc-fill-wrap-merge
     (add-hook 'erc-button--prev-next-predicate-functions
               #'erc-fill--wrap-merged-button-p nil t))
   (visual-line-mode +1))
  ((when erc-stamp--display-margin-mode
     (erc-stamp--display-margin-mode -1))
   (kill-local-variable 'erc-fill--wrap-value)
   (kill-local-variable 'erc-fill--function)
   (kill-local-variable 'erc-fill--wrap-visual-keys)
   (remove-hook 'erc-button--prev-next-predicate-functions
                #'erc-fill--wrap-merged-button-p t)
   (remove-function (local 'erc-stamp--insert-date-function)
                    #'erc-fill--wrap-stamp-insert-prefixed-date)
   (visual-line-mode -1))
  'local)

(defvar-local erc-fill--wrap-length-function nil
  "Function to determine length of overhanging characters.
It should return an EXPR as defined by the Info node `(elisp)
Pixel Specification'.  This value should represent the width of
the overhang with all faces applied, including any enclosing
brackets (which are not normally fontified) and a trailing space.
It can also return nil to tell ERC to fall back to the default
behavior of taking the length from the first \"word\".  This
variable can be converted to a public one if needed by third
parties.")

(defvar-local erc-fill--wrap-last-msg nil)
(defvar-local erc-fill--wrap-max-lull (* 24 60 60))

(defun erc-fill--wrap-continued-message-p ()
  (prog1 (and-let*
             ((m (or erc-fill--wrap-last-msg
                     (setq erc-fill--wrap-last-msg (point-min-marker))
                     nil))
              ((< (1+ (point-min)) (- (point) 2)))
              (props (save-restriction
                       (widen)
                       (when (eq 'erc-timestamp (field-at-pos m))
                         (set-marker m (field-end m)))
                       (and (eq 'PRIVMSG (get-text-property m 'erc-command))
                            (not (eq (get-text-property m 'font-lock-face)
                                     'erc-action-face))
                            (cons (get-text-property m 'erc-timestamp)
                                  (get-text-property (1+ m) 'erc-data)))))
              (ts (pop props))
              ((not (time-less-p (erc-stamp--current-time) ts)))
              ((time-less-p (time-subtract (erc-stamp--current-time) ts)
                            erc-fill--wrap-max-lull))
              (nick  (buffer-substring-no-properties
                      (1+ (point-min)) (- (point) 2)))
              (props)
              ((erc-nick-equal-p (car props) nick))))
    (set-marker erc-fill--wrap-last-msg (point-min))))

(defun erc-fill--wrap-stamp-insert-prefixed-date (&rest args)
  "Apply `line-prefix' property to args."
  (let* ((ts-left (car args))
         (start)
         ;; Insert " " to simulate gap between <speaker> and msg beg.
         (end (save-excursion (skip-chars-backward "\n")
                              (setq start (pos-bol))
                              (insert " ")
                              (point)))
         (width (if (and erc-fill-wrap-use-pixels
                         (fboundp 'buffer-text-pixel-size))
                    (save-restriction (narrow-to-region start end)
                                      (list (car (buffer-text-pixel-size))))
                  (length (string-trim-left ts-left)))))
    (delete-region (1- end) end)
    ;; Use `point-min' instead of `start' to cover leading newilnes.
    (put-text-property (point-min) (point) 'line-prefix
                       `(space :width (- erc-fill--wrap-value ,width))))
  args)

(defun erc-fill-wrap ()
  "Use text props to mimic the effect of `erc-fill-static'.
See `erc-fill-wrap-mode' for details."
  (unless erc-fill-wrap-mode
    (erc-fill-wrap-mode +1))
  (save-excursion
    (goto-char (point-min))
    (let ((len (or (and erc-fill--wrap-length-function
                        (funcall erc-fill--wrap-length-function))
                   (progn
                     (skip-syntax-forward "^-")
                     (forward-char)
                     ;; Using the `invisible' property might make more
                     ;; sense, but that would require coordination
                     ;; with other modules, like `erc-match'.
                     (cond ((and erc-fill-wrap-merge
                                 (erc-fill--wrap-continued-message-p))
                            (put-text-property (point-min) (point)
                                               'display "")
                            0)
                           ((and erc-fill-wrap-use-pixels
                                 (fboundp 'buffer-text-pixel-size))
                            (save-restriction
                              (narrow-to-region (point-min) (point))
                              (list (car (buffer-text-pixel-size)))))
                           (t (- (point) (point-min))))))))
      (erc-put-text-properties (point-min) (1- (point-max)) ; exclude "\n"
                               '(line-prefix wrap-prefix) nil
                               `((space :width (- erc-fill--wrap-value ,len))
                                 (space :width erc-fill--wrap-value))))))

;; FIXME use own text property to avoid false positives.
(defun erc-fill--wrap-merged-button-p (point)
  (equal "" (get-text-property point 'display)))

(defun erc-fill--wrap-nudge (arg)
  (when (zerop arg)
    (setq arg (- erc-fill-static-center erc-fill--wrap-value)))
  (cl-incf erc-fill--wrap-value arg)
  arg)

(defun erc-fill-wrap-nudge (arg)
  "Adjust `erc-fill-wrap' by ARG columns.
Offer to repeat command in a manner similar to
`text-scale-adjust'.

   \\`=' Increase indentation by one column
   \\`-' Decrease indentation by one column
   \\`0' Reset indentation to the default
   \\`+' Shift right margin rightward (shrink) by one column
   \\`_' Shift right margin leftward (grow) by one column
   \\`)' Reset the right margin to the default

Note that misalignment may occur when messages contain
decorations applied by third-party modules."
  (interactive "p")
  (unless erc-fill--wrap-value
    (cl-assert (not erc-fill-wrap-mode))
    (user-error "Minor mode `erc-fill-wrap-mode' disabled"))
  (unless (get-buffer-window)
    (user-error "Command called in an undisplayed buffer"))
  (let* ((total (erc-fill--wrap-nudge arg))
         (win-ratio (/ (float (- (window-point) (window-start)))
                       (- (window-end nil t) (window-start)))))
    (when (zerop arg)
      (setq arg 1))
    (erc-compat-call
     set-transient-map
     (let ((map (make-sparse-keymap)))
       (dolist (key '(?= ?- ?0))
         (let ((a (pcase key
                    (?0 0)
                    (?- (- (abs arg)))
                    (_ (abs arg)))))
           (define-key map (vector (list key))
                       (lambda ()
                         (interactive)
                         (cl-incf total (erc-fill--wrap-nudge a))
                         (recenter (round (* win-ratio (window-height))))))))
       (dolist (key '(?\) ?_ ?+))
         (let ((a (pcase key
                    (?\) 0)
                    (?_ (- (abs arg)))
                    (?+ (abs arg)))))
           (define-key map (vector (list key))
                       (lambda ()
                         (interactive)
                         (erc-stamp--adjust-right-margin (- a))
                         (recenter (round (* win-ratio (window-height))))))))
       map)
     t
     (lambda ()
       (message "Fill prefix: %d (%+d col%s)"
                erc-fill--wrap-value total (if (> (abs total) 1) "s" "")))
     "Use %k for further adjustment"
     1)
    (recenter (round (* win-ratio (window-height))))))

(defun erc-fill-regarding-timestamp ()
  "Fills a text such that messages start at column `erc-fill-static-center'."
  (fill-region (point-min) (point-max) t t)
  (goto-char (point-min))
  (forward-line)
  (indent-rigidly (point) (point-max) (erc-timestamp-offset)))

(defun erc-timestamp-offset ()
  "Get length of timestamp if inserted left."
  (if (and (boundp 'erc-timestamp-format)
           erc-timestamp-format
           (eq erc-insert-timestamp-function 'erc-insert-timestamp-left)
           (not erc-hide-timestamps))
      (length (format-time-string erc-timestamp-format))
    0))

(provide 'erc-fill)

;;; erc-fill.el ends here
;; Local Variables:
;; generated-autoload-file: "erc-loaddefs.el"
;; End:
