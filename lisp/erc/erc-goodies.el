;;; erc-goodies.el --- Collection of ERC modules  -*- lexical-binding: t; -*-

;; Copyright (C) 2001-2024 Free Software Foundation, Inc.

;; Author: Jorgen Schaefer <forcer@forcix.cx>
;; Maintainer: Amin Bandali <bandali@gnu.org>, F. Jason Park <jp@neverwas.me>

;; Most code is taken verbatim from erc.el, see there for the original
;; authors.

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

;; This provides some small but still useful modes for ERC.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'erc)

(declare-function fringe-columns "fringe" (side &optional real))
(declare-function pulse-available-p "pulse" nil)
(declare-function pulse-momentary-highlight-overlay "pulse" (o &optional face))


;;; Automatically scroll to bottom
(defcustom erc-input-line-position nil
  "Specify where to position the input line when using `erc-scroll-to-bottom'.

This should be an integer specifying the line of the buffer on which
the input line should stay.  A value of \"-1\" would keep the input
line positioned on the last line in the buffer.  This is passed as an
argument to `recenter', unless `erc-scrolltobottom-all' is
`relaxed', in which case, ERC interprets it as additional lines
to scroll down by per message insertion (minus one for the
prompt)."
  :group 'erc-display
  :type '(choice integer (const nil)))

(defcustom erc-scrolltobottom-all nil
  "Whether to scroll all windows or just the selected one.
ERC expects this option to be configured before module
initialization.  A value of nil preserves pre-5.6 behavior, in
which scrolling only affects the selected window.  A value of t
means ERC attempts to recenter all visible windows whose point
resides in the input area.

A value of `relaxed' tells ERC to forgo forcing prompt to the
bottom of the window.  When point is at the prompt, ERC scrolls
the window up when inserting messages, making the prompt appear
stationary.  Users who find this effect too \"stagnant\" can
adjust the option `erc-input-line-position', borrowed here to
express a scroll step offset.  Setting that value to zero lets
the prompt drift toward the bottom by one line per message, which
is generally slow enough not to distract while composing input.
Of course, this doesn't apply when receiving a large influx of
messages, such as after typing \"/msg NickServ help\".

Note that users should consider this option's non-nil behavior to
be experimental.  It currently only works with Emacs 28+."
  :group 'erc-display
  :package-version '(ERC . "5.6")
  :type '(choice boolean (const relaxed)))

;;;###autoload(autoload 'erc-scrolltobottom-mode "erc-goodies" nil t)
(define-erc-module scrolltobottom nil
  "This mode causes the prompt to stay at the end of the window."
  ((add-hook 'erc-mode-hook #'erc--scrolltobottom-setup)
   (when (and erc-scrolltobottom-all (< emacs-major-version 28))
     (erc-button--display-error-notice-with-keys
      "Option `erc-scrolltobottom-all' requires Emacs 28+. Disabling.")
     (setq erc-scrolltobottom-all nil))
   (unless erc--updating-modules-p (erc-buffer-do #'erc--scrolltobottom-setup))
   (if erc-scrolltobottom-all
       (progn
         (add-hook 'erc-insert-pre-hook #'erc--scrolltobottom-on-pre-insert 25)
         (add-hook 'erc-pre-send-functions #'erc--scrolltobottom-on-pre-insert)
         (add-hook 'erc-insert-done-hook #'erc--scrolltobottom-all)
         (add-hook 'erc-send-completed-hook #'erc--scrolltobottom-all))
     (add-hook 'erc-insert-done-hook #'erc-possibly-scroll-to-bottom)))
  ((remove-hook 'erc-mode-hook #'erc--scrolltobottom-setup)
   (erc-buffer-do #'erc--scrolltobottom-setup)
   (remove-hook 'erc-insert-pre-hook #'erc--scrolltobottom-on-pre-insert)
   (remove-hook 'erc-send-completed-hook #'erc--scrolltobottom-all)
   (remove-hook 'erc-insert-done-hook #'erc--scrolltobottom-all)
   (remove-hook 'erc-pre-send-functions #'erc--scrolltobottom-on-pre-insert)
   (remove-hook 'erc-insert-done-hook #'erc-possibly-scroll-to-bottom)))

(defun erc-possibly-scroll-to-bottom ()
  "Like `erc-add-scroll-to-bottom', but only if window is selected."
  (when (eq (selected-window) (get-buffer-window))
    (erc-scroll-to-bottom)))

(defvar-local erc--scrolltobottom-window-info nil
  "Alist with windows as keys and lists of window-related info as values.
Values are lists containing the last window start position and
the last \"window line\" of point.  The \"window line\", which
may be nil, is the number of lines between `window-start' and
`window-point', inclusive.")

;; FIXME treat `end-of-buffer' specially and always recenter -1.
;; FIXME make this work when `erc-scrolltobottom-all' is set to
;; `relaxed'.
(defvar erc--scrolltobottom-post-ignore-commands '(text-scale-adjust)
  "Commands to skip instead of force-scroll on `post-command-hook'.")

(defun erc--scrolltobottom-on-post-command ()
  "Scroll selected window unless `this-command' is exempted."
  (when (eq (selected-window) (get-buffer-window))
    (unless (memq this-command erc--scrolltobottom-post-ignore-commands)
      (setq erc--scrolltobottom-window-info nil)
      (erc--scrolltobottom-confirm))))

;; It may be desirable to also restore the relative line position of
;; window point after changing dimensions.  Perhaps stashing the
;; previous ratio of window line to body height and later recentering
;; proportionally would achieve this.
(defun erc--scrolltobottom-on-win-conf-change ()
  "Scroll window to bottom when at prompt and using the minibuffer."
  (setq erc--scrolltobottom-window-info nil)
  (erc--scrolltobottom-confirm))

(defun erc--scrolltobottom-all (&rest _)
  "Maybe put prompt on last line in all windows displaying current buffer.
Expect to run when narrowing is in effect, such as on insertion
or send-related hooks.  When recentering has not been performed,
attempt to restore last `window-start', if known."
  (dolist (window (get-buffer-window-list nil nil 'visible))
    (with-selected-window window
      (when-let
          ((erc--scrolltobottom-window-info)
           (found (assq window erc--scrolltobottom-window-info))
           ((not (erc--scrolltobottom-confirm (nth 2 found)))))
        (set-window-start window (cadr found) 'no-force))))
  ;; Necessary unless we're sure `erc--scrolltobottom-on-pre-insert'
  ;; always runs between calls to this function.
  (setq erc--scrolltobottom-window-info nil))

(defun erc-add-scroll-to-bottom ()
  "A hook function for `erc-mode-hook' to recenter output at bottom of window.

If you find that ERC hangs when using this function, try customizing
the value of `erc-input-line-position'.

Note that the prior suggestion comes from a time when this
function used `window-scroll-functions', which was replaced by
`post-command-hook' in ERC 5.3."
  (declare (obsolete erc--scrolltobottom-setup "30.1"))
  (add-hook 'post-command-hook #'erc-scroll-to-bottom nil t))

(defun erc--scrolltobottom-setup ()
  "Perform buffer-local setup for module `scrolltobottom'."
  (if erc-scrolltobottom-mode
      (if erc-scrolltobottom-all
          (progn
            (setq-local read-minibuffer-restore-windows nil)
            (when (zerop scroll-conservatively)
              (setq-local scroll-step 1))
            (unless (eq erc-scrolltobottom-all 'relaxed)
              (add-hook 'window-configuration-change-hook
                        #'erc--scrolltobottom-on-win-conf-change 50 t)
              (add-hook 'post-command-hook
                        #'erc--scrolltobottom-on-post-command 50 t)))
        (add-hook 'post-command-hook #'erc-scroll-to-bottom nil t))
    (remove-hook 'post-command-hook #'erc-scroll-to-bottom t)
    (remove-hook 'post-command-hook #'erc--scrolltobottom-on-post-command t)
    (remove-hook 'window-configuration-change-hook
                 #'erc--scrolltobottom-on-win-conf-change t)
    (kill-local-variable 'read-minibuffer-restore-windows)
    (kill-local-variable 'scroll-step)
    (kill-local-variable 'erc--scrolltobottom-window-info)))

(defun erc--scrolltobottom-on-pre-insert (_)
  "Remember `window-start' before inserting a message."
  (setq erc--scrolltobottom-window-info
        (mapcar (lambda (w)
                  (list w
                        (window-start w)
                        (and-let*
                            (((eq erc-scrolltobottom-all 'relaxed))
                             (c (count-screen-lines (window-start w)
                                                    (point-max) nil w)))
                          (if (= ?\n (char-before (point-max))) (1+ c) c))))
                (get-buffer-window-list nil nil 'visible))))

(defun erc--scrolltobottom-confirm (&optional scroll-to)
  "Like `erc-scroll-to-bottom', but use `window-point'.
Position current line (with `recenter') SCROLL-TO lines below
window's top.  Return nil if point is not in prompt area or if
prompt isn't ready."
  (when erc-insert-marker
    (let ((resize-mini-windows nil))
      (save-restriction
        (widen)
        (when (>= (window-point) erc-input-marker)
          (save-excursion
            (goto-char (point-max))
            (recenter (+ (or scroll-to 0) (or erc-input-line-position -1)))
            t))))))

(defun erc-scroll-to-bottom ()
  "Recenter WINDOW so that `point' is on the last line.

You can control which line is recentered to by customizing the
variable `erc-input-line-position'."
      ;; Temporarily bind resize-mini-windows to nil so that users who have it
      ;; set to a non-nil value will not suffer from premature minibuffer
      ;; shrinkage due to the below recenter call.  I have no idea why this
      ;; works, but it solves the problem, and has no negative side effects.
      ;; (Fran Litterio, 2003/01/07)
  (let ((resize-mini-windows nil))
    (save-restriction
      (widen)
      (when (and erc-insert-marker
                 (eq (current-buffer) (window-buffer))
		 ;; we're editing a line. Scroll.
		 (> (point) erc-insert-marker))
	(save-excursion
	  (goto-char (point-max))
	  (recenter (or erc-input-line-position -1)))))))

;;; Make read only
;;;###autoload(autoload 'erc-readonly-mode "erc-goodies" nil t)
(define-erc-module readonly nil
  "This mode causes all inserted text to be read-only."
  ((add-hook 'erc-insert-post-hook #'erc-make-read-only 70)
   (add-hook 'erc-send-post-hook #'erc-make-read-only 70))
  ((remove-hook 'erc-insert-post-hook #'erc-make-read-only)
   (remove-hook 'erc-send-post-hook #'erc-make-read-only)))

(defun erc-make-read-only ()
  "Make all the text in the current buffer read-only.
Put this function on `erc-insert-post-hook' and/or `erc-send-post-hook'."
  (put-text-property (point-min) (point-max) 'read-only t)
  (put-text-property (point-min) (point-max) 'front-sticky t)
  (put-text-property (point-min) (point-max) 'rear-nonsticky t))

;;; Move to prompt when typing text
;;;###autoload(autoload 'erc-move-to-prompt-mode "erc-goodies" nil t)
(define-erc-module move-to-prompt nil
  "This mode causes the point to be moved to the prompt when typing text."
  ((add-hook 'erc-mode-hook #'erc-move-to-prompt-setup)
   (unless erc--updating-modules-p (erc-buffer-do #'erc-move-to-prompt-setup)))
  ((remove-hook 'erc-mode-hook #'erc-move-to-prompt-setup)
   (dolist (buffer (erc-buffer-list))
     (with-current-buffer buffer
       (remove-hook 'pre-command-hook #'erc-move-to-prompt t)))))

(defun erc-move-to-prompt ()
  "Move the point to the ERC prompt if this is a self-inserting command."
  (when (and erc-input-marker (< (point) erc-input-marker)
             (eq 'self-insert-command this-command))
    (deactivate-mark)
    (push-mark)
    (goto-char (point-max))))

(defun erc-move-to-prompt-setup ()
  "Initialize the move-to-prompt module."
  (add-hook 'pre-command-hook #'erc-move-to-prompt 70 t))

;;; Keep place in unvisited channels
;;;###autoload(autoload 'erc-keep-place-mode "erc-goodies" nil t)
(define-erc-module keep-place nil
  "Leave point above un-viewed text in other channels."
  ((add-hook 'erc-insert-pre-hook  #'erc-keep-place 65))
  ((remove-hook 'erc-insert-pre-hook  #'erc-keep-place)))

(defcustom erc-keep-place-indicator-style t
  "Flavor of visual indicator applied to kept place.
For use with the `keep-place-indicator' module.  A value of `arrow'
displays an arrow in the left fringe or margin.  When it's
`face', ERC adds the face `erc-keep-place-indicator-line' to the
appropriate line.  A value of t does both."
  :group 'erc
  :package-version '(ERC . "5.6")
  :type '(choice (const :tag "Use arrow" arrow)
                 (const :tag "Use face" face)
                 (const :tag "Use both arrow and face" t)))

(defcustom erc-keep-place-indicator-buffer-type t
  "ERC buffer type in which to display `keep-place-indicator'.
A value of t means \"all\" ERC buffers."
  :group 'erc
  :package-version '(ERC . "5.6")
  :type '(choice (const t) (const server) (const target)))

(defcustom erc-keep-place-indicator-follow nil
  "Whether to sync visual kept place to window's top when reading.
For use with `erc-keep-place-indicator-mode'.  When enabled, the
indicator updates when the last window displaying the same buffer
switches away, but only if the indicator resides earlier in the
buffer than the window's start."
  :group 'erc
  :package-version '(ERC . "5.6")
  :type 'boolean)

(defface erc-keep-place-indicator-line
  '((((class color) (min-colors 88) (background light)
      (supports :underline (:style wave)))
     (:underline (:color "PaleGreen3" :style wave)))
    (((class color) (min-colors 88) (background dark)
      (supports :underline (:style wave)))
     (:underline (:color "PaleGreen1" :style wave)))
    (t :underline t))
  "Face for option `erc-keep-place-indicator-style'."
  :group 'erc-faces)

(defface erc-keep-place-indicator-arrow
  '((((class color) (min-colors 88) (background light))
     (:foreground "PaleGreen3"))
    (((class color) (min-colors 88) (background dark))
     (:foreground "PaleGreen1"))
    (t :inherit fringe))
  "Face for arrow value of option `erc-keep-place-indicator-style'."
  :group 'erc-faces)

(defvar-local erc--keep-place-indicator-overlay nil
  "Overlay for `erc-keep-place-indicator-mode'.")

(defun erc--keep-place-indicator-on-window-buffer-change (_)
  "Maybe sync `erc--keep-place-indicator-overlay'.
Do so only when switching to a new buffer in the same window if
the replaced buffer is no longer visible in another window and
its `window-start' at the time of switching is strictly greater
than the indicator's position."
  (when-let ((erc-keep-place-indicator-follow)
             (window (selected-window))
             ((not (eq window (active-minibuffer-window))))
             (old-buffer (window-old-buffer window))
             ((buffer-live-p old-buffer))
             ((not (eq old-buffer (current-buffer))))
             (ov (buffer-local-value 'erc--keep-place-indicator-overlay
                                     old-buffer))
             ((not (get-buffer-window old-buffer 'visible)))
             (prev (assq old-buffer (window-prev-buffers window)))
             (old-start (nth 1 prev))
             (old-inmkr (buffer-local-value 'erc-insert-marker old-buffer))
             ((< (overlay-end ov) old-start old-inmkr)))
    (with-current-buffer old-buffer
      (erc-keep-place-move old-start))))

;;;###autoload(put 'keep-place-indicator 'erc--feature 'erc-goodies)
;;;###autoload(autoload 'erc-keep-place-indicator-mode "erc-goodies" nil t)
(define-erc-module keep-place-indicator nil
  "Buffer-local `keep-place' with fringe arrow and/or highlighted face.
Play nice with global module `keep-place' but don't depend on it.
Expect that users may want different combinations of `keep-place'
and `keep-place-indicator' in different buffers."
  ((cond (erc-keep-place-mode)
         ((memq 'keep-place erc-modules)
          (erc-keep-place-mode +1))
         ;; Enable a local version of `keep-place-mode'.
         (t (add-hook 'erc-insert-pre-hook  #'erc-keep-place 65 t)))
   (require 'fringe)
   (add-hook 'window-buffer-change-functions
             #'erc--keep-place-indicator-on-window-buffer-change 40)
   (add-hook 'erc-keep-place-mode-hook
             #'erc--keep-place-indicator-on-global-module 40)
   (if (pcase erc-keep-place-indicator-buffer-type
         ('target erc--target)
         ('server (not erc--target))
         ('t t))
       (progn
         (erc--restore-initialize-priors erc-keep-place-indicator-mode
           erc--keep-place-indicator-overlay (make-overlay 0 0))
         (when-let (((memq erc-keep-place-indicator-style '(t arrow)))
                    (ov-property (if (zerop (fringe-columns 'left))
                                     'after-string
                                   'before-string))
                    (display (if (zerop (fringe-columns 'left))
                                 `((margin left-margin) ,overlay-arrow-string)
                               '(left-fringe right-triangle
                                             erc-keep-place-indicator-arrow)))
                    (bef (propertize " " 'display display)))
           (overlay-put erc--keep-place-indicator-overlay ov-property bef))
         (when (memq erc-keep-place-indicator-style '(t face))
           (overlay-put erc--keep-place-indicator-overlay 'face
                        'erc-keep-place-indicator-line)))
     (erc-keep-place-indicator-mode -1)))
  ((when erc--keep-place-indicator-overlay
     (delete-overlay erc--keep-place-indicator-overlay))
   (let ((buffer (current-buffer)))
     ;; Remove global hooks unless others exist with mode enabled.
     (unless (erc-buffer-filter (lambda ()
                                  (and (not (eq buffer (current-buffer)))
                                       erc-keep-place-indicator-mode)))
       (remove-hook 'erc-keep-place-mode-hook
                    #'erc--keep-place-indicator-on-global-module)
       (remove-hook 'window-buffer-change-functions
                    #'erc--keep-place-indicator-on-window-buffer-change)))
   (when (local-variable-p 'erc-insert-pre-hook)
     (remove-hook 'erc-insert-pre-hook  #'erc-keep-place t))
   (remove-hook 'erc-keep-place-mode-hook
                #'erc--keep-place-indicator-on-global-module t)
   (kill-local-variable 'erc--keep-place-indicator-overlay))
  'local)

(defun erc--keep-place-indicator-on-global-module ()
  "Ensure `keep-place-indicator' survives toggling `erc-keep-place-mode'.
Do this by simulating `keep-place' in all buffers where
`keep-place-indicator' is enabled."
  (erc-with-all-buffers-of-server nil (lambda () erc-keep-place-indicator-mode)
    (if erc-keep-place-mode
        (remove-hook 'erc-insert-pre-hook  #'erc-keep-place t)
      (add-hook 'erc-insert-pre-hook  #'erc-keep-place 65 t))))

(defun erc-keep-place-move (pos)
  "Move keep-place indicator to current line or POS.
For use with `keep-place-indicator' module.  When called
interactively, interpret POS as an offset.  Specifically, when
POS is a raw prefix arg, like (4), move the indicator to the
window's last line.  When it's the minus sign, put it on the
window's first line.  Interpret an integer as an offset in lines."
  (interactive
   (progn
     (unless erc-keep-place-indicator-mode
       (user-error "`erc-keep-place-indicator-mode' not enabled"))
     (list (pcase current-prefix-arg
             ((and (pred integerp) v)
              (save-excursion
                (let ((inhibit-field-text-motion t))
                  (forward-line v)
                  (point))))
             (`(,_) (1- (min erc-insert-marker (window-end))))
             ('- (min (1- erc-insert-marker) (window-start)))))))
  (save-excursion
    (let ((inhibit-field-text-motion t))
      (when pos
        (goto-char pos))
      (move-overlay erc--keep-place-indicator-overlay
                    (line-beginning-position)
                    (line-end-position)))))

(defun erc-keep-place-goto ()
  "Jump to keep-place indicator.
For use with `keep-place-indicator' module."
  (interactive
   (prog1 nil
     (unless erc-keep-place-indicator-mode
       (user-error "`erc-keep-place-indicator-mode' not enabled"))
     (deactivate-mark)
     (push-mark)))
  (goto-char (overlay-start erc--keep-place-indicator-overlay))
  (recenter (truncate (* (window-height) 0.25)) t)
  (require 'pulse)
  (when (pulse-available-p)
    (pulse-momentary-highlight-overlay erc--keep-place-indicator-overlay)))

(defun erc-keep-place (_ignored)
  "Move point away from the last line in a non-selected ERC buffer."
  (when (and (not (eq (window-buffer (selected-window))
                      (current-buffer)))
             (>= (point) erc-insert-marker))
    (deactivate-mark)
    (goto-char (erc-beg-of-input-line))
    (forward-line -1)
    (when erc-keep-place-indicator-mode
      (unless (or (minibuffer-window-active-p (selected-window))
                  (get-buffer-window nil 'visible))
        (erc-keep-place-move nil)))
    ;; if `switch-to-buffer-preserve-window-point' is set,
    ;; we cannot rely on point being saved, and must commit
    ;; it to window-prev-buffers.
    (when switch-to-buffer-preserve-window-point
      (dolist (frame (frame-list))
        (walk-window-tree
         (lambda (window)
           (let ((prev (assq (current-buffer)
                             (window-prev-buffers window))))
             (when prev
	       (setf (nth 2 prev) (point-marker)))))
         frame nil 'nominibuf)))))

;;; Distinguish non-commands
(defvar erc-noncommands-list '(erc-cmd-ME
                               erc-cmd-COUNTRY
                               erc-cmd-SV
                               erc-cmd-SM
                               erc-cmd-SAY
                               erc-cmd-LASTLOG)
  "List of client \"slash commands\" that perform their own buffer I/O.
The `command-indicator' module forgoes echoing these commands,
most of which aren't actual interactive lisp commands.")

;;;###autoload(autoload 'erc-noncommands-mode "erc-goodies" nil t)
(define-erc-module noncommands nil
  "Treat commands that display themselves specially.
This module has been a no-op since ERC 5.3 and has likely only
ever made sense in the context of `erc-command-indicator'.  It
was deprecated in ERC 5.6."
  ((add-hook 'erc--input-review-functions #'erc-send-distinguish-noncommands))
  ((remove-hook 'erc--input-review-functions
                #'erc-send-distinguish-noncommands)))
(make-obsolete-variable 'erc-noncommand-mode
                        'erc-command-indicator-mode "30.1")
(make-obsolete 'erc-noncommand-mode 'erc-command-indicator-mode "30.1")
(make-obsolete 'erc-noncommand-enable 'erc-command-indicator-enable "30.1")
(make-obsolete 'erc-noncommand-disable 'erc-command-indicator-disable "30.1")

(defun erc-send-distinguish-noncommands (state)
  "If STR is an ERC non-command, set `insertp' in STATE to nil."
  (let* ((string (erc-input-string state))
         (command (erc-extract-command-from-line string))
         (cmd-fun (and command
                       (car command))))
    (when (and cmd-fun
               (not (string-match "\n.+$" string))
               (memq cmd-fun erc-noncommands-list))
      ;; Inhibit sending this string.
      (setf (erc-input-insertp state) nil))))


;;; Command-indicator

(defface erc-command-indicator-face
  '((t :inherit (erc-input-face fixed-pitch-serif)))
  "Face for echoed command lines, including the prompt.
See option `erc-command-indicator'."
  :package-version '(ERC . "5.6") ; standard value, from bold
  :group 'erc-faces)

(defcustom erc-command-indicator 'erc-prompt
  "Pseudo prompt for echoed command lines.
An analog of the option `erc-prompt' that replaces the \"speaker
label\" for echoed \"slash\" commands submitted at the prompt.  A
value of nil means ERC only inserts the command-line portion
alone, without the prompt, which may trick certain modules, like
`fill', into treating the leading slash command itself as the
message's speaker."
  :package-version '(ERC . "5.6")
  :group 'erc-display
  :type '(choice (const :tag "Defer to `erc-prompt'" erc-prompt)
                 (const :tag "Print command lines without a prompt" nil)
                 (string :tag "User-provided string")
                 (function :tag "User-provided function")))

;;;###autoload(autoload 'erc-command-indicator-mode "erc-goodies" nil t)
(define-erc-module command-indicator nil
  "Echo command lines for \"slash commands,\" like /JOIN, /HELP, etc.
Skip those appearing in `erc-noncommands-list'.

Users can run \\[erc-command-indicator-toggle-hidden] to hide and
reveal echoed command lines after they've been inserted."
  ((add-hook 'erc--input-review-functions
             #'erc--command-indicator-permit-insertion 80 t)
   (erc-command-indicator-toggle-hidden -1))
  ((remove-hook 'erc--input-review-functions
                #'erc--command-indicator-permit-insertion t)
   (erc-command-indicator-toggle-hidden +1))
  'local)

(defun erc-command-indicator ()
  "Return the command-indicator prompt as a string.
Do nothing if the variable `erc-command-indicator' is nil."
  (and erc-command-indicator
       (let ((prompt (if (functionp erc-command-indicator)
                         (funcall erc-command-indicator)
                       erc-command-indicator)))
         (concat prompt (and (not (string-empty-p prompt))
                             (not (string-suffix-p " " prompt))
                             " ")))))

(defun erc-command-indicator-toggle-hidden (arg)
  "Toggle whether echoed \"slash commands\" are visible."
  (interactive "P")
  (erc--toggle-hidden 'command-indicator arg))

(defun erc--command-indicator-permit-insertion (state)
  "Insert `erc-input' STATE's message if it's an echoed command."
  (cl-assert erc-command-indicator-mode)
  (when (erc--input-split-cmdp state)
    (setf (erc--input-split-insertp state) t
          (erc--input-split-substxt state) #'erc--command-indicator-display)
    (erc-send-distinguish-noncommands state)))

;; This function used to be called `erc-display-command'.  It was
;; neutered in ERC 5.3.x (Emacs 24.5), commented out in 5.4, removed
;; in 5.5, and restored in 5.6.
(defun erc--command-indicator-display (line &rest rest)
  "Insert command LINE as echoed input resembling that of REPLs and shells."
  (when erc-insert-this
    (when rest
      (setq line (string-join (cons line rest) "\n")))
    (save-excursion
      (erc--assert-input-bounds)
      (let ((insert-position (marker-position (goto-char erc-insert-marker)))
            (erc--msg-props (or erc--msg-props
                                (let ((ovs erc--msg-prop-overrides))
                                  (map-into `((erc--msg . slash-cmd)
                                              ,@(reverse ovs))
                                            'hash-table)))))
        (when-let ((string (erc-command-indicator))
                   (erc-input-marker (copy-marker erc-input-marker)))
          (erc-display-prompt nil nil string 'erc-command-indicator-face)
          (remove-text-properties insert-position (point)
                                  '(field nil erc-prompt nil))
          (set-marker erc-input-marker nil))
        (let ((beg (point)))
          (insert line)
          (erc-put-text-property beg (point)
                                 'font-lock-face 'erc-command-indicator-face)
          (insert "\n"))
        (save-restriction
          (narrow-to-region insert-position (point))
          (run-hooks 'erc-send-modify-hook)
          (run-hooks 'erc-send-post-hook)
          (cl-assert (> (- (point-max) (point-min)) 1))
          (erc--hide-message 'command-indicator)
          (add-text-properties (point-min) (1+ (point-min))
                               (erc--order-text-properties-from-hash
                                erc--msg-props))))
      (erc--refresh-prompt))))

;;;###autoload
(defun erc-load-irc-script-lines (lines &optional force noexpand)
  "Process a list of LINES as prompt input submissions.
If optional NOEXPAND is non-nil, do not expand script-specific
substitution sequences via `erc-process-script-line' and instead
process LINES as literal prompt input.  With FORCE, bypass flood
protection."
  ;; The various erc-cmd-CMDs were designed to return non-nil when
  ;; their command line should be echoed.  But at some point, these
  ;; handlers began displaying their own output, which naturally
  ;; appeared *above* the echoed command.  This tries to intercept
  ;; these insertions, deferring them until the command has returned
  ;; and its command line has been printed.
  (cl-assert (eq 'erc-mode major-mode))
  (let ((args (and erc-script-args
                   (if (string-match "^ " erc-script-args)
                       (substring erc-script-args 1)
                     erc-script-args))))
    (with-silent-modifications
      (dolist (line lines)
        (erc-log (concat "erc-load-script: CMD: " line))
        (unless (string-match (rx bot (* (syntax whitespace)) eot) line)
          (unless noexpand
            (setq line (erc-process-script-line line args)))
          (let ((erc--current-line-input-split (erc--make-input-split line))
                calls insertp)
            (add-function :around (local 'erc--send-message-nested-function)
                          (lambda (&rest args) (push args calls))
                          '((name . erc-script-lines-fn) (depth . -80)))
            (add-function :around (local 'erc--send-action-function)
                          (lambda (&rest args) (push args calls))
                          '((name . erc-script-lines-fn) (depth . -80)))
            (setq insertp
                  (unwind-protect (erc-process-input-line line force)
                    (remove-function (local 'erc--send-action-function)
                                     'erc-script-lines-fn)
                    (remove-function (local 'erc--send-message-nested-function)
                                     'erc-script-lines-fn)))
            (when (and insertp erc-script-echo)
              (erc--command-indicator-display line)
              (dolist (call calls)
                (apply (car call) (cdr call))))))))))

;;; IRC control character processing.
(defgroup erc-control-characters nil
  "Dealing with control characters."
  :group 'erc)

(defcustom erc-interpret-controls-p t
  "If non-nil, display IRC colors and other highlighting effects.

If this is set to the symbol `remove', ERC removes all IRC colors and
highlighting effects.  When this variable is non-nil, it can cause Emacs to run
slowly on systems lacking sufficient CPU speed.  In chatty channels, or in an
emergency (message flood) it can be turned off to save processing time.  See
`erc-toggle-interpret-controls'."
  :type '(choice (const :tag "Highlight control characters" t)
                 (const :tag "Remove control characters" remove)
                 (const :tag "Display raw control characters" nil)))

(defcustom erc-interpret-mirc-color nil
  "If non-nil, ERC will interpret mIRC color codes."
  :type 'boolean)

(defcustom erc-beep-p nil
  "Beep if C-g is in the server message.
The value `erc-interpret-controls-p' must also be t for this to work."
  :type 'boolean)

(defface erc-bold-face '((t :weight bold))
  "ERC bold face."
  :group 'erc-faces)

(defface erc-italic-face '((t :slant italic))
  "ERC italic face."
  :group 'erc-faces)

(defface erc-inverse-face
  '((t :inverse-video t))
  "ERC inverse face."
  :group 'erc-faces)

(defface erc-spoiler-face '((t :inherit default))
  "ERC spoiler face."
  :group 'erc-faces)

(defface erc-underline-face '((t :underline t))
  "ERC underline face."
  :group 'erc-faces)

;; FIXME rename these to something like `erc-control-color-N-fg',
;; and deprecate the old names via `define-obsolete-face-alias'.
(defface fg:erc-color-face0 '((t :foreground "White"))
  "ERC face."
  :group 'erc-faces)
(defface fg:erc-color-face1 '((t :foreground "black"))
  "ERC face."
  :group 'erc-faces)
(defface fg:erc-color-face2 '((t :foreground "blue4"))
  "ERC face."
  :group 'erc-faces)
(defface fg:erc-color-face3 '((t :foreground "green4"))
  "ERC face."
  :group 'erc-faces)
(defface fg:erc-color-face4 '((t :foreground "red"))
  "ERC face."
  :group 'erc-faces)
(defface fg:erc-color-face5 '((t :foreground "brown"))
  "ERC face."
  :group 'erc-faces)
(defface fg:erc-color-face6 '((t :foreground "purple"))
  "ERC face."
  :group 'erc-faces)
(defface fg:erc-color-face7 '((t :foreground "orange"))
  "ERC face."
  :group 'erc-faces)
(defface fg:erc-color-face8 '((t :foreground "yellow"))
  "ERC face."
  :group 'erc-faces)
(defface fg:erc-color-face9 '((t :foreground "green"))
  "ERC face."
  :group 'erc-faces)
(defface fg:erc-color-face10 '((t :foreground "lightblue1"))
  "ERC face."
  :group 'erc-faces)
(defface fg:erc-color-face11 '((t :foreground "cyan"))
  "ERC face."
  :group 'erc-faces)
(defface fg:erc-color-face12 '((t :foreground "blue"))
  "ERC face."
  :group 'erc-faces)
(defface fg:erc-color-face13 '((t :foreground "deeppink"))
  "ERC face."
  :group 'erc-faces)
(defface fg:erc-color-face14 '((t :foreground "gray50"))
  "ERC face."
  :group 'erc-faces)
(defface fg:erc-color-face15 '((t :foreground "gray90"))
  "ERC face."
  :group 'erc-faces)

(defface bg:erc-color-face0 '((t :background "White"))
  "ERC face."
  :group 'erc-faces)
(defface bg:erc-color-face1 '((t :background "black"))
  "ERC face."
  :group 'erc-faces)
(defface bg:erc-color-face2 '((t :background "blue4"))
  "ERC face."
  :group 'erc-faces)
(defface bg:erc-color-face3 '((t :background "green4"))
  "ERC face."
  :group 'erc-faces)
(defface bg:erc-color-face4 '((t :background "red"))
  "ERC face."
  :group 'erc-faces)
(defface bg:erc-color-face5 '((t :background "brown"))
  "ERC face."
  :group 'erc-faces)
(defface bg:erc-color-face6 '((t :background "purple"))
  "ERC face."
  :group 'erc-faces)
(defface bg:erc-color-face7 '((t :background "orange"))
  "ERC face."
  :group 'erc-faces)
(defface bg:erc-color-face8 '((t :background "yellow"))
  "ERC face."
  :group 'erc-faces)
(defface bg:erc-color-face9 '((t :background "green"))
  "ERC face."
  :group 'erc-faces)
(defface bg:erc-color-face10 '((t :background "lightblue1"))
  "ERC face."
  :group 'erc-faces)
(defface bg:erc-color-face11 '((t :background "cyan"))
  "ERC face."
  :group 'erc-faces)
(defface bg:erc-color-face12 '((t :background "blue"))
  "ERC face."
  :group 'erc-faces)
(defface bg:erc-color-face13 '((t :background "deeppink"))
  "ERC face."
  :group 'erc-faces)
(defface bg:erc-color-face14 '((t :background "gray50"))
  "ERC face."
  :group 'erc-faces)
(defface bg:erc-color-face15 '((t :background "gray90"))
  "ERC face."
  :group 'erc-faces)

;; https://lists.gnu.org/archive/html/emacs-erc/2021-07/msg00005.html
(defvar erc--controls-additional-colors
  ["#470000" "#472100" "#474700" "#324700" "#004700" "#00472c"
   "#004747" "#002747" "#000047" "#2e0047" "#470047" "#47002a"
   "#740000" "#743a00" "#747400" "#517400" "#007400" "#007449"
   "#007474" "#004074" "#000074" "#4b0074" "#740074" "#740045"
   "#b50000" "#b56300" "#b5b500" "#7db500" "#00b500" "#00b571"
   "#00b5b5" "#0063b5" "#0000b5" "#7500b5" "#b500b5" "#b5006b"
   "#ff0000" "#ff8c00" "#ffff00" "#b2ff00" "#00ff00" "#00ffa0"
   "#00ffff" "#008cff" "#0000ff" "#a500ff" "#ff00ff" "#ff0098"
   "#ff5959" "#ffb459" "#ffff71" "#cfff60" "#6fff6f" "#65ffc9"
   "#6dffff" "#59b4ff" "#5959ff" "#c459ff" "#ff66ff" "#ff59bc"
   "#ff9c9c" "#ffd39c" "#ffff9c" "#e2ff9c" "#9cff9c" "#9cffdb"
   "#9cffff" "#9cd3ff" "#9c9cff" "#dc9cff" "#ff9cff" "#ff94d3"
   "#000000" "#131313" "#282828" "#363636" "#4d4d4d" "#656565"
   "#818181" "#9f9f9f" "#bcbcbc" "#e2e2e2" "#ffffff"])

(defun erc-get-bg-color-face (n)
  "Fetches the right face for background color N (0-15)."
  (if (stringp n) (setq n (string-to-number n)))
  (if (not (numberp n))
      (prog1 'default
        (erc-error "erc-get-bg-color-face: n is NaN: %S" n))
    (when (> n 99)
      (erc-log (format "   Wrong color: %s" n))
      (setq n (mod n 16)))
    (cond
     ((and (>= n 0) (< n 16))
      (intern (concat "bg:erc-color-face" (number-to-string n))))
     ((< 15 n 99)
      (list :background (aref erc--controls-additional-colors (- n 16))))
     (t (erc-log (format "   Wrong color: %s" n)) nil))))

(defun erc-get-fg-color-face (n)
  "Fetches the right face for foreground color N (0-15)."
  (if (stringp n) (setq n (string-to-number n)))
  (if (not (numberp n))
      (prog1 'default
        (erc-error "erc-get-fg-color-face: n is NaN: %S" n))
    (when (> n 99)
      (erc-log (format "   Wrong color: %s" n))
      (setq n (mod n 16)))
    (cond
     ((and (>= n 0) (< n 16))
      (intern (concat "fg:erc-color-face" (number-to-string n))))
     ((< 15 n 99)
      (list :foreground (aref erc--controls-additional-colors (- n 16))))
     (t (erc-log (format "   Wrong color: %s" n)) nil))))

;;;###autoload(autoload 'erc-irccontrols-mode "erc-goodies" nil t)
(define-erc-module irccontrols nil
  "This mode enables the interpretation of IRC control chars."
  ((add-hook 'erc-insert-modify-hook #'erc-controls-highlight -50)
   (add-hook 'erc-send-modify-hook #'erc-controls-highlight)
   (erc--modify-local-map t "C-c C-c" #'erc-toggle-interpret-controls))
  ((remove-hook 'erc-insert-modify-hook #'erc-controls-highlight)
   (remove-hook 'erc-send-modify-hook #'erc-controls-highlight)
   (erc--modify-local-map nil "C-c C-c" #'erc-toggle-interpret-controls)))

;; These patterns were moved here to circumvent compiler warnings but
;; otherwise translated verbatim from their original string-literal
;; definitions (minus a small bug fix to satisfy newly added tests).
(defvar erc-controls-remove-regexp
  (rx (or ?\C-b ?\C-\] ?\C-_ ?\C-v ?\C-g ?\C-o
          (: ?\C-c (? (any "0-9")) (? (any "0-9"))
             (? (group ?, (any "0-9") (? (any "0-9")))))))
  "Regular expression matching control characters to remove.")

;; Before the change to `rx', group 3 used to be a sibling of group 2.
;; This was assumed to be a bug.  A few minor simplifications were
;; also performed.  If incorrect, please admonish.
(defvar erc-controls-highlight-regexp
  (rx (group (or ?\C-b ?\C-\] ?\C-v ?\C-_ ?\C-g ?\C-o
                 (: ?\C-c (? (group (** 1 2 (any "0-9")))
                             (? (group ?, (group (** 1 2 (any "0-9")))))))))
      (group (* (not (any ?\C-b ?\C-c ?\C-g ?\n ?\C-o ?\C-v ?\C-\] ?\C-_)))))
  "Regular expression matching control chars to highlight.")

(defun erc-controls-interpret (str)
   "Return a copy of STR after dealing with IRC control characters.
See `erc-interpret-controls-p' and `erc-interpret-mirc-color' for options."
   (when str
     (let ((s str))
       (cond ((eq erc-interpret-controls-p 'remove)
              (erc-controls-strip s))
             (erc-interpret-controls-p
              (let ((boldp nil)
                    (italicp nil)
                    (inversep nil)
                    (underlinep nil)
                    (fg nil)
                    (bg nil))
                (while (string-match erc-controls-highlight-regexp s)
                  (let ((control (match-string 1 s))
                        (fg-color (match-string 2 s))
                        (bg-color (match-string 4 s))
                        (start (match-beginning 0))
                        (end (+ (match-beginning 0)
                                (length (match-string 5 s)))))
                    (setq s (replace-match "" nil nil s 1))
                    (cond ((and erc-interpret-mirc-color (or fg-color bg-color))
                           (setq fg fg-color)
                           (when bg-color (setq bg bg-color)))
                          ((string= control "\C-b")
                           (setq boldp (not boldp)))
                          ((string= control "\C-]")
                           (setq italicp (not italicp)))
                          ((string= control "\C-v")
                           (setq inversep (not inversep)))
                          ((string= control "\C-_")
                           (setq underlinep (not underlinep)))
                          ((string= control "\C-c")
                           (setq fg nil
                                 bg nil))
                          ((string= control "\C-g")
                           (when erc-beep-p
                             (ding)))
                          ((string= control "\C-o")
                           (setq boldp nil
                                 italicp nil
                                 inversep nil
                                 underlinep nil
                                 fg nil
                                 bg nil))
                          (t nil))
                    (erc-controls-propertize
                     start end boldp italicp inversep underlinep fg bg s)))
                s))
             (t s)))))

;;;###autoload
(defun erc-controls-strip (str)
  "Return a copy of STR with all IRC control characters removed."
  (when str
    (let ((s str))
      (while (string-match erc-controls-remove-regexp s)
        (setq s (replace-match "" nil nil s)))
      s)))

(defun erc-controls-highlight ()
  "Highlight IRC control chars in the buffer.
This is useful for `erc-insert-modify-hook' and `erc-send-modify-hook'.
Also see `erc-interpret-controls-p' and `erc-interpret-mirc-color'."
  (goto-char (point-min))
  (cond ((eq erc-interpret-controls-p 'remove)
         (while (re-search-forward erc-controls-remove-regexp nil t)
           (replace-match "")))
        (erc-interpret-controls-p
         (let ((boldp nil)
               (italicp nil)
               (inversep nil)
               (underlinep nil)
               (fg nil)
               (bg nil))
           (while (re-search-forward erc-controls-highlight-regexp nil t)
             (let ((control (match-string 1))
                   (fg-color (match-string 2))
                   (bg-color (match-string 4))
                   (start (match-beginning 0))
                   (end (+ (match-beginning 0) (length (match-string 5)))))
               (replace-match "" nil nil nil 1)
               (cond ((and erc-interpret-mirc-color (or fg-color bg-color))
                      (setq fg fg-color)
                      (when bg-color (setq bg bg-color)))
                     ((string= control "\C-b")
                      (setq boldp (not boldp)))
                     ((string= control "\C-]")
                      (setq italicp (not italicp)))
                     ((string= control "\C-v")
                      (setq inversep (not inversep)))
                     ((string= control "\C-_")
                      (setq underlinep (not underlinep)))
                     ((string= control "\C-c")
                      (setq fg nil
                            bg nil))
                     ((string= control "\C-g")
                      (when erc-beep-p
                        (ding)))
                     ((string= control "\C-o")
                      (setq boldp nil
                            italicp nil
                            inversep nil
                            underlinep nil
                            fg nil
                            bg nil))
                     (t nil))
               (erc-controls-propertize start end
                                        boldp italicp inversep underlinep fg bg)))))
        (t nil)))

(defun erc-controls-propertize (from to boldp italicp inversep underlinep fg bg
                                     &optional str)
  "Prepend properties from IRC control characters between FROM and TO.
If optional argument STR is provided, apply to STR, otherwise prepend properties
to a region in the current buffer."
  (when (and fg bg (equal fg bg) (not (equal fg "99")))
    (add-text-properties from to '( mouse-face erc-spoiler-face
                                    cursor-face erc-spoiler-face)
                         str)
    (erc--reserve-important-text-props from to
                                       '( mouse-face erc-spoiler-face
                                          cursor-face erc-spoiler-face)
                                       str))
  (when fg (setq fg (erc-get-fg-color-face fg)))
  (when bg (setq bg (erc-get-bg-color-face bg)))
  (font-lock-prepend-text-property
   from
   to
   'font-lock-face
   (append (if boldp
               '(erc-bold-face)
             nil)
           (if italicp
               '(erc-italic-face)
             nil)
           (if inversep
               '(erc-inverse-face)
             nil)
           (if underlinep
               '(erc-underline-face)
             nil)
           (if fg
               (list fg)
             nil)
           (if bg
               (list bg)
             nil))
   str)
  str)

(defun erc-toggle-interpret-controls (&optional arg)
  "Toggle interpretation of control sequences in messages.

If ARG is positive, interpretation is turned on.
Else interpretation is turned off."
  (interactive "P")
  (cond ((and (numberp arg) (> arg 0))
         (setq erc-interpret-controls-p t))
        (arg (setq erc-interpret-controls-p nil))
        (t (setq erc-interpret-controls-p (not erc-interpret-controls-p))))
  (message "ERC color interpretation %s"
           (if erc-interpret-controls-p "ON" "OFF")))

;; Smiley
;;;###autoload(autoload 'erc-smiley-mode "erc-goodies" nil t)
(define-erc-module smiley nil
  "This mode translates text-smileys such as :-) into pictures.
This requires the function `smiley-region', which is defined in
smiley.el, which is part of Gnus."
  ((add-hook 'erc-insert-modify-hook #'erc-smiley)
   (add-hook 'erc-send-modify-hook #'erc-smiley))
  ((remove-hook 'erc-insert-modify-hook #'erc-smiley)
   (remove-hook 'erc-send-modify-hook #'erc-smiley)))

(defun erc-smiley ()
  "Smilify a region.
This function should be used with `erc-insert-modify-hook'."
  (when (fboundp 'smiley-region)
    (smiley-region (point-min) (point-max))))

;; Unmorse
;;;###autoload(autoload 'erc-unmorse-mode "erc-goodies" nil t)
(define-erc-module unmorse nil
  "This mode causes morse code in the current channel to be unmorsed."
  ((add-hook 'erc-insert-modify-hook #'erc-unmorse))
  ((remove-hook 'erc-insert-modify-hook #'erc-unmorse)))

(defun erc-unmorse ()
  "Unmorse some text.
Add this to `erc-insert-modify-hook' if you happen to be on a
channel that has weird people talking in morse to each other.

See also `unmorse-region'."
  (goto-char (point-min))
  (when (re-search-forward "[.-]+[./ -]*[.-]/?" nil t)
    (save-restriction
      (narrow-to-region (match-beginning 0) (match-end 0))
      ;; Turn " / " into "  "
      (goto-char (point-min))
      (while (re-search-forward " / " nil t)
        (replace-match "  "))
      ;; Turn "/ " into "/"
      (goto-char (point-min))
      (while (re-search-forward "/ " nil t)
        (replace-match "/"))
      ;; Unmorse region
      (unmorse-region (point-min) (point-max)))))

;;; erc-occur
(defun erc-occur (string &optional proc)
  "Search for STRING in all buffers related to current server.
If called interactively and prefix argument is given, search on all connected
servers.  If called from a program, PROC specifies the server process."
  (interactive
   (list (read-string "Search for: ")
         (if current-prefix-arg
             nil erc-server-process)))
  (multi-occur (erc-buffer-list nil proc) string))


;;;; querypoll

(declare-function ring-empty-p "ring" (ring))
(declare-function ring-insert "ring" (ring item))
(declare-function ring-insert+extend "ring" (ring item))
(declare-function ring-length "ring" (ring))
(declare-function ring-member "ring" (ring item))
(declare-function ring-ref "ring" (ring index))
(declare-function ring-remove "ring" (ring &optional index))

(defvar-local erc--querypoll-ring nil)
(defvar-local erc--querypoll-timer nil)

(defcustom erc-querypoll-exclude-regexp
  (rx bot (or (: "*" (+ nonl)) (: (+ (in "A-Za-z")) "Serv")) eot)
  "Pattern to skip polling for bots and services you regularly query."
  :group 'erc
  :package-version '(ERC . "5.6")
  :type 'regexp)

;;;###autoload(autoload 'erc-querypoll-mode "erc-goodies" nil t)
(define-erc-module querypoll nil
  "Send periodic \"WHO\" requests for each query buffer.
Omit query participants who are currently present in some channel.
Instead of announcing arrivals and departures, rely on other modules,
like `nickbar', to provide UI feedback when changes occur.

Once ERC implements the `monitor' extension, this module will serve as
an optional fallback for keeping query-participant rolls up to date on
servers that lack support or are stingy with their allotments.  Until
such time, this module should be considered experimental.

This is a local ERC module, so selectively polling only a subset of
query targets is possible but cumbersome.  To do so, ensure
`erc-querypoll-mode' is enabled in the server buffer, and then toggle it
as appropriate in desired query buffers.  To stop polling for the
current connection, toggle off the command \\[erc-querypoll-mode] from a
server buffer, or run \\`M-x C-u erc-querypoll-disable RET' from a
target buffer."
  ((if erc--target
       (if (erc-query-buffer-p)
           (progn ; accommodate those who eschew `erc-modules'
             (erc-with-server-buffer
               (unless erc-querypoll-mode
                 (erc-querypoll-mode +1)))
             (erc--querypoll-subscribe (current-buffer)))
         (erc-querypoll-mode -1))
     (cl-assert (not erc--decouple-query-and-channel-membership-p))
     (setq-local erc--querypoll-ring (make-ring 5))
     (erc-with-all-buffers-of-server erc-server-process nil
       (unless erc-querypoll-mode
         (erc-querypoll-mode +1)))))
  ((when erc--querypoll-timer
     (cancel-timer erc--querypoll-timer))
   (if erc--target
       (when-let (((erc-query-buffer-p))
                  (ring (erc-with-server-buffer erc--querypoll-ring))
                  (index (ring-member ring (current-buffer)))
                  ((not (erc--querypoll-target-in-chan-p (current-buffer)))))
         (ring-remove ring index)
         (unless (erc-current-nick-p (erc-target))
           (erc-remove-current-channel-member (erc-target))))
     (erc-with-all-buffers-of-server erc-server-process #'erc-query-buffer-p
       (erc-querypoll-mode -1)))
   (kill-local-variable 'erc--querypoll-ring)
   (kill-local-variable 'erc--querypoll-timer))
  'local)

(cl-defmethod erc--queries-current-p (&context (erc-querypoll-mode (eql t))) t)

(defvar erc-querypoll-period-params '(10 10 1)
  "Parameters affecting the delay with respect to the number of buffers.
The elements represent some parameters of an exponential decay function,
a(e)^{-x/b}+c.  The first number (a) affects the overall scaling.  A
higher value means longer delays for all query buffers relative to queue
length.  The second number (b) determines how quickly the delay
decreases as the queue length increases.  Larger values make the delay
taper off more gradually.  The last number (c) sets the minimum delay
between updates regardless of queue length.")

(defun erc--querypoll-compute-period (queue-size)
  "Calculate delay based on QUEUE-SIZE."
  (let ((scale (nth 0 erc-querypoll-period-params))
        (rate (* 1.0 (nth 1 erc-querypoll-period-params)))
        (min (nth 2 erc-querypoll-period-params)))
    (+ (* scale (exp (/ (- queue-size) rate))) min)))

(defun erc--querypoll-target-in-chan-p (buffer)
  "Determine whether buffer's target, as a user, is joined to any channels."
  (and-let*
      ((target (erc--target-string (buffer-local-value 'erc--target buffer)))
       (user (erc-get-server-user target))
       (buffers (erc-server-user-buffers user))
       ((seq-some #'erc-channel-p buffers)))))

(defun erc--querypoll-get-length (ring)
  "Return the effective length of RING, discounting chan members."
  (let ((count 0))
    (dotimes (i (ring-length ring))
      (unless (erc--querypoll-target-in-chan-p (ring-ref ring i))
        (cl-incf count 1)))
    count))

(defun erc--querypoll-get-next (ring)
  (let ((n (ring-length ring)))
    (catch 'found
      (while (natnump (cl-decf n))
        (when-let ((buffer (ring-remove ring))
                   ((buffer-live-p buffer)))
          ;; Push back buffers for users joined to some chan.
          (if (erc--querypoll-target-in-chan-p buffer)
              (ring-insert ring buffer)
            (throw 'found buffer)))))))

(defun erc--querypoll-subscribe (query-buffer &optional penalty)
  "Add QUERY-BUFFER to FIFO and ensure timer is running."
  (when query-buffer
    (cl-assert (erc-query-buffer-p query-buffer)))
  (erc-with-server-buffer
    (when (and query-buffer
               (not (with-current-buffer query-buffer
                      (or (erc-current-nick-p (erc-target))
                          (string-match erc-querypoll-exclude-regexp
                                        (erc-target)))))
               (not (ring-member erc--querypoll-ring query-buffer)))
      (ring-insert+extend erc--querypoll-ring query-buffer))
    (unless erc--querypoll-timer
      (setq erc--querypoll-timer
            (let* ((length (erc--querypoll-get-length erc--querypoll-ring))
                   (period (erc--querypoll-compute-period length)))
              (run-at-time (+ (or penalty 0) period)
                           nil #'erc--querypoll-send (current-buffer)))))))

(defun erc--querypoll-on-352 (target-nick args)
  "Add or update `erc-server-users' data for TARGET-NICK from ARGS.
Then add user to participant rolls in any existing query buffers."
  (pcase-let
      ((`(,_ ,channel ,login ,host ,_server ,nick ,_flags, hop-real) args))
    (when (and (string= channel "*") (erc-nick-equal-p nick target-nick))
      (if-let ((user (erc-get-server-user nick)))
          (erc-update-user user nick host login
                           (erc--extract-352-full-name hop-real))
        ;; Don't add unless target is already known.
        (when (erc-get-buffer nick erc-server-process)
          (erc-add-server-user
           nick (make-erc-server-user
                 :nickname nick :login login :host host
                 :full-name (erc--extract-352-full-name hop-real)))))
      (erc--ensure-query-member nick)
      t)))

;; This uses heuristics to associate replies to the initial request
;; because ERC does not yet support `labeled-response'.
(defun erc--querypoll-send (server-buffer)
  "Send a captive \"WHO\" in SERVER-BUFFER."
  (when (and (buffer-live-p server-buffer)
             (buffer-local-value 'erc-server-connected server-buffer))
    (with-current-buffer server-buffer
      (setq erc--querypoll-timer nil)
      (if-let ((buffer (erc--querypoll-get-next erc--querypoll-ring)))
          (letrec
              ((target (erc--target-string
                        (buffer-local-value 'erc--target buffer)))
               (penalty 0)
               (here-fn (erc-once-with-server-event
                         "352" (lambda (_ parsed)
                                 (erc--querypoll-on-352
                                  target (erc-response.command-args parsed)))))
               (done-fn (erc-once-with-server-event
                         "315"
                         (lambda (_ parsed)
                           (if (memq here-fn erc-server-352-functions)
                               (erc-remove-user
                                (nth 1 (erc-response.command-args parsed)))
                             (remove-hook 'erc-server-352-functions here-fn t))
                           (remove-hook 'erc-server-263-functions fail-fn t)
                           (remove-hook 'erc-server-315-functions done-fn t)
                           (erc--querypoll-subscribe buffer penalty)
                           t)))
               (fail-fn (erc-once-with-server-event
                         "263"
                         (lambda (proc parsed)
                           (setq penalty 60)
                           (funcall done-fn proc parsed)
                           t))))
            (erc-server-send (concat "WHO " target)))
        (unless (ring-empty-p erc--querypoll-ring)
          (erc--querypoll-subscribe nil 30))))))

(provide 'erc-goodies)

;;; erc-goodies.el ends here

;; Local Variables:
;; generated-autoload-file: "erc-loaddefs.el"
;; End:
