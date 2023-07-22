;;; erc-goodies.el --- Collection of ERC modules  -*- lexical-binding: t; -*-

;; Copyright (C) 2001-2023 Free Software Foundation, Inc.

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
argument to `recenter', unless `erc-scrolltobottom-relaxed' is
non-nil, in which case, ERC interprets it as additional lines to
scroll down by per message insertion (minus one for the prompt)."
  :group 'erc-display
  :type '(choice integer (const nil)))

(defcustom erc-scrolltobottom-all nil
  "Whether to scroll all windows or just the selected one.
A value of nil preserves pre-5.6 behavior, in which scrolling
only affects the selected window.  Users should consider its
non-nil behavior experimental for the time being.  Note also that
ERC expects this option to be configured before module
initialization."
  :group 'erc-display
  :package-version '(ERC . "5.6") ; FIXME sync on release
  :type 'boolean)

(defcustom erc-scrolltobottom-relaxed nil
  "Whether to forgo forcing prompt to the bottom of the window.
When non-nil, and point is at the prompt, ERC scrolls the window
up when inserting messages, making the prompt appear stationary.
Users who find this effect too \"stagnant\" can adjust the option
`erc-input-line-position', which ERC borrows to express a scroll
step offset when this option is non-nil.  Setting that value to
zero lets the prompt drift toward the bottom by one line per
message, which is generally slow enough not to distract while
composing input.  Of course, this doesn't apply when receiving a
large influx of messages, such as after typing \"/msg NickServ
help\".  Note that ERC only considers this option when the
experimental companion option `erc-scrolltobottom-all' is enabled
and, only then, during module setup."
  :group 'erc-display
  :package-version '(ERC . "5.6") ; FIXME sync on release
  :type 'boolean)

;;;###autoload(autoload 'erc-scrolltobottom-mode "erc-goodies" nil t)
(define-erc-module scrolltobottom nil
  "This mode causes the prompt to stay at the end of the window."
  ((add-hook 'erc-mode-hook #'erc--scrolltobottom-setup)
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
   (if erc-scrolltobottom-all
       (progn
         (remove-hook 'erc-insert-pre-hook #'erc--scrolltobottom-on-pre-insert)
         (remove-hook 'erc-send-completed-hook #'erc--scrolltobottom-all)
         (remove-hook 'erc-insert-done-hook #'erc--scrolltobottom-all)
         (remove-hook 'erc-pre-send-functions
                      #'erc--scrolltobottom-on-pre-insert))
     (remove-hook 'erc-insert-done-hook #'erc-possibly-scroll-to-bottom))))

(defun erc-possibly-scroll-to-bottom ()
  "Like `erc-add-scroll-to-bottom', but only if window is selected."
  (when (eq (selected-window) (get-buffer-window))
    (erc-scroll-to-bottom)))

(defvar-local erc--scrolltobottom-relaxed-commands '(end-of-buffer)
  "Commands triggering a forced scroll to prompt.
Only applies with `erc-scrolltobottom-relaxed' while away from
prompt.")

(defvar-local erc--scrolltobottom-window-info nil
  "Alist with windows as keys and lists of window-related info as values.
Values are lists containing the last window start position and
the last \"window line\" of point.  The \"window line\", which
may be nil, is the number of lines between `window-start' and
`window-point', inclusive.")

(defvar erc--scrolltobottom-post-force-commands
  '(beginning-of-buffer
    electric-newline-and-maybe-indent
    default-indent-new-line)
  "Commands that force a scroll after execution at prompt.
That is, ERC recalculates the window's start instead of blindly
restoring it.")

(defvar erc--scrolltobottom-relaxed-skip-commands
  '(recenter-top-bottom scroll-down-command)
  "Commands exempt from triggering a stash and restore of `window-start'.
Only applies with `erc-scrolltobottom-relaxed' while in the input
area.")

(defun erc--scrolltobottom-on-pre-command ()
  (when (and (eq (selected-window) (get-buffer-window))
             (>= (point) erc-input-marker))
    (setq erc--scrolltobottom-window-info
          (list (list (selected-window)
                      (window-start)
                      (count-screen-lines (window-start) (point-max)))))))

(defun erc--scrolltobottom-on-post-command ()
  "Restore window start or scroll to prompt and recenter.
When `erc--scrolltobottom-window-info' is non-nil and its first
item is associated with the selected window, restore start of
window so long as prompt hasn't moved.  Expect buffer to be
unnarrowed."
  (when (eq (selected-window) (get-buffer-window))
    (if-let (((not (input-pending-p)))
             (erc--scrolltobottom-window-info)
             (found (car erc--scrolltobottom-window-info))
             ((eq (car found) (selected-window)))
             ((not (memq this-command
                         erc--scrolltobottom-post-force-commands)))
             ((= (nth 2 found)
                 (count-screen-lines (window-start) (point-max)))))
        (set-window-start (selected-window) (nth 1 found))
      (erc--scrolltobottom-confirm))
    (setq erc--scrolltobottom-window-info nil)))

(defun erc--scrolltobottom-on-pre-command-relaxed ()
  "Maybe scroll to bottom when away from prompt.
When `erc-scrolltobottom-relaxed' is active, only scroll when
prompt is past window's end and the command is `end-of-buffer' or
`self-insert-command' (assuming `move-to-prompt' is active).
When at prompt and current command does not appear in
`erc--scrolltobottom-relaxed-skip-commands', stash
`erc--scrolltobottom-window-info' for the selected window.
Assume an unnarrowed buffer."
  (when (eq (selected-window) (get-buffer-window))
    (when (and (not (input-pending-p))
               (< (point) erc-input-marker)
               (memq this-command erc--scrolltobottom-relaxed-commands)
               (< (window-end nil t) erc-input-marker))
      (save-excursion
        (goto-char (point-max))
        (recenter (or erc-input-line-position -1))))
    (when (and (>= (point) erc-input-marker)
               (not (memq this-command
                          erc--scrolltobottom-relaxed-skip-commands)))
      (setq erc--scrolltobottom-window-info
            (list (list (selected-window)
                        (window-start)
                        (count-screen-lines (window-start) (point-max))))))))

(defun erc--scrolltobottom-on-post-command-relaxed ()
  "Set window start or scroll when data was captured on pre-command."
  (when-let (((eq (selected-window) (get-buffer-window)))
             (erc--scrolltobottom-window-info)
             (found (car erc--scrolltobottom-window-info))
             ((eq (car found) (selected-window))))
    (if (and (not (memq this-command erc--scrolltobottom-post-force-commands))
             (= (nth 2 found)
                (count-screen-lines (window-start) (point-max))))
        (set-window-start (selected-window) (nth 1 found))
      (recenter (nth 2 found)))
    (setq erc--scrolltobottom-window-info nil)))

;; It may be desirable to also restore the relative line position of
;; window point after changing dimensions.  Perhaps stashing the
;; previous ratio of window line to body height and later recentering
;; proportionally would achieve this.
(defun erc--scrolltobottom-at-prompt-minibuffer-active ()
  "Scroll window to bottom when at prompt and using the minibuffer."
  ;; This is redundant or ineffective in the selected window if at
  ;; prompt or if only one window exists.
  (unless (or (input-pending-p)
              (and (minibuffer-window-active-p (minibuffer-window))
                   (eq (old-selected-window) (minibuffer-window))))
    (erc--scrolltobottom-confirm)))

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
        (setf (window-start window) (cadr found)))))
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

(cl-defgeneric erc--scrolltobottom-setup ()
  "Arrange for scrolling to bottom on window configuration changes.
Undo that arrangement when disabling `erc-scrolltobottom-mode'."
  (if erc-scrolltobottom-mode
      (add-hook 'post-command-hook #'erc-scroll-to-bottom nil t)
    (remove-hook 'post-command-hook #'erc-scroll-to-bottom t)))

(cl-defmethod erc--scrolltobottom-setup (&context
                                         (erc-scrolltobottom-all (eql t)))
  "Add and remove local hooks specific to `erc-scrolltobottom-all'."
  (if erc-scrolltobottom-mode
      (if erc-scrolltobottom-relaxed
          (progn
            (when (or (bound-and-true-p erc-move-to-prompt-mode)
                      (memq 'move-to-prompt erc-modules))
              (cl-pushnew 'self-insert-command
                          erc--scrolltobottom-relaxed-commands))
            (add-hook 'post-command-hook
                      #'erc--scrolltobottom-on-post-command-relaxed 60 t)
            (add-hook 'pre-command-hook ; preempt `move-to-prompt'
                      #'erc--scrolltobottom-on-pre-command-relaxed 60 t))
        (add-hook 'window-configuration-change-hook
                  #'erc--scrolltobottom-at-prompt-minibuffer-active nil t)
        (add-hook 'pre-command-hook
                  #'erc--scrolltobottom-on-pre-command 60 t)
        (add-hook 'post-command-hook
                  #'erc--scrolltobottom-on-post-command 60 t))
    (remove-hook 'window-configuration-change-hook
                 #'erc--scrolltobottom-at-prompt-minibuffer-active t)
    (remove-hook 'pre-command-hook
                 #'erc--scrolltobottom-on-pre-command t)
    (remove-hook 'post-command-hook
                 #'erc--scrolltobottom-on-post-command t)
    (remove-hook 'pre-command-hook
                 #'erc--scrolltobottom-on-pre-command-relaxed t)
    (remove-hook 'post-command-hook
                 #'erc--scrolltobottom-on-post-command-relaxed t)
    (kill-local-variable 'erc--scrolltobottom-relaxed-commands)
    (kill-local-variable 'erc--scrolltobottom-window-info)))

(cl-defmethod erc--scrolltobottom-on-pre-insert (_input-or-string)
  "Remember the `window-start' before inserting a message."
  (setq erc--scrolltobottom-window-info
        (mapcar (lambda (w)
                  (list w
                        (window-start w)
                        (and-let*
                            ((erc-scrolltobottom-relaxed)
                             (c (count-screen-lines (window-start w)
                                                    (point-max) nil w)))
                          (if (= ?\n (char-before (point-max))) (1+ c) c))))
                (get-buffer-window-list nil nil 'visible))))

(cl-defmethod erc--scrolltobottom-on-pre-insert ((input erc-input))
  "Remember the `window-start' before inserting a message."
  (when (erc-input-insertp input)
    (cl-call-next-method)))

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
  ((add-hook 'erc-insert-post-hook #'erc-make-read-only)
   (add-hook 'erc-send-post-hook #'erc-make-read-only))
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
  ((add-hook 'erc-insert-pre-hook  #'erc-keep-place 85))
  ((remove-hook 'erc-insert-pre-hook  #'erc-keep-place)))

(defcustom erc-keep-place-indicator-style t
  "Flavor of visual indicator applied to kept place.
For use with the `keep-place-indicator' module.  A value of `arrow'
displays an arrow in the left fringe or margin.  When it's
`face', ERC adds the face `erc-keep-place-indicator-line' to the
appropriate line.  A value of t does both."
  :group 'erc
  :package-version '(ERC . "5.6") ; FIXME sync on release
  :type '(choice (const t) (const server) (const target)))

(defcustom erc-keep-place-indicator-buffer-type t
  "ERC buffer type in which to display `keep-place-indicator'.
A value of t means \"all\" ERC buffers."
  :group 'erc
  :package-version '(ERC . "5.6") ; FIXME sync on release
  :type '(choice (const t) (const server) (const target)))

(defcustom erc-keep-place-indicator-follow nil
  "Whether to sync visual kept place to window's top when reading.
For use with `erc-keep-place-indicator-mode'."
  :group 'erc
  :package-version '(ERC . "5.6") ; FIXME sync on release
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

(defun erc--keep-place-indicator-on-window-configuration-change ()
  "Maybe sync `erc--keep-place-indicator-overlay'.
Specifically, do so unless switching to or from another window in
the active frame."
  (when erc-keep-place-indicator-follow
    (unless (or (minibuffer-window-active-p (minibuffer-window))
                (eq (window-old-buffer) (current-buffer)))
      (when (< (overlay-end erc--keep-place-indicator-overlay)
               (window-start)
               erc-insert-marker)
        (erc-keep-place-move (window-start))))))

(defun erc--keep-place-indicator-setup ()
  "Initialize buffer for maintaining `erc--keep-place-indicator-overlay'."
  (require 'fringe)
  (erc--restore-initialize-priors erc-keep-place-indicator-mode
    erc--keep-place-indicator-overlay (make-overlay 0 0))
  (add-hook 'erc-keep-place-mode-hook
            #'erc--keep-place-indicator-on-global-module nil t)
  (add-hook 'window-configuration-change-hook
            #'erc--keep-place-indicator-on-window-configuration-change nil t)
  (when-let* (((memq erc-keep-place-indicator-style '(t arrow)))
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

;;;###autoload(put 'keep-place-indicator 'erc--feature 'erc-goodies)
(define-erc-module keep-place-indicator nil
  "Buffer-local `keep-place' with fringe arrow and/or highlighted face.
Play nice with global module `keep-place' but don't depend on it.
Expect that users may want different combinations of `keep-place'
and `keep-place-indicator' in different buffers."
  ((cond (erc-keep-place-mode)
         ((memq 'keep-place erc-modules)
          (erc-keep-place-mode +1))
         ;; Enable a local version of `keep-place-mode'.
         (t (add-hook 'erc-insert-pre-hook  #'erc-keep-place 85 t)))
   (if (pcase erc-keep-place-indicator-buffer-type
         ('target erc--target)
         ('server (not erc--target))
         ('t t))
       (erc--keep-place-indicator-setup)
     (erc-keep-place-indicator-mode -1)))
  ((when erc--keep-place-indicator-overlay
     (delete-overlay erc--keep-place-indicator-overlay))
   (remove-hook 'window-configuration-change-hook
                #'erc--keep-place-indicator-on-window-configuration-change t)
   (remove-hook 'erc-keep-place-mode-hook
                #'erc--keep-place-indicator-on-global-module t)
   (remove-hook 'erc-insert-pre-hook  #'erc-keep-place t)
   (kill-local-variable 'erc--keep-place-indicator-overlay))
  'local)

(defun erc--keep-place-indicator-on-global-module ()
  "Ensure `keep-place-indicator' can cope with `erc-keep-place-mode'.
That is, ensure the local module can survive a user toggling the
global one."
  (if erc-keep-place-mode
      (remove-hook 'erc-insert-pre-hook  #'erc-keep-place t)
    (add-hook 'erc-insert-pre-hook  #'erc-keep-place 85 t)))

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
                  (and (frame-visible-p (selected-frame))
                       (get-buffer-window (current-buffer) (selected-frame))))
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
                               erc-cmd-SMV
                               erc-cmd-LASTLOG)
  "List of commands that are aliases for CTCP ACTION or for ERC messages.

If a command's function symbol is in this list, the typed command
does not appear in the ERC buffer after the user presses ENTER.")

;;;###autoload(autoload 'erc-noncommands-mode "erc-goodies" nil t)
(define-erc-module noncommands nil
  "This mode distinguishes non-commands.
Commands listed in `erc-insert-this' know how to display
themselves."
  ((add-hook 'erc--input-review-functions #'erc-send-distinguish-noncommands))
  ((remove-hook 'erc--input-review-functions
                #'erc-send-distinguish-noncommands)))

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
  '((t :foreground "White" :background "Black"))
  "ERC inverse face."
  :group 'erc-faces)

(defface erc-spoiler-face
  '((((background light)) :foreground "DimGray" :background "DimGray")
    (((background dark)) :foreground "LightGray" :background "LightGray"))
  "ERC spoiler face."
  :group 'erc-faces)

(defface erc-underline-face '((t :underline t))
  "ERC underline face."
  :group 'erc-faces)

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
     (t (erc-log (format "   Wrong color: %s" n)) '(default)))))

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
     (t (erc-log (format "   Wrong color: %s" n)) '(default)))))

;;;###autoload(autoload 'erc-irccontrols-mode "erc-goodies" nil t)
(define-erc-module irccontrols nil
  "This mode enables the interpretation of IRC control chars."
  ((add-hook 'erc-insert-modify-hook #'erc-controls-highlight)
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
                           (setq bg bg-color))
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
                      (setq bg bg-color))
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
  (if (and fg bg (equal fg bg))
      (progn
        (setq fg 'erc-spoiler-face
              bg nil)
        (put-text-property from to 'mouse-face 'erc-inverse-face str))
    (when fg (setq fg (erc-get-fg-color-face fg)))
    (when bg (setq bg (erc-get-bg-color-face bg))))
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

(provide 'erc-goodies)

;;; erc-goodies.el ends here

;; Local Variables:
;; generated-autoload-file: "erc-loaddefs.el"
;; End:
