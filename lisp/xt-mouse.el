;;; xt-mouse.el --- support the mouse when emacs run in an xterm

;; Copyright (C) 1994, 2000-2014 Free Software Foundation, Inc.

;; Author: Per Abrahamsen <abraham@dina.kvl.dk>
;; Keywords: mouse, terminals

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Enable mouse support when running inside an xterm.

;; This is actually useful when you are running X11 locally, but is
;; working on remote machine over a modem line or through a gateway.

;; It works by translating xterm escape codes into generic emacs mouse
;; events so it should work with any package that uses the mouse.

;; You don't have to turn off xterm mode to use the normal xterm mouse
;; functionality, it is still available by holding down the SHIFT key
;; when you press the mouse button.

;;; Todo:

;; Support multi-click -- somehow.

;;; Code:

(defvar xterm-mouse-debug-buffer nil)

;; Mouse events symbols must have an 'event-kind property with
;; the value 'mouse-click.
(dolist (event '(mouse-1 mouse-2 mouse-3 mouse-4 mouse-5))
  (let ((M-event (intern (concat "M-" (symbol-name event)))))
    (put event 'event-kind 'mouse-click)
    (put M-event 'event-kind 'mouse-click)))

(defun xterm-mouse-translate (_event)
  "Read a click and release event from XTerm."
  (xterm-mouse-translate-1))

(defun xterm-mouse-translate-extended (_event)
  "Read a click and release event from XTerm.
Similar to `xterm-mouse-translate', but using the \"1006\"
extension, which supports coordinates >= 231 (see
http://invisible-island.net/xterm/ctlseqs/ctlseqs.html)."
  (xterm-mouse-translate-1 1006))

(defun xterm-mouse-translate-1 (&optional extension)
  (save-excursion
    (save-window-excursion              ;FIXME: Why?
      (deactivate-mark)                 ;FIXME: Why?
      (let* ((event (xterm-mouse-event extension))
	     (ev-command (nth 0 event))
	     (ev-data    (nth 1 event))
	     (ev-where   (nth 1 ev-data))
             (vec (if (and (symbolp ev-where) (consp ev-where))
                      ;; FIXME: This condition can *never* be non-nil!?!
                      (vector (list ev-where ev-data) event)
                    (vector event)))
	     (is-down (string-match "down-" (symbol-name ev-command))))

          (cond
         ((null event) nil)           ;Unknown/bogus byte sequence!
         (is-down
          (setf (terminal-parameter nil 'xterm-mouse-last-down) event)
          vec)
         (t
          (let* ((down (terminal-parameter nil 'xterm-mouse-last-down))
                 (down-data (nth 1 down))
                 (down-where (nth 1 down-data)))
            (setf (terminal-parameter nil 'xterm-mouse-last-down) nil)
            (cond
             ((null down)
              ;; This is an "up-only" event.  Pretend there was an up-event
              ;; right before and keep the up-event for later.
              (push event unread-command-events)
              (vector (cons (intern (replace-regexp-in-string
                                     "\\`\\([ACMHSs]-\\)*" "\\&down-"
                                     (symbol-name ev-command) t))
                            (cdr event))))
             ((equal ev-where down-where) vec)
           (t
              (let ((drag (if (symbolp ev-where)
                                 0      ;FIXME: Why?!?
                               (list (replace-regexp-in-string
                                      "\\`\\([ACMHSs]-\\)*" "\\&drag-"
                                      (symbol-name ev-command) t)
                                     down-data ev-data))))
                (if (null track-mouse)
                    (vector drag)
                  (push drag unread-command-events)
                  (vector (list 'mouse-movement ev-data)))))))))))))

;; These two variables have been converted to terminal parameters.
;;
;;(defvar xterm-mouse-x 0
;;  "Position of last xterm mouse event relative to the frame.")
;;
;;(defvar xterm-mouse-y 0
;;  "Position of last xterm mouse event relative to the frame.")

(defvar xt-mouse-epoch nil)

;; Indicator for the xterm-mouse mode.

(defun xterm-mouse-position-function (pos)
  "Bound to `mouse-position-function' in XTerm mouse mode."
  (when (terminal-parameter nil 'xterm-mouse-x)
    (setcdr pos (cons (terminal-parameter nil 'xterm-mouse-x)
		      (terminal-parameter nil 'xterm-mouse-y))))
  pos)

(defun xterm-mouse-truncate-wrap (f)
  "Truncate with wrap-around."
  (condition-case nil
      ;; First try the built-in truncate, in case there's no overflow.
      (truncate f)
    ;; In case of overflow, do wraparound by hand.
    (range-error
     ;; In our case, we wrap around every 3 days or so, so if we assume
     ;; a maximum of 65536 wraparounds, we're safe for a couple years.
     ;; Using a power of 2 makes rounding errors less likely.
     (let* ((maxwrap (* 65536 2048))
            (dbig (truncate (/ f maxwrap)))
            (fdiff (- f (* 1.0 maxwrap dbig))))
       (+ (truncate fdiff) (* maxwrap dbig))))))

;; Normal terminal mouse click reporting: expect three bytes, of the
;; form <BUTTON+32> <X+32> <Y+32>.  Return a list (EVENT-TYPE X Y).
(defun xterm-mouse--read-event-sequence-1000 ()
  (let* ((code (- (read-event) 32))
         (type
	  ;; For buttons > 3, the release-event looks differently
	  ;; (see xc/programs/xterm/button.c, function EditorButton),
	  ;; and come in a release-event only, no down-event.
	  (cond ((>= code 64)
		 (format "mouse-%d" (- code 60)))
		((memq code '(8 9 10))
		 (format "M-down-mouse-%d" (- code 7)))
		((memq code '(3 11))
                 (let ((down (car (terminal-parameter
                                   nil 'xterm-mouse-last-down))))
                   (when (and down (string-match "[0-9]" (symbol-name down)))
                     (format (if (eq code 3) "mouse-%s" "M-mouse-%s")
                             (match-string 0 (symbol-name down))))))
		((memq code '(0 1 2))
		 (format "down-mouse-%d" (+ 1 code)))))
         (x (- (read-event) 33))
         (y (- (read-event) 33)))
    (and type (wholenump x) (wholenump y)
         (list (intern type) x y))))

;; XTerm's 1006-mode terminal mouse click reporting has the form
;; <BUTTON> ; <X> ; <Y> <M or m>, where the button and ordinates are
;; in encoded (decimal) form.  Return a list (EVENT-TYPE X Y).
(defun xterm-mouse--read-event-sequence-1006 ()
  (let (button-bytes x-bytes y-bytes c)
    (while (not (eq (setq c (read-event)) ?\;))
      (push c button-bytes))
    (while (not (eq (setq c (read-event)) ?\;))
      (push c x-bytes))
    (while (not (memq (setq c (read-event)) '(?m ?M)))
      (push c y-bytes))
    (list (let* ((code (string-to-number
			(apply 'string (nreverse button-bytes))))
		 (wheel (>= code 64))
		 (down (and (not wheel)
			    (eq c ?M))))
	    (intern (format "%s%smouse-%d"
			    (cond (wheel "")
				  ((< code 4)  "")
				  ((< code 8)  "S-")
				  ((< code 12) "M-")
				  ((< code 16) "M-S-")
				  ((< code 20) "C-")
				  ((< code 24) "C-S-")
				  ((< code 28) "C-M-")
				  ((< code 32) "C-M-S-")
				  (t
				   (error "Unexpected escape sequence from XTerm")))
			    (if down "down-" "")
			    (if wheel
				(- code 60)
			      (1+ (mod code 4))))))
	  (1- (string-to-number (apply 'string (nreverse x-bytes))))
	  (1- (string-to-number (apply 'string (nreverse y-bytes)))))))

(defun xterm-mouse--set-click-count (event click-count)
  (setcdr (cdr event) (list click-count))
  (let ((name (symbol-name (car event))))
    (when (string-match "\\(.*?\\)\\(\\(?:down-\\)?mouse-.*\\)" name)
      (setcar event
              (intern (concat (match-string 1 name)
                              (if (= click-count 2)
                                  "double-" "triple-")
                              (match-string 2 name)))))))

(defun xterm-mouse-event (&optional extension)
  "Convert XTerm mouse event to Emacs mouse event.
EXTENSION, if non-nil, means to use an extension to the usual
terminal mouse protocol; we currently support the value 1006,
which is the \"1006\" extension implemented in Xterm >= 277."
  (let* ((click (cond ((null extension)
		       (xterm-mouse--read-event-sequence-1000))
		      ((eq extension 1006)
		       (xterm-mouse--read-event-sequence-1006))
		      (t
		       (error "Unsupported XTerm mouse protocol")))))
    (when click
      (let* ((type (nth 0 click))
             (x    (nth 1 click))
             (y    (nth 2 click))
             ;; Emulate timestamp information.  This is accurate enough
             ;; for default value of mouse-1-click-follows-link (450msec).
             (timestamp (xterm-mouse-truncate-wrap
                         (* 1000
                            (- (float-time)
                               (or xt-mouse-epoch
                                   (setq xt-mouse-epoch (float-time)))))))
             (w (window-at x y))
             (ltrb (window-edges w))
             (left (nth 0 ltrb))
             (top (nth 1 ltrb))
             (posn (if w
                                (posn-at-x-y (- x left) (- y top) w t)
                              (append (list nil 'menu-bar)
                             (nthcdr 2 (posn-at-x-y x y)))))
             (event (list type posn)))
        (setcar (nthcdr 3 posn) timestamp)

        ;; Try to handle double/triple clicks.
        (let* ((last-click (terminal-parameter nil 'xterm-mouse-last-click))
               (last-type (nth 0 last-click))
               (last-name (symbol-name last-type))
               (last-time (nth 1 last-click))
               (click-count (nth 2 last-click))
               (this-time (float-time))
               (name (symbol-name type)))
          (cond
           ((not (string-match "down-" name))
            ;; For up events, make the up side match the down side.
            (setq this-time last-time)
            (when (and (> click-count 1)
                       (string-match "down-" last-name)
                       (equal name (replace-match "" t t last-name)))
              (xterm-mouse--set-click-count event click-count)))
           ((not last-time) nil)
           ((and (> double-click-time (* 1000 (- this-time last-time)))
                 (equal last-name (replace-match "" t t name)))
            (setq click-count (1+ click-count))
            (xterm-mouse--set-click-count event click-count))
           (t (setq click-count 1)))
          (set-terminal-parameter nil 'xterm-mouse-last-click
                                  (list type this-time click-count)))

        (set-terminal-parameter nil 'xterm-mouse-x x)
        (set-terminal-parameter nil 'xterm-mouse-y y)
        (setq last-input-event event)))))

;;;###autoload
(define-minor-mode xterm-mouse-mode
  "Toggle XTerm mouse mode.
With a prefix argument ARG, enable XTerm mouse mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

Turn it on to use Emacs mouse commands, and off to use xterm mouse commands.
This works in terminal emulators compatible with xterm.  It only
works for simple uses of the mouse.  Basically, only non-modified
single clicks are supported.  When turned on, the normal xterm
mouse functionality for such clicks is still available by holding
down the SHIFT key while pressing the mouse button."
  :global t :group 'mouse
  (funcall (if xterm-mouse-mode 'add-hook 'remove-hook)
           'terminal-init-xterm-hook
           'turn-on-xterm-mouse-tracking-on-terminal)
  (if xterm-mouse-mode
      ;; Turn it on
      (progn
	(setq mouse-position-function #'xterm-mouse-position-function)
        (mapc #'turn-on-xterm-mouse-tracking-on-terminal (terminal-list)))
    ;; Turn it off
    (mapc #'turn-off-xterm-mouse-tracking-on-terminal (terminal-list))
    (setq mouse-position-function nil)))

(defconst xterm-mouse-tracking-enable-sequence
  "\e[?1000h\e[?1006h"
  "Control sequence to enable xterm mouse tracking.
Enables basic tracking, then extended tracking on
terminals that support it.")

(defconst xterm-mouse-tracking-disable-sequence
  "\e[?1006l\e[?1000l"
  "Reset the modes set by `xterm-mouse-tracking-enable-sequence'.")

(defun turn-on-xterm-mouse-tracking-on-terminal (&optional terminal)
  "Enable xterm mouse tracking on TERMINAL."
  (when (and xterm-mouse-mode (eq t (terminal-live-p terminal))
	     ;; Avoid the initial terminal which is not a termcap device.
	     ;; FIXME: is there more elegant way to detect the initial terminal?
	     (not (string= (terminal-name terminal) "initial_terminal")))
    (unless (terminal-parameter terminal 'xterm-mouse-mode)
      ;; Simulate selecting a terminal by selecting one of its frames
      ;; so that we can set the terminal-local `input-decode-map'.
      (with-selected-frame (car (frames-on-display-list terminal))
        (define-key input-decode-map "\e[M" 'xterm-mouse-translate)
        (define-key input-decode-map "\e[<" 'xterm-mouse-translate-extended))
      (send-string-to-terminal xterm-mouse-tracking-enable-sequence terminal)
      (push xterm-mouse-tracking-enable-sequence
            (terminal-parameter nil 'tty-mode-set-strings))
      (push xterm-mouse-tracking-disable-sequence
            (terminal-parameter nil 'tty-mode-reset-strings))
      (set-terminal-parameter terminal 'xterm-mouse-mode t))))

(defun turn-off-xterm-mouse-tracking-on-terminal (terminal)
  "Disable xterm mouse tracking on TERMINAL."
  ;; Only send the disable command to those terminals to which we've already
  ;; sent the enable command.
  (when (and (terminal-parameter terminal 'xterm-mouse-mode)
             (eq t (terminal-live-p terminal)))
    ;; We could remove the key-binding and unset the `xterm-mouse-mode'
    ;; terminal parameter, but it seems less harmful to send this escape
    ;; command too many times (or to catch an unintended key sequence), than
    ;; to send it too few times (or to fail to let xterm-mouse events
    ;; pass by untranslated).
    (send-string-to-terminal xterm-mouse-tracking-disable-sequence terminal)
    (setf (terminal-parameter nil 'tty-mode-set-strings)
          (remq xterm-mouse-tracking-enable-sequence
                (terminal-parameter nil 'tty-mode-set-strings)))
    (setf (terminal-parameter nil 'tty-mode-reset-strings)
          (remq xterm-mouse-tracking-disable-sequence
                (terminal-parameter nil 'tty-mode-reset-strings)))
    (set-terminal-parameter terminal 'xterm-mouse-mode nil)))

(provide 'xt-mouse)

;;; xt-mouse.el ends here
