;;; xt-mouse.el --- support the mouse when emacs run in an xterm -*- lexical-binding: t -*-

;; Copyright (C) 1994, 2000-2026 Free Software Foundation, Inc.

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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Enable mouse support when running inside an xterm.

;; This is actually useful when you are running X11 locally, but is
;; working on remote machine over a modem line or through a gateway.

;; It works by translating xterm escape codes into generic Emacs mouse
;; events so it should work with any package that uses the mouse.

;; You don't have to turn off xterm mode to use the normal xterm mouse
;; functionality, it is still available by holding down the SHIFT key
;; when you press the mouse button.

;;; Todo:

;; Support multi-click -- somehow.

;;; Code:

(defvar xterm-mouse-debug-buffer nil)

(defun xterm-mouse-translate (_event)
  "Read a click and release event from XTerm."
  (xterm-mouse-translate-1))

(defun xterm-mouse-translate-extended (_event)
  "Read a click and release event from XTerm.
Similar to `xterm-mouse-translate', but using the \"1006\"
extension, which supports coordinates >= 231 (see
https://invisible-island.net/xterm/ctlseqs/ctlseqs.html)."
  (xterm-mouse-translate-1 1006))

(defun xterm-mouse-translate-1 (&optional extension)
  (save-excursion
    (let* ((event (xterm-mouse-event extension))
	   (ev-command (nth 0 event))
	   (ev-data    (nth 1 event))
	   (ev-window  (nth 0 ev-data))
	   (ev-where   (nth 1 ev-data))
	   (last-window (terminal-parameter nil 'xterm-mouse-last-window))
	   (vec (vector event))
	   (is-move (eq 'mouse-movement ev-command))
	   (is-down (string-match "down-" (symbol-name ev-command))))

      ;; Mouse events symbols must have an 'event-kind property set.
      ;; Most of them use the value 'mouse-click, but 'mouse-movement has
      ;; a different value.  See head_table in keyboard.c. (bug#67457)
      (when ev-command (put ev-command 'event-kind
                            (if (eq ev-command 'mouse-movement)
                                'mouse-movement
                              'mouse-click)))

      ;; remember window of current mouse position
      (set-terminal-parameter nil 'xterm-mouse-last-window ev-window)

      (cond
       ((null event) nil)		;Unknown/bogus byte sequence!
       (is-down
	(setf (terminal-parameter nil 'xterm-mouse-last-down)
              ;; EVENT might be handed back to the input queue, which
              ;; might modify it.  Copy it into the terminal parameter
              ;; to guard against that.
              (copy-sequence event))
	vec)
       (is-move
        (xterm-mouse--handle-mouse-movement)
        ;; after mouse movement autoselect the mouse window, but ...
	(cond ((and mouse-autoselect-window
                    ;; ignore modeline, tab-bar, menu-bar and so forth ...
		    (windowp ev-window)
                    ;; and don't deselect the minibuffer ...
                    (not (window-minibuffer-p (selected-window)))
                    ;; and select only, if mouse is over a new window ...
                    (not (eq ev-window last-window))
                    ;; which is different from the selected window
		    (not (eq ev-window (selected-window))))
	       (put 'select-window 'event-kind 'switch-frame)
	       (push `(select-window (,ev-window)) unread-command-events)
               [])
	       ;;(vector `(select-window (,ev-window))))
              (track-mouse vec)
              (t [])))
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
			    0		;FIXME: Why?!?
			  (list (intern (replace-regexp-in-string
					 "\\`\\([ACMHSs]-\\)*" "\\&drag-"
					 (symbol-name ev-command) t))
				down-data ev-data))))
	      (if (null track-mouse)
		  (vector drag)
		(push drag unread-command-events)
                (xterm-mouse--handle-mouse-movement)
		(vector (list 'mouse-movement ev-data))))))))))))

(defun xterm-mouse--handle-mouse-movement ()
  "Handle mouse motion that was just generated for XTerm mouse."
  (when-let* ((frame (terminal-parameter nil 'xterm-mouse-frame)))
    (display--update-for-mouse-movement
     frame
     (terminal-parameter nil 'xterm-mouse-x)
     (terminal-parameter nil 'xterm-mouse-y))))

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
  (if (terminal-parameter nil 'xterm-mouse-x)
      (cons (terminal-parameter nil 'xterm-mouse-frame)
            (cons (terminal-parameter nil 'xterm-mouse-x)
		  (terminal-parameter nil 'xterm-mouse-y)))
    pos))

(define-obsolete-function-alias 'xterm-mouse-truncate-wrap 'truncate "27.1")

(defcustom xterm-mouse-utf-8 nil
  "Non-nil if UTF-8 coordinates should be used to read mouse coordinates.
Set this to non-nil if you are sure that your terminal
understands UTF-8 coordinates, but not SGR coordinates."
  :version "25.1"
  :type 'boolean
  :risky t
  :group 'xterm)

(defun xterm-mouse--read-coordinate ()
  "Read a mouse coordinate from the current terminal.
If `xterm-mouse-utf-8' was non-nil when
`turn-on-xterm-mouse-tracking-on-terminal' was called, reads the
coordinate as an UTF-8 code unit sequence; otherwise, reads a
single byte."
  (let ((previous-keyboard-coding-system (keyboard-coding-system))
        (utf-8-p (terminal-parameter nil 'xterm-mouse-utf-8))
        ;; Prevent conversions inside 'read-char' due to input method,
        ;; when we call 'read-char' below with 2nd argument non-nil.
        (input-method-function nil))
    (unwind-protect
        (progn
          (set-keyboard-coding-system (if utf-8-p 'utf-8-unix 'no-conversion))
          (read-char nil
                     ;; Force 'read-char' to decode UTF-8 sequences if
                     ;; 'xterm-mouse-utf-8' is non-nil.
                     utf-8-p
                     ;; Wait only a little; we assume that the entire
                     ;; escape sequence has already been sent when
                     ;; this function is called.
                     0.1))
      (set-keyboard-coding-system previous-keyboard-coding-system))))

;; In default mode, each numeric parameter of XTerm's mouse report is
;; a single char, possibly encoded as utf-8.  The actual numeric
;; parameter then is obtained by subtracting 32 from the character
;; code.  In extended mode the parameters are returned as decimal
;; string delimited either by semicolons or for the last parameter by
;; one of the characters "m" or "M".  If the last character is a "m",
;; then the mouse event was a button release, else it was a button
;; press or a mouse motion.  Return value is a cons cell with
;; (NEXT-NUMERIC-PARAMETER . LAST-CHAR)
(defun xterm-mouse--read-number-from-terminal (extension)
  (let (c)
    (if extension
        (let ((n 0))
          (while (progn
                   (setq c (read-char))
                   (<= ?0 c ?9))
            (setq n (+ (* 10 n) c (- ?0))))
          (cons n c))
      (cons (- (setq c (xterm-mouse--read-coordinate)) 32) c))))

;; XTerm reports mouse events as
;; <EVENT-CODE> <X> <Y> in default mode, and
;; <EVENT-CODE> ";" <X> ";" <Y> <"M" or "m"> in extended mode.
;; The macro read-number-from-terminal takes care of reading
;; the response parameters appropriately.  The EVENT-CODE differs
;; slightly between default and extended mode.
;; Return a list (EVENT-TYPE-SYMBOL X Y).
(defun xterm-mouse--read-event-sequence (&optional extension)
  (pcase-let*
      ((`(,code . ,_) (xterm-mouse--read-number-from-terminal extension))
       (`(,x . ,_) (xterm-mouse--read-number-from-terminal extension))
       (`(,y . ,c) (xterm-mouse--read-number-from-terminal extension))
       (wheel (/= (logand code 64) 0))
       (move (/= (logand code 32) 0))
       (ctrl (/= (logand code 16) 0))
       (meta (/= (logand code 8) 0))
       (shift (/= (logand code 4) 0))
       (down (and (not wheel)
                  (not move)
                  (if extension
                      (eq c ?M)
                    (/= (logand code 3) 3))))
       (btn (cond
             ((or extension down wheel)
              (+ (logand code 3) (if wheel 4 1)))
              ;; The default mouse protocol does not report the button
              ;; number in release events: extract the button number
              ;; from last button-down event.
             ((terminal-parameter nil 'xterm-mouse-last-down)
              (string-to-number
               (substring
                (symbol-name
                 (car (terminal-parameter nil 'xterm-mouse-last-down)))
                -1)))
             ;; Spurious release event without previous button-down
             ;; event: assume, that the last button was button 1.
             (t 1)))
       (sym
        (if move 'mouse-movement
          (intern
           (concat
            (if ctrl "C-" "")
            (if meta "M-" "")
            (if shift "S-" "")
            (if down "down-" "")
            (let ((remap (alist-get btn mouse-wheel-buttons)))
              (if remap
                  (symbol-name remap)
                (format "mouse-%d" btn))))))))
    (list sym (1- x) (1- y))))

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
  (let ((click (cond ((memq extension '(1006 nil))
		      (xterm-mouse--read-event-sequence extension))
		     (t
		      (error "Unsupported XTerm mouse protocol")))))
    (when (and click
               ;; In very obscure circumstances, the click may become
               ;; invalid (see bug#17378).
               (>= (nth 1 click) 0))
      (let* ((type (nth 0 click))
             (x    (nth 1 click))
             (y    (nth 2 click))
             ;; Emulate timestamp information.  This is accurate enough
             ;; for default value of mouse-1-click-follows-link (450msec).
	     (timestamp (if (not xt-mouse-epoch)
			    (progn (setq xt-mouse-epoch (float-time)) 0)
			  (car (time-convert (time-since xt-mouse-epoch)
					     1000))))
             ;; FIXME: The test for running in batch mode is here solely
             ;; for the sake of xt-mouse-tests where the only frame is
             ;; the initial frame.  The same goes for the computation of
             ;; x and y.
             (frame-and-xy (unless noninteractive (tty-frame-at x y)))
             (frame (nth 0 frame-and-xy))
             (x (or (nth 1 frame-and-xy) x))
             (y (or (nth 2 frame-and-xy) y))
             (w (window-at x y frame))
             (posn
	      (if w
		  (let* ((ltrb (window-edges w))
			 (left (nth 0 ltrb))
			 (top (nth 1 ltrb)))
		    (posn-at-x-y (- x left) (- y top) w t))
		(let* ((frame-has-menu-bar
			(not (zerop (frame-parameter frame 'menu-bar-lines))))
		       (frame-has-tab-bar
			(not (zerop (frame-parameter frame 'tab-bar-lines))))
		       (item
			(cond
                         ((and frame-has-menu-bar (eq y 0))
			  'menu-bar)
			 ((and frame-has-tab-bar
			       (or (and frame-has-menu-bar
					(eq y 1))
				   (eq y 0)))
                          'tab-bar)
			 ((eq x -1)
			  (cond
			   ((eq y -1) 'top-left-corner)
			   ((eq y (frame-height frame)) 'bottom-left-corner)
			   (t 'left-edge)))
			 ((eq x (frame-width frame))
			  (cond
			   ((eq y -1) 'top-right-corner)
			   ((eq y (frame-height frame)) 'bottom-right-corner)
			   (t 'right-edge)))
			 ((eq y -1) 'top-edge)
			 (t 'bottom-edge))))
		  (append (list (unless (memq item '(menu-bar tab-bar))
				  frame)
				item)
			  (nthcdr 2 (posn-at-x-y x y (selected-frame)))))))
             (event (list type posn)))
        (setcar (nthcdr 3 posn) timestamp)

        ;; Try to handle double/triple clicks.
        (let* ((last-click (terminal-parameter nil 'xterm-mouse-last-click))
               (last-type (nth 0 last-click))
               (last-name (symbol-name last-type))
               (last-time (nth 1 last-click))
               (click-count (nth 2 last-click))
               (last-x (nth 3 last-click))
               (last-y (nth 4 last-click))
               (this-time (float-time))
               (name (symbol-name type)))
          (cond
           ((not (string-match "down-" name))
            ;; For up events, make the up side match the down side.
            (setq this-time last-time)
            (when (and click-count (> click-count 1)
                       (string-match "down-" last-name)
                       (equal name (replace-match "" t t last-name)))
              (xterm-mouse--set-click-count event click-count)))
           ((and last-time
                 double-click-time
                 (or (eq double-click-time t)
                     (> double-click-time (* 1000 (- this-time last-time))))
                 (<= (abs (- x last-x))
                     (/ double-click-fuzz 8))
                 (<= (abs (- y last-y))
                     (/ double-click-fuzz 8))
                 (equal last-name (replace-match "" t t name)))
            (setq click-count (1+ click-count))
            (xterm-mouse--set-click-count event click-count))
           (t (setq click-count 1)))
          (set-terminal-parameter nil 'xterm-mouse-last-click
                                  (list type this-time click-count x y)))

        (set-terminal-parameter nil 'xterm-mouse-x x)
        (set-terminal-parameter nil 'xterm-mouse-y y)
        (set-terminal-parameter nil 'xterm-mouse-frame frame)
        (setq last-input-event event)))))

;;;###autoload
(defvar xterm-mouse-mode-called nil
  "If `xterm-mouse-mode' has been called already.
This can be used to detect if xterm-mouse-mode was explicitly set.")

;;;###autoload
(define-minor-mode xterm-mouse-mode
  "Toggle XTerm mouse mode.

Turn it on to use Emacs mouse commands, and off to use xterm mouse
commands.  This works in terminal emulators compatible with xterm.  When
turned on, the normal xterm mouse functionality for such clicks is still
available by holding down the SHIFT key while pressing the mouse button.

On text terminals that Emacs knows are compatible with the mouse as well
as other critical editing functionality, this is automatically turned on
at startup.  See Info node `(elisp)Terminal-Specific' and `xterm--init'."
  :global t :group 'mouse
  :version "31.1"
  (setq xterm-mouse-mode-called t)
  (funcall (if xterm-mouse-mode 'add-hook 'remove-hook)
           'terminal-init-xterm-hook
           'turn-on-xterm-mouse-tracking-on-terminal)
  (if xterm-mouse-mode
      ;; Turn it on
      (progn
        (setq mouse-position-function #'xterm-mouse-position-function
              tty-menu-calls-mouse-position-function t)
        (mapc #'turn-on-xterm-mouse-tracking-on-terminal (terminal-list)))
    ;; Turn it off
    (mapc #'turn-off-xterm-mouse-tracking-on-terminal (terminal-list))
    (setq mouse-position-function nil
          tty-menu-calls-mouse-position-function nil)))

(defun xterm-mouse-tracking-enable-sequence ()
  "Return a control sequence to enable XTerm mouse tracking.
The returned control sequence enables basic mouse tracking, mouse
motion events and finally extended tracking on terminals that
support it.  The following escape sequences are understood by
modern xterms:

\"\\e[?1000h\" \"Basic mouse mode\": Enables reports for mouse
            clicks.  There is a limit to the maximum row/column
            position (<= 223), which can be reported in this
            basic mode.

\"\\e[?1003h\" \"Mouse motion mode\": Enables reports for mouse
            motion events.

\"\\e[?1005h\" \"UTF-8 coordinate extension\": Enables an
            extension to the basic mouse mode, which uses UTF-8
            characters to overcome the 223 row/column limit.
            This extension may conflict with non UTF-8
            applications or non UTF-8 locales.  It is only
            enabled when the option `xterm-mouse-utf-8' is
            non-nil.

\"\\e[?1006h\" \"SGR coordinate extension\": Enables a newer
            alternative extension to the basic mouse mode, which
            overcomes the 223 row/column limit without the
            drawbacks of the UTF-8 coordinate extension.

The two extension modes are mutually exclusive, where the last
given escape sequence takes precedence over the former."
  (apply #'concat (xterm-mouse--tracking-sequence ?h)))

(defconst xterm-mouse-tracking-enable-sequence
  "\e[?1000h\e[?1003h\e[?1005h\e[?1006h"
  "Control sequence to enable xterm mouse tracking.
Enables basic mouse tracking, mouse motion events and finally
extended tracking on terminals that support it.  The following
escape sequences are understood by modern xterms:

\"\\e[?1000h\" \"Basic mouse mode\": Enables reports for mouse
            clicks.  There is a limit to the maximum row/column
            position (<= 223), which can be reported in this
            basic mode.

\"\\e[?1003h\" \"Mouse motion mode\": Enables reports for mouse
            motion events.

\"\\e[?1005h\" \"UTF-8 coordinate extension\": Enables an extension
            to the basic mouse mode, which uses UTF-8
            characters to overcome the 223 row/column limit.  This
            extension may conflict with non UTF-8 applications or
            non UTF-8 locales.

\"\\e[?1006h\" \"SGR coordinate extension\": Enables a newer
            alternative extension to the basic mouse mode, which
            overcomes the 223 row/column limit without the
            drawbacks of the UTF-8 coordinate extension.

The two extension modes are mutually exclusive, where the last
given escape sequence takes precedence over the former.")

(make-obsolete-variable
 'xterm-mouse-tracking-enable-sequence
 "use the function `xterm-mouse-tracking-enable-sequence' instead."
 "25.1")

(defun xterm-mouse-tracking-disable-sequence ()
  "Return a control sequence to disable XTerm mouse tracking.
The control sequence resets the modes set by
`xterm-mouse-tracking-enable-sequence'."
  (apply #'concat (nreverse (xterm-mouse--tracking-sequence ?l))))

(defconst xterm-mouse-tracking-disable-sequence
  "\e[?1006l\e[?1005l\e[?1003l\e[?1000l"
  "Reset the modes set by `xterm-mouse-tracking-enable-sequence'.")

(make-obsolete-variable
 'xterm-mouse-tracking-disable-sequence
 "use the function `xterm-mouse-tracking-disable-sequence' instead."
 "25.1")

(defun xterm-mouse--tracking-sequence (suffix)
  "Return a control sequence to enable or disable mouse tracking.
SUFFIX is the last character of each escape sequence (?h to
enable, ?l to disable)."
  (mapcar
   (lambda (code) (format "\e[?%d%c" code suffix))
   `(1000 1003 ,@(when xterm-mouse-utf-8 '(1005)) 1006)))

(defun turn-on-xterm-mouse-tracking-on-terminal (&optional terminal)
  "Enable xterm mouse tracking on TERMINAL."
  (when (and xterm-mouse-mode (eq t (terminal-live-p terminal))
	     ;; Avoid the initial terminal which is not a termcap device.
	     ;; FIXME: is there more elegant way to detect the initial
             ;; terminal?
	     (not (string= (terminal-name terminal) "initial_terminal")))
    (unless (terminal-parameter terminal 'xterm-mouse-mode)
      ;; Simulate selecting a terminal by selecting one of its frames
      ;; so that we can set the terminal-local `input-decode-map'.
      ;; Use the tty-top-frame to avoid accidentally making an invisible
      ;; child frame visible by selecting it (bug#79960).
      ;; The test for match mode is here because xt-mouse-tests run in
      ;; match mode, and there is no top-frame in that case.
      (with-selected-frame (if noninteractive
                               (car (frame-list))
                             (tty-top-frame terminal))
        (define-key input-decode-map "\e[M" 'xterm-mouse-translate)
        (define-key input-decode-map "\e[<" 'xterm-mouse-translate-extended))
      (let ((enable (xterm-mouse-tracking-enable-sequence))
            (disable (xterm-mouse-tracking-disable-sequence)))
        (condition-case err
            (send-string-to-terminal enable terminal)
          ;; FIXME: This should use a dedicated error signal.
          (error (if (equal (cadr err) "Terminal is currently suspended")
                     nil ; The sequence will be sent upon resume.
                   (signal (car err) (cdr err)))))
        (push enable (terminal-parameter nil 'tty-mode-set-strings))
        (push disable (terminal-parameter nil 'tty-mode-reset-strings))
        (set-terminal-parameter terminal 'xterm-mouse-mode t)
        (set-terminal-parameter terminal 'xterm-mouse-utf-8
                                xterm-mouse-utf-8)))))

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
    (condition-case err
        (send-string-to-terminal xterm-mouse-tracking-disable-sequence
                                 terminal)
      ;; FIXME: This should use a dedicated error signal.
      (error (if (equal (cadr err) "Terminal is currently suspended")
                 nil
               (signal (car err) (cdr err)))))
    (setf (terminal-parameter nil 'tty-mode-set-strings)
          (remq xterm-mouse-tracking-enable-sequence
                (terminal-parameter nil 'tty-mode-set-strings)))
    (setf (terminal-parameter nil 'tty-mode-reset-strings)
          (remq xterm-mouse-tracking-disable-sequence
                (terminal-parameter nil 'tty-mode-reset-strings)))
    (set-terminal-parameter terminal 'xterm-mouse-mode nil)))

(provide 'xt-mouse)

;;; xt-mouse.el ends here
