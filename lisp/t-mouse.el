;;; t-mouse.el --- mouse support within the text terminal

;; Authors: Alessandro Rubini and Ian T Zimmerman
;; Maintainer: Nick Roberts <nickrob@gnu.org>
;; Keywords: mouse gpm linux

;; Copyright (C) 1994, 1995, 1998, 2006, 2007 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This package provides access to mouse event as reported by the
;; gpm-Linux package.  It uses the program "mev" to get mouse events.
;; It tries to reproduce the functionality offered by Emacs under X.
;; The "gpm" server runs under Linux, so this package is rather
;; Linux-dependent.

;; Modified by Nick Roberts for Emacs 22.  In particular, the mode-line is
;; now position sensitive.

(defvar t-mouse-process nil
  "Embeds the process which passes mouse events to Emacs.
It is used by the program t-mouse.")

(defvar t-mouse-filter-accumulator ""
  "Accumulates input from the mouse reporting process.")

(defvar t-mouse-debug-buffer nil
  "Events normally posted to command queue are printed here in debug mode.
See `t-mouse-start-debug'.")

(defvar t-mouse-current-xy '(0 . 0)
  "Stores the last mouse position t-mouse has been told about.")

(defvar t-mouse-drag-start nil
  "Whenever a drag starts in a special part of a window
\(not the text), the `translated' starting coordinates including the
window and part involved are saved here.  This is necessary lest they
get re-translated when the button goes up, at which time window
configuration may have changed.")

(defvar t-mouse-prev-set-selection-function 'x-set-selection)
(defvar t-mouse-prev-get-selection-function 'x-get-selection)

(defvar t-mouse-swap-alt-keys nil
  "When set, Emacs will handle mouse events with the right Alt
\(a.k.a.  Alt-Ger) modifier, not with the regular left Alt modifier.
Useful for people who play strange games with their keyboard tables.")

(defvar t-mouse-fix-21 nil
  "Enable brain-dead chords for 2 button mice.")


;;; Code:

;; get the number of the current virtual console

(defun t-mouse-tty ()
  "Return number of virtual terminal Emacs is running on, as a string.
For example, \"2\" for /dev/tty2."
  (with-temp-buffer
    (call-process "ps" nil t nil "h" (format "%s" (emacs-pid)))
    (goto-char (point-min))
    (if (or
	 ;; Many versions of "ps", all different....
	 (re-search-forward " +tty\\(.?[0-9a-f]\\)" nil t)
	 (re-search-forward "p \\([0-9a-f]\\)" nil t)
	 (re-search-forward "v0\\([0-9a-f]\\)" nil t)
	 (re-search-forward "[0-9]+ +\\([0-9]+\\)" nil t)
	 (re-search-forward "[\\t ]*[0-9]+[\\t ]+\\([0-9]+\\)" nil t)
	 (re-search-forward " +vc/\\(.?[0-9a-f]\\)" nil t)
	 (re-search-forward " +pts/\\(.?[0-9a-f]\\)" nil t))
	(buffer-substring (match-beginning 1) (match-end 1)))))


;; due to a horrible kludge in Emacs' keymap handler
;; (read_key_sequence) mouse clicks on funny parts of windows generate
;; TWO events, the first being a dummy of the sort '(mode-line).
;; That's why Per Abrahamsen's code in xt-mouse.el doesn't work for
;; the modeline, for instance.

;; now get this:  the Emacs C code that generates these fake events
;; depends on certain things done by the very lowest level input
;; handlers; namely the symbols for the events (for instance
;; 'C-S-double-mouse-2) must have an 'event-kind property, set to
;; 'mouse-click.  Since events from unread-command-events do not pass
;; through the low level handlers, they don't get this property unless
;; I set it myself.  I imagine this has caused innumerable attempts by
;; hackers to do things similar to t-mouse to lose.

;; The next page of code is devoted to fixing this ugly problem.

;; WOW! a fully general powerset generator
;; (C) Ian Zimmerman Mon Mar 23 12:00:16 PST 1998 :-)
(defun t-mouse-powerset (l)
  (if (null l) '(nil)
    (let ((l1 (t-mouse-powerset (cdr l)))
          (first (nth 0 l)))
      (append
       (mapcar (function (lambda (l) (cons first l))) l1) l1))))

;; and a slightly less general cartesian product
(defun t-mouse-cartesian (l1 l2)
  (if (null l1) l2
    (append (mapcar (function (lambda (x) (append (nth 0 l1) x))) l2)
            (t-mouse-cartesian (cdr l1) l2))))

(let* ((modifier-sets (t-mouse-powerset '(control meta shift)))
       (typed-sets (t-mouse-cartesian '((down) (drag))
                                      '((mouse-1) (mouse-2) (mouse-3))))
       (multipled-sets (t-mouse-cartesian '((double) (triple)) typed-sets))
       (all-sets (t-mouse-cartesian modifier-sets multipled-sets)))
  (while all-sets
    (let ((event-sym (event-convert-list (nth 0 all-sets))))
      (if (not (get event-sym 'event-kind))
          (put event-sym 'event-kind 'mouse-click)))
    (setq all-sets (cdr all-sets))))

(defun t-mouse-make-event-element (x-dot-y-avec-time)
  (let* ((x-dot-y (nth 0 x-dot-y-avec-time))
	 (time (nth 1 x-dot-y-avec-time))
         (x (car x-dot-y))
         (y (cdr x-dot-y))
         (w (window-at x y))
         (ltrb (window-edges w))
         (left (nth 0 ltrb))
         (top (nth 1 ltrb))
	 (event (if w
		    (posn-at-x-y (- x left) (- y top) w t)
		  (append (list nil 'menu-bar)
			  (nthcdr 2 (posn-at-x-y x y))))))
    (setcar (nthcdr 3 event) time)
    event))

;;; This fun is partly Copyright (C) 1994 Per Abrahamsen <abraham@iesd.auc.dk>
(defun t-mouse-make-event ()
  "Make a Lisp style event from the contents of mouse input accumulator.
Also trim the accumulator by all the data used to build the event."
  (let (ob (ob-pos (condition-case nil
		       (progn
			 ;; this test is just needed for Fedora Core 3
			 (if (string-match "STILL RUNNING_1\n"
					   t-mouse-filter-accumulator)
			     (setq t-mouse-filter-accumulator
				   (substring
				    t-mouse-filter-accumulator (match-end 0))))
			 (read-from-string t-mouse-filter-accumulator))
                     (error nil))))
    ;; this test is just needed for Fedora Core 3
    (if (or (eq (car ob-pos) 'STILL) (eq (car ob-pos) '***) (not ob-pos))
	nil
      (setq ob (car ob-pos))
      (if (string-match "mev:$" (prin1-to-string ob))
	  (error "Can't open mouse connection"))
      (setq t-mouse-filter-accumulator
            (substring t-mouse-filter-accumulator (cdr ob-pos)))

      ;;now the real work

      (let ((event-type (nth 0 ob))
            (current-xy-avec-time (nth 1 ob))
            (type-switch (length ob)))
 	(if t-mouse-fix-21
            (let
                ;;Acquire the event's symbol's name.
                ((event-name-string (symbol-name event-type))
                 end-of-root-event-name
                 new-event-name-string)

              (if (string-match "-\\(21\\|\\12\\)$" event-name-string)

                  ;;Transform the name to what it should have been.
                  (progn
                    (setq end-of-root-event-name (match-beginning 0))
                    (setq new-event-name-string
                          (concat (substring
                                   event-name-string 0
                                   end-of-root-event-name) "-3"))

                    ;;Change the event to the symbol that corresponds to the
                    ;;name we made. The proper symbol already exists.
                    (setq event-type
                          (intern new-event-name-string))))))

        ;;store current position for mouse-position

        (setq t-mouse-current-xy (nth 0 current-xy-avec-time))

        ;;events have many types but fortunately they differ in length

        (cond
         ((= type-switch 4)             ;must be drag
          (let ((count (nth 2 ob))
                (start-element
                 (or t-mouse-drag-start
                     (t-mouse-make-event-element (nth 3 ob))))
                (end-element
                 (t-mouse-make-event-element current-xy-avec-time)))
            (setq t-mouse-drag-start nil)
            (list event-type start-element end-element count)))
         ((= type-switch 3)             ;down or up
          (let ((count (nth 2 ob))
                (element
                 (t-mouse-make-event-element current-xy-avec-time)))
            (if (and (not t-mouse-drag-start)
                     (symbolp (nth 1 element)))
                ;; OUCH! GOTCHA! emacs uses setc[ad]r on these!
                (setq t-mouse-drag-start (copy-sequence element))
              (setq t-mouse-drag-start nil))
            (list event-type element count)))
         ((= type-switch 2)             ;movement
          (list (if (eq 'vertical-scroll-bar
                        (nth 1 t-mouse-drag-start)) 'scroll-bar-movement
                  'mouse-movement)
                (t-mouse-make-event-element current-xy-avec-time))))))))

(defun t-mouse-process-filter (proc string)
  (setq t-mouse-filter-accumulator
        (concat t-mouse-filter-accumulator string))
  (let ((event (t-mouse-make-event)))
    (while event
      (if (or track-mouse
              (not (eq 'mouse-movement (event-basic-type event))))
          (setq unread-command-events
                (nconc unread-command-events (list event))))
      (if t-mouse-debug-buffer
          (print unread-command-events t-mouse-debug-buffer))
      (setq event (t-mouse-make-event)))))

(defun t-mouse-mouse-position-function (pos)
  "Return the t-mouse-position unless running with a window system.
The (secret) scrollbar interface is not implemented yet."
  (setcdr pos t-mouse-current-xy)
  pos)

;; It should be possible to just send SIGTSTP to the inferior with
;; stop-process.  That doesn't work; mev receives the signal fine but
;; is not really stopped: instead it returns from
;; kill(getpid(), SIGTSTP) immediately.  I don't understand what's up
;; itz Tue Mar 24 14:27:38 PST 1998.

(add-hook 'suspend-hook
          (function (lambda ()
                      (and t-mouse-process
                           ;(stop-process t-mouse-process)
                           (process-send-string
                            t-mouse-process "push -enone -dall -Mnone\n")))))

(add-hook 'suspend-resume-hook
          (function (lambda ()
                      (and t-mouse-process
                           ;(continue-process t-mouse-process)
                           (process-send-string t-mouse-process "pop\n")))))

;;;###autoload
(define-minor-mode t-mouse-mode
  "Toggle t-mouse mode to use the mouse in Linux consoles.
With prefix arg, turn t-mouse mode on iff arg is positive.

This allows the use of the mouse when operating on a Linux console, in the
same way as you can use the mouse under X11.
It requires the `mev' program, part of the `gpm' utilities."
  nil " Mouse" nil :global t
  (if t-mouse-mode
      ;; Turn it on
      (unless window-system
        ;; Starts getting a stream of mouse events from an asynchronous process.
        ;; Only works if Emacs is running on a virtual terminal without a window system.
	(progn
	 (setq mouse-position-function #'t-mouse-mouse-position-function)
	 (let ((tty (t-mouse-tty))
	       (process-connection-type t))
	   (if (not (stringp tty))
	       (error "Cannot find a virtual terminal"))
	   (setq t-mouse-process
		 (start-process "t-mouse" nil
				"mev" "-i" "-E" "-C" tty
				(if t-mouse-swap-alt-keys
				    "-M-leftAlt" "-M-rightAlt")
				"-e-move"
				"-dall" "-d-hard"
				"-f")))
	 (setq t-mouse-filter-accumulator "")
	 (set-process-filter t-mouse-process 't-mouse-process-filter)
	 (set-process-query-on-exit-flag t-mouse-process nil)))
    ;; Turn it off
    (setq mouse-position-function nil)
    (delete-process t-mouse-process)
    (setq t-mouse-process nil)))

(provide 't-mouse)

;; arch-tag: a63163b3-bfbe-4eb2-ab4f-201cd164b05d
;;; t-mouse.el ends here
