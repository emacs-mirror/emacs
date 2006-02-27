;;; t-mouse.el --- mouse support within the text terminal

;;; Copyright (C) 1994,1995 Alessandro Rubini <rubini@linux.it>
;;;               parts are by Ian T Zimmermann <itz@rahul.net>, 1995,1998

;; Maintainer: gpm mailing list: gpm@prosa.it
;; Keywords: mouse gpm linux

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; This package provides access to mouse event as reported by the
;; gpm-Linux package. It uses the program "mev" to get mouse events.
;; It tries to reproduce the functionality offered by emacs under X.
;; The "gpm" server runs under Linux, so this package is rather
;; Linux-dependent.

;; Developed for GNU Emacs 19.34, likely won't work with many others
;; too much internals dependent cruft here.


(require 'advice)

(defvar t-mouse-process nil 
  "Embeds the process which passes mouse events to emacs.
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
(not the text), the `translated' starting coordinates including the
window and part involved are saved here.  This is necessary lest they
get re-translated when the button goes up, at which time window
configuration may have changed.")

(defvar t-mouse-prev-set-selection-function 'x-set-selection)
(defvar t-mouse-prev-get-selection-function 'x-get-selection)

(defvar t-mouse-swap-alt-keys nil
  "When set, Emacs will handle mouse events with the right Alt
(a.k.a. Alt-Ger) modifier, not with the regular left Alt modifier.
Useful for people who play strange games with their keyboard tables.")

(defvar t-mouse-fix-21 nil 
  "Enable brain-dead chords for 2 button mice.")


;;; Code:

;; get the number of the current virtual console

(defun t-mouse-tty ()
  "Returns number of virtual terminal Emacs is running on, as a string.
For example, \"2\" for /dev/tty2."
  (let ((buffer (generate-new-buffer "*t-mouse*")))
    (call-process "ps" nil buffer nil "h" (format "%s" (emacs-pid)))
    (prog1 (save-excursion
             (set-buffer buffer)
             (goto-char (point-min))
             (if (or
		  ;; Many versions of "ps", all different....
		  (re-search-forward " +tty\\(.?[0-9a-f]\\)" nil t)
		  (re-search-forward "p \\([0-9a-f]\\)" nil t)
		  (re-search-forward "v0\\([0-9a-f]\\)" nil t)
		  (re-search-forward "[0-9]+ +\\([0-9]+\\)" nil t)
		  (re-search-forward "[\\t ]*[0-9]+[\\t ]+\\([0-9]+\\)" nil t))
                 (buffer-substring (match-beginning 1) (match-end 1))))
      (kill-buffer buffer))))


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


;;; This fun is partly Copyright (C) 1994 Per Abrahamsen <abraham@iesd.auc.dk>
;; This is basically a feeble attempt to mimic what the c function
;; buffer_posn_from_coords in dispnew.c does.  I wish that function
;; were exported to Lisp.

(defun t-mouse-lispy-buffer-posn-from-coords (w col line)
  "Return buffer position of character at COL and LINE within window W.
COL and LINE are glyph coordinates, relative to W topleft corner."
  (save-window-excursion
    (select-window w)
    (save-excursion
      (move-to-window-line line)
      (move-to-column (+ col (current-column)
                         (if (not (window-minibuffer-p w)) 0
                           (- (minibuffer-prompt-width)))
                         (max 0 (1- (window-hscroll)))))
      (point))))

;; compute one element of the form (WINDOW BUFFERPOS (COL . ROW) TIMESTAMP)

(defun t-mouse-make-event-element (x-dot-y-avec-time)
  (let* ((x-dot-y (nth 0 x-dot-y-avec-time))
         (x (car x-dot-y))
         (y (cdr x-dot-y))
         (timestamp (nth 1 x-dot-y-avec-time))
         (w (window-at x y))
         (left-top-right-bottom (window-edges w))
         (left (nth 0 left-top-right-bottom))
         (top (nth 1 left-top-right-bottom))
         (right (nth 2 left-top-right-bottom))
         (bottom (nth 3 left-top-right-bottom))
         (coords-or-part (coordinates-in-window-p x-dot-y w)))
    (cond
     ((consp coords-or-part)
      (let ((wx (car coords-or-part)) (wy (cdr coords-or-part)))
        (if (< wx (- right left 1))
            (list w 
                  (t-mouse-lispy-buffer-posn-from-coords w wx wy)
                  coords-or-part timestamp)
          (list w 'vertical-scroll-bar
                (cons (1+ wy) (- bottom top)) timestamp))))
     ((eq coords-or-part 'mode-line)
      (list w 'mode-line (cons (- x left) 0) timestamp))
     ((eq coords-or-part 'vertical-line)
      (list w 'vertical-line (cons 0 (- y top)) timestamp)))))     

;;; This fun is partly Copyright (C) 1994 Per Abrahamsen <abraham@iesd.auc.dk>

(defun t-mouse-make-event ()
  "Makes a Lisp style event from the contents of mouse input accumulator.
Also trims the accumulator by all the data used to build the event."
  (let (ob (ob-pos (condition-case nil
                       (read-from-string t-mouse-filter-accumulator)
                     (error nil))))
    (if (not ob-pos) nil
      (setq ob (car ob-pos))
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
         ;;sink all events on the stupid text mode menubar.
         ((and menu-bar-mode (eq 0 (cdr t-mouse-current-xy))) nil)
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


;; this overrides a C function which stupidly assumes (no X => no mouse)
(defadvice mouse-position (around t-mouse-mouse-position activate)
  "Return the t-mouse-position unless running with a window system.
The (secret) scrollbar interface is not implemented yet."
  (if (not window-system)
      (setq ad-return-value
            (cons (selected-frame) t-mouse-current-xy))
    ad-do-it))

(setq mouse-sel-set-selection-function
      (function (lambda (type value)
                  (if (not window-system)
                      (if (eq 'PRIMARY type) (kill-new value))
                    (funcall t-mouse-prev-set-selection-function
                             type value)))))

(setq mouse-sel-get-selection-function
      (function (lambda (type)
                  (if (not window-system)
                      (if (eq 'PRIMARY type)
                          (current-kill 0) "")
                    (funcall t-mouse-prev-get-selection-function type)))))

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


;;; User commands

(defun t-mouse-stop ()
  "Stop getting mouse events from an asynchronous process."
  (interactive)
  (delete-process t-mouse-process)
  (setq t-mouse-process nil))

(defun t-mouse-run ()
  "Starts getting a stream of mouse events from an asynchronous process.
Only works if Emacs is running on a virtual terminal without a window system.
Returns the newly created asynchronous process."
  (interactive)
  (let ((tty (t-mouse-tty))
        (process-connection-type t))
    (if (or window-system (not (stringp tty)))
        (error "Run t-mouse on a virtual terminal without a window system"))
    (setq t-mouse-process 
          (start-process "t-mouse" nil
                         "mev" "-i" "-E" "-C" tty
                         (if t-mouse-swap-alt-keys
                             "-M-leftAlt" "-M-rightAlt")
                         "-e-move" "-dall" "-d-hard"
                         "-f")))
  (setq t-mouse-filter-accumulator "")
  (set-process-filter t-mouse-process 't-mouse-process-filter)
  (process-kill-without-query t-mouse-process)
  t-mouse-process)

(provide 't-mouse)

;;; t-mouse.el ends here
