;;; zone.el --- idle display hacks  -*- lexical-binding: t -*-

;; Copyright (C) 2000-2026 Free Software Foundation, Inc.

;; Author: Victor Zandy <zandy@cs.wisc.edu>
;; Maintainer: emacs-devel@gnu.org
;; Keywords: games
;; Created: June 6, 1998

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

;; Don't zone out in front of Emacs!  Try M-x zone.
;; If it eventually irritates you, try M-x zone-leave-me-alone.

;; Bored by the zone pyrotechnics?  Write your own!  Add it to
;; `zone-programs'.  See `zone-call' for higher-ordered zoning.

;; THANKS: Christopher Mayer, Scott Flinchbaugh,
;;         Rachel Kalmar, Max Froumentin, Juri Linkov,
;;         Luigi Panzeri, John Paul Wallington.

;;; Code:

(defgroup zone nil
  "Zone related settings."
  :prefix "zone-"
  :group 'play)

(defconst zone-buffer-name "*zone*"
  "Name of the zone buffer that holds zoned text.")

(defvar zone-timer nil
  "The timer we use to decide when to zone out, or nil if none.")

(defvar zone-timeout nil
  "Seconds to timeout the zoning.
If nil, don't interrupt for about 1^26 seconds.")

;; Vector of functions that zone out.  `zone' will execute one of
;; these functions, randomly chosen.  The chosen function is invoked
;; in the *zone* buffer, which contains the text of the selected
;; window.  If the function loops, it *must* periodically check and
;; halt if `input-pending-p' is t (because quitting is disabled when
;; Emacs idle timers are run).
(defvar zone-programs [
                       zone-pgm-jitter
                       zone-pgm-putz-with-case
                       zone-pgm-dissolve
                       ;; zone-pgm-explode
                       zone-pgm-whack-chars
                       zone-pgm-rotate
                       zone-pgm-rotate-LR-lockstep
                       zone-pgm-rotate-RL-lockstep
                       zone-pgm-rotate-LR-variable
                       zone-pgm-rotate-RL-variable
                       zone-pgm-drip
                       zone-pgm-drip-fretfully
                       zone-pgm-five-oclock-swan-dive
                       zone-pgm-martini-swan-dive
                       zone-pgm-rat-race
                       zone-pgm-paragraph-spaz
                       zone-pgm-stress
                       zone-pgm-stress-destress
                       zone-pgm-random-life
                       ])

(defun zone-add-program (pgm)
  "Add a zone program PGM to `zone-programs'."
  (unless (seq-contains-p zone-programs pgm #'eq)
    (setq zone-programs (vconcat zone-programs (list pgm)))))

(defun zone-remove-program (pgm)
  "Remove a zone program PGM from `zone-programs'.
If PGM is a symbol, remove it from the `zone-programs'; if it is a
string, assume it is a regular expression that will remove programs
whose name matches the pattern."
  (setq zone-programs
        (vector (seq-remove
                 (lambda (v)
                   (cond
                    ((symbolp pgm)  (eq pgm v))
                    ((stringp pgm)  (string-match-p pgm (symbol-name v)))
                    (t              nil)))
                 zone-programs))))

(defmacro zone-orig (&rest body)
  "Perform BODY in the original source buffer of the zone buffer."
  `(with-current-buffer (get 'zone 'orig-buffer)
     ,@body))

(defmacro zone-hiding-mode-line (&rest body)
  "Perform BODY without a window mode line."
  ;; This formerly worked by temporarily altering face `mode-line',
  ;; which did not even work right, it seems.
  `(let (mode-line-format)
     ,@body))

(defun zone-call (program &optional timeout)
  "Call PROGRAM in a zoned way.
If PROGRAM is a function, call it, interrupting after the amount
 of time in seconds specified by optional arg TIMEOUT, or `zone-timeout'
 if unspecified, q.v.
PROGRAM can also be a list of elements, which are interpreted like so:
If the element is a function or a list of a function and a number,
 apply `zone-call' recursively."
  (cond ((functionp program)
         (with-timeout ((or timeout zone-timeout (ash 1 26)))
           (funcall program)))
        ((listp program)
         (mapcar (lambda (elem)
                   (cond ((functionp elem) (zone-call elem))
                         ((and (listp elem)
                               (functionp (car elem))
                               (numberp (cadr elem)))
                          (apply 'zone-call elem))
                         (t (error "Bad `zone-call' elem: %S" elem))))
                 program))))

;;;; Customization flags to control what is zoned

(defcustom zone-all-frames nil
  "When non-nil, zone in all open frames.
Displays the `*zone*' buffer in all windows in all frames."
  :type 'boolean)

(defcustom zone-all-windows-in-frame nil
  "When non-nil, zone in all windows in the current frame."
  :type 'boolean)

(defcustom zone-delete-other-windows nil
  "When non-nil, make the frame a single window before zoning.
The original windows and their content will be restored when zoning
completes."
  :type 'boolean)

;;;; Hooks to detect the start and finish of zone activity

(defvar zone-time-elapsed-while-zoning nil
  "In the `zone-finish-hook', will report the time spent zoning.")

(defvar zone-start-hook nil
  "Hook at the start of zoning.")

(defvar zone-finish-hook nil
  "Hook at the finish of Zoning.
When this is invoked, `zone-time-elapsed-while-zoning' will be properly set.")

;;;; Save frame configuration so it can be restored when we finish zoning

(defvar zone-frame-configuration-alist nil
  "An Alist of frames and their cursor and window configuration.

Before zone starts using a frame, it saves the configuration before it
touches anything.  At the end of zoning, the frame configuration is
restored.

The Alist key is the frame object, then value is the cons cell
containing the window configuration and cursor type.")

(defun zone--save-frame-configuration (frm &optional reset)
  "Save the frame FRM's configuration.

When RESET is non-nil, the `zone-frame-configuration-alist' will contain
this frame only, otherwise the frame's configuration will be appended to
the Alist."
  (when reset
    (setq zone-frame-configuration-alist nil))
  (when (frame-visible-p frm)
    (push (cons frm
                (cons
                 (current-window-configuration frm)
                 (frame-parameter frm 'cursor-type)))
          zone-frame-configuration-alist)))

(defun zone--restore-frame-configuration (frm)
  "Restore the frame FRM's configuration from the Alist."
  (when-let* ((config (alist-get frm zone-frame-configuration-alist)))
    (with-selected-frame frm
      (set-window-configuration (car config))
      (modify-frame-parameters frm (list (cons 'cursor-type (cdr config)))))))

(defun zone--restore-all-frame-configurations ()
  "Restore all of the saved frame configurations."
  (mapc #'zone--restore-frame-configuration
        (mapcar #'car zone-frame-configuration-alist))
  (setq zone-frame-configuration-alist nil)
  (when (get-buffer zone-buffer-name)
    (kill-buffer zone-buffer-name)))

;;;; Here we zone...

;;;###autoload
(defun zone (&optional pgm)
  "Zone out, completely.
With a prefix argument the user is prompted for a program to run.
When called from Lisp the optional argument PGM can be used to
run a specific program.  The program must be a member of
`zone-programs'."
  (interactive
   (and current-prefix-arg
        (let ((choice (completing-read
                       "Program: "
                       (mapcar
                        (lambda (prog)
                          (substring (symbol-name prog) 9))
                        zone-programs)
                       nil t)))
          (list (intern (concat "zone-pgm-" choice))))))
  (unless pgm
    (setq pgm (aref zone-programs (random (length zone-programs)))))
  (run-hooks 'zone-start-hook)
  (let* ((start-time (current-time))
         (zone-again nil)
         (src-winbuf (zone--choose-window-and-buffer))
         (src-win  (car src-winbuf))
         (src-buf  (cdr src-winbuf))
         (src-frm  (window-frame src-win)))
    (setq zone-frame-configuration-alist nil)
    (unwind-protect
        (progn
          (zone--save-frame-configuration src-frm)
          (zone--build-zone-buffer src-win src-buf)
          (zone--prepare-frames src-frm)
          (condition-case zone-err
              (progn
                (message "Zoning... (%s)" pgm)
                (garbage-collect)
                ;; If some input is pending, zone says "sorry", which
                ;; isn't nice; this might happen e.g. when they invoke the
                ;; game by clicking the menu bar.  So discard any pending
                ;; input before zoning out.
                (if (input-pending-p)
                    (discard-input))
                (zone-call pgm)
                (message "Zoning...sorry"))

            (error
             (message "%s error: %S" (or pgm 'zone) zone-err)
             (zone--apologize-for-failing pgm)
             (setq zone-again t))

            (quit
             (ding)
             (message "Zoning...sorry"))))
      (zone--restore-all-frame-configurations))
    (when (and zone-again
               (not (input-pending-p)))
      (zone))
    (setq zone-time-elapsed-while-zoning (time-since start-time)))
    (run-hooks 'zone-finish-hook))

;;;; Identify the current window and the best buffer to use as zone source

(defun zone--buffer-empty-p (buffer)
  "Is BUFFER empty?"
  (zerop (buffer-size buffer)))

(defun zone--buffer-encrypted-p (buffer)
  "Is BUFFER encrypted with `epa'?"
  (require 'epa-hook)
  (when-let* ((name (buffer-file-name buffer)))
    (epa-file-name-p name)))

(defun zone--choose-window-and-buffer ()
  "Choose the current window and an acceptable buffer.
Check each buffer to determine whether it is suitable for zoning,
starting with the buffer in the current window.  For example, encrypted
files, certain source modules, or command sessions may be inappropriate
if they might expose privileged or secret information."
  (cons
   (selected-window)
   (or (seq-find #'zone--buffer-zoneable-p
                 (buffer-list (selected-frame)))
       ;; only create *scratch* if we need one as fall back
       (get-scratch-buffer-create))))

(defvar zone-ignored-buffers
  '( "\\`\s"                    ;; Hidden buffers
     zone--buffer-empty-p       ;; Empty buffers (not very interesting)
     special-mode               ;; Special/internal buffers
     image-mode                 ;; image buffers
     authinfo-mode              ;; encrypted buffers
     zone--buffer-encrypted-p
     "\\`\\*scratch\\*\\'"      ;; zone will fallback to scratch , but
                                ;; ignore it in the first pass
     )
  "Buffers that satisfy any of these rules are ignored as a zone buffer.
Each entry in the list must be one of the following:

+ MODE: all derived modes of the MODE are considered unacceptable,
+ REGEXP: a buffer name that matches the REGEXP is not acceptable, and
+ PRED: a buffer that satisfies the PRED, such that it returns a non-nil
  value when invoked as a function with the buffer supplied as a
  parameter, is considered not acceptable.")

(defun zone--buffer-zoneable-p (buffer)
  "Is BUFFER suitable for zoning?
For example, buffers containing passwords, critical source files, and
command line transcripts might not be appropriate for a zone buffer.

To be acceptable, the buffer must NOT satisfy any of the entries on
`zone-ignored-buffers'."
  (not (any
        (lambda (ign)
          (cond
           ((stringp ign)
            (string-match-p ign (buffer-name buffer)))
           ((and (symbolp ign)
                 (string-suffix-p "-mode" (symbol-name ign)))
            (with-current-buffer buffer
              (derived-mode-p ign)))
           ((functionp ign)
            (funcall ign buffer))))
        zone-ignored-buffers)))

;;;; Prepare the *zone* buffer with a copy of the source buffer

(defun zone--build-zone-buffer (win buf)
  "Construct the *zone* buffer in window WIN, based on buffer BUF.
Remove other windows if `zone-delete-other-windows' is non-nil.  The
selected buffer is then placed in the window.  If only a portion of the
buffer is visible, try to recenter it to expose more."
  ;; Make us single window if desired
  (when zone-delete-other-windows
    (delete-other-windows win))
  ;; Switch in the source buffer into the window
  (unless (eq (current-buffer) buf)
    (set-window-buffer win buf nil))
  ;; Try to scroll the buffer into the window
  (unless (< (window-end) (point-max))
    (let ((scroll-margin 0))
      (recenter -1)))
  (redisplay)
  ;; Create the zone buffer and populate it
  (let* ((win-end (window-end win t))
         (win-beg (window-start))
         (win-pt  (window-point))
         (new-pt  (1+ (- win-pt win-beg)))
         (win-ht  (line-pixel-height)))
    (with-current-buffer (get-buffer-create zone-buffer-name)
      (put 'zone 'orig-buffer buf)
      (erase-buffer)
      (setq-local mode-name "Zone"
                  buffer-undo-list t
                  truncate-lines t
                  scroll-margin 0
                  scroll-conservatively 1000
                  scroll-up-aggressively 0
                  scroll-down-aggressively 0
                  show-trailing-whitespace nil
                  tab-width (buffer-local-value 'tab-width buf)
                  line-spacing (buffer-local-value 'line-spacing buf))
      ;; Grab the visible portion of the source buffer
      (insert-buffer-substring buf win-beg win-end)
      ;; Remove read-only property so zone can play with all of it
      (let ((inhibit-read-only t)
            (beg (point-min))
            (end (point-max)))
        (remove-text-properties beg end '(read-only nil))
        ;; Adjust line height with the settings from the original buffer
        (add-text-properties beg end `(line-height ,win-ht))
        ;; Get rid of tab characters and position the window
        (untabify beg end)))
    ;; Move the zone buffer to the window
    (set-window-buffer win zone-buffer-name nil)
    ;; Position the zone buffer in the window
    ;;   Position point and then fix the top.
    ;;   These with scroll settings above fix
    ;;   the content boundaries in the window
    (set-window-point win new-pt)
    (set-window-start win (point-min))
    (redisplay)))

;;;; Configure frames and windows based on customization flags

(defun zone--prepare-frames (prim-frm)
  "Reorganize frames and their windows to show the zone out.
This is based on the settings of three customization flags:
+ `zone-all-frames',
+ `zone-all-windows-in-frame', and
+ `zone-delete-other-windows'

These have been partially performed on the primary frame PRIM-FRM.
Based on the settings, the other frames may be similarly adjusted."
  (let* ((z (get-buffer zone-buffer-name))
         (prim-win (frame-selected-window prim-frm))
         (f prim-frm)
         (no-cursor '((cursor-type . (bar . 0))))
         w1)
    ;; Handle the primary frame
    (select-frame f t)
    (setq w1 (frame-selected-window f))
    ;; Put zone in current or every window
    (dolist (w (if zone-all-windows-in-frame
                   (window-list f 'no-minibuf w1)
                 (list w1)))
      (set-window-buffer w z nil))
    (modify-frame-parameters f no-cursor)
    ;; Handle the remaining frames
    (dolist (f (visible-frame-list))
      (unless (eq f prim-frm)
        (select-frame f t)
        (setq w1 (frame-selected-window f))
        ;; Single window frame
        (when zone-delete-other-windows
          (delete-other-windows))
        ;; Put zone in current or every window
        (dolist (w (if zone-all-windows-in-frame
                       (window-list f 'no-minibuf w1)
                     (list w1)))
          (set-window-buffer w z nil))
        (modify-frame-parameters f no-cursor)
        (set-frame-selected-window f w1 t)))
    (select-frame prim-frm)
    (set-frame-selected-window prim-frm prim-win t)))

;;;; If the zone program fails, apologize and try again

(defvar zone-apologize-seconds 60 ;; 1 minute
  "Number of seconds to apologize for failing.
This value is broken into 6 second cycles to allow for two messages
displayed for 3 seconds each in every cycle.")

(defun zone--apologize-for-failing (pgm)
  "Apologize for PGM failing for a minute."
  (let ((cycle 0)
        (n-cycles (floor zone-apologize-seconds 6)))
    (while (and (not (input-pending-p))
                (<= (incf cycle) n-cycles))
      (let ((message-log-max (= cycle 1))) ;; Log message first time only
        (message "We were zoning when we wrote %s..." pgm)
        (sit-for 3)
        (message "...here's hoping we didn't hose your buffer!")
        (sit-for 3)))))

;;;; Zone when idle, or not.

(defun zone-when-idle (secs)
  "Zone out when Emacs has been idle for SECS seconds."
  (interactive "nHow long before I start zoning (seconds): ")
  (if (timerp zone-timer)
      (cancel-timer zone-timer))
  (setq zone-timer nil)
  (or (<= secs 0)
      (setq zone-timer (run-with-idle-timer secs t 'zone))))

(defun zone-leave-me-alone ()
  "Don't zone out when Emacs is idle."
  (interactive)
  (if (timerp zone-timer)
      (cancel-timer zone-timer))
  (setq zone-timer nil)
  (message "I won't zone out any more"))


;;;; jittering

(defun zone-shift-up ()
  (let* ((b (point))
         (e (progn (forward-line 1) (point)))
         (s (buffer-substring b e)))
    (delete-region b e)
    (goto-char (point-max))
    (insert s)))

(defun zone-shift-down ()
  (goto-char (point-max))
  (let* ((b (point))
         (e (progn (forward-line -1) (point)))
         (s (buffer-substring b e)))
    (delete-region b e)
    (goto-char (point-min))
    (insert s)))

(defun zone-shift-left ()
  (let (s)
    (while (not (eobp))
      (unless (eolp)
        (setq s (buffer-substring (point) (1+ (point))))
        (delete-char 1)
        (end-of-line)
        (insert s))
      (ignore-errors (forward-char 1)))))

(defun zone-shift-right ()
  (goto-char (point-max))
  (let (s)
    (while (not (bobp))
      (unless (bolp)
        (setq s (buffer-substring (1- (point)) (point)))
        (delete-char -1)
        (beginning-of-line)
        (insert s))
      (end-of-line 0))))

(defun zone-pgm-jitter ()
  (let ((ops [
              zone-shift-left
              zone-shift-right
              zone-shift-down
              zone-shift-up
              ]))
    (goto-char (point-min))
    (while (not (input-pending-p))
      (funcall (elt ops (random (length ops))))
      (goto-char (point-min))
      (sit-for 0.01))))


;;;; whacking chars

(defun zone-pgm-whack-chars ()
  (let ((tbl (copy-sequence (get 'zone-pgm-whack-chars 'wc-tbl))))
    (while (not (input-pending-p))
      (let ((i 48))
        (while (< i 122)
          (aset tbl i (+ 48 (random (- 123 48))))
          (setq i (1+ i)))
        (translate-region (point-min) (point-max) tbl)
        (sit-for 0.002)))))

(put 'zone-pgm-whack-chars 'wc-tbl
     (let ((tbl (make-string 128 ?x))
           (i 0))
       (while (< i 128)
         (aset tbl i i)
         (setq i (1+ i)))
       tbl))

;;;; dissolving

(defun zone-remove-text ()
  (let ((working t))
    (while working
      (setq working nil)
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (if (looking-at "[^(){}\n\t ]")
              (let ((n (random 5)))
                (if (not (= n 0))
                    (progn
                      (setq working t)
                      (forward-char 1))
                  (delete-char 1)
                  (insert " ")))
            (forward-char 1))))
      (sit-for 0.002))))

(defun zone-pgm-dissolve ()
  (zone-remove-text)
  (zone-pgm-jitter))


;;;; exploding

(defun zone-exploding-remove ()
  (let ((i 0))
    (while (< i 5)
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (if (looking-at "[^*\n\t ]")
              (let ((n (random 5)))
                (if (not (= n 0))
                    (forward-char 1))
                (insert " ")))
          (forward-char 1)))
      (setq i (1+ i))
      (sit-for 0.002)))
  (zone-pgm-jitter))

(defun zone-pgm-explode ()
  (zone-exploding-remove)
  (zone-pgm-jitter))


;;;; putzing with case

;; Faster than `zone-pgm-putz-with-case', but not as good: all
;; instances of the same letter have the same case, which produces a
;; less interesting effect than you might imagine.
(defun zone-pgm-2nd-putz-with-case ()
  (let ((tbl (make-string 128 ?x))
        (i 0))
    (while (< i 128)
      (aset tbl i i)
      (setq i (1+ i)))
    (while (not (input-pending-p))
      (setq i ?a)
      (while (<= i ?z)
        (aset tbl i
              (if (zerop (random 5))
                  (upcase i)
                (downcase i)))
        (setq i (+ i (1+ (random 5)))))
      (setq i ?A)
      (while (<= i ?z)
        (aset tbl i
              (if (zerop (random 5))
                  (downcase i)
                (upcase i)))
        (setq i (+ i (1+ (random 5)))))
      (translate-region (point-min) (point-max) tbl)
      (sit-for 0.002))))

(defun zone-pgm-putz-with-case ()
  (goto-char (point-min))
  (while (not (input-pending-p))
    (let ((np (+ 2 (random 5)))
          (pm (point-max)))
      (while (< np pm)
        (funcall (if (zerop (random 2)) 'upcase-region
                   'downcase-region) (1- np) np)
        (setq np (+ np (1+ (random 5))))))
    (goto-char (point-min))
    (sit-for 0.002)))


;;;; rotating

(defun zone-line-specs ()
  (let ((ok t)
        ret)
    (save-excursion
      (goto-char (window-start))
      (while (and ok (< (point) (window-end)))
        (when (looking-at "[\t ]*\\([^\n]+\\)")
          (setq ret (cons (cons (match-beginning 1) (match-end 1)) ret)))
        (setq ok (zerop (forward-line 1)))))
    ret))

(defun zone-pgm-rotate (&optional random-style)
  (let* ((specs (apply
                 'vector
                 (let (res)
                   (mapc (lambda (ent)
			   (let* ((beg (car ent))
				  (end (cdr ent))
				  (amt (if random-style
					   (funcall random-style)
					 (- (random 7) 3))))
			     (when (< (- end (abs amt)) beg)
			       (setq amt (random (- end beg))))
			     (unless (= 0 amt)
			       (setq res
				     (cons
				      (vector amt beg (- end (abs amt)))
				      res)))))
			 (zone-line-specs))
                   res)))
         (n (length specs))
         amt aamt cut paste txt i ent)
    (while (not (input-pending-p))
      (setq i 0)
      (while (< i n)
        (setq ent (aref specs i))
        (setq amt (aref ent 0) aamt (abs amt))
        (if (> 0 amt)
            (setq cut 1 paste 2)
          (setq cut 2 paste 1))
        (goto-char (aref ent cut))
        (setq aamt (min aamt (- (point-max) (point))))
        (setq txt (buffer-substring (point) (+ (point) aamt)))
        (delete-char aamt)
        (goto-char (aref ent paste))
        (insert txt)
        (setq i (1+ i)))
      (sit-for 0.04))))

(defun zone-pgm-rotate-LR-lockstep ()
  (zone-pgm-rotate (lambda () 1)))

(defun zone-pgm-rotate-RL-lockstep ()
  (zone-pgm-rotate (lambda () -1)))

(defun zone-pgm-rotate-LR-variable ()
  (zone-pgm-rotate (lambda () (1+ (random 3)))))

(defun zone-pgm-rotate-RL-variable ()
  (zone-pgm-rotate (lambda () (1- (- (random 3))))))


;;;; dripping

(defsubst zone-cpos (pos)
  (buffer-substring pos (1+ pos)))

(defsubst zone-replace-char (count del-count char-as-string new-value)
  (delete-char (or del-count (- count)))
  (let ((s (apply #'propertize (string new-value)
                  (text-properties-at 0 char-as-string))))
    (dotimes (_ count) (insert s))))

(defsubst zone-park/sit-for (pos seconds)
  (let ((p (point)))
    (goto-char pos)
    (prog1 (sit-for seconds)
      (goto-char p))))

(defun zone-fret (wbeg pos)
  (let* ((case-fold-search nil)
         (c-string (zone-cpos pos))
         (cw-ceil (ceiling (char-width (aref c-string 0))))
         (hmm (cond
               ((string-match "[a-z]" c-string) (upcase c-string))
               ((string-match "[A-Z]" c-string) (downcase c-string))
               (t (propertize " " 'display `(space :width ,cw-ceil)))))
         (wait 0.5))
    (dotimes (i 20)
      (goto-char pos)
      (delete-char 1)
      (insert (if (evenp i) hmm c-string))
      (zone-park/sit-for wbeg (setq wait (* wait 0.8))))
    (delete-char -1) (insert c-string)))

(defun zone-fill-out-screen (width height)
  (let ((start (window-start))
	(line (make-string width 32)))
    (goto-char start)
    ;; fill out rectangular ws block
    (while (progn (end-of-line)
		  (let ((cc (current-column)))
		    (if (< cc width)
			(insert (substring line cc))
		      (delete-char (- width cc)))
		    (cond ((eobp) (insert "\n") nil)
			  (t (forward-char 1) t)))))
    ;; pad ws past bottom of screen
    (let ((nl (- height (count-lines (point-min) (point)))))
      (when (> nl 0)
	(setq line (concat line "\n"))
        (dotimes (_ nl)
	  (insert line))))
    (goto-char start)
    (recenter 0)
    (sit-for 0)))

(defun zone-fall-through-ws (c wbeg wend)
  (let* ((cw-ceil (ceiling (char-width (aref c 0))))
         (spaces (make-string cw-ceil 32))
         (col (current-column))
         (wait 0.15)
         newpos fall-p)
    (while (when (save-excursion
                   (and (zerop (forward-line 1))
                        (progn
                          (forward-char col)
                          (= col (current-column)))
                        (setq newpos (point))
                        (string= spaces (buffer-substring-no-properties
                                         newpos (+ newpos cw-ceil)))
                        (setq newpos (+ newpos (1- cw-ceil)))))
	     (setq fall-p t)
	     (delete-char 1)
	     (insert spaces)
             (goto-char newpos)
	     (when (< (point) wend)
	       (delete-char cw-ceil)
	       (insert c)
	       (forward-char -1)
	       (zone-park/sit-for wbeg (setq wait (* wait 0.8))))))
    fall-p))

(defun zone-pgm-drip (&optional fret-p pancake-p)
  (let* ((ww (1- (window-width)))
         (wh (window-height))
         (mc 0)                         ; miss count
         (total (* ww wh))
         (fall-p nil)
         wbeg wend c)
    (zone-fill-out-screen ww wh)
    (setq wbeg (window-start)
          wend (window-end))
    (catch 'done
      (while (not (input-pending-p))
        (setq mc 0 wend (window-end))
        ;; select non-ws character, but don't miss too much
        (goto-char (+ wbeg (random (- wend wbeg))))
        (while (looking-at "[ \n\f]")
          (if (= total (setq mc (1+ mc)))
              (throw 'done 'sel)
            (goto-char (+ wbeg (random (- wend wbeg))))))
        ;; character animation sequence
        (let ((p (point)))
          (when fret-p (zone-fret wbeg p))
          (goto-char p)
          (setq c (zone-cpos p)
                fall-p (zone-fall-through-ws c wbeg wend)))
        ;; assuming current-column has not changed...
        (when (and pancake-p
                   fall-p
                   (< (count-lines (point-min) (point))
                      wh))
          (let ((cw (ceiling (char-width (aref c 0)))))
            (zone-replace-char cw   1 c ?@) (zone-park/sit-for wbeg 0.137)
            (zone-replace-char cw nil c ?*) (zone-park/sit-for wbeg 0.137)
            (zone-replace-char cw nil c ?_)))))))

(defun zone-pgm-drip-fretfully ()
  (zone-pgm-drip t))

(defun zone-pgm-five-oclock-swan-dive ()
  (zone-pgm-drip nil t))

(defun zone-pgm-martini-swan-dive ()
  (zone-pgm-drip t t))

(defun zone-pgm-rat-race ()
  (while (not (input-pending-p))
    (zone-call '((zone-pgm-rotate 10)
                 (zone-pgm-drip-fretfully 15)
                 (zone-pgm-drip 10)
                 ((lambda ()
                    (goto-char (point-min))
                    (while (re-search-forward " +$" nil t)
                      (delete-region (match-beginning 0) (match-end 0))))
                  5)))))


;;;; paragraph spazzing (for textish modes)

(defun zone-pgm-paragraph-spaz ()
  (if (memq (zone-orig major-mode)
            ;; there should be a better way to distinguish textish modes
            '(text-mode texinfo-mode fundamental-mode))
      (let ((fill-column fill-column)
            (fc-min fill-column)
            (fc-max fill-column)
            (max-fc (1- (frame-width))))
        (while (sit-for 0.1)
          (fill-paragraph 1)
          (setq fill-column (+ fill-column (- (random 5) 2)))
          (when (< fill-column fc-min)
            (setq fc-min fill-column))
          (when (> fill-column max-fc)
            (setq fill-column max-fc))
          (when (> fill-column fc-max)
            (setq fc-max fill-column))))
    (message "Zoning... (zone-pgm-rotate)")
    (zone-pgm-rotate)))


;;;; stressing and destressing

(defun zone-pgm-stress ()
  (goto-char (point-min))
  (let ((ok t)
        lines)
    (while (and ok (< (point) (point-max)))
      (let ((p (point)))
        (setq ok (zerop (forward-line 1))
              lines (cons (buffer-substring p (point)) lines))))
    (sit-for 5)
    (zone-hiding-mode-line
     (let ((message-log-max nil)
           (msg "Zoning... (zone-pgm-stress)"))
       (while (not (string= msg ""))
         (message (setq msg (substring msg 1)))
         (sit-for 0.05)))
     (while (not (input-pending-p))
       (when (< 50 (random 100))
         (goto-char (point-max))
         (forward-line -1)
         (delete-region (point) (line-beginning-position 2))
         (goto-char (point-min))
         (insert (seq-random-elt lines)))
       (let ((message-log-max nil))
         (message (concat (make-string (random (- (frame-width) 5)) ? ) "grrr")))
       (sit-for 0.1)))))

(defun zone-pgm-stress-destress ()
  (zone-call 'zone-pgm-stress 25)
  (zone-hiding-mode-line
   (sit-for 3)
   (erase-buffer)
   (sit-for 3)
   (insert-buffer-substring "*Messages*")
   (message "")
   (goto-char (point-max))
   (recenter -1)
   (sit-for 3)
   (delete-region (point-min) (window-start))
   (message "hey why stress out anyway?")
   (zone-call '((zone-pgm-rotate         30)
                (zone-pgm-whack-chars    10)
                zone-pgm-drip))))


;;;; the lyfe so short the craft so long to lerne --chaucer

(defvar zone-pgm-random-life-wait nil
  "Seconds to wait between successive `life' generations.
If nil, `zone-pgm-random-life' chooses a value from 0-3 (inclusive).")

(defvar life-patterns) ; from life.el

(defun zone-pgm-random-life ()
  (require 'life)
  (zone-fill-out-screen (1- (window-width)) (1- (window-height)))
  (let ((top (progn (goto-char (window-start)) (forward-line 7) (point)))
        (bot (progn (goto-char (window-end)) (forward-line -7) (point)))
        (rtc (- (frame-width) 11))
        (min (window-start))
        (max (1- (window-end)))
        s c col)
    (delete-region max (point-max))
    (while (and (progn (goto-char min) (sit-for 0.05))
                (progn (goto-char (+ min (random max)))
                       (or (progn (skip-chars-forward " @\n" max)
                                  (not (= max (point))))
                           (unless (or (= 0 (skip-chars-backward " @\n" min))
                                       (= min (point)))
                             (forward-char -1)
                             t))))
      (unless (or (eolp) (eobp))
        (setq s (zone-cpos (point))
              c (aref s 0))
        (zone-replace-char
         (char-width c)
         1 s (cond ((or (> top (point))
                        (< bot (point))
                        (or (> 11 (setq col (current-column)))
                            (< rtc col)))
                    32)
                   ((and (<= ?a c) (>= ?z c)) (+ c (- ?A ?a)))
                   ((and (<= ?A c) (>= ?Z c)) ?*)
                   (t ?@)))))
    (sit-for 3)
    (setq col nil)
    (goto-char bot)
    (while (< top (point))
      (setq c (point))
      (move-to-column 9)
      (setq col (cons (buffer-substring (point) c) col))
      (end-of-line 0)
      (forward-char -10))
    (let ((life-patterns (vector
                          (if (and col (search-forward "@" max t))
                              (cons (make-string (length (car col)) 32) col)
                            (list (mapconcat 'identity
                                             (make-list (/ (- rtc 11) 15)
                                                        (make-string 5 ?@))
                                             (make-string 10 32)))))))
      (life (or zone-pgm-random-life-wait (random 4)))
      (kill-buffer nil))))


;;;;;;;;;;;;;;;
(provide 'zone)

;;; zone.el ends here
