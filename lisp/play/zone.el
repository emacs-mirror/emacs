;;; zone.el --- idle display hacks

;; Copyright (C) 2000, 2001 Free Software Foundation, Inc.

;; Author: Victor Zandy <zandy@cs.wisc.edu>
;; Maintainer: Thien-Thi Nguyen <ttn@gnu.org>
;; Keywords: games
;; Created: June 6, 1998

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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Don't zone out in front of Emacs!  Try M-x zone.
;; If it eventually irritates you, try M-x zone-leave-me-alone.

;; Bored by the zone pyrotechnics?  Write your own!  Add it to
;; `zone-programs'.  See `zone-call' for higher-ordered zoning.

;; WARNING: Not appropriate for Emacs sessions over modems or
;; computers as slow as mine.

;; THANKS: Christopher Mayer, Scott Flinchbaugh, Rachel Kalmar,
;;         Max Froumentin.

;;; Code:

(require 'timer)
(require 'tabify)
(eval-when-compile (require 'cl))

(defvar zone-idle 20
  "*Seconds to idle before zoning out.")

(defvar zone-timer nil
  "The timer we use to decide when to zone out, or nil if none.")

(defvar zone-timeout nil
  "*Seconds to timeout the zoning.
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
                       zone-pgm-paragraph-spaz
                       zone-pgm-stress
                       zone-pgm-stress-destress
                       ])

(defmacro zone-orig (&rest body)
  `(with-current-buffer (get 'zone 'orig-buffer)
     ,@body))

(defmacro zone-hiding-modeline (&rest body)
  `(let (bg mode-line-fg mode-line-bg mode-line-box)
     (unwind-protect
         (progn
           (when (and (= 0 (get 'zone 'modeline-hidden-level))
                      (display-color-p))
             (setq bg (face-background 'default)
                   mode-line-box (face-attribute 'mode-line :box)
                   mode-line-fg (face-attribute 'mode-line :foreground)
                   mode-line-bg (face-attribute 'mode-line :background))
             (set-face-attribute 'mode-line nil
                                 :foreground bg
                                 :background bg
                                 :box nil))
           (put 'zone 'modeline-hidden-level
                (1+ (get 'zone 'modeline-hidden-level)))
           ,@body)
       (put 'zone 'modeline-hidden-level
            (1- (get 'zone 'modeline-hidden-level)))
       (when (and (> 1 (get 'zone 'modeline-hidden-level))
                  mode-line-fg)
         (set-face-attribute 'mode-line nil
                             :foreground mode-line-fg
                             :background mode-line-bg
                             :box mode-line-box)))))

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
                         (t (error "bad `zone-call' elem:" elem))))
                 program))))

;;;###autoload
(defun zone ()
  "Zone out, completely."
  (interactive)
  (let ((f (selected-frame))
        (outbuf (get-buffer-create "*zone*"))
        (text (buffer-substring (window-start) (window-end)))
        (wp (1+ (- (window-point (selected-window))
                   (window-start)))))
    (put 'zone 'orig-buffer (current-buffer))
    (put 'zone 'modeline-hidden-level 0)
    (set-buffer outbuf)
    (setq mode-name "Zone")
    (erase-buffer)
    (insert text)
    (switch-to-buffer outbuf)
    (setq buffer-undo-list t)
    (untabify (point-min) (point-max))
    (set-window-start (selected-window) (point-min))
    (set-window-point (selected-window) wp)
    (sit-for 0 500)
    (let ((pgm (elt zone-programs (random (length zone-programs))))
          (ct (and f (frame-parameter f 'cursor-type))))
      (when ct (modify-frame-parameters f '((cursor-type . (bar . 0)))))
      (condition-case nil
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
         (while (not (input-pending-p))
           (message (format "We were zoning when we wrote %s..." pgm))
           (sit-for 3)
           (message "...here's hoping we didn't hose your buffer!")
           (sit-for 3)))
        (quit (ding) (message "Zoning...sorry")))
      (when ct (modify-frame-parameters f (list (cons 'cursor-type ct)))))
    (kill-buffer outbuf)))

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


;;;; zone-pgm-jitter

(defun zone-shift-up ()
  (let* ((b (point))
         (e (progn
              (end-of-line)
              (if (looking-at "\n") (1+ (point)) (point))))
         (s (buffer-substring b e)))
    (delete-region b e)
    (goto-char (point-max))
    (insert s)))

(defun zone-shift-down ()
  (goto-char (point-max))
  (forward-line -1)
  (beginning-of-line)
  (let* ((b (point))
         (e (progn
              (end-of-line)
              (if (looking-at "\n") (1+ (point)) (point))))
         (s (buffer-substring b e)))
    (delete-region b e)
    (goto-char (point-min))
    (insert s)))

(defun zone-shift-left ()
  (while (not (eobp))
    (or (eolp)
        (let ((c (following-char)))
          (delete-char 1)
          (end-of-line)
          (insert c)))
    (forward-line 1)))

(defun zone-shift-right ()
  (while (not (eobp))
    (end-of-line)
    (or (bolp)
        (let ((c (preceding-char)))
          (delete-backward-char 1)
          (beginning-of-line)
          (insert c)))
    (forward-line 1)))

(defun zone-pgm-jitter ()
  (let ((ops [
              zone-shift-left
              zone-shift-left
              zone-shift-left
              zone-shift-left
              zone-shift-right
              zone-shift-down
              zone-shift-down
              zone-shift-down
              zone-shift-down
              zone-shift-down
              zone-shift-up
              ]))
    (goto-char (point-min))
    (while (not (input-pending-p))
      (funcall (elt ops (random (length ops))))
      (goto-char (point-min))
      (sit-for 0 10))))


;;;; zone-pgm-whack-chars

(defun zone-pgm-whack-chars ()
  (let ((tbl (copy-sequence (get 'zone-pgm-whack-chars 'wc-tbl))))
    (while (not (input-pending-p))
      (let ((i 48))
        (while (< i 122)
          (aset tbl i (+ 48 (random (- 123 48))))
          (setq i (1+ i)))
        (translate-region (point-min) (point-max) tbl)
        (sit-for 0 2)))))

(put 'zone-pgm-whack-chars 'wc-tbl
     (let ((tbl (make-string 128 ?x))
           (i 0))
       (while (< i 128)
         (aset tbl i i)
         (setq i (1+ i)))
       tbl))

;;;; zone-pgm-dissolve

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
      (sit-for 0 2))))

(defun zone-pgm-dissolve ()
  (zone-remove-text)
  (zone-pgm-jitter))


;;;; zone-pgm-explode

(defun zone-exploding-remove ()
  (let ((i 0))
    (while (< i 20)
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
      (sit-for 0 2)))
  (zone-pgm-jitter))

(defun zone-pgm-explode ()
  (zone-exploding-remove)
  (zone-pgm-jitter))


;;;; zone-pgm-putz-with-case

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
      (sit-for 0 2))))

(defun zone-pgm-putz-with-case ()
  (goto-char (point-min))
  (while (not (input-pending-p))
    (let ((np (+ 2 (random 5)))
          (pm (point-max)))
      (while (< np pm)
        (goto-char np)
        (let ((prec (preceding-char))
              (props (text-properties-at (1- (point)))))
          (insert (if (zerop (random 2))
                      (upcase prec)
                    (downcase prec)))
          (set-text-properties (1- (point)) (point) props))
        (backward-char 2)
        (delete-char 1)
        (setq np (+ np (1+ (random 5))))))
    (goto-char (point-min))
    (sit-for 0 2)))


;;;; zone-pgm-rotate

(defun zone-line-specs ()
  (let (ret)
    (save-excursion
      (goto-char (window-start))
      (while (< (point) (window-end))
        (when (looking-at "[\t ]*\\([^\n]+\\)")
          (setq ret (cons (cons (match-beginning 1) (match-end 1)) ret)))
        (forward-line 1)))
    ret))

(defun zone-pgm-rotate (&optional random-style)
  (let* ((specs (apply
                 'vector
                 (let (res)
                   (mapcar (lambda (ent)
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


;;;; zone-pgm-drip

(defun zone-cpos (pos)
  (buffer-substring pos (1+ pos)))

(defun zone-fret (pos)
  (let* ((case-fold-search nil)
         (c-string (zone-cpos pos))
         (hmm (cond
               ((string-match "[a-z]" c-string) (upcase c-string))
               ((string-match "[A-Z]" c-string) (downcase c-string))
               (t " "))))
    (do ((i 0 (1+ i))
         (wait 0.5 (* wait 0.8)))
        ((= i 20))
      (goto-char pos)
      (delete-char 1)
      (insert (if (= 0 (% i 2)) hmm c-string))
      (sit-for wait))
    (delete-char -1) (insert c-string)))

(defun zone-fall-through-ws (c col wend)
  (let ((fall-p nil)                    ; todo: move outward
        (wait 0.15)
        (o (point))                     ; for terminals w/o cursor hiding
        (p (point)))
    (while (progn
             (forward-line 1)
             (move-to-column col)
             (looking-at " "))
      (setq fall-p t)
      (delete-char 1)
      (insert (if (< (point) wend) c " "))
      (save-excursion
        (goto-char p)
        (delete-char 1)
        (insert " ")
        (goto-char o)
        (sit-for (setq wait (* wait 0.8))))
      (setq p (1- (point))))
    fall-p))

(defun zone-pgm-drip (&optional fret-p pancake-p)
  (let* ((ww (1- (window-width)))
         (wh (window-height))
         (mc 0)                         ; miss count
         (total (* ww wh))
         (fall-p nil))
    (goto-char (point-min))
    ;; fill out rectangular ws block
    (while (not (eobp))
      (end-of-line)
      (let ((cc (current-column)))
        (if (< cc ww)
            (insert (make-string (- ww cc) ? ))
          (delete-char (- ww cc))))
      (unless (eobp)
        (forward-char 1)))
    ;; pad ws past bottom of screen
    (let ((nl (- wh (count-lines (point-min) (point)))))
      (when (> nl 0)
        (let ((line (concat (make-string (1- ww) ? ) "\n")))
          (do ((i 0 (1+ i)))
              ((= i nl))
            (insert line)))))
    (catch 'done
      (while (not (input-pending-p))
        (goto-char (point-min))
        (sit-for 0)
        (let ((wbeg (window-start))
              (wend (window-end)))
          (setq mc 0)
          ;; select non-ws character, but don't miss too much
          (goto-char (+ wbeg (random (- wend wbeg))))
          (while (looking-at "[ \n\f]")
            (if (= total (setq mc (1+ mc)))
                (throw 'done 'sel)
              (goto-char (+ wbeg (random (- wend wbeg))))))
          ;; character animation sequence
          (let ((p (point)))
            (when fret-p (zone-fret p))
            (goto-char p)
            (setq fall-p (zone-fall-through-ws
                          (zone-cpos p) (current-column) wend))))
        ;; assuming current-column has not changed...
        (when (and pancake-p
                   fall-p
                   (< (count-lines (point-min) (point))
                      wh))
          (previous-line 1)
          (forward-char 1)
          (sit-for 0.137)
          (delete-char -1)
          (insert "@")
          (sit-for 0.137)
          (delete-char -1)
          (insert "*")
          (sit-for 0.137)
          (delete-char -1)
          (insert "_"))))))

(defun zone-pgm-drip-fretfully ()
  (zone-pgm-drip t))

(defun zone-pgm-five-oclock-swan-dive ()
  (zone-pgm-drip nil t))

(defun zone-pgm-martini-swan-dive ()
  (zone-pgm-drip t t))


;;;; zone-pgm-paragraph-spaz

(defun zone-pgm-paragraph-spaz ()
  (if (memq (zone-orig major-mode) '(text-mode fundamental-mode))
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


;;;; zone-pgm-stress

(defun zone-pgm-stress ()
  (goto-char (point-min))
  (let (lines)
    (while (< (point) (point-max))
      (let ((p (point)))
        (forward-line 1)
        (setq lines (cons (buffer-substring p (point)) lines))))
    (sit-for 5)
    (zone-hiding-modeline
     (let ((msg "Zoning... (zone-pgm-stress)"))
       (while (not (string= msg ""))
         (message (setq msg (substring msg 1)))
         (sit-for 0.05)))
     (while (not (input-pending-p))
       (when (< 50 (random 100))
         (goto-char (point-max))
         (forward-line -1)
         (let ((kill-whole-line t))
           (kill-line))
         (goto-char (point-min))
         (insert (nth (random (length lines)) lines)))
       (message (concat (make-string (random (- (frame-width) 5)) ? ) "grrr"))
       (sit-for 0.1)))))


;;;; zone-pgm-stress-destress

(defun zone-pgm-stress-destress ()
  (zone-call 'zone-pgm-stress 25)
  (zone-hiding-modeline
   (sit-for 3)
   (erase-buffer)
   (sit-for 3)
   (insert-buffer "*Messages*")
   (message "")
   (goto-char (point-max))
   (recenter -1)
   (sit-for 3)
   (delete-region (point-min) (window-start))
   (message "hey why stress out anyway?")
   (zone-call '((zone-pgm-rotate         30)
                (zone-pgm-whack-chars    10)
                zone-pgm-drip))))


;;;;;;;;;;;;;;;
(provide 'zone)

;;; zone.el ends here
