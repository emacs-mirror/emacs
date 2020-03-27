;;; cursor-sensor.el --- React to cursor movement  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2020 Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Keywords:

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

;; This package implements the `cursor-intangible' and
;; `cursor-sensor-functions' properties, which are meant to replace
;; the old `intangible', `point-entered', and `point-left' properties.

;; To use `cursor-intangible', just enable the
;; `cursor-intangible-mode' minor mode, after which this package will
;; move point away from any position that has a non-nil
;; `cursor-intangible' property.  This is only done just before
;; redisplay happens, contrary to the old `intangible' property which
;; was done at a much lower level.

;; To use `cursor-sensor-functions', enable the `cursor-sensor-mode'
;; minor mode, after which the `cursor-sensor-functions' will be
;; called just before redisplay happens, according to the movement of
;; the cursor since the last redisplay.

;;;; Motivation

;; The old properties were very problematic in practice because they
;; operate at a much lower level and hence affect all motion
;; *functions* like goto-char, forward-char, ... hence breaking
;; invariants like:
;;
;;    (forward-char N) == (progn (forward-char N1) (forward-char (- N N1)))
;;    (point) == (progn (forward-char N) (forward-char -N) (point))
;;    (+ N (point)) == (progn (forward-char N) (point))
;;
;; The problems would usually show up due to interaction between
;; unrelated code working in the same buffer, where one code used those
;; properties and the other (unknowingly) assumed those aren't used.
;; In practice a *lot* of code assumes there's no such funny business.
;;
;; Worse: all(?) packages using those properties don't actually want those
;; properties to affect motion at such a low-level, they only want to
;; affect the overall effect of commands, but not the effect of every
;; single point-motion that a given command happened to use internally.

;;; Code:

;;;###autoload
(defvar cursor-sensor-inhibit nil
  "When non-nil, suspend `cursor-sensor-mode' and `cursor-intangible-mode'.
By convention, this is a list of symbols where each symbol stands for the
\"cause\" of the suspension.")

(defun cursor-sensor--intangible-p (pos)
  (let ((p (get-pos-property pos 'cursor-intangible)))
    (if p
        (let (a b)
          (if (and (setq a (get-char-property pos 'cursor-intangible))
                   (setq b (if (> pos (point-min))
                               (get-char-property (1- pos) 'cursor-intangible)))
                   (not (eq a b)))
              ;; If we're right between two different intangible thingies,
              ;; we can stop here.  This is not quite consistent with the
              ;; interpretation of "if it's sticky, then this boundary is
              ;; itself intangible", but it's convenient (and it better matches
              ;; the behavior of `intangible', making it easier to port code).
              nil p))
      p)))

(defun cursor-sensor-tangible-pos (curpos window &optional second-chance)
  (let ((newpos curpos))
    (when (cursor-sensor--intangible-p newpos)
      (let ((oldpos (window-parameter window 'cursor-intangible--last-point)))
        (cond
         ((or (and (integerp oldpos) (< oldpos newpos))
              (eq newpos (point-min)))
          (while
              (when (< newpos (point-max))
                (setq newpos
                      (if (get-char-property newpos 'cursor-intangible)
                          (next-single-char-property-change
                           newpos 'cursor-intangible nil (point-max))
                        (1+ newpos)))
                (cursor-sensor--intangible-p newpos))))
         (t ;; (>= oldpos newpos)
          (while
              (when (> newpos (point-min))
                (setq newpos
                      (if (get-char-property (1- newpos) 'cursor-intangible)
                          (previous-single-char-property-change
                           newpos 'cursor-intangible nil (point-min))
                        (1- newpos)))
                (cursor-sensor--intangible-p newpos)))))
        (if (not (and (or (eq newpos (point-min)) (eq newpos (point-max)))
                      (cursor-sensor--intangible-p newpos)))
            ;; All clear, we're good to go.
            newpos
          ;; We're still on an intangible position because we bumped
          ;; into an intangible BOB/EOB: try to move in the other direction.
          (if second-chance
              ;; Actually, we tried already and that failed!
              curpos
            (cursor-sensor-tangible-pos newpos window 'second-chance)))))))

(defun cursor-sensor-move-to-tangible (window)
  (let* ((curpos (window-point window))
         (newpos (cursor-sensor-tangible-pos curpos window)))
    (when newpos (set-window-point window newpos))
    (set-window-parameter window 'cursor-intangible--last-point
                          (or newpos curpos))))

(defun cursor-sensor--move-to-tangible (window)
  (unless cursor-sensor-inhibit
    (cursor-sensor-move-to-tangible window)))

;;;###autoload
(define-minor-mode cursor-intangible-mode
  "Keep cursor outside of any `cursor-intangible' text property."
  :global nil
  (if cursor-intangible-mode
      (add-hook 'pre-redisplay-functions #'cursor-sensor--move-to-tangible
                nil t)
    (remove-hook 'pre-redisplay-functions #'cursor-sensor--move-to-tangible t)))

;;; Detect cursor movement.

(defun cursor-sensor--detect (&optional window)
  (with-current-buffer (window-buffer window)
    (unless cursor-sensor-inhibit
      (let* ((point (window-point window))
             ;; It's often desirable to make the
             ;; cursor-sensor-functions property non-sticky on both
             ;; ends, but that means get-pos-property might never
             ;; see it.
             (new (or (get-char-property point 'cursor-sensor-functions)
                      (unless (<= (point-min) point)
                        (get-char-property (1- point)
                                           'cursor-sensor-functions))))
             (old (window-parameter window 'cursor-sensor--last-state))
             (oldposmark (car old))
             (oldpos (or (if oldposmark (marker-position oldposmark))
                         (point-min)))
             (start (min oldpos point))
             (end (max oldpos point)))
        (unless (or (null old) (eq (marker-buffer oldposmark) (current-buffer)))
          ;; `window' does not display the same buffer any more!
          (setcdr old nil))
        (if (or (and (null new) (null (cdr old)))
                (and (eq new (cdr old))
                     (eq (next-single-char-property-change
                          start 'cursor-sensor-functions nil end)
                         end)))
            ;; Clearly nothing to do.
            nil
          ;; Maybe something to do.  Let's see exactly what needs to run.
          (let* ((missing-p
                  (lambda (f)
                    "Non-nil if F is missing somewhere between START and END."
                    (let ((pos start)
                          (missing nil))
                      (while (< pos end)
                        (setq pos (next-single-char-property-change
                                   pos 'cursor-sensor-functions
                                   nil end))
                        (unless (memq f (get-char-property
                                         pos 'cursor-sensor-functions))
                          (setq missing t)))
                      missing)))
                 (window (selected-window)))
            (dolist (f (cdr old))
              (unless (and (memq f new) (not (funcall missing-p f)))
                (funcall f window oldpos 'left)))
            (dolist (f new)
              (unless (and (memq f (cdr old)) (not (funcall missing-p f)))
                (funcall f window oldpos 'entered)))))

        ;; Remember current state for next time.
        ;; Re-read cursor-sensor-functions since the functions may have moved
        ;; window-point!
        (if old
            (progn (move-marker (car old) point)
                   (setcdr old new))
          (set-window-parameter window 'cursor-sensor--last-state
                                (cons (copy-marker point) new)))))))

;;;###autoload
(define-minor-mode cursor-sensor-mode
  "Handle the `cursor-sensor-functions' text property.
This property should hold a list of functions which react to the motion
of the cursor.  They're called with three arguments (WINDOW OLDPOS DIR)
where WINDOW is the affected window, OLDPOS is the last known position of
the cursor and DIR can be `entered' or `left' depending on whether the cursor
is entering the area covered by the text-property property or leaving it."
  :global nil
  (cond
   (cursor-sensor-mode
    ;; Also add ourselves to `post-command-hook' because
    ;; `pre-redisplay-functions' are sometimes called too late (after
    ;; adjust_point_for_property has moved point, which makes it
    ;; "impossible" for cursor-sensor-functions to do things like
    ;; revealing invisible text).
    (add-hook 'post-command-hook #'cursor-sensor--detect nil t)
    (add-hook 'pre-redisplay-functions #'cursor-sensor--detect
              nil t))
   (t
    (remove-hook 'post-command-hook #'cursor-sensor--detect t)
    (remove-hook  'pre-redisplay-functions #'cursor-sensor--detect
                t))))

(provide 'cursor-sensor)
;;; cursor-sensor.el ends here
