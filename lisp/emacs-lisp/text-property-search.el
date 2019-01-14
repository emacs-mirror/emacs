;;; text-property-search.el --- search for text properties  -*- lexical-binding:t -*-

;; Copyright (C) 2018-2019 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: convenience

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

;;; Code:

(eval-when-compile (require 'cl-lib))

(cl-defstruct (prop-match)
  beginning end value)

(defun text-property-search-forward (property &optional value predicate
                                              not-immediate)
  "Search for the next region that has text property PROPERTY set to VALUE.
If not found, the return value is nil.  If found, point will be
placed at the end of the region and an object describing the
match is returned.

PREDICATE is called with two values.  The first is the VALUE
parameter.  The second is the value of PROPERTY.  This predicate
should return non-nil if there is a match.

Some convenience values for PREDICATE can also be used.  `t'
means the same as `equal'.  `nil' means almost the same as \"not
equal\", but will also end the match if the value of PROPERTY
changes.  See the manual for extensive examples.

If `not-immediate', if the match is under point, it will not be
returned, but instead the next instance is returned, if any.

The return value (if a match is made) is a `prop-match'
structure.  The accessors available are
`prop-match-beginning'/`prop-match-end' (the region in the buffer
that's matching), and `prop-match-value' (the value of PROPERTY
at the start of the region)."
  (interactive
   (list
    (let ((string (completing-read "Search for property: " obarray)))
      (when (> (length string) 0)
        (intern string obarray)))))
  (cond
   ;; No matches at the end of the buffer.
   ((eobp)
    nil)
   ;; We're standing in the property we're looking for, so find the
   ;; end.
   ((and (text-property--match-p value (get-text-property (point) property)
                                 predicate)
         (not not-immediate))
    (text-property--find-end-forward (point) property value predicate))
   (t
    (let ((origin (point))
          (ended nil)
          pos)
      ;; Fix the next candidate.
      (while (not ended)
        (setq pos (next-single-property-change (point) property))
        (if (not pos)
            (progn
              (goto-char origin)
              (setq ended t))
          (goto-char pos)
          (if (text-property--match-p value (get-text-property (point) property)
                                      predicate)
              (setq ended
                    (text-property--find-end-forward
                     (point) property value predicate))
            ;; Skip past this section of non-matches.
            (setq pos (next-single-property-change (point) property))
            (unless pos
              (goto-char origin)
              (setq ended t)))))
      (and (not (eq ended t))
           ended)))))

(defun text-property--find-end-forward (start property value predicate)
  (let (end)
    (if (and value
             (null predicate))
        ;; This is the normal case: We're looking for areas where the
        ;; values aren't, so we aren't interested in sub-areas where the
        ;; property has different values, all non-matching value.
        (let ((ended nil))
          (while (not ended)
            (setq end (next-single-property-change (point) property))
            (if (not end)
                (progn
                  (goto-char (point-max))
                  (setq end (point)
                        ended t))
              (goto-char end)
              (unless (text-property--match-p
                       value (get-text-property (point) property) predicate)
                (setq ended t)))))
      ;; End this at the first place the property changes value.
      (setq end (next-single-property-change (point) property nil (point-max)))
      (goto-char end))
    (make-prop-match :beginning start
                     :end end
                     :value (get-text-property start property))))


(defun text-property-search-backward (property &optional value predicate
                                               not-immediate)
  "Search for the previous region that has text property PROPERTY set to VALUE.
See `text-property-search-forward' for further documentation."
  (interactive
   (list
    (let ((string (completing-read "Search for property: " obarray)))
      (when (> (length string) 0)
        (intern string obarray)))))
  (cond
   ;; We're at the start of the buffer; no previous matches.
   ((bobp)
    nil)
   ;; We're standing in the property we're looking for, so find the
   ;; end.
   ((and (text-property--match-p
          value (get-text-property (1- (point)) property)
          predicate)
         (not not-immediate))
    (text-property--find-end-backward (1- (point)) property value predicate))
   (t
    (let ((origin (point))
          (ended nil)
          pos)
      (forward-char -1)
      ;; Fix the next candidate.
      (while (not ended)
        (setq pos (previous-single-property-change (point) property))
        (if (not pos)
            (progn
              (goto-char origin)
              (setq ended t))
          (goto-char (1- pos))
          (if (text-property--match-p value (get-text-property (point) property)
                                      predicate)
              (setq ended
                    (text-property--find-end-backward
                     (point) property value predicate))
            ;; Skip past this section of non-matches.
            (setq pos (previous-single-property-change (point) property))
            (unless pos
              (goto-char origin)
              (setq ended t)))))
      (and (not (eq ended t))
           ended)))))

(defun text-property--find-end-backward (start property value predicate)
  (let (end)
    (if (and value
             (null predicate))
        ;; This is the normal case: We're looking for areas where the
        ;; values aren't, so we aren't interested in sub-areas where the
        ;; property has different values, all non-matching value.
        (let ((ended nil))
          (while (not ended)
            (setq end (previous-single-property-change (point) property))
            (if (not end)
                (progn
                  (goto-char (point-min))
                  (setq end (point)
                        ended t))
              (goto-char (1- end))
              (unless (text-property--match-p
                       value (get-text-property (point) property) predicate)
                (goto-char end)
                (setq ended t)))))
      ;; End this at the first place the property changes value.
      (setq end (previous-single-property-change
                 (point) property nil (point-min)))
      (goto-char end))
    (make-prop-match :beginning end
                     :end (1+ start)
                     :value (get-text-property end property))))

(defun text-property--match-p (value prop-value predicate)
  (cond
   ((eq predicate t)
    (setq predicate #'equal))
   ((eq predicate nil)
    (setq predicate (lambda (val p-val)
                      (not (equal val p-val))))))
  (funcall predicate value prop-value))

(provide 'text-property-search)
