;;; text-property-search.el --- search for text properties  -*- lexical-binding:t -*-

;; Copyright (C) 2018-2022 Free Software Foundation, Inc.

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
                                              not-current)
  "Search for the next region of text where PREDICATE is true.
PREDICATE is used to decide whether a value of PROPERTY should be
considered as matching VALUE.

If PREDICATE is a function, it will be called with two arguments:
VALUE and the value of PROPERTY.  The function should return
non-nil if these two values are to be considered a match.

Two special values of PREDICATE can also be used:
If PREDICATE is t, that means a value must `equal' VALUE to be
considered a match.
If PREDICATE is nil (which is the default value), a value will
match if is not `equal' to VALUE.  Furthermore, a nil PREDICATE
means that the match region is ended if the value changes.  For
instance, this means that if you loop with

  (while (setq prop (text-property-search-forward 'face))
    ...)

you will get all distinct regions with non-nil `face' values in
the buffer, and the `prop' object will have the details about the
match.  See the manual for more details and examples about how
VALUE and PREDICATE interact.

If NOT-CURRENT is non-nil, the function will search for the first
region that doesn't include point and has a value of PROPERTY
that matches VALUE.

If no matches can be found, return nil and don't move point.
If found, move point to the end of the region and return a
`prop-match' object describing the match.  To access the details
of the match, use `prop-match-beginning' and `prop-match-end' for
the buffer positions that limit the region, and
`prop-match-value' for the value of PROPERTY in the region."
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
         (not not-current))
    (text-property--find-end-forward (point) property value predicate))
   (t
    (let ((origin (point))
          (ended nil)
          pos)
      ;; Find the next candidate.
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
                                               not-current)
  "Search for the previous region of text whose PROPERTY matches VALUE.

Like `text-property-search-forward', which see, but searches backward,
and if a matching region is found, place point at the start of the region."
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
   ((text-property--match-p
     value (get-text-property (1- (point)) property)
     predicate)
    (let ((origin (point))
          (match (text-property--find-end-backward
                  (1- (point)) property value predicate)))
      ;; When we want to ignore the current element, then repeat the
      ;; search if we haven't moved out of it yet.
      (if (and not-current
               (equal (get-text-property (point) property)
                      (get-text-property origin property)))
          (text-property-search-backward property value predicate)
        match)))
   (t
    (let ((origin (point))
          (ended nil)
          pos)
      (forward-char -1)
      ;; Find the previous candidate.
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

;;; text-property-search.el ends here
