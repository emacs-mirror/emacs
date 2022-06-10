;;; dnd-tests.el --- Tests for X DND support -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

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

;; Tests for stuff in x-dnd.el that doesn't require a window system.

;;; Code:

(require 'x-dnd)

(when (display-graphic-p)
  (error "This test cannot be run under X"))

;; Dummy replacements.

(defconst x-dnd-tests-drag-window-xid 3948573
  "XID of the drag window returned during the test.")

(defconst x-dnd-tests-targets-table
  (base64-decode-string
   "bAArAKIBAAAGAB8AAABqAQAANgIAAJMCAAAFAwAABgMAAAEAkMJbAAEAINNbAAUAHwAAAGoBAAA2
AgAAkwIAANkfAAALAB8AAABqAQAANgIAAJMCAADyAgAA2R8AANwfAADgHwAA4R8AAOIfAADjHwAA
AQDQMAgCAQBQTggCAQCwe5IAAQDQmZIABgDyAgAA9wIAABcRAADgHwAAvSEAAI3AAAABAHC52AAB
AGDY2AABAABq3QABAGBw3QAIAB8AAAA2AgAA8gIAANwfAADgHwAA4R8AAOIfAADjHwAAAQBwBOEA
AQCACuEAAQAwLwUCAQDwPgUCAQBQxoQBAQCQ3YQBAQCQBYoBAQDACYoBAQCgMooBAQCgOIoBAQAf
AAAAAQDATrcDAQAQ1LcDAQCw/sADAQAgBcEDAQBQt7oDAQAAUsIDAQCAc7wDAQAwerwDAQBAIKUE
AQAALKUEAQDwfKUEAQDgg6UEAQCgjesEAQAAmusEAQCA7+sEAQCw9usECAAfAAAAagEAADYCAACT
AgAABQMAAAYDAAATGwAAGhsAAA==")
  "Predefined Motif targets table used to test the targets table parser.")

(defconst x-dnd-tests-lispy-targets-table [[31 362 566 659 773 774] [6013584] [6017824]
                                           [31 362 566 659 8153]
                                           [31 362 566 659 754 8153 8156 8160 8161 8162 8163]
                                           [34091216] [34098768] [9599920]
                                           [9607632] [754 759 4375 8160 8637 49293]
                                           [14203248] [14211168] [14510592]
                                           [14512224] [31 566 754 8156 8160 8161 8162 8163]
                                           [14746736] [14748288] [33894192] [33898224]
                                           [25478736] [25484688] [25822608] [25823680]
                                           [25834144] [25835680] [31] [62344896] [62379024]
                                           [62979760] [62981408] [62568272] [63066624]
                                           [62681984] [62683696] [77930560] [77933568]
                                           [77954288] [77956064] [82546080] [82549248]
                                           [82571136] [82572976] [31 362 566 659 773 774 6931 6938]]
  "The expected result of parsing that targets table.")

(defalias 'x-window-property
  (lambda (prop &optional _frame type window-id _delete-p _vector-ret-p)
    (cond
     ((and (equal prop "_MOTIF_DRAG_WINDOW")
           (zerop window-id) (equal type "WINDOW"))
      x-dnd-tests-drag-window-xid)
     ((and (equal prop "_MOTIF_DRAG_TARGETS")
           (equal type "_MOTIF_DRAG_TARGETS")
           (equal window-id x-dnd-tests-drag-window-xid))
      x-dnd-tests-targets-table))))

;; This test also serves to exercise most of the Motif value
;; extraction code.
(ert-deftest x-dnd-tests-read-xm-targets-table ()
  (should (equal (x-dnd-xm-read-targets-table nil)
                 x-dnd-tests-lispy-targets-table)))

(provide 'x-dnd-tests)
;;; x-dnd-tests.el ends here
