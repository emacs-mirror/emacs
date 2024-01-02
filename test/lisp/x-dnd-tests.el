;;; dnd-tests.el --- Tests for X DND support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024 Free Software Foundation, Inc.

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
(require 'cl-lib)

(when (display-graphic-p)
  (error "This test cannot be run under X"))

;; Dummy replacements.

(defconst x-dnd-tests-drag-window-xid 3948573
  "XID of the drag window returned during the test.")

(defvar x-dnd-tests-xds-property-value nil
  "The value of the `XdndDirectSave0' window property.")

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
  (lambda (prop &optional _frame type window-id delete-p _vector-ret-p)
    (cond
     ((and (equal prop "_MOTIF_DRAG_WINDOW")
           (zerop window-id) (equal type "WINDOW"))
      x-dnd-tests-drag-window-xid)
     ((and (equal prop "_MOTIF_DRAG_TARGETS")
           (equal type "_MOTIF_DRAG_TARGETS")
           (equal window-id x-dnd-tests-drag-window-xid))
      x-dnd-tests-targets-table)
     ((and (equal prop "XdndDirectSave0")
           (or (equal type "text/plain")
               (equal type "AnyPropertyType")))
      (prog1 x-dnd-tests-xds-property-value
        (when delete-p
          (setq x-dnd-tests-xds-property-value nil)))))))

;; This test also serves to exercise most of the Motif value
;; extraction code.
(ert-deftest x-dnd-tests-read-xm-targets-table ()
  (should (equal (x-dnd-xm-read-targets-table nil)
                 x-dnd-tests-lispy-targets-table)))

;;; XDS tests.

(defvar x-dnd-xds-testing)

(defvar x-dnd-tests-xds-target-dir nil
  "The name of the target directory where the file will be saved.")

(defvar x-dnd-tests-xds-name nil
  "The name that the dragged file should be saved under.")

(defvar x-dnd-tests-xds-include-hostname nil
  "Whether or not to include the hostname inside the XDS URI.")

(defun x-dnd-tests-call-xds-converter ()
  "Look up the XDS selection converter and call it.
Return the result of the selection."
  (let ((conv (cdr (assq 'XdndDirectSave0
                         selection-converter-alist))))
    (should (functionp conv))
    (funcall conv 'XdndDirectSave0 'XdndDirectSave0 nil)))

(defalias 'x-begin-drag
  (lambda (_targets &optional action frame &rest _)
    ;; Verify that frame is either nil or a valid frame.
    (when (and frame (not (frame-live-p frame)))
      (signal 'wrong-type-argument frame))
    (prog1 'XdndActionDirectSave
      ;; Verify that the action is `XdndActionDirectSave'.
      (should (eq action 'XdndActionDirectSave))
      ;; Set the property value to the URI of the new file.
      (should (and (stringp x-dnd-tests-xds-property-value)
                   (not (multibyte-string-p x-dnd-tests-xds-property-value))))
      (let ((uri (if x-dnd-tests-xds-include-hostname
                     (format "file://%s%s" (system-name)
                             (expand-file-name x-dnd-tests-xds-property-value
                                               x-dnd-tests-xds-target-dir))
                   (concat "file://" (expand-file-name x-dnd-tests-xds-property-value
                                                       x-dnd-tests-xds-target-dir)))))
        (setq x-dnd-tests-xds-property-value
              (encode-coding-string (url-encode-url uri)
                                    'raw-text)))
      ;; Convert the selection and verify its success.
      (should (equal (x-dnd-tests-call-xds-converter)
                     '(STRING . "S"))))))

(defalias 'x-change-window-property
  (lambda (prop value &optional _frame type format outer-p _window-id)
    ;; Check that the properties are the right type.
    (should (equal prop "XdndDirectSave0"))
    (should (equal value (encode-coding-string
                          x-dnd-tests-xds-name
                          (or file-name-coding-system
                              default-file-name-coding-system))))
    (should (equal type "text/plain"))
    (should (equal format 8))
    (should (not outer-p))
    (setq x-dnd-tests-xds-property-value value)))

(defalias 'x-delete-window-property
  (lambda (&rest _args)
    ;; This function shouldn't ever be reached during XDS.
    (setq x-dnd-tests-xds-property-value nil)))

(defun x-dnd-tests-do-direct-save-internal (include-hostname)
  "Test the behavior of `x-dnd-do-direct-save'.
Make it perform a direct save to a randomly generated directory,
and check that the file exists.  If INCLUDE-HOSTNAME, include the
hostname in the target URI."
  (let ((x-dnd-tests-xds-include-hostname include-hostname)
        (x-dnd-tests-xds-target-dir
         (file-name-as-directory (expand-file-name
                                  (make-temp-name "x-dnd-test")
                                  temporary-file-directory)))
        (original-file (expand-file-name
                        (make-temp-name "x-dnd-test")
                        temporary-file-directory))
        (x-dnd-tests-xds-name (make-temp-name "x-dnd-test-target"))
        (x-dnd-xds-testing t))
    ;; The call to `gui-set-selection' is only used for providing the
    ;; conventional `text/uri-list' target and can be ignored.
    (cl-flet ((gui-set-selection #'ignore))
      (unwind-protect
          (progn
            ;; Touch `original-file' if it doesn't exist.
            (unless (file-exists-p original-file)
              (write-region "" 0 original-file))
            ;; Create `x-dnd-tests-xds-target-dir'.
            (make-directory x-dnd-tests-xds-target-dir)
            ;; Start the direct save and verify it returns the correct action.
            (should (eq (x-dnd-do-direct-save original-file
                                              x-dnd-tests-xds-name
                                              nil nil)
                        'XdndActionDirectSave))
            ;; Now verify that the new file exists.
            (should (file-exists-p
                     (expand-file-name x-dnd-tests-xds-name
                                       x-dnd-tests-xds-target-dir)))
            ;; The XDS protocol makes very clear that the window
            ;; property must be deleted after the drag-and-drop
            ;; operation completes.
            (should (not x-dnd-tests-xds-property-value)))
        ;; Clean up after ourselves.
        (ignore-errors
          (delete-file original-file))
        (ignore-errors
          (delete-directory x-dnd-tests-xds-target-dir t))))))

(ert-deftest x-dnd-tests-do-direct-save ()
  ;; TODO: add tests for application/octet-stream transfer.
  (x-dnd-tests-do-direct-save-internal nil)
  ;; Test with both kinds of file: URIs, since different programs
  ;; generate different kinds.
  (x-dnd-tests-do-direct-save-internal t))

(provide 'x-dnd-tests)
;;; x-dnd-tests.el ends here
