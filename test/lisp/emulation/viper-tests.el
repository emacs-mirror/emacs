;;; viper-tests.el --- tests for viper.

;; Copyright (C) 2016-2020 Free Software Foundation, Inc.

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


(require 'viper)

(defun viper-test-undo-kmacro (kmacro)
  "In a clean viper buffer, run KMACRO and return `buffer-string'.

This function makes as many attempts as possible to clean up
after itself, although it will leave a buffer called
*viper-test-buffer* if it fails (this is deliberate!)."
  (let (
        ;; Viper just turns itself off during batch use.
        (noninteractive nil)
        ;; Switch off start up message or it will chew the key presses.
        (viper-inhibit-startup-message 't)
        ;; Select an expert-level for the same reason.
        (viper-expert-level 5)
        ;; viper loads this even with -q so make sure it's empty!
        (viper-custom-file-name (make-temp-file "viper-tests" nil ".elc"))
        (before-buffer (current-buffer)))
    (unwind-protect
        (progn
          ;; viper-mode is essentially global, so set it here.
          (viper-mode)
          ;; We must switch to buffer because we are using a keyboard macro
          ;; which appears to not go to the current-buffer but what ever is
          ;; currently taking keyboard events. We use a named buffer because
          ;; then we can see what it in it if it all goes wrong.
          (switch-to-buffer
           (get-buffer-create
            "*viper-test-buffer*"))
          (erase-buffer)
          ;; The new buffer fails to enter vi state so set it.
          (viper-change-state-to-vi)
          ;; Run the macro.
          (execute-kbd-macro kmacro)
          (let ((rtn
                 (buffer-substring-no-properties
                  (point-min)
                  (point-max))))
            ;; Kill the buffer iff the macro succeeds.
            (kill-buffer)
            rtn))
      ;; Switch everything off and restore the buffer.
      (toggle-viper-mode)
      (delete-file viper-custom-file-name)
      (switch-to-buffer before-buffer))))

(ert-deftest viper-test-go ()
  "Test that this file is running."
  (should t))

(ert-deftest viper-test-fix ()
  "Test that the viper kmacro fixture is working."
  (should
   (viper-test-undo-kmacro [])))

(ert-deftest viper-test-undo-1 ()
  "Test for VI like undo behavior.

Insert 1, then 2 on consecutive lines, followed by undo. This
should leave just 1 in the buffer.

Test for Bug #22295"
  (should
   (equal
    "1\n"
    (viper-test-undo-kmacro
     [
      ?a
      ?1
      escape
      ?o
      ?2
      escape
      ?u
      ]
     ))))

(ert-deftest viper-test-undo-2 ()
  "Test for VI like undo behavior.

Insert \"1 2 3 4 5\" then delete the 2, then the 4, and undo.
Should restore the 4, but leave the 2 deleted.

Test for Bug #22295"
  (should
   (equal
    "1 3 4 5\n"
    (viper-test-undo-kmacro
     [
      ?i
      ?1 ?  ?2 ?  ?3 ?  ?4 ?  ?5
      escape
      ?F ?2 ?d ?w
      ?w ?d ?w
      ?u
      ]))))

(ert-deftest viper-test-undo-3 ()
  "Test for VI like undo behavior.

Insert \"1 2 3 4 5 6\", delete the 2, then the 3 4 and 5.
Should restore the 3 4 and 5 but not the 2.

Test for Bug #22295"
  (should
   (equal
    "1 3 4 5 6\n"
    (viper-test-undo-kmacro
     [
      ;; Insert this lot.
      ?i ?1 ? ?2 ? ?3 ? ?4 ? ?5 ? ?6
         escape
         ;; Start of line.
         ?0
         ;; Move to 2, delete
         ?w ?d ?w
         ;; Delete 3 4 5
         ?. ?. ?.
         ;; Undo del 5, then
         ?u ?. ?.
         ]))))


(ert-deftest viper-test-undo-4()
  (should
   (equal
    ""
    (viper-test-undo-kmacro
     [
      ?i ?1 escape
         ?o ?2 escape
         ?o ?3 escape
         ?u ?. ?.
         ])
    )))

;;; viper-tests.el ends here
