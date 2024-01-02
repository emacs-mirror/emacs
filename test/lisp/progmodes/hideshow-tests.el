;;; hideshow-tests.el --- Test suite for hideshow.el  -*- lexical-binding:t -*-

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

;;; Code:

(require 'ert)
(require 'ert-x)
(require 'hideshow)

;; Dependencies for testing:
(require 'cc-mode)


(defmacro hideshow-tests-with-temp-buffer (mode contents &rest body)
  "Create a `hs-minor-mode' enabled MODE temp buffer with CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
always located at the beginning of buffer."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (,mode)
     (hs-minor-mode 1)
     (insert ,contents)
     (goto-char (point-min))
     ,@body))

(defmacro hideshow-tests-with-temp-buffer-selected (mode contents &rest body)
  "Create and switch to a `hs-minor-mode' enabled MODE temp buffer with CONTENTS.
BODY is code to be executed within the temp buffer.  Point is
always located at the beginning of buffer."
  (declare (indent 1) (debug t))
  `(ert-with-test-buffer-selected ()
     (,mode)
     (hs-minor-mode 1)
     (insert ,contents)
     (goto-char (point-min))
     ,@body))

(defun hideshow-tests-look-at (string &optional num restore-point)
  "Move point at beginning of STRING in the current buffer.
Optional argument NUM defaults to 1 and is an integer indicating
how many occurrences must be found, when positive the search is
done forwards, otherwise backwards.  When RESTORE-POINT is
non-nil the point is not moved but the position found is still
returned.  When searching forward and point is already looking at
STRING, it is skipped so the next STRING occurrence is selected."
  (let* ((num (or num 1))
         (starting-point (point))
         (string (regexp-quote string))
         (search-fn (if (> num 0) #'re-search-forward #'re-search-backward))
         (deinc-fn (if (> num 0) #'1- #'1+))
         (found-point))
    (prog2
        (catch 'exit
          (while (not (= num 0))
            (when (and (> num 0)
                       (looking-at string))
              ;; Moving forward and already looking at STRING, skip it.
              (forward-char (length (match-string-no-properties 0))))
            (and (not (funcall search-fn string nil t))
                 (throw 'exit t))
            (when (> num 0)
              ;; `re-search-forward' leaves point at the end of the
              ;; occurrence, move back so point is at the beginning
              ;; instead.
              (forward-char (- (length (match-string-no-properties 0)))))
            (setq
             num (funcall deinc-fn num)
             found-point (point))))
        found-point
      (and restore-point (goto-char starting-point)))))

(defun hideshow-tests-visible-string (&optional min max)
  "Return the buffer string excluding invisible overlays.
Argument MIN and MAX delimit the region to be returned and
default to `point-min' and `point-max' respectively."
  (let* ((min (or min (point-min)))
         (max (or max (point-max)))
         (buffer-contents (buffer-substring-no-properties min max))
         (overlays
          (sort (overlays-in min max)
                (lambda (a b)
                  (let ((overlay-end-a (overlay-end a))
                        (overlay-end-b (overlay-end b)))
                    (> overlay-end-a overlay-end-b))))))
    (with-temp-buffer
      (insert buffer-contents)
      (dolist (overlay overlays)
        (if (overlay-get overlay 'invisible)
            (delete-region (overlay-start overlay)
                           (overlay-end overlay))))
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun hideshow-tests-make-event-at (string)
  "Make dummy mouse event at beginning of STRING."
  (save-excursion
    (let ((pos (hideshow-tests-look-at string)))
      (vector
       `(S-mouse-2
         (,(get-buffer-window) ,pos (1 . 1) 0 nil ,pos (1 . 1)
          nil (1 . 1) (1 . 1)))))))

(ert-deftest hideshow-already-hidden-p-1 ()
  (let ((contents "
int
main()
{
  printf(\"Hello\\n\");
}
"))
    (hideshow-tests-with-temp-buffer
     c-mode
     contents
     (hideshow-tests-look-at "printf")
     (should (not (hs-already-hidden-p)))
     (hs-hide-block)
     (goto-char (point-min))
     (hideshow-tests-look-at "{")
     (should (hs-already-hidden-p))
     (forward-line -1)
     (should (not (hs-already-hidden-p)))
     (hideshow-tests-look-at "}")
     (should (hs-already-hidden-p))
     (forward-line)
     (should (not (hs-already-hidden-p))))))

(ert-deftest hideshow-hide-block-1 ()
  "Should hide current block."
  (let ((contents "
int
main()
{
  printf(\"Hello\\n\");
}
"))
    (hideshow-tests-with-temp-buffer
     c-mode
     contents
     (hideshow-tests-look-at "printf")
     (hs-hide-block)
     (should (string=
              (hideshow-tests-visible-string)
              "
int
main()
{}
"))
     (hs-show-block)
     (should (string= (hideshow-tests-visible-string) contents)))))

(ert-deftest hideshow-hide-all-1 ()
  "Should hide all blocks and comments."
  (let ((contents "
/*
   Comments
*/

int
main()
{
  sub();
}

void
sub()
{
  printf(\"Hello\\n\");
}
"))
    (hideshow-tests-with-temp-buffer
     c-mode
     contents
     (hs-hide-all)
     (should (string=
              (hideshow-tests-visible-string)
              "
/*

int
main()
{}

void
sub()
{}
"))
     (hs-show-all)
     (should (string= (hideshow-tests-visible-string) contents)))))

(ert-deftest hideshow-hide-all-2 ()
  "Should not hide comments when `hs-hide-comments-when-hiding-all' is nil."
  (let ((contents "
/*
   Comments
*/

int
main()
{
  sub();
}

void
sub()
{
  printf(\"Hello\\n\");
}
"))
    (hideshow-tests-with-temp-buffer
     c-mode
     contents
     (let ((hs-hide-comments-when-hiding-all nil))
       (hs-hide-all))
     (should (string=
              (hideshow-tests-visible-string)
              "
/*
   Comments
*/

int
main()
{}

void
sub()
{}
"))
     (hs-show-all)
     (should (string= (hideshow-tests-visible-string) contents)))))

(ert-deftest hideshow-hide-level-1 ()
  "Should hide 1st level blocks."
  (hideshow-tests-with-temp-buffer
   c-mode
   "
/*
   Comments
*/

int
main(int argc, char **argv)
{
  if (argc > 1) {
    printf(\"Hello\\n\");
  }
}
"
   (hs-hide-level 1)
   (should (string=
            (hideshow-tests-visible-string)
            "
/*
   Comments
*/

int
main(int argc, char **argv)
{}
"))))

(ert-deftest hideshow-hide-level-2 ()
  "Should hide 2nd level blocks."
  (hideshow-tests-with-temp-buffer
   c-mode
   "
/*
   Comments
*/

int
main(int argc, char **argv)
{
  if (argc > 1) {
    printf(\"Hello\\n\");
  }
}
"
   (hs-hide-level 2)
   (should (string=
            (hideshow-tests-visible-string)
            "
/*
   Comments
*/

int
main(int argc, char **argv)
{
  if (argc > 1) {}
}
"))))

(ert-deftest hideshow-toggle-hiding-1 ()
  "Should toggle hiding/showing of a block."
  (let ((contents "
int
main()
{
  printf(\"Hello\\n\");
}
"))
    (hideshow-tests-with-temp-buffer
     c-mode
     contents
     (hideshow-tests-look-at "printf")
     (hs-toggle-hiding)
     (should (string=
              (hideshow-tests-visible-string)
              "
int
main()
{}
"))
     (hs-toggle-hiding)
     (should (string= (hideshow-tests-visible-string) contents)))))

(ert-deftest hideshow-mouse-toggle-hiding-1 ()
  "Should toggle hiding/showing of a block by mouse events."
  (let ((contents "
int
main()
{
  printf(\"Hello\\n\");
}
")
        (hidden "
int
main()
{}
")
        (call-at (lambda (str)
                   (let* ((events (hideshow-tests-make-event-at str))
                          (last-nonmenu-event (aref events 0)))
                     (call-interactively #'hs-toggle-hiding nil events)))))
    (hideshow-tests-with-temp-buffer-selected
     c-mode
     contents
     ;; Should not hide the block when clicked outside of the block.
     (funcall call-at "int")
     (should (string= (hideshow-tests-visible-string) contents))
     ;; Should hide the block when clicked inside of the block.
     (goto-char (point-min))
     (funcall call-at "printf")
     (should (string= (hideshow-tests-visible-string) hidden))
     ;; Should not show the block when clicked outside of the block.
     (goto-char (point-min))
     (funcall call-at "int")
     (should (string= (hideshow-tests-visible-string) hidden))
     ;; Should show the block when clicked inside of the block.
     (goto-char (point-min))
     (funcall call-at "}")
     (should (string= (hideshow-tests-visible-string) contents)))))

(provide 'hideshow-tests)

;;; hideshow-tests.el ends here
