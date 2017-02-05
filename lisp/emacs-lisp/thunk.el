;;; thunk.el --- Lazy form evaluation  -*- lexical-binding: t -*-

;; Copyright (C) 2015-2017 Free Software Foundation, Inc.

;; Author: Nicolas Petton <nicolas@petton.fr>
;; Keywords: sequences
;; Version: 1.0
;; Package: thunk

;; Maintainer: emacs-devel@gnu.org

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Thunk provides functions and macros to delay the evaluation of
;; forms.
;;
;; Use `thunk-delay' to delay the evaluation of a form, and
;; `thunk-force' to evaluate it. The result of the evaluation is
;; cached, and only happens once.
;;
;; Here is an example of a form which evaluation is delayed:
;;
;;     (setq delayed (thunk-delay (message "this message is delayed")))
;;
;; `delayed' is not evaluated until `thunk-force' is called, like the
;; following:
;;
;;    (thunk-force delayed)

;; Tests are located at test/automated/thunk-tests.el

;;; Code:

(defmacro thunk-delay (&rest body)
  "Delay the evaluation of BODY."
  (declare (debug t))
  (let ((forced (make-symbol "forced"))
        (val (make-symbol "val")))
    `(let (,forced ,val)
       (lambda (&optional check)
         (if check
             ,forced
           (unless ,forced
             (setf ,val (progn ,@body))
             (setf ,forced t))
           ,val)))))

(defun thunk-force (delayed)
  "Force the evaluation of DELAYED.
The result is cached and will be returned on subsequent calls
with the same DELAYED argument."
  (funcall delayed))

(defun thunk-evaluated-p (delayed)
  "Return non-nil if DELAYED has been evaluated."
  (funcall delayed t))

(provide 'thunk)
;;; thunk.el ends here
