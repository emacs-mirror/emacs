;;; url-future.el --- general futures facility for url.el

;; Copyright (C) 2011  Free Software Foundation, Inc.

;; Author: Teodor Zlatanov <tzz@lifelogs.com>
;; Keywords: data

;; This file is part of GNU Emacs.
;;
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

;; Make a url-future (basically a defstruct):
;; (make-url-future :value (lambda () (calculation goes here))
;;   :callback (lambda (future) (use future on success))
;;   :errorback (lambda (future &rest error) (error handler)))

;; Then either call it with `url-future-call' or cancel it with
;; `url-future-cancel'.  Generally the functions will return the
;; future itself, not the value it holds.  Also the functions will
;; throw a url-future-already-done error if you try to call or cancel
;; a future more than once.

;; So, to get the value:
;; (when (url-future-completed-p future) (url-future-value future))

;; See the ERT tests and the code for futher details.

;;; Code:

(eval-when-compile (require 'cl))
(eval-when-compile (require 'ert))

(defstruct url-future callback errorback status value)

(defmacro url-future-done-p (url-future)
  `(url-future-status ,url-future))

(defmacro url-future-completed-p (url-future)
  `(eq (url-future-status ,url-future) t))

(defmacro url-future-errored-p (url-future)
  `(eq (url-future-status ,url-future) 'error))

(defmacro url-future-cancelled-p (url-future)
  `(eq (url-future-status ,url-future) 'cancel))

(defun url-future-finish (url-future &optional status)
  (if (url-future-done-p url-future)
      (signal 'error 'url-future-already-done)
    (setf (url-future-status url-future) (or status t))
    ;; the status must be such that the future was completed
    ;; to run the callback
    (when (url-future-completed-p url-future)
      (funcall (or (url-future-callback url-future) 'ignore)
               url-future))
    url-future))

(defun url-future-errored (url-future errorcons)
  (if (url-future-done-p url-future)
      (signal 'error 'url-future-already-done)
    (setf (url-future-status url-future) 'error)
    (setf (url-future-value url-future) errorcons)
    (funcall (or (url-future-errorback url-future) 'ignore)
             url-future errorcons)))

(defun url-future-call (url-future)
  (if (url-future-done-p url-future)
      (signal 'error 'url-future-already-done)
    (let ((ff (url-future-value url-future)))
      (when (functionp ff)
        (condition-case catcher
            (setf (url-future-value url-future)
                  (funcall ff))
          (error (url-future-errored url-future catcher)))
        (url-future-value url-future)))
    (if (url-future-errored-p url-future)
        url-future
      (url-future-finish url-future))))

(defun url-future-cancel (url-future)
  (if (url-future-done-p url-future)
      (signal 'error 'url-future-already-done)
    (url-future-finish url-future 'cancel)))

(ert-deftest url-future-test ()
  (let* (saver
	 (text "running future")
         (good (make-url-future :value (lambda () (format text))
                                :callback (lambda (f) (set 'saver f))))
         (bad (make-url-future :value (lambda () (/ 1 0))
                               :errorback (lambda (&rest d) (set 'saver d))))
         (tocancel (make-url-future :value (lambda () (/ 1 0))
                                    :callback (lambda (f) (set 'saver f))
                                    :errorback (lambda (&rest d)
                                                 (set 'saver d)))))
    (should (equal good (url-future-call good)))
    (should (equal good saver))
    (should (equal text (url-future-value good)))
    (should (url-future-completed-p good))
    (should-error (url-future-call good))
    (setq saver nil)
    (should (equal bad (url-future-call bad)))
    (should-error (url-future-call bad))
    (should (equal saver (list bad '(arith-error))))
    (should (url-future-errored-p bad))
    (setq saver nil)
    (should (equal (url-future-cancel tocancel) tocancel))
    (should-error (url-future-call tocancel))
    (should (null saver))
    (should (url-future-cancelled-p tocancel))))

(provide 'url-future)
;;; url-future.el ends here
