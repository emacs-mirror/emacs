;;; timeout.el --- Throttle or debounce Elisp functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026 Free Software Foundation, Inc.

;; Author: Karthik Chikmagalur <karthikchikmagalur@gmail.com>
;; Keywords: convenience, extensions
;; Version: 2.1
;; Package-Requires: ((emacs "24.4"))
;; URL: https://github.com/karthink/timeout

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; timeout is a small Elisp library that provides higher order functions to
;; throttle or debounce Elisp functions.  This is useful for corralling
;; over-eager code that:
;; (i) is slow and blocks Emacs, and
;; (ii) does not provide customization options to limit how often it runs,
;;
;; To throttle a function FUNC to run no more than once every 2 seconds, run
;; (timeout-throttle 'func 2.0)
;;
;; To debounce a function FUNC to run after a delay of 0.3 seconds, run
;; (timeout-debounce 'func 0.3)
;;
;; To create a new throttled or debounced version of FUNC instead, run
;;
;; (timeout-throttled-func 'func 2.0)
;; (timeout-debounced-func 'func 0.3)
;;
;; You can bind this via fset or defalias:
;;
;; (defalias 'throttled-func (timeout-throttled-func 'func 2.0))
;; (fset     'throttled-func (timeout-throttled-func 'func 2.0))
;;
;; Dynamic duration is supported by passing a symbol or function instead of
;; a number:
;;
;; (defvar my-timeout 1.5)
;; (timeout-throttle 'func 'my-timeout)  ; uses value of my-timeout
;; (timeout-throttle 'func (lambda () (if busy-p 0.1 2.0)))  ; conditional
;;
;; The interactive spec and documentation of FUNC is carried over to the new
;; function.

;;; Code:
(require 'nadvice)

(defsubst timeout--eval-value (value)
  "Eval a VALUE.
If value is a function (either lambda or a callable symbol), eval the
function (with no argument) and return the result.  Else if value is a
symbol, return its value.  Else return itself."
  (cond ((numberp value) value)
        ((functionp value) (funcall value))
        ((and (symbolp value) (boundp value)) (symbol-value value))
        (t (error "Invalid value %s" value))))

(defun timeout--throttle-advice (&optional timeout)
  "Return a function that throttles its argument function.

For the meaning of TIMEOUT see `timeout-throttle'.

When FUNC does not run because of the throttle, the result from the
previous successful call is returned.

This is intended for use as function advice."
  (let ((throttle-timer)
        (timeout-value (or timeout 1.0))
        (result))
    (lambda (orig-fn &rest args)
      "Throttle calls to this function."
      (progn
        (unless (and throttle-timer (timerp throttle-timer))
          (setq result (apply orig-fn args))
          (setq throttle-timer
                (run-with-timer
                 (timeout--eval-value timeout-value) nil
                 (lambda ()
                   (cancel-timer throttle-timer)
                   (setq throttle-timer nil)))))
        result))))

(defun timeout--debounce-advice (&optional delay default)
  "Return a function that debounces its argument function.

For the meaning of DELAY see `timeout-debounce'.

The function returns immediately with value DEFAULT when called the
first time.  On future invocations, the result from the previous call is
returned.

This is intended for use as function advice."
  (let ((debounce-timer nil)
        (delay-value (or delay 0.50)))
    (lambda (orig-fn &rest args)
      "Debounce calls to this function."
      (prog1 default
        (if (timerp debounce-timer)
            (timer-set-idle-time debounce-timer (timeout--eval-value delay-value))
          (setq debounce-timer
                (run-with-idle-timer
                 (timeout--eval-value delay-value) nil
                 (lambda (buf)
                   (cancel-timer debounce-timer)
                   (setq debounce-timer nil)
                   (setq default
                         (if (buffer-live-p buf)
                             (with-current-buffer buf
                               (apply orig-fn args))
                           (apply orig-fn args))))
                 (current-buffer))))))))

;;;###autoload
(defun timeout-debounce (func &optional delay default)
  "Debounce FUNC by making it run DELAY seconds after it is called.

This advises FUNC, when called (interactively or from code), to
run after DELAY seconds.   If FUNC is called again within this time,
the timer is reset.

DELAY defaults to 0.5 seconds.  DELAY can be a number, a symbol (whose
value is a number), or a function (that evaluates to a number).  When
passed a symbol or function, it is evaluated at runtime for dynamic
duration.  Using a delay of 0 removes any debounce advice.

The function returns immediately with value DEFAULT when called the
first time.  On future invocations, the result from the previous call is
returned."
  (if (and delay (eq delay 0))
      (advice-remove func 'debounce)
    (advice-add func :around (timeout--debounce-advice delay default)
                '((name . debounce)
                  (depth . -99)))))

;;;###autoload
(defun timeout-throttle (func &optional throttle)
  "Make FUNC run no more frequently than once every THROTTLE seconds.

THROTTLE defaults to 1 second.  THROTTLE can be a number, a symbol (whose
value is a number), or a function (that evaluates to a number).  When
passed a symbol or function, it is evaluated at runtime for dynamic
duration.  Using a throttle of 0 removes any throttle advice.

When FUNC does not run because of the throttle, the result from the
previous successful call is returned."
  (if (and throttle (eq throttle 0))
      (advice-remove func 'throttle)
    (advice-add func :around (timeout--throttle-advice throttle)
                '((name . throttle)
                  (depth . -98)))))

;;;###autoload
(defun timeout-throttled-func (func &optional throttle)
  "Return a throttled version of function FUNC.

The throttled function runs no more frequently than once every THROTTLE
seconds.  THROTTLE defaults to 1 second.  THROTTLE can be a number, a
symbol (whose value is a number), or a function (that evaluates to a
number).  When passed a symbol or function, it is evaluated at runtime
for dynamic duration.

When FUNC does not run because of the throttle, the result from the
previous successful call is returned."
  (let ((throttle-timer nil)
        (throttle-value (or throttle 1))
        (result))
    (if (commandp func)
        ;; INTERACTIVE version
        (lambda (&rest args)
          (:documentation
           (concat
            (documentation func)
            "\n\nThrottle calls to this function"))
          (interactive (advice-eval-interactive-spec
                        (cadr (interactive-form func))))
          (progn
            (unless (and throttle-timer (timerp throttle-timer))
              (setq result (apply func args))
              (setq throttle-timer
                    (run-with-timer
                     (timeout--eval-value throttle-value) nil
                     (lambda ()
                       (cancel-timer throttle-timer)
                       (setq throttle-timer nil)))))
            result))
      ;; NON-INTERACTIVE version
      (lambda (&rest args)
        (:documentation
         (concat
          (documentation func)
          "\n\nThrottle calls to this function"))
        (progn
          (unless (and throttle-timer (timerp throttle-timer))
            (setq result (apply func args))
            (setq throttle-timer
             (run-with-timer
                   (timeout--eval-value throttle-value) nil
                   (lambda ()
                     (cancel-timer throttle-timer)
                     (setq throttle-timer nil)))))
          result)))))

;;;###autoload
(defun timeout-debounced-func (func &optional delay default)
  "Return a debounced version of function FUNC.

The debounced function runs DELAY seconds after it is called.  DELAY
defaults to 0.5 seconds.  DELAY can be a number, a symbol (whose value
is a number), or a function (that evaluates to a number).  When passed
a symbol or function, it is evaluated at runtime for dynamic duration.

The function returns immediately with value DEFAULT when called the
first time.  On future invocations, the result from the previous call is
returned."
  (let ((debounce-timer nil)
        (delay-value (or delay 0.50)))
    (if (commandp func)
        ;; INTERACTIVE version
        (lambda (&rest args)
          (:documentation
           (concat
            (documentation func)
            "\n\nDebounce calls to this function"))
          (interactive (advice-eval-interactive-spec
                        (cadr (interactive-form func))))
          (prog1 default
            (if (timerp debounce-timer)
                (timer-set-idle-time debounce-timer (timeout--eval-value delay-value))
              (setq debounce-timer
                    (run-with-idle-timer
                     (timeout--eval-value delay-value) nil
                     (lambda (buf)
                       (cancel-timer debounce-timer)
                       (setq debounce-timer nil)
                       (setq default
                             (if (buffer-live-p buf)
                                 (with-current-buffer buf
                                   (apply func args))
                               (apply func args))))
                     (current-buffer))))))
      ;; NON-INTERACTIVE version
      (lambda (&rest args)
        (:documentation
         (concat
          (documentation func)
          "\n\nDebounce calls to this function"))
        (prog1 default
          (if (timerp debounce-timer)
              (timer-set-idle-time debounce-timer (timeout--eval-value delay-value))
            (setq debounce-timer
                  (run-with-idle-timer
                   (timeout--eval-value delay-value) nil
                   (lambda (buf)
                     (cancel-timer debounce-timer)
                     (setq debounce-timer nil)
                     (setq default
                           (if (buffer-live-p buf)
                               (with-current-buffer buf
                                 (apply func args))
                             (apply func args))))
                   (current-buffer)))))))))

(provide 'timeout)
;;; timeout.el ends here
