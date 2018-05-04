;;; simple-single.el --- A single-file package with no dependencies

;; Author: J. R. Hacker <jrh@example.com>
;; Version: 1.3
;; Keywords: frobnicate
;; URL: http://doodles.au

;;; Commentary:

;; This package provides a minor mode to frobnicate and/or bifurcate
;; any flanges you desire. To activate it, type "C-M-r M-3 butterfly"
;; and all your dreams will come true.

;;; Code:

(defgroup simple-single nil "Simply a file"
  :group 'lisp)

(defcustom simple-single-super-sunday t
  "How great is this?"
  :type 'boolean
  :group 'simple-single)

(defvar simple-single-sudo-sandwich nil
  "Make a sandwich?")

;;;###autoload
(define-minor-mode simple-single-mode
  "It does good things to stuff")

(provide 'simple-single)

;;; simple-single.el ends here
