;;; simple-single.el --- A single-file package with no dependencies

;; Author: J. R. Hacker <jrh@example.com>
;; Version: 1.4
;; Keywords: frobnicate

;;; Commentary:

;; This package provides a minor mode to frobnicate and/or bifurcate
;; any flanges you desire. To activate it, type "C-M-r M-3 butterfly"
;; and all your dreams will come true.
;;
;; This is a new, updated version.

;;; Code:

(defgroup simple-single nil "Simply a file"
  :group 'lisp)

(defcustom simple-single-super-sunday nil
  "How great is this?
Default changed to nil."
  :type 'boolean
  :group 'simple-single
  :package-version "1.4")

(defvar simple-single-sudo-sandwich nil
  "Make a sandwich?")

;;;###autoload
(define-minor-mode simple-single-mode
  "It does good things to stuff")

(provide 'simple-single)

;;; simple-single.el ends here
