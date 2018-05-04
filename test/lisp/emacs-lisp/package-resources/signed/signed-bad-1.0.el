;;; signed-bad.el --- A single-file package with bad signature

;; Author: J. R. Hacker <jrh@example.com>
;; Version: 1.0
;; Keywords: frobnicate
;; URL: http://doodles.au

;;; Commentary:

;; This package provides a minor mode to frobnicate and/or bifurcate
;; any flanges you desire. To activate it, type "C-M-r M-3 butterfly"
;; and all your dreams will come true.

;;; Code:

(defgroup signed-bad nil "Simply a file"
  :group 'lisp)

(defcustom signed-bad-super-sunday t
  "How great is this?"
  :type 'boolean
  :group 'signed-bad)

(defvar signed-bad-sudo-sandwich nil
  "Make a sandwich?")

;;;###autoload
(define-minor-mode signed-bad-mode
  "It does good things to stuff")

(provide 'signed-bad)

;;; signed-bad.el ends here
