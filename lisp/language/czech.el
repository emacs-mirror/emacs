;;; czech.el -- support for Czech

;; Copyright (C) 1998 Free Software Foundation.

;; Author:     Milan Zamazal <pdm@fi.muni.cz>
;; Maintainer: Milan Zamazal <pdm@fi.muni.cz>
;; Keywords: multilingual, Czech

;; Copying and warranty: GNU General Public License, version 2

;;; Commentary:

;; Czech ISO 8859-2 environment.

;;; Code:

(defun setup-czech-environment ()
  "Setup multilingual environment (MULE) for Czech."
  (interactive)
  (setup-8-bit-environment "Czech" 'latin-iso8859-2 'iso-8859-2)
  (load "latin-2"))

(set-language-info-alist
 "Czech" '((setup-function . setup-czech-environment)
	   (charset . (ascii latin-iso8859-2))
	   (coding-system . (iso-8859-2))
	   (tutorial . "TUTORIAL.cz")
	   (sample-text . "P,Bx(Bejeme v,Ba(Bm hezk,Bi(B r,Ba(Bno!")
	   (documentation . t)))

(provide 'czech)

;; czech.el ends here
