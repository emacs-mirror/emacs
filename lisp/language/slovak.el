;;; slovak.el --- Support for Slovak

;; Copyright (C) 1998 Free Software Foundation.

;; Authors:    Tibor ,B)(Bimko <tibor.simko@fmph.uniba.sk>,
;;             Milan Zamazal <pdm@fi.muni.cz>
;; Maintainer: Milan Zamazal <pdm@fi.muni.cz>
;; Keywords: multilingual, Slovak

;; Copying and warranty: GNU General Public License, version 2

;;; Commentary:

;; Slovak ISO 8859-2 environment.

;;; Code:

(defun setup-slovak-environment ()
  "Setup multilingual environment (MULE) for Slovak."
  (interactive)
  (setup-8-bit-environment "Slovak" 'latin-iso8859-2 "slovak")
  (load "latin-2"))

(set-language-info-alist
 "Slovak" '((setup-function . setup-slovak-environment)
	    (charset . (ascii latin-iso8859-2))
	    (coding-system . (iso-latin-2))
	    (sample-text . "Prajeme V,Ba(Bm pr,Bm(Bjemn,B}(B de,Br(B!")
	    (documentation . t)))

(provide 'slovak)

;; slovak.el ends here
