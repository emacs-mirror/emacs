;;; slovak.el --- Support for Slovak

;; Copyright (C) 1998 Free Software Foundation.

;; Authors:    Tibor ,B)(Bimko <tibor.simko@fmph.uniba.sk>,
;;             Milan Zamazal <pdm@fi.muni.cz>
;; Maintainer: Milan Zamazal <pdm@fi.muni.cz>
;; Keywords: multilingual, Slovak

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Slovak ISO 8859-2 environment.

;;; Code:

(defun setup-slovak-environment ()
  "Setup multilingual environment (MULE) for Slovak."
  (interactive)
  (funcall (get-language-info "Latin-2" 'setup-function))
  (setq default-input-method "slovak"))

(set-language-info-alist
 "Slovak" '((setup-function . setup-slovak-environment)
	    (charset . (ascii latin-iso8859-2))
	    (coding-system . (iso-8859-2))
	    (coding-priority . (iso-8859-2))
	    (sample-text . "Prajeme V,Ba(Bm pr,Bm(Bjemn,B}(B de,Br(B!")
	    (documentation . t))
 '("European"))

(provide 'slovak)

;; slovak.el ends here
