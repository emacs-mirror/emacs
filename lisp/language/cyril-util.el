;;; cyril-util.el ---  utilities for Cyrillic scripts

;; Copyright (C) 1997 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.

;; Keywords: mule, multilingual, Cyrillic

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

;;; Code:

;;;###autoload
(defun setup-cyrillic-environment ()
  "Setup multilingual environment (MULE) for Cyrillic users."
  (interactive)
  (setup-8-bit-environment "Cyrillic" 'cyrillic-iso8859-5 'cyrillic-iso-8bit
			   "cyrillic-yawerty"))
  (setq primary-language "Cyrillic"))

;;
(provide 'cyril-util)

;;; cyril-util.el ends here
