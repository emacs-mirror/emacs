;;; cyril-util.el ---  utilities for Cyrillic scripts

;; Copyright (C) 1997 Free Software Foundation, Inc.
;; Copyright (C) 1997 Electrotechnical Laboratory, JAPAN.

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
(defun setup-cyrillic-iso-environment ()
  "Setup multilingual environment (MULE) for Cyrillic ISO-8859-5 users."
  (interactive)
  (setup-8-bit-environment 'cyrillic-iso8859-5 'cyrillic-iso-8bit
			   '("Cyrillic" . "quail-yawerty"))
  (setq primary-language "Cyrillic"))

;;;###autoload
(defun setup-cyrillic-koi8-environment ()
  "Setup multilingual environment for Cyrillic (KOI8-R) users."
  (interactive)
  (setup-8-bit-environment 'cyrillic-iso8859-5 'cyrillic-koi8
			   '("Cyrillic" . "quail-yawerty"))

  (setq primary-language "Cyrillic"))

;;;###autoload
(defun setup-cyrillic-alternativnyj-environment ()
  "Setup multilingual environment for Cyrillic (ALTERNATIVNYJ) users."
  (interactive)
  (setup-8-bit-environment 'cyrillic-iso8859-5 'cyrillic-alternativnyj
			   '("Cyrillic" . "quail-yawerty"))

  (setq primary-language "Cyrillic"))

;;
(provide 'language/cyril-util)

;;; Local Variables:
;;; generated-autoload-file: "../loaddefs.el"
;;; End:
;;; cyril-util.el ends here
