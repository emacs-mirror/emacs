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
			   "cyrillic-yawerty")
  (setq primary-language "Cyrillic"))

;; Display 

;; Written by Valery Alexeev <valery@math.uga.edu>.

(defun standard-display-cyrillic-translit (cyrillic-language)
  "Display a cyrillic buffer using a transliteration.
For readability, the table is slightly
different from the one used for the input method `cyrillic-translit'.

The argument specifies which language you are using;
that affects the choice of transliterations slightly.
Possible values are `ukranian', `bulgarian' or t (any other language).
If the argument is nil, we return the display table to its standard state."
  (if (null cyrillic-language)
      (setq standard-display-table (make-display-table))
    (aset standard-display-table ?Ğ  [?a])
    (aset standard-display-table ?Ñ  [?b])
    (aset standard-display-table ?Ò  [?v])
    (aset standard-display-table ?Ó  [?g])
    (aset standard-display-table ?Ô  [?d])
    (aset standard-display-table ?Õ  [?e])
    (aset standard-display-table ?ñ  [?y?o])
    (aset standard-display-table ?Ö  [?z?h])
    (aset standard-display-table ?×  [?z])
    (aset standard-display-table ?Ø  [?i])
    (aset standard-display-table ?Ù  [?j])
    (aset standard-display-table ?Ú  [?k])
    (aset standard-display-table ?Û  [?l])
    (aset standard-display-table ?Ü  [?m])
    (aset standard-display-table ?İ  [?n])
    (aset standard-display-table ?Ş  [?o])
    (aset standard-display-table ?ß  [?p])
    (aset standard-display-table ?à  [?r])
    (aset standard-display-table ?á  [?s])
    (aset standard-display-table ?â  [?t])
    (aset standard-display-table ?ã  [?u])
    (aset standard-display-table ?ä  [?f])
    (aset standard-display-table ?å  [?k?h])
    (aset standard-display-table ?æ  [?t?s])
    (aset standard-display-table ?ç  [?c?h])
    (aset standard-display-table ?è  [?s?h])
    (aset standard-display-table ?é  [?s?c?h])
    (aset standard-display-table ?ê  [?~])
    (aset standard-display-table ?ë  [?y])
    (aset standard-display-table ?ì  [?'])
    (aset standard-display-table ?í  [?e?'])
    (aset standard-display-table ?î  [?y?u])
    (aset standard-display-table ?ï  [?y?a])
  
    (aset standard-display-table ?°  [?A])
    (aset standard-display-table ?±  [?B])
    (aset standard-display-table ?²  [?V])
    (aset standard-display-table ?³  [?G])
    (aset standard-display-table ?´  [?D])
    (aset standard-display-table ?µ  [?E])
    (aset standard-display-table ?¡  [?Y?o])
    (aset standard-display-table ?¶  [?Z?h])
    (aset standard-display-table ?·  [?Z])
    (aset standard-display-table ?¸  [?I])
    (aset standard-display-table ?¹  [?J])
    (aset standard-display-table ?º  [?K])
    (aset standard-display-table ?\»  [?L])
    (aset standard-display-table ?¼  [?M])
    (aset standard-display-table ?½  [?N])
    (aset standard-display-table ?¾  [?O])
    (aset standard-display-table ?¿  [?P])
    (aset standard-display-table ?À  [?R])
    (aset standard-display-table ?Á  [?S])
    (aset standard-display-table ?Â  [?T])
    (aset standard-display-table ?Ã  [?U])
    (aset standard-display-table ?Ä  [?F])
    (aset standard-display-table ?Å  [?K?h])
    (aset standard-display-table ?Æ  [?T?s])
    (aset standard-display-table ?Ç  [?C?h])
    (aset standard-display-table ?È  [?S?h])
    (aset standard-display-table ?É  [?S?c?h])
    (aset standard-display-table ?Ê  [?~])
    (aset standard-display-table ?Ë  [?Y])
    (aset standard-display-table ?Ì  [?'])
    (aset standard-display-table ?Í  [?E?'])
    (aset standard-display-table ?Î  [?Y?u])
    (aset standard-display-table ?Ï  [?Y?a])

    (aset standard-display-table ?ô  [?i?e])
    (aset standard-display-table ?÷  [?i])
    (aset standard-display-table ?ş  [?u])
    (aset standard-display-table ?ò  [?d?j])
    (aset standard-display-table ?û  [?c?h?j])
    (aset standard-display-table ?ó  [?g?j])
    (aset standard-display-table ?õ  [?s])
    (aset standard-display-table ?ü  [?k])
    (aset standard-display-table ?ö  [?i])
    (aset standard-display-table ?ø  [?j])
    (aset standard-display-table ?ù  [?l?j])
    (aset standard-display-table ?ú  [?n?j])
    (aset standard-display-table ?ÿ  [?d?z])

    (aset standard-display-table ?¤  [?Y?e])
    (aset standard-display-table ?§  [?Y?i])
    (aset standard-display-table ?®  [?U])
    (aset standard-display-table ?¢  [?D?j])
    (aset standard-display-table ?\«  [?C?h?j])
    (aset standard-display-table ?£  [?G?j])
    (aset standard-display-table ?¥  [?S])
    (aset standard-display-table ?¬  [?K])
    (aset standard-display-table ?¦  [?I])
    (aset standard-display-table ?¨  [?J])
    (aset standard-display-table ?©  [?L?j])
    (aset standard-display-table ?ª  [?N?j])
    (aset standard-display-table ?¯  [?D?j])

    (when (eq cyrillic-language 'bulgarian)
      (aset standard-display-table ?é [?s?h?t])
      (aset standard-display-table ?É [?S?h?t])
      (aset standard-display-table ?î [?i?u])
      (aset standard-display-table ?Î [?I?u])
      (aset standard-display-table ?ï [?i?a])
      (aset standard-display-table ?Ï [?I?a]))

    (when (eq cyrillic-language 'ukranian) ; based on the official
					; transliteration table
      (aset standard-display-table ?Ø [?y])
      (aset standard-display-table ?¸ [?Y])
      (aset standard-display-table ?Ù [?i])
      (aset standard-display-table ?¹ [?Y])
      (aset standard-display-table ?î [?i?u])
      (aset standard-display-table ?ï [?i?a]))))

;;
(provide 'cyril-util)

;;; cyril-util.el ends here
