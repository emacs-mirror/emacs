;;; philippine.el --- Quail package for inputting Philippine characters  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024 Free Software Foundation, Inc.

;; Author: समीर सिंह Sameer Singh <lumarzeli30@gmail.com>
;; Keywords: multilingual, input method, i18n, Philippines

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Input methods for Philippine languages.

;;; Code:

(require 'quail)

;; This input method supports languages like Tagalog, Hanunoo, Buhid and
;; Tagbanwa, using the Baybayin script.
(quail-define-package
 "tagalog" "Tagalog" "ᜊ" nil "Tagalog phonetic input method."
 nil t t t t nil nil nil nil nil t)

(quail-define-rules
 ("q"  ?₱)
 ("w"  ?ᜏ)
 ("r"  ?ᜍ)
 ("R"  ?ᜟ)
 ("t"  ?ᜆ)
 ("y"  ?ᜌ)
 ("u"  ?ᜓ)
 ("U"  ?ᜂ)
 ("i"  ?ᜒ)
 ("I"  ?ᜁ)
 ("p"  ?ᜉ)
 ("a"  ?ᜀ)
 ("s"  ?ᜐ)
 ("d"  ?ᜇ)
 ("f"  ?᜔)
 ("g"  ?ᜄ)
 ("h"  ?ᜑ)
 ("j"  ?᜵)
 ("J"  ?᜶)
 ("k"  ?ᜃ)
 ("l"  ?ᜎ)
 ("v"  ?᜕)
 ("b"  ?ᜊ)
 ("n"  ?ᜈ)
 ("N"  ?ᜅ)
 ("m"  ?ᜋ))

(quail-define-package
 "hanunoo" "Hanunoo" "ᜱ" nil "Hanunoo phonetic input method."
 nil t t t t nil nil nil nil nil t)

(quail-define-rules
 ("q"  ?₱)
 ("w"  ?ᜯ)
 ("r"  ?ᜭ)
 ("t"  ?ᜦ)
 ("y"  ?ᜬ)
 ("u"  ?ᜳ)
 ("U"  ?ᜢ)
 ("i"  ?ᜲ)
 ("I"  ?ᜡ)
 ("p"  ?ᜩ)
 ("a"  ?ᜠ)
 ("s"  ?ᜰ)
 ("d"  ?ᜧ)
 ("f"  ?᜴)
 ("g"  ?ᜤ)
 ("h"  ?ᜱ)
 ("j"  ?᜵)
 ("J"  ?᜶)
 ("k"  ?ᜣ)
 ("l"  ?ᜮ)
 ("b"  ?ᜪ)
 ("n"  ?ᜨ)
 ("N"  ?ᜥ)
 ("m"  ?ᜫ))

(quail-define-package
 "buhid" "Buhid" "ᝊᝓ" nil "Buhid phonetic input method."
 nil t t t t nil nil nil nil nil t)

(quail-define-rules
 ("q"  ?₱)
 ("w"  ?ᝏ)
 ("r"  ?ᝍ)
 ("t"  ?ᝆ)
 ("y"  ?ᝌ)
 ("u"  ?ᝓ)
 ("U"  ?ᝂ)
 ("i"  ?ᝒ)
 ("I"  ?ᝁ)
 ("p"  ?ᝉ)
 ("a"  ?ᝀ)
 ("s"  ?ᝐ)
 ("d"  ?ᝇ)
 ("g"  ?ᝄ)
 ("h"  ?ᝑ)
 ("j"  ?᜵)
 ("J"  ?᜶)
 ("k"  ?ᝃ)
 ("l"  ?ᝎ)
 ("b"  ?ᝊ)
 ("n"  ?ᝈ)
 ("N"  ?ᝅ)
 ("m"  ?ᝋ))

(quail-define-package
 "tagbanwa" "Tagbanwa" "ᝦ" nil "Tagbanwa phonetic input method."
 nil t t t t nil nil nil nil nil t)

(quail-define-rules
 ("q"  ?₱)
 ("w"  ?ᝯ)
 ("t"  ?ᝦ)
 ("y"  ?ᝬ)
 ("u"  ?ᝳ)
 ("U"  ?ᝢ)
 ("i"  ?ᝲ)
 ("I"  ?ᝡ)
 ("p"  ?ᝩ)
 ("a"  ?ᝠ)
 ("s"  ?ᝰ)
 ("d"  ?ᝧ)
 ("g"  ?ᝤ)
 ("j"  ?᜵)
 ("J"  ?᜶)
 ("k"  ?ᝣ)
 ("l"  ?ᝮ)
 ("b"  ?ᝪ)
 ("n"  ?ᝨ)
 ("N"  ?ᝥ)
 ("m"  ?ᝫ))

(provide 'philippine)
;;; philippine.el ends here
