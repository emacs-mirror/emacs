;;; po-tests.el --- Tests for po.el                  -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2024 Free Software Foundation, Inc.

;; Author: Simen Heggestøyl <simenheg@gmail.com>
;; Keywords:

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

;;

;;; Code:

(require 'po)
(require 'ert)

(defconst po-tests--buffer-string
  "# Norwegian bokmål translation of the GIMP.
# Copyright (C) 1999-2001 Free Software Foundation, Inc.
#
msgid \"\"
msgstr \"\"
\"Project-Id-Version: gimp 2.8.5\\n\"
\"Report-Msgid-Bugs-To: https://gitlab.gnome.org/GNOME/gimp/issues\\n\"
\"POT-Creation-Date: 2013-05-27 14:57+0200\\n\"
\"PO-Revision-Date: 2013-05-27 15:21+0200\\n\"
\"Language: nb\\n\"
\"MIME-Version: 1.0\\n\"
\"Content-Type: text/plain; charset=UTF-8\\n\"
\"Content-Transfer-Encoding: 8bit\\n\"
\"Plural-Forms: nplurals=2; plural=(n != 1);\\n\"

#: ../desktop/gimp.desktop.in.in.h:1 ../app/about.h:26
msgid \"GNU Image Manipulation Program\"
msgstr \"GNU bildebehandlingsprogram\"
")

(ert-deftest po-tests-find-charset ()
  (with-temp-buffer
    (insert po-tests--buffer-string)
    (should (equal (po-find-charset (cons nil (current-buffer)))
                   "UTF-8"))))

(ert-deftest po-tests-find-file-coding-system-guts ()
  (with-temp-buffer
    (insert po-tests--buffer-string)
    (should (equal (po-find-file-coding-system-guts
                    'insert-file-contents
                    (cons "*tmp*" (current-buffer)))
                   '(utf-8 . nil)))))

(provide 'po-tests)
;;; po-tests.el ends here
