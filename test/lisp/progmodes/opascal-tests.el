;;; opascal-tests.el --- tests for opascal.el  -*- lexical-binding: t -*-

;; Copyright (C) 2020-2024 Free Software Foundation, Inc.

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

;;; Code:

(require 'ert)
(require 'opascal)

(ert-deftest opascal-indent-bug-36348 ()
  (with-temp-buffer
    (opascal-mode)
    (let ((orig "{ -*- opascal -*- }

procedure Toto ();
begin
   for i := 0 to 1 do
      Write (str.Chars[i]);

   // bug#36348
   for var i := 0 to 1 do
      Write (str.Chars[i]);

end;
"))
      (insert orig)
      (indent-region (point-min) (point-max))
      (should (equal (buffer-string) orig)))))

(provide 'opascal-tests)

;;; opascal-tests.el ends here
