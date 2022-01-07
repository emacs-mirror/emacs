;;; elide-head-tests.el --- Tests for elide-head.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2022 Free Software Foundation, Inc.

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

(require 'elide-head)
(require 'ert)

(ert-deftest elide-head-tests-elide-head ()
  (let ((elide-head-headers-to-hide '(("START" . "END"))))
    (with-temp-buffer
      (insert "foo\nSTART\nHIDDEN\nEND\nbar")
      (elide-head)
      (let ((o (car (overlays-at 14))))
        (should (= (overlay-start o) 10))
        (should (= (overlay-end o) 21))
        (should (overlay-get o 'invisible))
        (should (overlay-get o 'evaporate))))))

(ert-deftest elide-head-tests-elide-head-with-prefix-arg ()
  (let ((elide-head-headers-to-hide '(("START" . "END"))))
    (with-temp-buffer
      (insert "foo\nSTART\nHIDDEN\nEND\nbar")
      (elide-head)
      (should (overlays-at 14))
      (elide-head t)
      (should-not (overlays-at 14)))))

(ert-deftest elide-head-tests-show ()
  (let ((elide-head-headers-to-hide '(("START" . "END"))))
    (with-temp-buffer
      (insert "foo\nSTART\nHIDDEN\nEND\nbar")
      (elide-head)
      (should (overlays-at 14))
      (elide-head-show)
      (should-not (overlays-at 14)))))

(defmacro elide-head--add-test (name text search-str)
  `(ert-deftest ,(intern (format "elide-head--test-headers-to-hide/%s" name)) ()
     (with-temp-buffer
       (insert ,text)
       (elide-head)
       (goto-char (point-min))
       (re-search-forward ,search-str)
       (let ((o (car (overlays-at (match-beginning 0)))))
         (should (overlayp o))
         (should (overlay-get o 'invisible))
         (should (overlay-get o 'evaporate))))))


;;; GPLv3

;; from Emacs
(elide-head--add-test gpl3-1 "\
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
" "GNU Emacs is distributed in the hope that")

;; from libtorrent
(elide-head--add-test gpl3-2 "\
    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
" "This library is distributed in the hope that")

;; from notmuch
(elide-head--add-test gpl3-3 "\
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see https://www.gnu.org/licenses/ .
" "This program is distributed in the hope that")

;; from fribok
(elide-head--add-test gpl3-4 "\
/***************************************************************************
 *   Copyright (C) 2007, 2009 by J. Random Hacker <jrh@example.org>        *
 *                                                                         *
 *   This program is free software: you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation, either version 3 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program.  If not, see <http://www.gnu.org/licenses/>  *.
 *                                                                         *
 ***************************************************************************/
" "This program is distributed in the hope that")


;;; GPLv2

;; from jackmeter
(elide-head--add-test gpl2-1 "\
        This program is free software; you can redistribute it and/or
        modify it under the terms of the GNU General Public License
        as published by the Free Software Foundation; either version 2
        of the License, or (at your option) any later version.

        This program is distributed in the hope that it will be useful,
        but WITHOUT ANY WARRANTY; without even the implied warranty of
        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
        GNU General Public License for more details.

        You should have received a copy of the GNU General Public License
        along with this program; if not, write to the Free Software
        Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
" "This program is distributed in the hope that")

(provide 'elide-head-tests)
;;; elide-head-tests.el ends here
