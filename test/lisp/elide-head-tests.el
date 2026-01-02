;;; elide-head-tests.el --- Tests for elide-head.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2026 Free Software Foundation, Inc.

;; Author: Simen Heggestøyl <simenheg@gmail.com>

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

(require 'elide-head)
(require 'ert)
(require 'ert-x)

(ert-deftest elide-head-tests-elide-head-mode ()
  (let ((elide-head-headers-to-hide '(("START" . "END"))))
    (with-temp-buffer
      (insert "foo\nSTART\nHIDDEN\nEND\nbar")
      (elide-head-mode 1)
      (let ((o (car (overlays-at 14))))
        (should (= (overlay-start o) 10))
        (should (= (overlay-end o) 21))
        (should (overlay-get o 'invisible))
        (should (overlay-get o 'evaporate))))))

(ert-deftest elide-head-tests-elide-head-mode/enable-disable ()
  (let ((elide-head-headers-to-hide '(("START" . "END"))))
    (with-temp-buffer
      (insert "foo\nSTART\nHIDDEN\nEND\nbar")
      (elide-head-mode 1)
      (should (overlays-at 14))
      (elide-head-mode -1)
      (should-not (overlays-at 14)))))

(ert-deftest elide-head-tests-elide-head-mode/normal-mode ()
  (ert-with-temp-file fil
    (with-temp-file fil
      (insert "foo\nSTART\nHIDDEN\nEND\nbar"))
    (let ((elide-head-headers-to-hide '(("START" . "END")))
          (buf (find-file-noselect fil)))
      (save-excursion
        (unwind-protect
            (progn
              (set-buffer buf)
              (elide-head-mode 1)
              (should (= 1 (length (overlays-in (point-min) (point-max)))))
              (normal-mode)
              (should (= 0 (length (overlays-in (point-min) (point-max))))))
          (when buf (kill-buffer buf)))))))

(ert-deftest elide-head-tests-elide-head-mode/revert-buffer ()
  (ert-with-temp-file fil
    (with-temp-file fil
      (insert "foo\nSTART\nHIDDEN\nEND\nbar"))
    (let ((elide-head-headers-to-hide '(("START" . "END")))
          (buf (find-file-noselect fil)))
      (save-excursion
        (unwind-protect
            (progn
              (set-buffer buf)
              (elide-head-mode 1)
              (should (= 1 (length (overlays-in (point-min) (point-max)))))
              (revert-buffer nil t)
              (elide-head-mode 1)
              (should (= 1 (length (overlays-in (point-min) (point-max))))))
          (when buf (kill-buffer buf)))))))


(defmacro elide-head--add-test (name text search-str)
  `(ert-deftest ,(intern (format "elide-head--test-headers-to-hide/%s" name)) ()
     (with-temp-buffer
       (insert ,text)
       (elide-head-mode 1)
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

;; from mentor.el    [no "/" in the gnu.org URL]
(elide-head--add-test gpl3-5 "\
;; Mentor is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; Mentor is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Mentor.  If not, see <https://www.gnu.org/licenses>.
" "Mentor is distributed in the hope that")

;; from GnuTLS       [has a line break in snail mail address]
(elide-head--add-test gpl3-6 "\
# This file is part of GnuTLS.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301
# USA
" "This program is distributed in the hope that")

;; from GnuTLS       [has a different line break in snail mail address]
(elide-head--add-test gpl3-7 "\
# This file is part of GnuTLS.
#
# The GnuTLS is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public License
# as published by the Free Software Foundation; either version 2.1 of
# the License, or (at your option) any later version.
#
# The GnuTLS is distributed in the hope that it will be
# useful, but WITHOUT ANY WARRANTY; without even the implied warranty
# of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with GnuTLS; if not, write to the Free
# Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
# MA 02110-1301, USA
" "The GnuTLS is distributed in the hope that")

;; from GnuTLS       [has a typo in the 02111-1301 part]
(elide-head--add-test gpl3-8 "\
/* nettle, low-level cryptographics library
 *
 * Copyright (C) 2002 Niels Möller
 * Copyright (C) 2014 Red Hat
 *\s\s
 * The nettle library is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2.1 of the License, or (at your
 * option) any later version.
 *\s
 * The nettle library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *\s
 * You should have received a copy of the GNU Lesser General Public License
 * along with the nettle library; see the file COPYING.LIB.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02111-1301, USA.
 */
" "The nettle library is distributed in the hope that")

;; from GnuTLS-EXTRA  [has a different line break in snail mail address]
(elide-head--add-test gpl3-9 "\
# This file is part of GnuTLS-EXTRA.
#
# GnuTLS-extra is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation; either version 3 of the
# License, or (at your option) any later version.
#
# GnuTLS-extra is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GnuTLS-EXTRA; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
# 02110-1301, USA.
" "GnuTLS-extra is distributed in the hope that")


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


;;; Apache License

(elide-head--add-test apache1-1 "\
/*
 *  Copyright 2011-2016 The Pkcs11Interop Project
 *
 *  Licensed under the Apache License, Version 2.0 (the \"License\");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an \"AS IS\" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
" "Unless required by applicable law")



;;; Obsolete

(with-suppressed-warnings ((obsolete elide-head)
                           (obsolete elide-head-show))
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
        (should-not (overlays-at 14))))))

(provide 'elide-head-tests)
;;; elide-head-tests.el ends here
