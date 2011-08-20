;;; inversion-utest.el --- Unit tests for Inversion.
;;
;; Copyright (C) 2011 Eric M. Ludlam
;;
;; Author: Eric M. Ludlam <eric@siege-engine.com>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see http://www.gnu.org/licenses/.

;;; Commentary:
;;

(require 'inversion)

(defun inversion-unit-test ()
  "Test inversion to make sure it can identify different version strings."
  (interactive)
  (let ((c1 (inversion-package-version 'inversion))
        (c1i (inversion-package-incompatibility-version 'inversion))
        (c2 (inversion-decode-version  "1.3alpha2"))
        (c3 (inversion-decode-version  "1.3beta4"))
        (c4 (inversion-decode-version  "1.3 beta5"))
        (c5 (inversion-decode-version  "1.3.4"))
        (c6 (inversion-decode-version  "2.3alpha"))
        (c7 (inversion-decode-version  "1.3"))
        (c8 (inversion-decode-version  "1.3pre1"))
        (c9 (inversion-decode-version  "2.4 (patch 2)"))
        (c10 (inversion-decode-version "2.4 (patch 3)"))
        (c11 (inversion-decode-version "2.4.2.1"))
        (c12 (inversion-decode-version "2.4.2.2"))
        )
    (if (not (and
             (inversion-= c1 c1)
             (inversion-< c1i c1)
             (inversion-< c2 c3)
             (inversion-< c3 c4)
             (inversion-< c4 c5)
             (inversion-< c5 c6)
             (inversion-< c2 c4)
             (inversion-< c2 c5)
             (inversion-< c2 c6)
             (inversion-< c3 c5)
             (inversion-< c3 c6)
             (inversion-< c7 c6)
             (inversion-< c4 c7)
             (inversion-< c2 c7)
             (inversion-< c8 c6)
             (inversion-< c8 c7)
             (inversion-< c4 c8)
             (inversion-< c2 c8)
             (inversion-< c9 c10)
             (inversion-< c10 c11)
             (inversion-< c11 c12)
             ;; Negatives
             (not (inversion-< c3 c2))
             (not (inversion-< c4 c3))
             (not (inversion-< c5 c4))
             (not (inversion-< c6 c5))
             (not (inversion-< c7 c2))
             (not (inversion-< c7 c8))
             (not (inversion-< c12 c11))
             ;; Test the tester on inversion
             (not (inversion-test 'inversion inversion-version))
             ;; Test that we throw an error
             (inversion-test 'inversion "0.0.0")
             (inversion-test 'inversion "1000.0")
             ))
        (error "Inversion tests failed")
      (message "Inversion tests passed."))))


(provide 'inversion-utest)

;;; inversion.el ends here
