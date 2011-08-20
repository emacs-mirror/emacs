;;; lex-spp-utest.el ---
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
;;

;;; Code:

(defun semantic-lex-spp-write-test ()
  "Test the semantic tag writer against the current buffer."
  (interactive)
  (with-output-to-temp-buffer "*SPP Write Test*"
    (semantic-lex-spp-table-write-slot-value
     (semantic-lex-spp-save-table))))

(defun semantic-lex-spp-write-utest ()
  "Unit test using the test spp file to test the slot write fcn."
  (interactive)
  (let* ((sem (locate-library "semantic/lex-spp.el"))
	 (dir (file-name-directory sem)))
    (save-excursion
      (set-buffer (find-file-noselect
		   (expand-file-name "tests/testsppreplace.c"
				     dir)))
      (semantic-lex-spp-write-test))))


(provide 'cedet/semantic/lex-spp-utest)

;;; lex-spp-utest.el ends here
