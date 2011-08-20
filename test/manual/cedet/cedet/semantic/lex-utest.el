;;; lex-utest.el ---
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

(defun semantic-lex-test-full-depth (arg)
  "Test the semantic lexer in the current buffer parsing through lists.
Usually the lexer parses
If universal argument ARG, then try the whole buffer."
  (interactive "P")
  (let* ((start (current-time))
         (result (semantic-lex
                  (if arg (point-min) (point))
                  (point-max)
                  100))
         (end (current-time)))
    (message "Elapsed Time: %.2f seconds."
             (semantic-elapsed-time start end))
    (pop-to-buffer "*Lexer Output*")
    (require 'pp)
    (erase-buffer)
    (insert (pp-to-string result))
    (goto-char (point-min))))

(defun semantic-lex-test-region (beg end)
  "Test the semantic lexer in the current buffer.
Analyze the area between BEG and END."
  (interactive "r")
  (let ((result (semantic-lex beg end)))
    (pop-to-buffer "*Lexer Output*")
    (require 'pp)
    (erase-buffer)
    (insert (pp-to-string result))
    (goto-char (point-min))))


(provide 'cedet/semantic/lex-utest)

;;; lex-utest.el ends here
