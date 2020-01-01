;;; subword-tests.el --- Testing the subword rules

;; Copyright (C) 2011-2020 Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'ert)
(require 'subword)

(defconst subword-tests-strings
  '("ABC^" ;;Bug#13758
    "ABC^ ABC^Foo^ ABC^-Foo^ toto^ ABC^"))

(ert-deftest subword-tests ()
  "Test the `subword-mode' rules."
  (with-temp-buffer
    (dolist (str subword-tests-strings)
      (erase-buffer)
      (insert str)
      (goto-char (point-min))
      (while (search-forward "^" nil t)
        (replace-match ""))
      (goto-char (point-min))
      (while (not (eobp))
        (subword-forward 1)
        (insert "^"))
      (should (equal (buffer-string) str)))))

(ert-deftest subword-tests2 ()
  "Test that motion in subword-mode stops at the right places."

  (let* ((line "fooBarBAZ quXD g_TESTThingAbc word BLAH test")
         (fwrd "*  *  *  *  * * *    *    *  *    *    *    *")
         (bkwd "*  *  *   * *  * *   *    *   *    *    *   *"))

    (with-temp-buffer
      (subword-mode 1)
      (insert line)

      ;; Test forward motion.

      (goto-char (point-min))
      (let ((stops (make-string (length fwrd) ?\ )))
        (while (progn
                 (aset stops (1- (point)) ?\*)
                 (not (eobp)))
          (forward-word))
        (should (equal stops fwrd)))

      ;; Test backward motion.

      (goto-char (point-max))
      (let ((stops (make-string (length bkwd) ?\ )))
        (while (progn
                 (aset stops (1- (point)) ?\*)
                 (not (bobp)))
          (backward-word))
        (should (equal stops bkwd))))))

(provide 'subword-tests)
;;; subword-tests.el ends here
