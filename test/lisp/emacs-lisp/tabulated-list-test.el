;;; tabulated-list-test.el --- Tests for emacs-lisp/tabulated-list.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2017 Free Software Foundation, Inc.

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>

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

(require 'tabulated-list)
(require 'ert)

(defconst tabulated-list--test-entries
  '(("zzzz-game" ["zzzz-game" "zzzz-game" "2113" "installed" " play zzzz in Emacs"])
    ("4clojure"  ["4clojure" "4clojure" "1507" "obsolete" " Open and evaluate 4clojure.com questions"])
    ("abc-mode"  ["abc-mode" "abc-mode" "944" "available" " Major mode for editing abc music files"])
    ("mode"      ["mode" "mode" "1128" "installed" " A simple mode for editing Actionscript 3 files"])))

(defun tabulated-list--test-sort-car (a b)
  (string< (car a) (car b)))

(defconst tabulated-list--test-format
  [("name" 10 tabulated-list--test-sort-car)
   ("name-2" 10 t)
   ("Version" 9 nil)
   ("Status" 10 )
   ("Description" 0 nil)])

(defmacro tabulated-list--test-with-buffer (&rest body)
  `(with-temp-buffer
     (tabulated-list-mode)
     (setq tabulated-list-entries (copy-alist tabulated-list--test-entries))
     (setq tabulated-list-format tabulated-list--test-format)
     (setq tabulated-list-padding 7)
     (tabulated-list-init-header)
     (tabulated-list-print)
     ,@body))


;;; Tests
(ert-deftest tabulated-list-print ()
  (tabulated-list--test-with-buffer
   ;; Basic printing.
   (should (string= (buffer-substring-no-properties (point-min) (point-max))
                    "       zzzz-game  zzzz-game  2113      installed   play zzzz in Emacs
       4clojure   4clojure   1507      obsolete    Open and evaluate 4clojure.com questions
       abc-mode   abc-mode   944       available   Major mode for editing abc music files
       mode       mode       1128      installed   A simple mode for editing Actionscript 3 files\n"))
   ;; Preserve position.
   (forward-line 3)
   (let ((pos (thing-at-point 'line)))
     (pop tabulated-list-entries)
     (tabulated-list-print t)
     (should (equal (thing-at-point 'line) pos))
     (should (string= (buffer-substring-no-properties (point-min) (point-max))
                      "       4clojure   4clojure   1507      obsolete    Open and evaluate 4clojure.com questions
       abc-mode   abc-mode   944       available   Major mode for editing abc music files
       mode       mode       1128      installed   A simple mode for editing Actionscript 3 files\n"))
     ;; Check the UPDATE argument
     (pop tabulated-list-entries)
     (setf (cdr (car tabulated-list-entries)) (list ["x" "x" "944" "available" " XX"]))
     (tabulated-list-print t t)
     (should (string= (buffer-substring-no-properties (point-min) (point-max))
                      "       x          x          944       available   XX
       mode       mode       1128      installed   A simple mode for editing Actionscript 3 files\n"))
     (should (equal (thing-at-point 'line) pos)))))

(ert-deftest tabulated-list-sort ()
  (tabulated-list--test-with-buffer
   ;; Basic sorting
   (goto-char (point-min))
   (skip-chars-forward "[:blank:]")
   (tabulated-list-sort)
   (let ((text (buffer-substring-no-properties (point-min) (point-max))))
     (should (string= text "       4clojure   4clojure   1507      obsolete    Open and evaluate 4clojure.com questions
       abc-mode   abc-mode   944       available   Major mode for editing abc music files
       mode       mode       1128      installed   A simple mode for editing Actionscript 3 files
       zzzz-game  zzzz-game  2113      installed   play zzzz in Emacs\n"))

     (skip-chars-forward "^[:blank:]")
     (skip-chars-forward "[:blank:]")
     (should (equal (get-text-property (point) 'tabulated-list-column-name)
                    "name-2"))
     (tabulated-list-sort)
     ;; Check a `t' as the sorting predicate.
     (should (string= text (buffer-substring-no-properties (point-min) (point-max))))
     ;; Invert.
     (tabulated-list-sort 1)
     (should (string= (buffer-substring-no-properties (point-min) (point-max))
                      "       zzzz-game  zzzz-game  2113      installed   play zzzz in Emacs
       mode       mode       1128      installed   A simple mode for editing Actionscript 3 files
       abc-mode   abc-mode   944       available   Major mode for editing abc music files
       4clojure   4clojure   1507      obsolete    Open and evaluate 4clojure.com questions\n"))
     ;; Again
     (tabulated-list-sort 1)
     (should (string= text (buffer-substring-no-properties (point-min) (point-max)))))
   ;; Check that you can't sort some cols.
   (skip-chars-forward "^[:blank:]")
   (skip-chars-forward "[:blank:]")
   (should-error (tabulated-list-sort) :type 'user-error)
   (should-error (tabulated-list-sort 4) :type 'user-error)))

(provide 'tabulated-list-test)
;;; tabulated-list-test.el ends here
