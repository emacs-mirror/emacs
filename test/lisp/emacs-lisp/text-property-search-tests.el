;;; text-property-search-tests.el --- Testing text-property-search

;; Copyright (C) 2018-2020 Free Software Foundation, Inc.

;; Author: Lars Ingebrigtsen <larsi@gnus.org>
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
(require 'text-property-search)
(require 'cl-lib)

(defun text-property-setup ()
  (insert "This is "
          (propertize "bold1" 'face 'bold)
          " and this is "
          (propertize "italic1" 'face 'italic)
          (propertize "bold2" 'face 'bold)
          (propertize "italic2" 'face 'italic)
          " at the end")
  (goto-char (point-min)))

(defmacro with-test (form result &optional point)
  `(with-temp-buffer
     (text-property-setup)
     (when ,point
       (goto-char ,point))
     (should
      (equal
       (cl-loop for match = ,form
                while match
                collect (buffer-substring (prop-match-beginning match)
                                          (prop-match-end match)))
       ,result))))

(ert-deftest text-property-search-forward-bold-t ()
  (with-test (text-property-search-forward 'face 'bold t)
             '("bold1" "bold2")))

(ert-deftest text-property-search-forward-bold-nil ()
  (with-test (text-property-search-forward 'face 'bold nil)
             '("This is " " and this is italic1" "italic2 at the end")))

(ert-deftest text-property-search-forward-nil-t ()
  (with-test (text-property-search-forward 'face nil t)
             '("This is " " and this is " " at the end")))

(ert-deftest text-property-search-forward-nil-nil ()
  (with-test (text-property-search-forward 'face nil nil)
             '("bold1" "italic1" "bold2" "italic2")))

(ert-deftest text-property-search-forward-partial-bold-t ()
  (with-test (text-property-search-forward 'face 'bold t)
             '("old1" "bold2")
             10))

(ert-deftest text-property-search-forward-partial-non-current-bold-t ()
  (with-test (text-property-search-forward 'face 'bold t t)
             '("bold2")
             10))


(ert-deftest text-property-search-backward-bold-t ()
  (with-test (text-property-search-backward 'face 'bold t)
             '("bold2" "bold1")
             (point-max)))

(ert-deftest text-property-search-backward-bold-nil ()
  (with-test (text-property-search-backward 'face 'bold nil)
             '( "italic2 at the end" " and this is italic1" "This is ")
             (point-max)))

(ert-deftest text-property-search-backward-nil-t ()
  (with-test (text-property-search-backward 'face nil t)
             '(" at the end" " and this is " "This is ")
             (point-max)))

(ert-deftest text-property-search-backward-nil-nil ()
  (with-test (text-property-search-backward 'face nil nil)
             '("italic2" "bold2" "italic1" "bold1")
             (point-max)))

(ert-deftest text-property-search-backward-partial-bold-t ()
  (with-test (text-property-search-backward 'face 'bold t)
             '("b" "bold1")
             35))

(ert-deftest text-property-search-backward-partial-non-current-bold-t ()
  (with-test (text-property-search-backward 'face 'bold t t)
             '("bold1")
             35))

(defmacro with-match-test (form beginning end value &optional point)
  `(with-temp-buffer
     (text-property-setup)
     (when ,point
       (goto-char ,point))
     (should (equal ,form
                    (make-prop-match :beginning ,beginning
                                     :end ,end
                                     :value ,value)))))

(ert-deftest text-property-search-forward-prop-match-match-face-nil-nil ()
  (with-match-test
   (text-property-search-forward 'face nil nil)
   9 14 'bold))

(ert-deftest text-property-search-forward-prop-match-match-face-bold-t ()
  (with-match-test
   (text-property-search-forward 'face 'bold t)
   9 14 'bold))

(ert-deftest text-property-search-forward-prop-match-match-face-bold-nil ()
  (with-match-test
   (text-property-search-forward 'face 'bold nil)
   1 9 nil))

(ert-deftest text-property-search-backward-prop-match-match-face-nil-nil ()
  (with-match-test
   (text-property-search-backward 'face nil nil)
   39 46 'italic
   (point-max)))

(ert-deftest text-property-search-backward-prop-match-match-face-italic-t ()
  (with-match-test
   (text-property-search-backward 'face 'italic t)
   39 46 'italic
   (point-max)))

(ert-deftest text-property-search-backward-prop-match-match-face-italic-nil ()
  (with-match-test
   (text-property-search-backward 'face 'italic nil)
   46 57 nil
   (point-max)))

(provide 'text-property-search-tests)

;;; text-property-search-tests.el ends here
