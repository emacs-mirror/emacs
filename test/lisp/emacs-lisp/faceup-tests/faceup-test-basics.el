;;; faceup-test-basics.el --- Tests for the `faceup' package.

;; Copyright (C) 2014-2017 Free Software Foundation, Inc.

;; Author: Anders Lindgren
;; Keywords: languages, faces

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

;; Basic tests for the `faceup' package.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'faceup)

(ert-deftest faceup-functions ()
  "Test primitive functions."
  (should (equal (faceup-normalize-face-property '()) '()))
  (should (equal (faceup-normalize-face-property 'a) '(a)))
  (should (equal (faceup-normalize-face-property '(a)) '(a)))
  (should (equal (faceup-normalize-face-property '(:x t)) '((:x t))))
  (should (equal (faceup-normalize-face-property '(:x t a)) '((:x t))))
  (should (equal (faceup-normalize-face-property '(:x t a b)) '((:x t))))
  (should (equal (faceup-normalize-face-property '(a :x t)) '(a (:x t))))
  (should (equal (faceup-normalize-face-property '(a b :x t))
                 '(a b (:x t))))

  (should (equal (faceup-normalize-face-property '(:x t :y nil))
                 '((:y nil) (:x t))))
  (should (equal (faceup-normalize-face-property '(:x t :y nil a))
                 '((:y nil) (:x t))))
  (should (equal (faceup-normalize-face-property '(:x t  :y nil a b))
                 '((:y nil) (:x t))))
  (should (equal (faceup-normalize-face-property '(a :x t :y nil))
                 '(a (:y nil) (:x t))))
  (should (equal (faceup-normalize-face-property '(a b :x t :y nil))
                 '(a b (:y nil) (:x t)))))


(ert-deftest faceup-markup-basics ()
  (should (equal (faceup-markup-string "")     ""))
  (should (equal (faceup-markup-string "test") "test")))

(ert-deftest faceup-markup-escaping ()
  (should (equal (faceup-markup-string "«") "««"))
  (should (equal (faceup-markup-string "«A«B«C«") "««A««B««C««"))
  (should (equal (faceup-markup-string "»") "«»"))
  (should (equal (faceup-markup-string "»A»B»C»") "«»A«»B«»C«»")))

(ert-deftest faceup-markup-plain ()
  ;;   UU
  ;; ABCDEF
  (should (equal (faceup-markup-string
                  #("ABCDEF" 2 4 (face underline)))
                 "AB«U:CD»EF")))

(ert-deftest faceup-markup-plain-full-text ()
  ;; UUUUUU
  ;; ABCDEF
  (should (equal (faceup-markup-string
                  #("ABCDEF" 0 6 (face underline)))
                 "«U:ABCDEF»")))

(ert-deftest faceup-markup-anonymous-face ()
  ;;   AA
  ;; ABCDEF
  (should (equal (faceup-markup-string
                  #("ABCDEF" 2 4 (face (:underline t))))
                 "AB«:(:underline t):CD»EF")))

(ert-deftest faceup-markup-anonymous-face-2keys ()
  ;;   AA
  ;; ABCDEF
  (should (equal (faceup-markup-string
                  #("ABCDEF" 2 4 (face (:foo t :bar nil))))
                 "AB«:(:foo t):«:(:bar nil):CD»»EF"))
  ;; Plist in list.
  (should (equal (faceup-markup-string
                  #("ABCDEF" 2 4 (face ((:foo t :bar nil)))))
                 "AB«:(:foo t):«:(:bar nil):CD»»EF"))
  ;; Two plists.
  (should (equal (faceup-markup-string
                  #("ABCDEF" 2 4 (face ((:foo t) (:bar nil)))))
                 "AB«:(:bar nil):«:(:foo t):CD»»EF")))

(ert-deftest faceup-markup-anonymous-nested ()
  ;;   AA
  ;;  IIII
  ;; ABCDEF
  (should (equal (faceup-markup-string
                  #("ABCDEF"
                    1 2 (face ((:foo t)))
                    2 4 (face ((:bar t) (:foo t)))
                    4 5 (face ((:foo t)))))
                 "A«:(:foo t):B«:(:bar t):CD»E»F")))

(ert-deftest faceup-markup-nested ()
  ;;   UU
  ;;  IIII
  ;; ABCDEF
  (should (equal (faceup-markup-string
                  #("ABCDEF"
                    1 2 (face italic)
                    2 4 (face (underline italic))
                    4 5 (face italic)))
                 "A«I:B«U:CD»E»F")))

(ert-deftest faceup-markup-overlapping ()
  ;;   UUU
  ;;  III
  ;; ABCDEF
  (should (equal (faceup-markup-string
                  #("ABCDEF"
                    1 2 (face italic)
                    2 4 (face (underline italic))
                    4 5 (face underline)))
                 "A«I:B«U:CD»»«U:E»F"))
  ;;  III
  ;;   UUU
  ;; ABCDEF
  (should (equal (faceup-markup-string
                  #("ABCDEF"
                    1 2 (face italic)
                    2 4 (face (italic underline))
                    4 5 (face underline)))
                 "A«I:B»«U:«I:CD»E»F")))

(ert-deftest faceup-markup-multi-face ()
  ;; More than one face at the same location.
  ;;
  ;; The property to the front takes precedence, it is rendered as the
  ;; innermost parenthesis pair.
  (should (equal (faceup-markup-string
                  #("ABCDEF" 2 4 (face (underline italic))))
                 "AB«I:«U:CD»»EF"))
  (should (equal (faceup-markup-string
                  #("ABCDEF" 2 4 (face (italic underline))))
                 "AB«U:«I:CD»»EF"))
  ;; Equal ranges, full text.
  (should (equal (faceup-markup-string
                  #("ABCDEF" 0 6 (face (underline italic))))
                 "«I:«U:ABCDEF»»"))
  ;; Ditto, with stray markup characters.
  (should (equal (faceup-markup-string
                  #("AB«CD»EF" 0 8 (face (underline italic))))
                 "«I:«U:AB««CD«»EF»»")))

(ert-deftest faceup-markup-multi-property ()
  (let ((faceup-properties '(alpha beta gamma)))
    ;; One property.
    (should (equal (faceup-markup-string
                    #("ABCDEF" 2 4 (alpha (a l p h a))))
                   "AB«(alpha):(a l p h a):CD»EF"))

    ;; Two properties, inner enclosed.
    (should (equal (faceup-markup-string
                    (let ((s (copy-sequence "ABCDEFGHIJ")))
                      (set-text-properties 2 8 '(alpha (a l p h a)) s)
                      (font-lock-append-text-property 4 6 'beta '(b e t a) s)
                      s))
                   "AB«(alpha):(a l p h a):CD«(beta):(b e t a):EF»GH»IJ"))

    ;; Two properties, same end
    (should (equal (faceup-markup-string
                    (let ((s (copy-sequence "ABCDEFGH")))
                      (set-text-properties 2 6 '(alpha (a)) s)
                      (add-text-properties 4 6 '(beta (b)) s)
                      s))
                   "AB«(alpha):(a):CD«(beta):(b):EF»»GH"))

    ;; Two properties, overlap.
    (should (equal (faceup-markup-string
                    (let ((s (copy-sequence "ABCDEFGHIJ")))
                      (set-text-properties 2 6 '(alpha (a)) s)
                      (add-text-properties 4 8 '(beta (b)) s)
                      s))
                   "AB«(alpha):(a):CD«(beta):(b):EF»»«(beta):(b):GH»IJ"))))


(ert-deftest faceup-clean ()
  "Test the clean features of `faceup'."
  (should (equal (faceup-clean-string "")     ""))
  (should (equal (faceup-clean-string "test") "test"))
  (should (equal (faceup-clean-string "AB«U:CD»EF")         "ABCDEF"))
  (should (equal (faceup-clean-string "«U:ABCDEF»")         "ABCDEF"))
  (should (equal (faceup-clean-string "A«I:B«U:CD»E»F")     "ABCDEF"))
  (should (equal (faceup-clean-string "A«I:B«U:CD»»«U:E»F") "ABCDEF"))
  (should (equal (faceup-clean-string "AB«I:«U:CD»»EF")     "ABCDEF"))
  (should (equal (faceup-clean-string "«I:«U:ABCDEF»»")     "ABCDEF"))
  (should (equal (faceup-clean-string "«(foo)I:ABC»DEF")    "ABCDEF"))
  (should (equal (faceup-clean-string "«:(:foo t):ABC»DEF") "ABCDEF"))
  ;; Escaped markup characters.
  (should (equal (faceup-clean-string "««") "«"))
  (should (equal (faceup-clean-string "«»") "»"))
  (should (equal (faceup-clean-string "A«I:B«U:CD»«»»«U:E»F") "ABCD»EF")))


(ert-deftest faceup-render ()
  "Test the render features of `faceup'."
  (should (equal (faceup-render-string "")     ""))
  (should (equal (faceup-render-string "««") "«"))
  (should (equal (faceup-render-string "«»") "»"))
  (should (equal (faceup-render-string "A«I:B«U:CD»«»»«U:E»F") "ABCD»EF")))


(defvar faceup-test-resources-directory
  (concat (file-name-directory
           (substring (faceup-this-file-directory) 0 -1))
          "faceup-resources/")
  "The `faceup-resources' directory.")


(defvar faceup-test-this-file-directory nil
  "The result of `faceup-this-file-directory' in various contexts.

This is set by the file test support file
`faceup-test-this-file-directory.el'.")


(ert-deftest faceup-directory ()
  "Test `faceup-this-file-directory'."
  (let ((file (concat faceup-test-resources-directory
                      "faceup-test-this-file-directory.el"))
        (load-file-name nil))
    ;; Test normal load.
    (makunbound 'faceup-test-this-file-directory)
    (load file nil :nomessage)
    (should (equal faceup-test-this-file-directory
                   faceup-test-resources-directory))
    ;; Test `eval-buffer'.
    (makunbound 'faceup-test-this-file-directory)
    (save-excursion
      (find-file file)
      (eval-buffer))
    (should (equal faceup-test-this-file-directory
                   faceup-test-resources-directory))
    ;; Test `eval-defun'.
    (makunbound 'faceup-test-this-file-directory)
    (save-excursion
      (find-file file)
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          ;; Note: In batch mode, this prints the result of the
          ;; evaluation.  Unfortunately, this is hard to fix.
          (eval-defun nil)
          (forward-sexp))))
    (should (equal faceup-test-this-file-directory
                   faceup-test-resources-directory))))

(provide 'faceup-test-basics)

;;; faceup-test-basics.el ends here
