;;; hi-lock-tests.el --- Tests for hi-lock.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2024 Free Software Foundation, Inc.

;; Author: Tino Calancha <tino.calancha@gmail.com>
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

;;; Code:

(require 'ert)
(require 'hi-lock)

(ert-deftest hi-lock-bug26666 ()
  "Test for https://debbugs.gnu.org/26666 ."
  (let ((faces hi-lock-face-defaults))
    (with-temp-buffer
      (insert "a A b B\n")
      (cl-letf (((symbol-function 'read-face-name)
                   (lambda (_prompt &optional defaults)
                     (intern (car defaults)))))
        (dotimes (_ 2)
          (let ((face (hi-lock-read-face-name)))
            (hi-lock-set-pattern "a" face))))
      (should (equal hi-lock--unused-faces (cdr faces))))))

(ert-deftest hi-lock-test-set-pattern ()
  (let ((faces hi-lock-face-defaults))
    (with-temp-buffer
      (insert "foo bar")
      (cl-letf (((symbol-function 'read-face-name)
                   (lambda (_prompt &optional defaults)
                     (intern (car defaults)))))
        (hi-lock-set-pattern "9999" (hi-lock-read-face-name)) ; No match
        (hi-lock-set-pattern "foo" (hi-lock-read-face-name)))
      ;; Only one match, then we have used just 1 face
      (should (equal hi-lock--unused-faces (cdr faces))))))

(ert-deftest hi-lock-case-fold ()
  "Test for case-sensitivity."
  (let ((hi-lock-auto-select-face t))
    (with-temp-buffer
      (insert "a A b B\n")

      (dotimes (_ 2) (highlight-regexp "[a]"))
      (should (= (length (overlays-in (point-min) (point-max))) 2))
      (unhighlight-regexp "[a]")
      (should (= (length (overlays-in (point-min) (point-max))) 0))

      (dotimes (_ 2) (highlight-regexp "[a]" nil nil "a"))
      (should (= (length (overlays-in (point-min) (point-max))) 2))
      (unhighlight-regexp "a")
      (should (= (length (overlays-in (point-min) (point-max))) 0))

      (dotimes (_ 2) (highlight-regexp "[A]" ))
      (should (= (length (overlays-in (point-min) (point-max))) 1))
      (unhighlight-regexp "[A]")
      (should (= (length (overlays-in (point-min) (point-max))) 0))

      (dotimes (_ 2) (highlight-regexp "[A]" nil nil "A"))
      (should (= (length (overlays-in (point-min) (point-max))) 1))
      (unhighlight-regexp "A")
      (should (= (length (overlays-in (point-min) (point-max))) 0))

      (let ((case-fold-search nil)) (dotimes (_ 2) (highlight-regexp "[a]")))
      (should (= (length (overlays-in (point-min) (point-max))) 1))
      (unhighlight-regexp "[a]")
      (should (= (length (overlays-in (point-min) (point-max))) 0))

      (dotimes (_ 2) (highlight-phrase "a   a"))
      (should (= (length (overlays-in (point-min) (point-max))) 1))
      (unhighlight-regexp "a   a")
      (should (= (length (overlays-in (point-min) (point-max))) 0))

      (let ((search-spaces-regexp search-whitespace-regexp))
        (highlight-regexp "a   a"))
      (should (= (length (overlays-in (point-min) (point-max))) 1))
      ;; We bind use-dialog-box to nil to prevent unhighlight-regexp
      ;; from using popup menus, since the replacement for
      ;; completing-read below is not ready for that calamity
      (let ((use-dialog-box nil))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt _coll
                                    &optional _x _y _z _hist defaults _inherit)
                     (car defaults))))
          (call-interactively 'unhighlight-regexp)))
      (should (= (length (overlays-in (point-min) (point-max))) 0))

      (emacs-lisp-mode)
      (setq font-lock-mode t)

      (dotimes (_ 2) (highlight-regexp "[a]"))
      (font-lock-ensure)
      (should (memq 'hi-yellow (get-text-property 1 'face)))
      (should (memq 'hi-yellow (get-text-property 3 'face)))
      (let ((font-lock-fontified t)) (unhighlight-regexp "[a]"))
      (should (null (get-text-property 3 'face)))

      (dotimes (_ 2) (highlight-regexp "[a]" nil nil "a"))
      (font-lock-ensure)
      (should (memq 'hi-yellow (get-text-property 1 'face)))
      (should (memq 'hi-yellow (get-text-property 3 'face)))
      (let ((font-lock-fontified t)) (unhighlight-regexp "a"))
      (should (null (get-text-property 3 'face)))

      (dotimes (_ 2) (highlight-regexp "[A]" ))
      (font-lock-ensure)
      (should (null (get-text-property 1 'face)))
      (should (memq 'hi-yellow (get-text-property 3 'face)))
      (let ((font-lock-fontified t)) (unhighlight-regexp "[A]"))
      (should (null (get-text-property 3 'face)))

      (dotimes (_ 2) (highlight-regexp "[A]" nil nil "A"))
      (font-lock-ensure)
      (should (null (get-text-property 1 'face)))
      (should (memq 'hi-yellow (get-text-property 3 'face)))
      (let ((font-lock-fontified t)) (unhighlight-regexp "A"))
      (should (null (get-text-property 3 'face)))

      (let ((case-fold-search nil)) (dotimes (_ 2) (highlight-regexp "[a]")))
      (font-lock-ensure)
      (should (memq 'hi-yellow (get-text-property 1 'face)))
      (should (null (get-text-property 3 'face)))
      (let ((font-lock-fontified t)) (unhighlight-regexp "[a]"))
      (should (null (get-text-property 1 'face)))

      (dotimes (_ 2) (highlight-phrase "a   a"))
      (font-lock-ensure)
      (should (memq 'hi-yellow (get-text-property 1 'face)))
      (let ((font-lock-fontified t)) (unhighlight-regexp "a   a"))
      (should (null (get-text-property 1 'face)))

      (let ((search-spaces-regexp search-whitespace-regexp)) (highlight-regexp "a   a"))
      (font-lock-ensure)
      (should (memq 'hi-yellow (get-text-property 1 'face)))
      ;; We bind use-dialog-box to nil to prevent unhighlight-regexp
      ;; from using popup menus, since the replacement for
      ;; completing-read below is not ready for that calamity
      (let ((use-dialog-box nil))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt _coll
                                    &optional _x _y _z _hist defaults _inherit)
                     (car defaults)))
                  (font-lock-fontified t))
          (call-interactively 'unhighlight-regexp)))
      (should (null (get-text-property 1 'face))))))

(ert-deftest hi-lock-unhighlight ()
  "Test for unhighlighting and `hi-lock--regexps-at-point'."
  (let ((hi-lock-auto-select-face t))
    (with-temp-buffer
      (insert "aAbB\n")

      ;; We bind use-dialog-box to nil to prevent unhighlight-regexp
      ;; from using popup menus, since the replacement for
      ;; completing-read below is not ready for that calamity
      (let ((use-dialog-box nil))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (_prompt _coll
                                    &optional _x _y _z _hist defaults _inherit)
                     (car defaults))))
          (highlight-regexp "a")
          (highlight-regexp "b")
          (should (= (length (overlays-in (point-min) (point-max))) 4))
          ;; `hi-lock--regexps-at-point' should take regexp "a" at point 1,
          ;; not the last regexp "b"
          (goto-char 1)
          (call-interactively 'unhighlight-regexp)
          (should (= (length (overlays-in 1 3)) 0))
          (should (= (length (overlays-in 3 5)) 2))
          ;; Next call should unhighlight remaining regepxs
          (call-interactively 'unhighlight-regexp)
          (should (= (length (overlays-in 3 5)) 0))

          ;; Test unhighlight all
          (highlight-regexp "a")
          (highlight-regexp "b")
          (should (= (length (overlays-in (point-min) (point-max))) 4))
          (unhighlight-regexp t)
          (should (= (length (overlays-in (point-min) (point-max))) 0))

          (emacs-lisp-mode)
          (setq font-lock-mode t)

          (highlight-regexp "a")
          (highlight-regexp "b")
          (font-lock-ensure)
          (should (memq 'hi-yellow (get-text-property 1 'face)))
          (should (memq 'hi-yellow (get-text-property 3 'face)))
          ;; `hi-lock--regexps-at-point' should take regexp "a" at point 1,
          ;; not the last regexp "b"
          (goto-char 1)
          (let ((font-lock-fontified t))
            (call-interactively 'unhighlight-regexp))
          (should (null (get-text-property 1 'face)))
          (should (memq 'hi-yellow (get-text-property 3 'face)))
          ;; Next call should unhighlight remaining regepxs
          (let ((font-lock-fontified t))
            (call-interactively 'unhighlight-regexp))
          (should (null (get-text-property 3 'face)))

          ;; Test unhighlight all
          (highlight-regexp "a")
          (highlight-regexp "b")
          (font-lock-ensure)
          (should (memq 'hi-yellow (get-text-property 1 'face)))
          (should (memq 'hi-yellow (get-text-property 3 'face)))
          (let ((font-lock-fontified t))
            (unhighlight-regexp t))
          (should (null (get-text-property 1 'face)))
          (should (null (get-text-property 3 'face))))))))

(provide 'hi-lock-tests)
;;; hi-lock-tests.el ends here
