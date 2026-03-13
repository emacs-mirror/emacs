;;; charset-tests.el --- Tests for charset.c -*- lexical-binding: t -*-

;; Copyright 2017-2026 Free Software Foundation, Inc.

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

(ert-deftest charset-decode-char ()
  "Test `decode-char'."
  (should-error (decode-char 'ascii 0.5)))

(ert-deftest charset-tests-charsetp ()
  (should (charsetp 'ascii))
  (should (charsetp 'unicode))
  (should-not (charsetp 'charset-tests-no-such-charset)))

(ert-deftest charset-tests-charset-id-internal ()
  (let ((id (charset-id-internal 'ascii)))
    (should (integerp id))
    (should (<= 0 id))))

(ert-deftest charset-tests-charset-plist ()
  (let ((plist (charset-plist 'ascii)))
    (should (listp plist))
    (should (stringp (plist-get plist :short-name)))))

(ert-deftest charset-tests-charset-priority-list ()
  (let ((list (charset-priority-list)))
    (should (listp list))
    (should (consp list))
    (should (memq 'ascii list))
    (dolist (cs list)
      (should (charsetp cs))))
  (let ((highest (charset-priority-list t)))
    (should (symbolp highest))
    (should (charsetp highest))))

(ert-deftest charset-tests-charset-after ()
  (with-temp-buffer
    (insert "a")
    (goto-char (point-min))
    (should (eq (charset-after) 'ascii))
    (should-not (charset-after (1+ (point-max))))))

(ert-deftest charset-tests-find-charset-string ()
  (let ((charsets (find-charset-string "abc")))
    (should (memq 'ascii charsets))
    (dolist (cs charsets)
      (should (charsetp cs))))
  (let ((charsets (find-charset-string "あ")))
    (should (consp charsets))
    (dolist (cs charsets)
      (should (charsetp cs)))))

(ert-deftest charset-tests-find-charset-region ()
  (with-temp-buffer
    (insert "abc")
    (let ((charsets (find-charset-region (point-min) (point-max))))
      (should (memq 'ascii charsets))
      (dolist (cs charsets)
        (should (charsetp cs))))))

(ert-deftest charset-tests--map-charset-chars ()
  (let (chars)
    (map-charset-chars (lambda (range _arg)
                         (setq chars (append chars
                                             (number-sequence
                                              (car range) (cdr range)))))
                       'ascii nil 65 67)
    (setq chars (sort (delete-dups chars) #'<))
    (should (equal chars '(65 66 67)))))

(ert-deftest charset-tests--define-charset-internal-errors ()
  (should-error (define-charset-internal)))

(defvar charset-tests--internal-counter 0)

(ert-deftest charset-tests--define-charset-alias ()
  (let ((alias (intern (format "charset-tests--alias-%d"
                               (setq charset-tests--internal-counter
                                     (1+ charset-tests--internal-counter))))))
    (define-charset-alias alias 'ascii)
    (should (charsetp alias))))

(ert-deftest charset-tests--set-charset-plist ()
  (let ((orig (charset-plist 'ascii)))
    (unwind-protect
        (progn
          (set-charset-plist 'ascii '(:charset-tests t))
          (should (equal (charset-plist 'ascii) '(:charset-tests t))))
      (set-charset-plist 'ascii orig))))

(ert-deftest charset-tests--unify-charset-error ()
  (should-error (unify-charset 'ascii)))

(ert-deftest charset-tests--get-unused-iso-final-char ()
  (let ((val (get-unused-iso-final-char 1 94)))
    (when val
      (should (<= ?0 val))
      (should (<= val ??)))))

(ert-deftest charset-tests--declare-equiv-charset ()
  (should-not (declare-equiv-charset 1 94 ?B 'ascii))
  (should (eq (iso-charset 1 94 ?B) 'ascii)))

(ert-deftest charset-tests--split-char ()
  (let ((parts (split-char ?A)))
    (should (eq (car parts) 'ascii))
    (should (equal (cdr parts) '(65)))))

(ert-deftest charset-tests--iso-charset ()
  (should (eq (iso-charset 1 94 ?B) 'ascii)))

(ert-deftest charset-tests--clear-charset-maps ()
  (should-not (clear-charset-maps)))

(ert-deftest charset-tests--set-charset-priority ()
  (let ((orig (charset-priority-list)))
    (unwind-protect
        (progn
          (set-charset-priority 'unicode)
          (should (eq (charset-priority-list t) 'unicode)))
      (apply #'set-charset-priority orig))))

(ert-deftest charset-tests--sort-charsets ()
  (let* ((priority (charset-priority-list))
         (a (car priority))
         (b (cadr priority)))
    (skip-unless (and a b))
    (let ((sorted (sort-charsets (list b a))))
      (should (equal sorted (list a b))))))

(provide 'charset-tests)

;;; charset-tests.el ends here
