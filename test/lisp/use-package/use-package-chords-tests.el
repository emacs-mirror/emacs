;;; use-package-chords-tests.el --- Tests for use-package-chords.el  -*- lexical-binding: t; -*-

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

;;; Code:

(require 'use-package)
(require 'use-package-tests)
(require 'use-package-chords)

(defmacro match-expansion (form &rest value)
  `(should (pcase (expand-minimally ,form)
             ,@(mapcar #'(lambda (x) (list x t)) value))))

(defun use-package-test-normalize-chord (&rest args)
  (apply #'use-package-normalize-binder 'foo :chords args))

(ert-deftest use-package-test-normalize/:chords-1 ()
  (should (equal (use-package-test-normalize-chord
                  '(("C-a" . alpha)))
                 '(("C-a" . alpha)))))

(ert-deftest use-package-test-normalize/:chords-2 ()
  (should (equal (use-package-test-normalize-chord
                  '(("C-a" . alpha)
                    :map foo-map
                    ("C-b" . beta)))
                 '(("C-a" . alpha)
                   :map foo-map
                   ("C-b" . beta)))))

(ert-deftest use-package-test-normalize/:chords-3 ()
  (should (equal (use-package-test-normalize-chord
                  '(:map foo-map
                         ("C-a" . alpha)
                         ("C-b" . beta)))
                 '(:map foo-map
                        ("C-a" . alpha)
                        ("C-b" . beta)))))

(ert-deftest use-package-test/:chords-1 ()
  (match-expansion
   (use-package foo :chords ("C-k" . key1) ("C-u" . key2))
   `(progn
      (unless
          (fboundp 'key1)
        (autoload #'key1 "foo" nil t))
      (unless
          (fboundp 'key2)
        (autoload #'key2 "foo" nil t))
      (bind-chord "C-k" #'key1 nil)
      (bind-chord "C-u" #'key2 nil))))

(ert-deftest use-package-test/:chords-2 ()
  (match-expansion
   (use-package foo :chords (("C-k" . key1) ("C-u" . key2)))
   `(progn
      (unless (fboundp 'key1)
        (autoload #'key1 "foo" nil t))
      (unless (fboundp 'key2)
        (autoload #'key2 "foo" nil t))
      (bind-chord "C-k" #'key1 nil)
      (bind-chord "C-u" #'key2 nil))))

(ert-deftest use-package-test/:chords-3 ()
  (match-expansion
   (use-package foo :chords (:map my-map ("C-k" . key1) ("C-u" . key2)))
   `(progn
      (unless
          (fboundp 'key1)
        (autoload #'key1 "foo" nil t))
      (unless
          (fboundp 'key2)
        (autoload #'key2 "foo" nil t))
      (if
          (boundp 'my-map)
          (progn
            (bind-chord "C-k" #'key1 my-map)
            (bind-chord "C-u" #'key2 my-map))
        (eval-after-load 'foo
          '(progn
             (bind-chord "C-k" #'key1 my-map)
             (bind-chord "C-u" #'key2 my-map)))))))

(ert-deftest use-package-test/:chords-4 ()
  (should-error
   (match-expansion
    (use-package foo :chords :map my-map ("C-k" . key1) ("C-u" . key2))
    `(bind-chords :package foo))))

(ert-deftest use-package-test/:chords-5 ()
  (match-expansion
   (use-package foo :chords ("C-k" . key1) (:map my-map ("C-u" . key2)))
   `(progn
      (unless (fboundp 'key1)
        (autoload #'key1 "foo" nil t))
      (unless (fboundp 'key2)
        (autoload #'key2 "foo" nil t))
      (progn
        (bind-chord "C-k" #'key1 nil)
        (if
            (boundp 'my-map)
            (bind-chord "C-u" #'key2 my-map)
          (eval-after-load 'foo
            '(bind-chord "C-u" #'key2 my-map)))))))

(ert-deftest use-package-test/:chords-6 ()
  (match-expansion
   (use-package foo
     :chords
     ("C-k" . key1)
     (:map my-map ("C-u" . key2))
     (:map my-map2 ("C-u" . key3)))
   `(progn
      (unless
          (fboundp 'key1)
        (autoload #'key1 "foo" nil t))
      (unless
          (fboundp 'key2)
        (autoload #'key2 "foo" nil t))
      (unless
          (fboundp 'key3)
        (autoload #'key3 "foo" nil t))
      (progn
        (bind-chord "C-k" #'key1 nil)
        (if
            (boundp 'my-map)
            (bind-chord "C-u" #'key2 my-map)
          (eval-after-load 'foo
            '(bind-chord "C-u" #'key2 my-map)))
        (if
            (boundp 'my-map2)
            (bind-chord "C-u" #'key3 my-map2)
          (eval-after-load 'foo
            '(bind-chord "C-u" #'key3 my-map2)))))))

;; Local Variables:
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:

;;; use-package-chords-tests.el ends here
