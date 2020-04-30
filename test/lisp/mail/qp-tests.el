;;; qp-tests.el --- Tests for qp.el  -*- lexical-binding:t; coding:utf-8 -*-

;; Copyright (C) 2020 Free Software Foundation, Inc.

;; Author: Stefan Kangas <stefankangas@gmail.com>

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

;;; Code:

(require 'ert)
(require 'qp)

;; Quote by Antoine de Saint-Exupéry, Citadelle (1948)
;; from https://en.wikipedia.org/wiki/Quoted-printable
(defvar qp-tests-quote-qp
  (concat "J'interdis aux marchands de vanter trop leurs marchandises. Car ils se font =\n"
          "vite p=C3=A9dagogues et t'enseignent comme but ce qui n'est par essence qu'=\n"
          "un moyen, et te trompant ainsi sur la route =C3=A0 suivre les voil=C3=A0 bi=\n"
          "ent=C3=B4t qui te d=C3=A9gradent, car si leur musique est vulgaire ils te f=\n"
          "abriquent pour te la vendre une =C3=A2me vulgaire."))
(defvar qp-tests-quote-utf8
  (concat "J'interdis aux marchands de vanter trop leurs marchandises. Car ils se font "
          "vite pédagogues et t'enseignent comme but ce qui n'est par essence qu'"
          "un moyen, et te trompant ainsi sur la route à suivre les voilà bi"
          "entôt qui te dégradent, car si leur musique est vulgaire ils te f"
          "abriquent pour te la vendre une âme vulgaire."))

(ert-deftest qp-test--quoted-printable-decode-region ()
  (with-temp-buffer
    (insert qp-tests-quote-qp)
    (encode-coding-region (point-min) (point-max) 'utf-8)
    (quoted-printable-decode-region (point-min) (point-max) 'utf-8)
    (should (equal (buffer-string) qp-tests-quote-utf8))))

(ert-deftest qp-test--quoted-printable-decode-string ()
  (should (equal (quoted-printable-decode-string "foo!") "foo!"))
    (should (equal (quoted-printable-decode-string "=0C") "\^L"))
  (should (equal (quoted-printable-decode-string "=3D") "="))
  (should (equal (quoted-printable-decode-string "=A1Hola, se=F1or!?")
                 "\241Hola, se\361or!?")))

(ert-deftest qp-test--quoted-printable-encode-region ()
  (with-temp-buffer
    (insert (make-string 26 ?=))
    ;; (encode-coding-region (point-min) (point-max) 'utf-8)
    (quoted-printable-encode-region (point-min) (point-max) t)
    (should (equal (buffer-string)
                   (concat "=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D"
                           "=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=3D=\n=3D")))))

(ert-deftest qp-test--quoted-printable-encode-string ()
  (should (equal (quoted-printable-encode-string "\241Hola, se\361or!?")
                 "=A1Hola, se=F1or!?"))
  ;; Multibyte character.
  (should-error (quoted-printable-encode-string "å")))

(provide 'qp-tests)
;;; qp-tests.el ends here
