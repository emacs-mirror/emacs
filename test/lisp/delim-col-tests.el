;;; delim-col-tests.el --- Tests for delim-col.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Free Software Foundation, Inc.

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
(require 'delim-col)

(ert-deftest delim-col-tests-delimit-columns ()
  (with-temp-buffer
    (insert "a	b	c\n")
    (delimit-columns-region (point-min) (point-max))
    (should (equal (buffer-string) "a, b, c\n")))
  (with-temp-buffer
    (insert "a	b	c	d\n"
            "aaaa	bb	ccc	ddddd\n"
            "aaa	bbb	cccc	dddd\n"
            "aa	bb	ccccccc	ddd\n")
    (delimit-columns-region (point-min) (point-max))
    (should (equal (buffer-string)
                   (concat "a,    b,   c,       d    \n"
                           "aaaa, bb,  ccc,     ddddd\n"
                           "aaa,  bbb, cccc,    dddd \n"
                           "aa,   bb,  ccccccc, ddd  \n")))))

(ert-deftest delim-col-tests-delimit-rectangle ()
  (with-temp-buffer
    (insert "a	b	c	d\n"
            "aaaa	bb	ccc	ddddd\n"
            "aaa	bbb	cccc	dddd\n"
            "aa	bb	ccccccc	ddd\n")
    (delimit-columns-rectangle 3 58) ; from first b to last c
    (should (equal (buffer-string)
                   (concat "a	b,   c      	d\n"
                           "aaaa	bb,  ccc    	ddddd\n"
                           "aaa	bbb, cccc   	dddd\n"
                           "aa	bb,  ccccccc	ddd\n")))))

(ert-deftest delim-col-tests-delimit-columns-str-separator ()
  (let ((delimit-columns-str-separator ":"))
    (with-temp-buffer
      (insert "a	b\n")
      (delimit-columns-region (point-min) (point-max))
      (should (equal (buffer-string) "a:b\n")))
    (with-temp-buffer
      (insert "a	b	c	d\n"
              "aa	bb	cc	dd\n")
      (delimit-columns-rectangle 3 16) ; from first b to last c
      (should (equal (buffer-string)
                     (concat "a	b: c	d\n"
                             "aa	bb:cc	dd\n"))))))

(ert-deftest delim-col-tests-delimit-columns-str-before-after ()
  (let ((delimit-columns-str-before "[ ")
        (delimit-columns-str-after " ]"))
    (with-temp-buffer
      (insert "a	b	c\n")
      (delimit-columns-region (point-min) (point-max))
      (should (equal (buffer-string) "[ a, b, c ]\n")))
    (with-temp-buffer
      (insert "a	b	c	d\n"
              "aaaa	bb	ccc	ddddd\n"
              "aaa	bbb	cccc	dddd\n"
              "aa	bb	ccccccc	ddd\n")
      (delimit-columns-region (point-min) (point-max))
      (should (equal (buffer-string)
                     (concat "[ a,    b,   c,       d     ]\n"
                             "[ aaaa, bb,  ccc,     ddddd ]\n"
                             "[ aaa,  bbb, cccc,    dddd  ]\n"
                             "[ aa,   bb,  ccccccc, ddd   ]\n"))))
    (with-temp-buffer
      (insert "a	b	c	d\n"
              "aaaa	bb	ccc	ddddd\n"
              "aaa	bbb	cccc	dddd\n"
              "aa	bb	ccccccc	ddd\n")
     (delimit-columns-rectangle 3 58)   ; from first b to last c
     (should (equal (buffer-string)
                    (concat "a	[ b,   c       ]	d\n"
                            "aaaa	[ bb,  ccc     ]	ddddd\n"
                            "aaa	[ bbb, cccc    ]	dddd\n"
                            "aa	[ bb,  ccccccc ]	ddd\n"))))))

(ert-deftest delim-col-tests-delimit-colummns-before-after ()
  (let ((delimit-columns-before "<")
        (delimit-columns-after ">"))
    (with-temp-buffer
      (insert "a	b\n")
      (delimit-columns-region (point-min) (point-max))
      (should (equal (buffer-string) "<a>, <b>\n")))
    (with-temp-buffer
      (insert "a	b	c	d\n"
              "aa	bb	cc	dd\n")
      (delimit-columns-rectangle 3 17)
      (should (equal (buffer-string)
                     (concat "a	<b>,  <c> 	d\n"
                             "aa	<bb>, <cc>	dd\n"))))))

(ert-deftest delim-col-tests-delimit-columns-separator ()
  (let ((delimit-columns-separator ","))
    (with-temp-buffer
      (insert "a,b,c\n")
      (delimit-columns-region (point-min) (point-max))
      (should (equal (buffer-string) "a, b, c\n")))))

(ert-deftest delim-col-tests-delimit-columns-format/nil ()
  (let ((delimit-columns-format nil))
    (with-temp-buffer
      (insert "a	b\n"
              "aa	bb\n")
      (delimit-columns-region (point-min) (point-max))
      (should (equal (buffer-string)
                     (concat "a, b\n"
                             "aa, bb\n"))))
    (with-temp-buffer
      (insert "a	b	c	d\n"
              "aa	bb	cc	dd\n")
      (delimit-columns-rectangle 3 17) ; from first b to last c
      (should (equal (buffer-string)
                     (concat "a	b, c	d\n"
                             "aa	bb, cc	dd\n"))))))

(ert-deftest delim-col-tests-delimit-columns-format/separator ()
  (let ((delimit-columns-format 'separator)
        (delimit-columns-before "<")
        (delimit-columns-after ">"))
    (with-temp-buffer
      (insert "a	b\n"
              "aa	bb\n")
      (delimit-columns-region (point-min) (point-max))
      (should (equal (buffer-string)
                     (concat "<a> , <b> \n"
                             "<aa>, <bb>\n"))))
    (with-temp-buffer
      (insert "a	b	c	d\n"
              "aa	bb	cc	dd\n")
      (delimit-columns-rectangle 3 17) ; from first b to last c
      (should (equal (buffer-string)
                     (concat "a	<b> , <c> 	d\n"
                             "aa	<bb>, <cc>	dd\n"))))))

(ert-deftest delim-col-tests-delimit-columns-format/padding ()
  (let ((delimit-columns-format 'padding)
        (delimit-columns-before "<")
        (delimit-columns-after ">"))
    (with-temp-buffer
      (insert "a	b\n"
              "aa	bb\n")
      (delimit-columns-region (point-min) (point-max))
      (should (equal (buffer-string)
                     (concat "<a >, <b >\n"
                             "<aa>, <bb>\n"))))
    (with-temp-buffer
      (insert "a	b	c	d\n"
              "aa	bb	cc	dd\n")
      (delimit-columns-rectangle 3 17)  ; from first b to last c
      (should (equal (buffer-string)
                     (concat "a	<b >, <c >	d\n"
                             "aa	<bb>, <cc>	dd\n"))))))

(provide 'delim-col-tests)
;;; delim-col-tests.el ends here
