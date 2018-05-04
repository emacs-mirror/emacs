;;; erc-track-tests.el --- Tests for erc-track.

;; Copyright (C) 2016-2018 Free Software Foundation, Inc.

;; Author: Mario Lang <mlang@delysid.org>
;; Author: Vivek Dasmohapatra <vivek@etla.org>

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
(require 'erc-track)
(require 'font-core)

(ert-deftest erc-track--shorten-aggressive-nil ()
  "Test non-aggressive erc track buffer name shortening."
  (let (erc-track-shorten-aggressively)
    (should
     (equal (erc-unique-channel-names '("#emacs" "#vi" "#electronica" "#folk")
                                      '("#emacs" "#vi"))
            '("#em" "#vi")))
    (should
     (equal (erc-unique-channel-names '("#linux-de" "#linux-fr")
                                      '("#linux-de" "#linux-fr"))
            '("#linux-de" "#linux-fr")))
    (should
     (equal (erc-unique-channel-names
             '("#dunnet" "#lisp" "#sawfish" "#fsf" "#guile" "#testgnome"
               "#gnu" "#fsbot" "#hurd" "#hurd-bunny" "#emacs")
             '("#hurd-bunny" "#hurd" "#sawfish" "#lisp"))
            '("#hurd-" "#hurd" "#s" "#l")))
    (should
     (equal (erc-unique-substrings '("#emacs" "#vi" "#electronica" "#folk"))
            '("#em" "#vi" "#el" "#f")))
    (should
     (equal (erc-unique-channel-names
             '("#emacs" "#burse" "+linux.de" "#starwars"
               "#bitlbee" "+burse" "#ratpoison")
             '("+linux.de" "#starwars" "#burse"))
            '("+l" "#s" "#bu")))
    (should
     (equal (erc-unique-channel-names '("fsbot" "#emacs" "deego") '("fsbot"))
            '("fs")))
    (should
     (equal (erc-unique-channel-names '("fsbot" "#emacs" "deego")
                                      '("fsbot")
                                      (lambda (s) (> (length s) 4)) 1)
            '("f")))
    (should
     (equal (erc-unique-channel-names '("fsbot" "#emacs" "deego")
                                      '("fsbot")
                                      (lambda (s) (> (length s) 4)) 2)
            '("fs")))
    (should
     (equal (erc-unique-channel-names '("deego" "#hurd" "#hurd-bunny" "#emacs")
                                      '("#hurd" "#hurd-bunny"))
            '("#hurd" "#hurd-")))
    (should
     (and
      (equal (erc-unique-substring-1 "abc" '("ab" "abcd")) "abcd")
      (not   (erc-unique-substring-1 "a" '("xyz" "xab")))
      (equal (erc-unique-substrings '("abc" "xyz" "xab")) '("abc" "xyz" "xab"))
      (equal (erc-unique-substrings '("abc" "abcdefg")) '("abc" "abcd")))) ))

(ert-deftest erc-track--shorten-aggressive-t ()
  "Test aggressive erc track buffer name shortening."
  (let ((erc-track-shorten-aggressively t))
    (should
     (equal (erc-unique-channel-names '("#emacs" "#vi" "#electronica" "#folk")
                                      '("#emacs" "#vi"))
            '("#em" "#v")))
    (should
     (equal (erc-unique-channel-names '("#linux-de" "#linux-fr")
                                      '("#linux-de" "#linux-fr"))
            '("#linux-d" "#linux-f")))
    (should
     (equal (erc-unique-substrings '("#emacs" "#vi" "#electronica" "#folk"))
            '("#em" "#v" "#el" "#f")))
    (should
     (and
      (equal (erc-unique-substring-1 "abc" '("ab" "abcd")) "abcd")
      (not   (erc-unique-substring-1 "a" '("xyz" "xab")))
      (equal (erc-unique-substrings '("abc" "xyz" "xab")) '("ab" "xy" "xa"))
      (equal (erc-unique-substrings '("abc" "abcdefg")) '("abc" "abcd")))) ))

(ert-deftest erc-track--shorten-aggressive-max ()
  "Test maximally aggressive erc track buffer name shortening."
  (let ((erc-track-shorten-aggressively 'max))
    (should
     (equal (erc-unique-channel-names '("#emacs" "#vi" "#electronica" "#folk")
                                      '("#emacs" "#vi"))
            '("#e" "#v"))) ))

(ert-deftest erc-track--erc-faces-in ()
  "`erc-faces-in' should pick up both 'face and 'font-lock-face properties."
  (let ((str0 "is bold")
        (str1 "is bold"))
    ;; Turn on Font Lock mode: this initialize `char-property-alias-alist'
    ;; to '((face font-lock-face)).  Note that `font-lock-mode' don't
    ;; turn on the mode if the test is run on batch mode or if the
    ;; buffer name starts with ?\s (Bug#23954).
    (unless font-lock-mode (font-lock-default-function 1))
    (put-text-property 3 (length str0) 'font-lock-face
                       '(bold erc-current-nick-face) str0)
    (put-text-property 3 (length str1) 'face
                       '(bold erc-current-nick-face) str1)
    (should (erc-faces-in str0))
    (should (erc-faces-in str1)) ))
