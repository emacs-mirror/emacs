;; keypad and function key bindings for the Sony NEWS keyboard.
;; Copyright (C) 1989 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;; This file places entries in function-key-map for the raw escape
;; sequences of various keypad and function keys, binding them to
;; their symbolic forms.

(define-prefix-command 'news-fkey-prefix 'news-fkey-map)
(define-key function-key-map "\eO" 'news-fkey-prefix)

(define-key news-fkey-map "P" [f1])
(define-key news-fkey-map "Q" [f2])
(define-key news-fkey-map "R" [f3])
(define-key news-fkey-map "S" [f4])
(define-key news-fkey-map "T" [f5])
(define-key news-fkey-map "U" [f6])
(define-key news-fkey-map "V" [f7])
(define-key news-fkey-map "W" [f8])
(define-key news-fkey-map "X" [f9])
(define-key news-fkey-map "Y" [f10])
(define-key news-fkey-map "m" [kp-subtract])
(define-key news-fkey-map "k" [kp-add])
(define-key news-fkey-map "l" [kp-separator])
(define-key news-fkey-map "n" [kp-period])
(define-key news-fkey-map "M" [kp-enter])
(define-key news-fkey-map "p" [kp-0])
(define-key news-fkey-map "q" [kp-1])
(define-key news-fkey-map "r" [kp-2])
(define-key news-fkey-map "s" [kp-3])
(define-key news-fkey-map "t" [kp-4])
(define-key news-fkey-map "u" [kp-5])
(define-key news-fkey-map "v" [kp-6])
(define-key news-fkey-map "w" [kp-7])
(define-key news-fkey-map "x" [kp-8])
(define-key news-fkey-map "y" [kp-9])
(define-key news-fkey-map "a" [execute])
(define-key news-fkey-map "b" [select])
(define-key news-fkey-map "c" [cancel])
