;; news.el --- keypad and function key bindings for the Sony NEWS keyboard

;; Author: FSF
;; Keywords: terminals

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

;;; Commentary:

;;; Uses the Emacs 19 terminal initialization features --- won't work with 18.

;;; Code:

(if (boundp 'news-fkey-prefix)
    nil
  ;; The terminal initialization should already have set up some keys
  (setq news-fkey-prefix (lookup-key function-key-map "\eO"))
  (if (not (keymapp news-fkey-prefix))
      (error "What?  Your news termcap/terminfo has no keycaps in it."))

  ;; Termcap or terminfo will set these
  ;; (define-key news-fkey-prefix "P" [f1])
  ;; (define-key news-fkey-prefix "Q" [f2])
  ;; (define-key news-fkey-prefix "R" [f3])
  ;; (define-key news-fkey-prefix "S" [f4])
  ;; (define-key news-fkey-prefix "T" [f5])
  ;; (define-key news-fkey-prefix "U" [f6])
  ;; (define-key news-fkey-prefix "V" [f7])
  ;; (define-key news-fkey-prefix "W" [f8])
  ;; (define-key news-fkey-prefix "X" [f9])
  ;; (define-key news-fkey-prefix "Y" [f10])

  ;; Terminfo will set these
  (define-key news-fkey-prefix "a" [execute])
  (define-key news-fkey-prefix "b" [select])
  (define-key news-fkey-prefix "c" [cancel])
  (define-key news-fkey-prefix "M" [kp-enter])
  (define-key news-fkey-prefix "q" [kp-1])
  (define-key news-fkey-prefix "s" [kp-3])
  (define-key news-fkey-prefix "u" [kp-5])
  (define-key news-fkey-prefix "w" [kp-7])
  (define-key news-fkey-prefix "y" [kp-9])

  ;; These aren't in either termcap or terminfo's repertoire
  (define-key news-fkey-prefix "m" [kp-subtract])
  (define-key news-fkey-prefix "k" [kp-add])
  (define-key news-fkey-prefix "l" [kp-separator])
  (define-key news-fkey-prefix "n" [kp-period])
  (define-key news-fkey-prefix "p" [kp-0])
  (define-key news-fkey-prefix "r" [kp-2])
  (define-key news-fkey-prefix "t" [kp-4])
  (define-key news-fkey-prefix "v" [kp-6])
  (define-key news-fkey-prefix "x" [kp-8])
  )

;;; news.el ends here
