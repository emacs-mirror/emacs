;;; AT386.el --- terminal support package for IBM AT keyboards -*- no-byte-compile: t -*-

;; Author: Eric S. Raymond <esr@snark.thyrsus.com>
;; Keywords: terminals

;; Copyright (C) 1992 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Uses the Emacs 19 terminal initialization features --- won't work with 18.

;;; Code:

(if (boundp 'AT386-keypad-map)
    nil
  ;; The terminal initialization should already have set up some keys
  (setq AT386-keypad-map (lookup-key function-key-map "\e["))
  (if (not (keymapp AT386-keypad-map))
      (error "What?  Your AT386 termcap/terminfo has no keycaps in it"))

  ;; Equivalents of these are set up automatically by termcap/terminfo
  ;;  (define-key AT386-keypad-map "A" [up])
  ;;  (define-key AT386-keypad-map "B" [down])
  ;;  (define-key AT386-keypad-map "C" [right])
  ;;  (define-key AT386-keypad-map "D" [left])

  ;; These would be set up by terminfo, but not termcap
  (define-key AT386-keypad-map "H" [home])
  (define-key AT386-keypad-map "Y" [end])
  (define-key AT386-keypad-map "U" [next])	;; PgDn
  (define-key AT386-keypad-map "V" [prior])	;; PgUp
  (define-key AT386-keypad-map "@" [insert])	;; Ins key

  ;; These are not normally set up by either
  (define-key AT386-keypad-map "G" [kp-5])	;; Unlabeled center key
  (define-key AT386-keypad-map "S" [kp-subtract])
  (define-key AT386-keypad-map "T" [kp-add])

  ;; Arrange for the ALT key to be equivalent to ESC
  (define-key function-key-map "\eN" [27]) ; ALT map
  )

;;; arch-tag: abec1b03-582f-49f8-b8cb-e2fd52ea4bd7
;;; AT386.el ends here
