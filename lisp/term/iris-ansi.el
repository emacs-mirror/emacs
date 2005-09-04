;;; iris-ansi.el --- configure Emacs for SGI xwsh and winterm apps -*- no-byte-compile: t -*-

;; Copyright (C) 1997, 2002, 2003, 2004, 2005 Free Software Foundation, Inc.

;; Author: Dan Nicolaescu <dann@ics.uci.edu>

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

;;; Code:

(defvar iris-function-map nil
  "Function key definitions for SGI xwsh and winterm apps.")

;; Make reloads faster.
(unless iris-function-map
  (setq iris-function-map (make-sparse-keymap))

  (define-key iris-function-map "\e[120q" [S-escape])
  (define-key iris-function-map "\e[121q" [C-escape])

  (define-key iris-function-map "\e[001q" [f1])
  (define-key iris-function-map "\e[013q" [S-f1])
  (define-key iris-function-map "\e[025q" [C-f1])


  (define-key iris-function-map "\e[002q" [f2])
  (define-key iris-function-map "\e[014q" [S-f2])
  (define-key iris-function-map "\e[026q" [C-f2])
  (define-key iris-function-map "\e[038q" [M-f2])

  (define-key iris-function-map "\e[003q" [f3])
  (define-key iris-function-map "\e[015q" [S-f3])
  (define-key iris-function-map "\e[027q" [C-f3])


  (define-key iris-function-map "\e[004q" [f4])
  (define-key iris-function-map "\e[016q" [S-f4])
  (define-key iris-function-map "\e[028q" [C-f4])


  (define-key iris-function-map "\e[005q" [f5])
  (define-key iris-function-map "\e[017q" [S-f5])
  (define-key iris-function-map "\e[029q" [C-f5])


  (define-key iris-function-map "\e[006q" [f6])
  (define-key iris-function-map "\e[018q" [S-f6])
  (define-key iris-function-map "\e[030q" [C-f6])


  (define-key iris-function-map "\e[007q" [f7])
  (define-key iris-function-map "\e[019q" [S-f7])
  (define-key iris-function-map "\e[031q" [C-f7])


  (define-key iris-function-map "\e[008q" [f8])
  (define-key iris-function-map "\e[020q" [S-f8])
  (define-key iris-function-map "\e[032q" [C-f8])


  (define-key iris-function-map "\e[009q" [f9])
  (define-key iris-function-map "\e[021q" [S-f9])
  (define-key iris-function-map "\e[033q" [C-f9])


  (define-key iris-function-map "\e[010q" [f10])
  (define-key iris-function-map "\e[022q" [S-f10])
  (define-key iris-function-map "\e[034q" [C-f10])


  (define-key iris-function-map "\e[011q" [f11])
  (define-key iris-function-map "\e[023q" [S-f11])
  (define-key iris-function-map "\e[035q" [C-f11])
  (define-key iris-function-map "\e[047q" [M-f11])

  (define-key iris-function-map "\e[012q" [f12])
  (define-key iris-function-map "\e[024q" [S-f12])
  (define-key iris-function-map "\e[036q" [C-f12])
  (define-key iris-function-map "\e[048q" [M-f12])


  (define-key iris-function-map "\e[057q" [C-`])
  (define-key iris-function-map "\e[115q" [M-`])

  (define-key iris-function-map "\e[049q" [?\C-1])
  (define-key iris-function-map "\e[058q" [?\M-1])


  (define-key iris-function-map "\e[059q" [?\M-2])

  (define-key iris-function-map "\e[050q" [?\C-3])
  (define-key iris-function-map "\e[060q" [?\M-3])

  (define-key iris-function-map "\e[051q" [?\C-4])
  (define-key iris-function-map "\e[061q" [?\M-4])

  (define-key iris-function-map "\e[052q" [?\C-5])
  (define-key iris-function-map "\e[062q" [?\M-5])


  (define-key iris-function-map "\e[063q" [?\M-6])

  (define-key iris-function-map "\e[053q" [?\C-7])
  (define-key iris-function-map "\e[064q" [?\M-7])

  (define-key iris-function-map "\e[054q" [?\C-8])
  (define-key iris-function-map "\e[065q" [?\M-8])

  (define-key iris-function-map "\e[055q" [?\C-9])
  (define-key iris-function-map "\e[066q" [?\M-9])

  (define-key iris-function-map "\e[056q" [?\C-0])
  (define-key iris-function-map "\e[067q" [?\M-0])

  (define-key iris-function-map "\e[068q" [?\M--])

  (define-key iris-function-map "\e[069q" [?\C-=])
  (define-key iris-function-map "\e[070q" [?\M-=])

  ;; I don't know what to do with those.
  ;;(define-key iris-function-map "^H" [<del>])
  ;;(define-key iris-function-map "^H" [S-<del>])
  ;;(define-key iris-function-map "\177" [C-<del>])
  ;;(define-key iris-function-map "\e[071q" [M-<del>])

  (define-key iris-function-map "\e[Z" [?\S-\t])
  (define-key iris-function-map "\e[072q" [?\C-\t])
  ;; This only works if you remove the M-TAB keybing from the system.4Dwmrc
  ;; our your ~/.4Dwmrc, if you use the 4Dwm window manager.
  (define-key iris-function-map "\e[073q" [?\M-\t])

  (define-key iris-function-map "\e[074q" [?\M-q])

  (define-key iris-function-map "\e[075q" [?\M-w])

  (define-key iris-function-map "\e[076q" [?\M-e])

  (define-key iris-function-map "\e[077q" [?\M-r])

  (define-key iris-function-map "\e[078q" [?\M-t])

  (define-key iris-function-map "\e[079q" [?\M-y])

  (define-key iris-function-map "\e[080q" [?\M-u])

  (define-key iris-function-map "\e[081q" [?\M-i])

  (define-key iris-function-map "\e[082q" [?\M-o])

  (define-key iris-function-map "\e[083q" [?\M-p])

  (define-key iris-function-map "\e[084q" [?\M-\[])

  (define-key iris-function-map "\e[085q" [?\M-\]])

  (define-key iris-function-map "\e[086q" [?\M-\\])

  (define-key iris-function-map "\e[087q" [?\M-a])

  (define-key iris-function-map "\e[088q" [?\M-s])

  (define-key iris-function-map "\e[089q" [?\M-d])

  (define-key iris-function-map "\e[090q" [?\M-f])

  (define-key iris-function-map "\e[091q" [?\M-g])

  (define-key iris-function-map "\e[092q" [?\M-h])

  (define-key iris-function-map "\e[093q" [?\M-j])

  (define-key iris-function-map "\e[094q" [?\M-k])

  (define-key iris-function-map "\e[095q" [?\M-l])

  (define-key iris-function-map "\e[096q" [?\C-\;])
  (define-key iris-function-map "\e[097q" [?\M-:]) ;; we are cheating
					          ;; here, this is realy
						  ;; M-;, but M-:
						  ;; generates the same
						  ;; string and is more
						  ;; usefull.

  (define-key iris-function-map "\e[098q" [?\C-'])
  (define-key iris-function-map "\e[099q" [?\M-'])

  (define-key iris-function-map "\e[100q" [?\M-\n])

  (define-key iris-function-map "\e[101q" [?\M-z])

  (define-key iris-function-map "\e[102q" [?\M-x])

  (define-key iris-function-map "\e[103q" [?\M-c])

  (define-key iris-function-map "\e[104q" [?\M-v])

  (define-key iris-function-map "\e[105q" [?\M-b])

  (define-key iris-function-map "\e[106q" [M-n])

  (define-key iris-function-map "\e[107q" [M-m])

  (define-key iris-function-map "\e[108q" [?\C-,])
  (define-key iris-function-map "\e[109q" [?\M-,])

  (define-key iris-function-map "\e[110q" [?\C-.])
  (define-key iris-function-map "\e[111q" [?\M-.])

  (define-key iris-function-map "\e[112q" [?\C-/])
  (define-key iris-function-map "\e[113q" [?\M-/])

  (define-key iris-function-map "\e[139q" [insert])
  (define-key iris-function-map "\e[139q" [S-insert])
  (define-key iris-function-map "\e[140q" [C-insert])
  (define-key iris-function-map "\e[141q" [M-insert])

  (define-key iris-function-map "\e[H" [home])
  (define-key iris-function-map "\e[143q" [S-home])
  (define-key iris-function-map "\e[144q" [C-home])


  (define-key iris-function-map "\e[150q" [prior])
  (define-key iris-function-map "\e[151q" [S-prior]) ;; those don't seem
						    ;; to generate
						    ;; anything
  (define-key iris-function-map "\e[152q" [C-prior])


  ;; (define-key iris-function-map "^?" [delete]) ?? something else seems to take care of this.
  (define-key iris-function-map "\e[P" [S-delete])
  (define-key iris-function-map "\e[142q" [C-delete])
  (define-key iris-function-map "\e[M" [M-delete])

  (define-key iris-function-map "\e[146q" [end])
  (define-key iris-function-map "\e[147q" [S-end]) ;; those don't seem to
						  ;; generate anything
  (define-key iris-function-map "\e[148q" [C-end])

  (define-key iris-function-map "\e[154q" [next])
  (define-key iris-function-map "\e[155q" [S-next])
  (define-key iris-function-map "\e[156q" [C-next])


  (define-key iris-function-map "\e[161q" [S-up])
  (define-key iris-function-map "\e[162q" [C-up])
  (define-key iris-function-map "\e[163q" [M-up])

  (define-key iris-function-map "\e[158q" [S-left])
  (define-key iris-function-map "\e[159q" [C-left])
  (define-key iris-function-map "\e[160q" [M-left])

  (define-key iris-function-map "\e[164q" [S-down])
  (define-key iris-function-map "\e[165q" [C-down])
  (define-key iris-function-map "\e[166q" [M-down])

  (define-key iris-function-map "\e[167q" [S-right])
  (define-key iris-function-map "\e[168q" [C-right])
  (define-key iris-function-map "\e[169q" [M-right])

  ;; Keypad functions, most of those are untested.
  (define-key iris-function-map "\e[179q" [?\C-/])
  (define-key iris-function-map "\e[180q" [?\M-/])

  (define-key iris-function-map "\e[187q" [?\C-*])
  (define-key iris-function-map "\e[188q" [?\M-*])

  (define-key iris-function-map "\e[198q" [?\C--])
  (define-key iris-function-map "\e[199q" [?\M--])

  ;; Something else takes care of home, up, prior, down, left, right, next
  ;(define-key iris-function-map "\e[H" [home])
  (define-key iris-function-map "\e[172q" [C-home])

  ;(define-key iris-function-map "\e[A" [up])
  (define-key iris-function-map "\e[182q" [C-up])


  ;(define-key iris-function-map "\e[150q" [prior])
  (define-key iris-function-map "\e[190q" [C-prior])


  (define-key iris-function-map "\e[200q" [?\C-+])
  (define-key iris-function-map "\e[201q" [?\M-+])

  ;(define-key iris-function-map "\e[D" [left])
  (define-key iris-function-map "\e[174q" [C-left])


  (define-key iris-function-map "\e[000q" [begin])
  (define-key iris-function-map "\e[184q" [C-begin])


  ;(define-key iris-function-map "\e[C" [right])
  (define-key iris-function-map "\e[192q" [C-right])

  ;(define-key iris-function-map "\e[146q" [end])
  (define-key iris-function-map "\e[176q" [C-end])

  ;(define-key iris-function-map "\e[B" [down])
  (define-key iris-function-map "\e[186q" [C-down])

  ;(define-key iris-function-map "\e[154q" [next])
  (define-key iris-function-map "\e[194q" [C-next])


  (define-key iris-function-map "\e[100q" [M-enter])

  (define-key iris-function-map "\e[139q" [insert])
  (define-key iris-function-map "\e[178q" [C-inset])

  (define-key iris-function-map "\e[P" [delete])
  (define-key iris-function-map "\e[196q" [C-delete])
  (define-key iris-function-map "\e[197q" [M-delete]))

(defun terminal-init-iris-ansi ()
  "Terminal initialization function for iris-ansi."
  ;; Use inheritance to let the main keymap override these defaults.
  ;; This way we don't override terminfo-derived settings or settings
  ;; made in the .emacs file.
  (let ((m (copy-keymap iris-function-map)))
    (set-keymap-parent m (keymap-parent (terminal-local-value 'local-function-key-map nil)))
    (set-keymap-parent (terminal-local-value 'local-function-key-map nil) m)))

;;; arch-tag: b1d0e73a-bb7d-47be-9fb2-6fb126469a1b
;;; iris-ansi.el ends here
