;;; vt100.el --- define VT100 function key sequences in function-key-map

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

;; Uses the Emacs 19 terminal initialization features --- won't work with 18.

;; Handles all VT100 clones, including the Apollo terminal.  Also handles
;; the VT200 --- its PF- and arrow- keys are different, but all those
;; are really set up by the terminal initialization code, which mines them
;; out of termcap.  This package is here to define the keypad comma, dash
;; and period (which aren't in termcap's repertoire) and the function for
;; changing from 80 to 132 columns & vv.

;;; Code:

;; CSI sequences - those that start with "\e[".
;; Termcap or terminfo should set these up automatically
;; (if (boundp 'vt100-CSI-prefix)
;;     nil
;;   (define-prefix-command 'vt100-CSI-prefix)
;;   (define-key function-key-map "\e[" 'vt100-CSI-prefix)
;; 
;;   (define-key vt100-CSI-prefix "A" [up])
;;   (define-key vt100-CSI-prefix "B" [down])
;;   (define-key vt100-CSI-prefix "C" [right])
;;   (define-key vt100-CSI-prefix "D" [left])
;;   )

;; SS3 sequences - those that start with "\eO".
(if (boundp 'vt100-SS3-prefix)
    nil
  ;; The terminal initialization should already have set up some keys
  (setq vt100-SS3-prefix (lookup-key function-key-map "\eO"))
  (if (not (keymapp vt100-SS3-prefix))
      (error "What?  Your VT100 termcap/terminfo has no keycaps in it."))

  ;; These will typically be set up automatically by termcap or terminfo
  ;;   (define-key vt100-SS3-prefix "A" [up]) ; up-arrow
  ;;   (define-key vt100-SS3-prefix "B" [down]) ; down-arrow
  ;;   (define-key vt100-SS3-prefix "C" [right]) ; right-arrow
  ;;   (define-key vt100-SS3-prefix "D" [left]) ; left-arrow
  ;;   (define-key vt100-SS3-prefix "P" [kp-f1]) ; PF1  
  ;;   (define-key vt100-SS3-prefix "Q" [kp-f2]) ; PF2  
  ;;   (define-key vt100-SS3-prefix "R" [kp-f3]) ; PF3  
  ;;   (define-key vt100-SS3-prefix "S" [kp-f4]) ; PF4  

  ;; Terminfo might set these
  (define-key vt100-SS3-prefix "M" [kp-enter]) ; Enter
  (define-key vt100-SS3-prefix "p" [kp-0]) ; 0
  (define-key vt100-SS3-prefix "q" [kp-1]) ; 1
  (define-key vt100-SS3-prefix "r" [kp-2]) ; 2
  (define-key vt100-SS3-prefix "s" [kp-3]) ; 3
  (define-key vt100-SS3-prefix "t" [kp-4]) ; 4
  (define-key vt100-SS3-prefix "u" [kp-5]) ; 5
  (define-key vt100-SS3-prefix "v" [kp-6]) ; 6
  (define-key vt100-SS3-prefix "w" [kp-7]) ; 7
  (define-key vt100-SS3-prefix "x" [kp-8]) ; 8
  (define-key vt100-SS3-prefix "y" [kp-9]) ; 9

  ;; Neither termcap nor terminfo will set these
  (define-key vt100-SS3-prefix "l" [kp-separator]) ; ,
  (define-key vt100-SS3-prefix "m" [kp-subtract]) ; -
  (define-key vt100-SS3-prefix "n" [kp-period])	; .
  )

;;; Controlling the screen width.
(defconst vt100-wide-mode (= (frame-width) 132)
  "t if vt100 is in 132-column mode.")

(defun vt100-wide-mode (&optional arg)
  "Toggle 132/80 column mode for vt100s."
 (interactive "P")
 (setq vt100-wide-mode 
	(if (null arg) (not vt100-wide-mode)
	  (> (prefix-numeric-value arg) 0)))
 (send-string-to-terminal (if vt100-wide-mode "\e[?3h" "\e[?3l"))
 (set-frame-width (if vt100-wide-mode 132 80)))

;;; vt100.el ends here
