;;;; Define VT100 function key escape sequences in function-key-map.


;;; CSI sequences - those that start with "\e[".
(define-prefix-command 'vt100-CSI-prefix 'vt100-CSI-map)
(define-key function-key-map "\e[" 'vt100-CSI-prefix)

(define-key vt100-CSI-map "A" [up])
(define-key vt100-CSI-map "B" [down])
(define-key vt100-CSI-map "C" [right])
(define-key vt100-CSI-map "D" [left])

(defun enable-arrow-keys ()
  "Enable the use of the VT100 arrow keys for cursor motion.
Because of the nature of the VT100, this unavoidably breaks
the standard Emacs command ESC [; therefore, it is not done by default,
but only if you give this command."
  (interactive)
  (global-unset-key "\e["))



;;; SS3 sequences - those that start with "\eO".
(define-prefix-command 'vt100-SS3-prefix 'vt100-SS3-map)
(define-key function-key-map "\eO" 'vt100-SS3-prefix)

(define-key vt100-SS3-map "A" [up])
(define-key vt100-SS3-map "B" [down])		; down-arrow
(define-key vt100-SS3-map "C" [right])		; right-arrow
(define-key vt100-SS3-map "D" [left])		; left-arrow
(define-key vt100-SS3-map "M" [kp-enter])       ; Enter
(define-key vt100-SS3-map "P" [kp-f1])	   	; PF1  
(define-key vt100-SS3-map "Q" [kp-f2])	   	; PF2  
(define-key vt100-SS3-map "R" [kp-f3])	   	; PF3  
(define-key vt100-SS3-map "S" [kp-f4])	   	; PF4  
(define-key vt100-SS3-map "l" [kp-separator])   ; ,
(define-key vt100-SS3-map "m" [kp-subtract])    ; -
(define-key vt100-SS3-map "n" [kp-period])	; .
(define-key vt100-SS3-map "p" [kp-0])		; 0
(define-key vt100-SS3-map "q" [kp-1])		; 1
(define-key vt100-SS3-map "r" [kp-2])		; 2
(define-key vt100-SS3-map "s" [kp-3])		; 3
(define-key vt100-SS3-map "t" [kp-4])		; 4
(define-key vt100-SS3-map "u" [kp-5])		; 5
(define-key vt100-SS3-map "v" [kp-6])		; 6
(define-key vt100-SS3-map "w" [kp-7])		; 7
(define-key vt100-SS3-map "x" [kp-8])		; 8
(define-key vt100-SS3-map "y" [kp-9])		; 9
					   	   

;;; Controlling the screen width.
(defconst vt100-wide-mode (= (screen-width) 132)
  "t if vt100 is in 132-column mode.")

(defun vt100-wide-mode (&optional arg)
  "Toggle 132/80 column mode for vt100s."
 (interactive "P")
 (setq vt100-wide-mode 
	(if (null arg) (not vt100-wide-mode)
	  (> (prefix-numeric-value arg) 0)))
 (send-string-to-terminal (if vt100-wide-mode "\e[?3h" "\e[?3l"))
 (set-screen-width (if vt100-wide-mode 132 80)))
