;;; Terminal support for the Televideo 970.
;;; Jim Blandy <jimb@occs.cs.oberlin.edu>, January 1992


;;; Define the escape codes sent by the function keys.
(or (lookup-key function-key-map "\e[")
    (define-key function-key-map "\e[" (make-keymap)))
(or (lookup-key function-key-map "\eO")
    (define-key function-key-map "\eO" (make-keymap)))

;; Miscellaneous keys
(mapcar (function (lambda (key-binding)
		    (define-key function-key-map
		      (car key-binding) (nth 1 key-binding))))
	'(("\e[H" [home])
	  ("\e[Z" [backtab])
	  ("\e[i" [print])
	  ("\e[2J" [clear])
	  ("\e[@" [insert])
	  ("\e[P" [delete])
	  ("\e[L" [insertline])
	  ("\e[M" [deleteline])
	  ("\e[K" [eraseline])
	  ("\e[J" [erasepage])
	  ("\e[U" [page])
	  ("\e[g" [S-tab])
	  ("\e[2N" [clearentry])
	  ("\e[2K" [S-clearentry])
	  ("\e[E" [?\C-j])
	  ("\e[g" [S-backtab])
	  ("\e[?1i" [S-print])
	  ("\e[4h" [S-insert])
	  ("\e[4l" [S-delete])
	  ("\e[Q"  [S-insertline])
	  ("\e[1Q" [S-deleteline])
	  ("\e[19l" [S-eraseline])
	  ("\e[19h" [S-erasepage])
	  ("\e[V" [S-page])
	  ("\eS" [send])
	  ("\e5" [S-send])
	  ("\eOm" [kp-subtract])
	  ("\eOl" [kp-separator])
	  ("\eOn" [kp-decimal])
	  ("\eOM" [enter])
	  ("\eOP" [kp-f1])
	  ("\eOQ" [kp-f2])
	  ("\eOR" [kp-f3])
	  ("\eOS" [kp-f4])))
;; The numeric keypad keys.
(let ((i 0))
  (while (< i 10)
    (define-key function-key-map
      (format "\eO%c" (+ i ?p))
      (vector (intern (format "kp-%d" i))))
    (setq i (1+ i))))
;; The numbered function keys.
(let ((i 0))
  (while (< i 16)
    (define-key function-key-map
      (format "\e?%c" (+ i ?a))
      (vector (intern (format "f%d" (1+ i)))))
    (define-key function-key-map
      (format "\e?%c" (+ i ?A))
      (vector (intern (format "S-f%d" (1+ i)))))
    (setq i (1+ i))))


;;; Should keypad numbers send ordinary digits or distinct escape sequences?
(defvar tvi970-keypad-numeric nil
  "The terminal should be in numeric keypad mode iff this variable is non-nil.
Do not set this variable!  Call the function ``tvi970-set-keypad-mode''.")

(defun tvi970-set-keypad-mode (&optional arg)
  "Set the current mode of the TVI 970 numeric keypad.
In ``numeric keypad mode'', the number keys on the keypad act as
ordinary digits.  In ``alternate keypad mode'', the keys send distinct
escape sequences, meaning that they can have their own bindings,
independent of the normal number keys.
With no argument, toggle between the two possible modes.
With a positive argument, select alternate keypad mode.
With a negative argument, select numeric keypad mode."
  (interactive "P")
  (setq tvi970-keypad-numeric 
	(if (null arg)
	    (not tvi970-keypad-numeric)
	  (> (prefix-numeric-value arg) 0)))
  (send-string-to-terminal (if tvi970-keypad-numeric "\e=" "\e>")))

(tvi970-set-keypad-mode 1)
