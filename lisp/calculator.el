;;; calculator.el --- A simple pocket calculator.

;; Copyright (C) 1998 by Free Software Foundation, Inc.

;; Author: Eli Barzilay <eli@lambda.cs.cornell.edu>
;; Keywords: tools, convenience
;; Time-stamp: <2000-02-01 20:12:16 eli>

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 2, or (at your option) any
;; later version.

;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:
;;
;; A simple pocket calculator for Emacs.
;; Why touch your mouse to get xcalc (or calc.exe), when you have Emacs?
;;
;; If this is not part of your Emacs distribution, then simply bind
;; `calculator' to a key and make it an autoloaded function, e.g.:
;;   (autoload 'calculator "calculator"
;;     "Run the pocket calculator." t)
;;   (global-set-key [(control return)] 'calculator)
;;
;; Written by Eli Barzilay: Maze is Life!  eli@cs.cornell.edu
;;                                         http://www.cs.cornell.edu/eli
;;
;; For latest version, check
;;     http://www.cs.cornell.edu/eli/misc/calculator.el


(eval-and-compile
  (if (fboundp 'defgroup) nil
    (defmacro defgroup (&rest forms) nil)
    (defmacro defcustom (s v d &rest r) (list 'defvar s v d))))

;;; Customization:

(defgroup calculator nil
  "Simple pocket calculator."
  :prefix "calculator"
  :group 'tools
  :group 'convenience)

(defcustom calculator-electric-mode nil
  "*Run `calculator' electrically, in the echo area.
Note that if you use electric-mode, you wouldn't be able to use
conventional help keys."
  :type  'boolean
  :group 'calculator)

(defcustom calculator-bind-escape nil
  "*If non-nil, set escape to exit the calculator."
  :type  'boolean
  :group 'calculator)

(defcustom calculator-unary-style 'postfix
  "*Value is either 'prefix or 'postfix.
This determines the default behavior of unary operators."
  :type    '(choice (const prefix) (const postfix))
  :group   'calculator)

(defcustom calculator-prompt "Calculator=%s> "
  "*The prompt used by the pocket calculator.
It should contain a \"%s\" somewhere that will indicate the i/o radixes,
this string will be a two-character string as described in the
documentation for `calculator-mode'."
  :type  'string
  :group 'calculator)

(defcustom calculator-epsilon 1e-15
  "*A threshold for results.
If any result computed in `calculator-funcall' is smaller than this in
its absolute value, then zero will be returned."
  :type  'number
  :group 'calculator)

(defcustom calculator-number-format "%1.3f"
  "*The calculator's string used to display normal numbers."
  :type  'string
  :group 'calculator)

(defcustom calculator-number-exp-ulimit 1e16
  "*The calculator's upper limit for normal numbers."
  :type  'number
  :group 'calculator)

(defcustom calculator-number-exp-llimit 0.001
  "*The calculator's lower limit for normal numbers."
  :type  'number
  :group 'calculator)

(defcustom calculator-number-exp-format "%g"
  "*The calculator's string used to display exponential numbers."
  :type  'string
  :group 'calculator)

(defcustom calculator-show-integers t
  "*Non-nil value means delete all zero digits after the decimal point."
  :type  'boolean
  :group 'calculator)

(defcustom calculator-2s-complement nil
  "*If non-nil, show negative numbers in 2s complement in radix modes.
Otherwise show as a negative number."
  :type  'boolean
  :group 'calculator)

(defcustom calculator-mode-hook nil
  "*List of hook functions run by `calculator-mode'."
  :type  'hook
  :group 'calculator)

(defcustom calculator-user-registers nil
  "*An association list of user-defined register bindings.

Each element in this list is a list of a character and a number that
will be stored in that character's register.

For example, use this to define the golden ratio number:
  (setq calculator-user-registers '((?g .  1.61803398875)))"
  :type  '(repeat (cons character number))
  :set   '(lambda (_ val)
            (and (boundp 'calculator-registers)
                 (setq calculator-registers
                       (append val calculator-registers)))
            (setq calculator-user-registers val))
  :group 'calculator)

(defcustom calculator-user-operators nil
  "*A list of additional operators.

This is a list in the same format as specified in the documentation for
`calculator-operators', that you can use to bind additional calculator
operators.  It is probably not a good idea to modify this value with
`customize' since it is too complex...

Examples:

* A very simple one, adding a postfix \"x-to-y\" convertion keys, using
  `t' as a prefix key:

  (setq calculator-user-operators
        '((\"tf\" cl-to-fr (+ 32 (/ (* X 9) 5)) 1)
          (\"tc\" fr-to-cl (/ (* (- X 32) 5) 9) 1)
          (\"tp\" kg-to-lb (/ X 0.453592)       1)
          (\"tk\" lb-to-kg (* X 0.453592)       1)
          (\"tF\" mt-to-ft (/ X 0.3048)         1)
          (\"tM\" ft-to-mt (* X 0.3048)         1)))

* Using a function-like form is very simple, X for an argument (Y the
  second in case of a binary operator), TX is a truncated version of X
  and F does a recursive call, Here is a [very inefficient] Fibonacci
  number calculation:

  (add-to-list 'calculator-user-operators
               '(\"F\" fib (if (<= TX 1)
                           1
                           (+ (F (- TX 1)) (F (- TX 2)))) 0))

  Note that this will be either postfix or prefix, according to
  `calculator-unary-style'."
  :type  '(repeat (list string symbol sexp integer integer))
  :group 'calculator)

;;; Code:

(defvar calculator-initial-operators
  '(;; these have keybindings of themselves, not calculator-ops
    (nobind "=" =  identity  1 -1)
    (nobind "+" +  +         2  4)
    (nobind "-" -  -         2  4)
    (nobind "+" +  +        -1  9)
    (nobind "-" -  -        -1  9)
    (nobind "(" \( identity -1 -1)
    (nobind ")" \) identity +1 10)
    ;; normal keys
    ("|"  or   (logior TX TY)  2  2)
    ("#"  xor  (logxor TX TY)  2  2)
    ("&"  and  (logand TX TY)  2  3)
    ("*"  *    *               2  5)
    ("/"  /    /               2  5)
    ("\\" div  (/ TX TY)       2  5)
    ("%"  rem  (% TX TY)       2  5)
    ("L"  log  log             2  6)
    ("S"  sin  (sin DX)        x  6)
    ("C"  cos  (cos DX)        x  6)
    ("T"  tan  (tan DX)        x  6)
    ("IS" asin (D (asin X))    x  6)
    ("IC" acos (D (acos X))    x  6)
    ("IT" atan (D (atan X))    x  6)
    ("Q"  sqrt sqrt            x  7)
    ("^"  ^    expt            2  7)
    ("!"  !    calculator-fact x  7)
    (";"  1/   (/ 1 X)         1  7)
    ("_"  -    -               1  8)
    ("~"  ~    (lognot TX)     x  8)
    (">"  repR calculator-repR 1  8)
    ("<"  repL calculator-repL 1  8)
    ("v"  avg  (/ (apply '+ L) (length L)) 0 8)
    ("l"  tot  (apply '+ L)    0 8)
    )
  "A list of initial operators.

This is a list in the same format as `calculator-operators'.  Whenever
`calculator' starts, it looks at the value of this variable, and if it
is not empty, its contents is prepended to `calculator-operators' and
the appropriate key bindings are made.

This variable is then reset to nil.  Don't use this if you want to add
user-defined operators, use `calculator-user-operators' instead.")

(defvar calculator-operators nil
  "The calculator operators, each a list with:

1. The key that is bound to for this operation (usually a string);

2. The displayed symbol for this function;

3. The function symbol, or a form that uses the variables `X' and `Y',
   (if it is a binary operator), `TX' and `TY' (truncated integer
   versions), `DX' (converted to radians if degrees mode is on), `D'
   (function for converting radians to degrees if deg mode is on), `L'
   (list of saved values), `F' (function for recursive iteration calls)
   and evaluates to the function value - these variables are capital;

4. The function's arity, optional, one of: 2=binary, -1=prefix unary,
   +1=postfix unary, 0=a 0-arg operator func, non-number=postfix/prefix
   as determined by `calculator-unary-style' (the default);

5. The function's precedence - should be in the range of 1=lowest to
   9=highest (optional, defaults to 1);

It it possible have a unary prefix version of a binary operator if it
comes later in this list.  If the list begins with the symbol 'nobind,
then no key binding will take place - this is only useful for predefined
keys.

Use `calculator-user-operators' to add operators to this list, see its
documentation for an example.")

(defvar calculator-stack nil
  "Stack contents - operations and operands.")

(defvar calculator-curnum nil
  "Current number being entered (as a string).")

(defvar calculator-stack-display nil
  "Cons of the stack and its string representation.")

(defvar calculator-char-radix
  '((?D . nil) (?B . bin) (?O . oct) (?H . hex) (?X . hex))
  "A table to convert input characters to corresponding radix symbols.")

(defvar calculator-output-radix nil
  "The mode for display, one of: nil (decimal), 'bin, 'oct or 'hex.")

(defvar calculator-input-radix nil
  "The mode for input, one of: nil (decimal), 'bin, 'oct or 'hex.")

(defvar calculator-deg nil
  "Non-nil if trig functions operate on degrees instead of radians.")

(defvar calculator-saved-list nil
  "A list of saved values collected.")

(defvar calculator-saved-ptr 0
  "The pointer to the current saved number.")

(defvar calculator-add-saved nil
  "Bound to t when a value should be added to the saved-list.")

(defvar calculator-display-fragile nil
  "When non-nil, we see something that the next digit should replace.")

(defvar calculator-buffer nil
  "The current calculator buffer.")

(defvar calculator-forced-input nil
  "Used to make alias events, e.g., make Return equivalent to `='.")

(defvar calculator-last-opXY nil
  "The last binary operation and its arguments.
Used for repeating operations in calculator-repR/L.")

(defvar calculator-registers ; use user-bindings first
  (append calculator-user-registers (list (cons ?e e) (cons ?p pi)))
  "The association list of calculator register values.")

(defvar calculator-saved-global-map nil
  "Saved global key map.")

(defvar calculator-mode-map nil
  "The calculator key map.")

(or calculator-mode-map
  (let ((map (make-sparse-keymap "Calculator")))
    (suppress-keymap map t)
    (define-key map "i" nil)
    (define-key map "o" nil)
    (let ((p '(calculator-open-paren  "(" "[" "{"
               calculator-close-paren ")" "]" "}"
               calculator-op-or-exp   "+" "-" [kp-add] [kp-subtract]
               calculator-digit       "0" "1" "2" "3" "4" "5" "6" "7"
                                      "8" "9" "a" "b" "c" "d" "f"
                                      [kp-0] [kp-1] [kp-2] [kp-3] [kp-4]
                                      [kp-5] [kp-6] [kp-7] [kp-8] [kp-9]
               calculator-op          [kp-divide] [kp-multiply]
               calculator-decimal     "." [kp-decimal]
               calculator-exp         "e"
               calculator-dec/deg-mode "D"
               calculator-set-register "s"
               calculator-get-register "g"
               calculator-radix-mode        "H" "X" "O" "B"
               calculator-radix-input-mode  "id" "ih" "ix" "io" "ib"
                                            "iD" "iH" "iX" "iO" "iB"
               calculator-radix-output-mode "od" "oh" "ox" "oo" "ob"
                                            "oD" "oH" "oX" "oO" "oB"
               calculator-saved-up    [?\C-p] [up]
               calculator-saved-down  [?\C-n] [down]
               calculator-quit        "q" [?\C-g]
               calculator-enter       [enter] [linefeed] [kp-enter]
                                      [?\r] [?\n]
               calculator-save-on-list " " [space]
               calculator-clear-saved [?\C-c] [(control delete)]
               calculator-save-and-quit [(control return)]
                                        [(control kp-enter)]
               calculator-paste       [insert] [(shift insert)]
               calculator-clear       [delete] [?\C-?] [?\C-d]
               calculator-help        [?h] [??] [f1] [help]
               calculator-copy        [(control insert)]
               calculator-backspace   [backspace]
               ))
          (f nil))
      (while p
        (cond
          ((symbolp (car p)) (setq f (car p)))
          (p (define-key map (car p) f)))
        (setq p (cdr p))))
    (if calculator-bind-escape
      (progn (define-key map [?\e] 'calculator-quit)
             (define-key map [escape] 'calculator-quit))
      (define-key map [?\e ?\e ?\e] 'calculator-quit))
    ;; make C-h work in text-mode
    (or window-system (define-key map [?\C-h] 'calculator-backspace))
    (setq calculator-mode-map map)))

(defun calculator-mode ()
  "A simple pocket calculator in Emacs.

This calculator is used in the same way as other popular calculators
like xcalc or calc.exe - but using an Emacs interface.

Expressions are entered using normal infix notation, parens are used as
normal.  Unary functions are usually postfix, but some depends on the
value of `calculator-unary-style' (if the style for an operator below is
specified, then it is fixed, otherwise it depends on this variable).
`+' and `-' can be used as either binary operators or prefix unary
operators.  Numbers can be entered with exponential notation using `e',
except when using a non-decimal radix mode for input (in this case `e'
will be the hexadecimal digit).

Here are the editing keys:
* `RET' `='      evaluate the current expression
* `C-insert'     copy the whole current expression to the `kill-ring'
* `C-enter'      evaluate, save result the `kill-ring' and exit
* `insert'       paste a number if the one was copied (normally)
* `delete' `C-d' clear last argument or whole expression (hit twice)
* `backspace'    delete a digit or a previous expression element
* `h' `?'        pop-up a quick reference help
* `ESC' `q'      exit (`ESC' can be used if `calculator-bind-escape' is
                 non-nil, otherwise use three consecutive `ESC's)

These operators are pre-defined:
* `+' `-' `*' `/' the common binary operators
* `\\' `%'         integer division and reminder
* `_' `;'         postfix unary negation and reciprocal
* `^' `L'         binary operators for x^y and log(x) in base y
* `Q' `!'         unary square root and factorial
* `S' `C' `T'     unary trigonometric operators - sin, cos and tan
* `|' `#' `&' `~' bitwise operators - or, xor, and, not

The trigonometric functions can be inverted if prefixed with an `I', see
below for the way to use degrees instead of the default radians.

Two special postfix unary operators are `>' and `<': whenever a binary
operator is performed, it is remembered along with its arguments; then
`>' (`<') will apply the same operator with the same right (left)
argument.

hex/oct/bin modes can be set for input and for display separately.
Another toggle-able mode is for using degrees instead of radians for
trigonometric functions.
The keys to switch modes are (`X' is shortcut for `H'):
* `D'             switch to all-decimal mode, or toggle degrees/radians
* `B' `O' `H' `X' binary/octal/hexadecimal modes for input & display
* `i' `o'         followed by one of `D' `B' `O' `H' `X' (case
                  insensitive) sets only the input or display radix mode
The prompt indicates the current modes:
* \"D=\": degrees mode;
* \"?=\": (? is B/O/H) this is the radix for both input and output;
* \"=?\": (? is B/O/H) the display radix (when input is decimal);
* \"??\": (? is D/B/O/H) 1st char for input radix, 2nd for display.

Values can be saved for future reference in either a list of saved
values, or in registers.

The list of saved values is useful for statistics operations on some
collected data.  It is possible to navigate in this list, and if the
value shown is the current one on the list, an indication is displayed
as \"[N]\" if this is the last number and there are N numbers, or
\"[M/N]\" if the M-th value is shown.
* `SPC'            evaluate the current value as usual, but also adds
                   the result to the list of saved values
* `l' `v'          computes total / average of saved values
* `up' `C-p'       browse to the previous value in the list
* `down' `C-n'     browse to the next value in the list
* `delete' `C-d'   remove current value from the list (if it is on it)
* `C-delete' `C-c' delete the whole list

Registers are variable-like place-holders for values:
* `s' followed by a character attach the current value to that character
* `g' followed by a character fetches the attached value

There are many variables that can be used to customize the calculator.
Some interesting customization variables are:
* `calculator-electric-mode'  use only the echo-area electrically.
* `calculator-unary-style'    set most unary ops to pre/postfix style.
* `calculator-user-registers' to define user-preset registers.
* `calculator-user-operators' to add user-defined operators.
See the documentation for these variables, and \"calculator.el\" for
more information.

\\{calculator-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'calculator-mode)
  (setq mode-name "Calculator")
  (use-local-map calculator-mode-map)
  (run-hooks 'calculator-mode-hook))

;;;###autoload
(defun calculator ()
  "Run the pocket calculator.
See the documentation for `calculator-mode' for more information."
  (interactive)
  (if calculator-electric-mode
    (progn (require 'electric)
           (message nil))) ; hide load message
  (setq calculator-buffer
        (or (and (bufferp calculator-buffer)
                 (buffer-live-p calculator-buffer)
                 calculator-buffer)
            (if calculator-electric-mode
              (get-buffer-create "*calculator*")
              (let ((split-window-keep-point nil)
                    (window-min-height 2))
                (select-window
                 (split-window-vertically (- (window-height) 2)))
                (switch-to-buffer
                 (get-buffer-create "*calculator*"))))))
  (set-buffer calculator-buffer)
  (calculator-mode)
  (setq buffer-read-only t)
  (if calculator-initial-operators
    (progn (calculator-add-operators calculator-initial-operators)
           (setq calculator-initial-operators nil)
           ;; don't change this since it is a customization variable,
           ;; its set function will add any new operators.
           (calculator-add-operators calculator-user-operators)))
  (calculator-reset)
  (calculator-update-display)
  (if calculator-electric-mode
    (save-window-excursion
      (let (old-g-map old-l-map (echo-keystrokes 0)
            (garbage-collection-messages nil)) ; no gc msg when electric
        (kill-buffer calculator-buffer)
        ;; strange behavior in FSF: doesn't always select correct
        ;; minibuffer.  I have no idea how to fix this
        (setq calculator-buffer (window-buffer (minibuffer-window)))
        (select-window (minibuffer-window))
        (calculator-reset)
        (calculator-update-display)
        (setq old-l-map (current-local-map))
        (setq old-g-map (current-global-map))
        (setq calculator-saved-global-map (current-global-map))
        (use-local-map calculator-mode-map)
        (use-global-map calculator-mode-map)
        (unwind-protect
            (catch 'calculator-done
              (Electric-command-loop
               'calculator-done
               ;; can't use 'noprompt, bug in electric.el
               '(lambda () 'noprompt)
               nil
               (lambda (x y)
                 (calculator-update-display))))
          (and calculator-buffer
               (catch 'calculator-done (calculator-quit)))
          (use-local-map old-l-map)
          (use-global-map old-g-map))))
    (message "Hit `?' For a quick help screen.")))

(defun calculator-op-arity (op)
  "Return OP's arity, 2, +1 or -1."
  (let ((arity (or (nth 3 op) 'x)))
    (if (numberp arity)
      arity
      (if (eq calculator-unary-style 'postfix) +1 -1))))

(defun calculator-op-prec (op)
  "Return OP's precedence for reducing when inserting into the stack.
Defaults to 1."
  (or (nth 4 op) 1))

(defun calculator-add-operators (more-ops)
  "This function handles operator addition.
Adds MORE-OPS to `calculator-operator', called initially to handle
`calculator-initial-operators' and `calculator-user-operators'."
  (let ((added-ops nil))
    (while more-ops
      (or (eq (car (car more-ops)) 'nobind)
          (let ((i -1) (key (car (car more-ops))))
            ;; make sure the key is undefined, so it's easy to define
            ;; prefix keys
            (while (< (setq i (1+ i)) (length key))
              (or (keymapp
                   (lookup-key calculator-mode-map
                               (substring key 0 (1+ i))))
                  (progn
                    (define-key
                      calculator-mode-map (substring key 0 (1+ i)) nil)
                    (setq i (length key)))))
            (define-key calculator-mode-map key 'calculator-op)))
      (setq added-ops (cons (if (eq (car (car more-ops)) 'nobind)
                              (cdr (car more-ops))
                              (car more-ops))
                            added-ops))
      (setq more-ops (cdr more-ops)))
    ;; added-ops come first, but in correct order
    (setq calculator-operators
          (append (nreverse added-ops) calculator-operators))))

(defun calculator-reset ()
  "Reset calculator variables."
  (setq calculator-stack           nil
        calculator-curnum          nil
        calculator-stack-display   nil
        calculator-display-fragile nil)
  (calculator-update-display))

(defun calculator-get-prompt ()
  "Return a string to display.
The string is set not to exceed the screen width."
  (let* ((calculator-prompt
          (format calculator-prompt
                  (cond
                    ((or calculator-output-radix calculator-input-radix)
                     (if (eq calculator-output-radix
                             calculator-input-radix)
                       (concat
                        (char-to-string
                         (car (rassq calculator-output-radix
                                     calculator-char-radix)))
                        "=")
                       (concat
                        (if calculator-input-radix
                          (char-to-string
                           (car (rassq calculator-input-radix
                                       calculator-char-radix)))
                          "=")
                        (char-to-string
                         (car (rassq calculator-output-radix
                                     calculator-char-radix))))))
                    (calculator-deg "D=")
                    (t "=="))))
         (prompt
          (concat calculator-prompt
                  (cdr calculator-stack-display)
                  (cond (calculator-curnum
                         ;; number being typed
                         (concat calculator-curnum "_"))
                        ((and (= 1 (length calculator-stack))
                              calculator-display-fragile)
                         ;; only the result is shown, next number will
                         ;; restart
                         nil)
                        (t
                         ;; waiting for a number or an operator
                         "?"))))
         (trim (- (length prompt) (1- (window-width)))))
    (if (<= trim 0)
      prompt
      (concat calculator-prompt
              (substring prompt (+ trim (length calculator-prompt)))))))

(defun calculator-curnum-value ()
  "Get the numeric value of the displayed number string as a float."
  (if calculator-input-radix
    (let ((radix
           (cdr (assq calculator-input-radix
                      '((bin . 2) (oct . 8) (hex . 16)))))
          (i -1) (value 0))
      ;; assume valid input (upcased & characters in range)
      (while (< (setq i (1+ i)) (length calculator-curnum))
        (setq value
              (+ (let ((ch (aref calculator-curnum i)))
                   (- ch (if (<= ch ?9) ?0 (- ?A 10))))
                 (* radix value))))
      value)
    (car
     (read-from-string
      (cond
        ((equal "." calculator-curnum)
         "0.0")
        ((string-match "[eE][+-]?$" calculator-curnum)
         (concat calculator-curnum "0"))
        ((string-match "\\.[0-9]\\|[eE]" calculator-curnum)
         calculator-curnum)
        ((string-match "\\." calculator-curnum)
         ;; do this because Emacs reads "23." as an integer.
         (concat calculator-curnum "0"))
        ((stringp calculator-curnum)
         (concat calculator-curnum ".0"))
        (t "0.0"))))))

(defun calculator-num-to-string (num)
  "Convert NUM to a displayable string."
  (cond
    ((and (numberp num) calculator-output-radix)
     ;; print with radix - for binary I convert the octal number
     (let ((str (format (if (eq calculator-output-radix 'hex) "%x" "%o")
                        (calculator-truncate
                         (if calculator-2s-complement num (abs num))))))
       (if (eq calculator-output-radix 'bin)
         (let ((i -1) (s ""))
           (while (< (setq i (1+ i)) (length str))
             (setq s
                   (concat s
                           (cdr (assq (aref str i)
                                      '((?0 . "000") (?1 . "001")
                                        (?2 . "010") (?3 . "011")
                                        (?4 . "100") (?5 . "101")
                                        (?6 . "110") (?7 . "111")))))))
           (string-match "^0*\\(.+\\)" s)
           (setq str (match-string 1 s))))
       (upcase
        (if (and (not calculator-2s-complement) (< num 0))
          (concat "-" str)
          str))))
    ((and (numberp num)
          ;; is this a normal-range number?
          (>= (abs num) calculator-number-exp-llimit)
          (< (abs num) calculator-number-exp-ulimit))
     (let ((str (format calculator-number-format num)))
       (cond
         ((and calculator-show-integers (string-match "\\.?0+$" str))
          ;; remove all redundant zeros
          (substring str 0 (match-beginning 0)))
         ((and (not calculator-show-integers)
               (string-match "\\..\\(.*[^0]\\)?\\(0+\\)$" str))
          ;; remove zeros, except for first after the "."
          (substring str 0 (match-beginning 2)))
         (t str))))
    ((numberp num) (format calculator-number-exp-format num))
    (t (prin1-to-string (nth 1 num) t))))

(defun calculator-update-display (&optional force)
  "Update the display.
If optional argument FORCE is non-nil, don't use the cached string."
  (set-buffer calculator-buffer)
  ;; update calculator-stack-display
  (if (or force
          (not (eq (car calculator-stack-display) calculator-stack)))
    (setq calculator-stack-display
          (cons calculator-stack
                (if calculator-stack
                  (concat
                   (mapconcat 'calculator-num-to-string
                              (reverse calculator-stack)
                              " ")
                   " "
                   (and calculator-display-fragile
                        calculator-saved-list
                        (= (car calculator-stack)
                           (nth calculator-saved-ptr
                                calculator-saved-list))
                        (if (= 0 calculator-saved-ptr)
                          (format "[%s]" (length calculator-saved-list))
                          (format "[%s/%s]"
                                  (- (length calculator-saved-list)
                                     calculator-saved-ptr)
                                  (length calculator-saved-list)))))
                  ""))))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (calculator-get-prompt)))
  (set-buffer-modified-p nil)
  (if calculator-display-fragile
    (goto-char (1+ (length calculator-prompt)))
    (goto-char (1- (point)))))

(defun calculator-reduce-stack (prec)
  "Reduce the stack using top operator.
PREC is a precedence - reduce everything with higher precedence."
  (while
      (cond
        ((and (cdr (cdr calculator-stack))         ; have three values
              (consp   (nth 0 calculator-stack))   ; two operators & num
              (numberp (nth 1 calculator-stack))
              (consp   (nth 2 calculator-stack))
              (eq '\) (nth 1 (nth 0 calculator-stack)))
              (eq '\( (nth 1 (nth 2 calculator-stack))))
         ;; reduce "... ( x )" --> "... x"
         (setq calculator-stack
               (cons (nth 1 calculator-stack)
                     (nthcdr 3 calculator-stack)))
         ;; another iteration
         t)
        ((and (cdr (cdr calculator-stack))         ; have three values
              (numberp (nth 0 calculator-stack))   ; two nums & operator
              (consp   (nth 1 calculator-stack))
              (numberp (nth 2 calculator-stack))
              (= 2 (calculator-op-arity            ; binary operator
                    (nth 1 calculator-stack)))
              (<= prec                             ; with higher prec.
                  (calculator-op-prec (nth 1 calculator-stack))))
         ;; reduce "... x op y" --> "... r", r is the result
         (setq calculator-stack
               (cons (calculator-funcall
                      (nth 2 (nth 1 calculator-stack))
                      (nth 2 calculator-stack)
                      (nth 0 calculator-stack))
                     (nthcdr 3 calculator-stack)))
         ;; another iteration
         t)
        ((and (>= (length calculator-stack) 2)     ; have two values
              (numberp (nth 0 calculator-stack))   ; number & operator
              (consp   (nth 1 calculator-stack))
              (= -1 (calculator-op-arity           ; prefix-unary op
                     (nth 1 calculator-stack)))
              (<= prec                             ; with higher prec.
                  (calculator-op-prec (nth 1 calculator-stack))))
         ;; reduce "... op x" --> "... r" for prefix op
         (setq calculator-stack
               (cons (calculator-funcall
                      (nth 2 (nth 1 calculator-stack))
                      (nth 0 calculator-stack))
                     (nthcdr 2 calculator-stack)))
         ;; another iteration
         t)
        ((and (cdr calculator-stack)               ; have two values
              (consp   (nth 0 calculator-stack))   ; operator & number
              (numberp (nth 1 calculator-stack))
              (= +1 (calculator-op-arity           ; postfix-unary op
                     (nth 0 calculator-stack)))
              (<= prec                             ; with higher prec.
                  (calculator-op-prec (nth 0 calculator-stack))))
         ;; reduce "... x op" --> "... r" for postfix op
         (setq calculator-stack
               (cons (calculator-funcall
                      (nth 2 (nth 0 calculator-stack))
                      (nth 1 calculator-stack))
                     (nthcdr 2 calculator-stack)))
         ;; another iteration
         t)
        ((and calculator-stack                     ; have one value
              (consp (nth 0 calculator-stack))     ; an operator
              (= 0 (calculator-op-arity            ; 0-ary op
                    (nth 0 calculator-stack))))
         ;; reduce "... op" --> "... r" for 0-ary op
         (setq calculator-stack
               (cons (calculator-funcall
                      (nth 2 (nth 0 calculator-stack)))
                     (nthcdr 1 calculator-stack)))
         ;; another iteration
         t)
        ((and (cdr calculator-stack)               ; have two values
              (numberp (nth 0 calculator-stack))   ; both numbers
              (numberp (nth 1 calculator-stack)))
         ;; get rid of redundant numbers:
         ;;   reduce "... y x" --> "... x"
         ;; needed for 0-ary ops that puts more values
         (setcdr calculator-stack (cdr (cdr calculator-stack))))
        (t ;; no more iterations
           nil))))

(eval-when-compile ; silence the compiler
  (or (fboundp 'event-key)
      (defun event-key (&rest _) nil))
  (or (fboundp 'key-press-event-p)
      (defun key-press-event-p (&rest _) nil)))

(defun calculator-last-input ()
  "Last char (or event or event sequence) that was read."
  (let ((inp (or calculator-forced-input (this-command-keys))))
    (if (or (stringp inp) (not (arrayp inp)))
      inp
      ;; this translates kp-x to x and [tries to] create a string to
      ;; lookup operators
      (let* ((i -1) (converted-str (make-string (length inp) ? )) k)
        ;; converts an array to a string the ops lookup with keypad
        ;; input
        (while (< (setq i (1+ i)) (length inp))
          (setq k (aref inp i))
          ;; if Emacs will someday have a event-key, then this would
          ;; probably be modified anyway
          (and (fboundp 'event-key) (key-press-event-p k)
               (setq k (event-key k)))
          ;; assume all symbols are translatable with an ascii-character
          (and (symbolp k)
               (setq k (or (get k 'ascii-character) ? )))
          (aset converted-str i k))
        converted-str))))

(defun calculator-clear-fragile (&optional op)
  "Clear the fragile flag if it was set, then maybe reset all.
OP is the operator (if any) that caused this call."
  (if (and calculator-display-fragile
           (or (not op)
               (= -1 (calculator-op-arity op))
               (= 0 (calculator-op-arity op))))
    ;; reset if last calc finished, and now get a num or prefix or 0-ary
    ;; op.
    (calculator-reset))
  (setq calculator-display-fragile nil))

(defun calculator-digit ()
  "Enter a single digit."
  (interactive)
  (let ((inp (aref (calculator-last-input) 0)))
    (if (and (or calculator-display-fragile
                 (not (numberp (car calculator-stack))))
             (cond
               ((not calculator-input-radix)     (<= inp ?9))
               ((eq calculator-input-radix 'bin) (<= inp ?1))
               ((eq calculator-input-radix 'oct) (<= inp ?7))
               (t t)))
      ;; enter digit if starting a new computation or have an op on the
      ;; stack.
      (progn
        (calculator-clear-fragile)
        (let ((digit (upcase (char-to-string inp))))
          (if (equal calculator-curnum "0")
            (setq calculator-curnum nil))
          (setq calculator-curnum
                (concat (or calculator-curnum "") digit)))
        (calculator-update-display)))))

(defun calculator-decimal ()
  "Enter a decimal period."
  (interactive)
  (if (and (not calculator-input-radix)
           (or calculator-display-fragile
               (not (numberp (car calculator-stack))))
           (not (and calculator-curnum
                     (string-match "[.eE]" calculator-curnum))))
    ;; enter the period on the same condition as a digit, only if no
    ;; period or exponent entered yet.
    (progn
      (calculator-clear-fragile)
      (setq calculator-curnum (concat (or calculator-curnum "0") "."))
      (calculator-update-display))))

(defun calculator-exp ()
  "Enter an `E' exponent character, or a digit in hex input mode."
  (interactive)
  (if calculator-input-radix
    (calculator-digit)
    (if (and (or calculator-display-fragile
                 (not (numberp (car calculator-stack))))
             (not (and calculator-curnum
                       (string-match "[eE]" calculator-curnum))))
      ;; same condition as above, also no E so far.
      (progn
        (calculator-clear-fragile)
        (setq calculator-curnum (concat (or calculator-curnum "1") "e"))
        (calculator-update-display)))))

(defun calculator-op ()
  "Enter an operator on the stack, doing all necessary reductions."
  (interactive)
  (let* ((last-inp (calculator-last-input))
         (op (assoc last-inp calculator-operators)))
    (calculator-clear-fragile op)
    (if (and calculator-curnum (/= (calculator-op-arity op) 0))
      (setq calculator-stack
            (cons (calculator-curnum-value) calculator-stack)))
    (setq calculator-curnum nil)
    (if (and (= 2 (calculator-op-arity op))
             (not (and calculator-stack
                       (numberp (nth 0 calculator-stack)))))
      ;; we have a binary operator but no number - search for a prefix
      ;; version
      (let ((rest-ops calculator-operators))
        (while (not (equal last-inp (car (car rest-ops))))
          (setq rest-ops (cdr rest-ops)))
        (setq op (assoc last-inp (cdr rest-ops)))
        (if (not (and op (= -1 (calculator-op-arity op))))
          (error "Binary operator without a first operand"))))
    (calculator-reduce-stack
     (cond ((eq (nth 1 op) '\() 10)
           ((eq (nth 1 op) '\)) 0)
           (t (calculator-op-prec op))))
    (if (or (and (= -1 (calculator-op-arity op))
                 (numberp (car calculator-stack)))
            (and (/= (calculator-op-arity op) -1)
                 (/= (calculator-op-arity op) 0)
                 (not (numberp (car calculator-stack)))))
      (error "Unterminated expression"))
    (setq calculator-stack (cons op calculator-stack))
    (calculator-reduce-stack (calculator-op-prec op))
    (and (= (length calculator-stack) 1)
         (numberp (nth 0 calculator-stack))
         ;; the display is fragile if it contains only one number
         (setq calculator-display-fragile t)
         ;; add number to the saved-list
         calculator-add-saved
         (if (= 0 calculator-saved-ptr)
           (setq calculator-saved-list
                 (cons (car calculator-stack) calculator-saved-list))
           (let ((p (nthcdr (1- calculator-saved-ptr)
                            calculator-saved-list)))
             (setcdr p (cons (car calculator-stack) (cdr p))))))
    (calculator-update-display)))

(defun calculator-op-or-exp ()
  "Either enter an operator or a digit.
Used with +/- for entering them as digits in numbers like 1e-3."
  (interactive)
  (if (and (not calculator-display-fragile)
           calculator-curnum
           (string-match "[eE]$" calculator-curnum))
    (calculator-digit)
    (calculator-op)))

(defun calculator-dec/deg-mode ()
  "Set decimal mode for display & input, if decimal, toggle deg mode."
  (interactive)
  (if calculator-curnum
    (setq calculator-stack
          (cons (calculator-curnum-value) calculator-stack)))
  (setq calculator-curnum nil)
  (if (or calculator-input-radix calculator-output-radix)
    (progn (setq calculator-input-radix nil)
           (setq calculator-output-radix nil))
    ;; already decimal - toggle degrees mode
    (setq calculator-deg (not calculator-deg)))
  (calculator-update-display t))

(defun calculator-radix-mode ()
  "Set input and display radix modes."
  (interactive)
  (calculator-radix-input-mode)
  (calculator-radix-output-mode))

(defun calculator-radix-input-mode ()
  "Set input radix modes."
  (interactive)
  (if calculator-curnum
    (setq calculator-stack
          (cons (calculator-curnum-value) calculator-stack)))
  (setq calculator-curnum nil)
  (setq calculator-input-radix
        (let ((inp (calculator-last-input)))
          (cdr (assq (upcase (aref inp (1- (length inp))))
                     calculator-char-radix))))
  (calculator-update-display))

(defun calculator-radix-output-mode ()
  "Set display radix modes."
  (interactive)
  (if calculator-curnum
    (setq calculator-stack
          (cons (calculator-curnum-value) calculator-stack)))
  (setq calculator-curnum nil)
  (setq calculator-output-radix
        (let ((inp (calculator-last-input)))
          (cdr (assq (upcase (aref inp (1- (length inp))))
                     calculator-char-radix))))
  (calculator-update-display t))

(defun calculator-save-on-list ()
  "Evaluate current expression, put result on the saved values list."
  (interactive)
  (let ((calculator-add-saved t)) ; marks the result to be added
    (calculator-enter)))

(defun calculator-clear-saved ()
  "Clear the list of saved values in `calculator-saved-list'."
  (interactive)
  (setq calculator-saved-list nil)
  (calculator-update-display t))

(defun calculator-saved-move (n)
  "Go N elements up the list of saved values."
  (interactive)
  (and calculator-saved-list
       (or (null calculator-stack) calculator-display-fragile)
       (progn
         (setq calculator-saved-ptr
               (max (min (+ n calculator-saved-ptr)
                         (length calculator-saved-list))
                    0))
         (if (nth calculator-saved-ptr calculator-saved-list)
           (setq calculator-stack
                 (list (nth calculator-saved-ptr calculator-saved-list))
                 calculator-display-fragile t)
           (calculator-reset)))))

(defun calculator-saved-up ()
  "Go up the list of saved values."
  (interactive)
  (calculator-saved-move +1))

(defun calculator-saved-down ()
  "Go down the list of saved values."
  (interactive)
  (calculator-saved-move -1))

(defun calculator-open-paren ()
  "Equivalents of `(' use this."
  (interactive)
  (let ((calculator-forced-input "("))
    (calculator-op)))

(defun calculator-close-paren ()
  "Equivalents of `)' use this."
  (interactive)
  (let ((calculator-forced-input ")"))
    (calculator-op)))

(defun calculator-enter ()
  "Make Enter equivalent to `='."
  (interactive)
  (let ((calculator-forced-input "="))
    (calculator-op)))

(defun calculator-backspace ()
  "Backward delete a single digit or a stack element."
  (interactive)
  (if calculator-curnum
    (setq calculator-curnum
          (if (> (length calculator-curnum) 1)
            (substring calculator-curnum
                       0 (1- (length calculator-curnum)))
            nil))
    (setq calculator-stack (cdr calculator-stack)))
  (calculator-update-display))

(defun calculator-clear ()
  "Clear current number."
  (interactive)
  (setq calculator-curnum nil)
  (cond
    ;; if the current number is from the saved-list - remove it
    ((and calculator-display-fragile
          calculator-saved-list
          (= (car calculator-stack)
             (nth calculator-saved-ptr calculator-saved-list)))
     (if (= 0 calculator-saved-ptr)
       (setq calculator-saved-list (cdr calculator-saved-list))
       (let ((p (nthcdr (1- calculator-saved-ptr)
                        calculator-saved-list)))
         (setcdr p (cdr (cdr p)))
         (setq calculator-saved-ptr (1- calculator-saved-ptr))))
     (if calculator-saved-list
       (setq calculator-stack
             (list (nth calculator-saved-ptr calculator-saved-list)))
       (calculator-reset)))
    ;; reset if fragile or double clear
    ((or calculator-display-fragile (eq last-command this-command))
     (calculator-reset)))
  (calculator-update-display))

(defun calculator-copy ()
  "Copy current number to the `kill-ring'."
  (interactive)
  (calculator-enter)
  ;; remove trailing .0 and spaces .0
  (let ((s (cdr calculator-stack-display)))
    (if (string-match "^\\(.*[^ ]\\) *$" s)
      (setq s (match-string 1 s)))
    (kill-new s)))

(defun calculator-set-register (reg)
  "Set a register value for REG."
  (interactive "cRegister to store into: ")
  (let* ((as  (assq reg calculator-registers))
         (val (progn (calculator-enter) (car calculator-stack))))
    (if as
      (setcdr as val)
      (setq calculator-registers
            (cons (cons reg val) calculator-registers)))
    (message (format "[%c] := %S" reg val))))

(defun calculator-put-value (val)
  "Paste VAL as if entered.
Used by `calculator-paste' and `get-register'."
  (if (and (numberp val)
           ;; (not calculator-curnum)
           (or calculator-display-fragile
               (not (numberp (car calculator-stack)))))
    (progn
      (calculator-clear-fragile)
      (setq calculator-curnum (calculator-num-to-string val))
      (calculator-update-display))))

(defun calculator-paste ()
  "Paste a value from the `kill-ring'."
  (interactive)
  (calculator-put-value
   (condition-case nil (car (read-from-string (current-kill 0)))
     (error nil))))

(defun calculator-get-register (reg)
  "Get a value from a register REG."
  (interactive "cRegister to get value from: ")
  (calculator-put-value (cdr (assq reg calculator-registers))))

(defun calculator-help ()
  ;; this is used as the quick reference screen you get with `h'
  "Quick reference:
* numbers/operators/parens/./e - enter expressions
  + - * / \\(div) %(rem) _(-X,postfix) ;(1/X,postfix) ^(exp) L(og)
  Q(sqrt) !(fact) S(in) C(os) T(an) |(or) #(xor) &(and) ~(not)
* >/< repeats last binary operation with its 2nd (1st) arg as postfix op
* I inverses next trig function
* D         - switch to all-decimal mode, or toggles deg/rad mode
* B/O/H/X   - binary/octal/hex mode for i/o (X is a shortcut for H)
* i/o       - prefix for d/b/o/x - set only input/output modes
* enter/=   - evaluate current expr.   * s/g      - set/get a register
* space     - evaluate & save on list  * l/v      - list total/average
* up/down/C-p/C-n - browse saved       * C-delete - clear all saved
* C-insert  - copy whole expr.         * C-enter  - evaluate, copy, exit
* insert    - paste a number           * backspace- delete backwards
* delete    - clear argument or list value or whole expression (twice)
* escape/q  - exit."
  (interactive)
  (if (eq last-command 'calculator-help)
    (let ((mode-name "Calculator")
          (major-mode 'calculator-mode)
          (g-map (current-global-map))
          (win (selected-window)))
      (require 'ehelp)
      (if calculator-electric-mode
        (use-global-map calculator-saved-global-map))
      (electric-describe-mode)
      (if calculator-electric-mode
        (use-global-map g-map))
      (select-window win) ; these are for XEmacs (also below)
      (message nil))
    (let ((one (one-window-p t))
          (win (selected-window))
          (help-buf (get-buffer-create "*Help*")))
      (save-window-excursion
        (with-output-to-temp-buffer "*Help*"
          (princ (documentation 'calculator-help)))
        (if one
          (shrink-window-if-larger-than-buffer
           (get-buffer-window help-buf)))
        (message
         "`%s' again for more help, any other key continues normally."
         (calculator-last-input))
        (select-window win)
        (sit-for 360))
      (select-window win))))

(defun calculator-quit ()
  "Quit calculator."
  (interactive)
  (set-buffer calculator-buffer)
  (let ((inhibit-read-only t)) (erase-buffer))
  (if (not calculator-electric-mode)
    (progn
      (condition-case nil
          (while (get-buffer-window calculator-buffer)
            (delete-window (get-buffer-window calculator-buffer)))
        (error nil))
      (kill-buffer calculator-buffer)))
  (setq calculator-buffer nil)
  (message "Calculator done.")
  (if calculator-electric-mode (throw 'calculator-done nil)))

(defun calculator-save-and-quit ()
  "Quit the calculator, saving the result on the `kill-ring'."
  (interactive)
  (calculator-enter)
  (calculator-copy)
  (calculator-quit))

(defun calculator-funcall (f &optional X Y)
  "If F is a symbol, evaluate (F X Y).
Otherwise, it should be a list, evaluate it with X, Y bound to the
arguments."
  ;; remember binary ops for calculator-repR/L
  (if Y (setq calculator-last-opXY (list f X Y)))
  (condition-case nil
      (let ((result
             (if (symbolp f)
               (cond ((and X Y) (funcall f X Y))
                     (X         (funcall f X))
                     (t         (funcall f)))
               ;; f is an expression
               (let* ((__f__ f) ; so we can get this value below...
                      (TX (calculator-truncate X))
                      (TY (and Y (calculator-truncate Y)))
                      (DX (if calculator-deg (/ (* X pi) 180) X))
                      (L  calculator-saved-list)
                      (Fbound (fboundp 'F))
                      (Fsave  (and Fbound (symbol-function 'F)))
                      (Dbound (fboundp 'D))
                      (Dsave  (and Dbound (symbol-function 'D))))
                 ;; a shortened version of flet
                 (fset 'F (function
                           (lambda (&optional x y)
                             (calculator-funcall __f__ x y))))
                 (fset 'D (function
                           (lambda (x)
                             (if calculator-deg (/ (* x 180) pi) x))))
                 (unwind-protect (eval f)
                   (if Fbound (fset 'F Fsave) (fmakunbound 'F))
                   (if Dbound (fset 'D Dsave) (fmakunbound 'D)))))))
        (if (< (abs result) calculator-epsilon)
          0
          result))
    (error 0)))

(defun calculator-repR (x)
  "Repeats the last binary operation with its second argument and X.
To use this, apply a binary operator (evaluate it), then call this."
  (if calculator-last-opXY
    ;; avoid rebinding calculator-last-opXY
    (let ((calculator-last-opXY calculator-last-opXY))
      (calculator-funcall
       (car calculator-last-opXY) x (nth 2 calculator-last-opXY)))
    x))

(defun calculator-repL (x)
  "Repeats the last binary operation with its first argument and X.
To use this, apply a binary operator (evaluate it), then call this."
  (if calculator-last-opXY
    ;; avoid rebinding calculator-last-opXY
    (let ((calculator-last-opXY calculator-last-opXY))
      (calculator-funcall
       (car calculator-last-opXY) (nth 1 calculator-last-opXY) x))
    x))

(defun calculator-fact (x)
  "Simple factorial of X."
  (let ((r (if (<= x 10) 1 1.0)))
    (while (> x 0)
      (setq r (* r (truncate x)))
      (setq x (1- x)))
    r))

(defun calculator-truncate (n)
  "Truncate N, return 0 in case of overflow."
  (condition-case nil (truncate n) (error 0)))


(provide 'calculator)

;;; calculator.el ends here
