;;; calculator.el --- a calculator for Emacs  -*- lexical-binding: t -*-

;; Copyright (C) 1998, 2000-2014 Free Software Foundation, Inc.

;; Author: Eli Barzilay <eli@barzilay.org>
;; Keywords: tools, convenience

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;;=====================================================================
;;; Commentary:
;;
;; A calculator for Emacs.
;; Why should you reach for your mouse to get xcalc (calc.exe, gcalc or
;; whatever), when you have Emacs running already?
;;
;; If this is not part of your Emacs distribution, then simply bind
;; `calculator' to a key and make it an autoloaded function, e.g.:
;;   (autoload 'calculator "calculator"
;;     "Run the Emacs calculator." t)
;;   (global-set-key [(control return)] 'calculator)
;;
;; Written by Eli Barzilay, eli@barzilay.org
;;

;;;=====================================================================
;;; Customization:

(defgroup calculator nil
  "Simple Emacs calculator."
  :prefix "calculator"
  :version "21.1"
  :group 'tools
  :group 'applications)

(defcustom calculator-electric-mode nil
  "Run `calculator' electrically, in the echo area.
Electric mode saves some place but changes the way you interact with the
calculator."
  :type  'boolean
  :group 'calculator)

(defcustom calculator-use-menu t
  "Make `calculator' create a menu.
Note that this requires easymenu.  Must be set before loading."
  :type  'boolean
  :group 'calculator)

(defcustom calculator-bind-escape nil
  "If non-nil, set escape to exit the calculator."
  :type  'boolean
  :group 'calculator)

(defcustom calculator-unary-style 'postfix
  "Value is either 'prefix or 'postfix.
This determines the default behavior of unary operators."
  :type    '(choice (const prefix) (const postfix))
  :group   'calculator)

(defcustom calculator-prompt "Calc=%s> "
  "The prompt used by the Emacs calculator.
It should contain a \"%s\" somewhere that will indicate the i/o radixes;
this will be a two-character string as described in the documentation
for `calculator-mode'."
  :type  'string
  :group 'calculator)

(defcustom calculator-number-digits 3
  "The calculator's number of digits used for standard display.
Used by the `calculator-standard-display' function - it will use the
format string \"%.NC\" where this number is N and C is a character given
at runtime."
  :type  'integer
  :group 'calculator)

(defcustom calculator-radix-grouping-mode t
  "Use digit grouping in radix output mode.
If this is set, chunks of `calculator-radix-grouping-digits' characters
will be separated by `calculator-radix-grouping-separator' when in radix
output mode is active (determined by `calculator-output-radix')."
  :type  'boolean
  :group 'calculator)

(defcustom calculator-radix-grouping-digits 4
  "The number of digits used for grouping display in radix modes.
See `calculator-radix-grouping-mode'."
  :type  'integer
  :group 'calculator)

(defcustom calculator-radix-grouping-separator "'"
  "The separator used in radix grouping display.
See `calculator-radix-grouping-mode'."
  :type  'string
  :group 'calculator)

(defcustom calculator-remove-zeros t
  "Non-nil value means delete all redundant zero decimal digits.
If this value is not t and not nil, redundant zeros are removed except
for one.
Used by the `calculator-remove-zeros' function."
  :type  '(choice (const t) (const leave-decimal) (const nil))
  :group 'calculator)

(defcustom calculator-displayer '(std ?n)
  "A displayer specification for numerical values.
This is the displayer used to show all numbers in an expression.  Result
values will be displayed according to the first element of
`calculator-displayers'.

The displayer is a symbol, a string or an expression.  A symbol should
be the name of a one-argument function, a string is used with a single
argument and an expression will be evaluated with the variable `num'
bound to whatever should be displayed.  If it is a function symbol, it
should be able to handle special symbol arguments, currently `left' and
`right' which will be sent by special keys to modify display parameters
associated with the displayer function (for example to change the number
of digits displayed).

An exception to the above is the case of the list (std C [G]) where C is
a character and G is an optional boolean, in this case the
`calculator-standard-displayer' function will be used with these as
arguments."
  :type '(choice (function) (string) (sexp)
                 (list (const std) character)
                 (list (const std) character boolean))
  :group 'calculator)

(defcustom calculator-displayers
  '(((std ?n) "Standard display, decimal point or scientific")
    (calculator-eng-display "Eng display")
    ((std ?f t) "Standard display, decimal point with grouping")
    ((std ?e) "Standard display, scientific")
    ("%S"     "Emacs printer"))
  "A list of displayers.
Each element is a list of a displayer and a description string.  The
first element is the one which is currently used, this is for the
display of result values not values in expressions.  A displayer
specification is the same as the values that can be stored in
`calculator-displayer'.

`calculator-rotate-displayer' rotates this list."
  :type  'sexp
  :group 'calculator)

(defcustom calculator-paste-decimals t
  "If non-nil, convert pasted integers so they have a decimal point.
This makes it possible to paste big integers since they will be read as
floats, otherwise the Emacs reader will fail on them."
  :type  'boolean
  :group 'calculator)

(defcustom calculator-copy-displayer nil
  "If non-nil, this is any value that can be used for
`calculator-displayer', to format a string before copying it with
`calculator-copy'.  If nil, then `calculator-displayer's normal value is
used."
  :type  'boolean
  :group 'calculator)

(defcustom calculator-2s-complement nil
  "If non-nil, show negative numbers in 2s complement in radix modes.
Otherwise show as a negative number."
  :type  'boolean
  :group 'calculator)

(defcustom calculator-mode-hook nil
  "List of hook functions for `calculator-mode' to run.
Note: if `calculator-electric-mode' is on, then this hook will get
activated in the minibuffer -- in that case it should not do much more
than local key settings and other effects that will change things
outside the scope of calculator related code."
  :type  'hook
  :group 'calculator)

(defcustom calculator-user-registers nil
  "An association list of user-defined register bindings.
Each element in this list is a list of a character and a number that
will be stored in that character's register.

For example, use this to define the golden ratio number:
  (setq calculator-user-registers '((?g .  1.61803398875)))
before you load calculator."
  :type  '(repeat (cons character number))
  :set   (lambda (_ val)
           (and (boundp 'calculator-registers)
                (setq calculator-registers
                      (append val calculator-registers)))
           (setq calculator-user-registers val))
  :group 'calculator)

(defcustom calculator-user-operators nil
  "A list of additional operators.
This is a list in the same format as specified in the documentation for
`calculator-operators', that you can use to bind additional calculator
operators.  It is probably not a good idea to modify this value with
`customize' since it is too complex...

Examples:

* A very simple one, adding a postfix \"x-to-y\" conversion keys, using
  t as a prefix key:

  (setq calculator-user-operators
        '((\"tf\" cl-to-fr (+ 32 (/ (* X 9) 5)) 1)
          (\"tc\" fr-to-cl (/ (* (- X 32) 5) 9) 1)
          (\"tp\" kg-to-lb (/ X 0.453592)       1)
          (\"tk\" lb-to-kg (* X 0.453592)       1)
          (\"tF\" mt-to-ft (/ X 0.3048)         1)
          (\"tM\" ft-to-mt (* X 0.3048)         1)))

* Using a function-like form is very simple: use `X' for the argument
  (`Y' for the second in case of a binary operator), `TX' is a truncated
  version of `X' and `F' for a recursive call.  Here is a [very
  inefficient] Fibonacci number calculation:

  (add-to-list 'calculator-user-operators
               '(\"F\" fib
                 (if (<= TX 1) 1 (+ (F (- TX 1)) (F (- TX 2))))))

  Note that this will be either postfix or prefix, according to
  `calculator-unary-style'."
  :type  '(repeat (list string symbol sexp integer integer))
  :group 'calculator)

;;;=====================================================================
;;; Code:

(eval-when-compile (require 'cl-lib))

;;;---------------------------------------------------------------------
;;; Variables

(defvar calculator-initial-operators
  '(;; "+"/"-" have keybindings of their own, not calculator-ops
    ("=" =     identity        1 -1)
    (nobind "+" +  +           2  4)
    (nobind "-" -  -           2  4)
    (nobind "+" +  +          -1  9)
    (nobind "-" -  -          -1  9)
    ("(" \(    identity       -1 -1)
    (")" \)    identity       +1 10)
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
    ("^"  ^    calculator-expt 2  7)
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
   and evaluates to the function value -- these variables are capital;

4. The function's arity, optional, one of: 2 => binary, -1 => prefix
   unary, +1 => postfix unary, 0 => a 0-arg operator func (note that
   using such a function replaces the currently entered number, if any),
   non-number (the default) => postfix or prefix as determined by
   `calculator-unary-style';

5. The function's precedence -- should be in the range of 1 (lowest) to
   9 (highest) (optional, defaults to 1);

It it possible have a unary prefix version of a binary operator if it
comes later in this list.  If the list begins with the symbol 'nobind,
then no key binding will take place -- this is only useful for predefined
keys.

Use `calculator-user-operators' to add operators to this list, see its
documentation for an example.")

(defvar calculator-stack nil
  "Stack contents -- operations and operands.")

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

(defvar calculator-eng-extra nil
  "Internal value used by `calculator-eng-display'.")

(defvar calculator-eng-tmp-show nil
  "Internal value used by `calculator-eng-display'.")

(defvar calculator-last-opXY nil
  "The last binary operation and its arguments.
Used for repeating operations in calculator-repR/L.")

(defvar calculator-registers ; use user-bindings first
  (append calculator-user-registers
          (list (cons ?e float-e) (cons ?p float-pi)))
  "The association list of calculator register values.")

(defvar calculator-saved-global-map nil
  "Saved global key map.")

(defvar calculator-restart-other-mode nil
  "Used to hack restarting with the electric mode changed.")

;;;---------------------------------------------------------------------
;;; Key bindings

(defvar calculator-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map "i" nil)
    (define-key map "o" nil)
    (let ((p
           '((calculator-open-paren  "[")
             (calculator-close-paren "]")
             (calculator-op-or-exp   "+" "-" [kp-add] [kp-subtract])
             (calculator-digit       "0" "1" "2" "3" "4" "5" "6" "7" "8"
                                     "9" "a" "b" "c" "d" "f"
                                     [kp-0] [kp-1] [kp-2] [kp-3] [kp-4]
                                     [kp-5] [kp-6] [kp-7] [kp-8] [kp-9])
             (calculator-op          [kp-divide] [kp-multiply])
             (calculator-decimal     "." [kp-decimal])
             (calculator-exp         "e")
             (calculator-dec/deg-mode "D")
             (calculator-set-register "s")
             (calculator-get-register "g")
             (calculator-radix-mode        "H" "X" "O" "B")
             (calculator-radix-input-mode  "id" "ih" "ix" "io" "ib"
                                           "iD" "iH" "iX" "iO" "iB")
             (calculator-radix-output-mode "od" "oh" "ox" "oo" "ob"
                                           "oD" "oH" "oX" "oO" "oB")
             (calculator-rotate-displayer      "'")
             (calculator-rotate-displayer-back "\"")
             (calculator-displayer-prev        "{")
             (calculator-displayer-next        "}")
             (calculator-saved-up      [up] [?\C-p])
             (calculator-saved-down    [down] [?\C-n])
             (calculator-quit          "q" [?\C-g])
             (calculator-enter         [enter] [linefeed] [kp-enter]
                                       [return] [?\r] [?\n])
             (calculator-save-on-list  " " [space])
             (calculator-clear-saved   [?\C-c] [(control delete)])
             (calculator-save-and-quit [(control return)]
                                       [(control kp-enter)])
             (calculator-paste         [insert] [(shift insert)]
                                       [paste] [mouse-2] [?\C-y])
             (calculator-clear         [delete] [?\C-?] [?\C-d])
             (calculator-help          [?h] [??] [f1] [help])
             (calculator-copy          [(control insert)] [copy])
             (calculator-backspace     [backspace])
             )))
      (while p
        ;; reverse the keys so earlier definitions come last -- makes
        ;; the more sensible bindings visible in the menu
        (let ((func (caar p)) (keys (reverse (cdar p))))
          (while keys
            (define-key map (car keys) func)
            (setq keys (cdr keys))))
        (setq p (cdr p))))
    (if calculator-bind-escape
      (progn (define-key map [?\e] 'calculator-quit)
             (define-key map [escape] 'calculator-quit))
      (define-key map [?\e ?\e ?\e] 'calculator-quit))
    ;; make C-h work in text-mode
    (or window-system (define-key map [?\C-h] 'calculator-backspace))
    ;; set up a menu
    (when (and calculator-use-menu (not (boundp 'calculator-menu)))
      (let ((radix-selectors
             (mapcar (lambda (x)
                       `([,(nth 0 x)
                          (calculator-radix-mode ,(nth 2 x))
                          :style radio
                          :keys ,(nth 2 x)
                          :selected
                          (and
                           (eq calculator-input-radix ',(nth 1 x))
                           (eq calculator-output-radix ',(nth 1 x)))]
                         [,(concat (nth 0 x) " Input")
                          (calculator-radix-input-mode ,(nth 2 x))
                          :keys ,(concat "i" (downcase (nth 2 x)))
                          :style radio
                          :selected
                          (eq calculator-input-radix ',(nth 1 x))]
                         [,(concat (nth 0 x) " Output")
                          (calculator-radix-output-mode ,(nth 2 x))
                          :keys ,(concat "o" (downcase (nth 2 x)))
                          :style radio
                          :selected
                          (eq calculator-output-radix ',(nth 1 x))]))
                     '(("Decimal"     nil "D")
                       ("Binary"      bin "B")
                       ("Octal"       oct "O")
                       ("Hexadecimal" hex "H"))))
            (op (lambda (name key)
                  `[,name (calculator-op ,key) :keys ,key])))
        (easy-menu-define
          calculator-menu map "Calculator menu."
          `("Calculator"
            ["Help"
             (let ((last-command 'calculator-help)) (calculator-help))
             :keys "?"]
            "---"
            ["Copy"  calculator-copy]
            ["Paste" calculator-paste]
            "---"
            ["Electric mode"
             (progn (calculator-quit)
                    (setq calculator-restart-other-mode t)
                    (run-with-timer 0.1 nil (lambda () (message nil)))
                    ;; the message from the menu will be visible,
                    ;; couldn't make it go away...
                    (calculator))
             :active (not calculator-electric-mode)]
            ["Normal mode"
             (progn (setq calculator-restart-other-mode t)
                    (calculator-quit))
             :active calculator-electric-mode]
            "---"
            ("Functions"
             ,(funcall op "Repeat-right" ">")
             ,(funcall op "Repeat-left"  "<")
             "------General------"
             ,(funcall op "Reciprocal"   ";")
             ,(funcall op "Log"          "L")
             ,(funcall op "Square-root"  "Q")
             ,(funcall op "Factorial"    "!")
             "------Trigonometric------"
             ,(funcall op "Sinus"        "S")
             ,(funcall op "Cosine"       "C")
             ,(funcall op "Tangent"      "T")
             ,(funcall op "Inv-Sinus"    "IS")
             ,(funcall op "Inv-Cosine"   "IC")
             ,(funcall op "Inv-Tangent"  "IT")
             "------Bitwise------"
             ,(funcall op "Or"           "|")
             ,(funcall op "Xor"          "#")
             ,(funcall op "And"          "&")
             ,(funcall op "Not"          "~"))
            ("Saved List"
             ["Eval+Save"      calculator-save-on-list]
             ["Prev number"    calculator-saved-up]
             ["Next number"    calculator-saved-down]
             ["Delete current" calculator-clear
              :active (and calculator-display-fragile
                           calculator-saved-list
                           (= (car calculator-stack)
                              (nth calculator-saved-ptr
                                   calculator-saved-list)))]
             ["Delete all" calculator-clear-saved]
             "---"
             ,(funcall op "List-total"   "l")
             ,(funcall op "List-average" "v"))
            ("Registers"
             ["Get register" calculator-get-register]
             ["Set register" calculator-set-register])
            ("Modes"
             ["Radians"
              (progn
                (and (or calculator-input-radix calculator-output-radix)
                     (calculator-radix-mode "D"))
                (and calculator-deg (calculator-dec/deg-mode)))
              :keys "D"
              :style radio
              :selected (not (or calculator-input-radix
                                 calculator-output-radix
                                 calculator-deg))]
             ["Degrees"
              (progn
                (and (or calculator-input-radix calculator-output-radix)
                     (calculator-radix-mode "D"))
                (or calculator-deg (calculator-dec/deg-mode)))
              :keys "D"
              :style radio
              :selected (and calculator-deg
                             (not (or calculator-input-radix
                                      calculator-output-radix)))]
             "---"
             ,@(mapcar 'car radix-selectors)
             ("Separate I/O"
              ,@(mapcar (lambda (x) (nth 1 x)) radix-selectors)
              "---"
              ,@(mapcar (lambda (x) (nth 2 x)) radix-selectors)))
            ("Decimal Display"
             ,@(mapcar (lambda (d)
                         (vector (cadr d)
                                 ;; Note: inserts actual object here
                                 `(calculator-rotate-displayer ',d)))
                       calculator-displayers)
             "---"
             ["Change Prev Display" calculator-displayer-prev]
             ["Change Next Display" calculator-displayer-next])
            "---"
            ["Copy+Quit" calculator-save-and-quit]
            ["Quit"      calculator-quit]))))
    map)
  "The calculator key map.")

;;;---------------------------------------------------------------------
;;; Startup and mode stuff

(define-derived-mode calculator-mode fundamental-mode "Calculator"
  ;; this help is also used as the major help screen
  "A [not so] simple calculator for Emacs.

This calculator is used in the same way as other popular calculators
like xcalc or calc.exe -- but using an Emacs interface.

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
* `C-return'     evaluate, save result the `kill-ring' and exit
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
* `S' `C' `T'     unary trigonometric operators: sin, cos and tan
* `|' `#' `&' `~' bitwise operators: or, xor, and, not

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

Also, the quote key can be used to switch display modes for decimal
numbers (double-quote rotates back), and the two brace characters
\(\"{\" and \"}\" change display parameters that these displayers use,
if they handle such).  If output is using any radix mode, then these
keys toggle digit grouping mode and the chunk size.

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

\\{calculator-mode-map}")

(declare-function Electric-command-loop "electric"
                  (return-tag &optional prompt inhibit-quitting
                              loop-function loop-state))

;;;###autoload
(defun calculator ()
  "Run the Emacs calculator.
See the documentation for `calculator-mode' for more information."
  (interactive)
  (when calculator-restart-other-mode
    (setq calculator-electric-mode (not calculator-electric-mode)))
  (when calculator-initial-operators
    (calculator-add-operators calculator-initial-operators)
    (setq calculator-initial-operators nil)
    ;; don't change this since it is a customization variable,
    ;; its set function will add any new operators
    (calculator-add-operators calculator-user-operators))
  (setq calculator-buffer (get-buffer-create "*calculator*"))
  (if calculator-electric-mode
    (save-window-excursion
      (require 'electric) (message nil) ; hide load message
      (let (old-g-map old-l-map
            (old-buf (window-buffer (minibuffer-window)))
            (echo-keystrokes 0)
            (garbage-collection-messages nil)) ; no gc msg when electric
        (set-window-buffer (minibuffer-window) calculator-buffer)
        (select-window (minibuffer-window))
        (calculator-reset)
        (calculator-update-display)
        (setq old-l-map (current-local-map))
        (setq old-g-map (current-global-map))
        (setq calculator-saved-global-map (current-global-map))
        (use-local-map nil)
        (use-global-map calculator-mode-map)
        (run-hooks 'calculator-mode-hook)
        (unwind-protect
            (catch 'calculator-done
              (Electric-command-loop
               'calculator-done
               ;; can't use 'noprompt, bug in electric.el
               (lambda () 'noprompt)
               nil
               (lambda (_x _y) (calculator-update-display))))
          (set-window-buffer (minibuffer-window) old-buf)
          (kill-buffer calculator-buffer)
          (use-local-map old-l-map)
          (use-global-map old-g-map))))
    (progn
      (cond
        ((not (get-buffer-window calculator-buffer))
         (let ((window-min-height 2))
           ;; maybe leave two lines for our window because of the
           ;; normal `raised' mode line
           (select-window (split-window-below
                           (if (calculator-need-3-lines) -3 -2)))
           (switch-to-buffer calculator-buffer)))
        ((not (eq (current-buffer) calculator-buffer))
         (select-window (get-buffer-window calculator-buffer))))
      (calculator-mode)
      (setq buffer-read-only t)
      (calculator-reset)
      (message "Hit `?' For a quick help screen.")))
  (when (and calculator-restart-other-mode calculator-electric-mode)
    (calculator)))

(defun calculator-need-3-lines ()
  ;; If the mode line might interfere with the calculator buffer, use 3
  ;; lines instead.
  (let* ((dh (face-attribute 'default :height))
         (mh (face-attribute 'mode-line :height)))
    ;; if the mode line is shorter than the default, stick with 2 lines
    ;; (it may be necessary to check how much shorter)
    (and (not (or (and (integerp dh) (integerp mh) (< mh dh))
                  (and (numberp mh) (not (integerp mh)) (< mh 1))))
         (or ;; if the mode line is taller than the default, use 3 lines
             (and (integerp dh) (integerp mh) (> mh dh))
             (and (numberp mh) (not (integerp mh)) (> mh 1))
             ;; if the mode line has a box with non-negative line-width,
             ;; use 3 lines
             (let* ((bx (face-attribute 'mode-line :box))
                    (lh (plist-get bx :line-width)))
               (and bx (or (not lh) (> lh 0))))
             ;; if the mode line has an overline, use 3 lines
             (not (memq (face-attribute 'mode-line :overline)
                        '(nil unspecified)))))))

(defun calculator-message (string &rest arguments)
  "Same as `message', but also handle electric mode."
  (apply 'message string arguments)
  (when calculator-electric-mode (sit-for 1) (message nil)))

;;;---------------------------------------------------------------------
;;; Operators

(defun calculator-op-arity (op)
  "Return OP's arity.
Current results are one of 2 (binary), +1 (postfix), -1 (prefix), or
0 (nullary)."
  (let ((arity (nth 3 op)))
    (cond ((numberp arity)                      arity)
          ((eq calculator-unary-style 'postfix) +1)
          (t                                    -1))))

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
      (or (eq (caar more-ops) 'nobind)
          (let ((i -1) (key (caar more-ops)))
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
      (setq added-ops (cons (if (eq (caar more-ops) 'nobind)
                              (cdar more-ops)
                              (car more-ops))
                            added-ops))
      (setq more-ops (cdr more-ops)))
    ;; added-ops come first, but in correct order
    (setq calculator-operators
          (append (nreverse added-ops) calculator-operators))))

;;;---------------------------------------------------------------------
;;; Display stuff

(defun calculator-reset ()
  "Reset calculator variables."
  (or calculator-restart-other-mode
      (setq calculator-stack           nil
            calculator-curnum          nil
            calculator-stack-display   nil
            calculator-display-fragile nil))
  (setq calculator-restart-other-mode nil)
  (calculator-update-display))

(defun calculator-get-display ()
  "Return a string to display.
The result should not exceed the screen width."
  (let* ((in-r  (and calculator-input-radix
                     (char-to-string
                      (car (rassq calculator-input-radix
                                  calculator-char-radix)))))
         (out-r (and calculator-output-radix
                     (char-to-string
                      (car (rassq calculator-output-radix
                                  calculator-char-radix)))))
         (prompt (format calculator-prompt
                         (cond ((or in-r out-r)
                                (concat (or in-r "=")
                                        (if (equal in-r out-r) "="
                                            (or out-r "="))))
                               (calculator-deg "D=")
                               (t "=="))))
         (expr
          (concat (cdr calculator-stack-display)
                  (cond
                    ;; entering a number
                    (calculator-curnum (concat calculator-curnum "_"))
                    ;; showing a result
                    ((and (= 1 (length calculator-stack))
                          calculator-display-fragile)
                     nil)
                    ;; waiting for a number or an operator
                    (t "?"))))
         (trim (+ (length expr) (length prompt) 1 (- (window-width)))))
    (concat prompt (if (<= trim 0) expr (substring expr trim)))))

(defun calculator-string-to-number (str)
  "Convert the given STR to a number, according to the value of
`calculator-input-radix'."
  (if calculator-input-radix
    (let ((radix
           (cdr (assq calculator-input-radix
                      '((bin . 2) (oct . 8) (hex . 16)))))
          (i -1) (value 0) (new-value 0))
      ;; assume mostly valid input (e.g., characters in range)
      (while (< (setq i (1+ i)) (length str))
        (setq new-value
              (let* ((ch (upcase (aref str i)))
                     (n (cond ((< ch ?0)  nil)
                              ((<= ch ?9) (- ch ?0))
                              ((< ch ?A)  nil)
                              ((<= ch ?Z) (- ch (- ?A 10)))
                              (t          nil))))
                (if (and n (<= 0 n) (< n radix))
                  (+ n (* radix value))
                  (progn
                    (calculator-message
                     "Warning: Ignoring bad input character `%c'." ch)
                    (sit-for 1)
                    value))))
        (when (if (< new-value 0) (> value 0) (< value 0))
          (calculator-message "Warning: Overflow in input."))
        (setq value new-value))
      value)
    (car (read-from-string
          (cond ((equal "." str) "0.0")
                ((string-match-p "[eE][+-]?$" str) (concat str "0"))
                ((string-match-p "\\.[0-9]\\|[eE]" str) str)
                ((string-match-p "\\." str)
                 ;; do this because Emacs reads "23." as an integer
                 (concat str "0"))
                ((stringp str) (concat str ".0"))
                (t "0.0"))))))

(defun calculator-push-curnum ()
  "Push the numeric value of the displayed number to the stack."
  (when calculator-curnum
    (push (calculator-string-to-number calculator-curnum)
          calculator-stack)
    (setq calculator-curnum nil)))

(defun calculator-rotate-displayer (&optional new-disp)
  "Switch to the next displayer on the `calculator-displayers' list.
Can be called with an optional argument NEW-DISP to force rotation to
that argument.
If radix output mode is active, toggle digit grouping."
  (interactive)
  (cond
    (calculator-output-radix
     (setq calculator-radix-grouping-mode
           (not calculator-radix-grouping-mode))
     (calculator-message
      "Digit grouping mode %s."
      (if calculator-radix-grouping-mode "ON" "OFF")))
    (t
     (setq calculator-displayers
           (if (and new-disp (memq new-disp calculator-displayers))
             (let ((tmp nil))
               (while (not (eq (car calculator-displayers) new-disp))
                 (setq tmp (cons (car calculator-displayers) tmp))
                 (setq calculator-displayers
                       (cdr calculator-displayers)))
               (setq calculator-displayers
                     (nconc calculator-displayers (nreverse tmp))))
             (nconc (cdr calculator-displayers)
                    (list (car calculator-displayers)))))
     (calculator-message
      "Using %s." (cadr (car calculator-displayers)))))
  (calculator-enter))

(defun calculator-rotate-displayer-back ()
  "Like `calculator-rotate-displayer', but rotates modes back.
If radix output mode is active, toggle digit grouping."
  (interactive)
  (calculator-rotate-displayer (car (last calculator-displayers))))

(defun calculator-displayer-prev ()
  "Send the current displayer function a `left' argument.
This is used to modify display arguments (if the current displayer
function supports this).
If radix output mode is active, increase the grouping size."
  (interactive)
  (if calculator-output-radix
    (progn (setq calculator-radix-grouping-digits
                 (1+ calculator-radix-grouping-digits))
           (calculator-enter))
    (and (car calculator-displayers)
         (let ((disp (caar calculator-displayers)))
           (cond ((symbolp disp) (funcall disp 'left))
                 ((and (consp disp) (eq 'std (car disp)))
                  (calculator-standard-displayer 'left)))))))

(defun calculator-displayer-next ()
  "Send the current displayer function a `right' argument.
This is used to modify display arguments (if the current displayer
function supports this).
If radix output mode is active, decrease the grouping size."
  (interactive)
  (if calculator-output-radix
    (progn (setq calculator-radix-grouping-digits
                 (max 2 (1- calculator-radix-grouping-digits)))
           (calculator-enter))
    (and (car calculator-displayers)
         (let ((disp (caar calculator-displayers)))
           (cond ((symbolp disp) (funcall disp 'right))
                 ((and (consp disp) (eq 'std (car disp)))
                  (calculator-standard-displayer 'right)))))))

(defun calculator-remove-zeros (numstr)
  "Get a number string NUMSTR and remove unnecessary zeros.
The behavior of this function is controlled by
`calculator-remove-zeros'."
  (let* ((s (if (not (eq calculator-remove-zeros t)) numstr
                ;; remove all redundant zeros leaving an integer
                (replace-regexp-in-string
                 "\\.0+\\([eE].*\\)?$" "\\1" numstr)))
         (s (if (not calculator-remove-zeros) s
                ;; remove zeros, except for first after the "."
                (replace-regexp-in-string
                 "\\(\\..[0-9]*?\\)0+\\([eE].*\\)?$" "\\1\\2" s))))
    s))

(defun calculator-groupize-number (str n sep &optional fromleft)
  "Return the input string STR with occurrences of SEP that separate
every N characters starting from the right, or from the left if
FROMLEFT is true."
  (let* ((len (length str)) (i (/ len n)) (j (% len n))
         (r (if (or (not fromleft) (= j 0)) '()
                (list (substring str (- len j))))))
    (while (> i 0)
      (let* ((e (* i n)) (e (if fromleft e (+ e j))))
        (push (substring str (- e n) e) r))
      (setq i (1- i)))
    (when (and (not fromleft) (> j 0))
      (push (substring str 0 j) r))
    (mapconcat 'identity r sep)))

(defun calculator-standard-displayer (num &optional char group-p)
  "Standard display function, used to display NUM.
Its behavior is determined by `calculator-number-digits' and the given
CHAR argument (both will be used to compose a format string).  If the
char is \"n\" then this function will choose one between %f or %e, this
is a work around %g jumping to exponential notation too fast.

It will also split digit sequences into comma-separated groups
and/or remove redundant zeros.

The special `left' and `right' symbols will make it change the current
number of digits displayed (`calculator-number-digits')."
  (if (symbolp num)
    (cond ((eq num 'left)
           (and (> calculator-number-digits 0)
                (setq calculator-number-digits
                      (1- calculator-number-digits))
                (calculator-enter)))
          ((eq num 'right)
           (setq calculator-number-digits
                 (1+ calculator-number-digits))
           (calculator-enter)))
    (let* ((s (if (eq char ?n)
                (let ((n (abs num)))
                  (if (or (and (< 0 n) (< n 0.001)) (< 1e8 n)) ?e ?f))
                char))
           (s (format "%%.%s%c" calculator-number-digits s))
           (s (calculator-remove-zeros (format s num)))
           (s (if (or (not group-p) (string-match-p "[eE]" s)) s
                  (replace-regexp-in-string
                   "\\([0-9]+\\)\\(?:\\..*\\|$\\)"
                   (lambda (_) (calculator-groupize-number
                                (match-string 1 s) 3 ","))
                   s nil nil 1))))
      s)))

(defun calculator-eng-display (num)
  "Display NUM in engineering notation.
The number of decimal digits used is controlled by
`calculator-number-digits', so to change it at runtime you have to use
the `left' or `right' when one of the standard modes is used."
  (if (symbolp num)
    (cond ((eq num 'left)
           (setq calculator-eng-extra
                 (if calculator-eng-extra (1+ calculator-eng-extra) 1))
           (let ((calculator-eng-tmp-show t)) (calculator-enter)))
          ((eq num 'right)
           (setq calculator-eng-extra
                 (if calculator-eng-extra (1- calculator-eng-extra) -1))
           (let ((calculator-eng-tmp-show t)) (calculator-enter))))
    (let ((exp 0))
      (unless (= 0 num)
        (while (< (abs num) 1.0)
          (setq num (* num 1000.0)) (setq exp (- exp 3)))
        (while (> (abs num) 999.0)
          (setq num (/ num 1000.0)) (setq exp (+ exp 3)))
        (when (and calculator-eng-tmp-show
                   (not (= 0 calculator-eng-extra)))
          (let ((i calculator-eng-extra))
            (while (> i 0)
              (setq num (* num 1000.0)) (setq exp (- exp 3))
              (setq i (1- i)))
            (while (< i 0)
              (setq num (/ num 1000.0)) (setq exp (+ exp 3))
              (setq i (1+ i))))))
      (or calculator-eng-tmp-show (setq calculator-eng-extra nil))
      (let ((str (format (format "%%.%sf" calculator-number-digits)
                         num)))
        (concat (let ((calculator-remove-zeros
                       ;; make sure we don't leave integers
                       (and calculator-remove-zeros 'x)))
                  (calculator-remove-zeros str))
                "e" (number-to-string exp))))))

(defun calculator-number-to-string (num)
  "Convert NUM to a displayable string."
  (cond
    ;; operators are printed here, the rest is for numbers
    ((not (numberp num)) (prin1-to-string (nth 1 num) t))
    ;; %f/%e handle these, but avoid them in radix or in user displayers
    ((and (floatp num) (isnan num)) "NaN")
    ((<= 1.0e+INF num) "Inf")
    ((<= num -1.0e+INF) "-Inf")
    (calculator-output-radix
     ;; print with radix -- for binary, convert the octal number
     (let* ((fmt (if (eq calculator-output-radix 'hex) "%x" "%o"))
            (str (if calculator-2s-complement num (abs num)))
            (str (format fmt (calculator-truncate str)))
            (bins '((?0 "000") (?1 "001") (?2 "010") (?3 "011")
                    (?4 "100") (?5 "101") (?6 "110") (?7 "111")))
            (str (if (not (eq calculator-output-radix 'bin)) str
                     (replace-regexp-in-string
                      "^0+\\(.\\)" "\\1"
                      (apply 'concat (mapcar (lambda (c)
                                               (cadr (assq c bins)))
                                     str)))))
            (str (if (not calculator-radix-grouping-mode) str
                     (calculator-groupize-number
                      str calculator-radix-grouping-digits
                      calculator-radix-grouping-separator))))
       (upcase (if (or calculator-2s-complement (>= num 0)) str
                   (concat "-" str)))))
    ((stringp calculator-displayer) (format calculator-displayer num))
    ((symbolp calculator-displayer) (funcall calculator-displayer num))
    ((eq 'std (car-safe calculator-displayer))
     (apply 'calculator-standard-displayer
            num (cdr calculator-displayer)))
    ((listp calculator-displayer)
     (eval `(let ((num ',num)) ,calculator-displayer) t))
    ;; nil (or bad) displayer
    (t (prin1-to-string num t))))

(defun calculator-update-display (&optional force)
  "Update the display.
If optional argument FORCE is non-nil, don't use the cached string."
  (set-buffer calculator-buffer)
  ;; update calculator-stack-display
  (when (or force (not (eq (car calculator-stack-display)
                           calculator-stack)))
    (setq calculator-stack-display
          (cons calculator-stack
                (if calculator-stack
                  (concat
                   (let ((calculator-displayer
                          (if (and calculator-displayers
                                   (= 1 (length calculator-stack)))
                            ;; customizable display for a single value
                            (caar calculator-displayers)
                            calculator-displayer)))
                     (mapconcat 'calculator-number-to-string
                                (reverse calculator-stack)
                                " "))
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
    (insert (calculator-get-display)))
  (set-buffer-modified-p nil)
  (goto-char (if calculator-display-fragile
               (1+ (length calculator-prompt))
               (1- (point)))))

;;;---------------------------------------------------------------------
;;; Stack computations

(defun calculator-reduce-stack-once (prec)
  "Worker for `calculator-reduce-stack'."
  (cl-flet ((check (ar op)        (and (listp op)
                                       (<= prec (calculator-op-prec op))
                                       (= ar (calculator-op-arity op))))
            (call (op &rest args) (apply 'calculator-funcall
                                         (nth 2 op) args)))
    (pcase calculator-stack
      ;; reduce "... ( x )" --> "... x"
      (`((,_ \) . ,_) ,(and X (pred numberp)) (,_ \( . ,_) . ,rest)
       (cons X rest))
      ;; reduce "... x op y" --> "... r", r is the result
      (`(,(and Y (pred numberp))
         ,(and O (pred (check 2)))
         ,(and X (pred numberp))
         . ,rest)
       (cons (call O X Y) rest))
      ;; reduce "... op x" --> "... r" for prefix op
      (`(,(and X (pred numberp)) ,(and O (pred (check -1))) . ,rest)
       (cons (call O X) rest))
      ;; reduce "... x op" --> "... r" for postfix op
      (`(,(and O (pred (check +1))) ,(and X (pred numberp)) . ,rest)
       (cons (call O X) rest))
      ;; reduce "... op" --> "... r" for 0-ary op
      (`(,(and O (pred (check 0))) . ,rest)
       (cons (call O) rest))
      ;; reduce "... y x" --> "... x"
      ;; (needed for 0-ary ops: replace current number with result)
      (`(,(and X (pred numberp)) ,(and _Y (pred numberp)) . ,rest)
       (cons X rest))
      (_ nil)))) ; nil = done

(defun calculator-reduce-stack (prec)
  "Reduce the stack using top operators as long as possible.
PREC is a precedence -- reduce everything with higher precedence."
  (let ((new nil))
    (while (setq new (calculator-reduce-stack-once prec))
      (setq calculator-stack new))))

(defun calculator-funcall (f &optional X Y)
  "If F is a symbol, evaluate (F X Y).
Otherwise, it should be a list, evaluate it with X, Y bound to the
arguments."
  ;; remember binary ops for calculator-repR/L
  (when Y (setq calculator-last-opXY (list f X Y)))
  (if (symbolp f)
    (cond ((and X Y) (funcall f X Y))
          (X         (funcall f X))
          (t         (funcall f)))
    ;; f is an expression
    (let ((TX (and X (calculator-truncate X)))
          (TY (and Y (calculator-truncate Y)))
          (DX (if (and X calculator-deg) (/ (* X pi) 180) X))
          (L  calculator-saved-list)
          (fF `(calculator-funcall ',f x y))
          (fD `(if calculator-deg (/ (* x 180) float-pi) x)))
      (eval `(cl-flet ((F (&optional x y) ,fF) (D (x) ,fD))
               (let ((X ,X) (Y ,Y) (DX ,DX) (TX ,TX) (TY ,TY) (L ',L))
                 ,f))
            t))))

;;;---------------------------------------------------------------------
;;; Input interaction

(defun calculator-last-input (&optional keys)
  "Last char (or event or event sequence) that was read.
Use KEYS if given, otherwise use `this-command-keys'."
  (let ((inp (or keys (this-command-keys))))
    (if (or (stringp inp) (not (arrayp inp)))
      inp
      ;; Translates kp-x to x and [tries to] create a string to lookup
      ;; operators; assume all symbols are translatable via
      ;; `function-key-map' or with an 'ascii-character property.  This
      ;; is needed because we have key bindings for kp-* (which might be
      ;; the wrong thing to do) so they don't get translated in
      ;; `this-command-keys'.
      (concat (mapcar (lambda (k)
                        (if (numberp k) k (or (get k 'ascii-character)
                                              (error "??bad key??"))))
                      (or (lookup-key function-key-map inp) inp))))))

(defun calculator-clear-fragile (&optional op)
  "Clear the fragile flag if it was set, then maybe reset all.
OP is the operator (if any) that caused this call."
  (when (and calculator-display-fragile
             (or (not op) (memq (calculator-op-arity op) '(-1 0))))
    ;; reset if last calc finished, and now get a num or prefix or 0-ary
    ;; op
    (calculator-reset))
  (setq calculator-display-fragile nil))

(defun calculator-digit ()
  "Enter a single digit."
  (interactive)
  (let ((inp (aref (calculator-last-input) 0)))
    (when (and (or calculator-display-fragile
                   (not (numberp (car calculator-stack))))
               (<= inp (pcase calculator-input-radix
                         (`nil ?9) (`bin ?1) (`oct ?7) (_ 999))))
      (calculator-clear-fragile)
      (setq calculator-curnum
            (concat (if (equal calculator-curnum "0") ""
                        calculator-curnum)
                    (list (upcase inp))))
      (calculator-update-display))))

(defun calculator-decimal ()
  "Enter a decimal period."
  (interactive)
  (when (and (not calculator-input-radix)
             (or calculator-display-fragile
                 (not (numberp (car calculator-stack))))
             (not (and calculator-curnum
                       (string-match-p "[.eE]" calculator-curnum))))
    ;; enter the period on the same condition as a digit, only if no
    ;; period or exponent entered yet
    (calculator-clear-fragile)
    (setq calculator-curnum (concat (or calculator-curnum "0") "."))
    (calculator-update-display)))

(defun calculator-exp ()
  "Enter an `E' exponent character, or a digit in hex input mode."
  (interactive)
  (cond
    (calculator-input-radix (calculator-digit))
    ((and (or calculator-display-fragile
              (not (numberp (car calculator-stack))))
          (not (and calculator-curnum
                    (string-match-p "[eE]" calculator-curnum))))
     ;; same condition as above, also no E so far
     (calculator-clear-fragile)
     (setq calculator-curnum (concat (or calculator-curnum "1") "e"))
     (calculator-update-display))))

(defun calculator-op (&optional keys)
  "Enter an operator on the stack, doing all necessary reductions.
Optional string argument KEYS will force using it as the keys entered."
  (interactive)
  (catch 'op-error
    (let* ((last-inp (calculator-last-input keys))
           (op (assoc last-inp calculator-operators)))
      (calculator-clear-fragile op)
      (calculator-push-curnum)
      (when (and (= 2 (calculator-op-arity op))
                 (not (numberp (car calculator-stack))))
        ;; we have a binary operator but no number -- search for a
        ;; prefix version
        (setq op (assoc last-inp (cdr (memq op calculator-operators))))
        (unless (and op (= -1 (calculator-op-arity op)))
          (calculator-message "Binary operator without a first operand")
          (throw 'op-error nil)))
      (calculator-reduce-stack
       (cond ((eq (nth 1 op) '\() 10)
             ((eq (nth 1 op) '\)) 0)
             (t (calculator-op-prec op))))
      (when (let ((hasnum (numberp (car calculator-stack))))
              (pcase (calculator-op-arity op)
                (-1 hasnum)
                ((or 1 2) (not hasnum))))
        (calculator-message "Incomplete expression")
        (throw 'op-error nil))
      (push op calculator-stack)
      (calculator-reduce-stack (calculator-op-prec op))
      (and (= (length calculator-stack) 1)
           (numberp (car calculator-stack))
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
      (calculator-update-display))))

(defun calculator-op-or-exp ()
  "Either enter an operator or a digit.
Used with +/- for entering them as digits in numbers like 1e-3 (there is
no need for negative numbers since these are handled by unary
operators)."
  (interactive)
  (if (and (not calculator-display-fragile)
           calculator-curnum
           (string-match-p "[eE]$" calculator-curnum))
    (calculator-digit)
    (calculator-op)))

;;;---------------------------------------------------------------------
;;; Input/output modes (not display)

(defun calculator-dec/deg-mode ()
  "Set decimal mode for display & input, if decimal, toggle deg mode."
  (interactive)
  (calculator-push-curnum)
  (if (or calculator-input-radix calculator-output-radix)
    (progn (setq calculator-input-radix nil)
           (setq calculator-output-radix nil))
    ;; already decimal -- toggle degrees mode
    (setq calculator-deg (not calculator-deg)))
  (calculator-update-display t))

(defun calculator-radix-mode (&optional keys)
  "Set input and display radix modes.
Optional string argument KEYS will force using it as the keys entered."
  (interactive)
  (calculator-radix-input-mode keys)
  (calculator-radix-output-mode keys))

(defun calculator-radix-input-mode (&optional keys)
  "Set input radix modes.
Optional string argument KEYS will force using it as the keys entered."
  (interactive)
  (calculator-push-curnum)
  (setq calculator-input-radix
        (let ((inp (calculator-last-input keys)))
          (cdr (assq (upcase (aref inp (1- (length inp))))
                     calculator-char-radix))))
  (calculator-update-display))

(defun calculator-radix-output-mode (&optional keys)
  "Set display radix modes.
Optional string argument KEYS will force using it as the keys entered."
  (interactive)
  (calculator-push-curnum)
  (setq calculator-output-radix
        (let ((inp (calculator-last-input keys)))
          (cdr (assq (upcase (aref inp (1- (length inp))))
                     calculator-char-radix))))
  (calculator-update-display t))

;;;---------------------------------------------------------------------
;;; Saved values list

(defun calculator-save-on-list ()
  "Evaluate current expression, put result on the saved values list."
  (interactive)
  (let ((calculator-add-saved t)) ; marks the result to be added
    (calculator-enter)))

(defun calculator-clear-saved ()
  "Clear the list of saved values in `calculator-saved-list'."
  (interactive)
  (setq calculator-saved-list nil)
  (setq calculator-saved-ptr 0)
  (calculator-update-display t))

(defun calculator-saved-move (n)
  "Go N elements up the list of saved values."
  (interactive)
  (when (and calculator-saved-list
             (or (null calculator-stack) calculator-display-fragile))
    (setq calculator-saved-ptr
          (max (min (+ n calculator-saved-ptr)
                    (length calculator-saved-list))
               0))
    (if (nth calculator-saved-ptr calculator-saved-list)
      (setq calculator-stack (list (nth calculator-saved-ptr
                                        calculator-saved-list))
            calculator-display-fragile t)
      (calculator-reset))
    (calculator-update-display)))

(defun calculator-saved-up ()
  "Go up the list of saved values."
  (interactive)
  (calculator-saved-move +1))

(defun calculator-saved-down ()
  "Go down the list of saved values."
  (interactive)
  (calculator-saved-move -1))

;;;---------------------------------------------------------------------
;;; Misc functions

(defun calculator-open-paren ()
  "Equivalents of `(' use this."
  (interactive)
  (calculator-op "("))

(defun calculator-close-paren ()
  "Equivalents of `)' use this."
  (interactive)
  (calculator-op ")"))

(defun calculator-enter ()
  "Evaluate current expression."
  (interactive)
  (calculator-op "="))

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
    ;; if the current number is from the saved-list remove it
    ((and calculator-display-fragile
          calculator-saved-list
          (= (car calculator-stack)
             (nth calculator-saved-ptr calculator-saved-list)))
     (if (= 0 calculator-saved-ptr)
       (setq calculator-saved-list (cdr calculator-saved-list))
       (let ((p (nthcdr (1- calculator-saved-ptr)
                        calculator-saved-list)))
         (setcdr p (cddr p))
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
  (let ((calculator-displayer
         (or calculator-copy-displayer calculator-displayer))
        (calculator-displayers
         (if calculator-copy-displayer nil calculator-displayers)))
    (calculator-enter)
    ;; remove trailing spaces and an index
    (let ((s (cdr calculator-stack-display)))
      (when s
        (kill-new (replace-regexp-in-string
                   "^\\([^ ]+\\) *\\(\\[[0-9/]+\\]\\)? *$" "\\1" s))))))

(defun calculator-set-register (reg)
  "Set a register value for REG."
  ;; FIXME: this should use `register-read-with-preview', but it uses
  ;; calculator-registers rather than `register-alist'.  (Maybe
  ;; dynamically rebinding it will get blessed?)  Also in to
  ;; `calculator-get-register'.
  (interactive "cRegister to store into: ")
  (let* ((as  (assq reg calculator-registers))
         (val (progn (calculator-enter) (car calculator-stack))))
    (if as
      (setcdr as val)
      (setq calculator-registers
            (cons (cons reg val) calculator-registers)))
    (calculator-message "[%c] := %S" reg val)))

(defun calculator-put-value (val)
  "Paste VAL as if entered.
Used by `calculator-paste' and `get-register'."
  (when (and (numberp val)
             ;; (not calculator-curnum)
             (or calculator-display-fragile
                 (not (numberp (car calculator-stack)))))
    (calculator-clear-fragile)
    (setq calculator-curnum (let ((calculator-displayer "%S"))
                              (calculator-number-to-string val)))
    (calculator-update-display)))

(defun calculator-paste ()
  "Paste a value from the `kill-ring'."
  (interactive)
  (calculator-put-value
   (let ((str (replace-regexp-in-string
               "^ *\\(.+[^ ]\\) *$" "\\1" (current-kill 0))))
     (and (not calculator-input-radix)
          calculator-paste-decimals
          (string-match "\\([0-9]+\\)\\(\\.[0-9]+\\)?\\(e[0-9]+\\)?"
                        str)
          (or (match-string 1 str)
              (match-string 2 str)
              (match-string 3 str))
          (setq str (concat (or (match-string 1 str) "0")
                            (or (match-string 2 str) ".0")
                            (or (match-string 3 str) ""))))
     (ignore-errors (calculator-string-to-number str)))))

(defun calculator-get-register (reg)
  "Get a value from a register REG."
  (interactive "cRegister to get value from: ")
  (calculator-put-value (cdr (assq reg calculator-registers))))

(declare-function electric-describe-mode "ehelp" ())

(defun calculator-help ()
  ;; this is used as the quick reference screen you get with `h'
  "Quick reference:
* numbers/operators/parens/./e - enter expressions
  + - * / \\(div) %(rem) _(-X,postfix) ;(1/X,postfix) ^(exp) L(og)
  Q(sqrt) !(fact) S(in) C(os) T(an) |(or) #(xor) &(and) ~(not)
* >/< repeats last binary operation with its 2nd (1st) arg as postfix op
* I inverses next trig function        * '/\"/{} - display/display args
* D         - switch to all-decimal, or toggle deg/rad mode
* B/O/H/X   - binary/octal/hex mode for i/o (X is a shortcut for H)
* i/o       - prefix for d/b/o/x - set only input/output modes
* enter/=   - evaluate current expr.   * s/g      - set/get a register
* space     - evaluate & save on list  * l/v      - list total/average
* up/down/C-p/C-n - browse saved       * C-delete - clear all saved
* C-insert  - copy whole expr.         * C-return - evaluate, copy, exit
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
      (when calculator-electric-mode
        (use-global-map calculator-saved-global-map))
      (if calculator-electric-mode
        (electric-describe-mode)
        (describe-mode))
      (when calculator-electric-mode (use-global-map g-map))
      (select-window win)
      (message nil))
    (let ((one (one-window-p t))
          (win (selected-window))
          (help-buf (get-buffer-create "*Help*")))
      (save-window-excursion
        (with-output-to-temp-buffer "*Help*"
          (princ (documentation 'calculator-help)))
        (when one (shrink-window-if-larger-than-buffer
                   (get-buffer-window help-buf)))
        (message "`%s' again for more help, %s."
                 (calculator-last-input)
                 "any other key continues normally")
        (select-window win)
        (sit-for 360))
      (select-window win))))

(defun calculator-quit ()
  "Quit calculator."
  (interactive)
  (set-buffer calculator-buffer)
  (let ((inhibit-read-only t)) (erase-buffer))
  (unless calculator-electric-mode
    (ignore-errors
      (while (get-buffer-window calculator-buffer)
        (delete-window (get-buffer-window calculator-buffer)))))
  (kill-buffer calculator-buffer)
  (message "Calculator done.")
  (if calculator-electric-mode
    (throw 'calculator-done nil) ; will kill the buffer
    (setq calculator-buffer nil)))

(defun calculator-save-and-quit ()
  "Quit the calculator, saving the result on the `kill-ring'."
  (interactive)
  (calculator-enter)
  (calculator-copy)
  (calculator-quit))

(defun calculator-repR (x)
  "Repeat the last binary operation with its second argument and X.
To use this, apply a binary operator (evaluate it), then call this."
  (if calculator-last-opXY
    ;; avoid rebinding calculator-last-opXY
    (let ((calculator-last-opXY calculator-last-opXY))
      (calculator-funcall
       (car calculator-last-opXY) x (nth 2 calculator-last-opXY)))
    x))

(defun calculator-repL (x)
  "Repeat the last binary operation with its first argument and X.
To use this, apply a binary operator (evaluate it), then call this."
  (if calculator-last-opXY
    ;; avoid rebinding calculator-last-opXY
    (let ((calculator-last-opXY calculator-last-opXY))
      (calculator-funcall
       (car calculator-last-opXY) (nth 1 calculator-last-opXY) x))
    x))

(defun calculator-expt (x y)
  "Compute X^Y, dealing with errors appropriately."
  (condition-case nil
      (expt x y)
    (domain-error 0.0e+NaN)
    (range-error
     (cond ((and (< x 1.0) (> x -1.0))
            ;; For small x, the range error comes from large y.
            0.0)
           ((and (> x 0.0) (< y 0.0))
            ;; For large positive x and negative y, the range error
            ;; comes from large negative y.
            0.0)
           ((and (> x 0.0) (> y 0.0))
            ;; For large positive x and positive y, the range error
            ;; comes from large y.
            1.0e+INF)
           ;; For the rest, x must be large and negative.
           ;; The range errors come from large integer y.
           ((< y 0.0)
            0.0)
           ((eq (logand (truncate y) 1) 1)   ; expansion of cl `oddp'
            ;; If y is odd
            -1.0e+INF)
           (t
            ;;
            1.0e+INF)))
    (error 0.0e+NaN)))

(defun calculator-fact (x)
  "Simple factorial of X."
  (cond ((>= x 1.0e+INF) x)
        ((or (and (floatp x) (isnan x)) (< x 0)) 0.0e+NaN)
        ((>= (calculator-expt (/ x 3.0) x) 1.0e+INF) 1.0e+INF)
        (t (let ((x (truncate x)) (r 1.0))
             (while (> x 0) (setq r (* r x) x (1- x)))
             r))))

(defun calculator-truncate (n)
  "Truncate N, return 0 in case of overflow."
  (condition-case nil (truncate n) (range-error 0)))


(provide 'calculator)

;;; calculator.el ends here
