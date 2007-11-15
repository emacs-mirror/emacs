;;; calc-menu.el --- a menu for Calc

;; Copyright (C) 2007 Free Software Foundation, Inc.

;; Maintainer: Jay Belanger <jay.p.belanger@gmail.com>

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
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

(defvar calc-arithmetic-menu
  (list "Arithmetic"
        (list "Basic"
              ["-(1:)"         calc-change-sign :keys "n"]
              ["(2:) + (1:)"   calc-plus   :keys "+"]
              ["(2:) - (1:)"   calc-minus  :keys "-"]
              ["(2:) * (1:)"   calc-times  :keys "*"]
              ["(2:) / (1:)"   calc-divide :keys "/"]
              ["(2:) ^ (1:)"   calc-power  :keys "^"]
              ["(2:) ^ (1/(1:))" 
               (progn
                 (require 'calc-ext)
                 (let ((calc-inverse-flag t))
                   (call-interactively 'calc-power)))
               :keys "I ^"
               :help "The (1:)th root of (2:)"]
              ["abs(1:)"   
               (progn 
                 (require 'calc-arith)
                 (call-interactively 'calc-abs))
               :keys "A"
               :help "Absolute value"]
              ["1/(1:)"
               (progn
                 (require 'calc-arith)
                 (call-interactively 'calc-inv))
               :keys "&"]
              ["sqrt(1:)" 
               (progn
                 (require 'calc-math)
                 (call-interactively 'calc-sqrt))
               :keys "Q"]
              ["idiv(2:,1:)"
               (progn
                 (require 'calc-arith)
                 (call-interactively 'calc-idiv))
               :keys "\\"
               :help "The integer quotient of (2:) over (1:)"]
              ["(2:) mod (1:)"  
               (progn
                 (require 'calc-misc)
                 (call-interactively 'calc-mod))
               :keys "%"
               :help "The remainder when (2:) is divided by (1:)"])
        (list "Rounding"
              ["floor(1:)" 
               (progn
                 (require 'calc-arith)
                 (call-interactively 'calc-floor))
               :keys "F"
               :help "The greatest integer less than or equal to (1:)"]
              ["ceiling(1:)"  
               (progn
                 (require 'calc-arith)
                 (call-interactively 'calc-ceiling))
               :keys "I F"
               :help "The smallest integer greater than or equal to (1:)"]
              ["round(1:)"    
               (progn
                 (require 'calc-arith)
                 (call-interactively 'calc-round))
               :keys "R"
               :help "The nearest integer to (1:)"]
              ["truncate(1:)" 
               (progn
                 (require 'calc-arith)
                 (call-interactively 'calc-trunc))
               :keys "I R"
               :help "The integer part of (1:)"])
        (list "Complex Numbers"
              ["Re(1:)"
               (progn
                 (require 'calc-cplx)
                 (call-interactively 'calc-re))
               :keys "f r"]
              ["Im(1:)"
               (progn
                 (require 'calc-cplx)
                 (call-interactively 'calc-im))
               :keys "f i"]
              ["conj(1:)"
               (progn
                 (require 'calc-cplx)
                 (call-interactively 'calc-conj))
               :keys "J"
               :help "The complex conjugate of (1:)"]
              ["length(1:)"
               (progn (require 'calc-arith)
                      (call-interactively 'calc-abs))
               :keys "A"
               :help "The length (absolute value) of (1:)"]
              ["arg(1:)"
               (progn
                 (require 'calc-cplx)
                 (call-interactively 'calc-argument))
               :keys "G"
               :help "The argument (polar angle) of (1:)"])
        (list "Conversion"
              ["Convert (1:) to a float"    
               (progn
                 (require 'calc-ext)
                 (call-interactively 'calc-float))
               :keys "c f"]
              ["Convert (1:) to a fraction" 
               (progn
                 (require 'calc-ext)
                 (call-interactively 'calc-fraction))
               :keys "c F"])
        (list "Binary"
              ["Set word size" 
               (progn
                 (require 'calc-bin)
                 (call-interactively 'calc-word-size))
               :keys "b w"]
              ["Clip (1:) to word size" 
               (progn
                 (require 'calc-bin)
                 (call-interactively 'calc-clip))
               :keys "b c"
               :help "Reduce (1:) modulo 2^wordsize"]
              ["(2:) and (1:)"    
               (progn
                 (require 'calc-bin)
                 (call-interactively 'calc-and))
               :keys "b a"
               :help "Bitwise AND [modulo 2^wordsize]"]
              ["(2:) or (1:)"
               (progn
                 (require 'calc-bin)
                 (call-interactively 'calc-or))
               :keys "b o"
               :help "Bitwise inclusive OR [modulo 2^wordsize]"]
              ["(2:) xor (1:)"
               (progn 
                 (require 'calc-bin)
                 (call-interactively 'calc-xor))
               :keys "b x"
               :help "Bitwise exclusive OR [modulo 2^wordsize]"]
              ["diff(2:,1:)" 
               (progn
                 (require 'calc-bin)
                 (call-interactively 'calc-diff))
               :keys "b d"
               :help "Bitwise difference [modulo 2^wordsize]"]
              ["not (1:)"
               (progn
                 (require 'calc-bin)
                 (call-interactively 'calc-not))
               :keys "b n"
               :help "Bitwise NOT [modulo 2^wordsize]"]
              ["left shift(1:)"
               (progn
                 (require 'calc-bin)
                 (call-interactively 'calc-lshift-binary))
               :keys "b l"
               :help "Shift (1:)[modulo 2^wordsize] one bit left"]
              ["right shift(1:)"
               (progn
                 (require 'calc-bin)
                 (call-interactively 'calc-rshift-binary))
               :keys "b r"
               :help "Shift (1:)[modulo 2^wordsize] one bit right, putting 0s on the left"]
              ["arithmetic right shift(1:)"
               (progn
                 (require 'calc-bin)
                 (call-interactively 'calc-rshift-arith))
               :keys "b R"
               :help "Shift (1:)[modulo 2^wordsize] one bit right, duplicating the leftmost bit"]
              ["rotate(1:)"
               (progn
                 (require 'calc-bin)
                 (call-interactively 'calc-rotate-binary))
               :keys "b t"
               :help "Rotate (1:)[modulo 2^wordsize] one bit left"])
        "-------"
        ["Help on Arithmetic"
         (calc-info-goto-node "Arithmetic")])
  "Menu for Calc's arithmetic functions.")

(defvar calc-scientific-function-menu
  (list "Scientific functions"
        (list "Constants"
              ["pi"
               (progn
                 (require 'calc-math)
                 (call-interactively 'calc-pi))
               :keys "P"]
              ["e"
               (progn
                 (require 'calc-math)
                 (let ((calc-hyperbolic-flag t))
                   (call-interactively 'calc-pi)))
               :keys "H P"]
              ["phi"
               (progn
                 (require 'calc-math)
                 (let ((calc-inverse-flag t)
                       (calc-hyperbolic-flag t))
                   (call-interactively 'calc-pi)))
               :keys "I H P"
               :help "The golden ratio"]
              ["gamma"
               (progn
                 (require 'calc-math)
                 (let ((calc-inverse-flag t))
                   (call-interactively 'calc-pi)))
               :keys "I P"
               :help "Euler's constant"])
        (list "Logs and exps"
              ["ln(1:)" 
               (progn 
                 (require 'calc-math) 
                 (call-interactively 'calc-ln)) 
               :keys "L"
               :help "The natural logarithm"]
              ["e^(1:)"   
               (progn
                 (require 'calc-math)
                 (call-interactively 'calc-exp))
               :keys "E"]
              ["log(1:) [base 10]" 
               (progn
                 (require 'calc-math)
                 (call-interactively 'calc-log10))
               :keys "H L"
               :help "The common logarithm"]
              ["10^(1:)" 
               (progn
                 (require 'calc-math)
                 (let ((calc-inverse-flag t))
                   (call-interactively 'calc-log10)))
               :keys "I H L"]
              ["log(2:) [base(1:)]" 
               (progn
                 (require 'calc-math)
                 (call-interactively 'calc-log))
               :keys "B"
               :help "The logarithm with an arbitrary base"]
              ["(2:) ^ (1:)"  
               calc-power 
               :keys "^"])
        (list "Trig functions"
              ["sin(1:)"  
               (progn
                 (require 'calc-math)
                 (call-interactively 'calc-sin))
               :keys "S"]
              ["cos(1:)"  
               (progn
                 (require 'calc-math)
                 (call-interactively 'calc-cos))
               :keys "C"]
              ["tan(1:)"  
               (progn
                 (require 'calc-math)
                 (call-interactively 'calc-tan))
               :keys "T"]
              ["arcsin(1:)" 
               (progn
                 (require 'calc-math)
                 (call-interactively 'calc-arcsin))
               :keys "I S"]
              ["arccos(1:)" 
               (progn
                 (require 'calc-math)
                 (call-interactively 'calc-arccos))
               :keys "I C"]
              ["arctan(1:)" 
               (progn
                 (require 'calc-math)
                 (call-interactively 'calc-arctan))
               :keys "I T"]
              ["arctan2(2:,1:)" 
               (progn
                 (require 'calc-math)
                 (call-interactively 'calc-arctan2))
               :keys "f T"]
              "--Angle measure--"
              ["Radians"
               (progn
                 (require 'calc-math)
                 (calc-radians-mode))
               :keys "m r"
               :style radio
               :selected (eq calc-angle-mode 'rad)]
              ["Degrees"
               (progn
                 (require 'calc-math)
                 (calc-degrees-mode))
               :keys "m d"
               :style radio
               :selected (eq calc-angle-mode 'deg)]
              ["HMS"
               (progn
                 (require 'calc-math)
                 (calc-hms-mode))
               :keys "m h"
               :style radio
               :selected (eq calc-angle-mode 'hms)])
        (list "Hyperbolic Functions"
              ["sinh(1:)" 
               (progn
                 (require 'calc-math)
                 (call-interactively 'calc-sinh))
               :keys "H S"]
              ["cosh(1:)" 
               (progn
                 (require 'calc-math)
                 (call-interactively 'calc-cosh))
               :keys "H C"]
              ["tanh(1:)" 
               (progn
                 (require 'calc-math)
                 (call-interactively 'calc-tanh))
               :keys "H T"]
              ["arcsinh(1:)" 
               (progn
                 (require 'calc-math)
                 (call-interactively 'calc-arcsinh))
               :keys "I H S"]
              ["arccosh(1:)" 
               (progn
                 (require 'calc-math)
                 (call-interactively 'calc-arccosh))
               :keys "I H C"]
              ["arctanh(1:)" 
               (progn
                 (require 'calc-math)
                 (call-interactively 'calc-arctanh))
               :keys "I H T"])
        (list "Advanced math"
              ["Gamma(1:)" 
               (progn
                 (require 'calc-comb)
                 (call-interactively 'calc-gamma))
               :keys "f g"
               :help "The Euler Gamma function"]
              ["GammaP(2:,1:)" 
               (progn
                 (require 'calc-funcs)
                 (call-interactively 'calc-inc-gamma))
               :keys "f G"
               :help "The lower incomplete Gamma function"]
              ["Beta(2:,1:)" 
               (progn
                 (require 'calc-funcs)
                 (call-interactively 'calc-beta))
               :keys "f b"
               :help "The Euler Beta function"]
              ["BetaI(3:,2:,1:)" 
               (progn
                 (require 'calc-funcs)
                 (call-interactively 'calc-inc-beta))
               :keys "f B"
               :help "The incomplete Beta function"]
              ["erf(1:)"
               (progn
                 (require 'calc-funcs)
                 (call-interactively 'calc-erf))
               :keys "f e"
               :help "The error function"]
              ["BesselJ(2:,1:)" 
               (progn
                 (require 'calc-funcs)
                 (call-interactively 'calc-bessel-J))
               :keys "f j"
               :help "The Bessel function of the first kind (of order (2:))"]
              ["BesselY(2:,1:)" 
               (progn
                 (require 'calc-funcs)
                 (call-interactively 'calc-bessel-Y))
               :keys "f y"
               :help "The Bessel function of the second kind (of order (2:))"])
        (list "Combinatorial"
              ["gcd(2:,1:)" 
               (progn
                 (require 'calc-comb)
                 (call-interactively 'calc-gcd))
                 :keys "k g"]
              ["lcm(2:,1:)" 
               (progn
                 (require 'calc-comb)
                 (call-interactively 'calc-lcm))
               :keys "k l"]
              ["factorial(1:)" 
               (progn
                 (require 'calc-comb)
                 (call-interactively 'calc-factorial))
               :keys "!"]
              ["(2:) choose (1:)" 
               (progn
                 (require 'calc-comb)
                 (call-interactively 'calc-choose))
               :keys "k c"]
              ["permutations(2:,1:)" 
               (progn
                 (require 'calc-comb)
                 (call-interactively 'calc-perm))
               :keys "H k c"]
              ["Primality test for (1:)" 
               (progn
                 (require 'calc-comb)
                 (call-interactively 'calc-prime-test))
               :keys "k p"
               :help "For large (1:), a probabilistic test"]
              ["Factor (1:) into primes" 
               (progn
                 (require 'calc-comb)
                 (call-interactively 'calc-prime-factors))
               :keys "k f"]
              ["Next prime after (1:)"
               (progn
                 (require 'calc-comb)
                 (call-interactively 'calc-next-prime))
               :keys "k n"]
              ["Previous prime before (1:)"
               (progn
                 (require 'calc-comb)
                 (call-interactively 'calc-prev-prime))
               :keys "I k n"]
              ["phi(1:)"
               (progn
                 (require 'calc-comb)
                 (call-interactively 'calc-totient))
               :keys "k n"
               :help "Euler's totient function"]
              ["random(1:)"
               (progn
                 (require 'calc-comb)
                 (call-interactively 'calc-random))
               :keys "k r"
               :help "A random number >=1 and < (1:)"])
        "----"
        ["Help on Scientific Functions"
         (calc-info-goto-node "Scientific Functions")])
  "Menu for Calc's scientific functions.")
              
(defvar calc-algebra-menu
  (list "Algebra"
        (list "Simplification"
              ["Simplify (1:)" 
               (progn
                 (require 'calc-alg)
                 (call-interactively 'calc-simplify))
               :keys "a s"]
              ["Simplify (1:) with extended rules" 
               (progn
                 (require 'calc-alg)
                 (call-interactively 'calc-simplify-extended))
               :keys "a e"
               :help "Apply possibly unsafe simplifications"])
        (list "Manipulation"
              ["Expand formula (1:)" 
               (progn
                 (require 'calc-alg)
                 (call-interactively 'calc-expand-formula))
               :keys "a \""
               :help "Expand (1:) into its defining formula, if possible"]
              ["Evaluate variables in (1:)" 
               (progn
                 (require 'calc-ext)
                 (call-interactively 'calc-evaluate))
               :keys "="]
              ["Make substitution in (1:)" 
               (progn
                 (require 'calc-alg)
                 (call-interactively 'calc-substitute))
               :keys "a b"
               :help 
               "Substitute all occurrences of a sub-expression with a new sub-expression"])
        (list "Polynomials"
              ["Factor (1:)" 
               (progn
                 (require 'calc-alg)
                 (call-interactively 'calc-factor))
               :keys "a f"]
              ["Collect terms in (1:)" 
               (progn
                 (require 'calc-alg)
                 (call-interactively 'calc-collect)) 
               :keys "a c"
               :help "Arrange as a polynomial in a given variable"]
              ["Expand (1:)" 
               (progn
                 (require 'calc-alg)
                 (call-interactively 'calc-expand))
               :keys "a x"
               :help "Apply distributive law everywhere"]
              ["Find roots of (1:)" 
               (progn
                 (require 'calcalg2)
                 (call-interactively 'calc-poly-roots))
               :keys "a P"])
        (list "Calculus"
              ["Differentiate (1:)" 
               (progn
                 (require 'calcalg2)
                 (call-interactively 'calc-derivative))
               :keys "a d"]
              ["Integrate (1:) [indefinite]" 
               (progn
                 (require 'calcalg2)
                 (call-interactively 'calc-integral))
               :keys "a i"]
              ["Integrate (1:) [definite]" 
               (progn
                 (require 'calcalg2)
                 (let ((var (read-string "Integration variable: ")))
                   (calc-tabular-command 'calcFunc-integ "Integration" 
                                         "intg" nil var nil nil)))
               :keys "C-u a i"]
              ["Integrate (1:) [numeric]"
               (progn
                 (require 'calcalg2)
                 (call-interactively 'calc-num-integral))
               :keys "a I"
               :help "Integrate using the open Romberg method"]
              ["Taylor expand (1:)" 
               (progn
                 (require 'calcalg2)
                 (call-interactively 'calc-taylor))
               :keys "a t"]
              ["Minimize (2:) [initial guess = (1:)]" 
               (progn
                 (require 'calcalg3)
                 (call-interactively 'calc-find-minimum))
               :keys "a N"
               :help "Find a local minimum"]
              ["Maximize (2:) [initial guess = (1:)]" 
               (progn
                 (require 'calcalg3)
                 (call-interactively 'calc-find-maximum))
               :keys "a X"
               :help "Find a local maximum"])
        (list "Solving"
              ["Solve equation (1:)" 
               (progn
                 (require 'calcalg2)
                 (call-interactively 'calc-solve-for))
               :keys "a S"]
              ["Solve equation (2:) numerically [initial guess = (1:)]" 
               (progn
                 (require 'calcalg3)
                 (call-interactively 'calc-find-root))
               :keys "a R"]
              ["Find roots of polynomial (1:)" 
               (progn
                 (require 'calcalg2)
                 (call-interactively 'calc-poly-roots))
               :keys "a P"])
        (list "Curve fitting"
              ["Fit (1:)=[x values, y values] to a curve" 
               (progn
                 (require 'calcalg3)
                 (call-interactively 'calc-curve-fit))
               :keys "a F"])
        "----"
        ["Help on Algebra"
         (calc-info-goto-node "Algebra")])
  "Menu for Calc's algebraic facilities.")


(defvar calc-graphics-menu
  (list "Graphics"
        ["Graph 2D [(1:)= y values, (2:)= x values]" 
         (progn
           (require 'calc-graph)
           (call-interactively 'calc-graph-fast))
         :keys "g f"]
        ["Graph 3D [(1:)= z values, (2:)= y values, (3:)= xvalues]" 
         (progn
           (require 'calc-graph)
           (call-interactively 'calc-graph-fast-3d))
         :keys "g F"]
        "----"
        ["Help on Graphics"
         (calc-info-goto-node "Graphics")])
  "Menu for Calc's graphics.")


(defvar calc-vectors-menu
  (list "Matrices/Vectors"
        (list "Matrices"
              ["(2:) + (1:)"   calc-plus   :keys "+"]
              ["(2:) - (1:)"   calc-minus  :keys "-"]
              ["(2:) * (1:)"   calc-times  :keys "*"]
              ["(1:)^(-1)"    
               (progn
                 (require 'calc-arith)
                 (call-interactively 'calc-inv))
               :keys "&"]
              ["Create an identity matrix"
               (progn 
                 (require 'calc-vec)
                 (call-interactively 'calc-ident))
               :keys "v i"]
              ["transpose(1:)" 
               (progn
                 (require 'calc-vec)
                 (call-interactively 'calc-transpose))
               :keys "v t"]
              ["det(1:)" 
               (progn
                 (require 'calc-mtx)
                 (call-interactively 'calc-mdet))
               :keys "V D"]
              ["trace(1:)"
               (progn
                 (require 'calc-mtx)
                 (call-interactively 'calc-mtrace))
               :keys "V T"]
              ["LUD decompose (1:)" 
               (progn
                 (require 'calc-mtx)
                 (call-interactively 'calc-mlud))
               :keys "V L"]
              ["Extract a row from (1:)" 
               (progn
                 (require 'calc-vec)
                 (call-interactively 'calc-mrow))
               :keys "v r"]
              ["Extract a column from (1:)" 
               (progn
                 (require 'calc-vec)
                 (call-interactively 'calc-mcol))
               :keys "v c"])
        (list "Vectors"
              ["Extract the first element of (1:)"
               (progn
                 (require 'calc-vec)
                 (call-interactively 'calc-head))
               :keys "v h"]
              ["Extract an element from (1:)" 
               (progn
                 (require 'calc-vec)
                 (call-interactively 'calc-mrow))
               :keys "v r"]
              ["Reverse (1:)" 
               (progn
                 (require 'calc-vec)
                 (call-interactively 'calc-reverse-vector))
               :keys "v v"]
              ["Unpack (1:)"
               (progn
                 (require 'calc-vec)
                 (call-interactively 'calc-unpack))
               :keys "v u"
               :help "Separate the elements of (1:)"]
              ["(2:) cross (1:)" 
               (progn
                 (require 'calc-vec)
                 (call-interactively 'calc-cross))
               :keys "V C"
               :help "The cross product in R^3"]
              ["(2:) dot (1:)" 
               calc-mult 
               :keys "*"
               :help "The dot product"]
              ["Map a function across (1:)" 
               (progn
                 (require 'calc-map)
                 (call-interactively 'calc-map))
               :keys "V M"
               :help "Apply a function to each element"])
        (list "Vectors as sets"
              ["Remove duplicates from (1:)" 
               (progn
                 (require 'calc-vec)
                 (call-interactively 'calc-remove-duplicates))
               :keys "V +"]
              ["(2:) union (1:)" 
               (progn
                 (require 'calc-vec)
                 (call-interactively 'calc-set-union))
               :keys "V V"]
              ["(2:) intersect (1:)" 
               (progn
                 (require 'calc-vec)
                 (call-interactively 'calc-set-intersect))
               :keys "V ^"]
              ["(2:) \\ (1:)" 
               (progn
                 (require 'calc-vec)
                 (call-interactively 'calc-set-difference))
               :keys "V -"
               :help "Set difference"])
        (list "Statistics on vectors"
              ["length(1:)" 
               (progn
                 (require 'calc-stat)
                 (call-interactively 'calc-vector-count))
               :keys "u #"
               :help "The number of data values"]
              ["sum(1:)"    
               (progn
                 (require 'calc-stat)
                 (call-interactively 'calc-vector-sum))
               :keys "u +"
               :help "The sum of the data values"]
              ["max(1:)"    
               (progn
                 (require 'calc-stat)
                 (call-interactively 'calc-vector-max))  
               :keys "u x"
               :help "The maximum of the data values"]
              ["min(1:)"    
               (progn
                 (require 'calc-stat)
                 (call-interactively 'calc-vector-min))  
               :keys "u N"
               :help "The minumum of the data values"]
              ["mean(1:)"   
               (progn
                 (require 'calc-stat)
                 (call-interactively 'calc-vector-mean))
               :keys "u M"
               :help "The average (arithmetic mean) of the data values"]
              ["mean(1:) with error"
              (progn
                (require 'calc-stat)
                (call-interactively 'calc-vector-mean-error))
              :keys "I u M"
              :help "The average (arithmetic mean) of the data values as an error form"]
              ["sdev(1:)"   
               (progn
                 (require 'calc-stat)
                 (call-interactively 'calc-vector-sdev))
               :keys "u S"
               :help "The sample sdev, sqrt[sum((values - mean)^2)/(N-1)]"]
              ["variance(1:)" 
               (progn
                 (require 'calc-stat)
                 (call-interactively 'calc-vector-variance))
               :keys "H u S"
               :help "The sample variance, sum((values - mean)^2)/(N-1)"]
              ["population sdev(1:)" 
               (progn
                 (require 'calc-stat)
                 (call-interactively 'calc-vector-pop-sdev))
               :keys "I u S"
               :help "The population sdev, sqrt[sum((values - mean)^2)/N]"]
              ["population variance(1:)" 
               (progn
                 (require 'calc-stat)
                 (call-interactively 'calc-vector-pop-variance))
               :keys "H I u S"
               :help "The population variance, sum((values - mean)^2)/N"]
              ["median(1:)"
               (progn
                 (require 'calc-stat)
                 (call-interactively 'calc-vector-median))
               :keys "H u M"
               :help "The median of the data values"]
              ["harmonic mean(1:)"
               (progn
                 (require 'calc-stat)
                 (call-interactively 'calc-vector-harmonic-mean))
               :keys "H I u M"]
              ["geometric mean(1:)"
               (progn
                 (require 'calc-stat)
                 (call-interactively 'calc-vector-geometric-mean))
               :keys "u G"]
              ["arithmetic-geometric mean(1:)"
               (progn
                 (require 'calc-stat)
                 (let ((calc-hyperbolic-flag t))
                   (call-interactively 'calc-vector-geometric-mean)))
               :keys "H u G"]
               ["RMS(1:)"
                (progn (require 'calc-arith)
                       (call-interactively 'calc-abs))
                :keys "A"
                :help "The root-mean-square, or quadratic mean"])
        ["Abbreviate long vectors"
         (progn
           (require 'calc-mode)
           (call-interactively 'calc-full-vectors))
         :keys "v ."
         :style toggle
         :selected (not calc-full-vectors)]
        "----"
        ["Help on Matrices/Vectors"
         (calc-info-goto-node "Matrix Functions")])
  "Menu for Calc's vector and matrix functions.")

(defvar calc-units-menu
  (list "Units"
        ["Convert units in (1:)" 
         (progn
           (require 'calc-units)
           (call-interactively 'calc-convert-units ))
         :keys "u c"]
        ["Convert temperature in (1:)" 
         (progn
           (require 'calc-units)
           (call-interactively 'calc-convert-temperature))
         :keys "u t"]
        ["Simplify units in (1:)" 
         (progn
           (require 'calc-units)
           (call-interactively 'calc-simplify-units))
         :keys "u s"]
        ["View units table" 
         (progn
           (require 'calc-units)
           (call-interactively 'calc-view-units-table))
         :keys "u V"]
        "----"
        ["Help on Units"
         (calc-info-goto-node "Units")])
  "Menu for Calc's units functions.")

(defvar calc-variables-menu
  (list "Variables"
        ["Store (1:) into a variable" 
         (progn
           (require 'calc-store)
           (call-interactively 'calc-store))
         :keys "s s"]
        ["Recall a variable value" 
          (progn
            (require 'calc-store)
            (call-interactively 'calc-recall ))
         :keys "s r"]
        ["Edit the value of a variable" 
         (progn
           (require 'calc-store)
           (call-interactively 'calc-edit-variable))
         :keys "s e"]
        ["Exchange (1:) with a variable value" 
         (progn
           (require 'calc-store)
           (call-interactively 'calc-store-exchange))
         :keys "s x"]
        ["Clear variable value" 
         (progn
           (require 'calc-store)
           (call-interactively 'calc-unstore))
         :keys "s u"]
        ["Evaluate variables in (1:)" 
         (progn
           (require 'calc-ext)
           (call-interactively 'calc-evaluate))
         :keys "="]
        ["Evaluate (1:), assigning a value to a variable" 
         (progn
           (require 'calc-store)
           (call-interactively 'calc-let))
         :keys "s l"
         :help "Evaluate (1:) under a temporary assignment of a variable"]
        "----"
        ["Help on Variables"
         (calc-info-goto-node "Store and Recall")])
  "Menu for Calc's variables.")

(defvar calc-stack-menu
  (list "Stack"
        ["Remove (1:)" 
         calc-pop 
         :keys "DEL"]
        ["Switch (1:) and (2:)" 
         calc-roll-down 
         :keys "TAB"]
        ["Duplicate (1:)" 
         calc-enter 
         :keys "RET"]
        ["Edit (1:)" 
         (progn
           (require 'calc-yank)
           (call-interactively calc-edit))
         :keys "`"]
        "----"
        ["Help on Stack"
         (calc-info-goto-node "Stack and Trail")])
  "Menu for Calc's stack functions.")

(defvar calc-errors-menu
  (list "Undo"
        ["Undo" 
         (progn
           (require 'calc-undo)
           (call-interactively 'calc-undo))
         :keys "U"]
        ["Redo" 
         (progn
           (require 'calc-undo)
           (call-interactively 'calc-redo))
         :keys "D"]
        "----"
        ["Help on Undo"
         (progn
           (calc-info-goto-node "Introduction")
           (Info-goto-node "Undo"))]))

(defvar calc-modes-menu
  (list "Modes"
        ["Precision" 
         (progn
           (require 'calc-ext)
           (call-interactively 'calc-precision))
         :keys "p"
         :help "Set the precision for floating point calculations"]
        ["Fraction mode"
         (progn
           (require 'calc-frac)
           (call-interactively 'calc-frac-mode))
         :keys "m f"
         :style toggle
         :selected calc-prefer-frac
         :help "Leave integer quotients as fractions"]
        ["Symbolic mode"
         (lambda ()
           (interactive)
           (require 'calc-mode)
           (calc-symbolic-mode nil))
         :keys "m s"
         :style toggle
         :selected calc-symbolic-mode
         :help "Leave functions producing inexact answers in symbolic form"]
        ["Infinite mode"
         (lambda ()
           (interactive)
           (require 'calc-mode)
           (calc-infinite-mode nil))
         :keys "m i"
         :style toggle
         :selected calc-infinite-mode
         :help "Let expressions like 1/0 produce infinite results"]
        ["Abbreviate long vectors"
         (progn
           (require 'calc-mode)
           (call-interactively 'calc-full-vectors))
         :keys "v ."
         :style toggle
         :selected (not calc-full-vectors)]
        (list "Angle measure"
              ["Radians"
               (progn
                 (require 'calc-math)
                 (call-interactively 'calc-radians-mode))
               :keys "m r"
               :style radio
               :selected (eq calc-angle-mode 'rad)]
              ["Degrees"
               (progn
                 (require 'calc-math)
                 (call-interactively 'calc-degrees-mode))
               :keys "m d"
               :style radio
               :selected (eq calc-angle-mode 'deg)]
              ["HMS"
               (progn
                 (require 'calc-math)
                 (call-interactively 'calc-hms-mode))
               :keys "m h"
               :style radio
               :selected (eq calc-angle-mode 'hms)])
        (list "Radix"
              ["Decimal"
               (progn
                 (require 'calc-bin)
                 (call-interactively 'calc-decimal-radix))
               :keys "d 0"
               :style radio
               :selected (= calc-number-radix 10)]
              ["Binary"
               (progn
                 (require 'calc-bin)
                 (call-interactively 'calc-binary-radix))
               :keys "d 2"
               :style radio
               :selected (= calc-number-radix 2)]
              ["Octal"
               (progn
                 (require 'calc-bin)
                 (call-interactively 'calc-octal-radix))
               :keys "d 8"
               :style radio
               :selected (= calc-number-radix 8)]
              ["Hexadecimal"
               (progn
                 (require 'calc-bin)
                 (call-interactively 'calc-hex-radix))
               :keys "d 6"
               :style radio
               :selected (= calc-number-radix 16)]
              ["Other"
               (progn
                 (require 'calc-bin)
                 (call-interactively 'calc-radix))
               :keys "d r"
               :style radio
               :selected (not
                          (or
                           (= calc-number-radix 10)
                           (= calc-number-radix 2)
                           (= calc-number-radix 8)
                           (= calc-number-radix 16)))])
        (list "Float format"
              ["Normal"
               (progn
                 (require 'calc-mode)
                 (call-interactively 'calc-normal-notation))
               :keys "d n"
               :style radio
               :selected (eq (car-safe calc-float-format) 'float)]
              ["Fixed point"
               (progn
                 (require 'calc-mode)
                 (call-interactively 'calc-fix-notation))
               :keys "d f"
               :style radio
               :selected (eq (car-safe calc-float-format) 'fix)]
              ["Scientific notation"
               (progn
                 (require 'calc-mode)
                 (call-interactively 'calc-sci-notation))
               :keys "d s"
               :style radio
               :selected (eq (car-safe calc-float-format) 'sci)]
              ["Engineering notation"
               (progn
                 (require 'calc-mode)
                 (call-interactively 'calc-eng-notation))
               :keys "d e"
               :style radio
               :selected (eq (car-safe calc-float-format) 'eng)])
        (list "Algebraic"
              ["Algebraic Mode"
               (progn
                 (require 'calc-mode)
                 (call-interactively 'calc-algebraic-mode))
               :keys "m a"
               :style radio
               :selected (eq calc-algebraic-mode t)
               :help "Keys which start numeric entry also start algebraic entry"]
              ["Total Algebraic Mode"
               (progn
                 (require 'calc-mode)
                 (call-interactively 'calc-total-algebraic-mode))
               :keys "m t"
               :style radio
               :selected (eq calc-algebraic-mode 'total)
               :help "All regular letters and punctuation begin algebraic entry"])
        "----"
        ["Save mode settings" calc-save-modes :keys "m m"]
        "----"
        ["Help on Modes"
         (calc-info-goto-node "Mode settings")])
  "Menu for Calc's mode settings.")

(defvar  calc-help-menu
  (list "Help"
        ["Manual"   
         calc-info
         :keys "h i"]
        ["Tutorial" 
         calc-tutorial 
         :keys "h t"]
        ["Summary"  
         calc-info-summary 
         :keys "h s"]
        "----"
        ["Help on Help" 
         (progn
           (calc-info-goto-node "Introduction")
           (Info-goto-node "Help Commands"))])
  "Menu for Calc's help functions.")

(easy-menu-define
  calc-menu
  calc-mode-map
  "Menu for Calc."
  (list "Calc"
        :visible '(eq major-mode 'calc-mode)
        calc-arithmetic-menu
        calc-scientific-function-menu
        calc-algebra-menu
        calc-graphics-menu
        calc-vectors-menu
        calc-units-menu
        calc-variables-menu
        calc-stack-menu
        calc-errors-menu
        calc-modes-menu
        calc-help-menu
        ["Reset"  
         (progn
           (require 'calc-ext)
           (call-interactively 'calc-reset))
         :help "Reset Calc to its initial state"]
        ["Quit" calc-quit]))

(provide 'calc-menu)
