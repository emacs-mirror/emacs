;;; floatfns-tests.el --- tests for floating point operations

;; Copyright 2017-2020 Free Software Foundation, Inc.

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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

(require 'ert)

(ert-deftest divide-extreme-sign ()
  (should (= (ceiling most-negative-fixnum -1.0) (- most-negative-fixnum)))
  (should (= (floor most-negative-fixnum -1.0) (- most-negative-fixnum)))
  (should (= (round most-negative-fixnum -1.0) (- most-negative-fixnum)))
  (should (= (truncate most-negative-fixnum -1.0) (- most-negative-fixnum))))

(ert-deftest logb-extreme-fixnum ()
  (should (= (logb most-negative-fixnum) (1+ (logb most-positive-fixnum)))))

(ert-deftest fround-fixnum ()
  (should-error (ffloor 0) :type 'wrong-type-argument)
  (should-error (fceiling 0) :type 'wrong-type-argument)
  (should-error (ftruncate 0) :type 'wrong-type-argument)
  (should-error (fround 0) :type 'wrong-type-argument))

(ert-deftest bignum-to-float ()
  ;; 122 because we want to go as big as possible to provoke a rounding error,
  ;; but not too big: 2**122 < 10**37 < 2**123, and the C standard says
  ;; 10**37 <= DBL_MAX so 2**122 cannot overflow as a double.
  (let ((a (1- (ash 1 122))))
    (should (or (eql a (1- (floor (float a))))
                (eql a (floor (float a))))))
  (should (eql (float (+ most-positive-fixnum 1))
               (+ (float most-positive-fixnum) 1))))

(ert-deftest bignum-abs ()
  (should (= most-positive-fixnum
             (- (abs most-negative-fixnum) 1))))

(ert-deftest bignum-expt ()
  (dolist (n (list most-positive-fixnum (1+ most-positive-fixnum)
                   most-negative-fixnum (1- most-negative-fixnum)
                   (* 5 most-negative-fixnum)
                   (* 5 (1+ most-positive-fixnum))
                   -2 -1 0 1 2))
    (should (or (<= n 0) (= (expt 0 n) 0)))
    (should (= (expt 1 n) 1))
    (should (or (< n 0) (= (expt -1 n) (if (zerop (logand n 1)) 1 -1))))
    (should (= (expt n 0) 1))
    (should (= (expt n 1) n))
    (should (= (expt n 2) (* n n)))
    (should (= (expt n 3) (* n n n)))))

(ert-deftest bignum-logb ()
  (should (= (+ (logb most-positive-fixnum) 1)
             (logb (+ most-positive-fixnum 1)))))

(ert-deftest bignum-mod ()
  (should (= 0 (mod (1+ most-positive-fixnum) 2.0))))

(ert-deftest bignum-round ()
  (let ((ns (list (* most-positive-fixnum most-negative-fixnum)
                  (1- most-negative-fixnum) most-negative-fixnum
                  (1+ most-negative-fixnum) -2 1 1 2
                  (1- most-positive-fixnum) most-positive-fixnum
                  (1+ most-positive-fixnum)
                  (* most-positive-fixnum most-positive-fixnum))))
    (dolist (n ns)
      (should (= n (ceiling n)))
      (should (= n (floor n)))
      (should (= n (round n)))
      (should (= n (truncate n)))
      (let ((-n (- n))
	    (f (float n))
	    (-f (- (float n))))
	(should (= 1 (round n f) (round -n -f) (round f n) (round -f -n)))
	(should (= -1 (round -n f) (round n -f) (round f -n) (round -f n))))
      (dolist (d ns)
        (let ((q (/ n d))
              (r (% n d))
              (same-sign (eq (< n 0) (< d 0))))
          (should (= (ceiling n d)
                     (+ q (if (and same-sign (not (zerop r))) 1 0))))
          (should (= (floor n d)
                     (- q (if (and (not same-sign) (not (zerop r))) 1 0))))
          (should (= (truncate n d) q))
          (let ((cdelta (abs (- n (* d (ceiling n d)))))
                (fdelta (abs (- n (* d (floor n d)))))
                (rdelta (abs (- n (* d (round n d))))))
            (should (<= rdelta cdelta))
            (should (<= rdelta fdelta))
            (should (if (zerop r)
                        (= 0 cdelta fdelta rdelta)
                      (or (/= cdelta fdelta)
                          (zerop (% (round n d) 2)))))))))))

(ert-deftest special-round ()
  (dolist (f '(ceiling floor round truncate))
    (let ((ns '(-1e+INF 1e+INF -1 -0.0 0.0 0 1 -1e+NaN 1e+NaN)))
      (dolist (n ns)
	(if (not (<= (abs n) 1))
	    (should-error (funcall f n))
	  (should (= n (funcall f n)))
	  (dolist (d '(-1e+INF 1e+INF))
	    (should (eq 0 (funcall f n d)))))
	(dolist (d ns)
	  (when (or (zerop d) (= (abs n) 1e+INF) (not (= n n)) (not (= d d)))
	    (should-error (funcall f n d))))))))

(ert-deftest big-round ()
  (should (= (floor 54043195528445955 3)
             (floor 54043195528445955 3.0)))
  (should (= (floor 1.7976931348623157e+308 5e-324)
             (ash (1- (ash 1 53)) 2045))))

(provide 'floatfns-tests)
