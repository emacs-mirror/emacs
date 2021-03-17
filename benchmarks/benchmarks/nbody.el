;;; benchmarks/nbody.el ---   -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2021 Free Software Foundation, Inc.

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

;;; Commentary:

;; Adapted to elisp from CL version from:
;; https://benchmarksgame-team.pages.debian.net/benchmarksgame/program/nbody-sbcl-2.html

(require 'cl-lib)

(defconst elb-days-per-year 365.24)
(defconst elb-solar-mass (* 4 float-pi float-pi))

(cl-defstruct (elb-body (:type vector)
			(:conc-name nil)
			(:constructor make-elb-body (x y z vx vy vz mass)))
  x y z
  vx vy vz
  mass)

(defvar elb-jupiter
  (make-elb-body 4.84143144246472090
		 -1.16032004402742839
		 -1.03622044471123109e-1
		 (* 1.66007664274403694e-3 elb-days-per-year)
		 (* 7.69901118419740425e-3 elb-days-per-year)
		 (* -6.90460016972063023e-5  elb-days-per-year)
		 (* 9.54791938424326609e-4 elb-solar-mass)))

(defvar elb-saturn
  (make-elb-body 8.34336671824457987
		 4.12479856412430479
		 -4.03523417114321381e-1
		 (* -2.76742510726862411e-3 elb-days-per-year)
		 (* 4.99852801234917238e-3 elb-days-per-year)
		 (* 2.30417297573763929e-5 elb-days-per-year)
		 (* 2.85885980666130812e-4 elb-solar-mass)))

(defvar elb-uranus
  (make-elb-body 1.28943695621391310e1
		 -1.51111514016986312e1
		 -2.23307578892655734e-1
		 (* 2.96460137564761618e-03 elb-days-per-year)
		 (* 2.37847173959480950e-03 elb-days-per-year)
		 (* -2.96589568540237556e-05 elb-days-per-year)
		 (* 4.36624404335156298e-05 elb-solar-mass)))

(defvar elb-neptune
  (make-elb-body 1.53796971148509165e+01
		 -2.59193146099879641e+01
		 1.79258772950371181e-01
		 (* 2.68067772490389322e-03 elb-days-per-year)
		 (* 1.62824170038242295e-03 elb-days-per-year)
		 (* -9.51592254519715870e-05 elb-days-per-year)
		 (* 5.15138902046611451e-05 elb-solar-mass)))

(defvar elb-sun
  (make-elb-body 0.0 0.0 0.0 0.0 0.0 0.0 elb-solar-mass))

(defun elb-applyforces (a b dt)
  (let* ((dx (- (x a) (x b)))
	 (dy (- (y a) (y b)))
	 (dz (- (z a) (z b)))
	 (distance (sqrt (+ (* dx dx) (* dy dy) (* dz dz))))
	 (mag (/ dt (* distance distance distance)))
	 (dxmag (* dx mag))
	 (dymag (* dy mag))
	 (dzmag (* dz mag)))
    (cl-decf (vx a) (* dxmag (mass b)))
    (cl-decf (vy a) (* dymag (mass b)))
    (cl-decf (vz a) (* dzmag (mass b)))
    (cl-incf (vx b) (* dxmag (mass a)))
    (cl-incf (vy b) (* dymag (mass a)))
    (cl-incf (vz b) (* dzmag (mass a))))
  nil)

(defun elb-advance (system dt)
  (cl-loop for (a . rest) on system
	   do (dolist (b rest)
		(elb-applyforces a b dt)))
  (dolist (b system)
    (cl-incf (x b) (* dt (vx b)))
    (cl-incf (y b) (* dt (vy b)))
    (cl-incf (z b) (* dt (vz b))))
  nil)

(defun elb-energy (system)
  (let ((e 0.0))
    (cl-loop for (a . rest) on system do
	     (cl-incf e (* 0.5
			   (mass a)
			   (+ (* (vx a) (vx a))
			      (* (vy a) (vy a))
			      (* (vz a) (vz a)))))
	     (dolist (b rest)
	       (let* ((dx (- (x a) (x b)))
		      (dy (- (y a) (y b)))
		      (dz (- (z a) (z b)))
		      (dist (sqrt (+ (* dx dx) (* dy dy) (* dz dz)))))
		 (cl-decf e (/ (* (mass a) (mass b)) dist)))))
    e))

(defun elb-offset-momentum (system)
  (let ((px 0.0)
	(py 0.0)
	(pz 0.0))
    (dolist (p system)
      (cl-incf px (* (vx p) (mass p)))
      (cl-incf py (* (vy p) (mass p)))
      (cl-incf pz (* (vz p) (mass p))))
    (setf (vx (car system)) (/ (- px) elb-solar-mass)
	  (vy (car system)) (/ (- py) elb-solar-mass)
	  (vz (car system)) (/ (- pz) elb-solar-mass))
    nil))

(defun elb-nbody (n)
  (let ((system (list elb-sun elb-jupiter elb-saturn elb-uranus elb-neptune)))
    (elb-offset-momentum system)
    (dotimes (_ n)
      (elb-advance system 0.01))
    (elb-energy system)))

(defun elb-nbody-entry ()
  (elb-nbody 300000))

(provide 'benchmarks/nbody)
;;; benchmarks/nbody.el ends here
