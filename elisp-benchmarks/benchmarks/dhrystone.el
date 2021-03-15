;; -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Free Software Foundation, Inc.

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

;; Porting to elisp of the famous Dhrystone benchmark
;;
;; Adapted from C version:
;; https://github.com/Keith-S-Thompson/dhrystone/blob/master/v2.2/dry.c

(require 'cl-lib)

(cl-defstruct dhry-record
  discr
  variant)

(cl-defstruct dhry-var-1
  enum-comp
  int-comp
  str-comp)

(cl-defstruct dhry-var-2
  e-comp-2
  str-2-comp)

(cl-defstruct dhry-var-3
  ch-1-comp
  ch-2-comp)

(defvar dhry-ptr-glob)
(defvar dhry-next-ptr-glob)
(defvar dhry-int-glob)
(defvar dhry-bool-glob)
(defvar dhry-ch-1-glob)
(defvar dhry-ch-2-glob)
(defvar dhry-arr-1-glob)
(defvar dhry-arr-2-glob)

(defun dhry-structassign (dst src)
  (setf (cdr dst) (cdr src))
  (let ((src-record (car src))
	(dst-record (car dst)))
    (setf (dhry-record-discr dst-record)
	  (dhry-record-discr src-record))
    (let ((type (dhry-record-discr src-record))
	  (src-variant (dhry-record-variant src-record))
	  (dst-variant (dhry-record-variant dst-record)))
      (cl-case type
	(0
	 (setf (dhry-var-1-int-comp dst-variant)
	       (dhry-var-1-int-comp src-variant))
	 (setf (dhry-var-1-enum-comp dst-variant)
	       (dhry-var-1-enum-comp src-variant))
	 (store-substring (dhry-var-1-str-comp dst-variant)
			  0
			  (dhry-var-1-str-comp src-variant)))
	(1
	 (setf (dhry-var-2-e-comp-2 dst-variant)
	       (dhry-var-2-e-comp-2 src-variant))
	 (store-substring (dhry-var-2-str-2-comp dst-variant)
			  0
			  (dhry-var-2-str-2-comp src-variant)))
	(2
	 (setf (dhry-var-3-ch-1-comp dst-variant)
	       (dhry-var-3-ch-1-comp src-variant))
	 (setf (dhry-var-3-ch-2-comp dst-variant)
	       (dhry-var-3-ch-2-comp src-variant)))))))

(defun dhry-proc-1 (ptr-val-par)
  (let ((next-record (cdr ptr-val-par)))
    (dhry-structassign (cdr ptr-val-par) dhry-ptr-glob)
    (setf (dhry-var-1-int-comp (dhry-record-variant (car ptr-val-par))) 5)
    (setf (dhry-var-1-int-comp (dhry-record-variant (car next-record)))
	  (dhry-var-1-int-comp (dhry-record-variant (car ptr-val-par))))
    (setf (cdr next-record) (dhry-proc-3 (cdr next-record)))
    (if (= (dhry-record-discr (car next-record)) 0)
	(progn
	  (setf (dhry-var-1-int-comp (dhry-record-variant (car next-record))) 6)
	  (setf (dhry-var-1-enum-comp (dhry-record-variant (car next-record)))
		(dhry-proc-6 (dhry-var-1-enum-comp (dhry-record-variant (car ptr-val-par)))))
	  (setf (cdr next-record) (cdr dhry-ptr-glob))
	  (setf (dhry-var-1-int-comp (dhry-record-variant (car next-record)))
		(dhry-proc-7 (dhry-var-1-int-comp (dhry-record-variant (car next-record))) 10)))
      (dhry-structassign ptr-val-par (cdr ptr-val-par)))))

(defun dhry-proc-2 (int-par-ref)
  (let (int-loc enum-loc)
    (setq int-loc (+ int-par-ref 10))
    (cl-loop when (= dhry-ch-1-glob ?A)
	     do (cl-decf int-loc)
	        (setq int-par-ref (- int-loc dhry-int-glob))
	        (setq enum-loc 0)
	     while (/= enum-loc 0))
    int-par-ref))

(defun dhry-proc-3 (ptr-ref-par)
  (let ((ret ptr-ref-par))
    (when dhry-ptr-glob
      (setq ret (cdr dhry-ptr-glob)))
    (setf (dhry-var-1-int-comp (dhry-record-variant (car dhry-ptr-glob))) (dhry-proc-7 10 dhry-int-glob))
    ret))

(defun dhry-proc-4 ()
  (let (bool-loc)
    (setq bool-loc (= dhry-ch-1-glob ?A))
    (setq dhry-bool-glob (or bool-loc dhry-bool-glob))
    (setq dhry-ch-2-glob ?B)))

(defun dhry-proc-5 ()
  (setq dhry-ch-1-glob ?A)
  (setq dhry-bool-glob nil))

(defun dhry-proc-6 (enum-val-par)
  (let (enum-ref-par)
    (setq enum-ref-par enum-val-par)
    (unless (dhry-func-3 enum-val-par)
      (setq enum-ref-par 3))
    (cl-case enum-val-par
      (0
       (setq enum-ref-par 0))
      (1
       (if (> dhry-int-glob 100)
	   (setq enum-ref-par 0)
	 (setq enum-ref-par 3)))
      (2
       (setq enum-ref-par 1))
      (3
       nil)
      (4
       (setq enum-ref-par 2)))
    enum-ref-par))

(defun dhry-proc-7 (int-1-par-val int-2-par-val)
  (let (int-loc)
    (setq int-loc (+ int-1-par-val 2))
    (+ int-2-par-val int-loc)))

(defun dhry-proc-8 (arr-1-par-ref arr-2-par-ref int-1-par-val int-2-par-val)
  (let (int-loc)
    (setq int-loc (+ int-1-par-val 5))
    (setf (aref arr-1-par-ref int-loc) int-2-par-val)
    (setf (aref arr-1-par-ref (1+ int-loc)) (aref arr-1-par-ref int-loc))
    (setf (aref arr-1-par-ref (+ int-loc 30)) int-loc)
    (cl-loop for int-index from int-loc to (1+ int-loc)
	     do (setf (aref (aref arr-2-par-ref int-loc) int-index) int-loc))
    (cl-incf (aref (aref arr-2-par-ref int-loc) (1- int-loc)))
    (setf (aref (aref arr-2-par-ref (+ int-loc 20)) int-loc) (aref arr-1-par-ref int-loc))
    (setq dhry-int-glob 5)))

(defun dhry-func-1 (ch-1-par-val ch-2-par-val)
  (let (ch-1-loc ch-2-loc)
    (setq ch-1-loc ch-1-par-val)
    (setq ch-2-loc ch-1-loc)
    (if (/= ch-2-loc ch-2-par-val)
	0
      (setq dhry-ch-1-glob ch-1-loc)
      1)))

(defun dhry-func-2 (str-1-par-ref str-2-par-ref)
  (let (int-loc ch-loc)
    (setq int-loc 2)
    (while (<= int-loc 2)
      (if (= (dhry-func-1 (aref str-1-par-ref int-loc)
			  (aref str-2-par-ref (1+ int-loc)))
	     0)
	  (progn
	    (setq ch-loc ?A)
	    (cl-incf int-loc))))
    (if (and (>= ch-loc ?W) (< ch-loc ?Z))
	(setq int-loc 7))
    (if (= ch-loc ?R)
	t
      (if (string> str-1-par-ref str-2-par-ref)
	  (progn
	    (cl-incf int-loc 7)
	    (setq dhry-int-glob int-loc)
	    t)
	nil))))

(defun dhry-func-3 (enum-par-val)
  (let (enum-loc)
    (setq enum-loc enum-par-val)
    (if (= enum-loc 2)
	t
      nil)))

(defun dhrystone (number-of-runs &optional check)
  (let (int-1-loc
	int-2-loc
	int-3-loc
	enum-loc
	(str-1-loc (make-string 30 0))
	(str-2-loc (make-string 30 0)))
    ;; initialization (pre-allocate to avoid consing in the loop)
    (setq dhry-ptr-glob (list (make-dhry-record) (make-dhry-record)))
    (setf (dhry-record-discr (car dhry-ptr-glob)) 0)
    (setf (dhry-record-variant (car dhry-ptr-glob))
	  (make-dhry-var-1
	   :enum-comp 2
	   :int-comp 40
	   :str-comp "DHRYSTONE PROGRAM, SOME STRING"))
    (setf (dhry-record-variant (cadr dhry-ptr-glob))
	  (make-dhry-var-1
	   :str-comp (make-string 30 0)))
    (setq dhry-int-glob 0)
    (setq dhry-bool-glob nil)
    (setq dhry-ch-1-glob 0)
    (setq dhry-ch-2-glob 0)
    (setq dhry-arr-1-glob (make-vector 50 0))
    (setq dhry-arr-2-glob (make-vector 50 0))
    (dotimes (i 50)
      (setf (aref dhry-arr-2-glob i) (make-vector 50 0)))
    (setf (aref (aref dhry-arr-2-glob 8) 7) 10)
    (store-substring str-1-loc 0 "DHRYSTONE PROGRAM, 1'ST STRING")
    ;; dhrystone loop
    (dotimes (run-index number-of-runs)
      (dhry-proc-5)
      (dhry-proc-4)
      (setq int-1-loc 2)
      (setq int-2-loc 3)
      (store-substring str-2-loc 0 "DHRYSTONE PROGRAM, 2'ND STRING")
      (setq enum-loc 1)
      (setq dhry-bool-glob (not (dhry-func-2 str-1-loc str-2-loc)))
      (while (< int-1-loc int-2-loc)
	(setq int-3-loc (- (* 5 int-1-loc) int-2-loc))
	(setq int-3-loc (dhry-proc-7 int-1-loc int-2-loc))
	(cl-incf int-1-loc))
      (dhry-proc-8 dhry-arr-1-glob dhry-arr-2-glob int-1-loc int-3-loc)
      (dhry-proc-1 dhry-ptr-glob)
      (cl-loop for ch-index from ?A to dhry-ch-2-glob
               when (= enum-loc (dhry-func-1 ch-index ?C))
	       do (setq enum-loc (dhry-proc-6 0))
	          (store-substring str-2-loc 0 "DHRYSTONE PROGRAM, 3'RD STRING")
	          (setq int-2-loc run-index)
	          (setq dhry-int-glob run-index))
      (setq int-2-loc (* int-2-loc int-1-loc))
      (setq int-1-loc (/ int-2-loc int-3-loc))
      (setq int-2-loc (- (* 7 (- int-2-loc int-3-loc)) int-1-loc))
      (setq int-1-loc (dhry-proc-2 int-1-loc)))
    ;; check results
    (when check
      (cl-flet ((result-compare (name val ref)
		  (unless (equal val ref)
		    (error "%s: %s, expected: %s" name val ref))))
	(result-compare "Int_Glob"
			dhry-int-glob 5)
	(result-compare "Bool_Glob"
			dhry-bool-glob t)
	(result-compare "Ch_1_Glob"
			dhry-ch-1-glob ?A)
	(result-compare "Ch_2_Glob"
			dhry-ch-2-glob ?B)
	(result-compare "Arr_1_Glob[8]"
			(aref dhry-arr-1-glob 8) 7)
	(result-compare "Arr_2_Glob[8][7]"
			(aref (aref dhry-arr-2-glob 8) 7) (+ number-of-runs 10))
	(result-compare "Ptr_Glob->Discr"
			(dhry-record-discr (car dhry-ptr-glob)) 0)
	(result-compare "Ptr_Glob->var_1->Enum_Comp"
			(dhry-var-1-enum-comp (dhry-record-variant (car dhry-ptr-glob))) 2)
	(result-compare "Ptr-Glob->var_1->Int_Comp"
			(dhry-var-1-int-comp (dhry-record-variant (car dhry-ptr-glob))) 17)
	(result-compare "Ptr_Glob->var_1->Str_Comp"
			(dhry-var-1-str-comp (dhry-record-variant (car dhry-ptr-glob))) "DHRYSTONE PROGRAM, SOME STRING")
	(result-compare "Next_Ptr_Glob->Discr"
			(dhry-record-discr (cadr dhry-ptr-glob)) 0)
	(result-compare "Next_Ptr_Glob->var_1->Enum_Comp"
			(dhry-var-1-enum-comp (dhry-record-variant (cadr dhry-ptr-glob))) 1)
	(result-compare "Next_Ptr_Glob->var_1->Int_Comp"
			(dhry-var-1-int-comp (dhry-record-variant (cadr dhry-ptr-glob))) 18)
	(result-compare "Next_Ptr_Glob->var_1->Str_Comp"
			(dhry-var-1-str-comp (dhry-record-variant (cadr dhry-ptr-glob))) "DHRYSTONE PROGRAM, SOME STRING")
	(result-compare "Int_1_Loc"
			int-1-loc 5)
	(result-compare "Int_2_Loc"
			int-2-loc 13)
	(result-compare "Int_3_Loc"
			int-3-loc 7)
	(result-compare "Enum_Loc"
			enum-loc 1)
	(result-compare "Str_1_Loc"
			str-1-loc "DHRYSTONE PROGRAM, 1'ST STRING")
	(result-compare "Str_2_Loc"
			str-2-loc "DHRYSTONE PROGRAM, 2'ND STRING")))))

(defun elb-dhrystone-entry ()
  (dhrystone 1000000))

(provide 'elb-dhrystone)

;; Local Variables:
;; comp-speed: 3
;; End:
