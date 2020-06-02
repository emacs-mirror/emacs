;;; buffer-tests.el --- tests for buffer.c functions -*- lexical-binding: t -*-

;; Copyright (C) 2015-2020 Free Software Foundation, Inc.

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

;;; Code:

(require 'ert)
(require 'seq)
(eval-when-compile (require 'cl-lib))

(ert-deftest overlay-modification-hooks-message-other-buf ()
  "Test for bug#21824.
After a modification-hook has been run and there is an overlay in
the *Messages* buffer, the message coalescing [2 times] wrongly
runs the modification-hook of the overlay in the 1st buffer, but
with parameters from the *Messages* buffer modification."
  (let ((buf nil)
        (msg-ov nil))
    (with-temp-buffer
      (insert "123")
      (overlay-put (make-overlay 1 3)
                   'modification-hooks
                   (list (lambda (&rest _)
                           (setq buf (current-buffer)))))
      (goto-char 2)
      (insert "x")
      (unwind-protect
          (progn
            (setq msg-ov (make-overlay 1 1 (get-buffer-create "*Messages*")))
            (message "a message")
            (message "a message")
            (should (eq buf (current-buffer))))
        (when msg-ov (delete-overlay msg-ov))))))

(ert-deftest overlay-modification-hooks-deleted-overlay ()
  "Test for bug#30823."
  (let ((check-point nil)
	(ov-delete nil)
	(ov-set nil))
    (with-temp-buffer
      (insert "abc")
      (setq ov-set (make-overlay 1 3))
      (overlay-put ov-set 'modification-hooks
		   (list (lambda (_o after &rest _args)
			   (and after (setq check-point t)))))
      (setq ov-delete (make-overlay 1 3))
      (overlay-put ov-delete 'modification-hooks
		   (list (lambda (o after &rest _args)
			   (and (not after) (delete-overlay o)))))
      (goto-char 2)
      (insert "1")
      (should (eq check-point t)))))

(ert-deftest test-generate-new-buffer-name-bug27966 ()
  (should-not (string-equal "nil"
                            (progn (get-buffer-create "nil")
                                   (generate-new-buffer-name "nil")))))

(ert-deftest test-buffer-base-buffer-indirect ()
  (with-temp-buffer
    (let* ((ind-buf-name (generate-new-buffer-name "indbuf"))
           (ind-buf (make-indirect-buffer (current-buffer) ind-buf-name)))
      (should (eq (buffer-base-buffer ind-buf) (current-buffer))))))

(ert-deftest test-buffer-base-buffer-non-indirect ()
  (with-temp-buffer
    (should (eq (buffer-base-buffer (current-buffer)) nil))))

(ert-deftest overlay-evaporation-after-killed-buffer ()
  (let* ((ols (with-temp-buffer
                (insert "toto")
                (list
                 (make-overlay (point-min) (point-max))
                 (make-overlay (point-min) (point-max))
                 (make-overlay (point-min) (point-max)))))
         (ol (nth 1 ols)))
    (overlay-put ol 'evaporate t)
    ;; Evaporation within move-overlay of an overlay that was deleted because
    ;; of a kill-buffer, triggered an assertion failure in unchain_both.
    (with-temp-buffer
      (insert "toto")
      (move-overlay ol (point-min) (point-min)))))


;; +==========================================================================+
;; | Overlay test setup
;; +==========================================================================+

(eval-when-compile
  (defun buffer-tests--make-test-name (fn x y)
    (intern (format "buffer-tests--%s-%s-%s" fn x y))))

(defun buffer-tests--unmake-test-name (symbol)
  (let ((name (if (stringp symbol) symbol (symbol-name symbol))))
    (when (string-match "\\`buffer-tests--\\(.*\\)-\\(.*\\)-\\(.*\\)\\'" name)
      (list (match-string 1 name)
            (match-string 2 name)
            (match-string 3 name)))))

(defmacro deftest-make-overlay-1 (id args)
  (declare (indent 1))
  `(ert-deftest ,(buffer-tests--make-test-name 'make-overlay 1 id) ()
     (with-temp-buffer
       (should ,(cons 'make-overlay args)))))

(defmacro deftest-make-overlay-2 (id args condition)
  (declare (indent 1))
  `(ert-deftest ,(buffer-tests--make-test-name 'make-overlay 2 id) ()
     (with-temp-buffer
       (should-error
        ,(cons 'make-overlay args)
        :type ',condition
        :exclude-subtypes t))))

(defmacro deftest-overlay-start/end-1 (id start-end-args start-end-should)
  (declare (indent 1))
  (cl-destructuring-bind (start end sstart send)
      (append start-end-args start-end-should)
    `(ert-deftest ,(buffer-tests--make-test-name 'overlay-start/end 1 id) ()
       (with-temp-buffer
         (insert (make-string 9 ?\n))
         (let ((ov (make-overlay ,start ,end)))
           (should (equal ,sstart (overlay-start ov)))
           (should (equal ,send (overlay-end ov))))))))

(defmacro deftest-overlay-buffer-1 (id arg-expr should-expr)
  (declare (indent 1))
  `(ert-deftest ,(buffer-tests--make-test-name 'overlay-buffer 1 id) ()
     (with-temp-buffer
       (should (equal (overlay-buffer (make-overlay 1 1 ,arg-expr))
                      ,should-expr)))))

(defmacro deftest-overlayp-1 (id arg-expr should-expr)
  (declare (indent 1))
  `(ert-deftest ,(buffer-tests--make-test-name 'overlay-buffer 1 id) ()
     (with-temp-buffer
       (should (equal ,should-expr (overlayp ,arg-expr))))))

(defmacro deftest-next-overlay-change-1 (id pos result &rest ov-tuple)
  `(ert-deftest ,(buffer-tests--make-test-name 'next-overlay-change 1 id) ()
     (let ((tuple (copy-sequence ',ov-tuple)))
       (with-temp-buffer
         (insert (make-string (max 100 (if tuple
                                           (apply #'max
                                                  (mapcar
                                                   (lambda (m) (apply #'max m))
                                                   tuple))
                                         0))
                              ?\n))
         (dolist (tup tuple)
           (make-overlay (car tup) (cadr tup)))
         (should (equal (next-overlay-change ,pos)
                        ,result))))))

(defmacro deftest-previous-overlay-change-1 (id pos result &rest ov-tuple)
  `(ert-deftest ,(buffer-tests--make-test-name 'previous-overlay-change 1 id) ()
     (let ((tuple ',ov-tuple))
       (with-temp-buffer
         (insert (make-string (max 100 (if tuple
                                           (apply #'max
                                                  (mapcar
                                                   (lambda (m) (apply #'max m))
                                                   tuple))
                                         0))
                              ?\n))
         (dolist (tup tuple)
           (make-overlay (car tup) (cadr tup)))
         (should (equal (previous-overlay-change ,pos)
                        ,result))))))

(defmacro deftest-overlays-at-1 (id pos result &rest ov-triple)
  `(ert-deftest ,(buffer-tests--make-test-name 'overlays-at 1 id) ()
     (let ((pos* ,pos))
       (with-temp-buffer
         (insert (make-string 100 ?\s))
         (should-not (memq nil ',result))
         (dolist (v ',ov-triple)
           (cl-destructuring-bind (tag start end)
               v
             (overlay-put (make-overlay start end) 'tag tag)))
         (let ((ovl (overlays-at pos*)))
           (should (equal (length ovl) (length ',result)))
           (dolist (ov ovl)
             (should (memq (overlay-get ov 'tag) ',result))))))))

(defmacro deftest-overlays-in-1 (id beg end result &rest ov-triple)
  `(ert-deftest ,(buffer-tests--make-test-name 'overlays-in 1 id) ()
     (let ((beg* ,beg)
           (end* ,end))
       (with-temp-buffer
         (insert (make-string 100 ?\s))
         (should-not (memq nil ',result))
         (dolist (v ',ov-triple)
           (cl-destructuring-bind (tag start end)
               v
             (overlay-put (make-overlay start end) 'tag tag)))
         (let ((ovl (overlays-in beg* end*)))
           (should (equal (length ovl) (length ',result)))
           (dolist (ov ovl)
             (should (memq (overlay-get ov 'tag) ',result))))))))

(defmacro test-with-overlay-in-buffer (symbol-beg-end-fa-ra &rest body)
  (declare (indent 1))
  (cl-destructuring-bind (symbol beg end &optional fa ra)
      symbol-beg-end-fa-ra
    `(with-temp-buffer
       (insert (make-string (max 1000 (1- ,end)) ?\s))
       (goto-char 1)
       (let ((,symbol (make-overlay ,beg ,end nil ,fa ,ra)))
         ,@body))))

(defmacro deftest-overlays-equal-1 (id result ov1-args ov2-args)
  `(ert-deftest ,(buffer-tests--make-test-name 'overlays-equal 1 id) ()
     (cl-flet ((create-overlay (args)
                 (cl-destructuring-bind (start end &optional fa ra
                                                   &rest properties)
                     args
                   (let ((ov (make-overlay start end nil fa ra)))
                     (while properties
                       (overlay-put ov (pop properties) (pop properties)))
                     ov))))
       (with-temp-buffer
         (insert (make-string 1024 ?\s))
         (should (,(if result 'identity 'not)
                  (equal (create-overlay ',ov1-args)
                         (create-overlay ',ov2-args))))))))


(defun buffer-tests--find-ert-test (name)
  (let ((test (buffer-tests--unmake-test-name name)))
    (or (and test
             (cl-destructuring-bind (fn x y)
                 test
               (let ((regexp (format "deftest-%s-%s +%s" fn x y)))
                 (re-search-forward regexp nil t))))
        (let ((find-function-regexp-alist
               (cl-remove #'buffer-tests--find-ert-test
                          find-function-regexp-alist :key #'cdr)))
          (find-function-do-it name 'ert-deftest
                               #'switch-to-buffer-other-window)))))

(add-to-list 'find-function-regexp-alist
             `(ert-deftest . ,#'buffer-tests--find-ert-test))


;; +==========================================================================+
;; | make-overlay
;; +==========================================================================+

;; Test if making an overlay succeeds.
(deftest-make-overlay-1 A (1 1))
(deftest-make-overlay-1 B (7 26))
(deftest-make-overlay-1 C (29 7))
(deftest-make-overlay-1 D (most-positive-fixnum 1))
(deftest-make-overlay-1 E (most-negative-fixnum 1))
(deftest-make-overlay-1 F (1 most-positive-fixnum))
(deftest-make-overlay-1 G (1 most-negative-fixnum))
(deftest-make-overlay-1 H (1 1 nil t))
(deftest-make-overlay-1 I (1 1 nil nil))
(deftest-make-overlay-1 J (1 1 nil nil nil))
(deftest-make-overlay-1 K (1 1 nil nil t))
(deftest-make-overlay-1 L (1 1 nil t t))
(deftest-make-overlay-1 M (1 1 nil "yes" "yes"))

;; Test if trying to make an overlay signals conditions.
(deftest-make-overlay-2 A ()            wrong-number-of-arguments)
(deftest-make-overlay-2 B (1)           wrong-number-of-arguments)
(deftest-make-overlay-2 C (1 2 3 4 5 6) wrong-number-of-arguments)
(deftest-make-overlay-2 D ("1")         wrong-number-of-arguments)
(deftest-make-overlay-2 E ("1" "2")     wrong-type-argument)
(deftest-make-overlay-2 F (1 2 "b")     wrong-type-argument)
(deftest-make-overlay-2 G (1 2 3.14)    wrong-type-argument)
(deftest-make-overlay-2 H (3.14 3)      wrong-type-argument)
(deftest-make-overlay-2 I (1 [1])       wrong-type-argument)
(deftest-make-overlay-2 J (1 1 (with-temp-buffer
                                 (current-buffer)))
  error)


;; +==========================================================================+
;; | overlay-start/end
;; +==========================================================================+

;; Test if the overlays return proper positions.  point-max of the
;; buffer will equal 10.        ARG RESULT
(deftest-overlay-start/end-1 A (1 1) (1 1))
(deftest-overlay-start/end-1 B (2 7) (2 7))
(deftest-overlay-start/end-1 C (7 2) (2 7))
(deftest-overlay-start/end-1 D (1 10) (1 10))
(deftest-overlay-start/end-1 E (1 11) (1 10))
(deftest-overlay-start/end-1 F (1 most-positive-fixnum) (1 10))
(deftest-overlay-start/end-1 G (most-positive-fixnum 1) (1 10))
(deftest-overlay-start/end-1 H (most-positive-fixnum most-positive-fixnum)
                               (10 10))
(deftest-overlay-start/end-1 I (100 11) (10 10))
(deftest-overlay-start/end-1 J (11 100) (10 10))
(deftest-overlay-start/end-1 K (0 1) (1 1))
(deftest-overlay-start/end-1 L (1 0) (1 1))
(deftest-overlay-start/end-1 M (0 0) (1 1))

(ert-deftest test-overlay-start/end-2 ()
  (should-not (overlay-start (with-temp-buffer (make-overlay 1 1))))
  (should-not (overlay-end (with-temp-buffer (make-overlay 1 1)))))


;; +==========================================================================+
;; | overlay-buffer
;; +==========================================================================+

;; Test if overlay-buffer returns appropriate values.
(deftest-overlay-buffer-1 A (current-buffer) (current-buffer))
(deftest-overlay-buffer-1 B nil (current-buffer))
(ert-deftest test-overlay-buffer-1-C ()
  (should-error (make-overlay
                 1 1 (with-temp-buffer (current-buffer)))))


;; +==========================================================================+
;; | overlayp
;; +==========================================================================+

;; Check the overlay predicate.
(deftest-overlayp-1 A (make-overlay 1 1) t)
(deftest-overlayp-1 B (with-temp-buffer (make-overlay 1 1)) t)
(deftest-overlayp-1 C nil                            nil)
(deftest-overlayp-1 D 'symbol                        nil)
(deftest-overlayp-1 E "string"                       nil)
(deftest-overlayp-1 F 42                             nil)
(deftest-overlayp-1 G [1 2]                          nil)
(deftest-overlayp-1 H (symbol-function 'car)         nil)
(deftest-overlayp-1 I float-pi                       nil)
(deftest-overlayp-1 J (cons 1 2)                     nil)
(deftest-overlayp-1 K (make-hash-table)              nil)
(deftest-overlayp-1 L (symbol-function 'ert-deftest) nil)
(deftest-overlayp-1 M (current-buffer)               nil)
(deftest-overlayp-1 N (selected-window)              nil)
(deftest-overlayp-1 O (selected-frame)               nil)


;; +==========================================================================+
;; | overlay equality
;; +==========================================================================+

(deftest-overlays-equal-1 A t (1 1) (1 1))
(deftest-overlays-equal-1 B t (5 10) (5 10))
(deftest-overlays-equal-1 C nil (5 11) (5 10))
(deftest-overlays-equal-1 D t (10 20 t) (10 20))
(deftest-overlays-equal-1 E t (10 20 nil t) (10 20))
(deftest-overlays-equal-1 F t (10 20 t t) (10 20 nil t))
(deftest-overlays-equal-1 G t (10 20 t t) (10 20 t nil))
(deftest-overlays-equal-1 H t (10 20 nil nil foo 42) (10 20 nil nil foo 42))
(deftest-overlays-equal-1 I nil (10 20 nil nil foo 42) (10 20 nil nil foo 43))


;; +==========================================================================+
;; | overlay-lists
;; +==========================================================================+

;; Check whether overlay-lists returns something sensible.
(ert-deftest test-overlay-lists-1 ()
  (with-temp-buffer
    (should (equal (cons nil nil) (overlay-lists)))
    (dotimes (i 10) (make-overlay 1 i))
    (should (listp (car (overlay-lists))))
    (should (listp (cdr (overlay-lists))))
    (let ((list (append (car (overlay-lists))
                        (cdr (overlay-lists)))))
      (should (= 10 (length list)))
      (should (seq-every-p #'overlayp list)))))


;; +==========================================================================+
;; | overlay-put/get/properties
;; +==========================================================================+

;; Test if overlay-put properties can be retrieved by overlay-get and
;; overlay-properties.
(ert-deftest test-overlay-props-1 ()
  (with-temp-buffer
    (let* ((keys '(:k1 :k2 :k3))
           (values '(1 "v2" v3))
           (ov (make-overlay 1 1))
           (n (length keys)))
      (should (equal (length keys) (length values)))
      (should (null (overlay-properties ov)))
      ;; Insert keys and values.
      (dotimes (i n)
        (should (equal (overlay-put ov (nth i keys) (nth i values))
                       (nth i values))))
      ;; Compare with what overlay-get says.
      (dotimes (i n)
        (should (equal (overlay-get ov (nth i keys))
                       (nth i values))))
      ;; Test if overlay-properties is a superset.
      (dotimes (i n)
        (should (equal (plist-get (overlay-properties ov)
                                  (nth i keys))
                       (nth i values))))
      ;; Check if overlay-properties is a subset.
      (should (= (length (overlay-properties ov)) (* n 2))))))


;; +==========================================================================+
;; | next-overlay-change
;; +==========================================================================+

;; Test if next-overlay-change returns RESULT if called with POS in a
;; buffer with overlays corresponding to OVS and point-max >= 100.
;;                               (POS RESULT &rest OVS)
;; 0 overlays
(deftest-next-overlay-change-1 A (point-min) (point-max))
(deftest-next-overlay-change-1 B (point-max) (point-max))
;; 1 non-empty overlay
(deftest-next-overlay-change-1 C 1 10 (10 20))
(deftest-next-overlay-change-1 D 10 20 (10 20))
(deftest-next-overlay-change-1 E 15 20 (10 20))
(deftest-next-overlay-change-1 F 20 (point-max) (10 20))
(deftest-next-overlay-change-1 G 30 (point-max) (10 20))
;; 1 empty overlay
(deftest-next-overlay-change-1 H 1 10 (10 10))
(deftest-next-overlay-change-1 I 10 (point-max) (10 10))
(deftest-next-overlay-change-1 J 20 (point-max) (10 10))
;; 2 non-empty, non-intersecting
(deftest-next-overlay-change-1 D 10 20 (20 30) (40 50))
(deftest-next-overlay-change-1 E 35 40 (20 30) (40 50))
(deftest-next-overlay-change-1 F 60 (point-max) (20 30) (40 50))
(deftest-next-overlay-change-1 G 30 40 (20 30) (40 50))
(deftest-next-overlay-change-1 H 50 (point-max) (20 30) (40 50))
;; 2 non-empty, intersecting
(deftest-next-overlay-change-1 I 10 20 (20 30) (25 35))
(deftest-next-overlay-change-1 J 20 25 (20 30) (25 35))
(deftest-next-overlay-change-1 K 23 25 (20 30) (25 35))
(deftest-next-overlay-change-1 L 25 30 (20 30) (25 35))
(deftest-next-overlay-change-1 M 28 30 (20 30) (25 35))
(deftest-next-overlay-change-1 N 30 35 (20 30) (25 35))
(deftest-next-overlay-change-1 O 35 (point-max) (20 30) (25 35))
(deftest-next-overlay-change-1 P 50 (point-max) (20 30) (25 35))
;; 2 non-empty, continuous
(deftest-next-overlay-change-1 Q 10 20 (20 30) (30 40))
(deftest-next-overlay-change-1 R 20 30 (20 30) (30 40))
(deftest-next-overlay-change-1 S 25 30 (20 30) (30 40))
(deftest-next-overlay-change-1 T 30 40 (20 30) (30 40))
(deftest-next-overlay-change-1 U 35 40 (20 30) (30 40))
(deftest-next-overlay-change-1 V 40 (point-max) (20 30) (30 40))
(deftest-next-overlay-change-1 W 50 (point-max) (20 30) (30 40))
;; 1 empty, 1 non-empty, non-in
(deftest-next-overlay-change-1 a 10 20 (20 20) (30 40))
(deftest-next-overlay-change-1 b 20 30 (20 30) (30 40))
(deftest-next-overlay-change-1 c 25 30 (20 30) (30 40))
(deftest-next-overlay-change-1 d 30 40 (20 30) (30 40))
(deftest-next-overlay-change-1 e 35 40 (20 30) (30 40))
(deftest-next-overlay-change-1 f 40 (point-max) (20 30) (30 40))
(deftest-next-overlay-change-1 g 50 (point-max) (20 30) (30 40))
;; 1 empty, 1 non-empty, intersecting at begin
(deftest-next-overlay-change-1 h 10 20 (20 20) (20 30))
(deftest-next-overlay-change-1 i 20 30 (20 20) (20 30))
(deftest-next-overlay-change-1 j 25 30 (20 20) (20 30))
(deftest-next-overlay-change-1 k 30 (point-max) (20 20) (20 30))
(deftest-next-overlay-change-1 l 40 (point-max) (20 20) (20 30))
;; 1 empty, 1 non-empty, intersecting at end
(deftest-next-overlay-change-1 h 10 20 (30 30) (20 30))
(deftest-next-overlay-change-1 i 20 30 (30 30) (20 30))
(deftest-next-overlay-change-1 j 25 30 (30 30) (20 30))
(deftest-next-overlay-change-1 k 30 (point-max) (20 20) (20 30))
(deftest-next-overlay-change-1 l 40 (point-max) (20 20) (20 30))
;; 1 empty, 1 non-empty, intersecting in the middle
(deftest-next-overlay-change-1 m 10 20 (25 25) (20 30))
(deftest-next-overlay-change-1 n 20 25 (25 25) (20 30))
(deftest-next-overlay-change-1 o 25 30 (25 25) (20 30))
(deftest-next-overlay-change-1 p 30 (point-max) (25 25) (20 30))
(deftest-next-overlay-change-1 q 40 (point-max) (25 25) (20 30))
;; 2 empty, intersecting
(deftest-next-overlay-change-1 r 10 20 (20 20) (20 20))
(deftest-next-overlay-change-1 s 20 (point-max) (20 20) (20 20))
(deftest-next-overlay-change-1 t 30 (point-max) (20 20) (20 20))
;; 2 empty, non-intersecting
(deftest-next-overlay-change-1 u 10 20 (20 20) (30 30))
(deftest-next-overlay-change-1 v 20 30 (20 20) (30 30))
(deftest-next-overlay-change-1 w 25 30 (20 20) (30 30))
(deftest-next-overlay-change-1 x 30 (point-max) (20 20) (30 30))
(deftest-next-overlay-change-1 y 50 (point-max) (20 20) (30 30))
;; 10 random
(deftest-next-overlay-change-1 aa 1 5
  (58 66) (41 10) (9 67) (28 88) (27 43)
  (24 27) (48 36) (5 90) (61 9))
(deftest-next-overlay-change-1 ab (point-max) (point-max)
  (58 66) (41 10) (9 67) (28 88) (27 43)
  (24 27) (48 36) (5 90) (61 9))
(deftest-next-overlay-change-1 ac 67 88
  (58 66) (41 10) (9 67) (28 88) (27 43)
  (24 27) (48 36) (5 90) (61 9))


;; +==========================================================================+
;; | previous-overlay-change.
;; +==========================================================================+

;; Same for previous-overlay-change.
;; 1 non-empty overlay
(deftest-previous-overlay-change-1 A (point-max) 1)
(deftest-previous-overlay-change-1 B 1 1)
(deftest-previous-overlay-change-1 C 1 1 (10 20))
(deftest-previous-overlay-change-1 D 10 1 (10 20))
(deftest-previous-overlay-change-1 E 15 10 (10 20))
(deftest-previous-overlay-change-1 F 20 10 (10 20))
(deftest-previous-overlay-change-1 G 30 20 (10 20))
;; 1 empty overlay
(deftest-previous-overlay-change-1 H 1 1 (10 10))
(deftest-previous-overlay-change-1 I 10 1 (10 10))
(deftest-previous-overlay-change-1 J 20 10 (10 10))
;; 2 non-empty, non-intersecting
(deftest-previous-overlay-change-1 D 10 1 (20 30) (40 50))
(deftest-previous-overlay-change-1 E 35 30 (20 30) (40 50))
(deftest-previous-overlay-change-1 F 60 50 (20 30) (40 50))
(deftest-previous-overlay-change-1 G 30 20 (20 30) (40 50))
(deftest-previous-overlay-change-1 H 50 40 (20 30) (40 50))
;; 2 non-empty, intersecting
(deftest-previous-overlay-change-1 I 10 1 (20 30) (25 35))
(deftest-previous-overlay-change-1 J 20 1 (20 30) (25 35))
(deftest-previous-overlay-change-1 K 23 20 (20 30) (25 35))
(deftest-previous-overlay-change-1 L 25 20 (20 30) (25 35))
(deftest-previous-overlay-change-1 M 28 25 (20 30) (25 35))
(deftest-previous-overlay-change-1 N 30 25 (20 30) (25 35))
(deftest-previous-overlay-change-1 O 35 30 (20 30) (25 35))
(deftest-previous-overlay-change-1 P 50 35 (20 30) (25 35))
;; 2 non-empty, continuous
(deftest-previous-overlay-change-1 Q 10 1 (20 30) (30 40))
(deftest-previous-overlay-change-1 R 20 1 (20 30) (30 40))
(deftest-previous-overlay-change-1 S 25 20 (20 30) (30 40))
(deftest-previous-overlay-change-1 T 30 20 (20 30) (30 40))
(deftest-previous-overlay-change-1 U 35 30 (20 30) (30 40))
(deftest-previous-overlay-change-1 V 40 30 (20 30) (30 40))
(deftest-previous-overlay-change-1 W 50 40 (20 30) (30 40))
;; 1 empty, 1 non-empty, non-intersecting
(deftest-previous-overlay-change-1 a 10 1 (20 20) (30 40))
(deftest-previous-overlay-change-1 b 20 1 (20 30) (30 40))
(deftest-previous-overlay-change-1 c 25 20 (20 30) (30 40))
(deftest-previous-overlay-change-1 d 30 20 (20 30) (30 40))
(deftest-previous-overlay-change-1 e 35 30 (20 30) (30 40))
(deftest-previous-overlay-change-1 f 40 30 (20 30) (30 40))
(deftest-previous-overlay-change-1 g 50 40 (20 30) (30 40))
;; 1 empty, 1 non-empty, intersecting at begin
(deftest-previous-overlay-change-1 h 10 1 (20 20) (20 30))
(deftest-previous-overlay-change-1 i 20 1 (20 20) (20 30))
(deftest-previous-overlay-change-1 j 25 20 (20 20) (20 30))
(deftest-previous-overlay-change-1 k 30 20 (20 20) (20 30))
(deftest-previous-overlay-change-1 l 40 30 (20 20) (20 30))
;; 1 empty, 1 non-empty, intersecting at end
(deftest-previous-overlay-change-1 m 10 1 (30 30) (20 30))
(deftest-previous-overlay-change-1 n 20 1 (30 30) (20 30))
(deftest-previous-overlay-change-1 o 25 20 (30 30) (20 30))
(deftest-previous-overlay-change-1 p 30 20 (20 20) (20 30))
(deftest-previous-overlay-change-1 q 40 30 (20 20) (20 30))
;; 1 empty, 1 non-empty, intersecting in the middle
(deftest-previous-overlay-change-1 r 10 1 (25 25) (20 30))
(deftest-previous-overlay-change-1 s 20 1 (25 25) (20 30))
(deftest-previous-overlay-change-1 t 25 20 (25 25) (20 30))
(deftest-previous-overlay-change-1 u 30 25 (25 25) (20 30))
(deftest-previous-overlay-change-1 v 40 30 (25 25) (20 30))
;; 2 empty, intersecting
(deftest-previous-overlay-change-1 w 10 1 (20 20) (20 20))
(deftest-previous-overlay-change-1 x 20 1 (20 20) (20 20))
(deftest-previous-overlay-change-1 y 30 20 (20 20) (20 20))
;; 2 empty, non-intersecting
(deftest-previous-overlay-change-1 z 10 1 (20 20) (30 30))
(deftest-previous-overlay-change-1 aa 20 1 (20 20) (30 30))
(deftest-previous-overlay-change-1 ab 25 20 (20 20) (30 30))
(deftest-previous-overlay-change-1 ac 30 20 (20 20) (30 30))
(deftest-previous-overlay-change-1 ad 50 30 (20 20) (30 30))
;; 10 random
(deftest-previous-overlay-change-1 ae 100 90
  (58 66) (41 10) (9 67) (28 88) (27 43)
  (24 27) (48 36) (5 90) (61 9))
(deftest-previous-overlay-change-1 af (point-min) (point-min)
  (58 66) (41 10) (9 67) (28 88) (27 43)
  (24 27) (48 36) (5 90) (61 9))
(deftest-previous-overlay-change-1 ag 29 28
  (58 66) (41 10) (9 67) (28 88) (27 43)
  (24 27) (48 36) (5 90) (61 9))


;; +==========================================================================+
;; | overlays-at
;; +==========================================================================+


;; Test whether overlay-at returns RESULT at POS after overlays OVL were
;; created in a buffer.         POS RES OVL
(deftest-overlays-at-1 A 1 ())
;; 1 overlay
(deftest-overlays-at-1 B 10 (a) (a 10 20))
(deftest-overlays-at-1 C 15 (a) (a 10 20))
(deftest-overlays-at-1 D 19 (a) (a 10 20))
(deftest-overlays-at-1 E 20 ()  (a 10 20))
(deftest-overlays-at-1 F 1 () (a 10 20))

;; 2 non-empty overlays non-intersecting
(deftest-overlays-at-1 G 1 () (a 10 20) (b 30 40))
(deftest-overlays-at-1 H 10 (a) (a 10 20) (b 30 40))
(deftest-overlays-at-1 I 15 (a) (a 10 20) (b 30 40))
(deftest-overlays-at-1 K 20 () (a 10 20) (b 30 40))
(deftest-overlays-at-1 L 25 () (a 10 20) (b 30 40))
(deftest-overlays-at-1 M 30 (b) (a 10 20) (b 30 40))
(deftest-overlays-at-1 N 35 (b) (a 10 20) (b 30 40))
(deftest-overlays-at-1 O 40 () (a 10 20) (b 30 40))
(deftest-overlays-at-1 P 50 () (a 10 20) (b 30 40))

;; 2 non-empty overlays intersecting
(deftest-overlays-at-1 G 1 () (a 10 30) (b 20 40))
(deftest-overlays-at-1 H 10 (a) (a 10 30) (b 20 40))
(deftest-overlays-at-1 I 15 (a) (a 10 30) (b 20 40))
(deftest-overlays-at-1 K 20 (a b) (a 10 30) (b 20 40))
(deftest-overlays-at-1 L 25 (a b) (a 10 30) (b 20 40))
(deftest-overlays-at-1 M 30 (b) (a 10 30) (b 20 40))
(deftest-overlays-at-1 N 35 (b) (a 10 30) (b 20 40))
(deftest-overlays-at-1 O 40 () (a 10 30) (b 20 40))
(deftest-overlays-at-1 P 50 () (a 10 30) (b 20 40))

;; 2 non-empty overlays continuous
(deftest-overlays-at-1 G 1 () (a 10 20) (b 20 30))
(deftest-overlays-at-1 H 10 (a) (a 10 20) (b 20 30))
(deftest-overlays-at-1 I 15 (a) (a 10 20) (b 20 30))
(deftest-overlays-at-1 K 20 (b) (a 10 20) (b 20 30))
(deftest-overlays-at-1 L 25 (b) (a 10 20) (b 20 30))
(deftest-overlays-at-1 M 30 () (a 10 20) (b 20 30))

;; overlays-at never returns empty overlays.
(deftest-overlays-at-1 N 1 (a) (a 1 60) (c 1 1) (b 30 30) (d 50 50))
(deftest-overlays-at-1 O 20 (a) (a 1 60) (c 1 1) (b 30 30) (d 50 50))
(deftest-overlays-at-1 P 30 (a) (a 1 60) (c 1 1) (b 30 30) (d 50 50))
(deftest-overlays-at-1 Q 40 (a) (a 1 60) (c 1 1) (b 30 30) (d 50 50))
(deftest-overlays-at-1 R 50 (a) (a 1 60) (c 1 1) (b 30 30) (d 50 50))
(deftest-overlays-at-1 S 60 () (a 1 60) (c 1 1) (b 30 30) (d 50 50))

;; behavior at point-min and point-max
(ert-deftest test-overlays-at-2 ()
  (cl-macrolet ((should-length (n list)
                               `(should (= ,n (length ,list)))))
    (with-temp-buffer
      (insert (make-string 100 ?\s))
      (make-overlay 1 (point-max))
      (make-overlay 10 10)
      (make-overlay 20 20)
      (make-overlay (point-max) (point-max))
      (should-length 1 (overlays-at 1))
      (should-length 1 (overlays-at 10))
      (should-length 1 (overlays-at 20))
      (should-length 0 (overlays-at (point-max)))
      (narrow-to-region 10 20)
      (should-length 1 (overlays-at (point-min)))
      (should-length 1 (overlays-at 15))
      (should-length 1 (overlays-at (point-max))))))


;; +==========================================================================+
;; | overlay-in
;; +==========================================================================+


;; Test whether overlays-in returns RES in BEG,END after overlays OVL were
;; created in a buffer.

(deftest-overlays-in-1 A 1 (point-max) ());;POS RES OVL
;; 1 overlay
(deftest-overlays-in-1 B 1 10 () (a 10 20))
(deftest-overlays-in-1 C 5 10 () (a 10 20))
(deftest-overlays-in-1 D 5 15 (a) (a 10 20))
(deftest-overlays-in-1 E 10 15 (a)  (a 10 20))
(deftest-overlays-in-1 F 10 20 (a) (a 10 20))
(deftest-overlays-in-1 G 15 20 (a) (a 10 20))
(deftest-overlays-in-1 H 15 25 (a) (a 10 20))
(deftest-overlays-in-1 I 20 25 () (a 10 20))
(deftest-overlays-in-1 J 30 50 () (a 10 20))

;; 2 non-empty overlays non-intersecting
(deftest-overlays-in-1 K 1 5 () (a 10 20) (b 30 40))
(deftest-overlays-in-1 L 5 10 () (a 10 20) (b 30 40))
(deftest-overlays-in-1 M 5 15 (a) (a 10 20) (b 30 40))
(deftest-overlays-in-1 N 10 15 (a) (a 10 20) (b 30 40))
(deftest-overlays-in-1 O 15 20 (a) (a 10 20) (b 30 40))
(deftest-overlays-in-1 P 15 25 (a) (a 10 20) (b 30 40))
(deftest-overlays-in-1 Q 20 25 () (a 10 20) (b 30 40))
(deftest-overlays-in-1 R 20 30 () (a 10 20) (b 30 40))
(deftest-overlays-in-1 S 25 30 () (a 10 20) (b 30 40))
(deftest-overlays-in-1 T 25 35 (b) (a 10 20) (b 30 40))
(deftest-overlays-in-1 U 30 35 (b) (a 10 20) (b 30 40))
(deftest-overlays-in-1 V 40 50  () (a 10 20) (b 30 40))
(deftest-overlays-in-1 W 50 60  () (a 10 20) (b 30 40))
(deftest-overlays-in-1 X 1 50  (a b) (a 10 20) (b 30 40))
(deftest-overlays-in-1 Y 10 40  (a b) (a 10 20) (b 30 40))
(deftest-overlays-in-1 Z 10 41  (a b) (a 10 20) (b 30 40))

;; 2 non-empty overlays intersecting
(deftest-overlays-in-1 a 1 5 () (a 10 30) (b 20 40))
(deftest-overlays-in-1 b 5 10 () (a 10 30) (b 20 40))
(deftest-overlays-in-1 c 5 15 (a) (a 10 30) (b 20 40))
(deftest-overlays-in-1 d 10 15 (a) (a 10 30) (b 20 40))
(deftest-overlays-in-1 e 10 20 (a) (a 10 30) (b 20 40))
(deftest-overlays-in-1 f 15 20 (a) (a 10 30) (b 20 40))
(deftest-overlays-in-1 g 20 30 (a b) (a 10 30) (b 20 40))
(deftest-overlays-in-1 h 20 40 (a b) (a 10 30) (b 20 40))
(deftest-overlays-in-1 i 25 30 (a b) (a 10 30) (b 20 40))
(deftest-overlays-in-1 j 30 30 (b) (a 10 30) (b 20 40))
(deftest-overlays-in-1 k 30 35 (b) (a 10 30) (b 20 40))
(deftest-overlays-in-1 l 35 40 (b) (a 10 30) (b 20 40))
(deftest-overlays-in-1 m 40 45 () (a 10 30) (b 20 40))
(deftest-overlays-in-1 n 41 45 () (a 10 30) (b 20 40))
(deftest-overlays-in-1 o 50 60 () (a 10 30) (b 20 40))

;; 2 non-empty overlays continuous
(deftest-overlays-in-1 p 1 5 () (a 10 20) (b 20 30))
(deftest-overlays-in-1 q 5 10 () (a 10 20) (b 20 30))
(deftest-overlays-in-1 r 15 20 (a) (a 10 20) (b 20 30))
(deftest-overlays-in-1 s 15 25 (a b) (a 10 20) (b 20 30))
(deftest-overlays-in-1 t 20 25 (b) (a 10 20) (b 20 30))
(deftest-overlays-in-1 u 25 30 (b) (a 10 20) (b 20 30))
(deftest-overlays-in-1 v 29 35 (b) (a 10 20) (b 20 30))
(deftest-overlays-in-1 w 30 35 () (a 10 20) (b 20 30))
(deftest-overlays-in-1 x 35 50 () (a 10 20) (b 20 30))
(deftest-overlays-in-1 y 1 50 (a b) (a 10 20) (b 20 30))
(deftest-overlays-in-1 z 15 50 (a b) (a 10 20) (b 20 30))
(deftest-overlays-in-1 aa 1 25 (a b) (a 10 20) (b 20 30))

;; 1 empty overlay
(deftest-overlays-in-1 ab 1 10 () (a 10 10))
(deftest-overlays-in-1 ac 10 10 (a) (a 10 10))
(deftest-overlays-in-1 ad 9 10 () (a 10 10))
(deftest-overlays-in-1 ae 9 11 (a) (a 10 10))
(deftest-overlays-in-1 af 10 11 (a) (a 10 10))


;; behavior at point-max
(ert-deftest test-overlays-in-2 ()
  (cl-macrolet ((should-length (n list)
                               `(should (= ,n (length ,list)))))
    (with-temp-buffer
      (insert (make-string 100 ?\s))
      (make-overlay (point-max) (point-max))
      (make-overlay 50 50)
      (should-length 1 (overlays-in 50 50))
      (should-length 2 (overlays-in 1 (point-max)))
      (should-length 1 (overlays-in (point-max) (point-max)))
      (narrow-to-region 1 50)
      (should-length 0 (overlays-in 1 (point-max)))
      (should-length 1 (overlays-in (point-max) (point-max))))))


;; +==========================================================================+
;; | overlay-recenter
;; +==========================================================================+

;; This function is a noop in the overlay tree branch.
(ert-deftest test-overlay-recenter ()
  (with-temp-buffer
    (should-not (overlay-recenter 1))
    (insert (make-string 100 ?\s))
    (dotimes (i 10)
      (make-overlay i (1+ i))
      (should-not (overlay-recenter i)))))


;; +==========================================================================+
;; | move-overlay
;; +==========================================================================+

;; buffer nil with live overlay
(ert-deftest test-move-overlay-1 ()
  (test-with-overlay-in-buffer (ov 1 100)
    (move-overlay ov 50 60)
    (should (= 50 (overlay-start ov)))
    (should (= 60 (overlay-end ov)))
    (should (eq (current-buffer) (overlay-buffer ov)))))

;; buffer nil, dead overlay
(ert-deftest test-move-overlay-2 ()
  (with-temp-buffer
    (let ((ov (test-with-overlay-in-buffer (ov 1 100) ov)))
      (insert (make-string 100 ?\s))
      (move-overlay ov 50 60)
      (should (= 50 (overlay-start ov)))
      (should (= 60 (overlay-end ov)))
      (should (eq (current-buffer) (overlay-buffer ov))))))

;; buffer non-nil, live overlay
(ert-deftest test-move-overlay-3 ()
  (test-with-overlay-in-buffer (ov 10 100)
    (with-temp-buffer
      (move-overlay ov 1 1 (current-buffer))
      (should (= 1 (overlay-start ov)))
      (should (= 1 (overlay-end ov)))
      (should (eq (current-buffer) (overlay-buffer ov))))
    (should-not (overlay-start ov))
    (should-not (overlay-end ov))
    (should-not (overlay-buffer ov))))

;; buffer non-nil, dead overlay
(ert-deftest test-move-overlay-4 ()
  (let ((ov (test-with-overlay-in-buffer (ov 1 1) ov)))
    (with-temp-buffer
      (move-overlay ov 1 1 (current-buffer))
      (should (= 1 (overlay-start ov)))
      (should (= 1 (overlay-end ov)))
      (should (eq (current-buffer) (overlay-buffer ov))))
    (should-not (overlay-start ov))
    (should-not (overlay-end ov))
    (should-not (overlay-buffer ov))))

;; +==========================================================================+
;; | delete-(all-)overlay
;; +==========================================================================+

;; delete live overlay
(ert-deftest test-delete-overlay-1 ()
  (test-with-overlay-in-buffer (ov 10 100)
    (should (buffer-live-p (overlay-buffer ov)))
    (delete-overlay ov)
    (should-not (overlay-start ov))
    (should-not (overlay-end ov))
    (should-not (overlay-buffer ov))))

;; delete dead overlay
(ert-deftest test-delete-overlay-2 ()
  (let ((ov (test-with-overlay-in-buffer (ov 10 100) ov)))
    (should-not (overlay-start ov))
    (should-not (overlay-end ov))
    (should-not (overlay-buffer ov))
    (should-not (delete-overlay ov))
    (should-not (overlay-start ov))
    (should-not (overlay-end ov))
    (should-not (overlay-buffer ov))))

(ert-deftest test-delete-all-overlay-1 ()
  (with-temp-buffer
    (should-not (delete-all-overlays))
    (should-not (delete-all-overlays (current-buffer)))
    (insert (make-string 100 ?\s))
    (dotimes (i 10) (make-overlay i (1+ i)))
    (should-not (delete-all-overlays (current-buffer)))
    (should-not (delete-all-overlays))))


;; +==========================================================================+
;; | get-char-property(-and-overlay)
;; +==========================================================================+

;; FIXME: TBD


;; +==========================================================================+
;; | Moving by insertions
;; +==========================================================================+

(defmacro deftest-moving-insert-1 (id beg-end insert sbeg-send fa ra)
  (cl-destructuring-bind (beg end ipos ilen sbeg send fa ra)
      (append beg-end insert sbeg-send (list fa ra) nil)
    `(ert-deftest ,(buffer-tests--make-test-name 'moving-insert 1 id) ()
       (test-with-overlay-in-buffer (ov ,beg ,end ,fa ,ra)
         (should (= ,beg (overlay-start ov)))
         (should (= ,end (overlay-end ov)))
         (goto-char ,ipos)
         (insert (make-string ,ilen ?x))
         (should (= ,sbeg (overlay-start ov)))
         (should (= ,send (overlay-end ov)))))))

;; non-empty, no fa, no ra
;; --------------------  OV      INS    RESULT
(deftest-moving-insert-1 A (10 20) (15 3) (10 23) nil nil)
(deftest-moving-insert-1 B (10 20) (20 4) (10 20) nil nil)
(deftest-moving-insert-1 C (10 20) (5 5) (15 25) nil nil)
(deftest-moving-insert-1 D (10 20) (10 3) (10 23) nil nil)
(deftest-moving-insert-1 E (10 20) (20 4) (10 20) nil nil)

;; non-empty no fa, ra
(deftest-moving-insert-1 F (10 20) (15 3) (10 23) nil t)
(deftest-moving-insert-1 G (10 20) (20 4) (10 24) nil t)
(deftest-moving-insert-1 H (10 20) (5 5) (15 25) nil t)
(deftest-moving-insert-1 I (10 20) (10 3) (10 23) nil t)
(deftest-moving-insert-1 J (10 20) (20 4) (10 24) nil t)

;; non-empty, fa, no r
(deftest-moving-insert-1 K (10 20) (15 3) (10 23) t nil)
(deftest-moving-insert-1 L (10 20) (20 4) (10 20) t nil)
(deftest-moving-insert-1 M (10 20) (5 5) (15 25) t nil)
(deftest-moving-insert-1 N (10 20) (10 3) (13 23) t nil)
(deftest-moving-insert-1 O (10 20) (20 4) (10 20) t nil)

;; This used to fail.
(ert-deftest test-moving-insert-2-a ()
  (with-temp-buffer
    (insert (make-string 1 ?.))
    (let ((ov (make-overlay 1 1 nil t nil)))
      (insert "()")
      (should (= 1 (overlay-end ov))))))

;; non-empty, fa, ra
(deftest-moving-insert-1 P (10 20) (15 3) (10 23) t t)
(deftest-moving-insert-1 Q (10 20) (20 4) (10 24) t t)
(deftest-moving-insert-1 R (10 20) (5 5) (15 25) t t)
(deftest-moving-insert-1 S (10 20) (10 3) (13 23) t t)
(deftest-moving-insert-1 T (10 20) (20 4) (10 24) t t)

;; empty, no fa, no ra
(deftest-moving-insert-1 U (15 15) (20 4) (15 15) nil nil)
(deftest-moving-insert-1 V (15 15) (5 5) (20 20) nil nil)
(deftest-moving-insert-1 W (15 15) (15 3) (15 15) nil nil)

;; empty no fa, ra
(deftest-moving-insert-1 X (15 15) (20 4) (15 15) nil t)
(deftest-moving-insert-1 Y (15 15) (5 5) (20 20) nil t)
(deftest-moving-insert-1 Z (15 15) (15 3) (15 18) nil t)

;; empty, fa, no ra
(deftest-moving-insert-1 a (15 15) (20 4) (15 15) t nil)
(deftest-moving-insert-1 b (15 15) (5 5) (20 20) t nil)
(deftest-moving-insert-1 c (15 15) (15 3) (15 15) t nil)

;; empty, fa, ra
(deftest-moving-insert-1 d (15 15) (20 4) (15 15) t t)
(deftest-moving-insert-1 e (15 15) (5 5) (20 20) t t)
(deftest-moving-insert-1 f (15 15) (15 3) (18 18) t t)

;; Try to trigger a pathological case where the tree could become
;; unordered due to an insert operation.

(ert-deftest test-moving-insert-2 ()
  (with-temp-buffer
    (insert (make-string 1000 ?x))
    (let ((root (make-overlay 50 75 nil    nil             'rear-advance))
          (left (make-overlay 25 50 nil    'front-advance  'rear-advance))
          (right (make-overlay 75 100 nil  nil             nil)))
      ;;     [50] <--- start
      ;;    /    \
      ;; (25)    (75)
      (delete-region 25 75)
      ;;     [25]
      ;;    /    \
      ;; (25)    (25)
      (should (= 25 (overlay-start root)))
      (should (= 25 (overlay-end root)))
      (should (= 25 (overlay-start left)))
      (should (= 25 (overlay-end left)))
      (should (= 25 (overlay-start right)))
      (should (= 50 (overlay-end right)))
      ;; Inserting at start should make left advance while right and
      ;; root stay, thus we would have left > right .
      (goto-char 25)
      (insert (make-string 25 ?x))
      ;;     [25]
      ;;    /    \
      ;; (50)    (25)
      (should (= 25 (overlay-start root)))
      (should (= 50 (overlay-end root)))
      (should (= 50 (overlay-start left)))
      (should (= 50 (overlay-end left)))
      (should (= 25 (overlay-start right)))
      (should (= 75 (overlay-end right)))
      ;; Try to detect the error, by removing left.  The should fail
      ;; an eassert, since it won't be found by a regular tree
      ;; traversal - in theory.
      (delete-overlay left)
      (should (= 2 (length (overlays-in 1 (point-max))))))))



;; +==========================================================================+
;; | Moving by deletions
;; +==========================================================================+

(defmacro deftest-moving-delete-1 (id beg-end delete sbeg-send fa ra)
  (cl-destructuring-bind (beg end dpos dlen sbeg send fa ra)
      (append beg-end delete sbeg-send (list fa ra) nil)
    `(ert-deftest ,(buffer-tests--make-test-name 'moving-delete 1 id) ()
       (test-with-overlay-in-buffer (ov ,beg ,end ,fa ,ra)
         (should (= ,beg (overlay-start ov)))
         (should (= ,end (overlay-end ov)))
         (delete-region ,dpos (+ ,dpos ,dlen))
         (should (= ,sbeg (overlay-start ov)))
         (should (= ,send (overlay-end ov)))))))

;; non-empty, no fa, no ra
;; --------------------  OV      DEL    RESULT
(deftest-moving-delete-1 A (10 20) (15 3) (10 17) nil nil)
(deftest-moving-delete-1 B (10 20) (20 4) (10 20) nil nil)
(deftest-moving-delete-1 C (10 20) (5 5) (5 15) nil nil)
(deftest-moving-delete-1 D (10 20) (10 3) (10 17) nil nil)
(deftest-moving-delete-1 E (10 20) (20 4) (10 20) nil nil)

;; non-empty no fa, ra
(deftest-moving-delete-1 F (10 20) (15 3) (10 17) nil t)
(deftest-moving-delete-1 G (10 20) (20 4) (10 20) nil t)
(deftest-moving-delete-1 H (10 20) (5 5) (5 15) nil t)
(deftest-moving-delete-1 I (10 20) (10 3) (10 17) nil t)
(deftest-moving-delete-1 J (10 20) (20 4) (10 20) nil t)

;; non-empty, fa, no ra
(deftest-moving-delete-1 K (10 20) (15 3) (10 17) t nil)
(deftest-moving-delete-1 L (10 20) (20 4) (10 20) t nil)
(deftest-moving-delete-1 M (10 20) (5 5) (5 15) t nil)
(deftest-moving-delete-1 N (10 20) (10 3) (10 17) t nil)
(deftest-moving-delete-1 O (10 20) (20 4) (10 20) t nil)

;; non-empty, fa, ra
(deftest-moving-delete-1 P (10 20) (15 3) (10 17) t t)
(deftest-moving-delete-1 Q (10 20) (20 4) (10 20) t t)
(deftest-moving-delete-1 R (10 20) (5 5) (5 15) t t)
(deftest-moving-delete-1 S (10 20) (10 3) (10 17) t t)
(deftest-moving-delete-1 T (10 20) (20 4) (10 20) t t)

;; empty, no fa, no ra
(deftest-moving-delete-1 U (15 15) (20 4) (15 15) nil nil)
(deftest-moving-delete-1 V (15 15) (5 5) (10 10) nil nil)
(deftest-moving-delete-1 W (15 15) (15 3) (15 15) nil nil)

;; empty no fa, ra
(deftest-moving-delete-1 X (15 15) (20 4) (15 15) nil t)
(deftest-moving-delete-1 Y (15 15) (5 5) (10 10) nil t)
(deftest-moving-delete-1 Z (15 15) (15 3) (15 15) nil t)

;; empty, fa, no ra
(deftest-moving-delete-1 a (15 15) (20 4) (15 15) t nil)
(deftest-moving-delete-1 b (15 15) (5 5) (10 10) t nil)
(deftest-moving-delete-1 c (15 15) (15 3) (15 15) t nil)

;; empty, fa, ra
(deftest-moving-delete-1 d (15 15) (20 4) (15 15) t t)
(deftest-moving-delete-1 e (15 15) (5 5) (10 10) t t)
(deftest-moving-delete-1 f (15 15) (15 3) (15 15) t t)


;; +==========================================================================+
;; | make-indirect-buffer
;; +==========================================================================+

;; Check if overlays are cloned/separate from indirect buffer.
(ert-deftest test-make-indirect-buffer-1 ()
  (with-temp-buffer
    (dotimes (_ 10) (make-overlay 1 1))
    (let (indirect clone)
      (unwind-protect
          (progn
            (setq indirect (make-indirect-buffer
                            (current-buffer) "indirect"))
            (with-current-buffer indirect
              (should-not (overlays-in (point-min) (point-max)))
              (dotimes (_ 20) (make-overlay 1 1))
              (should (= 20 (length (overlays-in (point-min) (point-max)))))
              (delete-all-overlays)
              (should-not (overlays-in (point-min) (point-max))))
            (should (= 10 (length (overlays-in (point-min) (point-max)))))
            (setq clone (make-indirect-buffer
                         (current-buffer) "clone" 'clone))
            (with-current-buffer clone
              (should (= 10 (length (overlays-in (point-min) (point-max)))))
              (dotimes (_ 30) (make-overlay 1 1))
              (should (= 40 (length (overlays-in (point-min) (point-max))))))
            ;; back in temp buffer
            (should (= 10 (length (overlays-in (point-min) (point-max)))))
            (with-current-buffer clone
              (mapc #'delete-overlay
                    (seq-take (overlays-in (point-min) (point-max)) 10))
              (should (= 30 (length (overlays-in (point-min) (point-max))))))
            (should (= 10 (length (overlays-in (point-min) (point-max)))))
            (delete-all-overlays)
            (with-current-buffer clone
              (should (= 30 (length (overlays-in (point-min) (point-max)))))))
        (when (buffer-live-p clone)
          (kill-buffer clone))
        (when (buffer-live-p indirect)
          (kill-buffer indirect))))))



;; +==========================================================================+
;; | buffer-swap-text
;; +==========================================================================+

(defmacro buffer-tests--with-temp-buffers (vars &rest body)
  (declare (indent 1) (debug (sexp &rest form)))
  (if (null vars)
      `(progn ,@body)
    `(with-temp-buffer
       (let ((,(car vars) (current-buffer)))
         (buffer-tests--with-temp-buffers ,(cdr vars) ,@body)))))

;; basic
(ert-deftest test-buffer-swap-text-1 ()
  (buffer-tests--with-temp-buffers (buffer other)
    (with-current-buffer buffer
      (let ((ov (make-overlay 1 1)))
        (buffer-swap-text other)
        (should-not (overlays-in 1 1))
        (with-current-buffer other
          (should (overlays-in 1 1))
          (should (eq ov (car (overlays-in 1 1)))))))))

;; properties
(ert-deftest test-buffer-swap-text-1 ()
  (buffer-tests--with-temp-buffers (buffer other)
    (with-current-buffer other
      (overlay-put (make-overlay 1 1) 'buffer 'other))
    (with-current-buffer buffer
      (overlay-put (make-overlay 1 1) 'buffer 'buffer)
      (buffer-swap-text other)
      (should (= 1 (length (overlays-in 1 1))))
      (should (eq (overlay-get (car (overlays-in 1 1)) 'buffer) 'other)))
    (with-current-buffer other
      (should (= 1 (length (overlays-in 1 1))))
      (should (eq (overlay-get (car (overlays-in 1 1)) 'buffer) 'buffer)))))


;; +==========================================================================+
;; | priorities
;; +==========================================================================+

(ert-deftest test-overlay-priorities-1 ()
  (with-temp-buffer
    (insert " ")
    (dotimes (i 10)
      (let ((ov (make-overlay 1 2)))
        (overlay-put ov 'priority i)
        (overlay-put ov 'value i)))
    (should (eq 9 (get-char-property 1 'value)))))

(ert-deftest test-overlay-priorities-2 ()
  (with-temp-buffer
    (insert " ")
    (dotimes (j 10)
      (let* ((i (- 9 j))
             (ov (make-overlay 1 2)))
        (overlay-put ov 'priority i)
        (overlay-put ov 'value i)))
    (should (eq 9 (get-char-property 1 'value)))))


;; +==========================================================================+
;; | Other
;; +==========================================================================+

(defun test-overlay-regions ()
  (sort (mapcar (lambda (ov)
                  (cons (overlay-start ov)
                        (overlay-end ov)))
                (overlays-in (point-min)
                             (point-max)))
        (lambda (o1 o2)
          (or (< (car o1) (car o2))
              (and (= (car o1) (car o2))
                   (< (cdr o1) (cdr o2)))))))

;; This test used to fail.
(ert-deftest overlay-complex-delete-with-offset ()
  (with-temp-buffer
    (let (todelete)
      (insert (make-string 1000 ?\s))
      (make-overlay 1 2 nil t nil)
      (make-overlay 2 3 nil t nil)
      (make-overlay 3 4 nil t nil)
      (make-overlay 4 5 nil t nil)
      (setq todelete (make-overlay 280 287 nil t nil))
      (make-overlay 265 275 nil t nil)
      (make-overlay 329 386 nil t nil)
      (make-overlay 386 390 nil t nil)
      (goto-char 50)
      (delete-char 50)
      (goto-char 1)
      (delete-char 2)
      (delete-overlay todelete)
      (should (equal (test-overlay-regions)
                     '((1 . 1) (1 . 1) (1 . 2) (2 . 3) (213 . 223) (277 . 334) (334 . 338)))))))

;; This test used to fail.
(ert-deftest overlay-complex-insert-1 ()
  (with-temp-buffer
    (insert "          ")
    (make-overlay 8 11 nil nil t)
    (make-overlay 2 7 nil nil nil)
    (make-overlay 2 4 nil t nil)
    (goto-char 1)
    (insert "     ")
    (should (equal (test-overlay-regions)
                   '((7 . 9)
                     (7 . 12)
                     (13 . 16))))))

;; This test used to fail.
(ert-deftest overlay-complex-insert-2 ()
  (with-temp-buffer
    (insert (make-string 100 ?\s))
    (make-overlay 77 7 nil nil t)
    (make-overlay 21 53 nil t t)
    (make-overlay 84 14 nil nil nil)
    (make-overlay 38 69 nil t nil)
    (make-overlay 93 15 nil nil t)
    (make-overlay 73 48 nil t t)
    (make-overlay 96 51 nil t t)
    (make-overlay 6 43 nil t t)
    (make-overlay 15 100 nil t t)
    (make-overlay 22 17 nil nil nil)
    (make-overlay 72 45 nil t nil)
    (make-overlay 2 74 nil nil t)
    (make-overlay 15 29 nil t t)
    (make-overlay 17 34 nil t t)
    (make-overlay 101 66 nil t nil)
    (make-overlay 94 24 nil nil nil)
    (goto-char 78)
    (insert "           ")
    (narrow-to-region 47 19)
    (goto-char 46)
    (widen)
    (narrow-to-region 13 3)
    (goto-char 9)
    (delete-char 0)
    (goto-char 11)
    (insert "           ")
    (goto-char 3)
    (insert "          ")
    (goto-char 8)
    (insert "       ")
    (goto-char 26)
    (insert "  ")
    (goto-char 14)
    (widen)
    (narrow-to-region 71 35)
    (should
     (equal (test-overlay-regions)
            '((2 . 104)
              (23 . 73)
              (24 . 107)
              (44 . 125)
              (45 . 59)
              (45 . 134)
              (45 . 141)
              (47 . 52)
              (47 . 64)
              (51 . 83)
              (54 . 135)
              (68 . 99))))))

(ert-deftest test-overlay-multibyte-transition-1 ()
  (with-temp-buffer
    (set-buffer-multibyte t)
    (insert "ääää")
    ;; aeaeaeae
    ;; 1 2 3 4 5
    ;; 123456789
    (let ((nonempty-bob (make-overlay 1 2))
          (empty-bob    (make-overlay 1 1))
          (empty        (make-overlay 2 2))
          (nonempty     (make-overlay 2 4))
          (nonempty-eob (make-overlay 4 5))
          (empty-eob    (make-overlay 5 5)))
      (set-buffer-multibyte nil)
      (cl-macrolet
          ((ovshould (ov begin end)
             `(should (equal (list (overlay-start ,ov) (overlay-end ,ov))
                             (list ,begin ,end)))))
        (ovshould nonempty-bob 1 3)
        (ovshould empty-bob    1 1)
        (ovshould empty        3 3)
        (ovshould nonempty     3 7)
        (ovshould nonempty-eob 7 9)
        (ovshould empty-eob    9 9)))))

(ert-deftest test-overlay-multibyte-transition-2 ()
  (with-temp-buffer
    (set-buffer-multibyte t)
    (insert "ääää")
    (set-buffer-multibyte nil)
    ;; aeaeaeae
    ;; 1 2 3 4 5
    ;; 123456789
    (let ((nonempty-bob-end (make-overlay 1 2))
          (nonempty-bob-beg (make-overlay 1 3))
          (empty-bob        (make-overlay 1 1))
          (empty-beg        (make-overlay 3 3))
          (empty-end        (make-overlay 2 2))
          (nonempty-beg-beg (make-overlay 3 7))
          (nonempty-beg-end (make-overlay 3 8))
          (nonempty-end-beg (make-overlay 4 7))
          (nonempty-end-end (make-overlay 4 8))
          (nonempty-eob-beg (make-overlay 5 9))
          (nonempty-eob-end (make-overlay 6 9))
          (empty-eob        (make-overlay 9 9)))
      (set-buffer-multibyte t)
      (cl-macrolet
          ((ovshould (ov begin end)
             `(should (equal (list (overlay-start ,ov) (overlay-end ,ov))
                             (list ,begin ,end)))))
        (ovshould nonempty-bob-end 1 2)
        (ovshould nonempty-bob-beg 1 2)
        (ovshould empty-bob        1 1)
        (ovshould empty-beg        2 2)
        (ovshould empty-end        2 2)
        (ovshould nonempty-beg-beg 2 4)
        (ovshould nonempty-beg-end 2 5)
        (ovshould nonempty-end-beg 3 4)
        (ovshould nonempty-end-end 3 5)
        (ovshould nonempty-eob-beg 3 5)
        (ovshould nonempty-eob-end 4 5)
        (ovshould empty-eob        5 5)))))

;;; buffer-tests.el ends here
