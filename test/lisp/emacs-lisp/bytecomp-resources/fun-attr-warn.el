;;; -*- lexical-binding: t -*-

;; Correct

(defun faw-str-decl-code (x)
  "something"
  (declare (pure t))
  (print x))

(defun faw-doc-decl-code (x)
  (:documentation "something")
  (declare (pure t))
  (print x))

(defun faw-str-int-code (x)
  "something"
  (interactive "P")
  (print x))

(defun faw-doc-int-code (x)
  (:documentation "something")
  (interactive "P")
  (print x))

(defun faw-decl-int-code (x)
  (declare (pure t))
  (interactive "P")
  (print x))

(defun faw-str-decl-int-code (x)
  "something"
  (declare (pure t))
  (interactive "P")
  (print x))

(defun faw-doc-decl-int-code (x)
  (:documentation "something")
  (declare (pure t))
  (interactive "P")
  (print x))


;; Correct (last string is return value)

(defun faw-str ()
  "something")

(defun faw-decl-str ()
  (declare (pure t))
  "something")

(defun faw-decl-int-str ()
  (declare (pure t))
  (interactive)
  "something")

(defun faw-str-str ()
  "something"
  "something else")

(defun faw-doc-str ()
  (:documentation "something")
  "something else")


;; Incorrect (bad order)

(defun faw-int-decl-code (x)
  (interactive "P")
  (declare (pure t))
  (print x))

(defun faw-int-str-code (x)
  (interactive "P")
  "something"
  (print x))

(defun faw-int-doc-code (x)
  (interactive "P")
  (:documentation "something")
  (print x))

(defun faw-decl-str-code (x)
  (declare (pure t))
  "something"
  (print x))

(defun faw-decl-doc-code (x)
  (declare (pure t))
  (:documentation "something")
  (print x))

(defun faw-str-int-decl-code (x)
  "something"
  (interactive "P")
  (declare (pure t))
  (print x))

(defun faw-doc-int-decl-code (x)
  (:documentation "something")
  (interactive "P")
  (declare (pure t))
  (print x))

(defun faw-int-str-decl-code (x)
  (interactive "P")
  "something"
  (declare (pure t))
  (print x))

(defun faw-int-doc-decl-code (x)
  (interactive "P")
  (:documentation "something")
  (declare (pure t))
  (print x))

(defun faw-int-decl-str-code (x)
  (interactive "P")
  (declare (pure t))
  "something"
  (print x))

(defun faw-int-decl-doc-code (x)
  (interactive "P")
  (declare (pure t))
  (:documentation "something")
  (print x))

(defun faw-decl-int-str-code (x)
  (declare (pure t))
  (interactive "P")
  "something"
  (print x))

(defun faw-decl-int-doc-code (x)
  (declare (pure t))
  (interactive "P")
  (:documentation "something")
  (print x))

(defun faw-decl-str-int-code (x)
  (declare (pure t))
  "something"
  (interactive "P")
  (print x))

(defun faw-decl-doc-int-code (x)
  (declare (pure t))
  (:documentation "something")
  (interactive "P")
  (print x))


;; Incorrect (duplication)

(defun faw-str-str-decl-int-code (x)
  "something"
  "something else"
  (declare (pure t))
  (interactive "P")
  (print x))

(defun faw-str-doc-decl-int-code (x)
  "something"
  (:documentation "something else")
  (declare (pure t))
  (interactive "P")
  (print x))

(defun faw-doc-str-decl-int-code (x)
  (:documentation "something")
  "something else"
  (declare (pure t))
  (interactive "P")
  (print x))

(defun faw-doc-doc-decl-int-code (x)
  (:documentation "something")
  (:documentation "something else")
  (declare (pure t))
  (interactive "P")
  (print x))

(defun faw-str-decl-str-int-code (x)
  "something"
  (declare (pure t))
  "something else"
  (interactive "P")
  (print x))

(defun faw-doc-decl-str-int-code (x)
  (:documentation "something")
  (declare (pure t))
  "something else"
  (interactive "P")
  (print x))

(defun faw-str-decl-doc-int-code (x)
  "something"
  (declare (pure t))
  (:documentation "something else")
  (interactive "P")
  (print x))

(defun faw-doc-decl-doc-int-code (x)
  (:documentation "something")
  (declare (pure t))
  (:documentation "something else")
  (interactive "P")
  (print x))

(defun faw-str-decl-decl-int-code (x)
  "something"
  (declare (pure t))
  (declare (indent 1))
  (interactive "P")
  (print x))

(defun faw-doc-decl-decl-int-code (x)
  (:documentation "something")
  (declare (pure t))
  (declare (indent 1))
  (interactive "P")
  (print x))

(defun faw-str-decl-int-decl-code (x)
  "something"
  (declare (pure t))
  (interactive "P")
  (declare (indent 1))
  (print x))

(defun faw-doc-decl-int-decl-code (x)
  (:documentation "something")
  (declare (pure t))
  (interactive "P")
  (declare (indent 1))
  (print x))

(defun faw-str-decl-int-int-code (x)
  "something"
  (declare (pure t))
  (interactive "P")
  (interactive "p")
  (print x))

(defun faw-doc-decl-int-int-code (x)
  (:documentation "something")
  (declare (pure t))
  (interactive "P")
  (interactive "p")
  (print x))

(defun faw-str-int-decl-int-code (x)
  "something"
  (interactive "P")
  (declare (pure t))
  (interactive "p")
  (print x))

(defun faw-doc-int-decl-int-code (x)
  (:documentation "something")
  (interactive "P")
  (declare (pure t))
  (interactive "p")
  (print x))
