;; -*- lexical-binding: t; -*-

;; In this test, we try and make sure that inlined functions's code isn't
;; mistakenly re-interpreted in the caller's context: we import an
;; inlinable function from another file where `foo-var' is a normal
;; lexical variable, and then call(inline) it in a function where
;; `foo-var' is a dynamically-scoped variable.

(require 'foo-inlinable
         (expand-file-name "foo-inlinable.el"
                           (file-name-directory
                            (or byte-compile-current-file load-file-name))))

(defvar foo-var)

(defun foo-fun ()
  (+ (foo-inlineable 5) 1))
