(defpackage :lem-vi-mode/leader
  (:use :cl
        :lem)
  (:import-from :lem-vi-mode/core
                :vi-keymap)
  (:export :leader-key))
(in-package :lem-vi-mode/leader)

(define-named-key "Leader")

(define-editor-variable leader-key "\\")

(defun mapleader-key ()
  (first (lem-core::parse-keyspec (variable-value 'leader-key))))

(defun mapleader-key-p (key)
  (eq key (mapleader-key)))

(defun leader-key ()
  (make-key :sym "Leader"))

(defmethod keymap-find-keybind ((keymap vi-keymap) (key lem-core::key) cmd)
  (if (mapleader-key-p key)
      (call-next-method keymap (leader-key) cmd)
      (call-next-method)))

(defmethod keymap-find-keybind ((keymap vi-keymap) (key cons) cmd)
  (if (mapleader-key-p (first key))
      (call-next-method keymap (cons (leader-key) (rest key)) cmd)
      (call-next-method)))
