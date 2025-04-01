(defpackage :lem-vi-mode/core
  (:use :cl
        :lem
        :lem/universal-argument)
  (:import-from :cl-package-locks)
  (:import-from :cl-ppcre)
  (:import-from :alexandria
                :with-gensyms)
  (:export :*enable-hook*
           :*disable-hook*
           :*default-cursor-color*
           :*last-repeat-keys*
           :*enable-repeat-recording*
           :vi-state
           :vi-mode
           :define-state
           :current-state
           :current-main-state
           :state=
           :change-state
           :with-state
           :with-temporary-state
           :mode-specific-keymaps
           :pre-command-hook
           :post-command-hook
           :state-enabled-hook
           :state-disabled-hook
           :vi-this-command-keys
           :this-motion-command
           :vi-command
           :vi-command-repeat
           :vi-motion
           :vi-motion-type
           :vi-motion-default-n-arg
           :vi-operator
           :vi-text-object
           :range
           :make-range
           :range-beginning
           :range-end
           :range-type
           :operator-abort
           :text-object-abort
           :text-object-abort-range
           :vi-current-window
           :with-main-window
           :vi-keymap
           :define-keymap))
(in-package :lem-vi-mode/core)

(defvar *last-repeat-keys* '())

(defvar *default-cursor-color* "#ffb472")

(defvar *enable-hook* '())
(defvar *disable-hook* '())

(defvar *enable-repeat-recording* t)

(defvar *this-motion-command* nil)

(defun enable-hook ()
  (run-hooks *enable-hook*))

(defun disable-hook ()
  (run-hooks *disable-hook*))

(defvar *fallback-keymap* *global-keymap*)

(define-global-mode vi-mode (emacs-mode)
  (:name "vi"
   :keymap *fallback-keymap*
   :enable-hook #'enable-hook
   :disable-hook #'disable-hook))

(defclass vi-state ()
  ((name :initarg :name
         :initform nil
         :reader state-name)
   (cursor-type
    :initarg :cursor-type
    :initform :box
    :reader state-cursor-type)
   (modeline-color
    :initarg :modeline-color
    :initform 'state-modeline-white
    :reader state-modeline-color)
   (keymaps
    :initarg :keymaps
    :initform '()
    :reader state-keymaps)
   (cursor-color
    :initarg :cursor-color
    :initform nil)))

(defun state-cursor-color (state)
  (or (slot-value state 'cursor-color)
      *default-cursor-color*))

(defvar *current-state* nil)
(defvar *current-main-state* nil)

(defun state= (state1 state2)
  (and (typep state1 'vi-state)
       (typep state2 'vi-state)
       (eq (state-name state1) (state-name state2))))

;;; vi-state methods
(defmacro define-state (name direct-super-classes direct-slot-specs &rest options)
  (let ((cleaned-super-classes (if (null direct-super-classes) '(vi-state) direct-super-classes)))
    `(progn
       (assert (find 'vi-state ',cleaned-super-classes :test #'(lambda (expected-class class) (closer-mop:subclassp class expected-class))) () "At least one of the direct-super-classes should be vi-state or a subclass of vi-state!")
       (defclass ,name ,cleaned-super-classes
         ,direct-slot-specs
         ,@options)
       (setf (get ',name 'state)
             (make-instance ',name)))))

(defgeneric pre-command-hook (state)
  (:method ((state vi-state))))

(defgeneric post-command-hook (state)
  (:method ((state vi-state))))

(defgeneric state-enabled-hook (state)
  (:method ((state vi-state))))

(defgeneric state-disabled-hook (state))

(defmethod state-disabled-hook ((state vi-state)))

(defun current-state ()
  *current-state*)

(defun current-main-state ()
  "Same as `current-state` except it returns the previous state insinde `with-temporary-state` macro."
  (or *current-main-state*
      *current-state*))

(defun ensure-state (state)
  (setf state
        (if (symbolp state)
            (get state 'state)
            state))
  (assert (typep state 'vi-state))
  state)

(defgeneric mode-specific-keymaps (mode)
  (:method (mode) nil))

(defmethod compute-keymaps ((mode vi-mode))
  (let* ((buffer (current-buffer))
         (major-mode (ensure-mode-object (buffer-major-mode buffer)))
         (minor-mode-keymaps (loop for mode-name in (buffer-minor-modes buffer)
                                   for mode = (ensure-mode-object mode-name)
                                   when (mode-keymap mode)
                                   collect it)))
    (append minor-mode-keymaps
            (mode-specific-keymaps major-mode)
            ;; Precede state keymaps over major-mode keymaps
            (state-keymaps (ensure-state *current-state*)))))

(defun update-cursor-styles (state)
  (set-attribute 'cursor
                 :background (or (state-cursor-color state) *default-cursor-color*))
  (lem-if:update-cursor-shape (lem:implementation)
                              (state-cursor-type state)))

(defun change-state (name)
  (and *current-state*
       (state-disabled-hook *current-state*))
  (let ((state (ensure-state name)))
    (setf *current-state* state)
    (state-enabled-hook state)
    (update-cursor-styles state)))

(defmacro with-state (state &body body)
  (with-gensyms (old-state)
    `(let ((,old-state (current-state)))
       (change-state ,state)
       (unwind-protect (progn ,@body)
         (change-state ,old-state)))))

(defmacro with-temporary-state (state &body body)
  `(let ((*current-main-state* *current-state*)
         (*current-state* (ensure-state ,state)))
     (update-cursor-styles *current-state*)
     (unwind-protect (progn ,@body)
       (update-cursor-styles *current-main-state*))))

(defun vi-pre-command-hook ()
  (when (mode-active-p (current-buffer) 'vi-mode)
    (pre-command-hook (ensure-state (current-state)))))

(defun vi-post-command-hook ()
  (when (mode-active-p (current-buffer) 'vi-mode)
    (post-command-hook (ensure-state (current-state)))))

(add-hook *pre-command-hook* 'vi-pre-command-hook)
(add-hook *post-command-hook* 'vi-post-command-hook)

(defun vi-this-command-keys ()
  (append
   (and (numberp (universal-argument-of-this-command))
        (map 'list (lambda (char) (lem:make-key :sym (string char)))
             (princ-to-string (universal-argument-of-this-command))))
   (this-command-keys)))

(defun this-motion-command ()
  *this-motion-command*)

(deftype repeat-type () '(member t nil :motion))

(defclass vi-command ()
  ((repeat :type repeat-type
           :initarg :repeat
           :initform nil
           :accessor vi-command-repeat)))

(defclass vi-motion (vi-command)
  ((type :type keyword
         :initarg :type
         :initform :exclusive
         :accessor vi-motion-type)
   (default-n-arg :type (or null integer)
     :initarg :default-n-arg
     :initform 1
     :accessor vi-motion-default-n-arg)))

(defclass vi-operator (vi-command) ())

(defclass vi-text-object (vi-motion) ())

(defstruct (range (:constructor make-range (beginning end &optional type)))
  beginning
  end
  type)

(define-condition operator-abort () ())

(define-condition text-object-abort (operator-abort)
  ((range :initarg :range
          :reader text-object-abort-range)))

(defvar *vi-current-window* nil)

(defun vi-current-window ()
  (or *vi-current-window*
      (lem:current-window)))

(defmacro with-main-window (window &body body)
  `(let ((*vi-current-window* ,window))
     ,@body))

(defstruct (vi-keymap (:include keymap)
                      (:constructor %make-vi-keymap)))

(defun make-vi-keymap (&rest args &key undef-hook parent name)
  (declare (ignore undef-hook parent name))
  (let ((keymap (apply #'%make-vi-keymap args)))
    (push keymap *keymaps*)
    keymap))

(defmacro define-keymap (name &key undef-hook parent)
  `(defvar ,name (make-vi-keymap :name ',name
                                 :undef-hook ,undef-hook
                                 :parent ,parent)))
