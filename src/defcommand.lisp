(in-package :lem-core)

(defvar *editor-warnings* '())

(eval-when (:compile-toplevel :load-toplevel)
  (defun editor-warning (fmt &rest args)
    (push (apply #'format nil fmt args) *editor-warnings*)
    (values))

  (defun parse-arg-descriptors (arg-descriptors universal-argument)
    "Parse arg descriptors given to define-command.

Descriptors (old char in parenthesis):

:universal (p) -> universal argument, defaults to 1. Don't prompt for anything.
:universal-nil (P) -> universal argument
:string (s) -> prompt for string
:number (n) -> prompt for integer
:buffer (b) -> prompt for a buffer, defaults to the current-buffer's name
:other-buffer (B) -> prompt for buffer, defaults to the other buffer's name
:file (f) -> prompt for a file, defaults to the buffer directory
:new-file (F) -> prompt for a file, defaults to the buffer directory, must not be existing
:region (r) -> operate on the region.
:splice -> splice in your own code returning a value or list of values"
    (let* ((pre-forms '())
           (forms
             (mapcar
              (lambda (arg-descriptor)
                (setf arg-descriptor (cond ((and (stringp arg-descriptor)
                                                 (< 0 (length arg-descriptor)))
                                            (editor-warning "define-command: Deprecated expression (~A) is used for arg-descriptor" arg-descriptor)
                                            (list (ecase (char arg-descriptor 0)
                                                    (#\p :universal) (#\P :universal-nil)
                                                    (#\s :string) (#\n :number)
                                                    (#\b :buffer) (#\B :other-buffer)
                                                    (#\f :file) (#\F :new-file)
                                                    (#\r :region))
                                                  (subseq arg-descriptor 1)))
                                           ((symbolp arg-descriptor)
                                            (list arg-descriptor))
                                           (t arg-descriptor)))
                (or (and (consp arg-descriptor)
                         (case (first arg-descriptor)
                           (:splice
                            (assert (alexandria:length= arg-descriptor 2))
                            (second arg-descriptor))
                           ((:universal :universal-1) `(list (or ,universal-argument 1)))
                           (:universal-nil `(list ,universal-argument))
                           (:string `(list (prompt-for-string ,(second arg-descriptor))))
                           (:number
                            `(list (prompt-for-integer ,(second arg-descriptor))))
                           (:buffer
                            `(list (prompt-for-buffer
                                    ,(second arg-descriptor)
                                    :default (buffer-name (current-buffer))
                                    :existing t)))
                           (:other-buffer
                            `(list (prompt-for-buffer ,(second arg-descriptor)
                                                      :default (buffer-name (other-buffer))
                                                      :existing nil)))
                           (:file
                            `(list (prompt-for-file
                                    ,(second arg-descriptor)
                                    :directory (buffer-directory)
                                    :default nil
                                    :existing t)))
                           (:new-file
                            `(list (prompt-for-file
                                    ,(second arg-descriptor)
                                    :directory (buffer-directory)
                                    :default nil
                                    :existing nil)))
                           (:region
                            (push '(check-marked) pre-forms)
                            '(list (region-beginning-using-global-mode
                                    (current-global-mode))
                              (region-end-using-global-mode (current-global-mode))))))
                    `(multiple-value-list ,arg-descriptor)))
              arg-descriptors)))
      (if (null pre-forms)
          `(append ,@forms)
          `(progn
             ,@pre-forms
             (append ,@forms)))))

  (alexandria:with-unique-names (arguments)
    (defun gen-defcommand-body (fn-name
                                universal-argument
                                arg-descriptors)
      `(block ,fn-name
         (destructuring-bind (&rest ,arguments)
             ,(parse-arg-descriptors arg-descriptors universal-argument)
           (apply #',fn-name ,arguments))))))

(defun check-already-defined-command (name source-location)
  #+sbcl
  (alexandria:when-let* ((command (get-command name))
                         (command-source-location (command-source-location command)))
    (unless (equal (sb-c:definition-source-location-namestring command-source-location)
                   (sb-c:definition-source-location-namestring source-location))
      (cerror "continue"
              "~A is already defined in another file ~A"
              name
              (sb-c:definition-source-location-namestring (command-source-location command))))))

(defun register-command (command &key mode-name command-name)
  (when mode-name
    (associate-command-with-mode mode-name command))
  (add-command command-name command))

(defmacro define-command (name-and-options params (&rest arg-descriptors) &body body)
  "Define an interactive command that is callable with M-x.

Example:

(define-command write-hello () ()
  (insert-string (current-point) \"hello\"))

A command can accept an universal argument. Use the :universal or \"p\" descriptor and add a parameter.

Example:

(define-command write-hellos (n) (:universal)
  (dotimes (i n)
    (insert-string (current-point) \"hello \")))

and call it with C-u 3 M-x write-hellos RET.

With :universal the argument defaults to 1, while :universal-nil (\"P\") is
similar and defaults to nil.

Other argument descriptors are available (old-style char in parenthesis):

   :string (s) -> prompt for a string. Use (:string \"My prompt\") or
                  \"sMy prompt: \" to give a custom prompt.
   :number (n) -> prompt for an integer.
   :buffer (b) -> prompt for a buffer, defaults to the current-buffer's name.
   :other-buffer (B) -> prompt for buffer, defaults to the other buffer's name.
   :file (f) -> prompt for a file, defaults to the buffer directory.
   :new-file (F) -> prompt for a file, defaults to the buffer directory, must
                    not be existing.
   :region (r) -> operate on the region. Needs two arguments, the `start` and
                  `end` positions of the region.
   :splice -> splice in your own code returning a list of values."
  (destructuring-bind (name . options) (uiop:ensure-list name-and-options)
    (let ((advice-classes (alexandria:assoc-value options :advice-classes))
          (class-name (alexandria:if-let (elt (assoc :class options))
                        (second elt)
                        name))
          (command-name (alexandria:if-let (elt (assoc :name options))
                          (second elt)
                          (string-downcase name)))
          (mode-name (second (assoc :mode options)))
          (initargs (rest (assoc :initargs options))))

      (check-type command-name string)
      (check-type mode-name (or null symbol))
      (check-type initargs list)

      (alexandria:with-unique-names (command universal-argument)
        `(progn
           (check-already-defined-command ',name
                                          #+sbcl (sb-c:source-location)
                                          #-sbcl nil)

           (defun ,name ,params
             ;; If you call it directly instead of using `call-command`:
             ;;   - *this-command* will not be bound
             ;;   - the execute-hook will not be called
             ,@body)

           (register-command-class ',name ',class-name)
           (defclass ,class-name (primary-command ,@advice-classes)
             ()
             (:default-initargs
              :source-location #+sbcl (sb-c:source-location) #-sbcl nil
              :name ',name
              ,@initargs))

           (defmethod execute (mode (,command ,class-name) ,universal-argument)
             (declare (ignorable ,universal-argument))
             ,(gen-defcommand-body name
                                   universal-argument
                                   arg-descriptors))

           (register-command (make-instance ',class-name)
                             :mode-name ',mode-name
                             :command-name ,command-name))))))

#|
(defclass foo-advice () ())

(define-command (foo-1 (:advice-classes foo-advice)) (p) ("p")
...body)

(define-command (foo-2 (:advice-classes foo-advice)) (s) ("sInput: ")
...body)

(defmethod execute (mode (command foo-advice) argument)
;; :advice-classesをfoo-adviceにしたfoo-1とfoo-2コマンドだけが呼び出される
)
|#
