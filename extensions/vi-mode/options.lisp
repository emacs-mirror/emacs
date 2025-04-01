(defpackage :lem-vi-mode/options
  (:use :cl
        :lem
        :split-sequence)
  (:import-from :lem-vi-mode/utils
                :change-directory*)
  (:import-from :parse-number
                :parse-number)
  (:import-from :cl-ppcre
                :scan-to-strings
                :register-groups-bind)
  (:import-from :alexandria
                :if-let
                :once-only
                :with-gensyms
                :disjoin
                :mappend
                :copy-hash-table)
  (:export :define-option
           :get-option
           :option
           :option-name
           :option-value
           :option-raw-value
           :option-default
           :option-type
           :option-aliases
           :option-getter
           :option-set-hook
           :option-documentation
           :reset-option-value
           :toggle-option-value
           :execute-set-command))
(in-package :lem-vi-mode/options)

(defstruct option
  (name nil :type string)
  %value
  default
  (type t :type (member t boolean number string list))
  (aliases '() :type list)
  (getter nil :type (or function null))
  (setter nil :type (or function null))
  (set-hook nil :type (or function null))
  (initializer nil :type (or function null))
  (documentation nil :type (or string null)))

(define-condition option-error (simple-error) ())

(defvar *option-scope* (make-hash-table :test 'equal))
(defvar *option-aliases* (make-hash-table :test 'equal))
(defvar *global-options* (make-hash-table :test 'equal))
(defvar *default-buffer-options* (make-hash-table :test 'equal))
(defvar *default-window-options* (make-hash-table :test 'equal))

(defun canonical-option-name (name)
  (or (gethash name *option-aliases*)
      name))

(defun new-buffer-options ()
  (copy-hash-table *default-buffer-options*
                   :key (lambda (option)
                          (let ((new-option (copy-structure option)))
                            (when (option-initializer new-option)
                              (funcall (option-initializer new-option) new-option))
                            new-option))))

(defun new-window-options ()
  (copy-hash-table *default-window-options*
                   :key (lambda (option)
                          (let ((new-option (copy-structure option)))
                            (when (option-initializer new-option)
                              (funcall (option-initializer new-option) new-option))
                            new-option))))

(defun get-buffer-options (&optional (buffer (lem:current-buffer)))
  (or (buffer-value buffer "vi-mode-options")
      (setf (buffer-value buffer "vi-mode-options")
            (new-buffer-options))))

(defun get-window-options (&optional (window (lem:current-window)))
  (or (lem:window-parameter window :vi-mode-options)
      (setf (lem:window-parameter window :vi-mode-options)
            (new-window-options))))

(defun get-global-options ()
  *global-options*)

(defun get-options-by-scope (scope)
  (ecase scope
    (:global (get-global-options))
    (:buffer (get-buffer-options))
    (:window (get-window-options))))

(defun get-option (name &optional (error-if-not-exists t))
  (check-type name string)
  (let* ((name (canonical-option-name name))
         (scope (gethash name *option-scope* :global)))
    (multiple-value-bind (option exists)
        (gethash name (get-options-by-scope scope))
      (when (and (null exists)
                 error-if-not-exists)
        (lem:editor-error "Unknown option: ~A" name))
      option)))

(defun ensure-option (name-or-option &optional (error-if-not-exists t))
  (etypecase name-or-option
    (option name-or-option)
    (string (get-option name-or-option error-if-not-exists))))

(defun option-raw-value (option)
  (option-%value (ensure-option option)))

(defun option-value (option)
  (let ((option (ensure-option option)))
    (values
     (if-let (getter (option-getter option))
       (funcall getter option)
       (option-raw-value option))
     (option-name option))))

(defun (setf option-value) (new-value option)
  (let ((option (ensure-option option)))
    (with-slots (type setter set-hook) option
      (unless (typep new-value type)
        (lem:editor-error "Option '~A' accepts only ~S, but given ~S"
                          (option-name option) type new-value))
      (let ((old-value (option-value option)))
        (handler-case
            (progn
              (if setter
                  (funcall setter new-value option)
                  (setf (option-%value option) new-value))
              (multiple-value-prog1
                  (values (option-value option)
                          (option-name option)
                          old-value
                          t)
                (when set-hook
                  (funcall set-hook new-value))))
          (option-error (e)
            (lem:editor-error (princ-to-string e))))))))

(defun reset-option-value (option)
  (setf (option-value option)
        (option-default option)))

(defun toggle-option-value (option)
  (with-slots (name type) option
    (unless (eq type 'boolean)
      (lem:editor-error "Can't toggle non-boolean option: '~A' (type=~S)" name type)))
  (setf (option-value option)
        (not (option-value option))))

(defun parse-option-string (option-string)
  (coerce
   (nth-value 1
              (ppcre:scan-to-strings "^(no|inv)?([^?!&=:\\+\\-\\^]+)(\\?|\\!|&|(?:\\+|\\-|\\^)?=|:)?(.+)?$" option-string))
   'list))

(defun execute-set-command (option-string)
  (destructuring-bind (&optional prefix option-name suffix new-value)
      (parse-option-string option-string)
    (unless option-name
      (lem:editor-error "Unknown option: ~A" option-string)
      (return-from execute-set-command nil))
    (let ((option (get-option option-name)))
      (cond
        ((equal suffix "?")
         (option-value option))
        ((equal prefix "no")
         (setf (option-value option) nil))
        ((or (equal prefix "inv")
             (equal suffix "!"))
         (toggle-option-value option))
        ((equal suffix "&")
         (reset-option-value option))
        ((member suffix '("=" ":") :test 'equal)
         (setf (option-value option)
               (case (option-type option)
                 (boolean
                  (cond
                    ((string-equal new-value "t") t)
                    ((string-equal new-value "nil") nil)
                    (t new-value)))
                 (number
                  (handler-case (parse-number new-value)
                    (error () new-value)))
                 (string
                  new-value)
                 (list
                  (check-type new-value string)
                  (split-sequence #\, new-value))
                 (otherwise new-value))))
        ((member suffix '("+=" "^=") :test 'equal)
         (ecase (option-type option)
           (list
            (let ((current-value (option-value option)))
              (if (member new-value current-value
                          :test 'equal)
                  (option-value option)
                  (setf (option-value option)
                        (if (string= suffix "+=")
                            (append current-value (list new-value))
                            (cons new-value current-value))))))
           (string
            (setf (option-value option)
                  (if (string= suffix "+=")
                      (concatenate 'string
                                   (option-value option)
                                   new-value)
                      (concatenate 'string
                                   new-value
                                   (option-value option)))))
           (number
            (setf (option-value option)
                  (funcall (if (string= suffix "+=")
                               #'+
                               #'*)
                           (option-value option)
                           new-value)))
           (boolean
            (lem:editor-error "Can't ~A a boolean option: ~A"
                              (if (string= suffix "+=")
                                  "increment"
                                  "multiply")
                              (option-name option)))))
        ((string= suffix "-=")
         (ecase (option-type option)
           (list
            (setf (option-value option)
                  (remove new-value (option-value option)
                          :test 'equal)))
           (string
            (lem:editor-error "Can't subtract a string option: ~A"
                              (option-name option)))
           (number
            (decf (option-value option) new-value))
           (boolean
            (lem:editor-error "Can't decrement a boolean option: ~A"
                              (option-name option)))))
        (t
         (assert (and (null prefix) (null suffix)))
         (if (eq (option-type option) 'boolean)
             (setf (option-value option) t)
             ;; Show the current value for other than boolean
             (option-value option)))))))

(defmacro define-option (name (default &key (type t) aliases (scope :global)) &rest others)
  (check-type name string)
  (check-type scope (member :global :buffer :window))
  (once-only (default scope)
    (with-gensyms (option alias)
      (destructuring-bind (&key getter setter set-hook initializer documentation)
          (mappend (lambda (other-arg)
                     (list (car other-arg) (cdr other-arg)))
                   others)
        `(progn
           (check-type ,default ,type)
           (dolist (,alias ',aliases)
             (setf (gethash ,alias *option-aliases*) ,name))
           (let ((,option
                   (make-option :name ,name
                                :%value ,default
                                :default ,default
                                :type ',type
                                :aliases ',aliases
                                :getter ,(and getter
                                              `(lambda ,@getter))
                                :setter ,(and setter `(lambda ,@setter))
                                :set-hook ,(and set-hook `(lambda ,@set-hook))
                                :initializer ,(and initializer
                                                   `(lambda ,@initializer))
                                :documentation ,(and documentation
                                                     (first documentation)))))
             (setf (gethash
                    ,name
                    (ecase ,scope
                      (:global *global-options*)
                      (:buffer *default-buffer-options*)
                      (:window *default-window-options*)))
                   ,option)
             (setf (gethash ,name *option-scope*) ,scope)
             ',name))))))

(defun auto-change-directory (buffer-or-window)
  (change-directory* (etypecase buffer-or-window
                       (lem:buffer (lem:buffer-directory buffer-or-window))
                       (lem:window (lem:buffer-directory (lem:window-buffer buffer-or-window))))))

(define-option "autochdir" (nil :type boolean :aliases ("acd"))
  (:documentation "Boolean to change the current directory to the buffer's directory automatically.
  Default: nil
  Aliases: acd")
  (:set-hook (new-value)
   (if new-value
       (progn
         (lem:add-hook lem:*find-file-hook* 'auto-change-directory)
         (dolist (window (lem:window-list))
           (lem:add-hook (lem-core::window-switch-to-buffer-hook window) 'auto-change-directory)
           (lem:add-hook (lem-core:window-leave-hook window) 'auto-change-directory)))
       (progn
         (lem:remove-hook lem:*find-file-hook* 'auto-change-directory)
         (dolist (window (lem:window-list))
           (lem:remove-hook (lem-core::window-switch-to-buffer-hook window) 'auto-change-directory)
           (lem:remove-hook (lem-core:window-leave-hook window) 'auto-change-directory))))))

(define-option "number" (nil :type boolean :aliases ("nu"))
  (:documentation "Boolean to show the line number.
  Default: nil
  Aliases: nu")
  (:getter (option)
   (declare (ignore option))
   (lem:variable-value 'lem/line-numbers:line-numbers :global))
  (:set-hook (new-value)
   (setf (lem:variable-value 'lem/line-numbers:line-numbers :global) new-value)))

(defun compile-rules (value option-name)
  (apply #'disjoin
         (mapcar (lambda (rule)
                   (check-type rule string)
                   (cond
                     ((string= rule "@")
                      #'alpha-char-p)
                     ((string= rule "@-@")
                      (lambda (c)
                        (char= c #\@)))
                     (t
                      (or (ppcre:register-groups-bind ((#'parse-integer start) (#'parse-integer end))
                              ("(\\d{2,})-(\\d{2,})" rule)
                            (lambda (c)
                              (<= start (char-code c) end)))
                          (ppcre:register-groups-bind (start end)
                              ("(.)-(.)" rule)
                            (let ((start-code (char-code (aref start 0)))
                                  (end-code (char-code (aref end 0))))
                              (lambda (c)
                                (<= start-code (char-code c) end-code))))
                          (progn
                            (unless (= (length rule) 1)
                              (error 'option-error
                                     :format-control "Invalid rule in ~A: ~A"
                                     :format-arguments (list option-name rule)))
                            (let ((rule-char (aref rule 0)))
                              (lambda (c)
                                (char= c rule-char))))))))
                 value)))

(defgeneric rules-option-init-chars (option-symbol syntax-table))

(defmethod rules-option-init-chars ((option-symbol (eql :iskeyword)) syntax-table)
  (declare (ignorable option-symbol))
  (syntax-table-symbol-chars syntax-table))

(defmethod rules-option-init-chars ((option-symbol (eql :iskeyword))
                                    (syntax-table (eql lem-lisp-syntax:*syntax-table*)))
  (declare (ignorable option-symbol))
  (set-difference (syntax-table-symbol-chars syntax-table)
                  '(#\/ #\. #\: #\-)))

(defmethod rules-option-init-chars ((option-symbol (eql :isseparator)) syntax-table)
  (declare (ignorable option-symbol))
  (syntax-table-space-chars syntax-table))

(defmethod rules-option-init-chars ((option-symbol (eql :isseparator))
                                    (syntax-table (eql lem-lisp-syntax:*syntax-table*)))
  (declare (ignorable option-symbol))
  (union (syntax-table-space-chars syntax-table)
         '(#\( #\) #\")))

(defmacro define-rules-option (name default &key (alias (subseq name 0 3)) doc)
  (once-only (default)
    `(define-option ,name ((cons ,default
                                 (compile-rules ,default ,name))
                           :type list
                           :aliases (,alias)
                           :scope :buffer)
       (:documentation ,doc)
       (:getter (option)
        (car (option-raw-value option)))
       (:setter (new-value option)
        (setf (option-%value option)
              (cons new-value
                    (compile-rules new-value ,name))))
       (:initializer (option)
        (let ((syntax-table (lem:mode-syntax-table (lem:buffer-major-mode (lem:current-buffer)))))
          (setf (option-value option)
                (delete-duplicates
                 (nconc (mapcar (lambda (c)
                                  (if (char= c #\@)
                                      "@-@"
                                      (string c)))
                                (rules-option-init-chars
                                 (intern ,(string-upcase name) "KEYWORD")
                                 syntax-table))
                        (option-value option))
                 :test 'equal)))))))

(define-rules-option "iskeyword" '("@" "48-57" "_" "192-255")
  :doc "Comma-separated string to specify the characters should be recognized as a keyword. (buffer local)
  Default: @,48-57,_,192-255
  Aliases: isk")

(define-rules-option
    "isseparator"
  (mapcar 'string (syntax-table-space-chars (lem:fundamental-syntax-table)))
  :doc "Comma-separated string to specify the characters that should be recognized as a non broad word characters. (buffer local)
  Aliases: iss")

(define-option "scrolloff" (0 :type number :aliases ("so"))
  (:documentation "The minimal number of lines to keep above of below the cursor.
Default: 0
Aliases: so"))

(define-option "ignorecase" (nil :type boolean))
