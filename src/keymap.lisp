(in-package :lem-core)

(defvar *keymaps* nil)

(defvar *special-keymap* nil)

(deftype key-sequence ()
  '(trivial-types:proper-list key))

(defun keyseq-to-string (key-sequence)
  (check-type key-sequence key-sequence)
  (format nil "~{~A~^ ~}" key-sequence))

(defstruct (keymap (:constructor %make-keymap))
  undef-hook
  parent
  (table (make-hash-table :test 'eq))
  (function-table (make-hash-table :test 'eq))
  name)

(defmethod print-object ((object keymap) stream)
  (print-unreadable-object (object stream :identity t :type t)
    (when (keymap-name object)
      (princ (keymap-name object) stream))))

(defun make-keymap (&key undef-hook parent name)
  (let ((keymap (%make-keymap
                 :undef-hook undef-hook
                 :parent parent
                 :name name)))
    (push keymap *keymaps*)
    keymap))

(defun prefix-command-p (command)
  (hash-table-p command))

(defun define-key (keymap keyspec command-name)
  "Bind a command COMMAND-NAME to a KEYSPEC in a KEYMAP.

Global bindings use `*global-keymap*' as KEYMAP argument.

If KEYSPEC argument is a `string', valid prefixes are:
H (Hyper), S (Super), M (Meta), C (Ctrl), Shift

Example: (define-key *global-keymap* \"C-'\" 'list-modes)"
  (check-type keyspec (or symbol string))
  (check-type command-name (or symbol keymap))
  (typecase keyspec
    (symbol
     (setf (gethash keyspec (keymap-function-table keymap))
           command-name))
    (string
     (let ((keys (parse-keyspec keyspec)))
       (define-key-internal keymap keys command-name))))
  (values))

(defmacro define-keys (keymap &body bindings)
  `(progn ,@(mapcar
             (lambda (binding)
               `(define-key ,keymap
                  ,(first binding)
                  ,(second binding)))
             bindings)))

(defun define-key-internal (keymap keys symbol)
  (loop :with table := (keymap-table keymap)
        :for rest :on (uiop:ensure-list keys)
        :for k := (car rest)
        :do (cond ((null (cdr rest))
                   (setf (gethash k table) symbol))
                  (t
                   (let ((next (gethash k table)))
                     (if (and next (prefix-command-p next))
                         (setf table next)
                         (let ((new-table (make-hash-table :test 'eq)))
                           (setf (gethash k table) new-table)
                           (setf table new-table))))))))

(defun undefine-key (keymap keyspec)
  "Remove a binding for a KEYSPEC in a KEYMAP.

If KEYSPEC argument is a `string', valid prefixes are:
H (Hyper), S (Super), M (Meta), C (Ctrl), Shift

Example: (undefine-key *paredit-mode-keymap* \"C-k\")"
  (check-type keyspec (or symbol string))
  (typecase keyspec
    (symbol
     (remhash keyspec (keymap-function-table keymap)))
    (string
     (let ((keys (parse-keyspec keyspec)))
       (undefine-key-internal keymap keys))))
  (values))

(defmacro undefine-keys (keymap &body bindings)
  `(progn ,@(mapcar
             (lambda (binding)
               `(undefine-key ,keymap
                              ,(first binding)))
             bindings)))

(defun undefine-key-internal (keymap keys)
  (loop :with table := (keymap-table keymap)
        :for rest :on (uiop:ensure-list keys)
        :for k := (car rest)
        :do (cond ((null (cdr rest))
                   (remhash k table))
                  (t
                   (let ((next (gethash k table)))
                     (when (prefix-command-p next)
                       (setf table next)))))))

(defun parse-keyspec (string)
  (labels ((fail ()
             (editor-error "parse error: ~A" string))
           (parse (str)
             (loop :with ctrl :and meta :and super :and hyper :and shift
                   :do (cond
                         ((ppcre:scan "^[cmshCMSH]-" str)
                          (ecase (char-downcase (char str 0))
                            ((#\c) (setf ctrl t))
                            ((#\m) (setf meta t))
                            ((#\s) (setf super t))
                            ((#\h) (setf hyper t)))
                          (setf str (subseq str 2)))
                         ((ppcre:scan "^[sS]hift-" str)
                          (setf shift t)
                          (setf str (subseq str 6)))
                         ((string= str "")
                          (fail))
                         ((and (not (insertion-key-sym-p str))
                               (not (named-key-sym-p str)))
                          (fail))
                         (t
                          (return (make-key :ctrl ctrl
                                            :meta meta
                                            :super super
                                            :hyper hyper
                                            :shift shift
                                            :sym (or (named-key-sym-p str)
                                                     str))))))))
    (mapcar #'parse (uiop:split-string string :separator " "))))

(defun traverse-keymap (keymap fun)
  (labels ((f (table prefix)
             (maphash (lambda (k v)
                        (cond ((prefix-command-p v)
                               (f v (cons k prefix)))
                              ((keymap-p v)
                               (f (keymap-table v) (cons k prefix)))
                              (t (funcall fun (reverse (cons k prefix)) v))))
                      table)))
    (f (keymap-table keymap) nil)))

(defgeneric keymap-find-keybind (keymap key cmd)
  (:method ((keymap t) key cmd)
    (let ((table (keymap-table keymap)))
      (labels ((f (k)
                 (let ((cmd (gethash k table)))
                   (cond ((prefix-command-p cmd)
                          (setf table cmd))
                         ((keymap-p cmd)
                          (setf table (keymap-table cmd)))
                         (t cmd)))))
        (let ((parent (keymap-parent keymap)))
          (when parent
            (setf cmd (keymap-find-keybind parent key cmd))))
        (or (etypecase key
              (key
               (f key))
              (list
               (let (cmd)
                 (dolist (k key)
                   (unless (setf cmd (f k))
                     (return)))
                 cmd)))
            (gethash cmd (keymap-function-table keymap))
            (keymap-undef-hook keymap)
            cmd)))))

(defun insertion-key-p (key)
  (let* ((key (typecase key
                (list (first key))
                (otherwise key)))
         (sym (key-sym key)))
    (cond ((match-key key :sym "Return") #\Return)
          ((match-key key :sym "Tab") #\Tab)
          ((match-key key :sym "Space") #\Space)
          ((and (insertion-key-sym-p sym)
                (match-key key :sym sym))
           (char sym 0)))))

(defgeneric compute-keymaps (global-mode)
  (:method ((mode global-mode)) nil))

(defun all-keymaps ()
  (let* ((keymaps (compute-keymaps (current-global-mode)))
         (keymaps
           (append keymaps
                   (alexandria:when-let* ((mode (major-mode-at-point (current-point)))
                                          (keymap (mode-keymap mode)))
                     (list keymap))
                   (loop :for mode :in (all-active-modes (current-buffer))
                         :when (mode-keymap mode)
                         :collect :it))))
    (when *special-keymap*
      (push *special-keymap* keymaps))
    (delete-duplicates (nreverse keymaps))))

(defun lookup-keybind (key &key (keymaps (all-keymaps)))
  (let (cmd)
    (loop :for keymap :in keymaps
          :do (setf cmd (keymap-find-keybind keymap key cmd)))
    cmd))

(defun find-keybind (key)
  (let ((cmd (lookup-keybind key)))
    (when (symbolp cmd)
      cmd)))

(defun collect-command-keybindings (command keymap)
  (let ((bindings '()))
    (traverse-keymap keymap
                     (lambda (kseq cmd)
                       (when (eq cmd command)
                         (push kseq bindings))))
    (nreverse bindings)))

(defvar *abort-key*)

(defun abort-key-p (key)
  (and (key-p key)
       (eq *abort-key* (lookup-keybind key))))

(defmacro with-special-keymap ((keymap) &body body)
  `(let ((*special-keymap* (or ,keymap *special-keymap*)))
     ,@body))
