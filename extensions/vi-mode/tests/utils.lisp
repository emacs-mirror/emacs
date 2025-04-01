(defpackage :lem-vi-mode/tests/utils
  (:use :cl
        :lem)
  (:shadow :with-current-buffer)
  (:import-from :rove
                :form-description
                :diag
                :testing)
  (:import-from :lem-core
                :*this-command-keys*
                :*input-hook*)
  (:import-from :lem-vi-mode/core
                :vi-mode
                :current-state
                :ensure-state)
  (:import-from :lem-vi-mode/states
                :normal
                :insert)
  (:import-from :lem-vi-mode/visual
                :visual-char
                :visual-line
                :visual-block
                :apply-visual-range
                :clear-visual-overlays)
  (:import-from :cl-ppcre)
  (:import-from :alexandria
                :remove-from-plistf
                :remove-from-plist
                :appendf
                :if-let
                :once-only
                :with-gensyms)
  (:export :with-vi-buffer
           :with-test-buffer
           :cmd
           :ex-cmd
           :pos=
           :text=
           :state=
           :visual=
           :buf=
           :lines))
(in-package :lem-vi-mode/tests/utils)

(defun state-to-keyword (state)
  (etypecase state
    (normal :normal)
    (insert :insert)
    (visual-char :visual)
    (visual-line :visual-line)
    (visual-block :visual-block)))

(defun keyword-to-state (state-keyword)
  (check-type state-keyword keyword)
  (ecase state-keyword
    (:normal 'normal)
    (:insert 'insert)
    (:visual 'visual-char)
    (:visual-line 'visual-line)
    (:visual-block 'visual-block)))

(defun parse-buffer-string (buffer-string)
  (let ((offset 0)
        cursor
        vstart
        visual-regions)
    (ppcre:do-matches (s e "(?<!\\\\)(\\[|\\]|<|>)" buffer-string)
      (let ((char (aref buffer-string s)))
        (ecase char
          (#\[
           (assert (null cursor))
           (let ((len (length buffer-string)))
             (assert
              (or (and (< e len)
                       (char= (aref buffer-string e) #\]))
                  (and (< (1+ e) len)
                       (char= (aref buffer-string (1+ e)) #\]))))
             (setf cursor (- e offset))))
          (#\]
           (assert cursor))
          (#\<
           (assert (null vstart))
           (setf vstart (- e offset)))
          (#\>
           (assert vstart)
           (push (cons vstart (- e offset)) visual-regions)
           (setf vstart nil)))
        (incf offset)))
    (values (ppcre:regex-replace-all "(?<!\\\\)(\\[|\\]|<|>)"
                                     buffer-string
                                     "")
            cursor
            (nreverse visual-regions))))

(defun %make-buffer-string (buffer-text buffer-pos)
  (check-type buffer-pos (integer 0))
  (let ((state (current-state)))
    (let ((buf-str
            (apply #'concatenate 'string
                   (subseq buffer-text 0 (1- buffer-pos))
                   (case state
                     (insert
                      (list
                       "[]"
                       (subseq buffer-text (1- buffer-pos))))
                     (otherwise
                      (if (< (length buffer-text) buffer-pos)
                          (list "[]")
                          (list
                           (format nil "[~C]" (aref buffer-text (1- buffer-pos)))
                           (subseq buffer-text buffer-pos))))))))
      (if (lem-vi-mode/visual:visual-p)
          (let ((read-pos 0))
            (concatenate
             'string
             (with-output-to-string (s)
               (apply-visual-range
                (lambda (start end)
                  (write-string buf-str s
                                :start read-pos
                                :end (1- (if (< buffer-pos (position-at-point start))
                                             (+ (position-at-point start) 2)
                                             (position-at-point start))))
                  (write-char #\< s)
                  (write-string buf-str s
                                :start (1- (if (< buffer-pos (position-at-point start))
                                               (+ (position-at-point start) 2)
                                               (position-at-point start)))
                                :end (1- (if (< buffer-pos (position-at-point end))
                                             (+ (position-at-point end) 2)
                                             (position-at-point end))))
                  (write-char #\> s)
                  (setf read-pos
                        (if (< buffer-pos (position-at-point end))
                            (+ (position-at-point end) 2)
                            (position-at-point end))))))
             (subseq buf-str (1- read-pos))))
          buf-str))))

(defun make-buffer-string (buffer)
  (%make-buffer-string (buffer-text buffer)
                       (position-at-point (buffer-point buffer))))

(defun text-backslashed (text)
  (ppcre:regex-replace-all "[\\n\\r\\t]" text
                           (lambda (matches)
                             (ecase (aref matches 0)
                               (#\Newline "\\n")
                               (#\Return "\\r")
                               (#\Tab "\\t")))
                           :simple-calls t))

(defun parse-command-keys (keys-string)
  (check-type keys-string string)
  (let (keys)
    (ppcre:do-matches-as-strings (key-str "(?<!\\\\)<[^\\>]+?>|." keys-string)
      (push
       (if (= (length key-str) 1)
           (make-key :sym key-str)
           (let (key-args)
             (ppcre:do-register-groups (modifier)
                 ("(H|S|M|C|Shift)-" key-str)
               (cond
                 ((string= modifier "H")
                  (appendf key-args '(:hyper t)))
                 ((string= modifier "S")
                  (appendf key-args '(:super t)))
                 ((string= modifier "M")
                  (appendf key-args '(:meta t)))
                 ((string= modifier "C")
                  (appendf key-args '(:ctrl t)))
                 ((string= modifier "Shift")
                  (appendf key-args '(:shift t)))))
             (let ((sym-str (ppcre:scan-to-strings "[^<-]+(?=>$)" key-str)))
               (apply #'make-key :sym (if-let (char (name-char sym-str))
                                        (case char
                                          (#\Esc "Escape")
                                          (#\Return "Return")
                                          (#\Space "Space")
                                          (#\Tab "Tab")
                                          (otherwise (string char)))
                                        sym-str)
                      key-args))))
       keys))
    (nreverse keys)))

(defun make-test-buffer (name &rest buffer-args
                              &key content (temporary t temporary-specified-p)
                              &allow-other-keys)
  (declare (ignore temporary))
  (unless temporary-specified-p
    (setf (getf buffer-args :temporary) t))
  (remove-from-plistf buffer-args :name :content)

  (let ((buffer (apply #'make-buffer name buffer-args)))
    (when content
      (multiple-value-bind (buffer-text position visual-regions)
          (parse-buffer-string content)
        (let ((point (buffer-point buffer)))
          (lem:insert-string point buffer-text)
          (lem:clear-buffer-edit-history buffer)
          (when position
            (move-to-position point position))
          (when visual-regions
            (let ((top-left-pos (car (first visual-regions)))
                  (bot-right-pos (cdr (car (last visual-regions)))))
              (with-point ((p point))
                (move-to-position p
                                  (if (= position top-left-pos)
                                      (1- bot-right-pos)
                                      top-left-pos))
                (setf lem-vi-mode/visual::*start-point* p))))
          (dolist (region visual-regions)
            (destructuring-bind (from . to) region
              (with-point ((start point)
                           (end point))
                (move-to-position start from)
                (move-to-position end to)
                (push (lem:make-overlay start end 'lem:region)
                      lem-vi-mode/visual::*visual-overlays*)))))))
    buffer))

(defmacro with-test-buffer ((var buffer-content
                             &rest buffer-args
                             &key name
                             &allow-other-keys)
                            &body body)
  (remove-from-plistf buffer-args :name)
  `(let ((,var (make-test-buffer ,name
                                 :content ,buffer-content
                                 ,@buffer-args)))
     ,@body))

(defun call-with-current-buffer (buffer fn)
  (lem:with-current-buffers ()
    (lem:with-current-buffer buffer
      (let ((window (lem:current-window)))
        (lem-core::set-window-buffer buffer window)
        (lem-core::set-window-view-point (lem:copy-point (lem:buffer-point buffer))
                                         window)
        (lem-core::set-window-point (lem:buffer-point buffer) window)
        (funcall fn)))))

(defmacro with-current-buffer ((buffer) &body body)
  `(call-with-current-buffer
    ,buffer
    (lambda () ,@body)))

(defmacro with-vi-state ((state) &body body)
  (once-only (state)
    `(if ,state
         (let ((lem-vi-mode/core::*current-state* nil))
           (lem-vi-mode/core::change-state (if (keywordp ,state)
                                               (keyword-to-state ,state)
                                               ,state))
           ,@body)
         (progn ,@body))))

(defun call-with-vi-buffer (buffer state fn)
  (with-current-buffer (buffer)
    (let ((state (or state
                     (if lem-vi-mode/visual::*visual-overlays*
                         'visual-char
                         (current-state))))
          (voverlay lem-vi-mode/visual::*visual-overlays*)
          (start (and lem-vi-mode/visual::*start-point*
                      (copy-point lem-vi-mode/visual::*start-point*))))
      (lem-core:change-buffer-mode buffer 'vi-mode)
      (with-vi-state (state)
        (setf lem-vi-mode/visual::*visual-overlays* voverlay
              lem-vi-mode/visual::*start-point* start)
        (testing (format nil "[buf] \"~A\""
                         (text-backslashed
                          (make-buffer-string (current-buffer))))
                 (funcall fn))))
    (clear-visual-overlays)))

(defun ensure-buffer (buffer-or-string
                      &rest buffer-args
                      &key name
                      &allow-other-keys)
  (etypecase buffer-or-string
    (string (apply #'make-test-buffer
                   name
                   :content buffer-or-string
                   (remove-from-plist buffer-args :name)))
    (buffer buffer-or-string)))

(defmacro with-vi-buffer ((buffer-or-string
                          &rest buffer-args
                          &key state
                          &allow-other-keys) &body body)
  (remove-from-plistf buffer-args :state)
  (with-gensyms (buffer)
    (once-only (buffer-or-string)
      `(let ((,buffer (ensure-buffer ,buffer-or-string ,@buffer-args)))
         (call-with-vi-buffer
          ,buffer
          ,state
          (lambda () ,@body))))))

(defun point-coord (point)
  (values (line-number-at-point point)
          (point-charpos point)))

(defun cmd (keys)
  (check-type keys string)
  (diag (format nil "[cmd] ~A~%" keys))
  (let ((*input-hook* (cons (cons (lambda (event)
                                    (push event *this-command-keys*))
                                  0)
                            *input-hook*)))
    (execute-key-sequence
     (parse-command-keys keys))))

(defun ex-cmd (command)
  (check-type command string)
  (diag (format nil "[ex-cmd] ~A~%" command))
  (lem-vi-mode/ex::execute-ex command))

(defun pos= (expected-point)
  (point= (current-point) expected-point))

(defun text= (expected-buffer-text)
  (string= (buffer-text (current-buffer))
           expected-buffer-text))

(defun state= (expected-state)
  (eq expected-state
      (state-to-keyword (current-state))))

(defun visual= (visual-regions)
  (let (current-regions)
    (apply-visual-range
     (lambda (start end)
       (push
        (cons (position-at-point start)
              (position-at-point end))
        current-regions)))
    (equalp (nreverse current-regions) visual-regions)))

(defun buf= (expected-buffer-string)
  (check-type expected-buffer-string string)
  (multiple-value-bind (expected-buffer-text expected-position visual-regions)
      (parse-buffer-string expected-buffer-string)
    (unless expected-position
      (error "No cursor is on the expected buffer"))
    (with-point ((p (current-point)))
      (move-to-position p expected-position)
      (and (text= expected-buffer-text)
           (pos= p)
           (visual= visual-regions)))))

(defmethod form-description ((function (eql 'lem:point=)) args values &key negative)
  (multiple-value-bind (expected-line expected-col)
      (point-coord (second values))
    (multiple-value-bind (actual-line actual-col)
        (point-coord (first values))
      (format nil "Expect ~W~:[~; not~] to be at (~D, ~D), but at (~D, ~D)"
              (first args)
              negative
              expected-line expected-col
              actual-line actual-col))))

(defmethod form-description ((function (eql 'pos=)) args values &key negative)
  (form-description 'point=
                    (cons '(current-point) args)
                    (cons (current-point) values)
                    :negative negative))

(defmethod form-description ((function (eql 'text=)) args values &key negative)
  (declare (ignore args))
  (let ((expected-text (first values))
        (actual-text (buffer-text (current-buffer))))
    (format nil "Expect the buffer text~:[~; not~] to be ~S (actual: ~S)"
            negative
            (text-backslashed expected-text)
            (text-backslashed actual-text))))

(defmethod form-description ((function (eql 'buf=)) args values &key negative)
  (declare (ignore args))
  (format nil "Expect the buffer~:[~; not~] to be \"~A\"~@[ (actual: \"~A\")~]"
          negative
          (text-backslashed (first values))
          ;; NOTE: For the older versions of Rove that doesn't cache the assertion description
          (ignore-errors
            (text-backslashed
              (make-buffer-string (current-buffer))))))

(defmethod form-description ((function (eql 'state=)) args values &key negative)
  (declare (ignore args))
  (format nil "Expect the vi state~:[~; not~] to be ~A~@[ (actual: ~A)~]"
          negative
          (first values)
          (ignore-errors (state-to-keyword (current-state)))))

(defun lines (&rest lines)
  (format nil "~{~A~%~}" lines))
