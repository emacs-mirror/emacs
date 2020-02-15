;;; cl-print.el --- CL-style generic printing  -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2020 Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Keywords:
;; Version: 1.0
;; Package-Requires: ((emacs "25"))

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

;; Customizable print facility.
;;
;; The heart of it is the generic function `cl-print-object' to which you
;; can add any method you like.
;;
;; The main entry point is `cl-prin1'.

;;; Code:

(require 'button)

(defvar cl-print-readably nil
  "If non-nil, try and make sure the result can be `read'.")

(defvar cl-print--number-table nil)
(defvar cl-print--currently-printing nil)
(defvar cl-print--depth nil
  "Depth of recursion within cl-print functions.
Compared to `print-level' to determine when to stop recursing.")


;;;###autoload
(cl-defgeneric cl-print-object (object stream)
  "Dispatcher to print OBJECT on STREAM according to its type.
You can add methods to it to customize the output.
But if you just want to print something, don't call this directly:
call other entry points instead, such as `cl-prin1'."
  ;; This delegates to the C printer.  The C printer will not call us back, so
  ;; we should only use it for objects which don't have nesting.
  (prin1 object stream))

(cl-defgeneric cl-print-object-contents (_object _start _stream)
  "Dispatcher to print the contents of OBJECT on STREAM.
Print the contents starting with the item at START, without
delimiters."
  ;; Every cl-print-object method which can print an ellipsis should
  ;; have a matching cl-print-object-contents method to expand an
  ;; ellipsis.
  (error "Missing cl-print-object-contents method"))

(cl-defmethod cl-print-object ((object cons) stream)
  (if (and cl-print--depth (natnump print-level)
           (> cl-print--depth print-level))
      (cl-print-insert-ellipsis object 0 stream)
    (let ((car (pop object))
          (count 1))
      (if (and print-quoted
               (memq car '(\, quote function \` \,@ \,.))
               (consp object)
               (null (cdr object)))
          (progn
            (princ (cond
                    ((eq car 'quote) '\')
                    ((eq car 'function) "#'")
                    (t car))
                   stream)
            (cl-print-object (car object) stream))
        (princ "(" stream)
        (cl-print-object car stream)
        (while (and (consp object)
                    (not (cond
                          (cl-print--number-table
                           (numberp (gethash object cl-print--number-table)))
                          ((memq object cl-print--currently-printing))
                          (t (push object cl-print--currently-printing)
                             nil))))
          (princ " " stream)
          (if (or (not (natnump print-length)) (> print-length count))
              (cl-print-object (pop object) stream)
            (cl-print-insert-ellipsis object print-length stream)
            (setq object nil))
          (cl-incf count))
        (when object
          (princ " . " stream) (cl-print-object object stream))
        (princ ")" stream)))))

(cl-defmethod cl-print-object-contents ((object cons) _start stream)
  (let ((count 0))
    (while (and (consp object)
                (not (cond
                      (cl-print--number-table
                       (numberp (gethash object cl-print--number-table)))
                      ((memq object cl-print--currently-printing))
                      (t (push object cl-print--currently-printing)
                         nil))))
      (unless (zerop count)
        (princ " " stream))
      (if (or (not (natnump print-length)) (> print-length count))
          (cl-print-object (pop object) stream)
        (cl-print-insert-ellipsis object print-length stream)
        (setq object nil))
      (cl-incf count))
    (when object
      (princ " . " stream) (cl-print-object object stream))))

(cl-defmethod cl-print-object ((object vector) stream)
  (if (and cl-print--depth (natnump print-level)
           (> cl-print--depth print-level))
      (cl-print-insert-ellipsis object 0 stream)
    (princ "[" stream)
    (let* ((len (length object))
           (limit (if (natnump print-length)
                      (min print-length len) len)))
      (dotimes (i limit)
        (unless (zerop i) (princ " " stream))
        (cl-print-object (aref object i) stream))
      (when (< limit len)
        (princ " " stream)
        (cl-print-insert-ellipsis object limit stream)))
    (princ "]" stream)))

(cl-defmethod cl-print-object-contents ((object vector) start stream)
  (let* ((len (length object))
         (limit (if (natnump print-length)
                    (min (+ start print-length) len) len))
         (i start))
    (while (< i limit)
      (unless (= i start) (princ " " stream))
      (cl-print-object (aref object i) stream)
      (cl-incf i))
    (when (< limit len)
      (princ " " stream)
      (cl-print-insert-ellipsis object limit stream))))

(cl-defmethod cl-print-object ((object hash-table) stream)
  (princ "#<hash-table " stream)
  (princ (hash-table-test object) stream)
  (princ " " stream)
  (princ (hash-table-count object) stream)
  (princ "/" stream)
  (princ (hash-table-size object) stream)
  (princ (format " %#x" (sxhash object)) stream)
  (princ ">" stream))

(define-button-type 'help-byte-code
  'follow-link t
  'action (lambda (button)
            (disassemble (button-get button 'byte-code-function)))
  'help-echo (purecopy "mouse-2, RET: disassemble this function"))

(defvar cl-print-compiled nil
  "Control how to print byte-compiled functions.
Acceptable values include:
- `static' to print the vector of constants.
- `disassemble' to print the disassembly of the code.
- nil to skip printing any details about the code.")

(defvar cl-print-compiled-button t
  "Control how to print byte-compiled functions into buffers.
When the stream is a buffer, make the bytecode part of the output
into a button whose action shows the function's disassembly.")

(autoload 'disassemble-1 "disass")

(cl-defmethod cl-print-object ((object compiled-function) stream)
  (unless stream (setq stream standard-output))
  ;; We use "#f(...)" rather than "#<...>" so that pp.el gives better results.
  (princ "#f(compiled-function " stream)
  (let ((args (help-function-arglist object 'preserve-names)))
    (if args
        (prin1 args stream)
      (princ "()" stream)))
  (pcase (help-split-fundoc (documentation object 'raw) object)
    ;; Drop args which `help-function-arglist' already printed.
    (`(,_usage . ,(and doc (guard (stringp doc))))
     (princ " " stream)
     (prin1 doc stream)))
  (let ((inter (interactive-form object)))
    (when inter
      (princ " " stream)
      (cl-print-object
       (if (eq 'byte-code (car-safe (cadr inter)))
           `(interactive ,(make-byte-code nil (nth 1 (cadr inter))
                                          (nth 2 (cadr inter))
                                          (nth 3 (cadr inter))))
         inter)
       stream)))
  (if (eq cl-print-compiled 'disassemble)
      (princ
       (with-temp-buffer
         (insert "\n")
         (disassemble-1 object 0)
         (buffer-string))
       stream)
    (princ " " stream)
    (let ((button-start (and cl-print-compiled-button
                             (bufferp stream)
                             (with-current-buffer stream (point)))))
      (princ (format "#<bytecode %#x>" (sxhash object)) stream)
      (when (eq cl-print-compiled 'static)
        (princ " " stream)
        (cl-print-object (aref object 2) stream))
      (when button-start
        (with-current-buffer stream
          (make-text-button button-start (point)
                            :type 'help-byte-code
                            'byte-code-function object)))))
  (princ ")" stream))

;; This belongs in nadvice.el, of course, but some load-ordering issues make it
;; complicated: cl-generic uses macros from cl-macs and cl-macs uses advice-add
;; from nadvice, so nadvice needs to be loaded before cl-generic and hence
;; can't use cl-defmethod.
(cl-defmethod cl-print-object :extra "nadvice"
              ((object compiled-function) stream)
  (if (not (advice--p object))
      (cl-call-next-method)
    (princ "#f(advice-wrapper " stream)
    (when (fboundp 'advice--where)
      (princ (advice--where object) stream)
      (princ " " stream))
    (cl-print-object (advice--cdr object) stream)
    (princ " " stream)
    (cl-print-object (advice--car object) stream)
    (let ((props (advice--props object)))
      (when props
        (princ " " stream)
        (cl-print-object props stream)))
    (princ ")" stream)))

(cl-defmethod cl-print-object ((object cl-structure-object) stream)
  (if (and cl-print--depth (natnump print-level)
           (> cl-print--depth print-level))
      (cl-print-insert-ellipsis object 0 stream)
    (princ "#s(" stream)
    (let* ((class (cl-find-class (type-of object)))
           (slots (cl--struct-class-slots class))
           (len (length slots))
           (limit (if (natnump print-length)
                      (min print-length len) len)))
      (princ (cl--struct-class-name class) stream)
      (dotimes (i limit)
        (let ((slot (aref slots i)))
          (princ " :" stream)
          (princ (cl--slot-descriptor-name slot) stream)
          (princ " " stream)
          (cl-print-object (aref object (1+ i)) stream)))
      (when (< limit len)
        (princ " " stream)
        (cl-print-insert-ellipsis object limit stream)))
    (princ ")" stream)))

(cl-defmethod cl-print-object-contents ((object cl-structure-object) start stream)
  (let* ((class (cl-find-class (type-of object)))
         (slots (cl--struct-class-slots class))
         (len (length slots))
         (limit (if (natnump print-length)
                    (min (+ start print-length) len) len))
         (i start))
    (while (< i limit)
      (let ((slot (aref slots i)))
        (unless (= i start) (princ " " stream))
        (princ ":" stream)
        (princ (cl--slot-descriptor-name slot) stream)
        (princ " " stream)
        (cl-print-object (aref object (1+ i)) stream))
      (cl-incf i))
    (when (< limit len)
      (princ " " stream)
      (cl-print-insert-ellipsis object limit stream))))

(cl-defmethod cl-print-object ((object string) stream)
  (unless stream (setq stream standard-output))
  (let* ((has-properties (or (text-properties-at 0 object)
                             (next-property-change 0 object)))
         (len (length object))
         (limit (if (natnump print-length) (min print-length len) len)))
    (if (and has-properties
             cl-print--depth
             (natnump print-level)
             (> cl-print--depth print-level))
        (cl-print-insert-ellipsis object 0 stream)
      ;; Print all or part of the string
      (when has-properties
        (princ "#(" stream))
      (if (= limit len)
          (prin1 (if has-properties (substring-no-properties object) object)
                 stream)
        (let ((part (concat (substring-no-properties object 0 limit) "...")))
          (prin1 part stream)
          (when (bufferp stream)
            (with-current-buffer stream
              (cl-print-propertize-ellipsis object limit
                                            (- (point) 4)
                                            (- (point) 1) stream)))))
      ;; Print the property list.
      (when has-properties
        (let* ((interval-limit (and (natnump print-length)
                                    (max 1 (/ print-length 3))))
               (interval-count 0)
               (start-pos (if (text-properties-at 0 object)
                              0 (next-property-change 0 object)))
               (end-pos (next-property-change start-pos object len)))
          (while (and (or (null interval-limit)
                          (< interval-count interval-limit))
                      (< start-pos len))
            (let ((props (text-properties-at start-pos object)))
              (when props
                (princ " " stream) (princ start-pos stream)
                (princ " " stream) (princ end-pos stream)
                (princ " " stream) (cl-print-object props stream)
                (cl-incf interval-count))
              (setq start-pos end-pos
                    end-pos (next-property-change start-pos object len))))
          (when (< start-pos len)
            (princ " " stream)
            (cl-print-insert-ellipsis object (list start-pos) stream)))
        (princ ")" stream)))))

(cl-defmethod cl-print-object-contents ((object string) start stream)
  ;; If START is an integer, it is an index into the string, and the
  ;; ellipsis that needs to be expanded is part of the string.  If
  ;; START is a cons, its car is an index into the string, and the
  ;; ellipsis that needs to be expanded is in the property list.
  (let* ((len (length object)))
    (if (atom start)
        ;; Print part of the string.
        (let* ((limit (if (natnump print-length)
                          (min (+ start print-length) len) len))
               (substr (substring-no-properties object start limit))
               (printed (prin1-to-string substr))
               (trimmed (substring printed 1 (1- (length printed)))))
          (princ trimmed)
          (when (< limit len)
            (cl-print-insert-ellipsis object limit stream)))

      ;; Print part of the property list.
      (let* ((first t)
             (interval-limit (and (natnump print-length)
                                  (max 1 (/ print-length 3))))
             (interval-count 0)
             (start-pos (car start))
             (end-pos (next-property-change start-pos object len)))
        (while (and (or (null interval-limit)
                        (< interval-count interval-limit))
                    (< start-pos len))
          (let ((props (text-properties-at start-pos object)))
            (when props
              (if first
                  (setq first nil)
                (princ " " stream))
              (princ start-pos stream)
              (princ " " stream) (princ end-pos stream)
              (princ " " stream) (cl-print-object props stream)
              (cl-incf interval-count))
            (setq start-pos end-pos
                  end-pos (next-property-change start-pos object len))))
        (when (< start-pos len)
          (princ " " stream)
          (cl-print-insert-ellipsis object (list start-pos) stream))))))

;;; Circularity and sharing.

;; I don't try to support the `print-continuous-numbering', because
;; I think it's ill defined anyway: if an object appears only once in each call
;; its sharing can't be properly preserved!

(cl-defmethod cl-print-object :around (object stream)
  ;; FIXME: Only put such an :around method on types where it's relevant.
  (let ((cl-print--depth (if cl-print--depth (1+ cl-print--depth) 1)))
    (cond
     (print-circle
      (let ((n (gethash object cl-print--number-table)))
        (if (not (numberp n))
            (cl-call-next-method)
          (if (> n 0)
              ;; Already printed.  Just print a reference.
              (progn (princ "#" stream) (princ n stream) (princ "#" stream))
            (puthash object (- n) cl-print--number-table)
            (princ "#" stream) (princ (- n) stream) (princ "=" stream)
            (cl-call-next-method)))))
     ((let ((already-printing (memq object cl-print--currently-printing)))
        (when already-printing
          ;; Currently printing, just print reference to avoid endless
          ;; recursion.
          (princ "#" stream)
          (princ (length (cdr already-printing)) stream))))
     (t (let ((cl-print--currently-printing
               (cons object cl-print--currently-printing)))
          (cl-call-next-method))))))

(defvar cl-print--number-index nil)

(defun cl-print--find-sharing (object table)
  ;; Avoid recursion: not only because it's too easy to bump into
  ;; `max-lisp-eval-depth', but also because function calls are fairly slow.
  ;; At first, I thought using a list for our stack would cause too much
  ;; garbage to generated, but I didn't notice any such problem in practice.
  ;; I experimented with using an array instead, but the result was slightly
  ;; slower and the reduction in GC activity was less than 1% on my test.
  (let ((stack (list object)))
    (while stack
      (let ((object (pop stack)))
        (unless
            ;; Skip objects which don't have identity!
            (or (floatp object) (numberp object)
                (null object) (if (symbolp object) (intern-soft object)))
          (let ((n (gethash object table)))
            (cond
             ((numberp n))                   ;All done.
             (n                              ;Already seen, but only once.
              (let ((n (1+ cl-print--number-index)))
                (setq cl-print--number-index n)
                (puthash object (- n) table)))
             (t
              (puthash object t table)
              (pcase object
                (`(,car . ,cdr)
                 (push cdr stack)
                 (push car stack))
                ((pred stringp)
                 (let* ((len (length object))
                        (start (if (text-properties-at 0 object)
                                   0 (next-property-change 0 object)))
                        (end (and start
                                  (next-property-change start object len))))
                   (while (and start (< start len))
                     (let ((props (text-properties-at start object)))
                       (when props
                         (push props stack))
                       (setq start end
                             end (next-property-change start object len))))))
                ((or (pred arrayp) (pred byte-code-function-p))
                 ;; FIXME: Inefficient for char-tables!
                 (dotimes (i (length object))
                   (push (aref object i) stack))))))))))))

(defun cl-print--preprocess (object)
  (let ((print-number-table (make-hash-table :test 'eq :rehash-size 2.0)))
    (if (fboundp 'print--preprocess)
        ;; Use the predefined C version if available.
        (print--preprocess object)           ;Fill print-number-table!
      (let ((cl-print--number-index 0))
        (cl-print--find-sharing object print-number-table)))
    print-number-table))

(defun cl-print-insert-ellipsis (object start stream)
  "Print \"...\" to STREAM with the `cl-print-ellipsis' text property.
Save state in the text property in order to print the elided part
of OBJECT later.  START should be 0 if the whole OBJECT is being
elided, otherwise it should be an index or other pointer into the
internals of OBJECT which can be passed to
`cl-print-object-contents' at a future time."
  (unless stream (setq stream standard-output))
  (let ((ellipsis-start (and (bufferp stream)
                             (with-current-buffer stream (point)))))
    (princ "..." stream)
    (when ellipsis-start
      (with-current-buffer stream
        (cl-print-propertize-ellipsis object start ellipsis-start (point)
                                      stream)))))

(defun cl-print-propertize-ellipsis (object start beg end stream)
  "Add the `cl-print-ellipsis' property between BEG and END.
STREAM should be a buffer.  OBJECT and START are as described in
`cl-print-insert-ellipsis'."
  (let ((value (list object start cl-print--number-table
                     cl-print--currently-printing)))
    (with-current-buffer stream
      (put-text-property beg end 'cl-print-ellipsis value stream))))

;;;###autoload
(defun cl-print-expand-ellipsis (value stream)
  "Print the expansion of an ellipsis to STREAM.
VALUE should be the value of the `cl-print-ellipsis' text property
which was attached to the ellipsis by `cl-prin1'."
  (let ((cl-print--depth 1)
        (object (nth 0 value))
        (start (nth 1 value))
        (cl-print--number-table (nth 2 value))
        (print-number-table (nth 2 value))
        (cl-print--currently-printing (nth 3 value)))
    (when (eq object (car cl-print--currently-printing))
      (pop cl-print--currently-printing))
    (if (equal start 0)
        (cl-print-object object stream)
      (cl-print-object-contents object start stream))))

;;;###autoload
(defun cl-prin1 (object &optional stream)
  "Print OBJECT on STREAM according to its type.
Output is further controlled by the variables
`cl-print-readably', `cl-print-compiled', along with output
variables for the standard printing functions.  See Info
node `(elisp)Output Variables'."
  (if cl-print-readably
      (prin1 object stream)
    (with-demoted-errors "cl-prin1: %S"
      (if (not print-circle)
          (cl-print-object object stream)
        (let ((cl-print--number-table (cl-print--preprocess object)))
          (cl-print-object object stream))))))

;;;###autoload
(defun cl-prin1-to-string (object)
  "Return a string containing the `cl-prin1'-printed representation of OBJECT."
  (with-temp-buffer
    (cl-prin1 object (current-buffer))
    (buffer-string)))

;;;###autoload
(defun cl-print-to-string-with-limit (print-function value limit)
  "Return a string containing a printed representation of VALUE.
Attempt to get the length of the returned string under LIMIT
characters with appropriate settings of `print-level' and
`print-length.'  Use PRINT-FUNCTION to print, which should take
the arguments VALUE and STREAM and which should respect
`print-length' and `print-level'.  LIMIT may be nil or zero in
which case PRINT-FUNCTION will be called with `print-level' and
`print-length' bound to nil.

Use this function with `cl-prin1' to print an object,
abbreviating it with ellipses to fit within a size limit.  Use
this function with `cl-prin1-expand-ellipsis' to expand an
ellipsis, abbreviating the expansion to stay within a size
limit."
  (setq limit (and (natnump limit)
                   (not (zerop limit))
                   limit))
  ;; Since this is used by the debugger when stack space may be
  ;; limited, if you increase print-level here, add more depth in
  ;; call_debugger (bug#31919).
  (let* ((print-length (when limit (min limit 50)))
         (print-level (when limit (min 8 (truncate (log limit)))))
         (delta-length (when limit
                         (max 1 (truncate (/ print-length print-level))))))
    (with-temp-buffer
      (catch 'done
        (while t
          (erase-buffer)
          (funcall print-function value (current-buffer))
          (let ((result (- (point-max) (point-min))))
            ;; Stop when either print-level is too low or the value is
            ;; successfully printed in the space allowed.
            (when (or (not limit) (< result limit) (<= print-level 2))
              (throw 'done (buffer-string)))
            (let* ((ratio (/ result limit))
                   (delta-level (max 1 (min (- print-level 2) ratio))))
              (cl-decf print-level delta-level)
              (cl-decf print-length (* delta-length delta-level)))))))))

(provide 'cl-print)
;;; cl-print.el ends here
