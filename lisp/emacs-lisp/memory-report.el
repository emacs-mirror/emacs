;;; memory-report.el --- Short function summaries  -*- lexical-binding: t -*-

;; Copyright (C) 2020-2022 Free Software Foundation, Inc.

;; Keywords: lisp, help

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

;; Todo (possibly): Font cache, regexp cache, bidi cache, various
;; buffer caches (newline cache, free_region_cache, etc), composition
;; cache, face cache.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'cl-lib)

(defvar memory-report--type-size (make-hash-table))

;;;###autoload
(defun memory-report ()
  "Generate a report of how Emacs is using memory.

This report is approximate, and will commonly over-count memory
usage by variables, because shared data structures will usually
by counted more than once."
  (interactive)
  (pop-to-buffer "*Memory Report*")
  (special-mode)
  (button-mode 1)
  (setq-local revert-buffer-function (lambda (_ignore-auto _noconfirm)
                                       (memory-report)))
  (setq truncate-lines t)
  (message "Gathering data...")
  (let ((reports (append (memory-report--garbage-collect)
                         (memory-report--image-cache)
                         (memory-report--symbol-plist)
                         (memory-report--buffers)
                         (memory-report--largest-variables)))
        (inhibit-read-only t)
        summaries details)
    (message "Gathering data...done")
    (erase-buffer)
    (insert (propertize "Estimated Emacs Memory Usage\n\n" 'face 'bold))
    (dolist (report reports)
      (if (listp report)
          (push report summaries)
        (push report details)))
    (dolist (summary (seq-sort (lambda (e1 e2)
                                 (> (cdr e1) (cdr e2)))
                               summaries))
      (insert (format "%s  %s\n"
                      (memory-report--format (cdr summary))
                      (car summary))))
    (insert "\n")
    (dolist (detail (nreverse details))
      (insert detail "\n")))
  (goto-char (point-min)))

(defun memory-report-object-size (object)
  "Return the size of OBJECT in bytes."
  (when (zerop (hash-table-count memory-report--type-size))
    (memory-report--garbage-collect))
  (memory-report--object-size (make-hash-table :test #'eq) object))

(defun memory-report--size (type)
  (or (gethash type memory-report--type-size)
      (gethash 'object memory-report--type-size)))

(defun memory-report--set-size (elems)
  (setf (gethash 'string memory-report--type-size)
        (cadr (assq 'strings elems)))
  (setf (gethash 'cons memory-report--type-size)
        (cadr (assq 'conses elems)))
  (setf (gethash 'symbol memory-report--type-size)
        (cadr (assq 'symbols elems)))
  (setf (gethash 'object memory-report--type-size)
        (cadr (assq 'vectors elems)))
  (setf (gethash 'float memory-report--type-size)
        (cadr (assq 'floats elems)))
  (setf (gethash 'buffer memory-report--type-size)
        (cadr (assq 'buffers elems))))

(defun memory-report--garbage-collect ()
  (let ((elems (garbage-collect)))
    (memory-report--set-size elems)
    (let ((data (list
                 (list 'strings
                       (+ (memory-report--gc-elem elems 'strings)
                          (memory-report--gc-elem elems 'string-bytes)))
                 (list 'vectors
                       (+ (memory-report--gc-elem elems 'vectors)
                          (memory-report--gc-elem elems 'vector-slots)))
                 (list 'floats (memory-report--gc-elem elems 'floats))
                 (list 'conses (memory-report--gc-elem elems 'conses))
                 (list 'symbols (memory-report--gc-elem elems 'symbols))
                 (list 'intervals (memory-report--gc-elem elems 'intervals))
                 (list 'buffer-objects
                       (memory-report--gc-elem elems 'buffers)))))
      (list (cons "Overall Object Memory Usage"
                  (seq-reduce #'+ (mapcar (lambda (elem)
                                            (* (nth 1 elem) (nth 2 elem)))
                                          elems)
                              0))
            (cons "Reserved (But Unused) Object Memory"
                  (seq-reduce #'+ (mapcar (lambda (elem)
                                            (if (nth 3 elem)
                                                (* (nth 1 elem) (nth 3 elem))
                                              0))
                                          elems)
                              0))
            (with-temp-buffer
              (insert (propertize "Object Storage\n\n" 'face 'bold))
              (dolist (object (seq-sort (lambda (e1 e2)
                                          (> (cadr e1) (cadr e2)))
                                        data))
                (insert (format "%s  %s\n"
                                (memory-report--format (cadr object))
                                (capitalize (symbol-name (car object))))))
              (buffer-string))))))

(defun memory-report--largest-variables ()
  (let ((variables nil))
    (mapatoms
     (lambda (symbol)
       (when (boundp symbol)
         (let ((size (memory-report--object-size
                      (make-hash-table :test #'eq)
                      (symbol-value symbol))))
           (when (> size 1000)
             (push (cons symbol size) variables)))))
     obarray)
    (list
     (cons (propertize "Memory Used By Global Variables"
                       'help-echo "Upper bound; mutually overlapping data from different variables are counted several times")
           (seq-reduce #'+ (mapcar #'cdr variables) 0))
     (with-temp-buffer
       (insert (propertize "Largest Variables\n\n" 'face 'bold))
       (cl-loop for i from 1 upto 20
                for (symbol . size) in (seq-sort (lambda (e1 e2)
                                                   (> (cdr e1) (cdr e2)))
                                                 variables)
                do (insert (memory-report--format size)
                           "  "
                           (symbol-name symbol)
                           "\n"))
       (buffer-string)))))

(defun memory-report--symbol-plist ()
  (let ((counted (make-hash-table :test #'eq))
        (total 0))
    (mapatoms
     (lambda (symbol)
       (cl-incf total (memory-report--object-size
                       counted (symbol-plist symbol))))
     obarray)
    (list
     (cons "Memory Used By Symbol Plists" total))))

(defun memory-report--object-size (counted value)
  (if (gethash value counted)
      0
    (setf (gethash value counted) t)
    (memory-report--object-size-1 counted value)))

(cl-defgeneric memory-report--object-size-1 (_counted _value)
  0)

(cl-defmethod memory-report--object-size-1 (_ (value symbol))
  ;; Don't count global symbols -- makes sizes of lists of symbols too
  ;; heavy.
  (if (intern-soft value obarray)
      0
    (memory-report--size 'symbol)))

(cl-defmethod memory-report--object-size-1 (_ (_value buffer))
  (memory-report--size 'buffer))

(cl-defmethod memory-report--object-size-1 (counted (value string))
  (+ (memory-report--size 'string)
     (string-bytes value)
     (memory-report--interval-size counted (object-intervals value))))

(defun memory-report--interval-size (counted intervals)
  ;; We get a list back of intervals, but only count the "inner list"
  ;; (i.e., the actual text properties), and add the size of the
  ;; intervals themselves.
  (+ (* (memory-report--size 'interval) (length intervals))
     (seq-reduce #'+ (mapcar
                      (lambda (interval)
                        (memory-report--object-size counted (nth 2 interval)))
                      intervals)
                 0)))

(cl-defmethod memory-report--object-size-1 (counted (value list))
  (let ((total 0)
        (size (memory-report--size 'cons)))
    (while value
      (cl-incf total size)
      (setf (gethash value counted) t)
      (when (car value)
        (cl-incf total (memory-report--object-size counted (car value))))
      (let ((next (cdr value)))
        (setq value (when next
                      (if (consp next)
                          (unless (gethash next counted)
                            (cdr value))
                        (cl-incf total (memory-report--object-size
                                        counted next))
                        nil)))))
    total))

(cl-defmethod memory-report--object-size-1 (counted (value vector))
  (let ((total (+ (memory-report--size 'vector)
                  (* (memory-report--size 'object) (length value)))))
    (cl-loop for elem across value
             do (cl-incf total (memory-report--object-size counted elem)))
    total))

(cl-defmethod memory-report--object-size-1 (counted (value hash-table))
  (let ((total (+ (memory-report--size 'vector)
                  (* (memory-report--size 'object) (hash-table-size value)))))
    (maphash
     (lambda (key elem)
       (cl-incf total (memory-report--object-size counted key))
       (cl-incf total (memory-report--object-size counted elem)))
     value)
    total))

;; All cl-defstruct types.
(cl-defmethod memory-report--object-size-1 (counted (value cl-structure-object))
  (let ((struct-type (type-of value)))
    (apply #'+
           (memory-report--size 'vector)
           (mapcar (lambda (slot)
                     (if (eq (car slot) 'cl-tag-slot)
                         0
                       (memory-report--object-size
                        counted
                        (cl-struct-slot-value struct-type (car slot) value))))
                   (cl-struct-slot-info struct-type)))))

(defun memory-report--format (bytes)
  (setq bytes (/ bytes 1024.0))
  (let ((units '("KiB" "MiB" "GiB" "TiB")))
    (while (>= bytes 1024)
      (setq bytes (/ bytes 1024.0))
      (setq units (cdr units)))
    (format "%6.1f %s" bytes (car units))))

(defun memory-report--gc-elem (elems type)
  (* (nth 1 (assq type elems))
     (nth 2 (assq type elems))))

(defun memory-report--buffers ()
  (let ((buffers (mapcar (lambda (buffer)
                           (cons buffer (memory-report--buffer buffer)))
                         (buffer-list))))
    (list (cons "Total Buffer Memory Usage"
                (seq-reduce #'+ (mapcar #'cdr buffers) 0))
          (with-temp-buffer
            (insert (propertize "Largest Buffers\n\n" 'face 'bold))
            (cl-loop for i from 1 upto 20
                     for (buffer . size) in (seq-sort (lambda (e1 e2)
                                                        (> (cdr e1) (cdr e2)))
                                                      buffers)
                     do (insert (memory-report--format size)
                                "  "
                                (button-buttonize
                                 (buffer-name buffer)
                                 #'memory-report--buffer-details buffer)
                                "\n"))
            (buffer-string)))))

(defun memory-report--buffer-details (buffer)
  (with-current-buffer buffer
    (apply
     #'message
     "Buffer text: %s; variables: %s; text properties: %s; overlays: %s"
     (mapcar #'string-trim (mapcar #'memory-report--format
                                   (memory-report--buffer-data buffer))))))

(defun memory-report--buffer (buffer)
  (seq-reduce #'+ (memory-report--buffer-data buffer) 0))

(defun memory-report--buffer-data (buffer)
  (with-current-buffer buffer
    (list (save-restriction
            (widen)
            (+ (position-bytes (point-max))
	       (- (position-bytes (point-min)))
	       (gap-size)))
          (seq-reduce #'+ (mapcar (lambda (elem)
                                    (if (and (consp elem) (cdr elem))
                                        (memory-report--object-size
                                         (make-hash-table :test #'eq)
                                         (cdr elem))
                                      0))
                                  (buffer-local-variables buffer))
                      0)
          (memory-report--object-size (make-hash-table :test #'eq)
                                      (object-intervals buffer))
          (memory-report--object-size (make-hash-table :test #'eq)
                                      (overlay-lists)))))

(defun memory-report--image-cache ()
  (list (cons "Total Image Cache Size" (if (fboundp 'image-cache-size)
                                           (image-cache-size)
                                         0))))

(provide 'memory-report)

;;; memory-report.el ends here
