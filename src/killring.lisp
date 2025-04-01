(in-package :lem-core)

(defvar *killring* (make-killring 10))

(defun current-killring ()
  *killring*)

(defmacro with-current-killring (killring &body body)
  `(let ((*killring* ,killring))
     ,@body))

(defmacro with-disable-killring (() &body body)
  `(let ((*killring* (make-nop-killring)))
     ,@body))

(defun copy-to-clipboard-with-killring (string)
  (push-killring-item (current-killring) string)
  (when (enable-clipboard-p)
    (copy-to-clipboard (peek-killring-item (current-killring) 0))))

(defun yank-from-clipboard-or-killring ()
  (multiple-value-bind (str options) (peek-killring-item (current-killring) 0)
    (or (and (enable-clipboard-p) (get-clipboard-data))
        (values str options))))
