(defpackage :lem-tests/self-insert-command
  (:use :cl
        :lem
        :rove)
  (:import-from :lem-fake-interface
                :with-fake-interface))
(in-package :lem-tests/self-insert-command)

(defun execute-self-insert (&rest key-seq)
  (erase-buffer (current-buffer))
  (execute-key-sequence key-seq)
  (buffer-text (current-buffer)))

(deftest self-insert-command
  (with-fake-interface ()
    (ok "a" (execute-self-insert (make-key :sym "a")))
    (ok "aaaa" (execute-self-insert (make-key :ctrl t :sym "u") (make-key :sym "a")))
    (handler-case
        (progn
          (execute-key-sequence (list (make-key :super t :meta t :hyper t :sym "a")))
          (fail "unreachable"))
      (editor-error (e)
        (ok (search "Key not found: " (princ-to-string e)))))))
