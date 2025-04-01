(defpackage :lem-paredit-mode/tests/main
  (:use :cl :rove :lem :lem-paredit-mode)
  (:import-from :lem
                :with-current-buffers))
(in-package :lem-paredit-mode/tests/main)

(defun setup-testing-current-buffer (text)
  (let ((buffer (make-buffer "*paredit test*" :syntax-table lem-lisp-syntax:*syntax-table*)))
    (insert-string (buffer-point buffer) text)
    (buffer-start (buffer-point buffer))
    (setf (current-buffer) buffer)))

(deftest paredit-insert-vertical-line
  (testing "inside normal text"
    (with-current-buffers ()
      (setup-testing-current-buffer "asdf")
      (paredit-insert-vertical-line)
      (ok (equal "||asdf" (buffer-text (current-buffer))))
      (paredit-backward-delete)
      (ok (equal "asdf" (buffer-text (current-buffer))))))
  (testing "inside comment"
    (with-current-buffers ()
      (setup-testing-current-buffer ";asdf")
      (character-offset (current-point) 1)
      (paredit-insert-vertical-line)
      (ok (equal ";|asdf" (buffer-text (current-buffer))))
      (paredit-backward-delete)
      (ok (equal ";asdf" (buffer-text (current-buffer))))))
  (testing "inside string"
    (with-current-buffers ()
      (setup-testing-current-buffer "\"asdf\"")
      (character-offset (current-point) 1)
      (paredit-insert-vertical-line)
      (ok (equal "\"|asdf\"" (buffer-text (current-buffer))))
      (paredit-backward-delete)
      (ok (equal "\"asdf\"" (buffer-text (current-buffer)))))))

(deftest paredit-vertical-line-wrap
  (with-current-buffers ()
    (setup-testing-current-buffer "asdf")
    (paredit-vertical-line-wrap)
    (ok (equal "|asdf|" (buffer-text (current-buffer))))))
