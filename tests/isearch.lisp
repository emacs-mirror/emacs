(defpackage :lem-tests/isearch
  (:use :cl :rove :lem)
  (:import-from :lem
                :with-current-buffers)
  (:import-from :lem-tests/utilities
                :lines
                :make-text-buffer)
  (:import-from :lem-tests/cursors
                :all-positions
                :positions-set-equal))
(in-package :lem-tests/isearch)

(defparameter *text* "
abcdefg
foo
foo
foo
xyz1234
")

(defun setup-testing-current-buffer (text)
  (let ((buffer (make-buffer "*isearch test*")))
    (insert-string (buffer-point buffer) text)
    (buffer-start (buffer-point buffer))
    (setf (current-buffer) buffer)))

(deftest replace-string
  (with-current-buffers ()
    (setup-testing-current-buffer *text*)
    (lem/isearch::query-replace-internal "foo"
                                         "foobar"
                                         #'search-forward
                                         #'search-backward
                                         :query nil
                                         :start (buffer-start-point (current-buffer))
                                         :end (buffer-end-point (current-buffer))
                                         :count 100)
    (ok (equal (ppcre:regex-replace-all "foo" *text* "foobar")
               (buffer-text (current-buffer))))))

(deftest mark-by-direction-backward/forward
  (lem-fake-interface:with-fake-interface ()
    (lem-core::save-continue-flags
      (lem-tests/utilities:with-testing-buffer
          (buffer (lem-tests/utilities:make-text-buffer
                   (lem-tests/utilities:lines "case 1"
                                              "//some code here"
                                              "case 3"
                                              "//some other code here"
                                              "case 4"
                                              "//some other code here")))
        (lem-core::set-window-buffer buffer (lem:current-window))
        (lem:execute (lem:buffer-major-mode buffer)
                     (make-instance 'lem:next-line)
                     2)

        (ok (positions-set-equal '((3 0))
                                 (all-positions buffer)))

        (handler-case
            (lem:execute (lem:buffer-major-mode buffer)
                         (make-instance 'lem/isearch:isearch-forward-symbol-at-point)
                         nil)
          (error (c)
            (uiop:println c)))

        (lem:execute (lem:buffer-major-mode buffer)
                     (make-instance 'lem/isearch:isearch-add-cursor-to-next-match)
                     nil)

        (ok (positions-set-equal '((3 4) (5 4))
                                 (all-positions buffer)))

        (lem:execute (lem:buffer-major-mode buffer)
                     (make-instance 'lem/isearch:isearch-add-cursor-to-prev-match)
                     nil)

        (ok (positions-set-equal '((1 4) (3 4) (5 4))
                                 (all-positions buffer)))

        (lem:execute (lem:buffer-major-mode buffer)
                     (make-instance 'lem/isearch:isearch-abort)
                     nil)

        (ok (positions-set-equal '((1 4) (3 4) (5 4))
                                 (all-positions buffer)))))))
