;;; url-future-tests.el --- Test suite for url-future.

;; Copyright (C) 2011-2017 Free Software Foundation, Inc.

;; Author: Teodor Zlatanov <tzz@lifelogs.com>
;; Keywords: data

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

;;; Code:

(require 'ert)
(require 'url-future)

(ert-deftest url-future-tests ()
  (let* (saver
         (text "running future")
         (good (make-url-future :value (lambda () (format text))
                                :callback (lambda (f) (set 'saver f))))
         (bad (make-url-future :value (lambda () (/ 1 0))
                               :errorback (lambda (&rest d) (set 'saver d))))
         (tocancel (make-url-future :value (lambda () (/ 1 0))
                                    :callback (lambda (f) (set 'saver f))
                                    :errorback (lambda (&rest d)
                                                 (set 'saver d)))))
    (should (equal good (url-future-call good)))
    (should (equal good saver))
    (should (equal text (url-future-value good)))
    (should (url-future-completed-p good))
    (should-error (url-future-call good))
    (setq saver nil)
    (should (equal bad (url-future-call bad)))
    (should-error (url-future-call bad))
    (should (equal saver (list bad '(arith-error))))
    (should (url-future-errored-p bad))
    (setq saver nil)
    (should (equal (url-future-cancel tocancel) tocancel))
    (should-error (url-future-call tocancel))
    (should (null saver))
    (should (url-future-cancelled-p tocancel))))

(provide 'url-future-tests)

;;; url-future-tests.el ends here
