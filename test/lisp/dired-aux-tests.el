;;; dired-aux-tests.el --- Test suite for dired-aux. -*- lexical-binding: t -*-

;; Copyright (C) 2017-2018 Free Software Foundation, Inc.

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
(require 'dired-aux)
(eval-when-compile (require 'cl-lib))

(ert-deftest dired-test-bug27496 ()
  "Test for https://debbugs.gnu.org/27496 ."
  (skip-unless (executable-find shell-file-name))
  (let* ((foo (make-temp-file "foo"))
         (files (list foo)))
    (unwind-protect
        (cl-letf (((symbol-function 'y-or-n-p) 'error))
          (dired temporary-file-directory)
          (dired-goto-file foo)
          ;; `dired-do-shell-command' returns nil on success.
          (should-error (dired-do-shell-command "ls ? ./?" nil files))
          (should-error (dired-do-shell-command "ls ./? ?" nil files))
          (should-not (dired-do-shell-command "ls ? ?" nil files))
          (should-error (dired-do-shell-command "ls * ./*" nil files))
          (should-not (dired-do-shell-command "ls * *" nil files))
          (should-not (dired-do-shell-command "ls ? ./`?`" nil files)))
      (delete-file foo))))

;; Auxiliar macro for `dired-test-bug28834': it binds
;; `dired-create-destination-dirs' to CREATE-DIRS and execute BODY.
;; If YES-OR-NO is non-nil, it binds `yes-or-no-p' to
;; to avoid the prompt.
(defmacro with-dired-bug28834-test (create-dirs yes-or-no &rest body)
  (declare (debug (form symbolp body)))
  (let ((foo (make-symbol "foo")))
    `(let* ((,foo (make-temp-file "foo" 'dir))
            (dired-create-destination-dirs ,create-dirs))
       (setq from (make-temp-file "from"))
       (setq to-cp
             (expand-file-name
              "foo-cp" (file-name-as-directory (expand-file-name "bar" ,foo))))
       (setq to-mv
             (expand-file-name
              "foo-mv" (file-name-as-directory (expand-file-name "qux" ,foo))))
       (unwind-protect
           (if ,yes-or-no
               (cl-letf (((symbol-function 'yes-or-no-p)
                          (lambda (_prompt) (eq ,yes-or-no 'yes))))
                 ,@body)
             ,@body)
         ;; clean up
         (delete-directory ,foo 'recursive)
         (delete-file from)))))

(ert-deftest dired-test-bug28834 ()
  "test for https://debbugs.gnu.org/28834 ."
  (let (from to-cp to-mv)
    ;; `dired-create-destination-dirs' set to 'always.
    (with-dired-bug28834-test
     'always nil
     (dired-copy-file-recursive from to-cp nil)
     (should (file-exists-p to-cp))
     (dired-rename-file from to-mv nil)
     (should (file-exists-p to-mv)))
    ;; `dired-create-destination-dirs' set to nil.
    (with-dired-bug28834-test
     nil nil
     (should-error (dired-copy-file-recursive from to-cp nil))
     (should-error (dired-rename-file from to-mv nil)))
    ;; `dired-create-destination-dirs' set to 'ask.
    (with-dired-bug28834-test
     'ask 'yes ; Answer `yes'
     (dired-copy-file-recursive from to-cp nil)
     (should (file-exists-p to-cp))
     (dired-rename-file from to-mv nil)
     (should (file-exists-p to-mv)))
    (with-dired-bug28834-test
     'ask 'no ; Answer `no'
     (should-error (dired-copy-file-recursive from to-cp nil))
     (should-error (dired-rename-file from to-mv nil)))))


(provide 'dired-aux-tests)
;; dired-aux-tests.el ends here
