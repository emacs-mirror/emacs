;;; jsonrpc-tests.el --- tests for jsonrpc.el        -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2026 Free Software Foundation, Inc.

;; Author: João Távora <joaotavora@gmail.com>
;; Keywords: tests

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

;; About "deferred" tests, `jsonrpc--test-client' has a flag that we
;; test in its `jsonrpc-connection-ready-p' API method.  It holds any
;; `jsonrpc-request's and `jsonrpc-async-request's explicitly passed
;; `:deferred'.  After clearing the flag, the held requests are
;; actually sent to the server in the next opportunity (when receiving
;; or sending something to the server).

;;; Code:

(require 'ert)
(require 'jsonrpc)
(require 'eieio)

(defclass jsonrpc--test-endpoint (jsonrpc-process-connection)
  ((scp :accessor jsonrpc--shutdown-complete-p)))

(defclass jsonrpc--test-client (jsonrpc--test-endpoint)
  ((hold-deferred :initform t :accessor jsonrpc--hold-deferred)))

(defun jsonrpc--call-with-emacsrpc-fixture (fn)
  "Do work for `jsonrpc--with-emacsrpc-fixture'.  Call FN."
  (let* (listen-server endpoint)
    (unwind-protect
        (progn
          (setq listen-server
                (make-network-process
                 :name "Emacs RPC server" :server t :host "localhost"
                 :service (if (version<= emacs-version "26.1")
                              44444
                            ;; 26.1 can automatically find ports if
                            ;; one passes 0 here.
                            0)
                 :log (lambda (listen-server client _message)
                        (push
                         (make-instance
                          'jsonrpc--test-endpoint
                          :name (process-name client)
                          :process client
                          :request-dispatcher
                          (lambda (_endpoint method params)
                            (unless (memq method '(+ - * / vconcat append
                                                     sit-for ignore))
                              (signal 'jsonrpc-error
                                      '((jsonrpc-error-message
                                         . "Sorry, this isn't allowed")
                                        (jsonrpc-error-code . -32601))))
                            (apply method (append params nil)))
                          :on-shutdown
                          (lambda (conn)
                            (setf (jsonrpc--shutdown-complete-p conn) t)))
                         (process-get listen-server 'handlers)))))
          (setq endpoint
                (make-instance
                 'jsonrpc--test-client
                 :process
                 (open-network-stream "JSONRPC test tcp endpoint"
                                      nil "localhost"
                                      (process-contact listen-server
                                                       :service))
                 :on-shutdown
                 (lambda (conn)
                   (setf (jsonrpc--shutdown-complete-p conn) t))))
          (funcall fn endpoint))
      (unwind-protect
          (when endpoint
            (kill-buffer (jsonrpc--events-buffer endpoint))
            (jsonrpc-shutdown endpoint))
        (when listen-server
          (cl-loop do (delete-process listen-server)
                   while (progn (accept-process-output nil 0.1)
                                (process-live-p listen-server))
                   do (jsonrpc--message
                       "test listen-server is still running, waiting"))
          (cl-loop for handler in (process-get listen-server 'handlers)
                   do (ignore-errors (jsonrpc-shutdown handler)))
          (mapc #'kill-buffer
                (mapcar #'jsonrpc--events-buffer
                        (process-get listen-server 'handlers))))))))

(cl-defmacro jsonrpc--with-emacsrpc-fixture ((endpoint-sym) &body body)
  (declare (indent 1))
  `(jsonrpc--call-with-emacsrpc-fixture (lambda (,endpoint-sym) ,@body)))

(ert-deftest returns-3 ()
  "A basic test for adding two numbers in our test RPC."
  (skip-when (eq system-type 'windows-nt))
  (jsonrpc--with-emacsrpc-fixture (conn)
    (should (= 3 (jsonrpc-request conn '+ [1 2])))))

(ert-deftest errors-with--32601 ()
  "Errors with -32601"
  (skip-when (eq system-type 'windows-nt))
  (jsonrpc--with-emacsrpc-fixture (conn)
    (condition-case err
        (progn
          (jsonrpc-request conn 'delete-directory "~/tmp")
          (ert-fail "A `jsonrpc-error' should have been signaled!"))
      (jsonrpc-error
       (should (= -32601 (cdr (assoc 'jsonrpc-error-code (cdr err)))))))))

(ert-deftest signals-an--32603-JSONRPC-error ()
  "Signals an -32603 JSONRPC error."
  (skip-when (eq system-type 'windows-nt))
  (jsonrpc--with-emacsrpc-fixture (conn)
    (condition-case err
        (let ((jsonrpc-inhibit-debug-on-error t))
          (jsonrpc-request conn '+ ["a" 2])
          (ert-fail "A `jsonrpc-error' should have been signaled!"))
      (jsonrpc-error
       (should (= -32603 (cdr (assoc 'jsonrpc-error-code (cdr err)))))))))

(ert-deftest times-out ()
  "Request for 3-sec sit-for with 1-sec timeout times out."
  (skip-when (eq system-type 'windows-nt))
  (jsonrpc--with-emacsrpc-fixture (conn)
    (should-error
     (jsonrpc-request conn 'sit-for [3] :timeout 1))))

(ert-deftest doesnt-time-out ()
  :tags '(:expensive-test)
  "Request for 1-sec sit-for with 2-sec timeout succeeds."
  (skip-when (eq system-type 'windows-nt))
  (jsonrpc--with-emacsrpc-fixture (conn)
    (jsonrpc-request conn 'sit-for [1] :timeout 2)))

(ert-deftest stretching-it-but-works ()
  "Vector of numbers or vector of vector of numbers are serialized."
  (skip-when (eq system-type 'windows-nt))
  (jsonrpc--with-emacsrpc-fixture (conn)
    ;; (vconcat [1 2 3] [3 4 5]) => [1 2 3 3 4 5] which can be
    ;; serialized.
    (should (equal
             [1 2 3 3 4 5]
             (jsonrpc-request conn 'vconcat [[1 2 3] [3 4 5]])))))

(cl-defmethod jsonrpc-connection-ready-p
  ((conn jsonrpc--test-client) what)
  (and (cl-call-next-method)
       (or (not (string-match "deferred" what))
           (not (jsonrpc--hold-deferred conn)))))

(ert-deftest deferred-action-toolate ()
  :tags '(:expensive-test)
  "Deferred request fails because no one clears the flag."
  (skip-when (eq system-type 'windows-nt))
  (jsonrpc--with-emacsrpc-fixture (conn)
    (should-error
     (jsonrpc-request conn '+ [1 2]
                      :deferred "deferred-testing" :timeout 0.5)
     :type 'jsonrpc-error)
    (should
     (= 3 (jsonrpc-request conn '+ [1 2]
                           :timeout 0.5)))))

(ert-deftest deferred-action-intime ()
  :tags '(:expensive-test)
  "Deferred request barely makes it after event clears a flag."
  (skip-when (eq system-type 'windows-nt))
  ;; Send an async request, which returns immediately. However the
  ;; success fun which sets the flag only runs after some time.
  (jsonrpc--with-emacsrpc-fixture (conn)
    (jsonrpc-async-request conn
                           'sit-for [0.5]
                           :success-fn
                           (lambda (_result)
                             (setf (jsonrpc--hold-deferred conn) nil)))
    ;; Now wait for an answer to this request, which should be sent as
    ;; soon as the previous one is answered.
    (should
     (= 3 (jsonrpc-request conn '+ [1 2]
                           :deferred "deferred"
                           :timeout 1)))))

(ert-deftest deferred-action-complex-tests ()
  :tags '(:expensive-test)
  "Test a more complex situation with deferred requests."
  (skip-when (eq system-type 'windows-nt))
  (jsonrpc--with-emacsrpc-fixture (conn)
    (let (n-deferred-1
          n-deferred-2
          second-deferred-went-through-p)
      ;; This returns immediately
      (jsonrpc-async-request
       conn
       'sit-for [0.1]
       :success-fn
       (lambda (_result)
         ;; this only gets runs after the "first deferred" is stashed.
         (setq n-deferred-1
               (hash-table-count (jsonrpc--deferred-actions conn)))))
      (should-error
       ;; This stashes the request and waits. It will error because
       ;; no-one clears the "hold deferred" flag.
       (jsonrpc-request conn 'ignore ["first deferred"]
                        :deferred "first deferred"
                        :timeout 0.5)
       :type 'jsonrpc-error)
      ;; The error means the deferred actions stash is now empty
      (should (zerop (hash-table-count (jsonrpc--deferred-actions conn))))
      ;; Again, this returns immediately.
      (jsonrpc-async-request
       conn
       'sit-for [0.1]
       :success-fn
       (lambda (_result)
         ;; This gets run while "third deferred" below is waiting for
         ;; a reply.  Notice that we clear the flag in time here.
         (setq n-deferred-2 (hash-table-count (jsonrpc--deferred-actions conn)))
         (setf (jsonrpc--hold-deferred conn) nil)))
      ;; This again stashes a request and returns immediately.
      (jsonrpc-async-request conn 'ignore ["second deferred"]
                             :deferred "second deferred"
                             :timeout 1
                             :success-fn
                             (lambda (_result)
                               (setq second-deferred-went-through-p t)))
      ;; And this also stashes a request, but waits.  Eventually the
      ;; flag is cleared in time and both requests go through.
      (jsonrpc-request conn 'ignore ["third deferred"]
                       :deferred "third deferred"
                       :timeout 1)
      ;; Wait another 0.5 secs just in case the success handlers of
      ;; one of these last two requests didn't quite have a chance to
      ;; run (Emacs 25.2 apparently needs this).
      (accept-process-output nil 0.5)
      (should second-deferred-went-through-p)
      (should (eq 1 n-deferred-1))
      (should (eq 2 n-deferred-2))
      (should (eq 0 (hash-table-count (jsonrpc--deferred-actions conn)))))))


;;; Tests using Python subprocesses (scontrol / anxious mechanism)
;;;

(defconst jsonrpc--test-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory of this test file, captured at load time.")

(cl-defmacro jsonrpc--with-python-fixture ((script conn &rest initargs) &body body)
  "Start SCRIPT under python3 as a pipe subprocess, bind connection to CONN.
SCRIPT is a path relative to this file's directory.
INITARGS are passed to `make-instance' for `jsonrpc-process-connection'."
  (declare (indent 1))
  `(let ((,conn nil))
     (unwind-protect
         (progn
           (setq ,conn
                 (make-instance
                  'jsonrpc-process-connection
                  :name "jsonrpc-python-test"
                  :process (make-process
                             :name "jsonrpc-python-test"
                             :command (list "python3"
                                            (expand-file-name
                                             ,script
                                             jsonrpc--test-dir))
                             :connection-type 'pipe
                             :noquery t)
                  ,@initargs))
           (with-timeout (5
                          (when ,conn
                            (let ((buf (jsonrpc--events-buffer ,conn)))
                              (when (buffer-live-p buf)
                                (if noninteractive
                                    (progn
                                      (message "contents of `%s':" (buffer-name buf))
                                      (princ (with-current-buffer buf (buffer-string))
                                             #'external-debugging-output))
                                  (message "Preserved for inspection: %s"
                                           (buffer-name buf))))))
                          (ert-fail "Test timed out after 5s"))
             ,@body))
       (when ,conn
         (ignore-errors
          (jsonrpc-notify ,conn 'harakiri nil)
          (kill-buffer (jsonrpc--events-buffer ,conn))
          (jsonrpc-shutdown ,conn))))))

(ert-deftest scontrol-remote-during-sync-1 ()
  "Anxious local continuations.
Endpoint sends a remote request RR1 on LR1, then replies to LR1
immediately before waiting for RR1 to resolve.
This is what JETLS does (bug#80623)."
  (skip-unless (executable-find "python3"))
  (skip-when (eq system-type 'windows-nt))
  (jsonrpc--with-python-fixture
      ("jsonrpc-resources/server-remote-during-sync-1.py" conn
       :request-dispatcher
       (lambda (_conn method _params)
         (pcase method
           ('RR1 "rr1-ok")
           (_ (error "unexpected method: %s" method)))))
    (should (equal "lr1-ok" (jsonrpc-request conn 'LR1 [] :timeout 5)))))

(ert-deftest scontrol-remote-during-sync-2 ()
  "Anxious local continuations.
Exactly the same test as 2, but different endpoint, which now still
sends RR1 on LR1 but now waits for RR1 to resolve before replying to
LR1.
This is what GoPls does (bug#80623)."
  (skip-unless (executable-find "python3"))
  (skip-when (eq system-type 'windows-nt))
  (jsonrpc--with-python-fixture
      ("jsonrpc-resources/server-remote-during-sync-2.py" conn
       :request-dispatcher
       (lambda (_conn method _params)
         (pcase method
           ('RR1 "rr1-ok")
           (_ (error "unexpected method: %s" method)))))
    (should (equal "lr1-ok" (jsonrpc-request conn 'LR1 [] :timeout 5)))))

(ert-deftest scontrol-anxious-nested ()
  "Nested anxious continuations
Two local sync requests LR1 and LR2 with a remote RR1 in between.
Vaguely similar to Julia's JETLS (bug#80623), but more complex."
  (skip-unless (executable-find "python3"))
  (skip-when (eq system-type 'windows-nt))
  (let (lr2-result completed)
    (jsonrpc--with-python-fixture
     ("jsonrpc-resources/server-anxious-nested.py" conn
      :request-dispatcher
      (lambda (conn method _params)
        (pcase method
          ('RR1
           (setq lr2-result
                 (jsonrpc-request conn 'LR2 [] :timeout 5))
           (push "lr2" completed)
           (push "rr1" completed))
          (_ (error "unexpected method: %s" method)))))
     (should (equal "lr1-ok" (jsonrpc-request conn 'LR1 [] :timeout 5)))
     (push "lr1" completed)
     (should (equal "lr2-ok" lr2-result))
     (should (equal '("lr1" "rr1" "lr2") completed)))))

(ert-deftest scontrol-remote-error ()
  "Anxious continuation even when rdispatcher signals errors."
  (skip-unless (executable-find "python3"))
  (skip-when (eq system-type 'windows-nt))
  (jsonrpc--with-python-fixture
      ("jsonrpc-resources/server-remote-error.py" conn
       :request-dispatcher
       (lambda (_conn method _params)
         (pcase method
           ('badMethod
            (signal 'jsonrpc-error
                    '((jsonrpc-error-message . "method not allowed")
                      (jsonrpc-error-code . -32601))))
           (_ (error "unexpected method: %s" method)))))
    (should (equal "ok" (jsonrpc-request conn 'LR1 [] :timeout 5)))))

(ert-deftest shutdown-clean-after-notification ()
  "Server exits cleanly after harakiri notification.
`jsonrpc-shutdown' should not emit a \"Sentinel hasn't run\" warning."
  (skip-unless (executable-find "python3"))
  (skip-when (eq system-type 'windows-nt))
  (let (warned)
    (cl-letf (((symbol-function 'jsonrpc--warn)
               (lambda (fmt &rest args)
                 (setq warned (apply #'format fmt args)))))
      (jsonrpc--with-python-fixture
          ("jsonrpc-resources/server-harakiri.py" conn)
        (jsonrpc-notify conn 'harakiri nil)
        ;; Give the server time to exit before shutdown checks the sentinel.
        (accept-process-output nil 0.3)
        (jsonrpc-shutdown conn)))
    (should-not warned)))

(provide 'jsonrpc-tests)
;;; jsonrpc-tests.el ends here
