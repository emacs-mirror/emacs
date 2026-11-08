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


;;; Tests using Python subprocesses
;;;

(defconst jsonrpc--test-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory of this test file, captured at load time.")

(cl-defmacro jsonrpc--with-python-fixture ((script conn &rest initargs) &body body)
  "Start SCRIPT under python3 as a pipe subprocess, bind connection to CONN.
SCRIPT is a path relative to this file's directory.
INITARGS are passed to `make-instance' for `jsonrpc--test-client'."
  (declare (indent 1))
  `(let ((,conn nil))
     (skip-unless (executable-find "python3"))
     (unwind-protect
         (progn
           (setq ,conn
                 (make-instance
                  'jsonrpc--test-client
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
          (jsonrpc-request ,conn 'harakiri nil :timeout 1)
          (kill-buffer (jsonrpc--events-buffer ,conn))
          (jsonrpc-shutdown ,conn))))))

(cl-defmethod jsonrpc-connection-ready-p
  ((conn jsonrpc--test-client) what)
  (and (cl-call-next-method)
       (or (not (string-match "deferred" what))
           (not (jsonrpc--hold-deferred conn)))))

(ert-deftest returns-3 ()
  "A basic test for adding two numbers in our test RPC."
  (skip-when (eq system-type 'windows-nt))
  (jsonrpc--with-python-fixture
      ("jsonrpc-resources/server-emacsrpc.py" conn)
    (should (= 3 (jsonrpc-request conn '+ [1 2])))))

(ert-deftest times-out ()
  "Request for 3-sec sit-for with 1-sec timeout times out."
  (skip-when (eq system-type 'windows-nt))
  (jsonrpc--with-python-fixture
      ("jsonrpc-resources/server-emacsrpc.py" conn)
    (should-error
     (jsonrpc-request conn 'sit-for [3] :timeout 1))))

(ert-deftest doesnt-time-out ()
  :tags '(:expensive-test)
  "Request for 1-sec sit-for with 2-sec timeout succeeds."
  (skip-when (eq system-type 'windows-nt))
  (jsonrpc--with-python-fixture
      ("jsonrpc-resources/server-emacsrpc.py" conn)
    (jsonrpc-request conn 'sit-for [1] :timeout 2)))

(ert-deftest stretching-it-but-works ()
  "Vector of numbers or vector of vector of numbers are serialized."
  (skip-when (eq system-type 'windows-nt))
  (jsonrpc--with-python-fixture
      ("jsonrpc-resources/server-emacsrpc.py" conn)
    ;; (vconcat [1 2 3] [3 4 5]) => [1 2 3 3 4 5] which can be
    ;; serialized.
    (should (equal
             [1 2 3 3 4 5]
             (jsonrpc-request conn 'vconcat [[1 2 3] [3 4 5]])))))

(ert-deftest deferred-action-toolate ()
  :tags '(:expensive-test)
  "Deferred request fails because no one clears the flag."
  (skip-when (eq system-type 'windows-nt))
  (jsonrpc--with-python-fixture
      ("jsonrpc-resources/server-emacsrpc.py" conn)
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
  (jsonrpc--with-python-fixture
      ("jsonrpc-resources/server-emacsrpc.py" conn)
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
  (jsonrpc--with-python-fixture
      ("jsonrpc-resources/server-emacsrpc.py" conn)
    (let (n-deferred-1
          n-deferred-2
          second-deferred-went-through-p)
      ;; This returns immediately
      (jsonrpc-async-request
       conn
       'sit-for [0.01]
       :success-fn
       (lambda (_result)
         ;; this only gets runs after the "first deferred" is stashed.
         (setq n-deferred-1
               (hash-table-count (jsonrpc--deferred-actions conn)))))
      (should-error
       ;; This stashes the request and waits. It will error with a
       ;; timeout after blocking for 1 sec because no-one clears the
       ;; "hold deferred" flag.
       (jsonrpc-request conn 'ignore ["first deferred"]
                        :deferred "first deferred"
                        :timeout 1.0)
       :type 'jsonrpc-error)
      ;; The error means the deferred actions stash is now empty
      (should (zerop (hash-table-count (jsonrpc--deferred-actions conn))))
      ;; Again, this returns immediately.
      (jsonrpc-async-request
       conn
       'sit-for [0.01]
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

(ert-deftest scontrol-remote-during-sync-1 ()
  "Anxious local continuations.
Endpoint sends a remote request RR1 on LR1, then replies to LR1
immediately before waiting for RR1 to resolve.
This is what JETLS does (bug#80623)."
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
           (push "rr1" completed)
           "rr1-ok")
          (_ (error "unexpected method: %s" method)))))
     (should (equal "lr1-ok" (jsonrpc-request conn 'LR1 [] :timeout 5)))
     (push "lr1" completed)
     (should (equal "lr2-ok" lr2-result))
     (should (equal '("lr1" "rr1" "lr2") completed)))))

(ert-deftest scontrol-remote-error ()
  "Anxious continuation even when rdispatcher signals errors."
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

(ert-deftest shutdown-clean-after-request ()
  "Server exits cleanly after harakiri request.
`jsonrpc-shutdown' should not emit a \"Sentinel hasn't run\" warning."
  (skip-when (eq system-type 'windows-nt))
  (let (warned)
    (cl-letf (((symbol-function 'jsonrpc--warn)
               (lambda (fmt &rest args)
                 (setq warned (apply #'format fmt args)))))
      (jsonrpc--with-python-fixture
          ("jsonrpc-resources/server-harakiri.py" conn)
        (jsonrpc-request conn 'harakiri nil :timeout 3)
        (jsonrpc-shutdown conn)
        (setq conn nil)))
    (should-not warned)))

(ert-deftest shutdown-promptly-after-exit ()
  "Check `jsonrpc-shutdown' returns in time."
  (skip-when (eq system-type 'windows-nt))
  (let (warned elapsed)
    (cl-letf (((symbol-function 'jsonrpc--warn)
               (lambda (fmt &rest args)
                 (setq warned (apply #'format fmt args)))))
      (jsonrpc--with-python-fixture
          ("jsonrpc-resources/server-harakiri.py" conn)
        (jsonrpc-notify conn 'harakiri nil)
        (let ((start (float-time)))
          (jsonrpc-shutdown conn)
          (setq elapsed (- (float-time) start)))
        (setq conn nil)))
    (should-not warned)
    (should (< elapsed 0.25))))

(ert-deftest shutdown-cleanup-kills-all-buffers ()
  "Check `jsonrpc-shutdown' with CLEANUP kills all buffers."
  (skip-when (eq system-type 'windows-nt))
  (let (buffers)
    (jsonrpc--with-python-fixture
        ("jsonrpc-resources/server-harakiri.py" conn)
      (setq buffers (list (process-buffer (jsonrpc--process conn))
                          (jsonrpc-stderr-buffer conn)
                          (jsonrpc-events-buffer conn)))
      (should (cl-every #'buffer-live-p buffers))
      (jsonrpc-notify conn 'harakiri nil)
      (jsonrpc-shutdown conn t)
      (should-not (cl-some #'buffer-live-p buffers))
      (setq conn nil))))

(defun jsonrpc--test-buffers (conn)
  "List CONN's process, stderr and events buffers."
  (list (process-buffer (jsonrpc--process conn))
        (jsonrpc-stderr-buffer conn)
        (jsonrpc-events-buffer conn)))

(defun jsonrpc--test-wait-killed (buffers)
  "Wait for all BUFFERS to be killed, failing after 2s."
  (with-timeout (2 (ert-fail "Buffers still live after 2s"))
    (while (cl-some #'buffer-live-p buffers)
      (accept-process-output nil 0.01))))

(ert-deftest async-shutdown-after-exit ()
  "Async `jsonrpc-shutdown' returns at once, cleans up after exit."
  (skip-when (eq system-type 'windows-nt))
  (let (warned)
    (cl-letf (((symbol-function 'jsonrpc--warn)
               (lambda (&rest _) (setq warned t))))
      (jsonrpc--with-python-fixture
          ("jsonrpc-resources/server-harakiri.py" conn)
        (let ((buffers (jsonrpc--test-buffers conn))
              (start (float-time)))
          (jsonrpc-notify conn 'harakiri nil)
          (jsonrpc-shutdown conn t t)
          (should (< (- (float-time) start) 0.05))
          (jsonrpc--test-wait-killed buffers)
          (should-not (jsonrpc-running-p conn))
          (setq conn nil))))
    (should-not warned)))

(ert-deftest async-shutdown-kills-stubborn-server ()
  "Async `jsonrpc-shutdown' deletes a server that doesn't exit."
  (skip-when (eq system-type 'windows-nt))
  (let ((warnings 0))
    (cl-letf (((symbol-function 'jsonrpc--warn)
               (lambda (&rest _) (cl-incf warnings))))
      (jsonrpc--with-python-fixture
          ("jsonrpc-resources/server-harakiri.py" conn)
        (let ((buffers (jsonrpc--test-buffers conn))
              (start (float-time)))
          ;; don't send any exit notification to the server
          (jsonrpc-shutdown conn t t)
          (should (jsonrpc-running-p conn))
          (jsonrpc--test-wait-killed buffers)
          (should (>= (- (float-time) start) 0.25))
          (should-not (jsonrpc-running-p conn))
          (setq conn nil))))
    (should (= warnings 1))))

(ert-deftest async-shutdown-after-sentinel ()
  "Async `jsonrpc-shutdown' cleans up at once once the sentinel ran."
  (skip-when (eq system-type 'windows-nt))
  (jsonrpc--with-python-fixture
      ("jsonrpc-resources/server-harakiri.py" conn)
    (let ((buffers (jsonrpc--test-buffers conn))
          (proc (jsonrpc--process conn)))
      (jsonrpc-notify conn 'harakiri nil)
      (with-timeout (2 (ert-fail "Sentinel didn't run"))
        (while (not (process-get proc 'jsonrpc-sentinel-done))
          (accept-process-output nil 0.01)))
      ;; The sentinel already killed the process buffer.
      (should (cl-every #'buffer-live-p (cdr buffers)))
      (jsonrpc-shutdown conn t t)
      (should-not (cl-some #'buffer-live-p buffers))
      (setq conn nil))))

(ert-deftest same-name-connections-dont-share-buffers ()
  "Check two connections with the same name get separate buffers.
Shutting one down leaves the other's buffers alone."
  (skip-when (eq system-type 'windows-nt))
  (skip-unless (executable-find "python3"))
  (let* ((name "jsonrpc-same-name")
         (script (expand-file-name "jsonrpc-resources/server-emacsrpc.py"
                                   jsonrpc--test-dir))
         (make-conn
          (lambda ()
            (jsonrpc-process-connection
             :name name
             :process
             (lambda ()
               (make-process
                :name name :command (list "python3" script)
                :connection-type 'pipe :noquery t
                ;; The old handshake: find jsonrpc's stderr buffer by name.
                :stderr (get-buffer-create (format "*%s stderr*" name)))))))
         (a (funcall make-conn))
         (b (funcall make-conn))
         (buffers-of (lambda (conn)
                       (list (process-buffer (jsonrpc--process conn))
                             (jsonrpc-stderr-buffer conn)
                             (jsonrpc-events-buffer conn)))))
    (unwind-protect
        (let ((a-buffers (funcall buffers-of a))
              (b-buffers (funcall buffers-of b)))
          (should (cl-every #'buffer-live-p (append a-buffers b-buffers)))
          (should-not (cl-intersection a-buffers b-buffers))
          (jsonrpc-notify a 'harakiri nil)
          (jsonrpc-shutdown a t)
          (should (cl-every #'buffer-live-p b-buffers))
          (should (= 3 (jsonrpc-request b '+ [1 2] :timeout 3))))
      (dolist (conn (list a b))
        (when (jsonrpc-running-p conn)
          (ignore-errors (jsonrpc-notify conn 'harakiri nil))
          (jsonrpc-shutdown conn t))
        (kill-buffer (jsonrpc-events-buffer conn))))))

(ert-deftest dead-connection-kills-process-buffer ()
  "Check that a dead connection's buffer dies even without CLEANUP.
Otherwise every reconnection under the same name would make garbage.."
  (skip-when (eq system-type 'windows-nt))
  (let (output events)
    (jsonrpc--with-python-fixture
        ("jsonrpc-resources/server-harakiri.py" conn)
      (setq output (process-buffer (jsonrpc--process conn))
            events (jsonrpc-events-buffer conn))
      (jsonrpc-notify conn 'harakiri nil)
      (jsonrpc-shutdown conn)
      (setq conn nil))
    (unwind-protect
        (progn (should-not (buffer-live-p output))
               (should (buffer-live-p events)))
      (kill-buffer events))))

(ert-deftest bug-82046-failed-send-cleans-up ()
  (skip-when (eq system-type 'windows-nt))
  (jsonrpc--with-python-fixture
      ("jsonrpc-resources/server-emacsrpc.py" conn)
    (delete-process (jsonrpc--process conn))
    (should-error (jsonrpc-async-request conn '+ [1 2] :timeout 2))
    (should-error (jsonrpc-request conn '+ [1 2] :timeout 2))
    (should-not (jsonrpc--continuations conn))
    (should-not (jsonrpc--scontrol conn))))

;; The following tests make the write of a big request block, so that
;; Emacs runs timers and reads process output while waiting for the pipe
;; to drain (bug#82046).  FIXME: They also access some jsonrpc.el
;; internals, but that's acceptable here.
(defconst jsonrpc--test-big-blob (vector (make-string (* 1024 1024) ?x))
  "Params big enough to fill a pipe's buffer.")

(ert-deftest bug-82046-reply-during-send ()
  (skip-when (eq system-type 'windows-nt))
  (jsonrpc--with-python-fixture
      ("jsonrpc-resources/server-emacsrpc.py" conn)
    (let (result)
      ;; The lambda will run as soon as it gets a chance _inside_ the
      ;; big blob request's send.
      (run-at-time 0 nil
                   (lambda ()
                     (setq result (jsonrpc-request conn '+ [1 2] :timeout 2))))
      ;; We want this to return normally (2 seconds should be enough,
      ;; ignore returns nil)
      (should-not (jsonrpc-request conn 'ignore jsonrpc--test-big-blob
                                   :timeout 2))
      ;; Check the 1 + 2 was calculated remotely and everything is clean
      (should (= 3 result))
      (should-not (jsonrpc--continuations conn))
      (should-not (jsonrpc--scontrol conn)))))

(ert-deftest bug-82046-reply-parked-during-aborted-send ()
  (skip-when (eq system-type 'windows-nt))
  (jsonrpc--with-python-fixture
      ("jsonrpc-resources/server-emacsrpc.py" conn)
    (let (result)
      ;; The async `+' reply will arrive during the send of the big sync
      ;; request below, so it'll wait on the latter's sync-control
      ;; "scontrol" entry.
      (jsonrpc-async-request conn '+ [1 2]
                             :success-fn (lambda (r) (setq result r))
                             :timeout 3)
      (jsonrpc-async-request conn 'sit-for [1] :timeout 3)
      ;; The above sit-for will make the server stop reading for a
      ;; while, and now we arrange for the big blob write, which will
      ;; block, to be aborted.
      (run-at-time 0.3 nil (lambda () (throw 'abort 'aborted)))
      (should (eq 'aborted
                  (catch 'abort
                    (jsonrpc-request conn 'ignore jsonrpc--test-big-blob
                                     :timeout 3))))
      ;; Now there should be no outstanding synchronous request holding
      ;; up other requests.
      (should-not (jsonrpc--scontrol conn))
      ;; Eventually our response should arrive (the fixture times
      ;; out if it never does).
      (while (not result) (accept-process-output nil 0.1))
      (should (= 3 result)))))

(ert-deftest bug-82046-aborted-send-spares-outer-request ()
  (skip-when (eq system-type 'windows-nt))
  (jsonrpc--with-python-fixture
      ("jsonrpc-resources/server-emacsrpc.py" conn)
    (let (result)
      ;; The async `+' reply will arrive while the outer `sit-for'
      ;; request a few lines below waits, so it'll wait on that
      ;; request's "scontrol" entry.
      (jsonrpc-async-request conn '+ [1 2]
                             :success-fn (lambda (r) (setq result r))
                             :timeout 3)
      ;; Meanwhile, an inner sync request's blocked send is aborted.
      ;; That mustn't disturb the outer request's scontrol entry.
      (run-at-time 0.1 nil
                   (lambda ()
                     (run-at-time 0.2 nil (lambda () (throw 'abort 'aborted)))
                     (catch 'abort
                       (jsonrpc-request conn 'ignore jsonrpc--test-big-blob
                                        :timeout 3))))
      (should-not (jsonrpc-request conn 'sit-for [1] :timeout 3))
      (while (not result) (accept-process-output nil 0.1))
      (should (= 3 result)))))

(provide 'jsonrpc-tests)
;;; jsonrpc-tests.el ends here
