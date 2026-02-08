;;; network-stream-tests.el --- tests for network processes       -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2026 Free Software Foundation, Inc.

;; Author: Lars Ingebrigtsen <larsi@gnus.org>

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


;;; Code:

(require 'ert)
(require 'ert-x)
(require 'gnutls)
(require 'network-stream)
;; The require above is needed for 'open-network-stream' to work, but
;; it pulls in nsm, which then makes the :nowait t' tests fail unless
;; we disable the nsm, which we do by binding 'network-security-level'

(declare-function gnutls-peer-status "gnutls.c")

(ert-deftest make-local-unix-server ()
  (skip-unless (featurep 'make-network-process '(:family local)))
  (let* ((file (make-temp-name "/tmp/server-test"))
         (server
          (make-network-process
           :name "server"
           :server t
           :buffer (get-buffer-create "*server*")
           :noquery t
           :family 'local
           :service file)))
    (should (equal (process-contact server :local) file))
    (delete-file (process-contact server :local))))

(ert-deftest make-ipv4-tcp-server-with-unspecified-port ()
  (let ((server
         (make-network-process
          :name "server"
          :server t
          :noquery t
          :family 'ipv4
          :service t
          :host 'local)))
    (should (and (arrayp (process-contact server :local))
                 (numberp (aref (process-contact server :local) 4))
                 (> (aref (process-contact server :local) 4) 0)))
    (delete-process server)))

(ert-deftest make-ipv4-tcp-server-with-specified-port ()
  (let ((server
         (make-network-process
          :name "server"
          :server t
          :noquery t
          :family 'ipv4
          :service 57869
          :host 'local)))
    (should (and (arrayp (process-contact server :local))
                 (= (aref (process-contact server :local) 4) 57869)))
    (delete-process server)))

(ert-deftest make-ipv6-tcp-server-with-unspecified-port ()
  (skip-unless (featurep 'make-network-process '(:family ipv6)))
  (let ((server
         (ignore-errors
           (make-network-process
            :name "server"
            :server t
            :noquery t
            :family 'ipv6
            :service t
            :host 'local))))
    (skip-unless server)
    (should (and (arrayp (process-contact server :local))
                 (numberp (aref (process-contact server :local) 8))
                 (> (aref (process-contact server :local) 8) 0)))
    (delete-process server)))

(ert-deftest make-ipv6-tcp-server-with-specified-port ()
  (skip-unless (featurep 'make-network-process '(:family ipv6)))
  (let ((server
         (ignore-errors
           (make-network-process
            :name "server"
            :server t
            :noquery t
            :family 'ipv6
            :service 57870
            :host 'local))))
    (skip-unless server)
    (should (and (arrayp (process-contact server :local))
                 (= (aref (process-contact server :local) 8) 57870)))
    (delete-process server)))

(defun make-server (host &optional family)
  (make-network-process
   :name "server"
   :server t
   :noquery t
   :family (or family 'ipv4)
   :coding 'raw-text-unix
   :buffer (get-buffer-create "*server*")
   :service t
   :sentinel 'server-sentinel
   :filter 'server-process-filter
   :host host))

(defun server-sentinel (_proc _msg)
  )

(defun server-process-filter (proc string)
  (let ((prev (process-get proc 'previous-string)))
    (when prev
      (setq string (concat prev string))
      (process-put proc 'previous-string nil)))
  (if (and (not (string-search "\n" string))
           (> (length string) 0))
      (process-put proc 'previous-string string))
  (let ((command (split-string string)))
    (cond
     ((equal (car command) "echo")
      (process-send-string proc (concat (cadr command) "\n")))
     (t
      ))))

(defun network-test--resolve-system-name ()
  (let ((addresses (network-lookup-address-info (system-name))))
    (if addresses
        (cl-loop for address in addresses
                 when (or (and (= (length address) 5)
                               ;; IPv4 localhost addresses start with 127.
                               (= (elt address 0) 127))
                          (and (= (length address) 9)
                               ;; IPv6 localhost address.
                               (equal address [0 0 0 0 0 0 0 1 0])))
                 return t)
      t)))

(ert-deftest echo-server-with-dns ()
  (skip-when (network-test--resolve-system-name))
  (let* ((server (make-server (system-name)))
         (port (aref (process-contact server :local) 4))
         (proc (make-network-process :name "foo"
                                     :buffer (generate-new-buffer "*foo*")
                                     :host (system-name)
                                     :service port)))
    (with-current-buffer (process-buffer proc)
      (process-send-string proc "echo foo")
      (sleep-for 0.1)
      (should (equal (buffer-string) "foo\n")))
    (delete-process server)))

(ert-deftest echo-server-with-localhost ()
  (let* ((server (make-server 'local))
         (port (aref (process-contact server :local) 4))
         (proc (make-network-process :name "foo"
                                     :buffer (generate-new-buffer "*foo*")
                                     :host "localhost"
                                     :service port)))
    (with-current-buffer (process-buffer proc)
      (process-send-string proc "echo foo")
      (sleep-for 0.1)
      (should (equal (buffer-string) "foo\n")))
    (delete-process server)))

(ert-deftest echo-server-with-local-ipv4 ()
  (let* ((server (make-server 'local 'ipv4))
         (port (aref (process-contact server :local) 4))
         (proc (make-network-process :name "foo"
                                     :buffer (generate-new-buffer "*foo*")
                                     :host 'local
                                     :family 'ipv4
                                     :service port)))
    (with-current-buffer (process-buffer proc)
      (process-send-string proc "echo foo")
      (sleep-for 0.1)
      (should (equal (buffer-string) "foo\n")))
    (delete-process server)))

(ert-deftest echo-server-with-local-ipv6 ()
  (skip-unless (featurep 'make-network-process '(:family ipv6)))
  (let ((server (ignore-errors (make-server 'local 'ipv6))))
    (skip-unless server)
    (let* ((port (aref (process-contact server :local) 8))
           (proc (make-network-process :name "foo"
                                       :buffer (generate-new-buffer "*foo*")
                                       :host 'local
                                       :family 'ipv6
                                       :service port)))
      (with-current-buffer (process-buffer proc)
        (process-send-string proc "echo foo")
        (sleep-for 0.1)
        (should (equal (buffer-string) "foo\n")))
      (delete-process server))))

(ert-deftest echo-server-with-ip ()
  (let* ((server (make-server 'local))
         (port (aref (process-contact server :local) 4))
         (proc (make-network-process :name "foo"
                                     :buffer (generate-new-buffer "*foo*")
                                     :host "127.0.0.1"
                                     :service port)))
    (with-current-buffer (process-buffer proc)
      (process-send-string proc "echo foo")
      (sleep-for 0.1)
      (should (equal (buffer-string) "foo\n")))
    (delete-process server)))

(ert-deftest echo-server-nowait ()
  (let* ((server (make-server 'local))
         (port (aref (process-contact server :local) 4))
         (proc (make-network-process :name "foo"
                                     :buffer (generate-new-buffer "*foo*")
                                     :host "localhost"
                                     :nowait t
                                     :family 'ipv4
                                     :service port))
         (times 0))
    (should (eq (process-status proc) 'connect))
    (while (and (eq (process-status proc) 'connect)
                (< (setq times (1+ times)) 10))
      (sit-for 0.1))
    (skip-when (eq (process-status proc) 'connect))
    (with-current-buffer (process-buffer proc)
      (process-send-string proc "echo foo")
      (sleep-for 0.1)
      (should (equal (buffer-string) "foo\n")))
    (delete-process server)))

(defun make-tls-server (&optional params)
  (catch 'server
    (let (port
          proc)
      (while t
        (setq port (+ 20000 (random 45535))
              proc (apply #'start-process
                          "gnutls" (generate-new-buffer "*tls*")
                          "gnutls-serv" "--http"
                          "--x509keyfile"
                          (ert-resource-file "key.pem")
                          "--x509certfile"
                          (ert-resource-file "cert.pem")
                          "--port" (format "%s" port)
                          params))
        (while (not (eq (process-status proc) 'run))
          (sit-for 0.1))
        (with-current-buffer (process-buffer proc)
          (when (eq
                 (catch 'status
                   (while t
                     (goto-char (point-min))
                     (when (search-forward (format "port %s..." port) nil t)
                       (if (looking-at "done")
                           (throw 'status 'done))
                       (if (looking-at "bind() failed")
                           (throw 'status 'failed)))
                     (sit-for 0.1)))
                 'done)
            (throw 'server (cons proc port))))
        (delete-process proc)))))

(defmacro with-tls-params (func &optional proc-parms proc-negotiate &rest server-parms)
  "Call TLS FUNC with extra parameters PROC-PARMS.
Call PROC-NEGOTIATE once the connection is up.  SERVER-PARMS are the
additional parameters to use to start the listening TLS server."
  (let (parms)
    (cond ((eq func 'make-network-process)
           (setq parms
                 '(:name "bar"
                   :buffer (generate-new-buffer "*foo*")
                   :service port)))
          ((eq func 'open-network-stream)
           (setq parms
                 '("bar"
                   (generate-new-buffer "*foo*")
                   "localhost"
                   port)))
          ((eq func 'open-gnutls-stream)
           (setq parms
                 '("bar"
                   (generate-new-buffer "*foo*")
                   "localhost"
                   port))
           ;; open-gnutls-stream has a different calling convention from
           ;; the other two, and we have to cater for the old api where
           ;; nowait is not specified with a plist.
           (when proc-parms
             (setq proc-parms (list proc-parms)))))
    `(let* ((s (make-tls-server ',server-parms))
            (server (car s))
            (port (cdr s))
            proc status)
       (unwind-protect
           (progn
             (setq proc (apply #',func ,@parms (list ,@proc-parms)))
             (should proc)
             ,proc-negotiate
             (skip-when (eq (process-status proc) 'connect)))
         (if (process-live-p server) (delete-process server)))
       (setq status (gnutls-peer-status proc))
       (should (consp status))
       (delete-process proc)
    ;; This sleep-for is needed for the native MS-Windows build.  If
    ;; it is removed, the next test mysteriously fails because the
    ;; initial part of the echo is not received.
       (sleep-for 0.1)
       (let ((issuer (plist-get (plist-get status :certificate) :issuer)))
         (should (stringp issuer))
         (setq issuer (split-string issuer ","))
         (should (equal (nth 3 issuer) "O=Emacs Test Servicess LLC"))))))

(ert-deftest connect-to-tls-ipv4-wait ()
  (skip-unless (executable-find "gnutls-serv"))
  (skip-unless (gnutls-available-p))
  (with-tls-params
   make-network-process
   (:host "localhost")
   (gnutls-negotiate :process proc
                     :type 'gnutls-x509pki
                     :hostname "localhost")))

(ert-deftest connect-to-tls-ipv4-nowait ()
  (skip-unless (executable-find "gnutls-serv"))
  (skip-unless (gnutls-available-p))
  (let ((times 0)
        (network-security-level 'low))
    (with-tls-params
     make-network-process
     (:nowait t
      :family 'ipv4
      :tls-parameters
      (cons 'gnutls-x509pki
            (gnutls-boot-parameters
             :hostname "localhost"))
      :host "localhost")
     (while (and (eq (process-status proc) 'connect)
                 (< (setq times (1+ times)) 10))
       (sit-for 0.1)))))

(ert-deftest connect-to-tls-ipv6-nowait ()
  (skip-unless (executable-find "gnutls-serv"))
  (skip-unless (gnutls-available-p))
  (skip-when (eq system-type 'windows-nt))
  (skip-unless (featurep 'make-network-process '(:family ipv6)))
  (let ((times 0)
        (network-security-level 'low))
    (with-tls-params
     make-network-process
     (:family 'ipv6
      :nowait t
      :tls-parameters
      (cons 'gnutls-x509pki
            (gnutls-boot-parameters
             :hostname "localhost"))
      :host "::1")
     (while (and (eq (process-status proc) 'connect)
                 (< (setq times (1+ times)) 10))
       (sit-for 0.1)))))

(ert-deftest open-network-stream-tls-wait ()
  (skip-unless (executable-find "gnutls-serv"))
  (skip-unless (gnutls-available-p))
  (let ((network-security-level 'low))
    (with-tls-params
     open-network-stream
     (:type 'tls
      :nowait nil))))

(ert-deftest open-network-stream-tls-nowait ()
  (skip-unless (executable-find "gnutls-serv"))
  (skip-unless (gnutls-available-p))
  (let ((network-security-level 'low)
        (times 0))
    (with-tls-params
     open-network-stream
     (:type 'tls
      :nowait t)
     (progn (while (and (eq (process-status proc) 'connect)
                 (< (setq times (1+ times)) 10))
              (sit-for 0.1))
            (skip-when (eq (process-status proc) 'connect))))))

(ert-deftest open-network-stream-tls ()
  (skip-unless (executable-find "gnutls-serv"))
  (skip-unless (gnutls-available-p))
  (let ((network-security-level 'low))
    (with-tls-params
     open-network-stream
     (:type 'tls))))

(ert-deftest open-network-stream-tls-nocert ()
  (skip-unless (executable-find "gnutls-serv"))
  (skip-unless (gnutls-available-p))
  (let ((network-security-level 'low))
    (with-tls-params
     open-network-stream
     (:type 'tls
      :client-certificate nil))))

(ert-deftest open-gnutls-stream-new-api-default ()
  (skip-unless (executable-find "gnutls-serv"))
  (skip-unless (gnutls-available-p))
  (with-tls-params
   open-gnutls-stream))

(ert-deftest open-gnutls-stream-new-api-wait ()
  (skip-unless (executable-find "gnutls-serv"))
  (skip-unless (gnutls-available-p))
  (with-tls-params
   open-gnutls-stream
   (list :nowait nil)))

(ert-deftest open-gnutls-stream-old-api-wait ()
  (skip-unless (executable-find "gnutls-serv"))
  (skip-unless (gnutls-available-p))
  (let ((nowait nil)) ; Workaround Bug#47080
    (with-tls-params
     open-gnutls-stream
     nowait)))

(ert-deftest open-gnutls-stream-new-api-nowait ()
  (skip-unless (executable-find "gnutls-serv"))
  (skip-unless (gnutls-available-p))
  (let ((times 0)
        (network-security-level 'low))
    (with-tls-params
     open-gnutls-stream
     (list :nowait t)
     (while (and (eq (process-status proc) 'connect)
                 (< (setq times (1+ times)) 10))
       (sit-for 0.1)))))

(ert-deftest open-gnutls-stream-old-api-nowait ()
  (skip-unless (executable-find "gnutls-serv"))
  (skip-unless (gnutls-available-p))
  (let ((times 0)
        (network-security-level 'low)
        (nowait t))
    (with-tls-params
     open-gnutls-stream
     nowait
     (while (and (eq (process-status proc) 'connect)
                 (< (setq times (1+ times)) 10))
       (sit-for 0.1)))))

(ert-deftest open-gnutls-stream-new-api-errors ()
  (skip-unless (gnutls-available-p))
  (should-error
   (open-gnutls-stream
    "bar"
    (generate-new-buffer "*foo*")
    "localhost"
    (+ 20000 (random 45535))
    (list t)))
  (should-error
   (open-gnutls-stream
    "bar"
    (generate-new-buffer "*foo*")
    "localhost"
    (+ 20000 (random 45535))
    (vector :nowait t))))

(ert-deftest check-network-process-coding-system-bind ()
  "Check that binding coding-system-for-{read,write} works."
  (let* ((coding-system-for-read 'binary)
         (coding-system-for-write 'utf-8-unix)
         (server
         (make-network-process
          :name "server"
          :server t
          :noquery t
          :family 'ipv4
          :service t
          :host 'local))
         (coding (process-coding-system server)))
    (should (eq (car coding) 'binary))
    (should (eq (cdr coding) 'utf-8-unix))
    (delete-process server)))

(ert-deftest check-network-process-coding-system-no-override ()
  "Check that coding-system-for-{read,write} is not overridden by :coding nil."
  (let* ((coding-system-for-read 'binary)
         (coding-system-for-write 'utf-8-unix)
         (server
         (make-network-process
          :name "server"
          :server t
          :noquery t
          :family 'ipv4
          :service t
          :coding nil
          :host 'local))
         (coding (process-coding-system server)))
    (should (eq (car coding) 'binary))
    (should (eq (cdr coding) 'utf-8-unix))
    (delete-process server)))

(ert-deftest check-network-process-coding-system-override ()
  "Check that :coding non-nil overrides coding-system-for-{read,write}."
  (let* ((coding-system-for-read 'binary)
         (coding-system-for-write 'utf-8-unix)
         (server
         (make-network-process
          :name "server"
          :server t
          :noquery t
          :family 'ipv4
          :service t
          :coding 'georgian-academy
          :host 'local))
         (coding (process-coding-system server)))
    (should (eq (car coding) 'georgian-academy))
    (should (eq (cdr coding) 'georgian-academy))
    (delete-process server)))
;;; network-stream-tests.el ends here
