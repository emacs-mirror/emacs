;;; network-stream-tests.el --- tests for network processes       -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2020 Free Software Foundation, Inc.

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

(require 'gnutls)
(require 'network-stream)
;; The require above is needed for 'open-network-stream' to work, but
;; it pulls in nsm, which then makes the :nowait t' tests fail unless
;; we disable the nsm, which we do by binding 'network-security-level'

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
  (message "Received %s" string)
  (let ((prev (process-get proc 'previous-string)))
    (when prev
      (setq string (concat prev string))
      (process-put proc 'previous-string nil)))
  (if (and (not (string-match "\n" string))
           (> (length string) 0))
      (process-put proc 'previous-string string))
  (let ((command (split-string string)))
    (cond
     ((equal (car command) "echo")
      (process-send-string proc (concat (cadr command) "\n")))
     (t
      ))))

(ert-deftest echo-server-with-dns ()
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
    (skip-unless (not (eq (process-status proc) 'connect)))
    (with-current-buffer (process-buffer proc)
      (process-send-string proc "echo foo")
      (sleep-for 0.1)
      (should (equal (buffer-string) "foo\n")))
    (delete-process server)))

(defconst network-stream-tests--datadir
  (expand-file-name "test/data/net" source-directory))

(defun make-tls-server (port)
  (start-process "gnutls" (generate-new-buffer "*tls*")
                 "gnutls-serv" "--http"
                 "--x509keyfile"
                 (concat network-stream-tests--datadir "/key.pem")
                 "--x509certfile"
                 (concat network-stream-tests--datadir "/cert.pem")
                 "--port" (format "%s" port)))

(ert-deftest connect-to-tls-ipv4-wait ()
  (skip-unless (executable-find "gnutls-serv"))
  (skip-unless (gnutls-available-p))
  (let ((server (make-tls-server 44332))
        (times 0)
        proc status)
    (unwind-protect
        (progn
          (sleep-for 1)
          (with-current-buffer (process-buffer server)
            (message "gnutls-serv: %s" (buffer-string)))

          ;; It takes a while for gnutls-serv to start.
          (while (and (null (ignore-errors
                              (setq proc (make-network-process
                                          :name "bar"
                                          :buffer (generate-new-buffer "*foo*")
                                          :host "localhost"
                                          :service 44332))))
                      (< (setq times (1+ times)) 10))
            (sit-for 0.1))
          (should proc)
          (gnutls-negotiate :process proc
                            :type 'gnutls-x509pki
                            :hostname "localhost"))
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
      (should (equal (nth 3 issuer) "O=Emacs Test Servicess LLC")))))

(ert-deftest connect-to-tls-ipv4-nowait ()
  (skip-unless (executable-find "gnutls-serv"))
  (skip-unless (gnutls-available-p))
  (let ((server (make-tls-server 44331))
        (times 0)
        (network-security-level 'low)
        proc status)
    (unwind-protect
        (progn
          (sleep-for 1)
          (with-current-buffer (process-buffer server)
            (message "gnutls-serv: %s" (buffer-string)))

          ;; It takes a while for gnutls-serv to start.
          (while (and (null (ignore-errors
                              (setq proc (make-network-process
                                          :name "bar"
                                          :buffer (generate-new-buffer "*foo*")
                                          :nowait t
                                          :tls-parameters
                                          (cons 'gnutls-x509pki
                                                (gnutls-boot-parameters
                                                 :hostname "localhost"))
                                          :host "localhost"
                                          :service 44331))))
                      (< (setq times (1+ times)) 10))
            (sit-for 0.1))
          (should proc)
          (setq times 0)
          (while (and (eq (process-status proc) 'connect)
                      (< (setq times (1+ times)) 10))
            (sit-for 0.1))
          (skip-unless (not (eq (process-status proc) 'connect))))
      (if (process-live-p server) (delete-process server)))
    (setq status (gnutls-peer-status proc))
    (should (consp status))
    (delete-process proc)
    (let ((issuer (plist-get (plist-get status :certificate) :issuer)))
      (should (stringp issuer))
      (setq issuer (split-string issuer ","))
      (should (equal (nth 3 issuer) "O=Emacs Test Servicess LLC")))))

(ert-deftest connect-to-tls-ipv6-nowait ()
  (skip-unless (executable-find "gnutls-serv"))
  (skip-unless (gnutls-available-p))
  (skip-unless (not (eq system-type 'windows-nt)))
  (skip-unless (featurep 'make-network-process '(:family ipv6)))
  (let ((server (make-tls-server 44333))
        (times 0)
        (network-security-level 'low)
        proc status)
    (unwind-protect
        (progn
          (sleep-for 1)
          (with-current-buffer (process-buffer server)
            (message "gnutls-serv: %s" (buffer-string)))

          ;; It takes a while for gnutls-serv to start.
          (while (and (null (ignore-errors
                              (setq proc (make-network-process
                                          :name "bar"
                                          :buffer (generate-new-buffer "*foo*")
                                          :family 'ipv6
                                          :nowait t
                                          :tls-parameters
                                          (cons 'gnutls-x509pki
                                                (gnutls-boot-parameters
                                                 :hostname "localhost"))
                                          :host "::1"
                                          :service 44333))))
                      (< (setq times (1+ times)) 10))
            (sit-for 0.1))
          (should proc)
          (setq times 0)
          (while (and (eq (process-status proc) 'connect)
                      (< (setq times (1+ times)) 10))
            (sit-for 0.1))
          (skip-unless (not (eq (process-status proc) 'connect))))
      (if (process-live-p server) (delete-process server)))
    (setq status (gnutls-peer-status proc))
    (should (consp status))
    (delete-process proc)
    (let ((issuer (plist-get (plist-get status :certificate) :issuer)))
      (should (stringp issuer))
      (setq issuer (split-string issuer ","))
      (should (equal (nth 3 issuer) "O=Emacs Test Servicess LLC")))))

(ert-deftest open-network-stream-tls-wait ()
  (skip-unless (executable-find "gnutls-serv"))
  (skip-unless (gnutls-available-p))
  (let ((server (make-tls-server 44334))
        (times 0)
        (network-security-level 'low)
        proc status)
    (unwind-protect
        (progn
          (sleep-for 1)
          (with-current-buffer (process-buffer server)
            (message "gnutls-serv: %s" (buffer-string)))

          ;; It takes a while for gnutls-serv to start.
          (while (and (null (ignore-errors
                              (setq proc (open-network-stream
                                          "bar"
                                          (generate-new-buffer "*foo*")
                                          "localhost"
                                          44334
                                          :type 'tls
                                          :nowait nil))))
                      (< (setq times (1+ times)) 10))
            (sit-for 0.1))
          (should proc)
          (skip-unless (not (eq (process-status proc) 'connect))))
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
      (should (equal (nth 3 issuer) "O=Emacs Test Servicess LLC")))))

(ert-deftest open-network-stream-tls-nowait ()
  (skip-unless (executable-find "gnutls-serv"))
  (skip-unless (gnutls-available-p))
  (let ((server (make-tls-server 44335))
        (times 0)
        (network-security-level 'low)
        proc status)
    (unwind-protect
        (progn
          (sleep-for 1)
          (with-current-buffer (process-buffer server)
            (message "gnutls-serv: %s" (buffer-string)))

          ;; It takes a while for gnutls-serv to start.
          (while (and (null (ignore-errors
                              (setq proc (open-network-stream
                                          "bar"
                                          (generate-new-buffer "*foo*")
                                          "localhost"
                                          44335
                                          :type 'tls
                                          :nowait t))))
                      (< (setq times (1+ times)) 10))
            (sit-for 0.1))
          (should proc)
          (setq times 0)
          (while (and (eq (process-status proc) 'connect)
                      (< (setq times (1+ times)) 10))
            (sit-for 0.1))
          (skip-unless (not (eq (process-status proc) 'connect))))
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
      (should (equal (nth 3 issuer) "O=Emacs Test Servicess LLC")))))

(ert-deftest open-network-stream-tls ()
  (skip-unless (executable-find "gnutls-serv"))
  (skip-unless (gnutls-available-p))
  (let ((server (make-tls-server 44336))
        (times 0)
        (network-security-level 'low)
        proc status)
    (unwind-protect
        (progn
          (sleep-for 1)
          (with-current-buffer (process-buffer server)
            (message "gnutls-serv: %s" (buffer-string)))

          ;; It takes a while for gnutls-serv to start.
          (while (and (null (ignore-errors
                              (setq proc (open-network-stream
                                          "bar"
                                          (generate-new-buffer "*foo*")
                                          "localhost"
                                          44336
                                          :type 'tls))))
                      (< (setq times (1+ times)) 10))
            (sit-for 0.1))
          (should proc)
          (skip-unless (not (eq (process-status proc) 'connect))))
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
      (should (equal (nth 3 issuer) "O=Emacs Test Servicess LLC")))))

(ert-deftest open-network-stream-tls-nocert ()
  (skip-unless (executable-find "gnutls-serv"))
  (skip-unless (gnutls-available-p))
  (let ((server (make-tls-server 44337))
        (times 0)
        (network-security-level 'low)
        proc status)
    (unwind-protect
        (progn
          (sleep-for 1)
          (with-current-buffer (process-buffer server)
            (message "gnutls-serv: %s" (buffer-string)))

          ;; It takes a while for gnutls-serv to start.
          (while (and (null (ignore-errors
                              (setq proc (open-network-stream
                                          "bar"
                                          (generate-new-buffer "*foo*")
                                          "localhost"
                                          44337
                                          :type 'tls
                                          :client-certificate nil))))
                      (< (setq times (1+ times)) 10))
            (sit-for 0.1))
          (should proc)
          (skip-unless (not (eq (process-status proc) 'connect))))
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
      (should (equal (nth 3 issuer) "O=Emacs Test Servicess LLC")))))

(ert-deftest open-gnutls-stream-new-api-default ()
  (skip-unless (executable-find "gnutls-serv"))
  (skip-unless (gnutls-available-p))
  (let ((server (make-tls-server 44665))
        (times 0)
        proc status)
    (unwind-protect
        (progn
          (sleep-for 1)
          (with-current-buffer (process-buffer server)
            (message "gnutls-serv: %s" (buffer-string)))

          ;; It takes a while for gnutls-serv to start.
          (while (and (null (ignore-errors
                              (setq proc (open-gnutls-stream
                                          "bar"
                                          (generate-new-buffer "*foo*")
                                          "localhost"
                                          44665))))
                      (< (setq times (1+ times)) 10))
            (sit-for 0.1))
          (should proc)
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

(ert-deftest open-gnutls-stream-new-api-wait ()
  (skip-unless (executable-find "gnutls-serv"))
  (skip-unless (gnutls-available-p))
  (let ((server (make-tls-server 44666))
        (times 0)
        proc status)
    (unwind-protect
        (progn
          (sleep-for 1)
          (with-current-buffer (process-buffer server)
            (message "gnutls-serv: %s" (buffer-string)))

          ;; It takes a while for gnutls-serv to start.
          (while (and (null (ignore-errors
                              (setq proc (open-gnutls-stream
                                          "bar"
                                          (generate-new-buffer "*foo*")
                                          "localhost"
                                          44666
                                          (list :nowait nil)))))
                      (< (setq times (1+ times)) 10))
            (sit-for 0.1))
          (should proc)
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

(ert-deftest open-gnutls-stream-old-api-wait ()
  (skip-unless (executable-find "gnutls-serv"))
  (skip-unless (gnutls-available-p))
  (let ((server (make-tls-server 44667))
        (times 0)
        nowait
        proc status)
    (unwind-protect
        (progn
          (sleep-for 1)
          (with-current-buffer (process-buffer server)
            (message "gnutls-serv: %s" (buffer-string)))

          ;; It takes a while for gnutls-serv to start.
          (while (and (null (ignore-errors
                              (setq proc (open-gnutls-stream
                                          "bar"
                                          (generate-new-buffer "*foo*")
                                          "localhost"
                                          44667
                                          nowait))))
                      (< (setq times (1+ times)) 10))
            (sit-for 0.1))
          (should proc)
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

(ert-deftest open-gnutls-stream-new-api-nowait ()
  (skip-unless (executable-find "gnutls-serv"))
  (skip-unless (gnutls-available-p))
  (let ((server (make-tls-server 44668))
        (times 0)
        (network-security-level 'low)
        proc status)
    (unwind-protect
        (progn
          (sleep-for 1)
          (with-current-buffer (process-buffer server)
            (message "gnutls-serv: %s" (buffer-string)))

          ;; It takes a while for gnutls-serv to start.
          (while (and (null (ignore-errors
                              (setq proc (open-gnutls-stream
                                          "bar"
                                          (generate-new-buffer "*foo*")
                                          "localhost"
                                          44668
                                          (list :nowait t)))))
                      (< (setq times (1+ times)) 10))
            (sit-for 0.1))
          (should proc)
          (setq times 0)
          (while (and (eq (process-status proc) 'connect)
                      (< (setq times (1+ times)) 10))
            (sit-for 0.1))
          (skip-unless (not (eq (process-status proc) 'connect))))
      (if (process-live-p server) (delete-process server)))
    (setq status (gnutls-peer-status proc))
    (should (consp status))
    (delete-process proc)
    (let ((issuer (plist-get (plist-get status :certificate) :issuer)))
      (should (stringp issuer))
      (setq issuer (split-string issuer ","))
      (should (equal (nth 3 issuer) "O=Emacs Test Servicess LLC")))))

(ert-deftest open-gnutls-stream-old-api-nowait ()
  (skip-unless (executable-find "gnutls-serv"))
  (skip-unless (gnutls-available-p))
  (let ((server (make-tls-server 44669))
        (times 0)
        (network-security-level 'low)
        (nowait t)
        proc status)
    (unwind-protect
        (progn
          (sleep-for 1)
          (with-current-buffer (process-buffer server)
            (message "gnutls-serv: %s" (buffer-string)))

          ;; It takes a while for gnutls-serv to start.
          (while (and (null (ignore-errors
                              (setq proc (open-gnutls-stream
                                          "bar"
                                          (generate-new-buffer "*foo*")
                                          "localhost"
                                          44669
                                          nowait))))
                      (< (setq times (1+ times)) 10))
            (sit-for 0.1))
          (should proc)
          (setq times 0)
          (while (and (eq (process-status proc) 'connect)
                      (< (setq times (1+ times)) 10))
            (sit-for 0.1))
          (skip-unless (not (eq (process-status proc) 'connect))))
      (if (process-live-p server) (delete-process server)))
    (setq status (gnutls-peer-status proc))
    (should (consp status))
    (delete-process proc)
    (let ((issuer (plist-get (plist-get status :certificate) :issuer)))
      (should (stringp issuer))
      (setq issuer (split-string issuer ","))
      (should (equal (nth 3 issuer) "O=Emacs Test Servicess LLC")))))

(ert-deftest open-gnutls-stream-new-api-errors ()
  (skip-unless (gnutls-available-p))
  (should-error
   (open-gnutls-stream
    "bar"
    (generate-new-buffer "*foo*")
    "localhost"
    44777
    (list t)))
  (should-error
   (open-gnutls-stream
    "bar"
    (generate-new-buffer "*foo*")
    "localhost"
    44777
    (vector :nowait t))))

;;; network-stream-tests.el ends here
