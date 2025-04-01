(defpackage :lem/common/socket
  (:use :cl)
  (:import-from :usocket)
  (:export :port-available-p
           :random-available-port)
  #+sbcl
  (:lock t))
(in-package :lem/common/socket)

(defconstant +private-port-min+ 49152)
(defconstant +private-port-max+ 65535)

(defun port-available-p (port)
  (let (socket)
    (unwind-protect
         (handler-case (progn
                         (setq socket (usocket:socket-listen "127.0.0.1" port :reuse-address nil))
                         port)
           (usocket:address-in-use-error () nil)
           (usocket:socket-error (e)
             (warn "USOCKET:SOCKET-ERROR: ~A" e)
             nil)
           #+sbcl
           (sb-bsd-sockets:socket-error (e)
             (warn "SB-BSD-SOCKETS:SOCKET-ERROR: ~A" e)
             nil))
      (when socket
        (usocket:socket-close socket)
        port))))

(defun random-available-port (&rest deny-ports)
  (loop :for port := (random-port)
        :when (and (port-available-p port)
                   (not (find port deny-ports :test #'equal)))
        :return port))

(defun random-port ()
  (random-range +private-port-min+ +private-port-max+))

(defun random-range (min max &optional (state *random-state*))
  (+ min (random (1+ (- max min)) state)))
