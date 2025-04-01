(uiop:define-package :lem-copilot/sign-in
  (:use :cl :lem)
  (:local-nicknames (:lem-copilot :lem-copilot)
                    (:client :lem-copilot/client)
                    (:copilot :lem-copilot))
  (:export :copilot-signin))
(in-package :lem-copilot/sign-in)

(define-condition already-sign-in (editor-error) ())

(defvar *sign-in-message* nil)

(defun make-verification-buffer (user-code verification-uri)
  (let* ((buffer (make-buffer "*GitHub Copilot Verification*" :temporary t))
         (point (buffer-point buffer)))
    (setf (variable-value 'line-wrap :buffer buffer) nil)
    (erase-buffer buffer)
    (insert-string point
                   (format nil
                           "Code: ~A (Copied to clipboard) ~%please paste it into your browser.~%~A~2%"
                           user-code
                           verification-uri))
    (insert-string point "Authenticate... (Close this window with Escape or C-g.)")
    (buffer-start point)
    buffer))

(defun start-sign-in (user-code verification-uri)
  (setf *sign-in-message* (display-popup-message
                         (make-verification-buffer user-code verification-uri)
                         :style '(:gravity :center)
                         :timeout nil))
  (add-hook *editor-abort-hook* 'abort-sign-in))

(defun abort-sign-in ()
  (delete-sign-in-message)
  (remove-hook *editor-abort-hook* 'abort-sign-in))

(defun delete-sign-in-message ()
  (when *sign-in-message*
    (delete-popup-message *sign-in-message*)
    (setf *sign-in-message* nil)))

(define-command copilot-signin () ()
  (unless (copilot::installed-copilot-server-p)
    (lem-copilot/install:copilot-install-server))
  (copilot::setup-client-async
   (lambda ()
     (let* ((response (client:sign-in-initiate (copilot::client)))
            (status (gethash "status" response))
            (user-code (gethash "userCode" response))
            (verification-uri (gethash "verificationUri" response))
            (user (gethash "user" response)))
       (when (equal status "AlreadySignedIn")
         (error 'already-sign-in :message (format nil "Already sign in as ~A" user)))
       (copy-to-clipboard user-code)
       (start-sign-in user-code verification-uri)
       (open-external-file verification-uri)
       (redraw-display)
       (let ((finished nil))
         (client:sign-in-confirm
          (copilot::client)
          user-code
          :callback (lambda (response)
                      (send-event (lambda ()
                                    (assert (equal "OK" (gethash "status" response)))
                                    (show-message (format nil "Authenticated as ~A" (gethash "user" response))
                                                  :style '(:gravity :center))
                                    (delete-sign-in-message)
                                    (setf finished t)
                                    (redraw-display)))))
         (handler-bind ((editor-abort (lambda (c)
                                        (declare (ignore c))
                                        (delete-sign-in-message))))

           (loop :until finished
                 :do (sit-for 1)))
         (copilot::enable-copilot))))))
