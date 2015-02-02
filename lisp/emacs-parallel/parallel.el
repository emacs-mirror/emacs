;; -*- lexical-binding: t; -*-
;;; parallel.el ---

;; Copyright (C) 2013 Grégoire Jadi

;; Author: Grégoire Jadi <gregoire.jadi@gmail.com>

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'cl)
(require 'parallel-remote)

(defgroup parallel nil
  "Execute stuff in parallel"
  :group 'emacs)

(defcustom parallel-sleep 0.05
  "How many sec should we wait while polling."
  :type 'number
  :group 'parallel)

(defcustom parallel-config nil
  "Global config setting to use."
  :type 'plist
  :group 'parallel)

(defvar parallel--server nil)
(defvar parallel--tasks nil)
(defvar parallel--tunnels nil)

;; Declare external function
(declare-function parallel-send "parallel-remote")

(defun parallel-make-tunnel (username hostname)
  (parallel--init-server)
  (let ((tunnel (find-if (lambda (tun)
                           (and (string= username
                                         (process-get tun 'username))
                                (string= hostname
                                         (process-get tun 'hostname))))
                         parallel--tunnels)))
    (unless tunnel
      (setq tunnel (start-process "parallel-ssh" nil "ssh"
                                  "-N" "-R" (format "0:localhost:%s"
                                                    (process-contact parallel--server :service))
                                  (format "%s@%s" username hostname)))
      (process-put tunnel 'username username)
      (process-put tunnel 'hostname hostname)
      (set-process-filter tunnel #'parallel--tunnel-filter)
      (while (null (process-get tunnel 'service))
        (sleep-for 0.01))
      (push tunnel parallel--tunnels))
    tunnel))

(defun parallel-stop-tunnel (tunnel)
  (setq parallel--tunnels (delq tunnel parallel--tunnels))
  (delete-process tunnel))

(defun parallel--tunnel-filter (proc output)
  (if (string-match "\\([0-9]+\\)" output)
      (process-put proc 'service (match-string 1 output))))

(defmacro parallel--set-option (place config)
  `(setf ,place (or ,place
                    (plist-get ,config ,(intern (format ":%s" (symbol-name place))))
                    (plist-get parallel-config ,(intern (format ":%s" (symbol-name place)))))))

(defmacro parallel--set-options (config &rest options)
  `(progn
     ,@(loop for option in options
             collect `(parallel--set-option ,option ,config))))

(defun* parallel-start (exec-fun &key post-exec env timeout
                                 emacs-path library-path emacs-args
                                 graphical debug on-event continue-when-executed
                                 username hostname hostport
                                 config)
  (parallel--init-server)

  ;; Initialize parameters
  (parallel--set-options config
                         post-exec
                         env
                         timeout
                         emacs-args
                         graphical
                         debug
                         on-event
                         continue-when-executed
                         username
                         hostname
                         hostport)

  (setq emacs-path (or emacs-path
                       (plist-get config :emacs-path)
                       (plist-get parallel-config :emacs-path)
                       (expand-file-name invocation-name
                                         invocation-directory))
        library-path (or library-path
                         (plist-get config :library-path)
                         (plist-get parallel-config :library-path)
                         (locate-library "parallel-remote")))

  (let ((task (parallel--new-task))
        proc tunnel ssh-args)
    (push task parallel--tasks)
    (put task 'initialized nil)
    (put task 'exec-fun exec-fun)
    (put task 'env env)
    (when (functionp post-exec)
      (put task 'post-exec post-exec))
    (when (functionp on-event)
      (put task 'on-event on-event))
    (put task 'results nil)
    (put task 'status 'run)
    (put task 'queue nil)

    ;; We need to get the tunnel if it exists so we can send the right
    ;; `service' to the remote.
    (when (and username hostname)
      (if hostport
          (setq ssh-args (list "-R" (format "%s:localhost:%s" hostport
                                            (process-contact parallel--server :service)))
                tunnel t)
        (setq tunnel (parallel-make-tunnel username hostname)
              hostport (process-get tunnel 'service)))
      (setq ssh-args (append
                      ssh-args
                      (if graphical (list "-X"))
                      (list (format "%s@%s" username hostname)))))
    (setq emacs-args (remq nil
                           (list* "-Q" "-l" library-path
                                  (if graphical nil "-batch")
                                  "--eval" (format "(setq parallel-service '%S)"
                                                   (if tunnel
                                                       hostport
                                                     (process-contact parallel--server :service)))
                                  "--eval" (format "(setq parallel-task-id '%S)" task)
                                  "--eval" (format "(setq debug-on-error '%S)" debug)
                                  "--eval" (format "(setq parallel-continue-when-executed '%S)" continue-when-executed)
                                  "-f" "parallel-remote--init"
                                  emacs-args)))

    ;; Reformat emacs-args if we use a tunnel (escape string)
    (when tunnel
      (setq emacs-args (list (mapconcat (lambda (string)
                                          (if (find ?' string)
                                              (prin1-to-string string)
                                            string))
                                        emacs-args " "))))
    (setq proc (apply #'start-process "parallel" nil
                      `(,@(when tunnel
                            (list* "ssh" ssh-args))
                        ,emacs-path
                        ,@emacs-args)))
    (put task 'proc proc)
    (set-process-sentinel (get task 'proc) #'parallel--sentinel)
    (when timeout
      (run-at-time timeout nil (lambda ()
                                 (when (memq (parallel-status task)
                                             '(run stop))
                                   (parallel-stop task)))))
    task))

(defun parallel--new-task ()
  "Generate a new task by enforcing a unique name."
  (let ((symbol-name (make-temp-name "parallel-task-")))
    (while (intern-soft symbol-name)
      (setq symbol-name (make-temp-name "parallel-task-")))
    (intern symbol-name)))

(defun parallel--init-server ()
  "Initialize `parallel--server'."
  (when (or (null parallel--server)
            (not (eq (process-status parallel--server)
                     'listen)))
    (setq parallel--server
          (make-network-process :name "parallel-server"
                                :buffer nil
                                :server t
                                :host "localhost"
                                :service t
                                :family 'ipv4
                                :filter #'parallel--filter
                                :filter-multibyte t))))

(defun parallel--get-task-process (proc)
  "Return the task running the given PROC."
  (find-if (lambda (task)
             (eq (get task 'proc) proc))
           parallel--tasks))

(defun parallel--sentinel (proc _event)
  "Sentinel to watch over the remote process.

This function do the necessary cleanup when the remote process is
finished."
  (when (memq (process-status proc) '(exit signal))
    (let* ((task (parallel--get-task-process proc))
           (results (get task 'results))
           (status (process-status proc)))
      ;; 0 means that the remote process has terminated normally (no
      ;; SIGNUM 0).
      (if (zerop (process-exit-status proc))
          (setq status 'success)
        ;; on failure, push the exit-code or signal number on the
        ;; results stack.
        (push (process-exit-status proc) results))
      (put task 'results results)
      (put task 'status status)

      (when (functionp (get task 'post-exec))
        (funcall (get task 'post-exec)
                 results status))
      (setq parallel--tasks (delq task parallel--tasks)))))

(defun parallel--call-with-env (fun env)
  "Return a string which can be READ/EVAL by the remote process
to `funcall' FUN with ENV as arguments."
  (format "(funcall (read %S) %s)"
          (prin1-to-string fun)
          (mapconcat (lambda (obj)
                       ;; We need to quote it because the remote
                       ;; process will READ/EVAL it.
                       (format "'%S" obj)) env " ")))

(defun parallel--filter (connection output)
  "Server filter used to retrieve the results send by the remote
process and send the code to be executed by it."
  (dolist (data (parallel--read-output output))
    (parallel--process-output connection (first data) (rest data))))

(defun parallel--process-output (connection task result)
  (put task 'connection connection)
  (cond ((and (not (get task 'initialized))
              (eq result 'code))
         (apply #'parallel-send
                task
                (get task 'exec-fun)
                (get task 'env))
         (let ((code nil))
           (while (setq code (pop (get task 'queue)))
             (apply #'parallel-send task (car code) (cdr code))))
         (put task 'initialized t))
        (t
         (push result (get task 'results))
         (if (functionp (get task 'on-event))
             (funcall (get task 'on-event) result)))))

(defun parallel-ready-p (task)
  "Determine whether TASK is finished and if the results are
available."
  (memq (parallel-status task) '(success exit signal)))

(defun parallel-get-result (task)
  "Return the last result send by the remote call, that is the
result returned by exec-fun."
  (first (parallel-get-results task)))

(defun parallel-get-results (task)
  "Return all results send during the call of exec-fun."
  (parallel-wait task)
  (get task 'results))

(defun parallel-success-p (task)
  "Determine whether TASK has ended successfully."
  (parallel-wait task)
  (eq (parallel-status task) 'success))

(defun parallel-status (task)
  "Return TASK status."
  (get task 'status))

(defun parallel-wait (task)
  "Wait for TASK."
  (while (not (parallel-ready-p task))
    (sleep-for parallel-sleep))
  t)                                    ; for REPL

(defun parallel-stop (task)
  "Stop TASK."
  (delete-process (get task 'proc)))

(defun parallel-send (task fun &rest env)
  "Send FUN to be evaluated by TASK in ENV."
  (let ((connection (get task 'connection)))
    (if connection
        (process-send-string
         connection
         (parallel--call-with-env fun env))
      (push (cons fun env) (get task 'queue)))))

(provide 'parallel)

;;; parallel.el ends here
