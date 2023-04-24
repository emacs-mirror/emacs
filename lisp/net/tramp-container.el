;;; tramp-container.el --- Tramp integration for Docker-like containers  -*- lexical-binding: t; -*-

;; Copyright © 2022-2023 Free Software Foundation, Inc.

;; Author: Brian Cully <bjc@kublai.com>
;; Keywords: comm, processes
;; Package: tramp

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

;; Allows Tramp access to environments provided by Docker and similar
;; programs.
;;
;; ## Usage
;;
;; Open a file on a running Docker container:
;;
;;     C-x C-f /docker:USER@CONTAINER:/path/to/file
;;
;; or Podman:
;;
;;     C-x C-f /podman:USER@CONTAINER:/path/to/file
;;
;; Where:
;;     USER          is the user on the container to connect as (optional)
;;     CONTAINER     is the container to connect to
;;
;;
;;
;; Open file in a Kubernetes container:
;;
;;     C-x C-f /kubernetes:POD:/path/to/file
;;
;; Where:
;;     POD	is the pod to connect to.
;;		By default, the first container in that pod will be
;;		used.
;;
;; Completion for POD and accessing it operate in the current
;; namespace, use this command to change it:
;;
;; "kubectl config set-context --current --namespace=<name>"
;;
;;
;;
;; Open a file on an existing Toolbox container:
;;
;;     C-x C-f /toolbox:CONTAINER:/path/to/file
;;
;; Where:
;;     CONTAINER     is the container to connect to (optional)
;;
;; If the container is not running, it is started.  If no container is
;; specified, the default Toolbox container is used.
;;
;;
;;
;; Open a file on a running Flatpak sandbox:
;;
;;     C-x C-f /flatpak:SANDBOX:/path/to/file
;;
;; Where:
;;     SANDBOX	is the running sandbox to connect to.
;;		It could be an application ID, an instance ID, or a PID.

;;; Code:

(require 'tramp)

;;;###tramp-autoload
(defcustom tramp-docker-program "docker"
  "Name of the Docker client program."
  :group 'tramp
  :version "29.1"
  :type '(choice (const "docker")
                 (string)))

;;;###tramp-autoload
(defcustom tramp-podman-program "podman"
  "Name of the Podman client program."
  :group 'tramp
  :version "29.1"
  :type '(choice (const "podman")
                 (string)))

;;;###tramp-autoload
(defcustom tramp-kubernetes-program "kubectl"
  "Name of the Kubernetes client program."
  :group 'tramp
  :version "29.1"
  :type '(choice (const "kubectl")
                 (string)))

;;;###tramp-autoload
(defcustom tramp-toolbox-program "toolbox"
  "Name of the Toolbox client program."
  :group 'tramp
  :version "30.1"
  :type '(choice (const "toolbox")
                 (string)))

;;;###tramp-autoload
(defcustom tramp-flatpak-program "flatpak"
  "Name of the Flatpak client program."
  :group 'tramp
  :version "30.1"
  :type '(choice (const "flatpak")
                 (string)))

;;;###tramp-autoload
(defconst tramp-docker-method "docker"
  "Tramp method name to use to connect to Docker containers.")

;;;###tramp-autoload
(defconst tramp-podman-method "podman"
  "Tramp method name to use to connect to Podman containers.")

;;;###tramp-autoload
(defconst tramp-kubernetes-method "kubernetes"
  "Tramp method name to use to connect to Kubernetes containers.")

;;;###tramp-autoload
(defconst tramp-toolbox-method "toolbox"
  "Tramp method name to use to connect to Toolbox containers.")

;;;###tramp-autoload
(defconst tramp-flatpak-method "flatpak"
  "Tramp method name to use to connect to Flatpak sandboxes.")

;;;###tramp-autoload
(defun tramp-container--completion-function (program)
  "List running containers available for connection.
PROGRAM is the program to be run for \"ps\", either
`tramp-docker-program' or `tramp-podman-program'.

This function is used by `tramp-set-completion-function', please
see its function help for a description of the format."
  (when-let ((default-directory tramp-compat-temporary-file-directory)
	     (raw-list (shell-command-to-string
			(concat program " ps --format '{{.ID}}\t{{.Names}}'")))
             (lines (split-string raw-list "\n" 'omit))
             (names (mapcar
		     (lambda (line)
                       (when (string-match
			      (rx bol (group (1+ nonl))
				  "\t" (? (group (1+ nonl))) eol)
			      line)
			 (or (match-string 2 line) (match-string 1 line))))
                     lines)))
    (mapcar (lambda (name) (list nil name)) (delq nil names))))

;;;###tramp-autoload
(defun tramp-kubernetes--completion-function (&rest _args)
  "List Kubernetes pods available for connection.

This function is used by `tramp-set-completion-function', please
see its function help for a description of the format."
  (when-let ((default-directory tramp-compat-temporary-file-directory)
	     (raw-list (shell-command-to-string
			(concat tramp-kubernetes-program
                                " get pods --no-headers "
                                "-o custom-columns=NAME:.metadata.name")))
             (names (split-string raw-list "\n" 'omit)))
    (mapcar (lambda (name) (list nil name)) (delq nil names))))

(defun tramp-kubernetes--current-context-data (vec)
  "Return Kubernetes current context data as JSON string."
  (with-temp-buffer
    (when (zerop
	   (tramp-call-process
	    vec tramp-kubernetes-program nil t nil
	    "config" "current-context"))
      (goto-char (point-min))
      (let ((current-context (buffer-substring (point) (line-end-position))))
	(erase-buffer)
	(when (zerop
	       (tramp-call-process
		vec tramp-kubernetes-program nil t nil
		"config" "view" "-o"
		(format
		 "jsonpath='{.contexts[?(@.name == \"%s\")]}'" current-context)))
	  (buffer-string))))))

;;;###tramp-autoload
(defun tramp-toolbox--completion-function (&rest _args)
  "List Toolbox containers available for connection.

This function is used by `tramp-set-completion-function', please
see its function help for a description of the format."
  (when-let ((default-directory tramp-compat-temporary-file-directory)
	     (raw-list (shell-command-to-string
			(concat tramp-toolbox-program " list -c")))
	     ;; Ignore header line.
             (lines (cdr (split-string raw-list "\n" 'omit)))
             (names (mapcar
		     (lambda (line)
                       (when (string-match
			      (rx bol (1+ (not space))
				  (1+ space) (group (1+ (not space))) space)
			      line)
			 (match-string 1 line)))
                     lines)))
    (mapcar (lambda (name) (list nil name)) (delq nil names))))

;;;###tramp-autoload
(defun tramp-flatpak--completion-function (&rest _args)
  "List Flatpak sandboxes available for connection.
It returns application IDs or, in case there is no application
ID, instance IDs.

This function is used by `tramp-set-completion-function', please
see its function help for a description of the format."
  (when-let ((default-directory tramp-compat-temporary-file-directory)
	     (raw-list
	      (shell-command-to-string
	       (concat tramp-flatpak-program
		       " ps --columns=instance,application")))
             (lines (split-string raw-list "\n" 'omit))
             (names (mapcar
		     (lambda (line)
                       (when (string-match
			      (rx bol (* space) (group (+ (not space)))
				  (? (+ space) (group (+ (not space)))) eol)
			      line)
			 (or (match-string 2 line) (match-string 1 line))))
                     lines)))
    (mapcar (lambda (name) (list nil name)) (delq nil names))))

;;;###tramp-autoload
(defvar tramp-default-remote-shell) ;; Silence byte compiler.

;;;###tramp-autoload
(tramp--with-startup
 (add-to-list 'tramp-methods
              `(,tramp-docker-method
                (tramp-login-program ,tramp-docker-program)
                (tramp-login-args (("exec")
                                   ("-it")
                                   ("-u" "%u")
                                   ("%h")
			           ("%l")))
		(tramp-direct-async (,tramp-default-remote-shell "-c"))
                (tramp-remote-shell ,tramp-default-remote-shell)
                (tramp-remote-shell-login ("-l"))
                (tramp-remote-shell-args ("-i" "-c"))))

 (add-to-list 'tramp-methods
              `(,tramp-podman-method
                (tramp-login-program ,tramp-podman-program)
                (tramp-login-args (("exec")
                                   ("-it")
                                   ("-u" "%u")
                                   ("%h")
			           ("%l")))
		(tramp-direct-async (,tramp-default-remote-shell "-c"))
                (tramp-remote-shell ,tramp-default-remote-shell)
                (tramp-remote-shell-login ("-l"))
                (tramp-remote-shell-args ("-i" "-c"))))

 (add-to-list 'tramp-methods
              `(,tramp-kubernetes-method
                (tramp-login-program ,tramp-kubernetes-program)
                (tramp-login-args (("exec")
                                   ("%h")
                                   ("-it")
                                   ("--")
			           ("%l")))
		(tramp-config-check tramp-kubernetes--current-context-data)
		(tramp-direct-async (,tramp-default-remote-shell "-c"))
                (tramp-remote-shell ,tramp-default-remote-shell)
                (tramp-remote-shell-login ("-l"))
                (tramp-remote-shell-args ("-i" "-c"))))

 (add-to-list 'tramp-methods
	      `(,tramp-toolbox-method
		(tramp-login-program ,tramp-toolbox-program)
		(tramp-login-args (("run")
				   ("-c" "%h")
				   ("%l")))
		(tramp-direct-async (,tramp-default-remote-shell "-c"))
		(tramp-remote-shell ,tramp-default-remote-shell)
		(tramp-remote-shell-login ("-l"))
		(tramp-remote-shell-args ("-c"))))

 (add-to-list 'tramp-default-host-alist `(,tramp-toolbox-method nil ""))

 (add-to-list 'tramp-methods
	      `(,tramp-flatpak-method
		(tramp-login-program ,tramp-flatpak-program)
		(tramp-login-args (("enter")
				   ("%h")
				   ("%l")))
		(tramp-direct-async (,tramp-default-remote-shell "-c"))
		(tramp-remote-shell ,tramp-default-remote-shell)
		(tramp-remote-shell-login ("-l"))
		(tramp-remote-shell-args ("-c"))))

 (tramp-set-completion-function
  tramp-docker-method
  `((tramp-container--completion-function
     ,(executable-find tramp-docker-program))))

 (tramp-set-completion-function
  tramp-podman-method
  `((tramp-container--completion-function
     ,(executable-find tramp-podman-program))))

 (tramp-set-completion-function
  tramp-kubernetes-method
  '((tramp-kubernetes--completion-function "")))

 (tramp-set-completion-function
  tramp-toolbox-method
  '((tramp-toolbox--completion-function "")))

 (tramp-set-completion-function
  tramp-flatpak-method
  '((tramp-flatpak--completion-function "")))

 ;; Default connection-local variables for Tramp.

 (defconst tramp-container-connection-local-default-flatpak-variables
   `((tramp-remote-path  . ,(cons "/app/bin" tramp-remote-path)))
   "Default connection-local variables for remote flatpak connections.")

 (connection-local-set-profile-variables
  'tramp-container-connection-local-default-flatpak-profile
  tramp-container-connection-local-default-flatpak-variables)

 (connection-local-set-profiles
  `(:application tramp :protocol ,tramp-flatpak-method)
  'tramp-container-connection-local-default-flatpak-profile))

(add-hook 'tramp-unload-hook
	  (lambda ()
	    (unload-feature 'tramp-container 'force)))

(provide 'tramp-container)

;;; tramp-container.el ends here
