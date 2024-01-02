;;; tramp-container.el --- Tramp integration for Docker-like containers  -*- lexical-binding: t; -*-

;; Copyright © 2022-2024 Free Software Foundation, Inc.

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
;; Open file in a Kubernetes container:
;;
;;     C-x C-f /kubernetes:POD:/path/to/file
;;
;; Where:
;;     POD     is the pod to connect to.
;;             By default, the first container in that pod will be
;;             used.
;;
;; Completion for POD and accessing it operate in the current
;; namespace, use this command to change it:
;;
;; "kubectl config set-context --current --namespace=<name>"

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
(defconst tramp-docker-method "docker"
  "Tramp method name to use to connect to Docker containers.")

;;;###tramp-autoload
(defconst tramp-podman-method "podman"
  "Tramp method name to use to connect to Podman containers.")

;;;###tramp-autoload
(defconst tramp-kubernetes-method "kubernetes"
  "Tramp method name to use to connect to Kubernetes containers.")

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
    (mapcar (lambda (name) (list nil name)) names)))

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
  '((tramp-kubernetes--completion-function ""))))

(add-hook 'tramp-unload-hook
	  (lambda ()
	    (unload-feature 'tramp-container 'force)))

(provide 'tramp-container)

;;; tramp-container.el ends here
