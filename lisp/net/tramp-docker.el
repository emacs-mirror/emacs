;;; tramp-docker.el --- Tramp integration for Docker containers  -*- lexical-binding: t; -*-

;; Copyright © 2022 Free Software Foundation, Inc.

;; Author: Brian Cully <bjc@kublai.com>
;; Maintainer: Brian Cully <bjc@kublai.com>
;; URL: https://git.spork.org/tramp-docker
;; Keywords: tramp, docker
;; Version: 0.99.1
;; Package-Requires: ((emacs "23"))

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;
;; ‘tramp-docker’ allows Tramp access to environments provided by
;; Docker.
;;
;; ## Usage
;;
;; Call ‘tramp-docker-setup’ in your Emacs initialization.
;;
;;     (add-hook 'after-init-hook 'tramp-docker-setup)
;;
;; Open a file on a running systemd-docker container:
;;
;;     C-x C-f /docker:USER@CONTAINER:/path/to/file
;;
;; Where:
;;     USER          is the user on the container to connect as (optional)
;;     CONTAINER     is the container to connect to
;;

;;; Code:

(require 'tramp)

(defgroup tramp-docker nil
  "Tramp integration for Docker containers."
  :prefix "tramp-docker-"
  :group 'applications
  :link '(url-link :tag "repo" "https://git.spork.org/tramp-docker.git")
  :link '(emacs-commentary-link :tag "Commentary" "tramp-docker"))

(defcustom tramp-docker-program "docker"
  "Name of the Docker client program."
  :type '(choice (const "docker")
                 (const "podman")
                 (string))
  :group 'tramp-docker)

(defconst tramp-docker-method "docker"
  "Tramp method name to use to connect to Docker containers.")

(defun tramp-docker--completion-function (&rest _args)
  "List Docker containers available for connection.

This function is used by ‘tramp-set-completion-function’, please
see its function help for a description of the format."
  (let* ((raw-list (shell-command-to-string
                    (concat tramp-docker-program
                            " ps --format '{{.ID}}\t{{.Names}}'")))
         (lines (split-string raw-list "\n"))
         (names (mapcar (lambda (line)
                          (let ((words (split-string line "\t")))
                            (or (nth 1 words) (nth 0 words))))
                        lines))
         (machines (seq-take-while (lambda (name) name) names)))
    (mapcar (lambda (m) (list nil m)) machines)))

;; todo: check tramp-async-args and tramp-direct-async
(defun tramp-docker--add-method ()
  "Add Tramp method handler for Docker containers."
  (push `(,tramp-docker-method
          (tramp-login-program ,tramp-docker-program)
          (tramp-login-args (("exec")
                             ("-it")
                             ("-u" "%u")
                             ("%h")
			     ("/bin/sh")))
          (tramp-remote-shell "/bin/sh")
          (tramp-remote-shell-login ("-l"))
          (tramp-remote-shell-args ("-i" "-c")))
        tramp-methods))

(defun tramp-docker--remove-method ()
  "Remove Tramp method handler for docker containers."
  (setf (alist-get tramp-docker-method tramp-methods nil t 'string=) nil))

(defun tramp-docker-unload-function ()
  "Remove Tramp method handler and completion functions."
  (tramp-set-completion-function tramp-docker-method nil)
  (tramp-docker--remove-method)
  nil)

(when nil
  (load-file (buffer-file-name))
  (setq tramp-docker-program "doas podman")
  (setq tramp-verbose 7) ;; default 3
  (tramp-docker-setup)
  (tramp-docker-unload-function))

;;;###autoload
(defun tramp-docker-setup ()
  "Initialize Docker support for Tramp."
  (tramp-docker--add-method)
  (tramp-set-completion-function tramp-docker-method
                                 '((tramp-docker--completion-function ""))))

(provide 'tramp-docker)
;;; tramp-docker.el ends here
