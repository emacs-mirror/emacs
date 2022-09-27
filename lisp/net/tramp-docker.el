;;; tramp-docker.el --- Tramp integration for Docker containers  -*- lexical-binding: t; -*-

;; Copyright © 2022 Free Software Foundation, Inc.

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

;; ‘tramp-docker’ allows Tramp access to environments provided by
;; Docker.
;;
;; ## Usage
;;
;; Open a file on a running systemd-docker container:
;;
;;     C-x C-f /docker:USER@CONTAINER:/path/to/file
;;
;; Where:
;;     USER          is the user on the container to connect as (optional)
;;     CONTAINER     is the container to connect to

;;; Code:

(require 'tramp)

;;;###tramp-autoload
(defcustom tramp-docker-program "docker"
  "Name of the Docker client program."
  :group 'tramp
  :version "29.1"
  :type '(choice (const "docker")
                 (const "podman")
                 (string)))

;;;###tramp-autoload
(defconst tramp-docker-method "docker"
  "Tramp method name to use to connect to Docker containers.")

;;;###tramp-autoload
(defun tramp-docker--completion-function (&rest _args)
  "List Docker containers available for connection.

This function is used by `tramp-set-completion-function', please
see its function help for a description of the format."
  (when-let ((raw-list (shell-command-to-string
			(concat tramp-docker-program
				" ps --format '{{.ID}}\t{{.Names}}'")))
             (lines (split-string raw-list "\n" 'omit))
             (names (mapcar
		     (lambda (line)
                       (when (string-match
			      (rx bol (group (1+ nonl))
				  "\t" (? (group (1+ nonl))) eol)
			      line)
			 (or (match-string 2 line) (match-string 1 line))))
                     lines)))
    (mapcar (lambda (m) (list nil m)) (delq nil names))))

;;;###tramp-autoload
(tramp--with-startup
 (push `(,tramp-docker-method
         (tramp-login-program ,tramp-docker-program)
         (tramp-login-args (("exec")
                            ("-it")
                            ("-u" "%u")
                            ("%h")
			    ("%l")))
         (tramp-remote-shell ,tramp-default-remote-shell)
         (tramp-remote-shell-login ("-l"))
         (tramp-remote-shell-args ("-i" "-c")))
       tramp-methods)

 (tramp-set-completion-function
  tramp-docker-method
  '((tramp-docker--completion-function ""))))

(add-hook 'tramp-unload-hook
	  (lambda ()
	    (unload-feature 'tramp-docker 'force)))

(provide 'tramp-docker)

;;; tramp-docker.el ends here
