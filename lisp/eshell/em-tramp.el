;;; em-tramp.el --- Eshell features that require Tramp  -*- lexical-binding:t -*-

;; Copyright (C) 1999-2024 Free Software Foundation, Inc.

;; Author: Aidan Gauland <aidalgol@no8wireless.co.nz>

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

;; Eshell features that require Tramp.

;;; Code:

(require 'esh-util)
(require 'esh-cmd)

(eval-when-compile
  (require 'esh-mode)
  (require 'eshell))

(require 'tramp)

;; There are no items in this custom group, but eshell modules (ab)use
;; custom groups.
;;;###autoload
(progn
 (defgroup eshell-tramp nil
   "This module defines commands that use Tramp in a way that is
  not transparent to the user.  So far, this includes only the
  built-in su, sudo and doas commands, which are not compatible
  with the full, external su, sudo, and doas commands, and
  require the user to understand how to use the Tramp sudo
  method."
   :tag "Tramp Eshell features"
   :group 'eshell-module))

(defun eshell-tramp-initialize ()   ;Called from `eshell-mode' via intern-soft!
  "Initialize the Tramp-using commands code."
  (when (eshell-using-module 'eshell-cmpl)
    (add-hook 'pcomplete-try-first-hook
	      'eshell-complete-host-reference nil t))
  (setq-local eshell-complex-commands
              (append '("su" "sudo" "doas")
                      eshell-complex-commands)))

(autoload 'eshell-parse-command "esh-cmd")

(defun eshell/su (&rest args)
  "Alias \"su\" to call Tramp.

Uses the system su through Tramp's su method."
  (eshell-eval-using-options
   "su" args
   '((?h "help" nil nil "show this usage screen")
     (?l "login" nil login "provide a login environment")
     (?  nil nil login "provide a login environment")
     :usage "[- | -l | --login] [USER]
Become another USER during a login session.")
   (throw 'eshell-replace-command
          (let ((user "root")
                (host (or (file-remote-p default-directory 'host)
                          tramp-default-host))
                (dir (file-local-name (expand-file-name default-directory)))
                (prefix (file-remote-p default-directory)))
            (dolist (arg args)
              (if (string-equal arg "-") (setq login t) (setq user arg)))
            (when login (setq dir "~/"))
            (if (and prefix
                     (or
                      (not (string-equal
                            "su" (file-remote-p default-directory 'method)))
                      (not (string-equal
                            user (file-remote-p default-directory 'user)))))
                (eshell-parse-command
                 "cd" (list (format "%s|su:%s@%s:%s"
                                    (substring prefix 0 -1) user host dir)))
              (eshell-parse-command
               "cd" (list (format "/su:%s@%s:%s" user host dir))))))))

(put 'eshell/su 'eshell-no-numeric-conversions t)

(defun eshell--method-wrap-directory (directory method &optional user)
  "Return DIRECTORY as accessed by a Tramp METHOD for USER."
  (let ((user (or user "root"))
        (dir (file-local-name (expand-file-name directory)))
        (prefix (file-remote-p directory))
        (host (or (file-remote-p directory 'host)
                 tramp-default-host))
        (rmethod (file-remote-p directory 'method))
        (ruser (file-remote-p directory 'user)))
    (if (and prefix (or (not (string-equal rmethod method))
                     (not (string-equal ruser user))))
        (format "%s|%s:%s@%s:%s"
                (substring prefix 0 -1) method user host dir)
      (format "/%s:%s@%s:%s" method user host dir))))

(defun eshell/sudo (&rest args)
  "Alias \"sudo\" to call Tramp.

Uses the system sudo through Tramp's sudo method."
  (eshell-eval-using-options
   "sudo" args
   '((?h "help" nil nil "show this usage screen")
     (?u "user" t user "execute a command as another USER")
     (?s "shell" nil shell "start a shell instead of executing COMMAND")
     :show-usage
     :parse-leading-options-only
     :usage "[(-u | --user) USER] (-s | --shell) | COMMAND
Execute a COMMAND as the superuser or another USER.")
   (let ((dir (eshell--method-wrap-directory default-directory "sudo" user)))
     (if shell
         (throw 'eshell-replace-command
                (eshell-parse-command "cd" (list dir)))
       (throw 'eshell-external
              (let ((default-directory dir))
                (eshell-named-command (car args) (cdr args))))))))

(put 'eshell/sudo 'eshell-no-numeric-conversions t)

(defun eshell/doas (&rest args)
  "Call Tramp's doas method with ARGS.

Uses the system doas through Tramp's doas method."
  (eshell-eval-using-options
   "doas" args
   '((?h "help" nil nil "show this usage screen")
     (?u "user" t user "execute a command as another USER")
     (?s "shell" nil shell "start a shell instead of executing COMMAND")
     :show-usage
     :parse-leading-options-only
     :usage "[(-u | --user) USER] (-s | --shell) | COMMAND
Execute a COMMAND as the superuser or another USER.")
   (let ((dir (eshell--method-wrap-directory default-directory "doas" user)))
     (if shell
         (throw 'eshell-replace-command
                (eshell-parse-command "cd" (list dir)))
       (throw 'eshell-external
              (let ((default-directory dir))
                (eshell-named-command (car args) (cdr args))))))))

(put 'eshell/doas 'eshell-no-numeric-conversions t)

(provide 'em-tramp)

;; Local Variables:
;; generated-autoload-file: "esh-groups.el"
;; End:

;;; em-tramp.el ends here
