;;; tramp-integration.el --- Tramp integration into other packages  -*- lexical-binding:t -*-

;; Copyright (C) 2019-2025 Free Software Foundation, Inc.

;; Author: Michael Albinus <michael.albinus@gmx.de>
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

;; This assembles all integration of Tramp with other packages.

;;; Code:

(require 'tramp-compat)

;; Pacify byte-compiler.
(require 'cl-lib)
(declare-function info-lookup->cache "info-look")
(declare-function info-lookup->mode-cache "info-look")
(declare-function info-lookup->mode-value "info-look")
(declare-function info-lookup->other-modes "info-look")
(declare-function info-lookup->topic-cache "info-look")
(declare-function info-lookup->topic-value "info-look")
(declare-function info-lookup-maybe-add-help "info-look")
(declare-function recentf-cleanup "recentf")
(declare-function shortdoc-add-function "shortdoc")
(declare-function tramp-dissect-file-name "tramp")
(declare-function tramp-file-name-equal-p "tramp")
(declare-function tramp-rename-files "tramp-cmds")
(declare-function tramp-rename-these-files "tramp-cmds")
(declare-function tramp-set-connection-local-variables-for-buffer "tramp")
(declare-function tramp-tramp-file-p "tramp")
(defvar eshell-path-env)
(defvar ido-read-file-name-non-ido)
(defvar info-lookup-alist)
(defvar ivy-completing-read-handlers-alist)
(defvar recentf-exclude)
(defvar shortdoc--groups)
(defvar tramp-current-connection)
(defvar tramp-postfix-host-format)
(defvar tramp-syntax)
(defvar tramp-use-connection-share)

;;; Fontification of `read-file-name':

;; An overlay covering the shadowed part of the filename (local to the
;; minibuffer).
(defvar-local tramp-rfn-eshadow-overlay nil)

(defun tramp-rfn-eshadow-setup-minibuffer ()
  "Set up a minibuffer for `file-name-shadow-mode'.
Adds another overlay hiding filename parts according to Tramp's
special handling of `substitute-in-file-name'."
  (declare (tramp-suppress-trace t))
  (when minibuffer-completing-file-name
    (setq tramp-rfn-eshadow-overlay
	  (make-overlay (minibuffer-prompt-end) (minibuffer-prompt-end)))
    ;; Copy `rfn-eshadow-overlay' properties.
    (let ((props (overlay-properties rfn-eshadow-overlay)))
      (while props
        ;; The `field' property prevents correct minibuffer
        ;; completion; we exclude it.
        (if (not (eq (car props) 'field))
            (overlay-put tramp-rfn-eshadow-overlay (pop props) (pop props))
          (pop props) (pop props))))))

(add-hook 'rfn-eshadow-setup-minibuffer-hook
	  #'tramp-rfn-eshadow-setup-minibuffer)
(add-hook 'tramp-unload-hook
	  (lambda ()
	    (remove-hook 'rfn-eshadow-setup-minibuffer-hook
			 #'tramp-rfn-eshadow-setup-minibuffer)))

(defun tramp-rfn-eshadow-update-overlay-regexp ()
  "An overlay covering the shadowed part of the filename."
  (declare (tramp-suppress-trace t))
  (rx-to-string
   `(: (* (not (any ,tramp-postfix-host-format "/~"))) (| "/" "~"))))

(defun tramp-rfn-eshadow-update-overlay ()
  "Update `rfn-eshadow-overlay' to cover shadowed part of minibuffer input.
This is intended to be used as a minibuffer `post-command-hook' for
`file-name-shadow-mode'; the minibuffer should have already
been set up by `rfn-eshadow-setup-minibuffer'."
  (declare (tramp-suppress-trace t))
  ;; In remote files name, there is a shadowing just for the local part.
  (ignore-errors
    (let ((end (or (overlay-end rfn-eshadow-overlay)
		   (minibuffer-prompt-end)))
	  ;; We do not want to send any remote command.
	  (non-essential t))
      (when (and (tramp-tramp-file-p (buffer-substring end (point-max)))
		 (not (file-name-quoted-p (buffer-substring end (point-max)))))
	(save-excursion
	  (save-restriction
	    (narrow-to-region
	     (1+ (or (string-match-p
		      (tramp-rfn-eshadow-update-overlay-regexp)
		      (buffer-string) end)
		     end))
	     (point-max))
	    (let ((rfn-eshadow-overlay tramp-rfn-eshadow-overlay)
		  rfn-eshadow-update-overlay-hook
		  file-name-handler-alist)
	      (move-overlay rfn-eshadow-overlay (point-max) (point-max))
	      (rfn-eshadow-update-overlay))))))))

(add-hook 'rfn-eshadow-update-overlay-hook
	  #'tramp-rfn-eshadow-update-overlay)
(add-hook 'tramp-unload-hook
	  (lambda ()
	    (remove-hook 'rfn-eshadow-update-overlay-hook
			 #'tramp-rfn-eshadow-update-overlay)))

;;; Integration of eshell.el:

;; eshell.el keeps the path in `eshell-path-env'.  We must change it
;; when `default-directory' points to another host.
;; This is fixed in Eshell with Emacs 29.1.

(defun tramp-eshell-directory-change ()
  "Set `eshell-path-env' to $PATH of the host related to `default-directory'."
  ;; Remove last element of `(exec-path)', which is `exec-directory'.
  ;; Use `path-separator' as it does eshell.
  (setq eshell-path-env
        (if (tramp-tramp-file-p default-directory)
            (string-join (butlast (exec-path)) path-separator)
          (getenv "PATH"))))

(with-eval-after-load 'esh-util
  (unless (boundp 'eshell-path-env-list)
    (add-hook 'eshell-mode-hook
	      #'tramp-eshell-directory-change)
    (add-hook 'eshell-directory-change-hook
	      #'tramp-eshell-directory-change)
    (add-hook 'tramp-integration-unload-hook
	      (lambda ()
	        (remove-hook 'eshell-mode-hook
			     #'tramp-eshell-directory-change)
	        (remove-hook 'eshell-directory-change-hook
			     #'tramp-eshell-directory-change)))))

;;; Integration of recentf.el:

(defun tramp-recentf-exclude-predicate (name)
  "Predicate to exclude a remote file name from recentf.
NAME must be equal to `tramp-current-connection'."
  (when (tramp-tramp-file-p name)
    (tramp-file-name-equal-p
     (tramp-dissect-file-name name) (car tramp-current-connection))))

(defun tramp-recentf-cleanup (vec)
  "Remove all file names related to VEC from recentf."
  (when (bound-and-true-p recentf-list)
    (let ((tramp-current-connection `(,vec))
	  (recentf-exclude '(tramp-recentf-exclude-predicate)))
      (recentf-cleanup))))

(defun tramp-recentf-cleanup-all ()
  "Remove all remote file names from recentf."
  (when (bound-and-true-p recentf-list)
    (let ((recentf-exclude '(file-remote-p)))
      (recentf-cleanup))))

(with-eval-after-load 'recentf
  (add-hook 'tramp-cleanup-connection-hook
	    #'tramp-recentf-cleanup)
  (add-hook 'tramp-cleanup-all-connections-hook
	    #'tramp-recentf-cleanup-all)
  (add-hook 'tramp-integration-unload-hook
	    (lambda ()
	      (remove-hook 'tramp-cleanup-connection-hook
			   #'tramp-recentf-cleanup)
	      (remove-hook 'tramp-cleanup-all-connections-hook
			   #'tramp-recentf-cleanup-all))))

;;; Integration of ido.el:

(with-eval-after-load 'ido
  (add-to-list 'ido-read-file-name-non-ido #'tramp-rename-files)
  (add-to-list 'ido-read-file-name-non-ido #'tramp-rename-these-files)
  (add-hook 'tramp-integration-unload-hook
	    (lambda ()
	      (setq ido-read-file-name-non-ido
		    (delq #'tramp-rename-these-files ido-read-file-name-non-ido)
		    ido-read-file-name-non-ido
		    (delq #'tramp-rename-files ido-read-file-name-non-ido)))))

;;; Integration of ivy.el:

(with-eval-after-load 'ivy
  (add-to-list 'ivy-completing-read-handlers-alist
	       '(tramp-rename-files . completing-read-default))
  (add-to-list 'ivy-completing-read-handlers-alist
	       '(tramp-rename-these-files . completing-read-default))
  (add-hook
   'tramp-integration-unload-hook
   (lambda ()
     (setq ivy-completing-read-handlers-alist
	   (delete
	    (assq #'tramp-rename-these-files ivy-completing-read-handlers-alist)
	    ivy-completing-read-handlers-alist)
	   ivy-completing-read-handlers-alist
	   (delete
	    (assq #'tramp-rename-files ivy-completing-read-handlers-alist)
	    ivy-completing-read-handlers-alist)))))

;;; Integration of info-look.el:

(with-eval-after-load 'info-look
  ;; Create a pseudo mode `tramp-info-lookup-mode' for Tramp symbol lookup.
  (info-lookup-maybe-add-help
   :mode 'tramp-info-lookup-mode :topic 'symbol
   :regexp (rx (+ (not (any "\t\n \"'(),[]`‘’"))))
   :doc-spec `(("(tramp)Function Index" nil
		,(rx bol blank (+ "-") blank (* nonl) ":" blank)
		,(rx (| blank eol)))
	       ("(tramp)Variable Index" nil
		,(rx bol blank (+ "-") blank (* nonl) ":" blank)
		,(rx (| blank eol)))))

  (add-hook
   'tramp-integration-unload-hook
   (lambda ()
     (setcdr (assq 'symbol info-lookup-alist)
	     (delete (info-lookup->mode-value 'symbol 'tramp-info-lookup-mode)
		     (info-lookup->topic-value 'symbol)))
     (setcdr (info-lookup->cache 'symbol)
	     (delete (info-lookup->mode-cache 'symbol 'tramp-info-lookup-mode)
		     (info-lookup->topic-cache 'symbol)))))

  (dolist (mode (mapcar #'car (info-lookup->topic-value 'symbol)))
    ;; Add `tramp-info-lookup-mode' to `other-modes' for either
    ;; `emacs-lisp-mode' itself, or to modes which use
    ;; `emacs-lisp-mode' as `other-modes'.  Reset `info-lookup-cache'.
    (when (and (or (equal mode 'emacs-lisp-mode)
		   (memq
		    'emacs-lisp-mode (info-lookup->other-modes 'symbol mode)))
	       (not (memq 'tramp-info-lookup-mode
			  (info-lookup->other-modes 'symbol mode))))
      (setcdr (info-lookup->mode-value 'symbol mode)
	      (append (butlast (cdr (info-lookup->mode-value 'symbol mode)))
		      `((tramp-info-lookup-mode
			 . ,(info-lookup->other-modes 'symbol mode)))))
      (setcdr (info-lookup->cache 'symbol)
	      (delete (info-lookup->mode-cache 'symbol mode)
		      (info-lookup->topic-cache 'symbol)))

      (add-hook
       'tramp-integration-unload-hook
       `(lambda ()
	  (setcdr (info-lookup->mode-value 'symbol ',mode)
		  (append (butlast
			   (cdr (info-lookup->mode-value 'symbol ',mode)))
			  (list
			   (delq 'tramp-info-lookup-mode
				 (info-lookup->other-modes 'symbol ',mode)))))
	  (setcdr (info-lookup->cache 'symbol)
		  (delete (info-lookup->mode-cache 'symbol ',mode)
			  (info-lookup->topic-cache 'symbol))))))))

;;; Integration of new `:link' type in `defcustom':

(define-widget 'tramp-info-link 'link
  "A link to the Tramp info file."
  :action 'tramp-widget-info-link-action)

(defun tramp-widget-info-link-action (widget &optional _event)
  "Open the info node specified by WIDGET.
It's value must be a Tramp user option, indexed in the Tramp manual via
`@vindex'."
  (let* ((topic (widget-value widget))
	 (pattern
	  (rx "\n*" (1+ " ") (0+ nonl)
	      (literal (if (stringp topic) topic (symbol-name topic)))
	      (0+ nonl) ":" (1+ (any "\t "))
	      (group (0+ nonl))
	      "." (0+ (any "\t\n ")) "(line" (1+ " ")
	      (group (1+ digit))
	      ")")))
    (info "(tramp) Variable Index")
    (goto-char (point-min))
    (when (re-search-forward pattern nil t)
      (let ((nodename (concat "(tramp) " (match-string-no-properties 1)))
	    (line (string-to-number (match-string 2))))
	(info nodename)
	(forward-line (- line 2))))))

;;; Integration of shortdoc.el:

(tramp--with-startup
 (with-eval-after-load 'shortdoc
   ;; Some packages deactivate Tramp.  They don't deserve a shortdoc entry then.
   (when (and (file-remote-p "/ssh:user@host:/tmp/foo")
              (eq tramp-syntax 'default))
     (dolist (elem `((file-remote-p
		      :eval (file-remote-p "/ssh:user@host:/tmp/foo")
		      :eval (file-remote-p "/ssh:user@host:/tmp/foo" 'method)
		      :eval (file-remote-p "/ssh:user@[::1]#1234:/tmp/foo" 'host)
		      ;; We don't want to see the text properties.
		      :no-eval (file-remote-p "/sudo::/tmp/foo" 'user)
		      :result ,(substring-no-properties
			        (file-remote-p "/sudo::/tmp/foo" 'user)))
		     (file-local-name
		      :eval (file-local-name "/ssh:user@host:/tmp/foo"))
		     (file-local-copy
		      :no-eval (file-local-copy "/ssh:user@host:/tmp/foo")
		      :eg-result "/tmp/tramp.8ihLbO"
		      :eval (file-local-copy "/tmp/foo"))))
       (unless (assoc (car elem)
		      (member "Remote Files" (assq 'file shortdoc--groups)))
	 (shortdoc-add-function 'file "Remote Files" elem)))

     (add-hook
      'tramp-integration-unload-hook
      (lambda ()
        (let ((glist (assq 'file shortdoc--groups)))
	  (while (and (consp glist)
                      (not (and (stringp (cadr glist))
                                (string-equal (cadr glist) "Remote Files"))))
            (setq glist (cdr glist)))
	  (when (consp glist)
            (setcdr glist nil))))))))

;;; Integration of compile.el:

;; Compilation processes use `accept-process-output' such a way that
;; Tramp's parallel `accept-process-output' blocks.  See last part of
;; Bug#45518.  So we don't use ssh ControlMaster options.
(defun tramp-compile-disable-ssh-controlmaster-options ()
  "Don't allow ssh ControlMaster while compiling."
  (setq-local tramp-use-connection-share 'suppress))

(with-eval-after-load 'compile
  (add-hook 'compilation-mode-hook
	    #'tramp-compile-disable-ssh-controlmaster-options)
  (add-hook 'tramp-integration-unload-hook
	    (lambda ()
	      (remove-hook 'compilation-mode-hook
			   #'tramp-compile-disable-ssh-controlmaster-options))))

;;; Default connection-local variables for Tramp.

(defconst tramp-connection-local-default-system-variables
  '((path-separator . ":")
    (null-device . "/dev/null")
    (exec-suffixes . ("")))
  "Default connection-local system variables for remote connections.")

(connection-local-set-profile-variables
 'tramp-connection-local-default-system-profile
 tramp-connection-local-default-system-variables)

(connection-local-set-profiles
 '(:application tramp)
 'tramp-connection-local-default-system-profile)

(defconst tramp-connection-local-default-shell-variables
  '((shell-file-name . "/bin/sh")
    (shell-command-switch . "-c"))
  "Default connection-local shell variables for remote connections.")

(connection-local-set-profile-variables
 'tramp-connection-local-default-shell-profile
 tramp-connection-local-default-shell-variables)

(with-eval-after-load 'shell
  (connection-local-set-profiles
   '(:application tramp)
   'tramp-connection-local-default-shell-profile))

;; Tested with FreeBSD 12.2.
(defconst tramp-bsd-process-attributes-ps-args
  `("-acxww"
    "-o"
    ,(string-join
      '("pid"
        "euid"
        "user"
        "egid"
        "egroup"
        "comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
      ",")
    "-o"
    ,(string-join
      '("state"
        "ppid"
        "pgid"
        "sid"
        "tty"
        "tpgid"
        "minflt"
        "majflt"
        "time"
        "pri"
        "nice"
        "vsz"
        "rss"
        "etimes"
        "pcpu"
        "pmem"
        "args")
      ","))
  "List of arguments for \"ps\".
See `tramp-process-attributes-ps-args'.")

(defconst tramp-bsd-process-attributes-ps-format
  '((pid . number)
    (euid . number)
    (user . string)
    (egid . number)
    (group . string)
    (comm . 52)
    (state . string)
    (ppid . number)
    (pgrp . number)
    (sess . number)
    (ttname . string)
    (tpgid . number)
    (minflt . number)
    (majflt . number)
    (time . tramp-ps-time)
    (pri . number)
    (nice . number)
    (vsize . number)
    (rss . number)
    (etime . number)
    (pcpu . number)
    (pmem . number)
    (args . nil))
  "Alist of formats for \"ps\".
See `tramp-process-attributes-ps-format'.")

(defconst tramp-connection-local-bsd-ps-variables
  `((tramp-process-attributes-ps-args
     . ,tramp-bsd-process-attributes-ps-args)
    (tramp-process-attributes-ps-format
     . ,tramp-bsd-process-attributes-ps-format))
  "Default connection-local ps variables for remote BSD connections.")

(connection-local-set-profile-variables
 'tramp-connection-local-bsd-ps-profile
 tramp-connection-local-bsd-ps-variables)

;; Tested with BusyBox v1.24.1.
(defconst tramp-busybox-process-attributes-ps-args
  `("-o"
    ,(string-join
      '("pid"
        "user"
        "group"
        "comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
      ",")
    "-o" "stat=abcde"
    "-o"
    ,(string-join
      '("ppid"
        "pgid"
        "tty"
        "time"
        "nice"
        "etime"
        "args")
      ","))
  "List of arguments for \"ps\".
See `tramp-process-attributes-ps-args'.")

(defconst tramp-busybox-process-attributes-ps-format
  '((pid . number)
    (user . string)
    (group . string)
    (comm . 52)
    (state . 5)
    (ppid . number)
    (pgrp . number)
    (ttname . string)
    (time . tramp-ps-time)
    (nice . number)
    (etime . tramp-ps-time)
    (args . nil))
  "Alist of formats for \"ps\".
See `tramp-process-attributes-ps-format'.")

(defconst tramp-connection-local-busybox-ps-variables
  `((tramp-process-attributes-ps-args
     . ,tramp-busybox-process-attributes-ps-args)
    (tramp-process-attributes-ps-format
     . ,tramp-busybox-process-attributes-ps-format))
  "Default connection-local ps variables for remote Busybox connections.")

(connection-local-set-profile-variables
 'tramp-connection-local-busybox-ps-profile
 tramp-connection-local-busybox-ps-variables)

;; Darwin (macOS).
(defconst tramp-darwin-process-attributes-ps-args
  `("-acxww"
    "-o"
    ,(string-join
      '("pid"
        "uid"
        "user"
        "gid"
        "comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
      ",")
    "-o" "state=abcde"
    "-o"
    ,(string-join
      '("ppid"
        "pgid"
        "sess"
        "tty"
        "tpgid"
        "minflt"
        "majflt"
        "time"
        "pri"
        "nice"
        "vsz"
        "rss"
        "etime"
        "pcpu"
        "pmem"
        "args")
      ","))
  "List of arguments for \"ps\".
See `tramp-process-attributes-ps-args'.")

(defconst tramp-darwin-process-attributes-ps-format
  '((pid . number)
    (euid . number)
    (user . string)
    (egid . number)
    (comm . 52)
    (state . 5)
    (ppid . number)
    (pgrp . number)
    (sess . number)
    (ttname . string)
    (tpgid . number)
    (minflt . number)
    (majflt . number)
    (time . tramp-ps-time)
    (pri . number)
    (nice . number)
    (vsize . number)
    (rss . number)
    (etime . tramp-ps-time)
    (pcpu . number)
    (pmem . number)
    (args . nil))
  "Alist of formats for \"ps\".
See `tramp-process-attributes-ps-format'.")

(defconst tramp-connection-local-darwin-ps-variables
  `((tramp-process-attributes-ps-args
     . ,tramp-darwin-process-attributes-ps-args)
    (tramp-process-attributes-ps-format
     . ,tramp-darwin-process-attributes-ps-format))
  "Default connection-local ps variables for remote Darwin connections.")

(connection-local-set-profile-variables
 'tramp-connection-local-darwin-ps-profile
 tramp-connection-local-darwin-ps-variables)

;; Preset default "ps" profile for local hosts, based on system type.

(when-let* ((local-profile
	     (cond ((eq system-type 'darwin)
		    'tramp-connection-local-darwin-ps-profile)
		   ;; ... Add other system types here.
		   )))
  (connection-local-set-profiles
   `(:application tramp :machine ,(system-name))
   local-profile)
  (connection-local-set-profiles
   '(:application tramp :machine "localhost")
   local-profile))

;; Set connection-local variables for buffers visiting a file.

(add-hook 'find-file-hook #'tramp-set-connection-local-variables-for-buffer -50)
(add-hook 'tramp-unload-hook
	  (lambda ()
	    (remove-hook
             'find-file-hook #'tramp-set-connection-local-variables-for-buffer)))

(add-hook 'tramp-unload-hook
	  (lambda () (unload-feature 'tramp-integration 'force)))

(provide 'tramp-integration)

;;; tramp-integration.el ends here
