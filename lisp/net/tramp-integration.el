;;; tramp-integration.el --- Tramp integration into other packages  -*- lexical-binding:t -*-

;; Copyright (C) 2019-2020 Free Software Foundation, Inc.

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
(declare-function tramp-dissect-file-name "tramp")
(declare-function tramp-file-name-equal-p "tramp")
(declare-function tramp-tramp-file-p "tramp")
(defvar eshell-path-env)
(defvar ido-read-file-name-non-ido)
(defvar info-lookup-alist)
(defvar ivy-completing-read-handlers-alist)
(defvar recentf-exclude)
(defvar tramp-current-connection)
(defvar tramp-postfix-host-format)

;;; Fontification of `read-file-name':

(defvar tramp-rfn-eshadow-overlay)
(make-variable-buffer-local 'tramp-rfn-eshadow-overlay)

(defun tramp-rfn-eshadow-setup-minibuffer ()
  "Set up a minibuffer for `file-name-shadow-mode'.
Adds another overlay hiding filename parts according to Tramp's
special handling of `substitute-in-file-name'."
  (when minibuffer-completing-file-name
    (setq tramp-rfn-eshadow-overlay
	  (make-overlay (minibuffer-prompt-end) (minibuffer-prompt-end)))
    ;; Copy rfn-eshadow-overlay properties.
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
  (format "[^%s/~]*\\(/\\|~\\)" tramp-postfix-host-format))

;; Package rfn-eshadow is preloaded in Emacs, but for some reason,
;; it only did (defvar rfn-eshadow-overlay) without giving it a global
;; value, so it was only declared as dynamically-scoped within the
;; rfn-eshadow.el file.  This is now fixed in Emacs>26.1 but we still need
;; this defvar here for older releases.
(defvar rfn-eshadow-overlay)

(defun tramp-rfn-eshadow-update-overlay ()
  "Update `rfn-eshadow-overlay' to cover shadowed part of minibuffer input.
This is intended to be used as a minibuffer `post-command-hook' for
`file-name-shadow-mode'; the minibuffer should have already
been set up by `rfn-eshadow-setup-minibuffer'."
  ;; In remote files name, there is a shadowing just for the local part.
  (ignore-errors
    (let ((end (or (overlay-end rfn-eshadow-overlay)
		   (minibuffer-prompt-end)))
	  ;; We do not want to send any remote command.
	  (non-essential t))
      (when (tramp-tramp-file-p (buffer-substring end (point-max)))
	(save-excursion
	  (save-restriction
	    (narrow-to-region
	     (1+ (or (string-match-p
		      (tramp-rfn-eshadow-update-overlay-regexp)
		      (buffer-string) end)
		     end))
	     (point-max))
	    (let ((rfn-eshadow-overlay tramp-rfn-eshadow-overlay)
		  (rfn-eshadow-update-overlay-hook nil)
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
(defun tramp-eshell-directory-change ()
  "Set `eshell-path-env' to $PATH of the host related to `default-directory'."
  ;; Remove last element of `(exec-path)', which is `exec-directory'.
  ;; Use `path-separator' as it does eshell.
  (setq eshell-path-env
	(mapconcat
	 #'identity (butlast (tramp-compat-exec-path)) path-separator)))

(with-eval-after-load 'esh-util
  (add-hook 'eshell-mode-hook
	    #'tramp-eshell-directory-change)
  (add-hook 'eshell-directory-change-hook
	    #'tramp-eshell-directory-change)
  (add-hook 'tramp-integration-unload-hook
	    (lambda ()
	      (remove-hook 'eshell-mode-hook
			   #'tramp-eshell-directory-change)
	      (remove-hook 'eshell-directory-change-hook
			   #'tramp-eshell-directory-change))))

;;; Integration of recentf.el:

(defun tramp-recentf-exclude-predicate (name)
  "Predicate to exclude a remote file name from recentf.
NAME must be equal to `tramp-current-connection'."
  (when (file-remote-p name)
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
  (add-to-list 'ido-read-file-name-non-ido 'tramp-rename-files)
  (add-to-list 'ido-read-file-name-non-ido 'tramp-these-rename-files)
  (add-hook 'tramp-integration-unload-hook
	    (lambda ()
	      (setq ido-read-file-name-non-ido
		    (delq 'tramp-these-rename-files ido-read-file-name-non-ido)
		    ido-read-file-name-non-ido
		    (delq 'tramp-rename-files ido-read-file-name-non-ido)))))

;;; Integration of ivy.el:

(with-eval-after-load 'ivy
  (add-to-list 'ivy-completing-read-handlers-alist
	       '(tramp-rename-files . completing-read-default))
  (add-to-list 'ivy-completing-read-handlers-alist
	       '(tramp-these-rename-files . completing-read-default))
  (add-hook
   'tramp-integration-unload-hook
   (lambda ()
     (setq ivy-completing-read-handlers-alist
	   (delete
	    (assq 'tramp-these-rename-files ivy-completing-read-handlers-alist)
	    ivy-completing-read-handlers-alist)
	   ivy-completing-read-handlers-alist
	   (delete
	    (assq 'tramp-rename-files ivy-completing-read-handlers-alist)
	    ivy-completing-read-handlers-alist)))))

;;; Integration of info-look.el:

(with-eval-after-load 'info-look
  ;; Create a pseudo mode `tramp-info-lookup-mode' for Tramp symbol lookup.
  (info-lookup-maybe-add-help
   :mode 'tramp-info-lookup-mode :topic 'symbol
   :regexp "[^][()`'‘’,\" \t\n]+"
   :doc-spec '(("(tramp)Function Index" nil "^ -+ .*: " "\\( \\|$\\)")
	       ("(tramp)Variable Index" nil "^ -+ .*: " "\\( \\|$\\)")))

  (add-hook
   'tramp-integration-unload-hook
   (lambda ()
     (setcdr (assq 'symbol info-lookup-alist)
	     (delete (info-lookup->mode-value 'symbol 'tramp-info-lookup-mode)
		     (info-lookup->topic-value 'symbol)))
     (setcdr (info-lookup->cache 'symbol)
	     (delete (info-lookup->mode-cache 'symbol 'tramp-info-lookup-mode)
		     (info-lookup->topic-cache 'symbol)))))

  (dolist (mode (mapcar 'car (info-lookup->topic-value 'symbol)))
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

;;; Default connection-local variables for Tramp:

(defconst tramp-connection-local-default-profile
  '((shell-file-name . "/bin/sh")
    (shell-command-switch . "-c"))
  "Default connection-local variables for remote connections.")

;; `connection-local-set-profile-variables' and
;; `connection-local-set-profiles' exists since Emacs 26.1.
(with-eval-after-load 'shell
  (tramp-compat-funcall
   'connection-local-set-profile-variables
   'tramp-connection-local-default-profile
   tramp-connection-local-default-profile)
  (tramp-compat-funcall
   'connection-local-set-profiles
   `(:application tramp)
   'tramp-connection-local-default-profile))

(add-hook 'tramp-unload-hook
	  (lambda () (unload-feature 'tramp-integration 'force)))

(provide 'tramp-integration)

;;; tramp-integration.el ends here
