;;; epg-config.el --- configuration of the EasyPG Library

;; Copyright (C) 2006-2018 Free Software Foundation, Inc.

;; Author: Daiki Ueno <ueno@unixuser.org>
;; Keywords: PGP, GnuPG
;; Package: epg

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

;;; Code:

(eval-when-compile (require 'cl-lib))

(defconst epg-package-name "epg"
  "Name of this package.")

(defconst epg-version-number "1.0.0"
  "Version number of this package.")

(defconst epg-bug-report-address "ueno@unixuser.org"
  "Report bugs to this address.")

(defgroup epg ()
  "Interface to the GNU Privacy Guard (GnuPG)."
  :tag "EasyPG"
  :version "23.1"
  :group 'data
  :group 'external)

(defcustom epg-gpg-program (if (executable-find "gpg2")
                               "gpg2"
                             "gpg")
  "The `gpg' executable.
Setting this variable directly does not take effect;
instead use \\[customize] (see the info node `Easy Customization')."
  :version "25.1"
  :group 'epg
  :type 'string)

(defcustom epg-gpgsm-program "gpgsm"
  "The `gpgsm' executable.
Setting this variable directly does not take effect;
instead use \\[customize] (see the info node `Easy Customization')."
  :group 'epg
  :type 'string)

(defcustom epg-gpgconf-program "gpgconf"
  "The `gpgconf' executable."
  :version "25.1"
  :group 'epg
  :type 'string)

(defcustom epg-gpg-home-directory nil
  "The directory which contains the configuration files of `epg-gpg-program'."
  :group 'epg
  :type '(choice (const :tag "Default" nil) directory))

(defcustom epg-passphrase-coding-system nil
  "Coding system to use with messages from `epg-gpg-program'."
  :group 'epg
  :type 'symbol)

(defcustom epg-debug nil
  "If non-nil, debug output goes to the \" *epg-debug*\" buffer.
Note that the buffer name starts with a space."
  :group 'epg
  :type 'boolean)

(defconst epg-gpg-minimum-version "1.4.3")

(defconst epg-config--program-alist
  `((OpenPGP
     epg-gpg-program
     ("gpg2" . "2.1.6") ("gpg" . ,epg-gpg-minimum-version))
    (CMS
     epg-gpgsm-program
     ("gpgsm" . "2.0.4")))
  "Alist used to obtain the usable configuration of executables.
The first element of each entry is protocol symbol, which is
either `OpenPGP' or `CMS'.  The second element is a symbol where
the executable name is remembered.  The rest of the entry is an
alist mapping executable names to the minimum required version
suitable for the use with Emacs.")

(defconst epg-config--configuration-constructor-alist
  '((OpenPGP . epg-config--make-gpg-configuration)
    (CMS . epg-config--make-gpgsm-configuration))
  "Alist used to obtain the usable configuration of executables.
The first element of each entry is protocol symbol, which is
either `OpenPGP' or `CMS'.  The second element is a function
which constructs a configuration object (actually a plist).")

(defvar epg--configurations nil)

;;;###autoload
(defun epg-find-configuration (protocol &optional no-cache program-alist)
  "Find or create a usable configuration to handle PROTOCOL.
This function first looks at the existing configuration found by
the previous invocation of this function, unless NO-CACHE is non-nil.

Then it walks through PROGRAM-ALIST or
`epg-config--program-alist'.  If `epg-gpg-program' or
`epg-gpgsm-program' is already set with custom, use it.
Otherwise, it tries the programs listed in the entry until the
version requirement is met."
  (unless program-alist
    (setq program-alist epg-config--program-alist))
  (let ((entry (assq protocol program-alist)))
    (unless entry
      (error "Unknown protocol %S" protocol))
    (cl-destructuring-bind (symbol . alist)
        (cdr entry)
      (let ((constructor
             (alist-get protocol epg-config--configuration-constructor-alist)))
        (or (and (not no-cache) (alist-get protocol epg--configurations))
            ;; If the executable value is already set with M-x
            ;; customize, use it without checking.
            (if (and symbol (or (get symbol 'saved-value)
                                (get symbol 'customized-value)))
                (let ((configuration
                       (funcall constructor (symbol-value symbol))))
                  (push (cons protocol configuration) epg--configurations)
                  configuration)
              (catch 'found
                (dolist (program-version alist)
                  (let ((executable (executable-find (car program-version))))
                    (when executable
                      (let ((configuration
                             (funcall constructor executable)))
                        (when (ignore-errors
                                (epg-check-configuration configuration
                                                         (cdr program-version))
                                t)
                          (unless no-cache
                            (push (cons protocol configuration)
                                  epg--configurations))
                          (throw 'found configuration)))))))))))))

;; Create an `epg-configuration' object for `gpg', using PROGRAM.
(defun epg-config--make-gpg-configuration (program)
  (let (config groups type args)
    (with-temp-buffer
      (apply #'call-process program nil (list t nil) nil
	     (append (if epg-gpg-home-directory
			 (list "--homedir" epg-gpg-home-directory))
		     '("--with-colons" "--list-config")))
      (goto-char (point-min))
      (while (re-search-forward "^cfg:\\([^:]+\\):\\(.*\\)" nil t)
	(setq type (intern (match-string 1))
	      args (match-string 2))
	(cond
	 ((eq type 'group)
	  (if (string-match "\\`\\([^:]+\\):" args)
		  (setq groups
			(cons (cons (downcase (match-string 1 args))
				    (delete "" (split-string
						(substring args
							   (match-end 0))
						";")))
			      groups))
	    (if epg-debug
		(message "Invalid group configuration: %S" args))))
	 ((memq type '(pubkey cipher digest compress))
	  (if (string-match "\\`\\([0-9]+\\)\\(;[0-9]+\\)*" args)
	      (setq config
		    (cons (cons type
				(mapcar #'string-to-number
					(delete "" (split-string args ";"))))
			  config))
	    (if epg-debug
		(message "Invalid %S algorithm configuration: %S"
			 type args))))
	 (t
	  (setq config (cons (cons type args) config))))))
    (push (cons 'program program) config)
    (if groups
	(cons (cons 'groups groups) config)
      config)))

;; Create an `epg-configuration' object for `gpgsm', using PROGRAM.
(defun epg-config--make-gpgsm-configuration (program)
  (with-temp-buffer
    (call-process program nil (list t nil) nil "--version")
    (goto-char (point-min))
    (when (looking-at "\\S-+ (")
      (goto-char (match-end 0))
      (backward-char)
      (forward-sexp)
      (skip-syntax-forward "-" (point-at-eol))
      (list (cons 'program program)
            (cons 'version (buffer-substring (point) (point-at-eol)))))))

;;;###autoload
(defun epg-configuration ()
  "Return a list of internal configuration parameters of `epg-gpg-program'."
  (declare (obsolete epg-find-configuration "25.1"))
  (epg-config--make-gpg-configuration epg-gpg-program))

;;;###autoload
(defun epg-check-configuration (config &optional minimum-version)
  "Verify that a sufficient version of GnuPG is installed."
  (let ((version (alist-get 'version config)))
    (unless (stringp version)
      (error "Undetermined version: %S" version))
    (unless (version<= (or minimum-version
                           epg-gpg-minimum-version)
                       version)
      (error "Unsupported version: %s" version))))

;;;###autoload
(defun epg-expand-group (config group)
  "Look at CONFIG and try to expand GROUP."
  (let ((entry (assq 'groups config)))
    (if (and entry
	     (setq entry (assoc (downcase group) (cdr entry))))
	(cdr entry))))

(provide 'epg-config)

;;; epg-config.el ends here
