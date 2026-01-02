;;; Submit code to a connected Android device  -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
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
;;
;; This file establishes connections to devices attached over `adb' and
;; arranges to execute the test driver and submit code to the same.

;;; Code:

(require 'tramp) ;; Only for a number of regexps.



;; Device management.

(defvar ats-adb-executable nil
  "Name of the `adb' executable on this system, or nil if uninitialized.")

(defvar ats-adb-host nil
  "Hostname and port on which the ADB server resides.
If nil, this value defaults to localhost and an ADB server will
automatically be started if none is currently executing.")

(defvar ats-adb-infile nil
  "File providing the stdin of `adb' subprocesses.")

(defvar ats-cache nil
  "Cache recording facts predicated of a device and its contents.")

(defvar ats-adb-disable-stderr t
  "Whether not to print error output from subprocesses invoked by `ats-adb'.")

(defconst ats-adb-device-regexp
  "\\([^[:space:]]+\\)[[:space:]]+\\([[:alnum:]]+\\)$"
  "Regexp with which to extract devices from `adb devices' output.")

(defun ats-adb (&rest commands)
  "Execute `adb COMMANDS' and insert its output into the current buffer.
Command output is inserted before point."
  (unless ats-adb-executable
    (setq ats-adb-executable
	  (or (executable-find "adb")
	      (progn
		(message "Could not locate a suitable `adb' binary.
Please arrange that a version of the Android debugging bridge be present
in `exec-path' and be permitted to access connected USB devices.
For more information, visit https://developer.android.com/tools/adb.")
		(error "Could not locate a suitable `adb' binary")))))
  (let ((point (point)) (coding-system-for-read 'utf-8-unix))
    (save-excursion
      (when ats-adb-host
	(setq commands (append (list "-H" ats-adb-host) commands)))
      (let ((rc (apply #'call-process ats-adb-executable
		       ats-adb-infile
		       (or (and ats-adb-disable-stderr '(t nil)) t)
		       nil commands)))
	(when (not (zerop rc))
	  (error "%s exited with %s"
		 (mapconcat #'shell-quote-argument
			    (cons ats-adb-executable commands)
			    " ")
		 rc))
	;; Undo misguided EOL format conversion performed by the ADB
	;; daemon on older releases of Android.
	(let ((end (point)))
	  (goto-char point)
	  (while (re-search-forward "\r+$" end t)
	    (replace-match "")))))))

(defun ats-adb-process-filter (proc string)
  "Insert STRING and update PROC's mark as the default filter does.
Remove all CR characters preceding newlines in STRING."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((new-string (replace-regexp-in-string "\r$" "" string)))
	(save-excursion
	  (goto-char (process-mark proc))
	  (insert new-string)
	  (set-marker (process-mark proc) (point)))))))

(defun ats-start-adb (&rest commands)
  "Execute `adb COMMANDS' in an asynchronous subprocess.
Apply a process filter to delete errant carriage return
characters."
  (unless ats-adb-executable
    (setq ats-adb-executable
	  (or (executable-find "adb")
	      (progn
		(message "Could not locate a suitable `adb' binary.
Please arrange that a version of the Android debugging bridge be present
in `exec-path' and be permitted to access connected USB devices.
For more information, visit https://developer.android.com/tools/adb.")
		(error "Could not locate a suitable `adb' binary")))))
  (let ((coding-system-for-read 'utf-8-unix))
    (save-excursion
      (when ats-adb-host
	(setq commands (append (list "-H" ats-adb-host) commands)))
      (let ((process (apply #'start-process " *ats adb*"
			    " *ats adb*"
			    ats-adb-executable commands)))
        (prog1 process
	  (set-process-filter process #'ats-adb-process-filter))))))

(defun ats-enumerate-devices (&optional pred arg)
  "Return a list of connected devices as an alist indiced by serial number.
Value is an alist of device serial numbers that may be provided
as the `-s' argument to `adb' and the state of the device, which
is a string that is either \"device\" for a fully available
device, or another value if the connection to the device is
defective.

If PRED is specified, invoke it on each device with ARG and its
serial number and state, and only return devices for which it
returns non-nil."
  (with-temp-buffer
    (ats-adb "devices")
    (re-search-forward "List of devices attached\n" nil t)
    (let ((devices nil))
      (while (re-search-forward ats-adb-device-regexp
				nil t nil)
	(let ((name (match-string 1))
	      (state (match-string 2)))
	  (when (or (not pred) (funcall pred name state arg))
	    (push (cons name state) devices))))
      (nreverse devices))))

(defun ats-online-devices ()
  "Like `ats-enumerate-devices', but only return devices which are available."
  (ats-enumerate-devices (lambda (_ state _)
			   (equal state "device"))))



;; Device introspection.

(defmacro ats-memoize (device key &rest bodyforms)
  "Return the result of executing BODYFORMS with memoization.
Cache such result and avoid executing BODYFORMS more than once
with the same DEVICE and KEY."
  (declare (indent 2))
  (let ((device-key (gensym))
	(cache (gensym))
	(value (gensym)))
    `(let* ((,device-key (concat (or ats-adb-host "localhost")
				 "/" ,device))
	    (,cache (or (cdr-safe (assoc ,device-key ats-cache))
			(setf (alist-get ,device-key ats-cache
					 :testfn #'equal)
			      (make-hash-table :test #'equal))))
	    (,value (gethash ,key ,cache)))
       (if ,value (car ,value)
	 (setq ,value (progn ,@bodyforms))
	 (prog1 ,value
	   (puthash ,key (list ,value) ,cache))))))

(defun ats-ps-device (device &optional predicate arg)
  "Return a list of running processes on DEVICE.
Return a list each of whose elements is an alist between the names
of columns returned by `ps' and their values.
If PREDICATE is non-nil, accept only those processes for which
it returns true, with ARG provided as a second argument."
  (with-temp-buffer
    (ats-adb "-s" device "shell" "ps")
    ;; Examples:
    ;; USER	PID   PPID  VSIZE  RSS	   WCHAN    PC	       NAME
    ;; USER	      PID  PPID	    VSZ	   RSS WCHAN		ADDR S NAME
    (end-of-line)
    (let* ((substr (buffer-substring (point-min) (point)))
	   (legend (mapcar #'intern
			   (string-split substr "[ \t]"
					 t "[[:space:]]")))
	   (state-present (memq 'S legend))
	   (last (car (last legend)))
	   (processes nil)
	   process)
      (while (re-search-forward "[[:alnum:]]" nil t)
	(backward-char)
	(setq process nil)
	(dolist (column legend)
	  (let ((beg (point)))
	    (re-search-forward (if (eq column last)
				   "[[:space:]]*$"
				 "\\([[:space:]]+\\|$\\)"))
	    ;; The `S' column is on certain older systems not listed in
	    ;; the legend but printed anyway before NAME.
	    (when (and (not state-present) (eq column 'NAME))
	      (save-excursion
		(goto-char beg)
		(save-match-data
		  (when (re-search-forward "\\([RSDZTtWXxKPI]\\) " nil t)
		    (setq beg (point))
		    (push (cons 'S (match-string 1)) process)))))
	    (push (cons column (buffer-substring beg (match-beginning 0)))
		  process)))
	(when (or (not predicate) (funcall predicate process arg))
	  (push (nreverse process) processes)))
      (nreverse processes))))

(defun ats-getprop (device prop)
  "Return the value of the system property PROP on DEVICE.
Among such properties are:

  - `ro.build.version.sdk': The version of Android present on
    the device."
  (ats-memoize device (concat "ats-getprop/" prop)
    (with-temp-buffer
      (ats-adb "-s" device "shell" "getprop" prop)
      (goto-char (point-max))
      (when (eq (char-before) ?\n)
	(delete-char -1))
      (buffer-string))))

(defun ats-get-sdk-version (device)
  "Return the version of Android installed on DEVICE."
  (or (string-to-number (ats-getprop device "ro.build.version.sdk")) 0))

(defconst ats-package-list-regexp
  "^\\([[:alnum:]\\.]+\\) \\([[:digit:]]+\\) \\([[:digit:]]\\).*/.*$"
  "Regexp with which to validate the format of packages.list.")

(defun ats-is-package-debuggable (device pkg)
  "Return whether the package identified by PKG is debuggable on DEVICE."
  (ats-memoize device (concat "ats-is-package-debuggable/" pkg)
    (with-temp-buffer
      (if (ignore-errors
	    (ats-adb "-s" device "shell" "cat" "/data/system/packages.list")
	    (re-search-forward ats-package-list-regexp nil nil))
	  ;; packages.list is readable.	 Search for an entry matching
	  ;; PKG.
	  (progn
	    (goto-char (point-min))
	    (unless (re-search-forward (rx bol
					   (literal pkg)
					   " "
					   ;; UID of package.
					   (group (+ (or alnum ".")))
					   " "
					   ;; Package debuggability.
					   (group (or "0" "1"))
					   ;; Package home directory.
					   (+ nonl)
					   "/"
					   (+ nonl)
					   eol)
				       nil t)
	      (error "No package on device: %s" pkg))
	    (equal (match-string 2) "1"))
	;; If packages.list is unreadable (as when adbd is not executing
	;; as root on recent OS releases), call run-as to establish
	;; whether this package is debuggable.
	(ignore-errors
	  (ats-adb "-s" device "shell" "run-as" pkg "echo" "emacs_token"))
	(when (re-search-forward "run-as:" nil t)
	  ;; Was an error message printed?  Does it indicate that the
	  ;; package is not present?
	  (when (re-search-forward "unknown" nil t)
	    (error "No package on device: %s" pkg))
	  nil)
	(goto-char (point-min))
	(re-search-forward "emacs_token" nil t)))))

(defun ats-list-users (device)
  "Return a list of user IDs present on DEVICE.
Each element of the list produced is a list of the form:

  (ID NAME EXTERNAL-STORAGE-DIRECTORY)"
  (if (< (ats-get-sdk-version device) 17)
      '((0 "Android user" "/sdcard"))
    (ats-memoize device "ats-list-users"
      (let ((users nil))
	(with-temp-buffer
	  (ats-adb "-s" device "shell" "pm" "list" "users")
	  (while (re-search-forward
		  "^\tUserInfo{\\([[:digit:]]+\\):\\(.*?\\):.*$" nil t)
	    (push (list (string-to-number (match-string 1))
			(match-string 2)
			(if (equal (match-string 1) "0")
			    (or (ignore-errors
				  (ats-verify-directory
				   device "/storage/emulated/0"))
				"/sdcard")
			  (or (ignore-errors
				(ats-verify-directory
				 device
				 (format "/mnt/shell/emulated/%s" (match-string 1))))
			      (format "/storage/emulated/%s" (match-string 1)))))
		  users)))
	(sort users :lessp (lambda (a b)
			     (< (car a) (car b)))
	      :in-place t)))))

(defun ats-get-package-aid (device package)
  "Return the base AID of the provided PACKAGE on DEVICE.
This value may be treated as-is as the UID of PACKAGE running as
the default Android user, or provided to `ats-get-package-uid'
to derive the UID assigned to instances of it that are executing
as another user."
  (ats-memoize device (concat "ats-get-package-aid/" package)
    (with-temp-buffer
      (ats-adb "-s" device "shell" "dumpsys" "package" package)
      (re-search-forward (rx bol (+ space)
			     "Package [" (literal package) "]"
			     (+ nonl) ":" eol))
      (re-search-forward "\\(userId\\|appId\\)=\\([[:digit:]]+\\)")
      (string-to-number (match-string 2)))))

;; Ref:
;; https://android.googlesource.com/platform/system/core/+/master/libcutils/include/private/android_filesystem_config.h
;; https://android.googlesource.com/platform/system/core/+/master/libcutils/multiuser.cpp

(defconst ats-aid-user-offset 100000
  "Value of `AID_USER_OFFSET' in `android_filesystem_config.h'.")
(defconst ats-aid-isolated-start 90000
  "Value of `AID_ISOLATED_START' in `android_filesystem_config.h'.")
(defconst ats-aid-app-start 10000
  "Value of `AID_APP_START' in `android_filesystem_config.h'.")

(defun ats-aid-to-uid (aid user)
  "Derive a UID from an application ID and a user ID.
Return the UID that will be assigned to instances of that
application which is identified by AID when executing as the
Android user USER.  AID should be a value returned by
`ats-get-package-uid', which see."
  (+ (% aid ats-aid-user-offset) (* user ats-aid-user-offset)))

;; Ref:
;; https://android.googlesource.com/platform/bionic/+/master/libc/bionic/grp_pwd.cpp

(defun ats-uid-to-username (device uid)
  "Return the name of an application user UID on DEVICE.
Signal if UID is not a valid application user ID."
  (let ((appid (% uid ats-aid-user-offset))
	(userid (/ uid ats-aid-user-offset)))
    (if (>= (ats-get-sdk-version device) 16)
	;; "New style" IDs with isolated environments.
	(cond
	 ((>= appid ats-aid-isolated-start)
	  (format "u%d_i%d" userid (- appid ats-aid-isolated-start)))
	 ((>= appid ats-aid-app-start)
	  (format "u%d_a%d" userid (- appid ats-aid-app-start)))
	 (t
	  (error "UID is not representable: %d" uid)))
      (cond
       ;; Old style IDs.
       ((>= appid ats-aid-app-start)
	(format "app_%d" (- appid ats-aid-app-start)))
       (t
	(error "UID is not representable: %d" uid))))))

(defun ats-verify-directory (device dir)
  "Verify whether DIR exists on DEVICE, and signal if not.
Value is DIR otherwise."
  (with-temp-buffer
    (ignore-errors
      (ats-adb "-s" device "shell" "test" "-d" dir "&&" "echo" "ATS_OK"))
    ;; There are Android systems where `test' is neither installed to
    ;; /system/bin nor available as a shell builtin.  On these systems,
    ;; this command prints an error message and exits.
    (prog1 dir
      (if (looking-at ".*\\btest\\b.*$")
	  ;; Call `mkdir' and test whether it reports that the directory
	  ;; already exists.
	  (progn
	    (erase-buffer)
	    (ignore-errors
	      (ats-adb "-s" device "shell" "mkdir" dir "||" "echo" "ATS_EXISTS"))
	    (goto-char (point-max))
	    (forward-line -1)
	    (unless (and (looking-at "ATS_EXISTS$")
			 (progn
			   (goto-char (point-min))
			   ;; Skip any instance of `dir' in the error
			   ;; message.
			   (search-forward dir nil t)
			   (looking-at ".*File exists.*")))
	      (error "Directory `%s' does not appear to exist" dir)))
	(goto-char (point-max))
	(forward-line -1)
	(unless (looking-at "ATS_OK$")
	  (error "Directory `%s' does not exist" dir))))))

(defun ats-get-package-data-directory (device package user)
  "Return PACKAGE's data directory on DEVICE.
Return PACKAGE's data directory when executing as that user
which is identified by the user ID USER."
  (ats-memoize device (concat "ats-get-package-data-directory/"
			      package "/" (number-to-string user))
    (with-temp-buffer
      (ats-adb "-s" device "shell" "dumpsys" "package" package)
      (re-search-forward (rx bol (+ space)
			     "Package [" (literal package) "]"
			     (+ nonl) ":" eol))
      (if (eq user 0)
	  (progn
	    (or (save-excursion
		  ;; Attempt to parse a dataDir= specification under a
		  ;; User: heading.  This line may be absent or not fall
		  ;; under this heading on older Android systems.
		  (when (and (re-search-forward "^[[:space:]]+User 0: " nil t)
			     (re-search-forward "dataDir=\\(/.*$\\)" nil t))
		    (match-string 1)))
		;; Resort to any dataDir= specification, as this is user
		;; 0.
		(and (re-search-forward "dataDir=\\(/.*$\\)" nil t)
		     (match-string 1))
		;; Signal failure.
		(error "Could not extract data directory of package `%s'" package)))
	;; Attempt to extract a dataDir= specification printed under a
	;; User heading.
	(or (save-excursion
	      (when (and (re-search-forward (format "^[[:space:]]+User %d: "
						    user)
					    nil t)
			 (re-search-forward "dataDir=\\(/.*$\\)" nil t))
		(match-string 1)))
	    ;; If this fails (as on Android systems where "dumpsys
	    ;; package" has not yet been revised to print user-specific
	    ;; data directories), return "/data/user/%d/%s", but verify
	    ;; that it exists.
	    (ats-verify-directory device (format "/data/user/%d/%s"
						 user package)))))))

(defun ats-get-user-external-storage-directory (device user)
  "Return the external storage directory visible to USER on DEVICE."
  (caddr (assq user (ats-list-users device))))

(defvar ats-transfer-padding (make-string 300 ?\n)
  "Padding delivered before attempting to transfer shell scripts.")

(defun ats-exec-script (device script &optional package user)
  "Execute SCRIPT on DEVICE and return its exit code.
Insert its output into the current buffer in the manner of
`ats-adb'.  If PACKAGE and USER are specified, run this script
as PACKAGE, provided that it is debuggable."
  (save-restriction
    (narrow-to-region (point) (point))
    (let* ((name (format "%s.sh" (make-temp-name "ats-")))
	   (fullname (concat (file-name-as-directory
			      temporary-file-directory)
			     name)))
      (with-temp-buffer
	(insert script)
	(write-region (point-min) (point-max) fullname))
      (unwind-protect
	  (let ((targetname (format "/data/local/tmp/%s" name)))
	    (with-temp-buffer
	      (ats-adb "-s" device "push" fullname targetname))
	    (if (not package)
		(progn
		  (ats-adb "-s" device "shell" "sh" "-c"
			   (shell-quote-argument
			    (let ((arg (shell-quote-argument targetname t)))
			      (format
			       "sh %s; echo ats_exit: $?; (rm %s &> /dev/null)"
			       arg arg))
			    t)))
	      ;; targetname names a script that will reconstruct SCRIPT
	      ;; in the `files' subdirectory of the current working
	      ;; directory.
	      ;;
	      ;; It is not possible reliably to transfer data through
	      ;; `adb shell', as the device may allocate a
	      ;; pseudoterminal, which imposes restrictions on both line
	      ;; length and transfer size, and to compensate, this
	      ;; script is first transferred to /data/local/tmp, and
	      ;; piped into run-as on-device in a single concise
	      ;; command.
	      (unless (ats-is-package-debuggable device package)
		(error "Package is not debuggable: `%s'" package))
	      (let* ((pkgname (format "files/%s" name))
		     (src (shell-quote-argument targetname t))
		     (arg (shell-quote-argument pkgname t))
		     (version (ats-get-sdk-version device)))
		(if (eq user 0)
		    (progn
		      (ats-adb
		       "-s" device "shell" "sh"
		       "-c" (shell-quote-argument
			     (format "run-as %s sh -c %s < %s"
				     package
				     (shell-quote-argument
				      (format "cat > %s" arg) t)
				     src)
			     t))
		      (ats-adb
		       "-s" device "shell" "run-as" package "sh"
		       "-c"
		       (shell-quote-argument
			(format
			 "sh %s; echo ats_exit: $?; (rm %s &> /dev/null)"
			 arg arg)
			t)))
		  (if (< version 23)
		      (error (concat "Cannot execute script as package and"
				     "non-default user on Android <= 5.1."))
		    (progn
		      (ats-adb
		       "-s" device "shell" "sh"
		       "-c" (shell-quote-argument
			     (format "run-as %s --user %d sh -c %s < %s"
				     package
				     user
				     (shell-quote-argument
				      (format "cat > %s" arg) t)
				     src)
			     t))
		      (ats-adb
		       "-s" device "shell" "run-as" package
		       "--user" (number-to-string user)
		       "sh" "-c"
		       (shell-quote-argument
			(format
			 "sh %s; echo ats_exit: $?; (rm %s &> /dev/null)"
			 arg arg)
			t))))))))
	(with-demoted-errors "Deleting temporary script: %S"
	  (delete-file fullname))))
    (goto-char (point-max))
    (re-search-backward "ats_exit: \\([[:digit:]]+\\)$")
    (prog1 (string-to-number (match-string 1))
      (delete-region (point) (point-max)))))

(defsubst ats-exec-script-checked (device script &optional package user)
  "Execute SCRIPT on DEVICE as PACKAGE and USER, as with `ats-exec-script'.
But signal an error if its exit code is non-zero."
  (let ((rc (ats-exec-script device script package user)))
    (when (/= rc 0)
      (error "Script exited with return code %d:\n%s" rc script))))

(defun ats-use-private-staging-directory (device package user)
  "Return whether PACKAGE running as USER admits of a private staging directory.
DEVICE is the device to which the test pertains.

A private staging directory is a staging directory within
PACKAGE's application data directory, enabling packages to be
debugged without holding external storage permissions."
  (and (or (eq user 0)
	   ;; `run-as --user' requires Android 6.0 or better.
	   (>= (ats-get-sdk-version device) 23))
       (ats-is-package-debuggable device package)))

(defun ats-get-staging-directory (device package user)
  "Create and return a staging directory for communication with PACKAGE.
Create and return a directory which is accessible both to this
instance of Emacs and to PACKAGE executing on DEVICE as USER."
  ;; Prefer invoking `run-as' to transfer files into a local directory.
  (ats-memoize device (concat "ats-get-staging-directory/"
			      package "/" (number-to-string user))
    (if (ats-use-private-staging-directory device package user)
	(progn
	  (with-temp-buffer
	    ;; The return value of mkdir is not tested and neither is
	    ;; any attempt made to supply such flags as `-p', as no
	    ;; flags to `mkdir' can be relied upon on Android.
	    (ats-exec-script device "mkdir files/ats-staging"
			     package user)
	    (erase-buffer)
	    (ats-exec-script-checked device "cd files/ats-staging && pwd"
				     package user)
	    (when (eq (char-before) ?\n)
	      (delete-char -1))
	    (buffer-string)))
      ;; Locate the external storage directory visible to USER.
      (let* ((external-storage (ats-get-user-external-storage-directory
				device user))
	     (subdirectory (format "%s/ats-staging" external-storage)))
	(with-temp-buffer
	  (ats-exec-script device (format "mkdir %s" subdirectory))
	  (erase-buffer)
	  (ats-exec-script-checked device
				   (format "cd %s && pwd"
					   (shell-quote-argument
					    subdirectory t)))
	  (when (eq (char-before) ?\n)
	    (delete-char -1))
	  (buffer-string))))))

(defun ats-base64-available (device)
  "Return whether a `base64' binary is available on DEVICE."
  (ats-memoize device "ats-base64-available"
    (with-temp-buffer
      (ats-exec-script
       device
       (format "export TMPDIR=/data/local/tmp\n
base64 -d <<'_ATS_BASE64_EOF'\n%s\n_ATS_BASE64_EOF"
	       (base64-encode-string "Emacs_Hello")))
      (equal (buffer-string) "Emacs_Hello"))))

(defun ats-echo-n-e (device)
  "Return whether `echo -n -e' is understood by DEVICE."
  (ats-memoize device "ats-proper-echo-flags"
    (with-temp-buffer
      ;; The Almquist shell distributed with old Android releases treats
      ;; flags subsequent to the first as additional strings to be
      ;; printed.
      (ats-exec-script device "echo -n -e '\\077'")
      (equal (buffer-string) "?"))))

(defun ats-echo-c (device)
  "Return whether \"echo -e '...\\c'\" is understood by DEVICE."
  (ats-memoize device "ats-almquist-echo-flags"
    (with-temp-buffer
      (ats-exec-script device "echo -e '\\077\\c'")
      (equal (buffer-string) "?"))))

(defvar ats-octab (make-vector 256 0)
  "Vector of numbers between 0 and 255 and their octal representations.")
(dotimes (c 256)
  (aset ats-octab c (format "\\0%o" c)))

(defun ats-upload-encode-binary (device file quoted)
  "Generate an script that will echo the contents of FILE into QUOTED.
QUOTED must have been processed by `shell-quote-argument'.
The script will be suitable for execution on DEVICE."
  ;; We would prefer to use uuencode rather than echo, but it appears
  ;; even scarcer than base64.
  (cond ((ats-base64-available device)
	 (with-temp-buffer
	   (set-buffer-multibyte nil)
	   (let ((coding-system-for-read 'no-conversion))
	     (insert-file-contents file))
	   (let ((encoded (base64-encode-string (buffer-string) nil)))
	     (erase-buffer)
	     (insert encoded)
	     (goto-char (point-min))
	     (insert "export TMPDIR=`pwd`\n"
		     "base64 -d <<_ATS_UPLOAD_EOF >"
		     quoted "\n")
	     (goto-char (point-max))
	     (insert "\n_ATS_UPLOAD_EOF\n"))
	   (buffer-string)))
	((or (ats-echo-n-e device)
	     (ats-echo-c device))
	 (let* ((is-echo-c (not (ats-echo-n-e device)))
		(echo-prefix (if is-echo-c "echo -e '" "echo -n -e '"))
		(echo-suffix (if is-echo-c "\\c'\n" "'\n"))
		(ats-upload-script
		 (shell-quote-argument
		  (concat (make-temp-name "ats-upload-") ".sh") t)))
	   (with-temp-buffer
	     (set-buffer-multibyte nil)
	     (let ((coding-system-for-read 'no-conversion))
	       (insert-file-contents file))
	     (with-output-to-string
	       (princ "export TMPDIR=`pwd`; cat <<_ATS_UPLOAD_EOF >")
	       (princ ats-upload-script)
	       (terpri)
	       (let ((point (point))
		     (point-max (point-max)))
		 (while (< point point-max)
		   (princ echo-prefix)
		   (let ((i (min 128 (- point-max point))))
		     (dotimes (idx i)
		       (princ (aref ats-octab (char-after (+ point idx)))))
		     (setq point (goto-char (+ point i))))
		   (princ echo-suffix)))
	       (princ "_ATS_UPLOAD_EOF\nsh ")
	       (princ ats-upload-script)
	       (princ (concat " > " quoted " && rm " ats-upload-script))))))
	(t (error "Cannot decide by what means to encode a binary file"))))

(defun ats-upload (device file package user)
  "Upload FILE to PACKAGE's staging directory on DEVICE.
Value is the file name on the device.  USER is the numerical ID
of the Android user as which PACKAGE will execute."
  (setq file (expand-file-name file))
  (let ((staging-dir (ats-get-staging-directory device package user)))
    (if (ats-use-private-staging-directory device package user)
	;; Upload by way of `run-as'.
	(let ((dst-file (concat staging-dir "/"
				(file-name-nondirectory file))))
	  (with-temp-buffer
	    (ats-exec-script-checked
	     device
	     (let ((quoted (shell-quote-argument dst-file t)))
	       (ats-upload-encode-binary device file quoted))
	     package user))
	  dst-file)
      (let ((dest-file-name
	     (concat staging-dir "/" (file-name-nondirectory file))))
	(with-temp-buffer
	  (ats-adb "-s" device "push" file dest-file-name))
	dest-file-name))))

(defun ats-download (device file package user)
  "Download FILE from PACKAGE's staging directory on DEVICE.
FILE's contents should be UTF-8 text with Unix line endings.
Insert its contents at point in the current buffer.  PACKAGE and
USER are as in `ats-upload'."
  (let* ((dir-private-p
	  (ats-use-private-staging-directory device package user))
	 (exec-package (and dir-private-p package))
	 (exec-user (and dir-private-p user)))
    (insert (with-temp-buffer
	      ;; It is not reliable to cat binary data through adb, nor
	      ;; possible to copy binary data as a package user to a
	      ;; location where the `adb shell' user may access it, or
	      ;; to transfer binary data over a `run-as' connection...
	      (ats-exec-script-checked device
				       (format "cat %s/%s"
					       (shell-quote-argument
						(ats-get-staging-directory
						 device package user)
						t)
					       (shell-quote-argument file t))
				       exec-package exec-user)
	      (buffer-string)))))

(defun ats-create-empty-temporary (device name package user)
  "Create an empty temporary file NAME in PACKAGE's staging directory.
DEVICE is the device where this temporary file is to be created.
USER is the user as which PACKAGE is expected to execute, and
value is the name of the said file."
  (let* ((staging-dir (ats-get-staging-directory device package user))
	 (name (concat staging-dir "/" name)))
    (unless (ats-use-private-staging-directory device package user)
      (setq package nil user nil))
    (with-temp-buffer
      (ats-exec-script-checked device
			       (format "cat </dev/null >%s"
				       (shell-quote-argument name t))
			       package user))
    name))

(defun ats-run-jar (device jar class &rest params)
  "Upload and execute the Dalvik archive JAR on DEVICE.
CLASS must be the name of the archive file's main class.  Value
is the exit code of the `app_process' process, and its output is
inserted in the manner of `ats-exec-script'."
  (let* ((jar (expand-file-name jar))
	 (name (file-name-nondirectory jar))
	 (tempname (concat "/data/local/tmp/" name)))
    (with-temp-buffer
      (ats-adb "-s" device "push" jar tempname))
    (ats-exec-script device (concat
			     "export ANDROID_DATA=/data/local/tmp;\n"
			     ;; `dalvik-cache' must be a writable
			     ;; directory in which dalvikvm is
			     ;; able to store optimized dex code.
			     "mkdir /data/local/tmp/dalvik-cache"
			     " &> /dev/null\n"
			     "app_process -Djava.class.path="
			     (shell-quote-argument tempname t)
			     " /data/local/tmp "
			     (shell-quote-argument class t)
			     " "
			     (mapconcat (lambda (arg)
					  (shell-quote-argument arg t))
					params " ")))))

(defun ats-supports-am-force-stop (device)
  "Return whether DEVICE supports the command `am force-stop'."
  (ats-memoize device "ats-supports-am-force-stop"
    (with-temp-buffer
      (ignore-errors
	(ats-adb "-s" device "shell" "am"))
      (not (null (re-search-forward "\\bforce-stop\\b" nil t))))))

(defun ats-supports-am-force-stop-user (device)
  "Return whether DEVICE supports the command `am force-stop --user'."
  (ats-memoize device "ats-supports-am-force-stop-user"
    (with-temp-buffer
      (ignore-errors
	(ats-adb "-s" device "shell" "am"))
      (not (null (re-search-forward
		  "^.*\\bforce-stop\\b[^[:alnum:]]+--user.*$"
		  nil t))))))

(defun ats-kill-process-by-username-and-name (device username name
						     &optional pkgname user)
  "Kill any process with NAME running with the username USERNAME.
If PKGNAME is a debuggable package, do so as that package's user
and as the Android user USER.  DEVICE is the device on which to
operate."
  (let ((any-killed nil))
    (with-temp-buffer
      (dolist (proc (ats-ps-device device
				   (lambda (item _)
				     (and (equal (cdr (assq 'NAME item))
						 name)
					  (equal (cdr (assq 'USER item))
						 username)))))
	(let* ((debuggable (and pkgname
				(ats-is-package-debuggable device pkgname)))
	       (run-as (and debuggable pkgname))
	       (user (and debuggable user))
	       (rc (ats-exec-script device (format "kill -9 %s"
						   (cdr (assq 'PID proc)))
				    run-as user)))
	  (unless (eq rc 0)
	    (error "Could not terminate an existing instance of `%s' (PID %s).
Please attempt to terminate this package by hand (as from the
App Info Settings page) before invoking this command"
		   name (assq 'PID proc)))
	  (setq any-killed t))))
    any-killed))

(defconst ats-portforward-local-type-regexp
  (concat "\\(tcp\\|localabstract\\|localreserved\\|localfilesystem"
	  "\\|dev\\)")
  "Regexp matching valid ADB port forwarding types.")

(defconst ats-portforward-remote-type-regexp
  (concat "\\(tcp\\|localabstract\\|localreserved\\|localfilesystem"
	  "\\|dev\\|jdwp\\|vsock\\|acceptfd\\)")
  "Regexp matching valid ADB port forwarding types.")

(defconst ats-portforward-list-regexp (concat
				       "^"
				       ;; Type & whitespace.
				       "\\(.*\\)[[:space:]]+"
				       ;; Local port type and name.
				       ats-portforward-local-type-regexp ":"
				       "\\(.*\\)[[:space:]]"
				       ;; Local port type and name.
				       ats-portforward-remote-type-regexp ":"
				       "\\(.*\\)$")
  "Regexp with which to parse port forwarding lists printed by ADB.")

(defconst ats-portreverse-type-regexp
  "\\(tcp\\|localabstract\\|localreserved\\|localfilesystem\\)"
  "Regexp matching valid ADB port forwarding types.")

(defconst ats-portreverse-list-regexp (concat
				       "^"
				       ;; Type & whitespace.
				       "\\(.*\\)[[:space:]]+"
				       ;; Remote port type and name.
				       ats-portreverse-type-regexp ":"
				       "\\(.*\\)[[:space:]]"
				       ;; Local port type and name.
				       ats-portreverse-type-regexp ":"
				       "\\(.*\\)$")
  "Regexp with which to parse port forwarding lists printed by ADB.")

(defun ats-reverse-list (device)
  "List connections being reverse-proxied from DEVICE.
Value is a list each of whose elements partakes of the form:

  (TYPE REMOTE-PROTO REMOTE-PORT LOCAL-PROTO LOCAL-PORT)"
  (let ((regexp ats-portreverse-list-regexp)
	(connections nil))
    (with-temp-buffer
      (ats-adb "-s" device "reverse" "--list")
      (while (re-search-forward regexp nil t)
	(push (list (match-string 1) (match-string 2)
		    (match-string 3) (match-string 4)
		    (match-string 5))
	      connections)))
    (nreverse connections)))

(defun ats-reverse-tcp (device local port)
  "Proxy to the local TCP port LOCAL from PORT on DEVICE.
If PORT is 0, select a suitable free port on DEVICE or that of
an existing forwarding session.  Return PORT or the selected
port as the case may be.

This is not supported by all versions of Android."
  (when (and (eq port 0) (< (ats-get-sdk-version device) 26))
    (error "Automatic port selection is unavailable < Android 8.0"))
  (or (let ((str (number-to-string local))
	    (port-str (number-to-string port))
	    (value nil))
	;; Is the local port already being forwarded to PORT (or any
	;; port if that be zero)?
	(dolist (conn (ats-reverse-list device) value)
	  (when (and (equal (nth 3 conn) "tcp")
		     (equal (nth 4 conn) str)
		     (equal (nth 1 conn) "tcp")
		     (or (eq port 0)
			 (equal (nth 2 conn) port-str)))
	    (setq value (string-to-number (nth 2 conn))))))
      (with-temp-buffer
	(ats-adb "-s" device "reverse" (format "tcp:%d" port)
		 (format "tcp:%d" local))
	(let ((num (string-to-number (buffer-string))))
	  (if (zerop num)
	      (if (and (not (eq port 0)) (eq (point-min) (point-max)))
		  port
		(error "Failed to establish reverse proxy \
to `localhost:%d' from `tcp:%d':\n%s" local port (buffer-string)))
	    num)))))

(defun ats-forward-list (device)
  "List connections being proxied to DEVICE.
Value is a list each of whose elements partakes of the form:

  (DEVICE LOCAL-PROTO LOCAL-PORT REMOTE-PROTO REMOTE-PORT)

DEVICE is only returned in the interests of consistency with
`ats-reverse-list'."
  (let ((regexp ats-portforward-list-regexp)
	(connections nil))
    (with-temp-buffer
      (ats-adb "forward" "--list")
      (while (re-search-forward regexp nil t)
	(when (equal (match-string 1) device)
	    (push (list (match-string 1) (match-string 2)
			(match-string 3) (match-string 4)
			(match-string 5))
		  connections))))
    (nreverse connections)))

(defun ats-forward-tcp (device port local)
  "Proxy to the remote TCP port PORT on DEVICE from LOCAL.
If LOCAL is 0, select a suitable local free port or that of an
existing forwarding session.  Return LOCAL or the selected port
as the case may be."
  (or (let ((str (number-to-string port))
	    (local-str (number-to-string local))
	    (value nil))
	;; Is the local port already being forwarded?
	(dolist (conn (ats-forward-list device) value)
	  (when (and (equal (nth 3 conn) "tcp")
		     (equal (nth 4 conn) str)
		     (equal (nth 1 conn) "tcp")
		     (or (eq local 0)
			 (equal (nth 2 conn) local-str)))
	    (setq value (string-to-number (nth 2 conn))))))
      (with-temp-buffer
	(ats-adb "-s" device "forward" (format "tcp:%d" local)
		 (format "tcp:%d" port))
	(let ((num (string-to-number (buffer-string))))
	  (if (zerop num)
	      (if (and (not (eq local 0)) (eq (point-min) (point-max)))
		  local
		(error "Failed to establish proxy \
from `localhost:%d' to `tcp:%d':\n%s" local port (buffer-string)))
	    num)))))

(defun ats-is-tail-available (device)
  "Return whether `tail is available on DEVICE and functional."
  (ats-memoize device "ats-is-tail-available"
    (with-temp-buffer
      (when (eq (ats-exec-script device "tail < /dev/null\n") 0)
	(erase-buffer)
	;; Now run `tail --help' and search for any lines indicating
	;; that `tail -f' is unimplemented, e.g.:
	;; usage: tail [-n|c NUMBER] [-f] [FILE...]

	;; Copy last lines from files to stdout. If no files listed, copy from
	;; stdin. Filename "-" is a synonym for stdin.

	;; -n	output the last NUMBER lines (default 10), +X counts from start.
	;; -c	output the last NUMBER bytes, +NUMBER counts from start
	;; #-f	follow FILE(s), waiting for more data to be appended [TODO]
	;;
	;; This may fail if tail does not implement `--help'.
	(ignore-errors
	  (ats-adb "-s" device "shell" "tail" "--help"))
	(not (re-search-forward "^#?-f.*follow.+TODO.*$" nil t))))))



;; Component management.

(defconst ats-java-int-min (- (expt 2 31))
  "Value of `Integer.MIN_VALUE' in Java.")

(defconst ats-java-int-max (1- (expt 2 31))
  "Value of `Integer.MAX_VALUE' in Java.")

(defconst ats-java-long-min (- (expt 2 63))
  "Value of `Long.MIN_VALUE' in Java.")

(defconst ats-java-long-max (1- (expt 2 63))
  "Value of `Long.MAX_VALUE' in Java.")

(defun ats-intent-array-type (element)
  "Return the type of an Intent array from its first element ELEMENT."
  (cond ((stringp element) "--esa")
	((integerp element) "--eia")
	((and (consp element) (eq (car element) 'long)) "--ela")
	((floatp element) "--efa")
	(t (error "Invalid Intent array element: %s" element))))

(defun ats-fmt-array-element (atype element)
  "Format an array ELEMENT appropriately for an array of type ATYPE."
  (cond ((equal atype "--esa")
	 (if (stringp element)
	     (replace-regexp-in-string "," "\\\\," element)
	   (error "Array elements are not uniform of type")))
	((equal atype "--eia")
	 (if (integerp element)
	     (progn
	       (if (or (< element ats-java-int-min)
		       (> element ats-java-int-max))
		   (error "Integer not representable by Java `int': %d"
			  element)
		 (format "%d" element)))
	   (error "Array elements are not uniform of type")))
	((equal atype "--ela")
	 (if (and (consp element) (eq (car element) 'long))
	     (let ((element (cdr element)))
	       (if (or (< element ats-java-long-min)
		       (> element ats-java-long-max))
		   (error "Integer not representable by Java `long': %d"
			  element)
		 (format "%d" element)))
	   (error "Array elements are not uniform of type")))
	((equal atype "--efa")
	 (if (floatp element)
	     (format "%f" element)
	   (error "Array elements are not uniform of type")))))

(defun ats-build-intent (data)
  "Construct an intent arg list from an alist DATA.
DATA's keys must either be one of the annexed keywords, or a
string property name.  The value of each element with a string
key must be:

  - A string.
  - A cons of the form `(uri . URI)', where URI is an Android URI.
  - A fixnum or bignum, which is treated as an integer and
    mustn't exceed the limits of Java's `int' type's
    representation.
  - A cons of the form `(long . LONG)', where LONG is a fixnum
    or a bignum.
  - A float.
  - A boolean t or nil.
  - A list of any single type of item listed above, excluding
    `(uri . URI)' and booleans.

That which follows is a list of keywords that may appear as keys
juxtaposed with the meaning of their values.

  :action ACTION
  The action taken by this intent, e.g. `android.intent.action.VIEW'.

  :data URI
  URI data to be attached to this intent.

  :type TYPE
  The MIME type of this intent's data.

  :category CATEGORY
  This intent's category, e.g. `android.intent.category.DEFAULT'.

  :component COMPONENT
  This intent's target component, e.g. `org.gnu.emacs/.EmacsActivity'.

  :flags FLAGS
  A fixnum or bignum specifying integer flags affecting the intent.

Value is a list of command line arguments fit to be provided to
`am' commands, or to `AtsStub.class'."
  (let ((directives nil))
    (dolist (element data)
      (let ((key (car element)))
	(cond
	 ((eq key :action)
	  (push "-a" directives)
	  (push (cdr element) directives))
	 ((eq key :data)
	  (push "-d" directives)
	  (push (cdr element) directives))
	 ((eq key :type)
	  (push "-t" directives)
	  (push (cdr element) directives))
	 ((eq key :category)
	  (push "-c" directives)
	  (push (cdr element) directives))
	 ((eq key :component)
	  (push "-n" directives)
	  (push (cdr element) directives))
	 ((eq key :flags)
	  (push "-f" directives)
	  (push (format "%d" (cdr element)) directives))
	 ((stringp key)
	  (let ((value (cdr element)))
	    (cond ((stringp value)
		   (push "-e" directives)
		   (push key directives)
		   (push value directives))
		  ((and (consp value) (eq (car value) 'uri))
		   (push "--eu" directives)
		   (push key directives)
		   (push (cdr value) directives))
		  ((integerp value)
		   (when (or (< value ats-java-int-min)
			     (> value ats-java-int-max))
		     (error "Integer not representable by Java `int': %d"
			    value))
		   (push "--ei" directives)
		   (push key directives)
		   (push (format "%d" value) directives))
		  ((and (consp value) (eq (car value) 'long))
		   (when (or (< (cdr value) ats-java-long-min)
			     (> (cdr value) ats-java-long-max))
		     (error "Integer not representable by Java `long': %d"
			    (cdr value)))
		   (push "--el" directives)
		   (push key directives)
		   (push (format "%d" (cdr value)) directives))
		  ((floatp value)
		   (push "--ef" directives)
		   (push key directives)
		   (push (format "%f" value) directives))
		  ((or (eq value t) (null value))
		   (push "--ez" directives)
		   (push key directives)
		   (push (or (and value "true") "false") directives))
		  ((listp value)
		   (let ((atype (ats-intent-array-type (car value))))
		     (push atype directives)
		     (push key directives)
		     (push (mapconcat (lambda (element)
					(ats-fmt-array-element atype element))
				      value ",")
			   directives)))
		  (t (error "Invalid property value: %s" value)))))
	 (t (error "Invalid key: %s" key)))))
    (nreverse directives)))

(defvar ats-working-stub-file nil
  "Name of a functioning AtsStub Java archive.")

(defvar ats-file-directory)
(defun ats-am-start-intent (device user data)
  "Start an activity identified by the Intent DATA on DEVICE.
DATA should be provided in such a format as `ats-build-intent'
accepts.
USER should identify the Android user for whom DATA will be
started."
  (let ((args (ats-build-intent data)))
    (when (not (eq user 0))
      (push (number-to-string user) args)
      (push "--user" args))
    ;; If the device is running Android 5.0 or later, whose `am' command
    ;; supports array parameter construction, simply invoke `am start'.
    (if (>= (ats-get-sdk-version device) 21)
	(with-temp-buffer
	  (ignore-errors
	    (let ((ats-adb-disable-stderr nil))
	      (ats-adb "-s" device "shell" "sh" "-c"
		       (shell-quote-argument
			(format "am start %s && echo ats_success"
				(mapconcat (lambda (arg)
					     (shell-quote-argument arg t))
					   args " "))
			t))))
	  (goto-char (point-max))
	  (unless (re-search-backward "^ats_success$" nil t)
	    (error "`am start' failed with the following output:\n%s"
		   (buffer-string))))
      ;; Otherwise, invoke a short Java stub class that invokes the
      ;; ActivityManager.
      (let ((stub-file (or ats-working-stub-file
			   (expand-file-name
			    (read-file-name "stub.zip file: "
					    (concat
					     (file-name-as-directory
					      ats-file-directory)
					     (file-name-as-directory "bin"))
					    "stub.zip" t nil
					    (lambda (filename)
					      (member
					       (file-name-extension filename)
					       '("zip" "jar" "dex"))))))))
	(unless (file-regular-p stub-file)
	  (error "Invalid or nonexistent ActivityManager stub: %s"
		 stub-file))
	(with-temp-buffer
	  (unless (zerop (apply #'ats-run-jar device
				stub-file "ats.AtsStub"
				"start" args))
	    (error "ActivityManager stub failed with the following output:\n%s"
		   (buffer-string))))
	;; Save the stub file upon success.
	(setq ats-working-stub-file stub-file))))
  nil)

(defun ats-create-commfile (device package user)
  "Create a file to which a remote program may write data.
DEVICE, PACKAGE, and USER, identify the device and environment
from which the file must be available, in the same sense as in
`ats-get-staging-directory'.

The data written to the file must be exceedingly minuscule (just
adequate to enable a connection to be established between
controller and driver), and such a file ought to be provided to
`ats-watch-commfile', which see."
  (let ((tempname (make-temp-name "ats-commfile-")))
    (ats-create-empty-temporary device tempname package user)))

(defun ats-watch-commfile (device commfile package user)
  "Poll the contents of COMMFILE as PACKAGE and as USER.
Return the contents of the first line written to the file and
delete the same once a newline is written.
DEVICE is the device where COMMFILE resides."
  (unless (ats-use-private-staging-directory device package user)
    (setq package nil user nil))
  (prog1
      (cond ((and (ats-is-tail-available device)
		  ;; `tail -f' is defective on Android <= 8.1.
		  (> (ats-get-sdk-version device) 28))
	     ;; Excellent, tail -f exists.  Collect process output into a
	     ;; buffer till the first newline is received.
	     (let* ((command-line (cond
				   ((eq user 0)
				    (list "-s" device "shell"
					  "run-as" package
					  "tail" "-f" "-c1300" commfile))
				   (user
				    (list "-s" device "shell"
					  "run-as" package
					  "--user" (number-to-string user)
					  "tail" "-f" "-c1300" commfile))
				   (t (list "-s" device "shell"
					    "tail" "-f" "-c1300" commfile))))
		    (process (apply #'ats-start-adb command-line))
		    (time (float-time))
		    (data nil))
	       (set-process-query-on-exit-flag process nil)
	       (with-current-buffer (process-buffer process)
		 (unwind-protect
		     (while (not data)
		       (when (accept-process-output process 1 nil)
			 (when (search-forward "\n" nil t)
			   (setq data (buffer-substring (point-min)
							(1- (point))))))
		       (when (not (eq (process-status process) 'run))
			 (error "`adb' died unexpectedly..."))
		       (message
			"Waiting for response from remote process...  (%d s)"
			(floor (- (float-time) time))))
		   (kill-buffer)))
	       data))
	    (t ;; Periodic polling must be resorted to instead.
	     (let ((value nil)
		   (command-line (cond
				  ((eq user 0)
				   (list "-s" device "shell"
					 "run-as" package
					 "cat" commfile))
				  (user
				   (list "-s" device "shell"
					 "run-as" package
					 "--user" (number-to-string user)
					 "cat" commfile))
				  (t (list "-s" device "shell"
					   "cat" commfile))))
		   (time (float-time)))
	       ;; I would rather have exercised sticky broadcasts, but
	       ;; it's impossible to post them from Emacs Lisp on the
	       ;; driver's side...
	       (with-temp-buffer
		 (while (not value)
		   (sleep-for 1.0)
		   (message
		    "Waiting for response from remote process...  (%d s)"
		    (floor (- (float-time) time)))
		   (erase-buffer)
		   ;; XXX: how ought errors reliably be separated from
		   ;; this command's ordinary output?
		   (apply #'ats-adb command-line)
		   (when (search-forward "\n" nil t)
		     (setq value (buffer-substring (point-min)
						   (1- (point))))))
		 value))))
    (with-temp-buffer
      (ats-exec-script-checked
       device (format "rm %s" (shell-quote-argument commfile t))
       package user))))



;; Connection management.

(defvar ats-file-directory (and load-file-name
				(file-name-directory load-file-name))
  "Directory holding `test-controller.el'.")

(defvar ats-server nil
  "ATS server process or nil if yet unavailable.")

(defvar ats-default-port 45419
  "Port on which ATS servers listen if auto selection is unavailable.")

(defvar ats-accepting-connection nil
  "UUID of connections being established.")

(defvar-local ats-associated-process nil
  "ATS process associated with this buffer.
Such a process will be returned by `ats-read-connection' without
prompting the user.")

(defun ats-address-to-hostname (address)
  "Return the hostname component of the address ADDRESS."
  (progn
    (string-match "\\[?\\(.+?\\)\\]?\\(:[[:alnum:]]+\\)?$" address)
    (match-string 1 address)))

(defun ats-is-localhost-p (address)
  "Return whether the hostname in ADDRESS identifies this machine or is nil."
  (or (not address)
      (let ((host (ats-address-to-hostname address)))
	(let ((address-info (network-lookup-address-info host))
	      (localhost-info (network-lookup-address-info "localhost")))
	  (catch 'result
	    (dolist (addr address-info)
	      (dolist (addr-1 localhost-info)
		(when (equal addr addr-1)
		  (throw 'result t)))))))))

(defun ats-server-sentinel (process _)
  "Sentinel function for ATS connections.
PROCESS is the connection at hand."
  (when (process-get process 'ats-connection-details)
    (ats-disconnect-internal process)
    (kill-buffer (process-buffer process))))

(defun ats-server-log (_ connection _)
  "Log function for `ats-server' processes.
If `ats-accepting-connection' is non-nil, read a string from
CONNECTION identifying the process, and, if in agreement with
the former variable, establish a connection and throw.
Otherwise, terminate the connection."
  (if (not ats-accepting-connection)
      (progn
	(process-send-string connection "-not-accepting-connections\n")
	(delete-process connection))
    (with-current-buffer (process-buffer connection)
      (while connection
	(let ((beg (point)))
	  (message "Device connected...")
	  (when (accept-process-output connection)
	    (goto-char beg)
	    (when (search-forward "\n" (process-mark connection) t)
	      (let ((uuid (buffer-substring (point-min) (1- (point)))))
		(if (equal uuid ats-accepting-connection)
		    (progn
		      (process-send-string connection "-ok\n")
		      (delete-region (point-min) (point))
		      (throw 'connection-established connection))
		  (process-send-string connection
				       (concat "-incorrect-uuid "
					       uuid
					       " "
					       ats-accepting-connection
					       "\n"))
		  (delete-process connection)
		  (setq connection nil))))))))))

(defsubst ats-server-exists-p ()
  "Return whether the ATS server is alive and well.
Value, if non-nil, is the port on which it listens."
  (and ats-server
       (eq (process-status ats-server) 'listen)
       (process-contact ats-server :service)))

(defun ats-start-server ()
  "Start a server to which remote devices may connect.
Alternatively, return a value pertaining to an existing server.
Value is the port on which it will listen."
  (if (ats-server-exists-p)
      (process-contact ats-server :service)
    (let ((process
	   (make-network-process :name " *ats server*"
				 :server t
				 :host 'local
				 :service (if (featurep 'make-network-process
							'(:service t))
					      t
					    ats-default-port)
				 :family 'ipv4
				 :coding 'no-conversion
				 :sentinel #'ats-server-sentinel
				 :log #'ats-server-log)))
      (setq ats-server process)
      (process-contact process :service))))

(defvar ats-await-connection-timeout 180
  "Timeout after which to declare a connection failure.")

(defun ats-await-connection (uuid device)
  "Await a connection by a client identifying as UUID.
DEVICE should be the name of the device to which the connection
is to be established, to be printed in timeout methods.
Value is the connection established between the ATS server,
which must already have been started, and the client.
Signal an error if connection establishment times out."
  (unless (ats-server-exists-p)
    (error "The ATS server is off-line.  Please call `ats-start-server'"))
  (let ((ats-accepting-connection uuid))
    (prog1 (catch 'connection-established
	     (with-timeout (ats-await-connection-timeout
			    (error "Connection to `%s' timed out..."
				   device))
	       (let ((time (float-time)))
		 (while t
		   (message "Connecting...  (%s s)"
			    (let* ((current-time (float-time))
				   (elapsed (- current-time time)))
			      (floor elapsed)))
		   (accept-process-output nil 1)))))
      (message ""))))

(defun ats-forward-server-sentinel (process _)
  "Terminate PROCESS's buffer after it completes."
  (when (not (memq (process-status process) '(run stop)))
    (when (and (process-buffer process)
	       (buffer-live-p (process-buffer process)))
      (kill-buffer (process-buffer process)))))

(defun ats-forward-server-filter (process string)
  "Prompt for a password or other details if requested by PROCESS.
Set the process property `ats-connection-established' to t if a
string indicating success is read, and insert STRING."
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (let ((string (string-replace "\r" "" string)))
	(insert string)
	(cond
	 ((string-prefix-p "ATS_CONNECTION_ESTABLISHED" string)
	  (process-put process 'ats-connection-established t))
	 ((string-match comint-password-prompt-regexp string)
	  (process-send-string
	   process (concat (read-passwd string) "\n")))
	 ((string-match tramp-yesno-prompt-regexp string)
	  (process-send-string
	   process (concat
		    (or (and (yes-or-no-p string) "yes") "no") "\n")))
	 ((string-match tramp-yn-prompt-regexp string)
	  (process-send-string
	   process (concat
		    (or (and (y-or-n-p string) "yes") "no") "\n"))))))))

(defun ats-reverse-server (address port)
  "Proxy to port PORT here from the server at ADDRESS, over SSH.
Value is the port at the destination."
  (if (ats-is-localhost-p address)
      port
    (let* ((host (ats-address-to-hostname address))
	   (name (format " *ats-reverse-server %s:%d*" host port))
	   (existing-process (get-process name)))
      ;; Is that connection available?
      (if (and existing-process
	       (process-get existing-process 'ats-connection-established))
	  port
	;; Kill it.
	(when existing-process
	  (kill-process existing-process))
	(let ((process (start-process name name "ssh" "-o"
				      "ExitOnForwardFailure=yes"
				      "-R"
				      (format "%d:localhost:%d" port port)
				      host
				      (concat
				       "echo ATS_CONNECTION_ESTABLISHED; "
				       "while :; do sleep 10; done"))))
	  (set-process-sentinel process #'ats-forward-server-sentinel)
	  (set-process-filter process #'ats-forward-server-filter)
	  (save-window-excursion
	    (pop-to-buffer (process-buffer process))
	    (while (not (process-get process 'ats-connection-established))
	      (if (not (eq (process-status process) 'run))
		  (error "ssh forwarding failed with exit code: %d"
			 (process-exit-status process))
		(accept-process-output process))))
	  port)))))

(defun ats-forward-server (address port)
  "Forward from hence to the service at PORT on server ADDRESS over SSH.
Value is the local port which being forwarded to the destination."
  (if (ats-is-localhost-p address)
      port
    (let* ((host (ats-address-to-hostname address))
	   (name (format " *ats-forward-server %s:%d*" host port))
	   (existing-process (get-process name)))
      ;; Is that connection available?
      (if (and existing-process
	       (process-get existing-process 'ats-connection-established))
	  port
	;; Kill it.
	(when existing-process
	  (kill-process existing-process))
	(let ((process (start-process name name "ssh" "-o"
				      "ExitOnForwardFailure=yes"
				      "-L"
				      (format "%d:localhost:%d" port port)
				      host
				      (concat
				       "echo ATS_CONNECTION_ESTABLISHED; "
				       "while :; do sleep 10; done"))))
	  (set-process-sentinel process #'ats-forward-server-sentinel)
	  (set-process-filter process #'ats-forward-server-filter)
	  (save-window-excursion
	    (pop-to-buffer (process-buffer process))
	    (while (not (process-get process 'ats-connection-established))
	      (if (not (eq (process-status process) 'run))
		  (error "ssh forwarding failed with exit code: %d"
			 (process-exit-status process))
		(accept-process-output process))))
	  port)))))

(defun ats-cancel-forward-server (address port)
  "Cease forwarding to PORT at ADDRESS over SSH."
  (unless (ats-is-localhost-p address)
    (let* ((host (ats-address-to-hostname address))
	   (name (format " *ats-forward-server %s:%d*" host port))
	   (process (get-process name)))
      (with-local-quit
	(when (and process
		   (memq (process-status process) '(run stop)))
	  (interrupt-process process)
	  (while (memq (process-status process) '(run stop))
	    (accept-process-output process nil nil t)))))))

(defconst ats-remote-port 10053
  "ATS port on devices with reverse forwarding but no auto port selection.
This is offset by the user ID.")

(defmacro ats-in-connection-context (process details &rest bodyforms)
  "Evaluate BODYFORMS in PROCESS's context.
Bind PROCESS's connection details to DETAILS, bind
`ats-adb-host' to the value under which PROCESS was created, and
select PROCESS's buffer."
  (declare (indent 2))
  (let ((old-proc process) (process (gensym)))
    `(let* ((,process ,old-proc)
	    (,details (process-get ,process 'ats-connection-details)))
       (with-current-buffer (process-buffer ,process)
	 (unless ,details
	   (error "Not an ATS process: %S" ,process))
	 (let ((ats-adb-host (cdr (assq 'host ,details))))
	   ,@bodyforms)))))

(defvar ats-outstanding-reverse-connection nil
  "If non-nil, a list of (HOST DEVICE REMOTE-PORT).
Which elements are, respectively, the hostname, device, and
remote port of a reverse proxy connection reserved for a
connection still being established that mustn't be terminated.")

(defun ats-terminate-reverse-safely (device remote-port &optional process)
  "Terminate a reverse forwarding connection from DEVICE:REMOTE-PORT if unused.
Call `adb -s DEVICE reverse --remove tcp:REMOTE-PORT' safely.
That is to say, unless REMOTE-PORT on DEVICE is reserved by any
connection presently established or being established, with the
exception of PROCESS, if specified."
  (let ((canon-host (or ats-adb-host "localhost")))
    (catch 'abort
      ;; Cancel reverse forwarding, but only after guaranteeing that no
      ;; other connections exist with the same remote port and device.
      (dolist (proc (process-list))
	(let ((details (and (not (eq process proc))
			    (process-get proc 'ats-connection-details))))
	  (when details
	    (let ((other-host (or (cdr (assq 'host details)) "localhost"))
		  (other-device (cdr (assq 'device details)))
		  (other-remote-port (cdr (assq 'remote-port details))))
	      (when (and (equal canon-host other-host)
			 (equal device other-device)
			 (eq remote-port other-remote-port))
		(throw 'abort nil))))))
      ;; And that the port is not reserved for any connection in
      ;; the making.
      (when ats-outstanding-reverse-connection
	(let ((other-host (nth 0 ats-outstanding-reverse-connection))
	      (other-device (nth 1 ats-outstanding-reverse-connection))
	      (other-port (nth 2 ats-outstanding-reverse-connection)))
	  (when (and (equal canon-host other-host)
		     (equal device other-device)
		     (eq remote-port other-port))
	    (throw 'abort nil))))
      (message
       "Canceling reverse forwarding to `%s:%d' from `localhost'"
       device remote-port)
      (ats-adb "-s" device "reverse" "--remove"
	       (format "tcp:%d" remote-port)))))

(defun ats-disconnect-internal (process)
  "Clean up the ATS connection represented by PROCESS.
If the connection was initiated by forwarding to the device,
terminate the local forwarding process if any, and remove the
port forward from the destination.  If initialization was
effected by reverse forwarding from the device, terminate this
reverse forwarding session if no other process is forwarding on
the same port."
  (ats-in-connection-context (get-process process) details
    (let ((device (cdr (assq 'device details)))
	  (method (cdr (assq 'connection-method details))))
      (when (eq method 'forward)
	(with-demoted-errors "Error in disconnecting device: %S"
	  ;; It is necessary to cancel port forwarding from the device
	  ;; to this host.
	  (let ((host-port (cdr (assq 'host-port details))))
	    (message "Canceling port forwarding from `localhost' to `%s:%d'"
		     ats-adb-host host-port)
	    (ats-cancel-forward-server ats-adb-host host-port)))
	(with-demoted-errors "Error in disconnecting device: %S"
	  ;; It is necessary to cancel port forwarding from the device
	  ;; to this host.
	  (let ((host-port (cdr (assq 'host-port details))))
	    (message "Canceling port forwarding from the device to `%s:%d'"
		     ats-adb-host host-port)
	    (ats-adb "-s" device "forward" "--remove"
		     (format "tcp:%d" host-port)))))
      (when (eq method 'reverse)
	(with-demoted-errors "Error in disconnecting device: %S"
	  (let ((remote-port (cdr (assq 'remote-port details))))
	    (ats-terminate-reverse-safely device remote-port process)))))))

(defun ats-read-connection (prompt)
  "Read an ATS connection from the user, with completion.
If `ats-associated-process' is set in the current buffer, return
this process if it remains alive.  PROMPT is the prompt
displayed by `completing-read'.  Value is a process representing
such a connection."
  (or (and ats-associated-process
	   (eq (process-status ats-associated-process) 'open)
	   ats-associated-process)
      (let ((procs))
	(dolist (proc (process-list))
	  (when (process-get proc 'ats-connection-details)
	    (push (buffer-name (process-buffer proc)) procs)))
	(let ((buffer (completing-read prompt procs
				       nil t nil
				       'ats-read-processes)))
	  (get-buffer-process buffer)))))

(defun ats-disconnect (process)
  "Disconnect from the ATS connection represented by PROCESS.
Interactively, prompt for a process to disconnect.

Close PROCESS's connection if appropriate and remove any port
forwarding currently in place."
  (interactive (list (ats-read-connection "Disconnect from: ")))
  (ats-in-connection-context (get-process process) details
    (delete-process process)))

(defun ats-establish-connection (process details &optional interactive)
  "Finalize a connection represented by PROCESS.
DETAILS should be an alist of connection information to which
`ats-adb-host' is appended, with the following keys:

  - `connection-method'
    Either `forward' or `reverse', indicating respectively that
    the connection was established by forwarding to the remote
    device and by forwarding from the local device.

  - `device'
    Serial number of the device, identifying it to ADB.

  - `user'
    ID of the user on the device as which the remote process
    executes.

  - `local-port'
    That port from which `host-port' on the ADB host system is
    being forwarded to, if `connection-method' is `forward'.

  - `remote-port'
    That port to which `host-port' is being forwarded from,
    if `connection-method' is `reverse'.

  - `host-port'
    The port on the ADB host system mediating between the local
    and the remote system.

If INTERACTIVE, open a Lisp interaction buffer with
`ats-open-lisp-interaction-buffer'.

Value is PROCESS itself."
  (process-put process 'ats-connection-details
	       (append `((host . ,ats-adb-host)
			 (eval-serial . 0))
		       details))
  (let ((device (cdr (assq 'device details)))
	(user (cdr (assq 'user details)))
	(host (or ats-adb-host "localhost")))
    (with-current-buffer (process-buffer process)
      (if (eq user 0)
	  (rename-buffer (format " *ats connection for %s (on %s)*"
				 device host)
			 t)
	(rename-buffer (format " *ats connection for %s (on %s, as %d)*"
			       device host user)
		       t)))
    (message "Connection established to %s (on %s)"
	     (cdr (assq 'device details)) host))
  (prog1 process
    (when interactive
      (ats-open-lisp-interaction-buffer process))))

;;;###autoload
(defun ats-connect (device user &optional host interactive)
  "Establish a connection to DEVICE on HOST executing as USER.
HOST, if nil, defaults to `ats-adb-host'.
If an instance of Emacs is already executing on DEVICE and the
test driver is available, connect to this test driver.
Otherwise, terminate any existing Emacs sessions, upload the
test driver, load it into a new Emacs session, and establish a
connection.

Interactively, prompt for a device and a user on the device to
which to connect.  With a prefix argument, also prompt for the
address of an ADB daemon on a host machine whose devices are to
be connected to (which requires that OpenSSH be installed on
this machine and an SSH daemon be executing on the host)."
  (interactive (let* ((host (or (and current-prefix-arg
				     (read-string "ADB hostname: "))
				ats-adb-host))
		      (ats-adb-host host)
		      (device
		       (completing-read "Connect to device: "
					(mapcar #'car
						(ats-online-devices))
					nil t nil 'ats-connect-device))
		      (user-alist
		       (mapcar (lambda (user)
				 (cons (format "%s (%d)"
					       (cadr user) (car user))
				       (car user)))
			       (ats-list-users device)))
		      (user
		       (let ((completions-sort nil))
			 (completing-read "Select a user: "
					  user-alist nil t))))
		 (list device (or (cdr (assoc user user-alist))
				  (error "Unknown user: %s" user))
		       host t)))
  ;; Terminate any existing instances of Emacs executing as this user.
  (let* ((ats-adb-host host)
	 (emacs-aid (ats-get-package-aid device "org.gnu.emacs"))
	 (emacs-uid (ats-aid-to-uid emacs-aid user))
	 (emacs-username (ats-uid-to-username device emacs-uid)))
    ;; Start Emacs and arrange to load the test driver.
    (cond
     ((ats-supports-am-force-stop-user device)
      (with-temp-buffer
	(ats-adb "-s" device "shell" "am" "force-stop" "--user"
		 (number-to-string user) "org.gnu.emacs")))
     ((and (ats-supports-am-force-stop device)
	   (eq user 0))
      (with-temp-buffer
	(ats-adb "-s" device "shell" "am" "force-stop"
		 "org.gnu.emacs")))
     (t (when (ats-kill-process-by-username-and-name
	       device emacs-username "org.gnu.emacs" "org.gnu.emacs" user)
	  (dotimes (_ 3)
	    ;; This must be repeated several times or the ActivityManager
	    ;; may attempt to restart Emacs with the previous intent's
	    ;; parameters.
	    (sleep-for 0.25)
	    (ats-kill-process-by-username-and-name
	     device emacs-username "org.gnu.emacs" "org.gnu.emacs" user))))))
  ;; Upload the test driver.
  (let* ((ats-adb-host host)
	 (staging-directory (ats-get-staging-directory device
						       "org.gnu.emacs"
						       user))
	 (ats-file (let ((file (and ats-file-directory
				    (concat (file-name-as-directory
					     ats-file-directory)
					    "test-driver.el"))))
		     (or (and file (file-exists-p file) file)
			 (read-file-name "ATS test driver file: "))))
	 (ats-early-init-file
	  (let ((file (and ats-file-directory
			   (concat (file-name-as-directory
				    ats-file-directory)
				   "early-init.el"))))
	    (or (and file (file-exists-p file) file)
		(read-file-name "ATS early-init file: "))))
	 (file (ats-upload device ats-file "org.gnu.emacs" user))
	 (_ (ats-upload device ats-early-init-file
			"org.gnu.emacs" user))
	 ;; Start the server.
	 (server-port (ats-start-server))
	 ;; Forward the server to the ADB host.
	 (host-port (ats-reverse-server ats-adb-host server-port))
	 ;; Forward the server to the device.
	 (remote-port (ignore-errors
			(if (>= (ats-get-sdk-version device) 26)
			    ;; Automatically select a port to open on
			    ;; the device.
			    (ats-reverse-tcp device host-port 0)
			  ;; Derive a fixed port from the user ID.
			  (ats-reverse-tcp device host-port
					   (+ ats-remote-port user)))))
	 (uuid (if (executable-find "uuidgen")
		   (string-trim
		    (shell-command-to-string "uuidgen"))
		 (format "%x" (random most-positive-fixnum))))
	 process)
    (if remote-port
	(progn
	  ;; Launch Emacs with arguments directing it to load the test
	  ;; driver file and connect to the local port, and begin to
	  ;; wait.
	  ;;
	  ;; Care must be exercised that process sentinels are not
	  ;; executed before `ats-outstanding-reverse-connection' is
	  ;; bound or after a connection is established!
	  (unwind-protect
	      (let ((ats-outstanding-reverse-connection
		     (list (or ats-adb-host "localhost")
			   device remote-port)))
		(ats-am-start-intent
		 device user
		 `((:component . "org.gnu.emacs/.EmacsActivity")
		   ("org.gnu.emacs.STARTUP_ARGUMENTS"
		    "--load" ,file
		    ;; Set the Emacs home directory to the ATS staging
		    ;; directory, where an early-init.el should be
		    ;; uploaded that inhibits the deletion of the
		    ;; initial frame.
		    "--init-directory" ,staging-directory
		    "--eval"
		    ,(format "(ats-establish-connection \"localhost\" %d \"%s\")"
			     remote-port uuid))))
		(setq process
		      (let* ((process (ats-await-connection uuid device)))
			(ats-establish-connection
			 process `((connection-method . reverse)
				   (remote-port . ,remote-port)
				   (host-port . ,host-port)
				   (user . ,user)
				   (device . ,device))
			 interactive))))
	    ;; On failure, cease forwarding to this device, but permit
	    ;; the connection to the host to remain.
	    (unless process
	      (with-demoted-errors "Winding up failed connection: %S"
		(ats-terminate-reverse-safely device remote-port))))
	  process)
      (message "Reverse forwarding is unsupported by this device.")
      (sit-for 1 t)
      (message "Instructing the device to establish a proxy connection instead.")
      (sit-for 1 t)
      ;; Since there are no alternative means by which to communicate
      ;; with a non-debuggable Emacs instance, create a file accessible
      ;; both to ADB and to Emacs, and arrange to store Emacs's server
      ;; port there.
      (let ((commfile (ats-create-commfile device "org.gnu.emacs" user)))
	(ats-am-start-intent
	 device user
	 `((:component . "org.gnu.emacs/.EmacsActivity")
	   ("org.gnu.emacs.STARTUP_ARGUMENTS"
	    "--load" ,file
	    ;; Set the Emacs home directory to the ATS staging
	    ;; directory, where an early-init.el should be uploaded that
	    ;; inhibits the deletion of the initial frame.
	    "--init-directory" ,staging-directory
	    "--eval"
	    ,(format "(ats-initiate-connection %S)" commfile))))
	(let* ((portno (with-timeout
			   (ats-await-connection-timeout
			    (error "Connection to `%s' timed out..." device))
			 (ats-watch-commfile device commfile
					     "org.gnu.emacs" user)))
	       (remote-port (string-to-number portno)))
	  (when (zerop remote-port)
	    (error "Failed to read port number from device"))
	  ;; Forward it.
	  (let* ((host-port (ats-forward-tcp device remote-port 0))
		 (name (format " *ats connection for %s (on %s)*"
			       device (or ats-adb-host "localhost")))
		 local-port process)
	    (condition-case err
		(progn
		  (setq local-port (ats-forward-server ats-adb-host host-port))
		  (setq process (make-network-process
				 :name name
				 :buffer name
				 :host 'local
				 :service local-port
				 :coding 'no-conversion
				 :sentinel #'ats-server-sentinel))
		  (process-send-string process "-ok\n")
		  (ats-establish-connection process
					    `((connection-method . forward)
					      (local-port . ,local-port)
					      (host-port . ,host-port)
					      (user . ,user)
					      (device . ,device))
					    interactive))
	      (error
	       (when process
		 ;; Finalize the failed process as best as can be
		 ;; managed.
		 (with-demoted-errors "Winding up failed connection: %S"
		   (ats-disconnect-internal process)))
	       (when local-port
		 (with-demoted-errors "Winding up failed connection: %S"
		   ;; Though local-port serves to attest whether a
		   ;; forwarding connection has been established, yet it
		   ;; is the destination port that identifies such a
		   ;; connection to `ats-cancel-forward-server', which
		   ;; is not consistent with `adb forward --remove'.
		   (ats-cancel-forward-server ats-adb-host host-port)))
	       (with-demoted-errors "Winding up failed connection: %S"
		 (ats-adb "-s" device "forward" "--remove"
			  (format "tcp:%d" host-port)))
	       (signal (car err) (cdr err))))))))))



;; Command submission and execution.

;; (defvar ats-eval-tm 0)

(defun ats-eval (process form &optional as-printed raw)
  "Evaluate FORM in PROCESS, which form must be printable.
Form should evaluate to a value that must be printable, or
signal an error.  Value is (ok . VALUE) if no error was
signaled, or (error . VALUE) otherwise.  It may also be (exit
. BACKTRACE) if Emacs exited whilst FORM was executing.  If RAW,
instruct PROCESS not to attempt to decode the printed
representation of FORM as multibyte text; this does not
influence the decoding whatever value it returns.

Set AS-PRINTED to insist that the value be returned as a string;
this enables non-printable values to be returned in a meaningful
manner."
  (ats-in-connection-context process details
    (save-restriction
      (let* ((str (encode-coding-string
		   (prin1-to-string form) 'utf-8-emacs t))
	     (length (length str))
	     (serial (setf (alist-get 'eval-serial details)
			   (1+ (alist-get 'eval-serial details))))
	     (serial-str (number-to-string serial))
	     (request-regexp (rx bol "\fats-request:"
				 (literal serial-str)
				 " " (group (+ digit)) "\n"))
	     (point (point))
	     size form)
	(process-send-string process
			     (format "-eval %d %d %s %s\n" serial
				     length
				     (if as-printed "t" "nil")
				     (if raw "nil" "t")))
	(process-send-string process str)
	;; Read the resultant form.
	(while (not form)
	  (when (not (eq (process-status process) 'open))
	    (error "Connection terminated unexpectedly..."))
	  ;; (let ((t1 (float-time)))
	  ;;   (prog1 (accept-process-output process nil nil 1)
	  ;;     (setq ats-eval-tm (+ (- (float-time) t1)
	  ;;	       ats-eval-tm))))
	  (when (accept-process-output process)
	    (when (not size)
	      ;; First skip all output till the header is read.
	      (save-excursion
		(goto-char point)
		(when-let* ((start (re-search-forward
				    request-regexp nil t)))
		  (setq size (string-to-number (match-string 1)))
		  (delete-region (point-min) (point)))))
	    (when size
	      ;; Read SIZE bytes from the process.
	      (when (>= (- (point-max) (point-min)) size)
		(narrow-to-region (point-min) (+ (point-min) size))
		(goto-char (point-min))
		(setq form (car (read-from-string
				 (decode-coding-string
				  (buffer-string)
				  'utf-8-unix t))))))))
	form))))



;; Remote Lisp Interaction mode.

(defvar ats-remote-eval-defuns
  '(progn
     (defalias 'ats-remote-eval-on-device
       #'(lambda (form)
	   "Remotely evaluate a submitted form FORM.
Collect FORM's standard output and return values, and return a
list of the form (ok STANDARD-OUTPUT VALUE VALUE-TRUNCATED),
where STANDARD-OUTPUT is any output the form has printed or
inserted, VALUE is FORM's value, and VALUE-TRUNCATED is FORM's
value after truncation as in the manner of `eval-expression',
both as strings.

If FORM should signal an error, value becomes (error ERROR),
where ERROR is a cons of the error's symbol and of its data."
	   (condition-case error
	       (let ((standard-output
		      (get-buffer-create "*ats-standard-output*")))
		 (with-current-buffer standard-output
		   (erase-buffer)
		   (let ((value (eval form nil)))
		     (list 'ok (buffer-string)
			   (prin1-to-string value)
			   (let ((print-length eval-expression-print-length)
				 (print-level eval-expression-print-level))
			     (prin1-to-string value))))))
	     (error (list 'error error))))))
  "Forms to be evaluated on the remote device before remote evaluation.")

(defun ats-remote-eval-print-sexp
    (value value-truncated output &optional no-truncate)
  "Print VALUE and VALUE-TRUNCATED (a string) to OUTPUT.
The manner of printing is subject to NO-TRUNCATE.
Adapted from `elisp--eval-last-sexp-print-value' in
`elisp-mode.el'."
  (let* ((unabbreviated value) (beg (point)) end)
    (prog1 (princ (if no-truncate
		      value
		    value-truncated)
		  output)
      (setq end (point))
      (when (and (bufferp output)
		 (or (not (null print-length))
		     (not (null print-level)))
		 (not (string= unabbreviated
			       (buffer-substring-no-properties beg end))))
	(last-sexp-setup-props beg end value
			       unabbreviated
			       (buffer-substring-no-properties beg end))))))

(defun ats-remote-eval-for-interaction (process form &optional no-truncate)
  "Evaluate FORM for Lisp interaction in a remote device.
PROCESS represents the connection to the said device.  Insert
text printed by FORM to standard output and its return value on
success, as would `eval-last-sexp', and signal an error on
failure.
If NO-TRUNCATE, print FORM's value in full without truncation."
  (let ((details (process-get process 'ats-connection-details))
	rc)
    ;; First, set up a utility function.
    (unless (cdr (assq 'remote-eval-initialized details))
      (setq rc (ats-eval process ats-remote-eval-defuns))
      (when (eq (car rc) 'error)
	(error "Could not initialize remote evaluation: %S"
	       (cdr rc)))
      (process-put process 'ats-connection-details
		   (cons '(remote-eval-initialized . t) details)))
    ;; Next, really evaluate the form, and also, recognize and convert
    ;; errors in preparing to evaluate the form appropriately.
    (let ((value (ats-eval process
			   `(let ((eval-expression-print-length
				   ,eval-expression-print-length)
				  (eval-expression-print-level
				   ,eval-expression-print-level))
			      (ats-remote-eval-on-device ',form)))))
      (cond ((eq (car value) 'ok)
	     ;; The form was read successfully, but evaluation may
	     ;; nevertheless have terminated with an error.
	     (let ((value (cdr value)))
	       (cond ((eq (car value) 'ok)
		      (insert (cadr value))
		      (ats-remote-eval-print-sexp (caddr value)
						  (cadddr value)
						  (current-buffer)
						  no-truncate))
		     ((eq (car value) 'error)
		      (signal (caadr value)
			      (cdadr value))))))
	    ((eq (car value) 'error)
	     ;; The device could not decode the form.
	     (error "Error decoding form on device: %S" (cdr value)))))))

(defun ats-remote-eval-print-last-sexp (process &optional arg)
  "Evaluate sexp before point; print value into the current buffer.
Evaluation transpires in the device controlled by the remote
connection represented by PROCESS.  ARG inhibits truncation of
printed values, as in `eval-print-last-sexp'."
  (interactive (list (ats-read-connection "Connection: ")
		     current-prefix-arg))
  (insert "\n")
  (ats-remote-eval-for-interaction process (elisp--preceding-sexp)
				   arg)
  (insert "\n"))

(defun ats-remote-eval-last-sexp (process &optional arg)
  "Evaluate sexp before point.
Subsequently, print value and inserted text in the echo area.
Evaluation transpires in the device controlled by the remote
connection represented by PROCESS.  ARG inhibits truncation of
printed values, as in `eval-print-last-sexp'."
  (interactive (list (ats-read-connection "Connection: ")
		     current-prefix-arg))
  (let ((sexp (elisp--preceding-sexp)))
    (with-temp-buffer
      (ats-remote-eval-for-interaction process sexp arg)
      (message (buffer-string)))))

(defun ats-remote-eval-defun (process)
  "Evaluate defun around or after point.
Evaluation transpires in the device controlled by the remote
connection represented by PROCESS."
  (interactive (list (ats-read-connection "Connection: ")))
  (let ((standard-output t) form)
    ;; Read the form from the buffer, and record where it ends.
    (save-excursion
      (end-of-defun)
      (beginning-of-defun)
      (setq form (read (current-buffer))))
    (with-temp-buffer
      (ats-remote-eval-for-interaction process form)
      (message (buffer-string)))))

(defun ats-remote-eval-region-or-buffer (process)
  "Evaluate the forms in the active region or the whole buffer.
Evaluation transpires in the device controlled by the remote
connection represented by PROCESS."
  (interactive (list (ats-read-connection "Connection: ")))
  (let ((evalstring (if (use-region-p)
			(buffer-substring (region-beginning)
					  (region-end))
		      (buffer-string))))
    (ats-eval process `(with-temp-buffer
			 (insert ,evalstring)
			 (eval-buffer)))))

(defvar ats-lisp-interaction-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap eval-print-last-sexp]
		#'ats-remote-eval-print-last-sexp)
    (define-key map [remap eval-defun]
		#'ats-remote-eval-defun)
    (define-key map [remap elisp-eval-region-or-buffer]
		#'ats-remote-eval-region-or-buffer)
    (define-key map [remap eval-last-sexp]
		#'ats-remote-eval-last-sexp)
    map)
  "Keymap applied in `ats-lisp-interaction-mode' buffers.")

(easy-menu-define ats-lisp-interaction-mode-menu
  ats-lisp-interaction-mode-map
  "Menu for Ats Lisp Interaction mode."
  '("Lisp-Interaction"
    ["Complete Lisp Symbol" completion-at-point
     :help "Perform completion on Lisp symbol preceding point"]
    ["Indent or Pretty-Print" indent-pp-sexp
     :help "Indent each line of the list starting just after point, or prettyprint it"]
    ["Evaluate and Print" ats-remote-eval-print-last-sexp
     :help "Evaluate sexp before point; print value into current buffer"]
    ["Evaluate Defun" ats-remote-eval-defun
     :help "Evaluate the top-level form containing point, or after point"]))

(define-derived-mode ats-lisp-interaction-mode lisp-interaction-mode
  `("Remote Lisp Interaction"
    (:eval (unless (and ats-associated-process
			(processp ats-associated-process)
			(eq (process-status ats-associated-process)
			    'open))
	     ,(propertize " disconnected" 'face 'error))))
  "Variant of `lisp-interaction-mode' that executes forms remotely.
This derivative of `lisp-interaction-mode' rebinds such commands
as \\[eval-print-last-sexp] to variants which submit forms for
execution on remote Android devices connected over `adb'.  It
also disables a number of features unsupported by remote
execution facilities, such as edebug.")

(defun ats-open-lisp-interaction-buffer (process)
  "Open an Ats Lisp Interaction Mode buffer on PROCESS
Create and display a buffer in `ats-lisp-interaction-mode'; that
is, a mode akin to `lisp-interaction-mode' but which submits
forms typed to a remote Android device over the connection
represented by PROCESS."
  (interactive (list (ats-read-connection "Connection: ")))
  (ats-in-connection-context process details
    (let ((device (cdr (assq 'device details)))
	  (user (cdr (assq 'user details))))
      (with-current-buffer (get-buffer-create
			    (format "*Lisp Interaction in %s (on %s%s)*"
				    device
				    (or ats-adb-host "localhost")
				    (if (not (eq user 0))
					(format ", as %d" user)
				      "")))
	(ats-lisp-interaction-mode)
	(setq ats-associated-process process)
	(when (eq (buffer-size) 0)
	  (insert (format "\
;; This buffer enables typed Lisp forms to be executed in the device `%s' on `%s'.
;; View the doc string of `ats-lisp-interaction-mode' for specifics.\n\n"
			  device
			  (or ats-adb-host "localhost")))
	  (save-excursion
	    (goto-char (point-min))
	    (fill-region (point) (progn
				   (end-of-line)
				   (point)))
	    (goto-char (point-max))
	    (beginning-of-line)
	    (fill-region (point) (point-max))))
	(pop-to-buffer (current-buffer))))))


;; ERT regression testing.

(defvar ats-emacs-test-directory
  (and load-file-name
       (expand-file-name
	(concat (file-name-directory load-file-name)
		"../../")))
  "Directory in which to locate Emacs regression tests, or nil otherwise.")

(defun ats-upload-test (process dir test-name)
  "Upload a test file and its resources to a remote device.
PROCESS represents the connection to the device.
TEST-NAME concatenated with \"-tests.el\" should identify a file
in DIR implementing a series of ERC regression tests.  If there
is additionally a directory by the name TEST-NAME-resources in
the same directory, upload it to the remote device also.
Once uploaded, tests defined in the file may be loaded and
executed by means of `ats-exec-tests'."
  (interactive
   (let* ((connection (ats-read-connection "Connection: "))
	  (dir (or ats-emacs-test-directory
		   (read-directory-name "Test base directory: "
					nil nil t)))
	  (test (completing-read "Test to upload: "
				 (ats-list-tests-locally dir)
				 nil t nil
				 'ats-uploaded-tests)))
     (list connection dir test)))
  (let* ((dir-name (file-name-as-directory
		    (expand-file-name dir)))
	 (test-file
	  (concat dir-name test-name "-tests.el"))
	 (internal-resource-directory
	  (concat dir-name (file-name-directory test-name)
		  "resources"))
	 (resources-directory
	  (if (file-directory-p internal-resource-directory)
	      internal-resource-directory
	    (concat dir-name test-name "-resources")))
	 ;; Strip all directories from the test name.
	 (default-directory (file-name-directory test-file)))
    (unless (file-regular-p test-file)
      (error "Not a regular file: %s" test-file))
    (if (file-directory-p resources-directory)
	;; Create a compressed tar file.  Though a cpio implementation
	;; exists in the sources for Android 2.2's command line tools,
	;; yet it is often deleted in release builds of the OS to reduce
	;; storage utilization, so it is best to resort to tar and gzip,
	;; which Emacs is able to decompress without command line
	;; utilities.
	(let ((temp-file (make-temp-file "ats-" nil ".tar"))
	      (bare-test-file (file-name-nondirectory test-file))
	      (bare-test-resources
	       (file-name-nondirectory resources-directory)))
	  (unwind-protect
	      (progn
		(let ((rc (call-process
			   "tar" nil nil nil "cfh" temp-file
			   bare-test-file bare-test-resources)))
		  (unless (eq 0 rc)
		    (error "tar exited with code: %d" rc)))
		;; Compress this file.
		(with-temp-buffer
		  (set-buffer-multibyte nil)
		  (let ((rc (call-process "gzip" nil '(t nil) nil
					  "-c" temp-file)))
		    (unless (eq 0 rc)
		      (error "gzip -c exited with code: %d" rc))
		    ;; Write this compressed data to the destination and
		    ;; decompress it there.
		    (let ((rc (ats-eval
			       process
			       `(with-temp-buffer
				  (set-buffer-multibyte nil)
				  (insert ,(buffer-string))
				  (zlib-decompress-region (point-min)
							  (point-max))
				  (let ((dir
					 (concat (file-name-as-directory
						  temporary-file-directory)
						 "ats-tests/" ,test-name)))
				    (if (file-directory-p dir)
					(let ((files
					       (directory-files-recursively
						dir ""))
					      (default-directory dir))
					  (mapc #'delete-file files))
				      (make-directory dir t))
				    (let ((default-directory dir)
					  ;; Otherwise file name handlers
					  ;; such as `epa-file-handler'
					  ;; are liable to interfere with
					  ;; the extraction process.
					  (file-name-handler-alist nil))
				      (require 'tar-mode)
				      (tar-mode)
				      (tar-untar-buffer))))
			       nil t)))
		      (when (eq (car rc) 'error)
			(error "Remote error: %S" (cdr rc)))
		      (message "Uploaded test `%s'" test-name)))))
	    (with-demoted-errors "Removing temporary file: %S"
	      (delete-file temp-file))))
      ;; Just compress and transfer the file alone.
      (with-temp-buffer
	(set-buffer-multibyte nil)
	(let ((rc (call-process "gzip" nil '(t nil) nil
				"-c" test-file)))
	  (unless (eq 0 rc)
	    (error "gzip -c exited with code: %d" rc))
	  ;; Write this compressed data to the destination and
	  ;; decompress it there.
	  (let ((rc (ats-eval
		     process
		     `(with-temp-buffer
			(set-buffer-multibyte nil)
			(insert ,(buffer-string))
			(zlib-decompress-region (point-min)
						(point-max))
			(let* ((dir
				(concat (file-name-as-directory
					 temporary-file-directory)
					"ats-tests/" ,test-name))
			       (dir-1 (file-name-as-directory dir)))
			  (if (file-directory-p dir)
			      (let ((files
				     (directory-files-recursively
				      dir ""))
				    (default-directory dir))
				(mapc #'delete-file files))
			    (make-directory dir t))
			  (write-region
			   (point-min) (point-max)
			   (concat dir-1 ,(file-name-nondirectory
					   test-file)))))
		     nil t)))
	    (when (eq (car rc) 'error)
	      (error "Remote error: %S" (cdr rc)))
	    (message "Uploaded test `%s'" test-name)))))))

(defun ats-list-tests-locally (dir)
  "Return a list of tests defined in DIR.
DIR ought to be the `test' directory in the Emacs repository or
a likewise structured directory tree."
  (let* ((default-directory (expand-file-name dir))
	 (start (length default-directory)))
    (let ((dirs (directory-files-recursively
		 dir "\\`[[:alnum:]-]+-tests\\.el\\'"
		 ;; Do not recurse into resource directories, as ERC's
		 ;; contain several files that resemble tests.
		 nil (lambda (dir-name)
		       (and (not (equal (file-name-nondirectory dir-name)
					"resources"))
			    (not (string-suffix-p "-resources" dir-name))))))
	  tests)
      (dolist (dir dirs)
	(let ((len (length dir)))
	  (push (substring dir start (- len 9)) tests)))
      (nreverse tests))))

(defun ats-list-tests (process)
  "Enumerate those tests which have already been uploaded to PROCESS.
Return a list of strings identifying tests which have been
uploaded to the remote device represented by PROCESS, as by
`ats-upload-tests', and which may be executed with
`ats-exec-tests'."
  (let ((rc (ats-eval
	     process
	     `(let* ((dir (concat (file-name-as-directory
				   temporary-file-directory)
				  "ats-tests"))
		     (len (length (file-name-as-directory dir)))
		     (default-directory dir)
		     (is-test-directory '(lambda (dir name)
					   (file-regular-p
					    (format "%s/%s-tests.el"
						    dir name)))))
		(let ((dirs
		       (directory-files-recursively
			dir "" t
			;; Do not iterate into directories that are tests of
			;; themselves, or their resources.
			(lambda (dir)
			  (let* ((name (file-name-nondirectory dir)))
			    (and (not (funcall is-test-directory name dir))
				 (not (equal name "resources"))
				 (not (string-suffix-p name "-resources")))))))
		      (tests nil))
		  (dolist (dir dirs)
		    (when (funcall is-test-directory
				   dir
				   (file-name-nondirectory dir))
		      (push (substring dir len) tests)))
		  (nreverse tests))))))
    (when (eq (car rc) 'error)
      (error "Remote error: %S" (cdr rc)))
    (cdr rc)))

(defun ats-run-test (process test &optional selector)
  "Run tests defined in a single test TEST on a remote device.
PROCESS represents the device on which to execute these tests.
SELECTOR is an ERT test selector, as with `ert-select-tests'.
\(You may upload tests beforehand by calling `ats-upload-test'.)
Display the output of the tests executed in a buffer."
  (interactive
   (let* ((connection
	   (ats-read-connection "Connection: "))
	  (test
	   (completing-read "Test to execute: "
			    (ats-list-tests connection)
			    nil t nil 'ats-tests-executed)))
     (list connection test)))
  ;; Attempt to byte-compile this test file.
  (let ((rc (ats-eval
	     process
	     `(progn
		(let* ((dir (concat (file-name-as-directory
				     temporary-file-directory)
				    "ats-tests/" ,test))
		       (name ,(file-name-nondirectory test))
		       (testfile (concat (file-name-as-directory dir)
					 name "-tests.el")))
		  (with-temp-buffer
		    (let ((value (byte-compile-file testfile))
			  (byte-compile-log-buffer (buffer-name)))
		      (cond ((eq value 'no-byte-compile)
			     testfile)
			    (value
			     (byte-compile-dest-file testfile))
			    (t (list (buffer-string))))))))))
	(device (cdr (assq 'device (process-get
				    process 'ats-connection-details))))
	file-name)
    (cond ((eq (car rc) 'error)
	   (error "Error during byte-compilation of `%s-tests.el': %S"
		  test (cdr rc)))
	  ((listp (cdr rc))
	   (error
	    "Encountered errors byte-compiling `%s-tests.el':\n%s"
	    test (cadr rc)))
	  (t (setq file-name (cdr rc))))
    ;; Delete all tests, load the byte-compiled test file, and execute
    ;; those tests just defined subject to SELECTOR.
    (with-current-buffer (get-buffer-create "*Test Output*")
      (goto-char (point-max))
      (insert (format "=== Executing %s on %s ===\n" test device))
      (redisplay)
      (setq rc (ats-eval process
			 `(progn
			    (require 'ert)
			    (ert-delete-all-tests)
			    (load ,file-name)
			    (with-selected-frame terminal-frame
			      (with-temp-buffer
				(let* ((temp-buffer (current-buffer))
				       (standard-output temp-buffer)
				       ;; Disable remote tests for the
				       ;; present...
				       (ert-remote-temporary-file-directory
					null-device)
				       (overriding-text-conversion-style nil)
				       (message-log-max t)
				       ;; It isn't possible for
				       ;; Vset_message_function to take
				       ;; effect when the initial frame
				       ;; is selected.
				       (messages-buffer-name
					(buffer-name temp-buffer)))
				  (let ((noninteractive t))
				    (ert-run-tests-batch ',selector)))
				(insert "=== Test execution complete ===\n")
				(buffer-substring-no-properties
				 (point-min) (point-max)))))))
      (cond ((eq (car rc) 'error)
	     (error "Error executing `%s-tests.el': %S" test (cdr rc)))
	    ((eq (car rc) 'exit)
	     (message "Backtrace:\n%s" (cdr rc))
	     (error "Remote Emacs exited inside `%s-tests.el'" test))
	    (t (progn
		 (goto-char (point-max))
		 (insert (cdr rc))
		 (pop-to-buffer (current-buffer))))))))

(defun ats-upload-all-tests (process dir)
  "Upload every Emacs test in DIR to the device represented by PROCESS.
Upload each and every test defined in DIR to the said device."
  (interactive
   (list (ats-read-connection "Connection: ")
	 (or ats-emacs-test-directory
	     (read-directory-name "Test base directory: "
				  nil nil t))))
  (let ((tests (ats-list-tests-locally dir)))
    (unless current-prefix-arg
      (dolist-with-progress-reporter (test tests)
	  "Uploading tests to device..."
	(ats-upload-test process dir test)))))

(defun ats-run-all-tests (process &optional selector)
  "Run every Emacs test uploaded to the device represented by PROCESS.
Execute every Emacs test that has been uploaded to PROCESS,
subject to SELECTOR, as in `ert-run-tests'."
  (interactive (list (ats-read-connection "Connection: ")
		     (and current-prefix-arg (read))))
  (let ((tests (ats-list-tests process)))
    (dolist-with-progress-reporter (test tests)
	"Running tests..."
      (condition-case err
	  (ats-run-test process test selector)
	(t (progn
	     (message "Error in executing `%s': %S" test err)))))))



(defun ats-cmd-error (format &rest args)
  "Print an error message FORMAT, formatted with ARGS, and exit."
  (apply #'message format args)
  (kill-emacs 1))

;; Batch mode text execution.
(defun ats-execute-tests-batch ()
  "Execute tests in batch mode, in the manner of `test/Makefile'.
Prompt for a device and execute tests on the same.  Save log
files to a directory specified by the user.
Call this function from the command line, with, for example:

  $ emacs --batch -l test-controller.el -f ats-execute-tests-batch

The following command-line arguments are also accepted:

  -h			Print help text.
  --device, -s DEVICE	Serial number of a device to which to connect.
  --user, -a UID	ID of the user as which to execute tests.
  --stub-file		Name of `stub.zip' wrapper required on Android <= 4.4.
  --test-dir		Directory in which Emacs's tests are situated.
  --output-dir, -o DIR	Name of a directory into which to save test logs.
  --no-upload		Don't upload tests; only run those which already exist.

In addition, these options exist to facilitate debugging the
automated testing process itself.

  --bisect COUNT	Skip COUNT tests from the beginning to investigate
			compatibility issues between tests."
  (let* ((ats-adb-host (getenv "ATS_ADB_HOST"))
	 (devices (ats-enumerate-devices
		   (lambda (name state _)
		     (and (equal state "device")
			  (ignore-errors
			    (ats-get-package-aid name "org.gnu.emacs"))))))
	 (cmd-device nil)
	 (cmd-user nil)
	 (cmd-output-dir nil)
	 (cmd-no-upload nil)
	 (bisect 0))
    ;; Read command-line arguments.
    (let (arg)
      (while (setq arg (pop argv))
	(cond ((equal arg "-f") (pop argv)) ;; Do nothing.  Emacs does
					    ;; not remove this from argv
					    ;; for unknown reasons.
	      ((equal arg "-h")
	       (message "Execute this file from the command line, with,\
 for example:

  $ emacs --batch -l test-controller.el -f ats-execute-tests-batch

The following command-line arguments are also accepted:

  --h			Print this help text.
  --device, -s DEVICE	Serial number of a device to which to connect.
  --user, -a UID	ID of the user as which to execute tests.
  --stub-file		Name of `stub.zip' wrapper required on Android <= 4.4.
  --test-dir		Directory in which Emacs's tests are situated.
  --output-dir, -o DIR	Name of a directory into which to save test logs.
  --no-upload		Don't upload tests; only run those which already exist.

In addition, these options exist to facilitate debugging the
automated testing process itself.

  --bisect COUNT	Skip COUNT tests from the beginning to investigate
			compatibility issues between tests.")
	       (kill-emacs 0))
	      ((or (equal arg "-s") (equal arg "--device"))
	       (setq cmd-device
		     (or (pop argv)
			 (ats-cmd-error
			  "Expected argument to `--device' option"))))
	      ((or (equal arg "-a") (equal arg "--user"))
	       (setq cmd-user
		     (or (pop argv)
			 (ats-cmd-error
			  "Expected argument to `--user' option"))))
	      ((or (equal arg "-o") (equal arg "--output-dir"))
	       (setq cmd-output-dir
		     (or (pop argv)
			 (ats-cmd-error
			  "Expected argument to `--output-dir' option"))))
	      ((equal arg "--stub-file")
	       (setq ats-working-stub-file
		     (or (pop argv)
			 (ats-cmd-error
			  "Expected argument to `--stub-file' option."))))
	      ((equal arg "--test-dir")
	       (setq ats-emacs-test-directory
		     (or (pop argv)
			 (ats-cmd-error
			  "Expected argument to `--test-dir' option."))))
	      ((equal arg "--no-upload")
	       (setq cmd-no-upload t))
	      ((equal arg "--bisect")
	       (let ((value (or (pop argv)
				(ats-cmd-error
				 "Expected argument to `--bisect' option."))))
		 (setq bisect (progn
				(unless (string-match-p
					 "\\`[[:digit:]]+\\'" value)
				  (ats-cmd-error
				   "Invalid value for `--bisect' option: `%s'"
				   value))
				(string-to-number value)))))
	      (t (ats-cmd-error "Unknown command line argument `%s'" arg)))))
    ;; Validate and apply command-line arguments or prompt the user for
    ;; parameters in their absence.
    (if cmd-device
	(unless (member cmd-device (mapcar #'car devices))
	  (ats-cmd-error
	   "Device `%s' does not exist or has no installation of Emacs"
	   cmd-device))
      (message "These devices are presently available for test execution:")
      (let ((nth 0))
	(dolist (device devices)
	  (message "%2d. %-24s(API level %d, %s)"
		   (setq nth (1+ nth)) (car device)
		   (ats-get-sdk-version (car device))
		   (ats-getprop (car device) "ro.product.cpu.abi")))))
    (let* ((number (and (not cmd-device)
			(string-to-number
			 (read-string
			  "Select a device by typing its number, and Return: "))))
	   (device (or cmd-device
		       (if (or (< number 1) (> number (length devices)))
			   (ats-cmd-error "Invalid selection: %s" number)
			 (car (nth (1- number) devices)))))
	   (users (ats-list-users device))
	   (nth 0)
	   (user nil))
      (if cmd-user
	  (progn
	    (let ((valid-number
		   (string-match-p "\\`[[:digit:]]+\\'" cmd-user))
		  (uid (string-to-number cmd-user)))
	      (unless valid-number
		(ats-cmd-error "Invalid value for `--user' argument: `'%s'"
			    cmd-user))
	      (unless (assq uid users)
		(ats-cmd-error "No such user exists: %d" uid))
	      ;; Don't prompt the user afterwards.
	      (setq user uid)))
	(dolist (user users)
	  (message "%2d. %s (id=%d)" (setq nth (1+ nth))
		   (cadr user) (car user)))
	(setq number (string-to-number
		      (read-string
		       "As which user should tests be executed? ")))
	(when (or (< number 1) (> number (length users)))
	  (ats-cmd-error "Invalid selection: %s" number)))
      (let* ((user (or user (car (nth (1- number) users))))
	     (connection (ats-connect device user)))
	(unless cmd-no-upload
	  (ats-upload-all-tests
	   connection
	   (or ats-emacs-test-directory
	       (read-directory-name "Test base directory: "
				    nil nil t))))
	(let ((output-directory
	       (or cmd-output-dir
		   (read-directory-name
		    "Where to save test log files? "))))
	  (mkdir output-directory t)
	  (let* ((tests (ats-list-tests connection))
		 (start (nthcdr bisect tests)))
	    (dolist (test start)
	      (message "Generating `%s/%s-test.log'"
		       output-directory test)
	      (ats-run-test connection test)
	      (let ((output-file
		     (concat (file-name-as-directory
			      output-directory)
			     test "-test.log")))
		(mkdir (file-name-directory output-file) t)
		(with-current-buffer "*Test Output*"
		  (write-region (point-min) (point-max)
				(concat (file-name-as-directory
					 output-directory)
					test "-test.log"))
		  (erase-buffer))))))))))

(provide 'test-controller)

;;; test-controller.el ends here

;; Local Variables:
;; emacs-lisp-docstring-fill-column: 64
;; indent-tabs-mode: t
;; End:
