;;; w32-fns.el --- Lisp routines for Windows NT

;; Copyright (C) 1994, 2001 Free Software Foundation, Inc.

;; Author: Geoff Voelker <voelker@cs.washington.edu>
;; Keywords: internal

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; (August 12, 1993)
;; Created.

;; (November 21, 1994)
;; [C-M-backspace] defined.
;; mode-line-format defined to show buffer file type.
;; audio bell initialized.

;;; Code:

;; Map delete and backspace
(define-key function-key-map [backspace] "\177")
(define-key function-key-map [delete] "\C-d")
(define-key function-key-map [M-backspace] [?\M-\177])
(define-key function-key-map [C-M-backspace] [\C-\M-delete])

;; Ignore case on file-name completion
(setq completion-ignore-case t)

;; Map all versions of a filename (8.3, longname, mixed case) to the
;; same buffer.
(setq find-file-visit-truename t)

(defun w32-version ()
  "Return the MS-Windows version numbers.
The value is a list of three integers: the major and minor version
numbers, and the build number."
  (x-server-version))

(defun w32-using-nt ()
  "Return non-nil if literally running on Windows NT (i.e., not Windows 9X)."
  (and (eq system-type 'windows-nt) (getenv "SystemRoot")))

(defun w32-shell-name ()
  "Return the name of the shell being used."
  (or (and (boundp 'explicit-shell-file-name) explicit-shell-file-name)
      (getenv "ESHELL")
      (getenv "SHELL")
      (and (w32-using-nt) "cmd.exe")
      "command.com"))

(defun w32-system-shell-p (shell-name)
  (and shell-name
       (member (downcase (file-name-nondirectory shell-name))
	       w32-system-shells)))

(defun w32-shell-dos-semantics ()
  "Return t if the interactive shell being used expects msdos shell semantics."
  (or (w32-system-shell-p (w32-shell-name))
      (and (member (downcase (file-name-nondirectory (w32-shell-name)))
		   '("cmdproxy" "cmdproxy.exe"))
	   (w32-system-shell-p (getenv "COMSPEC")))))

(defun w32-check-shell-configuration ()
  "Check the configuration of shell variables on Windows NT/9X.
This function is invoked after loading the init files and processing
the command line arguments.  It issues a warning if the user or site
has configured the shell with inappropriate settings."
  (interactive)
  (let ((prev-buffer (current-buffer))
	(buffer (get-buffer-create "*Shell Configuration*"))
	(system-shell))
    (set-buffer buffer)
    (erase-buffer)
    (if (w32-system-shell-p (getenv "ESHELL"))
	(insert (format "Warning! The ESHELL environment variable uses %s.
You probably want to change it so that it uses cmdproxy.exe instead.\n\n"
			(getenv "ESHELL"))))
    (if (w32-system-shell-p (getenv "SHELL"))
	(insert (format "Warning! The SHELL environment variable uses %s.
You probably want to change it so that it uses cmdproxy.exe instead.\n\n"
			(getenv "SHELL"))))
    (if (w32-system-shell-p shell-file-name)
	(insert (format "Warning! shell-file-name uses %s.
You probably want to change it so that it uses cmdproxy.exe instead.\n\n"
			shell-file-name)))
    (if (and (boundp 'explicit-shell-file-name)
	     (w32-system-shell-p explicit-shell-file-name))
	(insert (format "Warning! explicit-shell-file-name uses %s.
You probably want to change it so that it uses cmdproxy.exe instead.\n\n"
			explicit-shell-file-name)))
    (setq system-shell (> (buffer-size) 0))

    ;; Allow user to specify that they really do want to use one of the
    ;; "system" shells, despite the drawbacks, but still warn if
    ;; shell-command-switch doesn't match.
    (if w32-allow-system-shell
	(erase-buffer))

    (cond (system-shell
	   ;; System shells.
	   (if (string-equal "-c" shell-command-switch)
	       (insert "Warning! shell-command-switch is \"-c\".
You should set this to \"/c\" when using a system shell.\n\n"))
	   (if w32-quote-process-args
	       (insert "Warning! w32-quote-process-args is t.
You should set this to nil when using a system shell.\n\n")))
	  ;; Non-system shells.
	  (t
	   (if (string-equal "/c" shell-command-switch)
	       (insert "Warning! shell-command-switch is \"/c\".
You should set this to \"-c\" when using a non-system shell.\n\n"))
	   (if (not w32-quote-process-args)
	       (insert "Warning! w32-quote-process-args is nil.
You should set this to t when using a non-system shell.\n\n"))))
    (if (> (buffer-size) 0)
	(display-buffer buffer)
      (kill-buffer buffer))
    (set-buffer prev-buffer)))

(add-hook 'after-init-hook 'w32-check-shell-configuration)

;;; Override setting chosen at startup.
(defun set-default-process-coding-system ()
  ;; Most programs on Windows will accept Unix line endings on input
  ;; (and some programs ported from Unix require it) but most will
  ;; produce DOS line endings on output.
  (setq default-process-coding-system
	(if default-enable-multibyte-characters
	    '(undecided-dos . undecided-unix)
	  '(raw-text-dos . raw-text-unix)))
  (or (w32-using-nt)
      ;; On Windows 9x, make cmdproxy default to using DOS line endings
      ;; for input, because command.com requires this.
      (setq process-coding-system-alist
	    `(("[cC][mM][dD][pP][rR][oO][xX][yY]"
	       . ,(if default-enable-multibyte-characters
		      '(undecided-dos . undecided-dos)
		    '(raw-text-dos . raw-text-dos)))))))

(add-hook 'before-init-hook 'set-default-process-coding-system)


;;; Basic support functions for managing Emacs' locale setting

(defvar w32-valid-locales nil
  "List of locale ids known to be supported.")

;;; This is the brute-force version; an efficient version is now
;;; built-in though.
(if (not (fboundp 'w32-get-valid-locale-ids))
    (defun w32-get-valid-locale-ids ()
      "Return list of all valid Windows locale ids."
      (let ((i 65535)
	    locales)
	(while (> i 0)
	  (if (w32-get-locale-info i)
	      (setq locales (cons i locales)))
	  (setq i (1- i)))
	locales)))

(defun w32-list-locales ()
  "List the name and id of all locales supported by Windows."
  (interactive)
  (if (null w32-valid-locales)
      (setq w32-valid-locales (w32-get-valid-locale-ids)))
  (switch-to-buffer-other-window (get-buffer-create "*Supported Locales*"))
  (erase-buffer)
  (insert "LCID\tAbbrev\tFull name\n\n")
  (insert (mapconcat
	   '(lambda (x)
	      (format "%d\t%s\t%s"
		      x
		      (w32-get-locale-info x)
		      (w32-get-locale-info x t)))
	   w32-valid-locales "\n"))
  (insert "\n")
  (goto-char (point-min)))


;;; Setup Info-default-directory-list to include the info directory
;;; near where Emacs executable was installed.  We used to set INFOPATH,
;;; but when this is set Info-default-directory-list is ignored.  We
;;; also cannot rely upon what is set in paths.el because they assume
;;; that configuration during build time is correct for runtime.
(defun w32-init-info ()
  (let* ((instdir (file-name-directory invocation-directory))
	 (dir1 (expand-file-name "../info/" instdir))
	 (dir2 (expand-file-name "../../../info/" instdir)))
    (if (file-exists-p dir1)
	(setq Info-default-directory-list
	      (append Info-default-directory-list (list dir1)))
      (if (file-exists-p dir2)
	  (setq Info-default-directory-list
		(append Info-default-directory-list (list dir2)))))))

(add-hook 'before-init-hook 'w32-init-info)

;;; The variable source-directory is used to initialize Info-directory-list.
;;; However, the common case is that Emacs is being used from a binary
;;; distribution, and the value of source-directory is meaningless in that
;;; case.  Even worse, source-directory can refer to a directory on a drive
;;; on the build machine that happens to be a removable drive on the user's
;;; machine.  When this happens, Emacs tries to access the removable drive
;;; and produces the abort/retry/ignore dialog.  Since we do not use
;;; source-directory, set it to something that is a reasonable approximation
;;; on the user's machine.

;(add-hook 'before-init-hook
;	  '(lambda ()
;	     (setq source-directory (file-name-as-directory
;				     (expand-file-name ".." exec-directory)))))

(defun convert-standard-filename (filename)
  "Convert a standard file's name to something suitable for the current OS.
This function's standard definition is trivial; it just returns the argument.
However, on some systems, the function is redefined
with a definition that really does change some file names."
  (let ((name
         (save-match-data
           (if (string-match "\\`/cygdrive/\\([a-zA-Z]\\)/" filename)
               (replace-match "\\1:/" t nil filename)
             (copy-sequence filename))))
	(start 0))
    ;; leave ':' if part of drive specifier
    (if (and (> (length name) 1)
	     (eq (aref name 1) ?:))
	(setq start 2))
    ;; destructively replace invalid filename characters with !
    (while (string-match "[?*:<>|\"\000-\037]" name start)
      (aset name (match-beginning 0) ?!)
      (setq start (match-end 0)))
    ;; convert directory separators to Windows format
    ;; (but only if the shell in use requires it)
    (when (w32-shell-dos-semantics)
      (setq start 0)
      (while (string-match "/" name start)
	(aset name (match-beginning 0) ?\\)
	(setq start (match-end 0))))
    name))

;;; Fix interface to (X-specific) mouse.el
(defun x-set-selection (type data)
  (or type (setq type 'PRIMARY))
  (put 'x-selections type data))

(defun x-get-selection (&optional type data-type)
  (or type (setq type 'PRIMARY))
  (get 'x-selections type))

(defun set-w32-system-coding-system (coding-system)
  "Set the coding system used by the Windows System to CODING-SYSTEM.
This is used for things like passing font names with non-ASCII
characters in them to the system. For a list of possible values of
CODING-SYSTEM, use \\[list-coding-systems].

This function is provided for backward compatibility, since
w32-system-coding-system is now an alias for `locale-coding-system'."
  (interactive
   (list (let ((default locale-coding-system))
           (read-coding-system
            (format "Coding system for system calls (default, %s): "
                    default)
            default))))
  (check-coding-system coding-system)
  (setq locale-coding-system coding-system))

;; locale-coding-system was introduced to do the same thing as
;; w32-system-coding-system. Use that instead.
(defvaralias 'w32-system-coding-system 'locale-coding-system)

;;; Set to a system sound if you want a fancy bell.
(set-message-beep nil)

;;; The "Windows" keys on newer keyboards bring up the Start menu
;;; whether you want it or not - make Emacs ignore these keystrokes
;;; rather than beep.
(global-set-key [lwindow] 'ignore)
(global-set-key [rwindow] 'ignore)

;; Map certain keypad keys into ASCII characters
;; that people usually expect.
(define-key function-key-map [tab] [?\t])
(define-key function-key-map [linefeed] [?\n])
(define-key function-key-map [clear] [11])
(define-key function-key-map [return] [13])
(define-key function-key-map [escape] [?\e])
(define-key function-key-map [M-tab] [?\M-\t])
(define-key function-key-map [M-linefeed] [?\M-\n])
(define-key function-key-map [M-clear] [?\M-\013])
(define-key function-key-map [M-return] [?\M-\015])
(define-key function-key-map [M-escape] [?\M-\e])

;; These don't do the right thing (voelker)
;(define-key function-key-map [backspace] [127])
;(define-key function-key-map [delete] [127])
;(define-key function-key-map [M-backspace] [?\M-\d])
;(define-key function-key-map [M-delete] [?\M-\d])

;; These tell read-char how to convert
;; these special chars to ASCII.
(put 'tab 'ascii-character ?\t)
(put 'linefeed 'ascii-character ?\n)
(put 'clear 'ascii-character 12)
(put 'return 'ascii-character 13)
(put 'escape 'ascii-character ?\e)
(put 'backspace 'ascii-character 127)
(put 'delete 'ascii-character 127)

;; W32 uses different color indexes than standard:

(defvar w32-tty-standard-colors
  '(("black"          0     0     0     0)
    ("blue"           1     0     0 52480) ; MediumBlue
    ("green"          2  8704 35584  8704) ; ForestGreen
    ("cyan"           3     0 52736 53504) ; DarkTurquoise
    ("red"            4 45568  8704  8704) ; FireBrick
    ("magenta"        5 35584     0 35584) ; DarkMagenta
    ("brown"          6 40960 20992 11520) ; Sienna
    ("lightgray"      7 48640 48640 48640) ; Gray
    ("darkgray"       8 26112 26112 26112) ; Gray40
    ("lightblue"      9     0     0 65535) ; Blue
    ("lightgreen"    10     0 65535     0) ; Green
    ("lightcyan"     11     0 65535 65535) ; Cyan
    ("lightred"      12 65535     0     0) ; Red
    ("lightmagenta"  13 65535     0 65535) ; Magenta
    ("yellow"        14 65535 65535     0) ; Yellow
    ("white"         15 65535 65535 65535))
"A list of VGA console colors, their indices and 16-bit RGB values.")


(defun w32-add-charset-info (xlfd-charset windows-charset codepage)
  "Function to add character sets to display with Windows fonts.
Creates entries in `w32-charset-info-alist'.
XLFD-CHARSET is a string which will appear in the XLFD font name to
identify the character set. WINDOWS-CHARSET is a symbol identifying
the Windows character set this maps to. For the list of possible
values, see the documentation for `w32-charset-info-alist'. CODEPAGE
can be a numeric codepage that Windows uses to display the character
set, t for Unicode output with no codepage translation or nil for 8
bit output with no translation."
  (add-to-list 'w32-charset-info-alist
               (cons xlfd-charset (cons windows-charset codepage)))
  )

(w32-add-charset-info "iso8859-1" 'w32-charset-ansi 1252)
(w32-add-charset-info "iso8859-14" 'w32-charset-ansi  28604)
(w32-add-charset-info "iso8859-15" 'w32-charset-ansi  28605)
(w32-add-charset-info "jisx0208-sjis" 'w32-charset-shiftjis 932)
(w32-add-charset-info "jisx0201-latin" 'w32-charset-shiftjis 932)
(w32-add-charset-info "jisx0201-katakana" 'w32-charset-shiftjis 932)
(w32-add-charset-info "ksc5601.1987" 'w32-charset-hangeul 949)
(w32-add-charset-info "big5" 'w32-charset-chinesebig5 950)
(w32-add-charset-info "gb2312" 'w32-charset-gb2312 936)
(w32-add-charset-info "ms-symbol" 'w32-charset-symbol nil)
(w32-add-charset-info "ms-oem" 'w32-charset-oem 437)
(w32-add-charset-info "ms-oemlatin" 'w32-charset-oem 850)
(if (boundp 'w32-extra-charsets-defined)
    (progn
      (w32-add-charset-info "iso8859-2" 'w32-charset-easteurope 28592)
      (w32-add-charset-info "iso8859-3" 'w32-charset-turkish 28593)
      (w32-add-charset-info "iso8859-4" 'w32-charset-baltic 28594)
      (w32-add-charset-info "iso8859-5" 'w32-charset-russian 28595)
      (w32-add-charset-info "iso8859-6" 'w32-charset-arabic 28596)
      (w32-add-charset-info "iso8859-7" 'w32-charset-greek 28597)
      (w32-add-charset-info "iso8859-8" 'w32-charset-hebrew 1255)
      (w32-add-charset-info "iso8859-9" 'w32-charset-turkish 1254)
      (w32-add-charset-info "iso8859-13" 'w32-charset-baltic 1257)
      (w32-add-charset-info "koi8-r" 'w32-charset-russian 20866)
      (w32-add-charset-info "tis620" 'w32-charset-thai 874)
      (w32-add-charset-info "ksc5601.1992" 'w32-charset-johab 1361)
      (w32-add-charset-info "mac" 'w32-charset-mac nil)))
(if (boundp 'w32-unicode-charset-defined)
    (progn
      (w32-add-charset-info "iso10646-1" 'w32-charset-unicode t)
      (w32-add-charset-info "unicode" 'w32-charset-unicode t))
  ;; If unicode windows charset is not defined, use ansi fonts.
  (w32-add-charset-info "iso10646-1" 'w32-charset-ansi t))

(make-obsolete-variable 'w32-enable-italics
                        'w32-enable-synthesized-fonts "21.1")
(make-obsolete-variable 'w32-charset-to-codepage-alist
                        'w32-charset-info-alist "21.1")


;;;; Selections and cut buffers

;;; We keep track of the last text selected here, so we can check the
;;; current selection against it, and avoid passing back our own text
;;; from x-cut-buffer-or-selection-value.
(defvar x-last-selected-text nil)

;;; It is said that overlarge strings are slow to put into the cut buffer.
;;; Note this value is overridden below.
(defvar x-cut-buffer-max 20000
  "Max number of characters to put in the cut buffer.")

(defun x-select-text (text &optional push)
  "Make TEXT the last selected text.
If `x-select-enable-clipboard' is non-nil, copy the text to the system
clipboard as well. Optional PUSH is ignored on Windows."
  (if x-select-enable-clipboard
      (w32-set-clipboard-data text))
  (setq x-last-selected-text text))

(defun x-get-selection-value ()
  "Return the value of the current selection.
Consult the selection, then the cut buffer.  Treat empty strings as if
they were unset."
  (if x-select-enable-clipboard
      (let (text)
	;; Don't die if x-get-selection signals an error.
	(condition-case c
	    (setq text (w32-get-clipboard-data))
	  (error (message "w32-get-clipboard-data:%s" c)))
	(if (string= text "") (setq text nil))
	(cond
	 ((not text) nil)
	 ((eq text x-last-selected-text) nil)
	 ((string= text x-last-selected-text)
	  ;; Record the newer string, so subsequent calls can use the 'eq' test.
	  (setq x-last-selected-text text)
	  nil)
	 (t
	  (setq x-last-selected-text text))))))

(defalias 'x-cut-buffer-or-selection-value 'x-get-selection-value)

;;; Arrange for the kill and yank functions to set and check the clipboard.
(setq interprogram-cut-function 'x-select-text)
(setq interprogram-paste-function 'x-get-selection-value)


;;; w32-fns.el ends here
