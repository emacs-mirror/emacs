;;; browse-url.el --- Pass a URL to a WWW browser

;; Copyright 1995, 1996, 1997 Free Software Foundation, Inc.

;; Author: Denis Howe <dbh@doc.ic.ac.uk>
;; Maintainer: Dave Love <d.love@dl.ac.uk>
;; Created: 03 Apr 1995
;; Keywords: hypertext, hypermedia, mouse
;; X-Home page: http://wombat.doc.ic.ac.uk/

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

;; The latest version of this package should be available from
;; <URL:http://wombat.doc.ic.ac.uk/emacs/browse-url.el>.

;; This package provides functions which read a URL (Uniform Resource
;; Locator) from the minibuffer, defaulting to the URL around point,
;; and ask a World-Wide Web browser to load it.  It can also load the
;; URL associated with the current buffer.  Different browsers use
;; different methods of remote control so there is one function for
;; each supported browser.  If the chosen browser is not running, it
;; is started.  Currently there is support for:

;; Function              Browser     Earliest version
;; browse-url-netscape   Netscape    1.1b1	   
;; browse-url-mosaic     XMosaic     <= 2.4
;; browse-url-cci        XMosaic     2.5
;; browse-url-w3         w3          0
;; browse-url-w3-gnudoit w3 remotely
;; browse-url-iximosaic  IXI Mosaic  ?
;; browse-url-lynx-*	 Lynx	     0
;; browse-url-grail      Grail       0.3b1
;; browse-url-mmm        MMM         ?
;; browse-url-generic    arbitrary

;; Note that versions of Netscape before 1.1b1 did not have remote
;; control.  <URL:http://www.netscape.com/newsref/std/x-remote.html>.

;; Netscape can cache Web pages so it may be necessary to tell it to
;; reload the current page if it has changed (e.g. if you have edited
;; it).  There is currently no perfect automatic solution to this.

;; Netscape allows you to specify the id of the window you want to
;; control but which window DO you want to control and how do you
;; discover its id?

;; If using XMosaic before version 2.5, check the definition of
;; browse-url-usr1-signal below.
;; <URL:http://www.ncsa.uiuc.edu/SDG/Software/XMosaic/remote-control.html>

;; XMosaic version 2.5 introduced Common Client Interface allowing you
;; to control mosaic through Unix sockets.
;; <URL:http://www.ncsa.uiuc.edu/SDG/Software/XMosaic/CCI/cci-spec.html>

;; William M. Perry's excellent "w3" WWW browser for
;; Emacs <URL:ftp://cs.indiana.edu/pub/elisp/w3/>
;; has a function w3-follow-url-at-point, but that
;; doesn't let you edit the URL like browse-url.
;; The `gnuserv' package that can be used to control it in another
;; Emacs process is available from
;; <URL:http://hplbwww.hpl.hp.com/people/ange/gnuserv/>.

;; Grail is the freely available WWW browser implemented in Python, a
;; cool object-oriented freely available interpreted language.  Grail
;; 0.3b1 was the first version to have remote control as distributed.
;; For more information on Grail see
;; <URL:http://grail.cnri.reston.va.us/> and for more information on
;; Python see <url:http://www.python.org/>.  Grail support in
;; browse-url.el written by Barry Warsaw <bwarsaw@python.org>.

;; MMM is the freely available WWW browser implemented in Caml Special
;; Light, a cool impure functional programming language, by Francois
;; Rouaix.  See the MMM home page
;; <URL:http://pauillac.inria.fr/%7Erouaix/mmm/>.

;; Lynx is now distributed by the FSF.  See also
;; <URL:http://lynx.browser.org/>.

;; Free graphical browsers that could be used by `browse-url-generic'
;; include Chimera <URL:ftp://ftp.cs.unlv.edu/pub/chimera> and
;; <URL:http://www.unlv.edu/chimera/>, Arena
;; <URL:ftp://ftp.yggdrasil.com/pub/dist/web/arena>, Amaya
;; <URL:ftp://ftp.w3.org/pub/amaya>, mMosaic
;; <URL:ftp://sig.enst.fr/pub/multicast/mMosaic/> (the latter with
;; development support for Java applets).

;; I [Denis Howe] recommend Nelson Minar <nelson@santafe.edu>'s excellent
;; html-helper-mode.el for editing HTML and thank Nelson for
;; his many useful comments on this code.
;; <URL:http://www.santafe.edu/%7Enelson/hhm-beta/>

;; See also hm--html-menus <URL:http://www.tnt.uni-hannover.de/%7Emuenkel/
;; software/own/hm--html-menus/>.  For composing correct HTML see also
;; PSGML the general SGML structure editor package
;; <URL:ftp://ftp.lysator.liu.se/pub/sgml>; hm--html-menus can be used
;; with this.

;; This package generalises function html-previewer-process in Marc
;; Andreessen <marca@ncsa.uiuc.edu>'s html-mode (LCD
;; modes/html-mode.el.Z) and provides better versions of the URL
;; functions in Michelangelo Grigni <mic@cs.ucsd.edu>'s ffap.el
;; (find-file-at-point) <URL:ftp://cs.ucsd.edu:/pub/mic/>.  The huge
;; hyperbole package also contains similar functions.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Help!

;; Can you write and test some code for the Macintrash and Windoze
;; Netscape remote control APIs?  (See the URL above).

;; Do any other browsers have remote control?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Usage

;; To display the URL at or before point:
;; M-x browse-url-at-point RET
;; or, similarly but with the opportunity to edit the URL extracted from
;; the buffer, use:
;; M-x browse-url

;; To display a URL by shift-clicking on it, put this in your ~/.emacs
;; file:
;;      (global-set-key [S-mouse-2] 'browse-url-at-mouse)
;; (Note that using Shift-mouse-1 is not desirable because
;; that event has a standard meaning in Emacs.)

;; To display the current buffer in a web browser:
;; M-x browse-url-of-buffer RET

;; To display the current region in a web browser:
;; M-x browse-url-of-region RET

;; In Dired, to display the file named on the current line:
;; M-x browse-url-of-dired-file RET

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customisation (~/.emacs)

;; To see what variables are available for customization, type
;; `M-x set-variable browse-url TAB'.

;; Bind the browse-url commands to keys with the `C-c C-z' prefix
;; (as used by html-helper-mode):
;;	(global-set-key "\C-c\C-z." 'browse-url-at-point)
;;	(global-set-key "\C-c\C-zb" 'browse-url-of-buffer)
;;	(global-set-key "\C-c\C-zr" 'browse-url-of-region)
;;	(global-set-key "\C-c\C-zu" 'browse-url)
;;	(global-set-key "\C-c\C-zv" 'browse-url-of-file)
;;	(add-hook 'dired-mode-hook
;;		  (function (lambda ()
;;			      (local-set-key "\C-c\C-zf" 'browse-url-of-dired-file))))

;; Browse URLs in mail messages by clicking mouse-2:
;;	(add-hook 'rmail-mode-hook (function (lambda () ; rmail-mode startup
;;	  (define-key rmail-mode-map [mouse-2] 'browse-url-at-mouse))))

;; Browse URLs in Usenet messages by clicking mouse-2:
;;	(eval-after-load "gnus"
;;	  '(define-key gnus-article-mode-map [mouse-2] 'browse-url-at-mouse))
;; [The current version of Gnus provides a standard feature to
;; activate URLs in article buffers for invocation of browse-url with
;; mouse-2.]

;; Use the Emacs w3 browser when not running under X11:
;;	(or (eq window-system 'x)
;;	    (setq browse-url-browser-function 'browse-url-w3))

;; To always save modified buffers before displaying the file in a browser:
;;	(setq browse-url-save-file t)

;; To get round the Netscape caching problem, you could EITHER have
;; write-file in html-helper-mode make Netscape reload the document:
;;
;;	(autoload 'browse-url-netscape-reload "browse-url"
;;	  "Ask a WWW browser to redisplay the current file." t)
;;	(add-hook 'html-helper-mode-hook
;;		  (function (lambda ()
;;		     (add-hook 'local-write-file-hooks
;;			       (function (lambda ()
;;				  (let ((local-write-file-hooks))
;;				    (save-buffer))
;;				  (browse-url-netscape-reload)
;;				  t))			; => file written by hook
;;			       t))))			; append to l-w-f-hooks
;;
;; OR have browse-url-of-file ask Netscape to load and then reload the
;; file:
;;
;;	(add-hook 'browse-url-of-file-hook 'browse-url-netscape-reload)

;; You may also want to customise browse-url-netscape-arguments, e.g.
;;	(setq browse-url-netscape-arguments '("-install"))
;;
;; or similarly for the other browsers.

;; To invoke different browsers for different URLs:
;;      (setq browse-url-browser-function '(("^mailto:" . browse-url-mail)
;;      				    ("." . browse-url-netscape)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

(eval-when-compile (require 'dired)
		   (require 'thingatpt))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables

(defgroup browse-url nil
  "Use a web browser to look at a URL."
  :group 'hypermedia)

;;;###autoload
(defcustom browse-url-browser-function
  'browse-url-netscape
  "*Function to display the current buffer in a WWW browser.
This is used by the `browse-url-at-point', `browse-url-at-mouse', and
`browse-url-of-file' commands.

If the value is not a function it should be a list of pairs
(REGEXP.FUNCTION).  In this case the function called will be the one
associated with the first REGEXP which matches the current URL.  The
function is passed the URL and any other args of `browse-url'.  The last
regexp should probably be \".\" to specify a default browser."
  :type 'function
  :group 'browse-url)

(defcustom browse-url-netscape-program "netscape"
  "*The name by which to invoke Netscape."
  :type 'string
  :group 'browse-url)

(defcustom browse-url-netscape-arguments nil
  "*A list of strings to pass to Netscape as arguments."
  :type '(repeat (string :tag "Argument"))
  :group 'browse-url)

(defcustom browse-url-netscape-startup-arguments browse-url-netscape-arguments
  "*A list of strings to pass to Netscape when it starts up.
Defaults to the value of `browse-url-netscape-arguments' at the time
`browse-url' is loaded."
  :type '(repeat (string :tag "Argument"))
  :group 'browse-url)

(defcustom browse-url-new-window-p nil
  "*If non-nil, always open a new browser window.
Passing an interactive argument to \\[browse-url-netscape] or
\\[browse-url-cci] reverses the effect of this variable.  Requires
Netscape version 1.1N or later or XMosaic version 2.5 or later."
  :type 'boolean
  :group 'browse-url)

(defcustom browse-url-netscape-display nil
  "*The X display for running Netscape, if not same as Emacs's."
  :type 'string
  :group 'browse-url)

(defcustom browse-url-mosaic-arguments nil
  "*A list of strings to pass to Mosaic as arguments."
  :type '(repeat (string :tag "Argument"))
  :group 'browse-url)

(defvar browse-url-path-regexp
  "[^]\t\n \"'()<>[^`{}]*[^]\t\n \"'()<>[^`{}.,;]+"
  "A regular expression to match the host, path or e-mail part of a URL.")

(defvar browse-url-short-regexp
  (concat "[-A-Za-z0-9.]+" browse-url-path-regexp)
  "A regular expression probably matching a URL without an access scheme.
Hostname matching is stricter in this case than for
``browse-url-regexp''.")

(defvar browse-url-regexp
  (concat
   "\\(https?://\\|ftp://\\|gopher://\\|telnet://\\|wais://\\|file:/\\|s?news:\\|mailto:\\)"
   browse-url-path-regexp)
  "A regular expression probably matching a complete URL.")

(defvar browse-url-markedup-regexp
  "<URL:[^>]+>"
  "A regular expression matching a URL marked up per RFC1738.
This may be broken across lines.")

(defvar browse-url-filename-alist
  '(("^/+" . "file:/"))
  "An alist of (REGEXP . STRING) pairs.
Any substring of a filename matching one of the REGEXPs is replaced by
the corresponding STRING.  All pairs are applied in the order given.
The default value prepends `file:' to any path beginning with `/'.
Used by the `browse-url-of-file' command.

For example, to map EFS filenames to URLs:

    (setq browse-url-filename-alist
	  '((\"/webmaster@webserver:/home/www/html/\" .
	     \"http://www.acme.co.uk/\")
	    (\"^/+\" . \"file:/\")))
")

(defvar browse-url-save-file nil
  "If non-nil, save the buffer before displaying its file.
Used by the `browse-url-of-file' command.")

(defvar browse-url-of-file-hook nil
  "A hook to be run with run-hook after `browse-url-of-file' has asked
a browser to load a file.

Set this to `browse-url-netscape-reload' to force Netscape to load the
file rather than displaying a cached copy.")

(defvar browse-url-usr1-signal
  (if (and (boundp 'emacs-major-version)
	   (or (> emacs-major-version 19) (>= emacs-minor-version 29)))
      'SIGUSR1 ; Why did I think this was in lower case before?
    30)					; Check /usr/include/signal.h.
  "The argument to `signal-process' for sending SIGUSR1 to XMosaic.
Emacs 19.29 accepts 'SIGUSR1, earlier versions require an integer
which is 30 on SunOS and 16 on HP-UX and Solaris.")

(defvar browse-url-CCI-port 3003
  "Port to access XMosaic via CCI.
This can be any number between 1024 and 65535 but must correspond to
the value set in the browser.")

(defvar browse-url-CCI-host "localhost"
  "*Host to access XMosaic via CCI.
This should be the host name of the machine running XMosaic with CCI
enabled.  The port number should be set in `browse-url-CCI-port'.")

(defvar browse-url-temp-file-name nil)
(make-variable-buffer-local 'browse-url-temp-file-name)

(defcustom browse-url-xterm-program "xterm"
  "*The name of the terminal emulator used by `browse-url-lynx-xterm'.
This might, for instance, be a separate colour version of xterm."
  :type 'string
  :group 'browse-url)

(defcustom browse-url-xterm-args nil
  "*A list of strings defining options for `browse-url-xterm-program'.
These might set its size, for instance."
  :type '(repeat (string :tag "Argument"))
  :group 'browse-url)

(defcustom browse-url-gnudoit-program "gnudoit"
  "*The name of the `gnudoit' program used by `browse-url-w3-gnudoit'."
  :type 'string
  :group 'browse-url)

(defcustom browse-url-gnudoit-args '("-q")
  "*A list of strings defining options for `browse-url-gnudoit-program'.
These might set the port, for instance."
  :type '(repeat (string :tag "Argument"))
  :group 'browse-url)

(defcustom browse-url-generic-program nil
  "*The name of the browser program used by `browse-url-generic'."
  :type 'string
  :group 'browse-url)

(defcustom browse-url-generic-args nil
  "*A list of strings defining options for `browse-url-generic-program'."
  :type '(repeat (string :tag "Argument"))
  :group 'browse-url)

(defcustom browse-url-temp-dir
  (or (getenv "TMPDIR") "/tmp")
  "*The name of a directory in which to store temporary files
generated by functions like `browse-url-of-region'.  You might want to
set this to somewhere with restricted read permissions for privacy's sake."
  :type 'string
  :group 'browse-url)

(defcustom browse-url-netscape-version
  3
  "*The version of Netscape you are using.
This affects how URL reloading is done; the mechanism changed
incompatibly at version 4."
  :type 'number
  :group 'browse-url)

(defvar browse-url-temp-file-list '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; URL input

(defun browse-url-url-at-point ()
  (let ((url (thing-at-point 'url)))
    (set-text-properties 0 (length url) nil url)
    url))

;; Having this as a separate function called by the browser-specific
;; functions allows them to be stand-alone commands, making it easier
;; to switch between browsers.

(defun browse-url-interactive-arg (prompt)
  "Read a URL from the minibuffer, prompting with PROMPT.
Default to the URL at or before point.  If invoked with a mouse button,
set point to the position clicked first.  Return a list for use in
`interactive' containing the URL and `browse-url-new-window-p' or its
negation if a prefix argument was given."
  (let ((event (elt (this-command-keys) 0)))
    (and (listp event) (mouse-set-point event)))
  (list (read-string prompt (browse-url-url-at-point))
	(not (eq (null browse-url-new-window-p)
		 (null current-prefix-arg)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Browse current buffer

;;;###autoload
(defun browse-url-of-file (&optional file)
  "Ask a WWW browser to display FILE.
Display the current buffer's file if FILE is nil or if called
interactively.  Turn the filename into a URL with function
`browse-url-file-url'.  Pass the URL to a browser using the
`browse-url' function then run `browse-url-of-file-hook'."
  (interactive)
  (or file
      (setq file (buffer-file-name))
      (error "Current buffer has no file"))
  (let ((buf (get-file-buffer file)))
    (if buf
	(save-excursion
	  (set-buffer buf)
	  (cond ((not (buffer-modified-p)))
		(browse-url-save-file (save-buffer))
		(t (message "%s modified since last save" file))))))
  (browse-url (browse-url-file-url file))
  (run-hooks 'browse-url-of-file-hook))

(defun browse-url-file-url (file)
  "Return the URL corresponding to FILE.
Use variable `browse-url-filename-alist' to map filenames to URLs.
Convert EFS file names of the form /USER@HOST:PATH to ftp://HOST/PATH."
  ;; URL-encode special chars, do % first
  (let ((s 0))
    (while (setq s (string-match "%" file s))
      (setq file (replace-match "%25" t t file)
	    s (1+ s))))
  (while (string-match "[*\"()',=;? ]" file)
    (let ((enc (format "%%%x" (aref file (match-beginning 0)))))
      (setq file (replace-match enc t t file))))
  (let ((maps browse-url-filename-alist))
    (while maps
      (let* ((map (car maps))
	     (from-re (car map))
	     (to-string (cdr map)))
	(setq maps (cdr maps))
	(and (string-match from-re file)
	     (setq file (replace-match to-string t t file))))))
  ;; Check for EFS path
  (and (string-match "^/\\([^:@]+@\\)?\\([^:]+\\):/*" file)
       (setq file (concat "ftp://"
			  (substring file (match-beginning 2) (match-end 2))
			  "/" (substring file (match-end 0)))))
  file)

;;;###autoload
(defun browse-url-of-buffer (&optional buffer)
  "Ask a WWW browser to display BUFFER.
Display the current buffer if BUFFER is nil.  Display only the
currently visible part of BUFFER (from a temporary file) if buffer is
narrowed."
  (interactive)
  (save-excursion
    (and buffer (set-buffer buffer))
    (let ((file-name
	   ;; Ignore real name if restricted
	   (and (= (- (point-max) (point-min)) (buffer-size))
		(or buffer-file-name
		    (and (boundp 'dired-directory) dired-directory)))))
      (or file-name
	  (progn
	    (or browse-url-temp-file-name
		(setq browse-url-temp-file-name
		      (convert-standard-filename
		       (make-temp-name
			(expand-file-name "burl" browse-url-temp-dir)))))
	    (setq file-name browse-url-temp-file-name)
	    (write-region (point-min) (point-max) file-name nil 'no-message)))
      (browse-url-of-file file-name))))

(defun browse-url-delete-temp-file (&optional temp-file-name)
  ;; Delete browse-url-temp-file-name from the file system and from
  ;; browse-url-temp-file-list.  If optional arg TEMP-FILE-NAME is
  ;; non-nil, delete it instead, but only from the file system --
  ;; browse-url-temp-file-list is not affected.
  (let ((file-name (or temp-file-name browse-url-temp-file-name)))
    (if (and file-name (file-exists-p file-name))
	(progn
	  (delete-file file-name)
	  (if (null temp-file-name)
	      (setq browse-url-temp-file-list
		    (delete browse-url-temp-file-name
			    browse-url-temp-file-list)))))))

(defun browse-url-delete-temp-file-list ()
  ;; Delete all elements of browse-url-temp-file-list.
  (while browse-url-temp-file-list
    (browse-url-delete-temp-file (car browse-url-temp-file-list))
    (setq browse-url-temp-file-list
	  (cdr browse-url-temp-file-list))))

(add-hook 'kill-buffer-hook 'browse-url-delete-temp-file)
(add-hook 'kill-emacs-hook 'browse-url-delete-temp-file-list)

;;;###autoload
(defun browse-url-of-dired-file ()
  "In Dired, ask a WWW browser to display the file named on this line."
  (interactive)
  (browse-url-of-file (dired-get-filename)))

;;;###autoload
(defun browse-url-of-region (min max)
  "Ask a WWW browser to display the current region."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region (mark) (point))
      (browse-url-of-buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Browser-independent commands

;; A generic command to call the current browse-url-browser-function

;;;###autoload
(defun browse-url (&rest args)
  "Ask a WWW browser to load URL.
Prompts for a URL, defaulting to the URL at or before point.  Variable
`browse-url-browser-function' says which browser to use."
  (interactive (browse-url-interactive-arg "URL: "))
  (if (consp browse-url-browser-function)
      (browse-url-choose-browser args)
    (apply browse-url-browser-function args)))

(defun browse-url-choose-browser (url &rest args)
  "Pass URL to a browser function chosen.
This is done according to the association list in variable
`browse-url-browser-function'."
  (let ((blist browse-url-browser-function)
	re bf)
    (while (consp blist)
      (setq re (car (car blist))
	    bf (cdr (car blist))
	    blist (cdr blist))
      (if (string-match re url)
	  (progn (apply bf url args) (setq blist t))))
    (or blist
	(error "No browser in browse-url-browser-function matching URL %s" url))))

;;;###autoload
(defun browse-url-at-point ()
  "Ask a WWW browser to load the URL at or before point.
Doesn't let you edit the URL like browse-url.  Variable
`browse-url-browser-function' says which browser to use."
  (interactive)
  (browse-url (browse-url-url-at-point)))

(defun browse-url-event-buffer (event)
  (window-buffer (posn-window (event-start event))))

(defun browse-url-event-point (event)
  (posn-point (event-start event)))

;;;###autoload
(defun browse-url-at-mouse (event)
  "Ask a WWW browser to load a URL clicked with the mouse.
The URL is the one around or before the position of the mouse click
but point is not changed.  Doesn't let you edit the URL like
browse-url.  Variable `browse-url-browser-function' says which browser
to use."
  (interactive "e")
  (save-excursion
    (set-buffer (browse-url-event-buffer event))
    (goto-char (browse-url-event-point event))
    (let ((url (browse-url-url-at-point)))
      (if (string-equal url "")
	  (error "No URL found"))
      (browse-url url))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Browser-specific commands

;; --- Netscape ---

(defun browse-url-process-environment ()
  "Set DISPLAY in the environment to the X display Netscape is running on.
This is either the value of variable `browse-url-netscape-display' if
non-nil, or the same display as Emacs if different from the current
environment, otherwise just use the current environment."
  (let ((display (or browse-url-netscape-display (browse-url-emacs-display))))
    (if display
	(cons (concat "DISPLAY=" display) process-environment)
      process-environment)))

(defun browse-url-emacs-display ()
  "Return the X display Emacs is running on.
This nil if the display is the same as the DISPLAY environment variable.

Actually Emacs could be using several screens on several displays, as
listed by (emacs-display-list) and (x-display-screens DISPLAY), this
just returns the display showing the selected frame.  You got a
problem with that?"
  (let (device display)
    (and (fboundp 'selected-device) (fboundp 'device-type) (fboundp 'device-connection)
	 (setq device (selected-device))
	 (eq (device-type device) 'x)
	 (setq display (device-connection device))
	 (not (equal display (getenv "DISPLAY")))
	 display)))

;;;###autoload
(defun browse-url-netscape (url &optional new-window)
  "Ask the Netscape WWW browser to load URL.

Default to the URL around or before point.  The strings in variable
`browse-url-netscape-arguments' are also passed to Netscape.

When called interactively, if variable `browse-url-new-window-p' is
non-nil, load the document in a new Netscape window, otherwise use a
random existing one.  A non-nil interactive prefix argument reverses
the effect of `browse-url-new-window-p'.

When called non-interactively, optional second argument NEW-WINDOW is
used instead of `browse-url-new-window-p'."
  (interactive (browse-url-interactive-arg "Netscape URL: "))
  ;; URL encode any commas in the URL
  (while (string-match "," url)
    (setq url (replace-match "%2C" t t url)))
  (let* ((process-environment (browse-url-process-environment))
         (process (apply 'start-process
 			(concat "netscape " url) nil
 			browse-url-netscape-program
 			(append browse-url-netscape-arguments
				(if (eq window-system 'w32)
				    (list url)
				  (if new-window '("-noraise"))
				  (list "-remote" 
					(concat "openURL(" url 
						(if new-window ",new-window")
						")")))))))
    (set-process-sentinel process
			  (list 'lambda '(process change)
				(list 'browse-url-netscape-sentinel 'process url)))))

(defun browse-url-netscape-sentinel (process url)
  "Handle a change to the process communicating with Netscape."
  (or (eq (process-exit-status process) 0)
      (let* ((process-environment (browse-url-process-environment)))
	;; Netscape not running - start it
	(message "Starting Netscape...")
	(apply 'start-process (concat "netscape" url) nil
	       browse-url-netscape-program
	       (append browse-url-netscape-startup-arguments (list url))))))

(defun browse-url-netscape-reload ()
  "Ask Netscape to reload its current document."
  (interactive)
  ;; Backwards incompatibility reported by
  ;; <peter.kruse@psychologie.uni-regensburg.de>.
  (browse-url-netscape-send (if (>= browse-url-netscape-version 4)
				"xfeDoCommand(reload)"
				"reload")))

(defun browse-url-netscape-send (command)
  "Send a remote control command to Netscape."
  (let* ((process-environment (browse-url-process-environment)))
    (apply 'start-process "netscape" nil
           browse-url-netscape-program
           (append browse-url-netscape-arguments
                   (list "-remote" command)))))

;; --- Mosaic ---

;;;###autoload
(defun browse-url-mosaic (url &optional new-window)
  ;; new-window ignored
  "Ask the XMosaic WWW browser to load URL.
Default to the URL around or before point."
  (interactive (browse-url-interactive-arg "Mosaic URL: "))
  (let ((pidfile (expand-file-name "~/.mosaicpid"))
	pid pidbuf)
    (if (file-readable-p pidfile)
	(save-excursion
	  (find-file pidfile)
	  (goto-char (point-min))
	  (setq pid (read (current-buffer)))
	  (kill-buffer nil)))
    (if (and pid (zerop (signal-process pid 0))) ; Mosaic running
	(save-excursion
	  (find-file (format "/tmp/Mosaic.%d" pid))
	  (erase-buffer)
	  (insert "goto\n" url "\n")
	  (save-buffer)
	  (kill-buffer nil)
	  ;; Send signal SIGUSR to Mosaic
	  (message "Signalling Mosaic...")
	  (signal-process pid browse-url-usr1-signal)
	  ;; Or you could try:
	  ;; (call-process "kill" nil 0 nil "-USR1" (int-to-string pid))
	  (message "Signalling Mosaic...done")
	  )
      ;; Mosaic not running - start it
      (message "Starting Mosaic...")
      (apply 'start-process "xmosaic" nil "xmosaic"
	     (append browse-url-mosaic-arguments (list url)))
      (message "Starting Mosaic...done"))))

;; --- Grail ---

;;;###autoload
(defvar browse-url-grail
  (concat (or (getenv "GRAILDIR") "~/.grail") "/user/rcgrail.py")
  "*Location of Grail remote control client script `rcgrail.py'.
Typically found in $GRAILDIR/rcgrail.py, or ~/.grail/user/rcgrail.py.")

;;;###autoload
(defun browse-url-grail (url &optional new-window)
  "Ask the Grail WWW browser to load URL.
Default to the URL around or before point.  Runs the program in the
variable `browse-url-grail'."
  (interactive (browse-url-interactive-arg "Grail URL: "))
  (message "Sending URL to Grail...")
  (save-excursion
    (set-buffer (get-buffer-create " *Shell Command Output*"))
    (erase-buffer)
    ;; don't worry about this failing.
    (call-process browse-url-grail nil 0 nil url)
    (message "Sending URL to Grail... done")))

;; --- Mosaic using CCI ---

(defun browse-url-cci (url &optional new-window)
  "Ask the XMosaic WWW browser to load URL.
Default to the URL around or before point.

This function only works for XMosaic version 2.5 or later.  You must
select `CCI' from XMosaic's File menu, set the CCI Port Address to the
value of variable `browse-url-CCI-port', and enable `Accept requests'.

When called interactively, if variable `browse-url-new-window-p' is
non-nil, load the document in a new browser window, otherwise use a
random existing one.  A non-nil interactive prefix argument reverses
the effect of `browse-url-new-window-p'.

When called non-interactively, optional second argument NEW-WINDOW is
used instead of `browse-url-new-window-p'."
  (interactive (browse-url-interactive-arg "Mosaic URL: "))
  (open-network-stream "browse-url" " *browse-url*"
		       browse-url-CCI-host browse-url-CCI-port)
  ;; Todo: start browser if fails
  (process-send-string "browse-url"
		       (concat "get url (" url ") output "
			       (if new-window "new" "current") "\r\n"))
  (process-send-string "browse-url" "disconnect\r\n")
  (delete-process "browse-url"))

;; --- IXI Mosaic ---

;;;###autoload
(defun browse-url-iximosaic (url &optional new-window)
  ;; new-window ignored
  "Ask the IXIMosaic WWW browser to load URL.
Default to the URL around or before point."
  (interactive (browse-url-interactive-arg "IXI Mosaic URL: "))
  (start-process "tellw3b" nil "tellw3b"
		 "-service WWW_BROWSER ixi_showurl " url))

;; --- W3 ---

;;;###autoload
(defun browse-url-w3 (url &optional new-window)
  ;; new-window ignored
  "Ask the w3 WWW browser to load URL.
Default to the URL around or before point."
  (interactive (browse-url-interactive-arg "W3 URL: "))
  (w3-fetch url))

;;;###autoload
(defun browse-url-w3-gnudoit (url &optional new-window)
  ;; new-window ignored
  "Ask another Emacs running gnuserv to load the URL using the W3 browser.
The `browse-url-gnudoit-program' program is used with options given by
`browse-url-gnudoit-args'.  Default to the URL around or before point."
  (interactive (browse-url-interactive-arg "W3 URL: "))
    (apply 'start-process (concat "gnudoit:" url) nil
	   browse-url-gnudoit-program
	   (append browse-url-gnudoit-args (list (concat "(w3-fetch \"" url "\")") "(raise-frame)"))))

;; --- Lynx in an xterm ---

;;;###autoload
(defun browse-url-lynx-xterm (url &optional new-window)
  ;; new-window ignored
  "Ask the Lynx WWW browser to load URL.
Default to the URL around or before point.  A new Lynx process is run
in an Xterm window using the Xterm program named by `browse-url-xterm-program'
with possible additional arguments `browse-url-xterm-args'."
  (interactive (browse-url-interactive-arg "Lynx URL: "))
  (apply 'start-process (concat "lynx" url) nil browse-url-xterm-program
             (append browse-url-xterm-args (list "-e" "lynx" url))))

;; --- Lynx in an Emacs "term" window ---

;;;###autoload
(defun browse-url-lynx-emacs (url &optional new-window)
  ;; new-window ignored
  "Ask the Lynx WWW browser to load URL.
Default to the URL around or before point.  Run a new Lynx process in
an Emacs buffer."
  (interactive (browse-url-interactive-arg "Lynx URL: "))
  (let ((system-uses-terminfo t))	; Lynx uses terminfo
    (if (fboundp 'make-term)
	(let ((term-term-name "vt100"))
	  (set-buffer (make-term "browse-url" "lynx" nil url))
	  (term-mode)
	  (term-char-mode)
	  (switch-to-buffer "*browse-url*"))
      (terminal-emulator "*browse-url*" "lynx" (list url)))))

;; --- MMM ---

;;;###autoload
(defun browse-url-mmm (url &optional new-window)
  "Ask the MMM WWW browser to load URL.
Default to the URL around or before point."
  (interactive (browse-url-interactive-arg "MMM URL: "))
  (message "Sending URL to MMM...")
  (save-excursion
    (set-buffer (get-buffer-create " *Shell Command Output*"))
    (erase-buffer)
    ;; mmm_remote just SEGVs if the file isn't there...
    (if (or (file-exists-p (expand-file-name "~/.mmm_remote"))
	    ;; location in v 0.4:
	    (file-exists-p (expand-file-name "~/.mmm/remote")))
	(call-process "mmm_remote" nil 0 nil url)
      (call-process "mmm" nil 0 nil "-external" url))
    (message "Sending URL to MMM... done")))

;; --- mailto ---

;;;###autoload
(defun browse-url-mail (url &optional new-window)
  "Open a new mail message buffer within Emacs.
Default to using the mailto: URL around or before point as the
recipient's address.  Supplying a non-nil interactive prefix argument
will cause the mail to be composed in another window rather than the
current one."
  (interactive (browse-url-interactive-arg "Mailto URL: "))
  (save-excursion
    (let ((func (if new-window
		    'compose-mail-other-window
		  'compose-mail))
	  (to (if (string-match "^mailto:" url)
		  (substring url 7)
		url)))
      (apply func
	     (list to nil nil nil nil nil (cons 'insert-buffer
						(current-buffer)))))))

;; --- Random browser ---

;;;###autoload
(defun browse-url-generic (url &optional new-window)
  ;; new-window ignored
  "Ask the WWW browser defined by `browse-url-generic-program' to load URL.
Default to the URL around or before point.  A fresh copy of the
browser is started up in a new process with possible additional arguments
`browse-url-generic-args'.  This is appropriate for browsers which
don't offer a form of remote control."
  (interactive (browse-url-interactive-arg "URL: "))
  (if (not browse-url-generic-program)
    (error "No browser defined (`browse-url-generic-program')"))
  (apply 'start-process (concat browse-url-generic-program url) nil
	 browse-url-generic-program
	 (append browse-url-generic-args (list url))))

(provide 'browse-url)

;;; browse-url.el ends here
