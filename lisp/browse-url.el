;;; browse-url.el --- ask a WWW browser to load a URL

;; Copyright 1995, 1996 Free Software Foundation, Inc.

;; Author: Denis Howe <dbh@doc.ic.ac.uk>
;; Maintainer: Denis Howe <dbh@doc.ic.ac.uk>
;; Created: 03 Apr 1995
;; Keywords: hypertext
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
;; browse-url-iximosaic  IXI Mosaic  ?

;; Note that versions of Netscape before 1.1b1 did not have remote
;; control.  <URL:http://home.netscape.com/newsref/std/x-remote.html>
;; and <URL:http://home.netscape.com/info/APIs/>.

;; Netscape can cache Web pages so it may be necessary to tell it to
;; reload the current page if it has changed (eg. if you have edited
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

;; I recommend Nelson Minar <nelson@santafe.edu>'s excellent
;; html-helper-mode.el for editing HTML and thank Nelson for
;; his many useful comments on this code.
;; <URL:http://www.santafe.edu/~nelson/hhm-beta/>

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

;; To display a URL by shift-clicking on it, put this in your ~/.emacs
;; file:
;;      (global-set-key [S-mouse-2] 'browse-url-at-mouse)
;; (Note that using Shift-mouse-1 is not desirable because
;; that event has a standard meaning in Emacs.)

;; To display the current buffer in a web browser:
;; M-x browse-url-of-buffer RET

;; In Dired, to display the file named on the current line:
;; M-x browse-url-of-dired-file RET

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customisation (~/.emacs)

;; To see what variables are available for customization, type `M-x
;; set-variable browse-url TAB'.

;; To bind the browse-url commands to keys with the `C-c u' prefix:
;;      (global-set-key "\C-cu." 'browse-url-at-point)
;;      (global-set-key "\C-cub" 'browse-url-of-buffer)
;;      (global-set-key "\C-cuf" 'browse-url-of-file)
;;      (add-hook 'dired-mode-hook
;;                (lambda ()
;;                  (local-set-key "\C-cuf" 'browse-url-of-dired-file))))
;;      (if (boundp 'browse-url-browser-function)
;;          (global-set-key "\C-cuu" browse-url-browser-function)
;;        (eval-after-load
;;         "browse-url"
;;         '(global-set-key "\C-cuu" browse-url-browser-function)))

;; To use the Emacs w3 browser when not running under X11:
;;      (if (not (eq window-system 'x))
;;          (setq browse-url-browser-function 'browse-url-w3))

;; To always save modified buffers before displaying the file in a browser:
;;      (setq browse-url-save-file t)

;; To get round the Netscape caching problem, you could try either of
;; the following (but not both).  EITHER write-file in
;; html-helper-mode makes Netscape reload document:
;;
;;      (autoload 'browse-url-netscape-reload "browse-url"
;;        "Ask a WWW browser to redisplay the current file." t)
;;      (add-hook 'html-helper-mode-hook
;;                (function (lambda ()
;;                   (add-hook 'local-write-file-hooks
;;                             (function (lambda ()
;;                                (let ((local-write-file-hooks))
;;                                  (save-buffer))
;;                                (browse-url-netscape-reload)
;;                                t))                   ; => file written by hook
;;                             t))))                    ; append to l-w-f-hooks
;;
;; [Does this work for html-mode too?]
;;
;; OR browse-url-of-file ask Netscape to load and then reload the
;; file:
;;
;;      (add-hook 'browse-url-of-file-hook 'browse-url-netscape-reload)

;; You may also want to customise browse-url-netscape-arguments, eg.
;;
;;      (setq browse-url-netscape-arguments '("-install"))
;;
;; or similarly for the other browsers. 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Change Log:

;; 0.00 03 Apr 1995 Denis Howe <dbh@doc.ic.ac.uk>
;;      Created.

;; 0.01 04 Apr 1995
;;      All names start with "browse-url-".  Added provide.

;; 0.02 05 Apr 1995
;;      Save file at start of browse-url-of-file.
;;      Use start-process instead of start-process-shell-command.

;; 0.03 06 Apr 1995
;;      Add browse-url-netscape-reload, browse-url-netscape-command.
;;      browse-url-of-file save file option.

;; 0.04 08 Apr 1995
;;      b-u-file-url separate function.  Change b-u-filename-alist
;;      default.

;; 0.05 09 Apr 1995
;;      Added b-u-of-file-hook.

;; 0.06 11 Apr 1995
;;      Improved .emacs suggestions and documentation.

;; 0.07 13 Apr 1995
;;      Added browse-url-interactive-arg optional prompt.

;; 0.08 18 Apr 1995
;;      Exclude final "." from browse-url-regexp.

;; 0.09 21 Apr 1995
;;      Added mouse-set-point to browse-url-interactive-arg.

;; 0.10 24 Apr 1995
;;      Added Mosaic signal sending variations.
;;      Thanks Brian K Servis <servis@ecn.purdue.edu>.
;;      Don't use xprop for Netscape.

;; 0.11 25 Apr 1995
;;      Fix reading of ~/.mosaicpid.  Thanks Dag.H.Wanvik@kvatro.no.

;; 0.12 27 Apr 1995
;;      Interactive prefix arg => URL *after* point.
;;      Thanks Michelangelo Grigni <mic@cs.ucsd.edu>.
;;      Added IXI Mosaic support.
;;      Thanks David Karr <dkarr@nmo.gtegsc.com>.

;; 0.13 28 Apr 1995
;;      Exclude final [,;] from browse-url-regexp.

;; 0.14 02 May 1995
;;      Provide browser argument variables.

;; 0.15 07 May 1995
;;      More Netscape options.  Thanks Peter Arius
;;      <arius@immd2.informatik.uni-erlangen.de>.

;; 0.16 17 May 1995
;;      Added browse-url-at-mouse.
;;      Thanks Wayne Mesard <wmesard@sgi.com>

;; 0.17 27 Jun 1995
;;      Renamed browse-url-at-point to browse-url-url-at-point.
;;      Added browse-url-at-point.
;;      Thanks Jonathan Cano <cano@patch.tandem.com>.

;; 0.18 16 Aug 1995
;;      Fixed call to browse-url-url-at-point in browse-url-at-point.
;;      Thanks Eric Ding <ericding@San-Jose.ate.slb.com>.

;; 0.19 24 Aug 1995
;;      Improved documentation.
;;      Thanks Kevin Rodgers <kevin.rodgers@ihs.com>.

;; 0.20 31 Aug 1995
;;      browse-url-of-buffer to handle file-less buffers.
;;      browse-url-of-dired-file browses current file in dired.
;;      Thanks Kevin Rodgers <kevin.rodgers@ihs.com>.

;; 0.21 09 Sep 1995
;;      XMosaic CCI functions.
;;      Thanks Marc Furrer <Marc.Furrer@di.epfl.ch>.

;; 0.22 13 Sep 1995
;;      Fixed new-window documentation and added to browse-url-cci.
;;      Thanks Dilip Sequeira <djs@dcs.ed.ac.uk>.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

(defvar browse-url-regexp
  "\\(https?://\\|ftp://\\|gopher://\\|telnet://\\|wais://\\|file:/\\|s?news:\\|mailto:\\)[^]\t\n \"'()<>[^`{}]*[^]\t\n \"'()<>[^`{}.,;]+"
  "A regular expression probably matching a URL.")

(defvar browse-url-browser-function
  'browse-url-choose-browser
  "*Function to display the current buffer in a WWW browser.
This is used by the `browse-url-at-point', `browse-url-at-mouse', and
`browse-url-of-file' commands.
The function should take one argument, an URL.")

(defvar browse-url-netscape-program "netscape"
  "*The name for invoking Netscape.")

(defvar browse-url-netscape-arguments nil
  "*A list of strings to pass to Netscape as arguments.")

(defvar browse-url-new-window-p nil
  "*If non-nil, always open a new browser window.
Passing an interactive argument to \\[browse-url-netscape] or
\\[browse-url-cci] reverses the effect of this variable.  Requires
Netscape version 1.1N or later or XMosaic version 2.5 or later.")

(defvar browse-url-mosaic-program "xmosaic"
  "*The name for invoking Mosaic.")

(defvar browse-url-mosaic-arguments nil
  "*A list of strings to pass to Mosaic as arguments.")

(defvar browse-url-filename-alist
  '(("^/+" . "file:/"))
  "An alist of (REGEXP . STRING) pairs.
Any substring of a filename matching one of the REGEXPs is replaced by
the corresponding STRING.  All pairs are applied in the order given.
Used by the `browse-url-of-file' command.")

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
      'SIGUSR1
    30)                                 ; Check /usr/include/signal.h.
  "The argument to `signal-process' for sending SIGUSR1 to XMosaic.
Emacs 19.29 accepts 'SIGUSR1, earlier versions require an integer
which is 30 on SunOS and 16 on HP-UX and Solaris.")

(defvar browse-url-CCI-port 3003
  "Port to access XMosaic via CCI.
This can be any number between 1024 and 65535 but must correspond to
the value set in the browser.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; URL input

;; thingatpt.el doesn't work for complex regexps.

(defun browse-url-url-at-point ()
  "Return the URL around or before point.
Then search backwards for the start of a URL.  If no URL found, return
the empty string."
  (if (or (looking-at browse-url-regexp)        ; Already at start
          (let ((eol (save-excursion (end-of-line) (point))))
            ;; Search forwards for the next URL or end of line in case
            ;; we're in the middle of one.
            (and (re-search-forward browse-url-regexp eol 'lim)
                 (goto-char (match-beginning 0)))
            ;; Now back to where we started or earlier.
            (re-search-backward browse-url-regexp nil t)))
      (buffer-substring (match-beginning 0) (match-end 0))
    ""))                                ; No match

;; Todo: restrict to around or immediately before point.  Expand bare
;; hostname to URL.

(defun browse-url-interactive-arg (&optional prompt)
  "Read a URL from the minibuffer, optionally prompting with PROMPT.
Default to the URL at or before point.  If bound to a mouse button,
set point to the position clicked.  Return the result as a list for
use in `interactive'."
  (let ((event (elt (this-command-keys) 0)))
    (and (listp event) (mouse-set-point event)))
  (list (read-string (or prompt "URL: ") (browse-url-url-at-point))))

;;;###autoload
(defun browse-url-at-point ()
  "Ask a WWW browser to load the URL at or before point.
The URL is loaded according to the value of `browse-url-browser-function'."
  (interactive)
  (funcall browse-url-browser-function (browse-url-url-at-point)))

;;;###autoload
(defun browse-url-at-mouse (event)
  "Ask a WWW browser to load a URL clicked with the mouse.
The URL is the one around or before the position of the mouse click
but point is not changed.  The URL is loaded according to the value of
`browse-url-browser-function'."
  (interactive "e")
  (save-excursion
    (let ((posn (event-start event)))
      (set-buffer (window-buffer (posn-window posn)))
      (goto-char (posn-point posn))
      (let ((url (browse-url-url-at-point)))
        (if (string-equal url "")
            (error "No URL found"))
        (funcall browse-url-browser-function url)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Browse current buffer

;;;###autoload
(defun browse-url-of-file (&optional file)
  "Ask a WWW browser to display FILE.
Display the current buffer's file if FILE is nil or if called
interactively.  Turn the filename into a URL by performing
replacements given in variable `browse-url-filename-alist'.  Pass the
URL to a browser using variable `browse-url-browser-function' then run
`browse-url-of-file-hook'."
  (interactive)
  (setq file
        (or file
            (buffer-file-name)
            (error "Current buffer has no file")))
  (let ((buf (get-file-buffer file)))
    (if buf
        (save-excursion
          (set-buffer buf)
          (cond ((not (buffer-modified-p)))
                (browse-url-save-file (save-buffer))
                (t (message "%s modified since last save" file))))))
  (funcall browse-url-browser-function (browse-url-file-url file))
  (run-hooks 'browse-url-of-file-hook))

(defun browse-url-file-url (file)
  "Return the URL corresponding to FILE.
Uses variable `browse-url-filename-alist' to map filenames to URLs."
  (let ((maps browse-url-filename-alist))
    (while maps
      (let* ((map (car maps))
             (from-re (car map))
             (to-string (cdr map)))
        (setq maps (cdr maps))
        (if (string-match from-re file)
            (setq file (concat (substring file 0 (match-beginning 0))
                               to-string
                               (substring file (match-end 0))))))))
  file)

(defvar browse-url-temp-file-name nil)
(make-variable-buffer-local 'browse-url-temp-file-name)

(defvar browse-url-temp-file-list '())

;;;###autoload
(defun browse-url-of-buffer (&optional buffer)
  "Ask a WWW browser to display BUFFER.
Display the current buffer if BUFFER is nil."
  (interactive)
  (save-excursion
    (set-buffer (or buffer (current-buffer)))
    (let ((file-name
           (or buffer-file-name
               (and (boundp 'dired-directory) dired-directory))))
      (if (null file-name)
          (progn
            (if (null browse-url-temp-file-name)
                (progn
                  (setq browse-url-temp-file-name
                        (make-temp-name
                         (expand-file-name (buffer-name)
                                           (or (getenv "TMPDIR") "/tmp"))))
                  (setq browse-url-temp-file-list
                        (cons browse-url-temp-file-name
                              browse-url-temp-file-list))))
            (write-region (point-min) (point-max) browse-url-temp-file-name
                          nil 'no-message)))
      (browse-url-of-file (or file-name browse-url-temp-file-name)))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Browser-specific functions

(defun browse-url-netscape (url &optional new-window)
  "Ask the Netscape WWW browser to load URL.

Default to the URL around or before point.  The strings in variable
`browse-url-netscape-arguments' are also passed to Netscape.

When called interactively, if variable `browse-url-new-window-p' is
non-nil, load the document in a new Netscape window, otherwise use a
random existing one.  A non-nil interactive prefix argument reverses
the effect of browse-url-new-window-p.

When called non-interactively, optional second argument NEW-WINDOW is
used instead of browse-url-new-window-p."

  (interactive (append (browse-url-interactive-arg "Netscape URL: ")
                       (list (not (eq (null browse-url-new-window-p)
                                      (null current-prefix-arg))))))
  (let ((res
	 (apply 'call-process browse-url-netscape-program nil nil nil
		(append browse-url-netscape-arguments
			(if new-window '("-noraise"))
			(list "-remote" 
			      (concat "openURL(" url 
				      (if new-window ",new-window")
				      ")"))))
	 ))
    (if (stringp res)
	(error "netscape got signal: %s" res)
      (or (zerop res)
	  (progn			; Netscape not running - start it
	    (message "Starting Netscape...")
	    (apply 'start-process "netscape" nil browse-url-netscape-program
		   (append browse-url-netscape-arguments (list url))))))))

(defun browse-url-netscape-reload ()
  "Ask Netscape to reload its current document."
  (interactive)
  (browse-url-netscape-command "reload"))

(defun browse-url-netscape-command (command)
  "Send a remote control command to Netscape."
  (apply 'start-process "netscape" nil "netscape"
         (append browse-url-netscape-arguments
                 (list "-remote" command))))

(defun browse-url-mosaic (url)
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
          (signal-process pid browse-url-usr1-signal)
          (message "Signal sent to Mosaic")
          ;; Or you could try:
          ;; (call-process "kill" nil 0 nil "-USR1" (int-to-string pid))
          )
      ;; Mosaic not running - start it
      (message "Starting Mosaic...")
      (apply 'start-process "xmosaic" nil browse-url-mosaic-program
             (append browse-url-mosaic-arguments (list url)))
      (message "Starting Mosaic...done"))))

(defun browse-url-cci (url &optional new-window)
  "Ask the XMosaic WWW browser to load URL.
Default to the URL around or before point.

This function only works for XMosaic version 2.5 or later.  You must
select `CCI' from XMosaic's File menu, set the CCI Port Address to the
value of variable `browse-url-CCI-port', and enable `Accept requests'.

When called interactively, if variable `browse-url-new-window-p' is
non-nil, load the document in a new browser window, otherwise use a
random existing one.  A non-nil interactive prefix argument reverses
the effect of browse-url-new-window-p.

When called non-interactively, optional second argument NEW-WINDOW is
used instead of browse-url-new-window-p."
  (interactive (append (browse-url-interactive-arg "Mosaic URL: ")
                       (list (not (eq (null browse-url-new-window-p)
                                      (null current-prefix-arg))))))
  (open-network-stream "browse-url" " *browse-url*"
                       "localhost" browse-url-CCI-port)
  ;; Todo: start browser if fails
  (process-send-string "browse-url"
                       (concat "get url (" url ") output "
                               (if new-window "new" "current") "\r\n"))
  (process-send-string "browse-url" "disconnect\r\n")
  (delete-process "browse-url"))

(defun browse-url-iximosaic (url)
  "Ask the IXIMosaic WWW browser to load URL.
Default to the URL around or before point."
  (interactive (browse-url-interactive-arg "IXI Mosaic URL: "))
  (start-process "tellw3b" nil "tellw3b"
                 "-service WWW_BROWSER ixi_showurl " url))

(defun browse-url-w3 (url)
  "Ask the w3 WWW browser to load URL.
Default to the URL around or before point."
  (interactive (browse-url-interactive-arg "W3 URL: "))
  (w3-fetch url))

(defun browse-url-choose-browser (argument)
  "Decide which browser to use, then invoke it.
This is the default value of `browse-url-browser-function'."
  (if (fboundp 'w3-fetch)
      (setq browse-url-browser-function 'browse-url-w3)
    (setq browse-url-browser-function 'browse-url-netscape))
  (funcall browse-url-browser-function argument))

(provide 'browse-url)

;;; browse-url.el ends here
