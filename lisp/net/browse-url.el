;;; browse-url.el --- pass a URL to a web browser  -*- lexical-binding: t; -*-

;; Copyright (C) 1995-2026 Free Software Foundation, Inc.

;; Author: Denis Howe <dbh@doc.ic.ac.uk>
;; Maintainer: emacs-devel@gnu.org
;; Created: 03 Apr 1995
;; Keywords: hypertext, hypermedia, mouse

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

;; This package provides functions which read a URL from the
;; minibuffer, defaulting to the URL around point, and ask a web
;; browser to load it.  It can also load the URL at point, or one
;; associated with the current buffer.  The main functions are:

;;   `browse-url'             Open URL
;;   `browse-url-at-point'    Open URL at point
;;   `browse-url-of-buffer'   Use web browser to display buffer
;;   `browse-url-of-file'     Use web browser to display file

;; Different browsers use different methods of remote control so there
;; is one function for each supported browser.  If the chosen browser
;; is not running, it is started.  Currently there is support for the
;; following browsers, as well as some other obsolete ones:

;; Function                           Browser     Earliest version
;; browse-url-firefox                 Firefox     Don't know (tried with 1.0.1)
;; browse-url-chrome                  Chrome      47.0.2526.111
;; browse-url-chromium                Chromium    3.0
;; browse-url-epiphany                GNOME Web (Epiphany)    Don't know
;; browse-url-webpositive             WebPositive 1.2-alpha (Haiku R1/beta3)
;; browse-url-text-*                  Any text browser     0
;; browse-url-generic                 arbitrary
;; browse-url-default-windows-browser MS-Windows browser
;; browse-url-default-macosx-browser  macOS browser
;; browse-url-xdg-open                freedesktop.org xdg-open
;; browse-url-kde                     KDE konqueror (kfm)
;; browse-url-elinks                  Elinks      Don't know (tried with 0.12.GIT)
;; browse-url-default-android-browser Android     2.3.3 (should work on 2.2 too)
;; eww-browse-url                     Emacs Web Wowser

;; Browsers can cache web pages so it may be necessary to tell them to
;; reload the current page if it has changed (e.g., if you have edited
;; it).  There is currently no perfect automatic solution to this.

;; See also the ffap.el package.  The huge hyperbole package also
;; contains similar functions.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Usage

;; To display the URL at or before point:
;; M-x browse-url-at-point RET
;; or, similarly but with the opportunity to edit the URL extracted from
;; the buffer, use:
;; M-x browse-url

;; To display a URL by shift-clicking on it, put this in your init file:
;;      (keymap-global-set "S-<mouse-2>" 'browse-url-at-mouse)
;; (Note that using Shift-mouse-1 is not desirable because
;; that event has a standard meaning in Emacs.)

;; To display the current buffer in a web browser:
;; M-x browse-url-of-buffer RET

;; To display the current region in a web browser:
;; M-x browse-url-of-region RET

;; In Dired, to display the file named on the current line:
;; M-x browse-url-of-dired-file RET

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization (Init File)

;; To see what variables are available for customization, type
;; `M-x set-variable browse-url TAB'.  Better, use
;; `M-x customize-group browse-url'.

;; Bind the browse-url commands to keys with the `C-c C-z' prefix:

;;	(keymap-global-set "C-c C-z ." 'browse-url-at-point)
;;	(keymap-global-set "C-c C-z b" 'browse-url-of-buffer)
;;	(keymap-global-set "C-c C-z r" 'browse-url-of-region)
;;	(keymap-global-set "C-c C-z u" 'browse-url)
;;	(keymap-global-set "C-c C-z v" 'browse-url-of-file)
;;	(add-hook 'dired-mode-hook
;;		  (lambda ()
;;	             (keymap-local-set "C-c C-z f" 'browse-url-of-dired-file)))

;; Browse URLs in mail messages under RMAIL by clicking mouse-2:
;;	(add-hook 'rmail-mode-hook (lambda () ; rmail-mode startup
;;	  (keymap-set rmail-mode-map [mouse-2] 'browse-url-at-mouse)))
;; Alternatively, add `goto-address' to `rmail-show-message-hook'.

;; Gnus provides a standard feature to activate URLs in article
;; buffers for invocation of browse-url.

;; Use the Emacs Web Wowser (EWW) when not running under X11:
;;	(or (eq window-system 'x)
;;	    (setopt browse-url-browser-function #'eww-browse-url))

;; To always save modified buffers before displaying the file in a browser:
;;	(setopt browse-url-save-file t)

;; To invoke different browsers/tools for different URLs, customize
;; `browse-url-handlers'.  In earlier versions of Emacs, the same
;; could be done by setting `browse-url-browser-function' to an alist
;; but this usage is deprecated now.

;; All browser functions provided by here have a
;; `browse-url-browser-kind' symbol property set to either `internal'
;; or `external' which determines if they browse the given URL inside
;; Emacs or spawn an external application with it.  Some parts of
;; Emacs make use of that, e.g., when an URL is dragged into Emacs, it
;; is not sensible to invoke an external browser with it, so here only
;; internal browsers are considered.  Therefore, it is advised to put
;; that property also on custom browser functions.
;;       (function-put 'my-browse-url-in-emacs 'browse-url-browser-kind
;;                     'internal)
;;       (function-put 'my-browse-url-externally 'browse-url-browser-kind
;;                     'external)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

(require 'url)
(require 'xdg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables

(defgroup browse-url nil
  "Use a web browser to look at a URL."
  :prefix "browse-url-"
  :link '(emacs-commentary-link "browse-url")
  :group 'external
  :group 'comm)

(defvar browse-url--browser-defcustom-type
  `(choice
    (function-item :tag "Emacs Web Wowser (EWW)" :value  eww-browse-url)
    (function-item :tag "Firefox" :value browse-url-firefox)
    (function-item :tag "Google Chrome" :value browse-url-chrome)
    (function-item :tag "Chromium" :value browse-url-chromium)
    (function-item :tag "GNOME Web (Epiphany)" :value  browse-url-epiphany)
    ,@(when (eq system-type 'haiku)
        (list '(function-item :tag "WebPositive" :value browse-url-webpositive)))
    (function-item :tag "Text browser in an xterm window"
		   :value browse-url-text-xterm)
    (function-item :tag "Text browser in an Emacs window"
		   :value browse-url-text-emacs)
    (function-item :tag "KDE" :value browse-url-kde)
    (function-item :tag "Elinks" :value browse-url-elinks)
    (function-item :tag "Qutebrowser" :value browse-url-qutebrowser)
    (function-item :tag "Specified by `Browse Url Generic Program'"
                   :value browse-url-generic)
    ,@(when (eq system-type 'windows-nt)
        (list '(function-item :tag "Default Windows browser"
                              :value browse-url-default-windows-browser)))
    ,@(when (eq system-type 'darwin)
        (list '(function-item :tag "Default macOS browser"
                              :value browse-url-default-macosx-browser)))
    ,@(when (eq system-type 'android)
        (list '(function-item :tag "Default Android browser"
                              :value browse-url-default-android-browser)))
    ,@(when (eq window-system 'pgtk)
        (list '(function-item :tag "Default GTK browser"
                              :value browse-url-default-gtk-browser)))
    (function-item :tag "Default browser"
		   :value browse-url-default-browser)
    (function :tag "Your own function")
    (alist :tag "Regexp/function association list"
	   :key-type regexp :value-type function
           :format "%{%t%}\n%d%v\n"
           :doc "Deprecated.  Use `browse-url-handlers' instead.")))

;;;###autoload
(defcustom browse-url-browser-function 'browse-url-default-browser
  "Function to display the current buffer in a WWW browser.
This is used by the `browse-url-at-point', `browse-url-at-mouse', and
`browse-url-of-file' commands.

Also see `browse-url-secondary-browser-function' and
`browse-url-handlers'."
  :type browse-url--browser-defcustom-type
  :version "24.1")

(defcustom browse-url-secondary-browser-function 'browse-url-default-browser
  "Function used to launch an alternative browser.

This browser is used as the secondary browser choice, typically
when a prefix argument is given to a URL-opening command in those
modes that support this (for instance `browse-url-at-point',
`goto-addr-at-point', eww or shr).

This assumption is that `browse-url-secondary-browser-function'
and `browse-url-browser-function' are set to distinct browsers.
Either one of the two functions should call an external browser
and the other one should not do the same.

Also see `browse-url-browser-function'."
  :version "27.1"
  :type browse-url--browser-defcustom-type)

(defcustom browse-url-mailto-function 'browse-url-mail
  "Function to display mailto: links.
This variable uses the same syntax as the
`browse-url-browser-function' variable.  If the
`browse-url-mailto-function' variable is nil, that variable will
be used instead."
  :type '(choice
	  (function-item :tag "Emacs Mail" :value browse-url-mail)
	  (function-item :tag "None" nil))
  :version "24.1")

(defcustom browse-url-man-function 'browse-url-man
  "Function to display man: links."
  :type '(radio
          (function-item :tag "Emacs Man" :value browse-url-man)
          (const :tag "None" nil)
          (function :tag "Other function"))
  :version "26.1")

(defcustom browse-url-irc-function 'browse-url-irc
  "Function to open an irc:// link."
  :type '(choice
          (function-item :tag "Emacs IRC" :value browse-url-irc)
          (const :tag "None" nil)
          (function :tag "Other function"))
  :version "29.1")

(defcustom browse-url-button-regexp
  (concat
   "\\b\\(\\(www\\.\\|\\(s?https?\\|ftps?\\|file\\|gophers?\\|gemini\\|"
   "nntps?\\|s?news\\|telnet\\|wais\\|mailto\\|info\\):\\)"
   "\\(//[-a-z0-9_.]+:[0-9]*\\)?"
   (let ((chars "-a-z0-9_=#$@~%&*+\\/[:word:]")
	 (punct "!?:;.,"))
     (concat
      "\\(?:"
      ;; Match paired parentheses, e.g. in Wikipedia URLs:
      ;; http://thread.gmane.org/47B4E3B2.3050402@gmail.com [dead link]
      "[" chars punct "]+" "(" "[" chars punct "]+" ")"
      "\\(?:" "[" chars punct "]+" "[" chars "]" "\\)?"
      "\\|"
      "[" chars punct "]+" "[" chars "]"
      "\\)"))
   "\\)")
  "Regular expression that matches URLs."
  :version "31.1"
  :type 'regexp)

(defcustom browse-url-transform-alist nil
  "Alist of transformations to apply to URLs before loading it.
Each element has the form (ORIG . REPLACEMENT), where ORIG is a regular
expression and REPLACEMENT is the replacement text.  Every element will
be tested in turn, allowing more than one transformation to be made.

Note that ORIG and REPLACEMENT are passed as arguments to
`string-match', so you can, for example, use match groups in ORIG and
backreferences in REPLACEMENT."
  :type '(choice
          (const :tag "None" nil)
          (alist
           :tag "Alist mapping from regexp to replacement"
           :key-type (regexp :tag "Regexp")
           :value-type (regexp :tag "Replacement")))
  :version "31.1")

(defcustom browse-url-browser-display nil
  "The X display for running the browser, if not same as Emacs's."
  :type '(choice string (const :tag "Default" nil)))

(defcustom browse-url-mozilla-program "mozilla"
  "The name by which to invoke Mozilla."
  :type 'string)
(make-obsolete-variable 'browse-url-mozilla-program nil "29.1")

(defcustom browse-url-mozilla-arguments nil
  "A list of strings to pass to Mozilla as arguments."
  :type '(repeat (string :tag "Argument")))
(make-obsolete-variable 'browse-url-mozilla-arguments nil "29.1")

(defcustom browse-url-mozilla-startup-arguments browse-url-mozilla-arguments
  "A list of strings to pass to Mozilla when it starts up.
Defaults to the value of `browse-url-mozilla-arguments' at the time
`browse-url' is loaded."
  :type '(repeat (string :tag "Argument")))
(make-obsolete-variable 'browse-url-mozilla-startup-arguments nil "29.1")

(defun browse-url--find-executable (candidates default)
  (while (and candidates (not (executable-find (car candidates))))
    (setq candidates (cdr candidates)))
  (or (car candidates) default))

(defcustom browse-url-firefox-program
  (browse-url--find-executable '("icecat" "iceweasel") "firefox")
  "The name by which to invoke Firefox or a variant of it."
  :type 'string)

(defcustom browse-url-firefox-arguments nil
  "A list of strings to pass to Firefox (or variant) as arguments."
  :type '(repeat (string :tag "Argument")))

(defcustom browse-url-firefox-startup-arguments browse-url-firefox-arguments
  "A list of strings to pass to Firefox (or variant) when it starts up.
Defaults to the value of `browse-url-firefox-arguments' at the time
`browse-url' is loaded."
  :type '(repeat (string :tag "Argument")))

(make-obsolete-variable 'browse-url-firefox-startup-arguments
                        "it no longer has any effect." "24.5")

(defcustom browse-url-chrome-program
  (browse-url--find-executable '("google-chrome-stable" "google-chrome")
                               "chromium")
  "The name by which to invoke the Chrome browser."
  :type 'string
  :version "25.1")

(defcustom browse-url-chrome-arguments nil
  "A list of strings to pass to Google Chrome as arguments."
  :type '(repeat (string :tag "Argument"))
  :version "25.1")

(defcustom browse-url-chromium-program
  (browse-url--find-executable '("chromium" "chromium-browser") "chromium")
  "The name by which to invoke Chromium."
  :type 'string
  :version "24.1")

(defcustom browse-url-chromium-arguments nil
  "A list of strings to pass to Chromium as arguments."
  :type '(repeat (string :tag "Argument"))
  :version "24.1")

(defcustom browse-url-epiphany-program "epiphany"
  "The name by which to invoke GNOME Web (Epiphany)."
  :type 'string)

(defcustom browse-url-epiphany-arguments nil
  "A list of strings to pass to GNOME Web (Epiphany) as arguments."
  :type '(repeat (string :tag "Argument")))

(defcustom browse-url-epiphany-startup-arguments browse-url-epiphany-arguments
  "A list of strings to pass to GNOME Web (Epiphany) when it starts up.
Defaults to the value of `browse-url-epiphany-arguments' at the time
`browse-url' is loaded."
  :type '(repeat (string :tag "Argument")))

(defcustom browse-url-qutebrowser-program "qutebrowser"
  "The name by which to invoke Qutebrowser."
  :type 'string
  :version "31.1")

(defcustom browse-url-qutebrowser-arguments nil
  "A list of strings to pass to Qutebrowser when it starts up."
  :type '(repeat (string :tag "Argument"))
  :version "31.1")

(defcustom browse-url-webpositive-program "WebPositive"
  "The name by which to invoke WebPositive."
  :type 'string
  :version "29.1")

(defcustom browse-url-mozilla-new-window-is-tab nil
  "Whether to open up new Mozilla windows in a tab or a new window.
If non-nil, then open the URL in a new tab rather than a new window if
`browse-url-mozilla' is asked to open it in a new window via the
NEW-WINDOW argument."
  :type 'boolean)
(make-obsolete-variable 'browse-url-mozilla-new-window-is-tab nil "29.1")

(defcustom browse-url-firefox-new-window-is-tab nil
  "Whether to open up new Firefox windows in a tab or a new window.
If non-nil, then open the URL in a new tab rather than a new window if
`browse-url-firefox' is asked to open it in a new window via the
NEW-WINDOW argument."
  :type 'boolean)

(defcustom browse-url-epiphany-new-window-is-tab nil
  "Whether to open up new Epiphany windows in a tab or a new window.
If non-nil, then open the URL in a new tab rather than a new window if
`browse-url-epiphany' is asked to open it in a new window via the
NEW-WINDOW argument."
  :type 'boolean)

(defcustom browse-url-qutebrowser-new-window-is-tab nil
  "Whether to open up new Qutebrowser windows in a tab or a new window.
If non-nil, then open the URL in a new tab rather than a new window if
`browse-url-qutebrowser' is asked to open it in a new window via the
NEW-WINDOW argument."
  :type 'boolean
  :version "31.1")

(defcustom browse-url-new-window-flag nil
  "Non-nil means always open a new browser window with appropriate browsers.
Passing an interactive argument to \\[browse-url], or specific browser
commands reverses the effect of this variable."
  :type 'boolean)

(defcustom browse-url-filename-alist
  `(("^/\\(ftp@\\|anonymous@\\)?\\([^:/]+\\):/*" . "ftp://\\2/")
    ;; The above loses the username to avoid the browser prompting for
    ;; it in anonymous cases.  If it's not anonymous the next regexp
    ;; applies.
    ("^/\\([^:@/]+@\\)?\\([^:/]+\\):/*" . "ftp://\\1\\2/")
    ,@(if (memq system-type '(windows-nt ms-dos))
          '(("^\\([a-zA-Z]:\\)[\\/]" . "file:///\\1/")
            ("^[\\/][\\/]+" . "file://")))
    ("^/+" . "file:///"))
  "An alist of (REGEXP . STRING) pairs used by `browse-url-of-file'.
Any substring of a filename matching one of the REGEXPs is replaced by
the corresponding STRING using `replace-match', not treating STRING
literally.  All pairs are applied in the order given.  The default
value converts ange-ftp-style file names into ftp URLs and prepends
`file:' to any file name beginning with `/'.

For example, adding to the default a specific translation of an ange-ftp
address to an HTTPS URL:

    (setopt browse-url-filename-alist
            \\='((\"/webmaster@webserver:/home/www/html/\" .
               \"https://www.example.org/\")
              (\"^/\\(ftp@\\|anonymous@\\)?\\([^:/]+\\):/*\" . \"ftp://\\2/\")
              (\"^/\\([^:@/]+@\\)?\\([^:/]+\\):/*\" . \"ftp://\\1\\2/\")
              (\"^/+\" . \"file:/\")))"
  :type '(repeat (cons :format "%v"
                       (regexp :tag "Regexp")
                       (string :tag "Replacement")))
  :version "25.1")

(defcustom browse-url-save-file nil
  "If non-nil, save the buffer before displaying its file.
Used by the `browse-url-of-file' command."
  :type 'boolean)

(defcustom browse-url-of-file-hook nil
  "Hook run after `browse-url-of-file' has asked a browser to load a file."
  :type 'hook)

(defvar-local browse-url-temp-file-name nil)

(defcustom browse-url-xterm-program "xterm"
  "The name of the terminal emulator used by `browse-url-text-xterm'.
This might, for instance, be a separate color version of xterm."
  :type 'string)

(defcustom browse-url-xterm-args nil
  "A list of strings defining options for `browse-url-xterm-program'.
These might set its size, for instance."
  :type '(repeat (string :tag "Argument")))

(defcustom browse-url-generic-program nil
  "The name of the browser program used by `browse-url-generic'."
  :type '(choice string (const :tag "None" nil)))

(defcustom browse-url-generic-args nil
  "A list of strings defining options for `browse-url-generic-program'."
  :type '(repeat (string :tag "Argument")))

(defcustom browse-url-temp-dir temporary-file-directory
  "The name of a directory for browse-url's temporary files.
Such files are generated by functions like `browse-url-of-region'.
You might want to set this to somewhere with restricted read permissions
for privacy's sake."
  :type 'string)

(defcustom browse-url-text-browser
  (cond ((executable-find "lynx") "lynx")
        ((executable-find "links") "links")
        ((executable-find "elinks") "elinks")
        ("lynx"))
  "The name of the text browser to invoke."
  :type 'string
  :version "31.1")

(defcustom browse-url-text-emacs-args (and (not window-system)
					   '("-show_cursor"))
  "A list of strings defining options for a text browser in an Emacs buffer.

The default is none in a window system, otherwise `-show_cursor' to
indicate the position of the current link in the absence of
highlighting, assuming the normal default for showing the cursor."
  :type '(repeat (string :tag "Argument"))
  :version "23.1")

(defcustom browse-url-text-input-field 'avoid
  "Action on selecting an existing text browser buffer at an input field.
What to do when sending a new URL to an existing text browser buffer in Emacs
if the browser cursor is on an input field (in which case the `g' command
would be entered as data).  Such fields are recognized by the
underlines ____.  Allowed values: nil: disregard it, `warn': warn the
user and don't emit the URL, `avoid': try to avoid the field by moving
down (this *won't* always work)."
  :type '(choice (const :tag "Move to try to avoid field" :value avoid)
                 (const :tag "Disregard" :value nil)
                 (const :tag "Warn, don't emit URL" :value warn))
  :version "23.1")

(defcustom browse-url-text-input-attempts 10
  "How many times to try to move down from a series of text browser input fields."
  :type 'integer
  :version "23.1")

(defcustom browse-url-text-input-delay 0.2
  "Seconds to wait for a text browser between moves down from an input field."
  :type 'number
  :version "23.1")

(defcustom browse-url-kde-program "kde-open"
  "The name by which to invoke the KDE web browser."
  :type 'string
  :version "31.1")

(defcustom browse-url-kde-args nil
  "A list of strings defining options for `browse-url-kde-program'."
  :type '(repeat (string :tag "Argument"))
  :version "31.1")

(defcustom browse-url-elinks-wrapper '("xterm" "-e")
  "Wrapper command prepended to the Elinks command-line."
  :type '(repeat (string :tag "Wrapper")))

(defun browse-url--browser-kind (function url)
  "Return the browser kind of a browser FUNCTION for URL.
The browser kind is either `internal' (the browser runs inside
Emacs), `external' (the browser is spawned in an external
process), or nil (we don't know)."
  (let ((kind (if (symbolp function)
                  (get function 'browse-url-browser-kind))))
    (if (functionp kind)
        (funcall kind url)
      kind)))

(defun browse-url--mailto (url &rest args)
  "Call `browse-url-mailto-function' with URL and ARGS."
  (funcall browse-url-mailto-function url args))

(defun browse-url--browser-kind-mailto (url)
  (browse-url--browser-kind browse-url-mailto-function url))
(function-put 'browse-url--mailto 'browse-url-browser-kind
              #'browse-url--browser-kind-mailto)

(defun browse-url--man (url &rest args)
  "Call `browse-url-man-function' with URL and ARGS."
  (funcall browse-url-man-function url args))

(defun browse-url--browser-kind-man (url)
  (browse-url--browser-kind browse-url-man-function url))
(function-put 'browse-url--man 'browse-url-browser-kind
              #'browse-url--browser-kind-man)

(defun browse-url--irc (url &rest args)
  "Call `browse-url-irc-function' with URL and ARGS."
  (funcall browse-url-irc-function url args))
(function-put 'browse-url--irc 'browse-url-browser-kind 'internal)

(defun browse-url--browser (url &rest args)
  "Call `browse-url-browser-function' with URL and ARGS."
  (funcall browse-url-browser-function url args))

(defun browse-url--browser-kind-browser (url)
  (browse-url--browser-kind browse-url-browser-function url))
(function-put 'browse-url--browser 'browse-url-browser-kind
              #'browse-url--browser-kind-browser)

(defun browse-url--non-html-file-url-p (url)
  "Return non-nil if URL is a file:// URL of a non-HTML file."
  (and (string-match-p "\\`file://" url)
       (not (string-match-p "\\`file://.*\\.html?\\b" url))))

;;;###autoload
(defvar browse-url-default-handlers
  '(("\\`mailto:" . browse-url--mailto)
    ("\\`man:" . browse-url--man)
    ("\\`irc6?s?://" . browse-url--irc)
    (browse-url--non-html-file-url-p . browse-url-emacs))
  "Like `browse-url-handlers' but populated by Emacs and packages.

Emacs and external packages capable of browsing certain URLs
should place their entries in this alist rather than
`browse-url-handlers' which is reserved for the user.")

(defcustom browse-url-handlers nil
  "An alist with elements of the form (REGEXP-OR-PREDICATE . HANDLER).
Each REGEXP-OR-PREDICATE is matched against the URL to be opened
in turn and the first match's HANDLER is invoked with the URL.

A HANDLER must be a function with the same arguments as
`browse-url'.

If no REGEXP-OR-PREDICATE matches, the same procedure is
performed with the value of `browse-url-default-handlers'.  If
there is also no match, the URL is opened using the value of
`browse-url-browser-function'."
  :type '(alist :key-type (choice
                           (regexp :tag "Regexp")
                           (function :tag "Predicate"))
                :value-type (function :tag "Handler"))
  :version "28.1")

;;;###autoload
(defun browse-url-select-handler (url &optional kind)
  "Return a handler of suitable for browsing URL.
This searches `browse-url-handlers', and
`browse-url-default-handlers' for a matching handler.  Return nil
if no handler is found.

If KIND is given, the search is restricted to handlers whose
function symbol has the symbol-property `browse-url-browser-kind'
set to KIND.

Currently, it also consults `browse-url-browser-function' first
if it is set to an alist, although this usage is deprecated since
Emacs 28.1 and will be removed in a future release."
  (catch 'custom-url-handler
    (dolist (rxpred-handler
             (append
              ;; The alist choice of browse-url-browser-function
              ;; is deprecated since 28.1, so the (unless ...)
              ;; can be removed at some point in time.
              (when (and (consp browse-url-browser-function)
	                 (not (functionp browse-url-browser-function)))
                (lwarn 'browse-url :warning
                       "Having `browse-url-browser-function' set to an
alist is deprecated.  Use `browse-url-handlers' instead.")
                browse-url-browser-function)
              browse-url-handlers
              browse-url-default-handlers))
      (let ((rx-or-pred (car rxpred-handler))
            (handler (cdr rxpred-handler)))
        (when (and (or (null kind)
                       (eq kind (browse-url--browser-kind
                                 handler url)))
                   (if (functionp rx-or-pred)
                       (funcall rx-or-pred url)
                     (string-match-p rx-or-pred url)))
          (throw 'custom-url-handler handler))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; URL encoding

(defun browse-url-url-encode-chars (text chars)
  "URL-encode the chars in TEXT that match CHARS.
CHARS is a regexp that matches a character."
  (replace-regexp-in-string chars
                            (lambda (s)
                              (format "%%%X" (string-to-char s)))
                            text))

(defun browse-url-encode-url (url)
  "Escape annoying characters in URL.
The annoying characters are those that can mislead a web browser
regarding its parameter treatment."
  ;; FIXME: Is there an actual example of a web browser getting
  ;; confused?  (This used to encode commas and dollar signs, but at
  ;; least Firefox handles commas correctly and doesn't accept those
  ;; encoded.)
  (browse-url-url-encode-chars url "[\"() ]"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; URL input

(defcustom browse-url-default-scheme "http"
  "URL scheme that `browse-url' (and related commands) will use by default.

For example, when point is on an URL fragment like \"www.example.org\",
`browse-url' will assume that this is an \"http\" URL by default (for
example, \"http://www.example.org\")."
  :type '(choice (const :tag "HTTP" "http")
                 (const :tag "HTTPS" "https")
                 (string :tag "Something else" "https"))
  :risky t
  :version "29.1")

(defun browse-url-url-at-point ()
  (or (thing-at-point 'url t)
      ;; assume that the user is pointing at something like gnu.org/gnu
      (when-let* ((f (thing-at-point 'filename t)))
	(if (string-match-p browse-url-button-regexp f)
	    f
	  (concat browse-url-default-scheme "://" f)))))

;; Having this as a separate function called by the browser-specific
;; functions allows them to be stand-alone commands, making it easier
;; to switch between browsers.

(defun browse-url-interactive-arg (prompt)
  "Read a URL from the minibuffer, prompting with PROMPT.
If `transient-mark-mode' is non-nil and the mark is active,
it defaults to the current region, else to the URL at or before
point.  If invoked with a mouse button, it moves point to the
position clicked before acting.

This function returns a list (URL NEW-WINDOW-FLAG) for use in
`interactive'.  NEW-WINDOW-FLAG is the prefix arg; if
`browse-url-new-window-flag' is non-nil, invert the prefix arg
instead."
  (mouse-set-point last-nonmenu-event)
  (list (read-string prompt (or (and transient-mark-mode mark-active
				     ;; rfc2396 Appendix E.
				     (replace-regexp-in-string
				      "[\t\r\f\n ]+" ""
				      (buffer-substring-no-properties
				       (region-beginning) (region-end))))
				(browse-url-url-at-point)))
	(xor browse-url-new-window-flag current-prefix-arg)))

;; called-interactive-p needs to be called at a function's top-level, hence
;; this macro.  We use that rather than interactive-p because
;; use in a keyboard macro should not change this behavior.
(defmacro browse-url-maybe-new-window (arg)
  `(if (or noninteractive (not (called-interactively-p 'any)))
       ,arg
     browse-url-new-window-flag))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Browse current buffer

(defmacro browse-url--temp-file-setup (&rest body)
  (declare (indent defun))
  `(progn
     (add-hook 'write-file-functions #'browse-url-delete-temp-file nil t)
     (add-hook 'kill-buffer-hook #'browse-url-delete-temp-file nil t)
     (with-file-modes #o600
       ,@body)))

;;;###autoload
(defun browse-url-of-file (&optional file)
  "Use a web browser to display FILE.
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
	(with-current-buffer buf
	  (cond ((not (buffer-modified-p)))
		(browse-url-save-file (save-buffer))
		(t (message "%s modified since last save" file))))))
  (when (file-remote-p file)
    (browse-url--temp-file-setup
      (unless browse-url-temp-file-name
        (setq browse-url-temp-file-name (file-local-copy file)))
      (setq file browse-url-temp-file-name)))
  (browse-url (browse-url-file-url file))
  (run-hooks 'browse-url-of-file-hook))

(defun browse-url--file-name-coding-system ()
  (if (equal system-type 'windows-nt)
      ;; W32 pretends that file names are UTF-8 encoded.
      'utf-8
    (or file-name-coding-system default-file-name-coding-system)))

(defun browse-url-file-url (file)
  "Return the URL corresponding to FILE.
Use variable `browse-url-filename-alist' to map filenames to URLs."
  (when-let* ((coding (browse-url--file-name-coding-system)))
    (setq file (encode-coding-string file coding)))
  (if (and (file-remote-p file)
           ;; We're applying special rules for FTP URLs for historical
           ;; reasons.
           (seq-find (lambda (match)
                       (and (string-match-p (car match) file)
                            (not (string-match "\\`file:" (cdr match)))))
                     browse-url-filename-alist))
      (setq file (browse-url-url-encode-chars file "[*\"()',=;?% ]"))
    ;; Encode all other file names properly.
    (let ((bits (file-name-split file)))
      (setq file
            (string-join
             ;; On Windows, the first bit here might be "c:" or the
             ;; like, so don't encode the ":" in the first bit.
             (cons (let ((url-unreserved-chars
                          (if (file-name-absolute-p file)
                              (cons ?: url-unreserved-chars)
                            url-unreserved-chars)))
                     (url-hexify-string (car bits)))
                   (mapcar #'url-hexify-string (cdr bits)))
             "/"))))
  (dolist (map browse-url-filename-alist)
    (when (and map (string-match (car map) file))
      (setq file (replace-match (cdr map) t nil file))))
  file)

;;;###autoload
(defun browse-url-of-buffer (&optional buffer)
  "Use a web browser to display BUFFER.
See `browse-url' for details.

Display the current buffer if BUFFER is nil.  Display only the
currently visible part of BUFFER (from a temporary file) if buffer is
narrowed."
  (interactive)
  (save-excursion
    (and buffer (set-buffer buffer))
    (let ((file-name
	   ;; Ignore real name if restricted
	   (and (not (buffer-narrowed-p))
		(or buffer-file-name
		    (and (boundp 'dired-directory) dired-directory)))))
      (when (or (not file-name)
                ;; This can happen when we're looking at a file from a
                ;; zip file buffer, for instance.
                (not (file-exists-p file-name)))
        (browse-url--temp-file-setup
          (unless browse-url-temp-file-name
            (setq browse-url-temp-file-name
                  (convert-standard-filename
                   (make-temp-file
                    (expand-file-name "burl" browse-url-temp-dir)
                    nil ".html"))))
          (setq file-name browse-url-temp-file-name)
          (write-region (point-min) (point-max) file-name nil 'no-message)))
      (browse-url-of-file file-name))))

(defun browse-url-delete-temp-file (&optional temp-file-name)
  "Delete `browse-url-temp-file-name' from the file system."
  (declare (advertised-calling-convention () "31.1"))
  (let ((file-name (or temp-file-name browse-url-temp-file-name)))
    (if (and file-name (file-exists-p file-name))
	(delete-file file-name))
    (unless temp-file-name (setq browse-url-temp-file-name nil))))

(declare-function dired-get-filename "dired"
		  (&optional localp no-error-if-not-filep))

;;;###autoload
(defun browse-url-of-dired-file (&optional secondary)
  "In Dired, ask a WWW browser to display the file named on this line.
With prefix arg, use the secondary browser instead (e.g. EWW if
`browse-url-secondary-browser-function' is set to
`eww-browse-url'."
  (interactive "P")
  (let ((tem (dired-get-filename t t))
        (browse-url-browser-function
         (if secondary
             browse-url-secondary-browser-function
           browse-url-browser-function))
        ;; Some URL handlers open files in Emacs.  We want to always
        ;; open in a browser, so disable those.
        (browse-url-default-handlers nil))
    (if tem
	(browse-url-of-file (expand-file-name tem))
      (error "No file on this line"))))

;;;###autoload
(defun browse-url-of-region (min max)
  "Use a web browser to display the current region.
See `browse-url' for details."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region min max)
      (browse-url-of-buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Browser-independent commands

;; A generic command to call the current browse-url-browser-function

(declare-function pgtk-backend-display-class "pgtkfns.c" (&optional terminal))

;;;###autoload
(defun browse-url (url &rest args)
  "Open URL using a configurable method.
This will typically (by default) open URL with an external web
browser, but a wide variety of different methods can be used,
depending on the URL type.

The variables `browse-url-browser-function',
`browse-url-handlers', and `browse-url-default-handlers'
determine which browser function to use.

Interactively, this command prompts for a URL, defaulting to the
URL at or before point.

The additional ARGS are passed to the browser function.  See the
doc strings of the actual functions, starting with
`browse-url-browser-function', for information about the
significance of ARGS (most of the functions ignore it).

If ARGS are omitted, the default is to pass
`browse-url-new-window-flag' as ARGS.  Interactively, pass the
prefix arg as ARGS; if `browse-url-new-window-flag' is non-nil,
invert the prefix arg instead."
  (interactive (browse-url-interactive-arg "URL: "))
  (unless (called-interactively-p 'interactive)
    (setq args (or args (list browse-url-new-window-flag))))
  (when browse-url-transform-alist
    (dolist (trans browse-url-transform-alist)
      (when (string-match (car trans) url)
        (setq url (replace-match (cdr trans) nil t url)))))
  (when (and url-handler-mode
             (not (file-name-absolute-p url))
             (not (string-match "\\`[a-z]+:" url)))
    (setq url (expand-file-name url)))
  (let ((process-environment (copy-sequence process-environment))
	(function (or (browse-url-select-handler url)
                      browse-url-browser-function))
	;; Ensure that `default-directory' exists and is readable (bug#6077).
	(default-directory (or (unhandled-file-name-directory default-directory)
			       (expand-file-name "~/"))))
    ;; When connected to various displays, be careful to use the display of
    ;; the currently selected frame, rather than the original start display,
    ;; which may not even exist any more.
    (let ((dpy (frame-parameter nil 'display))
          classname)
      (if (stringp dpy)
        (cond
         ((featurep 'pgtk)
          (setq classname (pgtk-backend-display-class))
          (if (equal classname "GdkWaylandDisplay")
              (progn
                ;; The `display' frame parameter is probably wrong.
                ;; See bug#53969 for some context.
                ;; (setenv "WAYLAND_DISPLAY" dpy)
                )
            (setenv "DISPLAY" dpy)))
         ((featurep 'android)
          ;; Avoid modifying the DISPLAY environment variable here,
          ;; which interferes with any X server the user may have
          ;; expressly set.
          nil)
         (t
          (setenv "DISPLAY" dpy)))))
    (if (functionp function)
        (apply function url args)
      (error "No suitable browser for URL %s" url))))

;;;###autoload
(defun browse-url-at-point (&optional arg)
  "Open URL at point using a configurable method.
See `browse-url' for details.
Optional prefix argument ARG non-nil inverts the value of the option
`browse-url-new-window-flag'."
  (interactive "P")
  (let ((url (browse-url-url-at-point)))
    (if url
	(browse-url url (if arg
			    (not browse-url-new-window-flag)
			  browse-url-new-window-flag))
      (error "No URL found"))))

;;;###autoload
(defun browse-url-with-browser-kind (kind url &optional arg)
  "Browse URL with a browser of the given browser KIND.

KIND is either `internal' or `external'.  In order to find an
appropriate browser for the given KIND, first consult the `browse-url-handlers'
and `browse-url-default-handlers' lists.  If no handler is found, try the
functions `browse-url-browser-function',
`browse-url-secondary-browser-function', `browse-url-default-browser'
and `eww', in that order.

When called interactively, the default browser kind is the
opposite of the browser kind of `browse-url-browser-function'."
  (interactive
   (let* ((url-arg (browse-url-interactive-arg "URL: "))
          ;; Default to the inverse kind of the default browser.
          (default (if (eq (browse-url--browser-kind
                            browse-url-browser-function (car url-arg))
                           'internal)
                       'external
                     'internal))
          (k (intern (completing-read
                      (format-prompt "Browser kind" default)
                      '(internal external)
                      nil t nil nil
                      default))))
     (cons k url-arg)))
  (let ((function (browse-url-select-handler url kind)))
    (unless function
      (setq function
            (seq-find
             (lambda (fun)
               (eq kind (browse-url--browser-kind fun url)))
             (list browse-url-browser-function
                   browse-url-secondary-browser-function
                   #'browse-url-default-browser
                   #'eww))))
    (let ((browse-url-browser-function function)
          (browse-url-handlers nil)
          (browse-url-default-handlers nil))
      (browse-url url arg))))

;;;###autoload
(defun browse-url-at-mouse (event)
  "Use a web browser to load a URL clicked with the mouse.
See `browse-url' for details.

The URL is the one around or before the position of the mouse
click but point is not changed."
  (interactive "e")
  (save-excursion
    (mouse-set-point event)
    ;; This handles browse-url-new-window-flag properly
    ;; when it gets no arg.
    (browse-url-at-point)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Browser-specific commands

;; --- Default MS-Windows browser ---

(defvar dos-windows-version)
(declare-function w32-shell-execute "w32fns.c")    ;; Defined in C.

(defun browse-url-default-windows-browser (url &optional _new-window)
  "Invoke the MS-Windows system's default Web browser.
The optional NEW-WINDOW argument is not used."
  (interactive (browse-url-interactive-arg "URL: "))
  (setq url (browse-url-encode-url url))
  (cond ((eq system-type 'ms-dos)
	 (if dos-windows-version
	     (shell-command (concat "start " (shell-quote-argument url)))
	   (error "Browsing URLs is not supported on this system")))
	((eq system-type 'cygwin)
	 (call-process "cygstart" nil nil nil url))
	(t
         (w32-shell-execute "open"
                            ;; w32-shell-execute passes file:// URLs
                            ;; to APIs that expect file names, so we
                            ;; need to unhex any %nn encoded
                            ;; characters in the URL.  We don't do
                            ;; that for other URLs; in particular,
                            ;; default Windows mail client barfs on
                            ;; quotes in the MAILTO URLs, so we prefer
                            ;; to leave the URL with its embedded %nn
                            ;; encoding intact.
                            (if (string-prefix-p "file://" url)
                                (url-unhex-string url)
                              url)))))

(function-put 'browse-url-default-windows-browser 'browse-url-browser-kind
              'external)

(defun browse-url-default-macosx-browser (url &optional _new-window)
  "Invoke the macOS system's default Web browser.
The optional NEW-WINDOW argument is not used."
  (interactive (browse-url-interactive-arg "URL: "))
  (setq url (browse-url-encode-url url))
  (start-process (concat "open " url) nil "open" url))

(function-put 'browse-url-default-macosx-browser 'browse-url-browser-kind
              'external)

(defun browse-url-process-environment ()
  "Set DISPLAY in the environment to the X display the browser will use.
This is either the value of variable `browse-url-browser-display' if
non-nil, or the same display as Emacs if different from the current
environment, otherwise just use the current environment."
  (let ((display (or browse-url-browser-display (browse-url-emacs-display))))
    (if display
	(cons (concat (if (and (eq window-system 'pgtk)
                               (equal (pgtk-backend-display-class)
                                      "GdkWaylandDisplay"))
                          "WAYLAND_DISPLAY="
                        "DISPLAY=")
                      display)
              process-environment)
      process-environment)))

(defun browse-url-emacs-display ()
  "Return the X display Emacs is running on.
This is nil if the display is the same as the DISPLAY environment variable.

Actually Emacs could be using several displays; this just returns the
one showing the selected frame."
  (let ((display (cdr-safe (assq 'display (frame-parameters)))))
    (and (not (equal display (getenv "DISPLAY")))
         display)))

(defvar browse-url--inhibit-pgtk nil)

(defun browse-url-default-browser (url &rest args)
  "Find a suitable browser and ask it to load URL.
Default to the URL around or before point.

When called interactively, if variable `browse-url-new-window-flag' is
non-nil, load the document in a new window, if possible, otherwise use
a random existing one.  A non-nil interactive prefix argument reverses
the effect of `browse-url-new-window-flag'.

When called non-interactively, optional second argument ARGS is used
instead of `browse-url-new-window-flag'."
  (apply
   (cond
    ((memq system-type '(windows-nt ms-dos cygwin))
     'browse-url-default-windows-browser)
    ((memq system-type '(darwin))
     'browse-url-default-macosx-browser)
    ((featurep 'haiku)
     'browse-url-default-haiku-browser)
    ((eq system-type 'android)
     'browse-url-default-android-browser)
    ((and (eq (frame-parameter nil 'window-system) 'pgtk)
          (not browse-url--inhibit-pgtk))
     'browse-url-default-gtk-browser)
    ((browse-url-can-use-xdg-open) 'browse-url-xdg-open)
    ((executable-find browse-url-firefox-program) 'browse-url-firefox)
    ((executable-find browse-url-chromium-program) 'browse-url-chromium)
    ((executable-find browse-url-kde-program) 'browse-url-kde)
    ((executable-find browse-url-chrome-program) 'browse-url-chrome)
    ((executable-find browse-url-webpositive-program) 'browse-url-webpositive)
    ((executable-find browse-url-xterm-program) 'browse-url-text-xterm)
    (t #'eww-browse-url))
   url args))

(function-put 'browse-url-default-browser 'browse-url-browser-kind
              ;; Well, most probably external if we ignore EWW.
              'external)

(defun browse-url-can-use-xdg-open ()
  "Return non-nil if the \"xdg-open\" program can be used.
xdg-open is a desktop utility that calls your preferred web browser."
  ;; The exact set of situations where xdg-open works is complicated,
  ;; and it would be a pain to duplicate xdg-open's situation-specific
  ;; code here, as the code is a moving target.  So assume that
  ;; xdg-open will work if there is a graphical display; this should
  ;; be good enough for platforms Emacs is likely to be running on.
  (and (or (getenv "DISPLAY") (getenv "WAYLAND_DISPLAY"))
       (executable-find "xdg-open")))

;;;###autoload
(defun browse-url-xdg-open (url &optional _ignored)
  "Pass the specified URL to the \"xdg-open\" command.
xdg-open is a desktop utility that calls your preferred web browser.
The optional argument IGNORED is not used."
  (interactive (browse-url-interactive-arg "URL: "))
  (call-process "xdg-open" nil 0 nil url))

(function-put 'browse-url-xdg-open 'browse-url-browser-kind 'external)

;;;###autoload
(defun browse-url-mozilla (url &optional new-window)
  "Ask the Mozilla WWW browser to load URL.
Default to the URL around or before point.  The strings in variable
`browse-url-mozilla-arguments' are also passed to Mozilla.

When called interactively, if variable `browse-url-new-window-flag' is
non-nil, load the document in a new Mozilla window, otherwise use a
random existing one.  A non-nil interactive prefix argument reverses
the effect of `browse-url-new-window-flag'.

If `browse-url-mozilla-new-window-is-tab' is non-nil, then whenever a
document would otherwise be loaded in a new window, it is loaded in a
new tab in an existing window instead.

When called non-interactively, optional second argument NEW-WINDOW is
used instead of `browse-url-new-window-flag'."
  (declare (obsolete nil "29.1"))
  (interactive (browse-url-interactive-arg "URL: "))
  (setq url (browse-url-encode-url url))
  (let* ((process-environment (browse-url-process-environment))
         (process
	  (apply #'start-process
		 (concat "mozilla " url) nil
		 browse-url-mozilla-program
		 (append
		  browse-url-mozilla-arguments
		  (list "-remote"
			(concat "openURL("
				url
				(if (browse-url-maybe-new-window
				     new-window)
				    (if browse-url-mozilla-new-window-is-tab
					",new-tab"
				      ",new-window"))
				")"))))))
    (set-process-sentinel process
			  (lambda (process _change)
			    (browse-url-mozilla-sentinel process url)))))

(function-put 'browse-url-mozilla 'browse-url-browser-kind 'external)

(defun browse-url-mozilla-sentinel (process url)
  "Handle a change to the process communicating with Mozilla."
  (declare (obsolete nil "29.1"))
  (or (eq (process-exit-status process) 0)
      (let* ((process-environment (browse-url-process-environment)))
	;; Mozilla is not running - start it
	(message "Starting %s..." browse-url-mozilla-program)
	(apply #'start-process (concat "mozilla " url) nil
	       browse-url-mozilla-program
	       (append browse-url-mozilla-startup-arguments (list url))))))

;;;###autoload
(defun browse-url-firefox (url &optional new-window)
  "Ask the Firefox WWW browser to load URL.
Defaults to the URL around or before point.  Passes the strings
in the variable `browse-url-firefox-arguments' to Firefox.

Interactively, if the variable `browse-url-new-window-flag' is non-nil,
loads the document in a new Firefox window.  A non-nil prefix argument
reverses the effect of `browse-url-new-window-flag'.

If `browse-url-firefox-new-window-is-tab' is non-nil, then
whenever a document would otherwise be loaded in a new window, it
is loaded in a new tab in an existing window instead.

Non-interactively, this uses the optional second argument NEW-WINDOW
instead of `browse-url-new-window-flag'."
  (interactive (browse-url-interactive-arg "URL: "))
  (setq url (browse-url-encode-url url))
  (let* ((process-environment (browse-url-process-environment)))
    (apply #'start-process
           (concat "firefox " url) nil
           browse-url-firefox-program
           (append
            browse-url-firefox-arguments
            (if (browse-url-maybe-new-window new-window)
		(if browse-url-firefox-new-window-is-tab
		    '("-new-tab")
		  '("-new-window")))
            (list url)))))

(function-put 'browse-url-firefox 'browse-url-browser-kind 'external)

;;;###autoload
(defun browse-url-chromium (url &optional _new-window)
  "Ask the Chromium WWW browser to load URL.
Default to the URL around or before point.  The strings in
variable `browse-url-chromium-arguments' are also passed to
Chromium.
The optional argument NEW-WINDOW is not used."
  (interactive (browse-url-interactive-arg "URL: "))
  (setq url (browse-url-encode-url url))
  (let* ((process-environment (browse-url-process-environment)))
    (apply #'start-process
	   (concat "chromium " url) nil
	   browse-url-chromium-program
	   (append
	    browse-url-chromium-arguments
	    (list url)))))

(function-put 'browse-url-chromium 'browse-url-browser-kind 'external)

(defun browse-url-chrome (url &optional _new-window)
  "Ask the Google Chrome WWW browser to load URL.
Default to the URL around or before point.  The strings in
variable `browse-url-chrome-arguments' are also passed to
Google Chrome.
The optional argument NEW-WINDOW is not used."
  (interactive (browse-url-interactive-arg "URL: "))
  (setq url (browse-url-encode-url url))
  (let* ((process-environment (browse-url-process-environment)))
    (apply #'start-process
	   (concat "google-chrome " url) nil
	   browse-url-chrome-program
	   (append
	    browse-url-chrome-arguments
	    (list url)))))

(function-put 'browse-url-chrome 'browse-url-browser-kind 'external)

(defun browse-url-epiphany (url &optional new-window)
  "Ask the GNOME Web (Epiphany) WWW browser to load URL.
Default to the URL around or before point.  The strings in variable
`browse-url-epiphany-arguments' are also passed to GNOME Web.

When called interactively, if variable `browse-url-new-window-flag' is
non-nil, load the document in a new GNOME Web window, otherwise use a
random existing one.  A non-nil interactive prefix argument reverses
the effect of `browse-url-new-window-flag'.

If `browse-url-epiphany-new-window-is-tab' is non-nil, then whenever a
document would otherwise be loaded in a new window, it is loaded in a
new tab in an existing window instead.

When called non-interactively, optional second argument NEW-WINDOW is
used instead of `browse-url-new-window-flag'."
  (interactive (browse-url-interactive-arg "URL: "))
  (setq url (browse-url-encode-url url))
  (let* ((process-environment (browse-url-process-environment))
         (process (apply #'start-process
			 (concat "epiphany " url)
			 nil
			 browse-url-epiphany-program
			 (append
			  browse-url-epiphany-arguments
                          (if (browse-url-maybe-new-window new-window)
			      (if browse-url-epiphany-new-window-is-tab
				  '("--new-tab")
				'("--new-window" "--noraise"))
                            '("--existing"))
                          (list url)))))
    (set-process-sentinel process
			  (lambda (process _change)
			    (browse-url-epiphany-sentinel process url)))))

(function-put 'browse-url-epiphany 'browse-url-browser-kind 'external)

(defun browse-url-epiphany-sentinel (process url)
  "Handle a change to the process communicating with GNOME Web (Epiphany)."
  (or (eq (process-exit-status process) 0)
      (let* ((process-environment (browse-url-process-environment)))
        ;; GNOME Web is not running - start it
	(message "Starting %s..." browse-url-epiphany-program)
	(apply #'start-process (concat "epiphany " url) nil
	       browse-url-epiphany-program
	       (append browse-url-epiphany-startup-arguments (list url))))))

(defun browse-url-qutebrowser-send (cmd)
  "Send CMD to Qutebrowser via IPC."
  (let* ((dir (xdg-runtime-dir))
         (sock (and dir (expand-file-name
                         (format "qutebrowser/ipc-%s" (md5 (user-login-name)))
                         dir))))
    (unless (file-exists-p sock)
      (error "No Qutebrowser IPC socket found"))
    (let ((proc
           (make-network-process
            :name "qutebrowser"
            :family 'local
            :service sock
            :coding 'utf-8)))
      (unwind-protect
          (process-send-string
           proc
           (concat
            (json-serialize `( :args [,cmd]
                               :target_arg :null
                               :protocol_version 1))
            "\n"))
        (delete-process proc)))))

(defun browse-url-qutebrowser (url &optional new-window)
  "Ask the Qutebrowser WWW browser to load URL.
Default to the URL around or before point.

When called interactively, if variable `browse-url-new-window-flag' is
non-nil, load the document in a new Qutebrowser window, otherwise use a
random existing one.  A non-nil interactive prefix argument reverses
the effect of `browse-url-new-window-flag'.

If `browse-url-qutebrowser-new-window-is-tab' is non-nil, then whenever a
document would otherwise be loaded in a new window, it is loaded in a
new tab in an existing window instead.

When called non-interactively, optional second argument NEW-WINDOW is
used instead of `browse-url-new-window-flag'."
  (interactive (browse-url-interactive-arg "URL: "))
  (let ((cmd (concat ":open "
                     (and (browse-url-maybe-new-window new-window)
                          (if browse-url-qutebrowser-new-window-is-tab
                              "-t " "-w "))
                     (browse-url-encode-url url))))
    (condition-case nil
        (browse-url-qutebrowser-send cmd)
      (error
       (apply #'start-process (concat "qutebrowser " url) nil
              browse-url-qutebrowser-program
              (append browse-url-qutebrowser-arguments (list cmd)))))))

(function-put 'browse-url-qutebrowser 'browse-url-browser-kind 'external)

(defvar url-handler-regexp)

;;;###autoload
(defun browse-url-webpositive (url &optional _new-window)
  "Ask the WebPositive WWW browser to load URL.
Default to the URL around or before point.
The optional argument NEW-WINDOW is not used."
  (interactive (browse-url-interactive-arg "URL: "))
  (setq url (browse-url-encode-url url))
  (let* ((process-environment (browse-url-process-environment)))
    (start-process (concat "WebPositive " url) nil "WebPositive" url)))

(function-put 'browse-url-webpositive 'browse-url-browser-kind 'external)

(declare-function haiku-roster-launch "haikuselect.c")

;;;###autoload
(defun browse-url-default-haiku-browser (url &optional _new-window)
  "Browse URL with the system default browser.
Default to the URL around or before point."
  (interactive (browse-url-interactive-arg "URL: "))
  (setq url (browse-url-encode-url url))
  (let* ((scheme (save-match-data
                   (if (string-match "\\(.+\\):/" url)
                       (match-string 1 url)
                     browse-url-default-scheme)))
         (mime (concat "application/x-vnd.Be.URL." scheme)))
    (haiku-roster-launch mime (vector url))))

(function-put 'browse-url-default-haiku-browser
              'browse-url-browser-kind 'external)

(defcustom browse-url-android-share nil
  "If non-nil, share URLs on Android systems instead of opening them.
When non-nil, `browse-url-default-android-browser' will try to
share the URL being browsed through programs such as mail clients
and instant messengers instead of opening it in a web browser."
  :type 'boolean
  :version "30.1")

(declare-function android-browse-url "../term/android-win")

;;;###autoload
(defun browse-url-default-android-browser (url &optional _new-window)
  "Browse URL with the system default browser.
If `browse-url-android-share' is non-nil, try to share URL using
an external program instead.  Default to the URL around or before
point."
  (interactive (browse-url-interactive-arg "URL: "))
  (unless browse-url-android-share
    ;; The URL shouldn't be encoded if it's being shared through
    ;; another program.
    (setq url (browse-url-encode-url url)))
  ;; Make sure the URL starts with an appropriate scheme.
  (unless (string-match "\\(.+\\):/" url)
    (setq url (concat browse-url-default-scheme "://" url)))
  (android-browse-url url browse-url-android-share))

(function-put 'browse-url-default-android-browser
              'browse-url-browser-kind 'external)

(declare-function x-gtk-launch-uri "pgtkfns.c")

;;;###autoload
(defun browse-url-default-gtk-browser (url &optional new-window)
  "Browse URL with GTK's idea of the default browser.
If the selected frame isn't a GTK frame, fall back to
`browse-url-default-browser'."
  (interactive (browse-url-interactive-arg "URL: "))
  (let ((frame (selected-frame)))
    (if (eq (frame-parameter frame 'window-system) 'pgtk)
        (x-gtk-launch-uri frame url)
      (let ((browse-url--inhibit-pgtk t))
        (browse-url-default-browser url new-window)))))

(function-put 'browse-url-default-gtk-browser
              'browse-url-browser-kind 'external)

;;;###autoload
(defun browse-url-emacs (url &optional same-window)
  "Ask Emacs to load URL into a buffer and show it in another window.
Optional argument SAME-WINDOW non-nil means show the URL in the
currently selected window instead."
  (interactive (browse-url-interactive-arg "URL: "))
  (require 'url-handlers)
  (let ((parsed (url-generic-parse-url url))
        (func (if same-window 'find-file 'find-file-other-window)))
    (if (equal (url-type parsed) "file")
        ;; It's a file; just open it.
        (let ((file (url-unhex-string (url-filename parsed))))
          (when-let* ((coding (browse-url--file-name-coding-system)))
            (setq file (decode-coding-string file 'utf-8)))
          ;; The local-part of file: URLs on Windows is supposed to
          ;; start with an extra slash.
          (when (eq system-type 'windows-nt)
            (setq file (replace-regexp-in-string
                        "\\`/\\([a-z]:\\)" "\\1" file)))
          (funcall func file))
      (let ((file-name-handler-alist
             (cons (cons url-handler-regexp 'url-file-handler)
                   file-name-handler-alist)))
        (funcall func url)))))

(function-put 'browse-url-emacs 'browse-url-browser-kind 'internal)

;; --- W3 ---

;; External.
(declare-function w3-fetch-other-window "ext:w3m" (&optional url))
(declare-function w3-fetch              "ext:w3m" (&optional url target))

;;;###autoload
(defun browse-url-w3 (url &optional new-window)
  "Ask the w3 WWW browser to load URL.
Default to the URL around or before point.

When called interactively, if variable `browse-url-new-window-flag' is
non-nil, load the document in a new window.  A non-nil interactive
prefix argument reverses the effect of `browse-url-new-window-flag'.

When called non-interactively, optional second argument NEW-WINDOW is
used instead of `browse-url-new-window-flag'."
  (declare (obsolete nil "29.1"))
  (interactive (browse-url-interactive-arg "W3 URL: "))
  (require 'w3)			; w3-fetch-other-window not autoloaded
  (if (browse-url-maybe-new-window new-window)
      (w3-fetch-other-window url)
    (w3-fetch url)))

(function-put 'browse-url-w3 'browse-url-browser-kind 'internal)

;; --- Lynx in an xterm ---

;;;###autoload
(defun browse-url-text-xterm (url &optional _new-window)
  ;; new-window ignored
  "Ask a text browser to load URL.
URL defaults to the URL around or before point.
This runs the text browser specified by `browse-url-text-browser'.
in an Xterm window using the Xterm program named by `browse-url-xterm-program'
with possible additional arguments `browse-url-xterm-args'.
The optional argument NEW-WINDOW is not used."
  (interactive (browse-url-interactive-arg "Text browser URL: "))
  (apply #'start-process `(,(concat browse-url-text-browser url)
			   nil ,browse-url-xterm-program
			   ,@browse-url-xterm-args "-e" ,browse-url-text-browser
			   ,url)))

(function-put 'browse-url-text-xterm 'browse-url-browser-kind 'external)

;; --- Lynx in an Emacs "term" window ---

(declare-function term-char-mode "term" ())
(declare-function term-send-down "term" ())
(declare-function term-send-string "term" (proc str))

;;;###autoload
(defun browse-url-text-emacs (url &optional new-buffer)
  "Ask a text browser to load URL.
URL defaults to the URL around or before point.
This runs the text browser specified by `browse-url-text-browser'.
With a prefix argument, it runs a new browser process in a new buffer.

When called interactively, if variable `browse-url-new-window-flag' is
non-nil, load the document in a new browser process in a new term window,
otherwise use any existing one.  A non-nil interactive prefix argument
reverses the effect of `browse-url-new-window-flag'.

When called non-interactively, optional second argument NEW-WINDOW is
used instead of `browse-url-new-window-flag'."
  (interactive (browse-url-interactive-arg "Text browser URL: "))
  (let* ((system-uses-terminfo t)     ; Lynx uses terminfo
	 ;; (term-term-name "vt100") ; ??
	 (buf (get-buffer "*text browser*"))
	 (proc (and buf (get-buffer-process buf)))
	 (n browse-url-text-input-attempts))
    (require 'term)
    (if (and (browse-url-maybe-new-window new-buffer) buf)
	;; Rename away the OLD buffer.  This isn't very polite, but
	;; term insists on working in a buffer named *lynx* and would
	;; choke on *lynx*<1>
	(progn (set-buffer buf)
	       (rename-uniquely)))
    (if (or (browse-url-maybe-new-window new-buffer)
	    (not buf)
	    (not proc)
	    (not (memq (process-status proc) '(run stop))))
	;; start a new text browser
	(progn
          (setq buf
                (apply #'make-term
                       `(,browse-url-text-browser
			 ,browse-url-text-browser
			 nil ,@browse-url-text-emacs-args
			 ,url)))
          (switch-to-buffer buf)
          (term-char-mode)
          (set-process-sentinel
           (get-buffer-process buf)
           ;; Don't leave around a dead one (especially because of its
           ;; munged keymap.)
           (lambda (process _event)
             (if (not (memq (process-status process) '(run stop)))
                 (let ((buf (process-buffer process)))
                   (if buf (kill-buffer buf)))))))
      ;; Send the url to the text browser in the old buffer
      (let ((win (get-buffer-window buf t)))
	(if win
	    (select-window win)
	  (switch-to-buffer buf)))
      (if (eq (following-char) ?_)
	  (cond ((eq browse-url-text-input-field 'warn)
		 (error "Please move out of the input field first"))
		((eq browse-url-text-input-field 'avoid)
		 (while (and (eq (following-char) ?_) (> n 0))
		   (term-send-down)	; down arrow
		   (sit-for browse-url-text-input-delay))
		 (if (eq (following-char) ?_)
		     (error "Cannot move out of the input field, sorry")))))
      (term-send-string proc (concat "g"    ; goto
				     "\C-u" ; kill default url
				     url
				     "\r")))))

(function-put 'browse-url-text-emacs 'browse-url-browser-kind 'internal)

;; --- irc ---

;;;###autoload
(defun browse-url-irc (url &rest _)
  "Call `url-irc' directly after parsing URL.
This function is a fit for options like `gnus-button-alist'."
  (url-irc (url-generic-parse-url url)))

(function-put 'browse-url-irc 'browse-url-browser-kind 'internal)

;; --- mailto ---

(autoload 'rfc6068-parse-mailto-url "rfc6068")

;;;###autoload
(defun browse-url-mail (url &optional new-window)
  "Open a new mail message buffer within Emacs for the RFC 2368 URL.
Default to using the mailto: URL around or before point as the
recipient's address.  Supplying a non-nil interactive prefix argument
will cause the mail to be composed in another window rather than the
current one.

When called interactively, if variable `browse-url-new-window-flag' is
non-nil use `compose-mail-other-window', otherwise `compose-mail'.  A
non-nil interactive prefix argument reverses the effect of
`browse-url-new-window-flag'.

When called non-interactively, optional second argument NEW-WINDOW is
used instead of `browse-url-new-window-flag'."
  (interactive (browse-url-interactive-arg "Mailto URL: "))
  (save-excursion
    (let* ((alist (rfc6068-parse-mailto-url url))
	   (to (assoc "To" alist))
	   (subject (assoc "Subject" alist))
	   (body (assoc "Body" alist))
	   (rest (delq to (delq subject (delq body alist))))
	   (to (cdr to))
	   (subject (cdr subject))
	   (body (cdr body))
	   (mail-citation-hook (unless body mail-citation-hook)))
      (if (browse-url-maybe-new-window new-window)
	  (compose-mail-other-window to subject rest nil
				     (list 'insert-buffer (current-buffer)))
	(compose-mail to subject rest nil nil
		      (list 'insert-buffer (current-buffer))))
      (when body
	(goto-char (point-min))
	(unless (or (search-forward (concat "\n" mail-header-separator "\n")
				    nil 'move)
		    (bolp))
	  (insert "\n"))
	(goto-char (prog1
		       (point)
		     (insert (string-replace "\r\n" "\n" body))
		     (unless (bolp)
		       (insert "\n"))))))))

(function-put 'browse-url-mail 'browse-url-browser-kind 'internal)

;; --- man ---

(defvar manual-program)

(defun browse-url-man (url &optional _new-window)
  "Open a man page."
  (interactive (browse-url-interactive-arg "Man page URL: "))
  (require 'man)
  (setq url (replace-regexp-in-string "\\`man:" "" url))
  (cond
   ((executable-find manual-program) (man url))
   (t (woman (replace-regexp-in-string "([[:alnum:]]+)" "" url)))))

(function-put 'browse-url-man 'browse-url-browser-kind 'internal)

;; --- Random browser ---

;;;###autoload
(defun browse-url-generic (url &optional _new-window)
  ;; new-window ignored
  "Ask the WWW browser defined by `browse-url-generic-program' to load URL.
Default to the URL around or before point.  A fresh copy of the
browser is started up in a new process with possible additional arguments
`browse-url-generic-args'.  This is appropriate for browsers which
don't offer a form of remote control."
  (interactive (browse-url-interactive-arg "URL: "))
  (if (not browse-url-generic-program)
      (error "No browser defined (`browse-url-generic-program')"))
  (apply #'call-process browse-url-generic-program nil
	 0 nil
	 (append browse-url-generic-args (list url))))

(function-put 'browse-url-generic 'browse-url-browser-kind 'external)

;;;###autoload
(defun browse-url-kde (url &optional _new-window)
  "Ask the KDE WWW browser to load URL.
Default to the URL around or before point.
The optional argument NEW-WINDOW is not used."
  (interactive (browse-url-interactive-arg "KDE URL: "))
  (message "Sending URL to KDE...")
  (apply #'start-process (concat "KDE " url) nil browse-url-kde-program
	 (append browse-url-kde-args (list url))))

(function-put 'browse-url-kde 'browse-url-browser-kind 'external)

(defun browse-url-elinks-new-window (url)
  "Ask the Elinks WWW browser to load URL in a new window."
  (let ((process-environment (browse-url-process-environment)))
    (apply #'start-process
	   (append (list (concat "elinks:" url)
			 nil)
		   browse-url-elinks-wrapper
		   (list "elinks" url)))))

(function-put 'browse-url-elinks-new-window 'browse-url-browser-kind
              'external)

;;;###autoload
(defun browse-url-elinks (url &optional new-window)
  "Ask the Elinks WWW browser to load URL.
Default to the URL around the point.

The document is loaded in a new tab of a running Elinks or, if
none yet running, a newly started instance.

The Elinks command will be prepended by the program+arguments
from `browse-url-elinks-wrapper'."
  (interactive (browse-url-interactive-arg "URL: "))
  (setq url (browse-url-encode-url url))
  (if new-window
      (browse-url-elinks-new-window url)
    (let ((process-environment (browse-url-process-environment))
	  (elinks-ping-process (start-process "elinks-ping" nil
					      "elinks" "-remote" "ping()")))
      (set-process-sentinel elinks-ping-process
			    (lambda (process _change)
			      (browse-url-elinks-sentinel process url))))))

(function-put 'browse-url-elinks 'browse-url-browser-kind 'external)

(defun browse-url-elinks-sentinel (process url)
  "Determines if Elinks is running or a new one has to be started."
  ;; Try to determine if an instance is running or if we have to
  ;; create a new one.
  (pcase (process-exit-status process)
    (5
     ;; No instance, start a new one.
     (browse-url-elinks-new-window url))
    (0
     ;; Found an instance, open URL in new tab.
     (let ((process-environment (browse-url-process-environment)))
       (start-process (concat "elinks:" url) nil
                      "elinks" "-remote"
                      (concat "openURL(\"" url "\",new-tab)"))))
    (exit-status
     (error "Unrecognized exit-code %d of process `elinks'"
            exit-status))))

;;; Adding buttons to a buffer to call `browse-url' when you hit them.

(defvar-keymap browse-url-button-map
  :doc "The keymap used for `browse-url' buttons."
  "RET"       (keymap-read-only-bind #'browse-url-button-open)
  "C-c RET"   #'browse-url-button-open
  "<mouse-2>" #'browse-url-button-open
  "w"         #'browse-url-button-copy)

(defface browse-url-button
  '((t :inherit link))
  "Face for `browse-url' buttons (i.e., links)."
  :version "27.1")

(defun browse-url-add-buttons ()
  "Add clickable buttons to the text following point in the current buffer.
Everything that matches `browse-url-button-regexp' will be made
clickable and will use `browse-url' to open the URLs in question."
  (let ((inhibit-read-only t))
    (save-excursion
      (while (re-search-forward browse-url-button-regexp nil t)
        (add-text-properties (match-beginning 0)
                             (match-end 0)
                             `(help-echo "Open the URL under point"
                                         keymap ,browse-url-button-map
                                         face browse-url-button
                                         button t
                                         category browse-url
                                         browse-url-data ,(match-string 0)))))))

;;;###autoload
(defun browse-url-button-open (&optional secondary mouse-event)
  "Follow the link under point using `browse-url'.
If SECONDARY (the prefix if used interactively), open with the
secondary browser instead of the default one."
  (interactive (list current-prefix-arg last-nonmenu-event))
  (mouse-set-point mouse-event)
  (let ((url (get-text-property (point) 'browse-url-data)))
    (unless url
      (error "No URL under point"))
    (let ((browse-url-browser-function
           (if secondary
               browse-url-secondary-browser-function
             browse-url-browser-function)))
      (browse-url url))))

;;;###autoload
(defun browse-url-button-open-url (url)
  "Open URL using `browse-url'.
If `current-prefix-arg' is non-nil, use
`browse-url-secondary-browser-function' instead."
  (let ((browse-url-browser-function
         (if current-prefix-arg
             browse-url-secondary-browser-function
           browse-url-browser-function)))
    (browse-url url)))

(defun browse-url-button-copy ()
  "Copy the URL under point."
  (interactive)
  (let ((url (get-text-property (point) 'browse-url-data)))
    (unless url
      (error "No URL under point"))
    (kill-new url)
    (message "Copied %s" url)))

(provide 'browse-url)

;;; browse-url.el ends here
