;;; ffap.el --- find file (or url) at point
;;
;; Copyright (C) 1995, 96, 97, 2000  Free Software Foundation, Inc.
;;
;; Author: Michelangelo Grigni <mic@mathcs.emory.edu>
;; Created: 29 Mar 1993
;; Keywords: files, hypermedia, matching, mouse, convenience
;; X-URL: ftp://ftp.mathcs.emory.edu/pub/mic/emacs/

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
;;
;; Command find-file-at-point replaces find-file.  With a prefix, it
;; behaves exactly like find-file.  Without a prefix, it first tries
;; to guess a default file or URL from the text around the point
;; (`ffap-require-prefix' swaps these behaviors).  This is useful for
;; following references in situations such as mail or news buffers,
;; README's, MANIFEST's, and so on.  Submit bugs or suggestions with
;; M-x ffap-bug.
;;
;; For the default installation, add this line to your .emacs file:
;;
;; (ffap-bindings)                      ; do default key bindings
;;
;; ffap-bindings makes the following global key bindings:
;;
;; C-x C-f       find-file-at-point (abbreviated as ffap)
;; C-x 4 f       ffap-other-window
;; C-x 5 f       ffap-other-frame
;; S-mouse-3     ffap-at-mouse
;; C-S-mouse-3   ffap-menu
;;
;; ffap-bindings also adds hooks to make the following local bindings
;; in vm, gnus, and rmail:
;;
;; M-l         ffap-next, or ffap-gnus-next in gnus (l == "link")
;; M-m         ffap-menu, or ffap-gnus-menu in gnus (m == "menu")
;;
;; If you do not like these bindings, modify the variable
;; `ffap-bindings', or write your own.
;;
;; If you use ange-ftp, browse-url, complete, efs, or w3, it is best
;; to load or autoload them before ffap.  If you use ff-paths, load it
;; afterwards.  Try apropos {C-h a ffap RET} to get a list of the many
;; option variables.  In particular, if ffap is slow, try these:
;;
;; (setq ffap-alist nil)                ; faster, dumber prompting
;; (setq ffap-machine-p-known 'accept)  ; no pinging
;; (setq ffap-url-regexp nil)           ; disable URL features in ffap
;;
;; ffap uses `browse-url' (if found, else `w3-fetch') to fetch URL's.
;; For a hairier `ffap-url-fetcher', try ffap-url.el (same ftp site).
;; Also, you can add `ffap-menu-rescan' to various hooks to fontify
;; the file and URL references within a buffer.  


;;; Change Log:
;;
;; The History and Contributors moved to ffap.LOG (same ftp site),
;; which also has some old examples and commentary from ffap 1.5.


;;; Todo list:
;; * use kpsewhich
;; * let "/path/file#key" jump to key (tag or regexp) in /path/file
;; * find file of symbol if TAGS is loaded (like above)
;; * break long menus into multiple panes (like imenu?)
;; * notice node in "(dired)Virtual Dired" (quotes, parentheses, whitespace)
;; * notice "machine.dom blah blah blah path/file" (how?)
;; * as w3 becomes standard, rewrite to rely more on its functions
;; * regexp options for ffap-string-at-point, like font-lock (MCOOK)
;; * v19: could replace `ffap-locate-file' with a quieter `locate-library'
;; * handle "$(VAR)" in Makefiles
;; * use the font-lock machinery


;;; Code:

(provide 'ffap)

;; Please do not delete this variable, it is checked in bug reports.
(defconst ffap-version "1.9-fsf <97/06/25 13:21:41 mic>"
  "The version of ffap: \"Major.Minor-Build <Timestamp>\"")


(defgroup ffap nil
  "Find file or URL at point."
  :link '(url-link :tag "URL" "ftp://ftp.mathcs.emory.edu/pub/mic/emacs/")
  :group 'matching
  :group 'convenience)

;; The code is organized in pages, separated by formfeed characters.
;; See the next two pages for standard customization ideas.


;;; User Variables:

(defun ffap-soft-value (name &optional default)
  "Return value of symbol with NAME, if it is interned.
Otherwise return nil (or the optional DEFAULT value)."
  ;; Bug: (ffap-soft-value "nil" 5) --> 5
  (let ((sym (intern-soft name)))
    (if (and sym (boundp sym)) (symbol-value sym) default)))

(defcustom ffap-ftp-regexp
  ;; This used to test for ange-ftp or efs being present, but it should be
  ;; harmless (and simpler) to give it this value unconditionally.
  "\\`/[^/:]+:"
  "*Paths matching this regexp are treated as remote ftp paths by ffap.
If nil, ffap neither recognizes nor generates such paths."
  :type '(choice (const :tag "Disable" nil)
		 (const :tag "Standard" "\\`/[^/:]+:")
		 regexp)
  :group 'ffap)

(defcustom ffap-url-unwrap-local t
  "*If non-nil, convert `file:' url to local path before prompting."
  :type 'boolean
  :group 'ffap)

(defcustom ffap-url-unwrap-remote t
  "*If non-nil, convert `ftp:' url to remote path before prompting.
This is ignored if `ffap-ftp-regexp' is nil."
  :type 'boolean
  :group 'ffap)

(defcustom ffap-ftp-default-user "anonymous"
  "*User name in ftp paths generated by `ffap-host-to-path'.
Note this name may be omitted if it equals the default
\(either `efs-default-user' or `ange-ftp-default-user'\)."
  :type 'string
  :group 'ffap)

(defcustom ffap-rfs-regexp
  ;; Remote file access built into file system?  HP rfa or Andrew afs:
  "\\`/\\(afs\\|net\\)/."
  ;; afs only: (and (file-exists-p "/afs") "\\`/afs/.")
  "*Matching paths are treated as remote.  nil to disable."
  :type 'regexp
  :group 'ffap)

(defvar ffap-url-regexp
  ;; Could just use `url-nonrelative-link' of w3, if loaded.
  ;; This regexp is not exhaustive, it just matches common cases.
  (concat
   "\\`\\("
   "news\\(post\\)?:\\|mailto:\\|file:" ; no host ok
   "\\|"
   "\\(ftp\\|https?\\|telnet\\|gopher\\|www\\|wais\\)://" ; needs host
   "\\)."				; require one more character
   )
   "Regexp matching URL's.  nil to disable URL features in ffap.")

(defcustom ffap-foo-at-bar-prefix "mailto"
  "*Presumed URL prefix type of strings like \"<foo.9z@bar>\".
Sensible values are nil, \"news\", or \"mailto\"."
  :type '(choice (const "mailto")
		 (const "news")
		 (const :tag "Disable" nil)
		 ;; string -- possible, but not really useful
		 )
  :group 'ffap)


;;; Peanut Gallery (More User Variables):
;;
;; Users of ffap occasionally suggest new features.  If I consider
;; those features interesting but not clear winners (a matter of
;; personal taste) I try to leave options to enable them.  Read
;; through this section for features that you like, put an appropriate
;; enabler in your .emacs file.

(defcustom ffap-dired-wildcards nil
  ;; Suggestion from RHOGEE, 07 Jul 1994.  Disabled, dired is still
  ;; available by "C-x C-d <pattern>", and valid filenames may
  ;; sometimes contain wildcard characters.
  "*A regexp matching filename wildcard characters, or nil.
If `find-file-at-point' gets a filename matching this pattern,
it passes it on to `dired' instead of `find-file'."
  :type '(choice (const :tag "Disable" nil)
		 (const :tag "Enable" "[*?][^/]*\\'")
		 ;; regexp -- probably not useful
		 )
  :group 'ffap)

(defcustom ffap-newfile-prompt nil
  ;; Suggestion from RHOGEE, 11 Jul 1994.  Disabled, I think this is
  ;; better handled by `find-file-not-found-hooks'.
  "*Whether `find-file-at-point' prompts about a nonexistent file."
  :type 'boolean
  :group 'ffap)

(defcustom ffap-require-prefix nil
  ;; Suggestion from RHOGEE, 20 Oct 1994.
  "*If set, reverses the prefix argument to `find-file-at-point'.
This is nil so neophytes notice ffap.  Experts may prefer to disable
ffap most of the time."
  :type 'boolean
  :group 'ffap)

(defcustom ffap-file-finder 'find-file
  "*The command called by `find-file-at-point' to find a file."
  :type 'function
  :group 'ffap)
(put 'ffap-file-finder 'risky-local-variable t)

(defcustom ffap-url-fetcher
  (if (fboundp 'browse-url)
      'browse-url			; rely on browse-url-browser-function
    'w3-fetch)
  ;; Remote control references:
  ;; http://www.ncsa.uiuc.edu/SDG/Software/XMosaic/remote-control.html
  ;; http://home.netscape.com/newsref/std/x-remote.html
  "*A function of one argument, called by ffap to fetch an URL.
Reasonable choices are `w3-fetch' or a `browse-url-*' function.
For a fancy alternative, get `ffap-url.el'."
  :type '(choice (const w3-fetch)
		 (const browse-url)	; in recent versions of browse-url
		 (const browse-url-netscape)
		 (const browse-url-mosaic)
		 function)
  :group 'ffap)
(put 'ffap-url-fetcher 'risky-local-variable t)


;;; Compatibility:
;;
;; This version of ffap supports Emacs 20 only, see the ftp site
;; for a more general version.  The following functions are necessary
;; "leftovers" from the more general version.

(defun ffap-mouse-event nil		; current mouse event, or nil
  (and (listp last-nonmenu-event) last-nonmenu-event))
(defun ffap-event-buffer (event)
  (window-buffer (car (event-start event))))


;;; Find Next Thing in buffer (`ffap-next'):
;;
;; Original ffap-next-url (URL's only) from RPECK 30 Mar 1995.  Since
;; then, broke it up into ffap-next-guess (noninteractive) and
;; ffap-next (a command).  It now work on files as well as url's.

(defcustom ffap-next-regexp
  ;; If you want ffap-next to find URL's only, try this:
  ;; (and ffap-url-regexp (string-match "\\\\`" ffap-url-regexp)
  ;;	  (concat "\\<" (substring ffap-url-regexp 2))))
  ;;
  ;; It pays to put a big fancy regexp here, since ffap-guesser is
  ;; much more time-consuming than regexp searching:
  "[/:.~a-zA-Z]/\\|@[a-zA-Z][-a-zA-Z0-9]*\\."
  "*Regular expression governing movements of `ffap-next'."
  :type 'regexp
  :group 'ffap)

(defvar ffap-next-guess nil
  "Last value returned by `ffap-next-guess'.")

(defvar ffap-string-at-point-region '(1 1)
  "List (BEG END), last region returned by `ffap-string-at-point'.")

(defun ffap-next-guess (&optional back lim)
  "Move point to next file or URL, and return it as a string.
If nothing is found, leave point at limit and return nil.
Optional BACK argument makes search backwards.
Optional LIM argument limits the search.
Only considers strings that match `ffap-next-regexp'."
  (or lim (setq lim (if back (point-min) (point-max))))
  (let (guess)
    (while (not (or guess (eq (point) lim)))
      (funcall (if back 're-search-backward 're-search-forward)
	       ffap-next-regexp lim 'move)
      (setq guess (ffap-guesser)))
    ;; Go to end, so we do not get same guess twice:
    (goto-char (nth (if back 0 1) ffap-string-at-point-region))
    (setq ffap-next-guess guess)))

;;;###autoload
(defun ffap-next (&optional back wrap)
  "Search buffer for next file or URL, and run ffap.
Optional argument BACK says to search backwards.
Optional argument WRAP says to try wrapping around if necessary.
Interactively: use a single prefix to search backwards,
double prefix to wrap forward, triple to wrap backwards.
Actual search is done by `ffap-next-guess'."
  (interactive
   (cdr (assq (prefix-numeric-value current-prefix-arg)
	      '((1) (4 t) (16 nil t) (64 t t)))))
  (let ((pt (point))
	(guess (ffap-next-guess back)))
    ;; Try wraparound if necessary:
    (and (not guess) wrap
	 (goto-char (if back (point-max) (point-min)))
	 (setq guess (ffap-next-guess back pt)))
    (if guess
	(progn
	  (sit-for 0)			; display point movement
	  (find-file-at-point (ffap-prompter guess)))
      (goto-char pt)			; restore point
      (message "No %sfiles or URL's found"
	       (if wrap "" "more ")))))

(defun ffap-next-url (&optional back wrap)
  "Like `ffap-next', but search with `ffap-url-regexp'."
  (interactive)
  (let ((ffap-next-regexp ffap-url-regexp))
    (if (interactive-p)
	(call-interactively 'ffap-next)
      (ffap-next back wrap))))


;;; Machines (`ffap-machine-p'):

;; I cannot decide a "best" strategy here, so these are variables.  In
;; particular, if `Pinging...' is broken or takes too long on your
;; machine, try setting these all to accept or reject.
(defcustom ffap-machine-p-local 'reject	; this happens often
  "*What `ffap-machine-p' does with hostnames that have no domain.
Value should be a symbol, one of `ping', `accept', and `reject'."
  :type '(choice (const ping)
		 (const accept)
		 (const reject))
  :group 'ffap)
(defcustom ffap-machine-p-known 'ping	; `accept' for higher speed
  "*What `ffap-machine-p' does with hostnames that have a known domain.
Value should be a symbol, one of `ping', `accept', and `reject'.
See `mail-extr.el' for the known domains."
  :type '(choice (const ping)
		 (const accept)
		 (const reject))
  :group 'ffap)
(defcustom ffap-machine-p-unknown 'reject
  "*What `ffap-machine-p' does with hostnames that have an unknown domain.
Value should be a symbol, one of `ping', `accept', and `reject'.
See `mail-extr.el' for the known domains."
  :type '(choice (const ping)
		 (const accept)
		 (const reject))
  :group 'ffap)

(defun ffap-what-domain (domain)
  ;; Like what-domain in mail-extr.el, returns string or nil.
  (require 'mail-extr)
  (let ((ob (or (ffap-soft-value "mail-extr-all-top-level-domains")
		(ffap-soft-value "all-top-level-domains")))) ; XEmacs
    (and ob (get (intern-soft (downcase domain) ob) 'domain-name))))

(defun ffap-machine-p (host &optional service quiet strategy)
  "Decide whether HOST is the name of a real, reachable machine.
Depending on the domain (none, known, or unknown), follow the strategy
named by the variable `ffap-machine-p-local', `ffap-machine-p-known',
or `ffap-machine-p-unknown'.  Pinging uses `open-network-stream'.
Optional SERVICE specifies the port used \(default \"discard\"\).
Optional QUIET flag suppresses the \"Pinging...\" message.
Optional STRATEGY overrides the three variables above.
Returned values:
 t      means that HOST answered.
'accept means the relevant variable told us to accept.
\"mesg\"  means HOST exists, but does not respond for some reason."
  ;; Try some (Emory local):
  ;; (ffap-machine-p "ftp" nil nil 'ping)
  ;; (ffap-machine-p "nonesuch" nil nil 'ping)
  ;; (ffap-machine-p "ftp.mathcs.emory.edu" nil nil 'ping)
  ;; (ffap-machine-p "mathcs" 5678 nil 'ping)
  ;; (ffap-machine-p "foo.bonk" nil nil 'ping)
  ;; (ffap-machine-p "foo.bonk.com" nil nil 'ping)
  (if (or (string-match "[^-a-zA-Z0-9.]" host) ; Illegal chars (?)
	  (not (string-match "[^0-9]" host))) ; 1: a number? 2: quick reject
      nil
    (let* ((domain
	    (and (string-match "\\.[^.]*$" host)
		 (downcase (substring host (1+ (match-beginning 0))))))
	   (what-domain (if domain (ffap-what-domain domain) "Local")))
      (or strategy
	  (setq strategy
		(cond ((not domain) ffap-machine-p-local)
		      ((not what-domain) ffap-machine-p-unknown)
		      (t ffap-machine-p-known))))
      (cond
       ((eq strategy 'accept) 'accept)
       ((eq strategy 'reject) nil)
       ((not (fboundp 'open-network-stream)) nil)
       ;; assume (eq strategy 'ping)
       (t
	(or quiet
	    (if (stringp what-domain)
		(message "Pinging %s (%s)..." host what-domain)
	      (message "Pinging %s ..." host)))
	(condition-case error
	    (progn
	      (delete-process
	       (open-network-stream
		"ffap-machine-p" nil host (or service "discard")))
	      t)
	  (error
	   (let ((mesg (car (cdr error))))
	     (cond
	      ;; v18:
	      ((string-match "^Unknown host" mesg) nil)
	      ((string-match "not responding$" mesg) mesg)
	      ;; v19:
	      ;; (file-error "connection failed" "permission denied"
	      ;;             "nonesuch" "ffap-machine-p")
	      ;; (file-error "connection failed" "host is unreachable"
	      ;;	     "gopher.house.gov" "ffap-machine-p")
	      ;; (file-error "connection failed" "address already in use"
	      ;;	     "ftp.uu.net" "ffap-machine-p")
	      ((equal mesg "connection failed")
	       (if (equal (nth 2 error) "permission denied")
		   nil			; host does not exist
		 ;; Other errors mean the host exists:
		 (nth 2 error)))
	      ;; Could be "Unknown service":
	      (t (signal (car error) (cdr error))))))))))))


;;; Possibly Remote Resources:

(defun ffap-replace-path-component (fullname name)
  "In remote FULLNAME, replace path with NAME.  May return nil."
  ;; Use ange-ftp or efs if loaded, but do not load them otherwise.
  (let (found)
    (mapcar
     (function (lambda (sym) (and (fboundp sym) (setq found sym))))
     '(
       efs-replace-path-component
       ange-ftp-replace-path-component
       ange-ftp-replace-name-component
       ))
    (and found
	 (fset 'ffap-replace-path-component found)
	 (funcall found fullname name))))
;; (ffap-replace-path-component "/who@foo.com:/whatever" "/new")

(defun ffap-file-suffix (file)
  "Return trailing `.foo' suffix of FILE, or nil if none."
  (let ((pos (string-match "\\.[^./]*\\'" file)))
    (and pos (substring file pos nil))))

(defvar ffap-compression-suffixes '(".gz" ".Z")	; .z is mostly dead
  "List of suffixes tried by `ffap-file-exists-string'.")

(defun ffap-file-exists-string (file &optional nomodify)
  ;; Early jka-compr versions modified file-exists-p to return the
  ;; filename, maybe modified by adding a suffix like ".gz".  That
  ;; broke the interface of file-exists-p, so it was later dropped.
  ;; Here we document and simulate the old behavior.
  "Return FILE (maybe modified) if the file exists, else nil.
When using jka-compr (a.k.a. `auto-compression-mode'), the returned
name may have a suffix added from `ffap-compression-suffixes'.
The optional NOMODIFY argument suppresses the extra search."
  (cond
   ((not file) nil)			; quietly reject nil
   ((file-exists-p file) file)		; try unmodified first
   ;; three reasons to suppress search:
   (nomodify nil)
   ((not (rassq 'jka-compr-handler file-name-handler-alist)) nil)
   ((member (ffap-file-suffix file) ffap-compression-suffixes) nil)
   (t					; ok, do the search
    (let ((list ffap-compression-suffixes) try ret)
      (while list
	(if (file-exists-p (setq try (concat file (car list))))
	    (setq ret try list nil)
	  (setq list (cdr list))))
      ret))))

(defun ffap-file-remote-p (filename)
  "If FILENAME looks remote, return it (maybe slightly improved)."
  ;; (ffap-file-remote-p "/user@foo.bar.com:/pub")
  ;; (ffap-file-remote-p "/cssun.mathcs.emory.edu://path")
  ;; (ffap-file-remote-p "/ffap.el:80")
  (or (and ffap-ftp-regexp
	   (string-match ffap-ftp-regexp filename)
	   ;; Convert "/host.com://path" to "/host:/path", to handle a dieing
	   ;; practice of advertising ftp paths as "host.dom://path".
	   (if (string-match "//" filename)
	       ;; (replace-match "/" nil nil filename)
	       (concat (substring filename 0 (1+ (match-beginning 0)))
		       (substring filename (match-end 0)))
	     filename))
      (and ffap-rfs-regexp
	   (string-match ffap-rfs-regexp filename)
	   filename)))

(defun ffap-machine-at-point nil
  "Return machine name at point if it exists, or nil."
  (let ((mach (ffap-string-at-point 'machine)))
    (and (ffap-machine-p mach) mach)))

(defsubst ffap-host-to-path (host)
  "Convert HOST to something like \"/USER@HOST:\" or \"/HOST:\".
Looks at `ffap-ftp-default-user', returns \"\" for \"localhost\"."
  (if (equal host "localhost")
      ""
    (let ((user ffap-ftp-default-user))
      ;; Avoid including the user if it is same as default:
      (if (or (equal user (ffap-soft-value "ange-ftp-default-user"))
	      (equal user (ffap-soft-value "efs-default-user")))
	  (setq user nil))
      (concat "/" user (and user "@") host ":"))))

(defun ffap-fixup-machine (mach)
  ;; Convert a hostname into an url, an ftp path, or nil.
  (cond
   ((not (and ffap-url-regexp (stringp mach))) nil)
   ;; gopher.well.com
   ((string-match "\\`gopher[-.]" mach)	; or "info"?
    (concat "gopher://" mach "/"))
   ;; www.ncsa.uiuc.edu
   ((and (string-match "\\`w\\(ww\\|eb\\)[-.]" mach))
    (concat "http://" mach "/"))
   ;; More cases?  Maybe "telnet:" for archie?
   (ffap-ftp-regexp (ffap-host-to-path mach))
   ))

(defvar ffap-newsgroup-regexp "^[a-z]+\\.[-+a-z_0-9.]+$"
  "Strings not matching this fail `ffap-newsgroup-p'.")
(defvar ffap-newsgroup-heads		; entirely inadequate
  '("alt" "comp" "gnu" "misc" "news" "sci" "soc" "talk")
  "Used by `ffap-newsgroup-p' if gnus is not running.")

(defun ffap-newsgroup-p (string)
  "Return STRING if it looks like a newsgroup name, else nil."
  (and
   (string-match ffap-newsgroup-regexp string)
   (let ((htbs '(gnus-active-hashtb gnus-newsrc-hashtb gnus-killed-hashtb))
	 (heads ffap-newsgroup-heads)
	 htb ret)
     (while htbs
       (setq htb (car htbs) htbs (cdr htbs))
       (condition-case nil
	   (progn
	     ;; errs: htb symbol may be unbound, or not a hash-table.
	     ;; gnus-gethash is just a macro for intern-soft.
	     (and (symbol-value htb)
		  (intern-soft string (symbol-value htb))
		  (setq ret string htbs nil))
	     ;; If we made it this far, gnus is running, so ignore "heads":
	     (setq heads nil))
	 (error nil)))
     (or ret (not heads)
	 (let ((head (string-match "\\`\\([a-z]+\\)\\." string)))
	   (and head (setq head (substring string 0 (match-end 1)))
		(member head heads)
		(setq ret string))))
     ;; Is there ever a need to modify string as a newsgroup name?
     ret)))

(defsubst ffap-url-p (string)
  "If STRING looks like an url, return it (maybe improved), else nil."
  (let ((case-fold-search t))
    (and ffap-url-regexp (string-match ffap-url-regexp string)
	 ;; I lied, no improvement:
	 string)))

;; Broke these out of ffap-fixup-url, for use of ffap-url package.
(defsubst ffap-url-unwrap-local (url)
  "Return URL as a local file, or nil.  Ignores `ffap-url-regexp'."
  (and (string-match "\\`\\(file\\|ftp\\):/?\\([^/]\\|\\'\\)" url)
       (substring url (1+ (match-end 1)))))
(defsubst ffap-url-unwrap-remote (url)
  "Return URL as a remote file, or nil.  Ignores `ffap-url-regexp'."
  (and (string-match "\\`\\(ftp\\|file\\)://\\([^:/]+\\):?\\(/.*\\)" url)
       (concat
	(ffap-host-to-path (substring url (match-beginning 2) (match-end 2)))
	(substring url (match-beginning 3) (match-end 3)))))
;; Test: (ffap-url-unwrap-remote "ftp://foo.com/bar.boz")

(defun ffap-fixup-url (url)
  "Clean up URL and return it, maybe as a file name."
  (cond
   ((not (stringp url)) nil)
   ((and ffap-url-unwrap-local (ffap-url-unwrap-local url)))
   ((and ffap-url-unwrap-remote ffap-ftp-regexp
	 (ffap-url-unwrap-remote url)))
   ((fboundp 'url-normalize-url)	; may autoload url (part of w3)
    (url-normalize-url url))
   (url)))


;;; Path Handling:
;;
;; The upcoming ffap-alist actions need various utilities to prepare
;; and search paths of directories.  Too many features here.

;; (defun ffap-last (l) (while (cdr l) (setq l (cdr l))) l)
;; (defun ffap-splice (func inlist)
;;  "Equivalent to (apply 'nconc (mapcar FUNC INLIST)), but less consing."
;;  (let* ((head (cons 17 nil)) (last head))
;;    (while inlist
;;      (setcdr last (funcall func (car inlist)))
;;      (setq last (ffap-last last) inlist (cdr inlist)))
;;    (cdr head)))

(defun ffap-list-env (env &optional empty)
  "Return a list of strings parsed from environment variable ENV.
Optional EMPTY is the default list if \(getenv ENV\) is undefined, and
also is substituted for the first empty-string component, if there is one.
Uses `path-separator' to separate the path into substrings."
  ;; We cannot use parse-colon-path (files.el), since it kills
  ;; "//" entries using file-name-as-directory.
  ;; Similar: dired-split, TeX-split-string, and RHOGEE's psg-list-env
  ;; in ff-paths and bib-cite.  The EMPTY arg may help mimic kpathsea.
  (if (or empty (getenv env))		; should return something
      (let ((start 0) match dir ret)
	(setq env (concat (getenv env) path-separator))
	(while (setq match (string-match path-separator env start))
	  (setq dir (substring env start match) start (1+ match))
	  ;;(and (file-directory-p dir) (not (member dir ret)) ...)
	  (setq ret (cons dir ret)))
	(setq ret (nreverse ret))
	(and empty (setq match (member "" ret))
	     (progn			; allow string or list here
	       (setcdr match (append (cdr-safe empty) (cdr match)))
	       (setcar match (or (car-safe empty) empty))))
	ret)))

(defun ffap-reduce-path (path)
  "Remove duplicates and non-directories from PATH list."
  (let (ret tem)
    (while path
      (setq tem path path (cdr path))
      (if (equal (car tem) ".") (setcar tem ""))
      (or (member (car tem) ret)
	  (not (file-directory-p (car tem)))
	  (progn (setcdr tem ret) (setq ret tem))))
    (nreverse ret)))

(defun ffap-all-subdirs (dir &optional depth)
  "Return list all subdirectories under DIR, starting with itself.
Directories beginning with \".\" are ignored, and directory symlinks
are listed but never searched (to avoid loops).
Optional DEPTH limits search depth."
  (and (file-exists-p dir)
       (ffap-all-subdirs-loop (expand-file-name dir) (or depth -1))))

(defun ffap-all-subdirs-loop (dir depth) ; internal
  (setq depth (1- depth))
  (cons dir
	(and (not (eq depth -1))
	     (apply 'nconc
		    (mapcar
		     (function
		      (lambda (d)
			(cond
			 ((not (file-directory-p d)) nil)
			 ((file-symlink-p d) (list d))
			 (t (ffap-all-subdirs-loop d depth)))))
		     (directory-files dir t "\\`[^.]")
		     )))))

(defvar ffap-kpathsea-depth 1
  "Bound on depth of subdirectory search in `ffap-kpathsea-expand-path'.
Set to 0 to avoid all searching, or nil for no limit.")

(defun ffap-kpathsea-expand-path (path)
  "Replace each \"//\"-suffixed dir in PATH by a list of its subdirs.
The subdirs begin with the original directory, and the depth of the
search is bounded by `ffap-kpathsea-depth'.  This is intended to mimic
kpathsea, a library used by some versions of TeX."
  (apply 'nconc
	 (mapcar
	  (function
	   (lambda (dir)
	     (if (string-match "[^/]//\\'" dir)
		 (ffap-all-subdirs (substring dir 0 -2) ffap-kpathsea-depth)
	       (list dir))))
	  path)))

(defun ffap-locate-file (file &optional nosuffix path dir-ok)
  ;; The Emacs 20 version of locate-library could almost replace this,
  ;; except it does not let us overrride the suffix list.  The
  ;; compression-suffixes search moved to ffap-file-exists-string.
  "A generic path-searching function, mimics `load' by default.
Returns path to file that \(load FILE\) would load, or nil.
Optional NOSUFFIX, if nil or t, is like the fourth argument
for load: whether to try the suffixes (\".elc\" \".el\" \"\").
If a nonempty list, it is a list of suffixes to try instead.
Optional PATH is a list of directories instead of `load-path'.
Optional DIR-OK means that returning a directory is allowed,
DIR-OK is already implicit if FILE looks like a directory.

This uses ffap-file-exists-string, which may try adding suffixes from
`ffap-compression-suffixes'."
  (or path (setq path load-path))
  (or dir-ok (setq dir-ok (equal "" (file-name-nondirectory file))))
  (if (file-name-absolute-p file)
      (setq path (list (file-name-directory file))
	    file (file-name-nondirectory file)))
  (let ((suffixes-to-try
	 (cond
	  ((consp nosuffix) nosuffix)
	  (nosuffix '(""))
	  (t '(".elc" ".el" ""))))
	suffixes try found)
    (while path
      (setq suffixes suffixes-to-try)
      (while suffixes
	(setq try (ffap-file-exists-string
		   (expand-file-name
		    (concat file (car suffixes)) (car path))))
	(if (and try (or dir-ok (not (file-directory-p try))))
	    (setq found try suffixes nil path nil)
	  (setq suffixes (cdr suffixes))))
      (setq path (cdr path)))
    found))


;;; Action List (`ffap-alist'):
;;
;; These search actions depend on the major-mode or regexps matching
;; the current name.  The little functions and their variables are
;; deferred to the next section, at some loss of "code locality".  A
;; good example of featuritis.  Trim this list for speed.

(defvar ffap-alist
  '(
    ("" . ffap-completable)		; completion, slow on some systems
    ("\\.info\\'" . ffap-info)		; gzip.info
    ("\\`info/" . ffap-info-2)		; info/emacs
    ("\\`[-a-z]+\\'" . ffap-info-3)	; (emacs)Top [only in the parentheses]
    ("\\.elc?\\'" . ffap-el)		; simple.el, simple.elc
    (emacs-lisp-mode . ffap-el-mode)	; rmail, gnus, simple, custom
    ;; (lisp-interaction-mode . ffap-el-mode) ; maybe
    (finder-mode . ffap-el-mode)	; type {C-h p} and try it
    (help-mode . ffap-el-mode)		; maybe useful
    (c++-mode . ffap-c-mode)		; search ffap-c-path
    (cc-mode . ffap-c-mode)		; same
    ("\\.\\([chCH]\\|cc\\|hh\\)\\'" . ffap-c-mode) ; stdio.h
    (fortran-mode . ffap-fortran-mode)	; FORTRAN requested by MDB
    ("\\.[fF]\\'" . ffap-fortran-mode)
    (tex-mode . ffap-tex-mode)		; search ffap-tex-path
    (latex-mode . ffap-latex-mode)	; similar
    ("\\.\\(tex\\|sty\\|doc\\|cls\\)\\'" . ffap-tex)
    ("\\.bib\\'" . ffap-bib)		; search ffap-bib-path
    ("\\`\\." . ffap-home)		; .emacs, .bashrc, .profile
    ("\\`~/" . ffap-lcd)		; |~/misc/ffap.el.Z|
    ("^[Rr][Ff][Cc][- #]?\\([0-9]+\\)"	; no $
     . ffap-rfc)			; "100% RFC2100 compliant"
    (dired-mode . ffap-dired)		; maybe in a subdirectory
    )
  "Alist of \(KEY . FUNCTION\) pairs parsed by `ffap-file-at-point'.
If string NAME at point (maybe \"\") is not a file or url, these pairs
specify actions to try creating such a string.  A pair matches if either
  KEY is a symbol, and it equals `major-mode', or
  KEY is a string, it should matches NAME as a regexp.
On a match, \(FUNCTION NAME\) is called and should return a file, an
url, or nil. If nil, search the alist for further matches.")

(put 'ffap-alist 'risky-local-variable t)

;; Example `ffap-alist' modifications:
;;
;; (setq ffap-alist                   ; remove a feature in `ffap-alist'
;;	 (delete (assoc 'c-mode ffap-alist) ffap-alist))
;;
;; (setq ffap-alist                   ; add something to `ffap-alist'
;;	 (cons
;;	  (cons "^YSN[0-9]+$"
;;		(defun ffap-ysn (name)
;;		  (concat
;;		   "http://www.physics.uiuc.edu/"
;;                 "ysn/httpd/htdocs/ysnarchive/issuefiles/"
;;		   (substring name 3) ".html")))
;;	  ffap-alist))


;;; Action Definitions:
;;
;; Define various default members of `ffap-alist'.

(defun ffap-completable (name)
  (let* ((dir (or (file-name-directory name) default-directory))
	 (cmp (file-name-completion (file-name-nondirectory name) dir)))
    (and cmp (concat dir cmp))))

(defun ffap-home (name) (ffap-locate-file name t '("~")))

(defun ffap-info (name)
  (ffap-locate-file
   name '("" ".info")
   (or (ffap-soft-value "Info-directory-list")
       (ffap-soft-value "Info-default-directory-list")
       )))

(defun ffap-info-2 (name) (ffap-info (substring name 5)))

(defun ffap-info-3 (name)
  ;; This ignores the node! "(emacs)Top" same as "(emacs)Intro"
  (and (equal (ffap-string-around) "()") (ffap-info name)))

(defun ffap-el (name) (ffap-locate-file name t))

(defun ffap-el-mode (name)
  ;; If name == "foo.el" we will skip it, since ffap-el already
  ;; searched for it once.  (This assumes the default ffap-alist.)
  (and (not (string-match "\\.el\\'" name))
       (ffap-locate-file name '(".el"))))

(defvar ffap-c-path
  ;; Need smarter defaults here!  Suggestions welcome.
  '("/usr/include" "/usr/local/include"))
(defun ffap-c-mode (name)
  (ffap-locate-file name t ffap-c-path))

(defvar ffap-fortran-path '("../include" "/usr/include"))

(defun ffap-fortran-mode (name)
  (ffap-locate-file name t ffap-fortran-path))

(defvar ffap-tex-path
  t				; delayed initialization
  "Path where `ffap-tex-mode' looks for tex files.
If t, `ffap-tex-init' will initialize this when needed.")

(defun ffap-tex-init nil
  ;; Compute ffap-tex-path if it is now t.
  (and (eq t ffap-tex-path)
       ;; this may be slow, so say something
       (message "Initializing ffap-tex-path ...")
       (setq ffap-tex-path
	     (ffap-reduce-path
	      (cons
	       "."
	       (ffap-kpathsea-expand-path
		(append
		 (ffap-list-env "TEXINPUTS")
		 ;; (ffap-list-env "BIBINPUTS")
		 (ffap-soft-value
		  "TeX-macro-global"	; AUCTeX
		  '("/usr/local/lib/tex/macros"
		    "/usr/local/lib/tex/inputs")))))))))

(defun ffap-tex-mode (name)
  (ffap-tex-init)
  (ffap-locate-file name '(".tex" "") ffap-tex-path))

(defun ffap-latex-mode (name)
  (ffap-tex-init)
  ;; only rare need for ""
  (ffap-locate-file name '(".cls" ".sty" ".tex" "") ffap-tex-path))

(defun ffap-tex (name)
  (ffap-tex-init)
  (ffap-locate-file name t ffap-tex-path))

(defvar ffap-bib-path
  (ffap-list-env "BIBINPUTS"
		 (ffap-reduce-path
		  '(
		    ;; a few wild guesses, need better
		    "/usr/local/lib/tex/macros/bib" ; Solaris?
		    "/usr/lib/texmf/bibtex/bib"	; Linux?
		    ))))

(defun ffap-bib (name)
  (ffap-locate-file name t ffap-bib-path))

(defun ffap-dired (name)
  (let ((pt (point)) dir try)
    (save-excursion
      (and (progn
	     (beginning-of-line)
	     (looking-at " *[-d]r[-w][-x][-r][-w][-x][-r][-w][-x] "))
	   (re-search-backward "^ *$" nil t)
	   (re-search-forward "^ *\\([^ \t\n:]*\\):\n *total " pt t)
	   (file-exists-p
	    (setq try
		  (expand-file-name
		   name
		   (buffer-substring
		    (match-beginning 1) (match-end 1)))))
	   try))))

;; Maybe a "Lisp Code Directory" reference:
(defun ffap-lcd (name)
  (and
   (or
    ;; lisp-dir-apropos output buffer:
    (string-match "Lisp Code Dir" (buffer-name))
    ;; Inside an LCD entry like |~/misc/ffap.el.Z|,
    ;; or maybe the holy LCD-Datafile itself:
    (member (ffap-string-around) '("||" "|\n")))
   (concat
    ;; lispdir.el may not be loaded yet:
    (ffap-host-to-path
     (ffap-soft-value "elisp-archive-host"
		      "archive.cis.ohio-state.edu"))
    (file-name-as-directory
     (ffap-soft-value "elisp-archive-directory"
		      "/pub/gnu/emacs/elisp-archive/"))
    (substring name 2))))

(defvar ffap-rfc-path
  (concat (ffap-host-to-path "ds.internic.net") "/rfc/rfc%s.txt"))

(defun ffap-rfc (name)
  (format ffap-rfc-path
	  (substring name (match-beginning 1) (match-end 1))))


;;; At-Point Functions:

(defvar ffap-string-at-point-mode-alist
  '(
    ;; The default, used when the `major-mode' is not found.
    ;; Slightly controversial decisions:
    ;; * strip trailing "@" and ":"
    ;; * no commas (good for latex)
    (file "--:$+<>@-Z_a-z~" "<@" "@>;.,!?:")
    ;; An url, or maybe a email/news message-id:
    (url "--:=&?$+@-Z_a-z~#,%" "^A-Za-z0-9" ":;.,!?")
    ;; Find a string that does *not* contain a colon:
    (nocolon "--9$+<>@-Z_a-z~" "<@" "@>;.,!?")
    ;; A machine:
    (machine "-a-zA-Z0-9." "" ".")
    ;; Mathematica paths: allow backquotes
    (math-mode ",-:$+<>@-Z_a-z~`" "<" "@>;.,!?`:")
    )
  "Alist of \(MODE CHARS BEG END\), where MODE is a symbol,
possibly a major-mode name, or one of the symbol
`file', `url', `machine', and `nocolon'.
`ffap-string-at-point' uses the data fields as follows:
1. find a maximal string of CHARS around point,
2. strip BEG chars before point from the beginning,
3. Strip END chars after point from the end.")

(defvar ffap-string-at-point nil
  ;; Added at suggestion of RHOGEE (for ff-paths), 7/24/95.
  "Last string returned by `ffap-string-at-point'.")

(defun ffap-string-at-point (&optional mode)
  "Return a string of characters from around point.
MODE (defaults to value of `major-mode') is a symbol used to look up string
syntax parameters in `ffap-string-at-point-mode-alist'.
If MODE is not found, we use `file' instead of MODE.
Sets `ffap-string-at-point' and `ffap-string-at-point-region'."
  (let* ((args
	  (cdr
	   (or (assq (or mode major-mode) ffap-string-at-point-mode-alist)
	       (assq 'file ffap-string-at-point-mode-alist))))
	 (pt (point))
	 (str
	  (buffer-substring
	   (save-excursion
	     (skip-chars-backward (car args))
	     (skip-chars-forward (nth 1 args) pt)
	     (setcar ffap-string-at-point-region (point)))
	   (save-excursion
	     (skip-chars-forward (car args))
	     (skip-chars-backward (nth 2 args) pt)
	     (setcar (cdr ffap-string-at-point-region) (point))))))
    (set-text-properties 0 (length str) nil str)
    (setq ffap-string-at-point str)))

(defun ffap-string-around nil
  ;; Sometimes useful to decide how to treat a string.
  "Return string of two chars around last `ffap-string-at-point'.
Assumes the buffer has not changed."
  (save-excursion
    (format "%c%c"
	    (progn
	      (goto-char (car ffap-string-at-point-region))
	      (preceding-char))		; maybe 0
	    (progn
	      (goto-char (nth 1 ffap-string-at-point-region))
	      (following-char))		; maybe 0
	    )))

(defun ffap-copy-string-as-kill (&optional mode)
  ;; Requested by MCOOK.  Useful?
  "Call `ffap-string-at-point', and copy result to `kill-ring'."
  (interactive)
  (let ((str (ffap-string-at-point mode)))
    (if (equal "" str)
	(message "No string found around point.")
      (kill-new str)
      ;; Older: (apply 'copy-region-as-kill ffap-string-at-point-region)
      (message "Copied to kill ring: %s"  str))))

(defun ffap-url-at-point nil
  "Return url from around point if it exists, or nil."
  ;; Could use w3's url-get-url-at-point instead.  Both handle "URL:",
  ;; ignore non-relative links, trim punctuation.  The other will
  ;; actually look back if point is in whitespace, but I would rather
  ;; ffap be less aggressive in such situations.
  (and
   ffap-url-regexp
   (or
    ;; In a w3 buffer button?
    (and (eq major-mode 'w3-mode)
	 ;; interface recommended by wmperry:
	 (w3-view-this-url t))
    ;; Is there a reason not to strip trailing colon?
    (let ((name (ffap-string-at-point 'url)))
      (cond
       ((string-match "^url:" name) (setq name (substring name 4)))
       ((and (string-match "\\`[^:</>@]+@[^:</>@]+[a-zA-Z0-9]\\'" name)
	     ;; "foo@bar": could be "mailto" or "news" (a Message-ID).
	     ;; Without "<>" it must be "mailto".  Otherwise could be
	     ;; either, so consult `ffap-foo-at-bar-prefix'.
	     (let ((prefix (if (and (equal (ffap-string-around) "<>")
				    ;; Expect some odd characters:
				    (string-match "[$.0-9].*[$.0-9].*@" name))
			       ;; Could be news:
			       ffap-foo-at-bar-prefix
			     "mailto")))
	       (and prefix (setq name (concat prefix ":" name))))))
       ((ffap-newsgroup-p name) (setq name (concat "news:" name)))
       ((and (string-match "\\`[a-z0-9]+\\'" name) ; <mic> <root> <nobody>
	     (equal (ffap-string-around) "<>")
	     ;;	(ffap-user-p name):
	     (not (string-match "~" (expand-file-name (concat "~" name))))
	     )
	(setq name (concat "mailto:" name)))
       )
      (and (ffap-url-p name) name)
      ))))

(defvar ffap-gopher-regexp
  "^.*\\<\\(Type\\|Name\\|Path\\|Host\\|Port\\) *= *\\(.*\\) *$"
  "Regexp Matching a line in a gopher bookmark (maybe indented).
The two subexpressions are the KEY and VALUE.")

(defun ffap-gopher-at-point nil
  "If point is inside a gopher bookmark block, return its url."
  ;; `gopher-parse-bookmark' from gopher.el is not so robust
  (save-excursion
    (beginning-of-line)
    (if (looking-at ffap-gopher-regexp)
	(progn
	  (while (and (looking-at ffap-gopher-regexp) (not (bobp)))
	    (forward-line -1))
	  (or (looking-at ffap-gopher-regexp) (forward-line 1))
	  (let ((type "1") name path host (port "70"))
	    (while (looking-at ffap-gopher-regexp)
	      (let ((var (intern
			  (downcase
			   (buffer-substring (match-beginning 1)
					     (match-end 1)))))
		    (val (buffer-substring (match-beginning 2)
					   (match-end 2))))
		(set var val)
		(forward-line 1)))
	    (if (and path (string-match "^ftp:.*@" path))
		(concat "ftp://"
			(substring path 4 (1- (match-end 0)))
			(substring path (match-end 0)))
	      (and (= (length type) 1)
		   host;; (ffap-machine-p host)
		   (concat "gopher://" host
			   (if (equal port "70") "" (concat ":" port))
			   "/" type path))))))))

(defvar ffap-ftp-sans-slash-regexp
  (and
   ffap-ftp-regexp
   ;; Note: by now, we know it is not an url.
   ;; Icky regexp avoids: default: 123: foo::bar cs:pub
   ;; It does match on: mic@cs: cs:/pub mathcs.emory.edu: (point at end)
   "\\`\\([^:@]+@[^:@]+:\\|[^@.:]+\\.[^@:]+:\\|[^:]+:[~/]\\)\\([^:]\\|\\'\\)")
  "Strings matching this are coerced to ftp paths by ffap.
That is, ffap just prepends \"/\".  Set to nil to disable.")

(defun ffap-file-at-point nil
  "Return filename from around point if it exists, or nil.
Existence test is skipped for names that look remote.
If the filename is not obvious, it also tries `ffap-alist',
which may actually result in an url rather than a filename."
  ;; Note: this function does not need to look for url's, just
  ;; filenames.  On the other hand, it is responsible for converting
  ;; a pseudo-url "site.com://path" to an ftp path
  (let* ((case-fold-search t)		; url prefixes are case-insensitive
	 (data (match-data))
	 (string (ffap-string-at-point)) ; uses mode alist
	 (name
	  (or (condition-case nil
		  (and (not (string-match "//" string)) ; foo.com://bar
		       (substitute-in-file-name string))
		(error nil))
	      string))
	 (abs (file-name-absolute-p name))
	 (default-directory default-directory))
    (unwind-protect
	(cond
	 ;; Immediate rejects (/ and // are too common in C++):
         ((member name '("" "/" "//" ".")) nil)
         ;; Immediately test local filenames.  If default-directory is
         ;; remote, you probably already have a connection.
         ((and (not abs) (ffap-file-exists-string name)))
         ;; Try stripping off line numbers; good for compilation/grep output.
         ((and (not abs) (string-match ":[0-9]" name)
               (ffap-file-exists-string (substring name 0 (match-beginning 0)))))
	 ;; Immediately test local filenames.  If default-directory is
	 ;; remote, you probably already have a connection.
	 ((and (not abs) (ffap-file-exists-string name)))
	 ;; Accept remote names without actual checking (too slow):
	 ((if abs
	      (ffap-file-remote-p name)
	    ;; Try adding a leading "/" (common omission in ftp paths):
	    (and
	     ffap-ftp-sans-slash-regexp
	     (string-match ffap-ftp-sans-slash-regexp name)
	     (ffap-file-remote-p (concat "/" name)))))
	 ;; Ok, not remote, try the existence test even if it is absolute:
	 ((and abs (ffap-file-exists-string name)))
	 ;; If it contains a colon, get rid of it (and return if exists)
	 ((and (string-match path-separator name)
	       (setq name (ffap-string-at-point 'nocolon))
	       (ffap-file-exists-string name)))
	 ;; File does not exist, try the alist:
	 ((let ((alist ffap-alist) tem try case-fold-search)
	    (while (and alist (not try))
	      (setq tem (car alist) alist (cdr alist))
	      (if (or (eq major-mode (car tem))
		      (and (stringp (car tem))
			   (string-match (car tem) name)))
		  (and (setq try
			     (condition-case nil
				 (funcall (cdr tem) name)
			       (error nil)))
		       (setq try (or
				  (ffap-url-p try) ; not a file!
				  (ffap-file-remote-p try)
				  (ffap-file-exists-string try))))))
	    try))
	 ;; Alist failed?  Try to guess an active remote connection
	 ;; from buffer variables, and try once more, both as an
	 ;; absolute and relative path on that remote host.
	 ((let* (ffap-rfs-regexp	; suppress
		 (remote-dir
		  (cond
		   ((ffap-file-remote-p default-directory))
		   ((and (eq major-mode 'internal-ange-ftp-mode)
			 (string-match "^\\*ftp \\(.*\\)@\\(.*\\)\\*$"
				       (buffer-name)))
		    (concat "/" (substring (buffer-name) 5 -1) ":"))
		   ;; This is too often a bad idea:
		   ;;((and (eq major-mode 'w3-mode)
		   ;;	   (stringp url-current-server))
		   ;; (host-to-ange-path url-current-server))
		   )))
	    (and remote-dir
		 (or
		  (and (string-match "\\`\\(/?~?ftp\\)/" name)
		       (ffap-file-exists-string
			(ffap-replace-path-component
			 remote-dir (substring name (match-end 1)))))
		  (ffap-file-exists-string
		   (ffap-replace-path-component remote-dir name))))))
	 )
      (set-match-data data))))

;;; Prompting (`ffap-read-file-or-url'):
;;
;; We want to complete filenames as in read-file-name, but also url's
;; which read-file-name-internal would truncate at the "//" string.
;; The solution here is to replace read-file-name-internal with
;; `ffap-read-file-or-url-internal', which checks the minibuffer
;; contents before attempting to complete filenames.

(defun ffap-read-file-or-url (prompt guess)
  "Read file or url from minibuffer, with PROMPT and initial GUESS."
  (or guess (setq guess default-directory))
  (let (dir)
    ;; Tricky: guess may have or be a local directory, like "w3/w3.elc"
    ;; or "w3/" or "../el/ffap.el" or "../../../"
    (or (ffap-url-p guess)
	(progn
	  (or (ffap-file-remote-p guess)
	      (setq guess
		    (abbreviate-file-name (expand-file-name guess))
		    ))
	  (setq dir (file-name-directory guess))))
    (let ((minibuffer-completing-file-name t))
      (setq guess
	    (completing-read
	     prompt
	     'ffap-read-file-or-url-internal
	     dir
	     nil
	     (if dir (cons guess (length dir)) guess)
	     (list 'file-name-history))))
    ;; Do file substitution like (interactive "F"), suggested by MCOOK.
    (or (ffap-url-p guess) (setq guess (substitute-in-file-name guess)))
    ;; Should not do it on url's, where $ is a common (VMS?) character.
    ;; Note: upcoming url.el package ought to handle this automatically.
    guess))

(defun ffap-read-url-internal (string dir action)
  "Complete url's from history, treating given string as valid."
  (let ((hist (ffap-soft-value "url-global-history-hash-table")))
    (cond
     ((not action)
      (or (try-completion string hist) string))
     ((eq action t)
      (or (all-completions string hist) (list string)))
     ;; action == lambda, documented where?  Tests whether string is a
     ;; valid "match".  Let us always say yes.
     (t t))))

(defun ffap-read-file-or-url-internal (string dir action)
  (unless dir
    (setq dir default-directory))
  (unless string
    (setq string default-directory))
  (if (ffap-url-p string)
      (ffap-read-url-internal string dir action)
    (read-file-name-internal string dir action)))

;; The rest of this page is just to work with package complete.el.
;; This code assumes that you load ffap.el after complete.el.
;;
;; We must inform complete about whether our completion function
;; will do filename style completion.  For earlier versions of
;; complete.el, this requires a defadvice.  For recent versions
;; there may be a special variable for this purpose.

(defun ffap-complete-as-file-p nil
  ;; Will `minibuffer-completion-table' complete the minibuffer
  ;; contents as a filename?  Assumes the minibuffer is current.
  ;; Note: t and non-nil mean somewhat different reasons.
  (if (eq minibuffer-completion-table 'ffap-read-file-or-url-internal)
      (not (ffap-url-p (buffer-string))) ; t
    (memq minibuffer-completion-table
	  '(read-file-name-internal read-directory-name-internal)) ; list
    ))

(and
 (featurep 'complete)
 (if (boundp 'PC-completion-as-file-name-predicate)
     ;; modern version of complete.el, just set the variable:
     (setq PC-completion-as-file-name-predicate 'ffap-complete-as-file-p)
   (require 'advice)
   (defadvice PC-do-completion (around ffap-fix act)
     "Work with ffap."
     (let ((minibuffer-completion-table
	    (if (eq t (ffap-complete-as-file-p))
		'read-file-name-internal
	      minibuffer-completion-table)))
       ad-do-it))))


;;; Highlighting (`ffap-highlight'):
;;
;; Based on overlay highlighting in Emacs 19.28 isearch.el.

(defvar ffap-highlight t
  "If non-nil, ffap highlights the current buffer substring.")

(defvar ffap-highlight-overlay nil
  "Overlay used by `ffap-highlight'.")

(defun ffap-highlight (&optional remove)
  "If `ffap-highlight' is set, highlight the guess in this buffer.
That is, the last buffer substring found by `ffap-string-at-point'.
Optional argument REMOVE means to remove any such highlighting.
Uses the face `ffap' if it is defined, or else `highlight'."
  (cond
   (remove
    (and ffap-highlight-overlay
	 (delete-overlay ffap-highlight-overlay))
    )
   ((not ffap-highlight) nil)
   (ffap-highlight-overlay
    (move-overlay
     ffap-highlight-overlay
     (car ffap-string-at-point-region)
     (nth 1 ffap-string-at-point-region)
     (current-buffer)))
   (t
    (setq ffap-highlight-overlay
	  (apply 'make-overlay ffap-string-at-point-region))
    (overlay-put ffap-highlight-overlay 'face
		      (if (facep 'ffap) 'ffap 'highlight)))))


;;; Main Entrance (`find-file-at-point' == `ffap'):

(defun ffap-guesser nil
  "Return file or URL or nil, guessed from text around point."
  (or (and ffap-url-regexp
	   (ffap-fixup-url (or (ffap-url-at-point)
			       (ffap-gopher-at-point))))
      (ffap-file-at-point)		; may yield url!
      (ffap-fixup-machine (ffap-machine-at-point))))

(defun ffap-prompter (&optional guess)
  ;; Does guess and prompt step for find-file-at-point.
  ;; Extra complication for the temporary highlighting.
  (unwind-protect
      ;; This catch will let ffap-alist entries do their own prompting
      ;; and then maybe skip over this prompt (ff-paths, for example).
      (catch 'ffap-prompter
	(ffap-read-file-or-url
	 (if ffap-url-regexp "Find file or URL: " "Find file: ")
	 (prog1
	     (setq guess (or guess (ffap-guesser))) ; using ffap-alist here
	   (and guess (ffap-highlight))
	   )))
    (ffap-highlight t)))

;;;###autoload
(defun find-file-at-point (&optional filename)
  "Find FILENAME, guessing a default from text around point.
If `ffap-url-regexp' is not nil, the FILENAME may also be an URL.
With a prefix, this command behaves exactly like `ffap-file-finder'.
If `ffap-require-prefix' is set, the prefix meaning is reversed.
See also the variables `ffap-dired-wildcards', `ffap-newfile-prompt',
and the functions `ffap-file-at-point' and `ffap-url-at-point'.

See <ftp://ftp.mathcs.emory.edu/pub/mic/emacs/> for latest version."
  (interactive)
  (if (and (interactive-p)
	   (if ffap-require-prefix (not current-prefix-arg)
	     current-prefix-arg))
      ;; Do exactly the ffap-file-finder command, even the prompting:
      (let (current-prefix-arg)		; we already interpreted it
	(call-interactively ffap-file-finder))
    (or filename (setq filename (ffap-prompter)))
    (cond
     ((ffap-url-p filename)
      (let (current-prefix-arg)		; w3 2.3.25 bug, reported by KPC
	(funcall ffap-url-fetcher filename)))
     ;; This junk more properly belongs in a modified ffap-file-finder:
     ((and ffap-dired-wildcards
	   (string-match ffap-dired-wildcards filename))
      (dired filename))
     ((or (not ffap-newfile-prompt)
	  (file-exists-p filename)
	  (y-or-n-p "File does not exist, create buffer? "))
      (funcall ffap-file-finder
	       ;; expand-file-name fixes "~/~/.emacs" bug sent by CHUCKR.
	       (expand-file-name filename)))
     ;; User does not want to find a non-existent file:
     ((signal 'file-error (list "Opening file buffer"
				"no such file or directory"
				filename))))))

;; Shortcut: allow {M-x ffap} rather than {M-x find-file-at-point}.
;;;###autoload
(defalias 'ffap 'find-file-at-point)


;;; Menu support (`ffap-menu'):

(defvar ffap-menu-regexp nil
  "*If non-nil, overrides `ffap-next-regexp' during `ffap-menu'.
Make this more restrictive for faster menu building.
For example, try \":/\" for URL (and some ftp) references.")

(defvar ffap-menu-alist nil
  "Buffer local cache of menu presented by `ffap-menu'.")
(make-variable-buffer-local 'ffap-menu-alist)

(defvar ffap-menu-text-plist
  (cond
   ((display-mouse-p) '(face bold mouse-face highlight)) ; keymap <mousy-map>
   (t nil))
  "Text properties applied to strings found by `ffap-menu-rescan'.
These properties may be used to fontify the menu references.")

;;;###autoload
(defun ffap-menu (&optional rescan)
  "Put up a menu of files and urls mentioned in this buffer.
Then set mark, jump to choice, and try to fetch it.  The menu is
cached in `ffap-menu-alist', and rebuilt by `ffap-menu-rescan'.
The optional RESCAN argument \(a prefix, interactively\) forces
a rebuild.  Searches with `ffap-menu-regexp'."
  (interactive "P")
  ;; (require 'imenu) -- no longer used, but roughly emulated
  (if (or (not ffap-menu-alist) rescan
	  ;; or if the first entry is wrong:
	  (and ffap-menu-alist
	       (let ((first (car ffap-menu-alist)))
		 (save-excursion
		   (goto-char (cdr first))
		   (not (equal (car first) (ffap-guesser)))))))
      (ffap-menu-rescan))
  ;; Tail recursive:
  (ffap-menu-ask
   (if ffap-url-regexp "Find file or URL" "Find file")
   (cons (cons "*Rescan Buffer*" -1) ffap-menu-alist)
   'ffap-menu-cont))

(defun ffap-menu-cont (choice)		; continuation of ffap-menu
  (if (< (cdr choice) 0)
      (ffap-menu t)			; *Rescan*
    (push-mark)
    (goto-char (cdr choice))
    ;; Momentary highlight:
    (unwind-protect
	(progn
	  (and ffap-highlight (ffap-guesser) (ffap-highlight))
	  (sit-for 0)			; display
	  (find-file-at-point (car choice)))
      (ffap-highlight t))))

(defun ffap-menu-ask (title alist cont)
  "Prompt from a menu of choices, and then apply some action.
Arguments are TITLE, ALIST, and CONT \(a continuation function\).
This uses either a menu or the minibuffer depending on invocation.
The TITLE string is used as either the prompt or menu title.
Each ALIST entry looks like (STRING . DATA) and defines one choice.
Function CONT is applied to the entry chosen by the user."
  ;; Note: this function is used with a different continuation
  ;; by the ffap-url add-on package.
  ;; Could try rewriting to use easymenu.el or lmenu.el.
  (let (choice)
    (cond
     ;; Emacs mouse:
     ((and (fboundp 'x-popup-menu) (ffap-mouse-event))
      (setq choice
	    (x-popup-menu
	     t
	     (list "" (cons title
			    (mapcar (function (lambda (i) (cons (car i) i)))
				    alist))))))
     ;; minibuffer with completion buffer:
     (t
      (let ((minibuffer-setup-hook 'minibuffer-completion-help))
	;; Bug: prompting may assume unique strings, no "".
	(setq choice
	      (completing-read
	       (format "%s (default %s): " title (car (car alist)))
	       alist nil t
	       ;; (cons (car (car alist)) 0)
	       nil)))
      (sit-for 0)			; redraw original screen
      ;; Convert string to its entry, or else the default:
      (setq choice (or (assoc choice alist) (car alist))))
     )
    (if choice
	(funcall cont choice)
      (message "No choice made!")	; possible with menus
      nil)))

(defun ffap-menu-rescan nil
  "Search buffer for `ffap-menu-regexp' to build `ffap-menu-alist'.
Applies `ffap-menu-text-plist' text properties at all matches."
  (interactive)
  (let ((ffap-next-regexp (or ffap-menu-regexp ffap-next-regexp))
	(range (- (point-max) (point-min)))
	(mod (buffer-modified-p))	; was buffer modified?
	buffer-read-only		; to set text-properties
	item
	;; Avoid repeated searches of the *mode-alist:
	(major-mode (if (assq major-mode ffap-string-at-point-mode-alist)
			major-mode
		      'file)))
    (setq ffap-menu-alist nil)
    (unwind-protect
	(save-excursion
	  (goto-char (point-min))
	  (while (setq item (ffap-next-guess))
	    (setq ffap-menu-alist (cons (cons item (point)) ffap-menu-alist))
	    (add-text-properties (car ffap-string-at-point-region) (point)
				 ffap-menu-text-plist)
	    (message "Scanning...%2d%% <%s>"
		     (/ (* 100 (- (point) (point-min))) range) item)))
      (or mod (set-buffer-modified-p nil))))
  (message "Scanning...done")
  ;; Remove duplicates.
  (setq ffap-menu-alist			; sort by item
	(sort ffap-menu-alist
	      (function
	       (lambda (a b) (string-lessp (car a) (car b))))))
  (let ((ptr ffap-menu-alist))		; remove duplicates
    (while (cdr ptr)
      (if (equal (car (car ptr)) (car (car (cdr ptr))))
	  (setcdr ptr (cdr (cdr ptr)))
	(setq ptr (cdr ptr)))))
  (setq ffap-menu-alist			; sort by position
	(sort ffap-menu-alist
	      (function
	       (lambda (a b) (< (cdr a) (cdr b)))))))


;;; Mouse Support (`ffap-at-mouse'):
;;
;; See the suggested binding in ffap-bindings (near eof).

(defvar ffap-at-mouse-fallback nil	; ffap-menu? too time-consuming
  "Command invoked by `ffap-at-mouse' if nothing found at click, or nil.
Ignored when `ffap-at-mouse' is called programmatically.")
(put 'ffap-at-mouse-fallback 'risky-local-variable t)

;;;###autoload
(defun ffap-at-mouse (e)
  "Find file or url guessed from text around mouse click.
Interactively, calls `ffap-at-mouse-fallback' if no guess is found.
Return value:
  * if a guess string is found, return it (after finding it)
  * if the fallback is called, return whatever it returns
  * otherwise, nil"
  (interactive "e")
  (let ((guess
	 ;; Maybe less surprising without the save-excursion?
	 (save-excursion
	   (mouse-set-point e)
	   ;; Would prefer to do nothing unless click was *on* text.  How
	   ;; to tell that the click was beyond the end of current line?
	   (ffap-guesser))))
    (cond
     (guess
      (set-buffer (ffap-event-buffer e))
      (ffap-highlight)
      (unwind-protect
	  (progn
	    (sit-for 0)			; display
	    (message "Finding `%s'" guess)
	    (find-file-at-point guess)
	    guess)			; success: return non-nil
	(ffap-highlight t)))
     ((interactive-p)
      (if ffap-at-mouse-fallback
	  (call-interactively ffap-at-mouse-fallback)
	(message "No file or url found at mouse click.")
	nil))				; no fallback, return nil
     ;; failure: return nil
     )))


;;; ffap-other-* commands:
;;
;; Requested by KPC.

;; There could be a real `ffap-noselect' function, but we would need
;; at least two new user variables, and there is no w3-fetch-noselect.
;; So instead, we just fake it with a slow save-window-excursion.

(defun ffap-other-window nil
  "Like `ffap', but put buffer in another window.
Only intended for interactive use."
  (interactive)
  (switch-to-buffer-other-window
   (save-window-excursion (call-interactively 'ffap) (current-buffer))))

(defun ffap-other-frame nil
  "Like `ffap', but put buffer in another frame.
Only intended for interactive use."
  (interactive)
  ;; Extra code works around dedicated windows (noted by JENS, 7/96):
  (let* ((win (selected-window)) (wdp (window-dedicated-p win)))
    (unwind-protect
	(progn
	  (set-window-dedicated-p win nil)
	  (switch-to-buffer-other-frame
	   (save-window-excursion
	     (call-interactively 'ffap)
	     (current-buffer))))
      (set-window-dedicated-p win wdp))))


;;; Bug Reporter:

(defun ffap-bug nil
  "Submit a bug report for the ffap package."
  ;; Important: keep the version string here in synch with that at top
  ;; of file!  Could use lisp-mnt from Emacs 19, but that would depend
  ;; on being able to find the ffap.el source file.
  (interactive)
  (require 'reporter)
  (let ((reporter-prompt-for-summary-p t))
    (reporter-submit-bug-report
     "Michelangelo Grigni <mic@mathcs.emory.edu>"
     "ffap"
     (mapcar 'intern (all-completions "ffap-" obarray 'boundp)))))

(fset 'ffap-submit-bug 'ffap-bug)	; another likely name


;;; Hooks for Gnus, VM, Rmail:
;;
;; If you do not like these bindings, write versions with whatever
;; bindings you would prefer.

(defun ffap-ro-mode-hook nil
  "Bind `ffap-next' and `ffap-menu' to M-l and M-m, resp."
  (local-set-key "\M-l" 'ffap-next)
  (local-set-key "\M-m" 'ffap-menu)
  )

(defun ffap-gnus-hook nil
  "Bind `ffap-gnus-next' and `ffap-gnus-menu' to M-l and M-m, resp."
  (set (make-local-variable 'ffap-foo-at-bar-prefix) "news") ; message-id's
  ;; Note "l", "L", "m", "M" are taken:
  (local-set-key "\M-l" 'ffap-gnus-next)
  (local-set-key "\M-m" 'ffap-gnus-menu))

(defun ffap-gnus-wrapper (form)		; used by both commands below
  (and (eq (current-buffer) (get-buffer gnus-summary-buffer))
       (gnus-summary-select-article))	; get article of current line
  ;; Preserve selected buffer, but do not do save-window-excursion,
  ;; since we want to see any window created by the form.  Temporarily
  ;; select the article buffer, so we can see any point movement.
  (let ((sb (window-buffer (selected-window))))
    (gnus-configure-windows 'article)
    (pop-to-buffer gnus-article-buffer)
    (widen)
    ;; Skip headers for ffap-gnus-next (which will wrap around)
    (if (eq (point) (point-min)) (search-forward "\n\n" nil t))
    (unwind-protect
	(eval form)
      (pop-to-buffer sb))))

(defun ffap-gnus-next nil
  "Run `ffap-next' in the gnus article buffer."
  (interactive) (ffap-gnus-wrapper '(ffap-next nil t)))

(defun ffap-gnus-menu nil
  "Run `ffap-menu' in the gnus article buffer."
  (interactive) (ffap-gnus-wrapper '(ffap-menu)))


(defcustom dired-at-point-require-prefix nil
  "*If set, reverses the prefix argument to `dired-at-point'.
This is nil so neophytes notice ffap.  Experts may prefer to disable
ffap most of the time."
  :type 'boolean
  :group 'ffap
  :version "20.3")

;;;###autoload
(defun dired-at-point (&optional filename)
  "Start Dired, defaulting to file at point.  See `ffap'."
  (interactive)
  (if (and (interactive-p)
	   (if dired-at-point-require-prefix
	       (not current-prefix-arg)
	     current-prefix-arg))
      (let (current-prefix-arg)		; already interpreted
	(call-interactively 'dired))
    (or filename (setq filename (dired-at-point-prompter)))
    (cond
     ((ffap-url-p filename)
      (funcall ffap-url-fetcher filename))
     ((and ffap-dired-wildcards
	   (string-match ffap-dired-wildcards filename))
      (dired filename))
     ((file-exists-p filename)
      (if (file-directory-p filename)
	  (dired (expand-file-name filename))
	(dired (concat (expand-file-name filename) "*"))))
     ((y-or-n-p "Directory does not exist, create it? ")
      (make-directory filename)
      (dired filename))
     ((error "No such file or directory `%s'" filename)))))

(defun dired-at-point-prompter (&optional guess)
  ;; Does guess and prompt step for find-file-at-point.
  ;; Extra complication for the temporary highlighting.
  (unwind-protect
      (ffap-read-file-or-url
       (if ffap-url-regexp "Dired file or URL: " "Dired file: ")
       (prog1
	   (setq guess (or guess (ffap-guesser)))
	 (and guess (ffap-highlight))
	 ))
    (ffap-highlight t)))

;;; Offer default global bindings (`ffap-bindings'):

(defvar ffap-bindings
   '(
     (global-set-key [S-mouse-3] 'ffap-at-mouse)
     (global-set-key [C-S-mouse-3] 'ffap-menu)
     (global-set-key "\C-x\C-f" 'find-file-at-point)
     (global-set-key "\C-x4f"   'ffap-other-window)
     (global-set-key "\C-x5f"   'ffap-other-frame)
     (global-set-key "\C-xd"    'dired-at-point)
     (add-hook 'gnus-summary-mode-hook 'ffap-gnus-hook)
     (add-hook 'gnus-article-mode-hook 'ffap-gnus-hook)
     (add-hook 'vm-mode-hook 'ffap-ro-mode-hook)
     (add-hook 'rmail-mode-hook 'ffap-ro-mode-hook)
     ;; (setq dired-x-hands-off-my-keys t) ; the default
     )
     "List of binding forms evaluated by function `ffap-bindings'.
A reasonable ffap installation needs just this one line:
  (ffap-bindings)
Of course if you do not like these bindings, just roll your own!")

;;;###autoload
(defun ffap-bindings nil
  "Evaluate the forms in variable `ffap-bindings'."
  (interactive)
  (eval (cons 'progn ffap-bindings)))


;;; ffap.el ends here
