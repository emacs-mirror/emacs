;;; ffap.el --- find file or url at point

;; Copyright (C) 1995, 1996 Free Software Foundation, Inc.

;; Author: Michelangelo Grigni <mic@mathcs.emory.edu>
;; Created: 29 Mar 1993
;; Keywords: files, hypermedia, matching, mouse
;; X-Latest: ftp://ftp.mathcs.emory.edu:/pub/mic/emacs/

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
;; to guess a default file or url from the text around the point
;; (`ffap-require-prefix' swaps these behaviors).  This is useful for
;; following references in situations such as mail or news buffers,
;; README's, MANIFEST's, and so on.  Submit bugs or suggestions with
;; M-x ffap-bug.
;;
;; For the default installation, byte-compile ffap.el somewhere in
;; your `load-path' and add these two lines to your .emacs file:
;;
;; (require 'ffap)                      ; load the package
;; (ffap-bindings)                      ; do default key bindings
;;
;; ffap-bindings makes the following global key bindings:
;;
;; C-x C-f       find-file-at-point (abbreviated as ffap)
;; C-x 4 f       ffap-other-window
;; C-x 5 f       ffap-other-frame
;; S-mouse-3     ffap-at-mouse
;;
;; ffap-bindings also adds hooks to make the following local bindings
;; in vm, gnus, and rmail:
;;
;; M-l         ffap-next, or ffap-gnus-next in gnus
;; M-m         ffap-menu, or ffap-gnus-menu in gnus
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
;; (setq ffap-url-regexp nil)           ; disable url features in ffap
;;
;; ffap uses w3 (if found) or else browse-url to fetch url's.  For
;; a hairier `ffap-url-fetcher', try ffap-url.el (same ftp site).
;; Also, you can add `ffap-menu-rescan' to various hooks to fontify
;; the file and url references within a buffer.

;;; Todo list:
;; * recognize paths inside /usr/bin:/bin:/etc, ./ffap.el:80:
;; * let "/path/file#key" jump to key (offset or regexp) in /path/file
;; * find file of symbol if TAGS is loaded (like above)
;; * break up long menus into multiple panes (like imenu?)
;; * notice node in "(dired)Virtual Dired" (handle the space?)
;; * notice "machine.dom blah blah blah path/file" (how?)
;; * if w3 becomes standard, could rewrite to use its functions
;; * regexp options for ffap-string-at-point, like font-lock (MCOOK)
;; * v19: could replace `ffap-locate-file' with a quieter `locate-library'
;; * support for custom.el
;; + handle "$(HOME)" in Makefiles?
;; + modify `font-lock-keywords' to do fontification


;;; Code:

(provide 'ffap)

;; Versions: This file is tested with Emacs 19.30.  It mostly works
;; with XEmacs, but get ffap-xe.el for the popup menu.  Emacs 18 is
;; now abandoned (get ffap-15.el instead).

(defvar ffap-xemacs (and (string-match "X[Ee]macs" emacs-version) t)
  "Whether ffap thinks it is running under XEmacs.")



;;; User Variables:

;; This function is used inside defvars:
(defun ffap-soft-value (name &optional default)
  "Return value of symbol with NAME, if it is interned.
Otherwise return nil (or the optional DEFAULT value)."
  ;; Bug: (ffap-soft-value "nil" 5) --> 5
  (let ((sym (intern-soft name)))
    (if (and sym (boundp sym)) (symbol-value sym) default)))


(defvar ffap-ftp-regexp
  (and
   (or (featurep 'ange-ftp)
       (featurep 'efs)
       (and (boundp 'file-name-handler-alist) ; v19
	    (or (rassq 'ange-ftp-hook-function file-name-handler-alist)
		(rassq 'efs-file-handler-function file-name-handler-alist))))
   ;; Apparently this is good enough for both ange-ftp and efs:
   "\\`/[^/:]+:")
  "*Treat paths matching this as remote ftp paths.  Nil to disable.
Nil also disables the generation of such paths by ffap.")

(defvar ffap-url-unwrap-local t
  "*If non-nil, convert \"file:\" url to local path before prompting.")

(defvar ffap-url-unwrap-remote t
  "*If non-nil, convert \"ftp:\" url to remote path before prompting.
This is ignored if `ffap-ftp-regexp' is nil.")

(defvar ffap-ftp-default-user
  (if (or (equal (ffap-soft-value "ange-ftp-default-user") "anonymous")
	  (equal (ffap-soft-value "efs-default-user") "anonymous"))
      nil
    "anonymous")
  "*User name in ftp paths generated by `ffap-host-to-path'.
Nil to rely on `efs-default-user' or `ange-ftp-default-user'.")

(defvar ffap-rfs-regexp
  ;; Remote file access built into file system?  HP rfa or Andrew afs:
  "\\`/\\(afs\\|net\\)/."
  ;; afs only: (and (file-exists-p "/afs") "\\`/afs/.")
  "*Matching paths are treated as remote.  Nil to disable.")

(defvar ffap-url-regexp
  ;; Could just use `url-nonrelative-link' of w3, if loaded.
  ;; This regexp is not exhaustive, it just matches common cases.
  (concat
   "\\`\\("
   "news\\(post\\)?:\\|mailto:\\|file:" ; no host ok
   "\\|"
   "\\(ftp\\|http\\|telnet\\|gopher\\|www\\|wais\\)://" ; needs host
   "\\)."				; require one more character
   )
   "Regexp matching url's.  Nil to disable url features in ffap.")

(defvar ffap-foo-at-bar-prefix "mailto"
  "*Presumed url prefix type of strings like \"<foo.9z@bar>\".
Sensible values are nil, \"news\", or \"mailto\".")


;;; Peanut Gallery:
;;
;; Users of ffap occasionally suggest new features.  If I consider
;; those features interesting but not clear winners (a matter of
;; personal taste) I try to leave options to enable them.  Read
;; through this section for features that you like, put an appropriate
;; enabler in your .emacs file.

(defvar ffap-dired-wildcards nil	; "[*?][^/]*$"
  ;; Suggestion from RHOGEE, 07 Jul 1994.  Disabled, dired is still
  ;; available by "C-x C-d <pattern>", and valid filenames may
  ;; sometimes contain wildcard characters.
  "*A regexp matching filename wildcard characters, or nil.
If `find-file-at-point' gets a filename matching this pattern,
it passes it on to `dired' instead of `find-file'.")

(defvar ffap-newfile-prompt nil		; t
  ;; Suggestion from RHOGEE, 11 Jul 1994.  Disabled, I think this is
  ;; better handled by `find-file-not-found-hooks'.
  "*Whether `find-file-at-point' prompts about a nonexistent file.")

(defvar ffap-require-prefix nil
  ;; Suggestion from RHOGEE, 20 Oct 1994.
  "*If set, reverses the prefix argument to `find-file-at-point'.
This is nil so neophytes notice ffap.  Experts may prefer to disable
ffap most of the time.")

(defvar ffap-file-finder 'find-file
  "*The command called by `find-file-at-point' to find a file.")
(put 'ffap-file-finder 'risky-local-variable t)

(defvar ffap-url-fetcher
  (cond ((fboundp 'w3-fetch) 'w3-fetch)
	((fboundp 'browse-url-netscape) 'browse-url-netscape)
	(t 'w3-fetch))
  ;; Remote control references:
  ;; http://www.ncsa.uiuc.edu/SDG/Software/XMosaic/remote-control.html
  ;; http://home.netscape.com/newsref/std/x-remote.html
  "*A function of one argument, called by ffap to fetch an URL.
Reasonable choices are `w3-fetch' or `browse-url-netscape'.
For a fancier alternative, get ffap-url.el.")
(put 'ffap-url-fetcher 'risky-local-variable t)


;;; Command ffap-next:
;;
;; Original ffap-next-url (URL's only) from RPECK 30 Mar 1995.  Since
;; then, broke it up into ffap-next-guess (noninteractive) and
;; ffap-next (a command).  It now work on files as well as url's.

(defvar ffap-next-regexp
  ;; If you want ffap-next to find URL's only, try this:
  ;; (and ffap-url-regexp (string-match "\\\\`" ffap-url-regexp)
  ;;	  (concat "\\<" (substring ffap-url-regexp 2))))
  ;;
  ;; It pays to put a big fancy regexp here, since ffap-guesser is
  ;; much more time-consuming than regexp searching:
  "[/:.~a-zA-Z]/\\|@[a-zA-Z][-a-zA-Z0-9]*\\."
  "*Regular expression governing movements of `ffap-next'.")

(defvar ffap-next-guess nil "Last value returned by `ffap-next-guess'.")
(defun ffap-next-guess (&optional back lim)
  "Move point to next file or url, and return it as a string.
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
  "Search buffer for next file or url, and run ffap.
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
      (message "No %sfiles or URL's found."
	       (if wrap "" "more ")))))

(defun ffap-next-url (&optional back wrap)
  "Like `ffap-next', but search with `ffap-url-regexp'."
  (interactive)
  (let ((ffap-next-regexp ffap-url-regexp))
    (if (interactive-p)
	(call-interactively 'ffap-next)
      (ffap-next back wrap))))


;;; Remote machines and paths:

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

(defun ffap-file-exists-string (file)
  ;; With certain packages (ange-ftp, jka-compr?) file-exists-p
  ;; sometimes returns a nicer string than it is given.  Otherwise, it
  ;; just returns nil or t.
  "Return FILE \(maybe modified\) if it exists, else nil."
  (and file				; quietly reject nil
       (let ((exists (file-exists-p file)))
	 (and exists (if (stringp exists) exists file)))))

;; I cannot decide a "best" strategy here, so these are variables.  In
;; particular, if `Pinging...' is broken or takes too long on your
;; machine, try setting these all to accept or reject.
(defvar ffap-machine-p-local 'reject	; this happens often
  "*A symbol, one of: ping, accept, reject.
What `ffap-machine-p' does with hostnames that have no domain.")
(defvar ffap-machine-p-known 'ping	; 'accept for speed
  "*A symbol, one of: ping, accept, reject.
What `ffap-machine-p' does with hostnames that have a known domain
\(see mail-extr.el for the known domains\).")
(defvar ffap-machine-p-unknown 'reject
  "*A symbol, one of: ping, accept, reject.
What `ffap-machine-p' does with hostnames that have an unknown domain
\(see mail-extr.el for the known domains\).")

(defun ffap-what-domain (domain)
  ;; Like what-domain in mail-extr.el, returns string or nil.
  (require 'mail-extr)
  (defvar mail-extr-all-top-level-domains
    (ffap-soft-value "all-top-level-domains" obarray)) ; XEmacs, old Emacs
  (get (intern-soft (downcase domain) mail-extr-all-top-level-domains)
       'domain-name))

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

(defun ffap-file-remote-p (filename)
  "If FILENAME looks remote, return it \(maybe slightly improved\)."
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
  "Convert HOST to something like \"/anonymous@HOST:\".
Looks at `ffap-ftp-default-user', returns \"\" for \"localhost\"."
  (if (equal host "localhost") ""
    (concat "/"
	    ffap-ftp-default-user (and ffap-ftp-default-user "@")
	    host ":")))

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
	     (and (intern-soft string (symbol-value htb))
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
(defvar ffap-newsgroup-regexp "^[a-z]+\\.[-+a-z_0-9.]+$"
  "Strings not matching this fail `ffap-newsgroup-p'.")
(defvar ffap-newsgroup-heads		; entirely inadequate
  '("alt" "comp" "gnu" "misc" "news" "sci" "soc" "talk")
  "Used by `ffap-newsgroup-p' if gnus is not running.")

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
   ;; Do not load w3 just for this:
   (t (let ((normal (and (fboundp 'url-normalize-url)
			 (url-normalize-url url))))
	;; In case url-normalize-url is confused:
	(or (and normal (not (zerop (length normal))) normal)
	    url)))))


;;; `ffap-alist':
;;
;; Search actions depending on the major-mode or extensions of the
;; current name.  Note all the little defun's could be broken out, at
;; some loss of locality.  A good example of featuritis.

;; First, some helpers for functions in `ffap-alist':

(defvar path-separator ":")		; for XEmacs 19.13

(defun ffap-list-env (env &optional empty)
  ;; Replace this with parse-colon-path (lisp/files.el)?
  "Directory list parsed from path envinronment variable ENV.
Optional EMPTY is default if (getenv ENV) is undefined, and is also
substituted for the first empty-string component, if there is one.
Uses `path-separator' to separate the path into directories."
  ;; Derived from psg-list-env in RHOGEE's ff-paths and
  ;; bib-cite packages.  The `empty' argument is intended to mimic
  ;; the semantics of TeX/BibTeX variables, it is substituted for
  ;; any empty string entry.
  (if (or empty (getenv env))		; should return something
      (let ((start 0) match dir ret)
	(setq env (concat (getenv env) path-separator))
	(while (setq match (string-match path-separator env start))
	  (setq dir (substring env start match) start (1+ match))
	  ;;(and (file-directory-p dir) (not (member dir ret)) ...)
	  (setq ret (cons dir ret)))
	(setq ret (nreverse ret))
	(and empty (setq match (member "" ret))
	     (progn
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

(defun ffap-add-subdirs (path)
  "Return PATH augmented with its immediate subdirectories."
  ;; (ffap-add-subdirs '("/notexist" "~"))
  (let (ret subs)
    (while path
      (mapcar
       (function
	(lambda (f) (and (file-directory-p f) (setq ret (cons f ret)))))
       (condition-case nil
	   (directory-files (car path) t "[^.]")
	 (error nil)))
      (setq ret (cons (car path) ret)
	    path (cdr path)))
    (nreverse ret)))

(defvar ffap-locate-jka-suffixes t
  "List of compression suffixes tried by `ffap-locate-file'.
If not a list, it is initialized by `ffap-locate-file',
and it becomes nil unless you are using jka-compr.
Typical values are nil or '(\".gz\" \".z\" \".Z\").")

(defun ffap-locate-file (file &optional nosuffix path)
  "A generic path-searching function, mimics `load' by default.
Returns path to file that \(load FILE\) would load, or nil.
Optional NOSUFFIX, if nil or t, is like the fourth argument
for load: whether to try the suffixes (\".elc\" \".el\" \"\").
If a nonempty list, it is a list of suffixes to try instead.
Optional PATH is a list of directories instead of `load-path'."
  (or path (setq path load-path))
  (if (file-name-absolute-p file)
      (setq path (list (file-name-directory file))
	    file (file-name-nondirectory file)))
  (let ((suffixes-to-try
	 (cond
	  ((consp nosuffix) nosuffix)
	  (nosuffix '(""))
	  (t '(".elc" ".el" "")))))
    ;; Modern (>19.27) jka-compr doesn't try foo.gz when you want foo.
    (or (listp ffap-locate-jka-suffixes)
	(setq ffap-locate-jka-suffixes
	      (and (featurep 'jka-compr)
		   (not (featurep 'jka-aux))
		   jka-compr-file-name-handler-entry
		   (not (string-match
			 (car jka-compr-file-name-handler-entry)
			 "foo"))
		   ;; Hard to do this cleverly across jka-compr versions:
		   '(".gz" ".Z"))))
    (if ffap-locate-jka-suffixes	; so nil behaves like '("")
	(setq suffixes-to-try
	      (apply
	       'nconc
	       (mapcar
		(function
		 (lambda (suf)
		   (cons suf
			 (mapcar
			  (function (lambda (x) (concat suf x)))
			  ffap-locate-jka-suffixes))))
		suffixes-to-try))))
    (let (found suffixes)
      (while (and path (not found))
	(setq suffixes suffixes-to-try)
	(while (and suffixes (not found))
	  (let ((try (expand-file-name
		      (concat file (car suffixes))
		      (car path))))
	    (if (and (file-exists-p try) (not (file-directory-p try)))
		(setq found try)))
	  (setq suffixes (cdr suffixes)))
	(setq path (cdr path)))
      found)))

(defvar ffap-alist
  ;; A big mess!  Parts are probably useless.
  (list
   (cons "\\.info\\'"
	 (defun ffap-info (name)
	   (ffap-locate-file
	    name '("" ".info")
	    (or (ffap-soft-value "Info-directory-list")
		(ffap-soft-value "Info-default-directory-list")
		;; v18:
		(list (ffap-soft-value "Info-directory" "~/info/"))))))
   ;; Since so many info files do not have .info extension, also do this:
   (cons "\\`info/"
	 (defun ffap-info-2 (name) (ffap-info (substring name 5))))
   (cons "\\`[-a-z]+\\'"
	 ;; This ignores the node! "(emacs)Top" same as "(emacs)Intro"
	 (defun ffap-info-3 (name)
	   (and (equal (ffap-string-around) "()") (ffap-info name))))
   (cons "\\.elc?\\'"
	 (defun ffap-el (name) (ffap-locate-file name t)))
   (cons 'emacs-lisp-mode
	 (defun ffap-el-mode (name)
	   ;; We do not bother with "" here, since it was considered above.
	   ;; Also ignore "elc", for speed (who else reads elc files?)
	   (and (not (string-match "\\.el\\'" name))
		(ffap-locate-file name '(".el")))))
   '(finder-mode . ffap-el-mode)	; v19: {C-h p}
   '(help-mode . ffap-el-mode)		; v19.29
   (cons 'c-mode
	 (progn
	   ;; Need better defaults here!
	   (defvar ffap-c-path '("/usr/include" "/usr/local/include"))
	   (defun ffap-c-mode (name)
	     (ffap-locate-file name t ffap-c-path))))
   '(c++-mode . ffap-c-mode)
   '(cc-mode . ffap-c-mode)
   '("\\.\\([chCH]\\|cc\\|hh\\)\\'" . ffap-c-mode)
   (cons 'tex-mode
	 ;; Complicated because auctex may not be loaded yet.
	 (progn
	   (defvar ffap-tex-path
	     t				; delayed initialization
	     "Path where `ffap-tex-mode' looks for tex files.
If t, `ffap-tex-init' will initialize this when needed.")
	   (defun ffap-tex-init nil
	     ;; Compute ffap-tex-path if it is now t.
	     (and (eq t ffap-tex-path)
		  (message "Initializing ffap-tex-path ...")
		  (setq ffap-tex-path
			(ffap-reduce-path
			 (append
			  (list ".")
			  (ffap-list-env "TEXINPUTS")
			  ;; (ffap-list-env "BIBINPUTS")
			  (ffap-add-subdirs
			   (ffap-list-env "TEXINPUTS_SUBDIR"
					  (ffap-soft-value
					   "TeX-macro-global"
					   '("/usr/local/lib/tex/macros"
					     "/usr/local/lib/tex/inputs")
					   ))))))))
	   (defun ffap-tex-mode (name)
	     (ffap-tex-init)
	     (ffap-locate-file name '(".tex" "") ffap-tex-path))))
   (cons 'latex-mode
	   (defun ffap-latex-mode (name)
	     (ffap-tex-init)
	     ;; Any real need for "" here?
	     (ffap-locate-file name '(".cls" ".sty" ".tex" "")
			       ffap-tex-path)))
   (cons "\\.\\(tex\\|sty\\|doc\\|cls\\)\\'"
	 (defun ffap-tex (name)
	   (ffap-tex-init)
	   (ffap-locate-file name t ffap-tex-path)))
   (cons "\\.bib\\'"
	 (defun ffap-bib (name)
	   (ffap-locate-file
	    name t
	    (ffap-list-env "BIBINPUTS" '("/usr/local/lib/tex/macros/bib")))))
   (cons 'math-mode
	 (defun ffap-math-mode (name)
	   (while (string-match "`" name)
	     (setq name (concat (substring name 0 (match-beginning 0))
				"/"
				(substring name (match-end 0)))))
	   (ffap-locate-file
	    name '(".m" "") (ffap-soft-value "Mathematica-search-path"))))
   (cons "\\`\\." (defun ffap-home (name) (ffap-locate-file name t '("~"))))
   (cons "\\`~/"
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
	     (substring name 2)))))
   (cons "^[Rr][Ff][Cc][- #]?\\([0-9]+\\)" ; no $
	 (progn
	   (defvar ffap-rfc-path
	     (concat (ffap-host-to-path "ds.internic.net") "/rfc/rfc%s.txt"))
	   (defun ffap-rfc (name)
	     (format ffap-rfc-path
		     (substring name (match-beginning 1) (match-end 1))))))
   (cons "\\`[^/]*\\'"
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
		    try)))))
   )
  "Alist of \(KEY . FUNCTION\) pairs parsed by `ffap-file-at-point'.
If string NAME at point (maybe \"\") is not a file or url, these pairs
specify actions to try creating such a string.  A pair matches if either
  KEY is a symbol, and it equals `major-mode', or
  KEY is a string, it should matches NAME as a regexp.
On a match, \(FUNCTION NAME\) is called and should return a file, an
url, or nil. If nil, search the alist for further matches.")

(put 'ffap-alist 'risky-local-variable t)


;;; At-Point Functions:

(defvar ffap-string-at-point-mode-alist
  '(
    ;; The default, used when the `major-mode' is not found.
    ;; Slightly controversial decisions:
    ;; * strip trailing "@" and ":"
    ;; * no commas (good for latex)
    (file "--:$+<>@-Z_a-z~" "<@" "@>;.,!?:")
    ;; An url, or maybe a email/news message-id:
    (url "--:?$+@-Z_a-z~#,%" "^A-Za-z0-9" ":;.,!?")
    ;; Find a string that does *not* contain a colon:
    (nocolon "--9$+<>@-Z_a-z~" "<@" "@>;.,!?")
    ;; A machine:
    (machine "-a-zA-Z0-9." "" ".")
    ;; Mathematica paths: allow backquotes
    (math-mode ",-:$+<>@-Z_a-z~`" "<" "@>;.,!?`:")
    )
  "Alist of \(MODE CHARS BEG END\), where MODE is a symbol,
possibly a `major-mode' or some symbol internal to ffap
\(such as 'file, 'url, 'machine, and 'nocolon\).
`ffap-string-at-point' uses the data fields as follows:
1. find a maximal string of CHARS around point,
2. strip BEG chars before point from the beginning,
3. Strip END chars after point from the end.")

(defvar ffap-string-at-point-region '(1 1)
  "List (BEG END), last region returned by `ffap-string-at-point'.")

(defvar ffap-string-at-point nil
  ;; Added at suggestion of RHOGEE (for ff-paths), 7/24/95.
  "Last string returned by `ffap-string-at-point'.")

(defun ffap-string-at-point (&optional mode)
  "Return a string of characters from around point.
MODE (defaults to `major-mode') is a symbol used to lookup string
syntax parameters in `ffap-string-at-point-mode-alist'.
If MODE is not found, we fall back on the symbol 'file.
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
    (or ffap-xemacs (set-text-properties 0 (length str) nil str))
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
  ;; ffap be non-rabid in such situations.
  (and
   ffap-url-regexp
   (or
    ;; In a w3 buffer button zone?
    (let (tem)
      (and (eq major-mode 'w3-mode)
	   ;; assume: (boundp 'w3-zone-at) (boundp 'w3-zone-data)
	   (setq tem (w3-zone-at (point)))
	   (consp (setq tem (w3-zone-data tem)))
	   (nth 2 tem)))
    ;; Is there a reason not to strip trailing colon?
    (let ((name (ffap-string-at-point 'url)))
      ;; (case-fold-search t), why?
      (cond
       ((string-match "^url:" name) (setq name (substring name 4)))
       ((and (string-match "\\`[^:</>@]+@[^:</>@]+[a-zA-Z]\\'" name)
	     ;; "foo@bar": could be "mailto" or "news" (a Message-ID).
	     ;; If not adorned with "<>", it must be "mailto".
	     ;;	Otherwise could be either, so consult `ffap-foo-at-bar-prefix'.
	     (let ((prefix (if (and (equal (ffap-string-around) "<>")
				    ;; At least a couple of odd characters:
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
	 ((member name '("" "/" "//")) nil)
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
		  (and (setq try (funcall (cdr tem) name))
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
      (store-match-data data))))


;;; ffap-read-file-or-url:
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
	      (setq guess (abbreviate-file-name (expand-file-name guess))))
	  (setq dir (file-name-directory guess))))
    (setq guess
	  (completing-read
	   prompt
	   'ffap-read-file-or-url-internal
	   dir
	   nil
	   (if dir (cons guess (length dir)) guess)
	   (list 'file-name-history)
	   ))
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


;;; Highlighting:
;;
;; Based on overlay highlighting in Emacs 19.28 isearch.el.

(defvar ffap-highlight (and window-system t)
  "If non-nil, ffap highlights the current buffer substring.")

(defvar ffap-highlight-overlay nil "Overlay used by `ffap-highlight'.")

(defun ffap-highlight (&optional remove)
  "If `ffap-highlight' is set, highlight the guess in this buffer.
That is, the last buffer substring found by `ffap-string-at-point'.
Optional argument REMOVE means to remove any such highlighting.
Uses the face `ffap' if it is defined, or else `highlight'."
  (cond
   (remove (and ffap-highlight-overlay (delete-overlay ffap-highlight-overlay)))
   ((not ffap-highlight) nil)
   (ffap-highlight-overlay
    (move-overlay ffap-highlight-overlay
		  (car ffap-string-at-point-region)
		  (nth 1 ffap-string-at-point-region)
		  (current-buffer)))
   (t
    (setq ffap-highlight-overlay (apply 'make-overlay ffap-string-at-point-region))
    (overlay-put ffap-highlight-overlay 'face
		 (if (internal-find-face 'ffap nil)
		     'ffap 'highlight)))))


;;; The big enchilada:

(defun ffap-guesser nil
  "Return file or url or nil, guessed from text around point."
  (or (and ffap-url-regexp
	   (ffap-fixup-url (or (ffap-url-at-point)
			       (ffap-gopher-at-point))))
      (ffap-file-at-point)		; may yield url!
      (ffap-fixup-machine (ffap-machine-at-point))))

(defun ffap-prompter (&optional guess)
  ;; Does guess and prompt step for find-file-at-point.
  ;; Extra complication for the temporary highlighting.
  (unwind-protect
      (ffap-read-file-or-url
       (if ffap-url-regexp "Find file or URL: " "Find file: ")
       (prog1
	   (setq guess (or guess (ffap-guesser)))
	 (and guess (ffap-highlight))
	 ))
    (ffap-highlight t)))

;;;###autoload
(defun find-file-at-point (&optional filename)
  "Find FILENAME (or url), guessing default from text around point.
If `ffap-dired-wildcards' is set, wildcard patterns are passed to dired.
See also the functions `ffap-file-at-point', `ffap-url-at-point'.
With a prefix, this command behaves *exactly* like `ffap-file-finder'.
If `ffap-require-prefix' is set, the prefix meaning is reversed.

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

;; M-x shortcut:
;;###autoload
(defalias 'ffap 'find-file-at-point)


;;; Menu support:
;;
;; Bind ffap-menu to a key if you want, since it also works in tty mode.
;; Or just use it through the ffap-at-mouse binding (next section).

(defvar ffap-menu-regexp nil
  "*If non-nil, overrides `ffap-next-regexp' during `ffap-menu'.
Make this more restrictive for faster menu building.
For example, try \":/\" for url (and some ftp) references.")

(defvar ffap-menu-alist nil
  "Buffer local cache of menu presented by `ffap-menu'.")
(make-variable-buffer-local 'ffap-menu-alist)

(defvar ffap-menu-text-plist
  (and window-system
       ;; These choices emulate goto-addr:
       (if ffap-xemacs
	   '(face bold highlight t) ; keymap <map>
	 '(face bold mouse-face highlight) ; keymap <mousy-map>
	 ))
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
Arguments are TITLE, ALIST, and CONT (a continuation).
This uses either a menu or the minibuffer depending on invocation.
The TITLE string is used as either the prompt or menu title.
Each \(string . data\) ALIST entry defines a choice \(data is ignored\).
Once the user makes a choice, function CONT is applied to the entry.
Always returns nil."
  ;; Bug: minibuffer prompting assumes the strings are unique.
  (let ((choice
	 (if (and (fboundp 'x-popup-menu) ; Emacs 19 or XEmacs 19.13
		  (boundp 'last-nonmenu-event) ; not in XEmacs 19.13
		  (listp last-nonmenu-event))
	     (x-popup-menu
	      t
	      (list ""
		    (cons title
			  (mapcar
			   (function (lambda (i) (cons (car i) i)))
			   alist))))
	   ;; Immediately popup completion buffer:
	   (prog1
	       (let ((minibuffer-setup-hook 'minibuffer-completion-help))
		 ;; BUG: this code assumes that "" is not a valid choice
		 (completing-read
		  (format "%s (default %s): " title (car (car alist)))
		  alist nil t
		  ;; (cons (car (car alist)) 0)
		  nil
		  ))
	     ;; Redraw original screen:
	     (sit-for 0)))))
    ;; Defaulting: convert "" to (car (car alist))
    (and (equal choice "") (setq choice (car (car alist))))
    (and (stringp choice) (setq choice (assoc choice alist)))
    (if choice (funcall cont choice) (message "No choice made!")))
  nil)					; return nothing

(defun ffap-menu-rescan nil
  "Search buffer for `ffap-menu-regexp' to build `ffap-menu-alist'.
Applies `ffap-menu-text-plist' text properties at all matches."
  (interactive)
  (let ((ffap-next-regexp (or ffap-menu-regexp ffap-next-regexp))
	(range (- (point-max) (point-min))) item
	buffer-read-only		; to set text-properties
	;; Avoid repeated searches of the *mode-alist:
	(major-mode (if (assq major-mode ffap-string-at-point-mode-alist)
			major-mode
		      'file))
	)
    (setq ffap-menu-alist nil)
    (save-excursion
      (goto-char (point-min))
      (while (setq item (ffap-next-guess))
	(setq ffap-menu-alist (cons (cons item (point)) ffap-menu-alist))
	(add-text-properties (car ffap-string-at-point-region) (point)
			     ffap-menu-text-plist)
	(message "Scanning...%2d%% <%s>"
		 (/ (* 100 (- (point) (point-min))) range) item))))
  (message "Scanning...done")
  ;; Remove duplicates.
  (setq ffap-menu-alist			; sort by item
	(sort ffap-menu-alist
	      (function
	       (lambda (a b) (string-lessp (car a) (car b))))))
  (let ((ptr ffap-menu-alist))
    (while (cdr ptr)
      (if (equal (car (car ptr)) (car (car (cdr ptr))))
	  (setcdr ptr (cdr (cdr ptr)))
	(setq ptr (cdr ptr)))))
  (setq ffap-menu-alist			; sort by position
	(sort ffap-menu-alist
	      (function
	       (lambda (a b) (< (cdr a) (cdr b)))))))


;;; Mouse Support:
;;
;; See the suggested binding in ffap-bindings (near eof).

(defvar ffap-at-mouse-fallback 'ffap-menu
  "Invoked by `ffap-at-mouse' if no file or url at click.
A command symbol, or nil for nothing.")
(put 'ffap-at-mouse-fallback 'risky-local-variable t)

(defun ffap-at-mouse (e)
  "Find file or url guessed from text around mouse point.
If none is found, call `ffap-at-mouse-fallback'."
  (interactive "e")
  (let ((guess
	 ;; Maybe less surprising without the save-excursion?
	 (save-excursion
	   (mouse-set-point e)
	   ;; Would like to do nothing unless click was *on* text.  How?
	   ;; (cdr (posn-col-row (event-start e))) is always same as
	   ;; current column.  For posn-x-y, need pixel-width!
	   (ffap-guesser))))
    (cond
     (guess
      (ffap-highlight)
      (unwind-protect
	  (progn
	    (sit-for 0)			; display
	    (message "Guessing `%s'" guess)
	    (find-file-at-point guess))
	(ffap-highlight t)))
     ((and (interactive-p)
	   ffap-at-mouse-fallback)
      (call-interactively ffap-at-mouse-fallback))
     ((message "No file or URL found at mouse click.")))))


;;; ffap-other-* commands
;; Suggested by KPC.

(defun ffap-other-window nil
  "Like `ffap', but put buffer in another window."
  (interactive)
  (switch-to-buffer-other-window
   (save-window-excursion (call-interactively 'ffap) (current-buffer))))

(defun ffap-other-frame nil
  "Like `ffap', but put buffer in another frame."
  (interactive)
  (switch-to-buffer-other-frame
   (save-window-excursion (call-interactively 'ffap) (current-buffer))))


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
     "ffap 1.6"
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


;;; ffap-bindings: offer default global bindings

(defvar ffap-bindings
  (nconc
   (cond
    ((not (eq window-system 'x))
     nil)
    ;; GNU coding standards say packages should not bind S-mouse-*.
    ;; Is it ok to simply suggest such a binding to the user?
    (ffap-xemacs
     '((global-set-key '(shift button3) 'ffap-at-mouse)))
    (t
     '((global-set-key [S-down-mouse-3] 'ffap-at-mouse))))
   '(
     (global-set-key "\C-x\C-f" 'find-file-at-point)
     (global-set-key "\C-x4f"   'ffap-other-window)
     (global-set-key "\C-x5f"   'ffap-other-frame)
     (add-hook 'gnus-summary-mode-hook 'ffap-gnus-hook)
     (add-hook 'gnus-article-mode-hook 'ffap-gnus-hook)
     (add-hook 'vm-mode-hook 'ffap-ro-mode-hook)
     (add-hook 'rmail-mode-hook 'ffap-ro-mode-hook)
     ;; (setq dired-x-hands-off-my-keys t) ; the default
     ))
  "List of forms evaluated by function `ffap-bindings'.
A reasonable ffap installation needs just these two lines:
  (require 'ffap)
  (ffap-bindings)
These are only suggestions, they may be modified or ignored.")

(defun ffap-bindings nil
  "Evaluate the forms in variable `ffap-bindings'."
  (eval (cons 'progn ffap-bindings)))

;; Example modifications:
;;
;; (setq ffap-alist                   ; remove a feature in `ffap-alist'
;;	 (delete (assoc 'c-mode ffap-alist) ffap-alist))
;;
;; (setq ffap-alist                   ; add something to `ffap-alist'
;;	 (cons
;;	  (cons "^[Yy][Ss][Nn][0-9]+$"
;;		(defun ffap-ysn (name)
;;		  (concat
;;		   "http://snorri.chem.washington.edu/ysnarchive/issuefiles/"
;;		   (substring name 3) ".html")))
;;	  ffap-alist))


;;; XEmacs:
;; Extended suppport in another file, for copyright reasons.
(or (not ffap-xemacs)
    (load "ffap-xe" t t)
    (message "ffap warning: ffap-xe.el not found"))


;;; ffap.el ends here
