;;; ffap.el -- find-file-at-point,
;; Copyright (C) 1994, 1995 Free Software Foundation, Inc.

;;; Author: Michelangelo Grigni <mic@mathcs.emory.edu>

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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; A replacement for find-file {C-x C-f}: finds file or URL,
;; guessing default from text at point.  Many features!
;; Send bugs or suggestions with M-x ffap-bug.

;; See ftp://ftp.mathcs.emory.edu:/pub/mic/emacs/ for most recent version:
;;    ffap.el.gz       -- this file, compressed with gzip
;;    ffap-xe.el       -- support code for XEmacs 19.*
;;    COPYING.gz       -- GNU General Public License, version 2
;;    README           -- description of these and other files
;;
;; For the last version sent to elisp-archive@cis.ohio-state.edu, see:
;;    ftp://ftp.cis.ohio-state.edu/pub/gnu/emacs/elisp-archive/misc/ffap.el.Z
;;    (mirrored in gatekeeper.dec.com:/pub/GNU/elisp-archive/misc/)

;;; Description:
;;
;; Command find-file-at-point (== ffap) replaces find-file.  With a
;; prefix, it behaves exactly like find-file.  Without a prefix, it
;; first tries to guess a default file or URL based on the text around
;; the point (set `ffap-require-prefix' to swap these behaviors).
;; This is a quick way to fetch URL and file references in many
;; situations, such as in mail or news messages, README's, and
;; MANIFEST's.
;;
;; Some related commands are ffap-at-mouse, ffap-next, ffap-menu,
;; ffap-other-window, ffap-other-frame.
;;
;; This package is about user convenience.  It adds nothing to the
;; elisp programmer's repertoire.


;;; Installation:

;; Quick Setup:
;;
;; For a basic installation, just install ffap.el somewhere in your
;; `load-path', byte-compile it, and add the following two lines near
;; the end of your ~/.emacs (or equivalent) file:
;;
;; (require 'ffap)                      ; load this file
;; (global-set-key "\C-x\C-f" 'find-file-at-point)
;;
;; Other Packages: ffap notices the presence of several other packages
;; when it is loaded.  In particular, if you use ange-ftp, efs, w3,
;; complete, or ff-paths (version < 3.00), it is best to load or
;; autoload them before loading ffap (ffap does not need any of them).
;; If you use ff-paths version >= 3.00, load it after ffap.

;; Fancier Setup:
;;
;; ffap has many options.  The next comment block contains some
;; fancier code that you might want to adapt for your .emacs.  For
;; even more features, look at the documentation (M-x apropos ffap),
;; and perhaps check the comments in the "User Variables" and "Peanut
;; Gallery" sections of this file.

;; ;; Before loading ffap:
;;
;; (setq ffap-url-regexp nil)           ; to disable all URL features
;;
;; ;; Loading ffap:
;; (require 'ffap)                      ; as in "Quick Setup" above
;;
;; After loading ffap:
;;
;; (global-set-key "\C-x\C-f" 'find-file-at-point) ; as in "Quick Setup"
;; (global-set-key "\C-x4f" 'ffap-other-window) ; or \C-f
;; (global-set-key "\C-x5f" 'ffap-other-frame) ; or \C-f
;;
;; (setq ffap-alist                     ; remove something in `ffap-alist'
;;	 (delete (assoc 'c-mode ffap-alist) ffap-alist))
;;
;; (setq ffap-alist                     ; add something to `ffap-alist'
;;	 (cons
;;	  (cons "^[Yy][Ss][Nn][0-9]+$"
;;		(defun ffap-ysn (name)
;;		  (concat
;;		   "http://snorri.chem.washington.edu/ysnarchive/issuefiles/"
;;		   (substring name 3) ".html")))
;;	  ffap-alist))
;;
;;
;; Before or after loading ffap:
;;
;; (setq ffap-alist nil)                ; disable all `ffap-alist' actions
;;
;; (setq ffap-require-prefix t)         ; without prefix, ffap == find-file
;;
;; (setq ffap-machine-p-known 'accept)  ; to avoid pinging
;;
;; ;; Choose a mouse binding appropriate for your emacs version:
;; (global-set-key [S-mouse-1] 'ffap-at-mouse) ; Emacs 19
;; (global-set-key [(meta button1)] 'ffap-at-mouse) ; XEmacs
;; (and window-system 			; Emacs 18 (from .emacs)
;;      (setq window-setup-hook
;;            '(lambda nil (define-key mouse-map x-button-s-left
;;                           'ffap-at-mouse))))
;;
;; ;; Use Netscape instead of w3 to fetch URL's.  Mosaic is very similar.
;; (if (eq window-system 'x)
;;     (progn
;;       ;; Get browse-url at http://wombat.doc.ic.ac.uk/emacs/browse-url.el,
;;	 ;; or get a (probably out of date) copy from the ftp site above.
;;	 (autoload 'browse-url-netscape "browse-url" nil t)
;;	 (setq ffap-url-fetcher 'browse-url-netscape)))
;; ;; Or for a hairier ffap-url-fetcher, get ffap-url.el (same ftp site).
;;
;; ;; Support for gnus, vm, rmail (see hook definitions for bindings):
;; (add-hook 'gnus-summary-mode-hook 'ffap-gnus-hook)
;; (add-hook 'gnus-article-mode-hook 'ffap-gnus-hook)
;; (add-hook 'vm-mode-hook 'ffap-ro-mode-hook)
;; (add-hook 'rmail-mode-hook 'ffap-ro-mode-hook)


;;; Related packages:
;;
;; If you have hyperbole, you may not need this package, although ffap
;; is smaller and smarter at this particular task.  Also note that w3
;; (ftp.cs.indiana.edu:/pub/elisp/w3/README) offers a similar command
;; w3-follow-url-at-point.
;;
;; The browse-url package (above) notices URL's and hands them off to
;; w3 or an external WWW browser.  Package |~/misc/goto-address.el.gz|
;; by Eric J. Ding <ericding@mit.edu> notices URL's and mail
;; addresses, and can pre-fontify a buffer to highlight them.  Gnus5
;; (ding) and vm also provide similar support in their messages.


;;; Examples:
;;
;; Try M-x find-file-at-point (maybe {C-x C-f}) on these examples.
;; These local file examples use ordinary find-file:
;;
;;    ffap.el, /etc/motd, $MAIL     -- find local or absolute files
;;    .emacs book.sty info/cl pwd.h -- search paths depending on filename
;;    (require 'rmail)              -- search paths depending on major-mode
;;    file:/etc/motd                -- depends on `ffap-url-unwrap-local'
;;
;; These remote file examples work if you have ange-ftp or efs:
;;
;;    ftp:/pub                      -- no ping (always works)
;;    ftp.x.org:README              -- no ping, a nice recursive example
;;    anonymous@ftp.x.org:/README   -- synonym
;;    ftp.x.org://README            -- synonym
;;    ftp://ftp.x.org/README        -- depends on `ffap-url-unwrap-remote'
;;    ftp.mathcs.emory.edu          -- depends on `ffap-machine-p-known'
;;    mic@ftp:/                     -- depends on `ffap-machine-p-local'
;;    ftp.mathcs.emory.edu:/        -- depends on `ffap-ftp-sans-slash-regexp'
;;
;; These URL examples use `ffap-url-fetcher' (default w3-fetch):
;;
;;    http://www.cc.emory.edu
;;    http://www.cs.indiana.edu/elisp/w3/docs.html
;;    http://info.cern.ch/default.html
;;    news:news.newusers.questions
;;    mailto:mic@mathcs.emory.edu
;;    mic@mathcs.emory.edu          -- same as previous
;;    <mic@mathcs.emory.edu>        -- same as previous
;;    <root>                        -- mailto:root
;;    <mic.9@mathcs.emory.edu>      -- see `ffap-foo@bar-prefix'
;;    file:/etc/motd                -- see `ffap-url-unwrap-local'
;;    ftp://ftp.x.org/README        -- see `ffap-url-unwrap-remote'
;;
;; Multiline gopher blocks (as in .gopherrc and usenet of yesteryear):
;;
;;    Type=1
;;    Name=Electronic Texts (ffap ignores this)
;;    Path=
;;    Host=etext.archive.umich.edu
;;    Port=70


;;; Code:

(provide 'ffap)

;;; User Variables:

;; This function is used inside defvars:
(defun ffap-soft-value (name &optional default)
  ;; Avoid interning.  Bug: (ffap-soft-value "nil" 5) --> 5
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
  "*If set, convert local \"file:\" URL to path before prompting.")

(defvar ffap-url-unwrap-remote t
  "*Convert remote \"file:\" or \"ftp:\" URL to path before prompting.
This is ignored if `ffap-ftp-regexp' is nil.")

(defvar ffap-ftp-default-user
  (if (or (equal (ffap-soft-value "ange-ftp-default-user") "anonymous")
	  (equal (ffap-soft-value "efs-default-user") "anonymous"))
      nil
    "anonymous")
  "*User name in ftp paths generated by ffap (see host-to-ftp-path).
Nil to fall back on `efs-default-user' or `ange-ftp-default-user'.")

(defvar ffap-rfs-regexp
  ;; Remote file access built into file system?  HP rfa or Andrew afs:
  "\\`/\\(afs\\|net\\)/."
  ;; afs only: (and (file-exists-p "/afs") "\\`/afs/.")
  "*Paths matching this are remote file-system paths.  Nil to disable.")

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
   "Regexp matching URL's, or nil to disable.")

(defvar ffap-foo@bar-prefix "mailto"
  "*Presumed url prefix type of strings like \"<foo.9z@bar>\".
Sensible values are nil, \"news\", or \"mailto\".")


;;; Peanut Gallery:

;; Users of ffap occasionally suggest new features.  If I consider
;; those features interesting but not clear winners (a matter of
;; personal taste) I try to leave options to enable them.  Read
;; through this section, and for any features you like, put an
;; appropriate form in your ~/.emacs file.

(defvar ffap-dired-wildcards nil	; "[*?][^/]*$"
  ;; From RHOGEE, 07 Jul 1994.
  ;; Disabled: dired is still available by "C-x C-d <pattern>", and
  ;; valid filenames may contain wildcard characters.
  "*A regexp matching filename wildcard characters, or nil.
If find-file-at-point gets a filename matching this pattern,
it passes it on to dired instead of find-file.")

(defvar ffap-newfile-prompt nil		; t
  ;; From RHOGEE, 11 Jul 1994.
  ;; Disabled: this is better handled by `find-file-not-found-hooks'.
  "*Whether find-file-at-point prompts about a nonexistent file.")

(defvar ffap-require-prefix nil
  ;; From RHOGEE, 20 Oct 1994.
  ;; This is nil so that neophytes notice ffap.  Experts instead may
  ;; prefer to disable ffap most of the time.
  "*If set, reverses the prefix argument to find-file-at-point.")

(defvar ffap-file-finder
  ;; From RHOGEE, 20 Oct 1994.
  ;; This allows compatibility with ff-paths version < 3.00.
  ;; For ff-paths version >= 3.00, just load it after ffap.
  (if (commandp 'find-file-using-paths)
      'find-file-using-paths
    ;; Try to overcome load-order dependency:
    (eval-after-load
     "ff-paths"
     '(and (commandp 'find-file-using-paths)
	   (setq ffap-file-finder find-file-using-paths)))
    'find-file)
  "*The command symbol called by find-file-at-point to find a file.
Probably find-file, or find-file-using-paths if you use ff-paths
with version < 3.00.")
(put 'ffap-file-finder 'risky-local-variable t)

(defvar ffap-url-fetcher 'w3-fetch
  "*A function of one argument, called by ffap to fetch URL's.
The default is w3-fetch from the w3 package.  If you prefer Mosaic or
Netscape, install http://wombat.doc.ic.ac.uk/emacs/browse-url.el, and
add one of the following lines to your setup:

\(setq ffap-url-fetcher 'browse-url-netscape\)
\(setq ffap-url-fetcher 'browse-url-mosaic\)

Or for something hairier \(choose fetch method based on url type and
prompting\) get ffap-url.el wherever you ffap.el."
  ;; Big old `lambda' examples deleted. Some remote-control references:
  ;; http://www.ncsa.uiuc.edu/SDG/Software/XMosaic/remote-control.html
  ;; http://home.netscape.com/newsref/std/x-remote.html
  )
(put 'ffap-url-fetcher 'risky-local-variable t)


;;; Command ffap-next:
;;
;; Original ffap-next-url (URL's only) from RPECK 30 Mar 1995.
;; Since then, broke up into ffap-next-guess (noninteractive) and
;; ffap-next (a command), now work on files as well as url's.

(defvar ffap-next-regexp
  ;; If you want ffap-next to find URL's only, try this:
  ;; (and ffap-url-regexp (string-match "\\\\`" ffap-url-regexp)
  ;;	  (concat "\\<" (substring ffap-url-regexp 2))))
  ;;
  ;; It pays to put a big fancy regexp here, since ffap-guesser is
  ;; much more time-consuming than regexp searching:
  "[/:.~a-zA-Z]/\\|@[a-zA-Z][-a-zA-Z0-9]*\\."
  "*Regular expression governing search of ffap-next.")

(defvar ffap-next-guess nil "Last value returned by `ffap-next-guess'.")
(defun ffap-next-guess (&optional back lim)
  "Move point to next file or url, and return it as a string.
If nothing found, leaves point at limit and returns nil.
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
Actual search is done by ffap-next-guess."
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
  "Just like ffap-next, but searches with `ffap-url-regexp'."
  (interactive)
  (let ((ffap-next-regexp ffap-url-regexp))
    (if (interactive-p)
	(call-interactively 'ffap-next)
      (ffap-next back wrap))))


;;; Hooks for GNUS, VM, Rmail:
;;
;; See "Installation" above for suggested use of these hooks.
;; If you do not like these bindings, just write hooks with
;; whatever bindings you would prefer.
;;
;; Any suggestions of more "memorable" bindings? -- Mic

(defun ffap-ro-mode-hook nil
  "Binds ffap-gnus-next and ffap-gnus-menu to M-l and M-m, resp."
  (local-set-key "\M-l" 'ffap-next)
  (local-set-key "\M-m" 'ffap-menu)
  )

(defun ffap-gnus-hook nil
  "Binds ffap-gnus-next and ffap-gnus-menu to L and M, resp."
  (set (make-local-variable 'ffap-foo@bar-prefix) "news") ; message-id's
  ;; Note lowercase l and m are taken:
  (local-set-key "L" 'ffap-gnus-next)
  (local-set-key "M" 'ffap-gnus-menu))

(defun ffap-gnus-wrapper (form)		; used by both commands below
  (and (eq (current-buffer) (get-buffer gnus-summary-buffer))
       (gnus-summary-select-article))	; get article of current line
  ;; Preserve selected buffer, but do not do save-window-excursion,
  ;; since we want to see any window created by form.  Temporarily
  ;; select the article buffer, so we see any point movement.
  (let ((sb (window-buffer (selected-window))))
    (gnus-configure-windows 'article)
    (pop-to-buffer gnus-article-buffer)
    (widen)
    ;; Skip headers at first, for ffap-gnus-next (which wraps around)
    (if (eq (point) (point-min)) (search-forward "\n\n" nil t))
    (unwind-protect
	(eval form)
      (pop-to-buffer sb))))

(defun ffap-gnus-next nil
  "Run ffap-next in the GNUS article buffer."
  (interactive) (ffap-gnus-wrapper '(ffap-next nil t)))

(defun ffap-gnus-menu nil
  "Run ffap-menu in the GNUS article buffer."
  (interactive) (ffap-gnus-wrapper '(ffap-menu)))


;;; Remote machines and paths:

(fset 'ffap-replace-path-component
      (if (or (featurep 'efs)
	      (and
	       (boundp 'file-name-handler-alist) ; v19
	       (rassq 'efs-file-handler-function file-name-handler-alist)))
	  'efs-replace-path-component
	'ange-ftp-replace-name-component))

(defun ffap-file-exists-string (file)
  ;; With certain packages (ange-ftp, jka-compr?) file-exists-p
  ;; sometimes returns a nicer string than it is given.  Otherwise, it
  ;; just returns nil or t.
  "Return FILE \(maybe modified\) if it exists, else nil."
  (let ((exists (file-exists-p file)))
    (and exists (if (stringp exists) exists file))))

;; I cannot decide a "best" strategy here, so these are variables.  In
;; particular, if `Pinging...' is broken or takes too long on your
;; machine, try setting these all to accept or reject.
(defvar ffap-machine-p-local 'reject	; this happens often
  "A symbol, one of: ping, accept, reject.
This is what ffap-machine-p does with hostnames that have no domain.")
(defvar ffap-machine-p-known 'ping	; 'accept for speed
  "A symbol, one of: ping, accept, reject.
This is what ffap-machine-p does with hostnames that have a known domain
\(see lisp/mail-extr.el for the list of known domains\).")
(defvar ffap-machine-p-unknown 'reject
  "A symbol, one of: ping, accept, reject.
This is what ffap-machine-p does with hostnames that have an unknown domain
\(see lisp/mail-extr.el for the list of known domains\).")

(defvar ffap-machine-p-known-domains
  '("com" "edu" "net" "org" "mil" "gov" "us" "arpa") ; USA USA...
  ;; This variable is mainly for emacs18.
  "Top-level domains known to ffap.  Ignored if mail-extr is loadable.")

(defun ffap-machine-p (host &optional service quiet)
  "Indicate whether HOST is the name of a real machine.
The variables ffap-machine-p-local, ffap-machine-p-known, and ffap-machine-p-unknown
control ffap-machine-p depending on HOST's domain \(none/known/unknown\).
Pinging is done using open-network-stream to decide HOST existence.
Optional SERVICE specifies the service used \(default \"discard\"\).
Optional QUIET flag suppresses the \"Pinging...\" message.
Returned values:
A t value means that HOST answered.
A symbol \(accept\) means the relevant variable told us to accept.
A string means the machine exists, but does not respond for some reason."
  ;; Try some:
  ;; (ffap-machine-p "ftp")
  ;; (ffap-machine-p "nonesuch")
  ;; (ffap-machine-p "ftp.mathcs.emory.edu")
  ;; (ffap-machine-p "foo.bonk")
  ;; (ffap-machine-p "foo.bonk.com")
  ;; (ffap-machine-p "cs" 5678)
  ;; (ffap-machine-p "gopher.house.gov")
  ;; Not known to 19.28
  ;; (ffap-
  (if (or (string-match "[^-a-zA-Z0-9.]" host) ; Illegal chars (?)
	  (not (string-match "[^0-9]" host))) ; all numeric! reject it
      nil
    (let* ((domain
	    (and (string-match "\\.[^.]*$" host)
		 (downcase (substring host (1+ (match-beginning 0))))))
	   (domain-name			; t, "Country", "Local", or nil
	    (cond
	     ((not domain) "Local")
	     ;; common non-country domains (some imply US though):
	     ;;	t)
	     (t
	      ;; Use domain-name properties from v19 lisp/mail-extr.el;
	      ;; bbdb/mail-extr also puts this in `all-top-level-domains'.
	      (if (or (featurep 'mail-extr)
		      (and (load "mail-extr" t t)
			   ;; It became a feature between 19.22 and 19.28
			   (provide 'mail-extr)))
		  (get (intern-soft
			domain
			(condition-case nil
			    mail-extr-all-top-level-domains
			  ;; Before 19.28, the symbols were in `obarray':
			  (error obarray)))
		       'domain-name)
		;; Emacs18 does not have mail-extr:
		(and (member domain ffap-machine-p-known-domains) t))
	      )))
	   (strategy
	    (cond ((not domain) ffap-machine-p-local)
		  ((not domain-name) ffap-machine-p-unknown)
		  (ffap-machine-p-known))))
      (cond
       ((eq strategy 'accept) 'accept)
       ((eq strategy 'reject) nil)
       ;; assume (eq strategy 'ping)
       (t
	(or quiet
	    (if (stringp domain-name)
		(message "Pinging %s (%s)..." host domain-name)
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
		 ;; Other errors mean host exists:
		 (nth 2 error)))
	      ;; Could be "Unknown service":
	      (t (signal (car error) (cdr error))))))))))))

(defun ffap-file-remote-p (filename)
  "If FILENAME looks remote, return it \(maybe slightly improved\)."
  ;; (ffap-file-remote-p "/user@foo.bar.com:/pub")
  ;; (ffap-file-remote-p "/foo.dom://path")
  (or (and ffap-ftp-regexp
	   (string-match ffap-ftp-regexp filename)
	   ;; Convert "/host://path" to "/host:/path", to handle a dieing
	   ;; practice of advertising ftp paths as "host.dom://path".
	   (if (string-match "//" filename)
	       (concat (substring filename 0 (match-beginning 0))
		       (substring filename (1- (match-end 0))))
	     filename))
      (and ffap-rfs-regexp
	   (string-match ffap-rfs-regexp filename)
	   filename)))

(defun ffap-machine-at-point nil
  "Return machine name from around point if it exists, or nil."
  (let ((mach (ffap-string-at-point "-a-zA-Z0-9." nil ".")))
    (and (ffap-machine-p mach) mach)))

(defun ffap-fixup-machine (mach)
  ;; Convert a machine into an URL, an ftp path, or nil.
  (cond
   ((not (and ffap-url-regexp (stringp mach))) nil)
   ((string-match "\\`gopher[-.]" mach)	; or "info"?
    (concat "gopher://" mach "/"))
   ((and (string-match "\\`w\\(ww\\|eb\\)[-.]" mach))
    (concat "http://" mach "/"))
   ;; More cases?  Maybe "telnet:" for archie?
   (ffap-ftp-regexp (ffap-host-to-path mach))
   ))

(defun ffap-host-to-path (host)
  "Convert \"HOST\" to \"/anonymous@HOST:\" (or \"\" for \"localhost\").
Variable `ffap-ftp-default-user' overrides or suppresses \"anonymous\"."
  (if (equal host "localhost")
      ""
    (if ffap-ftp-default-user
	(concat "/" ffap-ftp-default-user "@" host ":")
      (concat "/" host ":"))))

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
	     ;; If we made it this far, GNUS is running, so ignore "heads":
	     (setq heads nil))
	 (error nil)))
     (or ret (not heads)
	 (let ((head (string-match "\\`\\([a-z]+\\)\\." string)))
	   (and head (setq head (substring string 0 (match-end 1)))
		(member head heads)
		(setq ret string))))
     ;; Ever any need to modify string as a newsgroup name?
     ret)))
(defvar ffap-newsgroup-regexp "^[a-z]+\\.[-+a-z_0-9.]+$"
  "ffap-newsgroup-p quickly rejects strings that do not match this.")
(defvar ffap-newsgroup-heads		; entirely inadequate
  '("alt" "comp" "gnu" "misc" "news" "sci" "soc" "talk")
  "Used by ffap-newsgroup-p if GNUS is not running.")

(defun ffap-url-p (string)
  "If STRING looks like an URL, return it (maybe improved), else nil."
  ;; Does it look like an URL?  Ignore case.
  (let ((case-fold-search t))
    (and ffap-url-regexp (string-match ffap-url-regexp string)
	 ;; I lied, no improvement:
	 string)))

;; Broke these two out of ffap-fixup-url, for sake of ffap-url package.
(defun ffap-url-unwrap-local (url)
  "Return unwrapped local file URL, or nil.  Ignores ffap-* variables."
  (and (string-match "\\`\\(file\\|ftp\\):/?\\([^/]\\|\\'\\)" url)
       (substring url (1+ (match-end 1)))))
(defun ffap-url-unwrap-remote (url)
  "Return unwrapped remote file URL, or nil.  Ignores ffap-* variables."
  (and (string-match "\\`\\(ftp\\|file\\)://\\([^:/]+\\):?\\(/.*\\)" url)
       (concat
	(ffap-host-to-path (substring url (match-beginning 2) (match-end 2)))
	(substring url (match-beginning 3) (match-end 3)))))

(defun ffap-fixup-url (url)
  "Given URL, clean it up and return it.  May become a file name."
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
;; some loss of locality.  I have had a vote for eliminating this
;; from ffap (featuritis)

;; First, some helpers for functions in `ffap-alist':

(defun ffap-list-env (env &optional empty)
  ;; Replace this with parse-colon-path (lisp/files.el)?
  "Directory list parsed from \":\"-separated ENVinronment variable.
Optional EMPTY is default if (getenv ENV) is undefined, and is also
substituted for the first empty-string component, if there is one."
  ;; Derived from psg-list-env in RHOGEE's ff-paths and
  ;; bib-cite packages.  The `empty' argument is intended to mimic
  ;; the semantics of TeX/BibTeX variables, it is substituted for
  ;; any empty string entry.
  (if (or empty (getenv env))		; should return something
      (let ((start 0) match dir ret)
	(setq env (concat (getenv env) ":")) ; note undefined --> ":"
	(while (setq match (string-match ":" env start))
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
  "Remove duplicates or non-dirs from PATH."
  (let (ret tem)
    (while path
      (setq tem path path (cdr path))
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
  "List of compression suffixes that ffap-locate-file tries.
If not a list, it will be initialized by ffap-locate-file,
and it will become nil unless you are using jka-compr.
You might set this to nil or a list like '(\".gz\" \".z\" \".Z\").")

(defun ffap-locate-file (file &optional nosuffix path)
  ;; If this package is only working in v19 now, maybe should
  ;; replace this with a quiet version of locate-library.
  "A generic path-searching function, defaults mimic `load' behavior.
Returns path of an existing FILE that (load FILE) would load, or nil.
Optional second argument NOSUFFIX, if t, is like the fourth argument
for load, i.e. don't try adding suffixes \".elc\" and \".el\".
If a list, it is taken as a list of suffixes to try instead.
Optional third argument PATH specifies a different search path, it
defaults to `load-path'."
  (or path (setq path load-path))
  (if (file-name-absolute-p file)
      (setq path (list (file-name-directory file))
	    file (file-name-nondirectory file)))
  (let ((suffixes-to-try
	 (cond
	  ((consp nosuffix) nosuffix)
	  (nosuffix '(""))
	  (t '(".elc" ".el" "")))))
    ;; Compensate for modern (19.28) jka-compr, that no longer searches
    ;; for foo.gz when you asked for foo:
    (or (listp ffap-locate-jka-suffixes)
	(setq ffap-locate-jka-suffixes
	      (and (featurep 'jka-compr) ; an early version was jka-compr19
		   (not (featurep 'jka-aux))
		   jka-compr-file-name-handler-entry
		   (not (string-match
			 (car jka-compr-file-name-handler-entry)
			 "foo"))
		   ;; Hard to do cleverly across various jka-compr versions:
		   '(".gz" ".Z"))))
    (if ffap-locate-jka-suffixes
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
	   ;; Need better default here:
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
				)))))
	     "*Where ffap-tex-mode looks for tex files.")
	   (defun ffap-tex-mode (name)
	     (ffap-locate-file name '(".tex" "") ffap-tex-path))))
   (cons 'latex-mode
	   (defun ffap-latex-mode (name)
	     ;; Any real need for "" here?
	     (ffap-locate-file name '(".sty" ".tex" "") ffap-tex-path)))
   (cons "\\.\\(tex\\|sty\\|doc\\)\\'"
	 (defun ffap-tex (name)
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
   )
  "Alist of \(KEY . FUNCTION\), applied to text around point.

If ffap-file-at-point has a string NAME (maybe \"\") which is not an
existing filename, it looks for pairs with a matching KEY:
  * if KEY is a symbol, it should equal `major-mode'.
  * if KEY is a string, it should match NAME as a regular expression.
If KEY matches, ffap-file-at-point calls \(FUNCTION NAME\).
FUNCTION should return a file, url, or nil \(nil means keep looking
for more KEY matches\).  Note URL's are ok despite the function name.")
(put 'ffap-alist 'risky-local-variable t)


;;; At-Point Functions:

(defvar ffap-string-at-point-mode-alist
  '(
    ;; Slightly controversial decisions:
    ;; * strip trailing "@" and ":"
    ;; * no commas (good for latex)
    (t "--:$+<>@-Z_a-z~" "<@" "@>;.,!?:")
    (math-mode ",-:$+<>@-Z_a-z~`" "<" "@>;.,!?`:") ; allow backquote
    ;; Note: you are better off using "C-c C-c" in compilation buffers:
    ;; Maybe handle "$HOME", or "$(HOME)/bin/foo" in makefile-mode?
    )
  "Alist of \(MODE CHARS BEG END\), where MODE is a major-mode or t.
The data are arguments to ffap-string-at-point, used to guess the
filename at point.  The `t' entry is the default.")

(defvar ffap-string-at-point-region '(1 1)
  "List (BEG END), last region returned by ffap-string-at-point.")

(defvar ffap-string-at-point nil
  ;; Added at suggestion of RHOGEE (for ff-paths), 7/24/95.
  "Last string returned by ffap-string-at-point.")
(defun ffap-string-at-point (&optional chars begpunct endpunct)
  "Return maximal string of CHARS (a string) around point.
Optional BEGPUNCT chars before point are stripped from the beginning;
Optional ENDPUNCT chars after point are stripped from the end.
Without arguments, uses `ffap-string-at-point-mode-alist'.
Also sets `ffap-string-at-point' and `ffap-string-at-point-region'."
  (if chars
      (let* ((pt (point))
	     (str
	      (buffer-substring
	       (save-excursion
		 (skip-chars-backward chars)
		 (and begpunct (skip-chars-forward begpunct pt))
		 (setcar ffap-string-at-point-region (point)))
	       (save-excursion
		 (skip-chars-forward chars)
		 (and endpunct (skip-chars-backward endpunct pt))
		 (setcar (cdr ffap-string-at-point-region) (point))))))
	(set-text-properties 0 (length str) nil str)
	(setq ffap-string-at-point str))
    ;; Get default args from `ffap-string-at-point-mode-alist'
    (apply 'ffap-string-at-point
	   (cdr (or (assq major-mode ffap-string-at-point-mode-alist)
		    (assq t ffap-string-at-point-mode-alist)
		    ;; avoid infinite loop!
		    (error "ffap-string-at-point: bad alist")
		    )))))

(defun ffap-string-around nil
  ;; Sometimes useful to decide how to treat a string.
  "Return string of two characters around last ffap-string-at-point."
  (save-excursion
    (format "%c%c"
	    (progn
	      (goto-char (car ffap-string-at-point-region))
	      (preceding-char))		; maybe 0
	    (progn
	      (goto-char (nth 1 ffap-string-at-point-region))
	      (following-char))		; maybe 0
	    )))

(defun ffap-url-at-point nil
  "Return URL from around point if it exists, or nil."
  ;; Could use url-get-url-at-point instead ... how do they compare?
  ;; Both handle "URL:", ignore non-relative links, trim punctuation.
  ;; The other will actually look back if point is in whitespace, but
  ;; I would rather ffap be non-rabid in such situations.
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
    (let ((name (ffap-string-at-point
		 ;; Allow leading digits for email/news id's:
		 "--:?$+@-Z_a-z~#,%" "^A-Za-z0-9" ":;.,!?")))
      ;; (case-fold-search t), why?
      (cond
       ((string-match "^url:" name) (setq name (substring name 4)))
       ((and (string-match "\\`[^:</>@]+@[^:</>@]+[a-zA-Z]\\'" name)
	     ;; "foo@bar": could be "mailto" or "news" (a Message-ID).
	     ;; If not adorned with "<>", it must be "mailto".
	     ;;	Otherwise could be either, so consult `ffap-foo@bar-prefix'.
	     (let ((prefix (if (and (equal (ffap-string-around) "<>")
				    ;; At least a couple of odd characters:
				    (string-match "[$.0-9].*[$.0-9].*@" name))
			       ;; Could be news:
			       ffap-foo@bar-prefix
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
Two subexpressions are the KEY and VALUE.")

(defun ffap-gopher-at-point nil
  "If point is inside a gopher bookmark block, return its url."
  ;; We could use gopher-parse-bookmark from gopher.el, but it is not
  ;; so robust, and w3 users are better off without gopher.el anyway.
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
   ;; Note: by now, we know it is not an URL.
   ;; Icky regexp avoids: default: 123: foo::bar cs:pub
   ;; It does match on: mic@cs: cs:/pub mathcs.emory.edu: (point at end)
   ;; Todo: handle foo.com://path
   "\\`\\([^:@]+@[^:@]+:\\|[^@.:]+\\.[^@:]+:\\|[^:]+:[~/]\\)\\([^:]\\|\\'\\)")
  "Strings matching this are coerced to ftp paths by ffap.
That is, ffap just prepends \"/\".  Set to nil to disable.")

(defun ffap-file-at-point nil
  "Return filename from around point if it exists, or nil.
Existence test is skipped for names that look remote.
If the filename is not obvious, it also tries `ffap-alist',
which may actually result in an URL rather than a filename."
  ;; Note: this function does not need to look for URL's, just
  ;; filenames.  On the other hand, it is responsible for converting
  ;; a pseudo-URL "site.dom://path" to an ftp path "/site.dom:/path"
  (let* ((case-fold-search t)		; url prefixes are case-insensitive
	 (data (match-data))
	 (string (ffap-string-at-point)) ; use its mode-alist
	 (name
	  (condition-case nil
	      (substitute-in-file-name string)
	    (error string)))
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
;; Want to read filenames with completion as in read-file-name, but
;; also allow URL's which read-file-name-internal would truncate at
;; the "//" string.  Solution here is to replace read-file-name-internal
;; with another function that does not attempt to complete url's.

;; We implement a pretty clean completion semantics to work with
;; packages like complete.el and exit-minibuffer.el.  Even for
;; complete.el (v19.22), we still need to make a small patch (it has a
;; hardwired list of `minibuffer-completion-table' values which it
;; considers to deal with filenames, this ought to be a variable).

(defun ffap-read-file-or-url (prompt guess)
  "Read a file or url from minibuffer, with PROMPT and initial GUESS."
  (or guess (setq guess default-directory))
  (let ((filep (not (ffap-url-p guess))) dir)
    ;; Tricky: guess may have or be a local directory, like "w3/w3.elc"
    ;; or "w3/" or "../el/ffap.el" or "../../../"
    (if filep
	(progn
	  (or (ffap-file-remote-p guess)
	      (setq guess (abbreviate-file-name (expand-file-name guess))))
	  (setq dir (file-name-directory guess))))
    (apply
     'completing-read
     prompt
     'ffap-read-file-or-url-internal
     dir
     nil
     (if (and dir) (cons guess (length dir)) guess)
     (list 'file-name-history)
     )))

(defvar url-global-history-completion-list nil)	; variable in w3/url.el

(defun ffap-read-url-internal (string dir action)
  ;; Complete URL's from history, always treat given url as acceptable.
  (let ((hist url-global-history-completion-list))
    (cond
     ((not action)
      (or (try-completion string hist) string))
     ((eq action t)
      (or (all-completions string hist) (list string)))
     ;; lambda?
     (t string))))

(defun ffap-read-file-or-url-internal (string dir action)
  (if (ffap-url-p string)
      (ffap-read-url-internal string dir action)
    (read-file-name-internal string dir action)))

;; Unfortunately, for complete.el to work correctly, we need to vary
;; the value it sees of minibuffer-completion-table, depending on the
;; current minibuffer contents!  It would be nice if it were written a
;; little more easily.  I consider this a bug in complete.el, since
;; the builtin emacs functions do not have this problem.
(and
 (featurep 'complete)
 (require 'advice)
 (defadvice PC-do-completion (around ffap-fix act)
   "Work with ffap.el."
   (let ((minibuffer-completion-table minibuffer-completion-table)
	 ;; (minibuffer-completion-predicate minibuffer-completion-predicate)
	 )
     (and (eq minibuffer-completion-table 'ffap-read-file-or-url-internal)
	  (setq minibuffer-completion-table
		(if (ffap-url-p (buffer-string))
		    ;; List would work better with icomplete ...
		    'ffap-read-url-internal
		  'read-file-name-internal)))
     ad-do-it)))


;;; Highlighting:
;;
;; Based on overlay highlighting in Emacs 19.28 isearch.el.

(defvar ffap-highlight (and window-system t)
  "If non-nil, ffap highlights the current buffer substring.")

(defvar ffap-overlay nil "Overlay used by ffap-highlight.")

(defun ffap-highlight (&optional remove)
  "If `ffap-highlight' is set, highlight the guess in the buffer.
That is, the last buffer substring found by ffap-string-at-point.
Optional argument REMOVE means to remove any such highlighting.
Uses the face `ffap' if it is defined, else `highlight'."
  (cond
   (remove (and ffap-overlay (delete-overlay ffap-overlay)))
   ((not ffap-highlight) nil)
   (ffap-overlay
    (move-overlay ffap-overlay
		  (car ffap-string-at-point-region)
		  (nth 1 ffap-string-at-point-region)
		  (current-buffer)))
   (t
    (setq ffap-overlay (apply 'make-overlay ffap-string-at-point-region))
    (overlay-put ffap-overlay 'face
		 (if (internal-find-face 'ffap nil)
		     'ffap 'highlight)))))

;;; The big enchilada:

(defun ffap-guesser nil
  "Return file or URL or nil, guessed from text around point."
  (or (and ffap-url-regexp
	   (ffap-fixup-url (or (ffap-url-at-point)
			       (ffap-gopher-at-point))))
      (ffap-file-at-point)		; may yield url!
      (ffap-fixup-machine (ffap-machine-at-point))))

(defun ffap-prompter (&optional guess)
  ;; Does guess and prompt step for find-file-at-point.
  ;; Extra complication just to do the temporary highlighting.
  (unwind-protect
      (ffap-read-file-or-url
       (if ffap-url-regexp "Find file or URL: " "Find file: ")
       (prog1
	   (setq guess (or guess (ffap-guesser)))
	 (and guess (ffap-highlight))))
    (ffap-highlight t)))

;;;###autoload
(defun find-file-at-point (&optional filename)
  "Find FILENAME (or url), guessing default from text around point.
If `ffap-dired-wildcards' is set, wildcard patterns are passed to dired.
See also the functions ffap-file-at-point, ffap-url-at-point.
With a prefix, this command behaves *exactly* like `ffap-file-finder'.
If `ffap-require-prefix' is set, the prefix meaning is reversed.

See ftp://ftp.mathcs.emory.edu/pub/mic/emacs/ for most recent version."
  (interactive)
  (if (and (interactive-p)
	   (if ffap-require-prefix (not current-prefix-arg)
	     current-prefix-arg))
      ;; Do exactly the ffap-file-finder command, even the prompting:
      (call-interactively ffap-file-finder)
    (or filename (setq filename (ffap-prompter)))
    (cond
     ((ffap-url-p filename)
      (funcall ffap-url-fetcher filename))
     ;; This junk more properly belongs in a modified ffap-file-finder:
     ((and ffap-dired-wildcards (string-match ffap-dired-wildcards filename))
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
(fset 'ffap 'find-file-at-point)


;;; Menu support:
;;
;; Bind ffap-menu to a key if you want, since it also works in tty mode.
;; Or just use it through the ffap-at-mouse binding (next section).

(defvar ffap-menu-regexp nil
  "*If non-nil, overrides `ffap-next-regexp' during ffap-menu.
Make this more restrictive for faster menu building.
For example, try \":/\" for url (and some ftp) references.")

(defvar ffap-menu-alist nil
  "Buffer local menu of files and urls cached by ffap-menu.")
(make-variable-buffer-local 'ffap-menu-alist)

;;;###autoload
(defun ffap-menu (&optional rescan)
  "Puts up a menu of files and urls mentioned in the buffer.
Sets mark, jumps to choice, and tries to fetch it.
Menu is cached in `ffap-menu-alist', but will always be rebuilt
with the optional RESCAN argument (a prefix interactively).
Searches buffer with `ffap-menu-regexp' (see `ffap-next-regexp')."
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
Each (string . data) entry in ALIST defines a choice (data is ignored).
Once the user makes a choice, function CONT is applied to the entry.
Always returns nil."
  ;; Bug: minibuffer prompting assumes the strings are unique.
  ;; Todo: break up long menus into multiple panes (like imenu).
  (let ((choice
	 (if (and (fboundp 'x-popup-menu) ; 19 or XEmacs 19.13
		  (boundp 'last-nonmenu-event) ; not in XEmacs 19.13
		  (listp last-nonmenu-event))
	     (x-popup-menu
	      t
	      (list ""
		    (cons title
			  (mapcar
			   (function (lambda (i) (cons (car i) i)))
			   alist))))
	   ;; Automatically popup completion help, one way or another:
	   (let ((minibuffer-setup-hook 'minibuffer-completion-help)
		 (unread-command-char -1))
	     ;; BUG: this code assumes that "" is not a valid choice
	     (completing-read
	      (format "%s (default %s): " title (car (car alist)))
	      alist nil t
	      ;; Let first be default:
	      ;; (if ffap-v18 (car (car alist))
	      ;;   (cons (car (car alist)) 0))
	      ;; No, then you do not get all completions!
	      nil
	      )))))
    ;; Defaulting: convert "" to (car (car alist))
    (and (equal choice "") (setq choice (car (car alist))))
    (and (stringp choice) (setq choice (assoc choice alist)))
    (if choice (funcall cont choice) (message "No choice made!")))
  nil)					; return nothing

(defun ffap-menu-rescan nil
  (interactive)
  (let ((ffap-next-regexp (or ffap-menu-regexp ffap-next-regexp))
	(range (- (point-max) (point-min))) item)
    (setq ffap-menu-alist nil)
    (save-excursion
      (goto-char (point-min))
      (while (setq item (ffap-next-guess))
	(setq ffap-menu-alist (cons (cons item (point)) ffap-menu-alist))
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
;; I suggest a mouse binding, something like:
;; (global-set-key [S-mouse-1] 'ffap-at-mouse)

(defvar ffap-at-mouse-fallback 'ffap-menu
  "Invoked by ffap-at-mouse if no file or url found at point.
A command symbol, or nil for nothing.")
(put 'ffap-at-mouse-fallback 'risky-local-variable t)

(defun ffap-at-mouse (e)
  "Find file or URL guessed from text around mouse point.
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
;; Suggested by KPC.  Possible bindings for C-x 4 C-f, C-x 5 C-f.

(defun ffap-other-window nil
  "Like ffap, but put buffer in another window."
  (interactive)
  (switch-to-buffer-other-window
   (save-window-excursion (call-interactively 'ffap) (current-buffer))))

(defun ffap-other-frame nil
  "Like ffap, but put buffer in another frame."
  (interactive)
  (switch-to-buffer-other-frame
   (save-window-excursion (call-interactively 'ffap) (current-buffer))))


;;; ffap-bug:
(defun ffap-bug nil
  ;; Tested with Emacs 19.28 reporter.el
  "Submit a bug report for ffap."
  (interactive)
  (require 'reporter)
  (let ((reporter-prompt-for-summary-p t))
    (reporter-submit-bug-report
     "mic@mathcs.emory.edu" "ffap "
     (mapcar 'intern (all-completions "ffap-" obarray 'boundp))
     )))
(fset 'ffap-submit-bug 'ffap-bug)	; another likely name


;;; Todo, End.
;;
;; * w3 may eventually make URL's part of the filesystem!
;;   this package (prompt & completion) could become much simpler
;; * improve minibuffer-completion-help display of long completions
;; * notice "machine.dom blah blah blah path/file" (how?)
;; * check X selections (x-get-selection PRIMARY/SECONDARY LENGTH/TEXT)
;; * let "/path/file#key" jump to key (anchor or regexp) in /path/file
;; * notice node in "(dired)Virtual Dired" (how to handle space?)
;; * try find-tag on symbol if TAGS is loaded (need above)
;;
;; For information on URL/URI syntax, try:
;; <http://ds.internic.net/rfc/rfc1630.txt>
;; <http://www.w3.org/hypertext/WWW/Protocols/Overview.html>
;; <http://info.cern.ch/hypertext/WWW/Addressing/Addressing.html>

;; Local Variables?
;; foo: bar
;; End:


;;; ffap.el ends here
