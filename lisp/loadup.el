;;; loadup.el --- load up standardly loaded Lisp files for Emacs

;; Copyright (C) 1985-1986, 1992, 1994, 2001-2018 Free Software
;; Foundation, Inc.

;; Maintainer: emacs-devel@gnu.org
;; Keywords: internal
;; Package: emacs

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

;; This is loaded into a bare Emacs to make a dumpable one.

;; If you add a file to be loaded here, keep the following points in mind:

;; i) If the file is no-byte-compile, explicitly load the .el version.
;; Such files should (where possible) obey the doc-string conventions
;; expected by make-docfile.  They should also be added to the
;; uncompiled[] list in make-docfile.c.

;; ii) If the file is dumped with Emacs (on any platform), put the
;; load statement at the start of a line (leading whitespace is ok).

;; iii) If the file is _not_ dumped with Emacs, make sure the load
;; statement is _not_ at the start of a line.  See pcase for an example.

;; These rules are so that src/Makefile can construct lisp.mk automatically.
;; This ensures both that the Lisp files are compiled (if necessary)
;; before the emacs executable is dumped, and that they are passed to
;; make-docfile.  (Any that are not processed for DOC will not have
;; doc strings in the dumped Emacs.)

;;; Code:

;; This is used in xdisp.c to determine when bidi reordering is safe.
;; (It starts non-nil in temacs, but we set it non-nil here anyway, in
;; case someone loads loadup one more time.)  We reset it after
;; successfully loading charprop.el, which defines the Unicode tables
;; bidi.c needs for its job.
(setq redisplay--inhibit-bidi t)

;; Add subdirectories to the load-path for files that might get
;; autoloaded when bootstrapping.
;; This is because PATH_DUMPLOADSEARCH is just "../lisp".
(if (or (equal (member "bootstrap" command-line-args) '("bootstrap"))
	;; FIXME this is irritatingly fragile.
	(and (stringp (nth 4 command-line-args))
	     (string-match "^unidata-gen\\(\\.elc?\\)?$"
			   (nth 4 command-line-args)))
	(member (nth 7 command-line-args) '("unidata-gen-file"
					    "unidata-gen-charprop"))
	(if (fboundp 'dump-emacs)
	    (string-match "src/bootstrap-emacs" (nth 0 command-line-args))
	  t))
    (let ((dir (car load-path)))
      ;; We'll probably overflow the pure space.
      (setq purify-flag nil)
      ;; Value of max-lisp-eval-depth when compiling initially.
      ;; During bootstrapping the byte-compiler is run interpreted when
      ;; compiling itself, which uses a lot more stack than usual.
      (setq max-lisp-eval-depth 2200)
      (setq load-path (list (expand-file-name "." dir)
			    (expand-file-name "emacs-lisp" dir)
			    (expand-file-name "progmodes" dir)
			    (expand-file-name "language" dir)
			    (expand-file-name "international" dir)
			    (expand-file-name "textmodes" dir)
			    (expand-file-name "vc" dir)))))

(if (eq t purify-flag)
    ;; Hash consing saved around 11% of pure space in my tests.
    (setq purify-flag (make-hash-table :test 'equal :size 80000)))

(message "Using load-path %s" load-path)

;; This is a poor man's `last', since we haven't loaded subr.el yet.
(if (and (fboundp 'dump-emacs)
         (or (equal (member "bootstrap" command-line-args) '("bootstrap"))
             (equal (member "dump" command-line-args) '("dump"))))
    (progn
      ;; To reduce the size of dumped Emacs, we avoid making huge char-tables.
      (setq inhibit-load-charset-map t)
      ;; --eval gets handled too late.
      (defvar load--prefer-newer load-prefer-newer)
      (setq load-prefer-newer t)))

;; We don't want to have any undo records in the dumped Emacs.
(set-buffer "*scratch*")
(setq buffer-undo-list t)

(load "emacs-lisp/byte-run")
(load "emacs-lisp/backquote")
(load "subr")

;; Do it after subr, since both after-load-functions and add-hook are
;; implemented in subr.el.
(add-hook 'after-load-functions (lambda (f) (garbage-collect)))

(load "version")

(load "widget")
(load "custom")
(load "emacs-lisp/map-ynp")
(load "international/mule")
(load "international/mule-conf")
(load "env")
(load "format")
(load "bindings")
(load "window")  ; Needed here for `replace-buffer-in-windows'.
;; We are now capable of resizing the mini-windows, so give the
;; variable its advertised default value (it starts as nil, see
;; xdisp.c).
(setq resize-mini-windows 'grow-only)
(setq load-source-file-function 'load-with-code-conversion)
(load "files")

;; Load-time macro-expansion can only take effect after setting
;; load-source-file-function because of where it is called in lread.c.
(load "emacs-lisp/macroexp")
(if (byte-code-function-p (symbol-function 'macroexpand-all))
    nil
  ;; Since loaddefs is not yet loaded, macroexp's uses of pcase will simply
  ;; fail until pcase is explicitly loaded.  This also means that we have to
  ;; disable eager macro-expansion while loading pcase.
  (let ((macroexp--pending-eager-loads '(skip))) (load "emacs-lisp/pcase"))
  ;; Re-load macroexp so as to eagerly macro-expand its uses of pcase.
  (let ((max-lisp-eval-depth (* 2 max-lisp-eval-depth)))
    (load "emacs-lisp/macroexp")))

(load "cus-face")
(load "faces")  ; after here, `defface' may be used.

(load "button")

;; We don't want to store loaddefs.el in the repository because it is
;; a generated file; but it is required in order to compile the lisp files.
;; When bootstrapping, we cannot generate loaddefs.el until an
;; emacs binary has been built.  We therefore compromise and keep
;; ldefs-boot.el in the repository.  This does not need to be updated
;; as often as the real loaddefs.el would.  Bootstrap should always
;; work with ldefs-boot.el.  Therefore, whenever a new autoload cookie
;; gets added that is necessary during bootstrapping, ldefs-boot.el
;; should be updated by overwriting it with an up-to-date copy of
;; loaddefs.el that is not corrupted by local changes.
;; admin/update_autogen can be used to update ldefs-boot.el periodically.
(condition-case nil (load "loaddefs.el")
  ;; In case loaddefs hasn't been generated yet.
  (file-error (load "ldefs-boot.el")))

(let ((new (make-hash-table :test 'equal)))
  ;; Now that loaddefs has populated definition-prefixes, purify its contents.
  (maphash (lambda (k v) (puthash (purecopy k) (purecopy v) new))
           definition-prefixes)
  (setq definition-prefixes new))

(load "emacs-lisp/nadvice")
(load "emacs-lisp/cl-preloaded")
(load "minibuffer")            ;After loaddefs, for define-minor-mode.
(load "obarray")        ;abbrev.el is implemented in terms of obarrays.
(load "abbrev")         ;lisp-mode.el and simple.el use define-abbrev-table.
(load "simple")

(load "help")

(load "jka-cmpr-hook")
(load "epa-hook")
;; Any Emacs Lisp source file (*.el) loaded here after can contain
;; multilingual text.
(load "international/mule-cmds")
(load "case-table")
;; This file doesn't exist when building a development version of Emacs
;; from the repository.  It is generated just after temacs is built.
(load "international/charprop.el" t)
(if (featurep 'charprop)
    (setq redisplay--inhibit-bidi nil))
(load "international/characters")
(load "composite")

;; Load language-specific files.
(load "language/chinese")
(load "language/cyrillic")
(load "language/indian")
(load "language/sinhala")
(load "language/english")
(load "language/ethiopic")
(load "language/european")
(load "language/czech")
(load "language/slovak")
(load "language/romanian")
(load "language/greek")
(load "language/hebrew")
(load "international/cp51932")
(load "international/eucjp-ms")
(load "language/japanese")
(load "language/korean")
(load "language/lao")
(load "language/tai-viet")
(load "language/thai")
(load "language/tibetan")
(load "language/vietnamese")
(load "language/misc-lang")
(load "language/utf-8-lang")
(load "language/georgian")
(load "language/khmer")
(load "language/burmese")
(load "language/cham")

(load "indent")
(load "emacs-lisp/cl-generic")
(load "frame")
(load "startup")
(load "term/tty-colors")
(load "font-core")
;; facemenu must be loaded before font-lock, because `facemenu-keymap'
;; needs to be defined when font-lock is loaded.
(load "facemenu")
(load "emacs-lisp/syntax")
(load "font-lock")
(load "jit-lock")

(load "mouse")
(if (boundp 'x-toolkit-scroll-bars)
    (load "scroll-bar"))
(load "select")
(load "emacs-lisp/timer")
(load "isearch")
(load "rfn-eshadow")

(load "menu-bar")
(load "emacs-lisp/lisp")
(load "textmodes/page")
(load "register")
(load "textmodes/paragraphs")
(load "progmodes/prog-mode")
(load "emacs-lisp/lisp-mode")
(load "progmodes/elisp-mode")
(load "textmodes/text-mode")
(load "textmodes/fill")
(load "newcomment")

(load "replace")
(load "emacs-lisp/tabulated-list")
(load "buff-menu")

(if (fboundp 'x-create-frame)
    (progn
      (load "fringe")
      ;; Needed by `imagemagick-register-types'
      (load "emacs-lisp/regexp-opt")
      (load "image")
      (load "international/fontset")
      (load "dnd")
      (load "tool-bar")))

(if (featurep 'dynamic-setting)
    (load "dynamic-setting"))

(if (featurep 'x)
    (progn
      (load "x-dnd")
      (load "term/common-win")
      (load "term/x-win")))

(if (or (eq system-type 'windows-nt)
        (featurep 'w32))
    (progn
      (load "term/common-win")
      (load "w32-vars")
      (load "term/w32-win")
      (load "disp-table")
      (when (eq system-type 'windows-nt)
        (load "w32-fns")
        (load "ls-lisp")
        (load "dos-w32"))))
(if (eq system-type 'ms-dos)
    (progn
      (load "dos-w32")
      (load "dos-fns")
      (load "dos-vars")
      ;; Don't load term/common-win: it isn't appropriate for the `pc'
      ;; ``window system'', which generally behaves like a terminal.
      (load "term/internal")
      (load "term/pc-win")
      (load "ls-lisp")
      (load "disp-table"))) ; needed to setup ibm-pc char set, see internal.el
(if (featurep 'ns)
    (progn
      (load "term/common-win")
      ;; Don't load ucs-normalize.el unless uni-*.el files were
      ;; already produced, because it needs uni-*.el files that might
      ;; not be built early enough during bootstrap.
      (when (featurep 'charprop)
        (load "international/mule-util")
        (load "international/ucs-normalize")
        (load "term/ns-win"))))
(if (fboundp 'x-create-frame)
    ;; Do it after loading term/foo-win.el since the value of the
    ;; mouse-wheel-*-event vars depends on those files being loaded or not.
    (load "mwheel"))
;; Preload some constants and floating point functions.
(load "emacs-lisp/float-sup")

(load "vc/vc-hooks")
(load "vc/ediff-hook")
(load "uniquify")
(load "electric")
(load "emacs-lisp/eldoc")
(load "cus-start") ;Late to reduce customize-rogue (needs loaddefs.el anyway)
(if (not (eq system-type 'ms-dos))
    (load "tooltip"))

;; This file doesn't exist when building a development version of Emacs
;; from the repository.  It is generated just after temacs is built.
(load "leim/leim-list.el" t)

;; If you want additional libraries to be preloaded and their
;; doc strings kept in the DOC file rather than in core,
;; you may load them with a "site-load.el" file.
;; But you must also cause them to be scanned when the DOC file
;; is generated.
(let ((lp load-path))
  (load "site-load" t)
  ;; We reset load-path after dumping.
  ;; For a permanent change in load-path, use configure's
  ;; --enable-locallisppath option.
  ;; See https://debbugs.gnu.org/16107 for more details.
  (or (equal lp load-path)
      (message "Warning: Change in load-path due to site-load will be \
lost after dumping")))

;; Make sure default-directory is unibyte when dumping.  This is
;; because we cannot decode and encode it correctly (since the locale
;; environment is not, and should not be, set up).  default-directory
;; is used every time we call expand-file-name, which we do in every
;; file primitive.  So the only workable solution to support building
;; in non-ASCII directories is to manipulate unibyte strings in the
;; current locale's encoding.
(if (and (member (car (last command-line-args)) '("dump" "bootstrap"))
         (fboundp 'dump-emacs)
	 (multibyte-string-p default-directory))
    (error "default-directory must be unibyte when dumping Emacs!"))

;; Determine which build number to use
;; based on the executables that now exist.
(if (and (equal (last command-line-args) '("dump"))
         (fboundp 'dump-emacs)
	 (not (eq system-type 'ms-dos)))
    (let* ((base (concat "emacs-" emacs-version "."))
	   (exelen (if (eq system-type 'windows-nt) -4))
	   (files (file-name-all-completions base default-directory))
	   (versions (mapcar (function
			      (lambda (name)
				(string-to-number
				 (substring name (length base) exelen))))
			     files)))
      (setq emacs-repository-version (condition-case nil (emacs-repository-get-version)
                              (error nil)))
      ;; A constant, so we shouldn't change it with `setq'.
      (defconst emacs-build-number
	(if versions (1+ (apply 'max versions)) 1))))


(message "Finding pointers to doc strings...")
(if (and (fboundp 'dump-emacs)
         (equal (last command-line-args) '("dump")))
    (Snarf-documentation "DOC")
  (condition-case nil
      (Snarf-documentation "DOC")
    (error nil)))
(message "Finding pointers to doc strings...done")

;; Note: You can cause additional libraries to be preloaded
;; by writing a site-init.el that loads them.
;; See also "site-load" above
(let ((lp load-path))
  (load "site-init" t)
  (or (equal lp load-path)
      (message "Warning: Change in load-path due to site-init will be \
lost after dumping")))

(setq current-load-list nil)
;; Avoid storing references to build directory in the binary.
(setq custom-current-group-alist nil)

;; We keep the load-history data in PURE space.
;; Make sure that the spine of the list is not in pure space because it can
;; be destructively mutated in lread.c:build_load_history.
(setq load-history (mapcar 'purecopy load-history))

(set-buffer-modified-p nil)

(remove-hook 'after-load-functions (lambda (f) (garbage-collect)))

(if (boundp 'load--prefer-newer)
    (progn
      (setq load-prefer-newer load--prefer-newer)
      (put 'load-prefer-newer 'standard-value load--prefer-newer)
      (makunbound 'load--prefer-newer)))

(setq inhibit-load-charset-map nil)
(clear-charset-maps)
(garbage-collect)

;; At this point, we're ready to resume undo recording for scratch.
(buffer-enable-undo "*scratch*")

(when (hash-table-p purify-flag)
  (let ((strings 0)
        (vectors 0)
        (bytecodes 0)
        (conses 0)
        (others 0))
    (maphash (lambda (k v)
               (cond
                ((stringp k) (setq strings (1+ strings)))
                ((vectorp k) (setq vectors (1+ vectors)))
                ((consp k)   (setq conses  (1+ conses)))
                ((byte-code-function-p v) (setq bytecodes (1+ bytecodes)))
                (t           (setq others  (1+ others)))))
             purify-flag)
    (message "Pure-hashed: %d strings, %d vectors, %d conses, %d bytecodes, %d others"
             strings vectors conses bytecodes others)))

;; Avoid error if user loads some more libraries now and make sure the
;; hash-consing hash table is GC'd.
(setq purify-flag nil)

(if (null (garbage-collect))
    (setq pure-space-overflow t))

;; Make sure we will attempt bidi reordering henceforth.
(setq redisplay--inhibit-bidi nil)

(if (and (fboundp 'dump-emacs)
         (member (car (last command-line-args)) '("dump" "bootstrap")))
    (progn
      ;; Prevent build-time PATH getting stored in the binary.
      ;; Mainly cosmetic, but helpful for Guix.  (Bug#20330)
      ;; Do this here, rather than earlier, so that the above code
      ;; can invoke Git commands and the like.
      (setq exec-path nil)
      (message "Dumping under the name emacs")
      (condition-case ()
	  (delete-file "emacs")
	(file-error nil))
      ;; We used to dump under the name xemacs, but that occasionally
      ;; confused people installing Emacs (they'd install the file
      ;; under the name `xemacs'), and it's inconsistent with every
      ;; other GNU program's build process.
      (dump-emacs "emacs" "temacs")
      (message "%d pure bytes used" pure-bytes-used)
      ;; Recompute NAME now, so that it isn't set when we dump.
      (if (not (or (eq system-type 'ms-dos)
                   ;; Don't bother adding another name if we're just
                   ;; building bootstrap-emacs.
                   (equal (last command-line-args) '("bootstrap"))))
	  (let ((name (format "emacs-%s.%d" emacs-version emacs-build-number))
		(exe (if (eq system-type 'windows-nt) ".exe" "")))
	    (while (string-match "[^-+_.a-zA-Z0-9]+" name)
	      (setq name (concat (downcase (substring name 0 (match-beginning 0)))
				 "-"
				 (substring name (match-end 0)))))
	    (setq name (concat name exe))
            (message "Adding name %s" name)
	    ;; When this runs on Windows, invocation-directory is not
	    ;; necessarily the current directory.
	    (add-name-to-file (expand-file-name (concat "emacs" exe)
						invocation-directory)
			      (expand-file-name name invocation-directory)
			      t)))
      (kill-emacs)))

;; For machines with CANNOT_DUMP defined in config.h,
;; this file must be loaded each time Emacs is run.
;; So run the startup code now.  First, remove `-l loadup' from args.

(if (and (member (nth 1 command-line-args) '("-l" "--load"))
	 (equal (nth 2 command-line-args) "loadup"))
    (setcdr command-line-args (nthcdr 3 command-line-args)))

(eval top-level)


;; Local Variables:
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:

;;; loadup.el ends here
