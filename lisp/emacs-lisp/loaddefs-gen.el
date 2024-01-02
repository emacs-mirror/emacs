;;; loaddefs-gen.el --- generate loaddefs.el files  -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Free Software Foundation, Inc.

;; Keywords: maint
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

;; This package generates the main lisp/loaddefs.el file, as well as
;; all the other loaddefs files, like calendar/diary-loaddefs.el, etc.

;; The main entry point is `loaddefs-generate' (normally called
;; from loaddefs-generate-batch via lisp/Makefile).
;;
;; The "other" loaddefs files are specified either via a file-local
;; setting of `generated-autoload-file', or by specifying
;;
;;   ;;;###foo-autoload
;;
;; This makes the autoload go to foo-loaddefs.el in the current directory.
;; Normal ;;;###autoload specs go to the main loaddefs file.

;;; Code:

(require 'radix-tree)
(require 'lisp-mnt)
(require 'generate-lisp-file)

(defvar autoload-compute-prefixes t
  "If non-nil, autoload will add code to register the prefixes used in a file.
Standard prefixes won't be registered anyway.  I.e. if a file
\"foo.el\" defines variables or functions that use \"foo-\" as
prefix, that will not be registered.  But all other prefixes will
be included.")
(put 'autoload-compute-prefixes 'safe-local-variable #'booleanp)

(defvar no-update-autoloads nil
  "File local variable to prevent scanning this file for autoload cookies.")

(defvar autoload-ignored-definitions
  '("define-obsolete-function-alias"
    "define-obsolete-variable-alias"
    "define-category"
    "define-key" "define-key-after" "define-keymap"
    "defgroup" "defface" "defadvice"
    "def-edebug-spec"
    ;; Hmm... this is getting ugly:
    "define-widget"
    "define-erc-module"
    "define-erc-response-handler"
    "defun-rcirc-command"
    "define-short-documentation-group"
    "def-edebug-elem-spec"
    "defvar-mode-local"
    "defcustom-mode-local-semantic-dependency-system-include-path"
    "define-ibuffer-column"
    "define-ibuffer-sorter")
  "List of strings naming definitions to ignore for prefixes.
More specifically those definitions will not be considered for the
`register-definition-prefixes' call.")

(defvar generated-autoload-file nil
  "File into which to write autoload definitions.
A Lisp file can set this in its local variables section to make
its autoloads go somewhere else.

If this is a relative file name, the directory is determined as
follows:
 - If a Lisp file defined `generated-autoload-file' as a
   file-local variable, use its containing directory.
 - Otherwise use the \"lisp\" subdirectory of `source-directory'.

The autoload file is assumed to contain a trailer starting with a
FormFeed character.")
;;;###autoload
(put 'generated-autoload-file 'safe-local-variable 'stringp)

(defvar generated-autoload-load-name nil
  "Load name for `autoload' statements generated from autoload cookies.
If nil, this defaults to the file name, sans extension.
Typically, you need to set this when the directory containing the file
is not in `load-path'.
This also affects the generated cus-load.el file.")
;;;###autoload
(put 'generated-autoload-load-name 'safe-local-variable 'stringp)

(defun loaddefs-generate--file-load-name (file outfile)
  "Compute the name that will be used to load FILE.
OUTFILE should be the name of the global loaddefs.el file, which
is expected to be at the root directory of the files we are
scanning for autoloads and will be in the `load-path'."
  (let* ((name (file-relative-name file (file-name-directory outfile)))
         (names '())
         (dir (file-name-directory outfile)))
    ;; If `name' lives inside an ancestor directory of OUTFILE, only
    ;; keep the last few leading directories that are really needed.
    ;; (It will always live in an ancestor directory of OUTFILE on
    ;; Posix systems, but on DOS/Windows it could not be, if FILE and
    ;; OUTFILE are on different drives.)
    (when (not (file-name-absolute-p name))
      (while name
        (setq name (directory-file-name name))
        (push (file-name-nondirectory name) names)
        (setq name (file-name-directory name)))
      (while (not name)
        (cond
         ((null (cdr names)) (setq name (car names)))
         ((file-exists-p (expand-file-name "subdirs.el" dir))
          ;; FIXME: here we only check the existence of subdirs.el,
          ;; without checking its content.  This makes it generate
          ;; wrong load names for cases like lisp/term which is not
          ;; added to load-path.
          (setq dir (expand-file-name (pop names) dir)))
         (t (setq name (mapconcat #'identity names "/"))))))
    (if (string-match "\\.elc?\\(\\.\\|\\'\\)" name)
        (substring name 0 (match-beginning 0))
      name)))

(defun loaddefs-generate--shorten-autoload (form)
  "Remove optional nil elements from an `autoload' form."
  (take (max (- (length form)
                (seq-position (reverse form) nil
                              (lambda (e1 e2)
                                (not (eq e1 e2)))))
             3)
        form))

(defun loaddefs-generate--make-autoload (form file &optional expansion)
  "Turn FORM into an autoload or defvar for source file FILE.
Returns nil if FORM is not a special autoload form (i.e. a function definition
or macro definition or a defcustom).
If EXPANSION is non-nil, we're processing the macro expansion of an
expression, in which case we want to handle forms differently."
  (let ((car (car-safe form)) expand)
    (cond
     ((and expansion (eq car 'defalias))
      (pcase-let*
          ((`(,_ ,_ ,arg . ,rest) form)
           ;; `type' is non-nil if it defines a macro.
           ;; `fun' is the function part of `arg' (defaults to `arg').
           ((or (and (or `(cons 'macro ,fun) `'(macro . ,fun)) (let type t))
                (and (let fun arg) (let type nil)))
            arg)
           ;; `lam' is the lambda expression in `fun' (or nil if not
           ;; recognized).
           (lam (if (memq (car-safe fun) '(quote function)) (cadr fun)))
           ;; `args' is the list of arguments (or t if not recognized).
           ;; `body' is the body of `lam' (or t if not recognized).
           ((or `(lambda ,args . ,body)
                (and (let args t) (let body t)))
            lam)
           ;; Get the `doc' from `body' or `rest'.
           (doc (cond ((stringp (car-safe body)) (car body))
                      ((stringp (car-safe rest)) (car rest))))
           ;; Look for an interactive spec.
           (interactive (pcase body
                          ((or `((interactive . ,iargs) . ,_)
                               `(,_ (interactive . ,iargs) . ,_))
                           ;; List of modes or just t.
                           (if (nthcdr 1 iargs)
                               (list 'quote (nthcdr 1 iargs))
                             t)))))
        ;; Add the usage form at the end where describe-function-1
        ;; can recover it.
        (when (consp args) (setq doc (help-add-fundoc-usage doc args)))
        (loaddefs-generate--shorten-autoload
         `(autoload ,(nth 1 form) ,file ,doc ,interactive ,type))))

     ((and expansion (memq car '(progn prog1)))
      (let ((end (memq :autoload-end form)))
	(when end             ;Cut-off anything after the :autoload-end marker.
          (setq form (copy-sequence form))
          (setcdr (memq :autoload-end form) nil))
        (let ((exps (delq nil (mapcar (lambda (form)
                                        (loaddefs-generate--make-autoload
                                         form file expansion))
                                      (cdr form)))))
          (when exps (cons 'progn exps)))))

     ;; For complex cases, try again on the macro-expansion.
     ((and (memq car '(easy-mmode-define-global-mode define-global-minor-mode
                       define-globalized-minor-mode defun defmacro
		       easy-mmode-define-minor-mode define-minor-mode
                       define-inline cl-defun cl-defmacro cl-defgeneric
                       cl-defstruct pcase-defmacro iter-defun cl-iter-defun
                       transient-define-prefix))
           (macrop car)
	   (setq expand (let ((load-true-file-name file)
                              (load-file-name file))
                          (macroexpand form)))
	   (memq (car expand) '(progn prog1 defalias)))
      ;; Recurse on the expansion.
      (loaddefs-generate--make-autoload expand file 'expansion))

     ;; For special function-like operators, use the `autoload' function.
     ((memq car '(define-skeleton define-derived-mode
                   define-compilation-mode define-generic-mode
		   easy-mmode-define-global-mode define-global-minor-mode
		   define-globalized-minor-mode
		   easy-mmode-define-minor-mode define-minor-mode
		   cl-defun defun* cl-defmacro defmacro*
                   define-overloadable-function))
      (let* ((macrop (memq car '(defmacro cl-defmacro defmacro*)))
	     (name (nth 1 form))
	     (args (pcase car
                     ((or 'defun 'defmacro
                          'defun* 'defmacro* 'cl-defun 'cl-defmacro
                          'define-overloadable-function)
                      (nth 2 form))
                     ('define-skeleton '(&optional str arg))
                     ((or 'define-generic-mode 'define-derived-mode
                          'define-compilation-mode)
                      nil)
                     (_ t)))
	     (body (nthcdr (or (function-get car 'doc-string-elt) 3) form))
	     (doc (if (stringp (car body)) (pop body))))
        ;; Add the usage form at the end where describe-function-1
        ;; can recover it.
	(when (listp args) (setq doc (help-add-fundoc-usage doc args)))
        ;; `define-generic-mode' quotes the name, so take care of that
        (loaddefs-generate--shorten-autoload
         `(autoload ,(if (listp name) name (list 'quote name))
            ,file ,doc
            ,(or (and (memq car '(define-skeleton define-derived-mode
                                   define-generic-mode
                                   easy-mmode-define-global-mode
                                   define-global-minor-mode
                                   define-globalized-minor-mode
                                   easy-mmode-define-minor-mode
                                   define-minor-mode))
                      t)
                 (and (eq (car-safe (car body)) 'interactive)
                      ;; List of modes or just t.
                      (or (if (nthcdr 1 (car body))
                              (list 'quote (nthcdr 1 (car body)))
                            t))))
            ,(if macrop ''macro nil)))))

     ;; For defclass forms, use `eieio-defclass-autoload'.
     ((eq car 'defclass)
      (let ((name (nth 1 form))
	    (superclasses (nth 2 form))
	    (doc (nth 4 form)))
	(list 'eieio-defclass-autoload (list 'quote name)
	      (list 'quote superclasses) file doc)))

     ;; Convert defcustom to less space-consuming data.
     ((eq car 'defcustom)
      (let* ((varname (car-safe (cdr-safe form)))
	     (props (nthcdr 4 form))
	     (initializer (plist-get props :initialize))
	     (init (car-safe (cdr-safe (cdr-safe form))))
	     (doc (car-safe (cdr-safe (cdr-safe (cdr-safe form)))))
	     ;; (rest (cdr-safe (cdr-safe (cdr-safe (cdr-safe form)))))
	     )
	`(progn
	   ,(if (not (member initializer '(nil 'custom-initialize-default
	                                   #'custom-initialize-default
	                                   'custom-initialize-reset
	                                   #'custom-initialize-reset)))
	        form
	      `(defvar ,varname ,init ,doc))
	   ;; When we include the complete `form', this `custom-autoload'
           ;; is not indispensable, but it still helps in case the `defcustom'
           ;; doesn't specify its group explicitly, and probably in a few other
           ;; corner cases.
	   (custom-autoload ',varname ,file
                            ,(condition-case nil
                                 (null (plist-get props :set))
                               (error nil)))
           ;; Propagate the :safe property to the loaddefs file.
           ,@(when-let ((safe (plist-get props :safe)))
               `((put ',varname 'safe-local-variable ,safe))))))

     ;; Extract theme properties.
     ((eq car 'deftheme)
      (let* ((name (car-safe (cdr-safe form)))
	     (props (nthcdr 3 form)))
	`(put ',name 'theme-properties (list ,@props))))

     ((eq car 'defgroup)
      ;; In Emacs this is normally handled separately by cus-dep.el, but for
      ;; third party packages, it can be convenient to explicitly autoload
      ;; a group.
      (let ((groupname (nth 1 form))
            (parent (eval (plist-get form :group) t)))
        `(let ((loads (get ',groupname 'custom-loads)))
           (if (member ',file loads) nil
             (put ',groupname 'custom-loads (cons ',file loads))
             ,@(when parent
               `((put ',parent 'custom-loads
                      (cons ',groupname (get ',parent 'custom-loads)))))))))

     ;; When processing a macro expansion, any expression
     ;; before a :autoload-end should be included.  These are typically (put
     ;; 'fun 'prop val) and things like that.
     ((and expansion (consp form)) form)

     ;; nil here indicates that this is not a special autoload form.
     (t nil))))

(defun loaddefs-generate--make-prefixes (defs file)
  ;; Remove the defs that obey the rule that file foo.el (or
  ;; foo-mode.el) uses "foo-" as prefix.  Then compute a small set of
  ;; prefixes that cover all the remaining definitions.
  (let* ((tree (let ((tree radix-tree-empty))
                 (dolist (def defs)
                   (setq tree (radix-tree-insert tree def t)))
                 tree))
         (prefixes nil))
    ;; Get the root prefixes, that we should include in any case.
    (radix-tree-iter-subtrees
     tree (lambda (prefix subtree)
            (push (cons prefix subtree) prefixes)))
    ;; In some cases, the root prefixes are too short, e.g. if you define
    ;; "cc-helper" and "c-mode", you'll get "c" in the root prefixes.
    (dolist (pair (prog1 prefixes (setq prefixes nil)))
      (let ((s (car pair)))
        (if (or (and (> (length s) 2)   ; Long enough!
                     ;; But don't use "def" from deffoo-pkg-thing.
                     (not (string= "def" s)))
                (string-match ".[[:punct:]]\\'" s) ;A real (tho short) prefix?
                (radix-tree-lookup (cdr pair) "")) ;Nothing to expand!
            (push pair prefixes)                   ;Keep it as is.
          (radix-tree-iter-subtrees
           (cdr pair) (lambda (prefix subtree)
                        (push (cons (concat s prefix) subtree) prefixes))))))
    (when prefixes
      (let ((strings
             (mapcar
              (lambda (x)
                (let ((prefix (car x)))
                  (if (or (> (length prefix) 2) ;Long enough!
                          (and (eq (length prefix) 2)
                               (string-match "[[:punct:]]" prefix)))
                      prefix
                    ;; Some packages really don't follow the rules.
                    ;; Drop the most egregious cases such as the
                    ;; one-letter prefixes.
                    (let ((dropped ()))
                      (radix-tree-iter-mappings
                       (cdr x) (lambda (s _)
                                 (push (concat prefix s) dropped)))
                      (message "%s:0: Warning: Not registering prefix \"%s\".  Affects: %S"
                               file prefix dropped)
                      nil))))
              prefixes)))
        `(register-definition-prefixes ,file ',(sort (delq nil strings)
						     'string<))))))

(defun loaddefs-generate--parse-file (file main-outfile &optional package-data)
  "Examining FILE for ;;;###autoload statements.
MAIN-OUTFILE is the main loaddefs file these statements are
destined for, but this can be overridden by the buffer-local
setting of `generated-autoload-file' in FILE, and
by ;;;###foo-autoload statements.

If PACKAGE-DATA is `only', return only the package data.  If t,
include the package data with the rest of the data.  Otherwise,
don't include."
  (let ((defs nil)
        (load-name (loaddefs-generate--file-load-name file main-outfile))
        (compute-prefixes t)
        local-outfile inhibit-autoloads)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-max))
      ;; We "open-code" this version of `hack-local-variables',
      ;; because it's really slow in bootstrap-emacs.
      (when (search-backward ";; Local Variables:" (- (point-max) 1000) t)
        (save-excursion
          (when (re-search-forward "generated-autoload-file: *" nil t)
            ;; Buffer-local file that should be interpreted relative to
            ;; the .el file.
            (setq local-outfile (expand-file-name (read (current-buffer))
                                                  (file-name-directory file)))))
        (save-excursion
          (when (re-search-forward "generated-autoload-load-name: *" nil t)
            (setq load-name (read (current-buffer)))))
        (save-excursion
          (when (re-search-forward "no-update-autoloads: *" nil t)
            (setq inhibit-autoloads (read (current-buffer)))))
        (save-excursion
          (when (re-search-forward "autoload-compute-prefixes: *" nil t)
            (setq compute-prefixes (read (current-buffer))))))

      ;; We always return the package version (even for pre-dumped
      ;; files).
      (if (not package-data)
          ;; We have to switch `emacs-lisp-mode' when scanning
          ;; loaddefs for packages so that `syntax-ppss' later gives
          ;; correct results.
          (emacs-lisp-mode)
        (let ((version (lm-header "version"))
              package)
          (when (and version
                     (setq version (ignore-errors (version-to-list version)))
                     (setq package (or (lm-header "package")
                                       (file-name-sans-extension
                                        (file-name-nondirectory file)))))
            (push (list (or local-outfile main-outfile) file
                        `(push (purecopy ',(cons (intern package) version))
                               package--builtin-versions))
                  defs))))

      ;; Obey the `no-update-autoloads' file local variable.
      (when (and (not inhibit-autoloads)
                 (not (eq package-data 'only)))
        (goto-char (point-min))
        ;; The cookie might be like ;;;###tramp-autoload...
        (while (re-search-forward lisp-mode-autoload-regexp nil t)
          (when (or package-data
                    ;; Outside of the main Emacs build (`package-data'
                    ;; is set in the Emacs build), check that we don't
                    ;; have an autoload cookie on the first column of a
                    ;; doc string or the like.  (The Emacs tree
                    ;; shouldn't contain any such instances.)
                    (not (ppss-string-terminator
                          (save-match-data (syntax-ppss)))))
            ;; ... and if we have one of these names, then alter outfile.
            (let* ((aname (match-string 2))
                   (to-file (if aname
                                (expand-file-name
                                 (concat aname "-loaddefs.el")
                                 (file-name-directory file))
                              (or local-outfile main-outfile))))
              (if (eolp)
                  ;; We have a form following.
                  (let* ((form (prog1
                                   (read (current-buffer))
                                 (unless (bolp)
                                   (forward-line 1))))
                         (autoload (or (loaddefs-generate--make-autoload
                                        form load-name)
                                       form)))
                    ;; We get back either an autoload form, or a tree
                    ;; structure of `(progn ...)' things, so unravel that.
                    (let ((forms (if (eq (car autoload) 'progn)
                                     (cdr autoload)
                                   (list autoload))))
                      (while forms
                        (let ((elem (pop forms)))
                          (if (eq (car elem) 'progn)
                              ;; More recursion; add it to the start.
                              (setq forms (nconc (cdr elem) forms))
                            ;; We have something to add to the defs; do it.
                            (push (list to-file file elem) defs))))))
                ;; Just put the rest of the line into the loaddefs.
                ;; FIXME: We skip the first space if there's more
                ;; whitespace after.
                (when (looking-at-p " [\t ]")
                  (forward-char 1))
                (push (list to-file file
                            (buffer-substring (point) (line-end-position)))
                      defs)))))

        (when (and autoload-compute-prefixes
                   compute-prefixes)
          (when-let ((form (loaddefs-generate--compute-prefixes load-name)))
            ;; This output needs to always go in the main loaddefs.el,
            ;; regardless of `generated-autoload-file'.
            (push (list main-outfile file form) defs)))))
    defs))

(defun loaddefs-generate--compute-prefixes (load-name)
  (goto-char (point-min))
  (let ((prefs nil))
    ;; Avoid (defvar <foo>) by requiring a trailing space.
    (while (re-search-forward
            "^(\\(def[^ \t\n]+\\)[ \t\n]+['(]*\\([^' ()\"\n]+\\)[\n \t]" nil t)
      (unless (member (match-string 1) autoload-ignored-definitions)
        (let ((name (match-string-no-properties 2)))
          (when (save-excursion
                  (goto-char (match-beginning 0))
                  (or (bobp)
                      (progn
                        (forward-line -1)
                        (not (looking-at ";;;###autoload")))))
            (push name prefs)))))
    (loaddefs-generate--make-prefixes prefs load-name)))

(defun loaddefs-generate--rubric (file &optional type feature compile)
  "Return a string giving the appropriate autoload rubric for FILE.
TYPE (default \"autoloads\") is a string stating the type of
information contained in FILE.  TYPE \"package\" acts like the default,
but adds an extra line to the output to modify `load-path'.

If FEATURE is non-nil, FILE will provide a feature.  FEATURE may
be a string naming the feature, otherwise it will be based on
FILE's name.

If COMPILE, don't include a \"don't compile\" cookie."
  (let ((lp (and (equal type "package") (setq type "autoloads"))))
    (with-temp-buffer
      (generate-lisp-file-heading
       file 'loaddefs-generate
       :title (concat "automatically extracted " (or type "autoloads"))
       :commentary (and (string-match "/lisp/loaddefs\\.el\\'" file)
                        "This file will be copied to ldefs-boot.el and checked in periodically."))
      (when lp
        (insert "(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))\n\n"))
      (insert "\n;;; End of scraped data\n\n")
      (generate-lisp-file-trailer
       file :provide (and (stringp feature) feature)
       :compile compile
       :inhibit-native-compile t
       :inhibit-provide (not feature))
      (buffer-string))))

;;;###autoload
(defun loaddefs-generate (dir output-file &optional excluded-files
                              extra-data include-package-version
                              generate-full)
  "Generate loaddefs files for Lisp files in one or more directories given by DIR.
DIR can be either a single directory or a list of directories.

The autoloads will be written to OUTPUT-FILE.  If any Lisp file
binds `generated-autoload-file' as a file-local variable, write
its autoloads into the specified file instead.

The function does NOT recursively descend into subdirectories of the
directories specified by DIR.

Optional argument EXCLUDED-FILES, if non-nil, should be a list of
files, such as preloaded files, whose autoloads should not be written
to OUTPUT-FILE.

If EXTRA-DATA is non-nil, it should be a string; include that string
at the beginning of the generated file.  This will also force the
generation of OUTPUT-FILE even if there are no autoloads to put into
that file.

If INCLUDE-PACKAGE-VERSION is non-nil, include package version data.

If GENERATE-FULL is non-nil, regenerate all the loaddefs files anew,
instead of just updating them with the new/changed autoloads."
  (let* ((files-re (let ((tmp nil))
		     (dolist (suf (get-load-suffixes))
                       ;; We don't use module-file-suffix below because
                       ;; we don't want to depend on whether Emacs was
                       ;; built with or without modules support, nor
                       ;; what is the suffix for the underlying OS.
		       (unless (string-match "\\.\\(elc\\|so\\|dll\\)" suf)
                         (push suf tmp)))
                     (concat "\\`[^=.].*" (regexp-opt tmp t) "\\'")))
	 (files (apply #'nconc
		       (mapcar (lambda (d)
				 (directory-files (expand-file-name d)
                                                  t files-re))
			       (if (consp dir) dir (list dir)))))
         (updating (and (file-exists-p output-file) (not generate-full)))
         (defs nil))

    ;; Allow the excluded files to be relative.
    (setq excluded-files
          (mapcar (lambda (file) (expand-file-name file dir))
                  excluded-files))

    ;; Collect all the autoload data.
    (let ((progress (make-progress-reporter
                     (byte-compile-info
                      (concat "Scraping files for loaddefs"))
                     0 (length files) nil 10))
          (output-time
           (file-attribute-modification-time (file-attributes output-file)))
          (file-count 0))
      (dolist (file files)
        (progress-reporter-update progress (setq file-count (1+ file-count)))
        (when (or (not updating)
                  (time-less-p output-time
                               (file-attribute-modification-time
                                (file-attributes file))))
          ;; If we're scanning for package versions, we want to look
          ;; at the file even if it's excluded.
          (let* ((excluded (member (expand-file-name file dir) excluded-files))
                 (package-data
                  (and include-package-version (if excluded 'only t))))
            (when (or package-data (not excluded))
              (setq defs (nconc (loaddefs-generate--parse-file
                                 file output-file package-data)
                                defs))))))
      (progress-reporter-done progress))

    ;; First group per output file.
    (dolist (fdefs (seq-group-by (lambda (x) (expand-file-name (car x)))
                                 defs))
      (let ((loaddefs-file (car fdefs))
            hash)
        (with-temp-buffer
          (if (and updating (file-exists-p loaddefs-file))
              (insert-file-contents loaddefs-file)
            (insert (loaddefs-generate--rubric
                     loaddefs-file nil t include-package-version))
            (search-backward "\f")
            (when extra-data
              (insert extra-data)
              (ensure-empty-lines 1)))
          (setq hash (buffer-hash))
          ;; Then group by source file (and sort alphabetically).
          (dolist (section (sort (seq-group-by #'cadr (cdr fdefs))
                                 (lambda (e1 e2)
                                   (string<
                                    (file-name-sans-extension
                                     (file-name-nondirectory (car e1)))
                                    (file-name-sans-extension
                                     (file-name-nondirectory (car e2)))))))
            (pop section)
            (let* ((relfile (file-relative-name
                             (cadar section)
                             (file-name-directory loaddefs-file)))
                   (head (concat "\n\f\n;;; Generated autoloads from "
                                 relfile "\n\n")))
              (when (file-exists-p loaddefs-file)
                ;; If we're updating an old loaddefs file, then see if
                ;; there's a section here for this file already.
                (goto-char (point-min))
                (if (not (search-forward head nil t))
                    ;; It's a new file; put the data at the end.
                    (progn
                      (goto-char (point-max))
                      (search-backward "\f\n" nil t))
                  ;; Delete the old version of the section.  Strictly
                  ;; speaking this should search for "\n\f\n;;;", but
                  ;; there are loaddefs files in the wild that only
                  ;; have two ';;'.  (Bug#63236)
                  (delete-region (match-beginning 0)
                                 (and (search-forward "\n\f\n;;")
                                      (match-beginning 0)))
                  (forward-line -2)))
              (insert head)
              (dolist (def (reverse section))
                (setq def (caddr def))
                (if (stringp def)
                    (princ def (current-buffer))
                  (loaddefs-generate--print-form def))
                (unless (bolp)
                  (insert "\n")))))
          ;; Only write the file if we actually made a change.
          (unless (equal (buffer-hash) hash)
            (write-region (point-min) (point-max) loaddefs-file nil 'silent)
            (byte-compile-info
             (file-relative-name loaddefs-file (car (ensure-list dir)))
             t "GEN")))))

    ;; If processing files without any autoloads, the above loop will
    ;; not generate any files.  If the function was invoked with
    ;; EXTRA-DATA, we want to ensure that even if no autoloads were
    ;; found, that at least a file will have been generated containing
    ;; the contents of EXTRA-DATA:
    (when (and extra-data (not (file-exists-p output-file)))
      (with-temp-buffer
        (insert (loaddefs-generate--rubric output-file nil t))
        (search-backward "\f")
        (insert extra-data)
        (ensure-empty-lines 1)
        (write-region (point-min) (point-max) output-file nil 'silent)))))

(defun loaddefs-generate--print-form (def)
  "Print DEF in a format that makes sense for version control."
  (if (or (not (consp def))
          (not (symbolp (car def)))
          (memq (car def) '( make-obsolete
                             define-obsolete-function-alias))
          (not (stringp (nth 3 def))))
      (prin1 def (current-buffer) t)
    ;; We want to print, for instance, `defvar' values while escaping
    ;; control characters (so that we don't end up with lines with
    ;; trailing tab characters and the like), but we don't want to do
    ;; this for doc strings, because then the doc strings would be on
    ;; one single line, which would lead to more VC churn.  So --
    ;; typically (defvar foo 'value "\ Doc string" ...).
    (insert "(")
    (dotimes (_ 3)
      (prin1 (pop def) (current-buffer)
             '(t (escape-newlines . t)
                 (escape-control-characters . t)))
      (insert " "))
    (let ((start (point)))
      (prin1 (pop def) (current-buffer) t)
      (save-excursion
        (goto-char (1+ start))
        (insert "\\\n")))
    (while def
      (insert " ")
      (prin1 (pop def) (current-buffer)
             '(t (escape-newlines . t)
                 (escape-control-characters . t))))
    (insert ")")))

(defun loaddefs-generate--excluded-files ()
  ;; Exclude those files that are preloaded on ALL platforms.
  ;; These are the ones in loadup.el where "(load" is at the start
  ;; of the line (crude, but it works).
  (let ((default-directory (file-name-directory lisp-directory))
        (excludes nil)
	file)
    (with-temp-buffer
      (insert-file-contents "loadup.el")
      (while (re-search-forward "^(load \"\\([^\"]+\\)\"" nil t)
	(setq file (match-string 1))
	(or (string-match "\\.el\\'" file)
	    (setq file (format "%s.el" file)))
	(or (string-match "\\`site-" file)
	    (push (expand-file-name file) excludes))))
    ;; Don't scan ldefs-boot.el, either.
    (cons (expand-file-name "ldefs-boot.el") excludes)))

;;;###autoload
(defun loaddefs-generate-batch ()
  "Generate loaddefs.el files in batch mode.
This scans for ;;;###autoload forms and related things.

The first element on the command line should be the (main)
loaddefs.el output file, and the rest are the directories to
use."
  (let ((args command-line-args-left))
    (setq command-line-args-left nil)
    (loaddefs-generate (cdr args) (expand-file-name (car args)))))

(defun loaddefs-generate--emacs-batch ()
  "Generate the loaddefs for the Emacs build.
This is like `loaddefs-generate-batch', but has some specific
rules for built-in packages and excluded files."
  (let ((args command-line-args-left)
        (output-file (expand-file-name "loaddefs.el" lisp-directory)))
    (setq command-line-args-left nil)
    (loaddefs-generate
     args output-file
     (loaddefs-generate--excluded-files)
     nil t
     ;; Always do a complete update if loaddefs-gen.el has been
     ;; updated.
     (file-newer-than-file-p
      (expand-file-name "emacs-lisp/loaddefs-gen.el" lisp-directory)
      output-file)))
  (let ((lisp-mode-autoload-regexp
         "^;;;###\\(\\(noexist\\)-\\)?\\(theme-autoload\\)"))
      (loaddefs-generate
       (expand-file-name "../etc/themes/" lisp-directory)
       (expand-file-name "theme-loaddefs.el" lisp-directory))))

;;;###autoload (load "theme-loaddefs.el" t)

(provide 'loaddefs-gen)

;;; loaddefs-gen.el ends here
