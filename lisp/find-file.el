;;; find-file.el --- find a file corresponding to this one given a pattern

;; Author:         Henry Guillaume <henry@qbd.com.au>
;; Keywords: c, matching, tools

;; Copyright (C) 1994 Free Software Foundation, Inc.

;;; This file is part of GNU Emacs.

;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.

;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; PURPOSE:
;; This package features a function called ff-find-other-file, which performs 
;; the following function: 
;;
;;     When in a .c file, find the first corresponding .h file in a set
;;     of directories and display it, and vice-versa from the .h file.
;;
;; Many people maintain their include file in a directory separate to their
;; src directory, and very often you may be editing a file and have a need to
;; visit the "other file". This package searches through a set of directories
;; to find that file.
;;
;; THE "OTHER FILE", or "corresponding file", generally has the same basename,
;; and just has a different extension as described by the ff-other-file-alist 
;; variable:
;;
;;   '(("\\.cc$"  (".hh" ".h"))
;;     ("\\.hh$"  (".cc" ".C" ".CC" ".cxx" ".cpp")))
;;
;; If the current file has a .cc extension, ff-find-other-file will attempt
;; to look for a .hh file, and then a .h file in some directory as described
;; below. The mechanism here is to replace the matched part of the original
;; filename with each of the corresponding extensions in turn.
;;
;; Alternatively, there are situations where the filename of the other file
;; cannot be determined easily with regexps. For example, a .c file may
;; have two corresponding .h files, for its public and private parts, or
;; the filename for the .c file contains part of the pathname of the .h
;; file, as between src/fooZap.cc and include/FOO/zap.hh. In that case, the
;; format above can be changed to include a function to be called when the
;; current file matches the regexp:
;;
;;   '(("\\.cc$"  cc-function)
;;     ("\\.hh$"  hh-function))
;;
;; These functions must return a list consisting of the possible names of the 
;; corresponding file, with or without path. There is no real need for more 
;; than one function, and one could imagine the following value for cc-other-
;; file-alist:
;;
;;    (setq cc-other-file-alist
;;        '(("\\.cc$"  ff-cc-hh-converter)
;;          ("\\.hh$"  ff-cc-hh-converter)
;;          ("\\.c$"   (".h"))
;;          ("\\.h$"   (".c" ".cc" ".C" ".CC" ".cxx" ".cpp"))))
;; 
;; ff-cc-hh-converter is included at the end of this file as a reference.
;; 
;; SEARCHING is carried out in a set of directories specified by the
;; ff-search-directories variable:
;;
;;     ("." "../../src" "../include/*" "/usr/local/*/src/*" "$PROJECT/src")
;;
;; This means that the corresponding file will be searched for first in
;; the current directory, then in ../../src, then in one of the directories
;; under ../include, and so on. The star is _not_ a general wildcard
;; character: it just indicates that the subdirectories of this directory
;; must each be searched in turn. Environment variables will be expanded in
;; the ff-search-directories variable.
;;
;; If the point is on a #include line, the file to be #included is searched
;; for in the same manner. This can be disabled with the ff-ignore-include
;; variable, or by calling ff-get-other-file instead of ff-find-other-file.
;;
;; If the file was not found, ff-find-other-file will prompt you for where
;; to create the new "corresponding file" (defaults to the current directory),
;; unless the variable ff-always-try-to-create is set to nil. 
;;
;; GIVEN AN ARGUMENT (with the ^U prefix), ff-find-other-file will get the 
;; other file in another (the other?) window (see find-file-other-window and 
;; switch-to-buffer-other-window). This can be set on a more permanent basis 
;; by setting ff-always-in-other-window to t in which case the ^U prefix will 
;; do the opposite of what was described above.
;;
;; THERE ARE FIVE AVAILABLE HOOKS, called in this order if non-nil:
;;
;; - ff-pre-find-hooks     - called before the search for the other file starts
;; - ff-not-found-hooks    - called when the other file could not be found
;; - ff-pre-load-hooks     - called just before the other file is 'loaded'
;; - ff-file-created-hooks - called when the other file is created
;; - ff-post-load-hooks    - called just after the other file is 'loaded'
;;
;; The *load-hooks allow you to place point where you want it in the other
;; file. 

;; LCD Archive Entry:
;; find-file|Henry Guillaume|henry@qbd.com.au|
;; Find a file associated with this buffer.|
;; 21-Dec-1994|4.0|~/misc/find-file.el.Z|

;; FEEDBACK:
;; Please send me bug reports, bug fixes, and extensions, so that I can
;; merge them into the master source.

;; CREDITS:
;; Many thanks go to TUSC Computer Systems Pty Ltd for providing an environ-
;; ment that made the development of this package possible.
;;
;; Many thanks also go to all those who provided valuable feedback throughout
;; the development of this package:
;;     Rolf Ebert in particular, Fritz Knabe, Heddy Boubaker, Sebastian Kremer,
;;     Vasco Lopes Paulo, Mark A. Plaksin, Robert Lang, Trevor West, Kevin 
;;     Pereira & Benedict Lofstedt.

;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User definable variables:

(defvar ff-pre-find-hooks nil
  "*List of functions to be called before the search for the file starts.")

(defvar ff-pre-load-hooks nil
  "*List of functions to be called before the other file is loaded.")

(defvar ff-post-load-hooks nil
  "*List of functions to be called after the other file is loaded.")

(defvar ff-not-found-hooks nil
  "*List of functions to be called if the other file could not be found.")

(defvar ff-file-created-hooks nil
  "*List of functions to be called if the other file needs to be created.")

(defvar ff-case-fold-search nil
  "*Non-nil means ignore cases in matches (see case-fold-search).
If you have extensions in different cases, you will want this to be nil.")

(defvar ff-always-in-other-window nil
  "*If non-nil, always open the other file in another window, unless an
argument is given to ff-find-other-file.")

(defvar ff-ignore-include nil
  "*If non-nil, ignores include lines.")

(defvar ff-always-try-to-create t
  "*If non-nil, always attempt to create the other file if it was not found.")

(defvar ff-quiet-mode nil
  "*If non-nil, traces which directories are being searched.")

(defvar ff-special-constructs 
  '(
    ;; C/C++ include, for NeXTSTEP too
    ("^\#\\s *\\(include\\|import\\)\\s +[<\"]\\(.*\\)[>\"]" .
     (lambda ()
       (setq fname (buffer-substring (match-beginning 2) (match-end 2)))))

    ;; Ada import
    ("^with[ \t]+\\([a-zA-Z0-9_\\.]+\\)" .
     (lambda ()
       (setq fname (buffer-substring (match-beginning 1) (match-end 1)))
       (setq fname (concat (ada-make-filename-from-adaname fname)
                           ada-spec-suffix))))
    )
  "*A list of regular expressions specifying how to recognise special 
constructs such as include files etc, and an associated method for 
extracting the filename from that construct.")

(defvar ff-other-file-alist 'cc-other-file-alist
  "*Alist of extensions to find given the current file's extension.

This list should contain the most used extensions before the others,
since the search algorithm searches sequentially through each
directory specified in ff-search-directories. If a file is not found,
a new one is created with the first matching extension (.cc yields .hh).
This alist should be set by the major-mode.")

(defvar ff-search-directories 'cc-search-directories
  "*List of directories to search for a specific file.

Set by default to 'cc-search-directories, expanded at run-time.

This list is searched through with each extension specified in
ff-other-file-alist that matches this file's extension. So the
longer the list, the longer it'll take to realise that a file
may not exist.

A typical format is 

    '(\".\" \"/usr/include/*\" \"$PROJECT/*/include\")

Environment variables can be inserted between slashes ('/').
They will be replaced by their definition. If a variable does
not exist, it will (silently) be replaced with an empty string.

The stars are _not_ wildcards: they are searched for together with
the preceding slash. The star represents all the subdirectories except
'..', and each of these subdirectories will be searched in turn.")

(defvar cc-search-directories
  '("." "/usr/include/*" "/usr/local/include/*")
  "*See the description of the ff-search-directories variable.")

(defvar cc-other-file-alist
  '(
    ("\\.cc$"  (".hh" ".h"))
    ("\\.hh$"  (".cc" ".C"))

    ("\\.c$"   (".h"))
    ("\\.h$"   (".c" ".cc" ".C" ".CC" ".cxx" ".cpp"))

    ("\\.C$"   (".H"  ".hh" ".h"))
    ("\\.H$"   (".C"  ".CC"))

    ("\\.CC$"  (".HH" ".H"  ".hh" ".h"))
    ("\\.HH$"  (".CC"))

    ("\\.cxx$" (".hh" ".h"))
    ("\\.cpp$" (".hh" ".h"))
    )
  "*Alist of extensions to find given the current file's extension.

This list should contain the most used extensions before the others,
since the search algorithm searches sequentially through each directory
specified in ff-search-directories. If a file is not found, a new one
is created with the first matching extension (.cc yields .hh).")

(defvar ada-search-directories
  '("." "/usr/adainclude" "/usr/local/adainclude")
  "*See the description for the ff-search-directories variable.")

(defvar ada-other-file-alist
  '(
    ("\\.ads$" (".adb")) ;; Ada specs and bodies
    ("\\.adb$" (".ads")) ;; GNAT filename conventions
    )
  "*Alist of extensions to find given the current file's extension.

This list should contain the most used extensions before the others,
since the search algorithm searches sequentially through each directory
specified in ada-search-directories. If a file is not found, a new one
is created with the first matching extension (.adb yields .ads).
")

;;;### autoload
(autoload 'ada-make-filename-from-adaname "ada-mode"
  "Determine the filename of a package/procedure from its own Ada name.")
(defvar ada-spec-suffix ".ads"  
  "*Suffix of Ada specification files.")
(make-variable-buffer-local 'ada-spec-suffix)

(defvar modula2-other-file-alist
  '(
    ("\\.mi$" (".md")) ;; Modula-2 module definition
    ("\\.md$" (".mi")) ;; and implementation.
    )
  "*See the description for the ff-search-directories variable.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; No user definable variables beyond this point!
;; ==============================================

(make-variable-buffer-local 'ff-pre-find-hooks)
(make-variable-buffer-local 'ff-pre-load-hooks)
(make-variable-buffer-local 'ff-post-load-hooks)
(make-variable-buffer-local 'ff-not-found-hooks)
(make-variable-buffer-local 'ff-file-created-hooks)
(make-variable-buffer-local 'ff-case-fold-search)
(make-variable-buffer-local 'ff-always-in-other-window)
(make-variable-buffer-local 'ff-ignore-include)
(make-variable-buffer-local 'ff-quiet-mode)
(make-variable-buffer-local 'ff-other-file-alist)
(make-variable-buffer-local 'ff-search-directories)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User entry points

;;;###autoload
(defun ff-get-other-file (&optional in-other-window)
  "Find the corresponding header or source file to this source or header
file. See also the documentation for ff-find-other-file.

If optional IN-OTHER-WINDOW is non-nil, finds the file in another window.

Arguments: (&optional in-other-window)"
  (interactive "P")
  (let ((ignore ff-ignore-include))
    (setq ff-ignore-include t)
    (ff-find-the-other-file in-other-window)
    (setq ff-ignore-include ignore)))

;;;###autoload
(defun ff-find-other-file (&optional in-other-window ignore-include)
  "Find the corresponding header or source file to this source or header
file; being on a #include line pulls in that file.

If optional IN-OTHER-WINDOW is non-nil, finds the file in the other window.
If optional IGNORE-INCLUDE is non-nil, ignores being on #include lines.

Arguments: (&optional in-other-window ignore-include)

Variables of interest include:

 - ff-case-fold-search
   Non-nil means ignore cases in matches (see case-fold-search).
   If you have extensions in different cases, you will want this to be nil.

 - ff-always-in-other-window 
   If non-nil, always open the other file in another window, unless an
   argument is given to ff-find-other-file.

 - ff-ignore-include 
   If non-nil, ignores #include lines.

 - ff-always-try-to-create 
   If non-nil, always attempt to create the other file if it was not found.

 - ff-quiet-mode 
   If non-nil, traces which directories are being searched.

 - ff-special-constructs 
   A list of regular expressions specifying how to recognise special 
   constructs such as include files etc, and an associated method for 
   extracting the filename from that construct.

 - ff-other-file-alist
   Alist of extensions to find given the current file's extension.

 - ff-search-directories 
   List of directories searched through with each extension specified in
   ff-other-file-alist that matches this file's extension.

 - ff-pre-find-hooks 
   List of functions to be called before the search for the file starts.

 - ff-pre-load-hooks 
   List of functions to be called before the other file is loaded.

 - ff-post-load-hooks
   List of functions to be called after the other file is loaded.

 - ff-not-found-hooks
   List of functions to be called if the other file could not be found.

 - ff-file-created-hooks
   List of functions to be called if the other file has been created."
  (interactive "P")
  (let ((ignore ff-ignore-include))
    (setq ff-ignore-include ignore-include)
    (ff-find-the-other-file in-other-window)
    (setq ff-ignore-include ignore)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Support functions

(defun ff-gnu-emacs-19 ()
  (string-match "^19\\.[0-9]+\\.[0-9]+$" emacs-version))

(defun ff-xemacs ()
  (or (string-match "Lucid"  emacs-version)
      (string-match "XEmacs" emacs-version)))

(defun ff-find-the-other-file (&optional in-other-window)
  "Find the corresponding header or source file to this source or header
file; being on a #include line pulls in that file, but see the help on
the ff-ignore-include variable.

If optional IN-OTHER-WINDOW is non-nil, finds the file in another window.

Arguments: (&optional in-other-window)"

  (let (match           ;; matching regexp for this file
        suffixes        ;; set of replacing regexps for the matching regexp
        action          ;; function to generate the names of the other files
        fname           ;; basename of this file
        pos             ;; where we start matching filenames
        stub            ;; name of the file without extension
        alist           ;; working copy of the list of file extensions
        pathname        ;; the pathname of the file or the #include line
        default-name    ;; file we should create if none found
        format          ;; what we have to match    
        found           ;; name of the file or buffer found - nil if none 
        dirs            ;; local value of ff-search-directories
        no-match)       ;; whether we know about this kind of file

    (if ff-pre-find-hooks
        (run-hooks 'ff-pre-find-hooks))

    (message "Working...")

    (setq dirs
          (if (symbolp ff-search-directories)
              (ff-list-replace-env-vars (symbol-value ff-search-directories))
            (ff-list-replace-env-vars ff-search-directories)))

    (save-excursion
      (beginning-of-line 1)
      (setq fname (ff-treat-as-special)))

    (cond
     ((and (not ff-ignore-include) fname)
      (setq default-name fname)
      (setq found (ff-get-file dirs fname nil in-other-window)))

     ;; let's just get the corresponding file
     (t
      (setq alist (if (symbolp ff-other-file-alist)
                      (symbol-value ff-other-file-alist)
                    ff-other-file-alist)
            pathname (if (buffer-file-name)
                         (buffer-file-name)
                       "/none.none"))

      (string-match ".*/\\(.+\\)$" pathname)
      (setq fname (substring pathname (match-beginning 1) (match-end 1))
            no-match nil
            match (car alist))

      ;; find the table entry corresponding to this file
      (setq pos (ff-string-match (car match) fname))
      (while (and match (if (and pos (>= pos 0)) nil (not pos)))
        (setq alist (cdr alist))
        (setq match (car alist))
        (setq pos (ff-string-match (car match) fname)))

      ;; no point going on if we haven't found anything
      (if (not match)
          (setq no-match t)

        ;; otherwise, suffixes contains what we need
        (setq suffixes (car (cdr match))
              action (car (cdr match))
              found nil)

        ;; if we have a function to generate new names, 
        ;; invoke it with the name of the current file
        (if (and (atom action) (fboundp action))
            (progn
              (setq suffixes (funcall action (buffer-file-name))
                    match (cons (car match) (list suffixes))
                    stub nil
                    default-name (car suffixes)))

          ;; otherwise build our filename stub
          (cond 

           ;; get around the problem that 0 and nil both mean false!
           ((= pos 0)
            (setq format "")
            (setq stub "")
            )

           (t
            (setq format (concat "\\(.+\\)" (car match)))
            (string-match format fname)
            (setq stub (substring fname (match-beginning 1) (match-end 1)))
            ))

          ;; if we find nothing, we should try to get a file like this one
          (setq default-name
                (concat stub (car (car (cdr match))))))

        ;; do the real work - find the file
        (setq found 
              (ff-get-file dirs
                           stub
                           suffixes 
                           in-other-window)))))

    (cond
     (no-match                     ;; could not even determine the other file
      (message ""))

     (t 
      (cond

       ((not found)                ;; could not find the other file

        (if ff-not-found-hooks     ;; run the hooks
            (run-hooks 'ff-not-found-hooks))

        (cond 
         (ff-always-try-to-create  ;; try to create the file
          (let (name pathname)

            (setq name
                  (expand-file-name
                   (read-file-name
                    (format "Find or create %s in: " default-name)
                    default-directory default-name nil)))
            
            (setq pathname
                  (if (file-directory-p name)
                      (concat (file-name-as-directory name) default-name)
                    (setq found name)))
            
            (ff-find-file pathname in-other-window t)))

         (t                        ;; don't create the file, just whinge
          (message "no file found for %s" fname))))

       (t                          ;; matching file found
        nil))))

    found))                        ;; return buffer-name or filename

(defun ff-get-file (search-dirs fname-stub &optional suffix-list other-window)
  "Find a file in the SEARCH-DIRS with the given FILENAME (or filename stub). 
If (optional) SUFFIXES is nil, search for fname, otherwise search for fname 
with each of the given suffixes. Gets the file or the buffer corresponding 
to the name of the first file found, or nil.

Arguments: (search-dirs fname-stub &optional suffix-list in-other-window)
"
  (let ((filename (ff-get-file-name search-dirs fname-stub suffix-list)))
            
    (cond 
     ((not filename)
      nil)

     ((bufferp (get-buffer filename))
      (ff-switch-to-buffer filename other-window)
      filename)
               
     ((file-exists-p filename)
      (ff-find-file filename other-window nil)
      filename)

     (t
      nil))))

(defun ff-get-file-name (search-dirs fname-stub &optional suffix-list)
  "Find a file in the SEARCH-DIRS with the given FILENAME (or filename stub). 
If (optional) SUFFIXES is nil, search for fname, otherwise search for fname 
with each of the given suffixes. Returns the name of the first file found.

Arguments: (search-dirs fname-stub &optional suffix-list)
"
  (let* (dirs         ;; working copy of dirs to search
         dir          ;; the current dir considered
         file         ;; filename being looked for
         rest         ;; pathname after first /*
         this-suffix  ;; the suffix we are currently considering
         suffixes     ;; working copy of suffix-list
         filename     ;; built filename
         blist        ;; list of live buffers
         buf          ;; current buffer in blist
         found)       ;; whether we have found anything

    (setq suffixes suffix-list)

    ;; suffixes is nil => fname-stub is the file we are looking for
    ;; otherwise fname-stub is a stub, and we append a suffix
    (if suffixes
        (setq this-suffix (car suffixes))
      (setq this-suffix "")
      (setq suffixes (list "")))
            
    ;; find whether the file is in a buffer first
    (while (and suffixes (not found))
      (setq filename (concat fname-stub this-suffix))

      (if (not ff-quiet-mode)
          (message "finding buffer %s..." filename))

      (if (bufferp (get-buffer filename))
          (setq found filename))

      (setq blist (buffer-list))
      (setq buf (buffer-name (car blist)))
      (while (and blist (not found))

        (if (string-match (concat filename "<[0-9]+>") buf)
            (setq found buf))

        (setq blist (cdr blist))
        (setq buf (buffer-name (car blist))))

      (setq suffixes (cdr suffixes))
      (setq this-suffix (car suffixes)))

    ;; now look for the real file
    (setq dirs search-dirs)
    (setq dir  (car dirs))
    (while (and (not found) dirs)

      (setq suffixes suffix-list)

      ;; if dir does not contain '/*', look for the file
      (if (and dir (not (string-match "\\([^*]*\\)/\\\*\\(/.*\\)*" dir)))
          (progn 
            
            ;; suffixes is nil => fname-stub is the file we are looking for
            ;; otherwise fname-stub is a stub, and we append a suffix
            (if suffixes
                (setq this-suffix (car suffixes))
              (setq this-suffix "")
              (setq suffixes (list "")))
            
            (while (and suffixes (not found))

              (setq filename (concat fname-stub this-suffix))
              (setq file (concat dir "/" filename))
              
              (if (not ff-quiet-mode)
                  (message "finding %s..." file))

              (if (file-exists-p file)
                  (setq found file))
              
              (setq suffixes (cdr suffixes))
              (setq this-suffix (car suffixes))))

        ;; otherwise dir matches the '/*', so search each dir separately
        (progn
          (if (match-beginning 2)
              (setq rest (substring dir (match-beginning 2) (match-end 2)))
            (setq rest "")
            )
          (setq dir  (substring dir (match-beginning 1) (match-end 1)))

          (let ((dirlist (ff-all-dirs-under dir '("..")))
                this-dir compl-dirs)

            (setq this-dir (car dirlist))
            (while dirlist
              (setq compl-dirs
                    (append
                     compl-dirs
                     (list (concat this-dir rest))
                     ))
              (setq dirlist  (cdr dirlist))
              (setq this-dir (car dirlist)))

            (if compl-dirs
                (setq found (ff-get-file-name compl-dirs
                                              fname-stub
                                              suffix-list))))))
      (setq dirs (cdr dirs))
      (setq dir (car dirs)))

    (if found
        (message "%s found" found))

    found))

(defun ff-string-match (regexp string &optional start)
  "Like string-match (which see), but sets case-fold-search to 
ff-case-fold-search before searching, and then resets it back again."
  (let ((exact-match case-fold-search)
        match)
    (if regexp
        (progn
          (setq case-fold-search ff-case-fold-search)
          (setq match (string-match regexp string start))
          (setq case-fold-search exact-match)))
    match))

(defun ff-list-replace-env-vars (search-list)
  "Replace environment variables (of the form $VARIABLE) in SEARCH-LIST."
  (let (list
        (var (car search-list)))
    (while search-list
      (if (string-match "\\(.*\\)\\$[({]*\\([a-zA-Z0-9_]+\\)[)}]*\\(.*\\)" var)
          (setq var
                (concat
                 (substring var (match-beginning 1) (match-end 1))
                 (getenv (substring var (match-beginning 2) (match-end 2)))
                 (substring var (match-beginning 3) (match-end 3)))))
      (setq search-list (cdr search-list))
      (setq list (cons var list))
      (setq var (car search-list)))
    (setq search-list (reverse list))))

(defun ff-treat-as-special ()
  "Returns the file to look for if the construct was special, otherwise
returns nil. The construct is defined in the variable ff-special-constructs 
(which see)."
  (let* (fname
         (list ff-special-constructs)
         (elem (car list))
         (regexp (car elem))
         (match (cdr elem)))
    (while (and list (not fname))
      (if (and (looking-at regexp) match)
          (setq fname (funcall match)))
      (setq list (cdr list))
      (setq elem (car list))
      (setq regexp (car elem))
      (setq match (cdr elem)))
    fname))

(defun ff-basename (string)
  "Returns the basename of PATHNAME."
  (setq string (concat "/" string))
  (string-match ".*/\\([^/]+\\)$" string)
  (setq string (substring string (match-beginning 1) (match-end 1))))

(defun ff-all-dirs-under (here &optional exclude)
  "Get all the directory files under DIRECTORY.
Exclude all files in the optional EXCLUDE list."
  (if (file-directory-p here)
      (condition-case nil
          (progn
            (let ((files (directory-files here t))
                  (dirlist (list))
                  file)
              (while files
                (setq file (car files))
                (if (and
                     (file-directory-p file)
                     (not (member (ff-basename file) exclude)))
                    (setq dirlist (cons file dirlist)))
                (setq files (cdr files)))
              (setq dirlist (reverse dirlist))))
        (error nil))
    nil))

(defun ff-switch-file (f1 f2 file &optional in-other-window new-file)
  "Calls Function2 or Function1 with FILE as argument, depending on whether 
(optional) OTHER-WINDOW is set or not. Function1 and Function2 are typically 
find-file / find-file-other-window or switch-to-buffer / switch-to-buffer-
other-window function pairs.

If optional NEW-FILE is t, then a special hook (ff-file-created-hooks) is 
called before ff-post-load-hooks.

Arguments: (function1 function2 file &optional in-other-window new-file)
"
  (if ff-pre-load-hooks
      (run-hooks 'ff-pre-load-hooks))
  (if (or
       (and in-other-window (not ff-always-in-other-window))
       (and (not in-other-window) ff-always-in-other-window))
      (funcall f2 file)
    (funcall f1 file))
  (if new-file
      (if ff-file-created-hooks
          (run-hooks 'ff-file-created-hooks)))
  (if ff-post-load-hooks
      (run-hooks 'ff-post-load-hooks)))

(defun ff-find-file (file &optional in-other-window new-file)
  "Like find-file (which see), but checks whether the file goes in another 
window or not.

Arguments: (file &optional in-other-window new-file)
"
  (ff-switch-file 'find-file 
                  'find-file-other-window 
                  file in-other-window new-file))

(defun ff-switch-to-buffer (file &optional in-other-window)
  "Like switch-to-buffer (which see), but checks whether the buffer ends up 
in another window or not.

Arguments: (file &optional in-other-window)
"
  (ff-switch-file 'switch-to-buffer 
                  'switch-to-buffer-other-window 
                  file in-other-window nil))

(cond 
 ((ff-gnu-emacs-19)
  (defun ff-goto-click (event)
    (set-buffer (window-buffer (posn-window (event-end event))))
    (goto-char (posn-point (event-end event))))

  ;;;###autoload
  (defun ff-mouse-find-other-file (event)
    "Visit the file you click on."
    (interactive "e")
    (save-excursion
      (ff-goto-click event)
      (ff-find-other-file nil)))

  ;;;###autoload
  (defun ff-mouse-find-other-file-other-window (event)
    "Visit the file you click on."
    (interactive "e")
    (save-excursion
      (ff-goto-click event)
      (ff-find-other-file t)))

  ;;;###autoload
  (defun locate-file (fname dirs &optional suffix-list ignore-perms)
    "Defines XEmacs look-alike locate-file for GNU Emacs-19."
    (interactive)
    (ff-get-file dirs fname suffix-list)) 
  )

 ((ff-xemacs)

  ;;;###autoload
  (defun ff-mouse-find-other-file (event)
    "Visit the file you click on."
    (interactive "@e")
    (save-excursion
      (mouse-set-point event)
      (ff-find-other-file nil)))

  ;;;###autoload
  (defun ff-mouse-find-other-file-other-window (event)
    "Visit the file you click on."
    (interactive "@e")
    (save-excursion
      (mouse-set-point event)
      (ff-find-other-file t))) 
  ))

(provide 'find-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This section offers an example of user defined function to select files

(defun upcase-p (string &optional start end)
  "Return t if this string is all uppercase. Given START and/or END,
checks between these characters."
  (let (match str)
    (if (not start)
        (setq start 0))
    (if (not end)
        (setq end (length string)))
    (if (= start end)
        (setq end (1+ end)))
    (setq str (substring string start end))
    (if (and 
         (ff-string-match "[A-Z]+" str)
         (setq match (match-data))
         (= (car match) 0)
         (= (car (cdr match)) (length str)))
        t
      nil)))

(defun ff-cc-hh-converter (arg)
  "Discriminate file extensions and build up a new file list based 
possibly on part of the directory name and the name of the file 
passed in."
  (ff-string-match "\\(.*\\)/\\([^/]+\\)/\\([^.]+\\).\\([^/]+\\)$" arg)
  (let ((path (if (match-beginning 1) 
                  (substring arg (match-beginning 1) (match-end 1)) nil))
        (dire (if (match-beginning 2) 
                  (substring arg (match-beginning 2) (match-end 2)) nil))
        (file (if (match-beginning 3) 
                  (substring arg (match-beginning 3) (match-end 3)) nil))
        (extn (if (match-beginning 4) 
                  (substring arg (match-beginning 4) (match-end 4)) nil))
        return-list)
    (cond
     ;; fooZapJunk.cc => ZapJunk.{hh,h} or fooZapJunk.{hh,h}
     ((and (string= extn "cc") 
           (ff-string-match "^\\([a-z]+\\)\\([A-Z].+\\)$" file))
      (let ((stub  (substring file (match-beginning 2) (match-end 2))))
        (setq dire (upcase (substring file (match-beginning 1) (match-end 1))))
        (setq return-list (list (concat stub ".hh")
                                (concat stub ".h")
                                (concat file ".hh")
                                (concat file ".h")))
        ))
     ;; FOO/ZapJunk.hh => fooZapJunk.{cc,C} or ZapJunk.{cc,C}
     ((and (string= extn "hh") (upcase-p dire) file)
      (let ((stub (concat (downcase dire) file)))
        (setq return-list (list (concat stub ".cc")           
                                (concat stub ".C")
                                (concat file ".cc")
                                (concat file ".C")))
        ))
     ;; zap.cc => zap.hh or zap.h
     ((string= extn "cc")
      (let ((stub file))
        (setq return-list (list (concat stub ".hh")
                                (concat stub ".h")))
        ))
     ;; zap.hh => zap.cc or zap.C
     ((string= extn "hh")
      (let ((stub file))
        (setq return-list (list (concat stub ".cc")
                                (concat stub ".C")))
        ))
     (t 
      nil))
    
    return-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This section offers an example of user defined function to place point.
;; The regexps are Ada specific.

(defvar ff-function-name nil "Name of the function we are in.")

(defvar ada-procedure-start-regexp)
(defvar ada-package-start-regexp)

;; bind with (setq ff-pre-load-hooks 'ff-which-function-are-we-in)
;;
(defun ff-which-function-are-we-in ()
  "Determine whether we are on a function definition/declaration and 
remember the name of that function."

  (setq ff-function-name nil)

  (save-excursion
    (if (re-search-backward ada-procedure-start-regexp nil t)
        (setq ff-function-name (buffer-substring (match-beginning 0)
                                                 (match-end 0)))
      ; we didn't find a procedure start, perhaps there is a package
      (if (re-search-backward ada-package-start-regexp nil t)
          (setq ff-function-name (buffer-substring (match-beginning 0)
                                                   (match-end 0)))
        ))))

;; bind with (setq ff-post-load-hooks 'ff-set-point-accordingly)
;;
(defun ff-set-point-accordingly ()
  "Find the function specified in ff-function-name, previously 
determined by ff-which-function-are-we-in."
  (if ff-function-name
      (progn
        (goto-char (point-min))
        (search-forward ff-function-name nil t))))

;; find-file.el ends here

