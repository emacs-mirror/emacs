;; @(#) ada-xref.el --- use Gnat for lookup and completion in Ada mode

;; Copyright (C) 1994, 1995--1998, 1999 Free Software Foundation, Inc.

;; Author: Markus Heritsch <Markus.Heritsch@studbox.uni-stuttgart.de>
;;      Rolf Ebert <ebert@inf.enst.fr>
;;      Emmanuel Briot <briot@gnat.com>
;; Maintainer: Emmanuel Briot <briot@gnat.com>
;; Ada Core Technologies's version:   $Revision: 1.75 $
;; Keywords: languages ada xref

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;;; This Package provides a set of functions to use the output of the
;;; cross reference capabilities of the GNAT Ada compiler
;;; for lookup and completion in Ada mode.
;;;
;;; The functions provided are the following ones :
;;;    - `ada-complete-identifier': completes the current identifier as much as
;;;      possible, depending of the known identifier in the unit
;;;    - `ada-point-and-xref': moves the mouse pointer and shows the declaration
;;;      of the selected identifier (either in the same buffer or in another
;;;      buffer
;;;    - `ada-goto-declaration': shows the declaration of the selected
;;;      identifier (the one under the cursor), either in the same buffer or in
;;;      another buffer
;;;    - `ada-goto-declaration-other-frame': same as previous, but opens a new
;;      frame to show the declaration
;;;    - `ada-compile-application': recompile your whole application, provided
;;;      that a project file exists in your directory
;;;    - `ada-run-application': run your application directly from Emacs
;;;    - `ada-reread-prj-file': force Emacs to read your project file again.
;;;      Otherwise, this file is only read the first time Emacs needs some
;;;      informations, which are then kept in memory
;;;    - `ada-change-prj': change the prj file associated with a buffer
;;;    - `ada-change-default-prj': change the default project file used for
;;;      every new buffer
;;;
;;; If a file *.`adp' exists in the ada-file directory, then it is
;;; read for configuration informations. It is read only the first
;;; time a cross-reference is asked for, and is not read later.

;;; You need Emacs >= 20.2 to run this package

;; ----- Requirements -----------------------------------------------------

(require 'compile)
(require 'comint)

;; ----- Dynamic byte compilation -----------------------------------------
(defvar byte-compile-dynamic nil)
(make-local-variable 'byte-compile-dynamic)
(setq byte-compile-dynamic t)

;; ------ Use variables
(defcustom ada-xref-other-buffer t
  "*If nil, always display the cross-references in the same buffer.
Otherwise create either a new buffer or a new frame."
  :type 'boolean :group 'ada)

(defcustom ada-xref-create-ali t
  "*If non-nil, run gcc whenever the cross-references are not up-to-date.
If nil, the cross-reference mode will never run gcc."
  :type 'boolean :group 'ada)

(defcustom ada-xref-confirm-compile nil
  "*If non-nil, always ask for user confirmation before compiling or running
the application."
  :type 'boolean :group 'ada)

(defcustom ada-krunch-args "0"
  "*Maximum number of characters for filenames created by gnatkr.
Set to 0, if you don't use crunched filenames. This should be a string."
  :type 'string :group 'ada)

(defcustom ada-prj-default-comp-cmd
  "${cross_prefix}gcc -c -g -gnatq ${comp_opt} -I${src_dir}"
  "*Default command to be used to compile a single file.
Emacs will add the filename at the end of this command.
This is the same syntax as in the project file."
  :type 'string :group 'ada)

(defcustom ada-prj-default-make-cmd
  (concat "${cross_prefix}gnatmake ${main} -aI${src_dir} -aO${obj_dir} "
          "-g -gnatq -cargs ${comp_opt} "
          "-bargs ${bind_opt} -largs ${link_opt}")
  "*Default command to be used to compile the application.
This is the same syntax as in the project file."
  :type 'string :group 'ada)

(defcustom ada-prj-default-project-file ""
  "*Name of the project file to use for every Ada file.
Emacs will not try to use the standard algorithm to find the project file if
this string is not empty."
  :type '(file :must-match t) :group 'ada)

(defcustom ada-gnatstub-opts "-q -I${src_dir}"
  "*List of the options to pass to gnatsub to generate the body of a package.
This has the same syntax as in the project file (with variable substitution)."
  :type 'string :group 'ada)

(defcustom ada-always-ask-project nil
  "*If nil, use default values when no project file was found.
Otherwise, ask the user for the name of the project file to use.")

;; ------- Nothing to be modified by the user below this
(defvar ada-last-prj-file ""
  "Name of the last project file entered by the user.")

(defvar ada-check-switch " -gnats "
  "Switch added to the command line to check the current file.")

(defvar ada-project-file-extension ".adp"
  "The extension used for project files.")

(defconst is-windows (memq system-type (quote (windows-nt)))
  "True if we are running on windows NT or windows 95.")

(defvar ada-xref-pos-ring '()
  "List of positions selected by the cross-references functions.
Used to go back to these positions.")

(defconst ada-xref-pos-ring-max 16
  "Number of positions kept in the list ada-xref-pos-ring.")

(defvar ada-operator-re
  "\\+\\|-\\|/\\|\\*\\|=\\|mod\\|and\\|not\\|or\\|xor\\|<=\\|<\\|>=\\|>"
  "Regexp to match for operators.")

(defvar ada-xref-default-prj-file nil
  "Name of the default prj file, per directory.
Every directory is potentially associated with a default project file.
If it is nil, then the first prj file loaded will be the default for this
Emacs session.")

;; These variables will be overwritted by buffer-local variables
(defvar ada-prj-prj-file nil
  "Name of the project file for the current ada buffer.")
(defvar ada-prj-src-dir nil
  "List of directories to look into for ada sources.")
(defvar ada-prj-obj-dir nil
  "List of directories to look into for object and .ali files.")
(defvar ada-prj-comp-opt nil
  "Switches to use on the command line for the default compile command.")
(defvar ada-prj-bind-opt nil
  "Switches to use on the command line for the default bind command.")
(defvar ada-prj-link-opt nil
  "Switches to use on the command line for the default link command.")
(defvar ada-prj-comp-cmd nil
  "Command to use to compile the current file only.")
(defvar ada-prj-make-cmd nil
  "Command to use to compile the whole current application.")
(defvar ada-prj-run-cmd nil
  "Command to use to run the current application.")
(defvar ada-prj-debug-cmd nil
  "Command to use to run the debugger.")
(defvar ada-prj-main nil
  "Name of the main programm of the current application.")
(defvar ada-prj-remote-machine nil
  "Name of the machine to log on before a compilation.")
(defvar ada-prj-cross-prefix nil
  "Prefix to be added to the gnatmake, gcc, ... commands when
using a cross-compilation environment.
A '-' is automatically added at the end if not already present.
For instance, the compiler is called `ada-prj-cross-prefix'gnatmake.")

;; ----- Keybindings ------------------------------------------------------

(defun ada-add-keymap ()
  "Add new key bindings when using `ada-xrel.el'."
  (interactive)
  (if ada-xemacs
      (progn
        (define-key ada-mode-map '(shift button3) 'ada-point-and-xref)
        (define-key ada-mode-map '(control tab) 'ada-complete-identifier))
    (define-key ada-mode-map [C-tab] 'ada-complete-identifier)
    (define-key ada-mode-map [S-mouse-3] 'ada-point-and-xref))

  (define-key ada-mode-map "\C-co"    'ff-find-other-file)
  (define-key ada-mode-map "\C-c5\C-d" 'ada-goto-declaration-other-frame)
  (define-key ada-mode-map "\C-c\C-d" 'ada-goto-declaration)
  (define-key ada-mode-map "\C-c\C-s" 'ada-xref-goto-previous-reference)
  (define-key ada-mode-map "\C-c\C-x" 'ada-reread-prj-file)
  (define-key ada-mode-map [f10] 'next-error)
  (define-key ada-mode-map "\C-c\C-c" 'ada-compile-application)
  (define-key ada-mode-map "\C-cb"  'ada-buffer-list)
  (define-key ada-mode-map "\C-cc"  'ada-change-prj)
  (define-key ada-mode-map "\C-cd"  'ada-change-default-prj)
  (define-key ada-mode-map "\C-cg"  'ada-gdb-application)
  (define-key ada-mode-map "\C-cr"  'ada-run-application)
  (define-key ada-mode-map "\C-c\C-o" 'ada-goto-parent)
  (define-key ada-mode-map "\C-c\C-r" 'ada-find-references)
  (define-key ada-mode-map "\C-c\C-v" 'ada-check-current)
  )

;; ----- Menus --------------------------------------------------------------
(defun ada-add-ada-menu ()
  "Add some items to the standard Ada mode menu."
  (interactive)

  (if ada-xemacs
      (progn
        (add-menu-button '("Ada") ["Check file" ada-check-current t] "Goto")
        (add-menu-button '("Ada") ["Compile file" ada-compile-current t]
			 "Goto")
        (add-menu-button '("Ada") ["Build" ada-compile-application t] "Goto")
        (add-menu-button '("Ada") ["Run" ada-run-application t] "Goto")
        (add-menu-button '("Ada") ["Debug" ada-gdb-application t] "Goto")
        (add-menu-button '("Ada") ["--" nil t] "Goto")
        (add-submenu '("Ada") '("Project"
                                ["Associate"   ada-change-prj t]
                                ["Set Default" ada-set-default-project-file t]
                                ["List" ada-buffer-list t])
                     "Goto")
        (add-menu-button '("Ada" "Goto") ["Goto Parent Unit" ada-goto-parent t]
                         "Next compilation error")
        (add-menu-button '("Ada" "Goto") ["Goto References to any entity"
					  ada-find-any-references t]
                         "Next compilation error")
        (add-menu-button '("Ada" "Goto") ["List References"
					  ada-find-references t]
                         "Next compilation error")
        (add-menu-button '("Ada" "Goto") ["Goto Declaration Other Frame"
                                          ada-goto-declaration-other-frame t]
                         "Next compilation error")
        (add-menu-button '("Ada" "Goto") ["Goto Declaration/Body"
					  ada-goto-declaration t]
                         "Next compilation error")
        (add-menu-button '("Ada" "Goto") ["Goto Previous Reference"
					  ada-xref-goto-previous-reference t]
                         "Next compilation error")
        (add-menu-button '("Ada" "Goto") ["--" nil t]
                         "Next compilation error")
        (add-menu-button '("Ada" "Edit") ["Complete Identifier"
					  ada-complete-identifier t]
                         "Indent Line")
        (add-menu-button '("Ada" "Edit") ["--------" nil t]
                         "Indent Line")
	(add-menu-button '("Ada" "Help") ["Gnat User Guide" (info "gnat_ug")])
	(add-menu-button '("Ada" "Help") ["Gnat Reference Manual"
					  (info "gnat_rm")])
	(add-menu-button '("Ada" "Help") ["Gcc Documentation" (info "gcc")])
	(add-menu-button '("Ada" "Help") ["Gdb Documentation" (info "gdb")])
        )

    ;; for Emacs
    (define-key-after (lookup-key ada-mode-map [menu-bar Ada]) [Check]
      '("Check file" . ada-check-current) 'Customize)
    (define-key-after (lookup-key ada-mode-map [menu-bar Ada]) [Compile]
      '("Compile file" . ada-compile-current) 'Check)
    (define-key-after (lookup-key ada-mode-map [menu-bar Ada]) [Build]
      '("Build" . ada-compile-application) 'Compile)
    (define-key-after (lookup-key ada-mode-map [menu-bar Ada]) [Run]
      '("Run" . ada-run-application) 'Build)
    (define-key-after (lookup-key ada-mode-map [menu-bar Ada]) [Debug]
      '("Debug" . ada-gdb-application) 'Run)
    (define-key-after (lookup-key ada-mode-map [menu-bar Ada]) [rem]
      '("--" . nil) 'Debug)
    (define-key-after (lookup-key ada-mode-map [menu-bar Ada]) [Project]
      (cons "Project" (easy-menu-create-menu
                       "Project"
                       '(["Associate" ada-change-prj t]
                         ["Set Default" ada-set-default-project-file t]
                         ["List" ada-buffer-list t])))
      'rem)

    (let ((help-submenu (lookup-key ada-mode-map [menu-bar Ada Help]))
          (goto-submenu (lookup-key ada-mode-map [menu-bar Ada Goto]))
          (edit-submenu (lookup-key ada-mode-map [menu-bar Ada Edit])))

      (define-key help-submenu [Gnat_ug]
        '("Gnat User Guide" . (lambda() (interactive) (info "gnat_ug"))))
      (define-key help-submenu [Gnat_rm]
        '("Gnat Reference Manual" . (lambda() (interactive) (info "gnat_rm"))))
      (define-key help-submenu [Gcc]
        '("Gcc Documentation" . (lambda() (interactive) (info "gcc"))))
      (define-key help-submenu [gdb]
        '("Ada Aware Gdb Documentation" .
	  (lambda() (interactive) (info "gdb"))))
      (define-key goto-submenu [rem]    '("----" . nil))
      (define-key goto-submenu [Parent]
	'("Goto Parent Unit" . ada-goto-parent))
      (define-key goto-submenu [References-any]
        '("Goto References to any entity" . ada-find-any-references))
      (define-key goto-submenu [References]
        '("List References" . ada-find-references))
      (define-key goto-submenu [Prev]
        '("Goto Previous Reference" . ada-xref-goto-previous-reference))
      (define-key goto-submenu [Decl-other]
        '("Goto Declaration Other Frame" . ada-goto-declaration-other-frame))
      (define-key goto-submenu [Decl]
        '("Goto Declaration/Body" . ada-goto-declaration))

      (define-key edit-submenu [rem] '("----" . nil))
      (define-key edit-submenu [Complete] '("Complete Identifier"
                                            . ada-complete-identifier))
      )
    ))

;; ----- Utilities -------------------------------------------------

(defun ada-require-project-file ()
  "If no project file is assigned to this buffer, load one."
  (if (not ( my-local-variable-if-set-p 'ada-prj-src-dir (current-buffer)))
      (ada-parse-prj-file (ada-prj-find-prj-file))))

(defun my-local-variable-if-set-p (variable &optional buffer)
  "Returns t if VARIABLE is local in BUFFER and is non-nil."
  (and (local-variable-p variable buffer)
       (save-excursion
         (set-buffer buffer)
         (symbol-value variable))))

(defun ada-xref-push-pos (filename position)
  "Push (FILENAME, POSITION) on the position ring for cross-references."
  (setq ada-xref-pos-ring (cons (list position filename) ada-xref-pos-ring))
  (if (> (length ada-xref-pos-ring) ada-xref-pos-ring-max)
      (setcdr (nthcdr (1- ada-xref-pos-ring-max) ada-xref-pos-ring) nil)))

(defun ada-xref-goto-previous-reference ()
  "Go to the previous cross-reference we were on."
  (interactive)
  (if ada-xref-pos-ring
      (let ((pos (car ada-xref-pos-ring)))
	(setq ada-xref-pos-ring (cdr ada-xref-pos-ring))
	(find-file (car (cdr pos)))
	(goto-char (car pos)))))

(defun ada-convert-file-name (name)
  "Converts from NAME to a name that can be used by the compilation commands.
This is overriden on VMS to convert from VMS filenames to Unix filenames."
  name)

(defun ada-set-default-project-file (name)
  "Set the file whose name is NAME as the default project file."
  (interactive "fProject file:")
  (set 'ada-prj-default-project-file name)
  (ada-reread-prj-file t)
  )

(defun ada-replace-substring (cmd-string search-for replace-with)
  "Replace all instances of SEARCH-FOR with REPLACE-WITH in CMD-STRING."
  (while (string-match search-for cmd-string)
    (setq cmd-string (replace-match replace-with t t cmd-string)))
  cmd-string)

(defun ada-treat-cmd-string (cmd-string)
  "Replace meta-sequences like ${...} in CMD-STRING with the appropriate value.
The current buffer must be the one where all local variable are defined (that
is the ada source)"
  (if ( my-local-variable-if-set-p 'ada-prj-src-dir (current-buffer))
      (if  (string-match "\\(-[^-\$I]*I\\)\${src_dir}" cmd-string)
          (progn
            (let ((str-def (substring cmd-string (match-beginning 1)
                                      (match-end 1))))
              (setq cmd-string
                    (ada-replace-substring cmd-string
                                           "\\(-[^-\$I]*I\\)\${src_dir}"
                      (mapconcat
                      (lambda (x) (concat str-def x))
                      ada-prj-src-dir " ")))))))
  (if ( my-local-variable-if-set-p 'ada-prj-obj-dir (current-buffer))
      (if (string-match "\\(-[^-\$O]*O\\)\${obj_dir}" cmd-string)
          (progn
            (let ((str-def (substring cmd-string (match-beginning 1)
                                      (match-end 1))))
              (setq cmd-string
                    (ada-replace-substring cmd-string
                                           "\\(-[^-\$O]*O\\)\${obj_dir}"
                        (mapconcat
                         (lambda (x) (concat str-def x))
                         ada-prj-obj-dir
                         " ")))))))
  (if ( my-local-variable-if-set-p 'ada-prj-remote-machine (current-buffer))
      (setq cmd-string
            (ada-replace-substring cmd-string "\${remote_machine}"
                                   ada-prj-remote-machine)))
  (if ( my-local-variable-if-set-p 'ada-prj-comp-opt (current-buffer))
      (setq cmd-string
            (ada-replace-substring cmd-string "\${comp_opt}"
                                   ada-prj-comp-opt)))
  (if ( my-local-variable-if-set-p 'ada-prj-bind-opt (current-buffer))
      (setq cmd-string
            (ada-replace-substring cmd-string "\${bind_opt}"
                                   ada-prj-bind-opt)))
  (if ( my-local-variable-if-set-p 'ada-prj-link-opt (current-buffer))
      (setq cmd-string
            (ada-replace-substring cmd-string "\${link_opt}"
                                   ada-prj-link-opt)))
  (if ( my-local-variable-if-set-p 'ada-prj-main (current-buffer))
      (setq cmd-string
            (ada-replace-substring cmd-string "\${main}"
                                   ada-prj-main)))
  (if ( my-local-variable-if-set-p 'ada-prj-cross-prefix (current-buffer))
      (setq cmd-string
            (ada-replace-substring cmd-string "\${cross_prefix}"
                                   ada-prj-cross-prefix)))
  cmd-string)


(defun ada-prj-find-prj-file (&optional no-user-question)
  "Find the prj file associated with the current buffer.
The rules are the following ones :
- If the buffer is already associated with a prj file, use this one
- else if there's a default prj file for the same directory use it
- else if a prj file with the same filename exists, use it
- else if there's only one prj file in the directory, use it
- else if there are more than one prj file, ask the user
- else if there is no prj file and NO-USER-QUESTION is nil, ask the user
  for the project file to use."
  (let* ((current-file (buffer-file-name))
         (first-choice (concat
                        (file-name-sans-extension current-file)
                        ada-project-file-extension))
         (dir          (file-name-directory current-file))

	 ;; on Emacs 20.2, directory-files does not work if
	 ;; parse-sexp-lookup-properties is set
	 (parse-sexp-lookup-properties nil)
         (prj-files    (directory-files
                        dir t
                        (concat ".*" (regexp-quote ada-project-file-extension) "$")))
         (choice       nil)
         (default      (assoc dir ada-xref-default-prj-file))
         )

    (cond

     ((my-local-variable-if-set-p 'ada-prj-prj-file (current-buffer))
      ada-prj-prj-file)

     (default                       ;; directory default project file
       (cdr default))

     ;; global default project file
     ((and ada-prj-default-project-file
           (not (string= ada-prj-default-project-file "")))
      ada-prj-default-project-file)

     ((file-exists-p first-choice)
      first-choice)

     ((= (length prj-files) 1)
      (car prj-files))

     ((> (length prj-files) 1)
      ;; more than one possible prj file => ask the user
      (with-output-to-temp-buffer "*choice list*"
        (princ "There are more than one possible project file. Which one should\n")
        (princ "I use ?\n\n")
        (princ "  no.   file name  \n")
        (princ "  ---   ------------------------\n")
        (let ((counter 1))
          (while (<= counter (length prj-files))
            (princ (format "  %2d)    %s\n"
                           counter
                           (nth (1- counter) prj-files)))
            (setq counter (1+ counter))
            ) ; end of while
          ) ; end of let
        ) ; end of with-output-to ...
      (setq choice nil)
      (while (or
              (not choice)
              (not (integerp choice))
              (< choice 1)
              (> choice (length prj-files)))
        (setq choice (string-to-int
                      (read-from-minibuffer "Enter No. of your choice: "
                                            ))))
      (nth (1- choice) prj-files))

     ((= (length prj-files) 0)
      ;;  no project file found. Ask the user about it (the default value
      ;;  is the last one the user entered.
      (if (or no-user-question (not ada-always-ask-project))
          nil
        (setq ada-last-prj-file
              (read-file-name "project file:" nil ada-last-prj-file))
        (if (string= ada-last-prj-file "") nil ada-last-prj-file))
     )
  )))


(defun ada-parse-prj-file (prj-file)
  "Reads and parses the project file PRJ-FILE.
Does nothing if PRJ-FILE was not found.
The current buffer should be the ada-file buffer"

  (let ((tmp-src-dir  nil)
        (tmp-obj-dir  nil)
        (tmp-comp-opt nil)
        (tmp-bind-opt nil)
        (tmp-link-opt nil)
        (tmp-main     nil)
        (tmp-comp-cmd nil)
        (tmp-make-cmd nil)
        (tmp-run-cmd  nil)
        (tmp-debug-cmd nil)
        (tmp-remote-machine nil)
        (tmp-cross-prefix nil)
        (tmp-cd-cmd   (if prj-file
                          (concat "cd " (file-name-directory prj-file) " && ")
                        (concat "cd " (file-name-directory (buffer-file-name (current-buffer))) " && ")))
        (ada-buffer (current-buffer))
        )
    ;; tries to find a project file in the current directory
    (if prj-file
        (progn
          (find-file prj-file)

          ;; first look for the src_dir lines
          (widen)
          (goto-char (point-min))
          (while
              (re-search-forward "^src_dir=\\(.*\\)" nil t)
            (progn
              (setq tmp-src-dir (cons
                                 (file-name-as-directory
                                  (match-string 1))
                                 tmp-src-dir
                                 ))))
          ;; then for the obj_dir lines
          (goto-char (point-min))
          (while (re-search-forward "^obj_dir=\\(.*\\)" nil t)
            (setq tmp-obj-dir (cons
                               (file-name-as-directory
                                (match-string 1))
                               tmp-obj-dir
                               )))

          ;; then for the options lines
          (goto-char (point-min))
          (if (re-search-forward "^comp_opt=\\(.*\\)" nil t)
              (setq tmp-comp-opt (match-string 1)))
          (goto-char (point-min))
          (if (re-search-forward "^bind_opt=\\(.*\\)" nil t)
              (setq tmp-bind-opt (match-string 1)))
          (goto-char (point-min))
          (if (re-search-forward "^link_opt=\\(.*\\)" nil t)
              (setq tmp-link-opt (match-string 1)))
          (goto-char (point-min))
          (if (re-search-forward "^main=\\(.*\\)" nil t)
              (setq tmp-main (match-string 1)))
          (goto-char (point-min))
          (if (re-search-forward "^comp_cmd=\\(.*\\)" nil t)
              (setq tmp-comp-cmd (match-string 1)))
          (goto-char (point-min))
          (if (re-search-forward "^remote_machine=\\(.*\\)" nil t)
              (setq tmp-remote-machine (match-string 1)))
          (goto-char (point-min))
          (if (re-search-forward "^cross_prefix=\\(.*\\)" nil t)
              (setq tmp-cross-prefix (match-string 1)))
          (goto-char (point-min))
          (if (re-search-forward "^make_cmd=\\(.*\\)" nil t)
              (setq tmp-make-cmd (match-string 1)))
          (goto-char (point-min))
          (if (re-search-forward "^run_cmd=\\(.*\\)" nil t)
              (setq tmp-run-cmd (match-string 1)))
          (goto-char (point-min))
          (if (re-search-forward "^debug_cmd=\\(.*\\)" nil t)
              (setq tmp-debug-cmd (match-string 1)))

          ;; kills the project file buffer, and go back to the ada buffer
          (kill-buffer nil)
          (set-buffer ada-buffer)
          ))

    ;; creates local variables (with default values if needed)
    (set (make-local-variable 'ada-prj-prj-file) prj-file)

    (set (make-local-variable 'ada-prj-src-dir)
         (if tmp-src-dir (reverse tmp-src-dir)  '("./")))

    (set (make-local-variable 'ada-prj-obj-dir)
         (if tmp-obj-dir (reverse tmp-obj-dir)  '("./")))

    (set (make-local-variable 'ada-prj-comp-opt)
         (if tmp-comp-opt tmp-comp-opt ""))

    (set (make-local-variable 'ada-prj-bind-opt)
         (if tmp-bind-opt tmp-bind-opt ""))

    (set (make-local-variable 'ada-prj-link-opt)
         (if tmp-link-opt tmp-link-opt ""))

    (set (make-local-variable 'ada-prj-cross-prefix)
         (if tmp-cross-prefix
             (if (or (string= tmp-cross-prefix "")
                     (= (aref tmp-cross-prefix (1- (length tmp-cross-prefix))) ?-))
                 tmp-cross-prefix
               (concat tmp-cross-prefix "-"))
              ""))

    (set (make-local-variable 'ada-prj-main)
         (if tmp-main tmp-main
           (substring (buffer-file-name) 0 -4)))

    (set (make-local-variable 'ada-prj-remote-machine)
         (ada-treat-cmd-string
          (if tmp-remote-machine tmp-remote-machine "")))

    (set (make-local-variable 'ada-prj-comp-cmd)
         (ada-treat-cmd-string
          (if tmp-comp-cmd tmp-comp-cmd
            (concat tmp-cd-cmd ada-prj-default-comp-cmd))))

    (set (make-local-variable 'ada-prj-make-cmd)
         (ada-treat-cmd-string
          (if tmp-make-cmd tmp-make-cmd
            (concat tmp-cd-cmd ada-prj-default-make-cmd))))

    (set (make-local-variable 'ada-prj-run-cmd)
         (ada-treat-cmd-string
          (if tmp-run-cmd tmp-run-cmd
            (if is-windows "${main}.exe" "${main}"))))

    (set (make-local-variable 'ada-prj-debug-cmd)
         (ada-treat-cmd-string
          (if tmp-debug-cmd tmp-debug-cmd
            (if is-windows
                "${cross_prefix}gdb ${main}.exe"
              "${cross_prefix}gdb ${main}"))))

    ;; Add each directory in src_dir to the default prj list
    (if prj-file
        (mapcar (lambda (x)
                  (if (not (assoc (expand-file-name x)
                                  ada-xref-default-prj-file))
                      (setq ada-xref-default-prj-file
                            (cons (cons (expand-file-name x)
                                        prj-file)
                                  ada-xref-default-prj-file))))
                ada-prj-src-dir))

    ;; Add the directories to the search path for ff-find-other-file
    ;; Do not add the '/' or '\' at the end
    (set (make-local-variable 'ff-search-directories)
         (append (mapcar 'directory-file-name ada-prj-src-dir)
                 ada-search-directories))

    ;; Sets up the compilation-search-path so that Emacs is able to
    ;; go to the source of the errors in a compilation buffer
    (setq compilation-search-path ada-prj-src-dir)

    ))


(defun ada-find-references (&optional pos)
  "Find all references to the entity under POS.
Calls gnatfind to find the references."
  (interactive "")
  (unless pos
    (set 'pos (point)))
  (ada-require-project-file)

  (let* ((identlist (ada-read-identifier pos))
         (alifile (ada-get-ali-file-name (ada-file-of identlist))))

    (set-buffer (get-file-buffer (ada-file-of identlist)))

    ;;  if the file is more recent than the executable
    (if (or (buffer-modified-p (current-buffer))
            (file-newer-than-file-p (ada-file-of identlist) alifile))
        (ada-find-any-references (ada-name-of identlist)
                                 (ada-file-of identlist)
                                 nil nil)
      (ada-find-any-references (ada-name-of identlist)
                               (ada-file-of identlist)
                               (ada-line-of identlist)
                               (ada-column-of identlist))))
  )

(defun ada-find-any-references (entity &optional file line column)
  "Search for references to any entity whose name is ENTITY.
ENTITY was first found the location given by FILE, LINE and COLUMN."
  (interactive "sEntity name: ")
  (ada-require-project-file)

  (let* ((command (concat "gnatfind -rf " entity
                          (if file (concat ":" (file-name-nondirectory file)))
                          (if line (concat ":" line))
                          (if column (concat ":" column)))))

    ;;  If a project file is defined, use it
    (if (my-local-variable-if-set-p 'ada-prj-prj-file (current-buffer))
        (setq command (concat command " -p" ada-prj-prj-file)))

    (compile-internal command "No more references" "gnatfind")

    ;;  Hide the "Compilation" menu
    (save-excursion
      (set-buffer "*gnatfind*")
      (local-unset-key [menu-bar compilation-menu]))
    )
  )

(defun ada-buffer-list ()
  "Display a buffer with all the Ada buffers and their associated project."
  (interactive)
  (save-excursion
    (set-buffer (get-buffer-create "*Buffer List*"))
    (setq buffer-read-only nil)
    (erase-buffer)
    (setq standard-output (current-buffer))
    (princ "The following line is a list showing the associations between
directories and project file. It has the format : ((directory_1 . project_file1)
(directory2 . project_file2)...)\n\n")
    (princ ada-xref-default-prj-file)
    (princ "\n
 Buffer              Mode         Project file
 ------              ----         ------------
\n")
    (let ((bl (buffer-list)))
      (while bl
        (let* ((buffer (car bl))
               (buffer-name (buffer-name buffer))
               this-buffer-mode-name
               this-buffer-project-file)
          (save-excursion
            (set-buffer buffer)
            (setq this-buffer-mode-name
                  (if (eq buffer standard-output)
                      "Buffer Menu" mode-name))
            (if (string= this-buffer-mode-name
                         "Ada")
                (setq this-buffer-project-file
                      (if ( my-local-variable-if-set-p 'ada-prj-prj-file
                                                   (current-buffer))
                          (expand-file-name ada-prj-prj-file)
                        ""))))
          (if (string= this-buffer-mode-name
                         "Ada")
              (progn
                (princ (format "%-19s  "  buffer-name))
                  (princ (format "%-6s " this-buffer-mode-name))
                  (princ this-buffer-project-file)
                  (princ "\n")
                  ))
          ) ;; end let*
        (setq bl (cdr bl))
        ) ;; end while
      );; end let
    ) ;; end save-excursion
  (display-buffer "*Buffer List*")
  (other-window 1)
  )

(defun ada-change-prj (filename)
  "Set FILENAME to be the project file for current buffer."
  (interactive "fproject file:")

  ;; make sure we are using an Ada file
  (if (not (string= mode-name "Ada"))
    (error "You must be in ada-mode to use this function"))

  ;; create the local variable if necessay
  (if (not ( my-local-variable-if-set-p 'ada-prj-prj-file (current-buffer)))
      (make-local-variable 'ada-prj-prj-file))

  ;; ask the user for the new file name
  (setq ada-prj-prj-file filename)

  ;; force Emacs to reread the prj file next-time
  (ada-reread-prj-file)
  )

(defun ada-change-default-prj (filename)
  "Set FILENAME to be the default project file for the current directory."
  (interactive "ffile name:")
  (let ((dir (file-name-directory (buffer-file-name)))
	(prj (expand-file-name filename)))

    ;;  Associate the directory with a project file
    (if (assoc dir ada-xref-default-prj-file)
	(setcdr (assoc dir ada-xref-default-prj-file) prj)
      (add-to-list 'ada-xref-default-prj-file (list dir prj)))

    ;; Reparse the project file
    (ada-parse-prj-file ada-prj-default-project-file)))


;; ----- Identlist manipulation -------------------------------------------
;; An identlist is a vector that is used internally to reference an identifier
;; To facilitate its use, we provide the following macros

(defmacro ada-make-identlist () (make-vector 8 nil))
(defmacro ada-name-of   (identlist)    (list 'aref identlist 0))
(defmacro ada-line-of   (identlist)    (list 'aref identlist 1))
(defmacro ada-column-of (identlist)    (list 'aref identlist 2))
(defmacro ada-file-of   (identlist)    (list 'aref identlist 3))
(defmacro ada-ali-index-of    (identlist) (list 'aref identlist 4))
(defmacro ada-declare-file-of (identlist) (list 'aref identlist 5))
(defmacro ada-references-of   (identlist) (list 'aref identlist 6))
(defmacro ada-on-declaration  (identlist) (list 'aref identlist 7))

(defmacro ada-set-name         (identlist name) (list 'aset identlist 0 name))
(defmacro ada-set-line         (identlist line) (list 'aset identlist 1 line))
(defmacro ada-set-column       (identlist col)  (list 'aset identlist 2 col))
(defmacro ada-set-file         (identlist file) (list 'aset identlist 3 file))
(defmacro ada-set-ali-index   (identlist index) (list 'aset identlist 4 index))
(defmacro ada-set-declare-file (identlist file) (list 'aset identlist 5 file))
(defmacro ada-set-references   (identlist ref)  (list 'aset identlist 6 ref))
(defmacro ada-set-on-declaration (ident value) (list 'aset ident 7 value))

(defsubst ada-get-ali-buffer (file)
  "Reads the ali file into a new buffer, and returns this buffer's name"
  (find-file-noselect (ada-get-ali-file-name file)))



;; ----- Identifier Completion --------------------------------------------
(defun ada-complete-identifier (pos)
  "Tries to complete the identifier around POS.
The feature is only available if the files where not compiled using the -gnatx
option."
  (interactive "d")
  (ada-require-project-file)

  ;; Initialize function-local variablesand jump to the .ali buffer
  ;; Note that for regexp search is case insensitive too
  (let* ((curbuf (current-buffer))
         (identlist (ada-read-identifier pos))
         (sofar (concat "^[0-9]+[a-zA-Z][0-9]+[ *]\\("
                        (regexp-quote (ada-name-of identlist))
                        "[a-zA-Z0-9_]*\\)"))
         (completed nil)
         (symalist nil)
         (insertpos nil))

    ;; we are already in the .ali buffer
    (goto-char (point-max))

    ;; build an alist of possible completions
    (while (re-search-backward sofar nil t)
      (setq symalist (cons (cons (match-string 1) nil) symalist)))

    (setq completed  (try-completion "" symalist))

    ;; kills .ali buffer
    (kill-buffer nil)

    ;; deletes the incomplete identifier in the buffer
    (set-buffer curbuf)
    (looking-at "[a-zA-Z0-9_]+")
    (replace-match "")
    ;; inserts the completed symbol
    (insert completed)
    ))

;; ----- Cross-referencing ----------------------------------------

(defun ada-point-and-xref ()
 "Calls `mouse-set-point' and then `ada-goto-declaration'."
  (interactive)
  (mouse-set-point last-input-event)
  (ada-goto-declaration (point)))

(defun ada-goto-declaration (pos)
  "Display the declaration of the identifier around POS.
The declaration is shown in another buffer if `ada-xref-other-buffer' is
non-nil."
  (interactive "d")
  (ada-require-project-file)
  (push-mark pos)
  (ada-xref-push-pos (buffer-file-name) pos)
  (ada-find-in-ali (ada-read-identifier pos)))

(defun ada-goto-declaration-other-frame (pos)
  "Display the declaration of the identifier around POS.
The declation is shown in another frame if `ada-xref-other-buffer' is non-nil."
  (interactive "d")
  (ada-require-project-file)
  (push-mark pos)
  (ada-xref-push-pos (buffer-file-name) pos)
  (ada-find-in-ali (ada-read-identifier pos) t))

(defun ada-compile (command)
  "Start COMMAND on the machine specified in the project file."
  (if (and (my-local-variable-if-set-p 'ada-prj-remote-machine (current-buffer))
           (not (string= ada-prj-remote-machine "")))
      (set 'command
           (concat "rsh " ada-prj-remote-machine " '"
                   command "'")))
  (compile command))

(defun ada-compile-application ()
  "Compiles the application, using the command found in the project file."
  (interactive)
  (ada-require-project-file)

  ;; prompt for command to execute
  (ada-compile
   (if ada-xref-confirm-compile
       (read-from-minibuffer "enter command to compile: "
                             ada-prj-make-cmd)
     ada-prj-make-cmd))
  )

(defun ada-compile-current ()
  "Recompile the current file."
  (interactive)
  (ada-require-project-file)

  ;; prompt for command to execute
  (ada-compile
   (if ada-xref-confirm-compile
       (read-from-minibuffer "enter command to compile: "
                             (concat
                              ada-prj-comp-cmd " " (ada-convert-file-name (buffer-file-name))))
     (concat ada-prj-comp-cmd " " (ada-convert-file-name (buffer-file-name)))))
  )

(defun ada-check-current ()
  "Recompile the current file."
  (interactive)
  (ada-require-project-file)

  ;; prompt for command to execute
  (let ((command (concat ada-prj-comp-cmd ada-check-switch
                         (ada-convert-file-name (buffer-file-name)))))
    (compile
     (if ada-xref-confirm-compile
         (read-from-minibuffer "enter command to compile: " command)
       command))))


(defun ada-run-application ()
  "Run the application."
  (interactive)
  (ada-require-project-file)

  (if (and (my-local-variable-if-set-p 'ada-prj-cross-prefix (current-buffer))
           (not (string= ada-prj-cross-prefix "")))
      (error "This feature is not supported yet for cross-compilation environments"))

  (let ((command ada-prj-run-cmd)
        (buffer  (current-buffer)))
    ;; Search the command name if necessary
    (if (not (my-local-variable-if-set-p 'ada-prj-run-cmd (current-buffer)))
        (setq command (file-name-sans-extension (buffer-name)))
      )

    ;; Ask for the arguments to the command
    (setq command
          (read-from-minibuffer "Enter command to execute: "
                                command))

    ;; Run the command
    (save-excursion
      (set-buffer (get-buffer-create "*run*"))
      (goto-char (point-max))
      (insert "\nRunning " command "\n\n")
      (make-comint "run"
                   (comint-arguments command 0 0)
                   nil
                   (comint-arguments command 1 nil))
      )
    (display-buffer "*run*")

    ;;  change to buffer *run* for interactive programs
    (other-window 1)
    (switch-to-buffer "*run*")
    )
  )


(defun ada-gdb-application ()
  "Start the debugger on the application."
  (interactive)

  (require 'gud)
  (let ((buffer (current-buffer))
        gdb-buffer)
    (ada-require-project-file)

    (if (and (my-local-variable-if-set-p 'ada-prj-cross-prefix buffer)
             (not (string= ada-prj-cross-prefix "")))
        (error "This feature is not supported yet for cross-compilation environments"))

    ;; If the command to use was given in the project file
    (if (my-local-variable-if-set-p 'ada-prj-debug-cmd buffer)
	(gdb ada-prj-debug-cmd)
      ;; Else the user will have to enter the command himself
      (gdb "")
      )

    (set 'gdb-buffer (current-buffer))
    
    ;;  Switch back to the source buffer
    ;;  and Activate the debug part in the contextual menu
    (switch-to-buffer buffer)

    (if (functionp 'gud-make-debug-menu)
	(gud-make-debug-menu))

    ;;  Warning: on Emacs >= 20.3.8, same-window-regexps includes gud-*,
    ;;  so the following call to display buffer will select the
    ;;  buffer instead of displaying it in another window
    ;;  This is why the second argument to display-buffer is 't'
    (display-buffer gdb-buffer t)
    ))


(defun ada-reread-prj-file (&optional for-all-buffer)
  "Forces Emacs to read the project file again.
Otherwise, this file is only read once, and never read again
If FOR-ALL-BUFFER is non-nil, or the function was called with \C-u prefix,
then do this for every opened buffer."
  (interactive "P")
  (if for-all-buffer

      ;; do this for every buffer
      (mapcar (lambda (x)
                 (save-excursion
                   (set-buffer x)
		   ;; if we have the ada-mode and there is a real file
		   ;; associated with the buffer
                   (if (and (string= mode-name "Ada")
			    (buffer-file-name))
                       (progn
                         (kill-local-variable 'ada-prj-src-dir)
                         (kill-local-variable 'ada-prj-obj-dir)
                         (ada-parse-prj-file (ada-prj-find-prj-file))))
                   ))
               (buffer-list))

    ;; else do this just for the current buffer
    (kill-local-variable 'ada-prj-src-dir)
    (kill-local-variable 'ada-prj-obj-dir)
    (ada-parse-prj-file (ada-prj-find-prj-file)))
  )

;; ------ Private routines

(defun ada-xref-current (file &optional ali-file-name)
  "Update the cross-references for FILE.
This in fact recompiles FILE to create ALI-FILE-NAME."
  ;; kill old buffer
  (if (and ali-file-name
           (get-file-buffer ali-file-name))
      (kill-buffer (get-file-buffer ali-file-name)))
  ;; prompt for command to execute
  (setq compile-command (concat ada-prj-comp-cmd
                                " "
                                file))
  (compile
   (if ada-xref-confirm-compile
       (read-from-minibuffer "enter command to execute gcc: "
                             compile-command)
     compile-command))
  )

(defun ada-first-non-nil (list)
  "Returns the first non-nil element of the LIST"
  (cond
   ((not list) nil)
   ((car list) (car list))
   (t (ada-first-non-nil (cdr list)))
   ))


(defun ada-find-ali-file-in-dir (file)
  "Search for FILE in obj_dir.
The current buffer must be the Ada file."
  (ada-first-non-nil
   (mapcar (lambda (x)
             (if (file-exists-p (concat (file-name-directory x)
                                        file))
                 (concat (file-name-directory x) file)
               nil))
           ada-prj-obj-dir))
  )

(defun ada-get-ali-file-name (file)
  "Create the ali file name for the ada-file FILE.
The file is searched for in every directory shown in the obj_dir lines of
the project file."

  ;; This function has to handle the special case of non-standard
  ;; file names (i.e. not .adb or .ads)
  ;; The trick is the following:
  ;;   1- replace the extension of the current file with .ali,
  ;;      and look for this file
  ;;   2- If this file is found:
  ;;      grep the "^U" lines, and make sure we are not reading the
  ;;      .ali file for a spec file. If we are, go to step 3.
  ;;   3- If the file is not found or step 2 failed:
  ;;      find the name of the "other file", ie the body, and look
  ;;      for its associated .ali file by subtituing the extension

  (save-excursion
    (set-buffer (get-file-buffer file))
    (let ((short-ali-file-name
           (concat (file-name-sans-extension (file-name-nondirectory file))
                   ".ali"))
          (ali-file-name ""))
      ;; First step
      ;; we take the first possible completion
      (setq ali-file-name (ada-find-ali-file-in-dir short-ali-file-name))

      ;; If we have found the .ali file, but the source file was a spec
      ;; with a non-standard name, search the .ali file for the body if any,
      ;; since the xref information is more complete in that one
      (unless ali-file-name
	  (if (not (string= (file-name-extension file) ".ads"))
	      (let ((is-spec nil)
		    (specs ada-spec-suffixes)
		    body-ali)
		(while specs
		  (if (string-match (concat (regexp-quote (car specs)) "$")
				    file)
		      (set 'is-spec t))
		  (set 'specs (cdr specs)))

		(if is-spec
		    (set 'body-ali
			 (ada-find-ali-file-in-dir
			  (concat (file-name-sans-extension
				   (file-name-nondirectory
				    (ada-other-file-name)))
				  ".ali"))))
                (if body-ali
                    (set 'ali-file-name body-ali))))
	
        ;;  else we did not find the .ali file
        ;;  Second chance: in case the files do not have standard names (such
        ;;  as for instance file_s.ada and file_b.ada), try to go to the
        ;;  other file and look for its ali file
        (setq short-ali-file-name
              (concat (file-name-sans-extension
		       (file-name-nondirectory (ada-other-file-name)))
                      ".ali"))
        (setq ali-file-name (ada-find-ali-file-in-dir short-ali-file-name))
	
        ;; If still not found, try to recompile the file
        (if (not ali-file-name)
            (progn
              ;; recompile only if the user asked for this
              (if ada-xref-create-ali
                  (ada-xref-current file ali-file-name))
              (error "Ali file not found. Recompile your file")))
        )

      ;; same if the .ali file is too old and we must recompile it
      (if (and (file-newer-than-file-p file ali-file-name)
               ada-xref-create-ali)
          (ada-xref-current file ali-file-name))

      ;; else returns the correct absolute file name
      (expand-file-name ali-file-name))
    ))

(defun ada-get-ada-file-name (file original-file)
  "Create the complete file name (+directory) for FILE.
The original file (where the user was) is ORIGINAL-FILE. Search in project
file for possible paths."

  (save-excursion
    (set-buffer (get-file-buffer original-file))
    ;; we choose the first possible completion and we
    ;; return the absolute file name
    (let ((filename
           (ada-first-non-nil (mapcar (lambda (x)
                          (if (file-exists-p (concat (file-name-directory x)
                                                     (file-name-nondirectory file)))
                              (concat (file-name-directory x)
                                      (file-name-nondirectory file))
                            nil))
                        ada-prj-src-dir))))

      (if filename
          (expand-file-name filename)
        (error (concat
                (file-name-nondirectory file)
                " not found in src_dir. Please check your project file")))

      )))

(defun ada-find-file-number-in-ali (file)
  "Returns the file number for FILE in the associated ali file."
  (set-buffer (ada-get-ali-buffer file))
  (goto-char (point-min))

  (let ((begin (re-search-forward "^D")))
    (beginning-of-line)
    (re-search-forward (concat "^D " (file-name-nondirectory file)))
    (count-lines begin (point))))

(defun ada-read-identifier (pos)
  "Returns the identlist around POS and switch to the .ali buffer."

  ;; If there's a compilation in progress, it's probably because the
  ;; .ali file didn't exist. So we should wait...
  (if compilation-in-progress
      (progn
        (message "Compilation in progress. Try again when it is finished")
        (set 'quit-flag t)))

  ;; If at end of buffer (e.g the buffer is empty), error
  (if (>= (point) (point-max))
      (error "No identifier on point"))
  
  ;; goto first character of the identifier/operator (skip backward < and >
  ;; since they are part of multiple character operators
  (goto-char pos)
  (skip-chars-backward "a-zA-Z0-9_<>")

  ;; check if it really is an identifier
  (if (ada-in-comment-p)
      (error "Inside comment"))

  (let (identifier identlist)
    ;; Just in front of a string => we could have an operator declaration,
    ;; as in "+", "-", ..
    (if (= (char-after) ?\")
        (forward-char 1))

    ;; if looking at an operator
    (if (looking-at ada-operator-re)
        (progn
          (if (and (= (char-before) ?\")
                   (= (char-after (+ (length (match-string 0)) (point))) ?\"))
              (forward-char -1))
          (set 'identifier (concat "\"" (match-string 0) "\"")))

      (if (ada-in-string-p)
          (error "Inside string or character constant"))
      (if (looking-at (concat ada-keywords "[^a-zA-Z_]"))
          (error "No cross-reference available for reserved keyword"))
      (if (looking-at "[a-zA-Z0-9_]+")
          (set 'identifier (match-string 0))
        (error "No identifier around")))
    
    ;; Build the identlist
    (set 'identlist    (ada-make-identlist))
    (ada-set-name      identlist (downcase identifier))
    (ada-set-line      identlist
		       (number-to-string (count-lines (point-min) (point))))
    (ada-set-column    identlist
		       (number-to-string (1+ (current-column))))
    (ada-set-file      identlist (buffer-file-name))
    identlist
    ))

(defun ada-get-all-references (identlist)
  "Completes and returns the IDENTLIST with the information extracted
from the ali file (definition file and places where it is referenced)."
  
  (let ((ali-buffer (ada-get-ali-buffer (ada-file-of identlist)))
	declaration-found)
    (set-buffer ali-buffer)
    (goto-char (point-min))
    (ada-set-on-declaration identlist nil)

    ;; First attempt: we might already be on the declaration of the identifier
    ;; We want to look for the declaration only in a definite interval (after
    ;; the "^X ..." line for the current file, and before the next "^X" line
    
    (if (re-search-forward
	 (concat "^X [0-9]+ " (file-name-nondirectory (ada-file-of identlist)))
	 nil t)
        (let ((bound (save-excursion (re-search-forward "^X " nil t))))
          (set 'declaration-found
	       (re-search-forward
		(concat "^"    (ada-line-of identlist)
			"."    (ada-column-of identlist)
			"[ *]" (regexp-quote (ada-name-of identlist))
			" \\(.*\\)$") bound t))
	  (if declaration-found
	      (ada-set-on-declaration identlist t))
	  ))

    ;; If declaration is still nil, then we were not on a declaration, and
    ;; have to fall back on other algorithms

    (unless declaration-found
      
      ;; Since we alread know the number of the file, search for a direct
      ;; reference to it
      (goto-char (point-min))
      (set 'declaration-found t)
      (ada-set-ali-index
       identlist
       (number-to-string (ada-find-file-number-in-ali
			  (ada-file-of identlist))))
      (unless (re-search-forward (concat (ada-ali-index-of identlist)
					 "|\\([0-9]+.[0-9]+ \\)*"
					 (ada-line-of identlist)
					 "[^0-9]"
					 (ada-column-of identlist))
				 nil t)

          ;; if we did not find it, it may be because the first reference
          ;; is not required to have a 'unit_number|' item included.
          ;; Or maybe we are already on the declaration...
          (unless (re-search-forward (concat "^\\([a-zA-Z0-9_.\"]+[ *]\\)*"
					     (ada-line-of identlist)
					     "[^0-9]"
					     (ada-column-of identlist))
				     nil t)
	    
	    ;; If still not found, then either the declaration is unknown
	    ;; or the source file has been modified since the ali file was
	    ;; created
	    (set 'declaration-found nil)
            )
	  )

      ;; Last check to be completly sure we have found the correct line (the
      ;; ali might not be up to date for instance)
      (if declaration-found
	  (progn
	    (beginning-of-line)
	    ;; while we have a continuation line, go up one line
	    (while (looking-at "^\\.")
	      (previous-line 1))
	    (unless (looking-at (concat "[0-9]+.[0-9]+[ *]"
					(ada-name-of identlist) " "))
	      (set 'declaration-found nil))))

      ;; Still no success ! The ali file must be too old, and we need to
      ;; use a basic algorithm based on guesses. Note that this only happens
      ;; if the user does not want us to automatically recompile files
      ;; automatically
      (unless declaration-found
	(unless (ada-xref-find-in-modified-ali identlist)
	  ;; no more idea to find the declaration. Give up
	  (progn
	    (kill-buffer ali-buffer)
	    (error (concat "No declaration of " (ada-name-of identlist)
			   " found."))
	    )))
      )

    
    ;; Now that we have found a suitable line in the .ali file, get the
    ;; information available
    (beginning-of-line)
    (if declaration-found
        (let ((current-line (buffer-substring
			     (point) (save-excursion (end-of-line) (point)))))
          (save-excursion
            (next-line 1)
            (beginning-of-line)
            (while (looking-at "^\\.\\(.*\\)")
              (set 'current-line (concat current-line (match-string 1)))
              (next-line 1))
            )

	  (if (re-search-backward "^X [0-9]+ \\([a-zA-Z0-9_.-]+\\)" nil t)
	      (ada-set-declare-file
	       identlist
	       (ada-get-ada-file-name (match-string 1)
				      (ada-file-of identlist))))
	  
	  (ada-set-references   identlist current-line)
	  ))
  ))

(defun ada-xref-find-in-modified-ali (identlist)
  "Find the matching position for IDENTLIST in the current ali buffer.
This function is only called when the file was not up-to-date, so we need
to make some guesses.
This function is disabled for operators, and only works for identifiers."

  (unless (= (string-to-char (ada-name-of identlist)) ?\")
      (progn
        (let ((declist '()) ;;; ( (line_in_ali_file line_in_ada) ( ... ))
	      (my-regexp  (concat "[ *]"
				  (regexp-quote (ada-name-of identlist)) " "))
	      (line-ada "--")
	      (col-ada  "--")
	      (line-ali 0)
	      (len 0)
	      (choice 0))

          (goto-char (point-max))
          (while (re-search-backward my-regexp nil t)
            (save-excursion
              (set 'line-ali (count-lines (point-min) (point)))
              (beginning-of-line)
              ;; have a look at the line and column numbers
              (if (looking-at "^\\([0-9]+\\).\\([0-9]+\\)[ *]")
                  (progn
                    (setq line-ada (match-string 1))
                    (setq col-ada  (match-string 2)))
                (setq line-ada "--")
                (setq col-ada  "--")
                )
              ;; construct a list with the file names and the positions within
              (if (re-search-backward "^X [0-9]+ \\([a-zA-Z0-9._-]+\\)" nil t)
		  (add-to-list
		   'declist (list line-ali (match-string 1) line-ada col-ada))
                )
              )
            )

          ;; how many possible declarations have we found ?
          (setq len (length declist))
          (cond
           ;; none => error
           ((= len 0)
            (kill-buffer (current-buffer))
            (error (concat "No declaration of "
                           (ada-name-of identlist)
                           " recorded in .ali file")))
	   
           ;; one => should be the right one
           ((= len 1)
            (goto-line (caar declist)))
	   
           ;; more than one => display choice list
           (t
            (with-output-to-temp-buffer "*choice list*"

              (princ "Identifier is overloaded and Xref information is not up to date.\n")
              (princ "Possible declarations are:\n\n")
              (princ "  no.   in file                at line  col\n")
              (princ "  ---   ---------------------     ----  ----\n")
              (let ((counter 1))
                (while (<= counter len)
                  (princ (format "  %2d)    %-21s   %4s  %4s\n"
                                 counter
				 (ada-get-ada-file-name
				  (nth 1 (nth (1- counter) declist))
				  (ada-file-of identlist))
                                 (nth 2 (nth (1- counter) declist))
                                 (nth 3 (nth (1- counter) declist))
                                 ))
                  (setq counter (1+ counter))
                  ) ; end of while
                ) ; end of let
              ) ; end of with-output-to ...
            (setq choice nil)
            (while (or
                    (not choice)
                    (not (integerp choice))
                    (< choice 1)
                    (> choice len))
              (setq choice (string-to-int
                            (read-from-minibuffer "Enter No. of your choice: "))))
            (goto-line (car (nth (1- choice) declist)))
            ))))))


(defun ada-find-in-ali (identlist &optional other-frame)
  "Look in the .ali file for the definition of the identifier in IDENTLIST.
If OTHER-FRAME is non nil, and `ada-xref-other-buffer' is non nil,
opens a new window to show the declaration."

  (ada-get-all-references identlist)
  (let ((ali-line (ada-references-of identlist))
	file  line  col)
    
    ;; If we were on a declaration, go to the body
    (if (ada-on-declaration identlist)
	(if (string-match "\\([0-9]+\\)[bc]\\([0-9]+\\)" ali-line)
	    (progn
	      (setq line (match-string 1 ali-line)
		    col  (match-string 2 ali-line))
	      ;;  it there was a file number in the same line
	      (if (string-match "\\([0-9]+\\)|\\([^|bc]+\\)?[bc]" ali-line)
		  (let ((file-number (match-string 1 ali-line)))
		    (goto-char (point-min))
		    (re-search-forward "^D \\([a-zA-Z0-9_.-]+\\)" nil t
				       (string-to-number file-number))
		    (set 'file (match-string 1))
		    )
		;; Else get the nearest file
		(re-search-backward "^X [0-9]+ \\([a-zA-Z0-9_.-]+\\)" nil t)
		(set 'file (match-string 1))
		)
	      )
	  (error "No body found"))
    
      ;; Else we were not on the declaration, find the place for it
      (string-match "\\([0-9]+\\)[a-zA-Z+]\\([0-9]+\\)[ *]" ali-line)
      (setq line (match-string 1 ali-line)
	    col  (match-string 2 ali-line)
	    file (ada-declare-file-of identlist))
      )

    ;; Now go to the buffer
    (ada-xref-change-buffer
     (ada-get-ada-file-name file (ada-file-of identlist))
     (string-to-number line)
     (1- (string-to-number col))
     identlist
     other-frame)
    ))

(defun ada-xref-change-buffer
  (file line column identlist &optional other-frame)
  "Select and display FILE, at LINE and COLUMN. The new file is
associated with the same project file as the one for IDENTLIST.
If we do not end on the same identifier as IDENTLIST, find the closest
match. Kills the .ali buffer at the end.
If OTHER-FRAME is non-nil, creates a new frame to show the file."

  (let (prj-file
        declaration-buffer
	(ali-buffer (current-buffer)))

    ;; get the current project file for the source ada file
    (save-excursion
      (set-buffer (get-file-buffer (ada-file-of identlist)))
      (set 'prj-file ada-prj-prj-file))

    ;; Select and display the destination buffer
    (if ada-xref-other-buffer
        (if other-frame
            (find-file-other-frame file)
          (set 'declaration-buffer (find-file-noselect file))
          (set-buffer declaration-buffer)
          (switch-to-buffer-other-window declaration-buffer)
          )
      (find-file file)
      )

    ;; If the new buffer is not already associated with a project file, do it
    (unless (my-local-variable-if-set-p 'ada-prj-prj-file (current-buffer))
        (progn
          (make-local-variable 'ada-prj-prj-file)
          (set 'ada-prj-prj-file prj-file)))

    ;; move the cursor to the correct position
    (push-mark)
    (goto-line line)
    (move-to-column column)

    ;; If we are not on the identifier, the ali file was not up-to-date.
    ;; Try to find the nearest position where the identifier is found,
    ;; this is probably the right one.
    (unless (looking-at (ada-name-of identlist))
      (ada-xref-search-nearest (ada-name-of identlist)))

    (kill-buffer ali-buffer)))


(defun ada-xref-search-nearest (name)
  "Searches for NAME nearest to the position recorded in the Xref file.
It returns the position of the declaration in the buffer or nil if not found."
  (let ((orgpos (point))
        (newpos nil)
        (diff nil))

    (goto-char (point-max))

    ;; loop - look for all declarations of name in this file
    (while (search-backward name nil t)

      ;; check if it really is a complete Ada identifier
      (if (and
           (not (save-excursion
                  (goto-char (match-end 0))
                  (looking-at "_")))
           (not (ada-in-string-or-comment-p))
           (or
            ;; variable declaration ?
            (save-excursion
              (skip-chars-forward "a-zA-Z_0-9" )
              (ada-goto-next-non-ws)
              (looking-at ":[^=]"))
            ;; procedure, function, task or package declaration ?
            (save-excursion
              (ada-goto-previous-word)
              (looking-at "\\<[pP][rR][oO][cC][eE][dD][uU][rR][eE]\\>\\|\\<[fF][uU][nN][cC][tT][iI][oO][nN]\\>\\|\\<[tT][yY][pP][eE]\\>\\|\\<[tT][aA][sS][kK]\\>\\|\\<[pP][aA][cC][kK][aA][gG][eE]\\>\\|\\<[bB][oO][dD][yY]\\>"))))

          ;; check if it is nearer than the ones before if any
          (if (or (not diff)
                  (< (abs (- (point) orgpos)) diff))
              (progn
                (setq newpos (point)
		      diff (abs (- newpos orgpos))))))
      )

    (if newpos
        (progn
          (message "ATTENTION: this declaration is only a (good) guess ...")
          (goto-char newpos))
      nil)))


;; Find the parent library file of the current file
(defun ada-goto-parent ()
  "Go to the parent library file."
  (interactive)
  (ada-require-project-file)

  (let ((buffer (ada-get-ali-buffer (buffer-file-name)))
        (unit-name nil)
        (body-name nil)
        (ali-name nil))
    (save-excursion
      (set-buffer buffer)
      (goto-char (point-min))
      (re-search-forward "^U \\([^ \t%]+\\)%[bs][ \t]+\\([^ \t]+\\)")
      (setq unit-name (match-string 1))
      (if (not (string-match "\\(.*\\)\\.[^.]+" unit-name))
          (progn
            (kill-buffer buffer)
            (error "No parent unit !"))
        (setq unit-name (match-string 1 unit-name))
        )

      ;; look for the file name for the parent unit specification
      (goto-char (point-min))
      (re-search-forward (concat "^W " unit-name
                                 "%s[ \t]+\\([^ \t]+\\)[ \t]+"
                                 "\\([^ \t\n]+\\)"))
      (setq body-name (match-string 1))
      (setq ali-name (match-string 2))
      (kill-buffer buffer)
      )

    (setq ali-name (ada-find-ali-file-in-dir ali-name))

    (save-excursion
      ;; Tries to open the new ali file to find the spec file
      (if ali-name
          (progn
            (find-file ali-name)
            (goto-char (point-min))
            (re-search-forward (concat "^U " unit-name "%s[ \t]+"
                                       "\\([^ \t]+\\)"))
            (setq body-name (match-string 1))
            (kill-buffer (current-buffer))
            )
        )
      )

    (find-file body-name)
    ))

(defun ada-make-filename-from-adaname (adaname)
  "Determine the filename in which ADANAME is found.
This is a GNAT specific function that uses gnatkrunch."
  (let (krunch-buf)
    (setq krunch-buf (generate-new-buffer "*gkrunch*"))
    (save-excursion
      (set-buffer krunch-buf)
      ;; send adaname to external process `gnatkr'.
      (call-process "gnatkr" nil krunch-buf nil
                    adaname ada-krunch-args)
      ;; fetch output of that process
      (setq adaname (buffer-substring
                     (point-min)
                     (progn
                       (goto-char (point-min))
                       (end-of-line)
                       (point))))
      (kill-buffer krunch-buf)))
  adaname
  )


(defun ada-make-body-gnatstub ()
  "Create an Ada package body in the current buffer.
This function uses the `gnatstub' program to create the body.
This function typically is to be hooked into `ff-file-created-hooks'."
  (interactive)

  (save-some-buffers nil nil)

  (ada-require-project-file)

  (delete-region (point-min) (point-max))

  ;; Call the external process gnatstub
  (let* ((gnatstub-opts (ada-treat-cmd-string ada-gnatstub-opts))
         (filename      (buffer-file-name (car (cdr (buffer-list)))))
         (output        (concat (file-name-sans-extension filename) ".adb"))
         (gnatstub-cmd  (concat "gnatstub " gnatstub-opts " " filename))
         (buffer        (get-buffer-create "*gnatstub*")))

    (save-excursion
      (set-buffer buffer)
      (compilation-minor-mode 1)
      (erase-buffer)
      (insert gnatstub-cmd)
      (newline)
      )
    ;; call gnatstub to create the body file
    (call-process shell-file-name nil buffer nil "-c" gnatstub-cmd)

    (if (save-excursion
          (set-buffer buffer)
          (goto-char (point-min))
          (search-forward "command not found" nil t))
        (progn
          (message "gnatstub was not found -- using the basic algorithm")
          (sleep-for 2)
          (kill-buffer buffer)
          (ada-make-body))

      ;; Else clean up the output

      ;;  Kill the temporary buffer created by find-file
      (set-buffer-modified-p nil)
      (kill-buffer (current-buffer))

      (if (file-exists-p output)
          (progn
            (find-file output)
            (kill-buffer buffer))

        ;; display the error buffer
        (display-buffer buffer)
        )
      )))


(defun ada-xref-initialize ()
  "Function called by ada-mode-hook to initialize the ada-xref.el package.
For instance, it creates the gnat-specific menus, set some hooks for
find-file...."
  (ada-add-ada-menu)
  (make-local-hook 'ff-file-created-hooks)
  (setq ff-file-created-hooks 'ada-make-body-gnatstub)

  ;; Read the project file and update the search path
  ;; before looking for the other file
  (make-local-hook 'ff-pre-find-hooks)
  (add-hook 'ff-pre-find-hooks 'ada-require-project-file)

  ;; Completion for file names in the mini buffer should ignore .ali files
  (add-to-list 'completion-ignored-extensions ".ali")
  )


;; ----- Add to ada-mode-hook ---------------------------------------------

;;  Set the keymap once and for all, so that the keys set by the user in his
;;  config file are not overwritten every time we open a new file.
(ada-add-keymap)

(add-hook 'ada-mode-hook 'ada-xref-initialize)

(provide 'ada-xref)

;;; ada-xref.el ends here
