;;; @(#) ada-prj.el --- Easy editing of project files for the ada-mode

;; Copyright (C) 1998,1999 Free Software Foundation, Inc.

;; Author: Emmanuel Briot <briot@gnat.com>
;; Ada Core Technologies's version:   $Revision: 1.30 $
;; Keywords: languages, ada, project file

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
;;; This package provides a set of functions to easily edit the project
;;; files used by the ada-mode.
;;; The only function publicly available here is `ada-prj-customize'.
;;; Please ada-mode.el and its documentation for more information about the
;;; project files.
;;;
;;; You need Emacs >= 20.2 to run this package

;; Code:


;; ----- Requirements -----------------------------------------------------

(require 'cus-edit)


;; ----- Buffer local variables -------------------------------------------
;; if non nil, then all the widgets will have the default values, instead
;; of reading them from the project file
(make-variable-buffer-local (defvar ada-prj-edit-use-default-values nil))

;; List of the default values used for the field in the project file
;; Mainly used to save only the modified fields into the file itself
;; The values are hold in the properties of this variable
(make-variable-buffer-local (defvar ada-prj-default nil))

(make-variable-buffer-local (defvar ada-prj-widget-prj-dir nil))
(make-variable-buffer-local (defvar ada-prj-widget-src-dir nil))
(make-variable-buffer-local (defvar ada-prj-widget-obj-dir nil))
(make-variable-buffer-local (defvar ada-prj-widget-main nil))
(make-variable-buffer-local (defvar ada-prj-widget-comp-opt nil))
(make-variable-buffer-local (defvar ada-prj-widget-bind-opt nil))
(make-variable-buffer-local (defvar ada-prj-widget-link-opt nil))
(make-variable-buffer-local (defvar ada-prj-widget-remote-machine nil))
(make-variable-buffer-local (defvar ada-prj-widget-comp-cmd nil))
(make-variable-buffer-local (defvar ada-prj-widget-make-cmd nil))
(make-variable-buffer-local (defvar ada-prj-widget-run-cmd nil))
(make-variable-buffer-local (defvar ada-prj-widget-debug-cmd nil))
(make-variable-buffer-local (defvar ada-prj-widget-cross-prefix nil))

;; ------ Functions -------------------------------------------------------

(defun ada-prj-add-ada-menu ()
  "Add a new submenu to the Ada menu."
  (interactive)

  (if ada-xemacs
      (progn
        (add-menu-button '("Ada" "Project") ["New/Edit" ada-customize t] "Associate")
        )
    (let ((prj-menu (lookup-key ada-mode-map [menu-bar Ada Project])))
      (define-key prj-menu [New] '("New/Edit" . ada-customize)))
    ))

(defun ada-prj-add-keymap ()
  "Add new keybindings for ada-prj."
  (define-key ada-mode-map "\C-cu"  'ada-customize))

(defun ada-customize (&optional new-file)
  "Edit the project file associated with the current buffer.
If there is none or NEW-FILE is non-nil, make a new one."
  (interactive)
  (if new-file
      (progn
        (setq ada-prj-edit-use-default-values t)
        (kill-local-variable 'ada-prj-prj-file)
        (ada-prj-customize)
        (setq ada-prj-edit-use-default-values nil))
    (ada-prj-customize)))

(defun ada-prj-save ()
  "Save the currently edited project file."
  (interactive)
  (let ((file-name (widget-value ada-prj-widget-prj-dir))
        value output)
    (setq output
          (concat
           (ada-prj-set-list "src_dir" (widget-value ada-prj-widget-src-dir))
           "\n"
           (ada-prj-set-list "obj_dir" (widget-value ada-prj-widget-obj-dir))
           "\n"
           (unless (string= (setq value (widget-value ada-prj-widget-comp-opt))
                            (get 'ada-prj-default 'comp_opt))
             (concat "comp_opt=" value "\n"))
           (unless (string= (setq value (widget-value ada-prj-widget-bind-opt))
                            (get 'ada-prj-default 'bind_opt))
             (concat "bind_opt=" value "\n"))
           (unless (string= (setq value (widget-value ada-prj-widget-link-opt))
                            (get 'ada-prj-default 'link_opt))
             (concat "link_opt=" value "\n"))
           (unless (string= (setq value (widget-value ada-prj-widget-main))
                            (get 'ada-prj-default 'main))
             (concat "main=" value "\n"))
           (unless (string= (setq value (widget-value ada-prj-widget-cross-prefix))
                            (get 'ada-prj-default 'cross-prefix))
             (concat "cross_prefix=" value "\n"))
           (unless (string= (setq value (widget-value ada-prj-widget-remote-machine))
                            (get 'ada-prj-default 'remote-machine))
             (concat "remote_machine=" value "\n"))
           (unless (string= (setq value (widget-value ada-prj-widget-comp-cmd))
                            (get 'ada-prj-default 'comp_cmd))
             (concat "comp_cmd=" value "\n"))
           (unless (string= (setq value (widget-value ada-prj-widget-make-cmd))
                            (get 'ada-prj-default 'make_cmd))
             (concat "make_cmd=" value "\n"))
           (unless (string= (setq value (widget-value ada-prj-widget-run-cmd))
                            (get 'ada-prj-default 'run_cmd))
             (concat "run_cmd=" value "\n"))
           (unless (string= (setq value (widget-value ada-prj-widget-debug-cmd))
                            (get 'ada-prj-default 'debug_cmd))
             (concat "debug_cmd=" value "\n"))
           ))
    (find-file file-name)
    (erase-buffer)
    (insert output)
    (save-buffer)
    ;; kill the project buffer
    (kill-buffer nil)

    ;; kill the editor buffer
    (kill-buffer "*Customize Ada Mode*")

    ;; automatically associates the current buffer with the
    ;; new project file
    (make-local-variable 'ada-prj-prj-file)
    (setq ada-prj-prj-file file-name)

    ;; force emacs to reread the project files
    (ada-reread-prj-file t)
    )
  )

(defun ada-prj-customize ()
  "Edit the project file associated with the current Ada buffer."
  (let* ((old-name (buffer-file-name))
         prj-file)

    (unless old-name
      (error
       "No file name given for this buffer ! You need to open a file first"))
    
    ;;  Find the project file associated with the buffer
    (setq prj-file (ada-prj-get-prj-dir old-name))

    (switch-to-buffer "*Customize Ada Mode*")
    (kill-all-local-variables)

    ;;  Find the default values
    (setq ada-prj-default nil)
    (put 'ada-prj-default 'src_dir (list (file-name-directory old-name)))
    (put 'ada-prj-default 'obj_dir (list (file-name-directory old-name)))
    (put 'ada-prj-default 'comp_opt "")
    (put 'ada-prj-default 'bind_opt "")
    (put 'ada-prj-default 'link_opt "")
    (put 'ada-prj-default 'main     "")
    (put 'ada-prj-default 'cross_prefix "")
    (put 'ada-prj-default 'remote_machine "")
    (put 'ada-prj-default 'comp_cmd
         (concat "cd " (file-name-directory old-name) " && "
                 ada-prj-default-comp-cmd))
    (put 'ada-prj-default 'make_cmd
         (concat "cd " (file-name-directory old-name) " && "
                 ada-prj-default-make-cmd))
    (put 'ada-prj-default 'run_cmd (if is-windows "${main}.exe" "${main}"))
    (put 'ada-prj-default 'debug_cmd
         (if is-windows "${cross_prefix}gdb ${main}.exe"
           "${cross_prefix}gdb ${main}"))

    (let ((inhibit-read-only t))
      (erase-buffer))

    ;;; Overlay-lists is not defined on XEmacs
    (if (fboundp 'overlay-lists)
        (let ((all (overlay-lists)))
          ;; Delete all the overlays.
          (mapcar 'delete-overlay (car all))
          (mapcar 'delete-overlay (cdr all))))

    (use-local-map (copy-keymap custom-mode-map))
    (local-set-key "\C-x\C-s" 'ada-prj-save)

    (widget-insert "
----------------------------------------------------------------
--  Customize your Emacs Ada mode for the current application --
----------------------------------------------------------------
This buffer will allow you to create easily a project file for your application.
This file will tell Emacs where to find the ada sources, the cross-referencing
informations, how to compile and run your application, ...

Please use the RETURN key, or middle mouse button to activate the fields.\n\n")

    ;; Reset Button
    (widget-create 'push-button
                   :notify (lambda (&rest ignore)
                             (setq ada-prj-edit-use-default-values t)
                             (kill-buffer nil)
                             (ada-prj-customize)
                             (setq ada-prj-edit-use-default-values nil)
                             )
                   "Reset to Default Values")
    (widget-insert "\n")


    ;;  Create local variables with their initial value
    (setq ada-prj-widget-prj-dir
          (ada-prj-new 'ada-prj-widget-prj-dir nil "" prj-file
                       "\nName and directory of the project file.
Put a new name here if you want to create a new project file\n"))

    (setq ada-prj-widget-src-dir
          (ada-prj-list 'ada-prj-widget-src-dir prj-file "src_dir"
                        (get 'ada-prj-default 'src_dir)
                        "\nYou should enter below all the directories where Emacs
will find your ada sources for the current application\n"))

    (setq ada-prj-widget-obj-dir
          (ada-prj-list 'ada-prj-widget-obj-dir prj-file "obj_dir"
                        (get 'ada-prj-default 'obj_dir)
                        "\nBelow are the directories where the object files generated
by the compiler will be found. This files are required for the cross-referencing
capabilities of the Emacs' Ada-mode.\n"))

    (setq ada-prj-widget-comp-opt
          (ada-prj-new 'ada-prj-widget-comp-opt prj-file "comp_opt"
                       (get 'ada-prj-default 'comp_opt)
                       "\nPut below the compiler switches.\n"))

    (setq ada-prj-widget-bind-opt
          (ada-prj-new 'ada-prj-widget-bind-opt prj-file "bind_opt"
                       (get 'ada-prj-default 'bind_opt)
                       "\nPut below the binder switches.\n"))

    (setq ada-prj-widget-link-opt
          (ada-prj-new 'ada-prj-widget-link-opt prj-file "link_opt"
                       (get 'ada-prj-default 'link_opt)
                       "\nPut below the linker switches.\n"))

    (setq ada-prj-widget-main
          (ada-prj-new 'ada-prj-widget-main prj-file "main"
                       (file-name-sans-extension old-name)
                       "\nPut below the name of the main program for your application\n"))

    (setq ada-prj-widget-cross-prefix
          (ada-prj-new 'ada-prj-widget-cross-prefix prj-file "cross_prefix"
                       (get 'ada-prj-default 'cross_prefix)
                       "\nIf you are using a cross compiler, you might want to
set the following variable so that the correct compiler is used by default\n"))

    (setq ada-prj-widget-remote-machine
          (ada-prj-new 'ada-prj-widget-remote-machine prj-file "remote_machine"
                       (get 'ada-prj-default 'remote_machine)
                       "\nName of the machine to log on before a compilation.
Leave an empty field if you want to compile on the local machine.
This will not work on Windows NT, since we only do a 'rsh' to the
remote machine and then issue the command. \n"))

    (widget-insert "\n
-------------------------------------------------------------------------------
      / \\        !! Advanced Users !! : For the following commands, you may use
     / | \\       a somewhat more complicated syntax to describe them. If you
    /  |  \\      use some special fields,  they will be replaced at run-time by
   /   |   \\     the variables defined above.
  /    |    \\    These special fields are : ${remote_machine}
 /     o     \\   -aI${src_dir} -I${src_dir} -aO${obj_dir} ${comp_opt}
 -------------   ${bind_opt}  ${link_opt} ${main} ${cross_prefix}

The easiest way is to ignore this possibility. These fields are intended only
for user who really understand what `variable substitution' means.
-------------------------------------------------------------------------------\n")

    (setq ada-prj-widget-comp-cmd
          (ada-prj-new 'ada-prj-widget-comp-cmd prj-file "comp_cmd"
                       (get 'ada-prj-default 'comp_cmd)
                       "\nPut below the command used to compile ONE file.
The name of the file to compile will be added at the end of the command.
This command will also be used to check the file.\n"))

    (setq ada-prj-widget-make-cmd
          (ada-prj-new 'ada-prj-widget-make-cmd prj-file "make_cmd"
                       (get 'ada-prj-default 'make_cmd)
                       "\nPut below the command used to compile the whole application.\n"))

    (setq ada-prj-widget-run-cmd
          (ada-prj-new 'ada-prj-widget-run-cmd prj-file "run_cmd"
                       (get 'ada-prj-default 'run_cmd)
                       "\nPut below the command used to run your application.\n"))

    (setq ada-prj-widget-debug-cmd
          (ada-prj-new 'ada-prj-widget-run-cmd prj-file "debug_cmd"
                       (get 'ada-prj-default 'debug_cmd)
                       "\nPut below the command used to launch the debugger on your application.\n"))

    ;; the two buttons to validate or cancel the modification
    (widget-insert "\nWhen you have finish completing the above fields, choose one of the two buttons
below, to validate or cancel your modifications.
If you choose `OK', your settings will be saved to the file whose name is given above.\n")

    (widget-create 'push-button
                   :notify (lambda (&rest ignore) (ada-prj-save))
                   "OK")

    (widget-insert "   ")
    (widget-create 'push-button
                   :notify (lambda (&rest ignore)
                             (kill-buffer nil))
                   "Cancel")
    (widget-insert "\n")


    ;; if it exists, kill the project file buffer
    (if (and prj-file
             (get-file-buffer prj-file))
        (kill-buffer (get-file-buffer prj-file)))

    (widget-setup)
    (beginning-of-buffer)
    )
  )


;; ---------------- Utilities --------------------------------

(defun ada-prj-new (variable prj-file text default message)
  "Create a buffer-local variable with name VARIABLE.
If PRJ-FILE exists, read its value from that file, otherwise set it to
DEFAULT.
It also creates a widget in the current buffer to edit this variable,
which MESSAGE explaning what the variable is supposed to do.
TEXT is put just before the editable field, and should display the name
of the variable."

  ;; create local variable
  (make-local-variable variable)
  (let ((value  default)
        (regexp (concat "^" text "=\\(.*\\)")))
    ;; if the project file exists
    (if (and prj-file (not ada-prj-edit-use-default-values)
             (file-readable-p prj-file))
        ;; find the value
        (save-excursion
          (find-file prj-file)
          (beginning-of-buffer)
          (if (re-search-forward regexp nil t)
              (setq value (match-string 1)))
          ))
    ;; assign a new value to the variable
    (setq variable value))

  (widget-insert message)

  (widget-create 'editable-field
                 :format (if (string= text "")  "%v"
                           (concat text "= %v"))
                 :keymap widget-keymap
                 variable))


(defun ada-prj-list (variable prj-file text default message)
  "Create a buffer-local list variable with name VARIABLE.
If PRJ-FILE exists, read its value from that file, otherwise set it to
DEFAULT.
It also creates a widget in the current buffer to edit this variable,
which MESSAGE explaning what the variable is supposed to do.
TEXT is put just before the editable field, and should display the name
of the variable."

  ;; create local variable
  (make-local-variable variable)
  (let ((value nil)
        (regexp  (concat "^" text "=\\(.*\\)")))
    ;; if the project file exists
    (if (and prj-file (not ada-prj-edit-use-default-values)
             (file-readable-p prj-file))
        ;; find the value
        (save-excursion
          (find-file prj-file)
          (goto-char (point-min))
          ;; for each line, add its value
          (while
              (re-search-forward regexp nil t)
            (progn
              (setq value (cons (match-string 1) value)))
            )))

    ;; assign a new value to the variable
    (setq variable
          (if value (reverse value) default)))

  (widget-insert message)
  (widget-create 'editable-list
                 :entry-format (concat text "=  %i %d %v")
                 :value variable
                 (list 'editable-field :keymap widget-keymap)))

(defsubst ada-prj-set-list (string ada-dir-list)
  "Join the strings in ADA-DIR-LIST into a single string. Each name is put
on a separate line that begins with STRING."
  (mapconcat (lambda (x)
               (concat string "=" x
                       (unless (string= (substring x -1) "/")
                         "/")))
             ada-dir-list "\n"))

(defun ada-prj-get-prj-dir (&optional ada-file)
  "Returns the directory/name of the project file for ADA-FILE.
If ADA-FILE is nil, returns the project file for the current buffer."
  (unless ada-file
    (setq ada-file (buffer-file-name)))

  (save-excursion
    (set-buffer (get-file-buffer ada-file))
    (if ada-prj-edit-use-default-values
        (concat (file-name-sans-extension ada-file)
                ada-project-file-extension)

      (let ((prj-file (ada-prj-find-prj-file t)))
        (if (or (not prj-file)
                (not (file-exists-p prj-file))
                )
            (setq prj-file
                  (concat (file-name-sans-extension ada-file)
                          ada-project-file-extension)))
        prj-file)
      ))
  )


;;  Initializations for the package
(add-hook 'ada-mode-hook 'ada-prj-add-ada-menu)

;;  Set the keymap once and for all, so that the keys set by the user in his
;;  config file are not overwritten every time we open a new file.
(ada-prj-add-keymap)

(provide 'ada-prj)
;;; package ada-prj.el ends here



